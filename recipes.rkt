#lang racket

(require racket/fixnum
         racket/undefined)

(require "utility.rkt"
         "items.rkt"
         "generated-items.rkt")

(provide (struct-out recipe$)
         get-recipes-for
         get-recipes-using
         canonicalize-input-form
         get-craftable-item-names
         craftable-item?
         make-recipe
         add-recipe
         add-recipe-by-names)

(struct recipe$ (action output count net-count inputs) #:transparent)

(define (make-recipe action output-name count inputs)
  (define output-item (get-item output-name))
  (define output-as-input (assoc output-item inputs))
  (define net-produced (if output-as-input
                           (- count (cdr output-as-input))
                           count))
  (when (null? inputs)
    (raise-argument-error 'make-recipe
                          "(not (null? inputs))"
                          (list output-name inputs)))
  (recipe$ action
           output-item
           count
           net-produced
           (sort inputs symbol<? #:key (λ (i) (item$-name (car i))))))

(define all-recipes '())
(define recipes-by-output (make-hasheq))
(define recipes-by-input (make-hasheq))

(define (get-recipes-for item-ref)
  (define item (if (item$? item-ref) item-ref (get-item item-ref)))
  (hash-ref recipes-by-output item '()))

(define (get-recipes-using item-ref)
  (define item (if (item$? item-ref) item-ref (get-item item-ref)))
  (hash-ref recipes-by-input item '()))

(define (get-craftable-item-names)
  (map item$-name (hash-keys recipes-by-output)))

(define (craftable-item? item)
  (hash-has-key? recipes-by-output item))

(define (recipe-short-form r)
  (list* (recipe$-action r)
         (item$-name (recipe$-output r))
         (recipe$-count r)
         (for/list ([i (recipe$-inputs r)]) (cons (item$-name (car i)) (cdr i)))))

;;; Helper methods for defining recipes.

;; Input definitions can simply name an item, with an implicit quantity of "one", or they can be
;; a (quantity name) list. This function normalizes a definition to a (name . quantity) pair.
(define (canonicalize-input-form input)
  (match input
    [(list (? symbol? item) (? integer? n))
     (cons (get-item item) n)]
    [(? symbol? item)
     (cons (get-item item) 1)]))

;; Transforms an input list to a list of lists of inputs, canonicalizing the input definitions
;; and expanding (or input ...) lists into lists of alternatives. The result serves as a basis
;; for forming all the possible mutations of an input lists that uses or-forms.
(define (input->alternatives-list input-spec)
  (match input-spec
    [(list 'or alts ...) (map canonicalize-input-form alts)]
    [input (list (canonicalize-input-form input))]))

;; Canonicalize, expand, and permute the input list of a recipe.
(define (permute-input-list input-specs)
  (apply permute-lists (map input->alternatives-list input-specs)))

;; Parse and canonicalize a recipe definition, returning a list of recipe$ struct instances.
(define (parse-recipe target recipe)
  (match recipe
    [(list 'build output-count inputs ...)
     #:when (integer? output-count)
     (map (λ (inputs) (make-recipe 'build target output-count inputs)) (permute-input-list inputs))]
    [(list 'build inputs ...)
     (map (λ (inputs) (make-recipe 'build target 1 inputs)) (permute-input-list inputs))]
    [(list 'refine output-count inputs ...)
     #:when (integer? output-count)
     (map (λ (inputs) (make-recipe 'refine target output-count inputs)) (permute-input-list inputs))]
    [(list 'refine  inputs ...)
     (map (λ (inputs) (make-recipe 'refine target 1 inputs)) (permute-input-list inputs))]))

(define (similar-recipes r1 r2)
  ; Don't look at count.
  (and (eq? (recipe$-output r1) (recipe$-output r2))
       (or (and (eq? 'build (recipe$-action r1)) (eq? 'build (recipe$-action r2)))
           (equal? (recipe$-inputs r1) (recipe$-inputs r2)))))

(define (add-recipe r)
  (define dupe (findf (λ (o) (similar-recipes r o))
                      (hash-ref recipes-by-output (recipe$-output r) null)))
  (cond
    [dupe
     (printf "Duplicate recipes: ~a~n                 : ~a~n" (recipe-short-form dupe) (recipe-short-form r))]
    [else
     (hash-update! recipes-by-output (recipe$-output r) (λ (v) (cons r v))  null)
     (for ((i (map car (recipe$-inputs r))))
       (hash-update! recipes-by-input i (λ (v) (cons r v)) '()))
     (set! all-recipes (cons r all-recipes))]))

(define (add-recipe-by-names action target count inputs)
  (define translated-inputs
    (for/list ([i inputs])
      (cons (get-item (car i) #f) (cdr i))))
  (when (andmap car translated-inputs)
    (add-recipe (make-recipe action target count translated-inputs))))

;; Parse and record a resource and its recipes.
(define (def-recipes name . recipe-defs)
  (define recipes (append-map (λ (recipe) (parse-recipe name recipe)) recipe-defs))
  (for ([r recipes])
    (add-recipe r)))

;;
;; Known recipes.
;;
;; Many of these come from the No Man's Sky wiki, the rest from the game itsel.f
;;
;; The syntax is simple. Each entry is a list. The first element of the list is
;; a symbol representing the output item of the following recipes. It must match
;; an item defined in items.rkt.
;;
;; Each recipe is a list which starts with the symbol refine or build, your two
;; kinds of in-game crafting actions. Then comes up an optional number of the output
;; item that the recipe produces, which defaults to one. Then come the recipe inputs.
;; Each input is either an item symbol, implying that one of that input item is needed,
;; or a list of item symbol and count, indicating how many of that input item is needed.
;;
;; In order to greatly collapse the size of definitions, an input item can also be
;; specified using a list of form (or {input} ...). There are some items that can be
;; produced by a bunch of different recipes that can be succinctly represented this
;; way. When the recipe definitions are stored, these forms are expanded into simpler
;; canonical representations.
;;
;; TODO: Recipe lists are incomplete.

#;(define raw-recipes
  '(
    ;;
    ;; Resources
    ;;
    ;; All filled in from data on https://nomanssky.gamepedia.com
    ;;
    (Cactus-Flesh
     (refine (Pyrite 2) Sulphurine))
    (Chromatic-Metal
     (refine 1 (Copper 2))
     (refine 1 Cadmium)
     (refine 3 (Emeril 2))
     (refine 2 Indium)
     (refine 1 Activated-Copper)
     (refine 2 Activated-Cadmium)
     (refine 3 Activated-Emeril)
     (refine 4 Activated-Indium)
     (refine 1 Pure-Ferrite Copper)
     (refine 2 Pure-Ferrite Cadmium)
     (refine 3 Pure-Ferrite Emeril)
     (refine 4 Pure-Ferrite Indium)
     (refine 2 Pure-Ferrite Activated-Copper)
     (refine 4 Pure-Ferrite Activated-Cadmium)
     (refine 6 Pure-Ferrite Activated-Emeril)
     (refine 8 Pure-Ferrite Activated-Indium)
     (refine 5 Gold Silver Copper)
     (refine 10 Gold Silver Cadmium)
     (refine 20 Gold Silver Emeril)
     (refine 30 Gold Silver Indium))
    (Cobalt
     (refine 2 Ionised-Cobalt))
    (Condensed-Carbon
     (refine 150 Carbon-Crystal)
     (refine (Carbon 2))
     (refine 5 (Carbon 2) (Oxygen 2))
     (refine 6 Condensed-Carbon (Oxygen 2))
     (refine 2 Coprite (or Mordite (Carbon 2)))
     (refine 3 Coprite Condensed-Carbon)
     (refine 2 Uranium Di=hydrogen))
    (Copper
     (refine 4 Copper Chromatic-Metal))
    (Coprite
     (refine (Mordite 3))
     (refine Di=hydrogen Carbon)
     (refine 3 Coprite Oxygen)
     (refine Nitrogen Sulphurine)
     (refine 3 Mordite (Carbon 2))
     (refine 4 Mordite Condensed-Carbon))
    (Deuterium
     (refine Di=hydrogen Tritium))
    (Di=hydrogen
     (refine 50 Di=hydrogen-Jelly)
     (refine (Tritium 5)))
    (Dioxite
     (refine 2 Condensed-Carbon Sodium-Nitrate)
     (refine Carbon Sodium-Nitrate)
     (refine (Frost-Crystal 2) Salt)
     (refine (Ammonia 2) (or Ferrite-Dust Pure-Ferrite)))
    (Emeril
     (refine 4 Emeril Chromatic-Metal))
    (Ferrite-Dust
     (refine (or Paraffinium Dioxite Phosphorus Pyrite (Rusted-Metal 5) Uranium Ammonia)))
    (Frost-Crystal
     (build (Dioxite 100) (Chromatic-Metal 25))
     (refine 2 Frost-Crystal Dioxite)
     (refine (Dioxite 2) (or Oxygen Radon)))
    (Fungal-Mould
     (refine 2 Fungal-Mould Ammonia)
     (refine Ammonia (or Oxygen Nitrogen)))
    (Gamma-Root
     (refine 2 Gamma-Root Uranium)
     (refine (Uranium 2) (or Oxygen Radon)))
    (Gold
     (refine 125 (or Lemmium Magno=Gold Grantine))
     (refine Mordite Pugneum)
     (refine Coprite Residual-Goop)
     (refine 2 Coprite Pugneum)
     (refine 10 Ferrite-Dust Oxygen Emeril))
    (Indium
     (refine 4 Indium Chromatic-Metal))
    (Ionised-Cobalt
     (refine (Cobalt 2))
     (refine 150 TetraCobalt)
     (refine 6 Ionised-Cobalt (Oxygen 2))
     (refine 5 (Cobalt 2) (Oxygen 2))
     (refine 2 Marrow-Bulb (or Pugneum Oxygen Cobalt Ionised-Cobalt)))
    (Kelp-Sac
     (refine Salt Nitrogen))
    (Living-Slime
     (refine Viscous-Fluids))
    (Magnetised-Ferrite
     (refine (Pure-Ferrite 2))
     (refine 1 Ferrite-Dust (or Copper Carbon))
     (refine 2 Ferrite-Dust (or Cadmium Activated-Copper Condensed-Carbon))
     (refine 3 Ferrite-Dust Emeril)
     (refine 4 Ferrite-Dust (or Indium Activated-Cadmium))
     (refine 6 Ferrite-Dust Activated-Emeril)
     (refine 8 Ferrite-Dust Activated-Indium)
     (refine 4 (Magnetised-Ferrite 3) Pugneum)
     (refine 3 (Pure-Ferrite 2) Pugneum)
     (refine 10 Platinum Oxygen)
     (refine 2 Pure-Ferrite Carbon)
     (refine 3 Pure-Ferrite Condensed-Carbon)
     (refine 5 Ferrite-Dust Pure-Ferrite Silver)
     (refine 8 Ferrite-Dust Pure-Ferrite Gold)
     (refine 12 Ferrite-Dust Pure-Ferrite Platinum))     
    (Marrow-Bulb
     (refine 2 Cobalt Nitrogen)
     (refine 4 Mordite Sodium-Nitrate)
     (refine 3 Mordite Sodium))
    (Mordite
     (refine 2 (Coprite 3))
     (refine Carbon Pugneum)
     (refine 2 Condensed-Carbon Pugneum)
     (refine Di=hydrogen Condensed-Carbon))
    (Nitrogen
     (refine (Radon 3))
     (refine Radon Oxygen)
     (refine Radon Chromatic-Metal))
    (Oxygen
     (refine 150 Superoxide-Crystal)
     (refine Kelp-Sac)
     (refine 2 Kelp-Sac (or Carbon Condensed-Carbon))
     (refine 10 (or Fungal-Mould Frost-Crystal Cactus-Flesh Solanium Gamma-Root Star-Bulb) Kelp-Sac Condensed-Carbon))
    (Paraffinium
     (refine 2 Silver Oxygen)
     (refine (Star-Bulb 2) Salt)
     (refine Pyrite (or Ferrite-Dust Pure-Ferrite))
     (refine Sulphurine Ferrite-Dust)
     (refine 2 Sulphurine Pure-Ferrite)
     (refine 3 Sulphurine Magnetised-Ferrite))
    (Phosphorus
     (refine (Solanium 2) Salt)
     (refine (Dioxite 2) (or Ferrite-Dust Pure-Ferrite)))
    (Platinum
     (refine 250 (or Geodesite Iridesite))
     (refine Silver Gold)
     (refine 10 Ferrite-Dust Oxygen (Chromatic-Metal 250)))
    (Pugneum)
    (Pure-Ferrite
     (refine 150 Rare-Metal-Element)
     (refine 3 (Ferrite-Dust 2) Pugneum)
     (refine 2 Magnetised-Ferrite)
     (refine Ferrite-Dust))
    (Pyrite
     (refine Gold)
     (refine 2 Gold Oxygen)
     (refine (Cactus-Flesh 2) Salt)
     (refine Uranium (or Ferrite-Dust Pure-Ferrite)))
    (Radon
     (refine (Sulphurine 3))
     (refine Sulphurine (or Oxygen Chromatic-Metal)))
    (Residual-Goop)
    (Runaway-Mould
     (refine Living-Slime))
    (Rusted-Metal
     (refine Oxygen Ferrite-Dust)
     (refine 2 Oxygen Pure-Ferrite)
     (refine 4 Oxygen Magnetised-Ferrite))
    (Salt
     (refine 2 Chlorine)
     (refine Di=hydrogen Oxygen))
    (Silver
     (refine 250 (or Herox Aronium Dirty-Bronze)))
    (Sodium
     (refine 2 Sodium-Nitrate)
     (refine Marrow-Bulb)
     (refine 2 Marrow-Bulb Carbon))
    (Sodium-Nitrate
     (refine 150 Destablised-Sodium)
     (refine (Sodium 2))
     (refine Sodium (or Carbon Oxygen Pugneum))
     (refine 2 Sodium Condensed-Carbon)
     (refine 2 Sodium-Nitrate Oxygen)
     (refine 3 Sodium-Nitrate Pugneum)
     ;(refine 2 (Tritium 2) Catalyst) ; From wiki - but what are catalysts???
     (refine 2 (or Sodium Sodium-Nitrate) (or Coprite Marrow-Bulb Kelp-Sac))
     (refine 2 Marrow-Bulb Condensed-Carbon)
     (refine (or Sodium Magnetised-Ferrite) Nitrogen)
     (refine 2 (or Sodium-Nitrate Pure-Ferrite) Nitrogen)
     (refine Ferrite-Dust Nitrogen))
    (Solanium
     (refine 2 Solanium Phosphorus)
     (refine (Phosphorus 2) Oxygen)
     (refine (or Phosphorus Di=hydrogen) Sulphurine))
    (Star-Bulb
     (refine 2 Star-Bulb Paraffinium)
     (refine Paraffinium (or Oxygen Nitrogen)))
    (Sulphurine
     (refine (Nitrogen 3))
     (refine Nitrogen (or Oxygen Chromatic-Metal)))
    (Uranium
     (refine Phosphorus (or Ferrite-Dust Pure-Ferrite))
     (refine (Gamma-Root 2) Salt)
     (refine Radon (or Di=hydrogen Ferrite-Dust))
     (refine 2 Radon Pure-Ferrite)
     (refine 3 Radon Magnetised-Ferrite))
    (Viscous-Fluids)

    ;;
    ;; Other items
    ;;
    (Destablised-Sodium) ;TODO
    (Dirty-Bronze
     (refine (Pyrite 30)
             (or (Tritium 20) (Silver 20) (Gold 10) (Platinum 5))
             (or (Ferrite-Dust 120) (Pure-Ferrite 60))))
     (Enriched-Carbon
     (refine
      (Radon 100)
      (or (Carbon 20) (Condensed-Carbon 10))
      (or (Salt 10) (Chlorine 5))))
    (Glass
     (refine (Silver 100)))
    (Grantine
     (refine (Dioxite 30)
             (or (Tritium 20) (Silver 20) (Gold 10) (Platinum 5))
             (or (Cobalt 60) (Ionised-Cobalt 30))))
    (Herox
     (refine (Ammonia 30)
             (or (Tritium 20) (Silver 20) (Gold 10) (Platinum 5))
             (or (Cobalt 60) (Ionised-Cobalt 30))))
    (Lemmium
     (refine (Uranium 30)
             (or (Tritium 20) (Silver 20) (Gold 10) (Platinum 5))
             (or (Ferrite-Dust 120) (Pure-Ferrite 60))))
    (Magno=Gold
     (refine (Phosphorus 30)
             (or (Tritium 20) (Silver 20) (Gold 10) (Platinum 5))
             (or (Cobalt 60) (Ionised-Cobalt 30))))
    (Nitrogen-Salt
     (refine
      (Nitrogen 100)
      (or (Carbon 20) (Condensed-Carbon 10))
      (or (Salt 10) (Chlorine 5))))
    (Rare-Metal-Element
     (refine (Magnetised-Ferrite 35) (Pure-Ferrite 35) (Ferrite-Dust 35))
     (refine (Magnetised-Ferrite 25) (Oxygen 50))
     (refine (Magnetised-Ferrite 20) (Oxygen 20) (Tritium 25)))
    (Superoxide-Crystal
     (refine (Oxygen 100)(Tritium 50))
     (refine (Oxygen 40)(Tritium 50) (Uranium 50)))
    (TetraCobalt
     (refine (Cobalt 50) (Ionised-Cobalt 50))
     (refine (Cobalt 40) (Ionised-Cobalt 40) (Tritium 50)))
    (Thermic-Condensate
     (refine
      (Sulphurine 100)
      (or (Carbon 20) (Condensed-Carbon 10))
      (or (Salt 10) (Chlorine 5))))))

#;(for ((def raw-recipes))
  (apply def-recipes def))



