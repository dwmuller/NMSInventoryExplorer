#lang racket

(require racket/undefined)

(struct item$ (name base-value produced-by) #:transparent)
(define items (make-hash))

(struct recipe$ (action output count inputs) #:transparent)
(define all-recipes '())

(define (display-recipe-trees-for item-name [num 1] [accept-recipe (lambda (r) #t)] [depth 0] [seen '()])
  
  (define (display-recipe-tree recipe reps depth accept-recipe seen)
    (define indent (make-string (* 2 depth) #\space))
    #;(printf "~a~a~n"indent (make-string (- 80 depth) #\-))
    (printf "~a~a:" indent (recipe$-action recipe))
    (define output (recipe$-output recipe))
    (define inputs (recipe$-inputs recipe))
    (define repetitions (ceiling (/ num (recipe$-count recipe))))
    (define extra (- (* (recipe$-count recipe) repetitions) num))
    (cond [(> extra 0)
           (printf " (makes ~a extra)" extra)])
    (newline)
    (for ([input (recipe$-inputs recipe)] )
      (match input
        [(list sub-item-name count)
         (display-recipe-trees-for sub-item-name reps accept-recipe (+ depth 1) seen)])))

  (define item (hash-ref items item-name))
  (define indent (make-string (* 2 depth) #\space))
  (define recipes (item$-produced-by item))
  (printf "~a~a ~a~n" indent item-name num)
  (cond
    [(not (null? recipes))
     (for ([recipe recipes]
           #:when (and (accept-recipe recipe)
                       (null? (set-intersect seen (map car (recipe$-inputs recipe))))))
       (display-recipe-tree recipe num (+ 1 depth) accept-recipe (cons item-name seen)))]))

;;; Helper methods for defining items and their recipes.

;; Input definitions can simply name an item, with an implicit quantity of "one", or they can be
;; a (quantity name) list. This function normalizes a definition to a (name quantity) list.
(define (canonicalize-input input)
  (match input
    [(list n item) #:when (integer? n) (symbol? item) (list item n)]
    [item #:when (symbol? item) (list item 1)]))

;; Transforms an input list to a list of lists of inputs, canonicalizing the input definitions
;; and expanding (or input ...) lists into lists of alternatives. The result serves as a basis
;; for forming all the possible mutations of an input lists that uses or-forms.
(define (input->alternatives-list input-spec)
  (match input-spec
    [(list 'or alts ...) (map canonicalize-input alts)]
    [input (list (canonicalize-input input))]))

;; Given a list of lists, return a list of all the possible permutations.
;; Each permutation has an element from the first sub-list, then one from
;; the second sub-list, etc.
(define (permute-lists list-of-lists)
  (match list-of-lists
    [(list) '()]
    [(list only) (map list only)]
    [(list head-list remainder ...)
     (for*/list ([head head-list] [tail (permute-lists remainder)])
       (list* head tail))]))

;; Canonicalize, expand, and permute the input list of a recipe.
(define (permute-input-list input-specs)
  (permute-lists (map input->alternatives-list input-specs)))

;; Parse and canonicalize a recipe definition, returning a recipe$ struct instance.
(define (parse-recipe target recipe)
  (match recipe
    [(list 'build output-count inputs ...)
     #:when (integer? output-count)
     (map (lambda (inputs) (recipe$ 'build target output-count inputs)) (permute-input-list inputs))]
    [(list 'build inputs ...)
     (map (lambda (inputs) (recipe$ 'build target 1 inputs)) (permute-input-list inputs))]
    [(list 'refine output-count inputs ...)
     #:when (integer? output-count)
     (map (lambda (inputs) (recipe$ 'refine target output-count inputs)) (permute-input-list inputs))]
    [(list 'refine  inputs ...)
     (map (lambda (inputs) (recipe$ 'refine target 1 inputs)) (permute-input-list inputs))]))

;; Parse and record a resource and its recipes.
(define (def-item name base-value recipes)
  (define r (append-map (lambda (recipe) (parse-recipe name recipe)) recipes))
  (hash-set! items name (item$ name base-value r))
  (set! all-recipes (append r all-recipes)))

;; A convenient macro to parse and record a resource and its recipes.
(define-syntax-rule (define-item name base-value recipes ...)
  (def-item 'name base-value (quote (recipes ...))))


;; Resources
(define-item Activated-Cadmium 450)
(define-item Activated-Copper 245)
(define-item Activated-Emeril 696)
(define-item Activated-Indium 949)
(define-item Ammonia 62
  (refine Salt (2 Fungal-Mould))
  (refine Nitrogen Di-Hydrogen)
  (refine (or Ferrite-Dust Pure-Ferrite) (2 Paraffinium)))
(define-item Cactus-Flesh 28
  (refine 2 Cactus-Flesh Pyrite)
  (refine (2 Pyrite) (or Oxygen Sulphurine)))
(define-item Cadmium 234
  (refine 4 Cadmium Chromatic-Metal))
(define-item Carbon 7
  (refine 2 (or Cactus-Flesh Condensed-Carbon Frost-Crystal Gamma-Root Fungal-Mould Solanium Star-Bulb))
  (refine Oxygen))
(define-item Chlorine 602
  (refine (2 Salt))
  (refine 5(2 Salt) (2 Oxygen))
  (refine 150 Chloride-Lattice)
  (refine 6 Chlorine (2 Oxygen))
  (refine 2 Kelp-Sac (or Oxygen Pugneum Salt Chlorine)))
(define-item Chromatic-Metal 245
  (refine 1 (2 Copper))
  (refine 1 Cadmium)
  (refine 3 (2 Emeril))
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
(define-item Cobalt 198
  (refine 2 Ionised-Cobalt))
(define-item Condensed-Carbon 24
  (refine 150 Carbon-Crystal)
  (refine (2 Carbon))
  (refine 5 (2 Carbon) (2 Oxygen))
  (refine 6 Condensed-Carbon (2 Oxygen))
  (refine 2 Coprite (or Mordite (2 Carbon)))
  (refine 3 Coprite Condensed-Carbon)
  (refine 2 Uranium Di-hydrogen))
(define-item Copper 110
  (refine 4 Copper Chromatic-Metal))
(define-item Coprite 30
  (refine (3 Mordite))
  (refine Di-hydrogen Carbon)
  (refine 3 Coprite Oxygen)
  (refine Nitrogen Sulphurine)
  (refine 3 Mordite (2 Carbon))
  (refine 4 Mordite Condensed-Carbon))
(define-item Deuterium 34
  (refine Di-hydrogen Tritium))
(define-item Di-hydrogen 34
  (refine 50 Di-hydrogen-Jelly)
  (refine (5 Tritium)))
(define-item Dioxite 62
  (refine 2 Condensed-Carbon Sodium-Nitrate)
  (refine Carbon Sodium-Nitrate)
  (refine (2 Frost-Crystal) Salt)
  (refine (2 Ammonia) (or Ferrite-Dust Pure-Ferrite)))
(define-item Emeril 275
  (refine 4 Emeril Chromatic-Metal))
(define-item Ferrite-Dust 14
  (refine (or Paraffinium Dioxite Phosphorus Pyrite (5 Rusted-Metal) Uranium Ammonia)))
(define-item Frost-Crystal 12
  (build (100 Dioxite) (25 Chromatic-Metal))
  (refine 2 Frost-Crystal Dioxite)
  (refine (2 Dioxite) (or Oxygen Radon)))
(define-item Fungal-Mould 16
  (refine 2 Fungal-Mould Ammonia)
  (refine Ammonia (or Oxygen Nitrogen)))
(define-item Gamma-Root 16
  (refine 2 Gamma-Root Uranium)
  (refine (2 Uranium) (or Oxygen Radon)))
(define-item Gold 202
  (refine 125 (or Lemmium Magno-Gold Grantine))
  (refine Mordite Pugneum)
  (refine Coprite Residual-Goop)
  (refine 2 Coprite Pugneum)
  (refine 10 Ferrite-Dust Oxygen Emeril))
(define-item Indium 464
  (refine 4 Indium Chromatic-Metal))
;;; TODO: Recipes are incomplete from here on.
(define-item Ionised-Cobalt 401)
(define-item Kelp-Sac 41)
(define-item Living-Slime 20)
(define-item Magnetised-Ferrite 82)
(define-item Marrow-Bulb 41)
(define-item Mordite 40)
(define-item Nitrogen 20)
(define-item Oxygen 34)
(define-item Paraffinium 62)
(define-item Phosphorus 62)
(define-item Platinum 303)
(define-item Pugneum 138)
(define-item Pure-Ferrite 14)
(define-item Pyrite 62)
(define-item Radon 20
  (refine (3 Sulphurine))
  (refine Sulphurine (or Oxygen Chromatic-Metal)))
(define-item Residual-Goop 20)
(define-item Runaway-Mould 20)
(define-item Rusted-Metal 20)
(define-item Salt 299)
(define-item Silver 101
  (refine 250 Aronium))
(define-item Sodium 41)
(define-item Sodium-Nitrate 82)
(define-item Solanium 70)
(define-item Star-Bulb 32)
(define-item Sulphurine 20
  (refine (3 Nitrogen))
  (refine Nitrogen (or Oxygen Chromatic-Metal)))
(define-item Tritium 6)
(define-item Uranium 62)
(define-item Viscous-Fluids 20)

;; Other items
(define-item Aronium undefined
  (build (50 Paraffinium) (50 Ionised-Cobalt))
  (refine (30 Paraffinium)
          (or (20 Tritium) (20 Silver) (10 Gold) (5 Platinum))
          (or (60 Cobalt) (30 Ionised-Cobalt))))
(define-item Circuit-Board undefined
  (build Heat-Capacitor Poly-Fibre))
(define-item Cryo-Pump 150000000
  (build Hot-Ice Thermic-Condensate))
(define-item Cryogenic-Chamber undefined
  (build Living-Glass Cryo-Pump))
(define-item Enriched-Carbon undefined
  (refine
   (100 Radon)
   (or (20 Carbon) (10 Condensed-Carbon))
   (or (10 Salt) (5 Chlorine))))
(define-item Glass undefined
  (build (50 Frost-Crystal))
  (refine (100 Silver)))
(define-item Grantine undefined
  (build (50 Dioxite) (50 Ionised-Cobalt))
  (refine (30 Dioxite)
          (or (20 Tritium) (20 Silver) (10 Gold) (5 Platinum))
          (or (60 Cobalt) (30 Ionised-Cobalt))))
(define-item Heat-Capacitor undefined
  (build (100 Frost-Crystal) (200 Solanium)))
(define-item Hot-Ice undefined
  (build Nitrogen-Salt Enriched-Carbon))
(define-item Iridesite undefined
  (build Aronium Magno-Gold Grantine))
(define-item Lemmium undefined)
(define-item Living-Glass undefined
  (build Lubricant (5 Glass)))
(define-item Lubricant undefined
  (build (50 Coprite) (400 Gamma-Root)))
(define-item Magno-Gold undefined
  (build (50 Phosphorus) (50 Ionised-Cobalt))
  (refine (30 Phosphorus)
          (or (20 Tritium) (20 Silver) (10 Gold) (5 Platinum))
          (or (60 Cobalt) (30 Ionised-Cobalt))))
(define-item Nitrogen-Salt undefined
  (build (250 Nitrogen) (50 Condensed-Carbon))
  (refine
   (100 Nitrogen)
   (or (20 Carbon) (10 Condensed-Carbon))
   (or (10 Salt) (5 Chlorine))))
(define-item Poly-Fibre undefined
  (build (100 Cactus-Flesh) (200 Star-Bulb)))
(define-item Quantum-Processor undefined
  (build Circuit-Board Superconductor))
(define-item Semiconductor undefined
  (build Thermic-Condensate Nitrogen-Salt))
(define-item Stasis-Device undefined
  (build Quantum-Processor Cryogenic-Chamber Iridesite))
(define-item Superconductor undefined
  (build Semiconductor Enriched-Carbon))
(define-item Thermic-Condensate undefined
  (refine
   (100 Sulphurine)
   (or (20 Carbon) (10 Condensed-Carbon))
   (or (10 Salt) (5 Chlorine))))

;; Validate definitions.


