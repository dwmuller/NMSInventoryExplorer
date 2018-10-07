#lang racket

(require racket/fixnum
         racket/undefined)

(require "utility.rkt")
(require "items.rkt")

(provide (struct-out recipe$) get-recipes-for get-recipes-using canonicalize-input-form)

(struct recipe$ (action output count net-count inputs) #:transparent)
(define (make-recipe action output-name count inputs)
  (define output-item (get-item output-name))
  (define output-as-input (assoc output-item inputs))
  (define net-produced (if output-as-input
                           (- count (cdr output-as-input))
                           count))
  (recipe$ action
           output-item
           count
           net-produced
           inputs))

(define all-recipes '())
(define recipes-by-output (make-hasheq))
(define recipes-by-input (make-hasheq))

(define (get-recipes-for item-ref)
  (define item (if (item$? item-ref) item-ref (get-item item-ref)))
  (hash-ref recipes-by-output item '()))

(define (get-recipes-using item-ref)
  (define item (if (item$? item-ref) item-ref (get-item item-ref)))
  (hash-ref recipes-by-input item '()))
  
;;; Helper methods for defining recipes.

;; Input definitions can simply name an item, with an implicit quantity of "one", or they can be
;; a (quantity name) list. This function normalizes a definition to a (name quantity) list.
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

;; Parse and canonicalize a recipe definition, returning a recipe$ struct instance.
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

;; Parse and record a resource and its recipes.
(define (def-recipes name . recipe-defs)
  (define recipes (append-map (λ (recipe) (parse-recipe name recipe)) recipe-defs))
  (define output (get-item name))
  (when (hash-has-key? recipes-by-output output)
    (raise-argument-error 'def-recipes "(not (hash-has-key? (get-item name)))" name))
  (set! all-recipes (append recipes all-recipes))
  (when (not (null? recipes))
    (hash-set! recipes-by-output output recipes))
  (for ((r recipes))
    (for ((i (map car (recipe$-inputs r))))
      (hash-update! recipes-by-input i (λ (cur) (list* r cur)) '())))) 

(define raw-recipes
  '(
    ;; Resources
    (Ammonia
     (refine Salt (Fungal-Mould 2))
     (refine Nitrogen Di-hydrogen)
     (refine (or Ferrite-Dust Pure-Ferrite) (Paraffinium 2)))
    (Cactus-Flesh
     (refine 2 Cactus-Flesh Pyrite)
     (refine (Pyrite 2) (or Oxygen Sulphurine)))
    (Cadmium
     (refine 4 Cadmium Chromatic-Metal))
    (Carbon
     (refine 2 (or Cactus-Flesh Condensed-Carbon Frost-Crystal Gamma-Root Fungal-Mould Solanium Star-Bulb))
     (refine Oxygen))
    (Chlorine
     (refine (Salt 2))
     (refine 5(Salt 2) (Oxygen 2))
     (refine 150 Chloride-Lattice)
     (refine 6 Chlorine (Oxygen 2))
     (refine 2 Kelp-Sac (or Oxygen Pugneum Salt Chlorine)))
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
     (refine 2 Uranium Di-hydrogen))
    (Copper
     (refine 4 Copper Chromatic-Metal))
    (Coprite
     (refine (Mordite 3))
     (refine Di-hydrogen Carbon)
     (refine 3 Coprite Oxygen)
     (refine Nitrogen Sulphurine)
     (refine 3 Mordite (Carbon 2))
     (refine 4 Mordite Condensed-Carbon))
    (Deuterium
     (refine Di-hydrogen Tritium))
    (Di-hydrogen
     (refine 50 Di-hydrogen-Jelly)
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
    (Frigate-Fuel-50
     (build (Di-hydrogen 50) (Tritium 50)))
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
     (refine 125 (or Lemmium Magno-Gold Grantine))
     (refine Mordite Pugneum)
     (refine Coprite Residual-Goop)
     (refine 2 Coprite Pugneum)
     (refine 10 Ferrite-Dust Oxygen Emeril))
    (Indium
     (refine 4 Indium Chromatic-Metal))
    ;;; TODO: Recipes are incomplete from here on.
    (Ionised-Cobalt)
    (Kelp-Sac)
    (Living-Slime)
    (Magnetised-Ferrite)
    (Marrow-Bulb)
    (Mineral-Compressor
     (build Cobalt-Mirror (Gold 50)))
    (Mind-Control-Device
     (build Sodium-Diode (Gold 50)))
    (Mordite)
    (Nitrogen)
    (Oxygen)
    (Paraffinium)
    (Phosphorus)
    (Platinum)
    (Pugneum)
    (Pure-Ferrite)
    (Pyrite)
    (Radon
     (refine (Sulphurine 3))
     (refine Sulphurine (or Oxygen Chromatic-Metal)))
    (Residual-Goop)
    (Runaway-Mould)
    (Rusted-Metal)
    (Salt)
    (Silver
     (refine 250 Aronium))
    (Sodium)
    (Sodium-Nitrate)
    (Solanium)
    (Star-Bulb)
    (Sulphurine
     (refine (Nitrogen 3))
     (refine Nitrogen (or Oxygen Chromatic-Metal)))
    (Tritium)
    (Uranium)
    (Viscous-Fluids)

    ;; Other items
    (Acid
     (build (Mordite 25) (Fungal-Mould 600)))
    (Advanced-Ion-Battery
     (build (Ionised-Cobalt 25) (Pure-Ferrite 20)))
    (Antimatter
     (build (Chromatic-Metal 25) (Condensed-Carbon 20)))
    (Antimatter-Housing
     (build (Oxygen 30) (Ferrite-Dust 50)))
    (Aronium
     (build (Paraffinium 50) (Ionised-Cobalt 50))
     (refine (Paraffinium 30)
             (or (Tritium 20) (Silver 20) (Gold 10) (Platinum 5))
             (or (Cobalt 60) (Ionised-Cobalt 30))))
    (Carbon-Nanotubes
     (build (Carbon 50)))
    (Circuit-Board
     (build Heat-Capacitor Poly-Fibre))
    (Cobalt-Mirror
     (build (Ionised-Cobalt 50)))
    (Cryo-Pump
     (build Hot-Ice Thermic-Condensate))
    (Cryogenic-Chamber
     (build Living-Glass Cryo-Pump))
    (Di-hydrogen-Jelly
     (build (Di-hydrogen 40)))
    (Enriched-Carbon
     (refine
      (Radon 100)
      (or (Carbon 20) (Condensed-Carbon 10))
      (or (Salt 10) (Chlorine 5))))
    (Explosive-Drones
     (build Walker-Brain (Gold 50)))
    (Fuel-Oxidiser
     (build (Quad-Servo 2) (Gold 50)))
    (Geodesite
     (build Dirty-Bronze Herox Lemmium))
    (Glass
     (build (Frost-Crystal 50))
     (refine (Silver 100)))
    (Grantine
     (build (Dioxite 50) (Ionised-Cobalt 50))
     (refine (Dioxite 30)
             (or (Tritium 20) (Silver 20) (Gold 10) (Platinum 5))
             (or (Cobalt 60) (Ionised-Cobalt 30))))
    (Heat-Capacitor
     (build (Frost-Crystal 100) (Solanium 200)))
    (Hermetic-Seal
     (build (Carbon 30)))
    (Herox
     (build (Ammonia 50) (Ionised-Cobalt 50)))
    (Holographic-Analyser
     (build Oxygen-Filter (Gold 50)))
    (Hot-Ice
     (build Nitrogen-Salt Enriched-Carbon))
    (Ion-Battery
     (build (Cobalt 25) (Ferrite-Dust 20)))
    (Iridesite
     (build Aronium Magno-Gold Grantine))
    (Lemmium
     (build (Uranium 50) (Pure-Ferrite 100)))
    (Life-Support-Gel
     (build Di-hydrogen-Jelly (Carbon 20)))
    (Liquid-Explosive
     (build Acid Unstable-Gel))
    (Living-Glass
     (build Lubricant (Glass 5)))
    (Lubricant
     (build (Coprite 50) (Gamma-Root 400)))
    (Magno-Gold
     (build (Phosphorus 50) (Ionised-Cobalt 50))
     (refine (Phosphorus 30)
             (or (Tritium 20) (Silver 20) (Gold 10) (Platinum 5))
             (or (Cobalt 60) (Ionised-Cobalt 30))))
    (Metal-Plating
     (build (Ferrite-Dust 50)))
    (Microprocessor
     (build (Chromatic-Metal 40) Carbon-Nanotubes))
    (Nitrogen-Salt
     (build (Nitrogen 250) (Condensed-Carbon 50))
     (refine
      (Nitrogen 100)
      (or (Carbon 20) (Condensed-Carbon 10))
      (or (Salt 10) (Chlorine 5))))
    (Organic-Catalyst
     (build Thermic-Condensate Enriched-Carbon))
    (Oxygen-Capsule
     (build (Oxygen 25) (Ferrite-Dust 20)))
    (Oxygen-Filter
     (build (Oxygen 90) (Pure-Ferrite 30)))
    (Poly-Fibre
     (build (Cactus-Flesh 100) (Star-Bulb 200)))
    (Projectile-Ammo
     (build 500 (Ferrite-Dust 60)))
    (Quantum-Processor
     (build Circuit-Board Superconductor))
    (Salt-Refractor
     (build (Chlorine 50)))
    (Semiconductor
     (build Thermic-Condensate Nitrogen-Salt))
    (Sodium-Diode
     (build (Sodium-Nitrate 40) (Ferrite-Dust 40)))
    (Starship-Launch-Fuel
     (build (Di-hydrogen 40) Metal-Plating))
    (Stasis-Device
     (build Quantum-Processor Cryogenic-Chamber Iridesite))
    (Superconductor
     (build Semiconductor Enriched-Carbon))
    (Thermic-Condensate
     (refine
      (Sulphurine 100)
      (or (Carbon 20) (Condensed-Carbon 10))
      (or (Salt 10) (Chlorine 5))))
    (Unstable-Gel
     (build (Cactus-Flesh 200)))
    (Unstable-Plasma
     (build (Oxygen 50) Metal-Plating))
    (Warp-Cell
     (build Antimatter-Housing Antimatter))))

(for ((def raw-recipes))
  (apply def-recipes def))



