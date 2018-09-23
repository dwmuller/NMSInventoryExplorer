#lang racket

(require racket/undefined)
(require racket/trace)

(struct item-info (name base-value produced-by)
  #:transparent)
(define items (make-hash))

(struct recipe (action output count input->count) #:transparent)
(define all-recipes '())


(define (canonicalize-input input)
  (match input
    [(list n item) input]
    [_ (list 1 input)]))

(define (input->alternatives-list input-spec)
  ; Expands an input spec into a list of alternatives, each canonicalized to include a count (which can be implicitly 1 in the spec).
  (match input-spec
    [(list 'or alts ...) (map canonicalize-input alts)]
    [input (list (canonicalize-input input))]))

(define (permute-lists list-of-lists)
  (match list-of-lists
    [(list) '()]
    [(list only) (map list only)]
    [(list head-list remainder ...)
     (for*/list ([head head-list] [tail (permute-lists remainder)])
       (list* head tail))]))

(define (permute-input-list input-specs)
  (permute-lists (map input->alternatives-list input-specs)))

(define (expand-recipe recipe)
  (match recipe
    [(list 'craft inputs ...)
     (map (lambda (inputs) (list* 'craft 1 inputs)) (permute-input-list inputs))]
    [(list 'refine count inputs ...)
     #:when (integer? count)
     (map (lambda (inputs) (list* 'refine count inputs)) (permute-input-list inputs))]
   [(list 'refine  inputs ...)
     (map (lambda (inputs) (list* 'refine 1 inputs)) (permute-input-list inputs))]))

(define (canonicalize-recipes recipes)
  (append-map expand-recipe recipes))

(define-syntax-rule (define-item name base-value recipes ...)
  (let ([r (canonicalize-recipes (quote (recipes ...)))])
    (hash-set! items 'name (item-info (quote name) base-value r))
    (set! all-recipes (append r all-recipes))))



;;; Resources
;;; TODO: Recipes are incomplete.
(define-item Activated-Cadmium 450)
(define-item Activated-Copper 245)
(define-item Activated-Emeril 696)
(define-item Activated-Indium 949)
(define-item Ammonia 62)
(define-item Cactus-Flesh 28)
(define-item Cadmium 234)
(define-item Carbon 7)
(define-item Chlorine 602)
(define-item Chromatic-Metal 245)
(define-item Cobalt 198)
(define-item Condensed-Carbon 24)
(define-item Copper 110)
(define-item Coprite 30)
(define-item Deuterium 34)
(define-item Di-hydrogen 34)
(define-item Dioxite 62)
(define-item Emeril 275)
(define-item Ferrite-Dust 14)
(define-item Frost-Crystal 12)
(define-item Fungal-Mould 16)
(define-item Gamma-Root 16)
(define-item Gold 202)
(define-item Indium 464)
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
(define-item Silver 101)
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



(define-item Cryo-Pump 150000000
  (craft Hot-Ice Thermic-Condensate))
(define-item Cryogenic-Chamber undefined
  (craft Living-Glass Cryo-Pump))
(define-item Enriched-Carbon undefined
  (refine
   (100 Radon)
   (or (20 Carbon) (10 Condensed-Carbon))
   (or (10 Salt) (5 Chlorine))))
(define-item Hot-Ice undefined
  (craft Nitrogen-Salt Enriched-Carbon))
(define-item Living-Glass undefined
  (craft Lubricant (5 Glass)))
(define-item Nitrogen-Salt undefined
  (refine
   (Nitrogen 100)
   (or (20 Carbon) (10 Condensed-Carbon))
   (or (10 Salt) (5 Chlorine))))
(define-item Quantum-Processor undefined
  (craft Circuit-Board Superconductor))
(define-item Semiconductor undefined
  (craft Thermic-Condensate Nitrogen-Salt))
(define-item Stasis-Device undefined
  (craft Quantum-Processor Cryogenic-Chamber Iridesite))
(define-item Super-Conductor undefined
  (craft Semiconductor Enriched-Carbon))
(define-item Thermic-Condensate undefined
  (refine
   (100 Sulphurine)
   (or (20 Carbon) (10 Condensed-Carbon))
   (or (10 Salt) (5 Chlorine))))


