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

(define (canonicalize-input input)
  (match input
    [(list n item) #:when (integer? n) (symbol? item) (list item n)]
    [item #:when (symbol? item) (list item 1)]))

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

(define (expand-recipe target recipe)
  (match recipe
    [(list 'craft output-count inputs ...)
     #:when (integer? output-count)
     (map (lambda (inputs) (recipe$ 'craft target output-count inputs)) (permute-input-list inputs))]
    [(list 'craft inputs ...)
     (map (lambda (inputs) (recipe$ 'craft target 1 inputs)) (permute-input-list inputs))]
    [(list 'refine output-count inputs ...)
     #:when (integer? output-count)
     (map (lambda (inputs) (recipe$ 'refine target output-count inputs)) (permute-input-list inputs))]
   [(list 'refine  inputs ...)
     (map (lambda (inputs) (recipe$ 'refine target 1 inputs)) (permute-input-list inputs))]))

(define (canonicalize-recipes target recipes)
  (append-map (lambda (recipe) (expand-recipe target recipe)) recipes))

(define (def-item name base-value recipes)
  (define r (canonicalize-recipes name recipes))
  (hash-set! items name (item$ name base-value r))
  (set! all-recipes (append r all-recipes)))

(define-syntax-rule (define-item name base-value recipes ...)
  (def-item 'name base-value (quote (recipes ...))))


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
(define-item Gold 202
  (refine 125 Magno-Gold)
  (refine 125 Grantine))
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


(define-item Aronium undefined
  (craft (50 Paraffinium) (50 Ionised-Cobalt))
  (refine (30 Paraffinium)
          (or (20 Tritium) (20 Silver) (10 Gold) (5 Platinum))
          (or (60 Cobalt) (30 Ionised-Cobalt))))
(define-item Circuit-Board undefined
  (craft Heat-Capacitor Poly-Fibre))
(define-item Cryo-Pump 150000000
  (craft Hot-Ice Thermic-Condensate))
(define-item Cryogenic-Chamber undefined
  (craft Living-Glass Cryo-Pump))
(define-item Enriched-Carbon undefined
  (refine
   (100 Radon)
   (or (20 Carbon) (10 Condensed-Carbon))
   (or (10 Salt) (5 Chlorine))))
(define-item Glass undefined
  (craft (50 Frost-Crystal))
  (refine (100 Silver)))
(define-item Grantine undefined
  (craft (50 Dioxite) (50 Ionised-Cobalt))
  (refine (30 Dioxite)
          (or (20 Tritium) (20 Silver) (10 Gold) (5 Platinum))
          (or (60 Cobalt) (30 Ionised-Cobalt))))
(define-item Heat-Capacitor undefined
  (craft (100 Frost-Crystal) (200 Solanium)))
(define-item Hot-Ice undefined
  (craft Nitrogen-Salt Enriched-Carbon))
(define-item Iridesite undefined
  (craft Aronium Magno-Gold Grantine))
(define-item Living-Glass undefined
  (craft Lubricant (5 Glass)))
(define-item Lubricant undefined
  (craft (50 Coprite) (400 Gamma-Root)))
(define-item Magno-Gold undefined
  (craft (50 Phosphorus) (50 Ionised-Cobalt))
  (refine (30 Phosphorus)
          (or (20 Tritium) (20 Silver) (10 Gold) (5 Platinum))
          (or (60 Cobalt) (30 Ionised-Cobalt))))
(define-item Nitrogen-Salt undefined
  (craft (250 Nitrogen) (50 Condensed-Carbon))
  (refine
   (100 Nitrogen)
   (or (20 Carbon) (10 Condensed-Carbon))
   (or (10 Salt) (5 Chlorine))))
(define-item Poly-Fibre undefined
  (craft (100 Cactus-Flesh) (200 Star-Bulb)))
(define-item Quantum-Processor undefined
  (craft Circuit-Board Superconductor))
(define-item Semiconductor undefined
  (craft Thermic-Condensate Nitrogen-Salt))
(define-item Stasis-Device undefined
  (craft Quantum-Processor Cryogenic-Chamber Iridesite))
(define-item Superconductor undefined
  (craft Semiconductor Enriched-Carbon))
(define-item Thermic-Condensate undefined
  (refine
   (100 Sulphurine)
   (or (20 Carbon) (10 Condensed-Carbon))
   (or (10 Salt) (5 Chlorine))))


