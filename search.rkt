#lang racket

(require data/heap)

(require "items.rkt"
         "recipes.rkt"
         "utility.rkt")

;;; Inventory functions
(define (make-inventory . assoc-list)
  (make-immutable-hasheq (filter (λ (m) (not (zero? (cdr m)))) assoc-list)))

(define (inventory-available inventory item)
  (hash-ref inventory item 0))

(define (inventory-deposit inventory item count)
  (if (zero? count)
      inventory
      (hash-update inventory item (λ (value) (+ value count)) 0)))

(define (inventory-withdraw inventory item count)
  (define avail (hash-ref inventory item 0))
  (cond
    [(< avail count)
     (raise-argument-error 'inventory-withdraw "(<= (inventory-avail inventory item))" count)]
    [(= avail count)
     (hash-remove inventory item)]
    [(> avail count)
     (hash-set inventory item (- avail count))]))

(define (inventory-set inventory item count)
  (if (zero? count)
      (hash-remove inventory item)
      (hash-set inventory item count)))

(define (inventory-withdraw+unsatisfied inventory item num)
  (define cur (hash-ref inventory item 0))
  (cond
    [(<= cur num) (values (hash-remove inventory item) (- num cur)) ]
    [(>  cur num) (values (hash-set inventory item (- cur num)) 0)]))


;;; State
(struct state (; (recipe . iterations) in approximate order
               recipe-applications
               ; Inventory that will be consumed by the sequence
               will-consume
               ; Inventory that will be produced by the sequence
               will-produce)
  #:transparent
  #:constructor-name make-state)

(define (state-net-result s item initial)
  (- (+ (inventory-available initial item)
        (inventory-available (state-will-produce s) item))
     (inventory-available (state-will-consume s) item)))

(define (state-deficit s initial)
  (for/fold ([result '()])
            ([item (hash-keys (state-will-consume s))])
    (define net (state-net-result s item initial))
    (if (negative? net)
        (cons (cons item (- net)) result)
        result)
    ))

(define (state-closed? s initial)
  (empty? (state-deficit s initial)))

;;; Cost-based search for a sequence of recipe applications to produce a quantity of an item.
;;;
;;; state:  (recipe-applications will-consume)
;;; recipe-applications: ((recipe . iter-count) ...)
;;;
;;; The first element is a list of recipes/iteration count pairs.
;;;

;;; Alternate recipes cause a proliferation of alternate states.
;;; Unsatisfied inputs engender a new set of antecedent recipes.
;;;
;;; closed state: A state with empty deficit-inventory.
;;; state cost heuristics:
;;; v1:  A*cost(consumed-inventory) + B*(sum of recipe costs)
;;;
;;; inventory cost:
;;; v1: unit count
;;;
;;; recipe cost:
;;; v1:  iter-count


(define (expand-state s avail)

  (define (add-recipe-consumption recipe iter inventory)
    (for/fold ([result inventory])
              ([input (recipe$-inputs recipe)])
      (inventory-deposit result (car input) (* iter (cdr input)))))
  
  ;(printf "Expanding ~s~n" s)
  ;; Permute the possible next recipe applications for the needed items.
  (define recipe-lists
    (for/fold ([rl '()])
              ([net-pair (state-deficit s avail)])
      (define output-item (car net-pair))
      (define needed (cdr net-pair))
      (define alternate-recipes (get-recipes-for output-item))
      (cons (for/list ([recipe alternate-recipes])
              ;; When calculating the repetitions of a recipe, we have to take
              ;; into account that it may use its own output as an input.
              (define iterations (ceiling (/ needed (recipe$-net-count recipe))))
              (cons recipe iterations))
            rl)))
  (define recipe-application-sequences
    (apply permute-lists recipe-lists))
  (for/fold ([new-states '()])
            ([app-list recipe-application-sequences])
    (cons (call-with-values
           (λ () 
             (for/fold ([new-app-list (state-recipe-applications s)]
                        [new-will-consume (state-will-consume s)]
                        [new-will-produce (state-will-produce s)])
                       ([app app-list])
               (define recipe (car app))
               (define iterations (cdr app))
               (values (cons app new-app-list)
                       (add-recipe-consumption recipe iterations new-will-consume)
                       (inventory-deposit new-will-produce (get-item (recipe$-output recipe)) (* iterations (recipe$-count recipe))))))
           make-state)
          new-states)))

(define (get-best-recipe-sequence item num initial-inventory state-cost)

  (define (states<=? s1 s2)
    (<= (state-cost s1 initial-inventory) (state-cost s2 initial-inventory)))

  (define will-consume (make-inventory (cons item num)))
  (define will-produce (make-inventory))
  (define initial-state (make-state '() will-consume will-produce))
  (define pending-states (make-heap states<=?))
  (define expanded-states (make-heap states<=?))
  (define closed-states (make-heap states<=?))
  (for ([s (expand-state initial-state initial-inventory)])
    (heap-add! pending-states s))

  ; Repeatedly expand the best state into next states by
  ; adding alternate recipe applications.
  ; State cost increases as applications are added, so
  ; any closed states (those with recipe chains that can be
  ; satisfied from inventory) will eventually bubble to the
  ; top of the heap.
  ;
  ; If we iterate too long, we return the best alternative,
  ; but the second result will indicate what we're missing to
  ; execute that recipe chain.

  ;
  ;
  ; TODO: Prioritize alternate recipe selection - prefer satisfiable.
  ;
  ; TODO: Why do we keep using X to produce more X? Used 100 Gamma-Root to make 200 GR to make 400 GR - should have a deficit of 100 GR.
  ;
  ; TODO: Provide alternatives - ties or close?
  ;
  (for ([i (in-range 50)]
        #:unless (zero? (heap-count pending-states)))
    (define best (heap-min pending-states))
    (heap-remove-min! pending-states)
    (cond
      [(state-closed? best initial-inventory)
       (heap-add! closed-states best)]
      [else
       (heap-add! expanded-states best)
       (for ([s (expand-state best initial-inventory)])
         (heap-add! pending-states s))]))
  (define best
    (cond
      [(not (zero? (heap-count closed-states)))
       (heap-min closed-states)]
      [(zero? (heap-count pending-states))
       (heap-min expanded-states)]
      [(states<=? (heap-min expanded-states) (heap-min pending-states))
       (heap-min expanded-states)]
      [else (heap-min pending-states)]))
  (values (state-recipe-applications best)
          (state-deficit best initial-inventory)))

(require racket/trace)
;(trace make-state)
;(trace make-inventory)
;(trace state-closed?)
(trace expand-state)
;(trace state)

;;; Heuristic function for a state.
(define (state-cost state initial)
  (+ (* 10 (length (state-recipe-applications state)))
     (* 0 (foldl (λ (a sum) (+ sum (cdr a))) 0 (state-recipe-applications state)))
     (* 100 (apply + (hash-values (state-will-consume state))))
     (* 10 (foldl (λ (a sum) (+ sum (cdr a))) 0 (state-deficit state initial)))))

(define (display-recipe-application r num)
  (printf "~s X ~s:~n" (recipe$-action r) num)
  (for ([i (recipe$-inputs r)])
    (printf "  ~v ~v~n" (* num (cdr i)) (item$-name (car i))))
  (printf "  -> ~v ~v~n" (* num (recipe$-count r)) (recipe$-output r)))
  
(define (display-recipe-applications recipe-applications)
  (for ([r recipe-applications])
    (display-recipe-application (car r) (cdr r))))

(define (display-best-recipe-sequence  item-name num inventory-forms)
  (define initial-inventory (apply make-inventory (map canonicalize-input-form inventory-forms)))
  (define-values (s i) (get-best-recipe-sequence (get-item item-name) num initial-inventory state-cost))
  (display-recipe-applications s)
  (display i))



