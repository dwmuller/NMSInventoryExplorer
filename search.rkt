#lang racket

(require data/heap)
(require racket/generator)

(require "items.rkt"
         "recipes.rkt"
         "utility.rkt"
         "inventory.rkt")

(provide get-best-recipe-sequence
         display-best-recipe-sequence) 

(define logging #t)

;;; State
;;;
;;; A state represents a potential sequence of recipe applications and the
;;; deficit at the start of the sequence, needed to complete the sequence.
;;;
;;; The initial state consists of an empty sequence, and a deficit representing
;;; the products that are wanted to withdraw from inventory.
;;;
;;; A success state has an empty deficit inventory.
;;;
;;; A state is expanded by selecting a deficit item, a recipe that will produce it,
;;; and a number of iterations of that recipe to wipe the deficit out. Order matters.
;;; The number of permutations is large, and some permutations are equivalent.
;;; Some recipe applications produce more output than necessary; the extra is ignored,
;;; but can play a role in preferring a state that performs that recipe earlier in the sequence,
;;; if the extra input can be used.

(struct state (; heuristic cost
               cost
               ; (recipe . iterations) in approximate order:
               recipe-applications
               ; Deficit at start of sequence:
               deficit)
  #:transparent)

(define (make-state recipe-applications deficit initial)
  (state (calc-cost recipe-applications deficit initial) recipe-applications deficit))

(define (state-closed? s)
  (inventory-empty? (state-deficit s)))

;;; Heuristic functions for a state.

;; estimate number of batches or crafting steps.
(define (steps recipe-application)
  (define recipe (car recipe-application))
  (define iterations (cdr recipe-application))  
  (max (ceiling (/ (* iterations (recipe$-count recipe)) 250))
       (apply max (map
                   (λ (input)
                     (ceiling (/ (* iterations (cdr input)) 250)))
                   (recipe$-inputs recipe)))))

(define (calc-cost recipe-applications deficit initial)
  (+ (* 0 (length recipe-applications))
     (* 10 (foldl (λ (a sum) (+ sum (steps a))) 0 recipe-applications))
     (* 0 (foldl (λ (a sum) (+ sum (cdr a))) 0 recipe-applications))
     (* 100 (hash-count deficit))
     (* 0 (apply + (hash-values deficit)))))

;;; Cost-based search for a sequence of recipe applications to produce a quantity of an item.
;;;
;;; deficit(item) =>
;;;   initial-deficit(item)
;;;   + sum(inputs(item)*iter)
;;;   - sum(output(item)*iter)
;;;   - initial-inventory(item)
(define (expand-state s initial-inventory final-withdrawl)

  (define (withdraw item count inventory deficit)
    (define-values (i shortfall)
      (inventory-withdraw+unsatisfied inventory item count))
    (values i (inventory-deposit deficit item shortfall)))
    
  (define (apply-recipe recipe iterations inventory deficit)
    (define-values (i d)
      (for/fold ([new-inventory inventory]
                 [new-deficit deficit])
                ([input (recipe$-inputs recipe)])
        (withdraw (car input) (* iterations (cdr input)) new-inventory new-deficit)))
    (values (inventory-deposit i (recipe$-output recipe) (* iterations (recipe$-count recipe)))
            d))
    
  (define (calc-inventories recipe-applications initial-inventory final-withdrawl)
    (define-values (i d)
      (for/fold ([inventory initial-inventory]
                 [deficit (make-inventory)])
                ([recipe-app recipe-applications])
        (apply-recipe (car recipe-app) (cdr recipe-app) inventory deficit)))
    (for/fold ([inventory i]
               [deficit d])
              ([(item count) final-withdrawl])
      (withdraw item count inventory deficit)))
      
  (generator ()
             (for* ([(target-item count-needed) (state-deficit s)])
               (for ([recipe (get-recipes-for target-item)])
                 ;; When calculating the repetitions of a recipe, the net count takes
                 ;; into account that it may use its own output as an input.
                 (define iterations (ceiling (/ count-needed (recipe$-net-count recipe))))
                 (define applications (cons (cons recipe iterations) (state-recipe-applications s)))
                 ;; Calculate the deficit by simulating the recipe applications in forward
                 ;; direction. (Calculating this incrementally is hard.)
                 (define-values (inventory deficit)
                   (calc-inventories applications initial-inventory final-withdrawl))
                 (yield (make-state
                         applications
                         deficit
                         initial-inventory))))
             (void)))

(define (debug-monitor iters expanded pending solved)
  (when (and logging (zero? (modulo iters 100)))
    (printf "Iteration ~a: Expanded: ~a, Pending: ~a, Solutions: ~a~n"
            iters expanded pending solved)))

;;
;; Search for the best available recipe chain to produce {num} {item}s.
;;
;; -> (list recipe-application ...) deficit-inventory
;; recipe-application: (cons recipe$? integer?)
;;
;; item: craftable-item? (see recipes.rkt)
;;
;; If deficit-inventory is empty, then the recipe chain can be executed entirely
;; from the initial inventory and intermediate products.
;;
;; If no recipe chain at all can be found, returns (values #f #f).
;; This should only happen if the requested item is not craftable.
;;
;; An empty recipe sequence indicates that the request can be satisfied
;; from initial inventory.
;; 
(define (get-best-recipe-sequence item num initial-inventory [continue? (if logging debug-monitor (λ (ignored ...) #t))])  
  (when (not (craftable-item? item))
    (raise-argument-error 'not-craftable "(craftable-item? item)" item))
  (define (states<=? s1 s2)
    (<= (state-cost s1) (state-cost s2)))
  (define (heap-empty? heap) (zero? (heap-count heap)))
  (define (heap-any? heap)  (not (zero? (heap-count heap))))

  (define final-withdrawl (make-inventory (cons item num)))
  (define initial-deficit (make-inventory (cons item (inventory-shortfall initial-inventory item num))))
  (define initial-state (make-state '() initial-deficit initial-inventory))
  (define pending-states (make-heap states<=?))
  (define expanded-states (make-heap states<=?))
  (define closed-states (make-heap states<=?))

  (heap-add! pending-states initial-state)
  
  ; On each iteration, pop the best pending state and examine it.
  ; If it's closed (i.e. it represents a recipe sequence that can
  ; be executed against the initial inventory), add it to the closed
  ; states. Otherwise expand it into all next possible states, each
  ; of which has starts with another possible recipe application to
  ; generate something that's still missing in order to execute
  ; the best state's recipe sequence.
  ;
  ; State cost increases as recipe applications are added, so
  ; any closed states (those with recipe chains that can be
  ; satisfied from inventory) will eventually bubble to the
  ; top of the heap.
  ;
  ; If we iterate too long, we return the best alternative,
  ; but the second result will indicate what we're missing to
  ; execute that recipe chain.
  ;
  ; TODO: Provide alternatives - ties or close?
  ; TODO: Continue past first closed state to find better solutions?
  ;
  (for ([i (in-range 10000)]
        #:unless (zero? (heap-count pending-states)))
    #:break (not (zero? (heap-count closed-states)))
    #:break (not (continue? i (heap-count expanded-states) (heap-count pending-states) (heap-count closed-states)))
    (define best (heap-min pending-states))
    (heap-remove-min! pending-states)
    (cond
      [(state-closed? best)
       (heap-add! closed-states best)]
      [else
       (heap-add! expanded-states best)
       (for ([s (in-producer (expand-state best initial-inventory final-withdrawl) (void))])
         (heap-add! pending-states s))]))
  
  ; Now interpret the results.
  (define best
    (cond
      [(heap-any?   closed-states)    (heap-min closed-states)]
      [else
       ; If the best expanded state has an empty sequence, discard it,
       ; because it's trivial. ("Make X by acquiring X.")
       (when (and (heap-any? expanded-states)
                  (null? (state-recipe-applications (heap-min expanded-states))))
         (heap-remove-min! expanded-states))
       ; Now choose the best state available from either expanded or pending.
       (define e (and (heap-any? expanded-states) (heap-min expanded-states)))
       (define p (and (heap-any? pending-states) (heap-min pending-states)))
       (cond
         [(and e p) (if (states<=? e p) e p)]
         [e e]
         [p p]
         [else (raise-result-error 'unexpected-search-result
                                   "(or (heap-any? closed-states expanded-states pending-states))"
                                   closed-states
                                   expanded-states
                                   pending-states)])]))
  
  (values (and best (state-recipe-applications best))
          (and best (state-deficit best))))

;;; Display functions for development
(define (display-recipe-application app)
  (define r (car app))
  (define num (cdr app))
  (printf "~s X ~s:~n" (recipe$-action r) num)
  (for ([i (recipe$-inputs r)])
    (printf "  ~v ~v~n" (* num (cdr i)) (item$-name (car i))))
  (printf "  -> ~v ~v~n" (* num (recipe$-count r)) (item$-name (recipe$-output r))))
  
(define (display-recipe-applications recipe-applications)
  (for ([a recipe-applications])
    (display-recipe-application a)))

(define (display-best-recipe-sequence  item-name num inventory)
  (define initial-inventory inventory)
  (define-values (s i) (get-best-recipe-sequence (get-item item-name) num initial-inventory))
  (display-recipe-applications s)
  (display i))



