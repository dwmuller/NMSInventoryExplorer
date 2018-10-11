#lang racket

(require data/heap)
(require racket/generator)

(require "items.rkt"
         "recipes.rkt"
         "utility.rkt"
         "inventory.rkt")

(provide get-best-recipe-sequence
         display-best-recipe-sequence) 

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

(struct state (; (recipe . iterations) in approximate order:
               recipe-applications
               ; Deficit at start of sequence:
               deficit)
  #:transparent)

(define (make-state recipe-applications deficit initial)
  (state recipe-applications deficit))

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

(define (state-cost state initial)
  (+ (* 0 (length (state-recipe-applications state)))
     (* 10 (foldl (λ (a sum) (+ sum (steps a))) 0 (state-recipe-applications state)))
     (* 0 (foldl (λ (a sum) (+ sum (cdr a))) 0 (state-recipe-applications state)))
     (* 100 (hash-count (state-deficit state)))
     (* 0 (apply + (hash-values (state-deficit state))))))

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
             (for ([(target-item count-needed) (state-deficit s)])
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

(define (get-best-recipe-sequence item num initial-inventory)

  (define (states<=? s1 s2)
    (<= (state-cost s1 initial-inventory) (state-cost s2 initial-inventory)))
  (define (heap-empty? heap) (zero? (heap-count heap)))
  (define (heap-any? heap)  (not (zero? (heap-count heap))))

  (define final-withdrawl (make-inventory (cons item num)))
  (define initial-deficit (make-inventory (cons item (inventory-shortfall initial-inventory item num))))
  (define initial-state (make-state '() initial-deficit initial-inventory))
  (define pending-states (make-heap states<=?))
  (define expanded-states (make-heap states<=?))
  (define closed-states (make-heap states<=?))

  (for ([s (in-producer (expand-state initial-state initial-inventory final-withdrawl) (void))])
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
  ; TODO: Provide alternatives - ties or close?
  ; TODO: Continue past first closed state to find better solutions?
  ;
  (for ([i (in-range 10000)]
        #:unless (zero? (heap-count pending-states)))
    #:break (not (zero? (heap-count closed-states)))
    (define best (heap-min pending-states))
    (heap-remove-min! pending-states)
    (cond
      [(state-closed? best)
       (heap-add! closed-states best)]
      [else
       (heap-add! expanded-states best)
       (for ([s (in-producer (expand-state best initial-inventory final-withdrawl) (void))])
         (heap-add! pending-states s))])
    (when (zero? (modulo i 100))
      (printf "~s: ~s ~s~n" i (heap-count expanded-states) (heap-count pending-states))))
  (define best
    (cond
      [(heap-any?   closed-states)    (heap-min closed-states)]
      [(heap-empty? pending-states)   (heap-min expanded-states)]
      [else
       (define e (heap-min expanded-states))
       (define p (heap-min pending-states))
       (if (states<=? e p) e p)]))
  (values (state-recipe-applications best)
          (state-deficit best)))

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



