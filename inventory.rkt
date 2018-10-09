#lang racket

;;;
;;; Functions for managing a simple representation of an item
;;; inventory.
;;;
;;; Item amounts are guaranteed to positive and non-zero.
;;;
;;; Uses an immutable hash for internal representation, thus
;;; all inventory manipulation functions return a new inventory
;;; object.
;;;

(provide make-inventory
         inventory-available
         inventory-deposit
         inventory-withdraw
         inventory-set
         inventory-shortfall
         inventory-withdraw+unsatisfied
         inventory-empty?)

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

(define (inventory-shortfall inventory item count)
  (max 0 (- count (inventory-available inventory item))))

(define (inventory-withdraw+unsatisfied inventory item num)
  (define cur (hash-ref inventory item 0))
  (cond
    [(<= cur num) (values (hash-remove inventory item) (- num cur)) ]
    [(>  cur num) (values (hash-set inventory item (- cur num)) 0)]))

(define (inventory-empty? inventory)
  (zero? (hash-count inventory)))


