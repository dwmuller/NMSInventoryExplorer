#lang racket

(provide inventory-key?
         inventory-key->label)

(define (inventory-key? k)
  (match k
    [(cons 'exosuit (or 0 1)) #t]
    [(cons 'freighter 0) #t]
    [(cons 'ship (? string? n)) #t]
    [(cons 'vehicle (? string? n)) #t]
    [(cons 'chest (or 0 1 2 3 4 5 6 7 8 9)) #t]
    [_ #f]))


;;;
;;; (inventory-key->label key) -> string?
;;;
;;; key: inventory-key?
;;;
(define (inventory-key->label key)
  (match (car key)
    ['exosuit (match (cdr key)
                [0 "Exo General"]
                [1 "Exo Cargo"])]
    ['freighter (match (cdr key)
                  [0 "Freighter"])]
    ['ship    (cdr key)]
    ['vehicle (cdr key)]
    ['chest   (format "Storage ~a" (cdr key))]))

