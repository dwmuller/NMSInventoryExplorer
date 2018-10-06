#lang racket

(provide permute-lists)

;; Given a list of lists, return a list of all the possible permutations.
;; Each permutation has an element from the first sub-list, then one from
;; the second sub-list, etc.
(define (permute-lists . list-of-lists)
  (match list-of-lists
    [(list) '()]
    [(list (? list? only)) (map list only)]
    [(list (? list? head-list) (? list? remainder) ...)
     (for*/list ([head head-list] [tail (apply permute-lists remainder)])
       (list* head tail))]))

