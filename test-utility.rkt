#lang racket

(require rackunit
         "utility.rkt")


(test-case "Empty permutation"
           (check-equal? (permute-lists) '()))

(test-case "Trivial permutation"
           (check-equal? (permute-lists '(a b)) '((a) (b))))

(test-case "Dual permutation"
           (check-equal? (permute-lists '(c d) '(a b)) '((c a) (c b) (d a) (d b))))

(test-case "Triple permutation"
           (check-equal?
            (permute-lists '(a b) '(c d) '(e f g))
            '((a c e) (a c f) (a c g) (a d e) (a d f) (a d g)
                      (b c e) (b c f) (b c g) (b d e) (b d f) (b d g) )))

(test-case "Non-trivial permutations with empty lists"
           (check-equal? (permute-lists '() '(c d) '(a b)) '())
           (check-equal? (permute-lists '(c d) '() '(a b)) '())
           (check-equal? (permute-lists '(c d) '(a b) '()) '()))
