#lang racket

;;;
;;; Define all items
;;;

(provide (struct-out item$)
         get-item
         get-item-by-save-id
         get-sorted-item-names
         item-name->label
         item->label
         label->item-name
         label->item
         add-item)

(define logging #t)

(struct item$ (name
               id
               base-value
               flags
               label) #:prefab)

(define (item-name->label sym)
  (string-replace (string-replace (symbol->string sym) "-" " ") "=" "-"))

(define (item->label item)
  (item-name->label (item$-name item)))

(define (label->item-name label)
  (string->symbol (string-replace (string-replace label "-" "=") " " "-")))

(define (label->item label)
  (get-item (label->item-name label)))

(define items (make-hasheq))

(define (get-item name [default (void)])
  (define result (hash-ref items name default))
  (when (void? result) 
    (raise-argument-error 'get-item "defined item name symbol" name))
  result)

(define items-by-save-id
  (make-hash
   (for/list ([i (hash-values items)]
              #:unless (void? (item$-id i)))
     (cons (item$-id i) i))))

(define (get-item-by-save-id id [default (void)])
  (define match (regexp-match "^\\^?([^#]+)(#[0-9]+)?$" id))
  (unless match (raise-argument-error 'get-item-by-save-id "item save identifier in form ^?id(#num)?" id))
  (define key (second match))
  (define result (hash-ref items-by-save-id key default))
  (unless result
    (when logging
      (printf "Item save identifier not found: ~a~n" key)))
  result)

(define (get-sorted-item-names)
  (sort (hash-keys items) symbol<?))

(define (add-item item)
  (hash-set! items (item$-name item) item)
  (hash-set! items-by-save-id (item$-id item) item))

         
