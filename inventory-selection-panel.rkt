#lang racket/gui

(provide inventory-selection-panel% inventory-key->label)

(require table-panel)

;;
;; Inventory selection panel.
;;
(define inventory-selection-panel%
  (class vertical-panel%
    (init [(init-callback callback)])
    (super-new)

    ;
    ; Add "All" and "None" buttons to panels that display
    ; a sequence of checkboxes.
    ; 
    (define (add-quick-check-box-buttons! parent check-box-container)
      (define container
        (new horizontal-panel% [parent parent] [alignment '(center top)]))
      (define (all-true . ignored)
        (define changed
          (for/fold ([result #f])
                    ([c (send check-box-container get-children)]
                     #:when (is-a? c check-box%))
            (define value (send c get-value))
            (send c set-value #t)
            (or result (not value))
            ))
        (when changed 
          (on-checkbox-changed)))
      (define (all-false . ignored)
        (define changed
          (for/fold ([result #f])
                    ([c (send check-box-container get-children)]
                     #:when (is-a? c check-box%))
            (define value (send c get-value))
            (send c set-value #f)
            (or result value)))
        (when changed 
          (on-checkbox-changed)))
      (new button%
           [parent container]
           [label "All"]
           [callback all-true])
      (new button%
           [parent container]
           [label "None"]
           [callback all-false])
      (void))
  
    ;;;
    ;;; Checkbox class that also stores an inventory key to indicate
    ;;; the inventory that it relates to. 
    ;;;
    (define inventory-check-box%
      (class check-box%
        (init parent inventory-key)
        (define key inventory-key)
        (super-new [parent parent]
                   [label (inventory-key->label key)]
                   [callback (λ (ignored ...) ( on-checkbox-changed))])
        (define/public (get-inventory-key) key)))


    (define callback init-callback)
    
    (define basic-inventories-selections
      (new group-box-panel%
           [parent this]
           [label "Inventories"]))
    (define basic-inventories-check-boxes (new horizontal-pane% [parent basic-inventories-selections]))
    (define exosuit-general
      (new inventory-check-box%
           [parent basic-inventories-check-boxes]
           [inventory-key '(exosuit . 0)]
           [value #t]))
    (define exosuit-cargo
      (new inventory-check-box%
           [parent basic-inventories-check-boxes]
           [inventory-key '(exosuit . 1)]
           [value #t]))
    (define freighter
      (new inventory-check-box%
           [parent basic-inventories-check-boxes]
           [inventory-key '(freighter . 0)]))
    (add-quick-check-box-buttons! basic-inventories-selections basic-inventories-check-boxes)

    (define vessels (new horizontal-pane% [parent this]))

    (define ship-selections
      (new group-box-panel%
           [parent vessels]
           [label "Starship inventories"]))
    (define ship-check-boxes
      (new vertical-pane%
           [parent ship-selections]
           [alignment '(left top)]))
    (add-quick-check-box-buttons! ship-selections ship-check-boxes)

    (define vehicle-selections
      (new group-box-panel%
           [parent vessels]
           [label "Vehicle inventories"]))
    (define vehicle-check-boxes
      (new vertical-pane%
           [parent vehicle-selections]
           [alignment '(left top)]))
    (add-quick-check-box-buttons! vehicle-selections vehicle-check-boxes)

    (define chest-selections
      (new group-box-panel% [parent this]
           [label "Storage inventories"]))
    (define chest-check-boxes
      (new table-panel%
           [parent chest-selections]
           [dimensions '(2 1000)]
           [major-axis 'column]))
    (for ([i (in-range 10)])
      (new inventory-check-box%
           [parent chest-check-boxes]
           [inventory-key (cons 'chest  i)]))

    (add-quick-check-box-buttons! chest-selections chest-check-boxes)

    (define/public (get-selected-inventory-keys)
      (define (selected lst)
        (map (λ (cb) (send cb get-inventory-key))
             (filter (λ (cb) (send cb get-value)) lst)))
      (append (selected (send basic-inventories-check-boxes get-children))
              (selected (send ship-check-boxes get-children))
              (selected (send vehicle-check-boxes get-children))
              (selected (send chest-check-boxes get-children))))


    (define (update-inventory-check-boxes! container available-keys)
      (send container change-children
            (λ (children)
              ; Make sure there's an inventory checkbox for each key, keeping existing
              ; ones where possible.
              (for/list ([key available-keys])
                (define child (findf (λ (c) (equal? key (send c get-inventory-key))) children))
                (if child
                    child
                    (new inventory-check-box%
                         [parent container]
                         [inventory-key key]))))))
        
    (define/public (set-inventory-keys lst)
      (update-inventory-check-boxes! ship-check-boxes (filter (λ (k) (eq? (car k) 'ship)) lst))
      (update-inventory-check-boxes! vehicle-check-boxes (filter (λ (k) (eq? (car k) 'vehicle)) lst)))

    (define (on-checkbox-changed)
      (callback (get-selected-inventory-keys)))
    
    ))

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

