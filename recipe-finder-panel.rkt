#lang racket/gui

(provide recipe-finder-panel%)

(require "inventory-key.rkt"
         "items.rkt"
         "recipes.rkt"
         "inventory.rkt"
         "search.rkt")

(define recipe-finder-panel%
  (class vertical-panel%
    (super-new)

    (define available-inventory (make-hash))
    
    (define recipe-finder-input-area
      (new horizontal-pane%
           [parent this]
           [alignment '(left top)]
           [stretchable-height #f]))
    (define recipe-finder-output-selection-area
      (new group-box-panel%
           [parent recipe-finder-input-area]
           [label "Output selection"]
           [alignment '(left top)]
           [stretchable-width #f]
           [stretchable-height #f]))
    (define recipe-finder-output-item
      (new choice%
           [parent recipe-finder-output-selection-area]
           [label "Item"]
           [choices (map item-name->label (sort (get-craftable-item-names) symbol<?))]))
    (define recipe-finder-output-quantity
      (new text-field%
           [parent recipe-finder-output-selection-area]
           [label "Quantity"]
           [init-value "1"]))
    (define recipe-finder-buttons-area
      (new horizontal-pane%
           [parent this]
           [alignment '(center top)]
           [stretchable-height #f]))
    (define recipe-finder-go
      (new button%
           [parent recipe-finder-buttons-area]
           [label "Search"]
           [callback (λ (b e) (find-recipes))]))
    (define recipe-finder-clear
      (new button%
           [parent recipe-finder-buttons-area]
           [label "Clear"]
           [callback (λ (b e) (clear-recipe-finder-output))]))
    (define recipe-finder-result-area
      (new vertical-panel%
           [parent this]
           #;[style '(auto-vscroll auto-hscroll)]))

    (define/public (set-available-inventory inventory)
      (clear-recipe-finder-output)
      (set! available-inventory inventory))
    
    (define (find-recipes)
      (define target-item (get-recipe-finder-output-item))
      (define target-count (string->number (send recipe-finder-output-quantity get-value)))
      (cond
        [(or (not target-count)
             (not (positive? target-count)))
         (message-box "Input error" "The output quantity must be a positive integer." this '(stop ok))]
        [else
         (clear-recipe-finder-output)
         (define-values (best deficit) (get-best-recipe-sequence target-item target-count available-inventory))
         (if (not (null? best))
             (show-recipe-sequence best deficit recipe-finder-result-area)
             (new message%
                  [parent recipe-finder-result-area]
                  [label "You have that in inventory already!"]))]))

    (define (show-recipe-sequence best deficit parent)  
      (when (not (inventory-empty? deficit))
        (define output
          (new group-box-panel%
               [parent parent]
               [label "Missing inputs"]
               [stretchable-height #f]))
        (send output set-orientation #t)
        (show-inventory deficit output))
      (define output-area
        (new horizontal-panel%
             [parent (new group-box-panel%
                          [parent parent]
                          [label "Steps"])]
             [style '(auto-vscroll)]
             [alignment '(left top)]))
      (define outputs-column (new vertical-pane%
                                  [parent output-area]
                                  [alignment '(left top)]
                                  [stretchable-width #f]
                                  [stretchable-height #f]))
      (define inputs-column  (new vertical-pane%
                                  [parent output-area]
                                  [alignment '(left top)]
                                  [stretchable-width #f]
                                  [stretchable-height #f]))
      (for ([app best])
        (define recipe (car app))
        (define reps (cdr app))
        (new check-box%
             [parent outputs-column]
             [label (format "~aX ~a ~a ~a"
                            reps
                            (recipe$-action recipe)
                            (item->label (recipe$-output recipe))
                            (recipe$-count recipe))]
             [stretchable-height #t])
        (define inputs
          (for/fold ([result null])
                    ([i (recipe$-inputs recipe)])
            (cons (format "~a ~a" (* (cdr i) reps) (item->label (car i))) result)))
        (new message%
             [parent (new pane%
                          [parent inputs-column]
                          [stretchable-height #t]
                          [alignment '(left center)])]
             [label (string-append " <== " (string-join inputs ", "))])
        ))

    (define (get-recipe-finder-output-item)
      (label->item (send recipe-finder-output-item get-string (send recipe-finder-output-item get-selection))))

    (define (set-recipe-finder-output-items)
      ; TODO: This should take into account known items, when we know how to determine those.
      ; At that point, the combo will need to reset on file load.
      (define old-combo recipe-finder-output-item)
      (set! recipe-finder-output-item
            (new choice%
                 [parent recipe-finder-output-selection-area]
                 [label "Item"]
                 [style '(deleted)]
                 [choices (map item-name->label
                               (sort (get-craftable-item-names) symbol<?))]))
      (send recipe-finder-output-selection-area
            change-children
            (λ (children)
              (for/list ([child children])
                (if (eq? child old-combo)
                    recipe-finder-output-item
                    child)))))

    (define (show-inventory inventory container)
      (define qty (new vertical-pane% [parent container] [alignment '(right top)] [stretchable-width #f]))
      (define name (new vertical-pane% [parent container] [alignment '(left top)]))
      (for ([key (sort (hash-keys inventory) (λ (i1 i2) (symbol<? (item$-name i1) (item$-name i2))))])
        (define count (inventory-available inventory key #f))
        (when count
          (new message% [parent qty] [label (number->string count)])
          (new message% [parent name] [label (item->label key)]))))

    (define (clear-recipe-finder-output)
      (send recipe-finder-result-area change-children (λ (ignored) null)))

    (set-recipe-finder-output-items) ; Needs to be done on file load, eventually.
    ))
