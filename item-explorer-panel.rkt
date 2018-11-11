#lang racket/gui

(provide item-explorer-panel%)

(require "items.rkt"
         "recipes.rkt")

(define item-explorer-panel%
  (class vertical-panel%
    (super-new)

    (define root-selection-area
      (new horizontal-panel%
           [parent this]
           [alignment '(left top)]
           [stretchable-height #f]))
    (define item-selection
      (new choice%
           [parent root-selection-area]
           [label "Item"]
           [choices (map item-name->label (get-sorted-item-names))]
           [callback (λ (ignore ...)
                       (show-root-item))]))    
    (define item-info
      (new vertical-pane%
           [parent root-selection-area]))

    (define recipe-explorer-panel%
      (class vertical-panel%
        (super-new [alignment '(left top)]
                   [style '(auto-vscroll)])
        (define item #f)
        (define recipes-using null)
        (define recipes-for null)

        (define/public (set-item item)
          
          (define (make-recipe-list lst)
            (define recipes-panel (new horizontal-panel%
                                      [parent this]
                                      [stretchable-height #f]))
            (define outputs-column (new vertical-pane%
                                        [parent recipes-panel]
                                        [alignment '(left top)]
                                        [stretchable-width #f]
                                        [stretchable-height #f]))
            (define inputs-column (new vertical-pane%
                                       [parent recipes-panel]
                                       [alignment '(left top)]
                                       [stretchable-width #f]
                                       [stretchable-height #f]))
            (for ([recipe lst])
              (new message%
                   [parent outputs-column]
                   [label (format "  ~a ~a"
                                  (item->label (recipe$-output recipe))
                                  (recipe$-count recipe))]
                   [stretchable-height #t])
              (define inputs
                (for/fold ([result null])
                          ([i (recipe$-inputs recipe)])
                  (cons (format "~a ~a" (cdr i) (item->label (car i))) result)))
              (new message%
                   [parent (new pane%
                                [parent inputs-column]
                                [stretchable-height #t]
                                [alignment '(left center)])]
                   [label (format " <== ~a ~a" (recipe$-action recipe) (string-join inputs ", "))]))
            recipes-panel)

          (set! recipes-using (get-recipes-using item))
          (set! recipes-for (get-recipes-for item))
          (define children null)
          (unless (null? recipes-for)
            (define recipes-for-header
              (new message%
                   [parent this]
                   [stretchable-width #t]
                   [label (format "~a recipes for ~a ..."
                                  (length recipes-for) (item->label item))]))
            (set! children (list* recipes-for-header (make-recipe-list recipes-for) children)))
        
          (unless (null? recipes-using)
            (define recipes-using-header
              (new message%
                   [parent this]
                   [stretchable-width #t]
                   [label (format "~a recipes using ~a..."
                                  (length recipes-using) (item->label item))]))
            (set! children (list* recipes-using-header (make-recipe-list recipes-using) children)))
        
        (send this change-children (λ (c) children)))))
    
    (define root-recipe-explorer (new recipe-explorer-panel% [parent this]))

    (define (show-root-item)
      (define item  (get-item (list-ref (get-sorted-item-names)
                                        (send item-selection get-selection))))
      (send item-info
            change-children
            (λ (c)
              (list
               (new message%
                    [parent item-info]
                    [label (format "Value: ~a" (item$-base-value item))])
               (new message%
                    [parent item-info]
                    [label (format "ID: ~a" (item$-id item))]))))
      (send root-recipe-explorer set-item item)
      (void))
    
    (show-root-item)


    (define (add-recipe-areas parent)
      (void))
    (define (show-recipes parent)
      (void))
    (define (hide-recipes parent)
      (void))
    (define (show-recipe-detail parent)
      (void))
    (define (hide-recipe-detail parent)
      (void))
    ))
