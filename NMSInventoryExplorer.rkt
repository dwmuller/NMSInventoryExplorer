#lang racket/gui

(require racket/gui/base
         racket/path
         racket/class
         framework)

(require "inventory-selection-panel.rkt"
         "save-file.rkt"
         "items.rkt"
         "inventory.rkt"
         "data-table.rkt"
         "search.rkt"
         "generated-items.rkt" ; Defines items, provides nothing.
         "recipes.rkt"
         "generated-recipes.rkt")

; TODO: Show recipe list's total inventory usage.
; TODO: Enhance recipe search to produce multiple results.
; TODO? Improve ranking of failed recipes - prefer "common" input items?

(define (inventory-selection-changed selected-inventory-keys)
  (set! total-of-selected-inventories (calc-totals-inventory))
  (define inventories (list* "Totals" (map inventory-key->label selected-inventory-keys)))
  (define items (map item->label (sort (hash-keys total-of-selected-inventories) symbol<? #:key item$-name)))
  (send inventories-grid set-column-headers inventories)
  (send inventories-grid set-row-headers items))

;;;
;;; Support data and functions
;;;
(define current (void))
(define keyed-inventories null)
(define total-of-selected-inventories (make-inventory))


(define (calc-totals-inventory)
  (for/fold ([result (make-inventory)])
            ([key (send inventory-selection get-selected-inventory-keys)])
    (merge-inventories result (cdr (assoc key keyed-inventories)))))


(define (available-inventory-keys) (map car keyed-inventories))

(define (available-keys-for type)
  (filter (λ (k) (eq? (car k) type))
          (available-inventory-keys)))

(define (load-data! path)
  (set! current (get-game-data path))
  (set! keyed-inventories (game-data-inventories current))
  (send inventory-selection set-inventory-keys (available-inventory-keys))
  (set-recipe-finder-output-items)
  (queue-callback (λ () (inventory-selection-changed (send inventory-selection get-selected-inventory-keys)))))

(define (visit-inventory-data visitor)
  
  (define items (sort (hash-keys total-of-selected-inventories) symbol<? #:key item$-name))

  (define (visit-inventory-column inventory col)
    (for ([item items] [row-index (in-naturals)])
      (define value (inventory-available inventory item #f))
      (when value
        (visitor row-index col (number->string value)))))

  (define selected-keys (send inventory-selection get-selected-inventory-keys))
  (visit-inventory-column total-of-selected-inventories 0)
  (for ([ki (filter (λ (e) (member (car e) selected-keys)) keyed-inventories)]
        [col-index (in-naturals 1)])
    (visit-inventory-column (cdr ki) col-index)))

(define (select-tab panel event)
  (when (eq? 'tab-panel (send event get-event-type))
    (define selected (send panel get-selection))
    (send tab-data-area active-child (vector-ref tab-panels selected))))

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
  (define old-combo recipe-finder-output-item)
  (set! recipe-finder-output-item
        (new choice%
       [parent recipe-finder-output-selection-area]
       [label "Item"]
       [style '(deleted)]
       [choices (map item-name->label
                     (sort (filter (λ (n) (set-member? (game-data-known-items current) n))
                                   (get-craftable-item-names))
                           symbol<?))]))
  (send recipe-finder-output-selection-area
        change-children
        (λ (children)
          (for/list ([child children])
            (if (eq? child old-combo)
                recipe-finder-output-item
                child)))))

(define (find-recipes)
  (define target-item (get-recipe-finder-output-item))
  (define target-count (string->number (send recipe-finder-output-quantity get-value)))
  (cond
    [(or (not target-count)
         (not (positive? target-count)))
     (message-box "Input error" "The output quantity must be a positive integer." frame '(stop ok))]
    [else
     (clear-recipe-finder-output)
     (define-values (best deficit) (get-best-recipe-sequence target-item target-count total-of-selected-inventories))
     (if (not (null? best))
         (show-recipe-sequence best deficit recipe-finder-result-area)
         (new message%
              [parent recipe-finder-result-area]
              [label "You have that in inventory already!"]))]))

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

;;;
;;; Top-level window
;;;
(define frame
  (new frame%
       [label "No Man's Sky Inventory Explorer"]
       [alignment '(left top)]))

;;;
;;; File selection/control panel.
;;;
(define file-selection
  (new group-box-panel%
       [parent frame]
       [label "Save File"]
       [stretchable-height #f]))
(define data-root
  (new text-field%
       [parent file-selection]
       [label "NMS data directory:"]
       [init-value (path->string (get-default-data-path))]))
(define save-file-path
  (new text-field%
       [parent file-selection]
       [label "File:"]
       [init-value (path->string (find-relative-path (get-default-data-path) (get-latest-save-file-path)))]))

;;;
;;; Main panel, including inventory selection and related data displays.
;;;
(define main-panel
  (new horizontal-panel%
       [parent frame]
       [alignment '(left top)]))

(define inventory-selection
  (new inventory-selection-panel%
       [parent main-panel]       
       [stretchable-width #f]
       [stretchable-height #f]
       [callback inventory-selection-changed]))

;;
;; Tab area to display inventory details, totals, and related operations.
;;
(define tab-area
  (new tab-panel%
       [parent main-panel]
       [callback select-tab]
       [choices '("Inventories" "Recipe Finder")]))

(define tab-data-area
  (new panel:single% [parent tab-area]))

;;
;; Inventories panel, one of the tab choices.
;;
;; Shows totals, and details in currently selected inventories.
;;
(define inventories-grid
  (new data-table%
       [parent tab-data-area]
       [data-visitor visit-inventory-data]
       [style '(vscroll hscroll)]
       [spacing 6]
       [default-column-vars '([alignment (right center)])]))

;;
;; Recipe Finder panel, one of the tab choices
;;
(define recipe-finder
  (new vertical-panel%
       [parent tab-data-area]
       [alignment '(left top)]))
(define recipe-finder-input-area
  (new horizontal-pane%
       [parent recipe-finder]
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
       [parent recipe-finder]
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
       [parent recipe-finder]
       #;[style '(auto-vscroll auto-hscroll)]))

;;
;; Vector of tab panel choices.
;;
(define tab-panels (vector inventories-grid recipe-finder))

(load-data! (get-latest-save-file-path))
(send tab-data-area active-child inventories-grid)

(send frame show #t)

