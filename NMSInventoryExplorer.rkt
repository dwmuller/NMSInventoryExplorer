#lang racket/gui

(require racket/gui/base
         racket/path
         racket/class
         framework
         table-panel)

(require "save-file.rkt"
         "items.rkt"
         "inventory.rkt"
         "data-table.rkt"
         "search.rkt"
         "recipes.rkt")

; TODO: Fix recipe search failures.
; TODO: Cull items with no recipes from output selection choice box.
; TODO: Show recipe list's total inventory usage.
; TODO: Enhance recipe search to produce multiple results.


(define (inventory-type? sym)
  (member sym '(exosuit freighter ship vehicle chest)))

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
    ['ship    (list-ref (game-data-starships current) (cdr key))]
    ['vehicle (list-ref (game-data-vehicles current) (cdr key))]
    ['chest   (format "Storage ~a" (cdr key))]))

(define (inventory-selection-changed . ignored)
  (set! total-of-selected-inventories (calc-totals-inventory))
  (define inventories (list* "Totals" (map inventory-key->label (selected-inventory-keys))))
  (define items (map item->label (sort (hash-keys total-of-selected-inventories) symbol<? #:key item$-name)))
  (send inventories-grid set-column-headers inventories)
  (send inventories-grid set-row-headers items))

;;;
;;; Checkbox class that also stores an inventory key to indicate
;;; the inventory that it relates to. 
;;;
(define inventory-check-box%
  (class check-box%
    (init inventory-key)
    (define key inventory-key)
    (super-new [label (inventory-key->label key)]
               [callback (λ (ignored ...) (queue-callback inventory-selection-changed))])
    (define/public (get-inventory-key) key)))

;;;
;;; Support data and functions
;;;
(define current (void))
(define keyed-inventories null)
(define total-of-selected-inventories (make-inventory))


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
      (queue-callback inventory-selection-changed)))
  (define (all-false . ignored)
    (define changed
      (for/fold ([result #f])
                ([c (send check-box-container get-children)]
                 #:when (is-a? c check-box%))
        (define value (send c get-value))
        (send c set-value #f)
        (or result value)))
    (when changed 
      (queue-callback inventory-selection-changed)))
  (new button%
       [parent container]
       [label "All"]
       [callback all-true])
  (new button%
       [parent container]
       [label "None"]
       [callback all-false])
  (void))
  
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

(define (available-inventory-keys) (map car keyed-inventories))

(define (available-keys-for type)
  (filter (λ (k) (eq? (car k) type))
          (available-inventory-keys)))

(define (selected-inventory-keys)
  (define (selected lst)
    (map (λ (cb) (send cb get-inventory-key))
         (filter (λ (cb) (send cb get-value)) lst)))
  (append (selected (send basic-inventories-check-boxes get-children))
          (selected (send ship-check-boxes get-children))
          (selected (send vehicle-check-boxes get-children))
          (selected (send chest-check-boxes get-children))))

(define (calc-totals-inventory)
  (for/fold ([result (make-inventory)])
            ([key (selected-inventory-keys)])
    (merge-inventories result (cdr (assoc key keyed-inventories)))))

(define (load-data! path)
  (set! current (get-game-data path))
  (set! keyed-inventories (game-data-inventories current))
  (update-inventory-check-boxes! ship-check-boxes (available-keys-for 'ship))
  (update-inventory-check-boxes! vehicle-check-boxes (available-keys-for 'vehicle))
  (update-inventory-check-boxes! chest-check-boxes (available-keys-for 'chest))
  (queue-callback inventory-selection-changed))

(define (visit-inventory-data visitor)
  
  (define items (sort (hash-keys total-of-selected-inventories) symbol<? #:key item$-name))

  (define (visit-inventory-column inventory col)
    (for ([item items] [row-index (in-naturals)])
      (define value (inventory-available inventory item #f))
      (when value
        (visitor row-index col (number->string value)))))

  (define selected-keys (selected-inventory-keys))
  (visit-inventory-column total-of-selected-inventories 0)
  (for ([ki (filter (λ (e) (member (car e) selected-keys)) keyed-inventories)]
        [col-index (in-naturals 1)])
    (visit-inventory-column (cdr ki) col-index)))

(define (select-tab panel event)
  (when (eq? 'tab-panel (send event get-event-type))
    (define selected (send panel get-selection))
    (send tab-data-area active-child (vector-ref tab-panels selected))))

(define (find-recipes)
  (define target-item (label->item (send recipe-finder-output-item get-string (send recipe-finder-output-item get-selection))))
  (define target-count (string->number (send recipe-finder-output-quantity get-value)))
  (cond
    [(or (not target-count)
         (not (positive? target-count)))
     (message-box "Input error" "The output quantity must be a positive integer." frame '(stop ok))]
    [else
     (clear-recipe-finder-output)
     (define-values (best deficit) (get-best-recipe-sequence target-item target-count total-of-selected-inventories))
     (when best
       (when (not (inventory-empty? deficit))
         (define output
           (new group-box-panel%
              [parent recipe-finder-result-area]
              [label "Missing inputs"]
              [stretchable-height #f]))
         (send output set-orientation #t)
         (show-inventory deficit output))
       (define output-area
         (new horizontal-panel%
              [parent (new group-box-panel%
                           [parent recipe-finder-result-area]
                           [label "Steps"])]
              [style '(auto-vscroll)]))
       (define outputs-column (new vertical-pane%
                                   [parent output-area]
                                   [alignment '(left center)]
                                   [stretchable-width #f]))
       (define inputs-column  (new vertical-pane%
                                   [parent output-area]
                                   [alignment '(left center)]
                                   [stretchable-width #f]))
       (for ([app best])
         (define recipe (car app))
         (define reps (cdr app))
         (new check-box%
              [parent outputs-column]
              [label (format "~aX ~a ~a ~a"
                             reps
                             (recipe$-action recipe)
                             (item->label (recipe$-output recipe))
                             (recipe$-count recipe))])
         (define inputs
           (for/fold ([result null])
                     ([i (recipe$-inputs recipe)])
             (cons (format "~a ~a" (* (cdr i) reps) (item->label (car i))) result)))
         (new message%
              [parent inputs-column]
              [label (string-append " <== " (string-join inputs ", "))])))]))

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

;;
;; Inventory selection panel.
;;
(define inventory-selection
  (new vertical-panel%
       [parent main-panel]       
       [stretchable-width #f]
       [stretchable-height #f]))

(define basic-inventories-selections
  (new group-box-panel%
       [parent inventory-selection]
       [label "Inventories"]))
(define basic-inventories-check-boxes (new horizontal-pane% [parent basic-inventories-selections]))
(define exosuit-general
  (new inventory-check-box%
       [parent basic-inventories-check-boxes]
       [inventory-key '(exosuit . 0)]))
(define exosuit-cargo
  (new inventory-check-box%
       [parent basic-inventories-check-boxes]
       [inventory-key '(exosuit . 1)]))
(define freighter
  (new inventory-check-box%
       [parent basic-inventories-check-boxes]
       [inventory-key '(freighter . 0)]))
(add-quick-check-box-buttons! basic-inventories-selections basic-inventories-check-boxes)
(send exosuit-general set-value #t)
(send exosuit-cargo set-value #t)

(define vessels (new horizontal-pane% [parent inventory-selection]))

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
  (new group-box-panel% [parent inventory-selection]
       [label "Storage inventories"]))
(define chest-check-boxes
  (new table-panel%
       [parent chest-selections]
       [dimensions '(2 1000)]
       [major-axis 'column]))
(add-quick-check-box-buttons! chest-selections chest-check-boxes)


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
       [choices (map item-name->label (get-sorted-item-names))]))
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

