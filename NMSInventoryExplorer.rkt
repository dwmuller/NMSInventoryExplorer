#lang racket/gui

(require racket/gui/base
         racket/path
         racket/class
         framework)

(require "inventory-selection-panel.rkt"
         "recipe-finder-panel.rkt"
         "item-explorer-panel.rkt"
         "save-file.rkt"
         "items.rkt"
         "inventory.rkt"
         "data-table.rkt"
         "generated-items.rkt" ; Defines items, provides nothing.
         "generated-recipes.rkt")

; TODO: Show recipe list's total inventory usage.
; TODO: Enhance recipe search to produce multiple results.
; TODO? Improve ranking of failed recipes - prefer "common" input items?

(define (open-save-file)
  (define sfpath (finder:get-file (path-only (send save-file-path get-value))
                                  "Select Save File"
                                  "save[0-9]*\\.hg$"))
  (when sfpath
    (load-data! sfpath)
    (send save-file-path set-value (path->string sfpath))))

(define (inventory-selection-changed selected-inventory-keys)
  (set! total-of-selected-inventories (calc-totals-inventory))
  (send recipe-finder set-available-inventory total-of-selected-inventories)
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
(send file-selection set-orientation 'horizontal)
(new button%
     [label "Open Save File ..."]
     [parent file-selection]
     [callback (λ (ignored ...) (queue-callback open-save-file))])

(define save-file-path
  (new text-field%
       [parent file-selection]
       [label "File:"]
       [init-value (path->string (get-latest-save-file-path))]))

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
       [choices '("Inventories" "Item Explorer" "Recipe Finder")]))

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
;; Item Explorer panel, one of the tab choices.
;;
(define item-explorer
  (new item-explorer-panel%
       [parent tab-data-area]
       [alignment '(left top)]))

;;
;; Recipe Finder panel, one of the tab choices
;;
(define recipe-finder
  (new recipe-finder-panel%
       [parent tab-data-area]
       [alignment '(left top)]))
;;
;; Vector of tab panel choices.
;;
(define tab-panels (vector inventories-grid item-explorer recipe-finder))

(load-data! (get-latest-save-file-path))
(send tab-data-area active-child inventories-grid)

(send frame show #t)

