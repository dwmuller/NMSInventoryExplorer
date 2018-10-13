#lang racket/gui

(require racket/gui/base
         racket/path
         framework
         table-panel)

(require "save-file.rkt"
         "items.rkt"
         "inventory.rkt")

;;;
;;; Support data and functions
;;;
(define current (get-game-inventories (get-latest-save-file-path)))

(define (get-inventory-choices)
  (append (list (cons "Exosuit General" (game-inventories-exosuit-general current))
                (cons "Exosuit Cargo"   (game-inventories-exosuit-cargo current))
                (cons "Freighter"       (game-inventories-freighter current)))
          (for/list ([s (game-inventories-starships current)]
                     [i (game-inventories-starship-inventories current)])
            (cons s i))
          (for/list ([s (game-inventories-vehicles current)]
                     [i (game-inventories-vehicle-inventories current)])
            (cons s i))
          (for/list ([n (in-naturals)]
                     [i (game-inventories-storage-inventories current)])
            (cons (format "Storage ~s" n) i)))) 
            
                  

;;;
;;; Window definitions
;;;
(define frame
  (new frame%
       [label "No Man's Sky Inventory Explorer"]
       [alignment '(left top)]))

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

(define main-panel
  (new horizontal-panel%
       [parent frame]
       [alignment '(left top)]))


;;;
;;; Inventory selection controls.
;;;
(define inventory-selection
  (new vertical-panel%
       [parent main-panel]       
       [stretchable-width #f]
       [stretchable-height #f]))

(define basic-inventories
  (new horizontal-panel%
       [parent (new group-box-panel%
                    [parent inventory-selection]
                    [label "Inventories"])]))

(define exosuit-general
  (new check-box%
       [parent basic-inventories]
       [label "Exosuit General"]
       [value #t]))
(define exosuit-cargo
  (new check-box%
       [parent basic-inventories]
       [label "Exosuit Cargo"]
       [value #t]))
(define freighter
  (new check-box%
       [parent basic-inventories]
       [label "Freighter"]
       [value #f]))

;;;
;;; Adds "All" and "None" buttons to panels that display
;;; a sequence of checkboxes.
;;; 
(define (add-quick-buttons! parent checkboxes)
  (define container
    (new horizontal-panel% [parent parent] [alignment '(center top)]))
  (new button%
       [parent container]
       [label "All"]
       [callback (λ (b e)
                   (for ([c checkboxes])
                     (send c set-value #t)))])
  (new button%
       [parent container]
       [label "None"]
       [callback (λ (b e)
                   (for ([c checkboxes])
                     (send c set-value #f)))])
  (void))
  
(define vessels (new horizontal-pane% [parent inventory-selection]))

(define ships-area
  (new group-box-panel%
       [parent vessels]
       [label "Starship inventories"]
       [alignment '(left top)]))
(define ships
  (for/list ([i (in-range 3)])
    (new check-box%
         [parent ships-area]
         [label (number->string i)])))
(add-quick-buttons! ships-area ships)

(define vehicles-area
  (new group-box-panel%
       [parent vessels]
       [label "Vehicle inventories"]
       [alignment '(left top)]))
(define vehicles
  (for/list ([i (in-range 3)])
    (new check-box%
         [parent vehicles-area]
         [label (number->string i)])))
(add-quick-buttons! vehicles-area vehicles)

(define storage-boxes-area
  (new group-box-panel% [parent inventory-selection]
       [label "Storage inventories"]))
(define storage-boxes-area2
  (new horizontal-pane% [parent storage-boxes-area]))

(define storage-boxes-left (new vertical-pane% [parent storage-boxes-area2] [alignment '(left top)]))
(define storage-boxes-right (new vertical-pane% [parent storage-boxes-area2] [alignment '(left top)]))
(define storage-boxes
  (for/list ([i (in-range 10)])
    (new check-box%
         [parent (if (< i 5) storage-boxes-left storage-boxes-right)]
         [label (format "Storage ~s" i)])))
(add-quick-buttons! storage-boxes-area storage-boxes)


;;;
;;; The working bits, in a tabbed area.
;;;
(define (show-inventory-in-table-panel inventory table)
  #;(send table enable #f)
  ; NOTE WELL: set-dimensions is documented as taking rows, cols - BUT
  ; ACTUALLY TAKES cols, rows!
  (send table set-dimensions 2 (max 1 (hash-count inventory)))
  (send table change-children (λ (ignored) '()))
  (for ([key (sort (hash-keys inventory) symbol<? #:key item$-name)])
    (new message%
         [parent (new panel% [parent table] [alignment '(right center)])]
         [label (number->string (inventory-available inventory key))])
    (new message%
         [parent (new panel% [parent table] [alignment '(left center)])]
         [label (symbol->string (item$-name key))]))
  #;(send table enable #t))


(define tab-area
  (new tab-panel%
       [parent main-panel]
       [choices '("Inventories" "Totals" "Craft" "Suggest")]))

(define tab-data-area
  (new panel:single% [parent tab-area]))

(define inventories
  (new vertical-panel%
       [parent tab-data-area]
       [style '(vscroll)]))
(define inventory-choices (get-inventory-choices))
(define inventory-choice
  (new choice%
       [parent inventories]
       [label "Inventory:"]
       [choices (map car inventory-choices)]
       [callback (λ (c . ignored)
                   (show-selected-inventory))]))
(define inventory-data-panel
  (new table-panel%
       [parent inventories]
       [column-stretchability #f]
       [row-stretchability #f]))

(define (set-inventory-choices!)
  (send inventory-choice clear)
  (for ([i (map car inventory-choices)])
    (send inventory-choice append i))
  (send inventory-choice set-selection 0))

(define (show-selected-inventory)
  (show-inventory-in-table-panel
   (cdr (sequence-ref inventory-choices (send inventory-choice get-selection)))
   inventory-data-panel))

(set-inventory-choices!)
(show-selected-inventory)

;(send tab-data-area active-child inventories)

          

(send frame show #t)

