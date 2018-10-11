#lang racket/gui

(require racket/gui/base
         racket/path)

(require "save-file.rkt")

(define frame
  (new frame%
       [label "No Man's Sky Inventory Explorer"]
       [alignment '(left top)]))

(define file-selection
  (new group-box-panel%
       [parent frame]
       [label "Save File"]))
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

(define general-inventory
  (new check-box%
       [parent basic-inventories]
       [label "Exosuit General"]
       [value #t]))
(define cargo-inventory
  (new check-box%
       [parent basic-inventories]
       [label "Exosuit Cargo"]
       [value #t]))
(define freighter-inventory
  (new check-box%
       [parent basic-inventories]
       [label "Freighter"]
       [value #f]))

;;;
;;; Adds "All" and "None" buttons to panels that display
;;; a sequence of checkboxes.
;;; 
(define (add-quick-buttons parent checkboxes)
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
                     (send c set-value #f)))]))
  
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
(add-quick-buttons ships-area ships)

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
(add-quick-buttons vehicles-area vehicles)

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
(add-quick-buttons storage-boxes-area storage-boxes)


;;;
;;; The working bits, in a tabbed area.
;;;
(define tab-area
  (new tab-panel%
       [parent main-panel]
       [choices '("Totals" "Details" "Craft" "Suggest")]))

(define totals
  (new horizontal-panel%
       [parent tab-area]))

  

(send frame show #t)

