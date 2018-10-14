#lang racket/gui

(require racket/gui/base
         racket/path
         racket/class
         framework
         table-panel)

(require "save-file.rkt"
         "items.rkt"
         "inventory.rkt")


(define (inventory-type? sym)
  (member sym '(exosuit freighter ship vehicle chest)))

(define (inventory-key? key)
  (match key
    [(list (? inventory-type?) (? integer?)) #t]
    [else #f]))

;;;
;;; (inventory-key->label key) -> string?
;;;
;;; key: inventory-key?
;;;
(define (inventory-key->label key)
  (match (car key)
    ['exosuit (match (cdr key)
                [0 "Exosuit General"]
                [1 "Exosuit Cargo"])]
    ['freighter (match (cdr key)
                  [0 "Freighter"])]
    ['ship    (list-ref (game-inventories-starships current) (cdr key))]
    ['vehicle (list-ref (game-inventories-vehicles current) (cdr key))]
    ['chest   (format "Storage ~a" (cdr key))]))

(define (item->label item)
  ;; TODO: Prettify item name.
  (symbol->string (item$-name item)))

;;;
;;; Checkbox class that also stores an inventory key to indicate
;;; the inventory that it relates to. 
;;;
(define inventory-check-box%
  (class check-box%
    (init inventory-key)
    (define key inventory-key)
    (super-new [label (inventory-key->label key)])
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
  (new button%
       [parent container]
       [label "All"]
       [callback (λ (b e)
                   (for ([c (send check-box-container get-children)]
                         #:when (is-a? c check-box%))
                     (send c set-value #t))
                   (inventory-selection-changed))])
  (new button%
       [parent container]
       [label "None"]
       [callback (λ (b e)
                   (for ([c (send check-box-container get-children)]
                         #:when (is-a? c check-box%))
                     (send c set-value #f)
                     (inventory-selection-changed)))])
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
                     [inventory-key key]
                     [callback inventory-selection-changed]))))))

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
  (set! current (get-game-inventories path))
  (set! keyed-inventories (game-inventories-inventories current))
  (set! total-of-selected-inventories (calc-totals-inventory))
  (update-inventory-check-boxes! ship-check-boxes (available-keys-for 'ship))
  (update-inventory-check-boxes! vehicle-check-boxes (available-keys-for 'vehicle))
  (update-inventory-check-boxes! chest-check-boxes (available-keys-for 'chest))
  (show-inventory-content-in-table-panel inventory-header-panel inventory-data-panel)
  )

(define (inventory-selection-changed . ignored)
  (set! total-of-selected-inventories (calc-totals-inventory))
  (show-inventory-content-in-table-panel inventory-header-panel inventory-data-panel))

(define (show-inventory-content-in-table-panel header table)
  (send inventories begin-container-sequence)
  ;
  ; Note: For a child to be deletable, it must implement window<%>, so
  ; pane% classes do not qualify.
  ;
  (define selected-keys (selected-inventory-keys))
  (define selected-inventories
    (map cdr
         (map (λ (k) (assoc k keyed-inventories))
              selected-keys)))

  (define (make-inventory-column heading i items)
    (define column (new vertical-panel%
                        [parent table]
                        [alignment '(right center)]))
    (new message%
         [parent (new panel%
                      [parent header]
                      [alignment '(right center)]
                      [style '(border)])]
         [label heading])
    (for ([item items])
      (define value (inventory-available i item #f))
      (new message% [parent column] [label (if value (number->string value) ".")])))

  (send header change-children (λ (ignored) '()))
  (send table change-children (λ (ignored) '()))

  (define items (sort (hash-keys total-of-selected-inventories) symbol<? #:key item$-name))
  (make-inventory-column "Total" total-of-selected-inventories items)

  (new message%
       [parent (new panel%
                    [parent header]
                    [alignment '(left center)]
                    [style '(border)])]
       [label "Item"])
  (define names-column (new vertical-panel% [parent table] [alignment '(left center)]))
  (for ([item items])
    (new message% [parent names-column] [label (item->label item)]))

  (for ([key selected-keys]
        [i selected-inventories])
    (make-inventory-column (inventory-key->label key) i items))

  ; Add a dummy spacer at the end of headings to match the vertical scrollbar
  ; in the data area, so that width calcs work. Width chosen empirically.
  (new message% [parent header] [label ""] [min-width 14])
  
  ; Now adjust widths of headings and data columns to match:
  (for ([h (send header get-children)]
        [d (send table get-children)])
    (define-values (wh hh) (send h get-graphical-min-size))
    (define-values (wd hd) (send d get-graphical-min-size))
    (if (< wh wd)
        (send h min-width wd)
        (send d min-width wh)))
  (send inventories end-container-sequence)
  )

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
       [inventory-key '(exosuit . 0)]
       [callback inventory-selection-changed]))
(define exosuit-cargo
  (new inventory-check-box%
       [parent basic-inventories-check-boxes]
       [inventory-key '(exosuit . 1)]
       [callback inventory-selection-changed]))
(define freighter
  (new inventory-check-box%
       [parent basic-inventories-check-boxes]
       [inventory-key '(freighter . 0)]
       [callback inventory-selection-changed]))
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
       [choices '("Inventories" "Totals" "Craft" "Suggest")]))

(define tab-data-area
  (new panel:single% [parent tab-area]))

;
; Inventories panel, one of the tab choices.
;
; Shows totals, and details in currently selected inventories.
;
(define inventories            (new vertical-panel% [parent tab-data-area]))
(define inventory-header-panel
  (new horizontal-panel%
       [parent inventories]
       [stretchable-height #f]))
(define inventory-data-panel
  (new horizontal-panel%
       [parent inventories]
       [style '(vscroll)]))

;;;
;;; Ready, set, go ...
;;;
(load-data! (get-latest-save-file-path))
(send tab-data-area active-child inventories)

          

(send frame show #t)

