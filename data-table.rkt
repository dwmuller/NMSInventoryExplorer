#lang racket/gui

(require racket/generator
         racket/draw)

(provide data-table%)

; TODO: Adjust font selection in data panel, maybe configurable. (medium?)
; TODO: Check initialization variable contracts.
; TODO? Support deleted (hidden) cols.
; TODO? Support deleted (hidden) rows.
; TODO? Support non-uniform col widths and row heights. (medium)
; TODO? If we keep per-column variables, make the field an assoc list so it can be a sparse mapping. (easy, except for testing)
; TODO? Support interactive re-ordering of columns and rows. (medium)
; TODO? Support sorting by columns (And by rows?) (hard - "data visitor" paradigm is ill suited to this)
; TODO? Support bitmaps in addition to text, both data and headers. (easy)
; TODO? Support multiple row/column headers or locked rows/columns. (easy)
;

(define static-default-column-vars
  '([alignment (left center)]
    [style null]
    [min-width 0]
    [min-height 0]))

(define logging #f)

;;
;; A class to display data in a grid, optionally scrollable,
;; with optional column and row headers.
;;
(define data-table%
  (class vertical-panel%
    ;;
    ;; Initialization variables
    ;;
    ;; data-visitor is a procedure that can visit each datum.
    ;;
    ;; When called, it should call its argument back once for
    ;; each datum, in any order. There need not be a datum for
    ;; every cell in the table.
    ;; 
    ;; (-> (-> row-index col-index datum))
    ;; datum: (or/c string? (is-a?/c bitmap%))
    ;; row-index: natural-number/c
    ;; col-index: natural-number/c
    (init parent
          data-visitor
          ;
          ; When false, columns use defaults.
          ; Otherwise, a list of lists of initialization variables, one for each column.
          ; Most apply to each cell of a column.
          ; (listof col-init-vars ...)
          ;  col-init-vars: (listof col-init-var ...)
          ;  col-init-var: (name value)
          ;
          ; Valid names and values are:
          ;  alignment (or/c (or/c left center right) (or/c top center bottom)) = '(left center)
          ;  style     (listof (or/c deleted) ...) = null
          ;  min-width
          ;  min-height
          ;
          ; TODO: The only init var honored right now is alignment.
          [(init-column-vars column-vars) #f]

          ; Default column vars. See above.
          [(init-default-column-vars default-column-vars) static-default-column-vars]
          ;
          ; When false, the number of columns is determined by the data, and no headers are shown.
          ; Otherwise, a list of labels to use as a column headers, one for each column.
          [(init-column-headers column-headers) #f]
          ; Similar to column-headers.
          [(init-row-headers row-headers) #f]

          ; Border around this entire GUI element.
          [border 0]
          
          ; Spacing around each data element.
          [(init-spacing spacing) 0]

          ; area<%>
          [(init-min-width min-width) #f]
          [(init-min-height min-height) #f]
          [(init-stretchable-width stretchable-width) #t]
          [(init-stretchable-height stretchable-height) #t]
          
          ; subarea<%>
          [(init-horiz-margin horiz-margin) 0]
          [(init-vert-margin vert-margin) 0]
          
          ; window<%>
          [(init-enabled enabled) #t]
          
          ; data-table%
          [style null]            ; border, deleted, hscroll, vscroll
          )

    ;
    ; A function provided by our client that we can call, giving it
    ; a function that takes a row index, a column index, and a datum to
    ; display in the data area. The client's function should call us
    ; back once for each datum, in any order.
    ;
    (define visit-data data-visitor)
    ;
    ; Spacing to apply between data items in the data area, and by implication
    ; between row and column headers.
    ;
    (define spacing init-spacing)
    ;
    ; Variables to affect the display of column data, similar
    ; to GUI initialization variables.
    ;
    (define default-column-vars init-default-column-vars)
    ;
    ; Per-column variables.
    ;
    (define column-vars init-column-vars)
    ;
    ; The header lists, strings or bitmaps.
    ; These can be #f if no headers are wanted.
    ;
    (define column-headers #f) ; Will be set later.
    (define row-headers #f)
    ;
    ; Flags to indicate if scrolling is enabled.
    ;
    (define data-vscroll (member 'vscroll style))
    (define data-hscroll (member 'hscroll style))
    ;
    ; The list of graphical objects that represent the headers, if any.
    ; They are not all always visible. The list is rebuilt when headers
    ; are changed.
    ;
    (define column-labels null)
    (define row-labels null)
    ;
    ; Effective data cell height and width, taking into account the needs
    ; of both data and headers, but not cell spacing.
    ;
    (define effective-cell-height 0)
    (define effective-cell-width 0)
    ;
    ; Number of rows and columns.
    ; Affected by actual data seen.
    ;
    (define effective-row-count 0)
    (define effective-column-count 0)
          
    (super-new [parent parent]
               [style (set-intersect '(border deleted) style)]
               [alignment '(left top)]
               [horiz-margin init-horiz-margin]
               [vert-margin init-vert-margin]
               [border border]
               [enabled init-enabled]
               [stretchable-width init-stretchable-width]
               [stretchable-height init-stretchable-height]
               [min-width init-min-width]
               [min-height init-min-height])

    ;;;
    ;;; Contained GUI elements
    ;;;
    
    (define column-header-area
      (new horizontal-pane%
           [parent this]
           [stretchable-height #f]))
    (define row-header-spacer
      (new pane%
           [parent column-header-area]
           [stretchable-height #f]
           [stretchable-width #f]))
    (define column-header-container
      (new horizontal-panel%
           [parent column-header-area]
           [style (if data-hscroll '(hide-hscroll) '())]
           [spacing spacing]
           [alignment '(left top)]))
    (define column-header-scroll-spacer
      (new pane%
           [parent column-header-area]
           [min-width (if (member 'vscroll style) 15 0)]
           [stretchable-width #f]))
    (define lower-area (new horizontal-pane% [parent this]))
    (define row-header-area
      (new vertical-pane%
           [parent lower-area]
           [stretchable-width #f]))
    (define row-header-container
      (new vertical-panel%
           [parent row-header-area]
           [style (if data-vscroll '(hide-vscroll) '())]
           [spacing spacing]
           [alignment '(left top)]))
    (define row-header-scroll-spacer
      (new pane%
           [parent row-header-area]
           [min-height (if (member 'hscroll style) 15 0)]
           [stretchable-height #f]))
    (define my-data-canvas%
      (class canvas%
        (define/override (on-scroll event)
          (paint-column-header-area)
          (paint-row-header-area)
          (send this refresh)
          (super on-scroll event))
        (super-new)))      
    (define data-area
      (new my-data-canvas%
           [parent lower-area]
           [paint-callback (λ (canvas dc) (paint-data-container))]
           [style (list* 'no-focus (set-intersect '(vscroll hscroll) style))]))

    ;;;
    ;;; Internal helper functions.
    ;;;
    
    ;;
    ;; Retrieve an initialization variable for the referenced column.
    ;;
    (define (get-column-var col-index sym)
      ; The value may come from a column-specifc set of vars,
      ; the user-configured default column vars, or the static
      ; default set of vars. Only the last is guaranteed to
      ; have an entry for a supported var.
      (define vars (and column-vars
                        (< col-index (length column-vars))
                        (list-ref column-vars col-index)))
      (car (or (and vars (dict-ref vars sym #f))
               (dict-ref default-column-vars sym #f)
               (dict-ref static-default-column-vars sym))))

    ;;
    ;; Calculates a header container's dimensions when all labels
    ;; are visible.
    ;;
    (define (min-header-dimensions container labels)
      ; The only sane way to do this is to temporarily make all
      ; labels visible. The dimensions are affected by the children's
      ; dimensions, margins, border, spacing.
      (send container begin-container-sequence)
      (define children (send container get-children))
      (send container change-children (λ (ch) labels))
      (send container reflow-container)
      (define-values (gw gh) (send container get-graphical-min-size))
      (values (max gw (send container min-width))
              (max gh (send container min-height)))
      ; Put things back the way they were:
      (send container change-children (λ (ch) children))
      (send container end-container-sequence)
      (values gw gh))

    ;;
    ;; Given a set of header labels, return the largest graphical
    ;; minimum width.
    ;;
    (define (labels-width labels)
      (for/fold ([width 0])
                ([label labels])
        (define-values (w h) (send label get-graphical-min-size))
        (max width w)))

    ;;
    ;; Given a set of header labels, return the largest graphical
    ;; minimum height.
    ;;
    (define (labels-height labels)
      (for/fold ([height 0])
                ([label labels])
        (define-values (w h) (send label get-graphical-min-size))
        (max height h)))
        
    ;;
    ;; Based on the data (and not headers), figure out
    ;; the largest width and height of a cell, and the
    ;; highest row and column indexes associated with any
    ;; datum.
    ;;
    (define (calc-data-extents)
      (define dc (send data-area get-dc))
      (define (element-extents element)
        (cond
          [(string? element)
           (define-values (w h d a) (send dc get-text-extent element))
           (values (inexact->exact w) (inexact->exact h))]))
      (define max-width 0)
      (define max-height 0)
      (define max-row (- (length row-labels) 1))
      (define max-col (- (length column-labels) 1))
      (define (visitor row col datum)
        (define-values (w h) (element-extents datum))
        (set! max-row (max max-row row))
        (set! max-col (max max-col col))
        (set! max-width (max max-width w))
        (set! max-height (max max-height h)))
      (visit-data visitor)
      (values max-height
              max-width
              (+ max-row 1)
              (+ max-col 1)))

    (define (paint-element row col element row-height col-width dc alignment)
      (cond
        [(string? element)
         (define-values (w h d a) (send dc get-text-extent element))
         (define x (+ (* col-width col)
                      (* spacing col)
                      (match (car alignment)
                        ['right  (- col-width w)]
                        ['center (quotient (- col-width w) 2)]
                        ['left   0])))
         (define y (+ (
                       * row-height row)
                      (* spacing row)
                      (match (cadr alignment)
                        ['top 0]
                        ['center (quotient (- row-height h) 2)]
                        ['bottom (- row-height h)])))
         (send dc draw-text element x y)]))
    
    (define (paint-data-container)
      (define first-visible-col (send data-area get-scroll-pos 'horizontal))
      (define first-visible-row (send data-area get-scroll-pos 'vertical))
      (define dc (send data-area get-dc))
      (define (paint-datum row col datum)
        (when (and (>= col first-visible-col) (>= row first-visible-row))
          (paint-element (- row first-visible-row) (- col first-visible-col)
                         datum
                         effective-cell-height effective-cell-width
                         dc
                         (get-column-var col 'alignment))))
      (visit-data paint-datum))
    
    (define (paint-column-header-area)
      (when column-headers
        (send column-header-area begin-container-sequence)
        (define first-visible-col (send data-area get-scroll-pos 'horizontal))
        ;(printf "col label count ~a, scroll pos ~a~n" (length column-labels) first-visible-col)
        (define new-children (list-tail column-labels first-visible-col))
        (send column-header-container change-children (λ (ignored) new-children))
        (send column-header-area end-container-sequence)))

    (define (paint-row-header-area)
      (when row-headers
        (send row-header-area begin-container-sequence)
        (define first-visible-row (send data-area get-scroll-pos 'vertical))
        ;(printf "row label count ~a, scroll pos ~a~n" (length row-labels) first-visible-row)
        (define new-children (list-tail row-labels first-visible-row))
        (send row-header-container change-children (λ (ignored) new-children))
        (send row-header-area end-container-sequence)))

    (define (update-dimensions)
      (send this begin-container-sequence)
      ;
      ; Get the overall data cell height and width, without spacing.
      ;
      (define-values (max-height max-width nrows ncols) (calc-data-extents))
      (set! effective-row-count nrows)
      (set! effective-column-count ncols)
      (when logging
        (printf "Initial cell h/w: (~a ~a)~n" max-height max-width))
      ;
      ; Calculate and stash the effective data cell sizes. This is used
      ; in numerous places.
      ;
      (set! effective-cell-height (max max-height (labels-height row-labels)))
      (set! effective-cell-width  (max max-width (labels-width column-labels)))
      (when logging
        (printf "Effective cell h/w: (~a ~a)~n" effective-cell-height effective-cell-width))
      ;
      ; We need width and height calculations for the row and column headers, respectively,
      ; since those don't have to match the data cell sizes.
      ;
      (define-values (ch-max-width ch-max-height) (min-header-dimensions column-header-container column-labels))
      (define-values (rh-max-width rh-max-height) (min-header-dimensions row-header-container row-labels))
      (when logging
        (printf "Column header max height: ~a~n" ch-max-height)
        (printf "Row    header max width : ~a~n" rh-max-width))
      ;
      ; The row header spacer, a blank space in the upper left corner, needs to match the
      ; width of the row headers. (Which can be zero!)
      ; 
      (send row-header-spacer min-width rh-max-width)
      ;
      ; Adjust the header areas themselves to accomodate the widest or tallest label.
      ; We don't want this dimension to shift when scrolling hides some labels.
      ;
      (send row-header-area min-width rh-max-width)
      (send column-header-area min-height ch-max-height)
      ;
      ; Adjust each label to have the same width or height as all cells do.
      ;
      (for ([label column-labels]) (send label min-width  effective-cell-width))
      (for ([label row-labels])    (send label min-height effective-cell-height))
      ;
      ; All done. Reset scrollbars, queue a refresh and allow updates to occur.
      ;
      (update-scrollbars #t)
      (send this refresh)
      (send this end-container-sequence))

    ;;
    ;; Update the scrollbar settings.
    ;; Must be done when the viewable are size changes, or when the data changes.
    ;; Optionally reset the scrollbar positions.
    ;;
    (define (update-scrollbars [reset-pos #f])
      (define first-visible-col (if reset-pos 0 (send data-area get-scroll-pos 'horizontal)))
      (define first-visible-row (if reset-pos 0 (send data-area get-scroll-pos 'vertical)))
      
      (define total-data-width (+ (* effective-cell-width effective-column-count)
                                  (* effective-column-count spacing)))
      (define total-data-height (+ (* effective-cell-height effective-row-count)
                                   (* effective-row-count spacing)))
      (unless data-hscroll (send data-area min-client-width total-data-width))
      (unless data-vscroll (send data-area min-client-height total-data-height))
      (when (or data-hscroll data-vscroll)
        (define-values (client-width client-height) (send data-area get-client-size))
        (define scroll-total-cols (and data-hscroll
                                       (positive? effective-column-count)
                                       (max 2 (- effective-column-count 1))))
        (define scroll-total-rows (and data-vscroll
                                       (positive? effective-row-count)
                                       (max 2 (- effective-row-count 1))))
        (define scroll-page-cols (max 1 (if (positive? effective-cell-width)
                                            (quotient client-width effective-cell-width)
                                            0)))
        (define scroll-page-rows (max 1 (if (positive? effective-cell-height)
                                            (quotient client-height effective-cell-height)
                                            0)))
        ;(printf "Scroll total rows/cols: (~a ~a)~n" scroll-total-rows scroll-total-cols)
        ;(printf "Scroll page  rows/cols: (~a ~a)~n" scroll-page-rows scroll-page-cols)
        (send data-area init-manual-scrollbars
              scroll-total-cols scroll-total-rows
              scroll-page-cols scroll-page-rows
              first-visible-col first-visible-row))
      ;(printf "data w/h: (~a ~a)~n" total-data-width total-data-height)
      (send this refresh))
      

    ;;;
    ;;; Methods
    ;;;
    
    (define/override (on-size width height)
      (update-scrollbars)
      (super on-size width height))

    (define/override (refresh)
      (paint-column-header-area)
      (paint-row-header-area)
      (super refresh))
    
    (define/public (set-column-headers headers)
      (set! column-headers headers)
      (set! column-labels
            (if headers
                (for/list ([header headers]
                           [col-index (in-naturals)])
                  (define alignment (get-column-var col-index 'alignment))
                  (define ph (new panel%
                                  [parent column-header-container]
                                  [alignment alignment]
                                  [stretchable-width #f]))
                  (new message% [parent ph] [label header])
                  ph)
                null))
      (update-dimensions))
    
    (define/public (set-row-headers headers)
      (set! row-headers headers)
      (set! row-labels
            (if headers
                (for/list ([header headers]
                           [col-index (in-naturals)])
                  (define ph (new panel%
                                  [parent row-header-container]
                                  [alignment '(left center)]
                                  [stretchable-height #f]))
                  (new message% [parent ph] [label header])
                  ph)
                null))
      (update-dimensions))

    ;;;
    ;;; Final initialization steps.
    ;;;
    
    (set-column-headers init-column-headers)
    (set-row-headers init-row-headers)

    ))

; TESTING:
;(define f (new frame% [label "Yowzer!"]))
;
;(define data '((0 0 "Flub")
;               (2 0 "Pollux")
;               (1 1 "Warble")
;               (0 2 "Kazoo!")
;               (1 2 "42")))
;(define (visit visitor)
;  (for ([d data])
;    (printf "~a~n" d)
;    (apply visitor d)))
;
;(define test (new data-table% [parent f] [data-visitor visit] [spacing 2] [column-headers '("A" "2" "III")]))
;
;(send f show #t)
