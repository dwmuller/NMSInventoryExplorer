#lang racket/gui

(require racket/generator
         racket/draw)

(provide data-table%)

; TODO: Font selection in canvases.
; BUG : Row number during vertical scrolling occasionally too long for list.
; TODO: Header scrolling now works, but is totally different for row vs. column headers. After the above refinements, choose one.
;       Approach using message% objects may not work reliably unless I stop creating new objects.
; TODO: Refresh causes vscroll (& hscroll?) position to reset to zero. May need before/after canvas sizes to fix.
; TODO: Check initialization variable contracts.
; TODO? Support deleted cols.
; TODO? Support deleted rows.
; TODO? Support non-uniform col widths and row heights.
; TODO? Restrict paint to visible portion when scrolled.
; TODO? Derive from subwindow%. Makes use of internal panes, panels impossible, would have to paint headers on separate canvases.
;       But who owns the canvases? Must be frame, dialog, panel, or pane! Grrr.


(define static-default-column-vars
  '([alignment (left center)]
    [style null]
    [min-width 0]
    [min-height 0]))

;;
;; A class to display data in a grid, optionally scrollable,
;; with optional column and row headers.
;;
;; Although this is derived from vertical-panel%, that's an implementation
;; detail which I would rather have hidden. I couldn't figure out a practical
;; way to do that given some details of the gui library. E.g. vertical-panel% itself
;; wont't accept just any window<%> interface implementation as a parent.
;;
;; TODO: Make this implement window<%>. Internally, make a plain panel% with this as parent,
;; put the current vertical-panel% inside that panel%. That would work and would properly
;; encapsulate everything.
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
          [(init-border border) 0]
          
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
    (define border init-border)
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
    
    (define column-vars init-column-vars)

    (define column-headers #f) ; Will be set later.
    (define row-headers #f)
    ;
    ; Flags to indicate if scrolling is enabled.
    ;
    (define data-vscroll (member 'vscroll style))
    (define data-hscroll (member 'hscroll style))
    ;
    ; The list of graphical objects that represent the row headers, if any.
    ; They are not all always visible. The list is rebuilt when headers
    ; are changed.
    ;
    (define row-labels null)
          
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
      (new canvas%
           [parent column-header-area]
           [paint-callback (λ (canvas dc) (paint-column-header-area))]
           [style '()]))

    ;    (define column-header-container
    ;      (new horizontal-pane%
    ;           [parent column-header-area]
    ;           [spacing spacing]
    ;           [alignment '(left top)]))
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
    
    (define my-canvas%
      (class canvas%
        (define/override (on-scroll event)
          (send this refresh)
          (paint-column-header-area)
          (paint-row-header-area)
          (super on-scroll event))
        (super-new)))
      
    (define data-area
      (new my-canvas%
           [parent lower-area]
           [paint-callback (λ (canvas dc) (paint-data-container))]
           [style (list* 'no-focus (set-intersect '(vscroll hscroll) style))]))

    (define (get-vars-for-column col)
      (define vars (or (and column-vars
                            (< col (length column-vars))
                            (list-ref column-vars col))
                       default-column-vars))
      (define (get sym)
        (car (or (dict-ref vars sym #f)
                 (dict-ref default-column-vars sym #f)
                 (dict-ref static-default-column-vars sym))))
      (values (get 'alignment)
              (get 'style)
              (get 'min-width)
              (get 'min-height)))

    (define (get-column-var col-index sym)
      (define vars (or (and column-vars
                            (< col-index (length column-vars))
                            (list-ref column-vars col-index))
                       default-column-vars))
      (car (or (dict-ref vars sym #f)
               (dict-ref default-column-vars sym #f)
               (dict-ref static-default-column-vars sym))))

    (define (element-extents dc element)
      (cond
        [(string? element)
         (define-values (w h d a) (send dc get-text-extent element))
         (values (inexact->exact w) (inexact->exact h))]))

    (define (calc-header-max-extents dc headers)
      (if headers
          (for/fold ([max-width 0]
                     [max-height 0])
                    ([element headers])
            (define-values (w h) (element-extents dc element))
            (values (max max-width w) (max max-height h)))
          (values 0 0)))

    (define (calc-extents)
      (define-values (ch-max-width ch-max-height)
        (calc-header-max-extents (send column-header-container get-dc) column-headers))
      (define max-width ch-max-width)
      (define row-headers (send row-header-container get-children))
      (define max-height 0)
      (define max-row (- (length row-headers) 1))
      (define max-col (if column-headers (- (length column-headers) 1) -1))
      (define dc (send data-area get-dc))
      (define (visitor row col datum)
        (define-values (w h) (element-extents dc datum))
        (set! max-row (max max-row row))
        (set! max-col (max max-col col))
        (set! max-width (max max-width w))
        (set! max-height (max max-height h)))
      (visit-data visitor)
      ; TODO: Remove this!
      (for ([h row-headers])
        (send h min-height max-height))
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
         (define y (+ (* row-height row)
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
      (define-values (rh cw nr nc) (calc-extents))
      (define (paint-datum row col datum)
        (when (and (>= col first-visible-col) (>= row first-visible-row))
          (paint-element (- row first-visible-row) (- col first-visible-col) datum rh cw dc (get-column-var col 'alignment))))
      (visit-data paint-datum))
    
    (define (paint-column-header-area)
      (define dc (send column-header-container get-dc))
      (send dc clear)
      (when column-headers
        (define first-visible-col (send data-area get-scroll-pos 'horizontal))
        (define ch-max-height (send column-header-area min-height))
        (define-values (max-height max-width nr nc) (calc-extents))
        (define-values (width height) (send column-header-container get-client-size))
        (for ([header (list-tail column-headers first-visible-col)]
              [col-index (in-naturals first-visible-col)])
          (paint-element 0 (- col-index first-visible-col) header ch-max-height max-width dc (get-column-var col-index 'alignment)))))

    (define (paint-row-header-area)
      (when row-headers
        (send row-header-area begin-container-sequence)
        (define first-visible-row (send data-area get-scroll-pos 'vertical))
        (define new-children (list-tail row-labels first-visible-row))
        (send row-header-container change-children (λ (ignored) new-children))
        (send row-header-area end-container-sequence)))

    (define (update-dimensions)
      (define-values (max-height max-width nrows ncols) (calc-extents))
      (define-values (ch-max-width ch-max-height)
        (calc-header-max-extents (send column-header-container get-dc) column-headers))
      ; Make all row header labels visible so we can get the width of that column. Kinda grody.
      ; We ask for a refresh after this, which will repaint them correctly.
      (send row-header-container change-children (λ (ignored) row-labels))
      (send row-header-container reflow-container)
      (define-values (rh-max-width rh-total-height) (send row-header-container get-client-size))
      (send row-header-spacer min-width rh-max-width)
      (send row-header-spacer min-height ch-max-height)
      (send row-header-area min-width rh-max-width)
      (send column-header-area min-height ch-max-height)
      (define total-data-width (+ (* max-width ncols) (* (- ncols 1) spacing)))
      (define total-data-height (+ (* max-height nrows) (* (- nrows 1) spacing)))
      (unless data-hscroll (send data-area min-client-width total-data-width))
      (unless data-vscroll (send data-area min-client-height total-data-height))
      (when (or data-hscroll data-vscroll)
        (define-values (client-width client-height) (send data-area get-client-size))
        (define scroll-total-cols (and data-hscroll (positive? ncols) (max 2 (- ncols 1))))
        (define scroll-total-rows (and data-vscroll (positive? nrows) (max 2 (- nrows 1))))
        (define scroll-page-cols (max 1 (if (positive? max-width) (quotient client-width max-width) 0)))
        (define scroll-page-rows (max 1 (if (positive? max-height) (quotient client-height max-height) 0)))
        (printf "Scroll total rows/cols: (~a ~a)~n" scroll-total-rows scroll-total-cols)
        (printf "Scroll page  rows/cols: (~a ~a)~n" scroll-page-rows scroll-page-cols)
        (send data-area init-manual-scrollbars
              scroll-total-cols scroll-total-rows
              scroll-page-cols scroll-page-rows
              0 0))
      (printf "data w/h: (~a ~a)~n" total-data-width total-data-height)
      (send this refresh))


    (define/override (on-size width height)
      (update-dimensions)
      (super on-size width height))
    
    (define/public (set-column-headers headers)
      (set! column-headers headers)
      (update-dimensions))
    
    (define/public (set-row-headers headers)
      (send row-header-area begin-container-sequence)
      (set! row-headers headers)
      (cond
        [headers  (set! row-labels
                        (for/list ([header row-headers]
                                   [row-index (in-naturals)])
                          (define ph (new panel%
                                          [parent row-header-container]
                                          [alignment '(left center)]
                                          [stretchable-height #f]))
                          (new message% [parent ph] [label header])
                          ph))]
        [else (set! row-labels null)])
      (update-dimensions)
      (send row-header-area end-container-sequence))
    
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
