#lang racket/gui

(require racket/generator
         racket/draw)

(provide data-table%)

; TODO: Scrolling has to include headers. Use hide-(h|v)scroll, generate route copies of events.
;       Looks like the header containers need to be converted to canvases. Can't manually scroll panels AFAICT.
; TODO: Refresh causes vscroll position to reset to zero. May need before/after canvas sizes to fix.
; TODO: Font selection in data area.
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
          ; When false, columns use defaults and the number of columns is determined
          ; by the data.
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
          [(init-column-vars column-vars) #f]
          ;
          ; When false, the number of columns is determined by the data, and no headers are shown.
          ; Otherwise, a list of labels to use as a column headers, one for each column.
          [(init-column-headers column-headers) #f]
          ; Similar to column-headers.
          [(init-row-headers row-headers) #f]
          [(init-default-column-vars default-column-vars) static-default-column-vars]
          [(init-border border) 0]
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
    
    (define visit-data data-visitor)
    (define border init-border)
    (define spacing init-spacing)
    (define default-column-vars init-default-column-vars)
    (define column-headers #f) ; Will be set later.
    (define row-headers #f)
    (define column-vars init-column-vars)
    (define data-vscroll (member 'vscroll style))
    (define data-hscroll (member 'hscroll style))
          
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
           [stretchable-height #f]
           [stretchable-width #f]
           ))
    (define row-header-spacer
      (new pane%
           [parent column-header-area]
           [stretchable-height #f]
           [stretchable-width #f]))
    (define column-header-container
      (new horizontal-pane%
           [parent column-header-area]
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
    
    (define data-area
      (new canvas%
           [parent lower-area]
           [paint-callback (λ (canvas dc) (paint))]
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
      
    (define (calc-extents dc)
      (define (datum-extents col datum)
        (cond
          [(string? datum)
           (define-values (w h d a) (send dc get-text-extent datum))
           (values (inexact->exact w) (inexact->exact h))]))

      (define col-headers (send column-header-container get-children))
      (define col-width (apply max 0 (map (λ (h) (send h get-width)) col-headers)))
      (define row-headers (send row-header-container get-children))
      (define row-height (apply max 0 (map (λ (h) (send h get-height)) row-headers)))
      (define max-row (- (length row-headers) 1))
      (define max-col (- (length col-headers) 1))
      (define (visitor row col datum)
        (define-values (w h) (datum-extents col datum))
        (set! max-row (max max-row row))
        (set! max-col (max max-col col))
        (set! col-width (max col-width w))
        (set! row-height (max row-height h)))
      (visit-data visitor)
      (for ([h col-headers])
        (send h min-width col-width))
      (for ([h row-headers])
        (send h min-height row-height))
      (values row-height
              col-width
              (+ max-row 1)
              (+ max-col 1)))

    (define (paint)
      (define-values (start-x start-y) (send data-area get-view-start))
      (printf "paint at x/y (~a ~a)~n" start-x start-y)
      (when data-vscroll
        (send row-header-container on-move 0 start-y))
      (define dc (send data-area get-dc))
      (define-values (rh cw nr nc) (calc-extents dc))
      (define (paint-datum row col datum)
        (define-values (alignment style min-w min-h)
          (get-vars-for-column col))
        (cond
          [(string? datum)
           (define-values (w h d a) (send dc get-text-extent datum))
           (define x (+ (* cw col)
                        (* spacing col)
                        (match (car alignment)
                          ['right  (- cw w)]
                          ['center (quotient (- cw w) 2)]
                          ['left   0])))
           (define y (+ (* rh row)
                        (* spacing row)
                        (match (cadr alignment)
                          ['top 0]
                          ['center (quotient (- rh h) 2)]
                          ['bottom (- rh h)])))
           (send dc draw-text datum x y)]))      
      (visit-data paint-datum))

    
    (define (paint-column-header-container headers)
      (define new-children
        (for/list ([header headers]
                   [col-index (in-naturals)])
          (define-values (alignment style min-w min-h)
            (get-vars-for-column col-index))
          (define ph (new panel%
                          [parent column-header-container]
                          [min-width min-w]
                          [alignment alignment]))
          (new message% [parent ph] [label header])
          ph))
      (send column-header-container change-children (λ (ignored) new-children)))
    
    (define (paint-row-header-container headers)
      (define new-children
        (for/list ([header headers]
                   [row-index (in-naturals)])
          (define ph (new panel%
                          [parent row-header-container]
                          [alignment '(left center)]))
          (new message% [parent ph] [label header])
          ph))
      (send row-header-container change-children (λ (ignored) new-children)))

    (define/override (refresh)
      (define-values (rh cw nr nc) (calc-extents (send data-area get-dc)))
      (define data-width (max (send column-header-container min-width)
                              (+ (* cw nc) (* (- nc 1) spacing))))
      (define data-height (+ (* rh nr) (* (- nr 1) spacing)))
      (unless data-vscroll (send data-area min-client-height data-height))
      (unless data-hscroll (send data-area min-client-width data-width))
      (define-values (hpos vpos) (send data-area get-view-start))
      (printf "data w/h: (~a ~a)~n" data-width data-height)
      (printf "view-start: (~a ~a)~n" hpos vpos)
      (send data-area
            init-auto-scrollbars
            (and data-hscroll (positive? data-width) data-width)
            (and data-vscroll (positive? data-height) data-height)
            0 0)
      (super refresh))

    (define/public (set-column-headers headers)
      (send column-header-area begin-container-sequence)
      (cond
        [headers (paint-column-header-container headers)
                 (send column-header-container reflow-container)
                 (send row-header-spacer min-height (send column-header-container min-height))]
        [else (send column-header-container change-children (λ (ignored) null))
              (send row-header-spacer min-height 0)])
      (set! column-headers headers)
      (send column-header-area end-container-sequence)
      (refresh))
    
    (define/public (set-row-headers headers)
      (send row-header-area begin-container-sequence)
      (cond
        [headers (paint-row-header-container headers)
                 (send row-header-container reflow-container)
                 (define-values (header-width header-height) (send row-header-container get-client-size))
                 (printf "row-header-container client w/h: (~a ~a)~n" header-width header-height)
                 (send row-header-spacer min-width header-width)]
        [else (send row-header-container change-children (λ (ignored) null))
              (send row-header-spacer min-width 0)])
      (set! row-headers headers)
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
;(define test (new data-table% [parent f] [data-visitor visit] [spacing 2]))
;
;(send f show #t)
