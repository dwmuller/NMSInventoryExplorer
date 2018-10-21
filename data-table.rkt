#lang racket/gui

(require racket/generator
         racket/draw)

(provide data-table%)

; TODO? Consider more convenient methods of providing column init-vars.
; TODO: Ugh, column headers never disappear.
; TODO: Refresh causes vscroll position to reset to zero. May need before/after canvas sizes to fix.
; TODO: Locked row(s) aka row headers, horizontal scrolling.
;       ... or at least separate width calc for first column.
; TODO: Scrolling has to include headers. Use hide-(h|v)scroll, generate route copies of events.
; TODO: Font selection in data area!
; TODO: Check initialization variable contracts.
; TODO? Support deleted cols.
; TODO? Support deleted rows.
; TODO? Support non-uniform col widths and row heights?
; TODO? Restrict paint to visible portion when scrolled.


(define default-column-vars
  (list (list 'header (λ (p c) (new message% [parent p] [label (format "Column~a" c)])))
        '[alignment (left center)]
        '[style null]
        '[min-width 0]
        '[min-height 0]))

(define data-table%
  (class vertical-panel%
    ;;
    ;; Initialization variables
    ;;
    ;; visit-data is a procedure that can visit each datum.
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
          ; A list of lists of initialization variables, one for each column.
          ; The last one applies to all remaining columns.
          ; Most apply to each cell of a column.
          ; (listof col-init-vars ...)
          ;  col-init-vars: (listof col-init-var ...)
          ;  col-init-var: (name value)
          ;
          ; Valid names and values are:
          ;  header    (or/c (-> parent col-index control%) label-string?) = proc that produces "ColN"
          ;  alignment (or/c (or/c left center right) (or/c top center bottom)) = '(left center)
          ;  style     (listof (or/c deleted) ...) = null
          ;  min-width
          ;  min-height
          [column-init-vars (list default-column-vars)]
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
    (define column-vars column-init-vars)
          
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
    (define header-pane
      (new horizontal-pane%
           [parent this]
           [stretchable-height #f]
           [stretchable-width #f]
           ))
    (define header-container
      (new horizontal-pane%
           [parent header-pane]
           [spacing spacing]
           [alignment '(left top)]
           ))
    ; Spacer to match vert scrollbar in data area:
    (new pane% [parent header-pane] [min-width 14] [stretchable-width #f])

    (define data-area
      (new canvas%
           [parent this]
           [paint-callback (λ (canvas dc) (paint))]
           [style (list* 'no-focus (set-intersect '(vscroll hscroll) style))]))    


    (define (get-vars-for-column col)
      (define vars (if (< col (length column-vars))
                       (list-ref column-vars col)
                       (last column-vars)))
      (define (get sym)
        (car (or (dict-ref vars sym #f) (dict-ref sym default-column-vars))))
      (values (get 'header)
              (get 'alignment)
              (get 'style)
              (get 'min-width)
              (get 'min-height)))

    
    (define (add-missing-columns col n)
      (for/last ([i (in-range col (+ col n))])
        (define-values (header alignment style min-w min-h)
          (get-vars-for-column i))
        (define ph (new panel%
                        [parent header-container]
                        [style null] ; TODO: Allow only deleted? 
                        [min-width min-w]
                        [alignment alignment]))
        (cond
          [(string? header)    (new message% [parent ph] [label header])]
          [(procedure? header) (header ph i)])
        ph))
            
    (define (calc-extents dc)
      (define (datum-extents col datum)
        (cond
          [(string? datum)
           (define-values (w h d a) (send dc get-text-extent datum))
           (values (inexact->exact w) (inexact->exact h))]))

      (define col-width 0)
      (define row-height 0)
      (define max-row -1)
      (define max-col (- (length (send header-container get-children)) 1))
      (define (visitor row col datum)
        (when (> col max-col)
          (add-missing-columns (+ 1 max-col) (- col max-col)))
        (define-values (w h) (datum-extents col datum))
        (set! max-row (max max-row row))
        (set! max-col (max max-col col))
        (set! col-width (max col-width w))
        (set! row-height (max row-height h)))
      (visit-data visitor)
      (send header-container reflow-container)
      (define headers (send header-container get-children))
      (set! col-width (apply max col-width (map (λ (a) (send a get-width)) headers)))
      (for ([h headers])
        (send h min-width col-width))
      (values row-height
              col-width
              (+ max-row 1)
              (+ max-col 1)))

    (define (paint)
      (define dc (send data-area get-dc))
      (define-values (rh cw nr nc) (calc-extents dc))
      (define (paint-datum row col datum)
        (define-values (header alignment style min-w min-h)
          (get-vars-for-column col))
        (cond
          [(string? datum)
           (define-values (w h d a) (send dc get-text-extent datum))
           (define x (+ (* cw col)
                        (* spacing col)
                        (match (car alignment)
                          ['right  (- cw w)]
                          ['center (/ (- cw w) 2)]
                          ['left   0])))
           (define y (+ (* rh row)
                        (* spacing row)
                        (match (cadr alignment)
                          ['top 0]
                          ['center (/ (- rh h) 2)]
                          ['bottom (- rh h)])))
           (send dc draw-text datum x y)]))
      
      (visit-data paint-datum))

    (define/override (refresh)
      (define-values (rh cw nr nc) (calc-extents (send data-area get-dc)))
      (define data-width (max (send header-container min-width)
                              (+ (* cw nc) (* (- nc 1) spacing))))
      (define data-height (+ (* rh nr) (* (- nr 1) spacing)))
      (define-values (hpos vpos) (send data-area get-view-start))
      (printf "view-start: (~a ~a)~n" hpos vpos)
      (send data-area
            init-auto-scrollbars
            (and (positive? data-width) data-width)
            (and (positive? data-height) data-height)
            0 0)
      (super refresh))))

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
