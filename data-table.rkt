#lang racket/gui

(require racket/generator
         racket/draw)

(provide data-table%)

;;
;; Given an expression that resolves to an object, followed
;; by a list of (uneveluated) symbols representing method names,
;; define methods by those same names which are implemented by
;; forwarding calls to the given object.
;;
(define-syntax forward
  (syntax-rules ()
    [(forward target (internal external))
     (define/public ((internal external) . args) (send target external . args)) ]
    [(forward target external)
     (define/public (external . args) (send target external . args)) ]
    [(forward target current rest ... )
     (begin
       (forward target current)
       (forward target rest ...))]))    

(define default-column-vars
  (list (list 'header (λ (p c) (new message% [parent p] [label (format "Column~a" c)])))
        '[alignment (left center)]
        '[style null]
        '[min-width 0]
        '[min-height 0]))

(define data-table%
  (class* object% (subwindow<%>)
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
    
    ; TODO: Initialization variable contract checking.
    ; TODO: hscroll has to include columns - apply to main panel?

    (define visit-data data-visitor)
    (define border init-border)
    (define spacing init-spacing)
    (define column-vars column-init-vars)
          
        
    ;;
    ;; Fields
    ;;
    (define main-panel
      (new vertical-panel%
           [parent this]
           [style (set-intersect '(border deleted) style)]
           [alignment '(left top)]
           [horiz-margin init-horiz-margin]
           [vert-margin init-vert-margin]
           [border border]
           [enabled init-enabled]
           [stretchable-width init-stretchable-width]
           [stretchable-height init-stretchable-height]
           [min-width init-min-width]
           [min-height init-min-height]))
    (define header-container
      (new horizontal-pane%
           [parent main-panel]
           [spacing spacing]
           [alignment '(left top)]
           [stretchable-height #f]
           [stretchable-width #f]
           ))
    (define data-area
      (new canvas%
           [parent main-panel]
           [paint-callback (λ (canvas dc) (paint))]
           [style (list* 'no-focus (set-intersect '(vscroll hscroll) style))]))

    (define (get-vars-for-column col)
      (define vars (or (and (< col (length column-vars)) (list-ref column-vars col))
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

      ; TODO: Support deleted cols?
      ; TODO: Support deleted rows?
      ; TODO: Support variable col widths and row heights?
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


    ;    (define-values (row-height
    ;                    col-width
    ;                    nrows
    ;                    ncols)
    ;      (calc-extents (send data get-dc) visit-data))
    ;
    ;    (send data min-height (+ (* nrows row-height) (* spacing (- nrows 1))))
    ;    (send data min-width  (+ (* ncols col-width)  (* spacing (- ncols 1))))
    ;    
    ;    (define column-headers
    ;      (let ([cs (if column-styles
    ;                    (sequence->generator column-styles)
    ;                    (infinite-generator (yield null)))]
    ;            [ca (if column-alignment
    ;                    (sequence->generator column-alignment)
    ;                    (infinite-generator (yield #f)))])
    ;        (for/list ([h init-column-headers])
    ;          (define s (cs))
    ;          (define a (ca))
    ;          ph)))
    ;
    ;    (for ([h column-headers])
    ;      (send h min-width col-width))

    ;;
    ;; Interface implementations
    ;;
    (forward main-panel
             ; area<%>
             get-graphical-min-size
             get-parent
             get-top-level-window
             min-width
             min-height
             stretchable-height
             stretchable-width
             ; subarea<%>
             horiz-margin
             vert-margin
             ; window<%>
             accept-drop-files
             client->screen
             enable
             focus
             get-client-handle
             get-client-size
             get-cursor
             get-handle
             get-height
             get-label
             get-plain-label
             get-size
             get-width
             get-x
             get-y
             has-focus?
             is-enabled?
             is-shown?
             on-drop-file
             on-focus
             on-move
             on-size
             on-subwindow-char
             on-subwindow-event
             on-subwindow-focus
             on-superwindow-enable
             on-superwindow-show
             popup-menu
             refresh
             screen->client
             set-cursor
             set-label
             show
             warp-pointer
             ; subwindow<%>
             reparent)
    
    (super-new)
    (send parent add-child this)

    ;    (define/public (get-dimensions) (values nrows ncols))
    
    ))

(define f (new frame% [label "Yowzer!"]))

(define data '((0 0 "Flub")
               (2 0 "Pollux")
               (1 1 "Warble")
               (0 2 "Kazoo!")
               (1 2 "42")))
(define (visit visitor)
  (for ([d data])
    (printf "~a~n" d)
    (apply visitor d)))

(define test (new data-table% [parent f] [data-visitor visit] [spacing 2]))

(send f show #t)
