#lang racket/gui

(require racket/generator)

(define-syntax forward
  (syntax-rules ()
    [(forward target (internal external))
     (define/public ((internal external) . args) (send target external . args)) ]
    [(forward target external)
     (define/public (external . args) (send target external . args)) ]
    [(forward target (internal external) rest ... )
     (begin
       (forward target (internal external))
       (forward target rest ...))]
    [(forward target external rest ...)
     (begin
       (forward target external)
       (forward target rest ...))]))
 
(define data-table%
  (class* object% (subwindow<%>)
    ;;
    ;; Initialization variables
    ;;
    (init parent
          ; area<%>
          [(init-min-width min-width) #f]
          [(init-min-height min-height) #f]
          [(init-stretchable-width stretchable-width) #t]  ;.
          [(init-stretchable-height stretchable-height) #t] ;.
          ; subarea<%>
          [(init-horiz-margin horiz-margin) 0]
          [(init-vert-margin vert-margin) 0]
          ; window<%>
          [(init-enabled enabled) #t]           ;.
    
          ; ... and also, panel-like:
          [border 0]              ;.
          [spacing 0]

          ; alignment? -> per-row

          ; data-table%
          [style null]            ; border, deleted, hscroll, vscroll
          [(init-column-headers column-headers) null]   ; (list/c label-string?)
          [column-styles #f]      ; (list/c (list/c ???))
          [column-alignment #f]
          [column-order #f])       ; like list-box%

    ;;
    ;; Fields
    ;;
    (define main-panel
      (new vertical-panel%
           [parent parent]
           [style (set-intersect '(border deleted) style)]
           [horiz-margin init-horiz-margin]
           [vert-margin init-vert-margin]
           [border border]
           [enabled init-enabled]
           [stretchable-width init-stretchable-width]
           [stretchable-height init-stretchable-height]
           [min-width init-min-width]
           [min-height init-min-height]))
    (define header
      (new horizontal-pane%
           [parent main-panel]
           [spacing spacing]
           [alignment '(left top)]
           [stretchable-height #f]
           ))
    (define data
      (new horizontal-panel%
           [parent main-panel]
           [spacing spacing]
           [alignment '(left top)]
           [style (set-intersect '(vscroll hscroll) style)]))

    ; TODO: Initialization variable contract checking.

    (define-values (column-headers column-data)
      (let ([cs (if column-styles
                    (sequence->generator column-styles)
                    (infinite-generator (yield null)))]
            [ca (if column-alignment
                    (sequence->generator column-alignment)
                    (infinite-generator (yield #f)))])
        (for/lists (r1 r2)
                   ([h init-column-headers])
          (define s (cs))
          (define a (ca))
          (define ph (new panel%
                          [parent header]
                          [style s] ; TODO: Allow only deleted? Maybe border?
                          [alignment (if a a '(left center))]
                          ))
          (new message%
               [parent ph]
               [label h])
          (define pd (new panel%
                          [parent data]
                          [style s] ; TODO: Allow only deleted? Maybe border?
                          [alignment (if a a '(left center))]
                          ))
          (new message%
               [parent pd]
               [label ""])
          (values ph pd))))

    (define/public (set-data-column column-index data-list)
      (define c (list-ref column-data column-index))
      ;(define cm (car (send c get-children)))
      (define new-label (string-join data-list "\n"))
      ; This does not appear to work, possibly because the graphical minimum size is not recalculated:
      ;(send cm set-label new-label)
      ; And this does not work with right-alignment because the text is left-aligned inside the message%.
      (send c change-children (λ (ignored) (list (new message% [parent c] [label new-label]))))
      )
    
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
    ))
    
(define f (new frame% [label "Yowzer!"]))


