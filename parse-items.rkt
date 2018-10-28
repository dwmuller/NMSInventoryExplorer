#lang racket

(require xml
         xml/path
         racket/generator
         racket/date
         "items.rkt")

;;; Use "NMS Modding Station" and MBINCompiler to extract the needed EXML files.
;;; These files are insanely verbose. The typical structure is a Data element,
;;; containing nested trees of Property elements with "name" attributes
;;; and occasional "value" attributes. About as inefficient a use of XML as you can
;;; imagine.


(define root (simplify-path "D:/NMS-tools/out-1.65/"))
(define output-root (simplify-path "C:/Users/danm/Documents/Racket/NMSRecipes/"))

(define (read-doc path)
  (define (read port)
    (parameterize ([collapse-whitespace #t])
      (document-element (read-xml port))))
  (call-with-input-file path read))

(define (get-attribute element name)
  (ormap (λ (a) (and (eq? name (attribute-name a))
                     (attribute-value a)))
         (element-attributes element)))

(define (visit-child-elements element path visitor)
  (define name (car path))
  (for ([child (element-content element)])
    (when (and (element? child) (eq? name (element-name child)))
      (if (null? (cdr path))
          (visitor child)
          (visit-child-elements child (cdr path) visitor)))))

(define (child-element-sequence element path [pred #f])
  (in-generator #:arity 1
   (define (visitor e)
     (when (or (not pred) (pred e))
       (yield e)))
   (visit-child-elements element path visitor)))

(define (child-element element path [pred #f])
  (define-values (any? next)
    (sequence-generate (child-element-sequence element path pred)))
  (cond
    [(not (any?)) #f]
    [else
     (define result (next))
     (when (any?)
       (raise-result-error 'child-element "Expected one matching child, found multiple." path))
     result]))

(define (names-value element names)
  (cond
    [(null? names) (get-attribute element 'value)]
    [else
     (define child (child-element element
                                  '(Property)
                                  (λ (e) (string=? (car names) (get-attribute e 'name)))))
     (if child (names-value child (cdr names)) #f)]))

(define (scan-localization-table path language name-lower-id-map product-map)
  (define doc (read-doc path))
  (unless (and (eq? 'Data (element-name doc))
               (equal? "TkLocalisationTable" (get-attribute doc 'template)))
    (raise-result-error 'load-localization-table
                        "<Data template=\"TkLocalisationTable\" ..."
                        (format "<~a template=\"~a\" ..." (element-name doc) (get-attribute doc 'template))))
  (for ([elem (child-element-sequence doc '(Property Property))])
    (when (string=? "TkLocalisationEntry.xml" (get-attribute elem 'value))
      (define name-id (names-value elem '("Id")))
      (define data (hash-ref name-lower-id-map name-id #f))
      (when data
        (define label (names-value elem (list language "Value")))
        (define item-name (label->item-name label))
        (hash-remove! name-lower-id-map name-id)
        (hash-set! product-map
                   item-name
                   (item$ item-name (first data) (second data) (fifth data) label (third data) (fourth data)))))))

(define (read-products id-map path table-type items-type . extra-flags)
  (define doc (read-doc path))
  (unless (and (eq? 'Data (element-name doc))
               (equal? table-type (get-attribute doc 'template)))
    (raise-result-error 'read-products
                        (format "<Data template=\"~a\" ..." table-type)
                        (format "<~a template=\"~a\" ..." (element-name doc) (get-attribute doc 'template))))
  ; TODO: Collect build recipes?
  (for ([elem (child-element-sequence doc '(Property Property))])
    (when (string=? items-type (get-attribute elem 'value))
      (define name-lower-id      (names-value elem '("NameLower")))
      (define id                 (string->symbol (or (names-value elem '("Id")) (names-value elem '("ID")))))
      (define base-value         (string->number (names-value elem '("BaseValue"))))
      (define substance-category (string->symbol
                                  (or (names-value elem '("SubstanceCategory" "SubstanceCategory")) "Unknown")))
      (define rarity             (string->symbol (names-value elem '("Rarity" "Rarity"))))
      (define flags (for/fold ([result null])
                              ([flag-spec extra-flags])
                      (define value (names-value elem flag-spec))
                      (if value
                          (cons (string->symbol (format "~a:~a" (last flag-spec) value)) result)
                          result)))
      (hash-set! id-map name-lower-id (list id base-value substance-category rarity flags)))))



; Read items first, put placeholder for name text. Then translate placeholders.
(define id-map (make-hash))
(read-products id-map (build-path root "METADATA/REALITY/TABLES/NMS_REALITY_GCPRODUCTTABLE.EXML") "GcProductTable" "GcProductData.xml" '("Type" "ProductCategory"))
(read-products id-map (build-path root "METADATA/REALITY/TABLES/NMS_REALITY_GCSUBSTANCETABLE.EXML") "GcSubstanceTable" "GcRealitySubstanceData.xml")
(define product-map (make-hasheq))
(scan-localization-table (build-path root "LANGUAGE/NMS_LOC1_USENGLISH.EXML") "USEnglish" id-map product-map)
(scan-localization-table (build-path root "LANGUAGE/NMS_LOC4_USENGLISH.EXML") "USEnglish" id-map product-map)
(scan-localization-table (build-path root "LANGUAGE/NMS_UPDATE3_USENGLISH.EXML") "USEnglish" id-map product-map)

; Attempted additional scans to find missing items; no luck.
;(scan-localization-table (build-path root "LANGUAGE/NMS_LOC4_ENGLISH.EXML") "English" id-map product-map)

(define missing-item-translations (set-subtract (map car (hash-values id-map)) (map (λ (v) (item$-id v)) (hash-values product-map))))
(printf "Found and translated ~a items.~n" (length (hash-keys product-map)))
(unless (null? missing-item-translations)
  (printf "Missing translations for ~a items.~n" (length missing-item-translations)))
(printf "Substance categories: ~a~n" (list->set (map (λ (v) (item$-substance-category v)) (hash-values product-map))))
(printf "Rarities: ~a~n" (list->set (map (λ (v) (item$-rarity v)) (hash-values product-map))))
(printf "Flags: ~a~n" (list->set (append-map (λ (v) (item$-flags v)) (hash-values product-map))))

(define (write-generated-items)
  (call-with-output-file (build-path output-root "generated-items.rkt") #:mode 'text #:exists 'replace
    (λ (port)
      (displayln "#lang racket" port)
      (define timestamp (parameterize ([date-display-format 'iso-8601])
                          (let ([d (seconds->date (current-seconds))])
                            (format "~a ~a" (date->string d #t) (date*-time-zone-name d)))))
      (define user (getenv "USERNAME"))
      (displayln (format "; Generated via parse-items.rkt by ~a at ~a" user timestamp) port)
      (pretty-write '(require "items.rkt") port)
      (pretty-write (list 'define 'generated-items product-map) port))))
