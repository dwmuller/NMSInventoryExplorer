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
                   (item$ item-name (first data) (second data) (third data) label))))))

(define (read-items id-map path table-type items-type . extra-flags)
  (define doc (read-doc path))
  (unless (and (eq? 'Data (element-name doc))
               (equal? table-type (get-attribute doc 'template)))
    (raise-result-error 'read-products
                        (format "<Data template=\"~a\" ..." table-type)
                        (format "<~a template=\"~a\" ..." (element-name doc) (get-attribute doc 'template))))
  ; TODO: Collect build recipes?
  (define dupes (mutable-set))
  (for ([elem (child-element-sequence doc '(Property Property))]
        [index (in-naturals)])
    (when (string=? items-type (get-attribute elem 'value))
      (define name-lower-id      (names-value elem '("NameLower")))
      (define id                 (string->symbol (or (names-value elem '("Id")) (names-value elem '("ID")))))
      (define base-value         (string->number (names-value elem '("BaseValue"))))
      (define flags (for/fold ([result null])
                              ([flag-spec extra-flags])
                      (define prefix (car flag-spec))
                      (define value (names-value elem (cdr flag-spec)))
                      (if value
                          (cons (string->symbol (format "~a:~a" prefix value)) result)
                          result)))
      (define data (list id base-value flags))
      (cond
        [(set-member? dupes name-lower-id)
         (hash-set! id-map (string-append (symbol->string id) "_NAME_L") data)]
        [(hash-has-key? id-map name-lower-id)
         ; An attempt to deal with duplicates.
         ; CURRENTLY: First one wins.
         ; ALSO TRIED: Base on the fact that there are unused localization entries that would match, tried constructing alternate name reference.
         ;             This made things worse.
         ; ALSO TRIED: Last one wins. Not sure if this was better or worse.
         ;(set-add! dupes name-lower-id)
         (printf "Duplicate ref to name ~a by entry ~a: ~a in ~a~n" name-lower-id index id path)
         ;(define prev (hash-ref id-map name-lower-id))
         ;(hash-set! id-map (string-append (symbol->string (car prev)) "_NAME_L") prev)
         ;(hash-set! id-map (string-append (symbol->string id) "_NAME_L") data)
         ]
        [else (hash-set! id-map name-lower-id data)]))))

; Read items first, put placeholder for name text. Then translate placeholders.
(define id-map (make-hash))
(read-items id-map (build-path root "METADATA/REALITY/TABLES/NMS_REALITY_GCPRODUCTTABLE.EXML") "GcProductTable" "GcProductData.xml"
            '("Product" "Type" "ProductCategory")
            '("Rarity" "Rarity" "Rarity")
            '("Substance" "SubstanceCategory" "SubstanceCategory"))
(read-items id-map (build-path root "METADATA/REALITY/TABLES/NMS_REALITY_GCSUBSTANCETABLE.EXML") "GcSubstanceTable" "GcRealitySubstanceData.xml"
            '("Rarity" "Rarity" "Rarity")
            '("Substance" "SubstanceCategory" "SubstanceCategory"))
(read-items id-map (build-path root "METADATA/REALITY/TABLES/NMS_REALITY_GCTECHNOLOGYTABLE.EXML") "GcTechnologyTable" "GcTechnology.xml"
            '("TechShopRarity" "TechShopRarity" "TechnologyRarity")
            '("TechnologyRarity" "TechnologyRarity" "TechnologyRarity")
            '("Technology" "TechnologyCategory" "TechnologyCategory"))
(define product-map (make-hasheq))

; By default, my game seems to be using U.K. English rather than U.S. English, so let's stick with that.
;(scan-localization-table (build-path root "LANGUAGE/NMS_LOC1_USENGLISH.EXML") "USEnglish" id-map product-map)
;(scan-localization-table (build-path root "LANGUAGE/NMS_LOC4_USENGLISH.EXML") "USEnglish" id-map product-map)
;(scan-localization-table (build-path root "LANGUAGE/NMS_UPDATE3_USENGLISH.EXML") "USEnglish" id-map product-map)
(scan-localization-table (build-path root "LANGUAGE/NMS_LOC1_ENGLISH.EXML") "English" id-map product-map)
(scan-localization-table (build-path root "LANGUAGE/NMS_LOC4_ENGLISH.EXML") "English" id-map product-map)
(scan-localization-table (build-path root "LANGUAGE/NMS_UPDATE3_ENGLISH.EXML") "English" id-map product-map)

; Attempted additional scans to find missing items; no luck.
;(scan-localization-table (build-path root "LANGUAGE/NMS_LOC4_ENGLISH.EXML") "English" id-map product-map)

(define missing-item-translations (set-subtract (map car (hash-values id-map)) (map (λ (v) (item$-id v)) (hash-values product-map))))
(printf "Found and translated ~a items.~n" (length (hash-keys product-map)))
(unless (null? missing-item-translations)
  (printf "Missing translations for ~a items.~n" (length missing-item-translations)))
(pretty-print (list 'Flags (sort (remove-duplicates (append-map (λ (v) (item$-flags v)) (hash-values product-map))) symbol<?)))

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
      (writeln port)
      (pretty-write (list 'define 'generated-items product-map) port)
      (writeln port)
      (pretty-write '(for ([item (hash-values generated-items)]) (add-item item)) port))))
