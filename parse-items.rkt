#lang racket

(require xml
         xml/path
         racket/generator
         racket/date
         "items.rkt"
         "recipes.rkt")

;;; Use "NMS Modding Station" and MBINCompiler to extract the needed EXML files.
;;; These files are insanely verbose. The typical structure is a Data element,
;;; containing nested trees of Property elements with "name" attributes
;;; and occasional "value" attributes. About as inefficient a use of XML as you can
;;; imagine.

;;; Adjust the following two root directories.
;;; Run this module, examine the output. Use the REPL to look, carefully (they're large!)
;;; at the module-level variables (id-map, recipe-list, missing-item-translations).
;;; When satisfied, call write-generated-items and write-generated-recipes to generate
;;; new files.

;;; WARNING! This module uses a lot of memory while running. Increase the allowed memory
;;; in Racket|Limit Memory. I suggest restarting DrRacket after finishing with it.


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
     (and child (names-value child (cdr names)))]))

(define (names->children element names)
  (cond
    [(null? names) (child-element-sequence element '(Property))]
    [else
     (define child (child-element element
                                  '(Property)
                                  (λ (e) (string=? (car names) (get-attribute e 'name)))))
     (and child (names->children child (cdr names)))]))
  

(define (scan-localization-table path language name-id-map id-map)
  ; These files are huge, so avoid loading mappings in for the entire file, even though that
  ; would seem the more obvious way to go about this.
  (define doc (read-doc path))
  (unless (and (eq? 'Data (element-name doc))
               (equal? "TkLocalisationTable" (get-attribute doc 'template)))
    (raise-result-error 'load-localization-table
                        "<Data template=\"TkLocalisationTable\" ..."
                        (format "<~a template=\"~a\" ..." (element-name doc) (get-attribute doc 'template))))
  (for ([elem (child-element-sequence doc '(Property Property))])
    (when (string=? "TkLocalisationEntry.xml" (get-attribute elem 'value))
      (define name-id (names-value elem '("Id")))
      (for ([data (hash-ref name-id-map name-id null)]
            [index (in-naturals)])
        (define label (string-append (names-value elem (list language "Value"))
                                     (if (zero? index) "" (format " [~a]" index))))
        (define item-name (label->item-name label))
        (define id (first data))
        (hash-remove! name-id-map name-id)
        (hash-set! id-map
                   id
                   (item$ item-name id (second data) (third data) label))))))

(define (save-recipe elem id recipe-list)
  (define recipe-inputs (names->children elem '("Requirements")))
  (when (and recipe-inputs (not (null? recipe-inputs)))
    (define recipe
      (cons id (for/fold ([result null])
                         ([input recipe-inputs])
                 (define input-id (names-value input '("ID")))
                 (define input-amount (names-value input '("Amount")))
                 (cons (cons input-id input-amount) result))))
    (set-box! recipe-list (cons recipe (unbox recipe-list)))))

(define (read-items name-id-map save-id-set recipe-list path table-type items-type . extra-flags)
  (define doc (read-doc path))
  (unless (and (eq? 'Data (element-name doc))
               (equal? table-type (get-attribute doc 'template)))
    (raise-result-error 'read-products
                        (format "<Data template=\"~a\" ..." table-type)
                        (format "<~a template=\"~a\" ..." (element-name doc) (get-attribute doc 'template))))
  (for ([elem (child-element-sequence doc '(Property Property))]
        [index (in-naturals)])
    (when (string=? items-type (get-attribute elem 'value))
      (define name-lower-id      (names-value elem '("NameLower")))
      (define id                 (or (names-value elem '("Id")) (names-value elem '("ID"))))
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
        [(set-member? save-id-set id)
         (printf "Duplicate save-id: ~a, entry ~a in ~a ~n" id index path)]
        [else
         (set-add! save-id-set id)
         ;(when (hash-has-key? name-id-map name-lower-id) 
            ; Dealing with multiple references to lowercase name entries:
            ; CURRENTLY: Keep them all, but they'll look like dupes in the UI.
            ; ALSO TRIED: First one wins. No idea how many items we dropped.
            ; ALSO TRIED: Base on the fact that there are unused localization entries that would match, tried constructing alternate name reference.
            ;             This made things worse.
            ; ALSO TRIED: Last one wins. Not sure if this was better or worse.
            ;(printf "Duplicate ref to name ~a by entry ~a: ~a in ~a~n" name-lower-id index id path))
         (hash-update! name-id-map name-lower-id (λ (v) (cons data v)) null)
         (save-recipe elem id recipe-list)]))))

; Read items first, put placeholder for name text. Then translate placeholders.
(define save-id-set (mutable-set))
(define name-id-map (make-hash))
(define recipe-list (box null))

; TODO: Load NMS_REALITY_GCPROCEDURALTECHNOLOGYTABLE? Items there do not have BaseValue.
; Loading only the _U3REALITY_ files leaves some basic stuff undefined.
; Loading only the _REALITY_ files seems to work OK.
; Additionally loading the _U3REALITY files finds a lot of duplicate save ids.
; Much investigation needed to figure out the right set.

(read-items name-id-map save-id-set recipe-list (build-path root "METADATA/REALITY/TABLES/NMS_REALITY_GCPRODUCTTABLE.EXML") "GcProductTable" "GcProductData.xml"
            '("Product" "Type" "ProductCategory")
            '("Rarity" "Rarity" "Rarity")
            '("Substance" "SubstanceCategory" "SubstanceCategory"))
(read-items name-id-map save-id-set recipe-list (build-path root "METADATA/REALITY/TABLES/NMS_REALITY_GCSUBSTANCETABLE.EXML") "GcSubstanceTable" "GcRealitySubstanceData.xml"
            '("Rarity" "Rarity" "Rarity")
            '("Substance" "SubstanceCategory" "SubstanceCategory"))
(read-items name-id-map save-id-set recipe-list (build-path root "METADATA/REALITY/TABLES/NMS_REALITY_GCTECHNOLOGYTABLE.EXML") "GcTechnologyTable" "GcTechnology.xml"
            '("TechShopRarity" "TechShopRarity" "TechnologyRarity")
            '("TechnologyRarity" "TechnologyRarity" "TechnologyRarity")
            '("Technology" "TechnologyCategory" "TechnologyCategory"))
;(read-items name-id-map save-id-set recipe-list (build-path root "METADATA/REALITY/TABLES/NMS_U3REALITY_GCPRODUCTTABLE.EXML") "GcProductTable" "GcProductData.xml"
;            '("Product" "Type" "ProductCategory")
;            '("Rarity" "Rarity" "Rarity")
;            '("Substance" "SubstanceCategory" "SubstanceCategory"))
;(read-items name-id-map save-id-set recipe-list (build-path root "METADATA/REALITY/TABLES/NMS_U3REALITY_GCSUBSTANCETABLE.EXML") "GcSubstanceTable" "GcRealitySubstanceData.xml"
;            '("Rarity" "Rarity" "Rarity")
;            '("Substance" "SubstanceCategory" "SubstanceCategory"))
;(read-items name-id-map save-id-set recipe-list (build-path root "METADATA/REALITY/TABLES/NMS_U3REALITY_GCTECHNOLOGYTABLE.EXML") "GcTechnologyTable" "GcTechnology.xml"
;            '("TechShopRarity" "TechShopRarity" "TechnologyRarity")
;            '("TechnologyRarity" "TechnologyRarity" "TechnologyRarity")
;            '("Technology" "TechnologyCategory" "TechnologyCategory"))

(define id-map (make-hash))

; By default, my game seems to be using U.K. English rather than U.S. English, so let's stick with that.
;(scan-localization-table (build-path root "LANGUAGE/NMS_LOC1_USENGLISH.EXML") "USEnglish" name-id-map id-map)
;(scan-localization-table (build-path root "LANGUAGE/NMS_LOC4_USENGLISH.EXML") "USEnglish" name-id-map id-map)
;(scan-localization-table (build-path root "LANGUAGE/NMS_UPDATE3_USENGLISH.EXML") "USEnglish" name-id-map id-map)
(scan-localization-table (build-path root "LANGUAGE/NMS_LOC1_ENGLISH.EXML") "English" name-id-map id-map)
(scan-localization-table (build-path root "LANGUAGE/NMS_LOC4_ENGLISH.EXML") "English" name-id-map id-map)
(scan-localization-table (build-path root "LANGUAGE/NMS_UPDATE3_ENGLISH.EXML") "English" name-id-map id-map)

; Attempted additional scans to find missing items; no luck.
;(scan-localization-table (build-path root "LANGUAGE/NMS_LOC4_ENGLISH.EXML") "English" name-id-map id-map)

(printf "Found and translated ~a items.~n" (length (hash-keys id-map)))
(unless (null? (hash-keys name-id-map))
  (printf "Missing translations for ~a items.~n" (length (hash-keys name-id-map))))
(pretty-print (list 'Flags (sort (remove-duplicates (append-map (λ (v) (item$-flags v)) (hash-values id-map))) symbol<?)))

(for ([lst (hash-values name-id-map)])
  (for ([i lst])
    (define item-fake-name (string->symbol (car i)))
    (define id (car i))
    (hash-set! id-map id (item$ item-fake-name id (second i) (third i) id))))
    
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
      (pretty-write (list 'define 'generated-items (list 'quote (hash-values id-map))) port)
      (writeln port)
      (pretty-write '(for ([item generated-items]) (add-item item)) port))))

(define resolved-recipes
  (for/list ([r (unbox recipe-list)])
    (list (item$-name (hash-ref id-map (car r)))
          (for/list ([i (cdr r)])
            (cons (item$-name (hash-ref id-map (car i))) (cdr i))))))

(define (write-generated-recipes)
  (call-with-output-file (build-path output-root "generated-recipes.rkt") #:mode 'text #:exists 'replace
    (λ (port)
      (displayln "#lang racket" port)
      (define timestamp (parameterize ([date-display-format 'iso-8601])
                          (let ([d (seconds->date (current-seconds))])
                            (format "~a ~a" (date->string d #t) (date*-time-zone-name d)))))
      (define user (getenv "USERNAME"))
      (displayln (format "; Generated via parse-items.rkt by ~a at ~a" user timestamp) port)
      (pretty-write '(require "items.rkt" "recipes.rkt") port)
      (writeln port)
      (pretty-write (list 'define 'generated-recipes (list 'quote (unbox recipe-list))) port)
      (pretty-write
       '(for ([r generated-recipes])
          (add-recipe-by-names r))
       port))))
