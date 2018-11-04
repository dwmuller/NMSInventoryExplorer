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

;;; 1. Adjust the following two root directories.
;;; 2. Run this module, examine the output.
;;;    Use the REPL to look, carefully (they're large!)
;;;    at the module-level variables (items, build-recipes, refiner-recipes).
;;; 3. When satisfied, call write-generated-items and write-generated-recipes to generate
;;;    new source files.

;;; WARNING! This module uses a lot of memory while running. Increase the allowed memory
;;; in Racket|Limit Memory. I suggest restarting DrRacket after finishing with it.


(define root (simplify-path "D:/NMS-tools/out-1.7/"))
(define output-root (simplify-path "C:/Users/danm/Documents/Racket/NMSRecipes/"))

;;;
;;; Low-level EXML Utilities
;;;
(define (read-doc path)
  (call-with-input-file path
    (λ (port)
      (parameterize ([collapse-whitespace #t])
        (document-element (read-xml/document port))))))

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

(define (names-value element . names)
  (cond
    [(null? names) (get-attribute element 'value)]
    [else
     (define child (child-element element
                                  '(Property)
                                  (λ (e) (string=? (car names) (get-attribute e 'name)))))
     (and child (apply names-value child (cdr names)))]))

(define (names->children element . names)
  (cond
    [(null? names) (child-element-sequence element '(Property))]
    [else
     (define child (child-element element
                                  '(Property)
                                  (λ (e) (string=? (car names) (get-attribute e 'name)))))
     (and child (apply names->children child (cdr names)))]))
  
;;;
;;; Schema-specific EXML utilities
;;;
(define (read-refiner-ingredient-xml elem)
  (cons (names-value elem "Id")
        (string->number (names-value elem "Amount"))))
  
(define (read-refiner-recipe-xml elem)
  (define result (names-value elem "Result" "Id"))
  (define amount (string->number (names-value elem "Result" "Amount")))
  ; TODO: Read TimeToMake
  (define inputs
    (for/list ([ingredient (names->children elem "Ingredients")])
      (read-refiner-ingredient-xml ingredient)))
  ; There have been, in fact, bogus recipe entries with no inputs
  (if (null? inputs)
      #f
      (list result amount inputs)))

(define (read-refiner-recipes doc)
  (apply append
         (for/list ([n (in-range 1 4)])
           (define elem-name (format "RefinerRecipeTable~aInput" n))
           (for/fold ([result null])
                     ([elem (names->children doc elem-name)])
             (define r (read-refiner-recipe-xml elem))
             (if r
                 (cons r result)
                 result)))))
  
(define (read-recipe-inputs-xml elem)
  (define recipe-inputs-xml (names->children elem "Requirements"))
  (for/list ([input recipe-inputs-xml])
    (define input-id (names-value input "ID"))
    (define input-amount (string->number (names-value input "Amount")))
    (cons input-id input-amount)))

(define (read-flags elem . defs)
  (for/fold ([result null])
            ([flag-spec defs])
    (define prefix (car flag-spec))
    (define value (apply names-value elem (cdr flag-spec)))
    (if value
        (cons (string->symbol (format "~a:~a" prefix value)) result)
        result)))

(define (read-technology-xml elem)
  (define name-lower-id (names-value elem "NameLower"))
  (define id            (or (names-value elem "Id") (names-value elem "ID")))
  (define base-value    (string->number (names-value elem "BaseValue")))
  (define flags         (read-flags elem
                                    '("TechShopRarity" "TechShopRarity" "TechnologyRarity")
                                    '("TechnologyRarity" "TechnologyRarity" "TechnologyRarity")
                                    '("Technology" "TechnologyCategory" "TechnologyCategory")))
  (define recipe-inputs (read-recipe-inputs-xml elem))
  (values name-lower-id (list* id base-value flags recipe-inputs)))

(define (read-reality-substance-xml elem)
  (define name-lower-id (names-value elem "NameLower"))
  (define id            (or (names-value elem "Id") (names-value elem "ID")))
  (define base-value    (string->number (names-value elem "BaseValue")))
  (define flags         (read-flags elem
                                    '("Product" "Type" "ProductCategory")
                                    '("Rarity" "Rarity" "Rarity")
                                    '("Substance" "SubstanceCategory" "SubstanceCategory")))
  (values name-lower-id (list id base-value flags)))

(define (read-product-xml elem)
  (define name-lower-id (names-value elem "NameLower"))
  (define id            (or (names-value elem "Id") (names-value elem "ID")))
  (define base-value    (string->number (names-value elem "BaseValue")))
  (define flags         (read-flags elem
                                    '("Product" "Type" "ProductCategory")
                                    '("Rarity" "Rarity" "Rarity")
                                    '("Substance" "SubstanceCategory" "SubstanceCategory")))
  (define recipe-inputs (read-recipe-inputs-xml elem))
  (values name-lower-id (list* id base-value flags recipe-inputs)))

;;;
;;; File-specific EXML procdures
;;;
(define (scan-localization-table path language name-id-map id-map)
  ; These files are huge, so avoid loading mappings in for the entire file, even though that
  ; would seem the more obvious way to go about this.
  (printf "Reading ~a.~n" path)
  (define doc (read-doc path))
  (unless (and (eq? 'Data (element-name doc))
               (equal? "TkLocalisationTable" (get-attribute doc 'template)))
    (raise-result-error 'load-localization-table
                        "<Data template=\"TkLocalisationTable\" ..."
                        (format "<~a template=\"~a\" ..." (element-name doc) (get-attribute doc 'template))))
  (for ([elem (child-element-sequence doc '(Property Property))])
    (when (string=? "TkLocalisationEntry.xml" (get-attribute elem 'value))
      (define name-id (names-value elem "Id"))
      (for ([data (hash-ref name-id-map name-id null)]
            [index (in-naturals)])
        (define label (string-append (names-value elem language "Value")
                                     (if (zero? index) "" (format " [~a]" index))))
        (define item-name (label->item-name label))
        (define id (first data))
        (hash-remove! name-id-map name-id)
        (hash-set! id-map
                   id
                   (item$ item-name id (second data) (third data) label))))))

; Item readers return two values:
;   lower-name-id
;   (list save-id base-value flags . recipe-input ...)

(define item-type-readers
  (hash "GcTechnology.xml"           read-technology-xml
        "GcRealitySubstanceData.xml" read-reality-substance-xml
        "GcProductData.xml"          read-product-xml))

(define table-member-types
  #hash(("GcTechnologyTable" . "GcTechnology.xml")
        ("GcSubstanceTable"  . "GcRealitySubstanceData.xml")
        ("GcProductTable"    . "GcProductData.xml")))

(define (load-item-table-doc path table-type save-id-set name-id-map)
  (printf "Reading ~a.~n" path)
  (define doc (read-doc path))
  (unless (and (eq? 'Data (element-name doc))
               (equal? table-type (get-attribute doc 'template)))
    (raise-result-error 'load-item-table-doc
                        (format "<Data template=\"~a\" ..." table-type)
                        (format "<~a template=\"~a\" ..." (element-name doc) (get-attribute doc 'template))))
  (define table-elem (child-element doc '(Property) (λ (e) (string=? "Table" (get-attribute e 'name)))))
  (unless table-elem
    (raise-argument-error 'read-table-xml
                          "Single Property element with attribute name='Table'"
                          (element-attributes (child-element doc '(Property)))))
  (define items-type (hash-ref table-member-types table-type))  
  (define item-reader (hash-ref item-type-readers items-type))
  (for ([item (child-element-sequence table-elem '(Property))]
        [index (in-naturals)])
    (when (string=? items-type (get-attribute item 'value))
      (define-values (name-lower-id data) (item-reader item))
      (cond
        [(set-member? save-id-set (car data))
         (printf "Duplicate save-id: ~a, entry ~a in ~a ~n" (car data) index path)]
        [else
         (set-add! save-id-set (car data))
         ;(when (hash-has-key? name-id-map name-lower-id) 
         ; Dealing with multiple references to the same lowercase name entry:
         ; CURRENTLY: Keep them all, add a suffix on dupes.
         ; ALSO TRIED: First one wins. No idea how many items we dropped.
         ; ALSO TRIED: Base on the fact that there are unused localization entries that would match, tried constructing alternate name reference.
         ;             This made things worse.
         ; ALSO TRIED: Last one wins. Not sure if this was better or worse.
         ;(printf "Duplicate ref to name ~a by entry ~a: ~a in ~a~n" name-lower-id index id path))
         (hash-update! name-id-map name-lower-id (λ (v) (cons data v)) null)]))))

; TODO: Load NMS_REALITY_GCPROCEDURALTECHNOLOGYTABLE? Items there do not have BaseValue.
; Loading only the _U3REALITY_ files leaves some basic stuff undefined.
; Loading only the _REALITY_ files seems to work OK.
; Additionally loading the _U3REALITY files finds a lot of duplicate save ids.
; Also note that the U3REALITY files have references to things that are also listed in LEGACYITEMTABLE and TRADINGCOSTTABLE. This makes
; me think that the U3REALITY and TRADINGCOST files are for backwards compatibility only. 
; Much investigation needed to figure out the right set.

; Read items first, put placeholder for name text. Then translate placeholders.(define build-recipes null)

(define (read-default-reality root)
  (define (get-filename name)
    (build-path root (regexp-replace ".MXML$" (names-value doc name) ".EXML")))
  (define path (build-path root "METADATA/REALITY/DEFAULTREALITY.EXML"))
  (printf "Reading ~a.~n" path)
  (define doc (read-doc path))
  (define template-type (get-attribute doc 'template))
  (unless (and (eq? 'Data (element-name doc))
               (equal? "GcRealityManagerData" template-type))
    (raise-argument-error 'read-default-reality
                          "Expected <Data template='GcRealityManagerData' ... "
                          (format "<~a template='~a'" (element-name doc) template-type)))

  ; Grab the names of the reality tables, which contain definitions for items.
  (define technology-table (get-filename "TechnologyTable"))
  (define substance-table  (get-filename "SubstanceTable"))
  (define product-table    (get-filename "ProductTable"))

  ; Refiner recipes are stored directly in the default reality doc.
  (define raw-refiner-recipes (read-refiner-recipes doc))
  
  ; We're done with the top-level doc; free up the memory its XML rep uses.
  (set! doc null)

  ; Now we load all the item in. Items also often have a primary crafting recipe associated
  ; with them. The name-id-map is keyed by an identifier for a language-specific name. The
  ; mapping of these names to items is not unique, so each map entry is a list of data for
  ; multiple items.
  ;
  ; The save-id (often just called the id in this program) identifies items in the save file,
  ; and *is* a unique but not user-friendly identifier for each item.
  (define name-id-map (make-hash))
  (define save-id-set (mutable-set))
  (load-item-table-doc technology-table "GcTechnologyTable" save-id-set name-id-map)
  (load-item-table-doc substance-table  "GcSubstanceTable"  save-id-set name-id-map)
  (load-item-table-doc product-table    "GcProductTable"    save-id-set name-id-map)
  ;(load-item-table-doc (build-path root (names-value doc "ProceduralProductTable")) "GcProceduralProductTable"  save-id-set name-id-map)

  ; Next, find English translations for each item based on the name-id. In the process
  ; of doing these, we also turn the item data into actual item$ struct objects. The
  ; English names are used to generate name-symbols that are used a lot in the program
  ; to identify items. (This was convenient when items were being defined manually, and is
  ; still useful when debugging, but is not strictly necessary anymore -- the save-ids would suffice.)
  (define all-item-data (apply append (hash-values name-id-map))) ; Save for later...
  (define id-map (make-hash))
  ; By default, my game seems to be using U.K. English rather than U.S. English, so let's stick with that.
  (scan-localization-table (build-path root "LANGUAGE/NMS_LOC1_ENGLISH.EXML") "English" name-id-map id-map)
  (scan-localization-table (build-path root "LANGUAGE/NMS_LOC4_ENGLISH.EXML") "English" name-id-map id-map)
  (scan-localization-table (build-path root "LANGUAGE/NMS_UPDATE3_ENGLISH.EXML") "English" name-id-map id-map)

  ; Report some results.
  (printf "Found and translated ~a items.~n" (length (hash-keys id-map)))
  (unless (null? (hash-keys name-id-map))
    (printf "Missing translations for ~a items:~n" (length (hash-keys name-id-map)))
    (for ([(key value) name-id-map])
      (printf "  ~a: ~a~n" key value)))

  ; Make up fake  names for the items for which we found no translation. Let's
  ; hope they don't show up in the UI, but if they do we might have to figure out
  ; why we didn't find user-friendly names for them.
  (for ([lst (hash-values name-id-map)])
    (for ([i lst])
      (define item-fake-name (string->symbol (first i)))
      (define id (first i))
      (hash-set! id-map id (item$ item-fake-name id (second i) (third i) id))))

  ; The basic and refiner recipes are currently expressed in terms of save-ids. Translate
  ; these to item names.
  (define (id->name id)
    (define item (hash-ref id-map id #f))
    (and item (item$-name item)))
  (define build-recipes        
    (for/list ([item all-item-data]
               #:when (> (length item) 3))
      (list* (id->name (first item))
             (for/list ([i (list-tail item 3)])
               (cons (id->name (car i)) (cdr i))))))
  (define refiner-recipes
    (for/fold ([result null])
              ([r raw-refiner-recipes])
      (define output (id->name (first r)))
      (define inputs
        (for/list ([i (third r)])
              (cons (id->name (car i)) (cdr i))))
      (cond
        [(and output (andmap car inputs))
          (list* (list* output (second r) inputs) result)]
        [else
         (printf "Unresolved refiner recipe: ~a.~n" r)
         result])))
  (values (hash-values id-map) build-recipes refiner-recipes))

(define-values (items build-recipes refiner-recipes)
  (read-default-reality root))

; Show developer what "flags" we found for items:
(pretty-print (list 'Flags (sort (remove-duplicates (append-map (λ (v) (item$-flags v)) items)) symbol<?)))

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
      (pretty-write (list 'define 'generated-items (list 'quote (sort items symbol<? #:key item$-name))) port)
      (writeln port)
      (pretty-write '(for ([item generated-items]) (add-item item)) port))))

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
      (pretty-write (list 'define 'generated-build-recipes (list 'quote (sort build-recipes symbol<? #:key car))) port)
      (pretty-write (list 'define 'generated-refiner-recipes (list 'quote (sort refiner-recipes symbol<? #:key car))) port)
      (pretty-write
       '(for ([r generated-build-recipes])
          (add-recipe-by-names 'build (car r) 1 (cdr r)))
       port)
      (pretty-write
       '(for ([r generated-refiner-recipes])
          (add-recipe-by-names 'refine (first r) (second r) (list-tail r 2)))
       port))))
