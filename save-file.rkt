#lang racket

(require json)
(require "items.rkt"
         "inventory.rkt")

(provide (struct-out game-data)
         get-default-data-path
         get-latest-save-file-path
         get-game-data
         inventory-key?)

;;;
;;; A struct representing the data that we snarfed from a save file.
;;;
;;; Contains only those parts that we need.
;;;
(struct game-data
  (
   ; Path of the save file.
   path
   ; Time it was last modified.
   modify-seconds
   ; An assoc list of inventories.
   ; The keys are pairs (conses) with a specific structure to identify an inventory; see inventory-key?.
   ; The values are inventories; see inventory.rkt.
   inventories
   ; List of ship names. Length varies; I believe the current in-game limit is three, but I'm not sure.
   ; The ship indexes in inventory keys corresponds to position in this list.
   starships
   ; List of vehicle names. Length varies; current in-game limit is three.
   ; The vehicle indexes in inventory keys corresponds to position in this list.
   vehicles
   ; Set of item names known by player
   known-items)
  #:transparent)

; TODO:
; Primary ship index: PlayerStateData.PrimaryShip
; Primary vehicle index: PlayerStateData.PrimaryVehicle
; Support older save files that are not obfuscated. Check version #, skip mapping.

;;;
;;; Mapping of JSON tags to old-style tag names, both represented as symbols.
;;;
;;; Contains those mappings needed to retrieve inventories, plus others
;;; that I happened to figure out while dumpster-diving in the save file.
;;;
(define json-tag-to-name
  #hash((F2P . Version)
        (8>q . Platform)
        (rnc . SpawnStateData)

        (6f= . PlayerStateData)
        (\;l5 . Inventory)
        (gan . Inventory_Cargo)
        (8ZP . FreighterInventory)
        (3Nc . Chest1Inventory)
        (IDc . Chest2Inventory)
        (M=: . Chest3Inventory)
        (iYp . Chest4Inventory)
        (<IP . Chest5Inventory)
        (qYJ . Chest6Inventory)
        (@e5 . Chest7Inventory)
        (5uh . Chest8Inventory)
        (5Tg . Chest9Inventory)
        (Bq< . Chest10Inventory)
        (\;?C . ChestMagicInventory) ; what is this?
        (fCh . ChestMagic2Inventory) ; what is this?
        ; list of ships
        ; each with Name, Resource, Inventory, Inventory_TechOnly, InventoryLayout, Location, Position, Direction
        (@Cs . ShipOwnership)
         
        ; list of vehicles
        ; each with TODO
        (P\;m . VehicleOwnership)

        ; Directly under PlayerStateData
        ;( . PrimaryShip)
        ; Currently set to zero, too many other possibilities.
        ;( . PrimaryVehicle)
        ; ProgressLevel, PrimaryVehicle => 5sx or DtI
        
        (:No . Slots)
        (b2n . Id)
        (3ZH . Index)
        (>Qh . X)
        (XJ> . Y)
        (F9q . MaxAmount)
        (1o9 . Amount)
        (Vn8 . Type)
        (elv . InventoryType)

        ; Stuff the player knows, under PlayerStateData
        (4kj . KnownTech)
        (eZ< . KnownProducts)
        (24< . KnownSpecials)
        
        ; Misc. stuff that I noticed.
        (jk4 . LastKnownPlayerState)
        (rnc . MultiplayerSpawn)
        (eVk . DamageFactor)
        (Kgt . WeaponInventory)

        (NKm . Name)
        (YTa . Location)
        (pMa . InventoryLayout)
        (9\;o . Level)
        (PMT . Inventory_TechOnly)
        (hl? . ValidSlotIndices)
        (B@N . Class)
        (lo6 . InventoryClass)
        (cTY . ProductMaxStorageMultiplier)
        (=Tb . Width)
        (N9> . Height)
        (0H2 . SubstanceMaxStorageMultiplier)
        (FTP . Version)
        (@bB . BaseStatValues)
        (QL1 . BaseStateID)
        (>MX . Value)
        (MMm . SpecialSlots)
        (iF: . IsCool)
        ; wMC, l?l <=> Position, Direction
        
        (NTx . Resource)
        (93M . Filename)
        (@EL . Seed)
        (QlJ . AltId)
        (<d2 . ProceduralTexture)
        (bnT . Samplers)
        (8P3 . DD)
        (\5L6 . UA)
        (bEr . VP)
        
        ))

(define name-to-json-tag (make-hasheq (hash-map json-tag-to-name (λ (k v) (cons v k)))))

(define (get-json-element json . names)
  (for/fold ([elem json])
            ([name names])
    (cond
      [(symbol? name) (hash-ref elem (hash-ref name-to-json-tag name))]
      [(integer? name) (sequence-ref elem name)])))


(define save-file-name-regexp (regexp "^save[0-9]*.hg"))

(define (json->inventory json . path)
  (define slots (apply get-json-element json (append path '(Slots))))
  (define inventory (make-inventory))
  ; TODO
  ; Filter out installed tech? (Maybe just by -1 amount?)
  (for/fold ([result inventory])
            ([slot slots]
             [index (in-naturals)])
    (define inventory-type (get-json-element slot 'Type 'InventoryType))
    (define amount (get-json-element slot 'Amount))
    (define id (get-json-element slot 'Id))
    (define item (get-item-by-save-id id))
    (cond
      [(or (<= amount 0) (string=? "Technology" inventory-type))
       result]
      [(void? item)
       (printf "Unknown item id in ~a [~a]: ~a~n" path index id)
       result]
      [else
       (inventory-deposit result item amount)])))

(define (json->chest-inventories json)
  (for/vector ([i (in-range 10)])
    (define name (string->symbol (format "Chest~sInventory" (+ 1 i))))
    (json->inventory json 'PlayerStateData name)))

(define (json->ship-inventories json)
  (define count (length (get-json-element json 'PlayerStateData 'ShipOwnership)))
  (reverse
   (for/fold ([result null])
             ([i (in-range (- count 1))])
     (define filename (get-json-element json 'PlayerStateData 'ShipOwnership i 'Resource 'Filename))
     (if (non-empty-string? filename)
         (cons (json->inventory json 'PlayerStateData 'ShipOwnership i 'Inventory) result)
         result))))

(define (json->vehicle-inventories json)
  (define vehicles-json (get-json-element json 'PlayerStateData 'VehicleOwnership))
  (define count (length vehicles-json))
  (for/fold ([result null]
             #:result (reverse result))
            ([vehicle vehicles-json]
             [i (in-range count)])
    (define location (get-json-element vehicle 'Location))
    (if (string? location)
        ; Use full path here to help with error messages.
        (cons (json->inventory json 'PlayerStateData 'VehicleOwnership i 'Inventory) result)
        ; A non-string location means, I think, an unused slot.
        result)))

(define (json->ships json)
  (define ships-json (get-json-element json 'PlayerStateData 'ShipOwnership))
  (reverse
   (for/fold ([result null])
             ([ship ships-json]
              [i (in-naturals)])
     (define filename (get-json-element ship 'Resource 'Filename))
     (define name (get-json-element ship 'Name))
     (cond
       [(non-empty-string? name) (cons name result)]
       [(non-empty-string? filename)
        (cons
         ; Best we can do right now is determine the ship type.
         (format "~a (~a)"
                 (match filename
                   ["MODELS/COMMON/SPACECRAFT/DROPSHIPS/DROPSHIP_PROC.SCENE.MBIN" "Hauler"]
                   ["MODELS/COMMON/SPACECRAFT/FIGHTERS/FIGHTER_PROC.SCENE.MBIN" "Fighter"]
                   ["MODELS/COMMON/SPACECRAFT/SCIENTIFIC/SCIENTIFIC_PROC.SCENE.MBIN" "Explorer"]
                   ["MODELS/COMMON/SPACECRAFT/SHUTTLE/SHUTTLE_PROC.SCENE.MBIN" "Shuttle"]
                   ["MODELS/COMMON/SPACECRAFT/S-CLASS/S-CLASS_PROC.SCENE.MBIN" "Exotic"])
                 i)
         result)]
       [else
        ; Appears to be an unused slot.
        result]))))

;;
;; Default vehicle names.
;;
;; TODO: Two more slots were added in or shortly before update 1.65.
;;       Don't yet know the names for them.
(define default-vehicle-names #["Roamer" "Nomad" "Colossus"])

(define (json->vehicles json)
  ; TODO: Checking for string Location to determine if slot in use.
  ; Not sure if they always appear in the order expected by the default list.
  (define vehicles-json (get-json-element json 'PlayerStateData 'VehicleOwnership))
  (for/fold ([result null]
             #:result (reverse result))
            ([vehicle vehicles-json]
             [i (in-naturals)])
    (define location (get-json-element vehicle 'Location))
    (define name (get-json-element vehicle 'Name))
    (if (string? location)
        (cons (cond [(non-empty-string? name) name]
                    [(< i (vector-length default-vehicle-names))
                     (vector-ref default-vehicle-names i)]
                    [else (format "Unknown~a" i)])
              result)
        result)))

(define (json->known-items json)
  (for/fold ([result (seteq)])
            ([key #[KnownTech KnownProducts KnownSpecials]])
    (set-union result
               (for/fold ([result2 (seteq)])
                         ([id (get-json-element json 'PlayerStateData key)]
                          [index (in-naturals)])
                 (define item (get-item-by-save-id id #f))
                 (cond
                   [item (set-add result2 (item$-name item))]
                   [else
                    (printf "Unknown item id in ~a [~a]: ~a~n" (list 'PlayerStateData key) index id)
                    result2])))))
    
(define (inventory-key? k)
  (match k
    [(cons 'exosuit (or 0 1)) #t]
    [(cons 'freighter 0) #t]
    [(cons 'ship (? integer? n)) #:when (not (negative? n)) #t]
    [(cons 'vehicle '(or 0 1 2)) #t]
    [(cons 'chest (or 0 1 2 3 4 5 6 7 8 9)) #t]
    [_ #f]))


(define (get-keyed-inventories json ships vehicles)
  (append
   (list
    (cons '(exosuit . 0)    (json->inventory json 'PlayerStateData 'Inventory))
    (cons '(exosuit . 1)    (json->inventory json 'PlayerStateData 'Inventory_Cargo))
    (cons '(freighter . 0)  (json->inventory json 'PlayerStateData 'FreighterInventory)))
   (for/list ([n ships]
              [i (json->ship-inventories json)])
     (cons (cons 'ship n) i))
   (for/list ([n vehicles]
              [i (json->vehicle-inventories json)])
     (cons (cons 'vehicle n) i))
   (for/list ([n (in-naturals)]
              [i (json->chest-inventories json)])
     (cons (cons 'chest n) i))))

(define (get-game-data save-file-path)
  (define modify-seconds (file-or-directory-modify-seconds save-file-path))
  (define json (call-with-input-file save-file-path read-json #:mode 'text))
  (define ships (json->ships json))
  (define vehicles (json->vehicles json))
  (game-data save-file-path
             modify-seconds
             (get-keyed-inventories json ships vehicles)
             ships
             vehicles
             (json->known-items json)))

(define (get-default-data-path)
  (match (system-type)
    ['windows
     (define nms (build-path (getenv "APPDATA") "HelloGames/NMS"))
     (when (not (directory-exists? nms))
       (raise-result-error 'nms-base-dir-not-found "existing directory"))
     nms]))

(define (get-latest-save-file-path [data-root (get-default-data-path)])
  (for*/fold ([best-file (void)]
              [best-date (void)]
              #:result best-file)
             ([sub (directory-list data-root #:build? #t)]
              #:when (directory-exists? sub)
              [file (directory-list sub #:build? #f)])
    (define file-path (build-path sub file))
    (cond
      [(and (file-exists? file-path) (regexp-match? save-file-name-regexp file))
       (define date (file-or-directory-modify-seconds file-path))
       (if (or (void? best-date) (> date best-date))
           (values (simplify-path file-path) date)
           (values best-file best-date))]
      [else
       (values best-file best-date)])))
