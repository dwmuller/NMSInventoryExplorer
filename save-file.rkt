#lang racket

(require json)
(require "items.rkt"
         "inventory.rkt")

; JSON nodes containing inventory slots:
; Exosuit General: PlayerStateData.Inventory
; Exosuit Cargo: PlayerStateData.Inventory_Cargo
; Freighter general: PlayerStateData.FreighterInventory
; Container inventory: PlayerStateData.ChestXInventory (where X is container number, 1-10)

; Primary ship index: PlayerStateData.PrimaryShip
; Primary vehicle index: PlayerStateData.PrimaryVehicle
;


;PrimaryVehicleNode.Inventory

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
        (elv . InventoryType)


        ; Misc. stuff that I noticed.
        (jk4 . LastKnownPlayerState)
        (rnc . MultiplayerSpawn)
        (eVk . DamageFactor)
        (Kgt . WeaponInventory)

        (NKm . Name)
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
        (eZ< . KnownProducts)
        (4kj . KnownTech)
        
        ))

;; Ids of items that we ignore as irrelevant for our purposes here.
(define ignored-item-ids
  '("^JET1"))
(define name-to-json-tag (make-hasheq (hash-map json-tag-to-name (λ (k v) (cons v k)))))

(define (get-json-element json . names)
  (for/fold ([elem json])
            ([name names])
    (cond
      [(symbol? name) (hash-ref elem (hash-ref name-to-json-tag name))]
      [(integer? name) (sequence-ref elem name)])))


(define save-file-name-regexp (regexp "^save[0-9]*.hg"))

(define (add-inventory json path inventory)
  (define slots (apply get-json-element json (append path '(Slots))))
  ; TODO
  ; Need mapping of internal item name to symbol. Add to items.rkt.
  ; Filter out tech (maybe just by -1 amount) and empty slots.
  (for/fold ([result inventory])
            ([slot slots])
    (define amount (get-json-element slot 'Amount))
    (define id (get-json-element slot 'Id))
    (define item (get-item-by-save-id id))
    (cond
      [(<= amount 0) result]
      [(void? item)
       (printf "Unknown item id in ~s: ~s~n" path id)
       result]
      [else
       (inventory-deposit result item amount)])))

(define (get-ownership-paths json ownership)
  (define count (length (get-json-element json 'PlayerStateData ownership)))
  (for/list ([i (in-range (- count 1))])
    (list 'PlayerStateData ownership i 'Inventory)))

(define (inventory-paths json)
  (append
   '(
     (PlayerStateData Inventory)
     (PlayerStateData Inventory_Cargo)
     (PlayerStateData FreighterInventory)
     (PlayerStateData Chest1Inventory)
     (PlayerStateData Chest2Inventory)
     (PlayerStateData Chest3Inventory)
     (PlayerStateData Chest4Inventory)
     (PlayerStateData Chest5Inventory)
     (PlayerStateData Chest6Inventory)
     (PlayerStateData Chest7Inventory)
     (PlayerStateData Chest8Inventory)
     (PlayerStateData Chest9Inventory)
     (PlayerStateData Chest10Inventory))
   (get-ownership-paths json 'ShipOwnership)
   (get-ownership-paths json 'VehicleOwnership)))

(define (gather-inventory-from-save-json json)
  (for/fold ([inventory (make-inventory)])
            ([path (inventory-paths json)])
    (add-inventory json path inventory)))

(define (read-save-file path)
  (call-with-input-file path read-json #:mode 'text))

(define (read-inventory path)
  (gather-inventory-from-save-json (read-save-file path)))

(define (get-nms-data-path)
  (match (system-type)
    ['windows
     (define nms (build-path (getenv "APPDATA") "HelloGames/NMS"))
     (when (not (directory-exists? nms))
       (raise-result-error 'nms-base-dir-not-found "existing directory"))
     nms]))

(define (get-latest-game-save-file-path)
  (for*/fold ([best-file (void)]
              [best-date (void)]
              #:result best-file)
             ([sub (directory-list (get-nms-data-path) #:build? #t)]
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
   

