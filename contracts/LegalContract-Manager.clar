;; LegalContract-Manager
;; A platform for managing legal contracts with verification and execution capabilities

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-status (err u104))

;; Data Variables
(define-data-var next-contract-id uint u1)
(define-data-var platform-fee uint u100) ;; in STX

;; Contract Status Types
(define-constant STATUS_DRAFT "draft")
(define-constant STATUS_PENDING "pending")
(define-constant STATUS_ACTIVE "active")
(define-constant STATUS_COMPLETED "completed")
(define-constant STATUS_DISPUTED "disputed")

;; Data Maps
(define-map legal-firms 
    principal 
    {
        active: bool,
        contracts-created: uint,
        subscription-expiry: uint
    }
)

(define-map contracts 
    uint 
    {
        creator: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        parties: (list 10 principal),
        status: (string-ascii 20),
        created-at: uint,
        updated-at: uint
    }
)

;; Public Functions

;; Register a legal firm
(define-public (register-legal-firm (subscription-duration uint))
    (let
        (
            (payment (* subscription-duration (var-get platform-fee)))
        )
        (try! (stx-transfer? payment tx-sender contract-owner))
        (ok (map-set legal-firms tx-sender {
            active: true,
            contracts-created: u0,
            subscription-expiry: (+ stacks-block-height (* subscription-duration u144)) ;; ~1 day blocks
        }))
    )
)

;; Create new legal contract
(define-public (create-contract (title (string-ascii 100)) (description (string-ascii 500)) (parties (list 10 principal)))
    (let
        (
            (firm-data (unwrap! (map-get? legal-firms tx-sender) (err err-unauthorized)))
            (contract-id (var-get next-contract-id))
        )
        (asserts! (get active firm-data) (err err-unauthorized))
        (asserts! (<= stacks-block-height (get subscription-expiry firm-data)) (err err-unauthorized))
        
        (map-set contracts contract-id {
            creator: tx-sender,
            title: title,
            description: description,
            parties: parties,
            status: STATUS_DRAFT,
            created-at: stacks-block-height,
            updated-at: stacks-block-height
        })
        
        (var-set next-contract-id (+ contract-id u1))
        (ok contract-id)
    )
)

;; Update contract status
(define-public (update-contract-status (contract-id uint) (new-status (string-ascii 20)))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
        )
        (asserts! (is-eq (get creator contract) tx-sender) (err err-unauthorized))
        (ok (map-set contracts contract-id 
            (merge contract { 
                status: new-status,
                updated-at: stacks-block-height
            })
        ))
    )
)

;; Read Only Functions

;; Get contract details
(define-read-only (get-contract (contract-id uint))
    (ok (map-get? contracts contract-id))
)

;; Get legal firm details
(define-read-only (get-legal-firm (firm-principal principal))
    (ok (map-get? legal-firms firm-principal))
)

;; Check if legal firm subscription is active
(define-read-only (is-subscription-active (firm-principal principal))
    (match (map-get? legal-firms firm-principal)
        firm-data (ok (and 
            (get active firm-data)
            (<= stacks-block-height (get subscription-expiry firm-data))
        ))
        (ok false)
    )
)

;; Private Functions

;; Validate contract status transition
(define-private (is-valid-status-transition (current-status (string-ascii 20)) (new-status (string-ascii 20)))
    (or
        (and (is-eq current-status STATUS_DRAFT) (is-eq new-status STATUS_PENDING))
        (and (is-eq current-status STATUS_PENDING) (is-eq new-status STATUS_ACTIVE))
        (and (is-eq current-status STATUS_ACTIVE) (is-eq new-status STATUS_COMPLETED))
        (is-eq new-status STATUS_DISPUTED)
    )
)

