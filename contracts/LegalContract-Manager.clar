;; LegalContract-Manager
;; A platform for managing legal contracts with verification and execution capabilities

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-status (err u104))


(define-map contract-approval-requirements
    uint
    {
        required-approvals: uint,
        approvers: (list 10 principal),
        created-by: principal
    }
)

(define-map contract-approvals
    { contract-id: uint, approver: principal }
    {
        approved: bool,
        timestamp: uint,
        comments: (string-ascii 200)
    }
)

(define-map contract-approval-counts uint uint)


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

(define-map contract-templates
    uint 
    {
        name: (string-ascii 50),
        content: (string-ascii 1000),
        created-by: principal
    }
)

(define-data-var next-template-id uint u1)

(define-public (create-template (name (string-ascii 50)) (content (string-ascii 1000)))
    (let
        (
            (template-id (var-get next-template-id))
            (firm-data (unwrap! (map-get? legal-firms tx-sender) (err err-unauthorized)))
        )
        (asserts! (get active firm-data) (err err-unauthorized))
        (map-set contract-templates template-id {
            name: name,
            content: content,
            created-by: tx-sender
        })
        (var-set next-template-id (+ template-id u1))
        (ok template-id)
    )
)

(define-read-only (get-template (template-id uint))
    (ok (map-get? contract-templates template-id))
)


(define-map contract-signatures
    { contract-id: uint, signer: principal }
    { signed: bool, timestamp: uint }
)

(define-public (sign-contract (contract-id uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
            (is-party (index-of? (get parties contract) tx-sender))
        )
        (asserts! (not (is-none is-party)) (err err-unauthorized))
        (map-set contract-signatures { contract-id: contract-id, signer: tx-sender }
            { signed: true, timestamp: stacks-block-height }
        )
        (ok true)
    )
)

(define-read-only (get-signature-status (contract-id uint) (signer principal))
    (ok (map-get? contract-signatures { contract-id: contract-id, signer: signer }))
)


(define-map contract-versions
    { contract-id: uint, version: uint }
    {
        content: (string-ascii 1000),
        timestamp: uint,
        modified-by: principal
    }
)

(define-public (create-contract-version (contract-id uint) (content (string-ascii 1000)))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
            (current-version (default-to u0 (get-last-version contract-id)))
        )
        (asserts! (is-eq (get creator contract) tx-sender) (err err-unauthorized))
        (map-set contract-versions 
            { contract-id: contract-id, version: (+ current-version u1) }
            { content: content, timestamp: stacks-block-height, modified-by: tx-sender }
        )
        (ok (+ current-version u1))
    )
)

(define-read-only (get-contract-version (contract-id uint) (version uint))
    (ok (map-get? contract-versions { contract-id: contract-id, version: version }))
)

(define-private (get-last-version (contract-id uint))
    (some u1)
)


(define-map disputes
    uint
    {
        contract-id: uint,
        filed-by: principal,
        description: (string-ascii 500),
        status: (string-ascii 20),
        resolution: (optional (string-ascii 500))
    }
)

(define-data-var next-dispute-id uint u1)

(define-public (file-dispute (contract-id uint) (description (string-ascii 500)))
    (let
        (
            (dispute-id (var-get next-dispute-id))
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
        )
        (asserts! (is-some (index-of? (get parties contract) tx-sender)) (err err-unauthorized))
        (map-set disputes dispute-id {
            contract-id: contract-id,
            filed-by: tx-sender,
            description: description,
            status: "open",
            resolution: none
        })
        (var-set next-dispute-id (+ dispute-id u1))
        (ok dispute-id)
    )
)

(define-map contract-expiration
    uint
    {
        expiry-height: uint,
        auto-renew: bool
    }
)

(define-public (set-contract-expiration (contract-id uint) (blocks uint) (auto-renew bool))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
        )
        (asserts! (is-eq (get creator contract) tx-sender) (err err-unauthorized))
        (map-set contract-expiration contract-id {
            expiry-height: (+ stacks-block-height blocks),
            auto-renew: auto-renew
        })
        (ok true)
    )
)

(define-read-only (is-contract-expired (contract-id uint))
    (match (map-get? contract-expiration contract-id)
        expiry-data (ok (> stacks-block-height (get expiry-height expiry-data)))
        (ok false)
    )
)


(define-map contract-comments
    { contract-id: uint, comment-id: uint }
    {
        author: principal,
        content: (string-ascii 500),
        timestamp: uint
    }
)

(define-map contract-comment-counts uint uint)

(define-public (add-comment (contract-id uint) (content (string-ascii 500)))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
            (comment-count (default-to u0 (map-get? contract-comment-counts contract-id)))
        )
        (asserts! (is-some (index-of? (get parties contract) tx-sender)) (err err-unauthorized))
        (map-set contract-comments 
            { contract-id: contract-id, comment-id: (+ comment-count u1) }
            { author: tx-sender, content: content, timestamp: stacks-block-height }
        )
        (map-set contract-comment-counts contract-id (+ comment-count u1))
        (ok (+ comment-count u1))
    )
)

(define-read-only (get-comment (contract-id uint) (comment-id uint))
    (ok (map-get? contract-comments { contract-id: contract-id, comment-id: comment-id }))
)


(define-map contract-payments
    uint 
    {
        amount: uint,
        payer: principal,
        payee: principal,
        released: bool
    }
)

(define-public (deposit-payment (contract-id uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
            (payment-amount (unwrap! (map-get? contract-payments contract-id) (err err-not-found)))
        )
                ;; TODO: Check if payment is released
        ;; (try! (stx-transfer? (get amount payment-amount) tx-sender (as-contract tx-sender)))
        (ok (map-set contract-payments contract-id 
            (merge payment-amount { released: false })))
    )
)

(define-public (release-payment (contract-id uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
            (payment (unwrap! (map-get? contract-payments contract-id) (err err-not-found)))
        )
        (asserts! (is-eq (get creator contract) tx-sender) (err err-unauthorized))
        (asserts! (not (get released payment)) (err err-unauthorized))
        ;; TODO: Check if payment is released
        ;; (try! (as-contract (stx-transfer? (get amount payment) tx-sender (get payee payment))))
        (ok (map-set contract-payments contract-id 
            (merge payment { released: true })))
    )
)


(define-map contract-milestones
    { contract-id: uint, milestone-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        deadline: uint,
        completed: bool,
        verified-by: (optional principal)
    }
)

(define-map milestone-counts uint uint)

(define-public (add-milestone (contract-id uint) (title (string-ascii 100)) (description (string-ascii 500)) (deadline uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
            (milestone-count (default-to u0 (map-get? milestone-counts contract-id)))
        )
        (asserts! (is-eq (get creator contract) tx-sender) (err err-unauthorized))
        (map-set contract-milestones
            { contract-id: contract-id, milestone-id: (+ milestone-count u1) }
            {
                title: title,
                description: description,
                deadline: deadline,
                completed: false,
                verified-by: none
            }
        )
        (map-set milestone-counts contract-id (+ milestone-count u1))
        (ok (+ milestone-count u1))
    )
)

(define-public (verify-milestone (contract-id uint) (milestone-id uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
            (milestone (unwrap! (map-get? contract-milestones { contract-id: contract-id, milestone-id: milestone-id }) (err err-not-found)))
        )
        (asserts! (is-some (index-of? (get parties contract) tx-sender)) (err err-unauthorized))
        (ok (map-set contract-milestones
            { contract-id: contract-id, milestone-id: milestone-id }
            (merge milestone {
                completed: true,
                verified-by: (some tx-sender)
            })
        ))
    )
)


(define-map contract-milestonesS
    { contract-id: uint, milestone-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        deadline: uint,
        completed: bool,
        verified-by: (optional principal)
    }
)

;; (define-map milestone-counts uint uint)

(define-public (add-milestone-v1 (contract-id uint) (title (string-ascii 100)) (description (string-ascii 500)) (deadline uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
            (milestone-count (default-to u0 (map-get? milestone-counts contract-id)))
        )
        (asserts! (is-eq (get creator contract) tx-sender) (err err-unauthorized))
        (map-set contract-milestones
            { contract-id: contract-id, milestone-id: (+ milestone-count u1) }
            {
                title: title,
                description: description,
                deadline: deadline,
                completed: false,
                verified-by: none
            }
        )
        (map-set milestone-counts contract-id (+ milestone-count u1))
        (ok (+ milestone-count u1))
    )
)

(define-public (verify-milestone-v2 (contract-id uint) (milestone-id uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) (err err-not-found)))
            (milestone (unwrap! (map-get? contract-milestones { contract-id: contract-id, milestone-id: milestone-id }) (err err-not-found)))
        )
        (asserts! (is-some (index-of? (get parties contract) tx-sender)) (err err-unauthorized))
        (ok (map-set contract-milestones
            { contract-id: contract-id, milestone-id: milestone-id }
            (merge milestone {
                completed: true,
                verified-by: (some tx-sender)
            })
        ))
    )
)

(define-public (set-approval-requirements (contract-id uint) (required-approvals uint) (approvers (list 10 principal)))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) err-not-found))
        )
        (asserts! (is-eq (get creator contract) tx-sender) err-unauthorized)
        (asserts! (> required-approvals u0) err-invalid-status)
        (asserts! (<= required-approvals (len approvers)) err-invalid-status)
        (map-set contract-approval-requirements contract-id {
            required-approvals: required-approvals,
            approvers: approvers,
            created-by: tx-sender
        })
        (map-set contract-approval-counts contract-id u0)
        (ok true)
    )
)

(define-public (submit-approval (contract-id uint) (comments (string-ascii 200)))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) err-not-found))
            (approval-req (unwrap! (map-get? contract-approval-requirements contract-id) err-not-found))
            (current-count (default-to u0 (map-get? contract-approval-counts contract-id)))
            (existing-approval (map-get? contract-approvals { contract-id: contract-id, approver: tx-sender }))
        )
        (asserts! (is-some (index-of? (get approvers approval-req) tx-sender)) err-unauthorized)
        (asserts! (is-none existing-approval) err-already-exists)
        (map-set contract-approvals 
            { contract-id: contract-id, approver: tx-sender }
            {
                approved: true,
                timestamp: stacks-block-height,
                comments: comments
            }
        )
        (map-set contract-approval-counts contract-id (+ current-count u1))
        (ok true)
    )
)

(define-public (revoke-approval (contract-id uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) err-not-found))
            (approval-req (unwrap! (map-get? contract-approval-requirements contract-id) err-not-found))
            (existing-approval (unwrap! (map-get? contract-approvals { contract-id: contract-id, approver: tx-sender }) err-not-found))
            (current-count (default-to u0 (map-get? contract-approval-counts contract-id)))
        )
        (asserts! (is-some (index-of? (get approvers approval-req) tx-sender)) err-unauthorized)
        (asserts! (get approved existing-approval) err-invalid-status)
        (map-delete contract-approvals { contract-id: contract-id, approver: tx-sender })
        (map-set contract-approval-counts contract-id (- current-count u1))
        (ok true)
    )
)

(define-public (activate-contract-with-approvals (contract-id uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) err-not-found))
            (approval-req (unwrap! (map-get? contract-approval-requirements contract-id) err-not-found))
            (approval-count (default-to u0 (map-get? contract-approval-counts contract-id)))
        )
        (asserts! (is-eq (get creator contract) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status contract) STATUS_PENDING) err-invalid-status)
        (asserts! (>= approval-count (get required-approvals approval-req)) err-unauthorized)
        (map-set contracts contract-id 
            (merge contract { 
                status: STATUS_ACTIVE,
                updated-at: stacks-block-height
            })
        )
        (ok true)
    )
)

(define-read-only (get-approval-requirements (contract-id uint))
    (ok (map-get? contract-approval-requirements contract-id))
)

(define-read-only (get-approval-status (contract-id uint) (approver principal))
    (ok (map-get? contract-approvals { contract-id: contract-id, approver: approver }))
)

(define-read-only (get-approval-count (contract-id uint))
    (ok (map-get? contract-approval-counts contract-id))
)

(define-read-only (is-fully-approved (contract-id uint))
    (match (map-get? contract-approval-requirements contract-id)
        approval-req 
        (let
            (
                (current-count (default-to u0 (map-get? contract-approval-counts contract-id)))
            )
            (ok (>= current-count (get required-approvals approval-req)))
        )
        (ok false)
    )
)

(define-read-only (get-pending-approvers (contract-id uint))
    (match (map-get? contract-approval-requirements contract-id)
        approval-req (ok (some (get approvers approval-req)))
        (ok none)
    )
)