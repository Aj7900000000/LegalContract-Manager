;; Contract Amendment & Voting System
;; Enables democratic modification of existing contracts through party voting

;; Error codes
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-status (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-amendment-closed (err u106))
(define-constant err-insufficient-votes (err u107))

;; Amendment status constants
(define-constant AMENDMENT_PROPOSED "proposed")
(define-constant AMENDMENT_VOTING "voting")
(define-constant AMENDMENT_APPROVED "approved")
(define-constant AMENDMENT_REJECTED "rejected")
(define-constant AMENDMENT_IMPLEMENTED "implemented")

;; Reference to main contract structure
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

;; Data variables for amendment tracking
(define-data-var next-amendment-id uint u1)
(define-data-var voting-period-blocks uint u1440) ;; ~10 days default

;; Amendment proposals storage
(define-map contract-amendments
    uint
    {
        contract-id: uint,
        proposed-by: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        proposed-changes: (string-ascii 1000),
        status: (string-ascii 20),
        voting-deadline: uint,
        required-votes: uint,
        current-yes-votes: uint,
        current-no-votes: uint,
        created-at: uint
    }
)

;; Individual vote tracking
(define-map amendment-votes
    { amendment-id: uint, voter: principal }
    {
        vote: bool, ;; true for yes, false for no
        timestamp: uint,
        reasoning: (optional (string-ascii 200))
    }
)

;; Vote delegation system
(define-map vote-delegations
    { contract-id: uint, delegator: principal }
    {
        delegate: principal,
        active: bool,
        created-at: uint
    }
)

;; Amendment implementation tracking
(define-map amendment-implementations
    uint
    {
        implemented-by: principal,
        implementation-timestamp: uint,
        original-content: (string-ascii 1000),
        new-content: (string-ascii 1000)
    }
)

;; Propose a new contract amendment
(define-public (propose-amendment 
    (contract-id uint) 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (proposed-changes (string-ascii 1000)))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) err-not-found))
            (amendment-id (var-get next-amendment-id))
            (parties-count (len (get parties contract)))
            (required-votes (+ (/ parties-count u2) u1)) ;; majority + 1
        )
        ;; Verify proposer is a contract party
        (asserts! (is-some (index-of? (get parties contract) tx-sender)) err-unauthorized)
        
        ;; Create amendment proposal
        (map-set contract-amendments amendment-id {
            contract-id: contract-id,
            proposed-by: tx-sender,
            title: title,
            description: description,
            proposed-changes: proposed-changes,
            status: AMENDMENT_PROPOSED,
            voting-deadline: (+ stacks-block-height (var-get voting-period-blocks)),
            required-votes: required-votes,
            current-yes-votes: u0,
            current-no-votes: u0,
            created-at: stacks-block-height
        })
        
        (var-set next-amendment-id (+ amendment-id u1))
        (ok amendment-id)
    )
)

;; Start voting period for an amendment
(define-public (start-amendment-voting (amendment-id uint))
    (let
        (
            (amendment (unwrap! (map-get? contract-amendments amendment-id) err-not-found))
        )
        (asserts! (is-eq (get proposed-by amendment) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status amendment) AMENDMENT_PROPOSED) err-invalid-status)
        
        (map-set contract-amendments amendment-id
            (merge amendment { status: AMENDMENT_VOTING })
        )
        (ok true)
    )
)

;; Cast vote on an amendment
(define-public (vote-on-amendment (amendment-id uint) (vote bool) (reasoning (optional (string-ascii 200))))
    (let
        (
            (amendment (unwrap! (map-get? contract-amendments amendment-id) err-not-found))
            (contract (unwrap! (map-get? contracts (get contract-id amendment)) err-not-found))
            (existing-vote (map-get? amendment-votes { amendment-id: amendment-id, voter: tx-sender }))
            (delegation (map-get? vote-delegations { contract-id: (get contract-id amendment), delegator: tx-sender }))
            (effective-voter (if (and (is-some delegation) (get active (unwrap! delegation err-not-found)))
                (get delegate (unwrap! delegation err-not-found))
                tx-sender))
        )
        ;; Verify voting conditions
        (asserts! (is-eq (get status amendment) AMENDMENT_VOTING) err-amendment-closed)
        (asserts! (<= stacks-block-height (get voting-deadline amendment)) err-amendment-closed)
        (asserts! (is-some (index-of? (get parties contract) effective-voter)) err-unauthorized)
        (asserts! (is-none existing-vote) err-already-voted)
        
        ;; Record vote
        (map-set amendment-votes { amendment-id: amendment-id, voter: effective-voter }
            {
                vote: vote,
                timestamp: stacks-block-height,
                reasoning: reasoning
            }
        )
        
        ;; Update vote counts
        (let
            (
                (new-yes-votes (if vote 
                    (+ (get current-yes-votes amendment) u1)
                    (get current-yes-votes amendment)))
                (new-no-votes (if vote
                    (get current-no-votes amendment)
                    (+ (get current-no-votes amendment) u1)))
            )
            (map-set contract-amendments amendment-id
                (merge amendment {
                    current-yes-votes: new-yes-votes,
                    current-no-votes: new-no-votes
                })
            )
            (ok true)
        )
    )
)

;; Finalize amendment voting and determine outcome
(define-public (finalize-amendment-vote (amendment-id uint))
    (let
        (
            (amendment (unwrap! (map-get? contract-amendments amendment-id) err-not-found))
        )
        (asserts! (is-eq (get status amendment) AMENDMENT_VOTING) err-invalid-status)
        (asserts! (> stacks-block-height (get voting-deadline amendment)) err-amendment-closed)
        
        ;; Determine if amendment passed
        (let
            (
                (passed (>= (get current-yes-votes amendment) (get required-votes amendment)))
                (new-status (if passed AMENDMENT_APPROVED AMENDMENT_REJECTED))
            )
            (map-set contract-amendments amendment-id
                (merge amendment { status: new-status })
            )
            (ok passed)
        )
    )
)

;; Implement an approved amendment
(define-public (implement-amendment (amendment-id uint) (original-content (string-ascii 1000)) (new-content (string-ascii 1000)))
    (let
        (
            (amendment (unwrap! (map-get? contract-amendments amendment-id) err-not-found))
            (contract (unwrap! (map-get? contracts (get contract-id amendment)) err-not-found))
        )
        (asserts! (is-eq (get creator contract) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status amendment) AMENDMENT_APPROVED) err-invalid-status)
        
        ;; Record implementation
        (map-set amendment-implementations amendment-id {
            implemented-by: tx-sender,
            implementation-timestamp: stacks-block-height,
            original-content: original-content,
            new-content: new-content
        })
        
        ;; Update amendment status
        (map-set contract-amendments amendment-id
            (merge amendment { status: AMENDMENT_IMPLEMENTED })
        )
        
        (ok true)
    )
)

;; Delegate voting rights to another party
(define-public (delegate-vote (contract-id uint) (delegate principal))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) err-not-found))
        )
        (asserts! (is-some (index-of? (get parties contract) tx-sender)) err-unauthorized)
        (asserts! (is-some (index-of? (get parties contract) delegate)) err-unauthorized)
        (asserts! (not (is-eq tx-sender delegate)) err-invalid-status)
        
        (map-set vote-delegations { contract-id: contract-id, delegator: tx-sender }
            {
                delegate: delegate,
                active: true,
                created-at: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Revoke vote delegation
(define-public (revoke-vote-delegation (contract-id uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) err-not-found))
            (delegation (unwrap! (map-get? vote-delegations { contract-id: contract-id, delegator: tx-sender }) err-not-found))
        )
        (asserts! (is-some (index-of? (get parties contract) tx-sender)) err-unauthorized)
        (asserts! (get active delegation) err-invalid-status)
        
        (map-set vote-delegations { contract-id: contract-id, delegator: tx-sender }
            (merge delegation { active: false })
        )
        (ok true)
    )
)

;; Read-only functions

;; Get amendment details
(define-read-only (get-amendment (amendment-id uint))
    (ok (map-get? contract-amendments amendment-id))
)

;; Get vote details for specific voter
(define-read-only (get-vote (amendment-id uint) (voter principal))
    (ok (map-get? amendment-votes { amendment-id: amendment-id, voter: voter }))
)

;; Get vote delegation status
(define-read-only (get-vote-delegation (contract-id uint) (delegator principal))
    (ok (map-get? vote-delegations { contract-id: contract-id, delegator: delegator }))
)

;; Get amendment implementation details
(define-read-only (get-amendment-implementation (amendment-id uint))
    (ok (map-get? amendment-implementations amendment-id))
)

;; Check if amendment voting is still active
(define-read-only (is-voting-active (amendment-id uint))
    (match (map-get? contract-amendments amendment-id)
        amendment (ok (and 
            (is-eq (get status amendment) AMENDMENT_VOTING)
            (<= stacks-block-height (get voting-deadline amendment))
        ))
        (ok false)
    )
)

;; Get current voting results
(define-read-only (get-voting-results (amendment-id uint))
    (match (map-get? contract-amendments amendment-id)
        amendment (ok {
            yes-votes: (get current-yes-votes amendment),
            no-votes: (get current-no-votes amendment),
            required-votes: (get required-votes amendment),
            voting-deadline: (get voting-deadline amendment)
        })
        err-not-found
    )
)

;; Set voting period (only contract owner)
(define-public (set-voting-period (blocks uint))
    (begin
        (var-set voting-period-blocks blocks)
        (ok true)
    )
)

;; Get current voting period setting
(define-read-only (get-voting-period)
    (ok (var-get voting-period-blocks))
)
