;; Contract Analytics Extension
;; This contract extends the Legal Contract Manager with performance tracking

;; Import constants and error codes
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-status (err u104))
(define-constant STATUS_ACTIVE "active")
(define-constant STATUS_COMPLETED "completed")

;; Reference to main contract maps (these would need to be imported in practice)
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

(define-map contract-analytics
    uint
    {
        completion-time: uint,
        success-rate: uint,
        milestone-completion-rate: uint,
        total-disputes: uint,
        average-approval-time: uint
    }
)

(define-map party-performance
    principal
    {
        contracts-completed: uint,
        contracts-disputed: uint,
        average-completion-time: uint,
        reliability-score: uint,
        total-contracts-participated: uint
    }
)

(define-map firm-analytics
    principal
    {
        total-contracts-created: uint,
        successful-contracts: uint,
        average-contract-duration: uint,
        client-satisfaction-score: uint,
        total-revenue-generated: uint
    }
)

(define-map contract-completion-tracking
    uint
    {
        start-time: uint,
        end-time: uint,
        total-milestones: uint,
        completed-milestones: uint,
        disputed: bool
    }
)

(define-data-var total-platform-contracts uint u0)
(define-data-var total-successful-contracts uint u0)
(define-data-var platform-success-rate uint u0)

(define-public (record-contract-completion (contract-id uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) err-not-found))
            (tracking (unwrap! (map-get? contract-completion-tracking contract-id) err-not-found))
            (completion-time (- stacks-block-height (get start-time tracking)))
            (milestone-rate (if (> (get total-milestones tracking) u0)
                (/ (* (get completed-milestones tracking) u100) (get total-milestones tracking))
                u100))
            (success-rate (if (get disputed tracking) u0 u100))
        )
        (asserts! (is-eq (get creator contract) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status contract) STATUS_COMPLETED) err-invalid-status)
        
        (map-set contract-analytics contract-id {
            completion-time: completion-time,
            success-rate: success-rate,
            milestone-completion-rate: milestone-rate,
            total-disputes: (if (get disputed tracking) u1 u0),
            average-approval-time: u0
        })
        
        (update-firm-analytics (get creator contract) success-rate completion-time)
        (update-platform-stats success-rate)
        
        (ok true)
    )
)

(define-public (initialize-contract-tracking (contract-id uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) err-not-found))
        )
        (asserts! (is-eq (get creator contract) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status contract) STATUS_ACTIVE) err-invalid-status)
        
        (map-set contract-completion-tracking contract-id {
            start-time: stacks-block-height,
            end-time: u0,
            total-milestones: u0,
            completed-milestones: u0,
            disputed: false
        })
        
        (ok true)
    )
)

(define-public (update-milestone-tracking (contract-id uint) (milestone-completed bool))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) err-not-found))
            (tracking (unwrap! (map-get? contract-completion-tracking contract-id) err-not-found))
        )
        (asserts! (is-some (index-of? (get parties contract) tx-sender)) err-unauthorized)
        
        (map-set contract-completion-tracking contract-id
            (merge tracking {
                total-milestones: (+ (get total-milestones tracking) u1),
                completed-milestones: (if milestone-completed 
                    (+ (get completed-milestones tracking) u1)
                    (get completed-milestones tracking))
            })
        )
        
        (ok true)
    )
)

(define-public (mark-contract-disputed (contract-id uint))
    (let
        (
            (contract (unwrap! (map-get? contracts contract-id) err-not-found))
            (tracking (unwrap! (map-get? contract-completion-tracking contract-id) err-not-found))
        )
        (asserts! (is-some (index-of? (get parties contract) tx-sender)) err-unauthorized)
        
        (map-set contract-completion-tracking contract-id
            (merge tracking { disputed: true })
        )
        
        (ok true)
    )
)



(define-private (update-single-party-performance-fold (data { success-rate: uint, completion-time: uint }) (party principal))
    (let
        (
            (current-perf (default-to { 
                contracts-completed: u0,
                contracts-disputed: u0,
                average-completion-time: u0,
                reliability-score: u100,
                total-contracts-participated: u0
            } (map-get? party-performance party)))
            (new-total (+ (get total-contracts-participated current-perf) u1))
            (new-completed (+ (get contracts-completed current-perf) (if (> (get success-rate data) u0) u1 u0)))
            (new-disputed (+ (get contracts-disputed current-perf) (if (is-eq (get success-rate data) u0) u1 u0)))
            (new-avg-time (/ (+ (* (get average-completion-time current-perf) (get total-contracts-participated current-perf)) (get completion-time data)) new-total))
            (new-reliability (/ (* new-completed u100) new-total))
        )
        (begin
            (map-set party-performance party {
                contracts-completed: new-completed,
                contracts-disputed: new-disputed,
                average-completion-time: new-avg-time,
                reliability-score: new-reliability,
                total-contracts-participated: new-total
            })
            data
        )
    )
)

(define-private (update-firm-analytics (firm principal) (success-rate uint) (completion-time uint))
    (let
        (
            (current-analytics (default-to {
                total-contracts-created: u0,
                successful-contracts: u0,
                average-contract-duration: u0,
                client-satisfaction-score: u100,
                total-revenue-generated: u0
            } (map-get? firm-analytics firm)))
            (new-total (+ (get total-contracts-created current-analytics) u1))
            (new-successful (+ (get successful-contracts current-analytics) (if (> success-rate u0) u1 u0)))
            (new-avg-duration (/ (+ (* (get average-contract-duration current-analytics) (get total-contracts-created current-analytics)) completion-time) new-total))
            (new-satisfaction (/ (* new-successful u100) new-total))
        )
        (map-set firm-analytics firm {
            total-contracts-created: new-total,
            successful-contracts: new-successful,
            average-contract-duration: new-avg-duration,
            client-satisfaction-score: new-satisfaction,
            total-revenue-generated: (get total-revenue-generated current-analytics)
        })
    )
)

(define-private (update-platform-stats (success-rate uint))
    (let
        (
            (current-total (var-get total-platform-contracts))
            (current-successful (var-get total-successful-contracts))
            (new-total (+ current-total u1))
            (new-successful (+ current-successful (if (> success-rate u0) u1 u0)))
            (new-success-rate (/ (* new-successful u100) new-total))
        )
        (var-set total-platform-contracts new-total)
        (var-set total-successful-contracts new-successful)
        (var-set platform-success-rate new-success-rate)
    )
)

(define-read-only (get-contract-analytics (contract-id uint))
    (ok (map-get? contract-analytics contract-id))
)

(define-read-only (get-party-performance (party principal))
    (ok (map-get? party-performance party))
)

(define-read-only (get-firm-analytics (firm principal))
    (ok (map-get? firm-analytics firm))
)

(define-read-only (get-platform-stats)
    (ok {
        total-contracts: (var-get total-platform-contracts),
        successful-contracts: (var-get total-successful-contracts),
        success-rate: (var-get platform-success-rate)
    })
)

(define-read-only (get-top-performing-parties (minimum-contracts uint))
    (ok "analytics-query-function")
)

(define-read-only (get-contract-tracking (contract-id uint))
    (ok (map-get? contract-completion-tracking contract-id))
)

(define-read-only (calculate-party-reliability (party principal))
    (match (map-get? party-performance party)
        perf-data (ok (get reliability-score perf-data))
        (ok u0)
    )
)
