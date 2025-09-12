;; Document Verification and Digital Notarization System
;; Provides blockchain-based document authenticity and notarization for legal contracts

;; Error codes
(define-constant err-not-found (err u201))
(define-constant err-unauthorized (err u202))
(define-constant err-already-notarized (err u203))
(define-constant err-invalid-hash (err u204))
(define-constant err-insufficient-fee (err u205))
(define-constant err-notary-not-authorized (err u206))
(define-constant err-document-tampered (err u207))

;; Notarization constants
(define-constant NOTARIZATION_FEE u100) ;; STX
(define-constant VERIFICATION_FEE u25) ;; STX
(define-constant MIN_NOTARY_STAKE u5000) ;; STX required to become notary

;; Document verification status
(define-constant STATUS_PENDING "pending")
(define-constant STATUS_VERIFIED "verified")
(define-constant STATUS_NOTARIZED "notarized")
(define-constant STATUS_TAMPERED "tampered")

;; Data variables
(define-data-var next-document-id uint u1)
(define-data-var next-notary-id uint u1)
(define-data-var contract-owner principal tx-sender)

;; Authorized digital notaries
(define-map authorized-notaries
    principal
    {
        notary-id: uint,
        certification-level: uint, ;; 1-5 scale
        stake-amount: uint,
        documents-notarized: uint,
        reputation-score: uint,
        authorized-date: uint,
        active: bool
    }
)

;; Document registry with cryptographic proofs
(define-map document-registry
    uint
    {
        contract-id: uint,
        document-hash: (buff 32),
        document-type: (string-ascii 50),
        uploaded-by: principal,
        notarized-by: (optional principal),
        verification-status: (string-ascii 20),
        timestamp: uint,
        notarization-timestamp: (optional uint),
        metadata: (string-ascii 200),
        integrity-proof: (buff 64)
    }
)

;; Document access permissions
(define-map document-permissions
    { document-id: uint, accessor: principal }
    {
        access-level: (string-ascii 20), ;; "read", "verify", "notarize"
        granted-by: principal,
        granted-at: uint,
        expires-at: (optional uint)
    }
)

;; Verification audit trail
(define-map verification-history
    { document-id: uint, verification-id: uint }
    {
        verified-by: principal,
        verification-method: (string-ascii 30),
        verification-result: bool,
        confidence-score: uint,
        timestamp: uint,
        notes: (string-ascii 200)
    }
)

;; Document verification counter
(define-map document-verification-count uint uint)

;; Notarization certificates
(define-map notarization-certificates
    uint ;; document-id
    {
        certificate-hash: (buff 32),
        notary-signature: (buff 65),
        witness-count: uint,
        certificate-metadata: (string-ascii 300),
        legal-jurisdiction: (string-ascii 50),
        validity-period: uint
    }
)

;; Register as authorized notary
(define-public (register-notary (certification-level uint))
    (let
        ((notary-id (var-get next-notary-id))
         (stake-amount MIN_NOTARY_STAKE))
        (asserts! (and (>= certification-level u1) (<= certification-level u5)) err-unauthorized)
        (try! (stx-transfer? stake-amount tx-sender (var-get contract-owner)))
        
        (map-set authorized-notaries tx-sender
            {
                notary-id: notary-id,
                certification-level: certification-level,
                stake-amount: stake-amount,
                documents-notarized: u0,
                reputation-score: u100,
                authorized-date: stacks-block-height,
                active: true
            }
        )
        (var-set next-notary-id (+ notary-id u1))
        (ok notary-id)
    )
)

;; Upload document for verification
(define-public (upload-document 
    (contract-id uint)
    (document-hash (buff 32))
    (document-type (string-ascii 50))
    (metadata (string-ascii 200)))
    (let
        ((document-id (var-get next-document-id))
         (integrity-proof (generate-integrity-proof document-hash tx-sender)))
        
        (try! (stx-transfer? VERIFICATION_FEE tx-sender (var-get contract-owner)))
        
        (map-set document-registry document-id
            {
                contract-id: contract-id,
                document-hash: document-hash,
                document-type: document-type,
                uploaded-by: tx-sender,
                notarized-by: none,
                verification-status: STATUS_PENDING,
                timestamp: stacks-block-height,
                notarization-timestamp: none,
                metadata: metadata,
                integrity-proof: integrity-proof
            }
        )
        
        (map-set document-verification-count document-id u0)
        (var-set next-document-id (+ document-id u1))
        (ok document-id)
    )
)

;; Verify document integrity
(define-public (verify-document (document-id uint) (provided-hash (buff 32)))
    (let
        ((document (unwrap! (map-get? document-registry document-id) err-not-found))
         (verification-count (default-to u0 (map-get? document-verification-count document-id)))
         (verification-id (+ verification-count u1)))
        
        (let
            ((hash-matches (is-eq provided-hash (get document-hash document)))
             (integrity-valid (verify-integrity-proof 
                 (get integrity-proof document) 
                 (get document-hash document) 
                 (get uploaded-by document)))
             (verification-result (and hash-matches integrity-valid))
             (confidence-score (if verification-result u100 u0)))
            
            (map-set verification-history { document-id: document-id, verification-id: verification-id }
                {
                    verified-by: tx-sender,
                    verification-method: "cryptographic-hash",
                    verification-result: verification-result,
                    confidence-score: confidence-score,
                    timestamp: stacks-block-height,
                    notes: (if verification-result "Document integrity confirmed" "Hash mismatch detected")
                }
            )
            
            (map-set document-verification-count document-id verification-id)
            
            ;; Update document status if tampered
            (if (not verification-result)
                (map-set document-registry document-id
                    (merge document { verification-status: STATUS_TAMPERED }))
                (map-set document-registry document-id
                    (merge document { verification-status: STATUS_VERIFIED }))
            )
            (ok verification-result)
        )
    )
)

;; Digital notarization by authorized notary
(define-public (notarize-document 
    (document-id uint) 
    (jurisdiction (string-ascii 50))
    (validity-period uint)
    (certificate-metadata (string-ascii 300)))
    (let
        ((document (unwrap! (map-get? document-registry document-id) err-not-found))
         (notary-info (unwrap! (map-get? authorized-notaries tx-sender) err-notary-not-authorized)))
        
        (asserts! (get active notary-info) err-notary-not-authorized)
        (asserts! (is-eq (get verification-status document) STATUS_VERIFIED) err-document-tampered)
        (asserts! (is-none (get notarized-by document)) err-already-notarized)
        
        (try! (stx-transfer? NOTARIZATION_FEE (get uploaded-by document) tx-sender))
        
        (let
            ((certificate-hash (generate-certificate-hash document-id tx-sender))
             (notary-signature (generate-notary-signature document-id tx-sender)))
            
            (map-set notarization-certificates document-id
                {
                    certificate-hash: certificate-hash,
                    notary-signature: notary-signature,
                    witness-count: u1,
                    certificate-metadata: certificate-metadata,
                    legal-jurisdiction: jurisdiction,
                    validity-period: validity-period
                }
            )
            
            (map-set document-registry document-id
                (merge document {
                    notarized-by: (some tx-sender),
                    verification-status: STATUS_NOTARIZED,
                    notarization-timestamp: (some stacks-block-height)
                })
            )
            
            ;; Update notary statistics
            (map-set authorized-notaries tx-sender
                (merge notary-info {
                    documents-notarized: (+ (get documents-notarized notary-info) u1)
                })
            )
            (ok true)
        )
    )
)

;; Grant document access permission
(define-public (grant-document-access 
    (document-id uint) 
    (accessor principal) 
    (access-level (string-ascii 20))
    (expires-blocks (optional uint)))
    (let
        ((document (unwrap! (map-get? document-registry document-id) err-not-found)))
        
        (asserts! (is-eq (get uploaded-by document) tx-sender) err-unauthorized)
        
        (map-set document-permissions { document-id: document-id, accessor: accessor }
            {
                access-level: access-level,
                granted-by: tx-sender,
                granted-at: stacks-block-height,
                expires-at: (match expires-blocks
                    some-blocks (some (+ stacks-block-height some-blocks))
                    none)
            }
        )
        (ok true)
    )
)

;; Batch verify multiple documents
(define-public (batch-verify-documents (document-ids (list 5 uint)))
    (let
        ((verification-results (map verify-single-document document-ids)))
        (ok verification-results)
    )
)

;; Generate document authenticity report
(define-public (generate-authenticity-report (document-id uint))
    (let
        ((document (unwrap! (map-get? document-registry document-id) err-not-found))
         (certificate (map-get? notarization-certificates document-id))
         (verification-count (default-to u0 (map-get? document-verification-count document-id))))
        
        (ok {
            document-id: document-id,
            verification-status: (get verification-status document),
            upload-timestamp: (get timestamp document),
            notarization-status: (is-some (get notarized-by document)),
            notary: (get notarized-by document),
            verification-count: verification-count,
            certificate-valid: (is-some certificate),
            authenticity-score: (calculate-authenticity-score document certificate)
        })
    )
)

;; Private helper functions
(define-private (generate-integrity-proof (doc-hash (buff 32)) (uploader principal))
    ;; Simplified proof generation - hash the document hash with block height
    (concat doc-hash (hash160 (unwrap-panic (to-consensus-buff? stacks-block-height)))))

(define-private (verify-integrity-proof (proof (buff 64)) (doc-hash (buff 32)) (uploader principal))
    ;; Simplified verification - would implement proper cryptographic verification
    (let
        ((expected-proof (generate-integrity-proof doc-hash uploader)))
        (is-eq proof expected-proof)
    )
)

(define-private (generate-certificate-hash (document-id uint) (notary principal))
    ;; Generate certificate hash using document ID
    (hash160 (unwrap-panic (to-consensus-buff? document-id))))

(define-private (generate-notary-signature (document-id uint) (notary principal))
    ;; Generate notary signature - simplified implementation
    (concat (generate-certificate-hash document-id notary) 
            (hash160 (unwrap-panic (to-consensus-buff? stacks-block-height))))
)

(define-private (verify-single-document (document-id uint))
    (match (map-get? document-registry document-id)
        document (is-eq (get verification-status document) STATUS_VERIFIED)
        false
    )
)

(define-private (calculate-authenticity-score (document { contract-id: uint, document-hash: (buff 32), document-type: (string-ascii 50), uploaded-by: principal, notarized-by: (optional principal), verification-status: (string-ascii 20), timestamp: uint, notarization-timestamp: (optional uint), metadata: (string-ascii 200), integrity-proof: (buff 64) }) (certificate (optional { certificate-hash: (buff 32), notary-signature: (buff 65), witness-count: uint, certificate-metadata: (string-ascii 300), legal-jurisdiction: (string-ascii 50), validity-period: uint })))
    (let
        ((base-score (if (is-eq (get verification-status document) STATUS_NOTARIZED) u80 u40))
         (notary-bonus (if (is-some (get notarized-by document)) u15 u0))
         (certificate-bonus (if (is-some certificate) u5 u0)))
        (+ base-score notary-bonus certificate-bonus)
    )
)

;; Read-only functions
(define-read-only (get-document-info (document-id uint))
    (ok (map-get? document-registry document-id))
)

(define-read-only (get-notary-info (notary principal))
    (ok (map-get? authorized-notaries notary))
)

(define-read-only (get-document-certificate (document-id uint))
    (ok (map-get? notarization-certificates document-id))
)

(define-read-only (get-verification-history (document-id uint) (verification-id uint))
    (ok (map-get? verification-history { document-id: document-id, verification-id: verification-id }))
)

(define-read-only (has-document-access (document-id uint) (accessor principal))
    (match (map-get? document-permissions { document-id: document-id, accessor: accessor })
        permission (ok (match (get expires-at permission)
            some-expiry (< stacks-block-height some-expiry)
            true))
        (ok false)
    )
)

(define-read-only (is-document-authentic (document-id uint))
    (match (map-get? document-registry document-id)
        document (ok (not (is-eq (get verification-status document) STATUS_TAMPERED)))
        (ok false)
    )
)

(define-read-only (get-notarization-stats (notary principal))
    (match (map-get? authorized-notaries notary)
        info (ok {
            documents-notarized: (get documents-notarized info),
            reputation-score: (get reputation-score info),
            certification-level: (get certification-level info)
        })
        err-not-found
    )
)
