(define-constant contract-owner tx-sender)
(define-constant min-investment u1000000)
(define-constant max-projects u100)
(define-constant funding-period u144)
(define-constant success-threshold u800000000)

(define-data-var total-projects uint u0)

(define-map Projects uint {
    owner: principal,
    title: (string-ascii 50),
    target-amount: uint,
    current-amount: uint,
    status: (string-ascii 20),
    end-block: uint,
    investor-count: uint
})

(define-map Investments { project-id: uint, investor: principal } {
    amount: uint,
    stacks-block-height: uint
})

(define-map InvestorTotalAmount principal uint)

(define-read-only (get-project (project-id uint))
    (map-get? Projects project-id)
)

(define-read-only (get-investment (project-id uint) (investor principal))
    (map-get? Investments { project-id: project-id, investor: investor })
)

(define-read-only (get-investor-total (investor principal))
    (default-to u0 (map-get? InvestorTotalAmount investor))
)

(define-public (create-project (title (string-ascii 50)) (target-amount uint))
    (let ((project-id (var-get total-projects)))
        (asserts! (< project-id max-projects) (err u1))
        (asserts! (> target-amount min-investment) (err u2))
        (asserts! (is-eq tx-sender contract-owner) (err u3))
        
        (map-set Projects project-id {
            owner: tx-sender,
            title: title,
            target-amount: target-amount,
            current-amount: u0,
            status: "active",
            end-block: (+ stacks-block-height funding-period),
            investor-count: u0
        })
        
        (var-set total-projects (+ project-id u1))
        (ok project-id)
    )
)

(define-public (invest (project-id uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u4)))
        (amount (stx-get-balance tx-sender))
        (current-total (default-to u0 (map-get? InvestorTotalAmount tx-sender)))
    )
        (asserts! (is-eq (get status project) "active") (err u5))
        (asserts! (>= amount min-investment) (err u6))
        (asserts! (<= (+ (get current-amount project) amount) (get target-amount project)) (err u7))
        
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-set Projects project-id 
            (merge project {
                current-amount: (+ (get current-amount project) amount),
                investor-count: (+ (get investor-count project) u1)
            })
        )
        
        (map-set Investments { project-id: project-id, investor: tx-sender }
            { amount: amount, stacks-block-height: stacks-block-height }
        )
        
        (map-set InvestorTotalAmount tx-sender (+ current-total amount))
        (ok true)
    )
)

(define-public (finalize-project (project-id uint))
    (let ((project (unwrap! (map-get? Projects project-id) (err u8))))
        (asserts! (is-eq tx-sender contract-owner) (err u9))
        (asserts! (>= stacks-block-height (get end-block project)) (err u10))
        
        (if (>= (get current-amount project) (get target-amount project))
            (begin
                (try! (as-contract (stx-transfer? (get current-amount project) tx-sender (get owner project))))
                (map-set Projects project-id (merge project { status: "completed" }))
                (ok true)
            )
            (begin
                (map-set Projects project-id (merge project { status: "failed" }))
                (ok false)
            )
        )
    )
)

(define-public (claim-refund (project-id uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u11)))
        (investment (unwrap! (map-get? Investments { project-id: project-id, investor: tx-sender }) (err u12)))
    )
        (asserts! (is-eq (get status project) "failed") (err u13))
        (try! (as-contract (stx-transfer? (get amount investment) tx-sender tx-sender)))
        
        (map-delete Investments { project-id: project-id, investor: tx-sender })
        (ok true)
    )
)



(define-map ProjectUpdates { project-id: uint, update-id: uint } {
    message: (string-ascii 280),
    timestamp: uint
})

(define-map ProjectUpdateCount uint uint)

(define-read-only (get-project-update (project-id uint) (update-id uint))
    (map-get? ProjectUpdates { project-id: project-id, update-id: update-id })
)

(define-public (add-project-update (project-id uint) (message (string-ascii 280)))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u14)))
        (update-count (default-to u0 (map-get? ProjectUpdateCount project-id)))
    )
        (asserts! (is-eq tx-sender (get owner project)) (err u15))
        
        (map-set ProjectUpdates 
            { project-id: project-id, update-id: update-count }
            { message: message, timestamp: stacks-block-height }
        )
        (map-set ProjectUpdateCount project-id (+ update-count u1))
        (ok true)
    )
)


(define-constant categories (list 
    "technology"
    "art"
    "social"
    "environment"
    "business"
))

(define-map ProjectCategories uint (string-ascii 20))

(define-map CategoryProjects { category: (string-ascii 20) } (list 50 uint))

(define-public (set-project-category (project-id uint) (category (string-ascii 11)))
    (let ((project (unwrap! (map-get? Projects project-id) (err u16))))
        (asserts! (is-eq tx-sender (get owner project)) (err u17))
        (asserts! (is-some (index-of categories category)) (err u18))
        
        (map-set ProjectCategories project-id category)
        (map-set CategoryProjects 
            { category: category }
            (unwrap-panic (as-max-len? 
                (append (default-to (list) (map-get? CategoryProjects { category: category })) project-id)
                u50
            ))
        )
        (ok true)
    )
)

(define-read-only (get-projects-by-category (category (string-ascii 20)))
    (default-to (list) (map-get? CategoryProjects { category: category }))
)


(define-map ProjectMilestones { project-id: uint, milestone-id: uint } {
    title: (string-ascii 100),
    description: (string-ascii 500),
    funding-amount: uint,
    status: (string-ascii 20),
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint,
    created-at: uint
})

(define-map ProjectMilestoneCount uint uint)

(define-map MilestoneVotes { project-id: uint, milestone-id: uint, voter: principal } {
    vote: bool,
    voting-power: uint
})

(define-map ProjectMilestoneSettings uint {
    total-milestones: uint,
    approval-threshold: uint,
    voting-period: uint
})

(define-constant milestone-voting-period u1008)
(define-constant default-approval-threshold u51)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
    (map-get? ProjectMilestones { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-milestone-count (project-id uint))
    (default-to u0 (map-get? ProjectMilestoneCount project-id))
)

(define-read-only (get-milestone-vote (project-id uint) (milestone-id uint) (voter principal))
    (map-get? MilestoneVotes { project-id: project-id, milestone-id: milestone-id, voter: voter })
)

(define-public (setup-milestone-funding (project-id uint) (approval-threshold uint))
    (let ((project (unwrap! (map-get? Projects project-id) (err u19))))
        (asserts! (is-eq tx-sender (get owner project)) (err u20))
        (asserts! (is-eq (get status project) "active") (err u21))
        (asserts! (and (>= approval-threshold u1) (<= approval-threshold u100)) (err u22))
        
        (map-set ProjectMilestoneSettings project-id {
            total-milestones: u0,
            approval-threshold: approval-threshold,
            voting-period: milestone-voting-period
        })
        (ok true)
    )
)

(define-public (create-milestone (project-id uint) (title (string-ascii 100)) (description (string-ascii 500)) (funding-amount uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u23)))
        (milestone-count (default-to u0 (map-get? ProjectMilestoneCount project-id)))
        (settings (unwrap! (map-get? ProjectMilestoneSettings project-id) (err u24)))
    )
        (asserts! (is-eq tx-sender (get owner project)) (err u25))
        (asserts! (is-eq (get status project) "active") (err u26))
        (asserts! (> funding-amount u0) (err u27))
        (asserts! (<= funding-amount (get current-amount project)) (err u28))
        
        (map-set ProjectMilestones 
            { project-id: project-id, milestone-id: milestone-count }
            {
                title: title,
                description: description,
                funding-amount: funding-amount,
                status: "pending",
                votes-for: u0,
                votes-against: u0,
                voting-deadline: (+ stacks-block-height (get voting-period settings)),
                created-at: stacks-block-height
            }
        )
        
        (map-set ProjectMilestoneCount project-id (+ milestone-count u1))
        (map-set ProjectMilestoneSettings project-id 
            (merge settings { total-milestones: (+ milestone-count u1) })
        )
        (ok milestone-count)
    )
)

(define-public (vote-on-milestone (project-id uint) (milestone-id uint) (vote bool))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u29)))
        (milestone (unwrap! (map-get? ProjectMilestones { project-id: project-id, milestone-id: milestone-id }) (err u30)))
        (investment (unwrap! (map-get? Investments { project-id: project-id, investor: tx-sender }) (err u31)))
        (existing-vote (map-get? MilestoneVotes { project-id: project-id, milestone-id: milestone-id, voter: tx-sender }))
        (voting-power (get amount investment))
    )
        (asserts! (is-eq (get status milestone) "pending") (err u32))
        (asserts! (< stacks-block-height (get voting-deadline milestone)) (err u33))
        (asserts! (is-none existing-vote) (err u34))
        
        (map-set MilestoneVotes 
            { project-id: project-id, milestone-id: milestone-id, voter: tx-sender }
            { vote: vote, voting-power: voting-power }
        )
        
        (map-set ProjectMilestones 
            { project-id: project-id, milestone-id: milestone-id }
            (merge milestone {
                votes-for: (if vote (+ (get votes-for milestone) voting-power) (get votes-for milestone)),
                votes-against: (if vote (get votes-against milestone) (+ (get votes-against milestone) voting-power))
            })
        )
        (ok true)
    )
)

(define-public (finalize-milestone (project-id uint) (milestone-id uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u35)))
        (milestone (unwrap! (map-get? ProjectMilestones { project-id: project-id, milestone-id: milestone-id }) (err u36)))
        (settings (unwrap! (map-get? ProjectMilestoneSettings project-id) (err u37)))
        (total-votes (+ (get votes-for milestone) (get votes-against milestone)))
        (approval-rate (if (> total-votes u0) (/ (* (get votes-for milestone) u100) total-votes) u0))
    )
        (asserts! (>= stacks-block-height (get voting-deadline milestone)) (err u38))
        (asserts! (is-eq (get status milestone) "pending") (err u39))
        
        (if (>= approval-rate (get approval-threshold settings))
            (begin
                (try! (as-contract (stx-transfer? (get funding-amount milestone) tx-sender (get owner project))))
                (map-set ProjectMilestones 
                    { project-id: project-id, milestone-id: milestone-id }
                    (merge milestone { status: "approved" })
                )
                (ok true)
            )
            (begin
                (map-set ProjectMilestones 
                    { project-id: project-id, milestone-id: milestone-id }
                    (merge milestone { status: "rejected" })
                )
                (ok false)
            )
        )
    )
)

(define-read-only (get-milestone-approval-rate (project-id uint) (milestone-id uint))
    (let (
        (milestone (unwrap! (map-get? ProjectMilestones { project-id: project-id, milestone-id: milestone-id }) (err u40)))
        (total-votes (+ (get votes-for milestone) (get votes-against milestone)))
    )
        (if (> total-votes u0)
            (ok (/ (* (get votes-for milestone) u100) total-votes))
            (ok u0)
        )
    )
)

(define-read-only (get-project-milestone-summary (project-id uint))
    (let ((settings (map-get? ProjectMilestoneSettings project-id)))
        (if (is-some settings)
            (ok {
                total-milestones: (get total-milestones (unwrap-panic settings)),
                approval-threshold: (get approval-threshold (unwrap-panic settings)),
                voting-period: (get voting-period (unwrap-panic settings))
            })
            (err u41)
        )
    )
)