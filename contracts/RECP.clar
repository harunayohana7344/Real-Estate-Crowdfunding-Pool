(define-constant contract-owner tx-sender)
(define-constant min-investment u1000000)
(define-constant max-projects u100)
(define-constant funding-period u144)
(define-constant success-threshold u800000000)

(define-data-var total-projects uint u0)


(define-map ProjectOwnerReputation principal {
    total-projects: uint,
    successful-projects: uint,
    failed-projects: uint,
    total-funds-raised: uint,
    average-completion-time: uint,
    reputation-score: uint,
    last-updated: uint
})

(define-map InvestorPerformance principal {
    total-investments: uint,
    successful-investments: uint,
    failed-investments: uint,
    total-invested-amount: uint,
    total-returns: uint,
    average-roi: uint,
    investment-streak: uint,
    last-investment-block: uint
})

(define-map ProjectAnalytics uint {
    funding-velocity: uint,
    milestone-completion-rate: uint,
    investor-satisfaction: uint,
    project-risk-score: uint,
    funding-timeline: uint,
    community-engagement: uint
})

(define-map ProjectReturns uint {
    total-returned: uint,
    return-rate: uint,
    distribution-count: uint,
    last-distribution: uint
})

(define-map InvestorReturns { project-id: uint, investor: principal } {
    total-returned: uint,
    return-percentage: uint,
    distribution-history: (list 10 uint)
})

(define-map GlobalAnalytics (string-ascii 20) uint)

(define-constant reputation-multiplier u100)
(define-constant min-reputation-score u0)
(define-constant max-reputation-score u1000)
(define-constant roi-calculation-base u10000)

(define-read-only (get-owner-reputation (owner principal))
    (default-to 
        {
            total-projects: u0,
            successful-projects: u0,
            failed-projects: u0,
            total-funds-raised: u0,
            average-completion-time: u0,
            reputation-score: u500,
            last-updated: u0
        }
        (map-get? ProjectOwnerReputation owner)
    )
)

(define-read-only (get-investor-performance (investor principal))
    (default-to 
        {
            total-investments: u0,
            successful-investments: u0,
            failed-investments: u0,
            total-invested-amount: u0,
            total-returns: u0,
            average-roi: u0,
            investment-streak: u0,
            last-investment-block: u0
        }
        (map-get? InvestorPerformance investor)
    )
)

(define-read-only (get-project-analytics (project-id uint))
    (default-to 
        {
            funding-velocity: u0,
            milestone-completion-rate: u0,
            investor-satisfaction: u0,
            project-risk-score: u50,
            funding-timeline: u0,
            community-engagement: u0
        }
        (map-get? ProjectAnalytics project-id)
    )
)

(define-read-only (get-project-returns (project-id uint))
    (default-to 
        {
            total-returned: u0,
            return-rate: u0,
            distribution-count: u0,
            last-distribution: u0
        }
        (map-get? ProjectReturns project-id)
    )
)

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


(define-read-only (get-investor-returns (project-id uint) (investor principal))
    (default-to 
        {
            total-returned: u0,
            return-percentage: u0,
            distribution-history: (list)
        }
        (map-get? InvestorReturns { project-id: project-id, investor: investor })
    )
)

(define-read-only (get-global-analytics (metric (string-ascii 20)))
    (default-to u0 (map-get? GlobalAnalytics metric))
)

(define-private (calculate-reputation-score (owner principal))
    (let (
        (reputation (get-owner-reputation owner))
        (success-rate (if (> (get total-projects reputation) u0)
            (/ (* (get successful-projects reputation) u100) (get total-projects reputation))
            u0))
        (volume-calc (/ (get total-funds-raised reputation) u1000000))
        (volume-bonus (if (< volume-calc u200) volume-calc u200))
        (streak-bonus (if (> (get successful-projects reputation) u3) u100 u0))
        (base-score (+ (* success-rate u5) volume-bonus streak-bonus))
        (clamped-score (if (< base-score min-reputation-score) min-reputation-score base-score))
    )
        (if (> clamped-score max-reputation-score) max-reputation-score clamped-score)
    )
)

(define-private (calculate-project-risk-score (project-id uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) u50))
        (owner-reputation (get-owner-reputation (get owner project)))
        (funding-ratio (if (> (get target-amount project) u0)
            (/ (* (get current-amount project) u100) (get target-amount project))
            u0))
        (investor-diversity (if (< (get investor-count project) u20) (get investor-count project) u20))
        (reputation-factor (/ (get reputation-score owner-reputation) u20))
        (risk-score (- u100 (/ (+ funding-ratio investor-diversity reputation-factor) u3)))
        (clamped-risk (if (< risk-score u0) u0 risk-score))
    )
        (if (> clamped-risk u100) u100 clamped-risk)
    )
)

(define-private (update-global-analytics (metric (string-ascii 20)) (value uint))
    (map-set GlobalAnalytics metric value)
)

(define-public (update-project-analytics (project-id uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u42)))
        (milestone-count (get-milestone-count project-id))
        (current-analytics (get-project-analytics project-id))
        (funding-velocity (if (> (get current-amount project) u0)
            (/ (get current-amount project) (- stacks-block-height (get stacks-block-height (unwrap-panic (get-investment project-id (get owner project))))))
            u0))
        (risk-score (calculate-project-risk-score project-id))
    )
        (map-set ProjectAnalytics project-id {
            funding-velocity: funding-velocity,
            milestone-completion-rate: (get milestone-completion-rate current-analytics),
            investor-satisfaction: (get investor-satisfaction current-analytics),
            project-risk-score: risk-score,
            funding-timeline: (- stacks-block-height (get stacks-block-height (unwrap-panic (get-investment project-id (get owner project))))),
            community-engagement: (get community-engagement current-analytics)
        })
        (ok true)
    )
)

(define-public (update-owner-reputation (owner principal) (project-id uint) (success bool))
    (let (
        (current-reputation (get-owner-reputation owner))
        (project (unwrap! (map-get? Projects project-id) (err u43)))
        (new-total-projects (+ (get total-projects current-reputation) u1))
        (new-successful (if success (+ (get successful-projects current-reputation) u1) (get successful-projects current-reputation)))
        (new-failed (if success (get failed-projects current-reputation) (+ (get failed-projects current-reputation) u1)))
        (new-funds-raised (+ (get total-funds-raised current-reputation) (get current-amount project)))
        (new-reputation-score (calculate-reputation-score owner))
    )
        (map-set ProjectOwnerReputation owner {
            total-projects: new-total-projects,
            successful-projects: new-successful,
            failed-projects: new-failed,
            total-funds-raised: new-funds-raised,
            average-completion-time: (get average-completion-time current-reputation),
            reputation-score: new-reputation-score,
            last-updated: stacks-block-height
        })
        (ok true)
    )
)

(define-public (update-investor-performance (investor principal) (project-id uint) (success bool))
    (let (
        (current-performance (get-investor-performance investor))
        (investment (unwrap! (map-get? Investments { project-id: project-id, investor: investor }) (err u44)))
        (new-total-investments (+ (get total-investments current-performance) u1))
        (new-successful (if success (+ (get successful-investments current-performance) u1) (get successful-investments current-performance)))
        (new-failed (if success (get failed-investments current-performance) (+ (get failed-investments current-performance) u1)))
        (new-streak (if success (+ (get investment-streak current-performance) u1) u0))
    )
        (map-set InvestorPerformance investor {
            total-investments: new-total-investments,
            successful-investments: new-successful,
            failed-investments: new-failed,
            total-invested-amount: (+ (get total-invested-amount current-performance) (get amount investment)),
            total-returns: (get total-returns current-performance),
            average-roi: (get average-roi current-performance),
            investment-streak: new-streak,
            last-investment-block: stacks-block-height
        })
        (ok true)
    )
)

(define-public (distribute-returns (project-id uint) (return-amount uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u45)))
        (current-returns (get-project-returns project-id))
    )
        (asserts! (is-eq tx-sender (get owner project)) (err u46))
        (asserts! (is-eq (get status project) "completed") (err u47))
        (asserts! (> return-amount u0) (err u48))
        
        (map-set ProjectReturns project-id {
            total-returned: (+ (get total-returned current-returns) return-amount),
            return-rate: (if (> (get current-amount project) u0)
                (/ (* (+ (get total-returned current-returns) return-amount) u100) (get current-amount project))
                u0),
            distribution-count: (+ (get distribution-count current-returns) u1),
            last-distribution: stacks-block-height
        })
        (ok true)
    )
)

(define-public (claim-investor-returns (project-id uint) (return-amount uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u49)))
        (investment (unwrap! (map-get? Investments { project-id: project-id, investor: tx-sender }) (err u50)))
        (current-returns (get-investor-returns project-id tx-sender))
        (project-returns (get-project-returns project-id))
        (investor-share (/ (* return-amount (get amount investment)) (get current-amount project)))
        (current-performance (get-investor-performance tx-sender))
    )
        (asserts! (is-eq (get status project) "completed") (err u51))
        (asserts! (> return-amount u0) (err u52))
        (asserts! (> (get total-returned project-returns) u0) (err u53))
        
        (try! (as-contract (stx-transfer? investor-share tx-sender tx-sender)))
        
        (map-set InvestorReturns { project-id: project-id, investor: tx-sender } {
            total-returned: (+ (get total-returned current-returns) investor-share),
            return-percentage: (if (> (get amount investment) u0)
                (/ (* (+ (get total-returned current-returns) investor-share) u100) (get amount investment))
                u0),
            distribution-history: (unwrap-panic (as-max-len? 
                (append (get distribution-history current-returns) investor-share)
                u10))
        })
        
        (let (
            (new-total-returns (+ (get total-returns current-performance) investor-share))
            (new-avg-roi (if (> (get total-invested-amount current-performance) u0)
                (/ (* new-total-returns u100) (get total-invested-amount current-performance))
                u0))
        )
            (map-set InvestorPerformance tx-sender (merge current-performance {
                total-returns: new-total-returns,
                average-roi: new-avg-roi
            }))
        )
        (ok true)
    )
)

(define-read-only (get-top-performing-owners (limit uint))
    (ok (list))
)

(define-read-only (get-project-performance-summary (project-id uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u54)))
        (analytics (get-project-analytics project-id))
        (returns (get-project-returns project-id))
        (owner-reputation (get-owner-reputation (get owner project)))
    )
        (ok {
            project-status: (get status project),
            funding-completion: (if (> (get target-amount project) u0)
                (/ (* (get current-amount project) u100) (get target-amount project))
                u0),
            owner-reputation-score: (get reputation-score owner-reputation),
            risk-score: (get project-risk-score analytics),
            return-rate: (get return-rate returns),
            investor-count: (get investor-count project),
            funding-velocity: (get funding-velocity analytics)
        })
    )
)

(define-read-only (calculate-investment-recommendation (project-id uint) (investor principal))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u55)))
        (analytics (get-project-analytics project-id))
        (owner-reputation (get-owner-reputation (get owner project)))
        (investor-performance (get-investor-performance investor))
        (risk-score (get project-risk-score analytics))
        (reputation-score (get reputation-score owner-reputation))
        (recommendation-score (- (+ reputation-score (- u100 risk-score)) u50))
    )
        (let (
            (clamped-recommendation (if (< recommendation-score u0) u0 recommendation-score))
            (final-recommendation (if (> clamped-recommendation u100) u100 clamped-recommendation))
        )
            (ok {
                recommendation-score: final-recommendation,
                risk-level: (if (< risk-score u30) "low" (if (< risk-score u70) "medium" "high")),
                owner-track-record: (if (> reputation-score u700) "excellent" (if (> reputation-score u400) "good" "average")),
                suggested-investment: (if (> final-recommendation u70) "recommended" (if (> final-recommendation u40) "consider" "avoid"))
            })
        )
    )
)

;; Insurance Pool System - Protects investors against project failures
(define-data-var insurance-pool-balance uint u0)
(define-data-var total-claims-paid uint u0)
(define-data-var insurance-fee-rate uint u300) ;; 3% of successful project funds
(define-data-var max-coverage-rate uint u7000) ;; 70% max coverage of investment
(define-data-var claim-processing-period uint u1008) ;; Blocks to wait before processing claims

(define-map InsuranceClaims uint {
    project-id: uint,
    claimant: principal,
    claim-amount: uint,
    coverage-amount: uint,
    claim-status: (string-ascii 20),
    claim-submitted: uint,
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint
})

(define-map InsuranceClaimCount principal uint)
(define-map ClaimVotes { claim-id: uint, voter: principal } bool)
(define-map ProjectInsuranceCoverage uint {
    total-covered: uint,
    coverage-rate: uint,
    premium-paid: uint,
    eligible-for-claims: bool
})

(define-map InvestorInsurance { project-id: uint, investor: principal } {
    insured-amount: uint,
    premium-rate: uint,
    coverage-start: uint,
    claim-eligible: bool
})

(define-data-var next-claim-id uint u0)

(define-constant insurance-voting-period u504) ;; 3.5 days
(define-constant min-coverage-threshold u5000000) ;; Minimum project size for insurance
(define-constant claim-approval-threshold u51) ;; 51% approval needed

(define-read-only (get-insurance-pool-balance)
    (var-get insurance-pool-balance)
)

(define-read-only (get-insurance-claim (claim-id uint))
    (map-get? InsuranceClaims claim-id)
)

(define-read-only (get-project-insurance-coverage (project-id uint))
    (default-to 
        {
            total-covered: u0,
            coverage-rate: u0,
            premium-paid: u0,
            eligible-for-claims: false
        }
        (map-get? ProjectInsuranceCoverage project-id)
    )
)

(define-read-only (get-investor-insurance (project-id uint) (investor principal))
    (default-to 
        {
            insured-amount: u0,
            premium-rate: u0,
            coverage-start: u0,
            claim-eligible: false
        }
        (map-get? InvestorInsurance { project-id: project-id, investor: investor })
    )
)

(define-read-only (calculate-insurance-premium (investment-amount uint) (project-risk-score uint))
    (let (
        (base-premium (/ (* investment-amount u200) u10000)) ;; 2% base premium
        (risk-multiplier (if (< project-risk-score u30) u50 
                           (if (< project-risk-score u70) u100 u150)))
        (adjusted-premium (/ (* base-premium risk-multiplier) u100))
    )
        adjusted-premium
    )
)

(define-public (purchase-investment-insurance (project-id uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u56)))
        (investment (unwrap! (map-get? Investments { project-id: project-id, investor: tx-sender }) (err u57)))
        (analytics (get-project-analytics project-id))
        (risk-score (get project-risk-score analytics))
        (premium-amount (calculate-insurance-premium (get amount investment) risk-score))
        (coverage-amount (/ (* (get amount investment) (var-get max-coverage-rate)) u10000))
    )
        (asserts! (is-eq (get status project) "active") (err u58))
        (asserts! (>= (get target-amount project) min-coverage-threshold) (err u59))
        (asserts! (>= (stx-get-balance tx-sender) premium-amount) (err u60))
        
        (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
        (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) premium-amount))
        
        (map-set InvestorInsurance { project-id: project-id, investor: tx-sender } {
            insured-amount: coverage-amount,
            premium-rate: (/ (* premium-amount u10000) (get amount investment)),
            coverage-start: stacks-block-height,
            claim-eligible: true
        })
        
        (let ((current-coverage (get-project-insurance-coverage project-id)))
            (map-set ProjectInsuranceCoverage project-id {
                total-covered: (+ (get total-covered current-coverage) coverage-amount),
                coverage-rate: (/ (* (+ (get total-covered current-coverage) coverage-amount) u10000) (get current-amount project)),
                premium-paid: (+ (get premium-paid current-coverage) premium-amount),
                eligible-for-claims: true
            })
        )
        (ok true)
    )
)

(define-public (submit-insurance-claim (project-id uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u61)))
        (investment (unwrap! (map-get? Investments { project-id: project-id, investor: tx-sender }) (err u62)))
        (insurance (unwrap! (map-get? InvestorInsurance { project-id: project-id, investor: tx-sender }) (err u63)))
        (claim-id (var-get next-claim-id))
    )
        (asserts! (is-eq (get status project) "failed") (err u64))
        (asserts! (is-eq (get claim-eligible insurance) true) (err u65))
        (asserts! (>= stacks-block-height (+ (get coverage-start insurance) (var-get claim-processing-period))) (err u66))
        
        (map-set InsuranceClaims claim-id {
            project-id: project-id,
            claimant: tx-sender,
            claim-amount: (get amount investment),
            coverage-amount: (get insured-amount insurance),
            claim-status: "pending",
            claim-submitted: stacks-block-height,
            votes-for: u0,
            votes-against: u0,
            voting-deadline: (+ stacks-block-height insurance-voting-period)
        })
        
        (var-set next-claim-id (+ claim-id u1))
        (ok claim-id)
    )
)

(define-public (vote-on-insurance-claim (claim-id uint) (support bool))
    (let (
        (claim (unwrap! (map-get? InsuranceClaims claim-id) (err u67)))
        (existing-vote (map-get? ClaimVotes { claim-id: claim-id, voter: tx-sender }))
        (voter-investment (map-get? Investments { project-id: (get project-id claim), investor: tx-sender }))
    )
        (asserts! (is-eq (get claim-status claim) "pending") (err u68))
        (asserts! (< stacks-block-height (get voting-deadline claim)) (err u69))
        (asserts! (is-none existing-vote) (err u70))
        (asserts! (is-some voter-investment) (err u71)) ;; Only investors can vote
        
        (map-set ClaimVotes { claim-id: claim-id, voter: tx-sender } support)
        
        (map-set InsuranceClaims claim-id (merge claim {
            votes-for: (if support (+ (get votes-for claim) u1) (get votes-for claim)),
            votes-against: (if support (get votes-against claim) (+ (get votes-against claim) u1))
        }))
        (ok true)
    )
)

(define-public (process-insurance-claim (claim-id uint))
    (let (
        (claim (unwrap! (map-get? InsuranceClaims claim-id) (err u72)))
        (total-votes (+ (get votes-for claim) (get votes-against claim)))
        (approval-rate (if (> total-votes u0) (/ (* (get votes-for claim) u100) total-votes) u0))
        (coverage-amount (get coverage-amount claim))
    )
        (asserts! (>= stacks-block-height (get voting-deadline claim)) (err u73))
        (asserts! (is-eq (get claim-status claim) "pending") (err u74))
        (asserts! (>= (var-get insurance-pool-balance) coverage-amount) (err u75))
        
        (if (>= approval-rate claim-approval-threshold)
            (begin
                (try! (as-contract (stx-transfer? coverage-amount tx-sender (get claimant claim))))
                (var-set insurance-pool-balance (- (var-get insurance-pool-balance) coverage-amount))
                (var-set total-claims-paid (+ (var-get total-claims-paid) coverage-amount))
                (map-set InsuranceClaims claim-id (merge claim { claim-status: "approved" }))
                (ok true)
            )
            (begin
                (map-set InsuranceClaims claim-id (merge claim { claim-status: "rejected" }))
                (ok false)
            )
        )
    )
)

(define-public (contribute-to-insurance-pool (project-id uint))
    (let (
        (project (unwrap! (map-get? Projects project-id) (err u76)))
        (contribution-amount (/ (* (get current-amount project) (var-get insurance-fee-rate)) u10000))
    )
        (asserts! (is-eq (get status project) "completed") (err u77))
        (asserts! (is-eq tx-sender (get owner project)) (err u78))
        
        (try! (stx-transfer? contribution-amount tx-sender (as-contract tx-sender)))
        (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) contribution-amount))
        (ok true)
    )
)

(define-public (update-insurance-parameters (new-fee-rate uint) (new-max-coverage uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u79))
        (asserts! (and (<= new-fee-rate u1000) (>= new-fee-rate u100)) (err u80)) ;; 1-10%
        (asserts! (and (<= new-max-coverage u8000) (>= new-max-coverage u5000)) (err u81)) ;; 50-80%
        
        (var-set insurance-fee-rate new-fee-rate)
        (var-set max-coverage-rate new-max-coverage)
        (ok true)
    )
)

(define-read-only (get-insurance-pool-stats)
    (ok {
        total-pool-balance: (var-get insurance-pool-balance),
        total-claims-paid: (var-get total-claims-paid),
        current-fee-rate: (var-get insurance-fee-rate),
        max-coverage-rate: (var-get max-coverage-rate),
        pool-utilization: (if (> (var-get insurance-pool-balance) u0)
            (/ (* (var-get total-claims-paid) u100) (var-get insurance-pool-balance))
            u0)
    })
)

