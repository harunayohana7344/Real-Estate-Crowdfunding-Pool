;; Real Estate Asset Tokenization System
;; Enables tokenization of completed real estate projects into tradeable assets

(define-constant contract-owner tx-sender)
(define-constant err-unauthorized u100)
(define-constant err-invalid-project u101)
(define-constant err-already-tokenized u102)
(define-constant err-not-tokenized u103)
(define-constant err-insufficient-balance u104)
(define-constant err-invalid-price u105)
(define-constant err-invalid-amount u106)
(define-constant err-self-trade u107)
(define-constant err-listing-not-found u108)

;; Data variables
(define-data-var next-token-id uint u0)
(define-data-var next-listing-id uint u0)
(define-data-var platform-fee-rate uint u250) ;; 2.5% platform fee
(define-data-var min-valuation uint u1000000000) ;; 1000 STX minimum

;; Asset tokens represent ownership shares in tokenized real estate projects
(define-map AssetTokens uint {
    project-id: uint,
    asset-name: (string-ascii 50),
    total-supply: uint,
    token-price: uint,
    current-valuation: uint,
    tokenization-date: uint,
    revenue-per-token: uint,
    last-dividend-block: uint
})

;; Token ownership tracking
(define-map TokenHoldings { token-id: uint, holder: principal } {
    amount: uint,
    acquisition-price: uint,
    purchase-block: uint
})

;; Asset marketplace for trading tokenized real estate
(define-map AssetListings uint {
    token-id: uint,
    seller: principal,
    amount: uint,
    price-per-token: uint,
    listing-type: (string-ascii 20), ;; "fixed" or "auction"
    expiry-block: uint,
    status: (string-ascii 20)
})

;; Asset valuation and performance metrics
(define-map AssetPerformance uint {
    total-trades: uint,
    total-volume: uint,
    last-trade-price: uint,
    price-change-24h: int,
    average-holding-period: uint,
    liquidity-score: uint
})

;; Dividend distribution tracking
(define-map DividendDistributions { token-id: uint, distribution-id: uint } {
    total-amount: uint,
    per-token-amount: uint,
    distribution-block: uint,
    claimed-amount: uint
})

(define-map DividendClaims { token-id: uint, distribution-id: uint, holder: principal } {
    amount-claimed: uint,
    claim-block: uint
})

(define-map TokenDistributionCount uint uint)

;; Read-only functions
(define-read-only (get-asset-token (token-id uint))
    (map-get? AssetTokens token-id)
)

(define-read-only (get-token-holding (token-id uint) (holder principal))
    (default-to 
        { amount: u0, acquisition-price: u0, purchase-block: u0 }
        (map-get? TokenHoldings { token-id: token-id, holder: holder })
    )
)

(define-read-only (get-asset-listing (listing-id uint))
    (map-get? AssetListings listing-id)
)

(define-read-only (get-asset-performance (token-id uint))
    (default-to 
        { total-trades: u0, total-volume: u0, last-trade-price: u0, price-change-24h: 0, average-holding-period: u0, liquidity-score: u0 }
        (map-get? AssetPerformance token-id)
    )
)

;; Helper function to calculate platform fee
(define-private (calculate-platform-fee (amount uint))
    (/ (* amount (var-get platform-fee-rate)) u10000)
)

;; Tokenize a completed real estate project
(define-public (tokenize-real-estate-project (project-id uint) (asset-name (string-ascii 50)) (total-supply uint) (initial-valuation uint))
    (let (
        (token-id (var-get next-token-id))
        (token-price (/ initial-valuation total-supply))
    )
        ;; Verify project exists and is completed (assume external verification)
        (asserts! (> total-supply u0) (err err-invalid-amount))
        (asserts! (>= initial-valuation (var-get min-valuation)) (err err-invalid-price))
        (asserts! (is-eq tx-sender contract-owner) (err err-unauthorized))
        
        ;; Create asset token
        (map-set AssetTokens token-id {
            project-id: project-id,
            asset-name: asset-name,
            total-supply: total-supply,
            token-price: token-price,
            current-valuation: initial-valuation,
            tokenization-date: stacks-block-height,
            revenue-per-token: u0,
            last-dividend-block: u0
        })
        
        ;; Initialize performance tracking
        (map-set AssetPerformance token-id {
            total-trades: u0,
            total-volume: u0,
            last-trade-price: token-price,
            price-change-24h: 0,
            average-holding-period: u0,
            liquidity-score: u50
        })
        
        ;; Mint initial supply to contract owner
        (map-set TokenHoldings { token-id: token-id, holder: tx-sender } {
            amount: total-supply,
            acquisition-price: token-price,
            purchase-block: stacks-block-height
        })
        
        (var-set next-token-id (+ token-id u1))
        (ok token-id)
    )
)

;; Create a marketplace listing for asset tokens
(define-public (list-asset-tokens (token-id uint) (amount uint) (price-per-token uint) (duration-blocks uint))
    (let (
        (listing-id (var-get next-listing-id))
        (holding (get-token-holding token-id tx-sender))
    )
        (asserts! (>= (get amount holding) amount) (err err-insufficient-balance))
        (asserts! (> price-per-token u0) (err err-invalid-price))
        (asserts! (> amount u0) (err err-invalid-amount))
        
        (map-set AssetListings listing-id {
            token-id: token-id,
            seller: tx-sender,
            amount: amount,
            price-per-token: price-per-token,
            listing-type: "fixed",
            expiry-block: (+ stacks-block-height duration-blocks),
            status: "active"
        })
        
        (var-set next-listing-id (+ listing-id u1))
        (ok listing-id)
    )
)

;; Purchase asset tokens from marketplace
(define-public (purchase-asset-tokens (listing-id uint) (amount uint))
    (let (
        (listing (unwrap! (map-get? AssetListings listing-id) (err err-listing-not-found)))
        (token-id (get token-id listing))
        (total-cost (* (get price-per-token listing) amount))
        (platform-fee (calculate-platform-fee total-cost))
        (seller-payment (- total-cost platform-fee))
        (buyer-holding (get-token-holding token-id tx-sender))
        (seller-holding (get-token-holding token-id (get seller listing)))
    )
        (asserts! (is-eq (get status listing) "active") (err err-invalid-project))
        (asserts! (< stacks-block-height (get expiry-block listing)) (err err-invalid-project))
        (asserts! (<= amount (get amount listing)) (err err-invalid-amount))
        (asserts! (not (is-eq tx-sender (get seller listing))) (err err-self-trade))
        (asserts! (>= (stx-get-balance tx-sender) total-cost) (err err-insufficient-balance))
        
        ;; Transfer payment
        (try! (stx-transfer? seller-payment tx-sender (get seller listing)))
        (try! (stx-transfer? platform-fee tx-sender contract-owner))
        
        ;; Update token holdings
        (map-set TokenHoldings { token-id: token-id, holder: tx-sender } {
            amount: (+ (get amount buyer-holding) amount),
            acquisition-price: (get price-per-token listing),
            purchase-block: stacks-block-height
        })
        
        (map-set TokenHoldings { token-id: token-id, holder: (get seller listing) } {
            amount: (- (get amount seller-holding) amount),
            acquisition-price: (get acquisition-price seller-holding),
            purchase-block: (get purchase-block seller-holding)
        })
        
        ;; Update listing
        (if (is-eq amount (get amount listing))
            (map-set AssetListings listing-id (merge listing { status: "completed" }))
            (map-set AssetListings listing-id (merge listing { amount: (- (get amount listing) amount) }))
        )
        
        ;; Update performance metrics
        (let ((current-performance (get-asset-performance token-id)))
            (map-set AssetPerformance token-id {
                total-trades: (+ (get total-trades current-performance) u1),
                total-volume: (+ (get total-volume current-performance) total-cost),
                last-trade-price: (get price-per-token listing),
                price-change-24h: (get price-change-24h current-performance),
                average-holding-period: (get average-holding-period current-performance),
                liquidity-score: (if (> (+ (get liquidity-score current-performance) u5) u100) u100 (+ (get liquidity-score current-performance) u5))
            })
        )
        (ok true)
    )
)

;; Distribute dividends to token holders
(define-public (distribute-asset-dividends (token-id uint) (total-dividend-amount uint))
    (let (
        (asset (unwrap! (map-get? AssetTokens token-id) (err err-not-tokenized)))
        (distribution-count (default-to u0 (map-get? TokenDistributionCount token-id)))
        (per-token-amount (/ total-dividend-amount (get total-supply asset)))
    )
        (asserts! (is-eq tx-sender contract-owner) (err err-unauthorized))
        (asserts! (> total-dividend-amount u0) (err err-invalid-amount))
        
        (map-set DividendDistributions { token-id: token-id, distribution-id: distribution-count } {
            total-amount: total-dividend-amount,
            per-token-amount: per-token-amount,
            distribution-block: stacks-block-height,
            claimed-amount: u0
        })
        
        (map-set AssetTokens token-id (merge asset {
            revenue-per-token: (+ (get revenue-per-token asset) per-token-amount),
            last-dividend-block: stacks-block-height
        }))
        
        (map-set TokenDistributionCount token-id (+ distribution-count u1))
        (ok distribution-count)
    )
)

;; Claim dividend payments
(define-public (claim-asset-dividends (token-id uint) (distribution-id uint))
    (let (
        (distribution (unwrap! (map-get? DividendDistributions { token-id: token-id, distribution-id: distribution-id }) (err err-listing-not-found)))
        (holding (get-token-holding token-id tx-sender))
        (dividend-amount (* (get amount holding) (get per-token-amount distribution)))
        (existing-claim (map-get? DividendClaims { token-id: token-id, distribution-id: distribution-id, holder: tx-sender }))
    )
        (asserts! (> (get amount holding) u0) (err err-insufficient-balance))
        (asserts! (is-none existing-claim) (err err-already-tokenized))
        
        (try! (as-contract (stx-transfer? dividend-amount tx-sender tx-sender)))
        
        (map-set DividendClaims { token-id: token-id, distribution-id: distribution-id, holder: tx-sender } {
            amount-claimed: dividend-amount,
            claim-block: stacks-block-height
        })
        
        (map-set DividendDistributions { token-id: token-id, distribution-id: distribution-id }
            (merge distribution { claimed-amount: (+ (get claimed-amount distribution) dividend-amount) })
        )
        (ok dividend-amount)
    )
)

;; Update asset valuation
(define-public (update-asset-valuation (token-id uint) (new-valuation uint))
    (let ((asset (unwrap! (map-get? AssetTokens token-id) (err err-not-tokenized))))
        (asserts! (is-eq tx-sender contract-owner) (err err-unauthorized))
        (asserts! (>= new-valuation (var-get min-valuation)) (err err-invalid-price))
        
        (map-set AssetTokens token-id (merge asset {
            current-valuation: new-valuation,
            token-price: (/ new-valuation (get total-supply asset))
        }))
        (ok true)
    )
)

;; Transfer asset tokens between holders
(define-public (transfer-asset-tokens (token-id uint) (recipient principal) (amount uint))
    (let (
        (sender-holding (get-token-holding token-id tx-sender))
        (recipient-holding (get-token-holding token-id recipient))
    )
        (asserts! (>= (get amount sender-holding) amount) (err err-insufficient-balance))
        (asserts! (not (is-eq tx-sender recipient)) (err err-self-trade))
        
        (map-set TokenHoldings { token-id: token-id, holder: tx-sender } {
            amount: (- (get amount sender-holding) amount),
            acquisition-price: (get acquisition-price sender-holding),
            purchase-block: (get purchase-block sender-holding)
        })
        
        (map-set TokenHoldings { token-id: token-id, holder: recipient } {
            amount: (+ (get amount recipient-holding) amount),
            acquisition-price: (get acquisition-price recipient-holding),
            purchase-block: stacks-block-height
        })
        (ok true)
    )
)

;; Cancel marketplace listing
(define-public (cancel-asset-listing (listing-id uint))
    (let ((listing (unwrap! (map-get? AssetListings listing-id) (err err-listing-not-found))))
        (asserts! (is-eq tx-sender (get seller listing)) (err err-unauthorized))
        (asserts! (is-eq (get status listing) "active") (err err-invalid-project))
        
        (map-set AssetListings listing-id (merge listing { status: "cancelled" }))
        (ok true)
    )
)

;; Read-only analytics functions
(define-read-only (get-token-holder-count (token-id uint))
    ;; Simplified - would need iteration in full implementation
    (ok u1)
)

(define-read-only (get-asset-market-stats (token-id uint))
    (let (
        (asset (unwrap! (map-get? AssetTokens token-id) (err err-not-tokenized)))
        (performance (get-asset-performance token-id))
    )
        (ok {
            current-price: (get token-price asset),
            market-cap: (get current-valuation asset),
            trading-volume: (get total-volume performance),
            liquidity-score: (get liquidity-score performance),
            total-trades: (get total-trades performance)
        })
    )
)

(define-read-only (get-portfolio-value (holder principal))
    ;; Simplified calculation - would iterate through all holdings in full implementation
    (ok u0)
)

;; Admin function to update platform parameters
(define-public (update-platform-parameters (new-fee-rate uint) (new-min-valuation uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-unauthorized))
        (asserts! (<= new-fee-rate u1000) (err err-invalid-price)) ;; Max 10%
        (asserts! (>= new-min-valuation u100000000) (err err-invalid-price)) ;; Min 100 STX
        
        (var-set platform-fee-rate new-fee-rate)
        (var-set min-valuation new-min-valuation)
        (ok true)
    )
)
