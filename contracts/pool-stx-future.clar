
;;;;;;;;;;;;;;;;;;;;;; Begin configuration ;;;;;;;;;;;;;;;;;;;;;;

;; Hard-coded reward cycle in which the contained STX tokens shall lock.
;; Change to your liking.  Must be greater than the current reward cycle.
(define-constant FIRST-REWARD-CYCLE u1)

;; Hard-coded length of the lock-up.
;; Change to your liking.  Can be between 1 and 12, inclusive.
(define-constant REWARD-CYCLE-LOCK-PERIOD u12)

(define-constant pool-stx-address 'ST312YCG27SPCWX627V2X1CQNBPC0TRHYPXTA06J0)
(define-constant pool-pox-address {hashbytes: 0x13effebe0ea4bb45e35694f5a15bb5b96e851afb, version: 0x01})
(define-constant time-limit u600000 )
(define-constant pool-deployer tx-sender)

;;;;;;;;;;;;;;;;;;;;;; End of configuration ;;;;;;;;;;;;;;;;;;;;;;

;; self-consistency check
(begin
    (asserts! (> FIRST-REWARD-CYCLE (burn-height-to-reward-cycle burn-block-height))
        (err "Invalid configuration -- bad FIRST-REWARD-CYCLE"))
    (asserts! (and (>= REWARD-CYCLE-LOCK-PERIOD u1) (<= REWARD-CYCLE-LOCK-PERIOD u12))
        (err "Invalid configuration -- bad REWARD-CYCLE-LOCK-PERIOD"))
)

;; error constants
(define-constant ERR-UNAUTHORIZED u1)
(define-constant ERR-IN-PROGRESS u2)
(define-constant ERR-INSUFFICIENT-BALANCE u3)
(define-constant ERR-REQUEST-TOO-LARGE u4)
(define-constant ERR-ALREADY-LOCKED u5)
(define-constant ERR-NOT-YET-REDEEMABLE u6)
(define-constant ERR-COMMIT-TOO-EARLY u7)

;; the actual token
(define-fungible-token stx-future)

;; if set to true, then the tokens have been locked already in PoX
(define-data-var locked bool false)

;; Backport of .pox's burn-height-to-reward-cycle
(define-private (burn-height-to-reward-cycle (height uint))
    (let (
        (pox-info (unwrap-panic (contract-call? 'ST000000000000000000002AMW42H.pox get-pox-info)))
    )
    (/ (- height (get first-burnchain-block-height pox-info)) (get reward-cycle-length pox-info)))
)

;; Backport of .pox's reward-cycle-to-burn-height
(define-private (reward-cycle-to-burn-height (cycle uint))
    (let (
        (pox-info (unwrap-panic (contract-call? 'ST000000000000000000002AMW42H.pox get-pox-info)))
    )
    (+ (get first-burnchain-block-height pox-info) (* cycle (get reward-cycle-length pox-info))))
)

;; Delegate stx and stack immediately
(define-private (pox-delegate-stx-and-stack (amount-ustx uint) (until-burn-ht (optional uint)))
  (begin
    (let ((ignore-result-revoke (contract-call? 'ST000000000000000000002AMW42H.pox revoke-delegate-stx))
          (start-block-ht (print (+ burn-block-height u1)))
          (locking-cycles REWARD-CYCLE-LOCK-PERIOD))
      (match (as-contract (contract-call? 'ST000000000000000000002AMW42H.pox delegate-stx amount-ustx pool-stx-address until-burn-ht none))
        success
          (let ((stacker (as-contract tx-sender)))
            (match (as-contract (contract-call? 'ST000000000000000000002AMW42H.pox delegate-stack-stx stacker amount-ustx pool-pox-address start-block-ht locking-cycles))
              stack-success (ok stack-success)
              stack-error (print (err (to-uint stack-error)))))
        error (err (to-uint error))))))

;; Self-service endpoint for buying STX futures for STX that will be locked in the tranche's reward cycle.
(define-public (buy-stx-futures (amount-ustx uint))
    (let (
        (cur-reward-cycle (print (burn-height-to-reward-cycle burn-block-height)))
        (locked? (var-get locked))
    )
    (begin
        ;; can't buy futures for a reward cycle that's already in progress
        (asserts! (< cur-reward-cycle FIRST-REWARD-CYCLE)
            (err ERR-IN-PROGRESS))

        ;; can't buy futures if the STX for this reward cycle are already stacked
        (asserts! (not locked?)
            (err ERR-ALREADY-LOCKED))

        ;; buyer has to have enough STX
        (asserts! (<= amount-ustx (stx-get-balance tx-sender))
            (err ERR-INSUFFICIENT-BALANCE))

        ;; do the transfer and mint, but abort entirely if this fails
        (unwrap-panic (stx-transfer? amount-ustx tx-sender (as-contract tx-sender)))
        (unwrap-panic (ft-mint? stx-future amount-ustx tx-sender))
        (match (pox-delegate-stx-and-stack amount-ustx none)
            success-pox (ok true)
            error (err error))
    ))
)

;; Self-service endpoint for redeeming STX that have unlocked (or were never locked)
;; Anyone with some `stx-future` tokens can call this endpoint to redeem them for
;; the given tranche's STX.
(define-public (redeem-stx-futures (amount-futures uint))
    (let (
        (sender-futures (ft-get-balance stx-future tx-sender))
        (contract-ustx (stx-get-balance (as-contract tx-sender)))
        (unlock-cycle (+ FIRST-REWARD-CYCLE REWARD-CYCLE-LOCK-PERIOD))
        (locked? (var-get locked))
        (cur-reward-cycle (burn-height-to-reward-cycle burn-block-height))
        (caller-id tx-sender)
    )
    (begin
        ;; caller must have this many stx-futures to burn
        (asserts! (<= amount-futures sender-futures)
            (err ERR-REQUEST-TOO-LARGE))

        ;; contract must have this many STX to redeem
        (asserts! (<= amount-futures contract-ustx)
            (err ERR-REQUEST-TOO-LARGE))

        ;; the STX must have unlocked by the time of this call,
        ;; OR,
        ;; the STX used to buy these stx-futures are not Stacked
        ;; and the intended reward cycle has already begun (i.e. the operator
        ;; of this contract forgot to Stack).
        (asserts! (or (>= cur-reward-cycle unlock-cycle)
                      (and (>= cur-reward-cycle FIRST-REWARD-CYCLE) (not locked?)))
            (err ERR-NOT-YET-REDEEMABLE))

        ;; destroy the stx-future token and redeem the STX
        (as-contract
            (unwrap-panic (stx-transfer? amount-futures tx-sender caller-id)))
        (unwrap-panic (ft-burn? stx-future amount-futures tx-sender))
        (ok true)
    ))
)

;; inner fold function for verifying that the `candidate` is authorized to Stack a tranche's STX.
(define-private (auth-check (candidate principal) (data { caller: principal, was-allowed: bool }))
    {
        caller: (get caller data),
        was-allowed: (if (is-eq candidate (get caller data))
                        true
                        (get was-allowed data))
    }
)

;; Stack the STX tranche, and send the rewards to the given pox-addr.
(define-public (stack-stx-tranche (pox-addr { version: (buff 1), hashbytes: (buff 20) }))
        (let (
            (already-locked (var-get locked))
            (contract-balance (stx-get-balance (as-contract tx-sender)))
            (caller-id tx-sender)
            (cur-reward-cycle (burn-height-to-reward-cycle burn-block-height))
        )
        (begin
            ;; contract has STX to Stack
            (asserts! (< u0 contract-balance)
                (err 18))   ;; ERR_STACKING_INVALID_AMOUNT in .pox

            ;; must happen before the intended start of locking
            (asserts! (< cur-reward-cycle FIRST-REWARD-CYCLE)
                (err 24))   ;; ERR_INVALID_START_BURN_HEIGHT in .pox

            ;; can only do this successfully once
            (asserts! (not already-locked)
                (err 3))    ;; ERR_STACKING_ALREADY_STACKED in .pox

            (let (
                ;; do the actual stacking.
                (pox-result (as-contract
                    (unwrap-panic (contract-call? 'ST000000000000000000002AMW42H.pox stack-stx contract-balance pox-addr burn-block-height REWARD-CYCLE-LOCK-PERIOD)))
                )
            )
            (begin
                ;; don't lock in this reward cycle again
                (var-set locked true)
                (ok pox-result)
            ))
        ))
)


(define-public (stack-aggregation-commit (reward-cycle uint))
  (if (> burn-block-height time-limit)
    (match (as-contract (contract-call? 'ST000000000000000000002AMW42H.pox stack-aggregation-commit pool-pox-address reward-cycle))
      success (ok success)
      error (err-pox-stack-aggregation-commit error))
    (err ERR-COMMIT-TOO-EARLY)))


(define-public (allow-contract-caller (this-contract principal))
  (if (is-eq tx-sender pool-deployer)
    (as-contract (contract-call? 'ST000000000000000000002AMW42H.pox allow-contract-caller this-contract none))
    (err 403)))

;;;;;;;;;;;;;;;;;;;;; SIP 010 ;;;;;;;;;;;;;;;;;;;;;;

(define-public (transfer (amount uint) (from principal) (to principal))
    (begin
        (asserts! (is-eq from tx-sender)
            (err ERR-UNAUTHORIZED))

        (ft-transfer? stx-future amount from to)
    )
)

(define-public (get-name)
    (ok "STX-futures"))

(define-public (get-symbol)
    (ok "STXF"))

(define-public (get-decimals)
    (ok u6))

(define-public (get-balance-of (user principal))
    (ok (ft-get-balance stx-future user)))

(define-public (get-total-supply)
    (ok (stx-get-balance (as-contract tx-sender))))

(define-public (get-token-uri)
    (ok none))

;; Error Handling

(define-private (err-pox-stack-aggregation-commit (code int))
  (err (to-uint (* 1000 code))))
