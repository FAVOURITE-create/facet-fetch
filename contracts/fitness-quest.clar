;; Facet-Fetch Fitness Quest Contract
;; A decentralized platform for tracking fitness challenges and rewarding user achievements.
;; Core functionalities remain consistent with the original implementation.

;; Error Codes
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-QUEST-NOT-FOUND (err u101))
(define-constant ERR-QUEST-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-PARAMETERS (err u103))
(define-constant ERR-ALREADY-ENROLLED (err u104))
(define-constant ERR-NOT-ENROLLED (err u105))
(define-constant ERR-INSUFFICIENT-STAKE (err u106))
(define-constant ERR-QUEST-FULL (err u107))
(define-constant ERR-QUEST-ENDED (err u108))
(define-constant ERR-QUEST-NOT-STARTED (err u109))
(define-constant ERR-QUEST-ACTIVE (err u110))
(define-constant ERR-INVALID-PROGRESS (err u111))
(define-constant ERR-REWARDS-CLAIMED (err u112))

;; Roles and Access Control
(define-constant PLATFORM-ADMIN "platform-admin")
(define-constant DATA-ORACLE "data-oracle")
(define-constant MIN-QUEST-DURATION u86400) ;; 1 day in seconds
(define-constant MAX-QUEST-DURATION u2592000) ;; 30 days in seconds

;; Data Structures
(define-map authorized-roles
  {
    role: (string-ascii 20),
    address: principal,
  }
  { authorized: bool }
)

(define-map fitness-quests
  { quest-id: uint }
  {
    name: (string-utf8 100),
    description: (string-utf8 500),
    creator: principal,
    is-official: bool,
    start-time: uint,
    end-time: uint,
    fitness-goal: uint,
    distance-goal: uint,
    entry-fee: uint,
    max-participants: uint,
    reward-pool: uint,
    is-active: bool,
    is-ended: bool,
    participants-count: uint,
  }
)

(define-map participant-progress
  {
    quest-id: uint,
    participant: principal,
  }
  {
    enrolled-at: uint,
    total-activity: uint,
    total-distance: uint,
    last-update: uint,
    stake-amount: uint,
    reward-claimed: bool,
  }
)

(define-map quest-leaderboard
  { quest-id: uint }
  { participants: (list 50 {
    participant: principal,
    total-activity: uint,
    total-distance: uint,
  }) }
)

(define-map participant-achievements
  {
    quest-id: uint,
    participant: principal,
  }
  {
    completed-quest: bool,
    reached-activity-goal: bool,
    reached-distance-goal: bool,
  }
)

;; Global Variables
(define-data-var next-quest-id uint u1)
(define-data-var platform-owner principal tx-sender)

;; Private Helper Functions
(define-private (is-platform-owner (caller principal))
  (is-eq caller (var-get platform-owner))
)

(define-private (quest-exists (quest-id uint))
  (is-some (map-get? fitness-quests { quest-id: quest-id }))
)

(define-private (is-quest-active (quest-id uint))
  (let ((quest (unwrap! (map-get? fitness-quests { quest-id: quest-id }) false)))
    (and
      (get is-active quest)
      (not (get is-ended quest))
      (>= block-height (get start-time quest))
      (< block-height (get end-time quest))
    )
  )
)

(define-private (is-enrolled
    (quest-id uint)
    (user principal)
  )
  (is-some (map-get? participant-progress {
    quest-id: quest-id,
    participant: user,
  }))
)

(define-read-only (get-quest (quest-id uint))
  (map-get? fitness-quests { quest-id: quest-id })
)

(define-read-only (get-participant-progress
    (quest-id uint)
    (participant principal)
  )
  (map-get? participant-progress {
    quest-id: quest-id,
    participant: participant,
  })
)

(define-public (set-platform-owner (new-owner principal))
  (begin
    (asserts! (is-platform-owner tx-sender) ERR-UNAUTHORIZED)
    (ok (var-set platform-owner new-owner))
  )
)

(define-public (grant-role
    (role (string-ascii 20))
    (address principal)
  )
  (begin
    (asserts! (is-platform-owner tx-sender) ERR-UNAUTHORIZED)
    (ok (map-set authorized-roles {
      role: role,
      address: address,
    } { authorized: true }
    ))
  )
)

(define-public (revoke-role
    (role (string-ascii 20))
    (address principal)
  )
  (begin
    (asserts! (is-platform-owner tx-sender) ERR-UNAUTHORIZED)
    (ok (map-set authorized-roles {
      role: role,
      address: address,
    } { authorized: false }
    ))
  )
)

;; Main Quest Registration and Progress Tracking Functions
(define-public (enroll-in-quest (quest-id uint))
  (let ((quest (unwrap! (map-get? fitness-quests { quest-id: quest-id })
      ERR-QUEST-NOT-FOUND
    )))
    (begin
      ;; Validate quest status
      (asserts! (get is-active quest) ERR-QUEST-NOT-STARTED)
      (asserts! (not (get is-ended quest)) ERR-QUEST-ENDED)
      (asserts! (< block-height (get end-time quest)) ERR-QUEST-ENDED)
      
      ;; Check enrollment status
      (asserts! (not (is-enrolled quest-id tx-sender)) ERR-ALREADY-ENROLLED)
      
      ;; Check quest capacity
      (asserts!
        (< (get participants-count quest) (get max-participants quest))
        ERR-QUEST-FULL
      )
      
      ;; Handle entry fee
      (if (> (get entry-fee quest) u0)
        (try! (stx-transfer? (get entry-fee quest) tx-sender
          (as-contract tx-sender)
        ))
        true
      )
      
      ;; Register participant
      (map-set participant-progress {
        quest-id: quest-id,
        participant: tx-sender,
      } {
        enrolled-at: block-height,
        total-activity: u0,
        total-distance: u0,
        last-update: block-height,
        stake-amount: (get entry-fee quest),
        reward-claimed: false,
      })
      
      ;; Initialize achievements
      (map-set participant-achievements {
        quest-id: quest-id,
        participant: tx-sender,
      } {
        completed-quest: false,
        reached-activity-goal: false,
        reached-distance-goal: false,
      })
      
      ;; Update quest participants
      (map-set fitness-quests { quest-id: quest-id }
        (merge quest { 
          participants-count: (+ (get participants-count quest) u1),
          reward-pool: (+ (get reward-pool quest) (get entry-fee quest))
        })
      )
      
      (ok true)
    )
  )
)

(define-public (add-to-reward-pool
    (quest-id uint)
    (amount uint)
  )
  (let ((quest (unwrap! (map-get? fitness-quests { quest-id: quest-id })
      ERR-QUEST-NOT-FOUND
    )))
    (begin
      ;; Validate quest status
      (asserts! (get is-active quest) ERR-QUEST-NOT-STARTED)
      (asserts! (not (get is-ended quest)) ERR-QUEST-ENDED)
      
      ;; Transfer STX to contract
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update reward pool
      (map-set fitness-quests { quest-id: quest-id }
        (merge quest { reward-pool: (+ (get reward-pool quest) amount) })
      )
      
      (ok true)
    )
  )
)