;;; wire-host.el --- protojson codec for the agentrepl.v1 HOST section -*- lexical-binding: t; -*-

;;; Commentary:

;; THE HOST SECTION'S CODEC.  Emacs is a HOST, not an author: its whole
;; workspace contribution is REGISTER (hand the daemon a dir, get back the
;; minted identity) and SELECT (the user switched tabs), plus the two streams
;; it holds — one WatchHostWorkspace per open workspace and the one
;; WatchDaemon — and the handover's AdoptHostWorkspace.  This file encodes
;; those requests and decodes those responses and pushes.
;;
;; The shared primitives, the validation invariant and the elisp shape all
;; live in `wire-common.el'; read its commentary first.
;;
;; TWO PLACES WHERE AN UNSET ONEOF IS LEGAL, both stated by the proto:
;;   - `HostSessionLive.vendor_info' — "Unset while no vendor conversation
;;     exists yet";
;; everything else unset is a contract breach that raises here.

;;; Code:

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl-wire--check-keys "wire-common")
(declare-function agent-repl-wire-decode-inside-temporary-directory "wire-common")
(declare-function agent-repl-wire--decode-bool "wire-common")
(declare-function agent-repl-wire--decode-empty "wire-common")
(declare-function agent-repl-wire--decode-int64 "wire-common")
(declare-function agent-repl-wire--decode-message "wire-common")
(declare-function agent-repl-wire--decode-oneof "wire-common")
(declare-function agent-repl-wire--decode-uint32 "wire-common")
(declare-function agent-repl-wire--decode-optional-message "wire-common")
(declare-function agent-repl-wire--decode-optional-string "wire-common")
(declare-function agent-repl-wire--decode-uint64 "wire-common")
(declare-function agent-repl-wire-decode-turn-id "wire-common")
(declare-function agent-repl-wire-decode-user-said "wire-common")
(declare-function agent-repl-wire--decode-optional-uint32 "wire-common")
(declare-function agent-repl-wire--decode-repeated "wire-common")
(declare-function agent-repl-wire--decode-string "wire-common")
(declare-function agent-repl-wire--decoded "wire-common")
(declare-function agent-repl-wire--encode-empty "wire-common")
(declare-function agent-repl-wire--encode-oneof "wire-common")
(declare-function agent-repl-wire--encode-repeated "wire-common")
(declare-function agent-repl-wire-decode-roster-row "wire-roster" (value))
(declare-function agent-repl-wire--encode-string "wire-common")
(declare-function agent-repl-wire--encoded "wire-common")
(declare-function agent-repl-wire--fail "wire-common")
(declare-function agent-repl-wire--object "wire-common")
(declare-function agent-repl-wire-decode-drain-reason "wire-common")
(declare-function agent-repl-wire-decode-workspace-ref "wire-common")
(declare-function agent-repl-wire-decode-workspace-parent-not-open "wire-common")
(declare-function agent-repl-wire-decode-daemon-stream-ending "wire-common")
(declare-function agent-repl-wire-decode-create-workspace-error "wire-verbs")
(declare-function agent-repl-wire-decode-nuke-workspace-error "wire-verbs")
(declare-function agent-repl-wire-decode-daemon-fault "wire-verbs")
(declare-function agent-repl-wire-decode-persistent-wifi-state "wire-verbs")
(declare-function agent-repl-wire--raw "wire-common")
(declare-function agent-repl-wire--present-p "wire-common")
(declare-function agent-repl-wire-decode-feed-id "wire-common")
(declare-function agent-repl-wire-decode-session-fault-bounce-died "wire-common")
(declare-function agent-repl-wire-decode-session-fault-bounce-unknown "wire-common")
(declare-function agent-repl-wire-decode-session-fault-adoption-window-expired "wire-common")
(declare-function agent-repl-wire-decode-session-fault-final-answer-unresolved "wire-common")
(declare-function agent-repl-wire-decode-session-fault-vendor-start-retrying "wire-common")
(declare-function agent-repl-wire-decode-session-fault-vendor-start-rejected "wire-common")
(declare-function agent-repl-wire-decode-session-fault-vendor-start-failed "wire-common")
(declare-function agent-repl-wire-decode-session-fault-classifier-failed "wire-common")
(declare-function agent-repl-wire-decode-session-fault-conversation-abandoned "wire-common")
(declare-function agent-repl-wire-decode-session-fault-daemon-state-unreadable "wire-common")
(declare-function agent-repl-wire-decode-session-fault-link-severed "wire-common")
(declare-function agent-repl-wire-decode-session-fault-resume-failed "wire-common")
(declare-function agent-repl-wire-decode-session-fault-session-absent "wire-common")
(declare-function agent-repl-wire-decode-session-fault-shim-died "wire-common")
(declare-function agent-repl-wire-decode-session-fault-shim-reported "wire-common")
(declare-function agent-repl-wire-decode-session-fault-shim-start-failed "wire-common")
(declare-function agent-repl-wire-decode-session-fault-watch-open-refused "wire-common")
(declare-function agent-repl-wire-decode-workspace-ref "wire-common")
(declare-function agent-repl-wire-encode-workspace-ref "wire-common")

;; wire-common.el is loaded immediately before this file by config.el's
;; module list; that ordering is the dependency, as it is everywhere else in
;; this module (no file here `require's a sibling).

;;;; ---- Leaf identities ----

(defun agent-repl-wire-decode-host-session-id (value)
  "Decode VALUE as `HostSessionId', a plist `(:value)'.
The echo token Emacs correlates transcripts, health probes and fault
windows against; sessions rotate under one workspace."
  (let ((object (agent-repl-wire--object "HostSessionId" value)))
    (agent-repl-wire--check-keys "HostSessionId" object '(value))
    (agent-repl-wire--decoded
     "HostSessionId"
     (list :value (agent-repl-wire--decode-string "HostSessionId" 'value object)))))

(defun agent-repl-wire-decode-host-generation-id (value)
  "Decode VALUE as `HostGenerationId', a plist `(:value)'.
Rotates on restart within one session; fault windows scope to it."
  (let ((object (agent-repl-wire--object "HostGenerationId" value)))
    (agent-repl-wire--check-keys "HostGenerationId" object '(value))
    (agent-repl-wire--decoded
     "HostGenerationId"
     (list :value (agent-repl-wire--decode-string "HostGenerationId" 'value object)))))

(defun agent-repl-wire-decode-host-vendor-claude (value)
  "Decode VALUE as `HostVendorClaude', a plist `(:session-id :config-dir)'.
`config_dir' names WHICH ACCOUNT the conversation belongs to."
  (let ((object (agent-repl-wire--object "HostVendorClaude" value)))
    (agent-repl-wire--check-keys "HostVendorClaude" object '(sessionId configDir))
    (agent-repl-wire--decoded
     "HostVendorClaude"
     (list :session-id (agent-repl-wire--decode-string
                        "HostVendorClaude" 'sessionId object)
           :config-dir (agent-repl-wire--decode-string
                        "HostVendorClaude" 'configDir object)))))

(defun agent-repl-wire-decode-host-fault-shim-start-failed (value)
  "Decode HostFault's `shim_start_failed' kind arm from VALUE as a
`SessionFaultShimStartFailed'."
  (agent-repl-wire-decode-session-fault-shim-start-failed value))

(defun agent-repl-wire-decode-host-fault-shim-died (value)
  "Decode HostFault's `shim_died' kind arm from VALUE as a
`SessionFaultShimDied'."
  (agent-repl-wire-decode-session-fault-shim-died value))

(defun agent-repl-wire-decode-host-fault-link-severed (value)
  "Decode HostFault's `link_severed' kind arm from VALUE as a
`SessionFaultLinkSevered'."
  (agent-repl-wire-decode-session-fault-link-severed value))

(defun agent-repl-wire-decode-host-fault-resume-failed (value)
  "Decode HostFault's `resume_failed' kind arm from VALUE as a
`SessionFaultResumeFailed'."
  (agent-repl-wire-decode-session-fault-resume-failed value))

(defun agent-repl-wire-decode-host-fault-bounce-died (value)
  "Decode HostFault's `bounce_died' kind arm from VALUE as a
`SessionFaultBounceDied'."
  (agent-repl-wire-decode-session-fault-bounce-died value))

(defun agent-repl-wire-decode-host-fault-bounce-unknown (value)
  "Decode HostFault's `bounce_unknown' kind arm from VALUE as a
`SessionFaultBounceUnknown'."
  (agent-repl-wire-decode-session-fault-bounce-unknown value))

(defun agent-repl-wire-decode-host-fault-classifier-failed (value)
  "Decode HostFault's `classifier_failed' kind arm from VALUE as a
`SessionFaultClassifierFailed'."
  (agent-repl-wire-decode-session-fault-classifier-failed value))

(defun agent-repl-wire-decode-host-fault-shim-reported (value)
  "Decode HostFault's `shim_reported' kind arm from VALUE as a
`SessionFaultShimReported'."
  (agent-repl-wire-decode-session-fault-shim-reported value))

(defun agent-repl-wire-decode-host-fault-conversation-abandoned (value)
  "Decode HostFault's `conversation_abandoned' kind arm from VALUE as a
`SessionFaultConversationAbandoned'."
  (agent-repl-wire-decode-session-fault-conversation-abandoned value))

(defun agent-repl-wire-decode-host-fault-session-absent (value)
  "Decode HostFault's `session_absent' kind arm from VALUE as a
`SessionFaultSessionAbsent'."
  (agent-repl-wire-decode-session-fault-session-absent value))

(defun agent-repl-wire-decode-host-fault-watch-open-refused (value)
  "Decode HostFault's `watch_open_refused' kind arm from VALUE as a
`SessionFaultWatchOpenRefused'."
  (agent-repl-wire-decode-session-fault-watch-open-refused value))

(defun agent-repl-wire-decode-host-fault-daemon-state-unreadable (value)
  "Decode HostFault's `daemon_state_unreadable' kind arm from VALUE as a
`SessionFaultDaemonStateUnreadable'."
  (agent-repl-wire-decode-session-fault-daemon-state-unreadable value))

(defun agent-repl-wire-decode-host-fault-adoption-window-expired (value)
  "Decode HostFault's `adoption_window_expired' kind arm from VALUE as a
`SessionFaultAdoptionWindowExpired'."
  (agent-repl-wire-decode-session-fault-adoption-window-expired value))

(defun agent-repl-wire-decode-host-fault-final-answer-unresolved (value)
  "Decode HostFault's `final_answer_unresolved' kind arm from VALUE as a
`SessionFaultFinalAnswerUnresolved'."
  (agent-repl-wire-decode-session-fault-final-answer-unresolved value))

(defun agent-repl-wire-decode-host-fault-vendor-start-retrying (value)
  "Decode HostFault's `vendor_start_retrying' kind arm from VALUE as a
`SessionFaultVendorStartRetrying'."
  (agent-repl-wire-decode-session-fault-vendor-start-retrying value))

(defun agent-repl-wire-decode-host-fault-vendor-start-rejected (value)
  "Decode HostFault's `vendor_start_rejected' kind arm from VALUE as a
`SessionFaultVendorStartRejected'."
  (agent-repl-wire-decode-session-fault-vendor-start-rejected value))

(defun agent-repl-wire-decode-host-fault-vendor-start-failed (value)
  "Decode HostFault's `vendor_start_failed' kind arm from VALUE as a
`SessionFaultVendorStartFailed'."
  (agent-repl-wire-decode-session-fault-vendor-start-failed value))

(defun agent-repl-wire-decode-host-fault-kind (value)
  "Decode HostFault's `kind' oneof from the object VALUE.
THE ARM IS THE FAULT CLASS: `detail' supplements it and never replaces
it, so a fault with no kind is a contract breach rather than a prose-only
fault the consumer would have to parse."
  (let ((object (agent-repl-wire--object "HostFault" value)))
    (agent-repl-wire--decode-oneof
     "HostFault" 'kind object
     '((shimStartFailed :shim-start-failed agent-repl-wire-decode-host-fault-shim-start-failed)
       (shimDied :shim-died agent-repl-wire-decode-host-fault-shim-died)
       (linkSevered :link-severed agent-repl-wire-decode-host-fault-link-severed)
       (resumeFailed :resume-failed agent-repl-wire-decode-host-fault-resume-failed)
       (bounceDied :bounce-died agent-repl-wire-decode-host-fault-bounce-died)
       (bounceUnknown :bounce-unknown agent-repl-wire-decode-host-fault-bounce-unknown)
       (classifierFailed :classifier-failed agent-repl-wire-decode-host-fault-classifier-failed)
       (shimReported :shim-reported agent-repl-wire-decode-host-fault-shim-reported)
       (conversationAbandoned :conversation-abandoned agent-repl-wire-decode-host-fault-conversation-abandoned)
       (sessionAbsent :session-absent agent-repl-wire-decode-host-fault-session-absent)
       (watchOpenRefused :watch-open-refused agent-repl-wire-decode-host-fault-watch-open-refused)
       (daemonStateUnreadable :daemon-state-unreadable agent-repl-wire-decode-host-fault-daemon-state-unreadable)
       (adoptionWindowExpired :adoption-window-expired agent-repl-wire-decode-host-fault-adoption-window-expired)
       (finalAnswerUnresolved :final-answer-unresolved agent-repl-wire-decode-host-fault-final-answer-unresolved)
       (vendorStartRetrying :vendor-start-retrying agent-repl-wire-decode-host-fault-vendor-start-retrying)
       (vendorStartRejected :vendor-start-rejected agent-repl-wire-decode-host-fault-vendor-start-rejected)
       (vendorStartFailed :vendor-start-failed agent-repl-wire-decode-host-fault-vendor-start-failed)))))

(defun agent-repl-wire-decode-host-fault (value)
  "Decode VALUE as `HostFault', a plist `(:detail :opened-at-ms :kind)'.
The kinds are the session controller's own fault vocabulary, shared
verbatim with SessionHealth's `SessionFault' — the stream reporting a
fault never changes its class."
  (let ((object (agent-repl-wire--object "HostFault" value)))
    (agent-repl-wire--check-keys
     "HostFault" object '(detail openedAtMs shimStartFailed shimDied linkSevered resumeFailed bounceDied bounceUnknown classifierFailed shimReported conversationAbandoned sessionAbsent watchOpenRefused daemonStateUnreadable adoptionWindowExpired finalAnswerUnresolved vendorStartRetrying vendorStartRejected vendorStartFailed))
    (agent-repl-wire--decoded
     "HostFault"
     (list :detail (agent-repl-wire--decode-string "HostFault" 'detail object)
           :opened-at-ms (agent-repl-wire--decode-int64
                          "HostFault" 'openedAtMs object)
           :kind (agent-repl-wire-decode-host-fault-kind object)))))

(defun agent-repl-wire-decode-host-workspace-naming (value)
  "Decode VALUE as `HostWorkspaceNaming', a plist `(:slug :title)'.
Both fields are OPTIONAL and stay unset until the daemon (slug) and the
vendor (title) have derived one; the MESSAGE itself is required, because a
buffer needs a name in every standing."
  (let ((object (agent-repl-wire--object "HostWorkspaceNaming" value)))
    (agent-repl-wire--check-keys "HostWorkspaceNaming" object '(slug title))
    (agent-repl-wire--decoded
     "HostWorkspaceNaming"
     (list :slug (agent-repl-wire--decode-optional-string
                  "HostWorkspaceNaming" 'slug object)
           :title (agent-repl-wire--decode-optional-string
                   "HostWorkspaceNaming" 'title object)))))

;;;; ---- HostBackfill ----

(defun agent-repl-wire-decode-host-backfill-none (value)
  "Decode VALUE as the empty message `HostBackfillNone'."
  (agent-repl-wire--decode-empty "HostBackfillNone" value))

(defun agent-repl-wire-decode-host-backfill-pending (value)
  "Decode VALUE as the empty message `HostBackfillPending'."
  (agent-repl-wire--decode-empty "HostBackfillPending" value))

(defun agent-repl-wire-decode-host-backfill-done (value)
  "Decode VALUE as the empty message `HostBackfillDone'."
  (agent-repl-wire--decode-empty "HostBackfillDone" value))

(defun agent-repl-wire-decode-host-backfill-failed (value)
  "Decode VALUE as `HostBackfillFailed', a plist `(:detail)'."
  (let ((object (agent-repl-wire--object "HostBackfillFailed" value)))
    (agent-repl-wire--check-keys "HostBackfillFailed" object '(detail))
    (agent-repl-wire--decoded
     "HostBackfillFailed"
     (list :detail (agent-repl-wire--decode-string
                    "HostBackfillFailed" 'detail object)))))

(defun agent-repl-wire-decode-host-backfill-state (value)
  "Decode `HostBackfill''s `state' oneof from VALUE.
The never-blue signal: a live session whose history never arrived is not
ready to show."
  (let ((object (agent-repl-wire--object "HostBackfill" value)))
    (agent-repl-wire--check-keys "HostBackfill" object '(none pending done failed))
    (agent-repl-wire--decode-oneof
     "HostBackfill" 'state object
     '((none :none agent-repl-wire-decode-host-backfill-none)
       (pending :pending agent-repl-wire-decode-host-backfill-pending)
       (done :done agent-repl-wire-decode-host-backfill-done)
       (failed :failed agent-repl-wire-decode-host-backfill-failed)))))

(defun agent-repl-wire-decode-host-backfill (value)
  "Decode VALUE as `HostBackfill', the oneof plist `(:arm :value)'."
  (agent-repl-wire--decoded
   "HostBackfill" (agent-repl-wire-decode-host-backfill-state value)))

;;;; ---- The composer gate ----

(defun agent-repl-wire-decode-host-composer-open (value)
  "Decode VALUE as the empty message `HostComposerOpen'."
  (agent-repl-wire--decode-empty "HostComposerOpen" value))

(defun agent-repl-wire-decode-host-composer-merging (value)
  "Decode VALUE as the empty message `HostComposerMerging'."
  (agent-repl-wire--decode-empty "HostComposerMerging" value))

(defun agent-repl-wire-decode-host-composer-draining (value)
  "Decode VALUE as the empty message `HostComposerDraining'."
  (agent-repl-wire--decode-empty "HostComposerDraining" value))

(defun agent-repl-wire-decode-host-composer-restarting (value)
  "Decode VALUE as the empty message `HostComposerRestarting'."
  (agent-repl-wire--decode-empty "HostComposerRestarting" value))

(defun agent-repl-wire-decode-host-session-live-composer (value)
  "Decode `HostSessionLive''s `composer' oneof from VALUE.
THE ARM IS THE GATE, and it exists only on the LIVE standing — the other
standings are blocked by their own nature.  Emacs renders the arm verbatim
as a fixed treatment; it never maps a value."
  (let ((object (agent-repl-wire--object "HostSessionLive" value)))
    (agent-repl-wire--decode-oneof
     "HostSessionLive" 'composer object
     '((open :open agent-repl-wire-decode-host-composer-open)
       (merging :merging agent-repl-wire-decode-host-composer-merging)
       (draining :draining agent-repl-wire-decode-host-composer-draining)
       (restarting :restarting agent-repl-wire-decode-host-composer-restarting)))))

;;;; ---- HostSessionLive and the session axis ----

(defun agent-repl-wire-decode-host-session-live-vendor-info (value)
  "Decode `HostSessionLive''s `vendor_info' oneof from VALUE.
THE ARM IS THE VENDOR.  UNSET IS LEGAL here and only here on this message:
the proto says the oneof stays unset while no vendor conversation exists
yet, so an absent arm decodes to nil rather than raising."
  (let ((object (agent-repl-wire--object "HostSessionLive" value)))
    (agent-repl-wire--decode-oneof
     "HostSessionLive" 'vendor_info object
     '((claude :claude agent-repl-wire-decode-host-vendor-claude))
     t)))

(defun agent-repl-wire-decode-host-session-live-generation (value)
  "Decode `HostSessionLive''s `generation' field VALUE as a HostGenerationId."
  (agent-repl-wire-decode-host-generation-id value))

(defun agent-repl-wire-decode-host-session-live-backfill (value)
  "Decode `HostSessionLive''s `backfill' field VALUE as a HostBackfill."
  (agent-repl-wire-decode-host-backfill value))

(defun agent-repl-wire-decode-host-session-live-faults (value)
  "Decode one element of `HostSessionLive''s repeated `faults' field VALUE."
  (agent-repl-wire-decode-host-fault value))

(defun agent-repl-wire-decode-host-session-live (value)
  "Decode VALUE as `HostSessionLive'.
Returns `(:generation G :shim-attached BOOL :vendor-info ONEOF :backfill
ONEOF :composer ONEOF :faults LIST)'.  `shim_attached' false means live but
momentarily unwired — the daemon between shim starts — and has no
treatment of its own."
  (let ((object (agent-repl-wire--object "HostSessionLive" value)))
    (agent-repl-wire--check-keys
     "HostSessionLive" object
     '(generation shimAttached claude backfill
                  open merging draining restarting faults))
    (agent-repl-wire--decoded
     "HostSessionLive"
     (list :generation (agent-repl-wire--decode-message
                        "HostSessionLive" 'generation object
                        #'agent-repl-wire-decode-host-session-live-generation)
           :shim-attached (agent-repl-wire--decode-bool
                           "HostSessionLive" 'shimAttached object)
           :vendor-info (agent-repl-wire-decode-host-session-live-vendor-info object)
           :backfill (agent-repl-wire--decode-message
                      "HostSessionLive" 'backfill object
                      #'agent-repl-wire-decode-host-session-live-backfill)
           :composer (agent-repl-wire-decode-host-session-live-composer object)
           :faults (agent-repl-wire--decode-repeated
                    "HostSessionLive" 'faults object
                    #'agent-repl-wire-decode-host-session-live-faults)))))

(defun agent-repl-wire-decode-host-session-terminal (value)
  "Decode VALUE as `HostSessionTerminal', a plist `(:rehydratable)'.
False means reopening starts a fresh vendor conversation."
  (let ((object (agent-repl-wire--object "HostSessionTerminal" value)))
    (agent-repl-wire--check-keys "HostSessionTerminal" object '(rehydratable))
    (agent-repl-wire--decoded
     "HostSessionTerminal"
     (list :rehydratable (agent-repl-wire--decode-bool
                          "HostSessionTerminal" 'rehydratable object)))))

(defun agent-repl-wire-decode-host-session-existing-id (value)
  "Decode `HostSessionExisting''s `id' field VALUE as a HostSessionId."
  (agent-repl-wire-decode-host-session-id value))

(defun agent-repl-wire-decode-host-session-existing-standing (value)
  "Decode `HostSessionExisting''s `standing' oneof from the object VALUE."
  (agent-repl-wire--decode-oneof
   "HostSessionExisting" 'standing value
   '((live :live agent-repl-wire-decode-host-session-live)
     (terminal :terminal agent-repl-wire-decode-host-session-terminal))))

(defun agent-repl-wire-decode-host-session-existing (value)
  "Decode VALUE as `HostSessionExisting', a plist `(:id :standing)'.
The identity is hoisted OVER the standing oneof because it is common to
every standing."
  (let ((object (agent-repl-wire--object "HostSessionExisting" value)))
    (agent-repl-wire--check-keys "HostSessionExisting" object '(id live terminal))
    (agent-repl-wire--decoded
     "HostSessionExisting"
     (list :id (agent-repl-wire--decode-message
                "HostSessionExisting" 'id object
                #'agent-repl-wire-decode-host-session-existing-id)
           :standing (agent-repl-wire-decode-host-session-existing-standing object)))))

(defun agent-repl-wire-decode-host-session-none (value)
  "Decode VALUE as the empty message `HostSessionNone'."
  (agent-repl-wire--decode-empty "HostSessionNone" value))

(defun agent-repl-wire-decode-host-workspace-session (value)
  "Decode `HostWorkspace''s `session' oneof from the object VALUE.
THE ARM IS WHETHER A SESSION EXISTS."
  (agent-repl-wire--decode-oneof
   "HostWorkspace" 'session value
   '((none :none agent-repl-wire-decode-host-session-none)
     (existing :existing agent-repl-wire-decode-host-session-existing))))

(defun agent-repl-wire-decode-host-held-prompt-edit-turn (value)
  "Decode HostHeldPromptEdit's `turn' field VALUE as a TurnId."
  (agent-repl-wire-decode-turn-id value))

(defun agent-repl-wire-decode-host-held-prompt-edit-said (value)
  "Decode HostHeldPromptEdit's `said' field VALUE as a UserSaid."
  (agent-repl-wire-decode-user-said value))

(defun agent-repl-wire-decode-host-held-prompt-edit (value)
  "Decode VALUE as `HostHeldPromptEdit', a plist `(:turn :said :edit)'.
The held prompt this workspace's editor is editing: the turn a commit or
cancel names, the content as the edit began, and the daemon-minted EDIT id
that tells a new edit from the one the composer already took."
  (let ((object (agent-repl-wire--object "HostHeldPromptEdit" value)))
    (agent-repl-wire--check-keys "HostHeldPromptEdit" object '(turn said edit))
    (agent-repl-wire--decoded
     "HostHeldPromptEdit"
     (list :turn (agent-repl-wire--decode-message
                  "HostHeldPromptEdit" 'turn object
                  #'agent-repl-wire-decode-host-held-prompt-edit-turn)
           :said (agent-repl-wire--decode-message
                  "HostHeldPromptEdit" 'said object
                  #'agent-repl-wire-decode-host-held-prompt-edit-said)
           :edit (agent-repl-wire--decode-uint64 "HostHeldPromptEdit" 'edit object)))))

(defconst agent-repl-wire-host-gate-arms
  '((coldGate :cold-gate agent-repl-wire-decode-host-gate-cold-gate))
  "HostGate's `kind' arms: (WIRE-KEY ARM-KEYWORD DECODER).")

(defun agent-repl-wire-decode-host-gate-cold-gate (value)
  "Decode VALUE as the empty message `HostGateColdGate'."
  (agent-repl-wire--decode-empty "HostGateColdGate" value))

(defun agent-repl-wire-decode-host-gate (value)
  "Decode VALUE as `HostGate', a plist `(:arm :value)'.
The arm is the gate's kind.  SET-BUT-UNASSIGNED is legal: it is a gate this
build does not name, decoded as `(:arm nil :value nil)' so the gate's
presence still stands."
  (let ((object (agent-repl-wire--object "HostGate" value)))
    (agent-repl-wire--check-keys "HostGate" object
                                 (mapcar #'car agent-repl-wire-host-gate-arms))
    (agent-repl-wire--decoded
     "HostGate"
     (or (agent-repl-wire--decode-oneof "HostGate" 'kind object
                                        agent-repl-wire-host-gate-arms t)
         (list :arm nil :value nil)))))

;;;; ---- The standing ask ----

(defun agent-repl-wire-decode-host-ask-question-option (value)
  "Decode VALUE as `HostAskQuestionOption', a plist `(:label)'.
LABEL is the served option label, echoed verbatim by a pick."
  (let ((object (agent-repl-wire--object "HostAskQuestionOption" value)))
    (agent-repl-wire--check-keys "HostAskQuestionOption" object '(label))
    (agent-repl-wire--decoded
     "HostAskQuestionOption"
     (list :label (agent-repl-wire--decode-string "HostAskQuestionOption" 'label object)))))

(defun agent-repl-wire--decode-host-ask-options (message-name value)
  "Decode VALUE as MESSAGE-NAME, a message whose one field is `options'.
Both selection arms of `HostAskQuestion' carry exactly that shape, so the
plist is `(:options OPTIONS)' for either, OPTIONS in the served order."
  (let ((object (agent-repl-wire--object message-name value)))
    (agent-repl-wire--check-keys message-name object '(options))
    (agent-repl-wire--decoded
     message-name
     (list :options (agent-repl-wire--decode-repeated
                     message-name 'options object
                     #'agent-repl-wire-decode-host-ask-question-option)))))

(defun agent-repl-wire-decode-host-ask-question-single-select (value)
  "Decode VALUE as `HostAskQuestionSingleSelect', a plist `(:options)'."
  (agent-repl-wire--decode-host-ask-options "HostAskQuestionSingleSelect" value))

(defun agent-repl-wire-decode-host-ask-question-multi-select (value)
  "Decode VALUE as `HostAskQuestionMultiSelect', a plist `(:options)'."
  (agent-repl-wire--decode-host-ask-options "HostAskQuestionMultiSelect" value))

(defun agent-repl-wire-decode-host-ask-question (value)
  "Decode VALUE as `HostAskQuestion', a plist `(:question-text :selection)'.
SELECTION is `(:arm :single-select|:multi-select :value (:options))'; an
unset selection is a contract breach."
  (let ((object (agent-repl-wire--object "HostAskQuestion" value)))
    (agent-repl-wire--check-keys "HostAskQuestion" object
                                 '(questionText singleSelect multiSelect))
    (agent-repl-wire--decoded
     "HostAskQuestion"
     (list :question-text (agent-repl-wire--decode-string
                           "HostAskQuestion" 'questionText object)
           :selection (agent-repl-wire--decode-oneof
                       "HostAskQuestion" 'selection object
                       '((singleSelect :single-select
                                       agent-repl-wire-decode-host-ask-question-single-select)
                         (multiSelect :multi-select
                                      agent-repl-wire-decode-host-ask-question-multi-select)))))))

(defun agent-repl-wire-decode-host-ask-permission-standing-offered (value)
  "Decode VALUE as the empty message `HostAskPermissionStandingOffered'."
  (agent-repl-wire--decode-empty "HostAskPermissionStandingOffered" value))

(defun agent-repl-wire-decode-host-ask-permission (value)
  "Decode VALUE as `HostAskPermission', a plist `(:standing-offered)'.
STANDING-OFFERED is t when the field is present: presence is the fact."
  (let ((object (agent-repl-wire--object "HostAskPermission" value)))
    (agent-repl-wire--check-keys "HostAskPermission" object '(standingOffered))
    (agent-repl-wire--decode-optional-message
     "HostAskPermission" 'standingOffered object
     #'agent-repl-wire-decode-host-ask-permission-standing-offered)
    (agent-repl-wire--decoded
     "HostAskPermission"
     (list :standing-offered (and (agent-repl-wire--present-p object 'standingOffered) t)))))

(defun agent-repl-wire-decode-host-ask (value)
  "Decode VALUE as `HostAsk', a plist `(:row :kind)'.
ROW is the card's `FeedId' plist, echoed by the answer verbs.  KIND is
`(:arm :question|:permission :value V)'; an unset kind is a contract
breach."
  (let ((object (agent-repl-wire--object "HostAsk" value)))
    (agent-repl-wire--check-keys "HostAsk" object '(row question permission))
    (agent-repl-wire--decoded
     "HostAsk"
     (list :row (agent-repl-wire--decode-message
                 "HostAsk" 'row object #'agent-repl-wire-decode-feed-id)
           :kind (agent-repl-wire--decode-oneof
                  "HostAsk" 'kind object
                  '((question :question agent-repl-wire-decode-host-ask-question)
                    (permission :permission agent-repl-wire-decode-host-ask-permission)))))))

(defun agent-repl-wire-decode-host-workspace (value)
  "Decode VALUE as `HostWorkspace'.
The plist is `(:session :naming :held-prompt-edit :gate :ask
:composer-entries)'.
`naming' is a REQUIRED message sitting beside the session oneof; its two
fields are the optional halves, not the message.  `held_prompt_edit' is
absent when no held-prompt edit stands, and its absence is that fact.
`gate' is absent when no gate stands, likewise, and `ask' when no question
or permission card stands on the root feed.  `composer_entries' is always
set: the composer's labels that answer nothing."
  (let ((object (agent-repl-wire--object "HostWorkspace" value)))
    (agent-repl-wire--check-keys "HostWorkspace" object
                                 '(none existing naming heldPromptEdit gate ask composerEntries))
    (agent-repl-wire--decoded
     "HostWorkspace"
     (list :session (agent-repl-wire-decode-host-workspace-session object)
           :naming (agent-repl-wire--decode-message
                    "HostWorkspace" 'naming object
                    #'agent-repl-wire-decode-host-workspace-naming)
           :held-prompt-edit (agent-repl-wire--decode-optional-message
                              "HostWorkspace" 'heldPromptEdit object
                              #'agent-repl-wire-decode-host-held-prompt-edit)
           :gate (agent-repl-wire--decode-optional-message
                  "HostWorkspace" 'gate object
                  #'agent-repl-wire-decode-host-gate)
           :ask (agent-repl-wire--decode-optional-message
                 "HostWorkspace" 'ask object
                 #'agent-repl-wire-decode-host-ask)
           :composer-entries (agent-repl-wire--decode-message
                              "HostWorkspace" 'composerEntries object
                              #'agent-repl-wire-decode-host-composer-entries)))))

(defun agent-repl-wire-decode-host-composer-entries (value)
  "Decode VALUE as `HostComposerEntries', the plist (:unanswered LABELS).
LABELS are the composer's entry labels that answer nothing, in order; an
absent field is none."
  (let ((object (agent-repl-wire--object "HostComposerEntries" value)))
    (agent-repl-wire--check-keys "HostComposerEntries" object '(unanswered))
    (agent-repl-wire--decoded
     "HostComposerEntries"
     (list :unanswered (agent-repl-wire--decode-repeated
                        "HostComposerEntries" 'unanswered object
                        (lambda (label)
                          (if (stringp label)
                              label
                            (agent-repl-wire--fail "HostComposerEntries" 'unanswered
                                                   "an element is not a string"))))))))

;;;; ---- The notification click push ----

(defun agent-repl-wire-decode-host-workspace-notification-clicked (value)
  "Decode VALUE as the empty message `HostWorkspaceNotificationClicked'.
The user clicked this workspace's desktop banner; the daemon posted it
and read the click back, so selecting the tab is all that is left."
  (agent-repl-wire--decode-empty "HostWorkspaceNotificationClicked" value))

;;;; ---- The final-response click push ----

(defun agent-repl-wire-decode-host-workspace-final-response-clicked (value)
  "Decode VALUE as the empty message `HostWorkspaceFinalResponseClicked'.
The reader clicked a final response in the webview, selecting it, so
focusing the composer for the reply is all that is left."
  (agent-repl-wire--decode-empty "HostWorkspaceFinalResponseClicked" value))

;;;; ---- The rail's row click push ----

(defun agent-repl-wire-decode-host-workspace-select-requested (value)
  "Decode VALUE as the empty message `HostWorkspaceSelectRequested'.
The user clicked this workspace's row in a page's rail; the daemon
relayed the click, so requesting the switch to it is all that is left."
  (agent-repl-wire--decode-empty "HostWorkspaceSelectRequested" value))

;;;; ---- The prompt-entry click push ----

(defun agent-repl-wire-decode-host-workspace-prompt-entry-clicked (value)
  "Decode VALUE as `HostWorkspacePromptEntryClicked', (:row ROW :label L :text T).
The reader clicked an entry of a prompt in the webview, selecting the
prompt; the composer is filled with TEXT to edit the entry answering LABEL.
The row is REQUIRED: it is the prompt the edit resends."
  (let ((object (agent-repl-wire--object "HostWorkspacePromptEntryClicked" value)))
    (agent-repl-wire--check-keys "HostWorkspacePromptEntryClicked" object '(row label text))
    (agent-repl-wire--decoded
     "HostWorkspacePromptEntryClicked"
     (list :row (agent-repl-wire--decode-message
                 "HostWorkspacePromptEntryClicked" 'row object
                 #'agent-repl-wire-decode-feed-id)
           :label (agent-repl-wire--decode-string "HostWorkspacePromptEntryClicked" 'label object)
           :text (agent-repl-wire--decode-string "HostWorkspacePromptEntryClicked" 'text object)))))

;;;; ---- The selection push ----

(defun agent-repl-wire-decode-host-workspace-selection (value)
  "Decode VALUE as `HostWorkspaceSelection', a plist `(:kind :markdown)'.
KIND is `:none', `:response', `:prompt' or `:bubble': which kind of row the
feed has selected, never the row, because Emacs never names it back.
MARKDOWN is the selected bubble's text, which the composer's `/' and `?'
search, and nil exactly when KIND is `:none'.  An unset or doubled oneof is
a contract breach."
  (let ((object (agent-repl-wire--object "HostWorkspaceSelection" value)))
    (agent-repl-wire--check-keys "HostWorkspaceSelection" object '(none response prompt bubble))
    (let ((selection
           (agent-repl-wire--decoded
            "HostWorkspaceSelection"
            (agent-repl-wire--decode-oneof
             "HostWorkspaceSelection" 'selection object
             '((none :none agent-repl-wire-decode-host-workspace-selection-none)
               (response :response agent-repl-wire-decode-host-workspace-selection-response)
               (prompt :prompt agent-repl-wire-decode-host-workspace-selection-prompt)
               (bubble :bubble agent-repl-wire-decode-host-workspace-selection-bubble))))))
      (list :kind (plist-get selection :arm)
            :markdown (plist-get selection :value)))))

(defun agent-repl-wire-decode-host-workspace-selection-none (value)
  "Decode VALUE as the empty message `HostWorkspaceSelectionNone'."
  (agent-repl-wire--decode-empty "HostWorkspaceSelectionNone" value))

(defun agent-repl-wire--decode-host-workspace-selection-text (message-name value)
  "Decode VALUE as the selected-arm MESSAGE-NAME, answering its text.
Every selected arm carries the bubble's text in a REQUIRED `markdown'
element; the text itself may be empty (a prompt that is an image alone)."
  (let ((object (agent-repl-wire--object message-name value)))
    (agent-repl-wire--check-keys message-name object '(markdown))
    (agent-repl-wire--decode-message
     message-name 'markdown object
     #'agent-repl-wire-decode-host-workspace-selection-markdown)))

(defun agent-repl-wire-decode-host-workspace-selection-markdown (value)
  "Decode VALUE as `HostWorkspaceSelectionMarkdown', answering its text."
  (let ((object (agent-repl-wire--object "HostWorkspaceSelectionMarkdown" value)))
    (agent-repl-wire--check-keys "HostWorkspaceSelectionMarkdown" object '(text))
    (agent-repl-wire--decode-string "HostWorkspaceSelectionMarkdown" 'text object)))

(defun agent-repl-wire-decode-host-workspace-selection-response (value)
  "Decode VALUE as `HostWorkspaceSelectionResponse', answering its text."
  (agent-repl-wire--decode-host-workspace-selection-text "HostWorkspaceSelectionResponse" value))

(defun agent-repl-wire-decode-host-workspace-selection-prompt (value)
  "Decode VALUE as `HostWorkspaceSelectionPrompt', answering its text."
  (agent-repl-wire--decode-host-workspace-selection-text "HostWorkspaceSelectionPrompt" value))

(defun agent-repl-wire-decode-host-workspace-selection-bubble (value)
  "Decode VALUE as `HostWorkspaceSelectionBubble', answering its text."
  (agent-repl-wire--decode-host-workspace-selection-text "HostWorkspaceSelectionBubble" value))

;;;; ---- The remaining WatchHostWorkspace push arms ----

(defun agent-repl-wire-decode-host-workspace-transferred (value)
  "Decode VALUE as the empty message `HostWorkspaceTransferred'."
  (agent-repl-wire--decode-empty "HostWorkspaceTransferred" value))

(defun agent-repl-wire-decode-host-workspace-reload-webapp (value)
  "Decode VALUE as `HostWorkspaceReloadWebapp', a plist `(:webapp-build)'.
`webapp_build' is OPTIONAL: unset means a reload of the workspace's page
alone, on the build it runs."
  (let ((object (agent-repl-wire--object "HostWorkspaceReloadWebapp" value)))
    (agent-repl-wire--check-keys "HostWorkspaceReloadWebapp" object '(webappBuild))
    (agent-repl-wire--decoded
     "HostWorkspaceReloadWebapp"
     (list :webapp-build (agent-repl-wire--decode-optional-string
                          "HostWorkspaceReloadWebapp" 'webappBuild object)))))

(defun agent-repl-wire-decode-host-open-in-editor (value)
  "Decode VALUE as `HostOpenInEditor', a plist `(:path :line)'.
`line' is 1-indexed and OPTIONAL: unset means the file's top, or a
directory."
  (let ((object (agent-repl-wire--object "HostOpenInEditor" value)))
    (agent-repl-wire--check-keys "HostOpenInEditor" object '(path line))
    (agent-repl-wire--decoded
     "HostOpenInEditor"
     (list :path (agent-repl-wire--decode-string "HostOpenInEditor" 'path object)
           :line (agent-repl-wire--decode-optional-uint32
                  "HostOpenInEditor" 'line object)))))

(defun agent-repl-wire-decode-open-in-editor-commit-log (value)
  "Decode VALUE as `OpenInEditorCommitLog'.
The plist is `(:repository-dir :branch :commit)'.  Every field is
required: the daemon relays a target it validated."
  (let ((object (agent-repl-wire--object "OpenInEditorCommitLog" value)))
    (agent-repl-wire--check-keys "OpenInEditorCommitLog" object '(repositoryDir branch commit))
    (agent-repl-wire--decoded
     "OpenInEditorCommitLog"
     (list :repository-dir (agent-repl-wire--decode-string "OpenInEditorCommitLog" 'repositoryDir object)
           :branch (agent-repl-wire--decode-string "OpenInEditorCommitLog" 'branch object)
           :commit (agent-repl-wire--decode-string "OpenInEditorCommitLog" 'commit object)))))

;;;; ---- Editor evaluation: the `evaluate' push and its answer ----
;;
;; editor_evaluation.proto.  The daemon pushes an agent's elisp on the host
;; stream with a token; the editor evaluates it in its one handler
;; (evaluate.el) and answers AnswerEditorEvaluation with the token, unchanged.

(defun agent-repl-wire-decode-editor-evaluation-token (value)
  "Decode VALUE as `EditorEvaluationToken', a plist (:value STRING).
The token is opaque to the editor, which only echoes it back."
  (let ((object (agent-repl-wire--object "EditorEvaluationToken" value)))
    (agent-repl-wire--check-keys "EditorEvaluationToken" object '(value))
    (agent-repl-wire--decoded
     "EditorEvaluationToken"
     (list :value (agent-repl-wire--decode-string "EditorEvaluationToken" 'value object)))))

(defun agent-repl-wire-encode-editor-evaluation-token (value)
  "Encode the `EditorEvaluationToken' plist VALUE (:value STRING)."
  (list (cons 'value (agent-repl-wire--encode-string
                      "EditorEvaluationToken" 'value (plist-get value :value)))))

(defun agent-repl-wire-decode-host-evaluate (value)
  "Decode VALUE as `HostEvaluate', a plist (:token TOKEN :text TEXT).
The token is REQUIRED: an evaluation that cannot be answered is a breach."
  (let ((object (agent-repl-wire--object "HostEvaluate" value)))
    (agent-repl-wire--check-keys "HostEvaluate" object '(token text))
    (agent-repl-wire--decoded
     "HostEvaluate"
     (list :token (agent-repl-wire--decode-message
                   "HostEvaluate" 'token object
                   #'agent-repl-wire-decode-editor-evaluation-token)
           :text (agent-repl-wire--decode-string "HostEvaluate" 'text object)))))

(defun agent-repl-wire-encode-editor-evaluation-value (value)
  "Encode the `EditorEvaluationValue' plist VALUE (:printed P :readable B).
An unreadable value omits `readable', its proto3 default."
  (append
   (list (cons 'printed (agent-repl-wire--encode-string
                         "EditorEvaluationValue" 'printed (plist-get value :printed))))
   (when (plist-get value :readable)
     (list (cons 'readable t)))))

(defun agent-repl-wire-encode-editor-evaluation-read-error (value)
  "Encode the `EditorEvaluationReadError' plist VALUE (:message M)."
  (list (cons 'message (agent-repl-wire--encode-string
                        "EditorEvaluationReadError" 'message (plist-get value :message)))))

(defun agent-repl-wire-encode-editor-evaluation-eval-error (value)
  "Encode the `EditorEvaluationEvalError' plist VALUE.
VALUE is (:error-symbol S :data-printed D :message M), each a string."
  (let ((message "EditorEvaluationEvalError"))
    (list (cons 'errorSymbol (agent-repl-wire--encode-string
                              message 'errorSymbol (plist-get value :error-symbol)))
          (cons 'dataPrinted (agent-repl-wire--encode-string
                              message 'dataPrinted (plist-get value :data-printed)))
          (cons 'message (agent-repl-wire--encode-string
                          message 'message (plist-get value :message))))))

(defun agent-repl-wire-encode-editor-evaluation-outcome (value)
  "Encode the `EditorEvaluationOutcome' plist VALUE.
VALUE is (:outcome ONEOF :messages LINES).
ONEOF's arm is `:value', `:read-error' or `:eval-error'; it is required.
LINES are the `*Messages*' lines the evaluation added, oldest first."
  (let ((message "EditorEvaluationOutcome")
        (lines (plist-get value :messages)))
    (append
     (agent-repl-wire--encode-oneof
      message 'outcome (plist-get value :outcome)
      '((:value value agent-repl-wire-encode-editor-evaluation-value)
        (:read-error readError agent-repl-wire-encode-editor-evaluation-read-error)
        (:eval-error evalError agent-repl-wire-encode-editor-evaluation-eval-error)))
     (when lines
       (list (cons 'messages
                   (agent-repl-wire--encode-repeated
                    message 'messages lines
                    (lambda (line) (agent-repl-wire--encode-string message 'messages line)))))))))

;;;; ---- AnswerEditorEvaluation ----

(defun agent-repl-wire-encode-answer-editor-evaluation-request (value)
  "Encode the AnswerEditorEvaluationRequest plist VALUE.
VALUE is (:workspace REF :token TOKEN :outcome OUTCOME), all required."
  (let ((message "AnswerEditorEvaluationRequest"))
    (dolist (field '((:workspace . workspace) (:token . token) (:outcome . outcome)))
      (unless (plist-get value (car field))
        (agent-repl-wire--fail message (cdr field) "required message field is absent")))
    (agent-repl-wire--encoded
     message
     (list (cons 'workspace (agent-repl-wire-encode-workspace-ref (plist-get value :workspace)))
           (cons 'token (agent-repl-wire-encode-editor-evaluation-token (plist-get value :token)))
           (cons 'outcome (agent-repl-wire-encode-editor-evaluation-outcome
                           (plist-get value :outcome)))))))

(defun agent-repl-wire-decode-answer-editor-evaluation-success (value)
  "Decode VALUE as the empty `AnswerEditorEvaluationSuccess'."
  (agent-repl-wire--decode-empty "AnswerEditorEvaluationSuccess" value))

(defun agent-repl-wire-decode-answer-editor-evaluation-unknown-token (value)
  "Decode VALUE as the empty `AnswerEditorEvaluationUnknownToken'."
  (agent-repl-wire--decode-empty "AnswerEditorEvaluationUnknownToken" value))

(defun agent-repl-wire-decode-answer-editor-evaluation-error (value)
  "Decode VALUE as `AnswerEditorEvaluationError', a plist (:cause ONEOF)."
  (let ((object (agent-repl-wire--object "AnswerEditorEvaluationError" value)))
    (agent-repl-wire--check-keys "AnswerEditorEvaluationError" object '(unknownToken))
    (agent-repl-wire--decoded
     "AnswerEditorEvaluationError"
     (list :cause (agent-repl-wire--decode-oneof
                   "AnswerEditorEvaluationError" 'cause object
                   '((unknownToken :unknown-token
                                   agent-repl-wire-decode-answer-editor-evaluation-unknown-token)))))))

(defun agent-repl-wire-decode-answer-editor-evaluation-response (value)
  "Decode VALUE as `AnswerEditorEvaluationResponse'.  THE ARM IS THE OUTCOME."
  (let ((object (agent-repl-wire--object "AnswerEditorEvaluationResponse" value)))
    (agent-repl-wire--check-keys "AnswerEditorEvaluationResponse" object '(success error))
    (agent-repl-wire--decoded
     "AnswerEditorEvaluationResponse"
     (agent-repl-wire--decode-oneof
      "AnswerEditorEvaluationResponse" 'result object
      '((success :success agent-repl-wire-decode-answer-editor-evaluation-success)
        (error :error agent-repl-wire-decode-answer-editor-evaluation-error))))))

;;;; ---- WatchHostWorkspace ----

(defun agent-repl-wire-encode-watch-host-workspace-request-workspace (value)
  "Encode `WatchHostWorkspaceRequest''s `workspace' field VALUE."
  (agent-repl-wire-encode-workspace-ref value))

(defun agent-repl-wire-encode-watch-host-workspace-request (value)
  "Encode the WatchHostWorkspaceRequest plist VALUE `(:workspace REF)'."
  (unless (plist-member value :workspace)
    (agent-repl-wire--fail "WatchHostWorkspaceRequest" 'workspace
                           "required message field is absent"))
  (agent-repl-wire--encoded
   "WatchHostWorkspaceRequest"
   (list (cons 'workspace
               (agent-repl-wire-encode-watch-host-workspace-request-workspace
                (plist-get value :workspace))))))

(defun agent-repl-wire-decode-watch-host-workspace-response-host (value)
  "Decode the `host' push arm VALUE as a HostWorkspace."
  (agent-repl-wire-decode-host-workspace value))

(defun agent-repl-wire-decode-watch-host-workspace-response-notification-clicked (value)
  "Decode the `notification_clicked' push arm VALUE."
  (agent-repl-wire-decode-host-workspace-notification-clicked value))

(defun agent-repl-wire-decode-watch-host-workspace-response-final-response-clicked (value)
  "Decode the `final_response_clicked' push arm VALUE."
  (agent-repl-wire-decode-host-workspace-final-response-clicked value))

(defun agent-repl-wire-decode-watch-host-workspace-response-select-requested (value)
  "Decode the `select_requested' push arm VALUE."
  (agent-repl-wire-decode-host-workspace-select-requested value))

(defun agent-repl-wire-decode-watch-host-workspace-response-prompt-entry-clicked (value)
  "Decode the `prompt_entry_clicked' push arm VALUE."
  (agent-repl-wire-decode-host-workspace-prompt-entry-clicked value))

(defun agent-repl-wire-decode-watch-host-workspace-response-selection (value)
  "Decode the `selection' push arm VALUE as its `(:kind :markdown)' plist."
  (agent-repl-wire-decode-host-workspace-selection value))

(defun agent-repl-wire-decode-watch-host-workspace-response-transferred (value)
  "Decode the `transferred' push arm VALUE."
  (agent-repl-wire-decode-host-workspace-transferred value))

(defun agent-repl-wire-decode-watch-host-workspace-response-reload-webapp (value)
  "Decode the `reload_webapp' push arm VALUE."
  (agent-repl-wire-decode-host-workspace-reload-webapp value))

(defun agent-repl-wire-decode-watch-host-workspace-response-open-in-editor (value)
  "Decode the `open_in_editor' push arm VALUE."
  (agent-repl-wire-decode-host-open-in-editor value))

(defun agent-repl-wire-decode-watch-host-workspace-response-open-commit-log (value)
  "Decode the `open_commit_log' push arm VALUE."
  (agent-repl-wire-decode-open-in-editor-commit-log value))

(defun agent-repl-wire-decode-watch-host-workspace-response-evaluate (value)
  "Decode the `evaluate' push arm VALUE as its `(:token :text)' plist."
  (agent-repl-wire-decode-host-evaluate value))

(defun agent-repl-wire-decode-watch-host-workspace-response-ending (value)
  "Decode the `ending' push arm VALUE."
  (agent-repl-wire-decode-daemon-stream-ending value))

(defun agent-repl-wire-decode-watch-host-workspace-response-push (value)
  "Decode `WatchHostWorkspaceResponse''s `push' oneof from the object VALUE."
  (agent-repl-wire--decode-oneof
   "WatchHostWorkspaceResponse" 'push value
   '((host :host agent-repl-wire-decode-watch-host-workspace-response-host)
     (transferred :transferred
                  agent-repl-wire-decode-watch-host-workspace-response-transferred)
     (reloadWebapp :reload-webapp
                   agent-repl-wire-decode-watch-host-workspace-response-reload-webapp)
     (openInEditor :open-in-editor
                   agent-repl-wire-decode-watch-host-workspace-response-open-in-editor)
     (openCommitLog :open-commit-log
                    agent-repl-wire-decode-watch-host-workspace-response-open-commit-log)
     (ending :ending agent-repl-wire-decode-watch-host-workspace-response-ending)
     (notificationClicked :notification-clicked
                          agent-repl-wire-decode-watch-host-workspace-response-notification-clicked)
     (selection :selection agent-repl-wire-decode-watch-host-workspace-response-selection)
     (evaluate :evaluate agent-repl-wire-decode-watch-host-workspace-response-evaluate)
     (finalResponseClicked :final-response-clicked
                           agent-repl-wire-decode-watch-host-workspace-response-final-response-clicked)
     (selectRequested :select-requested
                      agent-repl-wire-decode-watch-host-workspace-response-select-requested)
     (promptEntryClicked :prompt-entry-clicked
                         agent-repl-wire-decode-watch-host-workspace-response-prompt-entry-clicked))))

(defun agent-repl-wire-decode-watch-host-workspace-response (value)
  "Decode VALUE as `WatchHostWorkspaceResponse', the push oneof plist."
  (let ((object (agent-repl-wire--object "WatchHostWorkspaceResponse" value)))
    (agent-repl-wire--check-keys
     "WatchHostWorkspaceResponse" object
     '(host transferred reloadWebapp openInEditor openCommitLog ending notificationClicked selection evaluate
       finalResponseClicked selectRequested promptEntryClicked))
    (agent-repl-wire--decoded
     "WatchHostWorkspaceResponse"
     (agent-repl-wire-decode-watch-host-workspace-response-push object))))

;;;; ---- RegisterWorkspace ----

(defun agent-repl-wire-encode-register-workspace-request (value)
  "Encode the RegisterWorkspaceRequest plist VALUE `(:dir)'.
A PATH, not an identity: the daemon normalizes it and mints the identity."
  (agent-repl-wire--encoded
   "RegisterWorkspaceRequest"
   (list (cons 'dir (agent-repl-wire--encode-string
                     "RegisterWorkspaceRequest" 'dir (plist-get value :dir))))))

(defun agent-repl-wire-decode-register-workspace-success-workspace (value)
  "Decode `RegisterWorkspaceSuccess''s `workspace' field VALUE."
  (agent-repl-wire-decode-workspace-ref value))

(defun agent-repl-wire-decode-register-workspace-success (value)
  "Decode VALUE as `RegisterWorkspaceSuccess', a plist `(:workspace REF)'."
  (let ((object (agent-repl-wire--object "RegisterWorkspaceSuccess" value)))
    (agent-repl-wire--check-keys "RegisterWorkspaceSuccess" object '(workspace))
    (agent-repl-wire--decoded
     "RegisterWorkspaceSuccess"
     (list :workspace (agent-repl-wire--decode-message
                       "RegisterWorkspaceSuccess" 'workspace object
                       #'agent-repl-wire-decode-register-workspace-success-workspace)))))

(defun agent-repl-wire-decode-register-workspace-not-a-worktree (value)
  "Decode VALUE as the empty message `RegisterWorkspaceNotAWorktree'.
The dir exists but is not a git worktree the daemon can adopt."
  (agent-repl-wire--decode-empty "RegisterWorkspaceNotAWorktree" value))

(defun agent-repl-wire-decode-register-workspace-error-not-a-worktree (value)
  "Decode RegisterWorkspaceError's `not_a_worktree' cause arm from VALUE."
  (agent-repl-wire-decode-register-workspace-not-a-worktree value))

(defun agent-repl-wire-decode-register-workspace-error-inside-temporary-directory (value)
  "Decode RegisterWorkspaceError's `inside_temporary_directory' cause arm.
The dir lies inside a temporary directory, which agent-repl does not
register; VALUE carries the canonical `dir' and `temporary_root'."
  (agent-repl-wire-decode-inside-temporary-directory
   "RegisterWorkspaceInsideTemporaryDirectory" value))

(defun agent-repl-wire-decode-register-workspace-error-cause (value)
  "Decode RegisterWorkspaceError's `cause' oneof from the object VALUE.
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((object (agent-repl-wire--object "RegisterWorkspaceError" value)))
    (agent-repl-wire--check-keys "RegisterWorkspaceError" object '(notAWorktree insideTemporaryDirectory parentNotOpen))
    (agent-repl-wire--decode-oneof
     "RegisterWorkspaceError" 'cause object
     '((notAWorktree :not-a-worktree agent-repl-wire-decode-register-workspace-error-not-a-worktree)
       (insideTemporaryDirectory :inside-temporary-directory
                                 agent-repl-wire-decode-register-workspace-error-inside-temporary-directory)
       (parentNotOpen :parent-not-open agent-repl-wire-decode-workspace-parent-not-open)))))

(defun agent-repl-wire-decode-register-workspace-error (value)
  "Decode VALUE as `RegisterWorkspaceError', a plist (:cause ONEOF)."
  (agent-repl-wire--decoded
   "RegisterWorkspaceError"
   (list :cause (agent-repl-wire-decode-register-workspace-error-cause value))))

(defun agent-repl-wire-decode-register-workspace-response-result (value)
  "Decode `RegisterWorkspaceResponse''s `result' oneof from the object VALUE."
  (agent-repl-wire--decode-oneof
   "RegisterWorkspaceResponse" 'result value
   '((success :success agent-repl-wire-decode-register-workspace-success)
     (error :error agent-repl-wire-decode-register-workspace-error))))

(defun agent-repl-wire-decode-register-workspace-response (value)
  "Decode VALUE as `RegisterWorkspaceResponse'.  THE ARM IS THE OUTCOME."
  (let ((object (agent-repl-wire--object "RegisterWorkspaceResponse" value)))
    (agent-repl-wire--check-keys "RegisterWorkspaceResponse" object '(success error))
    (agent-repl-wire--decoded
     "RegisterWorkspaceResponse"
     (agent-repl-wire-decode-register-workspace-response-result object))))

;;;; ---- SelectWorkspace ----

(defun agent-repl-wire-encode-select-workspace-request-workspace (value)
  "Encode `SelectWorkspaceRequest''s `workspace' field VALUE."
  (agent-repl-wire-encode-workspace-ref value))

(defun agent-repl-wire-encode-select-workspace-request (value)
  "Encode the SelectWorkspaceRequest plist VALUE.
VALUE is `(:workspace REF :keep-selection-history BOOL)'.  A true
`keep_selection_history' marks a step of the recency walk; false is
proto3's default and is omitted, so every other select is spelled as before."
  (unless (plist-member value :workspace)
    (agent-repl-wire--fail "SelectWorkspaceRequest" 'workspace
                           "required message field is absent"))
  (agent-repl-wire--encoded
   "SelectWorkspaceRequest"
   (append
    (list (cons 'workspace
                (agent-repl-wire-encode-select-workspace-request-workspace
                 (plist-get value :workspace))))
    (when (plist-get value :keep-selection-history)
      (list (cons 'keepSelectionHistory t))))))

(defun agent-repl-wire-decode-select-workspace-success (value)
  "Decode VALUE as `SelectWorkspaceSuccess', `(:roster-revision N)'.
N is the roster revision the selection is in; a revision the wire omits is
zero, as proto3 writes it."
  (let ((object (agent-repl-wire--object "SelectWorkspaceSuccess" value)))
    (agent-repl-wire--check-keys "SelectWorkspaceSuccess" object '(rosterRevision))
    (list :roster-revision (or (agent-repl-wire--decode-uint64
                                "SelectWorkspaceSuccess" 'rosterRevision object)
                               0))))

(defun agent-repl-wire-decode-select-workspace-unknown-workspace (value)
  "Decode VALUE as the empty message `SelectWorkspaceUnknownWorkspace'.
The workspace id is not in the daemon's registry."
  (agent-repl-wire--decode-empty "SelectWorkspaceUnknownWorkspace" value))

(defun agent-repl-wire-decode-select-workspace-workspace-ref-mismatch (value)
  "Decode VALUE as `SelectWorkspaceWorkspaceRefMismatch', a plist (`:registry-
dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((object (agent-repl-wire--object "SelectWorkspaceWorkspaceRefMismatch" value)))
    (agent-repl-wire--check-keys "SelectWorkspaceWorkspaceRefMismatch" object '(registryDir))
    (agent-repl-wire--decoded
     "SelectWorkspaceWorkspaceRefMismatch"
     (list :registry-dir (agent-repl-wire--decode-string
                    "SelectWorkspaceWorkspaceRefMismatch" 'registryDir object)))))

(defun agent-repl-wire-decode-select-workspace-transferring-away (value)
  "Decode VALUE as `SelectWorkspaceTransferringAway', a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((object (agent-repl-wire--object "SelectWorkspaceTransferringAway" value)))
    (agent-repl-wire--check-keys "SelectWorkspaceTransferringAway" object '(address))
    (agent-repl-wire--decoded
     "SelectWorkspaceTransferringAway"
     (list :address (agent-repl-wire--decode-string
                    "SelectWorkspaceTransferringAway" 'address object)))))

(defun agent-repl-wire-decode-select-workspace-not-yet-adopted (value)
  "Decode VALUE as the empty message `SelectWorkspaceNotYetAdopted'.
A joining daemon has not finished adopting this workspace yet."
  (agent-repl-wire--decode-empty "SelectWorkspaceNotYetAdopted" value))

(defun agent-repl-wire-decode-select-workspace-standing-down (value)
  "Decode VALUE as the empty message `SelectWorkspaceStandingDown'.
This daemon is standing down and starts no session; the selection is
recorded and the client re-asserts it on the daemon that serves next."
  (agent-repl-wire--decode-empty "SelectWorkspaceStandingDown" value))

(defun agent-repl-wire-decode-select-workspace-error-standing-down (value)
  "Decode SelectWorkspaceError's `standing_down' cause arm from VALUE."
  (agent-repl-wire-decode-select-workspace-standing-down value))

(defun agent-repl-wire-decode-select-workspace-error-unknown-workspace (value)
  "Decode SelectWorkspaceError's `unknown_workspace' cause arm from VALUE."
  (agent-repl-wire-decode-select-workspace-unknown-workspace value))

(defun agent-repl-wire-decode-select-workspace-error-workspace-ref-mismatch (value)
  "Decode SelectWorkspaceError's `workspace_ref_mismatch' cause arm from VALUE."
  (agent-repl-wire-decode-select-workspace-workspace-ref-mismatch value))

(defun agent-repl-wire-decode-select-workspace-error-transferring-away (value)
  "Decode SelectWorkspaceError's `transferring_away' cause arm from VALUE."
  (agent-repl-wire-decode-select-workspace-transferring-away value))

(defun agent-repl-wire-decode-select-workspace-error-not-yet-adopted (value)
  "Decode SelectWorkspaceError's `not_yet_adopted' cause arm from VALUE."
  (agent-repl-wire-decode-select-workspace-not-yet-adopted value))

(defun agent-repl-wire-decode-select-workspace-error-cause (value)
  "Decode SelectWorkspaceError's `cause' oneof from the object VALUE.
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((object (agent-repl-wire--object "SelectWorkspaceError" value)))
    (agent-repl-wire--check-keys "SelectWorkspaceError" object '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted standingDown))
    (agent-repl-wire--decode-oneof
     "SelectWorkspaceError" 'cause object
     '((unknownWorkspace :unknown-workspace agent-repl-wire-decode-select-workspace-error-unknown-workspace)
       (workspaceRefMismatch :workspace-ref-mismatch agent-repl-wire-decode-select-workspace-error-workspace-ref-mismatch)
       (transferringAway :transferring-away agent-repl-wire-decode-select-workspace-error-transferring-away)
       (notYetAdopted :not-yet-adopted agent-repl-wire-decode-select-workspace-error-not-yet-adopted)
       (standingDown :standing-down agent-repl-wire-decode-select-workspace-error-standing-down)))))

(defun agent-repl-wire-decode-select-workspace-error (value)
  "Decode VALUE as `SelectWorkspaceError', a plist (:cause ONEOF)."
  (agent-repl-wire--decoded
   "SelectWorkspaceError"
   (list :cause (agent-repl-wire-decode-select-workspace-error-cause value))))

(defun agent-repl-wire-decode-select-workspace-response-result (value)
  "Decode `SelectWorkspaceResponse''s `result' oneof from the object VALUE."
  (agent-repl-wire--decode-oneof
   "SelectWorkspaceResponse" 'result value
   '((success :success agent-repl-wire-decode-select-workspace-success)
     (error :error agent-repl-wire-decode-select-workspace-error))))

(defun agent-repl-wire-decode-select-workspace-response (value)
  "Decode VALUE as `SelectWorkspaceResponse'.  THE ARM IS THE OUTCOME."
  (let ((object (agent-repl-wire--object "SelectWorkspaceResponse" value)))
    (agent-repl-wire--check-keys "SelectWorkspaceResponse" object '(success error))
    (agent-repl-wire--decoded
     "SelectWorkspaceResponse"
     (agent-repl-wire-decode-select-workspace-response-result object))))

;;;; ---- The workspace-subject verbs' one shape ----

;; A WORKSPACE-SUBJECT VERB names one workspace and answers success or the
;; four refusals the daemon's subject guard raises (`subjectFor'): unknown
;; workspace, ref mismatch, transferring away, not yet adopted.  Every such
;; verb whose success is empty shares the codec below, named by its rpc
;; NAME, so no two copies of the same refusal decoding can drift.

(defun agent-repl-wire--encode-subject-request (name value)
  "Encode the NAME request plist VALUE `(:workspace REF)'.
NAME is the rpc's name, e.g. \"ReportPageUnloading\"."
  (let ((message (concat name "Request")))
    (unless (plist-member value :workspace)
      (agent-repl-wire--fail message 'workspace "required message field is absent"))
    (agent-repl-wire--encoded
     message
     (list (cons 'workspace (agent-repl-wire-encode-workspace-ref (plist-get value :workspace)))))))

(defun agent-repl-wire--decode-subject-string-refusal (message key field value)
  "Decode VALUE as MESSAGE, whose one string FIELD (wire KEY) is its fact."
  (let ((object (agent-repl-wire--object message value)))
    (agent-repl-wire--check-keys message object (list key))
    (agent-repl-wire--decoded
     message (list field (agent-repl-wire--decode-string message key object)))))

(defun agent-repl-wire--decode-subject-error (name value)
  "Decode VALUE as the NAME rpc's Error, a plist (:cause ONEOF).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let* ((message (concat name "Error"))
         (object (agent-repl-wire--object message value)))
    (agent-repl-wire--check-keys
     message object '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted))
    (agent-repl-wire--decoded
     message
     (list :cause
           (agent-repl-wire--decode-oneof
            message 'cause object
            `((unknownWorkspace
               :unknown-workspace
               ,(lambda (v) (agent-repl-wire--decode-empty (concat name "UnknownWorkspace") v)))
              (workspaceRefMismatch
               :workspace-ref-mismatch
               ,(lambda (v) (agent-repl-wire--decode-subject-string-refusal
                             (concat name "WorkspaceRefMismatch") 'registryDir :registry-dir v)))
              (transferringAway
               :transferring-away
               ,(lambda (v) (agent-repl-wire--decode-subject-string-refusal
                             (concat name "TransferringAway") 'address :address v)))
              (notYetAdopted
               :not-yet-adopted
               ,(lambda (v) (agent-repl-wire--decode-empty (concat name "NotYetAdopted") v)))))))))

(defun agent-repl-wire--decode-subject-response (name value)
  "Decode VALUE as the NAME rpc's Response.  THE ARM IS THE OUTCOME.
The success arm is the empty NAME Success message."
  (let* ((message (concat name "Response"))
         (object (agent-repl-wire--object message value)))
    (agent-repl-wire--check-keys message object '(success error))
    (agent-repl-wire--decoded
     message
     (agent-repl-wire--decode-oneof
      message 'result object
      `((success :success ,(lambda (v) (agent-repl-wire--decode-empty (concat name "Success") v)))
        (error :error ,(lambda (v) (agent-repl-wire--decode-subject-error name v))))))))

;;;; ---- ReportPageUnloading ----

(defun agent-repl-wire-encode-report-page-unloading-request (value)
  "Encode the ReportPageUnloadingRequest plist VALUE `(:workspace REF)'.
Emacs's report that it is about to unload the workspace's page on purpose."
  (agent-repl-wire--encode-subject-request "ReportPageUnloading" value))

(defun agent-repl-wire-decode-report-page-unloading-response (value)
  "Decode VALUE as `ReportPageUnloadingResponse'.  THE ARM IS THE OUTCOME."
  (agent-repl-wire--decode-subject-response "ReportPageUnloading" value))

;;;; ---- MarkWorkspaceViewed ----

(defun agent-repl-wire-encode-mark-workspace-viewed-request (value)
  "Encode the MarkWorkspaceViewedRequest plist VALUE `(:workspace REF)'.
The editor's report that the user has now SEEN this workspace."
  (agent-repl-wire--encode-subject-request "MarkWorkspaceViewed" value))

(defun agent-repl-wire-decode-mark-workspace-viewed-response (value)
  "Decode VALUE as `MarkWorkspaceViewedResponse'.  THE ARM IS THE OUTCOME.
Marked; the roster stream carries the row's viewed marker."
  (agent-repl-wire--decode-subject-response "MarkWorkspaceViewed" value))

;;;; ---- AdoptHostWorkspace ----

(defun agent-repl-wire-encode-adopt-host-workspace-request-workspace (value)
  "Encode `AdoptHostWorkspaceRequest''s `workspace' field VALUE."
  (agent-repl-wire-encode-workspace-ref value))

(defun agent-repl-wire-encode-adopt-host-workspace-request (value)
  "Encode the AdoptHostWorkspaceRequest plist VALUE `(:workspace REF)'.
The VERB identifies the participant — Emacs and the webview have sibling
adopt verbs — so nothing in the request self-declares the caller's kind."
  (unless (plist-member value :workspace)
    (agent-repl-wire--fail "AdoptHostWorkspaceRequest" 'workspace
                           "required message field is absent"))
  (agent-repl-wire--encoded
   "AdoptHostWorkspaceRequest"
   (list (cons 'workspace
               (agent-repl-wire-encode-adopt-host-workspace-request-workspace
                (plist-get value :workspace))))))

(defun agent-repl-wire-decode-adopt-host-workspace-success (value)
  "Decode VALUE as the empty message `AdoptHostWorkspaceSuccess'.
Adoption is COMPLETE: re-subscribe the workspace's streams here now."
  (agent-repl-wire--decode-empty "AdoptHostWorkspaceSuccess" value))

(defun agent-repl-wire-decode-adopt-host-workspace-unknown-workspace (value)
  "Decode VALUE as the empty message `AdoptHostWorkspaceUnknownWorkspace'.
The workspace id is not in the daemon's registry."
  (agent-repl-wire--decode-empty "AdoptHostWorkspaceUnknownWorkspace" value))

(defun agent-repl-wire-decode-adopt-host-workspace-workspace-ref-mismatch (value)
  "Decode VALUE as `AdoptHostWorkspaceWorkspaceRefMismatch', a plist
(`:registry-dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((object (agent-repl-wire--object "AdoptHostWorkspaceWorkspaceRefMismatch" value)))
    (agent-repl-wire--check-keys "AdoptHostWorkspaceWorkspaceRefMismatch" object '(registryDir))
    (agent-repl-wire--decoded
     "AdoptHostWorkspaceWorkspaceRefMismatch"
     (list :registry-dir (agent-repl-wire--decode-string
                    "AdoptHostWorkspaceWorkspaceRefMismatch" 'registryDir object)))))

(defun agent-repl-wire-decode-adopt-host-workspace-transferring-away (value)
  "Decode VALUE as `AdoptHostWorkspaceTransferringAway', a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((object (agent-repl-wire--object "AdoptHostWorkspaceTransferringAway" value)))
    (agent-repl-wire--check-keys "AdoptHostWorkspaceTransferringAway" object '(address))
    (agent-repl-wire--decoded
     "AdoptHostWorkspaceTransferringAway"
     (list :address (agent-repl-wire--decode-string
                    "AdoptHostWorkspaceTransferringAway" 'address object)))))

(defun agent-repl-wire-decode-adopt-host-workspace-not-yet-adopted (value)
  "Decode VALUE as the empty message `AdoptHostWorkspaceNotYetAdopted'.
A joining daemon has not finished adopting this workspace yet."
  (agent-repl-wire--decode-empty "AdoptHostWorkspaceNotYetAdopted" value))

(defun agent-repl-wire-decode-adopt-host-workspace-no-transfer-announced (value)
  "Decode VALUE as the empty message `AdoptHostWorkspaceNoTransferAnnounced'.
No transfer was announced for this workspace."
  (agent-repl-wire--decode-empty "AdoptHostWorkspaceNoTransferAnnounced" value))

(defun agent-repl-wire-decode-adopt-host-workspace-participant-not-expected (value)
  "Decode VALUE as the empty message `AdoptHostWorkspaceParticipantNotExpected'.
The caller's stream was not open at announcement."
  (agent-repl-wire--decode-empty "AdoptHostWorkspaceParticipantNotExpected" value))

(defun agent-repl-wire-decode-adopt-host-workspace-transfer-deferred (value)
  "Decode VALUE as the empty message `AdoptHostWorkspaceTransferDeferred'.
The successor refused a mid-work adoption; the incumbent serves the
workspace until it is free and its next `transferred' push is the cue."
  (agent-repl-wire--decode-empty "AdoptHostWorkspaceTransferDeferred" value))

(defun agent-repl-wire-decode-adopt-host-workspace-error-unknown-workspace (value)
  "Decode AdoptHostWorkspaceError's `unknown_workspace' cause arm from VALUE."
  (agent-repl-wire-decode-adopt-host-workspace-unknown-workspace value))

(defun agent-repl-wire-decode-adopt-host-workspace-error-workspace-ref-mismatch (value)
  "Decode AdoptHostWorkspaceError's `workspace_ref_mismatch' cause arm from
VALUE."
  (agent-repl-wire-decode-adopt-host-workspace-workspace-ref-mismatch value))

(defun agent-repl-wire-decode-adopt-host-workspace-error-transferring-away (value)
  "Decode AdoptHostWorkspaceError's `transferring_away' cause arm from VALUE."
  (agent-repl-wire-decode-adopt-host-workspace-transferring-away value))

(defun agent-repl-wire-decode-adopt-host-workspace-error-not-yet-adopted (value)
  "Decode AdoptHostWorkspaceError's `not_yet_adopted' cause arm from VALUE."
  (agent-repl-wire-decode-adopt-host-workspace-not-yet-adopted value))

(defun agent-repl-wire-decode-adopt-host-workspace-error-no-transfer-announced (value)
  "Decode AdoptHostWorkspaceError's `no_transfer_announced' cause arm from
VALUE."
  (agent-repl-wire-decode-adopt-host-workspace-no-transfer-announced value))

(defun agent-repl-wire-decode-adopt-host-workspace-error-participant-not-expected (value)
  "Decode AdoptHostWorkspaceError's `participant_not_expected' cause arm from
VALUE."
  (agent-repl-wire-decode-adopt-host-workspace-participant-not-expected value))

(defun agent-repl-wire-decode-adopt-host-workspace-error-transfer-deferred (value)
  "Decode AdoptHostWorkspaceError's `transfer_deferred' cause arm from VALUE."
  (agent-repl-wire-decode-adopt-host-workspace-transfer-deferred value))

(defun agent-repl-wire-decode-adopt-host-workspace-error-cause (value)
  "Decode AdoptHostWorkspaceError's `cause' oneof from the object VALUE.
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((object (agent-repl-wire--object "AdoptHostWorkspaceError" value)))
    (agent-repl-wire--check-keys "AdoptHostWorkspaceError" object '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted noTransferAnnounced participantNotExpected transferDeferred))
    (agent-repl-wire--decode-oneof
     "AdoptHostWorkspaceError" 'cause object
     '((unknownWorkspace :unknown-workspace agent-repl-wire-decode-adopt-host-workspace-error-unknown-workspace)
       (workspaceRefMismatch :workspace-ref-mismatch agent-repl-wire-decode-adopt-host-workspace-error-workspace-ref-mismatch)
       (transferringAway :transferring-away agent-repl-wire-decode-adopt-host-workspace-error-transferring-away)
       (notYetAdopted :not-yet-adopted agent-repl-wire-decode-adopt-host-workspace-error-not-yet-adopted)
       (noTransferAnnounced :no-transfer-announced agent-repl-wire-decode-adopt-host-workspace-error-no-transfer-announced)
       (participantNotExpected :participant-not-expected agent-repl-wire-decode-adopt-host-workspace-error-participant-not-expected)
       (transferDeferred :transfer-deferred agent-repl-wire-decode-adopt-host-workspace-error-transfer-deferred)))))

(defun agent-repl-wire-decode-adopt-host-workspace-error (value)
  "Decode VALUE as `AdoptHostWorkspaceError', a plist (:cause ONEOF)."
  (agent-repl-wire--decoded
   "AdoptHostWorkspaceError"
   (list :cause (agent-repl-wire-decode-adopt-host-workspace-error-cause value))))

(defun agent-repl-wire-decode-adopt-host-workspace-response-result (value)
  "Decode `AdoptHostWorkspaceResponse''s `result' oneof from the object VALUE."
  (agent-repl-wire--decode-oneof
   "AdoptHostWorkspaceResponse" 'result value
   '((success :success agent-repl-wire-decode-adopt-host-workspace-success)
     (error :error agent-repl-wire-decode-adopt-host-workspace-error))))

(defun agent-repl-wire-decode-adopt-host-workspace-response (value)
  "Decode VALUE as `AdoptHostWorkspaceResponse'.  THE ARM IS THE OUTCOME."
  (let ((object (agent-repl-wire--object "AdoptHostWorkspaceResponse" value)))
    (agent-repl-wire--check-keys "AdoptHostWorkspaceResponse" object '(success error))
    (agent-repl-wire--decoded
     "AdoptHostWorkspaceResponse"
     (agent-repl-wire-decode-adopt-host-workspace-response-result object))))

;;;; ---- WatchDaemon ----

;;;; ---- EditorFocus ----

(defun agent-repl-wire-encode-editor-focus-focused (value)
  "Encode the empty message `EditorFocusFocused' from VALUE."
  (agent-repl-wire--encode-empty "EditorFocusFocused" value))

(defun agent-repl-wire-encode-editor-focus-unfocused (value)
  "Encode the empty message `EditorFocusUnfocused' from VALUE."
  (agent-repl-wire--encode-empty "EditorFocusUnfocused" value))

(defun agent-repl-wire-encode-editor-focus (value)
  "Encode `EditorFocus' from VALUE, the oneof plist `(:arm :focused)' or
`(:arm :unfocused)'.  THE ARM IS THE FOCUS, and it is REQUIRED: a focus
naming neither is refused HERE, before anything is sent."
  (agent-repl-wire--encoded
   "EditorFocus"
   (agent-repl-wire--encode-oneof
    "EditorFocus" 'focus value
    '((:focused focused agent-repl-wire-encode-editor-focus-focused)
      (:unfocused unfocused agent-repl-wire-encode-editor-focus-unfocused)))))

(defun agent-repl-wire-editor-focus (focused)
  "Answer the `EditorFocus' oneof plist for FOCUSED, non-nil or nil."
  (list :arm (if focused :focused :unfocused)))

;;;; ---- ReportEditorFocus ----

(defun agent-repl-wire-encode-report-editor-focus-request (value)
  "Encode the ReportEditorFocusRequest plist VALUE `(:focus F)'.
The focus is REQUIRED."
  (unless (plist-member value :focus)
    (agent-repl-wire--fail "ReportEditorFocusRequest" 'focus
                           "required message field is absent"))
  (agent-repl-wire--encoded
   "ReportEditorFocusRequest"
   (list (cons 'focus (agent-repl-wire-encode-editor-focus
                       (plist-get value :focus))))))

(defun agent-repl-wire-decode-report-editor-focus-no-gain (value)
  "Decode VALUE as the empty `ReportEditorFocusNoGain'.
The report was no focus gain, so it claimed nothing."
  (agent-repl-wire--decode-empty "ReportEditorFocusNoGain" value))

(defun agent-repl-wire-decode-report-editor-focus-nothing-held (value)
  "Decode VALUE as the empty `ReportEditorFocusNothingHeld'.
The report was a gain and the daemon held no banner."
  (agent-repl-wire--decode-empty "ReportEditorFocusNothingHeld" value))

(defun agent-repl-wire-decode-report-editor-focus-banner-claimed (value)
  "Decode VALUE as `ReportEditorFocusBannerClaimed', a plist (:workspace REF).
REF is the newest held banner's workspace, which Emacs selects."
  (let ((object (agent-repl-wire--object "ReportEditorFocusBannerClaimed" value)))
    (agent-repl-wire--check-keys "ReportEditorFocusBannerClaimed" object '(workspace))
    (agent-repl-wire--decoded
     "ReportEditorFocusBannerClaimed"
     (list :workspace (agent-repl-wire--decode-message
                       "ReportEditorFocusBannerClaimed" 'workspace object
                       #'agent-repl-wire-decode-workspace-ref)))))

(defun agent-repl-wire-decode-report-editor-focus-success-claim (value)
  "Decode `ReportEditorFocusSuccess''s `claim' oneof from the object VALUE."
  (agent-repl-wire--decode-oneof
   "ReportEditorFocusSuccess" 'claim value
   '((noGain :no-gain agent-repl-wire-decode-report-editor-focus-no-gain)
     (nothingHeld :nothing-held agent-repl-wire-decode-report-editor-focus-nothing-held)
     (banner :banner agent-repl-wire-decode-report-editor-focus-banner-claimed))))

(defun agent-repl-wire-decode-report-editor-focus-success (value)
  "Decode VALUE as `ReportEditorFocusSuccess', a plist (:claim ONEOF).
THE ARM IS THE ANSWER: `:banner' names the workspace a focus gain claimed,
`:nothing-held' and `:no-gain' name nothing."
  (let ((object (agent-repl-wire--object "ReportEditorFocusSuccess" value)))
    (agent-repl-wire--check-keys "ReportEditorFocusSuccess" object '(noGain nothingHeld banner))
    (agent-repl-wire--decoded
     "ReportEditorFocusSuccess"
     (list :claim (agent-repl-wire-decode-report-editor-focus-success-claim object)))))

(defun agent-repl-wire-decode-report-editor-focus-no-emacs-stream (value)
  "Decode VALUE as the empty `ReportEditorFocusNoEmacsStream'."
  (agent-repl-wire--decode-empty "ReportEditorFocusNoEmacsStream" value))

(defun agent-repl-wire-decode-report-editor-focus-error-cause (value)
  "Decode `ReportEditorFocusError''s `cause' oneof from the object VALUE."
  (agent-repl-wire--decode-oneof
   "ReportEditorFocusError" 'cause value
   '((noEmacsStream :no-emacs-stream
                    agent-repl-wire-decode-report-editor-focus-no-emacs-stream))))

(defun agent-repl-wire-decode-report-editor-focus-error (value)
  "Decode VALUE as `ReportEditorFocusError', a plist (:cause ONEOF)."
  (let ((object (agent-repl-wire--object "ReportEditorFocusError" value)))
    (agent-repl-wire--check-keys "ReportEditorFocusError" object '(noEmacsStream))
    (agent-repl-wire--decoded
     "ReportEditorFocusError"
     (list :cause (agent-repl-wire-decode-report-editor-focus-error-cause object)))))

(defun agent-repl-wire-decode-report-editor-focus-response-result (value)
  "Decode `ReportEditorFocusResponse''s `result' oneof from the object VALUE."
  (agent-repl-wire--decode-oneof
   "ReportEditorFocusResponse" 'result value
   '((success :success agent-repl-wire-decode-report-editor-focus-success)
     (error :error agent-repl-wire-decode-report-editor-focus-error))))

(defun agent-repl-wire-decode-report-editor-focus-response (value)
  "Decode VALUE as `ReportEditorFocusResponse'.  THE ARM IS THE OUTCOME."
  (let ((object (agent-repl-wire--object "ReportEditorFocusResponse" value)))
    (agent-repl-wire--check-keys "ReportEditorFocusResponse" object '(success error))
    (agent-repl-wire--decoded
     "ReportEditorFocusResponse"
     (agent-repl-wire-decode-report-editor-focus-response-result object))))

(defun agent-repl-wire-encode-editor-instance (value)
  "Encode `EditorInstance' from the string VALUE, this Emacs process's identity.
REQUIRED and never empty: a watch without it is refused, and an empty one
is refused HERE, before anything is sent."
  (unless (stringp value)
    (agent-repl-wire--fail "EditorInstance" 'value "required field is unset"))
  (when (string-empty-p value)
    (agent-repl-wire--fail "EditorInstance" 'value "required string is empty"))
  (agent-repl-wire--encoded
   "EditorInstance"
   (list (cons 'value (agent-repl-wire--encode-string "EditorInstance" 'value value)))))

(defun agent-repl-wire-encode-watch-daemon-emacs (value)
  "Encode `WatchDaemonEmacs' from VALUE.
VALUE is the plist `(:elisp-build BUILD :focus F :instance I)', plus the
OPTIONAL `:pid' PID of this Emacs process, which the daemon measures in its
process metrics while the stream stands
\(`agent-repl-wire-encode-editor-process').
The build is REQUIRED and never empty: a watch without it is refused,
because a deploy could not tell whether this Emacs runs the checkout's
elisp.  An empty one is refused HERE, before anything is sent.  The focus
is REQUIRED too: the daemon decides every desktop banner on it from the
stream's first instant (`agent-repl-wire-encode-editor-focus').  So is the
INSTANCE, this Emacs process's identity, the same on every stream it
opens: a new one is how the daemon tells a full Emacs restart from a
reconnect (`agent-repl-wire-encode-editor-instance')."
  (let ((build (plist-get value :elisp-build)))
    (unless (stringp build)
      (agent-repl-wire--fail "WatchDaemonEmacs" 'elispBuild "required field is unset"))
    (when (string-empty-p build)
      (agent-repl-wire--fail "WatchDaemonEmacs" 'elispBuild "required string is empty"))
    (unless (plist-member value :focus)
      (agent-repl-wire--fail "WatchDaemonEmacs" 'focus "required message field is absent"))
    (unless (plist-member value :instance)
      (agent-repl-wire--fail "WatchDaemonEmacs" 'instance "required message field is absent"))
    (agent-repl-wire--encoded
     "WatchDaemonEmacs"
     (append
      (list (cons 'elispBuild (agent-repl-wire--encode-string
                               "WatchDaemonEmacs" 'elispBuild build))
            (cons 'focus (agent-repl-wire-encode-editor-focus
                          (plist-get value :focus)))
            (cons 'instance (agent-repl-wire-encode-editor-instance
                             (plist-get value :instance))))
      (when (plist-member value :pid)
        (list (cons 'process (agent-repl-wire-encode-editor-process
                              (plist-get value :pid)))))))))

(defun agent-repl-wire-encode-editor-process (pid)
  "Encode `EditorProcess' from PID, this Emacs's process id.
A pid that is not a positive integer is refused HERE, before anything is sent."
  (unless (and (integerp pid) (> pid 0))
    (agent-repl-wire--fail "EditorProcess" 'pid "not a positive integer"))
  (agent-repl-wire--encoded "EditorProcess" (list (cons 'pid pid))))

(defun agent-repl-wire-encode-watch-daemon-request-emacs (value)
  "Encode `WatchDaemonRequest''s `emacs' client arm from VALUE."
  (agent-repl-wire-encode-watch-daemon-emacs value))

(defun agent-repl-wire-encode-watch-daemon-request (value)
  "Encode the WatchDaemonRequest from VALUE `(:client ONEOF)'.
THE ARM IS THE CLIENT, and it is REQUIRED.  Emacs only ever connects as
`emacs', stating the elisp it has loaded; the `webview' arm is a
browser's, so this codec has no spelling for it and refuses it as an
unknown arm."
  (agent-repl-wire--encoded
   "WatchDaemonRequest"
   (agent-repl-wire--encode-oneof
    "WatchDaemonRequest" 'client (plist-get value :client)
    '((:emacs emacs agent-repl-wire-encode-watch-daemon-request-emacs)))))

(defun agent-repl-wire-decode-daemon-shutdown-self-merge-rollout (value)
  "Decode VALUE as the empty `DaemonShutdownSelfMergeRollout'."
  (agent-repl-wire--decode-empty "DaemonShutdownSelfMergeRollout" value))

(defun agent-repl-wire-decode-daemon-shutdown-scheduled-drain-reason (value)
  "Decode `DaemonShutdownScheduledDrain''s `reason' field VALUE."
  (agent-repl-wire-decode-drain-reason value))

(defun agent-repl-wire-decode-daemon-shutdown-scheduled-drain (value)
  "Decode VALUE as `DaemonShutdownScheduledDrain', a plist `(:reason)'."
  (let ((object (agent-repl-wire--object "DaemonShutdownScheduledDrain" value)))
    (agent-repl-wire--check-keys "DaemonShutdownScheduledDrain" object '(reason))
    (agent-repl-wire--decoded
     "DaemonShutdownScheduledDrain"
     (list :reason (agent-repl-wire--decode-message
                    "DaemonShutdownScheduledDrain" 'reason object
                    #'agent-repl-wire-decode-daemon-shutdown-scheduled-drain-reason)))))

(defun agent-repl-wire-decode-daemon-shutdown-immediate-reason (value)
  "Decode `DaemonShutdownImmediate''s `reason' field VALUE."
  (agent-repl-wire-decode-drain-reason value))

(defun agent-repl-wire-decode-daemon-shutdown-immediate (value)
  "Decode VALUE as `DaemonShutdownImmediate', a plist `(:reason)'."
  (let ((object (agent-repl-wire--object "DaemonShutdownImmediate" value)))
    (agent-repl-wire--check-keys "DaemonShutdownImmediate" object '(reason))
    (agent-repl-wire--decoded
     "DaemonShutdownImmediate"
     (list :reason (agent-repl-wire--decode-message
                    "DaemonShutdownImmediate" 'reason object
                    #'agent-repl-wire-decode-daemon-shutdown-immediate-reason)))))

(defun agent-repl-wire-decode-daemon-shutdown-cause-kind (value)
  "Decode `DaemonShutdownCause''s `kind' oneof from the object VALUE."
  (agent-repl-wire--decode-oneof
   "DaemonShutdownCause" 'kind value
   '((selfMergeRollout :self-merge-rollout
                       agent-repl-wire-decode-daemon-shutdown-self-merge-rollout)
     (scheduledDrain :scheduled-drain
                     agent-repl-wire-decode-daemon-shutdown-scheduled-drain)
     (immediate :immediate agent-repl-wire-decode-daemon-shutdown-immediate))))

(defun agent-repl-wire-decode-daemon-shutdown-cause (value)
  "Decode VALUE as `DaemonShutdownCause'.  THE ARM IS THE CAUSE."
  (let ((object (agent-repl-wire--object "DaemonShutdownCause" value)))
    (agent-repl-wire--check-keys
     "DaemonShutdownCause" object '(selfMergeRollout scheduledDrain immediate))
    (agent-repl-wire--decoded
     "DaemonShutdownCause"
     (agent-repl-wire-decode-daemon-shutdown-cause-kind object))))

(defun agent-repl-wire-decode-daemon-shutdown-announced-cause (value)
  "Decode `DaemonShutdownAnnounced''s `cause' field VALUE."
  (agent-repl-wire-decode-daemon-shutdown-cause value))

(defun agent-repl-wire-decode-daemon-shutdown-announced (value)
  "Decode VALUE as `DaemonShutdownAnnounced'.
Returns `(:address A-or-nil :cause ONEOF :expected-outage-ms N
:minted-at-ms N)'.  An UNSET address is a PLAIN BOUNCE: no successor is
up, so the client waits out the outage instead of dual-attaching."
  (let ((object (agent-repl-wire--object "DaemonShutdownAnnounced" value)))
    (agent-repl-wire--check-keys
     "DaemonShutdownAnnounced" object '(address cause expectedOutageMs mintedAtMs))
    (agent-repl-wire--decoded
     "DaemonShutdownAnnounced"
     (list :address (agent-repl-wire--decode-optional-string
                     "DaemonShutdownAnnounced" 'address object)
           :cause (agent-repl-wire--decode-message
                   "DaemonShutdownAnnounced" 'cause object
                   #'agent-repl-wire-decode-daemon-shutdown-announced-cause)
           :expected-outage-ms (agent-repl-wire--decode-int64
                                "DaemonShutdownAnnounced" 'expectedOutageMs object)
           :minted-at-ms (agent-repl-wire--decode-int64
                          "DaemonShutdownAnnounced" 'mintedAtMs object)))))

(defun agent-repl-wire-decode-daemon-drain-scheduled-reason (value)
  "Decode `DaemonDrainScheduled''s `reason' field VALUE."
  (agent-repl-wire-decode-drain-reason value))

(defun agent-repl-wire-decode-daemon-drain-scheduled (value)
  "Decode VALUE as `DaemonDrainScheduled', a plist `(:at-ms :reason)'.
The standing schedule, re-pushed to late subscribers."
  (let ((object (agent-repl-wire--object "DaemonDrainScheduled" value)))
    (agent-repl-wire--check-keys "DaemonDrainScheduled" object '(atMs reason))
    (agent-repl-wire--decoded
     "DaemonDrainScheduled"
     (list :at-ms (agent-repl-wire--decode-int64 "DaemonDrainScheduled" 'atMs object)
           :reason (agent-repl-wire--decode-message
                    "DaemonDrainScheduled" 'reason object
                    #'agent-repl-wire-decode-daemon-drain-scheduled-reason)))))

(defun agent-repl-wire-decode-daemon-reload-elisp (value)
  "Decode VALUE as `DaemonReloadElisp', a plist `(:module-root :build)'.
BOTH ARE REQUIRED: the root is what Emacs checks against its own before it
loads anything, and the build is what it reports from then on.  An empty
one is a contract breach, never a reload to attempt."
  (let ((object (agent-repl-wire--object "DaemonReloadElisp" value)))
    (agent-repl-wire--check-keys "DaemonReloadElisp" object '(moduleRoot build))
    (let ((root (agent-repl-wire--decode-string "DaemonReloadElisp" 'moduleRoot object))
          (build (agent-repl-wire--decode-string "DaemonReloadElisp" 'build object)))
      (when (string-empty-p root)
        (agent-repl-wire--fail "DaemonReloadElisp" 'moduleRoot "required string is empty"))
      (when (string-empty-p build)
        (agent-repl-wire--fail "DaemonReloadElisp" 'build "required string is empty"))
      (agent-repl-wire--decoded
       "DaemonReloadElisp"
       (list :module-root root :build build)))))

(defun agent-repl-wire-decode-daemon-reload-webhost (value)
  "Decode VALUE as `DaemonReloadWebhost', a plist `(:build)'.
The build is REQUIRED: it is what the host is reloaded onto, and an empty
one is a contract breach, never a reload to attempt."
  (let ((object (agent-repl-wire--object "DaemonReloadWebhost" value)))
    (agent-repl-wire--check-keys "DaemonReloadWebhost" object '(build))
    (let ((build (agent-repl-wire--decode-string "DaemonReloadWebhost" 'build object)))
      (when (string-empty-p build)
        (agent-repl-wire--fail "DaemonReloadWebhost" 'build "required string is empty"))
      (agent-repl-wire--decoded "DaemonReloadWebhost" (list :build build)))))

(defun agent-repl-wire-decode-daemon-drain-cancelled (value)
  "Decode VALUE as the empty `DaemonDrainCancelled' — presence is the fact."
  (agent-repl-wire--decode-empty "DaemonDrainCancelled" value))

;;;; ---- Workspace-mutation progress ------------------------------------

(defun agent-repl-wire-decode-workspace-create-stage-deriving-name (value)
  "Decode VALUE as the empty `WorkspaceCreateStageDerivingName'."
  (agent-repl-wire--decode-empty "WorkspaceCreateStageDerivingName" value))

(defun agent-repl-wire-decode-workspace-create-stage-creating-worktree (value)
  "Decode VALUE as the empty `WorkspaceCreateStageCreatingWorktree'."
  (agent-repl-wire--decode-empty "WorkspaceCreateStageCreatingWorktree" value))

(defun agent-repl-wire-decode-workspace-create-stage-starting-session (value)
  "Decode VALUE as the empty `WorkspaceCreateStageStartingSession'."
  (agent-repl-wire--decode-empty "WorkspaceCreateStageStartingSession" value))

(defun agent-repl-wire-decode-workspace-create-stage (value)
  "Decode `WorkspaceCreateStage' from VALUE into the stage keyword.
THE SET ARM IS THE STAGE, and every arm is an empty message, so the
keyword is the whole fact: `:deriving-name', `:creating-worktree' or
`:starting-session'.  An arm this codec does not hold is refused as an
unknown field by the key check -- logged at ERROR and signalled, never a
stage guessed at or dropped -- and an unset oneof is refused likewise."
  (let ((object (agent-repl-wire--object "WorkspaceCreateStage" value)))
    (agent-repl-wire--check-keys "WorkspaceCreateStage" object
                                 '(derivingName creatingWorktree startingSession))
    (agent-repl-wire--decoded
     "WorkspaceCreateStage"
     (plist-get
      (agent-repl-wire--decode-oneof
       "WorkspaceCreateStage" 'stage object
       '((derivingName :deriving-name
                       agent-repl-wire-decode-workspace-create-stage-deriving-name)
         (creatingWorktree :creating-worktree
                           agent-repl-wire-decode-workspace-create-stage-creating-worktree)
         (startingSession :starting-session
                          agent-repl-wire-decode-workspace-create-stage-starting-session)))
      :arm))))

(defun agent-repl-wire-decode-workspace-create-succeeded (value)
  "Decode `WorkspaceCreateSucceeded' from VALUE into `(:workspace REF :name N)'.
The minted identity a client selects and the name it announces."
  (let ((object (agent-repl-wire--object "WorkspaceCreateSucceeded" value)))
    (agent-repl-wire--check-keys "WorkspaceCreateSucceeded" object '(workspace name))
    (agent-repl-wire--decoded
     "WorkspaceCreateSucceeded"
     (list :workspace (agent-repl-wire--decode-message
                       "WorkspaceCreateSucceeded" 'workspace object
                       #'agent-repl-wire-decode-workspace-ref)
           :name (agent-repl-wire--decode-string
                  "WorkspaceCreateSucceeded" 'name object)))))

(defun agent-repl-wire-decode-workspace-create-failed-refusal (value)
  "Decode `WorkspaceCreateFailed''s `refusal' arm — a CreateWorkspaceError."
  (agent-repl-wire-decode-create-workspace-error value))

(defun agent-repl-wire-decode-workspace-create-failed (value)
  "Decode `WorkspaceCreateFailed' from VALUE into `(:arm ARM :value V)'.
THE ARM IS THE KIND OF FAILURE: a typed `refusal' the synchronous form
would answer, or an `internal' sentence it would fail the rpc with."
  (let ((object (agent-repl-wire--object "WorkspaceCreateFailed" value)))
    (agent-repl-wire--check-keys "WorkspaceCreateFailed" object '(refusal internal))
    (agent-repl-wire--decode-oneof
     "WorkspaceCreateFailed" 'cause object
     '((refusal :refusal agent-repl-wire-decode-workspace-create-failed-refusal)
       (internal :internal identity)))))

(defun agent-repl-wire-decode-workspace-create-progress (value)
  "Decode `WorkspaceCreateProgress' from VALUE into `(:arm STEP :value V)'.
THE ARM IS THE STEP: an intermediate `entered_stage' (whose value is the
stage keyword), or a terminal `succeeded' or `failed'.  The retired
field-1 `stage' enum is not a key this codec holds, so a push in that
shape is refused as an unknown field rather than misread."
  (let ((object (agent-repl-wire--object "WorkspaceCreateProgress" value)))
    (agent-repl-wire--check-keys "WorkspaceCreateProgress" object
                                 '(enteredStage succeeded failed))
    (agent-repl-wire--decode-oneof
     "WorkspaceCreateProgress" 'step object
     '((enteredStage :entered-stage agent-repl-wire-decode-workspace-create-stage)
       (succeeded :succeeded agent-repl-wire-decode-workspace-create-succeeded)
       (failed :failed agent-repl-wire-decode-workspace-create-failed)))))

;; Each open-stage arm is an empty message: the arm being set is the whole
;; fact, so each decoder only checks the arm carries no fields.
(defun agent-repl-wire-decode-workspace-open-stage-checking-worktree (value)
  "Decode VALUE as the empty `WorkspaceOpenStageCheckingWorktree'."
  (agent-repl-wire--decode-empty "WorkspaceOpenStageCheckingWorktree" value))

(defun agent-repl-wire-decode-workspace-open-stage-starting-session (value)
  "Decode VALUE as the empty `WorkspaceOpenStageStartingSession'."
  (agent-repl-wire--decode-empty "WorkspaceOpenStageStartingSession" value))

(defun agent-repl-wire-decode-workspace-open-stage-reviving (value)
  "Decode VALUE as the empty `WorkspaceOpenStageReviving'."
  (agent-repl-wire--decode-empty "WorkspaceOpenStageReviving" value))

(defun agent-repl-wire-decode-workspace-open-stage-clearing-closed (value)
  "Decode VALUE as the empty `WorkspaceOpenStageClearingClosed'."
  (agent-repl-wire--decode-empty "WorkspaceOpenStageClearingClosed" value))

(defun agent-repl-wire-decode-workspace-open-stage-checking-build (value)
  "Decode VALUE as the empty `WorkspaceOpenStageCheckingBuild'."
  (agent-repl-wire--decode-empty "WorkspaceOpenStageCheckingBuild" value))

(defun agent-repl-wire-decode-workspace-open-stage-restoring-worktree (value)
  "Decode VALUE as the empty `WorkspaceOpenStageRestoringWorktree'."
  (agent-repl-wire--decode-empty "WorkspaceOpenStageRestoringWorktree" value))

(defun agent-repl-wire-decode-workspace-open-stage (value)
  "Decode `WorkspaceOpenStage' from VALUE into the stage keyword.
THE SET ARM IS THE STAGE, and every arm is an empty message, so the
keyword is the whole fact: `:checking-worktree', `:restoring-worktree',
`:starting-session', `:reviving', `:clearing-closed' or
`:checking-build'.  An arm this codec does not hold is refused as an
unknown field by the key check -- logged at ERROR and signalled, never a
stage guessed at or dropped -- and an unset oneof is refused likewise."
  (let ((object (agent-repl-wire--object "WorkspaceOpenStage" value)))
    (agent-repl-wire--check-keys "WorkspaceOpenStage" object
                                 '(checkingWorktree restoringWorktree startingSession
                                   reviving clearingClosed checkingBuild))
    (agent-repl-wire--decoded
     "WorkspaceOpenStage"
     (plist-get
      (agent-repl-wire--decode-oneof
       "WorkspaceOpenStage" 'stage object
       '((checkingWorktree :checking-worktree
                           agent-repl-wire-decode-workspace-open-stage-checking-worktree)
         (startingSession :starting-session
                          agent-repl-wire-decode-workspace-open-stage-starting-session)
         (reviving :reviving
                   agent-repl-wire-decode-workspace-open-stage-reviving)
         (clearingClosed :clearing-closed
                         agent-repl-wire-decode-workspace-open-stage-clearing-closed)
         (checkingBuild :checking-build
                        agent-repl-wire-decode-workspace-open-stage-checking-build)
         (restoringWorktree :restoring-worktree
                            agent-repl-wire-decode-workspace-open-stage-restoring-worktree)))
      :arm))))

(defun agent-repl-wire-decode-workspace-open-progress (value)
  "Decode `WorkspaceOpenProgress' from VALUE into `(:stage STAGE)'.
STAGE is the keyword its required `entered_stage' decodes to.  NO
TERMINAL STEP: an open is answered synchronously on its own rpc, so its
success and every refusal reach the caller there.  This message carries
only the wait that answer cannot express.  The retired field-1 `stage'
enum is not a key this codec holds, so a push in that shape is refused as
an unknown field rather than misread."
  (let ((object (agent-repl-wire--object "WorkspaceOpenProgress" value)))
    (agent-repl-wire--check-keys "WorkspaceOpenProgress" object '(enteredStage))
    (agent-repl-wire--decoded
     "WorkspaceOpenProgress"
     (list :stage (agent-repl-wire--decode-message
                   "WorkspaceOpenProgress" 'enteredStage object
                   #'agent-repl-wire-decode-workspace-open-stage)))))

(defun agent-repl-wire-decode-workspace-kill-succeeded (value)
  "Decode VALUE as the empty `WorkspaceKillSucceeded'."
  (agent-repl-wire--decode-empty "WorkspaceKillSucceeded" value))

(defun agent-repl-wire-decode-workspace-kill-failed (value)
  "Decode `WorkspaceKillFailed' from VALUE into `(:internal SENTENCE)'.
A kill has no typed refusal after its ack, so the sentence is the whole
account."
  (let ((object (agent-repl-wire--object "WorkspaceKillFailed" value)))
    (agent-repl-wire--check-keys "WorkspaceKillFailed" object '(internal))
    (agent-repl-wire--decoded
     "WorkspaceKillFailed"
     (list :internal (agent-repl-wire--decode-string "WorkspaceKillFailed" 'internal object)))))

(defun agent-repl-wire-decode-workspace-kill-progress (value)
  "Decode `WorkspaceKillProgress' from VALUE into `(:arm STEP :value V)'.
THE ARM IS THE END of an accepted kill: `succeeded' or `failed'."
  (let ((object (agent-repl-wire--object "WorkspaceKillProgress" value)))
    (agent-repl-wire--check-keys "WorkspaceKillProgress" object '(succeeded failed))
    (agent-repl-wire--decode-oneof
     "WorkspaceKillProgress" 'step object
     '((succeeded :succeeded agent-repl-wire-decode-workspace-kill-succeeded)
       (failed :failed agent-repl-wire-decode-workspace-kill-failed)))))

(defun agent-repl-wire-decode-workspace-nuke-succeeded (value)
  "Decode VALUE as the empty `WorkspaceNukeSucceeded'."
  (agent-repl-wire--decode-empty "WorkspaceNukeSucceeded" value))

(defun agent-repl-wire-decode-workspace-nuke-failed-refusal (value)
  "Decode `WorkspaceNukeFailed''s `refusal' arm — a NukeWorkspaceError."
  (agent-repl-wire-decode-nuke-workspace-error value))

(defun agent-repl-wire-decode-workspace-nuke-failed (value)
  "Decode `WorkspaceNukeFailed' from VALUE into `(:arm ARM :value V)'.
THE ARM IS THE KIND OF FAILURE: a typed `refusal' (after the ack, only
`git_failed') or an `internal' sentence."
  (let ((object (agent-repl-wire--object "WorkspaceNukeFailed" value)))
    (agent-repl-wire--check-keys "WorkspaceNukeFailed" object '(refusal internal))
    (agent-repl-wire--decode-oneof
     "WorkspaceNukeFailed" 'cause object
     '((refusal :refusal agent-repl-wire-decode-workspace-nuke-failed-refusal)
       (internal :internal identity)))))

(defun agent-repl-wire-decode-workspace-nuke-progress (value)
  "Decode `WorkspaceNukeProgress' from VALUE into `(:arm STEP :value V)'.
THE ARM IS THE END of an accepted nuke: `succeeded' or `failed'."
  (let ((object (agent-repl-wire--object "WorkspaceNukeProgress" value)))
    (agent-repl-wire--check-keys "WorkspaceNukeProgress" object '(succeeded failed))
    (agent-repl-wire--decode-oneof
     "WorkspaceNukeProgress" 'step object
     '((succeeded :succeeded agent-repl-wire-decode-workspace-nuke-succeeded)
       (failed :failed agent-repl-wire-decode-workspace-nuke-failed)))))

(defun agent-repl-wire-decode-workspace-mutation-progress-create (value)
  "Decode `WorkspaceMutationProgress''s `create' event arm from VALUE."
  (agent-repl-wire-decode-workspace-create-progress value))

(defun agent-repl-wire-decode-workspace-mutation-progress-open (value)
  "Decode `WorkspaceMutationProgress''s `open' event arm from VALUE."
  (agent-repl-wire-decode-workspace-open-progress value))

(defun agent-repl-wire-decode-workspace-mutation-progress-kill (value)
  "Decode `WorkspaceMutationProgress''s `kill' event arm from VALUE."
  (agent-repl-wire-decode-workspace-kill-progress value))

(defun agent-repl-wire-decode-workspace-mutation-progress-nuke (value)
  "Decode `WorkspaceMutationProgress''s `nuke' event arm from VALUE."
  (agent-repl-wire-decode-workspace-nuke-progress value))

(defun agent-repl-wire-decode-workspace-mutation-progress (value)
  "Decode `WorkspaceMutationProgress' from VALUE.
Returns `(:op-id ID :event (:arm ARM :value V))'.  THE OP ID IS THE
CORRELATION KEY: a client matches this push to the operation it issued by
it, and drops any it does not recognize."
  (let ((object (agent-repl-wire--object "WorkspaceMutationProgress" value)))
    (agent-repl-wire--check-keys "WorkspaceMutationProgress" object '(opId create open kill nuke))
    (agent-repl-wire--decoded
     "WorkspaceMutationProgress"
     (list :op-id (agent-repl-wire--decode-string "WorkspaceMutationProgress" 'opId object)
           :event (agent-repl-wire--decode-oneof
                   "WorkspaceMutationProgress" 'event object
                   '((create :create
                             agent-repl-wire-decode-workspace-mutation-progress-create)
                     (open :open
                           agent-repl-wire-decode-workspace-mutation-progress-open)
                     (kill :kill
                           agent-repl-wire-decode-workspace-mutation-progress-kill)
                     (nuke :nuke
                           agent-repl-wire-decode-workspace-mutation-progress-nuke)))))))

(defun agent-repl-wire-decode-daemon-standing-fault-fault (value)
  "Decode `DaemonStandingFault''s `fault' field VALUE as a `DaemonFault'.
The typed fault is DaemonHealth's own, decoded by the one codec it has."
  (agent-repl-wire-decode-daemon-fault value))

(defun agent-repl-wire-decode-daemon-standing-fault (value)
  "Decode VALUE as `DaemonStandingFault'.
Returns `(:fault-id ID :line LINE :fault FAULT :opened-at-ms MS)'.  THE ID
AND THE LINE ARE REQUIRED: the id is what a client surfaces a fault once
by, and the line is the one sentence it shows, so an empty one is a
contract breach rather than a fault to echo as a blank."
  (let ((object (agent-repl-wire--object "DaemonStandingFault" value)))
    (agent-repl-wire--check-keys "DaemonStandingFault" object
                                 '(faultId line fault openedAtMs))
    (let ((id (agent-repl-wire--decode-string "DaemonStandingFault" 'faultId object))
          (line (agent-repl-wire--decode-string "DaemonStandingFault" 'line object)))
      (when (string-empty-p id)
        (agent-repl-wire--fail "DaemonStandingFault" 'faultId "required string is empty"))
      (when (string-empty-p line)
        (agent-repl-wire--fail "DaemonStandingFault" 'line "required string is empty"))
      (agent-repl-wire--decoded
       "DaemonStandingFault"
       (list :fault-id id
             :line line
             :fault (agent-repl-wire--decode-message
                     "DaemonStandingFault" 'fault object
                     #'agent-repl-wire-decode-daemon-standing-fault-fault)
             :opened-at-ms (agent-repl-wire--decode-int64
                            "DaemonStandingFault" 'openedAtMs object))))))

(defun agent-repl-wire-decode-daemon-faults-standing-faults (value)
  "Decode one element of `DaemonFaultsStanding''s repeated `faults' VALUE."
  (agent-repl-wire-decode-daemon-standing-fault value))

(defun agent-repl-wire-decode-daemon-faults-standing (value)
  "Decode VALUE as `DaemonFaultsStanding', a plist `(:faults LIST)'.
The daemon's WHOLE standing set of loud faults, oldest first; an empty
list is the daemon saying none stands."
  (let ((object (agent-repl-wire--object "DaemonFaultsStanding" value)))
    (agent-repl-wire--check-keys "DaemonFaultsStanding" object '(faults))
    (agent-repl-wire--decoded
     "DaemonFaultsStanding"
     (list :faults (agent-repl-wire--decode-repeated
                    "DaemonFaultsStanding" 'faults object
                    #'agent-repl-wire-decode-daemon-faults-standing-faults)))))

(defun agent-repl-wire-decode-watch-daemon-response-faults-standing (value)
  "Decode `WatchDaemonResponse''s `faults_standing' push arm VALUE."
  (agent-repl-wire-decode-daemon-faults-standing value))

;;;; ---- OpenTabs (endpoint_open_tabs.proto) ----

(defun agent-repl-wire--decode-open-tabs-ref (message-name object)
  "Decode OBJECT's REQUIRED `workspace' WorkspaceRef of MESSAGE-NAME."
  (agent-repl-wire--decode-message message-name 'workspace object
                                   #'agent-repl-wire-decode-workspace-ref))

(defun agent-repl-wire-encode-open-tabs-page-loading (value)
  "Encode VALUE as the empty `OpenTabsPageLoading'."
  (agent-repl-wire--encode-empty "OpenTabsPageLoading" value))

(defun agent-repl-wire-encode-open-tabs-page-none (value)
  "Encode VALUE as the empty `OpenTabsPageNone'."
  (agent-repl-wire--encode-empty "OpenTabsPageNone" value))

(defun agent-repl-wire-encode-open-tabs-workspace (value)
  "Encode the OpenTabsWorkspace plist VALUE `(:workspace REF :page ONEOF)'.
THE PAGE ARM IS REQUIRED: `:loading' when this Emacs is loading the
workspace's web page, `:none' when it draws none."
  (unless (plist-member value :workspace)
    (agent-repl-wire--fail "OpenTabsWorkspace" 'workspace "required message field is absent"))
  (agent-repl-wire--encoded
   "OpenTabsWorkspace"
   (cons (cons 'workspace (agent-repl-wire-encode-workspace-ref (plist-get value :workspace)))
         (agent-repl-wire--encode-oneof
          "OpenTabsWorkspace" 'page (plist-get value :page)
          '((:loading loading agent-repl-wire-encode-open-tabs-page-loading)
            (:none none agent-repl-wire-encode-open-tabs-page-none))))))

(defun agent-repl-wire-encode-open-tabs-request (value)
  "Encode the OpenTabsRequest plist VALUE `(:workspaces LIST)'.
An empty list is refused here: the daemon would refuse it as malformed."
  (let ((workspaces (plist-get value :workspaces)))
    (unless (and (listp workspaces) workspaces)
      (agent-repl-wire--fail "OpenTabsRequest" 'workspaces "expected a non-empty list"))
    (agent-repl-wire--encoded
     "OpenTabsRequest"
     (list (cons 'workspaces
                 (agent-repl-wire--encode-repeated
                  "OpenTabsRequest" 'workspaces workspaces
                  #'agent-repl-wire-encode-open-tabs-workspace))))))

(defun agent-repl-wire-decode-open-tabs-step-vendor-retrying (value)
  "Decode VALUE as `OpenTabsStepVendorRetrying', a plist `(:attempt N)'."
  (let ((object (agent-repl-wire--object "OpenTabsStepVendorRetrying" value)))
    (agent-repl-wire--check-keys "OpenTabsStepVendorRetrying" object '(attempt))
    (list :attempt (agent-repl-wire--decode-uint32
                    "OpenTabsStepVendorRetrying" 'attempt object))))

(defun agent-repl-wire-decode-open-tabs-step-vendor-rejected (value)
  "Decode VALUE as `OpenTabsStepVendorRejected', a plist `(:cause S)'."
  (let ((object (agent-repl-wire--object "OpenTabsStepVendorRejected" value)))
    (agent-repl-wire--check-keys "OpenTabsStepVendorRejected" object '(cause))
    (list :cause (agent-repl-wire--decode-string
                  "OpenTabsStepVendorRejected" 'cause object))))

(defmacro agent-repl-wire--define-empty-decoder (name message)
  "Define NAME decoding the empty MESSAGE."
  `(defun ,name (value)
     ,(format "Decode VALUE as the empty `%s'." message)
     (agent-repl-wire--decode-empty ,message value)))

(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-step-starting-session "OpenTabsStepStartingSession")
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-step-waking "OpenTabsStepWaking")
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-step-resuming "OpenTabsStepResuming")
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-step-vendor-failed "OpenTabsStepVendorFailed")
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-step-cold-gate "OpenTabsStepColdGate")
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-step-offline "OpenTabsStepOffline")
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-step-served "OpenTabsStepServed")
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-step-page-connected "OpenTabsStepPageConnected")
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-step-page-drawn "OpenTabsStepPageDrawn")
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-step-conversation-read "OpenTabsStepConversationRead")
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-step-resuming-cut-turn "OpenTabsStepResumingCutTurn")

(defconst agent-repl-wire-open-tabs-step-arms
  '((startingSession :starting-session agent-repl-wire-decode-open-tabs-step-starting-session)
    (waking :waking agent-repl-wire-decode-open-tabs-step-waking)
    (resuming :resuming agent-repl-wire-decode-open-tabs-step-resuming)
    (vendorRetrying :vendor-retrying agent-repl-wire-decode-open-tabs-step-vendor-retrying)
    (vendorRejected :vendor-rejected agent-repl-wire-decode-open-tabs-step-vendor-rejected)
    (vendorFailed :vendor-failed agent-repl-wire-decode-open-tabs-step-vendor-failed)
    (coldGate :cold-gate agent-repl-wire-decode-open-tabs-step-cold-gate)
    (offline :offline agent-repl-wire-decode-open-tabs-step-offline)
    (served :served agent-repl-wire-decode-open-tabs-step-served)
    (pageConnected :page-connected agent-repl-wire-decode-open-tabs-step-page-connected)
    (pageDrawn :page-drawn agent-repl-wire-decode-open-tabs-step-page-drawn)
    (conversationRead :conversation-read agent-repl-wire-decode-open-tabs-step-conversation-read)
    (resumingCutTurn :resuming-cut-turn agent-repl-wire-decode-open-tabs-step-resuming-cut-turn))
  "`OpenTabsStep.step''s arms: (WIRE-KEY ARM-KEYWORD DECODER).")

(defun agent-repl-wire-decode-open-tabs-step (value)
  "Decode VALUE as `OpenTabsStep', a plist `(:workspace REF :at-ms N :step ONEOF)'."
  (let ((object (agent-repl-wire--object "OpenTabsStep" value)))
    (agent-repl-wire--check-keys
     "OpenTabsStep" object
     (append '(workspace atMs) (mapcar #'car agent-repl-wire-open-tabs-step-arms)))
    (agent-repl-wire--decoded
     "OpenTabsStep"
     (list :workspace (agent-repl-wire--decode-open-tabs-ref "OpenTabsStep" object)
           :at-ms (agent-repl-wire--decode-int64 "OpenTabsStep" 'atMs object)
           :step (agent-repl-wire--decode-oneof
                  "OpenTabsStep" 'step object agent-repl-wire-open-tabs-step-arms)))))

(defun agent-repl-wire-decode-open-tabs-opened-ready (value)
  "Decode VALUE as `OpenTabsOpenedReady', `(:row ROW :roster-revision N)'.
ROW is the workspace's decoded roster row as of the answer, REQUIRED."
  (let ((object (agent-repl-wire--object "OpenTabsOpenedReady" value)))
    (agent-repl-wire--check-keys "OpenTabsOpenedReady" object '(row rosterRevision))
    (list :row (agent-repl-wire--decode-message "OpenTabsOpenedReady" 'row object
                                                #'agent-repl-wire-decode-roster-row)
          :roster-revision (agent-repl-wire--decode-uint64
                            "OpenTabsOpenedReady" 'rosterRevision object))))
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-opened-hidden "OpenTabsOpenedHidden")

(defun agent-repl-wire-decode-open-tabs-opened-failed (value)
  "Decode VALUE as `OpenTabsOpenedFailed'.
Returns `(:reason S :row ROW :roster-revision N)'."
  (let ((object (agent-repl-wire--object "OpenTabsOpenedFailed" value)))
    (agent-repl-wire--check-keys "OpenTabsOpenedFailed" object '(reason row rosterRevision))
    (list :reason (agent-repl-wire--decode-string "OpenTabsOpenedFailed" 'reason object)
          :row (agent-repl-wire--decode-message "OpenTabsOpenedFailed" 'row object
                                                #'agent-repl-wire-decode-roster-row)
          :roster-revision (agent-repl-wire--decode-uint64
                            "OpenTabsOpenedFailed" 'rosterRevision object))))

(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-refused-unknown-workspace "OpenTabsRefusedUnknownWorkspace")
(agent-repl-wire--define-empty-decoder
 agent-repl-wire-decode-open-tabs-refused-not-yet-adopted "OpenTabsRefusedNotYetAdopted")

(defun agent-repl-wire-decode-open-tabs-refused-workspace-ref-mismatch (value)
  "Decode VALUE as `OpenTabsRefusedWorkspaceRefMismatch', `(:registry-dir S)'."
  (let ((object (agent-repl-wire--object "OpenTabsRefusedWorkspaceRefMismatch" value)))
    (agent-repl-wire--check-keys "OpenTabsRefusedWorkspaceRefMismatch" object '(registryDir))
    (list :registry-dir (agent-repl-wire--decode-string
                         "OpenTabsRefusedWorkspaceRefMismatch" 'registryDir object))))

(defun agent-repl-wire-decode-open-tabs-refused-transferring-away (value)
  "Decode VALUE as `OpenTabsRefusedTransferringAway', `(:address S)'."
  (let ((object (agent-repl-wire--object "OpenTabsRefusedTransferringAway" value)))
    (agent-repl-wire--check-keys "OpenTabsRefusedTransferringAway" object '(address))
    (list :address (agent-repl-wire--decode-string
                    "OpenTabsRefusedTransferringAway" 'address object))))

(defconst agent-repl-wire-open-tabs-refused-arms
  '((unknownWorkspace :unknown-workspace agent-repl-wire-decode-open-tabs-refused-unknown-workspace)
    (workspaceRefMismatch :workspace-ref-mismatch
                          agent-repl-wire-decode-open-tabs-refused-workspace-ref-mismatch)
    (transferringAway :transferring-away agent-repl-wire-decode-open-tabs-refused-transferring-away)
    (notYetAdopted :not-yet-adopted agent-repl-wire-decode-open-tabs-refused-not-yet-adopted))
  "`OpenTabsOpenedRefused.cause''s arms: (WIRE-KEY ARM-KEYWORD DECODER).")

(defun agent-repl-wire-decode-open-tabs-opened-refused (value)
  "Decode VALUE as `OpenTabsOpenedRefused', a plist `(:cause ONEOF)'."
  (let ((object (agent-repl-wire--object "OpenTabsOpenedRefused" value)))
    (agent-repl-wire--check-keys
     "OpenTabsOpenedRefused" object (mapcar #'car agent-repl-wire-open-tabs-refused-arms))
    (list :cause (agent-repl-wire--decode-oneof
                  "OpenTabsOpenedRefused" 'cause object
                  agent-repl-wire-open-tabs-refused-arms))))

(defconst agent-repl-wire-open-tabs-opened-arms
  '((ready :ready agent-repl-wire-decode-open-tabs-opened-ready)
    (failed :failed agent-repl-wire-decode-open-tabs-opened-failed)
    (hidden :hidden agent-repl-wire-decode-open-tabs-opened-hidden)
    (refused :refused agent-repl-wire-decode-open-tabs-opened-refused))
  "`OpenTabsOpened.outcome''s arms: (WIRE-KEY ARM-KEYWORD DECODER).")

(defun agent-repl-wire-decode-open-tabs-opened (value)
  "Decode VALUE as `OpenTabsOpened', `(:workspace REF :at-ms N :outcome ONEOF)'."
  (let ((object (agent-repl-wire--object "OpenTabsOpened" value)))
    (agent-repl-wire--check-keys
     "OpenTabsOpened" object
     (append '(workspace atMs) (mapcar #'car agent-repl-wire-open-tabs-opened-arms)))
    (agent-repl-wire--decoded
     "OpenTabsOpened"
     (list :workspace (agent-repl-wire--decode-open-tabs-ref "OpenTabsOpened" object)
           :at-ms (agent-repl-wire--decode-int64 "OpenTabsOpened" 'atMs object)
           :outcome (agent-repl-wire--decode-oneof
                     "OpenTabsOpened" 'outcome object
                     agent-repl-wire-open-tabs-opened-arms)))))

(defun agent-repl-wire-decode-open-tabs-failed-workspace (value)
  "Decode VALUE as `OpenTabsFailedWorkspace', a plist `(:workspace REF :name S)'."
  (let ((object (agent-repl-wire--object "OpenTabsFailedWorkspace" value)))
    (agent-repl-wire--check-keys "OpenTabsFailedWorkspace" object '(workspace name))
    (list :workspace (agent-repl-wire--decode-open-tabs-ref "OpenTabsFailedWorkspace" object)
          :name (agent-repl-wire--decode-string "OpenTabsFailedWorkspace" 'name object))))

(defun agent-repl-wire-decode-open-tabs-finished (value)
  "Decode VALUE as `OpenTabsFinished', a plist `(:ready N :total N :failed LIST)'."
  (let ((object (agent-repl-wire--object "OpenTabsFinished" value)))
    (agent-repl-wire--check-keys "OpenTabsFinished" object '(ready total failed))
    (agent-repl-wire--decoded
     "OpenTabsFinished"
     (list :ready (agent-repl-wire--decode-uint32 "OpenTabsFinished" 'ready object)
           :total (agent-repl-wire--decode-uint32 "OpenTabsFinished" 'total object)
           :failed (agent-repl-wire--decode-repeated
                    "OpenTabsFinished" 'failed object
                    #'agent-repl-wire-decode-open-tabs-failed-workspace)))))

(defconst agent-repl-wire-open-tabs-frame-arms
  '((step :step agent-repl-wire-decode-open-tabs-step)
    (opened :opened agent-repl-wire-decode-open-tabs-opened)
    (finished :finished agent-repl-wire-decode-open-tabs-finished))
  "`OpenTabsResponse.frame''s arms: (WIRE-KEY ARM-KEYWORD DECODER).")

(defun agent-repl-wire-decode-open-tabs-response (value)
  "Decode VALUE as `OpenTabsResponse', the frame oneof plist.
THE ARM IS THE FRAME; an unset one is a contract breach."
  (let ((object (agent-repl-wire--object "OpenTabsResponse" value)))
    (agent-repl-wire--check-keys
     "OpenTabsResponse" object (mapcar #'car agent-repl-wire-open-tabs-frame-arms))
    (agent-repl-wire--decoded
     "OpenTabsResponse"
     (agent-repl-wire--decode-oneof
      "OpenTabsResponse" 'frame object agent-repl-wire-open-tabs-frame-arms))))

(defun agent-repl-wire-decode-watch-daemon-response-push (value)
  "Decode `WatchDaemonResponse''s `push' oneof from the object VALUE."
  (agent-repl-wire--decode-oneof
   "WatchDaemonResponse" 'push value
   '((shutdownAnnounced :shutdown-announced
                        agent-repl-wire-decode-daemon-shutdown-announced)
     (drainScheduled :drain-scheduled agent-repl-wire-decode-daemon-drain-scheduled)
     (drainCancelled :drain-cancelled agent-repl-wire-decode-daemon-drain-cancelled)
     (mutationProgress :mutation-progress
                       agent-repl-wire-decode-workspace-mutation-progress)
     (reloadElisp :reload-elisp agent-repl-wire-decode-watch-daemon-response-reload-elisp)
     (ending :ending agent-repl-wire-decode-watch-daemon-response-ending)
     (faultsStanding :faults-standing
                     agent-repl-wire-decode-watch-daemon-response-faults-standing)
     (persistentWifi :persistent-wifi
                     agent-repl-wire-decode-watch-daemon-response-persistent-wifi)
     (reloadWebhost :reload-webhost agent-repl-wire-decode-daemon-reload-webhost))))

(defun agent-repl-wire-decode-watch-daemon-response-persistent-wifi (value)
  "Decode `WatchDaemonResponse''s `persistent_wifi' push arm VALUE.
The machine's standing, decoded by the one codec it has (shared with
UpdatePersistentWifiMode's success)."
  (agent-repl-wire-decode-persistent-wifi-state value))

(defun agent-repl-wire-decode-watch-daemon-response-ending (value)
  "Decode `WatchDaemonResponse''s `ending' push arm VALUE."
  (agent-repl-wire-decode-daemon-stream-ending value))

(defun agent-repl-wire-decode-watch-daemon-response-reload-elisp (value)
  "Decode `WatchDaemonResponse''s `reload_elisp' push arm VALUE."
  (agent-repl-wire-decode-daemon-reload-elisp value))

(defun agent-repl-wire-decode-watch-daemon-response (value)
  "Decode VALUE as `WatchDaemonResponse', the push oneof plist."
  (let ((object (agent-repl-wire--object "WatchDaemonResponse" value)))
    (agent-repl-wire--check-keys
     "WatchDaemonResponse" object
     '(shutdownAnnounced drainScheduled drainCancelled mutationProgress reloadElisp
       ending faultsStanding persistentWifi reloadWebhost))
    (agent-repl-wire--decoded
     "WatchDaemonResponse"
     (agent-repl-wire-decode-watch-daemon-response-push object))))

;;;; ---- ListSlashCommands ----

(defun agent-repl-wire-encode-list-slash-commands-request-workspace (value)
  "Encode `ListSlashCommandsRequest''s `workspace' field VALUE."
  (agent-repl-wire-encode-workspace-ref value))

(defun agent-repl-wire-encode-list-slash-commands-request (value)
  "Encode the ListSlashCommandsRequest plist VALUE `(:workspace REF)'.
The composer asks for every slash command the workspace's runtime answers."
  (unless (plist-member value :workspace)
    (agent-repl-wire--fail "ListSlashCommandsRequest" 'workspace
                           "required message field is absent"))
  (agent-repl-wire--encoded
   "ListSlashCommandsRequest"
   (list (cons 'workspace
               (agent-repl-wire-encode-list-slash-commands-request-workspace
                (plist-get value :workspace))))))

(defun agent-repl-wire-decode-session-command-catalog-entry-alias (value)
  "Decode one `SessionCommandCatalogEntry.aliases' element VALUE, a string."
  (if (stringp value)
      value
    (agent-repl-wire--fail "SessionCommandCatalogEntry" 'aliases "expected a string")))

(defun agent-repl-wire-decode-session-command-catalog-entry (value)
  "Decode VALUE as `SessionCommandCatalogEntry', a plist
\(:name :description :argument-hint :aliases).
One command the runtime answers: its name WITHOUT the leading slash, and
the runtime's own description and argument hint, nil when it states none."
  (let* ((message "SessionCommandCatalogEntry")
         (object (agent-repl-wire--object message value)))
    (agent-repl-wire--check-keys message object '(name description argumentHint aliases))
    (let ((name (agent-repl-wire--decode-string message 'name object)))
      (when (string-empty-p name)
        (agent-repl-wire--fail message 'name "a command name is never empty"))
      (agent-repl-wire--decoded
       message
       (list :name name
             :description (agent-repl-wire--decode-optional-string message 'description object)
             :argument-hint (agent-repl-wire--decode-optional-string message 'argumentHint object)
             :aliases (agent-repl-wire--decode-repeated
                       message 'aliases object
                       #'agent-repl-wire-decode-session-command-catalog-entry-alias))))))

(defun agent-repl-wire-decode-session-command-catalog (value)
  "Decode VALUE as `SessionCommandCatalog', a plist (:commands LIST).
The runtime's whole catalog, in the order the runtime listed it."
  (let* ((message "SessionCommandCatalog")
         (object (agent-repl-wire--object message value)))
    (agent-repl-wire--check-keys message object '(commands))
    (agent-repl-wire--decoded
     message
     (list :commands (agent-repl-wire--decode-repeated
                      message 'commands object
                      #'agent-repl-wire-decode-session-command-catalog-entry)))))

(defun agent-repl-wire-decode-list-slash-commands-stated (value)
  "Decode VALUE as `ListSlashCommandsStated', a plist (:catalog CATALOG)."
  (let* ((message "ListSlashCommandsStated")
         (object (agent-repl-wire--object message value)))
    (agent-repl-wire--check-keys message object '(catalog))
    (agent-repl-wire--decoded
     message
     (list :catalog (agent-repl-wire--decode-message
                     message 'catalog object
                     #'agent-repl-wire-decode-session-command-catalog)))))

(defun agent-repl-wire-decode-list-slash-commands-unstated (value)
  "Decode VALUE as the empty message `ListSlashCommandsUnstated'.
No session of the workspace has stated its catalog yet."
  (agent-repl-wire--decode-empty "ListSlashCommandsUnstated" value))

(defun agent-repl-wire-decode-list-slash-commands-success (value)
  "Decode VALUE as `ListSlashCommandsSuccess', the `catalog' oneof plist.
UNSTATED IS AN ANSWER: it arrives inside success, never as an error."
  (let* ((message "ListSlashCommandsSuccess")
         (object (agent-repl-wire--object message value)))
    (agent-repl-wire--check-keys message object '(stated unstated))
    (agent-repl-wire--decoded
     message
     (agent-repl-wire--decode-oneof
      message 'catalog object
      '((stated :stated agent-repl-wire-decode-list-slash-commands-stated)
        (unstated :unstated agent-repl-wire-decode-list-slash-commands-unstated))))))

(defun agent-repl-wire-decode-list-slash-commands-workspace-ref-mismatch (value)
  "Decode VALUE as `ListSlashCommandsWorkspaceRefMismatch'.
The answer is a plist (`:registry-dir'): the echoed dir disagrees with the
registry's dir for this id."
  (let ((object (agent-repl-wire--object "ListSlashCommandsWorkspaceRefMismatch" value)))
    (agent-repl-wire--check-keys "ListSlashCommandsWorkspaceRefMismatch" object '(registryDir))
    (agent-repl-wire--decoded
     "ListSlashCommandsWorkspaceRefMismatch"
     (list :registry-dir (agent-repl-wire--decode-string
                          "ListSlashCommandsWorkspaceRefMismatch" 'registryDir object)))))

(defun agent-repl-wire-decode-list-slash-commands-transferring-away (value)
  "Decode VALUE as `ListSlashCommandsTransferringAway', a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((object (agent-repl-wire--object "ListSlashCommandsTransferringAway" value)))
    (agent-repl-wire--check-keys "ListSlashCommandsTransferringAway" object '(address))
    (agent-repl-wire--decoded
     "ListSlashCommandsTransferringAway"
     (list :address (agent-repl-wire--decode-string
                     "ListSlashCommandsTransferringAway" 'address object)))))

(defun agent-repl-wire-decode-list-slash-commands-error-unknown-workspace (value)
  "Decode ListSlashCommandsError's `unknown_workspace' cause arm from VALUE.
The workspace id is not in the daemon's registry."
  (agent-repl-wire--decode-empty "ListSlashCommandsUnknownWorkspace" value))

(defun agent-repl-wire-decode-list-slash-commands-error-workspace-ref-mismatch (value)
  "Decode ListSlashCommandsError's `workspace_ref_mismatch' cause arm from VALUE."
  (agent-repl-wire-decode-list-slash-commands-workspace-ref-mismatch value))

(defun agent-repl-wire-decode-list-slash-commands-error-transferring-away (value)
  "Decode ListSlashCommandsError's `transferring_away' cause arm from VALUE."
  (agent-repl-wire-decode-list-slash-commands-transferring-away value))

(defun agent-repl-wire-decode-list-slash-commands-error-not-yet-adopted (value)
  "Decode ListSlashCommandsError's `not_yet_adopted' cause arm from VALUE.
A joining daemon has not finished adopting this workspace yet."
  (agent-repl-wire--decode-empty "ListSlashCommandsNotYetAdopted" value))

(defun agent-repl-wire-decode-list-slash-commands-error (value)
  "Decode VALUE as `ListSlashCommandsError', a plist (:cause ONEOF).
THE ARM IS THE REFUSAL, so an unset cause is a contract breach."
  (let ((object (agent-repl-wire--object "ListSlashCommandsError" value)))
    (agent-repl-wire--check-keys "ListSlashCommandsError" object
                                 '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted))
    (agent-repl-wire--decoded
     "ListSlashCommandsError"
     (list :cause
           (agent-repl-wire--decode-oneof
            "ListSlashCommandsError" 'cause object
            '((unknownWorkspace :unknown-workspace agent-repl-wire-decode-list-slash-commands-error-unknown-workspace)
              (workspaceRefMismatch :workspace-ref-mismatch agent-repl-wire-decode-list-slash-commands-error-workspace-ref-mismatch)
              (transferringAway :transferring-away agent-repl-wire-decode-list-slash-commands-error-transferring-away)
              (notYetAdopted :not-yet-adopted agent-repl-wire-decode-list-slash-commands-error-not-yet-adopted)))))))

(defun agent-repl-wire-decode-list-slash-commands-response (value)
  "Decode VALUE as `ListSlashCommandsResponse'.  THE ARM IS THE OUTCOME."
  (let ((object (agent-repl-wire--object "ListSlashCommandsResponse" value)))
    (agent-repl-wire--check-keys "ListSlashCommandsResponse" object '(success error))
    (agent-repl-wire--decoded
     "ListSlashCommandsResponse"
     (agent-repl-wire--decode-oneof
      "ListSlashCommandsResponse" 'result object
      '((success :success agent-repl-wire-decode-list-slash-commands-success)
        (error :error agent-repl-wire-decode-list-slash-commands-error))))))

(provide 'wire-host)

;;; wire-host.el ends here
