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
(declare-function agent-repl-wire--decode-bool "wire-common")
(declare-function agent-repl-wire--decode-empty "wire-common")
(declare-function agent-repl-wire--decode-int64 "wire-common")
(declare-function agent-repl-wire--decode-message "wire-common")
(declare-function agent-repl-wire--decode-oneof "wire-common")
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
(declare-function agent-repl-wire--encode-string "wire-common")
(declare-function agent-repl-wire--encoded "wire-common")
(declare-function agent-repl-wire--fail "wire-common")
(declare-function agent-repl-wire--object "wire-common")
(declare-function agent-repl-wire-decode-drain-reason "wire-common")
(declare-function agent-repl-wire-decode-workspace-ref "wire-common")
(declare-function agent-repl-wire-decode-daemon-stream-ending "wire-common")
(declare-function agent-repl-wire-decode-create-workspace-error "wire-verbs")
(declare-function agent-repl-wire-decode-nuke-workspace-error "wire-verbs")
(declare-function agent-repl-wire-decode-daemon-fault "wire-verbs")
(declare-function agent-repl-wire--raw "wire-common")
(declare-function agent-repl-wire-decode-session-fault-bounce-died "wire-common")
(declare-function agent-repl-wire-decode-session-fault-bounce-unknown "wire-common")
(declare-function agent-repl-wire-decode-session-fault-adoption-window-expired "wire-common")
(declare-function agent-repl-wire-decode-session-fault-final-answer-unresolved "wire-common")
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
       (finalAnswerUnresolved :final-answer-unresolved agent-repl-wire-decode-host-fault-final-answer-unresolved)))))

(defun agent-repl-wire-decode-host-fault (value)
  "Decode VALUE as `HostFault', a plist `(:detail :opened-at-ms :kind)'.
The thirteen kinds are the session controller's own fault vocabulary, shared
verbatim with SessionHealth's `SessionFault' — the stream reporting a
fault never changes its class."
  (let ((object (agent-repl-wire--object "HostFault" value)))
    (agent-repl-wire--check-keys
     "HostFault" object '(detail openedAtMs shimStartFailed shimDied linkSevered resumeFailed bounceDied bounceUnknown classifierFailed shimReported conversationAbandoned sessionAbsent watchOpenRefused daemonStateUnreadable adoptionWindowExpired finalAnswerUnresolved))
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

(defun agent-repl-wire-decode-host-workspace (value)
  "Decode VALUE as `HostWorkspace', a plist `(:session :naming :held-prompt-edit)'.
`naming' is a REQUIRED message sitting beside the session oneof; its two
fields are the optional halves, not the message.  `held_prompt_edit' is
absent when no held-prompt edit stands, and its absence is that fact."
  (let ((object (agent-repl-wire--object "HostWorkspace" value)))
    (agent-repl-wire--check-keys "HostWorkspace" object '(none existing naming heldPromptEdit))
    (agent-repl-wire--decoded
     "HostWorkspace"
     (list :session (agent-repl-wire-decode-host-workspace-session object)
           :naming (agent-repl-wire--decode-message
                    "HostWorkspace" 'naming object
                    #'agent-repl-wire-decode-host-workspace-naming)
           :held-prompt-edit (agent-repl-wire--decode-optional-message
                              "HostWorkspace" 'heldPromptEdit object
                              #'agent-repl-wire-decode-host-held-prompt-edit)))))

;;;; ---- The notification click push ----

(defun agent-repl-wire-decode-host-workspace-notification-clicked (value)
  "Decode VALUE as the empty message `HostWorkspaceNotificationClicked'.
The user clicked this workspace's desktop banner; the daemon posted it
and read the click back, so selecting the tab is all that is left."
  (agent-repl-wire--decode-empty "HostWorkspaceNotificationClicked" value))

;;;; ---- The selection push ----

(defun agent-repl-wire-decode-host-workspace-selection (value)
  "Decode VALUE as `HostWorkspaceSelection', the kind keyword it names.
One of `:none', `:response' and `:prompt': which kind of row the feed has
selected, never the row, because Emacs never names it back.  Every arm is
empty, so the arm IS the whole fact; an unset or doubled oneof is a
contract breach."
  (let ((object (agent-repl-wire--object "HostWorkspaceSelection" value)))
    (agent-repl-wire--check-keys "HostWorkspaceSelection" object '(none response prompt))
    (plist-get
     (agent-repl-wire--decoded
      "HostWorkspaceSelection"
      (agent-repl-wire--decode-oneof
       "HostWorkspaceSelection" 'selection object
       '((none :none agent-repl-wire-decode-host-workspace-selection-none)
         (response :response agent-repl-wire-decode-host-workspace-selection-response)
         (prompt :prompt agent-repl-wire-decode-host-workspace-selection-prompt))))
     :arm)))

(defun agent-repl-wire-decode-host-workspace-selection-none (value)
  "Decode VALUE as the empty message `HostWorkspaceSelectionNone'."
  (agent-repl-wire--decode-empty "HostWorkspaceSelectionNone" value))

(defun agent-repl-wire-decode-host-workspace-selection-response (value)
  "Decode VALUE as the empty message `HostWorkspaceSelectionResponse'."
  (agent-repl-wire--decode-empty "HostWorkspaceSelectionResponse" value))

(defun agent-repl-wire-decode-host-workspace-selection-prompt (value)
  "Decode VALUE as the empty message `HostWorkspaceSelectionPrompt'."
  (agent-repl-wire--decode-empty "HostWorkspaceSelectionPrompt" value))

;;;; ---- The remaining WatchHostWorkspace push arms ----

(defun agent-repl-wire-decode-host-workspace-transferred (value)
  "Decode VALUE as the empty message `HostWorkspaceTransferred'."
  (agent-repl-wire--decode-empty "HostWorkspaceTransferred" value))

(defun agent-repl-wire-decode-host-workspace-reload-webapp (value)
  "Decode VALUE as the empty message `HostWorkspaceReloadWebapp'."
  (agent-repl-wire--decode-empty "HostWorkspaceReloadWebapp" value))

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

(defun agent-repl-wire-decode-watch-host-workspace-response-selection (value)
  "Decode the `selection' push arm VALUE as its kind keyword."
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
     (ending :ending agent-repl-wire-decode-watch-host-workspace-response-ending)
     (notificationClicked :notification-clicked
                          agent-repl-wire-decode-watch-host-workspace-response-notification-clicked)
     (selection :selection agent-repl-wire-decode-watch-host-workspace-response-selection))))

(defun agent-repl-wire-decode-watch-host-workspace-response (value)
  "Decode VALUE as `WatchHostWorkspaceResponse', the push oneof plist."
  (let ((object (agent-repl-wire--object "WatchHostWorkspaceResponse" value)))
    (agent-repl-wire--check-keys
     "WatchHostWorkspaceResponse" object
     '(host transferred reloadWebapp openInEditor ending notificationClicked selection))
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

(defun agent-repl-wire-decode-register-workspace-error-cause (value)
  "Decode RegisterWorkspaceError's `cause' oneof from the object VALUE.
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((object (agent-repl-wire--object "RegisterWorkspaceError" value)))
    (agent-repl-wire--check-keys "RegisterWorkspaceError" object '(notAWorktree))
    (agent-repl-wire--decode-oneof
     "RegisterWorkspaceError" 'cause object
     '((notAWorktree :not-a-worktree agent-repl-wire-decode-register-workspace-error-not-a-worktree)))))

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
  "Encode the SelectWorkspaceRequest plist VALUE `(:workspace REF)'."
  (unless (plist-member value :workspace)
    (agent-repl-wire--fail "SelectWorkspaceRequest" 'workspace
                           "required message field is absent"))
  (agent-repl-wire--encoded
   "SelectWorkspaceRequest"
   (list (cons 'workspace
               (agent-repl-wire-encode-select-workspace-request-workspace
                (plist-get value :workspace))))))

(defun agent-repl-wire-decode-select-workspace-success (value)
  "Decode VALUE as the empty message `SelectWorkspaceSuccess'."
  (agent-repl-wire--decode-empty "SelectWorkspaceSuccess" value))

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
    (agent-repl-wire--check-keys "SelectWorkspaceError" object '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted))
    (agent-repl-wire--decode-oneof
     "SelectWorkspaceError" 'cause object
     '((unknownWorkspace :unknown-workspace agent-repl-wire-decode-select-workspace-error-unknown-workspace)
       (workspaceRefMismatch :workspace-ref-mismatch agent-repl-wire-decode-select-workspace-error-workspace-ref-mismatch)
       (transferringAway :transferring-away agent-repl-wire-decode-select-workspace-error-transferring-away)
       (notYetAdopted :not-yet-adopted agent-repl-wire-decode-select-workspace-error-not-yet-adopted)))))

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

;;;; ---- MarkWorkspaceViewed ----

(defun agent-repl-wire-encode-mark-workspace-viewed-request-workspace (value)
  "Encode `MarkWorkspaceViewedRequest''s `workspace' field VALUE."
  (agent-repl-wire-encode-workspace-ref value))

(defun agent-repl-wire-encode-mark-workspace-viewed-request (value)
  "Encode the MarkWorkspaceViewedRequest plist VALUE `(:workspace REF)'.
The editor's report that the user has now SEEN this workspace."
  (unless (plist-member value :workspace)
    (agent-repl-wire--fail "MarkWorkspaceViewedRequest" 'workspace
                           "required message field is absent"))
  (agent-repl-wire--encoded
   "MarkWorkspaceViewedRequest"
   (list (cons 'workspace
               (agent-repl-wire-encode-mark-workspace-viewed-request-workspace
                (plist-get value :workspace))))))

(defun agent-repl-wire-decode-mark-workspace-viewed-success (value)
  "Decode VALUE as the empty message `MarkWorkspaceViewedSuccess'.
Marked; the roster stream carries the row's viewed marker."
  (agent-repl-wire--decode-empty "MarkWorkspaceViewedSuccess" value))

(defun agent-repl-wire-decode-mark-workspace-viewed-unknown-workspace (value)
  "Decode VALUE as the empty message `MarkWorkspaceViewedUnknownWorkspace'.
The workspace id is not in the daemon's registry."
  (agent-repl-wire--decode-empty "MarkWorkspaceViewedUnknownWorkspace" value))

(defun agent-repl-wire-decode-mark-workspace-viewed-workspace-ref-mismatch (value)
  "Decode VALUE as `MarkWorkspaceViewedWorkspaceRefMismatch', a plist (`:registry-
dir').
The echoed dir disagrees with the registry's dir for this id."
  (let ((object (agent-repl-wire--object "MarkWorkspaceViewedWorkspaceRefMismatch" value)))
    (agent-repl-wire--check-keys "MarkWorkspaceViewedWorkspaceRefMismatch" object '(registryDir))
    (agent-repl-wire--decoded
     "MarkWorkspaceViewedWorkspaceRefMismatch"
     (list :registry-dir (agent-repl-wire--decode-string
                    "MarkWorkspaceViewedWorkspaceRefMismatch" 'registryDir object)))))

(defun agent-repl-wire-decode-mark-workspace-viewed-transferring-away (value)
  "Decode VALUE as `MarkWorkspaceViewedTransferringAway', a plist (`:address').
This daemon released the workspace to a successor; dial `address'."
  (let ((object (agent-repl-wire--object "MarkWorkspaceViewedTransferringAway" value)))
    (agent-repl-wire--check-keys "MarkWorkspaceViewedTransferringAway" object '(address))
    (agent-repl-wire--decoded
     "MarkWorkspaceViewedTransferringAway"
     (list :address (agent-repl-wire--decode-string
                    "MarkWorkspaceViewedTransferringAway" 'address object)))))

(defun agent-repl-wire-decode-mark-workspace-viewed-not-yet-adopted (value)
  "Decode VALUE as the empty message `MarkWorkspaceViewedNotYetAdopted'.
A joining daemon has not finished adopting this workspace yet."
  (agent-repl-wire--decode-empty "MarkWorkspaceViewedNotYetAdopted" value))

(defun agent-repl-wire-decode-mark-workspace-viewed-error-unknown-workspace (value)
  "Decode MarkWorkspaceViewedError's `unknown_workspace' cause arm from VALUE."
  (agent-repl-wire-decode-mark-workspace-viewed-unknown-workspace value))

(defun agent-repl-wire-decode-mark-workspace-viewed-error-workspace-ref-mismatch (value)
  "Decode MarkWorkspaceViewedError's `workspace_ref_mismatch' cause arm from VALUE."
  (agent-repl-wire-decode-mark-workspace-viewed-workspace-ref-mismatch value))

(defun agent-repl-wire-decode-mark-workspace-viewed-error-transferring-away (value)
  "Decode MarkWorkspaceViewedError's `transferring_away' cause arm from VALUE."
  (agent-repl-wire-decode-mark-workspace-viewed-transferring-away value))

(defun agent-repl-wire-decode-mark-workspace-viewed-error-not-yet-adopted (value)
  "Decode MarkWorkspaceViewedError's `not_yet_adopted' cause arm from VALUE."
  (agent-repl-wire-decode-mark-workspace-viewed-not-yet-adopted value))

(defun agent-repl-wire-decode-mark-workspace-viewed-error-cause (value)
  "Decode MarkWorkspaceViewedError's `cause' oneof from the object VALUE.
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((object (agent-repl-wire--object "MarkWorkspaceViewedError" value)))
    (agent-repl-wire--check-keys "MarkWorkspaceViewedError" object '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted))
    (agent-repl-wire--decode-oneof
     "MarkWorkspaceViewedError" 'cause object
     '((unknownWorkspace :unknown-workspace agent-repl-wire-decode-mark-workspace-viewed-error-unknown-workspace)
       (workspaceRefMismatch :workspace-ref-mismatch agent-repl-wire-decode-mark-workspace-viewed-error-workspace-ref-mismatch)
       (transferringAway :transferring-away agent-repl-wire-decode-mark-workspace-viewed-error-transferring-away)
       (notYetAdopted :not-yet-adopted agent-repl-wire-decode-mark-workspace-viewed-error-not-yet-adopted)))))

(defun agent-repl-wire-decode-mark-workspace-viewed-error (value)
  "Decode VALUE as `MarkWorkspaceViewedError', a plist (:cause ONEOF)."
  (agent-repl-wire--decoded
   "MarkWorkspaceViewedError"
   (list :cause (agent-repl-wire-decode-mark-workspace-viewed-error-cause value))))

(defun agent-repl-wire-decode-mark-workspace-viewed-response-result (value)
  "Decode `MarkWorkspaceViewedResponse''s `result' oneof from the object VALUE."
  (agent-repl-wire--decode-oneof
   "MarkWorkspaceViewedResponse" 'result value
   '((success :success agent-repl-wire-decode-mark-workspace-viewed-success)
     (error :error agent-repl-wire-decode-mark-workspace-viewed-error))))

(defun agent-repl-wire-decode-mark-workspace-viewed-response (value)
  "Decode VALUE as `MarkWorkspaceViewedResponse'.  THE ARM IS THE OUTCOME."
  (let ((object (agent-repl-wire--object "MarkWorkspaceViewedResponse" value)))
    (agent-repl-wire--check-keys "MarkWorkspaceViewedResponse" object '(success error))
    (agent-repl-wire--decoded
     "MarkWorkspaceViewedResponse"
     (agent-repl-wire-decode-mark-workspace-viewed-response-result object))))

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

(defun agent-repl-wire-decode-adopt-host-workspace-error-cause (value)
  "Decode AdoptHostWorkspaceError's `cause' oneof from the object VALUE.
THE ARM IS THE REFUSAL, so an unset cause is a contract breach and an
arm this codec does not know is refused as an unknown field."
  (let ((object (agent-repl-wire--object "AdoptHostWorkspaceError" value)))
    (agent-repl-wire--check-keys "AdoptHostWorkspaceError" object '(unknownWorkspace workspaceRefMismatch transferringAway notYetAdopted noTransferAnnounced participantNotExpected))
    (agent-repl-wire--decode-oneof
     "AdoptHostWorkspaceError" 'cause object
     '((unknownWorkspace :unknown-workspace agent-repl-wire-decode-adopt-host-workspace-error-unknown-workspace)
       (workspaceRefMismatch :workspace-ref-mismatch agent-repl-wire-decode-adopt-host-workspace-error-workspace-ref-mismatch)
       (transferringAway :transferring-away agent-repl-wire-decode-adopt-host-workspace-error-transferring-away)
       (notYetAdopted :not-yet-adopted agent-repl-wire-decode-adopt-host-workspace-error-not-yet-adopted)
       (noTransferAnnounced :no-transfer-announced agent-repl-wire-decode-adopt-host-workspace-error-no-transfer-announced)
       (participantNotExpected :participant-not-expected agent-repl-wire-decode-adopt-host-workspace-error-participant-not-expected)))))

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

(defun agent-repl-wire-decode-report-editor-focus-success (value)
  "Decode VALUE as the empty `ReportEditorFocusSuccess'."
  (agent-repl-wire--decode-empty "ReportEditorFocusSuccess" value))

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

(defun agent-repl-wire-encode-watch-daemon-emacs (value)
  "Encode `WatchDaemonEmacs' from the plist VALUE `(:elisp-build BUILD :focus F)'.
The build is REQUIRED and never empty: a watch without it is refused,
because a deploy could not tell whether this Emacs runs the checkout's
elisp.  An empty one is refused HERE, before anything is sent.  The focus
is REQUIRED too: the daemon decides every desktop banner on it from the
stream's first instant (`agent-repl-wire-encode-editor-focus')."
  (let ((build (plist-get value :elisp-build)))
    (unless (stringp build)
      (agent-repl-wire--fail "WatchDaemonEmacs" 'elispBuild "required field is unset"))
    (when (string-empty-p build)
      (agent-repl-wire--fail "WatchDaemonEmacs" 'elispBuild "required string is empty"))
    (unless (plist-member value :focus)
      (agent-repl-wire--fail "WatchDaemonEmacs" 'focus "required message field is absent"))
    (agent-repl-wire--encoded
     "WatchDaemonEmacs"
     (list (cons 'elispBuild (agent-repl-wire--encode-string
                              "WatchDaemonEmacs" 'elispBuild build))
           (cons 'focus (agent-repl-wire-encode-editor-focus
                         (plist-get value :focus)))))))

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

(defun agent-repl-wire-decode-workspace-open-stage (value)
  "Decode `WorkspaceOpenStage' from VALUE into the stage keyword.
THE SET ARM IS THE STAGE, and every arm is an empty message, so the
keyword is the whole fact: `:checking-worktree', `:starting-session',
`:reviving', `:clearing-closed' or `:checking-build'.  An arm this codec
does not hold is refused as an unknown field by the key check -- logged
at ERROR and signalled, never a stage guessed at or dropped -- and an
unset oneof is refused likewise."
  (let ((object (agent-repl-wire--object "WorkspaceOpenStage" value)))
    (agent-repl-wire--check-keys "WorkspaceOpenStage" object
                                 '(checkingWorktree startingSession reviving
                                   clearingClosed checkingBuild))
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
                        agent-repl-wire-decode-workspace-open-stage-checking-build)))
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
                     agent-repl-wire-decode-watch-daemon-response-faults-standing))))

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
       ending faultsStanding))
    (agent-repl-wire--decoded
     "WatchDaemonResponse"
     (agent-repl-wire-decode-watch-daemon-response-push object))))

(provide 'wire-host)

;;; wire-host.el ends here
