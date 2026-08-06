;;; merge-handlers.el --- Frontend merge REQUESTS (the daemon owns merging) -*- lexical-binding: t; -*-

;;; Commentary:

;; Workspace merging is DAEMON-OWNED end to end (design §4.6/§9.3).  The
;; daemon records each workspace's source branch, source worktree, and
;; merge target at workspace-CREATION time, runs the cherry-pick, holds
;; the merge lease over the workspace's shim while it resolves conflicts
;; through that workspace's own agent, coordinates the per-repository
;; merge queue, and publishes every transition as `WorkspaceState'
;; (`merging' / `merge_queued' / `merged' / `merge_conflict' /
;; `merge_failed').
;;
;; This file is therefore the WHOLE of Emacs's merge surface: two bare
;; requests keyed by workspace.  There is deliberately NOTHING here that
;; computes geometry, resolves a handler, or tracks merge state:
;;
;;   - No geometry.  Emacs used to compute the source branch, source dir,
;;     and target dir and ride them on `MergeWorkspaceCmd'.  Two owners of
;;     one map is how a merge landed against a target the daemon had never
;;     heard of, so the map now has exactly one owner.
;;   - No handler registry, no `.claude/emacs/workspace-merge.eld' lookup,
;;     no per-repo override alist.  Every merge is the same request.
;;   - No queue, no in-flight bookkeeping, no dispatch marker.  A merge's
;;     position, depth, landed-commit count and failure cause all arrive
;;     on `WorkspaceState.merge_status' and are RENDERED, never derived.
;;   - No pre-merge or post-merge action.  The daemon runs both ends of
;;     the pipeline itself and reports them as phases; Emacs neither
;;     defers a merge behind an action nor delivers one as a turn.
;;
;; Emacs reacts to merge state in three places: the state icons that
;; render the pushed merge states, the kill guard that refuses to tear
;; down a workspace whose merge has not reached a terminal state, and the
;; minibuffer echo below (`agent-repl--merge-echo-pushed-state'), which
;; narrates each pushed merge-phase transition — without it, a merge that
;; failed 400ms after its command was acked produced NO user-visible
;; feedback at all (observed live: three refused merges, total silence).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Transport helpers; defined in frontend-uds.el / frontend-client.el /
;; workspace-create-client.el, resolved at call time (same module).
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--ws-get "core" (ws key))
(declare-function agent-repl--uds-send-command "frontend-uds"
                  (field payload &optional workspace process &rest keys))
(declare-function agent-repl--frontend-ws-command-key "frontend-client" (ws))
(declare-function agent-repl--host-action-defer "workspace-create-client"
                  (token))
(declare-function agent-repl--host-action-settle "workspace-create-client"
                  (token ok error-text))
(declare-function agent-repl--ws-put "core" (ws key value))
(declare-function agent-repl--ws-open-p "workspace" (ws))
(declare-function agent-repl--close-workspace "worktree" (ws &optional preserve-entry))

(defun agent-repl--merge-command-payload (ws &optional resume)
  "Return the `mergeWorkspace' payload for WS.
The command is a BARE merge request keyed by workspace: the daemon
resolves the geometry it recorded when it created WS.  RESUME non-nil
sets `conflictResolvedContinue', the resolve-and-continue handoff.

SIGNALS `user-error' when WS is not a non-empty string — an unkeyed
merge request is a request the daemon cannot attribute, and sending one
would file merge state under an empty workspace (No-Silent-Fallbacks)."
  (unless (and (stringp ws) (not (string-empty-p ws)))
    (agent-repl--log nil
                     "merge-command-payload: refusing merge request with invalid workspace=%S"
                     ws)
    (user-error "Cannot merge: no workspace name given"))
  (if resume
      (list :conflictResolvedContinue t :workspaceName ws)
    (list :workspaceName ws)))

(defun agent-repl--merge-dispatch-over-uds (ws)
  "Send the daemon a bare `mergeWorkspace' request for WS.
Returns the request-id.  A rejected `CommandAck' surfaces loudly through
`frontend-uds' and settles this dispatch's host action as failed; an ack
settles it as succeeded, so the skill's outcome is the MERGE's own
outcome rather than the fact that a frame was written.

The envelope is routed by WS's workspace command key, exactly like every
other command; the DISPLAY name rides `:workspaceName'."
  (let ((req nil))
    (agent-repl--uds-send-command
     "mergeWorkspace"
     (agent-repl--merge-command-payload ws)
     (agent-repl--frontend-ws-command-key ws) nil
     ;; THE HOST ACTION'S OUTCOME IS THIS COMMAND'S ACK, not this dispatch.
     ;; Returning here without deferring is what let the daemon record a merge
     ;; as `ok=true' 52ms before its own rejection arrived, so the failure that
     ;; killed every merge left no trace on the workspace at all.  The deferral
     ;; is declared from `:on-registered', ahead of the write, and
     ;; `agent-repl--host-action-defer' REGISTERS it there and then, so the
     ;; settle below can never run against a deferral nobody has recorded.
     :on-registered (lambda (id)
                      (setq req id)
                      (agent-repl--host-action-defer id))
     :on-failure
     (lambda (err)
       (agent-repl--host-action-settle req nil err)
       (agent-repl--log ws
                        "merge-dispatch-over-uds: ws=%s request-id=%s command REJECTED err=%s"
                        ws req err)
       (message "agent-repl: merge of %s refused: %s" ws err))
     :on-success
     (lambda ()
       (agent-repl--host-action-settle req t nil)
       (agent-repl--log ws
                        "merge-dispatch-over-uds: ws=%s request-id=%s command ACKED"
                        ws req)))
    (agent-repl--log ws
                     "merge-dispatch-over-uds: ws=%s command-issued request-id=%s"
                     ws req)
    (message "agent-repl: merge of %s requested — the daemon reports each phase here" ws)
    req))

(defun agent-repl--merge-resume-over-uds (ws)
  "Send the daemon a `mergeWorkspace' resume for WS (resolve-and-continue).
The design §9.3 handoff, kept as a human escape hatch for the case where
a person resolved the in-tree conflict themselves rather than leaving it
to the merge lease: the daemon runs `git add -u' + `cherry-pick
--continue' against the geometry it already holds.  Returns the
request-id."
  (let ((req (agent-repl--uds-send-command
              "mergeWorkspace"
              (agent-repl--merge-command-payload ws 'resume)
              (agent-repl--frontend-ws-command-key ws))))
    (agent-repl--log ws
                     "merge-resume-over-uds: ws=%s command-issued request-id=%s tracking-registered"
                     ws req)
    req))

(defconst agent-repl--merge-echo-states
  (delete-dups
   (mapcar #'cdr
           (cl-remove-if-not
            (lambda (entry) (string-prefix-p "RENDER_STATE_MERG" (car entry)))
            agent-repl--frontend-render-state-map)))
  "The pushed render keywords that narrate a merge in the minibuffer.
DERIVED from the merge arm of `agent-repl--frontend-render-state-map'
\(frontend-state.el) rather than restated, so a merge state added to the
daemon's vocabulary narrates itself the moment the map learns it.  The
restated version silently stopped narrating whichever state was added
last, which is the failure mode of every list that claims in a comment to
equal another list.  Load order is the frontend-state module first
\(config.el), which is also how `agent-repl--frontend-render-state-map' is
already resolvable here.")

(defconst agent-repl--merge-phase-words
  '((:enqueued       . "merge queued")
    (:before-action  . "running its pre-merge action")
    (:cherry-picking . "cherry-picking")
    (:testing        . "testing its merge")
    (:conflict       . "merge conflicted")
    (:after-action   . "running its post-merge action")
    (:merged         . "merged")
    (:failed         . "merge failed"))
  "The narrated word for each `MergeStatus' phase keyword.
Finer than the render-state vocabulary on purpose: the daemon reports
`:before-action', `:cherry-picking', `:testing' and `:after-action' all
under one pushed `:merging' state, and \"merging\" for four different
things is exactly the feedback that makes a user ask what it is doing.")

(defconst agent-repl--merge-echo-detail-max 60
  "Longest commit subject or action prompt the narration quotes.
The echo area is one line, and a long subject pushed the counts that
precede it off the end of it.")

(defun agent-repl--merge-echo-clip (text)
  "Return TEXT clipped to `agent-repl--merge-echo-detail-max', or nil.
Answers nil for a nil or blank TEXT so callers can drop the clause
entirely rather than narrate an empty one."
  (when (and (stringp text) (not (string-empty-p text)))
    (if (<= (length text) agent-repl--merge-echo-detail-max)
        text
      (concat (substring text 0 agent-repl--merge-echo-detail-max) "…"))))

(defun agent-repl--merge-failed-record (json)
  "Return the reported clause carrying JSON, the failed arm's own record.

JSON is `MergeStatusFailed.failed_json' — the WHOLE failed arm as the
daemon serialized it through proto3's JSON mapping.  It rides the merge
error as a field of its own so a reader gets the counts, the failing sha
and anything the schema learns later, none of which the prose clauses
beside it quote.

DELIBERATELY UNCLIPPED, unlike every other clause: the clip exists to
keep a long commit subject from pushing the counts off the echo line,
and a record truncated at 60 characters is not a record — it is a
fragment of JSON that cannot be read back.  A merge failure is the one
merge status a person has to diagnose rather than watch, and this is the
line they copy out of `*Messages*' to do it.

Answers nil for a nil or blank JSON so the clause is dropped rather than
narrated empty, exactly as `agent-repl--merge-echo-clip' does."
  (when (and (stringp json) (not (string-empty-p json)))
    (format "record %s" json)))

(defun agent-repl--merge-echo-facts (phase status)
  "Return the narration clauses for PHASE of decoded merge STATUS.
A list of strings, already clipped, with the absent ones dropped.  Each
phase names only what it actually knows: a queue position while enqueued,
the landed/total counts and the commit on the table while picking or
testing, the conflicted subject on a conflict, and on a failure the cause
plus the failed arm's whole JSON record (`agent-repl--merge-failed-record')."
  (let ((total (plist-get status :commits-total))
        (landed (plist-get status :commits-landed)))
    (delq nil
          (pcase phase
            (:enqueued
             (list (when-let ((position (plist-get status :position))
                              (depth (plist-get status :depth)))
                     (format "position %s of %s" position depth))))
            ((or :before-action :after-action)
             (list (agent-repl--merge-echo-clip (plist-get status :prompt))))
            ((or :cherry-picking :testing)
             (list (when total (format "%s/%s commits" (or landed 0) total))
                   (agent-repl--merge-echo-clip
                    (plist-get status :current-subject))))
            (:conflict
             (list (agent-repl--merge-echo-clip
                    (plist-get status :conflicted-subject))
                   (when total (format "%s/%s commits" (or landed 0) total))))
            (:merged
             (list (when total (format "%s commits" total))
                   (when-let ((err (plist-get status :after-action-error)))
                     (and (not (string-empty-p err))
                          (format "post-merge action failed: %s" err)))))
            (:failed
             (list (agent-repl--merge-echo-clip (plist-get status :cause))
                   (agent-repl--merge-echo-clip
                    (plist-get status :failing-subject))
                   (agent-repl--merge-failed-record
                    (plist-get status :failed-json))))))))

(defun agent-repl--merge-echo-pushed-state (ws new previous)
  "Echo WS's merge progress NEW in the minibuffer, one line per transition.
Subscriber for `agent-repl-ws-state-transition-functions'
\(frontend-state.el).  Non-merge states are ignored.

THREE things count as a transition worth narrating, and nothing else:

  - the pushed render state changed (the pre-MergeStatus contract);
  - the `MergeStatus' PHASE changed, which the render state cannot
    always report — the daemon runs the pre-merge action, the picks, the
    tests and the post-merge action all under one pushed `:merging';
  - `commits_landed' changed, the per-pick tick.  Gated on the count
    itself CHANGING rather than on a status arriving, because the daemon
    also pushes within-phase revisions that land no commit, and echoing
    those would narrate the same line repeatedly.

What was last narrated is recorded on `:merge-echo-last', so a re-push
that moved none of the three is silent.

Without a `MergeStatus' (a daemon predating it, or a merge state pushed
with no run to describe) the narration is the render-state word plus, for
`:merge-failed', the pushed `:cause-kind' minus its `merge_transition:'
routing prefix — \"merge failed\" with no reason is feedback that only
creates a second question."
  (when (memq new agent-repl--merge-echo-states)
    (let* ((status (agent-repl--ws-get ws :pushed-merge-status))
           (phase (plist-get status :phase))
           (landed (plist-get status :commits-landed))
           (last (agent-repl--ws-get ws :merge-echo-last))
           (state-edge (not (eq new previous)))
           (phase-edge (and phase (not (eq phase (plist-get last :phase)))))
           (tick (and landed
                      (not (equal landed (plist-get last :commits-landed))))))
      (when (or state-edge phase-edge tick)
        (let* ((meta (agent-repl--ws-get ws :pushed-render-state-meta))
               (cause (plist-get meta :cause-kind))
               (word (or (alist-get phase agent-repl--merge-phase-words)
                         (string-replace "-" " " (substring (symbol-name new) 1))))
               (facts (if status
                          (agent-repl--merge-echo-facts phase status)
                        (and (eq new :merge-failed)
                             (stringp cause)
                             (list (string-remove-prefix "merge_transition:"
                                                         cause))))))
          (agent-repl--ws-put ws :merge-echo-last
                              (list :phase phase :commits-landed landed))
          (agent-repl--log ws
                           "merge-echo-pushed-state: ws=%s %s -> %s phase=%s landed=%s run-id=%s edges=(state=%S phase=%S tick=%S) cause=%S facts=%S"
                           ws previous new phase landed
                           (plist-get status :run-id)
                           state-edge phase-edge tick cause facts)
          (message "agent-repl: %s %s%s" ws word
                   (if facts (format " — %s" (string-join facts ", ")) "")))))))

;; Registered here (merge-handlers.el is the merge surface) though the hook
;; variable is defined in frontend-state.el: `add-hook' auto-vivifies the
;; unbound variable, and frontend-state.el's `defvar ... nil' does not reset
;; an already-bound one, so this subscriber survives either load order.
(add-hook 'agent-repl-ws-state-transition-functions
          #'agent-repl--merge-echo-pushed-state)

(defun agent-repl--merge-kill-on-merged (ws new _previous)
  "Kill WS's editor workspace when its merge lands.
Subscriber for `agent-repl-ws-state-transition-functions'.  A merged
workspace is concluded — the work is on the target branch and nothing
the user can do to the tab is useful — so the tab must DIE, not merely
hide.  The old filter-only approach (`agent-repl--filter-merged-names')
kept the persp alive forever, and any transient non-merged push (a
daemon bounce re-resolving states) un-hid every merged workspace at
once, marching the whole Recently Merged rail back into the tab-bar.

Keyed on the pushed state alone, NOT on the transition edge: a
`:merged' re-push against a still-open tab (the bounce case above, or a
tab opened while this subscriber was not yet loaded) must converge to
closed, so a same-state re-push kills too.  Once the tab is gone
`agent-repl--ws-open-p' is nil and re-pushes are no-ops.

The close PRESERVES the `agent-repl--workspaces' entry (data-only, the
same shape `agent-repl--register-merged-workspace' produces), so the
merged fact keeps rendering, `finish' can still reap the worktree, and
a debugging recovery via the workspace `open' verb stays possible.
`:merge-completed' is stamped first so renderers observing the close
classify the entry as merged rather than dead."
  (when (and (eq new :merged)
             (agent-repl--ws-open-p ws))
    (agent-repl--log ws
                     "merge-kill-on-merged: merge landed for open workspace ws=%s — killing its tab (entry preserved)"
                     ws)
    (agent-repl--ws-put ws :merge-completed t)
    (agent-repl--close-workspace ws 'preserve-entry)))

(add-hook 'agent-repl-ws-state-transition-functions
          #'agent-repl--merge-kill-on-merged)

(provide 'merge-handlers)

;;; merge-handlers.el ends here
