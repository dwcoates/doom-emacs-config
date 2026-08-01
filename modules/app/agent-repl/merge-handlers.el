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
;;     position and depth arrive on `WorkspaceState'
;;     (`merge_queue_position' / `merge_queue_depth') and are RENDERED,
;;     never derived.
;;
;; Emacs reacts to merge state in exactly two places, both outside this
;; file: the state icons that render the pushed merge states, and the
;; kill guard that refuses to tear down a workspace whose merge has not
;; reached a terminal state.

;;; Code:

(require 'cl-lib)

;; Transport helpers; defined in frontend-uds.el / frontend-client.el /
;; workspace-create-client.el, resolved at call time (same module).
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--uds-send-command "frontend-uds"
                  (field payload &optional workspace process))
(declare-function agent-repl--frontend-ws-command-key "frontend-client" (ws))
(declare-function agent-repl--uds-track-command "frontend-uds"
                  (request-id field workspace
                              &optional on-failure on-success on-challenge))
(declare-function agent-repl--host-action-defer "workspace-create-client"
                  (token))
(declare-function agent-repl--host-action-settle "workspace-create-client"
                  (token ok error-text))

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
  (let ((req (agent-repl--uds-send-command
              "mergeWorkspace"
              (agent-repl--merge-command-payload ws)
              (agent-repl--frontend-ws-command-key ws))))
    (agent-repl--log ws
                     "merge-dispatch-over-uds: ws=%s command-issued request-id=%s"
                     ws req)
    ;; THE HOST ACTION'S OUTCOME IS THIS COMMAND'S ACK, not this dispatch.
    ;; Returning here without deferring is what let the daemon record a merge
    ;; as `ok=true' 52ms before its own rejection arrived, so the failure that
    ;; killed every merge left no trace on the workspace at all.
    (agent-repl--host-action-defer req)
    (agent-repl--uds-track-command
     req "mergeWorkspace" ws
     (lambda (err)
       (agent-repl--host-action-settle req nil err)
       (agent-repl--log ws
                        "merge-dispatch-over-uds: ws=%s request-id=%s command REJECTED err=%s"
                        ws req err))
     (lambda ()
       (agent-repl--host-action-settle req t nil)
       (agent-repl--log ws
                        "merge-dispatch-over-uds: ws=%s request-id=%s command ACKED"
                        ws req)))
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
    (agent-repl--uds-track-command req "mergeWorkspace" ws)
    (agent-repl--log ws
                     "merge-resume-over-uds: ws=%s command-issued request-id=%s tracking-registered"
                     ws req)
    req))

(provide 'merge-handlers)

;;; merge-handlers.el ends here
