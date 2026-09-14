;;; mutation-progress.el --- Correlate workspace-mutation progress -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A workspace mutation under option B (a create with an op_id) is ACKED at once
;; and worked in the daemon's background; its staged progress and terminal
;; outcome arrive later on the daemon-level WatchDaemon stream, keyed on the
;; op_id the command minted.  This module is the correlation seat: a command
;; mints an op_id, registers the callbacks that render its progress, and the
;; daemon-link's push dispatch hands each decoded progress event here to match
;; it back to its command.
;;
;; The stream is a BROADCAST — every WatchDaemon subscriber receives every push
;; — so a progress event for an op this Emacs never registered (another client's
;; op, or a stale event the daemon's topic replayed to a fresh subscription) is
;; expected and dropped quietly, never an error.

;;; Code:

(declare-function agent-repl--info "core")
(declare-function agent-repl--log "core")
(declare-function agent-repl--error "core")

(defconst agent-repl-mutation-progress--scope
  '(:agent-repl-central "workspace creation and daemon administration can precede workspace ownership")
  "The log scope for mutation progress: it precedes any workspace's ownership.")

(defvar agent-repl-mutation-progress--pending (make-hash-table :test 'equal)
  "Map of op id -> callback plist for an in-flight workspace mutation.
The plist keys are `:on-stage', `:on-succeeded' and `:on-failed'; each is
optional.  A terminal event (succeeded or failed) removes the entry.")

(defvar agent-repl-mutation-progress--counter 0
  "A per-session monotonic counter, the tail of every minted op id.")

(defun agent-repl-mutation-progress-new-op-id ()
  "Mint a fresh op id, unique among this Emacs's in-flight mutations.
The wall-clock prefix keeps it from colliding with an id a previous
session minted, whose stale progress the daemon's topic could replay to a
fresh subscription."
  (setq agent-repl-mutation-progress--counter
        (1+ agent-repl-mutation-progress--counter))
  (format "op-%d-%d"
          (time-convert nil 'integer)
          agent-repl-mutation-progress--counter))

(defun agent-repl-mutation-progress-register (op-id &rest callbacks)
  "Register CALLBACKS for the mutation OP-ID.
CALLBACKS is a plist of `:on-stage' (called with the stage keyword),
`:on-succeeded' (called with the succeeded value) and `:on-failed' (called
with the failure ARM and VALUE).  Each is optional."
  (puthash op-id callbacks agent-repl-mutation-progress--pending)
  (agent-repl--info agent-repl-mutation-progress--scope
                    "elisp.mutation-progress.register op-id=%s" op-id))

(defun agent-repl-mutation-progress-forget (op-id)
  "Drop any pending callbacks for OP-ID."
  (remhash op-id agent-repl-mutation-progress--pending))

(defun agent-repl-mutation-progress--dispatch-create (op-id callbacks create)
  "Dispatch one decoded WorkspaceCreateProgress CREATE for OP-ID to CALLBACKS.
CREATE is `(:arm STEP :value V)'."
  (let ((step (plist-get create :arm))
        (value (plist-get create :value)))
    (pcase step
      (:stage
       (when-let ((fn (plist-get callbacks :on-stage)))
         (funcall fn value)))
      (:succeeded
       ;; TERMINAL: the op is done, so forget it BEFORE the callback runs its
       ;; editor changes -- a callback that errors must not leave the op live.
       (agent-repl-mutation-progress-forget op-id)
       (when-let ((fn (plist-get callbacks :on-succeeded)))
         (funcall fn value)))
      (:failed
       (agent-repl-mutation-progress-forget op-id)
       (when-let ((fn (plist-get callbacks :on-failed)))
         (funcall fn (plist-get value :arm) (plist-get value :value))))
      (_
       (agent-repl--error agent-repl-mutation-progress--scope
                          "elisp.mutation-progress.unknown-step op-id=%s step=%S"
                          op-id step)))))

(defun agent-repl-mutation-progress-handle (progress)
  "Dispatch one decoded WorkspaceMutationProgress PROGRESS to its op's callbacks.
PROGRESS is `(:op-id ID :event (:arm ARM :value V))'.  An op id this Emacs
never registered is dropped quietly: the stream is a broadcast, so a push
for another client's op, or a replayed stale one, is expected."
  (let* ((op-id (plist-get progress :op-id))
         (callbacks (gethash op-id agent-repl-mutation-progress--pending)))
    (if (null callbacks)
        (agent-repl--log agent-repl-mutation-progress--scope
                         "elisp.mutation-progress.unknown-op op-id=%S" op-id)
      (let* ((event (plist-get progress :event))
             (arm (plist-get event :arm))
             (value (plist-get event :value)))
        (pcase arm
          (:create (agent-repl-mutation-progress--dispatch-create op-id callbacks value))
          (_
           (agent-repl--error agent-repl-mutation-progress--scope
                              "elisp.mutation-progress.unknown-mutation op-id=%s arm=%S"
                              op-id arm)))))))

(provide 'mutation-progress)

;;; mutation-progress.el ends here
