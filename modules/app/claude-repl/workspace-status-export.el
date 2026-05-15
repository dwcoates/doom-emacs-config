;;; workspace-status-export.el --- export per-workspace status as JSON -*- lexical-binding: t; -*-

;;; Commentary:

;; Writes a JSON snapshot of every registered claude-repl workspace to a
;; shared file so other claude sessions can introspect peer workspace
;; state (the `/workspace-status' skill is the documented consumer).
;;
;; The file lives at `claude-repl-workspace-status-file' — by default
;; `~/.claude/emacs/workspace-status.json' — alongside `workspaces.el'
;; and per-project `state.el'.  Because the host's `~/.claude' is
;; bind-mounted into each sandboxed claude as `/home/claude/.claude',
;; every workspace's claude can read the same file regardless of which
;; environment wrote it.
;;
;; Refresh cadence: an outer scheduler runs every
;; `claude-repl-workspace-status-write-window-seconds' (default 60s)
;; and stages N evenly-spaced sub-timers across that window, where N is
;; the current registered-workspace count.  Each sub-timer rewrites the
;; full snapshot.  This decouples the JSON encode (the second-biggest
;; CPU/memory hotspot from profiling) from the 1-Hz state poll while
;; preserving sub-second freshness on busy multi-workspace setups.

;;; Code:

(require 'json)

(defconst claude-repl-workspace-status-file
  (expand-file-name "workspace-status.json" "~/.claude/emacs/")
  "Path where the JSON workspace status export is written.
Lives alongside `claude-repl-workspace-snapshot-file' so the same
mount semantics apply: host writes here, sandboxed claude reads the
same bytes via `/home/claude/.claude/emacs/workspace-status.json'.")

(defun claude-repl--ws-keyword-to-string (val)
  "Return a string form of VAL for JSON export, or nil when VAL is nil.
Keywords drop their leading colon (`:thinking' → \"thinking\") so the
JSON values are plain strings rather than `:thinking' literals that
a jq consumer would have to know to strip."
  (cond
   ((null val) nil)
   ((keywordp val) (substring (symbol-name val) 1))
   ((symbolp val) (symbol-name val))
   ((stringp val) val)
   (t (format "%s" val))))

(defun claude-repl--json-null-if-nil (val)
  "Return VAL, or `json-null' when VAL is nil.
Needed because `json-encode' treats a bare nil as an empty alist (and
serializes it to `{}'), which would mis-render absent optional fields
like `priority' or `last_prompt_summary'.  Substituting the
`json-null' sentinel routes those through `json-encode-keyword' so
they emit as `null'."
  (if (null val) json-null val))

(defun claude-repl--workspace-status-entry (ws)
  "Return an alist of JSON-serializable status fields for workspace WS.
Fields mirror the plist keys most useful to a peer consumer: the two
state axes, the project locations, the user-set priority, the latest
prompt summary, the cached git-clean status, and the done-acked flag.
Nil values are routed through `claude-repl--json-null-if-nil' so they
serialize as JSON `null' instead of `{}'."
  (let ((priority (claude-repl--ws-get ws :priority))
        (summary  (claude-repl--ws-get ws :last-prompt-summary))
        (proj     (claude-repl--ws-get ws :project-dir))
        (src      (claude-repl--ws-get ws :source-ws-dir))
        (claude   (claude-repl--ws-keyword-to-string
                   (claude-repl--ws-claude-state ws)))
        (repl     (claude-repl--ws-keyword-to-string
                   (claude-repl--ws-repl-state ws)))
        (git      (claude-repl--ws-keyword-to-string
                   (claude-repl--ws-get ws :git-clean)))
        (acked    (if (claude-repl--ws-get ws :done-acked) t json-false)))
    `(("claude_state"        . ,(claude-repl--json-null-if-nil claude))
      ("repl_state"          . ,(claude-repl--json-null-if-nil repl))
      ("project_dir"         . ,(claude-repl--json-null-if-nil proj))
      ("source_ws_dir"       . ,(claude-repl--json-null-if-nil src))
      ("priority"            . ,(claude-repl--json-null-if-nil priority))
      ("last_prompt_summary" . ,(claude-repl--json-null-if-nil summary))
      ("git_clean"           . ,(claude-repl--json-null-if-nil git))
      ("done_acked"          . ,acked))))

(defun claude-repl--workspace-status-snapshot ()
  "Return an alist describing every registered workspace's status.
Shape:
  ((\"updated_at\" . ISO-8601-STRING)
   (\"workspaces\" . ((WS-NAME . STATUS-ALIST) ...)))
`workspaces' is wrapped in a `:json-object' marker (via the alist
shape `json-encode' recognizes) so an empty roster serializes as
`{}' instead of `[]'."
  (let (entries)
    (maphash (lambda (ws _plist)
               (push (cons ws (claude-repl--workspace-status-entry ws))
                     entries))
             claude-repl--workspaces)
    ;; Sort by workspace name so the file is diff-stable across ticks
    ;; — easier to eyeball changes when tailing the file.
    (setq entries (sort entries (lambda (a b) (string< (car a) (car b)))))
    `(("updated_at" . ,(format-time-string "%FT%T%z"))
      ;; Empty alist must be a non-nil singleton so json-encode emits
      ;; `{}' rather than `null'.  When entries is nil we substitute
      ;; an alist with a no-op marker that json.el ignores; the
      ;; simpler fix is to emit a hash table for the workspaces map.
      ("workspaces" . ,(claude-repl--alist->json-object entries)))))

(defun claude-repl--alist->json-object (entries)
  "Convert ENTRIES (an alist) into a hash table for stable JSON object emission.
Using a hash table sidesteps `json-encode's nil → null / [] confusion
when ENTRIES is empty — an empty hash table always serializes to `{}'."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (e entries)
      (puthash (car e) (cdr e) h))
    h))

(defun claude-repl--write-workspace-status ()
  "Write the current workspace status snapshot to disk as JSON.
Writes to `claude-repl-workspace-status-file', creating the parent
directory if it does not exist.  Errors are caught and logged via
`claude-repl--with-error-logging' so a serialization failure cannot
break the 1-Hz poll loop that drives this.

`json-null' is rebound to `:null' BEFORE the snapshot is built so the
substitution inside `claude-repl--json-null-if-nil' uses the same
sentinel value that `json-encode' will later route through
`json-encode-keyword'.  Binding only after the snapshot would leave
the substitution a no-op (the default `json-null' is plain nil)."
  (claude-repl--with-error-logging "write-workspace-status"
    (let* ((json-null :null)
           (snapshot (claude-repl--workspace-status-snapshot))
           (json-encoding-pretty-print t)
           (file claude-repl-workspace-status-file)
           (dir  (file-name-directory file)))
      (when (and dir (not (file-directory-p dir)))
        (make-directory dir t))
      (with-temp-file file
        (insert (json-encode snapshot))
        (insert "\n")))))

;;;; Staggered write scheduler --------------------------------------------------

(defcustom claude-repl-workspace-status-write-window-seconds 60
  "Window (in seconds) over which to spread workspace-status JSON writes.
Every window, the scheduler schedules N sub-timers — one per registered
workspace — evenly spaced from t=0 to t=window.  Each sub-timer
rewrites the full snapshot.

Trade-off: snapshot freshness scales with workspace count.  With N
workspaces, the file is rewritten every WINDOW/N seconds.  Increase the
window to amortize cost more aggressively; decrease it for fresher
peer-visibility.  The default of 60s is the smallest value that still
reclaims the JSON-encode cost surfaced by profiling."
  :type 'integer
  :group 'claude-repl)

(defvar claude-repl--workspace-status-write-sub-timers nil
  "Active sub-timers scheduled by the workspace-status write scheduler.
Re-cancelled and re-populated each window by
`claude-repl--reschedule-workspace-status-writes'.")

;; Re-eval safety: cancel any sub-timers left over from a prior load of
;; this file.  Without this, re-evaluating workspace-status-export.el
;; (e.g. via `doom/reload') would leak the previous window's sub-timers
;; — they outlive `claude-repl--cancel-all-timers' because they are not
;; registered in `claude-repl--timers'.
(dolist (claude-repl--ws-status-write-stale-timer
         claude-repl--workspace-status-write-sub-timers)
  (when (timerp claude-repl--ws-status-write-stale-timer)
    (cancel-timer claude-repl--ws-status-write-stale-timer)))
(setq claude-repl--workspace-status-write-sub-timers nil)

(defun claude-repl--reschedule-workspace-status-writes ()
  "Cancel pending sub-timers; schedule N evenly-spaced writes over the window.
N is the current count of registered workspaces.  Each scheduled write
calls `claude-repl--write-workspace-status', which serialises the entire
workspace snapshot — the staggering amortises that cost across the
window rather than spiking at the 1-Hz state-poll cadence.

When the workspace roster is empty no writes are scheduled; the next
window will pick up any newly-registered workspaces."
  (dolist (timer claude-repl--workspace-status-write-sub-timers)
    (when (timerp timer) (cancel-timer timer)))
  (setq claude-repl--workspace-status-write-sub-timers nil)
  (let ((n (if (boundp 'claude-repl--workspaces)
               (hash-table-count claude-repl--workspaces)
             0)))
    (when (> n 0)
      (let ((interval (/ (float claude-repl-workspace-status-write-window-seconds)
                         n)))
        (dotimes (i n)
          (push (run-at-time (* i interval) nil
                             #'claude-repl--write-workspace-status)
                claude-repl--workspace-status-write-sub-timers))))))

;; Outer scheduler: fire immediately on load so a fresh status file is
;; produced without waiting a full window, then re-plan every window.
(push (run-with-timer 0 claude-repl-workspace-status-write-window-seconds
                      #'claude-repl--reschedule-workspace-status-writes)
      claude-repl--timers)

(provide 'claude-repl-workspace-status-export)
;;; workspace-status-export.el ends here
