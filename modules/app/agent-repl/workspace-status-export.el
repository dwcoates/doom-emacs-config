;;; workspace-status-export.el --- export per-workspace status as JSON -*- lexical-binding: t; -*-

;;; Commentary:

;; Writes a JSON snapshot of every registered agent-repl workspace to a
;; shared file so other claude sessions can introspect peer workspace
;; state (the `/workspace-status' skill is the documented consumer).
;;
;; The file lives at `agent-repl-workspace-status-file' — by default
;; `~/.claude-emacs/workspace-status.json' — alongside `workspaces.el'.
;; agent-repl launches plain `claude' on the host, so the publishing
;; editor and every consuming claude session share the same host
;; filesystem and read/write the identical path.
;;
;; Refresh cadence: an outer scheduler runs every
;; `agent-repl-workspace-status-write-window-seconds' (default 60s)
;; and stages N evenly-spaced sub-timers across that window, where N is
;; the current registered-workspace count.  Each sub-timer rewrites the
;; full snapshot.  This decouples the JSON encode (the second-biggest
;; CPU/memory hotspot from profiling) from the 1-Hz state poll while
;; preserving sub-second freshness on busy multi-workspace setups.

;;; Code:

(require 'json)

(defconst agent-repl-workspace-status-file
  (agent-repl--global-state-file "workspace-status.json")
  "Path where the JSON workspace status export is written.
Lives at `~/.claude-emacs/workspace-status.json' (under
`agent-repl--global-state-dir'), alongside
`agent-repl-workspace-snapshot-file'.  The peer-introspection consumer
\(the `create-or-update-workspace' status skill) reads the identical
path.")

(defun agent-repl--ws-keyword-to-string (val)
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

(defun agent-repl--json-null-if-nil (val)
  "Return VAL, or `json-null' when VAL is nil.
Needed because `json-encode' treats a bare nil as an empty alist (and
serializes it to `{}'), which would mis-render absent optional fields
like `priority' or `last_prompt_summary'.  Substituting the
`json-null' sentinel routes those through `json-encode-keyword' so
they emit as `null'."
  (if (null val) json-null val))

(defun agent-repl--workspace-status-entry (ws)
  "Return an alist of JSON-serializable status fields for workspace WS.
Fields mirror the plist keys most useful to a peer consumer: the two
state axes, the project locations, the user-set priority, the latest
prompt summary, the cached git-clean status, and the done-acked flag.
Nil values are routed through `agent-repl--json-null-if-nil' so they
serialize as JSON `null' instead of `{}'."
  (let ((priority (agent-repl--ws-get ws :priority))
        (summary  (agent-repl--ws-get ws :last-prompt-summary))
        (proj     (agent-repl--ws-get ws :project-dir))
        (src      (agent-repl--ws-get ws :source-ws-dir))
        (claude   (agent-repl--ws-keyword-to-string
                   (agent-repl--ws-agent-state ws)))
        (repl     (agent-repl--ws-keyword-to-string
                   (agent-repl--ws-repl-state ws)))
        (git      (agent-repl--ws-keyword-to-string
                   (agent-repl--ws-get ws :git-clean)))
        (acked    (if (agent-repl--ws-get ws :done-acked) t json-false)))
    `(("agent_state"        . ,(agent-repl--json-null-if-nil claude))
      ("repl_state"          . ,(agent-repl--json-null-if-nil repl))
      ("project_dir"         . ,(agent-repl--json-null-if-nil proj))
      ("source_ws_dir"       . ,(agent-repl--json-null-if-nil src))
      ("priority"            . ,(agent-repl--json-null-if-nil priority))
      ("last_prompt_summary" . ,(agent-repl--json-null-if-nil summary))
      ("git_clean"           . ,(agent-repl--json-null-if-nil git))
      ("done_acked"          . ,acked))))

(defun agent-repl--workspace-status-merged-p (ws)
  "Return non-nil when WS is a merged-and-dead workspace.
Merged workspaces have no live claude session and all interesting
fields stay `null' for the remainder of the registry's lifetime, so
they are filtered out of the JSON snapshot to (a) keep the on-disk
file focused on live workspaces and (b) scale json-encode cost with
the live roster rather than the total roster.  Profiling on a
~111-workspace registry showed ~95% of entries were merged."
  (eq (agent-repl--ws-repl-state ws) :merged))

(defun agent-repl--workspace-status-snapshot ()
  "Return an alist describing every non-merged registered workspace's status.
Shape:
  ((\"updated_at\" . ISO-8601-STRING)
   (\"workspaces\" . ((WS-NAME . STATUS-ALIST) ...)))
`workspaces' is wrapped in a `:json-object' marker (via the alist
shape `json-encode' recognizes) so an empty roster serializes as
`{}' instead of `[]'.

`:merged' workspaces are skipped — see
`agent-repl--workspace-status-merged-p'."
  (let (entries)
    (maphash (lambda (ws _plist)
               (unless (agent-repl--workspace-status-merged-p ws)
                 (push (cons ws (agent-repl--workspace-status-entry ws))
                       entries)))
             agent-repl--workspaces)
    ;; Sort by workspace name so the file is diff-stable across ticks
    ;; — easier to eyeball changes when tailing the file.
    (setq entries (sort entries (lambda (a b) (string< (car a) (car b)))))
    `(("updated_at" . ,(format-time-string "%FT%T%z"))
      ;; Empty alist must be a non-nil singleton so json-encode emits
      ;; `{}' rather than `null'.  When entries is nil we substitute
      ;; an alist with a no-op marker that json.el ignores; the
      ;; simpler fix is to emit a hash table for the workspaces map.
      ("workspaces" . ,(agent-repl--alist->json-object entries)))))

(defun agent-repl--alist->json-object (entries)
  "Convert ENTRIES (an alist) into a hash table for stable JSON object emission.
Using a hash table sidesteps `json-encode's nil → null / [] confusion
when ENTRIES is empty — an empty hash table always serializes to `{}'."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (e entries)
      (puthash (car e) (cdr e) h))
    h))

(defun agent-repl--snapshot->json-serializable (snapshot)
  "Project SNAPSHOT (a string-keyed alist) onto a hash table tree.
`json-serialize' requires alist KEYS to be symbols; our snapshot uses
human-readable string keys throughout.  Re-keying the alists to
symbols would either churn intern-pool entries or force tests to
rewrite their `(assoc \"foo\" ...)' lookups.  Hash tables sidestep
both: they accept string keys natively, and `json-serialize'
encodes them as JSON objects.

The projection is shallow-recursive: the top-level alist becomes a
hash table; the `workspaces' value is already a hash table whose
values are per-workspace string-keyed alists, so each of those is
projected into its own hash table too."
  (let ((top (make-hash-table :test 'equal)))
    (dolist (pair snapshot)
      (let ((k (car pair)) (v (cdr pair)))
        (puthash k
                 (cond
                  ;; Per-workspace map: rewrap each entry-alist as a
                  ;; hash table so json-serialize accepts it.
                  ((and (equal k "workspaces") (hash-table-p v))
                   (let ((out (make-hash-table :test 'equal)))
                     (maphash (lambda (ws entry-alist)
                                (let ((entry-hash (make-hash-table :test 'equal)))
                                  (dolist (p entry-alist)
                                    (puthash (car p) (cdr p) entry-hash))
                                  (puthash ws entry-hash out)))
                              v)
                     out))
                  (t v))
                 top)))
    top))

(defun agent-repl--write-workspace-status ()
  "Write the current workspace status snapshot to disk as JSON.
Writes to `agent-repl-workspace-status-file', creating the parent
directory if it does not exist.  Errors are caught and logged via
`agent-repl--with-error-logging' so a serialization failure cannot
break the 1-Hz poll loop that drives this.

`json-null' is rebound to `:null' BEFORE the snapshot is built so the
substitution inside `agent-repl--json-null-if-nil' uses the same
sentinel value that the encoder will later route through to JSON
`null'.  Binding only after the snapshot would leave the substitution
a no-op (the default `json-null' is plain nil, and bare nil collides
with json-serialize's empty-object encoding).

Encoder: native C `json-serialize' rather than elisp `json-encode'.
Profiling on a ~111-workspace registry showed `json-encode' (with
`json-encoding-pretty-print') allocated ~937 MB of transient garbage
per profile window to produce a ~43 KB file; that GC was the proximate
cause of the visible periodic hang.  `json-serialize' is a C builtin
that emits compact JSON with dramatically lower allocation."
  (agent-repl--with-error-logging "write-workspace-status"
    (let* ((json-null :null)
           (snapshot (agent-repl--workspace-status-snapshot))
           (file agent-repl-workspace-status-file)
           (dir  (file-name-directory file)))
      (when (and dir (not (file-directory-p dir)))
        (make-directory dir t))
      ;; Force utf-8-unix on the write.  On Emacs 30, `with-temp-file' without an
      ;; explicit coding system can land in `select-safe-coding-system' when the
      ;; serialized JSON contains characters whose default encoding is ambiguous
      ;; (e.g. U+FFFD from upstream-corrupted byte sequences in last-prompt
      ;; summaries).  Hitting the interactive "Select coding system:" prompt is
      ;; especially bad here because this writer fires from a 1-Hz timer, so the
      ;; prompt instantly re-pops the moment focus shifts away.  utf-8-unix is
      ;; the only encoding that round-trips json-serialize's output, so pin it.
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file file
          (insert (json-serialize
                   (agent-repl--snapshot->json-serializable snapshot)
                   :null-object :null
                   :false-object :json-false))
          (insert "\n"))))))

;;;; Staggered write scheduler --------------------------------------------------

(defcustom agent-repl-workspace-status-write-window-seconds 60
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
  :group 'agent-repl)

(defvar agent-repl--workspace-status-write-sub-timers nil
  "Active sub-timers scheduled by the workspace-status write scheduler.
Re-cancelled and re-populated each window by
`agent-repl--reschedule-workspace-status-writes'.")

;; Re-eval safety: cancel any sub-timers left over from a prior load of
;; this file.  Without this, re-evaluating workspace-status-export.el
;; (e.g. via `doom/reload') would leak the previous window's sub-timers
;; — they outlive `agent-repl--cancel-all-timers' because they are not
;; registered in `agent-repl--timers'.
(dolist (agent-repl--ws-status-write-stale-timer
         agent-repl--workspace-status-write-sub-timers)
  (when (timerp agent-repl--ws-status-write-stale-timer)
    (cancel-timer agent-repl--ws-status-write-stale-timer)))
(setq agent-repl--workspace-status-write-sub-timers nil)

(defun agent-repl--workspace-status-live-count ()
  "Return the number of registered workspaces that are NOT `:merged'.
Mirrors the filter in `agent-repl--workspace-status-snapshot' so the
staggered scheduler's N tracks the live roster rather than the total
roster.  Without this, a long-running session that accumulates 100+
merged workspaces would still schedule 100+ writes per window even
though each write only encodes the ~5 live entries."
  (if (boundp 'agent-repl--workspaces)
      (let ((n 0))
        (maphash (lambda (ws _plist)
                   (unless (agent-repl--workspace-status-merged-p ws)
                     (cl-incf n)))
                 agent-repl--workspaces)
        n)
    0))

(defun agent-repl--reschedule-workspace-status-writes ()
  "Cancel pending sub-timers; schedule N evenly-spaced writes over the window.
N is the count of NON-merged registered workspaces (see
`agent-repl--workspace-status-live-count').  Each scheduled write
calls `agent-repl--write-workspace-status', which serialises the
entire workspace snapshot — the staggering amortises that cost across
the window rather than spiking at the 1-Hz state-poll cadence.

When no live workspaces are registered, no writes are scheduled; the
next window will pick up any newly-registered workspaces."
  (dolist (timer agent-repl--workspace-status-write-sub-timers)
    (when (timerp timer) (cancel-timer timer)))
  (setq agent-repl--workspace-status-write-sub-timers nil)
  (let ((n (agent-repl--workspace-status-live-count)))
    (when (> n 0)
      (let ((interval (/ (float agent-repl-workspace-status-write-window-seconds)
                         n)))
        (dotimes (i n)
          (push (run-at-time (* i interval) nil
                             #'agent-repl--write-workspace-status)
                agent-repl--workspace-status-write-sub-timers))))))

;; Outer scheduler: fire immediately on load so a fresh status file is
;; produced without waiting a full window, then re-plan every window.
(push (run-with-timer 0 agent-repl-workspace-status-write-window-seconds
                      #'agent-repl--reschedule-workspace-status-writes)
      agent-repl--timers)

(provide 'agent-repl-workspace-status-export)
;;; workspace-status-export.el ends here
