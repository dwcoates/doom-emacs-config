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
;; Refresh cadence matches the existing 1-Hz state poll
;; (`claude-repl--update-all-workspace-states' in status.el), which
;; calls into us on every tick.

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

(defun claude-repl--workspace-status-entry (ws)
  "Return an alist of JSON-serializable status fields for workspace WS.
Fields mirror the plist keys most useful to a peer consumer: the two
state axes, the project locations, the user-set priority, the latest
prompt summary, the cached git-clean status, and the done-acked flag.
Nil values are emitted as JSON null (via `json-null')."
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
    `(("claude_state"        . ,claude)
      ("repl_state"          . ,repl)
      ("project_dir"         . ,proj)
      ("source_ws_dir"       . ,src)
      ("priority"            . ,priority)
      ("last_prompt_summary" . ,summary)
      ("git_clean"           . ,git)
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
break the 1-Hz poll loop that drives this."
  (claude-repl--with-error-logging "write-workspace-status"
    (let* ((snapshot (claude-repl--workspace-status-snapshot))
           (json-encoding-pretty-print t)
           (json-null :null)
           (file claude-repl-workspace-status-file)
           (dir  (file-name-directory file)))
      (when (and dir (not (file-directory-p dir)))
        (make-directory dir t))
      (with-temp-file file
        (insert (json-encode snapshot))
        (insert "\n")))))

(provide 'claude-repl-workspace-status-export)
;;; workspace-status-export.el ends here
