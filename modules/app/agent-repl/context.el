;;; context.el --- Context-window utilization in vterm mode-line -*- lexical-binding: t; -*-

;;; Commentary:

;; Render the context-window utilization of a workspace's Claude session
;; in the vterm mode-line, alongside the model, prompt-summary, and
;; aiTitle segments.
;;
;; Source of truth: the workspace's own session jsonl under
;; `~/.claude/projects/<encoded-cwd>/<session-id>.jsonl' — the same
;; per-workspace file `ai-title.el' and `model.el' read.  Every
;; main-chain `type:assistant' line carries a `message.usage' object
;; whose input-side counters describe the tokens that were in context
;; when that turn was served:
;;   input_tokens + cache_creation_input_tokens + cache_read_input_tokens
;; We surface the most recent such total as a raw token count, rendered
;; with comma thousands separators (e.g. `tokens 53,179').  We do not
;; divide by a context-window size: the effective window depends on the
;; model and whether a long-context (1M) mode is active, and that figure
;; is not recorded anywhere in the session data, so any percentage would
;; rest on a brittle assumption.  `output_tokens' is deliberately
;; excluded: it is the freshly-generated response, not part of the
;; context that turn consumed.
;;
;; Mode-line layout (left-to-right): parent label → model → tokens.
;;
;; Reading the jsonl on every mode-line redraw would re-stat and
;; re-scan a multi-MB file; we cache (mtime, path, used) on the
;; workspace plist and only re-scan the last
;; `agent-repl-context-scan-bytes' of the file when mtime changes.
;;
;; Path resolution and mtime keying are deliberately reused from
;; `ai-title.el' (`agent-repl--ai-title-jsonl-path' /
;; `agent-repl--ai-title-mtime') since all three segments resolve the
;; exact same per-workspace jsonl; this file is loaded after
;; `ai-title.el' so those helpers are available.

;;; Code:

(require 'json)
(require 'cl-lib)

;;;; Defcustoms

(defcustom agent-repl-context-enabled t
  "Non-nil to render context-window utilization in the vterm mode-line."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-context-scan-bytes 32768
  "Number of bytes to read from the end of the session jsonl when scanning
for the most recent assistant usage.  Each entry's usage object is short,
so the tail of the file is enough; reading the whole file on every
mode-line refresh would be wasteful for large transcripts."
  :type 'integer
  :group 'agent-repl)

;;;; Usage extraction

(defun agent-repl--context-usage-total (usage)
  "Return the input-side token total from a parsed USAGE alist, or nil.
Sums `input_tokens', `cache_creation_input_tokens', and
`cache_read_input_tokens', treating any missing counter as 0.  Returns
nil when USAGE is not an alist or carries none of the three counters, so
callers can skip lines that lack usable usage."
  (when (listp usage)
    (let ((keys '("input_tokens"
                  "cache_creation_input_tokens"
                  "cache_read_input_tokens"))
          (total 0)
          (any nil))
      (dolist (k keys)
        (let ((v (cdr (assoc k usage))))
          (when (numberp v)
            (setq any t)
            (setq total (+ total v)))))
      (when any total))))

(defun agent-repl--context-extract-from-tail (tail)
  "Return the most recent assistant context-token total found in TAIL, or nil.
TAIL is the trailing chunk of a session jsonl (one JSON object per
line).  Walks lines bottom-up so we hit the most recent assistant entry
first.  Skips lines that are not main-chain `type:assistant' entries
\(sidechain lines are ignored so the figure reflects the primary
conversation), don't parse, or carry no usable `message.usage'."
  (when (and (stringp tail) (not (string-empty-p tail)))
    (let ((lines (split-string tail "\n" t))
          (found nil))
      (cl-loop for line in (nreverse lines)
               while (not found)
               when (and (string-match-p "\"type\":\"assistant\"" line)
                         (not (string-match-p "\"isSidechain\":true" line)))
               do (let ((parsed (ignore-errors
                                  (let ((json-object-type 'alist)
                                        (json-array-type 'list)
                                        (json-key-type 'string)
                                        (json-false nil)
                                        (json-null nil))
                                    (json-read-from-string line)))))
                    (let* ((message (and (listp parsed)
                                         (cdr (assoc "message" parsed))))
                           (usage (and (listp message)
                                       (cdr (assoc "usage" message))))
                           (total (agent-repl--context-usage-total usage)))
                      (when total
                        (setq found total)))))
      found)))

(defun agent-repl--context-read-from-jsonl (path)
  "Return the most recent assistant context-token total from PATH, or nil.
Reads only the trailing `agent-repl-context-scan-bytes' so this stays
cheap on large transcripts.  Returns nil on missing/unreadable file or
when no assistant usage is present in the scanned tail."
  (when (and path (file-readable-p path))
    (let* ((size (or (file-attribute-size (file-attributes path)) 0))
           (cap agent-repl-context-scan-bytes)
           (start (max 0 (- size cap))))
      (when (> size 0)
        (with-temp-buffer
          (insert-file-contents path nil start size)
          (agent-repl--context-extract-from-tail (buffer-string)))))))

;;;; Cached lookup

(defun agent-repl--context-for-ws (ws)
  "Return the context-token total for WS, using a per-workspace mtime cache.
Cache shape (stored in WS's plist under `:context-cache'):
  (PATH MTIME USED)
If the current resolved (path, mtime) matches the cache, returns the
cached USED without re-reading.  Otherwise re-scans and refreshes the
cache.  Reuses `ai-title.el's path/mtime resolvers since all mode-line
session segments read the identical per-workspace jsonl.  Returns nil
when no usage is available."
  (let* ((path (agent-repl--ai-title-jsonl-path ws))
         (mtime (agent-repl--ai-title-mtime path))
         (cache (agent-repl--ws-get ws :context-cache)))
    (cond
     ((null path) nil)
     ((null mtime) nil)
     ((and (consp cache)
           (equal (nth 0 cache) path)
           (equal (nth 1 cache) mtime))
      (nth 2 cache))
     (t
      (let ((used (agent-repl--context-read-from-jsonl path)))
        (agent-repl--ws-put ws :context-cache (list path mtime used))
        used)))))

;;;; Formatting

(defun agent-repl--context-commafy (n)
  "Return non-negative integer N as a string with comma thousands separators.
E.g. 53179 → \"53,179\" and 1000000 → \"1,000,000\"."
  (let ((s (number-to-string n)))
    (while (string-match "\\([0-9]+\\)\\([0-9]\\{3\\}\\)" s)
      (setq s (replace-match "\\1,\\2" nil nil s)))
    s))

(defun agent-repl--context-format (used)
  "Return the display string for USED context tokens, e.g. `tokens 53,179'."
  (format "tokens %s" (agent-repl--context-commafy used)))

;;;; Mode-line segment

(defun agent-repl--context-segment ()
  "Return a propertized string for the mode-line's context-tokens segment.
Reads `agent-repl--owning-workspace' from the current buffer (set on
every claude-owned vterm buffer) and pulls the workspace's used context
token count.  Returns the empty string when disabled, the workspace is
unknown, or no usage is yet available."
  (if (not agent-repl-context-enabled)
      ""
    (let ((ws (agent-repl--buffer-owner (current-buffer))))
      (if (not ws)
          ""
        (let ((used (agent-repl--context-for-ws ws)))
          (if (not (numberp used))
              ""
            (concat "  "
                    (propertize (agent-repl--context-format used)
                                'face '(:foreground "medium sea green"
                                        :weight normal)))))))))

;;;; Mode-line attachment

(defconst agent-repl--context-mode-line-spec
  '(:eval (agent-repl--context-segment))
  "Trailing `:eval' mode-line segment that paints context utilization.
Captured as a constant so the attach helper can detect (via `equal')
whether a buffer's mode-line already contains it.")

(defun agent-repl--context-attach-to-mode-line (buf)
  "Append the context segment to BUF's `mode-line-format' if missing.
Idempotent — does nothing when the segment is already present, the
buffer is dead, or the buffer's mode-line is not a list."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (listp mode-line-format)
                 (not (member agent-repl--context-mode-line-spec
                              mode-line-format)))
        (setq-local mode-line-format
                    (append mode-line-format
                            (list agent-repl--context-mode-line-spec)))
        (force-mode-line-update t)))))

(defun agent-repl-context-attach-all ()
  "Attach the context segment to every live workspace vterm buffer.
Run automatically when this file loads so reloading agent-repl upgrades
pre-existing vterm buffers.  Also exposed interactively for manual
recovery."
  (interactive)
  (when (and (boundp 'agent-repl--workspaces)
             (hash-table-p agent-repl--workspaces))
    (maphash
     (lambda (_ws plist)
       (let ((buf (plist-get plist :vterm-buffer)))
         (when (and buf (buffer-live-p buf))
           (agent-repl--context-attach-to-mode-line buf))))
     agent-repl--workspaces)))

(agent-repl-context-attach-all)

(provide 'agent-repl-context)
;;; context.el ends here
