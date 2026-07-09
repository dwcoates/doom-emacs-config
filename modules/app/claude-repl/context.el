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
;; We surface the most recent such total as a percentage of
;; `claude-repl-context-window-tokens' (200k by default), matching the
;; "context used" figure Claude Code itself displays.  `output_tokens'
;; is deliberately excluded: it is the freshly-generated response, not
;; part of the context that turn consumed.
;;
;; Mode-line layout (left-to-right): parent label → model → context →
;; prompt-summary → ai-title.
;;
;; Reading the jsonl on every mode-line redraw would re-stat and
;; re-scan a multi-MB file; we cache (mtime, path, used) on the
;; workspace plist and only re-scan the last
;; `claude-repl-context-scan-bytes' of the file when mtime changes.
;;
;; Path resolution and mtime keying are deliberately reused from
;; `ai-title.el' (`claude-repl--ai-title-jsonl-path' /
;; `claude-repl--ai-title-mtime') since all three segments resolve the
;; exact same per-workspace jsonl; this file is loaded after
;; `ai-title.el' so those helpers are available.

;;; Code:

(require 'json)
(require 'cl-lib)

;;;; Defcustoms

(defcustom claude-repl-context-enabled t
  "Non-nil to render context-window utilization in the vterm mode-line."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-context-scan-bytes 32768
  "Number of bytes to read from the end of the session jsonl when scanning
for the most recent assistant usage.  Each entry's usage object is short,
so the tail of the file is enough; reading the whole file on every
mode-line refresh would be wasteful for large transcripts."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-context-window-tokens 200000
  "Size of the model's context window in tokens, used as the denominator
for the utilization percentage.  Defaults to the 200k window Claude
Code assumes for Opus/Sonnet; set higher when running a long-context
(e.g. 1M) session so the percentage stays meaningful."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-context-warn-percent 70
  "Utilization percentage at or above which the segment paints in its
warning color instead of its normal color."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-context-critical-percent 90
  "Utilization percentage at or above which the segment paints in its
critical color."
  :type 'integer
  :group 'claude-repl)

;;;; Usage extraction

(defun claude-repl--context-usage-total (usage)
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

(defun claude-repl--context-extract-from-tail (tail)
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
                           (total (claude-repl--context-usage-total usage)))
                      (when total
                        (setq found total)))))
      found)))

(defun claude-repl--context-read-from-jsonl (path)
  "Return the most recent assistant context-token total from PATH, or nil.
Reads only the trailing `claude-repl-context-scan-bytes' so this stays
cheap on large transcripts.  Returns nil on missing/unreadable file or
when no assistant usage is present in the scanned tail."
  (when (and path (file-readable-p path))
    (let* ((size (or (file-attribute-size (file-attributes path)) 0))
           (cap claude-repl-context-scan-bytes)
           (start (max 0 (- size cap))))
      (when (> size 0)
        (with-temp-buffer
          (insert-file-contents path nil start size)
          (claude-repl--context-extract-from-tail (buffer-string)))))))

;;;; Cached lookup

(defun claude-repl--context-for-ws (ws)
  "Return the context-token total for WS, using a per-workspace mtime cache.
Cache shape (stored in WS's plist under `:context-cache'):
  (PATH MTIME USED)
If the current resolved (path, mtime) matches the cache, returns the
cached USED without re-reading.  Otherwise re-scans and refreshes the
cache.  Reuses `ai-title.el's path/mtime resolvers since all mode-line
session segments read the identical per-workspace jsonl.  Returns nil
when no usage is available."
  (let* ((path (claude-repl--ai-title-jsonl-path ws))
         (mtime (claude-repl--ai-title-mtime path))
         (cache (claude-repl--ws-get ws :context-cache)))
    (cond
     ((null path) nil)
     ((null mtime) nil)
     ((and (consp cache)
           (equal (nth 0 cache) path)
           (equal (nth 1 cache) mtime))
      (nth 2 cache))
     (t
      (let ((used (claude-repl--context-read-from-jsonl path)))
        (claude-repl--ws-put ws :context-cache (list path mtime used))
        used)))))

;;;; Percentage + formatting

(defun claude-repl--context-percent (used)
  "Return the integer utilization percentage for USED tokens, or nil.
Computes `round(100 * USED / claude-repl-context-window-tokens)', clamped
to the inclusive range 0..100 so a mis-sized window can never render a
nonsensical meter.  Returns nil for nil USED or a non-positive window."
  (when (and (numberp used)
             (numberp claude-repl-context-window-tokens)
             (> claude-repl-context-window-tokens 0))
    (max 0 (min 100 (round (* 100.0 used) claude-repl-context-window-tokens)))))

(defun claude-repl--context-face-for-percent (percent)
  "Return the mode-line face plist for utilization PERCENT.
Green below `claude-repl-context-warn-percent', gold from the warn
threshold up to `claude-repl-context-critical-percent', and tomato at or
above the critical threshold."
  (cond
   ((>= percent claude-repl-context-critical-percent)
    '(:foreground "tomato" :weight bold))
   ((>= percent claude-repl-context-warn-percent)
    '(:foreground "goldenrod" :weight normal))
   (t
    '(:foreground "medium sea green" :weight normal))))

(defun claude-repl--context-format (percent)
  "Return the display string for utilization PERCENT, e.g. `ctx 27%'."
  (format "ctx %d%%" percent))

;;;; Mode-line segment

(defun claude-repl--context-segment ()
  "Return a propertized string for the mode-line's context segment.
Reads `claude-repl--owning-workspace' from the current buffer (set on
every claude-owned vterm buffer) and pulls the workspace's context
utilization.  Returns the empty string when disabled, the workspace is
unknown, or no usage is yet available."
  (if (not claude-repl-context-enabled)
      ""
    (let ((ws (claude-repl--buffer-owner (current-buffer))))
      (if (not ws)
          ""
        (let ((percent (claude-repl--context-percent
                        (claude-repl--context-for-ws ws))))
          (if (null percent)
              ""
            (concat "  "
                    (propertize (claude-repl--context-format percent)
                                'face (claude-repl--context-face-for-percent
                                       percent)))))))))

;;;; Mode-line attachment

(defconst claude-repl--context-mode-line-spec
  '(:eval (claude-repl--context-segment))
  "Trailing `:eval' mode-line segment that paints context utilization.
Captured as a constant so the attach helper can detect (via `equal')
whether a buffer's mode-line already contains it.")

(defun claude-repl--context-attach-to-mode-line (buf)
  "Append the context segment to BUF's `mode-line-format' if missing.
Idempotent — does nothing when the segment is already present, the
buffer is dead, or the buffer's mode-line is not a list."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (listp mode-line-format)
                 (not (member claude-repl--context-mode-line-spec
                              mode-line-format)))
        (setq-local mode-line-format
                    (append mode-line-format
                            (list claude-repl--context-mode-line-spec)))
        (force-mode-line-update t)))))

(defun claude-repl-context-attach-all ()
  "Attach the context segment to every live workspace vterm buffer.
Run automatically when this file loads so reloading claude-repl upgrades
pre-existing vterm buffers.  Also exposed interactively for manual
recovery."
  (interactive)
  (when (and (boundp 'claude-repl--workspaces)
             (hash-table-p claude-repl--workspaces))
    (maphash
     (lambda (_ws plist)
       (let ((buf (plist-get plist :vterm-buffer)))
         (when (and buf (buffer-live-p buf))
           (claude-repl--context-attach-to-mode-line buf))))
     claude-repl--workspaces)))

(claude-repl-context-attach-all)

(provide 'claude-repl-context)
;;; context.el ends here
