;;; context.el --- Context-window usage transcript reader -*- lexical-binding: t; -*-

;;; Commentary:

;; Read the context-window utilization of a workspace's Claude session
;; from its own session jsonl.  This file used to ALSO render that
;; figure as a segment in the vterm mode-line, alongside the model and
;; aiTitle segments; the vterm frontend is gone, and the sole surviving
;; frontend (the xwidget webview) has no mode-line of its own — the
;; webapp topbar already renders model + tokens directly
;; (`webapp/src/render.ts').  What remains here is the reading
;; capability itself: `backend.el' registers
;; `agent-repl--context-read-from-jsonl' as the claude backend's
;; TRANSCRIPT-CONTEXT-FN capability, and `agent-repl--context-for-ws'
;; is the cached per-workspace lookup any future caller can use.
;;
;; Source of truth: the workspace's own session jsonl under
;; `~/.claude/projects/<encoded-cwd>/<session-id>.jsonl' — the same
;; per-workspace file `ai-title.el' and `model.el' read.  Every
;; main-chain `type:assistant' line carries a `message.usage' object
;; whose input-side counters describe the tokens that were in context
;; when that turn was served:
;;   input_tokens + cache_creation_input_tokens + cache_read_input_tokens
;; We surface the most recent such total as a raw token count.  We do
;; not divide by a context-window size: the effective window depends on
;; the model and whether a long-context (1M) mode is active, and that
;; figure is not recorded anywhere in the session data, so any
;; percentage would rest on a brittle assumption.  `output_tokens' is
;; deliberately excluded: it is the freshly-generated response, not
;; part of the context that turn consumed.
;;
;; Reading the jsonl on every lookup would re-stat and re-scan a
;; multi-MB file; we cache (mtime, path, used) on the workspace plist
;; and only re-scan the last `agent-repl-context-scan-bytes' of the
;; file when mtime changes.
;;
;; Path resolution and mtime keying are deliberately reused from
;; `ai-title.el' (`agent-repl--ai-title-jsonl-path' /
;; `agent-repl--ai-title-mtime') since every session-transcript reader
;; resolves the exact same per-workspace jsonl; this file is loaded
;; after `ai-title.el' so those helpers are available.

;;; Code:

(require 'json)
(require 'cl-lib)

;;;; Defcustoms

(defcustom agent-repl-context-scan-bytes 32768
  "Number of bytes to read from the end of the session jsonl when scanning
for the most recent assistant usage.  Each entry's usage object is short,
so the tail of the file is enough; reading the whole file on every
lookup would be wasteful for large transcripts."
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
Reads only the trailing `agent-repl-context-scan-bytes' (via
`agent-repl--transcript-read-tail') so this stays cheap on large
transcripts.  Returns nil on missing/unreadable file or when no
assistant usage is present in the scanned tail."
  (let ((tail (agent-repl--transcript-read-tail
               path agent-repl-context-scan-bytes)))
    (and tail (agent-repl--context-extract-from-tail tail))))

;;;; Cached lookup

(defun agent-repl--context-for-ws (ws)
  "Return the context-token total for WS, using a per-workspace mtime cache.
Delegates to `agent-repl--transcript-cached' with the `:context-cache'
key and WS's backend TRANSCRIPT-CONTEXT-FN reader (this file's
`agent-repl--context-read-from-jsonl' for the claude backend).  Returns
nil when no usage is available."
  (agent-repl--transcript-cached
   ws :context-cache #'agent-repl-backend-transcript-context-fn))

(provide 'agent-repl-context)
;;; context.el ends here
