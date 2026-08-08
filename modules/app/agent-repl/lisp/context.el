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

(defvar agent-repl--context-log-ws nil
  "Dynamically bound workspace whose transcript context is being read.
`agent-repl--transcript-cached' invokes backend readers with PATH only, so
`agent-repl--context-for-ws' binds this while its reader runs.  Keeping the
capability's one-argument signature preserves the backend boundary while
letting every context-reader diagnostic retain workspace metadata.")

(defun agent-repl--context-usage-total (usage &optional ws)
  "Return the input-side token total from a parsed USAGE alist, or nil.
Sums `input_tokens', `cache_creation_input_tokens', and
`cache_read_input_tokens', treating any missing counter as 0.  Returns
nil when USAGE is not an alist or carries none of the three counters, so
callers can skip lines that lack usable usage.  WS supplies logging context
when this helper runs as part of a workspace lookup."
  (if (not (listp usage))
      (progn
        (agent-repl--log ws
                         "context usage-total: usage-type=%S result=no-usage-alist"
                         (type-of usage))
        nil)
    (let ((keys '("input_tokens"
                  "cache_creation_input_tokens"
                  "cache_read_input_tokens"))
          (total 0)
          (numeric-key-count 0))
      (dolist (k keys)
        (let ((v (cdr (assoc k usage))))
          (when (numberp v)
            (cl-incf numeric-key-count)
            (setq total (+ total v)))))
      (agent-repl--log ws
                       "context usage-total: usage=%S numeric-key-count=%d total=%d result=%s"
                       usage numeric-key-count total
                       (if (> numeric-key-count 0) "total" "no-numeric-counters"))
      (when (> numeric-key-count 0) total))))

(defun agent-repl--context-extract-from-tail (tail &optional ws)
  "Return the most recent assistant context-token total found in TAIL, or nil.
TAIL is the trailing chunk of a session jsonl (one JSON object per
line).  Walks lines bottom-up so we hit the most recent assistant entry
first.  Skips lines that are not main-chain `type:assistant' entries
\(sidechain lines are ignored so the figure reflects the primary
conversation), don't parse, or carry no usable `message.usage'.  WS supplies
logging context when available."
  (if (not (and (stringp tail) (not (string-empty-p tail))))
      (progn
        (agent-repl--log ws
                         "context extract: tail-type=%S empty=%s result=invalid-tail"
                         (type-of tail) (and (stringp tail) (string-empty-p tail)))
        nil)
    (let ((lines (split-string tail "\n" t))
          (found nil)
          (candidate-count 0)
          (sidechain-count 0)
          (parse-error-count 0)
          (missing-message-count 0)
          (missing-usage-count 0)
          (no-total-count 0))
      (cl-loop for line in (nreverse lines)
               while (not found)
               do (cond
                   ((not (string-match-p "\"type\":\"assistant\"" line)))
                   ((string-match-p "\"isSidechain\":true" line)
                    (cl-incf sidechain-count))
                   (t
                    (cl-incf candidate-count)
                    (let ((parsed
                           (condition-case _err
                               (let ((json-object-type 'alist)
                                     (json-array-type 'list)
                                     (json-key-type 'string)
                                     (json-false nil)
                                     (json-null nil))
                                 (json-read-from-string line))
                             (error
                              (cl-incf parse-error-count)
                              nil))))
                      (let* ((message (and (listp parsed)
                                           (cdr (assoc "message" parsed))))
                             (usage (and (listp message)
                                         (cdr (assoc "usage" message))))
                             (total (agent-repl--context-usage-total usage ws)))
                        (cond
                         ((not (listp message))
                          (cl-incf missing-message-count))
                         ((not (listp usage))
                          (cl-incf missing-usage-count))
                         (total
                         (setq found total))
                         (t
                          (cl-incf no-total-count))))))))
      (agent-repl--log ws
                       "context extract: tail-bytes=%d lines=%d candidates=%d sidechains=%d parse-errors=%d missing-message=%d missing-usage=%d no-total=%d result=%s total=%S"
                       (string-bytes tail) (length lines) candidate-count sidechain-count
                       parse-error-count missing-message-count missing-usage-count no-total-count
                       (if found "found" "no-context-usage") found)
      found)))

(defun agent-repl--context-read-from-jsonl (path &optional ws)
  "Return the most recent assistant context-token total from PATH, or nil.
Reads only the trailing `agent-repl-context-scan-bytes' (via
`agent-repl--transcript-read-tail') so this stays cheap on large
transcripts.  Returns nil on missing/unreadable file or when no
assistant usage is present in the scanned tail."
  (let* ((log-ws (or ws agent-repl--context-log-ws))
         (tail (agent-repl--transcript-read-tail
                path agent-repl-context-scan-bytes))
         (total (and tail (agent-repl--context-extract-from-tail tail log-ws))))
    (agent-repl--log log-ws
                     "context read: path=%S scan-bytes=%d tail-present=%s tail-bytes=%s result=%s total=%S"
                     path agent-repl-context-scan-bytes (not (null tail))
                     (and tail (string-bytes tail))
                     (if total "context-found" "no-context-usage") total)
    total))

;;;; Cached lookup

(defun agent-repl--context-for-ws (ws)
  "Return the context-token total for WS, using a per-workspace mtime cache.
Delegates to `agent-repl--transcript-cached' with the `:context-cache'
key and WS's backend TRANSCRIPT-CONTEXT-FN reader (this file's
`agent-repl--context-read-from-jsonl' for the claude backend).  Returns
nil when no usage is available."
  ;; WHY: backend transcript-reader capabilities take PATH only; this dynamic
  ;; binding keeps their diagnostic records attached to the caller's workspace.
  (let ((agent-repl--context-log-ws ws)
        (total (agent-repl--transcript-cached
                ws :context-cache #'agent-repl-backend-transcript-context-fn)))
    ;; Mode-line consumers can request this on each redisplay, so keep the
    ;; per-lookup outcome behind verbose logging.
    (agent-repl--log-verbose ws "context for-ws: result=%s total=%S"
                             (if total "context-found" "no-context-usage") total)
    total))

(provide 'agent-repl-context)
;;; context.el ends here
