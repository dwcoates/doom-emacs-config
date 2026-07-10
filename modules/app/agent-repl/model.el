;;; model.el --- Claude model in vterm mode-line -*- lexical-binding: t; -*-

;;; Commentary:

;; Render the Claude model actually in use by a workspace's session in
;; the vterm mode-line, alongside the prompt-summary and aiTitle
;; segments.
;;
;; Source of truth: the workspace's own session jsonl under
;; `~/.claude/projects/<encoded-cwd>/<session-id>.jsonl' — the same
;; "corresponding workspace's ~/.claude/* file" that `ai-title.el' reads.
;; Every main-chain `type:assistant' line carries the model that served
;; it under `message.model' (e.g. `claude-opus-4-8'); we surface the
;; most recent one so the segment reflects the live model even after a
;; mid-session `/model' switch.
;;
;; Mode-line layout (left-to-right): parent label → model →
;; prompt-summary → ai-title.
;;
;; Reading the jsonl on every mode-line redraw would re-stat and
;; re-scan a multi-MB file; we cache (mtime, path, model) on the
;; workspace plist and only re-scan the last
;; `agent-repl-model-scan-bytes' of the file when mtime changes.  Each
;; assistant line is small so scanning the file tail is sufficient.
;;
;; Path resolution and mtime keying are deliberately reused from
;; `ai-title.el' (`agent-repl--ai-title-jsonl-path' /
;; `agent-repl--ai-title-mtime') since both segments resolve the exact
;; same per-workspace jsonl; this file is loaded after `ai-title.el' so
;; those helpers are available.

;;; Code:

(require 'json)
(require 'cl-lib)

;;;; Defcustoms

(defcustom agent-repl-model-enabled t
  "Non-nil to render the Claude model in the vterm mode-line."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-model-scan-bytes 32768
  "Number of bytes to read from the end of the session jsonl when scanning
for the most recent assistant model.  Each entry's model field is short,
so the tail of the file is enough; reading the whole file on every
mode-line refresh would be wasteful for large transcripts."
  :type 'integer
  :group 'agent-repl)

;;;; Model id prettifying

(defun agent-repl--model-prettify (model)
  "Return a compact human label for raw MODEL id, or nil.
Strips the leading `claude-' vendor prefix, then renders a known family
(`opus'/`sonnet'/`haiku') capitalized with any trailing `-MAJOR-MINOR'
version rejoined with a dot (e.g. `claude-opus-4-8' → `Opus 4.8').
Falls back to the prefix-stripped id verbatim for unrecognized shapes.
Returns nil for nil/empty input so callers can short-circuit."
  (when (and (stringp model) (not (string-empty-p model)))
    (let ((s (if (string-prefix-p "claude-" model)
                 (substring model (length "claude-"))
               model)))
      (if (string-match
           "\\`\\(opus\\|sonnet\\|haiku\\)\\(?:-\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?\\)?"
           s)
          (let ((fam (capitalize (match-string 1 s)))
                (maj (match-string 2 s))
                (min (match-string 3 s)))
            (cond
             ((and maj min) (format "%s %s.%s" fam maj min))
             (maj (format "%s %s" fam maj))
             (t fam)))
        s))))

;;;; File reading

(defun agent-repl--model-extract-from-tail (tail)
  "Return the most recent assistant model string found in TAIL, or nil.
TAIL is the trailing chunk of a session jsonl (one JSON object per
line).  Walks lines bottom-up so we hit the most recent assistant entry
first.  Skips lines that are not main-chain `type:assistant' entries
\(sidechain lines, e.g. subagent or headless-haiku traffic, are ignored
so the segment reflects the primary conversation model), don't parse, or
carry no string `message.model'."
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
                           (model (and (listp message)
                                       (cdr (assoc "model" message)))))
                      (when (and (stringp model) (not (string-empty-p model)))
                        (setq found model)))))
      found)))

(defun agent-repl--model-read-from-jsonl (path)
  "Return the most recent assistant model string from PATH, or nil.
Reads only the trailing `agent-repl-model-scan-bytes' so this stays
cheap on large transcripts.  Returns nil on missing/unreadable file or
when no assistant entry is present in the scanned tail."
  (when (and path (file-readable-p path))
    (let* ((size (or (file-attribute-size (file-attributes path)) 0))
           (cap agent-repl-model-scan-bytes)
           (start (max 0 (- size cap))))
      (when (> size 0)
        (with-temp-buffer
          (insert-file-contents path nil start size)
          (agent-repl--model-extract-from-tail (buffer-string)))))))

;;;; Cached lookup

(defun agent-repl--model-for-ws (ws)
  "Return the raw model id for WS, using a per-workspace mtime cache.
Cache shape (stored in WS's plist under `:model-cache'):
  (PATH MTIME MODEL)
If the current resolved (path, mtime) matches the cache, returns the
cached MODEL without re-reading.  Otherwise re-scans and refreshes the
cache.  Reuses `ai-title.el's path/mtime resolvers since both segments
read the identical per-workspace session jsonl.  Returns nil when no
model is available."
  (let* ((path (agent-repl--ai-title-jsonl-path ws))
         (mtime (agent-repl--ai-title-mtime path))
         (cache (agent-repl--ws-get ws :model-cache)))
    (cond
     ((null path) nil)
     ((null mtime) nil)
     ((and (consp cache)
           (equal (nth 0 cache) path)
           (equal (nth 1 cache) mtime))
      (nth 2 cache))
     (t
      (let ((model (agent-repl--model-read-from-jsonl path)))
        (agent-repl--ws-put ws :model-cache (list path mtime model))
        model)))))

;;;; Persisted model resolution

(defun agent-repl--model-persist-value (ws)
  "Return the model id to persist for WS as its session's current model.
Inspects WS's configured Claude config dir via `agent-repl--model-for-ws',
which reads the most recent main-chain assistant model from the
workspace's own session jsonl under `<config-dir>/projects/...', so a
mid-session `/model' switch (e.g. `opus' to `fable') is captured.  Falls
back to WS's `:model' plist value (the workspace-generation model) when
the session has produced no assistant turn yet, and to nil when neither
source yields a model.  Callers persist the result via
`agent-repl--state-save' so a later restore re-launches the session
under the same model."
  (or (agent-repl--model-for-ws ws)
      (agent-repl--ws-get ws :model)))

;;;; Mode-line segment

(defun agent-repl--model-segment ()
  "Return a propertized string for the mode-line's model segment.
Reads `agent-repl--owning-workspace' from the current buffer (set on
every claude-owned vterm buffer) and pulls the workspace's model.
Returns the empty string when disabled, the workspace is unknown, or no
model is yet available."
  (if (not agent-repl-model-enabled)
      ""
    (let ((ws (agent-repl--buffer-owner (current-buffer))))
      (if (not ws)
          ""
        (let ((label (agent-repl--model-prettify (agent-repl--model-for-ws ws))))
          (if (or (null label) (string-empty-p label))
              ""
            (concat "  "
                    (propertize label
                                'face '(:foreground "deep sky blue"
                                        :weight normal)))))))))

;;;; Mode-line attachment

(defconst agent-repl--model-mode-line-spec
  '(:eval (agent-repl--model-segment))
  "Trailing `:eval' mode-line segment that paints the Claude model.
Captured as a constant so the attach helper can detect (via `equal')
whether a buffer's mode-line already contains it.")

(defun agent-repl--model-attach-to-mode-line (buf)
  "Append the model segment to BUF's `mode-line-format' if missing.
Idempotent — does nothing when the segment is already present, the
buffer is dead, or the buffer's mode-line is not a list."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (listp mode-line-format)
                 (not (member agent-repl--model-mode-line-spec
                              mode-line-format)))
        (setq-local mode-line-format
                    (append mode-line-format
                            (list agent-repl--model-mode-line-spec)))
        (force-mode-line-update t)))))

(defun agent-repl-model-attach-all ()
  "Attach the model segment to every live workspace vterm buffer.
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
           (agent-repl--model-attach-to-mode-line buf))))
     agent-repl--workspaces)))

(agent-repl-model-attach-all)

(provide 'agent-repl-model)
;;; model.el ends here
