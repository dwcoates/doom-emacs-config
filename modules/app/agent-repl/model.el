;;; model.el --- Agent model transcript reader -*- lexical-binding: t; -*-

;;; Commentary:

;; Read the model actually in use by a workspace's session from its own
;; session jsonl.
;;
;; Emacs does not display the model at all, by design.  This file used
;; to ALSO render it as a segment in the vterm mode-line; the vterm
;; frontend is gone, and the sole surviving frontend (the xwidget
;; webview) has no mode-line of its own — the webapp topbar already
;; renders model + tokens directly (`webapp/src/render.ts').  The
;; display-formatting helper that survived that removal
;; (`agent-repl--model-prettify', which turned `claude-opus-4-8' into
;; `Opus 4.8') had no remaining callers and is now deleted too; nothing
;; in elisp turns a model id into user-facing text.
;;
;; What remains here is the reading capability itself, whose values are
;; consumed as DATA rather than displayed, and which two callers still
;; depend on:
;;
;;   - `backend.el' registers `agent-repl--model-read-from-jsonl' as
;;     the claude backend's TRANSCRIPT-MODEL-FN capability.
;;   - `history.el' calls `agent-repl--model-persist-value' when saving
;;     a workspace snapshot, so a restored session relaunches under the
;;     model it was actually last running (captured from the transcript
;;     in case a mid-session `/model' switch happened, rather than the
;;     stale workspace-generation model).
;;
;; Source of truth: the workspace's own session jsonl under
;; `~/.claude/projects/<encoded-cwd>/<session-id>.jsonl' — the same
;; "corresponding workspace's ~/.claude/* file" that `ai-title.el' reads.
;; Every main-chain `type:assistant' line carries the model that served
;; it under `message.model' (e.g. `claude-opus-4-8'); we surface the
;; most recent one so callers see the live model even after a
;; mid-session `/model' switch.
;;
;; Reading the jsonl on every lookup would re-stat and re-scan a
;; multi-MB file; we cache (mtime, path, model) on the workspace plist
;; and only re-scan the last `agent-repl-model-scan-bytes' of the file
;; when mtime changes.  Each assistant line is small so scanning the
;; file tail is sufficient.
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

(defcustom agent-repl-model-scan-bytes 32768
  "Number of bytes to read from the end of the session jsonl when scanning
for the most recent assistant model.  Each entry's model field is short,
so the tail of the file is enough; reading the whole file on every
lookup would be wasteful for large transcripts."
  :type 'integer
  :group 'agent-repl)

;;;; File reading

(defun agent-repl--model-extract-from-tail (tail)
  "Return the most recent assistant model string found in TAIL, or nil.
TAIL is the trailing chunk of a session jsonl (one JSON object per
line).  Walks lines bottom-up so we hit the most recent assistant entry
first.  Skips lines that are not main-chain `type:assistant' entries
\(sidechain lines, e.g. subagent or headless-haiku traffic, are ignored
so the result reflects the primary conversation model), don't parse, or
carry no string `message.model'."
  (if (not (and (stringp tail) (not (string-empty-p tail))))
      (progn
        ;; This is a pure tail parser; no workspace exists at this boundary.
        (agent-repl--log-verbose nil
                                 "model-extract-from-tail: tail-type=%S result=empty-input"
                                 (type-of tail))
        nil)
    (let ((lines (split-string tail "\n" t))
          (found nil)
          (candidate-count 0)
          (sidechain-count 0)
          (parse-error-count 0)
          (rejected-model-count 0))
      (dolist (line (nreverse lines))
        (when (and (not found)
                   (string-match-p "\"type\":\"assistant\"" line))
          (if (string-match-p "\"isSidechain\":true" line)
              (cl-incf sidechain-count)
            (cl-incf candidate-count)
            (let ((parse-error nil)
                  (parsed
                   (condition-case err
                       (let ((json-object-type 'alist)
                             (json-array-type 'list)
                             (json-key-type 'string)
                             (json-false nil)
                             (json-null nil))
                         (json-read-from-string line))
                     (error
                      (setq parse-error err)
                      nil))))
              (if parse-error
                  (cl-incf parse-error-count)
                (let* ((message (and (listp parsed)
                                     (cdr (assoc "message" parsed))))
                       (model (and (listp message)
                                   (cdr (assoc "model" message)))))
                  (if (and (stringp model) (not (string-empty-p model)))
                      (setq found model)
                    (cl-incf rejected-model-count))))))))
      ;; A scan can accompany frequent transcript-derived lookups, so keep
      ;; its branch-level detail behind the verbose gate.
      (agent-repl--log-verbose nil
                               "model-extract-from-tail: chars=%d lines=%d candidates=%d skipped-sidechains=%d parse-errors=%d rejected-models=%d result=%S"
                               (length tail) (length lines) candidate-count sidechain-count
                               parse-error-count rejected-model-count found)
      found)))

(defun agent-repl--model-read-from-jsonl (path)
  "Return the most recent assistant model string from PATH, or nil.
Reads only the trailing `agent-repl-model-scan-bytes' (via
`agent-repl--transcript-read-tail') so this stays cheap on large
transcripts.  Returns nil on missing/unreadable file or when no
assistant entry is present in the scanned tail."
  (let ((tail (agent-repl--transcript-read-tail
               path agent-repl-model-scan-bytes)))
    (if tail
        (let ((model (agent-repl--model-extract-from-tail tail)))
          ;; The backend reader contract carries only PATH, not WS.
          (agent-repl--log-verbose nil
                                   "model-read-from-jsonl: path=%S scan-bytes=%d tail-chars=%d model=%S"
                                   path agent-repl-model-scan-bytes (length tail) model)
          model)
      (agent-repl--log-verbose nil
                               "model-read-from-jsonl: path=%S scan-bytes=%d result=no-tail"
                               path agent-repl-model-scan-bytes)
      nil)))

;;;; Cached lookup

(defun agent-repl--model-for-ws (ws)
  "Return the raw model id for WS, using a per-workspace mtime cache.
Delegates to `agent-repl--transcript-cached' with the `:model-cache'
key and WS's backend TRANSCRIPT-MODEL-FN reader (this file's
`agent-repl--model-read-from-jsonl' for the claude backend).  Returns
nil when no model is available."
  (let ((model (agent-repl--transcript-cached
                ws :model-cache #'agent-repl-backend-transcript-model-fn)))
    (agent-repl--log-verbose ws
                             "model-for-ws: ws=%s cache-key=%s model=%S"
                             ws :model-cache model)
    model))

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
  (let* ((transcript-model (agent-repl--model-for-ws ws))
         (workspace-model (agent-repl--ws-get ws :model))
         (model (or transcript-model workspace-model))
         (source (cond
                  (transcript-model 'transcript)
                  (workspace-model 'workspace)
                  (t 'none))))
    (agent-repl--log ws
                     "model-persist-value: ws=%s transcript-model=%S workspace-model=%S selected-model=%S source=%s"
                     ws transcript-model workspace-model model source)
    model))

(provide 'agent-repl-model)
;;; model.el ends here
