;;; prompt-summary.el --- last-prompt summary in vterm mode-line -*- lexical-binding: t; -*-

;;; Commentary:

;; After the user submits a prompt in the input panel, kick off an async
;; headless `claude -p --model haiku' call to produce a short title for
;; that prompt.  The result lands in the vterm buffer's mode-line (the
;; bar that sits between the vterm output and the input panel — visually
;; the input panel's status area), in blue, after the BARE METAL /
;; DOCKER SANDBOX environment label.
;;
;; Concurrency model: each kickoff captures the workspace name and the
;; raw prompt text in lexical scope.  The process sentinel writes the
;; summary back to that workspace's state only if the workspace's
;; `:last-prompt-text' still equals the captured raw — otherwise a newer
;; send has already overwritten the bookmark and the stale summary is
;; dropped.  This keeps races (rapid successive sends) from displaying a
;; summary for a prompt the user has since superseded.

;;; Code:

;;;; Defcustoms

(defcustom claude-repl-prompt-summary-enabled t
  "Non-nil to fire async haiku summaries after each prompt send."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-prompt-summary-program "claude"
  "Executable used to generate prompt summaries.
Invoked with `-p --model MODEL' and the prompt sent on stdin."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-prompt-summary-model "haiku"
  "Model alias or ID passed to `--model' for prompt summaries."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-prompt-summary-max-length 60
  "Maximum number of characters to display for the summary segment.
Also used in the prompt template the model receives, so the model
aims for titles within this budget."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-prompt-summary-input-truncate 2000
  "Truncate the raw prompt to this many characters before sending to the
summarizer.  Beyond this length, summary quality plateaus and we waste
tokens."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-prompt-summary-prompt-format
  "Summarize the following user prompt as a brief, descriptive title in under %d characters. Output ONLY the title — no quotes, no preamble like \"Title:\", no trailing punctuation, no markdown. Just the bare title text on a single line.\n\nPROMPT:\n%s"
  "Format string used to wrap the raw prompt before sending to the model.
First %s slot is the max-length integer, second is the raw prompt text."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-prompt-summary-pending-label "summarizing…"
  "Placeholder shown in the mode-line while the haiku call is in flight."
  :type 'string
  :group 'claude-repl)

;;;; State helpers

(defun claude-repl--prompt-summary-segment ()
  "Return a propertized string for the mode-line's summary segment.
Reads `claude-repl--owning-workspace' from the current buffer (set on
every claude-owned vterm buffer by `claude-repl--create-buffer') and
pulls the workspace's :last-prompt-summary / :last-prompt-summary-pending
state.  Returns the empty string when no prompt has been sent yet."
  (let ((ws (and (boundp 'claude-repl--owning-workspace)
                 claude-repl--owning-workspace)))
    (if (not ws)
        ""
      (let ((summary (claude-repl--ws-get ws :last-prompt-summary))
            (pending (claude-repl--ws-get ws :last-prompt-summary-pending))
            (raw     (claude-repl--ws-get ws :last-prompt-text)))
        (cond
         ((and (stringp summary) (not (string-empty-p summary)))
          (concat "  "
                  (propertize (claude-repl--prompt-summary-truncate summary)
                              'face '(:foreground "deep sky blue" :weight normal))))
         (pending
          (concat "  "
                  (propertize
                   (concat (claude-repl--prompt-summary-truncate
                            (or raw "") 'allow-empty)
                           (when (and raw (not (string-empty-p (string-trim raw))))
                             " ")
                           claude-repl-prompt-summary-pending-label)
                   'face '(:foreground "deep sky blue" :weight normal :slant italic))))
         (t ""))))))

(defun claude-repl--prompt-summary-truncate (s &optional allow-empty)
  "Return S trimmed to a single line, capped at the summary max length.
Multiple lines collapse to spaces; trailing ellipsis is added when the
input is longer than the cap.  Returns the empty string for nil or
all-whitespace S unless ALLOW-EMPTY is non-nil (in which case nil
becomes \"\")."
  (let* ((raw (or s ""))
         (collapsed (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim raw)))
         (max claude-repl-prompt-summary-max-length))
    (cond
     ((and (string-empty-p collapsed) (not allow-empty)) "")
     ((<= (length collapsed) max) collapsed)
     (t (concat (substring collapsed 0 (max 1 (- max 1))) "…")))))

;;;; Async kickoff

(defun claude-repl--prompt-summary-skip-p (raw)
  "Return non-nil when RAW should not trigger a summary.
Skips empty/whitespace-only inputs and inputs that look like quick
control commands (single-character / pure-digit / single slash command
without arguments) — these don't carry enough content to summarize and
shouldn't blow away the previous summary."
  (let ((trimmed (string-trim (or raw ""))))
    (or (string-empty-p trimmed)
        (string-match-p "\\`[0-9]+\\'" trimmed)
        (string-match-p "\\`[yYnN]\\'" trimmed)
        (string-match-p "\\`/[A-Za-z0-9_-]+\\'" trimmed))))

(defun claude-repl--prompt-summary-build-input (raw)
  "Render the prompt text sent to the summarizer for RAW."
  (let* ((cap claude-repl-prompt-summary-input-truncate)
         (raw (or raw ""))
         (trimmed (if (> (length raw) cap)
                      (substring raw 0 cap)
                    raw)))
    (format claude-repl-prompt-summary-prompt-format
            claude-repl-prompt-summary-max-length
            trimmed)))

(defun claude-repl--prompt-summary-clean (out)
  "Normalize OUT (raw stdout from claude) into a single-line summary.
Strips surrounding whitespace, drops surrounding ASCII or smart quotes,
collapses internal whitespace, and trims any trailing terminal punctuation."
  (let* ((s (or out ""))
         (s (string-trim s))
         ;; Drop a one-line preamble like "Title:" or "Summary:" if present.
         (s (replace-regexp-in-string
             "\\`\\(?:[Tt]itle\\|[Ss]ummary\\)\\s-*:\\s-*" "" s))
         ;; Strip wrapping quotes (matching pairs only).
         (s (cond
             ((and (string-match-p "\\`\".*\"\\'" s)) (substring s 1 -1))
             ((and (string-match-p "\\`'.*'\\'" s))   (substring s 1 -1))
             (t s)))
         (s (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim s)))
         (s (replace-regexp-in-string "[.!,;:]+\\'" "" s)))
    s))

(defun claude-repl--prompt-summary-apply (ws raw summary)
  "Apply SUMMARY for prompt RAW to workspace WS.
Drops the write when WS's :last-prompt-text no longer matches RAW
(meaning a newer send has displaced the bookmark).  Forces a mode-line
update on the vterm buffer so the new segment paints immediately."
  (let ((current-raw (claude-repl--ws-get ws :last-prompt-text)))
    (if (not (equal current-raw raw))
        (claude-repl--log ws "prompt-summary: drop stale summary (raw mismatched)")
      (claude-repl--ws-put ws :last-prompt-summary summary)
      (claude-repl--ws-put ws :last-prompt-summary-pending nil)
      (claude-repl--prompt-summary-redisplay ws))))

(defun claude-repl--prompt-summary-redisplay (ws)
  "Force a mode-line redisplay for WS's vterm buffer."
  (let ((buf (claude-repl--ws-get ws :vterm-buffer)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (force-mode-line-update t)))))

(defun claude-repl--prompt-summary-make-sentinel (ws raw out-buf)
  "Build a process sentinel that writes the summary for WS/RAW.
OUT-BUF is the process's stdout collection buffer; it is killed when
the process exits."
  (lambda (proc event)
    (when (memq (process-status proc) '(exit signal))
      (unwind-protect
          (let* ((status (process-exit-status proc))
                 (raw-out (and (buffer-live-p out-buf)
                               (with-current-buffer out-buf (buffer-string))))
                 (cleaned (and raw-out (claude-repl--prompt-summary-clean raw-out))))
            (claude-repl--log ws "prompt-summary: sentinel ws=%s status=%s event=%s len=%s"
                              ws status (string-trim (or event ""))
                              (if cleaned (length cleaned) "nil"))
            (cond
             ((and (eq (process-status proc) 'exit)
                   (zerop status)
                   cleaned
                   (not (string-empty-p cleaned)))
              (claude-repl--prompt-summary-apply ws raw cleaned))
             (t
              ;; Failed / empty — clear pending so the segment goes back
              ;; to whatever summary was there before (or nothing).
              (let ((current-raw (claude-repl--ws-get ws :last-prompt-text)))
                (when (equal current-raw raw)
                  (claude-repl--ws-put ws :last-prompt-summary-pending nil)
                  (claude-repl--prompt-summary-redisplay ws))))))
        (when (buffer-live-p out-buf)
          (kill-buffer out-buf))))))

(defun claude-repl--prompt-summary-spawn (ws raw)
  "Spawn the async claude process to summarize RAW for WS.
Returns the process, or nil when prerequisites are missing.  Separated
from `claude-repl--kickoff-prompt-summary' so tests can stub
`make-process' / `process-send-string' here without going through the
state-mutation entry point."
  (let* ((out-buf (generate-new-buffer
                   (format " *claude-prompt-summary-%s*" ws)))
         (cmd (list claude-repl-prompt-summary-program
                    "-p" "--model" claude-repl-prompt-summary-model))
         (proc-input (claude-repl--prompt-summary-build-input raw))
         (sentinel (claude-repl--prompt-summary-make-sentinel ws raw out-buf)))
    (condition-case err
        (let ((proc (make-process
                     :name (format "claude-prompt-summary-%s" ws)
                     :buffer out-buf
                     :command cmd
                     :connection-type 'pipe
                     :noquery t
                     :sentinel sentinel)))
          (process-send-string proc proc-input)
          (process-send-eof proc)
          proc)
      (error
       (claude-repl--log ws "prompt-summary: spawn failed err=%S" err)
       (when (buffer-live-p out-buf) (kill-buffer out-buf))
       nil))))

(defun claude-repl--kickoff-prompt-summary (ws raw)
  "Fire an async haiku summary call for RAW in workspace WS.
No-op when the feature is disabled, WS is nil, or RAW is too small to
summarize.  Bookmarks WS + RAW into the workspace state immediately so
the mode-line shows a pending placeholder, then captures both into the
sentinel closure so the result is written back to the right workspace
even if the user has switched perspectives by the time it resolves."
  (when (and claude-repl-prompt-summary-enabled
             ws
             (stringp raw)
             (not (claude-repl--prompt-summary-skip-p raw)))
    (claude-repl--log ws "prompt-summary: kickoff ws=%s raw-len=%d" ws (length raw))
    (claude-repl--ws-put ws :last-prompt-text raw)
    (claude-repl--ws-put ws :last-prompt-summary nil)
    (claude-repl--ws-put ws :last-prompt-summary-pending t)
    (claude-repl--prompt-summary-redisplay ws)
    (claude-repl--prompt-summary-spawn ws raw)))

(provide 'claude-repl-prompt-summary)
;;; prompt-summary.el ends here
