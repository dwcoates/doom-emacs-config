;;; context-cost.el --- the expensive-turn alarm in the output window's footer -*- lexical-binding: t; -*-

;;; Commentary:

;; Renders `ProgressView.expensive_turn' (a `ContextCostAlert') as a red
;; message in the footer of every agent-repl OUTPUT (webview) window.
;;
;; WHAT IT IS.  The daemon watches each turn's UNCACHED input cost —
;; `input_tokens' PLUS `cache_creation_input_tokens', because the vendor marks
;; nearly all input cacheable and a cold prompt therefore surfaces as cache
;; CREATION rather than as raw input — and sets the field when that crossed the
;; configured threshold.  The turn re-ingested context that should have been
;; served from the prompt cache, and it was billed for it.
;;
;; TWO MESSAGES, NOT ONE.  A `PROMPT_ORIGIN_CACHE_KEEP_ALIVE' turn is the
;; daemon's own keep-alive ping, a minimal prompt whose entire purpose is to
;; refresh the cache BEFORE its TTL expires.  A ping that paid full freight
;; means the cache was already cold when it fired, so the mitigation is the
;; thing reporting the failure — a strictly louder fact than an expensive user
;; turn, and the proto asks for it as its own message rather than folded into
;; the generic one.
;;
;; NO LOCAL TIMER, DELIBERATELY.  The field's presence IS the alarm's lifetime:
;; the daemon clears it when the next turn starts, and every ProgressView push
;; carries the whole current answer.  A local expiry would let the message
;; outlive, or predecease, the fact it reports — and either way Emacs would be
;; asserting something the daemon never said.  Absence has exactly one reading,
;; "the last turn was cache-efficient", and this file renders nothing for it.
;;
;; THE FOOTER IS THE MODE LINE.  This is the same surface `readiness.el',
;; `prompt-summary.el' and `ai-title.el' attach to, reached through the same
;; `agent-repl-frontend-webview-adopt-hook': the bottom line of the OUTPUT
;; window, which is the only footer the Emacs frontend has.  The workspace is
;; resolved per buffer (`agent-repl--buffer-owner'), so a workspace's alarm can
;; never appear over another workspace's output.
;;
;; This module is ALSO why `progress' is no longer an ignored frame arm.  It
;; was ignored on the settled decision that the consolidated progress footer is
;; webapp-only, and that decision stands for every other field on the message —
;; this file reads `expensive_turn' and nothing else.
;;
;; Run tests with:
;;   emacs -batch -Q -l ert -l test-context-cost.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "core" (ws fmt &rest args))
(declare-function agent-repl--buffer-owner "core" (buf))
(declare-function agent-repl--frontend-ws-name "frontend-state" (workspace))
(declare-function agent-repl--frontend-int64 "frontend-state" (raw))
(declare-function agent-repl--uds-register-handler "frontend-uds" (field fn))

(defvar agent-repl-frontend-webview-adopt-hook)
(defvar agent-repl--color-thinking-red)

;;;; Face

(defface agent-repl-context-cost-alert
  `((t :foreground ,(if (boundp 'agent-repl--color-thinking-red)
                        agent-repl--color-thinking-red
                      "#cc3333")
       :weight bold))
  "Face for the expensive-turn alarm in the output window's footer.
Red, and red from the module's ONE red constant rather than a fresh hex:
a surface that invents its own shade is a surface that can drift away
from the color vocabulary every other agent-repl element shares."
  :group 'agent-repl)

;;;; State

(defvar agent-repl--context-cost-alerts (make-hash-table :test 'equal)
  "Hash of workspace NAME -> a NORMALIZED alarm plist, or absent.
The plist carries `:turn-id', `:uncached', `:threshold' and `:keep-alive'
— the wire values already decoded, so the renderer reads numbers rather
than re-deciding what a protojson int64 or enum means on every redisplay.

Absent means the daemon is not reporting an expensive turn for that
workspace, which is the only reading of absence: the daemon sends the
whole current answer on every ProgressView, so an entry lives exactly as
long as the wire field does.")

(defconst agent-repl--context-cost-keep-alive-origin "PROMPT_ORIGIN_CACHE_KEEP_ALIVE"
  "The protojson name of the cache keep-alive prompt origin.
Mirrors `agentshim.core.v1.PromptOrigin' value 29.")

(defconst agent-repl--context-cost-keep-alive-origin-number 29
  "The numeric wire value of `PROMPT_ORIGIN_CACHE_KEEP_ALIVE'.
protojson emits enums by NAME, but a hand-built frame (and a client
configured to emit numbers) may carry the number, and reading only one
of the two encodings would silently downgrade the loudest alarm here to
the generic message.")

(defun agent-repl--context-cost-keep-alive-p (alert)
  "Return non-nil when ALERT's `promptOrigin' is the cache keep-alive origin."
  (let ((origin (plist-get alert :promptOrigin)))
    (cond ((stringp origin)
           (equal origin agent-repl--context-cost-keep-alive-origin))
          ((numberp origin)
           (= origin agent-repl--context-cost-keep-alive-origin-number)))))

(defun agent-repl--context-cost-normalize (ws alert)
  "Decode wire ALERT for workspace WS into the normalized alarm plist.

Signals when either token count is missing or unreadable.  Both are
REQUIRED by the contract — the count is meaningless without the threshold
it crossed, and the threshold is daemon config the frontend must not
supply for itself — so a placeholder would be a fabricated reading of a
number the daemon actually sent, or failed to.  There is no fallback
count (AGENTS.md No-Silent-Fallbacks)."
  (let ((uncached (agent-repl--frontend-int64
                   (plist-get alert :uncachedInputTokens)))
        (threshold (agent-repl--frontend-int64
                    (plist-get alert :thresholdTokens))))
    (unless (and uncached threshold)
      (agent-repl--log ws
                       "context-cost-normalize: REJECTED ws=%s turn=%s uncached=%S threshold=%S"
                       ws (plist-get alert :turnId)
                       (plist-get alert :uncachedInputTokens)
                       (plist-get alert :thresholdTokens))
      (error "agent-repl context-cost: ContextCostAlert missing a token count for %s" ws))
    (list :turn-id (plist-get alert :turnId)
          :uncached uncached
          :threshold threshold
          :keep-alive (and (agent-repl--context-cost-keep-alive-p alert) t))))

;;;; Frame application

(defun agent-repl--context-cost-apply (progress)
  "Apply PROGRESS (a `ProgressView' plist) to the expensive-turn alarm.
Records the decoded `expensiveTurn' under the frame's workspace, or
clears the workspace's record when the field is absent.  Returns the
recorded alarm plist, or nil when nothing is recorded.

A frame whose workspace resolves to no known workspace is DROPPED loudly
rather than keyed under the raw wire cwd — a stub entry would hold an
alarm no footer ever reads."
  (let* ((wire (plist-get progress :workspace))
         (ws (agent-repl--frontend-ws-name wire))
         (alert (plist-get progress :expensiveTurn)))
    (cond
     ((null ws)
      (agent-repl--log nil
                       "context-cost-apply: DROPPED unresolvable workspace=%S expensive=%s"
                       wire (and alert t))
      nil)
     (alert
      (let ((normalized (agent-repl--context-cost-normalize ws alert)))
        (puthash ws normalized agent-repl--context-cost-alerts)
        (agent-repl--log ws
                         "context-cost-apply: ws=%s ALERT turn=%s uncached=%d threshold=%d keep-alive=%s"
                         ws (plist-get normalized :turn-id)
                         (plist-get normalized :uncached)
                         (plist-get normalized :threshold)
                         (plist-get normalized :keep-alive))
        (force-mode-line-update t)
        normalized))
     (t
      (let ((had (gethash ws agent-repl--context-cost-alerts)))
        (remhash ws agent-repl--context-cost-alerts)
        ;; Clearing is the common case — every cache-efficient turn pushes it —
        ;; so only the transition out of an alarm is worth a durable line.
        (if had
            (agent-repl--log ws "context-cost-apply: ws=%s CLEARED" ws)
          (agent-repl--log-verbose ws "context-cost-apply: ws=%s no-alert" ws)))
      (force-mode-line-update t)
      nil))))

;;;; Rendering

(defun agent-repl--context-cost-text (ws)
  "Return the alarm text for workspace WS, or nil when there is no alarm.
The generic message names the observed uncached cost against the
threshold that tripped.  A cache keep-alive turn gets its own wording:
the ping exists to keep the cache warm, so one that paid full freight is
reporting that the cache was ALREADY cold, which is a different fact from
a user turn that happened to be expensive."
  (when-let ((alarm (gethash ws agent-repl--context-cost-alerts)))
    (format "%s%d uncached tokens (threshold %d)"
            (if (plist-get alarm :keep-alive) "keep-alive hit a cold cache: " "")
            (plist-get alarm :uncached)
            (plist-get alarm :threshold))))

(defun agent-repl--context-cost-segment ()
  "Return the propertized footer segment for the current buffer's workspace.
Empty string when the buffer owns no workspace or that workspace has no
alarm.  Evaluated on every mode-line redisplay, so it neither logs nor
touches anything but the alarm hash."
  (let ((ws (agent-repl--buffer-owner (current-buffer))))
    (if (not ws)
        ""
      (let ((text (agent-repl--context-cost-text ws)))
        (if (not text)
            ""
          (concat "  " (propertize text 'face 'agent-repl-context-cost-alert)))))))

;;;; Footer attachment

(defconst agent-repl--context-cost-mode-line-spec
  '(:eval (agent-repl--context-cost-segment))
  "Trailing `:eval' mode-line segment painting the expensive-turn alarm.
Captured as a constant so the attach helper can detect (via `member')
whether a buffer's mode-line already carries it.")

(defun agent-repl--context-cost-attach-to-mode-line (buf)
  "Append the alarm segment to BUF's `mode-line-format' if missing.
Idempotent — does nothing when the segment is already present, the buffer
is dead, or the buffer's mode-line is not a list."
  (if (not (buffer-live-p buf))
      (agent-repl--log nil "context-cost attach: skipped dead buffer=%S" buf)
    (with-current-buffer buf
      (let* ((format-is-list (listp mode-line-format))
             (already (and format-is-list
                           (member agent-repl--context-cost-mode-line-spec
                                   mode-line-format))))
        (if (and format-is-list (not already))
            (progn
              (setq-local mode-line-format
                          (append mode-line-format
                                  (list agent-repl--context-cost-mode-line-spec)))
              (force-mode-line-update t)
              (agent-repl--log nil "context-cost attach: attached buffer=%S"
                               (buffer-name buf)))
          (agent-repl--log nil
                           "context-cost attach: skipped buffer=%S format-is-list=%s already=%s"
                           (buffer-name buf) format-is-list (not (null already))))))))

(defun agent-repl--context-cost-attach-to-current-webview ()
  "Attach the alarm segment to the current buffer.
Registered on `agent-repl-frontend-webview-adopt-hook', which frontend.el
runs inside every freshly adopted webview buffer — the OUTPUT window."
  (agent-repl--context-cost-attach-to-mode-line (current-buffer)))

(add-hook 'agent-repl-frontend-webview-adopt-hook
          #'agent-repl--context-cost-attach-to-current-webview)

;;;; Transport wiring

(agent-repl--uds-register-handler "progress" #'agent-repl--context-cost-apply)

(provide 'agent-repl-context-cost)
;;; context-cost.el ends here
