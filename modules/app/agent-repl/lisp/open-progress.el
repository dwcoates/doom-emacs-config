;;; open-progress.el --- Visible feedback while a workspace opens -*- lexical-binding: t; -*-

;;; Commentary:

;; The window between `SPC o c' and a mounted webview is REAL WORK, not a
;; hiccup: the daemon has to be ensured (built and launched when stale or
;; absent), its readiness asserted, `openWorkspace' round-tripped, a session
;; controller brought up or reattached, the transcript backfilled, and only
;; then does an xwidget appear.  Live incidents put that window between 6.8s
;; on a quiet machine and 16.9s on a congested one.
;;
;; For all of it, Emacs used to say NOTHING.  The frame kept whatever was on
;; it, the keypress produced no mark of any kind, and the two bounds that
;; could end the wait — the 10s ack deadline and the 30s open timeout — both
;; resolved into a log line and a `agent-repl--warn' the user never looks at.
;; So a SUCCEEDING open was indistinguishable from a dropped keypress, and a
;; FAILING one was indistinguishable from a slow success.  The observed
;; consequence is people pressing `SPC o c' again, which dispatches a second
;; establishment for a workspace already establishing.
;;
;; This file is the missing surface, and it holds three contracts:
;;
;;   INSTANT.  The placeholder is displayed inside the command that read the
;;   keypress, before any asynchronous stage is even dispatched, so the very
;;   first redisplay after the key already carries the workspace's name and
;;   the word "opening".
;;
;;   PHASED.  Every later line comes from something that ARRIVED — a
;;   continuation firing in the establishment ladder, or a daemon-pushed
;;   `WorkspaceState' reaching `agent-repl-ws-state-transition-functions'.
;;   Nothing here polls the daemon; the escalation timer is the file's only
;;   timer and it measures OUR OWN patience, not the daemon's state.
;;
;;   RESOLVED ON EVERY PATH.  A placeholder is torn down by success, and
;;   REPLACED IN PLACE by a named cause on nack, timeout, or a severed
;;   bring-up.  It never simply vanishes: vanishing is what made the old
;;   behavior unreadable, because it looks exactly like the success it is not.
;;
;; Color follows the module vocabulary (AGENTS.md): the whole ladder is blue,
;; because everything on it is the LOCAL environment — Emacs, the daemon, the
;; shim, the store — and a bring-up in flight is what `:init' blue already
;; means everywhere else.  The escalation is red, the same red a broken route
;; takes on the tab bar.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--sanitize-ws-name "agent-repl-core" (name))
(declare-function agent-repl--frontend-main-area-window "agent-repl-frontend" ())
(declare-function agent-repl--frontend-ws-command-key "agent-repl-frontend-client" (ws))
(declare-function agent-repl--frontend-backfill-settled-p "agent-repl-frontend-client" (workspace))

(defvar agent-repl--color-init-blue)
(defvar agent-repl--color-thinking-red)

;;;; ---- Configuration ----------------------------------------------------

(defcustom agent-repl-open-progress-buffer-name-format " *agent-opening-%s*"
  "Buffer name format for a workspace's open placeholder.
Takes the sanitized workspace name.  Leading space keeps the placeholder
out of the buffer list: it is furniture for one operation, not something
a user navigates to."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-open-progress-escalate-seconds 12
  "Seconds a pending open may sit before the placeholder escalates.

Deliberately BELOW both establishment bounds it stands in front of — the
10s health ack and the 30s `agent-repl-frontend-open-workspace-timeout'.
Those bounds resolve the OPERATION; this one resolves the user's
uncertainty, and it has to fire while there is still something worth
saying.  Set above them and the escalation would only ever restate a
failure the resolution path had already written."
  :type 'number
  :group 'agent-repl)

;;;; ---- Faces ------------------------------------------------------------

(defface agent-repl-open-progress-heading
  '((t :inherit bold))
  "Face for the placeholder's \"Opening <workspace>\" heading."
  :group 'agent-repl)

(defface agent-repl-open-progress-active
  `((t :foreground ,(if (boundp 'agent-repl--color-init-blue)
                        agent-repl--color-init-blue
                      "#3366cc")
       :weight bold))
  "Face for the stage the open is CURRENTLY on (blue: a local bring-up).
Blue is the module's local-environment color and `:init' blue is
specifically \"a bring-up is in flight\", which is exactly this."
  :group 'agent-repl)

(defface agent-repl-open-progress-done
  '((t :inherit shadow))
  "Face for stages the open has already cleared."
  :group 'agent-repl)

(defface agent-repl-open-progress-pending
  '((t :inherit shadow))
  "Face for stages the open has not reached yet."
  :group 'agent-repl)

(defface agent-repl-open-progress-alert
  `((t :foreground ,(if (boundp 'agent-repl--color-thinking-red)
                        agent-repl--color-thinking-red
                      "#cc3333")
       :weight bold))
  "Face for an escalated or failed open (red: the route is broken)."
  :group 'agent-repl)

;;;; ---- The stage ladder -------------------------------------------------

(defconst agent-repl--open-progress-stages
  '((:dispatched  . "Asking the daemon to open this workspace")
    (:daemon-ready . "Daemon reached; waiting for it to report ready")
    (:opening     . "openWorkspace sent; awaiting acknowledgement")
    (:acked       . "Acknowledged; bringing the session up")
    (:backfilling . "Session up; loading the conversation")
    (:rendering   . "Rendering the view"))
  "The ordered stages an open passes through, newest last.
Each entry is (PHASE . LABEL).  The order is the ladder: a phase may
only ever ADVANCE (`agent-repl--open-progress-note' drops a regression),
because a stage report arriving late must not make a nearly-mounted view
claim it is still waiting for the daemon.")

(defconst agent-repl--open-progress-terminal-phases '(:failed :timed-out)
  "Phases that END a placeholder's progress and leave it standing.
Reaching one stops the ladder and replaces it with a named cause; the
buffer is deliberately NOT torn down, because the whole defect this file
exists for is a placeholder that disappears without saying why.")

(defun agent-repl--open-progress-stage-index (phase)
  "Return PHASE's position on the ladder, or nil when it is not a stage."
  (cl-position phase agent-repl--open-progress-stages :key #'car))

;;;; ---- The registry -----------------------------------------------------

(defvar agent-repl--open-progress (make-hash-table :test 'equal)
  "Hash of WORKSPACE NAME -> the plist describing its pending open.

Keys: `:buffer' (the placeholder), `:phase', `:detail' (a cause string on
a terminal phase), `:started' (float-time) and `:timer' (the escalation
timer).

AT MOST ONE ENTRY PER WORKSPACE, and that is the double-invoke contract:
a second `SPC o c' while an open is pending finds this entry, re-shows
the placeholder it already owns, and dispatches nothing.")

(defun agent-repl--open-progress-entry (ws)
  "Return WS's pending-open plist, or nil when no open is pending."
  (and ws (gethash ws agent-repl--open-progress)))

(defun agent-repl--open-progress-active-p (ws)
  "Return non-nil when WS has an open placeholder standing.
The guard `agent-repl--toggle' consults before dispatching an open, and
the reason a second keypress cannot stack a second placeholder."
  (and (agent-repl--open-progress-entry ws) t))

(defun agent-repl--open-progress-buffer-name (ws)
  "Return the placeholder buffer name for workspace WS."
  (format agent-repl-open-progress-buffer-name-format
          (or (agent-repl--sanitize-ws-name ws) "workspace")))

;;;; ---- Rendering --------------------------------------------------------

(defvar agent-repl-open-progress-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `agent-repl-open-progress-mode'.")

(define-derived-mode agent-repl-open-progress-mode special-mode "Agent-Opening"
  "Major mode for the agent-repl workspace-open placeholder."
  (setq-local cursor-type nil))

(defun agent-repl--open-progress-stage-line (phase entry-phase)
  "Return the rendered ladder line for PHASE given the entry's ENTRY-PHASE."
  (let* ((label (alist-get phase agent-repl--open-progress-stages))
         (here (agent-repl--open-progress-stage-index phase))
         (now (agent-repl--open-progress-stage-index entry-phase))
         ;; A terminal phase has no ladder index; every stage before it
         ;; stays as it was, so an unresolved ladder reads as unresolved
         ;; rather than silently completing itself.
         (now (or now (agent-repl--open-progress-stage-index :dispatched))))
    (cond
     ((< here now) (concat (propertize "  ✓ " 'face 'agent-repl-open-progress-done)
                           (propertize label 'face 'agent-repl-open-progress-done)))
     ((= here now) (concat (propertize "  ▸ " 'face 'agent-repl-open-progress-active)
                           (propertize label 'face 'agent-repl-open-progress-active)))
     (t (concat "    " (propertize label 'face 'agent-repl-open-progress-pending))))))

(defun agent-repl--open-progress-alert-text (ws phase detail)
  "Return the alert block for WS's terminal PHASE with cause DETAIL."
  (let ((headline (if (eq phase :timed-out)
                      (format "Still opening %s after %ss." ws
                              (round agent-repl-open-progress-escalate-seconds))
                    (format "Opening %s FAILED." ws))))
    (concat
     (propertize headline 'face 'agent-repl-open-progress-alert)
     "\n\n"
     (if detail (concat "  " detail "\n\n") "")
     "  Try:\n"
     "    - wait: establishment legitimately takes up to 30s on a busy machine\n"
     "    - M-x agent-repl-frontend-daemon-restart  (bounce an unreachable daemon)\n"
     "    - press SPC o c again once this placeholder is gone\n")))

(defun agent-repl--open-progress-render (ws)
  "Redraw WS's placeholder buffer from its registry entry.
Returns the buffer, or nil when WS has no pending open or its buffer has
been killed out from under us."
  (when-let* ((entry (agent-repl--open-progress-entry ws))
              (buf (plist-get entry :buffer))
              (_ (buffer-live-p buf)))
    (let ((phase (plist-get entry :phase))
          (detail (plist-get entry :detail))
          (elapsed (- (float-time) (plist-get entry :started))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "\n")
          (insert (propertize (format "  Opening %s…\n\n" ws)
                              'face 'agent-repl-open-progress-heading))
          (if (memq phase agent-repl--open-progress-terminal-phases)
              (insert (agent-repl--open-progress-alert-text ws phase detail))
            (dolist (stage agent-repl--open-progress-stages)
              (insert (agent-repl--open-progress-stage-line (car stage) phase) "\n")))
          (insert (format "\n  %.1fs elapsed\n" elapsed))
          (goto-char (point-min))))
      buf)))

(defun agent-repl--open-progress-show (ws buf)
  "Display BUF as WS's placeholder in the window the webview will take.
Uses the webview's own host resolution so the placeholder occupies the
main area the mount is about to claim — the mount then REPLACES it in
place rather than shuffling the frame a second time."
  (let ((win (agent-repl--frontend-main-area-window)))
    (when (window-live-p win)
      (set-window-buffer win buf)
      (agent-repl--log ws "open-progress: shown window=%s buffer=%s" win (buffer-name buf)))
    win))

;;;; ---- Lifecycle --------------------------------------------------------

(defun agent-repl--open-progress-cancel-timer (entry)
  "Cancel ENTRY's escalation timer, if it still holds one."
  (when-let ((timer (plist-get entry :timer)))
    (when (timerp timer) (cancel-timer timer))))

(defun agent-repl--open-progress-start (ws)
  "Show WS's open placeholder NOW and return it, or nil when one stands.

Returning nil is the double-invoke answer: a caller that gets nil must
NOT dispatch an establishment, because the one already in flight owns
the placeholder this call just re-showed.

The buffer is displayed and redisplayed inside this call, so the
placeholder is on the frame before the caller's own command returns —
the whole point being that the keypress leaves a mark before anything
asynchronous is even attempted."
  (let ((standing (agent-repl--open-progress-entry ws)))
    (if standing
        (progn
          (agent-repl--log ws "open-progress: REFUSED second placeholder phase=%s"
                           (plist-get standing :phase))
          (when-let ((buf (plist-get standing :buffer)))
            (when (buffer-live-p buf) (agent-repl--open-progress-show ws buf)))
          nil)
      (let ((buf (get-buffer-create (agent-repl--open-progress-buffer-name ws))))
        (with-current-buffer buf
          (unless (derived-mode-p 'agent-repl-open-progress-mode)
            (agent-repl-open-progress-mode)))
        (puthash ws (list :buffer buf
                          :phase :dispatched
                          :detail nil
                          :started (float-time)
                          :timer (run-at-time agent-repl-open-progress-escalate-seconds
                                              nil
                                              #'agent-repl--open-progress-escalate ws))
                 agent-repl--open-progress)
        (agent-repl--open-progress-render ws)
        (agent-repl--open-progress-show ws buf)
        (agent-repl--log ws "open-progress: STARTED buffer=%s" (buffer-name buf))
        ;; Forced, because the caller has synchronous work left (xwidget
        ;; validation, the daemon ensure's first leg) before its command
        ;; returns and the command loop would redisplay on its own.
        (unless noninteractive (redisplay))
        buf))))

(defun agent-repl--open-progress-note (ws phase &optional detail)
  "Advance WS's placeholder to PHASE, carrying optional DETAIL.
A no-op when WS has no pending open (background opens raise no
placeholder and must stay silent), when the placeholder has already
resolved to a terminal phase, and when PHASE would move the ladder
BACKWARDS.  Returns the new phase, or nil when nothing moved."
  (when-let ((entry (agent-repl--open-progress-entry ws)))
    (let* ((current (plist-get entry :phase))
           (from (agent-repl--open-progress-stage-index current))
           (to (agent-repl--open-progress-stage-index phase)))
      (cond
       ((memq current agent-repl--open-progress-terminal-phases)
        (agent-repl--log-verbose ws "open-progress: note dropped phase=%s reason=already-terminal" phase)
        nil)
       ((null to)
        (agent-repl--log ws "open-progress: note REFUSED unknown phase=%s" phase)
        nil)
       ((and from (<= to from))
        (agent-repl--log-verbose ws "open-progress: note dropped phase=%s reason=not-an-advance current=%s"
                                 phase current)
        nil)
       (t
        (puthash ws (plist-put (plist-put entry :phase phase) :detail detail)
                 agent-repl--open-progress)
        (agent-repl--open-progress-render ws)
        (agent-repl--log ws "open-progress: phase %s -> %s" current phase)
        phase)))))

(defun agent-repl--open-progress-fail (ws detail)
  "Resolve WS's placeholder to a standing FAILURE naming DETAIL.
The placeholder stays on the frame: a failed open that erased its own
report would be indistinguishable from the success it is not.  Returns
the buffer, or nil when WS had no pending open."
  (when-let ((entry (agent-repl--open-progress-entry ws)))
    (agent-repl--open-progress-cancel-timer entry)
    (puthash ws (plist-put (plist-put (plist-put entry :phase :failed)
                                      :detail detail)
                           :timer nil)
             agent-repl--open-progress)
    (agent-repl--warn ws "open-progress: FAILED detail=%s" detail)
    (agent-repl--open-progress-render ws)))

(defun agent-repl--open-progress-escalate (ws)
  "Escalate WS's still-pending placeholder to a visible warning.
Armed by `agent-repl--open-progress-start' and cancelled by every
resolution, so it can only fire on an open that is genuinely still
waiting.  A placeholder already resolved is left exactly as it is."
  (when-let ((entry (agent-repl--open-progress-entry ws)))
    (let ((phase (plist-get entry :phase)))
      (if (memq phase agent-repl--open-progress-terminal-phases)
          (agent-repl--log-verbose ws "open-progress: escalation skipped phase=%s" phase)
        (puthash ws (plist-put
                     (plist-put
                      (plist-put entry :phase :timed-out)
                      :detail (format "no response past %s — last stage reached: %s"
                                      (alist-get phase agent-repl--open-progress-stages)
                                      phase))
                     :timer nil)
                 agent-repl--open-progress)
        (agent-repl--warn ws "open-progress: ESCALATED after %ss stage=%s"
                          agent-repl-open-progress-escalate-seconds phase)
        (agent-repl--open-progress-render ws)))))

(defun agent-repl--open-progress-finish (ws)
  "Tear down WS's placeholder because the view is really up.
Cancels the escalation timer, drops the registry entry, and kills the
buffer — the real panel is what the user should be looking at.  Returns
non-nil when a placeholder was actually torn down."
  (when-let ((entry (agent-repl--open-progress-entry ws)))
    (agent-repl--open-progress-cancel-timer entry)
    (remhash ws agent-repl--open-progress)
    (let ((buf (plist-get entry :buffer)))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (agent-repl--log ws "open-progress: FINISHED elapsed=%.3fs"
                     (- (float-time) (plist-get entry :started)))
    t))

(defun agent-repl--open-progress-abandon (ws)
  "Tear down WS's placeholder because WS itself is going away.

Subscriber for `agent-repl-ws-del-hook', which runs at the top of
`agent-repl--ws-del' — while WS is still registered, so this line still
routes to WS's own sink.

Without it, a workspace closed with an open still in flight left its
escalation timer armed.  The timer outlives the workspace, and when it
fires it calls `agent-repl--warn' against a name the registry no longer
resolves, which is how a closed workspace kept producing records that
`agent-repl--note-unroutable-log-workspace' then warned about.  The
emitter is what has to stop: the placeholder is a report on an open, and
a closed workspace has no open left to report on.

This is a teardown, not a resolution: unlike
`agent-repl--open-progress-fail' it leaves no standing buffer, because
there is no longer a workspace for the user to read a verdict about.
Returns non-nil when a placeholder was actually torn down."
  (when-let ((entry (agent-repl--open-progress-entry ws)))
    (agent-repl--open-progress-cancel-timer entry)
    (remhash ws agent-repl--open-progress)
    (let ((buf (plist-get entry :buffer)))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (agent-repl--log ws "open-progress: ABANDONED phase=%s reason=workspace-deleted"
                     (plist-get entry :phase))
    t))

;; Registered here though the hook lives in workspace.el, for the same
;; load-order reason the state-transition subscription below is.
(add-hook 'agent-repl-ws-del-hook #'agent-repl--open-progress-abandon)

;;;; ---- Pushed-state subscription ----------------------------------------

(defconst agent-repl--open-progress-broken-states
  '(:severed :dead :degraded)
  "Pushed render states that RESOLVE a pending open as failed.
Every one of them is the blue band's \"the local route to a session is
broken\" claim, which is a verdict on the very thing the placeholder is
waiting for — so the placeholder reports it instead of waiting out its
own deadline against a bring-up the daemon has already given up on.")

(defun agent-repl--open-progress-react-to-pushed-state (ws new _previous)
  "Advance WS's placeholder from a daemon-pushed render state NEW.
Subscriber for `agent-repl-ws-state-transition-functions'
\(frontend-state.el).  This is the PHASED half of the contract and it
polls nothing: the daemon pushes `WorkspaceState', frontend-state.el
applies it, and the placeholder moves on the same frame.

`:init' is the bring-up itself.  Any other live state means the session
is up, so the remaining wait is the transcript — read off the same
pushed `SessionView.backfill' the switch-ensure reads.  `:hibernated'
moves nothing: the workspace is asleep and the open is what will wake
it."
  (when (agent-repl--open-progress-active-p ws)
    (cond
     ((memq new agent-repl--open-progress-broken-states)
      (agent-repl--open-progress-fail
       ws (format "the daemon pushed %s for this workspace — its session route is broken"
                  new)))
     ((eq new :init)
      (agent-repl--open-progress-note ws :acked))
     ((eq new :hibernated) nil)
     (t
      (agent-repl--open-progress-note
       ws (if (agent-repl--frontend-backfill-settled-p
               (agent-repl--frontend-ws-command-key ws))
              :rendering
            :backfilling))))))

;; Registered here though the hook lives in frontend-state.el: `add-hook'
;; auto-vivifies the variable and that file's `defvar ... nil' does not reset
;; an already-bound one, so this survives either load order (the arrangement
;; sidebar.el and status.el already use).
(add-hook 'agent-repl-ws-state-transition-functions
          #'agent-repl--open-progress-react-to-pushed-state)

(provide 'agent-repl-open-progress)
;;; open-progress.el ends here
