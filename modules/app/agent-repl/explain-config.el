;;; explain-config.el --- conversational read-only Q&A about this doom config -*- lexical-binding: t; -*-

;;; Commentary:

;; `SPC j h c' — ask Claude about this doom config (with particular
;; emphasis on `modules/app/agent-repl/') and get the answer rendered in
;; the SAME webkit GUI the workspace frontend uses (frontend.el), not in
;; a bespoke Emacs buffer.
;;
;; Two properties define this entry point:
;;
;;   - CONVERSATIONAL.  The daemon session is long-lived and global, so
;;     follow-up questions land in the same conversation with full
;;     context.  Ask them either by re-running `SPC j h c' (the
;;     minibuffer route) or by typing straight into the webapp's own
;;     composer (the reason the webview is mounted WITHOUT the
;;     `&composer=0' that the workspace frontend passes — Emacs owns
;;     input there, nobody owns it here).
;;
;;   - WORKSPACE-LESS.  There is no perspective, no worktree, no input
;;     panel, no `agent-repl--ws-*' plist behind it.  The session id and
;;     the webview live in module-level state, and the session is rooted
;;     at `agent-repl-explain-config-dir' (the canonical doom checkout,
;;     NOT the current worktree — the explainer must see the config the
;;     user actually runs).
;;
;; The read-only contract lives in `agent-repl-explain-config-preamble',
;; which primes the session on its FIRST turn only; every later turn in
;; that session (from either input route) inherits the contract from
;; conversation context rather than re-sending it.
;;
;; Everything below the wire is reuse: the daemon (daemon.el), the
;; session CRUD + message injection (frontend-client.el), and the
;; webview boundary wrapper (frontend.el).  This module owns only the
;; global session/webview binding and the popup's window placement.
;;
;; The SESSION is global (one conversation shared everywhere), but the
;; popup WINDOW is strictly per-workspace: `SPC j h c' shows it in the
;; invoking workspace only, a workspace switch never re-creates it
;; elsewhere, and quitting the window (via `q', window deletion, or the
;; close command) is permanent until the next explicit show.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ensure-frontend-daemon "agent-repl-daemon" (&optional force))
(declare-function agent-repl--frontend-wait-ready "agent-repl-frontend-client" ())
(declare-function agent-repl--frontend-create-session "agent-repl-frontend-client" (cwd &optional model resume))
(declare-function agent-repl--frontend-delete-session "agent-repl-frontend-client" (id))
(declare-function agent-repl--frontend-session-live-p "agent-repl-frontend-client" (id))
(declare-function agent-repl--frontend-session-url "agent-repl-frontend-client" (session-id))
(declare-function agent-repl--frontend-send-message "agent-repl-frontend-client" (session-id text))
(declare-function agent-repl--frontend-make-webview-buffer "agent-repl-frontend" (url))
(declare-function agent-repl--frontend-adopt-webview-buffer "agent-repl-frontend" (buf name))
(declare-function agent-repl--frontend-kill-webview "agent-repl-frontend" (buf))
(declare-function agent-repl--frontend-require-xwidget "agent-repl-frontend" ())
(declare-function agent-repl-window--panel-window "agent-repl-window" (kind &optional ws frame))
(declare-function agent-repl-window--delete-buffer-windows "agent-repl-window" (buf &rest args))
(declare-function agent-repl-window--harden "agent-repl-window" (win &rest recipe))

(defvar agent-repl-frontend-permission-mode)
(defvar xwidget-webkit-buffer-name-format)

;;;; ---- Customization -------------------------------------------------------

(defcustom agent-repl-explain-config-dir "~/.config/doom"
  "Working directory of the explain-config daemon session.
Resolves to the canonical doom config checkout (NOT the current
worktree) so the explainer sees the user's installed configuration."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-explain-config-model "haiku"
  "Model alias pinned for the explain-config session.
`explain-config' is short-form Q&A, so the small/fast model is used
rather than the default-tier model."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-explain-config-permission-mode "bypassPermissions"
  "Permission mode for the explain-config daemon session (POST /sessions).
`bypassPermissions' is the daemon-side spelling of the headless
`--dangerously-skip-permissions' this entry point has always run with:
the session is read-only by CONTRACT (see
`agent-repl-explain-config-preamble'), and every tool it legitimately
needs is a read.  Prompting for each of those would make a Q&A popup
unusable."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-explain-config-buffer-name "*agent-explain-config*"
  "Pinned name of the explain-config webview buffer.
Deliberately outside the `agent-repl-panel-buffer-name-format'
namespace (like the workspace webview) so the panel regexes in core.el
never mistake the popup for an agent panel."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-explain-config-width-fraction 0.5
  "Fraction of frame width for the explain-config right-side popup.
Only applies when the popup falls back to its own side window (i.e.
when the agent output window is not visible to take over).  Width
is inherited from the agent output window when the popup takes
that window over — see `agent-repl--explain-config-show'.  The
drawer is untouched in either branch."
  :type 'float
  :group 'agent-repl)

(defcustom agent-repl-explain-config-preamble
  (concat
   "You are being asked questions about the Doom Emacs configuration"
   " in this repository (~/.config/doom), with particular emphasis on"
   " `modules/app/agent-repl/' (the Agent REPL integration).  The"
   " user wants an EXPLANATION or CLARIFICATION of how the config or"
   " its capabilities work.  This is NOT a call to action.\n"
   "\n"
   "This is a CONVERSATION: the user will ask follow-up questions in"
   " this same session.  Every constraint below binds for the whole"
   " conversation, not just the first question.\n"
   "\n"
   "STRICT CONSTRAINT -- READ-ONLY: You MUST NOT take any mutating"
   " action of any kind.  Do NOT edit files, do NOT run shell commands"
   " that change state, do NOT perform git operations, do NOT install"
   " or uninstall anything, do NOT rebuild, do NOT restart any process,"
   " do NOT send messages, and do NOT alter the system in any way."
   " Read-only tools (reading files, grepping, listing files, code"
   " search) are fine and encouraged for grounding your answer."
   " Anything write-side is FORBIDDEN.\n"
   "\n"
   "If a question appears to be a disguised request to"
   " make changes (e.g. \"fix\", \"add\", \"refactor\", \"change\","
   " \"implement\", \"create\", \"update\", \"delete\", \"rename\","
   " or any other imperative implying side effects on the repo or"
   " system), REFUSE to act and respond by explaining that this entry"
   " point is for clarification and explanation only, and that the"
   " user should re-issue the request through the appropriate Claude"
   " REPL workspace command if they want changes made.\n"
   "\n"
   "OUTPUT FORMAT: your answer is rendered as Markdown in a webview, so"
   " ordinary Markdown (headings, bold, fenced code blocks, bullets,"
   " links) is exactly right.  Keep answers concise, accurate, and"
   " grounded in the actual code.\n"
   "\n"
   "QUESTION:\n"
   "%s")
  "Format string priming the explain-config session on its FIRST turn.
`%s' is replaced with the raw question.  The preamble is the read-only
contract for this entry point -- edit with care.  Follow-up turns in
the same session are sent verbatim: the contract is already in the
conversation's context, and re-sending it every turn would both waste
tokens and read as nagging."
  :type 'string
  :group 'agent-repl)

;;;; ---- Module state --------------------------------------------------------

(defvar agent-repl--explain-config-session-id nil
  "Daemon session id backing the explain-config conversation, or nil.
Global, not per-workspace: this entry point has no workspace context.
Like every daemon `s_<hex>' id it dies with the daemon process, so it
is never persisted.")

(defvar agent-repl--explain-config-webview-session-id nil
  "Session id the live explain-config webview is attached to, or nil.
An xwidget session cannot be retargeted reliably from outside, so a
session change forces a fresh webview mount.")

(defvar agent-repl--explain-config-primed-p nil
  "Non-nil once the read-only preamble has been sent into the live session.
Reset whenever a new session is created, so the first turn of every
conversation carries the contract and no later turn repeats it.")

(defvar agent-repl--explain-config-replaced-window nil
  "When the popup has taken over the agent output window, holds (WIN . PREV-BUF).
WIN is the live agent output window the popup took over; PREV-BUF
is the buffer that window was displaying before takeover (the
agent output buffer for the current workspace).  Nil when the
popup is hosted in its own side window (i.e. the agent output
window was not visible at show time).  Consumed by
`agent-repl--explain-config-hide' to restore the prior buffer in
the same window position when the popup closes.")

;;;; ---- Popup placement -----------------------------------------------------

(defun agent-repl--explain-config-window-width (window)
  "Return the configured explain-config width in columns for WINDOW.
Resolves `agent-repl-explain-config-width-fraction' against the
host frame's width."
  (let ((frame-cols (frame-width (window-frame window))))
    (max 1 (round (* agent-repl-explain-config-width-fraction frame-cols)))))

(defvar agent-repl--explain-config-display-action
  `((display-buffer-in-side-window)
    (side . right)
    (slot . 0)
    (window-width . ,#'agent-repl--explain-config-window-width)
    (window-parameters
     (no-delete-other-windows . t)
     (no-other-window . nil)))
  "Fallback display action for the explain-config webview buffer.
Used only when the agent output window is not visible to take
over — when it is, `--show' reuses it directly via
`set-window-buffer' and bypasses `display-buffer' entirely.  The
drawer is never touched.")

(defun agent-repl--explain-config-apply-width (window)
  "Resize WINDOW to the configured explain-config width.
Side-window action alists honor `window-width' only at window-creation
time, so re-displaying the popup keeps its old width if the fraction
changed.  This forces the resize on every show — mirrors the drawer's
`--apply-width'."
  (let* ((target (agent-repl--explain-config-window-width window))
         (window-min-width 1))
    (with-selected-window window
      (setq-local window-size-fixed nil)
      (let ((delta (- target (window-total-width window))))
        (cond
         ((> delta 0) (enlarge-window delta t))
         ((< delta 0) (shrink-window (abs delta) t)))))))

(defun agent-repl--explain-config-current-agent-output-window ()
  "Return the live agent output window in the selected frame, or nil.
Looks up the current workspace's agent output panel via
`agent-repl-window--panel-window' with the `:view' key (the
existing panel-lookup key — note we do NOT introduce that name
here, the popup itself only deals in \"agent output\").  Guards
on `fboundp' so callers in load order before window.el (e.g. early
test harnesses) get nil instead of a void-function error."
  (and (fboundp 'agent-repl-window--panel-window)
       (agent-repl-window--panel-window :view)))

(defun agent-repl--explain-config-take-over-agent-output-window (output-win buf)
  "Swap OUTPUT-WIN's buffer for BUF and record the original for restoration.
The agent output panel is a dedicated window, so this temporarily
clears `window-dedicated-p' before `set-window-buffer' — otherwise
the swap errors.  The pre-swap buffer is stashed in
`agent-repl--explain-config-replaced-window' so
`agent-repl--explain-config-hide' can restore it.  Returns OUTPUT-WIN."
  (let ((prev-buf (window-buffer output-win)))
    (set-window-dedicated-p output-win nil)
    (set-window-buffer output-win buf)
    (setq agent-repl--explain-config-replaced-window
          (cons output-win prev-buf)))
  output-win)

(defun agent-repl--explain-config-restore-replaced-window ()
  "Restore the buffer in the window the popup took over, if any.
No-op when no window was replaced or when the window or its prior
buffer is no longer live.  Re-applies the agent output window's
hardening recipe (dedicate / width-fixed / delete-protect — the same
recipe the agent output window is always given, whether it hosts the
gui webview or, transiently, this popup) on success so the restored
window matches its original recipe."
  (when-let ((cell agent-repl--explain-config-replaced-window))
    (setq agent-repl--explain-config-replaced-window nil)
    (let ((win (car cell))
          (prev (cdr cell)))
      (when (and (window-live-p win) (buffer-live-p prev))
        (set-window-buffer win prev)
        (agent-repl-window--harden win
                                   :dedicate t
                                   :size-fix 'width
                                   :delete-protect t)))))

(defun agent-repl--explain-config-show ()
  "Display the explain-config webview in the current workspace.
The popup is strictly per-workspace: nothing re-displays it in other
workspaces on a switch, and deleting its window (by `q', `C-x 0', the
close command, or any other route) is permanent until the next
explicit show.  No-op when the webview doesn't exist (nothing to
show yet).

Display priority:

  1. If a window already displays the webview, leave it in place
     (and re-apply the side-window width unless it is the stolen
     agent output window — stolen windows inherit the prior
     window's width).
  2. Otherwise, if the agent output window is visible, take it
     over via `set-window-buffer' and record the prior buffer so
     `--hide' can restore it.
  3. Otherwise, fall back to the right-side popup display action.

The drawer is never touched in any branch — its visibility is its
own concern.  Returns the displayed window or nil."
  (when-let ((buf (get-buffer agent-repl-explain-config-buffer-name)))
    (let ((existing (get-buffer-window buf t)))
      (cond
       ((window-live-p existing)
        (unless (and agent-repl--explain-config-replaced-window
                     (eq existing (car agent-repl--explain-config-replaced-window)))
          (agent-repl--explain-config-apply-width existing))
        existing)
       ((agent-repl--explain-config-current-agent-output-window)
        (agent-repl--explain-config-take-over-agent-output-window
         (agent-repl--explain-config-current-agent-output-window) buf))
       (t
        (let ((win (display-buffer buf agent-repl--explain-config-display-action)))
          (when (window-live-p win)
            (agent-repl--explain-config-apply-width win))
          win))))))

(defun agent-repl--explain-config-hide ()
  "Hide the explain-config webview in the current workspace.
Keeps the webview (and therefore the daemon session and the whole
conversation) alive — only its visibility is toggled.  Hiding is
permanent: nothing re-displays the popup on a later workspace
switch.

If `--show' took over the agent output window, restores the prior
buffer in that window via `--restore-replaced-window'.  Any
remaining windows still displaying the explain-config webview (e.g.
side-window fallbacks) are deleted.  The drawer is never touched."
  (agent-repl--explain-config-restore-replaced-window)
  (when-let ((buf (get-buffer agent-repl-explain-config-buffer-name)))
    (agent-repl-window--delete-buffer-windows buf)))

;;;; ---- Session ---------------------------------------------------------------

(defun agent-repl--explain-config-ensure-session ()
  "Return the live explain-config daemon session id, creating it if needed.
Ensures the daemon itself (built if stale, launched if absent) and
waits for it to answer, then reuses the recorded session while the
daemon still lists it as live.  A newly created session is rooted at
`agent-repl-explain-config-dir', pinned to
`agent-repl-explain-config-model', and created under
`agent-repl-explain-config-permission-mode' — and clears the primed
flag, so its first turn carries the read-only preamble."
  (unless (agent-repl--ensure-frontend-daemon)
    (error "agent-repl: frontend daemon not started (auto-start disabled or init inhibited)"))
  (agent-repl--frontend-wait-ready)
  (if (and agent-repl--explain-config-session-id
           (agent-repl--frontend-session-live-p agent-repl--explain-config-session-id))
      agent-repl--explain-config-session-id
    (let* ((dir (file-name-as-directory
                 (expand-file-name agent-repl-explain-config-dir)))
           (agent-repl-frontend-permission-mode
            agent-repl-explain-config-permission-mode)
           (id (agent-repl--frontend-create-session
                dir agent-repl-explain-config-model)))
      (setq agent-repl--explain-config-session-id id
            agent-repl--explain-config-primed-p nil)
      (agent-repl--log nil "explain-config: session created %s (cwd=%s model=%s)"
                       id dir agent-repl-explain-config-model)
      id)))

(defun agent-repl--explain-config-release-session ()
  "Best-effort DELETE of the explain-config daemon session; clear the binding.
Errors are LOGGED, never signalled: a reset must not abort because the
daemon is already gone — but nothing is silently dropped, the failure
lands in the agent-repl log."
  (when agent-repl--explain-config-session-id
    (condition-case err
        (progn
          (agent-repl--frontend-delete-session agent-repl--explain-config-session-id)
          (agent-repl--log nil "explain-config: session released %s"
                           agent-repl--explain-config-session-id))
      (error
       (agent-repl--log nil "explain-config: session release FAILED for %s: %s"
                        agent-repl--explain-config-session-id
                        (error-message-string err)))))
  (setq agent-repl--explain-config-session-id nil
        agent-repl--explain-config-primed-p nil))

;;;; ---- Webview ---------------------------------------------------------------

(defun agent-repl--explain-config-webview-url (session-id)
  "Return the webapp URL the explain-config popup mounts for SESSION-ID.
Unlike the workspace frontend this does NOT pass `&composer=0': there
is no Emacs input panel here, so the webapp's own composer IS the
follow-up-question surface."
  (agent-repl--frontend-session-url session-id))

(defun agent-repl--explain-config-ensure-webview (session-id)
  "Return a live explain-config webview buffer attached to SESSION-ID.
Reuses the existing webview only while it is live AND still bound to
SESSION-ID; a session change kills the stale webview and mounts a
fresh one, since an xwidget session cannot be retargeted reliably from
outside.  The fresh buffer is handed to the shared
`agent-repl--frontend-adopt-webview-buffer' (frontend.el), which pins
its name, drops the browser header-line, and arms the copy chords — the
popup is chrome, not a browser."
  (agent-repl--frontend-require-xwidget)
  (let ((existing (get-buffer agent-repl-explain-config-buffer-name)))
    (if (and (buffer-live-p existing)
             (equal agent-repl--explain-config-webview-session-id session-id))
        existing
      (when (buffer-live-p existing)
        (agent-repl--log nil "explain-config webview rebind: session %s -> %s (killing stale webview)"
                         agent-repl--explain-config-webview-session-id session-id)
        (agent-repl--frontend-kill-webview existing))
      (let* ((buf (agent-repl--frontend-make-webview-buffer
                   (agent-repl--explain-config-webview-url session-id)))
             (name agent-repl-explain-config-buffer-name))
        (agent-repl--frontend-adopt-webview-buffer buf name)
        (setq agent-repl--explain-config-webview-session-id session-id)
        (agent-repl--log nil "explain-config webview mounted: %s -> %s"
                         name (agent-repl--explain-config-webview-url session-id))
        buf))))

(defun agent-repl--explain-config-release-webview ()
  "Kill the explain-config webview buffer and clear its session binding."
  (when-let ((buf (get-buffer agent-repl-explain-config-buffer-name)))
    (when (buffer-live-p buf)
      (agent-repl--frontend-kill-webview buf)))
  (setq agent-repl--explain-config-webview-session-id nil))

;;;; ---- Turns -------------------------------------------------------------------

(defun agent-repl--explain-config-build-input (raw)
  "Wrap RAW with the explain-config read-only preamble."
  (format agent-repl-explain-config-preamble raw))

(defun agent-repl--explain-config-send (session-id question)
  "Send QUESTION into SESSION-ID as a user turn.
The FIRST turn of a session is wrapped in the read-only preamble;
every follow-up is sent verbatim, inheriting the contract from the
conversation's context."
  (let ((text (if agent-repl--explain-config-primed-p
                  question
                (agent-repl--explain-config-build-input question))))
    (agent-repl--log nil "explain-config: send session=%s primed=%s len=%d"
                     session-id agent-repl--explain-config-primed-p (length text))
    (agent-repl--frontend-send-message session-id text)
    (setq agent-repl--explain-config-primed-p t)))

;;;; ---- Commands ------------------------------------------------------------------

(defconst agent-repl--explain-config-orange "#FF8C42"
  "Claude-orange accent for the explain-config minibuffer prompt.")

;;;###autoload
(defun agent-repl-explain-config (prompt &optional new-conversation)
  "Ask Claude to explain something about this doom config.
The answer renders in the webkit GUI popup, attached to a long-lived,
workspace-less daemon session rooted at
`agent-repl-explain-config-dir'.  Re-running this command sends PROMPT
as a FOLLOW-UP into the same conversation; the webapp's own composer
does the same thing without leaving the webview.

With a prefix argument (NEW-CONVERSATION), the current conversation is
discarded first and PROMPT opens a fresh one.

The first turn of every conversation is wrapped in a read-only
preamble forbidding any mutating action — this entry point is for
clarification and explanation only."
  (interactive
   (list (read-string (propertize "🤖 Explain config: "
                                  'face `(:foreground ,agent-repl--explain-config-orange
                                          :weight bold)))
         current-prefix-arg))
  (let ((trimmed (string-trim (or prompt ""))))
    (when (string-empty-p trimmed)
      (user-error "Empty prompt"))
    (when new-conversation
      (agent-repl-explain-config-reset))
    (let* ((session-id (agent-repl--explain-config-ensure-session))
           (buf (agent-repl--explain-config-ensure-webview session-id)))
      (agent-repl--explain-config-show)
      (agent-repl--explain-config-send session-id trimmed)
      buf)))

;;;###autoload
(defun agent-repl-explain-config-close ()
  "Close the explain-config popup in the current workspace.
Deletes every visible explain-config window in the current frame; the
close is permanent — nothing re-displays the popup on a workspace
switch.  The conversation is preserved — re-running
`agent-repl-explain-config' shows the same webview again with the full
history and the new question appended."
  (interactive)
  (agent-repl--explain-config-hide))

;;;###autoload
(defun agent-repl-explain-config-reset ()
  "Discard the explain-config conversation and start fresh.
Hides the popup, kills the webview, and deletes the daemon session, so
the next `agent-repl-explain-config' opens a brand-new conversation
whose first turn re-primes the read-only contract."
  (interactive)
  (agent-repl--explain-config-hide)
  (agent-repl--explain-config-release-webview)
  (agent-repl--explain-config-release-session))

(provide 'explain-config)

;;; explain-config.el ends here
