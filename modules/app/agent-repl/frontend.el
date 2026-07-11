;;; frontend.el --- xwidget webview panel for the web frontend -*- lexical-binding: t; -*-

;;; Commentary:

;; Mounts the claude-repld webapp inside Emacs as an xwidget-webkit
;; buffer placed in the workspace's agent output window — the in-Emacs
;; browser rendering of the session that protocol.md's top diagram
;; calls the "Emacs (xwidget WS client)" layer.
;;
;; The command `agent-repl-frontend-open-panel' is the user entry point
;; AND the lazy initialization trigger: it ensures the daemon (built
;; if stale, launched if absent — daemon.el), ensures the workspace's
;; session (frontend-client.el), then shows the webview attached to
;; that session's URL.
;;
;; Buffer identity rules (hard-won from the panel machinery's naming
;; regexes in core.el):
;;   - Webview buffers use the `*agent-frontend-WS*' prefix, which
;;     deliberately does NOT match `agent-repl--vterm-buffer-re' /
;;     `--input-buffer-re': the bounce-from-vterm guard, the orphan
;;     sweep, and close-panels-on-open must all treat the webview as an
;;     ordinary buffer, not an agent panel.
;;   - xwidget-webkit renames its buffer on every document-title change
;;     (the webapp sets document.title per model); the buffer-local
;;     `xwidget-webkit-buffer-name-format' is pinned to the fixed name
;;     so the identity never drifts.
;;
;; The WKWebView is external state: creation funnels through the
;; boundary wrapper `agent-repl--frontend-make-webview-buffer',
;; registered in `agent-repl--external-boundary-functions'; batch tests
;; mock it (xwidgets do not exist in `emacs -batch' builds anyway).

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key val))
(declare-function agent-repl--frontend-ensure-session "agent-repl-frontend-client" (ws))
(declare-function agent-repl--frontend-session-url "agent-repl-frontend-client" (session-id))
(declare-function agent-repl-window--panel-window "agent-repl-window" (kind &optional ws frame))
(declare-function agent-repl-window--side-window-p "agent-repl-window" (win))
(declare-function agent-repl-window--harden "agent-repl-window" (win &rest recipe))
(declare-function agent-repl--panels-visible-p "agent-repl-panels" ())
(declare-function agent-repl--hide-panels "agent-repl-panels" ())
(declare-function agent-repl--ensure-input-buffer "agent-repl-panels" (ws))
(declare-function agent-repl--close-buffer-windows "agent-repl-panels" (&rest bufs))
(declare-function agent-repl--ws-backend-name "agent-repl-backend" (ws))
(declare-function agent-repl--frontend-validate-pair "agent-repl-frontends" (frontend-name backend-name))
(declare-function agent-repl-register-frontend "agent-repl-frontends" (frontend))
(declare-function agent-repl-frontend-create "agent-repl-frontends")
(declare-function agent-repl--gui-send-turn "agent-repl-frontend-client" (ws input raw &optional on-settle))
(declare-function agent-repl--gui-interrupt "agent-repl-frontend-client" (ws kind))
(declare-function agent-repl--gui-running-p "agent-repl-frontend-client" (ws))
(declare-function agent-repl--gui-durable-session-id "agent-repl-frontend-client" (ws))
(declare-function agent-repl--gui-adopt-session "agent-repl-frontend-client" (ws claude-session-id))
(defvar agent-repl-input-height-fraction)
(declare-function xwidget-webkit--create-new-session-buffer "xwidget" (url &optional callback))
(declare-function xwidget-webkit-current-session "xwidget" ())
(declare-function xwidget-webkit-goto-uri "xwidget.c" (xwidget uri))

(defvar xwidget-webkit-buffer-name-format)

;;;; ---- Customization ------------------------------------------------------

(defcustom agent-repl-frontend-buffer-name-format "*agent-frontend-%s*"
  "Format for webview buffer names; %s is the workspace name.
Must NOT collide with `agent-repl-panel-buffer-name-format' — the
panel regexes in core.el key real behavior (bounce, orphan sweep) off
that namespace and the webview must stay outside it."
  :type 'string
  :group 'agent-repl)

;;;; ---- Capability -----------------------------------------------------------

(defun agent-repl--frontend-xwidget-available-p ()
  "Return non-nil when this Emacs can host WKWebView xwidgets.
`xwidget-internal' (the C feature) proves build support; the lisp-side
creator is NOT autoloaded, so xwidget.el must be required BEFORE the
`fboundp' probe — checking first false-negatives on every xwidget
build that has not happened to load xwidget.el yet."
  (and (featurep 'xwidget-internal)
       (require 'xwidget nil t)
       (fboundp 'xwidget-webkit--create-new-session-buffer)))

;;;; ---- Webview buffer lifecycle ---------------------------------------------

(defun agent-repl--frontend-webview-buffer-name (ws)
  "Return the pinned webview buffer name for workspace WS."
  (format agent-repl-frontend-buffer-name-format ws))

(defun agent-repl--frontend-make-webview-buffer (url)
  "External-boundary wrapper: create a WKWebView xwidget buffer on URL.
The creator only seeds the buffer — it does NOT navigate (upstream
callers like `xwidget-webkit-new-session' always follow with
`xwidget-webkit-goto-uri', and skipping it shows a blank about:blank
webview).  Body does nothing but the external calls; tests mock via
`cl-letf'.  Registered in `agent-repl--external-boundary-functions'."
  (require 'xwidget)
  (let ((buf (xwidget-webkit--create-new-session-buffer url))) ;; ALLOW-EXTERNAL-BOUNDARY
    (with-current-buffer buf
      (xwidget-webkit-goto-uri (xwidget-webkit-current-session) url))
    buf))

(defun agent-repl--frontend-kill-webview (buf)
  "Kill webview BUF without the xwidget kill-query prompt.
`xwidget-kill-buffer-query-function' (on `kill-buffer-query-functions')
raises a blocking yes-or-no minibuffer prompt for any buffer holding
xwidgets; every frontend kill site is an INTENTIONAL teardown (rebind,
close-panel, workspace nuke), so the prompt is suppressed — left in
place it deadlocks non-interactive callers like the nuke hook."
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buf)))

(defun agent-repl--frontend-ensure-webview-buffer (ws session-id url)
  "Return a live webview buffer for WS attached to SESSION-ID at URL.
Reuses the recorded `:frontend-buffer' only while it is live AND still
bound to SESSION-ID (`:frontend-buffer-session-id'); a session change
kills the stale webview and mounts a fresh one, since an xwidget
session cannot be retargeted reliably from outside.  The new buffer's
name is pinned via buffer-local `xwidget-webkit-buffer-name-format' so
webapp title changes never rename it."
  (let ((existing (agent-repl--ws-get ws :frontend-buffer))
        (bound-to (agent-repl--ws-get ws :frontend-buffer-session-id)))
    (if (and (buffer-live-p existing) (equal bound-to session-id))
        existing
      (when (buffer-live-p existing)
        (agent-repl--log ws "frontend webview rebind: session %s -> %s (killing stale webview)"
                          bound-to session-id)
        (agent-repl--frontend-kill-webview existing))
      (let ((buf (agent-repl--frontend-make-webview-buffer url))
            (name (agent-repl--frontend-webview-buffer-name ws)))
        (with-current-buffer buf
          ;; Pin BOTH the live name and the format xwidget uses on
          ;; title-change renames; the format is the fixed name itself
          ;; (no %-constructs), so every "rename" is a no-op.
          (setq-local xwidget-webkit-buffer-name-format name)
          (rename-buffer name t))
        (agent-repl--ws-put ws :frontend-buffer buf)
        (agent-repl--ws-put ws :frontend-buffer-session-id session-id)
        (agent-repl--log ws "frontend webview mounted: %s -> %s" name url)
        buf))))

;;;; ---- Placement ---------------------------------------------------------------

(defun agent-repl--frontend-main-area-window ()
  "Return a live main-area window able to host the webview.
`window-main-window' can return an INTERNAL window when the main area
is split, and `select-window' on an internal window errors — so walk
the frame's live windows and take the first that is neither a side
window nor DEDICATED (a dedicated window rejects `set-window-buffer',
and the workspace's own hardened input panel is exactly such a window).
Falls back to the selected window (always live) when nothing matches."
  (or (seq-find (lambda (win)
                  (and (not (agent-repl-window--side-window-p win))
                       (not (window-dedicated-p win))))
                (window-list nil 'no-minibuffer))
      (selected-window)))

(defun agent-repl--frontend-display-webview (ws buf)
  "Display BUF as the workspace's frontend view in the frame's main area.
When the vterm/input panels are visible they are HIDDEN first through
the module's own path (`agent-repl--hide-panels') rather than swapped
under: replacing the buffer of the strongly-dedicated output window
would (a) leave the input panel orphaned for the sync-panels sweep to
reap and (b) break the next `agent-repl--show-panels' against the
still-dedicated window.  The webview then takes a live main-area
window like an ordinary buffer display."
  (when (agent-repl--panels-visible-p)
    (agent-repl--log ws "display-webview: hiding agent panels first")
    (agent-repl--hide-panels))
  (let* ((input-buf (agent-repl--ensure-input-buffer ws))
         (stale-input-win (get-buffer-window input-buf)))
    ;; A surviving input window from a previous webview mount (the
    ;; webview died or was rebound) is dedicated, so it can neither
    ;; host the webview nor be left to shadow the host search — remove
    ;; it and rebuild the canonical layout from scratch. When it is
    ;; the frame's ONLY window it cannot be deleted; reclaim it as the
    ;; host by lifting its dedication instead.
    (when (window-live-p stale-input-win)
      (if (one-window-p)
          (set-window-dedicated-p stale-input-win nil)
        (delete-window stale-input-win)))
    (let ((win (agent-repl--frontend-main-area-window)))
      (select-window win)
      (set-window-buffer win buf)
      ;; Hybrid UI: the classic input panel sits below the webview, with
      ;; the same window recipe the vterm layout uses (dedicated,
      ;; height-locked, delete-protected, mini-window-shrink-proof).
      ;; Focus lands there — typing is the whole point of the panel.
      (let ((input-win (split-window
                        win
                        (round (* (- agent-repl-input-height-fraction)
                                  (window-total-height win)))
                        'below)))
        (set-window-buffer input-win input-buf)
        (agent-repl-window--harden input-win
                                   :dedicate       t
                                   :size-fix       'height
                                   :delete-protect t
                                   :preserve-size  'height)
        (select-window input-win))))
  buf)

;;;; ---- Entry point ----------------------------------------------------------------

(defun agent-repl--gui-open (ws)
  "The gui frontend's open capability (registry `:open-fn').
The lazy end-to-end trigger: validates the backend pair and xwidget
capability, ensures the daemon (built if stale, launched if absent),
ensures WS's daemon session (rooted at its worktree), mounts the
webview attached to that session, and places it over the input panel."
  (unless (agent-repl--frontend-xwidget-available-p)
    (user-error "agent-repl: this Emacs build lacks xwidget-webkit support"))
  (agent-repl--frontend-validate-pair 'gui (agent-repl--ws-backend-name ws))
  (let* ((session-id (agent-repl--frontend-ensure-session ws))
         ;; composer=0: Emacs owns input (the panel below), so the
         ;; webview hides its own composer and stays output-only.
         (url (concat (agent-repl--frontend-session-url session-id)
                      "&composer=0"))
         (buf (agent-repl--frontend-ensure-webview-buffer ws session-id url)))
    (agent-repl--frontend-display-webview ws buf)))

(defun agent-repl--gui-show (ws)
  "The gui frontend's show capability (registry `:show-fn').
Remounts the live webview (or opens fresh when it died)."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (if (buffer-live-p buf)
        (agent-repl--frontend-display-webview ws buf)
      (agent-repl--gui-open ws))))

(defun agent-repl--gui-hide (ws)
  "The gui frontend's hide capability (registry `:hide-fn').
Closes the webview and input windows; buffers and session survive."
  (agent-repl--close-buffer-windows
   (agent-repl--ws-get ws :frontend-buffer)
   (agent-repl--ws-get ws :input-buffer)))

(defun agent-repl--gui-kill (ws)
  "The gui frontend's kill capability (registry `:kill-fn').
Tears down the LAYOUT first (webview + dedicated input windows), then
deletes the daemon session (best-effort) and kills the webview.  The
window teardown is contractual: `agent-repl-switch-frontend' opens the
next frontend immediately after kill, and a leftover dedicated input
window aborts the vterm launch mid-initialize (the observed
\"vterm buffer is null/dead\" cascade).  The input buffer itself
survives — it is workspace furniture, not session state."
  (agent-repl--gui-hide ws)
  (agent-repl--frontend-release-workspace-session ws)
  (agent-repl--frontend-release-workspace-webview ws)
  (agent-repl--ws-put ws :frontend-buffer nil)
  (agent-repl--ws-put ws :frontend-buffer-session-id nil))

(agent-repl-register-frontend
 (agent-repl-frontend-create
  :name 'gui
  :open-fn #'agent-repl--gui-open
  :kill-fn #'agent-repl--gui-kill
  :send-fn #'agent-repl--gui-send-turn
  :interrupt-fn #'agent-repl--gui-interrupt
  :running-p-fn #'agent-repl--gui-running-p
  :show-fn #'agent-repl--gui-show
  :hide-fn #'agent-repl--gui-hide
  :restart-fn (lambda (ws)
                (agent-repl--gui-kill ws)
                (agent-repl--gui-open ws))
  ;; The gui drives sessions through the claude Agent SDK; a codex
  ;; shim does not exist (yet), so the pair validation fails loudly.
  :supported-backends '(claude)
  :durable-session-id-fn #'agent-repl--gui-durable-session-id
  :adopt-session-fn #'agent-repl--gui-adopt-session))

;;;###autoload
(defun agent-repl-frontend-open-panel ()
  "Open the web frontend for the current workspace's session.
Stamps the workspace's `:frontend' choice to `gui' (the unified
command surface — `SPC o c' and friends — then routes here through the
frontend registry) and dispatches the gui open capability."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws
      (user-error "agent-repl: no current workspace"))
    (agent-repl--frontend-validate-pair 'gui (agent-repl--ws-backend-name ws))
    (agent-repl--ws-put ws :frontend 'gui)
    (agent-repl--gui-open ws)))

;;;###autoload
(defun agent-repl-frontend-close-panel ()
  "Kill the current workspace's webview buffer (the session stays alive).
The daemon session is NOT deleted — reopening the panel reattaches to
it with full replayed history; session teardown belongs to the
workspace nuke path (`agent-repl-ws-del-hook')."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (buf (agent-repl--ws-get ws :frontend-buffer)))
    (unless (buffer-live-p buf)
      (user-error "agent-repl: no webview open for workspace %s" ws))
    (agent-repl--frontend-kill-webview buf)
    (agent-repl--ws-put ws :frontend-buffer nil)
    (agent-repl--ws-put ws :frontend-buffer-session-id nil)
    (message "agent-repl: webview closed (session kept)")))

;;;; ---- Workspace teardown -----------------------------------------------------

(defun agent-repl--frontend-release-workspace-webview (ws)
  "Kill WS's webview buffer on nuke (for `agent-repl-ws-del-hook').
Tombstoning only nils the plist keys — without this the buffer (a live
WKWebView holding an open WebSocket) would outlive the workspace.
Runs pre-tombstone, while `:frontend-buffer' is still readable."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (when (buffer-live-p buf)
      (agent-repl--log ws "frontend webview released on nuke: %s" (buffer-name buf))
      (agent-repl--frontend-kill-webview buf))))

(add-hook 'agent-repl-ws-del-hook #'agent-repl--frontend-release-workspace-webview)

(provide 'frontend)

;;; frontend.el ends here
