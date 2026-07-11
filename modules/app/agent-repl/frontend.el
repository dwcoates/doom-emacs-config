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
(declare-function agent-repl--panels-visible-p "agent-repl-panels" ())
(declare-function agent-repl--hide-panels "agent-repl-panels" ())
(declare-function xwidget-webkit--create-new-session-buffer "xwidget" (url))

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
  "Return non-nil when this Emacs can host WKWebView xwidgets."
  (and (featurep 'xwidget-internal)
       (fboundp 'xwidget-webkit--create-new-session-buffer)))

;;;; ---- Webview buffer lifecycle ---------------------------------------------

(defun agent-repl--frontend-webview-buffer-name (ws)
  "Return the pinned webview buffer name for workspace WS."
  (format agent-repl-frontend-buffer-name-format ws))

(defun agent-repl--frontend-make-webview-buffer (url)
  "External-boundary wrapper: create a WKWebView xwidget buffer on URL.
Body does nothing but the external call; tests mock via `cl-letf'.
Registered in `agent-repl--external-boundary-functions'."
  (require 'xwidget)
  (xwidget-webkit--create-new-session-buffer url)) ;; ALLOW-EXTERNAL-BOUNDARY

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
        (kill-buffer existing))
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
  "Return a live main-area (non-side) window to host the webview.
`window-main-window' can return an INTERNAL window when the main area
is split, and `select-window' on an internal window errors — so walk
the frame's live windows and take the first that is not a side window.
Falls back to the selected window (always live) when everything else is
a side window."
  (or (seq-find (lambda (win)
                  (not (agent-repl-window--side-window-p win)))
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
  (let ((win (agent-repl--frontend-main-area-window)))
    (select-window win)
    (set-window-buffer win buf))
  buf)

;;;; ---- Entry point ----------------------------------------------------------------

;;;###autoload
(defun agent-repl-frontend-open-panel ()
  "Open the web frontend for the current workspace's session.
The lazy end-to-end trigger: ensures the daemon is built and running,
ensures this workspace's daemon session (rooted at its worktree),
mounts the webview buffer attached to that session, and places it in
the agent output window."
  (interactive)
  (unless (agent-repl--frontend-xwidget-available-p)
    (user-error "agent-repl: this Emacs build lacks xwidget-webkit support"))
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws
      (user-error "agent-repl: no current workspace"))
    (let* ((session-id (agent-repl--frontend-ensure-session ws))
           (url (agent-repl--frontend-session-url session-id))
           (buf (agent-repl--frontend-ensure-webview-buffer ws session-id url)))
      (agent-repl--frontend-display-webview ws buf))))

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
    (kill-buffer buf)
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
      (kill-buffer buf))))

(add-hook 'agent-repl-ws-del-hook #'agent-repl--frontend-release-workspace-webview)

(provide 'frontend)

;;; frontend.el ends here
