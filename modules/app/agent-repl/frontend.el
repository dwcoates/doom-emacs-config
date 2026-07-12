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
;;   - `xwidget-webkit-mode' installs a "WebKit: <document title>"
;;     header-line; the panel is chrome, not a browser, so the
;;     header-line is cleared on mount.
;;
;; The WKWebView is external state: creation funnels through the
;; boundary wrapper `agent-repl--frontend-make-webview-buffer',
;; registered in `agent-repl--external-boundary-functions'; batch tests
;; mock it (xwidgets do not exist in `emacs -batch' builds anyway).

;;; Code:

(require 'cl-lib)
(require 'url-util)

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
(declare-function agent-repl--clear-main-area-for-panels "agent-repl-panels" ())
(declare-function agent-repl--close-buffer-windows "agent-repl-panels" (&rest bufs))
(declare-function agent-repl--restore-fullscreen-config "agent-repl-panels" (ws))
(declare-function agent-repl--buffer-name "agent-repl-core" (suffix ws))
(declare-function agent-repl--ws-backend-name "agent-repl-backend" (ws))
(declare-function agent-repl--frontend-validate-pair "agent-repl-frontends" (frontend-name backend-name &optional env))
(declare-function agent-repl--frontend-validate-for-ws "agent-repl-frontends" (frontend-name ws))
(declare-function agent-repl--ws-choose-frontend "agent-repl-frontends" (ws name))
(declare-function agent-repl--ws-set-agent-state "agent-repl-status" (ws state))
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
(declare-function xwidget-webkit-get-selection "xwidget" (proc))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-normalize-keymaps "evil-core" (&optional state))

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

;;;; ---- Copying the webview's highlighted text --------------------------------

(defun agent-repl--frontend-webview-selection (callback)
  "External-boundary wrapper: hand WebKit's current selection to CALLBACK.
Runs `window.getSelection()' inside the current buffer's webview, so the
answer arrives asynchronously.  Body does nothing but the external call;
tests mock via `cl-letf'.  Registered in
`agent-repl--external-boundary-functions'."
  (require 'xwidget)
  (xwidget-webkit-get-selection callback)) ;; ALLOW-EXTERNAL-BOUNDARY

(defun agent-repl--frontend-yank-selection (text)
  "Put the webview's selected TEXT on the kill ring, reporting what happened.
The kill ring is the system clipboard's Emacs end (`select-enable-clipboard'),
so a killed selection is pasteable outside Emacs too.  An empty or
whitespace-only TEXT means nothing was highlighted, and is NOT killed —
clobbering the kill ring with a stray click's empty selection would be a
silent data loss."
  (if (or (null text) (string-empty-p (string-trim text)))
      (message "agent-repl: nothing highlighted in the webview")
    (kill-new text)
    (message "agent-repl: copied %d chars from the webview" (length text))))

;;;###autoload
(defun agent-repl-frontend-copy-selection ()
  "Copy the webview's highlighted text to the kill ring and system clipboard.
Bound to `C-c' and `y' (the vim reflex) in the webview panel, since the
WKWebView has no menu bar of its own to copy a mouse-made highlight with."
  (interactive)
  (agent-repl--frontend-webview-selection #'agent-repl--frontend-yank-selection))

(defvar agent-repl-frontend-webview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") #'agent-repl-frontend-copy-selection)
    (define-key map (kbd "C-c") #'agent-repl-frontend-copy-selection)
    map)
  "Keymap of `agent-repl-frontend-webview-mode'.
`C-c' shadows the mode-specific prefix in webview buffers, which host no
`C-c' bindings of their own — the webview is chrome, not an editor.")

;;;###autoload
(define-minor-mode agent-repl-frontend-webview-mode
  "Minor mode giving agent-repl webview buffers their copy chords.
Enabled on every webview the module mounts (the workspace's gui panel
and the explain-config popup alike), so `C-c' / `y' copy the highlight
there and nowhere else — plain `xwidget-webkit-mode' browsing keeps its
own bindings."
  :lighter nil
  :keymap agent-repl-frontend-webview-mode-map
  ;; Evil only consults a minor-mode map's auxiliary (per-state) keymaps
  ;; once `evil-normalize-keymaps' has rebuilt `evil-mode-map-alist' for
  ;; the buffer, and merely ENABLING a minor mode does not trigger that.
  ;; Unnormalized, evil's own maps still outrank this one and the chords
  ;; land elsewhere: `y' on the major mode's evil aux map (where `y y'
  ;; copies the page URL) and `C-c' on the global mode-specific prefix.
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps)))

;; Evil binds `y' (`evil-yank') in normal/visual state, and its state maps
;; outrank a minor-mode map — so the chord must be planted in this map's
;; evil auxiliary maps too, or `y' would start an Emacs-region yank
;; operator over a buffer that holds no text at all.
(when (fboundp 'evil-define-key*)
  (dolist (state '(normal motion visual insert emacs))
    (evil-define-key* state agent-repl-frontend-webview-mode-map
                      (kbd "y") #'agent-repl-frontend-copy-selection
                      (kbd "C-c") #'agent-repl-frontend-copy-selection)))

;;;; ---- Webview buffer adoption ----------------------------------------------

(defun agent-repl--frontend-adopt-webview-buffer (buf name)
  "Make webview BUF an agent-repl panel called NAME, and return it.
Every mount site (the workspace gui panel, the explain-config popup)
adopts its webview through here, so the three properties that make a
webview OURS never drift apart:
  - the buffer name is pinned via the buffer-local
    `xwidget-webkit-buffer-name-format' (itself the fixed NAME, with no
    %-constructs), so the webapp's `document.title' changes never rename it;
  - `xwidget-webkit-mode's \"WebKit: <title>\" header-line is cleared,
    since the webview is a panel, not a browser;
  - `agent-repl-frontend-webview-mode' arms the copy chords."
  (with-current-buffer buf
    (setq-local xwidget-webkit-buffer-name-format name)
    (setq-local header-line-format nil)
    (agent-repl-frontend-webview-mode 1)
    (rename-buffer name t))
  buf)

(defun agent-repl--frontend-ensure-webview-buffer (ws session-id url)
  "Return a live webview buffer for WS attached to SESSION-ID at URL.
Reuses the recorded `:frontend-buffer' only while it is live AND still
bound to SESSION-ID (`:frontend-buffer-session-id'); a session change
kills the stale webview and mounts a fresh one, since an xwidget
session cannot be retargeted reliably from outside.  The fresh buffer
is handed to `agent-repl--frontend-adopt-webview-buffer', which pins its
name, drops the browser header-line, and arms the copy chords."
  (let ((existing (agent-repl--ws-get ws :frontend-buffer))
        (bound-to (agent-repl--ws-get ws :frontend-buffer-session-id)))
    (if (and (buffer-live-p existing) (equal bound-to session-id))
        existing
      (when (buffer-live-p existing)
        (agent-repl--log ws "frontend webview rebind: session %s -> %s (killing stale webview)"
                          bound-to session-id)
        (agent-repl--frontend-kill-webview existing))
      (let* ((buf (agent-repl--frontend-make-webview-buffer url))
             (name (agent-repl--frontend-webview-buffer-name ws)))
        (agent-repl--frontend-adopt-webview-buffer buf name)
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
  "Display BUF as the workspace's frontend view filling the frame's main area.
When the vterm/input panels are visible they are HIDDEN first through
the module's own path (`agent-repl--hide-panels') rather than swapped
under: replacing the buffer of the strongly-dedicated output window
would (a) leave the input panel orphaned for the sync-panels sweep to
reap and (b) break the next `agent-repl--show-panels' against the
still-dedicated window.  The webview then takes a live main-area
window, and — exactly like the vterm layout, for which fullscreen is
the sole display format — every OTHER main-area window is cleared
\(`agent-repl--clear-main-area-for-panels', drawer excluded), so the
webview + input panels end up the only main-area windows.  Without
the clear, whatever the frame carried before the mount (magit, the
dashboard, a previous workspace's leftovers) stayed up beside the
panels — the extra-windows-on-first-switch bug."
  (when (agent-repl--panels-visible-p)
    (agent-repl--log ws "display-webview: hiding agent panels first")
    (agent-repl--hide-panels))
  ;; Save the pre-panel layout exactly like the vterm show path: the
  ;; gui hide/close paths restore it, which is what removes BOTH gui
  ;; windows (deleting them directly is impossible once the input
  ;; window is the frame's sole survivor). Guarded like show-panels so
  ;; re-shows never clobber the saved work layout.
  (unless (or (agent-repl--ws-get ws :fullscreen-config)
              (let ((webview (agent-repl--ws-get ws :frontend-buffer)))
                (and (buffer-live-p webview) (get-buffer-window webview))))
    (agent-repl--ws-put ws :fullscreen-config (current-window-configuration)))
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
      (agent-repl--clear-main-area-for-panels)
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
The lazy end-to-end trigger: validates the backend/env capability and
xwidget support, ensures the daemon (built if stale, launched if
absent), ensures WS's daemon session (rooted at its worktree), mounts
the webview attached to that session, and places it over the input
panel."
  (unless (agent-repl--frontend-xwidget-available-p)
    (user-error "agent-repl: this Emacs build lacks xwidget-webkit support"))
  (agent-repl--frontend-validate-for-ws 'gui ws)
  (let* ((session-id (agent-repl--frontend-ensure-session ws))
         (url (agent-repl--frontend-webview-url ws session-id))
         (buf (agent-repl--frontend-ensure-webview-buffer ws session-id url)))
    (agent-repl--frontend-display-webview ws buf)))

(defun agent-repl--gui-boot (ws &optional _project-dir-hint _active-env-hint)
  "The gui frontend's boot capability (registry `:boot-fn').
Starts WS's daemon session in the BACKGROUND — no daemon is asked for a
webview and no window is touched, because the birth and restore paths
run in the CALLER's frame (a newly generated workspace is not the
current one, and mounting its webview here would evict the user's
windows).  The view arrives later, when the user switches to WS and the
`:pending-show-panels' drain shows it through the frontend.

Booting the session eagerly (rather than lazily at first open) is what
gives a generated gui workspace the same contract the vterm boot has:
the agent starts immediately, its `session_start' hook fires, and the
`:pending-prompts' queued by the workspace-generation dispatch drain
into it (`agent-repl--on-session-start-event').

Writes `:agent-state :init' before the session exists — the same
documented lifecycle exception `agent-repl--initialize-agent' takes (no
hook fires between \"the session is being created\" and session_start,
so Emacs is the only observer of it).  Without it a generated gui
workspace would render NO state in the tab and drawer until its agent
answered, where the vterm-born one showed a loading badge; the gui
branch of `agent-repl--on-session-start-event' flips `:init' to `:idle'.

The hints are unused: `agent-repl--frontend-boot-session' has already
hydrated the environment with them, and the gui reads WS's
`:project-dir' from the plist (`agent-repl--frontend-ensure-session')."
  (agent-repl--frontend-validate-for-ws 'gui ws)
  (agent-repl--ws-set-agent-state ws :init)
  (agent-repl--frontend-ensure-session ws))

(defun agent-repl--frontend-webview-url (ws session-id)
  "Return the webapp URL for WS's webview attached to SESSION-ID.
composer=0: Emacs owns input (the panel below), so the webview hides
its own composer and stays output-only.  parent_ws: the recorded
parent worktree's basename (the same source
`agent-repl--workspace-mode-line' renders green) — the webapp's status
bar shows it for vterm parity.  Omitted when the workspace has no
recorded parent."
  (concat (agent-repl--frontend-session-url session-id)
          "&composer=0"
          (when-let ((parent (agent-repl--frontend-parent-ws-name ws)))
            (concat "&parent_ws=" (url-hexify-string parent)))))

(defun agent-repl--frontend-sync-webview (ws session-id)
  "Remount WS's displayed webview when it is bound to a session other than SESSION-ID.
The send path heals a dead daemon session by creating a fresh one
\(`agent-repl--frontend-ensure-session'); without this remount the
displayed webview keeps rendering the DEAD session while the turn
streams into the replacement.  No-op when no webview buffer is live
\(panel closed — the next open mounts fresh anyway) or when the
binding already matches."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer))
        (bound (agent-repl--ws-get ws :frontend-buffer-session-id)))
    (when (and (buffer-live-p buf) (not (equal bound session-id)))
      (agent-repl--log ws "sync-webview: displayed webview %s -> %s" bound session-id)
      (let ((win (get-buffer-window buf t))
            (new (agent-repl--frontend-ensure-webview-buffer
                  ws session-id (agent-repl--frontend-webview-url ws session-id))))
        (when (window-live-p win)
          (set-window-buffer win new))))))

(defun agent-repl--frontend-parent-ws-name (ws)
  "Return the basename of WS's recorded parent worktree, or nil.
Reads `:source-ws-dir' exactly like the vterm mode-line's parent label;
nil when no parent was recorded or the recorded value is empty."
  (let ((source-dir (agent-repl--ws-get ws :source-ws-dir)))
    (when (and source-dir (not (string-empty-p source-dir)))
      (file-name-nondirectory (directory-file-name source-dir)))))

(defun agent-repl--gui-show (ws)
  "The gui frontend's show capability (registry `:show-fn').
Remounts the live webview (or opens fresh when it died)."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (if (buffer-live-p buf)
        (agent-repl--frontend-display-webview ws buf)
      (agent-repl--gui-open ws))))

(defun agent-repl--gui-hide (ws)
  "The gui frontend's hide capability (registry `:hide-fn').
Restores the pre-panel window layout saved at display time (the same
contract as the vterm close paths — restoring is what removes BOTH gui
windows, since the input window cannot be deleted once it is the sole
survivor).  Buffers and the daemon session survive.  Falls back to
closing the individual windows when no layout was saved, resolving the
input buffer by NAME too since the plist key can go stale nil across
frontend switches while the named buffer stays displayed."
  (unless (agent-repl--restore-fullscreen-config ws)
    (agent-repl--close-buffer-windows
     (agent-repl--ws-get ws :frontend-buffer)
     (or (agent-repl--ws-get ws :input-buffer)
         (get-buffer (agent-repl--buffer-name "-input" ws))))))

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
  :boot-fn #'agent-repl--gui-boot
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
  ;; The daemon spawns the agent on the HOST — there is no docker
  ;; wrapper on this path (`agent-repl--build-start-cmd', the vterm's
  ;; sandbox launcher, has no counterpart here).  A `:sandbox'
  ;; workspace therefore cannot be presented by the gui: doing so would
  ;; quietly re-launch it outside the container it asked for.
  :supported-envs '(:bare-metal)
  :durable-session-id-fn #'agent-repl--gui-durable-session-id
  :adopt-session-fn #'agent-repl--gui-adopt-session))

;;;###autoload
(defun agent-repl-frontend-open-panel ()
  "Open the web frontend for the current workspace's session.
Records `gui' as the workspace's DELIBERATE frontend choice (asking for
the web panel by name is a choice, so it outlives a restart) and
dispatches the gui open capability.  The unified command surface —
`SPC o c' and friends — reaches the same place through the frontend
registry."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws
      (user-error "agent-repl: no current workspace"))
    (agent-repl--frontend-validate-for-ws 'gui ws)
    (agent-repl--ws-choose-frontend ws 'gui)
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
