;;; frontend.el --- xwidget webview panel for the web frontend -*- lexical-binding: t; -*-

;;; Commentary:

;; Mounts the claude-repld webapp inside Emacs as an xwidget-webkit
;; buffer placed in the workspace's agent output window — the in-Emacs
;; browser rendering of the session.
;;
;; The command `agent-repl-frontend-open-panel' is the user entry point
;; AND the lazy initialization trigger: it ensures the daemon (built
;; if stale, launched if absent — daemon.el), ensures the workspace's
;; session (frontend-client.el), then shows the webview attached to
;; that session's URL.
;;
;; Buffer identity rules (hard-won from the panel machinery's naming
;; regexes in core.el):
;;   - Webview buffers use the `*agent-frontend-WS*' prefix, matched by
;;     `agent-repl--frontend-buffer-re'.  Now that vterm is gone, the
;;     webview is simply one of a workspace's two buffers —
;;     `agent-repl--agent-panel-buffer-p' matches it alongside the
;;     input composer, and the orphan sweep / close-panels-on-open
;;     treat it as the agent panel it is, with no special-casing left
;;     to carve out.
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
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--agent-view-buffer-p "agent-repl-core" (&optional buf))
(declare-function agent-repl--buffer-owner "agent-repl-core" (buf))
(declare-function agent-repl--current-ws-p "agent-repl-core" (ws))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key val))
(declare-function agent-repl--align-buffer-to-ws-dir "agent-repl-status" (buf ws))
(declare-function agent-repl--frontend-after-ensure-session "agent-repl-frontend-client" (ws on-success on-failure))
(declare-function agent-repl--frontend-force-fresh-session "agent-repl-frontend-client" (ws on-success on-failure))
(declare-function agent-repl--frontend-restart-session "agent-repl-frontend-client" (ws))
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
(declare-function agent-repl--gui-adopt-session "agent-repl-frontend-client" (ws claude-session-id on-success on-failure))
(defvar agent-repl-input-height-fraction)
(declare-function xwidget-webkit--create-new-session-buffer "xwidget" (url &optional callback))
(declare-function xwidget-webkit-current-session "xwidget" ())
(declare-function xwidget-webkit-goto-uri "xwidget.c" (xwidget uri))
(declare-function xwidget-webkit-get-selection "xwidget" (proc))
(declare-function xwidget-webkit-execute-script "xwidget" (xwidget script &optional callback))
(declare-function xwidget-webkit-uri "xwidget.c" (xwidget))
(declare-function xwidget-at "xwidget" (pos))
(declare-function xwidget-live-p "xwidget" (xwidget))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-normalize-keymaps "evil-core" (&optional state))

(defvar xwidget-webkit-buffer-name-format)
(defvar agent-repl--owning-workspace)

;;;; ---- Customization ------------------------------------------------------

(defcustom agent-repl-frontend-buffer-name-format "*agent-frontend-%s*"
  "Format for webview buffer names; %s is the workspace name.
Must NOT collide with `agent-repl-panel-buffer-name-format' — the
panel regexes in core.el key real behavior (bounce, orphan sweep) off
that namespace and the webview must stay outside it."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-frontend-text-size-step 0.02
  "Fraction of the base text size one text-size command nudges the webview.
The gui's text size is a scale multiplier on the webapp's root font
\(webapp/src/host.ts), and each `agent-repl-frontend-text-size-increase'
or `agent-repl-frontend-text-size-decrease' adds or subtracts this
fraction.  Deliberately small so the size can be dialed in finely — the
default 0.02 is a two-percent step."
  :type 'number
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

(defun agent-repl--xwidget-remedy ()
  "Return the recipe, as indented lines, for obtaining an xwidget Emacs.
The Homebrew formulae are offered only on darwin, where they are the two
builds that actually carry `--with-xwidgets'; every platform gets the
from-source flag."
  (concat
   (when (eq system-type 'darwin)
     (concat
      "  brew reinstall emacs-mac  --with-xwidgets    (railwaycat/emacsmacport)\n"
      "  brew reinstall emacs-plus --with-xwidgets    (d12frosted/emacs-plus)\n"))
   "  ./configure --with-xwidgets                  (building from source)\n"))

(defun agent-repl--frontend-require-xwidget (&optional ws)
  "Signal a `user-error' unless this Emacs can host WKWebView xwidgets.

The gui is the only frontend agent-repl has, so an Emacs without
xwidget-webkit cannot open a workspace AT ALL — there is nothing to fall
back to (vterm was the fallback, and it is gone).  That makes this the
one error in the module a user can hit with no way forward, so it hands
back the recipe out instead of just the diagnosis."
  (unless (agent-repl--frontend-xwidget-available-p)
    (agent-repl--log ws "gui open rejected: xwidget-unavailable")
    (user-error
     "%s"
     (concat
      "agent-repl: this Emacs has no xwidget-webkit support, which the gui "
      "frontend requires — and the gui is the only frontend.\n\n"
      "Rebuild Emacs with xwidgets:\n"
      (agent-repl--xwidget-remedy)
      "\nThen verify with:  M-: (featurep 'xwidget-internal)  =>  t"))))

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

(defun agent-repl--frontend-webview-execute-script-1 (buf script)
  "External-boundary wrapper: evaluate SCRIPT inside BUF's webview.
Evaluating JavaScript against the live document is the ONLY channel
Emacs has into a mounted webview.  Every host-driven action reaches it
through `agent-repl--frontend-webview-execute-script', which wraps this
with the keyboard-release epilogue; nothing else may call it directly.
Body does nothing but the external calls; tests mock via `cl-letf'.
Registered in `agent-repl--external-boundary-functions'."
  (require 'xwidget)
  (with-current-buffer buf
    (xwidget-webkit-execute-script (xwidget-webkit-current-session) script))) ;; ALLOW-EXTERNAL-BOUNDARY

;;;; ---- Returning the keyboard to Emacs after a script evaluation -------------

;; Symptom: after a prompt send, keys stop reaching Emacs — RET draws the
;; macOS beep, evil-mode never sees the event — until the user clicks the
;; Emacs text area.  It is NOT app-level focus loss; the frame stays key.
;;
;; Mechanism, from the NS port's own source (src/nsxwidget.m).  When the
;; WKWebView holds first responder, its `keyDown:' override does not
;; swallow the event: it evaluates the injected `xwHasFocus()' and
;; FORWARDS the key to Emacs unless that returns true.  `xwHasFocus()' is
;; exactly
;;
;;     var ae = document.activeElement;
;;     return ae && (ae.nodeName == 'INPUT' || ae.nodeName == 'TEXTAREA');
;;
;; So the thing that eats the keyboard is not the webview being first
;; responder — it is an INPUT or TEXTAREA inside the page holding DOM
;; focus.  Our scripts re-render the feed and the sidebar under the page's
;; own focus management, so a render can leave one of the webapp's inputs
;; focused; whether it does depends on what the page had mounted and
;; selected, which is why the bug reads as "often, not always".
;;
;; The cure therefore has to be applied in the DOM, not in Emacs.  There
;; is no lisp lever here at all: `select-frame-set-input-focus',
;; `x-focus-frame' and `redirect-frame-focus' land in `ns_focus_frame'
;; (src/nsterm.m), which only does `makeKeyAndOrderFront:' — a no-op on an
;; already-key window, and it never calls `makeFirstResponder:'.
;; `select-window' is pure lisp bookkeeping and touches nothing.
;; `xwidget-perform-lispy-event' is `#ifdef USE_GTK' in its entirety
;; (src/xwidget.c), a silent no-op on this port.  The only built-in escape
;; is the page-side `C-g' handler, which is not reachable from lisp.
;;
;; Hence: every host-driven script carries a blur epilogue that drops DOM
;; focus, re-opening the `keyDown:' forwarding path.  Because the epilogue
;; rides IN the script rather than in a follow-up evaluation, it needs no
;; timer and cannot stack: a sidebar push across six webviews issues the
;; same six evaluations it always did, each self-contained and ordered
;; after its own render.

(defconst agent-repl-frontend-keyboard-release-js
  "if(document.activeElement&&document.activeElement.blur)document.activeElement.blur();"
  "JavaScript that drops the page's DOM focus so keys reach Emacs again.
Blurring is what flips the NS port's `xwHasFocus()' back to false — see
the commentary above.  Guarded on `blur' existing because
`document.activeElement' is null in a document with no body yet, and a
webview mid-navigation is an expected state rather than a violated
invariant.

Not a cure for a page that asynchronously re-focuses an input AFTER the
script runs; nothing host-side can be.  That case belongs to the webapp's
own focus management.")

(defun agent-repl--frontend-keyboard-release-wanted-p ()
  "Return non-nil when a script must hand the keyboard back to Emacs.
True unless the selected window is itself displaying a webview.  A user
who has deliberately selected the webview window is driving the page —
typing into its inputs is the whole point — and blurring underneath them
would break the very thing this release exists to protect."
  (let ((win (selected-window)))
    (not (and (window-live-p win)
              (agent-repl--agent-view-buffer-p (window-buffer win))))))

(defun agent-repl--frontend-script-with-keyboard-release (script)
  "Return SCRIPT with the keyboard-release epilogue appended, when wanted.
Returns SCRIPT unchanged when the webview's own window is selected (see
`agent-repl--frontend-keyboard-release-wanted-p').  The separating
semicolon is unconditional: SCRIPT's own terminator is its business, and
a doubled semicolon is an empty statement in JavaScript."
  (if (agent-repl--frontend-keyboard-release-wanted-p)
      (concat script ";\n" agent-repl-frontend-keyboard-release-js)
    script))

(defun agent-repl--frontend-webview-execute-script (buf script)
  "Evaluate SCRIPT inside BUF's webview, then hand the keyboard back to Emacs.
The single chokepoint every host-driven script goes through — tail snap,
sidebar push, topbar close, text size, chess step, output nav — so the
keyboard release is applied here once rather than at each caller.  See
the commentary above `agent-repl-frontend-keyboard-release-js' for why
the release is a DOM blur and not any of the lisp focus functions."
  (agent-repl--frontend-webview-execute-script-1
   buf (agent-repl--frontend-script-with-keyboard-release script)))

;;;; ---- Snapping the feed to its newest message -------------------------------

(defconst agent-repl-frontend-tail-hook "agentReplParkAtTail"
  "Name of the webapp global that parks the feed at its newest message.
The webapp plants it on `window' at boot (`TAIL_HOOK' in
webapp/src/host.ts) — the two names are one contract and MUST match.")

(defun agent-repl--frontend-tail-script ()
  "Return the JavaScript that snaps the webview's feed to its tail.
Calls the hook only when the webapp has already planted it: a webview
mid-navigation has no hook yet, and that is an expected state rather
than a violated invariant — a page that has not finished booting has
nothing to snap, and the boot's own restored-session render parks it at
the tail anyway."
  (format "window.%s && window.%s();"
          agent-repl-frontend-tail-hook
          agent-repl-frontend-tail-hook))

(defun agent-repl--frontend-snap-webview-to-tail (ws)
  "Snap WS's webview feed to its newest message, with no scroll animation.
Switching TO a workspace must show its agent's latest output
immediately, not the middle of the history the user happened to leave
the feed scrolled up to.  The snap is a scrollTop assignment inside the
page, so the tail is simply THERE on the next frame.

No-op when WS has no live webview — e.g. a workspace whose panel is
closed, which mounts a fresh webview (already tail-parked) on its next
open."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (when (buffer-live-p buf)
      (agent-repl--log ws "snap-webview-to-tail: buf=%s" (buffer-name buf))
      (agent-repl--frontend-webview-execute-script
       buf (agent-repl--frontend-tail-script)))
    (unless (buffer-live-p buf)
      (agent-repl--log-verbose ws "snap-webview-to-tail: skipped=no-live-webview"))))

(defun agent-repl--frontend-kill-webview (buf)
  "Kill webview BUF without the xwidget kill-query prompt.
`xwidget-kill-buffer-query-function' (on `kill-buffer-query-functions')
raises a blocking yes-or-no minibuffer prompt for any buffer holding
xwidgets; every frontend kill site is an INTENTIONAL teardown (rebind,
close-panel, workspace nuke), so the prompt is suppressed — left in
place it deadlocks non-interactive callers like the nuke hook."
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buf)))

;;;; ---- Refreshing live webviews ----------------------------------------------

(defun agent-repl--frontend-webview-live-widget (buf)
  "External-boundary wrapper: return BUF's live WKWebView xwidget, or nil.
Reads the xwidget out of BUF itself rather than through
`xwidget-webkit-current-session', whose last-session fallback would hand
back some OTHER buffer's webview for a buffer that has lost its own —
a sweep over many buffers must never be able to act on the wrong page.
Body does nothing but the external calls; tests mock via `cl-letf'.
Registered in `agent-repl--external-boundary-functions'."
  (require 'xwidget)
  (with-current-buffer buf
    (let ((xw (xwidget-at (point-min)))) ;; ALLOW-EXTERNAL-BOUNDARY
      (and (xwidget-live-p xw) xw))))    ;; ALLOW-EXTERNAL-BOUNDARY

(defun agent-repl--frontend-webview-reload-widget (xwidget)
  "External-boundary wrapper: re-navigate XWIDGET to its current URI.
Returns the URI navigated to.  Re-navigation (rather than
`xwidget-webkit-reload's zero-offset history walk) is what a redeploy
needs: the page must come back from the daemon's freshly restarted
listener, on the same session URL it already carries.  Signals when the
webview reports no URI — there is nothing to navigate to, and the sweep
records that as a failed refresh rather than pretending one happened.
Body does nothing but the external calls; tests mock via `cl-letf'.
Registered in `agent-repl--external-boundary-functions'."
  (require 'xwidget)
  (let ((uri (xwidget-webkit-uri xwidget))) ;; ALLOW-EXTERNAL-BOUNDARY
    (when (or (null uri) (string-empty-p uri))
      (error "agent-repl: webview reports no URI to reload (xwidget=%S)" xwidget))
    (xwidget-webkit-goto-uri xwidget uri) ;; ALLOW-EXTERNAL-BOUNDARY
    uri))

(defun agent-repl--frontend-webview-workspace (buf)
  "Return the workspace name webview BUF belongs to.
Prefers the permanent-local `agent-repl--owning-workspace' recorded on
the buffer, falling back to the name the buffer is pinned to — the
webview name format encodes the workspace, and a webview mounted before
the owner was stamped still has to be identifiable in the log."
  (or (agent-repl--buffer-owner buf)
      (let ((name (buffer-name buf)))
        (when (string-match "\\`\\*agent-frontend-\\(.+\\)\\*\\'" name)
          (match-string 1 name)))))

(defun agent-repl--frontend-live-webview-buffers ()
  "Return every live workspace webview buffer, in `buffer-list' order."
  (seq-filter (lambda (buf)
                (and (buffer-live-p buf)
                     (agent-repl--agent-view-buffer-p buf)))
              (buffer-list)))

;;;###autoload
(defun agent-repl-refresh-webviews ()
  "Reload every live workspace webview page, returning the count refreshed.

A webview outlives the daemon it was mounted against: bin/deploy-all.sh
restarts the daemon under the running Emacs, leaving each mounted page
talking to a listener that no longer exists.  This sweep re-navigates
every such page so it re-attaches to the freshly deployed daemon.

One bad webview never stops the sweep: a buffer whose WKWebView is dead
or refuses to report a URI is recorded as a warning and the sweep moves
on to the next buffer.  Every outcome — refreshed, dead, failed — is on
the log with its workspace and buffer."
  (interactive)
  (let ((bufs (agent-repl--frontend-live-webview-buffers))
        (refreshed 0))
    (agent-repl--log nil "refresh-webviews: sweep begin candidates=%d" (length bufs))
    (dolist (buf bufs)
      (let ((ws (agent-repl--frontend-webview-workspace buf)))
        (condition-case err
            (let ((xw (agent-repl--frontend-webview-live-widget buf)))
              (if (null xw)
                  (agent-repl--warn ws "refresh-webviews: buffer=%s outcome=dead-webview"
                                    (buffer-name buf))
                (let ((uri (agent-repl--frontend-webview-reload-widget xw)))
                  (setq refreshed (1+ refreshed))
                  (agent-repl--log ws "refresh-webviews: buffer=%s outcome=refreshed url=%s"
                                   (buffer-name buf) uri))))
          (error
           (agent-repl--warn ws "refresh-webviews: buffer=%s outcome=reload-failed err=%S"
                             (buffer-name buf) err)))))
    (agent-repl--log nil "refresh-webviews: sweep done refreshed=%d candidates=%d"
                     refreshed (length bufs))
    (when (called-interactively-p 'interactive)
      (message "agent-repl: refreshed %d of %d webview(s)" refreshed (length bufs)))
    refreshed))

;;;; ---- Copying the webview's highlighted text --------------------------------

(defun agent-repl--frontend-webview-selection (callback)
  "External-boundary wrapper: hand WebKit's current selection to CALLBACK.
Runs `window.getSelection()' inside the current buffer's webview, so the
answer arrives asynchronously.  Body does nothing but the external call;
tests mock via `cl-letf'.  Registered in
`agent-repl--external-boundary-functions'."
  (require 'xwidget)
  (xwidget-webkit-get-selection callback)) ;; ALLOW-EXTERNAL-BOUNDARY

(defun agent-repl--frontend-yank-selection (text &optional ws)
  "Put the webview's selected TEXT on the kill ring, reporting what happened.
The kill ring is the system clipboard's Emacs end (`select-enable-clipboard'),
so a killed selection is pasteable outside Emacs too.  An empty or
whitespace-only TEXT means nothing was highlighted, and is NOT killed —
clobbering the kill ring with a stray click's empty selection would be a
silent data loss."
  (if (or (null text) (string-empty-p (string-trim text)))
      (progn
        (agent-repl--log ws "copy-selection: outcome=empty")
        (message "agent-repl: nothing highlighted in the webview"))
    (kill-new text)
    (agent-repl--log ws "copy-selection: outcome=copied chars=%d" (length text))
    (message "agent-repl: copied %d chars from the webview" (length text))))

;;;###autoload
(defun agent-repl-frontend-copy-selection ()
  "Copy the webview's highlighted text to the kill ring and system clipboard.
Bound to `C-c' and `y' (the vim reflex) in the webview panel, since the
WKWebView has no menu bar of its own to copy a mouse-made highlight with."
  (interactive)
  (let ((ws agent-repl--owning-workspace)
        (buf (current-buffer)))
    (agent-repl--log ws "copy-selection: requested buf=%s" (buffer-name buf))
    (agent-repl--frontend-webview-selection
     (lambda (text) (agent-repl--frontend-yank-selection text ws)))))

;;;; ---- Chess-board keyboard navigation (out-of-band) -------------------------

(defconst agent-repl-frontend-chess-step-hook "agentReplChessStep"
  "Name of the webapp global that steps the active chess board.
The webapp plants it on `window' at boot (`CHESS_NAV_HOOK' in
webapp/src/chess-game.ts) — the two names are one contract and MUST
match.  It takes \"back\" or \"forward\" and routes to the board the
user last clicked.")

(defun agent-repl--frontend-chess-step-script (direction)
  "Return the JS that steps the active chess board DIRECTION.
Guarded on the hook's existence: a webview mid-boot or mid-navigation
has no hook yet, and that is an expected state rather than a violated
invariant — a page that has not finished booting holds no boards."
  (format "window.%s && window.%s(%S);"
          agent-repl-frontend-chess-step-hook
          agent-repl-frontend-chess-step-hook
          direction))

(defun agent-repl-frontend-chess-back ()
  "Unplay one move on the current webview's active chess board.
Out-of-band keyboard navigation: the NS xwidget cannot reliably deliver
keyboard events into the page, so the webview buffer's keys drive the
board over the execute-script channel instead.  No-op (page-side) when
no board has been clicked."
  (interactive)
  (agent-repl--log agent-repl--owning-workspace
                   "chess-step: direction=back buf=%s" (buffer-name))
  (agent-repl--frontend-webview-execute-script
   (current-buffer) (agent-repl--frontend-chess-step-script "back")))

(defun agent-repl-frontend-chess-forward ()
  "Play one move on the current webview's active chess board.
See `agent-repl-frontend-chess-back' for the out-of-band rationale."
  (interactive)
  (agent-repl--log agent-repl--owning-workspace
                   "chess-step: direction=forward buf=%s" (buffer-name))
  (agent-repl--frontend-webview-execute-script
   (current-buffer) (agent-repl--frontend-chess-step-script "forward")))

(defvar agent-repl-frontend-webview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") #'agent-repl-frontend-copy-selection)
    (define-key map (kbd "C-c") #'agent-repl-frontend-copy-selection)
    (define-key map (kbd "h") #'agent-repl-frontend-chess-back)
    (define-key map (kbd "l") #'agent-repl-frontend-chess-forward)
    (define-key map (kbd "<left>") #'agent-repl-frontend-chess-back)
    (define-key map (kbd "<right>") #'agent-repl-frontend-chess-forward)
    map)
  "Keymap of `agent-repl-frontend-webview-mode'.
`C-c' shadows the mode-specific prefix in webview buffers, which host no
`C-c' bindings of their own — the webview is chrome, not an editor.
`h'/`l' and the arrows step the active chess board (see
`agent-repl-frontend-chess-back'), keys that would otherwise be inert
char motions over a buffer holding no text.")

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

;; Evil's motion state owns `h'/`l' and the arrows (char motions), and
;; those land in the echo area as "Beginning of line"/"End of line" over
;; a buffer with no text — so the chess-nav keys are planted in the evil
;; auxiliary maps too.  Motion-ish states only: insert/emacs state keeps
;; plain typing semantics.
(when (fboundp 'evil-define-key*)
  (dolist (state '(normal motion visual))
    (evil-define-key* state agent-repl-frontend-webview-mode-map
                      (kbd "h") #'agent-repl-frontend-chess-back
                      (kbd "l") #'agent-repl-frontend-chess-forward
                      (kbd "<left>") #'agent-repl-frontend-chess-back
                      (kbd "<right>") #'agent-repl-frontend-chess-forward)))

;;;; ---- Closing the topbar dropdowns on an input-window click -----------------

(defconst agent-repl-frontend-close-menus-hook "agentReplCloseTopbarMenus"
  "Name of the webapp global that closes any open topbar dropdown.
The webapp plants it on `window' at boot (`CLOSE_MENUS_HOOK' in
webapp/src/host.ts) — the two names are one contract and MUST match.")

(defun agent-repl--frontend-close-menus-script ()
  "Return the JS that closes the webview's open topbar dropdowns.
Guarded on the hook's existence: a webview mid-boot or mid-navigation
has no hook yet, and that is an expected state rather than a violated
invariant — a page that has not finished booting holds no open menus."
  (format "window.%s && window.%s();"
          agent-repl-frontend-close-menus-hook
          agent-repl-frontend-close-menus-hook))

(defun agent-repl--frontend-close-topbar-menus (ws)
  "Close any open topbar dropdown in WS's webview.
The webapp's own outside-click handler dismisses its dropdowns on a
click anywhere INSIDE the page, but the input composer is a separate
Emacs window the webview cannot see — so a click there leaves the header
and bubble dropdowns open until Emacs reaches in through this script.

No-op when WS has no live webview — a closed panel mounts a fresh
webview (already free of open menus) on its next open."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (when (buffer-live-p buf)
      (agent-repl--log ws "close-topbar-menus: buf=%s" (buffer-name buf))
      (agent-repl--frontend-webview-execute-script
       buf (agent-repl--frontend-close-menus-script)))
    (unless (buffer-live-p buf)
      (agent-repl--log-verbose ws "close-topbar-menus: skipped=no-live-webview"))))

(defun agent-repl--frontend-close-menus-on-input-click (_frame)
  "Close the current workspace's topbar dropdowns when its input window is clicked.
Hook target is `window-selection-change-functions', so this fires during
redisplay after a selection change.  It acts only when the mouse
\(`mouse-event-p' on `last-input-event') selected the current workspace's
input window: keyboard-driven and programmatic selection are exempt,
which both honors the literal click gesture and skips the
autoselect-on-switch path that selects the input window without a click.

Every other selection change is a no-op — no current workspace, a
non-mouse selection, or a selected window that is not this workspace's
input panel all leave the webview untouched."
  (let ((ws (agent-repl--ws-current-name)))
    (when (and ws
               (mouse-event-p last-input-event)
               (eq (selected-window)
                   (agent-repl-window--panel-window :input ws)))
      (agent-repl--log-verbose ws "input-click: close-topbar-menus selected-window=%s"
                               (selected-window))
      (agent-repl--frontend-close-topbar-menus ws))))

(add-hook 'window-selection-change-functions
          #'agent-repl--frontend-close-menus-on-input-click)

;;;; ---- Adjusting the webview's text size -------------------------------------

(defconst agent-repl-frontend-text-size-hook "agentReplAdjustTextScale"
  "Name of the webapp global that resizes the feed's text.
The webapp plants it on `window' at boot (`TEXT_SCALE_HOOK' in
webapp/src/host.ts) — the two names are one contract and MUST match.  It
takes a numeric delta added to the page's current text scale, or the
string \"reset\" to restore the default size.")

(defun agent-repl--frontend-text-size-script (arg)
  "Return the JS that drives the text-size hook with ARG.
ARG is either a number delta added to the page's current text scale, or
the symbol `reset' to restore the default size.  Guarded on the hook's
existence: a webview mid-boot or mid-navigation has no hook yet, an
expected state rather than a violated invariant — a page that has not
finished booting has no text to resize."
  (let ((js-arg (if (numberp arg)
                    (format "%s" arg)
                  (format "%S" (symbol-name arg)))))
    (format "window.%s && window.%s(%s);"
            agent-repl-frontend-text-size-hook
            agent-repl-frontend-text-size-hook
            js-arg)))

(defun agent-repl--frontend-adjust-text-size (ws arg)
  "Drive WS's webview text-size hook with ARG, returning the webview buffer.
ARG is a number delta or the symbol `reset'.  Returns the webview buffer
when the script ran, or nil when WS has no live webview — a closed panel
mounts a fresh webview (default size) on its next open, so the size is a
live-page preference rather than persistent state."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (if (buffer-live-p buf)
        (progn
          (agent-repl--log ws "adjust-text-size: arg=%s buf=%s" arg (buffer-name buf))
          (agent-repl--frontend-webview-execute-script
           buf (agent-repl--frontend-text-size-script arg))
          buf)
      (agent-repl--log-verbose ws "adjust-text-size: skipped=no-live-webview arg=%s" arg)
      nil)))

(defun agent-repl--frontend-text-size-command (arg)
  "Drive the current workspace's webview text size with ARG.
ARG is a number delta or the symbol `reset'.  Shared by the interactive
increase, decrease, and reset commands.  Signals when there is no
current workspace, or when the current workspace has no webview open."
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws
      (user-error "agent-repl: no current workspace"))
    (unless (agent-repl--frontend-adjust-text-size ws arg)
      (agent-repl--log ws "adjust-text-size: rejected=no-live-webview arg=%s" arg)
      (user-error "agent-repl: no webview open for workspace %s" ws))))

;;;###autoload
(defun agent-repl-frontend-text-size-increase ()
  "Increase the current workspace's gui text size by one fine step.
The step is `agent-repl-frontend-text-size-step' of the base size, small
by design so the size can be dialed in precisely.  Scales the webapp's
root font, so every rem-sized run of text in the feed grows together.
Signals when there is no current workspace or no webview open for it."
  (interactive)
  (agent-repl--frontend-text-size-command agent-repl-frontend-text-size-step))

;;;###autoload
(defun agent-repl-frontend-text-size-decrease ()
  "Decrease the current workspace's gui text size by one fine step.
The inverse of `agent-repl-frontend-text-size-increase', shrinking the
webapp's root font by `agent-repl-frontend-text-size-step' of the base
size.  Signals when there is no current workspace or no webview open for
it."
  (interactive)
  (agent-repl--frontend-text-size-command (- agent-repl-frontend-text-size-step)))

;;;###autoload
(defun agent-repl-frontend-text-size-reset ()
  "Reset the current workspace's gui text size to its default.
Restores the webapp's root font to the base size, discarding every
increase and decrease applied to the live page.  Signals when there is
no current workspace or no webview open for it."
  (interactive)
  (agent-repl--frontend-text-size-command 'reset))

;;;; ---- Webview buffer adoption ----------------------------------------------

(defvar agent-repl-frontend-webview-adopt-hook nil
  "Hook run with the freshly adopted webview buffer CURRENT.

The seam for anything that must decorate the OUTPUT buffer specifically.
`agent-repl--frontend-adopt-webview-buffer' is the one place every mount
site passes through, and the input buffer never passes through it at
all, so a consumer registered here reaches the output window and only
the output window.

Consumers run inside `with-current-buffer' and must not signal: an error
here would abort a webview mount, which is a far worse outcome than a
missing decoration.")

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
    (rename-buffer name t)
    ;; Last, so consumers see a fully adopted buffer (final name, mode armed).
    ;; Wrapped because a decoration that fails must not cost the user a
    ;; webview; the failure is surfaced through the log rather than swallowed.
    (condition-case err
        (run-hooks 'agent-repl-frontend-webview-adopt-hook)
      (error
       (agent-repl--log nil "frontend webview adopt-hook failed buffer=%s err=%S"
                        name err))))
  buf)

(defun agent-repl--frontend-ensure-webview-buffer (ws session-id url)
  "Return a live webview buffer for WS attached to SESSION-ID at URL.
Reuses the recorded `:frontend-buffer' only while it is live AND still
bound to SESSION-ID (`:frontend-buffer-session-id'); a session change
kills the stale webview and mounts a fresh one, since an xwidget
session cannot be retargeted reliably from outside.  The fresh buffer
is handed to `agent-repl--frontend-adopt-webview-buffer', which pins its
name, drops the browser header-line, and arms the copy chords.

Whichever buffer is returned — reused or freshly mounted — its
`default-directory' is realigned to WS's project root via
`agent-repl--align-buffer-to-ws-dir', so `SPC .' from the webview window
resolves against the worktree the REPL is attached to rather than the
foreign directory the xwidget session inherited at creation."
  ;; This is the one rendering choke point: every initial mount, sync, and
  ;; remount arrives here.
  ;;
  ;; NO HEALTH PROBE.  This used to poll session health before mounting,
  ;; because `createSession' acked as soon as a spawn was issued and the mount
  ;; had no other way to know the shim was up.  The daemon now acks the create
  ;; only once the session is ESTABLISHED — its shim answered a health probe
  ;; healthy over the fully wired connection — so by the time a session id
  ;; reaches this function it is usable, and a probe here could only re-ask a
  ;; question already answered (and lose the race it kept losing).
  (let ((existing (agent-repl--ws-get ws :frontend-buffer))
        (bound-to (agent-repl--ws-get ws :frontend-buffer-session-id)))
    (agent-repl--log ws "ensure-webview: session=%s existing-live=%s bound-session=%s"
                     session-id (buffer-live-p existing) bound-to))
  (let* ((existing (agent-repl--ws-get ws :frontend-buffer))
         (bound-to (agent-repl--ws-get ws :frontend-buffer-session-id))
         (buf (if (and (buffer-live-p existing) (equal bound-to session-id))
                  (progn
                    (agent-repl--log ws "ensure-webview: outcome=reused buf=%s session=%s"
                                     (buffer-name existing) session-id)
                    existing)
                (progn
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
                    buf)))))
    (agent-repl--align-buffer-to-ws-dir buf ws)
    buf))

;;;; ---- Placement ---------------------------------------------------------------

(defun agent-repl--frontend-largest-main-area-window ()
  "Return the largest live non-side window of the selected frame.
Used as the parent to split a host out of when every main-area window
is dedicated.  Side windows are excluded because splitting one keeps
the child inside the side-window tree, which is not the main area.
Returns nil only when the frame carries no non-side window at all."
  (car (sort (seq-remove #'agent-repl-window--side-window-p
                         (window-list nil 'no-minibuffer))
             (lambda (a b)
               (> (* (window-total-height a) (window-total-width a))
                  (* (window-total-height b) (window-total-width b)))))))

(defun agent-repl--frontend-main-area-window ()
  "Return a live, UNDEDICATED main-area window able to host the webview.
`window-main-window' can return an INTERNAL window when the main area
is split, and `select-window' on an internal window errors — so walk
the frame's live windows and take the first that is neither a side
window nor DEDICATED (a dedicated window rejects `set-window-buffer',
and the workspace's own hardened input panel is exactly such a window).

When NO undedicated candidate exists, a host is MADE by splitting the
frame's largest main-area window.  This is the routine shape on a
workspace switch, not an exotic one: `agent-repl--maybe-autoselect-input'
leaves the previous workspace's input panel selected, and that panel is
hardened dedicated — so the frame can genuinely be all-dedicated at
mount time.  The old fallback returned `(selected-window)' WITHOUT
re-checking dedication, handed that dedicated window to
`set-window-buffer', and the mount died with \"Window is dedicated
to ...\" — the new workspace's webview never appeared.

Splitting is deliberately preferred over lifting the dedication: the
child of a split is undedicated and unhardened even when its parent is
dedicated and size-fixed, so a host is obtained without ever clearing
a dedication some other workspace's panel recipe set.  The stale-input
reclaim in `agent-repl--frontend-display-webview' does un-dedicate, but
only ever the CALLING workspace's own input window."
  (or (seq-find (lambda (win)
                  (and (not (agent-repl-window--side-window-p win))
                       (not (window-dedicated-p win))))
                (window-list nil 'no-minibuffer))
      (let* ((ws (agent-repl--ws-current-name))
             (parent (or (agent-repl--frontend-largest-main-area-window)
                         (selected-window)))
             (host (split-window parent nil 'below)))
        (agent-repl--log ws
                         "frontend-main-area-window: no undedicated host; split parent=%S host=%S"
                         parent host)
        host)))

(defun agent-repl--frontend-display-webview (ws buf)
  "Display BUF as the workspace's frontend view filling the frame's main area.
When the webview/input panels are visible they are HIDDEN first through
the module's own path (`agent-repl--hide-panels') rather than swapped
under: replacing the buffer of the strongly-dedicated output window
would (a) leave the input panel orphaned for the sync-panels sweep to
reap and (b) break the next display against the still-dedicated
window.  The webview then takes a live main-area window, and — since
fullscreen is the sole display format — every OTHER main-area window is
cleared \(`agent-repl--clear-main-area-for-panels', side windows
excluded), so the webview + input panels end up the only main-area
windows.
Without the clear, whatever the frame carried before the mount (magit,
the dashboard, a previous workspace's leftovers) stayed up beside the
panels — the extra-windows-on-first-switch bug."
  (agent-repl--log ws "display-webview: begin buf=%s panels-visible=%s"
                   (buffer-name buf) (agent-repl--panels-visible-p))
  (when (agent-repl--panels-visible-p)
    (agent-repl--log ws "display-webview: hiding agent panels first")
    (agent-repl--hide-panels))
  ;; Save the pre-panel layout before mounting: the gui hide/close
  ;; paths restore it, which is what removes BOTH gui windows
  ;; (deleting them directly is impossible once the input window is
  ;; the frame's sole survivor). Guarded so re-shows never clobber the
  ;; saved work layout.
  (unless (or (agent-repl--ws-get ws :fullscreen-config)
              (let ((webview (agent-repl--ws-get ws :frontend-buffer)))
                (and (buffer-live-p webview) (get-buffer-window webview))))
    (agent-repl--ws-put ws :fullscreen-config (current-window-configuration))
    (agent-repl--log ws "display-webview: saved-fullscreen-layout"))
  (let* ((input-buf (agent-repl--ensure-input-buffer ws))
         (stale-input-win (get-buffer-window input-buf)))
    ;; A surviving input window from a previous webview mount (the
    ;; webview died or was rebound) is dedicated, so it can neither
    ;; host the webview nor be left to shadow the host search — remove
    ;; it and rebuild the canonical layout from scratch. When it is
    ;; the frame's ONLY window it cannot be deleted; reclaim it as the
    ;; host by lifting its dedication instead.
    (when (window-live-p stale-input-win)
      (agent-repl--log ws "display-webview: reclaiming-stale-input-window window=%s only-window=%s"
                       stale-input-win (one-window-p))
      (if (one-window-p)
          (set-window-dedicated-p stale-input-win nil)
        (delete-window stale-input-win)))
    (let ((win (agent-repl--frontend-main-area-window)))
      (select-window win)
      (agent-repl--clear-main-area-for-panels)
      ;; Re-validate the host between the clear and the mount.
      ;; `--clear-main-area-for-panels' keeps `(selected-window)'
      ;; unconditionally, so whatever WIN is at this point is what
      ;; `set-window-buffer' gets — and a dedicated WIN is precisely
      ;; how the mount used to die ("Window is dedicated to ...")
      ;; when a foreign workspace's hardened input panel was
      ;; selected.  `--frontend-main-area-window' now guarantees an
      ;; undedicated host, so neither branch below should ever fire;
      ;; they exist so a regression surfaces as a named failure
      ;; instead of a raw dedication error from deep inside redisplay.
      (unless (window-live-p win)
        (error "agent-repl--frontend-display-webview: host window died during main-area clear (ws=%s)"
               ws))
      (when (window-dedicated-p win)
        (agent-repl--warn ws
                          "display-webview: host window %S still dedicated to %s after clear; reclaiming"
                          win (buffer-name (window-buffer win)))
        (set-window-dedicated-p win nil))
      (set-window-buffer win buf)
      ;; Hybrid UI: the classic input panel sits below the webview,
      ;; hardened with the standard panel recipe (dedicated,
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
        (select-window input-win)
        (agent-repl--log ws "display-webview: mounted webview-window=%s input-window=%s"
                         win input-win))))
  buf)

;;;; ---- Entry point ----------------------------------------------------------------

(defun agent-repl--gui-open (ws)
  "The gui frontend's open capability (registry `:open-fn').
The lazy end-to-end trigger: validates the backend/env capability and
xwidget support, ensures the daemon (built if stale, launched if
absent), ensures WS's daemon session (rooted at its worktree), mounts
the webview attached to that session, and places it over the input
panel."
  (agent-repl--log ws "gui-open: begin")
  (agent-repl--frontend-require-xwidget ws)
  (agent-repl--frontend-validate-for-ws 'gui ws)
  (agent-repl--frontend-after-ensure-session
   ws
   (lambda (session-id)
     (let* ((url (agent-repl--frontend-webview-url ws session-id))
            (buf (agent-repl--frontend-ensure-webview-buffer ws session-id url)))
       (agent-repl--frontend-display-webview ws buf)
       (agent-repl--log ws "gui-open: outcome=displayed session=%s buf=%s"
                        session-id buf)))
   (lambda (detail) (agent-repl--log ws "gui-open: FAILED detail=%s" detail)))
  :pending)

(defun agent-repl--gui-boot (ws &optional _project-dir-hint _active-env-hint)
  "The gui frontend's boot capability (registry `:boot-fn').
Starts WS's daemon session in the BACKGROUND — no daemon is asked for a
webview and no window is touched, because the birth and restore paths
run in the CALLER's frame (a newly generated workspace is not the
current one, and mounting its webview here would evict the user's
windows).  The view arrives later, when the user switches to WS and the
`:pending-show-panels' drain shows it through the frontend.

Booting the session eagerly (rather than lazily at first open) means a
generated gui workspace starts its agent immediately: its
`session_start' hook fires, and the `:pending-prompts' queued by the
workspace-generation dispatch drain into it
(`agent-repl--on-session-start-event').

Writes `:agent-state :init' before the session exists — there is a
brief window between \"the session is being created\" and the daemon's
own `session_start' event firing where Emacs is the only observer of
the workspace's existence.  Without this write a generated gui
workspace would render NO state in the tab until its agent
answered; the gui branch of `agent-repl--on-session-start-event' flips
`:init' to `:idle' once that event lands.

The hints are unused: `agent-repl--frontend-boot-session' has already
hydrated the environment with them, and the gui reads WS's
`:project-dir' from the plist (`agent-repl--frontend-after-ensure-session')."
  (agent-repl--frontend-validate-for-ws 'gui ws)
  (agent-repl--log ws "gui-boot: begin")
  (agent-repl--ws-set-agent-state ws :init)
  (agent-repl--frontend-after-ensure-session
   ws
   (lambda (session-id) (agent-repl--log ws "gui-boot: outcome=session-started session=%s" session-id))
   (lambda (detail) (agent-repl--log ws "gui-boot: FAILED detail=%s" detail)))
  :pending)

(defun agent-repl--frontend-webview-url (ws session-id)
  "Return the webapp URL for WS's webview attached to SESSION-ID.
composer=0: Emacs owns input (the panel below), so the webview hides
its own composer and stays output-only.  parent_ws: the recorded
parent worktree's basename — the webapp's status bar shows it in its
topbar.  Omitted when the workspace has no recorded parent."
  (concat (agent-repl--frontend-session-url session-id)
          "&composer=0"
          (when-let ((parent (agent-repl--frontend-parent-ws-name ws)))
            (concat "&parent_ws=" (url-hexify-string parent)))))

(defun agent-repl--frontend-sync-webview (ws session-id)
  "Remount WS's displayed webview when bound to a session other than SESSION-ID.
The send path heals a dead daemon session by creating a fresh one
\(`agent-repl--frontend-after-ensure-session'); without this remount the
displayed webview keeps rendering the DEAD session while the turn
streams into the replacement.  No-op when no webview buffer is live
\(panel closed — the next open mounts fresh anyway) or when the
binding already matches."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer))
        (bound (agent-repl--ws-get ws :frontend-buffer-session-id)))
    (cond
     ((not (buffer-live-p buf))
      (agent-repl--log-verbose ws "sync-webview: skipped=no-live-webview target-session=%s" session-id))
     ((equal bound session-id)
      (agent-repl--log-verbose ws "sync-webview: skipped=already-bound buf=%s session=%s"
                               (buffer-name buf) session-id))
     (t
      (agent-repl--log ws "sync-webview: displayed webview %s -> %s" bound session-id)
      (let ((win (get-buffer-window buf t))
            (new (agent-repl--frontend-ensure-webview-buffer
                  ws session-id (agent-repl--frontend-webview-url ws session-id))))
        (when (window-live-p win)
          (set-window-buffer win new)
          (agent-repl--log ws "sync-webview: outcome=swapped old-window=%s new-buffer=%s"
                           win (buffer-name new)))
        (unless (window-live-p win)
          (agent-repl--log ws "sync-webview: outcome=remounted-not-displayed new-buffer=%s"
                           (buffer-name new))))))))

(defun agent-repl--frontend-remount-webview (ws)
  "Force WS's open webview to reload the freshly served webapp bundle.
The daemon serves the webapp off disk (`http.FileServer'), so a rebuilt
bundle is live the moment `bin/build-frontend.sh' finishes — but an
already-mounted webview keeps rendering the bundle it first loaded, and
`agent-repl--frontend-after-ensure-session' would REUSE that live buffer
because the session is unchanged.  Kill the buffer and drop its binding
first, so the fresh mount navigates the URL clean and refetches (Vite's
content-hashed asset names turn the refetch into a guaranteed cache
miss).  The freshly mounted buffer is swapped back into the window that
showed the old one, so a visible panel reloads in place without the
`agent-repl--frontend-display-webview' window rebuild.

A no-op returning nil when WS has no live webview buffer — a closed
panel has nothing to reload, and its next open mounts fresh from the
current bundle anyway.  Returns the new buffer when a remount happened."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (if (not (buffer-live-p buf))
        (progn
          (agent-repl--log ws "remount-webview: skipped=no-live-webview")
          nil)
      (let ((win (get-buffer-window buf t)))
        (agent-repl--frontend-after-ensure-session
         ws
         (lambda (session-id)
           (let ((url (agent-repl--frontend-webview-url ws session-id)))
             (agent-repl--frontend-kill-webview buf)
             (agent-repl--ws-put ws :frontend-buffer nil)
             (agent-repl--ws-put ws :frontend-buffer-session-id nil)
             (let ((new (agent-repl--frontend-ensure-webview-buffer ws session-id url)))
               (agent-repl--log ws "remount-webview: reloaded bundle ws=%s -> %s" ws session-id)
               (when (window-live-p win) (set-window-buffer win new)))))
         (lambda (detail) (agent-repl--log ws "remount-webview: FAILED detail=%s" detail)))
        :pending))))

(defun agent-repl--frontend-remount-all-webviews ()
  "Remount every open workspace's webview so all pick up a rebuilt bundle.
Iterates the live workspaces, remounting each that has an open webview
\(`agent-repl--frontend-remount-webview' skips the rest).  Returns the
count of workspaces whose webview was actually remounted."
  (let ((n 0))
    (dolist (ws (agent-repl--live-ws-names) n)
      (when (agent-repl--frontend-remount-webview ws)
        (setq n (1+ n))))))

;;;###autoload
(defun agent-repl-frontend-reload-webview ()
  "Reload the current workspace's webview so it picks up a rebuilt bundle.
Remounts the live webview against its session
\(`agent-repl--frontend-remount-webview').  Use after rebuilding the
webapp: the daemon serves the bundle off disk, so a rebuild needs only
a fresh mount, not a daemon bounce.  Signals when no webview is open for
the current workspace."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws
      (user-error "agent-repl: no current workspace"))
    (unless (agent-repl--frontend-remount-webview ws)
      (user-error "agent-repl: no webview open for workspace %s" ws))
    (message "agent-repl: webview reloaded")))

;;;###autoload
(defun agent-repl-force-fresh-conversation ()
  "Start a FRESH conversation for the current workspace, discarding resume.
Recreates the workspace's daemon session with no resume — a blank
conversation replaces the resumed one — via
`agent-repl--frontend-force-fresh-session', then snaps the displayed
webview to the fresh session.  Use to abandon a wedged or unwanted
resumed conversation on demand, without opening the resume-loss
investigation workspace the automatic path dispatches.  Signals when
there is no current workspace."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws
      (user-error "agent-repl: no current workspace"))
    (agent-repl--log ws "force-fresh-conversation: begin")
    (agent-repl--frontend-force-fresh-session
     ws
     (lambda (id)
       (agent-repl--frontend-sync-webview ws id)
       (agent-repl--log ws "force-fresh-conversation: outcome=session=%s" id)
       (message "agent-repl: started a fresh conversation (%s)" id))
     (lambda (detail)
       (agent-repl--log ws "force-fresh-conversation: FAILED detail=%s" detail)))
    :pending))

;;;###autoload
(defun agent-repl-restart-session ()
  "HARD-RESTART the current workspace\='s session: new shim, same conversation.

Stops whatever shim is serving the workspace -- including one that
outlived a previous daemon, which this daemon never spawned and could not
otherwise reach -- then brings the SAME session record back up, so the
respawn resumes the same vendor conversation and nothing is lost.

This is a PROCESS restart, not a new conversation.  Reach for it when the
shim is wedged, when it survived a deploy and is running superseded code,
or when the backend simply needs rebuilding under a conversation worth
keeping.  Use `agent-repl-force-fresh-conversation\=' instead to abandon
the conversation itself.

A workspace whose session is merely hibernated or severed is brought up,
because a
restart and a start are the same request when nothing is running.  Signals when there
is no current workspace; a daemon-side failure is surfaced loudly through
the shared command-ack handler rather than read as success."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws
      (user-error "agent-repl: no current workspace"))
    (agent-repl--log ws "restart-session: begin")
    (agent-repl--frontend-restart-session ws)
    (message "agent-repl: restarting the session for %s..." ws)))

(defun agent-repl--frontend-parent-ws-name (ws)
  "Return the basename of WS's recorded parent worktree, or nil.
Reads WS's `:source-ws-dir'; nil when no parent was recorded or the
recorded value is empty."
  (let ((source-dir (agent-repl--ws-get ws :source-ws-dir)))
    (when (and source-dir (not (string-empty-p source-dir)))
      (file-name-nondirectory (directory-file-name source-dir)))))

(defun agent-repl--gui-show (ws)
  "The gui frontend's show capability (registry `:show-fn').
Remounts the live webview (or opens fresh when it died).
Before touching the window layout, synchronously ensures the existing
daemon session is operational.  This is the `SPC o c' wake invariant:
a hibernated workspace is brought up before its UI can look available."
  (agent-repl--frontend-after-ensure-session
   ws
   (lambda (session-id)
     (agent-repl--frontend-sync-webview ws session-id)
     (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
       (if (buffer-live-p buf)
           (agent-repl--frontend-display-webview ws buf)
         (agent-repl--gui-open ws))))
   (lambda (detail) (agent-repl--log ws "gui-show: FAILED detail=%s" detail)))
  :pending)

(defun agent-repl--gui-hide (ws)
  "The gui frontend's hide capability (registry `:hide-fn').
Restores the pre-panel window layout saved at display time — restoring
is what removes BOTH gui windows, since the input window cannot be
deleted once it is the sole survivor.  Buffers and the daemon session
survive.  Falls back to closing the individual windows when no layout
was saved, resolving the input buffer by NAME too as a defensive
fallback since the plist key can go stale nil while the named buffer
stays displayed.

Window teardown is scoped to the workspace currently ON the frame.
When WS is NOT the active workspace — e.g. a background merge tearing
down a DIFFERENT workspace through `agent-repl--gui-kill' — its panels
are not displayed on the visible frame, so restoring its saved layout
via `set-window-configuration' (a frame-global operation that would
clobber the visible workspace's layout) or closing its buffer windows
must NOT run.  In that case the frame is left untouched and the
now-moot saved layout is dropped so a later reopen cannot restore a
stale configuration.  This is the window-isolation guarantee: merging
one workspace never disturbs another workspace's windows."
  (if (not (agent-repl--current-ws-p ws))
      (progn
        (agent-repl--log ws "gui-hide: outcome=background-drop-layout")
        (agent-repl--ws-put ws :fullscreen-config nil))
    (if (agent-repl--restore-fullscreen-config ws)
        (agent-repl--log ws "gui-hide: outcome=restored-layout")
      (agent-repl--log ws "gui-hide: outcome=close-buffer-windows")
      (agent-repl--close-buffer-windows
       (agent-repl--ws-get ws :frontend-buffer)
       (or (agent-repl--ws-get ws :input-buffer)
           (get-buffer (agent-repl--buffer-name "-input" ws)))))))

(defun agent-repl--gui-kill (ws)
  "The gui frontend's kill capability (registry `:kill-fn').
Tears down the LAYOUT first (webview + dedicated input windows), then
deletes the daemon session (best-effort) and kills the webview.  The
window teardown is contractual: the registry's `:restart-fn' composes
this kill immediately followed by `agent-repl--gui-open', and a
leftover dedicated input window aborts that reopen mid-initialize (the
observed \"webview buffer is null/dead\" cascade).  The input buffer
itself survives — it is workspace furniture, not session state."
  (agent-repl--log ws "gui-kill: ws=%s kill-cause=%s" ws (agent-repl--kill-cause-str))
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
    (agent-repl--log ws "open-panel: selecting gui frontend")
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
         (_ (unless ws (user-error "agent-repl: no current workspace")))
         (buf (agent-repl--ws-get ws :frontend-buffer)))
    (unless (buffer-live-p buf)
      (agent-repl--log ws "close-panel: rejected=no-live-webview")
      (user-error "agent-repl: no webview open for workspace %s" ws))
    (agent-repl--log ws "close-panel: killing buf=%s" (buffer-name buf))
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
    (if (buffer-live-p buf)
        (progn
          (agent-repl--log ws "frontend webview released on nuke: %s" (buffer-name buf))
          (agent-repl--frontend-kill-webview buf))
      (agent-repl--log-verbose ws "frontend webview release: skipped=no-live-webview"))))

(add-hook 'agent-repl-ws-del-hook #'agent-repl--frontend-release-workspace-webview)

(provide 'frontend)

;;; frontend.el ends here
