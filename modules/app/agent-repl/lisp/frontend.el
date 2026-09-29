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

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--fatal "core")
(declare-function agent-repl--info "core")
(declare-function agent-repl--kill-cause-str "core")
(declare-function agent-repl--user-message "core")

(require 'cl-lib)
(require 'url-util)
(require 'url-parse)

(declare-function agent-repl--with-deferred-quit "agent-repl-core")
(declare-function agent-repl--deferred-quit-arm-audit "agent-repl-core" (context))
(declare-function agent-repl--deferred-quit-hand-off "agent-repl-core" (context))
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-live-p "agent-repl-workspace" (ws))
(declare-function agent-repl--ws-gui-frontend-p "agent-repl-frontends" (ws))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--agent-view-buffer-p "agent-repl-core" (&optional buf))
(declare-function agent-repl--buffer-owner "agent-repl-core" (buf))
(declare-function agent-repl--current-ws-p "agent-repl-core" (ws))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key val))
(declare-function agent-repl--align-buffer-to-ws-dir "agent-repl-status" (buf ws))
;; open-progress.el loads after this file; the placeholder API is resolved at
;; call time (see config.el for why that ordering is the right one).
(declare-function agent-repl--open-progress-note "agent-repl-open-progress" (ws phase &optional detail))
(declare-function agent-repl--open-progress-fail "agent-repl-open-progress" (ws detail))
(declare-function agent-repl--open-progress-finish "agent-repl-open-progress" (ws))
(declare-function agent-repl-open-progress-note-loaded "agent-repl-open-progress" (ws))
(declare-function xwidget-get "xwidget" (xwidget propname))
(declare-function xwidget-put "xwidget" (xwidget propname value))
(declare-function agent-repl--read-known-workspace "agent-repl-keybindings" (prompt))
(declare-function agent-repl-window--panel-window "agent-repl-window" (kind &optional ws frame))
(declare-function agent-repl-window--side-window-p "agent-repl-window" (win))
(declare-function agent-repl-window--harden "agent-repl-window" (win &rest recipe))
(declare-function agent-repl-window--input-height "agent-repl-window" (&optional frame ws))
(declare-function agent-repl-window--apply-height "agent-repl-window" (win lines &optional ws))
(declare-function agent-repl--panels-visible-p "agent-repl-panels" ())
(declare-function agent-repl--call-in-background-workspace "workspace" (ws fn))
(declare-function agent-repl--hide-panels "agent-repl-panels" ())
(declare-function agent-repl--ensure-input-buffer "agent-repl-panels" (ws))
(declare-function agent-repl--clear-main-area-for-panels "agent-repl-panels" ())
(declare-function agent-repl--close-buffer-windows "agent-repl-panels" (&rest bufs))
(declare-function agent-repl--restore-fullscreen-config "agent-repl-panels" (ws))
(declare-function agent-repl--save-pre-panel-layout "agent-repl-panels" (ws site))
(declare-function agent-repl--buffer-name "agent-repl-core" (suffix ws))
(declare-function agent-repl--ws-backend-name "agent-repl-backend" (ws))
(declare-function agent-repl--frontend-validate-pair "agent-repl-frontends" (frontend-name backend-name &optional env))
(declare-function agent-repl--frontend-validate-for-ws "agent-repl-frontends" (frontend-name ws))
(declare-function agent-repl--ws-choose-frontend "agent-repl-frontends" (ws name))
(declare-function agent-repl-register-frontend "agent-repl-frontends" (frontend))
(declare-function agent-repl-frontend-create "agent-repl-frontends")
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

;; W2-A's names (host.el) and the transport's connection accessor.
(declare-function agent-repl-host-ref "host" (ws))
(declare-function agent-repl-host-conn "host" (ws))
(declare-function agent-repl-host-vendor-session-id "host" (ws))
(declare-function agent-repl-connect-connection-address "connect" (conn))

;;;; ---- Customization ------------------------------------------------------

(defcustom agent-repl-frontend-buffer-name-format "*agent-frontend-%s*"
  "Format for webview buffer names; %s is the workspace name.
Must NOT collide with `agent-repl-panel-buffer-name-format' — the
panel regexes in core.el key real behavior (bounce, orphan sweep) off
that namespace and the webview must stay outside it."
  :type 'string
  :group 'agent-repl)

(defun agent-repl--frontend-getenv (name)
  "External-boundary wrapper: return environment variable NAME.
The body does nothing but read Emacs's process environment.  URL-building
tests bind `process-environment' so they never depend on ambient host state."
  (getenv name))

(defun agent-repl--frontend-log-level (ws)
  "Return the validated `AGENT_REPL_LOG_LEVEL' for WS's webview URL.
An unset variable means `info', the logging contract's declared default.  Any
present value outside the four-level vocabulary is an invariant violation and
aborts before a webview is created."
  (let ((value (agent-repl--frontend-getenv "AGENT_REPL_LOG_LEVEL")))
    (cond
     ((null value) "info")
     ((member value '("debug" "info" "warn" "error")) value)
     (t
      (agent-repl--fatal
       ws
       "elisp.frontend.log-level: invalid ws=%s variable=AGENT_REPL_LOG_LEVEL value=%S allowed=debug,info,warn,error"
       ws value)))))

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

(defun agent-repl--frontend-kill-webview (buf)
  "Kill webview BUF without the xwidget kill-query prompt.
`xwidget-kill-buffer-query-function' (on `kill-buffer-query-functions')
raises a blocking yes-or-no minibuffer prompt for any buffer holding
xwidgets; every frontend kill site is an INTENTIONAL teardown (rebind,
close-panel, workspace kill), so the prompt is suppressed — left in
place it deadlocks non-interactive callers like the kill hook."
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buf)))

(defun agent-repl--frontend-detach-webview (ws buf)
  "Kill webview BUF and clear WS\='s binding to it, in that order.
The two halves are ONE act: `agent-repl--frontend-ensure-webview-buffer'
reuses whatever `:frontend-buffer' names, so a kill that left the key set
would hand a dead buffer to the next mount, and a cleared key over a live
buffer would leak a WKWebView holding an open WebSocket.  Every site that
takes a workspace\='s webview down for a later remount — close-panel, the
bundle remount, the restart verb\='s bounce — goes through here so neither
half can be forgotten at one of them.

The kill path deliberately does NOT: tombstoning nils the plist keys
itself, and the put would resurrect the record."
  (agent-repl--frontend-kill-webview buf)
  (agent-repl--ws-put ws :frontend-buffer nil))

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

(defun agent-repl--frontend-webview-navigate-widget (xwidget uri)
  "External-boundary wrapper: navigate XWIDGET to URI.
Distinct from `agent-repl--frontend-webview-reload-widget', which can
only re-fetch the address the page already carries: a page running a
SUPERSEDED bundle must be sent to a DIFFERENT address, because the build
identity is folded into the URL precisely so a cache cannot answer a new
build out of an old one \(`agent-repl--frontend-workspace-url').
Re-navigating such a page to its own URI would hand it the same stale
bundle back.  Signals on an empty URI rather than navigating a webview
to nothing.  Body does nothing but the external calls; tests mock via
`cl-letf'.  Registered in `agent-repl--external-boundary-functions'."
  (require 'xwidget)
  (when (or (null uri) (string-empty-p uri))
    (error "agent-repl: refusing to navigate webview to an empty URI (xwidget=%S)" xwidget))
  (xwidget-webkit-goto-uri xwidget uri) ;; ALLOW-EXTERNAL-BOUNDARY
  uri)

(defun agent-repl--frontend-webview-uri (xwidget)
  "External-boundary wrapper: return the URI XWIDGET is currently showing.
READ-ONLY, and deliberately separate from
`agent-repl--frontend-webview-reload-widget', which reads the same URI
only to navigate back to it: the rescue command has to know WHERE a
webview went before deciding whether to act, and asking a navigating
wrapper would mean navigating to find out.  Body does nothing but the
external call; tests mock via `cl-letf'.  Registered in
`agent-repl--external-boundary-functions'."
  (require 'xwidget)
  (xwidget-webkit-uri xwidget)) ;; ALLOW-EXTERNAL-BOUNDARY

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

(defun agent-repl--frontend-adopt-webview-buffer (buf name owner)
  "Make webview BUF an agent-repl panel called NAME owned by OWNER, and return it.
Every mount site (the workspace gui panel) adopts its webview through
here, so the four properties that make a webview OURS never drift
apart:
  - the buffer name is pinned via the buffer-local
    `xwidget-webkit-buffer-name-format' (itself the fixed NAME, with no
    %-constructs), so the webapp's `document.title' changes never rename it;
  - `xwidget-webkit-mode's \"WebKit: <title>\" header-line is cleared,
    since the webview is a panel, not a browser;
  - `agent-repl--owning-workspace' records OWNER, the workspace name whose
    REPL this webview shows.

OWNER is REQUIRED, not optional: the ownership stamp is what every
owner-keyed predicate reads, and the one that matters most is
`agent-repl--foreign-owned-buffer-p' — the screen
`agent-repl--clean-frame-foreign-windows' uses to decide which windows a
workspace may tear down.  An unstamped webview reads as owned by nobody,
so a background panel build was free to take the window the user was
watching ANOTHER workspace's page in and mount its own page there.
Passing the owner at the sole adoption chokepoint is what makes an
unowned workspace webview unrepresentable rather than merely unlikely."
  (with-current-buffer buf
    (setq-local agent-repl--owning-workspace owner)
    (setq-local xwidget-webkit-buffer-name-format name)
    (setq-local header-line-format nil)
    (rename-buffer name t)
    ;; Last, so consumers see a fully adopted buffer (final name, mode armed).
    ;; Wrapped because a decoration that fails must not cost the user a
    ;; webview; the failure is surfaced through the log rather than swallowed.
    (condition-case err
        (run-hooks 'agent-repl-frontend-webview-adopt-hook)
      (error
       (agent-repl--warn owner "frontend webview adopt-hook failed buffer=%s err=%S"
                        name err))))
  buf)

(defun agent-repl--frontend-remap-stock-reload ()
  "Point this webview buffer's stock reload keys at our own reload command.

Run from `agent-repl-frontend-webview-adopt-hook' with the adopted
webview buffer current.  The stock `xwidget-webkit-history-reload' (bound
to \\`g' in `xwidget-webkit-mode-map') calls
`xwidget-webkit-back-forward-list', which is VOID in this Emacs build:
pressing the stock reload key in one of our panels signals \"Symbol's
function definition is void\" instead of reloading.  The sibling
`xwidget-webkit-reload' walks history at offset zero, which is also not
what a redeploy needs.

Both are remapped, buffer-locally, to
`agent-repl-frontend-reload-webview' — the command that re-navigates the
panel to its workspace URL, the correct reload after the daemon redeploys
the bundle.  A fresh keymap parented on the live local map carries the
remap, so ONLY our panels are affected; plain `xwidget-webkit-mode'
buffers elsewhere keep the stock (broken) bindings untouched, and every
other stock xwidget binding still resolves through the parent."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (define-key map [remap xwidget-webkit-history-reload]
                #'agent-repl-frontend-reload-webview)
    (define-key map [remap xwidget-webkit-reload]
                #'agent-repl-frontend-reload-webview)
    (use-local-map map)))

(add-hook 'agent-repl-frontend-webview-adopt-hook
          #'agent-repl--frontend-remap-stock-reload)

(defun agent-repl--frontend-watch-load (ws buf)
  "Report BUF's load-finished events for WS to the open-progress ladder.

The xwidget event handler calls the widget's own `callback', so the
existing one is WRAPPED rather than replaced: the webkit machinery still
gets every event it needs, and the ladder gets the ONE report that a
page actually finished loading.  This is the only signal in the ladder
that comes from the page at all, and it is a fact the widget emits —
nothing is asked of the page, which is what makes it trustworthy for
diagnosing a page too broken to answer a question.

THE LOAD REPORT IS AN INFO RECORD, and that rung is load-bearing.  It is
the ONE marker every painted panel leaves, whatever brought the workspace
into being: every mount arrives through
`agent-repl--frontend-ensure-webview-buffer' and every mount arms this
watcher.  On the DEBUG rung it never reached disk at all, because the
default `AGENT_REPL_LOG_LEVEL' is `info' — so realtest 5's created
workspace painted its panel (`elisp.frontend.gui-open: displayed', with
the page's own boot record beside it) while the run's harvest held no
evidence of any load and reported a workspace the owner could not work
in.  The pre-creation drain's `precreate-created' cannot cover for it: a
created workspace is mounted and DISPLAYED by the open path first, so
pre-creation refuses it `:already-mounted' and never writes a record
naming it.  One record per mount is the whole cost.

No-op when BUF holds no live widget."
  (let ((widget (agent-repl--frontend-webview-live-widget buf)))
    (if (not widget)
        (agent-repl--log ws "elisp.frontend.watch-load: skipped ws=%s reason=no-widget" ws)
      (let ((prior (xwidget-get widget 'callback)))
        (xwidget-put
         widget 'callback
         (lambda (xwidget event-type)
           (when (eq event-type 'load-changed)
             (agent-repl--info ws "elisp.frontend.watch-load: load-changed ws=%s" ws)
             (when (fboundp 'agent-repl-open-progress-note-loaded)
               (agent-repl-open-progress-note-loaded ws)))
           (when (functionp prior) (funcall prior xwidget event-type))))
        (agent-repl--log ws "elisp.frontend.watch-load: armed ws=%s" ws)))))

(defun agent-repl--frontend-ensure-webview-buffer (ws url)
  "Return a live webview buffer for WS at URL.
Reuses the recorded `:frontend-buffer' while it is live.  WS's webview
URL addresses the WORKSPACE, so it names the same thing for as long as
the workspace exists and a live buffer never needs retargeting — which
is fortunate, because an xwidget session cannot be retargeted reliably
from outside.  A freshly mounted buffer is handed to
`agent-repl--frontend-adopt-webview-buffer', which pins its name, drops
the browser header-line, and arms the copy chords.

Whichever buffer is returned — reused or freshly mounted — its
`default-directory' is realigned to WS's project root via
`agent-repl--align-buffer-to-ws-dir', so `SPC .' from the webview window
resolves against the worktree the REPL is attached to rather than the
foreign directory the xwidget session inherited at creation."
  ;; This is the one rendering choke point: every initial mount and remount
  ;; arrives here.
  ;;
  ;; NO HEALTH PROBE.  A mount used to poll session health first, because the
  ;; establishment command acked as soon as a spawn was issued and the mount
  ;; had no other way to know the shim was up.  The daemon acks only once the
  ;; session is ESTABLISHED — its shim answered a health probe healthy over
  ;; the fully wired connection — so a probe here could only re-ask a question
  ;; already answered (and lose the race it kept losing).
  ;;
  ;; THE MOUNT IS ONE STEP, NOT THREE.  Creating the WKWebView, adopting it
  ;; (name pinned, header-line dropped, chords armed) and binding it to the
  ;; workspace are what makes a webview belong to WS.  A `C-g' landing
  ;; between them leaves a live xwidget buffer no workspace holds — invisible
  ;; to `gui-kill', so it is never released — or an adopted-but-unbound
  ;; buffer the next open mounts a SECOND webview beside.  Quit is held off
  ;; until the registry names the buffer it just created.
  ;;
  ;; THROUGH `agent-repl--with-deferred-quit' RATHER THAN A BARE
  ;; `inhibit-quit', so the deferral is RECORDED and the armed flag is handed
  ;; on rather than left to whatever checkpoint comes next.  A bare binding
  ;; makes a quit that arrived here indistinguishable, from the log alone,
  ;; from a quit that never arrived.
  ;;
  ;; It is NOT what the 2026-09-12 / 2026-09-13 "a real C-g did not dismiss
  ;; the prompt" finding turned out to be.  That was the module's own quit
  ;; DELIVERY timer clearing `quit-flag' and then failing to abort; the guard
  ;; itself only ever left the flag armed, and this mount logged no deferral
  ;; in any of those runs.  core.el's commentary carries the Emacs source
  ;; that settles it.
  (let* ((existing (agent-repl--ws-get ws :frontend-buffer))
         (buf (if (buffer-live-p existing)
                  (progn
                    (agent-repl--log ws "ensure-webview: outcome=reused buf=%s"
                                     (buffer-name existing))
                    existing)
                (agent-repl--with-deferred-quit "frontend-ensure-webview"
                  (let* ((buf (agent-repl--frontend-make-webview-buffer url))
                         (name (agent-repl--frontend-webview-buffer-name ws)))
                    (agent-repl--frontend-adopt-webview-buffer buf name ws)
                    (agent-repl--ws-put ws :frontend-buffer buf)
                    (agent-repl--frontend-watch-load ws buf)
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
    ;; Save the pre-panel layout before mounting: the gui hide/close
    ;; paths restore it, which is what removes BOTH gui windows
    ;; (deleting them directly is impossible once the input window is
    ;; the frame's sole survivor).
    ;;
    ;; What is skipped is a mount that still COVERS THE FRAME: a re-show
    ;; over standing panels, or the reconciler remounting over the
    ;; remains of the pair.  In both the layout already saved is the one
    ;; underneath, and clobbering it would lose the user's real
    ;; pre-panel frame.
    ;;
    ;; Anything else on the frame means the saved configuration no
    ;; longer describes it — `delete-other-windows' from the composer
    ;; leaves the delete-protected composer alone with a landing-era
    ;; configuration behind it, and the user then builds their own
    ;; windows on top — so it is REPLACED here by the layout actually
    ;; being covered now.  Keeping it instead is what used to make the
    ;; next close restore the landing over the user's own windows.  The
    ;; same rule reads the key on the restore side
    ;; (`agent-repl--fullscreen-config-stale-p').
    ;;
    ;; Saved AFTER the stale-input-window reclaim above so a leftover
    ;; composer window is not baked into the layout a later close
    ;; restores.
    (agent-repl--save-pre-panel-layout ws "display-webview")
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
      ;;
      ;; The composer's height is the frame's, not this window's: it
      ;; comes from `agent-repl-window--input-height', which derives it
      ;; once per frame geometry.  Splitting by a fraction of WIN — what
      ;; this did — made the height depend on what the frame looked like
      ;; at the instant of the mount, so workspaces disagreed with each
      ;; other and a remount changed one workspace's composer under the
      ;; user.  The resize after the split is the check that the frame
      ;; granted the lines the split asked for, and it runs BEFORE the
      ;; hardening that locks the height.
      (let* ((target-height (agent-repl-window--input-height (window-frame win) ws))
             (input-win (split-window win (- target-height) 'below))
             (actual-height nil))
        (set-window-buffer input-win input-buf)
        (setq actual-height (agent-repl-window--apply-height input-win target-height ws))
        (agent-repl-window--harden input-win
                                   :dedicate       t
                                   :size-fix       'height
                                   :delete-protect t
                                   :preserve-size  'height)
        (select-window input-win)
        (agent-repl--log ws "display-webview: mounted webview-window=%s input-window=%s input-height=%s target-height=%s"
                         win input-win actual-height target-height))))
  buf)

;;;; ---- Entry point ----------------------------------------------------------------

(defun agent-repl--gui-open (ws)
  "The gui frontend's open capability (registry `:open-fn').
Mounts WS's webview at its URL and places it over the input panel.

THERE IS NO SESSION TO ENSURE.  The daemon starts or revives a
workspace's session implicitly, and the page connects to whatever state
the workspace is in and draws it honestly, so the mount waits on
nothing.  What it does need is WS's ref — the URL is built from it —
which the roster supplies the moment the workspace exists.

The mount runs through `agent-repl--call-in-background-workspace', which
activates WS for the duration of the display and restores the caller's
focus afterward: `--display-webview' deletes and re-lays the frame's
main-area windows, so a mis-anchored mount would evict the looked-at
workspace's layout.  The anchor is inert when WS is already current."
  (agent-repl--log ws "elisp.frontend.gui-open: begin ws=%s" ws)
  (agent-repl--frontend-require-xwidget ws)
  (agent-repl--frontend-validate-for-ws 'gui ws)
  (agent-repl--open-progress-note ws :acked)
  (agent-repl--call-in-background-workspace
   ws
   (lambda ()
     (let* ((url (agent-repl-frontend-webview-url ws))
            (buf (agent-repl--frontend-ensure-webview-buffer ws url)))
       (agent-repl--frontend-display-webview ws buf)
       ;; The real panel is on the frame; the placeholder has nothing left
       ;; to say and goes away in the same call that replaced it.
       (agent-repl--open-progress-finish ws)
       (agent-repl--info ws "elisp.frontend.gui-open: displayed ws=%s buffer=%s"
                         ws (and (buffer-live-p buf) (buffer-name buf)))
       buf))))

(defun agent-repl--gui-boot (ws &optional _project-dir-hint _active-env-hint)
  "The gui frontend's boot capability (registry `:boot-fn').
Pre-creates WS's page in the BACKGROUND — no window is touched, because
the birth and restore paths run in the CALLER's frame and mounting a
webview here would evict the user's windows.  The view arrives later,
when the user switches to WS and the `:pending-show-panels' drain shows
it through the frontend.

NO LOCAL STATE IS WRITTEN and no session is started: both are the
daemon's, and a colour or a session Emacs invented here would be a
second answer that outlives the daemon's own.

The hints are unused: the gui reads WS's directory from its ref."
  (agent-repl--frontend-validate-for-ws 'gui ws)
  (agent-repl--log ws "elisp.frontend.gui-boot: begin ws=%s" ws)
  (agent-repl--frontend-precreate-webview ws))

(defun agent-repl--frontend-precreate-webview (ws)
  "Create WS's webview buffer WITHOUT displaying or selecting it.
The non-displaying twin of `agent-repl--gui-open': it stops at the mount
\(`agent-repl--frontend-ensure-webview-buffer'), never reaching
`agent-repl--frontend-display-webview'.  The frame's windows and the
current perspective are therefore untouched — the buffer simply exists,
addressed at the served bundle.

WHY EAGERLY.  Mounting a WKWebView and loading the webapp is the slow
part of opening a workspace, and doing it at first look puts that cost
in front of the user every time.  Pre-creation is blessed performance
machinery, and it is STAGGERED (webview-recovery.el) so a link-up does
not mount every workspace at once.

Returns `:created' when a mount happened and nil when WS is not entitled
to one; the refusals are preconditions, not failures (see
`agent-repl--frontend-precreate-refusal')."
  (let ((refusal (agent-repl--frontend-precreate-refusal ws)))
    (if refusal
        (progn
          (agent-repl--log ws "elisp.frontend.precreate: skipped ws=%s reason=%s" ws refusal)
          nil)
      (agent-repl--log ws "elisp.frontend.precreate: begin ws=%s" ws)
      (agent-repl--frontend-precreate-mount ws)
      :created)))

(defun agent-repl--frontend-precreate-mount (ws)
  "Mount WS's webview buffer without displaying it, returning the buffer.
Anchored the same way the open path anchors, so the buffer is born into
WS's perspective rather than whichever one is current when the mount
runs.  No window is touched either way."
  (agent-repl--call-in-background-workspace
   ws
   (lambda ()
     (let ((buf (agent-repl--frontend-ensure-webview-buffer
                 ws (agent-repl-frontend-webview-url ws))))
       (agent-repl--log ws "elisp.frontend.precreate: mounted ws=%s buffer=%s"
                        ws (and (buffer-live-p buf) (buffer-name buf)))
       buf))))

(defun agent-repl--frontend-precreate-refusal (ws)
  "Return the keyword naming why WS may NOT be pre-created, or nil when it may.

THE ONE eligibility answer, shared by the mount
\(`agent-repl--frontend-precreate-webview') and the pre-creation queue
\(`agent-repl--webview-precreate-needed-p') so the two can never
disagree about which workspaces are owed a page.

Refusals, all preconditions rather than failures:

  - `:not-live'          a dead or killed workspace;
  - `:not-gui'           a workspace whose frontend is not the web gui;
  - `:no-ref'            no `WorkspaceRef' yet, so no URL exists to
                         mount — the roster has not reached this
                         workspace, and a guessed URL is not an option;
  - `:already-mounted'   a live webview buffer already exists (this is
                         what makes pre-creation idempotent);
  - `:no-xwidget'        an Emacs with no xwidget support has no webview
                         to make."
  (cond
   ((not (agent-repl--ws-live-p ws)) :not-live)
   ((not (agent-repl--ws-gui-frontend-p ws)) :not-gui)
   ((null (and (fboundp 'agent-repl-host-ref) (agent-repl-host-ref ws))) :no-ref)
   ((buffer-live-p (agent-repl--ws-get ws :frontend-buffer)) :already-mounted)
   ((not (agent-repl--frontend-xwidget-available-p)) :no-xwidget)))

(defun agent-repl--frontend-page-host-label (id)
  "Return the DNS label that names workspace ID's page host.
A real workspace id is already a valid label (lowercase hex), and is used
as is so the host reads as the workspace in any log.  Anything else is
hashed, because the label only has to be STABLE and DISTINCT per
workspace — it is never parsed back into an id."
  (if (string-match-p "\\`[a-z0-9-]\\{1,60\\}\\'" id)
      id
    (substring (secure-hash 'sha1 id) 0 16)))

(defun agent-repl--frontend-page-origin (ws conn)
  "Return the origin WS's page is served from over CONN, or signal.

  http://ws-<id>.localhost:<daemon port>

EACH WORKSPACE'S PAGE GETS A HOST OF ITS OWN, and that is what keeps the
page's requests moving.  WebKit caps HTTP/1.1 at six connections PER
HOST, and the cap is shared by every page of that host in the one
networking process — not per page.  Each page holds exactly one standing
stream (`WatchPage'), so six pages on the daemon's bare address pinned
all six connections, and every later request from every page — its log
forwarding, its cold-gate answer, every unary verb — queued forever with
nothing on the wire and no error.  A host per workspace gives each page
its own six, which is what the one-stream-per-page rule was written
assuming.

`*.localhost' resolves to loopback in WebKit (RFC 6761), the daemon does
not check the Host header, and the page addresses the daemon through its
own origin, so nothing but this string changes.  The daemon's address
must be loopback: a `localhost' name for any other host would address
this machine instead."
  (let* ((address (agent-repl-connect-connection-address conn))
         (colon (and address (string-match ":\\([0-9]+\\)\\'" address)))
         (host (and colon (substring address 0 (match-beginning 0))))
         (port (and colon (match-string 1 address)))
         (ref (and (fboundp 'agent-repl-host-ref) (agent-repl-host-ref ws))))
    (unless (and host (member host '("127.0.0.1" "localhost")))
      (agent-repl--fatal ws "elisp.frontend.page-origin: the daemon address %S is not a loopback host:port" address))
    (unless ref
      (agent-repl--fatal ws "elisp.frontend.page-origin: no ref for ws=%s" ws))
    (format "http://ws-%s.localhost:%s"
            (agent-repl--frontend-page-host-label (plist-get ref :id)) port)))

(defun agent-repl-frontend-webview-url (ws)
  "Return the webapp URL WS's webview loads.

  http://ws-<id>.localhost:<daemon port>/?workspace=<id>&dir=<dir>&log_level=<level>

The host is the workspace's own (`agent-repl--frontend-page-origin' says
why); the port is the owning daemon's.

THE WORKSPACE VALUES COME FROM THE WorkspaceRef VERBATIM — the one the daemon
minted and handed back (RegisterWorkspace's answer, or the roster) —
URL-encoded and nothing else.  The id is opaque and compared byte-wise,
so it is echoed, never constructed from a path; the dir rides along for
display and for opening files, and is likewise never parsed.

THE LOG LEVEL IS THE ONE PIECE OF DAEMON CONFIGURATION THAT RIDES THE URL.
JavaScript cannot read the daemon process's environment, so the host carries
the effective `AGENT_REPL_LOG_LEVEL' into the page's boot address.  An unset
variable becomes the logging contract's explicit `info' default; an invalid
present value fails before the webview is created.

Nothing else rides the URL.  There is no composer flag (the webapp runs
composer-less unless `&composer=1', which only dev mode and the webapp's
own tests use) and no parent_ws (the daemon resolves and draws
parentage).  A query parameter here would be a second, drifting channel
for facts the daemon already pushes.

The ADDRESS is the address of the connection that owns WS
\(`agent-repl-host-conn'), not a global one: during a daemon handover a
workspace's webview must load from whichever daemon currently owns it.

Signals through `agent-repl--fatal' when WS has no ref or no connection:
a URL invented without either would address the wrong daemon or the
wrong workspace, and a webview pointed at the wrong workspace is worse
than no webview."
  (let ((ref (and (fboundp 'agent-repl-host-ref) (agent-repl-host-ref ws)))
        (conn (and (fboundp 'agent-repl-host-conn) (agent-repl-host-conn ws))))
    (unless ref
      (agent-repl--fatal ws "elisp.frontend.webview-url: no ref for ws=%s" ws))
    (unless conn
      (agent-repl--fatal ws "elisp.frontend.webview-url: no connection for ws=%s" ws))
    (let ((url (format "%s/?workspace=%s&dir=%s&log_level=%s"
                       (agent-repl--frontend-page-origin ws conn)
                       (url-hexify-string (plist-get ref :id))
                       (url-hexify-string (plist-get ref :dir))
                       (url-hexify-string (agent-repl--frontend-log-level ws)))))
      (agent-repl--log ws "elisp.frontend.webview-url: ws=%s url=%s" ws url)
      url)))

;; THERE IS NO WEBVIEW RETARGETING.  The URL addresses the WORKSPACE, so a
;; session rotating, being superseded or being re-created under this view
;; leaves the URL naming the same thing, and the page re-reads the change off
;; the daemon's pushed views on its own connection.  A webview is BOUND TO ITS
;; WORKSPACE BUFFER FOR LIFE.

(defun agent-repl-frontend-reload-webview (&optional ws)
  "Navigate WS's webview to its current URL, reloading the served webapp.

The reaction to WatchHostWorkspace's `reload_webapp' push — rebuilt
webapp assets are deployed and this workspace's xwidget must reload
against the SAME daemon.  The reloaded webview's default first-page load
is the whole of the recovery: the daemon serves the bundle off disk, so
navigating the live widget to the URL again fetches the new assets
\(their content-hashed names make it a guaranteed cache miss).

The WIDGET IS NAVIGATED, never remounted: the webview is bound to its
buffer for life, and killing the buffer to make a new one would break
that binding for a rollout the page can simply re-fetch.

WS defaults to the current workspace, so the interactive command
\(`SPC o l') and the push arm share one implementation.  Returns the
xwidget on success and nil when WS has no live webview — a workspace
with no page has nothing to reload, and its next open mounts fresh."
  (interactive)
  (let* ((ws (or ws (agent-repl--ws-current-name)))
         (buf (and ws (agent-repl--ws-get ws :frontend-buffer)))
         (widget (and (buffer-live-p buf)
                      (agent-repl--frontend-webview-live-widget buf))))
    (cond
     ((null ws)
      (user-error "agent-repl: no current workspace"))
     ((null widget)
      (agent-repl--log ws "elisp.frontend.reload-webview: skipped ws=%s reason=no-live-webview" ws)
      nil)
     (t
      (agent-repl--frontend-webview-navigate-widget
       widget (agent-repl-frontend-webview-url ws))
      (agent-repl--info ws "elisp.frontend.reload-webview: navigated ws=%s" ws)
      widget))))

;;;; ---- Rescuing a webview that navigated away --------------------------------

(defun agent-repl--frontend-webview-current-uri (ws)
  "Return the URI WS's open webview is currently displaying, or nil.
nil covers both \"no webview open\" and \"the mounted WKWebView is
dead\": neither is a page that can be inspected, and the caller
distinguishes them by asking for the buffer itself."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (when (buffer-live-p buf)
      (when-let ((xw (agent-repl--frontend-webview-live-widget buf)))
        (agent-repl--frontend-webview-uri xw)))))

(defun agent-repl--frontend-home-origin (ws)
  "Return the origin WS's webapp is served from, or nil.

The origin is the address of the connection whose daemon OWNS WS
\(`agent-repl-host-conn'), which is the same source
`agent-repl-frontend-webview-url' builds its URL from — so home and the
page the rescue navigates back to can never name two different daemons.
There is no global base URL to ask instead: a workspace handed over to
another daemon is served by THAT daemon, and a module-wide address would
call its perfectly-correct page stray.

Nil when WS has no connection or no ref: a workspace whose daemon or
page host cannot be named cannot certify any page as home."
  (let ((conn (and (fboundp 'agent-repl-host-conn) (agent-repl-host-conn ws)))
        (ref (and (fboundp 'agent-repl-host-ref) (agent-repl-host-ref ws))))
    (when (and conn ref)
      (agent-repl--frontend-page-origin ws conn))))

(defun agent-repl--frontend-webview-at-home-p (ws uri)
  "Return non-nil when URI is served by the daemon that owns WS.
Home is that daemon's ORIGIN (`agent-repl--frontend-home-origin'), not
the workspace's full webview URL: the page rewrites its own query as the
user navigates the webapp, and the build stamp in a freshly built URL
differs from the one a still-correct mounted page carries, so comparing
whole URLs would call a perfectly-at-home webview stray.  What the
rescue actually detects is the page having left the daemon entirely.

An unknown or empty URI is NOT home: a webview that cannot say where it
is has no claim on being left alone.  Neither is any URI when WS has no
connection to be at home on."
  (and (stringp uri)
       (not (string-empty-p uri))
       (when-let ((origin (agent-repl--frontend-home-origin ws)))
         (let ((there (url-generic-parse-url uri))
               (home (url-generic-parse-url origin)))
           (and (equal (url-type there) (url-type home))
                (equal (url-host there) (url-host home))
                (equal (url-port there) (url-port home))
                t)))))

(defun agent-repl--frontend-webview-host (uri)
  "Return a short host label for URI, for user copy.
Falls back to the whole URI when it parses to no host, and to
\"an unknown page\" when there is no URI at all — the echo line names
where the view went, and a hole in that sentence is worse than a
coarser answer."
  (or (and (stringp uri)
           (not (string-empty-p uri))
           (or (url-host (url-generic-parse-url uri)) uri))
      "an unknown page"))

;;;###autoload
(defun agent-repl-frontend-rescue-webview (&optional ws)
  "Bring a webview that navigated away from the webapp back home.
Clicking an external hyperlink inside the webapp navigates the xwidget
itself, so the workspace's panel ends up rendering some other site with
no way back — the page is gone, and with it every control that could
return it.  This is the way back: it reports WHERE the view went and
navigates it back to its own
workspace's URL (`agent-repl-frontend-reload-webview', the same
operation the rollout reload uses).

WS defaults to the current workspace.  With a prefix argument the
target workspace is read interactively, and non-interactive callers may
pass a name directly.

A webview that is already home is left ALONE — no remount, just a line
saying so.  Remounting it anyway would throw away a rendered feed to
navigate to where it already is.  Signals when the workspace has no
webview open at all, matching `agent-repl-frontend-reload-webview'."
  (interactive
   (list (when current-prefix-arg
           (agent-repl--read-known-workspace "Rescue webview for workspace: "))))
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (unless ws
      (user-error "agent-repl: no current workspace"))
    (unless (buffer-live-p (agent-repl--ws-get ws :frontend-buffer))
      (user-error "agent-repl: no webview open for workspace %s" ws))
    (let ((uri (agent-repl--frontend-webview-current-uri ws)))
      (if (agent-repl--frontend-webview-at-home-p ws uri)
          (progn
            (agent-repl--log ws "rescue-webview: outcome=already-home")
            (agent-repl--user-message ws "webview is already home" nil
                                      :detail (format "uri=%s" uri))
            nil)
        (agent-repl--log ws "rescue-webview: outcome=astray url=%s" uri)
        (prog1 (agent-repl-frontend-reload-webview ws)
          (agent-repl--user-message
           ws "webview brought home from %s"
           (list (agent-repl--frontend-webview-host uri))
           :detail (format "stray-uri=%s" uri)))))))

(defun agent-repl--gui-show (ws)
  "The gui frontend's show capability (registry `:show-fn').
Displays the live webview, or opens fresh when it died.

NOTHING IS WOKEN FIRST.  Hibernation does not exist on the wire: a
parked workspace presents as live with `shim_attached' false, its
composer stays open, and typing revives it under the hood — so there is
no bring-up for a show to wait on, and the page connects and draws
whatever state the workspace is in."
  (agent-repl--open-progress-note ws :acked)
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (if (buffer-live-p buf)
        (progn
          (agent-repl--frontend-display-webview ws buf)
          (agent-repl--open-progress-finish ws)
          (agent-repl--log ws "elisp.frontend.gui-show: displayed ws=%s" ws)
          buf)
      ;; The webview died under a workspace we believed running.  The open
      ;; path owns the placeholder from here — including its teardown — so
      ;; nothing is resolved on this branch.
      (agent-repl--log ws "elisp.frontend.gui-show: remounting ws=%s reason=no-live-webview" ws)
      (agent-repl--gui-open ws))))

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

(defun agent-repl--gui-running-p (ws)
  "The gui frontend's liveness capability (registry `:running-p-fn').

Non-nil when WS has a MOUNTED WEBVIEW — a page that exists and can be
put back on the frame — which is exactly the question the toggle asks it
(`agent-repl--toggle'): a running frontend is SHOWN
\(`agent-repl--gui-show'), one that is not is OPENED
\(`agent-repl--gui-open').  A workspace whose panels were merely hidden
by the plain close still holds its buffer, so it is shown rather than
mounted a second time; one that was never opened, or whose webview was
killed, holds none and is opened.

IT IS NOT A QUESTION ABOUT THE DAEMON'S SESSION, and deliberately: a
parked workspace presents as live with `shim_attached' false and its
page draws whatever state it is in, so session liveness would answer a
question neither branch of the toggle asks."
  (and (buffer-live-p (agent-repl--ws-get ws :frontend-buffer)) t))

(defun agent-repl--gui-durable-session-id (ws)
  "The gui frontend's durable-session capability (`:durable-session-id-fn').

Answers the VENDOR conversation id — the claude session uuid a resume
replays — for WS, or nil.  The gui holds no session state of its own to
answer from: the daemon owns the session and publishes its vendor
identity on the host stream, so this reads
`agent-repl-host-vendor-session-id' and nothing else.  A second source
would be a second answer, and the two would disagree the moment a
session rotates.

Nil is an ANSWER, not a failure: a workspace with no live session, or
one whose vendor conversation has not started yet, durably identifies no
conversation."
  (agent-repl-host-vendor-session-id ws))

(defun agent-repl--gui-kill (ws)
  "The gui frontend's kill capability (registry `:kill-fn').
Tears down the LAYOUT first (webview + dedicated input windows), then
kills the webview.  The window teardown is contractual: the registry's
`:restart-fn' composes this kill immediately followed by
`agent-repl--gui-open', and a leftover dedicated input window aborts that
reopen mid-initialize (the observed \"webview buffer is null/dead\"
cascade).  The input buffer itself survives — it is workspace furniture,
not session state.

THE DAEMON SESSION IS LEFT ALONE, and that is the whole point of killing
only the layout: the daemon locates a session by cwd, so a reopened
workspace reattaches to the same record and the conversation comes back.
A teardown that ended the session would stamp its record with a death
reason `resume-resolve' reads as the user discarding the CONVERSATION,
which is not what closing a panel says."
  (agent-repl--log ws "gui-kill: ws=%s kill-cause=%s" ws (agent-repl--kill-cause-str))
  (agent-repl--gui-hide ws)
  (agent-repl--frontend-release-workspace-webview ws)
  (agent-repl--ws-put ws :frontend-buffer nil))

(agent-repl-register-frontend
 (agent-repl-frontend-create
  :name 'gui
  :open-fn #'agent-repl--gui-open
  :boot-fn #'agent-repl--gui-boot
  :kill-fn #'agent-repl--gui-kill
  ;; NO `:cancel-detached-fn'.  Stopping DETACHED work is the `Interrupt'
  ;; verb's `all_agents' and `detached' targets, and `Interrupt' is a FEED
  ;; verb: the `detached' target is named by `frontend.v1.FeedId',
  ;; vocabulary Emacs has no feed to hold, so the registry offers no
  ;; per-frontend cancel of detached work and the fan-wide stop stays the
  ;; webapp footer's, on the very page this frontend mounts.
  ;;
  ;; Emacs DOES call `Interrupt' for the `turn' target: `C-c C-k' in the
  ;; composer (`agent-repl-interrupt-turn', verbs.el) is the clean
  ;; turn stop, restored by owner order after the overhaul first routed the
  ;; whole gesture to the footer.  The turn target needs no feed vocabulary
  ;; — it names the running vendor query, not a bubble — so it reaches the
  ;; daemon where a detached-work interrupt from Emacs could not.
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
  ;; NO `:adopt-session-fn'.  No verb in the post-overhaul contract binds a
  ;; workspace to a vendor session uuid a client names: the daemon owns
  ;; session identity and resumes a workspace's own conversation from its
  ;; own record, and the cross-frontend switch that used to hand a uuid
  ;; across is gone with frontend-client.el.  A frontend that could adopt
  ;; would fill the slot; the gui cannot, and says so by leaving it unset.
  ))

;;;###autoload
(defun agent-repl-frontend-open-panel ()
  "Open the web frontend for the current workspace's session.
Dispatches the gui open capability and, once it is accepted, records
`gui' as the workspace's DELIBERATE frontend choice (asking for the web
panel by name is a choice, so it outlives a restart).  The unified
command surface — `SPC o c' and friends — reaches the same place through
the frontend registry.

THE CHOICE IS PERSISTED ONLY BY AN OPEN THAT WAS ACCEPTED.  A refused
open leaves NO durable trace: `agent-repl--gui-open' signals a
`user-error' synchronously when the build has no xwidget support or the
frontend cannot drive this workspace, and persisting ahead of it would
pin a workspace to a frontend it was never able to show — silently, and
across restarts, with nothing in the workspace to explain why.
The open is synchronous now — there is no session to establish first —
so the write below lands only after a mount that actually happened."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws
      (user-error "agent-repl: no current workspace"))
    (agent-repl--log ws "open-panel: selecting gui frontend")
    (agent-repl--frontend-validate-for-ws 'gui ws)
    (prog1 (agent-repl--gui-open ws)
      (agent-repl--ws-choose-frontend ws 'gui))))

;;;###autoload
(defun agent-repl-frontend-close-panel ()
  "Kill the current workspace's webview buffer (the session stays alive).
The daemon session is NOT deleted — reopening the panel reattaches to
it with full replayed history; session teardown belongs to the
workspace kill path (`agent-repl-ws-del-hook')."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (_ (unless ws (user-error "agent-repl: no current workspace")))
         (buf (agent-repl--ws-get ws :frontend-buffer)))
    (unless (buffer-live-p buf)
      (agent-repl--log ws "close-panel: rejected=no-live-webview")
      (user-error "agent-repl: no webview open for workspace %s" ws))
    (agent-repl--log ws "close-panel: killing buf=%s" (buffer-name buf))
    (agent-repl--frontend-detach-webview ws buf)
    (agent-repl--user-message ws "webview closed (session kept)" nil)))

;;;; ---- Workspace teardown -----------------------------------------------------

(defun agent-repl--frontend-release-workspace-webview (ws)
  "Kill WS's webview buffer on kill (for `agent-repl-ws-del-hook').
Tombstoning only nils the plist keys — without this the buffer (a live
WKWebView holding an open WebSocket) would outlive the workspace.
Runs pre-tombstone, while `:frontend-buffer' is still readable."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (if (buffer-live-p buf)
        (progn
          (agent-repl--log ws "frontend webview released on kill: %s" (buffer-name buf))
          (agent-repl--frontend-kill-webview buf))
      (agent-repl--log-verbose ws "frontend webview release: skipped=no-live-webview"))))

(add-hook 'agent-repl-ws-del-hook #'agent-repl--frontend-release-workspace-webview)

(provide 'frontend)

;;; frontend.el ends here
