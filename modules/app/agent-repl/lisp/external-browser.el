;;; external-browser.el --- Open every hyperlink in the external Chrome profile -*- lexical-binding: t; -*-

;;; Commentary:

;; Every hyperlink opens in ONE place: the Google Chrome window signed in
;; as dodge@chess.com, brought to the front.  Never an Emacs
;; `xwidget-webkit' buffer, never whatever `browse-url' would otherwise
;; pick, and never the dodge.w.coates@gmail.com profile window.
;;
;; Two link surfaces exist, and both are covered:
;;
;;   - EMACS.  `browse-url-browser-function' is pinned to
;;     `agent-repl-browse-url-external' (installed below), so org links,
;;     magit's github/PR links, help buttons, `browse-url-at-point', and
;;     every other Emacs-side visit routes through this file.
;;
;;   - THE GUI WEBAPP.  Response bubbles render markdown anchors with
;;     `target="_blank"', and a click on one inside the xwidget makes
;;     WebKit ask Emacs for a new webview buffer.  The webapp therefore
;;     cancels the click BEFORE it becomes a navigation
;;     (webapp/src/external-link.ts) and POSTs the URL to the daemon,
;;     which runs the same Chrome invocation from Go
;;     (daemon/internal/externalbrowser).  Cancelling the click is what
;;     makes an in-webview navigation impossible rather than merely
;;     unlikely -- there is no after-the-fact "navigate back" recovery.
;;
;; PROFILE SELECTION.  macOS `open -a Foo --args ...' DROPS the arguments
;; whenever Foo is already running, so `--profile-directory' would be
;; honored on a cold launch and silently ignored on every click after it.
;; Chrome's own executable is invoked directly instead: it hands the URL
;; to the already-running browser over Chrome's singleton socket TOGETHER
;; with the requested profile, which is the only invocation that lands the
;; tab in a specific profile's window reliably.
;;
;; Every shell-out goes through the single external-boundary wrapper
;; `agent-repl--external-browser-call-process', registered in
;; `agent-repl--external-boundary-functions' (core.el) so the batch test
;; harness stubs the boundary instead of launching a real browser.

;;; Code:

(require 'browse-url)
(require 'seq)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))

(defcustom agent-repl-external-browser-binary
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  "Absolute path to the Chrome executable hyperlinks are handed to.
The executable is invoked DIRECTLY rather than through macOS `open',
because `open' drops `--args' for an already-running app and the profile
would stop being honored after the first link of the session."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-external-browser-profile "Profile 6"
  "Chrome profile directory every hyperlink opens in.
\"Profile 6\" is the dodge@chess.com login window; the
dodge.w.coates@gmail.com login window is \"Profile 7\".  The value is the
on-disk directory name under Chrome's user-data dir, which is what
`--profile-directory' takes -- not the account address."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-external-browser-app "Google Chrome"
  "Application name used to raise the browser after a link is handed to it.
Handing Chrome a URL creates the tab but leaves window activation to the
window server, so the browser is activated explicitly and the user lands
on the page they clicked."
  :type 'string
  :group 'agent-repl)

(defconst agent-repl--external-browser-url-regexp "\\`https?://[^ \t\n]"
  "Regexp every URL handed to the external browser must match.
Restricted to http/https for the same reason the webapp's markdown
renderer restricts link targets: no other scheme has any business being
handed to a browser command line.")

;;;; ---- External boundary ----------------------------------------------------

(defun agent-repl--external-browser-call-process (program &rest args)
  "Run PROGRAM with ARGS synchronously; return its exit code (output discarded).
The one external-boundary wrapper for external-browser link opening;
registered in `agent-repl--external-boundary-functions' (core.el) so
tests stub it via `cl-letf' rather than launching a real browser."
  (apply #'call-process program nil nil nil args)) ;; ALLOW-EXTERNAL-BOUNDARY

;;;; ---- Command construction -------------------------------------------------

(defun agent-repl--external-browser-launch-args (url)
  "Return the Chrome argument list that opens URL in the pinned profile."
  (list (concat "--profile-directory=" agent-repl-external-browser-profile)
        url))

(defun agent-repl--external-browser-activate-args ()
  "Return the `osascript' argument list that raises the browser."
  (list "-e" (format "tell application \"%s\" to activate"
                     agent-repl-external-browser-app)))

;;;; ---- Opening --------------------------------------------------------------

(defun agent-repl-open-external-url (url)
  "Open URL in the pinned Chrome profile window and focus the browser.
Returns URL.  Signals when URL is not an http/https string, when Chrome
refuses the URL, or when the browser could not be raised: a link the user
clicked that silently went nowhere is worse than a loud failure, and each
of the three is a distinct diagnosis the canonical log records."
  (unless (and (stringp url)
               (string-match-p agent-repl--external-browser-url-regexp url))
    (agent-repl--error nil
                       "external-browser: rejected non-http url=%S profile=%s"
                       url agent-repl-external-browser-profile))
  (agent-repl--log nil "external-browser: opening url=%S profile=%s binary=%s"
                   url agent-repl-external-browser-profile
                   agent-repl-external-browser-binary)
  (let ((launch-exit (apply #'agent-repl--external-browser-call-process
                            agent-repl-external-browser-binary
                            (agent-repl--external-browser-launch-args url))))
    (unless (eq launch-exit 0)
      (agent-repl--error
       nil
       "external-browser: launch FAILED url=%S profile=%s binary=%s exit=%S"
       url agent-repl-external-browser-profile
       agent-repl-external-browser-binary launch-exit)))
  (let ((activate-exit (apply #'agent-repl--external-browser-call-process
                              "osascript"
                              (agent-repl--external-browser-activate-args))))
    (unless (eq activate-exit 0)
      (agent-repl--error
       nil
       "external-browser: activate FAILED url=%S app=%s exit=%S"
       url agent-repl-external-browser-app activate-exit)))
  (agent-repl--log nil "external-browser: opened url=%S profile=%s"
                   url agent-repl-external-browser-profile)
  url)

(defun agent-repl-browse-url-external (url &rest _args)
  "`browse-url-browser-function' entry point: open URL externally.
The trailing arguments `browse-url' passes (NEW-WINDOW and friends) are
ignored on purpose -- there is exactly one destination window, so there
is nothing for a caller to vary."
  (agent-repl-open-external-url url))

;;;; ---- Installation ---------------------------------------------------------

(defun agent-repl--external-browser-strip-emacs-handlers (handlers)
  "Return HANDLERS without the rules that visit a URL inside Emacs.
A `browse-url' handler outranks `browse-url-browser-function', so the
stock `browse-url-emacs' rule (which claims every non-HTML URL) would
keep sending a subset of links into an Emacs buffer no matter what the
function is set to.  Only the Emacs-visiting rules are dropped: the
`mailto:'/`man:'/`irc:' rules dispatch to a mail client, a man page, and
an IRC client rather than to a browser, so a hyperlink destination is not
what they decide."
  (seq-remove (lambda (entry) (eq (cdr entry) 'browse-url-emacs)) handlers))

(defun agent-repl--external-browser-install ()
  "Route every Emacs `browse-url' visit to the pinned Chrome profile."
  (setq browse-url-browser-function #'agent-repl-browse-url-external)
  (setq browse-url-handlers
        (agent-repl--external-browser-strip-emacs-handlers browse-url-handlers))
  (setq browse-url-default-handlers
        (agent-repl--external-browser-strip-emacs-handlers
         browse-url-default-handlers))
  (agent-repl--log nil
                   "external-browser: installed browse-url handler profile=%s"
                   agent-repl-external-browser-profile))

(agent-repl--external-browser-install)

(provide 'agent-repl-external-browser)
;;; external-browser.el ends here
