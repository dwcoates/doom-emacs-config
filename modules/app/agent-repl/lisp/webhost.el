;;; webhost.el --- the one web view that holds every workspace's page -*- lexical-binding: t; -*-

;;; Commentary:

;; ONE WEB VIEW, EVERY PAGE.  Emacs keeps exactly one WKWebView, the HOST, at
;; the webapp's host page (`?host=1', webapp/src/host/host.ts), and the host
;; holds every open workspace's page as an iframe at the workspace's own
;; origin.  The host is on screen whenever any workspace's page is, so WebKit
;; never suspends it, and an iframe of a page on screen is never suspended
;; either: every workspace page is live and current before the user switches
;; to it.  A switch only tells the host which iframe to show.
;;
;; WHY.  With a WKWebView per workspace, WebKit froze each hidden page about
;; half a second after it loaded; on the switch to it the page woke (up to
;; ~350ms) and replayed every push it had missed before it could paint, 0.7 to
;; 2.6s in all (measured 2026-10-12), and 36 web views held 36 WebKit content
;; processes and 11GB.  The host keeps one process.
;;
;; THE BUFFERS STAY WHAT THEY WERE.  Every workspace still has its own webview
;; buffer (`*agent-frontend-WS*', frontend.el), with its name, its owner stamp
;; and its place in the workspace's window layout.  It is a SLOT: it holds no
;; web view of its own, only a display of the host's one xwidget, so showing
;; any workspace's slot in a window shows the host there.  The host's own
;; buffer is hidden (its name starts with a space), belongs to no workspace,
;; and refuses to be killed by anything but this module.
;;
;; EMACS OWNS THE DESIRED STATE, THE HOST APPLIES IT.  Which pages exist (and
;; at which URL), which one is shown, and each page's input background and gate
;; slot are recorded here, and every call to the host states an end state, so
;; the whole of it is replayed whenever the host (re)loads.  A call made before
;; the host's script has started is queued ON THE PAGE and run when it starts
;; (`startHost', webapp/src/host/main.ts), so no call is lost to timing.
;;
;; EVERY CALL IS ANSWERED, AND THE ANSWER IS LOGGED.  The host has no daemon
;; connection, so its answers (and the notes it attaches to them, such as a
;; page message it could not read) are its only way into the logs.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url-parse)

(declare-function agent-repl--now-unix-ms "agent-repl-core" ())
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl-host-ref "host" (ws))
(declare-function agent-repl-host-conn "host" (ws))
(declare-function agent-repl-link-live "daemon-link" ())
(declare-function agent-repl-connect-connection-address "connect" (conn))
(declare-function agent-repl--frontend-make-webview-buffer "frontend" (url))
(declare-function agent-repl--frontend-webview-live-widget "frontend" (buf))
(declare-function agent-repl--frontend-webview-execute-script-value "frontend" (xw script callback))
(declare-function agent-repl--frontend-webview-navigate-widget "frontend" (xwidget uri))
(declare-function agent-repl--frontend-kill-webview "frontend" (buf))
(declare-function agent-repl-frontend-webview-url "frontend" (ws))
(declare-function agent-repl--ws-by-ref-id "workspace" (id))
(declare-function agent-repl-page-unload-note-shown "page-unload" (ws))
(declare-function agent-repl-roster-drawn-tab-order "roster" ())
(declare-function agent-repl--ws-current-name "workspace" ())
(declare-function xwidget-get "xwidget" (xwidget propname))
(declare-function xwidget-put "xwidget" (xwidget propname value))

(defvar xwidget-webkit-buffer-name-format)

(defconst agent-repl-webhost-buffer-name " *agent-webview-host*"
  "The host's buffer.  The leading space hides it from buffer lists, and it
matches no workspace buffer pattern, so no panel machinery mistakes it for a
workspace's own.")

(defconst agent-repl--webhost-central
  '(:agent-repl-central "the web view host spans every workspace")
  "The log scope of a host record that names no workspace.")

;;;; ---- Desired state ---------------------------------------------------------

(defvar agent-repl--webhost-pages (make-hash-table :test #'equal)
  "Workspace id -> the URL its page is held at.  What the host should hold.")

(defvar agent-repl--webhost-shown nil
  "The workspace id whose page the host should show, or nil.")

(defvar agent-repl--webhost-input-backgrounds (make-hash-table :test #'equal)
  "Workspace id -> the background its Emacs input window paints.")

(defvar agent-repl--webhost-gate-docks (make-hash-table :test #'equal)
  "Workspace id -> (PIXELS . BACKGROUND), the slot its docked gate takes.")

(defvar agent-repl--webhost-daemon-origin nil
  "The daemon origin the host's own calls were last pointed at, or nil.
Nil until a handover first moves them; a host load starts on the daemon
that served it.")

(defvar agent-repl--webhost-reloaded-for-call nil
  "The host URL a reload for an unknown call last loaded, or nil.
A host loaded from that URL that still lacks a call is not stale: the
daemon serves a webapp older than this elisp, which no reload fixes.")

(defvar agent-repl--webhost-predates-reload-pending nil
  "Non-nil from a reload for an unknown call until the reloaded host loads.
Answers that arrive meanwhile come from the document being left, which was
already found stale, so they ask for nothing more.")

(defvar agent-repl--webhost-reload-started-ms nil
  "When the host reload now loading began, in epoch ms, or nil.
Set when a reload navigates the host (`agent-repl--webhost-reload-now') and
handed with every page the next replay loads, then cleared: each page
measures how long it stood blank from it (`reload_blank').  Nil for the
host's first load, which is the editor starting, not a reload.")

(defvar agent-repl--webhost-held-ingress (make-hash-table :test #'equal)
  "Workspace id -> the vector of its held-prompt ingress entries the page draws
while it cannot reach the daemon (`agent-repl-webhost-set-held-ingress').")

(defvar agent-repl--webhost-log-level nil
  "The log level window the host and every page record at, or nil: a cons
\(LEVEL . UNTIL-MS), LEVEL a symbol and UNTIL-MS the window's end in epoch
milliseconds.  Replayed to a host that loads before the window ends.")

(defvar agent-repl--webhost-reload-timer nil
  "The pending host reload of a rollout burst, or nil.")

(defvar agent-repl--webhost-build nil
  "The webapp build a deploy last reloaded the host onto, or nil.
Nil too after any other reload, whose build nobody named
\(`agent-repl-webhost-reload-onto').")

(defvar agent-repl--webhost-url nil
  "The URL the host was last loaded at, which names the daemon it talks to.
The host draws the sidebar over its own connection to that daemon, so when
the workspaces move to another daemon the host must move with them
\(`agent-repl-webhost-reload-page').")

(defvar agent-repl--webhost-killing nil
  "Non-nil only while this module itself kills the host buffer.")

;;;; ---- The host web view -----------------------------------------------------

(defvar agent-repl--webhost-buffer nil
  "The host's buffer, held by identity: the one this module created.
A buffer that merely carries the host's name is not the host.")

(defun agent-repl-webhost-buffer ()
  "Return the live host buffer, or nil."
  (and (buffer-live-p agent-repl--webhost-buffer) agent-repl--webhost-buffer))

(defun agent-repl-webhost-xwidget ()
  "Return the host's live xwidget, or nil when there is no host."
  (when-let ((buf (agent-repl-webhost-buffer)))
    (agent-repl--frontend-webview-live-widget buf)))

(defun agent-repl-webhost-origin (conn)
  "Return the host page's origin on CONN's daemon.
  http://host.localhost:<daemon port>
The host page is the webapp's own bundle, served by the daemon like a
workspace's page, at a host name of its own so its loads share no
connection budget with any workspace's page (frontend.el's page origin)."
  (let* ((address (and conn (agent-repl-connect-connection-address conn)))
         (port (and address (string-match ":\\([0-9]+\\)\\'" address)
                    (match-string 1 address))))
    (unless port
      (error "agent-repl: the web view host has no daemon address to load from (address=%S)" address))
    (format "http://host.localhost:%s" port)))

(defun agent-repl-webhost-url (conn)
  "Return the host page's URL on CONN's daemon.
  http://host.localhost:<daemon port>/?host=1"
  (concat (agent-repl-webhost-origin conn) "/?host=1"))

(defun agent-repl--webhost-refuse-kill ()
  "Keep the host buffer alive against every kill but this module's own.
On `kill-buffer-query-functions' in the host buffer only: a perspective that
kills its buffers would otherwise take every workspace's page with it."
  (or agent-repl--webhost-killing
      (progn
        (agent-repl--warn agent-repl--webhost-central
                          "elisp.webhost.kill-refused command=%S" this-command)
        nil)))

(defun agent-repl--webhost-on-load (event-type)
  "Replay the desired state when the host's EVENT-TYPE is `load-changed'.
ONE REPLAY PER DOCUMENT, when it is committed.  A navigation reports
`load-started', `load-committed' and `load-finished' (and a redirect),
and a replay at each ran it three times per load (2026-10-17).  A call
before commit reaches the document being left; from commit on it reaches
the new one, which queues it until the host starts (main.ts).  So the
replay at commit is the earliest that lands, and the only one needed.

The stage rides only the event itself, as `xwidget-webkit-callback'
reads it, so it is read here off `last-input-event' rather than passed."
  (when (eq event-type 'load-changed)
    (let ((status (nth 3 last-input-event)))
      (agent-repl--info agent-repl--webhost-central "elisp.webhost.load-changed status=%s pages=%d shown=%s"
                        status (hash-table-count agent-repl--webhost-pages) agent-repl--webhost-shown)
      (when (equal status "load-committed")
        ;; From here on every answer is the new document's.
        (setq agent-repl--webhost-predates-reload-pending nil)
        (agent-repl--webhost-replay)))))

(defun agent-repl--webhost-callback (xwidget event-type)
  "The host XWIDGET's `callback': its prior handler, then the load handler.
INSTALLED BY NAME, never as a closure.  The xwidget keeps its callback for
the session, so a closure installed on it is code a hot reload cannot
reach: a closure calling `agent-repl--webhost-on-load' with one argument
broke when that function took two, and one calling it with two broke when
it went back to one, every load signalling \"Wrong number of arguments\"
and replaying nothing (2026-10-17).  By name, the host runs whatever this
module's current definitions are, and the prior handler rides the xwidget's
`agent-repl-prior-callback' property."
  (let ((prior (xwidget-get xwidget 'agent-repl-prior-callback)))
    (when (functionp prior) (funcall prior xwidget event-type)))
  (agent-repl--webhost-on-load event-type))

(defun agent-repl--webhost-watch-load (xw)
  "Make XW replay the desired state on every load, keeping its own handler.
The xwidget event handler calls the widget's `callback' property, so the
existing one is WRAPPED rather than replaced: the webkit machinery still gets
every event it needs.  The wrapper is `agent-repl--webhost-callback', by
name (see that function)."
  (xwidget-put xw 'agent-repl-prior-callback (xwidget-get xw 'callback))
  (xwidget-put xw 'callback 'agent-repl--webhost-callback))

(defun agent-repl--webhost-rewatch ()
  "Put the live host's callback back on `agent-repl--webhost-callback'.
Run each time this file loads, so a hot reload heals a host that an
earlier build wrapped in a closure (see `agent-repl--webhost-callback').  That
closure held the creator's handler, `xwidget-webkit-callback' (the host is
created with no handler of its own, `agent-repl-webhost-ensure'), so that
handler is the prior one again."
  (when-let ((xw (agent-repl-webhost-xwidget)))
    (let ((installed (xwidget-get xw 'callback)))
      (unless (eq installed 'agent-repl--webhost-callback)
        (xwidget-put xw 'agent-repl-prior-callback 'xwidget-webkit-callback)
        (xwidget-put xw 'callback 'agent-repl--webhost-callback)
        (agent-repl--info agent-repl--webhost-central "elisp.webhost.rewatched replaced=%s"
                          (if (symbolp installed) installed (type-of installed)))))))

(defun agent-repl-webhost-ensure (conn)
  "Return the host's live xwidget, creating the host on CONN's daemon if needed.
The host is created once and lives for the session.  No window is touched:
the host buffer is never displayed itself; workspace slots display it."
  (or (agent-repl-webhost-xwidget)
      (let* ((url (agent-repl-webhost-url conn))
             (buf (save-window-excursion (agent-repl--frontend-make-webview-buffer url))))
        (with-current-buffer buf
          (setq-local xwidget-webkit-buffer-name-format agent-repl-webhost-buffer-name)
          (setq-local header-line-format nil)
          (rename-buffer agent-repl-webhost-buffer-name t)
          (add-hook 'kill-buffer-query-functions #'agent-repl--webhost-refuse-kill nil t))
        (let ((xw (agent-repl--frontend-webview-live-widget buf)))
          (unless xw
            (let ((agent-repl--webhost-killing t))
              (agent-repl--frontend-kill-webview buf))
            (error "agent-repl: the web view host was created without a live xwidget"))
          (setq agent-repl--webhost-buffer buf
                agent-repl--webhost-url url)
          (agent-repl--webhost-watch-load xw)
          (agent-repl--info agent-repl--webhost-central "elisp.webhost.created url=%s" url)
          xw))))

(defun agent-repl-webhost-kill ()
  "Kill the host and forget its desired state.  Every page goes with it."
  (when-let ((buf (agent-repl-webhost-buffer)))
    (let ((agent-repl--webhost-killing t))
      (agent-repl--frontend-kill-webview buf))
    (setq agent-repl--webhost-buffer nil
          agent-repl--webhost-url nil)
    (agent-repl--info agent-repl--webhost-central "elisp.webhost.killed")))

;;;; ---- Calls -----------------------------------------------------------------

(defun agent-repl--webhost-script (method &rest args)
  "The script calling the host's METHOD with ARGS, queued until the host starts.
Each of ARGS is JSON-encoded, so nothing is spliced into the script unquoted.
A queued call answers `queued', naming the workspace id when the first of
ARGS is one, so the caller can ask again.  A host that has no METHOD (one
loaded from a build older than this elisp) answers `unknown-call' rather
than throwing, so the gap between the two is seen
\(`agent-repl--webhost-answered').
The answer is always a string: a null answer is dropped without a callback."
  (let ((call (format "h.%s(%s)" method (mapconcat #'json-encode args ",")))
        (op (json-encode method))
        (id (if (stringp (car args)) (json-encode (car args)) "null")))
    (format (concat "(function(){var c=function(h){return typeof h[%s]===\"function\"?%s:"
                    "JSON.stringify({op:%s,id:%s,outcome:\"unknown-call\",notes:[]});};"
                    "var h=window.agentReplHost;"
                    "if(h){return c(h);}"
                    "(window.__agentReplHostPending=window.__agentReplHostPending||[]).push(c);"
                    "return JSON.stringify({op:%s,id:%s,outcome:\"queued\",notes:[]});})()")
            op call op id op id)))

(defun agent-repl--webhost-predates-call (op)
  "The host has no OP: it was loaded from a build older than this elisp.
EMACS AND THE HOST SPEAK ONE PROTOCOL.  A host that predates a call Emacs
makes is reloaded on the daemon serving Emacs now, which serves the build
this elisp came with; one that still lacks the call after that reload is a
daemon serving an older webapp than this elisp, an ERROR no reload heals."
  (let* ((conn (agent-repl-link-live))
         (url (and conn (agent-repl-webhost-url conn))))
    (cond
     (agent-repl--webhost-predates-reload-pending
      (agent-repl--info agent-repl--webhost-central
                        "elisp.webhost.predates-call op=%s action=none reason=reload-pending" op))
     ((null conn)
      (agent-repl--warn agent-repl--webhost-central
                        "elisp.webhost.predates-call op=%s action=none reason=no-link" op))
     ((equal url agent-repl--webhost-reloaded-for-call)
      (agent-repl--error agent-repl--webhost-central
                         "elisp.webhost.predates-call op=%s action=none reason=served-webapp-predates-elisp url=%s"
                         op url))
     (t
      (setq agent-repl--webhost-reloaded-for-call url
            agent-repl--webhost-predates-reload-pending t)
      (agent-repl--info agent-repl--webhost-central
                        "elisp.webhost.predates-call op=%s action=reload url=%s" op url)
      (agent-repl-webhost-reload conn)))))

(defun agent-repl--webhost-answer-ws (answer)
  "The log scope for ANSWER: the workspace it names, or the central scope."
  (let ((id (plist-get answer :id)))
    (or (and (stringp id) (fboundp 'agent-repl--ws-by-ref-id)
             (agent-repl--ws-by-ref-id id))
        agent-repl--webhost-central)))

(defun agent-repl--webhost-record-note (scope op note)
  "Record NOTE, which the host attached to its answer to OP, under SCOPE.
NOTE is (:level LEVEL :text TEXT): LEVEL \"info\" for what a working host
does (a page cooling, a page from another build mid-rollout) and \"warn\"
for what it should never see.  A bare string is a host from a build before
notes carried a level, recorded at WARN as that build recorded them all; a
level this build does not know is an ERROR naming it, never dropped."
  (cond
   ((stringp note)
    (agent-repl--warn scope "elisp.webhost.note op=%s note=%s" op note))
   ((equal (plist-get note :level) "info")
    (agent-repl--info scope "elisp.webhost.note op=%s note=%s" op (plist-get note :text)))
   ((equal (plist-get note :level) "warn")
    (agent-repl--warn scope "elisp.webhost.note op=%s note=%s" op (plist-get note :text)))
   (t
    (agent-repl--error scope "elisp.webhost.note-unknown-level op=%s note=%S" op note))))

(defun agent-repl--webhost-answered (raw)
  "Record the host's RAW answer to a call, and every note it carries.
The xwidget `javascript-callback' handler calls this BY SYMBOL: see
`agent-repl--frontend-webview-read-script' for why a closure would crash."
  (condition-case err
      (let* ((answer (json-parse-string raw :object-type 'plist :array-type 'list
                                        :null-object nil :false-object nil))
             (scope (agent-repl--webhost-answer-ws answer)))
        (agent-repl--log scope "elisp.webhost.answer op=%s id=%s outcome=%s"
                         (plist-get answer :op) (plist-get answer :id) (plist-get answer :outcome))
        (when (equal (plist-get answer :outcome) "unknown-call")
          (agent-repl--webhost-predates-call (plist-get answer :op)))
        (dolist (note (plist-get answer :notes))
          (agent-repl--webhost-record-note scope (plist-get answer :op) note)))
    (error
     (agent-repl--error agent-repl--webhost-central
                        "elisp.webhost.answer-unreadable raw=%S error=%S" raw err))))

(defun agent-repl--webhost-call (method &rest args)
  "Call the host's METHOD with ARGS; non-nil when the call was sent.
A call with no host to send it to sends nothing: the desired state it served
is recorded by the caller and replayed when the host loads."
  (if-let ((xw (agent-repl-webhost-xwidget)))
      (progn
        (agent-repl--frontend-webview-execute-script-value
         xw (apply #'agent-repl--webhost-script method args) 'agent-repl--webhost-answered)
        t)
    (agent-repl--log agent-repl--webhost-central "elisp.webhost.call-skipped method=%s reason=no-host" method)
    nil))

(defun agent-repl-webhost-read (method args callback)
  "Ask the host's METHOD with ARGS, handing its answer string to CALLBACK.
CALLBACK must be a symbol naming a function (the read channel's crash
invariant).  Non-nil when the question was sent; nil when there is no host."
  (if-let ((xw (agent-repl-webhost-xwidget)))
      (progn
        (agent-repl--frontend-webview-execute-script-value
         xw (apply #'agent-repl--webhost-script method args) callback)
        t)
    nil))

;;;; ---- Pages ----------------------------------------------------------------

(defun agent-repl--webhost-id (ws)
  "WS's workspace id, the key the host holds its page under; nil without a ref."
  (plist-get (and (fboundp 'agent-repl-host-ref) (agent-repl-host-ref ws)) :id))

(defun agent-repl-webhost-ensure-page (ws)
  "Hold WS's page in the host at WS's URL, creating the host if needed.
Answers WS's id.  Signals when WS has no ref or no connection: its URL cannot
be built (`agent-repl-frontend-webview-url')."
  (let* ((url (agent-repl-frontend-webview-url ws))
         (id (agent-repl--webhost-id ws)))
    (agent-repl-webhost-ensure (agent-repl-host-conn ws))
    (unless (equal (gethash id agent-repl--webhost-pages) url)
      (puthash id url agent-repl--webhost-pages)
      (agent-repl--info ws "elisp.webhost.page-ensured ws=%s id=%s" ws id))
    (agent-repl--webhost-call "ensure" id url)
    id))

(defvar agent-repl--webhost-switch-started nil
  "The user\='s latest switch not yet handed to a show: (WS . UNIX-MS), or nil.
Set by `agent-repl-webhost-note-switch\=' at the switch keystroke, taken by
the next `agent-repl-webhost-show\=' whatever workspace it shows, so an
instant can never be charged to a later show it did not cause.")

(defun agent-repl-webhost-note-switch (ws)
  "Note that the user\='s switch to WS began now, for the page to measure.
The page times the switch from this instant to its first shown frame, or to
its conversation drawn on a first show (webapp/src/switch-latency.ts), and
reports it to the daemon\='s metrics.  The instant is the host\='s wall
clock in Unix milliseconds, the clock the page reads too."
  (setq agent-repl--webhost-switch-started
        (cons ws (agent-repl--now-unix-ms))))

(defun agent-repl--webhost-take-switch-start (ws)
  "The noted switch start for WS in Unix milliseconds, or nil; always cleared."
  (let ((noted agent-repl--webhost-switch-started))
    (setq agent-repl--webhost-switch-started nil)
    (and noted (equal (car noted) ws) (cdr noted))))

(defun agent-repl-webhost-show (ws)
  "Show WS's page in the host.  Nothing is sent when it is already the one shown.
A switch the user began to WS (`agent-repl-webhost-note-switch\=') rides the
show, so the page can measure it."
  (let ((id (agent-repl--webhost-id ws))
        (switch-started (agent-repl--webhost-take-switch-start ws)))
    (when (fboundp 'agent-repl-page-unload-note-shown)
      (agent-repl-page-unload-note-shown ws))
    (when (and id (not (equal id agent-repl--webhost-shown)))
      (setq agent-repl--webhost-shown id)
      (agent-repl--info ws "elisp.webhost.show ws=%s id=%s" ws id)
      (agent-repl--webhost-call "show" id switch-started))))

(defun agent-repl-webhost-remove (ws)
  "Drop WS's page from the host, and everything held for it."
  (when-let ((id (agent-repl--webhost-id ws)))
    (remhash id agent-repl--webhost-pages)
    (remhash id agent-repl--webhost-input-backgrounds)
    (remhash id agent-repl--webhost-gate-docks)
    (remhash id agent-repl--webhost-held-ingress)
    (when (equal id agent-repl--webhost-shown)
      (setq agent-repl--webhost-shown nil))
    (agent-repl--info ws "elisp.webhost.page-removed ws=%s id=%s" ws id)
    (agent-repl--webhost-call "remove" id)))

(defun agent-repl-webhost-has-page-p (ws)
  "Non-nil when the host is meant to hold WS's page."
  (let ((id (agent-repl--webhost-id ws)))
    (and id (gethash id agent-repl--webhost-pages) t)))

(defun agent-repl-webhost-reload-page (ws)
  "Load WS's page again, at its URL now: re-point it when the URL moved
\(a daemon handover), reload it in place otherwise.

A HANDOVER NEVER RELOADS THE HOST.  Reloading it replaced every page at
once, and the page on screen stood blank for seconds (4.8s on the
2026-10-23 14:38 deploy).  Instead the host's own calls (its sidebar's)
are pointed at the daemon WS moved to (`setDaemon'), and WS's page is
re-pointed there: the host keeps a shown page on screen until its
replacement has drawn (host.ts), so nobody sees it blank."
  (let* ((url (agent-repl-frontend-webview-url ws))
         (id (agent-repl--webhost-id ws))
         (started (agent-repl--now-unix-ms)))
    (if (equal (gethash id agent-repl--webhost-pages) url)
        (agent-repl--webhost-call "reload" id started)
      (puthash id url agent-repl--webhost-pages)
      (agent-repl-webhost-follow-daemon ws (agent-repl-host-conn ws))
      (agent-repl--webhost-call "ensure" id url nil started))
    (agent-repl--info ws "elisp.webhost.page-reloaded ws=%s id=%s" ws id)
    id))

(defun agent-repl-webhost-follow-daemon (ws conn)
  "Point the host's own calls at CONN's daemon, the one WS moved to.
The host keeps its page and every workspace's; only where its sidebar's
calls go moves (`setDaemon')."
  (let ((origin (agent-repl-webhost-origin conn)))
    (unless (equal origin (or agent-repl--webhost-daemon-origin
                              (and agent-repl--webhost-url
                                   (agent-repl--webhost-url-origin agent-repl--webhost-url))))
      (setq agent-repl--webhost-daemon-origin origin)
      (agent-repl--info ws "elisp.webhost.follows-daemon ws=%s to=%s" ws origin)
      (agent-repl--webhost-call "setDaemon" origin))))

(defun agent-repl--webhost-url-origin (url)
  "The scheme, host and port of URL, as the host page reads its own origin."
  (let ((parsed (url-generic-parse-url url)))
    (format "%s://%s:%d" (url-type parsed) (url-host parsed) (url-port parsed))))

(defun agent-repl-webhost-set-input-background (ws background)
  "Tell WS's page the BACKGROUND its input window paints; kept for its reloads."
  (when-let ((id (agent-repl--webhost-id ws)))
    (puthash id background agent-repl--webhost-input-backgrounds)
    (agent-repl--webhost-call "setInputBackground" id background)))

(defun agent-repl-webhost-set-gate-dock (ws pixels background)
  "Tell WS's page the slot its docked gate takes; kept for its reloads.
BACKGROUND is the input window's, or nil when it has none."
  (when-let ((id (agent-repl--webhost-id ws)))
    (puthash id (cons pixels background) agent-repl--webhost-gate-docks)
    (agent-repl--webhost-call "setGateDock" id pixels background)))

(defun agent-repl-webhost-set-held-ingress (ws entries)
  "Tell WS's page the held-prompt ingress ENTRIES for WS; kept for its reloads.
ENTRIES is a vector of hash tables, each an entry's `key', `queuedAt' and
`said' (webapp/src/host/protocol.ts `HeldIngressWireEntry'), the whole list.
The page draws them while it cannot reach the daemon, so a prompt sent
during an outage is seen as taken."
  (when-let ((id (agent-repl--webhost-id ws)))
    (puthash id entries agent-repl--webhost-held-ingress)
    (agent-repl--webhost-call "setHeldIngress" id entries)))

(defun agent-repl-webhost-set-log-level (level until-ms)
  "Raise the host and every page to LEVEL until UNTIL-MS (epoch ms).
LEVEL `info', with UNTIL-MS nil, closes the window.  Kept for the host's
reloads while the window lasts.  Non-nil when the call was sent."
  (setq agent-repl--webhost-log-level (unless (eq level 'info) (cons level until-ms)))
  (agent-repl--webhost-call "setLogLevel" (symbol-name level) until-ms))

(defun agent-repl-webhost-dismiss-digest (ws)
  "Ask WS's page to close its standing news digest."
  (when-let ((id (agent-repl--webhost-id ws)))
    (agent-repl--webhost-call "dismissDigest" id)))

;;;; ---- Replay and reload -----------------------------------------------------

(defun agent-repl--webhost-replay-lanes (ids)
  "IDS split into two lanes around the current workspace: `(AFTER . BEFORE)'.
The drawn tab order is a RING: the first and last tabs are next to each
other.  The tabs other than the current one are halved around it: AFTER
takes the nearer half going forward from the current tab, BEFORE the rest
going backward, each nearest first, and AFTER takes the odd one out.  So
with eleven tabs and the tenth current, AFTER is 11, 1, 2, 3, 4 and BEFORE
is 9, 8, 7, 6, 5.  A page whose workspace has no drawn tab, or every page
when no tab is current, goes to whichever lane is shorter then, in the
order given."
  (let* ((order (and (fboundp 'agent-repl-roster-drawn-tab-order)
                     (agent-repl-roster-drawn-tab-order)))
         (n (length order))
         (current (and (fboundp 'agent-repl--ws-current-name) (agent-repl--ws-current-name)))
         (here (and current (cl-position current order :test #'equal)))
         (after-span (/ n 2))
         after before untabbed)
    (dolist (id ids)
      (let* ((ws (and (fboundp 'agent-repl--ws-by-ref-id) (agent-repl--ws-by-ref-id id)))
             (at (and ws (cl-position ws order :test #'equal))))
        (if (or (null at) (null here))
            (push id untabbed)
          (let ((ahead (mod (- at here) n)))
            (if (<= ahead after-span)
                (push (cons ahead id) after)
              (push (cons (mod (- here at) n) id) before))))))
    (let* ((nearest-first (lambda (ranked)
                            (mapcar #'cdr (cl-stable-sort (nreverse ranked) #'< :key #'car))))
           (after (funcall nearest-first after))
           (before (funcall nearest-first before)))
      (dolist (id (nreverse untabbed))
        (if (<= (length after) (length before))
            (setq after (append after (list id)))
          (setq before (append before (list id)))))
      (cons after before))))

(defun agent-repl--webhost-adopting-p (id)
  "Non-nil when page ID\='s workspace has an adopt waiting on its page."
  (let ((ws (and (fboundp 'agent-repl--ws-by-ref-id) (agent-repl--ws-by-ref-id id))))
    (and ws (fboundp 'agent-repl-host-adopting-p) (agent-repl-host-adopting-p ws))))

(defun agent-repl--webhost-replay-page (id lane)
  "Hand the host page ID and its state.
Given a LANE, the host loads it once the pages before it in LANE have drawn.
A replay after a reload hands the reload's start with the page
\(`agent-repl--webhost-reload-started-ms')."
  (when-let ((url (gethash id agent-repl--webhost-pages)))
    (agent-repl--webhost-call "ensure" id url lane agent-repl--webhost-reload-started-ms)
    (when-let ((background (gethash id agent-repl--webhost-input-backgrounds)))
      (agent-repl--webhost-call "setInputBackground" id background))
    (when-let ((dock (gethash id agent-repl--webhost-gate-docks)))
      (agent-repl--webhost-call "setGateDock" id (car dock) (cdr dock)))
    (when-let ((entries (gethash id agent-repl--webhost-held-ingress)))
      (agent-repl--webhost-call "setHeldIngress" id entries))))

(defun agent-repl--webhost-replay ()
  "Hand the host everything it should hold, after a load: the host starts empty.
THE PAGES COME BACK IN TWO LANES, ONE PAGE AT A TIME IN EACH (owner
rulings, 2026-10-17).  Every page booting at once is what stalled the web
process for up to 1.8s after a reconnect.  The shown page loads now; every
other page is handed over IN TURN, in the lane after the current workspace
or the lane before it, nearest first (`agent-repl--webhost-replay-lanes').
The host loads each lane's next page once its last has drawn (host.ts), so
the pages a switch is likeliest to reach either way are fresh first, two at
a time and never piled up.  A switch to a page still waiting loads it at
once.

A PAGE WHOSE WORKSPACE IS MID-ADOPT LOADS AT ONCE TOO.  A handover\='s
first transfer reloads the host onto the successor, and the replay comes
while other workspaces\' adopts are still waiting on their pages: the
successor\='s rendezvous completes only once the page has called, inside
the host\='s own 10s call bound.  Queued behind a lane\='s other pages, two
such pages missed it on the 2026-10-22 11:14 deploy, and both adopts timed
out (`agent-repl-host-adopting-p')."
  (let (ids)
    (maphash (lambda (id _url) (push id ids)) agent-repl--webhost-pages)
    (when agent-repl--webhost-shown
      (agent-repl--webhost-replay-page agent-repl--webhost-shown nil)
      (agent-repl--webhost-call "show" agent-repl--webhost-shown))
    (let* ((rest (delete agent-repl--webhost-shown (nreverse ids)))
           (adopting (cl-remove-if-not #'agent-repl--webhost-adopting-p rest))
           (lanes (agent-repl--webhost-replay-lanes (cl-remove-if #'agent-repl--webhost-adopting-p rest))))
      (dolist (id adopting)
        (agent-repl--webhost-replay-page id nil))
      (dolist (id (car lanes))
        (agent-repl--webhost-replay-page id "after"))
      (dolist (id (cdr lanes))
        (agent-repl--webhost-replay-page id "before"))
      (agent-repl--info agent-repl--webhost-central "elisp.webhost.replay-in-turn after=%d before=%d adopting=%d reload-started-ms=%s"
                        (length (car lanes)) (length (cdr lanes)) (length adopting)
                        agent-repl--webhost-reload-started-ms)))
  ;; THE RELOAD'S START WAS HANDED WITH THIS REPLAY'S PAGES; a later load is
  ;; not this reload's.
  (setq agent-repl--webhost-reload-started-ms nil)
  (when (and agent-repl--webhost-log-level
             (> (cdr agent-repl--webhost-log-level) (agent-repl--now-unix-ms)))
    (agent-repl--webhost-call "setLogLevel" (symbol-name (car agent-repl--webhost-log-level))
                              (cdr agent-repl--webhost-log-level))))

(defun agent-repl--webhost-reload-now (conn)
  "Navigate the host to its URL on CONN's daemon; its load replays the rest."
  (setq agent-repl--webhost-reload-timer nil)
  (if-let ((xw (agent-repl-webhost-xwidget)))
      (progn
        (setq agent-repl--webhost-url (agent-repl-webhost-url conn)
              agent-repl--webhost-daemon-origin nil
              agent-repl--webhost-reload-started-ms (agent-repl--now-unix-ms))
        (agent-repl--frontend-webview-navigate-widget xw agent-repl--webhost-url)
        (agent-repl--info agent-repl--webhost-central "elisp.webhost.reloaded pages=%d"
                          (hash-table-count agent-repl--webhost-pages)))
    (agent-repl--log agent-repl--webhost-central "elisp.webhost.reload-skipped reason=no-host")))

(defun agent-repl-webhost-reload (conn)
  "Reload the host, and so every page, on CONN's daemon, once per turn.
Every page re-pointed in one turn of the command loop reloads the host
once, on the next turn.  The build the host comes onto is nobody's to
name, so the build a deploy brought is forgotten."
  (setq agent-repl--webhost-build nil)
  (unless agent-repl--webhost-reload-timer
    (setq agent-repl--webhost-reload-timer
          (run-at-time 0 nil #'agent-repl--webhost-reload-now conn))))

(defun agent-repl-webhost-reload-onto (conn build)
  "Reload the host onto the webapp BUILD a deploy brings, on CONN's daemon.
THE HOST RELOADS ONCE PER BUILD.  A deploy tells every workspace with a
stale page, each on its own stream, so the pushes arrive over several
turns of the command loop (eight in 100ms, 2026-10-17), and each reload
replays every page.  The host's reload fetches the build the daemon
serves, which is BUILD from the first push on, so a push naming the build
the host was last reloaded onto is already met."
  (if (equal build agent-repl--webhost-build)
      (agent-repl--info agent-repl--webhost-central "elisp.webhost.reload-met build=%s" build)
    (agent-repl-webhost-reload conn)
    (setq agent-repl--webhost-build build)
    (agent-repl--info agent-repl--webhost-central "elisp.webhost.reload-onto build=%s" build)))

(agent-repl--webhost-rewatch)

(provide 'webhost)

;;; webhost.el ends here
