;;; sidebar-view.el --- the sidebar's own web view, in a full-height window -*- lexical-binding: t; -*-

;;; Commentary:

;; THE SIDEBAR HAS A WINDOW OF ITS OWN (owner request, 2026-10-31).  Emacs
;; lays out three windows for agent-repl: the sidebar at the frame's left
;; edge, running the frame's whole height, and beside it the feed's window
;; (the host, webhost.el) over the composer's.
;;
;;   +-----------+----------------------------------+
;;   | sidebar   | feed (the host's web view)       |
;;   | (this     |                                  |
;;   |  web view)+----------------------------------+
;;   |           | composer                         |
;;   +-----------+----------------------------------+
;;
;; WHY A SECOND WEB VIEW.  On the NS port one xwidget shows in one window
;; only: its one native view is re-parented into whichever window displayed
;; it last (src/nsxwidget.m).  So the sidebar cannot be part of the host,
;; which shows in the feed's window: it is the webapp's own bundle at
;; `?sidebar=1' (webapp/src/host/sidebar-page.ts), in a web view of its own.
;; It is always on screen while the panels are, so WebKit never suspends it.
;;
;; A SIDE WINDOW.  The sidebar is the same for every workspace, so it is one
;; global window, not a third panel of each workspace's: a left side window,
;; which spans the frame's whole height by construction and which the panel
;; machinery already leaves alone (`agent-repl-window--side-window-p').  The
;; workspace's own panels stay the pair they were (view over input).  It has
;; no mode line, as the feed and the composer have none.
;;
;; EMACS OWNS THE DESIRED STATE, THE PAGE APPLIES IT, as with the host: which
;; workspace's row is shown, which daemon the page dials, and the log level
;; window are recorded here and replayed whenever the page (re)loads.  The
;; page follows the host's every reload (`agent-repl-sidebar-view-navigate',
;; called by webhost.el's), so the two always run one build.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--now-unix-ms "agent-repl-core" ())
(declare-function agent-repl--frontend-webview-live-widget "frontend" (buf))
(declare-function agent-repl--frontend-webview-execute-script-value "frontend" (xw script callback))
(declare-function agent-repl--frontend-webview-navigate-widget "frontend" (xwidget uri))
(declare-function agent-repl--frontend-kill-webview "frontend" (buf))
(declare-function agent-repl-webhost-origin "webhost" (conn))
(declare-function agent-repl-webhost-parse-answer "webhost" (raw))
(declare-function agent-repl--webhost-record-note "webhost" (scope op note))
(declare-function agent-repl--webview-script "webhost" (global pending method args))
(declare-function agent-repl--webview-strayed-uri "webhost" (own-url uri))
(declare-function agent-repl--webview-make-hidden "webhost" (url name refuse-kill kill what))
(declare-function agent-repl--webview-call-prior "webhost" (xwidget event-type))
(declare-function agent-repl--webview-watch-load "webhost" (xw callback))
(declare-function xwidget-webkit-uri "xwidget" (xwidget))

(defvar agent-repl--webhost-log-level)

(defgroup agent-repl-sidebar-view nil
  "The agent-repl sidebar's own window."
  :group 'agent-repl)

(defcustom agent-repl-sidebar-view-width-fraction 0.2
  "Fraction of the frame's width the sidebar's window takes when it opens.
A fifth, as the sidebar took of the feed's window when it was drawn there.
Dragging the window's divider resizes it for as long as it stands."
  :type 'number
  :group 'agent-repl-sidebar-view)

(defconst agent-repl-sidebar-view-buffer-name " *agent-webview-sidebar*"
  "The sidebar's buffer.  The leading space hides it from buffer lists, and it
matches no workspace buffer pattern, so no panel machinery mistakes it for a
workspace's own.")

(defconst agent-repl--sidebar-view-central
  '(:agent-repl-central "the sidebar spans every workspace")
  "The log scope of a sidebar record.")

(defvar agent-repl--sidebar-view-buffer nil
  "The sidebar's buffer, held by identity: the one this module created.")

(defvar agent-repl--sidebar-view-url nil
  "The URL the sidebar was last loaded at, which names the daemon it talks to.")

(defvar agent-repl--sidebar-view-killing nil
  "Non-nil only while this module itself kills the sidebar buffer.")

(defvar agent-repl--sidebar-view-shown nil
  "The workspace id whose row the sidebar should highlight, or nil.")

(defvar agent-repl--sidebar-view-daemon-origin nil
  "The daemon origin the sidebar's calls were last pointed at, or nil.
Nil until a handover first moves them; a load starts on the daemon that
served it.")

(defvar agent-repl--sidebar-view-reloaded-for-call nil
  "The sidebar URL a reload for an unknown call last loaded, or nil.
A sidebar loaded from that URL that still lacks a call is not stale: the
daemon serves a webapp older than this elisp, which no reload fixes.")

;;;; ---- The web view ----------------------------------------------------------

(defun agent-repl-sidebar-view-buffer ()
  "Return the live sidebar buffer, or nil."
  (and (buffer-live-p agent-repl--sidebar-view-buffer) agent-repl--sidebar-view-buffer))

(defun agent-repl-sidebar-view-xwidget ()
  "Return the sidebar's live xwidget, or nil when there is no sidebar."
  (when-let ((buf (agent-repl-sidebar-view-buffer)))
    (agent-repl--frontend-webview-live-widget buf)))

(defun agent-repl-sidebar-view-url (conn)
  "Return the sidebar page's URL on CONN's daemon.
  http://host.localhost:<daemon port>/?sidebar=1
On the host's own origin: the two are Emacs's own documents, and neither
owns a workspace."
  (concat (agent-repl-webhost-origin conn) "/?sidebar=1"))

(defun agent-repl--sidebar-view-refuse-kill ()
  "Keep the sidebar buffer alive against every kill but this module's own.
On `kill-buffer-query-functions' in the sidebar buffer only."
  (or agent-repl--sidebar-view-killing
      (progn
        (agent-repl--warn agent-repl--sidebar-view-central
                          "elisp.sidebar-view.kill-refused command=%S" this-command)
        nil)))

(defun agent-repl--sidebar-view-kill-buffer (buf)
  "Kill BUF, the sidebar's buffer, as this module's own kill."
  (let ((agent-repl--sidebar-view-killing t))
    (agent-repl--frontend-kill-webview buf)))

(defun agent-repl-sidebar-view-ensure (conn)
  "Return the sidebar's live xwidget, creating it on CONN's daemon if needed.
Created once and kept for the session.  No window is touched."
  (or (agent-repl-sidebar-view-xwidget)
      (let* ((url (agent-repl-sidebar-view-url conn))
             (buf (agent-repl--webview-make-hidden
                   url agent-repl-sidebar-view-buffer-name #'agent-repl--sidebar-view-refuse-kill
                   #'agent-repl--sidebar-view-kill-buffer "the sidebar's web view"))
             (xw (agent-repl--frontend-webview-live-widget buf)))
        (setq agent-repl--sidebar-view-buffer buf
              agent-repl--sidebar-view-url url
              agent-repl--sidebar-view-daemon-origin nil)
        (agent-repl--sidebar-view-watch-load xw)
        (agent-repl--info agent-repl--sidebar-view-central "elisp.sidebar-view.created url=%s" url)
        xw)))

(defun agent-repl-sidebar-view-kill ()
  "Kill the sidebar's web view, and its window with it."
  (when-let ((buf (agent-repl-sidebar-view-buffer)))
    (agent-repl--sidebar-view-kill-buffer buf)
    (setq agent-repl--sidebar-view-buffer nil
          agent-repl--sidebar-view-url nil)
    (agent-repl--info agent-repl--sidebar-view-central "elisp.sidebar-view.killed")))

;;;; ---- Loads -----------------------------------------------------------------

(defun agent-repl--sidebar-view-callback (xwidget event-type)
  "The sidebar XWIDGET's `callback': its prior handler, then the load handler.
INSTALLED BY NAME, never as a closure, so a hot reload reaches it (see
`agent-repl--webhost-callback')."
  (agent-repl--webview-call-prior xwidget event-type)
  (agent-repl--sidebar-view-on-load event-type))

(defun agent-repl--sidebar-view-watch-load (xw)
  "Make XW replay the desired state on every load, keeping its own handler."
  (agent-repl--webview-watch-load xw 'agent-repl--sidebar-view-callback))

(defun agent-repl--sidebar-view-on-load (event-type)
  "Replay the desired state when the sidebar's EVENT-TYPE commits a load.
One replay per document, at `load-committed', as the host replays
\(`agent-repl--webhost-on-load').  A load that strayed off the sidebar's own
origin is taken back instead."
  (when (eq event-type 'load-changed)
    (let ((status (nth 3 last-input-event))
          (uri (when-let ((xw (agent-repl-sidebar-view-xwidget))) (xwidget-webkit-uri xw))))
      (agent-repl--info agent-repl--sidebar-view-central "elisp.sidebar-view.load-changed status=%s" status)
      (when (and (equal status "load-committed") (not (equal uri "about:blank")))
        (if-let ((strayed (agent-repl--webview-strayed-uri agent-repl--sidebar-view-url uri)))
            (agent-repl--sidebar-view-take-back strayed)
          (agent-repl--sidebar-view-replay))))))

(defun agent-repl--sidebar-view-take-back (uri)
  "Navigate the sidebar, which strayed to URI, back to its own page.
An outward link never opens in a web view (owner ruling, 2026-10-26); one
that reached the sidebar is a defect in the page, recorded at ERROR."
  (agent-repl--error agent-repl--sidebar-view-central
                     "elisp.sidebar-view.navigated-away uri=%s: an outward link escaped the sidebar's routing; the sidebar is taken back and the link was not opened"
                     uri)
  (agent-repl--frontend-webview-navigate-widget (agent-repl-sidebar-view-xwidget) agent-repl--sidebar-view-url))

(defun agent-repl--sidebar-view-replay ()
  "Hand the sidebar everything it should know, after a load: it starts empty."
  (when agent-repl--sidebar-view-daemon-origin
    (agent-repl--sidebar-view-call "setDaemon" agent-repl--sidebar-view-daemon-origin))
  (when agent-repl--sidebar-view-shown
    (agent-repl--sidebar-view-call "markShown" agent-repl--sidebar-view-shown))
  (when (and agent-repl--webhost-log-level
             (> (cdr agent-repl--webhost-log-level) (agent-repl--now-unix-ms)))
    (agent-repl--sidebar-view-call "setLogLevel" (symbol-name (car agent-repl--webhost-log-level))
                                   (cdr agent-repl--webhost-log-level)))
  (agent-repl--info agent-repl--sidebar-view-central "elisp.sidebar-view.replayed shown=%s daemon=%s"
                    agent-repl--sidebar-view-shown agent-repl--sidebar-view-daemon-origin))

(defun agent-repl-sidebar-view-navigate (conn)
  "Load the sidebar again at its URL on CONN's daemon; its load replays the rest.
Called whenever the host reloads (`agent-repl--webhost-navigate'), so the
two documents always run the webapp build the daemon serves."
  (if-let ((xw (agent-repl-sidebar-view-xwidget)))
      (progn
        (setq agent-repl--sidebar-view-url (agent-repl-sidebar-view-url conn)
              agent-repl--sidebar-view-daemon-origin nil)
        (agent-repl--frontend-webview-navigate-widget xw agent-repl--sidebar-view-url)
        (agent-repl--info agent-repl--sidebar-view-central "elisp.sidebar-view.reloaded url=%s"
                          agent-repl--sidebar-view-url))
    (agent-repl--log agent-repl--sidebar-view-central "elisp.sidebar-view.reload-skipped reason=no-sidebar")))

;;;; ---- Calls -----------------------------------------------------------------

(defun agent-repl--sidebar-view-script (method &rest args)
  "The script calling the sidebar page's METHOD with ARGS.
See `agent-repl--webview-script'."
  (agent-repl--webview-script "agentReplSidebar" "__agentReplSidebarPending" method args))

(defun agent-repl--sidebar-view-call (method &rest args)
  "Call the sidebar page's METHOD with ARGS; non-nil when the call was sent.
A call with no sidebar to send it to sends nothing: the desired state it
served is recorded by the caller and replayed when the sidebar loads."
  (if-let ((xw (agent-repl-sidebar-view-xwidget)))
      (progn
        (agent-repl--frontend-webview-execute-script-value
         xw (apply #'agent-repl--sidebar-view-script method args) 'agent-repl--sidebar-view-answered)
        t)
    (agent-repl--log agent-repl--sidebar-view-central
                     "elisp.sidebar-view.call-skipped method=%s reason=no-sidebar" method)
    nil))

(defun agent-repl--sidebar-view-answered (raw)
  "Record the sidebar page's RAW answer to a call, and every note it carries.
Called BY SYMBOL from the read channel.  A page that lacks the call was
loaded from an older build: it is reloaded once on its own URL, and a page
that still lacks it after that is a daemon serving an older webapp than this
elisp, an ERROR no reload heals."
  (condition-case err
      (let* ((answer (agent-repl-webhost-parse-answer raw))
             (op (plist-get answer :op)))
        (agent-repl--log agent-repl--sidebar-view-central "elisp.sidebar-view.answer op=%s id=%s outcome=%s"
                         op (plist-get answer :id) (plist-get answer :outcome))
        (when (equal (plist-get answer :outcome) "unknown-call")
          (agent-repl--sidebar-view-predates-call op))
        (dolist (note (plist-get answer :notes))
          (agent-repl--webhost-record-note agent-repl--sidebar-view-central op note)))
    (error
     (agent-repl--error agent-repl--sidebar-view-central
                        "elisp.sidebar-view.answer-unreadable raw=%S error=%S" raw err))))

(defun agent-repl--sidebar-view-predates-call (op)
  "The sidebar page has no OP: reload it on its own URL, once per URL."
  (let ((url agent-repl--sidebar-view-url))
    (if (equal url agent-repl--sidebar-view-reloaded-for-call)
        (agent-repl--error agent-repl--sidebar-view-central
                           "elisp.sidebar-view.predates-call op=%s action=none reason=served-webapp-predates-elisp url=%s"
                           op url)
      (setq agent-repl--sidebar-view-reloaded-for-call url)
      (agent-repl--info agent-repl--sidebar-view-central
                        "elisp.sidebar-view.predates-call op=%s action=reload url=%s" op url)
      (when-let ((xw (agent-repl-sidebar-view-xwidget)))
        (agent-repl--frontend-webview-navigate-widget xw url)))))

(defun agent-repl-sidebar-view-mark-shown (id)
  "Highlight workspace ID's row in the sidebar; kept for its reloads."
  (unless (equal id agent-repl--sidebar-view-shown)
    (setq agent-repl--sidebar-view-shown id)
    (agent-repl--sidebar-view-call "markShown" id)))

(defun agent-repl-sidebar-view-follow-daemon (origin)
  "Point the sidebar's calls at the daemon at ORIGIN; kept for its reloads."
  (unless (equal origin agent-repl--sidebar-view-daemon-origin)
    (setq agent-repl--sidebar-view-daemon-origin origin)
    (agent-repl--info agent-repl--sidebar-view-central "elisp.sidebar-view.follows-daemon to=%s" origin)
    (agent-repl--sidebar-view-call "setDaemon" origin)))

(defun agent-repl-sidebar-view-set-log-level (level until-ms)
  "Raise the sidebar to LEVEL until UNTIL-MS; info with nil closes the window.
The window itself is kept by webhost.el (`agent-repl--webhost-log-level')."
  (agent-repl--sidebar-view-call "setLogLevel" (symbol-name level) until-ms))

(defun agent-repl-sidebar-view-focus-moved ()
  "Tell the sidebar page that Emacs moved its focus."
  (when (agent-repl-sidebar-view-xwidget)
    (agent-repl--sidebar-view-call "focusMoved")))

;;;; ---- The window ------------------------------------------------------------

(defun agent-repl-sidebar-view-window (&optional frame)
  "Return the window showing the sidebar on FRAME, or nil."
  (when-let ((buf (agent-repl-sidebar-view-buffer)))
    (get-buffer-window buf (or frame (selected-frame)))))

(defun agent-repl-sidebar-view-window-p (win)
  "Non-nil when WIN shows the sidebar."
  (and (window-live-p win)
       (eq (window-buffer win) (agent-repl-sidebar-view-buffer))
       (agent-repl-sidebar-view-buffer)
       t))

(defun agent-repl--sidebar-view-width (frame)
  "The sidebar's opening width on FRAME, in columns."
  (max window-min-width
       (round (* agent-repl-sidebar-view-width-fraction (frame-width frame)))))

(defun agent-repl-sidebar-view-show (conn)
  "Show the sidebar, on CONN's daemon, in its window at the frame's left edge.
A left side window: the frame's whole height, one for every workspace, with
no mode line.  Nothing is changed when it already stands.  Returns the window."
  (agent-repl-sidebar-view-ensure conn)
  (or (agent-repl-sidebar-view-window)
      (let* ((buf (agent-repl-sidebar-view-buffer))
             (width (agent-repl--sidebar-view-width (selected-frame)))
             (win (display-buffer-in-side-window
                   buf `((side . left) (slot . -1) (window-width . ,width) (dedicated . t)
                         (window-parameters . ((no-delete-other-windows . t)
                                               (no-other-window . t)
                                               (mode-line-format . none)))))))
        (unless (window-live-p win)
          (error "agent-repl: the sidebar's window could not be made (buffer=%S)" buf))
        (window-preserve-size win t t)
        (agent-repl--info agent-repl--sidebar-view-central "elisp.sidebar-view.shown window=%S width=%d" win width)
        win)))

(defun agent-repl-sidebar-view-hide ()
  "Take the sidebar's window off the frame, keeping its web view.
Non-nil when a window was removed."
  (when-let ((win (agent-repl-sidebar-view-window)))
    (delete-window win)
    (agent-repl--info agent-repl--sidebar-view-central "elisp.sidebar-view.hidden window=%S" win)
    t))

(provide 'sidebar-view)

;;; sidebar-view.el ends here
