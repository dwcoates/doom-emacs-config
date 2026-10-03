;;; startup.el --- The editor's startup: every workspace brought up, its tab opened in order -*- lexical-binding: t; -*-

;;; Commentary:

;; WHEN EMACS STARTS, EVERY WORKSPACE IS BROUGHT UP WITHOUT THE USER
;; SWITCHING TO IT (owner requirements 2026-10-02,
;; docs/protobuf-design/startup-and-fault-domains.md).  The daemon brings
;; each one's session up and tells THIS Emacs, over its WatchDaemon stream
;; (`agentrepl.v1.DaemonStartupEvent'), every step and each workspace's
;; GO-AHEAD, in the daemon's registry order.  This file is the editor's half:
;;
;;   - every workspace the roster names is PRE-CREATED -- its perspective,
;;     its input buffer, its webview and page -- while its TAB stays out of
;;     the tab bar (`agent-repl-startup-holds-p');
;;   - a workspace's tab opens only once BOTH its go-ahead has arrived AND
;;     its page has loaded, and strictly in go-ahead order: tab k waits for
;;     tab k-1 however early k was ready;
;;   - every step is ONE minibuffer line and one INFO record
;;     (`agent-repl--phase-echo'), worded as the design record lists them.
;;
;; THE STARTUP IS EXPECTED FROM THE MOMENT THIS PROCESS STARTS.  A new
;; Emacs process states a new identity on its first WatchDaemon
;; (`agent-repl-editor-instance'), and the serving daemon answers it with a
;; startup run.  Holding from process start, rather than from the run's
;; first event, is what makes "no tab opens out of order" structural: a
;; roster push that reaches Emacs before the run's `opening' cannot open a
;; tab ahead of its go-ahead.  The hold ends at the run's `finished' (once
;; every go-ahead's tab has opened), or when the stream the run rode ends
;; first -- events are never replayed, so a reconnect reads the roster
;; instead and every held tab opens in roster order.
;;
;; The steps before any stream exists (building, starting and connecting to
;; the daemon) are daemon.el's lines.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(declare-function agent-repl--phase-echo "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--ws-by-ref-id "workspace" (id))
(declare-function agent-repl-roster-row-name "roster" (row))
(declare-function agent-repl-roster-refresh-order "roster" ())
(declare-function agent-repl-roster-apply-current "roster" ())
(declare-function agent-repl-host-request-switch "host" (ws trigger))
(declare-function agent-repl-roster-drawn-tab-order "roster" ())
(declare-function agent-repl--ensure-input-buffer "panels" (ws))
(declare-function agent-repl--frontend-precreate-refusal "frontend" (ws))
(declare-function agent-repl--frontend-webview-read-script "frontend" (buf script callback))
(declare-function agent-repl--ws-get "workspace" (ws key))

(defvar agent-repl-roster--rows-by-id)
(defvar agent-repl-link-down-functions)
(defvar agent-repl-link-promote-functions)
(defvar agent-repl-roster-update-functions)

(defconst agent-repl-startup--central
  '(:agent-repl-central "the editor's startup spans every workspace")
  "The log scope every startup record that names no one workspace takes.")

;;;; ---- State --------------------------------------------------------------

(defvar agent-repl-startup--phase 'expected
  "Where this Emacs process's startup stands.
`expected' from process start until the daemon's run begins, `running'
while it is told, `done' once every held tab has opened.  A defvar, so a
hot reload of this file keeps the phase the process is in.")

(defvar agent-repl-startup--go-aheads nil
  "The ref ids the daemon has given a go-ahead, in the order it gave them.")

(defvar agent-repl-startup--next 0
  "Index into `agent-repl-startup--go-aheads' of the next tab to open.")

(defvar agent-repl-startup--released (make-hash-table :test 'equal)
  "Ref id -> t for every workspace whose tab the startup has opened.")

(defvar agent-repl-startup--loaded (make-hash-table :test 'equal)
  "Workspace name -> t for every workspace whose page has loaded.")

(defvar agent-repl-startup--loading-said (make-hash-table :test 'equal)
  "Ref id -> t once \"loading the page...\" was said for it.")

(defvar agent-repl-startup--selected nil
  "The workspace the startup selected, once one of its opened tabs is drawn.")

(defvar agent-repl-startup--finished nil
  "The run's `DaemonStartupFinished' plist once it arrived, else nil.")

(defun agent-repl-startup-active-p ()
  "Return non-nil while this process's startup holds tabs."
  (not (eq agent-repl-startup--phase 'done)))

(defun agent-repl-startup-holds-p (id)
  "Return non-nil while the startup keeps workspace ID's tab out of the bar.
Every workspace is held from process start until the startup opens its
tab (its go-ahead and its page both in) or the startup ends."
  (and (agent-repl-startup-active-p)
       id
       (not (gethash id agent-repl-startup--released))))

;;;; ---- Lines --------------------------------------------------------------

(defun agent-repl-startup--name (ref)
  "Return the display name the startup's lines use for workspace REF.
The tab name Emacs holds it under, else the roster row's name, else the
directory's base name -- the name the daemon itself registers by default."
  (let ((id (plist-get ref :id)))
    (or (and id (agent-repl--ws-by-ref-id id))
        (let ((row (and id (boundp 'agent-repl-roster--rows-by-id)
                        (gethash id agent-repl-roster--rows-by-id))))
          (and row (agent-repl-roster-row-name row)))
        (file-name-nondirectory (directory-file-name (or (plist-get ref :dir) ""))))))

(defun agent-repl-startup--say (fmt &rest args)
  "Say one startup line, FMT with ARGS, in the minibuffer and the log."
  (apply #'agent-repl--phase-echo agent-repl-startup--central fmt args))

(defun agent-repl-startup--say-step (ref step)
  "Say the line for workspace REF's STEP, a decoded step oneof."
  (let ((ws (agent-repl-startup--name ref))
        (value (plist-get step :value)))
    (pcase (plist-get step :arm)
      (:starting-session (agent-repl-startup--say "%s: starting session…" ws))
      (:waking (agent-repl-startup--say "%s: waking from sleep…" ws))
      (:resuming (agent-repl-startup--say "%s: resuming the conversation…" ws))
      (:vendor-retrying
       (agent-repl-startup--say "%s: Claude did not start, retrying (attempt %d)…"
                                ws (plist-get value :attempt)))
      (:vendor-rejected
       (agent-repl-startup--say "%s: Claude refused to start: %s." ws (plist-get value :cause)))
      (:vendor-failed (agent-repl-startup--say "%s: Claude failed to start after 10 minutes." ws))
      (:cold-gate (agent-repl-startup--say "%s: needs your answer to resume (large context)." ws))
      (:offline (agent-repl-startup--say "%s: offline, waiting for the network…" ws))
      (:waiting-for
       (agent-repl-startup--say "%s: ready, waiting for %s to open first…"
                                ws (agent-repl-startup--name (plist-get value :ahead))))
      (:failed
       (agent-repl-startup--say "%s: session failed to start: %s." ws (plist-get value :reason)))
      (arm (agent-repl--error agent-repl-startup--central
                              "elisp.startup.unknown-step arm=%S ws=%s" arm ws)))))

;;;; ---- The run --------------------------------------------------------------

(defun agent-repl-startup-handle (event)
  "Take one decoded `DaemonStartupEvent' EVENT off the WatchDaemon stream."
  (let* ((oneof (plist-get event :event))
         (value (plist-get oneof :value)))
    (pcase (plist-get oneof :arm)
      (:opening
       (setq agent-repl-startup--phase 'running)
       (agent-repl-startup--say "opening %d workspaces…" (plist-get value :workspaces)))
      (:workspace-step
       (agent-repl-startup--say-step (plist-get value :workspace) (plist-get value :step)))
      (:workspace-open
       (let ((id (plist-get (plist-get value :workspace) :id)))
         (agent-repl--info agent-repl-startup--central "elisp.startup.go-ahead id=%s position=%d"
                           id (1+ (length agent-repl-startup--go-aheads)))
         (setq agent-repl-startup--go-aheads
               (append agent-repl-startup--go-aheads (list id))))
       (agent-repl-startup--advance))
      (:finished
       (setq agent-repl-startup--finished value)
       (agent-repl-startup--advance))
      (arm (agent-repl--error agent-repl-startup--central
                              "elisp.startup.unknown-event arm=%S" arm)))))

(defun agent-repl-startup--page-ready-p (ws)
  "Return non-nil when WS's page has drawn, or WS can have no page at all.
An Emacs with no webview support, or a workspace whose frontend is not
the web gui, has no page to wait for."
  (or (gethash ws agent-repl-startup--loaded)
      (memq (and (fboundp 'agent-repl--frontend-precreate-refusal)
                 (agent-repl--frontend-precreate-refusal ws))
            '(:no-xwidget :not-gui))))

(defun agent-repl-startup--advance ()
  "Open every tab now due, strictly in go-ahead order, then finish if done."
  (let ((stop nil))
    (while (and (not stop)
                (< agent-repl-startup--next (length agent-repl-startup--go-aheads)))
      (let* ((id (nth agent-repl-startup--next agent-repl-startup--go-aheads))
             (ws (agent-repl--ws-by-ref-id id)))
        (cond
         ((null ws)
          ;; The roster has not reached this workspace yet: its push
          ;; re-runs this (`agent-repl-startup--on-roster-update').
          (agent-repl--log agent-repl-startup--central
                           "elisp.startup.waiting-for-roster id=%s" id)
          (setq stop t))
         ((not (agent-repl-startup--page-ready-p ws))
          (unless (gethash id agent-repl-startup--loading-said)
            (puthash id t agent-repl-startup--loading-said)
            (agent-repl-startup--say "%s: loading the page…" ws))
          (setq stop t))
         (t
          (agent-repl-startup--open id ws)
          (setq agent-repl-startup--next (1+ agent-repl-startup--next)))))))
  (when (and agent-repl-startup--finished
             (= agent-repl-startup--next (length agent-repl-startup--go-aheads)))
    (agent-repl-startup--finish)))

(defun agent-repl-startup--open (id ws)
  "Open workspace WS's tab (ref ID): its go-ahead and its page are both in.
THE FIRST DRAWN TAB OPENED IS SELECTED at once, and stays selected while
every later tab opens (owner request, 2026-10-03): a startup never shows no
workspace while the rest are coming up.  A tab of a repository held
collapsed is opened but not drawn, so it is never the one selected.  The
selection is asked of the daemon like every other switch, so the frame
follows the roster."
  (puthash id t agent-repl-startup--released)
  (agent-repl-roster-refresh-order)
  (let ((select (and (null agent-repl-startup--selected)
                     (member ws (agent-repl-roster-drawn-tab-order))
                     t)))
    (agent-repl--info ws "elisp.startup.tab-opened ws=%s id=%s selects=%s" ws id select)
    (agent-repl-startup--say "%s: ready." ws)
    (when select
      (setq agent-repl-startup--selected ws)
      (agent-repl-host-request-switch ws 'startup)))
  ;; The selection may name this workspace, and had no tab to land on.
  (agent-repl-roster-apply-current))

(defun agent-repl-startup--finish ()
  "End the startup: say how it went, and stop holding tabs."
  (let* ((done agent-repl-startup--finished)
         (failed (plist-get done :failed))
         (total (plist-get done :total)))
    (if (null failed)
        (agent-repl-startup--say "all %d workspaces ready." total)
      (agent-repl-startup--say "%d of %d workspaces ready; %s failed to start."
                               (plist-get done :ready) total
                               (mapconcat (lambda (f) (plist-get f :name)) failed ", ")))
    (agent-repl-startup--end "finished")))

(defun agent-repl-startup--end (why)
  "Stop holding tabs, for WHY; every tab still held opens in roster order."
  (setq agent-repl-startup--phase 'done)
  (agent-repl--info agent-repl-startup--central "elisp.startup.ended why=%s opened=%d"
                    why agent-repl-startup--next)
  (agent-repl-roster-refresh-order)
  (agent-repl-roster-apply-current))

;;;; ---- Edges ------------------------------------------------------------------

(defconst agent-repl-startup--drawn-attribute "data-conversation-drawn"
  "The attribute the page sets on its root element once its conversation drew.
The webapp sets it after the root feed's opening page is applied and painted
\(webapp/src/feed/conversation-drawn.ts).  An HTML load (`load-changed')
precedes it: the page is up before any of the conversation is.")

(defconst agent-repl-startup--probe-interval 0.25
  "Seconds between two reads of a loaded page that has not drawn yet.")

(defun agent-repl-startup--probe-script (ws)
  "The read that answers whether WS's page has drawn its conversation.
WS rides in the script and comes back in the reply, because the read's
callback must be a symbol (`agent-repl--frontend-webview-read-script').
The reply is always a string: a null reply is dropped without a callback."
  (format "(function(){return JSON.stringify({ws:%s,drawn:document.documentElement.hasAttribute(%s)});})()"
          (json-encode ws) (json-encode agent-repl-startup--drawn-attribute)))

(defun agent-repl-startup--probe (ws)
  "Ask WS's page whether its conversation is on screen.
Nothing is asked once the startup is over or WS's page is known drawn."
  (when (and (agent-repl-startup-active-p)
             (not (gethash ws agent-repl-startup--loaded)))
    (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
      (unless (and (buffer-live-p buf)
                   (agent-repl--frontend-webview-read-script
                    buf (agent-repl-startup--probe-script ws)
                    #'agent-repl-startup--on-probe))
        ;; The page went away under the probe; its next load asks again.
        (agent-repl--info ws "elisp.startup.probe-skipped ws=%s reason=no-webview" ws)))))

(defun agent-repl-startup--on-probe (raw)
  "Take the page's RAW reply to `agent-repl-startup--probe-script'.
A drawn page opens its tab when due; an undrawn one is asked again."
  (let* ((reply (condition-case err
                    (json-parse-string raw :object-type 'plist)
                  (error
                   (agent-repl--error '(:agent-repl-central "a reply naming no workspace cannot be routed")
                                      "elisp.startup.probe-unreadable raw=%S error=%S" raw err)
                   nil)))
         (ws (plist-get reply :ws)))
    (cond
     ((null reply) nil)
     ((not (stringp ws))
      (agent-repl--error '(:agent-repl-central "a reply naming no workspace cannot be routed")
                         "elisp.startup.probe-unreadable raw=%S error=no-workspace" raw))
     ((eq (plist-get reply :drawn) t)
      (agent-repl-startup--page-drawn ws))
     (t
      (agent-repl--log ws "elisp.startup.probe ws=%s drawn=nil" ws)
      (run-at-time agent-repl-startup--probe-interval nil #'agent-repl-startup--probe ws)))))

(defun agent-repl-startup-note-page-loaded (ws)
  "WS's page finished loading its HTML: ask whether its conversation drew.
The tab waits for the conversation on screen, not for the HTML."
  (when (agent-repl-startup-active-p)
    (agent-repl-startup--probe ws)))

(defun agent-repl-startup--page-drawn (ws)
  "Record that WS's conversation is on screen, and open any tab now due."
  (unless (gethash ws agent-repl-startup--loaded)
    (puthash ws t agent-repl-startup--loaded)
    (agent-repl--info ws "elisp.startup.page-drawn ws=%s" ws)
    (when (agent-repl-startup-active-p)
      (agent-repl-startup--advance))))

(defun agent-repl-startup-precreate (ws)
  "Pre-create WS's input buffer while the startup holds its tab.
Its webview is pre-created by the paced drain (webview-recovery.el), which
keeps its focus park: a visible but unfocused Emacs creates no page, and
so opens no tab, until it is looked at."
  (when (agent-repl-startup-active-p)
    (agent-repl--ensure-input-buffer ws)
    (agent-repl--log ws "elisp.startup.precreated ws=%s" ws)))

(defun agent-repl-startup--on-roster-update (&optional _roster)
  "Re-run the opening walk once a roster push may have named a held tab."
  (when (eq agent-repl-startup--phase 'running)
    (agent-repl-startup--advance)))

(defun agent-repl-startup--on-link-down (&optional _conn)
  "End a startup whose stream ended before it finished.
Events are never replayed: a reconnect reads the roster, so every held tab
opens now in roster order."
  (when (agent-repl-startup-active-p)
    (agent-repl-startup--end "stream-ended")))

(add-hook 'agent-repl-roster-update-functions #'agent-repl-startup--on-roster-update)
(add-hook 'agent-repl-link-down-functions #'agent-repl-startup--on-link-down)
(defun agent-repl-startup--on-link-promote (_old _new)
  "End a startup whose daemon handed the editor over to a successor.
A PROMOTION moves the editor onto a successor: the run rode the stream of
the daemon that is leaving, and is over with it.  The promote hook passes
the OLD and NEW connections, unlike the link-down hook\='s one."
  (agent-repl-startup--on-link-down))

;; A PROMOTION moves the editor onto a successor: the run rode the stream of
;; the daemon that is leaving, and is over with it.
(add-hook 'agent-repl-link-promote-functions #'agent-repl-startup--on-link-promote)

(provide 'agent-repl-startup)
;;; startup.el ends here
