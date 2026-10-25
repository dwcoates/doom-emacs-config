;;; page-unload.el --- the pages nobody looks at are unloaded -*- lexical-binding: t; -*-
;;; Commentary:
;; THE WEB PROCESS IS AS SLOW AS IT IS BIG.  Every workspace's page lives in
;; the one host web view (webhost.el), so in one web content process with one
;; JS heap.  Above about a gigabyte WebKit's memory-pressure handler runs every
;; 30 seconds, and every full collection marks every live page: the main-thread
;; stalls that put a switch's slow tail at 140-230ms (switch bench,
;; 2026-10-17) grow with the pages held.  So a page is held only while its
;; workspace is in use (owner ruling, 2026-10-17):
;;
;;   - a HIDDEN workspace (hidden, or in a collapsed repository section) holds
;;     no page while the user is not looking at it: one made before it was
;;     hidden, or while the user stood on it, is unloaded;
;;   - a workspace nobody has LOOKED AT for `agent-repl-page-unload-after'
;;     seconds of USE is unloaded too.
;;
;; THE CLOCK RUNS ONLY WHILE THE SYSTEM IS IN USE (owner rulings, 2026-10-17
;; and 2026-10-18).  In use means the user switched workspaces within the last
;; `agent-repl-page-unload-idle-after' seconds; a switch is the one sign that the
;; user is there, since an agent's turn runs as well with nobody there.  Once that
;; long passes with no switch, nobody is using the system, and the clock stops
;; and resets: the user coming back will certainly reach for a page the idle
;; time would otherwise have unloaded.  The next switch starts every page's
;; hold afresh from it, so a page is unloaded only for going unlooked while the
;; user was working elsewhere.
;;
;; LOOKED AT MEANS SHOWN FOR A WHILE.  A page shown for less than
;; `agent-repl-page-unload-dwell' seconds was passed through, as a held switch
;; key passes through every workspace on the way to another, and it does not
;; count.  A page's mount counts as a look, so a page made ahead of the look at
;; startup ages out like any other when it is never visited.
;;
;; AN UNLOAD IS INVISIBLE (owner ruling, 2026-10-20).  It is a performance
;; measure behind the scenes, never a state the user sees: the workspace's
;; status reads exactly as it would with its page held.  The page's web
;; stream ends with the page, and the daemon drew that end as a broken link,
;; blue, until the user switched back.  So the daemon is told FIRST
;; (`agent-repl-host-report-page-unloading'), and the page is unloaded only
;; once it has answered: the report cannot lose a race with the stream's end.
;; A report that does not land leaves the page loaded.
;;
;; AN UNLOAD DROPS THE PAGE AND KEEPS THE SLOT.  The page is what costs: it
;; lives in the one web content process (webhost.el).  The workspace's slot
;; buffer is free, a display of the host's one xwidget, and its windows are
;; the workspace's panels.  Killing the slot with the page (as the unload once
;; did) left the workspace's saved layout naming a dead buffer: the switch
;; back found its panels closed and re-showed them, every time (2026-10-24).
;; So a page is UNLOADED exactly when its workspace holds a live slot the host
;; holds no page for (`agent-repl-page-unloaded-p'), a fact read off the two,
;; never a flag beside them that could disagree.
;;
;; AN UNLOADED PAGE COMES BACK THE MOMENT ITS SLOT IS SHOWN.  The host shows
;; whatever page the displayed slot names (`agent-repl-webhost-show'), and
;; showing an unloaded one puts its page back first
;; (`agent-repl-page-unload--return'), so the panels stand throughout.
;;
;; A REMOUNT THE USER LEFT BEFORE IT DREW IS ABANDONED (owner ruling,
;; 2026-10-24).  A remounted page boots in the one web content process every
;; page shares, so while it loads it holds up the page the user flipped on
;; to.  A user who leaves a page before its conversation has drawn is
;; flipping through workspaces and does not want that one, so its load is
;; cancelled: the host is asked whether the page has drawn (its `drawn'
;; call), and a page that has not is unloaded as any other is, the daemon
;; told first (`agent-repl-page-unload--abandon').  It comes back on the
;; next switch to it.
;;; Code:
(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key value))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl-webhost-remove "webhost" (ws))
(declare-function agent-repl-webhost-ensure-page "webhost" (ws))
(declare-function agent-repl-webhost-has-page-p "webhost" (ws))
(declare-function agent-repl-open-tabs-withheld-p "open-tabs" (ws))
(declare-function agent-repl-host-report-page-unloading "host" (ws on-reported on-not-reported))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-by-ref-id "agent-repl-workspace" (id))
(declare-function agent-repl--webhost-id "webhost" (ws))
(declare-function agent-repl--webhost-answered "webhost" (raw))
(declare-function agent-repl-webhost-parse-answer "webhost" (raw))
(declare-function agent-repl-webhost-read "webhost" (method args callback))
(defvar agent-repl-link-up-functions)
(defvar agent-repl-roster-update-functions)

(defgroup agent-repl-page-unload nil
  "Unloading the workspace pages nobody looks at."
  :group 'agent-repl)

(defcustom agent-repl-page-unload-after 600
  "Seconds of use a workspace's page is held after it was last looked at."
  :type 'integer
  :group 'agent-repl-page-unload)

(defcustom agent-repl-page-unload-idle-after 300
  "Seconds with no workspace switch after which the system is not in use.
The hold's clock stops then, and starts afresh at the next switch."
  :type 'integer
  :group 'agent-repl-page-unload)

(defcustom agent-repl-page-unload-dwell 1
  "Seconds a page must be shown for the showing to count as a look."
  :type 'number
  :group 'agent-repl-page-unload)

(defcustom agent-repl-page-unload-interval 60
  "Seconds between the passes that unload pages nobody has looked at."
  :type 'integer
  :group 'agent-repl-page-unload)

(defconst agent-repl--page-unload-central
  '(:agent-repl-central "a page-unload pass spans workspaces")
  "The log scope of a pass's own records.")

(defvar agent-repl-page-unload--clock #'float-time
  "Answers the current time in seconds.  Tests bind it.")

(defvar agent-repl-page-unload--shown nil
  "The workspace whose page the host shows, or nil.")

(defvar agent-repl-page-unload--shown-since nil
  "When `agent-repl-page-unload--shown' was shown.")

(defvar agent-repl-page-unload--timer nil
  "The repeating timer that runs the pass, while one is running.")

(defvar agent-repl-page-unload--last-switch-at nil
  "When the user last switched workspaces, or nil before the first switch.")

(defvar agent-repl-page-unload--in-use-since nil
  "When the system's present spell of use began: the first switch after idle.")

(defun agent-repl-page-unload--now ()
  "The current time, in seconds."
  (funcall agent-repl-page-unload--clock))

(defun agent-repl-page-unload-note-shown (ws)
  "Record that the host now shows WS's page.
The page shown until now was looked at when it was shown for at least
`agent-repl-page-unload-dwell' seconds."
  (let ((now (agent-repl-page-unload--now))
        (previous agent-repl-page-unload--shown))
    (when (agent-repl-page-unloaded-p ws)
      (agent-repl-page-unload--return ws))
    (when (and previous (not (equal previous ws))
               (>= (- now agent-repl-page-unload--shown-since) agent-repl-page-unload-dwell))
      (agent-repl--ws-put previous :page-looked-at now))
    (unless (equal previous ws)
      (agent-repl-page-unload--note-use now)
      (setq agent-repl-page-unload--shown ws
            agent-repl-page-unload--shown-since now)
      (when (and previous (agent-repl--ws-get previous :page-remounting))
        (agent-repl-page-unload--abandon previous)))))

(defun agent-repl-page-unload-note-mounted (ws)
  "Record that WS's page was just mounted: a look."
  (agent-repl--ws-put ws :page-looked-at (agent-repl-page-unload--now)))

(defun agent-repl-page-unload--in-use-p (now)
  "Non-nil when the user switched workspaces within the idle span before NOW."
  (and agent-repl-page-unload--last-switch-at
       (<= (- now agent-repl-page-unload--last-switch-at) agent-repl-page-unload-idle-after)))

(defun agent-repl-page-unload--note-use (now)
  "Record that the user switched workspaces at NOW: the system is in use.
A switch after an idle spell starts the hold's clock afresh."
  (unless (agent-repl-page-unload--in-use-p now)
    (setq agent-repl-page-unload--in-use-since now)
    (agent-repl--log agent-repl--page-unload-central
                     "elisp.page-unload.in-use since-idle-s=%s"
                     (if agent-repl-page-unload--last-switch-at
                         (round (- now agent-repl-page-unload--last-switch-at))
                       "never")))
  (setq agent-repl-page-unload--last-switch-at now))

(defun agent-repl-page-unloaded-p (ws)
  "Non-nil when WS holds a live slot whose page the host does not hold.
That is what an unload leaves (`agent-repl-page-unload--unload'), read off
the slot and the host rather than kept as a flag beside them."
  (and (buffer-live-p (agent-repl--ws-get ws :frontend-buffer))
       (not (agent-repl-webhost-has-page-p ws))))

(defun agent-repl-page-unload--return (ws)
  "Put WS's unloaded page back in the host, its slot being shown.
The slot and its windows stood throughout; only the page comes back.  A
page the user leaves before it draws is unloaded again
\(`agent-repl-page-unload--abandon')."
  (agent-repl--ws-put ws :page-remounting t)
  (agent-repl-page-unload-note-mounted ws)
  (agent-repl--info ws "elisp.page-unload.returned ws=%s" ws)
  (agent-repl-webhost-ensure-page ws))

(defun agent-repl-page-unload--reason (ws now)
  "Why WS's page should be unloaded at NOW, or nil when it should be held.
A page with no recorded look (one mounted before this module ran) is
stamped as looked at now, so a reload of this module unloads nothing.  A
page goes unlooked only while the system is in use, and only for the time
since the later of its look and the start of the present spell of use."
  (cond
   ((and (fboundp 'agent-repl-open-tabs-withheld-p) (agent-repl-open-tabs-withheld-p ws)) 'hidden)
   ((null (agent-repl--ws-get ws :page-looked-at))
    (agent-repl--ws-put ws :page-looked-at now)
    nil)
   ((not (agent-repl-page-unload--in-use-p now)) nil)
   ((> (- now (max (agent-repl--ws-get ws :page-looked-at) agent-repl-page-unload--in-use-since))
       agent-repl-page-unload-after)
    'unlooked)))

(defun agent-repl-page-unload--holdable-p (ws)
  "The page of WS, when it is one a pass may unload, else nil.
Neither the page the host shows nor the current workspace\='s is ever
unloaded, nor one already unloaded, nor one whose unload is already being
reported."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (and (buffer-live-p buf)
         (agent-repl-webhost-has-page-p ws)
         (not (equal ws agent-repl-page-unload--shown))
         (not (equal ws (agent-repl--ws-current-name)))
         (not (agent-repl--ws-get ws :page-unloading))
         buf)))

(defun agent-repl-page-drop-announced (ws still-p drop on-kept)
  "Drop open workspace WS\='s page by calling DROP, once the daemon is told.
EVERY PATH THAT DROPS THE PAGE OF A WORKSPACE THAT STAYS OPEN COMES
THROUGH HERE: the unload pass, a hide, closing the panel.  The page\='s web
stream ends with the page, and a stream that ends unannounced is drawn as
a broken link, blue (2026-10-25: a fold of a repository section drew a
busy workspace severed).  So the daemon is told first
\(`agent-repl-host-report-page-unloading'), and DROP runs only once it has
answered success AND STILL-P, called with no argument, still answers
non-nil: what asked for the drop may have changed while the report was in
flight.  Otherwise ON-KEPT is called with `changed' or `not-reported', and
the page stays loaded.  The caller records each outcome.

WS\='s `:page-unloading' is set while the report is in flight.

A workspace that is ENDING (closed, killed, merged) drops its page
unannounced (`agent-repl--frontend-release-workspace-webview'): the daemon
ended it first, so its stream\='s end draws nothing."
  (agent-repl--ws-put ws :page-unloading t)
  (agent-repl-host-report-page-unloading
   ws
   (lambda ()
     (agent-repl--ws-put ws :page-unloading nil)
     (if (funcall still-p)
         (funcall drop)
       (funcall on-kept 'changed)))
   (lambda ()
     (agent-repl--ws-put ws :page-unloading nil)
     (funcall on-kept 'not-reported))))

(defun agent-repl-page-unload--unload (ws reason now)
  "Unload WS\='s page for REASON, found at NOW, once the daemon is told.
The page is detached only when the report answers success and the page
is still one a pass may unload: the user may have switched to it while
the report was in flight."
  (agent-repl-page-drop-announced
   ws
   (lambda () (agent-repl-page-unload--holdable-p ws))
   (lambda ()
     ;; THE PAGE GOES, THE SLOT STAYS: its windows are the panels.
     (agent-repl-webhost-remove ws)
     (agent-repl--ws-put ws :page-remounting nil)
     (agent-repl--info ws "elisp.page-unload.unloaded ws=%s reason=%s unlooked-s=%d"
                       ws reason
                       (round (- now (or (agent-repl--ws-get ws :page-looked-at) now)))))
   (lambda (why)
     (agent-repl--info ws "elisp.page-unload.kept ws=%s reason=%s why=%s"
                       ws reason (pcase why
                                   ('changed "in-use-since-report")
                                   ('not-reported "not-reported")
                                   (_ (error "Unknown kept reason %S" why)))))))

(defun agent-repl-page-unload--abandon (ws)
  "Ask the host whether WS\='s remounted page has drawn, the user having left it.
The answer goes to `agent-repl-page-unload--on-drawn-answer', which unloads
a page that has not.  With no host there is no page loading to cancel."
  (if-let ((id (agent-repl--webhost-id ws)))
      (if (agent-repl-webhost-read "drawn" (list id) 'agent-repl-page-unload--on-drawn-answer)
          (agent-repl--log ws "elisp.page-unload.abandon-asked ws=%s id=%s" ws id)
        (agent-repl--ws-put ws :page-remounting nil)
        (agent-repl--info ws "elisp.page-unload.abandon-skipped ws=%s reason=no-host" ws))
    ;; A remounted page was mounted at its workspace's ref: one gone since is a bug.
    (agent-repl--error ws "elisp.page-unload.abandon-failed ws=%s reason=no-ref" ws)
    (error "Remounted workspace %s has no ref" ws)))

(defun agent-repl-page-unload--on-drawn-answer (raw)
  "Cancel the remounted page\='s load that RAW, the host\='s `drawn' answer, names.
A page still loading is unloaded (`agent-repl-page-unload--unload'), the
daemon told first, unless the user is back on it.  A page that has drawn,
or that the host no longer holds, is left as it is.  Called BY SYMBOL from
the read channel (`agent-repl-webhost-read')."
  (agent-repl--webhost-answered raw)
  (condition-case err
      (let* ((answer (agent-repl-webhost-parse-answer raw))
             (id (plist-get answer :id))
             (outcome (plist-get answer :outcome))
             (ws (and (stringp id) (agent-repl--ws-by-ref-id id))))
        (cond
         ((null ws)
          (agent-repl--info agent-repl--page-unload-central
                            "elisp.page-unload.abandon-dropped id=%s reason=no-workspace" id))
         ((not (agent-repl--ws-get ws :page-remounting))
          (agent-repl--log ws "elisp.page-unload.abandon-dropped ws=%s reason=settled" ws))
         ((equal outcome "queued")
          ;; The host is loading, and replays every page once it starts: the
          ;; next switch away asks again.
          (agent-repl--info ws "elisp.page-unload.abandon-deferred ws=%s reason=host-loading" ws))
         ((not (equal outcome "present"))
          (agent-repl--ws-put ws :page-remounting nil)
          (agent-repl--info ws "elisp.page-unload.abandon-dropped ws=%s reason=%s" ws outcome))
         ((plist-get answer :drawn)
          (agent-repl--ws-put ws :page-remounting nil)
          (agent-repl--info ws "elisp.page-unload.abandon-dropped ws=%s reason=drawn drawn=%s"
                            ws (plist-get answer :drawn)))
         ((not (agent-repl-page-unload--holdable-p ws))
          (agent-repl--info ws "elisp.page-unload.abandon-deferred ws=%s reason=not-holdable" ws))
         (t
          (agent-repl--info ws "elisp.page-unload.abandon ws=%s reason=left-before-drawn" ws)
          (agent-repl-page-unload--unload ws 'abandoned (agent-repl-page-unload--now)))))
    (error
     (agent-repl--error agent-repl--page-unload-central
                        "elisp.page-unload.abandon-answer-unreadable raw=%S error=%S" raw err))))

(defun agent-repl-page-unload-pass ()
  "Unload every held page whose workspace is hidden or not looked at lately.
Each unload waits on the daemon being told (`agent-repl-page-unload--unload').
Answers how many unloads were started."
  (let ((now (agent-repl-page-unload--now))
        (started 0))
    (dolist (ws (agent-repl--live-ws-names))
      (when (agent-repl-page-unload--holdable-p ws)
        (when-let ((reason (agent-repl-page-unload--reason ws now)))
          (setq started (1+ started))
          (agent-repl-page-unload--unload ws reason now))))
    (when (> started 0)
      (agent-repl--info agent-repl--page-unload-central
                        "elisp.page-unload.pass started=%d" started))
    started))

(defun agent-repl-page-unload-start (&optional _conn)
  "Run the pass every `agent-repl-page-unload-interval' seconds.
Registered on `agent-repl-link-up-functions'; a timer already running is kept."
  (unless (timerp agent-repl-page-unload--timer)
    (setq agent-repl-page-unload--timer
          (run-with-timer agent-repl-page-unload-interval agent-repl-page-unload-interval
                          #'agent-repl-page-unload-pass))
    (agent-repl--log agent-repl--page-unload-central
                     "elisp.page-unload.started interval-s=%d after-s=%d idle-after-s=%d"
                     agent-repl-page-unload-interval agent-repl-page-unload-after
                     agent-repl-page-unload-idle-after)))

(defun agent-repl-page-unload-on-roster-update (&optional _roster)
  "Unload the pages a roster push hid, without waiting for the next pass."
  (agent-repl-page-unload-pass))

(add-hook 'agent-repl-link-up-functions #'agent-repl-page-unload-start)
(add-hook 'agent-repl-roster-update-functions #'agent-repl-page-unload-on-roster-update)

(defun agent-repl-page-unload-report-pageless ()
  "Tell the daemon about every open workspace whose page the host does not hold.
A page dropped before every drop was announced (`agent-repl-page-drop-announced')
left its workspace drawn severed, blue, until a page connected again.
Reporting it now has the daemon await its page instead, so the row heals.
A workspace whose report is already in flight is skipped.  Answers how
many reports were sent."
  (let ((sent 0))
    (dolist (ws (agent-repl--live-ws-names) sent)
      (unless (or (agent-repl-webhost-has-page-p ws) (agent-repl--ws-get ws :page-unloading))
        (when (agent-repl-host-report-page-unloading
               ws
               (lambda () (agent-repl--info ws "elisp.page-unload.pageless-reported ws=%s" ws))
               (lambda () (agent-repl--info ws "elisp.page-unload.pageless-unreported ws=%s" ws)))
          (setq sent (1+ sent)))))))

(defvar agent-repl-page-unload--loaded nil
  "Non-nil once this file has loaded: a later load is a hot reload.")

;; A HOT RELOAD HEALS THE ROWS AN UNANNOUNCED DROP LEFT SEVERED.  The first
;; load has no daemon link yet, and a fresh daemon awaits every page anyway.
(when agent-repl-page-unload--loaded
  (agent-repl--info agent-repl--page-unload-central "elisp.page-unload.reloaded reported=%d"
                    (agent-repl-page-unload-report-pageless)))
(setq agent-repl-page-unload--loaded t)

(provide 'page-unload)
;;; page-unload.el ends here
