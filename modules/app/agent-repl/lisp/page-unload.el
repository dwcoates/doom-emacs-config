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
;;     no page: one made before it was hidden is unloaded;
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
;; AN UNLOADED PAGE COMES BACK ON THE SWITCH TO IT.  The unload is the same
;; detach every remount uses (`agent-repl--frontend-detach-webview'), and the
;; switch mounts a workspace with no page as it mounts a new one.  Until then
;; the pre-creation pass (webview-recovery.el) leaves it unmade: the mount's
;; one eligibility answer refuses it as `:unloaded'.
;;; Code:
(require 'cl-lib)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key value))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--frontend-detach-webview "agent-repl-frontend" (ws buf))
(declare-function agent-repl-open-tabs-hidden-p "open-tabs" (ws))
(declare-function agent-repl--frontend-precreate-webview "agent-repl-frontend" (ws))
(declare-function agent-repl--panels-on-view-created "panels" (ws))
(declare-function agent-repl-host-report-page-unloading "host" (ws on-reported on-not-reported))
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
    (when (and previous (not (equal previous ws))
               (>= (- now agent-repl-page-unload--shown-since) agent-repl-page-unload-dwell))
      (agent-repl--ws-put previous :page-looked-at now))
    (unless (equal previous ws)
      (agent-repl-page-unload--note-use now)
      (setq agent-repl-page-unload--shown ws
            agent-repl-page-unload--shown-since now))))

(defun agent-repl-page-unload-note-mounted (ws)
  "Record that WS's page was just mounted: a look, and no longer unloaded."
  (agent-repl--ws-put ws :page-looked-at (agent-repl-page-unload--now))
  (when (agent-repl--ws-get ws :page-unloaded)
    (agent-repl--ws-put ws :page-unloaded nil)
    (agent-repl--info ws "elisp.page-unload.reloaded ws=%s" ws)))

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
  "Non-nil when WS's page was unloaded and nobody has switched to it since."
  (and (agent-repl--ws-get ws :page-unloaded) t))

(defun agent-repl-page-unload-remount (ws)
  "Mount WS's unloaded page again, for the switch to WS, and restore its panels.
Called from the switch's panel restore, which declined for want of the view
this makes (panels.el); the restore runs again on the new view."
  (agent-repl--ws-put ws :page-unloaded nil)
  (agent-repl--info ws "elisp.page-unload.remount ws=%s" ws)
  (agent-repl--frontend-precreate-webview ws)
  (agent-repl--panels-on-view-created ws))

(defun agent-repl-page-unload--reason (ws now)
  "Why WS's page should be unloaded at NOW, or nil when it should be held.
A page with no recorded look (one mounted before this module ran) is
stamped as looked at now, so a reload of this module unloads nothing.  A
page goes unlooked only while the system is in use, and only for the time
since the later of its look and the start of the present spell of use."
  (cond
   ((and (fboundp 'agent-repl-open-tabs-hidden-p) (agent-repl-open-tabs-hidden-p ws)) 'hidden)
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
unloaded, and nor is one whose unload is already being reported."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (and (buffer-live-p buf)
         (not (equal ws agent-repl-page-unload--shown))
         (not (equal ws (agent-repl--ws-current-name)))
         (not (agent-repl--ws-get ws :page-unloading))
         buf)))

(defun agent-repl-page-unload--unload (ws reason now)
  "Unload WS\='s page for REASON, found at NOW, once the daemon is told.
The page is detached only when the report answers success and the page
is still one a pass may unload: the user may have switched to it while
the report was in flight."
  (agent-repl--ws-put ws :page-unloading t)
  (agent-repl-host-report-page-unloading
   ws
   (lambda ()
     (agent-repl--ws-put ws :page-unloading nil)
     (if-let ((buf (agent-repl-page-unload--holdable-p ws)))
         (progn
           (agent-repl--frontend-detach-webview ws buf)
           (agent-repl--ws-put ws :page-unloaded t)
           (agent-repl--info ws "elisp.page-unload.unloaded ws=%s reason=%s unlooked-s=%d"
                             ws reason
                             (round (- now (or (agent-repl--ws-get ws :page-looked-at) now)))))
       (agent-repl--info ws "elisp.page-unload.kept ws=%s reason=%s why=in-use-since-report"
                         ws reason)))
   (lambda ()
     (agent-repl--ws-put ws :page-unloading nil)
     (agent-repl--info ws "elisp.page-unload.kept ws=%s reason=%s why=not-reported" ws reason))))

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

(provide 'page-unload)
;;; page-unload.el ends here
