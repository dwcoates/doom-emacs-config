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
;; THE CLOCK RUNS ONLY WHILE THE SYSTEM IS IN USE (owner ruling, 2026-10-17).
;; In use means some workspace started a turn within the last
;; `agent-repl-page-unload-idle-after' seconds.  Once that long passes with no
;; turn anywhere, nobody is using the system, and the clock stops and resets:
;; the user coming back will certainly reach for a page the idle time would
;; otherwise have unloaded.  The next turn anywhere starts every page's hold
;; afresh from that turn, so a page is unloaded only for going unlooked while
;; the user was working elsewhere.
;;
;; LOOKED AT MEANS SHOWN FOR A WHILE.  A page shown for less than
;; `agent-repl-page-unload-dwell' seconds was passed through, as a held switch
;; key passes through every workspace on the way to another, and it does not
;; count.  A page's mount counts as a look, so a page made ahead of the look at
;; startup ages out like any other when it is never visited.
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
(defvar agent-repl-link-up-functions)
(defvar agent-repl-roster-update-functions)
(defvar agent-repl-roster-status-change-functions)
(defvar agent-repl-roster-running-statuses)

(defgroup agent-repl-page-unload nil
  "Unloading the workspace pages nobody looks at."
  :group 'agent-repl)

(defcustom agent-repl-page-unload-after 600
  "Seconds of use a workspace's page is held after it was last looked at."
  :type 'integer
  :group 'agent-repl-page-unload)

(defcustom agent-repl-page-unload-idle-after 300
  "Seconds with no turn started anywhere after which the system is not in use.
The hold's clock stops then, and starts afresh at the next turn."
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

(defvar agent-repl-page-unload--last-turn-at nil
  "When a turn last started in any workspace, or nil before the first.")

(defvar agent-repl-page-unload--in-use-since nil
  "When the system's present spell of use began: the first turn after idle.")

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
      (setq agent-repl-page-unload--shown ws
            agent-repl-page-unload--shown-since now))))

(defun agent-repl-page-unload-note-mounted (ws)
  "Record that WS's page was just mounted: a look, and no longer unloaded."
  (agent-repl--ws-put ws :page-looked-at (agent-repl-page-unload--now))
  (when (agent-repl--ws-get ws :page-unloaded)
    (agent-repl--ws-put ws :page-unloaded nil)
    (agent-repl--info ws "elisp.page-unload.reloaded ws=%s" ws)))

(defun agent-repl-page-unload--in-use-p (now)
  "Non-nil when some workspace started a turn within the idle span before NOW."
  (and agent-repl-page-unload--last-turn-at
       (<= (- now agent-repl-page-unload--last-turn-at) agent-repl-page-unload-idle-after)))

(defun agent-repl-page-unload-note-turn ()
  "Record that a turn started in some workspace: the system is in use.
A turn after an idle spell starts the hold's clock afresh."
  (let ((now (agent-repl-page-unload--now)))
    (unless (agent-repl-page-unload--in-use-p now)
      (setq agent-repl-page-unload--in-use-since now)
      (agent-repl--log agent-repl--page-unload-central
                       "elisp.page-unload.in-use since-idle-s=%s"
                       (if agent-repl-page-unload--last-turn-at
                           (round (- now agent-repl-page-unload--last-turn-at))
                         "never")))
    (setq agent-repl-page-unload--last-turn-at now)))

(defun agent-repl-page-unload-on-status-change (_ws previous current)
  "Note a turn when a workspace moves from PREVIOUS into the running CURRENT."
  (when (and (memq current agent-repl-roster-running-statuses)
             (not (memq previous agent-repl-roster-running-statuses)))
    (agent-repl-page-unload-note-turn)))

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

(defun agent-repl-page-unload-pass ()
  "Unload every held page whose workspace is hidden or not looked at lately.
Neither the page the host shows nor the current workspace's is ever
unloaded.  Answers how many were unloaded."
  (let ((now (agent-repl-page-unload--now))
        (unloaded 0))
    (dolist (ws (agent-repl--live-ws-names))
      (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
        (when (and (buffer-live-p buf)
                   (not (equal ws agent-repl-page-unload--shown))
                   (not (equal ws (agent-repl--ws-current-name))))
          (when-let ((reason (agent-repl-page-unload--reason ws now)))
            (agent-repl--frontend-detach-webview ws buf)
            (agent-repl--ws-put ws :page-unloaded t)
            (setq unloaded (1+ unloaded))
            (agent-repl--info ws "elisp.page-unload.unloaded ws=%s reason=%s unlooked-s=%d"
                              ws reason
                              (round (- now (or (agent-repl--ws-get ws :page-looked-at) now))))))))
    (when (> unloaded 0)
      (agent-repl--info agent-repl--page-unload-central
                        "elisp.page-unload.pass unloaded=%d" unloaded))
    unloaded))

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
(add-hook 'agent-repl-roster-status-change-functions #'agent-repl-page-unload-on-status-change)

(provide 'page-unload)
;;; page-unload.el ends here
