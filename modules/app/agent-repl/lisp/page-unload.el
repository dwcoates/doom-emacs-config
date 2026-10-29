;;; page-unload.el --- the pages nobody looks at are unloaded -*- lexical-binding: t; -*-
;;; Commentary:
;; THE WEB PROCESS IS AS SLOW AS IT IS BIG.  Every workspace's page lives in
;; the one host web view (webhost.el), so in one web content process with one
;; JS heap.  Above about a gigabyte WebKit's memory-pressure handler runs every
;; 30 seconds, and every full collection marks every live page: the main-thread
;; stalls that put a switch's slow tail at 140-230ms (switch bench,
;; 2026-10-17) grow with the pages held.  So a page is held only while its
;; workspace needs it, by its STATUS (owner ruling, 2026-10-29, which
;; supersedes the looked-at clock of 2026-10-17, 2026-10-18 and 2026-10-26):
;;
;;   - WORKING (a turn running in front or behind, a permission or question
;;     ask, a vendor retry): always held, and loaded in the background the
;;     moment the workspace starts working;
;;   - IDLE (ready, done, interrupted, no session): held until an hour has
;;     passed since its last prompt, read as its last turn's end or its
;;     page's load, whichever is later.  A page loaded only because the user
;;     switched to it is unloaded the moment they switch away, unless the
;;     workspace started working while they were there: a prompt they sent;
;;   - MERGING, FAULTED (every blue and turquoise status), COLD-GATED,
;;     TRANSITIONAL (opening, closing, waiting) and MERGED: held only while
;;     shown, so unloaded the moment the workspace enters the status, or the
;;     moment the user switches away;
;;   - a HIDDEN workspace (one the user hid) holds no page while the user is
;;     not looking at it.
;;
;; The page on screen and the current workspace's are never unloaded.  A
;; workspace with no roster row yet decides nothing until its first push.
;;
;; A PAGE IS NOT LOADED AHEAD OF NEED (owner ruling, 2026-10-29).  A tab's
;; slot is made at its open with no page unless the workspace is shown or
;; working (`agent-repl-page-unload-wanted-p'); the switch to it loads the
;; page at once, with no wait, and the switch away from a page that is not
;; wanted unloads it at once, whether or not it had drawn: a user cycling
;; through tabs never waits on a page loading only to be dropped.
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
(declare-function agent-repl-open-tabs-holds-p "open-tabs" (id))
(declare-function agent-repl--webhost-id "webhost" (ws))
(declare-function agent-repl-host-report-page-unloading "host" (ws on-reported on-not-reported))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl-roster-status-for-ws "roster" (ws))
(declare-function agent-repl-host-report-page-residency "host" (ws residency on-reported))
(declare-function agent-repl-roster-last-turn-ended-ms "roster" (ws))
(defvar agent-repl-link-up-functions)
(defvar agent-repl-roster-update-functions)

(defgroup agent-repl-page-unload nil
  "Unloading the workspace pages nobody looks at."
  :group 'agent-repl)

(defcustom agent-repl-page-unload-after 3600
  "Seconds an IDLE workspace's page is held after its last prompt.
An hour (owner ruling, 2026-10-29)."
  :type 'integer
  :group 'agent-repl-page-unload)

(defcustom agent-repl-page-unload-interval 60
  "Seconds between the passes that unload the pages no status holds."
  :type 'integer
  :group 'agent-repl-page-unload)

(defconst agent-repl--page-unload-central
  '(:agent-repl-central "a page-unload pass spans workspaces")
  "The log scope of a pass's own records.")

(defconst agent-repl-page-unload-buckets
  '((working :submitting :thinking :clearing :compacting :permission :question
             :idle-async :api-retrying)
    (idle :ready :done :interrupted :none)
    (merging :merging :merge-queued)
    (faulted :vendor-blocked :vendor-fault :network-fault :severed :start-failed
             :degraded :dead :turn-died :merge-failed :turn-failed :daemon-impaired)
    (cold-gated :cold-gate)
    (transitional :init :inactive :closing :waiting)
    (merged :merged))
  "Every roster status arm, by how its workspace's page is held.
`working' is always held; `idle' is held for `agent-repl-page-unload-after'
past its last prompt; every other bucket is held only while shown (owner
ruling, 2026-10-29).")

(defvar agent-repl-page-unload--clock #'float-time
  "Answers the current time in seconds.  Tests bind it.")

(defvar agent-repl-page-unload--shown nil
  "The workspace whose page the host shows, or nil.")

(defvar agent-repl-page-unload--timer nil
  "The repeating timer that runs the pass, while one is running.")

(defun agent-repl-page-unload--now ()
  "The current time, in seconds."
  (funcall agent-repl-page-unload--clock))

(defun agent-repl-page-unload-bucket (arm)
  "The bucket of status ARM in `agent-repl-page-unload-buckets', or nil for no ARM.
An ARM no bucket names is a status this table was not taught: it is
recorded at ERROR and signalled, never guessed at."
  (when arm
    (or (car (seq-find (lambda (bucket) (memq arm (cdr bucket)))
                       agent-repl-page-unload-buckets))
        (progn
          (agent-repl--error agent-repl--page-unload-central
                             "elisp.page-unload.unknown-status arm=%S" arm)
          (error "page-unload: status %S is in no bucket" arm)))))

(defun agent-repl-page-unload--ws-bucket (ws)
  "WS's bucket from its roster status, or nil before its first row."
  (agent-repl-page-unload-bucket (agent-repl-roster-status-for-ws ws)))

(defun agent-repl-page-unload--on-screen-p (ws)
  "Non-nil when WS's page is the one shown or WS is the current workspace."
  (or (equal ws agent-repl-page-unload--shown)
      (equal ws (agent-repl--ws-current-name))))

(defun agent-repl-page-unload--withheld-p (ws)
  "Non-nil when the user hid WS, so it holds no page off screen."
  (and (fboundp 'agent-repl-open-tabs-withheld-p) (agent-repl-open-tabs-withheld-p ws)))

(defun agent-repl-page-unload-wanted-p (ws)
  "Non-nil when WS's page should be loaded now, before anyone shows it.
That is when it is on screen, or working and not hidden.  A slot made for
a workspace that is not wanted is made with no page
\(`agent-repl--frontend-ensure-webview-buffer'); its show loads it."
  (or (agent-repl-page-unload--on-screen-p ws)
      (and (not (agent-repl-page-unload--withheld-p ws))
           (eq (agent-repl-page-unload--ws-bucket ws) 'working))))

(defun agent-repl-page-unload-note-shown (ws)
  "Record that the host now shows WS's page.
An unloaded page is put back first (`agent-repl-page-unload--return').  The
page shown until now is judged at once: a page no status holds off screen
is unloaded the moment the user switches away from it."
  (let ((previous agent-repl-page-unload--shown))
    (when (agent-repl-page-unloaded-p ws)
      (agent-repl-page-unload--return ws))
    (unless (equal previous ws)
      (setq agent-repl-page-unload--shown ws)
      (when previous
        (agent-repl-page-unload--judge previous (agent-repl-page-unload--now))))
    (agent-repl-page-unload--report ws)))

(defun agent-repl-page-unload-note-mounted (ws)
  "Record that WS's page was just loaded: the start of its idle hold."
  (agent-repl--ws-put ws :page-loaded-at (agent-repl-page-unload--now)))

(defun agent-repl-page-unloaded-p (ws)
  "Non-nil when WS holds a live slot whose page the host does not hold.
That is what an unload leaves (`agent-repl-page-unload--unload'), and what a
slot made with no page is, read off the slot and the host rather than kept
as a flag beside them."
  (and (buffer-live-p (agent-repl--ws-get ws :frontend-buffer))
       (not (agent-repl-webhost-has-page-p ws))))

(defun agent-repl-page-unload--return (ws)
  "Put WS's unloaded page back in the host, its slot being shown.
The slot and its windows stood throughout; only the page comes back.  A
page put back for a workspace that is not working is a VISIT: it is
unloaded when the user switches away unless the workspace started working
meanwhile (`agent-repl-page-unload--reason')."
  (agent-repl--ws-put ws :page-visit
                      (not (eq (agent-repl-page-unload--ws-bucket ws) 'working)))
  (agent-repl-page-unload-note-mounted ws)
  (agent-repl--info ws "elisp.page-unload.returned ws=%s visit=%s"
                    ws (agent-repl--ws-get ws :page-visit))
  (agent-repl-webhost-ensure-page ws))

(defun agent-repl-page-unload--idle-since (ws)
  "When WS's idle hold began: its last turn's end or its page's load, later."
  (max (/ (or (agent-repl-roster-last-turn-ended-ms ws) 0) 1000.0)
       (or (agent-repl--ws-get ws :page-loaded-at) 0)))

(defun agent-repl-page-unload--reason (ws now)
  "Why WS's held page should be unloaded at NOW, or nil when it is held.
WS is off screen (`agent-repl-page-unload--holdable-p').  A working
workspace clears a standing visit: the user prompted it, or it started
working on its own, and either way it is held as working."
  (let ((bucket (agent-repl-page-unload--ws-bucket ws)))
    (when (eq bucket 'working)
      (agent-repl--ws-put ws :page-visit nil))
    (cond
     ((agent-repl-page-unload--withheld-p ws) 'hidden)
     ((memq bucket '(nil working)) nil)
     ((not (eq bucket 'idle)) bucket)
     ((agent-repl--ws-get ws :page-visit) 'visited)
     ((> (- now (agent-repl-page-unload--idle-since ws)) agent-repl-page-unload-after)
      'idle))))

(defun agent-repl-page-unload--holdable-p (ws)
  "The page of WS, when it is one a pass may unload, else nil.
Neither the page the host shows nor the current workspace\='s is ever
unloaded, nor one already unloaded, nor one whose unload is already being
reported, nor one whose tab's open is in flight: the daemon answers that
open once the page has drawn, so a page dropped under it left the tab
unanswered for good (2026-10-29)."
  (let ((buf (agent-repl--ws-get ws :frontend-buffer)))
    (and (buffer-live-p buf)
         (agent-repl-webhost-has-page-p ws)
         (not (agent-repl-page-unload--on-screen-p ws))
         (not (agent-repl--ws-get ws :page-unloading))
         (not (agent-repl-open-tabs-holds-p (agent-repl--webhost-id ws)))
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
     (agent-repl--ws-put ws :page-visit nil)
     (agent-repl-page-unload--report ws)
     (agent-repl--info ws "elisp.page-unload.unloaded ws=%s reason=%s held-s=%d"
                       ws reason
                       (round (- now (or (agent-repl--ws-get ws :page-loaded-at) now)))))
   (lambda (why)
     (agent-repl--info ws "elisp.page-unload.kept ws=%s reason=%s why=%s"
                       ws reason (pcase why
                                   ('changed "in-use-since-report")
                                   ('not-reported "not-reported")
                                   (_ (error "Unknown kept reason %S" why)))))))

(defun agent-repl-page-unload--judge (ws now)
  "Unload WS\='s page when no status holds it at NOW; answer non-nil if so."
  (when (agent-repl-page-unload--holdable-p ws)
    (when-let ((reason (agent-repl-page-unload--reason ws now)))
      (agent-repl-page-unload--unload ws reason now)
      t)))

(defun agent-repl-page-unload--load-working (ws)
  "Load WS\='s unloaded page in the background when WS is working; non-nil if so.
A workspace that starts working while its page is unloaded gets it back
before anyone switches to it (owner ruling, 2026-10-29)."
  (when (and (agent-repl-page-unloaded-p ws)
             (not (agent-repl-page-unload--withheld-p ws))
             (eq (agent-repl-page-unload--ws-bucket ws) 'working))
    (agent-repl--ws-put ws :page-visit nil)
    (agent-repl-page-unload-note-mounted ws)
    (agent-repl--info ws "elisp.page-unload.loaded ws=%s reason=working" ws)
    (agent-repl-webhost-ensure-page ws)
    t))

(defun agent-repl-page-unload-residency (ws)
  "What Emacs holds of WS\='s page now, as a `PageResidency' plist.
Not loaded, or loaded with the rule that lets it go: held while working
(or with no status yet), unloaded at the idle hold\='s end for an idle
page that is not a visit, and unloaded on the switch away for everything
else (`agent-repl-page-unload--reason').  The daemon serves it on the
rail\='s workspace memory hover (endpoint_report_page_residency.proto)."
  (if (not (agent-repl-webhost-has-page-p ws))
      (list :arm :not-loaded :value nil)
    (let ((bucket (agent-repl-page-unload--ws-bucket ws)))
      (list :arm :loaded
            :value (list :release
                         (cond
                          ((and (memq bucket '(nil working))
                                (not (agent-repl-page-unload--withheld-p ws)))
                           (list :arm :held :value nil))
                          ((and (eq bucket 'idle)
                                (not (agent-repl-page-unload--withheld-p ws))
                                (not (agent-repl--ws-get ws :page-visit)))
                           (list :arm :at
                                 :value (list :unix-ms
                                              (round (* 1000 (+ (agent-repl-page-unload--idle-since ws)
                                                                agent-repl-page-unload-after))))))
                          (t (list :arm :on-leave :value nil))))))))

(defun agent-repl-page-unload--report (ws)
  "Report WS\='s residency to the daemon when it differs from the last report.
A workspace with no slot has nothing to report.  The report is marked sent
only once the daemon has taken it, so one that does not land is sent again
on the next change or pass."
  (when (buffer-live-p (agent-repl--ws-get ws :frontend-buffer))
    (let ((residency (agent-repl-page-unload-residency ws)))
      (unless (equal residency (agent-repl--ws-get ws :page-residency-reported))
        (agent-repl-host-report-page-residency
         ws residency
         (lambda () (agent-repl--ws-put ws :page-residency-reported residency)))))))

(defun agent-repl-page-unload-report-all ()
  "Report every live workspace\='s residency that changed since its last report."
  (dolist (ws (agent-repl--live-ws-names))
    (agent-repl-page-unload--report ws)))

(defun agent-repl-page-unload-pass ()
  "Load every working workspace\='s page, and unload every page no status holds.
Each unload waits on the daemon being told (`agent-repl-page-unload--unload').
Answers how many unloads were started."
  (let ((now (agent-repl-page-unload--now))
        (started 0)
        (loaded 0))
    (dolist (ws (agent-repl--live-ws-names))
      (when (agent-repl-page-unload--load-working ws)
        (setq loaded (1+ loaded)))
      (when (agent-repl-page-unload--judge ws now)
        (setq started (1+ started))))
    (when (> (+ started loaded) 0)
      (agent-repl--info agent-repl--page-unload-central
                        "elisp.page-unload.pass started=%d loaded=%d" started loaded))
    (agent-repl-page-unload-report-all)
    started))

(defun agent-repl-page-unload-start (&optional _conn)
  "Run the pass every `agent-repl-page-unload-interval' seconds.
Registered on `agent-repl-link-up-functions'; a timer already running is kept.
A link-up may be to a daemon that holds no residency (a successor, a
restart), so every workspace\='s last report is forgotten and the next pass
reports them all again."
  (dolist (ws (agent-repl--live-ws-names))
    (agent-repl--ws-put ws :page-residency-reported nil))
  (unless (timerp agent-repl-page-unload--timer)
    (setq agent-repl-page-unload--timer
          (run-with-timer agent-repl-page-unload-interval agent-repl-page-unload-interval
                          #'agent-repl-page-unload-pass))
    (agent-repl--log agent-repl--page-unload-central
                     "elisp.page-unload.started interval-s=%d after-s=%d"
                     agent-repl-page-unload-interval agent-repl-page-unload-after)))

(defun agent-repl-page-unload-on-roster-update (&optional _roster)
  "Apply the statuses a roster push changed, without waiting for the next pass."
  (agent-repl-page-unload-pass))

(add-hook 'agent-repl-link-up-functions #'agent-repl-page-unload-start)
(add-hook 'agent-repl-roster-update-functions #'agent-repl-page-unload-on-roster-update)

(defun agent-repl-page-unload-report-pageless ()
  "Tell the daemon of every open workspace whose page the host does not hold.
A page dropped before every drop was announced
\(`agent-repl-page-drop-announced') left its workspace drawn severed,
blue, until a page connected again.
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
