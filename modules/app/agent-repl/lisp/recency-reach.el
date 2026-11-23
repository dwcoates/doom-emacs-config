;;; recency-reach.el --- the recency walk's reach, for the sidebar's gutters -*- lexical-binding: t; -*-

;;; Commentary:

;; THE SIDEBAR'S RECENCY GUTTERS TELL WHERE `s-[' AND `s-]' LAND.  Each tab
;; the recency walk reaches in few presses wears a bar beside its sidebar
;; row: orange when `s-[' (older) reaches it sooner, purple when `s-]'
;; (newer) does, the full color at one press and more muted with each press
;; after it.  At most the two nearest each way wear one.
;;
;; EMACS OWNS THE WALK'S ORDER (its selection history, filtered by each
;; visit's dwell), so Emacs computes the reach and the daemon only draws it
;; (agentrepl.v1.ReportRecencyReach).  The presses are counted with the
;; walk's own stepping (`agent-repl--ring-walk') over the walk's own ring
;; (`agent-repl--recency-ring') from where the next press starts, so a bar
;; can never disagree with the press it describes.
;;
;; The reach is recomputed after every roster push and every perspective
;; activation, and reported only when it changed.  The daemon keeps it for
;; the life of Emacs's WatchDaemon stream, so every link edge reports it
;; again.

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(require 'cl-lib)
(require 'seq)

(declare-function agent-repl-serial-create "core")
(declare-function agent-repl-serial-send-to-newest "core" (serial report scope label send))
(declare-function agent-repl--ws-current-name "workspace")
(declare-function agent-repl--ws-add-activated-hook "workspace" (fn))
(declare-function agent-repl--drawn-tab-names "commands")
(declare-function agent-repl--recency-ring "commands" (current))
(declare-function agent-repl--ring-walk "commands" (all drawn current step))
(declare-function agent-repl-host-pending-selection "host")
(declare-function agent-repl-host-ref "host" (ws))
(declare-function agent-repl-rpc-report-recency-reach "rpc" (conn request &rest keys))

(defvar agent-repl-roster-update-functions)

(defconst agent-repl-recency-reach-per-direction 2
  "How many tabs each chord's gutters mark: the nearest this many.")

(defconst agent-repl--recency-reach-log-scope
  '(:agent-repl-central "the recency walk's reach is Emacs-wide")
  "The log scope every reach record is written under.")

;;;; ---- The reach ----

(defun agent-repl--recency-reach (all drawn current)
  "Return each tab the walk reaches soonest from CURRENT, as (NAME ARM PRESSES).
ALL is the ring in walk order and DRAWN its drawn subset, as the walk
reads them (`agent-repl--ring-walk').  ARM is `:older' when `s-[' lands on
NAME in fewer presses than `s-]', or as few, and `:newer' otherwise;
PRESSES is that count, from 1.  CURRENT has no reach.  At most the
`agent-repl-recency-reach-per-direction' nearest of each ARM are kept,
nearest first, the older ones before the newer."
  (let* ((older (agent-repl--ring-walk all drawn current 1))
         (newer (agent-repl--ring-walk all drawn current -1))
         (reaches
          (cl-loop for name in older
                   for back from 1
                   for forward = (1+ (cl-position name newer :test #'equal))
                   unless (equal name current)
                   collect (if (<= back forward)
                               (list name :older back)
                             (list name :newer forward)))))
    (cl-flet ((nearest (arm)
                (seq-take (cl-sort (cl-remove-if-not (lambda (r) (eq (nth 1 r) arm)) reaches)
                                   #'< :key #'caddr)
                          agent-repl-recency-reach-per-direction)))
      (append (nearest :older) (nearest :newer)))))

(defun agent-repl--recency-reach-now ()
  "Return the reach of the walk as the next press would start it.
The start is the walk's own (`agent-repl--step-workspace'): the most
recent request while one is unanswered, else the tab shown."
  (let ((current (or (agent-repl-host-pending-selection) (agent-repl--ws-current-name))))
    (and current
         (agent-repl--recency-reach (agent-repl--recency-ring current)
                                    (agent-repl--drawn-tab-names)
                                    current))))

(defun agent-repl--recency-reach-request (reaches)
  "Return the ReportRecencyReachRequest plist for REACHES.
Every reached tab is drawn, and a drawn tab's workspace is registered, so
a name with no ref is a broken invariant: it is recorded and signalled."
  (list :reaches
        (mapcar (pcase-lambda (`(,name ,arm ,presses))
                  (let ((ref (agent-repl-host-ref name)))
                    (unless ref
                      (agent-repl--error agent-repl--recency-reach-log-scope
                                         "elisp.recency-reach.unregistered ws={ws} arm={arm} presses={presses}"
                                         :ws name :arm arm :presses presses)
                      (error "agent-repl: the recency walk reaches %s, which has no daemon ref" name))
                    (list :workspace ref
                          :direction (list :arm arm :value (list :presses presses)))))
              reaches)))

;;;; ---- Reporting it ----

(defvar agent-repl--recency-reach-serial (agent-repl-serial-create)
  "The reach reports' serializer (`agent-repl-serial-send').
One report at a time, the current reach sent once the one in flight is
answered, so the daemon's reach ends on Emacs's.")

(defvar agent-repl--recency-reach-reported 'none
  "The reach the daemon last took, or `none' when it holds none of ours.
A link edge resets it, since the daemon's reach dies with the stream.")

(defvar agent-repl--recency-reach-timer nil
  "The pending coalesced recompute, or nil.
A switch fires the activation and then the roster push, so the reach is
computed once that settles rather than at every step.")

(defun agent-repl--recency-reach-report ()
  "Report the walk's reach to the newest daemon, when it changed.
With no link there is no stream to belong to, and the link edge that
answers the next daemon reports again (`agent-repl--recency-reach-on-link')."
  (agent-repl-serial-send-to-newest
   agent-repl--recency-reach-serial #'agent-repl--recency-reach-report
   agent-repl--recency-reach-log-scope "recency-reach.report"
   #'agent-repl--recency-reach-send))

(defun agent-repl--recency-reach-send (conn done)
  "Send CONN the current reach unless the daemon already holds it; then DONE."
  (let ((reaches (agent-repl--recency-reach-now)))
    (if (equal reaches agent-repl--recency-reach-reported)
        (progn
          (agent-repl--log agent-repl--recency-reach-log-scope
                           "elisp.recency-reach.report unchanged reaches={reaches}" :reaches (length reaches))
          (funcall done))
      (agent-repl--info agent-repl--recency-reach-log-scope
                        "elisp.recency-reach.report send reaches={reaches}" :reaches (prin1-to-string reaches))
      (agent-repl-rpc-report-recency-reach
       conn (agent-repl--recency-reach-request reaches)
       :on-response (lambda (response) (agent-repl--recency-reach-answered reaches response done))
       :on-failure (lambda (detail) (agent-repl--recency-reach-failed reaches detail done))))))

(defun agent-repl--recency-reach-answered (reaches response done)
  "Record the daemon's RESPONSE to a report of REACHES, then end it with DONE."
  (pcase (plist-get response :arm)
    (:success
     (setq agent-repl--recency-reach-reported reaches)
     (agent-repl--log agent-repl--recency-reach-log-scope
                      "elisp.recency-reach.report taken reaches={reaches}" :reaches (length reaches)))
    (:error
     ;; NO EMACS STREAM STANDS on that daemon: the link is between streams,
     ;; and the link edge that answers the stream reports again.
     (agent-repl--info agent-repl--recency-reach-log-scope
                       "elisp.recency-reach.report refused reaches={reaches} cause={cause}"
                       :reaches (length reaches) :cause (prin1-to-string (plist-get (plist-get response :value) :cause))))
    (arm
     (agent-repl--error agent-repl--recency-reach-log-scope
                        "elisp.recency-reach.report unknown-arm reaches={reaches} arm={arm}"
                        :reaches (length reaches) :arm (prin1-to-string arm))))
  (funcall done))

(defun agent-repl--recency-reach-failed (reaches detail done)
  "Record a report of REACHES that failed in transport with DETAIL; then DONE."
  (agent-repl--error agent-repl--recency-reach-log-scope
                     "elisp.recency-reach.report failed reaches={reaches} detail={detail}"
                     :reaches (length reaches) :detail (prin1-to-string detail))
  (funcall done))

(defun agent-repl--recency-reach-changed (&rest _)
  "Report the reach once the calling change has settled.
Registered on every roster push and every perspective activation."
  (unless (timerp agent-repl--recency-reach-timer)
    (setq agent-repl--recency-reach-timer
          (run-at-time 0 nil (lambda ()
                               (setq agent-repl--recency-reach-timer nil)
                               (agent-repl--recency-reach-report))))))

(defun agent-repl--recency-reach-on-link (&rest _connections)
  "Report the reach to a daemon that just became the live one.
The daemon holds the reach of a stream only, so it holds none of a new
stream's: what was taken before is forgotten and reported again."
  (setq agent-repl--recency-reach-reported 'none)
  (agent-repl--recency-reach-report))

(add-hook 'agent-repl-roster-update-functions #'agent-repl--recency-reach-changed)
(agent-repl--ws-add-activated-hook #'agent-repl--recency-reach-changed)
(add-hook 'agent-repl-link-up-functions #'agent-repl--recency-reach-on-link)
(add-hook 'agent-repl-link-handover-functions #'agent-repl--recency-reach-on-link)
(add-hook 'agent-repl-link-promote-functions #'agent-repl--recency-reach-on-link)

(provide 'recency-reach)

;;; recency-reach.el ends here
