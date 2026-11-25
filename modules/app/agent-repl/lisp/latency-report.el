;;; latency-report.el --- the editor reports its measurements to the daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; THE ONE PATH EMACS'S MEASUREMENTS TAKE TO THE DAEMON'S METRICS
;; (`ReportLatencies', endpoint_report_latencies.proto).  Two kinds travel
;; it, each queued here by its owner:
;;
;;   startup milestones  startup.el, once the editor's startup has ended
;;   editor latencies    stall.el's command-loop stalls with their
;;                       culprits (stall-culprit.el), elisp-build.el's
;;                       pushed elisp reloads
;;
;; A MEASUREMENT IS KEPT UNTIL A REPORT OF IT IS ACKNOWLEDGED.  It stays in
;; its queue while a report carrying it is out, and leaves only on the
;; daemon's answer, so a report that did not arrive (a daemon handover, no
;; link) loses nothing: the next link edge sends it again.  Every answer ends
;; the batch: a success recorded it, and a refusal is a defect the same batch
;; would only repeat, so it is recorded at ERROR and dropped.
;;
;; ONE REPORT AT A TIME.  Startup milestones are sent at once; an editor
;; latency is sent a short while after it is recorded
;; (`agent-repl-latency-report-delay'), so a burst of stalls is one call.
;; Nothing is retried on a timer of its own: a daemon that is down costs no
;; calls until the next link-up or the next measurement.
;;
;; THE EDITOR-LATENCY QUEUE IS BOUNDED (`agent-repl-latency-report-max-held'),
;; because a daemon down for a long while would otherwise grow it without
;; end; the oldest are dropped, at WARN, and counted.  A hot reload of this
;; file keeps every queue: they are `defvar's.

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(require 'cl-lib)

(declare-function agent-repl-link-newest "daemon-link" ())
(declare-function agent-repl-rpc-report-latencies "rpc" (conn request &rest keys))

(defconst agent-repl-latency-report--central
  '(:agent-repl-central "the editor's measurements span every workspace")
  "The log scope of every report record.")

(defconst agent-repl-latency-report-delay 2.0
  "Seconds after an editor latency is recorded that the batch is sent.")

(defconst agent-repl-latency-report-max-held 512
  "The most editor latencies held unsent; the oldest beyond it are dropped.")

(defvar agent-repl-latency-report--startup nil
  "Queued startup milestones, oldest first.
Each is a plist (:milestone KEYWORD :daemon KEYWORD :seconds NUMBER) for
`agent-repl-wire-encode-reported-editor-startup'.")

(defvar agent-repl-latency-report--editor nil
  "Queued editor latencies and stalls, oldest first.
Each is a plist (:kind KEYWORD :seconds NUMBER) for
`agent-repl-wire-encode-reported-editor-latency', or a stall (:kind
:attributed-stall :seconds NUMBER :culprit PLIST) for
`agent-repl-wire-encode-reported-editor-stall'.")

(defvar agent-repl-latency-report--in-flight nil
  "Non-nil while a report is unanswered.")

(defvar agent-repl-latency-report--timer nil
  "The scheduled send of the editor latencies, or nil.")

(defvar agent-repl-latency-report--dropped 0
  "Editor latencies dropped to the bound since the last acknowledged report.")

(defun agent-repl-latency-report-startup (milestones)
  "Queue the startup MILESTONES, a list of plists, and send them now."
  (setq agent-repl-latency-report--startup
        (append agent-repl-latency-report--startup milestones))
  (agent-repl-latency-report-flush))

(defun agent-repl-latency-report-editor (kind seconds)
  "Queue one editor latency of KIND that took SECONDS, and schedule its send."
  (agent-repl-latency-report--queue-editor (list :kind kind :seconds seconds)))

(defun agent-repl-latency-report-stall (seconds culprit)
  "Queue one command-loop stall of SECONDS held by CULPRIT, and schedule its send.
CULPRIT is the plist `agent-repl-stall-culprit-take' answers under
`:culprit'.  It is sent as a ReportedEditorStall."
  (agent-repl-latency-report--queue-editor
   (list :kind :attributed-stall :seconds seconds :culprit culprit)))

(defun agent-repl-latency-report--queue-editor (entry)
  "Queue the editor measurement ENTRY, bounded, and schedule its send."
  (setq agent-repl-latency-report--editor
        (append agent-repl-latency-report--editor (list entry)))
  (agent-repl-latency-report--trim)
  (unless agent-repl-latency-report--timer
    (setq agent-repl-latency-report--timer
          (run-at-time agent-repl-latency-report-delay nil
                       #'agent-repl-latency-report--scheduled))))

(defun agent-repl-latency-report--trim ()
  "Drop the oldest editor latencies past the bound, at WARN."
  (let ((excess (- (length agent-repl-latency-report--editor)
                   agent-repl-latency-report-max-held)))
    (when (> excess 0)
      (setq agent-repl-latency-report--editor
            (nthcdr excess agent-repl-latency-report--editor)
            agent-repl-latency-report--dropped
            (+ agent-repl-latency-report--dropped excess))
      (agent-repl--warn agent-repl-latency-report--central
                        "elisp.latency-report.overflow dropped={dropped} held={held}"
                        :dropped agent-repl-latency-report--dropped
                        :held (length agent-repl-latency-report--editor)))))

(defun agent-repl-latency-report--scheduled ()
  "The scheduled send came due."
  (setq agent-repl-latency-report--timer nil)
  (agent-repl-latency-report-flush))

(defun agent-repl-latency-report-flush (&rest _connections)
  "Send every queued measurement, when nothing is in flight and a daemon takes it.
The report is daemon-wide, so it goes to the daemon that serves next
\(`agent-repl-link-newest'): never to one a handover is leaving.  Also the
handler of every link edge that answers a new daemon (up, a successor's
acceptance, a promotion), so what waited is sent there."
  (let ((conn (and (not agent-repl-latency-report--in-flight)
                   (or agent-repl-latency-report--startup
                       agent-repl-latency-report--editor)
                   (agent-repl-link-newest))))
    (when conn
      (let ((startup agent-repl-latency-report--startup)
            (editor agent-repl-latency-report--editor))
        (setq agent-repl-latency-report--in-flight t)
        (agent-repl--log agent-repl-latency-report--central
                         "elisp.latency-report.send milestones={milestones} latencies={latencies}"
                         :milestones (length startup) :latencies (length editor))
        (agent-repl-rpc-report-latencies
         conn
         (list :editor-startup startup :editor-latencies editor)
         :on-response (lambda (response)
                        (agent-repl-latency-report--answered startup editor response))
         :on-failure (lambda (detail)
                       (agent-repl-latency-report--failed startup editor detail)))))))

(defun agent-repl-latency-report--answered (startup editor response)
  "The daemon answered the report of STARTUP and EDITOR with RESPONSE.
Every answer ends the batch; what was queued meanwhile is sent next."
  (setq agent-repl-latency-report--in-flight nil
        agent-repl-latency-report--startup
        (cl-set-difference agent-repl-latency-report--startup startup :test #'eq)
        agent-repl-latency-report--editor
        (cl-set-difference agent-repl-latency-report--editor editor :test #'eq))
  (pcase (plist-get response :arm)
    (:success
     (agent-repl--info agent-repl-latency-report--central
                       "elisp.latency-report.ok milestones={milestones} latencies={latencies} outcome={outcome} dropped={dropped}"
                       :milestones (length startup) :latencies (length editor)
                       :outcome (plist-get (plist-get response :value) :arm)
                       :dropped agent-repl-latency-report--dropped)
     (setq agent-repl-latency-report--dropped 0))
    (:error
     (agent-repl--error agent-repl-latency-report--central
                        "elisp.latency-report.refused milestones={milestones} latencies={latencies} cause={cause}"
                        :milestones (length startup) :latencies (length editor) :cause (prin1-to-string (plist-get response :value))))
    (arm
     (agent-repl--error agent-repl-latency-report--central
                        "elisp.latency-report.unknown-arm milestones={milestones} latencies={latencies} arm={arm}"
                        :milestones (length startup) :latencies (length editor) :arm (prin1-to-string arm))))
  (agent-repl-latency-report-flush))

(defun agent-repl-latency-report--failed (startup editor detail)
  "The report of STARTUP and EDITOR did not reach the daemon (DETAIL).
Both stay queued for the next link-up or measurement."
  (setq agent-repl-latency-report--in-flight nil)
  (agent-repl--info agent-repl-latency-report--central
                    "elisp.latency-report.deferred milestones={milestones} latencies={latencies} detail={detail}"
                    :milestones (length startup) :latencies (length editor) :detail (prin1-to-string detail)))

(add-hook 'agent-repl-link-up-functions #'agent-repl-latency-report-flush)
(add-hook 'agent-repl-link-handover-functions #'agent-repl-latency-report-flush)
(add-hook 'agent-repl-link-promote-functions #'agent-repl-latency-report-flush)

(provide 'latency-report)

;;; latency-report.el ends here
