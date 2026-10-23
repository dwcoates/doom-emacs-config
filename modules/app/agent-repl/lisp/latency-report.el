;;; latency-report.el --- the editor reports its measurements to the daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; THE ONE PATH EMACS'S MEASUREMENTS TAKE TO THE DAEMON'S METRICS
;; (`ReportLatencies', endpoint_report_latencies.proto).  Two kinds travel
;; it, each queued here by its owner:
;;
;;   startup milestones  startup.el, once the editor's startup has ended
;;   editor latencies    stall.el's command-loop stalls, elisp-build.el's
;;                       pushed elisp reloads
;;
;; A MEASUREMENT IS KEPT UNTIL A REPORT OF IT IS ACKNOWLEDGED.  It stays in
;; its queue while a report carrying it is out, and leaves only on the
;; daemon's answer, so a report that did not arrive (a daemon handover, no
;; link) loses nothing: the next link-up sends it again.  Every answer ends
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

(require 'cl-lib)

(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl-link-live "daemon-link" ())
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
  "Queued editor latencies, oldest first.
Each is a plist (:kind KEYWORD :seconds NUMBER) for
`agent-repl-wire-encode-reported-editor-latency'.")

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
  (setq agent-repl-latency-report--editor
        (append agent-repl-latency-report--editor
                (list (list :kind kind :seconds seconds))))
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
                        "elisp.latency-report.overflow dropped=%d held=%d"
                        agent-repl-latency-report--dropped
                        (length agent-repl-latency-report--editor)))))

(defun agent-repl-latency-report--scheduled ()
  "The scheduled send came due."
  (setq agent-repl-latency-report--timer nil)
  (agent-repl-latency-report-flush))

(defun agent-repl-latency-report-flush (&rest _connections)
  "Send every queued measurement, when nothing is in flight and a link stands.
Also the link-up handler, so a link that comes back sends what waited."
  (let ((conn (and (not agent-repl-latency-report--in-flight)
                   (or agent-repl-latency-report--startup
                       agent-repl-latency-report--editor)
                   (agent-repl-link-live))))
    (when conn
      (let ((startup agent-repl-latency-report--startup)
            (editor agent-repl-latency-report--editor))
        (setq agent-repl-latency-report--in-flight t)
        (agent-repl--log agent-repl-latency-report--central
                         "elisp.latency-report.send milestones=%d latencies=%d"
                         (length startup) (length editor))
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
                       "elisp.latency-report.ok milestones=%d latencies=%d outcome=%s dropped=%d"
                       (length startup) (length editor)
                       (plist-get (plist-get response :value) :arm)
                       agent-repl-latency-report--dropped)
     (setq agent-repl-latency-report--dropped 0))
    (:error
     (agent-repl--error agent-repl-latency-report--central
                        "elisp.latency-report.refused milestones=%d latencies=%d cause=%S"
                        (length startup) (length editor) (plist-get response :value)))
    (arm
     (agent-repl--error agent-repl-latency-report--central
                        "elisp.latency-report.unknown-arm milestones=%d latencies=%d arm=%S"
                        (length startup) (length editor) arm)))
  (agent-repl-latency-report-flush))

(defun agent-repl-latency-report--failed (startup editor detail)
  "The report of STARTUP and EDITOR did not reach the daemon (DETAIL).
Both stay queued for the next link-up or measurement."
  (setq agent-repl-latency-report--in-flight nil)
  (agent-repl--info agent-repl-latency-report--central
                    "elisp.latency-report.deferred milestones=%d latencies=%d detail=%S"
                    (length startup) (length editor) detail))

(add-hook 'agent-repl-link-up-functions #'agent-repl-latency-report-flush)

(provide 'latency-report)

;;; latency-report.el ends here
