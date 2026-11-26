;;; page-report.el --- the editor tells the daemon which pages its web view holds -*- lexical-binding: t; -*-
;;; Commentary:
;; WHY A PAGE IS LOADED IS THIS EDITOR'S FACT (endpoint_report_page_state.proto).
;; Every load goes through `agent-repl-page-unload-load', and every unload
;; through `agent-repl-webhost-remove'; each tells the daemon here, with its
;; reason, so the daemon's page metrics (agentrepl_pages_loaded and its
;; counters) and the sidebar's loaded-page count know why each page was
;; loaded, not only what visibility makes eligible.  Which pages are held
;; is the web host's own report (its census's frames): this editor's table
;; is what it asked the host to hold, and the daemon records any
;; disagreement between the two that stands.
;;
;; THE DAEMON HOLDS THE STATE ONLY WHILE THE WORKSPACE'S HOST STREAM STANDS, so
;; a restarted Emacs or a daemon that took a workspace over never counts a
;; page nobody holds.  This editor therefore RESTATES what it holds each time a
;; workspace's host stream is subscribed (`agent-repl-page-report-restate'):
;; a restatement is never counted.
;;
;; THE REASON LIVES WITH THE PAGE.  The web host keeps each held page's load
;; reason in the page's own entry (`agent-repl-webhost-ensure-page'), so a
;; restatement on any host stream, the successor's after a handover
;; included, reads the reason the page was loaded for, and no page is held
;; without one.
;;
;; FIRE AND FORGET: a report that does not land is recorded by
;; `agent-repl-host--send-subject-verb' and changes nothing here; the next
;; report or restatement corrects the daemon's view.
;;
;; A WORKSPACE'S REPORTS REACH THE DAEMON IN THE ORDER THEY WERE MADE.  Each
;; is a separate unary call, and two in flight at once can land in either
;; order (2026-11-25 12:33, a load sent before a release landed after the
;; release's unload, and the daemon counted a page no web view held).  So a
;; workspace has at most one report in flight, and the next is sent only
;; once the daemon has answered it or the call has failed
;; (`agent-repl-page-report--lines').

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(declare-function agent-repl-host--send-subject-verb "host" (ws op rpc on-success on-not-sent &optional extra))
(declare-function agent-repl-host-ref "host" (ws))
(declare-function agent-repl-serial-queue-send "core" (lines key send))
(declare-function agent-repl-rpc-report-page-state "rpc" (conn request &rest keys))
(declare-function agent-repl-webhost-has-page-p "webhost" (ws))
(declare-function agent-repl-webhost-page-reason "webhost" (ws))

(defconst agent-repl-page-report-load-reasons
  '(shown allocated slot load-requested ungated)
  "Every reason a page is loaded, as `agent-repl-page-unload-load' names it.")

(defconst agent-repl-page-report-held-reasons
  (append agent-repl-page-report-load-reasons '(held-before-reasons))
  "Every reason a held page may carry: a load reason, or `held-before-reasons',
a page a running Emacs held before it kept a reason with each page
\(`agent-repl--webhost-migrate-page-entries') or held under a reason this
build no longer names (`agent-repl-webhost-page-reason').  It is never a
load's.")

(defconst agent-repl-page-report-unload-reasons
  '(deallocated detached released)
  "Every reason a page is dropped.")

(defun agent-repl-page-report--keyword (why)
  "WHY, a reason symbol, as the wire's keyword."
  (intern (concat ":" (symbol-name why))))

(defvar agent-repl-page-report--lines (make-hash-table :test 'equal)
  "Each workspace's reports not yet answered, by its daemon id, the one in
flight first (`agent-repl-serial-queue-send').")

(defun agent-repl-page-report--send (ws arm reason)
  "Send WS's page report ARM with REASON (a symbol, or nil for `:not-held'),
once every earlier report of WS's daemon id has been answered.
A workspace with no daemon ref yet is sent at once, which skips it
\(`agent-repl-host--send-subject-verb')."
  (let ((report (list :arm arm :value (and reason (agent-repl-page-report--keyword reason))))
        (id (plist-get (agent-repl-host-ref ws) :id)))
    (if (null id)
        (agent-repl-page-report--call ws report #'ignore)
      (agent-repl-serial-queue-send
       agent-repl-page-report--lines id
       (lambda (done) (agent-repl-page-report--call ws report done))))))

(defun agent-repl-page-report--call (ws report done)
  "Send WS's REPORT, calling DONE once the daemon answered or the call failed."
  (agent-repl-host--send-subject-verb
   ws "page-state" #'agent-repl-rpc-report-page-state
   done done
   (list :report report)))

(defun agent-repl-page-report-check-load-reason (ws why)
  "Signal unless WHY is a reason WS's page is loaded for.
A reason no load path names is a defect: recorded at ERROR and signalled,
before the page is held or anything is sent."
  (unless (memq why agent-repl-page-report-load-reasons)
    (agent-repl--error ws "elisp.page-report.unknown-reason ws={ws} why={why}" :ws ws :why why)
    (error "page-report: %S is no reason a page is loaded" why)))

(defun agent-repl-page-report-loaded (ws why)
  "Tell the daemon WS's page was just loaded, for WHY."
  (agent-repl-page-report-check-load-reason ws why)
  (agent-repl-page-report--send ws :loaded why))

(defun agent-repl-page-report-unloaded (ws why)
  "Tell the daemon WS's page was just dropped, for WHY."
  (unless (memq why agent-repl-page-report-unload-reasons)
    (agent-repl--error ws "elisp.page-report.unknown-unload-reason ws={ws} why={why}" :ws ws :why why)
    (error "page-report: %S is no reason a page is dropped" why))
  (agent-repl-page-report--send ws :unloaded why))

(defun agent-repl-page-report-restate (ws)
  "Restate to the daemon whether the web view holds WS's page, and why.
Called on every subscribe of WS's host stream.  The reason is the one
the page holds (`agent-repl-webhost-page-reason').  A page held with no
reason breaks the one-load-path invariant: it is recorded at ERROR and
nothing is restated, so the daemon counts no page it cannot explain."
  (let ((held (agent-repl-webhost-has-page-p ws))
        (why (agent-repl-webhost-page-reason ws)))
    (cond
     ((and held why) (agent-repl-page-report--send ws :held why))
     (held
      (agent-repl--error ws "elisp.page-report.held-without-reason ws={ws}" :ws ws))
     (t
      (agent-repl-page-report--send ws :not-held nil)))))

(provide 'page-report)
;;; page-report.el ends here
