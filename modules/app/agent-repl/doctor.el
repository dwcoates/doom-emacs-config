;;; app/agent-repl/doctor.el -*- lexical-binding: t; -*-

;; Loaded by `doom doctor' to surface configuration and daemon problems.
;;
;; TWO KINDS OF CHECK.
;;
;; The PROVIDER checks come from the loaded sources: each provider answers a
;; list of (LEVEL . MESSAGE) pairs and this file translates them into
;; `warn!' / `error!'.  A provider that is not bound is SKIPPED rather than
;; treated as a failure -- `doom doctor' loads this file independently of the
;; full module, so a provider's absence usually means its source did not
;; load, which is not itself the finding to report.
;;
;; The DAEMON PROBE asks the running daemon for its own verdict, over a
;; FRESH connection built for that one question.  Three outcomes, and the
;; distinction between the last two is the whole point:
;;
;;   no address        `daemon.addr' is absent -- no daemon has been started.
;;   no answer         the address is there but the transport failed.  That
;;                     is a STALE FILE, reported as "no daemon answering",
;;                     never as an unhealthy daemon: a daemon that is not
;;                     there cannot be unhealthy, and conflating the two
;;                     sends the reader hunting for faults in a process that
;;                     does not exist.
;;   an answer         healthy or unhealthy, both are ANSWERS.  Unhealthy
;;                     arrives inside success carrying its typed faults, and
;;                     each fault's detail is printed verbatim.

;; The doctor runs inside Doom, where `warn!' (doom-lib) exists.
(declare-function warn! "doom-lib")
(declare-function error! "doom-lib")
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl-connect-read-daemon-addr "agent-repl-connect" ())
(declare-function agent-repl-connect-open "agent-repl-connect" (address))
(declare-function agent-repl-connect-close "agent-repl-connect" (conn))
(declare-function agent-repl-rpc-daemon-health-sync "agent-repl-rpc"
                  (conn request &optional timeout))

(defvar agent-repl-doctor-daemon-probe-timeout-seconds 3.0
  "Seconds the doctor waits for the daemon's health answer.
Short on purpose: the doctor is a diagnostic, and a daemon that cannot
answer this quickly is a finding rather than something to wait on.")

(defun agent-repl--doctor-log (fmt &rest args)
  "Record a doctor lifecycle event described by FMT and ARGS.
`doom doctor' loads this file independently of the full agent-repl module,
so the core logger is not guaranteed to be initialized yet.  When it is
available, route every event through it with nil WS: doctor is host-wide,
not owned by a workspace."
  (when (fboundp 'agent-repl--log)
    (apply #'agent-repl--log nil fmt args)))

;; doctor.el stays at the module root (that is where `doom doctor' looks for
;; it), but the sources it loads live in the `lisp/' subdirectory.  The
;; daemon probe needs the transport, the codec and the rpc layer beneath it.
(let ((dir (expand-file-name "lisp/" (file-name-directory load-file-name)))
      (sources '("core.el" "wire-common.el" "wire-verbs.el"
                 "connect.el" "rpc.el" "daemon.el")))
  (agent-repl--doctor-log
   "doctor: start dir=%s sources=%S logger-available=%s"
   dir sources (fboundp 'agent-repl--log))
  (dolist (file sources)
    (let ((path (expand-file-name file dir)))
      (if (file-exists-p path)
          (condition-case err
              (progn
                (load path nil t)
                (agent-repl--doctor-log
                 "doctor: source=%s path=%s outcome=loaded" file path))
            (error
             (agent-repl--doctor-log
              "doctor: source=%s path=%s outcome=failed error=%S" file path err)
             (signal (car err) (cdr err))))
        (agent-repl--doctor-log
         "doctor: source=%s path=%s outcome=skipped reason=missing" file path)))))

;;;; ---- The daemon probe ----

(defun agent-repl--doctor-daemon-issues ()
  "Return the daemon's health as a list of (LEVEL . MESSAGE) doctor issues.
Nil when a daemon answered healthy: the doctor reports findings, and a
healthy daemon is not one."
  (if (not (fboundp 'agent-repl-rpc-daemon-health-sync))
      (progn
        (agent-repl--doctor-log "doctor: daemon-probe skipped reason=rpc-unavailable")
        nil)
    (let ((address (condition-case err
                       (agent-repl-connect-read-daemon-addr)
                     (error
                      (agent-repl--doctor-log
                       "doctor: daemon-probe addr-unreadable error=%S" err)
                      :unreadable))))
      (cond
       ((eq address :unreadable)
        (list (cons 'warn "agent-repl: daemon.addr exists but is malformed")))
       ((null address)
        (agent-repl--doctor-log "doctor: daemon-probe no-address")
        (list (cons 'warn "agent-repl: no daemon.addr — no daemon has been started")))
       (t
        (let ((conn nil))
          (condition-case err
              (progn
                (setq conn (agent-repl-connect-open address))
                (let* ((response (agent-repl-rpc-daemon-health-sync
                                  conn nil
                                  agent-repl-doctor-daemon-probe-timeout-seconds))
                       (verdict (plist-get response :value)))
                  (agent-repl-connect-close conn)
                  (setq conn nil)
                  (pcase (plist-get response :arm)
                    (:error
                     ;; The daemon REFUSED the question, which still proves
                     ;; something is serving on that address.
                     (list (cons 'warn (format "agent-repl: daemon at %s refused the health question"
                                               address))))
                    (:success
                     (pcase (plist-get verdict :arm)
                       (:healthy
                        (agent-repl--doctor-log "doctor: daemon-probe healthy address=%s" address)
                        nil)
                       (:unhealthy
                        (let ((faults (plist-get (plist-get verdict :value) :faults)))
                          (cons (cons 'warn
                                      (format "agent-repl: daemon at %s is UNHEALTHY (%d fault(s))"
                                              address (length faults)))
                                (mapcar (lambda (fault)
                                          (cons 'warn (format "agent-repl:   - %s"
                                                              (plist-get fault :detail))))
                                        faults))))
                       (arm
                        (list (cons 'warn (format "agent-repl: daemon at %s answered an unknown health arm (%S)"
                                                  address arm))))))
                    (arm
                     (list (cons 'warn (format "agent-repl: daemon at %s answered an unknown response arm (%S)"
                                               address arm)))))))
            (error
             ;; A TRANSPORT failure, which is evidence about the FILE and not
             ;; about a daemon's health: nobody is listening on the recorded
             ;; address.
             (when conn (ignore-errors (agent-repl-connect-close conn)))
             (agent-repl--doctor-log
              "doctor: daemon-probe no-answer address=%s error=%S" address err)
             (list (cons 'warn (format "agent-repl: no daemon answering at %s (stale daemon.addr)"
                                       address)))))))))))

;;;; ---- Aggregation ----

(let ((issues
       (apply #'append
              (mapcar
               (lambda (provider)
                 (if (fboundp provider)
                     (condition-case err
                         (let ((result (funcall provider)))
                           (agent-repl--doctor-log
                            "doctor: provider=%s outcome=called issue-count=%d issues=%S"
                            provider (length result) result)
                           result)
                       (error
                        (agent-repl--doctor-log
                         "doctor: provider=%s outcome=failed error=%S" provider err)
                        (signal (car err) (cdr err))))
                   (agent-repl--doctor-log
                    "doctor: provider=%s outcome=skipped reason=unbound" provider)
                   nil))
               '(agent-repl--widget-doctor-issues
                 agent-repl--doctor-daemon-issues)))))
  (agent-repl--doctor-log
   "doctor: aggregation issue-count=%d issues=%S" (length issues) issues)
  (dolist (issue issues)
    (pcase (car issue)
      ('error (if (fboundp 'error!)
                  (progn
                    (agent-repl--doctor-log
                     "doctor: issue-level=error dispatch=error! message=%S" (cdr issue))
                    (error! "%s" (cdr issue)))
                (agent-repl--doctor-log
                 "doctor: issue-level=error dispatch=warn! message=%S" (cdr issue))
                (warn! "FATAL: %s" (cdr issue))))
      (_      (agent-repl--doctor-log
               "doctor: issue-level=%S dispatch=warn! message=%S" (car issue) (cdr issue))
              (warn! "%s" (cdr issue))))))
