;;; app/agent-repl/doctor.el -*- lexical-binding: t; -*-

;; Loaded by `doom doctor' to surface configuration and daemon problems.
;;
;; THREE KINDS OF CHECK.
;;
;; The PROVIDER checks come from the loaded sources: each provider answers a
;; list of (LEVEL . MESSAGE) pairs and this file translates them into
;; `warn!' / `error!'.  A provider that is not bound is SKIPPED rather than
;; treated as a failure -- `doom doctor' loads this file independently of the
;; full module, so a provider's absence usually means its source did not
;; load, which is not itself the finding to report.
;;
;; The REQUIREMENTS check asks bin/requirements.sh, the one reader of the
;; requirement manifests, for its report (`check --json'): every unmet
;; required entry is an error and every unmet optional one a warning, each
;; carrying the fix the runner names.  The doctor checks no requirement itself.
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

;;;; ---- The requirements ----

(defconst agent-repl--doctor-requirements-script
  (expand-file-name "bin/requirements.sh" (file-name-directory (or load-file-name buffer-file-name)))
  "The requirements runner the doctor asks, beside this file.")

(defun agent-repl--doctor-run-requirements ()
  "Run the requirements check and answer (EXIT . OUTPUT).
OUTPUT is the runner's standard output and standard error together: the
JSON report, or the reason it could not make one.  THE ONE PROCESS SEAM
the tests stub."
  (with-temp-buffer
    (let ((exit (call-process agent-repl--doctor-requirements-script nil t nil "check" "--json")))
      (cons exit (buffer-string)))))

(defun agent-repl--doctor-requirement-issue (entry)
  "The doctor issue for one unmet requirement ENTRY of the runner's report.
A required entry is an error and an optional one a warning; each names the
fix the runner gave."
  (let ((optional (eq (gethash "optional" entry) t)))
    (cons (if optional 'warn 'error)
          (format "agent-repl: requirement %s is %s%s: %s. %s Fix: %s"
                  (gethash "id" entry) (gethash "status" entry)
                  (if optional " (optional)" "")
                  (gethash "detail" entry) (gethash "why" entry) (gethash "fix" entry)))))

(defun agent-repl--doctor-requirements-issues ()
  "Return every unmet requirement as a (LEVEL . MESSAGE) doctor issue.
The manifests are the one record of what agent-repl needs, and
bin/requirements.sh the one reader of them, so the doctor asks it rather
than checking anything itself.  A runner that refuses the manifests (exit
2) or answers something that is not its report is itself an error."
  (let* ((result (agent-repl--doctor-run-requirements))
         (exit (car result))
         (output (cdr result)))
    (agent-repl--doctor-log "doctor: requirements exit=%S output-bytes=%d" exit (length output))
    (if (not (memq exit '(0 1)))
        (progn
          (agent-repl--doctor-log "doctor: requirements refused exit=%S output=%S" exit output)
          (list (cons 'error (format "agent-repl: bin/requirements.sh check could not run (exit %S): %s"
                                     exit (string-trim output)))))
      (let ((report (condition-case err
                        (json-parse-string output)
                      (json-parse-error
                       (agent-repl--doctor-log "doctor: requirements unparseable error=%S output=%S" err output)
                       :unparseable))))
        (if (eq report :unparseable)
            (list (cons 'error (format "agent-repl: bin/requirements.sh check answered no report (exit %S): %s"
                                       exit (string-trim output))))
          (let ((issues (delq nil (mapcar (lambda (entry)
                                            (unless (equal (gethash "status" entry) "ok")
                                              (agent-repl--doctor-requirement-issue entry)))
                                          report))))
            (agent-repl--doctor-log "doctor: requirements entries=%d unmet=%d" (length report) (length issues))
            issues))))))

;;;; ---- Aggregation ----

(defvar agent-repl-doctor-inhibit-checks nil
  "Non-nil while doctor.el is loaded as a LIBRARY, not run as the doctor.
`doom doctor' loads this file for the side effect of the aggregation below,
which runs every check, the requirements runner's processes included.  A
caller that loads it only to reach its functions (the ERT suite) binds this
around the load so the load runs no check: it took ~1.8s of a test chunk and
probed the real host's tools from inside a unit test.")

(unless agent-repl-doctor-inhibit-checks
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
                 agent-repl--doctor-requirements-issues
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
              (warn! "%s" (cdr issue)))))))
