;;; app/agent-repl/doctor.el -*- lexical-binding: t; -*-

;; Loaded by `doom doctor' to surface hook-install and capability
;; problems.  The actual check logic lives in `install.el', `codex.el',
;; and `daemon.el' so it is testable; here we translate the returned
;; (LEVEL . MESSAGE) list into `warn!' / `error!' calls.

;; The doctor runs inside Doom, where `warn!' (doom-lib) exists.
(declare-function warn! "doom-lib")
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))

(defun agent-repl--doctor-log (fmt &rest args)
  "Record a doctor lifecycle event described by FMT and ARGS.
`doom doctor' loads this file independently of the full agent-repl module,
so the core logger is not guaranteed to be initialized yet.  When it is
available, route every event through it with nil WS: doctor is host-wide,
not owned by a workspace."
  (when (fboundp 'agent-repl--log)
    (apply #'agent-repl--log nil fmt args)))

(let ((dir (file-name-directory load-file-name))
      (sources '("install.el" "codex.el" "daemon.el")))
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
              "doctor: source=%s path=%s outcome=failed error=%S"
              file path err)
             (signal (car err) (cdr err))))
        (agent-repl--doctor-log
         "doctor: source=%s path=%s outcome=skipped reason=missing"
         file path)))))

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
                         "doctor: provider=%s outcome=failed error=%S"
                         provider err)
                        (signal (car err) (cdr err))))
                   (agent-repl--doctor-log
                    "doctor: provider=%s outcome=skipped reason=unbound"
                    provider)
                   nil))
               '(agent-repl--doctor-issues
                 agent-repl--codex-doctor-issues
                 agent-repl--widget-doctor-issues)))))
  (agent-repl--doctor-log
   "doctor: aggregation issue-count=%d issues=%S"
   (length issues) issues)
  (dolist (issue issues)
    (pcase (car issue)
      ('error (if (fboundp 'error!)
                  (progn
                    (agent-repl--doctor-log
                     "doctor: issue-level=error dispatch=error! message=%S"
                     (cdr issue))
                    (error! "%s" (cdr issue)))
                (agent-repl--doctor-log
                 "doctor: issue-level=error dispatch=warn! message=%S"
                 (cdr issue))
                (warn! "FATAL: %s" (cdr issue))))
      (_      (agent-repl--doctor-log
               "doctor: issue-level=%S dispatch=warn! message=%S"
               (car issue) (cdr issue))
              (warn! "%s" (cdr issue))))))
