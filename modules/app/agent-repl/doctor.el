;;; app/agent-repl/doctor.el -*- lexical-binding: t; -*-

;; Loaded by `doom doctor' to surface hook-install and capability
;; problems.  The actual check logic lives in `install.el', `codex.el',
;; and `daemon.el' so it is testable; here we translate the returned
;; (LEVEL . MESSAGE) list into `warn!' / `error!' calls.
;;
;; Skips everything inside the agent sandbox: hook installation and the
;; daemon's assets are host-only concerns.

;; The doctor runs inside Doom, where `warn!' (doom-lib) exists.
(declare-function warn! "doom-lib")

(let ((dir (file-name-directory load-file-name)))
  (dolist (file '("install.el" "codex.el" "daemon.el"))
    (let ((path (expand-file-name file dir)))
      (when (file-exists-p path)
        (load path nil t)))))

(dolist (issue (append (and (fboundp 'agent-repl--doctor-issues)
                            (agent-repl--doctor-issues))
                       (and (fboundp 'agent-repl--codex-doctor-issues)
                            (agent-repl--codex-doctor-issues))
                       (and (fboundp 'agent-repl--widget-doctor-issues)
                            (agent-repl--widget-doctor-issues))))
  (pcase (car issue)
    ('error (if (fboundp 'error!)
                (error! "%s" (cdr issue))
              (warn! "FATAL: %s" (cdr issue))))
    (_      (warn! "%s" (cdr issue)))))
