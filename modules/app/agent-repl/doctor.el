;;; app/agent-repl/doctor.el -*- lexical-binding: t; -*-

;; Loaded by `doom doctor' to surface hook-install problems.  The actual
;; check logic lives in `install.el' so it is testable; here we translate
;; the returned (LEVEL . MESSAGE) list into `warn!' / `error!' calls.
;;
;; Skips everything inside the agent sandbox: hook installation is a
;; host-only concern.

;; The doctor runs inside Doom, where `warn!' (doom-lib) exists.
(declare-function warn! "doom-lib")

(let ((dir (file-name-directory load-file-name)))
  (dolist (file '("install.el" "codex.el"))
    (let ((path (expand-file-name file dir)))
      (when (file-exists-p path)
        (load path nil t)))))

(dolist (issue (append (and (fboundp 'agent-repl--doctor-issues)
                            (agent-repl--doctor-issues))
                       (and (fboundp 'agent-repl--codex-doctor-issues)
                            (agent-repl--codex-doctor-issues))))
  (pcase (car issue)
    ('error (if (fboundp 'error!)
                (error! "%s" (cdr issue))
              (warn! "FATAL: %s" (cdr issue))))
    (_      (warn! "%s" (cdr issue)))))
