;;; app/claude-repl/doctor.el -*- lexical-binding: t; -*-

;; Loaded by `doom doctor' to surface hook-install problems.  The actual
;; check logic lives in `install.el' so it is testable; here we translate
;; the returned (LEVEL . MESSAGE) list into `warn!' / `error!' calls.
;;
;; Skips everything inside the agent sandbox: hook installation is a
;; host-only concern.

(let ((install-el (expand-file-name "install.el"
                                    (file-name-directory load-file-name))))
  (when (file-exists-p install-el)
    (load install-el nil t)))

(dolist (issue (and (fboundp 'claude-repl--doctor-issues)
                    (claude-repl--doctor-issues)))
  (pcase (car issue)
    ('error (if (fboundp 'error!)
                (error! "%s" (cdr issue))
              (warn! "FATAL: %s" (cdr issue))))
    (_      (warn! "%s" (cdr issue)))))
