;;; tools/personal-magit/config.el -*- lexical-binding: t; -*-

(set-popup-rule! "magit-revision.*" :side 'right :width 100 :select nil)

(set-popup-rule! "\*helpful .*:.*\*" :side 'right :width 81)

(defun copy-buffer-name ()
  (interactive)
  (let ((buf-name (buffer-name)))
    (message "Buffer name copied to kill ring: '%s'" buf-name)
    (kill-new (buffer-name))))

(add-hook 'emacs-startup-hook  (lambda () (add-hook 'kill-emacs-hook #'shutdown-computer-on-exit-maybe t)) t)

(defvar shutdown-computer-on-emacs-exit-p nil "Set when shutting down with `shutdown-computer`")
(defun shutdown-computer-on-exit-maybe ()
  (when shutdown-computer-on-emacs-exit-p
    (shell-command "shutdown now")))

(defun shutdown-computer ()
  (interactive)
  (setq shutdown-computer-on-emacs-exit-p t)
  (kill-emacs))
