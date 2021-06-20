;;; lang/personal-cc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +projectile-test-project (ARG)
  (interactive "P")
  (let* ((rules (assoc "\\*compilation.*\\*" +popup--display-buffer-alist))
         (side-rule (and rules (assoc 'side rules))))
    (when side-rule
      (let (side-rule-value (cdr side-rule))
        (setf (cdr side-rule) 'right)
        (projectile-test-project ARG)
        (setf (cdr side-rule) side-rule-value)))))

;;;###autoload
(defun +lookup-current-c-function-references()
  (interactive)
  (save-excursion
    (c-beginning-of-defun)
    (search-forward "(")
    (backward-char 1)
    (call-interactively #'+lookup/references)))
