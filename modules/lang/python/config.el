;;; lang/python/config.el -*- lexical-binding: t; -*-

(after! python
  (set-pretty-symbols! 'python-mode
    :lambda "lambda")

  (defun dwc-python-mode-hook ()
    (highlight-indentation-mode))

  (add-hook 'python-mode-hook 'dwc-python-mode-hook))
