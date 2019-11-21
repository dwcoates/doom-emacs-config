;;; lang/python/config.el -*- lexical-binding: t; -*-

(after! python
  (set-pretty-symbols! 'python-mode
    :lambda "lambda")

  (defun dwc-python-mode-hook ()
    (highlight-indentation-mode))

  (set-popup-rule! "*Python*" :quit nil :select t :width 80 :side 'right)

  (add-hook 'python-mode-hook 'dwc-python-mode-hook))
