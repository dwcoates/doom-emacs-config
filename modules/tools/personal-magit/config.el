;;; tools/personal-magit/config.el -*- lexical-binding: t; -*-

(set-popup-rule! "magit-revision.*" :side 'right :width 100 :select nil)

(set-popup-rule! "\*helpful .*:.*\*" :side 'right :width 81)

(defun copy-buffer-name ()
  (interactive)
  (let ((buf-name (buffer-name)))
    (message "Buffer name copied to kill ring: '%s'" buf-name)
    (kill-new (buffer-name))))

