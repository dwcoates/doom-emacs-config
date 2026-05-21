;;; lang/markdown-rich/config.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Read-only, face-based rich rendering of markdown buffers.
;;
;; - `+markdown-rich-render-buffer' is an interactive one-shot: it
;;   converts the current markdown buffer to HTML via
;;   `+markdown-rich-render-command' and renders it with `shr' into a
;;   read-only companion buffer, then switches the current window to
;;   that buffer.
;;
;; - `+markdown-rich-mode' is a buffer-local minor mode enabled in the
;;   source markdown buffer.  When on, a companion render buffer is
;;   created (and displayed in another window) and re-rendered every
;;   time the source buffer is saved.

;;; Code:

(require 'shr)

(defgroup +markdown-rich nil
  "Read-only rich rendering of markdown via shr."
  :group 'convenience
  :prefix "+markdown-rich-")

(defcustom +markdown-rich-render-command "pandoc -f markdown -t html"
  "Shell command that reads markdown on stdin and writes HTML on stdout."
  :type 'string
  :group '+markdown-rich)

(defcustom +markdown-rich-buffer-name-format "*Markdown Render: %s*"
  "`format' string for the render buffer's name.
The single %s is replaced with the source buffer name."
  :type 'string
  :group '+markdown-rich)

(defvar-local +markdown-rich--render-buffer nil
  "Render buffer linked to this source markdown buffer, if any.")

(defvar-local +markdown-rich--source-buffer nil
  "Source markdown buffer linked to this render buffer, if any.")

(defun +markdown-rich--render-buffer-name (src-buffer)
  "Return the render buffer name for SRC-BUFFER."
  (format +markdown-rich-buffer-name-format (buffer-name src-buffer)))

(defun +markdown-rich--convert-to-html (md)
  "Convert markdown string MD to HTML using `+markdown-rich-render-command'.
Returns the HTML string, or an empty string if MD is empty."
  (if (or (null md) (string-empty-p md))
      ""
    (with-temp-buffer
      (insert md)
      (let ((coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8))
        (shell-command-on-region
         (point-min) (point-max)
         +markdown-rich-render-command
         t t))
      (buffer-string))))

(defun +markdown-rich--render-html-into (html buffer)
  "Render HTML string into BUFFER via shr; leave BUFFER read-only."
  (unless (fboundp 'libxml-parse-html-region)
    (user-error "This Emacs build lacks libxml support; cannot render HTML"))
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((dom (with-temp-buffer
                   (insert (or html ""))
                   (libxml-parse-html-region (point-min) (point-max)))))
        (when dom
          (shr-insert-document dom)))
      (goto-char (point-min)))
    (read-only-mode 1)))

(defun +markdown-rich--ensure-render-buffer (src-buffer)
  "Return (creating if needed) the render buffer for SRC-BUFFER.
The buffer's content is set to the freshly-rendered HTML of
SRC-BUFFER's current markdown content; back-link to SRC-BUFFER is
recorded in the buffer-local `+markdown-rich--source-buffer'."
  (let* ((name (+markdown-rich--render-buffer-name src-buffer))
         (buf  (get-buffer-create name))
         (md   (with-current-buffer src-buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
         (html (+markdown-rich--convert-to-html md)))
    (+markdown-rich--render-html-into html buf)
    (with-current-buffer buf
      (setq +markdown-rich--source-buffer src-buffer))
    buf))

(defun +markdown-rich--rerender (src-buffer)
  "Refresh the render buffer linked to SRC-BUFFER, if any.
No-op if the link is missing or the render buffer was killed."
  (let ((render-buf (buffer-local-value '+markdown-rich--render-buffer src-buffer)))
    (when (buffer-live-p render-buf)
      (let* ((md   (with-current-buffer src-buffer
                     (buffer-substring-no-properties (point-min) (point-max))))
             (html (+markdown-rich--convert-to-html md)))
        (+markdown-rich--render-html-into html render-buf)))))

(defun +markdown-rich--after-save-hook ()
  "After-save hook in source markdown buffer: rerender linked render buffer."
  (+markdown-rich--rerender (current-buffer)))

(defun +markdown-rich--cleanup-on-kill ()
  "Kill the linked render buffer when the source buffer is killed."
  (when (buffer-live-p +markdown-rich--render-buffer)
    (kill-buffer +markdown-rich--render-buffer)))

;;;###autoload
(defun +markdown-rich-render-buffer ()
  "Render the current markdown buffer as rich HTML in a read-only buffer.
Switches the current window to the render buffer.  The source buffer
is not modified.  One-shot: does not auto-refresh on save (use
`+markdown-rich-mode' for live updates)."
  (interactive)
  (let* ((src (current-buffer))
         (buf (+markdown-rich--ensure-render-buffer src)))
    (switch-to-buffer buf)
    buf))

;;;###autoload
(define-minor-mode +markdown-rich-mode
  "Maintain a live, read-only rich-rendered companion buffer for this
markdown buffer.

When enabled, a buffer named per `+markdown-rich-buffer-name-format'
is created (or reused) containing the shr-rendered HTML of the source
buffer's markdown content.  After every save of the source buffer the
render buffer is refreshed.  Killing the source buffer also kills the
render buffer.

When disabled, the after-save hook is removed and the render buffer
is killed."
  :init-value nil
  :lighter " MdRich"
  (if +markdown-rich-mode
      (progn
        (setq +markdown-rich--render-buffer
              (+markdown-rich--ensure-render-buffer (current-buffer)))
        (display-buffer +markdown-rich--render-buffer)
        (add-hook 'after-save-hook #'+markdown-rich--after-save-hook nil t)
        (add-hook 'kill-buffer-hook #'+markdown-rich--cleanup-on-kill nil t))
    (remove-hook 'after-save-hook #'+markdown-rich--after-save-hook t)
    (remove-hook 'kill-buffer-hook #'+markdown-rich--cleanup-on-kill t)
    (when (buffer-live-p +markdown-rich--render-buffer)
      (kill-buffer +markdown-rich--render-buffer))
    (setq +markdown-rich--render-buffer nil)))

(provide 'markdown-rich)
;;; config.el ends here
