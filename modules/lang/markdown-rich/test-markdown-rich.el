;;; test-markdown-rich.el --- ERT tests for markdown-rich -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -batch -Q -l ert -l test-markdown-rich.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "config.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Helpers ----

(defmacro +mdr-test--with-src-buffer (markdown &rest body)
  "Run BODY in a fresh temp buffer containing MARKDOWN as its text."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,markdown)
     (goto-char (point-min))
     ,@body))

(defun +mdr-test--cleanup-render-buffer (src)
  "Kill render buffer linked to SRC, if any."
  (let ((buf (get-buffer (+markdown-rich--render-buffer-name src))))
    (when (buffer-live-p buf) (kill-buffer buf))))

;;;; ---- Buffer name format ----

(ert-deftest +markdown-rich-test-render-buffer-name-default-format ()
  "Render buffer name uses default `*Markdown Render: %s*' format."
  (with-temp-buffer
    (rename-buffer "foo.md" t)
    (should (equal (+markdown-rich--render-buffer-name (current-buffer))
                   "*Markdown Render: foo.md*"))))

(ert-deftest +markdown-rich-test-render-buffer-name-custom-format ()
  "Render buffer name honors `+markdown-rich-buffer-name-format'."
  (let ((+markdown-rich-buffer-name-format "<<%s>>"))
    (with-temp-buffer
      (rename-buffer "bar.md" t)
      (should (equal (+markdown-rich--render-buffer-name (current-buffer))
                     "<<bar.md>>")))))

;;;; ---- HTML conversion ----

(ert-deftest +markdown-rich-test-convert-empty-string-returns-empty ()
  "Empty markdown input returns empty string without invoking the converter."
  (let ((+markdown-rich-render-command "false")) ; would fail if invoked
    (should (equal (+markdown-rich--convert-to-html "") ""))))

(ert-deftest +markdown-rich-test-convert-nil-returns-empty ()
  "Nil markdown input returns empty string."
  (should (equal (+markdown-rich--convert-to-html nil) "")))

(ert-deftest +markdown-rich-test-convert-passes-stdin-through-command ()
  "Conversion pipes MD through `+markdown-rich-render-command' on stdin."
  (let ((+markdown-rich-render-command "cat"))
    (should (equal (+markdown-rich--convert-to-html "hello\n") "hello\n"))))

(ert-deftest +markdown-rich-test-convert-uses-current-command-value ()
  "Conversion uses the *current* value of the command (dynamic, not captured)."
  (let ((+markdown-rich-render-command "tr a-z A-Z"))
    (should (equal (+markdown-rich--convert-to-html "abc") "ABC"))))

;;;; ---- HTML → buffer rendering ----

(ert-deftest +markdown-rich-test-render-into-makes-buffer-read-only ()
  "After rendering, the target buffer is read-only."
  (let ((buf (generate-new-buffer " *mdr-test-render*")))
    (unwind-protect
        (progn
          (+markdown-rich--render-html-into "<p>hi</p>" buf)
          (should (buffer-local-value 'buffer-read-only buf)))
      (kill-buffer buf))))

(ert-deftest +markdown-rich-test-render-into-populates-with-rendered-text ()
  "Rendering produces non-empty text in the target buffer."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((buf (generate-new-buffer " *mdr-test-render*")))
    (unwind-protect
        (progn
          (+markdown-rich--render-html-into "<h1>Hello</h1>" buf)
          (with-current-buffer buf
            (should (string-match-p "Hello" (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest +markdown-rich-test-render-into-replaces-prior-content ()
  "Re-rendering replaces (not appends to) prior content."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((buf (generate-new-buffer " *mdr-test-render*")))
    (unwind-protect
        (progn
          (+markdown-rich--render-html-into "<p>first</p>" buf)
          (+markdown-rich--render-html-into "<p>second</p>" buf)
          (with-current-buffer buf
            (should-not (string-match-p "first" (buffer-string)))
            (should (string-match-p "second" (buffer-string)))))
      (kill-buffer buf))))

;;;; ---- ensure-render-buffer ----

(ert-deftest +markdown-rich-test-ensure-render-buffer-returns-named-buffer ()
  "ensure-render-buffer returns a buffer with the expected name."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (cl-letf (((symbol-function '+markdown-rich--convert-to-html)
             (lambda (_md) "<p>rendered</p>")))
    (+mdr-test--with-src-buffer "# Title"
      (rename-buffer "mdr-src.md" t)
      (unwind-protect
          (let ((buf (+markdown-rich--ensure-render-buffer (current-buffer))))
            (should (buffer-live-p buf))
            (should (equal (buffer-name buf) "*Markdown Render: mdr-src.md*")))
        (+mdr-test--cleanup-render-buffer (current-buffer))))))

(ert-deftest +markdown-rich-test-ensure-render-buffer-sets-back-link ()
  "Render buffer's `+markdown-rich--source-buffer' points at the source."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (cl-letf (((symbol-function '+markdown-rich--convert-to-html)
             (lambda (_md) "<p>x</p>")))
    (+mdr-test--with-src-buffer "src content"
      (rename-buffer "mdr-back.md" t)
      (unwind-protect
          (let* ((src (current-buffer))
                 (buf (+markdown-rich--ensure-render-buffer src)))
            (should (eq (buffer-local-value '+markdown-rich--source-buffer buf)
                        src)))
        (+mdr-test--cleanup-render-buffer (current-buffer))))))

(ert-deftest +markdown-rich-test-ensure-render-buffer-leaves-source-unmodified ()
  "ensure-render-buffer does not alter the source buffer's content."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (cl-letf (((symbol-function '+markdown-rich--convert-to-html)
             (lambda (_md) "<p>x</p>")))
    (+mdr-test--with-src-buffer "untouched source"
      (rename-buffer "mdr-untouched.md" t)
      (unwind-protect
          (progn
            (+markdown-rich--ensure-render-buffer (current-buffer))
            (should (equal (buffer-string) "untouched source")))
        (+mdr-test--cleanup-render-buffer (current-buffer))))))

;;;; ---- rerender ----

(ert-deftest +markdown-rich-test-rerender-noop-when-no-render-buffer ()
  "rerender is a no-op (no error) when source has no linked render buffer."
  (+mdr-test--with-src-buffer "anything"
    (should-not (+markdown-rich--rerender (current-buffer)))))

(ert-deftest +markdown-rich-test-rerender-updates-existing-render-buffer ()
  "rerender refreshes the linked render buffer with new content."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((html-fn (lambda (md) (format "<p>%s</p>" md))))
    (cl-letf (((symbol-function '+markdown-rich--convert-to-html) html-fn))
      (+mdr-test--with-src-buffer "first"
        (rename-buffer "mdr-rerender.md" t)
        (unwind-protect
            (let* ((src (current-buffer))
                   (buf (+markdown-rich--ensure-render-buffer src)))
              (setq-local +markdown-rich--render-buffer buf)
              (erase-buffer)
              (insert "second")
              (+markdown-rich--rerender src)
              (with-current-buffer buf
                (should (string-match-p "second" (buffer-string)))
                (should-not (string-match-p "first" (buffer-string)))))
          (+mdr-test--cleanup-render-buffer (current-buffer)))))))

;;;; ---- minor mode ----

(ert-deftest +markdown-rich-test-mode-on-creates-render-buffer ()
  "Enabling `+markdown-rich-mode' creates the linked render buffer."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (cl-letf (((symbol-function '+markdown-rich--convert-to-html)
             (lambda (_md) "<p>x</p>"))
            ((symbol-function 'display-buffer) #'ignore))
    (+mdr-test--with-src-buffer "src"
      (rename-buffer "mdr-mode-on.md" t)
      (unwind-protect
          (progn
            (+markdown-rich-mode 1)
            (should (buffer-live-p +markdown-rich--render-buffer))
            (should (equal (buffer-name +markdown-rich--render-buffer)
                           "*Markdown Render: mdr-mode-on.md*")))
        (+markdown-rich-mode -1)
        (+mdr-test--cleanup-render-buffer (current-buffer))))))

(ert-deftest +markdown-rich-test-mode-on-installs-after-save-hook ()
  "Enabling the mode installs the after-save hook buffer-locally."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (cl-letf (((symbol-function '+markdown-rich--convert-to-html)
             (lambda (_md) "<p>x</p>"))
            ((symbol-function 'display-buffer) #'ignore))
    (+mdr-test--with-src-buffer "src"
      (rename-buffer "mdr-mode-hook.md" t)
      (unwind-protect
          (progn
            (+markdown-rich-mode 1)
            (should (memq '+markdown-rich--after-save-hook
                          (buffer-local-value 'after-save-hook (current-buffer)))))
        (+markdown-rich-mode -1)
        (+mdr-test--cleanup-render-buffer (current-buffer))))))

(ert-deftest +markdown-rich-test-mode-off-removes-after-save-hook ()
  "Disabling the mode removes the after-save hook."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (cl-letf (((symbol-function '+markdown-rich--convert-to-html)
             (lambda (_md) "<p>x</p>"))
            ((symbol-function 'display-buffer) #'ignore))
    (+mdr-test--with-src-buffer "src"
      (rename-buffer "mdr-mode-off-hook.md" t)
      (unwind-protect
          (progn
            (+markdown-rich-mode 1)
            (+markdown-rich-mode -1)
            (should-not (memq '+markdown-rich--after-save-hook
                              (buffer-local-value 'after-save-hook (current-buffer)))))
        (+mdr-test--cleanup-render-buffer (current-buffer))))))

(ert-deftest +markdown-rich-test-mode-off-kills-render-buffer ()
  "Disabling the mode kills the linked render buffer."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (cl-letf (((symbol-function '+markdown-rich--convert-to-html)
             (lambda (_md) "<p>x</p>"))
            ((symbol-function 'display-buffer) #'ignore))
    (+mdr-test--with-src-buffer "src"
      (rename-buffer "mdr-mode-off-kill.md" t)
      (let (rbuf)
        (unwind-protect
            (progn
              (+markdown-rich-mode 1)
              (setq rbuf +markdown-rich--render-buffer)
              (+markdown-rich-mode -1)
              (should-not (buffer-live-p rbuf))
              (should (null +markdown-rich--render-buffer)))
          (when (buffer-live-p rbuf) (kill-buffer rbuf)))))))

(ert-deftest +markdown-rich-test-after-save-hook-refreshes-render ()
  "Saving the source buffer (firing after-save-hook) refreshes the render."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((html-fn (lambda (md) (format "<p>%s</p>" md))))
    (cl-letf (((symbol-function '+markdown-rich--convert-to-html) html-fn)
              ((symbol-function 'display-buffer) #'ignore))
      (+mdr-test--with-src-buffer "before"
        (rename-buffer "mdr-after-save.md" t)
        (unwind-protect
            (progn
              (+markdown-rich-mode 1)
              (erase-buffer)
              (insert "after")
              (run-hooks 'after-save-hook)
              (with-current-buffer +markdown-rich--render-buffer
                (should (string-match-p "after" (buffer-string)))
                (should-not (string-match-p "before" (buffer-string)))))
          (+markdown-rich-mode -1)
          (+mdr-test--cleanup-render-buffer (current-buffer)))))))

;;;; ---- one-shot interactive command ----

(ert-deftest +markdown-rich-test-render-buffer-creates-named-buffer ()
  "`+markdown-rich-render-buffer' creates a buffer with the expected name."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (cl-letf (((symbol-function '+markdown-rich--convert-to-html)
             (lambda (_md) "<p>x</p>"))
            ((symbol-function 'switch-to-buffer) #'ignore))
    (+mdr-test--with-src-buffer "# hi"
      (rename-buffer "mdr-oneshot.md" t)
      (unwind-protect
          (let ((buf (+markdown-rich-render-buffer)))
            (should (buffer-live-p buf))
            (should (equal (buffer-name buf)
                           "*Markdown Render: mdr-oneshot.md*")))
        (+mdr-test--cleanup-render-buffer (current-buffer))))))

(ert-deftest +markdown-rich-test-render-buffer-leaves-source-content-unmodified ()
  "`+markdown-rich-render-buffer' does not alter source buffer content."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (cl-letf (((symbol-function '+markdown-rich--convert-to-html)
             (lambda (_md) "<p>x</p>"))
            ((symbol-function 'switch-to-buffer) #'ignore))
    (+mdr-test--with-src-buffer "untouched source"
      (rename-buffer "mdr-untouched-oneshot.md" t)
      (unwind-protect
          (progn
            (+markdown-rich-render-buffer)
            (should (equal (buffer-string) "untouched source")))
        (+mdr-test--cleanup-render-buffer (current-buffer))))))

(provide 'test-markdown-rich)
;;; test-markdown-rich.el ends here
