;;; test-minibuffer-ground.el --- Tests for the minibuffer's ground -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for minibuffer-ground.el -- the minibuffer and the echo area wear
;; the composer's color, `agent-repl-input-background'.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defconst agent-repl-test-minibuffer-ground--source
  (expand-file-name "minibuffer-ground.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The module's source, for the scan below.")

(defun agent-repl-test-minibuffer-ground--backgrounds (face)
  "Return every :background the current buffer remaps FACE to, newest first."
  (delq nil (mapcar (lambda (spec) (and (listp spec) (plist-get spec :background)))
                    (cdr (assq face face-remapping-alist)))))

(defmacro agent-repl-test-minibuffer-ground--with-buffer (&rest body)
  "Run BODY in a fresh buffer, `logs' collecting every canonical log line."
  (declare (indent 0))
  `(let ((logs nil)
         (agent-repl-input-background "#123456"))
     (cl-letf (((symbol-function 'agent-repl--log) (agent-repl-test--record-into logs)))
       (with-temp-buffer ,@body))))

;;;; ---- Painting one buffer ----

(ert-deftest agent-repl-test-minibuffer-ground-paints-default-and-fringe ()
  "A painted buffer remaps its default and fringe to the composer's color."
  (agent-repl-test-minibuffer-ground--with-buffer
    ;; Act
    (agent-repl-minibuffer-ground-paint)
    ;; Assert
    (should (equal (list (agent-repl-test-minibuffer-ground--backgrounds 'default)
                         (agent-repl-test-minibuffer-ground--backgrounds 'fringe))
                   '(("#123456") ("#123456"))))))

(ert-deftest agent-repl-test-minibuffer-ground-same-color-paints-once ()
  "A repaint in the color already worn adds no second remap and logs nothing."
  (agent-repl-test-minibuffer-ground--with-buffer
    ;; Arrange
    (agent-repl-minibuffer-ground-paint)
    ;; Act
    (agent-repl-minibuffer-ground-paint)
    ;; Assert
    (should (equal (agent-repl-test-minibuffer-ground--backgrounds 'default) '("#123456")))
    (should (= (length logs) 1))))

(ert-deftest agent-repl-test-minibuffer-ground-new-color-takes-the-old-back ()
  "A repaint in a new color leaves only the new color's remap."
  (agent-repl-test-minibuffer-ground--with-buffer
    ;; Arrange
    (agent-repl-minibuffer-ground-paint)
    ;; Act
    (let ((agent-repl-input-background "#abcdef"))
      (agent-repl-minibuffer-ground-paint))
    ;; Assert
    (should (equal (list (agent-repl-test-minibuffer-ground--backgrounds 'default)
                         (agent-repl-test-minibuffer-ground--backgrounds 'fringe))
                   '(("#abcdef") ("#abcdef"))))))

(ert-deftest agent-repl-test-minibuffer-ground-paint-logs-buffer-and-colors ()
  "A paint records the buffer, the new color and the color it replaced."
  (agent-repl-test-minibuffer-ground--with-buffer
    ;; Arrange
    (rename-buffer " *test-minibuffer-ground*" t)
    (agent-repl-minibuffer-ground-paint)
    ;; Act
    (let ((agent-repl-input-background "#abcdef"))
      (agent-repl-minibuffer-ground-paint))
    ;; Assert
    (should (string-match-p
             "elisp\\.minibuffer-ground\\.paint buffer= \\*test-minibuffer-ground\\*.* color=#abcdef from=\"#123456\""
             (car logs)))))

;;;; ---- Painting every minibuffer and echo area ----

(ert-deftest agent-repl-test-minibuffer-ground-paint-all-paints-each-echo-area ()
  "Both echo area buffers wear the composer's color after a paint-all."
  (let ((original agent-repl-input-background))
    (unwind-protect
        (let ((agent-repl-input-background "#123456"))
          (cl-letf (((symbol-function 'agent-repl--log) #'ignore))
            ;; Act
            (agent-repl-minibuffer-ground-paint-all))
          ;; Assert
          (should (equal (mapcar (lambda (name)
                                   (with-current-buffer name
                                     (car (agent-repl-test-minibuffer-ground--backgrounds 'default))))
                                 agent-repl--minibuffer-ground-echo-buffers)
                         '("#123456" "#123456"))))
      (cl-letf (((symbol-function 'agent-repl--log) #'ignore))
        (let ((agent-repl-input-background original))
          (agent-repl-minibuffer-ground-paint-all))))))

(ert-deftest agent-repl-test-minibuffer-ground-paint-all-paints-each-minibuffer ()
  "Every minibuffer wears the composer's color after a paint-all."
  (let ((original agent-repl-input-background))
    (unwind-protect
        (let ((agent-repl-input-background "#123456"))
          (cl-letf (((symbol-function 'agent-repl--log) #'ignore))
            ;; Act
            (agent-repl-minibuffer-ground-paint-all))
          ;; Assert
          (let ((minibuffers (seq-filter #'minibufferp (buffer-list))))
            (should minibuffers)
            (dolist (buf minibuffers)
              (with-current-buffer buf
                (should (equal (car (agent-repl-test-minibuffer-ground--backgrounds 'default))
                               "#123456"))))))
      (cl-letf (((symbol-function 'agent-repl--log) #'ignore))
        (let ((agent-repl-input-background original))
          (agent-repl-minibuffer-ground-paint-all))))))

;;;; ---- Following the minibuffer ----

(ert-deftest agent-repl-test-minibuffer-ground-entering-the-minibuffer-paints ()
  "Each minibuffer entry repaints, since entering kills its local remap."
  (should (memq #'agent-repl-minibuffer-ground-paint minibuffer-setup-hook)))

(ert-deftest agent-repl-test-minibuffer-ground-leaving-the-minibuffer-paints ()
  "Each minibuffer exit repaints, since `minibuffer-inactive-mode' kills it too."
  (should (memq #'agent-repl-minibuffer-ground-paint minibuffer-inactive-mode-hook)))

(ert-deftest agent-repl-test-minibuffer-ground-inactive-mode-repaints ()
  "A minibuffer left in `minibuffer-inactive-mode' wears the color again."
  (agent-repl-test-minibuffer-ground--with-buffer
    ;; Act
    (minibuffer-inactive-mode)
    ;; Assert
    (should (equal (agent-repl-test-minibuffer-ground--backgrounds 'default) '("#123456")))))

;;;; ---- One color ----

(ert-deftest agent-repl-test-minibuffer-ground-names-no-color-of-its-own ()
  "The module writes no hex color: it wears `agent-repl-input-background'."
  (with-temp-buffer
    ;; Arrange
    (insert-file-contents agent-repl-test-minibuffer-ground--source)
    ;; Act
    (let ((literal (re-search-forward "#[0-9a-fA-F]\\{6\\}" nil t)))
      ;; Assert
      (should-not literal))))

(provide 'test-minibuffer-ground)
;;; test-minibuffer-ground.el ends here
