;;; config/doom-tomorrow-night/test-derive.el -*- lexical-binding: t; -*-
;;
;; Tests for modules/config/doom-tomorrow-night/derive.el.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-derive.el -f ert-run-tests-batch-and-exit

(require 'ert)
(load (expand-file-name "derive" (file-name-directory
                                  (or load-file-name buffer-file-name)))
      nil t)

(defconst test-derive--upstream
  '((require 'doom-themes)
    (defcustom upstream-pad nil "Pad.")
    (def-doom-theme upstream
      "Upstream doc."
      :family 'upstream
      :background-mode 'dark
      ((bg     '("#1d1f21" nil nil))
       (bg-alt '("#161719" nil nil))
       (base1  '("#1b1b1b" "#1b1b1b"))
       (modeline-bg `(,(doom-darken (car bg-alt) 0.3))))
      ((default :background bg))
      ()))
  "A minimal upstream theme file's forms.")

(defun test-derive--theme (forms)
  (seq-find (lambda (f) (eq (car-safe f) 'def-doom-theme)) forms))

(ert-deftest test-derive/renames-the-theme ()
  ;; Arrange / Act
  (let ((theme (test-derive--theme
                (doom-tomorrow-night-derive-theme test-derive--upstream 'warm nil))))
    ;; Assert
    (should (eq (nth 1 theme) 'warm))))

(ert-deftest test-derive/keeps-doc-and-properties ()
  ;; Arrange / Act
  (let ((theme (test-derive--theme
                (doom-tomorrow-night-derive-theme test-derive--upstream 'warm nil))))
    ;; Assert
    (should (equal (seq-take (cddr theme) 5)
                   '("Upstream doc." :family 'upstream :background-mode 'dark)))))

(ert-deftest test-derive/swaps-only-the-gui-color-of-a-ground ()
  ;; Arrange
  (let* ((forms (doom-tomorrow-night-derive-theme
                 test-derive--upstream 'warm '((bg . "#21201c"))))
         ;; Act
         (defs (nth 7 (test-derive--theme forms))))
    ;; Assert
    (should (equal (assq 'bg defs) '(bg '("#21201c" nil nil))))))

(ert-deftest test-derive/leaves-unswapped-entries-and-derivations-alone ()
  ;; Arrange
  (let* ((forms (doom-tomorrow-night-derive-theme
                 test-derive--upstream 'warm '((bg-alt . "#1a1814"))))
         ;; Act
         (defs (nth 7 (test-derive--theme forms))))
    ;; Assert
    (should (equal (assq 'base1 defs) '(base1 '("#1b1b1b" "#1b1b1b"))))
    (should (equal (assq 'modeline-bg defs)
                   '(modeline-bg `(,(doom-darken (car bg-alt) 0.3)))))))

(ert-deftest test-derive/keeps-extra-faces-and-vars ()
  ;; Arrange / Act
  (let ((theme (test-derive--theme
                (doom-tomorrow-night-derive-theme test-derive--upstream 'warm nil))))
    ;; Assert
    (should (equal (nthcdr 8 theme) '(((default :background bg)) ())))))

(ert-deftest test-derive/keeps-every-other-form-in-order ()
  ;; Arrange / Act
  (let ((forms (doom-tomorrow-night-derive-theme test-derive--upstream 'warm nil)))
    ;; Assert
    (should (equal (seq-take forms 2) (seq-take test-derive--upstream 2)))))

(ert-deftest test-derive/errors-on-an-unknown-ground ()
  ;; Arrange / Act
  (let ((err (should-error (doom-tomorrow-night-derive-theme
                            test-derive--upstream 'warm '((nope . "#000000"))))))
    ;; Assert
    (should (string-match-p "no literal entry nope" (cadr err)))))

(ert-deftest test-derive/errors-on-a-computed-ground ()
  ;; Arrange / Act
  (let ((err (should-error (doom-tomorrow-night-derive-theme
                            test-derive--upstream 'warm '((modeline-bg . "#000000"))))))
    ;; Assert
    (should (string-match-p "no literal entry modeline-bg" (cadr err)))))

(ert-deftest test-derive/errors-without-a-theme-form ()
  ;; Arrange / Act
  (let ((err (should-error (doom-tomorrow-night-derive-theme
                            '((require 'doom-themes)) 'warm nil))))
    ;; Assert
    (should (string-match-p "found 0" (cadr err)))))

(ert-deftest test-derive/reads-every-form-of-a-file ()
  ;; Arrange
  (let ((file (make-temp-file "derive" nil ".el" "(a 1)\n;; c\n(b 2)\n")))
    (unwind-protect
        ;; Act / Assert
        (should (equal (doom-tomorrow-night-read-forms file) '((a 1) (b 2))))
      (delete-file file))))
