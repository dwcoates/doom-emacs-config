;;; test-close-panels-on-open.el --- ERT tests for close-panels-on-open.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the :before advice that closes Claude panels when a buffer
;; or file is opened while the panels are visible.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-close-panels-on-open.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: claude-repl--close-panels-before-open ----

(ert-deftest claude-repl-test-close-on-open-closes-when-panels-visible ()
  "Advice closes panels when they are visible and the target is not a panel."
  (claude-repl-test--with-clean-state
    (let ((close-calls 0))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'claude-repl--simple-hide-and-preserve-status)
                 (lambda () (cl-incf close-calls))))
        (claude-repl--close-panels-before-open "some-file.txt")
        (should (= close-calls 1))))))

(ert-deftest claude-repl-test-close-on-open-no-op-when-panels-hidden ()
  "Advice does not close panels when they are not currently visible."
  (claude-repl-test--with-clean-state
    (let ((close-calls 0))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'claude-repl--simple-hide-and-preserve-status)
                 (lambda () (cl-incf close-calls))))
        (claude-repl--close-panels-before-open "some-file.txt")
        (should (= close-calls 0))))))

(ert-deftest claude-repl-test-close-on-open-no-op-when-target-is-panel ()
  "Advice does not close panels when the open target is itself a panel buffer."
  (claude-repl-test--with-clean-state
    (let ((close-calls 0)
          (panel-buf (get-buffer-create "*claude-panel-target*")))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                    ((symbol-function 'claude-repl--claude-panel-buffer-p)
                     (lambda (&optional buf) (eq buf panel-buf)))
                    ((symbol-function 'claude-repl--simple-hide-and-preserve-status)
                     (lambda () (cl-incf close-calls))))
            (claude-repl--close-panels-before-open panel-buf)
            (should (= close-calls 0)))
        (kill-buffer panel-buf)))))

(ert-deftest claude-repl-test-close-on-open-closes-for-non-panel-buffer-arg ()
  "Advice closes panels when the buffer arg is a live non-panel buffer."
  (claude-repl-test--with-clean-state
    (let ((close-calls 0)
          (other-buf (get-buffer-create "*ordinary-buffer*")))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                    ((symbol-function 'claude-repl--claude-panel-buffer-p)
                     (lambda (&optional _buf) nil))
                    ((symbol-function 'claude-repl--simple-hide-and-preserve-status)
                     (lambda () (cl-incf close-calls))))
            (claude-repl--close-panels-before-open other-buf)
            (should (= close-calls 1)))
        (kill-buffer other-buf)))))

(ert-deftest claude-repl-test-close-on-open-reentrancy-guard ()
  "Advice does not recurse while it is already closing panels."
  (claude-repl-test--with-clean-state
    (let ((close-calls 0))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'claude-repl--simple-hide-and-preserve-status)
                 (lambda ()
                   ;; Simulate the layout-restoring close re-entering an
                   ;; advised primitive: it must be a no-op the second time.
                   (cl-incf close-calls)
                   (claude-repl--close-panels-before-open "inner-file.txt"))))
        (claude-repl--close-panels-before-open "outer-file.txt")
        (should (= close-calls 1))))))

(ert-deftest claude-repl-test-close-on-open-ignores-nil-target ()
  "Advice closes panels when invoked with no args (interactive picker case)."
  (claude-repl-test--with-clean-state
    (let ((close-calls 0))
      (cl-letf (((symbol-function 'claude-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'claude-repl--simple-hide-and-preserve-status)
                 (lambda () (cl-incf close-calls))))
        (claude-repl--close-panels-before-open)
        (should (= close-calls 1))))))

;;;; ---- Tests: claude-repl--open-target-is-panel-p ----

(ert-deftest claude-repl-test-open-target-is-panel-p-filename-string ()
  "A filename string that names no buffer is not treated as a panel target."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--open-target-is-panel-p
                 (list "/no/such/buffer/path.txt")))))

(ert-deftest claude-repl-test-open-target-is-panel-p-panel-buffer ()
  "A live panel buffer object is recognized as a panel target."
  (claude-repl-test--with-clean-state
    (let ((panel-buf (get-buffer-create "*claude-panel-x*")))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--claude-panel-buffer-p)
                     (lambda (&optional buf) (eq buf panel-buf))))
            (should (claude-repl--open-target-is-panel-p (list panel-buf))))
        (kill-buffer panel-buf)))))

(ert-deftest claude-repl-test-open-target-is-panel-p-non-panel-buffer ()
  "A live non-panel buffer object is not a panel target."
  (claude-repl-test--with-clean-state
    (let ((other-buf (get-buffer-create "*plain-x*")))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl--claude-panel-buffer-p)
                     (lambda (&optional _buf) nil)))
            (should-not (claude-repl--open-target-is-panel-p (list other-buf))))
        (kill-buffer other-buf)))))

;;;; ---- Tests: advice registration ----

(ert-deftest claude-repl-test-close-on-open-advice-registered-on-switch-to-buffer ()
  "`switch-to-buffer' has the close-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'claude-repl--close-panels-before-open)
                     (setq found t)))
                 'switch-to-buffer)
    (should found)))

(ert-deftest claude-repl-test-close-on-open-advice-registered-on-find-file ()
  "`find-file' has the close-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'claude-repl--close-panels-before-open)
                     (setq found t)))
                 'find-file)
    (should found)))

(ert-deftest claude-repl-test-close-on-open-advice-registered-on-pop-to-buffer-same-window ()
  "`pop-to-buffer-same-window' has the close-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'claude-repl--close-panels-before-open)
                     (setq found t)))
                 'pop-to-buffer-same-window)
    (should found)))

;;; test-close-panels-on-open.el ends here
