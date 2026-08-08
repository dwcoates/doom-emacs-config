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

;;;; ---- Tests: agent-repl--close-panels-before-open ----

(ert-deftest agent-repl-test-close-on-open-closes-when-panels-visible ()
  "Advice closes panels when they are visible and the target is not a panel."
  (agent-repl-test--with-clean-state
    (let ((close-calls 0))
      (cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--simple-hide-and-preserve-status)
                 (lambda () (cl-incf close-calls))))
        (agent-repl--close-panels-before-open "some-file.txt")
        (should (= close-calls 1))))))

(ert-deftest agent-repl-test-close-on-open-no-op-when-panels-hidden ()
  "Advice does not close panels when they are not currently visible."
  (agent-repl-test--with-clean-state
    (let ((close-calls 0))
      (cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () nil))
                ((symbol-function 'agent-repl--simple-hide-and-preserve-status)
                 (lambda () (cl-incf close-calls))))
        (agent-repl--close-panels-before-open "some-file.txt")
        (should (= close-calls 0))))))

(ert-deftest agent-repl-test-close-on-open-no-op-when-target-is-panel ()
  "Advice does not close panels when the open target is itself a panel buffer."
  (agent-repl-test--with-clean-state
    (let ((close-calls 0)
          (panel-buf (get-buffer-create "*agent-panel-target*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                    ((symbol-function 'agent-repl--agent-panel-buffer-p)
                     (lambda (&optional buf) (eq buf panel-buf)))
                    ((symbol-function 'agent-repl--simple-hide-and-preserve-status)
                     (lambda () (cl-incf close-calls))))
            (agent-repl--close-panels-before-open panel-buf)
            (should (= close-calls 0)))
        (kill-buffer panel-buf)))))

(ert-deftest agent-repl-test-close-on-open-closes-for-non-panel-buffer-arg ()
  "Advice closes panels when the buffer arg is a live non-panel buffer."
  (agent-repl-test--with-clean-state
    (let ((close-calls 0)
          (other-buf (get-buffer-create "*ordinary-buffer*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                    ((symbol-function 'agent-repl--agent-panel-buffer-p)
                     (lambda (&optional _buf) nil))
                    ((symbol-function 'agent-repl--simple-hide-and-preserve-status)
                     (lambda () (cl-incf close-calls))))
            (agent-repl--close-panels-before-open other-buf)
            (should (= close-calls 1)))
        (kill-buffer other-buf)))))

(ert-deftest agent-repl-test-close-on-open-reentrancy-guard ()
  "Advice does not recurse while it is already closing panels."
  (agent-repl-test--with-clean-state
    (let ((close-calls 0))
      (cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--simple-hide-and-preserve-status)
                 (lambda ()
                   ;; Simulate the layout-restoring close re-entering an
                   ;; advised primitive: it must be a no-op the second time.
                   (cl-incf close-calls)
                   (agent-repl--close-panels-before-open "inner-file.txt"))))
        (agent-repl--close-panels-before-open "outer-file.txt")
        (should (= close-calls 1))))))

(ert-deftest agent-repl-test-close-on-open-ignores-nil-target ()
  "Advice closes panels when invoked with no args (interactive picker case)."
  (agent-repl-test--with-clean-state
    (let ((close-calls 0))
      (cl-letf (((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--simple-hide-and-preserve-status)
                 (lambda () (cl-incf close-calls))))
        (agent-repl--close-panels-before-open)
        (should (= close-calls 1))))))

(ert-deftest agent-repl-test-close-on-open-logs-close-decision-and-outcome ()
  "Advice logs its target classification, close decision, and completion."
  (agent-repl-test--with-clean-state
    (let (logs)
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "log-ws"))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--simple-hide-and-preserve-status)
                 (lambda () nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (ws fmt &rest args)
                   (push (list ws fmt args) logs))))
        (agent-repl--close-panels-before-open "some-file.txt")
        (setq logs (nreverse logs))
        (should (= (length logs) 3))
        (should (equal (mapcar #'car logs) '("log-ws" "log-ws" "log-ws")))
        (should (string-match-p "classified target" (nth 1 (nth 0 logs))))
        (should (string-match-p "closing panels" (nth 1 (nth 1 logs))))
        (should (string-match-p "close completed" (nth 1 (nth 2 logs))))))))

;;;; ---- Tests: agent-repl--open-target-is-panel-p ----

(ert-deftest agent-repl-test-open-target-is-panel-p-filename-string ()
  "A filename string that names no buffer is not treated as a panel target."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--open-target-is-panel-p
                 (list "/no/such/buffer/path.txt")))))

(ert-deftest agent-repl-test-open-target-is-panel-p-panel-buffer ()
  "A live panel buffer object is recognized as a panel target."
  (agent-repl-test--with-clean-state
    (let ((panel-buf (get-buffer-create "*agent-panel-x*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--agent-panel-buffer-p)
                     (lambda (&optional buf) (eq buf panel-buf))))
            (should (agent-repl--open-target-is-panel-p (list panel-buf))))
        (kill-buffer panel-buf)))))

(ert-deftest agent-repl-test-open-target-is-panel-p-non-panel-buffer ()
  "A live non-panel buffer object is not a panel target."
  (agent-repl-test--with-clean-state
    (let ((other-buf (get-buffer-create "*plain-x*")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--agent-panel-buffer-p)
                     (lambda (&optional _buf) nil)))
            (should-not (agent-repl--open-target-is-panel-p (list other-buf))))
        (kill-buffer other-buf)))))

;;;; ---- Tests: advice registration ----

(ert-deftest agent-repl-test-close-on-open-advice-registered-on-switch-to-buffer ()
  "`switch-to-buffer' has the close-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'agent-repl--close-panels-before-open)
                     (setq found t)))
                 'switch-to-buffer)
    (should found)))

(ert-deftest agent-repl-test-close-on-open-advice-registered-on-find-file ()
  "`find-file' has the close-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'agent-repl--close-panels-before-open)
                     (setq found t)))
                 'find-file)
    (should found)))

(ert-deftest agent-repl-test-close-on-open-advice-registered-on-pop-to-buffer-same-window ()
  "`pop-to-buffer-same-window' has the close-panels advice attached."
  (let ((found nil))
    (advice-mapc (lambda (fn _props)
                   (when (eq fn #'agent-repl--close-panels-before-open)
                     (setq found t)))
                 'pop-to-buffer-same-window)
    (should found)))

;;; test-close-panels-on-open.el ends here
