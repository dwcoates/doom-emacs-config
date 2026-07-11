;;; test-frontends.el --- ERT tests for frontends.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the presentation-frontend registry: struct/registration
;; invariants, workspace resolution, pair validation, dispatch, and the
;; selection/switch commands (capability fns mocked via cl-letf).
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-frontends.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Helpers -------------------------------------------------------------

(defun agent-repl-test--make-frontend (name &rest overrides)
  "Return a fully-populated test frontend NAME with optional OVERRIDES."
  (apply #'agent-repl-frontend-create
         (append overrides
                 (list :name name
                       :open-fn #'ignore
                       :kill-fn #'ignore
                       :send-fn #'ignore
                       :interrupt-fn #'ignore
                       :running-p-fn #'ignore
                       :show-fn #'ignore
                       :hide-fn #'ignore
                       :supported-backends '(claude)))))

(defmacro agent-repl-test--with-frontend-registry (&rest body)
  "Run BODY against a scratch copy of the frontend registry."
  `(let ((agent-repl--frontends (copy-hash-table agent-repl--frontends)))
     ,@body))

;;;; ---- Registration ----------------------------------------------------------

(ert-deftest agent-repl-test-frontends-builtin-both-registered ()
  "Module load registers exactly the vterm and gui frontends."
  ;; Assert
  (should (agent-repl-frontend-get 'vterm))
  (should (agent-repl-frontend-get 'gui)))

(ert-deftest agent-repl-test-frontends-register-rejects-non-struct ()
  "Registering a non-struct signals."
  ;; Act / Assert
  (should-error (agent-repl-register-frontend "not-a-frontend")))

(ert-deftest agent-repl-test-frontends-register-rejects-missing-slot ()
  "Registering a frontend without a required capability signals."
  ;; Arrange — no :send-fn.
  (agent-repl-test--with-frontend-registry
   (should-error
    (agent-repl-register-frontend
     (agent-repl-frontend-create
      :name 'broken
      :open-fn #'ignore
      :kill-fn #'ignore
      :interrupt-fn #'ignore
      :running-p-fn #'ignore
      :supported-backends '(claude))))))

(ert-deftest agent-repl-test-frontends-get-unknown-signals ()
  "Resolving an unregistered frontend signals, never falls back."
  ;; Act / Assert
  (should-error (agent-repl-frontend-get 'holograph)))

;;;; ---- Resolution ---------------------------------------------------------------

(ert-deftest agent-repl-test-frontends-ws-resolution-prefers-plist ()
  "The workspace's :frontend key wins over the default."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (let ((agent-repl-default-frontend 'vterm))
      ;; Act / Assert
      (should (eq (agent-repl--ws-frontend-name "ws1") 'gui))
      (should (agent-repl--ws-gui-frontend-p "ws1")))))

(ert-deftest agent-repl-test-frontends-ws-resolution-defaults ()
  "A workspace without :frontend resolves to the default."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-frontend 'vterm))
      ;; Act / Assert
      (should (eq (agent-repl--ws-frontend-name "ws1") 'vterm))
      (should-not (agent-repl--ws-gui-frontend-p "ws1")))))

;;;; ---- Pair validation -------------------------------------------------------------

(ert-deftest agent-repl-test-frontends-validate-pair-accepts-supported ()
  "A supported frontend/backend pair validates."
  ;; Act / Assert
  (should (agent-repl--frontend-validate-pair 'gui 'claude))
  (should (agent-repl--frontend-validate-pair 'vterm 'codex)))

(ert-deftest agent-repl-test-frontends-validate-pair-rejects-unsupported ()
  "gui+codex fails loudly until a codex shim exists."
  ;; Act / Assert
  (should-error (agent-repl--frontend-validate-pair 'gui 'codex)
                :type 'user-error))

;;;; ---- Dispatch ---------------------------------------------------------------------

(ert-deftest agent-repl-test-frontends-dispatch-send-routes-by-ws ()
  "Send dispatch reaches the workspace's frontend capability."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (let ((got nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'probe :send-fn (lambda (ws input raw settle)
                           (setq got (list ws input raw settle)))))
       (agent-repl--ws-put "ws1" :frontend 'probe)
       ;; Act
       (agent-repl--frontend-dispatch-send "ws1" "in" "raw" 'settle)
       ;; Assert
       (should (equal got '("ws1" "in" "raw" settle)))))))

(ert-deftest agent-repl-test-frontends-dispatch-interrupt-carries-kind ()
  "Interrupt dispatch forwards the gesture kind."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (let ((got nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'probe :interrupt-fn (lambda (ws kind) (setq got (list ws kind)) t)))
       (agent-repl--ws-put "ws1" :frontend 'probe)
       ;; Act / Assert
       (should (agent-repl--frontend-dispatch-interrupt "ws1" 'escape))
       (should (equal got '("ws1" escape)))))))

;;;; ---- Selection command -----------------------------------------------------------

(ert-deftest agent-repl-test-frontends-select-refuses-while-running ()
  "Selection refuses a workspace whose current frontend is running."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (agent-repl-register-frontend
      (agent-repl-test--make-frontend 'probe :running-p-fn (lambda (_ws) t)))
     (agent-repl--ws-put "ws1" :frontend 'probe)
     (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
               ((symbol-function 'completing-read) (lambda (&rest _) "vterm"))
               ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude)))
       ;; Act / Assert
       (should-error (agent-repl-select-frontend nil) :type 'user-error)))))

(ert-deftest agent-repl-test-frontends-select-sets-and-persists ()
  "Selection stamps :frontend and persists the workspace state."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((saved nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'completing-read) (lambda (&rest _) "gui"))
                ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                ((symbol-function 'agent-repl--state-save)
                 (lambda (&optional _ws) (setq saved t))))
        ;; Act
        (agent-repl-select-frontend nil)
        ;; Assert
        (should (eq (agent-repl--ws-get "ws1" :frontend) 'gui))
        (should saved)))))

;;;; ---- Switch command -----------------------------------------------------------------

(ert-deftest agent-repl-test-frontends-switch-hides-flips-opens ()
  "The switch composes close, select, open: hide old, flip, open new."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (let ((events nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'from :hide-fn (lambda (_ws) (push 'hide-from events))))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'to
         ;; Not running: the open path must fire, not show.
         :running-p-fn (lambda (_ws) nil)
         :open-fn (lambda (_ws) (push 'open-to events))
         :show-fn (lambda (_ws) (push 'show-to events))))
       (agent-repl--ws-put "ws1" :frontend 'from)
       (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                 ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                 ((symbol-function 'agent-repl--state-save) #'ignore))
         ;; Act
         (agent-repl-switch-frontend)
         ;; Assert — ordered hide then open; :frontend flipped; no show.
         (should (equal (nreverse events) '(hide-from open-to)))
         (should (eq (agent-repl--ws-get "ws1" :frontend) 'to)))))))

(ert-deftest agent-repl-test-frontends-switch-shows-running-target ()
  "A target frontend whose session survived a prior switch is SHOWN, not
re-initialized — re-opening a live session is the \"already
initialized\" trap the old kill-based switch kept hitting."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (let ((events nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'from :hide-fn (lambda (_ws) (push 'hide-from events))))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'to
         :running-p-fn (lambda (_ws) t)
         :open-fn (lambda (_ws) (push 'open-to events))
         :show-fn (lambda (_ws) (push 'show-to events))))
       (agent-repl--ws-put "ws1" :frontend 'from)
       (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                 ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                 ((symbol-function 'agent-repl--state-save) #'ignore))
         ;; Act
         (agent-repl-switch-frontend)
         ;; Assert
         (should (equal (nreverse events) '(hide-from show-to))))))))

(ert-deftest agent-repl-test-frontends-switch-never-kills-sessions ()
  "The switch composes hide+open only: neither frontend's kill runs."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (let ((killed nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'from :kill-fn (lambda (_ws) (setq killed t))))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'to :kill-fn (lambda (_ws) (setq killed t))))
       (agent-repl--ws-put "ws1" :frontend 'from)
       (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                 ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                 ((symbol-function 'agent-repl--state-save) #'ignore))
         ;; Act
         (agent-repl-switch-frontend)
         ;; Assert — sessions stay alive; continuity is free.
         (should-not killed))))))

(provide 'test-frontends)

;;; test-frontends.el ends here
