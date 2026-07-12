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
  "Run BODY against a scratch copy of the frontend registry.

Also scratch-binds `agent-repl-default-frontend', because the default and
the registry it indexes into must have the SAME lifetime.  The selection
commands adopt their choice as the default new workspaces are born with
\(`agent-repl--frontend-adopt-default'), so a BODY that switches to a
BODY-local frontend would otherwise leave the global default naming a
frontend that dies with BODY's registry copy — and every later test whose
workspace carries no `:frontend' resolves through that global."
  `(let ((agent-repl--frontends (copy-hash-table agent-repl--frontends))
         (agent-repl-default-frontend agent-repl-default-frontend))
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
  ;; Arrange — the default is scratch-bound because selection now adopts the
  ;; choice as the default for new workspaces (a global mutation).
  (agent-repl-test--with-clean-state
    (let ((saved nil)
          (agent-repl-default-frontend agent-repl-default-frontend))
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

;;;; ---- Default adoption (new-workspace inheritance) -------------------------------

(ert-deftest agent-repl-test-frontends-select-adopts-default-for-new-ws ()
  "Selection flips the default a NEW workspace is born with."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-frontend 'vterm))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'completing-read) (lambda (&rest _) "gui"))
                ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                ((symbol-function 'agent-repl--state-save) #'ignore))
        ;; Act
        (agent-repl-select-frontend nil)
        ;; Assert — a workspace with no :frontend of its own now resolves gui.
        (should (eq agent-repl-default-frontend 'gui))
        (should (eq (agent-repl--ws-frontend-name "born-later") 'gui))))))

(ert-deftest agent-repl-test-frontends-select-pins-existing-default-riding-ws ()
  "Selection pins an existing default-riding workspace so the flip can't
retroactively re-present it."
  ;; Arrange — ws2 exists and carries no :frontend of its own.
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-frontend 'vterm))
      (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'completing-read) (lambda (&rest _) "gui"))
                ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                ((symbol-function 'agent-repl--state-save) #'ignore))
        ;; Act
        (agent-repl-select-frontend nil)
        ;; Assert
        (should (eq (agent-repl--ws-get "ws2" :frontend) 'vterm))
        (should (eq (agent-repl--ws-frontend-name "ws2") 'vterm))))))

(ert-deftest agent-repl-test-frontends-select-persists-pinned-existing-ws ()
  "The pin of an existing workspace is written to its state file."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-frontend 'vterm)
          (saved nil))
      (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'completing-read) (lambda (&rest _) "gui"))
                ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                ((symbol-function 'agent-repl--state-save)
                 (lambda (&optional ws) (push ws saved))))
        ;; Act
        (agent-repl-select-frontend nil)
        ;; Assert
        (should (member "ws2" saved))))))

(ert-deftest agent-repl-test-frontends-adopt-default-leaves-explicit-ws-alone ()
  "Adoption never rewrites a workspace that already chose its own frontend."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (agent-repl-register-frontend (agent-repl-test--make-frontend 'from))
     (agent-repl-register-frontend (agent-repl-test--make-frontend 'to))
     (agent-repl-register-frontend (agent-repl-test--make-frontend 'other))
     (let ((agent-repl-default-frontend 'from))
       (agent-repl--ws-put "ws2" :frontend 'other)
       (cl-letf (((symbol-function 'agent-repl--state-save) #'ignore))
         ;; Act
         (agent-repl--frontend-adopt-default 'to)
         ;; Assert
         (should (eq (agent-repl--ws-get "ws2" :frontend) 'other)))))))

(ert-deftest agent-repl-test-frontends-adopt-default-noop-when-unchanged ()
  "Adopting the frontend that is already the default pins nothing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-frontend 'vterm))
      (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
      (cl-letf (((symbol-function 'agent-repl--state-save) #'ignore))
        ;; Act
        (agent-repl--frontend-adopt-default 'vterm)
        ;; Assert — still riding the default, not pinned.
        (should-not (agent-repl--ws-get "ws2" :frontend))))))

(ert-deftest agent-repl-test-frontends-select-default-arg-pins-existing-ws ()
  "The prefix-arg (set-default) path pins existing workspaces too."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-frontend 'vterm))
      (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "gui"))
                ((symbol-function 'agent-repl--state-save) #'ignore))
        ;; Act
        (agent-repl-select-frontend t)
        ;; Assert
        (should (eq agent-repl-default-frontend 'gui))
        (should (eq (agent-repl--ws-get "ws2" :frontend) 'vterm))))))

(ert-deftest agent-repl-test-frontends-adopt-default-does-not-leak-past-clean-state ()
  "Adoption's `setq' on the global default does NOT outlive
`agent-repl-test--with-clean-state'.

The regression guard for the class of cross-file test pollution this
adoption introduced: a leaked default re-routes frontend resolution for
every later test whose workspace carries no `:frontend' of its own.  A
scratch name leaks as a hard error (unregistered); a real name like
`gui' leaks SILENTLY, which is worse.  Asserted here against the real
registry so no registry-macro scratch binding can mask it."
  ;; Arrange
  (let ((before agent-repl-default-frontend))
    ;; Act — drive a real adoption inside the clean-state scope.
    (agent-repl-test--with-clean-state
      (agent-repl--frontend-adopt-default
       (if (eq agent-repl-default-frontend 'gui) 'vterm 'gui))
      (should-not (eq agent-repl-default-frontend before)))
    ;; Assert — the scope restored it.
    (should (eq agent-repl-default-frontend before))))

;;;; ---- Switch command -----------------------------------------------------------------

(ert-deftest agent-repl-test-frontends-switch-kills-flips-opens ()
  "The switch composes kill, select, open: kill old, flip, open new."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (let ((events nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'from
         :kill-fn (lambda (_ws) (push 'kill-from events))
         :running-p-fn (lambda (_ws) nil)))
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
         ;; Assert — ordered kill then open; :frontend flipped; no show.
         (should (equal (nreverse events) '(kill-from open-to)))
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
         'from
         :kill-fn (lambda (_ws) (push 'kill-from events))
         :running-p-fn (lambda (_ws) nil)))
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
         (should (equal (nreverse events) '(kill-from show-to))))))))

(ert-deftest agent-repl-test-frontends-switch-awaits-async-stop ()
  "The switch waits out an async teardown before opening the target."
  ;; Arrange — still \"running\" for the first two post-kill polls.
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (let ((polls 0)
           (opened nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'from
         :running-p-fn (lambda (_ws) (setq polls (1+ polls)) (< polls 3))))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'to
         :running-p-fn (lambda (_ws) nil)
         :open-fn (lambda (_ws) (setq opened t))))
       (agent-repl--ws-put "ws1" :frontend 'from)
       (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                 ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                 ((symbol-function 'agent-repl--state-save) #'ignore)
                 ((symbol-function 'sleep-for) #'ignore))
         ;; Act
         (agent-repl-switch-frontend)
         ;; Assert
         (should opened)
         (should (>= polls 3)))))))

(ert-deftest agent-repl-test-frontends-switch-adopts-default-for-new-ws ()
  "The switch flips the default a NEW workspace is born with."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (agent-repl-register-frontend
      (agent-repl-test--make-frontend 'from :running-p-fn (lambda (_ws) nil)))
     (agent-repl-register-frontend
      (agent-repl-test--make-frontend 'to :running-p-fn (lambda (_ws) nil)))
     (let ((agent-repl-default-frontend 'from))
       (agent-repl--ws-put "ws1" :frontend 'from)
       (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                 ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                 ((symbol-function 'agent-repl--state-save) #'ignore))
         ;; Act
         (agent-repl-switch-frontend)
         ;; Assert
         (should (eq agent-repl-default-frontend 'to))
         (should (eq (agent-repl--ws-frontend-name "born-later") 'to)))))))

(ert-deftest agent-repl-test-frontends-switch-pins-existing-default-riding-ws ()
  "The switch pins the OTHER default-riding workspaces to their current
frontend instead of dragging them along."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (agent-repl-register-frontend
      (agent-repl-test--make-frontend 'from :running-p-fn (lambda (_ws) nil)))
     (agent-repl-register-frontend
      (agent-repl-test--make-frontend 'to :running-p-fn (lambda (_ws) nil)))
     (let ((agent-repl-default-frontend 'from))
       (agent-repl--ws-put "ws1" :frontend 'from)
       (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
       (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                 ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                 ((symbol-function 'agent-repl--state-save) #'ignore))
         ;; Act
         (agent-repl-switch-frontend)
         ;; Assert
         (should (eq (agent-repl--ws-frontend-name "ws2") 'from)))))))

(ert-deftest agent-repl-test-frontends-switch-hands-durable-session-to-target ()
  "The switch captures the durable claude uuid BEFORE the kill destroys
the session that knows it, and adopts it into the target before the
open — the conversation carries across the frontend flip."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (let ((events nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'from
         :durable-session-id-fn (lambda (_ws) (push 'capture events) "cli-uuid-3")
         :kill-fn (lambda (_ws) (push 'kill events))
         :running-p-fn (lambda (_ws) nil)))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'to
         :adopt-session-fn (lambda (_ws id) (push (cons 'adopt id) events))
         :open-fn (lambda (_ws) (push 'open events))
         :running-p-fn (lambda (_ws) nil)))
       (agent-repl--ws-put "ws1" :frontend 'from)
       (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                 ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                 ((symbol-function 'agent-repl--state-save) #'ignore))
         ;; Act
         (agent-repl-switch-frontend)
         ;; Assert — capture precedes kill; adopt precedes open.
         (should (equal (nreverse events)
                        '(capture kill (adopt . "cli-uuid-3") open))))))))

(ert-deftest agent-repl-test-frontends-switch-skips-adopt-without-durable-id ()
  "No durable session (nil uuid) means the target opens fresh — no adopt."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (let ((adopted nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'from
         :durable-session-id-fn (lambda (_ws) nil)
         :running-p-fn (lambda (_ws) nil)))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'to
         :adopt-session-fn (lambda (_ws _id) (setq adopted t))
         :running-p-fn (lambda (_ws) nil)))
       (agent-repl--ws-put "ws1" :frontend 'from)
       (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                 ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                 ((symbol-function 'agent-repl--state-save) #'ignore))
         ;; Act
         (agent-repl-switch-frontend)
         ;; Assert
         (should-not adopted))))))

(ert-deftest agent-repl-test-frontends-switch-tolerates-missing-capability-fns ()
  "Frontends without the durable-id/adopt capabilities still switch cleanly."
  ;; Arrange — neither side defines the optional slots.
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (let ((opened nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend 'from :running-p-fn (lambda (_ws) nil)))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'to
         :open-fn (lambda (_ws) (setq opened t))
         :running-p-fn (lambda (_ws) nil)))
       (agent-repl--ws-put "ws1" :frontend 'from)
       (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                 ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
                 ((symbol-function 'agent-repl--state-save) #'ignore))
         ;; Act
         (agent-repl-switch-frontend)
         ;; Assert
         (should opened))))))

(ert-deftest agent-repl-test-frontends-switch-errors-when-old-never-stops ()
  "A frontend that never stops running fails the switch loudly."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (clrhash agent-repl--frontends)
     (agent-repl-register-frontend
      (agent-repl-test--make-frontend 'from :running-p-fn (lambda (_ws) t)))
     (agent-repl-register-frontend (agent-repl-test--make-frontend 'to))
     (agent-repl--ws-put "ws1" :frontend 'from)
     (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
               ((symbol-function 'agent-repl--ws-backend-name) (lambda (_ws) 'claude))
               ((symbol-function 'agent-repl--state-save) #'ignore)
               ((symbol-function 'sleep-for) #'ignore))
       ;; Act / Assert
       (should-error (agent-repl-switch-frontend))))))

(provide 'test-frontends)

;;; test-frontends.el ends here
