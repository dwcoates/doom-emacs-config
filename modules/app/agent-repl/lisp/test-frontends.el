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
  "Return a fully-populated test frontend NAME with optional OVERRIDES.
Supports the sole surviving environment (`:bare-metal') by default.  A
frontend that must NOT be able to run it overrides `:supported-envs' with
`(:containerized)' — the stand-in for the containerized environment the
env axis remains the seam for, and the only way left to exercise an
env-axis REJECTION now that every registered frontend runs `:bare-metal'."
  (apply #'agent-repl-frontend-create
         (append overrides
                 (list :name name
                       :open-fn #'ignore
                       :boot-fn #'ignore
                       :kill-fn #'ignore
                       :running-p-fn #'ignore
                       :show-fn #'ignore
                       :hide-fn #'ignore
                       :supported-backends '(claude)
                       :supported-envs '(:bare-metal)))))

(defmacro agent-repl-test--with-frontend-registry (&rest body)
  "Run BODY against a scratch copy of the frontend registry.

Also scratch-binds `agent-repl-default-frontend', because the default and
the registry it indexes into must have the SAME lifetime: a BODY that
rebinds the default to a BODY-local scratch frontend must not leave that
name in the global once BODY's registry copy is gone — and every later
test whose workspace carries no `:frontend' resolves through that
global."
  `(let ((agent-repl--frontends (copy-hash-table agent-repl--frontends))
         (agent-repl-default-frontend agent-repl-default-frontend))
     ,@body))

;;;; ---- Registration ----------------------------------------------------------

(ert-deftest agent-repl-test-frontends-builtin-gui-registered ()
  "Module load registers the gui frontend."
  ;; Assert
  (should (agent-repl-frontend-get 'gui)))

(ert-deftest agent-repl-test-frontends-process-wide-get-uses-central-scope ()
  "A frontend registry lookup without a workspace is explicitly central."
  ;; Arrange
  (let (scope)
    (cl-letf (((symbol-function 'agent-repl--log-verbose)
               (lambda (ws _fmt &rest _args) (setq scope ws))))
      ;; Act
      (agent-repl-frontend-get 'gui)
      ;; Assert
      (should (equal scope
                     '(:agent-repl-central
                       "frontend registry lookup has no workspace subject"))))))

(ert-deftest agent-repl-test-frontends-vterm-not-registered ()
  "vterm is no longer a registered frontend — the gui is the only one.
The vterm frontend was deleted from production; a name lookup for it
must signal rather than resolve to anything."
  ;; Act / Assert
  (should-error (agent-repl-frontend-get 'vterm)))

(ert-deftest agent-repl-test-frontends-register-rejects-non-struct ()
  "Registering a non-struct signals."
  ;; Act / Assert
  (should-error (agent-repl-register-frontend "not-a-frontend")))

(ert-deftest agent-repl-test-frontends-register-rejects-missing-slot ()
  "Registering a frontend without a required capability signals."
  ;; Arrange — no :running-p-fn.
  (agent-repl-test--with-frontend-registry
   (should-error
    (agent-repl-register-frontend
     (agent-repl-frontend-create
      :name 'broken
      :open-fn #'ignore
      :kill-fn #'ignore
      :supported-backends '(claude))))))

(ert-deftest agent-repl-test-frontends-register-rejects-missing-boot-fn ()
  "Registering a frontend without a boot capability signals.
Every birth and restore path boots through it, so a frontend lacking one
would detonate at the next workspace creation, not here."
  ;; Arrange — every slot but :boot-fn.
  (agent-repl-test--with-frontend-registry
   (should-error
    (agent-repl-register-frontend
     (agent-repl-frontend-create
      :name 'broken
      :open-fn #'ignore
      :kill-fn #'ignore
      :running-p-fn #'ignore
      :supported-backends '(claude)
      :supported-envs '(:bare-metal))))))

(ert-deftest agent-repl-test-frontends-register-rejects-missing-supported-envs ()
  "Registering a frontend that declares no supported environments signals."
  ;; Arrange — every slot but :supported-envs.
  (agent-repl-test--with-frontend-registry
   (should-error
    (agent-repl-register-frontend
     (agent-repl-frontend-create
      :name 'broken
      :open-fn #'ignore
      :boot-fn #'ignore
      :kill-fn #'ignore
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
    (agent-repl-test--with-frontend-registry
     (agent-repl-register-frontend (agent-repl-test--make-frontend 'scratch))
     (agent-repl--ws-put "ws1" :frontend 'gui)
     (let ((agent-repl-default-frontend 'scratch))
       ;; Act / Assert
       (should (eq (agent-repl--ws-frontend-name "ws1") 'gui))
       (should (agent-repl--ws-gui-frontend-p "ws1"))))))

(ert-deftest agent-repl-test-frontends-ws-resolution-defaults ()
  "A workspace without :frontend resolves to the default."
  ;; Arrange — a scratch frontend stands in for "some other frontend",
  ;; since gui is the only one actually registered today.
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (agent-repl-register-frontend (agent-repl-test--make-frontend 'scratch))
     (let ((agent-repl-default-frontend 'scratch))
       ;; Act / Assert
       (should (eq (agent-repl--ws-frontend-name "ws1") 'scratch))
       (should-not (agent-repl--ws-gui-frontend-p "ws1"))))))

(ert-deftest agent-repl-test-frontends-shipped-default-is-gui ()
  "The frontend shipped as the default is the web gui, not the vterm TUI.
Reads the defcustom's STANDARD value, so a `setq' anywhere in load order
cannot make this pass by accident."
  ;; Act
  (let ((shipped (eval (car (get 'agent-repl-default-frontend 'standard-value)) t)))
    ;; Assert
    (should (eq shipped 'gui))))

(ert-deftest agent-repl-test-frontends-ws-resolution-defaults-to-gui-out-of-the-box ()
  "Under the shipped default a workspace with no :frontend is a gui workspace."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-frontend
           (eval (car (get 'agent-repl-default-frontend 'standard-value)) t)))
      ;; Act / Assert
      (should (agent-repl--ws-gui-frontend-p "ws1")))))

;;;; ---- Capability-constrained default ----------------------------------------------

(ert-deftest agent-repl-test-frontends-default-for-ws-uses-the-default ()
  "A claude/bare-metal workspace resolves to the default frontend itself."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :active-env :bare-metal)
    ;; Act / Assert
    (should (eq (agent-repl--frontend-default-for-ws "ws1") 'gui))))

(ert-deftest agent-repl-test-frontends-default-for-ws-unhydrated-env-unconstrained ()
  "Before the env is hydrated (nil :active-env) the env axis constrains nothing."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act / Assert — no :active-env on the plist at all.
    (should (eq (agent-repl--frontend-default-for-ws "ws1") 'gui))))

(ert-deftest agent-repl-test-frontends-default-for-ws-env-rules-out-the-default ()
  "An environment the default frontend cannot run resolves to one that can.
The default is capability-constrained on `:active-env' exactly as it is on
the backend; both shipped frontends run the sole surviving environment, so
the rejection is staged with a frontend that declares it cannot."
  ;; Arrange — the default runs only a containerized env; the ws is bare-metal.
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (let ((agent-repl--frontends (make-hash-table :test #'eq)))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend 'bare-metal-capable))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend 'containerized-only
                                        :supported-envs '(:containerized)))
       (setq agent-repl-default-frontend 'containerized-only)
       (agent-repl--ws-put "ws1" :active-env :bare-metal)
       ;; Act / Assert
       (should (eq (agent-repl--frontend-default-for-ws "ws1")
                   'bare-metal-capable))))))

(ert-deftest agent-repl-test-frontends-ws-resolution-explicit-beats-the-constraint ()
  "An explicit `:frontend' is honored even where the constrained default differs."
  ;; Arrange — a codex workspace would otherwise resolve to vterm.
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :backend 'codex)
    (agent-repl--ws-put "ws1" :frontend 'gui)
    ;; Act / Assert — resolution reports the choice; validation is what refuses it.
    (should (eq (agent-repl--ws-frontend-name "ws1") 'gui))))

;;;; ---- Deliberate choice -----------------------------------------------------------

(ert-deftest agent-repl-test-frontends-choose-marks-the-choice-explicit ()
  "Choosing a frontend records it AND marks it as deliberate."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act
    (agent-repl--ws-choose-frontend "ws1" 'placeholder)
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :frontend) 'placeholder))
    (should (agent-repl--ws-get "ws1" :frontend-explicit))))

(ert-deftest agent-repl-test-frontends-validate-pair-accepts-supported ()
  "A supported frontend/backend pair validates.
A scratch frontend stands in for a second example beyond gui+claude,
since gui is the only frontend actually registered today."
  ;; Arrange
  (agent-repl-test--with-frontend-registry
   (agent-repl-register-frontend
    (agent-repl-test--make-frontend 'scratch :supported-backends '(codex)))
   ;; Act / Assert
   (should (agent-repl--frontend-validate-pair 'gui 'claude))
   (should (agent-repl--frontend-validate-pair 'scratch 'codex))))

(ert-deftest agent-repl-test-frontends-validate-pair-rejects-unsupported ()
  "gui+codex fails loudly until a codex shim exists."
  ;; Act / Assert
  (should-error (agent-repl--frontend-validate-pair 'gui 'codex)
                :type 'user-error))

(ert-deftest agent-repl-test-frontends-validate-pair-accepts-supported-env ()
  "A frontend that supports the environment validates against it.
The gui runs the sole surviving environment, so the env axis rules
nothing out today — but it is still checked, on both this and a
second (scratch) example."
  ;; Arrange
  (agent-repl-test--with-frontend-registry
   (agent-repl-register-frontend
    (agent-repl-test--make-frontend 'scratch :supported-backends '(codex)))
   ;; Act / Assert
   (should (agent-repl--frontend-validate-pair 'gui 'claude :bare-metal))
   (should (agent-repl--frontend-validate-pair 'scratch 'codex :bare-metal))))

(ert-deftest agent-repl-test-frontends-validate-pair-rejects-unsupported-env ()
  "An environment the frontend cannot run fails loudly, even on a valid backend.
Staged with a containerized-only frontend: the env axis is what a
containerized environment would be gated on, and it must still bite."
  ;; Arrange
  (agent-repl-test--with-frontend-registry
   (agent-repl-register-frontend
    (agent-repl-test--make-frontend 'containerized-only
                                    :supported-envs '(:containerized)))
   ;; Act / Assert — the backend axis passes; the env axis is what refuses.
   (should-error (agent-repl--frontend-validate-pair
                  'containerized-only 'claude :bare-metal)
                 :type 'user-error)))

(ert-deftest agent-repl-test-frontends-validate-pair-nil-env-skips-the-env-axis ()
  "A nil ENV validates the backend axis only."
  ;; Act / Assert
  (should (agent-repl--frontend-validate-pair 'gui 'claude nil)))

(ert-deftest agent-repl-test-frontends-validate-for-ws-reads-the-ws-env ()
  "The ws-shaped validator reads the workspace's env and refuses on it.
The backend axis alone would have let this pair through, so a validator
that forgot to pass `:active-env' would silently accept it."
  ;; Arrange — a bare-metal workspace, and a frontend that cannot run one.
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (agent-repl-register-frontend
      (agent-repl-test--make-frontend 'containerized-only
                                      :supported-envs '(:containerized)))
     (agent-repl--ws-put "ws1" :active-env :bare-metal)
     ;; Act / Assert
     (should-error (agent-repl--frontend-validate-for-ws 'containerized-only "ws1")
                   :type 'user-error))))

;;;; ---- Dispatch ---------------------------------------------------------------------

(ert-deftest agent-repl-test-frontends-dispatch-cancel-detached-routes-by-ws ()
  "Detached-agent cancel dispatch reaches the workspace's own capability."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (let ((got nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'probe :cancel-detached-fn (lambda (ws) (setq got ws) t)))
       (agent-repl--ws-put "ws1" :frontend 'probe)
       ;; Act / Assert
       (should (agent-repl--frontend-dispatch-cancel-detached "ws1"))
       (should (equal got "ws1"))))))

(ert-deftest agent-repl-test-frontends-dispatch-cancel-detached-without-capability-is-nil ()
  "A frontend with NO cancel capability answers nil rather than falling back."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (agent-repl-register-frontend (agent-repl-test--make-frontend 'probe))
     (agent-repl--ws-put "ws1" :frontend 'probe)
     ;; Act / Assert
     (should-not (agent-repl--frontend-dispatch-cancel-detached "ws1")))))

(ert-deftest agent-repl-test-frontends-register-accepts-a-frontend-without-cancel-detached ()
  "The cancel capability is OPTIONAL: registration does not require it.
It is a capability a frontend may not have, unlike the interrupt every
frontend must implement."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     ;; Act
     (agent-repl-register-frontend (agent-repl-test--make-frontend 'probe))
     ;; Assert
     (should (agent-repl-frontend-get 'probe)))))

(ert-deftest agent-repl-test-frontends-dispatch-show-routes-by-ws ()
  "Show dispatch reaches the workspace's frontend capability."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (let ((got nil))
       (agent-repl-register-frontend
        (agent-repl-test--make-frontend
         'probe :show-fn (lambda (ws) (setq got ws))))
       (agent-repl--ws-put "ws1" :frontend 'probe)
       ;; Act
       (agent-repl--frontend-dispatch-show "ws1")
       ;; Assert
       (should (equal got "ws1"))))))

;;;; ---- Headless boot ---------------------------------------------------------------

(ert-deftest agent-repl-test-frontends-dispatch-show-clears-explicit-close ()
  "Show dispatch clears the explicit-close preference so panels default
open again on a later switch."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-frontend-registry
     (agent-repl-register-frontend
      (agent-repl-test--make-frontend 'probe :show-fn #'ignore))
     (agent-repl--ws-put "ws1" :frontend 'probe)
     (agent-repl--ws-put "ws1" :panels-closed-by-user t)
     (agent-repl--frontend-dispatch-show "ws1")
     (should-not (agent-repl--ws-get "ws1" :panels-closed-by-user)))))
