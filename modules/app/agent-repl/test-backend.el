;;; test-backend.el --- Tests for backend.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the pluggable agent-CLI backend registry: struct
;; construction, register/get roundtrip, validation on registration,
;; per-workspace backend selection, and the built-in claude backend
;; (including that its start-cmd builder reproduces the historical
;; claude command byte-for-byte).

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defun agent-repl-test--dummy-backend (&optional name)
  "Return a minimal valid backend struct named NAME (default `dummy')."
  (agent-repl-backend-create
   :name (or name 'dummy)
   :binary "dummy-bin"
   :start-cmd-fn (lambda (_opts) "dummy-bin --go")))

(defmacro agent-repl-test--with-backends (&rest body)
  "Run BODY with a private, restored copy of the backend registry.
Prevents test registrations from leaking into the module-level
`agent-repl--backends' (which already holds the real claude backend)."
  (declare (indent 0))
  `(let ((agent-repl--backends (copy-hash-table agent-repl--backends)))
     ,@body))

;;;; ---- Tests: struct ----

(ert-deftest agent-repl-test-backend-struct-accessors ()
  "The backend struct exposes name, binary and start-cmd-fn."
  (let ((b (agent-repl-test--dummy-backend 'x)))
    (should (eq (agent-repl-backend-name b) 'x))
    (should (equal (agent-repl-backend-binary b) "dummy-bin"))
    (should (functionp (agent-repl-backend-start-cmd-fn b)))))

(ert-deftest agent-repl-test-backend-predicate ()
  "`agent-repl-backend-p' recognizes a backend struct and rejects others."
  (should (agent-repl-backend-p (agent-repl-test--dummy-backend)))
  (should-not (agent-repl-backend-p "not-a-backend"))
  (should-not (agent-repl-backend-p nil)))

;;;; ---- Tests: register / get ----

(ert-deftest agent-repl-test-backend-register-and-get-roundtrip ()
  "A registered backend is returned by `agent-repl-backend-get'."
  (agent-repl-test--with-backends
    (let ((b (agent-repl-test--dummy-backend 'roundtrip)))
      (agent-repl-register-backend b)
      (should (eq (agent-repl-backend-get 'roundtrip) b)))))

(ert-deftest agent-repl-test-backend-register-replaces-same-name ()
  "Re-registering the same name replaces the prior backend."
  (agent-repl-test--with-backends
    (let ((b1 (agent-repl-test--dummy-backend 'dup))
          (b2 (agent-repl-test--dummy-backend 'dup)))
      (agent-repl-register-backend b1)
      (agent-repl-register-backend b2)
      (should (eq (agent-repl-backend-get 'dup) b2)))))

(ert-deftest agent-repl-test-backend-register-rejects-non-struct ()
  "Registering a non-struct value signals an error."
  (agent-repl-test--with-backends
    (should-error (agent-repl-register-backend "nope"))))

(ert-deftest agent-repl-test-backend-register-rejects-missing-binary ()
  "Registering a backend with a nil required slot signals an error."
  (agent-repl-test--with-backends
    (should-error
     (agent-repl-register-backend
      (agent-repl-backend-create :name 'broken :binary nil
                                 :start-cmd-fn #'ignore)))))

(ert-deftest agent-repl-test-backend-get-unknown-errors ()
  "Fetching an unregistered backend signals an error (no silent fallback)."
  (agent-repl-test--with-backends
    (should-error (agent-repl-backend-get 'does-not-exist))))

;;;; ---- Tests: per-workspace selection ----

(ert-deftest agent-repl-test-backend-ws-name-defaults ()
  "A workspace with no `:backend' resolves to `agent-repl-default-backend'."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-backend 'claude))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (should (eq (agent-repl--ws-backend-name "ws1") 'claude)))))

(ert-deftest agent-repl-test-backend-ws-name-override ()
  "A workspace's `:backend' property overrides the default."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-backend 'claude))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (agent-repl--ws-put "ws1" :backend 'codex)
      (should (eq (agent-repl--ws-backend-name "ws1") 'codex)))))

(ert-deftest agent-repl-test-backend-ws-resolves-struct ()
  "`agent-repl--ws-backend' returns the registered struct for the ws."
  (agent-repl-test--with-backends
    (agent-repl-test--with-clean-state
      (let ((b (agent-repl-test--dummy-backend 'wsb)))
        (agent-repl-register-backend b)
        (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
        (agent-repl--ws-put "ws1" :backend 'wsb)
        (should (eq (agent-repl--ws-backend "ws1") b))))))

;;;; ---- Tests: built-in claude backend ----

(ert-deftest agent-repl-test-backend-claude-registered ()
  "The claude backend is registered at load time with binary `claude'."
  (let ((b (agent-repl-backend-get 'claude)))
    (should (eq (agent-repl-backend-name b) 'claude))
    (should (equal (agent-repl-backend-binary b) "claude"))))

(ert-deftest agent-repl-test-backend-claude-start-cmd-plain ()
  "The claude backend builds a plain `claude' command (no config dir)."
  (let ((agent-repl-interactive-model nil)
        (agent-repl-system-prompt nil))
    (let ((cmd (agent-repl--claude-start-cmd
                (list :session-id nil :fork-session-id nil
                      :project-dir "/home/user/personal-proj" :model nil))))
      (should (string-prefix-p "claude " cmd))
      (should (string-match-p "--dangerously-skip-permissions" cmd))
      (should-not (string-match-p "CLAUDE_CONFIG_DIR" cmd)))))

(ert-deftest agent-repl-test-backend-claude-start-cmd-continue ()
  "A prior session id yields `--continue' in the claude command."
  (let ((agent-repl-interactive-model nil)
        (agent-repl-system-prompt nil))
    (let ((cmd (agent-repl--claude-start-cmd
                (list :session-id "sess-1" :fork-session-id nil
                      :project-dir "/home/user/personal-proj" :model nil))))
      (should (string-match-p "--continue" cmd)))))

(ert-deftest agent-repl-test-backend-claude-start-cmd-matches-legacy-path ()
  "The backend command equals the direct helper composition byte-for-byte.
Guards the delegation refactor: `agent-repl--claude-start-cmd' must
produce exactly what the old inline perm-flag/config-dir/flags/assemble
chain produced."
  (let* ((agent-repl-interactive-model "opus")
         (agent-repl-system-prompt nil)
         (opts (list :session-id "s" :fork-session-id nil
                     :project-dir "/home/user/personal-proj" :model nil))
         (perm  (agent-repl--compute-perm-flag nil "/home/user/personal-proj"))
         (cfg   (agent-repl--compute-config-dir "/home/user/personal-proj"))
         (flags (agent-repl--compute-claude-flags "s" nil perm nil))
         (legacy (agent-repl--assemble-cmd flags cfg)))
    (should (equal (agent-repl--claude-start-cmd opts) legacy))))

;;;; ---- Tests: build-start-cmd delegation ----

(ert-deftest agent-repl-test-backend-build-start-cmd-uses-ws-backend ()
  "`agent-repl--build-start-cmd' routes cmd assembly through the ws backend."
  (agent-repl-test--with-backends
    (agent-repl-test--with-clean-state
      (agent-repl-register-backend
       (agent-repl-backend-create
        :name 'sentinel-backend :binary "sb"
        :start-cmd-fn (lambda (_opts) "SENTINEL-CMD")))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (agent-repl--ws-put "ws1" :backend 'sentinel-backend)
      (agent-repl--ws-put "ws1" :active-env :bare-metal)
      (agent-repl--ws-put "ws1" :bare-metal
                           (make-agent-repl-instantiation :session-id nil))
      (let ((result (agent-repl--build-start-cmd "ws1")))
        (should (equal (plist-get result :cmd) "SENTINEL-CMD"))))))

(ert-deftest agent-repl-test-backend-build-start-cmd-passes-opts ()
  "The ws's session-id / fork / project-dir / model reach the backend fn."
  (agent-repl-test--with-backends
    (agent-repl-test--with-clean-state
      (let (captured)
        (agent-repl-register-backend
         (agent-repl-backend-create
          :name 'capture-backend :binary "cb"
          :start-cmd-fn (lambda (opts) (setq captured opts) "X")))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/proj")
        (agent-repl--ws-put "ws1" :backend 'capture-backend)
        (agent-repl--ws-put "ws1" :fork-session-id "fork-9")
        (agent-repl--ws-put "ws1" :model "sonnet")
        (agent-repl--ws-put "ws1" :active-env :bare-metal)
        (agent-repl--ws-put "ws1" :bare-metal
                             (make-agent-repl-instantiation :session-id nil))
        (agent-repl--build-start-cmd "ws1")
        (should (equal (plist-get captured :project-dir) "/tmp/proj"))
        (should (equal (plist-get captured :fork-session-id) "fork-9"))
        (should (equal (plist-get captured :model) "sonnet"))))))

(provide 'test-backend)
;;; test-backend.el ends here
