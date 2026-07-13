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
  "The claude backend builds a plain `claude' command (no config dir).
The permission flag is bound explicitly so the assertion tracks the
personal-project ROUTING rather than the defcustom's current default
\(which changed once already: --dangerously-skip-permissions ->
--permission-mode auto)."
  (let ((agent-repl-interactive-model nil)
        (agent-repl-system-prompt nil)
        (agent-repl-personal-permission-flag "--personal-sentinel-flag"))
    (let ((cmd (agent-repl--claude-start-cmd
                (list :session-id nil :fork-session-id nil
                      :project-dir "/home/user/personal-proj" :model nil))))
      ;; The ownership env prefix leads every module-launched CLI (it is
      ;; what the hooks' sentinel marker is derived from); the invocation
      ;; itself is plain `claude'.
      (should (string-prefix-p "AGENT_REPL_OWNED=1 claude " cmd))
      (should (string-match-p "--personal-sentinel-flag" cmd))
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
         (perm  (agent-repl--compute-perm-flag "/home/user/personal-proj"))
         (cfg   (agent-repl--compute-config-dir "/home/user/personal-proj"))
         (flags (agent-repl--compute-claude-flags "s" nil perm nil))
         (legacy (agent-repl--assemble-cmd flags cfg)))
    (should (equal (agent-repl--claude-start-cmd opts) legacy))))

;;;; ---- Tests: headless command construction ----

(ert-deftest agent-repl-test-backend-claude-headless-cmd-shape ()
  "The claude headless builder prefixes `claude -p --model MODEL'."
  (should (equal (agent-repl--claude-headless-cmd "haiku" nil)
                 '("claude" "-p" "--model" "haiku"))))

(ert-deftest agent-repl-test-backend-claude-headless-cmd-appends-extra ()
  "Extra-args are appended after the standard claude headless prefix."
  (should (equal (agent-repl--claude-headless-cmd "opus" '("--foo" "bar"))
                 '("claude" "-p" "--model" "opus" "--foo" "bar"))))

(ert-deftest agent-repl-test-backend-headless-cmd-delegates ()
  "`agent-repl--backend-headless-cmd' delegates to the backend's fn."
  (agent-repl-test--with-backends
    (agent-repl-register-backend
     (agent-repl-backend-create
      :name 'hb :binary "hb"
      :start-cmd-fn #'ignore
      :headless-cmd-fn (lambda (model extra) (cons "hb-run" (cons model extra)))))
    (should (equal (agent-repl--backend-headless-cmd
                    (agent-repl-backend-get 'hb) "m" '("x"))
                   '("hb-run" "m" "x")))))

(ert-deftest agent-repl-test-backend-headless-cmd-errors-when-absent ()
  "Asking a headless-less backend for a headless command signals an error."
  (agent-repl-test--with-backends
    (agent-repl-register-backend
     (agent-repl-backend-create :name 'nohl :binary "nohl"
                                :start-cmd-fn #'ignore))
    (should-error (agent-repl--backend-headless-cmd
                   (agent-repl-backend-get 'nohl) "m" nil))))

(ert-deftest agent-repl-test-backend-claude-registered-with-headless ()
  "The built-in claude backend carries a headless-cmd-fn."
  (should (agent-repl-backend-headless-cmd-fn (agent-repl-backend-get 'claude))))

;;;; ---- Tests: select-backend command ----

(ert-deftest agent-repl-test-backend-select-sets-ws-backend-and-persists ()
  "The selector writes the choice to the current ws and state-saves it."
  (agent-repl-test--with-clean-state
    (let (saved)
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "codex"))
                ((symbol-function 'agent-repl--ws-current-name)
                 (lambda () "ws1"))
                ((symbol-function 'agent-repl--agent-running-p)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq saved ws))))
        (agent-repl-select-backend nil)
        (should (eq (agent-repl--ws-get "ws1" :backend) 'codex))
        (should (equal saved "ws1"))))))

(ert-deftest agent-repl-test-backend-select-refuses-running-agent ()
  "The selector refuses to change a workspace with a running agent."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "codex"))
              ((symbol-function 'agent-repl--ws-current-name)
               (lambda () "ws1"))
              ((symbol-function 'agent-repl--agent-running-p)
               (lambda (_ws) t)))
      (should-error (agent-repl-select-backend nil) :type 'user-error))))

(ert-deftest agent-repl-test-backend-select-no-current-ws-errors ()
  "The selector signals user-error when no current workspace resolves."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "codex"))
            ((symbol-function 'agent-repl--ws-current-name)
             (lambda () nil)))
    (should-error (agent-repl-select-backend nil) :type 'user-error)))

(ert-deftest agent-repl-test-backend-select-prefix-sets-default ()
  "With a prefix argument the selector sets `agent-repl-default-backend'."
  (let ((agent-repl-default-backend 'claude))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "codex")))
      (agent-repl-select-backend t)
      (should (eq agent-repl-default-backend 'codex)))))

(ert-deftest agent-repl-test-backend-select-switch-resets-session-ids ()
  "An actual backend CHANGE clears env session ids and the fork pointer.
Session ids are scoped to the CLI that minted them: leaving a claude
UUID in place would make the next start run `codex resume <claude-id>'."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-backend 'claude))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (agent-repl--ws-put "ws1" :fork-session-id "fork-1")
      (agent-repl--ws-put "ws1" :bare-metal
                           (make-agent-repl-instantiation :session-id "claude-sid"))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "codex"))
                ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--state-save) #'ignore))
        (agent-repl-select-backend nil)
        (should-not (agent-repl-instantiation-session-id
                     (agent-repl--ws-get "ws1" :bare-metal)))
        (should-not (agent-repl--ws-get "ws1" :fork-session-id))))))

(ert-deftest agent-repl-test-backend-select-same-backend-keeps-session-ids ()
  "Re-selecting the CURRENT backend preserves session continuity."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-backend 'claude))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (agent-repl--ws-put "ws1" :bare-metal
                           (make-agent-repl-instantiation :session-id "claude-sid"))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "claude"))
                ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--state-save) #'ignore))
        (agent-repl-select-backend nil)
        (should (equal (agent-repl-instantiation-session-id
                        (agent-repl--ws-get "ws1" :bare-metal))
                       "claude-sid"))))))

;;;; ---- Tests: backend session-id stash (switch round-trip) ----

(ert-deftest agent-repl-test-backend-capture-session-ids-collects-envs-and-fork ()
  "capture-backend-session-ids snapshots each env's sid plus the fork pointer."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :bare-metal
                         (make-agent-repl-instantiation :session-id "bm"))
    (agent-repl--ws-put "ws1" :fork-session-id "fk")
    (let ((snap (agent-repl--capture-backend-session-ids "ws1")))
      (should (equal (plist-get snap :bare-metal) "bm"))
      (should (equal (plist-get snap :fork-session-id) "fk")))))

(ert-deftest agent-repl-test-backend-capture-session-ids-nil-for-missing-struct ()
  "capture-backend-session-ids records nil for an env lacking a struct."
  (agent-repl-test--with-clean-state
    ;; Arrange — the ws carries the fork pointer but no instantiation struct.
    (agent-repl--ws-put "ws1" :fork-session-id "fk")
    (let ((snap (agent-repl--capture-backend-session-ids "ws1")))
      (should (null (plist-get snap :bare-metal)))
      (should (equal (plist-get snap :fork-session-id) "fk")))))

(ert-deftest agent-repl-test-backend-apply-session-ids-restores-envs-and-fork ()
  "apply-backend-session-ids writes the sids and fork back onto the ws."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :bare-metal (make-agent-repl-instantiation))
    (agent-repl--apply-backend-session-ids
     "ws1" '(:bare-metal "bm" :fork-session-id "fk"))
    (should (equal (agent-repl-instantiation-session-id
                    (agent-repl--ws-get "ws1" :bare-metal))
                   "bm"))
    (should (equal (agent-repl--ws-get "ws1" :fork-session-id) "fk"))))

(ert-deftest agent-repl-test-backend-apply-session-ids-nil-clears ()
  "apply-backend-session-ids with a nil plist clears every sid and the fork."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :bare-metal
                         (make-agent-repl-instantiation :session-id "bm"))
    (agent-repl--ws-put "ws1" :fork-session-id "fk")
    (agent-repl--apply-backend-session-ids "ws1" nil)
    (should-not (agent-repl-instantiation-session-id
                 (agent-repl--ws-get "ws1" :bare-metal)))
    (should-not (agent-repl--ws-get "ws1" :fork-session-id))))

(ert-deftest agent-repl-test-backend-session-ids-present-p-true-for-sid ()
  "session-ids-present-p is non-nil when an env carries a session id."
  (should (agent-repl--backend-session-ids-present-p '(:bare-metal "bm"))))

(ert-deftest agent-repl-test-backend-session-ids-present-p-true-for-fork ()
  "session-ids-present-p is non-nil when only the fork pointer is set."
  (should (agent-repl--backend-session-ids-present-p '(:fork-session-id "fk"))))

(ert-deftest agent-repl-test-backend-session-ids-present-p-nil-when-empty ()
  "session-ids-present-p is nil when every slot is nil or the empty string."
  (should-not (agent-repl--backend-session-ids-present-p
               '(:bare-metal "" :fork-session-id nil))))

(ert-deftest agent-repl-test-backend-switch-stashes-outgoing-under-old ()
  "switch-backend-session-ids stashes the live sids under the OLD backend."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :bare-metal
                         (make-agent-repl-instantiation :session-id "claude-sid"))
    (agent-repl--ws-switch-backend-session-ids "ws1" 'claude 'codex)
    (let ((stash (agent-repl--ws-get "ws1" :backend-session-stash)))
      (should (equal (plist-get (plist-get stash 'claude) :bare-metal)
                     "claude-sid")))))

(ert-deftest agent-repl-test-backend-switch-to-fresh-clears-and-returns-nil ()
  "Switching to a never-used backend clears the live sids and returns nil."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :bare-metal
                         (make-agent-repl-instantiation :session-id "claude-sid"))
    (let ((restored (agent-repl--ws-switch-backend-session-ids
                     "ws1" 'claude 'codex)))
      (should-not restored)
      (should-not (agent-repl-instantiation-session-id
                   (agent-repl--ws-get "ws1" :bare-metal))))))

(ert-deftest agent-repl-test-backend-switch-back-restores-and-returns-t ()
  "Switching AWAY then BACK restores the prior backend's sids (returns t)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :bare-metal
                         (make-agent-repl-instantiation :session-id "claude-sid"))
    (agent-repl--ws-switch-backend-session-ids "ws1" 'claude 'codex)
    (let ((restored (agent-repl--ws-switch-backend-session-ids
                     "ws1" 'codex 'claude)))
      (should restored)
      (should (equal (agent-repl-instantiation-session-id
                      (agent-repl--ws-get "ws1" :bare-metal))
                     "claude-sid")))))

(ert-deftest agent-repl-test-backend-select-switch-back-restores-session-ids ()
  "select-backend claude->codex->claude restores claude's env session ids.
This is the bug fix: after switching back to a prior backend, the next
start must resume (`--continue') rather than start fresh."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-backend 'claude)
          (target "codex"))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (agent-repl--ws-put "ws1" :bare-metal
                           (make-agent-repl-instantiation :session-id "claude-sid"))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) target))
                ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--state-save) #'ignore))
        (setq target "codex")
        (agent-repl-select-backend nil)
        (should-not (agent-repl-instantiation-session-id
                     (agent-repl--ws-get "ws1" :bare-metal)))
        (setq target "claude")
        (agent-repl-select-backend nil)
        (should (equal (agent-repl-instantiation-session-id
                        (agent-repl--ws-get "ws1" :bare-metal))
                       "claude-sid"))))))

(ert-deftest agent-repl-test-backend-select-switch-back-restores-fork ()
  "select-backend claude->codex->claude restores claude's fork pointer."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-default-backend 'claude)
          (target "codex"))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (agent-repl--ws-put "ws1" :bare-metal (make-agent-repl-instantiation))
      (agent-repl--ws-put "ws1" :fork-session-id "fork-1")
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) target))
                ((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--state-save) #'ignore))
        (setq target "codex")
        (agent-repl-select-backend nil)
        (should-not (agent-repl--ws-get "ws1" :fork-session-id))
        (setq target "claude")
        (agent-repl-select-backend nil)
        (should (equal (agent-repl--ws-get "ws1" :fork-session-id) "fork-1"))))))

;;;; ---- Tests: transcript seam ----

(defun agent-repl-test--transcript-backend (&rest overrides)
  "Register a `tr' backend whose transcript slots come from OVERRIDES.
OVERRIDES is a plist of slot keywords accepted by
`agent-repl-backend-create'."
  (agent-repl-register-backend
   (apply #'agent-repl-backend-create
          :name 'tr :binary "tr" :start-cmd-fn #'ignore overrides)))

(ert-deftest agent-repl-test-backend-ws-transcript-path-delegates ()
  "`agent-repl--ws-transcript-path' calls the backend's path fn with WS."
  (agent-repl-test--with-backends
    (agent-repl-test--with-clean-state
      (agent-repl-test--transcript-backend
       :transcript-path-fn (lambda (ws) (format "/x/%s.jsonl" ws)))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (agent-repl--ws-put "ws1" :backend 'tr)
      (should (equal (agent-repl--ws-transcript-path "ws1") "/x/ws1.jsonl")))))

(ert-deftest agent-repl-test-backend-ws-transcript-path-nil-slot ()
  "A backend without transcript support yields a nil transcript path."
  (agent-repl-test--with-backends
    (agent-repl-test--with-clean-state
      (agent-repl-test--transcript-backend)
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (agent-repl--ws-put "ws1" :backend 'tr)
      (should-not (agent-repl--ws-transcript-path "ws1")))))

(ert-deftest agent-repl-test-backend-transcript-cached-nil-reader ()
  "`agent-repl--transcript-cached' returns nil when the reader slot is nil."
  (agent-repl-test--with-backends
    (agent-repl-test--with-clean-state
      (agent-repl-test--transcript-backend
       :transcript-path-fn (lambda (_ws) "/x/t.jsonl"))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (agent-repl--ws-put "ws1" :backend 'tr)
      (cl-letf (((symbol-function 'agent-repl--ai-title-mtime)
                 (lambda (_p) 1.0)))
        (should-not (agent-repl--transcript-cached
                     "ws1" :tr-cache
                     #'agent-repl-backend-transcript-title-fn))))))

(ert-deftest agent-repl-test-backend-transcript-cached-caches-by-mtime ()
  "A second lookup with unchanged (path, mtime) does not re-invoke the reader."
  (agent-repl-test--with-backends
    (agent-repl-test--with-clean-state
      (let ((reads 0))
        (agent-repl-test--transcript-backend
         :transcript-path-fn (lambda (_ws) "/x/t.jsonl")
         :transcript-title-fn (lambda (_p) (cl-incf reads) "T"))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
        (agent-repl--ws-put "ws1" :backend 'tr)
        (cl-letf (((symbol-function 'agent-repl--ai-title-mtime)
                   (lambda (_p) 1.0)))
          (should (equal (agent-repl--transcript-cached
                          "ws1" :tr-cache
                          #'agent-repl-backend-transcript-title-fn)
                         "T"))
          (should (equal (agent-repl--transcript-cached
                          "ws1" :tr-cache
                          #'agent-repl-backend-transcript-title-fn)
                         "T"))
          (should (= reads 1)))))))

(ert-deftest agent-repl-test-backend-transcript-cached-rereads-on-mtime-change ()
  "An advanced mtime invalidates the cache and re-invokes the reader."
  (agent-repl-test--with-backends
    (agent-repl-test--with-clean-state
      (let ((reads 0) (mtime 1.0))
        (agent-repl-test--transcript-backend
         :transcript-path-fn (lambda (_ws) "/x/t.jsonl")
         :transcript-title-fn (lambda (_p) (cl-incf reads) (format "T%d" reads)))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
        (agent-repl--ws-put "ws1" :backend 'tr)
        (cl-letf (((symbol-function 'agent-repl--ai-title-mtime)
                   (lambda (_p) mtime)))
          (should (equal (agent-repl--transcript-cached
                          "ws1" :tr-cache
                          #'agent-repl-backend-transcript-title-fn)
                         "T1"))
          (setq mtime 2.0)
          (should (equal (agent-repl--transcript-cached
                          "ws1" :tr-cache
                          #'agent-repl-backend-transcript-title-fn)
                         "T2"))
          (should (= reads 2)))))))

(ert-deftest agent-repl-test-backend-transcript-read-tail-reads-tail-only ()
  "The tail reader returns only the trailing SCAN-BYTES of the file."
  (let ((path (make-temp-file "agent-tail-")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "HEAD-PART|TAIL-PART"))
          (should (equal (agent-repl--transcript-read-tail path 9)
                         "TAIL-PART")))
      (delete-file path))))

(ert-deftest agent-repl-test-backend-transcript-read-tail-whole-when-small ()
  "A file smaller than SCAN-BYTES is returned whole."
  (let ((path (make-temp-file "agent-tail-")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "tiny"))
          (should (equal (agent-repl--transcript-read-tail path 4096) "tiny")))
      (delete-file path))))

(ert-deftest agent-repl-test-backend-transcript-read-tail-missing-file ()
  "A missing file yields nil."
  (should-not (agent-repl--transcript-read-tail "/nonexistent/agent-tail" 64)))

(ert-deftest agent-repl-test-backend-transcript-read-tail-nil-path ()
  "A nil path yields nil."
  (should-not (agent-repl--transcript-read-tail nil 64)))

(ert-deftest agent-repl-test-backend-transcript-read-tail-empty-file ()
  "An empty file yields nil rather than an empty string."
  (let ((path (make-temp-file "agent-tail-")))
    (unwind-protect
        (should-not (agent-repl--transcript-read-tail path 64))
      (delete-file path))))

(ert-deftest agent-repl-test-backend-claude-transcript-slots ()
  "The claude backend wires all four transcript slots to the segment fns."
  (let ((b (agent-repl-backend-get 'claude)))
    (should (eq (agent-repl-backend-transcript-path-fn b)
                #'agent-repl--ai-title-jsonl-path))
    (should (eq (agent-repl-backend-transcript-title-fn b)
                #'agent-repl--ai-title-read-from-jsonl))
    (should (eq (agent-repl-backend-transcript-model-fn b)
                #'agent-repl--model-read-from-jsonl))
    (should (eq (agent-repl-backend-transcript-context-fn b)
                #'agent-repl--context-read-from-jsonl))))

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
