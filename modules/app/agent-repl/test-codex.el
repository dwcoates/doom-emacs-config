;;; test-codex.el --- Tests for codex.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the codex backend: interactive start-command assembly
;; (new/resume/fork, model + permission flags, CODEX_HOME prefix),
;; headless exec argv, rollout locating + caching, rollout parsing
;; (turn_context model, token_count usage), hooks.json installation,
;; doctor checks, and the backend registration itself.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defconst agent-repl-test--codex-uuid "019d44f0-12bd-7a11-9061-4233b50d0e9e"
  "A realistic codex session UUID for fixtures.")

(defmacro agent-repl-test--with-codex-defaults (&rest body)
  "Run BODY with codex defcustoms pinned to their defaults."
  (declare (indent 0))
  `(let ((agent-repl-codex-home nil)
         (agent-repl-codex-interactive-model nil)
         (agent-repl-codex-managed-permission-flags
          "--ask-for-approval on-request --sandbox workspace-write")
         (agent-repl-codex-personal-permission-flags
          "--ask-for-approval on-request --sandbox workspace-write"))
     ,@body))

;;;; ---- Tests: start-cmd — subcommand selection ----

(ert-deftest agent-repl-test-codex-start-cmd-fresh-session ()
  "No session ids yields a bare `codex' launch."
  (agent-repl-test--with-codex-defaults
    (should (equal (agent-repl--codex-start-cmd
                    (list :session-id nil :fork-session-id nil
                          :project-dir "/home/u/personal" :model nil))
                   "codex --ask-for-approval on-request --sandbox workspace-write"))))

(ert-deftest agent-repl-test-codex-personal-default-is-not-dangerous ()
  "The personal codex flags never default to the danger bypass.
Pins the posture master set for claude (--permission-mode auto for
personal projects): nothing runs dangerously by default."
  (should-not (string-match-p "--dangerously-bypass-approvals-and-sandbox"
                              (default-value
                               'agent-repl-codex-personal-permission-flags))))

(ert-deftest agent-repl-test-codex-start-cmd-resume-by-id ()
  "A known session id resumes that exact session (id-explicit, no picker)."
  (agent-repl-test--with-codex-defaults
    (should (string-prefix-p
             (format "codex resume %s" agent-repl-test--codex-uuid)
             (agent-repl--codex-start-cmd
              (list :session-id agent-repl-test--codex-uuid
                    :fork-session-id nil
                    :project-dir "/home/u/personal" :model nil))))))

(ert-deftest agent-repl-test-codex-start-cmd-fork-beats-resume ()
  "A fork-session-id forks (new session) and takes precedence over resume."
  (agent-repl-test--with-codex-defaults
    (should (string-prefix-p
             (format "codex fork %s" agent-repl-test--codex-uuid)
             (agent-repl--codex-start-cmd
              (list :session-id "other-session"
                    :fork-session-id agent-repl-test--codex-uuid
                    :project-dir "/home/u/personal" :model nil))))))

;;;; ---- Tests: start-cmd — flags ----

(ert-deftest agent-repl-test-codex-start-cmd-model-flag ()
  "A per-workspace model rides as `--model' after the subcommand."
  (agent-repl-test--with-codex-defaults
    (should (string-match-p
             " --model gpt-5-codex\\b"
             (agent-repl--codex-start-cmd
              (list :session-id nil :fork-session-id nil
                    :project-dir "/home/u/personal" :model "gpt-5-codex"))))))

(ert-deftest agent-repl-test-codex-start-cmd-model-falls-back-to-defcustom ()
  "With no per-workspace model, `agent-repl-codex-interactive-model' applies."
  (agent-repl-test--with-codex-defaults
    (let ((agent-repl-codex-interactive-model "gpt-5.1-codex"))
      (should (string-match-p
               " --model gpt-5\\.1-codex\\b"
               (agent-repl--codex-start-cmd
                (list :session-id nil :fork-session-id nil
                      :project-dir "/home/u/personal" :model nil)))))))

(ert-deftest agent-repl-test-codex-start-cmd-no-model-flag-when-nil ()
  "No model anywhere yields no `--model' flag (codex default applies)."
  (agent-repl-test--with-codex-defaults
    (should-not (string-match-p
                 "--model"
                 (agent-repl--codex-start-cmd
                  (list :session-id nil :fork-session-id nil
                        :project-dir "/home/u/personal" :model nil))))))

(ert-deftest agent-repl-test-codex-start-cmd-managed-perm-flags ()
  "A managed (pattern-matching) project routes to the managed flags.
Sentinel-bound so the routing is pinned independently of the two
defcustoms' (currently identical) default values."
  (agent-repl-test--with-codex-defaults
    (let ((agent-repl-managed-project-pattern "ChessCom")
          (agent-repl-codex-managed-permission-flags "--managed-sentinel"))
      (should (string-match-p
               "--managed-sentinel"
               (agent-repl--codex-start-cmd
                (list :session-id nil :fork-session-id nil
                      :project-dir "/home/u/ChessCom/repo" :model nil)))))))

(ert-deftest agent-repl-test-codex-start-cmd-personal-perm-flags ()
  "A personal project routes to the personal flags.
Sentinel-bound so the routing is pinned independently of the two
defcustoms' (currently identical) default values."
  (agent-repl-test--with-codex-defaults
    (let ((agent-repl-managed-project-pattern "ChessCom")
          (agent-repl-codex-personal-permission-flags "--personal-sentinel"))
      (should (string-match-p
               "--personal-sentinel"
               (agent-repl--codex-start-cmd
                (list :session-id nil :fork-session-id nil
                      :project-dir "/home/u/personal" :model nil)))))))

(ert-deftest agent-repl-test-codex-start-cmd-nil-project-dir-errors ()
  "A nil project-dir fails hard (permission split unresolvable)."
  (agent-repl-test--with-codex-defaults
    (should-error (agent-repl--codex-start-cmd
                   (list :session-id nil :fork-session-id nil
                         :project-dir nil :model nil)))))

;;;; ---- Tests: start-cmd — CODEX_HOME prefix ----

(ert-deftest agent-repl-test-codex-start-cmd-home-prefix ()
  "An explicit codex home is prepended as a CODEX_HOME env assignment."
  (agent-repl-test--with-codex-defaults
    (let ((agent-repl-codex-home "/tmp/codex-alt"))
      (should (string-prefix-p
               "CODEX_HOME=/tmp/codex-alt codex"
               (agent-repl--codex-start-cmd
                (list :session-id nil :fork-session-id nil
                      :project-dir "/home/u/personal" :model nil)))))))

(ert-deftest agent-repl-test-codex-start-cmd-home-prefix-shell-quoted ()
  "A CODEX_HOME containing spaces is shell-quoted in the prefix."
  (agent-repl-test--with-codex-defaults
    (let ((agent-repl-codex-home "/tmp/codex alt"))
      (should (string-match-p
               "\\`CODEX_HOME=/tmp/codex\\\\ alt "
               (agent-repl--codex-start-cmd
                (list :session-id nil :fork-session-id nil
                      :project-dir "/home/u/personal" :model nil)))))))

(ert-deftest agent-repl-test-codex-start-cmd-no-prefix-by-default ()
  "With a nil codex home, no CODEX_HOME assignment is emitted."
  (agent-repl-test--with-codex-defaults
    (should-not (string-match-p
                 "CODEX_HOME"
                 (agent-repl--codex-start-cmd
                  (list :session-id nil :fork-session-id nil
                        :project-dir "/home/u/personal" :model nil))))))

;;;; ---- Tests: headless cmd ----

(ert-deftest agent-repl-test-codex-headless-cmd-shape ()
  "The codex headless builder emits `codex exec --skip-git-repo-check --model M'.
The git-check skip is mandatory: headless spawns run from
`temporary-file-directory', which is not a git repository."
  (should (equal (agent-repl--codex-headless-cmd "gpt-5-codex" nil)
                 '("codex" "exec" "--skip-git-repo-check"
                   "--model" "gpt-5-codex"))))

(ert-deftest agent-repl-test-codex-headless-cmd-appends-extra ()
  "Extra-args are appended after the standard codex exec prefix."
  (should (equal (agent-repl--codex-headless-cmd "m" '("--json"))
                 '("codex" "exec" "--skip-git-repo-check"
                   "--model" "m" "--json"))))

;;;; ---- Tests: rollout locating ----

(defun agent-repl-test--make-rollout-tree (root uuid)
  "Create a fake dated rollout tree under ROOT for UUID; return the file path."
  (let* ((dir (expand-file-name "sessions/2026/07/10" root))
         (path (expand-file-name
                (format "rollout-2026-07-10T12-00-00-%s.jsonl" uuid) dir)))
    (make-directory dir t)
    (with-temp-file path (insert ""))
    path))

(ert-deftest agent-repl-test-codex-rollout-path-finds-by-uuid ()
  "The locator finds the rollout under the dated tree by session uuid."
  (agent-repl-test--with-clean-state
    (let* ((root (make-temp-file "agent-codex-" t))
           (expected (agent-repl-test--make-rollout-tree
                      root agent-repl-test--codex-uuid))
           (agent-repl-codex-home root))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ai-title-ws-session-id)
                     (lambda (_ws) agent-repl-test--codex-uuid)))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
            (should (equal (agent-repl--codex-rollout-path "ws1") expected)))
        (delete-directory root t)))))

(ert-deftest agent-repl-test-codex-rollout-path-nil-when-no-session ()
  "No session id yields nil (mode-line safe before a session starts)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ai-title-ws-session-id)
               (lambda (_ws) nil)))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (should-not (agent-repl--codex-rollout-path "ws1")))))

(ert-deftest agent-repl-test-codex-rollout-path-nil-when-dir-missing ()
  "A missing sessions dir yields nil rather than signalling."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-codex-home "/nonexistent/codex-home"))
      (cl-letf (((symbol-function 'agent-repl--ai-title-ws-session-id)
                 (lambda (_ws) agent-repl-test--codex-uuid)))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
        (should-not (agent-repl--codex-rollout-path "ws1"))))))

(ert-deftest agent-repl-test-codex-rollout-path-cached-skips-rescan ()
  "A cache hit (same session id, file alive) skips the directory rescan."
  (agent-repl-test--with-clean-state
    (let* ((root (make-temp-file "agent-codex-" t))
           (expected (agent-repl-test--make-rollout-tree
                      root agent-repl-test--codex-uuid))
           (agent-repl-codex-home root)
           (scans 0))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ai-title-ws-session-id)
                     (lambda (_ws) agent-repl-test--codex-uuid))
                    ((symbol-function 'directory-files-recursively)
                     (lambda (&rest _)
                       (cl-incf scans)
                       (list expected))))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
            (agent-repl--codex-rollout-path "ws1")
            (agent-repl--codex-rollout-path "ws1")
            (should (= scans 1)))
        (delete-directory root t)))))

(ert-deftest agent-repl-test-codex-rollout-path-rescans-when-cached-file-gone ()
  "A cached path whose file disappeared triggers a rescan."
  (agent-repl-test--with-clean-state
    (let* ((root (make-temp-file "agent-codex-" t))
           (expected (agent-repl-test--make-rollout-tree
                      root agent-repl-test--codex-uuid))
           (agent-repl-codex-home root))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ai-title-ws-session-id)
                     (lambda (_ws) agent-repl-test--codex-uuid)))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
            (agent-repl--ws-put "ws1" :codex-rollout-cache
                                 (cons agent-repl-test--codex-uuid
                                       "/gone/rollout.jsonl"))
            (should (equal (agent-repl--codex-rollout-path "ws1") expected)))
        (delete-directory root t)))))

;;;; ---- Tests: rollout parsing — model ----

(defconst agent-repl-test--codex-turn-context-line
  (concat "{\"timestamp\":\"2026-07-10T12:00:01Z\",\"type\":\"turn_context\","
          "\"payload\":{\"turn_id\":\"t1\",\"cwd\":\"/p\",\"model\":\"gpt-5.4\"}}")
  "A realistic rollout turn_context line.")

(ert-deftest agent-repl-test-codex-model-extract-latest-turn-context ()
  "The newest turn_context line wins (model can change mid-session)."
  (let ((tail (concat
               "{\"type\":\"turn_context\",\"payload\":{\"model\":\"gpt-old\"}}\n"
               agent-repl-test--codex-turn-context-line "\n"
               "{\"type\":\"event_msg\",\"payload\":{\"type\":\"agent_message\"}}\n")))
    (should (equal (agent-repl--codex-model-extract-from-tail tail) "gpt-5.4"))))

(ert-deftest agent-repl-test-codex-model-extract-skips-unparseable ()
  "A truncated turn_context line (tail-window cut) is skipped for an older one."
  (let ((tail (concat
               "{\"type\":\"turn_context\",\"payload\":{\"model\":\"gpt-old\"}}\n"
               "{\"type\":\"turn_context\",\"payload\":{\"model\":\"gpt-cut")))
    (should (equal (agent-repl--codex-model-extract-from-tail tail) "gpt-old"))))

(ert-deftest agent-repl-test-codex-model-extract-nil-cases ()
  "Empty / titleless tails yield nil."
  (should-not (agent-repl--codex-model-extract-from-tail nil))
  (should-not (agent-repl--codex-model-extract-from-tail ""))
  (should-not (agent-repl--codex-model-extract-from-tail
               "{\"type\":\"event_msg\",\"payload\":{\"type\":\"agent_message\"}}")))

;;;; ---- Tests: rollout parsing — context usage ----

(defun agent-repl-test--codex-token-line (input &optional cached output)
  "Return a token_count rollout line with INPUT/CACHED/OUTPUT counts."
  (format (concat "{\"type\":\"event_msg\",\"payload\":{\"type\":\"token_count\","
                  "\"info\":{\"total_token_usage\":{\"input_tokens\":99,\"output_tokens\":9},"
                  "\"last_token_usage\":{\"input_tokens\":%d,"
                  "\"cached_input_tokens\":%d,\"output_tokens\":%d}}}}")
          input (or cached 0) (or output 0)))

(ert-deftest agent-repl-test-codex-context-extract-last-usage-input ()
  "The context figure is last_token_usage.input_tokens (cache-inclusive)."
  (should (= (agent-repl--codex-context-extract-from-tail
              (agent-repl-test--codex-token-line 26256 22656 436))
             26256)))

(ert-deftest agent-repl-test-codex-context-extract-latest-wins ()
  "The newest token_count line wins over older ones."
  (let ((tail (concat (agent-repl-test--codex-token-line 100) "\n"
                      (agent-repl-test--codex-token-line 200) "\n")))
    (should (= (agent-repl--codex-context-extract-from-tail tail) 200))))

(ert-deftest agent-repl-test-codex-context-extract-skips-null-info ()
  "A rate-limit-only token_count (null info) is skipped for an older line."
  (let ((tail (concat
               (agent-repl-test--codex-token-line 300) "\n"
               "{\"type\":\"event_msg\",\"payload\":{\"type\":\"token_count\",\"info\":null}}\n")))
    (should (= (agent-repl--codex-context-extract-from-tail tail) 300))))

(ert-deftest agent-repl-test-codex-context-extract-nil-when-absent ()
  "A tail with no token_count lines yields nil."
  (should-not (agent-repl--codex-context-extract-from-tail
               agent-repl-test--codex-turn-context-line)))

;;;; ---- Tests: readers wire tail reading to parsers ----

(ert-deftest agent-repl-test-codex-model-read-from-file ()
  "The model reader reads the file tail and extracts the model."
  (let ((path (make-temp-file "agent-codex-rollout-")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert agent-repl-test--codex-turn-context-line "\n"))
          (should (equal (agent-repl--codex-model-read path) "gpt-5.4")))
      (delete-file path))))

(ert-deftest agent-repl-test-codex-context-read-from-file ()
  "The context reader reads the file tail and extracts the usage."
  (let ((path (make-temp-file "agent-codex-rollout-")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert (agent-repl-test--codex-token-line 512) "\n"))
          (should (= (agent-repl--codex-context-read path) 512)))
      (delete-file path))))

;;;; ---- Tests: hooks installation ----

(ert-deftest agent-repl-test-codex-install-hooks-writes-managed-set ()
  "The installer registers exactly the codex-managed events in hooks.json."
  (let* ((root (make-temp-file "agent-codex-" t))
         (agent-repl-codex-home root))
    (unwind-protect
        (progn
          (agent-repl-codex-install-hooks)
          (let* ((json (agent-repl--read-settings-alist
                        (agent-repl--codex-hooks-file)))
                 (hooks (cdr (assq 'hooks json))))
            (should (= (length hooks)
                       (length agent-repl--codex-managed-hooks)))
            (dolist (pair agent-repl--codex-managed-hooks)
              (should (agent-repl--event-has-command-p
                       hooks (car pair) (cdr pair))))))
      (delete-directory root t))))

(ert-deftest agent-repl-test-codex-install-hooks-excludes-claude-only-events ()
  "Notification and StopFailure (absent from codex) are never registered."
  (should-not (assq 'Notification agent-repl--codex-managed-hooks))
  (should-not (assq 'StopFailure agent-repl--codex-managed-hooks)))

(ert-deftest agent-repl-test-codex-install-hooks-idempotent ()
  "A second install run is a no-op (returns nil, no duplicate entries)."
  (let* ((root (make-temp-file "agent-codex-" t))
         (agent-repl-codex-home root))
    (unwind-protect
        (progn
          (should (agent-repl-codex-install-hooks))
          (should-not (agent-repl-codex-install-hooks)))
      (delete-directory root t))))

;;;; ---- Tests: doctor ----

(defmacro agent-repl-test--with-codex-doctor-env (&rest body)
  "Run BODY outside the sandbox with a temp codex home bound as ROOT."
  (declare (indent 0))
  `(let* ((root (make-temp-file "agent-codex-" t))
          (agent-repl-codex-home root))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-repl--in-sandbox-p)
                    (lambda () nil)))
           ,@body)
       (delete-directory root t))))

(ert-deftest agent-repl-test-codex-doctor-silent-when-not-in-use ()
  "No codex signals at all yields no issues."
  (let ((agent-repl-codex-home nil)
        (agent-repl-default-backend 'claude))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'agent-repl--codex-hooks-file)
               (lambda () "/nonexistent/hooks.json")))
      (agent-repl-test--with-clean-state
        (should-not (agent-repl--codex-doctor-issues))))))

(ert-deftest agent-repl-test-codex-doctor-missing-binary-errors ()
  "codex in use without the binary on PATH yields an error issue."
  (agent-repl-test--with-codex-doctor-env
    (agent-repl-codex-install-hooks)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should (rassoc "codex backend in use but `codex' is not on PATH"
                      (agent-repl--codex-doctor-issues))))))

(ert-deftest agent-repl-test-codex-doctor-missing-home-errors ()
  "An explicitly configured but absent CODEX_HOME yields an error issue."
  (let ((agent-repl-codex-home "/nonexistent/codex-home"))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () nil))
              ((symbol-function 'executable-find) (lambda (_) "/usr/bin/codex")))
      (should (cl-find-if (lambda (issue)
                            (and (eq (car issue) 'error)
                                 (string-match-p "does not exist" (cdr issue))))
                          (agent-repl--codex-doctor-issues))))))

(ert-deftest agent-repl-test-codex-doctor-missing-hooks-file-errors ()
  "codex in use with no hooks.json yields a single install-pointer error."
  (agent-repl-test--with-codex-doctor-env
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/codex")))
      (should (cl-find-if (lambda (issue)
                            (and (eq (car issue) 'error)
                                 (string-match-p "hooks\\.json is missing"
                                                 (cdr issue))))
                          (agent-repl--codex-doctor-issues))))))

(ert-deftest agent-repl-test-codex-doctor-unregistered-stop-errors ()
  "A hooks.json missing the Stop registration yields an error for it."
  (agent-repl-test--with-codex-doctor-env
    (with-temp-file (agent-repl--codex-hooks-file) (insert "{\"hooks\":{}}"))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/codex")))
      (should (cl-find-if (lambda (issue)
                            (and (eq (car issue) 'error)
                                 (string-match-p "hook Stop" (cdr issue))))
                          (agent-repl--codex-doctor-issues))))))

(ert-deftest agent-repl-test-codex-doctor-unregistered-subagent-warns ()
  "A hooks.json missing SubagentStart yields only a warning for it."
  (agent-repl-test--with-codex-doctor-env
    (with-temp-file (agent-repl--codex-hooks-file) (insert "{\"hooks\":{}}"))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/codex")))
      (should (cl-find-if (lambda (issue)
                            (and (eq (car issue) 'warn)
                                 (string-match-p "hook SubagentStart" (cdr issue))))
                          (agent-repl--codex-doctor-issues))))))

(ert-deftest agent-repl-test-codex-doctor-clean-after-install ()
  "A full install with the binary present yields zero issues."
  (agent-repl-test--with-codex-doctor-env
    (agent-repl-codex-install-hooks)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/codex")))
      (should-not (agent-repl--codex-doctor-issues)))))

(ert-deftest agent-repl-test-codex-doctor-silent-in-sandbox ()
  "Inside the sandbox the codex doctor is silent (host-only concern)."
  (let ((agent-repl-codex-home "/nonexistent"))
    (cl-letf (((symbol-function 'agent-repl--in-sandbox-p) (lambda () t)))
      (should-not (agent-repl--codex-doctor-issues)))))

(ert-deftest agent-repl-test-codex-doctor-in-use-via-ws-backend ()
  "A registered workspace with a codex :backend marks codex as in use."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-codex-home nil)
          (agent-repl-default-backend 'claude))
      (cl-letf (((symbol-function 'agent-repl--codex-hooks-file)
                 (lambda () "/nonexistent/hooks.json")))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
        (agent-repl--ws-put "ws1" :backend 'codex)
        (should (agent-repl--codex-in-use-p))))))

;;;; ---- Tests: registration ----

(ert-deftest agent-repl-test-codex-backend-registered ()
  "The codex backend registers with binary `codex' and all slots but title."
  (let ((b (agent-repl-backend-get 'codex)))
    (should (equal (agent-repl-backend-binary b) "codex"))
    (should (eq (agent-repl-backend-start-cmd-fn b)
                #'agent-repl--codex-start-cmd))
    (should (eq (agent-repl-backend-headless-cmd-fn b)
                #'agent-repl--codex-headless-cmd))
    (should (eq (agent-repl-backend-transcript-path-fn b)
                #'agent-repl--codex-rollout-path))
    (should (eq (agent-repl-backend-transcript-model-fn b)
                #'agent-repl--codex-model-read))
    (should (eq (agent-repl-backend-transcript-context-fn b)
                #'agent-repl--codex-context-read))))

(ert-deftest agent-repl-test-codex-backend-no-title-capability ()
  "codex rollouts carry no conversation title, so the title slot is nil."
  (should-not (agent-repl-backend-transcript-title-fn
               (agent-repl-backend-get 'codex))))

(provide 'test-codex)
;;; test-codex.el ends here
