;;; test-session.el --- Tests for session.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for session lifecycle management: command building, session
;; startup, completion handling, session ID management, readiness
;; handling, process state predicates, and the ready timer.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Migrated tests ----

(ert-deftest agent-repl-test-maybe-notify-debounce ()
  "maybe-notify-finished should debounce within 2 seconds."
  (agent-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (cl-incf notify-count))))
        ;; First call should notify
        (agent-repl--maybe-notify-finished "ws1")
        (should (= notify-count 1))
        ;; Second call within 2s window should be suppressed
        (agent-repl--maybe-notify-finished "ws1")
        (should (= notify-count 1))
        ;; Simulate time passing beyond debounce window
        (agent-repl--ws-put "ws1" :last-notify-time (- (float-time) 3.0))
        (agent-repl--maybe-notify-finished "ws1")
        (should (= notify-count 2))))))

(ert-deftest agent-repl-test-maybe-notify-skips-when-focused ()
  "maybe-notify-finished should NOT send desktop notification when frame is focused."
  (agent-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () t))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (cl-incf notify-count))))
        (agent-repl--maybe-notify-finished "ws1")
        (should (= notify-count 0))))))

(ert-deftest agent-repl-test-finished-from-hook-sets-done ()
  "handle-agent-finished sets :agent-state :done unconditionally.
There is no vterm buffer or visibility concept left in the gui-only
world; the Stop signal's intent is simply \"the agent finished\"."
  (agent-repl-test--with-clean-state
    (let ((done-set nil))
      ;; Register ws1 (required by handle-agent-finished guard).
      (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'agent-repl--maybe-notify-finished) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--ws-set-agent-state)
                 (lambda (ws state)
                   (when (eq state :done) (setq done-set ws)))))
        (agent-repl--handle-agent-finished "ws1")
        (should (equal done-set "ws1"))))))

;;;; ---- Tests: Deferred prompt drain ----

(ert-deftest agent-repl-test-drain-deferred-empty-queue-noop ()
  "`agent-repl--drain-deferred-prompts' on an empty queue does nothing."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state :done)
    (agent-repl--ws-put "ws1" :deferred-prompts nil)
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (&rest args) (push args sent))))
        (agent-repl--drain-deferred-prompts "ws1")
        (should (null sent))))))

(ert-deftest agent-repl-test-drain-deferred-pops-and-sends-when-done ()
  "Drain pops the head and sends it via `agent-repl--send' when state is `:done'."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state :done)
    (agent-repl--ws-put "ws1" :deferred-prompts '("alpha" "beta" "gamma"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (prompt ws &rest _) (setq sent (list prompt ws)))))
        (agent-repl--drain-deferred-prompts "ws1")
        (should (equal sent '("alpha" "ws1")))
        (should (equal (agent-repl--ws-get "ws1" :deferred-prompts)
                       '("beta" "gamma")))))))

(ert-deftest agent-repl-test-drain-deferred-pops-and-sends-when-idle ()
  "Drain also fires when state is `:idle' (decayed from `:done')."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state :idle)
    (agent-repl--ws-put "ws1" :deferred-prompts '("only-one"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (prompt ws &rest _) (setq sent (list prompt ws)))))
        (agent-repl--drain-deferred-prompts "ws1")
        (should (equal sent '("only-one" "ws1")))
        (should (null (agent-repl--ws-get "ws1" :deferred-prompts)))))))

(ert-deftest agent-repl-test-drain-deferred-skipped-when-thinking ()
  "Drain does NOT pop or send when state is `:thinking', even with a non-empty queue."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state :thinking)
    (agent-repl--ws-put "ws1" :deferred-prompts '("hold-me"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (&rest args) (push args sent))))
        (agent-repl--drain-deferred-prompts "ws1")
        (should (null sent))
        (should (equal (agent-repl--ws-get "ws1" :deferred-prompts)
                       '("hold-me")))))))

(ert-deftest agent-repl-test-drain-deferred-skipped-when-permission ()
  "Drain does NOT fire while the agent is at a `:permission' prompt."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state :permission)
    (agent-repl--ws-put "ws1" :deferred-prompts '("hold-me"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (&rest args) (push args sent))))
        (agent-repl--drain-deferred-prompts "ws1")
        (should (null sent))
        (should (equal (agent-repl--ws-get "ws1" :deferred-prompts)
                       '("hold-me")))))))

(ert-deftest agent-repl-test-drain-deferred-skipped-when-init ()
  "Drain does NOT fire while the agent is still initializing (`:init')."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state :init)
    (agent-repl--ws-put "ws1" :deferred-prompts '("hold-me"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl--send)
                 (lambda (&rest args) (push args sent))))
        (agent-repl--drain-deferred-prompts "ws1")
        (should (null sent))))))

(ert-deftest agent-repl-test-handle-agent-finished-drains-deferred ()
  "`agent-repl--handle-agent-finished' drains the deferred queue at the end."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
    (agent-repl--ws-put "ws1" :deferred-prompts '("first-deferred" "second"))
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl--maybe-notify-finished) #'ignore)
                ((symbol-function 'agent-repl--refresh-magit-status) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--send)
                 (lambda (prompt ws &rest _) (setq sent (list prompt ws)))))
        ;; handle-agent-finished marks state :done first, then drains.
        (agent-repl--handle-agent-finished "ws1")
        (should (equal sent '("first-deferred" "ws1")))
        ;; One drained, one remains for the next turn.
        (should (equal (agent-repl--ws-get "ws1" :deferred-prompts)
                       '("second")))))))

(ert-deftest agent-repl-test-handle-agent-finished-no-deferred-noop ()
  "`handle-agent-finished' with an empty deferred queue does not call `--send'."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
    (agent-repl--ws-put "ws1" :deferred-prompts nil)
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl--maybe-notify-finished) #'ignore)
                ((symbol-function 'agent-repl--refresh-magit-status) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--send)
                 (lambda (&rest args) (push args sent))))
        (agent-repl--handle-agent-finished "ws1")
        (should (null sent))))))

;;;; ---- Tests: Command building ----

(ert-deftest agent-repl-test-compute-claude-flags-continue ()
  "compute-claude-flags should emit --continue when session-id is set and no fork."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags "abc123" nil nil)
                   "--continue"))))

(ert-deftest agent-repl-test-compute-claude-flags-no-continue-without-session-id ()
  "compute-claude-flags should not emit --continue when session-id is nil."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags nil nil nil) ""))))

(ert-deftest agent-repl-test-compute-claude-flags-fork ()
  "compute-claude-flags should emit --resume <id> --fork-session for forks."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags "current" "fork-id" nil)
                   "--resume fork-id --fork-session"))))

(ert-deftest agent-repl-test-compute-claude-flags-fork-ignores-session ()
  "compute-claude-flags with fork should not also emit --continue for session-id."
  (let* ((agent-repl-system-prompt nil)
         (agent-repl-interactive-model nil)
         (result (agent-repl--compute-claude-flags "current" "fork-id" nil)))
    (should (string-match-p "--resume fork-id --fork-session" result))
    (should-not (string-match-p "--continue" result))))

(ert-deftest agent-repl-test-compute-claude-flags-model-arg-overrides-default ()
  "An explicit MODEL arg is emitted as `--model <model>' over the global default."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model "opus"))
    (should (equal (agent-repl--compute-claude-flags nil nil nil "sonnet")
                   "--model sonnet"))))

(ert-deftest agent-repl-test-compute-claude-flags-model-falls-back-to-interactive-model ()
  "When MODEL is nil, `agent-repl-interactive-model' supplies the model."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model "opus"))
    (should (equal (agent-repl--compute-claude-flags nil nil nil nil)
                   "--model opus"))))

(ert-deftest agent-repl-test-compute-claude-flags-model-nil-both-emits-no-model ()
  "When both MODEL and `agent-repl-interactive-model' are nil, no `--model' flag."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags nil nil nil nil) ""))))

(ert-deftest agent-repl-test-effective-model-explicit-wins ()
  "An explicit MODEL is returned over the interactive default."
  (let ((agent-repl-interactive-model "opus"))
    (should (equal (agent-repl--effective-model "sonnet") "sonnet"))))

(ert-deftest agent-repl-test-effective-model-falls-back-to-interactive ()
  "A nil MODEL falls back to `agent-repl-interactive-model'."
  (let ((agent-repl-interactive-model "opus"))
    (should (equal (agent-repl--effective-model nil) "opus"))))

(ert-deftest agent-repl-test-effective-model-nil-both ()
  "MODEL and `agent-repl-interactive-model' both nil yields nil."
  (let ((agent-repl-interactive-model nil))
    (should (null (agent-repl--effective-model nil)))))

(ert-deftest agent-repl-test-compute-claude-flags-perm-flag ()
  "compute-claude-flags should include permission flag when provided."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags nil nil "--permission-mode auto")
                   "--permission-mode auto"))))

(ert-deftest agent-repl-test-compute-claude-flags-all-nil ()
  "compute-claude-flags should return empty string when all args are nil."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags nil nil nil) ""))))

(ert-deftest agent-repl-test-compute-claude-flags-continue-plus-perm ()
  "compute-claude-flags should combine --continue and perm flag."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags "sess1" nil "--dangerously-skip-permissions")
                   "--continue --dangerously-skip-permissions"))))

(ert-deftest agent-repl-test-compute-claude-flags-system-prompt-period ()
  "compute-claude-flags should emit --system-prompt \".\" with literal quotes."
  (let ((agent-repl-system-prompt ".")
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags nil nil nil)
                   "--system-prompt \".\""))))

(ert-deftest agent-repl-test-compute-claude-flags-system-prompt-nil ()
  "compute-claude-flags should omit --system-prompt entirely when var is nil."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model nil))
    (let ((result (agent-repl--compute-claude-flags nil nil nil)))
      (should-not (string-match-p "--system-prompt" result)))))

(ert-deftest agent-repl-test-compute-claude-flags-system-prompt-shell-quoted ()
  "compute-claude-flags should wrap the system prompt in literal double quotes."
  (let ((agent-repl-system-prompt "be nice")
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags nil nil nil)
                   "--system-prompt \"be nice\""))))

(ert-deftest agent-repl-test-compute-claude-flags-system-prompt-escapes-dquote ()
  "compute-claude-flags should backslash-escape embedded double quotes."
  (let ((agent-repl-system-prompt "say \"hi\"")
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags nil nil nil)
                   "--system-prompt \"say \\\"hi\\\"\""))))

(ert-deftest agent-repl-test-compute-claude-flags-system-prompt-escapes-dollar ()
  "compute-claude-flags should backslash-escape $ to prevent expansion."
  (let ((agent-repl-system-prompt "$HOME")
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags nil nil nil)
                   "--system-prompt \"\\$HOME\""))))

(ert-deftest agent-repl-test-compute-claude-flags-system-prompt-combines-with-continue ()
  "compute-claude-flags should append --system-prompt after --continue and perm flag."
  (let ((agent-repl-system-prompt ".")
        (agent-repl-interactive-model nil))
    (should (equal (agent-repl--compute-claude-flags "sess1" nil "--dangerously-skip-permissions")
                   "--continue --dangerously-skip-permissions --system-prompt \".\""))))

(ert-deftest agent-repl-test-compute-claude-flags-model-default-opus ()
  "compute-claude-flags should emit --model opus with the default interactive model."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model "opus"))
    (should (equal (agent-repl--compute-claude-flags nil nil nil)
                   "--model opus"))))

(ert-deftest agent-repl-test-compute-claude-flags-model-nil-omits-flag ()
  "compute-claude-flags should omit --model entirely when interactive model is nil."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model nil))
    (should-not (string-match-p "--model" (agent-repl--compute-claude-flags nil nil nil)))))

(ert-deftest agent-repl-test-compute-claude-flags-model-precedes-continue ()
  "compute-claude-flags should emit --model before --continue."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model "opus"))
    (let ((result (agent-repl--compute-claude-flags "sess1" nil nil)))
      (should (equal result "--model opus --continue")))))

(ert-deftest agent-repl-test-compute-claude-flags-model-precedes-fork ()
  "compute-claude-flags should emit --model before --resume/--fork-session."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model "opus"))
    (let ((result (agent-repl--compute-claude-flags "current" "fork-id" nil)))
      (should (equal result "--model opus --resume fork-id --fork-session")))))

(ert-deftest agent-repl-test-compute-claude-flags-model-with-all-flags ()
  "compute-claude-flags should include --model opus first in full flag combination."
  (let ((agent-repl-system-prompt ".")
        (agent-repl-interactive-model "opus"))
    (should (equal (agent-repl--compute-claude-flags "sess1" nil "--dangerously-skip-permissions")
                   "--model opus --continue --dangerously-skip-permissions --system-prompt \".\""))))

(ert-deftest agent-repl-test-compute-claude-flags-model-custom-value ()
  "compute-claude-flags should emit --model with a custom model alias."
  (let ((agent-repl-system-prompt nil)
        (agent-repl-interactive-model "sonnet"))
    (should (equal (agent-repl--compute-claude-flags nil nil nil)
                   "--model sonnet"))))

(ert-deftest agent-repl-test-compute-perm-flag-chesscom ()
  "compute-perm-flag should return --permission-mode auto for ChessCom repos."
  (should (equal (agent-repl--compute-perm-flag "/home/user/ChessCom/project")
                 "--permission-mode auto")))

(ert-deftest agent-repl-test-compute-perm-flag-personal ()
  "compute-perm-flag should return --permission-mode auto for personal repos."
  (should (equal (agent-repl--compute-perm-flag "/home/user/personal/project")
                 "--permission-mode auto")))

(ert-deftest agent-repl-test-compute-perm-flag-nil-dir ()
  "compute-perm-flag with nil project-dir should signal an error."
  (should-error (agent-repl--compute-perm-flag nil) :type 'error))

(ert-deftest agent-repl-test-compute-perm-flag-chesscom-dir ()
  "compute-perm-flag with ChessCom project-dir returns auto permissions."
  (should (equal (agent-repl--compute-perm-flag "/tmp/ChessCom/test")
                 "--permission-mode auto")))

;;;; ---- Tests: agent-repl--model-haiku-p ----

(ert-deftest agent-repl-test-model-haiku-p-nil ()
  "model-haiku-p returns nil for a nil model."
  (should-not (agent-repl--model-haiku-p nil)))

(ert-deftest agent-repl-test-model-haiku-p-empty ()
  "model-haiku-p returns nil for an empty-string model."
  (should-not (agent-repl--model-haiku-p "")))

(ert-deftest agent-repl-test-model-haiku-p-bare-haiku ()
  "model-haiku-p returns non-nil for the bare `haiku' alias."
  (should (agent-repl--model-haiku-p "haiku")))

(ert-deftest agent-repl-test-model-haiku-p-versioned ()
  "model-haiku-p returns non-nil for a versioned `haiku-4-5' alias."
  (should (agent-repl--model-haiku-p "haiku-4-5")))

(ert-deftest agent-repl-test-model-haiku-p-vendor-prefixed ()
  "model-haiku-p returns non-nil for a vendor-prefixed `claude-haiku-4-5' id."
  (should (agent-repl--model-haiku-p "claude-haiku-4-5")))

(ert-deftest agent-repl-test-model-haiku-p-case-insensitive ()
  "model-haiku-p matches case-insensitively (`Haiku')."
  (should (agent-repl--model-haiku-p "Haiku")))

(ert-deftest agent-repl-test-model-haiku-p-opus ()
  "model-haiku-p returns nil for `opus'."
  (should-not (agent-repl--model-haiku-p "opus")))

(ert-deftest agent-repl-test-model-haiku-p-sonnet ()
  "model-haiku-p returns nil for `sonnet'."
  (should-not (agent-repl--model-haiku-p "sonnet")))

;;;; ---- Tests: agent-repl--compute-perm-flag model gating ----

(ert-deftest agent-repl-test-compute-perm-flag-haiku-personal-downgrades ()
  "compute-perm-flag downgrades a personal repo to skip-permissions for haiku."
  (should (equal (agent-repl--compute-perm-flag "/home/user/personal/project" "haiku")
                 "--dangerously-skip-permissions")))

(ert-deftest agent-repl-test-compute-perm-flag-haiku-chesscom-downgrades ()
  "compute-perm-flag downgrades a ChessCom repo to skip-permissions for haiku."
  (should (equal (agent-repl--compute-perm-flag "/home/user/ChessCom/project" "haiku")
                 "--dangerously-skip-permissions")))

(ert-deftest agent-repl-test-compute-perm-flag-haiku-versioned-downgrades ()
  "compute-perm-flag downgrades for a versioned haiku alias."
  (should (equal (agent-repl--compute-perm-flag "/home/user/personal/project" "haiku-4-5")
                 "--dangerously-skip-permissions")))

(ert-deftest agent-repl-test-compute-perm-flag-opus-keeps-auto ()
  "compute-perm-flag keeps --permission-mode auto for a non-haiku (opus) model."
  (should (equal (agent-repl--compute-perm-flag "/home/user/personal/project" "opus")
                 "--permission-mode auto")))

(ert-deftest agent-repl-test-compute-perm-flag-sonnet-keeps-auto ()
  "compute-perm-flag keeps --permission-mode auto for a non-haiku (sonnet) model."
  (should (equal (agent-repl--compute-perm-flag "/home/user/ChessCom/project" "sonnet")
                 "--permission-mode auto")))

(ert-deftest agent-repl-test-compute-perm-flag-nil-model-keeps-auto ()
  "compute-perm-flag keeps --permission-mode auto when model is nil."
  (should (equal (agent-repl--compute-perm-flag "/home/user/personal/project" nil)
                 "--permission-mode auto")))

(ert-deftest agent-repl-test-assemble-cmd-bare-metal ()
  "assemble-cmd should produce a plain `claude' command."
  (should (equal (agent-repl--assemble-cmd "--resume abc")
                 "AGENT_REPL_OWNED=1 claude --resume abc")))

(ert-deftest agent-repl-test-assemble-cmd-no-flags ()
  "assemble-cmd with empty flags should produce clean command."
  (should (equal (agent-repl--assemble-cmd "") "AGENT_REPL_OWNED=1 claude")))

(ert-deftest agent-repl-test-assemble-cmd-config-dir ()
  "assemble-cmd prepends CLAUDE_CONFIG_DIR when config-dir is given."
  (should (equal (agent-repl--assemble-cmd "--resume abc" "/home/u/.claude-cc")
                 "AGENT_REPL_OWNED=1 CLAUDE_CONFIG_DIR=/home/u/.claude-cc claude --resume abc")))

(ert-deftest agent-repl-test-assemble-cmd-always-marks-ownership ()
  "Every module-launched CLI carries AGENT_REPL_OWNED=1.
The hook scripts stamp the sentinel ownership marker from it, which is
what stops a foreign claude in the same cwd from hijacking the
workspace's durable session id."
  ;; Act / Assert — with and without a config dir.
  (should (string-prefix-p "AGENT_REPL_OWNED=1 " (agent-repl--assemble-cmd "")))
  (should (string-prefix-p "AGENT_REPL_OWNED=1 "
                           (agent-repl--assemble-cmd "--resume abc" "/cc"))))

(ert-deftest agent-repl-test-assemble-cmd-never-sandbox ()
  "assemble-cmd never emits a claude-sandbox invocation, even with a config-dir."
  (should-not (string-match-p "claude-sandbox"
                              (agent-repl--assemble-cmd "--resume abc" "/cc"))))

;;;; ---- Tests: agent-repl--compute-config-dir ----

(ert-deftest agent-repl-test-compute-config-dir-under-multi-repo ()
  "compute-config-dir returns the multi-repo config dir for projects under the root."
  (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment)))
    (should (equal (agent-repl--compute-config-dir "/home/user/multi/repoA/proj")
                   (expand-file-name agent-repl-multi-repo-config-dir)))))

(ert-deftest agent-repl-test-compute-config-dir-outside-nil-default ()
  "compute-config-dir returns nil outside the root when default-config-dir is nil."
  (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
        (agent-repl-default-config-dir nil))
    (should-not (agent-repl--compute-config-dir "/home/user/other/proj"))))

(ert-deftest agent-repl-test-compute-config-dir-outside-explicit-default ()
  "compute-config-dir returns the expanded explicit default dir outside the root."
  (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
        (agent-repl-default-config-dir "~/.claude-personal"))
    (should (equal (agent-repl--compute-config-dir "/home/user/other/proj")
                   (expand-file-name "~/.claude-personal")))))

(ert-deftest agent-repl-test-compute-config-dir-env-unset ()
  "compute-config-dir falls back to the default when the root env var is unset."
  (let ((process-environment (copy-sequence process-environment))
        (agent-repl-default-config-dir nil))
    (setenv "MULTI_REPO_ROOT" nil)
    (should-not (agent-repl--compute-config-dir "/home/user/multi/proj"))))

(ert-deftest agent-repl-test-compute-config-dir-env-empty ()
  "compute-config-dir treats an empty root env var as no multi-repo root."
  (let ((process-environment (cons "MULTI_REPO_ROOT=" process-environment))
        (agent-repl-default-config-dir nil))
    (should-not (agent-repl--compute-config-dir "/home/user/multi/proj"))))

(ert-deftest agent-repl-test-compute-config-dir-string-override-wins ()
  "A workspace :config-dir-override string beats the path-computed account."
  (agent-repl-test--with-clean-state
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment)))
      (agent-repl--ws-put "ws1" :config-dir-override "~/.claude-personal")
      (cl-letf (((symbol-function 'agent-repl--ws-for-dir) (lambda (_dir) "ws1")))
        (should (equal (agent-repl--compute-config-dir "/home/user/multi/repoA/proj")
                       (expand-file-name "~/.claude-personal")))))))

(ert-deftest agent-repl-test-compute-config-dir-default-override-yields-nil ()
  "A :default override selects the CLI's own root even under the multi-repo root."
  (agent-repl-test--with-clean-state
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment)))
      (agent-repl--ws-put "ws1" :config-dir-override :default)
      (cl-letf (((symbol-function 'agent-repl--ws-for-dir) (lambda (_dir) "ws1")))
        (should-not (agent-repl--compute-config-dir "/home/user/multi/repoA/proj"))))))

(ert-deftest agent-repl-test-compute-config-dir-no-override-falls-through ()
  "A workspace without an override still resolves the path-computed account."
  (agent-repl-test--with-clean-state
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment)))
      (cl-letf (((symbol-function 'agent-repl--ws-for-dir) (lambda (_dir) "ws1")))
        (should (equal (agent-repl--compute-config-dir "/home/user/multi/repoA/proj")
                       (expand-file-name agent-repl-multi-repo-config-dir)))))))

(ert-deftest agent-repl-test-compute-config-dir-nil-dir ()
  "compute-config-dir with nil project-dir should signal an error."
  (should-error (agent-repl--compute-config-dir nil) :type 'error))

;;;; ---- Tests: agent-repl-doom-multi-repo-mode ----

(ert-deftest agent-repl-test-doom-config-tree-p-canonical-root ()
  "doom-config-tree-p accepts the canonical doom config checkout itself."
  (let ((agent-repl-doom-config-root "/home/user/.config/doom"))
    (should (agent-repl--doom-config-tree-p "/home/user/.config/doom"))))

(ert-deftest agent-repl-test-doom-config-tree-p-under-root ()
  "doom-config-tree-p accepts a directory nested inside the doom config checkout."
  (let ((agent-repl-doom-config-root "/home/user/.config/doom"))
    (should (agent-repl--doom-config-tree-p "/home/user/.config/doom/modules/app"))))

(ert-deftest agent-repl-test-doom-config-tree-p-worktree ()
  "doom-config-tree-p accepts a generated worktree of the doom config checkout."
  (let ((agent-repl-doom-config-root "/home/user/.config/doom"))
    (should (agent-repl--doom-config-tree-p "/home/user/.config/doom-worktrees/feature-x"))))

(ert-deftest agent-repl-test-doom-config-tree-p-sibling-name-prefix ()
  "doom-config-tree-p rejects a sibling directory that merely shares the name prefix."
  (let ((agent-repl-doom-config-root "/home/user/.config/doom"))
    (should-not (agent-repl--doom-config-tree-p "/home/user/.config/doomsday/proj"))))

(ert-deftest agent-repl-test-doom-config-tree-p-unrelated ()
  "doom-config-tree-p rejects a project outside the doom config tree."
  (let ((agent-repl-doom-config-root "/home/user/.config/doom"))
    (should-not (agent-repl--doom-config-tree-p "/home/user/workspace/other"))))

(ert-deftest agent-repl-test-compute-config-dir-doom-mode-off ()
  "compute-config-dir leaves the doom checkout on the default account with the mode off."
  (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
        (agent-repl-doom-multi-repo-mode nil)
        (agent-repl-doom-config-root "/home/user/.config/doom")
        (agent-repl-default-config-dir nil))
    (should-not (agent-repl--compute-config-dir "/home/user/.config/doom"))))

(ert-deftest agent-repl-test-compute-config-dir-doom-mode-on-root ()
  "compute-config-dir returns the multi-repo config dir for the doom checkout with the mode on."
  (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
        (agent-repl-doom-multi-repo-mode t)
        (agent-repl-doom-config-root "/home/user/.config/doom")
        (agent-repl-default-config-dir nil))
    (should (equal (agent-repl--compute-config-dir "/home/user/.config/doom")
                   (expand-file-name agent-repl-multi-repo-config-dir)))))

(ert-deftest agent-repl-test-compute-config-dir-doom-mode-on-worktree ()
  "compute-config-dir returns the multi-repo config dir for a doom worktree with the mode on."
  (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
        (agent-repl-doom-multi-repo-mode t)
        (agent-repl-doom-config-root "/home/user/.config/doom")
        (agent-repl-default-config-dir nil))
    (should (equal (agent-repl--compute-config-dir "/home/user/.config/doom-worktrees/ws1")
                   (expand-file-name agent-repl-multi-repo-config-dir)))))

(ert-deftest agent-repl-test-compute-config-dir-doom-mode-on-spares-other-projects ()
  "compute-config-dir keeps non-doom projects on the default account with the mode on."
  (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
        (agent-repl-doom-multi-repo-mode t)
        (agent-repl-doom-config-root "/home/user/.config/doom")
        (agent-repl-default-config-dir nil))
    (should-not (agent-repl--compute-config-dir "/home/user/workspace/other"))))

(ert-deftest agent-repl-test-compute-config-dir-doom-mode-on-env-unset ()
  "compute-config-dir honors the doom mode even when the root env var is unset."
  (let ((process-environment (copy-sequence process-environment))
        (agent-repl-doom-multi-repo-mode t)
        (agent-repl-doom-config-root "/home/user/.config/doom")
        (agent-repl-default-config-dir nil))
    (setenv "MULTI_REPO_ROOT" nil)
    (should (equal (agent-repl--compute-config-dir "/home/user/.config/doom")
                   (expand-file-name agent-repl-multi-repo-config-dir)))))

(ert-deftest agent-repl-test-doom-multi-repo-mode-enable-switches-config-dir ()
  "Enabling the mode interactively switches the doom checkout to the multi-repo config dir."
  (let ((agent-repl-doom-config-root "/home/user/.config/doom")
        (agent-repl-default-config-dir nil)
        (was agent-repl-doom-multi-repo-mode))
    (unwind-protect
        (progn
          (agent-repl-doom-multi-repo-mode 1)
          (should agent-repl-doom-multi-repo-mode)
          (should (equal (agent-repl--compute-config-dir "/home/user/.config/doom")
                         (expand-file-name agent-repl-multi-repo-config-dir))))
      (agent-repl-doom-multi-repo-mode (if was 1 -1)))))

(ert-deftest agent-repl-test-doom-multi-repo-mode-disable-restores-config-dir ()
  "Disabling the mode interactively returns the doom checkout to the default account."
  (let ((agent-repl-doom-config-root "/home/user/.config/doom")
        (agent-repl-default-config-dir nil)
        (was agent-repl-doom-multi-repo-mode))
    (unwind-protect
        (progn
          (agent-repl-doom-multi-repo-mode 1)
          (agent-repl-doom-multi-repo-mode -1)
          (should-not agent-repl-doom-multi-repo-mode)
          (should-not (agent-repl--compute-config-dir "/home/user/.config/doom")))
      (agent-repl-doom-multi-repo-mode (if was 1 -1)))))

;;;; ---- Tests: Session completion handling ----

(ert-deftest agent-repl-test-mark-agent-done-sets-done ()
  "mark-agent-done sets :agent-state :done unconditionally."
  (agent-repl-test--with-clean-state
    (let ((done-set nil))
      (cl-letf (((symbol-function 'agent-repl--ws-set-agent-state)
                 (lambda (ws state)
                   (when (eq state :done) (setq done-set ws)))))
        (agent-repl--mark-agent-done "ws1")
        (should (equal done-set "ws1"))))))

(ert-deftest agent-repl-test-mark-agent-done-regardless-of-visibility ()
  "mark-agent-done no longer branches on vterm visibility.
The previous mark-done-if-hidden used the vterm window as a \"user is
already looking\" gate. Post-axis-split that gate is the renderer's job."
  (agent-repl-test--with-clean-state
    (let ((done-set nil))
      ;; Any hypothetical visibility — mark-agent-done does not read it.
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _) 'some-window))
                ((symbol-function 'agent-repl--ws-set-agent-state)
                 (lambda (ws state)
                   (when (eq state :done) (setq done-set ws)))))
        (agent-repl--mark-agent-done "ws1")
        (should (equal done-set "ws1"))))))

(ert-deftest agent-repl-test-mark-agent-done-current-ws-acks ()
  "mark-agent-done sets :done-acked t when the workspace is current
(user is actively looking when :done arrives)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--current-ws-p)
               (lambda (_ws) t)))
      (agent-repl--mark-agent-done "ws1")
      (should (eq (agent-repl--ws-get "ws1" :done-acked) t)))))

(ert-deftest agent-repl-test-mark-agent-done-current-ws-stamps-acked-at ()
  "mark-agent-done stamps :done-acked-at with current time when the
workspace is current, so the focus-dwell countdown can start."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--current-ws-p)
               (lambda (_ws) t)))
      (let ((before (float-time)))
        (agent-repl--mark-agent-done "ws1")
        (let ((stamp (agent-repl--ws-get "ws1" :done-acked-at)))
          (should (numberp stamp))
          (should (>= stamp before)))))))

(ert-deftest agent-repl-test-mark-agent-done-non-current-ws-clears-ack ()
  "mark-agent-done clears :done-acked to nil for non-current workspaces
so a fresh :done starts unacknowledged regardless of any leftover ack
from a prior cycle."
  (agent-repl-test--with-clean-state
    ;; Pretend a prior cycle left :done-acked t — must be cleared.
    (agent-repl--ws-put "ws1" :done-acked t)
    (cl-letf (((symbol-function 'agent-repl--current-ws-p)
               (lambda (_ws) nil)))
      (agent-repl--mark-agent-done "ws1")
      (should (null (agent-repl--ws-get "ws1" :done-acked))))))

(ert-deftest agent-repl-test-mark-agent-done-non-current-ws-clears-acked-at ()
  "mark-agent-done clears :done-acked-at for non-current workspaces so
a stale focus timestamp from a prior cycle does not bleed into the
new :done lifecycle."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :done-acked-at (float-time))
    (cl-letf (((symbol-function 'agent-repl--current-ws-p)
               (lambda (_ws) nil)))
      (agent-repl--mark-agent-done "ws1")
      (should (null (agent-repl--ws-get "ws1" :done-acked-at))))))

(ert-deftest agent-repl-test-handle-agent-finished-notifies-other-ws ()
  "handle-agent-finished should record a line when WS is not the current workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
    (let ((messaged nil))
      (cl-letf (((symbol-function 'agent-repl--maybe-notify-finished) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                ((symbol-function 'agent-repl--do-log-to-file) #'ignore)
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (string-match-p "Agent finished" (apply #'format fmt args))
                     (setq messaged t)))))
        (agent-repl--handle-agent-finished "ws1")
        (should messaged)))))

(ert-deftest agent-repl-test-handle-agent-finished-does-not-reach-echo-area ()
  "The agent-finished notice is background chatter: it goes to the log and
*Messages*, but must NOT be flashed in the echo area / modeline (a desktop
notification is the channel that gets the user's attention)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
    (let ((echoed nil))
      (cl-letf (((symbol-function 'agent-repl--maybe-notify-finished) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                ((symbol-function 'agent-repl--do-log-to-file) #'ignore)
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (and (null inhibit-message)
                              (string-match-p "Agent finished" (apply #'format fmt args)))
                     (setq echoed t)))))
        (agent-repl--handle-agent-finished "ws1")
        (should-not echoed)))))

(ert-deftest agent-repl-test-handle-agent-finished-no-message-current-ws ()
  "handle-agent-finished should NOT message when WS is the current workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
    (let ((messaged nil))
      (cl-letf (((symbol-function 'agent-repl--maybe-notify-finished) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (string-match-p "Claude finished" fmt)
                     (setq messaged t)))))
        (agent-repl--handle-agent-finished "ws1")
        (should-not messaged)))))

(ert-deftest agent-repl-test-handle-agent-finished-errors-on-unregistered-ws ()
  "handle-agent-finished errors hard when WS is not registered — guards
against stop events arriving after kill."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--handle-agent-finished "not-a-ws"))))

;;;; ---- Tests: refresh-magit-status ----

(ert-deftest agent-repl-test-refresh-magit-status-refreshes-matching-buffer ()
  "refresh-magit-status calls magit-refresh on a magit-status buffer whose
default-directory matches the workspace's :project-dir."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "agent-magit-" t))
          (buf (generate-new-buffer " *test-magit-match*"))
          (refreshed 0))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir tmpdir)
            (with-current-buffer buf
              (setq-local major-mode 'magit-status-mode)
              (setq-local default-directory (file-name-as-directory tmpdir)))
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (agent-repl--refresh-magit-status "ws1")
              (should (= refreshed 1))))
        (kill-buffer buf)
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-refresh-magit-status-skips-non-matching-dir ()
  "refresh-magit-status does not refresh a magit-status buffer whose
default-directory points at a different repo."
  (agent-repl-test--with-clean-state
    (let ((ws-dir (make-temp-file "agent-magit-ws-" t))
          (other-dir (make-temp-file "agent-magit-other-" t))
          (buf (generate-new-buffer " *test-magit-other*"))
          (refreshed 0))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir ws-dir)
            (with-current-buffer buf
              (setq-local major-mode 'magit-status-mode)
              (setq-local default-directory (file-name-as-directory other-dir)))
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (agent-repl--refresh-magit-status "ws1")
              (should (= refreshed 0))))
        (kill-buffer buf)
        (delete-directory ws-dir t)
        (delete-directory other-dir t)))))

(ert-deftest agent-repl-test-refresh-magit-status-skips-non-magit-buffer ()
  "refresh-magit-status does not refresh a non-magit buffer even when
default-directory matches the workspace."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "agent-magit-" t))
          (buf (generate-new-buffer " *test-non-magit*"))
          (refreshed 0))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir tmpdir)
            (with-current-buffer buf
              (setq-local major-mode 'fundamental-mode)
              (setq-local default-directory (file-name-as-directory tmpdir)))
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (agent-repl--refresh-magit-status "ws1")
              (should (= refreshed 0))))
        (kill-buffer buf)
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-refresh-magit-status-no-buffer-is-noop ()
  "refresh-magit-status is a no-op when no magit-status buffer exists for WS."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "agent-magit-" t))
          (refreshed 0))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir tmpdir)
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (agent-repl--refresh-magit-status "ws1")
              (should (= refreshed 0))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-refresh-magit-status-no-project-dir-is-noop ()
  "refresh-magit-status is a no-op when WS has no :project-dir."
  (agent-repl-test--with-clean-state
    (let ((refreshed 0))
      (cl-letf (((symbol-function 'magit-refresh)
                 (lambda (&rest _) (cl-incf refreshed))))
        (agent-repl--refresh-magit-status "ws1")
        (should (= refreshed 0))))))

(ert-deftest agent-repl-test-handle-agent-finished-refreshes-magit ()
  "handle-agent-finished calls refresh-magit-status as part of the done policy."
  (agent-repl-test--with-clean-state
    (let ((refresh-ws nil))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--maybe-notify-finished) #'ignore)
                ((symbol-function 'agent-repl--refresh-magit-status)
                 (lambda (ws) (setq refresh-ws ws))))
        (agent-repl--handle-agent-finished "ws1")
        (should (equal refresh-ws "ws1"))))))

;;;; ---- Tests: refresh-magit-status-for-dir ----

(ert-deftest agent-repl-test-refresh-magit-status-for-dir-refreshes-matching-buffer ()
  "refresh-magit-status-for-dir refreshes a magit-status buffer whose
default-directory matches DIR — directory-keyed, no workspace needed."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "agent-magit-dir-" t))
          (buf (generate-new-buffer " *test-magit-dir-match*"))
          (refreshed 0))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq-local major-mode 'magit-status-mode)
              (setq-local default-directory (file-name-as-directory tmpdir)))
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (agent-repl--refresh-magit-status-for-dir tmpdir)
              (should (= refreshed 1))))
        (kill-buffer buf)
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-refresh-magit-status-for-dir-skips-non-matching-dir ()
  "refresh-magit-status-for-dir does not refresh buffers whose default-directory
points at a different repo than the supplied DIR."
  (agent-repl-test--with-clean-state
    (let ((target-dir (make-temp-file "agent-magit-dir-target-" t))
          (other-dir (make-temp-file "agent-magit-dir-other-" t))
          (buf (generate-new-buffer " *test-magit-dir-other*"))
          (refreshed 0))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq-local major-mode 'magit-status-mode)
              (setq-local default-directory (file-name-as-directory other-dir)))
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (agent-repl--refresh-magit-status-for-dir target-dir)
              (should (= refreshed 0))))
        (kill-buffer buf)
        (delete-directory target-dir t)
        (delete-directory other-dir t)))))

(ert-deftest agent-repl-test-refresh-magit-status-for-dir-skips-non-magit-buffer ()
  "refresh-magit-status-for-dir does not refresh a non-magit buffer even when
its default-directory matches the supplied DIR."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "agent-magit-dir-nonmagit-" t))
          (buf (generate-new-buffer " *test-non-magit-dir*"))
          (refreshed 0))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq-local major-mode 'fundamental-mode)
              (setq-local default-directory (file-name-as-directory tmpdir)))
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (agent-repl--refresh-magit-status-for-dir tmpdir)
              (should (= refreshed 0))))
        (kill-buffer buf)
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-refresh-magit-status-for-dir-nil-dir-is-noop ()
  "refresh-magit-status-for-dir is a no-op when DIR is nil — guards against
callers that pass a missing target directory (e.g. unresolved master worktree)."
  (agent-repl-test--with-clean-state
    (let ((refreshed 0))
      (cl-letf (((symbol-function 'magit-refresh)
                 (lambda (&rest _) (cl-incf refreshed))))
        (agent-repl--refresh-magit-status-for-dir nil)
        (should (= refreshed 0))))))

(ert-deftest agent-repl-test-refresh-magit-status-for-dir-refreshes-multiple-matching-buffers ()
  "refresh-magit-status-for-dir refreshes every magit-status buffer whose
default-directory matches DIR — covers the post-merge case where a worktree
may have more than one stale magit-status buffer open."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "agent-magit-dir-multi-" t))
          (buf1 (generate-new-buffer " *test-magit-dir-multi-1*"))
          (buf2 (generate-new-buffer " *test-magit-dir-multi-2*"))
          (refreshed 0))
      (unwind-protect
          (progn
            (with-current-buffer buf1
              (setq-local major-mode 'magit-status-mode)
              (setq-local default-directory (file-name-as-directory tmpdir)))
            (with-current-buffer buf2
              (setq-local major-mode 'magit-status-mode)
              (setq-local default-directory (file-name-as-directory tmpdir)))
            (cl-letf (((symbol-function 'magit-refresh)
                       (lambda (&rest _) (cl-incf refreshed))))
              (agent-repl--refresh-magit-status-for-dir tmpdir)
              (should (= refreshed 2))))
        (kill-buffer buf1)
        (kill-buffer buf2)
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-refresh-magit-status-delegates-to-for-dir ()
  "refresh-magit-status forwards to refresh-magit-status-for-dir with
WS's :project-dir — guards against the wrapper drifting from the
directory-keyed primitive."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "agent-magit-delegate-" t))
          (forwarded-dir nil)
          (forwarded-ws nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir tmpdir)
            (cl-letf (((symbol-function 'agent-repl--refresh-magit-status-for-dir)
                       (lambda (dir &optional ws)
                         (setq forwarded-dir dir
                               forwarded-ws ws))))
              (agent-repl--refresh-magit-status "ws1")
              (should (equal forwarded-dir tmpdir))
              (should (equal forwarded-ws "ws1"))))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: maybe-notify-finished edge cases ----

(ert-deftest agent-repl-test-maybe-notify-first-call-no-last-time ()
  "maybe-notify-finished should notify on first call (no :last-notify-time set)."
  (agent-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda () nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest _args)
                   (cl-incf notify-count))))
        (agent-repl--maybe-notify-finished "ws1")
        (should (= notify-count 1))))))

(ert-deftest agent-repl-test-maybe-notify-stores-time ()
  "maybe-notify-finished should store :last-notify-time after notifying."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'frame-focus-state) (lambda () nil))
              ((symbol-function 'run-at-time) #'ignore))
      (agent-repl--maybe-notify-finished "ws1")
      (should (numberp (agent-repl--ws-get "ws1" :last-notify-time))))))

;;;; ---- Tests: Readiness handling ----

(ert-deftest agent-repl-test-drain-pending-prompts-empty ()
  "drain-pending-prompts should return nil when no pending prompts."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--drain-pending-prompts "ws1"))))

(ert-deftest agent-repl-test-drain-pending-prompts-sends ()
  "drain-pending-prompts should clear prompts and schedule delivery."
  (agent-repl-test--with-clean-state
    (let ((timer-scheduled nil))
      (agent-repl--ws-put "ws1" :pending-prompts '("prompt1" "prompt2"))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (setq timer-scheduled t))))
        (should (agent-repl--drain-pending-prompts "ws1"))
        (should timer-scheduled)
        (should-not (agent-repl--ws-get "ws1" :pending-prompts))))))

;; Test helper: install mocks that capture sends into SEND-SLOT (a cons
;; cell; the caller reads (car send-slot) for the reverse-chronological
;; list of prompts) and capture the scheduled verify-timer thunk into
;; TIMER-SLOT (a cons cell whose car is the most recent thunk).  Returns
;; nothing; callers invoke the body inside the cl-letf via the macro form.
(defmacro agent-repl-test--with-deliver-mocks (send-slot timer-slot &rest body)
  "Run BODY with `agent-repl--send' and `run-at-time' mocked.
SEND-SLOT is a cons cell; each send pushes its prompt onto (car SEND-SLOT).
TIMER-SLOT is a cons cell; the most recent scheduled thunk is stored at
(car TIMER-SLOT) for synchronous firing."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'agent-repl--send)
              (lambda (p _ws _force-meta on-settle)
                (setcar ,send-slot (cons p (car ,send-slot)))
                (when on-settle (funcall on-settle))))
             ((symbol-function 'run-at-time)
              (lambda (_delay _repeat fn &rest args)
                (setcar ,timer-slot (lambda () (apply fn args))))))
     ,@body))

(ert-deftest agent-repl-test-deliver-pending-prompts-sends-first ()
  "deliver-pending-prompts sends the first prompt immediately."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (let ((sent (list nil))
          (timer-slot (list nil)))
      (agent-repl-test--with-deliver-mocks sent timer-slot
        (agent-repl--deliver-pending-prompts '("a" "b") "ws1"))
      (should (equal (car sent) '("a"))))))

(ert-deftest agent-repl-test-deliver-pending-prompts-schedules-verify ()
  "deliver-pending-prompts schedules a verify timer via on-settle."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (let ((sent (list nil))
          (timer-slot (list nil)))
      (agent-repl-test--with-deliver-mocks sent timer-slot
        (agent-repl--deliver-pending-prompts '("a") "ws1"))
      (should (functionp (car timer-slot))))))

(ert-deftest agent-repl-test-deliver-pending-prompts-chains-on-ack ()
  "When the verify step sees an acknowledged state, the next prompt is sent."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (let ((sent (list nil))
          (timer-slot (list nil)))
      (agent-repl-test--with-deliver-mocks sent timer-slot
        (agent-repl--deliver-pending-prompts '("a" "b") "ws1")
        (should (equal (car sent) '("a")))
        ;; Simulate `prompt_submit' arrival between paste and verify.
        (agent-repl--ws-put "ws1" :agent-state :thinking)
        (funcall (car timer-slot))
        (should (equal (reverse (car sent)) '("a" "b")))))))

(ert-deftest agent-repl-test-deliver-pending-prompts-resends-when-not-acked ()
  "When verify sees :idle (no ack), the same prompt is resent."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (let ((sent (list nil))
          (timer-slot (list nil)))
      (agent-repl-test--with-deliver-mocks sent timer-slot
        (agent-repl--deliver-pending-prompts '("a") "ws1")
        (should (equal (car sent) '("a")))
        ;; State remains :idle — the agent never saw the paste.
        (agent-repl--ws-put "ws1" :agent-state :idle)
        (funcall (car timer-slot))
        ;; Same prompt resent.
        (should (equal (car sent) '("a" "a")))))))

(ert-deftest agent-repl-test-deliver-pending-prompts-gives-up-after-max-retries ()
  "After `agent-repl-prompt-delivery-max-retries' failed resends, give up
without sending again."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (let ((sent (list nil))
          (timer-slot (list nil))
          (agent-repl-prompt-delivery-max-retries 2))
      (agent-repl-test--with-deliver-mocks sent timer-slot
        (agent-repl--ws-put "ws1" :agent-state :idle)
        (agent-repl--deliver-pending-prompts '("a") "ws1")
        ;; First send + 2 retries = 3 total.
        (funcall (car timer-slot))   ; retry 1
        (funcall (car timer-slot))   ; retry 2
        (funcall (car timer-slot))   ; give up — no further send
        (should (equal (car sent) '("a" "a" "a")))))))

(ert-deftest agent-repl-test-deliver-pending-prompts-errors-when-not-alive ()
  "deliver-pending-prompts signals an error immediately when the frontend
session is not alive — the caller has queued prompts for a session that
is already gone (e.g. a released gui binding)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (should-error (agent-repl--deliver-pending-prompts '("a") "ws1")
                  :type 'error)))

;;;; ---- Frontend-aware delivery liveness ----

(ert-deftest agent-repl-test-pending-delivery-alive-p-gui-running ()
  "A gui workspace with a daemon session binding is deliverable."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
    (should (agent-repl--pending-delivery-alive-p "ws1" nil))))

(ert-deftest agent-repl-test-pending-delivery-alive-p-gui-unbound ()
  "A gui workspace without a daemon session binding is not deliverable."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'gui)
    (should-not (agent-repl--pending-delivery-alive-p "ws1" nil))))

(ert-deftest agent-repl-test-pending-delivery-alive-p-non-gui-live-buf ()
  "A non-gui workspace is deliverable while its passed-in buffer is live.
`:frontend' is set to an arbitrary unregistered symbol purely to exercise
the predicate's non-gui branch (`--ws-gui-frontend-p' only inspects the
plist key, never the frontend registry, so no such frontend need exist)."
  (agent-repl-test--with-clean-state
    (let ((fake-buf (generate-new-buffer " *test-alive-non-gui*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :frontend 'other)
            (should (agent-repl--pending-delivery-alive-p "ws1" fake-buf)))
        (kill-buffer fake-buf)))))

(ert-deftest agent-repl-test-pending-delivery-alive-p-non-gui-dead-buf ()
  "A non-gui workspace with a dead passed-in buffer is not deliverable."
  (agent-repl-test--with-clean-state
    (let ((fake-buf (generate-new-buffer " *test-dead-non-gui*")))
      (kill-buffer fake-buf)
      (agent-repl--ws-put "ws1" :frontend 'other)
      (should-not (agent-repl--pending-delivery-alive-p "ws1" fake-buf)))))

(ert-deftest agent-repl-test-deliver-pending-prompts-gui-sends-without-vterm ()
  "A gui workspace delivers pending prompts — no buffer is ever involved."
  (agent-repl-test--with-clean-state
    (let ((sent (list nil))
          (timer-slot (list nil)))
      (agent-repl--ws-put "ws1" :frontend 'gui)
      (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
      (agent-repl-test--with-deliver-mocks sent timer-slot
        (agent-repl--deliver-pending-prompts '("a") "ws1"))
      (should (equal (car sent) '("a"))))))

(ert-deftest agent-repl-test-deliver-pending-prompts-gui-abandons-when-released ()
  "A gui workspace whose daemon binding was released abandons at verify."
  (agent-repl-test--with-clean-state
    (let ((sent (list nil))
          (timer-slot (list nil)))
      (agent-repl--ws-put "ws1" :frontend 'gui)
      (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
      (agent-repl-test--with-deliver-mocks sent timer-slot
        (agent-repl--deliver-pending-prompts '("a" "b") "ws1")
        (should (equal (car sent) '("a")))
        ;; Session released (workspace nuke / daemon death) before verify.
        (agent-repl--ws-put "ws1" :frontend-session-id nil)
        (agent-repl--ws-put "ws1" :agent-state :thinking)
        (funcall (car timer-slot))
        ;; "b" was never sent — the binding is gone.
        (should (equal (car sent) '("a")))))))

(ert-deftest agent-repl-test-drain-pending-prompts-gui-schedules ()
  "drain-pending-prompts schedules delivery for a gui workspace."
  (agent-repl-test--with-clean-state
    (let ((scheduled-args nil))
      (agent-repl--ws-put "ws1" :frontend 'gui)
      (agent-repl--ws-put "ws1" :frontend-session-id "s_1")
      (agent-repl--ws-put "ws1" :pending-prompts '("task prompt"))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest args)
                   (setq scheduled-args args))))
        (should (agent-repl--drain-pending-prompts "ws1"))
        (should-not (agent-repl--ws-get "ws1" :pending-prompts))
        ;; Scheduled with (PENDING WS) — no buffer parameter any more.
        (should (equal scheduled-args '(("task prompt") "ws1")))))))

(ert-deftest agent-repl-test-prompt-acknowledged-p-states ()
  "prompt-acknowledged-p recognizes thinking/permission/done as ack'd."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state :thinking)
    (should (agent-repl--prompt-acknowledged-p "ws1"))
    (agent-repl--ws-put "ws1" :agent-state :permission)
    (should (agent-repl--prompt-acknowledged-p "ws1"))
    (agent-repl--ws-put "ws1" :agent-state :done)
    (should (agent-repl--prompt-acknowledged-p "ws1"))))

(ert-deftest agent-repl-test-prompt-acknowledged-p-idle-not-acked ()
  "prompt-acknowledged-p returns nil for :idle (the race state)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state :idle)
    (should-not (agent-repl--prompt-acknowledged-p "ws1"))))

(ert-deftest agent-repl-test-prompt-acknowledged-p-init-not-acked ()
  "prompt-acknowledged-p returns nil for :init (pre-ready)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :agent-state :init)
    (should-not (agent-repl--prompt-acknowledged-p "ws1"))))

(ert-deftest agent-repl-test-prompt-acknowledged-p-nil-not-acked ()
  "prompt-acknowledged-p returns nil when :agent-state is nil."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--prompt-acknowledged-p "ws1"))))

(ert-deftest agent-repl-test-show-panels-or-defer-current-ws ()
  "show-panels-or-defer should show panels when WS is current."
  (agent-repl-test--with-clean-state
    (let ((panels-opened nil))
      (cl-letf (((symbol-function 'agent-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--loading-placeholder-visible-p)
                 (lambda () nil))
                ((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (_ws) (setq panels-opened t))))
        (agent-repl--show-panels-or-defer "ws1")
        (should panels-opened)))))

(ert-deftest agent-repl-test-show-panels-or-defer-other-ws ()
  "show-panels-or-defer should set :pending-show-panels when WS is not current."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--current-ws-p) (lambda (_ws) nil)))
      (agent-repl--show-panels-or-defer "ws1")
      (should (agent-repl--ws-get "ws1" :pending-show-panels)))))

(ert-deftest agent-repl-test-open-panels-after-ready-with-pending ()
  "open-panels-after-ready should show panels when there are pending prompts."
  (agent-repl-test--with-clean-state
    (let ((shown nil))
      (cl-letf (((symbol-function 'agent-repl--drain-pending-prompts)
                 (lambda (_ws) '("prompt1")))
                ((symbol-function 'agent-repl--show-panels-or-defer)
                 (lambda (_ws) (setq shown t))))
        (agent-repl--open-panels-after-ready "ws1")
        (should shown)))))

(ert-deftest agent-repl-test-open-panels-after-ready-visible-no-reshow ()
  "open-panels-after-ready must NOT re-show already-visible panels.
Re-running the show path while panels are up can die on window
dedication mid-layout (input-only frame), so the guard skips it."
  (agent-repl-test--with-clean-state
    (let ((panels-opened nil))
      (cl-letf (((symbol-function 'agent-repl--drain-pending-prompts)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (_ws) (setq panels-opened t))))
        (agent-repl--open-panels-after-ready "ws1")
        (should-not panels-opened)))))

(ert-deftest agent-repl-test-open-panels-after-ready-pending-visible-no-reshow ()
  "The pending-prompts branch also skips the re-show when panels are visible."
  (agent-repl-test--with-clean-state
    (let ((shown nil))
      (cl-letf (((symbol-function 'agent-repl--drain-pending-prompts)
                 (lambda (_ws) '("prompt1")))
                ((symbol-function 'agent-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--panels-visible-p) (lambda () t))
                ((symbol-function 'agent-repl--show-panels-or-defer)
                 (lambda (_ws) (setq shown t))))
        (agent-repl--open-panels-after-ready "ws1")
        (should-not shown)))))

(ert-deftest agent-repl-test-open-panels-after-ready-no-pending-current ()
  "open-panels-after-ready should open panels when no pending prompts and WS is current."
  (agent-repl-test--with-clean-state
    (let ((panels-opened nil))
      (cl-letf (((symbol-function 'agent-repl--drain-pending-prompts)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--loading-placeholder-visible-p)
                 (lambda () nil))
                ((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (_ws) (setq panels-opened t))))
        (agent-repl--open-panels-after-ready "ws1")
        (should panels-opened)))))

(ert-deftest agent-repl-test-open-panels-after-ready-no-pending-other ()
  "open-panels-after-ready should NOT open panels when no pending prompts and WS is other."
  (agent-repl-test--with-clean-state
    (let ((panels-opened nil))
      (cl-letf (((symbol-function 'agent-repl--drain-pending-prompts)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--current-ws-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--frontend-dispatch-show)
                 (lambda (_ws) (setq panels-opened t))))
        (agent-repl--open-panels-after-ready "ws1")
        (should-not panels-opened)))))

(ert-deftest agent-repl-test-open-panels-after-ready-respects-persisted-inactive ()
  "open-panels-after-ready must NOT open panels when the workspace's
hydrated `:repl-state' is `:inactive' — even on the current ws — so
hide-mode survives Emacs restart."
  (agent-repl-test--with-clean-state
    (let ((panels-opened nil))
      (agent-repl--ws-put "ws1" :repl-state :inactive)
      (cl-letf (((symbol-function 'agent-repl--drain-pending-prompts)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--loading-placeholder-visible-p)
                 (lambda () nil))
                ((symbol-function 'agent-repl)
                 (lambda () (setq panels-opened t))))
        (agent-repl--open-panels-after-ready "ws1")
        (should-not panels-opened)))))

(ert-deftest agent-repl-test-open-panels-after-ready-respects-persisted-hidden ()
  "open-panels-after-ready must NOT open panels when the workspace's
hydrated `:repl-state' is `:hidden' — same skip as `:inactive' — so a
deprio-closed ws (`SPC o C') stays hidden across restart."
  (agent-repl-test--with-clean-state
    (let ((panels-opened nil))
      (agent-repl--ws-put "ws1" :repl-state :hidden)
      (cl-letf (((symbol-function 'agent-repl--drain-pending-prompts)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--current-ws-p) (lambda (_ws) t))
                ((symbol-function 'agent-repl--loading-placeholder-visible-p)
                 (lambda () nil))
                ((symbol-function 'agent-repl)
                 (lambda () (setq panels-opened t))))
        (agent-repl--open-panels-after-ready "ws1")
        (should-not panels-opened)))))

(ert-deftest agent-repl-test-open-panels-after-ready-pending-prompts-override-inactive ()
  "Pending prompts must force panel display even when persisted
`:repl-state' is `:inactive' — the user has explicitly queued work, so
they want to see the result; hide-mode is overridden."
  (agent-repl-test--with-clean-state
    (let ((shown nil))
      (agent-repl--ws-put "ws1" :repl-state :inactive)
      (cl-letf (((symbol-function 'agent-repl--drain-pending-prompts)
                 (lambda (_ws) '("prompt1")))
                ((symbol-function 'agent-repl--show-panels-or-defer)
                 (lambda (_ws) (setq shown t))))
        (agent-repl--open-panels-after-ready "ws1")
        (should shown)))))

;;;; ---- Tests: Workspace environment initialization ----

(ert-deftest agent-repl-test-initialize-ws-env-initializes-fresh ()
  "initialize-ws-env on a fresh workspace with no state file sets up
default `:active-env' and an instantiation struct for every environment
in `agent-repl--environment-keys' (`:bare-metal' alone, since the
`:sandbox' axis was retired).  The project-dir hint is used to locate
the (absent) state file."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-fresh-" t)))
      (unwind-protect
          (progn
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (eq (agent-repl--ws-get "ws1" :active-env) :bare-metal))
            (should (agent-repl-instantiation-p (agent-repl--ws-get "ws1" :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-refuses-dir-owned-by-other-live-ws ()
  "initialize-ws-env refuses to register a workspace for a :project-dir that a
DIFFERENT live workspace already owns (the duplicate-:project-dir invariant)."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-dup-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "owner" :project-dir (agent-repl--path-canonical tmpdir))
            (should-error (agent-repl--initialize-ws-env "shadow" tmpdir) :type 'error))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-allows-reinit-of-owning-ws ()
  "initialize-ws-env does NOT refuse when the ws being initialized is itself
the current owner of the dir — re-init / resurrection of the same ws is fine."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-reinit-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir (agent-repl--path-canonical tmpdir))
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (agent-repl--ws-get "ws1" :project-dir)
                           (agent-repl--path-canonical tmpdir))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-errors-when-no-root-derivable ()
  "initialize-ws-env errors when :project-dir cannot be derived from any source."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--git-root) (lambda (&optional _d) nil)))
      (should-error (agent-repl--initialize-ws-env "ws1") :type 'error))))

(ert-deftest agent-repl-test-initialize-ws-env-idempotent-recovers-partial-state ()
  "initialize-ws-env can be called on a workspace with partial state
(`:active-env' set, `:project-dir' nil) and re-initializes it correctly
using the project-dir hint.  Models the fix for the partial-init bug
where fresh-ws-env wrote :active-env without :project-dir."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-partial-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :active-env :bare-metal)
            ;; No :project-dir set — exactly the partial-init case.
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (agent-repl--ws-get "ws1" :project-dir)
                           (agent-repl--path-canonical tmpdir)))
            (should (agent-repl-instantiation-p (agent-repl--ws-get "ws1" :bare-metal))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-restores-last-prompt-time ()
  "initialize-ws-env hydrates `:last-prompt-time' from the saved state file."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-lpt-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir tmpdir)
            (agent-repl--ws-put "ws1" :active-env :bare-metal)
            (agent-repl--ws-put "ws1" :last-prompt-time 1700000000.5)
            (agent-repl--ws-put "ws1" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--state-save "ws1")
            (remhash "ws1" agent-repl--workspaces)
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (agent-repl--ws-get "ws1" :last-prompt-time)
                           1700000000.5)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-restores-last-prompt-summary-at ()
  "initialize-ws-env hydrates `:last-prompt-summary-at' so the mode-line
`X ago' prefix anchors against the original send across Emacs restarts,
rather than restarting its count from re-init time."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-lpsat-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir tmpdir)
            (agent-repl--ws-put "ws1" :active-env :bare-metal)
            (agent-repl--ws-put "ws1" :last-prompt-summary "Auth Bug Fix")
            (agent-repl--ws-put "ws1" :last-prompt-summary-at 1700000123.5)
            (agent-repl--ws-put "ws1" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--state-save "ws1")
            (remhash "ws1" agent-repl--workspaces)
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (agent-repl--ws-get "ws1" :last-prompt-summary-at)
                           1700000123.5)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-active-env-hint-honored ()
  "initialize-ws-env uses ACTIVE-ENV-HINT when provided and no state file exists.
`:bare-metal' is the only environment left on the axis, and the only value
any caller passes (see the worktree-setup call site)."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-hint-" t)))
      (unwind-protect
          (progn
            (agent-repl--initialize-ws-env "ws1" tmpdir :bare-metal)
            (should (eq (agent-repl--ws-get "ws1" :active-env) :bare-metal)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-state-file-beats-hint ()
  "Saved state supersedes the caller's ACTIVE-ENV-HINT.
The file here is a sandbox-era one, so its `:active-env' is migrated to
`:bare-metal' on the way in and its `:sandbox' instantiation is promoted
into the surviving env.  The hint path would have written a FRESH EMPTY
instantiation, so the saved session id landing on `:bare-metal' is what
proves the file — not the hint — is what hydrated the workspace (and that
the promotion supersedes the stale `:bare-metal' slot beside it)."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-override-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file (agent-repl--path-canonical tmpdir))
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :sandbox
               :bare-metal (:session-id "bm-saved")
               :sandbox (:session-id "sb-saved")))
            (agent-repl--initialize-ws-env "ws1" tmpdir :bare-metal)
            (should (eq (agent-repl--ws-get "ws1" :active-env) :bare-metal))
            (should (equal (agent-repl-instantiation-session-id
                            (agent-repl--ws-get "ws1" :bare-metal))
                           "sb-saved")))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-migrates-saved-sandbox-env ()
  "A sandbox-era state file hydrates as `:bare-metal'.
The environment axis retired `:sandbox', so a saved `:active-env :sandbox'
would fail `agent-repl--validate-ws-env' if it reached the workspace
plist verbatim; `agent-repl--migrate-saved-state' coerces it on the way in."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-migrate-env-" t)))
      (unwind-protect
          (progn
            ;; Arrange — a state file written before the sandbox was retired.
            (agent-repl--write-sexp-file
             (agent-repl--state-file (agent-repl--path-canonical tmpdir))
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :sandbox
               :sandbox (:session-id "sb-legacy")))
            ;; Act
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            ;; Assert
            (should (eq (agent-repl--ws-get "ws1" :active-env) :bare-metal)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-promotes-saved-sandbox-session-id ()
  "A sandbox-era state file keeps its claude session id through the migration.
The workspace's session id lived in the `:sandbox' instantiation, so a bare
relabel of `:active-env' would hydrate the empty `:bare-metal' slot beside it
and strand the conversation; the instantiation is PROMOTED instead."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-migrate-sid-" t)))
      (unwind-protect
          (progn
            ;; Arrange — a state file written before the sandbox was retired.
            (agent-repl--write-sexp-file
             (agent-repl--state-file (agent-repl--path-canonical tmpdir))
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :sandbox
               :sandbox (:session-id "sb-legacy")))
            ;; Act
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            ;; Assert — the session id rides the promotion into :bare-metal,
            ;; which is what `agent-repl--active-inst' now reads.
            (should (equal (agent-repl-instantiation-session-id
                            (agent-repl--ws-get "ws1" :bare-metal))
                           "sb-legacy")))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-restores-saved-tab-index ()
  "initialize-ws-env hydrates `:saved-tab-index' from the saved file so a
ws that was deprioritized at quit returns to its prior slot on restart."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-tabidx-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file tmpdir)
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :saved-tab-index 4
               :bare-metal nil))
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (eq (agent-repl--ws-get "ws1" :saved-tab-index) 4)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-restores-fork-session-id ()
  "initialize-ws-env hydrates `:fork-session-id' from the saved file so a
fork-ws whose agent session never started before quit can launch with
--fork-session on the next start."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-fork-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file tmpdir)
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :fork-session-id "fsid-abc"
               :bare-metal nil))
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (agent-repl--ws-get "ws1" :fork-session-id) "fsid-abc")))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-restores-last-prompt-summary ()
  "initialize-ws-env hydrates `:last-prompt-summary' so the tabline hint
survives restart."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-sum-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file tmpdir)
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :last-prompt-summary "refactor auth"
               :bare-metal nil))
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (agent-repl--ws-get "ws1" :last-prompt-summary)
                           "refactor auth")))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-restores-repl-state-inactive ()
  "initialize-ws-env hydrates `:repl-state :inactive' from the saved file
so hide-mode survives Emacs restart."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-rs-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file (agent-repl--path-canonical tmpdir))
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :repl-state :inactive
               :bare-metal nil))
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (eq (agent-repl--ws-get "ws1" :repl-state) :inactive)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-restores-repl-state-hidden ()
  "initialize-ws-env hydrates `:repl-state :hidden' from the saved file
so the deprio-hide marker survives Emacs restart."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-rs-hidden-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file tmpdir)
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :repl-state :hidden
               :bare-metal nil))
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (eq (agent-repl--ws-get "ws1" :repl-state) :hidden)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-restores-merge-completed-as-merged ()
  "initialize-ws-env reads `:merge-completed t' from the saved file and
restores `:repl-state :merged' so the 🔀 badge re-appears post-restart.
Absence of `:merge-failed' takes the success path."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-mc-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file (agent-repl--path-canonical tmpdir))
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :merge-completed t
               :bare-metal nil))
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (eq (agent-repl--ws-get "ws1" :merge-completed) t))
            (should (eq (agent-repl--ws-get "ws1" :repl-state) :merged))
            (should-not (agent-repl--ws-get "ws1" :merge-failed)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-restores-merge-failed-as-merge-failed ()
  "initialize-ws-env reads `:merge-failed t' from the saved file and
restores `:repl-state :merge-failed' so the ❌ badge re-appears
post-restart in the MERGED bucket (preserving the silent-failure
distinction across restarts)."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-mf-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file (agent-repl--path-canonical tmpdir))
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :merge-completed t
               :merge-failed t
               :bare-metal nil))
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (eq (agent-repl--ws-get "ws1" :merge-completed) t))
            (should (eq (agent-repl--ws-get "ws1" :merge-failed) t))
            (should (eq (agent-repl--ws-get "ws1" :repl-state) :merge-failed)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-skips-non-persistable-repl-state ()
  "initialize-ws-env ignores `:repl-state :dead' / nil from the saved file
— those are not desired-state hints and should not pin behavior on
restart (the lazy-start path applies its defaults instead)."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-rs-dead-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file (agent-repl--path-canonical tmpdir))
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :repl-state :dead
               :bare-metal nil))
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (null (agent-repl--ws-get "ws1" :repl-state))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-restores-priority-from-state ()
  "initialize-ws-env hydrates `:priority' from the saved state file."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-pri-" t)))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file
             (agent-repl--state-file (agent-repl--path-canonical tmpdir))
             `(:project-dir ,(agent-repl--path-canonical tmpdir)
               :active-env :bare-metal
               :priority "p1"
               :bare-metal nil))
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (agent-repl--ws-get "ws1" :priority) "p1")))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-initialize-ws-env-priority-fallback-to-plist ()
  "With no saved priority, existing plist `:priority' is preserved (covers
`agent-repl-set-priority' running before any state-save)."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-init-pri-fb-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :priority "p2")
            (agent-repl--initialize-ws-env "ws1" tmpdir)
            (should (equal (agent-repl--ws-get "ws1" :priority) "p2")))
        (delete-directory tmpdir t)))))

;;;; ---- Tests: agent-running-p ----

;; `--agent-running-p' asks the workspace's OWN frontend.  While it looked
;; only at `:vterm-buffer' it answered "not running" for every gui
;; workspace, which silently disarmed every guard keyed to it.

(ert-deftest agent-repl-test-agent-running-p-asks-an-arbitrary-frontend ()
  "Dispatch asks WHATEVER frontend struct WS resolves to, not a hardcoded one.
Proven by resolving WS to a fake, unregistered frontend struct and
checking its `running-p-fn' — not gui's — is the one invoked."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let* ((checked-ws nil)
           (fake-frontend (agent-repl-frontend-create
                           :name 'fake
                           :running-p-fn (lambda (ws) (setq checked-ws ws) t))))
      (cl-letf (((symbol-function 'agent-repl--ws-frontend)
                 (lambda (_ws) fake-frontend)))
        ;; Act / Assert
        (should (agent-repl--agent-running-p "my-ws"))
        (should (equal checked-ws "my-ws"))))))

(ert-deftest agent-repl-test-agent-running-p-asks-the-gui-frontend ()
  "A gui workspace's live daemon session counts as running.
This is the bug the registry dispatch fixes: keyed to `:vterm-buffer',
this answered nil for EVERY gui workspace, disarming the backend-switch
guard, the kill-before-workspace-delete advice, and the status poll."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((checked-ws nil))
      (cl-letf (((symbol-function 'agent-repl--gui-running-p)
                 (lambda (ws) (setq checked-ws ws) t)))
        ;; Act / Assert
        (should (agent-repl--agent-running-p "gui-ws"))
        (should (equal checked-ws "gui-ws"))))))

(ert-deftest agent-repl-test-agent-running-p-gui-without-a-session-is-not-running ()
  "A gui workspace with no daemon session is not running."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--gui-running-p) (lambda (_ws) nil)))
      ;; Act / Assert
      (should-not (agent-repl--agent-running-p "gui-ws")))))

(ert-deftest agent-repl-test-agent-running-p-nil-ws-uses-current ()
  "agent-running-p with nil ws should fall back to +workspace-current-name."
  (agent-repl-test--with-clean-state
    (let ((checked-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
                ((symbol-function 'agent-repl--gui-running-p)
                 (lambda (ws) (setq checked-ws ws) nil)))
        (should-not (agent-repl--agent-running-p))
        (should (equal checked-ws "current-ws"))))))

;;;; ---- Tests: session edge cases (status transitions .md) ----

(ert-deftest agent-repl-test-handle-agent-finished-second-notify-debounced ()
  "Two calls to handle-agent-finished within 2s should only produce one notification."
  (agent-repl-test--with-clean-state
    (let ((notify-count 0))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/fake")
      (cl-letf (((symbol-function 'agent-repl--notify)
                 (lambda (&rest _) (cl-incf notify-count)))
                ((symbol-function 'frame-focus-state) (lambda () nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args)
                   (apply fn args)))
                ((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                ((symbol-function 'agent-repl--current-ws-p) (lambda (_ws) nil)))
        ;; First call — should notify
        (agent-repl--handle-agent-finished "ws1")
        (should (= notify-count 1))
        ;; Second call within 2s window — should be debounced
        (agent-repl--handle-agent-finished "ws1")
        (should (= notify-count 1))))))

;;;; ---- Tests: set-session-id ----

(ert-deftest agent-repl-test-set-session-id-persists-to-disk ()
  "set-session-id mutates the active instantiation AND writes state to disk.
Persistence-on-capture is what makes a hook-delivered SID durable
through an Emacs crash — without it, the SID would only reach
.agent-repl-state at graceful teardown."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "test-set-sid-" t)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws" :project-dir tmpdir)
            (agent-repl--ws-put "ws" :active-env :bare-metal)
            (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
            (agent-repl--set-session-id "ws" "captured-sid")
            (let* ((file (agent-repl--state-file tmpdir))
                   (data (agent-repl--read-sexp-file file)))
              (should (equal (plist-get (plist-get data :bare-metal) :session-id)
                             "captured-sid"))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-set-session-id-no-project-dir-does-not-error ()
  "set-session-id is safe when :project-dir is nil; state-save logs and skips."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :active-env :bare-metal)
    (agent-repl--ws-put "ws" :bare-metal (make-agent-repl-instantiation))
    (agent-repl--set-session-id "ws" "captured-sid")
    (should (equal (agent-repl-instantiation-session-id
                    (agent-repl--ws-get "ws" :bare-metal))
                   "captured-sid"))))

;;;; ---- agent-repl--apply-display-state ----

(ert-deftest agent-repl-test-apply-display-state-sets-priority-from-saved ()
  "apply-display-state copies :priority out of the saved plist onto the ws."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state "ws1" '(:priority "p1"))
    (should (equal (agent-repl--ws-get "ws1" :priority) "p1"))))

(ert-deftest agent-repl-test-apply-display-state-priority-falls-back-to-plist ()
  "apply-display-state keeps the in-memory :priority when saved carries none."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :priority "mem")
    (agent-repl--apply-display-state "ws1" '(:project-dir "/x"))
    (should (equal (agent-repl--ws-get "ws1" :priority) "mem"))))

(ert-deftest agent-repl-test-apply-display-state-sets-model-from-saved ()
  "apply-display-state restores `:model' from the saved plist so a booted
session re-launches under the persisted model (e.g. fable)."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state "ws1" '(:model "claude-fable-5"))
    (should (equal (agent-repl--ws-get "ws1" :model) "claude-fable-5"))))

(ert-deftest agent-repl-test-apply-display-state-model-falls-back-to-plist ()
  "apply-display-state keeps the in-memory `:model' (the generation model)
when the saved plist carries none."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :model "opus")
    (agent-repl--apply-display-state "ws1" '(:project-dir "/x"))
    (should (equal (agent-repl--ws-get "ws1" :model) "opus"))))

(ert-deftest agent-repl-test-apply-display-state-sets-config-dir-override-from-saved ()
  "apply-display-state restores :config-dir-override so a switched workspace
does not silently revert to its path-computed account after a restart."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state "ws1" '(:config-dir-override "~/.claude-chesscom"))
    (should (equal (agent-repl--ws-get "ws1" :config-dir-override) "~/.claude-chesscom"))))

(ert-deftest agent-repl-test-apply-display-state-config-dir-override-falls-back-to-plist ()
  "apply-display-state keeps the in-memory :config-dir-override when saved
carries none (an account_changed_ sentinel handled before any save)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :config-dir-override :default)
    (agent-repl--apply-display-state "ws1" '(:project-dir "/x"))
    (should (eq (agent-repl--ws-get "ws1" :config-dir-override) :default))))

(ert-deftest agent-repl-test-apply-display-state-sets-backend-from-saved ()
  "apply-display-state copies :backend out of the saved plist onto the ws."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state "ws1" '(:backend codex))
    (should (eq (agent-repl--ws-get "ws1" :backend) 'codex))))

(ert-deftest agent-repl-test-apply-display-state-backend-falls-back-to-plist ()
  "apply-display-state keeps the in-memory :backend when saved carries none."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :backend 'codex)
    (agent-repl--apply-display-state "ws1" '(:project-dir "/x"))
    (should (eq (agent-repl--ws-get "ws1" :backend) 'codex))))

(ert-deftest agent-repl-test-apply-display-state-restores-a-chosen-frontend ()
  "apply-display-state restores a DELIBERATELY chosen frontend."
  ;; Arrange / Act — the shape `agent-repl--ws-choose-frontend' persists.
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state "ws1" '(:frontend vterm :frontend-explicit t))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :frontend) 'vterm))
    (should (agent-repl--ws-get "ws1" :frontend-explicit))))

(ert-deftest agent-repl-test-apply-display-state-ignores-an-incidental-frontend ()
  "apply-display-state IGNORES a saved frontend that was never chosen.
Every workspace predating the gui carries an incidental `:frontend vterm'
stamp from the vterm boot; honoring it would pin those workspaces to vterm
forever instead of letting them restore under `agent-repl-default-frontend'."
  ;; Arrange / Act — an old state file: a frontend, but no explicit marker.
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state "ws1" '(:frontend vterm))
    ;; Assert — nothing pinned, so resolution falls to the default (the gui).
    (should-not (agent-repl--ws-get "ws1" :frontend))
    (should (agent-repl--ws-gui-frontend-p "ws1"))))

(ert-deftest agent-repl-test-apply-display-state-no-saved-frontend-leaves-plist ()
  "apply-display-state leaves an in-memory `:frontend' alone when saved has none."
  ;; Arrange — a live workspace already stamped by its running vterm.
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :frontend 'vterm)
    ;; Act
    (agent-repl--apply-display-state "ws1" '(:project-dir "/x"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :frontend) 'vterm))))

(ert-deftest agent-repl-test-apply-display-state-restores-backend-session-stash ()
  "apply-display-state hydrates `:backend-session-stash' from the saved plist."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state
     "ws1" '(:backend-session-stash (claude (:bare-metal "claude-sid"))))
    (should (equal (plist-get (plist-get
                               (agent-repl--ws-get "ws1" :backend-session-stash)
                               'claude)
                              :bare-metal)
                   "claude-sid"))))

(ert-deftest agent-repl-test-apply-display-state-no-backend-session-stash ()
  "apply-display-state leaves `:backend-session-stash' unset when saved has none."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state "ws1" '(:project-dir "/x"))
    (should-not (agent-repl--ws-get "ws1" :backend-session-stash))))

(ert-deftest agent-repl-test-apply-display-state-restores-repl-state-inactive ()
  "apply-display-state restores a persistable :repl-state (:inactive)."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state "ws1" '(:repl-state :inactive))
    (should (eq (agent-repl--ws-get "ws1" :repl-state) :inactive))))

(ert-deftest agent-repl-test-apply-display-state-skips-non-persistable-repl-state ()
  "apply-display-state ignores a non-persistable :repl-state (:dead)."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state "ws1" '(:repl-state :dead))
    (should-not (agent-repl--ws-get "ws1" :repl-state))))

(ert-deftest agent-repl-test-apply-display-state-merge-completed-sets-merged ()
  "apply-display-state maps a saved :merge-completed t to :repl-state :merged."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state "ws1" '(:merge-completed t))
    (should (agent-repl--ws-get "ws1" :merge-completed))
    (should (eq (agent-repl--ws-get "ws1" :repl-state) :merged))))

(ert-deftest agent-repl-test-apply-display-state-merge-failed-sets-merge-failed ()
  "apply-display-state maps a saved :merge-failed t to :repl-state :merge-failed."
  (agent-repl-test--with-clean-state
    (agent-repl--apply-display-state "ws1" '(:merge-completed t :merge-failed t))
    (should (eq (agent-repl--ws-get "ws1" :repl-state) :merge-failed))))

(ert-deftest agent-repl-test-apply-display-state-nil-saved-keeps-plist-priority ()
  "apply-display-state with a nil saved plist preserves the in-memory :priority."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :priority "mem")
    (agent-repl--apply-display-state "ws1" nil)
    (should (equal (agent-repl--ws-get "ws1" :priority) "mem"))))

;;;; ---- agent-repl--load-display-state ----

(ert-deftest agent-repl-test-load-display-state-applies-priority-from-file ()
  "load-display-state reads the state file and applies :priority to the ws."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "agent-repl-lds-" t))))
      (unwind-protect
          (progn
            (agent-repl-test--seed-file
             (agent-repl--state-file tmpdir)
             (prin1-to-string '(:priority "p9")))
            (cl-letf (((symbol-function 'force-mode-line-update)
                       (lambda (&optional _all) nil)))
              (agent-repl--load-display-state "ws1" tmpdir)
              (should (equal (agent-repl--ws-get "ws1" :priority) "p9"))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-load-display-state-skips-when-active-env-set ()
  "load-display-state is a no-op once the ws is env-initialized (:active-env set).
An env-initialized workspace already carries display state in memory, so the
disk read must be skipped rather than re-read."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "agent-repl-lds-" t))))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :active-env :bare-metal)
            (agent-repl-test--seed-file
             (agent-repl--state-file tmpdir)
             (prin1-to-string '(:priority "disk")))
            (agent-repl--load-display-state "ws1" tmpdir)
            (should-not (agent-repl--ws-get "ws1" :priority)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-test-load-display-state-nil-ws-noop ()
  "load-display-state does not error when WS is nil."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--load-display-state nil "/tmp"))))

(ert-deftest agent-repl-test-load-display-state-nil-root-noop ()
  "load-display-state sets nothing and does not error when PROJECT-ROOT is nil."
  (agent-repl-test--with-clean-state
    (agent-repl--load-display-state "ws1" nil)
    (should-not (agent-repl--ws-get "ws1" :priority))))

(ert-deftest agent-repl-test-load-display-state-missing-state-file-noop ()
  "load-display-state is a no-op when the project has no state file on disk."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "agent-repl-lds-" t))))
      (unwind-protect
          (progn
            (agent-repl--load-display-state "ws1" tmpdir)
            (should-not (agent-repl--ws-get "ws1" :priority)))
        (delete-directory tmpdir t)))))

;;; test-session.el ends here
