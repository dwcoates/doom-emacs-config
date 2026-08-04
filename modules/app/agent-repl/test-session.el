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
      (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda () nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest _args)
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
  "maybe-notify-finished should NOT send desktop notification when Emacs is focused."
  (agent-repl-test--with-clean-state
    (let ((notify-count 0))
      (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda () t))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest _args)
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
      (cl-letf (((symbol-function 'agent-repl--maybe-notify-finished) #'ignore)
                ((symbol-function 'agent-repl--ws-set-agent-state)
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
      (cl-letf (((symbol-function 'agent-repl--maybe-notify-finished) #'ignore)
                ((symbol-function 'get-buffer-window)
                 (lambda (&rest _) 'some-window))
                ((symbol-function 'agent-repl--ws-set-agent-state)
                 (lambda (ws state)
                   (when (eq state :done) (setq done-set ws)))))
        (agent-repl--mark-agent-done "ws1")
        (should (equal done-set "ws1"))))))

(ert-deftest agent-repl-test-mark-agent-done-records-no-viewed-ack ()
  "mark-agent-done records no viewed-acknowledgment.
`:done', `:ready' and `:idle' are all READY under the five-color
vocabulary, so there is no decay left for an acknowledgment to pace."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--maybe-notify-finished) #'ignore)
              ((symbol-function 'agent-repl--current-ws-p) (lambda (_ws) t)))
      (agent-repl--mark-agent-done "ws1")
      (should (null (agent-repl--ws-get "ws1" :done-acked)))
      (should (null (agent-repl--ws-get "ws1" :done-acked-at))))))

(ert-deftest agent-repl-test-mark-agent-done-notifies ()
  "mark-agent-done fires the finished notification for WS, so every
transition to :done — not just the Stop-hook completion path — notifies
the user when the frame is unfocused."
  (agent-repl-test--with-clean-state
    (let (notified)
      (cl-letf (((symbol-function 'agent-repl--current-ws-p) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--maybe-notify-finished)
                 (lambda (ws) (setq notified ws))))
        (agent-repl--mark-agent-done "ws1")
        (should (equal notified "ws1"))))))

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
                 (lambda (fmt &rest _args)
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
      (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda () nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest _args)
                   (cl-incf notify-count))))
        (agent-repl--maybe-notify-finished "ws1")
        (should (= notify-count 1))))))

(ert-deftest agent-repl-test-maybe-notify-stores-time ()
  "maybe-notify-finished should store :last-notify-time after notifying."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda () nil))
              ((symbol-function 'run-at-time) #'ignore))
      (agent-repl--maybe-notify-finished "ws1")
      (should (numberp (agent-repl--ws-get "ws1" :last-notify-time))))))

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
