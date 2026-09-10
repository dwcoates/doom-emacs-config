;;; test-keybindings.el --- ERT tests for agent-repl keybindings.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-keybindings.el -f ert-run-tests-batch-and-exit
;;
;; `map!' is a no-op stub under `emacs -Q' (test-helpers.el), so the BINDINGS
;; themselves are not observable here and are not what this file asserts.
;; What it asserts is the half that is: every command a binding names is
;; DEFINED, and every command the overhaul retired is NOT -- a binding
;; pointing at a deleted command is dead on the first keypress, and that is
;; exactly the breakage this suite exists to catch.
;;
;; The two helpers that exist only to serve a binding are tested directly.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Every bound command is defined ----

(ert-deftest agent-repl-test-keybindings-workspace-verbs-are-defined ()
  "The SPC j / SPC TAB workspace verbs all exist."
  (dolist (cmd '(agent-repl-close-workspace
                 agent-repl-kill-workspace
                 agent-repl-nuke-workspace
                 agent-repl-open-workspace
                 agent-repl-merge-workspace
                 agent-repl-restart-workspace
                 agent-repl-create-workspace
                 agent-repl-fork-workspace
                 agent-repl-set-priority))
    (should (commandp cmd))))

(ert-deftest agent-repl-test-keybindings-admin-verbs-are-defined ()
  "The SPC j a daemon-admin commands all exist."
  (dolist (cmd '(agent-repl-daemon-health
                 agent-repl-session-health
                 agent-repl-daemon-shutdown-schedule
                 agent-repl-daemon-shutdown-cancel
                 agent-repl-daemon-shutdown-now
                 agent-repl-merge-queue-pause
                 agent-repl-merge-queue-resume
                 agent-repl-merge-queue-evict))
    (should (commandp cmd))))

(ert-deftest agent-repl-test-keybindings-oneshot-commands-are-defined ()
  "Both one-shot finish arms have a command behind them."
  (dolist (cmd '(agent-repl-create-oneshot-self-merge
                 agent-repl-create-oneshot-open-pr
                 agent-repl-create-oneshot-open-pr-reviewed))
    (should (commandp cmd))))

(ert-deftest agent-repl-test-keybindings-canned-prompt-commands-are-defined ()
  "The SPC j prompt families all exist."
  (dolist (cmd '(agent-repl-explain
                 agent-repl-explain-prompt
                 agent-repl-update-pr
                 agent-repl-rebase-onto-origin-master
                 agent-repl-create-or-update-pr
                 agent-repl-create-or-update-pr-no-self-certified
                 agent-repl-create-or-update-pr-paste
                 agent-repl-create-or-update-pr-no-self-certified-paste))
    (should (commandp cmd))))

(ert-deftest agent-repl-test-keybindings-diff-scope-commands-are-defined ()
  "Every diff-family scope bound under SPC j exists."
  (dolist (family '("explain-diff" "run-tests" "run-lint" "run-all"
                    "test-quality" "test-coverage"))
    (dolist (scope '("worktree" "staged" "uncommitted" "head" "branch"))
      (should (commandp (intern (format "agent-repl-%s-%s" family scope)))))))

(ert-deftest agent-repl-test-keybindings-navigation-commands-are-defined ()
  "The navigation and utility commands bound outside SPC j exist."
  (dolist (cmd '(agent-repl-switch-to-project
                 agent-repl-add-project-workspace
                 agent-repl-open-most-recent-workspace
                 agent-repl-switch-left
                 agent-repl-switch-right
                 agent-repl-switch-to-workspace
                 agent-repl-copy-reference
                 agent-repl-copy-workspace-name
                 agent-repl-revert-and-eval-buffer
                 agent-repl-reload-config))
    (should (commandp cmd))))

(ert-deftest agent-repl-test-keybindings-composer-commands-are-defined ()
  "The composer-local commands input.el binds exist."
  (dolist (cmd '(agent-repl-send
                 agent-repl-send-with-postfix
                 agent-repl-send-with-prefix
                 agent-repl-discard-input
                 agent-repl-queue-deferred-prompt))
    (should (commandp cmd))))

;;;; ---- Every retired command is gone ----

(ert-deftest agent-repl-test-keybindings-retired-commands-are-gone ()
  "No binding may name a command the overhaul retired.
A binding pointing at a deleted command is dead on the first keypress,
so the absence is asserted rather than assumed."
  (dolist (cmd '(agent-repl-rename-workspace
                 agent-repl-hibernate-workspace
                 agent-repl-explain-config
                 agent-repl-workspace-pull-to-front
                 agent-repl-workspace-switch-to-0
                 agent-repl-workspace-switch-to-final
                 agent-repl-interrupt
                 agent-repl-paste-clipboard
                 agent-repl-kill-all-workspaces
                 agent-repl-create-doom-oneshot-workspace
                 agent-repl-create-worktree-workspace
                 agent-repl-workspace-merge-current-into-source
                 agent-repl-output-next-prompt
                 agent-repl-sidebar-nav-next
                 ;; The jump-chord INSTALLER died with client-authored
                 ;; ordering and stays dead: the numerals are a keymap of
                 ;; data now, not a function that rewrites Doom's bindings.
                 agent-repl--install-workspace-jump-overrides))
    (should-not (fboundp cmd))))

(ert-deftest agent-repl-test-keybindings-priority-helpers-are-gone ()
  "The local priority picker moved into verbs.el with the verb it serves."
  (dolist (sym '(agent-repl--read-priority
                 agent-repl--decorate-priority-candidate
                 agent-repl--priority-remove-label))
    (should-not (or (fboundp sym) (boundp sym)))))

(ert-deftest agent-repl-test-keybindings-installs-no-kill-advice ()
  "The `+workspace/kill' advice is gone.
It used to tear an agent session down before the perspective went away,
because Emacs owned the session. It does not: CloseWorkspace is a VIEW
act that leaves the session running and KillWorkspace is the forced
death, both the daemon's. An advice killing a session behind a
persp-kill would be Emacs inventing a lifecycle decision the contract
gives it no say in."
  (should-not (fboundp 'agent-repl--kill-before-workspace-delete)))

(ert-deftest agent-repl-test-keybindings-numerals-name-the-drawn-tab-commands ()
  "Every numeral chord resolves to the command for its OWN tab slot.
Unlike the rest of this file the numerals are plain keymap data rather
than a `map!' form, so the binding itself is observable here -- which is
half of why they live on a keymap of the module's own."
  (dotimes (i agent-repl-switch-numeral-count)
    (let ((n (1+ i)))
      (should (eq (lookup-key agent-repl-workspace-numerals-mode-map
                              (kbd (format "M-%d" n)))
                  (intern (format "agent-repl-switch-to-workspace-%d" n))))
      (should (commandp (intern (format "agent-repl-switch-to-workspace-%d" n)))))))

(ert-deftest agent-repl-test-keybindings-numerals-shadow-dooms-own ()
  "A numeral reaches OUR command even with Doom's binding in `global-map'.
Doom binds `M-1' .. `M-0' to `+workspace/switch-to-N', which indexes
persp-mode's perspective list rather than the drawn tab bar. The
shadowing has to be structural: the mode's keymap is consulted before
`global-map' whatever order the two were installed in."
  (let ((saved (current-global-map)))
    (unwind-protect
        (progn
          (use-global-map (copy-keymap saved))
          (define-key (current-global-map) (kbd "M-2") '+workspace/switch-to-1)
          (agent-repl-workspace-numerals-mode 1)
          (should (eq (key-binding (kbd "M-2")) 'agent-repl-switch-to-workspace-2)))
      (use-global-map saved))))

(ert-deftest agent-repl-test-keybindings-leaves-m-0-to-doom ()
  "`M-0' is the one numeral the module does not claim."
  (should-not (lookup-key agent-repl-workspace-numerals-mode-map (kbd "M-0"))))

;;;; ---- The helpers a binding needs ----

(ert-deftest agent-repl-test-keybindings-read-known-workspace-defaults-to-current ()
  "RET picks the obvious target: the workspace the user is in."
  (let ((seen-default nil))
    (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("a" "b")))
              ((symbol-function 'agent-repl--ws-current-name) (lambda () "b"))
              ((symbol-function 'completing-read)
               (lambda (_p _c &optional _pr _rm _ii _h default)
                 (setq seen-default default) "b")))
      (should (equal (agent-repl--read-known-workspace "Pick: ") "b"))
      (should (equal seen-default "b")))))

(ert-deftest agent-repl-test-keybindings-read-known-workspace-omits-a-tombstone ()
  "A killed workspace must not surface in an interactive picker."
  (let ((offered nil))
    (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () '("live")))
              ((symbol-function 'agent-repl--ws-current-name) (lambda () nil))
              ((symbol-function 'completing-read)
               (lambda (_p candidates &rest _) (setq offered candidates) "live")))
      (agent-repl--read-known-workspace "Pick: ")
      (should (equal offered '("live"))))))

(ert-deftest agent-repl-test-keybindings-read-known-workspace-refuses-with-none ()
  "With nothing registered there is nothing to pick."
  (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () nil))
            ((symbol-function 'agent-repl--ws-current-name) (lambda () nil)))
    (should-error (agent-repl--read-known-workspace "Pick: ") :type 'user-error)))

(ert-deftest agent-repl-test-keybindings-reload-prefers-the-worktree-config ()
  "Reloading inside a worktree picks up THAT worktree's checkout."
  (agent-repl-test--with-clean-state
    (let* ((root (make-temp-file "agent-repl-reload" t))
           (config (expand-file-name "modules/app/agent-repl/config.el" root)))
      (unwind-protect
          (progn
            (make-directory (file-name-directory config) t)
            (with-temp-file config (insert ";; worktree copy"))
            (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
              (agent-repl--ws-put "ws1" :project-dir root)
              (should (equal (agent-repl--reload-config-file) config))))
        (delete-directory root t)))))

(ert-deftest agent-repl-test-keybindings-reload-falls-back-to-the-load-path ()
  "A project that does not vendor the module reloads the original copy."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--config-file "/tmp/original/config.el"))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1")))
        (agent-repl--ws-put "ws1" :project-dir "/tmp/no-module-here")
        (should (equal (agent-repl--reload-config-file) "/tmp/original/config.el"))))))

(ert-deftest agent-repl-test-keybindings-revert-and-eval-reverts-then-evals ()
  "The fast config reload reverts from disk BEFORE evaluating."
  (let ((order nil))
    (cl-letf (((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil))
              ((symbol-function 'revert-buffer) (lambda (&rest _) (push 'revert order)))
              ((symbol-function 'eval-buffer) (lambda (&rest _) (push 'eval order))))
      (with-temp-buffer (agent-repl-revert-and-eval-buffer))
      (should (equal (reverse order) '(revert eval))))))

(provide 'test-keybindings)

;;; test-keybindings.el ends here
