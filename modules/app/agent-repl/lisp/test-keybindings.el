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

;;;; ---- The restart binding says what it does ----

(defconst agent-repl-test-keybindings--file
  (expand-file-name "keybindings.el" (file-name-directory
                                      (or load-file-name buffer-file-name)))
  "The keybindings.el under test, resolved at load time.")

(defun agent-repl-test-keybindings--source ()
  "Return keybindings.el's text; `map!' is a stub, so the source is what is observable."
  (with-temp-buffer
    (insert-file-contents agent-repl-test-keybindings--file)
    (buffer-string)))

(ert-deftest agent-repl-test-keybindings-restart-description-names-shim-and-page ()
  "`SPC o C-c' is described as restarting the shim and the page."
  (should (string-match-p
           (regexp-quote "\"Restart workspace (shim + page)\" \"o C-c\"")
           (agent-repl-test-keybindings--source))))

(ert-deftest agent-repl-test-keybindings-restart-description-has-no-force-prefix ()
  "There is no forced restart, so no description offers `C-u' for one."
  (should-not (string-match-p (regexp-quote "C-u = force")
                              (agent-repl-test-keybindings--source))))

;;;; ---- Every bound command is defined ----

(ert-deftest agent-repl-test-keybindings-workspace-verbs-are-defined ()
  "The SPC j / SPC TAB workspace verbs all exist."
  (dolist (cmd '(agent-repl-close-workspace
                 agent-repl-kill-workspace
                 agent-repl-nuke-workspace
                 agent-repl-open-workspace
                 agent-repl-finish-workspace
                 agent-repl-restart-workspace
                 agent-repl-create-workspace
                 agent-repl-create-workspace-static
                 agent-repl-fork-workspace-into-primary
                 agent-repl-fork-workspace-into-primary-static
                 agent-repl-fork-workspace
                 agent-repl-fork-workspace-static
                 agent-repl-register-repository
                 agent-repl-set-priority
                 agent-repl-hide-workspace
                 agent-repl-show-hidden-workspace
                 agent-repl-send-hidden-keepalives))
    (should (commandp cmd))))

(ert-deftest agent-repl-test-keybindings-spc-j-R-sends-the-hidden-keepalives ()
  "`SPC j R' sends a keep-alive to every hidden workspace."
  (should (string-match-p
           (regexp-quote "\"R\"  #'agent-repl-send-hidden-keepalives")
           (agent-repl-test-keybindings--source))))

(ert-deftest agent-repl-test-keybindings-spc-j-C-r-reloads-the-config ()
  "`SPC j C-r' reloads the agent-repl config, `SPC j R' being the keep-alives."
  (should (string-match-p
           (regexp-quote "\"C-r\" #'agent-repl-reload-config")
           (agent-repl-test-keybindings--source))))

(ert-deftest agent-repl-test-keybindings-spc-j-C-return-pushes-a-release ()
  "`SPC j C-RET' pushes master as a release, bound as the GUI's `C-<return>'.
`C-RET' would read as `C-m', which no GUI Control+Return ever sends."
  (should (string-match-p
           (regexp-quote "\"C-<return>\" #'agent-repl-push-release")
           (agent-repl-test-keybindings--source))))

(ert-deftest agent-repl-test-keybindings-push-release-is-defined ()
  "The release push the binding names is an interactive command."
  (should (commandp 'agent-repl-push-release)))

(ert-deftest agent-repl-test-keybindings-admin-verbs-are-defined ()
  "The SPC j a daemon-admin commands all exist."
  (dolist (cmd '(agent-repl-daemon-health
                 agent-repl-session-health
                 agent-repl-daemon-shutdown-schedule
                 agent-repl-daemon-shutdown-cancel
                 agent-repl-daemon-shutdown-now
                 agent-repl-merge-queue-pause
                 agent-repl-merge-queue-resume
                 agent-repl-merge-queue-evict
                 agent-repl-persistent-wifi-mode-toggle))
    (should (commandp cmd))))

(ert-deftest agent-repl-test-keybindings-oneshot-command-is-defined ()
  "The ONE one-shot command exists: there is no finish to choose between."
  (should (commandp 'agent-repl-create-oneshot)))

(ert-deftest agent-repl-test-keybindings-oneshot-for-current-workspace-command-is-defined ()
  "The SPC TAB C-o one-shot about the current workspace exists."
  (should (commandp 'agent-repl-create-oneshot-for-current-workspace)))

(ert-deftest agent-repl-test-keybindings-explanation-engine-oneshot-command-is-defined ()
  "The SPC TAB O one-shot in the explanation-engine repository exists."
  (should (commandp 'agent-repl-create-explanation-engine-oneshot)))

(ert-deftest agent-repl-test-keybindings-help-workspace-is-gone ()
  "The SPC o h help workspace died with the /agent-repl-help skill."
  (should-not (fboundp 'agent-repl-create-help-workspace))
  (should-not (boundp 'agent-repl-help-preamble)))

(ert-deftest agent-repl-test-keybindings-oneshot-finish-commands-are-gone ()
  "The per-finish one-shot commands died with the finish choice."
  (dolist (cmd '(agent-repl-create-oneshot-self-merge
                 agent-repl-create-oneshot-open-pr
                 agent-repl-create-oneshot-open-pr-reviewed))
    (should-not (fboundp cmd))))

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
                 agent-repl-switch-to-any-workspace
                 agent-repl-add-project-workspace
                 agent-repl-open-most-recent-workspace
                 agent-repl-switch-left
                 agent-repl-switch-right
                 agent-repl-switch-older
                 agent-repl-switch-newer
                 agent-repl-switch-to-workspace
                 agent-repl-copy-reference
                 agent-repl-copy-workspace-name
                 agent-repl-revert-and-eval-buffer
                 agent-repl-reload-config))
    (should (commandp cmd))))

(ert-deftest agent-repl-test-keybindings-recency-walk-chords-are-bound ()
  "`s-[' walks to the workspace viewed before, `s-]' back toward the most recent."
  (should (string-match-p
           (regexp-quote "(map! \"s-[\" #'agent-repl-switch-older\n      \"s-]\" #'agent-repl-switch-newer)")
           (agent-repl-test-keybindings--source))))

(ert-deftest agent-repl-test-keybindings-composer-commands-are-defined ()
  "The composer-local commands input.el binds exist."
  (dolist (cmd '(agent-repl-send
                 agent-repl-send-with-postfix
                 agent-repl-send-with-prefix
                 agent-repl-discard-input
                 agent-repl-queue-deferred-prompt))
    (should (commandp cmd))))

;;;; ---- Doom's workspace navigation is remapped to the request path ----

(ert-deftest agent-repl-test-keybindings-doom-switch-left-is-remapped ()
  "Doom's `+workspace/switch-left' runs the drawn-bar request instead."
  (should (eq (lookup-key agent-repl-workspace-numerals-mode-map
                          [remap +workspace/switch-left])
              'agent-repl-switch-left)))

(ert-deftest agent-repl-test-keybindings-doom-switch-right-is-remapped ()
  "Doom's `+workspace/switch-right' runs the drawn-bar request instead."
  (should (eq (lookup-key agent-repl-workspace-numerals-mode-map
                          [remap +workspace/switch-right])
              'agent-repl-switch-right)))

(ert-deftest agent-repl-test-keybindings-doom-switch-to-n-is-remapped-one-based ()
  "Doom's 0-based `+workspace/switch-to-N' runs slot N+1 of the drawn bar."
  (should (eq (lookup-key agent-repl-workspace-numerals-mode-map
                          [remap +workspace/switch-to-0])
              'agent-repl-switch-to-workspace-1)))

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
                 ;; The kind-specific feed selection steps gave way to the
                 ;; one turn-boundary walk (`M-s-j' / `M-s-k').
                 agent-repl-response-select-prev
                 agent-repl-response-select-next
                 agent-repl-prompt-select-prev
                 agent-repl-prompt-select-next
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

(ert-deftest agent-repl-test-keybindings-numerals-command-digits-name-the-same-commands ()
  "Command-digit (`s-N\=') runs the SAME slot command as Option-digit (`M-N\=').
On macOS the Command key is Super; Doom binds `s-1\=' .. `s-9\=' to its
persp-indexed switch, so an unclaimed Command-digit landed on the wrong
workspace while Command-{ / Command-} walked the bar correctly."
  (dotimes (i agent-repl-switch-numeral-count)
    (let ((n (1+ i)))
      (should (eq (lookup-key agent-repl-workspace-numerals-mode-map (kbd (format "s-%d" n)))
                  (lookup-key agent-repl-workspace-numerals-mode-map (kbd (format "M-%d" n))))))))

(ert-deftest agent-repl-test-keybindings-numerals-are-an-evil-intercept-map ()
  "The numerals map is an evil INTERCEPT map for every state.
Doom binds Command-digit in `evil-normal-state-map\=', which outranks any
minor-mode map; only an intercept map is consulted before evil's state
maps.  The mark is the `[intercept-state]\=' entry `evil-make-intercept-map\='
writes, with `all\=' for every state."
  (should (eq (lookup-key agent-repl-workspace-numerals-mode-map [intercept-state]) 'all)))

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

(defconst agent-repl-test-keybindings--source
  (expand-file-name "keybindings.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The keybindings source the leader bindings are read off.")

(defun agent-repl-test-keybindings--leader-prefix-binding (prefix key)
  "Return the command keybindings.el binds to SPC PREFIX KEY, or nil.
`map!' is a no-op stub under `emacs -Q', so the binding is read off the
SOURCE: every top-level `(map! :leader (:prefix PREFIX ...))' form is
walked for KEY followed by its #\\='COMMAND."
  (let (found)
    (with-temp-buffer
      (insert-file-contents agent-repl-test-keybindings--source)
      (goto-char (point-min))
      (condition-case nil
          (while (not found)
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'map!) (eq (cadr form) :leader))
                (dolist (group (cddr form))
                  (when (and (consp group) (eq (car group) :prefix)
                             (equal (cadr group) prefix))
                    (let ((tail (member key (cddr group))))
                      (when (and tail (eq (car-safe (cadr tail)) 'function))
                        (setq found (cadr (cadr tail))))))))))
        (end-of-file nil)))
    found))

(ert-deftest agent-repl-test-keybindings-spc-p-p-switches-among-current-workspaces ()
  "`SPC p p\=' offers the current and recent workspaces."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "p" "p")
              'agent-repl-switch-to-project)))

(ert-deftest agent-repl-test-keybindings-spc-p-upper-p-switches-among-every-workspace ()
  "`SPC p P\=' offers every known workspace."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "p" "P")
              'agent-repl-switch-to-any-workspace)))

(ert-deftest agent-repl-test-keybindings-spc-tab-f-forks-into-the-primary-branch ()
  "`SPC TAB f\=' forks the conversation into a top-level workspace."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "f")
              'agent-repl-fork-workspace-into-primary)))

(ert-deftest agent-repl-test-keybindings-spc-tab-f-upper-is-the-named-fork-into-the-primary-branch ()
  "`SPC TAB F\=' is `SPC TAB f\=' asking for a name instead of a prompt."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "F")
              'agent-repl-fork-workspace-into-primary-static)))

(ert-deftest agent-repl-test-keybindings-spc-tab-c-is-the-child-fork ()
  "`SPC TAB c\=' is the child fork, which merges back into the current workspace."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "c")
              'agent-repl-fork-workspace)))

(ert-deftest agent-repl-test-keybindings-spc-tab-c-upper-is-the-named-child-fork ()
  "`SPC TAB C\=' is the child fork asking for a name instead of a prompt."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "C")
              'agent-repl-fork-workspace-static)))

(ert-deftest agent-repl-test-keybindings-spc-tab-upper-o-is-the-explanation-engine-oneshot ()
  "`SPC TAB O\=' makes a one-shot in the explanation-engine repository."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "O")
              'agent-repl-create-explanation-engine-oneshot)))

(ert-deftest agent-repl-test-keybindings-spc-tab-c-r-reopens-a-closed-workspace ()
  "`SPC TAB C-r\=' re-opens a closed workspace, moved off `SPC TAB O\='."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "C-r")
              'agent-repl-open-workspace)))

(ert-deftest agent-repl-test-keybindings-spc-tab-h-hides-the-workspace ()
  "`SPC TAB h\=' hides the current workspace: off the tab bar, into the sidebar's hidden section."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "h")
              'agent-repl-hide-workspace)))

(ert-deftest agent-repl-test-keybindings-spc-tab-p-is-the-swap ()
  "`SPC TAB p\=' swaps the current workspace with a drawn tab's workspace."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "p")
              'agent-repl-workspace-swap)))

(ert-deftest agent-repl-test-keybindings-spc-tab-shift-p-is-the-move ()
  "`SPC TAB P\=' moves the current workspace onto a drawn tab, the tabs between shifting over."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "P")
              'agent-repl-workspace-move)))

(ert-deftest agent-repl-test-keybindings-spc-tab-shift-h-shows-a-hidden-workspace ()
  "`SPC TAB H\=' picks a hidden workspace, shows it again and switches to it."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "H")
              'agent-repl-show-hidden-workspace)))

(ert-deftest agent-repl-test-keybindings-spc-tab-tab-switches-to-the-previous ()
  "`SPC TAB TAB\=' goes back to the workspace selected before the one shown."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "TAB")
              'agent-repl-switch-to-previous-workspace)))

(ert-deftest agent-repl-test-keybindings-spc-tab-g-switches-to-the-last-finished ()
  "`SPC TAB g\=' jumps to the workspace that most recently went from working to done."
  (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" "g")
              'agent-repl-switch-to-last-finished-workspace)))

(ert-deftest agent-repl-test-keybindings-doom-other-workspace-is-the-previous ()
  "Doom's `+workspace/other\=' (`SPC TAB `\=') agrees with `SPC TAB TAB\='."
  (should (eq (lookup-key agent-repl-workspace-numerals-mode-map [remap +workspace/other])
              'agent-repl-switch-to-previous-workspace)))

(ert-deftest agent-repl-test-keybindings-m-0-switches-to-the-last-tab ()
  "`M-0' runs the last-tab command, not Doom's persp-indexed final switch."
  (should (eq (lookup-key agent-repl-workspace-numerals-mode-map (kbd "M-0"))
              'agent-repl-switch-to-last-workspace)))

(ert-deftest agent-repl-test-keybindings-s-0-matches-m-0 ()
  "Command-0 (`s-0\=') runs the SAME command as Option-0 (`M-0\=')."
  (should (eq (lookup-key agent-repl-workspace-numerals-mode-map (kbd "s-0"))
              (lookup-key agent-repl-workspace-numerals-mode-map (kbd "M-0")))))

(ert-deftest agent-repl-test-keybindings-doom-switch-to-final-is-remapped ()
  "Doom's `+workspace/switch-to-final\=', wherever bound, runs the last-tab command."
  (should (eq (lookup-key agent-repl-workspace-numerals-mode-map
                          [remap +workspace/switch-to-final])
              'agent-repl-switch-to-last-workspace)))

(ert-deftest agent-repl-test-keybindings-m-0-shadows-dooms-own ()
  "`M-0' reaches OUR command even with Doom's final switch in `global-map'."
  (let ((saved (current-global-map)))
    (unwind-protect
        (progn
          (use-global-map (copy-keymap saved))
          (define-key (current-global-map) (kbd "M-0") '+workspace/switch-to-final)
          (agent-repl-workspace-numerals-mode 1)
          (should (eq (key-binding (kbd "M-0")) 'agent-repl-switch-to-last-workspace)))
      (use-global-map saved))))

(ert-deftest agent-repl-test-keybindings-reload-rebuilds-the-live-numerals-map ()
  "Reloading keybindings.el puts `M-0' back on the SAME live keymap object.
A map built inside its `defvar' kept the first load's chords forever, so
`M-0', added after Emacs started, never reached the running map."
  ;; Arrange
  (let ((live agent-repl-workspace-numerals-mode-map))
    (define-key live (kbd "M-0") nil)
    ;; Act
    (load agent-repl-test-keybindings--file nil t)
    ;; Assert
    (should (eq agent-repl-workspace-numerals-mode-map live))
    (should (eq (lookup-key live (kbd "M-0")) 'agent-repl-switch-to-last-workspace))))

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

;;;; ---- Chat threads ----

(ert-deftest agent-repl-test-keybindings-spc-tab-chat-threads ()
  "`SPC TAB t\=', `T\=' and `.\=' start, reopen and pick a chat thread."
  (dolist (case '(("t" . agent-repl-thread-new)
                  ("T" . agent-repl-thread-recent)
                  ("." . agent-repl-thread-pick)))
    (should (eq (agent-repl-test-keybindings--leader-prefix-binding "TAB" (car case))
                (cdr case)))))
