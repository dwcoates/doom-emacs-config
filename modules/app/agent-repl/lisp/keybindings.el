;;; keybindings.el --- keybindings -*- lexical-binding: t; -*-

;;; Commentary:

;; The leader-map bindings for every surviving agent-repl command, plus the
;; two small helpers that only exist to serve a binding.
;;
;; Composer-local keys (RET and its send variants, history motion, the
;; discard chord) are NOT here: they live in `input.el' beside the commands
;; they run, in `agent-repl-input-mode-map'.

;; `map!' comes from Doom.  Byte-compiled outside a Doom session
;; (emacs -batch -Q) the macro is undefined, so the compiler treats each
;; `(map! ...)' body as a function-call form — and the dotted
;; `(:prefix ("j" . "claude"))' pair is not a valid form, which aborts
;; the compile.  Provide an expansion-time no-op for that case only; a
;; real Doom session (and any Doom-driven compile) has the genuine
;; macro, so this guard is inert there.
(eval-when-compile
  (unless (fboundp 'map!)
    (defmacro map! (&rest _args) nil)))

;;; Code:

;; `agent-repl--config-file' is defined by config.el at the module root,
;; which is the loader rather than a lisp/ source.
(defvar agent-repl--config-file)

;; `commands.el' owns `agent-repl-switch-numeral-count', and `config.el'
;; loads that file before this one; the declaration exists so this file
;; byte-compiles on its own.
(defvar agent-repl-switch-numeral-count)

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--live-ws-names "workspace")
(declare-function agent-repl--log "core")
(declare-function agent-repl--ws-current-log-name "workspace")
(declare-function agent-repl--ws-current-name "workspace")
(declare-function agent-repl--ws-get "workspace")

;;;; ---- Helpers a binding needs -----------------------------------------

(defun agent-repl--read-known-workspace (prompt)
  "Prompt for a registered workspace with PROMPT.
Defaults to the current workspace when it is registered, so RET picks the
obvious target.  Filters out tombstoned entries via
`agent-repl--live-ws-names': a killed workspace's identity record
survives in the hash for `--ws-dir' callers, but must not surface in an
interactive picker."
  (let* ((known (agent-repl--live-ws-names))
         (current (agent-repl--ws-current-name))
         (default (and current (member current known) current)))
    (unless known
      (agent-repl--log current "elisp.keybindings.read-workspace-none prompt=%S" prompt)
      (user-error "No agent-repl workspaces registered"))
    (let ((selected (completing-read prompt known nil t nil nil default)))
      (agent-repl--log selected "elisp.keybindings.read-workspace selected=%S known=%d"
                       selected (length known))
      selected)))

;; SPC b R -- revert the buffer from disk then eval it as Elisp (fast
;; config reload).
(defun agent-repl-revert-and-eval-buffer ()
  "Revert the current buffer from disk, then evaluate it as Elisp."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name)
                   "elisp.keybindings.revert-and-eval buffer=%s" (buffer-name))
  (revert-buffer :ignore-auto :noconfirm)
  (eval-buffer))

(defun agent-repl--reload-config-file ()
  "Return the config.el path to reload for the current workspace.
Prefers `<project-dir>/modules/app/agent-repl/config.el' when it exists,
so reloading inside a doom-config worktree picks up THAT worktree's
checkout rather than the root copy the module was originally loaded from.
Falls back to the original load path for workspaces that do not vendor
the module."
  (let* ((ws (agent-repl--ws-current-name))
         (proj (and ws (agent-repl--ws-get ws :project-dir)))
         (candidate (and proj (expand-file-name "modules/app/agent-repl/config.el" proj))))
    (if (and candidate (file-exists-p candidate))
        candidate
      agent-repl--config-file)))

(defun agent-repl-reload-config ()
  "Reload the agent-repl module config for the current workspace."
  (interactive)
  (let ((file (agent-repl--reload-config-file)))
    (agent-repl--log (agent-repl--ws-current-log-name)
                     "elisp.keybindings.reload-config file=%s" file)
    (load-file file)
    (message "[agent-repl] Reloaded %s" file)))

;;;; ---- Window and panel ------------------------------------------------

(map! :leader :prefix "w" :n "f" #'agent-repl-fullscreen-and-focus)

;;;; ---- SPC o : session control -----------------------------------------

(map! :leader
      :desc "Agent REPL (simple)" "o c" #'agent-repl-simple
      :desc "Agent REPL (deprio)" "o C" #'agent-repl
      ;; SPC o C-c is RestartWorkspace and nothing else: the DAEMON owns
      ;; everything the restart entails.  It is always immediate -- there is
      ;; no graceful mode and no prefix argument -- and bounces the
      ;; workspace's shim (session resumed) and webapp page.
      :desc "Restart workspace (shim + page)" "o C-c" #'agent-repl-restart-workspace
      :desc "Claude input" "o v" #'agent-repl-focus-input
      :desc "Copy file reference" "o r" #'agent-repl-copy-reference
      :desc "Reload webview (rebuilt bundle)" "o l" #'agent-repl-frontend-reload-webview
      ;; SPC o L sits on the shifted twin of the reload because the two are
      ;; the same act with different reasons: `o l' remounts a page that is
      ;; where it should be but running an old bundle, `o L' remounts one
      ;; that navigated somewhere else entirely and has no way back.
      :desc "Rescue webview (navigated away)" "o L" #'agent-repl-frontend-rescue-webview)

;;;; ---- SPC p : projects -------------------------------------------------

(map! :leader
      (:prefix "p"
       :desc "Switch to workspace" "p" #'agent-repl-switch-to-project))

;;;; ---- SPC TAB : workspaces ---------------------------------------------

(map! :leader
      (:prefix "TAB"
       :desc "Add project from directory" "C-n" #'agent-repl-add-project-workspace
       :desc "New workspace" "n" #'agent-repl-create-workspace
       :desc "New named workspace (no prompt)" "N" #'agent-repl-create-workspace-static
       :desc "New child workspace" "c" #'agent-repl-create-child-workspace
       :desc "New named child workspace (no prompt)" "C" #'agent-repl-create-child-workspace-static
       :desc "Fork workspace + fork the conversation" "f" #'agent-repl-fork-workspace
       :desc "Fork workspace, named (no prompt)" "F" #'agent-repl-fork-workspace-static
       :desc "New one-shot workspace" "o" #'agent-repl-create-oneshot
       :desc "Re-open a closed workspace" "O" #'agent-repl-open-workspace
       :desc "Merge current workspace (enqueue; C-u keeps it open)" "M" #'agent-repl-merge-workspace
       :desc "Open most recent workspace" "R" #'agent-repl-open-most-recent-workspace))

;; Tabs follow ROSTER ORDER strictly (the resolver orders them, priority
;; included), so these two are navigation over a given order and never a
;; reordering of it.  The push/pull-tab chords are gone with client-authored
;; ordering; the numerals below are not, because a numeral names a SLOT OF
;; THE DRAWN BAR.
(map! "s-{" #'agent-repl-switch-left
      "s-}" #'agent-repl-switch-right)

;;;; ---- M-1 .. M-9 : the tab-bar numerals --------------------------------

;; ONE SOURCE OF TRUTH FOR NAVIGATION: THE DRAWN TAB ORDER.  Doom binds
;; `M-1' .. `M-0' (and, on macOS, `s-1' .. `s-0' -- Command-digit) to
;; `+workspace/switch-to-N', which indexes PERSP-MODE'S
;; perspective list -- Doom's own `main' at slot 0, persp-mode's `none'
;; among them -- so `M-1' landed on the splash screen with no tab
;; highlighted and `M-2' on the FIRST tab.  Every chord here indexes
;; `agent-repl-roster-drawn-tab-order' instead, the same list the bar is drawn
;; from, so the numerals and the picture cannot disagree.  `M-0' is left to
;; Doom.
;;
;; WHY A KEYMAP OF OUR OWN, and not a later `map!' over Doom's.  Doom's
;; numerals are `:g' bindings, i.e. `global-map' entries, and beating a
;; `global-map' entry by rebinding it is a LOAD-ORDER RACE: whoever writes
;; the cell last wins, and a module reload, a Doom upgrade or a `doom sync'
;; reordering can hand it back.  A minor-mode keymap is consulted BEFORE
;; `global-map' whatever order the two were installed in, so the shadowing
;; is structural instead of probabilistic.  It is also the reason the chords
;; are observable in batch: this map is plain data, where `map!' is a no-op
;; stub outside a Doom session.
;;
;; BOTH MODIFIERS, AND AN EVIL INTERCEPT MAP.  Doom binds the Command-digit
;; chords with `:n', i.e. in `evil-normal-state-map', and an evil state map
;; outranks every minor-mode map.  So Command-2 pressed in a normal-state
;; buffer (the webview) ran Doom's `+workspace/switch-to-1' and landed on
;; persp-mode's SECOND perspective -- a different workspace from the bar's
;; second tab whenever the two lists disagree -- while Command-{ and
;; Command-} walked the bar correctly.  The map therefore carries both
;; modifiers and is marked an evil INTERCEPT map for every state, which evil
;; consults before any state map.  The mark is keymap DATA (the
;; `[intercept-state]' entry `evil-make-intercept-map' writes), set when the
;; map is built, so it holds whether evil loads before this module or after.

(defconst agent-repl-switch-numeral-modifiers '("M" "s")
  "The modifiers the tab-bar slot chords ride: Meta (Option) and Super (Command).")

(defvar agent-repl-workspace-numerals-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (modifier agent-repl-switch-numeral-modifiers)
      (dotimes (i agent-repl-switch-numeral-count)
        (let ((n (1+ i)))
          (define-key map (kbd (format "%s-%d" modifier n))
                      (intern (format "agent-repl-switch-to-workspace-%d" n))))))
    ;; `evil-make-intercept-map' with no STATE, written as the data it writes.
    (define-key map [intercept-state] 'all)
    map)
  "Keymap carrying `M-1\=' .. `M-9\=' and `s-1\=' .. `s-9\=', the slot chords.
Built from `agent-repl-switch-numeral-count', the modifiers in
`agent-repl-switch-numeral-modifiers' and the commands `commands.el\='
generates from the same number, so a chord with no command behind it --
or a command with no chord -- cannot be spelled.  An evil intercept map
for every state; see the commentary above.")

(define-minor-mode agent-repl-workspace-numerals-mode
  "Make `M-1\=' .. `M-9\=' and `s-1\=' .. `s-9\=' switch to a DRAWN TAB SLOT.
The slot is one of the tab bar as drawn.  Global, and enabled by this
module\='s own load, because the chords are about the tab bar rather than
about any one buffer.  Its keymap shadows Doom\='s `global-map\=' numerals
structurally; see the commentary above."
  :global t
  :lighter nil
  :group 'agent-repl
  :keymap agent-repl-workspace-numerals-mode-map)

(agent-repl-workspace-numerals-mode 1)

;;;; ---- SPC j : tell the agent to do a predefined thing -------------------

(map! :leader
      (:prefix ("j" . "claude")
       :desc "Enqueue input as deferred prompt"    "RET" #'agent-repl-queue-deferred-prompt
       :desc "Close workspace (view only)"         "d"   #'agent-repl-close-workspace
       :desc "Update GitHub PR description"        "r"   #'agent-repl-update-pr
       :desc "Rebase branch onto origin/master"    "b"   #'agent-repl-rebase-onto-origin-master
       :desc "Toggle debug logging"                "D"   #'agent-repl-toggle-debug
       :desc "Set durable log level"               "L"   #'agent-repl-set-log-file-level
       :desc "Toggle verbose to disk"              "V"   #'agent-repl-toggle-verbose-to-disk
       :desc "Kill workspace session (forced)"     "x"   #'agent-repl-kill-workspace
       :desc "NUKE workspace (deletes data)"       "X"   #'agent-repl-nuke-workspace
       :desc "Workspace notes"                    "n"   #'agent-repl-notes-open
       :desc "Reload agent-repl config"            "R"   #'agent-repl-reload-config
       :desc "Register repository from file"       "."   #'agent-repl-register-repository
       :desc "Bind workspace to a conversation"    "c"   #'agent-repl-bind-conversation
       :desc "Toggle persistent wifi mode"         "w"   #'agent-repl-persistent-wifi-mode-toggle
       (:prefix ("h" . "help")
        :desc "Copy workspace name" "y" #'agent-repl-copy-workspace-name)
       (:prefix ("e" . "explain")
        :desc "line/region/hunk (prompt)" "e" #'agent-repl-explain-prompt
        :desc "line/region/hunk (canned)" "E" #'agent-repl-explain
        (:prefix ("d" . "diff")
         :desc "worktree"    "w" #'agent-repl-explain-diff-worktree
         :desc "staged"      "s" #'agent-repl-explain-diff-staged
         :desc "uncommitted" "u" #'agent-repl-explain-diff-uncommitted
         :desc "HEAD"        "h" #'agent-repl-explain-diff-head
         :desc "branch"      "b" #'agent-repl-explain-diff-branch))
       (:prefix ("s" "Send predefined input to Claude")
        :desc "create PR (no --self-certified)"        "p"     #'agent-repl-create-or-update-pr-no-self-certified
        :desc "create PR"                              "P"     #'agent-repl-create-or-update-pr
        :desc "paste: create PR (no --self-certified)"  "C-p"   #'agent-repl-create-or-update-pr-no-self-certified-paste
        :desc "paste: create PR"                        "C-S-p" #'agent-repl-create-or-update-pr-paste)
       (:prefix ("m" . "modify workspace")
        :desc "Set/clear priority" "p" #'agent-repl-set-priority)
       ;; DAEMON ADMIN.  Not host-natured at all -- Emacs is merely today's
       ;; caller for the daemon's own drain and merge-queue controls.
       (:prefix ("a" . "daemon admin")
        :desc "Daemon health"          "h" #'agent-repl-daemon-health
        :desc "Session health"         "H" #'agent-repl-session-health
        :desc "Schedule drain + exit"  "s" #'agent-repl-daemon-shutdown-schedule
        :desc "Cancel scheduled drain" "c" #'agent-repl-daemon-shutdown-cancel
        :desc "Shut down now"          "!" #'agent-repl-daemon-shutdown-now
        (:prefix ("q" . "merge queue")
         :desc "Pause"  "p" #'agent-repl-merge-queue-pause
         :desc "Resume" "r" #'agent-repl-merge-queue-resume
         :desc "Evict this workspace" "e" #'agent-repl-merge-queue-evict))
       (:prefix ("t" . "tests")
        (:prefix ("r" . "run")
         (:prefix ("t" . "tests")
          :desc "worktree"    "w" #'agent-repl-run-tests-worktree
          :desc "staged"      "s" #'agent-repl-run-tests-staged
          :desc "uncommitted" "u" #'agent-repl-run-tests-uncommitted
          :desc "HEAD"        "h" #'agent-repl-run-tests-head
          :desc "branch"      "b" #'agent-repl-run-tests-branch)
         (:prefix ("l" . "lint")
          :desc "worktree"    "w" #'agent-repl-run-lint-worktree
          :desc "staged"      "s" #'agent-repl-run-lint-staged
          :desc "uncommitted" "u" #'agent-repl-run-lint-uncommitted
          :desc "HEAD"        "h" #'agent-repl-run-lint-head
          :desc "branch"      "b" #'agent-repl-run-lint-branch)
         (:prefix ("a" . "all")
          :desc "worktree"    "w" #'agent-repl-run-all-worktree
          :desc "staged"      "s" #'agent-repl-run-all-staged
          :desc "uncommitted" "u" #'agent-repl-run-all-uncommitted
          :desc "HEAD"        "h" #'agent-repl-run-all-head
          :desc "branch"      "b" #'agent-repl-run-all-branch))
        (:prefix ("a" . "analyze")
         (:prefix ("q" . "quality")
          :desc "worktree"    "w" #'agent-repl-test-quality-worktree
          :desc "staged"      "s" #'agent-repl-test-quality-staged
          :desc "uncommitted" "u" #'agent-repl-test-quality-uncommitted
          :desc "HEAD"        "h" #'agent-repl-test-quality-head
          :desc "branch"      "b" #'agent-repl-test-quality-branch)
         (:prefix ("c" . "coverage")
          :desc "worktree"    "w" #'agent-repl-test-coverage-worktree
          :desc "staged"      "s" #'agent-repl-test-coverage-staged
          :desc "uncommitted" "u" #'agent-repl-test-coverage-uncommitted
          :desc "HEAD"        "h" #'agent-repl-test-coverage-head
          :desc "branch"      "b" #'agent-repl-test-coverage-branch)))))

(map! :leader "b R" #'agent-repl-revert-and-eval-buffer)
(map! :leader "m e B" #'agent-repl-revert-and-eval-buffer)

;; NO `+workspace/kill' ADVICE.  It used to tear an agent session down
;; before the perspective went away, because Emacs owned the session.  It
;; does not: CloseWorkspace is a VIEW act that leaves the session running,
;; KillWorkspace is the forced death, and both are the daemon's.  An advice
;; that killed a session behind the user's persp-kill would now be Emacs
;; inventing a lifecycle decision the contract gives it no say in.

(provide 'keybindings)

;;; keybindings.el ends here
