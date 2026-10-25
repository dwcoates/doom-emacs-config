;;; input-help.el --- the composer's clickable help bar -*- lexical-binding: t; -*-

;;; Commentary:

;; THE COMPOSER'S HEADER LINE ENDS IN ONE HELP BUTTON, on its far right.
;; Clicking it drops a menu of topic groups (Composer, Feed, Workspaces,
;; ...) at the click, each a submenu of topics, and picking one opens the
;; shared agent-repl popup (popup.el) listing that topic's keys and the
;; features that go with them.  The topics cover every binding agent-repl
;; sets, not only the composer's.
;; `agent-repl-input-help' opens the same popups from the keyboard.
;;
;; THE KEYS SHOWN ARE READ FROM THE LIVE KEYMAPS, never written down here.
;; A topic names COMMANDS; the popup asks the composer's mode map (with its
;; Evil state maps), the global maps a `[remap ...]' entry defers to, and
;; Doom's leader map which keys run each one.  A rebinding therefore shows
;; up in the popup by construction.  A topic command no map binds is a
;; broken table and is fatal, and test-input-help.el pins both directions:
;; every topic command is bound, and every composer binding is in a topic.

;;; Code:

(require 'cl-lib)
(require 'seq)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--fatal "core" (ws fmt &rest args))
(declare-function agent-repl-popup-show "popup" (buffer))
(declare-function agent-repl-defface "core" (face spec doc &rest args))
(declare-function agent-repl-refill-keymap "core" (map fill))
(declare-function agent-repl-settings "settings" ())
(defvar agent-repl-input-mode-map)
(defvar evil-normal-state-map)
(defvar evil-insert-state-map)
(defvar doom-leader-map)
(defvar doom-leader-key)

(defconst agent-repl-input-help--log-scope
  '(:agent-repl-central "the composer's help is one fixed table, not workspace-scoped")
  "The log scope every composer-help record is filed under.")

(defconst agent-repl-input-help-topics
  '(;; ---- Composer ----
    (:id writing
     :group "Composer"
     :title "Writing prompts"
     :keymaps (agent-repl-input-mode-map)
     :bindings ((newline "Insert a newline instead of sending.")
                (agent-repl-prompt-entries-open-below
                 "Open a line below. On an entry line, start the next entry with its label.")
                (agent-repl-prompt-entries-open-above
                 "Open a line above. On an entry line, start the previous entry with its label.")
                (agent-repl-discard-input "Save the composer to history, then clear it."))
     :features ("A word starting with / completes against the workspace's commands, and a known one turns purple."
                "A word that begins an open workspace's name completes to that name."
                "Only TAB takes a completion: RET closes the completion popup and sends."))
    (:id sending
     :group "Composer"
     :title "Sending prompts"
     :keymaps (agent-repl-input-mode-map)
     :bindings ((agent-repl-send "Send the composer's contents.")
                (agent-repl-input-control-return
                 "Send with the send postfix appended. With a multi-select standing and an empty composer, confirm its ticks instead."
                 :variables (agent-repl-send-postfix))
                (agent-repl-send-with-prefix "Send with the send prefix prepended."
                                             :variables (agent-repl-send-prefix))
                (agent-repl-queue-deferred-prompt "Defer the prompt until the current turn ends.")
                (agent-repl-interrupt-turn "Interrupt the running turn."))
     :features ("Prompts sent while the daemon is away wait, counted in the mode line, and go out once it is back."))
    (:id editing
     :group "Composer"
     :title "Edit sent prompts"
     :keymaps (agent-repl-input-mode-map agent-repl-input-selection-mode-map)
     :bindings ((agent-repl-turn-boundary-select-next "Select the next (newer) prompt or final response.")
                (agent-repl-turn-boundary-select-prev "Select the previous (older) prompt or final response.")
                (agent-repl-input-selection-return
                 "On a selected prompt, roll back to it: an empty composer gets the prompt back to edit, words in the composer resend it edited. On a selected response, reply quoting it.")
                (agent-repl-rollback-keep-files "Roll back to before the selected prompt, keeping files.")
                (agent-repl-rollback-restore-files "Roll back to before the selected prompt, restoring files.")
                (agent-repl-input-selection-escape "Clear the feed selection."))
     :features ("With no prompt selected, a rollback cancels the latest prompt."
                "A rollback asks before it acts."
                "Restoring files restores only the files the agent's edit tools changed."))
    (:id history
     :group "Composer"
     :title "Prompt history"
     :keymaps (agent-repl-input-mode-map)
     :bindings ((agent-repl--history-prev "Show the previous (older) history entry.")
                (agent-repl--history-next "Show the next (newer) history entry.")
                (agent-repl-history-search "Search the whole history and pick an entry."))
     :features ("Every sent prompt is saved to the workspace's history, which survives restarts."
                "Walking past the newest entry restores the draft you were typing."))
    (:id images
     :group "Composer"
     :title "Images"
     :keymaps (agent-repl-input-mode-map agent-repl--image-marker-map)
     :bindings ((agent-repl-attach-clipboard-image "Attach the clipboard's image, even when text rides beside it.")
                (agent-repl-input-yank "Paste the clipboard's image, or yank as usual.")
                (agent-repl-input-evil-paste-after "Evil's p: the clipboard's image, or paste after.")
                (agent-repl-input-evil-paste-before "Evil's P: the clipboard's image, or paste before.")
                (agent-repl-input-open-image-at-mouse "Open the clicked image large over the feed."))
     :features ("An image is one character of the prompt: delete, kill or yank it like text."))
    (:id answering
     :group "Composer"
     :title "Answering cards"
     :keymaps (agent-repl-input-mode-map)
     :bindings ((agent-repl-input-digit "Pick the standing card's numbered choice.")
                (agent-repl-input-control-return "Confirm a multi-select's ticked choices.")
                (agent-repl-input-insert-escape "Close a standing news digest and leave insert mode."))
     :features ("A digit answers only in an empty composer; anywhere else it is typed as usual."
                "On a multi-select a digit toggles its choice's tick instead of answering."))
    ;; ---- Feed ----
    (:id feed
     :group "Feed"
     :title "Reading the feed"
     :keymaps (agent-repl-frontend-webview-mode-map agent-repl-input-mode-map
               agent-repl-input-selection-mode-map)
     :bindings ((agent-repl-frontend-copy-selection "Copy the feed's highlighted text.")
                (agent-repl-response-search-buffer "Search the selected bubble's text.")
                (agent-repl-response-search-forward "Search the selected bubble's text forward.")
                (agent-repl-response-search-backward "Search the selected bubble's text backward.")
                (agent-repl-feed-text-scale-increase "Make the feed's text larger.")
                (agent-repl-feed-text-scale-decrease "Make the feed's text smaller.")
                (agent-repl-frontend-reload-webview "Reload the feed's page, after a rebuilt bundle.")
                (agent-repl-frontend-rescue-webview "Bring a page that navigated away back to the feed."))
     :features ("The zoom changes the feed's text only, never Emacs's own font, and is saved by the daemon."
                "The bubble search keys work while a bubble is selected (Edit sent prompts)."))
    ;; ---- Workspaces ----
    (:id switching
     :group "Workspaces"
     :title "Switching workspaces"
     :keymaps (agent-repl-workspace-numerals-mode-map)
     :bindings (((agent-repl-switch-to-workspace-1 agent-repl-switch-to-workspace-2
                  agent-repl-switch-to-workspace-3 agent-repl-switch-to-workspace-4
                  agent-repl-switch-to-workspace-5 agent-repl-switch-to-workspace-6
                  agent-repl-switch-to-workspace-7 agent-repl-switch-to-workspace-8
                  agent-repl-switch-to-workspace-9)
                 "Switch to tab 1 to 9 on the bar, counting from the left; 9 is the last when there are fewer.")
                (agent-repl-switch-to-last-workspace "Switch to the last tab on the bar.")
                (agent-repl-switch-left "Switch to the tab on the left, wrapping.")
                (agent-repl-switch-right "Switch to the tab on the right, wrapping.")
                (agent-repl-switch-older "Go back to the workspace viewed before this one.")
                (agent-repl-switch-newer "Go forward to the workspace viewed after this one.")
                (agent-repl-switch-to-previous-workspace "Switch to the workspace selected before this one.")
                (agent-repl-switch-to-last-finished-workspace "Switch to the workspace that most recently finished its turn.")
                (agent-repl-open-most-recent-workspace "Switch to the most recently selected workspace not yet visited this cycle.")
                (agent-repl-switch-to-project "Switch to a current or recent workspace, or to a project.")
                (agent-repl-switch-to-any-workspace "Switch to any known workspace, opening it first when needed.")))
    (:id creating
     :group "Workspaces"
     :title "Creating workspaces"
     :bindings ((agent-repl-create-workspace "New workspace, from a prompt.")
                (agent-repl-create-workspace-static "New named workspace, with no prompt.")
                (agent-repl-create-oneshot "New one-shot workspace, from a commission.")
                (agent-repl-create-oneshot-for-current-workspace "New one-shot that fixes something wrong with this workspace.")
                (agent-repl-create-explanation-engine-oneshot "New one-shot in the explanation-engine repository.")
                (agent-repl-fork-workspace "Fork this conversation into a child workspace that merges back here.")
                (agent-repl-fork-workspace-static "Fork into a named child workspace, with no prompt.")
                (agent-repl-fork-workspace-into-primary "Fork this conversation into a workspace off the main branch.")
                (agent-repl-fork-workspace-into-primary-static "Fork into a named workspace off the main branch, with no prompt.")
                (agent-repl-add-project-workspace "Add a project from a directory.")
                (agent-repl-register-repository "Register the repository a picked file is inside.")
                (agent-repl-open-workspace "Re-open a closed workspace.")))
    (:id managing
     :group "Workspaces"
     :title "Managing workspaces"
     :bindings ((agent-repl-finish-workspace "Finish this workspace: deploy its work, and close it once deployed.")
                (agent-repl-hide-workspace "Hide this workspace from the tab bar.")
                (agent-repl-show-hidden-workspace "Show a hidden workspace again and switch to it.")
                (agent-repl-workspace-swap "Swap this workspace with the tab at a digit (0 is 10).")
                (agent-repl-workspace-move "Move this workspace onto the tab at a digit (0 is 10).")
                (agent-repl-set-priority "Set or clear this workspace's priority.")
                (agent-repl-bind-conversation "Point this workspace at another of its conversations.")
                (agent-repl-restart-workspace "Restart this workspace's session and page.")
                (agent-repl-close-workspace "Close this workspace's view.")
                (agent-repl-kill-workspace "Kill this workspace's session by force.")
                (agent-repl-nuke-workspace "Destroy this workspace, its worktree and its branch.")
                (agent-repl-copy-workspace-name "Copy this workspace's name.")
                (agent-repl-notes-open "Open this workspace's notes.")))
    (:id threads
     :group "Workspaces"
     :title "Chat threads"
     :bindings ((agent-repl-thread-new "New chat thread, from its first question.")
                (agent-repl-thread-recent "Open the chat thread last interacted with.")
                (agent-repl-thread-pick "Open a chat thread picked by name.")))
    (:id panels
     :group "Workspaces"
     :title "Panels and windows"
     :keymaps (evil-motion-state-map agent-repl-popup-mode-map agent-repl-open-progress-mode-map)
     :bindings ((agent-repl "Hide the agent panels and lower the workspace's priority.")
                (agent-repl-simple "Show or hide the agent panels.")
                (agent-repl-focus-input "Focus the composer, or go back to the feed from it.")
                (agent-repl-fullscreen-and-focus "Focus the composer, or maximize a window that is not an agent's.")
                (agent-repl-copy-reference "Copy the current file and line as a reference.")
                (agent-repl-popup-quit "Close an agent-repl popup, like this one.")
                (quit-window "Close the workspace-opening progress buffer.")))
    ;; ---- Agent commands ----
    (:id explain
     :group "Agent commands"
     :title "Explain"
     :bindings ((agent-repl-explain "Explain the current line, region or hunk.")
                (agent-repl-explain-prompt "Ask about the current line, region or hunk, with your own words.")
                (agent-repl-explain-diff-branch "Explain the branch's changes.")
                (agent-repl-explain-diff-head "Explain the last commit.")
                (agent-repl-explain-diff-staged "Explain the staged changes.")
                (agent-repl-explain-diff-uncommitted "Explain all uncommitted changes.")
                (agent-repl-explain-diff-worktree "Explain the unstaged changes.")))
    (:id tests
     :group "Agent commands"
     :title "Tests and lint"
     :bindings ((agent-repl-run-tests-branch "Run tests for the branch's changes.")
                (agent-repl-run-tests-head "Run tests for the last commit.")
                (agent-repl-run-tests-staged "Run tests for the staged changes.")
                (agent-repl-run-tests-uncommitted "Run tests for all uncommitted changes.")
                (agent-repl-run-tests-worktree "Run tests for the unstaged changes.")
                (agent-repl-run-lint-branch "Run lint for the branch's changes.")
                (agent-repl-run-lint-head "Run lint for the last commit.")
                (agent-repl-run-lint-staged "Run lint for the staged changes.")
                (agent-repl-run-lint-uncommitted "Run lint for all uncommitted changes.")
                (agent-repl-run-lint-worktree "Run lint for the unstaged changes.")
                (agent-repl-run-all-branch "Run lint and tests for the branch's changes.")
                (agent-repl-run-all-head "Run lint and tests for the last commit.")
                (agent-repl-run-all-staged "Run lint and tests for the staged changes.")
                (agent-repl-run-all-uncommitted "Run lint and tests for all uncommitted changes.")
                (agent-repl-run-all-worktree "Run lint and tests for the unstaged changes.")
                (agent-repl-test-coverage-branch "Analyze test coverage of the branch's changes.")
                (agent-repl-test-coverage-head "Analyze test coverage of the last commit.")
                (agent-repl-test-coverage-staged "Analyze test coverage of the staged changes.")
                (agent-repl-test-coverage-uncommitted "Analyze test coverage of all uncommitted changes.")
                (agent-repl-test-coverage-worktree "Analyze test coverage of the unstaged changes.")
                (agent-repl-test-quality-branch "Analyze test quality of the branch's changes.")
                (agent-repl-test-quality-head "Analyze test quality of the last commit.")
                (agent-repl-test-quality-staged "Analyze test quality of the staged changes.")
                (agent-repl-test-quality-uncommitted "Analyze test quality of all uncommitted changes.")
                (agent-repl-test-quality-worktree "Analyze test quality of the unstaged changes.")))
    (:id prs
     :group "Agent commands"
     :title "Pull requests and releases"
     :bindings ((agent-repl-rebase-onto-origin-master "Fetch, then ask the agent to rebase onto origin/master.")
                (agent-repl-create-or-update-pr "Ask the agent to create or update the PR.")
                (agent-repl-create-or-update-pr-no-self-certified "Ask the agent to create or update the PR, without self-certifying.")
                (agent-repl-create-or-update-pr-paste "Insert the create-or-update-PR prompt instead of sending it.")
                (agent-repl-create-or-update-pr-no-self-certified-paste
                 "Insert the create-or-update-PR prompt, without self-certifying, instead of sending it.")
                (agent-repl-update-pr "Ask the agent to update the PR's description.")
                (agent-repl-open-workspace-pr-in-browser "Open this workspace's PR in the browser.")
                (agent-repl-push-release "Push master to origin as a release.")))
    ;; ---- Git ----
    (:id magit
     :group "Git"
     :title "Magit"
     :requires (magit magit-diff)
     :keymaps (magit-status-mode-map magit-mode-map magit-file-section-map magit-hunk-section-map
               magit-diff-section-map)
     :bindings ((agent-repl-magit-status-workspace "Open magit for this workspace's worktree.")
                (agent-repl-magit-status-main-worktree "Open magit for the repository's main worktree.")
                (agent-repl-magit-status-quit "Close magit and restore the windows it replaced.")
                (agent-repl-magit-open-commit-in-github "Open the commit at point on GitHub.")
                (agent-repl-magit-copy-commit-link "Copy a GitHub link to the commit at point.")
                (magit-jump-to-unpushed-to-upstream "Jump to the recent commits.")
                (magit-diff-visit-worktree-file "Visit the file at point, in the worktree.")
                (magit-diff-visit-file "Visit the file at point, at its revision.")
                (magit-diff-visit-file-other-window "Visit the file at point in another window.")))
    ;; ---- Daemon ----
    (:id merge-queue
     :group "Daemon"
     :title "Merge queue"
     :bindings ((agent-repl-merge-queue-evict "Take this workspace's merge off the queue.")
                (agent-repl-merge-queue-pause "Pause this repository's queue.")
                (agent-repl-merge-queue-resume "Resume this repository's queue.")))
    (:id daemon
     :group "Daemon"
     :title "Daemon and health"
     :bindings ((agent-repl-daemon-health "Show the daemon's health.")
                (agent-repl-session-health "Show this workspace's session health.")
                (agent-repl-daemon-shutdown-schedule "Schedule the daemon to drain and exit.")
                (agent-repl-daemon-shutdown-cancel "Cancel a scheduled drain.")
                (agent-repl-daemon-shutdown-now "Stop the daemon now.")
                (agent-repl-send-hidden-keepalives "Send a keep-alive to every hidden workspace.")
                (agent-repl-persistent-wifi-mode-toggle "Turn persistent wifi mode, for lid-closed operation, on or off.")))
    (:id config
     :group "Daemon"
     :title "Logging and config"
     :bindings ((agent-repl-set-log-file-level "Set the log file's level.")
                (agent-repl-toggle-debug "Show or hide debug logging in *Messages*.")
                (agent-repl-toggle-verbose-to-disk "Write verbose logging to the log file, or stop.")
                (agent-repl-reload-config "Reload agent-repl's config.")
                (agent-repl-revert-and-eval-buffer "Revert this buffer from disk, then evaluate it as Elisp.")))
    ;; ---- Settings ----
    (:id settings
     :group "Settings"
     :title "Settings page"
     :keymaps (agent-repl-settings-mode-map)
     :bindings ((agent-repl-settings-toggle-section "Open or shut the section at point.")
                (agent-repl-settings-refresh "Draw the page again, reading every key afresh.")
                (agent-repl-popup-quit "Close the page."))
     :features ("Click a key to change or remove it, and + to add one; a key is any sequence of keys, ended by ESC."
                "A changed command shows changed, and reset puts its keys back."
                "Changes are saved in ~/zettel/settings.eld and laid over the defaults at every start.")))
  "Every topic of agent-repl's help, in menu order.
Each is a plist: `:id' (a symbol), `:group' (the menu's submenu),
`:title' (the menu item's label and the popup's heading), `:keymaps' (the
keymap variables its keys are read from beside the global and leader
maps, by default none), `:requires' (features loaded before they are
read), `:bindings' and `:features'.  A binding is
\(COMMAND DESCRIPTION [:variables (VARIABLE ...)]), COMMAND one command or
a list of commands sharing one row: its keys are read from the live
keymaps when the popup opens, and each VARIABLE whose value shapes what
COMMAND does is shown with its current value.  `:features' are lines
describing what goes with those keys.

Every binding agent-repl sets is in a topic, or in
`agent-repl-input-help-unlisted-commands': test-input-help.el scans every
binding form of the module and every one of its keymaps.")

(defconst agent-repl-input-help-unlisted-commands
  '((ignore . "the Help button's press, which only keeps a click from dragging the header line")
    (agent-repl-input-help-click . "the Help button itself")
    (agent-repl-settings . "the Settings buttons themselves, on the composer's bar and the splash")
    (agent-repl-input-company-return . "RET in the completion popup, described under Writing prompts")
    (company-complete-selection . "TAB in the completion popup, described under Writing prompts")
    (isearch-backward . "C-r in a creation prompt's minibuffer, not the composer, described in USER-GUIDE.md"))
  "Commands agent-repl binds that no help topic lists, each with why.")

(defconst agent-repl-input-help-bar-background "#111110"
  "The help bar's ground: the webapp sidebar's dark `--sidebar-bg'.
Copied from webapp/src/styles.css, as `agent-repl-input-background' copies
`--composer-bg', and pinned by both test-input-help.el and
webapp/test/styles.test.ts so the two cannot drift.")

(defconst agent-repl-input-help-button-background "#151514"
  "A help button's fill: a little darker than the sidebar's selected row.
The webapp fills the selected workspace row with `--row-current-tint', 38%
of the dark `--card' over `--sidebar-bg' (#181817); a button sits between
that and the bar's ground, so it reads as a quieter cousin of the
selection.  test-input-help.el checks it stays between the two, and
webapp/test/styles.test.ts pins the selected row's color.")

(agent-repl-defface 'agent-repl-input-help-bar
  `((t :background ,agent-repl-input-help-bar-background))
  "Face of the composer's help bar around and between its buttons.")

(agent-repl-defface 'agent-repl-input-help-button
  `((t :foreground "#8b8e91" :background ,agent-repl-input-help-button-background
       :weight normal :box (:line-width (2 . -1) :style released-button)))
  "Face of one help button in the composer's header line.
Raised and filled a step above the bar's ground, with grey ink so the row
stays quieter than the composer's own text.")

(agent-repl-defface 'agent-repl-input-help-title
  '((t :inherit bold :height 1.3))
  "Face of a composer help popup's title.")

(agent-repl-defface 'agent-repl-input-help-section
  '((t :inherit shadow :weight bold))
  "Face of a section heading (Keys, Good to know) in a composer help popup.")

(agent-repl-defface 'agent-repl-input-help-annotation
  '((t :inherit shadow :slant italic))
  "Face of the Evil state a key in a composer help popup is bound in.")

(agent-repl-defface 'agent-repl-input-help-variable
  '((t :inherit (shadow button)))
  "Face of a variable named in a composer help popup; clicking describes it.")

;;;; ---- The header line ------------------------------------------------

(defvar agent-repl-input-help-button-map (make-sparse-keymap)
  "Keymap of the Help button in the composer's header line.")

(defvar agent-repl-input-help-settings-button-map (make-sparse-keymap)
  "Keymap of the Settings button in the composer's header line.")

(defun agent-repl--input-help-fill-button-map (command)
  "Return a fill binding a header-line button's click to COMMAND."
  (lambda (map)
    (define-key map [header-line mouse-1] command)
    ;; A press on a button is not a header-line drag.
    (define-key map [header-line down-mouse-1] #'ignore)))

(agent-repl-refill-keymap agent-repl-input-help-button-map
                          (agent-repl--input-help-fill-button-map #'agent-repl-input-help-click))
(agent-repl-refill-keymap agent-repl-input-help-settings-button-map
                          (agent-repl--input-help-fill-button-map #'agent-repl-settings))

(defconst agent-repl-input-help--bar-buttons
  '(("  Settings  " "mouse-1: change any key binding" agent-repl-input-help-settings-button-map)
    ("  Help  " "mouse-1: composer keys and features" agent-repl-input-help-button-map))
  "The bar's buttons, left to right: (LABEL HELP-ECHO KEYMAP-VARIABLE).")

(defun agent-repl--input-help-button (label echo map)
  "Return a header-line button LABEL, explained by ECHO, clicked through MAP."
  (propertize label
              'face 'agent-repl-input-help-button
              'mouse-face 'mode-line-highlight
              'pointer 'hand
              'help-echo echo
              'local-map map))

(defun agent-repl--input-help-header-line-text ()
  "Return the composer's header line: the bar, then its buttons at the right.
Every column of it is this text: the bar's stretch reaches to the
buttons, which end one column short of the window's right edge, so the
bar's ground is `agent-repl-input-help-bar' whatever face a theme
\(solaire's, say) remaps `header-line' to."
  (let* ((gap (propertize " " 'face 'agent-repl-input-help-bar))
         (buttons (mapconcat (pcase-lambda (`(,label ,echo ,map))
                               (agent-repl--input-help-button label echo (symbol-value map)))
                             agent-repl-input-help--bar-buttons gap))
         (width (1+ (string-width buttons))))
    (concat (propertize " " 'face 'agent-repl-input-help-bar
                        'display `(space :align-to (- right ,width)))
            buttons
            gap)))

;;;; ---- The topic menu -------------------------------------------------

(defun agent-repl--input-help-groups ()
  "Return the topics' groups, each (GROUP TOPIC ...), in table order."
  (let (groups)
    (dolist (topic agent-repl-input-help-topics)
      (let ((group (assoc (plist-get topic :group) groups)))
        (if group
            (setcdr group (append (cdr group) (list topic)))
          (push (list (plist-get topic :group) topic) groups))))
    (nreverse groups)))

(defun agent-repl--input-help-menu ()
  "Return the help button's menu: a submenu per group, an item per topic.
Both in table order.  Choosing an item shows that topic
\(`agent-repl-input-help-show')."
  (let ((menu (make-sparse-keymap "agent-repl help")))
    (pcase-dolist (`(,group . ,topics) (reverse (agent-repl--input-help-groups)))
      (let ((submenu (make-sparse-keymap group)))
        (dolist (topic (reverse topics))
          (let ((id (plist-get topic :id)))
            (define-key submenu (vector id)
                        `(menu-item ,(plist-get topic :title)
                                    ,(lambda () (interactive) (agent-repl-input-help-show id))))))
        (define-key menu (vector (intern group)) `(menu-item ,group ,submenu))))
    menu))

;; EVERY COMPOSER'S HEADER LINE IS THIS ONE VARIABLE, never a copy of it.
;; `header-line-format' holds the symbol, which redisplay reads afresh, so
;; rebuilding the value on every load (the `setq' below, since `defvar'
;; initializes once) updates every open composer by construction.  A
;; symbol's string value keeps its text properties, the buttons, only when
;; the symbol is risky.
(defvar agent-repl-input-help-header-line nil
  "The composer's header line, every composer's `header-line-format'.")
(put 'agent-repl-input-help-header-line 'risky-local-variable t)
(setq agent-repl-input-help-header-line (agent-repl--input-help-header-line-text))

(defun agent-repl-input-help-install ()
  "Make the current buffer's header line `agent-repl-input-help-header-line'."
  (setq-local header-line-format 'agent-repl-input-help-header-line)
  (force-mode-line-update))

(defun agent-repl--input-help-install-in-composers ()
  "Install the help header line in every open composer; return how many.
A composer made before this file last loaded may hold any header line an
older load gave it, so each load points every one at the variable."
  (let ((count 0))
    (dolist (buf (buffer-list))
      (when (eq (buffer-local-value 'major-mode buf) 'agent-repl-input-mode)
        (with-current-buffer buf (agent-repl-input-help-install))
        (setq count (1+ count))))
    (agent-repl--log agent-repl-input-help--log-scope
                     "elisp.input-help.install-in-composers: composers=%d" count)
    count))

;;;; ---- Finding a command's keys ---------------------------------------

(defun agent-repl--input-help-binding-context (topic)
  "Return the live keymaps TOPIC's popup reads keys from.
`:maps' are TOPIC's `:keymaps' as (VARIABLE . MAP) pairs (an Evil state
map rides inside each), loaded first through its `:requires'; `:global'
the (STATE VARIABLE . MAP) triples a `[remap ...]' entry resolves
through, and that are searched themselves (STATE nil for any state);
`:leader' Doom's leader map, and `:leader-key' the key that opens it.
Each map travels with its variable, so a key found in it can be named,
saved and changed (settings.el).  A keymap variable still unbound once
its topic's features are loaded is a broken table, and is fatal."
  (dolist (feature (plist-get topic :requires))
    (require feature))
  (list :maps (mapcar (lambda (variable)
                        (unless (and (boundp variable) (keymapp (symbol-value variable)))
                          (agent-repl--fatal agent-repl-input-help--log-scope
                                             "elisp.input-help.context: unbound keymap=%s topic=%s"
                                             variable (plist-get topic :id)))
                        (cons variable (symbol-value variable)))
                      (plist-get topic :keymaps))
        :global (list (cons "insert" (cons 'evil-insert-state-map evil-insert-state-map))
                      (cons "normal" (cons 'evil-normal-state-map evil-normal-state-map))
                      (cons nil (cons 'global-map global-map)))
        :leader doom-leader-map
        :leader-key doom-leader-key))

(defun agent-repl--input-help-canonical-key (description)
  "Return DESCRIPTION, a `key-description', with each key under one name.
`<return>' and `<tab>' are the function keys Emacs reads RET and TAB as
in a graphical frame, so a map binding both shows each once, as RET and
TAB."
  (replace-regexp-in-string
   "<\\(return\\|tab\\)>"
   (lambda (match) (if (equal match "<return>") "RET" "TAB"))
   description t t))

(defun agent-repl--input-help-state-key (key)
  "Split KEY, a key vector from the composer's map, into (STATE . REST).
Evil keeps a mode map's state bindings under a `normal-state' (and so on)
prefix event, so STATE is that state's name, or nil for a plain binding."
  (let ((first (aref key 0)))
    (if (and (symbolp first)
             (string-match "\\`\\(.+\\)-state\\'" (symbol-name first)))
        (cons (match-string 1 (symbol-name first)) (seq-subseq key 1))
      (cons nil key))))

(defun agent-repl--input-help-key-sources (command context)
  "Return where COMMAND's keys in CONTEXT come from, one plist per key.
Each is (:description D :state S :keymap VARIABLE :vector V): D the key
as shown, S the Evil state it is bound in (nil for any), and VARIABLE and
V the keymap variable and the key vector in it whose binding puts it
there, so `(define-key (symbol-value VARIABLE) V nil)' takes it away.
Each map of `:maps' is searched, a `[remap X]' entry standing for X's
keys in the global maps (its V is the remap entry, which every one of
those keys shares); then the global maps, less the keys a map of
`:maps' gives another command (`agent-repl--input-help-shadowed-p'), and
the leader."
  ;; Every search names its keymap in a list: given one keymap alone,
  ;; `where-is-internal' searches the global map too, which put Emacs's own
  ;; global keys under every map, the leader's included.
  (let (found)
    (cl-flet ((add (description state variable vector)
                (push (list :description description :state state
                            :keymap variable :vector vector)
                      found)))
      (pcase-dolist (`(,variable . ,map) (plist-get context :maps))
        (dolist (key (where-is-internal command (list map) nil t t))
          (if (eq (aref key 0) 'remap)
              (pcase-dolist (`(,state ,_ . ,global) (plist-get context :global))
                (dolist (target (where-is-internal (aref key 1) (list global) nil t))
                  (add (agent-repl--input-help-canonical-key (key-description target))
                       state variable key)))
            (pcase-let ((`(,state . ,rest) (agent-repl--input-help-state-key key)))
              (add (agent-repl--input-help-canonical-key (key-description rest))
                   state variable key)))))
      (pcase-dolist (`(,state ,variable . ,global) (plist-get context :global))
        (dolist (key (where-is-internal command (list global) nil t t))
          (unless (or (eq (aref key 0) 'remap)
                      (agent-repl--input-help-shadowed-p key state command context))
            (add (agent-repl--input-help-canonical-key (key-description key))
                 state variable key))))
      (dolist (key (where-is-internal command (list (plist-get context :leader)) nil t))
        (add (concat (plist-get context :leader-key) " "
                     (agent-repl--input-help-canonical-key (key-description key)))
             nil 'doom-leader-map key)))
    (nreverse found)))

(defun agent-repl--input-help-shadowed-p (key state command context)
  "Non-nil when a map of CONTEXT's `:maps' binds KEY to other than COMMAND.
A global key reaches the topic only where its own maps leave it free: the
composer's RET sends, so the global RET that runs `newline' is not
`newline''s key there.  KEY is read plainly and under STATE's Evil prefix,
both command and insert mode's when STATE is nil."
  (seq-some
   (pcase-lambda (`(,_ . ,map))
     (seq-some (lambda (vector)
                 (let ((binding (lookup-key map vector)))
                   (and binding (not (numberp binding)) (not (keymapp binding))
                        (not (eq binding command)))))
               (cons key (mapcar (lambda (each) (vconcat (vector (intern (concat each "-state"))) key))
                                 (if state (list state) '("normal" "insert"))))))
   (plist-get context :maps)))

(defun agent-repl--input-help-group-sources (sources)
  "Return SOURCES grouped by key, as (DESCRIPTION . SOURCES) pairs.
Shortest key first, then alphabetically: a keymap's own order is the
reverse of its definitions, which says nothing to a reader.  Shared by
the help popup and the Settings page (settings.el), so a key reads as
one key wherever it is bound."
  (let (groups)
    (dolist (source sources)
      (let ((group (assoc (plist-get source :description) groups)))
        (if group
            (setcdr group (append (cdr group) (list source)))
          (push (list (plist-get source :description) source) groups))))
    (sort groups (lambda (a b)
                   (let ((x (car a)) (y (car b)))
                     (or (< (length x) (length y))
                         (and (= (length x) (length y)) (string< x y))))))))

(defun agent-repl--input-help-group-label (sources)
  "Return the state annotation for one key's SOURCES, or nil for none."
  (agent-repl--input-help-state-label
   (mapcar (lambda (source) (plist-get source :state)) sources)))

(defun agent-repl--input-help-state-label (states)
  "Return the annotation for a key bound in STATES, or nil for none.
STATES holds nil when the key is bound in any state.  A key reachable in
both command and insert mode needs no annotation."
  (unless (or (memq nil states)
              (and (member "normal" states) (member "insert" states)))
    (mapconcat (lambda (state)
                 (pcase state
                   ("normal" "command mode")
                   ("insert" "insert mode")
                   (_ (concat state " mode"))))
               (sort (delete-dups (copy-sequence states)) #'string<)
               ", ")))

(defun agent-repl--input-help-digit-key (key)
  "Return KEY as (PREFIX . DIGIT) when it ends in a lone digit, else nil.
The digit stands alone, or follows a modifier's dash or a space, so M-3
and SPC TAB 3 are digit keys and F13 is not."
  (when (string-match "\\`\\(\\(?:.*[- ]\\)?\\)\\([0-9]\\)\\'" key)
    (cons (match-string 1 key) (string-to-number (match-string 2 key)))))

(defun agent-repl--input-help-collapse-digits (keys)
  "Return KEYS with each run of consecutive digit keys as one range key.
KEYS are (KEY . ANNOTATION) pairs.  A run is keys sharing a prefix and an
annotation whose digits climb by one, so M-1 M-2 ... M-9 reads as
\"M-1 … M-9\"; a run of two or more is collapsed."
  (let (out run)
    (cl-flet ((flush ()
                (when run
                  (pcase-let ((`(,prefix ,first ,last ,annotation) run))
                    (push (cons (if (= first last)
                                    (format "%s%d" prefix first)
                                  (format "%s%d … %s%d" prefix first prefix last))
                                annotation)
                          out))
                  (setq run nil))))
      (dolist (key keys)
        (let ((digit (agent-repl--input-help-digit-key (car key))))
          (cond ((and digit run
                      (equal (car digit) (nth 0 run))
                      (equal (cdr key) (nth 3 run))
                      (= (cdr digit) (1+ (nth 2 run))))
                 (setf (nth 2 run) (cdr digit)))
                (digit
                 (flush)
                 (setq run (list (car digit) (cdr digit) (cdr digit) (cdr key))))
                (t
                 (flush)
                 (push key out)))))
      (flush))
    (nreverse out)))

(defun agent-repl--input-help-keys (command context)
  "Return the keys that run COMMAND in CONTEXT, as (KEY . ANNOTATION) pairs.
COMMAND is one command or a list of commands sharing a row; each must be
bound.
Shortest first, then alphabetically: a keymap's own order is the reverse
of its definitions, which says nothing to a reader.  A key bound in
several states is one pair, annotated with its states
(`agent-repl--input-help-state-label', nil for none), and
a run of digits is one range (`agent-repl--input-help-collapse-digits').
A command no map binds is a broken help table, and is fatal."
  (agent-repl--input-help-collapse-digits
   (mapcar (lambda (group)
             (cons (car group) (agent-repl--input-help-group-label (cdr group))))
           (agent-repl--input-help-group-sources
            (mapcan (lambda (one)
                      (or (agent-repl--input-help-key-sources one context)
                          (agent-repl--fatal agent-repl-input-help--log-scope
                                             "elisp.input-help.keys: unbound command=%s" one)))
                    (if (listp command) (copy-sequence command) (list command)))))))

;;;; ---- The popup ------------------------------------------------------

(defun agent-repl--input-help-topic (key value)
  "Return the help topic whose KEY (`:id' or `:title') is VALUE.
No such topic is fatal."
  (or (seq-find (lambda (topic) (equal (plist-get topic key) value)) agent-repl-input-help-topics)
      (agent-repl--fatal agent-repl-input-help--log-scope
                         "elisp.input-help.topic: unknown %s=%S known=%S"
                         key value (mapcar (lambda (topic) (plist-get topic key))
                                           agent-repl-input-help-topics))))

(defconst agent-repl-input-help--key-column-max 24
  "The widest key cell a popup row keeps beside its description.
A wider cell puts its description on the next line, at the same column.")

(defun agent-repl--input-help-key-cell (keys)
  "Return KEYS, (KEY . ANNOTATION) pairs, as one row's keycaps."
  (mapconcat (lambda (key)
               (concat (propertize (car key) 'face 'help-key-binding)
                       (and (cdr key)
                            (concat " " (propertize (cdr key) 'face 'agent-repl-input-help-annotation)))))
             keys "  "))

(defun agent-repl--input-help-rows (topic context)
  "Return TOPIC's rows as (CELL DESCRIPTION VARIABLES), keys read from CONTEXT.
Every key and variable is resolved here, before any buffer is touched: a
command no map binds, or a variable that is not bound, is fatal."
  (mapcar (lambda (binding)
            (pcase-let ((`(,command ,description . ,props) binding))
              (let ((variables (plist-get props :variables)))
                (dolist (variable variables)
                  (unless (boundp variable)
                    (agent-repl--fatal agent-repl-input-help--log-scope
                                       "elisp.input-help.rows: unbound variable=%s command=%s"
                                       variable command)))
                (list (agent-repl--input-help-key-cell
                       (agent-repl--input-help-keys command context))
                      description variables))))
          (plist-get topic :bindings)))

(defun agent-repl--input-help-variable-value (variable)
  "Return VARIABLE's current value as shown: printed, newlines escaped.
Shared by the help popup and the Settings page (settings.el)."
  (let ((print-escape-newlines t))
    (propertize (prin1-to-string (symbol-value variable)) 'face 'font-lock-string-face)))

(defun agent-repl--input-help-variable-line (variable)
  "Return VARIABLE's line: its name, a button that describes it, and its value."
  (concat (make-text-button (symbol-name variable) nil
                            'face 'agent-repl-input-help-variable
                            'action (lambda (_) (describe-variable variable))
                            'help-echo (concat "mouse-1: describe " (symbol-name variable)))
          ": "
          (agent-repl--input-help-variable-value variable)))

(defun agent-repl--input-help-insert-row (cell description lines column)
  "Insert a row: CELL, then DESCRIPTION and each of LINES at COLUMN.
The text wraps back to COLUMN, and a CELL too wide for COLUMN
puts DESCRIPTION on the next line.  Shared by the help popup and the
Settings page (settings.el), so their rows are laid out alike."
  (let ((start (point))
        (align `(space :align-to ,column)))
    (insert "  " cell)
    (when (> (+ 2 (string-width cell)) (- column 2))
      (insert "\n"))
    (insert (propertize " " 'display align) description "\n")
    (dolist (line lines)
      (insert (propertize " " 'display align) line "\n"))
    (put-text-property start (point) 'wrap-prefix align)))

(defun agent-repl--input-help-key-column (cells)
  "Return the description column for rows whose key cells are CELLS."
  (+ 2 (min agent-repl-input-help--key-column-max
            (apply #'max 0 (mapcar #'string-width cells)))
     3))

(defun agent-repl--input-help-insert (topic rows)
  "Insert TOPIC's help, with its resolved ROWS, at point."
  (let ((column (agent-repl--input-help-key-column (mapcar #'car rows)))
        (features (plist-get topic :features)))
    (insert (propertize (plist-get topic :title) 'face 'agent-repl-input-help-title) "\n")
    (when rows
      (insert "\n" (propertize "Keys" 'face 'agent-repl-input-help-section) "\n")
      (pcase-dolist (`(,cell ,description ,variables) rows)
        (agent-repl--input-help-insert-row
         cell description (mapcar #'agent-repl--input-help-variable-line variables) column)))
    (when features
      (insert "\n" (propertize "Good to know" 'face 'agent-repl-input-help-section) "\n")
      (dolist (line features)
        (let ((start (point)))
          (insert "  • " line "\n")
          (put-text-property start (point) 'wrap-prefix "    "))))))

(defun agent-repl--input-help-buffer (topic context)
  "Return TOPIC's help buffer, filled from CONTEXT and read-only.
Its lines wrap at word boundaries, each under its own column."
  (let* ((rows (agent-repl--input-help-rows topic context))
         (buf (get-buffer-create (format "*agent-repl help: %s*" (plist-get topic :title)))))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-repl--input-help-insert topic rows))
      (setq-local truncate-lines nil)
      (setq-local line-spacing 0.2)
      (visual-line-mode 1)
      (goto-char (point-min)))
    buf))

(defun agent-repl-input-help-show (id)
  "Show help topic ID's keys and features in the shared agent-repl popup."
  (let* ((topic (agent-repl--input-help-topic :id id))
         (buf (agent-repl--input-help-buffer topic (agent-repl--input-help-binding-context topic))))
    (agent-repl--log agent-repl-input-help--log-scope "elisp.input-help.show: id=%s buffer=%s"
                     id (buffer-name buf))
    (agent-repl-popup-show buf)))

(defun agent-repl-input-help-click (event)
  "Drop the topic menu at the help button's click EVENT.
Choosing a topic shows it; dismissing the menu does nothing."
  (interactive "e")
  (agent-repl--log agent-repl-input-help--log-scope "elisp.input-help.click: menu")
  (popup-menu (agent-repl--input-help-menu) event))

(defun agent-repl--input-help-choice (topic)
  "Return TOPIC's name in `agent-repl-input-help''s completion: GROUP: TITLE."
  (concat (plist-get topic :group) ": " (plist-get topic :title)))

(defun agent-repl-input-help (choice)
  "Show the help topic CHOICE names, read with completion as GROUP: TITLE."
  (interactive
   (list (completing-read "agent-repl help: "
                          (mapcar #'agent-repl--input-help-choice agent-repl-input-help-topics)
                          nil t)))
  (agent-repl-input-help-show
   (plist-get (or (seq-find (lambda (topic) (equal (agent-repl--input-help-choice topic) choice))
                            agent-repl-input-help-topics)
                  (agent-repl--fatal agent-repl-input-help--log-scope
                                     "elisp.input-help: unknown choice=%S" choice))
              :id)))

(agent-repl--input-help-install-in-composers)

(provide 'input-help)

;;; input-help.el ends here
