;;; commands.el --- user commands for claude-repl -*- lexical-binding: t; -*-

;;; Code:

;; Forward declarations: defined in worktree.el (loaded after commands.el).
;; Snapshot save/load helpers and the interactive drain command in this
;; file refer to these symbols, so the names must be readable here at
;; compile/load time.
(defvar claude-repl--merge-queue)
(declare-function claude-repl--drain-merge-queue "worktree")
(declare-function claude-repl--any-cherry-pick-in-progress-p "worktree")

;;;; Customization — prompts & diff specs

(defcustom claude-repl-branch-diff-spec
  "changes in current branch (git diff $(git merge-base HEAD origin/master))"
  "Change-spec string used for branch-level diff analysis commands."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-explain-diff-prompt
  "please explain the changes"
  "Prompt sent to Claude by explain-diff commands."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-update-pr-diff-prompt
  "please update the PR description"
  "Prompt sent to Claude by update-pr-diff commands."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-update-pr-prompt
  "please update the PR description for the PR corresponding to our branch"
  "Prompt sent to Claude by `claude-repl-update-pr'."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-rebase-onto-origin-master-prompt
  "please rebase the current branch onto origin/master (I already ran `git fetch origin` for you), resolving any conflicts as appropriate"
  "Prompt sent to Claude by `claude-repl-rebase-onto-origin-master'."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-create-or-update-pr-base-flags
  '("commit" "--patch" "--self-certified" "--add-to-merge-queue" "--skip-tests")
  "Default flag list for the /create-or-update-pr slash command.
`claude-repl-create-or-update-pr' joins these into the prompt, dropping
any flag whose exclusion symbol appears in its EXCLUDED argument."
  :type '(repeat string)
  :group 'claude-repl)

(defcustom claude-repl-run-tests-prompt
  "please run tests, and summarize the issues found and probable causes"
  "Prompt sent to Claude by run-tests commands."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-run-lint-prompt
  "please run lint, and address any issues found"
  "Prompt sent to Claude by run-lint commands."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-run-all-prompt
  "please run lint and tests, and address any issues found for both"
  "Prompt sent to Claude by run-all commands."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-test-quality-prompt
  "please analyze tests to ensure they are following AAA standards for testing. Please be sure to confine your analysis to the specified context (branch, HEAD, uncommitted changes, etc). They should be employing DRY principle for refactoring as well (extract repeated code into helpers, use builder pattern to facilitate test DSL). We should only be testing one thing per test (can extract tests into subtests to ensure this). Ensure that tests are correctly grouped into subtests, and that very similar/redundant suites are merged. We should not be using ANY timing logic in tests. If there is any timing logic found, surface it. It is FINE for potentially hanging tests to become unblocked with ERROR after some amount of time -- we are only concerned with not attempting to ballpark synchronization via time. We should be careful to NOT reduce the production code path coverage of our refactors -- for example, we should avoid removing asserts in the effort to 'only test one thing', and instead prefer adding a new subtest. Please spin up ONE AGENT PER TEST FILE!"
  "Prompt sent to Claude by test-quality commands."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-test-coverage-prompt
  "Please be sure to confine your analysis to the specified context (branch, HEAD, uncommitted changes, etc). <<IF AND ONLY IF YOU JUST PRODUCED A LIST OF EDGE CASES>>: write up a plan for producing a unit test that covers each and every one of the edge cases you just enumerated. Each test should cover *precisely* one edge case. Each test file should be worked on by a separate agent. <<IF AND ONLY IF YOU DID NOT -- I REPEAT, NOT -- JUST PRODUCE A LIST OF EDGE CASES IN YOUR LAST RESPONSE MESSAGE>>: please enumerate each and every edge cases introduced or modified by each and every function added or modified."
  "Prompt sent to Claude by test-coverage commands."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-diff-analysis-message-template "for the %s, %s"
  "Format string for diff analysis messages sent to Claude.
First %s is the change-spec, second %s is the prompt."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-explain-prompt-template "please explain %s"
  "Format string for the explain command prompt.
%s is replaced with the context reference (file:line or file:range)."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-interrupt-escape-count 2
  "Number of Escape key presses sent to interrupt Claude."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-interrupt-reinsert-delay 0.25
  "Seconds to wait after interrupting before re-entering insert mode."
  :type 'number
  :group 'claude-repl)

;;;; Session helpers

(defun claude-repl--send-to-claude (text)
  "Send TEXT to Claude, starting it if needed."
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "send-to-claude len=%d" (length text))
    (unless (claude-repl--claude-running-p ws)
      (claude-repl--initialize-claude ws))
    (claude-repl--send-input-to-vterm
     (claude-repl--ws-get ws :vterm-buffer) text)))

;;;; File reference helpers

(defun claude-repl--buffer-relative-path ()
  "Return the current buffer's file path relative to the project root."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (let ((rel (file-relative-name (claude-repl--path-canonical file) (claude-repl--ws-dir (+workspace-current-name)))))
      (claude-repl--log (+workspace-current-name) "buffer-relative-path: path=%s" rel)
      rel)))

(defun claude-repl--format-file-ref ()
  "Return a file:line or file:startline-endline reference string.
With active region: returns file:startline-endline and deactivates mark.
Without region: returns file:line."
  (let ((rel (claude-repl--buffer-relative-path)))
    (if (use-region-p)
        (let ((start-line (line-number-at-pos (region-beginning)))
              (end-line (line-number-at-pos (region-end))))
          (deactivate-mark)
          (claude-repl--log (+workspace-current-name) "format-file-ref: region branch start=%d end=%d" start-line end-line)
          (format "%s:%d-%d" rel start-line end-line))
      (claude-repl--log (+workspace-current-name) "format-file-ref: single-line branch line=%d" (line-number-at-pos (point)))
      (format "%s:%d" rel (line-number-at-pos (point))))))

(defun claude-repl--format-magit-hunk-ref ()
  "Format a file reference for the current magit hunk context.
Returns a \"file:startline-endline\" string based on the hunk's to-range."
  (let* ((section (magit-current-section))
         (file (magit-file-at-point))
         (range (eieio-oref section 'to-range))
         (start (car range))
         (len (cadr range))
         (end (+ start len -1))
         (rel (file-relative-name
               (claude-repl--path-canonical (expand-file-name file (magit-toplevel)))
               (claude-repl--ws-dir (+workspace-current-name))))
         (ref (format "%s:%d-%d" rel start end)))
    (claude-repl--log (+workspace-current-name) "format-magit-hunk-ref: ref=%s" ref)
    ref))

(defun claude-repl--context-reference ()
  "Return a context-appropriate file reference string.
In a magit hunk: returns the hunk's file:startline-endline.
Otherwise: delegates to `claude-repl--format-file-ref' (which handles
both active region and point-at-line cases)."
  (if (and (derived-mode-p 'magit-diff-mode 'magit-status-mode
                           'magit-revision-mode)
           (magit-section-match 'hunk))
      (progn
        (claude-repl--log (+workspace-current-name) "context-reference: magit-hunk branch")
        (claude-repl--format-magit-hunk-ref))
    (claude-repl--log (+workspace-current-name) "context-reference: standard branch")
    (claude-repl--format-file-ref)))

;;;; Diff analysis infrastructure

(defun claude-repl--send-diff-analysis (change-spec prompt)
  "Send a diff analysis request to Claude.
CHANGE-SPEC describes which changes (e.g. \"unstaged changes (git diff)\").
PROMPT is the analysis instruction."
  (let ((msg (format claude-repl-diff-analysis-message-template change-spec prompt)))
    (claude-repl--log (+workspace-current-name) "diff-analysis: %s" change-spec)
    (claude-repl--send-to-claude msg)))

(defconst claude-repl--diff-scopes
  '((worktree    . "unstaged changes (git diff)")
    (staged      . "staged changes (git diff --cached)")
    (uncommitted . "uncommitted changes (git diff HEAD)")
    (head        . "last commit (git show HEAD)")
    (branch      . :use-branch-diff-spec))
  "Alist mapping scope names to their change-spec strings.
The special value `:use-branch-diff-spec' means use `claude-repl-branch-diff-spec'.")

(defconst claude-repl--diff-scope-labels
  '((worktree    . "unstaged changes")
    (staged      . "staged changes")
    (uncommitted . "all uncommitted changes")
    (head        . "the last commit")
    (branch      . "all changes in the current branch"))
  "Alist mapping scope symbols to human-readable labels for docstrings.")

(defconst claude-repl--update-pr-diff-scopes
  '((worktree    . "UNSTAGED changes (git diff). Do not consider staged changes or committed changes.")
    (staged      . "STAGED changes (git diff --cached). Do not consider unstaged changes or committed changes.")
    (uncommitted . "All UNCOMMITTED changes (git diff HEAD). Consider BOTH staged and unstaged changes. Do not consider committed changes.")
    (head        . "last commit (git show HEAD)."))
  "Scope overrides for `update-pr-diff' commands.
These provide more explicit instructions than the standard scopes.
The `branch' scope is omitted and falls through to the default.")

(defun claude-repl--resolve-change-spec (scope default-spec scope-overrides)
  "Resolve the change-spec form for SCOPE.
DEFAULT-SPEC is the value from `claude-repl--diff-scopes'.
SCOPE-OVERRIDES, when non-nil, is a symbol naming an alist of
 (SCOPE . CHANGE-SPEC) that takes precedence over DEFAULT-SPEC.
Returns a string literal or the symbol `claude-repl-branch-diff-spec'."
  (let ((override (and scope-overrides
                       (cdr (assq scope (eval scope-overrides))))))
    (cond
     (override
      (claude-repl--log nil "resolve-change-spec: override branch scope=%s" scope)
      override)
     ((eq default-spec :use-branch-diff-spec)
      (claude-repl--log nil "resolve-change-spec: branch-spec branch scope=%s" scope)
      'claude-repl-branch-diff-spec)
     (t
      (claude-repl--log nil "resolve-change-spec: default branch scope=%s" scope)
      default-spec))))

(defun claude-repl--diff-command-form (scope-entry family doc-verb prompt-var scope-overrides)
  "Build one `defun' form for a diff-analysis command.
SCOPE-ENTRY is a (SCOPE . DEFAULT-SPEC) pair from `claude-repl--diff-scopes'.
FAMILY, DOC-VERB, PROMPT-VAR, and SCOPE-OVERRIDES are forwarded from the
macro `claude-repl--define-diff-commands'."
  (let* ((scope (car scope-entry))
         (doc-scope (cdr (assq scope claude-repl--diff-scope-labels)))
         (fn-name (intern (format "claude-repl-%s-%s" family scope)))
         (change-spec-form (claude-repl--resolve-change-spec
                            scope (cdr scope-entry) scope-overrides)))
    `(defun ,fn-name ()
       ,(format "%s %s." doc-verb doc-scope)
       (interactive)
       (claude-repl--send-diff-analysis ,change-spec-form ,prompt-var))))

(defmacro claude-repl--define-diff-commands (family doc-verb prompt-var &optional scope-overrides)
  "Define 5 diff-analysis commands for FAMILY.

Each generated command is named `claude-repl-FAMILY-SCOPE' for SCOPE in
worktree, staged, uncommitted, head, and branch.  DOC-VERB is used in
docstrings (e.g. \"Explain\" produces \"Explain unstaged changes.\").
PROMPT-VAR is the symbol of the prompt variable to pass.

SCOPE-OVERRIDES, when non-nil, is a symbol naming an alist of (SCOPE . CHANGE-SPEC)
that replaces the default change-spec from `claude-repl--diff-scopes'
for specific scopes."
  (declare (indent 2))
  `(progn
     ,@(cl-loop for scope-entry in claude-repl--diff-scopes
                collect (claude-repl--diff-command-form
                         scope-entry
                         family doc-verb prompt-var scope-overrides))))

;;;; Diff command families

(claude-repl--define-diff-commands explain-diff "Explain"
  claude-repl-explain-diff-prompt)

(claude-repl--define-diff-commands update-pr-diff "Update the PR description for"
  claude-repl-update-pr-diff-prompt
  claude-repl--update-pr-diff-scopes)

(claude-repl--define-diff-commands run-tests "Run tests for"
  claude-repl-run-tests-prompt)

(claude-repl--define-diff-commands run-lint "Run lint for"
  claude-repl-run-lint-prompt)

(claude-repl--define-diff-commands run-all "Run lint and tests for"
  claude-repl-run-all-prompt)

(claude-repl--define-diff-commands test-quality "Analyze test quality for"
  claude-repl-test-quality-prompt)

(claude-repl--define-diff-commands test-coverage "Analyze test coverage for"
  claude-repl-test-coverage-prompt)

;;;; Standalone commands

(defun claude-repl-explain ()
  "Ask Claude to explain the current context.
In a magit hunk: sends the hunk's file path and line range.
With active region: sends file path and line range.
Without region: sends file path and current line."
  (interactive)
  (let* ((ref (claude-repl--context-reference))
         (msg (format claude-repl-explain-prompt-template ref)))
    (claude-repl--log (+workspace-current-name) "explain %s" msg)
    (claude-repl--send-to-claude msg)))

(defun claude-repl-explain-prompt ()
  "Prompt the user for a message to send to Claude about the current context.
Pre-fills the minibuffer with the context reference (file:line or file:range).
In a magit hunk: pre-fills with the hunk's file path and line range.
With active region: pre-fills with file path and line range.
Without region: pre-fills with file path and current line."
  (interactive)
  (let* ((ref (claude-repl--context-reference))
         (msg (read-string "Send to Claude: " ref)))
    (when (and msg (not (string-empty-p msg)))
      (claude-repl--log (+workspace-current-name) "explain-prompt %s" msg)
      (claude-repl--send-to-claude msg))))

;;;; Explain config -- read-only Q&A about this doom config via headless claude

(defcustom claude-repl-explain-config-dir "~/.config/doom"
  "Working directory for the headless `claude -p' spawned by
`claude-repl-explain-config'.  Resolves to the canonical doom config
checkout (not the current worktree) so the explainer sees the user's
installed configuration."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-explain-config-program "claude"
  "Executable invoked by `claude-repl-explain-config'."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-explain-config-flags
  '("-p" "--model" "haiku" "--dangerously-skip-permissions")
  "Argument list for the headless claude invocation.
`-p' makes claude exit after one turn; `--model haiku' pins the small,
fast model (explain-config is short-form Q&A — no need for the
default-tier model); `--dangerously-skip-permissions' prevents the run
from prompting for tool approval headlessly."
  :type '(repeat string)
  :group 'claude-repl)

(defcustom claude-repl-explain-config-buffer-name "*claude-explain-config*"
  "Buffer name where explain-config output is collected and displayed."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-explain-config-width-fraction 0.5
  "Fraction of frame width for the explain-config right-side popup.
Only applies when the popup falls back to its own side window (i.e.
when the claude output window is not visible to take over).  Width
is inherited from the claude output window when the popup takes
that window over — see `claude-repl--explain-config-show'.  The
drawer is untouched in either branch."
  :type 'float
  :group 'claude-repl)

(defun claude-repl--explain-config-window-width (window)
  "Return the configured explain-config width in columns for WINDOW.
Resolves `claude-repl-explain-config-width-fraction' against the
host frame's width."
  (let ((frame-cols (frame-width (window-frame window))))
    (max 1 (round (* claude-repl-explain-config-width-fraction frame-cols)))))

(defvar claude-repl--explain-config-display-action
  `((display-buffer-in-side-window)
    (side . right)
    (slot . 0)
    (window-width . ,#'claude-repl--explain-config-window-width)
    (window-parameters
     (no-delete-other-windows . t)
     (no-other-window . nil)))
  "Fallback display action for the explain-config output buffer.
Used only when the claude output window is not visible to take
over — when it is, `--show' reuses it directly via
`set-window-buffer' and bypasses `display-buffer' entirely.  The
drawer is never touched.  Reconciled across workspace switches via
the persp-activated hook.")

(defvar claude-repl--explain-config-global-visible-p nil
  "Non-nil when the explain-config popup should appear in every persp.
Set by `claude-repl--explain-config-show', cleared by
`claude-repl--explain-config-hide'.  The persp-activated hook
(`claude-repl--explain-config-ensure-visible-on-persp-switch')
consults this flag and re-displays the popup in newly-activated
workspaces so it feels like a frame-level UI element rather than a
per-workspace artifact — mirrors the drawer's own
`--global-visible-p' pattern.")

(defvar claude-repl--explain-config-replaced-window nil
  "When the popup has taken over the claude output window, holds (WIN . PREV-BUF).
WIN is the live claude output window the popup took over; PREV-BUF
is the buffer that window was displaying before takeover (the
claude output buffer for the current workspace).  Nil when the
popup is hosted in its own side window (i.e. the claude output
window was not visible at show time).  Consumed by
`claude-repl--explain-config-hide' to restore the prior buffer in
the same window position when the popup closes.")

(defun claude-repl--explain-config-apply-width (window)
  "Resize WINDOW to the configured explain-config width.
Side-window action alists honor `window-width' only at window-creation
time, so re-displaying the popup keeps its old width if the fraction
changed.  This forces the resize on every show — mirrors the drawer's
`--apply-width'."
  (let* ((target (claude-repl--explain-config-window-width window))
         (window-min-width 1))
    (with-selected-window window
      (setq-local window-size-fixed nil)
      (let ((delta (- target (window-total-width window))))
        (cond
         ((> delta 0) (enlarge-window delta t))
         ((< delta 0) (shrink-window (abs delta) t)))))))

(defun claude-repl--explain-config-current-claude-output-window ()
  "Return the live claude output window in the selected frame, or nil.
Looks up the current workspace's claude output panel via
`claude-repl-window--panel-window' with the `:vterm' key (the
existing panel-lookup key — note we do NOT introduce that name
here, the popup itself only deals in \"claude output\").  Guards
on `fboundp' so callers in load order before panels.el (e.g. early
test harnesses) get nil instead of a void-function error."
  (and (fboundp 'claude-repl-window--panel-window)
       (claude-repl-window--panel-window :vterm)))

(defun claude-repl--explain-config-take-over-claude-output-window (output-win buf)
  "Swap OUTPUT-WIN's buffer for BUF and record the original for restoration.
The claude output panel is a dedicated window, so this temporarily
clears `window-dedicated-p' before `set-window-buffer' — otherwise
the swap errors.  The pre-swap buffer is stashed in
`claude-repl--explain-config-replaced-window' so
`claude-repl--explain-config-hide' can restore it.  Returns OUTPUT-WIN."
  (let ((prev-buf (window-buffer output-win)))
    (set-window-dedicated-p output-win nil)
    (set-window-buffer output-win buf)
    (setq claude-repl--explain-config-replaced-window
          (cons output-win prev-buf)))
  output-win)

(defun claude-repl--explain-config-restore-replaced-window ()
  "Restore the buffer in the window the popup took over, if any.
No-op when no window was replaced or when the window or its prior
buffer is no longer live.  Re-applies the claude output window
hardening (dedicate / size-fix / delete-protect) on success so the
restored window matches its original recipe."
  (when-let ((cell claude-repl--explain-config-replaced-window))
    (setq claude-repl--explain-config-replaced-window nil)
    (let ((win (car cell))
          (prev (cdr cell)))
      (when (and (window-live-p win) (buffer-live-p prev))
        (set-window-buffer win prev)
        (when (fboundp 'claude-repl--configure-vterm-window)
          (claude-repl--configure-vterm-window win))))))

(defun claude-repl--explain-config-show ()
  "Display the explain-config buffer.
Sets the global visible-flag so the popup follows the user across
workspace switches.  No-op when the buffer doesn't exist (nothing
to show yet).

Display priority:

  1. If a window already displays the buffer, leave it in place
     (and re-apply the side-window width unless it is the stolen
     claude output window — stolen windows inherit the prior
     window's width).
  2. Otherwise, if the claude output window is visible, take it
     over via `set-window-buffer' and record the prior buffer so
     `--hide' can restore it.
  3. Otherwise, fall back to the right-side popup display action.

The drawer is never touched in any branch — its visibility is its
own concern.  Returns the displayed window or nil."
  (when-let ((buf (get-buffer claude-repl-explain-config-buffer-name)))
    (setq claude-repl--explain-config-global-visible-p t)
    (let ((existing (get-buffer-window buf t)))
      (cond
       ((window-live-p existing)
        (unless (and claude-repl--explain-config-replaced-window
                     (eq existing (car claude-repl--explain-config-replaced-window)))
          (claude-repl--explain-config-apply-width existing))
        existing)
       ((claude-repl--explain-config-current-claude-output-window)
        (claude-repl--explain-config-take-over-claude-output-window
         (claude-repl--explain-config-current-claude-output-window) buf))
       (t
        (let ((win (display-buffer buf claude-repl--explain-config-display-action)))
          (when (window-live-p win)
            (claude-repl--explain-config-apply-width win))
          win))))))

(defun claude-repl--explain-config-hide ()
  "Hide the explain-config buffer.
Clears the global visible-flag so the popup no longer auto-appears
on workspace switches.  Keeps the buffer itself alive — only its
visibility is toggled.

If `--show' took over the claude output window, restores the prior
buffer in that window via `--restore-replaced-window'.  Any
remaining windows still displaying the explain-config buffer (e.g.
side-window fallbacks) are deleted.  The drawer is never touched."
  (setq claude-repl--explain-config-global-visible-p nil)
  (claude-repl--explain-config-restore-replaced-window)
  (when-let ((buf (get-buffer claude-repl-explain-config-buffer-name)))
    (claude-repl-window--delete-buffer-windows buf)))

;;;###autoload
(defun claude-repl-explain-config-close ()
  "Close the explain-config popup everywhere.
Deletes every visible explain-config window in the current frame and
clears the global visible-flag so the popup will not reappear on
workspace switch.  Buffer contents are preserved — re-running
`claude-repl-explain-config' (or reopening via leader) will show them
again with the new question's output appended."
  (interactive)
  (claude-repl--explain-config-hide))

(defvar claude-repl-explain-config-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'claude-repl-explain-config-close)
    map)
  "Keymap for `claude-repl-explain-config-mode'.")

(define-minor-mode claude-repl-explain-config-mode
  "Minor mode enabled in the explain-config output popup.
Provides a buffer-local `q' binding to dismiss the popup globally via
`claude-repl-explain-config-close', so the user does not need to
navigate-and-`C-x 0' the window in every workspace separately."
  :lighter " ExplainCfg"
  :keymap claude-repl-explain-config-mode-map)

(defun claude-repl--explain-config-ensure-visible-on-persp-switch (&rest _)
  "Reconcile explain-config visibility with the global state on workspace switch.
Mirrors `claude-repl-drawer--ensure-visible-on-persp-switch' — when
the flag says show but the popup is missing in the activated persp,
re-display it via `claude-repl--explain-config-show' (which will
take over the new persp's claude output window if visible, else
fall back to the side-window display action).  When the flag says
hide but persp-mode restored a stale window, delete it.

Drops a stale `--replaced-window' whose target window is no longer
live before re-showing — the cell belongs to the persp we left, not
the one we just activated."
  (let* ((buf (get-buffer claude-repl-explain-config-buffer-name))
         (win (and buf (get-buffer-window buf))))
    (cond
     ((and claude-repl--explain-config-global-visible-p buf (not win))
      (when (and claude-repl--explain-config-replaced-window
                 (not (window-live-p
                       (car claude-repl--explain-config-replaced-window))))
        (setq claude-repl--explain-config-replaced-window nil))
      (claude-repl--explain-config-show))
     ((and (not claude-repl--explain-config-global-visible-p) win)
      (claude-repl-window--delete-buffer-windows buf)))))

(with-eval-after-load 'persp-mode
  (add-hook 'persp-activated-functions
            #'claude-repl--explain-config-ensure-visible-on-persp-switch))

(defcustom claude-repl-explain-config-preamble
  (concat
   "You are being asked a question about the Doom Emacs configuration"
   " in this repository (~/.config/doom), with particular emphasis on"
   " `modules/app/claude-repl/' (the Claude REPL integration).  The"
   " user wants an EXPLANATION or CLARIFICATION of how the config or"
   " its capabilities work.  This is NOT a call to action.\n"
   "\n"
   "STRICT CONSTRAINT -- READ-ONLY: You MUST NOT take any mutating"
   " action of any kind.  Do NOT edit files, do NOT run shell commands"
   " that change state, do NOT perform git operations, do NOT install"
   " or uninstall anything, do NOT rebuild, do NOT restart any process,"
   " do NOT send messages, and do NOT alter the system in any way."
   " Read-only tools (reading files, grepping, listing files, code"
   " search) are fine and encouraged for grounding your answer."
   " Anything write-side is FORBIDDEN.\n"
   "\n"
   "If the user's question below appears to be a disguised request to"
   " make changes (e.g. \"fix\", \"add\", \"refactor\", \"change\","
   " \"implement\", \"create\", \"update\", \"delete\", \"rename\","
   " or any other imperative implying side effects on the repo or"
   " system), REFUSE to act and respond by explaining that this entry"
   " point is for clarification and explanation only, and that the"
   " user should re-issue the request through the appropriate Claude"
   " REPL workspace command if they want changes made.\n"
   "\n"
   "OUTPUT FORMAT -- EMACS FACE MARKUP (NOT MARKDOWN):\n"
   "Your answer is rendered directly into an Emacs buffer.  DO NOT use"
   " Markdown (no `#` / `##` / `###` headers, no `**bold**`, no"
   " `*italic*`, no `` ```fences``` ``, no `- ` bullet syntax, no `>`"
   " quote prefix, no `[text](url)` links).  Instead, use the following"
   " face tags exactly.  Each tag pair wraps the text it styles; the"
   " open tag is `⟦NAME⟧`, the close tag is `⟦/NAME⟧`."
   "  Tags may nest (e.g. bold inside a heading).  Available tags:\n"
   "\n"
   "  ⟦h1⟧Top-level heading⟦/h1⟧\n"
   "  ⟦h2⟧Subheading⟦/h2⟧\n"
   "  ⟦h3⟧Sub-subheading⟦/h3⟧\n"
   "  ⟦b⟧bold⟦/b⟧   ⟦i⟧italic⟦/i⟧\n"
   "  ⟦code⟧inline-code⟦/code⟧\n"
   "  ⟦block⟧multi-line\n"
   "  code block⟦/block⟧\n"
   "  ⟦quote⟧blockquote text⟦/quote⟧\n"
   "  ⟦link⟧https://example.com⟦/link⟧\n"
   "  ⟦bullet⟧• list item label⟦/bullet⟧ then body text\n"
   "  ⟦kbd⟧SPC j h c⟦/kbd⟧   ⟦file⟧modules/app/claude-repl/commands.el⟦/file⟧\n"
   "  ⟦sym⟧claude-repl-explain-config⟦/sym⟧\n"
   "\n"
   "ESCAPE HATCH -- RAW EMACS LISP FACE PLISTS:\n"
   "When no semantic tag above fits the styling you want, use the raw-face"
   " tag: ⟦face PLIST⟧text⟦/face⟧ where PLIST is a raw Emacs Lisp face"
   " plist.  Examples:\n"
   "\n"
   "  ⟦face :foreground \"#FF7F7F\" :weight bold⟧critical warning⟦/face⟧\n"
   "  ⟦face :background \"#2A4A2A\" :foreground \"#8DE08D\"⟧success⟦/face⟧\n"
   "  ⟦face :slant italic :underline t⟧emphasis⟦/face⟧\n"
   "  ⟦face :box (:line-width 1 :color \"#888\")⟧boxed⟦/face⟧\n"
   "\n"
   "Permitted plist attributes: :foreground, :background, :weight, :slant,"
   " :underline, :overline, :strike-through, :box, :height.  Any other"
   " attribute is rejected and the tag renders verbatim.  Colors may be"
   " any string Emacs accepts (`\"red\"`, `\"#FF0000\"`).  The close tag"
   " is always plain ⟦/face⟧ -- do not repeat the plist.\n"
   "\n"
   "Rules for face markup:\n"
   "  - Plain prose needs NO tags -- only tag the things that benefit"
   " from emphasis or semantic styling.\n"
   "  - PREFER semantic tags over ⟦face …⟧.  Reach for the escape hatch"
   " only when no semantic tag captures the styling you want (e.g. a"
   " one-off color for a warning).\n"
   "  - Always close every tag you open.  Close in LIFO order when"
   " nesting.\n"
   "  - The ⟦ ⟧ brackets are U+27E6 / U+27E7 -- use those exact"
   " characters, not `[[` / `]]`.\n"
   "  - Do NOT emit literal `⟦` or `⟧` inside tagged content -- they"
   " are reserved as tag delimiters.  This applies inside ⟦face …⟧"
   " plists too: do not embed `⟦` or `⟧` in plist values.\n"
   "  - Unknown tag names render verbatim, so stick to the list above"
   " (plus the ⟦face …⟧ escape hatch).\n"
   "  - Use ⟦block⟧ for any multi-line code or shell example; use"
   " ⟦code⟧ only for short inline references.\n"
   "\n"
   "Answer the user's question below as a concise, accurate explanation"
   " grounded in the actual code, formatted using the face tags above.\n"
   "\n"
   "QUESTION:\n"
   "%s")
  "Format string wrapping the user's question before sending to claude.
`%s' is replaced with the raw question.  The preamble is the read-only
contract for this entry point -- edit with care."
  :type 'string
  :group 'claude-repl)

(defun claude-repl--explain-config-build-input (raw)
  "Wrap RAW with the explain-config preamble for sending to claude."
  (format claude-repl-explain-config-preamble raw))

;;; Rich-text faces for the model's tagged response body.  The model
;;; emits semantic face tags (see `claude-repl-explain-config-preamble');
;;; the streaming filter (`claude-repl--explain-config-filter') parses
;;; them and applies these faces to the inserted text so the buffer
;;; renders with Emacs's own font system instead of raw Markdown.

(defface claude-repl-explain-config-h1
  '((t :inherit org-level-1 :weight bold :height 1.25))
  "Face for ⟦h1⟧ headings in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-h2
  '((t :inherit org-level-2 :weight bold :height 1.15))
  "Face for ⟦h2⟧ headings in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-h3
  '((t :inherit org-level-3 :weight bold))
  "Face for ⟦h3⟧ headings in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-bold
  '((t :inherit bold))
  "Face for ⟦b⟧ bold spans in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-italic
  '((t :inherit italic))
  "Face for ⟦i⟧ italic spans in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-code
  '((t :inherit font-lock-constant-face :background "#2A2A2A"))
  "Face for ⟦code⟧ inline-code spans in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-block
  '((t :inherit org-block :extend t))
  "Face for ⟦block⟧ multi-line code blocks in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-quote
  '((t :inherit org-quote :slant italic))
  "Face for ⟦quote⟧ blockquotes in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-link
  '((t :inherit link))
  "Face for ⟦link⟧ URLs in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-bullet
  '((t :inherit org-list-dt :weight bold))
  "Face for ⟦bullet⟧ list-item labels in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-kbd
  '((t :inherit help-key-binding))
  "Face for ⟦kbd⟧ keybinding spans in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-file
  '((t :inherit font-lock-string-face :underline t))
  "Face for ⟦file⟧ filepath spans in explain-config output."
  :group 'claude-repl)

(defface claude-repl-explain-config-sym
  '((t :inherit font-lock-function-name-face))
  "Face for ⟦sym⟧ identifier spans in explain-config output."
  :group 'claude-repl)

(defconst claude-repl--explain-config-face-map
  '(("h1"     . claude-repl-explain-config-h1)
    ("h2"     . claude-repl-explain-config-h2)
    ("h3"     . claude-repl-explain-config-h3)
    ("b"      . claude-repl-explain-config-bold)
    ("i"      . claude-repl-explain-config-italic)
    ("code"   . claude-repl-explain-config-code)
    ("block"  . claude-repl-explain-config-block)
    ("quote"  . claude-repl-explain-config-quote)
    ("link"   . claude-repl-explain-config-link)
    ("bullet" . claude-repl-explain-config-bullet)
    ("kbd"    . claude-repl-explain-config-kbd)
    ("file"   . claude-repl-explain-config-file)
    ("sym"    . claude-repl-explain-config-sym))
  "Mapping of face-tag names (as emitted by the model) to Emacs faces.
Unknown tag names render verbatim and apply no face.")

(defconst claude-repl--explain-config-tag-re
  "⟦\\(/?\\)\\([a-z][a-z0-9]*\\)\\(\\(?: [^⟧]*\\)?\\)⟧"
  "Regex matching one open (`⟦NAME[ ATTRS]⟧') or close (`⟦/NAME⟧') face tag.
Capture groups: (1) slash-or-empty, (2) tag name, (3) optional attrs
payload (leading space included; empty string when no attrs).  ATTRS
is only meaningful for the `face' escape-hatch tag; semantic tags
ignore it.")

(defconst claude-repl--explain-config-partial-tag-re
  "⟦/?[a-z0-9]*\\(?: [^⟧]*\\)?\\'"
  "Regex matching a possibly-incomplete tag at the very end of a string.
Used to defer tag fragments across streaming chunk boundaries.  Now
also buffers `⟦face PLIST' fragments where the plist arrives split
across chunks before the closing `⟧'.")

(defconst claude-repl--explain-config-raw-face-tag-name "face"
  "Magic tag name for the raw-face escape hatch (`⟦face PLIST⟧').
Distinct from any semantic tag in `claude-repl--explain-config-face-map'
so the parser can branch on it.")

(defconst claude-repl--explain-config-face-attr-whitelist
  '(:foreground :background :weight :slant :underline :overline
    :strike-through :box :height)
  "Permitted plist attributes inside a raw-face ⟦face PLIST⟧ tag.
Any plist containing an attribute outside this list is rejected and
the tag renders verbatim — the preamble documents this whitelist to
the model.")

(defun claude-repl--explain-config-parse-face-attrs (attrs)
  "Parse ATTRS (the raw payload from `⟦face PLIST⟧') into a face plist.
Returns the plist on success, or nil if parsing fails or the plist
contains a non-whitelisted attribute.  ATTRS is the captured group 3
from `claude-repl--explain-config-tag-re' — it includes the leading
separating space (or is empty when no attrs were present).

The payload is documented to the model as bare plist tokens (no
surrounding parens), e.g. `:foreground \"red\" :weight bold'.  We
wrap it in parens before `read-from-string' so the entire sequence
parses as one list rather than as a single leading sexp.

Rejecting unknown attributes (rather than silently dropping them)
keeps the model honest about the documented surface and avoids
silently misrendering when the model emits a stray attribute name."
  (let ((trimmed (and attrs (string-trim attrs))))
    (when (and trimmed (not (string-empty-p trimmed)))
      (condition-case nil
          (let* ((wrapped (concat "(" trimmed ")"))
                 (read-result (read-from-string wrapped))
                 (plist (car read-result))
                 (consumed (cdr read-result))
                 (tail (string-trim (substring wrapped consumed))))
            (when (and (listp plist)
                       (zerop (mod (length plist) 2))
                       (string-empty-p tail)
                       (claude-repl--explain-config-plist-keys-valid-p plist))
              plist))
        (error nil)))))

(defun claude-repl--explain-config-plist-keys-valid-p (plist)
  "Return non-nil if every key in PLIST is in the face-attr whitelist."
  (let ((ok t)
        (rest plist))
    (while (and ok rest)
      (unless (memq (car rest) claude-repl--explain-config-face-attr-whitelist)
        (setq ok nil))
      (setq rest (cddr rest)))
    ok))

(defvar-local claude-repl--explain-config-pending ""
  "Accumulated stream bytes not yet flushed to the buffer.
Holds back partial face tags until their closing `⟧' arrives.")

(defvar-local claude-repl--explain-config-face-stack nil
  "Stack of active face symbols (innermost first) for the rendering filter.
Pushed on each open tag, popped on each matching close tag.")

(defconst claude-repl--explain-config-orange "#FF8C42"
  "Claude-orange accent used in the explain-config buffer chrome.")

(defconst claude-repl--explain-config-blue "#7FBFFF"
  "Question-label accent in the explain-config buffer chrome.")

(defconst claude-repl--explain-config-green "#8DE08D"
  "Response/success accent in the explain-config buffer chrome.")

(defconst claude-repl--explain-config-red "#FF7F7F"
  "Failure accent for non-zero exit statuses in the explain-config footer.")

(defconst claude-repl--explain-config-muted "#888888"
  "Muted accent for subtitles and rules in the explain-config chrome.")

(defun claude-repl--explain-config-format-header (prompt)
  "Return the propertized banner inserted at the top of the buffer.
PROMPT is the user's question.  The literal substring \"Question: PROMPT\"
is preserved so downstream tooling can scrape it."
  (let* ((rule (propertize (make-string 72 ?━)
                           'face `(:foreground ,claude-repl--explain-config-orange)))
         (title (propertize "🤖 Claude · Doom Config Q&A"
                            'face `(:foreground ,claude-repl--explain-config-orange
                                    :weight bold)))
         (badge (propertize "🔒 read-only"
                            'face `(:foreground ,claude-repl--explain-config-muted
                                    :slant italic)))
         (q-label (propertize "❓ Question: "
                              'face `(:foreground ,claude-repl--explain-config-blue
                                      :weight bold)))
         (q-body (propertize prompt 'face '(:slant italic)))
         (r-label (propertize "📜 Response"
                              'face `(:foreground ,claude-repl--explain-config-green
                                      :weight bold)))
         (r-tag (propertize " (streaming…)"
                            'face `(:foreground ,claude-repl--explain-config-muted
                                    :slant italic)))
         (r-rule (propertize (concat " " (make-string 50 ?─))
                             'face `(:foreground ,claude-repl--explain-config-green))))
    (concat rule "\n"
            "  " title "   " badge "\n"
            rule "\n\n"
            q-label q-body "\n\n"
            r-label r-tag r-rule "\n\n")))

(defun claude-repl--explain-config-format-footer (status)
  "Return the propertized footer for an explain-config run ending with STATUS."
  (let* ((success (zerop status))
         (emoji (if success "✅" "❌"))
         (color (if success
                    claude-repl--explain-config-green
                  claude-repl--explain-config-red))
         (verb (if success "exited cleanly" "exited with errors"))
         (rule (propertize (make-string 50 ?─) 'face `(:foreground ,color)))
         (body (propertize (format "%s claude %s (status %d)" emoji verb status)
                           'face `(:foreground ,color :weight bold))))
    (concat "\n\n" rule "\n" body "\n")))

(defun claude-repl--explain-config-sentinel (proc _event)
  "Process sentinel for `claude-repl-explain-config'.
Appends an exit-status footer to PROC's buffer when the process exits."
  (when (memq (process-status proc) '(exit signal))
    (let ((status (process-exit-status proc))
          (buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (claude-repl--explain-config-format-footer status))))))))

(defun claude-repl--explain-config-init-buffer (prompt)
  "Prepare the explain-config output buffer for a fresh run.
Erases prior contents, inserts the question header, returns the buffer.
Also resets the streaming filter's per-buffer parser state."
  (let ((buf (get-buffer-create claude-repl-explain-config-buffer-name)))
    (with-current-buffer buf
      (setq claude-repl--explain-config-pending ""
            claude-repl--explain-config-face-stack nil)
      (claude-repl-explain-config-mode 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (claude-repl--explain-config-format-header prompt))))
    buf))

(defun claude-repl--explain-config-current-face (stack)
  "Return the face property value for STACK (innermost first).
nil means no face; a single symbol means a single face; a list means
multiple faces merged in the standard Emacs left-overrides-right order."
  (cond
   ((null stack)        nil)
   ((null (cdr stack))  (car stack))
   (t                   stack)))

(defun claude-repl--explain-config-insert-styled (buf text stack)
  "Insert TEXT into BUF at point-max, propertized with the face for STACK.
No-op if TEXT is empty.  Buffer is treated as read-only-aware."
  (when (and (stringp text) (> (length text) 0))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (face (claude-repl--explain-config-current-face stack)))
        (goto-char (point-max))
        (if face
            (insert (propertize text 'face face))
          (insert text))))))

(defun claude-repl--explain-config-parse-chunk (buf chunk)
  "Parse CHUNK against the face-markup grammar and flush styled text into BUF.
Maintains BUF-local pending bytes and face stack across calls so partial
tags arriving in different process chunks are joined correctly.  Returns
the new pending string (also stored buffer-locally)."
  (with-current-buffer buf
    (let* ((input (concat claude-repl--explain-config-pending (or chunk "")))
           (pos 0)
           (stack claude-repl--explain-config-face-stack))
      (while (string-match claude-repl--explain-config-tag-re input pos)
        (let* ((m-start (match-beginning 0))
               (m-end   (match-end 0))
               (is-close (string= (match-string 1 input) "/"))
               (name    (match-string 2 input))
               (attrs   (match-string 3 input))
               (raw-face-p (string= name claude-repl--explain-config-raw-face-tag-name))
               (face    (cdr (assoc name claude-repl--explain-config-face-map))))
          (claude-repl--explain-config-insert-styled
           buf (substring input pos m-start) stack)
          (cond
           ;; Raw-face escape hatch: ⟦face PLIST⟧ / ⟦/face⟧
           (raw-face-p
            (cond
             ;; Close tag — pop the top entry if it is a plist (i.e. the
             ;; opener pushed one).  Otherwise emit verbatim so an
             ;; unmatched `⟦/face⟧' is visible rather than silently
             ;; consumed.  `(listp (car stack))' distinguishes a plist
             ;; top from a face-symbol top; nil stack falls through to
             ;; the verbatim branch.
             (is-close
              (if (and stack (consp (car stack)))
                  (setq stack (cdr stack))
                (claude-repl--explain-config-insert-styled
                 buf (substring input m-start m-end) stack)))
             ;; Open tag — parse the plist, push if valid; else verbatim.
             ;; On verbatim we deliberately do NOT push a sentinel, so a
             ;; matching `⟦/face⟧' also renders verbatim — keeps open
             ;; and close visible together when the model malformed the
             ;; plist.
             (t (let ((plist (claude-repl--explain-config-parse-face-attrs attrs)))
                  (if plist
                      (setq stack (cons plist stack))
                    (claude-repl--explain-config-insert-styled
                     buf (substring input m-start m-end) stack))))))
           ;; Unknown tag name -- emit verbatim, leave stack untouched.
           ((null face)
            (claude-repl--explain-config-insert-styled
             buf (substring input m-start m-end) stack))
           ;; Close tag -- pop only when it matches innermost open.
           (is-close
            (when (eq (car stack) face)
              (setq stack (cdr stack))))
           ;; Open tag -- push onto stack.  Reject stray attrs on
           ;; semantic tags (the preamble says attrs are only for
           ;; `⟦face …⟧') by emitting the open tag verbatim.
           ((and attrs (not (string-empty-p attrs)))
            (claude-repl--explain-config-insert-styled
             buf (substring input m-start m-end) stack))
           (t (setq stack (cons face stack))))
          (setq pos m-end)))
      (let ((tail (substring input pos)))
        (if (string-match claude-repl--explain-config-partial-tag-re tail)
            (let ((open (match-beginning 0)))
              (claude-repl--explain-config-insert-styled
               buf (substring tail 0 open) stack)
              (setq claude-repl--explain-config-pending (substring tail open)))
          (claude-repl--explain-config-insert-styled buf tail stack)
          (setq claude-repl--explain-config-pending "")))
      (setq claude-repl--explain-config-face-stack stack)
      claude-repl--explain-config-pending)))

(defun claude-repl--explain-config-filter (proc chunk)
  "Process filter for `claude-repl-explain-config'.
Routes CHUNK from PROC through the face-markup parser so the model's
tagged output renders with Emacs faces instead of raw Markdown.

Triggers `claude-repl--explain-config-show' on every chunk — `--show'
is idempotent w.r.t. the global flag, so the user-visible effect is
\"popup appears on the first chunk\"; the drawer-state capture (and
drawer-hide) happens exactly once on the hidden→visible transition."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (claude-repl--explain-config-parse-chunk buf chunk)
      (claude-repl--explain-config-show))))

(defun claude-repl--explain-config-spawn (prompt)
  "Spawn the headless claude process for explain-config PROMPT.
Returns the process.  Separated from the interactive entry point so
tests can stub `make-process' here without going through the input
read.

The popup is NOT displayed here — it is deferred to
`claude-repl--explain-config-filter', which calls `--show' on the
first streamed chunk.  This delays the visible side effect (and the
drawer-hide) until there is actual content to render, so a stalled
or never-responding `claude' invocation doesn't disturb the UI."
  (let* ((dir (file-name-as-directory
               (expand-file-name claude-repl-explain-config-dir)))
         (buf (claude-repl--explain-config-init-buffer prompt))
         (cmd (cons claude-repl-explain-config-program
                    claude-repl-explain-config-flags))
         (input (claude-repl--explain-config-build-input prompt)))
    (let* ((default-directory dir)
           (proc (make-process
                  :name "claude-explain-config"
                  :buffer buf
                  :command cmd
                  :connection-type 'pipe
                  :noquery t
                  :filter #'claude-repl--explain-config-filter
                  :sentinel #'claude-repl--explain-config-sentinel)))
      (process-send-string proc input)
      (process-send-eof proc)
      proc)))

(defun claude-repl-explain-config (prompt)
  "Ask a headless claude to explain something about this doom config.
Prompts for PROMPT, then spawns `claude -p
--dangerously-skip-permissions' in `claude-repl-explain-config-dir'
(`~/.config/doom' by default).  The prompt is wrapped in a
read-only preamble forbidding any mutating action -- this entry
point is for clarification and explanation only.  Output streams to
`claude-repl-explain-config-buffer-name'."
  (interactive
   (list (read-string (propertize "🤖 Explain config: "
                                  'face `(:foreground ,claude-repl--explain-config-orange
                                          :weight bold)))))
  (let ((trimmed (string-trim (or prompt ""))))
    (when (string-empty-p trimmed)
      (user-error "Empty prompt"))
    (claude-repl--log (+workspace-current-name)
                      "explain-config: dir=%s len=%d"
                      claude-repl-explain-config-dir (length trimmed))
    (claude-repl--explain-config-spawn trimmed)))

(defun claude-repl--send-interrupt-escape (ws vterm-buf)
  "Send two Escape key presses to VTERM-BUF to interrupt Claude.
WS is the current workspace name for logging."
  (claude-repl--log ws "send-interrupt-escape: sending %dx <escape> to vterm=%s" claude-repl-interrupt-escape-count (buffer-name vterm-buf))
  (with-current-buffer vterm-buf
    (dotimes (_ claude-repl-interrupt-escape-count)
      (vterm-send-key "<escape>"))))

(defun claude-repl--enter-insert-mode (vterm-buf)
  "Send \"i\" to VTERM-BUF to re-enter insert mode."
  (if (buffer-live-p vterm-buf)
      (progn
        (claude-repl--log (+workspace-current-name) "enter-insert-mode: sending \"i\" to vterm=%s" (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-string "i")))
    (claude-repl--log (+workspace-current-name) "enter-insert-mode: vterm is dead, skipping")))

(defun claude-repl-interrupt (&optional ws)
  "Interrupt Claude in workspace WS and re-enter insert mode after a delay.
Sends Escape to stop the current operation, then automatically sends
\"i\" after `claude-repl-interrupt-reinsert-delay' seconds to return
to insert mode.  Defaults to the current workspace when WS is nil
(matches the interactive `SPC o x' behavior); the drawer passes the
entry-at-point so interrupts target the selected entry.

After issuing the escape, marks the workspace's claude-state as
`:done' and clears the Stop / SubagentStop tracking — interrupting
terminates the in-flight turn, so the tab should immediately reflect
\"finished\" rather than linger on `:thinking' until a stray hook
arrives.  No Stop hook will fire for the interrupted turn, so Emacs
is the sole observer here."
  (interactive)
  (let* ((ws (or ws (+workspace-current-name)))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (claude-repl--log ws "interrupt")
    (if (and vterm-buf (buffer-live-p vterm-buf))
        (progn
          (claude-repl--send-interrupt-escape ws vterm-buf)
          (claude-repl--ws-clear-stop-tracking ws)
          (claude-repl--mark-claude-done ws)
          (run-at-time claude-repl-interrupt-reinsert-delay nil
                       #'claude-repl--enter-insert-mode vterm-buf))
      (claude-repl--log ws "interrupt: vterm not live, skipping"))))

(defun claude-repl-update-pr ()
  "Ask Claude to update the PR description for the current branch."
  (interactive)
  (claude-repl--log (+workspace-current-name) "update-pr: sending update-pr prompt")
  (claude-repl--send-to-claude claude-repl-update-pr-prompt))

(defun claude-repl--rebase-onto-origin-master-callback (ws ok output)
  "Process the `git fetch origin' result and ask Claude to rebase.
WS is the workspace name.  OK and OUTPUT come from the async-git
sentinel.  On success, dispatches `claude-repl-rebase-onto-origin-master-prompt'
to Claude so the agent runs the rebase itself.  On failure, surfaces
the git error via `message' and skips the agent dispatch — the rebase
would proceed against stale `origin/master' otherwise."
  (claude-repl--log ws "rebase-onto-origin-master: fetch ok=%s output=%s" ok output)
  (if ok
      (progn
        (message "[%s] git fetch origin complete; asking Claude to rebase onto origin/master" ws)
        (claude-repl--send-to-claude claude-repl-rebase-onto-origin-master-prompt))
    (message "[%s] git fetch origin failed: %s" ws output)))

(defun claude-repl-rebase-onto-origin-master ()
  "Fetch origin asynchronously, then ask Claude to rebase onto origin/master.
Runs `git fetch origin' in the current workspace's project directory.
When it succeeds, sends `claude-repl-rebase-onto-origin-master-prompt'
to Claude so the agent performs the rebase itself (and resolves any
conflicts).  On fetch failure, skips the dispatch and surfaces the git
error via `message'."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (project-dir (claude-repl--ws-dir ws)))
    (claude-repl--log ws "rebase-onto-origin-master: fetching origin in %s" project-dir)
    (message "[%s] git fetch origin..." ws)
    (claude-repl--async-git
     "rebase-fetch" project-dir '("fetch" "origin")
     (lambda (ok output)
       (claude-repl--rebase-onto-origin-master-callback ws ok output)))))

(defun claude-repl--exclusion-symbol-to-flag (sym)
  "Convert exclusion SYM (e.g. \\='no-self-certified) to flag (e.g. \"--self-certified\")."
  (let ((name (symbol-name sym)))
    (unless (string-prefix-p "no-" name)
      (error "claude-repl: exclusion symbol must start with `no-': %S" sym))
    (concat "--" (substring name 3))))

(defun claude-repl--build-create-or-update-pr-prompt (excluded)
  "Build the /create-or-update-pr prompt, omitting flags for EXCLUDED.
EXCLUDED is a list of `no-FLAG' symbols (e.g. \\='no-self-certified).  Each
must correspond to a flag in `claude-repl-create-or-update-pr-base-flags'
or an error is signalled."
  (let ((excluded-flags
         (mapcar (lambda (sym)
                   (let ((flag (claude-repl--exclusion-symbol-to-flag sym)))
                     (unless (member flag claude-repl-create-or-update-pr-base-flags)
                       (error "claude-repl: %S excludes %s, not in base flags" sym flag))
                     flag))
                 excluded)))
    (string-join
     (cons "/create-or-update-pr"
           (cl-remove-if (lambda (f) (member f excluded-flags))
                         claude-repl-create-or-update-pr-base-flags))
     " ")))

(defun claude-repl-create-or-update-pr (&optional excluded)
  "Send /create-or-update-pr to Claude with optional EXCLUDED flags dropped.
The current input buffer contents (right-trimmed, if non-empty) are
prepended as a prefix separated by a single space; the input buffer is
then cleared and its prior contents pushed to history.
EXCLUDED is a list of `no-FLAG' symbols (e.g. \\='(no-self-certified)) — each
named flag is removed from `claude-repl-create-or-update-pr-base-flags'
before the prompt is sent."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (base (claude-repl--build-create-or-update-pr-prompt excluded))
         (input-buf (claude-repl--ws-get ws :input-buffer))
         (raw-prefix (claude-repl--read-input-buffer ws))
         (prefix (and raw-prefix (string-trim-right raw-prefix)))
         (has-prefix (and prefix (not (string-empty-p prefix))))
         (prompt (if has-prefix (concat prefix " " base) base)))
    (claude-repl--log ws "create-or-update-pr: prefix-len=%d prompt=%s"
                      (length (or prefix "")) prompt)
    (claude-repl--send-to-claude prompt)
    (when (and has-prefix input-buf (buffer-live-p input-buf))
      (claude-repl--commit-input-buffer ws input-buf raw-prefix t))))

(defun claude-repl-create-or-update-pr-no-self-certified ()
  "Send /create-or-update-pr to Claude without --self-certified."
  (interactive)
  (claude-repl-create-or-update-pr '(no-self-certified)))

(defun claude-repl-create-or-update-pr-paste (&optional excluded)
  "Insert the /create-or-update-pr prompt at point instead of sending.
EXCLUDED has the same semantics as in `claude-repl-create-or-update-pr'.
The inserted prompt is wrapped in single backticks for inline-code
rendering in markdown contexts.  No workspace state is touched — the
input buffer is left intact and Claude is not contacted."
  (interactive)
  (let ((prompt (claude-repl--build-create-or-update-pr-prompt excluded)))
    (claude-repl--log (+workspace-current-name)
                      "create-or-update-pr-paste: prompt=%s" prompt)
    (insert "`" prompt "`")))

(defun claude-repl-create-or-update-pr-no-self-certified-paste ()
  "Insert the /create-or-update-pr prompt (no --self-certified) at point."
  (interactive)
  (claude-repl-create-or-update-pr-paste '(no-self-certified)))

(defun claude-repl--nuke-one-workspace (ws &optional preserve-entry)
  "Tear down a single claude-repl workspace WS without prompting.
Kills any in-flight git-diff process, tears down the vterm session
and buffers, removes WS from `claude-repl--workspaces', kills every
remaining buffer (and attached process) that belongs to the persp via
`claude-repl--kill-workspace-buffers', and finally kills the persp
workspace via `+workspace/kill'.  Designed to be reusable from
`claude-repl-nuke-workspace' (one-shot),
`claude-repl-nuke-all-workspaces' (loop), and
`claude-repl-kill-workspace'.

When PRESERVE-ENTRY is non-nil, the `claude-repl--workspaces' hashmap
entry is retained — every other teardown step runs as usual (vterm,
buffers, persp), but the ws plist survives so the drawer's MERGED
section can keep rendering the entry until the user explicitly
`finish'es it.  This is the merge-completed teardown path; standard
nuke/kill callers pass nil and the entry is dropped.

Persisted state (`<project>/.claude/emacs/state.el', including the
captured per-environment session-id) is ALWAYS preserved — nuke is
purely an in-memory teardown.  An explicit `--state-save' runs at the
top of the function so the file reflects the latest in-memory state
even if downstream teardown errors before the redundant state-save in
`--teardown-session-state' can fire.

The hashmap removal (`ws-del') runs inside an `unwind-protect' cleanup
so it always happens, even when kill-session errors partway through.
The persp kill is the very last step so all internal state is already
cleaned up before the UI workspace disappears.  Callers can rely on
the post-condition: after the call returns \(or throws), WS is not in
`claude-repl--workspaces' (unless PRESERVE-ENTRY was non-nil) and its
on-disk state.el is up-to-date."
  (claude-repl--log ws "nuke-one-workspace: ENTRY ws=%s preserve-entry=%s cache=%S"
                    ws (if preserve-entry "t" "nil")
                    (if (boundp 'persp-names-cache) persp-names-cache "(unbound)"))
  ;; Stamp the kill timestamp before the pre-teardown state-save so the
  ;; on-disk state.el reflects this kill.  The project picker
  ;; (`claude-repl-switch-to-project') reads `:last-killed-at' to sort
  ;; entries (most-recently-killed first) and to color the kill-date
  ;; column.  Recorded on the ws plist so the immediately-following
  ;; `--state-save' picks it up via `claude-repl--ws-get'.
  (claude-repl--ws-put ws :last-killed-at (current-time))
  ;; Save first, before any teardown touches the ws plist or risks
  ;; erroring.  The teardown path also calls state-save, but wrapping
  ;; ours up front guarantees preservation even if a downstream step
  ;; signals before that secondary save can run.  Wrapped in
  ;; condition-case so a save error doesn't abort the nuke itself.
  (condition-case err
      (claude-repl--state-save ws)
    (error (claude-repl--log ws "nuke-one-workspace: pre-teardown state-save error: %S" err)))
  (unwind-protect
      (progn
        (when-let ((proc (claude-repl--ws-get ws :git-proc)))
          (when (process-live-p proc)
            (claude-repl--log ws "nuke-one-workspace: killing git-proc")
            (condition-case err
                (delete-process proc)
              (error (claude-repl--log ws "nuke-one-workspace: git-proc kill error: %S" err)))))
        (condition-case err
            (claude-repl--kill-session ws)
          (error (claude-repl--log ws "nuke-one-workspace: kill-session error: %S" err))))
    ;; Cleanup: always remove the hashmap entry regardless of any error
    ;; in the steps above (unless PRESERVE-ENTRY was requested).
    ;; Persisted state.el is intentionally NOT touched here — see the
    ;; docstring.
    (unless preserve-entry
      (condition-case err
          (claude-repl--ws-del ws)
        (error (claude-repl--log ws "nuke-one-workspace: ws-del error: %S" err))))
    ;; WHY: keep `claude-repl--restored-workspaces' consistent with the
    ;; live hash — a ws that's been nuked is no longer a restore-batch
    ;; member, so a follow-up `nuke-restored-workspaces' won't try to
    ;; re-tear-down a stale name.
    (setq claude-repl--restored-workspaces
          (delete ws claude-repl--restored-workspaces))
    ;; Kill every remaining buffer (and attached process) that belongs to
    ;; the persp before tearing down the persp itself.  `kill-session'
    ;; only handles the vterm/input panels it tracks in the hashmap;
    ;; this sweep catches file buffers, magit buffers, auxiliary shells,
    ;; or anything else the user opened while inside the workspace so
    ;; nothing is orphaned after the persp goes away.
    (condition-case err
        (claude-repl--kill-workspace-buffers ws)
      (error (claude-repl--log ws "nuke-one-workspace: kill-workspace-buffers error: %S" err)))
    ;; Kill the persp workspace last so all internal state is already
    ;; cleaned up before the UI workspace disappears.
    ;;
    ;; Existence guard uses `+workspace-exists-p' (which checks
    ;; `persp-names-cache' via `+workspace-list-names'), matching the
    ;; same check `+workspace/kill' itself performs.  Earlier versions
    ;; gated on `(persp-get-by-name ws)' — but persp-mode's
    ;; `persp-get-by-name' returns the keyword `persp-not-persp' (i.e.
    ;; `:nil', a truthy value) when the persp is missing, so that
    ;; guard never short-circuited.  In the merge-async flow that
    ;; double-closes the workspace (once preemptively in
    ;; `--workspace-merge-async', then again in the deferred
    ;; success callback of `--workspace-merge-do'), pass 2 would slip
    ;; through the broken guard and call `+workspace/kill', which then
    ;; emitted the user-visible warning `'<ws>' workspace doesn't
    ;; exist' in the echo area after every successful merge.
    (condition-case err
        (when (and (bound-and-true-p persp-mode)
                   (fboundp '+workspace-exists-p)
                   (+workspace-exists-p ws))
          (claude-repl--log ws "nuke-one-workspace: pre-persp-kill ws=%s cache=%S"
                            ws persp-names-cache)
          (+workspace/kill ws)
          (claude-repl--log ws "nuke-one-workspace: post-persp-kill ws=%s in-cache=%s cache=%S"
                            ws (if (member ws persp-names-cache) "t" "nil") persp-names-cache))
      (error (claude-repl--log ws "nuke-one-workspace: workspace-kill error: %S" err)))))

(defun claude-repl--nuke-or-kill-workspace (ws)
  "Dispatch a nuke vs. plain persp-kill on WS based on liveness.

When WS is a live claude-repl workspace
\(`claude-repl--ws-live-p'), runs the full
`claude-repl--nuke-one-workspace' teardown and returns the symbol
`nuke'.  Otherwise WS is a tab-bar-only workspace (either a
tombstoned claude-repl entry whose persp still exists or a persp
that was never registered with claude-repl); in that case runs a
bare `+workspace/kill' guarded by `+workspace-exists-p' and returns
the symbol `kill'.

Shared by the interactive `claude-repl-nuke-workspace' and
`claude-repl-kill-workspace' commands so the picker (which
deliberately offers both kinds of candidates via
`claude-repl--nukeable-workspace-names') can hand the chosen WS to a
single routing point."
  (cond
   ((claude-repl--ws-live-p ws)
    (claude-repl--nuke-one-workspace ws)
    'nuke)
   (t
    (claude-repl--log ws "nuke-or-kill: ws not live, routing to +workspace/kill")
    (when (and (bound-and-true-p persp-mode)
               (fboundp '+workspace-exists-p)
               (+workspace-exists-p ws))
      (condition-case err
          (+workspace/kill ws)
        (error (claude-repl--log ws "nuke-or-kill: +workspace/kill error: %S" err))))
    'kill)))

(defun claude-repl-nuke-workspace (&optional ws)
  "Tear down a claude-repl workspace: session, buffers, persp, and hashmap entry.
Persisted state.el (priority, per-environment session-id) is preserved
so the workspace can be re-opened later and resume its Claude session.
When called interactively without WS, prompts to select from the union
of live claude-repl workspaces and tab-bar workspaces
\(`claude-repl--nukeable-workspace-names'), defaulting to the current
workspace when it appears in that candidate list.  Programmatic
callers (e.g. the drawer) pass WS directly to skip the prompt.

If the selected workspace is NOT a live claude-repl workspace (its
claude has already been killed but the persp/doom workspace is still
in the tab-bar), the operation falls back to a plain `+workspace/kill'
— there is no claude-repl session to tear down, so a normal persp kill
is the correct operation.

No confirmation prompt: teardown is immediate.  Persisted state.el is
preserved, so re-opening the workspace later resumes the Claude
session — accidental invocations are easily recoverable."
  (interactive)
  (let* ((ws (or ws (claude-repl--read-nukeable-workspace "Nuke workspace: ")))
         (action (claude-repl--nuke-or-kill-workspace ws)))
    (force-mode-line-update t)
    (message (if (eq action 'nuke)
                 "Nuked workspace: %s"
               "Killed persp workspace: %s")
             ws)))

(defun claude-repl-nuke-all-workspaces ()
  "Tear down ALL claude-repl workspaces.
Iterates every workspace registered in `claude-repl--workspaces' and
applies the same teardown as `claude-repl-nuke-workspace' to each.
Persisted state.el for each project is preserved.
Prompts once with the count before proceeding."
  (interactive)
  (let* ((known (claude-repl--live-ws-names))
         (count (length known)))
    (unless known (user-error "No claude-repl workspaces registered"))
    (unless (y-or-n-p (format "Nuke ALL %d claude-repl workspace(s)? This kills processes and buffers but preserves on-disk state. "
                              count))
      (user-error "Aborted"))
    (claude-repl--log (+workspace-current-name) "nuke-all-workspaces: count=%d" count)
    ;; Snapshot keys before iterating; each call mutates the hash.
    (dolist (ws known)
      (claude-repl--nuke-one-workspace ws))
    (force-mode-line-update t)
    (message "Nuked %d workspace(s)" count)))

(defun claude-repl-nuke-restored-workspaces ()
  "Tear down every workspace that was restored this session.
Tears down only the workspaces tracked in
`claude-repl--restored-workspaces' (those established by
`claude-repl-load-workspace-snapshot', including the
from-archive entry point); workspaces the user created manually
before or after the restore are left alone.  Persisted state.el for
each project is preserved.  Prompts once with the count before
proceeding.  Same per-workspace teardown as
`claude-repl-nuke-workspace'."
  (interactive)
  (let* ((restored (cl-remove-if-not
                    (lambda (ws) (claude-repl--ws-get ws :project-dir))
                    claude-repl--restored-workspaces))
         (count (length restored)))
    (unless restored
      (user-error "No restored claude-repl workspaces to nuke"))
    (unless (y-or-n-p (format "Nuke %d restored claude-repl workspace(s)? This kills processes and buffers but preserves on-disk state. "
                              count))
      (user-error "Aborted"))
    (claude-repl--log (+workspace-current-name)
                      "nuke-restored-workspaces: count=%d" count)
    (dolist (ws restored)
      (claude-repl--nuke-one-workspace ws))
    (force-mode-line-update t)
    (message "Nuked %d restored workspace(s)" count)))

(defun claude-repl-kill-workspace (&optional ws)
  "Tear down a claude-repl workspace and preserve its persisted state.
Alias for `claude-repl-nuke-workspace' — both functions go through
`claude-repl--nuke-or-kill-workspace', which preserves the on-disk
per-project state file on the live-claude-repl path and falls back to
a plain `+workspace/kill' for tab-bar workspaces whose claude has
already been killed.  Retained as a separate command for callers /
muscle-memory that bind `kill' semantics distinctly from `nuke'.

Prompts to select from the union of live claude-repl workspaces and
tab-bar workspaces (`claude-repl--nukeable-workspace-names'),
defaulting to the current workspace when it appears in that candidate
list.  Programmatic callers (e.g. the drawer) pass WS directly to
skip the prompt.

No confirmation prompt: teardown is immediate.  Persisted state.el is
preserved, so re-opening the workspace later resumes the Claude
session — accidental invocations are easily recoverable."
  (interactive)
  (let* ((ws (or ws (claude-repl--read-nukeable-workspace "Kill workspace: ")))
         (action (claude-repl--nuke-or-kill-workspace ws)))
    (force-mode-line-update t)
    (message (if (eq action 'nuke)
                 "Killed workspace: %s"
               "Killed persp workspace: %s")
             ws)))

;;;; Hide-mode sweep

(defun claude-repl--sweep-hidden-workspaces (&optional except)
  "Persp-kill every claude-repl workspace whose `:repl-state' is `:hidden'.
EXCEPT names a workspace to skip (typically the just-arrived destination
of a workspace switch — we don't want to kill the workspace the user is
currently sitting in).  Each match is torn down via
`claude-repl--nuke-one-workspace', which always preserves the on-disk
state file so the workspace can be re-opened via project switch.

No-op when there are no matching workspaces.  Returns the list of names
that were actually killed (useful for tests)."
  (let* ((current (or except (+workspace-current-name)))
         (candidates (cl-remove-if
                      (lambda (ws)
                        (or (equal ws current)
                            (not (eq (claude-repl--ws-repl-state ws) :hidden))))
                      (claude-repl--live-ws-names))))
    (claude-repl--log current
                      "sweep-hidden-workspaces: except=%s candidates=%S"
                      current candidates)
    (dolist (ws candidates)
      (condition-case err
          (claude-repl--nuke-one-workspace ws)
        (error
         (claude-repl--log ws "sweep-hidden-workspaces: kill error ws=%s err=%S"
                           ws err))))
    candidates))

(defun claude-repl--maybe-sweep-hidden-on-switch (&optional ws)
  "Run `claude-repl--sweep-hidden-workspaces' when hide-mode is enabled.
WS is the just-arrived-on workspace; when nil, falls back to
`(+workspace-current-name)'.  Callers from `--on-workspace-switch'
pass the ws captured at hook-fire time so the reset and sweep operate
on the workspace that was just switched to — not on whatever is
current when this deferred call eventually runs (rapid back-to-back
switches would otherwise leave intermediate `:hidden' workspaces
unreset and exposed to the sweep).

Hooked into `claude-repl--on-workspace-switch' (panels.el).  Also resets
WS's `:repl-state' from `:hidden' back to `:inactive' if applicable, so
navigating to a hidden workspace removes its hidden flag (the user is
actively viewing it; it should not be killed)."
  (let ((current (or ws (+workspace-current-name))))
    (when (eq (claude-repl--ws-repl-state current) :hidden)
      (claude-repl--log current
                        "maybe-sweep: arriving on :hidden ws, resetting to :inactive")
      (claude-repl--ws-set-repl-state current :inactive))
    (when claude-repl-hide-mode-enabled
      (claude-repl--sweep-hidden-workspaces current))))

(defun claude-repl-copy-reference ()
  "Copy the current file and line reference to the clipboard.
With active region: copies file:startline-endline.
Without region: copies file:line."
  (interactive)
  (claude-repl--log (+workspace-current-name) "copy-reference: copying file reference")
  (let ((ref (claude-repl--format-file-ref)))
    (kill-new ref)
    (message "Copied: %s" ref)))

(defun claude-repl-paste-clipboard ()
  "Insert the current workspace's `:clipboard' text at point.
The slot is populated by `clipboard'-typed workspace_commands files
\(see `claude-repl--handle-clipboard-command') — a per-workspace
clipboard, deliberately distinct from the OS clipboard.

Signals `user-error' when no text has been set for the current
workspace."
  (interactive)
  (let* ((ws (+workspace-current-name))
         (text (claude-repl--ws-get ws :clipboard)))
    (unless text
      (user-error "No clipboard text set for workspace '%s'" ws))
    (claude-repl--log ws "paste-clipboard: ws=%s len=%d" ws (length text))
    (insert text)))

;;;; Workspace snapshot save/load

;; defconst (not defcustom) so reload always re-evaluates the path —
;; defcustom would leave a stale path bound from before this var
;; existed at this default.  Users wanting a custom path can `setq'
;; after load.
(defconst claude-repl-workspace-snapshot-file
  (expand-file-name "workspaces.el" "~/.claude/emacs/")
  "Path to the file where the workspace roster snapshot is persisted.
Lives at `~/.claude/emacs/workspaces.el' (symmetric with per-project
`<root>/.claude/emacs/state.el' and `history.el').  Auto-created on
first save.")

(defconst claude-repl--legacy-workspace-snapshot-file
  (expand-file-name ".workspace-snapshot.el"
                    (file-name-directory (or load-file-name
                                              buffer-file-name
                                              default-directory)))
  "Pre-relocation snapshot file at the claude-repl module directory.
Read-only fallback: when the configured file does not exist but this
legacy file does, the loader uses it.  The writer never targets this
path — first save naturally migrates to the configured location.")

(defun claude-repl--workspace-snapshot-file-for-read ()
  "Return the path to read the workspace snapshot from.
Prefers `claude-repl-workspace-snapshot-file'; falls back to the
legacy module-dir path when only the legacy file exists."
  (cond ((file-exists-p claude-repl-workspace-snapshot-file)
         claude-repl-workspace-snapshot-file)
        ((file-exists-p claude-repl--legacy-workspace-snapshot-file)
         claude-repl--legacy-workspace-snapshot-file)
        (t claude-repl-workspace-snapshot-file)))

(defcustom claude-repl-workspace-snapshot-archive-max 20
  "Maximum number of historical workspace-snapshot archives to retain.
Each Emacs run archives the prior snapshot (if any) on its first save,
so this caps the count of distinct prior sessions kept on disk.  Older
archives are pruned silently.  Set to 0 to disable archival entirely."
  :type 'integer
  :group 'claude-repl)

(defvar claude-repl--snapshot-archived-this-run nil
  "Non-nil after the workspace snapshot has been archived this Emacs run.
The archival path runs at most once per Emacs run: the first save that
encounters an existing on-disk file copies it to the archive dir; every
subsequent save in the same run is a regular overwrite.  Cleared
implicitly by Emacs restart (the variable resets to nil at load).")

(defun claude-repl--workspace-snapshot-archive-dir ()
  "Return the directory where prior workspace-snapshot files are archived.
Sibling of `claude-repl-workspace-snapshot-file', named
`<basename-sans-ext>-archive'."
  (expand-file-name
   (concat (file-name-base claude-repl-workspace-snapshot-file) "-archive")
   (file-name-directory claude-repl-workspace-snapshot-file)))

(defun claude-repl--prune-snapshot-archives ()
  "Trim the snapshot archive dir to `claude-repl-workspace-snapshot-archive-max'.
Sorts archive files lexicographically (timestamp suffix is
sortable) and unlinks any beyond the cap."
  (let ((dir (claude-repl--workspace-snapshot-archive-dir))
        (max claude-repl-workspace-snapshot-archive-max))
    (when (and (file-directory-p dir) (> max 0))
      (let* ((all (sort (directory-files dir t "\\.el\\'" t) #'string<))
             (excess (- (length all) max)))
        (when (> excess 0)
          (dolist (f (seq-take all excess))
            (claude-repl--log nil "prune-snapshot-archives: deleting %s" f)
            (ignore-errors (delete-file f))))))))

(defun claude-repl--archive-workspace-snapshot ()
  "Copy the current workspace-snapshot file (if any) into the archive dir.
No-op when:
  - already archived this Emacs run (the once-per-run guard);
  - the archive cap is 0 (archival disabled);
  - the snapshot file does not yet exist (nothing to preserve).

The archive filename uses the OLD file's mtime as a `%Y%m%dT%H%M%S'
suffix so each archive is timestamped to the moment the previous
session's snapshot was last written.  Errors are caught (archival is
best-effort and must never block the live save)."
  (unless (or claude-repl--snapshot-archived-this-run
              (<= claude-repl-workspace-snapshot-archive-max 0)
              (not (file-exists-p claude-repl-workspace-snapshot-file)))
    (claude-repl--with-error-logging "archive-workspace-snapshot"
      (let* ((src claude-repl-workspace-snapshot-file)
             (mtime (file-attribute-modification-time (file-attributes src)))
             (suffix (format-time-string "%Y%m%dT%H%M%S" mtime))
             (dir (claude-repl--workspace-snapshot-archive-dir))
             (dest (expand-file-name (format "%s.el" suffix) dir)))
        (unless (file-directory-p dir) (make-directory dir t))
        (claude-repl--log nil "archive-workspace-snapshot: %s -> %s" src dest)
        (copy-file src dest t)
        (setq claude-repl--snapshot-archived-this-run t)
        (claude-repl--prune-snapshot-archives)))))

(defvar claude-repl--snapshot-loaded-p nil
  "Non-nil after `claude-repl-load-workspace-snapshot' has completed once
this session.  The save path checks this to refuse clobbering a richer
on-disk roster with the freshly started live roster (which only holds
the workspaces the user has visited manually so far).  Set to t at the
end of a successful load; reset by Emacs restart.")

(defvar claude-repl--restored-workspaces nil
  "List of workspace names established by snapshot-restore in this session.
Populated incrementally as each entry of the snapshot loader (either the
current file or an archived file via `claude-repl-load-workspace-snapshot-from-archive')
successfully calls `claude-repl--establish-workspace'.  Used by
`claude-repl-nuke-restored-workspaces' to nuke only the restored
workspaces while sparing any workspaces the user created manually before
or after the restore.  Entries are removed when their workspace is
nuked individually via `claude-repl--nuke-one-workspace'.")

(defun claude-repl--snapshot-save-safe-p (live-count)
  "Return non-nil when save may proceed with LIVE-COUNT entries.
Safe iff loader already ran this session (`claude-repl--snapshot-loaded-p'
is t) OR the on-disk roster is no larger than LIVE-COUNT.  The latter
covers the fresh-install case where no prior file exists, is empty, or
is unreadable as a sexp — there's nothing to lose."
  (or claude-repl--snapshot-loaded-p
      (let* ((file claude-repl-workspace-snapshot-file)
             (parsed (claude-repl--read-workspace-snapshot file))
             (on-disk (plist-get parsed :workspaces)))
        (or (null on-disk)
            (<= (length on-disk) live-count)))))

(defun claude-repl--snapshot-entry-normalize (entry)
  "Normalize a snapshot ENTRY to (NAME . PLIST).
Accepts the legacy `(NAME . DIR-STRING)' shape, the deprecated
`(NAME :project-dir DIR :priority PRI)' plist shape (priority ignored —
authoritative source is `<dir>/.claude/emacs/state.el'), and the
current `(NAME :project-dir DIR)' plist shape."
  (let ((name (car entry))
        (payload (cdr entry)))
    (cons name
          (cond
           ((stringp payload) (list :project-dir payload))
           ((listp payload) payload)
           (t (error "claude-repl: malformed snapshot entry: %S" entry))))))

(defun claude-repl--collect-snapshot-entries ()
  "Return a list of (NAME :project-dir DIR [:nuked-at TIME]) entries.
Sourced from `claude-repl--workspaces'.  Includes every workspace
whose plist has a non-nil `:project-dir'.  `:priority' is deliberately
NOT included — it lives in each project's `<root>/.claude/emacs/state.el'
so the roster doesn't become a second source of truth.

Tombstoned entries (`:nuked-at' set) ARE included so the tombstone
survives across Emacs restart — otherwise a nuked workspace's identity
record would resurrect as live on next load.  Live entries omit
`:nuked-at' entirely so the on-disk format stays minimal for the common
case."
  (let ((entries (make-hash-table :test 'equal)))
    (maphash (lambda (ws plist)
               (when-let ((dir (plist-get plist :project-dir)))
                 (let ((tomb (plist-get plist :nuked-at)))
                   (puthash ws
                            (if tomb
                                (list :project-dir dir :nuked-at tomb)
                              (list :project-dir dir))
                            entries))))
             claude-repl--workspaces)
    (let (snapshot)
      (maphash (lambda (ws plist) (push (cons ws plist) snapshot))
               entries)
      snapshot)))

(defun claude-repl--snapshot-raw-format (raw)
  "Classify the RAW sexp read from a workspace-snapshot file.
Returns `:plist' when RAW is a plist (top-level keyword keys — the
current format that carries both `:workspaces' and `:merge-queue'),
`:legacy' when RAW is the older list-of-entries shape (each element a
cons/list whose car is a ws-name string), and `:empty' when RAW is nil."
  (cond
   ((null raw) :empty)
   ((and (consp raw) (keywordp (car raw))) :plist)
   (t :legacy)))

(defun claude-repl--snapshot-entries-from-raw (raw)
  "Return the workspace-entries list from RAW (a parsed snapshot sexp).
Handles both the current plist format and the legacy list-of-entries
format.  Returns nil when RAW carries no entries (or is itself nil)."
  (pcase (claude-repl--snapshot-raw-format raw)
    (:plist (plist-get raw :workspaces))
    (:legacy raw)
    (_ nil)))

(defun claude-repl--snapshot-merge-queue-from-raw (raw)
  "Return the persisted merge-queue from RAW (a parsed snapshot sexp).
Returns nil when RAW is in the legacy list-of-entries format (which
predates merge-queue persistence) or carries no `:merge-queue' key."
  (pcase (claude-repl--snapshot-raw-format raw)
    (:plist (plist-get raw :merge-queue))
    (_ nil)))

(defun claude-repl--read-workspace-snapshot (file)
  "Read FILE and return (:workspaces ENTRIES :merge-queue QUEUE).
Normalizes both legacy (`((ws :project-dir dir) ...)') and current
plist-shaped files into the plist return shape so callers don't need
to branch on disk layout.  Returns nil when FILE does not exist or
the sexp is unreadable."
  (when (and file (file-exists-p file))
    (condition-case err
        (let ((raw (claude-repl--read-sexp-file file)))
          (list :workspaces (claude-repl--snapshot-entries-from-raw raw)
                :merge-queue (claude-repl--snapshot-merge-queue-from-raw raw)))
      (error
       (claude-repl--log nil "read-workspace-snapshot: read err file=%s err=%S"
                         file err)
       nil))))

(defun claude-repl--serialize-merge-queue (queue)
  "Return QUEUE (the live `claude-repl--merge-queue') stripped down to
the keys that survive `read' round-trip.  Every entry plist is plain
strings/booleans/nil today, so serialization is a key-pick.  The
indirection keeps the on-disk format insulated from future plist-key
additions.

Carries the loop-guard metadata `:last-attempt-target-head' (HEAD SHA
recorded at re-enqueue time after a failed merge) and the
`:halt-until-human' flag (set on generic-failure re-enqueues to block
auto-drain) so a restart preserves the same drain semantics as the
live queue."
  (mapcar (lambda (entry)
            (list :source-ws (plist-get entry :source-ws)
                  :silent (and (plist-get entry :silent) t)
                  :auto-resolve (and (plist-get entry :auto-resolve) t)
                  :last-attempt-target-head
                  (plist-get entry :last-attempt-target-head)
                  :halt-until-human
                  (and (plist-get entry :halt-until-human) t)))
          queue))

(defun claude-repl--write-workspace-snapshot (snapshot &optional merge-queue)
  "Write SNAPSHOT (a list of workspace entries) and MERGE-QUEUE to
`claude-repl-workspace-snapshot-file' in the plist format
`(:workspaces SNAPSHOT :merge-queue MERGE-QUEUE)'.

When MERGE-QUEUE is omitted, defaults to `claude-repl--merge-queue' so
every snapshot write captures the live FIFO alongside the roster.

Creates the parent directory if missing and archives the previous file
before overwriting.  Caller is responsible for any pre-write checks
\(e.g. `--snapshot-save-safe-p' or interactive confirmation)."
  (claude-repl--log nil "write-sexp-file: file=%s" claude-repl-workspace-snapshot-file)
  (let ((dir (file-name-directory claude-repl-workspace-snapshot-file)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (claude-repl--archive-workspace-snapshot)
  (let* ((queue (claude-repl--serialize-merge-queue
                 (or merge-queue
                     (and (boundp 'claude-repl--merge-queue)
                          claude-repl--merge-queue)))))
    (with-temp-file claude-repl-workspace-snapshot-file
      (insert "(:workspaces (")
      (let ((first t))
        (dolist (entry snapshot)
          (unless first (insert "\n               "))
          (setq first nil)
          (prin1 entry (current-buffer))))
      (insert ")\n :merge-queue (")
      (let ((first t))
        (dolist (entry queue)
          (unless first (insert "\n                "))
          (setq first nil)
          (prin1 entry (current-buffer))))
      (insert "))"))))

(defun claude-repl-save-workspace-snapshot ()
  "Save the current set of claude-repl workspaces to a hidden file.
Writes a list of (NAME :project-dir DIR) entries sourced from
`claude-repl--workspaces' (the live hash).

Refuses to overwrite when the loader hasn't run this session AND the
on-disk roster is larger than the live hash — the auto-piggyback save
path uses this to avoid clobbering a richer snapshot with a half-
populated live set during startup.  Use
`claude-repl-update-workspace-snapshot' to force an overwrite.

Called interactively prints a confirmation; called from
`claude-repl--state-save' (the common path) stays silent so the
roster-piggyback save doesn't spam the echo area on every state mutation."
  (interactive)
  (let ((snapshot (claude-repl--collect-snapshot-entries)))
    (if (not (claude-repl--snapshot-save-safe-p (length snapshot)))
        (claude-repl--log nil
                          "save-workspace-snapshot: ABORTED — loader hasn't run this session and on-disk roster is larger than live (%d)"
                          (length snapshot))
      (claude-repl--write-workspace-snapshot snapshot)
      (when (called-interactively-p 'interactive)
        (message "Saved %d workspace(s) to %s"
                 (length snapshot) claude-repl-workspace-snapshot-file)))))

(defun claude-repl-update-workspace-snapshot ()
  "Force-write the current live workspace roster to the snapshot file.
Captures every workspace in `claude-repl--workspaces' with a
`:project-dir' (the same set offered by `claude-repl-nuke-workspace')
and overwrites `claude-repl-workspace-snapshot-file' unconditionally.

Unlike `claude-repl-save-workspace-snapshot', this command bypasses the
loader-hasn't-run safety guard.  If the write would reduce the entry
count compared to what is currently on disk, prompts for confirmation
first so a slip can't silently shrink the roster.

Use this after manually creating / killing workspaces when you want
the on-disk snapshot to reflect the current live state immediately,
without waiting for the next `--state-save' piggyback."
  (interactive)
  (let* ((snapshot (claude-repl--collect-snapshot-entries))
         (live-count (length snapshot))
         (file claude-repl-workspace-snapshot-file)
         (parsed (claude-repl--read-workspace-snapshot file))
         (on-disk (plist-get parsed :workspaces))
         (on-disk-count (length on-disk)))
    (when (and (> on-disk-count live-count)
               (not (y-or-n-p
                     (format "On-disk snapshot has %d entries, live has %d.  Overwrite anyway? "
                             on-disk-count live-count))))
      (user-error "Aborted"))
    (claude-repl--write-workspace-snapshot snapshot)
    (message "Updated snapshot: %d workspace(s) -> %s"
             live-count file)))

(defun claude-repl--clean-frame-foreign-windows (ws)
  "Delete frame windows whose buffer is owned by a different workspace.
A window is foreign iff its buffer's `claude-repl--owning-workspace'
buffer-local is non-nil AND not `equal' to WS.  Buffers with no owning
workspace (regular files, dashboard, scratch, fallback) are treated as
allowed, since they are workspace-agnostic.

Strips `no-delete-other-windows' and dedication from foreign windows
first so prior workspaces' claude-repl panel windows (which are marked
dedicated + `no-delete-other-windows') can be torn down.  When every
frame window is foreign, deletes all but one and swaps that one's
buffer to `doom-fallback-buffer' so the new workspace starts with a
clean single-window layout instead of inheriting the previous
workspace's window-configuration.

Prior implementation used `persp-buffers' as the allowed set, which
was fragile: for a freshly-created persp the allowed set is empty so
all windows got scrubbed (correct by accident), but persp-mode can
auto-add the currently-selected buffer to the activated persp during
`persp-frame-switch' — that buffer (potentially a prior workspace's
Claude panel) then lands in the new persp's allowed set and the
predicate fails to scrub it.  Owning-workspace is set on the buffer
at creation time by `claude-repl--create-buffer' and is immune to
persp routing."
  (let* ((fallback (and (fboundp 'doom-fallback-buffer) (doom-fallback-buffer)))
         (all (window-list nil 'nomini))
         (foreign-p (lambda (w)
                      (let ((owner (buffer-local-value
                                    'claude-repl--owning-workspace
                                    (window-buffer w))))
                        (and owner (not (equal owner ws))))))
         (foreign (cl-remove-if-not foreign-p all))
         (any-native (< (length foreign) (length all))))
    (when foreign
      (claude-repl--log ws "clean-frame-foreign-windows: ws=%s removing=%d total=%d"
                        ws (length foreign) (length all))
      (dolist (win foreign)
        (set-window-parameter win 'no-delete-other-windows nil)
        (set-window-dedicated-p win nil))
      (cond
       (any-native
        (dolist (win foreign)
          (ignore-errors (delete-window win))))
       (t
        (dolist (win (cdr foreign))
          (ignore-errors (delete-window win)))
        (when (and fallback (car foreign) (window-live-p (car foreign)))
          (set-window-buffer (car foreign) fallback)))))))

(defun claude-repl--establish-workspace (ws dir)
  "Synchronously create + activate + fully set up workspace WS for DIR.
Mirrors what `claude-repl-switch-to-project' would do on an
interactive `SPC p p' but bypasses `+workspaces-switch-to-project-h'
to avoid the rename-on-empty collapse: persp is added directly via
`persp-add-new' and activated via `persp-frame-switch' keyed by the
snapshot's `ws' name (not the project basename Doom would otherwise
derive).

Each call:

- creates the persp (`persp-add-new'),
- activates it on this frame (`persp-frame-switch') so persp-mode
  saves a clean window-configuration for the previous persp and
  starts capturing one for this one,
- scrubs foreign windows lingering from the prior persp via
  `claude-repl--clean-frame-foreign-windows' (dedicated panels from
  the previous workspace would otherwise survive into WS's frame),
- registers the project with projectile,
- temporarily sets `default-directory' on the fallback buffer + loads
  dir-locals, runs `+workspaces-switch-project-function' (the user
  lambda that auto-opens magit when the persp has no real buffers),
  then restores the fallback buffer's original `default-directory'
  (the buffer is shared across persps, so permanent mutation would
  make scratch report the last-loaded ws's dir from every persp),
- opens the most-recent project file via `find-file' when one exists
  in `recentf-list',
- hydrates `:project-dir' into `claude-repl--workspaces' and
  rehydrates `:priority' from the per-project state file via
  `claude-repl--hydrate-priority-from-state',
- reorders the ws in `persp-names-cache' by its hydrated `:priority'
  via `claude-repl--reorder-workspace-by-priority' so restored
  workspaces appear in priority order, matching what
  `claude-repl-set-priority' does for user-driven changes,
- starts claude (`claude-repl--initialize-claude') unless already
  running."
  (claude-repl--with-error-logging (format "establish-workspace[%s]" ws)
    (when (fboundp 'persp-add-new)
      (persp-add-new ws))
    ;; WHY: this switch is load-bearing for `--snapshot-load-finish' in
    ;; addition to its primary purpose.  persp-mode auto-saves the
    ;; previous persp's window-configuration on switch-away, and finish
    ;; relies on that saved wconf when it does `persp-frame-switch
    ;; origin' at end-of-load — without this call here on the first
    ;; queue entry, origin would have no saved wconf and finish would
    ;; land in whatever windows the last-loaded ws ended with.  If this
    ;; is ever reordered or replaced with a setup-without-switch, the
    ;; snapshot loader's return-to-origin layout silently regresses.
    (when (fboundp 'persp-frame-switch)
      (persp-frame-switch ws))
    ;; Strip any window-configuration bleed-over from the prior persp before
    ;; populating WS — panels left over from the previous workspace are marked
    ;; `no-delete-other-windows', so the later `delete-other-windows' in
    ;; `--show-existing-panels' won't tear them down on its own.
    (claude-repl--clean-frame-foreign-windows ws)
    (when (fboundp 'projectile-add-known-project)
      (projectile-add-known-project dir))
    ;; The fallback buffer is SHARED across persps — permanently rewriting
    ;; its `default-directory' makes scratch report the last-loaded ws's
    ;; project root from every persp.  Save/restore the original so the
    ;; side effect lives only for the duration of the project-switch hook.
    (let* ((fb (and (fboundp 'doom-fallback-buffer) (doom-fallback-buffer)))
           (orig-dir (and fb (buffer-live-p fb)
                          (buffer-local-value 'default-directory fb))))
      (when fb
        (with-current-buffer fb
          (setq default-directory (file-name-as-directory dir))
          (when (fboundp 'hack-dir-local-variables-non-file-buffer)
            (hack-dir-local-variables-non-file-buffer))))
      (unwind-protect
          (when (and (boundp '+workspaces-switch-project-function)
                     +workspaces-switch-project-function)
            (funcall +workspaces-switch-project-function dir))
        (when (and fb (buffer-live-p fb) orig-dir)
          (with-current-buffer fb
            (setq default-directory orig-dir)))))
    (when-let ((recent-file (claude-repl--most-recent-project-file dir)))
      (when (file-exists-p recent-file)
        (find-file recent-file)))
    ;; Wake any pre-existing tombstone before re-asserting identity keys —
    ;; an `--establish-workspace' call is the canonical resurrection path
    ;; for snapshot-loaded entries that may have been tombstoned in a prior
    ;; session.  Clearing `:nuked-at' first keeps `--ws-live-p' coherent
    ;; with the post-establish state.
    (claude-repl--ws-put ws :nuked-at nil)
    (claude-repl--ws-put ws :project-dir dir)
    (when (fboundp 'claude-repl--hydrate-priority-from-state)
      (claude-repl--hydrate-priority-from-state dir))
    ;; WHY: snapshot entries are establish'd in file order, so
    ;; `persp-add-new' above appends each new ws at the cache tail in
    ;; that order — priority badges hydrate after the persp is already
    ;; placed.  Mirror what `claude-repl-set-priority' does after a
    ;; user-driven priority change: re-splice this ws into its rank-
    ;; correct slot.  Without this, restored workspaces sit in
    ;; snapshot-file order instead of priority order.  Guarded on
    ;; fboundp so a partial-load test environment doesn't crash here.
    (when (fboundp 'claude-repl--reorder-workspace-by-priority)
      (claude-repl--reorder-workspace-by-priority ws))
    (when (and (fboundp 'claude-repl--initialize-claude)
               (fboundp 'claude-repl--claude-running-p)
               (not (claude-repl--claude-running-p ws)))
      (claude-repl--initialize-claude ws))))

(defvar claude-repl--snapshot-load-state nil
  "Plist describing an in-progress recursive snapshot load, or nil.
Keys: `:queue' (list of (NORMALIZED-WS . PLIST) entries still to do),
`:origin' (workspace to switch back to at end), `:awaiting' (ws-name
the loader is currently waiting on a ready signal for, or nil),
`:loaded' (successfully established + ready/awaiting), `:skipped'
(dir missing/nil), `:load-error' (establish-workspace signaled),
`:total' (entry count from the file), `:timeout-timer' (the per-entry
watchdog timer).

Non-nil means a load is in flight — concurrent invocations of
`claude-repl-load-workspace-snapshot' are refused via a guard.")

(defcustom claude-repl-snapshot-load-per-entry-timeout 30
  "Per-entry watchdog in seconds for the recursive snapshot loader.
If the awaited workspace's `claude-repl--on-session-start-event' hasn't
fired by then, the loader advances to the next entry anyway and logs
a warning.  Tuned long enough for sandbox image build / first-time
claude startup but short enough that a wedged workspace doesn't lock
the entire load."
  :type 'number
  :group 'claude-repl)

(defun claude-repl--snapshot-load-ws-ready-p (ws)
  "Return non-nil when WS's vterm buffer reports ready."
  (when-let ((buf (claude-repl--ws-get ws :vterm-buffer)))
    (and (buffer-live-p buf)
         (buffer-local-value 'claude-repl--ready buf))))

(defun claude-repl--snapshot-load-cancel-timer ()
  "Cancel the pending per-entry watchdog timer, if any."
  (when-let ((timer (and claude-repl--snapshot-load-state
                         (plist-get claude-repl--snapshot-load-state :timeout-timer))))
    (when (timerp timer) (cancel-timer timer))
    (setq claude-repl--snapshot-load-state
          (plist-put claude-repl--snapshot-load-state :timeout-timer nil))))

(defun claude-repl--snapshot-restore-merge-queue (saved-mq)
  "Repopulate `claude-repl--merge-queue' from SAVED-MQ (read from disk).
Filters out entries whose `:source-ws' no longer exists in
`claude-repl--workspaces' (the workspace was removed between sessions,
or its snapshot entry was skipped because its `:project-dir' was gone).
Re-applies the `:repl-state :merge-queued' marker on each surviving
source-ws so the drawer's MERGING bucket re-surfaces them.

Does NOT auto-drain — `claude-repl--workspace-merge-do' is the normal
drain trigger and the user kicks it off via `claude-repl-drain-merge-queue'
\(intended for cases where the in-flight cherry-pick died with Emacs and
the user has manually resolved before re-entering the loop)."
  (when (and saved-mq (boundp 'claude-repl--merge-queue))
    (let ((restored nil)
          (dropped 0))
      (dolist (entry saved-mq)
        (let ((ws (plist-get entry :source-ws)))
          (cond
           ((and ws (gethash ws claude-repl--workspaces))
            (push (list :source-ws ws
                        :silent (and (plist-get entry :silent) t)
                        :auto-resolve (and (plist-get entry :auto-resolve) t))
                  restored)
            (claude-repl--ws-put ws :repl-state :merge-queued)
            (claude-repl--ws-put ws :claude-state nil))
           (t
            (cl-incf dropped)
            (claude-repl--log nil
                              "snapshot-restore-merge-queue: dropping entry ws=%s — ws absent post-load"
                              (or ws "nil"))))))
      (setq claude-repl--merge-queue (nreverse restored))
      (claude-repl--log nil
                        "snapshot-restore-merge-queue: restored=%d dropped=%d"
                        (length claude-repl--merge-queue) dropped))))

(defun claude-repl--snapshot-load-finish ()
  "Finalize the recursive load: detach hook, return to origin, message.
Idempotent: re-entry with `claude-repl--snapshot-load-state' already
nil is a no-op so the error-recovery path in `--snapshot-load-step'
can call finish without worrying whether a normal finish already ran."
  (when claude-repl--snapshot-load-state
    (remove-hook 'claude-repl-ws-fully-loaded-functions
                 #'claude-repl--snapshot-load-on-loaded)
    (claude-repl--snapshot-load-cancel-timer)
    (let* ((state claude-repl--snapshot-load-state)
           (origin (plist-get state :origin))
           (loaded (plist-get state :loaded))
           (skipped (plist-get state :skipped))
           (load-error (or (plist-get state :load-error) 0))
           (saved-mq (plist-get state :saved-merge-queue)))
      (claude-repl--snapshot-restore-merge-queue saved-mq)
      ;; persp-mode saved origin's window-config when the loader's first
      ;; `--establish-workspace' switched away from it, so this switch-back
      ;; replays that layout — and persp-mode's restore filters foreign
      ;; buffers, so panels owned by some other ws can't bleed in.
      (when (and origin
                 (fboundp '+workspace-exists-p)
                 (+workspace-exists-p origin)
                 (fboundp 'persp-frame-switch))
        (persp-frame-switch origin))
      (force-mode-line-update t)
      (setq claude-repl--snapshot-loaded-p t)
      (let ((mq-restored (and (boundp 'claude-repl--merge-queue)
                              (length claude-repl--merge-queue))))
        (claude-repl--log nil
                          "snapshot-load: END loaded=%d skipped=%d load-error=%d merge-queue=%d returned-to=%s"
                          loaded skipped load-error (or mq-restored 0) (or origin "nil"))
        (message "Loaded %d workspace(s), skipped %d, errored %d%s"
                 loaded skipped load-error
                 (if (and mq-restored (> mq-restored 0))
                     (format ", merge-queue=%d" mq-restored)
                   ""))))
    (setq claude-repl--snapshot-load-state nil)
    (claude-repl--snapshot-load-close-main)))

(defun claude-repl--snapshot-load-close-main ()
  "Kill the `main' workspace left over from Doom's startup, if it still exists.
Doom always creates `+workspaces-main' (typically \"main\") at startup;
once the snapshot load has populated the real workspace set, this
artifact is no longer useful and we kill it to keep the tabline clean.
Absent main, the function is a no-op.  Errors from `+workspace/kill'
are logged but never propagated — finish must remain robust."
  (let ((main (and (boundp '+workspaces-main) +workspaces-main)))
    (when (and main
               (fboundp '+workspace-exists-p)
               (+workspace-exists-p main)
               (fboundp '+workspace/kill))
      (claude-repl--log nil "snapshot-load: closing 'main' workspace artifact main=%s" main)
      (condition-case err
          (+workspace/kill main)
        (error (claude-repl--log nil "snapshot-load: close-main error: %S" err))))))

(defun claude-repl--snapshot-load-on-loaded (ws &optional _marker)
  "Ws-fully-loaded hook handler: advance the snapshot load queue iff WS is awaited.
Called from `claude-repl-ws-fully-loaded-functions' with the ws name and
an optional MARKER (e.g. `:timed-out' when the watchdog synthesized the
event).  Loader doesn't distinguish the marker — once a ws is loaded
or timed out, it advances.  Idempotent: the `:awaiting' equality guard
makes second fires for the same ws no-ops."
  (let ((state claude-repl--snapshot-load-state))
    (when (and state (equal ws (plist-get state :awaiting)))
      (claude-repl--log ws "snapshot-load: awaited ws=%s fully loaded — advancing" ws)
      (claude-repl--snapshot-load-cancel-timer)
      (setq claude-repl--snapshot-load-state
            (plist-put claude-repl--snapshot-load-state :awaiting nil))
      (claude-repl--snapshot-load-step))))

(defun claude-repl--snapshot-load-timeout (ws)
  "Watchdog firing for WS — force ws-fully-loaded with `:timed-out' marker.
The latch helper fires the ws-fully-loaded hook when both bits are
set, which in turn calls `--snapshot-load-on-loaded' to advance the
queue.  Flipping the missing bit(s) here funnels timeout through the
same advance path as the happy case, so observers see exactly one
ws-fully-loaded fire per entry (happy or timed-out, never both).

The bits we flip:
- `:ws-loaded' is flipped via the helper; the helper itself sets it
  before checking the both-bits condition, so this drives the
  emacs-side bit to t if it wasn't already.
- `:claude-ready' is also flipped to t directly so the helper's
  both-bits check passes even when claude never printed
  `session_start' (the most common timeout cause)."
  (let ((state claude-repl--snapshot-load-state))
    (when (and state (equal ws (plist-get state :awaiting)))
      (claude-repl--log ws "snapshot-load: TIMEOUT awaiting ws=%s — forcing fully-loaded :timed-out" ws)
      (message "[claude-repl] snapshot-load timeout awaiting ws=%s — advancing" ws)
      (setq claude-repl--snapshot-load-state
            (plist-put claude-repl--snapshot-load-state :timeout-timer nil))
      ;; Force both latch bits then fire via the helper.  Setting
      ;; :claude-ready directly before the helper call means the
      ;; helper's both-bits check will pass on its own :ws-loaded
      ;; flip, firing ws-fully-loaded with the :timed-out marker.
      (claude-repl--ws-put ws :claude-ready t)
      (claude-repl--latch-and-maybe-fire-loaded ws :ws-loaded :timed-out))))

(defun claude-repl--snapshot-load-step ()
  "Process the next entry in the snapshot-load queue.
Called both at start and from the ws-fully-loaded hook / timeout callback.

The body is wrapped in `condition-case' that routes any uncaught error
to `--snapshot-load-finish' — without this, a signal from
`--snapshot-load-ws-ready-p', `run-with-timer', a plist mutation, etc.,
would leave `claude-repl-ws-fully-loaded-functions' attached and
`claude-repl--snapshot-load-state' non-nil, turning a future
`session_start' event into a zombie-loader resume from a corrupt queue."
  (condition-case err
      (claude-repl--snapshot-load-step--unsafe)
    (error
     (claude-repl--log nil "snapshot-load: STEP ERROR err=%S — finishing early" err)
     (message "[claude-repl] snapshot-load step error: %S — aborting load" err)
     (claude-repl--snapshot-load-finish))))

(defun claude-repl--snapshot-load-step--unsafe ()
  "Unguarded implementation of `--snapshot-load-step'.
Public callers should use `--snapshot-load-step', which wraps this in
the error-routing `condition-case'."
  (let* ((state claude-repl--snapshot-load-state)
         (queue (plist-get state :queue))
         (total (plist-get state :total))
         (iter  (1+ (+ (plist-get state :loaded)
                       (plist-get state :skipped)
                       (or (plist-get state :load-error) 0)))))
    (cond
     ((null queue)
      (claude-repl--snapshot-load-finish))
     (t
      (let* ((entry (car queue))
             (ws (car entry))
             (plist (cdr entry))
             (dir (plist-get plist :project-dir)))
        ;; Pop this entry off the queue immediately so we don't double-process.
        (setq claude-repl--snapshot-load-state
              (plist-put state :queue (cdr queue)))
        (cond
         ((not (and dir (file-directory-p dir)))
          (claude-repl--log nil "snapshot-load iter=%d/%d SKIPPED ws=%s dir=%s reason=dir-missing-or-nil"
                            iter total ws (or dir "nil"))
          (setq claude-repl--snapshot-load-state
                (plist-put claude-repl--snapshot-load-state :skipped
                           (1+ (plist-get claude-repl--snapshot-load-state :skipped))))
          (claude-repl--snapshot-load-step))
         ;; Merged-completed entries: register data-only and advance.
         ;; The drawer's MERGED bucket renders these; `--finish-workspace'
         ;; (invoked via drawer `x') is the only way out.
         ((claude-repl--state-merge-completed-p dir)
          (claude-repl--log nil "snapshot-load iter=%d/%d ws=%s dir=%s register-merged"
                            iter total ws dir)
          (condition-case err
              (claude-repl--register-merged-workspace ws dir)
            (error
             (claude-repl--log nil "snapshot-load: register-merged err ws=%s err=%S" ws err)))
          (setq claude-repl--snapshot-load-state
                (plist-put claude-repl--snapshot-load-state :loaded
                           (1+ (plist-get claude-repl--snapshot-load-state :loaded))))
          (claude-repl--snapshot-load-step))
         (t
          (claude-repl--log nil "snapshot-load iter=%d/%d ws=%s dir=%s establishing"
                            iter total ws dir)
          ;; `:awaiting' must remain nil across the establish call so that any
          ;; re-entrant ready/timeout callback (today impossible because Emacs
          ;; is single-threaded and `--establish-workspace' is fully
          ;; synchronous, but a latent hazard if establish ever yields) sees
          ;; `(equal ws :awaiting)' fail and treats the firing as a no-op
          ;; instead of advancing the queue while establish is still on the
          ;; stack.  The `condition-case' both handles a thrown error and
          ;; ensures `:awaiting' is set only on the successful return path.
          (let ((establish-error nil))
            (condition-case err
                (claude-repl--establish-workspace ws dir)
              (error
               (setq establish-error err)
               (claude-repl--log nil "snapshot-load: establish-workspace err ws=%s err=%S" ws err)
               (message "[claude-repl] establish failed ws=%s — advancing" ws)))
            (cond
             (establish-error
              ;; Failure: bump :load-error, leave :awaiting nil, advance
              ;; immediately without arming the watchdog (no ws to wait on).
              (setq claude-repl--snapshot-load-state
                    (plist-put claude-repl--snapshot-load-state :load-error
                               (1+ (or (plist-get claude-repl--snapshot-load-state :load-error) 0))))
              (claude-repl--snapshot-load-step))
             (t
              ;; Success: bump :loaded, then wait for ready (or detect
              ;; already-ready and advance immediately).
              (setq claude-repl--snapshot-load-state
                    (plist-put claude-repl--snapshot-load-state :loaded
                               (1+ (plist-get claude-repl--snapshot-load-state :loaded))))
              (cond
               ((claude-repl--snapshot-load-ws-ready-p ws)
                ;; Already ready (e.g. the origin ws the user was sitting
                ;; in when load began, or any other ws claude was already
                ;; up in before the 2s idle loader fired).  Do NOT tag as
                ;; restored — this ws wasn't actually established by the
                ;; loader, it was already alive.  Tagging it would make
                ;; `claude-repl-nuke-restored-workspaces' incorrectly
                ;; sweep the user's pre-existing workspace.
                (claude-repl--log ws "snapshot-load: ws=%s already ready — advancing without waiting" ws)
                (claude-repl--snapshot-load-step))
               (t
                ;; WHY: tag this ws as restored-this-session so the user
                ;; can later nuke only the restore-batch via
                ;; `claude-repl-nuke-restored-workspaces' without
                ;; touching workspaces they created by hand or were
                ;; already in.  Accumulates across multiple loads (incl.
                ;; from-archive) so subsequent restores expand — never
                ;; shrink — the set.
                (cl-pushnew ws claude-repl--restored-workspaces :test #'equal)
                ;; Now — after establish has fully returned — mark `:awaiting'
                ;; and arm the watchdog.  The ws-fully-loaded hook (or the
                ;; watchdog) will call --snapshot-load-step again.
                (setq claude-repl--snapshot-load-state
                      (plist-put claude-repl--snapshot-load-state :awaiting ws))
                (setq claude-repl--snapshot-load-state
                      (plist-put claude-repl--snapshot-load-state :timeout-timer
                                 (run-with-timer
                                  claude-repl-snapshot-load-per-entry-timeout
                                  nil
                                  #'claude-repl--snapshot-load-timeout
                                  ws)))))))))))))))

(defun claude-repl-load-workspace-snapshot (&optional file)
  "Load workspaces from FILE (defaults to the configured snapshot path).
When FILE is nil, reads `claude-repl-workspace-snapshot-file' (or its
legacy module-dir fallback if the configured file is absent).  For each
entry, fully sets up the workspace via `claude-repl--establish-workspace'
\(persp creation + activation + projectile + dir-locals + magit lambda
+ find-file recent + claude init).

Recursive queue driver: establishes one entry, then yields to the main
loop until that workspace's `claude-repl-ws-fully-loaded-functions'
hook fires (i.e., both claude-side ready and emacs-side switch-settle
have completed), then advances.  Per-entry watchdog
\(`claude-repl-snapshot-load-per-entry-timeout') guarantees forward
progress even if the load barrier never fires; on timeout the loader
synthesizes a ws-fully-loaded fire with a `:timed-out' marker so all
hook observers see the same advance event.

Returns to the workspace that was active when the load began."
  (interactive)
  (when claude-repl--snapshot-load-state
    (user-error "claude-repl: a snapshot load is already in progress"))
  (let* ((file (or file (claude-repl--workspace-snapshot-file-for-read)))
         (parsed (claude-repl--read-workspace-snapshot file))
         (snapshot (plist-get parsed :workspaces))
         (saved-mq (plist-get parsed :merge-queue)))
    (unless snapshot
      (user-error "No workspace snapshot at %s" file))
    (let* ((normalized (mapcar #'claude-repl--snapshot-entry-normalize snapshot))
           ;; Partition: tombstoned entries (`:nuked-at' present) are
           ;; identity-only records — restore them directly to the hash
           ;; without queueing them for establish (which would create a
           ;; persp + start claude for a workspace the user already
           ;; nuked).  Live entries follow the original establish queue.
           (tombstones (cl-remove-if-not
                        (lambda (e) (plist-get (cdr e) :nuked-at))
                        normalized))
           (queue (cl-remove-if
                   (lambda (e) (plist-get (cdr e) :nuked-at))
                   normalized))
           (origin-ws (and (fboundp '+workspace-current-name)
                           (ignore-errors (+workspace-current-name)))))
      (dolist (entry tombstones)
        (let ((ws (car entry))
              (plist (cdr entry)))
          (claude-repl--ws-put ws :project-dir (plist-get plist :project-dir))
          (claude-repl--ws-put ws :nuked-at (plist-get plist :nuked-at))
          (claude-repl--log ws "snapshot-load: restored tombstone ws=%s dir=%s"
                            ws (plist-get plist :project-dir))))
      (setq claude-repl--snapshot-load-state
            (list :queue queue
                  :origin origin-ws
                  :awaiting nil
                  :loaded 0
                  :skipped 0
                  :load-error 0
                  :total (length queue)
                  :timeout-timer nil
                  :saved-merge-queue saved-mq))
      (add-hook 'claude-repl-ws-fully-loaded-functions
                #'claude-repl--snapshot-load-on-loaded)
      (claude-repl--log nil
                        "snapshot-load: BEGIN file=%s entries=%d merge-queue=%d origin-ws=%s"
                        file (length queue) (length saved-mq) (or origin-ws "nil"))
      (claude-repl--snapshot-load-step))))

(defun claude-repl--load-workspace-snapshot-on-startup ()
  "Restore the workspace snapshot silently at Emacs startup.
Does nothing if neither the configured snapshot file nor its legacy
fallback is present.  Errors are logged but never propagated, so a
corrupt snapshot can't block startup."
  (when (file-exists-p (claude-repl--workspace-snapshot-file-for-read))
    (condition-case err
        (claude-repl-load-workspace-snapshot)
      (error (message "[claude-repl] snapshot load failed: %S" err)))))

;;;; Workspace snapshot archive picker

(defun claude-repl--snapshot-file-ws-count (file)
  "Return the number of workspace entries in snapshot FILE, or 0 on error.
Reads via `claude-repl--read-workspace-snapshot' so both the current
plist format and the legacy list-of-entries shape report the workspace
roster length (not the wrapping plist's length)."
  (or (ignore-errors
        (length (plist-get (claude-repl--read-workspace-snapshot file)
                           :workspaces)))
      0))

(defun claude-repl--snapshot-file-mtime-string (file)
  "Return FILE's last-modified time as a short `YYYY-MM-DD HH:MM' string."
  (format-time-string "%Y-%m-%d %H:%M"
                      (nth 5 (file-attributes file))))

(defun claude-repl--snapshot-candidate-label (file)
  "Format the completing-read label for snapshot FILE.
Layout: `<basename>  <count>ws  <mtime>'.  Basename is padded so the
count/date columns align across candidates in ivy/vertico."
  (format "%-32s %3dws  %s"
          (file-name-nondirectory file)
          (claude-repl--snapshot-file-ws-count file)
          (claude-repl--snapshot-file-mtime-string file)))

(defun claude-repl--snapshot-archive-candidates ()
  "Return an alist of (LABEL . PATH) for the current snapshot + archives.
Current file first; archives newest-first by filename (the archive
filename is a timestamp, so lexicographic sort works)."
  (let* ((current (claude-repl--workspace-snapshot-file-for-read))
         (archive-dir (claude-repl--workspace-snapshot-archive-dir))
         (archives (and (file-directory-p archive-dir)
                        (sort (directory-files archive-dir t "\\.el\\'" t)
                              #'string>)))
         (paths (cl-remove-duplicates
                 (cl-remove-if-not #'file-exists-p
                                   (cons current archives))
                 :test #'equal)))
    (mapcar (lambda (p)
              (cons (claude-repl--snapshot-candidate-label p) p))
            paths)))

(defun claude-repl-load-workspace-snapshot-from-archive ()
  "Pick a snapshot file (current or archived) and load it.
Candidates are annotated with workspace count and last-modified time
so the user can identify the right archive by size + recency.  Loads
via `claude-repl-load-workspace-snapshot' with the chosen file passed
explicitly (skips the configured-vs-legacy resolver)."
  (interactive)
  (let* ((candidates (claude-repl--snapshot-archive-candidates))
         (_ (unless candidates
              (user-error "No snapshot files found (no current file, no archives)")))
         (choice (completing-read "Load workspace snapshot: "
                                  (mapcar #'car candidates) nil t))
         (file (cdr (assoc choice candidates))))
    (when file
      (claude-repl--log nil "load-workspace-snapshot-from-archive: file=%s" file)
      (claude-repl-load-workspace-snapshot file))))

;;;; Merge-queue manual drain

(defun claude-repl-drain-merge-queue ()
  "Re-kick the merge-queue drain loop after a manually-resolved stall.

Normal flow: `claude-repl--workspace-merge-do' completes (success or
failure) and immediately calls `claude-repl--drain-merge-queue', so the
queue drains naturally as cherry-picks finish.  This command is the
escape hatch for stalls where that automatic drain didn't happen — for
example, when Emacs restarts with a non-empty queue restored from the
on-disk snapshot, or when a cherry-pick fails in a way that requires
the user to repair the worktree by hand before the next queued merge
can proceed.

Clears `:halt-until-human' on the front queue entry before draining —
`claude-repl--reenqueue-merge-on-failure' sets that flag on generic
failures specifically so auto-drain does NOT retry them.  The
interactive kick IS the human signal that re-dispatch should proceed,
so the flag is dropped here and the entry becomes drainable.

No-op (with a `message') when the queue is empty.  No-op (with a
`message') when a cherry-pick is still in progress in any registered
workspace — the user must clear `CHERRY_PICK_HEAD' first (commit,
abort, or resolve) so the queue isn't dispatched into the middle of a
live merge.

The drain itself is the same `claude-repl--drain-merge-queue' that the
automatic path uses: one entry pops, the corresponding
`claude-repl--workspace-merge-into-source' runs, and its completion
cascades into the next drain."
  (interactive)
  (cond
   ((not (boundp 'claude-repl--merge-queue))
    (user-error "claude-repl: merge queue module not loaded"))
   ((null claude-repl--merge-queue)
    (message "[claude-repl] merge queue is empty — nothing to drain"))
   ((claude-repl--any-cherry-pick-in-progress-p)
    (user-error "claude-repl: a cherry-pick is still in progress — resolve it before draining"))
   (t
    (let ((front (car claude-repl--merge-queue)))
      (when (plist-get front :halt-until-human)
        (claude-repl--log nil
                          "drain-merge-queue: manual kick clearing :halt-until-human on front ws=%s"
                          (plist-get front :source-ws))
        (setcar claude-repl--merge-queue
                (plist-put (copy-sequence front) :halt-until-human nil))))
    (claude-repl--log nil
                      "drain-merge-queue: manual kick queue-len=%d"
                      (length claude-repl--merge-queue))
    (message "[claude-repl] draining merge queue (%d entries)"
             (length claude-repl--merge-queue))
    (claude-repl--drain-merge-queue))))

(defalias '+dwc/drain-merge-queue #'claude-repl-drain-merge-queue)

;;;; Merge-completed restore

(defun claude-repl--state-merge-completed-p (dir)
  "Return non-nil when the state.el under DIR has :merge-completed t.
Used by the snapshot loader to route merged-completed workspaces away
from `--establish-workspace' (which would re-create a persp and start
Claude) and into the lightweight `--register-merged-workspace' path.
Errors during the state-file read return nil so a malformed file
falls through to the normal establish path rather than blocking
startup."
  (when-let* ((state-file (and dir (claude-repl--state-file-for-read dir))))
    (and (file-exists-p state-file)
         (condition-case err
             (eq (plist-get (claude-repl--read-sexp-file state-file)
                            :merge-completed)
                 t)
           (error
            (claude-repl--log nil "state-merge-completed-p: read err file=%s err=%S"
                              state-file err)
            nil)))))

(defun claude-repl--register-merged-workspace (ws dir)
  "Register WS as a merged-completed workspace from on-disk state.
Reads DIR's state.el and populates `claude-repl--workspaces' with
just enough state for the drawer's MERGED bucket to render WS and for
`--finish-workspace' to later remove the worktree.  Does NOT create a
Doom persp and does NOT start Claude — the workspace is data-only
until the user presses `x' on its drawer entry.

Idempotent: a subsequent call overwrites the relevant plist fields.
Keys populated when present in the state file:
  :project-dir, :priority, :source-ws-dir, :last-prompt-summary,
  :last-prompt-time, :worktree-p, :merge-completed,
  :merge-completed-at, :merge-failed.

Runs `claude-repl--detect-merge-actually-landed-p' against git reality
to reclassify pre-:merge-failed workspaces: under the old flow, a
silent cherry-pick failure still wrote `:merge-completed t' without a
`:merge-failed' flag.  On load, the probe reads the parent worktree's
HEAD log for cherry-pick -x annotations of every target-branch commit;
any missing commit promotes the saved state to `:merge-failed t' /
`:repl-state :merge-failed' so the drawer surfaces the ❌ badge for
the first time.  Clean merges set `:repl-state :merged' so the 🔀
badge re-appears post-restart (the snapshot loader does not pass
through `--initialize-ws-env' for merged entries)."
  (claude-repl--with-error-logging (format "register-merged-workspace[%s]" ws)
    (let* ((state-file (claude-repl--state-file-for-read dir))
           (saved (and state-file
                       (file-exists-p state-file)
                       (condition-case err
                           (claude-repl--read-sexp-file state-file)
                         (error
                          (claude-repl--log ws "register-merged: state-read err err=%S" err)
                          nil)))))
      (claude-repl--log ws "register-merged: ws=%s dir=%s saved=%s"
                        ws dir (if saved "yes" "no"))
      ;; Merged workspaces are a re-registration path; clear any prior
      ;; tombstone so `--ws-live-p' agrees the entry is back in play.
      (claude-repl--ws-put ws :nuked-at nil)
      (claude-repl--ws-put ws :project-dir dir)
      (claude-repl--ws-put ws :merge-completed t)
      (when saved
        (dolist (key '(:priority :source-ws-dir :last-prompt-summary
                       :last-prompt-time :worktree-p :merge-completed-at))
          (when-let ((v (plist-get saved key)))
            (claude-repl--ws-put ws key v))))
      (let* ((saved-failed (and saved (eq (plist-get saved :merge-failed) t)))
             (landed (claude-repl--detect-merge-actually-landed-p ws))
             (failed (or saved-failed (not landed))))
        (claude-repl--log ws "register-merged: ws=%s saved-failed=%s landed=%s -> failed=%s"
                          ws saved-failed landed failed)
        (claude-repl--ws-put ws :merge-failed (when failed t))
        (claude-repl--ws-put ws :repl-state
                             (if failed :merge-failed :merged))))))

;;;; Project-switch wrapper

(defun claude-repl--hydrate-priority-from-state (project-root)
  "Hydrate :priority for the current workspace from PROJECT-ROOT's state file.
Reads the per-project state file (preferring `<PROJECT-ROOT>/.claude/emacs/state.el',
falling back to the legacy `<PROJECT-ROOT>/.claude-repl-state') and, if it
carries a `:priority', records it on the current workspace's plist so the
tabline badge renders without waiting for `claude-repl--initialize-ws-env'
\(which only runs when Claude actually starts).  No-op when the state
file is missing, malformed, or carries no `:priority'."
  (when-let* ((ws (ignore-errors (+workspace-current-name)))
              (state-file (claude-repl--state-file-for-read project-root))
              (saved (condition-case err
                         (claude-repl--read-sexp-file-if-exists state-file)
                       (error
                        (claude-repl--log ws "hydrate-priority: read error file=%s err=%S"
                                          state-file err)
                        nil)))
              (priority (plist-get saved :priority)))
    (claude-repl--log ws "hydrate-priority: ws=%s priority=%s" ws priority)
    (claude-repl--ws-put ws :priority priority)
    (force-mode-line-update t)))

(defvar recentf-list)

(defun claude-repl--most-recent-project-file (project-root)
  "Return the most-recently-accessed file under PROJECT-ROOT, or nil.
Uses `file-in-directory-p' (boundary-aware) rather than
`string-prefix-p' — the latter would mis-match `/p/foo' against a
file under `/p/foo-bar/' because the prefix isn't terminated at a
path separator.

Returns nil if `recentf-list' is unbound (recentf not loaded yet) or
contains no live file under PROJECT-ROOT.  Callers must still verify
the returned path exists before opening — `recentf-list' lags
filesystem deletions."
  (seq-find (lambda (file)
              (and (file-exists-p file)
                   (file-in-directory-p file project-root)))
            (bound-and-true-p recentf-list)))

;;;; Project picker (SPC p p)
;;
;; `claude-repl-switch-to-project' replaces the plain
;; `projectile-completing-read' candidate list with a richer column view:
;;
;;   <emoji> <project-name padded>   <created-date>   <last-killed-date>
;;
;; - The emoji prefix reflects the project's workspace state at picker time:
;;   for projects with a live workspace it mirrors the drawer's per-workspace
;;   glyph; for projects without a live workspace it falls back to a neutral
;;   📁.
;; - Date columns are populated from the live workspace's `:created-at' /
;;   `:last-killed-at' plist entries when one exists.  Projects without a
;;   live workspace show dash placeholders — we deliberately do NOT read the
;;   per-project state.el from disk on every `SPC p p' invocation, so the
;;   picker uses only the cached in-memory hash the drawer already maintains.
;; - The two date columns get distinct faces so they read at a glance.
;; - Entries are sorted most-recently-killed first, then by creation date
;;   when no kill is recorded — projects that need attention surface to the
;;   top.  Non-live projects (no cached dates) sort to the bottom.

(defface claude-repl-picker-created-face
  '((t :inherit font-lock-comment-face))
  "Face for the creation-date column in `claude-repl-switch-to-project'."
  :group 'claude-repl)

(defface claude-repl-picker-killed-face
  '((t :inherit error))
  "Face for the last-kill-date column in `claude-repl-switch-to-project'."
  :group 'claude-repl)

(defface claude-repl-picker-name-face
  '((t :inherit default))
  "Face for the project-name column in `claude-repl-switch-to-project'."
  :group 'claude-repl)

(defconst claude-repl--picker-date-format "%Y-%m-%d"
  "`format-time-string' template for the picker's date columns.")

(defconst claude-repl--picker-date-width 10
  "Width of each date column in the picker (matches
`claude-repl--picker-date-format').  Used to keep the placeholder
\"--\" aligned with real dates.")

(defconst claude-repl--picker-name-min-width 24
  "Minimum padding width for the project-name column in the picker.
Actual width is the max of this and the longest candidate basename.")

(defconst claude-repl--picker-column-gap "   "
  "Whitespace inserted between the picker's name and date columns.")

(defun claude-repl--project-has-live-workspace-p (project-root)
  "Return non-nil when any registered workspace points at PROJECT-ROOT.
Compares `expand-file-name' results so trailing-slash and `~/' vs.
absolute differences don't cause false negatives.  Returns nil for nil
PROJECT-ROOT."
  (when project-root
    (let ((canonical (file-name-as-directory (expand-file-name project-root)))
          (found nil))
      (maphash (lambda (_ws plist)
                 (when-let ((dir (plist-get plist :project-dir)))
                   (when (equal (file-name-as-directory (expand-file-name dir))
                                canonical)
                     (setq found t))))
               claude-repl--workspaces)
      found)))

(defun claude-repl--project-state-summary (project-root)
  "Return a plist summarizing PROJECT-ROOT's in-memory workspace state.

Sources values exclusively from `claude-repl--workspaces' (the live
hash) — performs NO disk I/O — so the picker reflects exactly the
cached values the drawer renders and a `SPC p p' invocation does not
fan out to a state-file read for every projectile-known project.

When a live workspace points at PROJECT-ROOT, `:created-at',
`:last-killed-at', and `:priority' are returned from that workspace's
plist.  When no live workspace matches, all those values are nil and
the picker falls back to a neutral non-live emoji + dash placeholders.

Keys:
  `:created-at'      from ws plist when live, else nil
  `:last-killed-at'  from ws plist when live, else nil
  `:priority'        from ws plist when live, else nil
  `:live-p'          non-nil iff a live workspace matches PROJECT-ROOT
  `:workspace-name'  the matching live ws name, or nil"
  (let ((workspace-name (and project-root
                             (claude-repl--ws-name-for-dir project-root))))
    (list :created-at (and workspace-name
                           (claude-repl--ws-get workspace-name :created-at))
          :last-killed-at (and workspace-name
                               (claude-repl--ws-get workspace-name :last-killed-at))
          :priority (and workspace-name
                         (claude-repl--ws-get workspace-name :priority))
          :live-p (not (null workspace-name))
          :workspace-name workspace-name)))

(defun claude-repl--picker-status-emoji (summary)
  "Return the status-emoji prefix for a candidate with SUMMARY.
SUMMARY is a plist from `claude-repl--project-state-summary'.

When `:workspace-name' is non-nil, returns the same glyph the drawer
would render for that workspace via `claude-repl-drawer--state-glyph'
— keeps emoji usage for a given workspace consistent between the
project picker (`SPC p p') and the drawer.

For projects without a live workspace returns a neutral 📁.  No
historical kill/dormant distinction is drawn because the picker
deliberately avoids disk I/O — the on-disk state file is NOT read on
every invocation."
  (let ((ws (plist-get summary :workspace-name)))
    (if ws
        (claude-repl-drawer--state-glyph ws)
      "📁")))

(defun claude-repl--picker-format-date (time width face placeholder)
  "Return a propertized fixed-width date string for TIME.
Format via `claude-repl--picker-date-format', then `truncate-string-to-width'
PLACEHOLDER (the dashes shown when TIME is nil) to WIDTH so the column
lines up regardless of whether TIME is set.  FACE is applied to the
result."
  (let ((str (if time
                 (format-time-string claude-repl--picker-date-format time)
               (truncate-string-to-width placeholder width 0 ?\s))))
    (propertize str 'face face)))

(defun claude-repl--picker-name-width (project-roots)
  "Return the padding width to use for the project-name column.
Max of `claude-repl--picker-name-min-width' and the longest basename
across PROJECT-ROOTS so every row's date columns start at the same
character position."
  (let ((max-basename
         (apply #'max 0
                (mapcar (lambda (p)
                          (length (file-name-nondirectory
                                   (directory-file-name p))))
                        project-roots))))
    (max claude-repl--picker-name-min-width max-basename)))

(defun claude-repl--picker-sort-key (summary)
  "Return the `:last-killed-at' time for SUMMARY, or its `:created-at'.
Used as the sort key so most-recently-killed projects surface first, with
never-killed projects falling back to creation-date order (also most-recent
first).  Returns nil when neither timestamp is available; callers treat
nil keys as oldest."
  (or (plist-get summary :last-killed-at)
      (plist-get summary :created-at)))

(defun claude-repl--picker-time-greater-p (a b)
  "Compare two `current-time'-shaped values: non-nil A newer than nil B."
  (cond ((and a b) (time-less-p b a))
        (a t)
        (t nil)))

(defun claude-repl--build-project-picker-candidates (project-roots)
  "Return a sorted alist of (display-string . project-root) for PROJECT-ROOTS.

Each entry's display string prefixes a status emoji, then a name column
padded to a width derived from the longest basename, then two date
columns (creation date, last kill/nuke date) separated by
`claude-repl--picker-column-gap'.  Empty date placeholders keep the
columns aligned across all rows.

Sort order: most-recently-killed first; never-killed projects sort by
creation date (most-recent first).  This is the input
`projectile-completing-read' / `ivy-read' receives."
  (let* ((name-width (claude-repl--picker-name-width project-roots))
         (entries (mapcar
                   (lambda (root)
                     (let* ((basename (file-name-nondirectory
                                       (directory-file-name root)))
                            (summary (claude-repl--project-state-summary root)))
                       (list :root root
                             :basename basename
                             :summary summary)))
                   project-roots))
         (sorted (sort entries
                       (lambda (a b)
                         (claude-repl--picker-time-greater-p
                          (claude-repl--picker-sort-key (plist-get a :summary))
                          (claude-repl--picker-sort-key (plist-get b :summary)))))))
    (mapcar
     (lambda (entry)
       (let* ((root (plist-get entry :root))
              (basename (plist-get entry :basename))
              (summary (plist-get entry :summary))
              (emoji (claude-repl--picker-status-emoji summary))
              (name-padded
               (propertize
                (truncate-string-to-width basename name-width 0 ?\s)
                'face 'claude-repl-picker-name-face))
              (created (claude-repl--picker-format-date
                        (plist-get summary :created-at)
                        claude-repl--picker-date-width
                        'claude-repl-picker-created-face
                        "----------"))
              (killed (claude-repl--picker-format-date
                       (plist-get summary :last-killed-at)
                       claude-repl--picker-date-width
                       'claude-repl-picker-killed-face
                       "----------"))
              (display (concat emoji " "
                               name-padded
                               claude-repl--picker-column-gap
                               created
                               claude-repl--picker-column-gap
                               killed)))
         (cons display root)))
     sorted)))

(defun claude-repl--read-project-via-picker ()
  "Prompt for a project root with the rich column picker.
Returns the selected project root path (the cdr of the matched
candidate) — never the propertized display string.  Uses `ivy-read' when
available (it preserves text-property faces in the candidate list) and
falls back to `completing-read'.

Captures the choice via the action closure rather than `ivy-read''s
return value because ivy's return shape for cons-cell candidates varies
across versions (sometimes the cons, sometimes the car); the action
sees `c' in a consistent shape so we can normalize once."
  (let* ((roots (projectile-relevant-known-projects))
         (candidates (claude-repl--build-project-picker-candidates roots))
         (selected nil))
    (if (fboundp 'ivy-read)
        (ivy-read "Switch to project: " candidates
                  :action (lambda (c)
                            (setq selected (cond ((consp c) (cdr c))
                                                 ((stringp c)
                                                  (cdr (assoc c candidates)))
                                                 (t c))))
                  :require-match t
                  :caller 'claude-repl-switch-to-project)
      (let* ((choice (completing-read "Switch to project: "
                                      (mapcar #'car candidates)
                                      nil t))
             (hit (assoc choice candidates)))
        (setq selected (and hit (cdr hit)))))
    selected))

(defun claude-repl-switch-to-project (&optional project)
  "Switch to PROJECT and hydrate the workspace's priority badge.
PROJECT is a project root path; when nil, prompt via
`claude-repl--read-project-via-picker' (rich column view sorted by
last-kill / creation date).

Switches via `projectile-switch-project-by-name' (which fires Doom's
`+workspaces-switch-to-project-h' to create/activate the persp keyed
on the project basename), then opens the most-recently-accessed file
under PROJECT via `claude-repl--most-recent-project-file', hydrates
the saved `:priority' from the per-project state file (so the
tabline badge appears immediately on `SPC p p' instead of only once
Claude starts), and flashes the activated tab.

Distinct from `claude-repl--switch-to-workspace': that primitive is
name-keyed and assumes the persp already exists; this one is
project-keyed and creates the persp via the Doom hook.  Both differ
from `claude-repl--establish-workspace', which is a snapshot-restore
path that bypasses the Doom hook to preserve the snapshot's exact ws
name."
  (interactive)
  (let ((project (or project (claude-repl--read-project-via-picker))))
    (when project
      (projectile-switch-project-by-name project)
      (when-let ((recent-file (claude-repl--most-recent-project-file project)))
        (when (file-exists-p recent-file)
          (find-file recent-file)))
      (claude-repl--hydrate-priority-from-state project)
      (claude-repl--flash-current-tab))))

;;;; Workspace cycling (hide-mode aware)

(defun claude-repl--workspace-cycle (n)
  "Cycle N workspaces (negative = left, positive = right).
Reimplements `+workspace/cycle' but iterates the hide-mode-filtered
visible workspace list (`claude-repl--filter-hidden-names') instead of
the raw `+workspace-list-names', so closed-REPL workspaces dropped from
the tabline are also skipped during s-{ / s-}.  Mirrors Doom's
protected-workspace handling: when current is the nil-persp, switch to
`+workspaces-main' instead of cycling.  Does NOT flash the destination
tab — left/right cycling is high-frequency navigation and the flash
becomes noise; identity-based jumps (`SPC p p', priority change,
worktree jump) keep the flash since they're discrete attention cues."
  (let ((current-name (+workspace-current-name)))
    (if (+workspace--protected-p current-name)
        (+workspace-switch +workspaces-main t)
      (condition-case-unless-debug ex
          (let* ((visible (claude-repl--filter-hidden-names
                           (+workspace-list-names) current-name))
                 (perspc (length visible))
                 (index (cl-position current-name visible :test #'equal)))
            (when (= perspc 1)
              (user-error "No other workspaces"))
            (+workspace-switch (nth (mod (+ index n) perspc) visible)))
        ('user-error (+workspace-error (cadr ex) t))
        ('error (+workspace-error ex t))))))

(defun claude-repl-switch-left ()
  "Cycle one workspace left, skipping hide-mode-filtered workspaces.
Drop-in replacement for `+workspace/switch-left' that honors
`claude-repl-hide-mode-enabled'."
  (interactive)
  (claude-repl--workspace-cycle -1))

(defun claude-repl-switch-right ()
  "Cycle one workspace right, skipping hide-mode-filtered workspaces.
Drop-in replacement for `+workspace/switch-right' that honors
`claude-repl-hide-mode-enabled'."
  (interactive)
  (claude-repl--workspace-cycle +1))

;;;; Indexed workspace switchers (M-1..M-9, M-0 bindings)
;;
;; Thin persp wrappers around `+workspace-switch' used in place of the
;; Doom `+workspace/switch-to-N' / `+workspace/switch-to-final' commands
;; for the M-1..M-9 / M-0 bindings.  They were extracted to make the
;; workspace-jump bindings ignore `current-prefix-arg' entirely — Doom's
;; `+workspace/switch-to' inspects `current-prefix-arg' in its
;; `interactive' form, which sporadically caused M-9 to land on the
;; final workspace (when the previous key sequence had set a prefix
;; arg) instead of the 9th, and M-0 to fall through to a no-op
;; `text-scale-set' with the "The font hasn't been resized" message.
;; These wrappers take no prefix argument and call `+workspace-switch'
;; directly by name, so the behaviour is deterministic.
;;
;; `claude-repl--workspace-switch-by-index' is the shared core; the
;; named commands below are the only entry points bound to keys.

(defun claude-repl--workspace-switch-by-index (index)
  "Switch to workspace at zero-based INDEX in `+workspace-list-names'.
Signals `user-error' if INDEX is out of range.  Pure persp wrapper —
does not consult `current-prefix-arg' and does not flash the tab."
  (let* ((names (+workspace-list-names))
         (dest (nth index names)))
    (unless dest
      (user-error "No workspace at #%s" (1+ index)))
    (+workspace-switch dest)))

(defun claude-repl-workspace-switch-to-0 ()
  "Switch to the 1st workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (claude-repl--workspace-switch-by-index 0))

(defun claude-repl-workspace-switch-to-1 ()
  "Switch to the 2nd workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (claude-repl--workspace-switch-by-index 1))

(defun claude-repl-workspace-switch-to-2 ()
  "Switch to the 3rd workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (claude-repl--workspace-switch-by-index 2))

(defun claude-repl-workspace-switch-to-3 ()
  "Switch to the 4th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (claude-repl--workspace-switch-by-index 3))

(defun claude-repl-workspace-switch-to-4 ()
  "Switch to the 5th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (claude-repl--workspace-switch-by-index 4))

(defun claude-repl-workspace-switch-to-5 ()
  "Switch to the 6th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (claude-repl--workspace-switch-by-index 5))

(defun claude-repl-workspace-switch-to-6 ()
  "Switch to the 7th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (claude-repl--workspace-switch-by-index 6))

(defun claude-repl-workspace-switch-to-7 ()
  "Switch to the 8th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (claude-repl--workspace-switch-by-index 7))

(defun claude-repl-workspace-switch-to-8 ()
  "Switch to the 9th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (claude-repl--workspace-switch-by-index 8))

(defun claude-repl-workspace-switch-to-final ()
  "Switch to the final (last) workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (let* ((names (+workspace-list-names))
         (dest (car (last names))))
    (unless dest
      (user-error "No workspaces"))
    (+workspace-switch dest)))
