;;; commands.el --- user commands for agent-repl -*- lexical-binding: t; -*-

;;; Code:

;; Forward declarations: defined in worktree.el (loaded after commands.el).
;; Snapshot save/load helpers and the interactive drain command in this
;; file refer to these symbols, so the names must be readable here at
;; compile/load time.
(defvar agent-repl--merge-queue)
(defvar agent-repl--in-flight-merges)
(declare-function agent-repl--drain-merge-queue "worktree")
(declare-function agent-repl--merge-queue-target-dirs "worktree")
(declare-function agent-repl--merge-queue-front-for-target "worktree")

;; Forward declaration: defined in hide-project-dirs.el (loaded after
;; commands.el).  The snapshot writer/loader persists and restores this
;; toggle so the hidden set survives an Emacs restart.
(defvar agent-repl-hide-project-dirs-enabled)

;;;; Customization — prompts & diff specs

(defcustom agent-repl-branch-diff-spec
  "changes in current branch (git diff $(git merge-base HEAD origin/master))"
  "Change-spec string used for branch-level diff analysis commands."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-explain-diff-prompt
  "please explain the changes"
  "Prompt sent to the agent by explain-diff commands."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-update-pr-diff-prompt
  "please update the PR description"
  "Prompt sent to the agent by update-pr-diff commands."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-update-pr-prompt
  "please update the PR description for the PR corresponding to our branch"
  "Prompt sent to the agent by `agent-repl-update-pr'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-rebase-onto-origin-master-prompt
  "please rebase the current branch onto origin/master (I already ran `git fetch origin` for you), resolving any conflicts as appropriate"
  "Prompt sent to the agent by `agent-repl-rebase-onto-origin-master'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-create-or-update-pr-base-flags
  '("commit" "--patch" "--self-certified" "--add-to-merge-queue" "--rebase")
  "Default flag list for the /create-or-update-pr slash command.
`agent-repl-create-or-update-pr' joins these into the prompt, dropping
any flag whose exclusion symbol appears in its EXCLUDED argument."
  :type '(repeat string)
  :group 'agent-repl)

(defcustom agent-repl-run-tests-prompt
  "please run tests, and summarize the issues found and probable causes"
  "Prompt sent to the agent by run-tests commands."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-run-lint-prompt
  "please run lint, and address any issues found"
  "Prompt sent to the agent by run-lint commands."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-run-all-prompt
  "please run lint and tests, and address any issues found for both"
  "Prompt sent to the agent by run-all commands."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-test-quality-prompt
  "please analyze tests to ensure they are following AAA standards for testing. Please be sure to confine your analysis to the specified context (branch, HEAD, uncommitted changes, etc). They should be employing DRY principle for refactoring as well (extract repeated code into helpers, use builder pattern to facilitate test DSL). We should only be testing one thing per test (can extract tests into subtests to ensure this). Ensure that tests are correctly grouped into subtests, and that very similar/redundant suites are merged. We should not be using ANY timing logic in tests. If there is any timing logic found, surface it. It is FINE for potentially hanging tests to become unblocked with ERROR after some amount of time -- we are only concerned with not attempting to ballpark synchronization via time. We should be careful to NOT reduce the production code path coverage of our refactors -- for example, we should avoid removing asserts in the effort to 'only test one thing', and instead prefer adding a new subtest. Please spin up ONE AGENT PER TEST FILE!"
  "Prompt sent to the agent by test-quality commands."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-test-coverage-prompt
  "Please be sure to confine your analysis to the specified context (branch, HEAD, uncommitted changes, etc). <<IF AND ONLY IF YOU JUST PRODUCED A LIST OF EDGE CASES>>: write up a plan for producing a unit test that covers each and every one of the edge cases you just enumerated. Each test should cover *precisely* one edge case. Each test file should be worked on by a separate agent. <<IF AND ONLY IF YOU DID NOT -- I REPEAT, NOT -- JUST PRODUCE A LIST OF EDGE CASES IN YOUR LAST RESPONSE MESSAGE>>: please enumerate each and every edge cases introduced or modified by each and every function added or modified."
  "Prompt sent to the agent by test-coverage commands."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-diff-analysis-message-template "for the %s, %s"
  "Format string for diff analysis messages sent to Claude.
First %s is the change-spec, second %s is the prompt."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-explain-prompt-template "please explain %s"
  "Format string for the explain command prompt.
%s is replaced with the context reference (file:line or file:range)."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-interrupt-escape-count 2
  "Number of Escape key presses sent to interrupt Claude."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-interrupt-reinsert-delay 0.25
  "Seconds to wait after interrupting before re-entering insert mode."
  :type 'number
  :group 'agent-repl)

;;;; Session helpers

(defun agent-repl--send-to-agent (text)
  "Send TEXT to Claude, starting it if needed.
The `:permission' -> `:thinking' flip is inherited from
`agent-repl--send-input-to-vterm' (the lowest-level send primitive),
so predefined-prompt sends (e.g. `agent-repl-create-or-update-pr')
answer permission prompts the same way every other send path does
even though this path does NOT funnel through `agent-repl--do-send'."
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log ws "send-to-agent len=%d" (length text))
    (unless (agent-repl--agent-running-p ws)
      (agent-repl--initialize-agent ws))
    (agent-repl--send-input-to-vterm
     (agent-repl--ws-get ws :vterm-buffer) text)))

;;;; File reference helpers

(defun agent-repl--buffer-relative-path ()
  "Return the current buffer's file path relative to the project root."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (let ((rel (file-relative-name (agent-repl--path-canonical file) (agent-repl--ws-dir (agent-repl--ws-current-name)))))
      (agent-repl--log (agent-repl--ws-current-name) "buffer-relative-path: path=%s" rel)
      rel)))

(defun agent-repl--format-file-ref ()
  "Return a file:line or file:startline-endline reference string.
With active region: returns file:startline-endline and deactivates mark.
Without region: returns file:line."
  (let ((rel (agent-repl--buffer-relative-path)))
    (if (use-region-p)
        (let ((start-line (line-number-at-pos (region-beginning)))
              (end-line (line-number-at-pos (region-end))))
          (deactivate-mark)
          (agent-repl--log (agent-repl--ws-current-name) "format-file-ref: region branch start=%d end=%d" start-line end-line)
          (format "%s:%d-%d" rel start-line end-line))
      (agent-repl--log (agent-repl--ws-current-name) "format-file-ref: single-line branch line=%d" (line-number-at-pos (point)))
      (format "%s:%d" rel (line-number-at-pos (point))))))

(defun agent-repl--format-magit-hunk-ref ()
  "Format a file reference for the current magit hunk context.
Returns a \"file:startline-endline\" string based on the hunk's to-range."
  (let* ((section (magit-current-section))
         (file (magit-file-at-point))
         (range (eieio-oref section 'to-range))
         (start (car range))
         (len (cadr range))
         (end (+ start len -1))
         (rel (file-relative-name
               (agent-repl--path-canonical (expand-file-name file (magit-toplevel)))
               (agent-repl--ws-dir (agent-repl--ws-current-name))))
         (ref (format "%s:%d-%d" rel start end)))
    (agent-repl--log (agent-repl--ws-current-name) "format-magit-hunk-ref: ref=%s" ref)
    ref))

(defun agent-repl--context-reference ()
  "Return a context-appropriate file reference string.
In a magit hunk: returns the hunk's file:startline-endline.
Otherwise: delegates to `agent-repl--format-file-ref' (which handles
both active region and point-at-line cases)."
  (if (and (derived-mode-p 'magit-diff-mode 'magit-status-mode
                           'magit-revision-mode)
           (magit-section-match 'hunk))
      (progn
        (agent-repl--log (agent-repl--ws-current-name) "context-reference: magit-hunk branch")
        (agent-repl--format-magit-hunk-ref))
    (agent-repl--log (agent-repl--ws-current-name) "context-reference: standard branch")
    (agent-repl--format-file-ref)))

;;;; Code linking (open file + select line range in a left window)

(defun agent-repl--select-line-range (start-line &optional end-line)
  "Select the inclusive line range START-LINE..END-LINE in the current buffer.
Widens, then activates a region from the beginning of START-LINE to the
end of END-LINE (defaults to START-LINE).  Line numbers are 1-indexed;
START-LINE is floored at 1 and END-LINE is floored at START-LINE, and
both clamp to the buffer's last line.  Leaves point at the beginning of
START-LINE so the region's top is what a follow-up `recenter' brings
into view."
  (let ((start-line (max 1 start-line))
        (end-line (max (max 1 start-line) (or end-line start-line))))
    (widen)
    (goto-char (point-min))
    (forward-line (1- start-line))
    (let ((beg (line-beginning-position)))
      (forward-line (- end-line start-line))
      (set-mark (line-end-position))
      (goto-char beg)
      (activate-mark))))

(defun agent-repl--link-code-display (buf start-line end-line)
  "Display BUF in a left-docked window with START-LINE..END-LINE selected.
Docks a window to the leftmost edge of the frame (half width), selects
the inclusive line range via `agent-repl--select-line-range', and
recenters so the region's top is brought into view.  Does NOT select
the displayed window, so input focus stays wherever it was.  Returns the
window, or nil when `display-buffer' declined to produce one."
  (let ((win (display-buffer
              buf
              '((display-buffer-in-direction)
                (direction . leftmost)
                (window-width . 0.5)))))
    (when win
      (with-selected-window win
        (agent-repl--select-line-range start-line end-line)
        (recenter)))
    win))

(defun agent-repl-link-code (file start-line &optional end-line workspace)
  "Open FILE in a window and select lines START-LINE..END-LINE.
Code-linking entry point for the runtime-eval-code skill.

In all cases the buffer is genuinely OPENED in a visible window docked
to the leftmost edge of the frame (not merely visited in the
background): the file is visited via `find-file-noselect', displayed via
`agent-repl--link-code-display', the line range selected, and the
window recentered.

When WORKSPACE is non-nil (a workspace name string), the buffer is also
registered into WORKSPACE's perspective via `agent-repl--ws-add-buffer'
so it is owned by the right perspective, and input focus is NOT stolen
(the window is displayed but not selected).  This is the path the
runtime-eval-code skill uses.

When WORKSPACE is nil (direct interactive use), the displayed window is
additionally selected so the user lands directly on the code.

Either way returns the window the buffer was displayed in, or nil when
`display-buffer' declined to produce one.

FILE is run through `expand-file-name', so pass an absolute path.
START-LINE and END-LINE are 1-indexed inclusive line numbers."
  (let* ((path (expand-file-name file))
         (buf (find-file-noselect path))
         (win (agent-repl--link-code-display buf start-line end-line)))
    (if workspace
        (let ((persp (agent-repl--ws-resolve-persp workspace)))
          (when persp
            (agent-repl--ws-add-buffer buf persp nil))
          (agent-repl--log workspace
                            "link-code: no-focus path file=%s lines=%s..%s persp=%s win=%s"
                            path start-line (or end-line start-line)
                            (if persp "resolved" "nil")
                            (if win "displayed" "nil")))
      (when win (select-window win))
      (agent-repl--log nil
                        "link-code: focus path file=%s lines=%s..%s win=%s"
                        path start-line (or end-line start-line)
                        (if win "displayed+selected" "nil")))
    win))

;;;; Diff analysis infrastructure

(defun agent-repl--send-diff-analysis (change-spec prompt)
  "Send a diff analysis request to Claude.
CHANGE-SPEC describes which changes (e.g. \"unstaged changes (git diff)\").
PROMPT is the analysis instruction."
  (let ((msg (format agent-repl-diff-analysis-message-template change-spec prompt)))
    (agent-repl--log (agent-repl--ws-current-name) "diff-analysis: %s" change-spec)
    (agent-repl--send-to-agent msg)))

(defconst agent-repl--diff-scopes
  '((worktree    . "unstaged changes (git diff)")
    (staged      . "staged changes (git diff --cached)")
    (uncommitted . "uncommitted changes (git diff HEAD)")
    (head        . "last commit (git show HEAD)")
    (branch      . :use-branch-diff-spec))
  "Alist mapping scope names to their change-spec strings.
The special value `:use-branch-diff-spec' means use `agent-repl-branch-diff-spec'.")

(defconst agent-repl--diff-scope-labels
  '((worktree    . "unstaged changes")
    (staged      . "staged changes")
    (uncommitted . "all uncommitted changes")
    (head        . "the last commit")
    (branch      . "all changes in the current branch"))
  "Alist mapping scope symbols to human-readable labels for docstrings.")

(defconst agent-repl--update-pr-diff-scopes
  '((worktree    . "UNSTAGED changes (git diff). Do not consider staged changes or committed changes.")
    (staged      . "STAGED changes (git diff --cached). Do not consider unstaged changes or committed changes.")
    (uncommitted . "All UNCOMMITTED changes (git diff HEAD). Consider BOTH staged and unstaged changes. Do not consider committed changes.")
    (head        . "last commit (git show HEAD)."))
  "Scope overrides for `update-pr-diff' commands.
These provide more explicit instructions than the standard scopes.
The `branch' scope is omitted and falls through to the default.")

(defun agent-repl--resolve-change-spec (scope default-spec scope-overrides)
  "Resolve the change-spec form for SCOPE.
DEFAULT-SPEC is the value from `agent-repl--diff-scopes'.
SCOPE-OVERRIDES, when non-nil, is a symbol naming an alist of
 (SCOPE . CHANGE-SPEC) that takes precedence over DEFAULT-SPEC.
Returns a string literal or the symbol `agent-repl-branch-diff-spec'."
  (let ((override (and scope-overrides
                       (cdr (assq scope (eval scope-overrides))))))
    (cond
     (override
      (agent-repl--log nil "resolve-change-spec: override branch scope=%s" scope)
      override)
     ((eq default-spec :use-branch-diff-spec)
      (agent-repl--log nil "resolve-change-spec: branch-spec branch scope=%s" scope)
      'agent-repl-branch-diff-spec)
     (t
      (agent-repl--log nil "resolve-change-spec: default branch scope=%s" scope)
      default-spec))))

(defun agent-repl--diff-command-form (scope-entry family doc-verb prompt-var scope-overrides)
  "Build one `defun' form for a diff-analysis command.
SCOPE-ENTRY is a (SCOPE . DEFAULT-SPEC) pair from `agent-repl--diff-scopes'.
FAMILY, DOC-VERB, PROMPT-VAR, and SCOPE-OVERRIDES are forwarded from the
macro `agent-repl--define-diff-commands'."
  (let* ((scope (car scope-entry))
         (doc-scope (cdr (assq scope agent-repl--diff-scope-labels)))
         (fn-name (intern (format "agent-repl-%s-%s" family scope)))
         (change-spec-form (agent-repl--resolve-change-spec
                            scope (cdr scope-entry) scope-overrides)))
    `(defun ,fn-name ()
       ,(format "%s %s." doc-verb doc-scope)
       (interactive)
       (agent-repl--send-diff-analysis ,change-spec-form ,prompt-var))))

(defmacro agent-repl--define-diff-commands (family doc-verb prompt-var &optional scope-overrides)
  "Define 5 diff-analysis commands for FAMILY.

Each generated command is named `agent-repl-FAMILY-SCOPE' for SCOPE in
worktree, staged, uncommitted, head, and branch.  DOC-VERB is used in
docstrings (e.g. \"Explain\" produces \"Explain unstaged changes.\").
PROMPT-VAR is the symbol of the prompt variable to pass.

SCOPE-OVERRIDES, when non-nil, is a symbol naming an alist of (SCOPE . CHANGE-SPEC)
that replaces the default change-spec from `agent-repl--diff-scopes'
for specific scopes."
  (declare (indent 2))
  `(progn
     ,@(cl-loop for scope-entry in agent-repl--diff-scopes
                collect (agent-repl--diff-command-form
                         scope-entry
                         family doc-verb prompt-var scope-overrides))))

;;;; Diff command families

(agent-repl--define-diff-commands explain-diff "Explain"
  agent-repl-explain-diff-prompt)

(agent-repl--define-diff-commands update-pr-diff "Update the PR description for"
  agent-repl-update-pr-diff-prompt
  agent-repl--update-pr-diff-scopes)

(agent-repl--define-diff-commands run-tests "Run tests for"
  agent-repl-run-tests-prompt)

(agent-repl--define-diff-commands run-lint "Run lint for"
  agent-repl-run-lint-prompt)

(agent-repl--define-diff-commands run-all "Run lint and tests for"
  agent-repl-run-all-prompt)

(agent-repl--define-diff-commands test-quality "Analyze test quality for"
  agent-repl-test-quality-prompt)

(agent-repl--define-diff-commands test-coverage "Analyze test coverage for"
  agent-repl-test-coverage-prompt)

;;;; Standalone commands

(defun agent-repl-explain ()
  "Ask Claude to explain the current context.
In a magit hunk: sends the hunk's file path and line range.
With active region: sends file path and line range.
Without region: sends file path and current line."
  (interactive)
  (let* ((ref (agent-repl--context-reference))
         (msg (format agent-repl-explain-prompt-template ref)))
    (agent-repl--log (agent-repl--ws-current-name) "explain %s" msg)
    (agent-repl--send-to-agent msg)))

(defun agent-repl-explain-prompt ()
  "Prompt the user for a message to send to Claude about the current context.
Pre-fills the minibuffer with the context reference (file:line or file:range).
In a magit hunk: pre-fills with the hunk's file path and line range.
With active region: pre-fills with file path and line range.
Without region: pre-fills with file path and current line."
  (interactive)
  (let* ((ref (agent-repl--context-reference))
         (msg (read-string "Send to Claude: " ref)))
    (when (and msg (not (string-empty-p msg)))
      (agent-repl--log (agent-repl--ws-current-name) "explain-prompt %s" msg)
      (agent-repl--send-to-agent msg))))

;;;; Explain config -- read-only Q&A about this doom config via headless claude

(defcustom agent-repl-explain-config-dir "~/.config/doom"
  "Working directory for the headless `claude -p' spawned by
`agent-repl-explain-config'.  Resolves to the canonical doom config
checkout (not the current worktree) so the explainer sees the user's
installed configuration."
  :type 'string
  :group 'agent-repl)

;; NOTE: the headless executable is no longer a defcustom here — it is
;; resolved from the default agent backend (explain-config is a global
;; utility with no workspace in scope) via
;; `agent-repl--backend-headless-cmd', which owns the one-shot flag
;; spelling (`-p' for claude, `exec' for codex, ...).

(defcustom agent-repl-explain-config-model "haiku"
  "Model alias pinned for the headless config-explainer run.
`explain-config' is short-form Q&A, so the small/fast model is used
rather than the default-tier model."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-explain-config-extra-args
  '("--dangerously-skip-permissions")
  "Extra flags appended to the headless config-explainer invocation.
`--dangerously-skip-permissions' prevents the run from prompting for
tool approval headlessly (in one-shot mode there is no one to approve)."
  :type '(repeat string)
  :group 'agent-repl)

(defcustom agent-repl-explain-config-buffer-name "*agent-explain-config*"
  "Buffer name where explain-config output is collected and displayed."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-explain-config-width-fraction 0.5
  "Fraction of frame width for the explain-config right-side popup.
Only applies when the popup falls back to its own side window (i.e.
when the agent output window is not visible to take over).  Width
is inherited from the agent output window when the popup takes
that window over — see `agent-repl--explain-config-show'.  The
drawer is untouched in either branch."
  :type 'float
  :group 'agent-repl)

(defun agent-repl--explain-config-window-width (window)
  "Return the configured explain-config width in columns for WINDOW.
Resolves `agent-repl-explain-config-width-fraction' against the
host frame's width."
  (let ((frame-cols (frame-width (window-frame window))))
    (max 1 (round (* agent-repl-explain-config-width-fraction frame-cols)))))

(defvar agent-repl--explain-config-display-action
  `((display-buffer-in-side-window)
    (side . right)
    (slot . 0)
    (window-width . ,#'agent-repl--explain-config-window-width)
    (window-parameters
     (no-delete-other-windows . t)
     (no-other-window . nil)))
  "Fallback display action for the explain-config output buffer.
Used only when the agent output window is not visible to take
over — when it is, `--show' reuses it directly via
`set-window-buffer' and bypasses `display-buffer' entirely.  The
drawer is never touched.  Reconciled across workspace switches via
the persp-activated hook.")

(defvar agent-repl--explain-config-global-visible-p nil
  "Non-nil when the explain-config popup should appear in every persp.
Set by `agent-repl--explain-config-show', cleared by
`agent-repl--explain-config-hide'.  The persp-activated hook
(`agent-repl--explain-config-ensure-visible-on-persp-switch')
consults this flag and re-displays the popup in newly-activated
workspaces so it feels like a frame-level UI element rather than a
per-workspace artifact — mirrors the drawer's own
`--global-visible-p' pattern.")

(defvar agent-repl--explain-config-replaced-window nil
  "When the popup has taken over the agent output window, holds (WIN . PREV-BUF).
WIN is the live agent output window the popup took over; PREV-BUF
is the buffer that window was displaying before takeover (the
agent output buffer for the current workspace).  Nil when the
popup is hosted in its own side window (i.e. the agent output
window was not visible at show time).  Consumed by
`agent-repl--explain-config-hide' to restore the prior buffer in
the same window position when the popup closes.")

(defun agent-repl--explain-config-apply-width (window)
  "Resize WINDOW to the configured explain-config width.
Side-window action alists honor `window-width' only at window-creation
time, so re-displaying the popup keeps its old width if the fraction
changed.  This forces the resize on every show — mirrors the drawer's
`--apply-width'."
  (let* ((target (agent-repl--explain-config-window-width window))
         (window-min-width 1))
    (with-selected-window window
      (setq-local window-size-fixed nil)
      (let ((delta (- target (window-total-width window))))
        (cond
         ((> delta 0) (enlarge-window delta t))
         ((< delta 0) (shrink-window (abs delta) t)))))))

(defun agent-repl--explain-config-current-agent-output-window ()
  "Return the live agent output window in the selected frame, or nil.
Looks up the current workspace's agent output panel via
`agent-repl-window--panel-window' with the `:vterm' key (the
existing panel-lookup key — note we do NOT introduce that name
here, the popup itself only deals in \"agent output\").  Guards
on `fboundp' so callers in load order before panels.el (e.g. early
test harnesses) get nil instead of a void-function error."
  (and (fboundp 'agent-repl-window--panel-window)
       (agent-repl-window--panel-window :vterm)))

(defun agent-repl--explain-config-take-over-agent-output-window (output-win buf)
  "Swap OUTPUT-WIN's buffer for BUF and record the original for restoration.
The agent output panel is a dedicated window, so this temporarily
clears `window-dedicated-p' before `set-window-buffer' — otherwise
the swap errors.  The pre-swap buffer is stashed in
`agent-repl--explain-config-replaced-window' so
`agent-repl--explain-config-hide' can restore it.  Returns OUTPUT-WIN."
  (let ((prev-buf (window-buffer output-win)))
    (set-window-dedicated-p output-win nil)
    (set-window-buffer output-win buf)
    (setq agent-repl--explain-config-replaced-window
          (cons output-win prev-buf)))
  output-win)

(defun agent-repl--explain-config-restore-replaced-window ()
  "Restore the buffer in the window the popup took over, if any.
No-op when no window was replaced or when the window or its prior
buffer is no longer live.  Re-applies the agent output window
hardening (dedicate / size-fix / delete-protect) on success so the
restored window matches its original recipe."
  (when-let ((cell agent-repl--explain-config-replaced-window))
    (setq agent-repl--explain-config-replaced-window nil)
    (let ((win (car cell))
          (prev (cdr cell)))
      (when (and (window-live-p win) (buffer-live-p prev))
        (set-window-buffer win prev)
        (when (fboundp 'agent-repl--configure-vterm-window)
          (agent-repl--configure-vterm-window win))))))

(defun agent-repl--explain-config-show ()
  "Display the explain-config buffer.
Sets the global visible-flag so the popup follows the user across
workspace switches.  No-op when the buffer doesn't exist (nothing
to show yet).

Display priority:

  1. If a window already displays the buffer, leave it in place
     (and re-apply the side-window width unless it is the stolen
     agent output window — stolen windows inherit the prior
     window's width).
  2. Otherwise, if the agent output window is visible, take it
     over via `set-window-buffer' and record the prior buffer so
     `--hide' can restore it.
  3. Otherwise, fall back to the right-side popup display action.

The drawer is never touched in any branch — its visibility is its
own concern.  Returns the displayed window or nil."
  (when-let ((buf (get-buffer agent-repl-explain-config-buffer-name)))
    (setq agent-repl--explain-config-global-visible-p t)
    (let ((existing (get-buffer-window buf t)))
      (cond
       ((window-live-p existing)
        (unless (and agent-repl--explain-config-replaced-window
                     (eq existing (car agent-repl--explain-config-replaced-window)))
          (agent-repl--explain-config-apply-width existing))
        existing)
       ((agent-repl--explain-config-current-agent-output-window)
        (agent-repl--explain-config-take-over-agent-output-window
         (agent-repl--explain-config-current-agent-output-window) buf))
       (t
        (let ((win (display-buffer buf agent-repl--explain-config-display-action)))
          (when (window-live-p win)
            (agent-repl--explain-config-apply-width win))
          win))))))

(defun agent-repl--explain-config-hide ()
  "Hide the explain-config buffer.
Clears the global visible-flag so the popup no longer auto-appears
on workspace switches.  Keeps the buffer itself alive — only its
visibility is toggled.

If `--show' took over the agent output window, restores the prior
buffer in that window via `--restore-replaced-window'.  Any
remaining windows still displaying the explain-config buffer (e.g.
side-window fallbacks) are deleted.  The drawer is never touched."
  (setq agent-repl--explain-config-global-visible-p nil)
  (agent-repl--explain-config-restore-replaced-window)
  (when-let ((buf (get-buffer agent-repl-explain-config-buffer-name)))
    (agent-repl-window--delete-buffer-windows buf)))

;;;###autoload
(defun agent-repl-explain-config-close ()
  "Close the explain-config popup everywhere.
Deletes every visible explain-config window in the current frame and
clears the global visible-flag so the popup will not reappear on
workspace switch.  Buffer contents are preserved — re-running
`agent-repl-explain-config' (or reopening via leader) will show them
again with the new question's output appended."
  (interactive)
  (agent-repl--explain-config-hide))

(defvar agent-repl-explain-config-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'agent-repl-explain-config-close)
    map)
  "Keymap for `agent-repl-explain-config-mode'.")

(defconst agent-repl--explain-config-insert-entry-keys
  '("i" "I" "a" "A" "o" "O" "s" "S" "c" "C" "R")
  "Evil normal/motion-state keys that would normally enter insert state.
Bound to `ignore' in the explain-config popup so the buffer can never
flip into evil insert state -- the popup is strictly read-only Q&A
output and there is nothing meaningful the user could type into it.")

(defun agent-repl--explain-config-install-evil-bindings ()
  "Install evil-mode bindings on `agent-repl-explain-config-mode-map'.
Binds `q' in normal/motion state to the close command so the popup
dismisses uniformly whether evil is enabled or not (the
`define-key' on `q' alone would be shadowed by
`evil-normal-state-map' otherwise).  Also blocks every insert-entry
key so the buffer never flips into evil insert state -- the popup is
read-only and there is nothing meaningful to type into it.

No-op when evil is not loaded."
  (when (fboundp 'evil-define-key)
    (evil-define-key '(normal motion) agent-repl-explain-config-mode-map
      "q" #'agent-repl-explain-config-close)
    (dolist (key agent-repl--explain-config-insert-entry-keys)
      (evil-define-key '(normal motion) agent-repl-explain-config-mode-map
        key #'ignore))))

(agent-repl--explain-config-install-evil-bindings)

(define-minor-mode agent-repl-explain-config-mode
  "Minor mode enabled in the explain-config output popup.
Provides a buffer-local `q' binding to dismiss the popup globally via
`agent-repl-explain-config-close', so the user does not need to
navigate-and-`C-x 0' the window in every workspace separately.

When evil is loaded, forces the buffer into motion state on enable so
`q' (bound via `agent-repl--explain-config-install-evil-bindings')
fires immediately without the user first hitting ESC, and so the
buffer never starts in normal state where insert-entry keys could
trigger before the ignore-bindings take effect."
  :lighter " ExplainCfg"
  :keymap agent-repl-explain-config-mode-map
  (when (and agent-repl-explain-config-mode
             (fboundp 'evil-motion-state))
    (evil-motion-state)))

(defun agent-repl--explain-config-ensure-visible-on-persp-switch (&rest _)
  "Reconcile explain-config visibility with the global state on workspace switch.
Mirrors `agent-repl-drawer--ensure-visible-on-persp-switch' — when
the flag says show but the popup is missing in the activated persp,
re-display it via `agent-repl--explain-config-show' (which will
take over the new persp's agent output window if visible, else
fall back to the side-window display action).  When the flag says
hide but persp-mode restored a stale window, delete it.

Drops a stale `--replaced-window' whose target window is no longer
live before re-showing — the cell belongs to the persp we left, not
the one we just activated."
  (let* ((buf (get-buffer agent-repl-explain-config-buffer-name))
         (win (and buf (get-buffer-window buf))))
    (cond
     ((and agent-repl--explain-config-global-visible-p buf (not win))
      (when (and agent-repl--explain-config-replaced-window
                 (not (window-live-p
                       (car agent-repl--explain-config-replaced-window))))
        (setq agent-repl--explain-config-replaced-window nil))
      (agent-repl--explain-config-show))
     ((and (not agent-repl--explain-config-global-visible-p) win)
      (agent-repl-window--delete-buffer-windows buf)))))

(agent-repl--ws-add-activated-hook
 #'agent-repl--explain-config-ensure-visible-on-persp-switch)

(defcustom agent-repl-explain-config-preamble
  (concat
   "You are being asked a question about the Doom Emacs configuration"
   " in this repository (~/.config/doom), with particular emphasis on"
   " `modules/app/agent-repl/' (the Agent REPL integration).  The"
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
   "  ⟦kbd⟧SPC j h c⟦/kbd⟧   ⟦file⟧modules/app/agent-repl/commands.el⟦/file⟧\n"
   "  ⟦sym⟧agent-repl-explain-config⟦/sym⟧\n"
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
  :group 'agent-repl)

(defun agent-repl--explain-config-build-input (raw)
  "Wrap RAW with the explain-config preamble for sending to claude."
  (format agent-repl-explain-config-preamble raw))

;;; Rich-text faces for the model's tagged response body.  The model
;;; emits semantic face tags (see `agent-repl-explain-config-preamble');
;;; the streaming filter (`agent-repl--explain-config-filter') parses
;;; them and applies these faces to the inserted text so the buffer
;;; renders with Emacs's own font system instead of raw Markdown.

(defface agent-repl-explain-config-h1
  '((t :inherit org-level-1 :weight bold :height 1.25))
  "Face for ⟦h1⟧ headings in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-h2
  '((t :inherit org-level-2 :weight bold :height 1.15))
  "Face for ⟦h2⟧ headings in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-h3
  '((t :inherit org-level-3 :weight bold))
  "Face for ⟦h3⟧ headings in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-bold
  '((t :inherit bold))
  "Face for ⟦b⟧ bold spans in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-italic
  '((t :inherit italic))
  "Face for ⟦i⟧ italic spans in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-code
  '((t :inherit font-lock-constant-face :background "#2A2A2A"))
  "Face for ⟦code⟧ inline-code spans in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-block
  '((t :inherit org-block :extend t))
  "Face for ⟦block⟧ multi-line code blocks in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-quote
  '((t :inherit org-quote :slant italic))
  "Face for ⟦quote⟧ blockquotes in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-link
  '((t :inherit link))
  "Face for ⟦link⟧ URLs in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-bullet
  '((t :inherit org-list-dt :weight bold))
  "Face for ⟦bullet⟧ list-item labels in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-kbd
  '((t :inherit help-key-binding))
  "Face for ⟦kbd⟧ keybinding spans in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-file
  '((t :inherit font-lock-string-face :underline t))
  "Face for ⟦file⟧ filepath spans in explain-config output."
  :group 'agent-repl)

(defface agent-repl-explain-config-sym
  '((t :inherit font-lock-function-name-face))
  "Face for ⟦sym⟧ identifier spans in explain-config output."
  :group 'agent-repl)

(defconst agent-repl--explain-config-face-map
  '(("h1"     . agent-repl-explain-config-h1)
    ("h2"     . agent-repl-explain-config-h2)
    ("h3"     . agent-repl-explain-config-h3)
    ("b"      . agent-repl-explain-config-bold)
    ("i"      . agent-repl-explain-config-italic)
    ("code"   . agent-repl-explain-config-code)
    ("block"  . agent-repl-explain-config-block)
    ("quote"  . agent-repl-explain-config-quote)
    ("link"   . agent-repl-explain-config-link)
    ("bullet" . agent-repl-explain-config-bullet)
    ("kbd"    . agent-repl-explain-config-kbd)
    ("file"   . agent-repl-explain-config-file)
    ("sym"    . agent-repl-explain-config-sym))
  "Mapping of face-tag names (as emitted by the model) to Emacs faces.
Unknown tag names render verbatim and apply no face.")

(defconst agent-repl--explain-config-tag-re
  "⟦\\(/?\\)\\([a-z][a-z0-9]*\\)\\(\\(?: [^⟧]*\\)?\\)⟧"
  "Regex matching one open (`⟦NAME[ ATTRS]⟧') or close (`⟦/NAME⟧') face tag.
Capture groups: (1) slash-or-empty, (2) tag name, (3) optional attrs
payload (leading space included; empty string when no attrs).  ATTRS
is only meaningful for the `face' escape-hatch tag; semantic tags
ignore it.")

(defconst agent-repl--explain-config-partial-tag-re
  "⟦/?[a-z0-9]*\\(?: [^⟧]*\\)?\\'"
  "Regex matching a possibly-incomplete tag at the very end of a string.
Used to defer tag fragments across streaming chunk boundaries.  Now
also buffers `⟦face PLIST' fragments where the plist arrives split
across chunks before the closing `⟧'.")

(defconst agent-repl--explain-config-raw-face-tag-name "face"
  "Magic tag name for the raw-face escape hatch (`⟦face PLIST⟧').
Distinct from any semantic tag in `agent-repl--explain-config-face-map'
so the parser can branch on it.")

(defconst agent-repl--explain-config-face-attr-whitelist
  '(:foreground :background :weight :slant :underline :overline
    :strike-through :box :height)
  "Permitted plist attributes inside a raw-face ⟦face PLIST⟧ tag.
Any plist containing an attribute outside this list is rejected and
the tag renders verbatim — the preamble documents this whitelist to
the model.")

(defun agent-repl--explain-config-parse-face-attrs (attrs)
  "Parse ATTRS (the raw payload from `⟦face PLIST⟧') into a face plist.
Returns the plist on success, or nil if parsing fails or the plist
contains a non-whitelisted attribute.  ATTRS is the captured group 3
from `agent-repl--explain-config-tag-re' — it includes the leading
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
                       (agent-repl--explain-config-plist-keys-valid-p plist))
              plist))
        (error nil)))))

(defun agent-repl--explain-config-plist-keys-valid-p (plist)
  "Return non-nil if every key in PLIST is in the face-attr whitelist."
  (let ((ok t)
        (rest plist))
    (while (and ok rest)
      (unless (memq (car rest) agent-repl--explain-config-face-attr-whitelist)
        (setq ok nil))
      (setq rest (cddr rest)))
    ok))

(defvar-local agent-repl--explain-config-pending ""
  "Accumulated stream bytes not yet flushed to the buffer.
Holds back partial face tags until their closing `⟧' arrives.")

(defvar-local agent-repl--explain-config-face-stack nil
  "Stack of active face symbols (innermost first) for the rendering filter.
Pushed on each open tag, popped on each matching close tag.")

(defconst agent-repl--explain-config-orange "#FF8C42"
  "Claude-orange accent used in the explain-config buffer chrome.")

(defconst agent-repl--explain-config-blue "#7FBFFF"
  "Question-label accent in the explain-config buffer chrome.")

(defconst agent-repl--explain-config-green "#8DE08D"
  "Response/success accent in the explain-config buffer chrome.")

(defconst agent-repl--explain-config-red "#FF7F7F"
  "Failure accent for non-zero exit statuses in the explain-config footer.")

(defconst agent-repl--explain-config-muted "#888888"
  "Muted accent for subtitles and rules in the explain-config chrome.")

(defun agent-repl--explain-config-format-header (prompt)
  "Return the propertized banner inserted at the top of the buffer.
PROMPT is the user's question.  The literal substring \"Question: PROMPT\"
is preserved so downstream tooling can scrape it."
  (let* ((rule (propertize (make-string 72 ?━)
                           'face `(:foreground ,agent-repl--explain-config-orange)))
         (title (propertize "🤖 Claude · Doom Config Q&A"
                            'face `(:foreground ,agent-repl--explain-config-orange
                                    :weight bold)))
         (badge (propertize "🔒 read-only"
                            'face `(:foreground ,agent-repl--explain-config-muted
                                    :slant italic)))
         (q-label (propertize "❓ Question: "
                              'face `(:foreground ,agent-repl--explain-config-blue
                                      :weight bold)))
         (q-body (propertize prompt 'face '(:slant italic)))
         (r-label (propertize "📜 Response"
                              'face `(:foreground ,agent-repl--explain-config-green
                                      :weight bold)))
         (r-tag (propertize " (streaming…)"
                            'face `(:foreground ,agent-repl--explain-config-muted
                                    :slant italic)))
         (r-rule (propertize (concat " " (make-string 50 ?─))
                             'face `(:foreground ,agent-repl--explain-config-green))))
    (concat rule "\n"
            "  " title "   " badge "\n"
            rule "\n\n"
            q-label q-body "\n\n"
            r-label r-tag r-rule "\n\n")))

(defun agent-repl--explain-config-format-footer (status)
  "Return the propertized footer for an explain-config run ending with STATUS."
  (let* ((success (zerop status))
         (emoji (if success "✅" "❌"))
         (color (if success
                    agent-repl--explain-config-green
                  agent-repl--explain-config-red))
         (verb (if success "exited cleanly" "exited with errors"))
         (rule (propertize (make-string 50 ?─) 'face `(:foreground ,color)))
         (body (propertize (format "%s claude %s (status %d)" emoji verb status)
                           'face `(:foreground ,color :weight bold))))
    (concat "\n\n" rule "\n" body "\n")))

(defun agent-repl--explain-config-sentinel (proc _event)
  "Process sentinel for `agent-repl-explain-config'.
Appends an exit-status footer to PROC's buffer when the process exits."
  (when (memq (process-status proc) '(exit signal))
    (let ((status (process-exit-status proc))
          (buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (agent-repl--explain-config-format-footer status))))))))

(defun agent-repl--explain-config-init-buffer (prompt)
  "Prepare the explain-config output buffer for a fresh run.
Erases prior contents, inserts the question header, returns the buffer.
Also resets the streaming filter's per-buffer parser state."
  (let ((buf (get-buffer-create agent-repl-explain-config-buffer-name)))
    (with-current-buffer buf
      (setq agent-repl--explain-config-pending ""
            agent-repl--explain-config-face-stack nil)
      (agent-repl-explain-config-mode 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (agent-repl--explain-config-format-header prompt))))
    buf))

(defun agent-repl--explain-config-current-face (stack)
  "Return the face property value for STACK (innermost first).
nil means no face; a single symbol means a single face; a list means
multiple faces merged in the standard Emacs left-overrides-right order."
  (cond
   ((null stack)        nil)
   ((null (cdr stack))  (car stack))
   (t                   stack)))

(defun agent-repl--explain-config-insert-styled (buf text stack)
  "Insert TEXT into BUF at point-max, propertized with the face for STACK.
No-op if TEXT is empty.  Buffer is treated as read-only-aware."
  (when (and (stringp text) (> (length text) 0))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (face (agent-repl--explain-config-current-face stack)))
        (goto-char (point-max))
        (if face
            (insert (propertize text 'face face))
          (insert text))))))

(defun agent-repl--explain-config-parse-chunk (buf chunk)
  "Parse CHUNK against the face-markup grammar and flush styled text into BUF.
Maintains BUF-local pending bytes and face stack across calls so partial
tags arriving in different process chunks are joined correctly.  Returns
the new pending string (also stored buffer-locally)."
  (with-current-buffer buf
    (let* ((input (concat agent-repl--explain-config-pending (or chunk "")))
           (pos 0)
           (stack agent-repl--explain-config-face-stack))
      (while (string-match agent-repl--explain-config-tag-re input pos)
        (let* ((m-start (match-beginning 0))
               (m-end   (match-end 0))
               (is-close (string= (match-string 1 input) "/"))
               (name    (match-string 2 input))
               (attrs   (match-string 3 input))
               (raw-face-p (string= name agent-repl--explain-config-raw-face-tag-name))
               (face    (cdr (assoc name agent-repl--explain-config-face-map))))
          (agent-repl--explain-config-insert-styled
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
                (agent-repl--explain-config-insert-styled
                 buf (substring input m-start m-end) stack)))
             ;; Open tag — parse the plist, push if valid; else verbatim.
             ;; On verbatim we deliberately do NOT push a sentinel, so a
             ;; matching `⟦/face⟧' also renders verbatim — keeps open
             ;; and close visible together when the model malformed the
             ;; plist.
             (t (let ((plist (agent-repl--explain-config-parse-face-attrs attrs)))
                  (if plist
                      (setq stack (cons plist stack))
                    (agent-repl--explain-config-insert-styled
                     buf (substring input m-start m-end) stack))))))
           ;; Unknown tag name -- emit verbatim, leave stack untouched.
           ((null face)
            (agent-repl--explain-config-insert-styled
             buf (substring input m-start m-end) stack))
           ;; Close tag -- pop only when it matches innermost open.
           (is-close
            (when (eq (car stack) face)
              (setq stack (cdr stack))))
           ;; Open tag -- push onto stack.  Reject stray attrs on
           ;; semantic tags (the preamble says attrs are only for
           ;; `⟦face …⟧') by emitting the open tag verbatim.
           ((and attrs (not (string-empty-p attrs)))
            (agent-repl--explain-config-insert-styled
             buf (substring input m-start m-end) stack))
           (t (setq stack (cons face stack))))
          (setq pos m-end)))
      (let ((tail (substring input pos)))
        (if (string-match agent-repl--explain-config-partial-tag-re tail)
            (let ((open (match-beginning 0)))
              (agent-repl--explain-config-insert-styled
               buf (substring tail 0 open) stack)
              (setq agent-repl--explain-config-pending (substring tail open)))
          (agent-repl--explain-config-insert-styled buf tail stack)
          (setq agent-repl--explain-config-pending "")))
      (setq agent-repl--explain-config-face-stack stack)
      agent-repl--explain-config-pending)))

(defun agent-repl--explain-config-filter (proc chunk)
  "Process filter for `agent-repl-explain-config'.
Routes CHUNK from PROC through the face-markup parser so the model's
tagged output renders with Emacs faces instead of raw Markdown.

Triggers `agent-repl--explain-config-show' on every chunk — `--show'
is idempotent w.r.t. the global flag, so the user-visible effect is
\"popup appears on the first chunk\"; the drawer-state capture (and
drawer-hide) happens exactly once on the hidden→visible transition."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (agent-repl--explain-config-parse-chunk buf chunk)
      (agent-repl--explain-config-show))))

(defun agent-repl--explain-config-spawn (prompt)
  "Spawn the headless claude process for explain-config PROMPT.
Returns the process.  Separated from the interactive entry point so
tests can stub `make-process' here without going through the input
read.

The popup is NOT displayed here — it is deferred to
`agent-repl--explain-config-filter', which calls `--show' on the
first streamed chunk.  This delays the visible side effect (and the
drawer-hide) until there is actual content to render, so a stalled
or never-responding `claude' invocation doesn't disturb the UI."
  (let* ((dir (file-name-as-directory
               (expand-file-name agent-repl-explain-config-dir)))
         (buf (agent-repl--explain-config-init-buffer prompt))
         (cmd (agent-repl--backend-headless-cmd
               (agent-repl--default-backend)
               agent-repl-explain-config-model
               agent-repl-explain-config-extra-args))
         (input (agent-repl--explain-config-build-input prompt)))
    (let* ((default-directory dir)
           (proc (make-process
                  :name "agent-explain-config"
                  :buffer buf
                  :command cmd
                  :connection-type 'pipe
                  :noquery t
                  :filter #'agent-repl--explain-config-filter
                  :sentinel #'agent-repl--explain-config-sentinel)))
      (process-send-string proc input)
      (process-send-eof proc)
      proc)))

(defun agent-repl-explain-config (prompt)
  "Ask a headless claude to explain something about this doom config.
Prompts for PROMPT, then spawns `claude -p
--dangerously-skip-permissions' in `agent-repl-explain-config-dir'
(`~/.config/doom' by default).  The prompt is wrapped in a
read-only preamble forbidding any mutating action -- this entry
point is for clarification and explanation only.  Output streams to
`agent-repl-explain-config-buffer-name'."
  (interactive
   (list (read-string (propertize "🤖 Explain config: "
                                  'face `(:foreground ,agent-repl--explain-config-orange
                                          :weight bold)))))
  (let ((trimmed (string-trim (or prompt ""))))
    (when (string-empty-p trimmed)
      (user-error "Empty prompt"))
    (agent-repl--log (agent-repl--ws-current-name)
                      "explain-config: dir=%s len=%d"
                      agent-repl-explain-config-dir (length trimmed))
    (agent-repl--explain-config-spawn trimmed)))

(defun agent-repl--send-interrupt-escape (ws vterm-buf)
  "Send two Escape key presses to VTERM-BUF to interrupt Claude.
WS is the current workspace name for logging."
  (agent-repl--log ws "send-interrupt-escape: sending %dx <escape> to vterm=%s" agent-repl-interrupt-escape-count (buffer-name vterm-buf))
  (with-current-buffer vterm-buf
    (dotimes (_ agent-repl-interrupt-escape-count)
      (vterm-send-key "<escape>"))))

(defun agent-repl--enter-insert-mode (ws)
  "Re-enter evil insert state in WS's input buffer after an interrupt.
Switches the Emacs-side input buffer back to evil insert state so the
user can keep typing where they left off.

Does NOT send a literal \"i\" keystroke to the Claude vterm.  The input
buffer — not the vterm — is the surface the user types into, so
forwarding \"i\" to the terminal both double-dispatches the mode switch
\(evil already owns insert mode) and leaks a stray \"i\" character onto
Claude's prompt line, which then prefixes the next message the user
sends.

No-op when WS is not the current workspace (a drawer-triggered
interrupt on a background workspace must not steal focus or flip a
hidden buffer's state) or when the input buffer is dead."
  (if (not (equal ws (agent-repl--ws-current-name)))
      (agent-repl--log ws "enter-insert-mode: ws not current, skipping")
    (let ((input-buf (agent-repl--ws-get ws :input-buffer)))
      (if (buffer-live-p input-buf)
          (let ((win (get-buffer-window input-buf)))
            (agent-repl--log ws "enter-insert-mode: evil insert state in input buffer=%s win=%s" (buffer-name input-buf) win)
            (when win (select-window win))
            (with-current-buffer input-buf
              (evil-insert-state)))
        (agent-repl--log ws "enter-insert-mode: input buffer is dead, skipping")))))

(defun agent-repl-interrupt (&optional ws)
  "Interrupt Claude in workspace WS and re-enter insert mode after a delay.
Sends Escape to stop the current operation, then automatically returns
the input buffer to evil insert state after
`agent-repl-interrupt-reinsert-delay' seconds (via
`agent-repl--enter-insert-mode', which switches evil state rather than
forwarding a literal \"i\" keystroke to the vterm).  Defaults to the
current workspace when WS is nil (matches the interactive `SPC o x'
behavior); the drawer passes the entry-at-point so interrupts target
the selected entry.

After issuing the escape, marks the workspace's agent-state as
`:done' and clears the Stop / SubagentStop tracking — interrupting
terminates the in-flight turn, so the tab should immediately reflect
\"finished\" rather than linger on `:thinking' until a stray hook
arrives.  No Stop hook will fire for the interrupted turn, so Emacs
is the sole observer here."
  (interactive)
  (let* ((ws (or ws (agent-repl--ws-current-name)))
         (vterm-buf (agent-repl--ws-get ws :vterm-buffer)))
    (agent-repl--log ws "interrupt")
    (if (and vterm-buf (buffer-live-p vterm-buf))
        (progn
          (agent-repl--send-interrupt-escape ws vterm-buf)
          (agent-repl--ws-clear-stop-tracking ws)
          (agent-repl--mark-agent-done ws)
          (run-at-time agent-repl-interrupt-reinsert-delay nil
                       #'agent-repl--enter-insert-mode ws))
      (agent-repl--log ws "interrupt: vterm not live, skipping"))))

(defun agent-repl--agent-process-pid (ws)
  "Return the PID of the `claude' process running in WS's vterm, or nil.
`claude' runs as the child of the workspace vterm's shell.  It is
identified STRUCTURALLY as that shell's child rather than by name,
because the native `claude' binary reports its version (e.g. \"2.1.206\",
from `~/.local/share/claude/versions/<v>') as its process `comm' — not
\"claude\".  Among the shell's children a agent-ish `args'/`comm' match
is preferred (in case the shell ever has more than one child), otherwise
the sole child is taken.  A pure query over `list-system-processes' /
`process-attributes' with no side effects; returns nil when the vterm is
dead or the shell has no children."
  (let* ((buf (agent-repl--ws-get ws :vterm-buffer))
         (proc (and (buffer-live-p buf) (get-buffer-process buf)))
         (shell-pid (and proc (process-id proc))))
    (when shell-pid
      (let ((children (seq-filter
                       (lambda (pid)
                         (eq (alist-get 'ppid (process-attributes pid)) shell-pid))
                       (list-system-processes))))
        (or (seq-find
             (lambda (pid)
               (let ((attrs (process-attributes pid)))
                 (or (string-match-p "claude" (or (alist-get 'args attrs) ""))
                     (string-match-p "claude" (or (alist-get 'comm attrs) "")))))
             children)
            (and (= (length children) 1) (car children)))))))

(defun agent-repl-kill-agent-process (&optional ws)
  "Kill ONLY the `claude' process in WS's vterm, leaving panels and buffers intact.
Unlike `agent-repl-kill' (which tears down the session's windows and
buffers via `agent-repl--kill-session'), this sends SIGTERM to the
`claude' CLI child of the workspace vterm's shell.  The vterm, input
buffer, drawer entry, and perspective all survive, so the process can be
restarted manually (e.g. `agent-repl-restart' or `SPC o c') without
disturbing the layout — useful for debugging or working around a wedged
session.  Defaults to the current workspace; signals a `user-error' when
no active workspace or no live `claude' process is found."
  (interactive)
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (unless ws (user-error "agent-repl-kill-agent-process: no active workspace"))
    (let ((pid (agent-repl--agent-process-pid ws)))
      (if (not pid)
          (progn
            (agent-repl--log ws "kill-agent-process: no claude child process for ws=%s" ws)
            (user-error "agent-repl: no live claude process found for %s" ws))
        (agent-repl--log ws "kill-agent-process: SIGTERM pid=%s ws=%s" pid ws)
        (agent-repl--signal-process pid 'TERM)
        (message "agent-repl: killed claude (pid %s) in %s — panels left intact" pid ws)))))

(defun agent-repl-update-pr ()
  "Ask Claude to update the PR description for the current branch."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "update-pr: sending update-pr prompt")
  (agent-repl--send-to-agent agent-repl-update-pr-prompt))

(defun agent-repl--rebase-onto-origin-master-callback (ws ok output)
  "Process the `git fetch origin' result and ask Claude to rebase.
WS is the workspace name.  OK and OUTPUT come from the async-git
sentinel.  On success, dispatches `agent-repl-rebase-onto-origin-master-prompt'
to Claude so the agent runs the rebase itself.  On failure, surfaces
the git error via `agent-repl--warn' and skips the agent dispatch — the
rebase would proceed against stale `origin/master' otherwise."
  (agent-repl--log ws "rebase-onto-origin-master: fetch ok=%s output=%s" ok output)
  (if ok
      (progn
        (agent-repl--info ws "[%s] git fetch origin complete; asking Claude to rebase onto origin/master" ws)
        (agent-repl--send-to-agent agent-repl-rebase-onto-origin-master-prompt))
    (agent-repl--warn ws "[%s] git fetch origin failed: %s" ws output)))

(defun agent-repl-rebase-onto-origin-master ()
  "Fetch origin asynchronously, then ask Claude to rebase onto origin/master.
Runs `git fetch origin' in the current workspace's project directory.
When it succeeds, sends `agent-repl-rebase-onto-origin-master-prompt'
to Claude so the agent performs the rebase itself (and resolves any
conflicts).  On fetch failure, skips the dispatch and surfaces the git
error via `agent-repl--warn'."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (project-dir (agent-repl--ws-dir ws)))
    (agent-repl--log ws "rebase-onto-origin-master: fetching origin in %s" project-dir)
    (agent-repl--info ws "[%s] git fetch origin..." ws)
    (agent-repl--async-git
     "rebase-fetch" project-dir '("fetch" "origin")
     (lambda (ok output)
       (agent-repl--rebase-onto-origin-master-callback ws ok output)))))

(defun agent-repl--exclusion-symbol-to-flag (sym)
  "Convert exclusion SYM (e.g. \\='no-self-certified) to flag (e.g. \"--self-certified\")."
  (let ((name (symbol-name sym)))
    (unless (string-prefix-p "no-" name)
      (error "agent-repl: exclusion symbol must start with `no-': %S" sym))
    (concat "--" (substring name 3))))

(defun agent-repl--build-create-or-update-pr-prompt (excluded)
  "Build the /create-or-update-pr prompt, omitting flags for EXCLUDED.
EXCLUDED is a list of `no-FLAG' symbols (e.g. \\='no-self-certified).  Each
must correspond to a flag in `agent-repl-create-or-update-pr-base-flags'
or an error is signalled."
  (let ((excluded-flags
         (mapcar (lambda (sym)
                   (let ((flag (agent-repl--exclusion-symbol-to-flag sym)))
                     (unless (member flag agent-repl-create-or-update-pr-base-flags)
                       (error "agent-repl: %S excludes %s, not in base flags" sym flag))
                     flag))
                 excluded)))
    (string-join
     (cons "/create-or-update-pr"
           (cl-remove-if (lambda (f) (member f excluded-flags))
                         agent-repl-create-or-update-pr-base-flags))
     " ")))

(defun agent-repl-create-or-update-pr (&optional excluded)
  "Send /create-or-update-pr to Claude with optional EXCLUDED flags dropped.
The current input buffer contents (right-trimmed, if non-empty) are
prepended as a prefix separated by a single space; the input buffer is
then cleared and its prior contents pushed to history.
EXCLUDED is a list of `no-FLAG' symbols (e.g. \\='(no-self-certified)) — each
named flag is removed from `agent-repl-create-or-update-pr-base-flags'
before the prompt is sent."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (base (agent-repl--build-create-or-update-pr-prompt excluded))
         (input-buf (agent-repl--ws-get ws :input-buffer))
         (raw-prefix (agent-repl--read-input-buffer ws))
         (prefix (and raw-prefix (string-trim-right raw-prefix)))
         (has-prefix (and prefix (not (string-empty-p prefix))))
         (prompt (if has-prefix (concat prefix " " base) base)))
    (agent-repl--log ws "create-or-update-pr: prefix-len=%d prompt=%s"
                      (length (or prefix "")) prompt)
    (agent-repl--send-to-agent prompt)
    (when (and has-prefix input-buf (buffer-live-p input-buf))
      (agent-repl--commit-input-buffer ws input-buf raw-prefix t))))

(defun agent-repl-create-or-update-pr-no-self-certified ()
  "Send /create-or-update-pr to Claude without --self-certified."
  (interactive)
  (agent-repl-create-or-update-pr '(no-self-certified)))

(defun agent-repl-create-or-update-pr-paste (&optional excluded)
  "Insert the /create-or-update-pr prompt at point instead of sending.
EXCLUDED has the same semantics as in `agent-repl-create-or-update-pr'.
The inserted prompt is wrapped in single backticks for inline-code
rendering in markdown contexts.  No workspace state is touched — the
input buffer is left intact and Claude is not contacted."
  (interactive)
  (let ((prompt (agent-repl--build-create-or-update-pr-prompt excluded)))
    (agent-repl--log (agent-repl--ws-current-name)
                      "create-or-update-pr-paste: prompt=%s" prompt)
    (insert "`" prompt "`")))

(defun agent-repl-create-or-update-pr-no-self-certified-paste ()
  "Insert the /create-or-update-pr prompt (no --self-certified) at point."
  (interactive)
  (agent-repl-create-or-update-pr-paste '(no-self-certified)))

;; `agent-repl--nuke-one-workspace' moved into `workspace.el' during
;; the persp-mode integration extraction (see AGENTS.md, "NEVER
;; manipulate third-party internals from a high-level layer").  It is
;; the canonical `+workspace/kill' call site and therefore belongs at
;; the integration boundary, not at the orchestration layer.

(defun agent-repl--nuke-or-kill-workspace (ws)
  "Dispatch a nuke vs. plain persp-kill on WS based on liveness.

When WS is a live agent-repl workspace
\(`agent-repl--ws-live-p'), runs the full
`agent-repl--nuke-one-workspace' teardown and returns the symbol
`nuke'.  Otherwise WS is a tab-bar-only workspace (either a
tombstoned agent-repl entry whose persp still exists or a persp
that was never registered with agent-repl); in that case runs a
bare `+workspace/kill' guarded by `+workspace-exists-p' and returns
the symbol `kill'.

Shared by the interactive `agent-repl-nuke-workspace' and
`agent-repl-kill-workspace' commands so the picker (which
deliberately offers both kinds of candidates via
`agent-repl--nukeable-workspace-names') can hand the chosen WS to a
single routing point."
  (agent-repl--log ws "nuke-or-kill-workspace: ENTRY ws=%s live=%s"
                    ws (if (agent-repl--ws-live-p ws) "t" "nil"))
  (cond
   ((agent-repl--ws-live-p ws)
    (agent-repl--nuke-one-workspace ws)
    'nuke)
   (t
    (agent-repl--log ws "nuke-or-kill: ws not live, routing to +workspace/kill")
    (when (and (agent-repl--ws-system-available-p)
               (agent-repl--ws-exists-p ws))
      (condition-case err
          (agent-repl--ws-kill ws)
        (error (agent-repl--log ws "nuke-or-kill: +workspace/kill error: %S" err))))
    'kill)))

(defun agent-repl-nuke-workspace (&optional ws)
  "Tear down a agent-repl workspace: session, buffers, persp, and hashmap entry.
Persisted state.el (priority, per-environment session-id) is preserved
so the workspace can be re-opened later and resume its Claude session.
When called interactively without WS, prompts to select from the union
of live agent-repl workspaces and tab-bar workspaces
\(`agent-repl--nukeable-workspace-names'), defaulting to the current
workspace when it appears in that candidate list.  Programmatic
callers (e.g. the drawer) pass WS directly to skip the prompt.

If the selected workspace is NOT a live agent-repl workspace (its
claude has already been killed but the persp/doom workspace is still
in the tab-bar), the operation falls back to a plain `+workspace/kill'
— there is no agent-repl session to tear down, so a normal persp kill
is the correct operation.

No confirmation prompt: teardown is immediate.  Persisted state.el is
preserved, so re-opening the workspace later resumes the Claude
session — accidental invocations are easily recoverable."
  (interactive)
  (let* ((ws (or ws (agent-repl--read-nukeable-workspace "Nuke workspace: ")))
         (t0 (float-time)))
    (agent-repl--log ws "nuke-workspace: ENTRY ws=%s" ws)
    (let ((action (agent-repl--nuke-or-kill-workspace ws)))
      (agent-repl--log ws
                        "nuke-workspace: nuke-or-kill-workspace returned action=%s elapsed=%.3fs — about to force-mode-line-update"
                        action (- (float-time) t0))
      (force-mode-line-update t)
      (agent-repl--log ws
                        "nuke-workspace: force-mode-line-update done elapsed=%.3fs — about to message"
                        (- (float-time) t0))
      (message (if (eq action 'nuke)
                   "Nuked workspace: %s"
                 "Killed persp workspace: %s")
               ws)
      (agent-repl--log ws "nuke-workspace: COMPLETE ws=%s action=%s total-elapsed=%.3fs"
                        ws action (- (float-time) t0)))))

(defun agent-repl-nuke-all-workspaces ()
  "Tear down ALL agent-repl workspaces.
Iterates every workspace registered in `agent-repl--workspaces' and
applies the same teardown as `agent-repl-nuke-workspace' to each.
Persisted state.el for each project is preserved.
Prompts once with the count before proceeding."
  (interactive)
  (let* ((known (agent-repl--live-ws-names))
         (count (length known)))
    (unless known (user-error "No agent-repl workspaces registered"))
    (unless (y-or-n-p (format "Nuke ALL %d agent-repl workspace(s)? This kills processes and buffers but preserves on-disk state. "
                              count))
      (user-error "Aborted"))
    (agent-repl--log (agent-repl--ws-current-name) "nuke-all-workspaces: count=%d" count)
    ;; Snapshot keys before iterating; each call mutates the hash.
    (dolist (ws known)
      (agent-repl--nuke-one-workspace ws))
    (force-mode-line-update t)
    (message "Nuked %d workspace(s)" count)))

(defun agent-repl-nuke-restored-workspaces ()
  "Tear down every workspace that was restored this session.
Tears down only the workspaces tracked in
`agent-repl--restored-workspaces' (those established by
`agent-repl-load-workspace-snapshot', including the
from-archive entry point); workspaces the user created manually
before or after the restore are left alone.  Persisted state.el for
each project is preserved.  Prompts once with the count before
proceeding.  Same per-workspace teardown as
`agent-repl-nuke-workspace'."
  (interactive)
  (let* ((restored (cl-remove-if-not
                    (lambda (ws) (agent-repl--ws-get ws :project-dir))
                    agent-repl--restored-workspaces))
         (count (length restored)))
    (unless restored
      (user-error "No restored agent-repl workspaces to nuke"))
    (unless (y-or-n-p (format "Nuke %d restored agent-repl workspace(s)? This kills processes and buffers but preserves on-disk state. "
                              count))
      (user-error "Aborted"))
    (agent-repl--log (agent-repl--ws-current-name)
                      "nuke-restored-workspaces: count=%d" count)
    (dolist (ws restored)
      (agent-repl--nuke-one-workspace ws))
    (force-mode-line-update t)
    (message "Nuked %d restored workspace(s)" count)))

(defun agent-repl-kill-workspace (&optional ws)
  "Tear down a agent-repl workspace and preserve its persisted state.
Alias for `agent-repl-nuke-workspace' — both functions go through
`agent-repl--nuke-or-kill-workspace', which preserves the on-disk
per-project state file on the live-agent-repl path and falls back to
a plain `+workspace/kill' for tab-bar workspaces whose claude has
already been killed.  Retained as a separate command for callers /
muscle-memory that bind `kill' semantics distinctly from `nuke'.

Prompts to select from the union of live agent-repl workspaces and
tab-bar workspaces (`agent-repl--nukeable-workspace-names'),
defaulting to the current workspace when it appears in that candidate
list.  Programmatic callers (e.g. the drawer) pass WS directly to
skip the prompt.

No confirmation prompt: teardown is immediate.  Persisted state.el is
preserved, so re-opening the workspace later resumes the Claude
session — accidental invocations are easily recoverable."
  (interactive)
  (let* ((ws (or ws (agent-repl--read-nukeable-workspace "Kill workspace: ")))
         (action (agent-repl--nuke-or-kill-workspace ws)))
    (force-mode-line-update t)
    (message (if (eq action 'nuke)
                 "Killed workspace: %s"
               "Killed persp workspace: %s")
             ws)))

;;;; Hide-mode sweep

(defun agent-repl--sweep-hidden-workspaces (&optional except)
  "Persp-kill every agent-repl workspace whose `:repl-state' is `:hidden'.
EXCEPT names a workspace to skip (typically the just-arrived destination
of a workspace switch — we don't want to kill the workspace the user is
currently sitting in).  Each match is torn down via
`agent-repl--nuke-one-workspace', which always preserves the on-disk
state file so the workspace can be re-opened via project switch.

No-op when there are no matching workspaces.  Returns the list of names
that were actually killed (useful for tests)."
  (let* ((current (or except (agent-repl--ws-current-name)))
         (candidates (cl-remove-if
                      (lambda (ws)
                        (or (equal ws current)
                            (not (eq (agent-repl--ws-repl-state ws) :hidden))))
                      (agent-repl--live-ws-names))))
    (agent-repl--log current
                      "sweep-hidden-workspaces: except=%s candidates=%S"
                      current candidates)
    (dolist (ws candidates)
      (condition-case err
          (agent-repl--nuke-one-workspace ws)
        (error
         (agent-repl--log ws "sweep-hidden-workspaces: kill error ws=%s err=%S"
                           ws err))))
    candidates))

(defun agent-repl--maybe-sweep-hidden-on-switch (&optional ws)
  "Run `agent-repl--sweep-hidden-workspaces' when hide-mode is enabled.
WS is the just-arrived-on workspace; when nil, falls back to
`(agent-repl--ws-current-name)'.  Callers from `--on-workspace-switch'
pass the ws captured at hook-fire time so the reset and sweep operate
on the workspace that was just switched to — not on whatever is
current when this deferred call eventually runs (rapid back-to-back
switches would otherwise leave intermediate `:hidden' workspaces
unreset and exposed to the sweep).

Hooked into `agent-repl--on-workspace-switch' (panels.el).  Also resets
WS's `:repl-state' from `:hidden' back to `:inactive' if applicable, so
navigating to a hidden workspace removes its hidden flag (the user is
actively viewing it; it should not be killed)."
  (let ((current (or ws (agent-repl--ws-current-name))))
    (when (eq (agent-repl--ws-repl-state current) :hidden)
      (agent-repl--log current
                        "maybe-sweep: arriving on :hidden ws, resetting to :inactive")
      (agent-repl--ws-set-repl-state current :inactive))
    (when agent-repl-hide-mode-enabled
      (agent-repl--sweep-hidden-workspaces current))))

(defun agent-repl-copy-reference ()
  "Copy the current file and line reference to the clipboard.
With active region: copies file:startline-endline.
Without region: copies file:line."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "copy-reference: copying file reference")
  (let ((ref (agent-repl--format-file-ref)))
    (kill-new ref)
    (message "Copied: %s" ref)))

(defun agent-repl-paste-clipboard ()
  "Insert the current workspace's `:clipboard' text at point.
The slot is populated by `clipboard'-typed workspace_commands files
\(see `agent-repl--handle-clipboard-command') — a per-workspace
clipboard, deliberately distinct from the OS clipboard.

Signals `user-error' when no text has been set for the current
workspace."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (text (agent-repl--ws-get ws :clipboard)))
    (unless text
      (user-error "No clipboard text set for workspace '%s'" ws))
    (agent-repl--log ws "paste-clipboard: ws=%s len=%d" ws (length text))
    (insert text)))

;;;; Workspace snapshot save/load

;; defconst (not defcustom) so reload always re-evaluates the path —
;; defcustom would leave a stale path bound from before this var
;; existed at this default.  Users wanting a custom path can `setq'
;; after load.
(defconst agent-repl-workspace-snapshot-file
  (agent-repl--global-state-file "workspaces.el")
  "Path to the file where the workspace roster snapshot is persisted.
Lives at `~/.claude-emacs/workspaces.el' (under `agent-repl--global-state-dir',
agent-repl's own state tree, NOT the Claude CLI config dir).
Auto-created on first save.")

(defconst agent-repl--legacy-workspace-snapshot-file
  (expand-file-name ".workspace-snapshot.el"
                    (file-name-directory (or load-file-name
                                              buffer-file-name
                                              default-directory)))
  "Pre-relocation snapshot file at the agent-repl module directory.
Read-only fallback: when the configured file does not exist but this
legacy file does, the loader uses it.  The writer never targets this
path — first save naturally migrates to the configured location.")

(defun agent-repl--workspace-snapshot-file-for-read ()
  "Return the path to read the workspace snapshot from.
Prefers `agent-repl-workspace-snapshot-file'; falls back to the
legacy module-dir path when only the legacy file exists."
  (cond ((file-exists-p agent-repl-workspace-snapshot-file)
         agent-repl-workspace-snapshot-file)
        ((file-exists-p agent-repl--legacy-workspace-snapshot-file)
         agent-repl--legacy-workspace-snapshot-file)
        (t agent-repl-workspace-snapshot-file)))

(defcustom agent-repl-workspace-snapshot-archive-max 20
  "Maximum number of historical workspace-snapshot archives to retain.
Each Emacs run archives the prior snapshot (if any) on its first save,
so this caps the count of distinct prior sessions kept on disk.  Older
archives are pruned silently.  Set to 0 to disable archival entirely."
  :type 'integer
  :group 'agent-repl)

(defvar agent-repl--snapshot-archived-this-run nil
  "Non-nil after the workspace snapshot has been archived this Emacs run.
The archival path runs at most once per Emacs run: the first save that
encounters an existing on-disk file copies it to the archive dir; every
subsequent save in the same run is a regular overwrite.  Cleared
implicitly by Emacs restart (the variable resets to nil at load).")

(defun agent-repl--workspace-snapshot-archive-dir ()
  "Return the directory where prior workspace-snapshot files are archived.
Sibling of `agent-repl-workspace-snapshot-file', named
`<basename-sans-ext>-archive'."
  (expand-file-name
   (concat (file-name-base agent-repl-workspace-snapshot-file) "-archive")
   (file-name-directory agent-repl-workspace-snapshot-file)))

(defun agent-repl--prune-snapshot-archives ()
  "Trim the snapshot archive dir to `agent-repl-workspace-snapshot-archive-max'.
Sorts archive files lexicographically (timestamp suffix is
sortable) and unlinks any beyond the cap."
  (let ((dir (agent-repl--workspace-snapshot-archive-dir))
        (max agent-repl-workspace-snapshot-archive-max))
    (when (and (file-directory-p dir) (> max 0))
      (let* ((all (sort (directory-files dir t "\\.el\\'" t) #'string<))
             (excess (- (length all) max)))
        (when (> excess 0)
          (dolist (f (seq-take all excess))
            (agent-repl--log nil "prune-snapshot-archives: deleting %s" f)
            (ignore-errors (delete-file f))))))))

(defun agent-repl--archive-workspace-snapshot ()
  "Copy the current workspace-snapshot file (if any) into the archive dir.
No-op when:
  - already archived this Emacs run (the once-per-run guard);
  - the archive cap is 0 (archival disabled);
  - the snapshot file does not yet exist (nothing to preserve).

The archive filename uses the OLD file's mtime as a `%Y%m%dT%H%M%S'
suffix so each archive is timestamped to the moment the previous
session's snapshot was last written.  Errors are caught (archival is
best-effort and must never block the live save)."
  (unless (or agent-repl--snapshot-archived-this-run
              (<= agent-repl-workspace-snapshot-archive-max 0)
              (not (file-exists-p agent-repl-workspace-snapshot-file)))
    (agent-repl--with-error-logging "archive-workspace-snapshot"
      (let* ((src agent-repl-workspace-snapshot-file)
             (mtime (file-attribute-modification-time (file-attributes src)))
             (suffix (format-time-string "%Y%m%dT%H%M%S" mtime))
             (dir (agent-repl--workspace-snapshot-archive-dir))
             (dest (expand-file-name (format "%s.el" suffix) dir)))
        (unless (file-directory-p dir) (make-directory dir t))
        (agent-repl--log nil "archive-workspace-snapshot: %s -> %s" src dest)
        (copy-file src dest t)
        (setq agent-repl--snapshot-archived-this-run t)
        (agent-repl--prune-snapshot-archives)))))

(defvar agent-repl--snapshot-loaded-p nil
  "Non-nil after `agent-repl-load-workspace-snapshot' has completed once
this session.  The save path checks this to refuse clobbering a richer
on-disk roster with the freshly started live roster (which only holds
the workspaces the user has visited manually so far).  Set to t at the
end of a successful load; reset by Emacs restart.")

(defvar agent-repl--restored-workspaces nil
  "List of workspace names established by snapshot-restore in this session.
Populated incrementally as each entry of the snapshot loader (either the
current file or an archived file via `agent-repl-load-workspace-snapshot-from-archive')
successfully calls `agent-repl--establish-workspace'.  Used by
`agent-repl-nuke-restored-workspaces' to nuke only the restored
workspaces while sparing any workspaces the user created manually before
or after the restore.  Entries are removed when their workspace is
nuked individually via `agent-repl--nuke-one-workspace'.")

(defun agent-repl--snapshot-save-safe-p (live-count)
  "Return non-nil when save may proceed with LIVE-COUNT entries.
Safe iff loader already ran this session (`agent-repl--snapshot-loaded-p'
is t) OR the on-disk roster is no larger than LIVE-COUNT.  The latter
covers the fresh-install case where no prior file exists, is empty, or
is unreadable as a sexp — there's nothing to lose."
  (or agent-repl--snapshot-loaded-p
      (let* ((file agent-repl-workspace-snapshot-file)
             (parsed (agent-repl--read-workspace-snapshot file))
             (on-disk (plist-get parsed :workspaces)))
        (or (null on-disk)
            (<= (length on-disk) live-count)))))

(defun agent-repl--snapshot-entry-normalize (entry)
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
           (t (error "agent-repl: malformed snapshot entry: %S" entry))))))

(defun agent-repl--collect-snapshot-entries ()
  "Return a list of (NAME :project-dir DIR [:nuked-at TIME] [:hidden-project-dir t]) entries.
Sourced from `agent-repl--workspaces'.  Includes every workspace
whose plist has a non-nil `:project-dir'.  `:priority' is deliberately
NOT included — it lives in each project's `<root>/.claude/emacs/state.el'
so the roster doesn't become a second source of truth.

Tombstoned entries (`:nuked-at' set) ARE included so the tombstone
survives across Emacs restart — otherwise a nuked workspace's identity
record would resurrect as live on next load.  Live entries omit
`:nuked-at' entirely so the on-disk format stays minimal for the common
case.

A tombstone killed by `agent-repl-toggle-hide-project-dirs' also
carries `:hidden-project-dir' so the next session can tell it apart
from a workspace the user nuked by hand and restore it on unhide.

Order: cache-ordered live prefix followed by tombstones.

Live entries are sourced via `--ws-list-names' (intersection of
`persp-names-cache' and `agent-repl--workspaces', nil-name stripped,
in cache order).  Live entries NOT in `persp-names-cache' are excluded
when the cache is bound — they have no current tab-bar presence and
saving them as live would cause the snapshot loader to re-establish
them as new tabs on the next load (the source of unexpected workspace
resurrection after kills that bypassed agent-repl's nuke path).

Tombstones are sourced via `--ws-tombstoned-names' and appended after
the live prefix, preserving their identity records across restarts.

Both helpers are workspace.el integration-boundary wrappers; this
function does not access `persp-names-cache' or `persp-nil-name'
directly.

Fallback: when `--ws-names-cache-bound-p' returns nil
\(pre-persp-mode init, test envs without persp-mode), all live entries
are included in hash-traversal order since the cache is not a
reliable tab-bar signal in that state.

Pairing the cache-ordered prefix with the snapshot loader's
skip-priority-reorder during load means the third tab at save time is
the third tab at load time — visual order is preserved across Emacs
restarts."
  (if (not (agent-repl--ws-names-cache-usable-p))
      ;; Fallback: persp-names-cache is not bound (pre-persp-mode init,
      ;; test envs without persp-mode).  Include all live entries in
      ;; hash-traversal order so nothing is silently dropped.
      (let (result)
        (maphash (lambda (ws plist)
                   (when-let ((dir (plist-get plist :project-dir)))
                     (let ((tomb (plist-get plist :nuked-at))
                           (hidden (plist-get plist :hidden-project-dir)))
                       (push (cons ws (if tomb
                                          (append (list :project-dir dir :nuked-at tomb)
                                                  (when hidden (list :hidden-project-dir t)))
                                        (list :project-dir dir)))
                             result))))
                 agent-repl--workspaces)
        result)
    ;; Normal path: cache is bound.  Route all persp-mode access through
    ;; workspace.el's integration boundary.
    (let* (;; Live tab-bar entries in cache order via --ws-list-names.
           ;; Entries not in the cache are naturally excluded.
           (live-entries
            (cl-remove-if-not
             #'identity
             (mapcar (lambda (ws)
                       (when-let ((dir (agent-repl--ws-get ws :project-dir)))
                         (cons ws (list :project-dir dir))))
                     (agent-repl--ws-list-names))))
           ;; Tombstones via --ws-tombstoned-names.  Tombstones have no
           ;; persp/cache presence so they never appear in --ws-list-names
           ;; and must be collected separately.
           (tomb-entries
            (cl-remove-if-not
             #'identity
             (mapcar (lambda (ws)
                       (when-let ((dir (agent-repl--ws-get ws :project-dir)))
                         (let ((tomb (agent-repl--ws-get ws :nuked-at))
                               (hidden (agent-repl--ws-get ws :hidden-project-dir)))
                           (cons ws (append (list :project-dir dir :nuked-at tomb)
                                            (when hidden (list :hidden-project-dir t)))))))
                     (agent-repl--ws-tombstoned-names)))))
      (append live-entries tomb-entries))))

(defun agent-repl--snapshot-raw-format (raw)
  "Classify the RAW sexp read from a workspace-snapshot file.
Returns `:plist' when RAW is a plist (top-level keyword keys — the
current format that carries both `:workspaces' and `:merge-queue'),
`:legacy' when RAW is the older list-of-entries shape (each element a
cons/list whose car is a ws-name string), and `:empty' when RAW is nil."
  (cond
   ((null raw) :empty)
   ((and (consp raw) (keywordp (car raw))) :plist)
   (t :legacy)))

(defun agent-repl--snapshot-entries-from-raw (raw)
  "Return the workspace-entries list from RAW (a parsed snapshot sexp).
Handles both the current plist format and the legacy list-of-entries
format.  Returns nil when RAW carries no entries (or is itself nil)."
  (pcase (agent-repl--snapshot-raw-format raw)
    (:plist (plist-get raw :workspaces))
    (:legacy raw)
    (_ nil)))

(defun agent-repl--snapshot-merge-queue-from-raw (raw)
  "Return the persisted merge-queue from RAW (a parsed snapshot sexp).
Returns nil when RAW is in the legacy list-of-entries format (which
predates merge-queue persistence) or carries no `:merge-queue' key."
  (pcase (agent-repl--snapshot-raw-format raw)
    (:plist (plist-get raw :merge-queue))
    (_ nil)))

(defun agent-repl--snapshot-in-flight-merges-from-raw (raw)
  "Return the persisted in-flight-merges list from RAW (a parsed snapshot sexp).
Returns nil when RAW predates the in-flight-merge persistence (legacy
list-of-entries or a plist without `:in-flight-merges')."
  (pcase (agent-repl--snapshot-raw-format raw)
    (:plist (plist-get raw :in-flight-merges))
    (_ nil)))

(defun agent-repl--snapshot-hide-project-dirs-from-raw (raw)
  "Return the persisted hide-project-dirs toggle state from RAW.
RAW is a parsed snapshot sexp.  Returns nil when RAW predates the
hide-project-dirs persistence (legacy list-of-entries, or a plist
without the `:hide-project-dirs-enabled' key)."
  (pcase (agent-repl--snapshot-raw-format raw)
    (:plist (plist-get raw :hide-project-dirs-enabled))
    (_ nil)))

(defun agent-repl--read-workspace-snapshot (file)
  "Read FILE and return a plist with the parsed snapshot contents.
Returned shape: `(:workspaces ENTRIES :merge-queue QUEUE
:in-flight-merges IN-FLIGHT :hide-project-dirs-enabled BOOL)'.

Normalizes both legacy (`((ws :project-dir dir) ...)') and current
plist-shaped files into the plist return shape so callers don't need
to branch on disk layout.  Returns nil when FILE does not exist or
the sexp is unreadable."
  (when (and file (file-exists-p file))
    (condition-case err
        (let ((raw (agent-repl--read-sexp-file file)))
          (list :workspaces (agent-repl--snapshot-entries-from-raw raw)
                :merge-queue (agent-repl--snapshot-merge-queue-from-raw raw)
                :in-flight-merges (agent-repl--snapshot-in-flight-merges-from-raw raw)
                :hide-project-dirs-enabled
                (agent-repl--snapshot-hide-project-dirs-from-raw raw)))
      (error
       (agent-repl--log nil "read-workspace-snapshot: read err file=%s err=%S"
                         file err)
       nil))))

(defun agent-repl--serialize-merge-queue (queue)
  "Return QUEUE (the live `agent-repl--merge-queue') stripped down to
the keys that survive `read' round-trip.  Every entry plist is plain
strings/booleans/nil today, so serialization is a key-pick.  The
indirection keeps the on-disk format insulated from future plist-key
additions.

Carries the loop-guard metadata `:last-attempt-target-head' (HEAD SHA
recorded at re-enqueue time after a failed merge), the
`:halt-until-human' flag (set on generic-failure re-enqueues to block
auto-drain), and the `:target-dir' bucket key (canonical cherry-pick
destination, used to partition the queue into independent per-target
sub-queues) so a restart preserves the same drain semantics as the
live queue."
  (mapcar (lambda (entry)
            (list :source-ws (plist-get entry :source-ws)
                  :silent (and (plist-get entry :silent) t)
                  :auto-resolve (and (plist-get entry :auto-resolve) t)
                  :target-dir (plist-get entry :target-dir)
                  :last-attempt-target-head
                  (plist-get entry :last-attempt-target-head)
                  :halt-until-human
                  (and (plist-get entry :halt-until-human) t)))
          queue))

(defun agent-repl--serialize-in-flight-merges (in-flight)
  "Return IN-FLIGHT (the live `agent-repl--in-flight-merges') stripped
down to the keys that survive `read' round-trip.  Mirrors
`agent-repl--serialize-merge-queue' so the persisted format stays
insulated from future plist-key additions on the live entries."
  (mapcar (lambda (entry)
            (list :source-ws (plist-get entry :source-ws)
                  :target-dir (plist-get entry :target-dir)
                  :started-at (plist-get entry :started-at)))
          in-flight))

(defun agent-repl--write-workspace-snapshot (snapshot &optional merge-queue in-flight-merges)
  "Write SNAPSHOT (a list of workspace entries) and queue state to
`agent-repl-workspace-snapshot-file' in the plist format
`(:workspaces SNAPSHOT :merge-queue MERGE-QUEUE
:in-flight-merges IN-FLIGHT-MERGES
:hide-project-dirs-enabled BOOL)'.

When MERGE-QUEUE / IN-FLIGHT-MERGES are omitted, defaults to the live
`agent-repl--merge-queue' / `agent-repl--in-flight-merges' so every
snapshot write captures the live state alongside the roster.

`:hide-project-dirs-enabled' records the live
`agent-repl-hide-project-dirs-enabled' toggle so a session restore
reconstructs the hidden set.

Creates the parent directory if missing and archives the previous file
before overwriting.  Caller is responsible for any pre-write checks
\(e.g. `--snapshot-save-safe-p' or interactive confirmation)."
  (agent-repl--log nil "write-sexp-file: file=%s" agent-repl-workspace-snapshot-file)
  (let ((dir (file-name-directory agent-repl-workspace-snapshot-file)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (agent-repl--archive-workspace-snapshot)
  (let* ((queue (agent-repl--serialize-merge-queue
                 (or merge-queue
                     (and (boundp 'agent-repl--merge-queue)
                          agent-repl--merge-queue))))
         (in-flight (agent-repl--serialize-in-flight-merges
                     (or in-flight-merges
                         (and (boundp 'agent-repl--in-flight-merges)
                              agent-repl--in-flight-merges)))))
    (with-temp-file agent-repl-workspace-snapshot-file
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
      (insert ")\n :in-flight-merges (")
      (let ((first t))
        (dolist (entry in-flight)
          (unless first (insert "\n                     "))
          (setq first nil)
          (prin1 entry (current-buffer))))
      (insert ")\n :hide-project-dirs-enabled ")
      (prin1 (and (boundp 'agent-repl-hide-project-dirs-enabled)
                  agent-repl-hide-project-dirs-enabled
                  t)
             (current-buffer))
      (insert ")"))))

(defun agent-repl-save-workspace-snapshot ()
  "Save the current set of agent-repl workspaces to a hidden file.
Writes a list of (NAME :project-dir DIR) entries sourced from
`agent-repl--workspaces' (the live hash).

Refuses to overwrite when the loader hasn't run this session AND the
on-disk roster is larger than the live hash — the auto-piggyback save
path uses this to avoid clobbering a richer snapshot with a half-
populated live set during startup.  Use
`agent-repl-update-workspace-snapshot' to force an overwrite.

Called interactively records a confirmation via `agent-repl--info' (log
+ *Messages*, never the echo area); called from `agent-repl--state-save'
(the common path) stays silent so the roster-piggyback save doesn't spam
on every state mutation."
  (interactive)
  (let ((snapshot (agent-repl--collect-snapshot-entries)))
    (if (not (agent-repl--snapshot-save-safe-p (length snapshot)))
        (agent-repl--log nil
                          "save-workspace-snapshot: ABORTED — loader hasn't run this session and on-disk roster is larger than live (%d)"
                          (length snapshot))
      (agent-repl--write-workspace-snapshot snapshot)
      (when (called-interactively-p 'interactive)
        (message "Saved %d workspace(s) to %s"
                 (length snapshot) agent-repl-workspace-snapshot-file)))))

(defun agent-repl-update-workspace-snapshot ()
  "Force-write the current live workspace roster to the snapshot file.
Captures every workspace in `agent-repl--workspaces' with a
`:project-dir' (the same set offered by `agent-repl-nuke-workspace')
and overwrites `agent-repl-workspace-snapshot-file' unconditionally.

Unlike `agent-repl-save-workspace-snapshot', this command bypasses the
loader-hasn't-run safety guard.  If the write would reduce the entry
count compared to what is currently on disk, prompts for confirmation
first so a slip can't silently shrink the roster.

Use this after manually creating / killing workspaces when you want
the on-disk snapshot to reflect the current live state immediately,
without waiting for the next `--state-save' piggyback."
  (interactive)
  (let* ((snapshot (agent-repl--collect-snapshot-entries))
         (live-count (length snapshot))
         (file agent-repl-workspace-snapshot-file)
         (parsed (agent-repl--read-workspace-snapshot file))
         (on-disk (plist-get parsed :workspaces))
         (on-disk-count (length on-disk)))
    (when (and (> on-disk-count live-count)
               (not (y-or-n-p
                     (format "On-disk snapshot has %d entries, live has %d.  Overwrite anyway? "
                             on-disk-count live-count))))
      (user-error "Aborted"))
    (agent-repl--write-workspace-snapshot snapshot)
    (message "Updated snapshot: %d workspace(s) -> %s"
             live-count file)))

(defun agent-repl--clean-frame-foreign-windows (ws)
  "Delete frame windows whose buffer is owned by a different workspace.
A window is foreign iff its buffer's `agent-repl--owning-workspace'
buffer-local is non-nil AND not `equal' to WS.  Buffers with no owning
workspace (regular files, dashboard, scratch, fallback) are treated as
allowed, since they are workspace-agnostic.

Strips `no-delete-other-windows' and dedication from foreign windows
first so prior workspaces' agent-repl panel windows (which are marked
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
at creation time by `agent-repl--create-buffer' and is immune to
persp routing."
  (let* ((fallback (and (fboundp 'doom-fallback-buffer) (doom-fallback-buffer)))
         (all (window-list nil 'nomini))
         (foreign-p (lambda (w)
                      (agent-repl--foreign-owned-buffer-p (window-buffer w) ws)))
         (foreign (cl-remove-if-not foreign-p all))
         (any-native (< (length foreign) (length all))))
    (when foreign
      (agent-repl--log ws "clean-frame-foreign-windows: ws=%s removing=%d total=%d"
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

(defun agent-repl--hydrate-and-reorder-on-open (ws project-root)
  "Hydrate WS's display state from PROJECT-ROOT then reseat WS by priority.
Shared post-open step for every path that opens or activates a
workspace — interactive `SPC p p' (`agent-repl-switch-to-project'),
the snapshot/worktree restore path (`agent-repl--establish-workspace'),
and any future opener.  Centralizing the sequence guarantees that a
workspace lands in priority order no matter how it was opened.

Steps:

- hydrates the persisted priority badge and drawer glyphs via
  `agent-repl--load-display-state', so `:priority' is in memory
  before the reseat reads it,
- pulls WS to its priority slot in `persp-names-cache' via
  `agent-repl--reorder-workspace-by-priority', so the tab-bar order
  reflects priority the moment the workspace is opened.

The reorder is SKIPPED while a snapshot load is in flight
(`agent-repl--snapshot-load-state' non-nil): the loader visits
entries in saved tab-bar order, and a per-entry priority reseating
would shuffle them back into priority order, defeating the order
preservation `agent-repl--collect-snapshot-entries' encodes on save.

Both inner calls are `fboundp'-guarded so a partial-load test
environment that has not defined them does not crash here."
  (when (fboundp 'agent-repl--load-display-state)
    (agent-repl--load-display-state ws project-root))
  (when (and (fboundp 'agent-repl--reorder-workspace-by-priority)
             (not agent-repl--snapshot-load-state))
    (agent-repl--reorder-workspace-by-priority ws)))

(defun agent-repl--establish-workspace (ws dir)
  "Synchronously create + activate + fully set up workspace WS for DIR.
Mirrors what `agent-repl-switch-to-project' would do on an
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
  `agent-repl--clean-frame-foreign-windows' (dedicated panels from
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
- hydrates `:project-dir' into `agent-repl--workspaces' and
  rehydrates persisted display state (`:priority' and the drawer
  badges) from the per-project state file via
  `agent-repl--load-display-state',
- reorders the ws in `persp-names-cache' by its hydrated `:priority'
  via `agent-repl--reorder-workspace-by-priority' (drawer-driven
  restores, worktree hydration), matching what
  `agent-repl-set-priority' does for user-driven changes,
  - SKIPPED while a snapshot load is in flight
    (`agent-repl--snapshot-load-state' non-nil): the loader visits
    entries in saved tab-bar order and per-entry priority reseating
    would shuffle them back into priority order, defeating
    `agent-repl--collect-snapshot-entries' order preservation,
- starts claude (`agent-repl--initialize-agent') unless already
  running."
  (agent-repl--with-error-logging (format "establish-workspace[%s]" ws)
    ;; Create the persp and tag it with `+workspace-project' so a later
    ;; `SPC p p' to DIR matches this workspace; see --ws-create for the
    ;; full rationale.
    (agent-repl--ws-create ws dir)
    ;; WHY: this switch is load-bearing for `--snapshot-load-finish' in
    ;; addition to its primary purpose.  persp-mode auto-saves the
    ;; previous persp's window-configuration on switch-away, and finish
    ;; relies on that saved wconf when it does `persp-frame-switch
    ;; origin' at end-of-load — without this call here on the first
    ;; queue entry, origin would have no saved wconf and finish would
    ;; land in whatever windows the last-loaded ws ended with.  If this
    ;; is ever reordered or replaced with a setup-without-switch, the
    ;; snapshot loader's return-to-origin layout silently regresses.
    (agent-repl--ws-frame-switch ws)
    ;; Strip any window-configuration bleed-over from the prior persp before
    ;; populating WS — panels left over from the previous workspace are marked
    ;; `no-delete-other-windows', so the later `delete-other-windows' in
    ;; `--show-existing-panels' won't tear them down on its own.
    (agent-repl--clean-frame-foreign-windows ws)
    (agent-repl--ws-register-project dir)
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
          (agent-repl--ws-run-switch-project-function dir)
        (when (and fb (buffer-live-p fb) orig-dir)
          (with-current-buffer fb
            (setq default-directory orig-dir)))))
    (when-let ((recent-file (agent-repl--most-recent-project-file dir)))
      (when (file-exists-p recent-file)
        (find-file recent-file)))
    ;; Wake any pre-existing tombstone before re-asserting identity keys —
    ;; an `--establish-workspace' call is the canonical resurrection path
    ;; for snapshot-loaded entries that may have been tombstoned in a prior
    ;; session.  Clearing `:nuked-at' first keeps `--ws-live-p' coherent
    ;; with the post-establish state.
    (agent-repl--ws-put ws :nuked-at nil)
    (agent-repl--ws-put ws :project-dir dir)
    ;; Hydrate the priority badge then reseat this ws into its priority
    ;; slot, via the shared opener step so `SPC p p' and snapshot/worktree
    ;; restore agree on ordering.  The reorder is skipped mid-snapshot-load
    ;; (see `agent-repl--hydrate-and-reorder-on-open').
    (agent-repl--hydrate-and-reorder-on-open ws dir)
    (when (and (fboundp 'agent-repl--initialize-agent)
               (fboundp 'agent-repl--agent-running-p)
               (not (agent-repl--agent-running-p ws)))
      (agent-repl--initialize-agent ws))))

(defvar agent-repl--snapshot-load-state nil
  "Plist describing an in-progress recursive snapshot load, or nil.
Keys: `:queue' (list of (NORMALIZED-WS . PLIST) entries still to do),
`:origin' (workspace to switch back to at end), `:awaiting' (ws-name
the loader is currently waiting on a ready signal for, or nil),
`:loaded' (successfully established + ready/awaiting), `:skipped'
(dir missing/nil), `:load-error' (establish-workspace signaled),
`:total' (entry count from the file), `:timeout-timer' (the per-entry
watchdog timer).

Non-nil means a load is in flight — concurrent invocations of
`agent-repl-load-workspace-snapshot' are refused via a guard.")

(defcustom agent-repl-snapshot-load-per-entry-timeout 30
  "Per-entry watchdog in seconds for the recursive snapshot loader.
If the awaited workspace's `agent-repl--on-session-start-event' hasn't
fired by then, the loader advances to the next entry anyway and logs
a warning.  Tuned long enough for sandbox image build / first-time
claude startup but short enough that a wedged workspace doesn't lock
the entire load."
  :type 'number
  :group 'agent-repl)

(defun agent-repl--snapshot-load-ws-ready-p (ws)
  "Return non-nil when WS's vterm buffer reports ready."
  (when-let ((buf (agent-repl--ws-get ws :vterm-buffer)))
    (and (buffer-live-p buf)
         (buffer-local-value 'agent-repl--ready buf))))

(defun agent-repl--snapshot-load-cancel-timer ()
  "Cancel the pending per-entry watchdog timer, if any."
  (when-let ((timer (and agent-repl--snapshot-load-state
                         (plist-get agent-repl--snapshot-load-state :timeout-timer))))
    (when (timerp timer) (cancel-timer timer))
    (setq agent-repl--snapshot-load-state
          (plist-put agent-repl--snapshot-load-state :timeout-timer nil))))

(defun agent-repl--snapshot-restore-merge-queue (saved-mq)
  "Repopulate `agent-repl--merge-queue' from SAVED-MQ (read from disk).
Filters out entries whose `:source-ws' no longer exists in
`agent-repl--workspaces' (the workspace was removed between sessions,
or its snapshot entry was skipped because its `:project-dir' was gone).
Re-applies the `:repl-state :merge-queued' marker on each surviving
source-ws so the drawer's MERGING bucket re-surfaces them.  Preserves
each entry's `:target-dir' so the per-target sub-queue partitioning
survives the restart (a missing key falls back to lazy resolution in
the drain).

Does NOT auto-drain — `agent-repl--workspace-merge-do' is the normal
drain trigger and the user kicks it off via `agent-repl-drain-merge-queue'
\(intended for cases where the in-flight cherry-pick died with Emacs and
the user has manually resolved before re-entering the loop)."
  (when (and saved-mq (boundp 'agent-repl--merge-queue))
    (let ((restored nil)
          (dropped 0))
      (dolist (entry saved-mq)
        (let ((ws (plist-get entry :source-ws)))
          (cond
           ((and ws (gethash ws agent-repl--workspaces))
            (push (list :source-ws ws
                        :silent (and (plist-get entry :silent) t)
                        :auto-resolve (and (plist-get entry :auto-resolve) t)
                        :target-dir (plist-get entry :target-dir))
                  restored)
            (agent-repl--ws-put ws :repl-state :merge-queued)
            (agent-repl--ws-put ws :agent-state nil))
           (t
            (cl-incf dropped)
            (agent-repl--log nil
                              "snapshot-restore-merge-queue: dropping entry ws=%s — ws absent post-load"
                              (or ws "nil"))))))
      (setq agent-repl--merge-queue (nreverse restored))
      (agent-repl--log nil
                        "snapshot-restore-merge-queue: restored=%d dropped=%d"
                        (length agent-repl--merge-queue) dropped))))

(defun agent-repl--snapshot-restore-in-flight-merges (saved-in-flight)
  "Repopulate `agent-repl--in-flight-merges' from SAVED-IN-FLIGHT (read from disk).
The early-recovery in `config.el' should have already drained on-disk
in-flight entries (aborted any orphan cherry-pick and moved each to
`:merge-queue').  This restoration is a safety net: if early-recovery
was skipped or failed silently, the live var still reflects the on-disk
state so subsequent `--push-in-flight-merge' / `--clear-in-flight-merge'
mutations have a consistent base."
  (when (and saved-in-flight (boundp 'agent-repl--in-flight-merges))
    (setq agent-repl--in-flight-merges
          (mapcar (lambda (entry)
                    (list :source-ws (plist-get entry :source-ws)
                          :target-dir (plist-get entry :target-dir)
                          :started-at (plist-get entry :started-at)))
                  saved-in-flight))
    (agent-repl--log nil
                      "snapshot-restore-in-flight-merges: restored=%d"
                      (length agent-repl--in-flight-merges))))

(defun agent-repl--snapshot-load-finish ()
  "Finalize the recursive load: detach hook, return to origin, message.
Idempotent: re-entry with `agent-repl--snapshot-load-state' already
nil is a no-op so the error-recovery path in `--snapshot-load-step'
can call finish without worrying whether a normal finish already ran."
  (when agent-repl--snapshot-load-state
    (remove-hook 'agent-repl-ws-fully-loaded-functions
                 #'agent-repl--snapshot-load-on-loaded)
    (agent-repl--snapshot-load-cancel-timer)
    (let* ((state agent-repl--snapshot-load-state)
           (origin (plist-get state :origin))
           (loaded (plist-get state :loaded))
           (skipped (plist-get state :skipped))
           (load-error (or (plist-get state :load-error) 0))
           (saved-mq (plist-get state :saved-merge-queue))
           (saved-ifm (plist-get state :saved-in-flight-merges)))
      (agent-repl--snapshot-restore-merge-queue saved-mq)
      (agent-repl--snapshot-restore-in-flight-merges saved-ifm)
      ;; persp-mode saved origin's window-config when the loader's first
      ;; `--establish-workspace' switched away from it, so this switch-back
      ;; replays that layout — and persp-mode's restore filters foreign
      ;; buffers, so panels owned by some other ws can't bleed in.
      (when (and origin
                 (agent-repl--ws-exists-p origin))
        (agent-repl--ws-frame-switch origin))
      (force-mode-line-update t)
      (setq agent-repl--snapshot-loaded-p t)
      (let ((mq-restored (and (boundp 'agent-repl--merge-queue)
                              (length agent-repl--merge-queue))))
        (agent-repl--log nil
                          "snapshot-load: END loaded=%d skipped=%d load-error=%d merge-queue=%d returned-to=%s"
                          loaded skipped load-error (or mq-restored 0) (or origin "nil"))
        (agent-repl--info nil "Loaded %d workspace(s), skipped %d, errored %d%s"
                          loaded skipped load-error
                          (if (and mq-restored (> mq-restored 0))
                              (format ", merge-queue=%d" mq-restored)
                            ""))))
    (setq agent-repl--snapshot-load-state nil)
    (agent-repl--snapshot-load-close-main)))

(defun agent-repl--snapshot-load-close-main ()
  "Nuke the `main' workspace left over from Doom's startup, if it still exists.
Doom always creates `+workspaces-main' (typically \"main\") at startup;
once the snapshot load has populated the real workspace set, this
artifact is no longer useful and we tear it down to keep the tabline
clean.  Absent main, the function is a no-op.

NUKE semantics (vs. a plain `+workspace/kill'): we first sweep every
buffer that belongs to the persp via
`agent-repl--kill-workspace-buffers' (dashboard, scratch, file
buffers, etc.) and only then drop the persp itself.  A bare
`+workspace/kill' leaves those buffers orphaned in the global buffer
list, which is exactly what the user asked us to avoid here.

Each step is wrapped independently in `condition-case' so an error in
the buffer sweep does not block the persp kill, and an error in either
step is logged but never propagated — finish must remain robust."
  (let ((main (agent-repl--ws-main-name)))
    (when (and main
               (agent-repl--ws-exists-p main))
      (agent-repl--log nil "snapshot-load: nuking 'main' workspace artifact main=%s" main)
      (condition-case err
          (agent-repl--kill-workspace-buffers main)
        (error (agent-repl--log nil "snapshot-load: nuke-main kill-buffers error: %S" err)))
      (condition-case err
          (agent-repl--ws-kill main)
        (error (agent-repl--log nil "snapshot-load: nuke-main persp-kill error: %S" err))))))

(defun agent-repl--snapshot-load-on-loaded (ws &optional _marker)
  "Ws-fully-loaded hook handler: advance the snapshot load queue iff WS is awaited.
Called from `agent-repl-ws-fully-loaded-functions' with the ws name and
an optional MARKER (e.g. `:timed-out' when the watchdog synthesized the
event).  Loader doesn't distinguish the marker — once a ws is loaded
or timed out, it advances.  Idempotent: the `:awaiting' equality guard
makes second fires for the same ws no-ops."
  (let ((state agent-repl--snapshot-load-state))
    (when (and state (equal ws (plist-get state :awaiting)))
      (agent-repl--log ws "snapshot-load: awaited ws=%s fully loaded — advancing" ws)
      (agent-repl--snapshot-load-cancel-timer)
      (setq agent-repl--snapshot-load-state
            (plist-put agent-repl--snapshot-load-state :awaiting nil))
      (agent-repl--snapshot-load-step))))

(defun agent-repl--snapshot-load-timeout (ws)
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
- `:agent-ready' is also flipped to t directly so the helper's
  both-bits check passes even when claude never printed
  `session_start' (the most common timeout cause)."
  (let ((state agent-repl--snapshot-load-state))
    (when (and state (equal ws (plist-get state :awaiting)))
      (agent-repl--log ws "snapshot-load: TIMEOUT awaiting ws=%s — forcing fully-loaded :timed-out" ws)
      (agent-repl--warn ws "snapshot-load timeout awaiting ws=%s — advancing" ws)
      (setq agent-repl--snapshot-load-state
            (plist-put agent-repl--snapshot-load-state :timeout-timer nil))
      ;; Force both latch bits then fire via the helper.  Setting
      ;; :agent-ready directly before the helper call means the
      ;; helper's both-bits check will pass on its own :ws-loaded
      ;; flip, firing ws-fully-loaded with the :timed-out marker.
      (agent-repl--ws-put ws :agent-ready t)
      (agent-repl--latch-and-maybe-fire-loaded ws :ws-loaded :timed-out))))

(defun agent-repl--snapshot-load-step ()
  "Process the next entry in the snapshot-load queue.
Called both at start and from the ws-fully-loaded hook / timeout callback.

The body is wrapped in `condition-case' that routes any uncaught error
to `--snapshot-load-finish' — without this, a signal from
`--snapshot-load-ws-ready-p', `run-with-timer', a plist mutation, etc.,
would leave `agent-repl-ws-fully-loaded-functions' attached and
`agent-repl--snapshot-load-state' non-nil, turning a future
`session_start' event into a zombie-loader resume from a corrupt queue."
  (condition-case err
      (agent-repl--snapshot-load-step--unsafe)
    (error
     (agent-repl--log nil "snapshot-load: STEP ERROR err=%S — finishing early" err)
     (agent-repl--warn nil "snapshot-load step error: %S — aborting load" err)
     (agent-repl--snapshot-load-finish))))

(defun agent-repl--snapshot-load-step--unsafe ()
  "Unguarded implementation of `--snapshot-load-step'.
Public callers should use `--snapshot-load-step', which wraps this in
the error-routing `condition-case'."
  (let* ((state agent-repl--snapshot-load-state)
         (queue (plist-get state :queue))
         (total (plist-get state :total))
         (iter  (1+ (+ (plist-get state :loaded)
                       (plist-get state :skipped)
                       (or (plist-get state :load-error) 0)))))
    (cond
     ((null queue)
      (agent-repl--snapshot-load-finish))
     (t
      (let* ((entry (car queue))
             (ws (car entry))
             (plist (cdr entry))
             (dir (plist-get plist :project-dir)))
        ;; Pop this entry off the queue immediately so we don't double-process.
        (setq agent-repl--snapshot-load-state
              (plist-put state :queue (cdr queue)))
        (cond
         ((not (and dir (file-directory-p dir)))
          (agent-repl--log nil "snapshot-load iter=%d/%d SKIPPED ws=%s dir=%s reason=dir-missing-or-nil"
                            iter total ws (or dir "nil"))
          (setq agent-repl--snapshot-load-state
                (plist-put agent-repl--snapshot-load-state :skipped
                           (1+ (plist-get agent-repl--snapshot-load-state :skipped))))
          (agent-repl--snapshot-load-step))
         ;; Merged-completed entries: register data-only and advance.
         ;; The drawer's MERGED bucket renders these; `--finish-workspace'
         ;; (invoked via drawer `x') is the only way out.
         ;;
         ;; Exception: when register-merged-workspace flags `:merge-failed t'
         ;; (either via on-disk state or the git-landing probe), promote the
         ;; entry from drawer-only to a real tab-bar workspace via
         ;; `--establish-workspace' and move it to the front of
         ;; `persp-names-cache' via `--reorder-workspace-to-front'.  A failed
         ;; cherry-pick must not hide in the MERGED bucket post-restart —
         ;; surfacing it as the leftmost tab forces the user to notice and
         ;; act (retry / investigate / dismiss).
         ((agent-repl--state-merge-completed-p dir)
          (agent-repl--log nil "snapshot-load iter=%d/%d ws=%s dir=%s register-merged"
                            iter total ws dir)
          (condition-case err
              (agent-repl--register-merged-workspace ws dir)
            (error
             (agent-repl--log nil "snapshot-load: register-merged err ws=%s err=%S" ws err)))
          (when (eq (agent-repl--ws-get ws :merge-failed) t)
            (agent-repl--log nil "snapshot-load iter=%d/%d ws=%s dir=%s merge-failed -> establish + front-reorder"
                              iter total ws dir)
            (condition-case err
                (progn
                  (agent-repl--establish-workspace ws dir)
                  (agent-repl--reorder-workspace-to-front ws))
              (error
               (agent-repl--log nil "snapshot-load: failed-restore establish err ws=%s err=%S" ws err))))
          (setq agent-repl--snapshot-load-state
                (plist-put agent-repl--snapshot-load-state :loaded
                           (1+ (plist-get agent-repl--snapshot-load-state :loaded))))
          (agent-repl--snapshot-load-step))
         (t
          (agent-repl--log nil "snapshot-load iter=%d/%d ws=%s dir=%s establishing"
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
                (agent-repl--establish-workspace ws dir)
              (error
               (setq establish-error err)
               (agent-repl--log nil "snapshot-load: establish-workspace err ws=%s err=%S" ws err)
               (agent-repl--warn ws "establish failed ws=%s — advancing" ws)))
            (cond
             (establish-error
              ;; Failure: bump :load-error, leave :awaiting nil, advance
              ;; immediately without arming the watchdog (no ws to wait on).
              (setq agent-repl--snapshot-load-state
                    (plist-put agent-repl--snapshot-load-state :load-error
                               (1+ (or (plist-get agent-repl--snapshot-load-state :load-error) 0))))
              (agent-repl--snapshot-load-step))
             (t
              ;; Success: bump :loaded, then wait for ready (or detect
              ;; already-ready and advance immediately).
              (setq agent-repl--snapshot-load-state
                    (plist-put agent-repl--snapshot-load-state :loaded
                               (1+ (plist-get agent-repl--snapshot-load-state :loaded))))
              (cond
               ((agent-repl--snapshot-load-ws-ready-p ws)
                ;; Already ready (e.g. the origin ws the user was sitting
                ;; in when load began, or any other ws claude was already
                ;; up in before the 2s idle loader fired).  Do NOT tag as
                ;; restored — this ws wasn't actually established by the
                ;; loader, it was already alive.  Tagging it would make
                ;; `agent-repl-nuke-restored-workspaces' incorrectly
                ;; sweep the user's pre-existing workspace.
                (agent-repl--log ws "snapshot-load: ws=%s already ready — advancing without waiting" ws)
                (agent-repl--snapshot-load-step))
               (t
                ;; WHY: tag this ws as restored-this-session so the user
                ;; can later nuke only the restore-batch via
                ;; `agent-repl-nuke-restored-workspaces' without
                ;; touching workspaces they created by hand or were
                ;; already in.  Accumulates across multiple loads (incl.
                ;; from-archive) so subsequent restores expand — never
                ;; shrink — the set.
                (cl-pushnew ws agent-repl--restored-workspaces :test #'equal)
                ;; Now — after establish has fully returned — mark `:awaiting'
                ;; and arm the watchdog.  The ws-fully-loaded hook (or the
                ;; watchdog) will call --snapshot-load-step again.
                (setq agent-repl--snapshot-load-state
                      (plist-put agent-repl--snapshot-load-state :awaiting ws))
                (setq agent-repl--snapshot-load-state
                      (plist-put agent-repl--snapshot-load-state :timeout-timer
                                 (run-with-timer
                                  agent-repl-snapshot-load-per-entry-timeout
                                  nil
                                  #'agent-repl--snapshot-load-timeout
                                  ws)))))))))))))))

(defun agent-repl-load-workspace-snapshot (&optional file)
  "Load workspaces from FILE (defaults to the configured snapshot path).
When FILE is nil, reads `agent-repl-workspace-snapshot-file' (or its
legacy module-dir fallback if the configured file is absent).  For each
entry, fully sets up the workspace via `agent-repl--establish-workspace'
\(persp creation + activation + projectile + dir-locals + magit lambda
+ find-file recent + claude init).

Recursive queue driver: establishes one entry, then yields to the main
loop until that workspace's `agent-repl-ws-fully-loaded-functions'
hook fires (i.e., both agent-side ready and emacs-side switch-settle
have completed), then advances.  Per-entry watchdog
\(`agent-repl-snapshot-load-per-entry-timeout') guarantees forward
progress even if the load barrier never fires; on timeout the loader
synthesizes a ws-fully-loaded fire with a `:timed-out' marker so all
hook observers see the same advance event.

Returns to the workspace that was active when the load began."
  (interactive)
  (when agent-repl--snapshot-load-state
    (user-error "agent-repl: a snapshot load is already in progress"))
  (let* ((file (or file (agent-repl--workspace-snapshot-file-for-read)))
         (parsed (agent-repl--read-workspace-snapshot file))
         (snapshot (plist-get parsed :workspaces))
         (saved-mq (plist-get parsed :merge-queue))
         (saved-ifm (plist-get parsed :in-flight-merges))
         (saved-hide (plist-get parsed :hide-project-dirs-enabled)))
    (unless snapshot
      (user-error "No workspace snapshot at %s" file))
    ;; Restore the hide-project-dirs toggle BEFORE establishing entries —
    ;; the tombstone-vs-live partition below already encodes the hidden
    ;; set (hidden workspaces were saved as `:hidden-project-dir'
    ;; tombstones), so the runtime flag just needs to agree with it.
    (when (boundp 'agent-repl-hide-project-dirs-enabled)
      (setq agent-repl-hide-project-dirs-enabled (and saved-hide t)))
    (let* ((normalized (mapcar #'agent-repl--snapshot-entry-normalize snapshot))
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
           (origin-ws (agent-repl--ws-current-name)))
      (dolist (entry tombstones)
        (let ((ws (car entry))
              (plist (cdr entry)))
          (agent-repl--ws-put ws :project-dir (plist-get plist :project-dir))
          (agent-repl--ws-put ws :nuked-at (plist-get plist :nuked-at))
          ;; Carry the hide marker so `agent-repl-toggle-hide-project-dirs'
          ;; can tell a hide-killed tombstone from a hand-nuked one and
          ;; restore only the former on unhide.
          (agent-repl--ws-put ws :hidden-project-dir
                               (plist-get plist :hidden-project-dir))
          (agent-repl--log ws "snapshot-load: restored tombstone ws=%s dir=%s hidden=%s"
                            ws (plist-get plist :project-dir)
                            (if (plist-get plist :hidden-project-dir) "t" "nil"))))
      (setq agent-repl--snapshot-load-state
            (list :queue queue
                  :origin origin-ws
                  :awaiting nil
                  :loaded 0
                  :skipped 0
                  :load-error 0
                  :total (length queue)
                  :timeout-timer nil
                  :saved-merge-queue saved-mq
                  :saved-in-flight-merges saved-ifm))
      (add-hook 'agent-repl-ws-fully-loaded-functions
                #'agent-repl--snapshot-load-on-loaded)
      (agent-repl--log nil
                        "snapshot-load: BEGIN file=%s entries=%d merge-queue=%d origin-ws=%s"
                        file (length queue) (length saved-mq) (or origin-ws "nil"))
      (agent-repl--snapshot-load-step))))

(defun agent-repl--load-workspace-snapshot-on-startup ()
  "Restore the workspace snapshot silently at Emacs startup.
Does nothing if neither the configured snapshot file nor its legacy
fallback is present.  Errors are logged but never propagated, so a
corrupt snapshot can't block startup."
  (when (file-exists-p (agent-repl--workspace-snapshot-file-for-read))
    (condition-case err
        (agent-repl-load-workspace-snapshot)
      (error (agent-repl--warn nil "snapshot load failed: %S" err)))))

;;;; Workspace snapshot archive picker

(defun agent-repl--snapshot-file-ws-count (file)
  "Return the number of workspace entries in snapshot FILE, or 0 on error.
Reads via `agent-repl--read-workspace-snapshot' so both the current
plist format and the legacy list-of-entries shape report the workspace
roster length (not the wrapping plist's length)."
  (or (ignore-errors
        (length (plist-get (agent-repl--read-workspace-snapshot file)
                           :workspaces)))
      0))

(defun agent-repl--snapshot-file-mtime-string (file)
  "Return FILE's last-modified time as a short `YYYY-MM-DD HH:MM' string."
  (format-time-string "%Y-%m-%d %H:%M"
                      (nth 5 (file-attributes file))))

(defun agent-repl--snapshot-candidate-label (file)
  "Format the completing-read label for snapshot FILE.
Layout: `<basename>  <count>ws  <mtime>'.  Basename is padded so the
count/date columns align across candidates in ivy/vertico."
  (format "%-32s %3dws  %s"
          (file-name-nondirectory file)
          (agent-repl--snapshot-file-ws-count file)
          (agent-repl--snapshot-file-mtime-string file)))

(defun agent-repl--snapshot-archive-candidates ()
  "Return an alist of (LABEL . PATH) for the current snapshot + archives.
Current file first; archives newest-first by filename (the archive
filename is a timestamp, so lexicographic sort works)."
  (let* ((current (agent-repl--workspace-snapshot-file-for-read))
         (archive-dir (agent-repl--workspace-snapshot-archive-dir))
         (archives (and (file-directory-p archive-dir)
                        (sort (directory-files archive-dir t "\\.el\\'" t)
                              #'string>)))
         (paths (cl-remove-duplicates
                 (cl-remove-if-not #'file-exists-p
                                   (cons current archives))
                 :test #'equal)))
    (mapcar (lambda (p)
              (cons (agent-repl--snapshot-candidate-label p) p))
            paths)))

(defun agent-repl-load-workspace-snapshot-from-archive ()
  "Pick a snapshot file (current or archived) and load it.
Candidates are annotated with workspace count and last-modified time
so the user can identify the right archive by size + recency.  Loads
via `agent-repl-load-workspace-snapshot' with the chosen file passed
explicitly (skips the configured-vs-legacy resolver)."
  (interactive)
  (let* ((candidates (agent-repl--snapshot-archive-candidates))
         (_ (unless candidates
              (user-error "No snapshot files found (no current file, no archives)")))
         (choice (completing-read "Load workspace snapshot: "
                                  (mapcar #'car candidates) nil t))
         (file (cdr (assoc choice candidates))))
    (when file
      (agent-repl--log nil "load-workspace-snapshot-from-archive: file=%s" file)
      (agent-repl-load-workspace-snapshot file))))

;;;; Merge-queue manual drain

(defun agent-repl-drain-merge-queue ()
  "Re-kick the merge-queue drain loop after a manually-resolved stall.

Normal flow: `agent-repl--workspace-merge-do' completes (success or
failure) and immediately calls `agent-repl--drain-merge-queue', so the
queue drains naturally as cherry-picks finish.  This command is the
escape hatch for stalls where that automatic drain didn't happen — for
example, when Emacs restarts with a non-empty queue restored from the
on-disk snapshot, or when a cherry-pick fails in a way that requires
the user to repair the worktree by hand before the next queued merge
can proceed.

Clears `:halt-until-human' on the FRONT entry of every per-target+repo
bucket before draining — `agent-repl--reenqueue-merge-on-failure' sets
that flag on generic failures specifically so auto-drain does NOT retry
them.  The interactive kick IS the human signal that re-dispatch should
proceed, so the flag is dropped on each bucket's front and those entries
become drainable.

No-op (with a `message') when the queue is empty.  Does NOT block when a
cherry-pick is in progress: the drain is now per-target, so buckets whose
target worktree has a live cherry-pick are simply skipped while free
buckets drain — the user no longer has to clear every `CHERRY_PICK_HEAD'
in the session before any queued merge can proceed.

The drain itself is the same `agent-repl--drain-merge-queue' that the
automatic path uses: each free bucket's front entry pops, the
corresponding `agent-repl--workspace-merge-into-source' runs, and its
completion cascades into the next drain."
  (interactive)
  (cond
   ((not (boundp 'agent-repl--merge-queue))
    (user-error "agent-repl: merge queue module not loaded"))
   ((null agent-repl--merge-queue)
    (message "[agent-repl] merge queue is empty — nothing to drain"))
   (t
    (dolist (target-dir (agent-repl--merge-queue-target-dirs))
      (let ((front (agent-repl--merge-queue-front-for-target target-dir)))
        (when (and front (plist-get front :halt-until-human))
          (agent-repl--log nil
                            "drain-merge-queue: manual kick clearing :halt-until-human on ws=%s target=%s"
                            (plist-get front :source-ws) (or target-dir "nil"))
          (plist-put front :halt-until-human nil))))
    (agent-repl--log nil
                      "drain-merge-queue: manual kick queue-len=%d"
                      (length agent-repl--merge-queue))
    (agent-repl--info nil "draining merge queue (%d entries)"
                      (length agent-repl--merge-queue))
    (agent-repl--drain-merge-queue))))

(defalias '+dwc/drain-merge-queue #'agent-repl-drain-merge-queue)

;;;; Merge-completed restore

(defun agent-repl--state-merge-completed-p (dir)
  "Return non-nil when the state.el under DIR has :merge-completed t.
Used by the snapshot loader to route merged-completed workspaces away
from `--establish-workspace' (which would re-create a persp and start
Claude) and into the lightweight `--register-merged-workspace' path.
Errors during the state-file read return nil so a malformed file
falls through to the normal establish path rather than blocking
startup."
  (when-let* ((state-file (and dir (agent-repl--state-file-for-read dir))))
    (and (file-exists-p state-file)
         (condition-case err
             (eq (plist-get (agent-repl--read-sexp-file state-file)
                            :merge-completed)
                 t)
           (error
            (agent-repl--log nil "state-merge-completed-p: read err file=%s err=%S"
                              state-file err)
            nil)))))

(defun agent-repl--register-merged-workspace (ws dir)
  "Register WS as a merged-completed workspace from on-disk state.
Reads DIR's state.el and populates `agent-repl--workspaces' with
just enough state for the drawer's MERGED bucket to render WS and for
`--finish-workspace' to later remove the worktree.  Does NOT create a
Doom persp and does NOT start Claude — the workspace is data-only
until the user presses `x' on its drawer entry.

Idempotent: a subsequent call overwrites the relevant plist fields.
Keys populated when present in the state file:
  :project-dir, :priority, :source-ws-dir, :last-prompt-summary,
  :last-prompt-time, :worktree-p, :merge-completed,
  :merge-completed-at, :merge-failed.

Runs `agent-repl--detect-merge-actually-landed-p' against git reality
to reclassify pre-:merge-failed workspaces: under the old flow, a
silent cherry-pick failure still wrote `:merge-completed t' without a
`:merge-failed' flag.  On load, the probe reads the parent worktree's
HEAD log for cherry-pick -x annotations of every target-branch commit;
any missing commit promotes the saved state to `:merge-failed t' /
`:repl-state :merge-failed' so the drawer surfaces the ❌ badge for
the first time.  Clean merges set `:repl-state :merged' so the 🔀
badge re-appears post-restart (the snapshot loader does not pass
through `--initialize-ws-env' for merged entries)."
  (agent-repl--with-error-logging (format "register-merged-workspace[%s]" ws)
    (let* ((state-file (agent-repl--state-file-for-read dir))
           (saved (and state-file
                       (file-exists-p state-file)
                       (condition-case err
                           (agent-repl--read-sexp-file state-file)
                         (error
                          (agent-repl--log ws "register-merged: state-read err err=%S" err)
                          nil)))))
      (agent-repl--log ws "register-merged: ws=%s dir=%s saved=%s"
                        ws dir (if saved "yes" "no"))
      ;; Merged workspaces are a re-registration path; clear any prior
      ;; tombstone so `--ws-live-p' agrees the entry is back in play.
      (agent-repl--ws-put ws :nuked-at nil)
      (agent-repl--ws-put ws :project-dir dir)
      (agent-repl--ws-put ws :merge-completed t)
      (when saved
        (dolist (key '(:priority :source-ws-dir :last-prompt-summary
                       :last-prompt-time :worktree-p :merge-completed-at))
          (when-let ((v (plist-get saved key)))
            (agent-repl--ws-put ws key v))))
      (let* ((saved-failed (and saved (eq (plist-get saved :merge-failed) t)))
             (landed (agent-repl--detect-merge-actually-landed-p ws))
             (failed (or saved-failed (not landed))))
        (agent-repl--log ws "register-merged: ws=%s saved-failed=%s landed=%s -> failed=%s"
                          ws saved-failed landed failed)
        (agent-repl--ws-put ws :merge-failed (when failed t))
        (agent-repl--ws-put ws :repl-state
                             (if failed :merge-failed :merged))))))

;;;; Project-switch wrapper

(defvar recentf-list)

(defun agent-repl--record-last-file-visit ()
  "Cache the visited file as `:last-file' on the current workspace plist.
Called from `find-file-hook'; no-op when the visited buffer has no
file, when there is no registered current workspace, or when the file
is not inside the workspace's project directory.

Keeps the `:last-file' cache fresh at zero per-switch cost so
`--most-recent-project-file' can satisfy the warm path with a plist
lookup instead of a linear `recentf-list' scan."
  (when-let* ((file buffer-file-name)
              (ws (agent-repl--ws-current-name))
              ((gethash ws agent-repl--workspaces))
              (project-dir (ignore-errors (agent-repl--ws-dir ws)))
              ((file-in-directory-p file project-dir)))
    (agent-repl--ws-put ws :last-file file)))

(add-hook 'find-file-hook #'agent-repl--record-last-file-visit)

(defun agent-repl--most-recent-project-file (project-root)
  "Return the most-recently-accessed file under PROJECT-ROOT, or nil.
Uses `file-in-directory-p' (boundary-aware) rather than
`string-prefix-p' — the latter would mis-match `/p/foo' against a
file under `/p/foo-bar/' because the prefix isn't terminated at a
path separator.

Prefers the `:last-file' key on the live workspace for PROJECT-ROOT —
populated by `agent-repl--record-last-file-visit' whenever a file is
opened — so the common warm-path is a zero-syscall plist lookup.
Falls back to scanning `recentf-list' on the cold path (no live
workspace or no `:last-file' cached yet).

Callers must still verify the returned path exists before opening —
both the plist cache and `recentf-list' lag filesystem deletions."
  (when project-root
    (or
     ;; Warm path: plist cache populated by find-file-hook — zero syscalls
     ;; until the file-exists-p guard, which is a single stat vs. the O(N)
     ;; scan below.
     (when-let* ((ws (agent-repl--ws-name-for-dir project-root))
                 (cached (agent-repl--ws-get ws :last-file))
                 ((file-exists-p cached)))
       cached)
     ;; Cold path: scan recentf for first-time project opens / no live ws.
     (seq-find (lambda (file)
                 (and (file-exists-p file)
                      (file-in-directory-p file project-root)))
               (bound-and-true-p recentf-list)))))

;;;; Project picker (SPC p p)
;;
;; `agent-repl-switch-to-project' replaces the plain
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

(defface agent-repl-picker-created-face
  '((t :inherit font-lock-comment-face))
  "Face for the creation-date column in `agent-repl-switch-to-project'."
  :group 'agent-repl)

(defface agent-repl-picker-killed-face
  '((t :inherit error))
  "Face for the last-kill-date column in `agent-repl-switch-to-project'."
  :group 'agent-repl)

(defface agent-repl-picker-name-face
  '((t :inherit default))
  "Face for the project-name column in `agent-repl-switch-to-project'."
  :group 'agent-repl)

(defconst agent-repl--picker-date-format "%Y-%m-%d"
  "`format-time-string' template for the picker's date columns.")

(defconst agent-repl--picker-date-width 10
  "Width of each date column in the picker (matches
`agent-repl--picker-date-format').  Used to keep the placeholder
\"--\" aligned with real dates.")

(defconst agent-repl--picker-name-min-width 24
  "Minimum padding width for the project-name column in the picker.
Actual width is the max of this and the longest candidate basename.")

(defconst agent-repl--picker-column-gap "   "
  "Whitespace inserted between the picker's name and date columns.")

(defun agent-repl--project-has-live-workspace-p (project-root)
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
               agent-repl--workspaces)
      found)))

(defun agent-repl--project-state-summary (project-root)
  "Return a plist summarizing PROJECT-ROOT's in-memory workspace state.

Sources values exclusively from `agent-repl--workspaces' (the live
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
                             (agent-repl--ws-name-for-dir project-root))))
    (list :created-at (and workspace-name
                           (agent-repl--ws-get workspace-name :created-at))
          :last-killed-at (and workspace-name
                               (agent-repl--ws-get workspace-name :last-killed-at))
          :priority (and workspace-name
                         (agent-repl--ws-get workspace-name :priority))
          :live-p (not (null workspace-name))
          :workspace-name workspace-name)))

(defun agent-repl--picker-status-emoji (summary)
  "Return the status-emoji prefix for a candidate with SUMMARY.
SUMMARY is a plist from `agent-repl--project-state-summary'.

When `:workspace-name' is non-nil, delegates to
`agent-repl--ws-render-status' for the render-state keyword and
looks up the glyph in `agent-repl-drawer-state-icons'.  This is the
same path the drawer's `--state-glyph' takes, so the project picker
(`SPC p p') and the drawer always agree on the emoji for a given
workspace.  Calling render-status directly (instead of going through
`agent-repl-drawer--state-glyph') keeps the picker free of drawer
internals.

For projects without a live workspace returns a neutral 📁.  No
historical kill/dormant distinction is drawn because the picker
deliberately avoids disk I/O — the on-disk state file is NOT read on
every invocation."
  (let ((ws (plist-get summary :workspace-name)))
    (if (and ws (agent-repl--ws-known-p ws))
        (or (alist-get (agent-repl--ws-render-status ws)
                       agent-repl-drawer-state-icons)
            agent-repl-drawer-state-icon-default)
      "📁")))

(defun agent-repl--picker-format-date (time width face placeholder)
  "Return a propertized fixed-width date string for TIME.
Format via `agent-repl--picker-date-format', then `truncate-string-to-width'
PLACEHOLDER (the dashes shown when TIME is nil) to WIDTH so the column
lines up regardless of whether TIME is set.  FACE is applied to the
result."
  (let ((str (if time
                 (format-time-string agent-repl--picker-date-format time)
               (truncate-string-to-width placeholder width 0 ?\s))))
    (propertize str 'face face)))

(defun agent-repl--picker-name-width (project-roots)
  "Return the padding width to use for the project-name column.
Max of `agent-repl--picker-name-min-width' and the longest basename
across PROJECT-ROOTS so every row's date columns start at the same
character position."
  (let ((max-basename
         (apply #'max 0
                (mapcar (lambda (p)
                          (length (file-name-nondirectory
                                   (directory-file-name p))))
                        project-roots))))
    (max agent-repl--picker-name-min-width max-basename)))

(defun agent-repl--picker-sort-key (summary)
  "Return the `:last-killed-at' time for SUMMARY, or its `:created-at'.
Used as the sort key so most-recently-killed projects surface first, with
never-killed projects falling back to creation-date order (also most-recent
first).  Returns nil when neither timestamp is available; callers treat
nil keys as oldest."
  (or (plist-get summary :last-killed-at)
      (plist-get summary :created-at)))

(defun agent-repl--picker-time-greater-p (a b)
  "Compare two `current-time'-shaped values: non-nil A newer than nil B."
  (cond ((and a b) (time-less-p b a))
        (a t)
        (t nil)))

(defun agent-repl--build-project-picker-candidates (project-roots)
  "Return a sorted alist of (display-string . project-root) for PROJECT-ROOTS.

Each entry's display string prefixes a status emoji, then a name column
padded to a width derived from the longest basename, then two date
columns (creation date, last kill/nuke date) separated by
`agent-repl--picker-column-gap'.  Empty date placeholders keep the
columns aligned across all rows.

Sort order: most-recently-killed first; never-killed projects sort by
creation date (most-recent first).  This is the input
`projectile-completing-read' / `ivy-read' receives."
  (let* ((name-width (agent-repl--picker-name-width project-roots))
         (entries (mapcar
                   (lambda (root)
                     (let* ((basename (file-name-nondirectory
                                       (directory-file-name root)))
                            (summary (agent-repl--project-state-summary root)))
                       (list :root root
                             :basename basename
                             :summary summary)))
                   project-roots))
         (sorted (sort entries
                       (lambda (a b)
                         (agent-repl--picker-time-greater-p
                          (agent-repl--picker-sort-key (plist-get a :summary))
                          (agent-repl--picker-sort-key (plist-get b :summary)))))))
    (mapcar
     (lambda (entry)
       (let* ((root (plist-get entry :root))
              (basename (plist-get entry :basename))
              (summary (plist-get entry :summary))
              (emoji (agent-repl--picker-status-emoji summary))
              (name-padded
               (propertize
                (truncate-string-to-width basename name-width 0 ?\s)
                'face 'agent-repl-picker-name-face))
              (created (agent-repl--picker-format-date
                        (plist-get summary :created-at)
                        agent-repl--picker-date-width
                        'agent-repl-picker-created-face
                        "----------"))
              (killed (agent-repl--picker-format-date
                       (plist-get summary :last-killed-at)
                       agent-repl--picker-date-width
                       'agent-repl-picker-killed-face
                       "----------"))
              (display (concat emoji " "
                               name-padded
                               agent-repl--picker-column-gap
                               created
                               agent-repl--picker-column-gap
                               killed)))
         (cons display root)))
     sorted)))

(defun agent-repl--read-project-via-picker ()
  "Prompt for a project root with the rich column picker.
Returns the selected project root path (the cdr of the matched
candidate) — never the propertized display string.  Uses `ivy-read' when
available (it preserves text-property faces in the candidate list) and
falls back to `completing-read'.

Captures the choice via the action closure rather than `ivy-read''s
return value because ivy's return shape for cons-cell candidates varies
across versions (sometimes the cons, sometimes the car); the action
sees `c' in a consistent shape so we can normalize once."
  (let* ((roots (agent-repl--ws-known-projects))
         (candidates (agent-repl--build-project-picker-candidates roots))
         (selected nil))
    (if (fboundp 'ivy-read)
        (ivy-read "Switch to project: " candidates
                  :action (lambda (c)
                            (setq selected (cond ((consp c) (cdr c))
                                                 ((stringp c)
                                                  (cdr (assoc c candidates)))
                                                 (t c))))
                  :require-match t
                  :caller 'agent-repl-switch-to-project)
      (let* ((choice (completing-read "Switch to project: "
                                      (mapcar #'car candidates)
                                      nil t))
             (hit (assoc choice candidates)))
        (setq selected (and hit (cdr hit)))))
    selected))

(defun agent-repl-switch-to-project (&optional project)
  "Switch to PROJECT and hydrate the workspace's priority badge.
PROJECT is a project root path; when nil, prompt via
`agent-repl--read-project-via-picker' (rich column view sorted by
last-kill / creation date).

Switches via `projectile-switch-project-by-name' (which fires Doom's
`+workspaces-switch-to-project-h' to create/activate the persp keyed
on the project basename), then opens the most-recently-accessed file
under PROJECT via `agent-repl--most-recent-project-file', hydrates
the saved display state (`:priority' and the drawer badges) from the
per-project state file and reseats the workspace into its priority
slot — both via the shared `agent-repl--hydrate-and-reorder-on-open'
step, so the tabline badge appears immediately on `SPC p p' (instead
of only once Claude starts) and the workspace lands in priority order
just like the snapshot/worktree restore path — and flashes the
activated tab.

Distinct from `agent-repl--switch-to-workspace': that primitive is
name-keyed and assumes the persp already exists; this one is
project-keyed and creates the persp via the Doom hook.  Both differ
from `agent-repl--establish-workspace', which is a snapshot-restore
path that bypasses the Doom hook to preserve the snapshot's exact ws
name."
  (interactive)
  (let ((project (or project (agent-repl--read-project-via-picker))))
    (when project
      (agent-repl--ws-switch-project project)
      ;; Defer the file open and display-state disk read so the persp switch
      ;; completes and Emacs redraws before any blocking I/O fires.  Both are
      ;; deferred together in one timer so they run in order on the same idle
      ;; cycle rather than racing across two separate timers.
      (run-at-time 0 nil
                   (lambda ()
                     (when-let ((recent-file (agent-repl--most-recent-project-file project)))
                       (when (file-exists-p recent-file)
                         (find-file recent-file)))
                     ;; Hydrate the priority badge then reseat the just-opened
                     ;; ws into its priority slot via the shared opener step, so
                     ;; `SPC p p' lands the workspace in priority order exactly
                     ;; like the snapshot/worktree restore path does.
                     (agent-repl--hydrate-and-reorder-on-open
                      (ignore-errors (agent-repl--ws-current-name))
                      project)))
      (agent-repl--flash-current-tab))))

;;;; Workspace cycling (hide-mode aware)

(defun agent-repl--workspace-cycle (n)
  "Cycle N workspaces (negative = left, positive = right).
Reimplements `+workspace/cycle' but iterates the visible workspace
list instead of the raw `+workspace-list-names': first the tab-bar list
\(`agent-repl--ws-tabline-names', which drops the workspaces of repos
folded in the drawer), then the hide-mode filter
\(`agent-repl--filter-hidden-names'), so both folded-repo workspaces and
closed-REPL workspaces dropped from the tabline are skipped during
s-{ / s-}.  Mirrors Doom's protected-workspace handling: when current
is the nil-persp, switch to `+workspaces-main' instead of cycling.
Does NOT flash the destination tab — left/right cycling is
high-frequency navigation and the flash becomes noise; identity-based
jumps (`SPC p p', priority change, worktree jump) keep the flash since
they're discrete attention cues."
  (let ((current-name (agent-repl--ws-current-name)))
    (if (agent-repl--ws-protected-p current-name)
        (agent-repl--ws-switch (agent-repl--ws-main-name) t)
      (condition-case-unless-debug ex
          (let* ((visible (agent-repl--filter-hidden-names
                           (agent-repl--ws-tabline-names) current-name))
                 (perspc (length visible))
                 (index (cl-position current-name visible :test #'equal)))
            (when (= perspc 1)
              (user-error "No other workspaces"))
            (agent-repl--ws-switch (nth (mod (+ index n) perspc) visible)))
        ('user-error (agent-repl--ws-error (cadr ex) t))
        ('error (agent-repl--ws-error ex t))))))

(defun agent-repl-switch-left ()
  "Cycle one workspace left, skipping hide-mode-filtered workspaces.
Drop-in replacement for `+workspace/switch-left' that honors
`agent-repl-hide-mode-enabled'."
  (interactive)
  (agent-repl--workspace-cycle -1))

(defun agent-repl-switch-right ()
  "Cycle one workspace right, skipping hide-mode-filtered workspaces.
Drop-in replacement for `+workspace/switch-right' that honors
`agent-repl-hide-mode-enabled'."
  (interactive)
  (agent-repl--workspace-cycle +1))

;;;; Indexed workspace switchers (first nine: SPC 1-9 / Cmd s-1..s-9; second nine: Opt M-1..M-9)
;;
;; Thin persp wrappers around `+workspace-switch' used in place of the
;; Doom `+workspace/switch-to-N' / `+workspace/switch-to-final' commands
;; for the workspace-jump bindings.  `switch-to-0'..`switch-to-8' back
;; the FIRST nine workspaces (bound to `SPC 1-9' and Cmd `s-1..s-9');
;; `switch-to-9'..`switch-to-17' back the SECOND nine (bound to Option
;; `M-1..M-9', whose key digits 1-9 address workspaces 10-18).  They were
;; extracted to make the workspace-jump bindings ignore
;; `current-prefix-arg' entirely — Doom's `+workspace/switch-to' inspects
;; `current-prefix-arg' in its `interactive' form, which sporadically
;; caused M-9 to land on the final workspace (when the previous key
;; sequence had set a prefix arg) instead of the intended one, and M-0 to
;; fall through to a no-op `text-scale-set' with the "The font hasn't
;; been resized" message.  These wrappers take no prefix argument and
;; call `+workspace-switch' directly by name, so the behaviour is
;; deterministic.
;;
;; `agent-repl--workspace-switch-by-index' is the shared core; the
;; named commands below are the only entry points bound to keys.

(defun agent-repl--workspace-switch-by-index (index)
  "Switch to workspace at zero-based INDEX in `agent-repl--ws-tabline-names'.
Signals `user-error' if INDEX is out of range.  Pure persp wrapper —
does not consult `current-prefix-arg' and does not flash the tab.

Indexes the TAB-BAR list (`--ws-tabline-names'), not the raw workspace
list, so a repo folded in the drawer takes its workspaces out of the
numbering entirely and the remaining numbers stay contiguous — `SPC 3'
always lands on the third tab the user can actually see."
  (let* ((names (agent-repl--ws-tabline-names))
         (dest (nth index names)))
    (unless dest
      (user-error "No workspace at #%s" (1+ index)))
    (agent-repl--ws-switch dest)))

(defun agent-repl-workspace-switch-to-0 ()
  "Switch to the 1st workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 0))

(defun agent-repl-workspace-switch-to-1 ()
  "Switch to the 2nd workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 1))

(defun agent-repl-workspace-switch-to-2 ()
  "Switch to the 3rd workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 2))

(defun agent-repl-workspace-switch-to-3 ()
  "Switch to the 4th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 3))

(defun agent-repl-workspace-switch-to-4 ()
  "Switch to the 5th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 4))

(defun agent-repl-workspace-switch-to-5 ()
  "Switch to the 6th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 5))

(defun agent-repl-workspace-switch-to-6 ()
  "Switch to the 7th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 6))

(defun agent-repl-workspace-switch-to-7 ()
  "Switch to the 8th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 7))

(defun agent-repl-workspace-switch-to-8 ()
  "Switch to the 9th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 8))

;; Second nine (Option `M-1'..`M-9').  The key digits 1-9 address
;; workspaces 10-18, so `M-1' -> `switch-to-9' (10th) .. `M-9' ->
;; `switch-to-17' (18th).  Same thin-wrapper contract as the first nine.
(defun agent-repl-workspace-switch-to-9 ()
  "Switch to the 10th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 9))

(defun agent-repl-workspace-switch-to-10 ()
  "Switch to the 11th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 10))

(defun agent-repl-workspace-switch-to-11 ()
  "Switch to the 12th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 11))

(defun agent-repl-workspace-switch-to-12 ()
  "Switch to the 13th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 12))

(defun agent-repl-workspace-switch-to-13 ()
  "Switch to the 14th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 13))

(defun agent-repl-workspace-switch-to-14 ()
  "Switch to the 15th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 14))

(defun agent-repl-workspace-switch-to-15 ()
  "Switch to the 16th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 15))

(defun agent-repl-workspace-switch-to-16 ()
  "Switch to the 17th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 16))

(defun agent-repl-workspace-switch-to-17 ()
  "Switch to the 18th workspace.  Thin wrapper, ignores prefix arg."
  (interactive)
  (agent-repl--workspace-switch-by-index 17))

(defun agent-repl-workspace-switch-to-final ()
  "Switch to the final (last) VISIBLE workspace.  Ignores prefix arg.
Reads `agent-repl--ws-tabline-names', so the destination is the last
tab actually rendered — workspaces of a folded repo are not candidates."
  (interactive)
  (let* ((names (agent-repl--ws-tabline-names))
         (dest (car (last names))))
    (unless dest
      (user-error "No workspaces"))
    (agent-repl--ws-switch dest)))

;;;; Workspace tab-order shuffles
;;
;; These rearrange `persp-names-cache' (the order persp-mode uses to
;; render the tab-bar) without touching the active workspace's identity.
;; They are agent-repl module commands rather than user-config defuns
;; so the workspace-merge reload picks up changes here, and so the
;; module's `agent-repl--on-close' deprio path (panels.el) can call
;; them directly without an `fboundp' user-config guard.

(defun agent-repl-workspace-push-to-back (&optional keep-focus)
  "Push the current workspace to the second-to-last position in the tab-bar.
By default switches focus to the workspace that now occupies the old
slot — the `SPC TAB p' UX, where the user keeps navigating the slot
they were sitting on.  When KEEP-FOCUS is non-nil, focus stays on the
moved workspace; this is what the on-close auto-deprio path wants,
since the user just closed claude in this workspace and shouldn't get
yanked away from it.  Pulses the moved tab via `agent-repl-flash-tab'
so the user can visually track it to its new home."
  (interactive)
  (let* ((current (agent-repl--ws-current-name))
         (names (agent-repl--ws-frame-ordered-names))
         (old-index (cl-position current names :test #'string=))
         (without-current (remove current names))
         (reordered (append (butlast without-current)
                            (list current)
                            (last without-current)))
         (next-name (nth (min old-index (1- (length without-current)))
                         without-current)))
    (agent-repl--log current "workspace-push-to-back: ws=%s old-index=%s next=%s keep-focus=%s"
                      current old-index next-name keep-focus)
    (agent-repl--ws-update-names-cache reordered)
    (agent-repl--force-tab-bar-redraw)
    (when (and next-name (not keep-focus))
      (agent-repl--ws-switch next-name))
    (agent-repl-flash-tab current)
    (if keep-focus
        (message "Pushed '%s' to second-to-last." current)
      (message "Pushed '%s' to second-to-last; switched to '%s'."
               current (or next-name current)))))

(defun agent-repl-workspace-pull-to-front ()
  "Pull the current workspace to the second position in the tab-bar.
Focus remains on the current workspace."
  (interactive)
  (let* ((current (agent-repl--ws-current-name))
         (names (agent-repl--ws-frame-ordered-names))
         (without-current (remove current names))
         (reordered (append (list (car without-current))
                            (list current)
                            (cdr without-current))))
    (agent-repl--log current "workspace-pull-to-front: ws=%s" current)
    (agent-repl--ws-update-names-cache reordered)
    (agent-repl--force-tab-bar-redraw)
    (agent-repl--ws-switch current)
    (message "Pulled '%s' to second position." current)))

(defun agent-repl-open-most-recent-workspace ()
  "Switch to the most recently visited workspace not yet opened by this command.
Each call returns a different workspace, cycling through
`agent-repl--workspace-history'.  When all workspaces have been visited,
resets the cycle.  Falls back to the full workspace list when history is
empty (e.g. a fresh session)."
  (interactive)
  (let* ((current (agent-repl--ws-current-name))
         (candidates (cl-remove-if
                      (lambda (name)
                        (or (string= name current)
                            (member name agent-repl--opened-recent-workspaces)))
                      agent-repl--workspace-history))
         (candidates (or candidates
                         (cl-remove-if
                          (lambda (name)
                            (or (string= name current)
                                (member name agent-repl--opened-recent-workspaces)))
                          (agent-repl--ws-all-names))))
         (target (car candidates)))
    (if target
        (progn
          (push target agent-repl--opened-recent-workspaces)
          (agent-repl--ws-switch target))
      (setq agent-repl--opened-recent-workspaces nil)
      (message "All workspaces visited — cycle reset"))))
