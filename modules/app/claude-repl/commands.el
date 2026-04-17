;;; commands.el --- user commands for claude-repl -*- lexical-binding: t; -*-

;;; Code:

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

;;;; Session helpers

(defun claude-repl--ensure-session (&optional ws)
  "Ensure a Claude session exists (vterm + input + overlay) for WS.
WS defaults to the current workspace name.  No-op if already running."
  (let ((ws (or ws (+workspace-current-name))))
    (unless ws (error "claude-repl--ensure-session: no active workspace"))
    (if (claude-repl--vterm-running-p ws)
        (claude-repl--log ws "ensure-session: already running for ws=%s" ws)
      (progn
        (claude-repl--log ws "ensure-session: starting new session for ws=%s" ws)
        (claude-repl--ensure-vterm-buffer ws)
        (claude-repl--ensure-input-buffer ws)
        (claude-repl--ws-put ws :prefix-counter 0)
        (claude-repl--enable-hide-overlay)))))

(defun claude-repl--send-to-claude (text)
  "Send TEXT to Claude, starting it if needed."
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "send-to-claude len=%d" (length text))
    (claude-repl--ensure-session)
    (claude-repl--send-input-to-vterm
     (claude-repl--ws-get ws :vterm-buffer) text)))

;;;; File reference helpers

(defun claude-repl--buffer-relative-path ()
  "Return the current buffer's file path relative to the project root."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (let ((rel (file-relative-name (claude-repl--path-canonical file) (claude-repl--resolve-root))))
      (claude-repl--log nil "buffer-relative-path: path=%s" rel)
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
          (claude-repl--log nil "format-file-ref: region branch start=%d end=%d" start-line end-line)
          (format "%s:%d-%d" rel start-line end-line))
      (claude-repl--log nil "format-file-ref: single-line branch line=%d" (line-number-at-pos (point)))
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
               (claude-repl--resolve-root)))
         (ref (format "%s:%d-%d" rel start end)))
    (claude-repl--log nil "format-magit-hunk-ref: ref=%s" ref)
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
        (claude-repl--log nil "context-reference: magit-hunk branch")
        (claude-repl--format-magit-hunk-ref))
    (claude-repl--log nil "context-reference: standard branch")
    (claude-repl--format-file-ref)))

;;;; Diff analysis infrastructure

(defun claude-repl--send-diff-analysis (change-spec prompt)
  "Send a diff analysis request to Claude.
CHANGE-SPEC describes which changes (e.g. \"unstaged changes (git diff)\").
PROMPT is the analysis instruction."
  (let ((msg (format "for the %s, %s" change-spec prompt)))
    (claude-repl--log nil "diff-analysis: %s" change-spec)
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
         (msg (format "please explain %s" ref)))
    (claude-repl--log nil "explain %s" msg)
    (claude-repl--send-to-claude msg)))

(defun claude-repl--send-interrupt-escape (ws vterm-buf)
  "Send two Escape key presses to VTERM-BUF to interrupt Claude.
WS is the current workspace name for logging."
  (claude-repl--log ws "send-interrupt-escape: sending escape to buf=%s" (buffer-name vterm-buf))
  (with-current-buffer vterm-buf
    (vterm-send-key "<escape>")
    (vterm-send-key "<escape>")))

(defun claude-repl--enter-insert-mode (vterm-buf)
  "Send \"i\" to VTERM-BUF to re-enter insert mode."
  (if (buffer-live-p vterm-buf)
      (progn
        (claude-repl--log nil "enter-insert-mode: sending i to buf=%s" (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-string "i")))
    (claude-repl--log nil "enter-insert-mode: buffer is dead, skipping")))

(defun claude-repl-interrupt ()
  "Interrupt Claude and re-enter insert mode after a short delay.
Sends Escape to stop the current operation, then automatically
sends \"i\" after 0.25s to return to insert mode."
  (interactive)
  (let ((ws (+workspace-current-name)))
    (claude-repl--log ws "interrupt")
    (if (claude-repl--vterm-live-p)
        (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
          (claude-repl--send-interrupt-escape ws vterm-buf)
          (run-at-time 0.25 nil #'claude-repl--enter-insert-mode vterm-buf))
      (claude-repl--log ws "interrupt: vterm not live, skipping"))))

(defun claude-repl-update-pr ()
  "Ask Claude to update the PR description for the current branch."
  (interactive)
  (claude-repl--log nil "update-pr: sending update-pr prompt")
  (claude-repl--send-to-claude claude-repl-update-pr-prompt))

(defun claude-repl--nuke-one-workspace (ws)
  "Tear down a single claude-repl workspace WS without prompting.
Kills any in-flight git-diff process, tears down the vterm session
and buffers, deletes the persp workspace (switching away first if WS
is current), and removes WS from `claude-repl--workspaces'.  Designed
to be reusable from both `claude-repl-nuke-workspace' (one-shot) and
`claude-repl-nuke-all-workspaces' (loop)."
  (claude-repl--log ws "nuke-one-workspace: ws=%s" ws)
  (when-let ((proc (claude-repl--ws-get ws :git-proc)))
    (when (process-live-p proc)
      (claude-repl--log ws "nuke-one-workspace: killing git-proc")
      (delete-process proc)))
  (condition-case err
      (claude-repl--kill-session ws)
    (error (claude-repl--log ws "nuke-one-workspace: kill-session error: %S" err)))
  (when (and (bound-and-true-p persp-mode)
             (persp-get-by-name ws))
    (when (string= ws (+workspace-current-name))
      (+workspace/other))
    (claude-repl--log ws "nuke-one-workspace: deleting persp workspace")
    (+workspace/delete ws))
  (claude-repl--ws-del ws))

(defun claude-repl-nuke-workspace ()
  "Completely destroy a claude-repl workspace: session, buffers, persp, and hashmap entry.
Prompts to select from workspaces registered in `claude-repl--workspaces',
defaulting to the current workspace when registered."
  (interactive)
  (let ((ws (claude-repl--read-known-workspace "Nuke workspace: ")))
    (unless (y-or-n-p (format "Nuke workspace '%s'? This kills processes, buffers, and removes all state. " ws))
      (user-error "Aborted"))
    (claude-repl--nuke-one-workspace ws)
    (force-mode-line-update t)
    (message "Nuked workspace: %s" ws)))

(defun claude-repl-nuke-all-workspaces ()
  "Completely destroy ALL claude-repl workspaces.
Iterates every workspace registered in `claude-repl--workspaces' and
applies the same teardown as `claude-repl-nuke-workspace' to each.
Prompts once with the count before proceeding."
  (interactive)
  (let* ((known (hash-table-keys claude-repl--workspaces))
         (count (length known)))
    (unless known (user-error "No claude-repl workspaces registered"))
    (unless (y-or-n-p (format "Nuke ALL %d claude-repl workspace(s)? This kills processes, buffers, and removes all state. "
                              count))
      (user-error "Aborted"))
    (claude-repl--log nil "nuke-all-workspaces: count=%d" count)
    ;; Snapshot keys before iterating; each call mutates the hash.
    (dolist (ws known)
      (claude-repl--nuke-one-workspace ws))
    (force-mode-line-update t)
    (message "Nuked %d workspace(s)" count)))

(defun claude-repl-copy-reference ()
  "Copy the current file and line reference to the clipboard.
With active region: copies file:startline-endline.
Without region: copies file:line."
  (interactive)
  (claude-repl--log nil "copy-reference: copying file reference")
  (let ((ref (claude-repl--format-file-ref)))
    (kill-new ref)
    (message "Copied: %s" ref)))
