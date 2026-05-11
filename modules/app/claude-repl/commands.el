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
          (run-at-time claude-repl-interrupt-reinsert-delay nil #'claude-repl--enter-insert-mode vterm-buf))
      (claude-repl--log ws "interrupt: vterm not live, skipping"))))

(defun claude-repl-update-pr ()
  "Ask Claude to update the PR description for the current branch."
  (interactive)
  (claude-repl--log (+workspace-current-name) "update-pr: sending update-pr prompt")
  (claude-repl--send-to-claude claude-repl-update-pr-prompt))

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

(cl-defun claude-repl--nuke-one-workspace (ws &key (purge-state t))
  "Tear down a single claude-repl workspace WS without prompting.
Kills any in-flight git-diff process, tears down the vterm session
and buffers, removes WS from `claude-repl--workspaces', kills every
remaining buffer (and attached process) that belongs to the persp via
`claude-repl--kill-workspace-buffers', and finally kills the persp
workspace via `+workspace/kill'.  Designed to be reusable from
`claude-repl-nuke-workspace' (one-shot),
`claude-repl-nuke-all-workspaces' (loop), and
`claude-repl-kill-workspace' (no state purge).

When PURGE-STATE is non-nil (the default — used by the nuke paths),
the per-project state file is also unlinked via
`claude-repl--state-purge'.  Pass :purge-state nil to keep the file so
priority and per-environment session-id survive — used by
`claude-repl-kill-workspace', which is intended to free in-memory
resources without losing persisted workspace identity.

The `:project-dir' is captured up front because `kill-session' runs
`teardown-session-state' which nils out buffer refs — the path would
be unreachable by the time we unlink the state files.

The hashmap removal (`ws-del') and (when applicable) state-file purge
run inside an `unwind-protect' cleanup so they always happen, even
when kill-session errors partway through.  The persp kill is the very
last step so all internal state is already cleaned up before the UI
workspace disappears.  Callers can rely on the post-condition: after
the call returns \(or throws), WS is not in
`claude-repl--workspaces'."
  (claude-repl--log ws "nuke-one-workspace: ENTRY ws=%s purge-state=%s cache=%S"
                    ws purge-state
                    (if (boundp 'persp-names-cache) persp-names-cache "(unbound)"))
  (let ((root (claude-repl--ws-get ws :project-dir)))
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
      ;; Cleanup: always remove the hashmap entry (and, for nukes, purge
      ;; persisted state), regardless of any error in the steps above.
      (condition-case err
          (claude-repl--ws-del ws)
        (error (claude-repl--log ws "nuke-one-workspace: ws-del error: %S" err)))
      (when purge-state
        (claude-repl--state-purge root))
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
      (condition-case err
          (when (and (bound-and-true-p persp-mode)
                     (persp-get-by-name ws))
            (claude-repl--log ws "nuke-one-workspace: pre-persp-kill ws=%s cache=%S"
                              ws persp-names-cache)
            (+workspace/kill ws)
            (claude-repl--log ws "nuke-one-workspace: post-persp-kill ws=%s in-cache=%s cache=%S"
                              ws (if (member ws persp-names-cache) "t" "nil") persp-names-cache))
        (error (claude-repl--log ws "nuke-one-workspace: workspace-kill error: %S" err))))))

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
    (claude-repl--log (+workspace-current-name) "nuke-all-workspaces: count=%d" count)
    ;; Snapshot keys before iterating; each call mutates the hash.
    (dolist (ws known)
      (claude-repl--nuke-one-workspace ws))
    (force-mode-line-update t)
    (message "Nuked %d workspace(s)" count)))

(defun claude-repl-kill-workspace ()
  "Kill a claude-repl workspace without destroying its persisted state.
Same teardown as `claude-repl-nuke-workspace' — kills the Claude
process, vterm/input buffers, every other buffer attached to the
persp, removes the workspace from `claude-repl--workspaces', and
kills the persp itself — but leaves the on-disk per-project state file (under
`.claude/emacs/' — see `claude-repl--state-file') untouched so
priority and per-environment session-id survive.  Use this when you want to free in-memory
resources but expect to re-open the workspace later and resume the
same Claude session.

Prompts to select from workspaces registered in
`claude-repl--workspaces', defaulting to the current workspace
when registered."
  (interactive)
  (let ((ws (claude-repl--read-known-workspace "Kill workspace: ")))
    (unless (y-or-n-p (format "Kill workspace '%s'? This kills processes and buffers but preserves priority/session-id on disk. " ws))
      (user-error "Aborted"))
    (claude-repl--nuke-one-workspace ws :purge-state nil)
    (force-mode-line-update t)
    (message "Killed workspace: %s" ws)))

;;;; Hide-mode sweep

(defun claude-repl--sweep-hidden-workspaces (&optional except)
  "Persp-kill every claude-repl workspace whose `:repl-state' is `:hidden'.
EXCEPT names a workspace to skip (typically the just-arrived destination
of a workspace switch — we don't want to kill the workspace the user is
currently sitting in).  Each match is torn down via
`claude-repl--nuke-one-workspace' with `:purge-state nil', preserving the
on-disk state file so the workspace can be re-opened via project switch.

No-op when there are no matching workspaces.  Returns the list of names
that were actually killed (useful for tests)."
  (let* ((current (or except (+workspace-current-name)))
         (candidates (cl-remove-if
                      (lambda (ws)
                        (or (equal ws current)
                            (not (eq (claude-repl--ws-repl-state ws) :hidden))))
                      (hash-table-keys claude-repl--workspaces))))
    (claude-repl--log current
                      "sweep-hidden-workspaces: except=%s candidates=%S"
                      current candidates)
    (dolist (ws candidates)
      (condition-case err
          (claude-repl--nuke-one-workspace ws :purge-state nil)
        (error
         (claude-repl--log ws "sweep-hidden-workspaces: kill error ws=%s err=%S"
                           ws err))))
    candidates))

(defun claude-repl--maybe-sweep-hidden-on-switch ()
  "Run `claude-repl--sweep-hidden-workspaces' when hide-mode is enabled.
Hooked into `claude-repl--on-workspace-switch' (panels.el).  Also resets
the just-arrived workspace's `:repl-state' from `:hidden' back to
`:inactive' if applicable, so navigating to a hidden workspace removes
its hidden flag (the user is actively viewing it; it should not be
killed)."
  (let ((current (+workspace-current-name)))
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

(defun claude-repl--snapshot-save-safe-p (live-count)
  "Return non-nil when save may proceed with LIVE-COUNT entries.
Safe iff loader already ran this session (`claude-repl--snapshot-loaded-p'
is t) OR the on-disk roster is no larger than LIVE-COUNT.  The latter
covers the fresh-install case where no prior file exists, is empty, or
is unreadable as a sexp — there's nothing to lose."
  (or claude-repl--snapshot-loaded-p
      (let* ((file claude-repl-workspace-snapshot-file)
             (on-disk (and (file-exists-p file)
                           (condition-case _
                               (claude-repl--read-sexp-file-if-exists file)
                             (error nil)))))
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

(defun claude-repl-save-workspace-snapshot ()
  "Save the current set of claude-repl workspaces to a hidden file.
Writes a list of (NAME :project-dir DIR) entries sourced from
`claude-repl--workspaces' (the live hash).

`:priority' is deliberately NOT included here — it lives in each
project's `<root>/.claude/emacs/state.el' (written by
`claude-repl--state-save', read by `claude-repl--initialize-ws-env'
and the fast-path `claude-repl--hydrate-priority-from-state').  Saving
it in the roster too would create two sources of truth.

Called interactively prints a confirmation; called from
`claude-repl--state-save' (the common path) stays silent so the
roster-piggyback save doesn't spam the echo area on every state mutation."
  (interactive)
  (let ((entries (make-hash-table :test 'equal)))
    (maphash (lambda (ws _plist)
               (when-let ((dir (claude-repl--ws-get ws :project-dir)))
                 (puthash ws (list :project-dir dir) entries)))
             claude-repl--workspaces)
    (let (snapshot)
      (maphash (lambda (ws plist) (push (cons ws plist) snapshot))
               entries)
      (if (not (claude-repl--snapshot-save-safe-p (length snapshot)))
          (claude-repl--log nil
                            "save-workspace-snapshot: ABORTED — loader hasn't run this session and on-disk roster is larger than live (%d)"
                            (length snapshot))
        (claude-repl--log nil "write-sexp-file: file=%s" claude-repl-workspace-snapshot-file)
        (let ((dir (file-name-directory claude-repl-workspace-snapshot-file)))
          (when (and dir (not (file-directory-p dir)))
            (make-directory dir t)))
        (claude-repl--archive-workspace-snapshot)
        (with-temp-file claude-repl-workspace-snapshot-file
          (insert "(")
          (let ((first t))
            (dolist (entry snapshot)
              (unless first (insert "\n "))
              (setq first nil)
              (prin1 entry (current-buffer))))
          (insert ")"))
        (when (called-interactively-p 'interactive)
          (message "Saved %d workspace(s) to %s"
                   (length snapshot) claude-repl-workspace-snapshot-file))))))

(defun claude-repl--clean-frame-foreign-windows (ws)
  "Delete frame windows whose buffer isn't in persp WS's buffer set.
Strips `no-delete-other-windows' and dedication from foreign windows
first so prior persps' claude-repl panel windows (which are marked
dedicated + `no-delete-other-windows') can be torn down.  When every
frame window is foreign, deletes all but one and swaps that one's
buffer to `doom-fallback-buffer' so the new persp starts with a clean
single-window layout instead of inheriting the previous persp's
window-configuration.  No-op when persp APIs are unavailable or the
persp has no recorded buffer set (test environments)."
  (when (and (fboundp 'persp-get-by-name)
             (fboundp 'persp-buffers))
    (let* ((persp (persp-get-by-name ws))
           (allowed (and persp (not (symbolp persp)) (persp-buffers persp)))
           (fallback (and (fboundp 'doom-fallback-buffer) (doom-fallback-buffer)))
           (all (window-list nil 'nomini))
           (foreign (cl-remove-if (lambda (w) (memq (window-buffer w) allowed)) all))
           (any-native (cl-some (lambda (w) (memq (window-buffer w) allowed)) all)))
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
            (set-window-buffer (car foreign) fallback))))))))

(defun claude-repl--establish-workspace (ws dir)
  "Synchronously create + activate + fully set up workspace WS for DIR.
Mirrors what `+dwc/switch-to-project' would do on an interactive
`SPC p p' but bypasses `+workspaces-switch-to-project-h' to avoid the
rename-on-empty collapse: persp is added directly via `persp-add-new'
and activated via `persp-frame-switch' keyed by the snapshot's `ws'
name (not the project basename Doom would otherwise derive).

Each call:

- creates the persp (`persp-add-new'),
- activates it on this frame (`persp-frame-switch') so persp-mode
  saves a clean window-configuration for the previous persp and
  starts capturing one for this one,
- scrubs foreign windows lingering from the prior persp via
  `claude-repl--clean-frame-foreign-windows' (dedicated panels from
  the previous workspace would otherwise survive into WS's frame),
- registers the project with projectile,
- sets `default-directory' on the fallback buffer + loads dir-locals,
- runs `+workspaces-switch-project-function' (the user lambda that
  auto-opens magit when the persp has no real buffers),
- opens the most-recent project file via `find-file' when one exists
  in `recentf-list',
- hydrates `:project-dir' into `claude-repl--workspaces' and
  rehydrates `:priority' from the per-project state file via
  `claude-repl--hydrate-priority-from-state',
- starts claude (`claude-repl--initialize-claude') unless already
  running."
  (claude-repl--with-error-logging (format "establish-workspace[%s]" ws)
    (when (fboundp 'persp-add-new)
      (persp-add-new ws))
    (when (fboundp 'persp-frame-switch)
      (persp-frame-switch ws))
    ;; Strip any window-configuration bleed-over from the prior persp before
    ;; populating WS — panels left over from the previous workspace are marked
    ;; `no-delete-other-windows', so the later `delete-other-windows' in
    ;; `--show-existing-panels' won't tear them down on its own.
    (claude-repl--clean-frame-foreign-windows ws)
    (when (fboundp 'projectile-add-known-project)
      (projectile-add-known-project dir))
    (when (fboundp 'doom-fallback-buffer)
      (with-current-buffer (doom-fallback-buffer)
        (setq default-directory (file-name-as-directory dir))
        (when (fboundp 'hack-dir-local-variables-non-file-buffer)
          (hack-dir-local-variables-non-file-buffer))))
    (when (and (boundp '+workspaces-switch-project-function)
               +workspaces-switch-project-function)
      (funcall +workspaces-switch-project-function dir))
    (when (fboundp '+dwc/get-most-recent-file-in-project)
      (when-let ((recent-file (+dwc/get-most-recent-file-in-project dir)))
        (when (file-exists-p recent-file)
          (find-file recent-file))))
    (claude-repl--ws-put ws :project-dir dir)
    (when (fboundp 'claude-repl--hydrate-priority-from-state)
      (claude-repl--hydrate-priority-from-state dir))
    (when (and (fboundp 'claude-repl--initialize-claude)
               (fboundp 'claude-repl--claude-running-p)
               (not (claude-repl--claude-running-p ws)))
      (claude-repl--initialize-claude ws))))

(defvar claude-repl--snapshot-load-state nil
  "Plist describing an in-progress recursive snapshot load, or nil.
Keys: `:queue' (list of (NORMALIZED-WS . PLIST) entries still to do),
`:origin' (workspace to switch back to at end), `:awaiting' (ws-name
the loader is currently waiting on a ready signal for, or nil),
`:loaded' / `:skipped' counters, `:total' (entry count from the file),
`:timeout-timer' (the per-entry watchdog timer).

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

(defun claude-repl--snapshot-load-finish ()
  "Finalize the recursive load: detach hook, return to origin, message."
  (remove-hook 'claude-repl-after-ready-functions
               #'claude-repl--snapshot-load-on-ready)
  (claude-repl--snapshot-load-cancel-timer)
  (let* ((state claude-repl--snapshot-load-state)
         (origin (plist-get state :origin))
         (loaded (plist-get state :loaded))
         (skipped (plist-get state :skipped)))
    (when (and origin
               (fboundp '+workspace-exists-p)
               (+workspace-exists-p origin)
               (fboundp 'persp-frame-switch))
      (persp-frame-switch origin))
    (force-mode-line-update t)
    (setq claude-repl--snapshot-loaded-p t)
    (claude-repl--log nil "snapshot-load: END loaded=%d skipped=%d returned-to=%s"
                      loaded skipped (or origin "nil"))
    (message "Loaded %d workspace(s), skipped %d" loaded skipped))
  (setq claude-repl--snapshot-load-state nil))

(defun claude-repl--snapshot-load-on-ready (ws)
  "After-ready hook: advance the snapshot load queue iff WS is awaited."
  (let ((state claude-repl--snapshot-load-state))
    (when (and state (equal ws (plist-get state :awaiting)))
      (claude-repl--log ws "snapshot-load: awaited ws=%s reported ready — advancing" ws)
      (claude-repl--snapshot-load-cancel-timer)
      (setq claude-repl--snapshot-load-state
            (plist-put claude-repl--snapshot-load-state :awaiting nil))
      (claude-repl--snapshot-load-step))))

(defun claude-repl--snapshot-load-timeout (ws)
  "Watchdog firing for WS — advance the queue with a warning."
  (let ((state claude-repl--snapshot-load-state))
    (when (and state (equal ws (plist-get state :awaiting)))
      (claude-repl--log ws "snapshot-load: TIMEOUT awaiting ws=%s — advancing anyway" ws)
      (message "[claude-repl] snapshot-load timeout awaiting ws=%s — advancing" ws)
      (setq claude-repl--snapshot-load-state
            (plist-put claude-repl--snapshot-load-state :awaiting nil))
      (setq claude-repl--snapshot-load-state
            (plist-put claude-repl--snapshot-load-state :timeout-timer nil))
      (claude-repl--snapshot-load-step))))

(defun claude-repl--snapshot-load-step ()
  "Process the next entry in the snapshot-load queue.
Called both at start and from the after-ready hook / timeout callback."
  (let* ((state claude-repl--snapshot-load-state)
         (queue (plist-get state :queue))
         (total (plist-get state :total))
         (iter  (1+ (+ (plist-get state :loaded) (plist-get state :skipped)))))
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
         (t
          (claude-repl--log nil "snapshot-load iter=%d/%d ws=%s dir=%s establishing"
                            iter total ws dir)
          (setq claude-repl--snapshot-load-state
                (plist-put claude-repl--snapshot-load-state :awaiting ws))
          (condition-case err
              (claude-repl--establish-workspace ws dir)
            (error
             (claude-repl--log nil "snapshot-load: establish-workspace err ws=%s err=%S" ws err)
             (message "[claude-repl] establish failed ws=%s — advancing" ws)))
          (setq claude-repl--snapshot-load-state
                (plist-put claude-repl--snapshot-load-state :loaded
                           (1+ (plist-get claude-repl--snapshot-load-state :loaded))))
          (cond
           ((claude-repl--snapshot-load-ws-ready-p ws)
            ;; Already ready (e.g. re-load, or claude was already up) —
            ;; advance immediately without waiting for a session_start.
            (claude-repl--log ws "snapshot-load: ws=%s already ready — advancing without waiting" ws)
            (setq claude-repl--snapshot-load-state
                  (plist-put claude-repl--snapshot-load-state :awaiting nil))
            (claude-repl--snapshot-load-step))
           (t
            ;; Arm the watchdog and yield to the main loop.  The after-
            ;; ready hook (or the watchdog) will call --snapshot-load-step
            ;; again.
            (setq claude-repl--snapshot-load-state
                  (plist-put claude-repl--snapshot-load-state :timeout-timer
                             (run-with-timer
                              claude-repl-snapshot-load-per-entry-timeout
                              nil
                              #'claude-repl--snapshot-load-timeout
                              ws))))))))))))

(defun claude-repl-load-workspace-snapshot (&optional file)
  "Load workspaces from FILE (defaults to the configured snapshot path).
When FILE is nil, reads `claude-repl-workspace-snapshot-file' (or its
legacy module-dir fallback if the configured file is absent).  For each
entry, fully sets up the workspace via `claude-repl--establish-workspace'
\(persp creation + activation + projectile + dir-locals + magit lambda
+ find-file recent + claude init).

Recursive queue driver: establishes one entry, then yields to the main
loop until that workspace's `claude-repl-after-ready-functions' hook
fires, then advances.  Per-entry watchdog
\(`claude-repl-snapshot-load-per-entry-timeout') guarantees forward
progress even if a ready signal goes missing.

Returns to the workspace that was active when the load began."
  (interactive)
  (when claude-repl--snapshot-load-state
    (user-error "claude-repl: a snapshot load is already in progress"))
  (let* ((file (or file (claude-repl--workspace-snapshot-file-for-read)))
         (snapshot (claude-repl--read-sexp-file-if-exists file)))
    (unless snapshot
      (user-error "No workspace snapshot at %s" file))
    (let ((queue (mapcar #'claude-repl--snapshot-entry-normalize snapshot))
          (origin-ws (and (fboundp '+workspace-current-name)
                          (ignore-errors (+workspace-current-name)))))
      (setq claude-repl--snapshot-load-state
            (list :queue queue
                  :origin origin-ws
                  :awaiting nil
                  :loaded 0
                  :skipped 0
                  :total (length queue)
                  :timeout-timer nil))
      (add-hook 'claude-repl-after-ready-functions
                #'claude-repl--snapshot-load-on-ready)
      (claude-repl--log nil "snapshot-load: BEGIN file=%s entries=%d origin-ws=%s"
                        file (length queue) (or origin-ws "nil"))
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
  "Return the number of workspace entries in snapshot FILE, or 0 on error."
  (or (ignore-errors
        (length (claude-repl--read-sexp-file-if-exists file)))
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

(defun claude-repl-switch-to-project (&optional project)
  "Switch to PROJECT and hydrate the workspace's priority badge.
Thin wrapper around `+dwc/switch-to-project' that, after switching,
reads the per-project state file from the project root (see
`claude-repl--state-file-for-read') and applies any saved
`:priority' to the new workspace, so the tabline badge appears
immediately on `SPC p p' instead of only after Claude starts.  Also
pulses the activated workspace's tab via `claude-repl-flash-tab' so
the user can spot the slot they just landed in — same flash semantic
as the close-deprio and reopen paths."
  (interactive)
  (let ((project (or project
                     (projectile-completing-read "Switch to project: "
                                                 (projectile-relevant-known-projects)))))
    (+dwc/switch-to-project project)
    (claude-repl--hydrate-priority-from-state project)
    (claude-repl--flash-current-tab)))

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
