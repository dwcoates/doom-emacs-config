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

(defun claude-repl--nuke-one-workspace (ws)
  "Tear down a single claude-repl workspace WS without prompting.
Kills any in-flight git-diff process, tears down the vterm session
and buffers, removes WS from `claude-repl--workspaces', purges the
per-project state file (see `claude-repl--state-purge'), kills every
remaining buffer (and attached process) that belongs to the persp via
`claude-repl--kill-workspace-buffers', and finally kills the persp
workspace via `+workspace/kill'.  Designed to be reusable from both
`claude-repl-nuke-workspace' (one-shot) and
`claude-repl-nuke-all-workspaces' (loop).

The `:project-dir' is captured up front because `kill-session' runs
`teardown-session-state' which nils out buffer refs — the path would
be unreachable by the time we unlink the state files.

The hashmap removal (`ws-del') and state-file purge run inside an
`unwind-protect' cleanup so they always happen, even when kill-session
errors partway through.  The persp kill is the very last step so all
internal state is already cleaned up before the UI workspace
disappears.  Callers can rely on the post-condition: after nuke
returns \(or throws), WS is not in `claude-repl--workspaces'."
  (claude-repl--log ws "nuke-one-workspace: ws=%s" ws)
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
      ;; Cleanup: always remove the hashmap entry and purge persisted state,
      ;; regardless of any error in the steps above.
      (condition-case err
          (claude-repl--ws-del ws)
        (error (claude-repl--log ws "nuke-one-workspace: ws-del error: %S" err)))
      (claude-repl--state-purge root)
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
            (claude-repl--log ws "nuke-one-workspace: killing persp workspace")
            (+workspace/kill ws))
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

(defun claude-repl-copy-reference ()
  "Copy the current file and line reference to the clipboard.
With active region: copies file:startline-endline.
Without region: copies file:line."
  (interactive)
  (claude-repl--log (+workspace-current-name) "copy-reference: copying file reference")
  (let ((ref (claude-repl--format-file-ref)))
    (kill-new ref)
    (message "Copied: %s" ref)))

;;;; Workspace snapshot save/load

(defcustom claude-repl-workspace-snapshot-file
  (expand-file-name ".workspace-snapshot.el"
                    (file-name-directory (or load-file-name
                                              buffer-file-name
                                              default-directory)))
  "Path to the hidden file where workspace snapshots are persisted.
Defaults to `.workspace-snapshot.el' in the claude-repl module directory."
  :type 'file
  :group 'claude-repl)

(defvar claude-repl--pending-snapshot-workspaces (make-hash-table :test 'equal)
  "Workspaces restored from snapshot but not yet visited this session.
Keys are workspace names, values are plists (at least `:project-dir',
optionally `:priority').  The lazy-start hook consumes from this set
on `persp-activated-functions' and starts claude for matching workspaces.
Entries remain here until the user actually switches to them, so
`claude-repl-save-workspace-snapshot' can still record them at quit
time even if they were never visited.")

(defvar claude-repl--loading-snapshot-p nil
  "Non-nil while `claude-repl-load-workspace-snapshot' is iterating entries.
The lazy-start hook checks this and skips firing during the loader loop
— otherwise each `+dwc/switch-to-project' call would eager-start claude
and defeat lazy restore.")

(defun claude-repl--snapshot-entry-normalize (entry)
  "Normalize a snapshot ENTRY to (NAME . PLIST).
Accepts the legacy `(NAME . DIR-STRING)' shape and the current
`(NAME :project-dir DIR :priority PRI)' plist shape."
  (let ((name (car entry))
        (payload (cdr entry)))
    (cons name
          (cond
           ((stringp payload) (list :project-dir payload))
           ((listp payload) payload)
           (t (error "claude-repl: malformed snapshot entry: %S" entry))))))

(defun claude-repl-save-workspace-snapshot ()
  "Save the current set of claude-repl workspaces to a hidden file.
Writes a list of (NAME :project-dir DIR :priority PRI) entries.  Sources:
workspaces registered in `claude-repl--workspaces' with a `:project-dir',
merged with any entries still pending in
`claude-repl--pending-snapshot-workspaces' (restored from snapshot but
never visited — included so unvisited workspaces aren't dropped)."
  (interactive)
  (let ((entries (make-hash-table :test 'equal)))
    ;; Visited workspaces — the live hash is the source of truth.
    (maphash (lambda (ws _plist)
               (when-let ((dir (claude-repl--ws-get ws :project-dir)))
                 (puthash ws
                          (list :project-dir dir
                                :priority (claude-repl--ws-get ws :priority))
                          entries)))
             claude-repl--workspaces)
    ;; Pending (unvisited) entries fill in anything still outstanding.
    (maphash (lambda (ws plist)
               (unless (gethash ws entries)
                 (puthash ws plist entries)))
             claude-repl--pending-snapshot-workspaces)
    (let (snapshot)
      (maphash (lambda (ws plist) (push (cons ws plist) snapshot))
               entries)
      (claude-repl--log nil "write-sexp-file: file=%s" claude-repl-workspace-snapshot-file)
      (with-temp-file claude-repl-workspace-snapshot-file
        (insert "(")
        (let ((first t))
          (dolist (entry snapshot)
            (unless first (insert "\n "))
            (setq first nil)
            (prin1 entry (current-buffer))))
        (insert ")"))
      (message "Saved %d workspace(s) to %s"
               (length snapshot) claude-repl-workspace-snapshot-file))))

(defun claude-repl-load-workspace-snapshot ()
  "Load workspaces from `claude-repl-workspace-snapshot-file'.
For each entry, calls `+dwc/switch-to-project' to create/switch to the
project's persp workspace and hydrates `:project-dir' and `:priority'
into `claude-repl--workspaces' so the tabline paints the priority badge
immediately.  Entries whose directory no longer exists are skipped.
Claude itself is not started here — the workspace is added to
`claude-repl--pending-snapshot-workspaces' and the lazy-start hook on
`persp-activated-functions' starts claude on first visit."
  (interactive)
  (let ((snapshot (claude-repl--read-sexp-file-if-exists
                   claude-repl-workspace-snapshot-file)))
    (unless snapshot
      (user-error "No workspace snapshot at %s" claude-repl-workspace-snapshot-file))
    (let ((loaded 0)
          (skipped 0)
          (claude-repl--loading-snapshot-p t))
      (dolist (raw snapshot)
        (let* ((entry (claude-repl--snapshot-entry-normalize raw))
               (ws (car entry))
               (plist (cdr entry))
               (dir (plist-get plist :project-dir))
               (priority (plist-get plist :priority)))
          (if (and dir (file-directory-p dir))
              (progn
                (+dwc/switch-to-project dir)
                (claude-repl--ws-put ws :project-dir dir)
                (when priority
                  (claude-repl--ws-put ws :priority priority))
                (puthash ws plist claude-repl--pending-snapshot-workspaces)
                (cl-incf loaded))
            (cl-incf skipped))))
      (force-mode-line-update t)
      (message "Loaded %d workspace(s), skipped %d" loaded skipped))))

(defun claude-repl--maybe-start-on-activate (&rest _)
  "Lazy-start hook for `persp-activated-functions'.
Starts claude for the just-activated workspace iff it was restored from
snapshot (still in `claude-repl--pending-snapshot-workspaces') and
claude isn't already running.  Skipped during snapshot loading so the
loader's own `+dwc/switch-to-project' calls don't eager-start claude.
No-op if the Doom workspace API isn't fboundp yet — the hook may be
invoked early in persp-mode activation before Doom's `+workspace-*'
autoloads are in place."
  (when (and (not claude-repl--loading-snapshot-p)
             (fboundp '+workspace-current-name))
    (let ((ws (ignore-errors (+workspace-current-name))))
      (when (and ws (gethash ws claude-repl--pending-snapshot-workspaces))
        (remhash ws claude-repl--pending-snapshot-workspaces)
        (unless (claude-repl--claude-running-p ws)
          (claude-repl--log ws "maybe-start-on-activate: starting ws=%s" ws)
          (claude-repl--initialize-claude ws))))))

(defun claude-repl--load-workspace-snapshot-on-startup ()
  "Restore the workspace snapshot silently at Emacs startup.
Does nothing if the snapshot file is absent.  Errors are logged but
never propagated, so a corrupt snapshot can't block startup."
  (when (file-exists-p claude-repl-workspace-snapshot-file)
    (condition-case err
        (claude-repl-load-workspace-snapshot)
      (error (message "[claude-repl] snapshot load failed: %S" err)))))

(defun claude-repl--save-workspace-snapshot-on-quit ()
  "Save the workspace snapshot when Emacs quits.
Skipped in `noninteractive' (batch) sessions so ERT test runs don't
clobber the user's real snapshot.  Errors are caught so a snapshot-write
failure never blocks quit."
  (unless noninteractive
    (condition-case err
        (claude-repl-save-workspace-snapshot)
      (error (message "[claude-repl] snapshot save failed: %S" err)))))
