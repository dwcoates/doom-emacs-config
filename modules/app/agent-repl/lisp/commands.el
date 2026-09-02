;;; commands.el --- canned prompts and editor-local conveniences -*- lexical-binding: t; -*-

;;; Commentary:

;; TWO KINDS OF THING LIVE HERE, and nothing else.
;;
;; THE CANNED PROMPT FAMILIES (explain / diff / PR / rebase / tests / lint /
;; quality / coverage).  Each composes prompt TEXT and hands it to the
;; composer pipeline (`agent-repl--send') under its own `PromptOrigin'.
;; They are legitimate because PROMPT COMPOSITION IS FREE: composing text
;; before submission is not rewriting what was submitted, and what is
;; submitted is what is drawn.  Every family is generated from one macro
;; over one scope table, so a new scope is a table row rather than five
;; hand-written commands.
;;
;; THE EDITOR-LOCAL CONVENIENCES: file-reference formatting and copying,
;; opening code through the ONE shared popup subroutine, the most-recent-file
;; cache, and workspace navigation.
;;
;; WHAT IS GONE, and why none of it was adapted.  The workspace snapshot
;; (save / load / archive / restore / tombstones), the rich revival picker
;; and `--establish-workspace': THE DAEMON IS THE SOURCE of which
;; workspaces exist, and Emacs opens tabs from the roster stream on connect.
;; A durable Emacs-side roster could only ever disagree with it.  Client
;; tab ORDERING and HIDING (push/pull tab, the switch-to-N tower, priority
;; reseating): tabs follow roster order strictly, priority included, and
;; the resolver does the ordering.  The interrupt family: no interrupt verb
;; exists in the contract, and the footer owns that gesture.  Close, kill
;; and nuke: `verbs.el' owns them as thin wrappers now.  The per-workspace
;; clipboard: dead with the host command loop that populated it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--path-canonical "agent-repl-core" (path))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-current-log-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key value))
(declare-function agent-repl--ws-known-p "agent-repl-workspace" (ws))
(declare-function agent-repl--ws-dir "agent-repl-status" (ws))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-switch "agent-repl-workspace" (ws &rest args))
(declare-function agent-repl--ws-switch-project "agent-repl-workspace" (project))
(declare-function agent-repl--ws-register-project "agent-repl-workspace" (dir))
(declare-function agent-repl--ws-resolve-persp "agent-repl-workspace" (ws))
(declare-function agent-repl--ws-add-buffer "agent-repl-workspace" (buf persp &optional no-display))
(declare-function agent-repl--ws-by-ref-id "agent-repl-workspace" (id))
(declare-function agent-repl--ws-name-for-dir "agent-repl-worktree" (dir))
(declare-function agent-repl--async-git "agent-repl-worktree" (label root args callback))
(declare-function agent-repl--send "agent-repl-input" (origin &optional prompt ws force))
(declare-function agent-repl--read-input-buffer "agent-repl-input" (ws))
(declare-function agent-repl-popup-open "agent-repl-popup" (path &optional line))
(declare-function agent-repl-host-register "agent-repl-host" (conn dir on-done))
(declare-function agent-repl-link-primary "agent-repl-daemon-link" ())
(declare-function agent-repl-verbs--all-rows "agent-repl-verbs" (&optional roster))
(declare-function agent-repl-verbs--row-ref "agent-repl-verbs" (row))
(declare-function agent-repl-verbs--row-closed-p "agent-repl-verbs" (row))
(declare-function magit-current-section "magit-section" ())
(declare-function magit-file-at-point "magit-git" (&optional expand assert))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function magit-section-match "magit-section" (condition &optional section))

;;;; ---- Canned prompt texts ----------------------------------------------

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
  "Format string for diff analysis messages.
First %s is the change-spec, second %s is the prompt."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-explain-prompt-template "please explain %s"
  "Format string for the explain command prompt.
%s is replaced with the context reference (file:line or file:range)."
  :type 'string
  :group 'agent-repl)

;;;; ---- The one send site every canned prompt uses ------------------------

(defun agent-repl--send-to-agent (text origin)
  "Submit TEXT to the current workspace's agent under ORIGIN.
Goes through the composer pipeline like every other send, so TEXT is
gated, wrapped into a `UserSaid' and submitted with an idempotency key
and this call site's own origin -- a canned prompt is not a privileged
path, it is a prompt whose words the editor happened to compose."
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log ws "elisp.commands.send-to-agent len=%d origin=%S" (length text) origin)
    (agent-repl--send origin text ws)))

;;;; ---- File reference helpers --------------------------------------------

(defun agent-repl--buffer-relative-path ()
  "Return the current buffer's file path relative to the workspace root."
  (let ((file (buffer-file-name))
        (ws (agent-repl--ws-current-name)))
    (unless file
      (agent-repl--log ws "elisp.commands.relative-path-refused buffer=%s" (buffer-name))
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (let ((rel (file-relative-name (agent-repl--path-canonical file)
                                   (agent-repl--ws-dir ws))))
      (agent-repl--log ws "elisp.commands.relative-path path=%s" rel)
      rel)))

(defun agent-repl--format-file-ref ()
  "Return a file:line or file:startline-endline reference string.
With an active region: file:startline-endline, and the mark is
deactivated.  Without one: file:line."
  (let ((rel (agent-repl--buffer-relative-path)))
    (if (use-region-p)
        (let ((start-line (line-number-at-pos (region-beginning)))
              (end-line (line-number-at-pos (region-end))))
          (deactivate-mark)
          (agent-repl--log (agent-repl--ws-current-log-name)
                           "elisp.commands.file-ref branch=region start=%d end=%d"
                           start-line end-line)
          (format "%s:%d-%d" rel start-line end-line))
      (agent-repl--log (agent-repl--ws-current-log-name)
                       "elisp.commands.file-ref branch=single-line line=%d"
                       (line-number-at-pos (point)))
      (format "%s:%d" rel (line-number-at-pos (point))))))

(eval-when-compile
  ;; Register magit-hunk-section's `to-range' slot name with eieio so the
  ;; `eieio-oref' below compiles warning-free without magit on the
  ;; compile-time load path.  Compile-time only; defines nothing at runtime.
  (cl-defstruct (agent-repl--magit-hunk-slot-shim) to-range))

(defun agent-repl--format-magit-hunk-ref ()
  "Format a file reference for the current magit hunk context.
Returns \"file:startline-endline\" from the hunk's to-range."
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
    (agent-repl--log (agent-repl--ws-current-log-name)
                     "elisp.commands.magit-hunk-ref ref=%s" ref)
    ref))

(defun agent-repl--context-reference ()
  "Return a context-appropriate file reference string.
In a magit hunk: that hunk's file:startline-endline.  Otherwise
`agent-repl--format-file-ref', which handles region and point alike."
  (if (and (derived-mode-p 'magit-diff-mode 'magit-status-mode 'magit-revision-mode)
           (magit-section-match 'hunk))
      (progn
        (agent-repl--log (agent-repl--ws-current-log-name)
                         "elisp.commands.context-ref branch=magit-hunk")
        (agent-repl--format-magit-hunk-ref))
    (agent-repl--log (agent-repl--ws-current-log-name)
                     "elisp.commands.context-ref branch=standard")
    (agent-repl--format-file-ref)))

;;;; ---- Opening code ------------------------------------------------------

(defun agent-repl-link-code (file start-line &optional _end-line workspace)
  "Open FILE at START-LINE through the ONE shared editor popup.
Entry point for the /runtime-eval-code skill.  Everything about HOW the
file is shown -- the right side, half the frame width, dired for a
directory -- belongs to `agent-repl-popup-open' and is deliberately not
re-decided here: divergence between the call sites of that subroutine is
a defect, so this site has no display logic of its own.

When WORKSPACE is given the buffer is also registered into that
workspace's perspective, so it is owned by the right perspective rather
than leaking into whichever one happened to be current."
  (let* ((path (expand-file-name file))
         (buf (agent-repl-popup-open path start-line))
         (ws (or workspace (agent-repl--ws-current-name))))
    (when (and workspace buf)
      (when-let ((persp (agent-repl--ws-resolve-persp workspace)))
        (agent-repl--ws-add-buffer buf persp nil)))
    (agent-repl--log ws "elisp.commands.link-code file=%s line=%s workspace=%s"
                     path start-line (and workspace t))
    buf))

;;;; ---- Diff analysis families --------------------------------------------

(defun agent-repl--send-diff-analysis (change-spec prompt)
  "Submit a diff analysis request.
CHANGE-SPEC describes which changes; PROMPT is the analysis instruction."
  (let ((msg (format agent-repl-diff-analysis-message-template change-spec prompt)))
    (agent-repl--log (agent-repl--ws-current-log-name)
                     "elisp.commands.diff-analysis scope=%s" change-spec)
    (agent-repl--send-to-agent msg :command-diff-analysis)))

;; The scope tables and the two helpers below are read by
;; `agent-repl--define-diff-commands' AT MACROEXPANSION TIME, so
;; byte-compiling this file requires them at compile time as well as at
;; load time.
(eval-and-compile
  (defconst agent-repl--diff-scopes
    '((worktree    . "unstaged changes (git diff)")
      (staged      . "staged changes (git diff --cached)")
      (uncommitted . "uncommitted changes (git diff HEAD)")
      (head        . "last commit (git show HEAD)")
      (branch      . :use-branch-diff-spec))
    "Alist mapping scope names to their change-spec strings.
  The special value `:use-branch-diff-spec' means use
  `agent-repl-branch-diff-spec'.")

  (defconst agent-repl--diff-scope-labels
    '((worktree    . "unstaged changes")
      (staged      . "staged changes")
      (uncommitted . "all uncommitted changes")
      (head        . "the last commit")
      (branch      . "all changes in the current branch"))
    "Alist mapping scope symbols to human-readable docstring labels.")

  (defconst agent-repl--update-pr-diff-scopes
    '((worktree    . "UNSTAGED changes (git diff). Do not consider staged changes or committed changes.")
      (staged      . "STAGED changes (git diff --cached). Do not consider unstaged changes or committed changes.")
      (uncommitted . "All UNCOMMITTED changes (git diff HEAD). Consider BOTH staged and unstaged changes. Do not consider committed changes.")
      (head        . "last commit (git show HEAD)."))
    "Scope overrides for the update-pr-diff family.
  More explicit than the standard scopes.  The `branch' scope is omitted
  and falls through to the default.")

  (defun agent-repl--resolve-change-spec (scope default-spec scope-overrides)
    "Resolve the change-spec form for SCOPE.
  DEFAULT-SPEC is the value from `agent-repl--diff-scopes'.
  SCOPE-OVERRIDES, when non-nil, names an alist taking precedence.
  Returns a string literal or the symbol `agent-repl-branch-diff-spec'."
    ;; Runs at macroexpansion time, where a standalone byte-compile has not
    ;; loaded core.el -- hence the `fboundp' gates on the logger.
    (let ((override (and scope-overrides (cdr (assq scope (eval scope-overrides))))))
      (cond
       (override
        (when (fboundp 'agent-repl--log)
          (agent-repl--log nil "elisp.commands.change-spec branch=override scope=%s" scope))
        override)
       ((eq default-spec :use-branch-diff-spec)
        (when (fboundp 'agent-repl--log)
          (agent-repl--log nil "elisp.commands.change-spec branch=branch-spec scope=%s" scope))
        'agent-repl-branch-diff-spec)
       (t
        (when (fboundp 'agent-repl--log)
          (agent-repl--log nil "elisp.commands.change-spec branch=default scope=%s" scope))
        default-spec))))

  (defun agent-repl--diff-command-form (scope-entry family doc-verb prompt-var scope-overrides)
    "Build one `defun' form for a diff-analysis command."
    (let* ((scope (car scope-entry))
           (doc-scope (cdr (assq scope agent-repl--diff-scope-labels)))
           (fn-name (intern (format "agent-repl-%s-%s" family scope)))
           (change-spec-form (agent-repl--resolve-change-spec
                              scope (cdr scope-entry) scope-overrides)))
      `(defun ,fn-name ()
         ,(format "%s %s." doc-verb doc-scope)
         (interactive)
         (agent-repl--send-diff-analysis ,change-spec-form ,prompt-var)))))

(defmacro agent-repl--define-diff-commands (family doc-verb prompt-var &optional scope-overrides)
  "Define one diff-analysis command per scope for FAMILY.

Each generated command is named `agent-repl-FAMILY-SCOPE' for SCOPE in
worktree, staged, uncommitted, head and branch.  DOC-VERB is used in
docstrings.  PROMPT-VAR names the prompt variable to pass.
SCOPE-OVERRIDES, when non-nil, names an alist replacing the default
change-spec for specific scopes."
  (declare (indent 2))
  `(progn
     ,@(cl-loop for scope-entry in agent-repl--diff-scopes
                collect (agent-repl--diff-command-form
                         scope-entry family doc-verb prompt-var scope-overrides))))

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

;;;; ---- Explain -----------------------------------------------------------

(defun agent-repl-explain ()
  "Ask the agent to explain the current context.
In a magit hunk: the hunk's file path and line range.  With an active
region: the file path and line range.  Otherwise: file path and line."
  (interactive)
  (let* ((ref (agent-repl--context-reference))
         (msg (format agent-repl-explain-prompt-template ref)))
    (agent-repl--log (agent-repl--ws-current-log-name) "elisp.commands.explain ref=%s" ref)
    (agent-repl--send-to-agent msg :command-explain-context)))

(defun agent-repl-explain-prompt ()
  "Prompt for a message about the current context, pre-filled with its reference."
  (interactive)
  (let* ((ref (agent-repl--context-reference))
         (msg (read-string "Send to the agent: " ref)))
    (if (and msg (not (string-empty-p msg)))
        (progn
          (agent-repl--log (agent-repl--ws-current-log-name)
                           "elisp.commands.explain-prompt len=%d" (length msg))
          (agent-repl--send-to-agent msg :command-explain-prompt))
      (agent-repl--log (agent-repl--ws-current-log-name)
                       "elisp.commands.explain-prompt-empty -- nothing sent"))))

;;;; ---- PR and rebase -----------------------------------------------------

(defun agent-repl-update-pr ()
  "Ask the agent to update the PR description for the current branch."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "elisp.commands.update-pr")
  (agent-repl--send-to-agent agent-repl-update-pr-prompt :command-update-pr))

(defun agent-repl--rebase-onto-origin-master-callback (ws ok output)
  "Process the `git fetch origin' result and ask the agent to rebase.
On failure the agent dispatch is SKIPPED and the git error is surfaced:
rebasing against a stale `origin/master' would silently do the wrong
thing, which is worse than not rebasing."
  (agent-repl--log ws "elisp.commands.rebase-fetch ok=%s output=%s" ok output)
  (if ok
      (progn
        (agent-repl--info ws "elisp.commands.rebase-fetch-complete ws=%s" ws)
        (agent-repl--send-to-agent agent-repl-rebase-onto-origin-master-prompt
                                   :command-rebase))
    (agent-repl--warn ws "elisp.commands.rebase-fetch-failed ws=%s output=%s" ws output)))

(defun agent-repl-rebase-onto-origin-master ()
  "Fetch origin, then ask the agent to rebase onto origin/master."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (project-dir (agent-repl--ws-dir ws)))
    (agent-repl--info ws "elisp.commands.rebase-fetch-start ws=%s dir=%s" ws project-dir)
    (agent-repl--async-git
     "rebase-fetch" project-dir '("fetch" "origin")
     (lambda (ok output)
       (agent-repl--rebase-onto-origin-master-callback ws ok output)))))

(defun agent-repl--exclusion-symbol-to-flag (sym &optional ws)
  "Convert exclusion SYM to the corresponding flag.
E.g. \\='no-self-certified becomes \"--self-certified\"."
  (let ((name (symbol-name sym)))
    (unless (string-prefix-p "no-" name)
      (agent-repl--log ws "elisp.commands.exclusion-invalid sym=%S" sym)
      (error "agent-repl: exclusion symbol must start with `no-': %S" sym))
    (let ((flag (concat "--" (substring name 3))))
      (agent-repl--log ws "elisp.commands.exclusion sym=%S flag=%s" sym flag)
      flag)))

(defun agent-repl--build-create-or-update-pr-prompt (excluded &optional ws)
  "Build the /create-or-update-pr prompt, omitting flags for EXCLUDED.
EXCLUDED is a list of `no-FLAG' symbols.  Each must name a flag actually
in `agent-repl-create-or-update-pr-base-flags', or this errors: an
exclusion that matches nothing is a typo that would otherwise silently
send the flag it meant to drop."
  (let ((excluded-flags
         (mapcar (lambda (sym)
                   (let ((flag (agent-repl--exclusion-symbol-to-flag sym ws)))
                     (unless (member flag agent-repl-create-or-update-pr-base-flags)
                       (agent-repl--log ws "elisp.commands.exclusion-unknown sym=%S flag=%s"
                                        sym flag)
                       (error "agent-repl: %S excludes %s, not in base flags" sym flag))
                     flag))
                 excluded)))
    (let ((prompt (string-join
                   (cons "/create-or-update-pr"
                         (cl-remove-if (lambda (f) (member f excluded-flags))
                                       agent-repl-create-or-update-pr-base-flags))
                   " ")))
      (agent-repl--log ws "elisp.commands.pr-prompt excluded=%S prompt=%s" excluded prompt)
      prompt)))

(defun agent-repl-create-or-update-pr (&optional excluded)
  "Send /create-or-update-pr, with any EXCLUDED flags dropped.
The composer's current text, when non-empty, is prepended as a prefix;
the composer is then left to the send pipeline, which clears it only once
the daemon accepts the submission."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (base (agent-repl--build-create-or-update-pr-prompt excluded ws))
         (raw-prefix (agent-repl--read-input-buffer ws))
         (prefix (and raw-prefix (string-trim-right raw-prefix)))
         (has-prefix (and prefix (not (string-empty-p prefix))))
         (prompt (if has-prefix (concat prefix " " base) base)))
    (agent-repl--log ws "elisp.commands.create-or-update-pr prefix-len=%d"
                     (length (or prefix "")))
    (agent-repl--send-to-agent prompt :command-create-or-update-pr)))

(defun agent-repl-create-or-update-pr-no-self-certified ()
  "Send /create-or-update-pr without --self-certified."
  (interactive)
  (agent-repl-create-or-update-pr '(no-self-certified)))

(defun agent-repl-create-or-update-pr-paste (&optional excluded)
  "Insert the /create-or-update-pr prompt at point instead of sending it.
Wrapped in backticks for inline-code rendering.  No workspace state is
touched and the agent is not contacted."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (prompt (agent-repl--build-create-or-update-pr-prompt excluded ws)))
    (agent-repl--log ws "elisp.commands.pr-paste prompt=%s" prompt)
    (insert "`" prompt "`")))

(defun agent-repl-create-or-update-pr-no-self-certified-paste ()
  "Insert the /create-or-update-pr prompt (no --self-certified) at point."
  (interactive)
  (agent-repl-create-or-update-pr-paste '(no-self-certified)))

;;;; ---- Copying -----------------------------------------------------------

(defun agent-repl-copy-reference ()
  "Copy the current file and line reference to the clipboard."
  (interactive)
  (let ((ref (agent-repl--format-file-ref)))
    (kill-new ref)
    (agent-repl--log (agent-repl--ws-current-log-name)
                     "elisp.commands.copy-reference ref=%s" ref)
    (message "Copied: %s" ref)))

(defun agent-repl-copy-workspace-name ()
  "Copy the current workspace's name to the kill ring and system clipboard."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws
      (user-error "No current workspace to copy the name of"))
    (kill-new ws)
    (agent-repl--log ws "elisp.commands.copy-workspace-name ws=%s" ws)
    (message "Copied workspace name: %s" ws)))

;;;; ---- The most-recent-file cache ---------------------------------------

(defvar recentf-list)

(defun agent-repl--record-last-file-visit ()
  "Cache the visited file as `:last-file' on the current workspace.
Called from `find-file-hook'; a no-op when the buffer has no file, when
there is no registered current workspace, or when the file is outside the
workspace's directory.  Keeps the cache fresh at zero per-switch cost so
`agent-repl--most-recent-project-file' can answer from a plist lookup
instead of a linear scan."
  (when-let* ((file buffer-file-name)
              (ws (agent-repl--ws-current-name))
              ((agent-repl--ws-known-p ws))
              (project-dir (ignore-errors (agent-repl--ws-dir ws)))
              ((file-in-directory-p file project-dir)))
    (agent-repl--ws-put ws :last-file file)))

(add-hook 'find-file-hook #'agent-repl--record-last-file-visit)

(defun agent-repl--most-recent-project-file (project-root)
  "Return the most-recently-accessed file under PROJECT-ROOT, or nil.
Uses `file-in-directory-p' rather than `string-prefix-p': the latter
would mis-match `/p/foo' against a file under `/p/foo-bar/' because the
prefix is not terminated at a separator.

Prefers the `:last-file' plist cache, falling back to scanning
`recentf-list'.  Callers must still verify the path exists -- both
sources lag filesystem deletions."
  (when project-root
    (or
     (when-let* ((ws (agent-repl--ws-name-for-dir project-root))
                 (cached (agent-repl--ws-get ws :last-file))
                 ((file-exists-p cached)))
       cached)
     (seq-find (lambda (file)
                 (and (file-exists-p file)
                      (file-in-directory-p file project-root)))
               (bound-and-true-p recentf-list)))))

;;;; ---- Onboarding a directory -------------------------------------------

(defun agent-repl-add-project-workspace (&optional dir)
  "Onboard DIR as a workspace (`SPC TAB C-n').
REGISTER is one of Emacs's only two workspace verbs: it hands the daemon
a directory and the daemon MINTS the identity, normalizing whatever
spelling of the path Emacs happened to have.  Idempotent by dir, so
onboarding a directory twice reconciles to the same ref.

This is the one RegisterWorkspace site besides the link-up re-register:
everything else about the workspace -- its parentage, branch, repo,
naming and status -- the daemon derives and pushes."
  (interactive (list (read-directory-name "Add project directory: " nil nil t)))
  (let ((canonical (file-name-as-directory (expand-file-name dir))))
    (unless (file-directory-p canonical)
      (user-error "agent-repl: %s is not a directory" canonical))
    (agent-repl--info nil "elisp.commands.add-project dir=%s" canonical)
    (agent-repl--ws-register-project canonical)
    (agent-repl-host-register
     (agent-repl-link-primary) canonical
     (lambda (ref)
       (if ref
           (agent-repl--info nil "elisp.commands.add-project-registered dir=%s id=%s"
                             canonical (plist-get ref :id))
         (agent-repl--warn nil "elisp.commands.add-project-not-registered dir=%s"
                           canonical))))
    (agent-repl-switch-to-project canonical)))

;;;; ---- Workspace navigation ---------------------------------------------

(defun agent-repl-switch-to-project (&optional project)
  "Switch to a live workspace, or to PROJECT.

With no argument, completes over the LIVE workspaces -- which the roster
stream is the source of -- and switches to the chosen one.  A CLOSED
workspace is not offered here: reopening one is `OpenWorkspace''s job
and lives on `agent-repl-open-workspace', because it is a request to the
daemon rather than an editor-local switch.

With PROJECT (a project root path), switches to that project and opens
its most recently accessed file.  Both steps are deferred onto one timer
so the perspective switch completes and Emacs redraws before any
blocking I/O fires, and so they run in order rather than racing across
two timers."
  (interactive)
  (if project
      (progn
        (agent-repl--log (agent-repl--ws-current-log-name)
                         "elisp.commands.switch-to-project path=%s" project)
        (agent-repl--ws-switch-project project)
        (run-at-time
         0 nil
         (lambda ()
           (let ((recent-file (agent-repl--most-recent-project-file project))
                 (current (ignore-errors (agent-repl--ws-current-name))))
             (if (and recent-file (file-exists-p recent-file))
                 (progn
                   (agent-repl--log current "elisp.commands.switch-open-recent file=%s"
                                    recent-file)
                   (find-file recent-file))
               (agent-repl--log current
                                "elisp.commands.switch-no-recent project=%s" project))))))
    (let ((names (agent-repl--live-ws-names)))
      (unless names
        (user-error "agent-repl: no live workspaces"))
      (let ((choice (completing-read "Switch to workspace: " names nil t)))
        (agent-repl--log (agent-repl--ws-current-log-name)
                         "elisp.commands.switch-chosen ws=%s" choice)
        (agent-repl--ws-switch choice)))))

(defvar agent-repl--opened-recent-cycle nil
  "Workspaces already visited by `agent-repl-open-most-recent-workspace'.
Reset when the cycle exhausts, so repeated presses walk the whole roster
rather than sticking on the second-most-recent workspace.")

(defun agent-repl--roster-recent-names ()
  "Return open workspace names, most recently selected first.
Ordered from the roster's WHEN COLUMN -- the daemon's own
`last_selected' instant -- rather than from a local history ring: the
roster is the source of which workspaces exist, so it is also the source
of which one was last looked at."
  (let (scored)
    (dolist (row (agent-repl-verbs--all-rows))
      (unless (agent-repl-verbs--row-closed-p row)
        (let* ((ref (agent-repl-verbs--row-ref row))
               (ws (and ref (agent-repl--ws-by-ref-id (plist-get ref :id))))
               (shown (plist-get row :when))
               (at (when (eq (plist-get shown :arm) :last-selected)
                     (plist-get (plist-get shown :value) :at-ms))))
          (when ws (push (cons ws (or at 0)) scored)))))
    (mapcar #'car (sort (nreverse scored) (lambda (a b) (> (cdr a) (cdr b)))))))

(defun agent-repl-open-most-recent-workspace ()
  "Switch to the most recently selected workspace not yet visited this cycle.
Each call lands on a different workspace, walking the roster's
when-column order; when every candidate has been visited the cycle
resets."
  (interactive)
  (let* ((current (agent-repl--ws-current-name))
         (ordered (agent-repl--roster-recent-names))
         (candidates (cl-remove-if
                      (lambda (name)
                        (or (equal name current)
                            (member name agent-repl--opened-recent-cycle)))
                      ordered))
         (target (car candidates)))
    (if target
        (progn
          (push target agent-repl--opened-recent-cycle)
          (agent-repl--log current "elisp.commands.open-most-recent target=%s" target)
          (agent-repl--ws-switch target))
      (setq agent-repl--opened-recent-cycle nil)
      (agent-repl--log current "elisp.commands.open-most-recent-cycle-reset")
      (message "All workspaces visited -- cycle reset"))))

(defun agent-repl--workspace-cycle (n)
  "Switch N places from the current workspace in roster order.
Roster order is the daemon resolver's, priority included, so this is
navigation over a given order and never a reordering of it."
  (let* ((names (agent-repl--live-ws-names))
         (current (agent-repl--ws-current-name))
         (index (cl-position current names :test #'equal)))
    (if (or (null names) (null index))
        (agent-repl--log current "elisp.commands.cycle-no-position n=%d names=%d"
                         n (length names))
      (let ((target (nth (mod (+ index n) (length names)) names)))
        (agent-repl--log current "elisp.commands.cycle n=%d target=%s" n target)
        (agent-repl--ws-switch target)))))

(defun agent-repl-switch-left ()
  "Switch to the previous workspace in roster order."
  (interactive)
  (agent-repl--workspace-cycle -1))

(defun agent-repl-switch-right ()
  "Switch to the next workspace in roster order."
  (interactive)
  (agent-repl--workspace-cycle 1))

(provide 'commands)

;;; commands.el ends here
