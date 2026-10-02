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
;; tab ORDERING and HIDING (push/pull tab, priority reseating): tabs follow
;; roster order strictly, priority included, and the resolver does the
;; ordering.  The switch-to-N chords are NOT in that category and are back:
;; a numeral that names a SLOT OF THE DRAWN BAR reads the given order
;; instead of authoring one.  The interrupt family: the CLEAN TURN STOP is
;; back on `C-c C-k' (`agent-repl-interrupt-turn', verbs.el) against the
;; `Interrupt' verb's `turn' target, restored by owner order; the fan-wide
;; and detached-work stops remain the footer's, whose `all_agents' and
;; FeedId targets need feed vocabulary this client does not hold.  Close,
;; kill and nuke: `verbs.el' owns them as thin wrappers now.  The per-workspace
;; clipboard: dead with the host command loop that populated it.

;;; Code:

;; `magit-section' objects are EIEIO instances; `eieio-oref' reads their
;; slots.
(require 'eieio)

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--path-canonical "agent-repl-core" (path))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-current-log-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-log-name "agent-repl-workspace" (ws))
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-put "agent-repl-workspace" (ws key value))
(declare-function agent-repl--ws-known-p "agent-repl-workspace" (ws))
(declare-function agent-repl--ws-dir "agent-repl-status" (ws))
(declare-function agent-repl--live-ws-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-switch-project "agent-repl-workspace" (project))
(declare-function agent-repl--ws-register-project "agent-repl-workspace" (dir))
(declare-function agent-repl--ws-resolve-persp "agent-repl-workspace" (ws))
(declare-function agent-repl--ws-add-buffer "agent-repl-workspace" (buf persp &optional no-display))
(declare-function agent-repl--ws-by-ref-id "agent-repl-workspace" (id))
(declare-function agent-repl--ws-name-for-dir "agent-repl-worktree" (dir))
(declare-function agent-repl--arm-landing-panels "agent-repl-workspace" (target))
(declare-function agent-repl--panels-note-arrival-reason "agent-repl-panels"
                  (id reason))
(declare-function agent-repl--async-git "agent-repl-worktree" (label root args callback))
(declare-function agent-repl--send "agent-repl-input" (origin &optional prompt ws force))
(declare-function agent-repl--read-input-buffer "agent-repl-input" (ws))
(declare-function agent-repl-popup-open "agent-repl-popup" (path &optional line))
(declare-function agent-repl-host-register "agent-repl-host" (conn dir on-done))
(declare-function agent-repl-host-request-switch "host" (ws trigger))
(declare-function agent-repl-host-pending-selection "host" ())
(declare-function agent-repl-link-primary "agent-repl-daemon-link" ())
(declare-function agent-repl-verbs-select-minted "agent-repl-verbs"
                  (ref &optional lander))
(declare-function agent-repl-verbs--land-on-tab "agent-repl-verbs" (ref why ws))
(declare-function agent-repl-verb-open "agent-repl-verbs" (ref))
(declare-function agent-repl-workspace-progress-report "mutation-progress" (kind phase &rest details))
(declare-function agent-repl-verbs--all-rows "agent-repl-verbs" (&optional roster))
(declare-function agent-repl-roster-tab-order "agent-repl-roster" ())
(declare-function agent-repl-roster-drawn-tab-order "agent-repl-roster" ())
(declare-function agent-repl-roster-selection-recency-order "agent-repl-roster" (names))
(declare-function agent-repl-verbs--row-ref "agent-repl-verbs" (row))
(declare-function agent-repl-verbs--row-closed-p "agent-repl-verbs" (row))
(declare-function agent-repl-verbs--row-name "agent-repl-verbs" (row))
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
file is shown -- the shared popup's side and width, dired for a
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
          (agent-repl--log '(:agent-repl-central "project setup and command generation precede workspace ownership") "elisp.commands.change-spec branch=override scope=%s" scope))
        override)
       ((eq default-spec :use-branch-diff-spec)
        (when (fboundp 'agent-repl--log)
          (agent-repl--log '(:agent-repl-central "project setup and command generation precede workspace ownership") "elisp.commands.change-spec branch=branch-spec scope=%s" scope))
        'agent-repl-branch-diff-spec)
       (t
        (when (fboundp 'agent-repl--log)
          (agent-repl--log '(:agent-repl-central "project setup and command generation precede workspace ownership") "elisp.commands.change-spec branch=default scope=%s" scope))
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
naming and status -- the daemon derives and pushes.

REGISTERING A WORKSPACE COMES UP ON ITS PANEL EXACTLY LIKE CREATING ONE.
The switch waits for the answer, because the answer is when the workspace
EXISTS: switching on the way out of this function switched to a directory
the daemon had not accepted yet, so `agent-repl-switch-to-project\' found
no workspace at it, armed no landing panels, and Doom\'s empty-project
fallback put MAGIT STATUS in the main area instead of the new
workspace\'s own panel.  The minted ref is handed to
`agent-repl-verbs-select-minted\' -- the same landing
`CreateWorkspaceSuccess\' takes -- so the two verbs stand on a workspace
they just made through ONE mechanism.  A register the daemon REFUSED
moves the user nowhere: there is no workspace to come up on."
  (interactive (list (read-directory-name "Add project directory: " nil nil t)))
  (let ((canonical (file-name-as-directory (expand-file-name dir))))
    (unless (file-directory-p canonical)
      (user-error "agent-repl: %s is not a directory" canonical))
    (agent-repl--info '(:agent-repl-central "project setup and command generation precede workspace ownership") "elisp.commands.add-project dir=%s" canonical)
    (agent-repl--ws-register-project canonical)
    ;; THE GESTURE LEAVES A MARK IMMEDIATELY, like every other way of putting
    ;; a workspace on the roster: the register round-trips to the daemon, and
    ;; a directory that takes a moment to accept used to look like a keypress
    ;; that went nowhere.
    (agent-repl-workspace-progress-report :register :requested canonical)
    (agent-repl-host-register
     (agent-repl-link-primary) canonical
     (lambda (ref)
       (if ref
           (progn
             (agent-repl--info '(:agent-repl-central "project setup and command generation precede workspace ownership") "elisp.commands.add-project-registered dir=%s id=%s"
                               canonical (plist-get ref :id))
             ;; A REGISTERED DIRECTORY COMES UP ON ITS PANELS like every
             ;; other way of opening a workspace (owner ruling,
             ;; 2026-09-13, item 6); the roster row is where that happens,
             ;; so the reason is left for it here.
             (agent-repl--panels-note-arrival-reason
              (plist-get ref :id) "registered")
             (agent-repl-workspace-progress-report :register :completed canonical)
             (agent-repl-verbs-select-minted ref))
         ;; A REGISTER THAT DID NOT LAND IS SAID OUT LOUD.  It only ever
         ;; reached the log before, so a refused or unanswered register was
         ;; indistinguishable from one that worked and simply did not move
         ;; the user -- the exact ambiguity this whole ladder is against.
         (agent-repl--warn '(:agent-repl-central "project setup and command generation precede workspace ownership") "elisp.commands.add-project-not-registered dir=%s"
                           canonical)
         (agent-repl-workspace-progress-report
          :register :failed (format "the daemon did not register %s" canonical)))))))

;;;; ---- Workspace navigation ---------------------------------------------

(defun agent-repl--switch-project-arm-panels (project)
  "Arm the workspace at PROJECT so this switch lands on ITS OWN panel.

A WORKSPACE YOU ARE ABOUT TO STAND ON MUST SHOW ITSELF.  The gui
frontend pre-creates a workspace's page without displaying it
\(`agent-repl--gui-boot'), so the view arrives only when the
`:pending-show-panels' drain shows it on arrival
\(`agent-repl--drain-pending-show-panels').  Nothing armed that flag on
this path, so `SPC TAB n' -- which stands on the workspace it just made
by switching projectile to the minted worktree -- landed the user on an
EMPTY frame: no webview, no composer, only the tab bar to say the
workspace existed at all.

The flag is armed BEFORE the switch, so the panel arrives through
the persp activation hook's own drain rather than through a second show
mechanism.

NOTHING IS ARMED WHEN THE TARGET IS ALREADY CURRENT: no perspective
activates, so no drain is coming, and a flag left standing would re-show
panels the user had dismissed the NEXT time they came back.  A directory
that is not a workspace arms nothing either -- there is no panel to show."
  (let ((ws (agent-repl--ws-name-for-dir project))
        (current (ignore-errors (agent-repl--ws-current-name))))
    (cond
     ((null ws)
      (agent-repl--log (agent-repl--ws-current-log-name)
                       "switch-project-arm-panels: path=%s branch=not-a-workspace" project))
     ((equal ws current)
      (agent-repl--log ws "switch-project-arm-panels: ws=%s branch=already-current" ws))
     (t
      (agent-repl--arm-landing-panels ws)))))

(defconst agent-repl--switch-state-affixes
  '((:open . "")
    (:closed . " (closed)")
    (:not-here . " (not open here)"))
  "Affix drawn after a switcher candidate for each state it can be in.

A CLOSED WORKSPACE IS STILL A WORKSPACE YOU CAN SWITCH TO, so it is
offered -- but picking it costs a daemon round trip and a session
revival, which picking an open one does not.  The user is owed that
difference before they press RET, and text is the whole of how it is
said: no face, no styling, nothing the affix could disagree with when
the row's real state changes underneath it.")

(defun agent-repl--switch-candidates ()
  "Return every KNOWN workspace the `SPC p p' switcher can land on.

THE DAEMON'S ROSTER IS THE SOURCE OF WHAT EXISTS, so it is the source
here (owner ruling, 2026-09-20).  The switcher used to complete over
`agent-repl--live-ws-names' -- an entry in Emacs's own registry with no
`:killed-at' tombstone -- which is the set of workspaces that happen to
have a PERSPECTIVE IN THIS EMACS right now.  Three kinds of workspace
are real and were unreachable through it: one that was closed or killed,
one the daemon knows that this Emacs has not reconciled yet (a
just-registered repo whose landing is still pending), and one with no
local perspective yet for any other reason.

Each entry is a plist:

  :name   what the roster calls the row, or the workspace name
  :dir    the workspace directory, used only to break a name collision
  :ref    the daemon-minted `WorkspaceRef', the only openable identity
  :ws     the local workspace name when one is standing, else nil
  :state  `:open' (a tab is here), `:closed' (the roster says closed) or
          `:not-here' (the daemon knows it, this Emacs has no tab)

A LIVE LOCAL WORKSPACE THE ROSTER DOES NOT CARRY IS STILL OFFERED: the
roster push and the registry are reconciled asynchronously, and the
switcher must never drop a workspace the user is standing in because a
push is in flight.  Pseudo perspectives stay out -- `main' and `none'
are persp-mode's, not workspaces -- and `agent-repl--live-ws-names'
excludes them at its source."
  (let ((entries nil)
        (claimed nil))
    (dolist (row (agent-repl-verbs--all-rows))
      (let* ((ref (agent-repl-verbs--row-ref row))
             (id (plist-get ref :id)))
        (when id
          (let ((ws (agent-repl--ws-by-ref-id id)))
            (when ws (push ws claimed))
            (push (list :name (or (agent-repl-verbs--row-name row)
                                  (plist-get ref :dir)
                                  id)
                        :dir (plist-get ref :dir)
                        :ref ref
                        :ws ws
                        :state (cond (ws :open)
                                     ((agent-repl-verbs--row-closed-p row) :closed)
                                     (t :not-here)))
                  entries)))))
    (dolist (ws (agent-repl--live-ws-names))
      (unless (member ws claimed)
        (push (list :name ws
                    :dir (agent-repl--ws-get ws :project-dir)
                    :ref (agent-repl--ws-get ws :ref)
                    :ws ws
                    :state :open)
              entries)))
    (nreverse entries)))

(defun agent-repl--switch-display-alist (entries)
  "Return ENTRIES as an alist of (DISPLAY . ENTRY), in order.

A DISPLAY STRING MUST BE UNIQUE, because `completing-read' answers with
one and nothing else: NAMES COLLIDE ACROSS REPOS, and two rows sharing a
name would make the second unreachable -- `assoc' would hand back the
first every time.  A colliding name is qualified by its directory, which
is the one thing that cannot collide; a name that is already unique is
left alone rather than qualified for the sake of uniformity."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((name (plist-get entry :name)))
        (puthash name (1+ (gethash name counts 0)) counts)))
    (mapcar
     (lambda (entry)
       (let* ((name (plist-get entry :name))
              (dir (plist-get entry :dir))
              (base (if (and (> (gethash name counts 0) 1) dir)
                        (format "%s [%s]" name dir)
                      name)))
         (cons (concat base
                       (alist-get (plist-get entry :state)
                                  agent-repl--switch-state-affixes ""))
               entry)))
     entries)))

(defun agent-repl--switch-to-known-workspace ()
  "Pick a known workspace and stand on it, opening it first if it is not here.

AN OPEN WORKSPACE IS A SWITCH REQUEST and nothing more: the daemon is
asked to select it (`agent-repl-host-request-switch') and the frame
follows the roster.

ANYTHING ELSE GOES THROUGH `OpenWorkspace', which is the ONE path that
reopens a workspace -- the same verb `agent-repl-open-workspace' runs,
with the same minibuffer progress reporting, so a revival that takes a
shim spawn and a vendor resume says every stage it reaches.  There is
deliberately no second open path here: a switcher that opened workspaces
its own way would be a second mechanism to disagree with the first.

THE LANDING IS BY IDENTITY, not by directory.  The tab is not here yet --
it arrives on the roster push that the open provokes -- so the landing is
registered with `agent-repl-verbs-select-minted' and fires when the tab
does.  `agent-repl-verbs--land-on-tab' is the lander because the
workspace's perspective carries the DAEMON's name, which need not be its
directory's basename; landing by directory would let Doom mint a
perspective of its own and re-point the workspace being left.

A CANDIDATE WITH NO REF CANNOT BE OPENED and says so: the ref is
daemon-minted and there is no spelling of it Emacs could construct."
  (let ((candidates (agent-repl--switch-display-alist
                     (agent-repl--switch-candidates))))
    (unless candidates
      (user-error "agent-repl: no known workspaces"))
    (let* ((choice (completing-read "Switch to workspace: "
                                    (mapcar #'car candidates) nil t))
           (entry (cdr (assoc choice candidates)))
           (ws (plist-get entry :ws))
           (ref (plist-get entry :ref)))
      (cond
       (ws
        (agent-repl--info ws "elisp.commands.switch-chosen ws=%s" ws)
        (agent-repl-host-request-switch ws 'picker))
       (ref
        (agent-repl--info (agent-repl--ws-current-log-name)
                          "elisp.commands.switch-opens-absent name=%s id=%s"
                          (plist-get entry :name) (plist-get ref :id))
        (agent-repl-verb-open ref)
        (agent-repl-verbs-select-minted ref #'agent-repl-verbs--land-on-tab))
       (t
        (agent-repl--warn (agent-repl--ws-current-log-name)
                          "elisp.commands.switch-no-ref name=%S" choice)
        (user-error "agent-repl: %s has no daemon identity to open" choice))))))

(defun agent-repl-switch-to-project (&optional project)
  "Switch to a known workspace, or to PROJECT.

With no argument, completes over EVERY KNOWN workspace -- the daemon's
roster is the source of what exists -- and stands on the chosen one,
opening it first when it is closed or has no tab here.  See
`agent-repl--switch-to-known-workspace'.

With PROJECT (a project root path), switches to that project and opens
its most recently accessed file.  Both steps are deferred onto one timer
so the perspective switch completes and Emacs redraws before any
blocking I/O fires, and so they run in order rather than racing across
two timers."
  (interactive)
  (if project
      (let* ((target (agent-repl--ws-name-for-dir project))
             ;; A non-workspace project switch is editor-local and may begin
             ;; before any workspace exists; a known target is always explicit.
             (scope (if target target
                      '(:agent-repl-context "plain project switch has no workspace target"))))
        (agent-repl--log scope
                         "elisp.commands.switch-to-project path=%s" project)
        (agent-repl--switch-project-arm-panels project)
        (agent-repl--ws-switch-project project)
        (run-at-time
         0 nil
         (lambda ()
           (let ((recent-file (agent-repl--most-recent-project-file project)))
             (if (and recent-file (file-exists-p recent-file))
                 (progn
                   (agent-repl--log scope "elisp.commands.switch-open-recent file=%s"
                                    recent-file)
                   (find-file recent-file))
               (agent-repl--log scope
                                "elisp.commands.switch-no-recent project=%s" project))))))
    (agent-repl--switch-to-known-workspace)))

(defvar agent-repl--opened-recent-cycle nil
  "Workspaces already visited by `agent-repl-open-most-recent-workspace'.
Reset when the cycle exhausts, so repeated presses walk the whole roster
rather than sticking on the second-most-recent workspace.")

(defun agent-repl--roster-recent-names ()
  "Return open workspace names, most recently selected first.
The open rows of the roster, ordered by the ONE selection-recency order
\(`agent-repl-roster-selection-recency-order'): this session's switches,
then the daemon's durable last-selected instant, then never-selected
rows in roster order -- the same order a close lands by."
  (let (names)
    (dolist (row (agent-repl-verbs--all-rows))
      (unless (agent-repl-verbs--row-closed-p row)
        (let* ((ref (agent-repl-verbs--row-ref row))
               (ws (and ref (agent-repl--ws-by-ref-id (plist-get ref :id)))))
          (when ws (push ws names)))))
    (agent-repl-roster-selection-recency-order (nreverse names))))

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
          (agent-repl--info (agent-repl--ws-log-name current)
                            "elisp.commands.open-most-recent target=%s" target)
          (agent-repl-host-request-switch target 'most-recent))
      (setq agent-repl--opened-recent-cycle nil)
      (agent-repl--info (agent-repl--ws-log-name current)
                        "elisp.commands.open-most-recent-cycle-reset")
      (message "All workspaces visited -- cycle reset"))))

(defun agent-repl--drawn-tab-names ()
  "Return the workspace names the tab bar DRAWS, in the order it draws them.

THE DRAWN ORDER IS NAVIGATION'S ONLY ORDER.  `agent-repl-roster-tab-order'
is the tab bar's own order (roster.el sets it from the daemon resolver's
walk, priority included), so left/right and the numeral chords walk the
PICTURE THE USER IS LOOKING AT by construction rather than agreeing with
it by coincidence.

This replaced `agent-repl--live-ws-names' at every navigation site, and
the difference was the whole defect: that list is the registry hash's key
order, which is neither the drawn order nor even a stable one, and it
carries persp-mode's own pseudo perspectives -- `none' and Doom's `main'
\(`agent-repl--pseudo-workspace-name-p') -- which own no workspace at all.
Cycling over it therefore went the wrong way relative to the bar, and off
one end it switched to a splash screen with no tab highlighted or signaled
an error trying to reach `none'.  A pseudo perspective can never appear in
the roster's tab order, so it can never be a navigation target.

A TAB OF A COLLAPSED REPOSITORY IS NOT DRAWN, so it is not here either:
the numerals count only the drawn tabs and stay contiguous."
  (and (fboundp 'agent-repl-roster-drawn-tab-order)
       (agent-repl-roster-drawn-tab-order)))

(defun agent-repl--cycle-target (all drawn current n)
  "Return the DRAWN tab N steps from CURRENT along ALL, or nil.
ALL is every tab in walk order and DRAWN its drawn subset.  The walk
starts at CURRENT's place in ALL, so a current workspace whose tab is
hidden (its repository was collapsed) still steps to its drawn
neighbours, and every hidden tab on the way is passed over.  Nil when
CURRENT has no tab or nothing is drawn."
  (let ((index (cl-position current all :test #'equal))
        (count (length all)))
    (when (and index drawn)
      (let ((step (if (< n 0) -1 1))
            (remaining (abs n))
            (at index)
            (seen 0))
        (while (and (> remaining 0) (< seen (* count (abs n))))
          (setq at (mod (+ at step) count)
                seen (1+ seen))
          (when (member (nth at all) drawn)
            (setq remaining (1- remaining))))
        (and (zerop remaining) (nth at all))))))

(defun agent-repl--workspace-cycle (n)
  "Switch N places from the current workspace along the DRAWN tab order.
Wraps at both ends, so `s-{' and `s-}' match the bar in both directions.

THE STEP STARTS FROM THE MOST RECENT REQUEST while one is unanswered
\(`agent-repl-host-pending-selection'), not from the tab shown: the frame
moves only when the roster says so, and `s-}' pressed twice before it does
must land two tabs over, not one.  The target is REQUESTED, never switched
to here (`agent-repl-host-request-switch').

A bar with no tabs, and a current workspace that is not ON the bar (a
pseudo perspective, or a workspace whose tab the roster has torn down),
are LOGGED NO-OPS: there is no slot to count from, and inventing one would
land the user somewhere the picture never offered.

THE RECORD IS `info', NOT `debug'.  A cycle is an action a PERSON took, and
the durable threshold is `info' by default, so on the debug rung every switch
the user made left nothing on disk: realtest 4 drove four real chords, every
one switched correctly, and not one could be read back afterwards.  A routine
action is never promoted to `warn' to make it easier to find -- the realtest
harvest fails a run on every warning."
  (let* ((names (agent-repl--drawn-tab-names))
         (all (and (fboundp 'agent-repl-roster-tab-order) (agent-repl-roster-tab-order)))
         (shown (agent-repl--ws-current-name))
         (current (or (agent-repl-host-pending-selection) shown))
         (log-ws (agent-repl--ws-log-name shown))
         (target (agent-repl--cycle-target all names current n)))
    (if (null target)
        (agent-repl--info log-ws "elisp.commands.cycle-no-position n=%d tabs=%d from=%s"
                          n (length names) current)
      (agent-repl--info log-ws "elisp.commands.cycle n=%d from=%s target=%s"
                        n current target)
      (agent-repl-host-request-switch target 'cycle))))

(defun agent-repl-switch-left ()
  "Switch to the tab LEFT of the current one on the tab bar, wrapping."
  (interactive)
  (agent-repl--workspace-cycle -1))

(defun agent-repl-switch-right ()
  "Switch to the tab RIGHT of the current one on the tab bar, wrapping."
  (interactive)
  (agent-repl--workspace-cycle 1))

(defun agent-repl-switch-to-workspace (&optional n)
  "Switch to the workspace in the Nth slot of the tab bar, counting from 1.

N indexes the DRAWN tab order (`agent-repl--drawn-tab-names'), which is
why this exists at all: Doom's `+workspace/switch-to-N' indexes
persp-mode's perspective list, whose slot 0 is Doom's own `main', so
`M-1' landed on the splash screen and every numeral was off by one
against the bar.

Called interactively with no prefix argument, completes over the DRAWN
NAMES and switches to the chosen one -- the same set the numerals reach,
so the picker cannot offer a target a chord could not.

An empty bar and a slot the bar does not draw are REPORTED no-ops rather
than errors: the numerals are a glance-and-press gesture, and a press
past the end of the bar is a miss, not a fault.

Every outcome records at `info' for the reason `agent-repl--workspace-cycle'
gives: the numerals are a user action, and an action nobody can read back
afterwards is a logging defect."
  (interactive "P")
  (let* ((names (agent-repl--drawn-tab-names))
         (log-ws (agent-repl--ws-current-log-name))
         (index (and n (prefix-numeric-value n))))
    (cond
     ((null names)
      (agent-repl--info log-ws "elisp.commands.switch-to-workspace-no-tabs n=%S" n)
      (message "[agent-repl] No workspace tabs on the bar"))
     ((null index)
      (let ((choice (completing-read "Switch to workspace: " names nil t)))
        (agent-repl--info log-ws "elisp.commands.switch-to-workspace-chosen ws=%s" choice)
        (agent-repl-host-request-switch choice 'picker)))
     ((or (< index 1) (> index (length names)))
      (agent-repl--info log-ws
                        "elisp.commands.switch-to-workspace-out-of-range n=%d tabs=%d"
                        index (length names))
      (message "[agent-repl] No workspace tab %d -- the bar draws %d"
               index (length names)))
     (t
      (let ((target (nth (1- index) names)))
        (agent-repl--info log-ws "elisp.commands.switch-to-workspace n=%d target=%s"
                          index target)
        (agent-repl-host-request-switch target 'slot))))))

(eval-and-compile
  ;; The count is needed at EXPANSION time by the macro below and at RUN time
  ;; by the keymap `keybindings.el' builds from it, and one number has to
  ;; serve both or a tenth command appears with no chord on it.
  (defconst agent-repl-switch-numeral-count 9
    "How many workspace numeral chords the module owns: `M-1\=' .. `M-9\='.
`M-0\=' is left to Doom (`+workspace/switch-to-final\='), which is the one
numeral whose meaning does not need a slot index to be right."))

(defmacro agent-repl--define-switch-numerals ()
  "Define `agent-repl-switch-to-workspace-1' .. `-9'.

Each is a NAMED command rather than a closure in a keymap so that
`C-h k' says what the chord does, a test can assert the chord resolves to
it, and the keymap stays data.

THE LAST CHORD IS THE ODD ONE.  `M-9' means \"the ninth tab, or the LAST
tab when the bar draws fewer than nine\", so the chord at the end of the
row always lands on the workspace at the end of the bar.  Chords 1-8 are
exact slots and report a miss."
  (cons
   'progn
   (cl-loop
    for n from 1 to agent-repl-switch-numeral-count
    collect
    `(defun ,(intern (format "agent-repl-switch-to-workspace-%d" n)) ()
       ,(if (= n agent-repl-switch-numeral-count)
            (format "Switch to tab %d on the bar, or the LAST when it draws fewer." n)
          (format "Switch to tab %d on the bar, counting from the left." n))
       (interactive)
       (agent-repl-switch-to-workspace
        ,(if (= n agent-repl-switch-numeral-count)
             `(max 1 (min ,n (length (agent-repl--drawn-tab-names))))
           n))))))

(agent-repl--define-switch-numerals)

(provide 'commands)

;;; commands.el ends here
