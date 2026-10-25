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
(declare-function agent-repl--now-unix-ms "agent-repl-core" ())
(defvar agent-repl--color-merging-purple)
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
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
(declare-function agent-repl--async-command "agent-repl-core" (label dir command callback &optional input))
;; `agent-repl--config-file' is defined by config.el at the module root,
;; which is the loader rather than a lisp/ source.
(defvar agent-repl--config-file)
(declare-function agent-repl--send "agent-repl-input" (origin &optional prompt ws force))
(declare-function agent-repl--read-input-buffer "agent-repl-input" (ws))
(declare-function agent-repl-popup-open "agent-repl-popup" (path &optional line))
(declare-function agent-repl-host-register "agent-repl-host" (conn dir on-done &optional workspace on-refused))
(declare-function agent-repl-verbs-register-refusal-sentence "verbs" (dir value))
(declare-function agent-repl-host-request-switch "host" (ws trigger))
(declare-function agent-repl-host-pending-selection "host" ())
(declare-function agent-repl-link-primary "agent-repl-daemon-link" ())
(declare-function agent-repl-verbs-select-minted "agent-repl-verbs"
                  (ref dispatched-at &optional lander))
(declare-function agent-repl-host-editor-switch-count "host" ())
(declare-function agent-repl-verbs--land-on-tab "agent-repl-verbs" (ref why ws))
(declare-function agent-repl-verb-open "agent-repl-verbs" (ref))
(declare-function agent-repl-verb-swap "agent-repl-verbs" (ws target))
(declare-function agent-repl-verb-move "agent-repl-verbs" (ws target))
(declare-function agent-repl-workspace-progress-report "mutation-progress" (kind phase &rest details))
(declare-function agent-repl-verbs--all-rows "agent-repl-verbs" (&optional roster))
(declare-function agent-repl-roster-tab-order "agent-repl-roster" ())
(declare-function agent-repl-roster-drawn-tab-order "agent-repl-roster" ())
(declare-function agent-repl-roster-selection-recency-order "agent-repl-roster" (names))
(declare-function agent-repl-roster-threads "agent-repl-roster" (&optional roster))
(declare-function agent-repl-roster-last-finished-name "agent-repl-roster" (names))
(declare-function agent-repl-roster-last-turn-ended-ms "agent-repl-roster" (ws))
(declare-function agent-repl-roster-row-ref "agent-repl-roster" (row))
(declare-function agent-repl-roster-row-closed-p "agent-repl-roster" (row))
(declare-function agent-repl-roster-row-name "agent-repl-roster" (row))
(declare-function agent-repl-verbs--roster-or-view "agent-repl-verbs" (roster))
(declare-function agent-repl-roster-walk "agent-repl-roster" (roster))
(declare-function agent-repl-roster-row-status "agent-repl-roster" (row))
(declare-function agent-repl-roster-row-instant-ms "agent-repl-roster" (row field))
(declare-function agent-repl-roster-row-merged-at "agent-repl-roster" (row))
(declare-function agent-repl-roster-row-closed-by-user-at "agent-repl-roster" (row))
(declare-function agent-repl-roster-row-commit-count "agent-repl-roster" (row))
(declare-function agent-repl-status-roster-mark "agent-repl-status" (arm &optional glyph))
(declare-function agent-repl-status--glyph-char "agent-repl-status" (name))
(declare-function agent-repl-format-age "core" (at-ms now-ms))
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
Entry point for the /agent-repl-runtime skill.  Everything about HOW the
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

;;;; ---- Release push --------------------------------------------------------
;;
;; Every push of master is a release: `bin/push-release.sh' tags master's tip
;; release-<version> and pushes both in one atomic push, which is the only
;; push the repository's pre-push hook accepts.  It pushes the repository
;; this module is loaded from, whichever workspace is shown.

(defconst agent-repl--push-release-log-scope
  '(:agent-repl-central "a release push is of the module's repository, not a workspace")
  "The log scope of a release push.")

(defun agent-repl--push-release-callback (ok output)
  "Report the release push's outcome: OK non-nil on success, OUTPUT its text.
The last line of OUTPUT is the script's own verdict, so it is what the
echo area shows either way.  A script killed before it printed has none."
  (let ((verdict (or (car (last (split-string output "\n" t)))
                     "the script printed nothing")))
    (if ok
        (progn
          (agent-repl--info agent-repl--push-release-log-scope
                            "elisp.commands.push-release-complete output=%S" output)
          (message "agent-repl: %s" verdict))
      (agent-repl--error agent-repl--push-release-log-scope
                         "elisp.commands.push-release-failed output=%S" output)
      (message "agent-repl: release push failed: %s" verdict))))

(defun agent-repl-push-release ()
  "Push master to origin as a release, with `bin/push-release.sh'."
  (interactive)
  (let* ((dir (file-name-directory agent-repl--config-file))
         (script (expand-file-name "bin/push-release.sh" dir)))
    (agent-repl--info agent-repl--push-release-log-scope
                      "elisp.commands.push-release-start script=%s" script)
    (message "agent-repl: pushing master to origin as a release...")
    (agent-repl--async-command "push-release" dir (list script)
                               #'agent-repl--push-release-callback)))

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
(defvar agent-repl-ws-state-icon-default)
(defvar agent-repl-status-dot-glyph)

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
  (let ((canonical (file-name-as-directory (expand-file-name dir)))
        ;; The gesture's moment: a switch in Emacs after it withdraws the landing.
        (dispatched-at (agent-repl-host-editor-switch-count)))
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
             (agent-repl-verbs-select-minted ref dispatched-at))
         ;; A REGISTER THAT DID NOT LAND IS SAID OUT LOUD.  It only ever
         ;; reached the log before, so a refused or unanswered register was
         ;; indistinguishable from one that worked and simply did not move
         ;; the user -- the exact ambiguity this whole ladder is against.
         (agent-repl--warn '(:agent-repl-central "project setup and command generation precede workspace ownership") "elisp.commands.add-project-not-registered dir=%s"
                           canonical)
         (agent-repl-workspace-progress-report
          :register :failed (format "the daemon did not register %s" canonical))))
     nil
     ;; A REFUSAL IS THE DAEMON'S ANSWER, and it is said in its own words: a
     ;; temporary folder, or a directory that is not a git worktree, names
     ;; the directory and why (owner ruling, 2026-10-06).
     (lambda (refusal)
       (agent-repl-workspace-progress-report
        :register :failed (agent-repl-verbs-register-refusal-sentence canonical refusal))))))

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

(defconst agent-repl--switch-state-labels
  '((:open . "")
    (:closed . "closed")
    (:not-here . "not open here"))
  "The state column's word for each state a switcher candidate can be in.

A CLOSED WORKSPACE IS STILL A WORKSPACE YOU CAN SWITCH TO, so it is
offered -- but picking it costs a daemon round trip and a session
revival, which picking an open one does not.  The user is owed that
difference before they press RET, and text is the whole of how it is
said: no face, no styling, nothing the column could disagree with when
the row's real state changes underneath it.  A hidden or merged row says
so in its place instead (`agent-repl--switch-state-label'), and the word
\"merged\" takes the merge purple every other surface paints a merge in.")

(defconst agent-repl--switch-recent-limit 25
  "How many workspaces of each RECENT kind `SPC p p' offers.
The most recently MERGED and the ones the USER most recently closed or
killed, this many of each (owner request, 2026-10-14): enough to reach
yesterday's work, few enough that the list stays a list of what is
current.  `SPC p P' offers every known workspace regardless.")

(defconst agent-repl--switch-hidden-glyph "◌"
  "The mark a HIDDEN workspace draws in the switcher in place of its dot.
A dotted ring: the workspace is there but kept out of sight.  It is inked
with the row's status color, so the switcher still says what the sidebar
says about the workspace's status.")

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
  :row    the roster row, nil for a live local workspace with none yet
  :section  the roster section the row sits in (`:repository',
          `:hidden' or `:merged', `agent-repl-roster-walk'), nil
          without a row

A LIVE LOCAL WORKSPACE THE ROSTER DOES NOT CARRY IS STILL OFFERED: the
roster push and the registry are reconciled asynchronously, and the
switcher must never drop a workspace the user is standing in because a
push is in flight.  Pseudo perspectives stay out -- `main' and `none'
are persp-mode's, not workspaces -- and `agent-repl--live-ws-names'
excludes them at its source."
  (let ((entries nil)
        (claimed nil))
    (dolist (walked (agent-repl-roster-walk
                     (agent-repl-verbs--roster-or-view nil)))
      (let* ((row (plist-get walked :row))
             (ref (agent-repl-roster-row-ref row))
             (id (plist-get ref :id)))
        (when id
          (let ((ws (agent-repl--ws-by-ref-id id)))
            (when ws (push ws claimed))
            (push (list :name (or (agent-repl-roster-row-name row)
                                  (plist-get ref :dir)
                                  id)
                        :dir (plist-get ref :dir)
                        :ref ref
                        :ws ws
                        :state (cond (ws :open)
                                     ((agent-repl-roster-row-closed-p row) :closed)
                                     (t :not-here))
                        :row row
                        :section (plist-get walked :section))
                  entries)))))
    (dolist (ws (agent-repl--live-ws-names))
      (unless (member ws claimed)
        (push (list :name ws
                    :dir (agent-repl--ws-get ws :project-dir)
                    :ref (agent-repl--ws-get ws :ref)
                    :ws ws
                    :state :open
                    :row nil
                    :section nil)
              entries)))
    (nreverse entries)))

(defun agent-repl--switch-display-alist (entries)
  "Return ENTRIES as an alist of (DISPLAY . ENTRY), in order.

A DISPLAY STRING MUST BE UNIQUE, because `completing-read' answers with
one and nothing else: NAMES COLLIDE ACROSS REPOS, and two rows sharing a
name would make the second unreachable -- `assoc' would hand back the
first every time.  A colliding name is qualified by its directory, which
is the one thing that cannot collide; a name that is already unique is
left alone rather than qualified for the sake of uniformity.  Everything
else a line shows rides beside the string, in its affixes
\(`agent-repl--switch-affixation'), so it never makes two names differ."
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
         (cons base entry)))
     entries)))


(defun agent-repl--switch-entry-invariant-failed (operation entry missing)
  "Fail hard: switcher ENTRY lacks MISSING, which OPERATION ranks by.
Records the violation through the canonical error log at the central
sink, naming OPERATION, the entry and what it lacks, then signals.
The one way a switcher ranking reports an entry that breaks its
invariant."
  (agent-repl--error '(:agent-repl-central "the workspace switcher reads the whole roster")
                     "elisp.commands.%s: name=%s has no %s"
                     operation (plist-get entry :name) missing)
  (error "agent-repl: switcher entry %s has no %s" (plist-get entry :name) missing))

(defun agent-repl--switch-most-recent (entries instant)
  "Return the `agent-repl--switch-recent-limit' ENTRIES latest by INSTANT.
INSTANT answers an entry's epoch-ms instant; newest first.  Every entry
handed here states one -- a merged row states when it landed, and a
user-closed row when it was closed, by construction of the lists -- so
an entry without one is a violated invariant and fails hard."
  (dolist (entry entries)
    (unless (funcall instant entry)
      (agent-repl--switch-entry-invariant-failed
       "switch-most-recent" entry "instant to rank by")))
    (seq-take (sort (copy-sequence entries)
                    (lambda (a b) (> (funcall instant a) (funcall instant b))))
              agent-repl--switch-recent-limit))

(defun agent-repl--switch-update-ms (entry)
  "Return ENTRY's update recency, epoch ms, for the switcher to rank by.
The daemon's `RosterRowLastUpdated': the latest of the workspace's
lifecycle instants and of its last status change.  A live local
workspace no roster push carries yet is being registered right now, so
nothing has been updated more recently: it ranks as `most-positive-fixnum'.
A ROW WITHOUT THE INSTANT IS A VIOLATED INVARIANT -- the decoder refuses
a row that does not state it -- and fails hard."
  (let ((row (plist-get entry :row)))
    (if (null row)
        most-positive-fixnum
      (or (agent-repl-roster-row-instant-ms row :last-updated)
          (agent-repl--switch-entry-invariant-failed
           "switch-update-ms" entry "update recency")))))

(defun agent-repl--switch-by-recency (entries)
  "Return ENTRIES most recently updated first (`agent-repl--switch-update-ms').
THE SWITCHER'S ONE ORDER (owner request, 2026-10-14): whatever most
recently happened to a workspace -- a turn resolving, an open, a close or
kill, a status change -- brings it to the top, whichever section it sits
in.  Ties keep ENTRIES' own order: `sort' is stable."
  (sort (copy-sequence entries)
        (lambda (a b) (> (agent-repl--switch-update-ms a)
                         (agent-repl--switch-update-ms b)))))

(defun agent-repl--switch-curated (entries)
  "Return the ENTRIES `SPC p p' offers.

THE WORKSPACES THAT ARE CURRENT, AND THE RECENT ONES (owner request,
2026-10-14): every open workspace, every hidden one, the
`agent-repl--switch-recent-limit' most recently MERGED, and as many of
the ones the USER most recently closed or killed -- by their own gesture,
which the daemon's close record says (`RosterRowClosure').  A workspace
the daemon, an agent or a merge closed is left to `SPC p P', and so is
one whose close predates that record: nothing says the user closed it.

A live local workspace with no roster row yet is open by construction --
a tab is standing for it here -- and is never dropped.

The order they are offered in is `agent-repl--switch-by-recency''s, applied
by the caller."
  (let (open hidden merged closed)
    (dolist (entry entries)
      (let ((row (plist-get entry :row)))
        (pcase (plist-get entry :section)
          (:hidden (push entry hidden))
          (:merged (push entry merged))
          (_ (cond
              ((or (null row) (not (agent-repl-roster-row-closed-p row)))
               (push entry open))
              ((agent-repl-roster-row-closed-by-user-at row)
               (push entry closed)))))))
    (append (nreverse open)
            (nreverse hidden)
            (agent-repl--switch-most-recent
             (nreverse merged)
             (lambda (entry) (agent-repl-roster-row-merged-at (plist-get entry :row))))
            (agent-repl--switch-most-recent
             (nreverse closed)
             (lambda (entry) (agent-repl-roster-row-closed-by-user-at (plist-get entry :row)))))))

;;;; ---- The switcher's columns ---------------------------------------------

(defun agent-repl--switch-mark (entry)
  "Return the mark ENTRY's line opens with: its status as the sidebar shows it.
A closed or merged workspace is the sidebar's grey dot; a hidden one
draws `agent-repl--switch-hidden-glyph' in its status color; every other
row draws the sidebar's own mark for its status arm
\(`agent-repl-status-roster-mark').  A live local workspace no roster push
carries yet has no status to show, and draws the default glyph, grey."
  (let ((row (plist-get entry :row)))
    (cond
     ((null row)
      (propertize agent-repl-ws-state-icon-default 'face 'shadow))
     ((or (eq (plist-get entry :section) :merged)
          (agent-repl-roster-row-closed-p row))
      (propertize (agent-repl-status--glyph-char agent-repl-status-dot-glyph)
                  'face 'shadow))
     ((eq (plist-get entry :section) :hidden)
      (agent-repl-status-roster-mark (agent-repl-roster-row-status row)
                                     agent-repl--switch-hidden-glyph))
     (t
      (agent-repl-status-roster-mark (agent-repl-roster-row-status row))))))

(defun agent-repl--switch-state-label (entry)
  "Return the state column's word for ENTRY.
Where the workspace sits outranks how it would open: a hidden or merged
workspace says so, and every other says `agent-repl--switch-state-labels'.
\"merged\" is drawn in `agent-repl--color-merging-purple', the one color
every surface paints a merge in."
  (pcase (plist-get entry :section)
    (:hidden "hidden")
    (:merged (propertize "merged" 'face
                         (list :foreground agent-repl--color-merging-purple)))
    (_ (alist-get (plist-get entry :state) agent-repl--switch-state-labels ""))))

(defun agent-repl--switch-turn-age (entry now-ms)
  "Return how long ago ENTRY's last turn ended, read at NOW-MS.
The daemon's durable instant (`RosterRowLastTurnEnded'), drawn in the
one age format (`agent-repl-format-age').  A workspace none of whose
turns has ended says so rather than showing an age."
  (let ((at (agent-repl-roster-row-instant-ms (plist-get entry :row) :last-turn-ended)))
    (if at (agent-repl-format-age at now-ms) "no turn yet")))

(defun agent-repl--switch-commits (entry)
  "Return the commit column for ENTRY: its branch's commits past the merge-base.
Empty when the daemon states no count; zero is a real count and says so."
  (let ((count (agent-repl-roster-row-commit-count (plist-get entry :row))))
    (cond ((null count) "")
          ((= count 1) "1 commit")
          (t (format "%d commits" count)))))

(defun agent-repl--switch-affixation (alist now-ms)
  "Return the `affixation-function' for the switcher over ALIST, read at NOW-MS.
ALIST is `agent-repl--switch-display-alist''s.  Each line is its status
mark, the name, and three columns aligned past the longest name: the
state, how long ago its last turn ended, and how many commits its branch
holds past its merge-base.  The columns take `completions-annotations'
beneath any face a column states itself, so the merged label keeps its
purple (`agent-repl--switch-state-label')."
  (let ((column (+ 4 (apply #'max 0 (mapcar (lambda (cell) (string-width (car cell)))
                                            alist)))))
    (lambda (candidates)
      (mapcar
       (lambda (candidate)
         (let ((entry (cdr (assoc candidate alist))))
           (list candidate
                 (concat (agent-repl--switch-mark entry) " ")
                 (concat (propertize " " 'display `(space :align-to ,column))
                         (let ((columns (format "%-14s %-12s %s"
                                                (agent-repl--switch-state-label entry)
                                                (agent-repl--switch-turn-age entry now-ms)
                                                (agent-repl--switch-commits entry))))
                           (add-face-text-property 0 (length columns)
                                                   'completions-annotations t columns)
                           columns)))))
       candidates))))

(defun agent-repl--switch-table (alist now-ms)
  "Return the completion table offering ALIST's names with their columns.
A function table, because that is how a table states its
`affixation-function' (`agent-repl--switch-affixation').  ALIST's order is
the order offered (`agent-repl--switch-by-recency'), so the table also
states identity sort functions: a completion UI that sorts on its own
would otherwise undo it."
  (let ((names (mapcar #'car alist))
        (affixation (agent-repl--switch-affixation alist now-ms)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          `(metadata (affixation-function . ,affixation)
                     (display-sort-function . identity)
                     (cycle-sort-function . identity))
        (complete-with-action action names string predicate)))))

(defun agent-repl--switch-to-known-workspace (&optional all)
  "Pick a workspace and stand on it, opening it first if it is not here.

WHICH WORKSPACES ARE OFFERED: the current and recent ones
\(`agent-repl--switch-curated'), or with ALL non-nil every known one --
either way most recently updated first (`agent-repl--switch-by-recency').
Each line shows the workspace's status mark and its columns
\(`agent-repl--switch-affixation').

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
  (let* ((known (agent-repl--switch-candidates))
         (candidates (agent-repl--switch-display-alist
                      (agent-repl--switch-by-recency
                       (if all known (agent-repl--switch-curated known))))))
    (unless candidates
      (user-error "agent-repl: no known workspaces"))
    (let* ((choice (completing-read (if all "Switch to any workspace: "
                                      "Switch to workspace: ")
                                    (agent-repl--switch-table
                                     candidates (agent-repl--now-unix-ms))
                                    nil t))
           (entry (cdr (assoc choice candidates)))
           (ws (plist-get entry :ws))
           (ref (plist-get entry :ref)))
      (cond
       (ws
        (agent-repl--info ws "elisp.commands.switch-chosen ws=%s all=%s" ws (and all t))
        (agent-repl-host-request-switch ws 'picker))
       (ref
        (agent-repl--info (agent-repl--ws-current-log-name)
                          "elisp.commands.switch-opens-absent name=%s id=%s"
                          (plist-get entry :name) (plist-get ref :id))
        (let ((dispatched-at (agent-repl-host-editor-switch-count)))
          (agent-repl-verb-open ref)
          (agent-repl-verbs-select-minted
           ref dispatched-at #'agent-repl-verbs--land-on-tab)))
       (t
        (agent-repl--warn (agent-repl--ws-current-log-name)
                          "elisp.commands.switch-no-ref name=%S" choice)
        (user-error "agent-repl: %s has no daemon identity to open" choice))))))

(defun agent-repl-switch-to-project (&optional project)
  "Switch to a current or recent workspace, or to PROJECT.

With no argument, completes over the open and hidden workspaces, the
most recently merged and the ones the user most recently closed -- the
daemon's roster is the source of what exists -- most recently updated
first, and stands on the chosen one, opening it first when it is closed
or has no tab here.  See
`agent-repl--switch-to-known-workspace'; `agent-repl-switch-to-any-workspace'
offers every known workspace.

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

(defun agent-repl-switch-to-any-workspace ()
  "Switch to ANY known workspace, opening it first when it is not here.
Every workspace the daemon's roster carries, closed and merged ones
however old, beside every live local one: what `SPC p p' offers when the
work wanted is no longer current or recent.  See
`agent-repl--switch-to-known-workspace'."
  (interactive)
  (agent-repl--switch-to-known-workspace t))

(defvar agent-repl--opened-recent-cycle nil
  "Workspaces already visited by `agent-repl-open-most-recent-workspace'.
Reset when the cycle exhausts, so repeated presses walk the whole roster
rather than sticking on the second-most-recent workspace.")

(defun agent-repl--roster-open-names ()
  "Return the open rows' workspace names, in roster walk order.
A row whose workspace Emacs has not opened yet names none, so it is left out."
  (let (names)
    (dolist (row (agent-repl-verbs--all-rows))
      (unless (agent-repl-roster-row-closed-p row)
        (let* ((ref (agent-repl-roster-row-ref row))
               (ws (and ref (agent-repl--ws-by-ref-id (plist-get ref :id)))))
          (when ws (push ws names)))))
    (nreverse names)))

(defun agent-repl--open-thread-names ()
  "Return the open chat threads' workspace names, the last interacted with first.
A thread whose workspace Emacs has not opened yet names none, so it is left out."
  (delq nil (mapcar (lambda (row)
                      (agent-repl--ws-by-ref-id (plist-get (plist-get row :thread) :id)))
                    (agent-repl-roster-threads))))

(defun agent-repl--roster-recent-names ()
  "Return open workspace names, most recently selected first.
The open rows of the roster (`agent-repl--roster-open-names'), ordered by
the ONE selection-recency order (`agent-repl-roster-selection-recency-order'):
this session's switches, then the daemon's durable last-selected instant,
then never-selected rows in roster order -- the same order a close lands by.
Chat threads are not among them."
  (agent-repl-roster-selection-recency-order (agent-repl--roster-open-names)))

(defun agent-repl--recent-workspace-and-thread-names ()
  "Return open workspace AND chat thread names, most recently selected first.
`agent-repl--roster-recent-names' with the open threads
\(`agent-repl--open-thread-names') among the candidates, ordered by the
same selection-recency order.  A thread's activation feeds this session's
history as a workspace's does, so a thread looked at last leads.  A thread
carries no durable last-selected instant, so one this session never
selected follows every workspace that has one."
  (agent-repl-roster-selection-recency-order
   (append (agent-repl--roster-open-names) (agent-repl--open-thread-names))))

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

(defun agent-repl--gesture-log-ws (shown gesture)
  "Return the log target of a navigation GESTURE made from SHOWN.
SHOWN's own log name, or, standing on a placeholder (a startup before its
first tab opened), an explicit context naming GESTURE, because a
placeholder names no workspace sink.  The one target every navigation
gesture logs under."
  (or (agent-repl--ws-log-name shown)
      (list :agent-repl-context
            (format "a %s from a placeholder names no workspace" gesture))))

(defun agent-repl--jump-log-ws (shown)
  "Return the log target of a jump made from SHOWN.
The target every recency jump logs under (`agent-repl--gesture-log-ws')."
  (agent-repl--gesture-log-ws shown "jump"))

(defun agent-repl-switch-to-previous-workspace ()
  "Switch to the workspace OR chat thread selected before the one shown.
HOWEVER it was selected: the order is the ONE selection-recency order
over open workspaces and threads alike
\(`agent-repl--recent-workspace-and-thread-names'), which every
perspective activation feeds, so a tab click, a numeral, a picker, a
thread command or a close's landing all count.  The switch itself is an
activation, so pressing again comes back: the command toggles between the
two most recent, a thread being one of them when it was looked at last.
The selection-history walk (`agent-repl-switch-older') stays on the tabs.

No other open workspace or thread is a reported, logged no-op.  The
target is REQUESTED of the daemon, never switched to here."
  (interactive)
  (let* ((shown (agent-repl--ws-current-name))
         (log-ws (agent-repl--jump-log-ws shown))
         (target (car (remove shown (agent-repl--recent-workspace-and-thread-names)))))
    (if (null target)
        (progn
          (agent-repl--info log-ws "elisp.commands.previous-none from=%s" shown)
          (message "[agent-repl] No previous workspace or thread to switch to"))
      (agent-repl--info log-ws "elisp.commands.previous from=%s target=%s" shown target)
      (agent-repl-host-request-switch target 'previous))))

(defun agent-repl-switch-to-last-finished-workspace ()
  "Switch to the workspace that most recently FINISHED its turn.
FINISHED means its tab went from working (red) to done (green), and it
is still done: `agent-repl-roster-last-finished-name' ranks by the
daemon's durable turn-end instant, so a finish that landed while Emacs
was down or reconnecting counts as surely as one it watched.

The candidates are the DRAWN tabs (`agent-repl--drawn-tab-names') less
the workspace shown: the red-to-green change the user is answering is
the one the bar showed them, and switching to where they stand is no
switch.  No candidate is a reported, logged no-op.  The target is
REQUESTED of the daemon, never switched to here."
  (interactive)
  (let* ((shown (agent-repl--ws-current-name))
         (log-ws (agent-repl--jump-log-ws shown))
         (names (remove shown (agent-repl--drawn-tab-names)))
         (target (agent-repl-roster-last-finished-name names)))
    (if (null target)
        (progn
          (agent-repl--info log-ws "elisp.commands.last-finished-none from=%s candidates=%d"
                            shown (length names))
          (message "[agent-repl] No other workspace has finished"))
      (agent-repl--info log-ws "elisp.commands.last-finished from=%s target=%s ended-ms=%s"
                        shown target (agent-repl-roster-last-turn-ended-ms target))
      (agent-repl-host-request-switch target 'last-finished))))

(defun agent-repl--drawn-tab-names ()
  "Return the workspace names the tab bar DRAWS, in the order it draws them.

THE DRAWN ORDER IS NAVIGATION'S ONLY ORDER.  `agent-repl-roster-tab-order'
is the tab bar's own order (roster.el sets it from the daemon resolver's
walk, in stored place order), so left/right and the numeral chords walk the
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

(defun agent-repl--step-workspace (n ring-fn op trigger)
  "Request the DRAWN tab N steps from the current workspace along a ring.
RING-FN is called with the workspace the step starts from and answers
every tab in the ring's order (`agent-repl--cycle-target''s ALL); the drawn
tabs are the ones the step may land on.  OP names the gesture's records
\(`elisp.commands.OP', `elisp.commands.OP-no-position') and TRIGGER is the
switch request's (`agent-repl-host-request-switch').

THE STEP STARTS FROM THE MOST RECENT REQUEST while one is unanswered
\(`agent-repl-host-pending-selection'), not from the tab shown: the frame
moves only when the roster says so, and a key pressed twice before it does
must land two steps over, not one.  The target is REQUESTED, never
switched to here.

A bar with no tabs, and a start that is not in the ring (a pseudo
perspective, or a workspace whose tab the roster has torn down), are
LOGGED NO-OPS: there is no slot to count from, and inventing one would
land the user somewhere the picture never offered.

THE RECORD IS `info', NOT `debug'.  A step is an action a PERSON took, and
the durable threshold is `info' by default, so on the debug rung every switch
the user made left nothing on disk: realtest 4 drove four real chords, every
one switched correctly, and not one could be read back afterwards.  A routine
action is never promoted to `warn' to make it easier to find -- the realtest
harvest fails a run on every warning."
  (let* ((names (agent-repl--drawn-tab-names))
         (shown (agent-repl--ws-current-name))
         (current (or (agent-repl-host-pending-selection) shown))
         (all (funcall ring-fn current))
         (log-ws (agent-repl--gesture-log-ws shown op))
         (target (agent-repl--cycle-target all names current n)))
    (if (null target)
        (agent-repl--info log-ws (format "elisp.commands.%s-no-position n=%%d tabs=%%d from=%%s" op)
                          n (length names) current)
      (agent-repl--info log-ws (format "elisp.commands.%s n=%%d from=%%s target=%%s" op)
                        n current target)
      (agent-repl-host-request-switch target trigger))))

(defun agent-repl--workspace-cycle (n)
  "Switch N places from the current workspace along the DRAWN tab order.
Wraps at both ends, so `s-{' and `s-}' match the bar in both directions.
The ring is the roster's whole tab order, hidden tabs included, so a
current workspace whose repository is collapsed still steps to its drawn
neighbours (`agent-repl--step-workspace')."
  (agent-repl--step-workspace
   n (lambda (_current)
       (and (fboundp 'agent-repl-roster-tab-order) (agent-repl-roster-tab-order)))
   "cycle" 'cycle))

(defun agent-repl-switch-left ()
  "Switch to the tab LEFT of the current one on the tab bar, wrapping."
  (interactive)
  (agent-repl--workspace-cycle -1))

(defun agent-repl-switch-right ()
  "Switch to the tab RIGHT of the current one on the tab bar, wrapping."
  (interactive)
  (agent-repl--workspace-cycle 1))

(defun agent-repl--recency-ring (current)
  "Return the DRAWN tabs, and CURRENT, most recently selected first.
The ONE selection-recency order (`agent-repl-roster-selection-recency-order')
over the tabs the bar draws, so `s-[' and `s-]' reach exactly the tabs
`s-{' and `s-}' do.  CURRENT joins the ring when its tab is not drawn (its
repository is collapsed), so it still has a place to step from."
  (let ((drawn (agent-repl--drawn-tab-names)))
    (agent-repl-roster-selection-recency-order
     (if (or (null current) (member current drawn))
         drawn
       (append drawn (list current))))))

(defun agent-repl--recency-walk (n)
  "Switch N places along the selection history, positive being older.
`s-[' and `s-]' are `s-{' and `s-}' over the recency order rather than the
bar's (`agent-repl--recency-ring'), wrapping at both ends.

A STEP IS NOT A VISIT.  The switch is requested with the `recency-walk'
trigger, so neither the daemon's last-selected instant nor Emacs's own
history moves (`agent-repl--recency-walk-landing').  The order therefore
holds still under the walk, and the current workspace's place in it IS the
walk's position: `s-[' pressed again lands one older, and `s-]' comes back
toward the most recent, with no cursor to keep."
  (agent-repl--step-workspace n #'agent-repl--recency-ring "recency-walk" 'recency-walk))

(defun agent-repl-switch-older ()
  "Switch to the workspace viewed before the current one, without recording a view.
Pressed again, goes one further back in the selection history."
  (interactive)
  (agent-repl--recency-walk 1))

(defun agent-repl-switch-newer ()
  "Switch to the workspace viewed after the current one, without recording a view.
Walks back toward the most recently viewed workspace."
  (interactive)
  (agent-repl--recency-walk -1))

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

(defun agent-repl--tab-slot (char)
  "Return the tab slot the digit CHAR names: `1'..`9' are slots 1-9 and
`0' is slot 10, the digit row's own order."
  (if (eq char ?0) 10 (- char ?0)))

(defun agent-repl--read-tab-slot (prompt)
  "Read ONE digit after PROMPT and return the tab slot it names.
`read-char-choice' returns on the keystroke itself, so the digit needs no
RET to send, and any other key is refused in place."
  (agent-repl--tab-slot
   (read-char-choice (concat prompt " (1-9, 0 = 10): ")
                     (number-sequence ?0 ?9))))

(defun agent-repl--act-on-tab-slot (slot op verb)
  "Ask VERB to act on the current workspace and tab SLOT's workspace.
SLOT counts the DRAWN tabs (`agent-repl--drawn-tab-names'), exactly as the
numeral chords do, so the digit names the tab the user is looking at.  OP
is the gesture's word (\"swap\", \"move\"), naming its log records and its
messages; VERB is called with the current workspace and the tab's.

A current workspace with no tab, a slot past the end of the bar, and the
current workspace's own slot are REPORTED no-ops, recorded at `info' like
every other navigation gesture."
  (let* ((names (agent-repl--drawn-tab-names))
         (current (agent-repl--ws-current-name))
         (log-ws (agent-repl--gesture-log-ws current op))
         (target (and (<= 1 slot (length names)) (nth (1- slot) names))))
    (cond
     ((not (member current names))
      (agent-repl--info log-ws (format "elisp.commands.%s-no-tab current=%%s slot=%%d" op) current slot)
      (message "[agent-repl] The current workspace has no tab to %s" op))
     ((null target)
      (agent-repl--info log-ws (format "elisp.commands.%s-out-of-range slot=%%d tabs=%%d" op)
                        slot (length names))
      (message "[agent-repl] No workspace tab %d -- the bar draws %d" slot (length names)))
     ((equal target current)
      (agent-repl--info log-ws (format "elisp.commands.%s-own-slot slot=%%d ws=%%s" op) slot current)
      (message "[agent-repl] %s is already tab %d" current slot))
     (t
      (agent-repl--info log-ws (format "elisp.commands.%s slot=%%d ws=%%s target=%%s" op)
                        slot current target)
      (funcall verb current target)))))

(defun agent-repl-workspace-swap (slot)
  "Swap the current workspace's place in the registry order with tab SLOT's.
Interactively SLOT is read as one digit, `0' naming slot 10, and counts
the drawn tabs (`agent-repl--act-on-tab-slot').  The current workspace
ends up as that tab, the workspace that was there takes the current
workspace's old tab, and every other tab stays where it was.

The daemon owns the registry order, so the swap is REQUESTED of it
\(`agent-repl-verb-swap') and the bar redraws from the next roster push.
The sidebar follows the same order within each of its lists."
  (interactive (list (agent-repl--read-tab-slot "Swap with tab")))
  (agent-repl--act-on-tab-slot slot "swap" #'agent-repl-verb-swap))

(defun agent-repl-workspace-move (slot)
  "Move the current workspace onto tab SLOT, the tabs between shifting over.
Interactively SLOT is read as one digit, `0' naming slot 10, and counts
the drawn tabs (`agent-repl--act-on-tab-slot').  Moving right shifts the
tabs it passes left and moving left shifts them right, so the current
workspace takes the pressed tab's number; it carries its own children
with it.

The tab must be a sibling's in the sidebar's arrangement: another top-level
workspace of the same repository, or another child of the same parent.  A
top-level workspace also joins that tab's subsection.  The daemon owns the
arrangement, so the move is REQUESTED of it (`agent-repl-verb-move') and
the bar redraws from the next roster push."
  (interactive (list (agent-repl--read-tab-slot "Move onto tab")))
  (agent-repl--act-on-tab-slot slot "move" #'agent-repl-verb-move))

(eval-and-compile
  ;; The count is needed at EXPANSION time by the macro below and at RUN time
  ;; by the keymap `keybindings.el' builds from it, and one number has to
  ;; serve both or a tenth command appears with no chord on it.
  (defconst agent-repl-switch-numeral-count 9
    "How many workspace SLOT chords the module owns: `M-1\=' .. `M-9\='.
`M-0\=' is the module's too, but it names no slot: it is
`agent-repl-switch-to-last-workspace\=', the tab at the end of the bar."))

(defun agent-repl--last-tab-slot ()
  "The 1-based slot of the LAST tab on the drawn bar, never below 1.
An empty bar still answers 1, so the switch reports its own no-tabs miss."
  (max 1 (length (agent-repl--drawn-tab-names))))

(defun agent-repl-switch-to-last-workspace ()
  "Switch to the LAST tab on the drawn bar, however many it draws.
Bound to `M-0\=' and `s-0\=' in place of Doom's `+workspace/switch-to-final\=',
which indexes persp-mode's perspective list rather than the bar."
  (interactive)
  (agent-repl-switch-to-workspace (agent-repl--last-tab-slot)))

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
             `(min ,n (agent-repl--last-tab-slot))
           n))))))

(agent-repl--define-switch-numerals)

(provide 'commands)

;;; commands.el ends here
