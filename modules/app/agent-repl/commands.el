;;; commands.el --- user commands for agent-repl -*- lexical-binding: t; -*-

;;; Code:

;; Forward declarations: defined in worktree.el (loaded after commands.el).
;; Snapshot save/load helpers in this file refer to these symbols, so the
;; names must be readable here at compile/load time.
(defvar agent-repl-master-branch-name)
(declare-function agent-repl--frontend-dispatch-send "frontends")
(declare-function agent-repl--frontend-boot-session "frontends")
(declare-function agent-repl--runtime-startup-prepare "services" (on-success on-failure))

;; Forward declaration: defined in hide-project-dirs.el (loaded after
;; commands.el).  The snapshot writer/loader persists and restores this
;; toggle so the hidden set survives an Emacs restart.
(defvar agent-repl-hide-project-dirs-enabled)

;; Forward declaration: defined in frontends.el (loaded after
;; commands.el).  The snapshot writer/loader persists and restores the
;; frontend NEW workspaces are born with, so a `SPC o f' / `SPC o F'
;; choice survives an Emacs restart.
(defvar agent-repl-default-frontend)

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

(defcustom agent-repl-interrupt-reinsert-delay 0.25
  "Seconds to wait after interrupting before re-entering insert mode."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-interrupt-confirm t
  "When non-nil, `agent-repl-interrupt' confirms before cancelling subagents.
The prompt appears only when the targeted workspace has detached
background work in flight (its daemon-pushed render-state is
`:idle-async' — see `agent-repl--agent-subagents-running-p'); a
workspace whose main agent
alone is running, or that has no turn in flight at all, is interrupted
without a prompt, since there is no subagent work to protect.  Detached
background watchers and shells never count as a running subagent and are
never stopped by the interrupt, so they never raise the prompt.  The
confirmation guards against an accidental `C-c C-k' discarding spawned
subagent work mid-turn."
  :type 'boolean
  :group 'agent-repl)

;;;; Session helpers

(defun agent-repl--send-to-agent (text prompt-origin)
  "Send TEXT to Claude, starting it if needed.
Dispatches unconditionally through the frontend registry's `:send-fn'
\(`agent-repl--gui-send-turn', the gui being the only registered
frontend) rather than branching on frontend here.  The gui's send path
ensures the daemon session itself
\(`agent-repl--frontend-after-ensure-session', via
`agent-repl--frontend-send-user-message'), healing a stale binding or
creating a fresh one on demand, so no separate not-running check or
manual boot is needed at this call site.  Used by every
predefined-prompt command (e.g. `agent-repl-create-or-update-pr') as
well as `agent-repl-explain' and friends."
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log ws "send-to-agent len=%d prompt-origin=%s" (length text) prompt-origin)
    (agent-repl--frontend-dispatch-send ws text text prompt-origin)))

;;;; File reference helpers

(defun agent-repl--buffer-relative-path ()
  "Return the current buffer's file path relative to the project root."
  (let ((file (buffer-file-name))
        (ws (agent-repl--ws-current-name)))
    (unless file
      (agent-repl--log ws "buffer-relative-path: refusing non-file buffer=%s" (buffer-name))
      (user-error "Buffer %s is not visiting a file" (buffer-name)))
    (let ((rel (file-relative-name (agent-repl--path-canonical file) (agent-repl--ws-dir ws))))
      (agent-repl--log ws "buffer-relative-path: path=%s" rel)
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
          (agent-repl--log (agent-repl--ws-current-log-name) "format-file-ref: region branch start=%d end=%d" start-line end-line)
          (format "%s:%d-%d" rel start-line end-line))
      (agent-repl--log (agent-repl--ws-current-log-name) "format-file-ref: single-line branch line=%d" (line-number-at-pos (point)))
      (format "%s:%d" rel (line-number-at-pos (point))))))

(eval-when-compile
  ;; Register magit-hunk-section's `to-range' slot name with eieio so
  ;; the `eieio-oref' below compiles warning-free without magit on the
  ;; compile-time load path.  Compile-time only; defines nothing at runtime.
  (cl-defstruct (agent-repl--magit-hunk-slot-shim) to-range))

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
    (agent-repl--log (agent-repl--ws-current-log-name) "format-magit-hunk-ref: ref=%s" ref)
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
        (agent-repl--log (agent-repl--ws-current-log-name) "context-reference: magit-hunk branch")
        (agent-repl--format-magit-hunk-ref))
    (agent-repl--log (agent-repl--ws-current-log-name) "context-reference: standard branch")
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
         (win (agent-repl--link-code-display buf start-line end-line))
         (ws (or workspace (agent-repl--ws-current-name))))
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
      (agent-repl--log ws
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
    (agent-repl--log (agent-repl--ws-current-log-name) "diff-analysis: %s" change-spec)
    (agent-repl--send-to-agent msg "PROMPT_ORIGIN_COMMAND_DIFF_ANALYSIS")))

;; The whole cluster below — the scope tables and the two helpers — is
;; read by `agent-repl--define-diff-commands' AT MACROEXPANSION TIME
;; (the macro loops over `agent-repl--diff-scopes' and calls
;; `agent-repl--diff-command-form' while expanding), so byte-compiling
;; this file requires them at compile time as well as at load time.
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
    ;; This runs at macroexpansion time, where a standalone byte-compile
    ;; has not loaded core.el — hence the `fboundp' gates on the logger.
    (let ((override (and scope-overrides
                         (cdr (assq scope (eval scope-overrides))))))
      (cond
       (override
        (when (fboundp 'agent-repl--log)
          (agent-repl--log nil "resolve-change-spec: override branch scope=%s" scope))
        override)
       ((eq default-spec :use-branch-diff-spec)
        (when (fboundp 'agent-repl--log)
          (agent-repl--log nil "resolve-change-spec: branch-spec branch scope=%s" scope))
        'agent-repl-branch-diff-spec)
       (t
        (when (fboundp 'agent-repl--log)
          (agent-repl--log nil "resolve-change-spec: default branch scope=%s" scope))
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
         (agent-repl--send-diff-analysis ,change-spec-form ,prompt-var)))))

(defmacro agent-repl--define-diff-commands (family doc-verb prompt-var &optional scope-overrides)
  "Define 5 diff-analysis commands for FAMILY.

Each generated command is named `agent-repl-FAMILY-SCOPE' for SCOPE in
worktree, staged, uncommitted, head, and branch.  DOC-VERB is used in
docstrings (e.g. \"Explain\" produces \"Explain unstaged changes.\").
PROMPT-VAR is the symbol of the prompt variable to pass.

SCOPE-OVERRIDES, when non-nil, is a symbol naming an alist of
\(SCOPE . CHANGE-SPEC) that replaces the default change-spec from
`agent-repl--diff-scopes' for specific scopes."
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
    (agent-repl--log (agent-repl--ws-current-log-name) "explain %s" msg)
    (agent-repl--send-to-agent msg "PROMPT_ORIGIN_COMMAND_EXPLAIN_CONTEXT")))

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
      (agent-repl--log (agent-repl--ws-current-log-name) "explain-prompt %s" msg)
      (agent-repl--send-to-agent msg "PROMPT_ORIGIN_COMMAND_EXPLAIN_PROMPT"))
    (when (or (null msg) (string-empty-p msg))
      (agent-repl--log (agent-repl--ws-current-log-name)
                        "explain-prompt: empty input; no message sent"))))

(defun agent-repl--enter-insert-mode (ws)
  "Re-enter evil insert state in WS's input buffer after an interrupt.
Switches the Emacs-side input buffer back to evil insert state so the
user can keep typing where they left off.

Does NOT send a literal \"i\" keystroke to Claude.  The input buffer is
the only surface the user types into, so forwarding \"i\" anywhere else
would both double-dispatch the mode switch (evil already owns insert
mode) and leak a stray \"i\" character onto Claude's prompt line, which
then prefixes the next message the user sends.

No-op when WS is not the current workspace (a programmatically
triggered interrupt on a background workspace must not steal focus or
flip a hidden buffer's state) or when the input buffer is dead."
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

(defun agent-repl--restore-retracted-prompt (ws)
  "Put WS's retracted prompt back in its input buffer for revision.
Called only once the frontend reports the daemon actually withdrew the
turn, so the feed has already given the prompt up — this is what keeps
the text from being lost rather than a courtesy.

The RAW text is restored, never the prepared text: the metaprompt
decoration was never the user's to revise.  A non-empty input buffer is
left alone and the prompt is logged instead of overwriting a draft the
user has since typed; losing the draft to save the prompt would just
trade one loss for another.

Clears `:sent-turn' either way: the turn is withdrawn, so there is
nothing left to undo."
  (let* ((sent (agent-repl--ws-get ws :sent-turn))
         (raw (plist-get sent :raw))
         (buf (agent-repl--ws-get ws :input-buffer)))
    (agent-repl--ws-put ws :sent-turn nil)
    (if (not (buffer-live-p buf))
        (agent-repl--log ws "interrupt: retracted %S but the input buffer is dead" raw)
      (with-current-buffer buf
        (if (not (zerop (buffer-size)))
            (agent-repl--log ws "interrupt: retracted prompt not restored over a draft: %S" raw)
          (agent-repl--history-replace-buffer-text raw)
          (agent-repl--history-reset)
          (agent-repl--log ws "interrupt: restored retracted prompt (%d chars)" (length raw)))))))

(defun agent-repl--agent-thinking-p (ws)
  "Return non-nil when workspace WS has a Claude turn actively in flight.
That is its `:agent-state' is `:thinking'.  This is narrower than
`agent-repl--agent-running-p' (session liveness): a workspace with a live
session but no in-flight turn is NOT thinking.  Detached background work
\(a watcher, a backgrounded shell) does not make the agent thinking, so a
workspace carrying only such work returns nil here."
  (eq (agent-repl--ws-agent-state ws) :thinking))

(defun agent-repl--agent-subagents-running-p (ws)
  "Return non-nil when workspace WS has detached background work in flight.
Re-keyed in the agent-shim cutover (design §10): the Emacs-side
`:pending-subagents' hook counter was deleted, so this reads the
daemon-pushed render-state instead.  `:idle-async' is the SSM's
resolved \"turn done, but backgrounded tasks (subagents/shells) are
still running\" state — exactly the detached work this guard protects.
The main agent's own in-flight turn resolves to `:thinking', not
`:idle-async', so interrupting an ordinary turn is NOT gated (matching
the prior intent: the guard fires only when background work is at
stake).  This gates the `C-c C-k' cancel confirmation (see
`agent-repl--confirm-cancel-running')."
  (eq (agent-repl--ws-render-status ws) :idle-async))

(defun agent-repl--confirm-cancel-prompt (running)
  "Return the confirmation prompt string naming the RUNNING workspaces.
RUNNING is a non-empty list of workspace names that have subagents in
flight."
  (if (= (length running) 1)
      (format "Cancel the running subagents in %s? " (car running))
    (format "Cancel the running subagents in %d workspaces (%s)? "
            (length running)
            (string-join running ", "))))

(defun agent-repl--confirm-cancel-running (wss)
  "Return non-nil if cancelling the running subagents among WSS is confirmed.
WSS is a list of workspace names.  Returns t WITHOUT prompting when
`agent-repl-interrupt-confirm' is nil, or when none of WSS has a subagent
in flight (see `agent-repl--agent-subagents-running-p') — there is
nothing to confirm.  A workspace whose main agent alone is running (no
spawned subagents), and detached watchers or shells, never raise the
prompt.  Otherwise prompts once, naming the workspaces with subagents,
and returns the user's answer."
  (let ((running (seq-filter #'agent-repl--agent-subagents-running-p wss)))
    (or (not agent-repl-interrupt-confirm)
        (null running)
        (y-or-n-p (agent-repl--confirm-cancel-prompt running)))))

(defun agent-repl-interrupt (&optional ws no-confirm)
  "Interrupt Claude in workspace WS and re-enter insert mode after a delay.
Sends Escape to stop the current operation, then automatically returns
the input buffer to evil insert state after
`agent-repl-interrupt-reinsert-delay' seconds (via
`agent-repl--enter-insert-mode', which switches evil state rather than
forwarding a literal \"i\" keystroke to Claude).  Defaults to the
current workspace when WS is nil (matches the interactive `SPC o x'
behavior); programmatic callers pass WS so interrupts target a
specific workspace.

When the targeted workspace has detached background work in flight (its
daemon-pushed render-state is `:idle-async') and NO-CONFIRM is nil, asks for
confirmation before cancelling, unless `agent-repl-interrupt-confirm' is
nil; declining aborts without interrupting.  A main agent running on its
own (no spawned subagents) is interrupted without a prompt.  Only the
running Claude agent is stopped — detached watchers and shells keep
running (see `agent-repl--confirm-cancel-running').  Batch callers that
confirm once for the whole set pass NO-CONFIRM non-nil to suppress the
per-target prompt.

Interrupting BEFORE the agent has answered is semantically an undo, so
this doubles as one: the frontend asks the daemon to retract the sent
turn, and when it does — the prompt's bubble leaving the feed — the
prompt lands back in the input buffer to be revised and sent again
\(`agent-repl--restore-retracted-prompt').  Once the agent has answered,
there is a response on screen to keep, so the turn stands and this is
an ordinary stop.  The daemon owns that judgment; Emacs only asks.

After issuing the escape, marks the workspace's agent-state as
`:done' — interrupting terminates the in-flight turn, so the tab
should immediately reflect \"finished\" rather than linger on
`:thinking'.  (The Stop / SubagentStop tracking clear was dropped in
the agent-shim cutover; that hook counter no longer exists.)"
  (interactive)
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (agent-repl--log ws "interrupt: requested no-confirm=%s reinsert-delay=%.3fs"
                      no-confirm agent-repl-interrupt-reinsert-delay)
    (if (and (not no-confirm)
             (not (agent-repl--confirm-cancel-running (list ws))))
        (agent-repl--log ws "interrupt: declined at confirmation, leaving agent running")
      ;; The frontend's interrupt capability returns non-nil only when the
      ;; interrupt was actually issued (a dead/unbound session returns nil);
      ;; the done-marking must not fire for an undelivered interrupt.
      (let ((outcome (agent-repl--frontend-dispatch-interrupt ws 'escape)))
        (if (not outcome)
            (agent-repl--log ws "interrupt: frontend reported not delivered, skipping")
          (agent-repl--mark-agent-done ws)
          ;; Restore before the re-insert timer so the prompt is already
          ;; there to revise when the buffer takes insert state.
          (when (eq outcome 'retracted)
            (agent-repl--restore-retracted-prompt ws))
          (agent-repl--log ws "interrupt: delivered outcome=%s retracted=%s scheduling-insert=%.3fs"
                            outcome (if (eq outcome 'retracted) "t" "nil")
                            agent-repl-interrupt-reinsert-delay)
          (run-at-time agent-repl-interrupt-reinsert-delay nil
                       #'agent-repl--enter-insert-mode ws))))))

;; The in-flight message-queue commands (agent-repl-queue-run-now /
;; agent-repl-queue-cancel and their picker helpers) were deleted in the S9
;; endgame: the queue plane is retired daemon-side (frontend.v1 has no
;; queue-control command and no queue snapshot), and the webapp owns the
;; queued-message UI end to end.

(defun agent-repl-update-pr ()
  "Ask Claude to update the PR description for the current branch."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "update-pr: sending update-pr prompt")
  (agent-repl--send-to-agent agent-repl-update-pr-prompt "PROMPT_ORIGIN_COMMAND_UPDATE_PR"))

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
        (agent-repl--send-to-agent agent-repl-rebase-onto-origin-master-prompt "PROMPT_ORIGIN_COMMAND_REBASE"))
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

(defun agent-repl--exclusion-symbol-to-flag (sym &optional ws)
  "Convert exclusion SYM to the corresponding flag.
E.g. \\='no-self-certified becomes \"--self-certified\"."
  (let ((name (symbol-name sym)))
    (unless (string-prefix-p "no-" name)
      (agent-repl--log ws "exclusion-symbol-to-flag: invalid sym=%S name=%s" sym name)
      (error "agent-repl: exclusion symbol must start with `no-': %S" sym))
    (let ((flag (concat "--" (substring name 3))))
      (agent-repl--log ws "exclusion-symbol-to-flag: sym=%S -> flag=%s" sym flag)
      flag)))

(defun agent-repl--build-create-or-update-pr-prompt (excluded &optional ws)
  "Build the /create-or-update-pr prompt, omitting flags for EXCLUDED.
EXCLUDED is a list of `no-FLAG' symbols (e.g. \\='no-self-certified).  Each
must correspond to a flag in `agent-repl-create-or-update-pr-base-flags'
or an error is signalled."
  (let ((excluded-flags
         (mapcar (lambda (sym)
                   (let ((flag (agent-repl--exclusion-symbol-to-flag sym ws)))
                     (unless (member flag agent-repl-create-or-update-pr-base-flags)
                       (agent-repl--log ws "build-create-or-update-pr-prompt: invalid exclusion sym=%S flag=%s base-flags=%S"
                                         sym flag agent-repl-create-or-update-pr-base-flags)
                       (error "agent-repl: %S excludes %s, not in base flags" sym flag))
                     flag))
                 excluded)))
    (let ((prompt (string-join
                   (cons "/create-or-update-pr"
                         (cl-remove-if (lambda (f) (member f excluded-flags))
                                       agent-repl-create-or-update-pr-base-flags))
                   " ")))
      (agent-repl--log ws "build-create-or-update-pr-prompt: excluded=%S excluded-flags=%S prompt=%s"
                        excluded excluded-flags prompt)
      prompt)))

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
         (base (agent-repl--build-create-or-update-pr-prompt excluded ws))
         (input-buf (agent-repl--ws-get ws :input-buffer))
         (raw-prefix (agent-repl--read-input-buffer ws))
         (prefix (and raw-prefix (string-trim-right raw-prefix)))
         (has-prefix (and prefix (not (string-empty-p prefix))))
         (prompt (if has-prefix (concat prefix " " base) base)))
    (agent-repl--log ws "create-or-update-pr: prefix-len=%d prompt=%s"
                      (length (or prefix "")) prompt)
    (agent-repl--send-to-agent prompt "PROMPT_ORIGIN_COMMAND_CREATE_OR_UPDATE_PR")
    (when (and has-prefix input-buf (buffer-live-p input-buf))
      (agent-repl--commit-input-buffer ws input-buf raw-prefix t))
    (unless (and has-prefix input-buf (buffer-live-p input-buf))
      (agent-repl--log ws "create-or-update-pr: input not committed has-prefix=%s input-buffer=%s live=%s"
                        (if has-prefix "t" "nil") input-buf
                        (if (buffer-live-p input-buf) "t" "nil")))))

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
  (let* ((ws (agent-repl--ws-current-name))
         (prompt (agent-repl--build-create-or-update-pr-prompt excluded ws)))
    (agent-repl--log ws
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
callers (e.g. the workspace-commands dispatch) pass WS directly to
skip the prompt.

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
         (t0 (float-time))
         (agent-repl--kill-cause (or agent-repl--kill-cause
                                     "interactive nuke command (agent-repl-nuke-workspace)")))
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
    (agent-repl--log (agent-repl--ws-current-log-name) "nuke-all-workspaces: count=%d" count)
    ;; Snapshot keys before iterating; each call mutates the hash.
    (let ((agent-repl--kill-cause "interactive nuke-all command (agent-repl-nuke-all-workspaces)"))
      (dolist (ws known)
        (agent-repl--nuke-one-workspace ws)))
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
    (agent-repl--log (agent-repl--ws-current-log-name)
                      "nuke-restored-workspaces: count=%d" count)
    (let ((agent-repl--kill-cause "interactive nuke-restored command (agent-repl-nuke-restored-workspaces)"))
      (dolist (ws restored)
        (agent-repl--nuke-one-workspace ws)))
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
list.  Programmatic callers (e.g. the workspace-commands dispatch)
pass WS directly to skip the prompt.

No confirmation prompt: teardown is immediate.  Persisted state.el is
preserved, so re-opening the workspace later resumes the Claude
session — accidental invocations are easily recoverable."
  (interactive)
  (let* ((ws (or ws (agent-repl--read-nukeable-workspace "Kill workspace: ")))
         (agent-repl--kill-cause (or agent-repl--kill-cause
                                     "interactive kill command (agent-repl-kill-workspace)"))
         (action (agent-repl--nuke-or-kill-workspace ws)))
    (force-mode-line-update t)
    (message (if (eq action 'nuke)
                 "Killed workspace: %s"
               "Killed persp workspace: %s")
             ws)))

(defun agent-repl-copy-reference ()
  "Copy the current file and line reference to the clipboard.
With active region: copies file:startline-endline.
Without region: copies file:line."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "copy-reference: copying file reference")
  (let ((ref (agent-repl--format-file-ref)))
    (kill-new ref)
    (message "Copied: %s" ref)))

(defun agent-repl-paste-clipboard ()
  "Insert the current workspace's `:clipboard' text at point.
The slot is populated by a daemon HostAction carrying the legacy
`clipboard' command (see `agent-repl--handle-clipboard-command') — a
per-workspace clipboard, deliberately distinct from the OS clipboard.

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

(defvar agent-repl--snapshot-materialized-pending nil
  "Names of daemon-materialized workspaces still missing from the roster file.
A workspace materialized before the snapshot loader has finished (the
boot-resume path: the daemon replays a `WorkspaceAvailable' while the
recursive loader is still walking the on-disk roster) cannot write the
roster immediately — `agent-repl--snapshot-save-safe-p' refuses, and
rightly so: the live hash is still a partial view of the file.  The name
is parked here instead and
`agent-repl--snapshot-flush-materialized-pending' issues one roster write
once the loader has finished and the live hash is authoritative again.

Names only.  The flush re-collects from the live hash, so a workspace
closed between materialization and flush is simply absent from the write
rather than resurrected by it.")

(defvar agent-repl--restored-workspaces nil
  "List of workspace names established by snapshot-restore in this session.
Populated incrementally as each entry of the snapshot loader (either
the current file or an archived file via
`agent-repl-load-workspace-snapshot-from-archive') successfully calls
`agent-repl--establish-workspace'.  Used by
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

(defun agent-repl--worktree-snapshot-fields (ws)
  "Return the durable worktree-identity plist fragment for workspace WS.
Carries `:worktree-p' and `:source-ws-dir' (each only when set) so the
roster snapshot (`agent-repl--collect-snapshot-entries') preserves what
`agent-repl--picker-recreate-directory' needs to rebuild a DELETED
worktree.  These two fields normally live in the per-project
`<root>/.claude/emacs/state.el', but that file is destroyed along with
the worktree directory, so the roster is the only place they survive a
worktree deletion — without them revival degrades to a plain,
repo-less directory instead of a re-added worktree."
  (let ((worktree-p (agent-repl--ws-get ws :worktree-p))
        (source (agent-repl--ws-get ws :source-ws-dir)))
    (append (when worktree-p (list :worktree-p worktree-p))
            (when source (list :source-ws-dir source)))))

(defun agent-repl--collect-snapshot-entries ()
  "Return a list of workspace snapshot entries.
Each entry has the shape
\(NAME :project-dir DIR [:nuked-at TIME] [:hidden-project-dir t]
      [:worktree-p t] [:source-ws-dir DIR]).
Sourced from `agent-repl--workspaces'.  Includes every workspace
whose plist has a non-nil `:project-dir'.  `:priority' is deliberately
NOT included — it lives in each project's `<root>/.claude/emacs/state.el'
so the roster doesn't become a second source of truth.

`:worktree-p'/`:source-ws-dir' ARE the deliberate exception to that
state.el-is-authoritative rule (appended via
`agent-repl--worktree-snapshot-fields'): a deleted worktree takes its
in-worktree state.el with it, so without a durable roster copy the
revival path (`agent-repl--picker-recreate-directory') loses the source
repo and can only make a plain empty directory rather than re-add the
worktree.  Each is emitted only when set.

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
        (dolist (ws (agent-repl--ws-registered-names))
          (let* ((plist (agent-repl--ws-plist ws))
                 (dir (plist-get plist :project-dir)))
            (when dir
              (let ((tomb (plist-get plist :nuked-at))
                    (hidden (plist-get plist :hidden-project-dir)))
                (push (cons ws (append
                                (if tomb
                                    (append (list :project-dir dir :nuked-at tomb)
                                            (when hidden (list :hidden-project-dir t)))
                                  (list :project-dir dir))
                                (agent-repl--worktree-snapshot-fields ws)))
                      result)))))
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
                         (cons ws (append (list :project-dir dir)
                                          (agent-repl--worktree-snapshot-fields ws)))))
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
                                            (when hidden (list :hidden-project-dir t))
                                            (agent-repl--worktree-snapshot-fields ws))))))
                     (agent-repl--ws-tombstoned-names)))))
      (append live-entries tomb-entries))))

(defun agent-repl--snapshot-raw-format (raw)
  "Classify the RAW sexp read from a workspace-snapshot file.
Returns `:plist' when RAW is a plist (top-level keyword keys — the
current format, keyed `:workspaces' plus the session toggles),
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

(defun agent-repl--snapshot-plist-key-from-raw (raw key)
  "Return KEY's value from RAW when RAW is in the current plist format.
RAW is a parsed workspace-snapshot sexp.

Every key EXCEPT `:workspaces' is plist-only: each was added after the
legacy list-of-entries format, so for those keys `RAW is legacy' and
`RAW is a plist lacking KEY' collapse to the same answer — nil.  That
collapse is the whole shape shared by `:hide-project-dirs-enabled' and
`:default-frontend', which is why they read through here rather than
each restating the pcase.

`agent-repl--snapshot-entries-from-raw' is deliberately NOT one of them:
`:workspaces' is the one key the legacy format also carries (as the
entire sexp), so it needs a real `:legacy' branch and keeps its own
reader."
  (pcase (agent-repl--snapshot-raw-format raw)
    (:plist (plist-get raw key))
    (_ nil)))

(defun agent-repl--read-workspace-snapshot (file)
  "Read FILE and return a plist with the parsed snapshot contents.
Returned shape: `(:workspaces ENTRIES :hide-project-dirs-enabled BOOL
:default-frontend SYMBOL)'.

Normalizes both legacy (`((ws :project-dir dir) ...)') and current
plist-shaped files into the plist return shape so callers don't need
to branch on disk layout.  Returns nil when FILE does not exist or
the sexp is unreadable."
  (when (and file (file-exists-p file))
    (condition-case err
        (let ((raw (agent-repl--read-sexp-file file)))
          (list :workspaces (agent-repl--snapshot-entries-from-raw raw)
                :hide-project-dirs-enabled
                (agent-repl--snapshot-plist-key-from-raw raw :hide-project-dirs-enabled)
                :default-frontend
                (agent-repl--snapshot-plist-key-from-raw raw :default-frontend)))
      (error
       (agent-repl--log nil "read-workspace-snapshot: read err file=%s err=%S"
                         file err)
       nil))))

(defun agent-repl--write-workspace-snapshot (snapshot)
  "Write SNAPSHOT (a list of workspace entries) to
`agent-repl-workspace-snapshot-file' in the plist format
`(:workspaces SNAPSHOT :hide-project-dirs-enabled BOOL
:default-frontend SYMBOL)'.

Merge queueing is DAEMON-OWNED, so no queue or in-flight-merge state is
persisted here — a workspace's queue position and depth arrive on its
pushed `WorkspaceState' and are rendered, never restored.

`:hide-project-dirs-enabled' records the live
`agent-repl-hide-project-dirs-enabled' toggle so a session restore
reconstructs the hidden set.

`:default-frontend' records the live `agent-repl-default-frontend' so a
`SPC o f' / `SPC o F' choice keeps governing the workspaces created
after an Emacs restart, not just the ones created before it.

Creates the parent directory if missing and archives the previous file
before overwriting.  Caller is responsible for any pre-write checks
\(e.g. `--snapshot-save-safe-p' or interactive confirmation)."
  (agent-repl--log nil "write-sexp-file: file=%s" agent-repl-workspace-snapshot-file)
  (let ((dir (file-name-directory agent-repl-workspace-snapshot-file)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (agent-repl--archive-workspace-snapshot)
  (progn
    (with-temp-file agent-repl-workspace-snapshot-file
      (insert "(:workspaces (")
      (let ((first t))
        (dolist (entry snapshot)
          (unless first (insert "\n               "))
          (setq first nil)
          (prin1 entry (current-buffer))))
      (insert ")\n :hide-project-dirs-enabled ")
      (prin1 (and (boundp 'agent-repl-hide-project-dirs-enabled)
                  agent-repl-hide-project-dirs-enabled
                  t)
             (current-buffer))
      (insert "\n :default-frontend ")
      (prin1 (and (boundp 'agent-repl-default-frontend)
                  agent-repl-default-frontend)
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
      (agent-repl--log nil "save-workspace-snapshot: wrote entries=%d interactive=%s file=%s"
                        (length snapshot) (if (called-interactively-p 'interactive) "t" "nil")
                        agent-repl-workspace-snapshot-file)
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
      (agent-repl--log nil "update-workspace-snapshot: declined overwrite live=%d on-disk=%d file=%s"
                        live-count on-disk-count file)
      (user-error "Aborted"))
    (agent-repl--log nil "update-workspace-snapshot: accepted live=%d on-disk=%d file=%s"
                      live-count on-disk-count file)
    (agent-repl--write-workspace-snapshot snapshot)
    (message "Updated snapshot: %d workspace(s) -> %s"
             live-count file)))

(defun agent-repl--snapshot-persist-materialized-workspace (ws)
  "Durably record daemon-materialized WS in the roster snapshot.

Materialization (`agent-repl--ws-materialize-daemon-workspace') commits a
workspace to `agent-repl--workspaces' and the tab-bar without starting a
session, so it never reaches `agent-repl--state-save' — the piggyback that
rewrites the roster for every normally-opened workspace.  Without this
call the new workspace lives only in memory and vanishes at the next
Emacs restart, its worktree and daemon session still on disk.  This is
the materialization path's equivalent of that piggyback: roster only, no
per-project `state.el', because a later real session-init still owns that
file.

Two cases, and neither may silently drop the workspace:

- Loader already finished this session (`agent-repl--snapshot-loaded-p'):
  the live hash is authoritative, so write the roster now.
- Loader has not finished (boot-resume replay, or a session with no
  roster file at all): park WS in
  `agent-repl--snapshot-materialized-pending' so
  `agent-repl--snapshot-flush-materialized-pending' writes it once the
  loader finishes, and still attempt the write — on a fresh install
  `agent-repl--snapshot-save-safe-p' permits it (nothing to lose), and
  when a richer roster is on disk that guard refuses and the parked name
  is what carries the workspace to the flush."
  (if agent-repl--snapshot-loaded-p
      (progn
        (agent-repl--log ws "snapshot-persist-materialized: writing roster now ws=%s" ws)
        (agent-repl-save-workspace-snapshot))
    (cl-pushnew ws agent-repl--snapshot-materialized-pending :test #'equal)
    (agent-repl--log
     ws
     "snapshot-persist-materialized: loader unfinished — parked ws=%s pending=%d"
     ws (length agent-repl--snapshot-materialized-pending))
    (agent-repl-save-workspace-snapshot)))

(defun agent-repl--snapshot-flush-materialized-pending ()
  "Write the roster once for workspaces materialized before the load finished.
No-op when nothing was parked.  The pending list is cleared before the
write so a failing write cannot make the flush re-enter on the next load.

The write re-collects from the live hash rather than replaying the parked
names, so a workspace closed between its materialization and this flush
stays out of the roster — parking records that a write is owed, never
what the write must contain."
  (when agent-repl--snapshot-materialized-pending
    (let ((pending agent-repl--snapshot-materialized-pending))
      (setq agent-repl--snapshot-materialized-pending nil)
      (agent-repl--log nil
                       "snapshot-flush-materialized: flushing pending=%d names=%S"
                       (length pending) pending)
      (agent-repl-save-workspace-snapshot))))

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
          (set-window-buffer (car foreign) fallback)))))
    (unless foreign
      (agent-repl--log ws "clean-frame-foreign-windows: ws=%s no foreign windows total=%d"
                        ws (length all)))))

(defun agent-repl--hydrate-and-reorder-on-open (ws project-root)
  "Hydrate WS's display state from PROJECT-ROOT then reseat WS by priority.
Shared post-open step for every path that opens or activates a
workspace — interactive `SPC p p' (`agent-repl-switch-to-project'),
the snapshot/worktree restore path (`agent-repl--establish-workspace'),
and any future opener.  Centralizing the sequence guarantees that a
workspace lands in priority order no matter how it was opened.

Steps:

- hydrates the persisted priority badge and workspace state glyphs
  via `agent-repl--load-display-state', so `:priority' is in memory
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
  (let ((load-available (fboundp 'agent-repl--load-display-state))
        (reorder-available (fboundp 'agent-repl--reorder-workspace-by-priority))
        (snapshot-loading (and agent-repl--snapshot-load-state t)))
    (agent-repl--log ws "hydrate-and-reorder-on-open: ws=%s root=%s load-available=%s reorder-available=%s snapshot-loading=%s"
                      ws project-root load-available reorder-available snapshot-loading)
    (when load-available
      (agent-repl--load-display-state ws project-root))
    (when (and reorder-available (not snapshot-loading))
      (agent-repl--reorder-workspace-by-priority ws))))

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
  rehydrates persisted display state (`:priority' and the workspace
  state badges) from the per-project state file via
  `agent-repl--load-display-state',
- reorders the ws in `persp-names-cache' by its hydrated `:priority'
  via `agent-repl--reorder-workspace-by-priority' (snapshot
  restores, worktree hydration), matching what
  `agent-repl-set-priority' does for user-driven changes,
  - SKIPPED while a snapshot load is in flight
    (`agent-repl--snapshot-load-state' non-nil): the loader visits
    entries in saved tab-bar order and per-entry priority reseating
    would shuffle them back into priority order, defeating
    `agent-repl--collect-snapshot-entries' order preservation,
- starts the agent unless already running, through the workspace's own
  frontend (`agent-repl--frontend-boot-session') — the vterm process for
  a vterm workspace, a background daemon session (resuming the durable
  claude session) for a gui one."
  (agent-repl--log ws "establish-workspace: begin ws=%s dir=%s" ws dir)
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
    (let ((recent-file (agent-repl--most-recent-project-file dir)))
      (if (and recent-file (file-exists-p recent-file))
          (progn
            (agent-repl--log ws "establish-workspace: opening recent-file=%s" recent-file)
            (find-file recent-file))
        (agent-repl--log ws "establish-workspace: no existing recent-file candidate=%s" recent-file)))
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
    ;; Boot the agent through the workspace's own frontend — the same
    ;; door worktree creation uses, so a restored workspace and a
    ;; freshly generated one agree on what they come up as.  The vterm
    ;; boot is the classic pre-start; the gui boot ensures a daemon
    ;; session (background — no webview until the user opens the panel)
    ;; that resumes the workspace's durable claude session, so a restored
    ;; gui workspace continues its conversation.  A workspace with no
    ;; DELIBERATE frontend choice (`:frontend-explicit') restores under
    ;; the current default rather than under whatever it happened to boot
    ;; last time.
    (agent-repl--log ws "establish-workspace: booting frontend")
    (agent-repl--frontend-boot-session ws)
    (agent-repl--log ws "establish-workspace: complete ws=%s dir=%s" ws dir)))

(defvar agent-repl--snapshot-load-state nil
  "Plist describing an in-progress recursive snapshot load, or nil.
Keys: `:queue' (list of (NORMALIZED-WS . PLIST) entries still to do),
`:origin' (workspace to switch back to at end), `:awaiting' (ws-name
the loader is currently waiting on a ready signal for, or nil),
`:loaded' (successfully established + ready/awaiting), `:skipped'
(dir missing/nil), `:load-error' (establish-workspace signaled, or the
per-entry watchdog timed the entry out — see
`agent-repl--snapshot-load-timeout', which reclassifies the entry out
of `:loaded' into this counter),
`:total' (entry count from the file), `:timeout-timer' (the per-entry
watchdog timer).

Non-nil means a load is in flight — concurrent invocations of
`agent-repl-load-workspace-snapshot' are refused via a guard.")

(defcustom agent-repl-snapshot-load-per-entry-timeout 30
  "Per-entry watchdog in seconds for the recursive snapshot loader.
If the awaited workspace's `agent-repl--on-session-start-event' hasn't
fired by then, `agent-repl--snapshot-load-timeout' faults that one
workspace — counting it under `:load-error' and surfacing it through
`agent-repl--error' — and the loader advances to the next entry anyway.
Readiness is never synthesized for the timed-out workspace.  Tuned long
enough for a first-time claude startup but short enough that a wedged
workspace doesn't lock the entire load."
  :type 'number
  :group 'agent-repl)

(defun agent-repl--snapshot-load-ws-ready-p (ws)
  "Return non-nil when WS already has a live agent session.
Dispatches through `agent-repl--agent-running-p' (the frontend-agnostic
liveness check), rather than reading a vterm-specific readiness flag —
this used to read the vterm buffer-local `agent-repl--ready', which is
always nil for a gui workspace, so this predicate always reported
not-ready and the snapshot loader always fell through to arming its
per-entry watchdog timer instead of recognizing a workspace that was
already up (e.g. the origin ws the user was sitting in when load
began)."
  (agent-repl--agent-running-p ws))

(defun agent-repl--snapshot-load-cancel-timer ()
  "Cancel the pending per-entry watchdog timer, if any."
  (when-let ((timer (and agent-repl--snapshot-load-state
                         (plist-get agent-repl--snapshot-load-state :timeout-timer))))
    (when (timerp timer) (cancel-timer timer))
    (setq agent-repl--snapshot-load-state
          (plist-put agent-repl--snapshot-load-state :timeout-timer nil))))

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
           (load-error (or (plist-get state :load-error) 0)))
      ;; persp-mode saved origin's window-config when the loader's first
      ;; `--establish-workspace' switched away from it, so this switch-back
      ;; replays that layout — and persp-mode's restore filters foreign
      ;; buffers, so panels owned by some other ws can't bleed in.
      (when (and origin
                 (agent-repl--ws-exists-p origin))
        (agent-repl--ws-frame-switch origin))
      (force-mode-line-update t)
      (setq agent-repl--snapshot-loaded-p t)
      ;; Any workspace the daemon materialized while this load was running
      ;; (boot-resume replay) could not write the roster then — the live
      ;; hash was a partial view of the file.  It is authoritative now, so
      ;; settle the debt before anything else can restart Emacs.  Wrapped
      ;; because finish must stay robust: a roster write that fails is
      ;; logged, never allowed to strand the load mid-finalization.
      (agent-repl--with-error-logging "snapshot-load: flush-materialized"
        (agent-repl--snapshot-flush-materialized-pending))
      (agent-repl--log nil
                        "snapshot-load: END loaded=%d skipped=%d load-error=%d returned-to=%s"
                        loaded skipped load-error (or origin "nil"))
      (agent-repl--info nil "Loaded %d workspace(s), skipped %d, errored %d"
                        loaded skipped load-error)
      ;; The startup backend preparation occurred before this loader read the
      ;; snapshot.  Clear recursive state only after the final workspace hook
      ;; has unwound; no post-restore bounce may rebind these fresh sessions.
      (setq agent-repl--snapshot-load-state nil))))

(defun agent-repl--snapshot-load-close-main ()
  "Nuke the `main' workspace left over from Doom's startup, if it still exists.
Doom always creates `+workspaces-main' (typically \"main\") at startup;
the snapshot loader replaces it with the real workspace set, so this
artifact is never useful and we tear it down to keep the tabline
clean.  Called right after the FIRST entry has been established — not
at load BEGIN — because `main' is the only workspace alive until then;
killing it any earlier would leave the frame with zero workspaces.
Naturally idempotent (guarded by `agent-repl--ws-exists-p'), so later
entries in the same load may call it again for free.  Absent main, the
function is a no-op.

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
an optional MARKER supplied by whoever emitted the event.  The loader
doesn't distinguish the marker — a fully-loaded event for the awaited
ws advances the queue.  The watchdog never routes through here: it
emits no event and advances via `agent-repl--snapshot-load-timeout'
instead, so a timed-out ws is never mistaken for a loaded one.
Idempotent: the `:awaiting' equality guard makes second fires for the
same ws no-ops."
  (let ((state agent-repl--snapshot-load-state))
    (if (and state (equal ws (plist-get state :awaiting)))
        (progn
          (agent-repl--log ws "snapshot-load: awaited ws=%s fully loaded marker=%s — advancing" ws _marker)
          (agent-repl--snapshot-load-cancel-timer)
          (setq agent-repl--snapshot-load-state
                (plist-put agent-repl--snapshot-load-state :awaiting nil))
          (agent-repl--snapshot-load-step))
      (agent-repl--log ws "snapshot-load: ignoring ready event ws=%s marker=%s awaiting=%s state=%s"
                        ws _marker (and state (plist-get state :awaiting))
                        (if state "active" "nil")))))

(defun agent-repl--snapshot-load-timeout (ws)
  "Fault WS and advance the snapshot load when WS never becomes genuinely ready.
A missing barrier is a fault for WS alone — it is not a reason to strand
every entry behind it in the queue.  The watchdog therefore scopes the
failure to WS and keeps the loader moving:

- it never synthesizes readiness.  An older watchdog set `:agent-ready'
  and ran the fully-loaded hook, letting a broken backend render as if
  its session shim existed; neither readiness latch is touched here, and
  no fully-loaded event is emitted, so WS keeps whatever unverified state
  it actually has,
- the entry is reclassified from `:loaded' (optimistically counted when
  `--establish-workspace' returned) to `:load-error', so the END line and
  the iteration counter both describe what really happened,
- WS is untagged from `agent-repl--restored-workspaces' so a later
  `agent-repl-nuke-restored-workspaces' sweep of the restore batch leaves
  the faulted workspace standing for the user to inspect,
- the half-established persp/tab is deliberately left in place rather than
  torn down.  `--establish-workspace' already returned, so the tab, its
  buffers and its frontend boot are real; the barrier is the only thing
  missing, and it may still arrive.  A visible, faulted workspace is what
  the user needs to see, and tearing it down would also destroy the
  `:origin' window-configuration bookkeeping finish relies on,
- `--snapshot-load-step' then runs the remaining queue, and only after it
  returns is the fault raised through `agent-repl--error'.  The error is
  last precisely because it signals: raising it first would unwind the
  timer callback before the queue advanced, which is the abort this
  function exists to avoid.  `agent-repl--error' writes the logfile line
  before signalling, so the fault is durable either way."
  (let ((state agent-repl--snapshot-load-state))
    (when (and state (equal ws (plist-get state :awaiting)))
      (agent-repl--log ws "snapshot-load: TIMEOUT awaiting ws=%s — faulting ws and advancing without synthetic readiness" ws)
      (setq agent-repl--snapshot-load-state
            (plist-put agent-repl--snapshot-load-state :timeout-timer nil))
      (setq agent-repl--snapshot-load-state
            (plist-put agent-repl--snapshot-load-state :awaiting nil))
      (setq agent-repl--snapshot-load-state
            (plist-put agent-repl--snapshot-load-state :loaded
                       (max 0 (1- (plist-get agent-repl--snapshot-load-state :loaded)))))
      (setq agent-repl--snapshot-load-state
            (plist-put agent-repl--snapshot-load-state :load-error
                       (1+ (or (plist-get agent-repl--snapshot-load-state :load-error) 0))))
      (setq agent-repl--restored-workspaces
            (delete ws agent-repl--restored-workspaces))
      (agent-repl--snapshot-load-step)
      (agent-repl--error ws
                         "snapshot-load timeout awaiting ws=%s; workspace left faulted, restore continued with the remaining entries"
                         ws))))

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
      (agent-repl--log nil "snapshot-load: queue exhausted iter=%d total=%d; finishing" iter total)
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
         ;; They carry `:repl-state :merged' but get no tab-bar tab;
         ;; `--finish-workspace' (reachable via the workspace-commands
         ;; "finish" verb) is the only way out.
         ;;
         ;; Exception: when register-merged-workspace flags `:merge-failed t'
         ;; (either via on-disk state or the git-landing probe), promote the
         ;; entry from data-only to a real tab-bar workspace via
         ;; `--establish-workspace' and move it to the front of
         ;; `persp-names-cache' via `--reorder-workspace-to-front'.  A failed
         ;; cherry-pick must not hide as a data-only entry post-restart —
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
                  (agent-repl--reorder-workspace-to-front ws)
                  ;; A real workspace now exists — safe to nuke `main'.
                  (agent-repl--snapshot-load-close-main))
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
              ;; A real workspace now exists (this entry's tab was just
              ;; created) — safe to nuke Doom's leftover startup `main'.
              ;; No-op on later entries once main is already gone.
              (agent-repl--snapshot-load-close-main)
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
                                  ws)))
                (agent-repl--log ws "snapshot-load: awaiting ws=%s timeout=%.3fs restored-count=%d"
                                  ws agent-repl-snapshot-load-per-entry-timeout
                                  (length agent-repl--restored-workspaces))))))))))))))

(defun agent-repl-load-workspace-snapshot (&optional file startup)
  "Load workspaces from FILE (defaults to the configured snapshot path).
When FILE is nil, reads `agent-repl-workspace-snapshot-file' (or its
legacy module-dir fallback if the configured file is absent).  For each
entry, fully sets up the workspace via `agent-repl--establish-workspace'
\(persp creation + activation + projectile + dir-locals + magit lambda
+ find-file recent + claude init).

Recursive queue driver: establishes one entry, then yields to the main
loop until that workspace's `agent-repl-ws-fully-loaded-functions'
hook fires (i.e., both agent-side ready and emacs-side switch-settle
have completed), then advances.  On a missing barrier the per-entry
watchdog \(`agent-repl-snapshot-load-per-entry-timeout') faults that
single workspace and advances to the next entry, so one wedged
workspace can no longer strand the entries behind it; it never
synthesizes readiness for an unverified session.  Only loader-state
corruption (the `--snapshot-load-step' error path) aborts the load.

Returns to the workspace that was active when the load began.

STARTUP marks the automatic Emacs-start restore.  The startup caller has
already rebuilt and restarted the runtime services and received correlated
daemon initialization readiness before this function reads the snapshot;
interactive and archive loads rely on the canonical session-health render
gate for each live shim/store route."
  (interactive)
  (when agent-repl--snapshot-load-state
    (agent-repl--log nil "snapshot-load: rejected concurrent request file=%s startup=%s awaiting=%s"
                      file startup (plist-get agent-repl--snapshot-load-state :awaiting))
    (user-error "agent-repl: a snapshot load is already in progress"))
  (let* ((file (or file (agent-repl--workspace-snapshot-file-for-read)))
         (parsed (agent-repl--read-workspace-snapshot file))
         (snapshot (plist-get parsed :workspaces))
         (saved-hide (plist-get parsed :hide-project-dirs-enabled))
         (saved-frontend (plist-get parsed :default-frontend)))
    (unless snapshot
      (agent-repl--log nil "snapshot-load: snapshot missing-or-empty file=%s startup=%s" file startup)
      (user-error "No workspace snapshot at %s" file))
    ;; Restore the hide-project-dirs toggle BEFORE establishing entries —
    ;; the tombstone-vs-live partition below already encodes the hidden
    ;; set (hidden workspaces were saved as `:hidden-project-dir'
    ;; tombstones), so the runtime flag just needs to agree with it.
    (when (boundp 'agent-repl-hide-project-dirs-enabled)
      (setq agent-repl-hide-project-dirs-enabled (and saved-hide t))
      (agent-repl--log nil "snapshot-load: restored hide-project-dirs-enabled=%s"
                        agent-repl-hide-project-dirs-enabled))
    ;; Restore the frontend NEW workspaces are born with.  Only when the
    ;; snapshot actually carries one: a pre-`:default-frontend' snapshot
    ;; must leave the customized `agent-repl-default-frontend' alone
    ;; rather than stomp it with nil.  Restored workspaces themselves are
    ;; unaffected either way — each carries its own `:frontend' in its
    ;; per-project state.el.
    (when (and saved-frontend (boundp 'agent-repl-default-frontend))
      (setq agent-repl-default-frontend saved-frontend)
      (agent-repl--log nil "snapshot-load: restored default-frontend=%s" saved-frontend))
    (let* ((normalized (mapcar #'agent-repl--snapshot-entry-normalize snapshot))
           ;; An ORPHANED tombstone names a `:project-dir' that no longer
           ;; exists.  Nothing can ever resolve it again: it cannot be
           ;; reverse-looked-up, cannot be restored by the hide toggle, cannot
           ;; be a peer's parent.  It is pure weight — and not a little of it,
           ;; since deleting a batch of worktrees orphans one per worktree, and
           ;; every roster-wide sweep then pays for all of them forever.
           (orphan-tombstone-p
            (lambda (e)
              (let ((plist (cdr e)))
                (and (plist-get plist :nuked-at)
                     (let ((dir (plist-get plist :project-dir)))
                       (and (stringp dir) (not (file-directory-p dir))))))))
           (orphans (cl-remove-if-not orphan-tombstone-p normalized))
           ;; Partition: tombstoned entries (`:nuked-at' present) are
           ;; identity-only records — restore them directly to the hash
           ;; without queueing them for establish (which would create a
           ;; persp + start claude for a workspace the user already
           ;; nuked).  Live entries follow the original establish queue.
           (tombstones (cl-remove-if-not
                        (lambda (e) (and (plist-get (cdr e) :nuked-at)
                                         (not (funcall orphan-tombstone-p e))))
                        normalized))
           (queue (cl-remove-if
                   (lambda (e) (plist-get (cdr e) :nuked-at))
                   normalized))
           (origin-ws (agent-repl--ws-current-name)))
      ;; Written back BEFORE anything else in the load runs, so a directory
      ;; deleted outside Emacs costs exactly one startup and is never
      ;; encountered again.  The pruned list is the normalized roster minus the
      ;; orphans, not a re-collection from the live hash: the hash is still
      ;; half-populated here (live entries have not been established yet), so
      ;; collecting from it would drop every workspace still queued.
      (when orphans
        (agent-repl--write-workspace-snapshot
         (cl-remove-if orphan-tombstone-p normalized))
        (agent-repl--info
         nil
         "snapshot-load: pruned %d orphaned tombstone(s) with a missing directory; rewrote roster %d -> %d entries file=%s"
         (length orphans) (length normalized)
         (- (length normalized) (length orphans))
         agent-repl-workspace-snapshot-file)
        (agent-repl--log-verbose
         nil "snapshot-load: pruned orphaned tombstones=%S"
         (mapcar #'car orphans)))
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
      ;; Doom's startup `main' workspace is nuked once the FIRST entry
      ;; has been established (see `agent-repl--snapshot-load-close-main'),
      ;; not here at load BEGIN: `main' is the only workspace alive right
      ;; now, and killing it before another one exists would leave the
      ;; frame with zero workspaces.  When the loader was started FROM
      ;; main (the startup restore path), main is also the origin — clear
      ;; `:origin' so finish doesn't try to switch back to the workspace
      ;; that will be killed once loading gets underway, and instead stays
      ;; on the last-loaded one.
      (let ((main (agent-repl--ws-main-name)))
        (when (and main (equal origin-ws main))
          (agent-repl--log nil "snapshot-load: origin is startup-main=%s; clearing return target" main)
          (setq origin-ws nil)))
      (setq agent-repl--snapshot-load-state
            (list :queue queue
                  :origin origin-ws
                  :awaiting nil
                  :loaded 0
                  :skipped 0
                  :load-error 0
                  :total (length queue)
                  :timeout-timer nil
                  :startup startup))
      (add-hook 'agent-repl-ws-fully-loaded-functions
                #'agent-repl--snapshot-load-on-loaded)
      (agent-repl--log nil
                        "snapshot-load: BEGIN file=%s entries=%d origin-ws=%s"
                        file (length queue) (or origin-ws "nil"))
      (agent-repl--snapshot-load-step))))

(defun agent-repl--load-workspace-snapshot-on-startup ()
  "Prepare runtime services, confirm daemon readiness, then restore snapshot.
The readiness prerequisite is deliberately before even the snapshot-path
lookup, so no workspace state is read or restored before daemon health is
authoritative.  A failed prerequisite or malformed snapshot aborts loudly;
there is no post-restore bounce and no degraded restore path."
  (agent-repl--log nil "startup restore: backend preparation begins before snapshot lookup")
  (agent-repl--runtime-startup-prepare
   (lambda ()
     (let ((file (agent-repl--workspace-snapshot-file-for-read)))
       (agent-repl--log nil
                        "startup restore: runtime prepared and daemon ready; snapshot candidate=%s"
                        file)
       (if (file-exists-p file)
           (condition-case err
               (agent-repl-load-workspace-snapshot file t)
             (error
              (agent-repl--error nil "startup restore: snapshot load aborted file=%s err=%S"
                                 file err)))
         (agent-repl--log nil
                          "startup restore: no snapshot file=%s; no restore requested"
                          file))))
   (lambda (detail)
     (agent-repl--log nil "startup restore: backend preparation FAILED detail=%s" detail))))

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
just enough state to record WS as merged-completed and for
`--finish-workspace' to later remove the worktree.  Does NOT create a
Doom persp and does NOT start Claude — the workspace is data-only
until the user finishes it (the workspace-commands \"finish\" verb).

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
`:repl-state :merge-failed' so the ❌ badge surfaces for the first
time.  Clean merges set `:repl-state :merged' so the 🔀
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
              ((agent-repl--ws-known-p ws))
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

;;;; Workspace picker (SPC p p)
;;
;; `agent-repl-switch-to-project' (interactive `SPC p p') is a picker over
;; KNOWN AGENT-REPL WORKSPACES — not over every projectile project — rendered
;; as a column view under a header row:
;;
;;   <emoji> <workspace-name padded>   <created>   <viewed>   <removed>
;;
;; - Candidates are the union of the in-memory workspace hash (live and
;;   tombstoned) and the on-disk roster snapshot, so workspaces killed /
;;   merged in a prior session still appear (`agent-repl--known-workspace-entries').
;; - The emoji is the per-workspace state glyph (`agent-repl-ws-state-icons')
;;   for a known workspace, else a neutral 📁 for a disk-only record.
;; - The three date columns are read from PERSISTED state — each candidate's
;;   `<root>/.claude/emacs/state.el' — with fresher live in-memory values
;;   overlaid, so dead workspaces still show real dates.  The candidate set
;;   is bounded to known workspaces, so this disk read is not the
;;   all-projectile-projects fan-out the old picker avoided.
;; - Entries are sorted most-recently-VIEWED first (`:last-viewed-at'); a
;;   workspace's create/kill time is irrelevant to ordering.  Never-viewed
;;   workspaces sink to the bottom.
;; - Selecting a live workspace switches to it; selecting a removed
;;   (killed / nuked / merged) workspace revives it from persisted state,
;;   recreating its worktree/directory first when missing — the only
;;   filesystem-existence check happens then, at selection time, never on
;;   every picker invocation.
;; - helm renders the header natively via its per-source header; ivy /
;;   `completing-read' fall back to putting the header in the prompt.

(defface agent-repl-picker-created-face
  '((t :inherit font-lock-comment-face))
  "Face for the creation-date column in `agent-repl-switch-to-project'."
  :group 'agent-repl)

(defface agent-repl-picker-viewed-face
  '((t :inherit success))
  "Face for the last-viewed-date column in `agent-repl-switch-to-project'."
  :group 'agent-repl)

(defface agent-repl-picker-killed-face
  '((t :inherit error))
  "Face for the removed-date (last kill/merge) column in the picker."
  :group 'agent-repl)

(defface agent-repl-picker-name-face
  '((t :inherit default))
  "Face for the workspace-name column in `agent-repl-switch-to-project'."
  :group 'agent-repl)

(defface agent-repl-picker-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the picker's column-title header row."
  :group 'agent-repl)

(defconst agent-repl--picker-date-format "%Y-%m-%d"
  "`format-time-string' template for the picker's date columns.")

(defconst agent-repl--picker-date-width 10
  "Width of each date column in the picker (matches
`agent-repl--picker-date-format').  Used to keep the placeholder
\"--\" aligned with real dates.")

(defconst agent-repl--picker-name-min-width 24
  "Minimum padding width for the workspace-name column in the picker.
Actual width is the max of this and the longest candidate name.")

(defconst agent-repl--picker-column-gap "   "
  "Whitespace inserted between the picker's name and date columns.")

(defun agent-repl--known-workspace-entries ()
  "Return an alist of (WS-NAME . PROJECT-DIR) for every known agent-repl workspace.

Unions two sources so a workspace killed / merged in a prior session
still appears:
  1. the in-memory `agent-repl--workspaces' hash — every entry, live or
     tombstoned, that carries a `:project-dir';
  2. the on-disk roster snapshot (`agent-repl-workspace-snapshot-file')
     for names not present in memory (a prior session's record that was
     never re-established this run).

In-memory entries win on name collision.  Order: in-memory entries first
\(hash-traversal order), then snapshot-only entries.  This is the
candidate universe of the project picker — deliberately NOT
`projectile-relevant-known-projects', which listed every project
regardless of whether an agent-repl workspace ever existed there."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (ws (agent-repl--ws-registered-names))
      (let* ((plist (agent-repl--ws-plist ws))
             (dir (plist-get plist :project-dir)))
        (when (and dir (not (gethash ws seen)))
          (puthash ws t seen)
          (push (cons ws dir) result))))
    (let* ((file (agent-repl--workspace-snapshot-file-for-read))
           (raw (and file (ignore-errors
                            (agent-repl--read-sexp-file-if-exists file))))
           (entries (agent-repl--snapshot-entries-from-raw raw)))
      (dolist (entry entries)
        (let* ((norm (ignore-errors (agent-repl--snapshot-entry-normalize entry)))
               (name (car norm))
               (dir (plist-get (cdr norm) :project-dir)))
          (when (and name dir (not (gethash name seen)))
            (puthash name t seen)
            (push (cons name dir) result)))))
    (nreverse result)))

(defun agent-repl--picker-workspace-summary (ws-name project-dir)
  "Return a plist summarizing workspace WS-NAME at PROJECT-DIR for the picker.

Reads the persisted state file at PROJECT-DIR for `:created-at',
`:last-killed-at', and `:last-viewed-at', then overlays fresher live
in-memory values from `agent-repl--workspaces' when WS-NAME is a known
entry.  Reading the state file is bounded to the known-workspace
candidate set (see `agent-repl--known-workspace-entries'), so this is
not the all-projectile-projects disk fan-out the old picker avoided.

Keys:
  `:created-at' `:last-killed-at' `:last-viewed-at' — persisted (live overlay)
  `:live-p'          non-nil iff WS-NAME is a live (non-tombstoned) entry
  `:workspace-name'  WS-NAME when it is a known hash entry, else nil
  `:project-dir'     PROJECT-DIR"
  (let* ((known (and ws-name (agent-repl--ws-known-p ws-name)))
         (state-file (and project-dir
                          (agent-repl--state-file-for-read project-dir)))
         (saved (and state-file
                     (ignore-errors
                       (agent-repl--read-sexp-file-if-exists state-file)))))
    (cl-flet ((val (key) (or (and known (agent-repl--ws-get ws-name key))
                             (plist-get saved key))))
      (list :created-at (val :created-at)
            :last-killed-at (val :last-killed-at)
            :last-viewed-at (val :last-viewed-at)
            :live-p (and ws-name (agent-repl--ws-live-p ws-name))
            :workspace-name (and known ws-name)
            :project-dir project-dir))))

(defun agent-repl--picker-status-emoji (summary)
  "Return the status-emoji prefix for a candidate with SUMMARY.
SUMMARY is a plist from `agent-repl--picker-workspace-summary'.

When `:workspace-name' is non-nil, delegates to
`agent-repl--ws-render-status' for the render-state keyword and
looks up the glyph in `agent-repl-ws-state-icons' — the shared
render-state palette — so the workspace picker (`SPC p p') and every
other renderer agree on the emoji for a given workspace.

For a disk-only record with no in-memory hash entry returns a neutral
📁 — a tombstoned or merged workspace still has a hash entry and so
still renders its (killed / merged / hibernated) state glyph."
  (let ((ws (plist-get summary :workspace-name)))
    (if (and ws (agent-repl--ws-known-p ws))
        (or (alist-get (agent-repl--ws-render-status ws)
                       agent-repl-ws-state-icons)
            agent-repl-ws-state-icon-default)
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

(defun agent-repl--picker-name-width (names)
  "Return the padding width to use for the workspace-name column.
Max of `agent-repl--picker-name-min-width' and the longest name across
NAMES so every row's date columns start at the same character position."
  (let ((max-name (apply #'max 0 (mapcar #'length names))))
    (max agent-repl--picker-name-min-width max-name)))

(defun agent-repl--picker-sort-key (summary)
  "Return the `:last-viewed-at' time for SUMMARY.
Used as the sole sort key so the picker orders workspaces
most-recently-viewed first — create / kill / merge times are
irrelevant to ordering.  Returns nil when the workspace has never been
viewed; callers treat nil keys as oldest (sorted to the bottom)."
  (plist-get summary :last-viewed-at))

(defun agent-repl--picker-time-greater-p (a b)
  "Compare two `current-time'-shaped values: non-nil A newer than nil B."
  (cond ((and a b) (time-less-p b a))
        (a t)
        (t nil)))

(defun agent-repl--build-workspace-picker-candidates (entries)
  "Return a sorted alist of (DISPLAY-STRING . PAYLOAD) for ENTRIES.

ENTRIES is an alist of (WS-NAME . PROJECT-DIR) — typically from
`agent-repl--known-workspace-entries'.  Each display string prefixes a
status emoji, then the workspace name padded to a shared width, then
three date columns (created, last-viewed, removed) separated by
`agent-repl--picker-column-gap'; dash placeholders keep columns aligned.
PAYLOAD is a plist (:name WS-NAME :project-dir DIR :live-p BOOL) that
`agent-repl--picker-open-selection' consumes.

Sort order: most-recently-VIEWED first; never-viewed workspaces sink to
the bottom (see `agent-repl--picker-sort-key')."
  (let* ((name-width (agent-repl--picker-name-width (mapcar #'car entries)))
         (rows (mapcar
                (lambda (entry)
                  (let* ((name (car entry))
                         (dir (cdr entry))
                         (summary (agent-repl--picker-workspace-summary name dir)))
                    (list :name name :dir dir :summary summary)))
                entries))
         (sorted (sort rows
                       (lambda (a b)
                         (agent-repl--picker-time-greater-p
                          (agent-repl--picker-sort-key (plist-get a :summary))
                          (agent-repl--picker-sort-key (plist-get b :summary)))))))
    (mapcar
     (lambda (row)
       (let* ((name (plist-get row :name))
              (dir (plist-get row :dir))
              (summary (plist-get row :summary))
              (emoji (agent-repl--picker-status-emoji summary))
              (name-padded
               (propertize
                (truncate-string-to-width name name-width 0 ?\s)
                'face 'agent-repl-picker-name-face))
              (created (agent-repl--picker-format-date
                        (plist-get summary :created-at)
                        agent-repl--picker-date-width
                        'agent-repl-picker-created-face
                        "----------"))
              (viewed (agent-repl--picker-format-date
                       (plist-get summary :last-viewed-at)
                       agent-repl--picker-date-width
                       'agent-repl-picker-viewed-face
                       "----------"))
              (killed (agent-repl--picker-format-date
                       (plist-get summary :last-killed-at)
                       agent-repl--picker-date-width
                       'agent-repl-picker-killed-face
                       "----------"))
              (display (concat emoji " "
                               name-padded
                               agent-repl--picker-column-gap created
                               agent-repl--picker-column-gap viewed
                               agent-repl--picker-column-gap killed)))
         (cons display (list :name name
                             :project-dir dir
                             :live-p (plist-get summary :live-p)))))
     sorted)))

(defun agent-repl--picker-header-line (name-width)
  "Return the propertized column-title header row for the picker.
Approximately aligns the titles over the emoji / name / date columns;
NAME-WIDTH matches the width
`agent-repl--build-workspace-picker-candidates' pads the name column to.
Rendered natively as helm's per-source header, and folded into the
prompt string on the ivy / `completing-read' fallbacks."
  (propertize
   (concat "   "                       ; emoji (≈2 display cols) + gap
           (truncate-string-to-width "Workspace" name-width 0 ?\s)
           agent-repl--picker-column-gap
           (truncate-string-to-width "Created" agent-repl--picker-date-width 0 ?\s)
           agent-repl--picker-column-gap
           (truncate-string-to-width "Viewed" agent-repl--picker-date-width 0 ?\s)
           agent-repl--picker-column-gap
           (truncate-string-to-width "Removed" agent-repl--picker-date-width 0 ?\s))
   'face 'agent-repl-picker-header-face))

(declare-function helm "ext:helm")

(defun agent-repl--read-workspace-via-picker ()
  "Prompt for a known workspace with the rich column picker.
Returns the selected PAYLOAD plist (:name :project-dir :live-p), or nil
when there are no candidates or the user aborts.

Backend preference: `helm' when available (its per-source header renders
the column titles natively), else `ivy-read', else `completing-read';
the latter two fold the header into the prompt.  The choice is captured
via the action closure (helm/ivy pass a consistent shape there) rather
than the backend's return value."
  (let* ((entries (agent-repl--known-workspace-entries))
         (candidates (agent-repl--build-workspace-picker-candidates entries))
         (header (agent-repl--picker-header-line
                  (agent-repl--picker-name-width (mapcar #'car entries))))
         (selected nil))
    (agent-repl--log (agent-repl--ws-current-log-name)
                      "workspace-picker: entries=%d candidates=%d helm=%s ivy=%s"
                      (length entries) (length candidates)
                      (if (fboundp 'helm) "t" "nil")
                      (if (fboundp 'ivy-read) "t" "nil"))
    (cond
     ((null candidates)
      (agent-repl--log (agent-repl--ws-current-log-name) "workspace-picker: no candidates")
      (message "No known agent-repl workspaces")
      nil)
     ((fboundp 'helm)
      (agent-repl--log (agent-repl--ws-current-log-name) "workspace-picker: backend=helm")
      ;; Raw-alist helm source: no helm macros, so byte-compilation does
      ;; not require helm at build time (the `fboundp' guard defers the
      ;; call to runtime, where helm is loaded).  A cons candidate
      ;; (DISPLAY . PAYLOAD) hands PAYLOAD to the action.
      (helm :sources
            (list `((name . "Switch to workspace")
                    (candidates . ,candidates)
                    (header-name . ,(lambda (_n) header))
                    (action . ,(lambda (payload) (setq selected payload)))))
            :buffer "*helm agent-repl workspaces*")
      selected)
     ((fboundp 'ivy-read)
      (agent-repl--log (agent-repl--ws-current-log-name) "workspace-picker: backend=ivy")
      (ivy-read (concat header "\nSwitch to workspace: ") candidates
                :action (lambda (c)
                          (setq selected (cond ((consp c) (cdr c))
                                               ((stringp c)
                                                (cdr (assoc c candidates)))
                                               (t c))))
                :require-match t
                :caller 'agent-repl-switch-to-project)
      selected)
     (t
      (agent-repl--log (agent-repl--ws-current-log-name) "workspace-picker: backend=completing-read")
      (let* ((choice (completing-read (concat header "  |  Switch to workspace: ")
                                      (mapcar #'car candidates)
                                      nil t))
             (hit (assoc choice candidates)))
        (and hit (cdr hit)))))))

(defun agent-repl--picker-worktree-branch (source-repo dir)
  "Return the branch git has registered for worktree DIR in SOURCE-REPO, or nil.
Parses `git worktree list --porcelain' — which lists registered
worktrees even when their directory is missing — for the entry whose
worktree path canonicalizes to DIR, returning its short branch name.
Used only when recreating a missing worktree at selection time."
  (when (and source-repo dir)
    (let ((out (ignore-errors
                 (agent-repl--git-string "-C" source-repo
                                         "worktree" "list" "--porcelain")))
          (want (directory-file-name (expand-file-name dir)))
          (cur nil) (result nil))
      (when (and out (not (string-empty-p out)))
        (dolist (line (split-string out "\n"))
          (cond
           ((string-prefix-p "worktree " line)
            (setq cur (directory-file-name (expand-file-name (substring line 9)))))
           ((and (string-prefix-p "branch " line) (equal cur want))
            (setq result (replace-regexp-in-string
                          "\\`refs/heads/" "" (substring line 7)))))))
      result)))

(defun agent-repl--picker-branches-for (source-repo name)
  "Return existing branches in SOURCE-REPO whose basename equals NAME.
A finished workspace's `git worktree remove' drops the path
registration but keeps the branch, usually under a launcher prefix
\(e.g. a legacy \"DWC/NAME\"), so the registered-branch lookup in
`agent-repl--picker-worktree-branch' comes back empty even though the
branch is sitting right there.  These are the reattach candidates for
`agent-repl--picker-recreate-directory'."
  (when (and source-repo name)
    (let ((out (ignore-errors
                 (agent-repl--git-string-quiet "-C" source-repo
                                               "for-each-ref"
                                               "--format=%(refname:short)"
                                               "refs/heads"))))
      (cl-remove-if-not
       (lambda (b) (equal (file-name-nondirectory b) name))
       (and out (split-string out "\n" t))))))

(defun agent-repl--snapshot-roster-plist-for (name)
  "Return the persisted roster plist for workspace NAME, or nil.
Reads the on-disk snapshot roster (via
`agent-repl--workspace-snapshot-file-for-read'), normalizes each entry,
and returns the plist of the entry whose name equals NAME.

The durable fallback for `:worktree-p'/`:source-ws-dir' in
`agent-repl--picker-recreate-directory': when a workspace's worktree
was deleted its in-worktree `state.el' died with it, but the roster
still carries these fields (persisted by
`agent-repl--worktree-snapshot-fields'), so revival can rebuild the
worktree instead of degrading to a plain, repo-less directory."
  (let* ((file (agent-repl--workspace-snapshot-file-for-read))
         (raw (and file (ignore-errors
                          (agent-repl--read-sexp-file-if-exists file))))
         (entries (agent-repl--snapshot-entries-from-raw raw)))
    (cl-loop for entry in entries
             for norm = (ignore-errors
                          (agent-repl--snapshot-entry-normalize entry))
             when (and norm (equal (car norm) name))
             return (cdr norm))))

(defun agent-repl--picker-recreate-directory (name dir)
  "Recreate the missing DIR for workspace NAME before revival.
For a git-worktree workspace, resolves the source repo (`:source-ws-dir'
on the live plist, in the in-worktree state file, or — once the worktree
and its state file are gone — in the durable roster via
`agent-repl--snapshot-roster-plist-for'), prunes stale worktree admin
entries (safe — prune only drops records for already-missing
worktrees), then reattaches DIR by trying, in order:

  1. the branch git still has REGISTERED for DIR (crash /
     manually-deleted-dir case — `agent-repl--picker-worktree-branch');
  2. an existing branch whose basename is NAME (finished workspace:
     `git worktree remove' deregistered the path but kept the branch,
     possibly prefixed, e.g. \"DWC/NAME\") — an exact NAME match wins,
     a single prefixed match is used, several prefixed matches error
     with the candidates named (picking one blind would be a guess);
  3. a FRESH NAME branch cut from `agent-repl-master-branch-name' —
     no branch survived at all; a merged workspace's commits are
     already in the main branch and the conversation resumes from the
     durable claude session either way.  Surfaced via
     `agent-repl--warn' so the fresh start is never silent.

For a plain (non-worktree) project dir, recreates the directory with
`make-directory'.  When git itself refuses, signals with git's OWN
output (re-captured via `agent-repl--git-string'), never a bare exit
code, so revival failures are diagnosable from the error alone."
  (let* ((known (agent-repl--ws-known-p name))
         ;; Prefer the in-worktree state.el; fall back to the durable
         ;; roster when the worktree (and thus that state.el) is gone.
         ;; The roster still carries :worktree-p/:source-ws-dir, so a
         ;; deleted worktree is rebuilt rather than recreated as a plain
         ;; repo-less directory (which magit would then offer to init).
         (saved (or (let ((f (agent-repl--state-file-for-read dir)))
                      (and f (ignore-errors
                               (agent-repl--read-sexp-file-if-exists f))))
                    (agent-repl--snapshot-roster-plist-for name)))
         (worktree-p (or (and known (agent-repl--ws-get name :worktree-p))
                         (plist-get saved :worktree-p)))
         (source (or (and known (agent-repl--ws-get name :source-ws-dir))
                     (plist-get saved :source-ws-dir))))
    (agent-repl--log name "picker-recreate-directory: ws=%s dir=%s worktree-p=%s source=%s"
                      name dir (if worktree-p "t" "nil") (or source "nil"))
    (cond
     ((not worktree-p)
      (make-directory dir t))
     ((and source (file-directory-p source))
      (agent-repl--git-exit-code source "worktree" "prune")
      (let* ((registered (agent-repl--picker-worktree-branch source dir))
             (matches (unless registered
                        (agent-repl--picker-branches-for source name)))
             (branch (or registered
                         (car (member name matches))
                         (and (= (length matches) 1) (car matches)))))
        (agent-repl--log name "picker-recreate-directory: ws=%s registered-branch=%s basename-matches=%S -> %s"
                          name (or registered "nil") matches
                          (if branch (format "reattach to '%s'" branch)
                            (format "fresh '%s' from '%s'" name agent-repl-master-branch-name)))
        (when (and (not branch) (> (length matches) 1))
          (error "agent-repl: cannot revive '%s': several branches match it (%s) and none exactly; delete or rename the stale ones and retry"
                 name (string-join matches ", ")))
        (let* ((add-args (if branch
                             (list "worktree" "add" dir branch)
                           (list "worktree" "add" "-b" name dir
                                 agent-repl-master-branch-name)))
               (ec (apply #'agent-repl--git-exit-code source add-args)))
          (unless (zerop ec)
            ;; Re-run the identical (failed, side-effect-free) add via
            ;; the output-capturing runner purely to recover git's error
            ;; text for the signal.
            (let ((out (string-trim
                        (or (ignore-errors
                              (apply #'agent-repl--git-string "-C" source add-args))
                            ""))))
              (error "agent-repl: could not recreate worktree for '%s' at %s (exit %d) — git said: %s"
                     name dir ec (if (string-empty-p out) "(no output)" out))))
          (unless branch
            (agent-repl--warn name
                              "revived workspace '%s' on a FRESH '%s' branch cut from %s — no prior branch survived (a merged workspace's commits are already in %s)"
                              name name agent-repl-master-branch-name
                              agent-repl-master-branch-name)))))
     (t
      (error "agent-repl: cannot recreate missing worktree for '%s' at %s: no source repo recorded, so there is no repo to recreate it from"
             name dir)))))

(defun agent-repl--picker-ensure-directory (name dir)
  "Ensure DIR exists on disk before reviving workspace NAME.
The ONLY filesystem-existence check in the picker, run at selection time
so `SPC p p' never polls the disk for every candidate.  No-op when DIR
already exists (the common case — kill / nuke / merge leave the worktree
in place); delegates to `agent-repl--picker-recreate-directory' when DIR
is missing."
  (if (and dir (not (file-directory-p dir)))
      (progn
        (agent-repl--log name "picker-ensure-directory: ws=%s dir=%s missing -> recreate" name dir)
        (agent-repl--picker-recreate-directory name dir))
    (agent-repl--log name "picker-ensure-directory: ws=%s dir=%s exists-or-nil" name dir)))

(defun agent-repl--picker-revive (name dir)
  "Revive removed workspace NAME at DIR from persisted state.
Re-asserts `:project-dir' (so a disk-only record with no live hash entry
gains one) and calls `agent-repl--establish-workspace', the canonical
resurrection path — it recreates the perspective, hydrates persisted
display state, clears any tombstone, and resumes the durable session."
  (agent-repl--ws-put name :project-dir dir)
  (agent-repl--establish-workspace name dir))

(defun agent-repl--picker-open-selection (payload)
  "Open the workspace described by PAYLOAD (:name :project-dir :live-p).
A workspace that still has a live perspective switches in place via
`agent-repl--ws-switch'.  One WITHOUT a live perspective is revived
from persisted state via `agent-repl--picker-revive', after
`agent-repl--picker-ensure-directory' recreates its worktree/directory
when missing.  Then hydrates the priority badge on a deferred timer,
mirroring the PROJECT-arg path of
`agent-repl-switch-to-project'.  Re-activating the workspace stamps its
`:last-viewed-at' via the persp-activated hook, so the picker reorders
it to the front next time.

The switch-vs-revive decision keys on `agent-repl--ws-open-p' (does a
live PERSPECTIVE exist?), NOT on PAYLOAD's `:live-p' (which reports
`agent-repl--ws-live-p' — merely a non-tombstoned REGISTRY entry).  A
merge or a close with `preserve-entry' leaves a workspace registered
but perspective-less; routing that on `:live-p' sent it to
`agent-repl--ws-switch' -> `+workspace-switch', which errors
\"... is not an available workspace\" because no perspective exists.
Keying on perspective existence revives it instead."
  (let ((name (plist-get payload :name))
        (dir (plist-get payload :project-dir)))
    (if name
        (progn
          (agent-repl--log name "picker-open-selection: ws=%s dir=%s open=%s payload=%S"
                            name dir (if (agent-repl--ws-open-p name) "t" "nil") payload)
          (if (agent-repl--ws-open-p name)
              (agent-repl--ws-switch name)
            (agent-repl--picker-ensure-directory name dir)
            (agent-repl--picker-revive name dir))
          (run-at-time 0 nil
                       (lambda ()
                         (let ((current (ignore-errors (agent-repl--ws-current-name))))
                           (agent-repl--log current "picker-open-selection: deferred hydrate current=%s selected=%s dir=%s"
                                             current name dir)
                           (agent-repl--hydrate-and-reorder-on-open current dir)))))
      (agent-repl--log (agent-repl--ws-current-log-name)
                        "picker-open-selection: invalid payload without :name payload=%S" payload))))

(defun agent-repl-switch-to-project (&optional project)
  "Switch to a known workspace (interactive `SPC p p'), or to PROJECT.

With no argument, prompts via `agent-repl--read-workspace-via-picker'
\(the column picker over KNOWN agent-repl workspaces, sorted
most-recently-viewed first) and opens the chosen workspace via
`agent-repl--picker-open-selection' — switching to it when live, or
reviving it from persisted state (recreating its worktree/directory
first when missing) when it was removed.

With PROJECT (a project root path — the programmatic contract
worktree.el's callers rely on), switches via
`projectile-switch-project-by-name' (Doom's
`+workspaces-switch-to-project-h' creates/activates the persp keyed on
the project basename), opens the most-recently-accessed file under
PROJECT, hydrates the saved display state and reseats by priority via
the shared `agent-repl--hydrate-and-reorder-on-open' step.

Distinct from `agent-repl--switch-to-workspace': that primitive is
name-keyed and assumes the persp already exists.  Both differ from
`agent-repl--establish-workspace', which is the snapshot-restore /
revival path that bypasses the Doom hook to preserve the exact ws name."
  (interactive)
  (if project
      (progn
        (agent-repl--log (agent-repl--ws-current-log-name) "switch-to-project: project path=%s" project)
        (agent-repl--ws-switch-project project)
        ;; Defer the file open and display-state disk read so the persp switch
        ;; completes and Emacs redraws before any blocking I/O fires.  Both are
        ;; deferred together in one timer so they run in order on the same idle
        ;; cycle rather than racing across two separate timers.
        (run-at-time 0 nil
                     (lambda ()
                       (let ((recent-file (agent-repl--most-recent-project-file project))
                             (current (ignore-errors (agent-repl--ws-current-name))))
                         (if (and recent-file (file-exists-p recent-file))
                             (progn
                               (agent-repl--log current "switch-to-project: deferred opening recent-file=%s project=%s"
                                                 recent-file project)
                               (find-file recent-file))
                           (agent-repl--log current "switch-to-project: deferred no existing recent-file candidate=%s project=%s"
                                             recent-file project))
                         (agent-repl--hydrate-and-reorder-on-open current project)))))
    (let ((sel (agent-repl--read-workspace-via-picker)))
      (if sel
          (progn
            (agent-repl--log (agent-repl--ws-current-log-name) "switch-to-project: picker selected=%S" sel)
            (agent-repl--picker-open-selection sel))
        (agent-repl--log (agent-repl--ws-current-log-name) "switch-to-project: picker cancelled-or-empty")))))

;;;; Workspace cycling

(defun agent-repl--workspace-cycle (n)
  "Cycle N workspaces (negative = left, positive = right).
Reimplements `+workspace/cycle' but iterates the tab-bar list
\(`agent-repl--ws-tabline-names', which drops the workspaces of folded
repos) instead of the raw `+workspace-list-names', so a folded repo's
workspaces are skipped during s-{ / s-}.  Mirrors Doom's
protected-workspace handling: when current is the nil-persp, switch to
`+workspaces-main' instead of cycling."
  (let ((current-name (agent-repl--ws-current-name)))
    (if (agent-repl--ws-protected-p current-name)
        (agent-repl--ws-switch (agent-repl--ws-main-name) t)
      (condition-case-unless-debug ex
          (let* ((visible (agent-repl--ws-tabline-names))
                 (perspc (length visible))
                 (index (cl-position current-name visible :test #'equal)))
            (when (zerop perspc)
              (user-error "No visible workspaces to switch to"))
            ;; CURRENT can legitimately be ABSENT from the tabline: a merged
            ;; workspace is torn down and dropped from the list while its
            ;; perspective is still the one the user sits in, and a folded
            ;; repo hides its members. Cycling from such a workspace used to
            ;; do arithmetic on the nil position — the number-or-marker-p nil
            ;; error on every switch keypress after a merge. An absent
            ;; current cycles from the list's edge instead: right lands on
            ;; the first visible workspace, left on the last.
            (unless index
              (agent-repl--log current-name
                               "workspace-cycle: current=%s not in tabline (%d visible) — cycling from the edge"
                               current-name perspc))
            (when (and index (= perspc 1))
              (user-error "No other workspaces"))
            (agent-repl--ws-switch
             (if index
                 (nth (mod (+ index n) perspc) visible)
               (if (< n 0) (car (last visible)) (car visible)))))
        ('user-error (agent-repl--ws-error (cadr ex) t))
        ('error (agent-repl--ws-error ex t))))))

(defun agent-repl-switch-left ()
  "Cycle one workspace left, skipping folded-repo workspaces.
Drop-in replacement for `+workspace/switch-left'."
  (interactive)
  (agent-repl--workspace-cycle -1))

(defun agent-repl-switch-right ()
  "Cycle one workspace right, skipping folded-repo workspaces.
Drop-in replacement for `+workspace/switch-right'."
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
does not consult `current-prefix-arg'.

Indexes the TAB-BAR list (`--ws-tabline-names'), not the raw workspace
list, so a folded repo takes its workspaces out of the
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
yanked away from it."
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
