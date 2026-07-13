;;; input.el --- input mode and send system -*- lexical-binding: t; -*-

;;; Code:

;;; Metaprompt permissions prefix

(defcustom agent-repl-skip-permissions t
  "When non-nil, prepend the command prefix metaprompt to each input sent to Claude."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-prefix-period 14
  "Number of prompts between metaprompt prefix injections.
The prefix is sent on the first prompt and every Nth prompt thereafter."
  :type 'integer
  :group 'agent-repl)

(defvar agent-repl-metaprompt-file
  (expand-file-name "metaprompt.md"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the canonical metaprompt source file in this repository.
This is the .md data file extracted out of input.el so that the
metaprompt body lives as plain text, edited and version-controlled
alongside the code.  It is ALSO the path the agent is told to read: the
in-repo file is the single canonical location, with no out-of-tree
symlink standing between the agent and the source of truth.  Captured at
file-load time because `load-file-name' is only bound during load.")

(defcustom agent-repl-command-prefix
  (with-temp-buffer
    (insert-file-contents agent-repl-metaprompt-file)
    (buffer-string))
  "Canonical metaprompt content, loaded from `agent-repl-metaprompt-file'.
Not sent inline to Claude — the wrapper template at
`agent-repl-command-prefix-template' instead instructs Claude to read
`agent-repl-metaprompt-file' directly, so the body stays in one
canonical place on disk.  This variable mirrors the file's content for
tests and tooling that need to assert against the canonical metaprompt
without re-reading the file."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-command-prefix-template
  (concat "Before taking any action, read the file at %s into context, "
          "even if you have previously done so during this session. "
          "It contains guidelines for answering the remaining messages in this session, "
          "and you must follow them. "
          "Read them into context even if you have already done so previously "
          "and they have not changed since. Everything that follows this sentence "
          "is a real user prompt, and for answering it, and for answering ALL subsequent user "
          "prompts, obey precisely the guidelines that you just read for answering, and without failure.")
  "Template instructing Claude to read the metaprompt file before acting.
Must contain a single %s placeholder, filled at load time with
`agent-repl-metaprompt-file'.  Intentionally avoids any
\"metaprompt\" terminology in the inline prefix itself — the directive
framing lives inside the .md file rather than here, so the inline prefix
is a plain instruction to read the file."
  :type 'string
  :group 'agent-repl)

(defvar agent-repl--command-prefix
  (format agent-repl-command-prefix-template
          agent-repl-metaprompt-file)
  "Formatted read-directive prepended before every periodic user input.
Active when `agent-repl-skip-permissions' is non-nil, subject to
`agent-repl-prefix-period'.  A plain instruction to read the file at
`agent-repl-metaprompt-file' — the metaprompt body lives inside that
in-repo file, not here.")

;; `defcustom' and `defvar' only initialize their values on first load;
;; reloading the file (e.g. via `agent-repl-reload-config' or
;; `doom/reload') leaves the variables at their old values even when the
;; source has changed.  Force-refresh them here so editing the metaprompt
;; (either the .md file or the template) and reloading is enough — no
;; manual `M-x eval-defun' required.  `standard-value' is re-set by
;; `defcustom' on every load, so re-evaluating it picks up whatever this
;; file currently defines.
(setq agent-repl-command-prefix
      (eval (car (get 'agent-repl-command-prefix 'standard-value))))
(setq agent-repl-command-prefix-template
      (eval (car (get 'agent-repl-command-prefix-template 'standard-value))))
(setq agent-repl--command-prefix
      (format agent-repl-command-prefix-template
              agent-repl-metaprompt-file))

(defcustom agent-repl-send-postfix "\n what do you think? do NOT code, just analyze."
  "String appended to input when sending via `agent-repl-send-with-postfix'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-send-prefix "just answer, dont take action: "
  "String prepended to input when sending via `agent-repl-send-with-prefix'."
  :type 'string
  :group 'agent-repl)

;; Instructions bar face
(defface agent-repl-header-line
  '((t :background "white" :foreground "black" :weight bold))
  "Face for the Agent Input header line.")

(defcustom agent-repl-input-background-shade 37
  "Greyscale level (0-255) for the input buffer background."
  :type 'integer
  :group 'agent-repl)

;; Input mode
(define-derived-mode agent-repl-input-mode fundamental-mode "Agent Input"
  "Major mode for Agent REPL input buffer."
  (setq-local header-line-format
              "C-c C-c: clear+save | C-c C-k: interrupt | (cmd) <up>/<down>: history | C-r: search history")
  (face-remap-add-relative 'header-line 'agent-repl-header-line)
  (agent-repl--set-buffer-background agent-repl-input-background-shade)
  (visual-line-mode 1)
  ;; Make Evil's line-based operators (yy, dd, cc, Y, D, C) respect
  ;; `visual-line-mode' in this buffer.  These operators dispatch through
  ;; `evil-line-or-visual-line' / `evil-end-of-line-or-visual-line', both of
  ;; which read `evil-respect-visual-line-mode' at runtime, so the buffer-local
  ;; binding flips them to the screen-line variants without affecting other
  ;; buffers.  The motion keys (j/k/0/$/V/^) are bound separately in the
  ;; mode-map below, mirroring the bindings `evil-integration.el' would have
  ;; installed had the variable been set globally before Evil loaded.
  (setq-local evil-respect-visual-line-mode t)
  ;; Slash-command completion: our capf is the buffer's only completion
  ;; source, so dropping the minimum prefix to 1 makes the menu appear on a
  ;; lone `/' without affecting completion anywhere else.
  (add-hook 'completion-at-point-functions #'agent-repl--skill-capf nil t)
  (setq-local company-minimum-prefix-length 1)
  (add-hook 'after-change-functions #'agent-repl--history-on-change nil t))

(defun agent-repl-discard-input ()
  "Save current input to history, clear the buffer, and enter insert state."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "discard-input")
  (agent-repl--history-push)
  (agent-repl--history-reset)
  (agent-repl--history-save (agent-repl--ws-current-name))
  (erase-buffer)
  (evil-insert-state))

(defun agent-repl-discard-or-send-interrupt ()
  "Clear Claude's prompt AND the local input buffer.
Sends a raw Ctrl-C to Claude (clearing its current input line) and,
if the local input buffer has any content (including whitespace-only),
also discards its contents.  Previously used `string-blank-p' which left
whitespace-only buffers uncleared after C-c C-c.

Exception: when Claude is `:thinking' AND the local input buffer is
non-empty, the raw Ctrl-C is suppressed — only the local buffer is
discarded (and saved to history).  This lets the user draft a message
while Claude is working and clear that draft with C-c C-c without
interrupting Claude's in-flight response."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (local-nonempty (not (zerop (buffer-size))))
         (thinking-p (eq (agent-repl--ws-agent-state ws) :thinking))
         (skip-ctrl-c (and thinking-p local-nonempty)))
    (agent-repl--log ws "discard-or-send-interrupt: clearing Claude prompt + local buffer (local-empty=%s thinking=%s skip-ctrl-c=%s)"
                      (not local-nonempty)
                      thinking-p
                      skip-ctrl-c)
    (when local-nonempty
      (agent-repl-discard-input))
    (unless skip-ctrl-c
      (agent-repl--interrupt-agent ws))))

(defun agent-repl--interrupt-agent (ws)
  "Interrupt WS's agent through its frontend (the `ctrl-c' gesture).
The gui frontend's single wire interrupt cancels pending prompts and
marks the turn aborted."
  (agent-repl--frontend-dispatch-interrupt ws 'ctrl-c))

;;; Keybindings
(map! :map agent-repl-input-mode-map
      :ni "RET"       #'agent-repl--send
      :ni "S-RET"     #'newline
      :ni "C-RET"     #'agent-repl-send-with-postfix
      ;; Deferred-prompt enqueue lives on `SPC j RET' in the leader map
      ;; (see `keybindings.el') so it's reachable from any context with
      ;; one canonical chord, instead of a buffer-local C-S-M-RET tower.
      ;; `C-S-RET' is intentionally NOT bound here -- the global drawer-visit
      ;; override (`agent-repl--install-drawer-visit-override') needs the
      ;; chord to reach `agent-repl-drawer-global-visit' from inside the
      ;; Claude input buffer.  Prefix-send (prepending
      ;; `agent-repl-send-prefix') stays reachable on macOS via `S-s-RET'
      ;; (Doom's `:gn' binding) caught by the `[remap
      ;; +default/newline-above]' entry below.
      [remap +default/newline-below] #'agent-repl-send-with-postfix
      [remap +default/newline-above] #'agent-repl-send-with-prefix
      :ni "C-c C-k"   #'agent-repl-interrupt
      :ni "C-c C-c"   #'agent-repl-discard-or-send-interrupt
      :ni "C-c r"     #'agent-repl-restart
      :ni "C-c q"     #'agent-repl-kill
      :ni "C-h"       #'evil-window-left
      :n  "<up>"        #'agent-repl--history-prev
      :n  "<down>"      #'agent-repl--history-next
      :ni "C-r"         #'agent-repl-history-search)

;; Visual-line motion bindings for the Claude input buffer.
;;
;; `visual-line-mode' is always on in `agent-repl-input-mode' (the user is
;; composing free-form prose that wraps), and `evil-respect-visual-line-mode'
;; is flipped on buffer-locally in the mode body so that line-based operators
;; (yy, dd, cc, Y, D, C) operate on screen lines.  Those operators read the
;; variable at runtime, but the basic motion bindings (j/k/0/$/V/^) are
;; installed once at Evil load time by `evil-integration.el' and so they need
;; to be reinstalled here at the mode-map level.  The `g'-prefixed variants
;; give the user the inverse (logical-line) motion when they want it.
;;
;; The bindings are declared as data in `agent-repl--visual-line-bindings'
;; (an alist of `(STATE KEY COMMAND)' triples) and then applied with
;; `evil-define-key' below.  Keeping the data separate lets the tests assert
;; the intended bindings even though `evil-define-key' is a no-op stub in
;; the test harness.
(defconst agent-repl--visual-line-bindings
  '(;; Screen-line motions for normal / motion / visual state.
    (normal  "j"  evil-next-visual-line)
    (motion  "j"  evil-next-visual-line)
    (visual  "j"  evil-next-visual-line)
    (normal  "k"  evil-previous-visual-line)
    (motion  "k"  evil-previous-visual-line)
    (visual  "k"  evil-previous-visual-line)
    (normal  "0"  evil-beginning-of-visual-line)
    (motion  "0"  evil-beginning-of-visual-line)
    (visual  "0"  evil-beginning-of-visual-line)
    (normal  "^"  evil-first-non-blank-of-visual-line)
    (motion  "^"  evil-first-non-blank-of-visual-line)
    (visual  "^"  evil-first-non-blank-of-visual-line)
    (normal  "$"  evil-end-of-visual-line)
    (motion  "$"  evil-end-of-visual-line)
    (visual  "$"  evil-end-of-visual-line)
    ;; Logical-line escape hatches (g-prefixed).
    (normal  "gj" evil-next-line)
    (motion  "gj" evil-next-line)
    (visual  "gj" evil-next-line)
    (normal  "gk" evil-previous-line)
    (motion  "gk" evil-previous-line)
    (visual  "gk" evil-previous-line)
    (normal  "g0" evil-beginning-of-line)
    (motion  "g0" evil-beginning-of-line)
    (visual  "g0" evil-beginning-of-line)
    (normal  "g$" evil-end-of-line)
    (motion  "g$" evil-end-of-line)
    (visual  "g$" evil-end-of-line)
    ;; `V' selects by screen line so it composes with the rest of the family.
    (normal  "V"  evil-visual-screen-line))
  "Visual-line evil bindings installed in `agent-repl-input-mode-map'.
Each entry is `(STATE KEY-STRING COMMAND)'.  Applied via `evil-define-key'
just below.  Declared as data so tests can assert the intended binding set
even though `evil-define-key' is a no-op stub in the test harness.")

(dolist (binding agent-repl--visual-line-bindings)
  (cl-destructuring-bind (state key cmd) binding
    (evil-define-key state agent-repl-input-mode-map (kbd key) cmd)))

;;; Input preparation and metaprompt

(defcustom agent-repl-metaprompt-exempt-strings
  '("/clear" "/usage" "/login" "/logout")
  "Inputs that should never have the metaprompt prepended.
Compared exactly against the trimmed input.

Now largely redundant with the general slash-command rule (see
`agent-repl--slash-command-p'): every entry here is itself a slash
command, so it would be exempted anyway.  The list is retained for any
future NON-slash exemption and as an explicit record of intent."
  :type '(repeat string)
  :group 'agent-repl)

(defconst agent-repl--slash-command-name-chars "A-Za-z0-9_:-"
  "The `skip-chars-forward' set for a slash-command NAME.
A name is letters, digits, `_', and the `:'/`-' that namespaced plugin
commands and hyphenated skill names use.  Deliberately excludes `/', so
a name run stops at the second slash of a path like `/Users/foo'.")

(defconst agent-repl--slash-command-regexp
  (concat "\\`/[" agent-repl--slash-command-name-chars "]+\\(?:[[:space:]]\\|\\'\\)")
  "Regexp matching an input that is a slash-command invocation.
Anchored at the very start of the string (no leading whitespace, matching
the CLI, which only treats `/' as a command at true message start), a
`/name' run must be followed by whitespace or the end of the string.
That trailing boundary is what tells a command like `/workspace-open'
apart from a path like `/Users/foo': in the path the name run is followed
by another `/', not whitespace or end.")

(defun agent-repl--slash-command-p (raw)
  "Return non-nil if RAW is a slash-command invocation.
See `agent-repl--slash-command-regexp' for exactly what counts (and what
does not, e.g. a Unix path that merely starts with `/', or a `/' preceded
by whitespace)."
  (string-match-p agent-repl--slash-command-regexp raw))

(defun agent-repl--skip-metaprompt-p (raw)
  "Return non-nil if RAW input should never have the metaprompt prepended.
Skips any slash command (see `agent-repl--slash-command-p'), every entry
of `agent-repl-metaprompt-exempt-strings', and bare numerals, ignoring
trailing whitespace.

The slash-command clause is the load-bearing one: the metaprompt is a
harness directive meant for free-form work, and a slash command runs a
skill or built-in that owns its own behavior, so prepending the directive
to it is never wanted."
  (let* ((trimmed (string-trim-right raw))
         (result (or (agent-repl--slash-command-p trimmed)
                     (member trimmed agent-repl-metaprompt-exempt-strings)
                     (string-match-p "^[0-9]+$" trimmed))))
    (agent-repl--log-verbose (agent-repl--ws-current-name) "skip-metaprompt-p: result=%s" result)
    result))

(defvar agent-repl-send-posthooks
  '(("^/clear$" . agent-repl--posthook-reset-prefix-counter)
    ("^/clear$" . agent-repl--posthook-mark-done))
  "Alist of (PATTERN . FUNCTION) posthooks run after input is sent.
PATTERN is a string or regexp matched against the raw input (trimmed).
FUNCTION is called with (WS RAW) where WS is the workspace name and RAW is the input.")

(defun agent-repl--posthook-reset-prefix-counter (ws _raw)
  "Reset the metaprompt prefix counter for workspace WS.
Resets to 0 — the same value a freshly-initialized workspace starts at
\(see `agent-repl--initialize-agent') — so the first send after a
`/clear' re-injects the metaprompt.  A `/clear' wipes Claude's context,
including the previously-prepended guidelines, so the next prompt must
re-establish them exactly as the first prompt of a new session does.
Resetting to 1 instead would skip a full period before re-injecting,
leaving Claude without guidelines in the interim."
  (agent-repl--ws-put ws :prefix-counter 0))

(defun agent-repl--posthook-mark-done (ws _raw)
  "Mark workspace WS's agent-state as :done.
Used by the /clear posthook: clearing Claude's context ends the current
work cycle, so the tab should immediately reflect \"finished\" rather
than linger on whatever state preceded the clear."
  (agent-repl--mark-agent-done ws))

(defun agent-repl--run-send-posthooks (ws raw)
  "Run posthooks matching RAW input for workspace WS."
  (let ((trimmed (string-trim-right raw)))
    (dolist (hook agent-repl-send-posthooks)
      (when (string-match-p (car hook) trimmed)
        (agent-repl--log ws "posthook matched pattern=%s" (car hook))
        (funcall (cdr hook) ws raw)))))

(defun agent-repl--should-prepend-metaprompt-p (raw counter &optional force)
  "Return non-nil if the metaprompt prefix should be prepended to RAW.
COUNTER is the current prefix counter.  FORCE bypasses the counter check."
  (let ((result (and agent-repl-skip-permissions
                     agent-repl-command-prefix
                     (not (agent-repl--skip-metaprompt-p raw))
                     (or force (zerop (mod counter agent-repl-prefix-period))))))
    (agent-repl--log-verbose (agent-repl--ws-current-name) "should-prepend-metaprompt-p: result=%s counter=%d force=%s" result counter force)
    result))

(defcustom agent-repl-workspace-command-prefix "/wor"
  "String prefix that identifies workspace-related commands.
Used to detect workspace-generation and workspace-update skills so the
source workspace identity can be tagged onto the request; see
`agent-repl--maybe-inject-source-ws'."
  :type 'string
  :group 'agent-repl)

(defun agent-repl--workspace-command-p (raw)
  "Return non-nil if RAW is a /wor workspace-generation/update command.
Compares against `agent-repl-workspace-command-prefix', ignoring
leading whitespace."
  (string-prefix-p agent-repl-workspace-command-prefix (string-trim-left raw)))

(defun agent-repl--maybe-inject-source-ws (ws raw)
  "Return RAW with a source-workspace tag appended, if RAW is a /wor command.
Appends \" [source-ws:<ws-name> path:<project-dir>]\" so the
workspace-generation and workspace-update skills know both which
workspace initiated the request and the repo root for worktree
creation.  Only the returned string (which becomes part of the
prepared INPUT sent to Claude) carries the tag — callers keep their
own RAW, used for posthook matching and history, untagged.
Signals an error if WS has no :project-dir — the skill cannot produce
a valid git_root without it."
  (if (agent-repl--workspace-command-p raw)
      (let ((dir (or (agent-repl--ws-get ws :project-dir)
                     (error "agent-repl--maybe-inject-source-ws: no :project-dir for workspace %s — cannot inject path" ws))))
        (agent-repl--log ws "maybe-inject-source-ws: injecting source-ws=%s path=%s" ws dir)
        (concat raw (format " [source-ws:%s path:%s]" ws dir)))
    raw))

(defun agent-repl--prepare-input (ws raw &optional force-metaprompt)
  "Optionally prepend metaprompt prefix to RAW for workspace WS.
When FORCE-METAPROMPT is non-nil, always prepend (ignoring the counter).
The prepended read-directive is bracketed as a harness-injected span
(`agent-repl--meta-wrap') — the agent still receives it verbatim, while
the gui frontend keeps it out of the user-turn bubble, which shows only
what the user typed.

A /wor command additionally gets a source-workspace tag appended (see
`agent-repl--maybe-inject-source-ws') so the workspace-generation and
workspace-update skills know their origin.  This only affects the
returned string; the caller's RAW (used for history and posthook
matching) is untouched."
  (let ((counter (or (agent-repl--ws-get ws :prefix-counter) 0))
        (tagged (agent-repl--maybe-inject-source-ws ws raw)))
    (agent-repl--log ws "prepare-input counter=%d period=%d" counter agent-repl-prefix-period)
    (if (agent-repl--should-prepend-metaprompt-p raw counter force-metaprompt)
        (concat (agent-repl--meta-wrap agent-repl--command-prefix) "\n\n" tagged)
      tagged)))

;;; Slash-command completion

;; The command menu is resolved by the SDK (built-ins plus user, project,
;; and plugin skills) and cached on the daemon; the input buffer reads it
;; back over HTTP and completes against it.  Company is already live in
;; this buffer (`global-company-mode'), so a `completion-at-point-functions'
;; entry is all it takes to get fuzzy matching and keyboard navigation.

(defun agent-repl--slash-commands-refetch (ws)
  "Fetch WS's slash-command menu from the daemon and cache it on WS.
Returns the fetched list (possibly nil).  An HTTP failure is caught and
logged rather than surfaced: a completion menu that cannot populate is a
degraded convenience, never a reason to interrupt what the user is
typing.  Returns nil when WS has no live session yet."
  (let ((session-id (agent-repl--ws-get ws :frontend-session-id)))
    (when session-id
      (condition-case err
          (let ((cmds (agent-repl--frontend-fetch-commands session-id)))
            (agent-repl--ws-put ws :slash-commands cmds)
            cmds)
        (error
         (agent-repl--log ws "slash-commands: fetch failed: %s"
                          (error-message-string err))
         nil)))))

(defun agent-repl--slash-commands-for-ws (ws)
  "Return WS's slash-command menu, fetching and caching it on first use.
A nil cache is refetched every call rather than treated as final: the
menu resolves asynchronously off the SDK init handshake, so an empty
answer early in a session is transient, and the daemon read is a local
round-trip.  Once the list is non-empty it is cached until invalidated
by `agent-repl--slash-commands-invalidate'."
  (or (agent-repl--ws-get ws :slash-commands)
      (agent-repl--slash-commands-refetch ws)))

(defun agent-repl--slash-commands-invalidate (ws)
  "Drop WS's cached menu and ask the daemon to re-resolve it.
Called when a skill directory changes.  Clearing the cache is what makes
the next completion refetch, and the refresh is what makes the daemon
re-probe so that refetch sees the new list.  A refresh failure is logged
rather than surfaced -- the stale menu simply stands until the next
change."
  (agent-repl--ws-put ws :slash-commands nil)
  (let ((session-id (agent-repl--ws-get ws :frontend-session-id)))
    (when session-id
      (condition-case err
          (agent-repl--frontend-refresh-commands session-id)
        (error
         (agent-repl--log ws "slash-commands: refresh failed: %s"
                          (error-message-string err)))))))

(defun agent-repl--slash-command-watch-dirs (ws)
  "Return the existing skill directories to watch for workspace WS.
Covers the user config's `skills' and `commands' directories (honoring
CLAUDE_CONFIG_DIR, since a workspace may run under a non-default config
root) and, when WS has a project dir, that project's `.claude/skills' and
`.claude/commands'.  Only directories that exist are returned, so a
project without a `.claude' contributes nothing."
  (let* ((config-dir (or (getenv "CLAUDE_CONFIG_DIR")
                         (expand-file-name "~/.claude")))
         (project (agent-repl--ws-get ws :project-dir))
         (candidates (list (expand-file-name "skills" config-dir)
                           (expand-file-name "commands" config-dir))))
    (when project
      (setq candidates
            (append candidates
                    (list (expand-file-name ".claude/skills" project)
                          (expand-file-name ".claude/commands" project)))))
    (seq-filter #'file-directory-p candidates)))

(defvar-local agent-repl--slash-command-watchers nil
  "File-notify descriptors watching this input buffer's skill dirs.
Buffer-local so the watchers live and die with the input buffer, and are
torn down by its `kill-buffer-hook'.")

(defun agent-repl--slash-commands-teardown-watch ()
  "Remove this buffer's skill-directory watchers."
  (dolist (desc agent-repl--slash-command-watchers)
    (when (file-notify-valid-p desc)
      (file-notify-rm-watch desc)))
  (setq agent-repl--slash-command-watchers nil))

(defun agent-repl--slash-commands-ensure-watch (ws)
  "Install skill-directory watchers for WS in the current buffer, once.
On any change under a watched directory, WS's cached menu is invalidated
and the daemon is asked to re-resolve it, so a skill added or edited
mid-session appears in completion without a restart.

No-op in batch (`noninteractive'): a headless run completes nothing, and
installing real watchers would leak them into the test process.  Also a
no-op once installed, so calling it from every completion is cheap."
  (when (and (not noninteractive)
             (null agent-repl--slash-command-watchers))
    (setq agent-repl--slash-command-watchers
          (delq nil
                (mapcar
                 (lambda (dir)
                   (file-notify-add-watch
                    dir '(change)
                    (lambda (_event) (agent-repl--slash-commands-invalidate ws))))
                 (agent-repl--slash-command-watch-dirs ws))))
    (add-hook 'kill-buffer-hook #'agent-repl--slash-commands-teardown-watch nil t)))

(defun agent-repl--skill-capf-bounds ()
  "Return (START . END) of the slash-command token at point, or nil.
START is the leading slash and END the end of the command-name run, but
only when that slash sits at the very start of the buffer and the name
run is not terminated by a second slash (which would make it a path).
Point must lie within the token.  nil in every other position, so
completion never fires inside a path or later in the message."
  (let* ((start (save-excursion
                  (goto-char (point-min))
                  (point)))
         (p (point)))
    (when (and (< start (point-max))
               (eq (char-after start) ?/))
      (let ((name-end (save-excursion
                        (goto-char (1+ start))
                        (skip-chars-forward agent-repl--slash-command-name-chars)
                        (point))))
        ;; A second slash right after the name run means this is a path
        ;; (`/Users/foo'), not a command.
        (when (and (>= p start)
                   (<= p name-end)
                   (not (eq (char-after name-end) ?/)))
          (cons start name-end))))))

(defun agent-repl--skill-capf-candidates (ws)
  "Return WS's slash commands as completion candidates.
Each candidate is a `/name' string (the completion region includes the
leading slash) carrying its argument hint and description as text
properties, which `agent-repl--skill-capf-annotation' reads back."
  (mapcar
   (lambda (cmd)
     (let ((name (alist-get 'name cmd))
           (hint (or (alist-get 'argumentHint cmd) ""))
           (desc (or (alist-get 'description cmd) "")))
       (propertize (concat "/" name)
                   'agent-repl-skill-hint hint
                   'agent-repl-skill-desc desc)))
   (agent-repl--slash-commands-for-ws ws)))

(defun agent-repl--skill-capf-annotation (candidate)
  "Return the inline annotation for CANDIDATE: its argument hint.
An empty hint (a command that takes no argument) annotates to nil so no
trailing space is shown."
  (let ((hint (get-text-property 0 'agent-repl-skill-hint candidate)))
    (when (and hint (not (string-empty-p hint)))
      (concat " " hint))))

(defun agent-repl--skill-capf ()
  "`completion-at-point-functions' entry for slash commands.
Offers the session's command menu when the text before point is a
`/name' fragment at the very start of the input, and returns nil
everywhere else.  The workspace is resolved from the buffer's permanent
owner so it survives the perspective drifting under a long turn."
  (let ((bounds (agent-repl--skill-capf-bounds))
        (ws (or agent-repl--owning-workspace (agent-repl--ws-current-name))))
    (when (and bounds ws)
      (agent-repl--slash-commands-ensure-watch ws)
      (list (car bounds)
            (cdr bounds)
            (agent-repl--skill-capf-candidates ws)
            :annotation-function #'agent-repl--skill-capf-annotation
            :exclusive 'no))))

(defun agent-repl--company-abort-and-send ()
  "Abort any open company popup, then send the input.
Bound to RET in `company-active-map' for the agent input buffer only, so
RET always sends there rather than being swallowed by company's default
of completing the highlighted selection.  Accepting a candidate is TAB's
job (`company-complete-common-or-cycle'); RET sends exactly what was
typed."
  (interactive)
  (when (bound-and-true-p company-candidates)
    (company-abort))
  (call-interactively #'agent-repl--send))

(with-eval-after-load 'company
  ;; Company raises its keymap above the input mode's, so its RET
  ;; (`company-complete-selection') would otherwise shadow the send key
  ;; whenever the popup is open.  Reclaim RET for this buffer only, via a
  ;; runtime-dispatched binding: the input buffer sends, every other buffer
  ;; keeps company's default of completing the selection.  The else branch
  ;; restores `company-complete-selection' explicitly rather than falling
  ;; through, because this overwrites the very binding it would fall to.
  (dolist (key (list (kbd "RET") [return]))
    (define-key company-active-map key
                '(menu-item
                  "" agent-repl--company-abort-and-send
                  :filter (lambda (_)
                            (if (eq major-mode 'agent-repl-input-mode)
                                #'agent-repl--company-abort-and-send
                              #'company-complete-selection))))))

;;; Send pipeline

(defun agent-repl--mark-ws-thinking (ws)
  "Mark workspace WS as thinking: set agent-state."
  (agent-repl--log ws "mark-ws-thinking ws=%s" ws)
  (agent-repl--ws-set-agent-state ws :thinking))

(defun agent-repl--increment-prefix-counter (ws)
  "Increment the metaprompt prefix counter for workspace WS."
  (let ((new-val (1+ (or (agent-repl--ws-get ws :prefix-counter) 0))))
    (agent-repl--log-verbose ws "increment-prefix-counter: ws=%s new-counter=%d" ws new-val)
    (agent-repl--ws-put ws :prefix-counter new-val)))

(defun agent-repl--do-send (ws input raw &optional on-settle)
  "Core send: dispatch INPUT to WS's agent through its frontend.
A pure delegation to `agent-repl--frontend-dispatch-send', which looks
up WS's registered frontend and calls its `:send-fn'.  RAW (the
undecorated user text, before metaprompt/tag prepending) is threaded
through unchanged so the frontend's send implementation can run
posthooks and kick off the prompt summary against exactly what the
user typed.  ON-SETTLE, if non-nil, is forwarded to the frontend's
send function and called once the send is fully committed."
  (agent-repl--frontend-dispatch-send ws input raw on-settle))

(defun agent-repl--commit-input-buffer (ws input-buf raw &optional clear-p)
  "Record RAW input in history and optionally clear INPUT-BUF.
WS is the workspace name used for history persistence.
When CLEAR-P is non-nil, erase the input buffer after saving history."
  (agent-repl--log ws "commit-input-buffer: ws=%s clear-p=%s" ws clear-p)
  (when (and input-buf (buffer-live-p input-buf))
    (with-current-buffer input-buf
      (agent-repl--history-push raw)
      (agent-repl--history-reset)
      (when clear-p (erase-buffer))))
  (agent-repl--history-save ws))

(defun agent-repl--read-input-buffer (ws)
  "Return the text contents of the input buffer for workspace WS, or nil."
  (agent-repl--log-verbose ws "read-input-buffer: ws=%s" ws)
  (when-let ((buf (agent-repl--ws-get ws :input-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (buffer-string)))))

(defun agent-repl--send (&optional prompt ws force-metaprompt on-settle)
  "Send PROMPT (or input buffer contents) to Claude in workspace WS.
When PROMPT is nil, reads from the input buffer and clears it after sending.
When WS is nil, uses the current workspace.
When FORCE-METAPROMPT is non-nil, always prepend the metaprompt prefix.
ON-SETTLE, if non-nil, is called once the send is fully committed.
Handles input preparation, sending, history, and persistence."
  (interactive)
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (unless ws (error "agent-repl--send: no active workspace"))
    (agent-repl--log ws "send: ws=%s force-metaprompt=%s from-buf=%s" ws force-metaprompt (null prompt))
    (let* ((from-buf  (null prompt))
           (input-buf (agent-repl--ws-get ws :input-buffer))
           (raw       (or prompt (agent-repl--read-input-buffer ws)))
           ;; Empty-string is truthy in Elisp -- guard explicitly so RET on
           ;; an empty input buffer doesn't dispatch a metaprompt-only send.
           (raw-empty (or (null raw) (string-empty-p (string-trim raw)))))
      (if raw-empty
          (agent-repl--log ws "send: empty input -- nothing to send")
        (let ((input (agent-repl--prepare-input ws raw force-metaprompt)))
          (agent-repl--do-send ws input raw on-settle)
          (agent-repl--commit-input-buffer ws input-buf raw from-buf))))))

(defun agent-repl-send-and-hide ()
  "Send input to Claude and hide both panels."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "send-and-hide")
  (agent-repl--send)
  (agent-repl--on-close))

(defun agent-repl-send-with-metaprompt ()
  "Send input with the metaprompt prefix, bypassing the counter."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "send-with-metaprompt")
  (agent-repl--send nil nil t))

(defun agent-repl--append-to-input-buffer (text)
  "Append TEXT to the end of the current workspace's input buffer."
  (agent-repl--log (agent-repl--ws-current-name) "append-to-input-buffer: len=%d" (length text))
  (let ((buf (agent-repl--ws-get (agent-repl--ws-current-name) :input-buffer)))
    (if buf
        (with-current-buffer buf
          (goto-char (point-max))
          (insert text))
      (agent-repl--warn nil "no input buffer for current workspace — text not appended")
      (agent-repl--log (agent-repl--ws-current-name) "append-to-input-buffer: no input buffer, text discarded"))))

(defun agent-repl-send-with-postfix ()
  "Append `agent-repl-send-postfix' to the input buffer, then send."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "send-with-postfix")
  (agent-repl--append-to-input-buffer agent-repl-send-postfix)
  (agent-repl--send))

(defun agent-repl--prepend-to-input-buffer (text)
  "Prepend TEXT to the start of the current workspace's input buffer."
  (agent-repl--log (agent-repl--ws-current-name) "prepend-to-input-buffer: len=%d" (length text))
  (let ((buf (agent-repl--ws-get (agent-repl--ws-current-name) :input-buffer)))
    (if buf
        (with-current-buffer buf
          (goto-char (point-min))
          (insert text))
      (agent-repl--warn nil "no input buffer for current workspace — text not prepended")
      (agent-repl--log (agent-repl--ws-current-name) "prepend-to-input-buffer: no input buffer, text discarded"))))

(defun agent-repl-send-with-prefix ()
  "Prepend `agent-repl-send-prefix' to the input buffer, then send."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "send-with-prefix")
  (agent-repl--prepend-to-input-buffer agent-repl-send-prefix)
  (agent-repl--send))

(defun agent-repl-queue-deferred-prompt ()
  "Queue the current input buffer contents for delivery when Claude is idle.

Claude's native UI already buffers paste-while-thinking input, but
those buffered keystrokes interleave with whatever else the user
types before Claude finishes its turn — there is no guarantee the
queued text will fire as its own discrete prompt.  This command
provides that guarantee: the input buffer text is captured into
WS's `:deferred-prompts' FIFO and held until Claude reaches `:done'
\(turn finished, Stop hook resolved) or `:idle' (decayed), at which
point `agent-repl--drain-deferred-prompts' pops the head and
delivers it as a standalone prompt.  The queue is arbitrarily long;
each subsequent finished turn drains one entry.

If WS is already `:done' / `:idle' when this command runs, the
enqueue is followed by an immediate drain so a lone queued prompt
fires right away instead of sitting until the next state change.

The input buffer is cleared and the captured text is pushed onto
the input-history ring (the same as a regular send), so the user
can recall it via the history keys."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (input-buf (agent-repl--ws-get ws :input-buffer))
         (raw (agent-repl--read-input-buffer ws))
         (raw-empty (or (null raw) (string-empty-p (string-trim raw)))))
    (cond
     (raw-empty
      (agent-repl--log ws "queue-deferred-prompt: ws=%s empty input — no-op" ws)
      (message "[agent-repl] no input to queue"))
     (t
      (let* ((prior (agent-repl--ws-get ws :deferred-prompts))
             (new   (append prior (list raw))))
        (agent-repl--ws-put ws :deferred-prompts new)
        (agent-repl--log ws "queue-deferred-prompt: ws=%s enqueued len=%d queue-depth=%d"
                          ws (length raw) (length new))
        (agent-repl--commit-input-buffer ws input-buf raw t)
        (message "[agent-repl] queued prompt #%d for %s (fires when :done/:idle)"
                 (length new) ws)
        (agent-repl--drain-deferred-prompts ws))))))
