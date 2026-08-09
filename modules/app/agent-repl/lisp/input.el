;;; input.el --- input mode and send system -*- lexical-binding: t; -*-

;;; Code:

;;; Metaprompt on-demand re-read

;; THE METAPROMPT IS THE SESSION'S SYSTEM PROMPT, and nothing here injects it.
;; The shim reads `metaprompt.md' out of the canonical doom checkout at
;; `~/.config/doom' — never out of the session's own cwd — and hands it to the
;; SDK as a `claude_code' preset append (agent-shim/claude/shim/src/
;; metaprompt.ts), so the guidelines are re-sent with every request and survive
;; `/clear', `/compact', and resume without anyone re-establishing them.
;;
;; What remains here is the MANUAL re-read: `agent-repl-send-with-metaprompt'
;; and `agent-repl--fire-metaprompt-read', for deliberately telling the agent
;; to go read the file again.  Neither fires on its own.

(defcustom agent-repl-skip-permissions t
  "When non-nil, the on-demand metaprompt re-read is available.
Turning this off makes `agent-repl-send-with-metaprompt' and
`agent-repl--fire-metaprompt-read' no-ops.  It does NOT affect the
metaprompt the shim installs as the session's system prompt, which is
the only path by which the guidelines ordinarily reach the agent."
  :type 'boolean
  :group 'agent-repl)

(defvar agent-repl-metaprompt-file
  (expand-file-name "../metaprompt.md"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the canonical metaprompt source file in this repository.
The .md data file lives as plain text at the module root, one level above
this file's own `lisp/' directory, edited and version-controlled
alongside the code, and is read at session spawn by the shim.  It is
ALSO the path the on-demand read-directive names.  Captured at file-load
time because `load-file-name' is only bound during load.")

(defcustom agent-repl-command-prefix
  (with-temp-buffer
    (insert-file-contents agent-repl-metaprompt-file)
    (buffer-string))
  "Canonical metaprompt content, loaded from `agent-repl-metaprompt-file'.
Never sent to Claude from Emacs.  The shim installs this same content as
the session's system prompt, and the on-demand directive
\(`agent-repl-command-prefix-template') names the file rather than
quoting it.  This variable mirrors the file's content for tests and
tooling that need to assert against the canonical metaprompt without
re-reading the file."
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
  "Template instructing Claude to re-read the metaprompt file before acting.
Must contain a single %s placeholder, filled with the workspace's own
metaprompt path.  Sent ONLY on an explicit request
\(`agent-repl-send-with-metaprompt', `agent-repl--fire-metaprompt-read'),
never periodically.  Intentionally avoids any \"metaprompt\" terminology
in the directive itself — the framing lives inside the .md file rather
than here, so the directive is a plain instruction to read the file."
  :type 'string
  :group 'agent-repl)

(defvar agent-repl--command-prefix
  (format agent-repl-command-prefix-template
          agent-repl-metaprompt-file)
  "Formatted read-directive for a workspace with no worktree-local copy.
Sent only on an explicit on-demand re-read, gated by
`agent-repl-skip-permissions'.")

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

(defcustom agent-repl-input-background-shade 20
  "Base greyscale level (0-255) for the input buffer background.
Sets the red and green channels; the blue channel adds
`agent-repl-input-background-blue-boost' on top for a faint blue tint."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-input-background-blue-boost 6
  "Extra amount added to the blue channel of the input buffer background.
Nudges the otherwise-grey background very slightly toward blue."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--input-background-color ()
  "Return the input buffer background as a #rrggbb hex string.
A dark grey base (`agent-repl-input-background-shade') tinted very
slightly blue by `agent-repl-input-background-blue-boost'."
  (agent-repl--rgb-hex agent-repl-input-background-shade
                       agent-repl-input-background-shade
                       (+ agent-repl-input-background-shade
                          agent-repl-input-background-blue-boost)))

;; Input mode
(define-derived-mode agent-repl-input-mode fundamental-mode "Agent Input"
  "Major mode for Agent REPL input buffer."
  (setq-local header-line-format
              "C-c C-c: clear+save | C-c C-k: interrupt | (cmd) <up>/<down>: history | C-r: search history")
  (face-remap-add-relative 'header-line 'agent-repl-header-line)
  (agent-repl--set-buffer-background (agent-repl--input-background-color))
  ;; Slash-command completion: our capf is the buffer's only completion
  ;; source, so dropping the minimum prefix to 1 makes the menu appear on a
  ;; lone `/' without affecting completion anywhere else.
  (add-hook 'completion-at-point-functions #'agent-repl--skill-capf nil t)
  (setq-local company-minimum-prefix-length 1)
  ;; Widen the fill column in the composer so wrapped prose and `fill-paragraph'
  ;; reflow to 150 columns rather than the 70-column default.
  (setq-local fill-column 150)
  ;; Soft-wrap long input lines at word boundaries so a long prompt stays
  ;; fully visible instead of running off the window edge or breaking
  ;; mid-word.  Set the two underlying variables directly rather than
  ;; enabling `visual-line-mode': that minor mode was deliberately dropped
  ;; here (commit 7dfef0d2) because it also remapped Evil's line motions to
  ;; screen lines, which is unwanted.  `word-wrap' plus a nil `truncate-lines'
  ;; gives the wrapping without touching cursor-motion semantics.
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  ;; Intentionally unlogged: `after-change-functions' runs per keystroke, so
  ;; logging it would overwhelm the durable input/send lifecycle diagnostics.
  (add-hook 'after-change-functions #'agent-repl--history-on-change nil t))

(defun agent-repl-discard-input ()
  "Save current input to history, clear the buffer, and enter insert state."
  (interactive)
  (let ((ws (agent-repl--ws-current-name))
        (input-len (buffer-size)))
    (agent-repl--log ws "discard-input ws=%s input-len=%d" ws input-len)
    (agent-repl--history-push)
    (agent-repl--history-reset)
    (agent-repl--history-save ws)
    (erase-buffer)
    (evil-insert-state)))

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
  (agent-repl--log ws "interrupt-agent gesture=ctrl-c")
  (agent-repl--frontend-dispatch-interrupt ws 'ctrl-c))

;;; Keybindings
(map! :map agent-repl-input-mode-map
      :ni "RET"       #'agent-repl--send
      :ni "S-RET"     #'newline
      :ni "C-RET"     #'agent-repl-send-with-postfix
      ;; Deferred-prompt enqueue lives on `SPC j RET' in the leader map
      ;; (see `keybindings.el') so it's reachable from any context with
      ;; one canonical chord, instead of a buffer-local C-S-M-RET tower.
      ;; Prefix-send (prepending `agent-repl-send-prefix') stays
      ;; reachable on macOS via `S-s-RET' (Doom's `:gn' binding) caught
      ;; by the `[remap +default/newline-above]' entry below.
      [remap +default/newline-below] #'agent-repl-send-with-postfix
      [remap +default/newline-above] #'agent-repl-send-with-prefix
      :ni "C-c C-k"   #'agent-repl-interrupt
      :ni "C-c C-c"   #'agent-repl-discard-or-send-interrupt
      :ni "C-c r"     #'agent-repl-restart
      :ni "C-c q"     #'agent-repl-kill
      :n  "<up>"        #'agent-repl--history-prev
      :n  "<down>"      #'agent-repl--history-next
      ;; Prompt-history search sits on `C-M-r', not the `C-r' its shell
      ;; reflex would suggest: `C-r' is vacated for the output feed's
      ;; incremental search (webapp/src/search.ts), whose isearch reflex
      ;; wants it.  Deliberately left UNBOUND here rather than rebound --
      ;; the two searches hunt different surfaces (this buffer's past
      ;; prompts vs the output window's text), and the input box can only
      ;; give the chord to one of them.
      :ni "C-M-r"       #'agent-repl-history-search
      ;; Cycle the OUTPUT feed without leaving this buffer (output-nav.el):
      ;; a modifier per bubble class, `j'/`k' for down/up per vim.  The
      ;; webview scrolls and marks the bubble; focus stays here and typing
      ;; keeps working.
      ;;
      ;; Both states MUST stay bound, and not only so the chords work in
      ;; each.  `C-j'/`C-k' are bound on `override-global-map' (config.el)
      ;; to `evil-window-down' and `kill-visual-line'; leaving a shifted
      ;; chord unbound lets Emacs shift-translate it down to the unshifted
      ;; one, so an unbound `C-S-k' here would silently KILL THE LINE the
      ;; user is composing instead of cycling anything.  Binding the
      ;; shifted chord is what suppresses that translation.
      :ni "C-S-j"       #'agent-repl-output-next-prompt
      :ni "C-S-k"       #'agent-repl-output-prev-prompt
      :ni "M-S-j"       #'agent-repl-output-next-final
      :ni "M-S-k"       #'agent-repl-output-prev-final
      :ni "C-M-S-j"     #'agent-repl-output-next-tool
      :ni "C-M-S-k"     #'agent-repl-output-prev-tool
      ;; Workspace-sidebar keyboard navigation (sidebar.el): `C-S-n' /
      ;; `C-S-p' move the roster cursor AND open the workspace it lands
      ;; on (auto-select), while `C-S-<return>' unfolds that row's detail
      ;; panel — all without leaving the input box, since the xwidget
      ;; swallows keystrokes and the sidebar's keys must live here.  Same
      ;; shifted-chord rule as above: unbound, `C-S-n'/`C-S-p' would
      ;; shift-translate down to `C-n'/`C-p' motion in the composer.
      :ni "C-S-n"        #'agent-repl-sidebar-nav-next
      :ni "C-S-p"        #'agent-repl-sidebar-nav-prev
      :ni "C-S-<return>" #'agent-repl-sidebar-nav-show-info)

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
That trailing boundary is what tells a command like
`/create-or-update-workspace open' apart from a path like
`/Users/foo': in the path the name run is followed by another `/',
not whitespace or end.")

(defun agent-repl--slash-command-p (raw)
  "Return non-nil if RAW is a slash-command invocation.
See `agent-repl--slash-command-regexp' for exactly what counts (and what
does not, e.g. a Unix path that merely starts with `/', or a `/' preceded
by whitespace)."
  (string-match-p agent-repl--slash-command-regexp raw))

(defun agent-repl--skip-metaprompt-p (raw &optional ws)
  "Return non-nil if RAW input should never have the metaprompt prepended.
Skips any slash command (see `agent-repl--slash-command-p'), every entry
of `agent-repl-metaprompt-exempt-strings', and bare numerals, ignoring
trailing whitespace.

The slash-command clause is the load-bearing one: the metaprompt is a
harness directive meant for free-form work, and a slash command runs a
skill or built-in that owns its own behavior, so prepending the directive
to it is never wanted.  WS, when supplied, scopes the diagnostic entry."
  (let* ((trimmed (string-trim-right raw))
         (slash-command-p (not (null (agent-repl--slash-command-p trimmed))))
         (exempt-p (not (null (member trimmed agent-repl-metaprompt-exempt-strings))))
         (numeral-p (not (null (string-match-p "^[0-9]+$" trimmed))))
         (result (or slash-command-p exempt-p numeral-p)))
    (agent-repl--log-verbose ws
                              "skip-metaprompt-p raw-len=%d trimmed-len=%d slash-command=%s exempt=%s numeral=%s result=%s"
                              (length raw) (length trimmed) slash-command-p exempt-p numeral-p result)
    result))

(defvar agent-repl-send-posthooks
  '(("^/clear$" . agent-repl--posthook-mark-done))
  "Alist of (PATTERN . FUNCTION) posthooks run after input is sent.
PATTERN is a string or regexp matched against the raw input (trimmed).
FUNCTION is called with (WS RAW) where WS is the workspace name and
RAW is the input.")

(defun agent-repl--posthook-mark-done (ws _raw)
  "Mark workspace WS's agent-state as :done.
Used by the /clear posthook: clearing Claude's context ends the current
work cycle, so the tab should immediately reflect \"finished\" rather
than linger on whatever state preceded the clear."
  (agent-repl--log ws "posthook-mark-done new-agent-state=:done")
  (agent-repl--mark-agent-done ws))

(defun agent-repl--run-send-posthooks (ws raw)
  "Run posthooks matching RAW input for workspace WS."
  (let ((trimmed (string-trim-right raw))
        (matched-count 0))
    (dolist (hook agent-repl-send-posthooks)
      (when (string-match-p (car hook) trimmed)
        (cl-incf matched-count)
        (agent-repl--log ws "posthook matched pattern=%s" (car hook))
        (funcall (cdr hook) ws raw)))
    (agent-repl--log ws "posthook scan raw-len=%d hook-count=%d matched-count=%d"
                      (length raw) (length agent-repl-send-posthooks) matched-count)))

(defun agent-repl--should-prepend-metaprompt-p (raw force &optional ws)
  "Return non-nil if the read-directive should be prepended to RAW.
FORCE is the caller's explicit request for it — nothing else prepends,
because the guidelines reach the agent as the session's system prompt
rather than as anything injected into a prompt.
WS, when supplied, scopes the diagnostic entry."
  (let* ((system-enabled-p (and agent-repl-skip-permissions
                                agent-repl-command-prefix
                                t))
         (force-p (and force t))
         ;; Preserve short-circuiting: a disabled system or an unforced send
         ;; must not evaluate the exemption check.
         (skip-p (and system-enabled-p force-p
                      (agent-repl--skip-metaprompt-p raw ws)
                      t))
         (result (and system-enabled-p force-p (not skip-p) t)))
    (agent-repl--log-verbose ws
                              "should-prepend-metaprompt-p enabled=%s prompt-len=%d force=%s skip=%s result=%s"
                              system-enabled-p (length raw) force-p skip-p result)
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
    (agent-repl--log-verbose ws "maybe-inject-source-ws: no workspace-command raw-len=%d" (length raw))
    raw))

(defun agent-repl--metaprompt-file-for (ws)
  "Metaprompt path the read-directive should point WS at.
`agent-repl-metaprompt-file' is fixed at load time to the checkout Emacs
loaded `input.el' from — the main worktree — so a WS running in a
DIFFERENT worktree of the same repo would be told to read (and thereby be
primed to edit against) that OTHER tree rather than its own.  That is the
root of the recurring \"edited master instead of the worktree\" mistake:
the agent works in its worktree but every absolute path it is handed is
master-rooted.  When WS's own `:project-dir' carries the metaprompt at the
in-repo path, return that copy so the agent reads, and is primed against,
the worktree it actually runs in; otherwise fall back to the canonical
file — WS is a foreign project that does not vendor the agent-repl
module, so the canonical path is the only metaprompt there is."
  (let* ((root (agent-repl--ws-get ws :project-dir))
         (in-ws (and root
                     (expand-file-name "modules/app/agent-repl/metaprompt.md"
                                       root)))
         (worktree-copy-p (and in-ws (file-exists-p in-ws))))
    (agent-repl--log ws "metaprompt-file-for root-present=%s worktree-copy=%s selected=%s"
                      (not (null root)) (not (null worktree-copy-p))
                      (if worktree-copy-p "workspace" "canonical"))
    (if worktree-copy-p in-ws agent-repl-metaprompt-file)))

(defun agent-repl--command-prefix-for (ws)
  "Read-directive string for WS, pointing at WS's own metaprompt copy.
Formats `agent-repl-command-prefix-template' with
`agent-repl--metaprompt-file-for' so each workspace is told to read the
metaprompt inside the worktree it runs in.  Reduces to the shared,
pre-formatted `agent-repl--command-prefix' whenever WS resolves to the
canonical file, so foreign-project workspaces (and callers that mock the
global prefix) are byte-for-byte unchanged."
  (let ((file (agent-repl--metaprompt-file-for ws)))
    (agent-repl--log ws "command-prefix-for selected=%s"
                      (if (equal file agent-repl-metaprompt-file) "canonical" "workspace"))
    (if (equal file agent-repl-metaprompt-file)
        agent-repl--command-prefix
      (format agent-repl-command-prefix-template file))))

(defun agent-repl--prepare-input (ws raw &optional force-metaprompt)
  "Optionally prepend the read-directive to RAW for workspace WS.
The directive is prepended ONLY when FORCE-METAPROMPT is non-nil: the
metaprompt itself arrives as the session's system prompt, so an ordinary
send carries nothing extra.
The prepended read-directive is bracketed as a harness-injected span
(`agent-repl--meta-wrap') — the agent still receives it verbatim, while
the gui frontend keeps it out of the user-turn bubble, which shows only
what the user typed.

A /wor command additionally gets a source-workspace tag appended (see
`agent-repl--maybe-inject-source-ws') so the workspace-generation and
workspace-update skills know their origin.  This only affects the
returned string; the caller's RAW (used for history and posthook
matching) is untouched."
  (let* ((tagged (agent-repl--maybe-inject-source-ws ws raw))
         (prepend-p (agent-repl--should-prepend-metaprompt-p raw force-metaprompt ws)))
    (agent-repl--log ws "prepare-input raw-len=%d tagged-len=%d force=%s prepend=%s"
                      (length raw) (length tagged) force-metaprompt prepend-p)
    (if prepend-p
        (concat (agent-repl--meta-wrap (agent-repl--command-prefix-for ws)) "\n\n" tagged)
      tagged)))

;;; Slash-command completion

;; The command menu is resolved by the SDK (built-ins plus user, project,
;; and plugin skills), retained by the daemon as the session's `SystemInit',
;; and PUSHED to Emacs as a `frontend.v1' `SessionInitView' frame (on attach
;; and in the connect snapshot).  The input buffer reads the retained
;; `slashCommands' straight off the pushed-frame store (frontend-state.el) —
;; there is no HTTP fetch, no local cache to invalidate, and no skill-dir
;; watcher to poke a daemon re-resolve (the daemon owns re-resolution and
;; re-pushes the frame).  Company is already live in this buffer
;; (`global-company-mode'), so a `completion-at-point-functions' entry is all
;; it takes to get fuzzy matching and keyboard navigation.
;;
;; NOTE: `SystemInit.slash_commands' is `repeated string' (command NAMES
;; only), so the per-command argument-hint / description the old GET /commands
;; menu carried are not available from the pushed source — the completion
;; offers bare `/name' candidates.

(defun agent-repl--slash-commands-for-ws (ws)
  "Return WS's slash-command names from the pushed `SessionInit' store.
Looks WS's retained `SystemInit' up by its workspace
\(`agent-repl--frontend-session-init') and returns the `slashCommands'
list (command NAME strings).  nil when WS has no project dir, or no init
has been pushed for it yet (a transient startup state, not an error)."
  (when-let ((workspace (agent-repl--ws-get ws :project-dir)))
    (plist-get (agent-repl--frontend-session-init workspace) :slashCommands)))

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
leading slash).  The pushed `SystemInit.slashCommands' carries command
NAMES only, so — unlike the old GET /commands menu — no argument-hint or
description annotation is available."
  (mapcar (lambda (name) (concat "/" name))
          (agent-repl--slash-commands-for-ws ws)))

(defun agent-repl--skill-capf ()
  "`completion-at-point-functions' entry for slash commands.
Offers the session's command menu (from the pushed `SessionInit') when the
text before point is a `/name' fragment at the very start of the input, and
returns nil everywhere else.  The workspace is resolved from the buffer's
permanent owner so it survives the perspective drifting under a long turn."
  ;; Intentionally unlogged: CAPF is queried on keystrokes and redisplay;
  ;; send-pipeline logs capture the corresponding bounded user action.
  (let ((bounds (agent-repl--skill-capf-bounds))
        (ws (or agent-repl--owning-workspace (agent-repl--ws-current-name))))
    (when (and bounds ws)
      (list (car bounds)
            (cdr bounds)
            (agent-repl--skill-capf-candidates ws)
            :exclusive 'no))))

;; Company is an optional runtime dependency and absent from the
;; compile-time load path; declare the pieces the two forms below use.
(declare-function company-abort "company")
(defvar company-active-map)

(defun agent-repl--company-abort-and-send ()
  "Abort any open company popup, then send the input.
Bound to RET in `company-active-map' for the agent input buffer only, so
RET always sends there rather than being swallowed by company's default
of completing the highlighted selection.  Accepting a candidate is TAB's
job (`company-complete-common-or-cycle'); RET sends exactly what was
typed."
  (interactive)
  (let ((ws (or agent-repl--owning-workspace (agent-repl--ws-current-name)))
        (popup-active-p (bound-and-true-p company-candidates)))
    (agent-repl--log ws "company-abort-and-send popup-active=%s" popup-active-p)
    (when popup-active-p
      (company-abort))
    (call-interactively #'agent-repl--send)))

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

(defun agent-repl--do-send (ws input raw prompt-origin &optional on-settle)
  "Core send: dispatch INPUT to WS's agent through its frontend.
A pure delegation to `agent-repl--frontend-dispatch-send', which looks
up WS's registered frontend and calls its `:send-fn'.  RAW (the
undecorated user text, before metaprompt/tag prepending) is threaded
through unchanged so the frontend's send implementation can run
posthooks and kick off the prompt summary against exactly what the
user typed.  ON-SETTLE, if non-nil, is forwarded to the frontend's
send function and called once the send is fully committed."
  (agent-repl--log ws "do-send input-len=%d raw-len=%d prompt-origin=%s on-settle=%s"
                    (length input) (length raw) prompt-origin (not (null on-settle)))
  (agent-repl--frontend-dispatch-send ws input raw prompt-origin on-settle))

(defun agent-repl--commit-input-buffer (ws input-buf raw &optional clear-p)
  "Record RAW input in history and optionally clear INPUT-BUF.
WS is the workspace name used for history persistence.
When CLEAR-P is non-nil, erase the input buffer after saving history."
  (let ((input-buffer-live-p (and input-buf (buffer-live-p input-buf))))
    (agent-repl--log ws "commit-input-buffer raw-len=%d clear-p=%s input-buffer-present=%s input-buffer-live=%s"
                      (length raw) clear-p (not (null input-buf)) (not (null input-buffer-live-p)))
    (when input-buffer-live-p
      (with-current-buffer input-buf
        (agent-repl--history-push raw)
        (agent-repl--history-reset)
        (when clear-p (erase-buffer))))
    (agent-repl--history-save ws)))

(defun agent-repl--read-input-buffer (ws)
  "Return the text contents of the input buffer for workspace WS, or nil."
  (let ((buf (agent-repl--ws-get ws :input-buffer)))
    (cond
     ((not buf)
      (agent-repl--log-verbose ws "read-input-buffer input-buffer-present=false result=nil")
      nil)
     ((not (buffer-live-p buf))
      (agent-repl--log-verbose ws "read-input-buffer input-buffer-present=true input-buffer-live=false result=nil")
      nil)
     (t
      (let ((contents (with-current-buffer buf (buffer-string))))
        (agent-repl--log-verbose ws "read-input-buffer input-buffer-present=true input-buffer-live=true result-len=%d"
                                  (length contents))
        contents)))))

(defun agent-repl--send (prompt-origin &optional prompt ws force-metaprompt on-settle)
  "Send PROMPT (or input buffer contents) to Claude in workspace WS.
When PROMPT is nil, reads from the input buffer and clears it after sending.
When WS is nil, uses the current workspace.
When FORCE-METAPROMPT is non-nil, always prepend the metaprompt prefix.
ON-SETTLE, if non-nil, is called once the send is fully committed.
Handles input preparation, sending, history, and persistence."
  (interactive (list "PROMPT_ORIGIN_USER_SENT"))
  (unless (and (stringp prompt-origin)
               (string-prefix-p "PROMPT_ORIGIN_" prompt-origin)
               (not (equal prompt-origin "PROMPT_ORIGIN_UNSPECIFIED")))
    (agent-repl--error ws "send: invalid prompt-origin=%S" prompt-origin))
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (unless ws
      (agent-repl--error nil "send: no active workspace prompt-supplied=%s force-metaprompt=%s on-settle=%s"
                          (not (null prompt)) force-metaprompt (not (null on-settle))))
    (agent-repl--log ws "send: ws=%s prompt-origin=%s force-metaprompt=%s from-buf=%s" ws prompt-origin force-metaprompt (null prompt))
    (let* ((from-buf  (null prompt))
           (input-buf (agent-repl--ws-get ws :input-buffer))
           (raw       (or prompt (agent-repl--read-input-buffer ws)))
           ;; Empty-string is truthy in Elisp -- guard explicitly so RET on
           ;; an empty input buffer doesn't dispatch a metaprompt-only send.
           (raw-empty (or (null raw) (string-empty-p (string-trim raw)))))
      (if raw-empty
          (agent-repl--log ws "send: empty input raw-present=%s raw-len=%d -- nothing to send"
                            (not (null raw)) (if raw (length raw) 0))
        (let ((input (agent-repl--prepare-input ws raw force-metaprompt)))
          (agent-repl--do-send ws input raw prompt-origin on-settle)
          (agent-repl--commit-input-buffer ws input-buf raw from-buf))))))

(defun agent-repl-send-and-hide ()
  "Send input to Claude and hide both panels."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "send-and-hide")
  (agent-repl--send "PROMPT_ORIGIN_USER_SENT_AND_HIDE")
  (agent-repl--on-close))

(defun agent-repl-send-with-metaprompt ()
  "Send input with the metaprompt read-directive prepended.
The deliberate on-demand re-read: an ordinary send carries no directive,
because the metaprompt is already the session's system prompt."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "send-with-metaprompt")
  (agent-repl--send "PROMPT_ORIGIN_USER_SENT_WITH_METAPROMPT" nil nil t))

(defun agent-repl--fire-metaprompt-read (ws)
  "Send the metaprompt read-directive to WS as a standalone message.
The programmatic half of the on-demand re-read, for callers that want
the agent to go re-read the file without any user prompt riding along.
NOTHING CALLS THIS AUTOMATICALLY: the metaprompt is WS's system prompt,
so it survives `/clear', `/compact', and resume with no re-establishing.

No-op unless the on-demand re-read is enabled — `agent-repl-skip-permissions'
and `agent-repl-command-prefix', the same gate
`agent-repl--should-prepend-metaprompt-p' applies — so a user who has
turned it off never has a directive sent behind their back.

The directive (`agent-repl--command-prefix-for', pointing at WS's own
worktree copy of the metaprompt) is meta-wrapped
\(`agent-repl--meta-wrap') so the gui strips it to an empty user turn
and draws no bubble, exactly as the webapp's auto-continue nudge does.
RAW is empty: no user text sits behind a harness re-read, and an empty
RAW makes `agent-repl--gui-send-turn' skip the prompt summary and match
no posthook."
  (if (not (and agent-repl-skip-permissions agent-repl-command-prefix))
      (agent-repl--log ws "fire-metaprompt-read: SKIP ws=%s (on-demand re-read disabled)" ws)
    (agent-repl--log ws "fire-metaprompt-read: ws=%s" ws)
    (agent-repl--do-send ws (agent-repl--meta-wrap (agent-repl--command-prefix-for ws)) ""
                         "PROMPT_ORIGIN_METAPROMPT_READ")))

(defun agent-repl--append-to-input-buffer (text)
  "Append TEXT to the end of the current workspace's input buffer."
  (let* ((ws (agent-repl--ws-current-name))
         (buf (agent-repl--ws-get ws :input-buffer)))
    (agent-repl--log ws "append-to-input-buffer: len=%d input-buffer-present=%s" (length text) (not (null buf)))
    (if buf
        (with-current-buffer buf
          (goto-char (point-max))
          (insert text))
      (agent-repl--warn ws "no input buffer for current workspace — text not appended")
      (agent-repl--log ws "append-to-input-buffer: no input buffer, text discarded"))))

(defun agent-repl-send-with-postfix ()
  "Append `agent-repl-send-postfix' to the input buffer, then send."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "send-with-postfix")
  (agent-repl--append-to-input-buffer agent-repl-send-postfix)
  (agent-repl--send "PROMPT_ORIGIN_USER_SENT_WITH_POSTFIX"))

(defun agent-repl--prepend-to-input-buffer (text)
  "Prepend TEXT to the start of the current workspace's input buffer."
  (let* ((ws (agent-repl--ws-current-name))
         (buf (agent-repl--ws-get ws :input-buffer)))
    (agent-repl--log ws "prepend-to-input-buffer: len=%d input-buffer-present=%s" (length text) (not (null buf)))
    (if buf
        (with-current-buffer buf
          (goto-char (point-min))
          (insert text))
      (agent-repl--warn ws "no input buffer for current workspace — text not prepended")
      (agent-repl--log ws "prepend-to-input-buffer: no input buffer, text discarded"))))

(defun agent-repl-send-with-prefix ()
  "Prepend `agent-repl-send-prefix' to the input buffer, then send."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "send-with-prefix")
  (agent-repl--prepend-to-input-buffer agent-repl-send-prefix)
  (agent-repl--send "PROMPT_ORIGIN_USER_SENT_WITH_PREFIX"))

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
