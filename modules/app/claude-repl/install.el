;;; install.el --- Emacs wrapper around .claude/install.sh -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive entry points for installing / uninstalling / reinstalling
;; the managed Claude Code hooks used by this module.  The canonical
;; implementation lives in `.claude/install.sh' at the Doom-config root;
;; this file shells out to it and surfaces output in a buffer.
;;
;; Also exposes `claude-repl--hooks-installed-p' as a predicate used by
;; `doctor.el' to report missing registrations.
;;
;; Host-only: no-ops when running inside the agent sandbox.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)

(defcustom claude-repl-sandbox-dockerenv-path "/.dockerenv"
  "Path to the Docker environment sentinel file for sandbox detection."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-sandbox-env-var "DOOM_SANDBOX"
  "Environment variable name that signals sandbox mode."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-sandbox-env-value "1"
  "Expected value of `claude-repl-sandbox-env-var' to indicate sandbox mode."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-install-shell "bash"
  "Shell interpreter used to invoke the install script."
  :type 'string
  :group 'claude-repl)

;;;; ---- Constants --------------------------------------------------------

(defconst claude-repl--managed-hooks
  '((Stop              . "~/.claude/hooks/stop-notify.sh")
    (StopFailure       . "~/.claude/hooks/stop-failure-notify.sh")
    (SubagentStart     . "~/.claude/hooks/subagent-start-notify.sh")
    (SubagentStop      . "~/.claude/hooks/subagent-stop-notify.sh")
    (UserPromptSubmit  . "~/.claude/hooks/prompt-submit-notify.sh")
    (SessionStart      . "~/.claude/hooks/session-start-notify.sh")
    (Notification      . "~/.claude/hooks/permission-notify.sh")
    (PermissionRequest . "~/.claude/hooks/permission-request-notify.sh"))
  "Alist (EVENT-SYMBOL . COMMAND-PATH) for hooks this module manages.
The COMMAND-PATH matches what `install.sh' writes into
`~/.claude/settings.json' — the literal `~/' is preserved
because Claude Code expands it at dispatch time.

`PermissionRequest' is the real-time permission-dialog signal: Claude
Code fires it the moment the permission UI appears, so the tab flips
to `:permission' WHILE Claude is waiting on the user rather than after.
The older `Notification' permission_prompt entry can lag the dialog
(it fires when Claude Code dispatches a NOTIFICATION about the prompt,
which is the 60s-idle nudge on \"needs your attention\"), so it's kept
as a fallback — the elisp callback's `:thinking' gate makes redundant
arrivals a no-op.")

(defconst claude-repl--managed-hook-matchers
  '((Notification . "permission_prompt"))
  "Alist (EVENT-SYMBOL . MATCHER) for managed hooks needing a matcher.
Mirrors the MATCHER field of the `HOOKS' array in `install.sh' so the
registrations `claude-repl--provision-config-dirs' writes into alt
CLAUDE_CONFIG_DIRs are byte-for-byte the same shape install.sh writes
into `~/.claude/settings.json'.  Only `Notification' carries one (its
`permission_prompt' matcher); every other managed hook is unmatched.")

(defconst claude-repl--install-script
  (let ((module-dir (file-name-directory (or load-file-name
                                              (buffer-file-name)))))
    (expand-file-name "../../../.claude/install.sh" module-dir))
  "Absolute path to the bash install script.
Resolved relative to this file so the wrapper keeps working regardless
of where the Doom config tree is mounted.")

(defconst claude-repl--hooks-source-dir
  (let ((module-dir (file-name-directory (or load-file-name
                                              (buffer-file-name)))))
    (file-name-as-directory (expand-file-name "hooks" module-dir)))
  "Absolute path to the checked-in managed hook scripts.")

(defconst claude-repl--install-output-buffer "*claude-repl-install*"
  "Buffer name used to surface install-script output to the user.")

(defconst claude-repl--settings-file "~/.claude/settings.json"
  "Path to the Claude Code settings file we read for installed-state checks.")

(defconst claude-repl--hooks-dest-dir "~/.claude/hooks/"
  "Destination directory where managed hook scripts are installed.")

(defconst claude-repl--managed-skills
  '("workspace"
    "build-skill")
  "Bare names for managed host-level skill symlinks.
Subset of cached skills (see `CACHED_SKILLS' in
`modules/app/claude-repl/skills-cache/manifest.sh') whose impl source
lives under `claude-repl-skills-src-dir', so the doctor's
`points-elsewhere' check has the right expected target.

The former `workspace-merge', `workspace-status', `workspace-update',
and `generate-workspace' skills were folded into the single `workspace'
skill and are no longer managed.")

(defconst claude-repl--skills-dest-dir "~/.claude/skills/"
  "Destination directory where managed skill symlinks are created.")

(defcustom claude-repl-skills-src-dir
  "~/workspace/ChessCom/explanation-engine/.claude/skills/"
  "Source directory for managed skill targets.
Must match the `SKILLS_SRC' default in `.claude/install.sh' (or the
`CLAUDE_REPL_SKILLS_SRC' env var override).  No hardcoded user path:
`~' expands per the running Emacs's HOME."
  :type 'directory
  :group 'claude-repl)

(defconst claude-repl--managed-local-skills
  '("debug-logs"
    "runtime-eval-code"
    "workspace-close"
    "workspace-open"
    "emit-workspace-commands.sh")
  "Bare names for repo-local managed skills.
Sourced from `claude-repl-local-skills-src-dir' (this repo's
`modules/app/claude-repl/skills/').  Must match the `LOCAL_SKILLS'
array in `.claude/install.sh'.")

(defcustom claude-repl-local-skills-src-dir
  (when load-file-name
    (file-name-as-directory
     (expand-file-name "skills" (file-name-directory load-file-name))))
  "Source directory for repo-local managed skill targets.
Defaults to `modules/app/claude-repl/skills/' alongside this file.
Must match the `LOCAL_SKILLS_SRC' value in `.claude/install.sh'."
  :type '(choice (const :tag "Unset" nil) directory)
  :group 'claude-repl)

;;;; ---- Sandbox detection ------------------------------------------------

(defun claude-repl--in-sandbox-p ()
  "Return non-nil when Emacs is running inside the agent sandbox.
Mirrors the detection rule in `install.sh' so the Emacs wrappers no-op
under the same conditions: a `/.dockerenv' file exists or the
`DOOM_SANDBOX' environment variable is set to `1'."
  (or (file-exists-p claude-repl-sandbox-dockerenv-path)
      (equal (getenv claude-repl-sandbox-env-var) claude-repl-sandbox-env-value)))

;;;; ---- Installed-state predicate ----------------------------------------

(defun claude-repl--settings-json ()
  "Return parsed `~/.claude/settings.json' or nil if absent/unreadable."
  (let ((path (expand-file-name claude-repl--settings-file)))
    (when (file-exists-p path)
      (condition-case err
          (json-read-file path)
        (error
         (message "[claude-repl] WARNING: failed to parse %s: %S" path err)
         nil)))))

(defun claude-repl--event-has-command-p (hooks event cmd)
  "Return non-nil when HOOKS alist has a CMD registered under EVENT.
HOOKS is the value of `.hooks' from `settings.json'.  EVENT is a
symbol (e.g. `Stop').  CMD is the literal command string we expect to
find in any entry's inner `.hooks[].command'."
  (let ((entries (cdr (assq event hooks))))
    (and entries
         (seq-some
          (lambda (entry)
            (let ((inner (cdr (assq 'hooks entry))))
              (seq-some (lambda (h) (equal (cdr (assq 'command h)) cmd))
                        inner)))
          entries))))

(defun claude-repl--hooks-installed-p ()
  "Return non-nil iff every managed hook is registered under its event.
Checks `~/.claude/settings.json' for an entry whose inner
`.hooks[].command' matches the canonical path for each member of
`claude-repl--managed-hooks'."
  (when-let* ((json (claude-repl--settings-json))
              (hooks (cdr (assq 'hooks json))))
    (cl-every (lambda (pair)
                (claude-repl--event-has-command-p hooks (car pair) (cdr pair)))
              claude-repl--managed-hooks)))

;;;; ---- Running the bash script ------------------------------------------

(defun claude-repl--run-install-script (action)
  "Invoke `install.sh' with ACTION, capturing output.
Returns a list (EXIT-CODE OUTPUT-STRING).  Signals an error when the
script cannot be located."
  (unless (file-exists-p claude-repl--install-script)
    (error "claude-repl install script not found: %s"
           claude-repl--install-script))
  (with-temp-buffer
    (let ((exit-code (call-process claude-repl-install-shell nil t nil
                                   claude-repl--install-script action)))
      (list exit-code (buffer-string)))))

(defun claude-repl--surface-install-output (output)
  "Place OUTPUT in `claude-repl--install-output-buffer' and return the buffer."
  (with-current-buffer (get-buffer-create claude-repl--install-output-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))
    (current-buffer)))

(defun claude-repl--run-install-action (action &optional quiet)
  "Run install script ACTION (install / uninstall / reinstall).
No-op in sandbox.  On success, messages the user with a pointer to the
output buffer.  On failure, surfaces the output buffer and signals an
error.

When QUIET is non-nil (the auto-install-on-load path driven by
`claude-repl--maybe-install-hooks'), a failure routes the script output
to the claude-repl log via `claude-repl--log' INSTEAD of popping a window
with `display-buffer' — every `SPC j R' reload would otherwise drop the
`*claude-repl-install*' window into the frame whenever `doctor.el' still
reports an issue.  The output buffer is still populated for later
inspection, and the error is still signaled so the caller's surfacing is
preserved.  Interactive callers leave QUIET nil so a failure pops the
output window as before."
  (if (claude-repl--in-sandbox-p)
      (message "[claude-repl] Sandbox detected; skipping hooks %s." action)
    (pcase-let ((`(,code ,output)
                 (claude-repl--run-install-script action)))
      (claude-repl--surface-install-output output)
      (if (= code 0)
          (message "[claude-repl] hooks %s succeeded (see %s)."
                   action claude-repl--install-output-buffer)
        (if quiet
            (claude-repl--log nil
                              "hooks %s failed (exit %d); output:\n%s"
                              action code output)
          (display-buffer claude-repl--install-output-buffer))
        (error "[claude-repl] hooks %s failed (exit %d); see %s"
               action code claude-repl--install-output-buffer)))))

;;;; ---- Interactive entry points -----------------------------------------

;;;###autoload
(defun claude-repl-install-hooks ()
  "Install managed Claude Code hooks into `~/.claude/'.
Copies the checked-in scripts from `modules/app/claude-repl/hooks/' to
`~/.claude/hooks/' and appends registrations in `~/.claude/settings.json'
under each event.  Idempotent: foreign entries are preserved."
  (interactive)
  (claude-repl--run-install-action "install"))

;;;###autoload
(defun claude-repl-uninstall-hooks ()
  "Uninstall managed Claude Code hooks from `~/.claude/'.
Removes the managed command paths from `~/.claude/settings.json' (leaving
foreign entries untouched) and deletes the managed scripts from
`~/.claude/hooks/'."
  (interactive)
  (claude-repl--run-install-action "uninstall"))

;;;###autoload
(defun claude-repl-reinstall-hooks ()
  "Reinstall managed Claude Code hooks: uninstall then install."
  (interactive)
  (claude-repl--run-install-action "reinstall"))

;;;; ---- Auto-install on load ---------------------------------------------

(defcustom claude-repl-auto-install-hooks t
  "When non-nil, install managed Claude Code hooks on Emacs startup if missing.
The install script is idempotent, but unconditional runs spam
`~/.claude/settings.json.bak.<ts>' backups on every start — so the
auto-install short-circuits when `claude-repl--doctor-issues' reports no
problems.  No-op inside the sandbox."
  :type 'boolean
  :group 'claude-repl)

(defun claude-repl--maybe-install-hooks ()
  "Run the install action only when registration or scripts are off.
Guarded by `claude-repl--doctor-issues' so a healthy load is a pure
JSON-parse (no bash, no backup file).  No-op in sandbox, in a
`noninteractive' (batch) session, or when `claude-repl-auto-install-hooks'
is nil.  Called inline from this file's load so hooks are registered
before later claude-repl sub-modules (sentinel, notifications, ...)
start relying on them.

Dispatches through `claude-repl--run-install-action' with QUIET set so a
failed install (common when `doctor.el' still flags a stale skill
symlink) routes its output to the claude-repl log rather than popping the
`*claude-repl-install*' window on every `SPC j R' reload.  A failure is
still surfaced — both as the quiet log line from the action and as the
caught-error log line here.

The `noninteractive' guard keeps batch invocations (the ERT test
suite, CI, ad-hoc scripts) from silently rewriting
`~/.claude/settings.json', spawning bash, and `display-buffer'ing the
install-output buffer into the session's frame — none of which is
meaningful without an interactive startup, and the stray window
corrupts window-layout assertions in the test suite."
  (when (and claude-repl-auto-install-hooks
             (not noninteractive)
             (not (claude-repl--in-sandbox-p))
             (claude-repl--doctor-issues))
    (condition-case err
        (claude-repl--run-install-action "install" t)
      (error
       (claude-repl--log nil "auto-install failed: %S" err)))))

;; The actual call happens at the bottom of this file, after
;; `claude-repl--doctor-issues' and its helpers are defined.

;;;; ---- Alt-account config-dir provisioning ------------------------------
;;
;; The per-account launch logic in session.el
;; (`claude-repl--compute-config-dir') can select a CLAUDE_CONFIG_DIR other
;; than the default ~/.claude — at minimum `claude-repl-multi-repo-config-dir'
;; (~/.claude-chesscom).  The readiness handshake depends on the managed
;; hooks (SessionStart et al.) being registered in whichever account's
;; settings.json launches the workspace, so each alt dir must carry the
;; SAME registration array install.sh writes into ~/.claude.
;;
;; The hook SCRIPTS stay in ~/.claude/hooks/: the registered command paths
;; are the literal "~/.claude/hooks/*.sh" (Claude Code expands `~' to HOME
;; regardless of CLAUDE_CONFIG_DIR), so every account funnels its
;; notifications through the same account-independent
;; ~/.claude/workspace-notifications/ dir the sentinel watches.  Only the
;; registration ARRAY is replicated per account, never the scripts nor the
;; notification dir.

;; Forward declarations: these defcustoms live in session.el, which loads
;; AFTER this file.  Declared here so byte-compilation doesn't warn about
;; free variables; they are always bound by the time the functions below
;; run (session.el load-time trigger or workspace launch).
(defvar claude-repl-multi-repo-config-dir)
(defvar claude-repl-default-config-dir)

(defun claude-repl--config-dirs-to-provision ()
  "Return the absolute alt CLAUDE_CONFIG_DIRs needing managed-hook registration.
Derived from the per-account defcustoms in session.el
\(`claude-repl-multi-repo-config-dir' and `claude-repl-default-config-dir').
The default ~/.claude is EXCLUDED — install.sh owns it, and it is the
account-independent notification funnel that must not be rewritten here.
Duplicates and the default dir are removed; order is stable."
  (let ((default-claude (file-name-as-directory (expand-file-name "~/.claude")))
        (dirs '()))
    (dolist (raw (list (and (boundp 'claude-repl-multi-repo-config-dir)
                            claude-repl-multi-repo-config-dir)
                       (and (boundp 'claude-repl-default-config-dir)
                            claude-repl-default-config-dir)))
      (when (and (stringp raw) (> (length raw) 0))
        (let ((abs (file-name-as-directory (expand-file-name raw))))
          (unless (or (equal abs default-claude) (member abs dirs))
            (push abs dirs)))))
    (nreverse dirs)))

(defun claude-repl--managed-hook-entry (cmd matcher)
  "Build a settings.json hook-array entry registering CMD, with optional MATCHER.
Shape mirrors install.sh: `((matcher . MATCHER) (hooks . (((type . \"command\")
\(command . CMD)))))', with the `matcher' pair omitted when MATCHER is nil."
  (append (and matcher (list (cons 'matcher matcher)))
          (list (cons 'hooks
                      (list (list (cons 'type "command")
                                  (cons 'command cmd)))))))

(defun claude-repl--alist-append (alist key value)
  "Return ALIST with VALUE appended to the list under KEY.
When KEY is present, VALUE is appended to its existing list in place and
the original ALIST head is returned.  When KEY is absent, a new
\(KEY . (VALUE)) cell is appended and the (possibly new) head is returned."
  (let ((cell (assq key alist)))
    (if cell
        (progn (setcdr cell (append (cdr cell) (list value))) alist)
      (append alist (list (cons key (list value)))))))

(defun claude-repl--alist-put (alist key value)
  "Return ALIST with KEY mapped to VALUE.
Updates the existing cell in place when KEY is present, otherwise appends
a new cell and returns the (possibly new) head."
  (let ((cell (assq key alist)))
    (if cell
        (progn (setcdr cell value) alist)
      (append alist (list (cons key value))))))

(defun claude-repl--read-settings-alist (path)
  "Parse the settings.json at PATH into an alist, or nil when PATH is absent.
Arrays are read as lists and objects as alists so the result is easy to
mutate and re-encode.  Signals (never swallows) when PATH exists but is
not valid JSON — a malformed settings file is a loud failure, not a
silent reset."
  (when (file-exists-p path)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (json-read-file path))))

(defun claude-repl--register-hooks-in-settings (settings-file)
  "Ensure every managed hook is registered in SETTINGS-FILE.
Reads SETTINGS-FILE (or starts from an empty object when absent), appends
any managed hook whose command path is not already present under its
event, and writes the result back (pretty-printed).  Idempotent: foreign
entries and already-present managed entries are preserved, so a no-change
run rewrites nothing.  Creates the parent directory when needed.  Returns
non-nil when a write occurred, nil when already complete.  Signals on
malformed existing JSON (never silently resets)."
  (let* ((path (expand-file-name settings-file))
         (json (claude-repl--read-settings-alist path))
         (hooks (cdr (assq 'hooks json)))
         (changed nil))
    (dolist (pair claude-repl--managed-hooks)
      (let ((event (car pair))
            (cmd (cdr pair)))
        (unless (claude-repl--event-has-command-p hooks event cmd)
          (let ((matcher (cdr (assq event claude-repl--managed-hook-matchers))))
            (setq hooks (claude-repl--alist-append
                         hooks event
                         (claude-repl--managed-hook-entry cmd matcher)))
            (setq changed t)))))
    (when changed
      (setq json (claude-repl--alist-put json 'hooks hooks))
      (make-directory (file-name-directory path) t)
      (with-temp-file path
        (insert (json-encode json))
        (json-pretty-print-buffer))
      (claude-repl--log nil "register-hooks-in-settings: wrote %s" path))
    changed))

(defun claude-repl--provision-config-dirs ()
  "Register the managed hooks into every alt CLAUDE_CONFIG_DIR's settings.json.
Iterates `claude-repl--config-dirs-to-provision' and registers the
managed hooks into `<dir>/settings.json' for each.  The default ~/.claude
is intentionally NOT touched here (install.sh owns it).  Signals loudly
on the first dir that fails (malformed JSON, unwritable path) rather than
swallowing — callers wanting startup robustness wrap this in their own
`condition-case' + log (see session.el's load-time trigger)."
  (dolist (dir (claude-repl--config-dirs-to-provision))
    (claude-repl--register-hooks-in-settings
     (expand-file-name "settings.json" dir))))

;;;; ---- Doctor support ---------------------------------------------------

(defconst claude-repl--hook-severity
  '((Stop              . error)
    (SessionStart      . error)
    (UserPromptSubmit  . warn)
    (Notification      . warn)
    ;; PermissionRequest: when missing, the `:permission' tab transition
    ;; falls back to the `Notification' permission_prompt signal, which
    ;; can lag the dialog appearance by up to the 60s-idle nudge window.
    ;; Tabs still eventually flip yellow — just not at the moment Claude
    ;; starts waiting.  Warn rather than error so users on older
    ;; Claude Code versions (where PermissionRequest may not exist) are
    ;; not blocked.
    (PermissionRequest . warn)
    ;; New (2026-05) Stop-coordination hooks.  Treated as warn rather than
    ;; error: their absence does not break the core REPL loop, only the
    ;; correctness of the `:done' transition gating (Stop will resolve
    ;; immediately) and the `:stop-failed' state (turns ending on API
    ;; errors will appear stuck in `:thinking').  Promote to `error' if
    ;; we ever rely on them as load-bearing.
    (StopFailure       . warn)
    (SubagentStart     . warn)
    (SubagentStop      . warn))
  "Severity of a missing managed hook.
`error' means the module is non-functional without it; `warn' means a
degraded UX but still usable.  Script-file problems for any hook are
treated as `error' (a registered hook pointing at a missing script will
fail noisily at dispatch time).")

(defun claude-repl--managed-script-name (cmd)
  "Return the bare filename for a managed command path CMD.
CMD is of the form \"~/.claude/hooks/<name>.sh\"."
  (file-name-nondirectory cmd))

(defun claude-repl--installed-script-path (cmd)
  "Absolute path where CMD's managed script should live after install."
  (expand-file-name (claude-repl--managed-script-name cmd)
                    (expand-file-name claude-repl--hooks-dest-dir)))

(defun claude-repl--source-script-path (cmd)
  "Absolute path of the checked-in source for CMD's managed script."
  (expand-file-name (claude-repl--managed-script-name cmd)
                    claude-repl--hooks-source-dir))

(defun claude-repl--file-contents (path)
  "Return PATH's contents as a string, or nil if unreadable."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (buffer-string))))

(defun claude-repl--script-drift-p (cmd)
  "Return non-nil when the installed managed script for CMD differs from source."
  (let ((installed (claude-repl--file-contents
                    (claude-repl--installed-script-path cmd)))
        (source (claude-repl--file-contents
                 (claude-repl--source-script-path cmd))))
    (and installed source (not (equal installed source)))))

(defun claude-repl--push-issue (issues-cell level msg)
  "Prepend (LEVEL . MSG) to the list held in ISSUES-CELL (a single-cons list)."
  (setcar issues-cell (cons (cons level msg) (car issues-cell))))

(defun claude-repl--check-registration (hooks issues-cell)
  "Populate ISSUES-CELL with any missing registrations per `claude-repl--managed-hooks'."
  (dolist (pair claude-repl--managed-hooks)
    (let* ((event (car pair))
           (cmd (cdr pair)))
      (unless (claude-repl--event-has-command-p hooks event cmd)
        (claude-repl--push-issue
         issues-cell
         (let ((sev (cdr (assq event claude-repl--hook-severity))))
           (unless sev
             (message "[claude-repl] WARNING: no severity defined for hook event %S — defaulting to warn" event))
           (or sev 'warn))
         (format "%s hook not registered in %s — run M-x claude-repl-install-hooks"
                 event claude-repl--settings-file))))))

(defun claude-repl--check-script-files (issues-cell)
  "Populate ISSUES-CELL with problems at each managed script's install location."
  (dolist (pair claude-repl--managed-hooks)
    (let* ((cmd (cdr pair))
           (path (claude-repl--installed-script-path cmd)))
      (cond
       ((not (file-exists-p path))
        (claude-repl--push-issue
         issues-cell 'error
         (format "Managed script missing: %s — run M-x claude-repl-install-hooks"
                 path)))
       ((not (file-executable-p path))
        (claude-repl--push-issue
         issues-cell 'error
         (format "Managed script not executable: %s" path)))
       ((claude-repl--script-drift-p cmd)
        (claude-repl--push-issue
         issues-cell 'warn
         (format "Managed script drift: %s differs from checked-in source — run M-x claude-repl-reinstall-hooks"
                 path)))))))

(defun claude-repl--skill-dest-path (name)
  "Absolute install destination for skill NAME."
  (expand-file-name name (expand-file-name claude-repl--skills-dest-dir)))

(defun claude-repl--skill-src-path (name &optional src-dir)
  "Absolute canonical source target for skill NAME.
SRC-DIR defaults to `claude-repl-skills-src-dir' (external source);
pass `claude-repl-local-skills-src-dir' for repo-local skills."
  (expand-file-name name (expand-file-name (or src-dir claude-repl-skills-src-dir))))

(defun claude-repl--skill-link-ok-p (name &optional src-dir)
  "Return non-nil when the host-level symlink for skill NAME is correct.
\"Correct\" means DEST is a symlink whose immediate target (via
`file-symlink-p', not dereferenced) resolves to the expected source
path.  This ensures we only flag *ours* as healthy — foreign files at
the same path are treated as problems.  SRC-DIR selects which source
directory the expected target is computed from."
  (let* ((dest (claude-repl--skill-dest-path name))
         (target (file-symlink-p dest))
         (expected (claude-repl--skill-src-path name src-dir)))
    (and target
         (equal (expand-file-name target (file-name-directory dest))
                expected))))

(defun claude-repl--all-managed-skill-names ()
  "Return a list of all managed skill names (cached + local)."
  (append claude-repl--managed-skills claude-repl--managed-local-skills))

(defun claude-repl--check-skill-links (issues-cell)
  "Populate ISSUES-CELL with problems for managed skill symlinks.
Covers both external skills (`claude-repl--managed-skills') and
repo-local skills (`claude-repl--managed-local-skills')."
  (dolist (pair (list (cons claude-repl--managed-skills nil)
                      (cons claude-repl--managed-local-skills
                            claude-repl-local-skills-src-dir)))
    (let ((names   (car pair))
          (src-dir (cdr pair)))
      (dolist (name names)
        (let ((dest (claude-repl--skill-dest-path name)))
          (cond
           ((not (file-exists-p dest))
            (claude-repl--push-issue
             issues-cell 'warn
             (format "Skill symlink missing: %s — run M-x claude-repl-install-hooks"
                     dest)))
           ((not (claude-repl--skill-link-ok-p name src-dir))
            (claude-repl--push-issue
             issues-cell 'warn
             (format "Skill symlink points elsewhere: %s — run M-x claude-repl-reinstall-hooks"
                     dest)))))))))

(defun claude-repl--check-unmanaged-broken-links (issues-cell)
  "Populate ISSUES-CELL with warnings for broken symlinks we don't manage.
Scans `claude-repl--skills-dest-dir' for symlinks that are both broken
and not in our managed set — likely stale leftovers from old worktrees."
  (let ((skills-dir (expand-file-name claude-repl--skills-dest-dir))
        (managed (claude-repl--all-managed-skill-names)))
    (when (file-directory-p skills-dir)
      (dolist (entry (directory-files skills-dir t))
        (let ((name (file-name-nondirectory entry)))
          (when (and (file-symlink-p entry)
                     (not (file-exists-p entry))
                     (not (member name managed)))
            (claude-repl--push-issue
             issues-cell 'warn
             (format "Unmanaged broken symlink: %s -> %s — consider removing"
                     entry (file-symlink-p entry)))))))))

(defun claude-repl--doctor-issues ()
  "Return a list of (LEVEL . MESSAGE) describing hook-install problems.
LEVEL is `error' or `warn'.  Empty list means all managed hooks are
registered and on-disk.  No-ops (returns nil) when running inside the
sandbox — the host is where installation happens, not the container.
When settings.json is missing or unreadable, a single top-level error is
returned and the per-hook checks are skipped."
  (if (claude-repl--in-sandbox-p)
      nil
    (let* ((issues (list nil))
           (json (claude-repl--settings-json)))
      (if (not json)
          (claude-repl--push-issue
           issues 'error
           (format "%s is missing or unreadable — run M-x claude-repl-install-hooks"
                   (expand-file-name claude-repl--settings-file)))
        (claude-repl--check-registration (cdr (assq 'hooks json)) issues)
        (claude-repl--check-script-files issues))
      (claude-repl--check-skill-links issues)
      (claude-repl--check-unmanaged-broken-links issues)
      (nreverse (car issues)))))

;; Run inline at load time so hooks are registered before later
;; claude-repl sub-modules (sentinel, notifications, ...) start relying
;; on them.  Guarded to no-op on healthy installs — see the function's
;; docstring for details.
(claude-repl--maybe-install-hooks)

(provide 'claude-repl-install)

;;; install.el ends here
