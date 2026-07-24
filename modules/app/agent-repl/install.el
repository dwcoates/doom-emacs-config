;;; install.el --- Emacs wrapper around .claude/install.sh -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive entry points for installing / uninstalling / reinstalling
;; the managed host-level SKILL symlinks used by this module.  The
;; canonical implementation lives in `.claude/install.sh' at the
;; Doom-config root; this file shells out to it and surfaces output in a
;; buffer.
;;
;; Emacs manages NO Claude Code HARNESS hooks any more (S8/S9 sentinel
;; endgame): render-state, permission UX, session death, and account
;; identity are all driven by daemon-pushed `frontend.v1' state, so there
;; is nothing for a Claude hook to feed.  What remains here is the skill
;; symlink provisioning + doctor, plus the settings-writer primitives
;; (`agent-repl--register-hooks-in-settings' and friends) that the codex
;; backend reuses for its own `~/.codex/hooks.json'.
;;
;; Host-only: no-ops when running inside the agent sandbox.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)

(defcustom agent-repl-sandbox-dockerenv-path "/.dockerenv"
  "Path to the Docker environment sentinel file for sandbox detection."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-sandbox-env-var "DOOM_SANDBOX"
  "Environment variable name that signals sandbox mode."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-sandbox-env-value "1"
  "Expected value of `agent-repl-sandbox-env-var' to indicate sandbox mode."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-install-shell "bash"
  "Shell interpreter used to invoke the install script."
  :type 'string
  :group 'agent-repl)

;;;; ---- Constants --------------------------------------------------------

;; Emacs manages NO Claude Code hooks after the S8/S9 sentinel endgame, so
;; there is no `agent-repl--managed-hooks' / `agent-repl--managed-hook-matchers'
;; any more: the two permission hooks (Notification / PermissionRequest)
;; that were the last survivors are deleted, and the permission UX is driven
;; by the pushed `PermissionItem' + `:permission' WorkspaceState instead
;; (see `permission.el').  The settings-writer primitives below are retained
;; ONLY because the codex backend reuses them for `~/.codex/hooks.json'.

(defconst agent-repl--install-script
  (let ((module-dir (file-name-directory (or load-file-name
                                              (buffer-file-name)))))
    (expand-file-name "../../../.claude/install.sh" module-dir))
  "Absolute path to the bash install script.
Resolved relative to this file so the wrapper keeps working regardless
of where the Doom config tree is mounted.")

(defconst agent-repl--install-output-buffer "*agent-repl-install*"
  "Buffer name used to surface install-script output to the user.")

(defconst agent-repl--managed-skills
  '("workspace"
    "build-skill")
  "Bare names for managed host-level skill symlinks.
Subset of cached skills (see `CACHED_SKILLS' in
`modules/app/agent-repl/skills-cache/manifest.sh') whose impl source
lives under `agent-repl-skills-src-dir', so the doctor's
`points-elsewhere' check has the right expected target.

The former `workspace-merge', `workspace-status', `workspace-update',
and `generate-workspace' skills were folded into the single `workspace'
skill and are no longer managed.")

(defconst agent-repl--skills-dest-dir "~/.claude/skills/"
  "Destination directory where managed skill symlinks are created.")

(defcustom agent-repl-skills-src-dir
  "~/workspace/ChessCom/explanation-engine/.claude/skills/"
  "Source directory for managed skill targets.
Must match the `SKILLS_SRC' default in `.claude/install.sh' (or the
`AGENT_REPL_SKILLS_SRC' env var override).  No hardcoded user path:
`~' expands per the running Emacs's HOME."
  :type 'directory
  :group 'agent-repl)

(defconst agent-repl--managed-local-skills
  '("runtime-eval-code"
    "workspace-close"
    "emit-workspace-commands.sh")
  "Bare names for repo-local managed skills.
Sourced from `agent-repl-local-skills-src-dir' (this repo's
`modules/app/agent-repl/skills/').  Must match the `LOCAL_SKILLS'
array in `.claude/install.sh'.

debug-logs is deliberately absent: it is PROJECT-scoped via the
checked-in `<repo>/.claude/skills/debug-logs' symlink, not a
user-level `~/.claude/skills' link.

The former `workspace-open' skill is deliberately absent: it was
superseded by the `create-or-update-workspace' skill's `open' verb
(which already claims `/workspace-open' as its legacy alias), so it is
no longer managed here.")

(defcustom agent-repl-local-skills-src-dir
  (when load-file-name
    (file-name-as-directory
     (expand-file-name "skills" (file-name-directory load-file-name))))
  "Source directory for repo-local managed skill targets.
Defaults to `modules/app/agent-repl/skills/' alongside this file.
Must match the `LOCAL_SKILLS_SRC' value in `.claude/install.sh'."
  :type '(choice (const :tag "Unset" nil) directory)
  :group 'agent-repl)

;;;; ---- Sandbox detection ------------------------------------------------

(defun agent-repl--in-sandbox-p ()
  "Return non-nil when Emacs is running inside the agent sandbox.
Mirrors the detection rule in `install.sh' so the Emacs wrappers no-op
under the same conditions: a `/.dockerenv' file exists or the
`DOOM_SANDBOX' environment variable is set to `1'."
  (or (file-exists-p agent-repl-sandbox-dockerenv-path)
      (equal (getenv agent-repl-sandbox-env-var) agent-repl-sandbox-env-value)))

;;;; ---- Settings-writer primitive (codex-shared) -------------------------

(defun agent-repl--event-has-command-p (hooks event cmd)
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

;;;; ---- Running the bash script ------------------------------------------

(defun agent-repl--run-install-script (action)
  "Invoke `install.sh' with ACTION, capturing output.
Returns a list (EXIT-CODE OUTPUT-STRING).  Signals an error when the
script cannot be located."
  (unless (file-exists-p agent-repl--install-script)
    (error "agent-repl install script not found: %s"
           agent-repl--install-script))
  (with-temp-buffer
    (let ((exit-code (call-process agent-repl-install-shell nil t nil
                                   agent-repl--install-script action)))
      (list exit-code (buffer-string)))))

(defun agent-repl--surface-install-output (output)
  "Place OUTPUT in `agent-repl--install-output-buffer' and return the buffer."
  (with-current-buffer (get-buffer-create agent-repl--install-output-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))
    (current-buffer)))

(defun agent-repl--run-install-action (action &optional quiet)
  "Run install script ACTION (install / uninstall / reinstall).
No-op in sandbox.  On success, messages the user with a pointer to the
output buffer.  On failure, surfaces the output buffer and signals an
error.

When QUIET is non-nil (the auto-install-on-load path driven by
`agent-repl--maybe-install-hooks'), a failure routes the script output
to the agent-repl log via `agent-repl--log' INSTEAD of popping a window
with `display-buffer' — every `SPC j R' reload would otherwise drop the
`*agent-repl-install*' window into the frame whenever `doctor.el' still
reports an issue.  The output buffer is still populated for later
inspection, and the error is still signaled so the caller's surfacing is
preserved.  Interactive callers leave QUIET nil so a failure pops the
output window as before."
  (if (agent-repl--in-sandbox-p)
      (agent-repl--info nil "Sandbox detected; skipping hooks %s." action)
    (pcase-let ((`(,code ,output)
                 (agent-repl--run-install-script action)))
      (agent-repl--surface-install-output output)
      (if (= code 0)
          (agent-repl--info nil "hooks %s succeeded (see %s)."
                            action agent-repl--install-output-buffer)
        (if quiet
            (agent-repl--log nil
                              "hooks %s failed (exit %d); output:\n%s"
                              action code output)
          (display-buffer agent-repl--install-output-buffer))
        (error "[agent-repl] hooks %s failed (exit %d); see %s"
               action code agent-repl--install-output-buffer)))))

;;;; ---- Interactive entry points -----------------------------------------

;;;###autoload
(defun agent-repl-install-hooks ()
  "Install the managed skill symlinks (and the git pre-commit hook).
Runs `.claude/install.sh install', which links the managed skills into
`~/.claude/skills/' and installs the repo's ERT/boundary pre-commit git
hook.  Idempotent.  (Emacs manages no Claude Code harness hooks any more —
the command name is retained for muscle memory.)"
  (interactive)
  (agent-repl--run-install-action "install"))

;;;###autoload
(defun agent-repl-uninstall-hooks ()
  "Uninstall the managed skill symlinks (and the git pre-commit hook).
Runs `.claude/install.sh uninstall', removing the managed skill symlinks
from `~/.claude/skills/' and the repo's managed pre-commit git hook."
  (interactive)
  (agent-repl--run-install-action "uninstall"))

;;;###autoload
(defun agent-repl-reinstall-hooks ()
  "Reinstall the managed skill symlinks: uninstall then install."
  (interactive)
  (agent-repl--run-install-action "reinstall"))

;;;; ---- Auto-install on load ---------------------------------------------

(defcustom agent-repl-auto-install-hooks t
  "When non-nil, install managed skill symlinks on Emacs startup if missing.
The install script is idempotent, but unconditional runs are noisy, so the
auto-install short-circuits when `agent-repl--doctor-issues' reports no
problems.  No-op inside the sandbox."
  :type 'boolean
  :group 'agent-repl)

(defun agent-repl--maybe-install-hooks ()
  "Run the install action only when a managed skill symlink is off.
Guarded by `agent-repl--doctor-issues' so a healthy load is a pure
symlink stat (no bash).  No-op in sandbox, in a `noninteractive' (batch)
session, or when `agent-repl-auto-install-hooks' is nil.  Called inline
from this file's load.

Dispatches through `agent-repl--run-install-action' with QUIET set so a
failed install (common when `doctor.el' still flags a stale skill
symlink) routes its output to the agent-repl log rather than popping the
`*agent-repl-install*' window on every `SPC j R' reload.  A failure is
still surfaced — both as the quiet log line from the action and as the
caught-error log line here.

The `noninteractive' guard keeps batch invocations (the ERT test
suite, CI, ad-hoc scripts) from spawning bash and `display-buffer'ing the
install-output buffer into the session's frame — none of which is
meaningful without an interactive startup, and the stray window
corrupts window-layout assertions in the test suite."
  (when (and agent-repl-auto-install-hooks
             (not noninteractive)
             (not (agent-repl--in-sandbox-p))
             (agent-repl--doctor-issues))
    (condition-case err
        (agent-repl--run-install-action "install" t)
      (error
       (agent-repl--log nil "auto-install failed: %S" err)))))

;; The actual call happens at the bottom of this file, after
;; `agent-repl--doctor-issues' and its helpers are defined.

;;;; ---- Settings-writer primitives (codex-shared) ------------------------
;;
;; Emacs no longer provisions Claude Code hooks into any CLAUDE_CONFIG_DIR
;; (the S8/S9 sentinel endgame removed every managed Claude hook).  What
;; survives here is the generic settings.json hook-array writer, retained
;; ONLY because the codex backend reuses it to register its own hooks into
;; `~/.codex/hooks.json' — codex's hooks block nests identically to Claude
;; Code's settings.json, so the two share one writer.

(defun agent-repl--managed-hook-entry (cmd matcher)
  "Build a settings.json hook-array entry registering CMD, with optional MATCHER.
Shape mirrors install.sh: `((matcher . MATCHER) (hooks . (((type . \"command\")
\(command . CMD)))))', with the `matcher' pair omitted when MATCHER is nil."
  (append (and matcher (list (cons 'matcher matcher)))
          (list (cons 'hooks
                      (list (list (cons 'type "command")
                                  (cons 'command cmd)))))))

(defun agent-repl--alist-append (alist key value)
  "Return ALIST with VALUE appended to the list under KEY.
When KEY is present, VALUE is appended to its existing list in place and
the original ALIST head is returned.  When KEY is absent, a new
\(KEY . (VALUE)) cell is appended and the (possibly new) head is returned."
  (let ((cell (assq key alist)))
    (if cell
        (progn (setcdr cell (append (cdr cell) (list value))) alist)
      (append alist (list (cons key (list value)))))))

(defun agent-repl--alist-put (alist key value)
  "Return ALIST with KEY mapped to VALUE.
Updates the existing cell in place when KEY is present, otherwise appends
a new cell and returns the (possibly new) head."
  (let ((cell (assq key alist)))
    (if cell
        (progn (setcdr cell value) alist)
      (append alist (list (cons key value))))))

(defun agent-repl--read-settings-alist (path)
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

(defun agent-repl--register-hooks-in-settings (settings-file hooks-alist &optional matchers-alist)
  "Ensure every hook in HOOKS-ALIST is registered in SETTINGS-FILE.
Reads SETTINGS-FILE (or starts from an empty object when absent), appends
any hook whose command path is not already present under its event, and
writes the result back (pretty-printed).  Idempotent: foreign entries and
already-present entries are preserved, so a no-change run rewrites
nothing.  Creates the parent directory when needed.  Returns non-nil when
a write occurred, nil when already complete.  Signals on malformed
existing JSON (never silently resets).

HOOKS-ALIST is the (EVENT-SYMBOL . COMMAND-PATH) list to register;
MATCHERS-ALIST is the optional (EVENT-SYMBOL . MATCHER) list (an event
with no entry gets no matcher).  The codex backend is the sole caller —
its `~/.codex/hooks.json' nests its hooks block identically to Claude
Code's settings.json, so it reuses this writer with its own alists.
Emacs itself no longer manages any Claude Code hooks."
  (let* ((path (expand-file-name settings-file))
         (json (agent-repl--read-settings-alist path))
         (hooks (cdr (assq 'hooks json)))
         (changed nil))
    (dolist (pair hooks-alist)
      (let ((event (car pair))
            (cmd (cdr pair)))
        (unless (agent-repl--event-has-command-p hooks event cmd)
          (let ((matcher (cdr (assq event matchers-alist))))
            (setq hooks (agent-repl--alist-append
                         hooks event
                         (agent-repl--managed-hook-entry cmd matcher)))
            (setq changed t)))))
    (when changed
      (setq json (agent-repl--alist-put json 'hooks hooks))
      (make-directory (file-name-directory path) t)
      (with-temp-file path
        (insert (json-encode json))
        (json-pretty-print-buffer))
      (agent-repl--log nil "register-hooks-in-settings: wrote %s" path))
    changed))

;;;; ---- Doctor support ---------------------------------------------------

(defun agent-repl--push-issue (issues-cell level msg)
  "Prepend (LEVEL . MSG) to the list held in ISSUES-CELL (a single-cons list)."
  (setcar issues-cell (cons (cons level msg) (car issues-cell))))

(defun agent-repl--skill-dest-path (name)
  "Absolute install destination for skill NAME."
  (expand-file-name name (expand-file-name agent-repl--skills-dest-dir)))

(defun agent-repl--skill-src-path (name &optional src-dir)
  "Absolute canonical source target for skill NAME.
SRC-DIR defaults to `agent-repl-skills-src-dir' (external source);
pass `agent-repl-local-skills-src-dir' for repo-local skills."
  (expand-file-name name (expand-file-name (or src-dir agent-repl-skills-src-dir))))

(defun agent-repl--skill-link-ok-p (name &optional src-dir)
  "Return non-nil when the host-level symlink for skill NAME is correct.
\"Correct\" means DEST is a symlink whose immediate target (via
`file-symlink-p', not dereferenced) resolves to the expected source
path.  This ensures we only flag *ours* as healthy — foreign files at
the same path are treated as problems.  SRC-DIR selects which source
directory the expected target is computed from."
  (let* ((dest (agent-repl--skill-dest-path name))
         (target (file-symlink-p dest))
         (expected (agent-repl--skill-src-path name src-dir)))
    (and target
         (equal (expand-file-name target (file-name-directory dest))
                expected))))

(defun agent-repl--all-managed-skill-names ()
  "Return a list of all managed skill names (cached + local)."
  (append agent-repl--managed-skills agent-repl--managed-local-skills))

(defun agent-repl--check-skill-links (issues-cell)
  "Populate ISSUES-CELL with problems for managed skill symlinks.
Covers both external skills (`agent-repl--managed-skills') and
repo-local skills (`agent-repl--managed-local-skills')."
  (dolist (pair (list (cons agent-repl--managed-skills nil)
                      (cons agent-repl--managed-local-skills
                            agent-repl-local-skills-src-dir)))
    (let ((names   (car pair))
          (src-dir (cdr pair)))
      (dolist (name names)
        (let ((dest (agent-repl--skill-dest-path name)))
          (cond
           ((not (file-exists-p dest))
            (agent-repl--push-issue
             issues-cell 'warn
             (format "Skill symlink missing: %s — run M-x agent-repl-install-hooks"
                     dest)))
           ((not (agent-repl--skill-link-ok-p name src-dir))
            (agent-repl--push-issue
             issues-cell 'warn
             (format "Skill symlink points elsewhere: %s — run M-x agent-repl-reinstall-hooks"
                     dest)))))))))

(defun agent-repl--check-unmanaged-broken-links (issues-cell)
  "Populate ISSUES-CELL with warnings for broken symlinks we don't manage.
Scans `agent-repl--skills-dest-dir' for symlinks that are both broken
and not in our managed set — likely stale leftovers from old worktrees."
  (let ((skills-dir (expand-file-name agent-repl--skills-dest-dir))
        (managed (agent-repl--all-managed-skill-names)))
    (when (file-directory-p skills-dir)
      (dolist (entry (directory-files skills-dir t))
        (let ((name (file-name-nondirectory entry)))
          (when (and (file-symlink-p entry)
                     (not (file-exists-p entry))
                     (not (member name managed)))
            (agent-repl--push-issue
             issues-cell 'warn
             (format "Unmanaged broken symlink: %s -> %s — consider removing"
                     entry (file-symlink-p entry)))))))))

(defun agent-repl--doctor-issues ()
  "Return a list of (LEVEL . MESSAGE) describing skill-install problems.
LEVEL is `error' or `warn'.  Empty list means every managed skill symlink
is present and points at its checked-in source.  No-ops (returns nil) when
running inside the sandbox — the host is where installation happens, not
the container.

Emacs manages no Claude Code hooks any more (S8/S9 sentinel endgame), so
this no longer checks `~/.claude/settings.json' registrations or hook
scripts; only the managed skill symlinks (plus stale unmanaged broken
links) are inspected."
  (if (agent-repl--in-sandbox-p)
      nil
    (let ((issues (list nil)))
      (agent-repl--check-skill-links issues)
      (agent-repl--check-unmanaged-broken-links issues)
      (nreverse (car issues)))))

;; Run inline at load time so a missing managed skill symlink self-heals
;; on startup.  Guarded to no-op on healthy installs — see the function's
;; docstring for details.
(agent-repl--maybe-install-hooks)

(provide 'agent-repl-install)

;;; install.el ends here
