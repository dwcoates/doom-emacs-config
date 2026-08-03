;;; install.el --- Emacs wrapper around .claude/install.sh -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive entry points for installing / uninstalling / reinstalling
;; the managed host-level SKILL symlinks used by this module.  The
;; canonical implementation lives in `.claude/install.sh' at the
;; Doom-config root; this file shells out to it and surfaces output in a
;; buffer.
;;
;; Emacs manages NO agent-harness hooks any more, for any backend (S8/S9
;; sentinel endgame + the D-phase census): render-state, permission UX,
;; session death, and account identity are all driven by daemon-pushed
;; `frontend.v1' state, so there is nothing for a hook to feed.  The
;; settings.json hook-array writer that survived for the codex backend
;; went with codex's own hook plane.  What remains here is the skill
;; symlink provisioning + doctor.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)

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
  '("create-or-update-workspace"
    "create-or-update-skill")
  "Bare names for managed host-level skill symlinks.
Subset of cached skills (see `CACHED_SKILLS' in
`modules/app/agent-repl/skills-cache/manifest.sh') whose impl source
lives under `agent-repl-skills-src-dir', so the doctor's
`points-elsewhere' check has the right expected target.

The former `workspace-merge', `workspace-status', `workspace-update',
and `generate-workspace' skills were folded into the single workspace
skill and are no longer managed.  That skill and `build-skill' were then
renamed upstream to `create-or-update-workspace' and
`create-or-update-skill'; the old names no longer exist on the host, so
managing them would make the doctor demand a dangling symlink.")

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

debug-emacs-agent-repl is deliberately absent: it is PROJECT-scoped
via the checked-in `<repo>/.claude/skills/debug-emacs-agent-repl'
symlink, not a user-level `~/.claude/skills' link.

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

;;;; ---- Running the bash script ------------------------------------------

(defun agent-repl--run-install-script (action)
  "Invoke `install.sh' with ACTION, capturing output.
Returns a list (EXIT-CODE OUTPUT-STRING).  Signals an error when the
script cannot be located."
  (unless (file-exists-p agent-repl--install-script)
    (agent-repl--log nil
                      "install-script unavailable action=%s script=%s shell=%s"
                      action agent-repl--install-script agent-repl-install-shell)
    (error "agent-repl install script not found: %s"
           agent-repl--install-script))
  (agent-repl--log nil "install-script invoking action=%s script=%s shell=%s"
                    action agent-repl--install-script agent-repl-install-shell)
  (with-temp-buffer
    (let ((exit-code (call-process agent-repl-install-shell nil t nil ;; ALLOW-EXTERNAL-BOUNDARY
                                   agent-repl--install-script action)))
      (let ((output (buffer-string)))
        (agent-repl--log nil
                          "install-script completed action=%s exit=%s output-bytes=%d"
                          action exit-code (string-bytes output))
        (list exit-code output)))))

(defun agent-repl--surface-install-output (output)
  "Place OUTPUT in `agent-repl--install-output-buffer' and return the buffer."
  (agent-repl--log nil "install-output surfacing buffer=%s output-bytes=%d"
                    agent-repl--install-output-buffer (string-bytes output))
  (with-current-buffer (get-buffer-create agent-repl--install-output-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))
    (current-buffer)))

(defun agent-repl--run-install-action (action &optional quiet)
  "Run install script ACTION (install / uninstall / reinstall).
On success, messages the user with a pointer to the output buffer.  On
failure, surfaces the output buffer and signals an error.

When QUIET is non-nil (the auto-install-on-load path driven by
`agent-repl--maybe-install-hooks'), a failure routes the script output
to the agent-repl log via `agent-repl--log' INSTEAD of popping a window
with `display-buffer' — every `SPC j R' reload would otherwise drop the
`*agent-repl-install*' window into the frame whenever `doctor.el' still
reports an issue.  The output buffer is still populated for later
inspection, and the error is still signaled so the caller's surfacing is
preserved.  Interactive callers leave QUIET nil so a failure pops the
output window as before."
  (pcase-let ((`(,code ,output)
               (agent-repl--run-install-script action)))
    (agent-repl--surface-install-output output)
    (if (= code 0)
        (progn
          (agent-repl--log nil "hooks %s succeeded output:\n%s" action output)
          (agent-repl--info nil "hooks %s succeeded (see %s)."
                            action agent-repl--install-output-buffer))
      (if quiet
          (agent-repl--log nil
                            "hooks %s failed (exit %d); output:\n%s"
                            action code output)
        (agent-repl--log nil
                          "hooks %s failed interactively (exit %d); output:\n%s"
                          action code output)
        (display-buffer agent-repl--install-output-buffer))
      (error "[agent-repl] hooks %s failed (exit %d); see %s"
             action code agent-repl--install-output-buffer))))

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
problems."
  :type 'boolean
  :group 'agent-repl)

(defun agent-repl--maybe-install-hooks ()
  "Run the install action only when a managed skill symlink is off.
Guarded by `agent-repl--doctor-issues' so a healthy load is a pure
symlink stat (no bash).  No-op in a `noninteractive' (batch) session, or
when `agent-repl-auto-install-hooks' is nil.  Called inline from this
file's load.

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
  (cond
   ((not agent-repl-auto-install-hooks)
    (agent-repl--log nil "auto-install skipped: disabled"))
   (noninteractive
    (agent-repl--log nil "auto-install skipped: batch session"))
   (t
    (let ((issues (agent-repl--doctor-issues)))
      (if (null issues)
          (agent-repl--log nil "auto-install skipped: doctor reports healthy")
        (agent-repl--log nil "auto-install starting: doctor issue-count=%d"
                          (length issues))
        (condition-case err
            (agent-repl--run-install-action "install" t)
          (error
           (agent-repl--log nil "auto-install failed: %S" err))))))))

;; The actual call happens at the bottom of this file, after
;; `agent-repl--doctor-issues' and its helpers are defined.

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
        (let ((dest (agent-repl--skill-dest-path name))
              (expected (agent-repl--skill-src-path name src-dir)))
          (cond
           ((not (file-exists-p dest))
            (agent-repl--log nil
                              "doctor skill-link missing name=%s dest=%s expected=%s"
                              name dest expected)
            (agent-repl--push-issue
             issues-cell 'warn
             (format "Skill symlink missing: %s — run M-x agent-repl-install-hooks"
                     dest)))
           ((not (agent-repl--skill-link-ok-p name src-dir))
            (agent-repl--log nil
                              "doctor skill-link incorrect name=%s dest=%s actual=%s expected=%s"
                              name dest (file-symlink-p dest) expected)
            (agent-repl--push-issue
             issues-cell 'warn
             (format "Skill symlink points elsewhere: %s — run M-x agent-repl-reinstall-hooks"
                     dest)))
           (t
            (agent-repl--log nil
                              "doctor skill-link healthy name=%s dest=%s expected=%s"
                              name dest expected))))))))

(defun agent-repl--check-unmanaged-broken-links (issues-cell)
  "Populate ISSUES-CELL with warnings for broken symlinks we don't manage.
Scans `agent-repl--skills-dest-dir' for symlinks that are both broken
and not in our managed set — likely stale leftovers from old worktrees."
  (let ((skills-dir (expand-file-name agent-repl--skills-dest-dir))
        (managed (agent-repl--all-managed-skill-names)))
    (when (file-directory-p skills-dir)
      (agent-repl--log nil "doctor scanning unmanaged symlinks dir=%s" skills-dir)
      (dolist (entry (directory-files skills-dir t))
        (let ((name (file-name-nondirectory entry)))
          (agent-repl--log-verbose nil
                                    "doctor scanned skill entry=%s symlink=%s exists=%s managed=%s"
                                    entry (file-symlink-p entry) (file-exists-p entry)
                                    (member name managed))
          (when (and (file-symlink-p entry)
                     (not (file-exists-p entry))
                     (not (member name managed)))
            (agent-repl--log nil "doctor unmanaged broken symlink entry=%s target=%s"
                              entry (file-symlink-p entry))
            (agent-repl--push-issue
             issues-cell 'warn
             (format "Unmanaged broken symlink: %s -> %s — consider removing"
                     entry (file-symlink-p entry)))))))
    (unless (file-directory-p skills-dir)
      (agent-repl--log nil "doctor skipped unmanaged symlink scan: missing dir=%s"
                        skills-dir))))

(defun agent-repl--doctor-issues ()
  "Return a list of (LEVEL . MESSAGE) describing skill-install problems.
LEVEL is `error' or `warn'.  Empty list means every managed skill symlink
is present and points at its checked-in source.

Emacs manages no Claude Code hooks any more (S8/S9 sentinel endgame), so
this no longer checks `~/.claude/settings.json' registrations or hook
scripts; only the managed skill symlinks (plus stale unmanaged broken
links) are inspected."
  (let ((issues (list nil)))
    (agent-repl--log nil "doctor starting managed-skill-count=%d local-skill-count=%d"
                      (length agent-repl--managed-skills)
                      (length agent-repl--managed-local-skills))
    (agent-repl--check-skill-links issues)
    (agent-repl--check-unmanaged-broken-links issues)
    (setq issues (nreverse (car issues)))
    (agent-repl--log nil "doctor complete issue-count=%d issues=%S"
                      (length issues) issues)
    issues))

;;;; ---- Legacy hook purge (first boot after the sentinel endgame) --------

(defconst agent-repl--legacy-hook-scripts
  '("permission-notify.sh"
    "permission-request-notify.sh"
    "prompt-submit-notify.sh"
    "session-start-notify.sh"
    "stop-failure-notify.sh"
    "stop-notify.sh"
    "subagent-start-notify.sh"
    "subagent-stop-notify.sh")
  "Basenames of this module's PRE-CUTOVER Claude Code hook scripts.
Mirrors `LEGACY_HOOK_SCRIPTS' in `.claude/install.sh', which owns the
removal itself.  This list exists only so the purge can be SKIPPED
cheaply when there is nothing to purge — see
`agent-repl--legacy-hooks-present-p'.

Emacs registers no hooks any more (S8/S9 sentinel endgame): every one of
these fed a sentinel plane the daemon replaced.  Left registered they
still fire on every CLI event, writing sentinel files nothing reads.")

(defun agent-repl--legacy-hook-settings-files ()
  "Return every Claude Code settings.json on this host.
The default `~/.claude' plus any sibling `~/.claude-*' account dir, since
the pre-cutover installer provisioned hooks into each one.  No account
name is hardcoded.  Mirrors `_legacy_hook_settings_files' in install.sh."
  (let (out)
    (dolist (dir (cons (expand-file-name "~/.claude")
                       (file-expand-wildcards (expand-file-name "~/.claude-*"))))
      (let ((settings (expand-file-name "settings.json" dir)))
        (when (and (file-directory-p dir) (file-readable-p settings))
          (agent-repl--log-verbose nil
                                    "purge-legacy-hooks: settings candidate accepted dir=%s file=%s"
                                    dir settings)
          (push settings out))))
    (setq out (nreverse out))
    (agent-repl--log nil "purge-legacy-hooks: readable settings-files=%S" out)
    out))

(defun agent-repl--legacy-hooks-present-p ()
  "Return non-nil when any settings.json still registers a legacy hook.
A cheap READ-ONLY substring scan, deliberately not a JSON parse: the
question is only whether the purge has anything to do, and the script
owns the actual structural edit.  Being wrong in the false-positive
direction costs one no-op bash run; being wrong the other way would
leave the hooks firing, so the scan is on the script BASENAMES, which
appear in the registered command string however it is spelled."
  (let ((settings-files (agent-repl--legacy-hook-settings-files)))
    (agent-repl--log nil "purge-legacy-hooks: scanning settings-file-count=%d"
                      (length settings-files))
    (or
     (seq-some
      (lambda (settings)
        (let ((body (condition-case nil
                        (with-temp-buffer
                          (insert-file-contents settings)
                          (buffer-string))
                      ;; An unreadable settings.json is not evidence either
                      ;; way; report it and treat this file as clean rather
                      ;; than forcing a purge run on a guess.
                      (error
                       (agent-repl--log nil "purge-legacy-hooks: cannot read %s" settings)
                       nil))))
          (when body
            (let ((legacy-name
                   (seq-find (lambda (name) (string-search name body))
                             agent-repl--legacy-hook-scripts)))
              (if legacy-name
                  (progn
                    (agent-repl--log nil
                                      "purge-legacy-hooks: legacy hook found file=%s script=%s"
                                      settings legacy-name)
                    legacy-name)
                (agent-repl--log-verbose nil
                                          "purge-legacy-hooks: no legacy hook in file=%s"
                                          settings))))))
      settings-files)
     (progn
       (agent-repl--log nil "purge-legacy-hooks: no legacy hook entries found")
       nil))))

(defvar agent-repl--legacy-hooks-purged nil
  "Non-nil once the legacy-hook purge has run in this Emacs session.")

(defun agent-repl--maybe-purge-legacy-hooks ()
  "Remove this module's pre-cutover settings.json hook entries, once.
Called inline from this file's load, so the FIRST Emacs to boot on the
post-cutover code cleans up after the generation before it with no user
action.

Guarded three ways, in increasing cost:
  - once per Emacs session, so a `SPC j R' reload does not re-run it;
  - never in batch, matching every other install action here (the test
    suite must not touch the developer's real config);
  - only when a legacy entry is actually registered, so a healthy load is
    a couple of small file reads and no bash at all.

The script itself is idempotent, so a redundant run is harmless; these
guards are about cost and blast radius, not correctness.  A failure is
logged, never signalled: a leftover hook writes sentinel files nobody
reads, which must not be allowed to break module load."
  (cond
   (agent-repl--legacy-hooks-purged
    (agent-repl--log nil "purge-legacy-hooks: skipped; already attempted this session"))
   (noninteractive
    (agent-repl--log nil "purge-legacy-hooks: skipped; batch session"))
   ((not agent-repl-auto-install-hooks)
    (agent-repl--log nil "purge-legacy-hooks: skipped; auto-install disabled"))
   (t
    (setq agent-repl--legacy-hooks-purged t)
    (agent-repl--log nil "purge-legacy-hooks: marked attempted for this session")
    (if (agent-repl--legacy-hooks-present-p)
        (progn
          (agent-repl--log nil "purge-legacy-hooks: legacy hook entries found; purging")
          (condition-case err
              (pcase-let ((`(,code ,output)
                           (agent-repl--run-install-script "purge-legacy-hooks")))
                (if (eq code 0)
                    (agent-repl--log nil "purge-legacy-hooks: done\n%s" output)
                  (agent-repl--log nil "purge-legacy-hooks: FAILED (exit %s)\n%s" code output)))
            (error
             (agent-repl--log nil "purge-legacy-hooks: FAILED (%s)"
                              (error-message-string err)))))
      (agent-repl--log nil "purge-legacy-hooks: skipped; no legacy entries")))))

;; Run inline at load time so a missing managed skill symlink self-heals
;; on startup.  Guarded to no-op on healthy installs — see the function's
;; docstring for details.
(agent-repl--maybe-install-hooks)

;; Same shape, for the hook entries the previous generation left behind.
(agent-repl--maybe-purge-legacy-hooks)

(provide 'agent-repl-install)

;;; install.el ends here
