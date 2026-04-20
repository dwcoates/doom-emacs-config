;;; worktree.el --- workspace creation, worktree management, merge -*- lexical-binding: t; -*-

;;; Code:

(require 'filenotify)

(defvar +dwc/workspace-history nil
  "Workspace names ordered by most-recently-visited first.
Defined in config.el; declared here to suppress byte-compiler warnings.")

;;; Worktree initial buffers

(defcustom claude-repl-workspace-initial-buffers nil
  "Alist mapping repo path patterns to files opened when a worktree workspace is created.
Each entry is (PATTERN . FILES) where PATTERN is a regexp matched against the
worktree path with `string-match-p', and FILES is a list of paths relative to
the worktree root.  Files are added to the new workspace's perspective via
`persp-add-buffer' without being displayed.  Missing files emit a warning but
do not abort workspace creation."
  :type '(alist :key-type regexp :value-type (repeat string))
  :group 'claude-repl)

(defcustom claude-repl-workspace-commands-file-prefix "workspace_commands_"
  "Filename prefix for workspace command files in the output directory."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-workspace-commands-output-dir "~/.claude/output/"
  "Directory watched for workspace command files."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-worktree-dir-suffix "-worktrees"
  "Suffix appended to repo name to form the sibling worktrees directory."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-worktree-default-base "origin/master"
  "Default git ref for new worktree branches when no fork source is active."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-master-workspace-name "master"
  "Name of the master workspace used as merge target."
  :type 'string
  :group 'claude-repl)

(defun claude-repl--open-initial-buffers (ws path)
  "Open configured initial buffers for workspace WS rooted at PATH.
Checks `claude-repl-workspace-initial-buffers' for entries whose PATTERN
matches PATH, then opens each listed file with `find-file-noselect' and adds
it to the WS perspective without displaying it."
  (claude-repl--log ws "open-initial-buffers: path=%s" path)
  (when-let ((persp (persp-get-by-name ws)))
    (dolist (entry claude-repl-workspace-initial-buffers)
      (when (string-match-p (car entry) path)
        (dolist (relpath (cdr entry))
          (let ((fullpath (expand-file-name relpath path)))
            (if (file-exists-p fullpath)
                (progn
                  (claude-repl--log ws "open-initial-buffers: opening file=%s" fullpath)
                  (persp-add-buffer (find-file-noselect fullpath) persp t))
              (claude-repl--log ws "open-initial-buffers: file not found in worktree: %s" fullpath))))))))


(defvar claude-repl--workspace-generation-watch nil)

(defun claude-repl--workspace-commands-watch-handler (event)
  "Handle a file-notify EVENT for the workspace commands output directory.
Dispatches to `claude-repl--process-workspace-commands-file' when a
workspace_commands_*.json file is created, changed, or renamed."
  (let* ((action (nth 1 event))
         ;; renamed events carry (descriptor renamed old-file new-file)
         ;; all other events carry (descriptor action file)
         (file (if (eq action 'renamed)
                   (nth 3 event)
                 (nth 2 event))))
    (claude-repl--log nil "workspace-commands-watch-handler: action=%s file=%s" action file)
    (if (and (memq action '(changed created renamed))
             (string-prefix-p claude-repl-workspace-commands-file-prefix
                              (file-name-nondirectory file)))
        (claude-repl--process-workspace-commands-file file)
      (claude-repl--log nil "workspace-commands-watch-handler: skipped (wrong action or wrong prefix)"))))

(defun claude-repl--register-workspace-commands-watch ()
  "Register a file-notify watch on ~/.claude/output/ for workspace command files.
Tears down any existing watch first to avoid duplicates on re-eval."
  (let ((output-dir (expand-file-name claude-repl-workspace-commands-output-dir)))
    (make-directory output-dir t)
    (when (and claude-repl--workspace-generation-watch
               (file-notify-valid-p claude-repl--workspace-generation-watch))
      (file-notify-rm-watch claude-repl--workspace-generation-watch))
    (claude-repl--log nil "workspace-commands-watch: registering watch on %s for workspace_commands_*.json"
                      output-dir)
    (setq claude-repl--workspace-generation-watch
          (file-notify-add-watch
           output-dir
           '(change)
           #'claude-repl--workspace-commands-watch-handler))))

(claude-repl--register-workspace-commands-watch)

;;; Git helpers

(defun claude-repl--git-exit-code (root &rest args)
  "Run git in ROOT with ARGS, return exit code."
  (apply #'call-process "git" nil nil nil "-C" root args))

(defun claude-repl--git-branch-exists-p (root branch)
  "Return non-nil if BRANCH exists in git repo at ROOT."
  (let ((result (= 0 (claude-repl--git-exit-code root "rev-parse" "--verify" branch))))
    (claude-repl--log nil "git-branch-exists-p: root=%s branch=%s result=%s" root branch result)
    result))

(defun claude-repl--bare-workspace-name (ws)
  "Extract bare workspace name from WS (e.g. \"DWC/foo\" -> \"foo\")."
  (file-name-nondirectory (directory-file-name ws)))

(defun claude-repl--switch-to-workspace (ws)
  "Switch to workspace WS.
Tries `+workspace-switch-to' first, then `+workspace/switch-to' on failure.
Signals an error if both methods fail — downstream code assumes the switch
succeeded, so silent failure would operate on the wrong workspace."
  (claude-repl--log ws "switch-to-workspace: ws=%s" ws)
  (condition-case err
      (progn
        (+workspace-switch-to ws)
        (claude-repl--log ws "switch-to-workspace: switched successfully via +workspace-switch-to ws=%s" ws))
    (error
     (claude-repl--log ws "switch-to-workspace: +workspace-switch-to failed, trying fallback ws=%s: %s"
                       ws (error-message-string err))
     (condition-case err2
         (+workspace/switch-to ws)
       (error
        (error "claude-repl--switch-to-workspace: both switch methods failed for ws=%s — primary: %S, fallback: %S"
               ws err err2))))))

(defun claude-repl--assert-clean-worktree (ws project-root)
  "Signal `user-error' if PROJECT-ROOT has uncommitted changes.
WS is used only for the error message."
  (claude-repl--log ws "assert-clean-worktree: ws=%s project-root=%s" ws project-root)
  (let ((unstaged (/= 0 (claude-repl--git-exit-code project-root "diff" "--quiet")))
        (staged   (/= 0 (claude-repl--git-exit-code project-root "diff" "--cached" "--quiet"))))
    (claude-repl--log ws "assert-clean-worktree: ws=%s unstaged=%s staged=%s" ws unstaged staged)
    (when (or unstaged staged)
      (user-error "Uncommitted changes in workspace '%s' (dir: %s) [unstaged=%s staged=%s] — stash or commit before merging"
                  ws project-root unstaged staged))))

;;; Worktree registration and session setup

(defun claude-repl--register-worktree-ws (ws-id &optional ws)
  "Mark workspace WS as a worktree workspace.
WS-ID is the hash identifier (used for logging/buffer naming); the state
is stored under WS, defaulting to `+workspace-current-name'.
Signals an error if no workspace name can be determined.  The project
root is recorded by `claude-repl--initialize-ws-env', not here."
  (let ((ws (or ws (+workspace-current-name))))
    (unless ws
      (error "claude-repl--register-worktree-ws: no workspace name provided and no current workspace"))
    (claude-repl--log ws "register-worktree-ws ws-id=%s ws=%s" ws-id ws)
    (claude-repl--ws-put ws :worktree-p t)))

(defun claude-repl--setup-worktree-session (ws-id path ws force-bare-metal)
  "Register WS as a worktree at PATH and start its Claude session.
Passes PATH and the desired environment as hints to `initialize-claude',
which threads them into `initialize-ws-env' (the sole writer of
`:project-dir', `:active-env', and per-env instantiation structs)."
  (claude-repl--register-worktree-ws ws-id ws)
  (let ((default-directory (file-name-as-directory path)))
    (claude-repl--initialize-claude ws path (if force-bare-metal :bare-metal :sandbox)))
  (claude-repl--log ws "worktree pre-started Claude ws=%s cmd=%s"
                    ws (claude-repl-instantiation-start-cmd (claude-repl--active-inst ws))))

(defun claude-repl--async-git-sentinel (proc _event)
  "Process sentinel for `claude-repl--async-git'.
When PROC exits or is signaled, collects output, kills the process buffer,
and invokes the callback stored as a process property."
  (when (memq (process-status proc) '(exit signal))
    (let ((ok (zerop (process-exit-status proc)))
          (output (with-current-buffer (process-buffer proc)
                    (string-trim (buffer-string))))
          (callback (process-get proc 'claude-repl-callback)))
      (claude-repl--log nil "async-git-sentinel: proc=%s status=%s exit-code=%s"
                        (process-name proc) (process-status proc) (process-exit-status proc))
      (kill-buffer (process-buffer proc))
      (funcall callback ok output))))

(defun claude-repl--async-git (label git-root args callback)
  "Run git -C GIT-ROOT with ARGS asynchronously.
LABEL names the process and temp buffer.
CALLBACK is called with (SUCCESS-P OUTPUT) when the process exits."
  (claude-repl--log nil "async-git: label=%s git-root=%s args=%S" label git-root args)
  (let* ((buf (generate-new-buffer (format " *claude-repl-%s*" label)))
         (proc (apply #'start-process
                      (format "claude-repl-%s" label)
                      buf
                      "git" "-C" git-root
                      args)))
    (process-put proc 'claude-repl-callback callback)
    (set-process-sentinel proc #'claude-repl--async-git-sentinel)))

;;; Worktree creation

(defun claude-repl--resolve-worktree-paths (git-root name)
  "Compute worktree paths for branch NAME rooted at GIT-ROOT.
GIT-ROOT is the repository the new worktree is being created from — the
caller resolves it once (via `claude-repl--resolve-current-git-root' or an
explicit capture) and passes it in.
Returns a plist with keys :git-root, :dirname, :branch-name,
:worktree-parent, :path, and :in-worktree."
  (let* ((git-root (claude-repl--path-canonical git-root))
         (dirname (claude-repl--bare-workspace-name name))
         (git-root-parent (file-name-directory git-root))
         (in-worktree (file-regular-p (expand-file-name ".git" git-root)))
         (worktree-parent (if in-worktree
                              git-root-parent
                            (let* ((repo-name (file-name-nondirectory (directory-file-name git-root)))
                                   (wt-dir (expand-file-name (concat repo-name claude-repl-worktree-dir-suffix) git-root-parent)))
                              (make-directory wt-dir t)
                              wt-dir)))
         (path (claude-repl--path-canonical (expand-file-name dirname worktree-parent))))
    (claude-repl--log name "resolve-worktree-paths: git-root=%s dirname=%s branch-name=%s worktree-parent=%s path=%s in-worktree=%s"
                      git-root dirname name worktree-parent path in-worktree)
    (list :git-root git-root
          :dirname dirname
          :branch-name name
          :worktree-parent worktree-parent
          :path path
          :in-worktree in-worktree)))

(defun claude-repl--apply-workspace-properties (ws &rest plist)
  "Apply optional properties from PLIST to workspace WS.
PLIST is a flat property list of keyword/value pairs.  Each non-nil
value is stored via `claude-repl--ws-put'."
  (claude-repl--log ws "apply-workspace-properties: ws=%s plist=%S" ws plist)
  (cl-loop for (key val) on plist by #'cddr
           when val do (claude-repl--ws-put ws key val)))

(defun claude-repl--register-projectile-project (path dirname)
  "Write a .projectile marker and register PATH (named DIRNAME) with projectile."
  (write-region dirname nil (expand-file-name ".projectile" path))
  (claude-repl--log dirname "worktree wrote .projectile, adding to projectile known projects")
  (projectile-add-known-project (file-name-as-directory path)))

(defconst claude-repl--autonomous-prompt-prefix
  "Do not wait for further instructions. Come up with a plan and then immediately execute on it. When finished, commit the result. Here is the task:\n\n"
  "Prefix prepended to preemptive prompts to instruct Claude to plan,
execute, and commit autonomously without waiting for confirmation.")

(defun claude-repl--enqueue-preemptive-prompt (ws prompt)
  "Enqueue PROMPT on workspace WS for delivery once Claude is ready.
Sets :pending-show-panels so panels open after switching to WS."
  (if (and prompt (not (string-empty-p prompt)))
      (progn
        (claude-repl--log ws "enqueue-preemptive-prompt: ws=%s enqueuing prompt" ws)
        (claude-repl--ws-put ws :pending-prompts (list prompt))
        (claude-repl--ws-put ws :pending-show-panels t))
    (claude-repl--log ws "enqueue-preemptive-prompt: ws=%s prompt empty, skipping" ws)))

(defun claude-repl--finalize-worktree-workspace (path dirname preemptive-prompt
                                                       priority fork-session-id force-bare-metal
                                                       callback)
  "Finalize a new worktree workspace at PATH with directory name DIRNAME.
Registers the project with projectile, creates a Doom workspace, applies
optional PREEMPTIVE-PROMPT, PRIORITY, and FORK-SESSION-ID settings, starts
the Claude session (with FORCE-BARE-METAL controlling the environment),
and invokes CALLBACK with (PATH DIRNAME) when done."
  (claude-repl--log dirname "finalize-worktree-workspace: path=%s dirname=%s priority=%s fork-session-id=%s force-bare-metal=%s"
                    path dirname priority fork-session-id force-bare-metal)
  (claude-repl--register-projectile-project path dirname)
  (let* ((canonical (claude-repl--path-canonical path))
         (ws-id (substring (md5 canonical) 0 claude-repl-workspace-id-length))
         (ws dirname))
    (claude-repl--log ws "worktree creating workspace %s" ws)
    (+workspace-new ws)
    (claude-repl--open-initial-buffers ws path)
    (claude-repl--enqueue-preemptive-prompt ws preemptive-prompt)
    (claude-repl--apply-workspace-properties ws
      :priority priority
      :fork-session-id fork-session-id)
    (claude-repl--setup-worktree-session ws-id path ws force-bare-metal)
    (message "Worktree '%s' ready." dirname)
    (when callback (funcall callback path dirname))))

(defun claude-repl--worktree-add-callback (path dirname preemptive-prompt
                                               priority fork-session-id force-bare-metal
                                               callback ok output)
  "Handle the result of an async git-worktree-add operation.
OK and OUTPUT are the success flag and git output.  The remaining arguments
describe the workspace being created and are forwarded to
`claude-repl--finalize-worktree-workspace'."
  (claude-repl--log dirname "worktree git result: %s" output)
  (if ok
      (progn
        (claude-repl--log dirname "worktree-add-callback: ok=t path=%s dirname=%s" path dirname)
        (claude-repl--finalize-worktree-workspace
         path dirname preemptive-prompt
         priority fork-session-id force-bare-metal callback))
    (claude-repl--log dirname "worktree-add-callback: ok=nil (git worktree add failed) path=%s" path)
    (message "git worktree add failed: %s" output)))

(defun claude-repl--async-worktree-add (git-root branch-name path base-commit
                                              fork-session-id
                                              dirname preemptive-prompt
                                              priority force-bare-metal callback)
  "Run `git worktree add' asynchronously for a new worktree.
Creates the worktree at PATH on BRANCH-NAME off BASE-COMMIT in GIT-ROOT.
When the git command finishes, `claude-repl--worktree-add-callback'
finalizes the workspace."
  (let ((add-args (list "worktree" "add" "-b" branch-name path base-commit)))
    (claude-repl--log dirname "worktree async git add: %S" add-args)
    (claude-repl--async-git
     "worktree-add" git-root add-args
     (apply-partially #'claude-repl--worktree-add-callback
                      path dirname preemptive-prompt
                      priority fork-session-id force-bare-metal callback))))

(defun claude-repl--worktree-fetch-callback (add-fn _ok output)
  "Handle the result of an async git-fetch for worktree creation.
Logs OUTPUT and then calls ADD-FN to proceed with the worktree-add step."
  (claude-repl--log nil "worktree fetch: %s" output)
  (funcall add-fn))

(defun claude-repl--validate-worktree-creation (name git-root dirname branch-name path)
  "Validate that a worktree can be created for NAME.
Checks that NAME is non-empty, PATH does not already exist as a project,
and BRANCH-NAME does not already exist in GIT-ROOT.  DIRNAME is used for
error messages.  Signals `user-error' on any failure."
  (claude-repl--log name "validate-worktree-creation: name=%s git-root=%s dirname=%s branch-name=%s path=%s"
                    name git-root dirname branch-name path)
  (when (string-empty-p name)
    (user-error "Name cannot be empty"))
  (when (projectile-project-p path)
    (user-error "Worktree '%s' already exists — use SPC p p to switch to it" dirname))
  (when (claude-repl--git-branch-exists-p git-root branch-name)
    (claude-repl--log name "ERROR: branch '%s' already exists — cannot create worktree" branch-name)
    (user-error "Branch '%s' already exists — delete it first or choose a different name" branch-name)))

(defun claude-repl--do-create-worktree-workspace (name &optional force-bare-metal fork-session-id preemptive-prompt callback priority base-commit git-root)
  "Create a git worktree and Doom workspace for NAME.
Git fetch and worktree-add run asynchronously so Emacs is not blocked.
When everything is ready, CALLBACK (if non-nil) is called with (PATH DIRNAME).

BASE-COMMIT is the git ref the new branch is created from.  When nil,
defaults to \"HEAD\" if FORK-SESSION-ID is set (forks track the live
session's tip) and \"origin/master\" otherwise.  The interactive entry
point passes \"HEAD\" explicitly so `SPC TAB n' always branches off the
current worktree; `C-u SPC TAB n' passes \"origin/master\".

The fetch step runs only when BASE-COMMIT has an \"origin/\" prefix
\(i.e. the new branch needs an up-to-date remote ref).

GIT-ROOT is the repository the new worktree is rooted in.  When nil, it
is resolved once here via `claude-repl--resolve-current-git-root'.  The
commands-file flow captures the git root at enqueue time and passes it
in explicitly so the resolved value reflects the user's context at
command-receipt, not at timer-fire."
  (let* ((base-commit (or base-commit (if fork-session-id "HEAD" claude-repl-worktree-default-base)))
         (git-root (or git-root (claude-repl--resolve-current-git-root)))
         (paths (claude-repl--resolve-worktree-paths git-root name))
         (git-root (plist-get paths :git-root))
         (dirname (plist-get paths :dirname))
         (branch-name (plist-get paths :branch-name))
         (in-worktree (plist-get paths :in-worktree))
         (path (plist-get paths :path)))
    (claude-repl--validate-worktree-creation name git-root dirname branch-name path)
    (claude-repl--log name "worktree git-root=%s name=%s dirname=%s branch=%s base=%s in-worktree=%s path=%s old-ws=%s old-ws-id=%s"
             git-root name dirname (or branch-name "none") base-commit in-worktree path
             (+workspace-current-name) (claude-repl--workspace-id))
    ;; --- kick off: fetch (if base is a remote ref) then add ---------------
    (let ((add-fn (apply-partially #'claude-repl--async-worktree-add
                                   git-root branch-name path base-commit
                                   fork-session-id
                                   dirname preemptive-prompt
                                   priority force-bare-metal callback)))
      (message "Creating worktree '%s' from %s..." dirname base-commit)
      (cond
       (fork-session-id
        (funcall add-fn))
       ((string-prefix-p "origin/" base-commit)
        (claude-repl--async-git
         "fetch" git-root
         (list "fetch" "origin" (substring base-commit (length "origin/")))
         (apply-partially #'claude-repl--worktree-fetch-callback add-fn)))
       (t
        (funcall add-fn))))))

(defun claude-repl--worktree-creation-switch-callback (path dirname)
  "Switch to the newly created worktree workspace and open magit.
PATH is the worktree directory; DIRNAME is the workspace name."
  (claude-repl--log dirname "worktree-creation-switch-callback: path=%s dirname=%s fboundp(+workspace-switch-to)=%s current-ws=%s target=%s"
                    path dirname (fboundp '+workspace-switch-to) (+workspace-current-name) dirname)
  (claude-repl--switch-to-workspace dirname)
  (magit-status path))

(defun claude-repl-create-worktree-workspace (arg)
  "Create a new git worktree and switch to it as a project workspace.
Prompts for a name (may use branch-style slashes like DC/CV-100/cool-branch).
The worktree directory uses only the last path component; the full name becomes
the branch name.

If called from a worktree, the new worktree is created as a sibling (../<dirname>).
If called from a normal repo, it is created under ../<repo-name>-worktrees/<dirname>.

Without a prefix argument, the new branch is created off the current
worktree's HEAD — so edits in-flight here naturally carry over.  With
\\[universal-argument], the new branch is created off `origin/master'
instead (and `origin/master' is fetched first).

Optionally prompts for a preemptive prompt.  If provided, the new workspace is
created in the background (no switch) and the prompt is sent to Claude the moment
its session becomes ready.  If left blank, behavior is the same as before: switch
to the new workspace immediately.

Git operations (fetch, worktree add) run asynchronously so Emacs is not blocked."
  (interactive "P")
  (let* ((base-commit (if arg "origin/master" "HEAD"))
         (name (read-string "Worktree name: "))
         (raw-prompt (read-string "Preemptive prompt (blank to switch there normally): "))
         (has-preemptive (and raw-prompt (not (string-empty-p raw-prompt))))
         (preemptive-prompt (when has-preemptive
                              (concat claude-repl--autonomous-prompt-prefix raw-prompt))))
    (claude-repl--log name "create-worktree-workspace: name=%s base-commit=%s has-preemptive=%s"
                      name base-commit has-preemptive)
    (claude-repl--do-create-worktree-workspace
     name nil nil preemptive-prompt
     (unless has-preemptive #'claude-repl--worktree-creation-switch-callback)
     nil base-commit)))

(defun claude-repl-fork-worktree-workspace (arg)
  "Fork the current Claude session into a new worktree workspace.
Like `claude-repl-create-worktree-workspace', but branches from HEAD (not
origin/master) and resumes the current Claude session with --fork-session.

With \\[universal-argument], force bare-metal (skip Docker sandbox).

Optionally prompts for a preemptive prompt.  If provided, the new workspace is
created in the background (no switch) and the prompt is sent to Claude the moment
its session becomes ready.  If left blank, behavior is the same as before: switch
to the new workspace immediately.

Git operations (fetch, worktree add) run asynchronously so Emacs is not blocked."
  (interactive "P")
  (let* ((force-bare-metal (and arg t))
         (fork-session-id
          (let ((sid (claude-repl-instantiation-session-id
                      (claude-repl--active-inst (+workspace-current-name)))))
            (unless sid
              (user-error "No session ID for current workspace — cannot fork"))
            (claude-repl--log (+workspace-current-name) "fork-worktree-workspace: fork requested, sid=%s" sid)
            sid))
         (name (read-string "Worktree name: "))
         (raw-prompt (read-string "Preemptive prompt (blank to switch there normally): "))
         (has-preemptive (and raw-prompt (not (string-empty-p raw-prompt))))
         (preemptive-prompt (when has-preemptive
                              (concat claude-repl--autonomous-prompt-prefix raw-prompt))))
    (claude-repl--log name "fork-worktree-workspace: name=%s force-bare-metal=%s fork-session-id=%s has-preemptive=%s"
                      name force-bare-metal fork-session-id has-preemptive)
    (claude-repl--do-create-worktree-workspace
     name force-bare-metal fork-session-id preemptive-prompt
     (unless has-preemptive #'claude-repl--worktree-creation-switch-callback))))

(defun claude-repl--new-workspace ()
  "Create a new workspace and open magit-status in it, mirroring
the behavior of `+workspaces-switch-project-function'.
Signals an error if not inside a git repository."
  (interactive)
  (let ((root (claude-repl--git-root)))
    (unless root
      (error "claude-repl--new-workspace: not in a git repository"))
    (claude-repl--log (+workspace-current-name) "new-workspace: root=%s" root)
    (+workspace/new)
    ;; Hydrate the new workspace's env state (writes :project-dir from ROOT
    ;; via the sole writer, `initialize-ws-env'). `magit-status' only needs
    ;; a directory — we don't start Claude yet.
    (claude-repl--initialize-ws-env (+workspace-current-name) root)
    (magit-status root)))

;;; Prompt dispatch

(defun claude-repl--dispatch-prompt-command (ws prompt)
  "Send PROMPT to WS immediately if ready, otherwise enqueue on :pending-prompts.
WS may be a full branch name (e.g. DWC/foo) or a bare workspace name (e.g. foo);
it is normalized to the dirname before lookup."
  (let* ((ws (claude-repl--bare-workspace-name ws))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (cond
     ((and vterm-buf (buffer-local-value 'claude-repl--ready vterm-buf))
      (claude-repl--log ws "dispatch-prompt-command: ws=%s ready, sending prompt" ws)
      (claude-repl--send prompt ws))
     (t
      (claude-repl--log ws "dispatch-prompt-command: ws=%s %s, enqueuing"
                        ws (if vterm-buf "not ready" "not in registry"))
      (claude-repl--ws-put ws :pending-prompts
                           (append (claude-repl--ws-get ws :pending-prompts)
                                   (list prompt)))))))

;;; Worktree cleanup

(defun claude-repl--remove-git-worktree (project-dir)
  "Remove the git worktree at PROJECT-DIR and deregister it from projectile.
Runs `git worktree remove' with PROJECT-DIR itself as the `-C' target — any
worktree (including the one being removed) can execute the remove, so we
do not need to track the owning repository separately."
  (claude-repl--log nil "remove-git-worktree: project-dir=%s" project-dir)
  (let* ((expanded (expand-file-name project-dir))
         (result (claude-repl--git-string
                  "-C" expanded
                  "worktree" "remove" expanded)))
    (claude-repl--log nil "finish-workspace worktree-remove: %s" result))
  (projectile-remove-known-project (file-name-as-directory project-dir)))

(defun claude-repl--finish-workspace (ws)
  "Tear down workspace WS: kill Claude session, remove state, kill persp, remove worktree.
WS may be a full branch name (e.g. DWC/foo) or a bare workspace name (e.g. foo);
it is normalized to the dirname before lookup."
  (let* ((ws (claude-repl--bare-workspace-name ws))
         (worktree-p (claude-repl--ws-get ws :worktree-p))
         (project-dir (claude-repl--ws-get ws :project-dir))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (claude-repl--log ws "finish-workspace ws=%s worktree-p=%s path=%s"
                      ws worktree-p (or project-dir "nil"))
    ;; Kill the Claude vterm process.
    (claude-repl--log ws "finish-workspace: killing vterm process vterm-buf=%s" (if vterm-buf "present" "nil"))
    (when vterm-buf
      (claude-repl--kill-vterm-process vterm-buf))
    ;; Remove all claude-repl tracking state.
    (claude-repl--log ws "finish-workspace: removing ws state ws=%s" ws)
    (claude-repl--ws-del ws)
    ;; Kill the Doom perspective.
    (claude-repl--log ws "finish-workspace: killing persp ws=%s" ws)
    (when (member ws (+workspace-list-names))
      (persp-kill ws))
    ;; Remove the git worktree and projectile entry.
    (claude-repl--log ws "finish-workspace: removing worktree worktree-p=%s project-dir=%s" worktree-p project-dir)
    (when (and worktree-p project-dir (file-directory-p project-dir))
      (claude-repl--remove-git-worktree project-dir))
    (message "Finished workspace: %s" ws)))

;;; Workspace commands file processing

(defun claude-repl--resolve-fork-session-id (fork-from)
  "Resolve FORK-FROM workspace name to a Claude session ID.
FORK-FROM is a workspace name (possibly a full branch like \"DWC/foo\");
it is normalized to the bare name (\"foo\") before lookup.
Returns the session ID string.  Signals `error' if FORK-FROM is non-nil
but the workspace is unknown or has no active session — callers must not
silently degrade to origin/master when forking was explicitly requested."
  (when fork-from
    (let* ((ws (claude-repl--bare-workspace-name fork-from))
           (inst (ignore-errors (claude-repl--active-inst ws)))
           (sid (and inst (claude-repl-instantiation-session-id inst))))
      (claude-repl--log ws "resolve-fork-session-id: fork-from=%s ws=%s sid=%s" fork-from ws sid)
      (unless sid
        (claude-repl--log ws "resolve-fork-session-id: FAILED fork-from=%s ws=%s — no session ID found" fork-from ws)
        (error "Cannot fork from workspace '%s': no active session ID (workspace unknown or session not started)" fork-from))
      sid)))

(defun claude-repl--create-worktree-from-command (git-root name prompt priority &optional fork-session-id)
  "Timer callback: create a worktree workspace for NAME with PROMPT and PRIORITY.
GIT-ROOT is the repository captured at enqueue time (in
`claude-repl--handle-create-command'); it is threaded through so the
resolved root reflects the user's context at command-receipt rather than
whatever workspace happens to be active when the timer fires.
When FORK-SESSION-ID is non-nil, the new worktree branches from HEAD and
resumes the fork source's Claude session."
  (claude-repl--log name "create-worktree-from-command: name=%s git-root=%s priority=%s fork-session-id=%s"
                    name git-root priority fork-session-id)
  (claude-repl--do-create-worktree-workspace
   name nil fork-session-id prompt nil priority nil git-root))

(defcustom claude-repl-worktree-stagger-seconds 5
  "Seconds between staggered worktree creation timers.
Prevents concurrent Claude startups from corrupting ~/.claude.json."
  :type 'integer
  :group 'claude-repl)

(defun claude-repl--handle-create-command (cmd delay)
  "Handle a \"create\" workspace command CMD, scheduling it after DELAY seconds.
When CMD contains a \"fork_from\" field, resolves it to a session ID so the
new workspace forks from the source workspace's Claude session and HEAD.
If fork_from is present but resolution fails, the workspace is NOT created
and an error message is shown to the user."
  (let* ((name (alist-get 'name cmd))
         (prompt (alist-get 'prompt cmd nil))
         (priority (alist-get 'priority cmd nil))
         (fork-from (alist-get 'fork_from cmd nil))
         (fork-session-id
          (condition-case err
              (claude-repl--resolve-fork-session-id fork-from)
            (error
             (claude-repl--log name "handle-create-command: ABORTING workspace '%s' — fork resolution failed: %s"
                              name (error-message-string err))
             (message "[claude-repl] ERROR: cannot create workspace '%s' — %s" name (error-message-string err))
             nil))))
    ;; If fork_from was requested but resolution failed, refuse to create.
    (if (and fork-from (null fork-session-id))
        (claude-repl--log name "handle-create-command: SKIPPED workspace '%s' (fork_from=%s failed, refusing silent fallback)"
                          name fork-from)
      ;; Capture git-root at enqueue time so the timer fires with the user's
      ;; current context, not whatever workspace is active at fire time.
      (let ((git-root (claude-repl--resolve-current-git-root)))
        (claude-repl--log name "workspace-commands-file create: %s (delay %.1fs) priority=%s fork-session-id=%s git-root=%s"
                          name delay priority fork-session-id git-root)
        (run-with-timer delay nil
                        #'claude-repl--create-worktree-from-command
                        git-root name prompt priority fork-session-id)))))

(defun claude-repl--handle-prompt-command (cmd)
  "Handle a \"prompt\" workspace command CMD."
  (let ((ws (alist-get 'workspace cmd)))
    (claude-repl--log ws "workspace-commands-file prompt: ws=%s" ws)
    (claude-repl--dispatch-prompt-command ws (alist-get 'prompt cmd))))

(defun claude-repl--handle-finish-command (cmd)
  "Handle a \"finish\" workspace command CMD."
  (let ((ws (alist-get 'workspace cmd)))
    (claude-repl--log ws "workspace-commands-file finish: ws=%s" ws)
    (claude-repl--finish-workspace ws)))

(defun claude-repl--dispatch-workspace-command (cmd create-delay)
  "Dispatch a single workspace command CMD with current CREATE-DELAY.
Returns the new create-delay value (incremented for \"create\" commands,
unchanged otherwise)."
  (let ((type (alist-get 'type cmd)))
    (cond
     ((string= type "create")
      (claude-repl--handle-create-command cmd create-delay)
      (+ create-delay claude-repl-worktree-stagger-seconds))
     ((string= type "prompt")
      (claude-repl--handle-prompt-command cmd)
      create-delay)
     ((string= type "finish")
      (claude-repl--handle-finish-command cmd)
      create-delay)
     (t
      (claude-repl--log nil "workspace-commands-file unknown type: %s" type)
      create-delay))))

(defun claude-repl--process-workspace-commands-file (file)
  "Process a workspace commands file FILE, dispatching each typed command.
Create commands are staggered by `claude-repl-worktree-stagger-seconds' to
avoid concurrent Claude startup writes corrupting ~/.claude.json."
  (if (not (file-exists-p file))
      (claude-repl--log nil "workspace-commands-file not found: %s" file)
    (claude-repl--log nil "workspace-commands-file processing: %s" file)
    (let ((commands (json-read-file file))
          (create-delay 0))
      (dolist (cmd (append commands nil))
        (setq create-delay (claude-repl--dispatch-workspace-command cmd create-delay))))
    (delete-file file)
    (claude-repl--log nil "workspace-commands-file deleted: %s" file)))

;;; Workspace merging

(defun claude-repl--extract-cherry-pick-shas (log-text)
  "Extract cherry-picked commit SHAs from LOG-TEXT.
Parses \"(cherry picked from commit SHA)\" annotations added by git cherry-pick -x."
  (let (shas)
    (with-temp-buffer
      (insert log-text)
      (goto-char (point-min))
      (while (re-search-forward
              "(cherry picked from commit \\([0-9a-f]\\{40\\}\\))"
              nil t)
        (push (match-string 1) shas)))
    (claude-repl--log nil "extract-cherry-pick-shas: found %d SHAs" (length shas))
    shas))

(defun claude-repl--cherry-pick-base (project-root target-branch)
  "Compute cherry-pick start point for incorporating TARGET-BRANCH into HEAD.
Scans HEAD's unique commits (HEAD...TARGET-BRANCH left-only) for -x annotations
of the form \"(cherry picked from commit SHA)\". Returns the most recent TARGET
commit whose SHA appears in those annotations — so only genuinely new commits are
replayed. Falls back to `merge-base HEAD TARGET-BRANCH' when no annotations match
(first-time merge, or pre-annotation history)."
  (let* ((symmetric-range (format "HEAD...%s" target-branch))
         (target-commits
          (split-string
           (claude-repl--git-string
            "-C" project-root
            "log" "--right-only" "--pretty=%H" "--no-merges"
            symmetric-range)
           "\n" t))
         (head-log
          (claude-repl--git-string
           "-C" project-root
           "log" "--left-only" "--pretty=%B"
           symmetric-range))
         (incorporated (claude-repl--extract-cherry-pick-shas head-log)))
    (or (cl-find-if (lambda (sha) (member sha incorporated))
                    target-commits)
        (claude-repl--git-string
         "-C" project-root "merge-base" "HEAD" target-branch))))

(defalias '+dwc/workspace-merge--fork #'claude-repl--cherry-pick-base)

(defun claude-repl--workspace-branch (ws)
  "Return the git branch checked out in workspace WS's worktree, or nil.
Workspace name != branch name: e.g. persp \"fix-login\" was created from
\"DWC/fix-login\", so the branch is \"DWC/fix-login\" but the persp is \"fix-login\".
Resolves via :project-dir stored in `claude-repl--workspaces'."
  (when-let* ((path (claude-repl--ws-get ws :project-dir))
              (branch (claude-repl--git-string
                       "-C" path "rev-parse" "--abbrev-ref" "HEAD"))
              (_valid (not (or (string-empty-p branch)
                               (string-prefix-p "fatal" branch)))))
    (claude-repl--log ws "workspace-branch ws=%s path=%s branch=%s" ws path branch)
    (if (string= branch "HEAD")
        (let ((sha (claude-repl--git-string "-C" path "rev-parse" "HEAD")))
          (claude-repl--log ws "workspace-branch ws=%s detached HEAD, sha=%s" ws sha)
          sha)
      branch)))

(defalias '+dwc/workspace->branch #'claude-repl--workspace-branch)

(defun claude-repl--cherry-pick-commits (root target-ws base-branch target-branch)
  "Cherry-pick commits BASE-BRANCH..TARGET-BRANCH in repo at ROOT.
TARGET-WS is used only for error messages.
Signals `user-error' if all commits are already incorporated or if a
conflict is detected (after opening magit)."
  (let* ((range (format "%s..%s" base-branch target-branch))
         (range-count (claude-repl--git-string
                       "-C" root "rev-list" "--count" range)))
    (when (string= range-count "0")
      (user-error "All commits from workspace '%s' are already incorporated" target-ws))
    (claude-repl--log target-ws "cherry-pick-commits target-ws=%s target-branch=%s base=%s range=%s"
                      target-ws target-branch base-branch range)
    (let ((exit-code (claude-repl--git-exit-code root "cherry-pick" "-x" range)))
      (claude-repl--log target-ws "cherry-pick-commits exit-code=%s" exit-code))
    (claude-repl--check-cherry-pick-conflict target-ws root target-ws)))

(defun claude-repl--check-cherry-pick-conflict (ws root target-ws)
  "Check if a cherry-pick conflict exists in repo at ROOT.
WS is the workspace name for logging.
If CHERRY_PICK_HEAD exists, open magit and signal `user-error'
mentioning TARGET-WS."
  (let* ((git-dir (claude-repl--git-string
                   "-C" root "rev-parse" "--absolute-git-dir"))
         (cherry-pick-head (expand-file-name "CHERRY_PICK_HEAD" git-dir))
         (head-exists (file-exists-p cherry-pick-head)))
    (claude-repl--log ws "cherry-pick-commits git-dir=%s cherry-pick-head=%s exists=%s"
                      git-dir cherry-pick-head head-exists)
    (when head-exists
      (let ((conflicting-commit (claude-repl--git-string
                                 "-C" root
                                 "rev-parse" "--short" "CHERRY_PICK_HEAD")))
        (magit-status)
        (user-error "Conflict cherry-picking %s from '%s' — resolve in magit" conflicting-commit target-ws)))))

(defun claude-repl--workspace-merge-do (target-ws)
  "Cherry-pick TARGET-WS's branch commits onto the current branch.
Replays each commit from the target branch (since it diverged from master)
individually. Aborts cleanly if any commit conflicts."
  (let* ((current-ws (+workspace-current-name))
         (target-branch (claude-repl--workspace-branch target-ws)))
    (claude-repl--log current-ws "workspace-merge-do current-ws=%s target-ws=%s target-branch=%s"
                      current-ws target-ws target-branch)
    (unless target-branch
      (user-error "Cannot resolve branch for workspace '%s'" target-ws))
    (let* ((project-root (claude-repl--ws-dir current-ws)))
      (claude-repl--log current-ws "workspace-merge-do project-root=%s (for ws=%s)" project-root current-ws)
      (unless (claude-repl--git-branch-exists-p project-root target-branch)
        (user-error "Branch '%s' not found in repo %s" target-branch project-root))
      (let ((base (claude-repl--cherry-pick-base project-root target-branch)))
        (claude-repl--cherry-pick-commits project-root target-ws base target-branch))
      (claude-repl--finish-workspace target-ws)
      (message "Merged workspace '%s' -> '%s'." target-ws current-ws)
      (load-file claude-repl--config-file)
      (magit-status))))

(defalias '+dwc/workspace-merge--do #'claude-repl--workspace-merge-do)

(defun claude-repl-workspace-merge ()
  "Cherry-pick another workspace's branch commits onto the current branch.
Prompts for which workspace to merge in."
  (interactive)
  (let* ((current-ws (+workspace-current-name))
         (other-ws (remove current-ws (+workspace-list-names))))
    (claude-repl--log current-ws "workspace-merge: current-ws=%s" current-ws)
    (unless other-ws
      (user-error "No other workspaces to merge"))
    ;; Guard: uncommitted changes would interfere with cherry-pick.
    (claude-repl--assert-clean-worktree
     current-ws (claude-repl--ws-dir current-ws))
    (let* ((default-ws (cl-find-if
                        (lambda (ws)
                          (and (member ws other-ws)
                               (gethash ws claude-repl--workspaces)))
                        +dwc/workspace-history))
           (target-ws (completing-read
                       (if default-ws
                           (format "Merge workspace into current (default %s): "
                                   default-ws)
                         "Merge workspace into current: ")
                       other-ws nil t nil nil default-ws)))
      (claude-repl--workspace-merge-do target-ws))))

(defalias '+dwc/workspace-merge #'claude-repl-workspace-merge)

(defun claude-repl-workspace-merge-current-into-master ()
  "Merge the current workspace's branch into the master workspace.
Switches to master, then cherry-picks commits from the current workspace."
  (interactive)
  (let* ((source-ws (+workspace-current-name))
         (master-ws claude-repl-master-workspace-name))
    (claude-repl--log source-ws "workspace-merge-current-into-master: source-ws=%s master-ws=%s" source-ws master-ws)
    (unless (member master-ws (+workspace-list-names))
      (user-error "No workspace named 'master' found"))
    (when (string= source-ws master-ws)
      (user-error "Already on the master workspace"))
    ;; Guard: uncommitted changes would interfere with cherry-pick.
    (claude-repl--assert-clean-worktree
     source-ws (claude-repl--ws-dir source-ws))
    (claude-repl--switch-to-workspace master-ws)
    ;; After switching, default-directory still points to the source workspace.
    ;; Bind it to master's directory so projectile-project-root resolves correctly.
    (let* ((master-dir (or (claude-repl--ws-get master-ws :project-dir)
                           (user-error "Cannot determine master workspace directory")))
           (default-directory (file-name-as-directory master-dir)))
      (claude-repl--workspace-merge-do source-ws))))

(defalias '+dwc/workspace-merge-current-into-master #'claude-repl-workspace-merge-current-into-master)
