;;; app/agent-repl/codex.el -*- lexical-binding: t; -*-
;;
;; The codex backend: drives OpenAI's Codex CLI (`codex') through the
;; `agent-repl-backend' seam (see backend.el).
;;
;; Contract notes (verified against codex-cli 0.144.1 and codex-rs main):
;;
;; - Interactive: `codex' starts a new session; `codex resume <UUID>'
;;   continues one; `codex fork <UUID>' seeds a NEW session from one
;;   (the original rollout is untouched).  Flags (-m/--model, -a, -s,
;;   --dangerously-bypass-approvals-and-sandbox) are accepted after the
;;   id.  Codex has no way to CHOOSE a session id at launch — ids are
;;   auto-generated and captured afterward via the SessionStart hook,
;;   which is exactly how agent-repl already tracks claude session ids.
;; - Headless: `codex exec' reads the prompt from piped stdin when no
;;   prompt argument is given.  agent-repl's headless spawns run from
;;   `temporary-file-directory' (a non-git dir), so
;;   `--skip-git-repo-check' is mandatory.
;; - CODEX_HOME relocates all codex state (config, auth, sessions,
;;   hooks.json); codex requires the directory to pre-exist.
;; - Hooks: none.  agent-repl used to register sentinel-writing hook
;;   scripts in ~/.codex/hooks.json, but that whole plane is gone: the
;;   scripts are no longer installed, the sentinels they wrote are no
;;   longer dispatched, and session state for EVERY backend now arrives
;;   as pushed `frontend.v1' state from the daemon.  Nothing here writes
;;   hooks.json, and its presence says nothing about this backend.
;; - Rollouts: `<sessions>/YYYY/MM/DD/rollout-<timestamp>-<uuid>.jsonl'
;;   where <uuid> is the session id.  The model rides on `turn_context'
;;   lines (payload.model); token usage on `event_msg' lines with
;;   payload.type "token_count" (payload.info.last_token_usage, whose
;;   input_tokens already INCLUDE cached tokens — unlike claude's
;;   split counters).  There is no conversation-title field, so the
;;   codex backend has no TRANSCRIPT-TITLE-FN and the aiTitle segment
;;   renders empty.

(require 'cl-lib)
(require 'json)

;;;; ---- Defcustoms ---------------------------------------------------------

(defcustom agent-repl-codex-home nil
  "CODEX_HOME directory for codex sessions, or nil for the default ~/.codex.
When non-nil, every codex invocation is prefixed with
`CODEX_HOME=<dir>' and rollouts/hooks are looked up under <dir>.
Codex requires the directory to pre-exist."
  :type '(choice (const :tag "Use codex's default (~/.codex)" nil)
                 (directory :tag "Explicit CODEX_HOME"))
  :group 'agent-repl)

(defcustom agent-repl-codex-interactive-model nil
  "Model passed to `--model' for interactive codex sessions, or nil.
When nil, no `--model' flag is added and codex uses its configured
default.  A per-workspace `:model' property overrides this."
  :type '(choice (const :tag "Use codex's default" nil)
                 (string :tag "Model id"))
  :group 'agent-repl)

(defcustom agent-repl-codex-managed-permission-flags
  "--ask-for-approval on-request --sandbox workspace-write"
  "Approval/sandbox flags for managed projects under the codex backend.
Managed projects are those matching `agent-repl-managed-project-pattern'
(the same split `agent-repl--compute-perm-flag' uses for claude)."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-codex-personal-permission-flags
  "--ask-for-approval on-request --sandbox workspace-write"
  "Approval/sandbox flags for personal projects under the codex backend.
Defaults to the same posture as the managed flags, mirroring the claude
backend where both permission flags now default to
`--permission-mode auto' so workspaces never run dangerously by
default.  Users wanting the old unguarded behavior can set this to
\"--dangerously-bypass-approvals-and-sandbox\" (note codex rejects
combining that flag with `--ask-for-approval')."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-codex-scan-bytes 32768
  "Bytes read from the end of a codex rollout when scanning for the most
recent model / token-usage entry.  Rollout lines are small, so the file
tail suffices (same rationale as the claude segment scan caps)."
  :type 'integer
  :group 'agent-repl)

;;;; ---- Paths --------------------------------------------------------------

(defun agent-repl--codex-effective-home ()
  "Return the effective CODEX_HOME directory (expanded)."
  (expand-file-name (or agent-repl-codex-home "~/.codex")))

(defun agent-repl--codex-sessions-dir ()
  "Return the codex rollout root: <CODEX_HOME>/sessions."
  (expand-file-name "sessions" (agent-repl--codex-effective-home)))

;;;; ---- Interactive start command ------------------------------------------

(defun agent-repl--codex-perm-flags (project-dir)
  "Return the approval/sandbox flag string for PROJECT-DIR.
Managed projects (see `agent-repl--managed-project-p') get
`agent-repl-codex-managed-permission-flags'; personal projects get
`agent-repl-codex-personal-permission-flags'."
  (let* ((managed-p (agent-repl--managed-project-p project-dir))
         (flags (if managed-p
                    agent-repl-codex-managed-permission-flags
                  agent-repl-codex-personal-permission-flags)))
    (agent-repl--log nil
                     "codex-perm-flags: project-dir=%s managed-p=%s flags=%s"
                     project-dir managed-p flags)
    flags))

(defun agent-repl--codex-start-cmd (opts)
  "Build the interactive `codex' start command from OPTS.
This is the `codex' backend's START-CMD-FN (see `agent-repl-backend').
OPTS is a plist carrying `:session-id', `:fork-session-id',
`:project-dir' and `:model' (any may be nil except `:project-dir').

Subcommand selection mirrors the claude backend's resume semantics:
a `:fork-session-id' seeds a new session from another env's
conversation (`codex fork <id>'); otherwise a known `:session-id'
continues this env's own session (`codex resume <id>' — id-explicit
rather than claude's cwd-scoped `--continue', which sidesteps codex's
cwd-scoped picker entirely); otherwise a fresh `codex'."
  (let* ((session-id      (plist-get opts :session-id))
         (fork-session-id (plist-get opts :fork-session-id))
         (project-dir     (plist-get opts :project-dir))
         (model (or (plist-get opts :model) agent-repl-codex-interactive-model))
         (base (cond
                (fork-session-id (format "codex fork %s" fork-session-id))
                (session-id (format "codex resume %s" session-id))
                (t "codex")))
         (flags (string-trim
                 (mapconcat #'identity
                            (delq nil
                                  (list (when model (format "--model %s" model))
                                        (agent-repl--codex-perm-flags project-dir)))
                            " ")))
         (env-prefix (if agent-repl-codex-home
                         (format "CODEX_HOME=%s "
                                 (shell-quote-argument
                                  (agent-repl--codex-effective-home)))
                       ""))
         (cmd (string-trim (concat env-prefix base " " flags))))
    (agent-repl--log
     nil
     "codex-start-cmd: session-id=%s fork-session-id=%s project-dir=%s requested-model=%s configured-model=%s resolved-model=%s launch=%s codex-home=%s cmd=%s"
     session-id fork-session-id project-dir (plist-get opts :model)
     agent-repl-codex-interactive-model model
     (cond (fork-session-id 'fork) (session-id 'resume) (t 'fresh))
     agent-repl-codex-home cmd)
    cmd))

;;;; ---- Headless command ----------------------------------------------------

(defun agent-repl--codex-headless-cmd (model extra-args)
  "Return the argv for a one-shot headless `codex exec' run.
This is the `codex' backend's HEADLESS-CMD-FN (see
`agent-repl-backend').  MODEL is the `--model' id; EXTRA-ARGS is a list
of additional flags appended after the standard prefix.  The prompt is
delivered on the process's stdin by the caller (`codex exec' with no
prompt argument reads piped stdin).  `--skip-git-repo-check' is always
included because agent-repl's headless spawns run from
`temporary-file-directory', which is not a git repository — codex
refuses to run there without it."
  (let ((cmd (append (list "codex" "exec" "--skip-git-repo-check" "--model" model)
                     extra-args)))
    (agent-repl--log nil
                     "codex-headless-cmd: model=%s extra-args=%S cmd=%S"
                     model extra-args cmd)
    cmd))

;;;; ---- Rollout transcript: locate -----------------------------------------

(defun agent-repl--codex-rollout-path (ws)
  "Return the absolute path of WS's codex rollout transcript, or nil.
This is the `codex' backend's TRANSCRIPT-PATH-FN.  Locates
`rollout-<timestamp>-<session-id>.jsonl' under the dated subdirectories
of `agent-repl--codex-sessions-dir' for WS's current session id
\(captured by the SessionStart hook, like claude's).  The found path is
cached on WS's plist under `:codex-rollout-cache' keyed by session id —
a rollout never moves once created — and re-scanned only when the
cached file disappears.  Returns nil (never signals) when WS has no
session id, the sessions dir is absent, or no rollout matches, so the
mode-line eval stays safe before a session starts."
  (let* ((session-id (agent-repl--ai-title-ws-session-id ws))
         (cache (agent-repl--ws-get ws :codex-rollout-cache)))
    (cond
     ((not (and (stringp session-id) (not (string-empty-p session-id))))
      (agent-repl--log-verbose ws
                                "codex-rollout-path: session-id=%S cache=%S outcome=no-session"
                                session-id cache)
      nil)
     ((and (consp cache)
           (equal (car cache) session-id)
           (file-exists-p (cdr cache)))
      (agent-repl--log-verbose ws
                                "codex-rollout-path: session-id=%s cache=%S outcome=cache-hit path=%s"
                                session-id cache (cdr cache))
      (cdr cache))
     (t
      (let* ((dir (agent-repl--codex-sessions-dir))
             (dir-exists-p (file-directory-p dir))
             (path (and dir-exists-p
                        (car (directory-files-recursively
                              dir
                              (concat "\\`rollout-.*-"
                                      (regexp-quote session-id)
                                      "\\.jsonl\\'"))))))
        (when path
          (agent-repl--ws-put ws :codex-rollout-cache (cons session-id path)))
        (agent-repl--log-verbose
         ws
         "codex-rollout-path: session-id=%s cache=%S sessions-dir=%s dir-exists-p=%s outcome=%s path=%s"
         session-id cache dir dir-exists-p (if path 'found 'not-found) path)
        path)))))

;;;; ---- Rollout transcript: parse ------------------------------------------

(defun agent-repl--codex-parse-line (line)
  "Parse LINE (one rollout JSON object) into a string-keyed alist, or nil.
Unparseable lines yield nil so scanners can skip them (rollout schemas
grow fields across codex versions; parse leniently)."
  (condition-case err
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'string)
            (json-false nil)
            (json-null nil))
        (prog1 (json-read-from-string line)
          (agent-repl--log-verbose nil
                                    "codex-parse-line: chars=%d outcome=parsed"
                                    (length line))))
    (error
     (agent-repl--log-verbose nil
                               "codex-parse-line: chars=%d outcome=unparseable error=%S"
                               (length line) err)
     nil)))

(defun agent-repl--codex-context-extract-from-tail (tail)
  "Return the most recent context-token total from rollout TAIL, or nil.
Walks lines bottom-up for the newest `token_count' event whose
`payload.info' is non-null (rate-limit-only updates carry a null info
and must be skipped) and returns `info.last_token_usage.input_tokens' —
the input size of the most recent request, which codex reports
cache-inclusive (no separate cache counters to sum, unlike claude's)."
  (when (and (stringp tail) (not (string-empty-p tail)))
    (let ((lines (split-string tail "\n" t))
          (found nil)
          (candidate-count 0))
      (cl-loop for line in (nreverse lines)
               while (not found)
               when (string-match-p "\"token_count\"" line)
               do (progn
                    (cl-incf candidate-count)
                    (let* ((parsed (agent-repl--codex-parse-line line))
                         (payload (and (listp parsed)
                                       (cdr (assoc "payload" parsed))))
                         (info (and (listp payload)
                                    (equal (cdr (assoc "type" payload))
                                           "token_count")
                                    (cdr (assoc "info" payload))))
                         (last-usage (and (listp info)
                                          (cdr (assoc "last_token_usage" info))))
                         (input (and (listp last-usage)
                                     (cdr (assoc "input_tokens" last-usage)))))
                    (when (numberp input)
                      (setq found input)))))
      (agent-repl--log-verbose nil
                                "codex-context-extract-from-tail: chars=%d lines=%d candidates=%d input-tokens=%S"
                                (length tail) (length lines) candidate-count found)
      found)))

(defun agent-repl--codex-context-read (path)
  "Return the most recent context-token total from the rollout at PATH, or nil.
This is the `codex' backend's TRANSCRIPT-CONTEXT-FN.  Reads only the
trailing `agent-repl-codex-scan-bytes'."
  (let ((tail (agent-repl--transcript-read-tail path agent-repl-codex-scan-bytes)))
    (let ((context (and tail (agent-repl--codex-context-extract-from-tail tail))))
      (agent-repl--log-verbose nil
                                "codex-context-read: path=%s scan-bytes=%d tail-chars=%s input-tokens=%S"
                                path agent-repl-codex-scan-bytes
                                (and tail (length tail)) context)
      context)))

;;;; ---- Doctor --------------------------------------------------------------

(defun agent-repl--codex-in-use-p ()
  "Return non-nil when the codex backend is plausibly in use on this host.
True when the default backend is codex, any registered workspace
carries a codex `:backend' override, or an explicit
`agent-repl-codex-home' is configured.  The boundp guards keep this
callable from `doom doctor''s standalone load, where the module proper
is not loaded."
  (let* ((default-backend (and (boundp 'agent-repl-default-backend)
                               agent-repl-default-backend))
         (default-codex-p (eq default-backend 'codex))
         ;; `doom doctor' loads this file without workspace.el.  In normal
         ;; module loads, query every registered name so tombstoned codex
         ;; workspaces retain the exact semantics of the old registry scan.
         (workspace-api-p (and (fboundp 'agent-repl--ws-registered-names)
                               (fboundp 'agent-repl--ws-get)))
         (registered-names (and workspace-api-p
                                (agent-repl--ws-registered-names)))
         (workspace-codex-p
          (and workspace-api-p
               (cl-some (lambda (ws)
                          (eq (agent-repl--ws-get ws :backend) 'codex))
                        registered-names)))
         (result (or default-codex-p workspace-codex-p
                     agent-repl-codex-home)))
    (when (fboundp 'agent-repl--log)
      (agent-repl--log
       nil
       "codex-in-use-p: default-backend=%S default-codex-p=%s workspace-api-p=%s registered-names=%S workspace-codex-p=%s codex-home=%S result=%S"
       default-backend default-codex-p workspace-api-p registered-names
       workspace-codex-p agent-repl-codex-home result))
    result))

(defun agent-repl--codex-doctor-issues ()
  "Return a list of (LEVEL . MESSAGE) describing codex-backend problems.
Empty when the codex backend is not in use (see
`agent-repl--codex-in-use-p'), when running inside the sandbox, or when
everything checks out.  Checks: the `codex' binary on PATH, the
existence of an explicitly configured CODEX_HOME (codex requires it to
pre-exist)."
  (cond
   ((agent-repl--in-sandbox-p) nil)
   ((not (agent-repl--codex-in-use-p)) nil)
   (t
    (let ((issues nil))
      (unless (executable-find "codex")
        (push (cons 'error "codex backend in use but `codex' is not on PATH")
              issues))
      (when (and agent-repl-codex-home
                 (not (file-directory-p (agent-repl--codex-effective-home))))
        (push (cons 'error
                    (format "agent-repl-codex-home %s does not exist — codex requires CODEX_HOME to pre-exist"
                            (agent-repl--codex-effective-home)))
              issues))
      (nreverse issues)))))

;;;; ---- Backend registration -----------------------------------------------

;; Guarded because `doom doctor' loads this file standalone (after
;; install.el, without backend.el) purely for
;; `agent-repl--codex-doctor-issues'; in the normal module load order
;; backend.el precedes this file and the registration always runs.
(when (fboundp 'agent-repl-backend-create)
  (agent-repl-register-backend
   (agent-repl-backend-create
    :name 'codex
    :binary "codex"
    :start-cmd-fn #'agent-repl--codex-start-cmd
    :headless-cmd-fn #'agent-repl--codex-headless-cmd
    :transcript-path-fn #'agent-repl--codex-rollout-path
    ;; No TRANSCRIPT-TITLE-FN: codex rollouts carry no conversation
    ;; title, so the aiTitle segment renders empty for codex workspaces.
    :transcript-context-fn #'agent-repl--codex-context-read)))

(provide 'agent-repl-codex)
;;; codex.el ends here
