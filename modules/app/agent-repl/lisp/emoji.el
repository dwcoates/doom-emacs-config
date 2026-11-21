;;; emoji.el --- random emoji prefixes for agent-repl commits -*- lexical-binding: t; -*-

;;; Commentary:

;; Automatically prefix agent-repl commit messages with a random emoji.
;; Integrates with Magit via `git-commit-setup-hook' for interactive commits,
;; and provides a `prepare-commit-msg' hook script for git CLI commits
;; (e.g. from Claude Code agents).
;;
;; The emoji is chosen based on the conventional-commit type (feat, fix, etc.)
;; with a wildcard injection chance for extra variety.

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(require 'cl-lib)

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--git-string-quiet "core")
(declare-function agent-repl--ws-current-log-name "workspace")

(defconst agent-repl--emoji-categories
  `((feat     . ("✨" "🚀" "🎉" "🌟" "💡" "🎨" "🌈" "🔮" "🎯" "⚡" "🏗" "🧩" "🪅" "🌻" "🍀"))
    (fix      . ("🔧" "🩹" "🐛" "🔨" "🛠" "🪛" "🏥" "💊" "🩺" "🪚" "🔩" "⛏" "🪠" "🧰" "🦷"))
    (refactor . ("♻" "🧹" "🪄" "🧬" "🏛" "🪆" "🎭" "🗿" "🧊" "💎" "🪨" "⚗" "🔬" "🧪" "📐"))
    (test     . ("🧪" "🔍" "🕵" "🎯" "📋" "✅" "🧫" "🔎" "📊" "🎓" "🧮" "📏" "⚖" "🏁" "🔬"))
    (docs     . ("📝" "📖" "📚" "🗒" "📄" "✏" "🖊" "📑" "📓" "🔖" "📰" "🏷" "🗞" "📃" "🗂"))
    (style    . ("💅" "🎨" "🖌" "🎭" "👗" "💄" "🪞" "🎀" "🌸" "🦋" "🧶" "🪡" "🎏" "🏮" "🪭"))
    (perf     . ("⚡" "🏎" "💨" "🚄" "🏃" "⏱" "🔥" "💪" "🦅" "🎿" "🏊" "🏋" "🧲" "⛷" "🏇"))
    (chore    . ("🔖" "📦" "🏷" "🔗" "📌" "🗃" "🧹" "📎" "🗄" "🛒" "🧺" "📍" "🪝" "🗑" "📮"))
    (ci       . ("🤖" "⚙" "🔄" "🏗" "🔀" "🛞" "🧩" "🪤" "⛓" "🎰" "🕹" "📡" "🛸" "🧭" "🏭"))
    (wildcard . ("🦄" "🐉" "🌵" "🍄" "🎸" "🪩" "🫧" "🧊" "🌋" "🦑" "🪸" "🎪" "🛸" "🪐" "🦕"
                 "🐙" "🦥" "🦔" "🐝" "🦊" "🐸" "🐧" "🦉" "🐺" "🦁" "🐨" "🦋" "🐬" "🦈" "🐢"
                 "🌮" "🍕" "🥨" "🧁" "🍩" "🫐" "🍉" "🥝" "🍇" "🧀" "🌶" "🥑" "🍑" "🫠" "🍣")))
  "Alist mapping commit type symbols to lists of candidate emojis.
The `wildcard' category provides maximum variety for any commit type.")

(defun agent-repl--emoji-log-ws ()
  "Return the workspace active for the current emoji operation, if any.

Magit commit buffers do not carry `agent-repl--owning-workspace', so use the
workspace integration boundary rather than buffer ownership.  Plain Git
contexts legitimately return nil and produce global diagnostics.

Screened through `agent-repl--ws-current-log-name' because this value only
ever feeds the logging ladder: committing from outside any workspace leaves
persp-mode's \"none\" placeholder current, which owns no log sink and would
otherwise make the diagnostic signal instead of being recorded globally."
  (agent-repl--ws-current-log-name))

(defun agent-repl--current-branch ()
  "Return the current git branch name (string), or nil if unresolvable.
Returns nil when the working tree is not in a git repo, when HEAD is
detached, or when the branch name is empty.  Routes through
`agent-repl--git-string-quiet' (the registered external-boundary
wrapper) so the call is mocked by the test-time runtime guards."
  (let* ((ws (agent-repl--emoji-log-ws))
         (out (agent-repl--git-string-quiet "rev-parse" "--abbrev-ref" "HEAD")))
    (cond
     ((string-empty-p out)
      (agent-repl--log ws "emoji-current-branch: unresolved reason=empty-git-output")
      nil)
     ((string= out "HEAD")
      (agent-repl--log ws "emoji-current-branch: unresolved reason=detached-head output={output}" :output (prin1-to-string out))
      nil)
     (t
      (agent-repl--log ws "emoji-current-branch: resolved branch={branch}" :branch (prin1-to-string out))
      out))))

(defun agent-repl--commit-prefix-regex (branch)
  "Return a regex matching `<type>(<BRANCH>): <rest>' at start of message.
Captures the type as group 1 and the rest-of-line as group 2.  BRANCH
is regex-quoted so branch names with `.', `/', `+' etc. are handled
literally."
  (concat "^\\([a-z]+\\)("
          (regexp-quote branch)
          "): \\(.*\\)"))

(defcustom agent-repl-emoji-wildcard-chance 30
  "Percentage chance (0-100) of using a wildcard emoji instead of a typed one.
Higher values produce more variety at the cost of semantic relevance."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-emoji-lookback 50
  "Number of recent conventional-commit lines to scan for already-used emojis.
Emojis used in the last LOOKBACK matching commits are excluded from the
candidate pool, deterministically guaranteeing variety from the git history.
When the typed pool is fully exhausted by recents, falls back to the
wildcard pool (also minus recents).

Scans commits whose subject matches `<type>(<scope>):' (any scope) so
both the legacy `<emoji> type(scope):' format and the current
`type(scope): <emoji> ...' format contribute to the recents set."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--commit-type-from-message (msg)
  "Extract the conventional-commit type keyword from MSG.
Returns a symbol like `feat', `fix', etc., or `wildcard' if no type matches.
Tolerant of any scope: looks at the leading `<type>(' substring."
  (let ((ws (agent-repl--emoji-log-ws)))
    (if (string-match "^\\([a-z]+\\)(" msg)
        (let ((type (intern (match-string 1 msg))))
          (if (assq type agent-repl--emoji-categories)
              (progn
                (agent-repl--log-verbose ws
                  "emoji-commit-type: message={message} parsed-type={parsed_type} category-known=t"
                  :message (prin1-to-string msg) :parsed_type (prin1-to-string type))
                type)
            (agent-repl--log-verbose ws
              "emoji-commit-type: message={message} parsed-type={parsed_type} category-known=nil effective-type=wildcard"
              :message (prin1-to-string msg) :parsed_type (prin1-to-string type))
            'wildcard))
      (agent-repl--log-verbose ws
        "emoji-commit-type: message={message} parsed-type=nil effective-type=wildcard"
        :message (prin1-to-string msg))
      'wildcard)))

(defun agent-repl--extract-commit-emoji (line)
  "Return the emoji token in conventional-commit subject LINE, or nil.
Recognizes both formats:
  - Legacy: `<emoji> type(scope): description'  → first token.
  - Current: `type(scope): <emoji> description' → first token after `: '.
The token must start with a non-ASCII character to qualify."
  (let* ((ws (agent-repl--emoji-log-ws))
         (token
         (cond
          ;; New format: type(scope): EMOJI rest
          ((string-match "^[a-z]+([^)]+): \\([^ ]+\\)" line)
           (match-string 1 line))
          ;; Legacy format: EMOJI type(scope): rest — first token.
          (t (car (split-string line " " t))))))
    (let ((accepted (and token
                         (> (length token) 0)
                         (> (aref token 0) 127))))
      (agent-repl--log-verbose ws
        "emoji-extract-commit-emoji: line={line} token={token} accepted={accepted}"
        :line (prin1-to-string line) :token (prin1-to-string token) :accepted accepted)
      (and accepted token))))

(defun agent-repl--recent-commit-emojis (&optional lookback)
  "Return list of emojis used by the last LOOKBACK conventional commits.
Defaults to `agent-repl-emoji-lookback'.  Returns nil when not in a
git repository or on any git error.  Newest commit first.

Scans commits whose subject matches `<type>(<scope>):' (any scope) so
both legacy and current formats contribute — the post-rebase change
to a branch-as-scope convention means filtering by a literal scope
string would miss commits authored under the new format."
  (let* ((ws (agent-repl--emoji-log-ws))
         (n (or lookback agent-repl-emoji-lookback))
         ;; Match any conventional-commit subject (any scope) via -E.
         ;; Route through `agent-repl--git-string-quiet' so the call is
         ;; the registered external-boundary wrapper; tests mock the
         ;; wrapper rather than `shell-command-to-string' directly.
         (output (condition-case err
                     (agent-repl--git-string-quiet
                      "log" "-n" (number-to-string n) "-E"
                      "--grep=^[a-z]+\\(.+\\):"
                      "--format=%s")
                   (error
                    (agent-repl--warn ws
                      "emoji-recent-commit-emojis: git-log failed lookback={lookback} error={error}"
                      :lookback n :error (prin1-to-string err))
                    nil)))
         (lines (and (stringp output) (split-string output "\n" t)))
         (emojis '()))
    (agent-repl--log ws
      "emoji-recent-commit-emojis: git-log completed lookback={lookback} output-type={output_type} line-count={line_count}"
      :lookback n :output_type (prin1-to-string (type-of output)) :line_count (length lines))
    (dolist (line lines)
      (when-let ((emoji (agent-repl--extract-commit-emoji line)))
        (push emoji emojis)))
    (setq emojis (nreverse emojis))
    (agent-repl--log ws
      "emoji-recent-commit-emojis: extracted emoji-count={emoji_count} emojis={emojis}"
      :emoji_count (length emojis) :emojis (prin1-to-string emojis))
    emojis))

(defun agent-repl--filter-pool (pool exclude)
  "Return POOL with any emoji string in EXCLUDE removed."
  (let ((filtered (cl-remove-if (lambda (e) (member e exclude)) pool)))
    (agent-repl--log-verbose (agent-repl--emoji-log-ws)
      "emoji-filter-pool: pool-count={pool_count} exclude-count={exclude_count} candidate-count={candidate_count}"
      :pool_count (length pool) :exclude_count (length exclude) :candidate_count (length filtered))
    filtered))

(defun agent-repl--random-commit-emoji (&optional commit-type recents)
  "Return a random emoji for COMMIT-TYPE (a symbol), avoiding RECENTS.
When COMMIT-TYPE is nil or not in `agent-repl--emoji-categories',
uses `wildcard'.
Injects a wildcard emoji `agent-repl-emoji-wildcard-chance' percent of the time
regardless of type, to maximize variety.
RECENTS, when non-nil, is a list of emoji strings to exclude from the pool;
if exclusion empties the typed pool, falls back to the wildcard pool minus
RECENTS, then to the full wildcard pool as a final guarantee of progress."
  (let* ((ws (agent-repl--emoji-log-ws))
         (type (or commit-type 'wildcard))
         (use-wildcard (< (random 100) agent-repl-emoji-wildcard-chance))
         (effective-type (if (or use-wildcard
                                (not (assq type agent-repl--emoji-categories)))
                            'wildcard
                          type))
         (candidates (agent-repl--filter-pool
                      (cdr (assq effective-type agent-repl--emoji-categories))
                      recents)))
    (agent-repl--log ws
      "emoji-random: requested-type={requested_type} effective-type={effective_type} use-wildcard={use_wildcard} wildcard-chance={wildcard_chance} recents-count={recents_count} initial-candidate-count={initial_candidate_count}"
      :requested_type (prin1-to-string commit-type) :effective_type (prin1-to-string effective-type) :use_wildcard use-wildcard :wildcard_chance agent-repl-emoji-wildcard-chance
      :recents_count (length recents) :initial_candidate_count (length candidates))
    (when (null candidates)
      (agent-repl--log ws
        "emoji-random: typed-pool-exhausted requested-type={requested_type} effective-type={effective_type} action=use-filtered-wildcard"
        :requested_type (prin1-to-string commit-type) :effective_type (prin1-to-string effective-type))
      (setq candidates (agent-repl--filter-pool
                        (cdr (assq 'wildcard agent-repl--emoji-categories))
                        recents)))
    (when (null candidates)
      (agent-repl--log ws
        "emoji-random: filtered-wildcard-pool-exhausted requested-type={requested_type} action=use-full-wildcard"
        :requested_type (prin1-to-string commit-type))
      (setq candidates (cdr (assq 'wildcard agent-repl--emoji-categories))))
    (let ((emoji (nth (random (length candidates)) candidates)))
      (agent-repl--log ws
        "emoji-random: selected emoji={emoji} final-candidate-count={final_candidate_count}"
        :emoji (prin1-to-string emoji) :final_candidate_count (length candidates))
      emoji)))

(defun agent-repl--description-has-emoji-prefix-p (description)
  "Return non-nil if DESCRIPTION starts with a non-ASCII char (likely emoji)."
  (let ((has-prefix (and (> (length description) 0)
                         (> (aref description 0) 127))))
    (agent-repl--log-verbose (agent-repl--emoji-log-ws)
      "emoji-description-prefix: description={description} has-emoji-prefix={has_emoji_prefix}"
      :description (prin1-to-string description) :has_emoji_prefix has-prefix)
    has-prefix))

;; Backward-compat alias — older tests still call this name.
(defalias 'agent-repl--message-has-emoji-prefix-p
  'agent-repl--description-has-emoji-prefix-p)

(defconst agent-repl--no-scope-prefix-regex
  "^\\([a-z]+\\): \\(.*\\)"
  "Regex matching `<type>: <rest>' (no scope) at start of message.
Captures the type as group 1 and the rest-of-line as group 2.")

(defun agent-repl--emoji-prefix-commit-message (msg &optional branch-override)
  "Inject the active branch as scope and a random emoji into MSG.
Two input shapes are normalized to `<type>(<branch>): <emoji> <description>':

  - `<type>: <description>'           → branch is injected as scope and
    an emoji is prepended to the description.
  - `<type>(<branch>): <description>' → emoji is prepended to the
    description (existing behavior).

When the description already starts with a non-ASCII char (likely
emoji), the emoji injection step is skipped — but a missing scope is
still filled in with the branch, so a hand-written
`feat: 🚀 desc' becomes `feat(<branch>): 🚀 desc'.  When the scope
is present but does NOT match the branch, MSG is returned unchanged
(the user's explicit scope choice is respected).  When the branch
cannot be resolved (no git repo / detached HEAD), MSG is returned
unchanged.

Excludes emojis used in the last `agent-repl-emoji-lookback' matching
commits to avoid back-to-back repeats.

BRANCH-OVERRIDE lets callers (mainly tests) inject a fixed branch
without going through `git rev-parse'."
  (let* ((ws (agent-repl--emoji-log-ws))
         (branch (or branch-override (agent-repl--current-branch))))
    (agent-repl--log ws
      "emoji-prefix-commit-message: entry branch-override={branch_override} resolved-branch={resolved_branch} message={message}"
      :branch_override (prin1-to-string branch-override) :resolved_branch (prin1-to-string branch) :message (prin1-to-string msg))
    (if (or (null branch) (string-empty-p branch))
        (progn
          (agent-repl--log ws
            "emoji-prefix-commit-message: unchanged reason=unresolved-branch")
          msg)
      (let ((branch-rx (agent-repl--commit-prefix-regex branch)))
        (cond
         ;; Already scoped to the branch: inject emoji only.
         ((string-match branch-rx msg)
          (let ((type-str (match-string 1 msg))
                (description (match-string 2 msg))
                (rest (substring msg (match-end 0))))
            (if (agent-repl--description-has-emoji-prefix-p description)
                (progn
                  (agent-repl--log ws
                    "emoji-prefix-commit-message: unchanged reason=branch-scope-description-has-emoji type={type} branch={branch}"
                    :type (prin1-to-string type-str) :branch (prin1-to-string branch))
                  msg)
              (let* ((type (agent-repl--commit-type-from-message msg))
                     (recents (agent-repl--recent-commit-emojis))
                     (emoji (agent-repl--random-commit-emoji type recents)))
                (let ((result (concat type-str "(" branch "): "
                                    emoji " " description rest)))
                  (agent-repl--log ws
                    "emoji-prefix-commit-message: changed reason=branch-scope-injected type={type} branch={branch} emoji={emoji} result={result}"
                    :type (prin1-to-string type-str) :branch (prin1-to-string branch) :emoji (prin1-to-string emoji) :result (prin1-to-string result))
                  result)))))
         ;; No scope at all: inject branch as scope, plus emoji unless
         ;; description already starts with one.
         ((string-match agent-repl--no-scope-prefix-regex msg)
          (let* ((type-str (match-string 1 msg))
                 (description (match-string 2 msg))
                 (rest (substring msg (match-end 0))))
            (if (agent-repl--description-has-emoji-prefix-p description)
                (let ((result (concat type-str "(" branch "): " description rest)))
                  (agent-repl--log ws
                    "emoji-prefix-commit-message: changed reason=no-scope-emoji-preserved type={type} branch={branch} result={result}"
                    :type (prin1-to-string type-str) :branch (prin1-to-string branch) :result (prin1-to-string result))
                  result)
              (let* ((type-sym (intern type-str))
                     (effective-type (if (assq type-sym agent-repl--emoji-categories)
                                         type-sym
                                       'wildcard))
                     (recents (agent-repl--recent-commit-emojis))
                     (emoji (agent-repl--random-commit-emoji effective-type recents)))
                (let ((result (concat type-str "(" branch "): "
                                    emoji " " description rest)))
                  (agent-repl--log ws
                    "emoji-prefix-commit-message: changed reason=no-scope-injected type={type} effective-type={effective_type} branch={branch} emoji={emoji} result={result}"
                    :type (prin1-to-string type-str) :effective_type (prin1-to-string effective-type) :branch (prin1-to-string branch) :emoji (prin1-to-string emoji) :result (prin1-to-string result))
                  result)))))
         ;; Scope present but not the branch (or no conventional prefix
         ;; at all): leave unchanged.
         (t
          (agent-repl--log ws
            "emoji-prefix-commit-message: unchanged reason=nonmatching-scope-or-prefix branch={branch} message={message}"
            :branch (prin1-to-string branch) :message (prin1-to-string msg))
          msg))))))

;;; Magit integration

(defun agent-repl--magit-emoji-setup ()
  "Inject a random emoji into the commit-message buffer's first line.
Intended for `git-commit-setup-hook'.  Acts only when the buffer's
first line matches `<type>(<current-branch>): <description>' and the
description does not already start with a non-ASCII character.  The
emoji is inserted between `: ' and the description, producing
`<type>(<branch>): <emoji> <description>'.  Variety/lookback handling
is delegated to `--emoji-prefix-commit-message' (which threads the
recent-emojis list through `--random-commit-emoji')."
  (let* ((ws (agent-repl--emoji-log-ws))
         (branch (agent-repl--current-branch)))
    (agent-repl--log ws
      "magit-emoji-setup: entry branch={branch} buffer={buffer} point={point}"
      :branch (prin1-to-string branch) :buffer (prin1-to-string (buffer-name)) :point (point))
    (if (and branch (not (string-empty-p branch)))
        (let* ((msg (buffer-string))
               (replaced (agent-repl--emoji-prefix-commit-message msg branch)))
          (if (equal replaced msg)
              (agent-repl--log ws
                "magit-emoji-setup: unchanged branch={branch} reason=prefixer-returned-input" :branch (prin1-to-string branch))
            (let ((point-pos (point)))
              (erase-buffer)
              (insert replaced)
              (goto-char (min point-pos (point-max)))
              (agent-repl--log ws
                "magit-emoji-setup: changed branch={branch} old-point={old_point} new-point={new_point} result={result}"
                :branch (prin1-to-string branch) :old_point point-pos :new_point (point) :result (prin1-to-string replaced)))))
      (agent-repl--log ws
        "magit-emoji-setup: unchanged reason=unresolved-branch branch={branch}" :branch (prin1-to-string branch)))))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook #'agent-repl--magit-emoji-setup))

;;; Git hook installation

(defconst agent-repl--prepare-commit-msg-hook-source
  (expand-file-name "../hooks/prepare-commit-msg-emoji.sh"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the checked-in prepare-commit-msg hook script.
`hooks/' sits at the module root; this file lives in `lisp/' below it.")

(defun agent-repl--git-hooks-dir ()
  "Return the git hooks directory for the current repository.
Works for both normal repos (.git/hooks) and worktrees.  Routes
through the mockable `agent-repl--git-string-quiet' wrapper so
tests can stub it via `cl-letf' instead of invoking real git
(see AGENTS.md \"No External Processes or External State in
Tests\")."
  (let* ((ws (agent-repl--emoji-log-ws))
         (git-dir (agent-repl--git-string-quiet "rev-parse" "--git-common-dir")))
    (if (and (not (string-empty-p git-dir))
             (not (string-prefix-p "fatal" git-dir)))
        (let ((hooks-dir (expand-file-name "hooks" git-dir)))
          (agent-repl--log ws
            "emoji-git-hooks-dir: resolved git-dir={git_dir} hooks-dir={hooks_dir}" :git_dir (prin1-to-string git-dir) :hooks_dir (prin1-to-string hooks-dir))
          hooks-dir)
      (agent-repl--log ws
        "emoji-git-hooks-dir: unresolved git-dir={git_dir} reason=empty-or-fatal-output" :git_dir (prin1-to-string git-dir))
      nil)))

;;;###autoload
(defun agent-repl-install-commit-emoji-hook ()
  "Install the prepare-commit-msg hook for automatic emoji prefixes.
Copies the hook script to the current repository's git hooks directory.
If an existing prepare-commit-msg hook is found, backs it up first."
  (interactive)
  (let ((ws (agent-repl--emoji-log-ws))
        (hooks-dir (agent-repl--git-hooks-dir)))
    (agent-repl--log ws
      "install-commit-emoji-hook: entry hooks-dir={hooks_dir} source={source}"
      :hooks_dir (prin1-to-string hooks-dir) :source (prin1-to-string agent-repl--prepare-commit-msg-hook-source))
    (unless hooks-dir
      (agent-repl--log ws "install-commit-emoji-hook: abort reason=not-in-git-repository")
      (user-error "Not inside a git repository"))
    (unless (file-exists-p agent-repl--prepare-commit-msg-hook-source)
      (agent-repl--log ws
        "install-commit-emoji-hook: abort reason=missing-hook-source source={source}"
        :source (prin1-to-string agent-repl--prepare-commit-msg-hook-source))
      (user-error "Hook source not found: %s" agent-repl--prepare-commit-msg-hook-source))
    (let ((dest (expand-file-name "prepare-commit-msg" hooks-dir)))
      (agent-repl--log ws "install-commit-emoji-hook: destination={destination} exists={exists}"
        :destination (prin1-to-string dest) :exists (file-exists-p dest))
      (when (file-exists-p dest)
        (let ((backup (concat dest ".bak")))
          (copy-file dest backup t)
          (agent-repl--log ws
            "install-commit-emoji-hook: existing-hook-backed-up source={source} backup={backup}"
            :source (prin1-to-string dest) :backup (prin1-to-string backup))
          (message "Backed up existing hook to %s" backup)))
      (copy-file agent-repl--prepare-commit-msg-hook-source dest t)
      (set-file-modes dest #o755)
      (agent-repl--log ws
        "install-commit-emoji-hook: installed source={source} destination={destination} mode={mode}"
        :source (prin1-to-string agent-repl--prepare-commit-msg-hook-source) :destination (prin1-to-string dest) :mode (format "%o" #o755))
      (message "Installed prepare-commit-msg hook to %s" dest))))

(provide 'agent-repl-emoji)

;;; emoji.el ends here
