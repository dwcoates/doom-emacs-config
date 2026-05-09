;;; emoji.el --- random emoji prefixes for claude-repl commits -*- lexical-binding: t; -*-

;;; Commentary:

;; Automatically prefix claude-repl commit messages with a random emoji.
;; Integrates with Magit via `git-commit-setup-hook' for interactive commits,
;; and provides a `prepare-commit-msg' hook script for git CLI commits
;; (e.g. from Claude Code agents).
;;
;; The emoji is chosen based on the conventional-commit type (feat, fix, etc.)
;; with a wildcard injection chance for extra variety.

;;; Code:

(defconst claude-repl--emoji-categories
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

(defun claude-repl--current-branch ()
  "Return the current git branch name (string), or nil if unresolvable.
Returns nil when the working tree is not in a git repo, when HEAD is
detached, or when the branch name is empty.  Shells out to git so the
function is available without requiring magit to be loaded."
  (let ((out (string-trim
              (shell-command-to-string
               "git rev-parse --abbrev-ref HEAD 2>/dev/null"))))
    (cond ((string-empty-p out) nil)
          ((string= out "HEAD") nil)
          (t out))))

(defun claude-repl--commit-prefix-regex (branch)
  "Return a regex matching `<type>(<BRANCH>): <rest>' at start of message.
Captures the type as group 1 and the rest-of-line as group 2.  BRANCH
is regex-quoted so branch names with `.', `/', `+' etc. are handled
literally."
  (concat "^\\([a-z]+\\)("
          (regexp-quote branch)
          "): \\(.*\\)"))

(defcustom claude-repl-emoji-wildcard-chance 30
  "Percentage chance (0-100) of using a wildcard emoji instead of a typed one.
Higher values produce more variety at the cost of semantic relevance."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-emoji-lookback 50
  "Number of recent conventional-commit lines to scan for already-used emojis.
Emojis used in the last LOOKBACK matching commits are excluded from the
candidate pool, deterministically guaranteeing variety from the git history.
When the typed pool is fully exhausted by recents, falls back to the
wildcard pool (also minus recents).

Scans commits whose subject matches `<type>(<scope>):' (any scope) so
both the legacy `<emoji> type(scope):' format and the current
`type(scope): <emoji> ...' format contribute to the recents set."
  :type 'integer
  :group 'claude-repl)

(defun claude-repl--commit-type-from-message (msg)
  "Extract the conventional-commit type keyword from MSG.
Returns a symbol like `feat', `fix', etc., or `wildcard' if no type matches.
Tolerant of any scope: looks at the leading `<type>(' substring."
  (if (string-match "^\\([a-z]+\\)(" msg)
      (let ((type (intern (match-string 1 msg))))
        (if (assq type claude-repl--emoji-categories)
            type
          'wildcard))
    'wildcard))

(defun claude-repl--extract-commit-emoji (line)
  "Return the emoji token in conventional-commit subject LINE, or nil.
Recognizes both formats:
  - Legacy: `<emoji> type(scope): description'  → first token.
  - Current: `type(scope): <emoji> description' → first token after `: '.
The token must start with a non-ASCII character to qualify."
  (let ((token
         (cond
          ;; New format: type(scope): EMOJI rest
          ((string-match "^[a-z]+([^)]+): \\([^ ]+\\)" line)
           (match-string 1 line))
          ;; Legacy format: EMOJI type(scope): rest — first token.
          (t (car (split-string line " " t))))))
    (when (and token
               (> (length token) 0)
               (> (aref token 0) 127))
      token)))

(defun claude-repl--recent-commit-emojis (&optional lookback)
  "Return list of emojis used by the last LOOKBACK conventional commits.
Defaults to `claude-repl-emoji-lookback'.  Returns nil when not in a
git repository or on any git error.  Newest commit first.

Scans commits whose subject matches `<type>(<scope>):' (any scope) so
both legacy and current formats contribute — the post-rebase change
to a branch-as-scope convention means filtering by a literal scope
string would miss commits authored under the new format."
  (let* ((n (or lookback claude-repl-emoji-lookback))
         ;; Match any conventional-commit subject (any scope) via -E.
         (cmd (format "git log -n %d -E --grep='^[a-z]+\\(.+\\):' --format=%%s 2>/dev/null"
                      n))
         (output (ignore-errors (shell-command-to-string cmd)))
         (lines (and (stringp output) (split-string output "\n" t)))
         (emojis '()))
    (dolist (line lines)
      (when-let ((emoji (claude-repl--extract-commit-emoji line)))
        (push emoji emojis)))
    (nreverse emojis)))

(defun claude-repl--filter-pool (pool exclude)
  "Return POOL with any emoji string in EXCLUDE removed."
  (cl-remove-if (lambda (e) (member e exclude)) pool))

(defun claude-repl--random-commit-emoji (&optional commit-type recents)
  "Return a random emoji for COMMIT-TYPE (a symbol), avoiding RECENTS.
When COMMIT-TYPE is nil or not in `claude-repl--emoji-categories', uses `wildcard'.
Injects a wildcard emoji `claude-repl-emoji-wildcard-chance' percent of the time
regardless of type, to maximize variety.
RECENTS, when non-nil, is a list of emoji strings to exclude from the pool;
if exclusion empties the typed pool, falls back to the wildcard pool minus
RECENTS, then to the full wildcard pool as a final guarantee of progress."
  (let* ((type (or commit-type 'wildcard))
         (use-wildcard (< (random 100) claude-repl-emoji-wildcard-chance))
         (effective-type (if (or use-wildcard
                                (not (assq type claude-repl--emoji-categories)))
                            'wildcard
                          type))
         (candidates (claude-repl--filter-pool
                      (cdr (assq effective-type claude-repl--emoji-categories))
                      recents)))
    (when (null candidates)
      (setq candidates (claude-repl--filter-pool
                        (cdr (assq 'wildcard claude-repl--emoji-categories))
                        recents)))
    (when (null candidates)
      (setq candidates (cdr (assq 'wildcard claude-repl--emoji-categories))))
    (nth (random (length candidates)) candidates)))

(defun claude-repl--description-has-emoji-prefix-p (description)
  "Return non-nil if DESCRIPTION starts with a non-ASCII char (likely emoji)."
  (and (> (length description) 0)
       (> (aref description 0) 127)))

;; Backward-compat alias — older tests still call this name.
(defalias 'claude-repl--message-has-emoji-prefix-p
  'claude-repl--description-has-emoji-prefix-p)

(defun claude-repl--emoji-prefix-commit-message (msg &optional branch-override)
  "Inject a random emoji into MSG when its scope matches the current branch.
Expected MSG shape: `<type>(<branch>): <description>'.  When the scope
matches the active branch (or BRANCH-OVERRIDE), the result becomes
`<type>(<branch>): <emoji> <description>'.  When the description
already starts with a non-ASCII char, MSG is returned unchanged
(idempotent).  When the scope does not match the branch, MSG is
returned unchanged.  When the branch cannot be resolved (no git repo
/ detached HEAD), MSG is returned unchanged.

Excludes emojis used in the last `claude-repl-emoji-lookback' matching
commits to avoid back-to-back repeats (variety guarantee carried over
from the prior auto-emoji iteration).

BRANCH-OVERRIDE lets callers (mainly tests) inject a fixed branch
without going through `git rev-parse'."
  (let ((branch (or branch-override (claude-repl--current-branch))))
    (if (or (null branch) (string-empty-p branch))
        msg
      (let* ((rx (claude-repl--commit-prefix-regex branch)))
        (if (not (string-match rx msg))
            msg
          (let ((type-str (match-string 1 msg))
                (description (match-string 2 msg))
                (rest (substring msg (match-end 0))))
            (if (claude-repl--description-has-emoji-prefix-p description)
                msg
              (let* ((type (claude-repl--commit-type-from-message msg))
                     (recents (claude-repl--recent-commit-emojis))
                     (emoji (claude-repl--random-commit-emoji type recents)))
                (concat type-str "(" branch "): "
                        emoji " " description rest)))))))))

;;; Magit integration

(defun claude-repl--magit-emoji-setup ()
  "Inject a random emoji into the commit-message buffer's first line.
Intended for `git-commit-setup-hook'.  Acts only when the buffer's
first line matches `<type>(<current-branch>): <description>' and the
description does not already start with a non-ASCII character.  The
emoji is inserted between `: ' and the description, producing
`<type>(<branch>): <emoji> <description>'.  Variety/lookback handling
is delegated to `--emoji-prefix-commit-message' (which threads the
recent-emojis list through `--random-commit-emoji')."
  (let ((branch (claude-repl--current-branch)))
    (when (and branch (not (string-empty-p branch)))
      (let* ((msg (buffer-string))
             (replaced (claude-repl--emoji-prefix-commit-message msg branch)))
        (unless (equal replaced msg)
          (let ((point-pos (point)))
            (erase-buffer)
            (insert replaced)
            (goto-char (min point-pos (point-max)))))))))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook #'claude-repl--magit-emoji-setup))

;;; Git hook installation

(defconst claude-repl--prepare-commit-msg-hook-source
  (expand-file-name "hooks/prepare-commit-msg-emoji.sh"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the checked-in prepare-commit-msg hook script.")

(defun claude-repl--git-hooks-dir ()
  "Return the git hooks directory for the current repository.
Works for both normal repos (.git/hooks) and worktrees."
  (let ((git-dir (string-trim (shell-command-to-string "git rev-parse --git-common-dir 2>/dev/null"))))
    (when (and (not (string-empty-p git-dir))
               (not (string-prefix-p "fatal" git-dir)))
      (expand-file-name "hooks" git-dir))))

;;;###autoload
(defun claude-repl-install-commit-emoji-hook ()
  "Install the prepare-commit-msg hook for automatic emoji prefixes.
Copies the hook script to the current repository's git hooks directory.
If an existing prepare-commit-msg hook is found, backs it up first."
  (interactive)
  (let ((hooks-dir (claude-repl--git-hooks-dir)))
    (unless hooks-dir
      (user-error "Not inside a git repository"))
    (unless (file-exists-p claude-repl--prepare-commit-msg-hook-source)
      (user-error "Hook source not found: %s" claude-repl--prepare-commit-msg-hook-source))
    (let ((dest (expand-file-name "prepare-commit-msg" hooks-dir)))
      (when (file-exists-p dest)
        (let ((backup (concat dest ".bak")))
          (copy-file dest backup t)
          (message "Backed up existing hook to %s" backup)))
      (copy-file claude-repl--prepare-commit-msg-hook-source dest t)
      (set-file-modes dest #o755)
      (message "Installed prepare-commit-msg hook to %s" dest))))

(provide 'claude-repl-emoji)

;;; emoji.el ends here
