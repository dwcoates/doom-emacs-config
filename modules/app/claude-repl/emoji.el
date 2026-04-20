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

(defconst claude-repl--emoji-scope-re "(claude-repl)"
  "Regexp matching the claude-repl conventional-commit scope.
Only commit messages containing this scope receive emoji prefixes.")

(defcustom claude-repl-emoji-wildcard-chance 30
  "Percentage chance (0-100) of using a wildcard emoji instead of a typed one.
Higher values produce more variety at the cost of semantic relevance."
  :type 'integer
  :group 'claude-repl)

(defun claude-repl--commit-type-from-message (msg)
  "Extract the conventional-commit type keyword from MSG.
Returns a symbol like `feat', `fix', etc., or `wildcard' if no type matches."
  (if (string-match "^\\([a-z]+\\)(" msg)
      (let ((type (intern (match-string 1 msg))))
        (if (assq type claude-repl--emoji-categories)
            type
          'wildcard))
    'wildcard))

(defun claude-repl--random-commit-emoji (&optional commit-type)
  "Return a random emoji for COMMIT-TYPE (a symbol).
When COMMIT-TYPE is nil or not in `claude-repl--emoji-categories', uses `wildcard'.
Injects a wildcard emoji `claude-repl-emoji-wildcard-chance' percent of the time
regardless of type, to maximize variety."
  (let* ((type (or commit-type 'wildcard))
         (use-wildcard (< (random 100) claude-repl-emoji-wildcard-chance))
         (effective-type (if (or use-wildcard
                                (not (assq type claude-repl--emoji-categories)))
                            'wildcard
                          type))
         (candidates (cdr (assq effective-type claude-repl--emoji-categories))))
    (nth (random (length candidates)) candidates)))

(defun claude-repl--message-has-emoji-prefix-p (msg)
  "Return non-nil if MSG already starts with a non-ASCII character (likely emoji)."
  (and (> (length msg) 0)
       (> (aref msg 0) 127)))

(defun claude-repl--emoji-prefix-commit-message (msg)
  "Prepend a random emoji to MSG if it contains the claude-repl scope.
Only prefixes when MSG matches `claude-repl--emoji-scope-re' and does not
already have an emoji prefix.  Returns the (possibly modified) message string."
  (if (and (string-match-p claude-repl--emoji-scope-re msg)
           (not (claude-repl--message-has-emoji-prefix-p msg)))
      (let* ((type (claude-repl--commit-type-from-message msg))
             (emoji (claude-repl--random-commit-emoji type)))
        (concat emoji " " msg))
    msg))

;;; Magit integration

(defun claude-repl--magit-emoji-setup ()
  "Insert a random emoji prefix into the commit message buffer.
Intended for `git-commit-setup-hook'.  Only acts when the initial
message template contains the claude-repl scope and no emoji is already present."
  (let ((msg (string-trim (buffer-string))))
    (when (and (string-match-p claude-repl--emoji-scope-re msg)
               (not (claude-repl--message-has-emoji-prefix-p msg)))
      (goto-char (point-min))
      (let* ((type (claude-repl--commit-type-from-message msg))
             (emoji (claude-repl--random-commit-emoji type)))
        (insert emoji " ")))))

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
