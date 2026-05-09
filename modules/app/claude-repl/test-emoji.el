;;; test-emoji.el --- ERT tests for claude-repl emoji.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -batch -Q -l ert -l test-emoji.el -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET test-emoji.el RET
;;   M-x ert RET t RET

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: commit-type-from-message ----

(ert-deftest claude-repl-test-commit-type-feat ()
  "commit-type-from-message should return `feat' for feat(claude-repl) messages."
  (should (eq (claude-repl--commit-type-from-message "feat(claude-repl): add emoji support")
              'feat)))

(ert-deftest claude-repl-test-commit-type-fix ()
  "commit-type-from-message should return `fix' for fix(claude-repl) messages."
  (should (eq (claude-repl--commit-type-from-message "fix(claude-repl): resolve crash")
              'fix)))

(ert-deftest claude-repl-test-commit-type-refactor ()
  "commit-type-from-message should return `refactor' for refactor(...) messages."
  (should (eq (claude-repl--commit-type-from-message "refactor(claude-repl): simplify logic")
              'refactor)))

(ert-deftest claude-repl-test-commit-type-test ()
  "commit-type-from-message should return `test' for test(...) messages."
  (should (eq (claude-repl--commit-type-from-message "test(claude-repl): add coverage")
              'test)))

(ert-deftest claude-repl-test-commit-type-docs ()
  "commit-type-from-message should return `docs' for docs(...) messages."
  (should (eq (claude-repl--commit-type-from-message "docs(claude-repl): update README")
              'docs)))

(ert-deftest claude-repl-test-commit-type-style ()
  "commit-type-from-message should return `style' for style(...) messages."
  (should (eq (claude-repl--commit-type-from-message "style(claude-repl): fix indentation")
              'style)))

(ert-deftest claude-repl-test-commit-type-perf ()
  "commit-type-from-message should return `perf' for perf(...) messages."
  (should (eq (claude-repl--commit-type-from-message "perf(claude-repl): optimize loop")
              'perf)))

(ert-deftest claude-repl-test-commit-type-chore ()
  "commit-type-from-message should return `chore' for chore(...) messages."
  (should (eq (claude-repl--commit-type-from-message "chore(claude-repl): update deps")
              'chore)))

(ert-deftest claude-repl-test-commit-type-ci ()
  "commit-type-from-message should return `ci' for ci(...) messages."
  (should (eq (claude-repl--commit-type-from-message "ci(claude-repl): fix pipeline")
              'ci)))

(ert-deftest claude-repl-test-commit-type-unknown-returns-wildcard ()
  "commit-type-from-message should return `wildcard' for unknown types."
  (should (eq (claude-repl--commit-type-from-message "banana(claude-repl): something")
              'wildcard)))

(ert-deftest claude-repl-test-commit-type-no-parens-returns-wildcard ()
  "commit-type-from-message should return `wildcard' when no parens present."
  (should (eq (claude-repl--commit-type-from-message "just a message")
              'wildcard)))

(ert-deftest claude-repl-test-commit-type-empty-string ()
  "commit-type-from-message should return `wildcard' for empty string."
  (should (eq (claude-repl--commit-type-from-message "")
              'wildcard)))

(ert-deftest claude-repl-test-commit-type-uppercase-returns-wildcard ()
  "commit-type-from-message should return `wildcard' for uppercase types."
  (should (eq (claude-repl--commit-type-from-message "FEAT(claude-repl): loud commit")
              'wildcard)))

;;;; ---- Tests: random-commit-emoji ----

(ert-deftest claude-repl-test-random-emoji-returns-string ()
  "random-commit-emoji should return a non-empty string."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let ((emoji (claude-repl--random-commit-emoji 'feat)))
      (should (stringp emoji))
      (should (> (length emoji) 0)))))

(ert-deftest claude-repl-test-random-emoji-nil-type-uses-wildcard ()
  "random-commit-emoji with nil should use the wildcard pool."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let ((emoji (claude-repl--random-commit-emoji nil)))
      (should (stringp emoji))
      (should (member emoji (cdr (assq 'wildcard claude-repl--emoji-categories)))))))

(ert-deftest claude-repl-test-random-emoji-unknown-type-uses-wildcard ()
  "random-commit-emoji with unknown type should fall back to wildcard."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let ((emoji (claude-repl--random-commit-emoji 'nonexistent)))
      (should (member emoji (cdr (assq 'wildcard claude-repl--emoji-categories)))))))

(ert-deftest claude-repl-test-random-emoji-feat-from-feat-pool ()
  "random-commit-emoji with feat (no wildcard chance) should pick from feat pool."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let ((emoji (claude-repl--random-commit-emoji 'feat)))
      (should (member emoji (cdr (assq 'feat claude-repl--emoji-categories)))))))

(ert-deftest claude-repl-test-random-emoji-fix-from-fix-pool ()
  "random-commit-emoji with fix (no wildcard chance) should pick from fix pool."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let ((emoji (claude-repl--random-commit-emoji 'fix)))
      (should (member emoji (cdr (assq 'fix claude-repl--emoji-categories)))))))

(ert-deftest claude-repl-test-random-emoji-100-percent-wildcard ()
  "random-commit-emoji with 100% wildcard chance always uses wildcard pool."
  (let ((claude-repl-emoji-wildcard-chance 100))
    (dotimes (_ 10)
      (let ((emoji (claude-repl--random-commit-emoji 'feat)))
        (should (member emoji (cdr (assq 'wildcard claude-repl--emoji-categories))))))))

(ert-deftest claude-repl-test-random-emoji-variety ()
  "random-commit-emoji should produce more than one unique emoji over many calls."
  (let ((claude-repl-emoji-wildcard-chance 0)
        (seen (make-hash-table :test 'equal)))
    (dotimes (_ 50)
      (puthash (claude-repl--random-commit-emoji 'wildcard) t seen))
    (should (> (hash-table-count seen) 1))))

;;;; ---- Tests: message-has-emoji-prefix-p ----

(ert-deftest claude-repl-test-emoji-prefix-detected ()
  "message-has-emoji-prefix-p should detect emoji at start of message."
  (should (claude-repl--message-has-emoji-prefix-p "🚀 feat(claude-repl): something")))

(ert-deftest claude-repl-test-no-emoji-prefix ()
  "message-has-emoji-prefix-p should return nil for ASCII-prefixed messages."
  (should-not (claude-repl--message-has-emoji-prefix-p "feat(claude-repl): something")))

(ert-deftest claude-repl-test-emoji-prefix-empty-string ()
  "message-has-emoji-prefix-p should return nil for empty string."
  (should-not (claude-repl--message-has-emoji-prefix-p "")))

(ert-deftest claude-repl-test-emoji-prefix-space-start ()
  "message-has-emoji-prefix-p should return nil when message starts with space."
  (should-not (claude-repl--message-has-emoji-prefix-p " feat(claude-repl): something")))

;;;; ---- Tests: emoji-prefix-commit-message ----
;;
;; The prefixer treats the active git branch as the conventional-commit
;; scope.  Tests pass an explicit BRANCH-OVERRIDE so they don't have to
;; mutate the actual git checkout.

(ert-deftest claude-repl-test-prefix-injects-emoji-after-colon ()
  "Prefix function injects an emoji between `: ' and the description,
producing `<type>(<branch>): <emoji> <description>'."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let ((result (claude-repl--emoji-prefix-commit-message
                   "feat(my-branch): add feature" "my-branch")))
      (should (string-match-p "^feat(my-branch): " result))
      (should (string-match-p "^feat(my-branch): [^[:ascii:]]" result))
      (should (string-suffix-p " add feature" result)))))

(ert-deftest claude-repl-test-prefix-preserves-original-description ()
  "Prefix function preserves the description text verbatim after the emoji."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let ((result (claude-repl--emoji-prefix-commit-message
                   "fix(my-branch): fix bug" "my-branch")))
      (should (string-suffix-p " fix bug" result)))))

(ert-deftest claude-repl-test-prefix-skips-when-scope-not-branch ()
  "Prefix function does not modify commits whose scope is not the branch."
  (let ((msg "feat(other-scope): add feature"))
    (should (equal (claude-repl--emoji-prefix-commit-message msg "my-branch") msg))))

(ert-deftest claude-repl-test-prefix-skips-when-description-already-emojified ()
  "Prefix function leaves a message alone when the description already
starts with a non-ASCII char (idempotent under repeated runs)."
  (let ((msg "feat(my-branch): 🚀 add feature"))
    (should (equal (claude-repl--emoji-prefix-commit-message msg "my-branch") msg))))

(ert-deftest claude-repl-test-prefix-skips-empty-string ()
  "Prefix function returns empty string unchanged."
  (should (equal (claude-repl--emoji-prefix-commit-message "" "my-branch") "")))

(ert-deftest claude-repl-test-prefix-skips-when-branch-unresolvable ()
  "Prefix function returns the message unchanged when the branch lookup
yields nil (e.g. detached HEAD or non-repo cwd)."
  (cl-letf (((symbol-function 'claude-repl--current-branch) (lambda () nil)))
    (let ((msg "feat(my-branch): add feature"))
      (should (equal (claude-repl--emoji-prefix-commit-message msg) msg)))))

(ert-deftest claude-repl-test-prefix-uses-correct-type-pool ()
  "Prefixed message for fix(<branch>) draws from the fix emoji pool
(no wildcard injection at chance=0)."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let* ((result (claude-repl--emoji-prefix-commit-message
                    "fix(my-branch): fix bug" "my-branch"))
           ;; Format: "fix(my-branch): EMOJI fix bug" — the third
           ;; whitespace-separated token is the emoji.
           (emoji (nth 1 (split-string result ": "))))
      (setq emoji (car (split-string emoji " ")))
      (should (member emoji (cdr (assq 'fix claude-repl--emoji-categories)))))))

(ert-deftest claude-repl-test-prefix-branch-with-special-chars ()
  "Prefix function tolerates branches with regex-meta characters
(slashes, dots, etc.) — `regexp-quote' is applied to the branch."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let* ((branch "feat/foo.bar")
           (msg (concat "feat(" branch "): something"))
           (result (claude-repl--emoji-prefix-commit-message msg branch)))
      (should (string-prefix-p (concat "feat(" branch "): ") result))
      (should-not (equal result msg)))))

;;;; ---- Tests: emoji-categories constant ----

(ert-deftest claude-repl-test-all-categories-present ()
  "All expected commit type categories should be present."
  (dolist (type '(feat fix refactor test docs style perf chore ci wildcard))
    (should (assq type claude-repl--emoji-categories))))

(ert-deftest claude-repl-test-all-categories-non-empty ()
  "Every category should have at least one emoji."
  (dolist (pair claude-repl--emoji-categories)
    (should (> (length (cdr pair)) 0))))

(ert-deftest claude-repl-test-all-emojis-are-strings ()
  "Every emoji in every category should be a string."
  (dolist (pair claude-repl--emoji-categories)
    (dolist (emoji (cdr pair))
      (should (stringp emoji)))))

(ert-deftest claude-repl-test-all-emojis-are-non-ascii ()
  "Every emoji should start with a non-ASCII character."
  (dolist (pair claude-repl--emoji-categories)
    (dolist (emoji (cdr pair))
      (should (> (aref emoji 0) 127)))))

(ert-deftest claude-repl-test-wildcard-pool-large ()
  "Wildcard pool should have significantly more emojis than typed pools."
  (let ((wildcard-count (length (cdr (assq 'wildcard claude-repl--emoji-categories))))
        (feat-count (length (cdr (assq 'feat claude-repl--emoji-categories)))))
    (should (> wildcard-count feat-count))))

;;;; ---- Tests: commit-prefix-regex ----

(ert-deftest claude-repl-test-commit-prefix-regex-matches-branch-scope ()
  "commit-prefix-regex matches `<type>(<branch>): <rest>' and captures
type + rest."
  (let ((rx (claude-repl--commit-prefix-regex "my-branch")))
    (should (string-match rx "feat(my-branch): hello"))
    (should (equal (match-string 1 "feat(my-branch): hello") "feat"))
    (should (equal (match-string 2 "feat(my-branch): hello") "hello"))))

(ert-deftest claude-repl-test-commit-prefix-regex-no-match-other-scope ()
  "commit-prefix-regex does not match a different scope."
  (let ((rx (claude-repl--commit-prefix-regex "my-branch")))
    (should-not (string-match-p rx "feat(other): hello"))))

(ert-deftest claude-repl-test-commit-prefix-regex-quotes-special-chars ()
  "commit-prefix-regex regex-quotes the branch so meta characters in
the branch name are matched literally."
  (let ((rx (claude-repl--commit-prefix-regex "feat/foo.bar")))
    (should (string-match-p rx "fix(feat/foo.bar): hi"))
    ;; The literal `.' must NOT be treated as wildcard — a different
    ;; char in that slot should NOT match.
    (should-not (string-match-p rx "fix(feat/fooXbar): hi"))))

;;;; ---- Tests: magit-emoji-setup ----

(ert-deftest claude-repl-test-magit-setup-inserts-emoji-after-colon ()
  "magit-emoji-setup rewrites the buffer to inject an emoji after `: '."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (cl-letf (((symbol-function 'claude-repl--current-branch)
               (lambda () "my-branch")))
      (with-temp-buffer
        (insert "feat(my-branch): new feature")
        (claude-repl--magit-emoji-setup)
        (let ((result (buffer-string)))
          (should (string-match-p "^feat(my-branch): " result))
          (should (string-match-p "^feat(my-branch): [^[:ascii:]]" result))
          (should (string-suffix-p " new feature" result)))))))

(ert-deftest claude-repl-test-magit-setup-skips-non-branch-scope ()
  "magit-emoji-setup leaves the buffer alone when scope isn't the branch."
  (cl-letf (((symbol-function 'claude-repl--current-branch)
             (lambda () "my-branch")))
    (with-temp-buffer
      (insert "feat(other): new feature")
      (claude-repl--magit-emoji-setup)
      (should (equal (buffer-string) "feat(other): new feature")))))

(ert-deftest claude-repl-test-magit-setup-skips-already-emojified-description ()
  "magit-emoji-setup is idempotent — a description already starting with
a non-ASCII char is left unchanged."
  (cl-letf (((symbol-function 'claude-repl--current-branch)
             (lambda () "my-branch")))
    (with-temp-buffer
      (insert "feat(my-branch): 🚀 new feature")
      (claude-repl--magit-emoji-setup)
      (should (equal (buffer-string) "feat(my-branch): 🚀 new feature")))))

(ert-deftest claude-repl-test-magit-setup-empty-buffer ()
  "magit-emoji-setup is a no-op for empty buffers."
  (cl-letf (((symbol-function 'claude-repl--current-branch)
             (lambda () "my-branch")))
    (with-temp-buffer
      (claude-repl--magit-emoji-setup)
      (should (equal (buffer-string) "")))))

(ert-deftest claude-repl-test-magit-setup-skips-when-branch-nil ()
  "magit-emoji-setup is a no-op when the branch lookup yields nil."
  (cl-letf (((symbol-function 'claude-repl--current-branch) (lambda () nil)))
    (with-temp-buffer
      (insert "feat(my-branch): new feature")
      (claude-repl--magit-emoji-setup)
      (should (equal (buffer-string) "feat(my-branch): new feature")))))

;;;; ---- Tests: git-hooks-dir ----

(ert-deftest claude-repl-test-git-hooks-dir-returns-path ()
  "git-hooks-dir should return a path ending in /hooks when in a git repo."
  (let ((result (claude-repl--git-hooks-dir)))
    (should (stringp result))
    (should (string-suffix-p "hooks" result))))

(ert-deftest claude-repl-test-git-hooks-dir-outside-repo ()
  "git-hooks-dir should return nil outside a git repository."
  (let ((default-directory "/tmp/"))
    (should-not (claude-repl--git-hooks-dir))))

;;;; ---- Tests: hook source constant ----

(ert-deftest claude-repl-test-hook-source-path-set ()
  "prepare-commit-msg hook source path should be set."
  (should (stringp claude-repl--prepare-commit-msg-hook-source)))

;;;; ---- Tests: defcustom ----

(ert-deftest claude-repl-test-wildcard-chance-default ()
  "Default wildcard chance should be 30."
  (should (equal (default-value 'claude-repl-emoji-wildcard-chance) 30)))

(ert-deftest claude-repl-test-lookback-default ()
  "Default emoji lookback should be 50."
  (should (equal (default-value 'claude-repl-emoji-lookback) 50)))

;;;; ---- Tests: filter-pool ----

(ert-deftest claude-repl-test-filter-pool-removes-excluded ()
  "filter-pool should remove every emoji listed in EXCLUDE."
  (should (equal (claude-repl--filter-pool '("a" "b" "c") '("b"))
                 '("a" "c"))))

(ert-deftest claude-repl-test-filter-pool-empty-exclude ()
  "filter-pool with empty exclude should return the pool unchanged."
  (should (equal (claude-repl--filter-pool '("a" "b" "c") '())
                 '("a" "b" "c"))))

(ert-deftest claude-repl-test-filter-pool-all-excluded ()
  "filter-pool that excludes the whole pool should return nil."
  (should-not (claude-repl--filter-pool '("a" "b") '("a" "b"))))

;;;; ---- Tests: random-commit-emoji with recents ----

(ert-deftest claude-repl-test-random-emoji-excludes-recents ()
  "random-commit-emoji should never return an emoji listed in RECENTS."
  (let* ((claude-repl-emoji-wildcard-chance 0)
         (feat-pool (cdr (assq 'feat claude-repl--emoji-categories)))
         (excluded (list (car feat-pool) (cadr feat-pool))))
    (dotimes (_ 30)
      (let ((emoji (claude-repl--random-commit-emoji 'feat excluded)))
        (should-not (member emoji excluded))))))

(ert-deftest claude-repl-test-random-emoji-typed-exhausted-falls-back-to-wildcard ()
  "When the typed pool is fully excluded, random-commit-emoji should use the wildcard pool."
  (let* ((claude-repl-emoji-wildcard-chance 0)
         (feat-pool (cdr (assq 'feat claude-repl--emoji-categories)))
         (wildcard-pool (cdr (assq 'wildcard claude-repl--emoji-categories))))
    (let ((emoji (claude-repl--random-commit-emoji 'feat feat-pool)))
      (should (member emoji wildcard-pool))
      (should-not (member emoji feat-pool)))))

(ert-deftest claude-repl-test-random-emoji-final-fallback-when-all-exhausted ()
  "When typed and wildcard pools are both exhausted by recents, fall back to full wildcard."
  (let* ((claude-repl-emoji-wildcard-chance 0)
         (feat-pool (cdr (assq 'feat claude-repl--emoji-categories)))
         (wildcard-pool (cdr (assq 'wildcard claude-repl--emoji-categories)))
         (all (append feat-pool wildcard-pool)))
    (let ((emoji (claude-repl--random-commit-emoji 'feat all)))
      (should (member emoji wildcard-pool)))))

(ert-deftest claude-repl-test-random-emoji-nil-recents-unchanged ()
  "Passing nil RECENTS should leave behavior identical to the old single-arg call."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let ((emoji (claude-repl--random-commit-emoji 'fix nil)))
      (should (member emoji (cdr (assq 'fix claude-repl--emoji-categories)))))))

;;;; ---- Tests: recent-commit-emojis ----

(ert-deftest claude-repl-test-recent-emojis-extracts-leading-emoji ()
  "recent-commit-emojis should pull the leading emoji token from each subject line."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (concat "🩹 fix(claude-repl): one\n"
                       "✨ feat(claude-repl): two\n"
                       "🐛 fix(claude-repl): three\n"))))
    (should (equal (claude-repl--recent-commit-emojis 50)
                   '("🩹" "✨" "🐛")))))

(ert-deftest claude-repl-test-recent-emojis-skips-ascii-prefix ()
  "recent-commit-emojis should drop entries whose first token is plain ASCII."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd)
               (concat "tweak: bump version\n"
                       "✨ feat(claude-repl): real one\n"))))
    (should (equal (claude-repl--recent-commit-emojis 50)
                   '("✨")))))

(ert-deftest claude-repl-test-recent-emojis-empty-output ()
  "recent-commit-emojis should return nil for empty git output."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) "")))
    (should-not (claude-repl--recent-commit-emojis 50))))

(ert-deftest claude-repl-test-recent-emojis-handles-error ()
  "recent-commit-emojis should return nil if git invocation errors out."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_cmd) (error "git not found"))))
    (should-not (claude-repl--recent-commit-emojis 50))))

;;;; ---- Tests: emoji-prefix-commit-message integration with recents ----

(ert-deftest claude-repl-test-prefix-excludes-recent-emojis ()
  "End-to-end: prefix should not pick an emoji that appears in recent commits."
  (let* ((claude-repl-emoji-wildcard-chance 0)
         (feat-pool (cdr (assq 'feat claude-repl--emoji-categories)))
         ;; Block all but one feat emoji via recents.
         (allowed (car (last feat-pool)))
         (recents (butlast feat-pool)))
    (cl-letf (((symbol-function 'claude-repl--recent-commit-emojis)
               (lambda (&optional _n) recents)))
      (dotimes (_ 20)
        (let* ((result (claude-repl--emoji-prefix-commit-message
                        "feat(claude-repl): something"))
               (emoji (car (split-string result " "))))
          (should (equal emoji allowed)))))))

(provide 'test-emoji)

;;; test-emoji.el ends here
