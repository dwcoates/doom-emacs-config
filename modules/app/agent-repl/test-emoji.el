;;; test-emoji.el --- ERT tests for agent-repl emoji.el -*- lexical-binding: t; -*-

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

(ert-deftest agent-repl-test-commit-type-feat ()
  "commit-type-from-message should return `feat' for feat(agent-repl) messages."
  (should (eq (agent-repl--commit-type-from-message "feat(agent-repl): add emoji support")
              'feat)))

(ert-deftest agent-repl-test-commit-type-fix ()
  "commit-type-from-message should return `fix' for fix(agent-repl) messages."
  (should (eq (agent-repl--commit-type-from-message "fix(agent-repl): resolve crash")
              'fix)))

(ert-deftest agent-repl-test-commit-type-refactor ()
  "commit-type-from-message should return `refactor' for refactor(...) messages."
  (should (eq (agent-repl--commit-type-from-message "refactor(agent-repl): simplify logic")
              'refactor)))

(ert-deftest agent-repl-test-commit-type-test ()
  "commit-type-from-message should return `test' for test(...) messages."
  (should (eq (agent-repl--commit-type-from-message "test(agent-repl): add coverage")
              'test)))

(ert-deftest agent-repl-test-commit-type-docs ()
  "commit-type-from-message should return `docs' for docs(...) messages."
  (should (eq (agent-repl--commit-type-from-message "docs(agent-repl): update README")
              'docs)))

(ert-deftest agent-repl-test-commit-type-style ()
  "commit-type-from-message should return `style' for style(...) messages."
  (should (eq (agent-repl--commit-type-from-message "style(agent-repl): fix indentation")
              'style)))

(ert-deftest agent-repl-test-commit-type-perf ()
  "commit-type-from-message should return `perf' for perf(...) messages."
  (should (eq (agent-repl--commit-type-from-message "perf(agent-repl): optimize loop")
              'perf)))

(ert-deftest agent-repl-test-commit-type-chore ()
  "commit-type-from-message should return `chore' for chore(...) messages."
  (should (eq (agent-repl--commit-type-from-message "chore(agent-repl): update deps")
              'chore)))

(ert-deftest agent-repl-test-commit-type-ci ()
  "commit-type-from-message should return `ci' for ci(...) messages."
  (should (eq (agent-repl--commit-type-from-message "ci(agent-repl): fix pipeline")
              'ci)))

(ert-deftest agent-repl-test-commit-type-unknown-returns-wildcard ()
  "commit-type-from-message should return `wildcard' for unknown types."
  (should (eq (agent-repl--commit-type-from-message "banana(agent-repl): something")
              'wildcard)))

(ert-deftest agent-repl-test-commit-type-no-parens-returns-wildcard ()
  "commit-type-from-message should return `wildcard' when no parens present."
  (should (eq (agent-repl--commit-type-from-message "just a message")
              'wildcard)))

(ert-deftest agent-repl-test-commit-type-empty-string ()
  "commit-type-from-message should return `wildcard' for empty string."
  (should (eq (agent-repl--commit-type-from-message "")
              'wildcard)))

(ert-deftest agent-repl-test-commit-type-uppercase-returns-wildcard ()
  "commit-type-from-message should return `wildcard' for uppercase types."
  (should (eq (agent-repl--commit-type-from-message "FEAT(agent-repl): loud commit")
              'wildcard)))

;;;; ---- Tests: random-commit-emoji ----

(ert-deftest agent-repl-test-random-emoji-returns-string ()
  "random-commit-emoji should return a non-empty string."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let ((emoji (agent-repl--random-commit-emoji 'feat)))
      (should (stringp emoji))
      (should (> (length emoji) 0)))))

(ert-deftest agent-repl-test-random-emoji-nil-type-uses-wildcard ()
  "random-commit-emoji with nil should use the wildcard pool."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let ((emoji (agent-repl--random-commit-emoji nil)))
      (should (stringp emoji))
      (should (member emoji (cdr (assq 'wildcard agent-repl--emoji-categories)))))))

(ert-deftest agent-repl-test-random-emoji-unknown-type-uses-wildcard ()
  "random-commit-emoji with unknown type should fall back to wildcard."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let ((emoji (agent-repl--random-commit-emoji 'nonexistent)))
      (should (member emoji (cdr (assq 'wildcard agent-repl--emoji-categories)))))))

(ert-deftest agent-repl-test-random-emoji-feat-from-feat-pool ()
  "random-commit-emoji with feat (no wildcard chance) should pick from feat pool."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let ((emoji (agent-repl--random-commit-emoji 'feat)))
      (should (member emoji (cdr (assq 'feat agent-repl--emoji-categories)))))))

(ert-deftest agent-repl-test-random-emoji-fix-from-fix-pool ()
  "random-commit-emoji with fix (no wildcard chance) should pick from fix pool."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let ((emoji (agent-repl--random-commit-emoji 'fix)))
      (should (member emoji (cdr (assq 'fix agent-repl--emoji-categories)))))))

(ert-deftest agent-repl-test-random-emoji-100-percent-wildcard ()
  "random-commit-emoji with 100% wildcard chance always uses wildcard pool."
  (let ((agent-repl-emoji-wildcard-chance 100))
    (dotimes (_ 10)
      (let ((emoji (agent-repl--random-commit-emoji 'feat)))
        (should (member emoji (cdr (assq 'wildcard agent-repl--emoji-categories))))))))

(ert-deftest agent-repl-test-random-emoji-variety ()
  "random-commit-emoji should produce more than one unique emoji over many calls."
  (let ((agent-repl-emoji-wildcard-chance 0)
        (seen (make-hash-table :test 'equal)))
    (dotimes (_ 50)
      (puthash (agent-repl--random-commit-emoji 'wildcard) t seen))
    (should (> (hash-table-count seen) 1))))

;;;; ---- Tests: message-has-emoji-prefix-p ----

(ert-deftest agent-repl-test-emoji-prefix-detected ()
  "message-has-emoji-prefix-p should detect emoji at start of message."
  (should (agent-repl--message-has-emoji-prefix-p "🚀 feat(agent-repl): something")))

(ert-deftest agent-repl-test-no-emoji-prefix ()
  "message-has-emoji-prefix-p should return nil for ASCII-prefixed messages."
  (should-not (agent-repl--message-has-emoji-prefix-p "feat(agent-repl): something")))

(ert-deftest agent-repl-test-emoji-prefix-empty-string ()
  "message-has-emoji-prefix-p should return nil for empty string."
  (should-not (agent-repl--message-has-emoji-prefix-p "")))

(ert-deftest agent-repl-test-emoji-prefix-space-start ()
  "message-has-emoji-prefix-p should return nil when message starts with space."
  (should-not (agent-repl--message-has-emoji-prefix-p " feat(agent-repl): something")))

;;;; ---- Tests: emoji-prefix-commit-message ----
;;
;; The prefixer treats the active git branch as the conventional-commit
;; scope.  Tests pass an explicit BRANCH-OVERRIDE so they don't have to
;; mutate the actual git checkout.

(ert-deftest agent-repl-test-prefix-injects-emoji-after-colon ()
  "Prefix function injects an emoji between `: ' and the description,
producing `<type>(<branch>): <emoji> <description>'."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let ((result (agent-repl--emoji-prefix-commit-message
                   "feat(my-branch): add feature" "my-branch")))
      (should (string-match-p "^feat(my-branch): " result))
      (should (string-match-p "^feat(my-branch): [^[:ascii:]]" result))
      (should (string-suffix-p " add feature" result)))))

(ert-deftest agent-repl-test-prefix-preserves-original-description ()
  "Prefix function preserves the description text verbatim after the emoji."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let ((result (agent-repl--emoji-prefix-commit-message
                   "fix(my-branch): fix bug" "my-branch")))
      (should (string-suffix-p " fix bug" result)))))

(ert-deftest agent-repl-test-prefix-skips-when-scope-not-branch ()
  "Prefix function does not modify commits whose scope is not the branch."
  (let ((msg "feat(other-scope): add feature"))
    (should (equal (agent-repl--emoji-prefix-commit-message msg "my-branch") msg))))

(ert-deftest agent-repl-test-prefix-skips-when-description-already-emojified ()
  "Prefix function leaves a message alone when the description already
starts with a non-ASCII char (idempotent under repeated runs)."
  (let ((msg "feat(my-branch): 🚀 add feature"))
    (should (equal (agent-repl--emoji-prefix-commit-message msg "my-branch") msg))))

(ert-deftest agent-repl-test-prefix-skips-empty-string ()
  "Prefix function returns empty string unchanged."
  (should (equal (agent-repl--emoji-prefix-commit-message "" "my-branch") "")))

(ert-deftest agent-repl-test-prefix-skips-when-branch-unresolvable ()
  "Prefix function returns the message unchanged when the branch lookup
yields nil (e.g. detached HEAD or non-repo cwd)."
  (cl-letf (((symbol-function 'agent-repl--current-branch) (lambda () nil)))
    (let ((msg "feat(my-branch): add feature"))
      (should (equal (agent-repl--emoji-prefix-commit-message msg) msg)))))

(ert-deftest agent-repl-test-prefix-uses-correct-type-pool ()
  "Prefixed message for fix(<branch>) draws from the fix emoji pool
(no wildcard injection at chance=0)."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let* ((result (agent-repl--emoji-prefix-commit-message
                    "fix(my-branch): fix bug" "my-branch"))
           ;; Format: "fix(my-branch): EMOJI fix bug" — the third
           ;; whitespace-separated token is the emoji.
           (emoji (nth 1 (split-string result ": "))))
      (setq emoji (car (split-string emoji " ")))
      (should (member emoji (cdr (assq 'fix agent-repl--emoji-categories)))))))

(ert-deftest agent-repl-test-prefix-branch-with-special-chars ()
  "Prefix function tolerates branches with regex-meta characters
(slashes, dots, etc.) — `regexp-quote' is applied to the branch."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let* ((branch "feat/foo.bar")
           (msg (concat "feat(" branch "): something"))
           (result (agent-repl--emoji-prefix-commit-message msg branch)))
      (should (string-prefix-p (concat "feat(" branch "): ") result))
      (should-not (equal result msg)))))

;;;; ---- Tests: no-scope branch injection ----

(ert-deftest agent-repl-test-prefix-injects-branch-when-scope-missing ()
  "When MSG lacks a scope, the active branch is injected and an emoji
is prepended to the description."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let ((result (agent-repl--emoji-prefix-commit-message
                   "feat: add feature" "my-branch")))
      (should (string-match-p "^feat(my-branch): " result))
      (should (string-match-p "^feat(my-branch): [^[:ascii:]]" result))
      (should (string-suffix-p " add feature" result)))))

(ert-deftest agent-repl-test-prefix-no-scope-uses-correct-type-pool ()
  "No-scope `fix: ...' draws an emoji from the fix pool (no wildcard at chance=0)."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let* ((result (agent-repl--emoji-prefix-commit-message
                    "fix: bug" "my-branch"))
           (after-colon (cadr (split-string result ": ")))
           (emoji (car (split-string after-colon " "))))
      (should (member emoji (cdr (assq 'fix agent-repl--emoji-categories)))))))

(ert-deftest agent-repl-test-prefix-no-scope-unknown-type-uses-wildcard ()
  "An unknown type (e.g. `infra:') still gets branch+emoji injected;
the emoji comes from the wildcard pool."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let* ((result (agent-repl--emoji-prefix-commit-message
                    "infra: bump runner" "my-branch"))
           (after-colon (cadr (split-string result ": ")))
           (emoji (car (split-string after-colon " "))))
      (should (string-prefix-p "infra(my-branch): " result))
      (should (string-suffix-p " bump runner" result))
      (should (member emoji (cdr (assq 'wildcard agent-repl--emoji-categories)))))))

(ert-deftest agent-repl-test-prefix-no-scope-with-existing-emoji-injects-branch-only ()
  "When the description already starts with a non-ASCII char and the
scope is missing, the branch is still injected but the emoji is
preserved verbatim (no second emoji prepended)."
  (let ((msg "feat: 🚀 add feature"))
    (should (equal (agent-repl--emoji-prefix-commit-message msg "my-branch")
                   "feat(my-branch): 🚀 add feature"))))

(ert-deftest agent-repl-test-prefix-non-branch-scope-still-noop ()
  "A scope that is present but does not match the branch is left alone
(respects the author's explicit scope choice) — regression check that
the new no-scope path didn't accidentally rewrite this case."
  (let ((msg "feat(other-scope): add feature"))
    (should (equal (agent-repl--emoji-prefix-commit-message msg "my-branch") msg))))

(ert-deftest agent-repl-test-prefix-no-scope-skips-when-branch-unresolvable ()
  "No-scope path is also a no-op when the branch lookup yields nil."
  (cl-letf (((symbol-function 'agent-repl--current-branch) (lambda () nil)))
    (let ((msg "feat: add feature"))
      (should (equal (agent-repl--emoji-prefix-commit-message msg) msg)))))

(ert-deftest agent-repl-test-prefix-no-scope-preserves-body ()
  "Multi-line MSG: only the first line is rewritten; the body is preserved."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let* ((msg "feat: add feature\n\nLonger body line.\nAnother body line.")
           (result (agent-repl--emoji-prefix-commit-message msg "my-branch")))
      (should (string-match-p "^feat(my-branch): [^[:ascii:]]" result))
      (should (string-suffix-p "\n\nLonger body line.\nAnother body line." result)))))

(ert-deftest agent-repl-test-prefix-no-scope-special-char-branch ()
  "Branch names with slashes/dots are injected as scope literally."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let* ((branch "DWC/feat.bar")
           (result (agent-repl--emoji-prefix-commit-message
                    "feat: ship it" branch)))
      (should (string-prefix-p (concat "feat(" branch "): ") result))
      (should (string-suffix-p " ship it" result)))))

(ert-deftest agent-repl-test-prefix-no-prefix-at-all-noop ()
  "Plain message with no `<type>:' header is left unchanged."
  (let ((msg "just some random text"))
    (should (equal (agent-repl--emoji-prefix-commit-message msg "my-branch") msg))))

;;;; ---- Tests: magit-emoji-setup, no-scope path ----

(ert-deftest agent-repl-test-magit-setup-injects-branch-when-scope-missing ()
  "magit-emoji-setup injects the branch as scope when the buffer has
`<type>: <description>' (no scope)."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (cl-letf (((symbol-function 'agent-repl--current-branch)
               (lambda () "my-branch")))
      (with-temp-buffer
        (insert "feat: new feature")
        (agent-repl--magit-emoji-setup)
        (let ((result (buffer-string)))
          (should (string-match-p "^feat(my-branch): [^[:ascii:]]" result))
          (should (string-suffix-p " new feature" result)))))))

;;;; ---- Tests: emoji-categories constant ----

(ert-deftest agent-repl-test-all-categories-present ()
  "All expected commit type categories should be present."
  (dolist (type '(feat fix refactor test docs style perf chore ci wildcard))
    (should (assq type agent-repl--emoji-categories))))

(ert-deftest agent-repl-test-all-categories-non-empty ()
  "Every category should have at least one emoji."
  (dolist (pair agent-repl--emoji-categories)
    (should (> (length (cdr pair)) 0))))

(ert-deftest agent-repl-test-all-emojis-are-strings ()
  "Every emoji in every category should be a string."
  (dolist (pair agent-repl--emoji-categories)
    (dolist (emoji (cdr pair))
      (should (stringp emoji)))))

(ert-deftest agent-repl-test-all-emojis-are-non-ascii ()
  "Every emoji should start with a non-ASCII character."
  (dolist (pair agent-repl--emoji-categories)
    (dolist (emoji (cdr pair))
      (should (> (aref emoji 0) 127)))))

(ert-deftest agent-repl-test-wildcard-pool-large ()
  "Wildcard pool should have significantly more emojis than typed pools."
  (let ((wildcard-count (length (cdr (assq 'wildcard agent-repl--emoji-categories))))
        (feat-count (length (cdr (assq 'feat agent-repl--emoji-categories)))))
    (should (> wildcard-count feat-count))))

;;;; ---- Tests: commit-prefix-regex ----

(ert-deftest agent-repl-test-commit-prefix-regex-matches-branch-scope ()
  "commit-prefix-regex matches `<type>(<branch>): <rest>' and captures
type + rest."
  (let ((rx (agent-repl--commit-prefix-regex "my-branch")))
    (should (string-match rx "feat(my-branch): hello"))
    (should (equal (match-string 1 "feat(my-branch): hello") "feat"))
    (should (equal (match-string 2 "feat(my-branch): hello") "hello"))))

(ert-deftest agent-repl-test-commit-prefix-regex-no-match-other-scope ()
  "commit-prefix-regex does not match a different scope."
  (let ((rx (agent-repl--commit-prefix-regex "my-branch")))
    (should-not (string-match-p rx "feat(other): hello"))))

(ert-deftest agent-repl-test-commit-prefix-regex-quotes-special-chars ()
  "commit-prefix-regex regex-quotes the branch so meta characters in
the branch name are matched literally."
  (let ((rx (agent-repl--commit-prefix-regex "feat/foo.bar")))
    (should (string-match-p rx "fix(feat/foo.bar): hi"))
    ;; The literal `.' must NOT be treated as wildcard — a different
    ;; char in that slot should NOT match.
    (should-not (string-match-p rx "fix(feat/fooXbar): hi"))))

;;;; ---- Tests: magit-emoji-setup ----

(ert-deftest agent-repl-test-magit-setup-inserts-emoji-after-colon ()
  "magit-emoji-setup rewrites the buffer to inject an emoji after `: '."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (cl-letf (((symbol-function 'agent-repl--current-branch)
               (lambda () "my-branch")))
      (with-temp-buffer
        (insert "feat(my-branch): new feature")
        (agent-repl--magit-emoji-setup)
        (let ((result (buffer-string)))
          (should (string-match-p "^feat(my-branch): " result))
          (should (string-match-p "^feat(my-branch): [^[:ascii:]]" result))
          (should (string-suffix-p " new feature" result)))))))

(ert-deftest agent-repl-test-magit-setup-skips-non-branch-scope ()
  "magit-emoji-setup leaves the buffer alone when scope isn't the branch."
  (cl-letf (((symbol-function 'agent-repl--current-branch)
             (lambda () "my-branch")))
    (with-temp-buffer
      (insert "feat(other): new feature")
      (agent-repl--magit-emoji-setup)
      (should (equal (buffer-string) "feat(other): new feature")))))

(ert-deftest agent-repl-test-magit-setup-skips-already-emojified-description ()
  "magit-emoji-setup is idempotent — a description already starting with
a non-ASCII char is left unchanged."
  (cl-letf (((symbol-function 'agent-repl--current-branch)
             (lambda () "my-branch")))
    (with-temp-buffer
      (insert "feat(my-branch): 🚀 new feature")
      (agent-repl--magit-emoji-setup)
      (should (equal (buffer-string) "feat(my-branch): 🚀 new feature")))))

(ert-deftest agent-repl-test-magit-setup-empty-buffer ()
  "magit-emoji-setup is a no-op for empty buffers."
  (cl-letf (((symbol-function 'agent-repl--current-branch)
             (lambda () "my-branch")))
    (with-temp-buffer
      (agent-repl--magit-emoji-setup)
      (should (equal (buffer-string) "")))))

(ert-deftest agent-repl-test-magit-setup-skips-when-branch-nil ()
  "magit-emoji-setup is a no-op when the branch lookup yields nil."
  (cl-letf (((symbol-function 'agent-repl--current-branch) (lambda () nil)))
    (with-temp-buffer
      (insert "feat(my-branch): new feature")
      (agent-repl--magit-emoji-setup)
      (should (equal (buffer-string) "feat(my-branch): new feature")))))

;;;; ---- Tests: git-hooks-dir ----
;;
;; `agent-repl--git-hooks-dir' routes through
;; `agent-repl--git-string-quiet' (the external boundary); tests mock
;; that wrapper instead of invoking real git.

(ert-deftest agent-repl-test-git-hooks-dir-returns-path ()
  "git-hooks-dir should return PATH/hooks when git emits a git-dir."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest args)
               (should (equal args '("rev-parse" "--git-common-dir")))
               "/tmp/repo/.git")))
    (let ((result (agent-repl--git-hooks-dir)))
      (should (stringp result))
      (should (string-suffix-p "hooks" result))
      (should (equal result "/tmp/repo/.git/hooks")))))

(ert-deftest agent-repl-test-git-hooks-dir-outside-repo ()
  "git-hooks-dir should return nil when git emits an empty string
\(quiet-mode for an outside-repo failure)."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args) "")))
    (should-not (agent-repl--git-hooks-dir))))

(ert-deftest agent-repl-test-git-hooks-dir-fatal-output ()
  "git-hooks-dir should return nil when git's stderr leaks a `fatal:'
prefix (defensive — the `-quiet' wrapper suppresses stderr, but the
guard remains for older callers / wrapper changes)."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args) "fatal: not a git repository")))
    (should-not (agent-repl--git-hooks-dir))))

;;;; ---- Tests: hook source constant ----

(ert-deftest agent-repl-test-hook-source-path-set ()
  "prepare-commit-msg hook source path should be set."
  (should (stringp agent-repl--prepare-commit-msg-hook-source)))

;;;; ---- Tests: defcustom ----

(ert-deftest agent-repl-test-wildcard-chance-default ()
  "Default wildcard chance should be 30."
  (should (equal (default-value 'agent-repl-emoji-wildcard-chance) 30)))

(ert-deftest agent-repl-test-lookback-default ()
  "Default emoji lookback should be 50."
  (should (equal (default-value 'agent-repl-emoji-lookback) 50)))

;;;; ---- Tests: filter-pool ----

(ert-deftest agent-repl-test-filter-pool-removes-excluded ()
  "filter-pool should remove every emoji listed in EXCLUDE."
  (should (equal (agent-repl--filter-pool '("a" "b" "c") '("b"))
                 '("a" "c"))))

(ert-deftest agent-repl-test-filter-pool-empty-exclude ()
  "filter-pool with empty exclude should return the pool unchanged."
  (should (equal (agent-repl--filter-pool '("a" "b" "c") '())
                 '("a" "b" "c"))))

(ert-deftest agent-repl-test-filter-pool-all-excluded ()
  "filter-pool that excludes the whole pool should return nil."
  (should-not (agent-repl--filter-pool '("a" "b") '("a" "b"))))

;;;; ---- Tests: random-commit-emoji with recents ----

(ert-deftest agent-repl-test-random-emoji-excludes-recents ()
  "random-commit-emoji should never return an emoji listed in RECENTS."
  (let* ((agent-repl-emoji-wildcard-chance 0)
         (feat-pool (cdr (assq 'feat agent-repl--emoji-categories)))
         (excluded (list (car feat-pool) (cadr feat-pool))))
    (dotimes (_ 30)
      (let ((emoji (agent-repl--random-commit-emoji 'feat excluded)))
        (should-not (member emoji excluded))))))

(ert-deftest agent-repl-test-random-emoji-typed-exhausted-falls-back-to-wildcard ()
  "When the typed pool is fully excluded, random-commit-emoji should use the wildcard pool."
  (let* ((agent-repl-emoji-wildcard-chance 0)
         (feat-pool (cdr (assq 'feat agent-repl--emoji-categories)))
         (wildcard-pool (cdr (assq 'wildcard agent-repl--emoji-categories))))
    (let ((emoji (agent-repl--random-commit-emoji 'feat feat-pool)))
      (should (member emoji wildcard-pool))
      (should-not (member emoji feat-pool)))))

(ert-deftest agent-repl-test-random-emoji-final-fallback-when-all-exhausted ()
  "When typed and wildcard pools are both exhausted by recents, fall back to full wildcard."
  (let* ((agent-repl-emoji-wildcard-chance 0)
         (feat-pool (cdr (assq 'feat agent-repl--emoji-categories)))
         (wildcard-pool (cdr (assq 'wildcard agent-repl--emoji-categories)))
         (all (append feat-pool wildcard-pool)))
    (let ((emoji (agent-repl--random-commit-emoji 'feat all)))
      (should (member emoji wildcard-pool)))))

(ert-deftest agent-repl-test-random-emoji-nil-recents-unchanged ()
  "Passing nil RECENTS should leave behavior identical to the old single-arg call."
  (let ((agent-repl-emoji-wildcard-chance 0))
    (let ((emoji (agent-repl--random-commit-emoji 'fix nil)))
      (should (member emoji (cdr (assq 'fix agent-repl--emoji-categories)))))))

;;;; ---- Tests: recent-commit-emojis ----

(ert-deftest agent-repl-test-recent-emojis-extracts-leading-emoji ()
  "recent-commit-emojis should pull the leading emoji token from each subject line.
Stubs the registered external-boundary wrapper
`agent-repl--git-string-quiet' — the runtime guards installed by
test-helpers.el make a raw `shell-command-to-string' stub insufficient
(production code now goes through the wrapper, which would fire the
guard's UNMOCKED error if not `cl-letf'-ed)."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args)
               (concat "🩹 fix(agent-repl): one\n"
                       "✨ feat(agent-repl): two\n"
                       "🐛 fix(agent-repl): three"))))
    (should (equal (agent-repl--recent-commit-emojis 50)
                   '("🩹" "✨" "🐛")))))

(ert-deftest agent-repl-test-recent-emojis-skips-ascii-prefix ()
  "recent-commit-emojis should drop entries whose first token is plain ASCII."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args)
               (concat "tweak: bump version\n"
                       "✨ feat(agent-repl): real one"))))
    (should (equal (agent-repl--recent-commit-emojis 50)
                   '("✨")))))

(ert-deftest agent-repl-test-recent-emojis-empty-output ()
  "recent-commit-emojis should return nil for empty git output."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args) "")))
    (should-not (agent-repl--recent-commit-emojis 50))))

(ert-deftest agent-repl-test-recent-emojis-handles-error ()
  "recent-commit-emojis should return nil if the git wrapper errors out."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _args) (error "git not found"))))
    (should-not (agent-repl--recent-commit-emojis 50))))

;;;; ---- Tests: emoji-prefix-commit-message integration with recents ----

(ert-deftest agent-repl-test-prefix-excludes-recent-emojis ()
  "End-to-end: prefix should not pick an emoji that appears in recent commits."
  (let* ((agent-repl-emoji-wildcard-chance 0)
         (feat-pool (cdr (assq 'feat agent-repl--emoji-categories)))
         ;; Block all but one feat emoji via recents.
         (allowed (car (last feat-pool)))
         (recents (butlast feat-pool)))
    (cl-letf (((symbol-function 'agent-repl--recent-commit-emojis)
               (lambda (&optional _n) recents)))
      (dotimes (_ 20)
        ;; Pass the branch explicitly via BRANCH-OVERRIDE so the
        ;; pipeline never reaches the git boundary.  The current
        ;; convention emits `<type>(<branch>): <emoji> <description>',
        ;; so the emoji is the third whitespace-delimited token.
        (let* ((result (agent-repl--emoji-prefix-commit-message
                        "feat(agent-repl): something" "agent-repl"))
               (emoji (nth 1 (split-string result " "))))
          (should (equal emoji allowed)))))))

(provide 'test-emoji)

;;; test-emoji.el ends here
