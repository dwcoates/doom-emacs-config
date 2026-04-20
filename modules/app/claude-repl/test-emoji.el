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

(ert-deftest claude-repl-test-prefix-adds-emoji-to-claude-repl-commit ()
  "emoji-prefix-commit-message should add an emoji to claude-repl commits."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let ((result (claude-repl--emoji-prefix-commit-message "feat(claude-repl): add feature")))
      (should (claude-repl--message-has-emoji-prefix-p result))
      (should (string-match-p "(claude-repl)" result)))))

(ert-deftest claude-repl-test-prefix-preserves-original-message ()
  "emoji-prefix-commit-message should preserve the full original message after emoji."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let ((result (claude-repl--emoji-prefix-commit-message "fix(claude-repl): fix bug")))
      (should (string-match-p "fix(claude-repl): fix bug$" result)))))

(ert-deftest claude-repl-test-prefix-skips-non-claude-repl ()
  "emoji-prefix-commit-message should not modify non-claude-repl commits."
  (let ((msg "feat(other-module): add feature"))
    (should (equal (claude-repl--emoji-prefix-commit-message msg) msg))))

(ert-deftest claude-repl-test-prefix-skips-already-prefixed ()
  "emoji-prefix-commit-message should not double-prefix messages."
  (let ((msg "🚀 feat(claude-repl): add feature"))
    (should (equal (claude-repl--emoji-prefix-commit-message msg) msg))))

(ert-deftest claude-repl-test-prefix-skips-empty-string ()
  "emoji-prefix-commit-message should return empty string unchanged."
  (should (equal (claude-repl--emoji-prefix-commit-message "") "")))

(ert-deftest claude-repl-test-prefix-format-emoji-space-message ()
  "Prefixed message should be exactly: EMOJI SPACE ORIGINAL."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let* ((original "feat(claude-repl): do thing")
           (result (claude-repl--emoji-prefix-commit-message original)))
      ;; Result should end with a space followed by the original message
      (should (string-suffix-p (concat " " original) result))
      ;; The emoji prefix (everything before the space+original) should be non-empty
      (let ((prefix-len (- (length result) (length original) 1)))
        (should (> prefix-len 0))))))

(ert-deftest claude-repl-test-prefix-uses-correct-type-pool ()
  "Prefixed message for fix(claude-repl) should use fix emoji pool (no wildcard)."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (let* ((result (claude-repl--emoji-prefix-commit-message "fix(claude-repl): fix bug"))
           (emoji (car (split-string result " "))))
      (should (member emoji (cdr (assq 'fix claude-repl--emoji-categories)))))))

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

;;;; ---- Tests: emoji-scope-re ----

(ert-deftest claude-repl-test-scope-re-matches-claude-repl ()
  "Scope regexp should match messages containing (claude-repl)."
  (should (string-match-p claude-repl--emoji-scope-re "feat(claude-repl): something")))

(ert-deftest claude-repl-test-scope-re-no-match-other-scope ()
  "Scope regexp should not match other scopes."
  (should-not (string-match-p claude-repl--emoji-scope-re "feat(other): something")))

(ert-deftest claude-repl-test-scope-re-no-match-plain ()
  "Scope regexp should not match plain messages."
  (should-not (string-match-p claude-repl--emoji-scope-re "just a message")))

;;;; ---- Tests: magit-emoji-setup ----

(ert-deftest claude-repl-test-magit-setup-inserts-emoji ()
  "magit-emoji-setup should insert emoji at start of buffer for claude-repl commits."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (with-temp-buffer
      (insert "feat(claude-repl): new feature")
      (claude-repl--magit-emoji-setup)
      (let ((result (buffer-string)))
        (should (claude-repl--message-has-emoji-prefix-p result))
        (should (string-match-p "(claude-repl)" result))))))

(ert-deftest claude-repl-test-magit-setup-skips-non-claude-repl ()
  "magit-emoji-setup should not modify non-claude-repl commit buffers."
  (with-temp-buffer
    (insert "feat(other): new feature")
    (claude-repl--magit-emoji-setup)
    (should (equal (buffer-string) "feat(other): new feature"))))

(ert-deftest claude-repl-test-magit-setup-skips-existing-emoji ()
  "magit-emoji-setup should not double-prefix messages."
  (with-temp-buffer
    (insert "🚀 feat(claude-repl): new feature")
    (claude-repl--magit-emoji-setup)
    (should (equal (buffer-string) "🚀 feat(claude-repl): new feature"))))

(ert-deftest claude-repl-test-magit-setup-handles-leading-whitespace ()
  "magit-emoji-setup should handle leading whitespace in buffer."
  (let ((claude-repl-emoji-wildcard-chance 0))
    (with-temp-buffer
      (insert "\n  feat(claude-repl): new feature")
      (claude-repl--magit-emoji-setup)
      (let ((result (buffer-string)))
        (should (claude-repl--message-has-emoji-prefix-p result))))))

(ert-deftest claude-repl-test-magit-setup-empty-buffer ()
  "magit-emoji-setup should be a no-op for empty buffers."
  (with-temp-buffer
    (claude-repl--magit-emoji-setup)
    (should (equal (buffer-string) ""))))

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

(provide 'test-emoji)

;;; test-emoji.el ends here
