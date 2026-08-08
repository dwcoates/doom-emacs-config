;;; test-prompts.el --- ERT tests for prompts.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the file-backed automatic prompt loader: substitution,
;; header stripping, and the loud failures that stand in for a
;; fallback copy.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-prompts.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(defmacro agent-repl-test--with-prompt (name content &rest body)
  "Stage a prompt file NAME containing CONTENT and run BODY.
`agent-repl-prompts-dir' is rebound to a throwaway directory for the
duration, which is the same knob a user pointing the loader elsewhere
would turn."
  (declare (indent 2))
  `(let ((agent-repl-prompts-dir (make-temp-file "agent-repl-prompts" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ,name agent-repl-prompts-dir)
             (insert ,content))
           ,@body)
       (delete-directory agent-repl-prompts-dir t))))

;;;; ---- Tests: substitution ----

(ert-deftest agent-repl-test-prompt-substitutes-a-placeholder ()
  "`agent-repl--prompt' replaces {{name}} with its supplied value."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: {{who}} -->\nhello {{who}}\n"
    (should (equal (agent-repl--prompt "p.md" '(("who" . "world")))
                   "hello world"))))

(ert-deftest agent-repl-test-prompt-substitutes-every-occurrence ()
  "A placeholder repeated in the file is replaced at every occurrence."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: {{x}} -->\n{{x}} and {{x}}\n"
    (should (equal (agent-repl--prompt "p.md" '(("x" . "a"))) "a and a"))))

(ert-deftest agent-repl-test-prompt-substitutes-value-literally ()
  "Backslashes in a substituted VALUE are not read as match references."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: {{v}} -->\n{{v}}\n"
    (should (equal (agent-repl--prompt "p.md" '(("v" . "a\\1&b"))) "a\\1&b"))))

(ert-deftest agent-repl-test-prompt-carries-braced-text-in-a-value ()
  "A user's own text may contain {{braces}} without failing the send."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: {{m}} -->\nsay {{m}}\n"
    (should (equal (agent-repl--prompt "p.md" '(("m" . "what is {{foo}}?")))
                   "say what is {{foo}}?"))))

;;;; ---- Tests: header and terminator handling ----

(ert-deftest agent-repl-test-prompt-strips-the-header-comment ()
  "The leading `<!-- used by: ... -->' header never reaches the agent."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: none -->\nbody\n"
    (should (equal (agent-repl--prompt "p.md") "body"))))

(ert-deftest agent-repl-test-prompt-keeps-an-in-body-comment ()
  "Only a LEADING comment is a header; prose may contain others."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: none -->\nsay <!-- this --> aloud\n"
    (should (equal (agent-repl--prompt "p.md") "say <!-- this --> aloud"))))

(ert-deftest agent-repl-test-prompt-drops-exactly-one-trailing-newline ()
  "Two terminators mean the prompt itself ends with one."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: none -->\nbody\n\n"
    (should (equal (agent-repl--prompt "p.md") "body\n"))))

(ert-deftest agent-repl-test-prompt-preserves-leading-blank-lines ()
  "Leading whitespace is content: suffix prompts open with blank lines."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: none -->\n\n\nbody\n"
    (should (equal (agent-repl--prompt "p.md") "\n\nbody"))))

;;;; ---- Tests: loud failures ----

(ert-deftest agent-repl-test-prompt-errors-on-a-missing-file ()
  "A missing prompt file signals instead of falling back to a copy."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: none -->\nbody\n"
    (should-error (agent-repl--prompt "absent.md") :type 'error)))

(ert-deftest agent-repl-test-prompt-missing-file-error-names-the-path ()
  "The missing-file error names the path so it can be fixed."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: none -->\nbody\n"
    (should (string-match-p
             "absent\\.md"
             (cadr (should-error (agent-repl--prompt "absent.md") :type 'error))))))

(ert-deftest agent-repl-test-prompt-errors-on-an-unsubstitutable-placeholder ()
  "A placeholder the call site cannot fill signals rather than shipping."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: {{who}} -->\nhello {{whom}}\n"
    (should-error (agent-repl--prompt "p.md" '(("who" . "world"))) :type 'error)))

(ert-deftest agent-repl-test-prompt-placeholder-error-lists-the-expected ()
  "The placeholder error lists what the call site DOES supply."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: {{a}} -->\n{{typo}}\n"
    (should (string-match-p
             "{{a}}"
             (cadr (should-error (agent-repl--prompt "p.md" '(("a" . "1")))
                                 :type 'error))))))

(ert-deftest agent-repl-test-prompt-errors-when-a-value-would-be-dropped ()
  "A supplied placeholder the file no longer mentions signals.
Silently dropping it would send a prompt missing a fact the agent
needs, which is precisely the quiet degradation this loader forbids."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: {{a}} -->\nonly {{a}}\n"
    (should-error (agent-repl--prompt "p.md" '(("a" . "1") ("b" . "2")))
                  :type 'error)))

(ert-deftest agent-repl-test-prompt-errors-on-a-header-only-file ()
  "A file that is nothing but its header would submit an empty turn."
  (agent-repl-test--with-prompt "p.md" "<!-- used by: test; placeholders: none -->\n"
    (should-error (agent-repl--prompt "p.md") :type 'error)))

;;;; ---- Tests: directory resolution ----

(ert-deftest agent-repl-test-prompts-dir-sits-beside-the-module ()
  "`agent-repl-prompts-dir' names the module's own prompts directory."
  (should (file-directory-p agent-repl-prompts-dir))
  (should (equal (file-name-nondirectory
                  (directory-file-name agent-repl-prompts-dir))
                 "prompts")))

(ert-deftest agent-repl-test-prompt-file-joins-onto-the-prompts-dir ()
  "`agent-repl--prompt-file' resolves a basename inside the directory."
  (let ((agent-repl-prompts-dir "/tmp/p"))
    (should (equal (agent-repl--prompt-file "x.md") "/tmp/p/x.md"))))

(provide 'test-prompts)
;;; test-prompts.el ends here
