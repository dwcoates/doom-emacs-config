;;; test-workspace-completion.el --- ERT tests for workspace-completion.el -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS GUARDS: that a word in the composer that begins an open
;; workspace's name completes to it, and only to open workspaces' names;
;; that a slash word is left to the command completion; and that a word
;; beginning no name falls through to the next completion function.  The
;; roster's open names are stubbed, so no daemon or roster push is needed.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

(defmacro agent-repl-test-workspace-completion--with (text names &rest body)
  "Run BODY in a buffer holding TEXT, point at its end, with NAMES open."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,text)
     (cl-letf (((symbol-function 'agent-repl-roster-open-workspace-names) (lambda () ,names)))
       ,@body)))

(ert-deftest agent-repl-test-workspace-completion-offers-the-names-a-word-begins ()
  "A word that begins an open workspace's name is completed over the names it begins."
  (agent-repl-test-workspace-completion--with "please ask agent-repl-ski"
      '("agent-repl-skill-migration" "agent-repl-skins" "parser-fix")
    (let ((capf (agent-repl-workspace-completion-at-point)))
      (should (equal (buffer-substring (nth 0 capf) (nth 1 capf)) "agent-repl-ski"))
      (should (equal (sort (copy-sequence (nth 2 capf)) #'string<)
                     '("agent-repl-skill-migration" "agent-repl-skins"))))))

(ert-deftest agent-repl-test-workspace-completion-is-non-exclusive ()
  "The completion lets the next completion function answer too."
  (agent-repl-test-workspace-completion--with "agent" '("agent-repl-skill-migration")
    (should (eq (plist-get (nthcdr 3 (agent-repl-workspace-completion-at-point)) :exclusive) 'no))))

(ert-deftest agent-repl-test-workspace-completion-matches-by-prefix-alone ()
  "A word inside a name, not at its start, offers nothing."
  (agent-repl-test-workspace-completion--with "skill" '("agent-repl-skill-migration")
    (should-not (agent-repl-workspace-completion-at-point))))

(ert-deftest agent-repl-test-workspace-completion-offers-nothing-with-no-workspace-open ()
  "With no open workspace there is nothing to complete."
  (agent-repl-test-workspace-completion--with "agent" nil
    (should-not (agent-repl-workspace-completion-at-point))))

(ert-deftest agent-repl-test-workspace-completion-leaves-a-slash-word-alone ()
  "A word that is part of a slash word is the command completion's."
  (agent-repl-test-workspace-completion--with "/agent-repl" '("agent-repl-skill-migration")
    (should-not (agent-repl-workspace-completion-at-point))))

(ert-deftest agent-repl-test-workspace-completion-offers-nothing-outside-a-word ()
  "Point after whitespace is in no name word."
  (agent-repl-test-workspace-completion--with "agent " '("agent-repl-skill-migration")
    (should-not (agent-repl-workspace-completion-at-point))))

(ert-deftest agent-repl-test-workspace-completion-bounds-stop-at-punctuation ()
  "A name word ends at punctuation names are not made of."
  (agent-repl-test-workspace-completion--with "(agent-repl" '("agent-repl-skill-migration")
    (let ((capf (agent-repl-workspace-completion-at-point)))
      (should (equal (buffer-substring (nth 0 capf) (nth 1 capf)) "agent-repl")))))

(ert-deftest agent-repl-test-workspace-completion-is-a-composer-completion ()
  "The composer installs the completion beside the slash-command one."
  (with-temp-buffer
    (agent-repl-input-mode)
    (should (memq #'agent-repl-workspace-completion-at-point completion-at-point-functions))
    (should (memq #'agent-repl-command-completion-at-point completion-at-point-functions))))

(provide 'test-workspace-completion)
;;; test-workspace-completion.el ends here
