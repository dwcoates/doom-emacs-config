;;; test-workspace-completion.el --- ERT tests for workspace-completion.el -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS GUARDS: that a word in the composer that begins a part of an
;; open workspace's name completes to it, and only to open workspaces' names;
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

(defun agent-repl-test-workspace-completion--offered (capf)
  "Every name CAPF's table holds, in its order."
  (all-completions "" (nth 2 capf)))

(ert-deftest agent-repl-test-workspace-completion-offers-the-names-a-word-begins ()
  "A word that begins an open workspace's name is completed over the names it begins."
  (agent-repl-test-workspace-completion--with "please ask agent-repl-ski"
      '("agent-repl-skill-migration" "agent-repl-skins" "parser-fix")
    (let ((capf (agent-repl-workspace-completion-at-point)))
      (should (equal (buffer-substring (nth 0 capf) (nth 1 capf)) "agent-repl-ski"))
      (should (equal (sort (agent-repl-test-workspace-completion--offered capf) #'string<)
                     '("agent-repl-skill-migration" "agent-repl-skins"))))))

(ert-deftest agent-repl-test-workspace-completion-decides-its-own-fall-through ()
  "The completion carries no `:exclusive': its nil is the fall-through."
  (agent-repl-test-workspace-completion--with "agent" '("agent-repl-skill-migration")
    (should-not (plist-member (nthcdr 3 (agent-repl-workspace-completion-at-point)) :exclusive))))

(ert-deftest agent-repl-test-workspace-completion-offers-a-name-a-later-part-begins ()
  "A word beginning a part past the name's first is completed to the name."
  (agent-repl-test-workspace-completion--with "skill" '("agent-repl-skill-migration" "parser-fix")
    (should (equal (agent-repl-test-workspace-completion--offered (agent-repl-workspace-completion-at-point))
                   '("agent-repl-skill-migration")))))

(ert-deftest agent-repl-test-workspace-completion-offers-a-word-spanning-parts ()
  "A word running across a separator, from a part's start, is completed."
  (agent-repl-test-workspace-completion--with "repl-ski" '("agent-repl-skill-migration")
    (should (equal (agent-repl-test-workspace-completion--offered (agent-repl-workspace-completion-at-point))
                   '("agent-repl-skill-migration")))))

(ert-deftest agent-repl-test-workspace-completion-splits-at-underscore-and-dot ()
  "`_' and `.' split a name into parts as `-' does."
  (agent-repl-test-workspace-completion--with "two" '("one_two" "one.two" "onetwo")
    (should (equal (agent-repl-test-workspace-completion--offered (agent-repl-workspace-completion-at-point)) '("one_two" "one.two")))))

(ert-deftest agent-repl-test-workspace-completion-ignores-a-word-inside-a-part ()
  "A word inside a part, not at its start, offers nothing."
  (agent-repl-test-workspace-completion--with "on" '("agent-repl-skill-migration")
    (should-not (agent-repl-workspace-completion-at-point))))

(ert-deftest agent-repl-test-workspace-completion-offers-a-later-part-past-the-wrapper ()
  "A later part's match survives Emacs's capf wrapper, as company runs it."
  (agent-repl-test-workspace-completion--with "skill" '("agent-repl-skill-migration")
    (let ((capf (cdr (completion--capf-wrapper #'agent-repl-workspace-completion-at-point 'all))))
      (should (equal (all-completions "skill" (nth 2 capf)) nil))
      (should (equal (car (completion-all-completions
                           "skill" (nth 2 capf) nil 5
                           (completion-metadata "skill" (nth 2 capf) nil)))
                     "agent-repl-skill-migration")))))

(ert-deftest agent-repl-test-workspace-completion-table-states-its-category ()
  "The table names its own category, given the substring style."
  (let ((table (agent-repl--workspace-completion-table '("a"))))
    (should (eq (completion-metadata-get (completion-metadata "" table nil) 'category)
                'agent-repl-workspace-name))
    (should (equal (alist-get 'agent-repl-workspace-name completion-category-defaults)
                   '((styles substring basic))))))

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
