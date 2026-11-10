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

(defmacro agent-repl-test-workspace-completion--with (text workspaces &rest body)
  "Run BODY in a buffer holding TEXT, point at its end, with WORKSPACES open.
Each of WORKSPACES is a name, open in no stated repository, or a
\(NAME . REPOSITORIES) element as `agent-repl-roster-open-workspaces' answers."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,text)
     (cl-letf (((symbol-function 'agent-repl-roster-open-workspaces)
                (lambda () (mapcar (lambda (ws) (if (consp ws) ws (list ws))) ,workspaces))))
       ,@body)))

(defun agent-repl-test-workspace-completion--annotations (capf)
  "Every name CAPF's table holds, paired with its annotation, in its order."
  (let ((annotate (plist-get (nthcdr 3 capf) :annotation-function)))
    (mapcar (lambda (name) (cons name (funcall annotate name)))
            (agent-repl-test-workspace-completion--offered capf))))

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

(ert-deftest agent-repl-test-workspace-completion-annotates-each-name-with-its-repository ()
  "Each offered name's annotation is its repository, in a column past the longest name."
  (agent-repl-test-workspace-completion--with "fix"
      '(("fix-a" "doom") ("fix-longer" "zettel"))
    (should (equal (mapcar (lambda (pair) (cons (car pair) (substring-no-properties (cdr pair))))
                           (agent-repl-test-workspace-completion--annotations
                            (agent-repl-workspace-completion-at-point)))
                   '(("fix-a" . "       doom") ("fix-longer" . "  zettel"))))))

(ert-deftest agent-repl-test-workspace-completion-aligns-past-the-offered-names-only ()
  "The column is measured over the names offered, not every open workspace."
  (agent-repl-test-workspace-completion--with "fix"
      '(("fix" "doom") ("a-much-longer-unoffered-name" "zettel"))
    (should (equal (substring-no-properties
                    (cdar (agent-repl-test-workspace-completion--annotations
                           (agent-repl-workspace-completion-at-point))))
                   "  doom"))))

(ert-deftest agent-repl-test-workspace-completion-draws-the-repository-dimmer ()
  "The repository is drawn in its own face, which inherits `shadow'."
  (agent-repl-test-workspace-completion--with "fix" '(("fix" "doom"))
    (let ((annotation (cdar (agent-repl-test-workspace-completion--annotations
                             (agent-repl-workspace-completion-at-point)))))
      (should (eq (get-text-property (string-search "doom" annotation) 'face annotation)
                  'agent-repl-workspace-completion-repository))
      (should (eq (face-attribute 'agent-repl-workspace-completion-repository :inherit)
                  'shadow)))))

(ert-deftest agent-repl-test-workspace-completion-joins-a-name-open-in-several-repositories ()
  "A name open in two repositories names both."
  (agent-repl-test-workspace-completion--with "fix" '(("fix" "doom" "zettel"))
    (should (equal (substring-no-properties
                    (cdar (agent-repl-test-workspace-completion--annotations
                           (agent-repl-workspace-completion-at-point))))
                   "  doom, zettel"))))

(ert-deftest agent-repl-test-workspace-completion-annotates-nothing-for-an-unstated-repository ()
  "A name whose repository the daemon did not state is annotated with nothing."
  (agent-repl-test-workspace-completion--with "fix" '("fix")
    (should (equal (agent-repl-test-workspace-completion--annotations
                    (agent-repl-workspace-completion-at-point))
                   '(("fix" . ""))))))

(ert-deftest agent-repl-test-workspace-completion-is-a-composer-completion ()
  "The composer installs the completion beside the slash-command one."
  (with-temp-buffer
    (agent-repl-input-mode)
    (should (memq #'agent-repl-workspace-completion-at-point completion-at-point-functions))
    (should (memq #'agent-repl-command-completion-at-point completion-at-point-functions))))

;;;; ---- Marking open workspaces' names ----

(defun agent-repl-test-workspace-completion--marked ()
  "Return the strings the current buffer marks as open workspaces' names, in order."
  (agent-repl-test--composer-marked 'agent-repl-workspace-completion-known))

(defmacro agent-repl-test-workspace-completion--marking (names &rest body)
  "Run BODY in a buffer marking on change, with NAMES open."
  (declare (indent 1))
  `(with-temp-buffer
     (cl-letf (((symbol-function 'agent-repl-roster-open-workspace-names) (lambda () ,names)))
       (add-hook 'after-change-functions #'agent-repl--workspace-completion-on-change nil t)
       ,@body)))

(ert-deftest agent-repl-test-workspace-completion-mark-a-typed-open-name ()
  "A typed word naming an open workspace is marked."
  (agent-repl-test-workspace-completion--marking '("parser-fix" "other")
    ;; Act
    (insert "ask parser-fix now")
    ;; Assert
    (should (equal (agent-repl-test-workspace-completion--marked) '("parser-fix")))))

(ert-deftest agent-repl-test-workspace-completion-mark-wears-the-known-commands-face ()
  "A marked workspace name wears the very face a known slash command wears."
  (agent-repl-test-workspace-completion--marking '("parser-fix")
    ;; Arrange
    (setq agent-repl--command-completion-catalog
          '((:name "merge-queue" :aliases nil :description nil :argument-hint nil)))
    (add-hook 'after-change-functions #'agent-repl--command-completion-on-change nil t)
    ;; Act
    (insert "/merge-queue parser-fix")
    ;; Assert
    (should (eq (get-char-property 2 'face) 'agent-repl-composer-known))
    (should (eq (get-char-property 15 'face) (get-char-property 2 'face)))))

(ert-deftest agent-repl-test-workspace-completion-mark-skips-a-part-of-a-name ()
  "A word that only begins an open workspace's name is not marked."
  (agent-repl-test-workspace-completion--marking '("parser-fix")
    ;; Act
    (insert "ask parser now")
    ;; Assert
    (should-not (agent-repl-test-workspace-completion--marked))))

(ert-deftest agent-repl-test-workspace-completion-mark-skips-a-longer-word ()
  "A word an open workspace's name is only part of is not marked."
  (agent-repl-test-workspace-completion--marking '("parser-fix")
    ;; Act
    (insert "ask parser-fix-two now")
    ;; Assert
    (should-not (agent-repl-test-workspace-completion--marked))))

(ert-deftest agent-repl-test-workspace-completion-mark-skips-a-closed-name ()
  "A word naming no open workspace is not marked."
  (agent-repl-test-workspace-completion--marking '()
    ;; Act
    (insert "ask parser-fix now")
    ;; Assert
    (should-not (agent-repl-test-workspace-completion--marked))))

(ert-deftest agent-repl-test-workspace-completion-mark-skips-a-slash-word ()
  "A name inside a slash word is the command completion's, never marked here."
  (agent-repl-test-workspace-completion--marking '("parser-fix")
    ;; Act
    (insert "run /parser-fix now")
    ;; Assert
    (should-not (agent-repl-test-workspace-completion--marked))))

(ert-deftest agent-repl-test-workspace-completion-mark-before-a-period ()
  "A name ending a sentence is marked without the period."
  (agent-repl-test-workspace-completion--marking '("parser-fix")
    ;; Act
    (insert "ask parser-fix.")
    ;; Assert
    (should (equal (agent-repl-test-workspace-completion--marked) '("parser-fix")))))

(ert-deftest agent-repl-test-workspace-completion-mark-a-name-ending-in-a-period ()
  "A name that itself ends in a period is marked whole."
  (agent-repl-test-workspace-completion--marking '("v1." "v1")
    ;; Act
    (insert "ask v1.")
    ;; Assert
    (should (equal (agent-repl-test-workspace-completion--marked) '("v1.")))))

(ert-deftest agent-repl-test-workspace-completion-mark-skips-bare-periods ()
  "A run of periods alone names nothing, even past the trailing-period trim."
  (agent-repl-test-workspace-completion--marking '("parser-fix")
    ;; Act
    (insert "wait...")
    ;; Assert
    (should-not (agent-repl-test-workspace-completion--marked))))

(ert-deftest agent-repl-test-workspace-completion-mark-inside-parens ()
  "A name wrapped in parens is marked without them."
  (agent-repl-test-workspace-completion--marking '("parser-fix")
    ;; Act
    (insert "(parser-fix)")
    ;; Assert
    (should (equal (agent-repl-test-workspace-completion--marked) '("parser-fix")))))

(ert-deftest agent-repl-test-workspace-completion-mark-is-removed-when-the-word-changes ()
  "Typing on past an open workspace's name unmarks it."
  (agent-repl-test-workspace-completion--marking '("parser-fix")
    ;; Arrange
    (insert "parser-fix")
    ;; Act
    (insert "es")
    ;; Assert
    (should-not (agent-repl-test-workspace-completion--marked))))

(defmacro agent-repl-test-workspace-completion--composer (text names &rest body)
  "Run BODY in the one composer, holding TEXT, while NAMES (a place) are open."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,text)
     (let ((composer (current-buffer)))
       (cl-letf (((symbol-function 'agent-repl-roster-open-workspace-names) (lambda () ,names))
                 ((symbol-function 'agent-repl--input-composers) (lambda () (list composer))))
         ,@body))))

(ert-deftest agent-repl-test-workspace-completion-mark-composers-marks-a-newly-open-name ()
  "A roster push that opens a workspace marks its name in every composer."
  (agent-repl-test-workspace-completion--composer "ask parser-fix" '("parser-fix")
    ;; Act
    (agent-repl--workspace-completion-mark-composers nil)
    ;; Assert
    (should (equal (agent-repl-test-workspace-completion--marked) '("parser-fix")))))

(ert-deftest agent-repl-test-workspace-completion-mark-composers-unmarks-a-closed-name ()
  "A roster push that closes a workspace unmarks its name in every composer."
  (let ((names '("parser-fix")))
    (agent-repl-test-workspace-completion--composer "ask parser-fix" names
      ;; Arrange
      (agent-repl--workspace-completion-mark-composers nil)
      (setq names '())
      ;; Act
      (agent-repl--workspace-completion-mark-composers nil)
      ;; Assert
      (should-not (agent-repl-test-workspace-completion--marked)))))

(ert-deftest agent-repl-test-workspace-completion-mark-composers-is-on-the-roster-hook ()
  "The re-mark runs on every roster push."
  (should (memq #'agent-repl--workspace-completion-mark-composers
                (default-value 'agent-repl-roster-update-functions))))

(provide 'test-workspace-completion)
;;; test-workspace-completion.el ends here
