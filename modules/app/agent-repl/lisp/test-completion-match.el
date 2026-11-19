;;; test-completion-match.el --- ERT tests for completion-match.el -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS GUARDS: that a composer completion answers exactly when its own
;; table's styles match the text typed, and nil otherwise, so a match past a
;; name's prefix survives Emacs's capf wrapper; and that both composer
;; completions answer through that one decision.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

(defconst agent-repl-test-completion-match--category 'agent-repl-test-completion-match
  "A category of the tests' own, given the `substring' style below.")

(add-to-list 'completion-category-defaults
             (list agent-repl-test-completion-match--category '(styles substring)))

(defun agent-repl-test-completion-match--table (names)
  "A completion table over NAMES in the tests' substring category."
  (agent-repl-completion-match-table agent-repl-test-completion-match--category names))

(ert-deftest agent-repl-test-completion-match-table-names-its-category ()
  "The table answers `metadata' with the category it was made with."
  ;; Act / Assert
  (should (eq (completion-metadata-get
               (completion-metadata "" (agent-repl-test-completion-match--table '("a")) nil)
               'category)
              agent-repl-test-completion-match--category)))

(ert-deftest agent-repl-test-completion-match-table-completes-a-list ()
  "Over a list, every other action completes the list."
  ;; Arrange
  (let ((table (agent-repl-test-completion-match--table '("alpha" "beta"))))
    ;; Act / Assert
    (should (equal (all-completions "al" table) '("alpha")))
    (should (equal (try-completion "be" table) "beta"))))

(ert-deftest agent-repl-test-completion-match-table-delegates-to-a-function ()
  "Over a completion function, every other action is the function's."
  ;; Arrange
  (let* ((seen nil)
         (table (agent-repl-completion-match-table
                 'any (lambda (string _predicate action) (push (list string action) seen) '("x")))))
    ;; Act
    (let ((answer (all-completions "s" table)))
      ;; Assert
      (should (equal answer '("x")))
      (should (equal seen '(("s" t)))))))

(ert-deftest agent-repl-test-completion-match-both-composer-tables-are-made-by-it ()
  "The slash-command and workspace-name tables are both made by the helper."
  ;; Arrange
  (let ((categories nil))
    (cl-letf (((symbol-function 'agent-repl-completion-match-table)
               (lambda (category _collection &optional _sort) (push category categories) 'table)))
      ;; Act
      (agent-repl--command-completion-table '("/a"))
      (agent-repl--workspace-completion-table '("a"))
      ;; Assert
      (should (equal categories '(agent-repl-workspace-name agent-repl-slash-command))))))

(ert-deftest agent-repl-test-completion-match-both-composer-tables-carry-their-sort ()
  "Both composer tables hand the helper the order they were given, unchanged."
  ;; Arrange
  (let ((sorts nil))
    (cl-letf (((symbol-function 'agent-repl-completion-match-table)
               (lambda (_category _collection &optional sort) (push sort sorts) 'table)))
      ;; Act
      (agent-repl--command-completion-table '("/a") 'command-sort)
      (agent-repl--workspace-completion-table '("a") 'workspace-sort)
      ;; Assert
      (should (equal sorts '(workspace-sort command-sort))))))

(ert-deftest agent-repl-test-completion-match-table-carries-its-sort ()
  "A table made with a sort answers it as both its display and cycle sort."
  ;; Arrange
  (let* ((sort (lambda (candidates) (reverse candidates)))
         (metadata (completion-metadata
                    "" (agent-repl-completion-match-table
                        agent-repl-test-completion-match--category '("a" "b") sort)
                    nil)))
    ;; Act / Assert
    (should (eq (completion-metadata-get metadata 'display-sort-function) sort))
    (should (eq (completion-metadata-get metadata 'cycle-sort-function) sort))))

(ert-deftest agent-repl-test-completion-match-table-without-a-sort-states-none ()
  "A table made with no sort leaves the completion UI's own order."
  ;; Arrange
  (let ((metadata (completion-metadata "" (agent-repl-test-completion-match--table '("a")) nil)))
    ;; Act / Assert
    (should-not (completion-metadata-get metadata 'display-sort-function))
    (should-not (completion-metadata-get metadata 'cycle-sort-function))))

(ert-deftest agent-repl-test-completion-match-p-follows-the-tables-styles ()
  "A substring the table's own style matches is a match."
  ;; Arrange
  (let ((table (agent-repl-test-completion-match--table '("agent-repl-workspaces"))))
    ;; Act / Assert
    (should (agent-repl-completion-match-p "workspaces" table))))

(ert-deftest agent-repl-test-completion-match-p-is-nil-for-no-match ()
  "Text no candidate holds is no match."
  ;; Arrange
  (let ((table (agent-repl-test-completion-match--table '("agent-repl-workspaces"))))
    ;; Act / Assert
    (should-not (agent-repl-completion-match-p "zzz" table))))

(ert-deftest agent-repl-test-completion-match-p-is-nil-for-no-candidates ()
  "A table with no candidates matches nothing, not even empty text."
  ;; Act / Assert
  (should-not (agent-repl-completion-match-p "" (agent-repl-test-completion-match--table nil))))

(ert-deftest agent-repl-test-completion-match-capf-answers-a-match ()
  "A match answers BEG END TABLE and the props, with no `:exclusive'."
  (with-temp-buffer
    ;; Arrange
    (insert "ask workspaces")
    (let ((table (agent-repl-test-completion-match--table '("agent-repl-workspaces"))))
      ;; Act
      (let ((capf (agent-repl-completion-match-capf 5 15 table :annotation-function #'ignore)))
        ;; Assert
        (should (equal capf (list 5 15 table :annotation-function #'ignore)))))))

(ert-deftest agent-repl-test-completion-match-capf-reads-the-text-up-to-point ()
  "The text matched runs from BEG to point, not to END."
  (with-temp-buffer
    ;; Arrange
    (insert "workzzz")
    (goto-char 5)
    ;; Act / Assert
    (should (agent-repl-completion-match-capf
             1 8 (agent-repl-test-completion-match--table '("agent-repl-workspaces"))))))

(ert-deftest agent-repl-test-completion-match-capf-is-nil-for-no-match ()
  "No match answers nil, the fall-through to the next completion function."
  (with-temp-buffer
    ;; Arrange
    (insert "zzz")
    ;; Act / Assert
    (should-not (agent-repl-completion-match-capf
                 1 4 (agent-repl-test-completion-match--table '("agent-repl-workspaces"))))))

(ert-deftest agent-repl-test-completion-match-capf-refuses-exclusive ()
  "A caller passing `:exclusive' fails hard: the helper owns the fall-through."
  (with-temp-buffer
    ;; Arrange
    (insert "work")
    ;; Act / Assert
    (should-error (agent-repl-completion-match-capf
                   1 5 (agent-repl-test-completion-match--table '("workspaces"))
                   :exclusive 'no))))

(ert-deftest agent-repl-test-completion-match-capf-survives-the-capf-wrapper ()
  "A substring match is kept by Emacs's capf wrapper, as company runs it."
  (with-temp-buffer
    ;; Arrange
    (insert "workspaces")
    (let ((fun (lambda ()
                 (agent-repl-completion-match-capf
                  1 11 (agent-repl-test-completion-match--table '("agent-repl-workspaces"))))))
      ;; Act / Assert
      (should (completion--capf-wrapper fun 'all)))))

(ert-deftest agent-repl-test-completion-match-both-composer-completions-answer-through-it ()
  "The slash-command and workspace-name capfs both answer through the helper."
  (with-temp-buffer
    ;; Arrange
    (setq-local agent-repl--owning-workspace "ws-1")
    (setq-local agent-repl--command-completion-asked-at '(1 . 0))
    (let ((callers nil))
      (cl-letf (((symbol-function 'agent-repl-completion-match-capf)
                 (lambda (&rest _) (push 'called callers) 'answer))
                ((symbol-function 'agent-repl-host-mention-targets)
                 (lambda (_ws) '((:name "agent-repl-workspaces" :workspace (:id "w" :dir "/w")
                                  :repository "doom")))))
        ;; Act
        (insert "/work")
        (let ((command (agent-repl-command-completion-at-point)))
          (erase-buffer)
          (insert "work")
          (let ((workspace (agent-repl-workspace-completion-at-point)))
            ;; Assert
            (should (eq command 'answer))
            (should (eq workspace 'answer))
            (should (= (length callers) 2))))))))

;;;; ---- Part-start matching ----

(ert-deftest agent-repl-test-completion-match-part-starts-match-a-names-start ()
  "A word matches a name it begins."
  (should (equal (agent-repl-completion-match-part-starts "par" '("parser-migration" "x") "-_.")
                 '("parser-migration"))))

(ert-deftest agent-repl-test-completion-match-part-starts-match-after-a-separator ()
  "A word matches a name one of whose parts it begins, in the names' order."
  (should (equal (agent-repl-completion-match-part-starts
                  "mig" '("db.migrate" "parser-migration" "team_mig") "-_.")
                 '("db.migrate" "parser-migration" "team_mig"))))

(ert-deftest agent-repl-test-completion-match-part-starts-skip-a-mid-part-match ()
  "A word inside a part, at no part's start, matches nothing."
  (should-not (agent-repl-completion-match-part-starts "on" '("parser-migration") "-_.")))

(ert-deftest agent-repl-test-completion-match-part-starts-quote-the-word ()
  "A word's regexp characters match only themselves."
  (should-not (agent-repl-completion-match-part-starts "a." '("ab") "-")))

(ert-deftest agent-repl-test-completion-match-every-part-start-completion-matches-through-it ()
  "Every completion matching at a name part's start narrows through the helper."
  ;; Arrange
  (let ((calls nil))
    (cl-letf (((symbol-function 'agent-repl-completion-match-part-starts)
               (lambda (word _names separators) (push (cons word separators) calls) nil)))
      ;; Act
      (agent-repl--workspace-completion-matches "w" '("w"))
      ;; Assert
      (should (equal calls `(("w" . ,agent-repl--workspace-completion-separators)))))))

(provide 'test-completion-match)
;;; test-completion-match.el ends here
