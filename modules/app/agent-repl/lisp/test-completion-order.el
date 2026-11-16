;;; test-completion-order.el --- ERT tests for completion-order.el -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS GUARDS: the one order both composer completions offer their
;; candidates in -- taken before, the most recently taken first; then
;; mentioned by the composer's own workspace, in the record's order; then the
;; rest in the table's order -- and that only a finished completion counts as
;; taken.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

(defmacro agent-repl-test-completion-order--fresh (&rest body)
  "Run BODY with no candidate taken yet."
  (declare (indent 0))
  `(let ((agent-repl--completion-order-selected (make-hash-table :test 'equal))
         (agent-repl--completion-order-clock 0))
     ,@body))

(defun agent-repl-test-completion-order--sort (kind mentioned candidates)
  "CANDIDATES of KIND in order, MENTIONED by the workspace, keyed by name."
  (agent-repl-completion-order-sort kind mentioned #'identity candidates))

(ert-deftest agent-repl-test-completion-order-keeps-the-tables-order-for-the-rest ()
  "Nothing taken and nothing mentioned leaves the table's order."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command nil '("c" "a" "b"))
                   '("c" "a" "b")))))

(ert-deftest agent-repl-test-completion-order-puts-mentioned-first-in-the-records-order ()
  "Mentioned candidates come first, most recently mentioned first."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command '("b" "d") '("a" "d" "c" "b"))
                   '("b" "d" "a" "c")))))

(ert-deftest agent-repl-test-completion-order-ignores-a-mention-no-candidate-is ()
  "A mentioned name the table does not offer changes nothing."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command '("gone") '("a" "b"))
                   '("a" "b")))))

(ert-deftest agent-repl-test-completion-order-puts-taken-before-mentioned ()
  "A taken candidate comes before every mentioned one."
  (agent-repl-test-completion-order--fresh
    ;; Arrange
    (agent-repl-completion-order-note-selected 'command "c")
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command '("b") '("a" "b" "c"))
                   '("c" "b" "a")))))

(ert-deftest agent-repl-test-completion-order-puts-the-latest-taken-first ()
  "Of two taken candidates, the one taken last comes first."
  (agent-repl-test-completion-order--fresh
    ;; Arrange
    (agent-repl-completion-order-note-selected 'command "a")
    (agent-repl-completion-order-note-selected 'command "b")
    (agent-repl-completion-order-note-selected 'command "a")
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command nil '("b" "c" "a"))
                   '("a" "b" "c")))))

(ert-deftest agent-repl-test-completion-order-keeps-kinds-apart ()
  "A workspace taken under one name orders no command of that name."
  (agent-repl-test-completion-order--fresh
    ;; Arrange
    (agent-repl-completion-order-note-selected 'workspace "b")
    ;; Act / Assert
    (should (equal (agent-repl-test-completion-order--sort 'command nil '("a" "b"))
                   '("a" "b")))))

(ert-deftest agent-repl-test-completion-order-keys-candidates-by-name ()
  "KEY maps a candidate to the name it is taken and mentioned under."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should (equal (agent-repl-completion-order-sort
                    'command '("b") (lambda (c) (string-remove-prefix "/" c)) '("/a" "/b"))
                   '("/b" "/a")))))

(ert-deftest agent-repl-test-completion-order-leaves-its-input-whole ()
  "Sorting does not modify the candidates it was given."
  (agent-repl-test-completion-order--fresh
    ;; Arrange
    (let ((candidates (list "a" "b" "c")))
      ;; Act
      (agent-repl-test-completion-order--sort 'command '("c") candidates)
      ;; Assert
      (should (equal candidates '("a" "b" "c"))))))

(ert-deftest agent-repl-test-completion-order-sorter-is-the-sort ()
  "The display-sort-function a table carries is the sort over its candidates."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should (equal (funcall (agent-repl-completion-order-sorter 'workspace '("b") #'identity)
                            '("a" "b"))
                   '("b" "a")))))

(ert-deftest agent-repl-test-completion-order-exit-records-a-finished-completion ()
  "A `finished' completion is taken, under the name KEY gives it."
  (agent-repl-test-completion-order--fresh
    ;; Act
    (funcall (agent-repl-completion-order-exit-function
              'command (lambda (c) (string-remove-prefix "/" c)))
             "/usage" 'finished)
    ;; Assert
    (should (gethash '(command . "usage") agent-repl--completion-order-selected))))

(ert-deftest agent-repl-test-completion-order-exit-records-a-sole-completion ()
  "A `sole' completion is taken."
  (agent-repl-test-completion-order--fresh
    ;; Act
    (funcall (agent-repl-completion-order-exit-function 'workspace #'identity) "alpha" 'sole)
    ;; Assert
    (should (gethash '(workspace . "alpha") agent-repl--completion-order-selected))))

(ert-deftest agent-repl-test-completion-order-exit-ignores-an-exact-match ()
  "An `exact' match leaves the reader still choosing, so nothing is taken."
  (agent-repl-test-completion-order--fresh
    ;; Act
    (funcall (agent-repl-completion-order-exit-function 'workspace #'identity) "alpha" 'exact)
    ;; Assert
    (should (zerop (hash-table-count agent-repl--completion-order-selected)))))

(ert-deftest agent-repl-test-completion-order-refuses-an-unknown-kind ()
  "Noting a selection of a kind no completion has is a defect, refused loudly."
  (agent-repl-test-completion-order--fresh
    ;; Act / Assert
    (should-error (agent-repl-completion-order-note-selected 'skill "x"))
    (should (zerop (hash-table-count agent-repl--completion-order-selected)))))

(provide 'test-completion-order)
;;; test-completion-order.el ends here
