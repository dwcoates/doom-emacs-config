;;; test-composer-mark.el --- ERT tests for composer-mark.el -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS GUARDS: that `agent-repl-composer-mark-lines' widens to whole
;; lines, unmarks only its own tag before marking, marks every word its
;; finder answers in the one canonical face, and that every kind of mark in
;; the composer goes through it.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

(defconst agent-repl-test-composer-mark--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "This suite's directory, where the composer's mark kinds' sources live.")

(defun agent-repl-test-composer-mark--find-word (word)
  "Return a finder answering each occurrence of WORD before its limit."
  (lambda (limit)
    (when (search-forward word limit t)
      (cons (match-beginning 0) (match-end 0)))))

(defun agent-repl-test-composer-mark--marked (tag)
  "Return the (START . END) of every overlay carrying TAG, in buffer order."
  (sort (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
                (cl-remove-if-not (lambda (ov) (overlay-get ov tag))
                                  (overlays-in (point-min) (point-max))))
        (lambda (a b) (< (car a) (car b)))))

(ert-deftest agent-repl-test-composer-mark-marks-every-found-word ()
  "Every word the finder answers is marked with the tag."
  (with-temp-buffer
    ;; Arrange
    (insert "foo bar foo")
    ;; Act
    (agent-repl-composer-mark-lines 1 2 'tag-a (agent-repl-test-composer-mark--find-word "foo"))
    ;; Assert
    (should (equal (agent-repl-test-composer-mark--marked 'tag-a) '((1 . 4) (9 . 12))))))

(ert-deftest agent-repl-test-composer-mark-uses-the-canonical-face ()
  "A marked word wears `agent-repl-composer-known'."
  (with-temp-buffer
    ;; Arrange
    (insert "foo")
    ;; Act
    (agent-repl-composer-mark-lines 1 1 'tag-a (agent-repl-test-composer-mark--find-word "foo"))
    ;; Assert
    (should (eq (get-char-property 2 'face) 'agent-repl-composer-known))))

(ert-deftest agent-repl-test-composer-mark-widens-to-whole-lines ()
  "A region inside one line marks that whole line and no other."
  (with-temp-buffer
    ;; Arrange
    (insert "foo\nxx foo\nfoo")
    ;; Act
    (agent-repl-composer-mark-lines 6 6 'tag-a (agent-repl-test-composer-mark--find-word "foo"))
    ;; Assert
    (should (equal (agent-repl-test-composer-mark--marked 'tag-a) '((8 . 11))))))

(ert-deftest agent-repl-test-composer-mark-unmarks-a-word-no-longer-found ()
  "Re-marking removes the tag's marks the finder no longer answers."
  (with-temp-buffer
    ;; Arrange
    (insert "foo")
    (agent-repl-composer-mark-lines 1 1 'tag-a (agent-repl-test-composer-mark--find-word "foo"))
    ;; Act
    (agent-repl-composer-mark-lines 1 1 'tag-a (lambda (_limit) nil))
    ;; Assert
    (should-not (agent-repl-test-composer-mark--marked 'tag-a))))

(ert-deftest agent-repl-test-composer-mark-leaves-another-tags-marks ()
  "Re-marking one tag never removes another tag's marks."
  (with-temp-buffer
    ;; Arrange
    (insert "foo bar")
    (agent-repl-composer-mark-lines 1 1 'tag-b (agent-repl-test-composer-mark--find-word "bar"))
    ;; Act
    (agent-repl-composer-mark-lines 1 1 'tag-a (lambda (_limit) nil))
    ;; Assert
    (should (equal (agent-repl-test-composer-mark--marked 'tag-b) '((5 . 8))))))

(ert-deftest agent-repl-test-composer-mark-is-not-duplicated ()
  "Re-marking the same line leaves exactly one mark per word."
  (with-temp-buffer
    ;; Arrange
    (insert "foo")
    (agent-repl-composer-mark-lines 1 1 'tag-a (agent-repl-test-composer-mark--find-word "foo"))
    ;; Act
    (agent-repl-composer-mark-lines 1 1 'tag-a (agent-repl-test-composer-mark--find-word "foo"))
    ;; Assert
    (should (equal (agent-repl-test-composer-mark--marked 'tag-a) '((1 . 4))))))

(ert-deftest agent-repl-test-composer-mark-the-old-face-name-aliases-the-canonical-one ()
  "The command completion's old face name resolves to the canonical face."
  (should (eq (get 'agent-repl-command-completion-known 'face-alias)
              'agent-repl-composer-known)))

(ert-deftest agent-repl-test-composer-mark-every-kind-marks-through-the-helper ()
  "Every composer mark function calls `agent-repl-composer-mark-lines', none hand-rolls one."
  (let ((dir agent-repl-test-composer-mark--dir))
    (dolist (file '("command-completion.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file dir))
        (should (search-forward "(agent-repl-composer-mark-lines" nil t))
        (goto-char (point-min))
        (should-not (search-forward "(make-overlay" nil t))))))

(provide 'test-composer-mark)
;;; test-composer-mark.el ends here
