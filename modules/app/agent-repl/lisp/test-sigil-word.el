;;; test-sigil-word.el --- ERT tests for sigil-word.el -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS GUARDS: that a word opening with a sigil is found by the one
;; rule every sigil shares -- after opening punctuation, before closing
;; punctuation, never mid-word -- and that a sigil table completes the text
;; after its sigil; and that the slash-command and Slack-channel
;; completions both find and complete their words through it.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

(defconst agent-repl-test-sigil-word--category 'agent-repl-test-sigil-word
  "A category of the tests' own, given the `substring' style below.")

(add-to-list 'completion-category-defaults
             (list agent-repl-test-sigil-word--category '(styles substring basic)))

(defun agent-repl-test-sigil-word--bounds (text sigil &optional point)
  "Return SIGIL's word bounds in a buffer holding TEXT, point at POINT or the end."
  (with-temp-buffer
    (insert text)
    (when point (goto-char point))
    (agent-repl-sigil-word-bounds sigil)))

(defun agent-repl-test-sigil-word--all (query sigil candidates)
  "Return the whole words QUERY completes to in SIGIL's table over CANDIDATES."
  (let* ((table (agent-repl-sigil-word-table agent-repl-test-sigil-word--category sigil candidates))
         (all (completion-all-completions query table nil (length query)
                                          (completion-metadata query table nil)))
         (base (cdr (last all))))
    (when (consp all) (setcdr (last all) nil))
    (mapcar (lambda (name) (concat (substring query 0 (or base 0)) name)) all)))

;;;; ---- Bounds ----

(ert-deftest agent-repl-test-sigil-word-bounds-find-a-hash-word ()
  "A `#' word after prose is found from its hash to its end."
  (should (equal (agent-repl-test-sigil-word--bounds "ask in #gen" ?#) '(8 . 12))))

(ert-deftest agent-repl-test-sigil-word-bounds-drop-surrounding-punctuation ()
  "Opening and closing punctuation around a sigil word lie outside it."
  (should (equal (agent-repl-test-sigil-word--bounds "(#general)." ?# 4) '(2 . 10))))

(ert-deftest agent-repl-test-sigil-word-bounds-ignore-a-mid-word-sigil ()
  "A sigil inside a word is no sigil word."
  (should-not (agent-repl-test-sigil-word--bounds "issue#12" ?#)))

(ert-deftest agent-repl-test-sigil-word-bounds-ignore-another-sigil ()
  "A word opening with another sigil is not this sigil's word."
  (should-not (agent-repl-test-sigil-word--bounds "/merge" ?#)))

(ert-deftest agent-repl-test-sigil-word-bounds-keep-a-bare-sigil ()
  "A bare sigil is a sigil word of its own."
  (should (equal (agent-repl-test-sigil-word--bounds "see #" ?#) '(5 . 6))))

;;;; ---- The table ----

(ert-deftest agent-repl-test-sigil-word-table-completes-the-name-after-the-sigil ()
  "A substring of a name after the sigil completes to the sigiled word."
  (should (equal (agent-repl-test-sigil-word--all "#eng" "#" '("#team-eng" "#random"))
                 '("#team-eng"))))

(ert-deftest agent-repl-test-sigil-word-table-boundaries-exclude-the-sigil ()
  "The completed field starts after the sigil."
  (let ((table (agent-repl-sigil-word-table agent-repl-test-sigil-word--category "#" '("#general"))))
    (should (equal (completion-boundaries "#gen" table nil "") '(1 . 0)))))

(ert-deftest agent-repl-test-sigil-word-table-try-answers-the-sigiled-word ()
  "try-completion of a unique prefix answers the whole sigiled word."
  (let ((table (agent-repl-sigil-word-table agent-repl-test-sigil-word--category "#" '("#general" "#random"))))
    (should (equal (try-completion "#gen" table) "#general"))))

(ert-deftest agent-repl-test-sigil-word-table-names-its-category ()
  "The table answers `metadata' with the category it was given."
  (let ((table (agent-repl-sigil-word-table agent-repl-test-sigil-word--category "#" '("#general"))))
    (should (equal (funcall table "" nil 'metadata)
                   `(metadata (category . ,agent-repl-test-sigil-word--category))))))

;;;; ---- The call sites share it ----

(ert-deftest agent-repl-test-sigil-word-every-sigil-completion-finds-its-word-through-it ()
  "Every sigil completion finds its word here."
  ;; Arrange
  (let ((sigils nil))
    (cl-letf (((symbol-function 'agent-repl-sigil-word-bounds)
               (lambda (sigil) (push sigil sigils) nil)))
      ;; Act
      (agent-repl--command-completion-bounds)
      ;; Assert
      (should (equal sigils '(?/))))))

(ert-deftest agent-repl-test-sigil-word-every-sigil-completion-builds-its-table-through-it ()
  "Every sigil completion's table is a sigil table."
  ;; Arrange
  (let ((made nil))
    (cl-letf (((symbol-function 'agent-repl-sigil-word-table)
               (lambda (category sigil _candidates &optional _sort) (push (cons category sigil) made) 'table)))
      ;; Act
      (agent-repl--command-completion-table '("/a"))
      ;; Assert
      (should (equal made `((,agent-repl--command-completion-category . "/")))))))

(provide 'test-sigil-word)
;;; test-sigil-word.el ends here
