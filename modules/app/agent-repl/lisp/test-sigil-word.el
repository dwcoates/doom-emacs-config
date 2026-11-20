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

;;;; ---- Asking ----

(ert-deftest agent-repl-test-sigil-word-ask-key-keys-a-bare-sigil ()
  "A bare sigil asks, keyed by its start and the buffer's tick."
  (with-temp-buffer
    (insert "#")
    (should (equal (agent-repl-sigil-word-ask-key '(1 . 2) nil)
                   (cons 1 (buffer-chars-modified-tick))))))

(ert-deftest agent-repl-test-sigil-word-ask-key-skips-a-longer-word-once-answered ()
  "A longer word asks nothing once candidates are in hand."
  (with-temp-buffer
    (insert "#gen")
    (should-not (agent-repl-sigil-word-ask-key '(1 . 5) nil))))

(ert-deftest agent-repl-test-sigil-word-ask-key-asks-for-a-longer-word-while-unanswered ()
  "A longer word asks while nothing is in hand."
  (with-temp-buffer
    (insert "#gen")
    (should (agent-repl-sigil-word-ask-key '(1 . 5) t))))

;;;; ---- Known words ----

(defun agent-repl-test-sigil-word--known (text sigil known)
  "Return every SIGIL word of TEXT that is a member of KNOWN, in order."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (words bounds)
      (while (setq bounds (agent-repl-sigil-word-next-known
                           sigil (lambda (word) (member word known)) (point-max)))
        (push (buffer-substring-no-properties (car bounds) (cdr bounds)) words))
      (nreverse words))))

(ert-deftest agent-repl-test-sigil-word-next-known-finds-each-known-word ()
  "Every known sigil word is found, in order, punctuation outside it."
  (should (equal (agent-repl-test-sigil-word--known "#a and (#b). #c" ?# '("#a" "#b"))
                 '("#a" "#b"))))

(ert-deftest agent-repl-test-sigil-word-next-known-skips-a-mid-word-sigil ()
  "A sigil inside a word starts no known word."
  (should-not (agent-repl-test-sigil-word--known "x#a" ?# '("#a"))))

;;;; ---- The call sites share it ----

(ert-deftest agent-repl-test-sigil-word-every-sigil-completion-asks-and-marks-through-it ()
  "The slash-command and Slack-channel completions both key their asks and find known words here."
  ;; Arrange
  (let ((asks 0) (sigils nil))
    (cl-letf (((symbol-function 'agent-repl-sigil-word-ask-key)
               (lambda (_bounds _unanswered) (cl-incf asks) nil))
              ((symbol-function 'agent-repl-sigil-word-next-known)
               (lambda (sigil _known-p _limit) (push sigil sigils) nil)))
      (with-temp-buffer
        (setq-local agent-repl--command-completion-catalog '((:name "a")))
        (let ((agent-repl--slack-channel-completion-channels '((:name "a"))))
          ;; Act
          (agent-repl--command-completion-ask-key '(1 . 2))
          (agent-repl--slack-channel-completion-ask-key '(1 . 2))
          (agent-repl--command-completion-next-known 1)
          (agent-repl--slack-channel-completion-next-known 1))))
    ;; Assert
    (should (= asks 2))
    (should (equal sigils '(?# ?/)))))

(ert-deftest agent-repl-test-sigil-word-every-sigil-completion-finds-its-word-through-it ()
  "Every sigil completion finds its word here."
  ;; Arrange
  (let ((sigils nil))
    (cl-letf (((symbol-function 'agent-repl-sigil-word-bounds)
               (lambda (sigil) (push sigil sigils) nil)))
      ;; Act
      (agent-repl--command-completion-bounds)
      (agent-repl--slack-channel-completion-bounds)
      ;; Assert
      (should (equal sigils '(?# ?/))))))

(ert-deftest agent-repl-test-sigil-word-every-sigil-completion-builds-its-table-through-it ()
  "Every sigil completion's table is a sigil table."
  ;; Arrange
  (let ((made nil))
    (cl-letf (((symbol-function 'agent-repl-sigil-word-table)
               (lambda (category sigil _candidates &optional _sort) (push (cons category sigil) made) 'table)))
      ;; Act
      (agent-repl--command-completion-table '("/a"))
      (agent-repl--slack-channel-completion-table '("#a"))
      ;; Assert
      (should (equal made `((,agent-repl--slack-channel-completion-category . "#")
                            (,agent-repl--command-completion-category . "/")))))))

(ert-deftest agent-repl-test-sigil-word-sigils-are-every-sigil-completions ()
  "The sigil list names exactly the sigils the composer's completions open with."
  (should (equal agent-repl-sigil-word-sigils '(?/ ?#))))

(provide 'test-sigil-word)
;;; test-sigil-word.el ends here
