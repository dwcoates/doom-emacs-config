;;; completion-match.el --- the composer's completions decide their own matches -*- lexical-binding: t; -*-

;;; Commentary:

;; A COMPOSER COMPLETION ANSWERS ONLY WHEN ITS OWN STYLES MATCH.  The slash
;; commands (command-completion.el) and the workspace names
;; (workspace-completion.el) both match more than a prefix: each table names
;; its own completion category, and that category's styles match a name by
;; substring.  Emacs's own test of whether a completion function matched --
;; `completion--capf-wrapper', which both `completion-at-point' and company's
;; `company-capf' run every function through -- is a PREFIX
;; `try-completion' on the text typed, and a function returned with
;; `:exclusive no' is dropped when that prefix test fails.  So `/workspaces'
;; never offered `/agent-repl-workspaces': the table matched it, the wrapper
;; never asked the table.
;;
;; So neither completion is `:exclusive no'.  Each asks its own table, under
;; the table's own styles (`completion-all-completions'), whether anything
;; matches the text typed, and answers nil when nothing does -- which is
;; what lets `/usr/local' or a word of prose fall through to the next
;; completion function.  `agent-repl-completion-match-capf' is that one
;; decision, shared by both.

;;; Code:

(require 'cl-lib)
(require 'minibuffer)

(defun agent-repl-completion-match-table (category collection)
  "Return a completion table over COLLECTION that names CATEGORY.
A `metadata' ACTION answers CATEGORY, so the styles registered for it in
`completion-category-defaults' are the table's.  Every other ACTION is
COLLECTION's -- a list of strings, or a completion function -- through
`complete-with-action'."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action collection string predicate))))

(defun agent-repl-completion-match-p (string table)
  "Return non-nil when TABLE offers a completion for STRING under its own styles.
The styles are those of TABLE's `metadata' category, as
`completion-all-completions' resolves them, with point at STRING's end."
  (consp (completion-all-completions
          string table nil (length string)
          (completion-metadata string table nil))))

(defun agent-repl-completion-match-capf (beg end table &rest props)
  "Return the capf answer (BEG END TABLE . PROPS), or nil when nothing matches.
Matching is TABLE's own (`agent-repl-completion-match-p') on the text from
BEG to point.  The answer is exclusive: its nil is the fall-through to the
next completion function, so PROPS must not carry `:exclusive'."
  (cl-assert (not (plist-member props :exclusive)) nil
             "a composer completion decides its own fall-through: %S" props)
  (when (agent-repl-completion-match-p
         (buffer-substring-no-properties beg (point)) table)
    (append (list beg end table) props)))

(provide 'completion-match)
;;; completion-match.el ends here
