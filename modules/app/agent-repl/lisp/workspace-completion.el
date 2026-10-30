;;; workspace-completion.el --- workspace-name completion in the composer -*- lexical-binding: t; -*-

;;; Commentary:

;; A WORD IN THE COMPOSER THAT BEGINS A PART OF AN OPEN WORKSPACE'S NAME
;; COMPLETES TO IT: typing `agent-repl-ski', or `skill', or `skill-mig',
;; offers `agent-repl-skill-migration' while that workspace is open.  The
;; candidates are the names of every workspace the roster carries that is
;; not closed (`agent-repl-roster-open-workspace-names'), read from the push
;; already in hand, so completion never waits on the daemon.
;;
;; A NAME WORD is a run of the characters workspace names are made of
;; (`agent-repl--workspace-completion-name-chars').  A word that is part of a
;; slash word is the command completion's (command-completion.el), never
;; this one's.
;;
;; MATCHING IS AT A NAME PART'S START, NOT ANYWHERE.  A name's parts are
;; split at `-', `_' and `.' (`agent-repl--workspace-completion-separators'),
;; and a word matches a name it occurs in starting at the name's start or
;; just after a separator.  The slash commands match anywhere, and then
;; fuzzily, because a slash word is always a command; a name word is any
;; word of prose, so matching anywhere would offer `parser-migration' for
;; `on'.  The candidates are narrowed so here, and their table
;; (`agent-repl--workspace-completion-table') names its own category, given
;; the `substring' style, so a completion UI keeps every one of them.
;;
;; A WORD MATCHING NO OPEN WORKSPACE'S NAME FALLS THROUGH: the capf answers
;; nil, so the next completion function answers it.  It decides that by its
;; own table (`agent-repl-completion-match-capf', completion-match.el), never
;; by Emacs's prefix-only `:exclusive no' test, which would drop every match
;; past a name's first part.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl-roster-open-workspace-names "agent-repl-roster" ())
(declare-function agent-repl-completion-match-capf "completion-match" (beg end table &rest props))

(defconst agent-repl--workspace-completion-name-chars "-a-zA-Z0-9_."
  "The characters a workspace name word is made of, as a `skip-chars' set.")

(defun agent-repl--workspace-completion-bounds ()
  "Return (START . END) of the name word around point, or nil.
Nil when point is in no name word, and when the word is part of a slash
word: the character before it is `/'."
  (save-excursion
    (let* ((here (point))
           (start (progn (skip-chars-backward agent-repl--workspace-completion-name-chars) (point)))
           (end (progn (goto-char here)
                       (skip-chars-forward agent-repl--workspace-completion-name-chars)
                       (point))))
      (when (and (< start end)
                 (not (eq (char-before start) ?/)))
        (cons start end)))))

(defconst agent-repl--workspace-completion-separators "-_."
  "The characters that split a workspace name into the parts a word may begin.")

(defun agent-repl--workspace-completion-matches (word names)
  "Return the NAMES WORD occurs in at a part's start, in NAMES's order.
A part starts at the name's start or just after one of
`agent-repl--workspace-completion-separators'."
  (let ((re (concat "\\(?:\\`\\|[" agent-repl--workspace-completion-separators "]\\)"
                    (regexp-quote word))))
    (cl-remove-if-not (lambda (name) (string-match-p re name)) names)))

(defconst agent-repl--workspace-completion-category 'agent-repl-workspace-name
  "This capf's own completion category, named to no other table's.
Registered just below, in `completion-category-defaults', with the
`substring' style: the candidates are already narrowed to the names a word
begins a part of (`agent-repl--workspace-completion-matches'), and the
style only keeps a completion UI from narrowing them again by prefix.")

(add-to-list 'completion-category-defaults
             (list agent-repl--workspace-completion-category '(styles substring basic)))

(defun agent-repl--workspace-completion-table (names)
  "Return a completion table over NAMES, in this capf's own category."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (category . ,agent-repl--workspace-completion-category))
      (complete-with-action action names string predicate))))

(defun agent-repl-workspace-completion-at-point ()
  "Complete the name word at point against the open workspaces' names.
The composer's `completion-at-point-functions' entry.  Answers nil when
point is in no name word, or the word begins a part of no open workspace's
name.  Deliberately unlogged: completion runs per keystroke."
  (when-let* ((bounds (agent-repl--workspace-completion-bounds))
              (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
              (names (agent-repl--workspace-completion-matches
                      word (agent-repl-roster-open-workspace-names))))
    (agent-repl-completion-match-capf
     (car bounds) (cdr bounds) (agent-repl--workspace-completion-table names)
     :annotation-function (lambda (_candidate) " workspace"))))

(provide 'workspace-completion)
;;; workspace-completion.el ends here
