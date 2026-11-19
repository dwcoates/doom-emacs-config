;;; sigil-word.el --- composer words that open with a sigil -*- lexical-binding: t; -*-

;;; Commentary:

;; A SIGIL WORD IS A WORD THE COMPOSER COMPLETES BY ITS FIRST CHARACTER: a
;; slash word names a command (command-completion.el), a hash word a Slack
;; channel (slack-channel-completion.el).  Both find their word, and both
;; complete the text after the sigil, the same way, so the one shape lives
;; here and each completion names its sigil.
;;
;; PUNCTUATION AROUND A SIGIL WORD IS NOT PART OF IT.  A sigil word may
;; follow opening brackets and quotes, and drops trailing closing brackets,
;; quotes and sentence punctuation, so `(/merge-queue).' holds the slash
;; word `/merge-queue' and `(#general).' the hash word `#general'.  The
;; webapp marks a drawn user prompt's slash words by the same rule; both are
;; held to proto/vocab/slash-word.json.
;;
;; THE COMPLETED FIELD STARTS AFTER THE SIGIL.  A completion UI keeps the
;; typed sigil and replaces only what follows it, so a sigil table
;; (`agent-repl-sigil-word-table') answers bare names for a sigiled string,
;; and its `boundaries' say the sigil is outside the field.

;;; Code:

(require 'subr-x)
(require 'minibuffer)

(declare-function agent-repl-completion-match-table "completion-match" (category collection &optional sort))

(defconst agent-repl-sigil-word-sigils '(?/ ?#)
  "Every sigil a composer completion opens its word with.
A word right after one is that sigil's completion's, never a plain word's
\(workspace-completion.el).")

(defconst agent-repl-sigil-word-opening-chars "([{<\"'`“‘"
  "The punctuation a sigil word may follow inside its whitespace-bounded run.
A `skip-chars-forward' set: opening brackets and quotes, so `(/clear' and
`\"/clear' hold the slash word `/clear'.")

(defconst agent-repl-sigil-word-closing-chars ")]}>\"'`.,;:!?”’…"
  "The punctuation a sigil word's whitespace-bounded run may end in, outside it.
A `skip-chars-backward' set: closing brackets and quotes and sentence
punctuation, so `/clear).' holds the slash word `/clear'.")

(defun agent-repl-sigil-word-bounds (sigil)
  "Return (START . END) of the SIGIL word around point, or nil.
SIGIL is a character.  A sigil word lies in a run of non-whitespace.  It
begins with SIGIL at the run's start or after the run's leading opening
punctuation (`agent-repl-sigil-word-opening-chars'), and ends at the
run's end less its trailing closing punctuation
\(`agent-repl-sigil-word-closing-chars'), never before the sigil's next
character.  Point sits inside it or at its end."
  (save-excursion
    (let* ((here (point))
           (run-start (progn (skip-chars-backward "^[:space:]") (point)))
           (run-end (progn (goto-char here) (skip-chars-forward "^[:space:]") (point)))
           (start (progn (goto-char run-start)
                         (skip-chars-forward agent-repl-sigil-word-opening-chars run-end)
                         (point))))
      (when (and (< start run-end)
                 (eq (char-after start) sigil))
        (let ((end (progn (goto-char run-end)
                          (skip-chars-backward agent-repl-sigil-word-closing-chars
                                               (1+ start))
                          (point))))
          (when (<= start here end)
            (cons start end)))))))

(defun agent-repl-sigil-word-table (category sigil candidates &optional sort)
  "Return a completion table over CANDIDATES, each a word opening with SIGIL.
SIGIL is a string; CATEGORY names the table's completion category and SORT,
when given, the order a completion UI offers the candidates in
\(`agent-repl-completion-match-table').

A `boundaries' ACTION answers that every candidate's leading SIGIL is
outside the completed field, so a `substring' style's pattern is built
from the NAME after the sigil alone -- `/profile' is then a substring of
`/agent-repl-profile' because `profile' is a substring of
`agent-repl-profile', even though the literal `/profile' never occurs in
it.

THE ANSWERS AGREE WITH THAT BOUNDARY.  For a STRING opening with SIGIL,
every ACTION completes the NAME after it, through
`completion-table-with-context': all-completions answers bare names, as a
completion UI expects of a field that starts after the sigil -- it keeps
the typed sigil and replaces only what follows, so a sigiled answer would
insert a second one -- while try-completion answers the whole sigiled
word."
  (let ((names (mapcar (lambda (candidate) (string-remove-prefix sigil candidate))
                       candidates)))
    (agent-repl-completion-match-table
     category
     (lambda (string predicate action)
       (cond
        ((string-prefix-p sigil string)
         (completion-table-with-context sigil names (substring string (length sigil))
                                        predicate action))
        ((eq (car-safe action) 'boundaries)
         (cons 'boundaries (cons 0 (length (cdr action)))))
        (t (complete-with-action action candidates string predicate))))
     sort)))

(defun agent-repl-sigil-word-ask-key (bounds unanswered)
  "Return the key a composer asks for its sigil's candidates under, or nil.
BOUNDS is the sigil word's (START . END).  A bare sigil -- just typed --
asks, keyed (START . TICK) by `buffer-chars-modified-tick', so typing it
again, even where it stood, is a new ask.  A longer word asks only while
UNANSWERED is non-nil (nothing to complete from is in hand yet), keyed
the same way.  The caller asks when the key differs from its last one."
  (when (or (= (- (cdr bounds) (car bounds)) 1) unanswered)
    (cons (car bounds) (buffer-chars-modified-tick))))

(defun agent-repl-sigil-word-next-known (sigil known-p limit)
  "Return (START . END) of the next SIGIL word before LIMIT KNOWN-P accepts.
KNOWN-P is called with the word's text, sigil included.  A sigil word is
`agent-repl-sigil-word-bounds''s, so a sigil mid-word starts none.  Point
is left past the word answered, or at LIMIT's side of the last sigil
searched; nil when no such word is left."
  (let (found)
    (while (and (not found) (search-forward (char-to-string sigil) limit t))
      (when-let* ((bounds (agent-repl-sigil-word-bounds sigil)))
        (when (and (= (car bounds) (1- (point)))
                   (funcall known-p (buffer-substring-no-properties (car bounds) (cdr bounds))))
          (setq found bounds))
        (goto-char (cdr bounds))))
    found))

(provide 'sigil-word)
;;; sigil-word.el ends here
