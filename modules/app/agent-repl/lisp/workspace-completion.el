;;; workspace-completion.el --- workspace-name completion in the composer -*- lexical-binding: t; -*-

;;; Commentary:

;; A WORD IN THE COMPOSER THAT BEGINS AN OPEN WORKSPACE'S NAME COMPLETES TO
;; IT: typing `agent-repl-ski' offers `agent-repl-skill-migration' while that
;; workspace is open.  The candidates are the names of every workspace the
;; roster carries that is not closed (`agent-repl-roster-open-workspace-names'),
;; read from the push already in hand, so completion never waits on the
;; daemon.
;;
;; A NAME WORD is a run of the characters workspace names are made of
;; (`agent-repl--workspace-completion-name-chars').  A word that is part of a
;; slash word is the command completion's (command-completion.el), never
;; this one's.
;;
;; MATCHING IS BY PREFIX.  The slash commands complete by substring because a
;; slash word is always a command; a name word is any word of prose, and a
;; substring match would offer workspaces in the middle of ordinary sentences.
;;
;; THE COMPLETION IS NON-EXCLUSIVE: a word that begins no open workspace's
;; name falls through to the next completion function.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl-roster-open-workspace-names "agent-repl-roster" ())

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

(defun agent-repl-workspace-completion-at-point ()
  "Complete the name word at point against the open workspaces' names.
The composer's `completion-at-point-functions' entry.  Answers nil when
point is in no name word, or no open workspace's name begins with it.
Deliberately unlogged: completion runs per keystroke."
  (when-let* ((bounds (agent-repl--workspace-completion-bounds))
              (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
              (names (cl-remove-if-not (lambda (name) (string-prefix-p word name))
                                       (agent-repl-roster-open-workspace-names))))
    (list (car bounds) (cdr bounds) names
          :exclusive 'no
          :annotation-function (lambda (_candidate) " workspace"))))

(provide 'workspace-completion)
;;; workspace-completion.el ends here
