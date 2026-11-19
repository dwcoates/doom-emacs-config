;;; workspace-completion.el --- workspace-name completion in the composer -*- lexical-binding: t; -*-

;;; Commentary:

;; A WORD IN THE COMPOSER THAT BEGINS A PART OF A MENTIONABLE WORKSPACE'S
;; NAME COMPLETES TO IT: typing `agent-repl-ski', or `skill', or
;; `skill-mig', offers `agent-repl-skill-migration' while that workspace is
;; open.  The candidates are the composer's own workspace's mention targets
;; (`agent-repl-host-mention-targets'), the very set the daemon serves that
;; workspace's page as the names its feed links, so a name the composer
;; completes is always a link in the feed.  They are read from the push
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
;; THE ORDER IS THE READER'S, then the workspace's.  A name taken from this
;; completion before is offered first, then one the composer's own workspace
;; mentioned in its final responses, then the rest (completion-order.el).
;;
;; EACH NAME IS ANNOTATED WITH ITS REPOSITORY, in a column aligned past the
;; longest name offered and drawn in a dimmer face
;; (`agent-repl-workspace-completion-repository'), read from the same push.
;; Every target names one workspace, so each annotation names one
;; repository.
;;
;; A WORD MATCHING NO OPEN WORKSPACE'S NAME FALLS THROUGH: the capf answers
;; nil, so the next completion function answers it.  It decides that by its
;; own table (`agent-repl-completion-match-capf', completion-match.el), never
;; by Emacs's prefix-only `:exclusive no' test, which would drop every match
;; past a name's first part.
;;
;; A MENTIONABLE WORKSPACE'S NAME IS PURPLE IN THE COMPOSER, as a known slash
;; command is: every name word that names a mention target exactly -- typed,
;; completed or pasted -- takes `agent-repl-composer-known' through an overlay
;; (composer-mark.el), the one face the known commands wear.  A name word
;; ending a sentence is matched without its trailing periods, as a slash word
;; is matched without its closing punctuation.  An edit re-marks the lines it
;; touched; a change of a workspace's mention targets re-marks its composer
;; whole.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl-host-mention-targets "agent-repl-host" (ws))
(declare-function agent-repl-annotation-column "agent-repl-core" (names gap))
(declare-function agent-repl-completion-match-capf "completion-match" (beg end table &rest props))
(declare-function agent-repl-completion-match-table "completion-match" (category collection &optional sort))
(declare-function agent-repl-completion-match-part-starts "completion-match" (word names separators))
(declare-function agent-repl-completion-order-sorter "completion-order" (kind mentioned key))
(declare-function agent-repl-completion-order-exit-function "completion-order" (kind key))
(declare-function agent-repl-host-mentions "agent-repl-host" (ws kind))
(declare-function agent-repl--buffer-owner "agent-repl-core" (buf))
(declare-function agent-repl-composer-mark-lines "composer-mark" (beg end tag find))
(declare-function agent-repl--input-composers "input" ())

(defvar agent-repl-host-mention-targets-functions)

(defface agent-repl-workspace-completion-repository
  '((t :inherit shadow))
  "Face of the repository column beside each workspace name the composer offers.
Dimmer than the name, so the name stays what the eye reads first."
  :group 'agent-repl)

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
`agent-repl--workspace-completion-separators'
\(`agent-repl-completion-match-part-starts')."
  (agent-repl-completion-match-part-starts
   word names agent-repl--workspace-completion-separators))

(defconst agent-repl--workspace-completion-category 'agent-repl-workspace-name
  "This capf's own completion category, named to no other table's.
Registered just below, in `completion-category-defaults', with the
`substring' style: the candidates are already narrowed to the names a word
begins a part of (`agent-repl--workspace-completion-matches'), and the
style only keeps a completion UI from narrowing them again by prefix.")

(add-to-list 'completion-category-defaults
             (list agent-repl--workspace-completion-category '(styles substring basic)))

(defun agent-repl--workspace-completion-table (names &optional sort)
  "Return a completion table over NAMES, in this capf's own category.
SORT, when given, is the order a completion UI offers them in
\(`agent-repl-completion-match-table')."
  (agent-repl-completion-match-table agent-repl--workspace-completion-category names sort))

(defun agent-repl--workspace-completion-annotation (workspaces)
  "Return the `:annotation-function' over WORKSPACES.
WORKSPACES is a list of (NAME . REPOSITORIES).
Each name's annotation is its repositories' names, comma-joined, in
`agent-repl-workspace-completion-repository', padded so the column starts
two spaces past the longest name in WORKSPACES.  Empty for a name whose
repository the daemon did not state."
  (let ((column (agent-repl-annotation-column (mapcar #'car workspaces) 2)))
    (lambda (candidate)
      (let ((repositories (cdr (assoc candidate workspaces))))
        (if repositories
            (concat (make-string (- column (string-width candidate)) ?\s)
                    (propertize (string-join repositories ", ")
                                'face 'agent-repl-workspace-completion-repository))
          "")))))

(defun agent-repl--workspace-completion-workspaces (ws)
  "Return WS's composer's completable workspaces, as (NAME . REPOSITORIES).
One element per mention target of WS (`agent-repl-host-mention-targets'),
REPOSITORIES the one repository the daemon stated for it, or nil."
  (mapcar (lambda (target)
            (let ((repository (plist-get target :repository)))
              (cons (plist-get target :name) (and repository (list repository)))))
          (agent-repl-host-mention-targets ws)))

(defun agent-repl-workspace-completion-at-point ()
  "Complete the name word at point against its composer's mention targets.
The composer's `completion-at-point-functions' entry.  Answers nil when
point is in no name word, or the word begins a part of no target's name.
Each name is annotated with its repository
\(`agent-repl--workspace-completion-annotation').  Deliberately unlogged:
completion runs per keystroke."
  (when-let* ((bounds (agent-repl--workspace-completion-bounds))
              (ws (agent-repl--buffer-owner (current-buffer)))
              (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
              (workspaces (agent-repl--workspace-completion-workspaces ws))
              (names (agent-repl--workspace-completion-matches
                      word (mapcar #'car workspaces))))
    (agent-repl-completion-match-capf
     (car bounds) (cdr bounds)
     (agent-repl--workspace-completion-table
      names
      (agent-repl-completion-order-sorter
       'workspace (agent-repl-host-mentions ws 'workspace) #'identity))
     :exit-function (agent-repl-completion-order-exit-function 'workspace #'identity)
     :annotation-function
     (agent-repl--workspace-completion-annotation
      (mapcar (lambda (name) (assoc name workspaces)) names)))))

;;;; ---- Marking mentionable workspaces' names ----

(defun agent-repl--workspace-completion-known-word (start end names)
  "Return (START . END'), the name word START to END naming one of NAMES, or nil.
END' is END, or END less the word's trailing periods when only the shorter
word names one."
  (let* ((word (buffer-substring-no-properties start end))
         (bare (string-trim-right word "\\.+")))
    (cond
     ((member word names) (cons start end))
     ((and (not (string-empty-p bare)) (member bare names))
      (cons start (+ start (length bare)))))))

(defun agent-repl--workspace-completion-next-known (names)
  "Return a finder of the next name word naming one of NAMES, for marking.
The finder takes a LIMIT and answers the next such word before it as
\(START . END), or nil, leaving point past it.  A name word is
`agent-repl--workspace-completion-bounds''s: a word that is part of a slash
word is never one."
  (let ((re (concat "[" agent-repl--workspace-completion-name-chars "]+")))
    (lambda (limit)
      (let (found)
        (while (and names (not found) (re-search-forward re limit t))
          (unless (eq (char-before (match-beginning 0)) ?/)
            (setq found (agent-repl--workspace-completion-known-word
                         (match-beginning 0) (match-end 0) names))))
        found))))

(defun agent-repl--workspace-completion-mark (beg end)
  "Mark every mention target's name on lines BEG to END, unmarking the rest.
The targets are the current buffer's owner's
\(`agent-repl-host-mention-targets'), none when the buffer has no owner.
Marked through `agent-repl-composer-mark-lines', in the face known slash
commands wear."
  (agent-repl-composer-mark-lines
   beg end 'agent-repl-workspace-completion-known
   (agent-repl--workspace-completion-next-known
    (when-let* ((ws (agent-repl--buffer-owner (current-buffer))))
      (mapcar (lambda (target) (plist-get target :name))
              (agent-repl-host-mention-targets ws))))))

(defun agent-repl--workspace-completion-on-change (beg end _len)
  "Re-mark the composer's lines BEG to END after an edit.
The composer's `after-change-functions' entry; unlogged, as it runs per
keystroke."
  (agent-repl--workspace-completion-mark beg end))

(defun agent-repl--workspace-completion-mark-composers (ws)
  "Re-mark WS's composers whole, as WS's mention targets changed.
Installed on `agent-repl-host-mention-targets-functions'; unlogged, as the
push it follows is logged."
  (dolist (buf (agent-repl--input-composers))
    (when (equal (agent-repl--buffer-owner buf) ws)
      (with-current-buffer buf
        (agent-repl--workspace-completion-mark (point-min) (point-max))))))

(add-hook 'agent-repl-host-mention-targets-functions #'agent-repl--workspace-completion-mark-composers)

(provide 'workspace-completion)
;;; workspace-completion.el ends here
