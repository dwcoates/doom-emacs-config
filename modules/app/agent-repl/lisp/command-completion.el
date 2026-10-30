;;; command-completion.el --- slash-command completion in the composer -*- lexical-binding: t; -*-

;;; Commentary:

;; A WORD STARTING WITH `/' IN THE COMPOSER COMPLETES AGAINST THE WORKSPACE'S
;; RUNTIME.  The candidates are every slash command the workspace's agent
;; runtime answers -- its built-ins and every skill, custom command, plugin
;; command and MCP prompt the user added to it -- exactly as the runtime
;; itself listed them.  Nothing here knows a command by name: the closed set
;; the daemon recognizes is a different, smaller thing.
;;
;; WHERE THE LIST COMES FROM.  The shim reads the runtime's command list and
;; states it on its session stream; the daemon keeps the last statement per
;; workspace and answers it on `ListSlashCommands'.  This file asks for it
;; and keeps the answer buffer-locally in the composer.
;;
;; WHEN IT ASKS.  Each time the user types a slash that begins a word -- a
;; bare `/' the composer has not asked for at this edit -- so a skill added
;; mid-session is offered from the next slash word on.  A composer that has
;; never asked also asks for a longer slash word (one pasted whole).  The ask
;; is asynchronous: completion is served from the catalog already in hand
;; and never waits on the daemon.  Company asks this function for its prefix
;; on the bare `/', before its minimum prefix length lets it draw anything,
;; so the answer is normally in hand by the word's second character.
;;
;; A SLASH WORD NAMING NO COMMAND FALLS THROUGH.  `/usr/local', or any slash
;; word the catalog's styles match nothing for, answers nil, so the next
;; completion function answers it.  The capf decides that itself, by its own
;; styles (`agent-repl-completion-match-capf', completion-match.el), never by
;; Emacs's prefix-only `:exclusive no' test, which dropped every substring
;; match before company saw it.
;;
;; MATCHING IS BY SUBSTRING, NOT ONLY PREFIX.  Typing `/profile' or `/prof'
;; offers `/agent-repl-profile'.  The candidates are not a plain list: they
;; are wrapped in a completion table (`agent-repl--command-completion-table')
;; that answers its own `metadata' category
;; (`agent-repl--command-completion-category'), and that category alone --
;; via `completion-category-defaults', never the user's own
;; `completion-category-overrides' -- is given the `substring' style.  Every
;; other completion in Emacs, including the user's own, still follows
;; whatever `completion-styles' they have set.
;;
;; A POPUP ROW STAYS NARROW.  Each candidate's annotation (argument hint,
;; then description) is cut to `agent-repl-command-completion-annotation-width'
;; columns; the full description is the candidate's doc signature.
;;
;; PUNCTUATION AROUND A SLASH WORD IS NOT PART OF IT.  A slash word may
;; follow opening brackets and quotes, and drops trailing closing brackets,
;; quotes and sentence punctuation, so `(/merge-queue).' completes and marks
;; `/merge-queue' just as a whitespace-bounded one does.  The webapp marks a
;; drawn user prompt's known commands by the same rule; both are held to
;; proto/vocab/slash-word.json.
;;
;; A KNOWN COMMAND IS PURPLE IN THE COMPOSER.  Every slash word naming a
;; command in the catalog in hand -- typed, completed or pasted -- takes
;; `agent-repl-command-completion-known' through an overlay.  An edit
;; re-marks the lines it touched; a newly installed catalog re-marks the
;; whole composer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'minibuffer)

(declare-function agent-repl--buffer-owner "agent-repl-core" (buf))
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl-host-ref "agent-repl-host" (ws))
(declare-function agent-repl-host-conn-or-primary "agent-repl-host" (ws))
(declare-function agent-repl-host-route-handover "agent-repl-host" (ws arm slug))
(declare-function agent-repl-completion-match-capf "completion-match" (beg end table &rest props))
(declare-function agent-repl-completion-match-table "completion-match" (category collection))
(declare-function agent-repl-rpc-list-slash-commands "agent-repl-rpc" (conn request &rest keys))

(defcustom agent-repl-command-completion-annotation-width 25
  "Columns a completion candidate's annotation may take in the popup."
  :type 'natnum
  :group 'agent-repl)

(defface agent-repl-command-completion-known
  '((t :foreground "#7d4bcf"))
  "Face of a slash word in the composer that names a known command."
  :group 'agent-repl)

(defvar-local agent-repl--command-completion-catalog nil
  "The slash commands this composer completes, or nil when none are known.
A list of decoded `SessionCommandCatalogEntry' plists, in the runtime's
order, as the daemon last answered them.")

(defvar-local agent-repl--command-completion-asked-at nil
  "The edit the composer last asked for its catalog at, or nil if it never has.
A cons (START . TICK): the bare slash word's start and the buffer's
`buffer-chars-modified-tick' when it was typed.  Typing a slash again --
even at the same position, as every prompt after a send does -- is a new
TICK, and so a new ask.")

;;;; ---- The slash word at point ----

(defconst agent-repl--command-completion-opening-chars "([{<\"'`\u201c\u2018"
  "The punctuation a slash word may follow inside its whitespace-bounded run.
A `skip-chars-forward' set: opening brackets and quotes, so `(/clear' and
`\"/clear' hold the slash word `/clear'.")

(defconst agent-repl--command-completion-closing-chars ")]}>\"'`.,;:!?\u201d\u2019\u2026"
  "The punctuation a slash word's whitespace-bounded run may end in, outside it.
A `skip-chars-backward' set: closing brackets and quotes and sentence
punctuation, so `/clear).' holds the slash word `/clear'.")

(defun agent-repl--command-completion-bounds ()
  "Return (START . END) of the slash word around point, or nil.
A slash word lies in a run of non-whitespace.  It begins with `/' at the
run's start or after the run's leading opening punctuation
\(`agent-repl--command-completion-opening-chars'), and ends at the run's
end less its trailing closing punctuation
\(`agent-repl--command-completion-closing-chars'), never before the slash's
next character.  Point sits inside it or at its end."
  (save-excursion
    (let* ((here (point))
           (run-start (progn (skip-chars-backward "^[:space:]") (point)))
           (run-end (progn (goto-char here) (skip-chars-forward "^[:space:]") (point)))
           (start (progn (goto-char run-start)
                         (skip-chars-forward agent-repl--command-completion-opening-chars run-end)
                         (point))))
      (when (and (< start run-end)
                 (eq (char-after start) ?/))
        (let ((end (progn (goto-char run-end)
                          (skip-chars-backward agent-repl--command-completion-closing-chars
                                               (1+ start))
                          (point))))
          (when (<= start here end)
            (cons start end)))))))

;;;; ---- Candidates ----

(defun agent-repl--command-completion-candidates (catalog)
  "Return the completion strings CATALOG offers: each name and alias, slashed."
  (cl-loop for entry in catalog
           append (cons (concat "/" (plist-get entry :name))
                        (mapcar (lambda (alias) (concat "/" alias))
                                (plist-get entry :aliases)))))

(defconst agent-repl--command-completion-category 'agent-repl-slash-command
  "This capf's own completion category, named to no other table's.
Registered just below, in `completion-category-defaults', with the
`substring' style, then `flex' -- so `/profile' offers `/agent-repl-profile'
by substring, not only by prefix, and `/arinst' offers `/agent-repl-install'
by its characters in order -- without touching the user's own
`completion-styles' or reaching for `completion-category-overrides',
which is the user's to set.")

(add-to-list 'completion-category-defaults
             (list agent-repl--command-completion-category '(styles substring flex basic)))

(defun agent-repl--command-completion-table (candidates)
  "Return a completion table over CANDIDATES, in this capf's own category.
A plain collection function -- STRING PREDICATE ACTION, as
`completion-at-point-functions' may return -- that answers a `metadata'
ACTION with `agent-repl--command-completion-category' and otherwise
completes over CANDIDATES the usual way, via `complete-with-action'.

A `boundaries' ACTION answers that every CANDIDATE's leading `/' is
outside the completed field, so the `substring' style's pattern is built
from the command's NAME alone -- `/profile' is then a substring of
`/agent-repl-profile' because `profile' is a substring of
`agent-repl-profile', even though `/profile' the literal eight characters
never occurs in it.

THE ANSWERS AGREE WITH THAT BOUNDARY.  For a STRING past its leading `/',
every ACTION completes the NAME after the slash, through
`completion-table-with-context': all-completions answers bare names, as
a completion UI expects of a field that starts after the `/' -- it keeps
the typed `/' and replaces only what follows, so a slashed answer would
insert a second `/' -- while try-completion answers the whole slashed
word."
  (let ((names (mapcar (lambda (candidate) (string-remove-prefix "/" candidate))
                       candidates)))
    (agent-repl-completion-match-table
     agent-repl--command-completion-category
     (lambda (string predicate action)
       (cond
        ((string-prefix-p "/" string)
         (completion-table-with-context "/" names (substring string 1) predicate action))
        ((eq (car-safe action) 'boundaries)
         (cons 'boundaries (cons 0 (length (cdr action)))))
        (t (complete-with-action action candidates string predicate)))))))

(defun agent-repl--command-completion-entry (catalog candidate)
  "Return the CATALOG entry CANDIDATE names, as its name or an alias, or nil."
  (let ((name (string-remove-prefix "/" candidate)))
    (cl-find-if (lambda (entry)
                  (or (equal (plist-get entry :name) name)
                      (member name (plist-get entry :aliases))))
                catalog)))

(defun agent-repl--command-completion-annotation (catalog candidate)
  "Return CANDIDATE's annotation from CATALOG: its argument hint, then description.
Cut to `agent-repl-command-completion-annotation-width' columns, ending in
an ellipsis when cut.  Empty when the runtime stated neither."
  (let* ((entry (agent-repl--command-completion-entry catalog candidate))
         (parts (delq nil (list (plist-get entry :argument-hint)
                                (plist-get entry :description)))))
    (if parts
        (concat " " (truncate-string-to-width
                     (string-join parts "  ")
                     agent-repl-command-completion-annotation-width nil nil "…"))
      "")))

(defun agent-repl--command-completion-description (catalog candidate)
  "Return CANDIDATE's description from CATALOG, or nil when it states none."
  (plist-get (agent-repl--command-completion-entry catalog candidate) :description))

;;;; ---- Marking known commands ----

(defun agent-repl--command-completion-mark (beg end)
  "Mark every known slash word on the lines BEG to END, and unmark the rest.
A known slash word -- as `agent-repl--command-completion-bounds' finds it --
names a command in the composer's catalog, by its name or an alias, and
takes `agent-repl-command-completion-known'."
  (save-excursion
    (let ((beg (progn (goto-char beg) (line-beginning-position)))
          (end (progn (goto-char end) (line-end-position)))
          (catalog agent-repl--command-completion-catalog))
      (remove-overlays beg end 'agent-repl-command-completion-known t)
      (when catalog
        (goto-char beg)
        (while (search-forward "/" end t)
          (when-let* ((bounds (agent-repl--command-completion-bounds)))
            (when (and (= (car bounds) (1- (point)))
                       (agent-repl--command-completion-entry
                        catalog (buffer-substring-no-properties (car bounds) (cdr bounds))))
              (let ((ov (make-overlay (car bounds) (cdr bounds))))
                (overlay-put ov 'agent-repl-command-completion-known t)
                (overlay-put ov 'face 'agent-repl-command-completion-known)))
            (goto-char (cdr bounds))))))))

(defun agent-repl--command-completion-on-change (beg end _len)
  "Re-mark the composer's lines BEG to END after an edit.
The composer's `after-change-functions' entry; unlogged, as it runs per
keystroke."
  (agent-repl--command-completion-mark beg end))

(defun agent-repl--command-completion-install (buf catalog)
  "Make CATALOG composer BUF's catalog and re-mark the whole composer.
A BUF that died since it asked is left alone."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq agent-repl--command-completion-catalog catalog)
      (agent-repl--command-completion-mark (point-min) (point-max)))))

;;;; ---- Asking the daemon ----

(defun agent-repl--command-completion-answered (ws buf response)
  "Install RESPONSE, a decoded `ListSlashCommandsResponse', into composer BUF.
WS names the workspace for the log.  A refusal leaves the catalog in hand
as it stood and is logged; a handover arm is walked by host.el."
  (pcase (plist-get response :arm)
    (:success
     (let ((catalog (plist-get response :value)))
       (pcase (plist-get catalog :arm)
         (:stated
          (let ((commands (plist-get (plist-get (plist-get catalog :value) :catalog) :commands)))
            (agent-repl--log ws "elisp.command-completion.answered ws=%s outcome=stated commands=%d"
                             ws (length commands))
            (agent-repl--command-completion-install buf commands)))
         (:unstated
          (agent-repl--log ws "elisp.command-completion.answered ws=%s outcome=unstated" ws)
          (agent-repl--command-completion-install buf nil))
         (other
          (agent-repl--error ws "elisp.command-completion.unknown-catalog-arm ws=%s arm=%S"
                             ws other)))))
    (:error
     (let ((cause (plist-get (plist-get response :value) :cause)))
       (unless (agent-repl-host-route-handover
                ws cause "elisp.command-completion.handover-refusal")
         (agent-repl--warn ws "elisp.command-completion.refused ws=%s cause=%S" ws cause))))
    (other
     (agent-repl--error ws "elisp.command-completion.unknown-response-arm ws=%s arm=%S"
                        ws other))))

(defun agent-repl-command-completion-refresh (ws)
  "Ask the daemon for WS's slash commands, for WS's composer (the current buffer).
Asynchronous: the answer replaces the composer's catalog when it arrives.
A workspace the daemon has not registered, or no connection, asks nothing
and says so in the log -- the next slash word asks again."
  (let ((ref (agent-repl-host-ref ws))
        (conn (agent-repl-host-conn-or-primary ws))
        (buf (current-buffer)))
    (cond
     ((null ref)
      (agent-repl--log ws "elisp.command-completion.refresh-skipped ws=%s reason=no-ref" ws))
     ((null conn)
      (agent-repl--log ws "elisp.command-completion.refresh-skipped ws=%s reason=no-connection" ws))
     (t
      (agent-repl--log ws "elisp.command-completion.refresh ws=%s" ws)
      (agent-repl-rpc-list-slash-commands
       conn (list :workspace ref)
       :on-response (lambda (response)
                      (agent-repl--command-completion-answered ws buf response))
       :on-failure (lambda (detail)
                     (agent-repl--warn ws "elisp.command-completion.transport-failure ws=%s detail=%S"
                                       ws detail)))))))

;;;; ---- completion-at-point ----

(defun agent-repl--command-completion-ask-key (bounds)
  "Return the ask key for the slash word at BOUNDS, or nil when it asks nothing.
The key of a bare `/' is (START . TICK); a longer slash word asks only in a
composer that has never asked, keyed the same way."
  (when (or (= (- (cdr bounds) (car bounds)) 1)
            (null agent-repl--command-completion-asked-at))
    (cons (car bounds) (buffer-chars-modified-tick))))

(defun agent-repl-command-completion-at-point ()
  "Complete the slash word at point against the workspace's slash commands.
The composer's `completion-at-point-functions' entry.  Asks the daemon
again when a slash has just been typed (see
`agent-repl--command-completion-ask-key'), and answers from the catalog
in hand, or nil when nothing in it matches.  Deliberately unlogged per call:
completion runs per keystroke."
  (when-let* ((bounds (agent-repl--command-completion-bounds))
              (ws (agent-repl--buffer-owner (current-buffer))))
    (let ((key (agent-repl--command-completion-ask-key bounds)))
      (when (and key (not (equal key agent-repl--command-completion-asked-at)))
        (setq agent-repl--command-completion-asked-at key)
        (agent-repl-command-completion-refresh ws)))
    (let ((catalog agent-repl--command-completion-catalog))
      (agent-repl-completion-match-capf
       (car bounds) (cdr bounds)
       (agent-repl--command-completion-table
        (agent-repl--command-completion-candidates catalog))
       :annotation-function
       (lambda (candidate) (agent-repl--command-completion-annotation catalog candidate))
       :company-docsig
       (lambda (candidate) (agent-repl--command-completion-description catalog candidate))))))

(provide 'command-completion)
;;; command-completion.el ends here
