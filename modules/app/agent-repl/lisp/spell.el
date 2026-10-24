;;; spell.el --- spellcheck the composer as you type -*- lexical-binding: t; -*-

;;; Commentary:

;; The composer is spellchecked by `flyspell-mode' running hunspell against
;; ONE dictionary: SCOWL size 80, the largest size SCOWL recommends for
;; spellchecking.  `bin/requirements.sh' installs it as the `spell-dictionary'
;; entry of `lisp/requirements.json' (a `download' kind, pinned by sha256),
;; and that manifest's `hunspell' entry installs the program.  The manifest
;; sits in `lisp/', beside the code that reads it.
;;
;; THE MANIFEST IS THE ONE RECORD of where the dictionary lives and what it is
;; called.  This file reads the `spell-dictionary' entry rather than naming
;; the directory or the file again, so the runner that installs the
;; dictionary and the editor that reads it cannot disagree about either.
;;
;; EMACS'S SPELLCHECKER IS GLOBAL, THE COMPOSER'S DICTIONARY IS LOCAL.
;; ispell.el reads its program and the process environment from inside
;; temporary buffers (`ispell-check-version', the hunspell dictionary
;; discovery), where a composer's buffer-local values are invisible: a
;; buffer-local program was answered by whatever `ispell' the global value
;; named, which then hung on hunspell's replies.  So the first composer
;; points Emacs's `ispell-program-name' at hunspell, puts the dictionary's
;; dir on hunspell's `DICPATH', and registers the dictionary in
;; `ispell-local-dictionary-alist' (`agent-repl--spell-configure-ispell').
;; Choosing that dictionary, the code-skipping predicate and the keys stay
;; local to each composer.
;;
;; A COMPOSER BORN WITHOUT THE PROGRAM OR THE DICTIONARY is not spellchecked,
;; and says so once on the log with the fix (`bin/requirements.sh install').
;; It is never a silent no-op.
;;
;; TEXT THAT ARRIVES WHOLE is checked too.  flyspell checks the word a command
;; just left, so a paste, a history recall, a restored draft or a rollback's
;; refill would stay unchecked until point visited each word.  Every
;; insertion longer than one character is queued, and the queue is checked
;; on the next turn of the command loop (a zero-second timer), so a refill
;; that arrives in a process filter is checked at once rather than at the
;; next keystroke.
;;
;; THE CHECK NEVER HOLDS THE COMMAND LOOP.  flyspell's own region check talks
;; to hunspell a word at a time and redisplays after each: measured on the
;; live editor (2026-10-24), 1.2 s for a 362-character prompt and 2.7 s for
;; 905 characters, every keystroke after a rollback's refill waiting behind
;; it.  Instead the queued text goes to a hunspell process of its own
;; (`agent-repl--spell-run-hunspell', through `agent-repl--async-command'),
;; and its answer is applied when it comes: each word it names is
;; highlighted wherever it still stands in the span, as flyspell highlights
;; one.  A misspelling is a property of the word, not of where it stands, so
;; an edit made while hunspell works moves nothing the answer is applied to.
;; Being a process of its own, it never shares an exchange with flyspell's.
;; CODE IS NOT PROSE.  A fenced block, an inline `code' span and a token that
;; reads as a path, a URL, an identifier or an address are skipped
;; (`agent-repl--spell-prose-at-p').
;;
;; Starting flyspell starts hunspell, and checking a region talks to it, so
;; so does the queued check's own hunspell; each goes through an
;; external-boundary wrapper registered in
;; `agent-repl--external-boundary-functions' (core.el), as does the PATH
;; lookup (`agent-repl--executable-find').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--fatal "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn-once "agent-repl-core" (ws fingerprint fmt &rest args))
(declare-function agent-repl--async-command "agent-repl-core" (label dir command callback &optional input))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--executable-find "agent-repl-core" (program))
(declare-function flyspell-mode "flyspell" (&optional arg))
(declare-function flyspell-highlight-incorrect-region "flyspell" (beg end poss))
(declare-function flyspell-get-word "flyspell" (&optional following extra-otherchars))

(defvar flyspell-mode)
(defvar flyspell-mode-map)
(defvar flyspell-generic-check-word-predicate)
(defvar ispell-program-name)
(defvar ispell-local-dictionary)
(defvar ispell-local-dictionary-alist)

(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))

(defcustom agent-repl-input-spellcheck t
  "Non-nil spellchecks the composer as you type.
Read when a composer is born, so a change applies to composers created
after it."
  :type 'boolean
  :group 'agent-repl)

(defconst agent-repl--spell-program "hunspell"
  "The spellchecker the composer runs (the manifest's `hunspell' entry).")

(defconst agent-repl--spell-requirement-id "spell-dictionary"
  "The id of the manifest entry that installs the composer's dictionary.")

(defconst agent-repl--spell-manifest
  (expand-file-name "requirements.json"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The Emacs layer's requirement manifest, which declares the dictionary.")

;;;; ---- The dictionary, as the manifest declares it ----------------------

(defun agent-repl--spell-cache-root ()
  "Return the cache dir `bin/requirements.sh' installs into.
The runner's `CACHE_ROOT': `$XDG_CACHE_HOME/agent-repl', with an unset or
empty `XDG_CACHE_HOME' meaning `~/.cache'."
  (let ((xdg (getenv "XDG_CACHE_HOME")))
    (expand-file-name "agent-repl"
                      (if (and xdg (not (string-empty-p xdg))) xdg "~/.cache"))))

(defun agent-repl--spell-manifest-entry (ws manifest)
  "Return the `spell-dictionary' entry of MANIFEST (a path) as a hash table.
The entry is part of this module, so its absence is a broken checkout and
is signalled, on WS's log."
  (let* ((root (with-temp-buffer
                 (insert-file-contents manifest)
                 (json-parse-buffer)))
         (entry (seq-find (lambda (e) (equal (gethash "id" e)
                                             agent-repl--spell-requirement-id))
                          (gethash "requirements" root))))
    (unless entry
      (agent-repl--fatal ws "elisp.spell.manifest ws=%s manifest=%s outcome=no-entry id=%s"
                         ws manifest agent-repl--spell-requirement-id))
    entry))

(defun agent-repl--spell-dictionary (ws &optional manifest)
  "Return the composer's dictionary as (:name NAME :dir DIR :files FILES).
Read from MANIFEST's `spell-dictionary' entry (the Emacs layer's own manifest
when nil): DIR is the cache dir it installs into, FILES the absolute path
of every file it installs, and NAME the hunspell dictionary name, the one
`.aff' file's name without its extension.  An entry that installs no
`.aff', more than one, or no `.dic' of the same name is not a hunspell
dictionary, which is a broken manifest and is signalled, on WS's log."
  (let* ((manifest (or manifest agent-repl--spell-manifest))
         (entry (agent-repl--spell-manifest-entry ws manifest))
         (dir (expand-file-name (gethash "dest" entry) (agent-repl--spell-cache-root)))
         (names (mapcar (lambda (f) (gethash "to" f)) (gethash "files" entry)))
         (affixes (seq-filter (lambda (n) (string-suffix-p ".aff" n)) names)))
    (unless (= (length affixes) 1)
      (agent-repl--fatal ws "elisp.spell.manifest ws=%s manifest=%s outcome=not-one-affix id=%s affixes=%d files=%S"
                         ws manifest agent-repl--spell-requirement-id (length affixes) names))
    (let ((name (file-name-sans-extension (car affixes))))
      (unless (member (concat name ".dic") names)
        (agent-repl--fatal ws "elisp.spell.manifest ws=%s manifest=%s outcome=no-dic id=%s name=%s files=%S"
                           ws manifest agent-repl--spell-requirement-id name names))
      (list :name name
            :dir dir
            :files (mapcar (lambda (n) (expand-file-name n dir)) names)))))

(defun agent-repl--spell-unmet (program dictionary)
  "Return what spellchecking lacks, as a list of strings; nil when nothing.
PROGRAM is hunspell's path (nil when it is not on PATH) and DICTIONARY the
plist `agent-repl--spell-dictionary' answers."
  (append (unless program
            (list (format "%s is not on PATH" agent-repl--spell-program)))
          (mapcar (lambda (file) (format "%s is absent" file))
                  (seq-remove #'file-exists-p (plist-get dictionary :files)))))

(defun agent-repl--spell-dictionary-entry (name)
  "Return the `ispell-local-dictionary-alist' entry for dictionary NAME.
Letters are what a word is made of, an apostrophe may sit inside one
\(\"don't\", \"it's\"), and the text is UTF-8.  No `-d' argument: ispell.el
adds `-d NAME' itself, and `DICPATH' tells hunspell where NAME lives."
  (list name "[[:alpha:]]" "[^[:alpha:]]" "['’]" t nil nil 'utf-8))

;;;; ---- What is prose ----------------------------------------------------

(defun agent-repl--spell-in-fence-p (pos)
  "Return non-nil when POS lies inside a fenced (```) code block.
A fence line itself counts as inside: its info string (```python) is a
language name, not prose."
  (save-excursion
    (goto-char pos)
    (let ((fences 0)
          (limit (line-end-position)))
      (goto-char (point-min))
      (while (and (<= (point) limit)
                  (re-search-forward "^[ \t]*```" limit t))
        (setq fences (1+ fences))
        (forward-line 1))
      (cl-oddp fences))))

(defun agent-repl--spell-in-inline-code-p (pos)
  "Return non-nil when POS lies inside an inline `code' span on its line."
  (save-excursion
    (goto-char pos)
    (cl-oddp (cl-count ?` (buffer-substring-no-properties (line-beginning-position) pos)))))

(defun agent-repl--spell-token-at (pos)
  "Return the whitespace-delimited token around POS."
  (save-excursion
    (goto-char pos)
    (let ((beg (progn (skip-chars-backward "^ \t\n") (point)))
          (end (progn (skip-chars-forward "^ \t\n") (point))))
      (buffer-substring-no-properties beg end))))

(defun agent-repl--spell-code-token-p (token)
  "Return non-nil when TOKEN reads as code rather than a word of prose.
A path, a URL or a slash command (`/'), a Windows path (`\\'), an
identifier (`_', `=', a dot inside it, or a lower-to-upper camelCase
step), or an address (`@')."
  (let ((case-fold-search nil))
    (string-match-p "[/\\\\_=@]\\|[[:alnum:]]\\.[[:alnum:]]\\|[[:lower:]][[:upper:]]" token)))

(defun agent-repl--spell-prose-at-p (pos)
  "Return non-nil when the word at POS is prose, which is spellchecked."
  (not (or (agent-repl--spell-in-fence-p pos)
           (agent-repl--spell-in-inline-code-p pos)
           (agent-repl--spell-code-token-p (agent-repl--spell-token-at pos)))))

(defun agent-repl--spell-check-word-p ()
  "The composer's `flyspell-generic-check-word-predicate'.
Non-nil when the word flyspell is about to check is prose."
  (let ((word (flyspell-get-word)))
    (and word (agent-repl--spell-prose-at-p (nth 1 word)))))

;;;; ---- Keys -------------------------------------------------------------

(defconst agent-repl--spell-released-keys '("C-M-i" "C-;")
  "flyspell keys the composer leaves to what they already do.
`C-M-i' is the composer's `completion-at-point' (slash commands), and `C-;'
is Doom's `embark-act'.  flyspell's other keys stay: `C-.' and `C-,'
correct and find a misspelling, and evil's `z=', `]s' and `[s' work too.")

(defun agent-repl--spell-keymap ()
  "Return flyspell's keymap without `agent-repl--spell-released-keys'.
A copy rather than a child: a key removed from a child still falls through
to its parent, while one removed from a copy falls through to the maps
below flyspell, where it was bound before."
  (let ((map (copy-keymap flyspell-mode-map)))
    (dolist (key agent-repl--spell-released-keys)
      (keymap-unset map key t))
    map))

;;;; ---- Text that arrives whole --------------------------------------------

(defvar-local agent-repl--spell-ws nil
  "The workspace this composer spellchecks for, named on its log records.")

(defvar-local agent-repl--spell-pending nil
  "(BEG . END) markers spanning insertions not yet checked, or nil.")

(defvar agent-repl--spell-pending-buffers nil
  "Composers holding queued insertions, checked on the next loop turn.")

(defvar agent-repl--spell-check-timer nil
  "The zero-second timer that checks the queue, or nil when none is armed.")

(defun agent-repl--spell-on-change (beg end _length)
  "Queue BEG..END for checking when more than one character was inserted.
The composer's `after-change-functions' entry.  A typed character is
flyspell's own to check; anything longer arrived whole.  Intentionally
unlogged: it runs on every keystroke."
  (when (> (- end beg) 1)
    (if agent-repl--spell-pending
        (progn
          (when (< beg (car agent-repl--spell-pending))
            (set-marker (car agent-repl--spell-pending) beg))
          (when (> end (cdr agent-repl--spell-pending))
            (set-marker (cdr agent-repl--spell-pending) end)))
      (setq agent-repl--spell-pending (cons (copy-marker beg) (copy-marker end t))))
    (cl-pushnew (current-buffer) agent-repl--spell-pending-buffers)
    (unless agent-repl--spell-check-timer
      (setq agent-repl--spell-check-timer
            (run-at-time 0 nil #'agent-repl--spell-check-pending-buffers)))))

(defun agent-repl--spell-check-pending-buffers ()
  "Check every composer's queued insertions, oldest queued first.
The queue's timer; it disarms itself, so the next insertion arms another."
  (let ((buffers (reverse agent-repl--spell-pending-buffers)))
    (setq agent-repl--spell-pending-buffers nil
          agent-repl--spell-check-timer nil)
    (mapc #'agent-repl--spell-check-pending buffers)))

(defun agent-repl--spell-check-pending (buffer)
  "Send BUFFER's queued insertions to hunspell, then forget the queue.
A BUFFER killed, or no longer spellchecked, before the check ran has
nothing left to check.  The span's markers live on until the answer is
applied (`agent-repl--spell-apply')."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((pending agent-repl--spell-pending))
        (setq agent-repl--spell-pending nil)
        (if (not flyspell-mode)
            (agent-repl--spell-release pending)
          (let ((ws agent-repl--spell-ws)
                (text (buffer-substring-no-properties (car pending) (cdr pending))))
            (agent-repl--spell-run-hunspell
             (agent-repl--spell-hunspell-input text)
             (lambda (ok output)
               (agent-repl--spell-answered ws buffer pending ok output)))))))))

(defun agent-repl--spell-release (span)
  "Point SPAN's two markers nowhere."
  (set-marker (car span) nil)
  (set-marker (cdr span) nil))

(defun agent-repl--spell-hunspell-input (text)
  "Return hunspell's `-a' input checking TEXT.
`!' asks for terse answers (nothing for a correct word), and every line of
TEXT is sent behind `^', so a line hunspell would read as a command (one
starting `*', `@', `#' and the like) is checked as words."
  (concat "!\n"
          (mapconcat (lambda (line) (concat "^" line "\n"))
                     (split-string text "\n") "")))

(defun agent-repl--spell-misspelled-words (output)
  "Return the words hunspell's `-a' OUTPUT names as misspelled, once each.
A word with suggestions answers `& WORD ...', one without `# WORD ...',
and a guess `? WORD ...'; every other line (the version banner, a blank
line closing each input line) names none."
  (let ((words nil))
    (dolist (line (split-string output "\n"))
      (when (string-match "\\`[&#?] \\([^ ]+\\)" line)
        (cl-pushnew (match-string 1 line) words :test #'equal)))
    (nreverse words)))

(defun agent-repl--spell-answered (ws buffer span ok output)
  "Apply hunspell's OUTPUT for BUFFER's SPAN, the check queued for WS.
OK nil is a hunspell that failed: recorded at WARN with what it said, and
nothing is highlighted.  A BUFFER killed, or no longer spellchecked, while
hunspell worked takes nothing."
  (unwind-protect
      (cond
       ((not ok)
        (agent-repl--warn ws "elisp.spell.check-failed ws=%s buffer=%s span-length=%d output=%S"
                          ws (buffer-name buffer)
                          (- (or (marker-position (cdr span)) 0) (or (marker-position (car span)) 0))
                          (string-trim output)))
       ((buffer-live-p buffer)
        (with-current-buffer buffer
          (when flyspell-mode
            (agent-repl--spell-apply (car span) (cdr span)
                                     (agent-repl--spell-misspelled-words output))))))
    (agent-repl--spell-release span)))

(defun agent-repl--spell-apply (beg end words)
  "Highlight each of WORDS wherever it stands as a prose word in BEG..END.
A match counts only where flyspell reads the same whole word starting
there (`flyspell-get-word'), so a misspelling inside a longer word is not
one, and only where it is prose (`agent-repl--spell-prose-at-p'), so code
stays unmarked as the as-you-type check leaves it."
  (save-excursion
    (let ((case-fold-search nil))
      (dolist (word words)
        (goto-char beg)
        (while (search-forward word end t)
          (let ((wbeg (match-beginning 0))
                (wend (match-end 0)))
            (save-excursion
              (goto-char wbeg)
              (let ((at (flyspell-get-word)))
                (when (and at (equal (car at) word) (= (nth 1 at) wbeg)
                           (agent-repl--spell-prose-at-p wbeg))
                  (flyspell-highlight-incorrect-region wbeg wend nil))))))))))

;;;; ---- External boundary --------------------------------------------------

(defun agent-repl--spell-flyspell-on ()
  "Turn `flyspell-mode' on in the current buffer, which starts hunspell.
Registered in `agent-repl--external-boundary-functions' (core.el)."
  (flyspell-mode 1)) ;; ALLOW-EXTERNAL-BOUNDARY

(defun agent-repl--spell-run-hunspell (input callback)
  "Run hunspell over INPUT with the composer's dictionary, then CALLBACK.
The current buffer's dictionary, the program `ispell-program-name' names
and the `DICPATH' `agent-repl--spell-configure-ispell' set; CALLBACK is
`agent-repl--async-command''s.  Registered in
`agent-repl--external-boundary-functions' (core.el)."
  (agent-repl--async-command
   "spell" temporary-file-directory
   (list ispell-program-name "-a" "-i" "utf-8" "-d" ispell-local-dictionary)
   callback input))

;;;; ---- Turning it on --------------------------------------------------------

(defun agent-repl--spell-configure-ispell (ws program name dir)
  "Point Emacs's spellchecker at PROGRAM and dictionary NAME in DIR, for WS.
Global, because ispell.el reads all three from inside temporary buffers
\(see the commentary).  `ispell-program-name' becomes PROGRAM, recorded
when it changes; DIR leads hunspell's `DICPATH'; NAME is registered in
`ispell-local-dictionary-alist'; and `DICTIONARY' names NAME unless you
set your own, so ispell.el's discovery, which asks hunspell for its
default dictionary, finds one.  Idempotent."
  (unless (equal ispell-program-name program)
    (agent-repl--info ws "elisp.spell.configure ws=%s program-from=%s program-to=%s"
                      ws ispell-program-name program)
    (setq ispell-program-name program))
  (let ((paths (split-string (or (getenv "DICPATH") "") ":" t)))
    (unless (equal (car paths) dir)
      (setenv "DICPATH" (string-join (cons dir (delete dir paths)) ":"))))
  (unless (getenv "DICTIONARY")
    (setenv "DICTIONARY" name))
  (unless (assoc name ispell-local-dictionary-alist)
    (push (agent-repl--spell-dictionary-entry name) ispell-local-dictionary-alist)))

(defun agent-repl-spell-enable (ws)
  "Spellcheck WS's composer, the current buffer, when it can be.
Off when `agent-repl-input-spellcheck' is nil.  With hunspell or the
manifest's dictionary missing, the composer is not spellchecked and the
log says what is missing and how to install it, once per Emacs."
  (require 'flyspell)
  (if (not agent-repl-input-spellcheck)
      (agent-repl--log ws "elisp.spell.enable ws=%s outcome=off reason=agent-repl-input-spellcheck-nil" ws)
    (let* ((dictionary (agent-repl--spell-dictionary ws))
           (program (agent-repl--executable-find agent-repl--spell-program))
           (unmet (agent-repl--spell-unmet program dictionary)))
      (if unmet
          (agent-repl--warn-once
           ws (format "spell-unmet:%S" unmet)
           "elisp.spell.enable ws=%s outcome=unavailable unmet=%S fix=%s"
           ws unmet "run bin/requirements.sh install (or /agent-repl-install)")
        (let ((name (plist-get dictionary :name))
              (dir (plist-get dictionary :dir)))
          (agent-repl--spell-configure-ispell ws program name dir)
          (setq-local agent-repl--spell-ws ws)
          (setq-local ispell-local-dictionary name)
          (setq-local flyspell-generic-check-word-predicate #'agent-repl--spell-check-word-p)
          (setq-local minor-mode-overriding-map-alist
                      (cons (cons 'flyspell-mode (agent-repl--spell-keymap))
                            minor-mode-overriding-map-alist))
          (add-hook 'after-change-functions #'agent-repl--spell-on-change nil t)
          (agent-repl--spell-flyspell-on)
          (agent-repl--log ws "elisp.spell.enable ws=%s outcome=on program=%s dictionary=%s dir=%s"
                           ws program name dir))))))

(provide 'agent-repl-spell)

;;; spell.el ends here
