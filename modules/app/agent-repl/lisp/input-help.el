;;; input-help.el --- the composer's clickable help bar -*- lexical-binding: t; -*-

;;; Commentary:

;; THE COMPOSER'S HEADER LINE IS A ROW OF HELP BUTTONS.  Each button names
;; one topic ("Sending prompts", "Prompt history", ...), and clicking it
;; opens the shared agent-repl popup (popup.el) listing that topic's keys
;; and the features that go with them.  `agent-repl-input-help' opens the
;; same popups from the keyboard.
;;
;; THE KEYS SHOWN ARE READ FROM THE LIVE KEYMAPS, never written down here.
;; A topic names COMMANDS; the popup asks the composer's mode map (with its
;; Evil state maps), the global maps a `[remap ...]' entry defers to, and
;; Doom's leader map which keys run each one.  A rebinding therefore shows
;; up in the popup by construction.  A topic command no map binds is a
;; broken table and is fatal, and test-input-help.el pins both directions:
;; every topic command is bound, and every composer binding is in a topic.

;;; Code:

(require 'cl-lib)
(require 'seq)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--fatal "core" (ws fmt &rest args))
(declare-function agent-repl-popup-show "popup" (buffer))
(declare-function agent-repl-defface "core" (face spec doc &rest args))
(declare-function agent-repl-refill-keymap "core" (map fill))
(defvar agent-repl-input-mode-map)
(defvar evil-normal-state-map)
(defvar evil-insert-state-map)
(defvar doom-leader-map)
(defvar doom-leader-key)

(defconst agent-repl-input-help--log-scope
  '(:agent-repl-central "the composer's help is one fixed table, not workspace-scoped")
  "The log scope every composer-help record is filed under.")

(defconst agent-repl-input-help-topics
  '((:id sending
     :title "Sending prompts"
     :bindings ((agent-repl-send "Send the composer's contents.")
                (newline "Insert a newline instead of sending.")
                (agent-repl-input-control-return
                 "Send with the send postfix appended. With a multi-select standing and an empty composer, confirm its ticks instead."
                 :variables (agent-repl-send-postfix))
                (agent-repl-send-with-prefix "Send with the send prefix prepended."
                                             :variables (agent-repl-send-prefix))
                (agent-repl-queue-deferred-prompt "Defer the prompt until the current turn ends.")
                (agent-repl-interrupt-turn "Interrupt the running turn."))
     :features ("Prompts sent while the daemon is away wait, counted in the mode line, and go out once it is back."))
    (:id history
     :title "Prompt history"
     :bindings ((agent-repl-discard-input "Save the composer to history, then clear it.")
                (agent-repl--history-prev "Show the previous (older) history entry.")
                (agent-repl--history-next "Show the next (newer) history entry.")
                (agent-repl-history-search "Search the whole history and pick an entry."))
     :features ("Every sent prompt is saved to the workspace's history, which survives restarts."
                "Walking past the newest entry restores the draft you were typing."))
    (:id images
     :title "Images"
     :bindings ((agent-repl-attach-clipboard-image "Attach the clipboard's image, even when text rides beside it.")
                (agent-repl-input-yank "Paste the clipboard's image, or yank as usual.")
                (agent-repl-input-evil-paste-after "Evil's p: the clipboard's image, or paste after.")
                (agent-repl-input-evil-paste-before "Evil's P: the clipboard's image, or paste before."))
     :features ("An image is one character of the prompt: delete, kill or yank it like text."
                "Clicking an image in the composer opens it large over the feed."))
    (:id answering
     :title "Answering cards"
     :bindings ((agent-repl-input-digit "Pick the standing card's numbered choice.")
                (agent-repl-input-control-return "Confirm a multi-select's ticked choices.")
                (agent-repl-input-insert-escape "Close a standing news digest and leave insert mode."))
     :features ("A digit answers only in an empty composer; anywhere else it is typed as usual."
                "On a multi-select a digit toggles its choice's tick instead of answering."))
    (:id rollback
     :title "Rollback"
     :bindings ((agent-repl-turn-boundary-select-next "Select the next (newer) prompt or final response.")
                (agent-repl-turn-boundary-select-prev "Select the previous (older) prompt or final response.")
                (agent-repl-input-selection-escape "Clear the feed selection.")
                (agent-repl-rollback-keep-files "Roll back to before the selected prompt, keeping files.")
                (agent-repl-rollback-restore-files "Roll back to before the selected prompt, restoring files."))
     :features ("With no prompt selected, a rollback cancels the latest prompt."
                "A rollback asks before it acts."
                "Restoring files restores only the files the agent's edit tools changed."))
    (:id completion
     :title "Completion"
     :bindings ()
     :features ("A word starting with / completes against the workspace's commands, and a known one turns purple."
                "A word that begins an open workspace's name completes to that name."
                "Only TAB takes a completion: RET closes the completion popup and sends."))
    (:id zoom
     :title "Feed zoom"
     :bindings ((agent-repl-feed-text-scale-increase "Make the feed's text larger.")
                (agent-repl-feed-text-scale-decrease "Make the feed's text smaller."))
     :features ("The zoom changes the feed's text only, never Emacs's own font.")))
  "The composer's help topics, in header-line order.
Each is a plist: `:id' (a symbol), `:title' (the button's label and the
popup's heading), `:bindings' and `:features'.  A binding is
\(COMMAND DESCRIPTION [:variables (VARIABLE ...)]): its keys are read from
the live keymaps when the popup opens, and each VARIABLE whose value
shapes what COMMAND does is shown with its current value.  `:features'
are lines describing what goes with those keys.")

(agent-repl-defface 'agent-repl-input-help-button
  '((t :foreground "#8b8e91" :background "#26282b" :weight normal
       :box (:line-width (2 . -1) :style released-button)))
  "Face of one help button in the composer's header line.
Raised and filled a step above the header line's ground, with grey ink so
the row stays quieter than the composer's own text.")

(agent-repl-defface 'agent-repl-input-help-title
  '((t :inherit bold :height 1.3))
  "Face of a composer help popup's title.")

(agent-repl-defface 'agent-repl-input-help-section
  '((t :inherit shadow :weight bold))
  "Face of a section heading (Keys, Good to know) in a composer help popup.")

(agent-repl-defface 'agent-repl-input-help-annotation
  '((t :inherit shadow :slant italic))
  "Face of the Evil state a key in a composer help popup is bound in.")

(agent-repl-defface 'agent-repl-input-help-variable
  '((t :inherit (shadow button)))
  "Face of a variable named in a composer help popup; clicking describes it.")

;;;; ---- The header line ------------------------------------------------

(defvar agent-repl-input-help-button-map (make-sparse-keymap)
  "Keymap of every help button in the composer's header line.")

(agent-repl-refill-keymap
 agent-repl-input-help-button-map
 (lambda (map)
   (define-key map [header-line mouse-1] #'agent-repl-input-help-click)
   ;; A press on a button is not a header-line drag.
   (define-key map [header-line down-mouse-1] #'ignore)))

(defun agent-repl--input-help-button (topic)
  "Return TOPIC's header-line button, carrying its id for the click."
  (let ((title (plist-get topic :title)))
    (propertize (concat "  " title "  ")
                'face 'agent-repl-input-help-button
                'mouse-face 'mode-line-highlight
                'pointer 'hand
                'help-echo (concat "mouse-1: keys and features for " title)
                'local-map agent-repl-input-help-button-map
                'agent-repl-input-help-topic (plist-get topic :id))))

(defun agent-repl--input-help-header-line-text ()
  "Return the composer's header line: one help button per topic."
  (concat " " (mapconcat #'agent-repl--input-help-button agent-repl-input-help-topics "  ")))

;; EVERY COMPOSER'S HEADER LINE IS THIS ONE VARIABLE, never a copy of it.
;; `header-line-format' holds the symbol, which redisplay reads afresh, so
;; rebuilding the value on every load (the `setq' below, since `defvar'
;; initializes once) updates every open composer by construction.  A
;; symbol's string value keeps its text properties, the buttons, only when
;; the symbol is risky.
(defvar agent-repl-input-help-header-line nil
  "The composer's header line, every composer's `header-line-format'.")
(put 'agent-repl-input-help-header-line 'risky-local-variable t)
(setq agent-repl-input-help-header-line (agent-repl--input-help-header-line-text))

(defun agent-repl-input-help-install ()
  "Make the current buffer's header line `agent-repl-input-help-header-line'."
  (setq-local header-line-format 'agent-repl-input-help-header-line)
  (force-mode-line-update))

(defun agent-repl--input-help-install-in-composers ()
  "Install the help header line in every open composer; return how many.
A composer made before this file last loaded may hold any header line an
older load gave it, so each load points every one at the variable."
  (let ((count 0))
    (dolist (buf (buffer-list))
      (when (eq (buffer-local-value 'major-mode buf) 'agent-repl-input-mode)
        (with-current-buffer buf (agent-repl-input-help-install))
        (setq count (1+ count))))
    (agent-repl--log agent-repl-input-help--log-scope
                     "elisp.input-help.install-in-composers: composers=%d" count)
    count))

;;;; ---- Finding a command's keys ---------------------------------------

(defun agent-repl--input-help-binding-context ()
  "Return the live keymaps a help popup reads keys from.
`:composer' is the composer's mode map (its Evil state maps ride inside
it), `:global' the (STATE . MAP) pairs a `[remap ...]' entry resolves
through (STATE nil for any state), `:leader' Doom's leader map, and
`:leader-key' the key that opens it."
  (list :composer agent-repl-input-mode-map
        :global (list (cons "insert" evil-insert-state-map)
                      (cons "normal" evil-normal-state-map)
                      (cons nil (current-global-map)))
        :leader doom-leader-map
        :leader-key doom-leader-key))

(defun agent-repl--input-help-state-key (key)
  "Split KEY, a key vector from the composer's map, into (STATE . REST).
Evil keeps a mode map's state bindings under a `normal-state' (and so on)
prefix event, so STATE is that state's name, or nil for a plain binding."
  (let ((first (aref key 0)))
    (if (and (symbolp first)
             (string-match "\\`\\(.+\\)-state\\'" (symbol-name first)))
        (cons (match-string 1 (symbol-name first)) (seq-subseq key 1))
      (cons nil key))))

(defun agent-repl--input-help-raw-keys (command context)
  "Return COMMAND's keys in CONTEXT as (DESCRIPTION . STATE) pairs.
STATE is the Evil state name a key is bound in, or nil for any state."
  (let (found)
    (dolist (key (where-is-internal command (plist-get context :composer) nil t t))
      (if (eq (aref key 0) 'remap)
          (pcase-dolist (`(,state . ,map) (plist-get context :global))
            (dolist (global (where-is-internal (aref key 1) map nil t))
              (push (cons (key-description global) state) found)))
        (pcase-let ((`(,state . ,rest) (agent-repl--input-help-state-key key)))
          (push (cons (key-description rest) state) found))))
    (dolist (key (where-is-internal command (plist-get context :leader) nil t))
      (push (cons (concat (plist-get context :leader-key) " " (key-description key)) nil)
            found))
    (nreverse found)))

(defun agent-repl--input-help-state-label (states)
  "Return the annotation for a key bound in STATES, or nil for none.
STATES holds nil when the key is bound in any state.  A key reachable in
both command and insert mode needs no annotation."
  (unless (or (memq nil states)
              (and (member "normal" states) (member "insert" states)))
    (mapconcat (lambda (state)
                 (pcase state
                   ("normal" "command mode")
                   ("insert" "insert mode")
                   (_ (concat state " mode"))))
               (sort (delete-dups (copy-sequence states)) #'string<)
               ", ")))

(defun agent-repl--input-help-collapse-digits (keys)
  "Return KEYS with each run of consecutive single digits as one range key.
KEYS are (KEY . ANNOTATION) pairs; a run is digits each one more than the
last, sharing an annotation, so 1 2 ... 9 reads as \"1-9\"."
  (let (out)
    (dolist (key keys)
      (let ((prev (car out)))
        (if (and prev
                 (string-match-p "\\`[0-9]\\'" (car key))
                 (string-match "\\`\\([0-9]\\)\\(?:-\\([0-9]\\)\\)?\\'" (car prev))
                 (equal (cdr prev) (cdr key))
                 (= (string-to-number (car key))
                    (1+ (string-to-number (or (match-string 2 (car prev))
                                              (match-string 1 (car prev)))))))
            (setcar out (cons (concat (match-string 1 (car prev)) "-" (car key)) (cdr key)))
          (push key out))))
    (nreverse out)))

(defun agent-repl--input-help-keys (command context)
  "Return the keys that run COMMAND in CONTEXT, as (KEY . ANNOTATION) pairs.
Shortest first, then alphabetically: a keymap's own order is the reverse
of its definitions, which says nothing to a reader.  A key bound in several states is one pair, annotated
with its states (`agent-repl--input-help-state-label', nil for none), and
a run of digits is one range (`agent-repl--input-help-collapse-digits').
A command no map binds is a broken help table, and is fatal."
  (let ((raw (agent-repl--input-help-raw-keys command context))
        order)
    (dolist (pair raw)
      (unless (member (car pair) order)
        (push (car pair) order)))
    (unless order
      (agent-repl--fatal agent-repl-input-help--log-scope
                         "elisp.input-help.keys: unbound command=%s" command))
    (agent-repl--input-help-collapse-digits
     (mapcar (lambda (description)
               (cons description
                     (agent-repl--input-help-state-label
                      (mapcar #'cdr (seq-filter (lambda (pair) (equal (car pair) description))
                                                raw)))))
             (sort order (lambda (a b)
                           (or (< (length a) (length b))
                               (and (= (length a) (length b)) (string< a b)))))))))

;;;; ---- The popup ------------------------------------------------------

(defun agent-repl--input-help-topic (key value)
  "Return the help topic whose KEY (`:id' or `:title') is VALUE.
No such topic is fatal."
  (or (seq-find (lambda (topic) (equal (plist-get topic key) value)) agent-repl-input-help-topics)
      (agent-repl--fatal agent-repl-input-help--log-scope
                         "elisp.input-help.topic: unknown %s=%S known=%S"
                         key value (mapcar (lambda (topic) (plist-get topic key))
                                           agent-repl-input-help-topics))))

(defconst agent-repl-input-help--key-column-max 24
  "The widest key cell a popup row keeps beside its description.
A wider cell puts its description on the next line, at the same column.")

(defun agent-repl--input-help-key-cell (keys)
  "Return KEYS, (KEY . ANNOTATION) pairs, as one row's keycaps."
  (mapconcat (lambda (key)
               (concat (propertize (car key) 'face 'help-key-binding)
                       (and (cdr key)
                            (concat " " (propertize (cdr key) 'face 'agent-repl-input-help-annotation)))))
             keys "  "))

(defun agent-repl--input-help-rows (topic context)
  "Return TOPIC's rows as (CELL DESCRIPTION VARIABLES), keys read from CONTEXT.
Every key and variable is resolved here, before any buffer is touched: a
command no map binds, or a variable that is not bound, is fatal."
  (mapcar (lambda (binding)
            (pcase-let ((`(,command ,description . ,props) binding))
              (let ((variables (plist-get props :variables)))
                (dolist (variable variables)
                  (unless (boundp variable)
                    (agent-repl--fatal agent-repl-input-help--log-scope
                                       "elisp.input-help.rows: unbound variable=%s command=%s"
                                       variable command)))
                (list (agent-repl--input-help-key-cell
                       (agent-repl--input-help-keys command context))
                      description variables))))
          (plist-get topic :bindings)))

(defun agent-repl--input-help-variable-line (variable)
  "Return VARIABLE's line: its name, a button that describes it, and its value."
  (let ((print-escape-newlines t))
    (concat (make-text-button (symbol-name variable) nil
                              'face 'agent-repl-input-help-variable
                              'action (lambda (_) (describe-variable variable))
                              'help-echo (concat "mouse-1: describe " (symbol-name variable)))
            ": "
            (propertize (prin1-to-string (symbol-value variable)) 'face 'font-lock-string-face))))

(defun agent-repl--input-help-insert-row (row column)
  "Insert ROW, (CELL DESCRIPTION VARIABLES), its text wrapping at COLUMN."
  (pcase-let ((`(,cell ,description ,variables) row)
              (start (point))
              (align `(space :align-to ,column)))
    (insert "  " cell)
    (when (> (+ 2 (string-width cell)) (- column 2))
      (insert "\n"))
    (insert (propertize " " 'display align) description "\n")
    (dolist (variable variables)
      (insert (propertize " " 'display align) (agent-repl--input-help-variable-line variable) "\n"))
    (put-text-property start (point) 'wrap-prefix align)))

(defun agent-repl--input-help-insert (topic rows)
  "Insert TOPIC's help, with its resolved ROWS, at point."
  (let ((column (+ 2 (min agent-repl-input-help--key-column-max
                          (apply #'max 0 (mapcar (lambda (row) (string-width (car row))) rows)))
                   3))
        (features (plist-get topic :features)))
    (insert (propertize (plist-get topic :title) 'face 'agent-repl-input-help-title) "\n")
    (when rows
      (insert "\n" (propertize "Keys" 'face 'agent-repl-input-help-section) "\n")
      (dolist (row rows)
        (agent-repl--input-help-insert-row row column)))
    (when features
      (insert "\n" (propertize "Good to know" 'face 'agent-repl-input-help-section) "\n")
      (dolist (line features)
        (let ((start (point)))
          (insert "  • " line "\n")
          (put-text-property start (point) 'wrap-prefix "    "))))))

(defun agent-repl--input-help-buffer (topic context)
  "Return TOPIC's help buffer, filled from CONTEXT and read-only.
Its lines wrap at word boundaries, each under its own column."
  (let* ((rows (agent-repl--input-help-rows topic context))
         (buf (get-buffer-create (format "*agent-repl help: %s*" (plist-get topic :title)))))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-repl--input-help-insert topic rows))
      (setq-local truncate-lines nil)
      (setq-local line-spacing 0.2)
      (visual-line-mode 1)
      (goto-char (point-min)))
    buf))

(defun agent-repl-input-help-show (id)
  "Show help topic ID's keys and features in the shared agent-repl popup."
  (let* ((topic (agent-repl--input-help-topic :id id))
         (buf (agent-repl--input-help-buffer topic (agent-repl--input-help-binding-context))))
    (agent-repl--log agent-repl-input-help--log-scope "elisp.input-help.show: id=%s buffer=%s"
                     id (buffer-name buf))
    (agent-repl-popup-show buf)))

(defun agent-repl-input-help-click (event)
  "Show the help topic of the header-line button EVENT clicked."
  (interactive "e")
  (let* ((string (posn-string (event-start event)))
         (id (and string
                  (get-text-property (cdr string) 'agent-repl-input-help-topic (car string)))))
    (unless id
      (agent-repl--fatal agent-repl-input-help--log-scope
                         "elisp.input-help.click: no topic at posn string=%S" string))
    (agent-repl-input-help-show id)))

(defun agent-repl-input-help (title)
  "Show the composer help topic named TITLE, read with completion."
  (interactive
   (list (completing-read "Composer help: "
                          (mapcar (lambda (topic) (plist-get topic :title))
                                  agent-repl-input-help-topics)
                          nil t)))
  (agent-repl-input-help-show (plist-get (agent-repl--input-help-topic :title title) :id)))

(agent-repl--input-help-install-in-composers)

(provide 'input-help)

;;; input-help.el ends here
