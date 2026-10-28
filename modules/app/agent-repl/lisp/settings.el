;;; settings.el --- the Settings page: every key binding, editable -*- lexical-binding: t; -*-

;;; Commentary:

;; THE SETTINGS PAGE LISTS EVERY KEY BINDING AGENT-REPL SETS, AND CHANGES THEM.
;; Its sections are the help's topics (`agent-repl-input-help-topics'), so the
;; help and the page read one registry and cannot disagree.  A section folds
;; open and shut; in it, each command's row shows its keys as buttons:
;; clicking a key changes or removes it, `+' adds a key, and a changed row
;; can be reset.  A key is entered as any sequence of keys, ended by ESC.
;; The variables that shape a binding (the send prefix and postfix) are
;; shown with their values, and can be edited there too.
;;
;; EDITS ARE A LAYER OVER THE DEFAULTS, NEVER EDITS TO THEM.  Each edit is
;; one override, saved in `agent-repl-settings-file' (~/zettel/settings.eld):
;;
;;   (:keymap VARIABLE :key KEY :command COMMAND :was DEFAULT)
;;
;; KEY in VARIABLE's keymap runs COMMAND (nil: unbound) where by default it
;; ran DEFAULT (nil: nothing).  Every load of this file applies the whole
;; layer after every other module has bound its keys, so a reload or a
;; deploy rebuilds the defaults and then lays the edits back over them by
;; construction.  An override for a keymap not loaded yet (magit's) waits
;; on `after-load-functions' until it is.  Resetting an override binds KEY
;; back to DEFAULT and drops it.
;;
;; WHERE AN ADDED KEY IS BOUND follows its section: a section with its own
;; keymaps binds into the first, in every mode, command mode or insert mode
;; as asked; a section without binds a key starting with the leader into
;; Doom's leader map, and any other key globally.

;;; Code:

(require 'cl-lib)
(require 'seq)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--fatal "core" (ws fmt &rest args))
(declare-function agent-repl--write-file-atomically "core" (file text &optional binary))
(declare-function agent-repl-defface "core" (face spec doc &rest args))
(declare-function agent-repl-refill-keymap "core" (map fill))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function agent-repl-popup-show "popup" (buffer))
(declare-function agent-repl-popup-quit "popup" ())
(declare-function agent-repl--set-buffer-background "core" (color))
(declare-function agent-repl--input-help-binding-context "input-help" (topic))
(declare-function agent-repl--input-help-key-sources "input-help" (command context))
(declare-function agent-repl--input-help-insert-row "input-help" (cell description lines column))
(declare-function agent-repl--input-help-key-column "input-help" (cells))
(declare-function agent-repl--input-help-groups "input-help" ())
(declare-function agent-repl--input-help-group-sources "input-help" (sources))
(declare-function agent-repl--input-help-group-label "input-help" (sources))
(declare-function agent-repl--input-help-variable-value "input-help" (variable))
(declare-function agent-repl--input-help-topic "input-help" (key value))
(defvar agent-repl-input-help-topics)
(defvar doom-leader-key)

(defconst agent-repl-settings--log-scope
  '(:agent-repl-central "the settings are one per Emacs, not per workspace")
  "The log scope every settings record is filed under.")

(defcustom agent-repl-zettel-directory "~/zettel/"
  "The directory ZETTEL keeps its user data in."
  :type 'directory
  :group 'agent-repl)

(defun agent-repl-settings-file ()
  "Return the file the Settings page saves its edits in."
  (expand-file-name "settings.eld" agent-repl-zettel-directory))

;;;; ---- The saved settings ---------------------------------------------

(defvar agent-repl-settings-overrides nil
  "The key binding overrides in force, oldest first.
Each is (:keymap VARIABLE :key KEY :command COMMAND :was DEFAULT).")

(defvar agent-repl-settings-variables nil
  "The variable values the Settings page set, as (VARIABLE . VALUE).")

(defun agent-repl-settings--valid-override-p (override)
  "Return non-nil when OVERRIDE has the saved shape."
  (and (symbolp (plist-get override :keymap))
       (plist-get override :keymap)
       (stringp (plist-get override :key))
       (symbolp (plist-get override :command))
       (symbolp (plist-get override :was))))

(defun agent-repl-settings--read ()
  "Return the saved settings as (:keybindings OVERRIDES :variables ALIST).
A missing file is no settings yet.  A file that does not read as that
shape is fatal: a broken settings file is surfaced, never ignored."
  (let ((file (agent-repl-settings-file)))
    (if (not (file-exists-p file))
        (list :keybindings nil :variables nil)
      (let ((data (condition-case err
                      (with-temp-buffer
                        (insert-file-contents file)
                        (read (current-buffer)))
                    (error
                     (agent-repl--fatal agent-repl-settings--log-scope
                                        "elisp.settings.read: unreadable file=%s cause=%S"
                                        file err)))))
        (unless (and (listp data)
                     (listp (plist-get data :keybindings))
                     (cl-every #'agent-repl-settings--valid-override-p (plist-get data :keybindings))
                     (listp (plist-get data :variables))
                     (cl-every (lambda (pair) (and (consp pair) (symbolp (car pair))))
                               (plist-get data :variables)))
          (agent-repl--fatal agent-repl-settings--log-scope
                             "elisp.settings.read: malformed file=%s data=%S" file data))
        data))))

(defun agent-repl-settings--save ()
  "Write the overrides and variables in force to `agent-repl-settings-file'."
  (let ((file (agent-repl-settings-file)))
    (make-directory (file-name-directory file) t)
    (agent-repl--write-file-atomically
     file
     (concat ";; ZETTEL settings, written by the Settings page.  -*- lisp-data -*-\n"
             (pp-to-string (list :keybindings agent-repl-settings-overrides
                                 :variables agent-repl-settings-variables))))
    (agent-repl--log agent-repl-settings--log-scope
                     "elisp.settings.save: file=%s overrides=%d variables=%d"
                     file (length agent-repl-settings-overrides)
                     (length agent-repl-settings-variables))))

;;;; ---- Applying the layer ---------------------------------------------

(defvar agent-repl-settings--pending nil
  "Overrides whose keymap is not loaded yet, waiting on a later load.")

(defun agent-repl-settings--bind (variable key command)
  "Bind KEY, a `key-description', to COMMAND in VARIABLE's keymap."
  (define-key (symbol-value variable) (kbd key) command))

(defun agent-repl-settings--apply (override)
  "Apply OVERRIDE now, or return nil when its keymap is not loaded yet."
  (let ((variable (plist-get override :keymap)))
    (when (and (boundp variable) (keymapp (symbol-value variable)))
      (agent-repl-settings--bind variable (plist-get override :key) (plist-get override :command))
      t)))

(defun agent-repl-settings--apply-pending (&rest _)
  "Apply every waiting override whose keymap has loaded since."
  (setq agent-repl-settings--pending
        (seq-remove #'agent-repl-settings--apply agent-repl-settings--pending))
  (unless agent-repl-settings--pending
    (remove-hook 'after-load-functions #'agent-repl-settings--apply-pending)))

(defun agent-repl-settings-apply ()
  "Read the saved settings and lay them over the defaults.
Returns the number of overrides applied now; the rest wait for their
keymap's load (`agent-repl-settings--apply-pending')."
  (let ((data (agent-repl-settings--read)))
    (setq agent-repl-settings-overrides (plist-get data :keybindings)
          agent-repl-settings-variables (plist-get data :variables))
    (pcase-dolist (`(,variable . ,value) agent-repl-settings-variables)
      (set-default variable value))
    (setq agent-repl-settings--pending
          (seq-remove #'agent-repl-settings--apply agent-repl-settings-overrides))
    (when agent-repl-settings--pending
      (add-hook 'after-load-functions #'agent-repl-settings--apply-pending))
    (agent-repl--log agent-repl-settings--log-scope
                     "elisp.settings.apply: overrides=%d pending=%d variables=%d"
                     (length agent-repl-settings-overrides) (length agent-repl-settings--pending)
                     (length agent-repl-settings-variables))
    (- (length agent-repl-settings-overrides) (length agent-repl-settings--pending))))

;;;; ---- Changing the layer ---------------------------------------------

(defun agent-repl-settings--find (variable key)
  "Return the override of KEY in VARIABLE's keymap, or nil."
  (seq-find (lambda (override)
              (and (eq (plist-get override :keymap) variable)
                   (equal (plist-get override :key) key)))
            agent-repl-settings-overrides))

(defun agent-repl-settings--set-key (variable key command)
  "Make KEY in VARIABLE's keymap run COMMAND (nil unbinds it), and save.
An earlier override of the same key keeps its DEFAULT, so a reset always
goes back to the module's own binding; a key set back to its default
drops its override."
  (let* ((earlier (agent-repl-settings--find variable key))
         (was (if earlier
                  (plist-get earlier :was)
                (let ((binding (lookup-key (symbol-value variable) (kbd key))))
                  (and (symbolp binding) binding)))))
    (setq agent-repl-settings-overrides (delq earlier agent-repl-settings-overrides))
    (unless (eq command was)
      (setq agent-repl-settings-overrides
            (append agent-repl-settings-overrides
                    (list (list :keymap variable :key key :command command :was was)))))
    (agent-repl-settings--bind variable key command)
    (agent-repl--log agent-repl-settings--log-scope
                     "elisp.settings.set-key: keymap=%s key=%S command=%s was=%s"
                     variable key command was)
    (agent-repl-settings--save)))

(defun agent-repl-settings--reset (overrides)
  "Bind each of OVERRIDES's keys back to its default, drop them, and save."
  (dolist (override overrides)
    (agent-repl-settings--bind (plist-get override :keymap) (plist-get override :key)
                               (plist-get override :was))
    (setq agent-repl-settings-overrides (delq override agent-repl-settings-overrides)))
  (agent-repl--log agent-repl-settings--log-scope "elisp.settings.reset: overrides=%d"
                   (length overrides))
  (agent-repl-settings--save))

(defun agent-repl-settings--overrides-of (commands)
  "Return the overrides that bind, or unbound, any of COMMANDS."
  (seq-filter (lambda (override)
                (or (memq (plist-get override :command) commands)
                    (memq (plist-get override :was) commands)))
              agent-repl-settings-overrides))

(defun agent-repl-settings--set-variable (variable value)
  "Set VARIABLE to VALUE, and save it with the settings."
  (set-default variable value)
  (setf (alist-get variable agent-repl-settings-variables) value)
  (agent-repl--log agent-repl-settings--log-scope "elisp.settings.set-variable: variable=%s value=%S"
                   variable value)
  (agent-repl-settings--save))

;;;; ---- Entering a key -------------------------------------------------

(defun agent-repl-settings-read-keys (prompt)
  "Read keys until ESC and return them as a vector, PROMPT naming the edit.
ESC with no key entered is no key, and a `user-error'."
  (let (keys)
    (catch 'done
      (while t
        (let ((event (read-key (format "%s: %s  (ESC to finish)" prompt
                                       (key-description (vconcat (reverse keys)))))))
          (if (memq event '(27 escape))
              (throw 'done nil)
            (push event keys)))))
    (unless keys
      (user-error "No key entered"))
    (vconcat (nreverse keys))))

(defconst agent-repl-settings--modes
  '(("every mode" . nil) ("command mode" . "normal") ("insert mode" . "insert"))
  "The modes a key added to a section's own keymap can be bound in.")

(defun agent-repl-settings--target (topic keys)
  "Return (VARIABLE . KEY) where adding KEYS for TOPIC binds them.
KEY is a `key-description' in VARIABLE's keymap: TOPIC's first keymap in
the mode asked for; with no keymaps of its own, Doom's leader map for a
sequence starting with the leader, the global map for any other."
  (let ((keymaps (plist-get topic :keymaps))
        (leader (vconcat (kbd doom-leader-key))))
    (cond
     (keymaps
      (let ((state (cdr (assoc (completing-read "Bind it in: " agent-repl-settings--modes nil t)
                               agent-repl-settings--modes))))
        (cons (car keymaps)
              (key-description (if state
                                   (vconcat (vector (intern (concat state "-state"))) keys)
                                 keys)))))
     ((and (> (length keys) (length leader))
           (equal (seq-subseq keys 0 (length leader)) leader))
      (cons 'doom-leader-map (key-description (seq-subseq keys (length leader)))))
     (t (cons 'global-map (key-description keys))))))

(defun agent-repl-settings--check-free (variable key command)
  "Make sure KEY in VARIABLE's keymap can be given to COMMAND.
A key that runs another command is taken over only when the user says
so; one that is a prefix of other keys, or extends a key that is not a
prefix, cannot be bound at all.  Each refusal is a `user-error'."
  (let* ((map (symbol-value variable))
         (vector (kbd key))
         (binding (lookup-key map vector)))
    ;; A number is how many leading keys bind something that is not a
    ;; keymap: nothing (the prefix is simply free), or a command.
    (when (numberp binding)
      (let ((prefix (lookup-key map (seq-take vector binding))))
        (when prefix
          (user-error "%s starts with a key that is not a prefix" key))
        (setq binding nil)))
    (cond
          ((keymapp binding)
           (user-error "%s is a prefix of other keys" key))
          ((and binding (not (eq binding command))
                (not (y-or-n-p (format "%s runs %s.  Give it to %s? " key binding command))))
           (user-error "%s keeps running %s" key binding)))))

;;;; ---- The page -------------------------------------------------------

(defvar-local agent-repl-settings--open nil
  "The ids of the sections this page shows open.")

(defun agent-repl-settings--keycap (command group topic)
  "Return GROUP's key as a keycap button that changes or removes it for COMMAND.
GROUP is (DESCRIPTION . SOURCES): one key, bound in every one of SOURCES
\(`agent-repl--input-help-group-sources'), so the button acts on all."
  (concat (make-text-button (concat " " (car group) " ") nil
                            'face 'agent-repl-settings-keycap
                            'mouse-face 'agent-repl-settings-hover
                            'help-echo "mouse-1, RET: change or remove this key"
                            'action (lambda (_) (agent-repl-settings-edit-key topic command group)))
          (let ((label (agent-repl--input-help-group-label (cdr group))))
            (and label (propertize (concat " " label) 'face 'agent-repl-settings-muted)))))

(defun agent-repl-settings--button (label echo action &optional face)
  "Return a button labeled LABEL, explained by ECHO, that runs ACTION.
FACE is its face, the raised settings button by default."
  (make-text-button label nil 'face (or face 'agent-repl-settings-button)
                    'mouse-face 'agent-repl-settings-hover 'help-echo echo
                    'action (lambda (_) (funcall action))))

(defun agent-repl-settings--cell (topic command context)
  "Return COMMAND's row cell: the add button, then its keys as keycaps.
The add button comes first, so every row's sits in one column."
  (concat (agent-repl-settings--button "+" (format "Add a key for %s" command)
                                       (lambda () (agent-repl-settings-add-key topic command))
                                       'agent-repl-settings-add)
          "  "
          (mapconcat (lambda (group) (agent-repl-settings--keycap command group topic))
                     (agent-repl--input-help-group-sources
                      (agent-repl--input-help-key-sources command context))
                     " ")))

(defun agent-repl-settings--variable-line (variable)
  "Return VARIABLE's line: its name and value, then a button to edit it."
  (concat (propertize (concat "↳ " (symbol-name variable)) 'face 'agent-repl-settings-muted)
          "  " (agent-repl--input-help-variable-value variable)
          "  " (agent-repl-settings--button "edit" (concat "Edit " (symbol-name variable))
                                            (lambda () (agent-repl-settings-edit-variable variable))
                                            'agent-repl-settings-link)))

(defun agent-repl-settings--changed-note (changed)
  "Return the note a row with the CHANGED overrides carries, a reset button in it."
  (concat "  " (propertize "● changed" 'face 'agent-repl-settings-changed) "  "
          (agent-repl-settings--button "reset" "Put this command's keys back"
                                       (lambda () (agent-repl-settings--reset changed)
                                         (agent-repl-settings--redraw))
                                       'agent-repl-settings-link)))

(defun agent-repl-settings--rows (topic context)
  "Return TOPIC's rows as (CELL DESCRIPTION LINES), one per command.
A binding naming several commands is a row per command, described by the
command's own first docstring line."
  (mapcan (lambda (binding)
            (pcase-let ((`(,command ,description . ,props) binding))
              (mapcar (lambda (one)
                        (let ((changed (agent-repl-settings--overrides-of (list one))))
                          (list (agent-repl-settings--cell topic one context)
                                (concat (propertize (if (listp command)
                                                        (car (split-string (documentation one) "\n"))
                                                      description)
                                                    'face 'agent-repl-settings-text)
                                        (and changed (agent-repl-settings--changed-note changed)))
                                (mapcar #'agent-repl-settings--variable-line
                                        (plist-get props :variables)))))
                      (if (listp command) command (list command)))))
          (plist-get topic :bindings)))

(defun agent-repl-settings--commands (topic)
  "Return every command TOPIC's rows name."
  (mapcan (lambda (binding)
            (let ((command (car binding)))
              (if (listp command) (copy-sequence command) (list command))))
          (plist-get topic :bindings)))

(defun agent-repl-settings--heading (topic open)
  "Return TOPIC's section heading band, a button folding it, OPEN or shut.
The command count sits at the band's right edge."
  (let* ((commands (agent-repl-settings--commands topic))
         (changed (length (agent-repl-settings--overrides-of commands)))
         (face (if open 'agent-repl-settings-section-open 'agent-repl-settings-section))
         (count (format "%d %s%s  " (length commands)
                        (if (= 1 (length commands)) "command" "commands")
                        (if (> changed 0) (format " · %d changed" changed) ""))))
    (concat (make-text-button (concat "  " (if open "▾" "▸") "  " (plist-get topic :title)) nil
                              'face face
                              'mouse-face 'agent-repl-settings-hover
                              'agent-repl-settings-topic (plist-get topic :id)
                              'help-echo "mouse-1, RET, TAB: open or shut"
                              'action (lambda (_) (agent-repl-settings-toggle (plist-get topic :id))))
            (propertize " " 'face face 'display `(space :align-to (- right ,(1+ (string-width count)))))
            (propertize count 'face (list (if (> changed 0) 'agent-repl-settings-changed
                                            'agent-repl-settings-muted)
                                          face))
            (propertize "\n" 'face face))))

(defun agent-repl-settings--insert (contexts)
  "Insert the page, each open section's keys read from CONTEXTS.
CONTEXTS maps a topic id to its binding context."
  (insert "\n  " (propertize "Settings" 'face 'agent-repl-settings-title) "\n"
          "  " (propertize (format "Every key ZETTEL binds, by feature · saved in %s"
                                   (abbreviate-file-name (agent-repl-settings-file)))
                           'face 'agent-repl-settings-muted)
          "\n\n  "
          (agent-repl-settings--button "  Reset all  " "Put every key and value back"
                                       (lambda ()
                                         (agent-repl-settings--reset (copy-sequence agent-repl-settings-overrides))
                                         (agent-repl-settings--redraw)))
          "   "
          (propertize "+ adds a key  ·  click a key to change or remove it  ·  TAB folds  ·  q closes"
                      'face 'agent-repl-settings-muted)
          "\n")
  (pcase-dolist (`(,group . ,topics) (agent-repl--input-help-groups))
    (insert "\n  " (propertize (upcase group) 'face 'agent-repl-settings-group) "\n"
            (propertize " " 'face 'agent-repl-settings-rule 'display '(space :align-to right))
            "\n")
    (dolist (topic topics)
      (let ((open (memq (plist-get topic :id) agent-repl-settings--open)))
        (insert (agent-repl-settings--heading topic open))
        (when open
          (let* ((rows (agent-repl-settings--rows topic (alist-get (plist-get topic :id) contexts)))
                 (column (+ 2 (agent-repl--input-help-key-column (mapcar #'car rows)))))
            (insert "\n")
            (pcase-dolist (`(,cell ,description ,lines) rows)
              (insert "  ")
              (agent-repl--input-help-insert-row cell description lines column))
            (insert "\n")))))))

(defun agent-repl-settings--contexts (open)
  "Return each of the OPEN topic ids with its binding context."
  (mapcar (lambda (id)
            (cons id (agent-repl--input-help-binding-context (agent-repl--input-help-topic :id id))))
          open))

(defun agent-repl-settings--buffer ()
  "Return the Settings page's buffer, creating it in its mode."
  (or (get-buffer "*zettel settings*")
      (with-current-buffer (get-buffer-create "*zettel settings*")
        (agent-repl-settings-mode)
        (current-buffer))))

(defun agent-repl-settings--redraw ()
  "Draw the Settings page again, keeping point on the same line."
  (with-current-buffer (agent-repl-settings--buffer)
    (let ((line (line-number-at-pos))
          (contexts (agent-repl-settings--contexts agent-repl-settings--open))
          (inhibit-read-only t))
      (erase-buffer)
      (agent-repl-settings--insert contexts)
      (goto-char (point-min))
      (forward-line (1- line)))))

;;;; ---- Commands -------------------------------------------------------

(defun agent-repl-settings ()
  "Show the Settings page: every key binding, editable."
  (interactive)
  (agent-repl-settings--redraw)
  (agent-repl--log agent-repl-settings--log-scope "elisp.settings.show: open=%S"
                   (buffer-local-value 'agent-repl-settings--open (agent-repl-settings--buffer)))
  (agent-repl-popup-show (agent-repl-settings--buffer)))

(defun agent-repl-settings-toggle (id)
  "Open section ID of the Settings page, or shut it if open."
  (with-current-buffer (agent-repl-settings--buffer)
    (setq agent-repl-settings--open
          (if (memq id agent-repl-settings--open)
              (delq id agent-repl-settings--open)
            (cons id agent-repl-settings--open))))
  (agent-repl-settings--redraw))

(defun agent-repl-settings-toggle-section ()
  "Open or shut the section whose heading point is on."
  (interactive)
  (let ((id (get-text-property (point) 'agent-repl-settings-topic)))
    (unless id
      (user-error "Not on a section heading"))
    (agent-repl-settings-toggle id)))

(defun agent-repl-settings-add-key (topic command)
  "Read a key and bind it to COMMAND, where TOPIC's section binds."
  (let* ((keys (agent-repl-settings-read-keys (format "New key for %s" command)))
         (target (agent-repl-settings--target topic keys)))
    (agent-repl-settings--check-free (car target) (cdr target) command)
    (agent-repl-settings--set-key (car target) (cdr target) command)
    (agent-repl-settings--redraw)))

(defun agent-repl-settings--unbind-group (group)
  "Unbind GROUP's key everywhere it is bound, each place once."
  (dolist (place (delete-dups (mapcar (lambda (source)
                                        (cons (plist-get source :keymap)
                                              (key-description (plist-get source :vector))))
                                      (cdr group))))
    (agent-repl-settings--set-key (car place) (cdr place) nil)))

(defun agent-repl-settings-edit-key (topic command group)
  "Change or remove GROUP's key for COMMAND, as the user chooses.
GROUP is (DESCRIPTION . SOURCES), and the edit takes the key away from
every one of SOURCES."
  (pcase (car (read-multiple-choice
               (format "%s runs %s" (car group) command)
               '((?c "change" "Enter a new key in its place")
                 (?r "remove" "Take this key away")
                 (?q "quit" "Leave it as it is"))))
    (?c (let ((keys (agent-repl-settings-read-keys (format "New key for %s" command))))
          (let ((target (agent-repl-settings--target topic keys)))
            (agent-repl-settings--check-free (car target) (cdr target) command)
            (agent-repl-settings--unbind-group group)
            (agent-repl-settings--set-key (car target) (cdr target) command))))
    (?r (agent-repl-settings--unbind-group group)))
  (agent-repl-settings--redraw))

(defun agent-repl-settings-edit-variable (variable)
  "Read a new value for VARIABLE, a string, and save it."
  (agent-repl-settings--set-variable
   variable (read-string (format "%s: " variable) (symbol-value variable)))
  (agent-repl-settings--redraw))

;;;; ---- The mode -------------------------------------------------------

(defconst agent-repl-settings--colors
  '((ground . "#1a1a1a") (band . "#262626") (card . "#2c2c2c") (border . "#2a2a2a")
    (keycap-edge . "#3a3a38") (text . "#f1efe9") (muted . "#898782") (accent . "#8bbeff"))
  "The page's colors, the webapp's dark theme's own (webapp/src/styles.css).
`ground' is the sidebar's `--sidebar-bg', `band' its selected row's tint,
`card' `--card', `border' `--border', `text' `--fg', `muted' `--muted' and
`accent' `--selected-entry'; `keycap-edge' sits between `border' and
`muted', so a keycap's edge reads on a `card' fill.  Pinned on both sides:
test-settings.el here, webapp/test/styles.test.ts there.")

(defun agent-repl-settings--color (name)
  "Return the page color NAME from `agent-repl-settings--colors'."
  (or (alist-get name agent-repl-settings--colors)
      (agent-repl--fatal agent-repl-settings--log-scope "elisp.settings.color: unknown name=%s" name)))

(agent-repl-defface 'agent-repl-settings-text
  `((t :foreground ,(agent-repl-settings--color 'text)))
  "Face of the Settings page's ordinary text.")

(agent-repl-defface 'agent-repl-settings-muted
  `((t :foreground ,(agent-repl-settings--color 'muted)))
  "Face of the Settings page's quieter text: notes, counts, variable names.")

(agent-repl-defface 'agent-repl-settings-title
  `((t :foreground ,(agent-repl-settings--color 'text) :weight bold :height 1.6))
  "Face of the Settings page's title.")

(agent-repl-defface 'agent-repl-settings-group
  `((t :foreground ,(agent-repl-settings--color 'muted) :weight bold :height 0.85))
  "Face of a group's label, upper case as the webapp's sidebar heads are.")

(agent-repl-defface 'agent-repl-settings-rule
  `((t :underline (:color ,(agent-repl-settings--color 'border))))
  "Face of the thin rule under a group's label.")

(agent-repl-defface 'agent-repl-settings-section
  `((t :foreground ,(agent-repl-settings--color 'text)
       :background ,(agent-repl-settings--color 'band) :extend t))
  "Face of a shut section's heading band.")

(agent-repl-defface 'agent-repl-settings-section-open
  `((t :foreground ,(agent-repl-settings--color 'text) :weight bold
       :background ,(agent-repl-settings--color 'card) :extend t))
  "Face of an open section's heading band, lifted to the webapp's card.")

(agent-repl-defface 'agent-repl-settings-keycap
  `((t :foreground ,(agent-repl-settings--color 'text)
       :background ,(agent-repl-settings--color 'card)
       :box (:line-width (1 . -1) :color ,(agent-repl-settings--color 'keycap-edge))))
  "Face of a key on the Settings page: a keycap on the webapp's card.")

(agent-repl-defface 'agent-repl-settings-add
  `((t :foreground ,(agent-repl-settings--color 'muted) :weight bold))
  "Face of a row's add button, quiet until pointed at.")

(agent-repl-defface 'agent-repl-settings-link
  `((t :foreground ,(agent-repl-settings--color 'accent) :underline t))
  "Face of a small text action (edit, reset), in the webapp's accent.")

(agent-repl-defface 'agent-repl-settings-changed
  `((t :foreground ,(agent-repl-settings--color 'accent)))
  "Face of a changed command's mark and its section's count.")

(agent-repl-defface 'agent-repl-settings-hover
  `((t :foreground ,(agent-repl-settings--color 'accent)
       :background ,(agent-repl-settings--color 'border)))
  "Face of whatever on the page the mouse points at.")

(agent-repl-defface 'agent-repl-settings-button
  '((t :inherit agent-repl-input-help-button))
  "Face of a raised button on the Settings page.")

(defun agent-repl-settings-refresh ()
  "Draw the Settings page again, reading every key afresh."
  (interactive)
  (agent-repl-settings--redraw))

(defvar agent-repl-settings-mode-map (make-sparse-keymap)
  "Keymap of `agent-repl-settings-mode'.")

(defun agent-repl-settings--fill-map (map)
  "Bind the Settings page's keys in MAP, in every Evil state too.
TAB folds the section at point, g draws the page again, and q or ESC
closes it."
  (dolist (binding '(("TAB" . agent-repl-settings-toggle-section)
                     ("g" . agent-repl-settings-refresh)
                     ("q" . agent-repl-popup-quit)
                     ("<escape>" . agent-repl-popup-quit)))
    (define-key map (kbd (car binding)) (cdr binding))
    (when (fboundp 'evil-define-key*)
      (evil-define-key* '(normal motion) map (kbd (car binding)) (cdr binding)))))

(agent-repl-refill-keymap agent-repl-settings-mode-map #'agent-repl-settings--fill-map)
(with-eval-after-load 'evil
  (agent-repl-settings--fill-map agent-repl-settings-mode-map))

(defun agent-repl-settings--close-when-left (window)
  "Close the Settings page's WINDOW once another window is selected.
Run from the page's `window-selection-change-functions', so clicking away
closes the page as q and ESC do.  The minibuffer is not away: entering a
key, picking a mode or answering a question reads there, and the page
stays for the answer."
  (let ((selected (selected-window)))
    (unless (or (eq selected window)
                (not (window-live-p window))
                (minibufferp (window-buffer selected)))
      (agent-repl--log agent-repl-settings--log-scope
                       "elisp.settings.close-when-left: window=%S selected=%S" window selected)
      (delete-window window))))

(define-derived-mode agent-repl-settings-mode special-mode "Settings"
  "Major mode of the Settings page, on the webapp sidebar's dark ground.
Selecting any other window closes it (`agent-repl-settings--close-when-left')."
  (agent-repl--set-buffer-background (agent-repl-settings--color 'ground))
  (face-remap-add-relative 'default :foreground (agent-repl-settings--color 'text))
  (setq-local truncate-lines nil)
  (setq-local line-spacing 0.2)
  (visual-line-mode 1)
  (add-hook 'window-selection-change-functions #'agent-repl-settings--close-when-left nil t))

(agent-repl-settings-apply)

(provide 'settings)

;;; settings.el ends here
