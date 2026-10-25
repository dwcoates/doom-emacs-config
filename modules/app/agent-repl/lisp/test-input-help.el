;;; test-input-help.el --- ERT tests for agent-repl input-help.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-input-help.el -f ert-run-tests-batch-and-exit
;;
;; input-help.el is the composer's header line of help buttons.  These tests
;; pin the buttons, how a command's keys are read out of the keymaps (plain,
;; Evil state, `[remap ...]' and leader bindings), the popup's text and
;; display, the click and keyboard entry points, every fatal branch, and the
;; topic table against the composer's real bindings in both directions.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Local harness ----

;; The live context's maps, special here so a test can `let'-bind them.
(defvar evil-normal-state-map)
(defvar evil-insert-state-map)
(defvar doom-leader-map)
(defvar doom-leader-key)

(defvar agent-repl-test-input-help--fatals nil
  "Messages handed to `agent-repl--fatal' during a test, newest first.")

(defmacro agent-repl-test-input-help--capturing-fatals (&rest body)
  "Run BODY with every `agent-repl--fatal' recorded, then signalled as it is."
  (declare (indent 0))
  `(let ((agent-repl-test-input-help--fatals nil))
     (cl-letf (((symbol-function 'agent-repl--fatal)
                (lambda (_ws fmt &rest args)
                  (let ((text (apply #'format fmt args)))
                    (push text agent-repl-test-input-help--fatals)
                    (error "%s" text)))))
       ,@body)))

(defun agent-repl-test-input-help--context (&rest bindings)
  "Return a binding context built from BINDINGS.
BINDINGS is a plist: `:composer' (the topic's own map), `:insert',
`:normal', `:global' and `:leader', each a list of (KEY-VECTOR . COMMAND)
for that map.  A `:composer' KEY-VECTOR may start with an Evil
`STATE-state' event."
  (cl-flet ((map-of (pairs)
              (let ((map (make-sparse-keymap)))
                (pcase-dolist (`(,key . ,command) pairs)
                  (define-key map key command))
                map)))
    (list :maps (list (cons 'composer-map (map-of (plist-get bindings :composer))))
          :global (list (cons "insert" (cons 'insert-map (map-of (plist-get bindings :insert))))
                        (cons "normal" (cons 'normal-map (map-of (plist-get bindings :normal))))
                        (cons nil (cons 'global-map (map-of (plist-get bindings :global)))))
          :leader (map-of (plist-get bindings :leader))
          :leader-key "SPC")))

(defun agent-repl-test-input-help--click-event (string pos)
  "Return a header-line `mouse-1' event on STRING at POS."
  `(mouse-1 (,(selected-window) header-line (0 . 0) 0 (,string . ,pos)
             nil (0 . 0) nil (0 . 0) (0 . 0))))

(defun agent-repl-test-input-help--binding-commands (binding)
  "Return the commands BINDING's row lists: one, or its list."
  (let ((command (car binding)))
    (if (listp command) (copy-sequence command) (list command))))

(defun agent-repl-test-input-help--topic-commands (&optional loadable-only)
  "Return every command the help topics name.
LOADABLE-ONLY keeps only the topics whose `:requires' batch Emacs has."
  (delete-dups
   (mapcan (lambda (topic)
             (when (or (not loadable-only)
                       (cl-every (lambda (feature) (locate-library (symbol-name feature)))
                                 (plist-get topic :requires)))
               (mapcan #'agent-repl-test-input-help--binding-commands (plist-get topic :bindings))))
           (copy-sequence agent-repl-input-help-topics))))

(defun agent-repl-test-input-help--quoted-commands (form)
  "Return every symbol FORM quotes with `#''."
  (let (found)
    (cl-labels ((walk (x)
                  (cond ((and (eq (car-safe x) 'function) (symbolp (cadr x)))
                         (push (cadr x) found))
                        ((consp x)
                         (walk (car x))
                         (walk (cdr x))))))
      (walk form))
    found))

(defconst agent-repl-test-input-help--binders
  '(define-key map! evil-define-key evil-define-key* keymap-set global-set-key local-set-key)
  "The forms that bind a key; a `#''-quoted command inside one is a binding.")

(defun agent-repl-test-input-help--source-commands (file)
  "Return the `#''-quoted commands inside FILE's binding forms.
`map!' is a no-op stub under `emacs -Q', so its bindings are read off the
source; so is every other binding form, wherever it nests."
  (with-temp-buffer
    (insert-file-contents file)
    (let (found)
      (cl-labels ((walk (x inside)
                    (cond ((and inside (eq (car-safe x) 'function) (symbolp (cadr x)))
                           (push (cadr x) found))
                          ((consp x)
                           (let ((in (or inside (memq (car x) agent-repl-test-input-help--binders))))
                             (walk (car x) in)
                             (when (listp (cdr x))
                               (dolist (y (cdr x)) (walk y in))))))))
        (condition-case nil
            (while t (walk (read (current-buffer)) nil))
          (end-of-file nil)))
      found)))

(defun agent-repl-test-input-help--module-sources ()
  "Return every non-test elisp source of the module, config.el included."
  (cons (expand-file-name "config.el" agent-repl-test--module-root)
        (seq-remove (lambda (file) (string-prefix-p "test-" (file-name-nondirectory file)))
                    (directory-files agent-repl-test--module-dir t "\\.el\\'"))))

(defun agent-repl-test-input-help--bound-commands ()
  "Return every command the module binds.
Its live keymaps (every `agent-repl'-named keymap variable, which a
`define-key' fills even under `emacs -Q'), and every binding form of its
sources (`agent-repl-test-input-help--source-commands').  Only commands
count: Evil marks a map with non-command bindings, such as an intercept
map's `[intercept-state] all'."
  (let (found)
    (cl-labels ((walk (map)
                  (map-keymap (lambda (_event def)
                                (cond ((keymapp def) (walk def))
                                      ((and (symbolp def) (commandp def)) (push def found))))
                              map)))
      (mapatoms (lambda (symbol)
                  (when (and (boundp symbol)
                             (string-prefix-p "agent-repl" (symbol-name symbol))
                             (keymapp (symbol-value symbol)))
                    (walk (symbol-value symbol))))))
    (dolist (file (agent-repl-test-input-help--module-sources))
      (setq found (append (agent-repl-test-input-help--source-commands file) found)))
    (delete-dups found)))

;;;; ---- The header line ----

(ert-deftest agent-repl-test-input-help-header-line-is-settings-then-help ()
  "The header line's only words, apart from the bar, are its two buttons."
  (let ((line (agent-repl--input-help-header-line-text)))
    (should (equal (split-string (substring-no-properties line)) '("Settings" "Help")))
    (dolist (word '("Settings" "Help"))
      (should (eq (get-text-property (string-search word line) 'face line)
                  'agent-repl-input-help-button)))))

(ert-deftest agent-repl-test-input-help-help-button-click-runs-the-menu ()
  "The Help button's `mouse-1' in the header line drops the topic menu."
  (let ((line (agent-repl--input-help-header-line-text)))
    (should (eq (lookup-key (get-text-property (string-search "Help" line) 'local-map line)
                            [header-line mouse-1])
                #'agent-repl-input-help-click))))

(ert-deftest agent-repl-test-input-help-settings-button-click-opens-settings ()
  "The Settings button's `mouse-1' in the header line opens the Settings page."
  (let ((line (agent-repl--input-help-header-line-text)))
    (should (eq (lookup-key (get-text-property (string-search "Settings" line) 'local-map line)
                            [header-line mouse-1])
                #'agent-repl-settings))))

(ert-deftest agent-repl-test-input-help-button-is-raised-and-highlights ()
  "A bar button wears the button face, highlights under the mouse, and clicks through its map."
  (let* ((map (make-sparse-keymap))
         (button (agent-repl--input-help-button "  X  " "echo" map)))
    (should (eq (get-text-property 1 'face button) 'agent-repl-input-help-button))
    (should (eq (get-text-property 1 'mouse-face button) 'mode-line-highlight))
    (should (eq (get-text-property 1 'local-map button) map))
    (should (equal (get-text-property 1 'help-echo button) "echo"))))

;;;; ---- The bar's colors ----

(defun agent-repl-test-input-help--rgb (hex)
  "Return HEX, a #rrggbb string, as a list of three channel integers."
  (mapcar (lambda (i) (string-to-number (substring hex i (+ i 2)) 16)) '(1 3 5)))

(ert-deftest agent-repl-test-input-help-bar-is-the-sidebar-ground ()
  "The bar's ground is the webapp sidebar's dark --sidebar-bg, #111110.
The ert suite may not read webapp/src/styles.css, so the shared value is
pinned on both sides: here, and in webapp/test/styles.test.ts."
  (should (equal agent-repl-input-help-bar-background "#111110")))

(ert-deftest agent-repl-test-input-help-button-is-darker-than-the-selected-row ()
  "A button's fill sits between the bar's ground and the selected-row tint.
The sidebar's selected row is #181817 in the dark theme, pinned by
webapp/test/styles.test.ts."
  (let ((button (agent-repl-test-input-help--rgb agent-repl-input-help-button-background)))
    (should (cl-every #'< button (agent-repl-test-input-help--rgb "#181817")))
    (should (cl-every #'> button (agent-repl-test-input-help--rgb agent-repl-input-help-bar-background)))))

(ert-deftest agent-repl-test-input-help-faces-take-the-bar-colors ()
  "The bar and button faces are filled with their two colors."
  (should (equal (face-attribute 'agent-repl-input-help-bar :background)
                 agent-repl-input-help-bar-background))
  (should (equal (face-attribute 'agent-repl-input-help-button :background)
                 agent-repl-input-help-button-background)))

(ert-deftest agent-repl-test-input-help-header-line-grounds-every-gap-with-the-bar ()
  "Every character outside the buttons carries the bar's face."
  (let ((line (agent-repl--input-help-header-line-text)))
    (dotimes (i (length line))
      (unless (eq (get-text-property i 'face line) 'agent-repl-input-help-button)
        (should (eq (get-text-property i 'face line) 'agent-repl-input-help-bar))))))

(ert-deftest agent-repl-test-input-help-header-line-puts-the-buttons-at-the-right-edge ()
  "The bar stretches to just short of the right edge, where the buttons sit."
  (let* ((line (agent-repl--input-help-header-line-text))
         (buttons (substring-no-properties line 1 (1- (length line)))))
    (should (equal (get-text-property 0 'display line)
                   `(space :align-to (- right ,(1+ (string-width buttons))))))
    (should (equal buttons "  Settings     Help  "))))

;;;; ---- Installing the header line ----

(ert-deftest agent-repl-test-input-help-header-line-variable-is-the-buttons ()
  "The shared variable holds the buttons, properties and all."
  (should (equal-including-properties agent-repl-input-help-header-line
                                      (agent-repl--input-help-header-line-text))))

(ert-deftest agent-repl-test-input-help-header-line-variable-is-risky ()
  "The variable is risky, so redisplay keeps its buttons' text properties."
  (should (get 'agent-repl-input-help-header-line 'risky-local-variable)))

(ert-deftest agent-repl-test-input-help-install-points-at-the-variable ()
  "Installing makes the buffer's header line the shared variable's name."
  (with-temp-buffer
    (agent-repl-input-help-install)
    (should (eq header-line-format 'agent-repl-input-help-header-line))))

(ert-deftest agent-repl-test-input-help-install-in-composers-updates-stale-composers ()
  "Every open composer holding an older header line is pointed at the variable."
  (let (logged)
    (with-temp-buffer
      (agent-repl-input-mode)
      (setq-local header-line-format "C-c C-c: clear+save")
      (cl-letf (((symbol-function 'agent-repl--log)
                 (agent-repl-test--record-into logged)))
        (should (>= (agent-repl--input-help-install-in-composers) 1)))
      (should (eq header-line-format 'agent-repl-input-help-header-line))
      (should (string-match-p "install-in-composers: composers=[1-9]" (car logged))))))

(ert-deftest agent-repl-test-input-help-install-in-composers-leaves-other-buffers ()
  "A buffer that is not a composer keeps its own header line."
  (with-temp-buffer
    (setq-local header-line-format "mine")
    (agent-repl--input-help-install-in-composers)
    (should (equal header-line-format "mine"))))

(ert-deftest agent-repl-test-input-help-reload-updates-open-composers ()
  "Loading input-help.el again updates a composer an older load set up."
  (with-temp-buffer
    (agent-repl-input-mode)
    (setq-local header-line-format "C-c C-c: clear+save")
    (load (symbol-file 'agent-repl-input-help-install) nil t)
    (should (eq header-line-format 'agent-repl-input-help-header-line))))

;;;; ---- Finding a command's keys ----

(ert-deftest agent-repl-test-input-help-state-key-plain ()
  "A plain key has no state."
  (should (equal (agent-repl--input-help-state-key [?\C-c ?\C-c]) (cons nil [?\C-c ?\C-c]))))

(ert-deftest agent-repl-test-input-help-state-key-evil-state ()
  "A key under Evil's `normal-state' prefix is a normal-state key."
  (should (equal (agent-repl--input-help-state-key [normal-state up]) (cons "normal" [up]))))

(ert-deftest agent-repl-test-input-help-state-label-cases ()
  "Which states a key is bound in decides its annotation."
  (pcase-dolist (`(,states ,want)
                 '(((nil) nil)
                   (("normal" nil) nil)
                   (("normal" "insert") nil)
                   (("normal") "command mode")
                   (("insert") "insert mode")
                   (("visual" "normal") "command mode, visual mode")))
    (should (equal (agent-repl--input-help-state-label states) want))))

(ert-deftest agent-repl-test-input-help-keys-plain-binding ()
  "A plain composer binding is its key, unannotated."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context :composer `((,(kbd "C-c C-c") . cmd))))
                 '(("C-c C-c")))))

(ert-deftest agent-repl-test-input-help-keys-state-binding-is-annotated ()
  "A command-mode-only binding is annotated with its state."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context :composer '(([normal-state up] . cmd))))
                 '(("<up>" . "command mode")))))

(ert-deftest agent-repl-test-input-help-keys-one-key-in-several-states-is-one-label ()
  "A key bound plainly and in both states is listed once, unannotated."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context
                        :composer `((,(kbd "RET") . cmd)
                                    (,(vconcat [normal-state] (kbd "RET")) . cmd)
                                    (,(vconcat [insert-state] (kbd "RET")) . cmd))))
                 '(("RET")))))

(ert-deftest agent-repl-test-input-help-keys-remap-resolves-through-global-maps ()
  "A `[remap X]' entry shows the keys X is bound to, with their state."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context
                        :composer '(([remap target] . cmd))
                        :insert '(([S-s-return] . target))))
                 '(("S-s-RET" . "insert mode")))))

(ert-deftest agent-repl-test-input-help-keys-leader-binding-is-prefixed ()
  "A leader binding is shown after the leader key."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context :leader `((,(kbd "j RET") . cmd))))
                 '(("SPC j RET")))))

(ert-deftest agent-repl-test-input-help-keys-unbound-command-is-fatal ()
  "A command no map binds signals, and the record names it."
  (agent-repl-test-input-help--capturing-fatals
    (should-error (agent-repl--input-help-keys 'nowhere (agent-repl-test-input-help--context)))
    (should (equal agent-repl-test-input-help--fatals
                   '("elisp.input-help.keys: unbound command=nowhere")))))

(ert-deftest agent-repl-test-input-help-binding-context-reads-the-live-maps ()
  "The live context is the topic's keymaps, Evil's state maps and Doom's leader."
  (let ((evil-insert-state-map (make-sparse-keymap))
        (evil-normal-state-map (make-sparse-keymap))
        (doom-leader-map (make-sparse-keymap))
        (doom-leader-key "SPC"))
    (let ((context (agent-repl--input-help-binding-context
                    '(:id x :keymaps (agent-repl-input-mode-map agent-repl-input-selection-mode-map)))))
      (should (equal (plist-get context :maps)
                     (list (cons 'agent-repl-input-mode-map agent-repl-input-mode-map)
                           (cons 'agent-repl-input-selection-mode-map agent-repl-input-selection-mode-map))))
      (should (equal (plist-get context :global)
                     (list (cons "insert" (cons 'evil-insert-state-map evil-insert-state-map))
                           (cons "normal" (cons 'evil-normal-state-map evil-normal-state-map))
                           (cons nil (cons 'global-map global-map)))))
      (should (eq (plist-get context :leader) doom-leader-map))
      (should (equal (plist-get context :leader-key) "SPC")))))

(ert-deftest agent-repl-test-input-help-binding-context-loads-the-topic-requires ()
  "A topic's `:requires' are loaded before its keymaps are read."
  (let ((evil-insert-state-map (make-sparse-keymap))
        (evil-normal-state-map (make-sparse-keymap))
        (doom-leader-map (make-sparse-keymap))
        (doom-leader-key "SPC")
        required)
    (cl-letf (((symbol-function 'require) (lambda (feature &rest _) (push feature required))))
      (agent-repl--input-help-binding-context '(:id x :requires (alpha beta))))
    (should (equal required '(beta alpha)))))

(ert-deftest agent-repl-test-input-help-binding-context-unbound-keymap-is-fatal ()
  "A topic keymap still unbound once loaded signals, naming it and the topic."
  (let ((evil-insert-state-map (make-sparse-keymap))
        (evil-normal-state-map (make-sparse-keymap))
        (doom-leader-map (make-sparse-keymap))
        (doom-leader-key "SPC"))
    (agent-repl-test-input-help--capturing-fatals
      (should-error (agent-repl--input-help-binding-context
                     '(:id x :keymaps (agent-repl-test-input-help--no-such-map))))
      (should (equal agent-repl-test-input-help--fatals
                     '("elisp.input-help.context: unbound keymap=agent-repl-test-input-help--no-such-map topic=x"))))))

(ert-deftest agent-repl-test-input-help-keys-include-global-bindings ()
  "A command bound in a global map shows that key."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context :global `((,(kbd "s-[") . cmd))))
                 '(("s-[")))))

(ert-deftest agent-repl-test-input-help-keys-of-a-command-list-are-one-row ()
  "A row of several commands lists every one's keys."
  (should (equal (agent-repl--input-help-keys
                  '(a b) (agent-repl-test-input-help--context
                          :composer `((,(kbd "M-1") . a) (,(kbd "M-2") . b))))
                 '(("M-1 … M-2")))))

(ert-deftest agent-repl-test-input-help-keys-unbound-command-in-a-list-is-fatal ()
  "One unbound command in a row's list signals, naming it."
  (agent-repl-test-input-help--capturing-fatals
    (should-error (agent-repl--input-help-keys
                   '(a nowhere) (agent-repl-test-input-help--context
                                 :composer `((,(kbd "M-1") . a)))))
    (should (equal agent-repl-test-input-help--fatals
                   '("elisp.input-help.keys: unbound command=nowhere")))))

(ert-deftest agent-repl-test-input-help-key-sources-name-each-keys-map-and-vector ()
  "Each key names the keymap variable and vector that bind it.
Compared in description order: a keymap's own order says nothing."
  (should (equal (sort (agent-repl--input-help-key-sources
                  'cmd (agent-repl-test-input-help--context
                        :composer `((,(kbd "C-c C-c") . cmd) ([normal-state up] . cmd))
                        :leader `((,(kbd "j RET") . cmd))))
                       (lambda (x y) (string< (plist-get x :description) (plist-get y :description))))
                 `((:description "<up>" :state "normal" :keymap composer-map :vector [normal-state up])
                   (:description "C-c C-c" :state nil :keymap composer-map :vector ,(vconcat (kbd "C-c C-c")))
                   (:description "SPC j RET" :state nil :keymap doom-leader-map :vector ,(vconcat (kbd "j RET")))))))

(ert-deftest agent-repl-test-input-help-key-sources-name-a-remap-by-its-entry ()
  "A key reached through `[remap X]' names the remap entry, which it shares."
  (should (equal (agent-repl--input-help-key-sources
                  'cmd (agent-repl-test-input-help--context
                        :composer '(([remap target] . cmd))
                        :insert '(([S-s-return] . target))))
                 '((:description "S-s-RET" :state "insert" :keymap composer-map :vector [remap target])))))

(ert-deftest agent-repl-test-input-help-key-sources-name-a-global-map ()
  "A key bound in a global map names that map's variable."
  (should (equal (agent-repl--input-help-key-sources
                  'cmd (agent-repl-test-input-help--context :normal `((,(kbd "s-[") . cmd))))
                 `((:description "s-[" :state "normal" :keymap normal-map :vector ,(vconcat (kbd "s-[")))))))

(ert-deftest agent-repl-test-input-help-canonical-key-cases ()
  "RET and TAB read the same whichever event names them."
  (pcase-dolist (`(,description ,want)
                 '(("<return>" "RET") ("C-<return>" "C-RET") ("S-s-<return>" "S-s-RET")
                   ("<tab>" "TAB") ("C-c <tab>" "C-c TAB") ("RET" "RET") ("<escape>" "<escape>")))
    (should (equal (agent-repl--input-help-canonical-key description) want))))

(ert-deftest agent-repl-test-input-help-keys-show-return-and-RET-once ()
  "A command bound to both C-RET and C-<return> shows one C-RET."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context
                        :composer `((,(kbd "C-RET") . cmd) ([C-return] . cmd))))
                 '(("C-RET")))))

;;;; ---- Topics and their text ----

(ert-deftest agent-repl-test-input-help-topic-by-id-and-title ()
  "A topic is found by its id or by its title."
  (should (eq (plist-get (agent-repl--input-help-topic :id 'history) :id) 'history))
  (should (eq (plist-get (agent-repl--input-help-topic :title "Prompt history") :id) 'history)))

(ert-deftest agent-repl-test-input-help-topic-unknown-is-fatal ()
  "An unknown topic signals, and the record names it and the known ids."
  (agent-repl-test-input-help--capturing-fatals
    (should-error (agent-repl--input-help-topic :id 'nope))
    (should (string-match-p "unknown :id=nope known=(writing sending "
                            (car agent-repl-test-input-help--fatals)))))

(ert-deftest agent-repl-test-input-help-keys-shortest-first-then-alphabetical ()
  "A command's keys are listed shortest first, then alphabetically."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context
                        :composer `((,(kbd "C-c C-k") . cmd) (,(kbd "RET") . cmd) (,(kbd "C-c C-a") . cmd))))
                 '(("RET") ("C-c C-a") ("C-c C-k")))))

(ert-deftest agent-repl-test-input-help-keys-collapse-a-digit-run ()
  "Bindings 1 through 9 read as one range key."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context
                        :composer (mapcar (lambda (d) (cons (string d) 'cmd)) "123456789")))
                 '(("1 … 9")))))

(ert-deftest agent-repl-test-input-help-collapse-digits-cases ()
  "Only consecutive digits sharing an annotation join one range."
  (pcase-dolist (`(,keys ,want)
                 '(((("1") ("2") ("3")) (("1 … 3")))
                   ((("M-1") ("M-2") ("s-1") ("s-2")) (("M-1 … M-2") ("s-1 … s-2")))
                   ((("SPC TAB 1") ("SPC TAB 2")) (("SPC TAB 1 … SPC TAB 2")))
                   ((("F1") ("F2")) (("F1") ("F2")))
                   ((("1") ("3")) (("1") ("3")))
                   ((("1") ("2" . "insert mode")) (("1") ("2" . "insert mode")))
                   ((("RET") ("1") ("2")) (("RET") ("1 … 2")))
                   ((("C-1") ("C-2")) (("C-1 … C-2")))))
    (should (equal (agent-repl--input-help-collapse-digits keys) want))))

(ert-deftest agent-repl-test-input-help-key-cell-is-keycaps-with-annotations ()
  "A cell is each key as a keycap, followed by its dim annotation."
  (let ((cell (agent-repl--input-help-key-cell '(("RET") ("<up>" . "command mode")))))
    (should (equal (substring-no-properties cell) "RET  <up> command mode"))
    (should (eq (get-text-property 0 'face cell) 'help-key-binding))
    (should (eq (get-text-property 5 'face cell) 'help-key-binding))
    (should (eq (get-text-property 10 'face cell) 'agent-repl-input-help-annotation))))

(ert-deftest agent-repl-test-input-help-variable-line-shows-the-current-value ()
  "A variable's line is its name, then its value printed with escaped newlines."
  (let ((agent-repl-send-postfix "\n be brief"))
    (should (equal (substring-no-properties
                    (agent-repl--input-help-variable-line 'agent-repl-send-postfix))
                   "agent-repl-send-postfix: \"\\n be brief\""))))

(ert-deftest agent-repl-test-input-help-variable-value-escapes-newlines ()
  "A value is printed with its newlines escaped, in the string face."
  (let* ((agent-repl-send-postfix "\n be brief")
         (value (agent-repl--input-help-variable-value 'agent-repl-send-postfix)))
    (should (equal (substring-no-properties value) "\"\\n be brief\""))
    (should (eq (get-text-property 0 'face value) 'font-lock-string-face))))

(ert-deftest agent-repl-test-input-help-variable-value-is-shared-by-help-and-settings ()
  "The help and the Settings page show a variable's value the same way."
  (let* ((agent-repl-send-prefix "p: ")
         (value (agent-repl--input-help-variable-value 'agent-repl-send-prefix)))
    (dolist (line (list (agent-repl--input-help-variable-line 'agent-repl-send-prefix)
                        (agent-repl-settings--variable-line 'agent-repl-send-prefix)))
      (should (string-search (concat "agent-repl-send-prefix: " value) (substring-no-properties line))))))

(ert-deftest agent-repl-test-input-help-variable-line-name-describes-the-variable ()
  "Pushing the variable's name describes it."
  (let ((line (agent-repl--input-help-variable-line 'agent-repl-send-prefix))
        described)
    (cl-letf (((symbol-function 'describe-variable) (lambda (v) (push v described))))
      (with-temp-buffer
        (insert line)
        (push-button 1)))
    (should (equal described '(agent-repl-send-prefix)))))

(ert-deftest agent-repl-test-input-help-rows-resolve-keys-and-variables ()
  "A row is the binding's keycap cell, description and variables."
  (should (equal (mapcar (lambda (row) (cons (substring-no-properties (car row)) (cdr row)))
                         (agent-repl--input-help-rows
                          '(:bindings ((a "Does a." :variables (agent-repl-send-prefix))))
                          (agent-repl-test-input-help--context :composer `((,(kbd "RET") . a)))))
                 '(("RET" "Does a." (agent-repl-send-prefix))))))

(ert-deftest agent-repl-test-input-help-rows-unbound-variable-is-fatal ()
  "A topic naming an unbound variable signals, naming it and its command."
  (agent-repl-test-input-help--capturing-fatals
    (should-error (agent-repl--input-help-rows
                   '(:bindings ((a "Does a." :variables (agent-repl-test-input-help--nowhere))))
                   (agent-repl-test-input-help--context :composer `((,(kbd "RET") . a)))))
    (should (equal agent-repl-test-input-help--fatals
                   '("elisp.input-help.rows: unbound variable=agent-repl-test-input-help--nowhere command=a")))))

(defun agent-repl-test-input-help--inserted (topic context)
  "Return TOPIC's help as inserted into a fresh buffer from CONTEXT."
  (with-temp-buffer
    (agent-repl--input-help-insert topic (agent-repl--input-help-rows topic context))
    (buffer-string)))

(ert-deftest agent-repl-test-input-help-insert-lays-out-title-keys-and-features ()
  "The help is the title, a Keys section of rows, then a Good to know section."
  (should (equal (substring-no-properties
                  (agent-repl-test-input-help--inserted
                   '(:title "T" :bindings ((a "Does a.") (b "Does b.")) :features ("F."))
                   (agent-repl-test-input-help--context
                    :composer `((,(kbd "C-c C-k") . a) (,(kbd "RET") . b)))))
                 "T\n\nKeys\n  C-c C-k Does a.\n  RET Does b.\n\nGood to know\n  • F.\n")))

(ert-deftest agent-repl-test-input-help-insert-aligns-descriptions-to-one-column ()
  "Every description starts at, and wraps back to, the widest key cell plus padding."
  (let* ((text (agent-repl-test-input-help--inserted
                '(:title "T" :bindings ((a "Does a.") (b "Does b.")) :features ())
                (agent-repl-test-input-help--context
                 :composer `((,(kbd "C-c C-k") . a) (,(kbd "RET") . b)))))
         (gap (string-search " Does b." text)))
    (should (equal (get-text-property gap 'display text) '(space :align-to 12)))
    (should (equal (get-text-property gap 'wrap-prefix text) '(space :align-to 12)))))

(ert-deftest agent-repl-test-input-help-insert-puts-a-wide-cell-description-below ()
  "A key cell wider than the column cap puts its description on the next line."
  (let ((text (substring-no-properties
               (agent-repl-test-input-help--inserted
                '(:title "T" :bindings ((a "Does a.")) :features ())
                (agent-repl-test-input-help--context
                 :composer `((,(kbd "C-c C-a") . a) (,(kbd "C-c C-b") . a) (,(kbd "C-c C-d") . a)
                             (,(kbd "C-c C-e") . a)))))))
    (should (string-match-p "C-c C-[a-e]\n Does a\\.\n" text))))

(ert-deftest agent-repl-test-input-help-insert-shows-each-variable-under-its-row ()
  "A row's variables follow its description, each with its current value."
  (let ((agent-repl-send-prefix "be brief: "))
    (should (string-match-p
             "  RET Send\\.\n agent-repl-send-prefix: \"be brief: \"\n"
             (substring-no-properties
              (agent-repl-test-input-help--inserted
               '(:title "T" :bindings ((a "Send." :variables (agent-repl-send-prefix))) :features ())
               (agent-repl-test-input-help--context :composer `((,(kbd "RET") . a)))))))))

(ert-deftest agent-repl-test-input-help-insert-without-bindings-is-features-only ()
  "A topic with no bindings has no Keys section."
  (should (equal (substring-no-properties
                  (agent-repl-test-input-help--inserted '(:title "T" :bindings () :features ("F."))
                                                        (agent-repl-test-input-help--context)))
                 "T\n\nGood to know\n  • F.\n")))

(ert-deftest agent-repl-test-input-help-insert-wraps-features-under-their-text ()
  "A feature line wraps back under its text, past the bullet."
  (let ((text (agent-repl-test-input-help--inserted '(:title "T" :bindings () :features ("F."))
                                                    (agent-repl-test-input-help--context))))
    (should (equal (get-text-property (string-search "F." text) 'wrap-prefix text) "    "))))

;;;; ---- Showing a topic ----

(ert-deftest agent-repl-test-input-help-show-pops-a-read-only-topic-buffer ()
  "Showing a topic hands its filled, read-only buffer to the shared popup."
  (let ((context (agent-repl-test-input-help--context
                  :leader `((,(kbd "j a q e") . agent-repl-merge-queue-evict)
                            (,(kbd "j a q p") . agent-repl-merge-queue-pause)
                            (,(kbd "j a q r") . agent-repl-merge-queue-resume))))
        logged)
    (agent-repl-test--recording-popups
      (cl-letf (((symbol-function 'agent-repl--input-help-binding-context) (lambda (_topic) context))
                ((symbol-function 'agent-repl--log)
                 (agent-repl-test--record-into logged)))
        (agent-repl-input-help-show 'merge-queue))
      (let ((buf (car agent-repl-test--popups-shown)))
        (unwind-protect
            (progn
              (should (equal (buffer-name buf) "*agent-repl help: Merge queue*"))
              (with-current-buffer buf
                (should buffer-read-only)
                (should visual-line-mode)
                (should-not truncate-lines)
                (should (string-match-p "SPC j a q e Take this workspace's merge off the queue" (buffer-string))))
              (should (member "elisp.input-help.show: id=merge-queue buffer=*agent-repl help: Merge queue*"
                              logged)))
          (kill-buffer buf))))))

(ert-deftest agent-repl-test-input-help-show-unknown-topic-shows-nothing ()
  "An unknown topic signals before any buffer is made or shown."
  (agent-repl-test-input-help--capturing-fatals
    (agent-repl-test--recording-popups
      (let ((before (buffer-list)))
        (should-error (agent-repl-input-help-show 'nope))
        (should-not agent-repl-test--popups-shown)
        (should (equal (buffer-list) before))))))

(ert-deftest agent-repl-test-input-help-show-unbound-command-shows-nothing ()
  "A topic command no map binds signals before its buffer is made or shown."
  (agent-repl-test-input-help--capturing-fatals
    (agent-repl-test--recording-popups
      (cl-letf (((symbol-function 'agent-repl--input-help-binding-context)
                 (lambda (_topic) (agent-repl-test-input-help--context))))
        (let ((before (buffer-list)))
          (should-error (agent-repl-input-help-show 'merge-queue))
          (should-not agent-repl-test--popups-shown)
          (should (equal (buffer-list) before)))))))

(defun agent-repl-test-input-help--menu-items (menu)
  "Return MENU's (LABEL . BINDING) items, in order."
  (let (items)
    (map-keymap (lambda (_event item)
                  (when (eq (car-safe item) 'menu-item)
                    (push (cons (nth 1 item) (nth 2 item)) items)))
                menu)
    (nreverse items)))

(ert-deftest agent-repl-test-input-help-groups-keep-table-order ()
  "Topics are grouped by `:group', groups and topics in table order."
  (should (equal (mapcar (lambda (group)
                           (cons (car group) (mapcar (lambda (topic) (plist-get topic :id)) (cdr group))))
                         (let ((agent-repl-input-help-topics
                                '((:id a :group "G1") (:id b :group "G2") (:id c :group "G1"))))
                           (agent-repl--input-help-groups)))
                 '(("G1" a c) ("G2" b)))))

(ert-deftest agent-repl-test-input-help-menu-has-a-submenu-per-group ()
  "The menu's items are the groups, each a submenu of its topics' titles."
  (let ((items (agent-repl-test-input-help--menu-items (agent-repl--input-help-menu))))
    (should (equal (mapcar #'car items) (mapcar #'car (agent-repl--input-help-groups))))
    (should (equal (mapcar #'car (agent-repl-test-input-help--menu-items (cdr (car items))))
                   (mapcar (lambda (topic) (plist-get topic :title))
                           (cdr (car (agent-repl--input-help-groups))))))))

(ert-deftest agent-repl-test-input-help-menu-item-shows-its-topic ()
  "Choosing a submenu's item shows that item's topic."
  (let (shown)
    (cl-letf (((symbol-function 'agent-repl-input-help-show) (lambda (id) (push id shown))))
      (call-interactively (lookup-key (agent-repl--input-help-menu) [Composer history])))
    (should (equal shown '(history)))))

(ert-deftest agent-repl-test-input-help-click-drops-the-menu-at-the-click ()
  "Clicking the button pops the topic menu at the click's event."
  (let ((event (agent-repl-test-input-help--click-event "  Help  " 2))
        popped)
    (cl-letf (((symbol-function 'popup-menu) (lambda (menu position) (push (list menu position) popped))))
      (agent-repl-input-help-click event))
    (should (equal popped (list (list (agent-repl--input-help-menu) event))))))

(ert-deftest agent-repl-test-input-help-command-shows-the-chosen-topic ()
  "`agent-repl-input-help' shows the topic it is given as GROUP: TITLE."
  (let (shown)
    (cl-letf (((symbol-function 'agent-repl-input-help-show) (lambda (id) (push id shown))))
      (agent-repl-input-help "Composer: Images"))
    (should (equal shown '(images)))))

(ert-deftest agent-repl-test-input-help-command-unknown-choice-is-fatal ()
  "A choice naming no topic signals and shows nothing."
  (let (shown)
    (agent-repl-test-input-help--capturing-fatals
      (cl-letf (((symbol-function 'agent-repl-input-help-show) (lambda (id) (push id shown))))
        (should-error (agent-repl-input-help "Nowhere: Nothing")))
      (should (equal agent-repl-test-input-help--fatals
                     '("elisp.input-help: unknown choice=\"Nowhere: Nothing\""))))
    (should-not shown)))

;;;; ---- The table against the composer's real bindings ----

(ert-deftest agent-repl-test-input-help-every-topic-variable-is-bound ()
  "Every variable a topic shows the value of is bound."
  (dolist (topic agent-repl-input-help-topics)
    (dolist (binding (plist-get topic :bindings))
      (dolist (variable (plist-get (cddr binding) :variables))
        (should (boundp variable))))))

(ert-deftest agent-repl-test-input-help-every-face-is-an-agent-repl-defface ()
  "input-help.el defines its faces with `agent-repl-defface', so a load re-specs them."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "input-help.el" agent-repl-test--module-dir))
    (should-not (search-forward "(defface " nil t))))

(ert-deftest agent-repl-test-input-help-every-topic-command-is-a-command ()
  "Every command a topic names is an interactive command.
Topics needing a package batch Emacs lacks (magit) are left out."
  (dolist (command (agent-repl-test-input-help--topic-commands t))
    (should (commandp command))))

(ert-deftest agent-repl-test-input-help-every-topic-command-is-bound ()
  "Every command a topic names is bound somewhere in the module."
  (let ((bound (agent-repl-test-input-help--bound-commands)))
    (dolist (command (agent-repl-test-input-help--topic-commands))
      (should (memq command bound)))))

(ert-deftest agent-repl-test-input-help-every-binding-is-in-a-topic ()
  "Every command the module binds is in a topic, or listed as unlisted."
  (let ((listed (append (agent-repl-test-input-help--topic-commands)
                        (mapcar #'car agent-repl-input-help-unlisted-commands))))
    (dolist (command (agent-repl-test-input-help--bound-commands))
      (should (memq command listed)))))

(ert-deftest agent-repl-test-input-help-every-unlisted-command-is-bound ()
  "Every command kept out of the topics is still bound, so the list holds no stale entry."
  (let ((bound (agent-repl-test-input-help--bound-commands)))
    (dolist (entry agent-repl-input-help-unlisted-commands)
      (should (memq (car entry) bound)))))

(ert-deftest agent-repl-test-input-help-no-command-is-both-listed-and-unlisted ()
  "A command is in a topic or unlisted, never both."
  (dolist (entry agent-repl-input-help-unlisted-commands)
    (should-not (memq (car entry) (agent-repl-test-input-help--topic-commands)))))

(ert-deftest agent-repl-test-input-help-every-topic-has-a-group-and-title ()
  "Every topic names its menu group and its title."
  (dolist (topic agent-repl-input-help-topics)
    (should (stringp (plist-get topic :group)))
    (should (stringp (plist-get topic :title)))))

(ert-deftest agent-repl-test-input-help-topic-ids-are-unique ()
  "No two topics share an id."
  (let ((ids (mapcar (lambda (topic) (plist-get topic :id)) agent-repl-input-help-topics)))
    (should (equal ids (delete-dups (copy-sequence ids))))))

(provide 'test-input-help)

;;; test-input-help.el ends here
