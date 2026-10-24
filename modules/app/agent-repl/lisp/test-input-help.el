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
BINDINGS is a plist: `:composer', `:insert', `:normal', `:global' and
`:leader', each a list of (KEY-VECTOR . COMMAND) for that map.  A
`:composer' KEY-VECTOR may start with an Evil `STATE-state' event."
  (cl-flet ((map-of (pairs)
              (let ((map (make-sparse-keymap)))
                (pcase-dolist (`(,key . ,command) pairs)
                  (define-key map key command))
                map)))
    (list :composer (map-of (plist-get bindings :composer))
          :global (list (cons "insert" (map-of (plist-get bindings :insert)))
                        (cons "normal" (map-of (plist-get bindings :normal)))
                        (cons nil (map-of (plist-get bindings :global))))
          :leader (map-of (plist-get bindings :leader))
          :leader-key "SPC")))

(defun agent-repl-test-input-help--click-event (string pos)
  "Return a header-line `mouse-1' event on STRING at POS."
  `(mouse-1 (,(selected-window) header-line (0 . 0) 0 (,string . ,pos)
             nil (0 . 0) nil (0 . 0) (0 . 0))))

(defun agent-repl-test-input-help--topic-commands ()
  "Return every command the help topics name."
  (delete-dups (mapcan (lambda (topic) (mapcar #'car (plist-get topic :bindings)))
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

(defun agent-repl-test-input-help--source-commands (file pred)
  "Return the `#''-quoted commands in FILE's top-level forms PRED accepts.
`map!' is a no-op stub under `emacs -Q', so its bindings are read off the source."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file (file-name-directory
                                                  (symbol-file 'agent-repl-input-help-install))))
    (let (found)
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (funcall pred form)
                (setq found (append (agent-repl-test-input-help--quoted-commands form) found)))))
        (end-of-file nil))
      found)))

(defun agent-repl-test-input-help--composer-map!-p (form)
  "Non-nil when FORM is a `(map! :map agent-repl-input-mode-map ...)'."
  (and (eq (car-safe form) 'map!)
       (eq (nth 1 form) :map)
       (eq (nth 2 form) 'agent-repl-input-mode-map)))

(defun agent-repl-test-input-help--composer-bound-commands ()
  "Return every command the composer binds: its live map, and its `map!' sources."
  (let (found)
    (cl-labels ((walk (map)
                  (map-keymap (lambda (_event def)
                                (cond ((keymapp def) (walk def))
                                      ((and def (symbolp def)) (push def found))))
                              map)))
      (walk agent-repl-input-mode-map))
    (dolist (file '("input.el" "clipboard-image.el"))
      (setq found (append (agent-repl-test-input-help--source-commands
                           file #'agent-repl-test-input-help--composer-map!-p)
                          found)))
    (delete-dups found)))

;;;; ---- The header line ----

(ert-deftest agent-repl-test-input-help-header-line-has-one-button-per-topic-in-order ()
  "The header line carries each topic's id, in table order."
  (let* ((line (agent-repl--input-help-header-line-text))
         (ids nil))
    (dotimes (i (length line))
      (let ((id (get-text-property i 'agent-repl-input-help-topic line)))
        (when (and id (not (eq id (car ids))))
          (push id ids))))
    (should (equal (nreverse ids)
                   (mapcar (lambda (topic) (plist-get topic :id)) agent-repl-input-help-topics)))))

(ert-deftest agent-repl-test-input-help-button-shows-its-title ()
  "A topic's button is its title, padded, with the button face."
  (let ((button (agent-repl--input-help-button '(:id x :title "Sending prompts"))))
    (should (equal (substring-no-properties button) "  Sending prompts  "))
    (should (eq (get-text-property 1 'face button) 'agent-repl-input-help-button))
    (should (eq (get-text-property 1 'mouse-face button) 'mode-line-highlight))))

(ert-deftest agent-repl-test-input-help-button-click-runs-the-click-command ()
  "A button's `mouse-1' in the header line runs `agent-repl-input-help-click'."
  (let ((button (agent-repl--input-help-button '(:id x :title "T"))))
    (should (eq (lookup-key (get-text-property 1 'local-map button) [header-line mouse-1])
                #'agent-repl-input-help-click))))

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
  "Every character outside a button carries the bar's face."
  (let ((line (agent-repl--input-help-header-line-text)))
    (dotimes (i (length line))
      (unless (get-text-property i 'agent-repl-input-help-topic line)
        (should (eq (get-text-property i 'face line) 'agent-repl-input-help-bar))))))

(ert-deftest agent-repl-test-input-help-header-line-stretches-to-the-right-edge ()
  "The header line ends in a bar-faced stretch to the window's right edge."
  (let ((line (agent-repl--input-help-header-line-text)))
    (should (equal (get-text-property (1- (length line)) 'display line) '(space :align-to right)))))

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
                 '(("S-s-<return>" . "insert mode")))))

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
  "The live context is the composer map, Evil's state maps and Doom's leader."
  (let ((evil-insert-state-map (make-sparse-keymap))
        (evil-normal-state-map (make-sparse-keymap))
        (doom-leader-map (make-sparse-keymap))
        (doom-leader-key "SPC"))
    (let ((context (agent-repl--input-help-binding-context)))
      (should (eq (plist-get context :composer) agent-repl-input-mode-map))
      (should (equal (plist-get context :global)
                     (list (cons "insert" evil-insert-state-map)
                           (cons "normal" evil-normal-state-map)
                           (cons nil (current-global-map)))))
      (should (eq (plist-get context :leader) doom-leader-map))
      (should (equal (plist-get context :leader-key) "SPC")))))

;;;; ---- Topics and their text ----

(ert-deftest agent-repl-test-input-help-topic-by-id-and-title ()
  "A topic is found by its id or by its title."
  (should (eq (plist-get (agent-repl--input-help-topic :id 'history) :id) 'history))
  (should (eq (plist-get (agent-repl--input-help-topic :title "Prompt history") :id) 'history)))

(ert-deftest agent-repl-test-input-help-topic-unknown-is-fatal ()
  "An unknown topic signals, and the record names it and the known ids."
  (agent-repl-test-input-help--capturing-fatals
    (should-error (agent-repl--input-help-topic :id 'nope))
    (should (string-match-p "unknown :id=nope known=(sending "
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
                 '(("1-9")))))

(ert-deftest agent-repl-test-input-help-collapse-digits-cases ()
  "Only consecutive digits sharing an annotation join one range."
  (pcase-dolist (`(,keys ,want)
                 '(((("1") ("2") ("3")) (("1-3")))
                   ((("1") ("3")) (("1") ("3")))
                   ((("1") ("2" . "insert mode")) (("1") ("2" . "insert mode")))
                   ((("RET") ("1") ("2")) (("RET") ("1-2")))
                   ((("C-1") ("C-2")) (("C-1") ("C-2")))))
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
  (let ((context (agent-repl-test-input-help--context :composer `((,(kbd "C-+") . agent-repl-feed-text-scale-increase)
                                                                  (,(kbd "C--") . agent-repl-feed-text-scale-decrease))))
        logged)
    (agent-repl-test--recording-popups
      (cl-letf (((symbol-function 'agent-repl--input-help-binding-context) (lambda () context))
                ((symbol-function 'agent-repl--log)
                 (agent-repl-test--record-into logged)))
        (agent-repl-input-help-show 'zoom))
      (let ((buf (car agent-repl-test--popups-shown)))
        (unwind-protect
            (progn
              (should (equal (buffer-name buf) "*agent-repl help: Feed zoom*"))
              (with-current-buffer buf
                (should buffer-read-only)
                (should visual-line-mode)
                (should-not truncate-lines)
                (should (string-match-p "C-\\+ Make the feed's text larger" (buffer-string))))
              (should (member "elisp.input-help.show: id=zoom buffer=*agent-repl help: Feed zoom*"
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
                 (lambda () (agent-repl-test-input-help--context))))
        (let ((before (buffer-list)))
          (should-error (agent-repl-input-help-show 'zoom))
          (should-not agent-repl-test--popups-shown)
          (should (equal (buffer-list) before)))))))

(ert-deftest agent-repl-test-input-help-click-shows-the-clicked-topic ()
  "Clicking a button shows the topic its text carries."
  (let ((line (agent-repl--input-help-header-line-text))
        shown)
    (cl-letf (((symbol-function 'agent-repl-input-help-show) (lambda (id) (push id shown))))
      (agent-repl-input-help-click
       (agent-repl-test-input-help--click-event line (string-search "Prompt history" line))))
    (should (equal shown '(history)))))

(ert-deftest agent-repl-test-input-help-click-off-a-button-is-fatal ()
  "A click whose text carries no topic signals and shows nothing."
  (let (shown)
    (agent-repl-test-input-help--capturing-fatals
      (cl-letf (((symbol-function 'agent-repl-input-help-show) (lambda (id) (push id shown))))
        (should-error (agent-repl-input-help-click
                       (agent-repl-test-input-help--click-event "plain" 0))))
      (should (string-match-p "no topic at posn" (car agent-repl-test-input-help--fatals))))
    (should-not shown)))

(ert-deftest agent-repl-test-input-help-command-shows-the-titled-topic ()
  "`agent-repl-input-help' shows the topic it is given by title."
  (let (shown)
    (cl-letf (((symbol-function 'agent-repl-input-help-show) (lambda (id) (push id shown))))
      (agent-repl-input-help "Images"))
    (should (equal shown '(images)))))

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
  "Every command a topic names is an interactive command."
  (dolist (command (agent-repl-test-input-help--topic-commands))
    (should (commandp command))))

(ert-deftest agent-repl-test-input-help-every-topic-command-is-bound ()
  "Every command a topic names is bound in the composer or the leader map."
  (let ((bound (append (agent-repl-test-input-help--composer-bound-commands)
                       (agent-repl-test-input-help--source-commands
                        "keybindings.el" (lambda (form) (and (eq (car-safe form) 'map!)
                                                             (eq (nth 1 form) :leader)))))))
    (dolist (command (agent-repl-test-input-help--topic-commands))
      (should (memq command bound)))))

(ert-deftest agent-repl-test-input-help-every-composer-binding-is-in-a-topic ()
  "Every command the composer binds is listed in some help topic."
  (let ((listed (agent-repl-test-input-help--topic-commands)))
    (dolist (command (agent-repl-test-input-help--composer-bound-commands))
      (should (memq command listed)))))

(provide 'test-input-help)

;;; test-input-help.el ends here
