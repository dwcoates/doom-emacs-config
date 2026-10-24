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
    (should (equal (substring-no-properties button) " Sending prompts "))
    (should (eq (get-text-property 1 'face button) 'agent-repl-input-help-button))
    (should (eq (get-text-property 1 'mouse-face button) 'mode-line-highlight))))

(ert-deftest agent-repl-test-input-help-button-click-runs-the-click-command ()
  "A button's `mouse-1' in the header line runs `agent-repl-input-help-click'."
  (let ((button (agent-repl--input-help-button '(:id x :title "T"))))
    (should (eq (lookup-key (get-text-property 1 'local-map button) [header-line mouse-1])
                #'agent-repl-input-help-click))))

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
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
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
                   (("normal") "(command mode)")
                   (("insert") "(insert mode)")
                   (("visual" "normal") "(command mode, visual mode)")))
    (should (equal (agent-repl--input-help-state-label states) want))))

(ert-deftest agent-repl-test-input-help-keys-plain-binding ()
  "A plain composer binding is its key, unannotated."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context :composer `((,(kbd "C-c C-c") . cmd))))
                 '("C-c C-c"))))

(ert-deftest agent-repl-test-input-help-keys-state-binding-is-annotated ()
  "A command-mode-only binding is annotated with its state."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context :composer '(([normal-state up] . cmd))))
                 '("<up> (command mode)"))))

(ert-deftest agent-repl-test-input-help-keys-one-key-in-several-states-is-one-label ()
  "A key bound plainly and in both states is listed once, unannotated."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context
                        :composer `((,(kbd "RET") . cmd)
                                    (,(vconcat [normal-state] (kbd "RET")) . cmd)
                                    (,(vconcat [insert-state] (kbd "RET")) . cmd))))
                 '("RET"))))

(ert-deftest agent-repl-test-input-help-keys-remap-resolves-through-global-maps ()
  "A `[remap X]' entry shows the keys X is bound to, with their state."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context
                        :composer '(([remap target] . cmd))
                        :insert '(([S-s-return] . target))))
                 '("S-s-<return> (insert mode)"))))

(ert-deftest agent-repl-test-input-help-keys-leader-binding-is-prefixed ()
  "A leader binding is shown after the leader key."
  (should (equal (agent-repl--input-help-keys
                  'cmd (agent-repl-test-input-help--context :leader `((,(kbd "j RET") . cmd))))
                 '("SPC j RET"))))

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

(ert-deftest agent-repl-test-input-help-text-aligns-keys-and-lists-features ()
  "The text is the title, one aligned row per binding, then the features."
  (should (equal (substring-no-properties
                  (agent-repl--input-help-text
                   '(:title "T" :bindings ((a . "Does a") (b . "Does b")) :features ("F."))
                   (agent-repl-test-input-help--context
                    :composer `((,(kbd "C-c C-k") . a) (,(kbd "RET") . b)))))
                 "T\n\n  C-c C-k  Does a\n  RET      Does b\n\n  - F.\n")))

(ert-deftest agent-repl-test-input-help-text-without-bindings-is-features-only ()
  "A topic with no bindings is its title and features."
  (should (equal (substring-no-properties
                  (agent-repl--input-help-text '(:title "T" :bindings () :features ("F."))
                                               (agent-repl-test-input-help--context)))
                 "T\n\n  - F.\n")))

;;;; ---- Showing a topic ----

(ert-deftest agent-repl-test-input-help-show-pops-a-read-only-topic-buffer ()
  "Showing a topic hands its filled, read-only buffer to the shared popup."
  (let ((context (agent-repl-test-input-help--context :composer `((,(kbd "C-+") . agent-repl-feed-text-scale-increase)
                                                                  (,(kbd "C--") . agent-repl-feed-text-scale-decrease))))
        logged)
    (agent-repl-test--recording-popups
      (cl-letf (((symbol-function 'agent-repl--input-help-binding-context) (lambda () context))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        (agent-repl-input-help-show 'zoom))
      (let ((buf (car agent-repl-test--popups-shown)))
        (unwind-protect
            (progn
              (should (equal (buffer-name buf) "*agent-repl help: Feed zoom*"))
              (with-current-buffer buf
                (should buffer-read-only)
                (should (string-match-p "C-\\+  Make the feed's text larger" (buffer-string))))
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
