;;; test-settings.el --- ERT tests for agent-repl settings.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-settings.el -f ert-run-tests-batch-and-exit
;;
;; settings.el is the Settings page.  These tests pin the saved file (its
;; reading, every refusal, its writing), laying the overrides over the
;; defaults (now, and once a keymap loads), setting and resetting a key,
;; entering a key up to ESC, where an added key is bound and what it may
;; take over, the page's sections and rows, and every command the page runs.
;; Every file lands in a private temporary ZETTEL directory.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;; The live context's maps, special here so a test can `let'-bind them.
(defvar evil-normal-state-map)
(defvar evil-insert-state-map)
(defvar doom-leader-map)
(defvar doom-leader-key)

(defvar agent-repl-test-settings--map nil
  "The keymap the settings tests bind into.")

(defvar agent-repl-test-settings--fatals nil
  "Messages handed to `agent-repl--fatal' during a test, newest first.")

(defun agent-repl-test-settings--a () "Command a." (interactive))
(defun agent-repl-test-settings--b () "Command b." (interactive))

(defconst agent-repl-test-settings--topic
  '(:id t1 :group "G" :title "Topic one"
    :keymaps (agent-repl-test-settings--map)
    :bindings ((agent-repl-test-settings--a "Does a.")
               (agent-repl-test-settings--b "Does b." :variables (agent-repl-send-prefix))))
  "A topic with its own keymap.")

(defmacro agent-repl-test-settings--with (&rest body)
  "Run BODY in a fresh ZETTEL directory, with fresh keymaps and no overrides."
  (declare (indent 0))
  `(let* ((agent-repl-zettel-directory (file-name-as-directory (make-temp-file "zettel" t)))
          (agent-repl-settings-overrides nil)
          (agent-repl-settings-variables nil)
          (agent-repl-settings--pending nil)
          (agent-repl-test-settings--map (make-sparse-keymap))
          (evil-normal-state-map (make-sparse-keymap))
          (evil-insert-state-map (make-sparse-keymap))
          (doom-leader-map (make-sparse-keymap))
          (doom-leader-key "SPC")
          (global-map (make-sparse-keymap))
          (agent-repl-send-prefix "prefix: ")
          (agent-repl-input-help-topics (list agent-repl-test-settings--topic))
          (agent-repl-test-settings--fatals nil))
     (cl-letf (((symbol-function 'agent-repl--fatal)
                (agent-repl-test--fatal-into agent-repl-test-settings--fatals)))
       (unwind-protect (progn ,@body)
         (remove-hook 'after-load-functions #'agent-repl-settings--apply-pending)
         (delete-directory agent-repl-zettel-directory t)))))

(defun agent-repl-test-settings--write (text)
  "Write TEXT as the settings file."
  (make-directory agent-repl-zettel-directory t)
  (with-temp-file (agent-repl-settings-file) (insert text)))

(defun agent-repl-test-settings--keys (&rest events)
  "Return a `read-key' stand-in answering EVENTS in turn."
  (lambda (&rest _) (pop events)))

;;;; ---- The saved settings ----

(ert-deftest agent-repl-test-settings-file-is-in-the-zettel-directory ()
  "The settings file is settings.eld in the ZETTEL directory."
  (let ((agent-repl-zettel-directory "/z/"))
    (should (equal (agent-repl-settings-file) "/z/settings.eld"))))

(ert-deftest agent-repl-test-settings-read-a-missing-file-is-no-settings ()
  "With no file yet there are no overrides and no variables."
  (agent-repl-test-settings--with
    (should (equal (agent-repl-settings--read) '(:keybindings nil :variables nil)))))

(ert-deftest agent-repl-test-settings-read-a-saved-file ()
  "A saved file reads back as it was written."
  (agent-repl-test-settings--with
    (agent-repl-test-settings--write
     "(:keybindings ((:keymap m :key \"C-c x\" :command c :was nil)) :variables ((v . \"x\")))")
    (should (equal (agent-repl-settings--read)
                   '(:keybindings ((:keymap m :key "C-c x" :command c :was nil))
                     :variables ((v . "x")))))))

(ert-deftest agent-repl-test-settings-read-an-unreadable-file-is-fatal ()
  "A file that does not read signals, naming the file and the cause."
  (agent-repl-test-settings--with
    (agent-repl-test-settings--write "(((")
    (should-error (agent-repl-settings--read))
    (should (string-match-p "elisp.settings.read: unreadable file=.*settings.eld cause=(end-of-file)"
                            (car agent-repl-test-settings--fatals)))))

(ert-deftest agent-repl-test-settings-read-a-malformed-file-is-fatal ()
  "A file of the wrong shape signals, naming the file and what it held."
  (agent-repl-test-settings--with
    (agent-repl-test-settings--write "(:keybindings ((:keymap m :key 3)))")
    (should-error (agent-repl-settings--read))
    (should (string-match-p "elisp.settings.read: malformed file=.*settings.eld data="
                            (car agent-repl-test-settings--fatals)))))

(ert-deftest agent-repl-test-settings-save-writes-what-read-reads ()
  "Saving writes the overrides and variables in force, readable back."
  (agent-repl-test-settings--with
    (let ((agent-repl-settings-overrides '((:keymap m :key "a" :command c :was nil)))
          (agent-repl-settings-variables '((v . 1))))
      (agent-repl-settings--save)
      (should (equal (agent-repl-settings--read)
                     '(:keybindings ((:keymap m :key "a" :command c :was nil)) :variables ((v . 1))))))))

(ert-deftest agent-repl-test-settings-save-logs-what-it-wrote ()
  "Saving records the file and how much it holds."
  (let (logged)
    (agent-repl-test-settings--with
      (cl-letf (((symbol-function 'agent-repl--log)
                 (agent-repl-test--record-into logged)))
        (agent-repl-settings--save)))
    (should (string-match-p "elisp.settings.save: file=.*settings.eld overrides=0 variables=0" (car logged)))))

;;;; ---- Applying the layer ----

(ert-deftest agent-repl-test-settings-apply-binds-each-override ()
  "Applying binds every saved override in its keymap, and counts them."
  (agent-repl-test-settings--with
    (agent-repl-test-settings--write
     "(:keybindings ((:keymap agent-repl-test-settings--map :key \"C-c x\" :command agent-repl-test-settings--a :was nil)) :variables nil)")
    (should (= (agent-repl-settings-apply) 1))
    (should (eq (lookup-key agent-repl-test-settings--map (kbd "C-c x")) #'agent-repl-test-settings--a))))

(ert-deftest agent-repl-test-settings-apply-sets-each-variable ()
  "Applying sets every saved variable."
  (agent-repl-test-settings--with
    (agent-repl-test-settings--write "(:keybindings nil :variables ((agent-repl-send-prefix . \"saved: \")))")
    (agent-repl-settings-apply)
    (should (equal agent-repl-send-prefix "saved: "))))

(ert-deftest agent-repl-test-settings-apply-waits-for-an-unloaded-keymap ()
  "An override whose keymap is not loaded waits, and applies once it is."
  (agent-repl-test-settings--with
    (agent-repl-test-settings--write
     "(:keybindings ((:keymap agent-repl-test-settings--later :key \"a\" :command agent-repl-test-settings--a :was nil)) :variables nil)")
    (should (= (agent-repl-settings-apply) 0))
    (should (memq #'agent-repl-settings--apply-pending after-load-functions))
    (defvar agent-repl-test-settings--later (make-sparse-keymap))
    (agent-repl-settings--apply-pending "some-file.el")
    (should (eq (lookup-key agent-repl-test-settings--later "a") #'agent-repl-test-settings--a))
    (should-not agent-repl-settings--pending)
    (should-not (memq #'agent-repl-settings--apply-pending after-load-functions))))

;;;; ---- Changing the layer ----

(ert-deftest agent-repl-test-settings-set-key-records-the-default ()
  "Setting a key binds it, remembers what it ran, and saves."
  (agent-repl-test-settings--with
    (define-key agent-repl-test-settings--map (kbd "C-c x") #'agent-repl-test-settings--b)
    (agent-repl-settings--set-key 'agent-repl-test-settings--map "C-c x" #'agent-repl-test-settings--a)
    (should (eq (lookup-key agent-repl-test-settings--map (kbd "C-c x")) #'agent-repl-test-settings--a))
    (should (equal agent-repl-settings-overrides
                   '((:keymap agent-repl-test-settings--map :key "C-c x"
                      :command agent-repl-test-settings--a :was agent-repl-test-settings--b))))
    (should (equal (plist-get (agent-repl-settings--read) :keybindings) agent-repl-settings-overrides))))

(ert-deftest agent-repl-test-settings-set-key-again-keeps-the-first-default ()
  "A key set twice still resets to the module's own binding."
  (agent-repl-test-settings--with
    (define-key agent-repl-test-settings--map (kbd "C-c x") #'agent-repl-test-settings--b)
    (agent-repl-settings--set-key 'agent-repl-test-settings--map "C-c x" #'agent-repl-test-settings--a)
    (agent-repl-settings--set-key 'agent-repl-test-settings--map "C-c x" nil)
    (should (equal agent-repl-settings-overrides
                   '((:keymap agent-repl-test-settings--map :key "C-c x"
                      :command nil :was agent-repl-test-settings--b))))))

(ert-deftest agent-repl-test-settings-set-key-back-to-default-drops-the-override ()
  "A key set back to what it ran by default leaves no override."
  (agent-repl-test-settings--with
    (define-key agent-repl-test-settings--map (kbd "C-c x") #'agent-repl-test-settings--b)
    (agent-repl-settings--set-key 'agent-repl-test-settings--map "C-c x" nil)
    (agent-repl-settings--set-key 'agent-repl-test-settings--map "C-c x" #'agent-repl-test-settings--b)
    (should-not agent-repl-settings-overrides)
    (should (eq (lookup-key agent-repl-test-settings--map (kbd "C-c x")) #'agent-repl-test-settings--b))))

(ert-deftest agent-repl-test-settings-reset-binds-the-default-back ()
  "Resetting binds each key back to its default, drops it, and saves."
  (agent-repl-test-settings--with
    (define-key agent-repl-test-settings--map (kbd "C-c x") #'agent-repl-test-settings--b)
    (agent-repl-settings--set-key 'agent-repl-test-settings--map "C-c x" #'agent-repl-test-settings--a)
    (agent-repl-settings--reset (copy-sequence agent-repl-settings-overrides))
    (should (eq (lookup-key agent-repl-test-settings--map (kbd "C-c x")) #'agent-repl-test-settings--b))
    (should-not agent-repl-settings-overrides)
    (should-not (plist-get (agent-repl-settings--read) :keybindings))))

(ert-deftest agent-repl-test-settings-overrides-of-matches-command-or-default ()
  "A command's overrides are those that bind it, or took a key from it."
  (let ((agent-repl-settings-overrides
         '((:keymap m :key "a" :command x :was nil)
           (:keymap m :key "b" :command nil :was x)
           (:keymap m :key "c" :command y :was nil))))
    (should (equal (mapcar (lambda (o) (plist-get o :key)) (agent-repl-settings--overrides-of '(x)))
                   '("a" "b")))))

(ert-deftest agent-repl-test-settings-set-variable-sets-and-saves ()
  "Setting a variable sets it and saves it with the settings."
  (agent-repl-test-settings--with
    (agent-repl-settings--set-variable 'agent-repl-send-prefix "new: ")
    (should (equal agent-repl-send-prefix "new: "))
    (should (equal (plist-get (agent-repl-settings--read) :variables) '((agent-repl-send-prefix . "new: "))))))

;;;; ---- Entering a key ----

(ert-deftest agent-repl-test-settings-read-keys-until-escape ()
  "Keys are read until ESC, which is not one of them."
  (cl-letf (((symbol-function 'read-key) (agent-repl-test-settings--keys ?\C-c ?x 'escape)))
    (should (equal (agent-repl-settings-read-keys "New key") [?\C-c ?x]))))

(ert-deftest agent-repl-test-settings-read-keys-takes-a-terminal-escape ()
  "ESC read as the character 27 ends the keys too."
  (cl-letf (((symbol-function 'read-key) (agent-repl-test-settings--keys ?a 27)))
    (should (equal (agent-repl-settings-read-keys "New key") [?a]))))

(ert-deftest agent-repl-test-settings-read-keys-with-none-is-a-user-error ()
  "ESC before any key is no key."
  (cl-letf (((symbol-function 'read-key) (agent-repl-test-settings--keys 'escape)))
    (should (equal (cadr (should-error (agent-repl-settings-read-keys "New key") :type 'user-error))
                   "No key entered"))))

(ert-deftest agent-repl-test-settings-target-in-command-mode ()
  "A section's own keymap takes a command-mode key under Evil's state prefix."
  (agent-repl-test-settings--with
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "command mode")))
      (should (equal (agent-repl-settings--target agent-repl-test-settings--topic [?\C-c ?x])
                     '(agent-repl-test-settings--map . "<normal-state> C-c x"))))))

(ert-deftest agent-repl-test-settings-target-in-every-mode ()
  "A key for every mode is a plain binding in the section's keymap."
  (agent-repl-test-settings--with
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "every mode")))
      (should (equal (agent-repl-settings--target agent-repl-test-settings--topic [?\C-c ?x])
                     '(agent-repl-test-settings--map . "C-c x"))))))

(ert-deftest agent-repl-test-settings-target-leader-key ()
  "A section without keymaps binds a leader sequence into the leader map."
  (agent-repl-test-settings--with
    (should (equal (agent-repl-settings--target '(:id x) [?\s ?j ?z])
                   '(doom-leader-map . "j z")))))

(ert-deftest agent-repl-test-settings-target-global-key ()
  "A section without keymaps binds any other sequence globally."
  (agent-repl-test-settings--with
    (should (equal (agent-repl-settings--target '(:id x) [?\C-c ?z])
                   '(global-map . "C-c z")))))

(ert-deftest agent-repl-test-settings-check-free-a-free-key ()
  "A key that runs nothing can be taken."
  (agent-repl-test-settings--with
    (should-not (agent-repl-settings--check-free 'agent-repl-test-settings--map "C-c x" 'c))))

(ert-deftest agent-repl-test-settings-check-free-under-a-free-prefix ()
  "A key under a prefix that binds nothing yet can be taken."
  (agent-repl-test-settings--with
    (should-not (agent-repl-settings--check-free 'agent-repl-test-settings--map "C-c x y" 'c))))

(ert-deftest agent-repl-test-settings-check-free-takes-over-when-told ()
  "A key running another command is taken when the user agrees."
  (agent-repl-test-settings--with
    (define-key agent-repl-test-settings--map (kbd "C-c x") #'agent-repl-test-settings--b)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (should-not (agent-repl-settings--check-free 'agent-repl-test-settings--map "C-c x"
                                                   #'agent-repl-test-settings--a)))))

(ert-deftest agent-repl-test-settings-check-free-keeps-it-when-refused ()
  "A key running another command stays when the user declines."
  (agent-repl-test-settings--with
    (define-key agent-repl-test-settings--map (kbd "C-c x") #'agent-repl-test-settings--b)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should (equal (cadr (should-error (agent-repl-settings--check-free
                                          'agent-repl-test-settings--map "C-c x" #'agent-repl-test-settings--a)
                                         :type 'user-error))
                     "C-c x keeps running agent-repl-test-settings--b")))))

(ert-deftest agent-repl-test-settings-check-free-refuses-a-prefix ()
  "A key that is a prefix of other keys cannot be bound."
  (agent-repl-test-settings--with
    (define-key agent-repl-test-settings--map (kbd "C-c x y") #'agent-repl-test-settings--b)
    (should (equal (cadr (should-error (agent-repl-settings--check-free 'agent-repl-test-settings--map "C-c x" 'c)
                                       :type 'user-error))
                   "C-c x is a prefix of other keys"))))

(ert-deftest agent-repl-test-settings-check-free-refuses-extending-a-key ()
  "A key that extends a key bound to a command cannot be bound."
  (agent-repl-test-settings--with
    (define-key agent-repl-test-settings--map (kbd "C-c x") #'agent-repl-test-settings--b)
    (should (equal (cadr (should-error (agent-repl-settings--check-free 'agent-repl-test-settings--map "C-c x y" 'c)
                                       :type 'user-error))
                   "C-c x y starts with a key that is not a prefix"))))

;;;; ---- The page ----

(defun agent-repl-test-settings--page ()
  "Draw the page and return its text."
  (agent-repl-settings--redraw)
  (with-current-buffer (agent-repl-settings--buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defmacro agent-repl-test-settings--with-page (&rest body)
  "Run BODY in `agent-repl-test-settings--with', the page's buffer fresh."
  (declare (indent 0))
  `(agent-repl-test-settings--with
     (unwind-protect (progn ,@body)
       (when (get-buffer "*zettel settings*") (kill-buffer "*zettel settings*")))))

(ert-deftest agent-repl-test-settings-page-shows-groups-and-shut-sections ()
  "The page shows each group, and each section shut with its count."
  (agent-repl-test-settings--with-page
    (let ((page (agent-repl-test-settings--page)))
      (should (string-match-p "^Settings\n" page))
      (should (string-match-p "\nG\n ▸ Topic one   2 commands\n" page))
      (should-not (string-match-p "Does a\\." page)))))

(ert-deftest agent-repl-test-settings-page-open-section-shows-rows ()
  "An open section shows each command's keys, its add button and its variables."
  (agent-repl-test-settings--with-page
    (define-key agent-repl-test-settings--map (kbd "C-c a") #'agent-repl-test-settings--a)
    (define-key agent-repl-test-settings--map (kbd "C-c b") #'agent-repl-test-settings--b)
    (agent-repl-settings-toggle 't1)
    (let ((page (agent-repl-test-settings--page)))
      (should (string-match-p "▾ Topic one" page))
      (should (string-match-p "C-c a  \\+ Does a\\." page))
      (should (string-match-p "agent-repl-send-prefix: \"prefix: \"  edit" page)))))

(ert-deftest agent-repl-test-settings-page-marks-a-changed-command ()
  "A changed command shows changed and a reset button; its section counts it."
  (agent-repl-test-settings--with-page
    (define-key agent-repl-test-settings--map (kbd "C-c a") #'agent-repl-test-settings--a)
    (agent-repl-settings--set-key 'agent-repl-test-settings--map "C-c z" #'agent-repl-test-settings--a)
    (agent-repl-settings-toggle 't1)
    (let ((page (agent-repl-test-settings--page)))
      (should (string-match-p "2 commands · 1 changed" page))
      (should (string-match-p "Does a\\.  changed reset" page)))))

(ert-deftest agent-repl-test-settings-toggle-opens-then-shuts ()
  "Toggling a section opens it, and toggling again shuts it."
  (agent-repl-test-settings--with-page
    (define-key agent-repl-test-settings--map (kbd "C-c a") #'agent-repl-test-settings--a)
    (define-key agent-repl-test-settings--map (kbd "C-c b") #'agent-repl-test-settings--b)
    (agent-repl-settings-toggle 't1)
    (should (string-match-p "Does a\\." (agent-repl-test-settings--page)))
    (agent-repl-settings-toggle 't1)
    (should-not (string-match-p "Does a\\." (agent-repl-test-settings--page)))))

(ert-deftest agent-repl-test-settings-toggle-section-off-a-heading-is-a-user-error ()
  "TAB away from a section heading says so."
  (agent-repl-test-settings--with-page
    (with-current-buffer (agent-repl-settings--buffer)
      (agent-repl-settings--redraw)
      (goto-char (point-min))
      (should (equal (cadr (should-error (agent-repl-settings-toggle-section) :type 'user-error))
                     "Not on a section heading")))))

(ert-deftest agent-repl-test-settings-toggle-section-on-a-heading-opens-it ()
  "TAB on a section heading opens it."
  (agent-repl-test-settings--with-page
    (with-current-buffer (agent-repl-settings--buffer)
      (agent-repl-settings--redraw)
      (goto-char (point-min))
      (search-forward "Topic one")
      (goto-char (match-beginning 0))
      (agent-repl-settings-toggle-section)
      (should (memq 't1 agent-repl-settings--open)))))

(ert-deftest agent-repl-test-settings-show-pops-the-page ()
  "The Settings command hands the page to the shared popup."
  (agent-repl-test-settings--with-page
    (agent-repl-test--recording-popups
      (agent-repl-settings)
      (should (equal (mapcar #'buffer-name agent-repl-test--popups-shown) '("*zettel settings*"))))))

(ert-deftest agent-repl-test-settings-keycap-names-its-mode-as-the-help-does ()
  "A keycap's mode note is the help's own state label, so the two read alike."
  (dolist (state '(nil "normal" "insert"))
    (let ((keycap (substring-no-properties
                   (agent-repl-settings--keycap 'c (list :description "K" :state state) '(:id x))))
          (label (agent-repl--input-help-state-label (list state))))
      (should (equal keycap (if label (concat "K " label) "K"))))))

;;;; ---- The page's commands ----

(ert-deftest agent-repl-test-settings-add-key-binds-and-saves ()
  "Adding a key binds the entered keys where the section binds, and saves."
  (agent-repl-test-settings--with-page
    (cl-letf (((symbol-function 'read-key) (agent-repl-test-settings--keys ?\C-c ?q 'escape))
              ((symbol-function 'completing-read) (lambda (&rest _) "every mode")))
      (agent-repl-settings-add-key agent-repl-test-settings--topic #'agent-repl-test-settings--a))
    (should (eq (lookup-key agent-repl-test-settings--map (kbd "C-c q")) #'agent-repl-test-settings--a))
    (should (equal (plist-get (agent-repl-settings--read) :keybindings)
                   '((:keymap agent-repl-test-settings--map :key "C-c q"
                      :command agent-repl-test-settings--a :was nil))))))

(ert-deftest agent-repl-test-settings-edit-key-remove ()
  "Removing a key unbinds it where it was bound."
  (agent-repl-test-settings--with-page
    (define-key agent-repl-test-settings--map (kbd "C-c a") #'agent-repl-test-settings--a)
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?r "remove"))))
      (agent-repl-settings-edit-key agent-repl-test-settings--topic #'agent-repl-test-settings--a
                                    (list :description "C-c a" :keymap 'agent-repl-test-settings--map
                                          :vector (kbd "C-c a"))))
    (should-not (lookup-key agent-repl-test-settings--map (kbd "C-c a")))
    (should (equal agent-repl-settings-overrides
                   '((:keymap agent-repl-test-settings--map :key "C-c a"
                      :command nil :was agent-repl-test-settings--a))))))

(ert-deftest agent-repl-test-settings-edit-key-change ()
  "Changing a key unbinds the old one and binds the entered one."
  (agent-repl-test-settings--with-page
    (define-key agent-repl-test-settings--map (kbd "C-c a") #'agent-repl-test-settings--a)
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?c "change")))
              ((symbol-function 'read-key) (agent-repl-test-settings--keys ?\C-c ?n 'escape))
              ((symbol-function 'completing-read) (lambda (&rest _) "every mode")))
      (agent-repl-settings-edit-key agent-repl-test-settings--topic #'agent-repl-test-settings--a
                                    (list :description "C-c a" :keymap 'agent-repl-test-settings--map
                                          :vector (kbd "C-c a"))))
    (should-not (lookup-key agent-repl-test-settings--map (kbd "C-c a")))
    (should (eq (lookup-key agent-repl-test-settings--map (kbd "C-c n")) #'agent-repl-test-settings--a))))

(ert-deftest agent-repl-test-settings-edit-key-change-refused-changes-nothing ()
  "A change whose new key is refused leaves the old key as it was."
  (agent-repl-test-settings--with-page
    (define-key agent-repl-test-settings--map (kbd "C-c a") #'agent-repl-test-settings--a)
    (define-key agent-repl-test-settings--map (kbd "C-c b") #'agent-repl-test-settings--b)
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?c "change")))
              ((symbol-function 'read-key) (agent-repl-test-settings--keys ?\C-c ?b 'escape))
              ((symbol-function 'completing-read) (lambda (&rest _) "every mode"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should-error (agent-repl-settings-edit-key
                     agent-repl-test-settings--topic #'agent-repl-test-settings--a
                     (list :description "C-c a" :keymap 'agent-repl-test-settings--map
                           :vector (kbd "C-c a")))
                    :type 'user-error))
    (should (eq (lookup-key agent-repl-test-settings--map (kbd "C-c a")) #'agent-repl-test-settings--a))
    (should-not agent-repl-settings-overrides)))

(ert-deftest agent-repl-test-settings-edit-variable-reads-and-saves ()
  "Editing a variable reads its new value and saves it."
  (agent-repl-test-settings--with-page
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "typed: ")))
      (agent-repl-settings-edit-variable 'agent-repl-send-prefix))
    (should (equal agent-repl-send-prefix "typed: "))
    (should (equal (plist-get (agent-repl-settings--read) :variables) '((agent-repl-send-prefix . "typed: "))))))

;;;; ---- The mode ----

(ert-deftest agent-repl-test-settings-mode-map-binds-tab-and-g ()
  "TAB folds the section at point and g draws the page again."
  (should (eq (lookup-key agent-repl-settings-mode-map (kbd "TAB")) #'agent-repl-settings-toggle-section))
  (should (eq (lookup-key agent-repl-settings-mode-map (kbd "g")) #'agent-repl-settings-refresh)))

(provide 'test-settings)

;;; test-settings.el ends here
