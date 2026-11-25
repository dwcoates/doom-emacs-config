;;; test-rules-views.el --- Rule tests: the views -*- lexical-binding: t; -*-

;;; Commentary:

;; Rules over the sources that draw and arrange what the user sees: panels,
;; popups, the web host, windows, workspaces, the roster, the status bar, the
;; composer and its help, each routing one concern through one helper.
;;
;; A rule test reads the module's elisp sources as data and holds them to a
;; structural rule.  Rule tests are no one module's tests, so they live by
;; rule area in `test-rules-*.el', the `ert-rules' suite every branch runs
;; before its merge is admitted (lisp/AGENTS.md, "Rule tests").

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- composer-mark.el ----

(defconst agent-repl-test-composer-mark--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "This suite's directory, where the composer's mark kinds' sources live.")

(ert-deftest agent-repl-test-composer-mark-every-kind-marks-through-the-helper ()
  "Every composer mark function calls `agent-repl-composer-mark-lines', none hand-rolls one."
  (let ((dir agent-repl-test-composer-mark--dir))
    (dolist (file '("command-completion.el" "workspace-completion.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file dir))
        (should (search-forward "(agent-repl-composer-mark-lines" nil t))
        (goto-char (point-min))
        (should-not (search-forward "(make-overlay" nil t))))))

;;;; ---- frame-border.el ----

(defconst agent-repl-test-frame-border--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The module's lisp directory, for the ownership scan below.")

(ert-deftest agent-repl-test-frame-border-is-the-only-writer-of-the-border ()
  "No module but frame-border.el sets the `internal-border' face."
  ;; Arrange
  (let ((writers nil))
    ;; Act
    (dolist (file (directory-files agent-repl-test-frame-border--lisp-dir t "\\`[^.].*\\.el\\'"))
      (unless (or (string-prefix-p "test-" (file-name-nondirectory file))
                  (equal (file-name-nondirectory file) "frame-border.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward "(set-face-attribute 'internal-border" nil t)
            (push (file-name-nondirectory file) writers)))))
    ;; Assert
    (should-not writers)))

;;;; ---- input-help.el ----

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

(ert-deftest agent-repl-test-input-help-every-face-is-an-agent-repl-defface ()
  "input-help.el defines its faces with `agent-repl-defface', so a load re-specs them."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "input-help.el" agent-repl-test--module-dir))
    (should-not (search-forward "(defface " nil t))))

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

;;;; ---- magit.el ----

(defconst agent-repl-test-magit--source
  (expand-file-name "magit.el" (file-name-directory (or load-file-name buffer-file-name)))
  "The magit.el source these tests load.")

(ert-deftest agent-repl-test-magit-every-display-binding-goes-through-the-helper ()
  "No magit.el definition but the helper let-binds the display function itself."
  (with-temp-buffer
    (insert-file-contents agent-repl-test-magit--source)
    (should (= (how-many "(let\\*? ((magit-display-buffer-function" (point-min) (point-max)) 1))))

;;;; ---- minibuffer-ground.el ----

(defconst agent-repl-test-minibuffer-ground--source
  (expand-file-name "minibuffer-ground.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The module's source, for the scan below.")

(ert-deftest agent-repl-test-minibuffer-ground-names-no-color-of-its-own ()
  "The module writes no hex color: it wears `agent-repl-input-background'."
  (with-temp-buffer
    ;; Arrange
    (insert-file-contents agent-repl-test-minibuffer-ground--source)
    ;; Act
    (let ((literal (re-search-forward "#[0-9a-fA-F]\\{6\\}" nil t)))
      ;; Assert
      (should-not literal))))

;;;; ---- open-tabs.el ----

(ert-deftest agent-repl-test-open-tabs-every-name-to-row-read-shares-ws-row ()
  "No function of open-tabs.el reads a workspace's id by its ref by hand."
  ;; Arrange
  (let ((source (with-temp-buffer
                  (insert-file-contents (replace-regexp-in-string "\\.elc\\'" ".el" (symbol-file 'agent-repl-open-tabs--ws-row)))
                  (buffer-string))))
    ;; Act / Assert: the one read is `agent-repl-open-tabs--ws-id' itself.
    (should (= (agent-repl-test-open-tabs--occurrences "(agent-repl--ws-get ws :ref) :id)" source) 1))))

(defun agent-repl-test-open-tabs--occurrences (needle haystack)
  "How many times NEEDLE occurs in HAYSTACK."
  (let ((count 0) (start 0))
    (while (setq start (string-search needle haystack start))
      (setq count (1+ count) start (1+ start)))
    count))

;;;; ---- page-unload.el ----

(defconst agent-repl-test-pu--page-drops
  '(agent-repl-webhost-remove agent-repl--frontend-detach-webview)
  "The calls that drop a workspace's page.")

(defconst agent-repl-test-pu--unannounced-drops
  '((agent-repl--frontend-detach-webview . "the drop primitive itself; its callers announce")
    (agent-repl--frontend-release-workspace-webview . "the workspace is ending: the daemon ended it first"))
  "The definitions that may drop a page unannounced, each with why.")

(defun agent-repl-test-pu--unannounced-drop-p (form)
  "Non-nil when FORM calls a page drop outside `agent-repl-page-drop-announced'."
  (cond
   ((not (consp form)) nil)
   ((eq (car form) 'agent-repl-page-drop-announced) nil)
   ((memq (car form) '(declare-function quote function))
    (and (eq (car form) 'function) (memq (cadr form) agent-repl-test-pu--page-drops)))
   ((memq (car form) agent-repl-test-pu--page-drops) t)
   (t (let ((found nil) (rest form))
        (while (and (consp rest) (not found))
          (setq found (agent-repl-test-pu--unannounced-drop-p (car rest))
                rest (cdr rest)))
        found))))

(defun agent-repl-test-pu--unannounced-droppers (forms)
  "The names of the definitions among FORMS that drop a page unannounced."
  (let (out)
    (dolist (form forms (nreverse out))
      (when (and (consp form) (memq (car form) '(defun defmacro cl-defun))
                 (agent-repl-test-pu--unannounced-drop-p (cdddr form)))
        (push (cadr form) out)))))

(defun agent-repl-test-pu--source-forms ()
  "Every top-level form of agent-repl's elisp sources, tests excluded."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name
                                      (locate-library "test-page-unload"))))
        forms)
    (dolist (file (directory-files dir t "\\`[^.].*\\.el\\'") forms)
      (unless (string-prefix-p "test-" (file-name-nondirectory file))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (while t (push (read (current-buffer)) forms))
            (end-of-file nil)))))))

(defvar agent-repl-test-pu--source-forms (agent-repl-test-pu--source-forms)
  "The sources' forms, read once when this file loads.")

(ert-deftest agent-repl-test-pu-every-page-drop-is-announced ()
  "Only the named exceptions drop a page without telling the daemon first."
  ;; Act
  (let ((droppers (agent-repl-test-pu--unannounced-droppers agent-repl-test-pu--source-forms)))
    ;; Assert
    (should (equal (sort (mapcar #'symbol-name droppers) #'string<)
                   (sort (mapcar (lambda (e) (symbol-name (car e)))
                                 agent-repl-test-pu--unannounced-drops)
                         #'string<)))))

(ert-deftest agent-repl-test-pu-the-drop-scan-catches-a-hand-rolled-drop ()
  "A definition that drops a page itself is found; one through the helper is not."
  ;; Arrange
  (let ((forms '((defun hand-rolled (ws buf) (when buf (agent-repl--frontend-detach-webview ws buf)))
                 (defun by-symbol (ws) (mapc #'agent-repl-webhost-remove (list ws)))
                 (defun announced (ws)
                   (agent-repl-page-drop-announced
                    ws #'always (lambda () (agent-repl-webhost-remove ws)) #'ignore)))))
    ;; Act
    (let ((droppers (agent-repl-test-pu--unannounced-droppers forms)))
      ;; Assert
      (should (equal droppers '(hand-rolled by-symbol))))))

(defun agent-repl-test-pu--calls-p (form fn)
  "Non-nil when FORM calls FN anywhere inside it, quoted forms aside."
  (cond
   ((not (consp form)) nil)
   ((eq (car form) 'quote) nil)
   ((eq (car form) fn) t)
   (t (let ((found nil) (rest form))
        (while (and (consp rest) (not found))
          (setq found (agent-repl-test-pu--calls-p (car rest) fn)
                rest (cdr rest)))
        found))))

(ert-deftest agent-repl-test-page-unload-only-the-one-loader-loads-a-page ()
  "`agent-repl-page-unload-load' is the one caller of the host's page load.
A second caller would load a page at a moment the page rule does not name."
  ;; Act
  (let (callers)
    (dolist (form agent-repl-test-pu--source-forms)
      (when (and (consp form) (memq (car form) '(defun defmacro cl-defun))
                 (agent-repl-test-pu--calls-p (cdddr form) 'agent-repl-webhost-ensure-page))
        (push (cadr form) callers)))
    ;; Assert
    (should (equal callers '(agent-repl-page-unload-load)))))

(ert-deftest agent-repl-test-page-unload-the-loader-scan-catches-a-second-caller ()
  "The scan finds a call to the host's page load nested anywhere in a body."
  (should (agent-repl-test-pu--calls-p
           '(when x (let ((y 1)) (agent-repl-webhost-ensure-page ws)))
           'agent-repl-webhost-ensure-page))
  (should-not (agent-repl-test-pu--calls-p
               '(when x '(agent-repl-webhost-ensure-page ws))
               'agent-repl-webhost-ensure-page)))

;;;; ---- panels.el ----

(defconst agent-repl-test-panels--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory holding the elisp sources, for the source scans below.")

(ert-deftest agent-repl-test-panels-webview-window-is-the-one-lookup ()
  "Every caller handing the keyboard to the feed looks the window up through
`agent-repl--webview-window', never by hand off `:frontend-buffer'."
  (let ((hits 0))
    (dolist (file '("panels.el" "verbs.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file agent-repl-test-panels--lisp-dir))
        (goto-char (point-min))
        (while (re-search-forward "(get-buffer-window[[:space:]]+webview)" nil t)
          (setq hits (1+ hits)))))
    ;; The helper's own body is the one site.
    (should (= hits 1))))

(ert-deftest agent-repl-test-panels-open-placeholder-is-asked-only-through-the-helper ()
  "panels.el asks open-progress.el for the placeholder in one place only."
  ;; Arrange
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "panels.el" agent-repl-test-panels--lisp-dir))
                  (buffer-string))))
    ;; Act
    (let ((calls (cl-loop with start = 0
                          while (string-match "(agent-repl--open-progress-placeholder " source start)
                          count t do (setq start (match-end 0)))))
      ;; Assert
      (should (= calls 1)))))

(defconst agent-repl-test-panels--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The lisp/ directory the mount sources are read from.")

(ert-deftest agent-repl-test-panels-every-mount-saves-through-the-one-helper ()
  "No mount site records `:fullscreen-config' by hand.
The layout under the mount is saved by `agent-repl--save-pre-panel-layout'
alone; a site writing a configuration into the key itself would save
whatever the mount had already put on the frame.  The helper saves through
`agent-repl-window--save-configuration', which keeps the side and panel
window parameters a restore must put back."
  (let ((dir agent-repl-test-panels--dir))
    (dolist (file '("frontend.el" "open-progress.el" "panels.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file dir))
        (goto-char (point-min))
        (let ((hits 0))
          (while (re-search-forward
                  ":fullscreen-config (agent-repl-window--save-configuration)" nil t)
            (setq hits (1+ hits)))
          (should (= hits (if (equal file "panels.el") 1 0))))))))

(ert-deftest agent-repl-test-panels-mount-awaited-view-is-the-one-site-that-announces-a-view ()
  "No module announces a made view to the panels by hand.
Every mount a switch or a pass owes goes through
`agent-repl--panels-mount-awaited-view', so a site that mounted and
announced on its own could drift from it: panels.el holds the one call."
  (let ((dir agent-repl-test-panels--dir))
    (dolist (file '("page-unload.el" "webview-recovery.el" "panels.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file dir))
        (goto-char (point-min))
        (let ((hits 0))
          (while (re-search-forward "^[ \t]+(agent-repl--panels-on-view-created ws)" nil t)
            (setq hits (1+ hits)))
          (should (equal (cons file hits)
                         (cons file (if (equal file "panels.el") 1 0)))))))))

;;;; ---- popup.el ----

(defconst agent-repl-test-popup--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The lisp/ directory the module sources live in.")

(defun agent-repl-test-popup--source (name)
  "Return the text of the module source NAME under lisp/."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name agent-repl-test-popup--lisp-dir))
    (buffer-string)))

(ert-deftest agent-repl-test-popup-no-module-displays-a-popup-itself ()
  "No module calls `display-buffer' for a popup: popup.el picks its window itself.
magit.el's same-window display is the one call, and it is not a popup."
  ;; Arrange
  (let ((sources (cl-remove-if (lambda (name) (string-prefix-p "test-" name))
                               (directory-files agent-repl-test-popup--lisp-dir nil "\\.el\\'"))))
    ;; Act
    (let ((callers (cl-remove-if-not
                    (lambda (name)
                      (string-match-p "(display-buffer\\(-in-side-window\\)? "
                                      (agent-repl-test-popup--source name)))
                    sources)))
      ;; Assert
      (should (equal callers '("magit.el"))))))

(ert-deftest agent-repl-test-popup-information-views-use-the-shared-popup ()
  "The build log and the health report are shown through `agent-repl-popup-show'."
  (dolist (name '("daemon.el" "verbs.el"))
    (should (string-match-p "(agent-repl-popup-show " (agent-repl-test-popup--source name)))))

;;;; ---- prompt-entries.el ----

(ert-deftest agent-repl-test-prompt-entries-every-opened-entry-is-opened-by-the-one-helper ()
  "`M-j'/`M-k' and a branch click open entries through `agent-repl-prompt-entries-opening'."
  (dolist (case '(("prompt-entries.el" "agent-repl-prompt-entries--open-on")
                  ("input.el" "agent-repl-input-compose-entry")))
    ;; Arrange
    (let ((source (with-temp-buffer
                    (insert-file-contents (expand-file-name (car case) agent-repl-test--module-dir))
                    (goto-char (point-min))
                    (re-search-forward (concat "^(defun " (regexp-quote (cadr case)) " "))
                    (beginning-of-line)
                    (buffer-substring (point) (progn (forward-sexp) (point))))))
      ;; Act / Assert: the shared helper, never a hand-rolled " - ".
      (should (equal (list (cadr case)
                           (and (string-match-p "(agent-repl-prompt-entries-opening " source) t)
                           (string-match-p "\" - \"" source))
                     (list (cadr case) t nil))))))

(defun agent-repl-test-prompt-entries--defun-source (name)
  "The source of NAME's `defun' in prompt-entries.el."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "prompt-entries.el" agent-repl-test--module-dir))
    (goto-char (point-min))
    (re-search-forward (concat "^(defun " (regexp-quote name) " "))
    (beginning-of-line)
    (buffer-substring (point) (progn (forward-sexp) (point)))))

(ert-deftest agent-repl-test-prompt-entries-every-line-beside-an-entry-opens-through-open-on ()
  "`M-j'/`M-k' open their line through `agent-repl-prompt-entries--open-on', never by hand."
  (dolist (name '("agent-repl-prompt-entries--open-sibling"))
    ;; Arrange
    (let ((source (agent-repl-test-prompt-entries--defun-source name)))
      ;; Act / Assert
      (should (equal (list name
                           (and (string-match-p "(agent-repl-prompt-entries--open-on\\_>" source) t)
                           (string-match-p "(evil-open-\\(below\\|above\\) " source))
                     (list name t nil))))))

(ert-deftest agent-repl-test-prompt-entries-every-entry-command-acts-through-on-entry ()
  "Stepping and opening a sibling both act through `agent-repl-prompt-entries--on-entry', never a hand-rolled refusal."
  (dolist (name '("agent-repl-prompt-entries--step" "agent-repl-prompt-entries--open-sibling"))
    ;; Arrange
    (let ((source (agent-repl-test-prompt-entries--defun-source name)))
      ;; Act / Assert
      (should (equal (list name
                           (and (string-match-p "(agent-repl-prompt-entries--on-entry\\_>" source) t)
                           (string-match-p "user-error\\|agent-repl--input-flash" source))
                     (list name t nil))))))

;;;; ---- roster.el ----

(defconst agent-repl-test-roster--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory this suite and `roster.el' live in.")

(ert-deftest agent-repl-test-roster-ws-repository-is-the-commands-only-lookup ()
  "commands.el reads a workspace's repository only through the shared lookup.
A hand-rolled ref-to-section walk there would drift from the helper."
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "commands.el" agent-repl-test-roster--dir))
                  (buffer-string))))
    (should (string-match-p "(agent-repl-roster-ws-repository " source))
    (should-not (string-match-p "(agent-repl-roster-repository-of " source))))

(ert-deftest agent-repl-test-roster-for-ws-readers-share-the-one-shape ()
  "Every `agent-repl-roster-*-for-ws' reader goes through `agent-repl-roster--for-ws'."
  ;; Arrange
  (let ((forms nil))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "roster.el" agent-repl-test-roster--dir))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil)))
    ;; Act
    (let ((readers (seq-filter (lambda (form)
                                 (and (eq (car-safe form) 'defun)
                                      (string-match-p "\\`agent-repl-roster-.*-for-ws\\'"
                                                      (symbol-name (nth 1 form)))
                                      (not (memq (nth 1 form) '(agent-repl-roster-row-for-ws
                                                                agent-repl-roster--for-ws)))))
                               forms)))
      ;; Assert
      (should (>= (length readers) 4))
      (dolist (form readers)
        (should (eq (car-safe (car (last form))) 'agent-repl-roster--for-ws))))))

(ert-deftest agent-repl-test-roster-every-move-to-a-selected-workspace-goes-through-move-frame ()
  "The roster's follow and Emacs's own switch request move the frame
through `agent-repl-roster-move-frame', and neither switches on its own."
  (dolist (spec '(("roster.el" . "agent-repl-roster--follow")
                  ("host.el" . "agent-repl-host--move-on-request")))
    (with-temp-buffer
      ;; Arrange
      (insert-file-contents (expand-file-name (car spec) agent-repl-test--module-dir))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (re-search-forward (concat "^(defun " (regexp-quote (cdr spec)) " "))
      (beginning-of-defun)
      ;; Act
      (let ((body (buffer-substring (point) (progn (end-of-defun) (point)))))
        ;; Assert
        (should (string-match-p "(agent-repl-roster-move-frame " body))
        (should-not (string-match-p "(agent-repl--ws-switch " body))))))

;;;; ---- status.el ----

(ert-deftest agent-repl-test-status-priority-labels-are-the-shared-vocabulary ()
  "The labels are proto/vocab/priority-labels.json's, never spelled in status.el."
  (let ((vocab (agent-repl-vocab-read "priority-labels.json" "labels")))
    (should (= (hash-table-count vocab) (length agent-repl-status--priority-labels)))
    (dolist (cell agent-repl-status--priority-labels)
      (should (equal (gethash (car cell) vocab) (cdr cell)))))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "status.el" agent-repl-test--module-dir))
    (should-not (search-forward "\"P0.5\"" nil t))))

(ert-deftest agent-repl-test-every-colored-tab-face-is-an-agent-repl-defface ()
  "Each colored tab face is defined by `agent-repl-defface', so a load re-specs it."
  ;; Arrange
  (let ((defined nil))
    ;; Act
    (dolist (form (agent-repl-test--file-forms
                   (expand-file-name "status.el" agent-repl-test--module-dir)))
      (when (and (eq (car-safe form) 'agent-repl-defface)
                 (eq (car-safe (cadr form)) 'quote))
        (push (cadr (cadr form)) defined)))
    ;; Assert
    (dolist (pair agent-repl--tab-state-faces)
      (should (memq (car pair) defined)))))

;;;; ---- webhost.el ----

(defun agent-repl-test-wh--count-in-source (file needle)
  "How many times NEEDLE occurs in lisp/FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file agent-repl-test--module-dir))
    (let ((n 0))
      (while (search-forward needle nil t) (cl-incf n))
      n)))

(ert-deftest agent-repl-test-wh-both-views-watch-their-loads-through-the-shared-helper ()
  "The host and the sidebar install their callbacks through the one helper."
  (should (= 1 (agent-repl-test-wh--count-in-source "webhost.el" "(agent-repl--webview-watch-load xw '")))
  (should (= 1 (agent-repl-test-wh--count-in-source "sidebar-view.el" "(agent-repl--webview-watch-load xw '"))))

(ert-deftest agent-repl-test-wh-no-view-sets-its-callback-by-hand ()
  "Only the shared helper puts an xwidget's callback.
The host's one other put is its hot-reload heal (`agent-repl--webhost-rewatch'),
which restores a known prior handler rather than keeping the current one."
  (should (= 2 (agent-repl-test-wh--count-in-source "webhost.el" "(xwidget-put xw 'callback")))
  (should (= 0 (agent-repl-test-wh--count-in-source "sidebar-view.el" "(xwidget-put xw 'callback"))))

(ert-deftest agent-repl-test-wh-every-held-page-entry-is-built-through-the-one-helper ()
  "Every held page's entry is made by `agent-repl--webhost-page-entry'."
  (should (= 1 (agent-repl-test-wh--count-in-source "webhost.el" "(list :url"))))

(ert-deftest agent-repl-test-wh-both-views-record-answers-through-the-shared-helper ()
  "The host and the sidebar record their answers through the one helper."
  (should (= 1 (agent-repl-test-wh--count-in-source "webhost.el" "(agent-repl--webview-answered raw \"elisp.webhost\"")))
  (should (= 1 (agent-repl-test-wh--count-in-source "sidebar-view.el" "(agent-repl--webview-answered raw \"elisp.sidebar-view\"")))
  (should (= 0 (agent-repl-test-wh--count-in-source "sidebar-view.el" "(agent-repl-webhost-parse-answer raw)"))))

(ert-deftest agent-repl-test-wh-both-views-decide-a-commit-through-the-shared-helper ()
  "The host and the sidebar decide a committed load through the one helper."
  (should (= 1 (agent-repl-test-wh--count-in-source "webhost.el" "(agent-repl--webview-on-commit\n")))
  (should (= 1 (agent-repl-test-wh--count-in-source "sidebar-view.el" "(agent-repl--webview-on-commit status")))
  (should (= 0 (agent-repl-test-wh--count-in-source "sidebar-view.el" "\"load-committed\""))))

(ert-deftest agent-repl-test-wh-both-views-read-their-boot-through-the-shared-helper ()
  "The host and the sidebar read a finished load's boot through the one helper."
  (should (= 1 (agent-repl-test-wh--count-in-source "webhost.el" "(agent-repl--webview-on-finish\n")))
  (should (= 1 (agent-repl-test-wh--count-in-source "sidebar-view.el" "(agent-repl--webview-on-finish status")))
  (should (= 0 (agent-repl-test-wh--count-in-source "sidebar-view.el" "\"load-finished\""))))

(ert-deftest agent-repl-test-wh-every-document-load-goes-through-the-generation-url ()
  "No host or sidebar load spells its URL without the reload generation.
A load that did would state no generation, and the daemon could not tell a
host that has not reloaded yet from one that ignored its reload."
  ;; Arrange
  (let (offenders)
    (dolist (file '("webhost.el" "sidebar-view.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file agent-repl-test--module-dir))
        (goto-char (point-min))
        ;; Act
        (while (re-search-forward "(agent-repl--\\(frontend-webview-navigate-widget\\|webview-make-hidden\\)[ \n]" nil t)
          (let ((call (save-excursion
                        (goto-char (match-beginning 0))
                        (buffer-substring-no-properties (point) (progn (forward-sexp) (point))))))
            (unless (or (string-match-p "agent-repl-webhost-document-url" call)
                        (save-excursion (beginning-of-defun)
                                        (looking-at "(defun agent-repl--webview-make-hidden ")))
              (push (format "%s:%d" file (line-number-at-pos)) offenders))))))
    ;; Assert
    (should-not offenders)))

;;;; ---- window.el ----

(ert-deftest agent-repl-test-window-perspective-records-share-the-one-scope-helper ()
  "window.el scopes every record about a perspective through `--ws-log-scope'.
`agent-repl--ws-log-name' answers nil for a name without a sink, which
leaves the record to the ambient context; a site using it would raise a
routing error again whenever it runs outside one."
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "window.el"
                                     (file-name-directory
                                      (symbol-file 'agent-repl-window--panel-window))))
                  (buffer-string))))
    (should-not (string-match-p "(agent-repl--ws-log-name " source))))

(ert-deftest agent-repl-test-window-panels-saves-and-restores-layouts-through-the-helpers ()
  "panels.el saves and restores every layout through the window.el pair.
A bare `current-window-configuration' or `set-window-configuration'
drops the layout parameters, which is how the sidebar's side survived a
close."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "panels.el"
                       (file-name-directory
                        (symbol-file 'agent-repl-window--panel-window))))
    (should (= 0 (how-many (regexp-quote "(set-window-configuration ") (point-min) (point-max))))
    (should (= 0 (how-many (regexp-quote "(current-window-configuration)") (point-min) (point-max))))
    (should (= 2 (how-many (regexp-quote "(agent-repl-window--restore-configuration ")
                           (point-min) (point-max))))
    (should (= 2 (how-many (regexp-quote "(agent-repl-window--save-configuration)")
                           (point-min) (point-max))))))

;;;; ---- workspace.el ----

(ert-deftest agent-repl-test-both-ref-id-lookups-share-one-rule ()
  "`agent-repl--ws-by-ref-id' and `agent-repl--ws-ref-id-table' find a
workspace by `agent-repl--ws-live-ref-id', and neither reads :killed-at
itself, so the two cannot drift apart."
  (dolist (fn '("agent-repl--ws-by-ref-id" "agent-repl--ws-ref-id-table"))
    (with-temp-buffer
      ;; Arrange
      (insert-file-contents (expand-file-name "workspace.el" agent-repl-test--module-dir))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (re-search-forward (concat "^(defun " (regexp-quote fn) " "))
      (beginning-of-defun)
      ;; Act
      (let ((body (buffer-substring (point) (progn (end-of-defun) (point)))))
        ;; Assert
        (should (string-match-p "agent-repl--ws-live-ref-id" body))
        (should-not (string-match-p ":killed-at)" body))))))

(ert-deftest agent-repl-test-roster-wide-walks-resolve-rows-through-the-ref-id-table ()
  "The walks over every roster row read one table per push, never one
registry scan per row (`agent-repl--ws-ref-id-table')."
  (dolist (spec '(("status.el" . "agent-repl-status-sync-attention")
                  ("roster.el" . "agent-repl-roster--walk-status-transitions")))
    (with-temp-buffer
      ;; Arrange
      (insert-file-contents (expand-file-name (car spec) agent-repl-test--module-dir))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (re-search-forward (concat "^(defun " (regexp-quote (cdr spec)) " "))
      (beginning-of-defun)
      ;; Act
      (let ((body (buffer-substring (point) (progn (end-of-defun) (point)))))
        ;; Assert
        (should (string-match-p "agent-repl--ws-ref-id-table" body))
        (should-not (string-match-p "agent-repl--ws-by-ref-id" body))))))

(defconst agent-repl-test-ws--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The `lisp/' directory this suite lives in, captured at LOAD time.")

(ert-deftest agent-repl-test-no-source-calls-doom-workspace-kill ()
  "No agent-repl source under `lisp/' calls or references Doom's `+workspace/kill'
as code.  Its current-workspace branch puts `doom-fallback-buffer' into
the landing workspace's panel window; teardown goes through
`agent-repl--ws-land-then-kill' instead.  Mentions in docstrings and
comments, written `+workspace/kill' with a leading backquote, are allowed."
  ;; Arrange
  (let ((offenders nil))
    (dolist (file (directory-files agent-repl-test-ws--lisp-dir t "\\`[^.].*\\.el\\'"))
      (unless (string-prefix-p "test-" (file-name-nondirectory file))
        (with-temp-buffer
          (insert-file-contents file)
          ;; Act
          (goto-char (point-min))
          (while (re-search-forward "\\(?:(\\|'\\|declare-function \\)\\+workspace/kill\\_>" nil t)
            (push (format "%s:%d" (file-name-nondirectory file)
                          (line-number-at-pos (match-beginning 0)))
                  offenders)))))
    ;; Assert
    (should-not offenders)))

(ert-deftest agent-repl-test-every-automatic-frame-move-goes-through-the-minibuffer-hold ()
  "The background build, the roster's follow and a minted workspace's landing
each reach the frame through `agent-repl--when-minibuffer-free', so none of
them can take the user out of a minibuffer by a path of its own."
  (dolist (spec '(("workspace.el" . "agent-repl--call-in-background-workspace")
                  ("roster.el" . "agent-repl-roster-apply-current")
                  ("verbs.el" . "agent-repl-verbs--land")))
    (with-temp-buffer
      ;; Arrange
      (insert-file-contents (expand-file-name (car spec) agent-repl-test--module-dir))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (re-search-forward (concat "^(defun " (regexp-quote (cdr spec)) " "))
      (beginning-of-defun)
      ;; Act
      (let ((body (buffer-substring (point) (progn (end-of-defun) (point)))))
        ;; Assert
        (should (string-match-p "(agent-repl--when-minibuffer-free" body))))))

(ert-deftest agent-repl-test-ws-log-scope-is-the-one-perspective-scope-in-panels ()
  "panels.el's perspective-lifecycle paths share the one scope helper.
A hand-rolled `log-ws'-or-global scope there would drift from it."
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "panels.el" agent-repl-test-ws--lisp-dir))
                  (buffer-string))))
    (should-not (string-match-p "(if log-ws log-ws agent-repl--global-log-scope)" source))))

(ert-deftest agent-repl-test-host-take-ref-is-the-only-writer-of-the-ref ()
  "No source writes either home of the ref but `agent-repl-host-take-ref'."
  ;; Arrange
  (let ((pattern "(agent-repl--ws-put [^ ]+ :ref \\|(agent-repl-host--put [^ ]+ :ref ")
        (offenders nil))
    ;; Act
    (dolist (file (directory-files agent-repl-test--module-dir t "\\`[^.].*\\.el\\'"))
      (unless (string-prefix-p "test-" (file-name-nondirectory file))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (unless (and (string= (file-name-nondirectory file) "host.el")
                         (save-excursion
                           (beginning-of-defun)
                           (looking-at "(defun agent-repl-host-take-ref ")))
              (push (format "%s:%d" (file-name-nondirectory file) (line-number-at-pos))
                    offenders))))))
    ;; Assert
    (should-not offenders)))

;;; test-rules-views.el ends here
