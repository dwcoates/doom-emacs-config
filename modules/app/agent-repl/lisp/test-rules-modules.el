;;; test-rules-modules.el --- Rule tests: module-wide structure -*- lexical-binding: t; -*-

;;; Commentary:

;; Module-wide structural rules: what config.el and doctor.el load and in what
;; order, the one async runner, executable lookup, vocabulary reader, clock,
;; plural, serializer and keymap shapes, and every required timer's owner.
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

;;;; ---- config.el ----

(ert-deftest agent-repl-config-test-load-resets-the-record ()
  "Every load of config.el starts the record over, like its load errors.
Read from the source rather than by re-loading config.el here: a real load
re-loads every module, which this suite has no business doing again.  The
reset must be a top-level `setq' that precedes the first module load."
  ;; Arrange
  (let ((forms (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "config.el" agent-repl-test--module-root))
                 (let (acc)
                   (condition-case nil
                       (while t (push (read (current-buffer)) acc))
                     (end-of-file nil))
                   (nreverse acc)))))
    ;; Act
    (let ((reset (cl-position '(setq agent-repl--elisp-module-builds nil) forms
                              :test #'equal))
          (first-load (cl-position-if
                       (lambda (form) (eq (car-safe form) 'agent-repl--load-module))
                       forms)))
      ;; Assert
      (should (and reset first-load (< reset first-load))))))

(defun agent-repl-config-test--top-level-forms ()
  "Every top-level form of config.el, in order."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "config.el" agent-repl-test--module-root))
    (let (acc)
      (condition-case nil
          (while t (push (read (current-buffer)) acc))
        (end-of-file nil))
      (nreverse acc))))

(ert-deftest agent-repl-config-test-lifecycle-is-registered-before-load-errors-signal ()
  "config.el registers the lifecycle hooks BEFORE it reports the load errors.
Regression, 2026-10-25: the report signalled first, and a cosmetic module
failing its load left Emacs with no daemon at all.  Read from the source:
a real load of config.el re-loads every module."
  ;; Arrange
  (let ((forms (agent-repl-config-test--top-level-forms)))
    ;; Act
    (let ((register (cl-position '(agent-repl--register-lifecycle-hooks noninteractive)
                                 forms :test #'equal))
          (report (cl-position-if
                   (lambda (form)
                     (and (eq (car-safe form) 'if)
                          (equal (nth 1 form) 'agent-repl--load-errors)))
                   forms))
          (last-load (cl-position-if
                      (lambda (form) (eq (car-safe form) 'agent-repl--load-module))
                      forms :from-end t)))
      ;; Assert
      (should (and register report last-load (< last-load register report))))))

(ert-deftest agent-repl-config-test-loads-verbs ()
  "verbs.el is in the load list: without it every workspace verb is unbound."
  (should (string-match-p "(agent-repl--load-module \"verbs\")"
                          (with-temp-buffer
                            (insert-file-contents
                             (expand-file-name "config.el"
                                               agent-repl-test--module-root))
                            (buffer-string)))))

(ert-deftest agent-repl-config-test-loads-no-deleted-module ()
  "config.el names no module this branch deleted.
A load of a missing module file is a load ERROR, which fails the whole
module, so this is the cheapest place to catch a stale load line."
  (let ((text (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "config.el" agent-repl-test--module-root))
                (buffer-string))))
    (dolist (name '("merge-handlers" "workspace-create-client"))
      (should-not (string-match-p (format "(agent-repl--load-module \"%s\")" name)
                                  text)))))

(ert-deftest agent-repl-config-test-doctor-aggregates-the-requirements-provider ()
  "The aggregation names the requirements provider, so doom doctor runs it."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "doctor.el" agent-repl-test--module-root))
    (should (search-forward "agent-repl--doctor-requirements-issues\n" nil t))))

(ert-deftest agent-repl-config-test-doctor-aggregates-the-spotlight-provider ()
  "The aggregation names the Spotlight provider, so doom doctor runs it."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "doctor.el" agent-repl-test--module-root))
    (should (search-forward "agent-repl--doctor-spotlight-issues\n" nil t))))

;;;; ---- core.el ----

(defconst agent-repl-test-core--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory this suite and the sources it reads live in.")

(ert-deftest agent-repl-test-write-file-atomically-is-the-shared-write-path ()
  "Every atomic writer goes through the shared helper, never a hand-rolled rename."
  ;; Arrange.
  (let ((sources (mapcar (lambda (name)
                           (with-temp-buffer
                             (insert-file-contents
                              (expand-file-name name agent-repl-test-core--lisp-dir))
                             (buffer-string)))
                         '("history.el" "held-ingress.el" "clipboard-image.el"))))
    ;; Assert.
    (dolist (source sources)
      (should (string-match-p "(agent-repl--write-file-atomically" source))
      (should-not (string-match-p "(rename-file" source)))))

(ert-deftest agent-repl-test-async-command-is-the-only-async-start-process ()
  "Only the async runners call `start-process', so a new spawn reuses them."
  ;; Arrange
  (let ((offenders nil))
    (dolist (file (agent-repl-test--production-lisp-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Act
        (while (re-search-forward "#'start-process" nil t)
          (let ((defun-name (save-excursion
                              (beginning-of-defun)
                              (and (looking-at "(defun \\(\\_<.+?\\_>\\)")
                                   (match-string 1)))))
            (unless (member defun-name '("agent-repl--async-command"
                                         "agent-repl--async-git"))
              (push (cons (file-name-nondirectory file) defun-name) offenders))))))
    ;; Assert
    (should (equal offenders nil))))

(ert-deftest agent-repl-test-async-command-is-the-only-stdin-feed ()
  "Only `agent-repl--async-command' closes a program's stdin, so a fed spawn reuses it."
  ;; Arrange
  (let ((offenders nil))
    (dolist (file (agent-repl-test--production-lisp-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Act
        (while (re-search-forward "(process-send-eof\\_>" nil t)
          (let ((defun-name (save-excursion
                              (beginning-of-defun)
                              (and (looking-at "(defun \\(\\_<.+?\\_>\\)")
                                   (match-string 1)))))
            (unless (equal defun-name "agent-repl--async-command")
              (push (cons (file-name-nondirectory file) defun-name) offenders))))))
    ;; Assert
    (should (equal offenders nil))))

(ert-deftest agent-repl-test-every-required-timer-key-names-a-loadable-owner ()
  "A required key whose owner file does not exist can never be satisfied.
`:readiness-poll' and `:workspace-status-export' outlived readiness.el
and workspace-status-export.el, so every cold start warned
`outcome=unavailable reason=owner-not-loaded' twice, a second after the
module loaded, about jobs nothing owns any more."
  ;; Arrange: the arm functions the production sources actually define.
  (let ((defined nil))
    (dolist (file (agent-repl-test--production-lisp-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(defun \\([^ \t\n()]+\\)" nil t)
          (push (intern (match-string 1)) defined))))
    ;; Act / Assert: every required key's owner is one of them.  Read off
    ;; the SOURCES, never `fboundp', because the suite's own process has
    ;; loaded files a cold start would not have reached yet.
    (dolist (entry agent-repl--required-timer-keys)
      (should (memq (cdr entry) defined)))))

(defun agent-repl-test-core--defvar-keymaps-built-inline ()
  "Return (FILE . VAR) for each top-level keymap `defvar' in lisp/ built inline.
A `-map' `defvar' whose initializer is anything but `(make-sparse-keymap)'
builds its bindings once per Emacs, so a reload never updates it."
  (let (found)
    (dolist (file (agent-repl-test--module-sources))
      (dolist (form (agent-repl-test--file-forms file))
        (when (and (eq (car-safe form) 'defvar)
                   (string-suffix-p "-map" (symbol-name (cadr form)))
                   (cddr form)
                   (not (equal (nth 2 form) '(make-sparse-keymap))))
          (push (cons (file-name-nondirectory file) (cadr form)) found))))
    found))

(ert-deftest agent-repl-test-refill-keymap-every-module-keymap-starts-empty ()
  "Every module keymap `defvar' starts empty and is filled by a reloadable form.
Shared shape with `agent-repl-refill-keymap': a keymap built inside its
`defvar' silently keeps the first load's bindings across every reload."
  (should-not (agent-repl-test-core--defvar-keymaps-built-inline)))

(ert-deftest agent-repl-test-core-every-registered-boundary-is-defined ()
  "Every external-boundary registry entry names a function some source defines.
A stale entry guards nothing and hides that the wrapper it named is gone."
  ;; Arrange
  (let ((sources (append (agent-repl-test--production-lisp-files)
                         (list (expand-file-name "../config.el" agent-repl-test--module-dir))))
        (defined nil))
    (dolist (file sources)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(defun \\([^ \n]+\\) " nil t)
          (push (intern (match-string 1)) defined))))
    ;; Act
    (let ((stale (seq-remove (lambda (name) (memq name defined))
                             agent-repl--external-boundary-functions)))
      ;; Assert
      (should (equal stale nil)))))

(ert-deftest agent-repl-test-core-no-module-looks-up-path-itself ()
  "Only core.el's wrapper calls `executable-find'; every module calls the wrapper.
A module with a PATH lookup of its own is a boundary the harness cannot guard."
  ;; Arrange
  (let ((offenders nil))
    (dolist (file (agent-repl-test--production-lisp-files))
      (unless (equal (file-name-nondirectory file) "core.el")
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          ;; Act
          (when (re-search-forward "(executable-find " nil t)
            (push (file-name-nondirectory file) offenders)))))
    ;; Assert
    (should (equal offenders nil))))

(ert-deftest agent-repl-test-core-every-vocab-file-is-read-through-the-reader ()
  "No source outside core.el resolves a proto/vocab path by hand."
  ;; Arrange
  (let* ((dir (file-name-directory (symbol-file 'agent-repl-vocab-read)))
         (sources (seq-remove (lambda (f) (string-prefix-p "test-" (file-name-nondirectory f)))
                              (directory-files dir t "\\.el\\'"))))
    ;; Act
    (let ((hand-rolled
           (seq-filter (lambda (f)
                         (and (not (equal (file-name-nondirectory f) "core.el"))
                              (with-temp-buffer
                                (insert-file-contents f)
                                (re-search-forward "\"\\.\\./proto/vocab/" nil t))))
                       sources)))
      ;; Assert
      (should-not hand-rolled))))

(ert-deftest agent-repl-test-core-no-source-hand-rolls-the-plural ()
  "No source file spells the singular-or-s rule itself: every count's noun
goes through `agent-repl-plural-noun'."
  ;; Arrange.
  (let* ((dir agent-repl-test-core--lisp-dir)
         (sources (seq-remove (lambda (f) (string-prefix-p "test-" (file-name-nondirectory f)))
                              (directory-files dir t "\\.el\\'")))
         (hand-rolled (rx " 1) \"\" \"s\")")))
    ;; Act.
    (let ((offenders (seq-filter (lambda (f)
                                   (with-temp-buffer
                                     (insert-file-contents f)
                                     (re-search-forward hand-rolled nil t)))
                                 sources)))
      ;; Assert.
      (should (null (mapcar #'file-name-nondirectory offenders))))))

(ert-deftest agent-repl-test-core-display-width-is-the-tab-bars-measure ()
  "The tab bar's packer measures its entries through `agent-repl-display-width'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "status.el" agent-repl-test-core--lisp-dir))
    (goto-char (point-min))
    (emacs-lisp-mode)
    (should (search-forward "(defun agent-repl--tabline-entry-width (entry)" nil t))
    (goto-char (match-beginning 0))
    (should (search-forward "(agent-repl-display-width entry)"
                            (save-excursion (forward-sexp 1) (point)) t))))

(ert-deftest agent-repl-test-core-annotation-column-callers-share-the-measure ()
  "Every completion list aligning a column past its names measures it through
the helper, never a hand-rolled widest-name measure."
  (dolist (file '("commands.el" "workspace-completion.el"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file agent-repl-test-core--lisp-dir))
      (should (search-forward "(agent-repl-annotation-column " nil t))
      (goto-char (point-min))
      (should-not (re-search-forward "(apply #'max 0 (mapcar (lambda ([a-z]+) (string-width" nil t)))))

(ert-deftest agent-repl-test-core-no-source-wraps-the-wall-clock-by-hand ()
  "No source but core.el defines a function whose body is `(float-time)'.
A measurement reads its clock through `agent-repl--now', so a test steps
every measurement's clock the same way."
  (let (offenders)
    (dolist (file (agent-repl-test--production-lisp-files))
      (unless (equal (file-name-nondirectory file) "core.el")
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward "(defun [^ ]+ ()\n\\(?:  \".*\n\\(?:.*\n\\)*?\\)?  (float-time))" nil t)
            (push (file-name-nondirectory file) offenders)))))
    (should-not offenders)))

(ert-deftest agent-repl-test-core-every-unix-ms-instant-reads-the-one-helper ()
  "No source but core.el converts the wall clock to milliseconds by hand.
Every instant Emacs stamps or compares in Unix milliseconds goes through
`agent-repl--now-unix-ms', so none can drift from the clock the daemon and
the page read."
  (let (offenders)
    (dolist (file (agent-repl-test--production-lisp-files))
      (unless (equal (file-name-nondirectory file) "core.el")
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward "(\\* 1000 (float-time))" nil t)
            (push (file-name-nondirectory file) offenders)))))
    (should-not offenders)))

(ert-deftest agent-repl-test-core-every-serialized-report-goes-through-the-serializer ()
  "The focus report and the composer's entries report run through `agent-repl-serial-send'."
  (let ((dir (file-name-directory (symbol-file 'agent-repl-serial-send 'defun))))
    (dolist (file '("notifications.el" "prompt-entries.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file dir))
        (should (save-excursion (search-forward "(agent-repl-serial-send" nil t)))
        (should-not (re-search-forward "in-flight nil$" nil t))))))

(ert-deftest agent-repl-test-core-ivy-tuned-reads-share-the-helper ()
  "Every read that tunes an ivy alist goes through the one helper.
A hand-rolled `let' of an ivy alist around a read would drift from it."
  (let ((dir (file-name-directory (symbol-file 'agent-repl--completing-read-under-ivy 'defun))))
    (dolist (file '("commands.el" "input.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file dir))
        (should (save-excursion (search-forward "(agent-repl--completing-read-under-ivy" nil t)))
        (should-not (re-search-forward "(let\\*? ((ivy-[a-z-]+-alist" nil t))))))

;;; test-rules-modules.el ends here
