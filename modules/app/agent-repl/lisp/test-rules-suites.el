;;; test-rules-suites.el --- Rule tests: the ERT suites themselves -*- lexical-binding: t; -*-

;;; Commentary:

;; Rules over the ERT suites' own sources (`test-*.el'): every suite goes
;; through the shared fixtures, recorders and readers of test-helpers.el and
;; test-integration-helpers.el rather than hand-rolling its own.
;;
;; A rule test reads the module's elisp sources as data and holds them to a
;; structural rule.  Rule tests are no one module's tests, so they live by
;; rule area in `test-rules-*.el', the `ert-rules' suite every branch runs
;; before its merge is admitted (lisp/AGENTS.md, "Rule tests").

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'url-util)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- integration-host.el ----

(ert-deftest agent-repl-itest-host-every-suite-reads-values-through-the-shared-reader ()
  "No integration suite reads the retired `arguments' list of a record's context."
  ;; Arrange
  (let ((dir agent-repl-test--module-dir)
        (hand-rolled nil))
    ;; Act
    (dolist (file (directory-files dir t "\\`test-integration-.*\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        ;; Spelled in two halves, so this test's own source never matches.
        (when (re-search-forward (concat "(alist-get '" "arguments") nil t)
          (push (file-name-nondirectory file) hand-rolled))))
    ;; Assert
    (should-not hand-rolled)))

;;;; ---- notifications.el ----

(ert-deftest agent-repl-test-notifications-no-test-hand-rolls-a-log-capture ()
  "Every log capture in this file goes through the recorder."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "test-notifications.el" agent-repl-test--module-dir))
    (should (= (how-many (concat "symbol-function '" "agent-repl--emit-\\(info\\|warn\\|error\\|log\\)\\_>")) 4))))

;;;; ---- status.el ----

(ert-deftest agent-repl-test-tabbar-install-tests-share-the-harness ()
  "Every installer test runs inside the shared install harness."
  ;; Arrange
  (let ((source (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "test-status.el"
                                     agent-repl-test--module-dir))
                  (buffer-string)))
        (start 0)
        (checked 0)
        (bare nil))
    ;; Act: each deftest whose body calls the installer must use the macro.
    (while (string-match "^(ert-deftest \\([^ ]+\\)" source start)
      (let* ((name (match-string 1 source))
             (beg (match-beginning 0))
             (end (or (string-match "^(ert-deftest " source (match-end 0))
                      (length source)))
             (body (substring source beg end)))
        (when (string-match-p "(agent-repl--install-fixed-height-tab-bar)" body)
          (setq checked (1+ checked))
          (unless (string-match-p "agent-repl-test-status--with-tabbar-install"
                                  body)
            (push name bare)))
        (setq start end)))
    ;; Assert: the scan saw the installer tests, and none is hand-rolled.
    (should (>= checked 4))
    (should-not bare)))

;;;; ---- test-helpers.el ----

(ert-deftest agent-repl-test-helpers-quit-deferral-sites-use-the-shared-macros ()
  "No suite hand-rolls the quit-deferral recipe beside the shared macros.
A hand-rolled site is the failure this asserts against: it is easy to
write one that forgets to disarm `quit-flag' inside the `inhibit-quit'
scope, and such a site passes while leaving a live quit for whichever
test runs next."
  ;; Arrange -- test-helpers.el is where the recipe is ALLOWED to appear.
  (let ((dir agent-repl-test--module-dir)
        (offenders nil))
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (member (file-name-nondirectory file)
                      '("test-helpers.el" "test-test-helpers.el" "test-rules-suites.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          ;; Act -- the recipe's own signature: binding `inhibit-quit' by hand
          ;; in a suite that also touches `quit-flag'.
          (when (and (save-excursion
                       (re-search-forward "(let ((inhibit-quit t))" nil t))
                     (save-excursion
                       (re-search-forward "\\_<quit-flag\\_>" nil t)))
            (push (file-name-nondirectory file) offenders)))))
    ;; Assert
    (should-not offenders)))

(ert-deftest agent-repl-test-helpers-popup-stubs-use-the-shared-macro ()
  "No suite but test-popup.el stubs `agent-repl-popup-show' by hand.
test-popup.el tests the popup module itself and records one level lower."
  ;; Arrange
  (let ((dir agent-repl-test--module-dir)
        (offenders nil))
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (member (file-name-nondirectory file)
                      '("test-helpers.el" "test-test-helpers.el" "test-rules-suites.el" "test-popup.el"))
        (with-temp-buffer
          (insert-file-contents file)
          ;; Act
          (when (search-forward "(symbol-function 'agent-repl-popup-show)" nil t)
            (push (file-name-nondirectory file) offenders)))))
    ;; Assert
    (should-not offenders)))

(ert-deftest agent-repl-test-helpers-subprocess-reaps-share-one-helper ()
  "Every subprocess reap in the shared harness goes through the group kill.
A site that hand-rolls `delete-process' on a subprocess orphans its children."
  ;; Arrange
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "test-integration-helpers.el" agent-repl-test--module-dir))
    ;; Act
    (let ((hand-rolled
           (cl-loop for name in '("agent-repl-daemon--build-process"
                                  "agent-repl--frontend-daemon-process")
                    when (progn (goto-char (point-min))
                                (search-forward (format "(delete-process %s)" name) nil t))
                    collect name))
          (helper (progn (goto-char (point-min))
                         (how-many (regexp-quote "(agent-repl-test--reap-process")))))
      ;; Assert
      (should-not hand-rolled)
      (should (= helper 3)))))

(ert-deftest agent-repl-test-helpers-no-suite-hand-rolls-a-temp-dir-fixture ()
  "Every temporary-directory fixture goes through `agent-repl-test--with-temp-dir'.
A hand-rolled one is a macro deleting its own bound directory, the
signature this looks for."
  ;; Arrange
  (let ((offenders nil))
    (dolist (file (directory-files agent-repl-test--module-dir t "\\`test-.*\\.el\\'"))
      (unless (member (file-name-nondirectory file) '("test-helpers.el" "test-test-helpers.el"))
        (with-temp-buffer
          (insert-file-contents file)
          ;; Act
          (when (re-search-forward "(delete-directory ,[^ ]+ t)" nil t)
            (push (file-name-nondirectory file) offenders)))))
    ;; Assert
    (should-not offenders)))

(ert-deftest agent-repl-test-helpers-no-source-scan-hand-rolls-its-reader ()
  "Every suite that reads module sources as data goes through the shared helpers.
A hand-rolled scan lists `lisp/' itself or loops `read' over a buffer, the
signatures this looks for."
  ;; Arrange
  (let ((offenders nil))
    (dolist (file (directory-files agent-repl-test--module-dir t "\\`test-.*\\.el\\'"))
      (unless (member (file-name-nondirectory file) '("test-helpers.el" "test-test-helpers.el"))
        (with-temp-buffer
          (insert-file-contents file)
          ;; Act
          (when (re-search-forward "(while t\\s-*(let ((form (read (current-buffer)))" nil t)
            (push (file-name-nondirectory file) offenders)))))
    ;; Assert
    (should-not offenders)))

(ert-deftest agent-repl-test-helpers-no-suite-hand-rolls-the-said-filter ()
  "Every suite that strips the log's echoes from recorded messages uses the one filter."
  ;; Arrange
  (let ((dir agent-repl-test--module-dir)
        (hand-rolled nil))
    ;; Act
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (member (file-name-nondirectory file) '("test-helpers.el" "test-test-helpers.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward "(string-match-p \"[^\"]*\\[agent-repl[^\"]*\" m)" nil t)
            (push (file-name-nondirectory file) hand-rolled)))))
    ;; Assert
    (should-not hand-rolled)))

(ert-deftest agent-repl-test-helpers-no-suite-hand-rolls-the-user-said-fixture ()
  "Every suite that builds a one-text-block UserSaid uses the one fixture."
  ;; Arrange
  (let ((dir agent-repl-test--module-dir)
        (hand-rolled nil))
    ;; Act
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (member (file-name-nondirectory file) '("test-helpers.el" "test-test-helpers.el" "test-rules-suites.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (when (search-forward "(list :content (list :blocks (list (list :arm :text :value (list :text text)))))" nil t)
            (push (file-name-nondirectory file) hand-rolled)))))
    ;; Assert
    (should-not hand-rolled)))

(ert-deftest agent-repl-test-test-helpers-composer-marked-is-every-kinds-reader ()
  "Every composer mark kind's suite reads its marks through the one reader."
  (dolist (file '("test-command-completion.el" "test-workspace-completion.el"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file agent-repl-test--module-dir))
      (should (search-forward "(agent-repl-test--composer-marked '" nil t))
      (goto-char (point-min))
      (should-not (search-forward "(overlays-in" nil t)))))

(ert-deftest agent-repl-test-helpers-no-suite-spells-the-render-colors-path-itself ()
  "Every suite reads render-colors.json through the one helper path."
  ;; Arrange
  (let ((spelled nil))
    ;; Act
    (dolist (file (directory-files agent-repl-test--module-dir t "\\`test-.*\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (when (re-search-forward "(expand-file-name \"[./]*proto/vocab/render-colors\\.json\"" nil t)
          (unless (equal (file-name-nondirectory file) "test-helpers.el")
            (push (file-name-nondirectory file) spelled)))))
    ;; Assert
    (should-not spelled)))

(ert-deftest agent-repl-test-helpers-no-suite-hand-rolls-the-fatal-stand-in ()
  "Every suite stubs `agent-repl--fatal' with `agent-repl-test--fatal-into'.
test-spell.el records each rung with its level, a different shape, and
stubs every rung alike."
  ;; Arrange
  (let ((dir agent-repl-test--module-dir)
        (hand-rolled nil))
    ;; Act
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (member (file-name-nondirectory file) '("test-helpers.el" "test-test-helpers.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward "(symbol-function 'agent-repl--emit-fatal)[ \t\n]*(lambda (_" nil t)
            (push (file-name-nondirectory file) hand-rolled)))))
    ;; Assert
    (should-not hand-rolled)))

(ert-deftest agent-repl-test-helpers-no-suite-hand-rolls-the-remapped-backgrounds ()
  "Every suite reads a buffer's remapped backgrounds through `agent-repl-test--remapped-backgrounds'."
  ;; Arrange
  (let ((dir agent-repl-test--module-dir)
        (hand-rolled nil))
    ;; Act
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (member (file-name-nondirectory file) '("test-helpers.el" "test-test-helpers.el" "test-rules-suites.el"))
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward "(plist-get spec :background)" nil t)
            (push (file-name-nondirectory file) hand-rolled)))))
    ;; Assert
    (should-not hand-rolled)))

(ert-deftest agent-repl-test-helpers-no-suite-hand-rolls-the-recorder ()
  "Every suite records a logging rung through `agent-repl-test--record-into'."
  ;; Arrange
  (let ((dir agent-repl-test--module-dir)
        (hand-rolled nil))
    ;; Act
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (equal (file-name-nondirectory file) "test-helpers.el")
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward
                 "(lambda (_[a-z]* fmt &rest args)[ \t\n]*(push (apply #'format fmt args) [^() \t\n]+))"
                 nil t)
            (push (file-name-nondirectory file) hand-rolled)))))
    ;; Assert
    (should-not hand-rolled)))

(ert-deftest agent-repl-test-helpers-no-suite-hand-rolls-the-settable-clock ()
  "Every suite steps `agent-repl--now' through `agent-repl-test--with-clock'."
  ;; Arrange
  (let ((dir agent-repl-test--module-dir)
        (hand-rolled nil))
    ;; Act
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (equal (file-name-nondirectory file) "test-helpers.el")
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward
                 "(symbol-function 'agent-repl--now)[ \t\n]*(lambda ()[ \t\n]*[a-z-]+)"
                 nil t)
            (push (file-name-nondirectory file) hand-rolled)))))
    ;; Assert
    (should-not hand-rolled)))

(ert-deftest agent-repl-test-helpers-no-suite-hand-rolls-the-leveled-recorder ()
  "Every suite records a rung with its level through `agent-repl-test--record-leveled-into'."
  ;; Arrange
  (let ((dir agent-repl-test--module-dir)
        (hand-rolled nil))
    ;; Act
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (equal (file-name-nondirectory file) "test-helpers.el")
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward
                 "(lambda (_[a-z]* fmt &rest args)[ \t\n]*(push (cons '[a-z]+ (apply #'format fmt args))"
                 nil t)
            (push (file-name-nondirectory file) hand-rolled)))))
    ;; Assert
    (should-not hand-rolled)))

(ert-deftest agent-repl-test-helpers-no-suite-hand-rolls-the-page-unloading-reporter ()
  "Every suite that answers a report three ways uses the one reporter."
  ;; Arrange
  (let ((dir agent-repl-test--module-dir)
        (hand-rolled nil))
    ;; Act
    (dolist (file (directory-files dir t "\\`test-.*\\.el\\'"))
      (unless (equal (file-name-nondirectory file) "test-helpers.el")
        (with-temp-buffer
          (insert-file-contents file)
          (while (re-search-forward
                  "(symbol-function 'agent-repl-host-report-page-unloading)[ \t\n]*(lambda (ws on-reported on-not-reported)"
                  nil t)
            (let ((end (save-excursion (up-list -1) (forward-sexp) (point))))
              (when (re-search-forward "'refused\\|'pending" end t)
                (push (file-name-nondirectory file) hand-rolled)))))))
    ;; Assert
    (should-not hand-rolled)))

;;; test-rules-suites.el ends here
