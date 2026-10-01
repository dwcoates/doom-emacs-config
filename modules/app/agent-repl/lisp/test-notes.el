;;; test-notes.el --- ERT tests for notes.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the per-workspace org notes file that survived tasks.el.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-notes.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(defmacro agent-repl-test-notes--with-dir (&rest body)
  "Run BODY with `agent-repl--notes-dir' pointing at a fresh temp directory.
The directory is removed afterwards, so no test observes another's files."
  (declare (indent 0))
  `(let ((dir (file-name-as-directory
               (make-temp-file "agent-repl-notes-test" t))))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-repl--notes-dir) (lambda () dir)))
           ,@body)
       (delete-directory dir t))))

;;;; ---- Tests: agent-repl--notes-dir ------------------------------------

(ert-deftest agent-repl-test-notes-dir-under-state-root ()
  "The notes directory resolves through the global state-file resolver."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--global-state-file)
               (lambda (relative) (concat "/tmp/state-root/" relative))))
      (should (equal (agent-repl--notes-dir) "/tmp/state-root/notes/")))))

;;;; ---- Tests: agent-repl--notes-file -----------------------------------

(ert-deftest agent-repl-test-notes-file-is-workspace-named ()
  "A workspace's notes file is `<notes dir>/<workspace name>.org'."
  (agent-repl-test--with-clean-state
    (agent-repl-test-notes--with-dir
      (should (equal (agent-repl--notes-file "alpha")
                     (expand-file-name "alpha.org" (agent-repl--notes-dir)))))))

(ert-deftest agent-repl-test-notes-file-rejects-nil-workspace ()
  "A nil workspace is refused rather than silently defaulted."
  (agent-repl-test--with-clean-state
    (agent-repl-test-notes--with-dir
      (should-error (agent-repl--notes-file nil) :type 'error))))

(ert-deftest agent-repl-test-notes-file-rejects-empty-workspace ()
  "An empty-string workspace is refused rather than silently defaulted."
  (agent-repl-test--with-clean-state
    (agent-repl-test-notes--with-dir
      (should-error (agent-repl--notes-file "") :type 'error))))

;;;; ---- Tests: agent-repl--notes-ensure ---------------------------------

(ert-deftest agent-repl-test-notes-ensure-creates-file ()
  "The notes file is created on first ensure."
  (agent-repl-test--with-clean-state
    (agent-repl-test-notes--with-dir
      (let ((file (agent-repl--notes-ensure "alpha")))
        (should (file-exists-p file))))))

(ert-deftest agent-repl-test-notes-ensure-seeds-title-header ()
  "A freshly created notes file carries an org TITLE naming the workspace."
  (agent-repl-test--with-clean-state
    (agent-repl-test-notes--with-dir
      (let ((file (agent-repl--notes-ensure "alpha")))
        (should (string-match-p "^#\\+TITLE: alpha$"
                                (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string))))))))

(ert-deftest agent-repl-test-notes-ensure-preserves-existing-content ()
  "Ensure is idempotent: an existing notes file is left untouched."
  (agent-repl-test--with-clean-state
    (agent-repl-test-notes--with-dir
      (let ((file (agent-repl--notes-file "alpha")))
        (make-directory (file-name-directory file) t)
        (with-temp-file file (insert "hand written\n"))
        (agent-repl--notes-ensure "alpha")
        (should (equal (with-temp-buffer (insert-file-contents file)
                                         (buffer-string))
                       "hand written\n"))))))

(ert-deftest agent-repl-test-notes-ensure-creates-missing-directory ()
  "Ensure creates the notes directory when it does not exist yet."
  (agent-repl-test--with-clean-state
    (agent-repl-test-notes--with-dir
      (let* ((sub (expand-file-name "deeper/" (agent-repl--notes-dir))))
        (cl-letf (((symbol-function 'agent-repl--notes-dir) (lambda () sub)))
          (agent-repl--notes-ensure "alpha")
          (should (file-directory-p sub)))))))

;;;; ---- Tests: agent-repl--notes-install-save-on-kill -------------------

(ert-deftest agent-repl-test-notes-save-on-kill-saves-modified-buffer ()
  "Killing a modified notes buffer saves it through the autosave helper."
  (agent-repl-test--with-clean-state
    (let ((saved nil))
      (cl-letf (((symbol-function 'agent-repl--save-buffer-if-modified)
                 (lambda (buf &rest _) (setq saved (buffer-name buf)) t)))
        (let ((buf (generate-new-buffer "notes-under-test")))
          (agent-repl--notes-install-save-on-kill buf "alpha")
          (kill-buffer buf))
        (should (equal saved "notes-under-test"))))))

(ert-deftest agent-repl-test-notes-save-on-kill-carries-workspace-to-autosave ()
  "The installed hook preserves its workspace attribution until buffer death."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (let ((saved-workspace nil))
      (cl-letf (((symbol-function 'agent-repl--save-buffer-if-modified)
                 (lambda (_buf &optional ws _aggregate-p)
                   (setq saved-workspace ws)
                   t)))
        (let ((buf (generate-new-buffer "notes-workspace-under-test")))
          (agent-repl--notes-install-save-on-kill buf "alpha")
          ;; Act.
          (kill-buffer buf))
        ;; Assert.
        (should (equal saved-workspace "alpha"))))))

;;;; ---- Tests: agent-repl-notes-open ------------------------------------

(ert-deftest agent-repl-test-notes-open-visits-current-workspace-file ()
  "Open visits the notes file of the CURRENT workspace."
  (agent-repl-test--with-clean-state
    (agent-repl-test-notes--with-dir
      (let ((visited nil))
        (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "alpha"))
                  ;; The ONE shared editor-popup subroutine is what notes
                  ;; opens through; a local `find-file' here would be the
                  ;; divergence the shared-subroutine rule forbids.
                  ((symbol-function 'agent-repl-popup-open)
                   (lambda (file &optional _line)
                     (setq visited file)
                     (find-file-noselect file))))
          (agent-repl-notes-open)
          (should (equal visited
                         (expand-file-name "alpha.org" (agent-repl--notes-dir)))))))))

(ert-deftest agent-repl-test-notes-open-seeds-the-file ()
  "Open seeds the notes file before visiting it."
  (agent-repl-test--with-clean-state
    (agent-repl-test-notes--with-dir
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "alpha"))
                ((symbol-function 'find-file) (lambda (&rest _) nil)))
        (agent-repl-notes-open)
        (should (file-exists-p (agent-repl--notes-file "alpha")))))))

(ert-deftest agent-repl-test-notes-open-rejects-absent-workspace ()
  "Open refuses when there is no current workspace to key the notes by."
  (agent-repl-test--with-clean-state
    (agent-repl-test-notes--with-dir
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () nil))
                ((symbol-function 'find-file) (lambda (&rest _) nil)))
        (should-error (agent-repl-notes-open) :type 'user-error)))))

(ert-deftest agent-repl-test-notes-open-is-interactive ()
  "`agent-repl-notes-open' is a user-facing command."
  (should (commandp 'agent-repl-notes-open)))

(provide 'test-notes)
;;; test-notes.el ends here
