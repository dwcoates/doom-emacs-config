;;; test-dir-watcher.el --- ERT tests for dir-watcher.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the generic watched-directory file intake.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-dir-watcher.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;; Forward declaration: the fixture accessor defuns below reference the
;; watcher defconst, which is defined after them.
(defvar agent-repl-dir-watcher-test--watcher)

;;;; ---- Fixture: a self-contained test channel ----

(defvar agent-repl-dir-watcher-test--dir nil
  "The fixture channel's watched directory (bound per test).")

(defvar agent-repl-dir-watcher-test--prefix "dwtest_")
(defvar agent-repl-dir-watcher-test--regexp "^dwtest_.*\\.json$")
(defvar agent-repl-dir-watcher-test--descriptor nil)
(defvar agent-repl-dir-watcher-test--processed nil
  "Files handed to the fixture processor, newest first.")

(defun agent-repl-dir-watcher-test--process (file)
  (push file agent-repl-dir-watcher-test--processed))

(defun agent-repl-dir-watcher-test--register ()
  (agent-repl--dir-watcher-register agent-repl-dir-watcher-test--watcher))

(defun agent-repl-dir-watcher-test--drain ()
  (agent-repl--dir-watcher-drain agent-repl-dir-watcher-test--watcher))

(defun agent-repl-dir-watcher-test--handler (event)
  (agent-repl--dir-watcher-handle-event
   agent-repl-dir-watcher-test--watcher event))

(defconst agent-repl-dir-watcher-test--watcher
  '(:label "dwtest"
    :dir-var agent-repl-dir-watcher-test--dir
    :prefix-var agent-repl-dir-watcher-test--prefix
    :regexp-var agent-repl-dir-watcher-test--regexp
    :descriptor-var agent-repl-dir-watcher-test--descriptor
    :process-fn agent-repl-dir-watcher-test--process
    :register-fn agent-repl-dir-watcher-test--register
    :drain-fn agent-repl-dir-watcher-test--drain
    :handler-fn agent-repl-dir-watcher-test--handler))

(defmacro agent-repl-dir-watcher-test--with-dir (&rest body)
  "Run BODY with a fresh temp dir bound as the fixture channel's dir."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "dir-watcher-test" t))
          (agent-repl-dir-watcher-test--dir (file-name-as-directory dir))
          (agent-repl-dir-watcher-test--descriptor nil)
          (agent-repl-dir-watcher-test--processed nil))
     (unwind-protect
         (progn ,@body)
       (delete-directory dir t))))

(defun agent-repl-dir-watcher-test--touch (name)
  "Create file NAME in the fixture dir; return its absolute path."
  (let ((path (expand-file-name name agent-repl-dir-watcher-test--dir)))
    (with-temp-file path (insert "{}"))
    path))

;;;; ---- Drain ----

(ert-deftest agent-repl-dir-watcher-test-drain-processes-and-counts ()
  "Drain processes every regexp-matching file and returns the count."
  (agent-repl-dir-watcher-test--with-dir
    (agent-repl-dir-watcher-test--touch "dwtest_a.json")
    (agent-repl-dir-watcher-test--touch "dwtest_b.json")
    (should (equal 2 (agent-repl-dir-watcher-test--drain)))
    (should (equal 2 (length agent-repl-dir-watcher-test--processed)))))

(ert-deftest agent-repl-dir-watcher-test-drain-ignores-non-matching ()
  "Drain leaves files outside the regexp untouched."
  (agent-repl-dir-watcher-test--with-dir
    (agent-repl-dir-watcher-test--touch "unrelated.json")
    (should (equal 0 (agent-repl-dir-watcher-test--drain)))
    (should-not agent-repl-dir-watcher-test--processed)))

(ert-deftest agent-repl-dir-watcher-test-drain-missing-dir-returns-zero ()
  "Drain on a nonexistent directory is a quiet zero, not an error."
  (let ((agent-repl-dir-watcher-test--dir "/nonexistent/dwtest/")
        (agent-repl-dir-watcher-test--processed nil))
    (should (equal 0 (agent-repl-dir-watcher-test--drain)))))

(ert-deftest agent-repl-dir-watcher-test-drain-resolves-dir-dynamically ()
  "A `let'-bound dir variable redirects the drain (symbols, not values)."
  (agent-repl-dir-watcher-test--with-dir
    (agent-repl-dir-watcher-test--touch "dwtest_a.json")
    (let ((agent-repl-dir-watcher-test--dir "/nonexistent/elsewhere/"))
      (should (equal 0 (agent-repl-dir-watcher-test--drain))))
    (should (equal 1 (agent-repl-dir-watcher-test--drain)))))

;;;; ---- Event dispatch ----

(ert-deftest agent-repl-dir-watcher-test-event-created-dispatches ()
  "A created event with the matching prefix reaches the processor."
  (agent-repl-dir-watcher-test--with-dir
    (let ((path (agent-repl-dir-watcher-test--touch "dwtest_a.json")))
      (agent-repl-dir-watcher-test--handler (list 'desc 'created path))
      (should (equal (list path) agent-repl-dir-watcher-test--processed)))))

(ert-deftest agent-repl-dir-watcher-test-event-renamed-uses-new-name ()
  "A renamed event dispatches on the NEW name (fourth element)."
  (agent-repl-dir-watcher-test--with-dir
    (let ((new (expand-file-name "dwtest_new.json"
                                 agent-repl-dir-watcher-test--dir)))
      (agent-repl-dir-watcher-test--handler
       (list 'desc 'renamed "/tmp/.dwtest_tmp" new))
      (should (equal (list new) agent-repl-dir-watcher-test--processed)))))

(ert-deftest agent-repl-dir-watcher-test-event-wrong-prefix-skipped ()
  "A created event without the prefix is ignored."
  (agent-repl-dir-watcher-test--with-dir
    (agent-repl-dir-watcher-test--handler
     (list 'desc 'created (expand-file-name "other.json"
                                            agent-repl-dir-watcher-test--dir)))
    (should-not agent-repl-dir-watcher-test--processed)))

(ert-deftest agent-repl-dir-watcher-test-event-nil-file-skipped ()
  "A changed event with a nil file is ignored rather than erroring."
  (agent-repl-dir-watcher-test--with-dir
    (agent-repl-dir-watcher-test--handler (list 'desc 'changed nil))
    (should-not agent-repl-dir-watcher-test--processed)))

;;;; ---- Re-arm ----

(ert-deftest agent-repl-dir-watcher-test-event-stopped-rearms-and-drains ()
  "A stopped event re-arms via the NAMED register fn and drains via the
NAMED drain fn, so `cl-letf' stubs of either are honored."
  (agent-repl-dir-watcher-test--with-dir
    (let ((rearmed nil) (drained nil))
      (cl-letf (((symbol-function 'agent-repl-dir-watcher-test--register)
                 (lambda () (setq rearmed t)))
                ((symbol-function 'agent-repl-dir-watcher-test--drain)
                 (lambda () (setq drained t) 0)))
        (agent-repl-dir-watcher-test--handler
         (list 'desc 'stopped agent-repl-dir-watcher-test--dir)))
      (should rearmed)
      (should drained))))

(ert-deftest agent-repl-dir-watcher-test-event-dir-deleted-rearms ()
  "A deleted event naming the watched directory itself re-arms."
  (agent-repl-dir-watcher-test--with-dir
    (let ((rearmed nil))
      (cl-letf (((symbol-function 'agent-repl-dir-watcher-test--register)
                 (lambda () (setq rearmed t)))
                ((symbol-function 'agent-repl-dir-watcher-test--drain)
                 (lambda () 0)))
        (agent-repl-dir-watcher-test--handler
         (list 'desc 'deleted (directory-file-name
                               agent-repl-dir-watcher-test--dir))))
      (should rearmed))))

(ert-deftest agent-repl-dir-watcher-test-event-file-deleted-no-rearm ()
  "A deleted event for an individual file (routine cleanup) does NOT re-arm."
  (agent-repl-dir-watcher-test--with-dir
    (let ((rearmed nil))
      (cl-letf (((symbol-function 'agent-repl-dir-watcher-test--register)
                 (lambda () (setq rearmed t))))
        (agent-repl-dir-watcher-test--handler
         (list 'desc 'deleted (expand-file-name
                               "dwtest_a.json"
                               agent-repl-dir-watcher-test--dir))))
      (should-not rearmed)
      (should-not agent-repl-dir-watcher-test--processed))))

;;;; ---- Register ----

(ert-deftest agent-repl-dir-watcher-test-register-creates-dir-and-sets-descriptor ()
  "Register creates the watched directory and stores the new descriptor."
  (agent-repl-dir-watcher-test--with-dir
    (let ((agent-repl-dir-watcher-test--dir
           (expand-file-name "nested/" agent-repl-dir-watcher-test--dir)))
      (cl-letf (((symbol-function 'file-notify-add-watch)
                 (lambda (_dir _flags _cb) 'fresh-descriptor)))
        (agent-repl-dir-watcher-test--register))
      (should (file-directory-p agent-repl-dir-watcher-test--dir))
      (should (eq 'fresh-descriptor agent-repl-dir-watcher-test--descriptor)))))

(ert-deftest agent-repl-dir-watcher-test-register-removes-existing-valid-watch ()
  "Register tears down a still-valid prior watch before adding the new one."
  (agent-repl-dir-watcher-test--with-dir
    (let ((agent-repl-dir-watcher-test--descriptor 'stale-descriptor)
          (removed nil))
      (cl-letf (((symbol-function 'file-notify-valid-p)
                 (lambda (_d) t))
                ((symbol-function 'file-notify-rm-watch)
                 (lambda (d) (setq removed d)))
                ((symbol-function 'file-notify-add-watch)
                 (lambda (_dir _flags _cb) 'fresh-descriptor)))
        (agent-repl-dir-watcher-test--register))
      (should (eq 'stale-descriptor removed))
      (should (eq 'fresh-descriptor agent-repl-dir-watcher-test--descriptor)))))

(ert-deftest agent-repl-dir-watcher-test-register-routes-events-to-named-handler ()
  "Events on the live watch route through the NAMED handler symbol, so a
stub installed after registration still intercepts."
  (agent-repl-dir-watcher-test--with-dir
    (let ((callback nil) (seen nil))
      (cl-letf (((symbol-function 'file-notify-add-watch)
                 (lambda (_dir _flags cb) (setq callback cb) 'descriptor)))
        (agent-repl-dir-watcher-test--register))
      (cl-letf (((symbol-function 'agent-repl-dir-watcher-test--handler)
                 (lambda (event) (setq seen event))))
        (funcall callback '(desc created "/tmp/dwtest_x.json")))
      (should (equal '(desc created "/tmp/dwtest_x.json") seen)))))

(provide 'test-dir-watcher)
;;; test-dir-watcher.el ends here
