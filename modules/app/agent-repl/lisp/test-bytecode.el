;;; test-bytecode.el --- ERT tests for agent-repl bytecode.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run through the daemon: agent-repld test run -ert lisp/test-bytecode.el

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defvar agent-repl-test-bc--infos nil "Every info record, newest first.")
(defvar agent-repl-test-bc--errors nil "Every error record, newest first.")
(defvar agent-repl-test-bc--timers nil "Every timer key registered, newest first.")
(defvar agent-repl-test-bc--cancelled nil "Every timer key cancelled, newest first.")
(defvar agent-repl-test-bc--clock 0.0 "What the stubbed wall clock answers.")

(defmacro agent-repl-test-bc--with-root (var &rest body)
  "Bind VAR to a fresh temp module root for BODY, deleted afterwards."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "agent-repl-bc-root" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun agent-repl-test-bc--load-module (root module source)
  "Write SOURCE as MODULE's file under ROOT and load it as a reload does."
  (let ((file (agent-repl--elisp-module-file root module)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert ";;; -*- lexical-binding: t; -*-\n" source))
    (load file nil t t)
    file))

(defmacro agent-repl-test-bc--with (&rest body)
  "Run BODY with recorded logs and timers, a settable clock and no run."
  (declare (indent 0))
  `(let ((agent-repl-test-bc--infos nil)
         (agent-repl-test-bc--errors nil)
         (agent-repl-test-bc--timers nil)
         (agent-repl-test-bc--cancelled nil)
         (agent-repl-test-bc--clock 100.0)
         (agent-repl-bytecode--queue nil)
         (agent-repl-bytecode--run nil))
     (cl-letf (((symbol-function 'agent-repl--now) (lambda () agent-repl-test-bc--clock))
               ((symbol-function 'agent-repl--info)
                (agent-repl-test--record-into agent-repl-test-bc--infos))
               ((symbol-function 'agent-repl--error)
                (agent-repl-test--record-into agent-repl-test-bc--errors))
               ((symbol-function 'run-with-timer) (lambda (&rest _) (timer-create)))
               ((symbol-function 'agent-repl--register-timer)
                (lambda (key timer) (push key agent-repl-test-bc--timers) timer))
               ((symbol-function 'agent-repl--cancel-timer-key)
                (lambda (key) (push key agent-repl-test-bc--cancelled) t)))
       ,@body)))

(defconst agent-repl-test-bc--source
  (concat "(defun agent-repl-test-bc-one (x) (let ((y (1+ x))) (* y 2)))\n"
          "(defun agent-repl-test-bc-two (x) (concat \"two-\" x))\n"
          "(let ((shared 0))\n"
          "  (defun agent-repl-test-bc-bump () (setq shared (1+ shared))))\n")
  "A module defining two top-level functions and one closure over a `let'.")

;;;; ---- What a load defined ----

(ert-deftest agent-repl-test-bc-file-functions-lists-top-level-definitions-in-order ()
  "The interpreted top-level definitions of the file are listed in definition order."
  (agent-repl-test-bc--with-root root
    ;; Arrange
    (let ((file (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)))
      ;; Act / Assert
      (should (equal (plist-get (agent-repl-bytecode-file-functions file) :functions)
                     '(agent-repl-test-bc-one agent-repl-test-bc-two))))))

(ert-deftest agent-repl-test-bc-file-functions-counts-a-closure-and-leaves-it-out ()
  "A function that closed over a `let' is counted, never listed."
  (agent-repl-test-bc--with-root root
    ;; Arrange
    (let ((file (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)))
      ;; Act / Assert
      (should (= (plist-get (agent-repl-bytecode-file-functions file) :closures) 1)))))

(ert-deftest agent-repl-test-bc-file-functions-lists-a-definition-under-a-file-special ()
  "A definition whose environment only declares a variable special is listed."
  (agent-repl-test-bc--with-root root
    ;; Arrange
    (let ((file (agent-repl-test-bc--load-module
                 root "gamma"
                 (concat "(defvar agent-repl-test-bc--local-special)\n"
                         "(defun agent-repl-test-bc-four () 4)\n"))))
      ;; Act / Assert
      (should (equal (aref (symbol-function 'agent-repl-test-bc-four) 2)
                     '(agent-repl-test-bc--local-special t)))
      (should (equal (plist-get (agent-repl-bytecode-file-functions file) :functions)
                     '(agent-repl-test-bc-four))))))

(ert-deftest agent-repl-test-bc-file-functions-leaves-out-a-compiled-definition ()
  "A definition no longer interpreted is not listed."
  (agent-repl-test-bc--with-root root
    ;; Arrange
    (let ((file (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)))
      (byte-compile 'agent-repl-test-bc-one)
      ;; Act / Assert
      (should (equal (plist-get (agent-repl-bytecode-file-functions file) :functions)
                     '(agent-repl-test-bc-two))))))

(ert-deftest agent-repl-test-bc-file-functions-answers-nil-for-a-file-never-loaded ()
  "A file with no `load-history' entry answers nil."
  ;; Act / Assert
  (should-not (agent-repl-bytecode-file-functions "/nowhere/lisp/never.el")))

;;;; ---- Scheduling ----

(ert-deftest agent-repl-test-bc-schedule-queues-every-module-in-order ()
  "The queue holds every module's functions, module by module, with its file."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (let ((alpha (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source))
            (beta (agent-repl-test-bc--load-module
                   root "beta" "(defun agent-repl-test-bc-three () 3)\n")))
        ;; Act
        (agent-repl-bytecode-schedule root '("alpha" "beta"))
        ;; Assert
        (should (equal agent-repl-bytecode--queue
                       `((agent-repl-test-bc-one . ,alpha)
                         (agent-repl-test-bc-two . ,alpha)
                         (agent-repl-test-bc-three . ,beta))))))))

(ert-deftest agent-repl-test-bc-schedule-arms-the-compile-timer ()
  "Scheduling arms the slice timer under `:bytecode'."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)
      ;; Act
      (agent-repl-bytecode-schedule root '("alpha"))
      ;; Assert
      (should (equal agent-repl-test-bc--timers '(:bytecode))))))

(ert-deftest agent-repl-test-bc-schedule-records-what-it-queued ()
  "The scheduled record counts the functions, files and closures."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)
      ;; Act
      (agent-repl-bytecode-schedule root '("alpha"))
      ;; Assert
      (should (equal agent-repl-test-bc--infos
                     '("elisp.bytecode.scheduled functions=2 files=1 closures=1"))))))

(ert-deftest agent-repl-test-bc-schedule-errors-on-a-module-never-loaded ()
  "A module with no `load-history' entry is an ERROR naming it, and queues nothing."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Act
      (agent-repl-bytecode-schedule root '("ghost"))
      ;; Assert
      (should (equal agent-repl-test-bc--errors
                     (list (format "elisp.bytecode.module-not-loaded module=ghost file=%S"
                                   (agent-repl--elisp-module-file root "ghost")))))
      (should-not agent-repl-bytecode--queue))))

(ert-deftest agent-repl-test-bc-schedule-replaces-a-run-under-way ()
  "A later load's schedule drops what the earlier run had left."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (let ((beta (agent-repl-test-bc--load-module
                   root "beta" "(defun agent-repl-test-bc-three () 3)\n")))
        (setq agent-repl-bytecode--queue '((agent-repl-test-bc-stale . "/old.el")))
        ;; Act
        (agent-repl-bytecode-schedule root '("beta"))
        ;; Assert
        (should (equal agent-repl-bytecode--queue `((agent-repl-test-bc-three . ,beta))))))))

;;;; ---- The slices ----

(ert-deftest agent-repl-test-bc-slice-installs-byte-code ()
  "A slice with time enough compiles every queued function to byte-code."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)
      (agent-repl-bytecode-schedule root '("alpha"))
      ;; Act
      (agent-repl-bytecode--slice)
      ;; Assert
      (should (byte-code-function-p (symbol-function 'agent-repl-test-bc-one)))
      (should (byte-code-function-p (symbol-function 'agent-repl-test-bc-two))))))

(ert-deftest agent-repl-test-bc-slice-keeps-what-a-function-answers ()
  "A compiled function answers what its interpreted definition answered."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)
      (agent-repl-bytecode-schedule root '("alpha"))
      ;; Act
      (agent-repl-bytecode--slice)
      ;; Assert
      (should (equal (list (agent-repl-test-bc-one 4) (agent-repl-test-bc-two "x"))
                     '(10 "two-x"))))))

(ert-deftest agent-repl-test-bc-slice-leaves-a-closure-interpreted ()
  "The closure over a `let' keeps its interpreted definition and its binding."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)
      (agent-repl-bytecode-schedule root '("alpha"))
      ;; Act
      (agent-repl-bytecode--slice)
      ;; Assert
      (should (interpreted-function-p (symbol-function 'agent-repl-test-bc-bump)))
      (should (= (agent-repl-test-bc-bump) 1)))))

(ert-deftest agent-repl-test-bc-slice-yields-at-its-deadline ()
  "A slice stops after the function that crosses its time, leaving the rest queued."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (let ((alpha (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)))
        (agent-repl-bytecode-schedule root '("alpha"))
        (advice-add 'agent-repl-bytecode--compile :after
                    (lambda (&rest _) (setq agent-repl-test-bc--clock
                                            (+ agent-repl-test-bc--clock 1.0)))
                    '((name . agent-repl-test-bc-tick)))
        (unwind-protect
            ;; Act
            (agent-repl-bytecode--slice)
          (advice-remove 'agent-repl-bytecode--compile 'agent-repl-test-bc-tick))
        ;; Assert
        (should (equal agent-repl-bytecode--queue `((agent-repl-test-bc-two . ,alpha))))
        (should-not agent-repl-test-bc--cancelled)))))

(ert-deftest agent-repl-test-bc-slice-interrupted-by-input-keeps-the-function-queued ()
  "Input arriving mid-compile leaves that function at the head of the queue."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (let ((alpha (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)))
        (agent-repl-bytecode-schedule root '("alpha"))
        (cl-letf (((symbol-function 'agent-repl-bytecode--compile)
                   (lambda (&rest _) (throw throw-on-input t))))
          ;; Act
          (agent-repl-bytecode--slice))
        ;; Assert
        (should (equal (car agent-repl-bytecode--queue) `(agent-repl-test-bc-one . ,alpha)))
        (should (= (plist-get agent-repl-bytecode--run :interrupted) 1))))))

(ert-deftest agent-repl-test-bc-last-slice-cancels-the-timer ()
  "The slice that empties the queue cancels the `:bytecode' timer."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)
      (agent-repl-bytecode-schedule root '("alpha"))
      ;; Act
      (agent-repl-bytecode--slice)
      ;; Assert
      (should (equal agent-repl-test-bc--cancelled '(:bytecode)))
      (should-not agent-repl-bytecode--run))))

(ert-deftest agent-repl-test-bc-last-slice-records-the-run ()
  "The run's end is recorded with what it compiled and how long it took."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)
      (agent-repl-bytecode-schedule root '("alpha"))
      ;; Act
      (agent-repl-bytecode--slice)
      ;; Assert
      (should (equal (car agent-repl-test-bc--infos)
                     "elisp.bytecode.compiled functions=2 failed=0 closures=1 files=1 slices=1 interrupted=0 cpu-ms=0 wall-ms=0")))))

;;;; ---- One function's compile ----

(ert-deftest agent-repl-test-bc-compile-failure-is-an-error-naming-the-function ()
  "A compile that signals is an ERROR naming the function, its file and the error."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (let ((alpha (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)))
        (cl-letf (((symbol-function 'byte-compile) (lambda (_) (error "Boom"))))
          ;; Act
          (should (eq (agent-repl-bytecode--compile 'agent-repl-test-bc-one alpha) 'failed)))
        ;; Assert
        (should (equal agent-repl-test-bc--errors
                       (list (format "elisp.bytecode.compile-failed function=agent-repl-test-bc-one file=%S error=(error \"Boom\")"
                                     alpha))))))))

(ert-deftest agent-repl-test-bc-compile-failure-leaves-the-definition-working ()
  "A failed compile leaves the interpreted definition installed."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (let ((alpha (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)))
        (cl-letf (((symbol-function 'byte-compile) (lambda (_) (error "Boom"))))
          ;; Act
          (agent-repl-bytecode--compile 'agent-repl-test-bc-one alpha))
        ;; Assert
        (should (interpreted-function-p (symbol-function 'agent-repl-test-bc-one)))
        (should (= (agent-repl-test-bc-one 4) 10))))))

(ert-deftest agent-repl-test-bc-compile-that-installs-nothing-is-an-error ()
  "A compile that leaves no byte-code installed is an ERROR, not a success."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (let ((alpha (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)))
        (cl-letf (((symbol-function 'byte-compile) #'ignore))
          ;; Act
          (should (eq (agent-repl-bytecode--compile 'agent-repl-test-bc-one alpha) 'failed)))
        ;; Assert
        (should (equal agent-repl-test-bc--errors
                       (list (format "elisp.bytecode.compile-failed function=agent-repl-test-bc-one file=%S error=the compile installed no byte-code"
                                     alpha))))))))

(ert-deftest agent-repl-test-bc-compile-skips-a-definition-replaced-since ()
  "A function redefined since it was queued is skipped, not compiled or failed."
  (agent-repl-test-bc--with
    ;; Arrange
    (defalias 'agent-repl-test-bc-replaced (byte-compile (lambda () 1)))
    ;; Act / Assert
    (should (eq (agent-repl-bytecode--compile 'agent-repl-test-bc-replaced "/x.el") 'skipped))
    (should-not agent-repl-test-bc--errors)))

(ert-deftest agent-repl-test-bc-a-failed-compile-is-counted-and-the-run-goes-on ()
  "A failed function counts as failed and the rest of the queue still compiles."
  (agent-repl-test-bc--with
    (agent-repl-test-bc--with-root root
      ;; Arrange
      (agent-repl-test-bc--load-module root "alpha" agent-repl-test-bc--source)
      (agent-repl-bytecode-schedule root '("alpha"))
      (let ((real (symbol-function 'byte-compile)))
        (cl-letf (((symbol-function 'byte-compile)
                   (lambda (form) (if (eq form 'agent-repl-test-bc-one) (error "Boom")
                                    (funcall real form)))))
          ;; Act
          (agent-repl-bytecode--slice)))
      ;; Assert
      (should (byte-code-function-p (symbol-function 'agent-repl-test-bc-two)))
      (should (string-match-p "functions=1 failed=1 " (car agent-repl-test-bc--infos))))))

(provide 'test-bytecode)

;;; test-bytecode.el ends here
