;;; test-prompt-entries.el --- ERT tests for prompt-entries.el -*- lexical-binding: t; -*-

;;; Commentary:

;; WHAT THIS GUARDS: that the prompt grammar is built once however many
;; composers wait for it, and only when its sources changed since the last
;; build; that a failed build is logged at ERROR with the compiler's words
;; and calls nothing waiting; and that the composer marks exactly the
;; headings the daemon reads as entries (a heading with a marker is a
;; pasted tree's root, never an entry).  The compiler's spawn
;; (`agent-repl--async-command') and `treesit' are stubbed, so no grammar is
;; built and none is loaded.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Fixtures ----

(defmacro agent-repl-test-prompt-entries--with-grammar (&rest body)
  "Run BODY with a throwaway grammar checkout and library dir.
`agent-repl--frontend-root' holds the grammar's two sources, the library
dir is empty, and no build is in flight or waited on."
  (declare (indent 0))
  `(let* ((root (make-temp-file "agent-repl-prompt-entries-" t))
          (agent-repl--frontend-root root)
          (agent-repl-prompt-entries--library-dir (expand-file-name "lib" root))
          (agent-repl-prompt-entries--building nil)
          (agent-repl-prompt-entries--waiting nil)
          (treesit-extra-load-path nil))
     (make-directory (agent-repl-prompt-entries--grammar-dir) t)
     (dolist (name agent-repl-prompt-entries--sources)
       (with-temp-file (expand-file-name name (agent-repl-prompt-entries--grammar-dir))
         (insert "/* " name " */\n")))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun agent-repl-test-prompt-entries--built ()
  "Leave a library built from the sources as they stand, as a build would."
  (make-directory agent-repl-prompt-entries--library-dir t)
  (with-temp-file (agent-repl-prompt-entries--library) (insert "lib"))
  (with-temp-file (agent-repl-prompt-entries--stamp)
    (insert (agent-repl-prompt-entries--source-hash) "\n")))

(defmacro agent-repl-test-prompt-entries--capturing (&rest body)
  "Run BODY recording builds started and log records.
Binds `builds' to each spawn's (:label :dir :command :callback), newest
first, `errors' and `infos' to the formatted records."
  (declare (indent 0))
  `(let ((builds nil) (errors nil) (infos nil))
     (cl-letf (((symbol-function 'agent-repl--async-command)
                (lambda (label dir command callback)
                  (push (list :label label :dir dir :command command :callback callback) builds)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args) (push (apply #'format fmt args) errors)))
               ((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args) (push (apply #'format fmt args) infos))))
       ,@body)))

;;;; ---- Freshness ----

(ert-deftest agent-repl-test-prompt-entries-fresh-without-a-library ()
  "No library built is stale."
  (agent-repl-test-prompt-entries--with-grammar
    (should-not (agent-repl-prompt-entries--fresh-p))))

(ert-deftest agent-repl-test-prompt-entries-fresh-from-todays-sources ()
  "A library stamped with today's sources is fresh."
  (agent-repl-test-prompt-entries--with-grammar
    ;; Arrange
    (agent-repl-test-prompt-entries--built)
    ;; Act + Assert
    (should (agent-repl-prompt-entries--fresh-p))))

(ert-deftest agent-repl-test-prompt-entries-stale-after-a-source-changes ()
  "A source edited since the build makes the library stale."
  (agent-repl-test-prompt-entries--with-grammar
    ;; Arrange
    (agent-repl-test-prompt-entries--built)
    (with-temp-file (expand-file-name "scanner.c" (agent-repl-prompt-entries--grammar-dir))
      (insert "/* changed */\n"))
    ;; Act + Assert
    (should-not (agent-repl-prompt-entries--fresh-p))))

(ert-deftest agent-repl-test-prompt-entries-stale-without-a-stamp ()
  "A library with no stamp beside it is stale: what it was built from is unknown."
  (agent-repl-test-prompt-entries--with-grammar
    ;; Arrange
    (agent-repl-test-prompt-entries--built)
    (delete-file (agent-repl-prompt-entries--stamp))
    ;; Act + Assert
    (should-not (agent-repl-prompt-entries--fresh-p))))

;;;; ---- Ensuring the grammar ----

(ert-deftest agent-repl-test-prompt-entries-ensure-fresh-answers-at-once ()
  "A fresh library is put on the load path and answered with no build."
  (agent-repl-test-prompt-entries--with-grammar
    (agent-repl-test-prompt-entries--capturing
      ;; Arrange
      (agent-repl-test-prompt-entries--built)
      (let ((ready 0))
        ;; Act
        (agent-repl-prompt-entries-ensure (lambda () (cl-incf ready)))
        ;; Assert
        (should (equal (list ready builds treesit-extra-load-path)
                       (list 1 nil (list agent-repl-prompt-entries--library-dir))))))))

(ert-deftest agent-repl-test-prompt-entries-ensure-stale-starts-one-build-for-every-waiter ()
  "Two composers waiting on a stale grammar start one build, and neither is answered yet."
  (agent-repl-test-prompt-entries--with-grammar
    (agent-repl-test-prompt-entries--capturing
      (let ((ready 0))
        ;; Act
        (agent-repl-prompt-entries-ensure (lambda () (cl-incf ready)))
        (agent-repl-prompt-entries-ensure (lambda () (cl-incf ready)))
        ;; Assert
        (should (equal (list ready (length builds) (length agent-repl-prompt-entries--waiting))
                       '(0 1 2)))))))

(ert-deftest agent-repl-test-prompt-entries-build-compiles-both-sources-into-the-library ()
  "The build compiles the parser and scanner into agent-repl's own library."
  (agent-repl-test-prompt-entries--with-grammar
    (agent-repl-test-prompt-entries--capturing
      ;; Act
      (agent-repl-prompt-entries-ensure #'ignore)
      ;; Assert
      (let* ((command (plist-get (car builds) :command))
             (dir (agent-repl-prompt-entries--grammar-dir)))
        (should (equal (list (car command)
                             (cadr (member "-o" command))
                             (last command 2))
                       (list "cc"
                             (agent-repl-prompt-entries--library)
                             (list (expand-file-name "parser.c" dir)
                                   (expand-file-name "scanner.c" dir)))))
        (should (equal (plist-get (car builds) :dir) dir))
        (should (file-directory-p agent-repl-prompt-entries--library-dir))))))

;;;; ---- A build ending ----

(ert-deftest agent-repl-test-prompt-entries-build-success-stamps-and-answers-in-order ()
  "A build that succeeds stamps its sources and answers every waiter, first first."
  (agent-repl-test-prompt-entries--with-grammar
    (agent-repl-test-prompt-entries--capturing
      ;; Arrange
      (let ((answered nil))
        (agent-repl-prompt-entries-ensure (lambda () (push 'first answered)))
        (agent-repl-prompt-entries-ensure (lambda () (push 'second answered)))
        (with-temp-file (agent-repl-prompt-entries--library) (insert "lib"))
        ;; Act
        (funcall (plist-get (car builds) :callback) t "")
        ;; Assert
        (should (equal (list (reverse answered)
                             (agent-repl-prompt-entries--fresh-p)
                             agent-repl-prompt-entries--building
                             agent-repl-prompt-entries--waiting)
                       '((first second) t nil nil)))
        (should (cl-some (lambda (m) (string-match-p "elisp.prompt-entries.built .*waiting=2" m)) infos))))))

(ert-deftest agent-repl-test-prompt-entries-build-failure-is-an-error-and-answers-nothing ()
  "A build that fails records the compiler's words at ERROR, stamps nothing, answers nothing."
  (agent-repl-test-prompt-entries--with-grammar
    (agent-repl-test-prompt-entries--capturing
      ;; Arrange
      (let ((answered 0))
        (agent-repl-prompt-entries-ensure (lambda () (cl-incf answered)))
        ;; Act
        (funcall (plist-get (car builds) :callback) nil "parser.c:1: error: boom\n")
        ;; Assert
        (should (equal (list answered
                             (file-exists-p (agent-repl-prompt-entries--stamp))
                             agent-repl-prompt-entries--building
                             agent-repl-prompt-entries--waiting
                             treesit-extra-load-path)
                       '(0 nil nil nil nil)))
        (should (cl-some (lambda (m)
                           (string-match-p "elisp.prompt-entries.build-failed .*output=\"parser.c:1: error: boom\"" m))
                         errors))))))

(ert-deftest agent-repl-test-prompt-entries-a-failed-build-is-retried-by-the-next-composer ()
  "After a failure the next composer starts a new build rather than waiting forever."
  (agent-repl-test-prompt-entries--with-grammar
    (agent-repl-test-prompt-entries--capturing
      ;; Arrange
      (agent-repl-prompt-entries-ensure #'ignore)
      (funcall (plist-get (car builds) :callback) nil "boom")
      ;; Act
      (agent-repl-prompt-entries-ensure #'ignore)
      ;; Assert
      (should (= (length builds) 2)))))

;;;; ---- Marking the composer ----

;; A fake syntax tree: a node is (TYPE START END CHILDREN...).

(defun agent-repl-test-prompt-entries--search (node pred)
  "The first descendant of fake NODE, NODE included, satisfying PRED."
  (if (funcall pred node)
      node
    (cl-some (lambda (child) (agent-repl-test-prompt-entries--search child pred))
             (nthcdr 3 node))))

(defmacro agent-repl-test-prompt-entries--with-tree (headings &rest body)
  "Run BODY in a buffer whose parse reads as HEADINGS, fake heading nodes."
  (declare (indent 1))
  `(with-temp-buffer
     (insert "1.1 - yes\n1 🔧 Fixed\n2.1 - no\n")
     (setq agent-repl-prompt-entries--parser 'fake-parser)
     (cl-letf (((symbol-function 'treesit-query-capture)
                (lambda (_parser _query) (mapcar (lambda (h) (cons 'heading h)) ,headings)))
               ((symbol-function 'treesit-search-subtree)
                (lambda (node pred &rest _) (agent-repl-test-prompt-entries--search node pred)))
               ((symbol-function 'treesit-node-type) #'car)
               ((symbol-function 'treesit-node-start) #'cadr)
               ((symbol-function 'treesit-node-end) #'caddr))
       ,@body)))

(defconst agent-repl-test-prompt-entries--headings
  '(("heading" 1 10 ("label" 1 4) ("separator" 4 7))
    ("heading" 11 22 ("label" 11 12) ("marker" 13 14))
    ("heading" 23 32 ("label" 23 26)))
  "An entry with a separator, a pasted root with a marker, an entry without a separator.")

(ert-deftest agent-repl-test-prompt-entries-label-spans-are-the-entries-alone ()
  "Each entry's label and separator is a span, a root with a marker none."
  (agent-repl-test-prompt-entries--with-tree agent-repl-test-prompt-entries--headings
    (should (equal (agent-repl-prompt-entries--label-spans) '((1 . 7) (23 . 26))))))

(ert-deftest agent-repl-test-prompt-entries-mark-draws-each-span-in-the-label-face ()
  "Marking lays one overlay in the label face over each entry's span."
  (agent-repl-test-prompt-entries--with-tree agent-repl-test-prompt-entries--headings
    ;; Act
    (agent-repl-prompt-entries-mark)
    ;; Assert
    (should (equal (sort (mapcar (lambda (ov) (list (overlay-start ov) (overlay-end ov)
                                                    (overlay-get ov 'face)))
                                 (cl-remove-if-not (lambda (ov) (overlay-get ov 'agent-repl-prompt-entry))
                                                   (overlays-in (point-min) (point-max))))
                         (lambda (a b) (< (car a) (car b))))
                   '((1 7 agent-repl-prompt-entry-label) (23 26 agent-repl-prompt-entry-label))))))

(ert-deftest agent-repl-test-prompt-entries-mark-restates-rather-than-piles-up ()
  "Marking again replaces the overlays: an entry that stopped being one loses its mark."
  (let ((headings agent-repl-test-prompt-entries--headings))
    (agent-repl-test-prompt-entries--with-tree headings
      ;; Arrange
      (agent-repl-prompt-entries-mark)
      (setq headings (list (car headings)))
      ;; Act
      (agent-repl-prompt-entries-mark)
      ;; Assert
      (should (equal (mapcar #'overlay-start
                             (cl-remove-if-not (lambda (ov) (overlay-get ov 'agent-repl-prompt-entry))
                                               (overlays-in (point-min) (point-max))))
                     '(1))))))

(ert-deftest agent-repl-test-prompt-entries-mark-before-the-grammar-loads-draws-nothing ()
  "A composer whose parser is not made yet is not marked."
  (with-temp-buffer
    (insert "1.1 - yes\n")
    (setq agent-repl-prompt-entries--parser nil)
    (agent-repl-prompt-entries-mark)
    (should-not (overlays-in (point-min) (point-max)))))

;;;; ---- Enabling a composer ----

(ert-deftest agent-repl-test-prompt-entries-enable-off-marks-nothing-and-says-so ()
  "With entry marking off, no grammar is ensured and the log says why."
  (agent-repl-test-prompt-entries--capturing
    (let ((agent-repl-input-entry-marking nil)
          (ensured nil))
      (cl-letf (((symbol-function 'agent-repl-prompt-entries-ensure)
                 (lambda (_f) (setq ensured t))))
        ;; Act
        (with-temp-buffer (agent-repl-prompt-entries-enable "ws-1")))
      ;; Assert
      (should-not ensured)
      (should (cl-some (lambda (m) (string-match-p "outcome=off reason=agent-repl-input-entry-marking-nil" m)) infos)))))

(ert-deftest agent-repl-test-prompt-entries-enable-marks-once-the-grammar-loads ()
  "An enabled composer gets its parser, marks on change, and is marked at once."
  (agent-repl-test-prompt-entries--capturing
    (let ((agent-repl-input-entry-marking t)
          (marked 0))
      (cl-letf (((symbol-function 'agent-repl-prompt-entries-ensure) #'funcall)
                ((symbol-function 'treesit-parser-create)
                 (lambda (language) (list 'parser language)))
                ((symbol-function 'agent-repl-prompt-entries-mark)
                 (lambda () (cl-incf marked))))
        (with-temp-buffer
          ;; Act
          (agent-repl-prompt-entries-enable "ws-1")
          ;; Assert
          (should (equal (list agent-repl-prompt-entries--parser
                               (car (memq #'agent-repl-prompt-entries--on-change after-change-functions))
                               marked)
                         (list '(parser tldr_prompt) #'agent-repl-prompt-entries--on-change 1)))
          (insert "x")
          (should (= marked 2)))))))

(ert-deftest agent-repl-test-prompt-entries-enable-for-a-composer-that-died-touches-nothing ()
  "A grammar loading after its composer was killed creates no parser anywhere."
  (agent-repl-test-prompt-entries--capturing
    (let ((agent-repl-input-entry-marking t)
          (ready nil)
          (created nil))
      (cl-letf (((symbol-function 'agent-repl-prompt-entries-ensure)
                 (lambda (f) (setq ready f)))
                ((symbol-function 'treesit-parser-create)
                 (lambda (_l) (setq created t))))
        (with-temp-buffer (agent-repl-prompt-entries-enable "ws-1"))
        ;; Act
        (funcall ready))
      ;; Assert
      (should-not created))))

(provide 'test-prompt-entries)

;;; test-prompt-entries.el ends here
