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
                (agent-repl-test--record-into errors))
               ((symbol-function 'agent-repl--info)
                (agent-repl-test--record-into infos)))
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
    ("heading" 21 30 ("label" 21 24)))
  "An entry with a separator, a pasted root with a marker, an entry without a separator.
They stand over the fake tree's text: `1.1 - yes', `1 🔧 Fixed', `2.1 - no'.")

(ert-deftest agent-repl-test-prompt-entries-entries-are-the-headings-without-a-marker ()
  "Each entry is its label and the span of its label and separator; a root with a marker is none."
  (agent-repl-test-prompt-entries--with-tree agent-repl-test-prompt-entries--headings
    (should (equal (agent-repl-prompt-entries--entries) '(("1.1" 1 7) ("2.1" 21 24))))))

(ert-deftest agent-repl-test-prompt-entries-entry-at-point-is-the-entry-on-points-line ()
  "Point anywhere on an entry's line names that entry; elsewhere none."
  (agent-repl-test-prompt-entries--with-tree agent-repl-test-prompt-entries--headings
    (let ((entries (agent-repl-prompt-entries--entries)))
      (should (equal (list (progn (goto-char 8) (agent-repl-prompt-entries--entry-at-point entries))
                           (progn (goto-char 15) (agent-repl-prompt-entries--entry-at-point entries))
                           (progn (goto-char 28) (agent-repl-prompt-entries--entry-at-point entries)))
                     '("1.1" nil "2.1"))))))

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
                   '((1 7 agent-repl-prompt-entry-label) (21 24 agent-repl-prompt-entry-label))))))

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

(ert-deftest agent-repl-test-prompt-entries-mark-draws-a-label-answering-nothing-as-a-warning ()
  "A label the daemon says answers nothing wears the unanswered face."
  (agent-repl-test-prompt-entries--with-tree agent-repl-test-prompt-entries--headings
    (setq agent-repl-prompt-entries--ws "ws-1")
    (cl-letf (((symbol-function 'agent-repl-host-unanswered-entries) (lambda (_ws) '("2.1"))))
      ;; Act
      (agent-repl-prompt-entries-mark))
    ;; Assert
    (should (equal (mapcar (lambda (ov) (overlay-get ov 'face))
                           (sort (cl-remove-if-not (lambda (ov) (overlay-get ov 'agent-repl-prompt-entry))
                                                   (overlays-in (point-min) (point-max)))
                                 (lambda (a b) (< (overlay-start a) (overlay-start b)))))
                   '(agent-repl-prompt-entry-label agent-repl-prompt-entry-unanswered)))))

(ert-deftest agent-repl-test-prompt-entries-a-host-push-re-marks-the-workspaces-composer ()
  "A host push re-marks the composer of its workspace."
  (let ((marked nil))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (cl-letf (((symbol-function 'agent-repl--input-buffer) (lambda (ws) (and (equal ws "ws-1") buf)))
                  ((symbol-function 'agent-repl-prompt-entries-mark) (lambda () (push (current-buffer) marked))))
          ;; Act
          (agent-repl-prompt-entries--host-updated "ws-1" nil))
        ;; Assert
        (should (equal marked (list buf)))))))

(ert-deftest agent-repl-test-prompt-entries-a-host-push-for-a-workspace-with-no-composer-marks-nothing ()
  "A host push for a workspace with no composer touches nothing."
  (let ((marked 0))
    (cl-letf (((symbol-function 'agent-repl--input-buffer) (lambda (_ws) nil))
              ((symbol-function 'agent-repl-prompt-entries-mark) (lambda () (cl-incf marked))))
      ;; Act
      (agent-repl-prompt-entries--host-updated "ws-1" nil))
    ;; Assert
    (should (= marked 0))))

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
                               agent-repl-prompt-entries--ws
                               (car (memq #'agent-repl-prompt-entries--on-change after-change-functions))
                               (car (memq #'agent-repl-prompt-entries--report post-command-hook))
                               marked)
                         (list '(parser tldr_prompt) "ws-1" #'agent-repl-prompt-entries--on-change
                               #'agent-repl-prompt-entries--report 1)))
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

;;;; ---- Reporting the composer's entries ----

(defmacro agent-repl-test-prompt-entries--reporting (&rest body)
  "Run BODY in a reporting composer of \"ws-1\" over the fake tree.
Binds `sent' to the report requests made, newest first, `answer' and
`fail' to the last report's callbacks, and `logs' to the formatted records
of every rung."
  (declare (indent 0))
  `(let ((sent nil) (answer nil) (fail nil) (logs nil))
     (agent-repl-test-prompt-entries--with-tree agent-repl-test-prompt-entries--headings
       (setq agent-repl-prompt-entries--ws "ws-1"
             agent-repl-prompt-entries--serial (agent-repl-serial-create))
       (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) '(:id "ws-1" :dir "/w")))
                 ((symbol-function 'agent-repl-host-conn-or-primary) (lambda (_ws) 'conn))
                 ((symbol-function 'agent-repl-rpc-report-composer-entries)
                  (lambda (_conn request &rest keys)
                    (push request sent)
                    (setq answer (plist-get keys :on-response) fail (plist-get keys :on-failure))))
                 ((symbol-function 'agent-repl--log) (agent-repl-test--record-into logs))
                 ((symbol-function 'agent-repl--info) (agent-repl-test--record-into logs))
                 ((symbol-function 'agent-repl--warn) (agent-repl-test--record-into logs))
                 ((symbol-function 'agent-repl--error) (agent-repl-test--record-into logs)))
         ,@body))))

(ert-deftest agent-repl-test-prompt-entries-report-names-the-labels-and-the-entry-at-point ()
  "A report carries every entry's label in order and the one point is on."
  (agent-repl-test-prompt-entries--reporting
    ;; Arrange
    (goto-char 25)
    ;; Act
    (agent-repl-prompt-entries--report)
    ;; Assert
    (should (equal (car sent) '(:workspace (:id "ws-1" :dir "/w") :labels ("1.1" "2.1")
                               :at-point (:arm :entry :value (:label "2.1")))))))

(ert-deftest agent-repl-test-prompt-entries-report-with-point-on-no-entry-says-none ()
  "Point off every entry line is reported as no entry."
  (agent-repl-test-prompt-entries--reporting
    (goto-char 15)
    (agent-repl-prompt-entries--report)
    (should (equal (plist-get (car sent) :at-point) '(:arm :none)))))

(ert-deftest agent-repl-test-prompt-entries-report-sends-nothing-when-nothing-moved ()
  "A command that moved neither the entries nor point's entry sends nothing."
  (agent-repl-test-prompt-entries--reporting
    ;; Arrange
    (goto-char 2)
    (agent-repl-prompt-entries--report)
    (funcall answer '(:arm :success :value nil))
    ;; Act: point moves within the same entry line.
    (goto-char 5)
    (agent-repl-prompt-entries--report)
    ;; Assert
    (should (= (length sent) 1))))

(ert-deftest agent-repl-test-prompt-entries-report-waits-for-the-one-in-flight-then-sends-the-latest ()
  "A move while a report is in flight is owed, and sent once that report is answered."
  (agent-repl-test-prompt-entries--reporting
    ;; Arrange
    (goto-char 2)
    (agent-repl-prompt-entries--report)
    (goto-char 15)
    (agent-repl-prompt-entries--report)
    (goto-char 25)
    (agent-repl-prompt-entries--report)
    (should (= (length sent) 1))
    ;; Act
    (funcall answer '(:arm :success :value nil))
    ;; Assert: one more report, of where point stands now.
    (should (equal (mapcar (lambda (r) (plist-get r :at-point)) sent)
                   '((:arm :entry :value (:label "2.1")) (:arm :entry :value (:label "1.1")))))))

(ert-deftest agent-repl-test-prompt-entries-a-failed-report-is-warned-and-sent-again ()
  "A report the daemon never answered is logged at WARN and sent again on the next command."
  (agent-repl-test-prompt-entries--reporting
    ;; Arrange
    (goto-char 2)
    (agent-repl-prompt-entries--report)
    ;; Act
    (funcall fail '(:kind :timeout))
    (agent-repl-prompt-entries--report)
    ;; Assert
    (should (= (length sent) 2))
    (should (cl-some (lambda (m) (string-match-p "elisp.prompt-entries.report-failure ws=ws-1" m)) logs))))

(ert-deftest agent-repl-test-prompt-entries-a-refused-report-is-recorded-at-info ()
  "A report the daemon refused is recorded with its cause."
  (agent-repl-test-prompt-entries--reporting
    (agent-repl-prompt-entries--report)
    (funcall answer '(:arm :error :value (:cause (:arm :not-yet-adopted :value nil))))
    (should (cl-some (lambda (m) (string-match-p "report-refused ws=ws-1 .*not-yet-adopted" m)) logs))))

(ert-deftest agent-repl-test-prompt-entries-a-report-answered-by-an-unknown-arm-is-an-error ()
  "An answer arm the composer does not know is recorded at ERROR."
  (agent-repl-test-prompt-entries--reporting
    (agent-repl-prompt-entries--report)
    (funcall answer '(:arm :mystery))
    (should (cl-some (lambda (m) (string-match-p "report-unknown-arm ws=ws-1 arm=:mystery" m)) logs))))

(ert-deftest agent-repl-test-prompt-entries-report-with-no-ref-sends-nothing-and-tries-again ()
  "With no daemon-minted ref nothing is sent, and the next command tries again."
  (agent-repl-test-prompt-entries--reporting
    ;; Arrange
    (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) nil)))
      (agent-repl-prompt-entries--report))
    ;; Act
    (agent-repl-prompt-entries--report)
    ;; Assert
    (should (= (length sent) 1))
    (should (cl-some (lambda (m) (string-match-p "report-skipped ws=ws-1 ref=nil" m)) logs))))

;;;; ---- Opening a line beside an entry ----

(defmacro agent-repl-test-prompt-entries--opening (answer &rest body)
  "Run BODY in a composer of \"ws-1\" over the fake tree, the neighbor call answering ANSWER.
ANSWER is a response plist, or (:signal ERR) to signal it.  Binds
`asked' to the neighbor requests made and `logs' to every record."
  (declare (indent 1))
  `(let ((asked nil) (logs nil))
     (agent-repl-test-prompt-entries--with-tree agent-repl-test-prompt-entries--headings
       (setq agent-repl-prompt-entries--ws "ws-1")
       (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (_ws) '(:id "ws-1" :dir "/w")))
                 ((symbol-function 'agent-repl-host-conn-or-primary) (lambda (_ws) 'conn))
                 ((symbol-function 'agent-repl-rpc-find-entry-neighbor-sync)
                  (lambda (_conn request &optional _timeout)
                    (push request asked)
                    (let ((a ,answer))
                      (if (eq (car a) :signal) (signal (cadr a) (cddr a)) a))))
                 ((symbol-function 'evil-open-below) (lambda (_n) (end-of-line) (insert "\n")))
                 ((symbol-function 'evil-open-above) (lambda (_n) (beginning-of-line) (insert "\n") (forward-line -1)))
                 ((symbol-function 'agent-repl--log) (agent-repl-test--record-into logs))
                 ((symbol-function 'agent-repl--info) (agent-repl-test--record-into logs))
                 ((symbol-function 'agent-repl--warn) (agent-repl-test--record-into logs))
                 ((symbol-function 'agent-repl--error) (agent-repl-test--record-into logs)))
         ,@body))))

(defun agent-repl-test-prompt-entries--line ()
  "The text of point's line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(ert-deftest agent-repl-test-prompt-entries-opening-is-the-label-and-its-dash ()
  "A blank entry opens on the label, a space, a dash and a space."
  (should (equal (agent-repl-prompt-entries-opening "3.1.2") "3.1.2 - ")))

(ert-deftest agent-repl-test-prompt-entries-every-opened-entry-is-opened-by-the-one-helper ()
  "`o'/`O' and a branch click open entries through `agent-repl-prompt-entries-opening'."
  (dolist (case '(("prompt-entries.el" "agent-repl-prompt-entries--open")
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

(ert-deftest agent-repl-test-prompt-entries-o-on-an-entry-opens-the-next-nodes-entry ()
  "`o' on an entry line opens a line below filled with the next node's label."
  (agent-repl-test-prompt-entries--opening '(:arm :success :value (:neighbor (:arm :found :value (:label "1.2"))))
    ;; Arrange
    (goto-char 3)
    ;; Act
    (agent-repl-prompt-entries-open-below)
    ;; Assert
    (should (equal (list (agent-repl-test-prompt-entries--line) (plist-get (car asked) :direction) (plist-get (car asked) :label))
                   '("1.2 - " (:arm :next) "1.1")))))

(ert-deftest agent-repl-test-prompt-entries-O-on-an-entry-opens-the-previous-nodes-entry ()
  "`O' on an entry line opens a line above filled with the previous node's label."
  (agent-repl-test-prompt-entries--opening '(:arm :success :value (:neighbor (:arm :found :value (:label "2"))))
    ;; Arrange
    (goto-char 23)
    ;; Act
    (agent-repl-prompt-entries-open-above)
    ;; Assert
    (should (equal (list (agent-repl-test-prompt-entries--line) (plist-get (car asked) :direction))
                   '("2 - " (:arm :previous))))))

(ert-deftest agent-repl-test-prompt-entries-o-with-no-neighbor-opens-an-empty-line ()
  "An entry with no neighbor that way opens the line empty."
  (agent-repl-test-prompt-entries--opening '(:arm :success :value (:neighbor (:arm :none :value nil)))
    (goto-char 3)
    (agent-repl-prompt-entries-open-below)
    (should (equal (agent-repl-test-prompt-entries--line) ""))))

(ert-deftest agent-repl-test-prompt-entries-o-off-an-entry-asks-nothing ()
  "`o' on a line that is no entry opens it empty and asks the daemon nothing."
  (agent-repl-test-prompt-entries--opening '(:arm :success :value (:neighbor (:arm :none :value nil)))
    (goto-char 15)
    (agent-repl-prompt-entries-open-below)
    (should (equal (list (agent-repl-test-prompt-entries--line) asked) '("" nil)))))

(ert-deftest agent-repl-test-prompt-entries-a-refused-neighbor-opens-an-empty-line ()
  "A refusal opens the line empty and is recorded at info."
  (agent-repl-test-prompt-entries--opening '(:arm :error :value (:cause (:arm :not-yet-adopted :value nil)))
    (goto-char 3)
    (agent-repl-prompt-entries-open-below)
    (should (equal (agent-repl-test-prompt-entries--line) ""))
    (should (cl-some (lambda (m) (string-match-p "neighbor-refused ws=ws-1 label=1.1" m)) logs))))

(ert-deftest agent-repl-test-prompt-entries-an-unanswered-neighbor-is-warned ()
  "A daemon that never answers opens the line empty and is logged at WARN."
  (agent-repl-test-prompt-entries--opening '(:signal agent-repl-connect-error "timed out")
    (goto-char 3)
    (agent-repl-prompt-entries-open-below)
    (should (equal (agent-repl-test-prompt-entries--line) ""))
    (should (cl-some (lambda (m) (string-match-p "neighbor-failure ws=ws-1 label=1.1" m)) logs))))

(ert-deftest agent-repl-test-prompt-entries-a-neighbor-answered-by-an-unknown-arm-is-an-error ()
  "An answer arm the composer does not know is recorded at ERROR."
  (agent-repl-test-prompt-entries--opening '(:arm :mystery)
    (goto-char 3)
    (agent-repl-prompt-entries-open-below)
    (should (cl-some (lambda (m) (string-match-p "neighbor-unknown-arm ws=ws-1 arm=:mystery" m)) logs))))

(ert-deftest agent-repl-test-prompt-entries-o-and-O-are-bound-in-command-mode ()
  "`o' and `O' are bound in the composer's normal state to the entry-aware opens."
  (let (bound)
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (state map &rest bindings) (push (list state (eq map agent-repl-input-mode-map) bindings) bound))))
      (agent-repl-prompt-entries-bind-keys))
    (should (equal bound
                   (list (list 'normal t (list "o" #'agent-repl-prompt-entries-open-below
                                               "O" #'agent-repl-prompt-entries-open-above)))))))

(provide 'test-prompt-entries)

;;; test-prompt-entries.el ends here
