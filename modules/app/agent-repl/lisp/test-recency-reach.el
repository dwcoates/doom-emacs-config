;;; test-recency-reach.el --- Tests for the recency walk's reach -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for recency-reach.el: which tabs the recency walk reaches soonest,
;; and reporting that reach to the daemon for the sidebar's gutters.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- The reach ----

(ert-deftest agent-repl-test-recency-reach-assigns-each-tab-its-nearer-chord ()
  "A tab goes to whichever chord lands on it in fewer presses."
  ;; Arrange: the ring from the most recent: c is current.
  (let ((ring '("c" "b" "a" "z" "y")))
    ;; Act / Assert: `s-[' meets b a z y; `s-]' meets y z a b.
    (should (equal (agent-repl--recency-reach ring ring "c")
                   '(("b" :older 1) ("a" :older 2) ("y" :newer 1) ("z" :newer 2))))))

(ert-deftest agent-repl-test-recency-reach-gives-a-tie-to-older ()
  "A tab equally far both ways is reached `s-['."
  (let ((ring '("c" "b" "a" "z")))
    (should (equal (agent-repl--recency-reach ring ring "c")
                   '(("b" :older 1) ("a" :older 2) ("z" :newer 1))))))

(ert-deftest agent-repl-test-recency-reach-keeps-the-two-nearest-each-way ()
  "Each chord marks at most two tabs, the nearest."
  ;; Arrange: six other tabs, three nearer each way, so one each way is left.
  (let* ((ring (cons "current" (mapcar (lambda (i) (format "t%02d" i)) (number-sequence 1 6))))
         ;; Act.
         (reach (agent-repl--recency-reach ring ring "current")))
    ;; Assert.
    (should (equal (mapcar #'car reach)
                   '("t01" "t02" "t06" "t05")))
    (should (equal (mapcar #'caddr reach) '(1 2 1 2)))))

(ert-deftest agent-repl-test-recency-reach-counts-only-drawn-tabs ()
  "A folded tab is passed over, so it neither takes a press nor wears a gutter."
  (should (equal (agent-repl--recency-reach '("c" "folded" "b" "a") '("c" "b" "a") "c")
                 '(("b" :older 1) ("a" :newer 1)))))

(ert-deftest agent-repl-test-recency-reach-from-an-undrawn-current ()
  "A folded current workspace has no gutter and steps from its own place."
  (should (equal (agent-repl--recency-reach '("folded" "b" "a") '("b" "a") "folded")
                 '(("b" :older 1) ("a" :newer 1)))))

(ert-deftest agent-repl-test-recency-reach-of-a-lone-tab-is-empty ()
  "With no other tab nothing is reached."
  (should-not (agent-repl--recency-reach '("c") '("c") "c")))

(ert-deftest agent-repl-test-recency-reach-counts-with-the-ring-walk ()
  "The presses are counted by the chords' own stepping, never a second copy of it."
  (should (string-match-p "agent-repl--ring-walk"
                          (format "%S" (symbol-function 'agent-repl--recency-reach)))))

(ert-deftest agent-repl-test-recency-reach-now-starts-from-the-pending-request ()
  "The reach starts where the next press does: an unanswered request first."
  (cl-letf (((symbol-function 'agent-repl-host-pending-selection) (lambda () "b"))
            ((symbol-function 'agent-repl--ws-current-name) (lambda () "c"))
            ((symbol-function 'agent-repl--recency-ring) (lambda (current) (list current "c" "a")))
            ((symbol-function 'agent-repl--drawn-tab-names) (lambda () '("a" "b" "c"))))
    (should (equal (agent-repl--recency-reach-now) '(("c" :older 1) ("a" :newer 1))))))

(ert-deftest agent-repl-test-recency-reach-now-with-no-workspace-is-empty ()
  "Standing on no workspace reaches nothing."
  (cl-letf (((symbol-function 'agent-repl-host-pending-selection) #'ignore)
            ((symbol-function 'agent-repl--ws-current-name) #'ignore))
    (should-not (agent-repl--recency-reach-now))))

;;;; ---- The request ----

(ert-deftest agent-repl-test-recency-reach-request-names-each-ref ()
  "Each reach is sent with its workspace's daemon ref."
  (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (ws) (list :id ws :dir (concat "/w/" ws)))))
    (should (equal (agent-repl--recency-reach-request '(("a" :older 1) ("b" :newer 2)))
                   '(:reaches ((:workspace (:id "a" :dir "/w/a") :direction (:arm :older :value (:presses 1)))
                               (:workspace (:id "b" :dir "/w/b") :direction (:arm :newer :value (:presses 2)))))))))

(ert-deftest agent-repl-test-recency-reach-request-refuses-an-unregistered-tab ()
  "A reached tab with no ref is a broken invariant: recorded at error and signalled."
  (let ((records nil))
    (cl-letf (((symbol-function 'agent-repl-host-ref) #'ignore)
              ((symbol-function 'agent-repl--error)
               (agent-repl-test--record-leveled-into records 'error)))
      (should-error (agent-repl--recency-reach-request '(("a" :older 1))))
      (should (seq-some (lambda (r) (string-match-p "recency-reach.unregistered ws=a arm=:older presses=1" (cdr r)))
                        records)))))

(ert-deftest agent-repl-test-recency-reach-request-encodes ()
  "The request a reach builds is one the wire codec takes."
  (cl-letf (((symbol-function 'agent-repl-host-ref) (lambda (ws) (list :id ws :dir (concat "/w/" ws)))))
    (should (equal (json-serialize
                    (agent-repl-wire-encode-report-recency-reach-request
                     (agent-repl--recency-reach-request '(("a" :older 1)))))
                   "{\"reaches\":[{\"workspace\":{\"id\":\"a\",\"dir\":\"/w/a\"},\"older\":{\"presses\":1}}]}"))))

;;;; ---- Reporting it ----

(defmacro agent-repl-test-recency-reach--with-reporter (reach conn &rest body)
  "Run BODY with the reporter fresh, the walk reaching REACH, and CONN newest.
Every `ReportRecencyReach' call is recorded into `calls' as (CONN . REQUEST)
with its callbacks in `answers', and every log record into `records' as
\(LEVEL . LINE)."
  (declare (indent 2))
  `(let ((agent-repl--recency-reach-serial (agent-repl-serial-create))
         (agent-repl--recency-reach-reported 'none)
         (agent-repl--recency-reach-timer nil)
         (reach-now ,reach)
         (calls nil)
         (answers nil)
         (records nil))
     (cl-letf (((symbol-function 'agent-repl-link-newest) (lambda () ,conn))
               ((symbol-function 'agent-repl--recency-reach-now) (lambda () reach-now))
               ((symbol-function 'agent-repl-host-ref) (lambda (ws) (list :id ws :dir "/w")))
               ((symbol-function 'agent-repl-rpc-report-recency-reach)
                (lambda (c request &rest keys)
                  (push (cons c request) calls)
                  (push keys answers)))
               ((symbol-function 'agent-repl--log) (agent-repl-test--record-leveled-into records 'debug))
               ((symbol-function 'agent-repl--info) (agent-repl-test--record-leveled-into records 'info))
               ((symbol-function 'agent-repl--error) (agent-repl-test--record-leveled-into records 'error)))
       ,@body)))

(defun agent-repl-test-recency-reach--recorded-p (records level pattern)
  "Non-nil when RECORDS holds a LEVEL line matching PATTERN."
  (seq-some (lambda (r) (and (eq (car r) level) (string-match-p pattern (cdr r)))) records))

(defconst agent-repl-test-recency-reach--success '(:arm :success :value nil)
  "The answer of a report the daemon took.")

(ert-deftest agent-repl-test-recency-reach-report-sends-the-reach ()
  "A report sends the current reach to the newest daemon."
  (agent-repl-test-recency-reach--with-reporter '(("a" :older 1)) 'conn
    (agent-repl--recency-reach-report)
    (should (equal (mapcar #'car calls) '(conn)))
    (should (equal (length (plist-get (cdar calls) :reaches)) 1))))

(ert-deftest agent-repl-test-recency-reach-report-with-no-link-sends-nothing ()
  "With no daemon there is no stream to belong to."
  (agent-repl-test-recency-reach--with-reporter '(("a" :older 1)) nil
    (agent-repl--recency-reach-report)
    (should-not calls)
    (should (agent-repl-test-recency-reach--recorded-p records 'debug "report skipped reason=no-link"))))

(ert-deftest agent-repl-test-recency-reach-report-of-an-unchanged-reach-sends-nothing ()
  "A reach the daemon already took is not sent again."
  (agent-repl-test-recency-reach--with-reporter '(("a" :older 1)) 'conn
    (agent-repl--recency-reach-report)
    (funcall (plist-get (car answers) :on-response) agent-repl-test-recency-reach--success)
    (agent-repl--recency-reach-report)
    (should (= (length calls) 1))
    (should (agent-repl-test-recency-reach--recorded-p records 'debug "report unchanged reaches=1"))))

(ert-deftest agent-repl-test-recency-reach-report-of-a-changed-reach-sends-it ()
  "A reach that moved since the daemon took the last is sent."
  (agent-repl-test-recency-reach--with-reporter '(("a" :older 1)) 'conn
    (agent-repl--recency-reach-report)
    (funcall (plist-get (car answers) :on-response) agent-repl-test-recency-reach--success)
    (setq reach-now '(("b" :newer 1)))
    (agent-repl--recency-reach-report)
    (should (= (length calls) 2))))

(ert-deftest agent-repl-test-recency-reach-report-in-flight-owes-the-next ()
  "A report while one is in flight is owed, and sent once that one is answered."
  (agent-repl-test-recency-reach--with-reporter '(("a" :older 1)) 'conn
    (agent-repl--recency-reach-report)
    (setq reach-now '(("b" :newer 1)))
    (agent-repl--recency-reach-report)
    (should (= (length calls) 1))
    (funcall (plist-get (car answers) :on-response) agent-repl-test-recency-reach--success)
    (should (= (length calls) 2))
    (should (equal (plist-get (plist-get (car (plist-get (cdar calls) :reaches)) :workspace) :id) "b"))))

(ert-deftest agent-repl-test-recency-reach-refusal-is-recorded-at-info-and-not-taken ()
  "A refusal for want of a stream is an ordinary reconnect, and the reach is sent again."
  (agent-repl-test-recency-reach--with-reporter '(("a" :older 1)) 'conn
    (agent-repl--recency-reach-report)
    (funcall (plist-get (car answers) :on-response)
             '(:arm :error :value (:cause (:arm :no-emacs-stream :value nil))))
    (should (agent-repl-test-recency-reach--recorded-p records 'info "report refused reaches=1"))
    (should (eq agent-repl--recency-reach-reported 'none))))

(ert-deftest agent-repl-test-recency-reach-transport-failure-is-an-error ()
  "A report that never reached an answer is recorded at error and not taken."
  (agent-repl-test-recency-reach--with-reporter '(("a" :older 1)) 'conn
    (agent-repl--recency-reach-report)
    (funcall (plist-get (car answers) :on-failure) "connection refused")
    (should (agent-repl-test-recency-reach--recorded-p records 'error "report failed reaches=1 detail=\"connection refused\""))
    (should (eq agent-repl--recency-reach-reported 'none))))

(ert-deftest agent-repl-test-recency-reach-unknown-arm-is-an-error ()
  "An answer naming no known outcome is recorded at error."
  (agent-repl-test-recency-reach--with-reporter '(("a" :older 1)) 'conn
    (agent-repl--recency-reach-report)
    (funcall (plist-get (car answers) :on-response) '(:arm :elsewhere :value nil))
    (should (agent-repl-test-recency-reach--recorded-p records 'error "report unknown-arm reaches=1 arm=:elsewhere"))))

(ert-deftest agent-repl-test-recency-reach-link-edge-reports-again ()
  "A new stream holds none of the reach, so a link edge sends it though unchanged."
  (agent-repl-test-recency-reach--with-reporter '(("a" :older 1)) 'conn
    (agent-repl--recency-reach-report)
    (funcall (plist-get (car answers) :on-response) agent-repl-test-recency-reach--success)
    (agent-repl--recency-reach-on-link 'conn)
    (should (= (length calls) 2))))

(ert-deftest agent-repl-test-recency-reach-change-coalesces-into-one-timer ()
  "Several changes in one command loop arm one recompute."
  (agent-repl-test-recency-reach--with-reporter nil 'conn
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) (timer-create))))
      (agent-repl--recency-reach-changed)
      (let ((first agent-repl--recency-reach-timer))
        (agent-repl--recency-reach-changed)
        (should (eq agent-repl--recency-reach-timer first))))))

(ert-deftest agent-repl-test-recency-reach-is-recomputed-on-each-change ()
  "Every roster push, perspective activation and link edge reaches the reporter."
  (should (memq #'agent-repl--recency-reach-changed agent-repl-roster-update-functions))
  (should (memq #'agent-repl--recency-reach-on-link agent-repl-link-up-functions))
  (should (memq #'agent-repl--recency-reach-on-link agent-repl-link-handover-functions))
  (should (memq #'agent-repl--recency-reach-on-link agent-repl-link-promote-functions)))

(provide 'test-recency-reach)

;;; test-recency-reach.el ends here
