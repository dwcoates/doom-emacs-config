;;; test-readiness.el --- ERT tests for readiness.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the deploy-readiness poller and its modeline segment.
;;
;; The report SCRIPT is not exercised here — it has its own hermetic harness
;; (`bin/test-readiness-report.sh'), and per AGENTS.md "We test lisp, not
;; external code" the boundary wrapper `agent-repl--readiness-run-script' is
;; stubbed in every test that reaches it.  Nothing in this file runs git,
;; launchctl, or a subprocess of any kind.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-readiness.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; Fixtures

(defun agent-repl-test--readiness-json (&rest systems)
  "Return a report document whose systems list is SYSTEMS (JSON strings)."
  (concat "{\"generated_at\": \"2026-07-28T12:00:00Z\","
          " \"repo_head\": {\"sha\": \"abc\"},"
          " \"systems\": [" (string-join systems ", ") "]}"))

(defun agent-repl-test--readiness-system
    (name &optional behind mins ready running-pid stale error)
  "Return one system object as a JSON string."
  (format (concat "{\"name\": \"%s\", \"deployed_sha\": \"dep\","
                  " \"deployed_dirty\": false, \"source_sha\": \"src\","
                  " \"commits_behind\": %s, \"minutes_behind\": %s,"
                  " \"running\": %s, \"ready\": %s%s}")
          name
          (if behind (number-to-string behind) "null")
          (if mins (number-to-string mins) "null")
          (if running-pid
              (format "{\"pid\": %d, \"started_at\": \"2026-07-28T11:00:00Z\", \"stale_binary\": %s}"
                      running-pid (if stale "true" "false"))
            "null")
          (if ready "true" "false")
          (if error (format ", \"error\": \"%s\"" error) "")))

(defmacro agent-repl-test--with-readiness-state (&rest body)
  "Run BODY with the readiness globals bound to fresh, isolated values."
  (declare (indent 0))
  `(let ((agent-repl--readiness nil)
         (agent-repl--readiness-process nil)
         (agent-repl--readiness-timer nil)
         (agent-repl--timers nil)
         (agent-repl-readiness-enabled t))
     ,@body))

;;;; Parsing

(ert-deftest agent-repl-readiness-test-parse-keys-systems-by-name ()
  "The parsed report indexes each system object under its name."
  ;; Arrange
  (let ((json (agent-repl-test--readiness-json
               (agent-repl-test--readiness-system "daemon" 0 0 t)
               (agent-repl-test--readiness-system "webapp" 3 40 nil))))
    ;; Act
    (let ((parsed (agent-repl--readiness-parse json)))
      ;; Assert
      (should (equal '("daemon" "webapp")
                     (mapcar #'car (plist-get parsed :systems)))))))

(ert-deftest agent-repl-readiness-test-parse-keeps-generated-at ()
  "The report's generation timestamp survives parsing."
  ;; Arrange
  (let ((json (agent-repl-test--readiness-json
               (agent-repl-test--readiness-system "daemon" 0 0 t))))
    ;; Act / Assert
    (should (equal "2026-07-28T12:00:00Z"
                   (plist-get (agent-repl--readiness-parse json) :generated-at)))))

(ert-deftest agent-repl-readiness-test-parse-renders-json-null-as-nil ()
  "A null commits_behind parses to nil, not to zero."
  ;; Arrange
  (let ((json (agent-repl-test--readiness-json
               (agent-repl-test--readiness-system "daemon" nil nil nil))))
    ;; Act
    (let* ((parsed (agent-repl--readiness-parse json))
           (system (car (plist-get parsed :systems))))
      ;; Assert
      (should (null (agent-repl--readiness-field system 'commits_behind))))))

;;;; Receiving a run

(ert-deftest agent-repl-readiness-test-receive-stores-a-good-report ()
  "A zero exit with valid JSON becomes the current report."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (let ((json (agent-repl-test--readiness-json
                 (agent-repl-test--readiness-system "daemon" 0 0 t))))
      ;; Act
      (agent-repl--readiness-receive 0 json)
      ;; Assert
      (should (equal 1 (length (plist-get agent-repl--readiness :systems))))
      (should (null (plist-get agent-repl--readiness :error))))))

(ert-deftest agent-repl-readiness-test-receive-clears-the-inflight-process ()
  "Any completed run releases the coalescing interlock."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (setq agent-repl--readiness-process 'a-live-looking-process)
    ;; Act
    (agent-repl--readiness-receive 1 "")
    ;; Assert
    (should (null agent-repl--readiness-process))))

(ert-deftest agent-repl-readiness-test-receive-records-a-nonzero-exit ()
  "A nonzero exit is recorded as an error rather than parsed."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    ;; Act
    (agent-repl--readiness-receive 1 "not json at all")
    ;; Assert
    (should (string-match-p "exited 1" (plist-get agent-repl--readiness :error)))))

(ert-deftest agent-repl-readiness-test-receive-records-unparseable-output ()
  "A zero exit with malformed JSON is recorded, not signalled."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    ;; Act
    (agent-repl--readiness-receive 0 "{ this is not json")
    ;; Assert
    (should (string-match-p "unparseable" (plist-get agent-repl--readiness :error)))))

(ert-deftest agent-repl-readiness-test-receive-keeps-last-systems-on-failure ()
  "A failed run leaves the previous systems in place so the segment persists."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (agent-repl--readiness-receive
     0 (agent-repl-test--readiness-json
        (agent-repl-test--readiness-system "daemon" 0 0 t)))
    ;; Act
    (agent-repl--readiness-receive 2 "")
    ;; Assert
    (should (equal 1 (length (plist-get agent-repl--readiness :systems))))))

;;;; Polling

(ert-deftest agent-repl-readiness-test-poll-starts-a-run ()
  "A poll with nothing in flight invokes the script boundary once."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (let ((calls 0))
      (cl-letf (((symbol-function 'agent-repl--readiness-run-script)
                 (lambda (_cb) (setq calls (1+ calls)) 'a-process)))
        ;; Act
        (agent-repl--readiness-poll)
        ;; Assert
        (should (equal 1 calls))))))

(ert-deftest agent-repl-readiness-test-poll-skips-while-a-run-is-in-flight ()
  "An overlapping tick is skipped rather than queued."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (let ((calls 0))
      (cl-letf (((symbol-function 'agent-repl--readiness-run-script)
                 (lambda (_cb) (setq calls (1+ calls)) 'a-process))
                ((symbol-function 'process-live-p) (lambda (_p) t)))
        (setq agent-repl--readiness-process 'a-process)
        ;; Act
        (agent-repl--readiness-poll)
        ;; Assert
        (should (equal 0 calls))))))

(ert-deftest agent-repl-readiness-test-poll-does-nothing-when-disabled ()
  "Disabling the feature stops the poll from shelling out at all."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (let ((calls 0)
          (agent-repl-readiness-enabled nil))
      (cl-letf (((symbol-function 'agent-repl--readiness-run-script)
                 (lambda (_cb) (setq calls (1+ calls)) 'a-process)))
        ;; Act
        (agent-repl--readiness-poll)
        ;; Assert
        (should (equal 0 calls))))))

(ert-deftest agent-repl-readiness-test-poll-records-a-failed-start ()
  "A boundary that signals is recorded, and never escapes to the timer."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (cl-letf (((symbol-function 'agent-repl--readiness-run-script)
               (lambda (_cb) (error "no bash here"))))
      ;; Act
      (agent-repl--readiness-poll)
      ;; Assert
      (should (string-match-p "could not start"
                              (plist-get agent-repl--readiness :error))))))

(ert-deftest agent-repl-readiness-test-poll-clears-process-after-a-failed-start ()
  "A failed start must not leave the interlock latched shut forever."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (cl-letf (((symbol-function 'agent-repl--readiness-run-script)
               (lambda (_cb) (error "no bash here"))))
      ;; Act
      (agent-repl--readiness-poll)
      ;; Assert
      (should (null agent-repl--readiness-process)))))

;;;; Timer lifecycle

(ert-deftest agent-repl-readiness-test-start-timer-registers-one-timer ()
  "Starting the poll timer registers exactly one timer."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    ;; Act
    (unwind-protect
        (progn
          (agent-repl--readiness-start-timer)
          ;; Assert
          (should (timerp agent-repl--readiness-timer))
          (should (equal 1 (length agent-repl--timers))))
      (agent-repl--readiness-cancel-timer))))

(ert-deftest agent-repl-readiness-test-start-timer-replaces-the-previous-one ()
  "A second start cancels the first, so there is never more than one."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (unwind-protect
        (let (first)
          (agent-repl--readiness-start-timer)
          (setq first agent-repl--readiness-timer)
          ;; Act
          (agent-repl--readiness-start-timer)
          ;; Assert
          (should-not (eq first agent-repl--readiness-timer))
          (should (equal 1 (length agent-repl--timers))))
      (agent-repl--readiness-cancel-timer))))

(ert-deftest agent-repl-readiness-test-cancel-timer-deregisters-it ()
  "Cancelling removes the timer from the module's timer registry."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (agent-repl--readiness-start-timer)
    ;; Act
    (agent-repl--readiness-cancel-timer)
    ;; Assert
    (should (null agent-repl--readiness-timer))
    (should (null agent-repl--timers))))

;;;; Segment rendering

(defun agent-repl-test--readiness-render (&rest systems)
  "Return the segment string for a report built from SYSTEMS."
  (agent-repl--readiness-receive 0 (apply #'agent-repl-test--readiness-json systems))
  (substring-no-properties (agent-repl--readiness-segment)))

(ert-deftest agent-repl-readiness-test-segment-is-ellipsis-before-first-poll ()
  "Before any report lands the segment says \"unknown\", not \"fine\"."
  ;; Arrange / Act / Assert
  (agent-repl-test--with-readiness-state
    (should (equal " …" (substring-no-properties (agent-repl--readiness-segment))))))

(ert-deftest agent-repl-readiness-test-segment-marks-a-ready-system ()
  "A ready system renders as its abbreviation plus a check."
  ;; Arrange / Act
  (agent-repl-test--with-readiness-state
    (let ((s (agent-repl-test--readiness-render
              (agent-repl-test--readiness-system "daemon" 0 0 t))))
      ;; Assert
      (should (equal " D✓" s)))))

(ert-deftest agent-repl-readiness-test-segment-shows-the-behind-count ()
  "A system behind master renders the number of commits it lacks."
  ;; Arrange / Act
  (agent-repl-test--with-readiness-state
    (let ((s (agent-repl-test--readiness-render
              (agent-repl-test--readiness-system "webapp" 4 90 nil))))
      ;; Assert
      (should (equal " W↓4" s)))))

(ert-deftest agent-repl-readiness-test-segment-marks-a-stale-running-binary ()
  "A stale running process gets its own marker, distinct from behind."
  ;; Arrange / Act
  (agent-repl-test--with-readiness-state
    (let ((s (agent-repl-test--readiness-render
              (agent-repl-test--readiness-system "daemon" 0 0 nil 42 t))))
      ;; Assert
      (should (equal " D↯" s)))))

(ert-deftest agent-repl-readiness-test-segment-stale-outranks-behind ()
  "A system both behind and stale shows the stale marker with the count."
  ;; Arrange / Act
  (agent-repl-test--with-readiness-state
    (let ((s (agent-repl-test--readiness-render
              (agent-repl-test--readiness-system "shim-store" 2 30 nil 7 t))))
      ;; Assert
      (should (equal " St↯2" s)))))

(ert-deftest agent-repl-readiness-test-segment-marks-an-unknown-system ()
  "A system with no distance and no verdict renders as unknown."
  ;; Arrange / Act
  (agent-repl-test--with-readiness-state
    (let ((s (agent-repl-test--readiness-render
              (agent-repl-test--readiness-system "shim" nil nil nil nil nil
                                                 "no stamp"))))
      ;; Assert
      (should (equal " S?" s)))))

(ert-deftest agent-repl-readiness-test-segment-joins-every-system ()
  "Every system gets one cell, in report order."
  ;; Arrange / Act
  (agent-repl-test--with-readiness-state
    (let ((s (agent-repl-test--readiness-render
              (agent-repl-test--readiness-system "daemon" 0 0 t)
              (agent-repl-test--readiness-system "webapp" 1 10 nil)
              (agent-repl-test--readiness-system "shim-claude-sidecar" 0 0 t))))
      ;; Assert
      (should (equal " D✓ W↓1 Sc✓" s)))))

(ert-deftest agent-repl-readiness-test-segment-marks-a-stale-report ()
  "Cells from a report whose latest refresh failed carry a trailing marker."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (agent-repl--readiness-receive
     0 (agent-repl-test--readiness-json
        (agent-repl-test--readiness-system "daemon" 0 0 t)))
    ;; Act
    (agent-repl--readiness-receive 1 "")
    ;; Assert
    (should (equal " D✓!" (substring-no-properties (agent-repl--readiness-segment))))))

(ert-deftest agent-repl-readiness-test-segment-degrades-when-nothing-parsed ()
  "A failure with no previous report renders a dim placeholder, not a crash."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    ;; Act
    (agent-repl--readiness-receive 1 "")
    ;; Assert
    (should (equal " rdy?" (substring-no-properties (agent-repl--readiness-segment))))))

(ert-deftest agent-repl-readiness-test-segment-is-empty-when-disabled ()
  "Disabling the feature empties the segment entirely."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (let ((agent-repl-readiness-enabled nil))
      ;; Act / Assert
      (should (equal "" (agent-repl--readiness-segment))))))

(ert-deftest agent-repl-readiness-test-cell-carries-a-face ()
  "Each cell is propertized so the modeline colors it."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (agent-repl--readiness-receive
     0 (agent-repl-test--readiness-json
        (agent-repl-test--readiness-system "daemon" 0 0 nil 42 t)))
    ;; Act
    (let ((s (agent-repl--readiness-segment)))
      ;; Assert
      (should (eq 'agent-repl-readiness-stale (get-text-property 1 'face s))))))

(ert-deftest agent-repl-readiness-test-cell-carries-a-tooltip ()
  "Each cell explains itself on hover."
  ;; Arrange
  (agent-repl-test--with-readiness-state
    (agent-repl--readiness-receive
     0 (agent-repl-test--readiness-json
        (agent-repl-test--readiness-system "webapp" 3 45 nil)))
    ;; Act
    (let ((echo (get-text-property 1 'help-echo (agent-repl--readiness-segment))))
      ;; Assert
      (should (string-match-p "3 commit(s) behind" echo)))))

(ert-deftest agent-repl-readiness-test-abbrev-falls-back-for-a-new-system ()
  "A system the abbreviation table does not know still renders."
  ;; Arrange / Act / Assert
  (should (equal "z" (agent-repl--readiness-abbrev "zebra"))))

;;;; Modeline attachment

(ert-deftest agent-repl-readiness-test-attach-appends-the-segment ()
  "Attaching adds the segment to the buffer's own modeline."
  ;; Arrange
  (let ((buf (generate-new-buffer " *readiness-attach*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mode-line-format '("base"))
          ;; Act
          (agent-repl--readiness-attach-to-mode-line buf)
          ;; Assert
          (should (member agent-repl--readiness-mode-line-spec mode-line-format)))
      (kill-buffer buf))))

(ert-deftest agent-repl-readiness-test-attach-is-idempotent ()
  "Attaching twice leaves exactly one segment."
  ;; Arrange
  (let ((buf (generate-new-buffer " *readiness-attach-twice*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mode-line-format '("base"))
          (agent-repl--readiness-attach-to-mode-line buf)
          ;; Act
          (agent-repl--readiness-attach-to-mode-line buf)
          ;; Assert
          (should (equal 2 (length mode-line-format))))
      (kill-buffer buf))))

(ert-deftest agent-repl-readiness-test-attach-skips-a-nonlist-modeline ()
  "A buffer whose modeline is not a list is left alone."
  ;; Arrange
  (let ((buf (generate-new-buffer " *readiness-attach-string*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mode-line-format "a plain string")
          ;; Act
          (agent-repl--readiness-attach-to-mode-line buf)
          ;; Assert
          (should (equal "a plain string" mode-line-format)))
      (kill-buffer buf))))

(ert-deftest agent-repl-readiness-test-attach-tolerates-a-dead-buffer ()
  "Attaching to a killed buffer is a no-op, not an error."
  ;; Arrange
  (let ((buf (generate-new-buffer " *readiness-attach-dead*")))
    (kill-buffer buf)
    ;; Act / Assert
    (should-not (agent-repl--readiness-attach-to-mode-line buf))))

(ert-deftest agent-repl-readiness-test-adopt-hook-attaches-to-the-webview ()
  "The webview adopt hook is what puts the segment on the OUTPUT buffer."
  ;; Arrange
  (let ((buf (generate-new-buffer " *readiness-adopt*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mode-line-format '("base"))
          ;; Act
          (run-hooks 'agent-repl-frontend-webview-adopt-hook)
          ;; Assert
          (should (member agent-repl--readiness-mode-line-spec mode-line-format)))
      (kill-buffer buf))))

(provide 'test-readiness)
;;; test-readiness.el ends here
