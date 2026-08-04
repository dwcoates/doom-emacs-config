;;; test-context-cost.el --- ERT tests for context-cost.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the expensive-turn alarm: the one field of `ProgressView' the
;; Emacs frontend reads, and the red footer segment it paints.
;;
;; Nothing here opens a socket.  Frames are hand-built plists handed straight
;; to the registered handler, which is exactly what the dispatcher does with a
;; decoded `progress' arm.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-context-cost.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; Fixtures

(defmacro agent-repl-test--with-cost-state (&rest body)
  "Run BODY with the alarm hash empty and workspace state isolated."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (clrhash agent-repl--context-cost-alerts)
     (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--log-verbose) (lambda (&rest _) nil))
               ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil)))
       (unwind-protect (progn ,@body)
         (clrhash agent-repl--context-cost-alerts)))))

(defun agent-repl-test--cost-alert (&optional origin)
  "Return a `ContextCostAlert' plist, protojson-shaped, with ORIGIN.
int64 fields are STRINGS, which is how protojson renders them."
  (append (list :turnId "t_1"
                :uncachedInputTokens "45000"
                :thresholdTokens "20000"
                :atMs "1750000000000")
          (when origin (list :promptOrigin origin))))

(defun agent-repl-test--cost-progress (workspace &optional alert)
  "Return a `ProgressView' plist for WORKSPACE carrying ALERT, or none."
  (append (list :workspace workspace :sessionId "s_1")
          (when alert (list :expensiveTurn alert))))

;;;; ---- The wire arm is dispatched, not ignored ---------------------------

(ert-deftest agent-repl-test-context-cost-progress-is-a-known-frame ()
  "`progress' is a known frame arm, so a push decodes instead of signalling."
  (should (member "progress" agent-repl--uds-known-frame-fields)))

(ert-deftest agent-repl-test-context-cost-progress-is-no-longer-ignored ()
  "`progress' has a handler now, so it must not sit on the ignored list.
An arm on both lists would render its handler unreachable in review even
though dispatch prefers the handler — the lists must agree."
  (should-not (member "progress" agent-repl--uds-ignored-frame-fields)))

;;;; ---- Applying the frame ------------------------------------------------

(ert-deftest agent-repl-test-context-cost-apply-records-the-alert ()
  "A ProgressView carrying `expensiveTurn' records it under the workspace."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    ;; Act
    (agent-repl--context-cost-apply
     (agent-repl-test--cost-progress "/w" (agent-repl-test--cost-alert)))
    ;; Assert
    (should (gethash "ws1" agent-repl--context-cost-alerts))))

(ert-deftest agent-repl-test-context-cost-apply-clears-when-the-field-is-absent ()
  "A ProgressView with no `expensiveTurn' clears the workspace's alarm.
The daemon clears the field when the next turn starts, and the field's
presence IS the alarm's lifetime — there is no local timer to expire it."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl--context-cost-apply
     (agent-repl-test--cost-progress "/w" (agent-repl-test--cost-alert)))
    ;; Act
    (agent-repl--context-cost-apply (agent-repl-test--cost-progress "/w"))
    ;; Assert
    (should-not (gethash "ws1" agent-repl--context-cost-alerts))))

(ert-deftest agent-repl-test-context-cost-apply-drops-an-unresolvable-workspace ()
  "A cwd no workspace owns is dropped rather than keyed under the raw path.
A stub entry would hold an alarm no footer ever reads."
  ;; Arrange
  (agent-repl-test--with-cost-state
    ;; Act
    (agent-repl--context-cost-apply
     (agent-repl-test--cost-progress "/nobody" (agent-repl-test--cost-alert)))
    ;; Assert
    (should (zerop (hash-table-count agent-repl--context-cost-alerts)))))

(ert-deftest agent-repl-test-context-cost-apply-rejects-a-missing-token-count ()
  "An alert with no uncached count signals rather than rendering a placeholder.
A guessed count is a fabricated reading of a number the daemon either
sent or failed to send."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (let ((alert (agent-repl-test--cost-alert)))
      (setq alert (plist-put (copy-sequence alert) :uncachedInputTokens nil))
      ;; Act / Assert
      (should-error
       (agent-repl--context-cost-apply
        (agent-repl-test--cost-progress "/w" alert))))))

(ert-deftest agent-repl-test-context-cost-apply-rejects-a-missing-threshold ()
  "An alert with no threshold signals: the count alone has no reading.
The bar that tripped is daemon config the frontend must never supply for
itself."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (let ((alert (plist-put (copy-sequence (agent-repl-test--cost-alert))
                            :thresholdTokens nil)))
      ;; Act / Assert
      (should-error
       (agent-repl--context-cost-apply
        (agent-repl-test--cost-progress "/w" alert))))))

(ert-deftest agent-repl-test-context-cost-apply-logs-a-rejected-alert ()
  "A rejected alert reaches the canonical log with its workspace and turn."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (let (logged
          (alert (plist-put (copy-sequence (agent-repl-test--cost-alert))
                            :thresholdTokens nil)))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (ignore-errors
          (agent-repl--context-cost-apply
           (agent-repl-test--cost-progress "/w" alert)))
        ;; Assert
        (should (cl-some (lambda (l) (string-match-p "REJECTED ws=ws1 turn=t_1" l))
                         logged))))))

(ert-deftest agent-repl-test-context-cost-apply-records-nothing-when-rejected ()
  "A rejected alert leaves no partial entry behind."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (let ((alert (plist-put (copy-sequence (agent-repl-test--cost-alert))
                            :thresholdTokens nil)))
      ;; Act
      (ignore-errors
        (agent-repl--context-cost-apply
         (agent-repl-test--cost-progress "/w" alert)))
      ;; Assert
      (should (zerop (hash-table-count agent-repl--context-cost-alerts))))))

;;;; ---- The rendered message ----------------------------------------------

(ert-deftest agent-repl-test-context-cost-text-is-nil-without-an-alert ()
  "A workspace with no recorded alarm renders no text at all."
  (agent-repl-test--with-cost-state
    (should-not (agent-repl--context-cost-text "ws1"))))

(ert-deftest agent-repl-test-context-cost-text-names-the-uncached-count ()
  "The generic message reports the observed uncached token count."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl--context-cost-apply
     (agent-repl-test--cost-progress "/w" (agent-repl-test--cost-alert)))
    ;; Act / Assert
    (should (string-match-p "45000 uncached tokens"
                            (agent-repl--context-cost-text "ws1")))))

(ert-deftest agent-repl-test-context-cost-text-names-the-threshold ()
  "The generic message reports the threshold that tripped.
The count alone is unreadable without the bar it crossed, and the bar is
daemon config the frontend must not know on its own."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl--context-cost-apply
     (agent-repl-test--cost-progress "/w" (agent-repl-test--cost-alert)))
    ;; Act / Assert
    (should (string-match-p "threshold 20000"
                            (agent-repl--context-cost-text "ws1")))))

(ert-deftest agent-repl-test-context-cost-text-is-generic-without-an-origin ()
  "An alert with no keep-alive origin does NOT claim the cache went cold."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl--context-cost-apply
     (agent-repl-test--cost-progress "/w" (agent-repl-test--cost-alert)))
    ;; Act / Assert
    (should-not (string-match-p "keep-alive"
                                (agent-repl--context-cost-text "ws1")))))

(ert-deftest agent-repl-test-context-cost-text-calls-out-a-cold-keep-alive ()
  "A CACHE_KEEP_ALIVE origin gets its own wording, not the generic note.
The ping exists to keep the cache warm, so one that paid full freight is
reporting that the cache was ALREADY cold."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl--context-cost-apply
     (agent-repl-test--cost-progress
      "/w" (agent-repl-test--cost-alert "PROMPT_ORIGIN_CACHE_KEEP_ALIVE")))
    ;; Act / Assert
    (should (string-match-p "keep-alive hit a cold cache"
                            (agent-repl--context-cost-text "ws1")))))

(ert-deftest agent-repl-test-context-cost-text-reads-a-numeric-origin ()
  "A numerically-encoded enum reaches the keep-alive wording too.
Reading only the name would silently downgrade the loudest alarm here to
the generic message against a client that emits numbers."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl--context-cost-apply
     (agent-repl-test--cost-progress "/w" (agent-repl-test--cost-alert 29)))
    ;; Act / Assert
    (should (string-match-p "keep-alive hit a cold cache"
                            (agent-repl--context-cost-text "ws1")))))

;;;; ---- The footer segment ------------------------------------------------

(ert-deftest agent-repl-test-context-cost-segment-is-empty-for-an-unowned-buffer ()
  "A buffer owning no workspace paints nothing."
  (agent-repl-test--with-cost-state
    (agent-repl-test--with-temp-buffer "*not-ours*"
      (should (equal (agent-repl--context-cost-segment) "")))))

(ert-deftest agent-repl-test-context-cost-segment-is-empty-without-an-alarm ()
  "An owned buffer whose workspace has no alarm paints nothing.
An empty segment and \"everything is fine\" are the same fact here — the
alarm's absence has exactly one reading."
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl-test--with-temp-buffer "*agent-frontend-ws1*"
      (setq-local agent-repl--owning-workspace "ws1")
      (should (equal (agent-repl--context-cost-segment) "")))))

(ert-deftest agent-repl-test-context-cost-segment-paints-the-alarm ()
  "An owned buffer whose workspace has an alarm paints its text."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl--context-cost-apply
     (agent-repl-test--cost-progress "/w" (agent-repl-test--cost-alert)))
    (agent-repl-test--with-temp-buffer "*agent-frontend-ws1*"
      (setq-local agent-repl--owning-workspace "ws1")
      ;; Act / Assert
      (should (string-match-p "45000 uncached tokens"
                              (agent-repl--context-cost-segment))))))

(ert-deftest agent-repl-test-context-cost-segment-is-red ()
  "The alarm carries the module's red face, never a bare string."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w")
    (agent-repl--context-cost-apply
     (agent-repl-test--cost-progress "/w" (agent-repl-test--cost-alert)))
    (agent-repl-test--with-temp-buffer "*agent-frontend-ws1*"
      (setq-local agent-repl--owning-workspace "ws1")
      ;; Act
      (let ((segment (agent-repl--context-cost-segment)))
        ;; Assert — index 2 is the first character past the leading separator.
        (should (eq (get-text-property 2 'face segment)
                    'agent-repl-context-cost-alert))))))

(ert-deftest agent-repl-test-context-cost-segment-is-per-workspace ()
  "One workspace's alarm never appears over another workspace's output."
  ;; Arrange
  (agent-repl-test--with-cost-state
    (agent-repl--ws-put "ws1" :project-dir "/w1")
    (agent-repl--ws-put "ws2" :project-dir "/w2")
    (agent-repl--context-cost-apply
     (agent-repl-test--cost-progress "/w1" (agent-repl-test--cost-alert)))
    (agent-repl-test--with-temp-buffer "*agent-frontend-ws2*"
      (setq-local agent-repl--owning-workspace "ws2")
      ;; Act / Assert
      (should (equal (agent-repl--context-cost-segment) "")))))

;;;; ---- Footer attachment -------------------------------------------------

(ert-deftest agent-repl-test-context-cost-attach-adds-the-segment ()
  "Attaching appends the alarm segment to the buffer's mode line."
  (agent-repl-test--with-cost-state
    (agent-repl-test--with-temp-buffer "*agent-frontend-ws1*"
      (setq-local mode-line-format '("%b"))
      ;; Act
      (agent-repl--context-cost-attach-to-mode-line (current-buffer))
      ;; Assert
      (should (member agent-repl--context-cost-mode-line-spec mode-line-format)))))

(ert-deftest agent-repl-test-context-cost-attach-is-idempotent ()
  "A second attach does not add a second copy of the segment."
  (agent-repl-test--with-cost-state
    (agent-repl-test--with-temp-buffer "*agent-frontend-ws1*"
      (setq-local mode-line-format '("%b"))
      (agent-repl--context-cost-attach-to-mode-line (current-buffer))
      ;; Act
      (agent-repl--context-cost-attach-to-mode-line (current-buffer))
      ;; Assert
      (should (= 1 (cl-count agent-repl--context-cost-mode-line-spec
                             mode-line-format :test #'equal))))))

(provide 'test-context-cost)
;;; test-context-cost.el ends here
