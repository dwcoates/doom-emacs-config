;;; test-events.el --- ERT tests for events.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the lightweight workspace create/merge event log.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-events.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Helpers ----

(defmacro agent-repl-events-test--with-fresh-store (&rest body)
  "Run BODY with a fresh on-disk events file and cleared in-memory cache."
  (declare (indent 0))
  `(let ((agent-repl-events-file
          (expand-file-name (format "claude-events-%s.el" (random))
                            temporary-file-directory))
         (agent-repl--events-cache nil)
         (agent-repl--events-cache-loaded nil))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p agent-repl-events-file)
         (delete-file agent-repl-events-file)))))

;;;; ---- Recording ----

(ert-deftest agent-repl-events-test-record-create ()
  "`--events-record' captures kind, ws, and a timestamp."
  (agent-repl-events-test--with-fresh-store
    (let ((t0 (- (float-time) 100)))
      (agent-repl--events-record "ws1" :create t0)
      (let ((events (agent-repl--events-recent)))
        (should (= 1 (length events)))
        (should (eq :create (plist-get (car events) :kind)))
        (should (equal "ws1" (plist-get (car events) :ws)))
        (should (= t0 (plist-get (car events) :time)))))))

(ert-deftest agent-repl-events-test-record-merge ()
  "`--events-record' supports :merge kind."
  (agent-repl-events-test--with-fresh-store
    (agent-repl--events-record "ws1" :merge (- (float-time) 100))
    (let ((events (agent-repl--events-recent)))
      (should (eq :merge (plist-get (car events) :kind))))))

(ert-deftest agent-repl-events-test-record-persists-to-disk ()
  "Recorded events survive cache reload from disk."
  (agent-repl-events-test--with-fresh-store
    (agent-repl--events-record "ws1" :create (- (float-time) 100))
    (setq agent-repl--events-cache nil
          agent-repl--events-cache-loaded nil)
    (let ((events (agent-repl--events-recent)))
      (should (= 1 (length events)))
      (should (equal "ws1" (plist-get (car events) :ws))))))

;;;; ---- 24h pruning ----

(ert-deftest agent-repl-events-test-prune-drops-events-older-than-24h ()
  "Events older than 24h are dropped by `--events-recent'."
  (agent-repl-events-test--with-fresh-store
    (let ((now (float-time)))
      ;; Plant an old event directly into the cache to bypass save-time prune.
      (setq agent-repl--events-cache
            (list (list :kind :create :ws "old"
                        :time (- now (* 25 60 60)))
                  (list :kind :create :ws "fresh"
                        :time (- now (* 1 60 60))))
            agent-repl--events-cache-loaded t)
      (let ((events (agent-repl--events-recent now)))
        (should (= 1 (length events)))
        (should (equal "fresh" (plist-get (car events) :ws)))))))

(ert-deftest agent-repl-events-test-save-prunes-disk ()
  "`--events-save' prunes the on-disk file so it cannot grow unbounded."
  (agent-repl-events-test--with-fresh-store
    (let ((now (float-time)))
      ;; Plant an old event in the cache and trigger save via record.
      (setq agent-repl--events-cache
            (list (list :kind :create :ws "old"
                        :time (- now (* 25 60 60))))
            agent-repl--events-cache-loaded t)
      (agent-repl--events-record "fresh" :create (- now (* 1 60 60)))
      ;; Reload from disk and confirm old is gone.
      (setq agent-repl--events-cache nil
            agent-repl--events-cache-loaded nil)
      (let ((events (agent-repl--events-recent now)))
        (should (= 1 (length events)))
        (should (equal "fresh" (plist-get (car events) :ws)))))))

(ert-deftest agent-repl-events-test-prune-keeps-events-just-under-24h ()
  "Events just under the 24h cutoff are retained."
  (agent-repl-events-test--with-fresh-store
    (let ((now (float-time)))
      (setq agent-repl--events-cache
            (list (list :kind :create :ws "edge"
                        :time (- now (- (* 24 60 60) 10))))
            agent-repl--events-cache-loaded t)
      (let ((events (agent-repl--events-recent now)))
        (should (= 1 (length events)))))))

;;;; ---- Ordering ----

(ert-deftest agent-repl-events-test-recent-is-newest-first ()
  "`--events-recent' returns events sorted newest first."
  (agent-repl-events-test--with-fresh-store
    (let ((base (- (float-time) 1000)))
      (agent-repl--events-record "a" :create (+ base 100))
      (agent-repl--events-record "b" :create (+ base 200))
      (agent-repl--events-record "c" :create (+ base 150))
      (let* ((events (agent-repl--events-recent))
             (names (mapcar (lambda (e) (plist-get e :ws)) events)))
        (should (equal '("b" "c" "a") names))))))

;;;; ---- Counting ----

(ert-deftest agent-repl-events-test-count-by-kind ()
  "`--events-count-by-kind' tallies events by :kind."
  (agent-repl-events-test--with-fresh-store
    (let ((t0 (- (float-time) 100)))
      (agent-repl--events-record "a" :create t0)
      (agent-repl--events-record "b" :create t0)
      (agent-repl--events-record "c" :merge  t0)
      (let ((events (agent-repl--events-recent)))
        (should (= 2 (agent-repl--events-count-by-kind events :create)))
        (should (= 1 (agent-repl--events-count-by-kind events :merge)))))))

;;;; ---- Empty / missing file ----

(ert-deftest agent-repl-events-test-recent-empty-when-no-file ()
  "`--events-recent' returns nil when no file exists."
  (agent-repl-events-test--with-fresh-store
    (should (null (agent-repl--events-recent)))))

(ert-deftest agent-repl-events-test-malformed-file-resets-cache ()
  "A malformed events file is treated as empty rather than signaling."
  (agent-repl-events-test--with-fresh-store
    (let ((dir (file-name-directory agent-repl-events-file)))
      (when (and dir (not (file-directory-p dir)))
        (make-directory dir t)))
    (with-temp-file agent-repl-events-file
      (insert "not a sexp ("))
    (should (null (agent-repl--events-recent)))))

;;;; ---- Drawer summary ----

(ert-deftest agent-repl-events-test-drawer-events-header-renders ()
  "`--insert-events-header' renders a summary block when events exist."
  (agent-repl-events-test--with-fresh-store
    (agent-repl--events-record "ws1" :create (- (float-time) 100))
    (agent-repl--events-record "ws2" :merge  (- (float-time) 50))
    (with-temp-buffer
      (agent-repl-drawer--insert-events-header)
      (let ((s (buffer-string)))
        (should (string-match-p "Last 24h" s))
        (should (string-match-p "1 created" s))
        (should (string-match-p "1 merged" s))
        (should (string-match-p "ws1" s))
        (should (string-match-p "ws2" s))))))

(ert-deftest agent-repl-events-test-drawer-events-header-empty-when-no-events ()
  "`--insert-events-header' is a no-op when nothing is in the 24h window."
  (agent-repl-events-test--with-fresh-store
    (with-temp-buffer
      (agent-repl-drawer--insert-events-header)
      (should (string-empty-p (buffer-string))))))

(ert-deftest agent-repl-events-test-drawer-events-header-excludes-old ()
  "Events older than 24h are not surfaced in the drawer header."
  (agent-repl-events-test--with-fresh-store
    ;; Plant directly into cache so save-time prune doesn't wipe it first.
    (setq agent-repl--events-cache
          (list (list :kind :create :ws "ancient"
                      :time (- (float-time) (* 25 60 60))))
          agent-repl--events-cache-loaded t)
    (with-temp-buffer
      (agent-repl-drawer--insert-events-header)
      (should (string-empty-p (buffer-string))))))

(provide 'test-events)

;;; test-events.el ends here
