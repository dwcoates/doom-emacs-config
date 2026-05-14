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

(defmacro claude-repl-events-test--with-fresh-store (&rest body)
  "Run BODY with a fresh on-disk events file and cleared in-memory cache."
  (declare (indent 0))
  `(let ((claude-repl-events-file
          (expand-file-name (format "claude-events-%s.el" (random))
                            temporary-file-directory))
         (claude-repl--events-cache nil)
         (claude-repl--events-cache-loaded nil))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p claude-repl-events-file)
         (delete-file claude-repl-events-file)))))

;;;; ---- Recording ----

(ert-deftest claude-repl-events-test-record-create ()
  "`--events-record' captures kind, ws, and a timestamp."
  (claude-repl-events-test--with-fresh-store
    (let ((t0 (- (float-time) 100)))
      (claude-repl--events-record "ws1" :create t0)
      (let ((events (claude-repl--events-recent)))
        (should (= 1 (length events)))
        (should (eq :create (plist-get (car events) :kind)))
        (should (equal "ws1" (plist-get (car events) :ws)))
        (should (= t0 (plist-get (car events) :time)))))))

(ert-deftest claude-repl-events-test-record-merge ()
  "`--events-record' supports :merge kind."
  (claude-repl-events-test--with-fresh-store
    (claude-repl--events-record "ws1" :merge (- (float-time) 100))
    (let ((events (claude-repl--events-recent)))
      (should (eq :merge (plist-get (car events) :kind))))))

(ert-deftest claude-repl-events-test-record-persists-to-disk ()
  "Recorded events survive cache reload from disk."
  (claude-repl-events-test--with-fresh-store
    (claude-repl--events-record "ws1" :create (- (float-time) 100))
    (setq claude-repl--events-cache nil
          claude-repl--events-cache-loaded nil)
    (let ((events (claude-repl--events-recent)))
      (should (= 1 (length events)))
      (should (equal "ws1" (plist-get (car events) :ws))))))

;;;; ---- 24h pruning ----

(ert-deftest claude-repl-events-test-prune-drops-events-older-than-24h ()
  "Events older than 24h are dropped by `--events-recent'."
  (claude-repl-events-test--with-fresh-store
    (let ((now (float-time)))
      ;; Plant an old event directly into the cache to bypass save-time prune.
      (setq claude-repl--events-cache
            (list (list :kind :create :ws "old"
                        :time (- now (* 25 60 60)))
                  (list :kind :create :ws "fresh"
                        :time (- now (* 1 60 60))))
            claude-repl--events-cache-loaded t)
      (let ((events (claude-repl--events-recent now)))
        (should (= 1 (length events)))
        (should (equal "fresh" (plist-get (car events) :ws)))))))

(ert-deftest claude-repl-events-test-save-prunes-disk ()
  "`--events-save' prunes the on-disk file so it cannot grow unbounded."
  (claude-repl-events-test--with-fresh-store
    (let ((now (float-time)))
      ;; Plant an old event in the cache and trigger save via record.
      (setq claude-repl--events-cache
            (list (list :kind :create :ws "old"
                        :time (- now (* 25 60 60))))
            claude-repl--events-cache-loaded t)
      (claude-repl--events-record "fresh" :create (- now (* 1 60 60)))
      ;; Reload from disk and confirm old is gone.
      (setq claude-repl--events-cache nil
            claude-repl--events-cache-loaded nil)
      (let ((events (claude-repl--events-recent now)))
        (should (= 1 (length events)))
        (should (equal "fresh" (plist-get (car events) :ws)))))))

(ert-deftest claude-repl-events-test-prune-keeps-events-just-under-24h ()
  "Events just under the 24h cutoff are retained."
  (claude-repl-events-test--with-fresh-store
    (let ((now (float-time)))
      (setq claude-repl--events-cache
            (list (list :kind :create :ws "edge"
                        :time (- now (- (* 24 60 60) 10))))
            claude-repl--events-cache-loaded t)
      (let ((events (claude-repl--events-recent now)))
        (should (= 1 (length events)))))))

;;;; ---- Ordering ----

(ert-deftest claude-repl-events-test-recent-is-newest-first ()
  "`--events-recent' returns events sorted newest first."
  (claude-repl-events-test--with-fresh-store
    (let ((base (- (float-time) 1000)))
      (claude-repl--events-record "a" :create (+ base 100))
      (claude-repl--events-record "b" :create (+ base 200))
      (claude-repl--events-record "c" :create (+ base 150))
      (let* ((events (claude-repl--events-recent))
             (names (mapcar (lambda (e) (plist-get e :ws)) events)))
        (should (equal '("b" "c" "a") names))))))

;;;; ---- Counting ----

(ert-deftest claude-repl-events-test-count-by-kind ()
  "`--events-count-by-kind' tallies events by :kind."
  (claude-repl-events-test--with-fresh-store
    (let ((t0 (- (float-time) 100)))
      (claude-repl--events-record "a" :create t0)
      (claude-repl--events-record "b" :create t0)
      (claude-repl--events-record "c" :merge  t0)
      (let ((events (claude-repl--events-recent)))
        (should (= 2 (claude-repl--events-count-by-kind events :create)))
        (should (= 1 (claude-repl--events-count-by-kind events :merge)))))))

;;;; ---- Empty / missing file ----

(ert-deftest claude-repl-events-test-recent-empty-when-no-file ()
  "`--events-recent' returns nil when no file exists."
  (claude-repl-events-test--with-fresh-store
    (should (null (claude-repl--events-recent)))))

(ert-deftest claude-repl-events-test-malformed-file-resets-cache ()
  "A malformed events file is treated as empty rather than signaling."
  (claude-repl-events-test--with-fresh-store
    (let ((dir (file-name-directory claude-repl-events-file)))
      (when (and dir (not (file-directory-p dir)))
        (make-directory dir t)))
    (with-temp-file claude-repl-events-file
      (insert "not a sexp ("))
    (should (null (claude-repl--events-recent)))))

;;;; ---- Drawer summary ----

(ert-deftest claude-repl-events-test-drawer-events-header-renders ()
  "`--insert-events-header' renders a summary block when events exist."
  (claude-repl-events-test--with-fresh-store
    (claude-repl--events-record "ws1" :create (- (float-time) 100))
    (claude-repl--events-record "ws2" :merge  (- (float-time) 50))
    (with-temp-buffer
      (claude-repl-drawer--insert-events-header)
      (let ((s (buffer-string)))
        (should (string-match-p "Last 24h" s))
        (should (string-match-p "1 created" s))
        (should (string-match-p "1 merged" s))
        (should (string-match-p "ws1" s))
        (should (string-match-p "ws2" s))))))

(ert-deftest claude-repl-events-test-drawer-events-header-empty-when-no-events ()
  "`--insert-events-header' is a no-op when nothing is in the 24h window."
  (claude-repl-events-test--with-fresh-store
    (with-temp-buffer
      (claude-repl-drawer--insert-events-header)
      (should (string-empty-p (buffer-string))))))

(ert-deftest claude-repl-events-test-drawer-events-header-excludes-old ()
  "Events older than 24h are not surfaced in the drawer header."
  (claude-repl-events-test--with-fresh-store
    ;; Plant directly into cache so save-time prune doesn't wipe it first.
    (setq claude-repl--events-cache
          (list (list :kind :create :ws "ancient"
                      :time (- (float-time) (* 25 60 60))))
          claude-repl--events-cache-loaded t)
    (with-temp-buffer
      (claude-repl-drawer--insert-events-header)
      (should (string-empty-p (buffer-string))))))

(provide 'test-events)

;;; test-events.el ends here
