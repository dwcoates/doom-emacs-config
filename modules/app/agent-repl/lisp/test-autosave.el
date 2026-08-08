;;; test-autosave.el --- ERT tests for agent-repl autosave -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for autosave.el -- periodic buffer autosave for agent-repl workspaces.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-autosave.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: agent-repl--save-buffer-if-modified ----

(ert-deftest agent-repl-test-autosave-dead-buffer ()
  "save-buffer-if-modified returns nil for a dead (killed) buffer."
  (let ((buf (generate-new-buffer " *test-dead*")))
    (kill-buffer buf)
    (should (null (agent-repl--save-buffer-if-modified buf)))))

(ert-deftest agent-repl-test-autosave-non-file-buffer ()
  "save-buffer-if-modified returns nil for a live buffer not visiting a file."
  (let ((buf (generate-new-buffer " *test-no-file*")))
    (unwind-protect
        (should (null (agent-repl--save-buffer-if-modified buf)))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-autosave-unmodified-file-buffer ()
  "save-buffer-if-modified returns nil for a file-visiting but unmodified buffer."
  (let* ((tmpfile (make-temp-file "autosave-test-"))
         (buf (find-file-noselect tmpfile)))
    (unwind-protect
        (progn
          ;; Buffer visits a file but is not modified
          (should (buffer-file-name buf))
          (should (not (buffer-modified-p buf)))
          (should (null (agent-repl--save-buffer-if-modified buf))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file tmpfile))))

(ert-deftest agent-repl-test-autosave-modified-file-buffer-saves ()
  "save-buffer-if-modified saves a modified file-visiting buffer and returns t."
  (let* ((tmpfile (make-temp-file "autosave-test-"))
         (buf (find-file-noselect tmpfile))
         (save-called nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "new content"))
          ;; Buffer is now modified
          (should (buffer-modified-p buf))
          ;; Stub save-buffer to verify it gets called silently
          (cl-letf (((symbol-function 'save-buffer)
                     (lambda (&rest _) (setq save-called t))))
            (should (eq t (agent-repl--save-buffer-if-modified buf)))
            (should save-called)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-file tmpfile))))

(ert-deftest agent-repl-test-autosave-save-is-silent ()
  "save-buffer-if-modified sets inhibit-message to t during save."
  (let* ((tmpfile (make-temp-file "autosave-test-"))
         (buf (find-file-noselect tmpfile))
         (captured-inhibit nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "content"))
          (cl-letf (((symbol-function 'save-buffer)
                     (lambda (&rest _) (setq captured-inhibit inhibit-message))))
            (agent-repl--save-buffer-if-modified buf)
            (should (eq t captured-inhibit))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-file tmpfile))))

(ert-deftest agent-repl-test-autosave-nil-buffer ()
  "save-buffer-if-modified handles nil gracefully (buffer-live-p returns nil)."
  (should (null (agent-repl--save-buffer-if-modified nil))))

(ert-deftest agent-repl-test-autosave-aggregate-suppresses-expected-buffer-records ()
  "A sweep delegates successful per-buffer evidence to its workspace summary."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/agent-repl-autosave-fixture")
    (insert "modified")
    (let (records)
      (cl-letf (((symbol-function 'save-buffer) #'ignore)
                ((symbol-function 'agent-repl--log-verbose)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) records))))
        (should (agent-repl--save-buffer-if-modified (current-buffer) "ws" t)))
      (should-not records))))

(ert-deftest agent-repl-test-autosave-aggregate-retains-save-failure ()
  "A sweep logs an identity-complete save failure before propagating it."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/agent-repl-autosave-failure")
    (insert "modified")
    (let (record)
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _) (error "simulated save failure")))
                ((symbol-function 'agent-repl--log)
                 (lambda (ws fmt &rest args)
                   (setq record (list ws (apply #'format fmt args))))))
        (should-error
         (agent-repl--save-buffer-if-modified (current-buffer) "ws" t)
         :type 'error))
      (should (equal (car record) "ws"))
      (should (string-match-p "outcome=save-failed" (cadr record)))
      (should (string-match-p "agent-repl-autosave-failure" (cadr record)))
      (should (string-match-p "simulated save failure" (cadr record))))))

;;;; ---- Tests: agent-repl--autosave-workspace-buffers ----

(ert-deftest agent-repl-test-autosave-workspace-noop-when-persp-disabled ()
  "autosave-workspace-buffers does nothing when persp-mode is nil."
  (let ((persp-mode nil)
        (persp-persps-called nil))
    (cl-letf (((symbol-function 'persp-persps)
               (lambda () (setq persp-persps-called t) nil)))
      (agent-repl--autosave-workspace-buffers)
      (should (null persp-persps-called)))))

(ert-deftest agent-repl-test-autosave-workspace-no-perspectives ()
  "autosave-workspace-buffers handles empty perspective list."
  (let ((persp-mode t)
        (log-messages nil))
    (cl-letf (((symbol-function 'persp-persps) (lambda () nil))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) log-messages))))
      (agent-repl--autosave-workspace-buffers)
      ;; No buffers saved, so no log message
      (should (null log-messages)))))

(ert-deftest agent-repl-test-autosave-workspace-nil-persp-silent ()
  "autosave-workspace-buffers skips nil entries silently (persp-mode no-persp container)."
  (let ((persp-mode t)
        (warnings nil))
    (cl-letf (((symbol-function 'persp-persps) (lambda () '(nil)))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) warnings))))
      (agent-repl--autosave-workspace-buffers)
      (should-not (cl-some (lambda (w) (string-match-p "non-perspective entry" w))
                           warnings)))))

(ert-deftest agent-repl-test-autosave-workspace-logs-bounded-perspective-summary ()
  "A sweep records causal counts without serializing perspective contents."
  (let* ((secret "SECRET-PERSPECTIVE-PAYLOAD")
         (persp (list :name "ws" :window-state (make-string 10000 ?x) secret))
         (buffers '(modified-saved modified-unsaved unmodified))
         verbose-records
         normal-records
         aggregate-flags)
    (cl-letf (((symbol-function 'agent-repl--ws-system-available-p) (lambda () t))
              ((symbol-function 'agent-repl--ws-all-persps) (lambda () (list persp)))
              ((symbol-function 'agent-repl--ws-persp-name) (lambda (_persp) "ws"))
              ((symbol-function 'agent-repl--ws-persp-identity)
               (lambda (_persp) "persp@abc123"))
              ((symbol-function 'agent-repl--ws-buffers) (lambda (_persp) buffers))
              ((symbol-function 'agent-repl--autosave-buffer-modified-p)
               (lambda (buf) (memq buf '(modified-saved modified-unsaved))))
              ((symbol-function 'agent-repl--save-buffer-if-modified)
               (lambda (buf _ws aggregate-p)
                 (push aggregate-p aggregate-flags)
                 (eq buf 'modified-saved)))
              ((symbol-function 'agent-repl--log-verbose)
               (lambda (ws fmt &rest args)
                 (push (list ws fmt args (apply #'format fmt args))
                       verbose-records)))
              ((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args)
                 (push (list ws (apply #'format fmt args)) normal-records))))
      (agent-repl--autosave-workspace-buffers))
    (let* ((workspace-record
            (cl-find-if (lambda (record)
                          (string-match-p "outcome=workspace-complete"
                                          (nth 3 record)))
                        verbose-records))
           (message (nth 3 workspace-record))
           (jsonl (agent-repl--log-record
                   nil "debug" "verbose"
                   (nth 1 workspace-record) (nth 2 workspace-record)))
           (all-output (prin1-to-string (list verbose-records normal-records))))
      (should (equal (car workspace-record) "ws"))
      (should (equal message
                     "autosave-workspace-buffers: outcome=workspace-complete workspace-name=ws perspective-identity=persp@abc123 buffer-count=3 modified-count=2 saved-count=1"))
      (should (< (string-bytes message) 512))
      (should (< (string-bytes jsonl) 1024))
      (should-not (string-match-p secret all-output))
      (should-not (string-match-p secret jsonl))
      (should-not (string-match-p "#s(perspective" all-output))
      (should-not (string-match-p "#s(perspective" jsonl))
      (should (equal aggregate-flags '(t t t))))))

(ert-deftest agent-repl-test-autosave-workspace-rejects-missing-name-before-save ()
  "An unnamed perspective logs and aborts without partial buffer mutation."
  (let ((persp (list :name nil))
        (save-count 0)
        record)
    (cl-letf (((symbol-function 'agent-repl--ws-system-available-p) (lambda () t))
              ((symbol-function 'agent-repl--ws-all-persps) (lambda () (list persp)))
              ((symbol-function 'agent-repl--ws-persp-name) (lambda (_persp) nil))
              ((symbol-function 'agent-repl--ws-persp-identity)
               (lambda (_persp) "persp@missing"))
              ((symbol-function 'agent-repl--ws-buffers) (lambda (_persp) '(buffer)))
              ((symbol-function 'agent-repl--autosave-buffer-modified-p) (lambda (_) t))
              ((symbol-function 'agent-repl--save-buffer-if-modified)
               (lambda (&rest _) (cl-incf save-count)))
              ((symbol-function 'agent-repl--log-verbose) #'ignore)
              ((symbol-function 'agent-repl--log)
               (lambda (ws fmt &rest args)
                 (setq record (list ws (apply #'format fmt args))))))
      (should-error (agent-repl--autosave-workspace-buffers) :type 'error))
    (should (= save-count 0))
    (should (equal (car record) nil))
    (should (string-match-p "perspective-identity=persp@missing" (cadr record)))
    (should (string-match-p "reason=invalid-workspace-name" (cadr record)))))

(ert-deftest agent-repl-test-autosave-workspace-symbol-persp-non-nil ()
  "autosave-workspace-buffers warns for non-nil symbol perspective entries."
  (let ((persp-mode t)
        (warnings nil))
    (cl-letf (((symbol-function 'persp-persps) (lambda () '(some-symbol)))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) warnings))))
      (agent-repl--autosave-workspace-buffers)
      (should (cl-some (lambda (w) (string-match-p "non-perspective entry" w))
                       warnings)))))

(ert-deftest agent-repl-test-autosave-workspace-saves-modified-buffers ()
  "autosave-workspace-buffers saves modified file-visiting buffers and logs count."
  (let* ((tmpfile1 (make-temp-file "autosave-ws-test-"))
         (tmpfile2 (make-temp-file "autosave-ws-test-"))
         (buf1 (find-file-noselect tmpfile1))
         (buf2 (find-file-noselect tmpfile2))
         (persp-mode t)
         (fake-persp (list :name "test"))
         (save-count 0)
         (log-messages nil))
    (unwind-protect
        (progn
          ;; Make both buffers modified
          (with-current-buffer buf1 (insert "mod1"))
          (with-current-buffer buf2 (insert "mod2"))
          (cl-letf (((symbol-function 'persp-persps) (lambda () (list fake-persp)))
                    ((symbol-function 'agent-repl--ws-persp-name)
                     (lambda (persp) (plist-get persp :name)))
                    ((symbol-function 'persp-buffers) (lambda (_p) (list buf1 buf2)))
                    ((symbol-function 'save-buffer)
                     (lambda (&rest _) (cl-incf save-count)))
                    ((symbol-function 'agent-repl--log)
                     (lambda (_ws fmt &rest args)
                       (push (apply #'format fmt args) log-messages))))
            (agent-repl--autosave-workspace-buffers)
            (should (= 2 save-count))
            ;; Should log "autosave: saved 2 buffer(s)"
            (should (cl-some (lambda (m) (string-match-p "saved 2 buffer" m))
                             log-messages))))
      (dolist (b (list buf1 buf2))
        (when (buffer-live-p b)
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b)))
      (delete-file tmpfile1)
      (delete-file tmpfile2))))

(ert-deftest agent-repl-test-autosave-workspace-skips-unmodified ()
  "autosave-workspace-buffers skips unmodified buffers; no log when 0 saved."
  (let* ((tmpfile (make-temp-file "autosave-ws-test-"))
         (buf (find-file-noselect tmpfile))
         (persp-mode t)
         (fake-persp (list :name "test"))
         (log-messages nil))
    (unwind-protect
        (progn
          ;; buf is not modified
          (should (not (buffer-modified-p buf)))
          (cl-letf (((symbol-function 'persp-persps) (lambda () (list fake-persp)))
                    ((symbol-function 'agent-repl--ws-persp-name)
                     (lambda (persp) (plist-get persp :name)))
                    ((symbol-function 'persp-buffers) (lambda (_p) (list buf)))
                    ((symbol-function 'agent-repl--log)
                     (lambda (_ws fmt &rest args)
                       (push (apply #'format fmt args) log-messages))))
            (agent-repl--autosave-workspace-buffers)
            ;; No log when saved count is 0
            (should (null log-messages))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file tmpfile))))

(ert-deftest agent-repl-test-autosave-workspace-mixed-buffers ()
  "autosave-workspace-buffers correctly counts only saved buffers in a mixed set."
  (let* ((tmpfile (make-temp-file "autosave-ws-test-"))
         (file-buf (find-file-noselect tmpfile))
         (non-file-buf (generate-new-buffer " *test-non-file*"))
         (persp-mode t)
         (fake-persp (list :name "test"))
         (save-count 0)
         (log-messages nil))
    (unwind-protect
        (progn
          ;; Only file-buf is modified and file-visiting
          (with-current-buffer file-buf (insert "modified"))
          (cl-letf (((symbol-function 'persp-persps) (lambda () (list fake-persp)))
                    ((symbol-function 'agent-repl--ws-persp-name)
                     (lambda (persp) (plist-get persp :name)))
                    ((symbol-function 'persp-buffers)
                     (lambda (_p) (list file-buf non-file-buf)))
                    ((symbol-function 'save-buffer)
                     (lambda (&rest _) (cl-incf save-count)))
                    ((symbol-function 'agent-repl--log)
                     (lambda (_ws fmt &rest args)
                       (push (apply #'format fmt args) log-messages))))
            (agent-repl--autosave-workspace-buffers)
            (should (= 1 save-count))
            (should (cl-some (lambda (m) (string-match-p "saved 1 buffer" m))
                             log-messages))))
      (dolist (b (list file-buf non-file-buf))
        (when (buffer-live-p b)
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b)))
      (delete-file tmpfile))))

(ert-deftest agent-repl-test-autosave-workspace-multiple-perspectives ()
  "autosave-workspace-buffers iterates across multiple perspectives."
  (let* ((tmpfile1 (make-temp-file "autosave-ws-test-"))
         (tmpfile2 (make-temp-file "autosave-ws-test-"))
         (buf1 (find-file-noselect tmpfile1))
         (buf2 (find-file-noselect tmpfile2))
         (persp-mode t)
         (persp-a (list :name "ws-a"))
         (persp-b (list :name "ws-b"))
         (save-count 0)
         (log-messages nil))
    (unwind-protect
        (progn
          (with-current-buffer buf1 (insert "mod"))
          (with-current-buffer buf2 (insert "mod"))
          (cl-letf (((symbol-function 'persp-persps)
                     (lambda () (list persp-a persp-b)))
                    ((symbol-function 'agent-repl--ws-persp-name)
                     (lambda (persp) (plist-get persp :name)))
                    ((symbol-function 'persp-buffers)
                     (lambda (p)
                       (cond ((eq p persp-a) (list buf1))
                             ((eq p persp-b) (list buf2))
                             (t nil))))
                    ((symbol-function 'save-buffer)
                     (lambda (&rest _) (cl-incf save-count)))
                    ((symbol-function 'agent-repl--log)
                     (lambda (_ws fmt &rest args)
                       (push (apply #'format fmt args) log-messages))))
            (agent-repl--autosave-workspace-buffers)
            (should (= 2 save-count))
            (should (cl-some (lambda (m) (string-match-p "saved 2 buffer" m))
                             log-messages))))
      (dolist (b (list buf1 buf2))
        (when (buffer-live-p b)
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b)))
      (delete-file tmpfile1)
      (delete-file tmpfile2))))

(ert-deftest agent-repl-test-autosave-workspace-dead-buffer-in-persp ()
  "autosave-workspace-buffers skips dead buffers within a perspective."
  (let* ((buf (generate-new-buffer " *test-dead-in-persp*"))
         (persp-mode t)
         (fake-persp (list :name "test"))
         (save-count 0))
    ;; Kill the buffer before autosave runs
    (kill-buffer buf)
    (cl-letf (((symbol-function 'persp-persps) (lambda () (list fake-persp)))
              ((symbol-function 'agent-repl--ws-persp-name)
               (lambda (persp) (plist-get persp :name)))
              ((symbol-function 'persp-buffers) (lambda (_p) (list buf)))
              ((symbol-function 'save-buffer)
               (lambda (&rest _) (cl-incf save-count))))
      (agent-repl--autosave-workspace-buffers)
      (should (= 0 save-count)))))

(ert-deftest agent-repl-test-autosave-workspace-empty-persp-buffers ()
  "autosave-workspace-buffers handles a perspective with no buffers."
  (let ((persp-mode t)
        (fake-persp (list :name "empty-ws"))
        (log-messages nil))
    (cl-letf (((symbol-function 'persp-persps) (lambda () (list fake-persp)))
              ((symbol-function 'agent-repl--ws-persp-name)
               (lambda (persp) (plist-get persp :name)))
              ((symbol-function 'persp-buffers) (lambda (_p) nil))
              ((symbol-function 'agent-repl--log)
               (lambda (_ws fmt &rest args)
                 (push (apply #'format fmt args) log-messages))))
      (agent-repl--autosave-workspace-buffers)
      (should (null log-messages)))))

(provide 'test-autosave)

;;; test-autosave.el ends here
