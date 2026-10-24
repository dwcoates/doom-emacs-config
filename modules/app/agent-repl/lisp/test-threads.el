;;; test-threads.el --- ERT tests for agent-repl threads.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-threads.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defvar agent-repl-test-threads--sent nil "CreateThread requests, newest first.")
(defvar agent-repl-test-threads--switched nil "Workspaces switched to, newest first.")
(defvar agent-repl-test-threads--records nil "Log records, newest first.")
(defvar agent-repl-test-threads--answer nil "What CreateThread answers: (:response R) or (:failure F).")

(defmacro agent-repl-test-threads--with (threads &rest body)
  "Run BODY with a roster carrying THREADS, each `(ID NAME WS)'.
WS is the workspace a thread is open in, nil while it is still opening."
  (declare (indent 1))
  `(let ((agent-repl-test-threads--sent nil)
         (agent-repl-test-threads--switched nil)
         (agent-repl-test-threads--records nil)
         (agent-repl-roster-view
          (list :threads (list :rows (mapcar (lambda (thread)
                                               (list :thread (list :id (nth 0 thread)
                                                                   :dir (concat "/z/" (nth 1 thread)))
                                                     :name (nth 1 thread)))
                                             ,threads)))))
     (cl-letf (((symbol-function 'agent-repl-link-primary) (lambda () 'conn))
               ((symbol-function 'agent-repl-rpc-create-thread)
                (cl-function
                 (lambda (_conn request &key on-response on-failure &allow-other-keys)
                   (push request agent-repl-test-threads--sent)
                   (pcase agent-repl-test-threads--answer
                     (`(:response ,r) (funcall on-response r))
                     (`(:failure ,f) (funcall on-failure f))))))
               ((symbol-function 'agent-repl--ws-by-ref-id)
                (lambda (id) (nth 2 (cl-find id ,threads :key #'car :test #'equal))))
               ((symbol-function 'agent-repl-host-request-switch)
                (lambda (ws trigger) (push (cons ws trigger) agent-repl-test-threads--switched)))
               ((symbol-function 'agent-repl--info)
                (lambda (_ fmt &rest args) (push (cons 'info (apply #'format fmt args)) agent-repl-test-threads--records)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_ fmt &rest args) (push (cons 'warn (apply #'format fmt args)) agent-repl-test-threads--records)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ fmt &rest args) (push (cons 'error (apply #'format fmt args)) agent-repl-test-threads--records))))
       ,@body)))

(defun agent-repl-test-threads--recorded (level prefix)
  "Whether a record at LEVEL starting with PREFIX was made."
  (cl-some (lambda (r) (and (eq (car r) level) (string-prefix-p prefix (cdr r))))
           agent-repl-test-threads--records))

(ert-deftest agent-repl-test-threads-a-new-thread-sends-its-question ()
  "The question read is sent as the thread's first question."
  (agent-repl-test-threads--with nil
    (let ((agent-repl-test-threads--answer
           '(:response (:arm :success :value (:thread (:id "t1" :dir "/z/why-blue"))))))
      (cl-letf (((symbol-function 'agent-repl-verbs--read-required-prompt)
                 (lambda (&rest _) "why is the sky blue?")))
        (agent-repl-thread-new))
      (should (equal agent-repl-test-threads--sent '((:question "why is the sky blue?"))))
      (should (agent-repl-test-threads--recorded 'info "elisp.threads.created id=t1 name=why-blue"))
      (should (null agent-repl-test-threads--switched)))))

(ert-deftest agent-repl-test-threads-a-refused-or-unsent-thread-is-reported ()
  "A refusal is recorded at WARN with its detail, a transport failure at ERROR."
  (dolist (case '(((:response (:arm :error :value (:cause (:arm :creation-failed :value (:detail "no name")))))
                   warn "elisp.threads.create-refused")
                  ((:failure (:code unavailable))
                   error "elisp.threads.create-failed")))
    (agent-repl-test-threads--with nil
      (let ((agent-repl-test-threads--answer (nth 0 case)))
        (agent-repl-threads--create "why?")
        (should (agent-repl-test-threads--recorded (nth 1 case) (nth 2 case)))))))

(ert-deftest agent-repl-test-threads-no-daemon-is-refused-loudly ()
  "With no daemon connected, nothing is sent and the user is told."
  (agent-repl-test-threads--with nil
    (cl-letf (((symbol-function 'agent-repl-link-primary) (lambda () nil)))
      (should-error (agent-repl-threads--create "why?") :type 'user-error)
      (should (null agent-repl-test-threads--sent))
      (should (agent-repl-test-threads--recorded 'warn "elisp.threads.create-unsent")))))

(ert-deftest agent-repl-test-threads-recent-opens-the-newest-thread ()
  "`SPC TAB T' opens the thread last interacted with."
  (agent-repl-test-threads--with '(("t2" "newer" "chat/newer") ("t1" "older" "chat/older"))
    (agent-repl-thread-recent)
    (should (equal agent-repl-test-threads--switched '(("chat/newer" . thread-recent))))))

(ert-deftest agent-repl-test-threads-a-thread-still-opening-is-not-switched-to ()
  "A thread whose workspace has not opened yet says so and moves nothing."
  (agent-repl-test-threads--with '(("t1" "why" nil))
    (agent-repl-thread-recent)
    (should (null agent-repl-test-threads--switched))
    (should (agent-repl-test-threads--recorded 'info "elisp.threads.open-pending id=t1"))))

(ert-deftest agent-repl-test-threads-pick-opens-the-named-thread ()
  "`SPC TAB .' opens the thread picked by name."
  (agent-repl-test-threads--with '(("t2" "newer" "chat/newer") ("t1" "older" "chat/older"))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "older")))
      (agent-repl-thread-pick))
    (should (equal agent-repl-test-threads--switched '(("chat/older" . thread-picker))))))

(ert-deftest agent-repl-test-threads-no-thread-yet-is-refused ()
  "With no thread at all, opening one is refused."
  (agent-repl-test-threads--with nil
    (should-error (agent-repl-thread-recent) :type 'user-error)
    (should-error (agent-repl-thread-pick) :type 'user-error)))

(ert-deftest agent-repl-test-threads-the-daemons-request-opens-the-prompt ()
  "The sidebar's button opens the new-thread prompt, off the process filter."
  (agent-repl-test-threads--with nil
    (let (opened)
      (cl-letf (((symbol-function 'run-at-time) (lambda (_ _ fn) (funcall fn)))
                ((symbol-function 'agent-repl--when-minibuffer-free)
                 (lambda (_scope _label _key fn) (funcall fn nil)))
                ((symbol-function 'call-interactively) (lambda (cmd) (setq opened cmd))))
        (agent-repl-thread-requested))
      (should (eq opened 'agent-repl-thread-new)))))

(provide 'test-threads)

;;; test-threads.el ends here
