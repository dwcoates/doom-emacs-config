;;; test-stale-sink.el --- ERT tests for agent-repl stale-sink.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   bin/background.sh emacs -batch -Q -l ert -l lisp/test-stale-sink.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defvar agent-repl-test-stale-sink--asked nil "Every status ask, newest first, as its request plist.")
(defvar agent-repl-test-stale-sink--answer nil "How the stubbed rpc answers: (:response R), (:failure D) or nil for never.")
(defvar agent-repl-test-stale-sink--records nil "Every record, newest first, as (LEVEL TEXT).")
(defvar agent-repl-test-stale-sink--warnings nil "Every popup raised, newest first.")
(defvar agent-repl-test-stale-sink--link nil "What the stubbed `agent-repl-link-live' answers.")

(defconst agent-repl-test-stale-sink--ref '(:id "0123456789abcdef" :dir "/tmp/gone")
  "The ref the daemon named the workspace by.")

(defun agent-repl-test-stale-sink--standing (arm)
  "A daemon answer whose workspace stands ARM, `:open' or `:closed'."
  (list :response (list :arm :success :value (list :status (list :standing (list :arm arm :value nil))))))

(defmacro agent-repl-test-stale-sink--with (&rest body)
  "Run BODY with a live link, a recording rpc and captured records and popups."
  (declare (indent 0))
  `(let ((agent-repl-stale-sink--waiting nil)
         (agent-repl-test-stale-sink--asked nil)
         (agent-repl-test-stale-sink--records nil)
         (agent-repl-test-stale-sink--warnings nil)
         (agent-repl-test-stale-sink--answer (agent-repl-test-stale-sink--standing :closed))
         (agent-repl-test-stale-sink--link 'conn))
     (cl-letf (((symbol-function 'agent-repl-link-live) (lambda () agent-repl-test-stale-sink--link))
               ((symbol-function 'agent-repl--ws-get)
                (lambda (_ws key) (and (eq key :ref) agent-repl-test-stale-sink--ref)))
               ((symbol-function 'display-warning)
                (lambda (_type text &rest _) (push text agent-repl-test-stale-sink--warnings)))
               ((symbol-function 'agent-repl--emit-info)
                (lambda (_ws fmt &rest args) (push (list 'info (agent-repl-log-site-render fmt args)) agent-repl-test-stale-sink--records)))
               ((symbol-function 'agent-repl--emit-warn)
                (lambda (_ws fmt &rest args) (push (list 'warn (agent-repl-log-site-render fmt args)) agent-repl-test-stale-sink--records)))
               ((symbol-function 'agent-repl--emit-error)
                (lambda (_ws fmt &rest args) (push (list 'error (agent-repl-log-site-render fmt args)) agent-repl-test-stale-sink--records)))
               ((symbol-function 'agent-repl-rpc-get-workspace-status)
                (lambda (_conn request &rest keys)
                  (push request agent-repl-test-stale-sink--asked)
                  (pcase agent-repl-test-stale-sink--answer
                    (`(:response ,r) (funcall (plist-get keys :on-response) r))
                    (`(:failure ,d) (funcall (plist-get keys :on-failure) d))))))
       ,@body)))

(defun agent-repl-test-stale-sink--recorded-p (level pattern)
  "Non-nil when a LEVEL record matching PATTERN was written."
  (cl-some (lambda (r) (and (eq (car r) level) (string-match-p pattern (cadr r))))
           agent-repl-test-stale-sink--records))

(ert-deftest agent-repl-test-stale-sink-an-open-workspace-warns-with-a-popup ()
  "The daemon saying the workspace is open makes its gone directory a WARN and a popup."
  (agent-repl-test-stale-sink--with
    ;; Arrange
    (setq agent-repl-test-stale-sink--answer (agent-repl-test-stale-sink--standing :open))
    ;; Act
    (agent-repl-stale-sink-confirm "gone-ws" "/tmp/gone")
    ;; Assert
    (should (equal agent-repl-test-stale-sink--asked (list (list :workspace agent-repl-test-stale-sink--ref))))
    (should (agent-repl-test-stale-sink--recorded-p 'warn "open-without-directory workspace=\"gone-ws\""))
    (should (= (length agent-repl-test-stale-sink--warnings) 1))
    (should (string-match-p "gone-ws" (car agent-repl-test-stale-sink--warnings)))))

(ert-deftest agent-repl-test-stale-sink-a-closed-workspace-departed-quietly ()
  "The daemon saying the workspace is closed records its departure at INFO, no popup."
  (agent-repl-test-stale-sink--with
    ;; Act
    (agent-repl-stale-sink-confirm "gone-ws" "/tmp/gone")
    ;; Assert
    (should (agent-repl-test-stale-sink--recorded-p 'info "departed workspace=\"gone-ws\".*closed"))
    (should-not agent-repl-test-stale-sink--warnings)
    (should-not (cl-some (lambda (r) (memq (car r) '(warn error))) agent-repl-test-stale-sink--records))))

(ert-deftest agent-repl-test-stale-sink-an-unknown-workspace-departed-quietly ()
  "A workspace the daemon no longer knows departed: INFO, no popup."
  (agent-repl-test-stale-sink--with
    ;; Arrange
    (setq agent-repl-test-stale-sink--answer
          '(:response (:arm :error :value (:arm :unknown-workspace :value nil))))
    ;; Act
    (agent-repl-stale-sink-confirm "gone-ws" "/tmp/gone")
    ;; Assert
    (should (agent-repl-test-stale-sink--recorded-p 'info "departed workspace=\"gone-ws\".*no longer knows"))
    (should-not agent-repl-test-stale-sink--warnings)))

(ert-deftest agent-repl-test-stale-sink-a-refused-ask-is-an-error ()
  "A refusal of the ask leaves the gone directory unexplained: ERROR, no popup."
  (agent-repl-test-stale-sink--with
    ;; Arrange
    (setq agent-repl-test-stale-sink--answer
          '(:response (:arm :error :value (:arm :workspace-ref-mismatch :value (:registry-dir "/elsewhere")))))
    ;; Act
    (agent-repl-stale-sink-confirm "gone-ws" "/tmp/gone")
    ;; Assert
    (should (agent-repl-test-stale-sink--recorded-p 'error "refused workspace=\"gone-ws\".*workspace-ref-mismatch"))
    (should-not agent-repl-test-stale-sink--warnings)))

(ert-deftest agent-repl-test-stale-sink-a-failed-ask-is-an-error ()
  "A transport failure of the ask is recorded at ERROR, with no popup."
  (agent-repl-test-stale-sink--with
    ;; Arrange
    (setq agent-repl-test-stale-sink--answer '(:failure (:kind timeout)))
    ;; Act
    (agent-repl-stale-sink-confirm "gone-ws" "/tmp/gone")
    ;; Assert
    (should (agent-repl-test-stale-sink--recorded-p 'error "ask-failed workspace=\"gone-ws\""))
    (should-not agent-repl-test-stale-sink--warnings)))

(ert-deftest agent-repl-test-stale-sink-waits-for-the-link-then-asks ()
  "A workspace met before the link is up is asked once the link comes up."
  (agent-repl-test-stale-sink--with
    ;; Arrange
    (setq agent-repl-test-stale-sink--link nil)
    (agent-repl-stale-sink-confirm "gone-ws" "/tmp/gone")
    (should-not agent-repl-test-stale-sink--asked)
    ;; Act
    (agent-repl-stale-sink-flush 'conn)
    ;; Assert
    (should (= (length agent-repl-test-stale-sink--asked) 1))
    (should-not agent-repl-stale-sink--waiting)
    (should (agent-repl-test-stale-sink--recorded-p 'info "waiting workspace=\"gone-ws\""))))

(ert-deftest agent-repl-test-stale-sink-no-ref-is-never-asked ()
  "A workspace the daemon never named is not asked, and nothing is warned."
  (agent-repl-test-stale-sink--with
    ;; Act
    (cl-letf (((symbol-function 'agent-repl--ws-get) (lambda (_ws _key) nil)))
      (agent-repl-stale-sink-confirm "gone-ws" "/tmp/gone"))
    ;; Assert
    (should-not agent-repl-test-stale-sink--asked)
    (should (agent-repl-test-stale-sink--recorded-p 'info "no-ref workspace=\"gone-ws\""))
    (should-not agent-repl-test-stale-sink--warnings)))

(ert-deftest agent-repl-test-stale-sink-flushes-on-every-link-up ()
  "The flush is hung on the link's up edge."
  (should (memq #'agent-repl-stale-sink-flush agent-repl-link-up-functions)))

(provide 'test-stale-sink)
;;; test-stale-sink.el ends here
