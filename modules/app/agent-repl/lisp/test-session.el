;;; test-session.el --- Tests for session.el -*- lexical-binding: t; -*-

;;; Commentary:

;; What is left of sessions in Emacs: the two finish-edge reactions whose
;; knowledge only this process has (desktop focus, magit buffers) and the
;; model preferences a creation carries onto the wire.  The command
;; assembly, config-dir and permission-flag computation, display-state
;; save/load and session-id bookkeeping are the daemon's now, and their
;; tests went with them.
;;
;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-session.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Model preferences ----

(ert-deftest agent-repl-test-session-oneshot-candidates-are-a-non-empty-list ()
  "The one-shot picker has something to offer."
  ;; Act / Assert
  (should (and (listp agent-repl-oneshot-model-candidates)
               agent-repl-oneshot-model-candidates
               (cl-every #'stringp agent-repl-oneshot-model-candidates))))

;;;; ---- Reaction (1): the unfocused banner ----

(defmacro agent-repl-test-session--with-notifier (var focused &rest body)
  "Run BODY with the notifier counted onto VAR and focus stubbed to FOCUSED."
  (declare (indent 2))
  `(let ((,var 0))
     (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) ,focused))
               ((symbol-function 'run-at-time)
                (lambda (_delay _repeat _fn &rest _args) (cl-incf ,var))))
       ,@body)))

(ert-deftest agent-repl-test-session-notify-fires-when-emacs-is-unfocused ()
  "A banner is posted when the user is looking elsewhere."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-session--with-notifier posted nil
      ;; Act
      (agent-repl--maybe-notify-finished "ws1")
      ;; Assert
      (should (equal posted 1)))))

(ert-deftest agent-repl-test-session-notify-names-the-workspace-in-its-focus-check ()
  "The focus check is made ON THIS WORKSPACE'S behalf, so it names it.
Passing nil left the check's own record unattributed, and the routing rung
recorded `elisp.core.log-routing-error' at ERROR beside every finished
turn."
  ;; Arrange.
  (agent-repl-test--with-clean-state
    (let ((asked nil))
      (cl-letf (((symbol-function 'agent-repl--emacs-focused-p)
                 (lambda (&optional ws) (push ws asked) t)))
        ;; Act.
        (agent-repl--maybe-notify-finished "ws1")
        ;; Assert.
        (should (equal asked (list "ws1")))))))

(ert-deftest agent-repl-test-session-notify-is-silent-when-emacs-is-focused ()
  "A banner is useless when the user is already looking at Emacs."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-session--with-notifier posted t
      ;; Act
      (agent-repl--maybe-notify-finished "ws1")
      ;; Assert
      (should (equal posted 0)))))

(ert-deftest agent-repl-test-session-notify-debounces-within-the-window ()
  "Two banners for one turn is a defect the user would blame on the agent."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-session--with-notifier posted nil
      (agent-repl--maybe-notify-finished "ws1")
      ;; Act
      (agent-repl--maybe-notify-finished "ws1")
      ;; Assert
      (should (equal posted 1)))))

(ert-deftest agent-repl-test-session-notify-fires-again-after-the-window ()
  "The debounce is a window, not a latch."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-session--with-notifier posted nil
      (agent-repl--maybe-notify-finished "ws1")
      ;; Act — age the stamp past the window rather than sleeping through it.
      (agent-repl--ws-put "ws1" :last-notify-time
                          (- (float-time) (* 2 agent-repl-notify-debounce-seconds)))
      (agent-repl--maybe-notify-finished "ws1")
      ;; Assert
      (should (equal posted 2)))))

(ert-deftest agent-repl-test-session-notify-debounces-per-workspace ()
  "The debounce is keyed by workspace: another workspace still notifies."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-session--with-notifier posted nil
      (agent-repl--maybe-notify-finished "ws1")
      ;; Act
      (agent-repl--maybe-notify-finished "ws2")
      ;; Assert
      (should (equal posted 2)))))

(ert-deftest agent-repl-test-session-notify-banner-text-names-the-workspace-after-the-prefix ()
  "The banner body is exactly \"Agent ready: <name>\".
The text is contract, not taste: the roster integration suite pins it, and
a workspace name used as a bare prefix reads as a different notification."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((body nil))
      (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest args) (setq body (nth 2 args)))))
        ;; Act
        (agent-repl--maybe-notify-finished "ws1")
        ;; Assert
        (should (equal body "Agent ready: ws1"))))))

(ert-deftest agent-repl-test-session-notify-passes-no-explicit-activation ()
  "The finish-edge banner leaves the click to the backend's workspace default.
`agent-repl--notify' treats a nil ACTIVATE as \"activate WS yourself\", and
`--notify-backend-alerter' answers it by running
`agent-repl--notification-activate' on WS -- raising the frame and
selecting WS's tab, which is precisely what this banner's click means.
An explicit function here would REPLACE that default with a hand-rolled
copy, so the absence is the contract."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((args 'unset))
      (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest a) (setq args a))))
        ;; Act
        (agent-repl--maybe-notify-finished "ws1")
        ;; Assert -- (WS TITLE MESSAGE) and nothing in the ACTIVATE slot.
        (should (equal (length args) 3))))))

(ert-deftest agent-repl-test-session-notify-targets-the-finished-workspace ()
  "The banner is posted FOR the workspace whose turn finished."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((ws nil))
      (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat _fn &rest a) (setq ws (nth 0 a)))))
        ;; Act
        (agent-repl--maybe-notify-finished "ws-finished")
        ;; Assert
        (should (equal ws "ws-finished"))))))

;;;; ---- Reaction (3): the magit refresh ----

(defmacro agent-repl-test-session--with-magit (var &rest body)
  "Run BODY with `magit-refresh' counted onto VAR."
  (declare (indent 1))
  `(let ((,var 0))
     (cl-letf (((symbol-function 'magit-refresh) (lambda () (cl-incf ,var))))
       ,@body)))

(ert-deftest agent-repl-test-session-magit-refresh-hits-a-matching-buffer ()
  "A magit-status buffer on the workspace's directory is refreshed."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-session-" t)))
      (unwind-protect
          (agent-repl-test-session--with-magit refreshed
            (with-temp-buffer
              (setq-local default-directory (file-name-as-directory dir))
              (setq-local major-mode 'magit-status-mode)
              ;; Act
              (agent-repl--refresh-magit-status-for-dir dir nil)
              ;; Assert
              (should (equal refreshed 1))))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-session-magit-refresh-skips-a-foreign-buffer ()
  "A status buffer on another repository is left alone."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-session-" t))
          (other (make-temp-file "agent-repl-other-" t)))
      (unwind-protect
          (agent-repl-test-session--with-magit refreshed
            (with-temp-buffer
              (setq-local default-directory (file-name-as-directory other))
              (setq-local major-mode 'magit-status-mode)
              ;; Act
              (agent-repl--refresh-magit-status-for-dir dir nil)
              ;; Assert
              (should (equal refreshed 0))))
        (delete-directory dir t)
        (delete-directory other t)))))

(ert-deftest agent-repl-test-session-magit-refresh-skips-a-non-magit-buffer ()
  "An ordinary buffer in the worktree is not a status buffer."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((dir (make-temp-file "agent-repl-session-" t)))
      (unwind-protect
          (agent-repl-test-session--with-magit refreshed
            (with-temp-buffer
              (setq-local default-directory (file-name-as-directory dir))
              ;; Act
              (agent-repl--refresh-magit-status-for-dir dir nil)
              ;; Assert
              (should (equal refreshed 0))))
        (delete-directory dir t)))))

(ert-deftest agent-repl-test-session-magit-refresh-without-a-directory-is-a-noop ()
  "No directory means nothing to refresh — not an error."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test-session--with-magit refreshed
      ;; Act
      (agent-repl--refresh-magit-status-for-dir nil nil)
      ;; Assert
      (should (equal refreshed 0)))))

