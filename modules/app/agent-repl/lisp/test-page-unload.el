;;; test-page-unload.el --- ERT tests for page-unload.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The pages nobody looks at are unloaded: what counts as a look, what a pass
;; unloads and what it never does, and the remount on the switch back.
;;
;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-page-unload.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Harness ----

(defvar agent-repl-test-pu--now 0
  "The harness's clock, in seconds.")

(defvar agent-repl-test-pu--detached nil
  "Workspaces the pass detached, newest first.")

(defvar agent-repl-test-pu--hidden nil
  "Workspaces the roster answers hidden.")

(defvar agent-repl-test-pu--logged nil
  "Every INFO record's text, newest first.")

(defvar agent-repl-test-pu--report 'reported
  "How the harness's daemon answers an unload report.
`reported' answers success at once, `refused' answers failure at once, and
`pending' holds the answer in `agent-repl-test-pu--pending'.")

(defvar agent-repl-test-pu--pending nil
  "The held reports, newest first, each (WS ON-REPORTED ON-NOT-REPORTED).")

(defvar agent-repl-test-pu--events nil
  "Every report and detach, newest first, as (KIND WS).")

(defmacro agent-repl-test-pu--with (names &rest body)
  "Run BODY with live workspaces NAMES, each holding a page, on a hand clock."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-test-pu--now 1000)
           (agent-repl-test-pu--detached nil)
           (agent-repl-test-pu--hidden nil)
           (agent-repl-test-pu--logged nil)
           (agent-repl-test-pu--report 'reported)
           (agent-repl-test-pu--pending nil)
           (agent-repl-test-pu--events nil)
           (agent-repl-page-unload--clock (lambda () agent-repl-test-pu--now))
           (agent-repl-page-unload--shown nil)
           (agent-repl-page-unload--shown-since nil)
           (agent-repl-page-unload-after 500)
           (agent-repl-page-unload-idle-after 300)
           ;; IN USE unless a test says otherwise: a switch just now, in a
           ;; spell of use as old as the harness's clock.
           (agent-repl-page-unload--last-switch-at 1000)
           (agent-repl-page-unload--in-use-since 0)
           (agent-repl-page-unload-dwell 2)
           (buffers nil))
       (unwind-protect
           (progn
             (dolist (ws ,names)
               (agent-repl--ws-put ws :project-dir (format "/tmp/test-pu/%s/" ws))
               (let ((buf (generate-new-buffer (format " *test-pu-%s*" ws))))
                 (push buf buffers)
                 (agent-repl--ws-put ws :frontend-buffer buf)))
             (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () ,names))
                       ((symbol-function 'agent-repl--ws-current-name) (lambda () nil))
                       ((symbol-function 'agent-repl-open-tabs-hidden-p)
                        (lambda (ws) (member ws agent-repl-test-pu--hidden)))
                       ((symbol-function 'agent-repl-host-report-page-unloading)
                        (lambda (ws on-reported on-not-reported)
                          (push (list 'report ws) agent-repl-test-pu--events)
                          (pcase agent-repl-test-pu--report
                            ('reported (funcall on-reported))
                            ('refused (funcall on-not-reported))
                            ('pending (push (list ws on-reported on-not-reported)
                                            agent-repl-test-pu--pending)))))
                       ((symbol-function 'agent-repl--frontend-detach-webview)
                        (lambda (ws buf)
                          (push (list 'detach ws) agent-repl-test-pu--events)
                          (push ws agent-repl-test-pu--detached)
                          (kill-buffer buf)
                          (agent-repl--ws-put ws :frontend-buffer nil)))
                       ((symbol-function 'agent-repl--info)
                        (lambda (_ws fmt &rest args)
                          (push (apply #'format fmt args) agent-repl-test-pu--logged))))
               ,@body))
         (mapc #'kill-buffer (seq-filter #'buffer-live-p buffers))))))

(defun agent-repl-test-pu--said-p (&rest parts)
  "Non-nil when one INFO record holds every one of PARTS."
  (seq-some (lambda (text) (seq-every-p (lambda (part) (string-search part text)) parts))
            agent-repl-test-pu--logged))

;;;; ---- What counts as a look ----

(ert-deftest agent-repl-test-pu-a-page-shown-past-the-dwell-is-looked-at ()
  "Switching away from a page shown for the dwell stamps it looked at."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-page-unload-note-shown "alpha")
    (setq agent-repl-test-pu--now 1005)
    ;; Act
    (agent-repl-page-unload-note-shown "beta")
    ;; Assert
    (should (equal (agent-repl--ws-get "alpha" :page-looked-at) 1005))))

(ert-deftest agent-repl-test-pu-a-page-passed-through-is-not-looked-at ()
  "A page shown for less than the dwell, as a held switch key shows it, is no look."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl--ws-put "alpha" :page-looked-at 10)
    (agent-repl-page-unload-note-shown "alpha")
    (setq agent-repl-test-pu--now 1001)
    ;; Act
    (agent-repl-page-unload-note-shown "beta")
    ;; Assert
    (should (equal (agent-repl--ws-get "alpha" :page-looked-at) 10))))

(ert-deftest agent-repl-test-pu-showing-the-same-page-again-keeps-its-start ()
  "A repeated show of the page already shown does not restart its dwell."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl-page-unload-note-shown "alpha")
    (setq agent-repl-test-pu--now 1010)
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should (equal agent-repl-page-unload--shown-since 1000))))

(ert-deftest agent-repl-test-pu-a-mount-is-a-look-and-ends-an-unload ()
  "Mounting a page stamps it looked at and clears its unloaded mark, recorded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-unloaded t)
    ;; Act
    (agent-repl-page-unload-note-mounted "alpha")
    ;; Assert
    (should (equal (list (agent-repl--ws-get "alpha" :page-looked-at)
                         (agent-repl-page-unloaded-p "alpha"))
                   '(1000 nil)))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.reloaded" "ws=alpha"))))

;;;; ---- The pass ----

(ert-deftest agent-repl-test-pu-the-daemon-is-told-before-the-page-is-unloaded ()
  "The report precedes the detach, so the stream's end is known deliberate."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal (reverse agent-repl-test-pu--events)
                   '((report "alpha") (detach "alpha"))))))

(ert-deftest agent-repl-test-pu-a-page-whose-report-did-not-land-is-kept ()
  "A report the daemon did not take leaves the page loaded, and recorded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (setq agent-repl-test-pu--report 'refused)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal (list agent-repl-test-pu--detached
                         (agent-repl-page-unloaded-p "alpha")
                         (agent-repl--ws-get "alpha" :page-unloading))
                   '(nil nil nil)))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.kept" "ws=alpha" "why=not-reported"))))

(ert-deftest agent-repl-test-pu-a-kept-page-is-reported-again-by-the-next-pass ()
  "A page kept for a report that did not land is tried again."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (setq agent-repl-test-pu--report 'refused)
    (agent-repl-page-unload-pass)
    (setq agent-repl-test-pu--report 'reported)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal agent-repl-test-pu--detached '("alpha")))))

(ert-deftest agent-repl-test-pu-a-report-in-flight-is-not-sent-again ()
  "A pass while a page's report is unanswered does not report it twice."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (setq agent-repl-test-pu--report 'pending)
    (agent-repl-page-unload-pass)
    ;; Act
    (let ((started (agent-repl-page-unload-pass)))
      ;; Assert
      (should (equal (list started (length agent-repl-test-pu--pending) agent-repl-test-pu--detached)
                     '(0 1 nil))))))

(ert-deftest agent-repl-test-pu-a-page-shown-while-its-report-was-in-flight-is-kept ()
  "The user switching to the page before the answer keeps it loaded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (setq agent-repl-test-pu--report 'pending)
    (agent-repl-page-unload-pass)
    (setq agent-repl-page-unload--shown "alpha")
    ;; Act
    (funcall (nth 1 (car agent-repl-test-pu--pending)))
    ;; Assert
    (should (equal (list agent-repl-test-pu--detached (agent-repl-page-unloaded-p "alpha")
                         (agent-repl--ws-get "alpha" :page-unloading))
                   '(nil nil nil)))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.kept" "ws=alpha" "why=in-use-since-report"))))

(ert-deftest agent-repl-test-pu-a-page-not-looked-at-in-a-while-is-unloaded ()
  "A page last looked at longer ago than the hold is unloaded, and recorded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    ;; Act
    (let ((unloaded (agent-repl-page-unload-pass)))
      ;; Assert
      (should (equal (list unloaded agent-repl-test-pu--detached (agent-repl-page-unloaded-p "alpha"))
                     '(1 ("alpha") t)))
      (should (agent-repl-test-pu--said-p "elisp.page-unload.unloaded" "ws=alpha" "reason=unlooked")))))

(ert-deftest agent-repl-test-pu-a-page-looked-at-lately-is-held ()
  "A page looked at within the hold stays."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 900)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--detached)))

;;;; ---- The clock runs only while the system is in use ----

(ert-deftest agent-repl-test-pu-an-idle-system-unloads-no-unlooked-page ()
  "With no switch for the idle span, an unlooked page is held."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (setq agent-repl-page-unload--last-switch-at 699)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--detached)))

(ert-deftest agent-repl-test-pu-a-system-never-switched-in-is-idle ()
  "Before the first switch, nothing goes unlooked."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (setq agent-repl-page-unload--last-switch-at nil)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--detached)))

(ert-deftest agent-repl-test-pu-an-idle-system-still-unloads-a-hidden-page ()
  "Idleness stops the unlooked clock only: a hidden workspace's page still goes."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 900)
    (setq agent-repl-page-unload--last-switch-at nil)
    (push "alpha" agent-repl-test-pu--hidden)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal agent-repl-test-pu--detached '("alpha")))))

(ert-deftest agent-repl-test-pu-a-switch-after-idle-starts-every-hold-afresh ()
  "The first switch after idle starts the clock: a page unlooked for ages is held until the hold passes from it."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta" "gamma")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (setq agent-repl-page-unload--last-switch-at 100)
    (agent-repl-page-unload-note-shown "beta")
    ;; Act
    (agent-repl-page-unload-pass)
    (let ((held-at-the-switch (null agent-repl-test-pu--detached)))
      ;; Switches keep coming, each within the idle span of the last.
      (cl-loop for at in '(1250 1501) for ws in '("gamma" "beta")
               do (setq agent-repl-test-pu--now at)
                  (agent-repl-page-unload-note-shown ws))
      (agent-repl-page-unload-pass)
      ;; Assert
      (should (equal (list held-at-the-switch agent-repl-test-pu--detached) '(t ("alpha")))))))

(ert-deftest agent-repl-test-pu-a-switch-in-use-keeps-the-spell-going ()
  "A switch while the system is in use moves the last switch, not the spell's start."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-page-unload--last-switch-at 900
          agent-repl-page-unload--in-use-since 400)
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should (equal (list agent-repl-page-unload--in-use-since agent-repl-page-unload--last-switch-at)
                   '(400 1000)))))

(ert-deftest agent-repl-test-pu-showing-the-same-page-again-is-no-use ()
  "Only a switch to another page is use: the page already shown, shown again, is not."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl-page-unload-note-shown "alpha")
    (setq agent-repl-test-pu--now 1400)
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should (equal agent-repl-page-unload--last-switch-at 1000))))

(ert-deftest agent-repl-test-pu-the-start-of-a-spell-of-use-is-recorded ()
  "The first switch after idle records how long the system sat idle."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-page-unload--last-switch-at 100)
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl-page-unload-note-shown "alpha"))
      ;; Assert
      (should (member "elisp.page-unload.in-use since-idle-s=900" logged)))))

(ert-deftest agent-repl-test-pu-a-hidden-workspace-page-is-unloaded-at-once ()
  "A hidden workspace's page is unloaded however lately it was looked at."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 1000)
    (setq agent-repl-test-pu--hidden '("alpha"))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal agent-repl-test-pu--detached '("alpha")))
    (should (agent-repl-test-pu--said-p "reason=hidden"))))

(ert-deftest agent-repl-test-pu-the-shown-page-is-never-unloaded ()
  "The page the host shows stays, hidden or not looked at."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (setq agent-repl-page-unload--shown "alpha")
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--detached)))

(ert-deftest agent-repl-test-pu-the-current-workspace-page-is-never-unloaded ()
  "The current workspace's page stays even before the host has shown it."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "alpha")))
      ;; Act
      (agent-repl-page-unload-pass))
    ;; Assert
    (should-not agent-repl-test-pu--detached)))

(ert-deftest agent-repl-test-pu-a-page-with-no-look-recorded-is-stamped-not-unloaded ()
  "A page mounted before the module ran gets its first look now, not an unload."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal (list agent-repl-test-pu--detached (agent-repl--ws-get "alpha" :page-looked-at))
                   '(nil 1000)))))

(ert-deftest agent-repl-test-pu-a-workspace-with-no-page-is-skipped ()
  "A workspace holding no page has nothing to unload."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (kill-buffer (agent-repl--ws-get "alpha" :frontend-buffer))
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    ;; Act
    (let ((unloaded (agent-repl-page-unload-pass)))
      ;; Assert
      (should (equal (list unloaded agent-repl-test-pu--detached) '(0 nil))))))

;;;; ---- The switch back ----

(ert-deftest agent-repl-test-pu-the-switch-back-remounts-and-restores ()
  "A remount clears the mark, makes the page, and runs the awaited panel restore."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-unloaded t)
    (let (calls)
      (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
                 (lambda (ws)
                   (push (list 'mount ws (agent-repl-page-unloaded-p ws)) calls)
                   :created))
                ((symbol-function 'agent-repl--panels-on-view-created)
                 (lambda (ws) (push (list 'restore ws) calls))))
        ;; Act
        (agent-repl-page-unload-remount "alpha"))
      ;; Assert: the mark is clear before the mount asks for eligibility.
      (should (equal (reverse calls) '((mount "alpha" nil) (restore "alpha"))))
      (should (agent-repl-test-pu--said-p "elisp.page-unload.remount" "ws=alpha")))))

(ert-deftest agent-repl-test-pu-start-keeps-a-running-timer ()
  "Starting twice keeps the one timer."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (let ((agent-repl-page-unload--timer nil))
      (unwind-protect
          (progn
            (agent-repl-page-unload-start)
            (let ((first agent-repl-page-unload--timer))
              ;; Act
              (agent-repl-page-unload-start)
              ;; Assert
              (should (eq agent-repl-page-unload--timer first))))
        (when (timerp agent-repl-page-unload--timer)
          (cancel-timer agent-repl-page-unload--timer))))))

(provide 'test-page-unload)
;;; test-page-unload.el ends here
