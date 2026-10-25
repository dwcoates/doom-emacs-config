;;; test-page-unload.el --- ERT tests for page-unload.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The pages nobody looks at are unloaded: what counts as a look, what a pass
;; unloads and what it never does, and the page's return when its slot is
;; shown.  An unload drops the page and keeps the slot; the harness's host
;; holds `agent-repl-test-pu--pages'.
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

(defvar agent-repl-test-pu--dropped nil
  "Workspaces whose page the pass dropped, newest first.")

(defvar agent-repl-test-pu--pages nil
  "Workspaces whose page the harness's host holds.")

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
  "Every report, dropped page and page put back, newest first, as (KIND WS).")

(defmacro agent-repl-test-pu--with (names &rest body)
  "Run BODY with live workspaces NAMES, each holding a page, on a hand clock."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-test-pu--now 1000)
           (agent-repl-test-pu--dropped nil)
           (agent-repl-test-pu--hidden nil)
           (agent-repl-test-pu--logged nil)
           (agent-repl-test-pu--report 'reported)
           (agent-repl-test-pu--pending nil)
           (agent-repl-test-pu--events nil)
           (agent-repl-test-pu--pages (copy-sequence ,names))
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
                       ((symbol-function 'agent-repl-webhost-has-page-p)
                        (lambda (ws) (and (member ws agent-repl-test-pu--pages) t)))
                       ((symbol-function 'agent-repl-webhost-remove)
                        (lambda (ws)
                          (push (list 'drop ws) agent-repl-test-pu--events)
                          (push ws agent-repl-test-pu--dropped)
                          (setq agent-repl-test-pu--pages (delete ws agent-repl-test-pu--pages))))
                       ((symbol-function 'agent-repl-webhost-ensure-page)
                        (lambda (ws)
                          (push (list 'ensure ws) agent-repl-test-pu--events)
                          (cl-pushnew ws agent-repl-test-pu--pages :test #'equal)))
                       ((symbol-function 'agent-repl--info)
                        (agent-repl-test--record-into agent-repl-test-pu--logged)))
               ,@body))
         (mapc #'kill-buffer (seq-filter #'buffer-live-p buffers))))))

(defun agent-repl-test-pu--said-p (&rest parts)
  "Non-nil when one INFO record holds every one of PARTS."
  (seq-some (lambda (text) (seq-every-p (lambda (part) (string-search part text)) parts))
            agent-repl-test-pu--logged))

;;;; ---- The one announced drop ----

(defun agent-repl-test-pu--announce (still)
  "Drop alpha's page through the announced drop while STILL answers non-nil.
Answers the outcomes, newest first: (dropped) or (kept WHY)."
  (let ((outcomes nil))
    (agent-repl-page-drop-announced
     "alpha"
     (lambda () (funcall still))
     (lambda ()
       (push (list 'dropped (agent-repl--ws-get "alpha" :page-unloading)) outcomes)
       (agent-repl-webhost-remove "alpha"))
     (lambda (why) (push (list 'kept why) outcomes)))
    (lambda () outcomes)))

(ert-deftest agent-repl-test-pu-an-announced-drop-reports-before-it-drops ()
  "The daemon is told first, and the drop runs once it answers."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    ;; Act
    (let ((outcomes (agent-repl-test-pu--announce (lambda () t))))
      ;; Assert
      (should (equal (list (reverse agent-repl-test-pu--events) (funcall outcomes))
                     '(((report "alpha") (drop "alpha")) ((dropped nil))))))))

(ert-deftest agent-repl-test-pu-an-announced-drop-marks-its-report-in-flight ()
  "While the report is unanswered the workspace reads `:page-unloading'."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--report 'pending)
    ;; Act
    (agent-repl-test-pu--announce (lambda () t))
    ;; Assert
    (should (equal (list (agent-repl--ws-get "alpha" :page-unloading) agent-repl-test-pu--dropped)
                   '(t nil)))))

(ert-deftest agent-repl-test-pu-an-announced-drop-whose-report-did-not-land-keeps-the-page ()
  "A report the daemon did not take keeps the page, and says why."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--report 'refused)
    ;; Act
    (let ((outcomes (agent-repl-test-pu--announce (lambda () t))))
      ;; Assert
      (should (equal (list (funcall outcomes) agent-repl-test-pu--dropped
                           (agent-repl--ws-get "alpha" :page-unloading))
                     '(((kept not-reported)) nil nil))))))

(ert-deftest agent-repl-test-pu-an-announced-drop-no-longer-wanted-keeps-the-page ()
  "A drop whose cause changed while the report was in flight keeps the page."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (let ((wanted t))
      (setq agent-repl-test-pu--report 'pending)
      (let ((outcomes (agent-repl-test-pu--announce (lambda () wanted))))
        (setq wanted nil)
        ;; Act
        (funcall (nth 1 (car agent-repl-test-pu--pending)))
        ;; Assert
        (should (equal (list (funcall outcomes) agent-repl-test-pu--dropped
                             (agent-repl--ws-get "alpha" :page-unloading))
                       '(((kept changed)) nil nil)))))))

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

(ert-deftest agent-repl-test-pu-a-mount-is-a-look ()
  "Mounting a page stamps it looked at."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    ;; Act
    (agent-repl-page-unload-note-mounted "alpha")
    ;; Assert
    (should (equal (agent-repl--ws-get "alpha" :page-looked-at) 1000))))

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
                   '((report "alpha") (drop "alpha"))))))

(ert-deftest agent-repl-test-pu-a-page-whose-report-did-not-land-is-kept ()
  "A report the daemon did not take leaves the page loaded, and recorded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (setq agent-repl-test-pu--report 'refused)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal (list agent-repl-test-pu--dropped
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
    (should (equal agent-repl-test-pu--dropped '("alpha")))))

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
      (should (equal (list started (length agent-repl-test-pu--pending) agent-repl-test-pu--dropped)
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
    (should (equal (list agent-repl-test-pu--dropped (agent-repl-page-unloaded-p "alpha")
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
      (should (equal (list unloaded agent-repl-test-pu--dropped (agent-repl-page-unloaded-p "alpha"))
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
    (should-not agent-repl-test-pu--dropped)))

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
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-a-system-never-switched-in-is-idle ()
  "Before the first switch, nothing goes unlooked."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (setq agent-repl-page-unload--last-switch-at nil)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

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
    (should (equal agent-repl-test-pu--dropped '("alpha")))))

(ert-deftest agent-repl-test-pu-a-switch-after-idle-starts-every-hold-afresh ()
  "The first switch after idle starts the clock: a page unlooked for ages is held until the hold passes from it."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta" "gamma")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (setq agent-repl-page-unload--last-switch-at 100)
    (agent-repl-page-unload-note-shown "beta")
    ;; Act
    (agent-repl-page-unload-pass)
    (let ((held-at-the-switch (null agent-repl-test-pu--dropped)))
      ;; Switches keep coming, each within the idle span of the last.
      (cl-loop for at in '(1250 1501) for ws in '("gamma" "beta")
               do (setq agent-repl-test-pu--now at)
                  (agent-repl-page-unload-note-shown ws))
      (agent-repl-page-unload-pass)
      ;; Assert
      (should (equal (list held-at-the-switch agent-repl-test-pu--dropped) '(t ("alpha")))))))

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
                 (agent-repl-test--record-into logged)))
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
    (should (equal agent-repl-test-pu--dropped '("alpha")))
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
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-the-current-workspace-page-is-never-unloaded ()
  "The current workspace's page stays even before the host has shown it."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "alpha")))
      ;; Act
      (agent-repl-page-unload-pass))
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-a-page-with-no-look-recorded-is-stamped-not-unloaded ()
  "A page mounted before the module ran gets its first look now, not an unload."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal (list agent-repl-test-pu--dropped (agent-repl--ws-get "alpha" :page-looked-at))
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
      (should (equal (list unloaded agent-repl-test-pu--dropped) '(0 nil))))))

;;;; ---- The switch back ----

(ert-deftest agent-repl-test-pu-an-unload-keeps-the-slot ()
  "An unload drops the page and keeps the slot, whose windows are the panels.
2026-10-24: killing the slot with the page left the workspace's saved layout
naming a dead buffer, and every switch back found its panels closed."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal (list (buffer-live-p (agent-repl--ws-get "alpha" :frontend-buffer))
                         (agent-repl-page-unloaded-p "alpha"))
                   '(t t)))))

(ert-deftest agent-repl-test-pu-an-unloaded-page-is-not-unloaded-again ()
  "A pass does not report a page it already unloaded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-looked-at 0)
    (agent-repl-page-unload-pass)
    (setq agent-repl-test-pu--events nil)
    ;; Act
    (let ((started (agent-repl-page-unload-pass)))
      ;; Assert
      (should (equal (list started agent-repl-test-pu--events) '(0 nil))))))

(ert-deftest agent-repl-test-pu-a-workspace-with-no-slot-is-not-unloaded ()
  "Unloaded means a live slot without its page: no slot at all is not unloaded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (kill-buffer (agent-repl--ws-get "alpha" :frontend-buffer))
    (setq agent-repl-test-pu--pages nil)
    ;; Act / Assert
    (should-not (agent-repl-page-unloaded-p "alpha"))))

(ert-deftest agent-repl-test-pu-showing-an-unloaded-page-puts-it-back ()
  "Showing an unloaded page's slot puts the page back, marked as remounting."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil)
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should (equal (list agent-repl-test-pu--events
                         (agent-repl-page-unloaded-p "alpha")
                         (agent-repl--ws-get "alpha" :page-remounting)
                         (agent-repl--ws-get "alpha" :page-looked-at))
                   '(((ensure "alpha")) nil t 1000)))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.returned" "ws=alpha"))))

(ert-deftest agent-repl-test-pu-showing-a-held-page-puts-nothing-back ()
  "Showing a page the host still holds sends it nothing."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should-not agent-repl-test-pu--events)))

;;;; ---- A remount the user left before it drew ----

(defmacro agent-repl-test-pu--with-host (&rest body)
  "Run BODY with a host whose `drawn' reads are recorded, not answered.
Each read lands in `asked' as (METHOD ARGS CALLBACK); every workspace's id
is \"id-\" and its name, and the answers handed to the webhost's own record
land in `recorded'."
  (declare (indent 0))
  `(let ((asked nil) (recorded nil))
     (cl-letf (((symbol-function 'agent-repl--webhost-id) (lambda (ws) (concat "id-" ws)))
               ((symbol-function 'agent-repl--ws-by-ref-id)
                (lambda (id) (and (string-prefix-p "id-" id) (substring id 3))))
               ((symbol-function 'agent-repl-webhost-read)
                (lambda (method args callback) (push (list method args callback) asked) t))
               ((symbol-function 'agent-repl--webhost-answered) (lambda (raw) (push raw recorded))))
       ,@body)))

(defun agent-repl-test-pu--drawn-answer (ws outcome drawn)
  "The host's `drawn' answer for WS: OUTCOME, and DRAWN or null."
  (json-serialize `(:op "drawn" :id ,(concat "id-" ws) :outcome ,outcome
                    :drawn ,(or drawn :null) :notes [])))

(defun agent-repl-test-pu--remount-and-leave (ws other)
  "Unload WS's page, show its slot (which puts the page back), then show OTHER."
  (setq agent-repl-test-pu--pages (delete ws agent-repl-test-pu--pages))
  (agent-repl-page-unload-note-shown ws)
  (agent-repl-page-unload-note-shown other)
  (setq agent-repl-test-pu--events nil))

(ert-deftest agent-repl-test-pu-leaving-a-remount-asks-the-host-if-it-drew ()
  "Switching away from a remounted page asks the host whether it has drawn."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      ;; Act
      (agent-repl-test-pu--remount-and-leave "alpha" "beta")
      ;; Assert
      (should (equal asked '(("drawn" ("id-alpha") agent-repl-page-unload--on-drawn-answer)))))))

(ert-deftest agent-repl-test-pu-leaving-a-page-not-remounted-asks-nothing ()
  "Switching away from a page that was not remounted asks the host nothing."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      (agent-repl-page-unload-note-shown "alpha")
      ;; Act
      (agent-repl-page-unload-note-shown "beta")
      ;; Assert
      (should-not asked))))

(ert-deftest agent-repl-test-pu-a-remount-left-before-it-drew-is-unloaded ()
  "A remounted page still loading when left is unloaded, the daemon told first."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      (agent-repl-test-pu--remount-and-leave "alpha" "beta")
      ;; Act
      (agent-repl-page-unload--on-drawn-answer
       (agent-repl-test-pu--drawn-answer "alpha" "present" nil))
      ;; Assert
      (should (equal (list (reverse agent-repl-test-pu--events)
                           (agent-repl-page-unloaded-p "alpha")
                           (agent-repl--ws-get "alpha" :page-remounting))
                     '(((report "alpha") (drop "alpha")) t nil)))
      (should (agent-repl-test-pu--said-p "elisp.page-unload.unloaded" "ws=alpha" "reason=abandoned")))))

(ert-deftest agent-repl-test-pu-the-drawn-answer-reaches-the-webhost-record ()
  "The host's answer is recorded by the webhost, notes and all."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      (agent-repl-test-pu--remount-and-leave "alpha" "beta")
      (let ((raw (agent-repl-test-pu--drawn-answer "alpha" "present" "page")))
        ;; Act
        (agent-repl-page-unload--on-drawn-answer raw)
        ;; Assert
        (should (equal recorded (list raw)))))))

(ert-deftest agent-repl-test-pu-a-remount-that-drew-is-kept-and-settled ()
  "A remounted page that drew before it was left stays, and is not asked of again."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      (agent-repl-test-pu--remount-and-leave "alpha" "beta")
      ;; Act
      (agent-repl-page-unload--on-drawn-answer
       (agent-repl-test-pu--drawn-answer "alpha" "present" "page"))
      ;; Assert
      (should (equal (list agent-repl-test-pu--events (agent-repl--ws-get "alpha" :page-remounting))
                     '(nil nil)))
      (should (agent-repl-test-pu--said-p "elisp.page-unload.abandon-dropped" "ws=alpha" "reason=drawn")))))

(ert-deftest agent-repl-test-pu-a-remount-the-user-is-back-on-is-kept ()
  "A remounted page the user switched back to before the answer stays loaded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      (agent-repl-test-pu--remount-and-leave "alpha" "beta")
      (agent-repl-page-unload-note-shown "alpha")
      ;; Act
      (agent-repl-page-unload--on-drawn-answer
       (agent-repl-test-pu--drawn-answer "alpha" "present" nil))
      ;; Assert
      (should (equal (list agent-repl-test-pu--events (agent-repl--ws-get "alpha" :page-remounting))
                     '(nil t)))
      (should (agent-repl-test-pu--said-p "elisp.page-unload.abandon-deferred" "ws=alpha"
                                          "reason=not-holdable")))))

(ert-deftest agent-repl-test-pu-a-remount-the-host-no-longer-holds-is-settled ()
  "A page the host answers absent has nothing to cancel."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      (agent-repl-test-pu--remount-and-leave "alpha" "beta")
      ;; Act
      (agent-repl-page-unload--on-drawn-answer
       (agent-repl-test-pu--drawn-answer "alpha" "absent" nil))
      ;; Assert
      (should (equal (list agent-repl-test-pu--events (agent-repl--ws-get "alpha" :page-remounting))
                     '(nil nil)))
      (should (agent-repl-test-pu--said-p "elisp.page-unload.abandon-dropped" "ws=alpha" "reason=absent")))))

(ert-deftest agent-repl-test-pu-a-remount-asked-of-a-loading-host-is-deferred ()
  "A read queued on a host still loading leaves the page to the next leave."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      (agent-repl-test-pu--remount-and-leave "alpha" "beta")
      ;; Act
      (agent-repl-page-unload--on-drawn-answer
       (agent-repl-test-pu--drawn-answer "alpha" "queued" nil))
      ;; Assert
      (should (equal (list agent-repl-test-pu--events (agent-repl--ws-get "alpha" :page-remounting))
                     '(nil t)))
      (should (agent-repl-test-pu--said-p "elisp.page-unload.abandon-deferred" "ws=alpha"
                                          "reason=host-loading")))))

(ert-deftest agent-repl-test-pu-an-abandon-whose-report-did-not-land-is-kept ()
  "A cancel whose report the daemon refused keeps the page, to be asked again."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      (setq agent-repl-test-pu--report 'refused)
      (agent-repl-test-pu--remount-and-leave "alpha" "beta")
      ;; Act
      (agent-repl-page-unload--on-drawn-answer
       (agent-repl-test-pu--drawn-answer "alpha" "present" nil))
      ;; Assert
      (should (equal (list agent-repl-test-pu--dropped (agent-repl--ws-get "alpha" :page-remounting))
                     '(nil t)))
      (should (agent-repl-test-pu--said-p "elisp.page-unload.kept" "ws=alpha" "why=not-reported")))))

(ert-deftest agent-repl-test-pu-an-answer-for-a-closed-workspace-is-dropped ()
  "An answer naming no live workspace changes nothing, and is recorded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      ;; Act
      (agent-repl-page-unload--on-drawn-answer
       (json-serialize '(:op "drawn" :id "gone" :outcome "absent" :drawn :null :notes [])))
      ;; Assert
      (should-not agent-repl-test-pu--events)
      (should (agent-repl-test-pu--said-p "elisp.page-unload.abandon-dropped" "id=gone"
                                          "reason=no-workspace")))))

(ert-deftest agent-repl-test-pu-an-unreadable-answer-is-an-error ()
  "An answer that is not JSON is recorded at ERROR and unloads nothing."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      (let (errors)
        (cl-letf (((symbol-function 'agent-repl--error) (agent-repl-test--record-into errors)))
          ;; Act
          (agent-repl-page-unload--on-drawn-answer "not json"))
        ;; Assert
        (should-not agent-repl-test-pu--events)
        (should (seq-some (lambda (text) (string-search "abandon-answer-unreadable" text)) errors))))))

(ert-deftest agent-repl-test-pu-leaving-a-remount-with-no-host-settles-it ()
  "With no host to ask there is no load to cancel, and the mark is cleared."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      (cl-letf (((symbol-function 'agent-repl-webhost-read) (lambda (&rest _) nil)))
        ;; Act
        (agent-repl-test-pu--remount-and-leave "alpha" "beta"))
      ;; Assert
      (should-not (agent-repl--ws-get "alpha" :page-remounting))
      (should (agent-repl-test-pu--said-p "elisp.page-unload.abandon-skipped" "ws=alpha" "reason=no-host")))))

(ert-deftest agent-repl-test-pu-leaving-a-remount-with-no-ref-fails-hard ()
  "A remounted workspace with no ref is a bug: recorded at ERROR, then signalled."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-test-pu--with-host
      (let (errors)
        (cl-letf (((symbol-function 'agent-repl--webhost-id) #'ignore)
                  ((symbol-function 'agent-repl--error) (agent-repl-test--record-into errors)))
          ;; Act / Assert
          (should-error (agent-repl-test-pu--remount-and-leave "alpha" "beta")))
        (should (seq-some (lambda (text) (string-search "abandon-failed ws=alpha reason=no-ref" text))
                          errors))))))

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
