;;; test-page-unload.el --- ERT tests for page-unload.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Each page is held by its workspace's status: what a pass unloads and
;; loads, what it never does, a visit's unload at the switch away, and the
;; page's return when its slot is shown.  An unload drops the page and keeps
;; the slot; the harness's host holds `agent-repl-test-pu--pages', and its
;; roster `agent-repl-test-pu--status'.
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

(defvar agent-repl-test-pu--status nil
  "Each workspace's roster status arm, (WS . ARM); a workspace absent is `:ready'.
\(WS . nil) is a workspace with no roster row yet.")

(defvar agent-repl-test-pu--turn-ended nil
  "Each workspace's last turn end in epoch ms, (WS . MS); absent is none.")

(defvar agent-repl-test-pu--current nil
  "The workspace the harness answers current.")

(defvar agent-repl-test-pu--opening nil
  "Workspaces whose tab's open the harness answers in flight.")

(defvar agent-repl-test-pu--successor nil
  "The handover successor the harness's link answers, nil outside a handover.")

(defvar agent-repl-test-pu--residencies nil
  "Every residency report sent, newest first, as (WS RESIDENCY).")

(defvar agent-repl-test-pu--residency-lands t
  "Whether the harness's daemon takes a residency report.")

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
           (agent-repl-test-pu--status nil)
           (agent-repl-test-pu--turn-ended nil)
           (agent-repl-test-pu--current nil)
           (agent-repl-test-pu--opening nil)
           (agent-repl-test-pu--residencies nil)
           (agent-repl-test-pu--successor nil)
           (agent-repl-test-pu--residency-lands t)
           (agent-repl-test-pu--logged nil)
           (agent-repl-test-pu--report 'reported)
           (agent-repl-test-pu--pending nil)
           (agent-repl-test-pu--events nil)
           (agent-repl-test-pu--pages (copy-sequence ,names))
           (agent-repl-page-unload--clock (lambda () agent-repl-test-pu--now))
           (agent-repl-page-unload--shown nil)
           (agent-repl-page-unload-after 500)
           (buffers nil))
       (unwind-protect
           (progn
             (dolist (ws ,names)
               (agent-repl--ws-put ws :project-dir (format "/tmp/test-pu/%s/" ws))
               (let ((buf (generate-new-buffer (format " *test-pu-%s*" ws))))
                 (push buf buffers)
                 (agent-repl--ws-put ws :frontend-buffer buf)))
             (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () ,names))
                       ((symbol-function 'agent-repl--ws-current-name)
                        (lambda () agent-repl-test-pu--current))
                       ((symbol-function 'agent-repl--webhost-id) (lambda (ws) (concat "id-" ws)))
                       ((symbol-function 'agent-repl-link-successor) (lambda () agent-repl-test-pu--successor))
                       ((symbol-function 'agent-repl-host-report-page-residency)
                        (lambda (ws residency on-reported)
                          (push (list ws residency) agent-repl-test-pu--residencies)
                          (when agent-repl-test-pu--residency-lands (funcall on-reported))))
                       ((symbol-function 'agent-repl-open-tabs-holds-p)
                        (lambda (id) (member (substring id 3) agent-repl-test-pu--opening)))
                       ((symbol-function 'agent-repl-roster-status-for-ws)
                        (lambda (ws) (alist-get ws agent-repl-test-pu--status :ready nil #'equal)))
                       ((symbol-function 'agent-repl-roster-last-turn-ended-ms)
                        (lambda (ws) (alist-get ws agent-repl-test-pu--turn-ended nil nil #'equal)))
                       ((symbol-function 'agent-repl-open-tabs-hidden-p)
                        (lambda (ws) (member ws agent-repl-test-pu--hidden)))
                       ((symbol-function 'agent-repl-host-report-page-unloading)
                        (agent-repl-test--page-unloading-reporter
                         (lambda () agent-repl-test-pu--report)
                         (lambda (ws) (push (list 'report ws) agent-repl-test-pu--events))
                         (lambda (held) (push held agent-repl-test-pu--pending))))
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

(ert-deftest agent-repl-test-pu-an-idle-page-is-held-an-hour-by-default ()
  "An idle page is held for an hour past its last prompt (owner ruling, 2026-10-29)."
  (should (= (eval (car (get 'agent-repl-page-unload-after 'standard-value)) t) 3600)))

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

;;;; ---- Every page drop is announced ----

(defconst agent-repl-test-pu--page-drops
  '(agent-repl-webhost-remove agent-repl--frontend-detach-webview)
  "The calls that drop a workspace's page.")

(defconst agent-repl-test-pu--unannounced-drops
  '((agent-repl--frontend-detach-webview . "the drop primitive itself; its callers announce")
    (agent-repl--frontend-release-workspace-webview . "the workspace is ending: the daemon ended it first"))
  "The definitions that may drop a page unannounced, each with why.")

(defun agent-repl-test-pu--unannounced-drop-p (form)
  "Non-nil when FORM calls a page drop outside `agent-repl-page-drop-announced'."
  (cond
   ((not (consp form)) nil)
   ((eq (car form) 'agent-repl-page-drop-announced) nil)
   ((memq (car form) '(declare-function quote function))
    (and (eq (car form) 'function) (memq (cadr form) agent-repl-test-pu--page-drops)))
   ((memq (car form) agent-repl-test-pu--page-drops) t)
   (t (let ((found nil) (rest form))
        (while (and (consp rest) (not found))
          (setq found (agent-repl-test-pu--unannounced-drop-p (car rest))
                rest (cdr rest)))
        found))))

(defun agent-repl-test-pu--unannounced-droppers (forms)
  "The names of the definitions among FORMS that drop a page unannounced."
  (let (out)
    (dolist (form forms (nreverse out))
      (when (and (consp form) (memq (car form) '(defun defmacro cl-defun))
                 (agent-repl-test-pu--unannounced-drop-p (cdddr form)))
        (push (cadr form) out)))))

(defun agent-repl-test-pu--source-forms ()
  "Every top-level form of agent-repl's elisp sources, tests excluded."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name
                                      (locate-library "test-page-unload"))))
        forms)
    (dolist (file (directory-files dir t "\\`[^.].*\\.el\\'") forms)
      (unless (string-prefix-p "test-" (file-name-nondirectory file))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (while t (push (read (current-buffer)) forms))
            (end-of-file nil)))))))

(defvar agent-repl-test-pu--source-forms (agent-repl-test-pu--source-forms)
  "The sources' forms, read once when this file loads.")

(ert-deftest agent-repl-test-pu-every-page-drop-is-announced ()
  "Only the named exceptions drop a page without telling the daemon first."
  ;; Act
  (let ((droppers (agent-repl-test-pu--unannounced-droppers agent-repl-test-pu--source-forms)))
    ;; Assert
    (should (equal (sort (mapcar #'symbol-name droppers) #'string<)
                   (sort (mapcar (lambda (e) (symbol-name (car e)))
                                 agent-repl-test-pu--unannounced-drops)
                         #'string<)))))

(ert-deftest agent-repl-test-pu-the-drop-scan-catches-a-hand-rolled-drop ()
  "A definition that drops a page itself is found; one through the helper is not."
  ;; Arrange
  (let ((forms '((defun hand-rolled (ws buf) (when buf (agent-repl--frontend-detach-webview ws buf)))
                 (defun by-symbol (ws) (mapc #'agent-repl-webhost-remove (list ws)))
                 (defun announced (ws)
                   (agent-repl-page-drop-announced
                    ws #'always (lambda () (agent-repl-webhost-remove ws)) #'ignore)))))
    ;; Act
    (let ((droppers (agent-repl-test-pu--unannounced-droppers forms)))
      ;; Assert
      (should (equal droppers '(hand-rolled by-symbol))))))

;;;; ---- A hot reload heals the rows an unannounced drop left severed ----

(ert-deftest agent-repl-test-pu-every-pageless-workspace-is-reported ()
  "Each open workspace the host holds no page for is reported, once each."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta" "gamma")
    (setq agent-repl-test-pu--pages '("alpha"))
    ;; Act
    (let ((sent (agent-repl-page-unload-report-pageless)))
      ;; Assert
      (should (equal (list sent (reverse agent-repl-test-pu--events))
                     '(2 ((report "beta") (report "gamma"))))))))

(ert-deftest agent-repl-test-pu-a-pageless-workspace-already-reporting-is-skipped ()
  "A workspace whose report is in flight is not reported a second time."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (setq agent-repl-test-pu--pages nil)
    (agent-repl--ws-put "alpha" :page-unloading t)
    ;; Act
    (agent-repl-page-unload-report-pageless)
    ;; Assert
    (should (equal (reverse agent-repl-test-pu--events) '((report "beta"))))))

(ert-deftest agent-repl-test-pu-a-pageless-report-records-its-outcome ()
  "A landed report and one that did not land are each recorded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil)
    (agent-repl-page-unload-report-pageless)
    (setq agent-repl-test-pu--report 'refused)
    ;; Act
    (agent-repl-page-unload-report-pageless)
    ;; Assert
    (should (agent-repl-test-pu--said-p "elisp.page-unload.pageless-reported ws=alpha"))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.pageless-unreported ws=alpha"))))

;;;; ---- Buckets ----

(ert-deftest agent-repl-test-pu-every-roster-status-has-a-bucket ()
  "Every status arm the roster decodes is in exactly one bucket."
  ;; Arrange
  (require 'wire-roster (expand-file-name "wire-roster.el" (file-name-directory (symbol-file 'agent-repl-page-unload-pass))))
  (let ((arms (mapcar #'cadr agent-repl-wire-roster-row-status-arms)))
    ;; Act
    (let ((placed (mapcar (lambda (arm)
                            (cons arm (length (seq-filter (lambda (b) (memq arm (cdr b)))
                                                          agent-repl-page-unload-buckets))))
                          arms)))
      ;; Assert
      (should (> (length arms) 20))
      (should (equal (seq-remove (lambda (p) (= (cdr p) 1)) placed) nil)))))

(ert-deftest agent-repl-test-pu-a-status-in-no-bucket-is-an-error ()
  "A status the table was not taught is recorded at ERROR and signalled."
  ;; Arrange
  (let ((errors nil))
    (cl-letf (((symbol-function 'agent-repl--error) (agent-repl-test--record-into errors)))
      ;; Act / Assert
      (should-error (agent-repl-page-unload-bucket :never-seen))
      (should (seq-some (lambda (text) (string-search "unknown-status arm=:never-seen" text)) errors)))))

(ert-deftest agent-repl-test-pu-no-status-has-no-bucket ()
  "A workspace with no roster row yet is in no bucket."
  (should-not (agent-repl-page-unload-bucket nil)))

;;;; ---- Which pages are wanted before a show ----

(ert-deftest agent-repl-test-pu-the-shown-page-is-wanted ()
  "The page on screen is wanted, whatever its status."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :severed))
          agent-repl-page-unload--shown "alpha")
    ;; Act / Assert
    (should (agent-repl-page-unload-wanted-p "alpha"))))

(ert-deftest agent-repl-test-pu-the-current-workspace-page-is-wanted ()
  "The current workspace's page is wanted before the host has shown it."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--current "alpha")
    ;; Act / Assert
    (should (agent-repl-page-unload-wanted-p "alpha"))))

(ert-deftest agent-repl-test-pu-a-working-page-is-wanted ()
  "A working workspace's page is wanted off screen."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :thinking)))
    ;; Act / Assert
    (should (agent-repl-page-unload-wanted-p "alpha"))))

(ert-deftest agent-repl-test-pu-an-idle-page-is-not-wanted ()
  "An idle workspace's page is not loaded ahead of its show."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    ;; Act / Assert
    (should-not (agent-repl-page-unload-wanted-p "alpha"))))

(ert-deftest agent-repl-test-pu-a-hidden-working-page-is-not-wanted ()
  "A hidden workspace holds no page off screen, working or not."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :thinking))
          agent-repl-test-pu--hidden '("alpha"))
    ;; Act / Assert
    (should-not (agent-repl-page-unload-wanted-p "alpha"))))

;;;; ---- The pass ----

(ert-deftest agent-repl-test-pu-the-daemon-is-told-before-the-page-is-unloaded ()
  "The report precedes the detach, so the stream's end is known deliberate."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :severed)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal (reverse agent-repl-test-pu--events)
                   '((report "alpha") (drop "alpha"))))))

(ert-deftest agent-repl-test-pu-a-page-whose-report-did-not-land-is-kept ()
  "A report the daemon did not take leaves the page loaded, and recorded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :severed))
          agent-repl-test-pu--report 'refused)
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
    (setq agent-repl-test-pu--status '(("alpha" . :severed))
          agent-repl-test-pu--report 'refused)
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
    (setq agent-repl-test-pu--status '(("alpha" . :severed))
          agent-repl-test-pu--report 'pending)
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
    (setq agent-repl-test-pu--status '(("alpha" . :severed))
          agent-repl-test-pu--report 'pending)
    (agent-repl-page-unload-pass)
    (setq agent-repl-page-unload--shown "alpha")
    ;; Act
    (funcall (nth 1 (car agent-repl-test-pu--pending)))
    ;; Assert
    (should (equal (list agent-repl-test-pu--dropped (agent-repl-page-unloaded-p "alpha")
                         (agent-repl--ws-get "alpha" :page-unloading))
                   '(nil nil nil)))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.kept" "ws=alpha" "why=in-use-since-report"))))

(ert-deftest agent-repl-test-pu-an-idle-page-within-the-hold-is-held ()
  "An idle page loaded within the hold stays."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 900)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-an-idle-page-past-the-hold-is-unloaded ()
  "An idle page held longer than the hold since its load is unloaded, and recorded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 400)
    ;; Act
    (let ((unloaded (agent-repl-page-unload-pass)))
      ;; Assert
      (should (equal (list unloaded agent-repl-test-pu--dropped) '(1 ("alpha"))))
      (should (agent-repl-test-pu--said-p "elisp.page-unload.unloaded" "ws=alpha" "reason=idle")))))

(ert-deftest agent-repl-test-pu-an-idle-hold-runs-from-the-last-turn-end ()
  "A turn ended after the page loaded restarts the idle hold."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 0)
    (setq agent-repl-test-pu--turn-ended '(("alpha" . 900000)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-a-working-page-is-held-however-old ()
  "A working workspace's page is never unloaded off screen."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 0)
    (setq agent-repl-test-pu--status '(("alpha" . :idle-async)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-a-page-with-no-row-yet-is-held ()
  "A workspace with no roster row yet decides nothing."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 0)
    (setq agent-repl-test-pu--status '(("alpha" . nil)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-a-faulted-page-is-unloaded-at-once ()
  "A faulted workspace's page is unloaded off screen however freshly loaded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 1000)
    (setq agent-repl-test-pu--status '(("alpha" . :vendor-fault)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal agent-repl-test-pu--dropped '("alpha")))
    (should (agent-repl-test-pu--said-p "reason=faulted"))))

(ert-deftest agent-repl-test-pu-a-cold-gated-page-is-unloaded-at-once ()
  "A cold-gated workspace's page is unloaded the moment it is gated."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 1000)
    (setq agent-repl-test-pu--status '(("alpha" . :cold-gate)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal agent-repl-test-pu--dropped '("alpha")))
    (should (agent-repl-test-pu--said-p "reason=cold-gated"))))

(ert-deftest agent-repl-test-pu-a-merging-page-is-unloaded-off-screen ()
  "A merging workspace's page is held only while shown."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 1000)
    (setq agent-repl-test-pu--status '(("alpha" . :merging)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (agent-repl-test-pu--said-p "ws=alpha" "reason=merging"))))

(ert-deftest agent-repl-test-pu-a-transitional-page-is-unloaded-off-screen ()
  "An opening, closing or waiting workspace's page is held only while shown."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 1000)
    (setq agent-repl-test-pu--status '(("alpha" . :waiting)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (agent-repl-test-pu--said-p "ws=alpha" "reason=transitional"))))

(ert-deftest agent-repl-test-pu-a-merged-page-is-unloaded ()
  "A merged workspace's page is unloaded at once."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 1000)
    (setq agent-repl-test-pu--status '(("alpha" . :merged)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (agent-repl-test-pu--said-p "ws=alpha" "reason=merged"))))

(ert-deftest agent-repl-test-pu-a-hidden-workspace-page-is-unloaded-at-once ()
  "A hidden workspace's page is unloaded off screen, working or not."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 1000)
    (setq agent-repl-test-pu--status '(("alpha" . :thinking))
          agent-repl-test-pu--hidden '("alpha"))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal agent-repl-test-pu--dropped '("alpha")))
    (should (agent-repl-test-pu--said-p "reason=hidden"))))

(ert-deftest agent-repl-test-pu-the-shown-page-is-never-unloaded ()
  "The page the host shows stays, whatever its status."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :severed))
          agent-repl-page-unload--shown "alpha")
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-the-current-workspace-page-is-never-unloaded ()
  "The current workspace's page stays even before the host has shown it."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :severed))
          agent-repl-test-pu--current "alpha")
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-a-page-whose-open-is-in-flight-is-held ()
  "A page the daemon's open of its tab is waiting to see drawn is not unloaded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :inactive))
          agent-repl-test-pu--opening '("alpha"))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-nothing-is-unloaded-during-a-handover ()
  "While a handover's successor is attached, no page is unloaded: the successor awaits it."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :inactive))
          agent-repl-test-pu--successor 'successor-connection)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-a-drop-a-handover-overtook-is-kept ()
  "A drop whose report lands after a handover's successor attached keeps the page."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :severed))
          agent-repl-test-pu--report 'pending)
    (agent-repl-page-unload-pass)
    (setq agent-repl-test-pu--successor 'successor-connection)
    ;; Act
    (funcall (nth 1 (car agent-repl-test-pu--pending)))
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-a-workspace-with-no-page-is-skipped ()
  "A workspace holding no slot has nothing to unload."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (kill-buffer (agent-repl--ws-get "alpha" :frontend-buffer))
    (setq agent-repl-test-pu--status '(("alpha" . :severed)))
    ;; Act
    (let ((unloaded (agent-repl-page-unload-pass)))
      ;; Assert
      (should (equal (list unloaded agent-repl-test-pu--dropped) '(0 nil))))))

;;;; ---- A working workspace's page loads in the background ----

(ert-deftest agent-repl-test-pu-an-unloaded-working-page-is-loaded ()
  "A workspace that starts working while unloaded gets its page back, recorded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil
          agent-repl-test-pu--status '(("alpha" . :submitting)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal (list agent-repl-test-pu--events (agent-repl--ws-get "alpha" :page-loaded-at))
                   '(((ensure "alpha")) 1000)))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.loaded" "ws=alpha" "reason=working"))))

(ert-deftest agent-repl-test-pu-an-unloaded-idle-page-is-not-loaded ()
  "An idle workspace's page waits for its show."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--events)))

(ert-deftest agent-repl-test-pu-an-unloaded-hidden-working-page-is-not-loaded ()
  "A hidden workspace is loaded by no status."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil
          agent-repl-test-pu--status '(("alpha" . :thinking))
          agent-repl-test-pu--hidden '("alpha"))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--events)))

;;;; ---- The switch to, and away ----

(ert-deftest agent-repl-test-pu-an-unload-keeps-the-slot ()
  "An unload drops the page and keeps the slot, whose windows are the panels.
2026-10-24: killing the slot with the page left the workspace's saved layout
naming a dead buffer, and every switch back found its panels closed."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :severed)))
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
    (setq agent-repl-test-pu--status '(("alpha" . :severed)))
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

(ert-deftest agent-repl-test-pu-showing-an-unloaded-idle-page-puts-it-back-as-a-visit ()
  "Showing an unloaded idle page's slot puts the page back, marked a visit."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil)
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should (equal (list agent-repl-test-pu--events
                         (agent-repl-page-unloaded-p "alpha")
                         (agent-repl--ws-get "alpha" :page-visit)
                         (agent-repl--ws-get "alpha" :page-loaded-at))
                   '(((ensure "alpha")) nil t 1000)))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.returned" "ws=alpha" "visit=t"))))

(ert-deftest agent-repl-test-pu-showing-an-unloaded-working-page-is-no-visit ()
  "A working workspace's page put back by its show is held as working."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil
          agent-repl-test-pu--status '(("alpha" . :thinking)))
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should-not (agent-repl--ws-get "alpha" :page-visit))))

(ert-deftest agent-repl-test-pu-showing-a-held-page-puts-nothing-back ()
  "Showing a page the host still holds sends it nothing."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should-not agent-repl-test-pu--events)))

(ert-deftest agent-repl-test-pu-switching-away-from-a-visit-unloads-it-at-once ()
  "A page loaded only for a visit goes the moment the user switches away."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (setq agent-repl-test-pu--pages '("beta"))
    (agent-repl-page-unload-note-shown "alpha")
    (setq agent-repl-test-pu--events nil)
    ;; Act
    (agent-repl-page-unload-note-shown "beta")
    ;; Assert
    (should (equal (reverse agent-repl-test-pu--events) '((report "alpha") (drop "alpha"))))
    (should (agent-repl-test-pu--said-p "ws=alpha" "reason=visited"))))

(ert-deftest agent-repl-test-pu-a-visit-that-started-working-is-kept ()
  "A workspace prompted during the visit is working, so its page stays."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (setq agent-repl-test-pu--pages '("beta"))
    (agent-repl-page-unload-note-shown "alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :submitting)))
    ;; Act
    (agent-repl-page-unload-note-shown "beta")
    ;; Assert
    (should (equal (list agent-repl-test-pu--dropped (agent-repl--ws-get "alpha" :page-visit))
                   '(nil nil)))))

(ert-deftest agent-repl-test-pu-a-visit-whose-turn-ended-is-no-visit-again ()
  "A visit cleared by working stays cleared when the turn ends: the page ages out."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (setq agent-repl-test-pu--pages '("beta"))
    (agent-repl-page-unload-note-shown "alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :thinking)))
    (agent-repl-page-unload-note-shown "beta")
    (setq agent-repl-test-pu--status nil)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-switching-away-from-a-held-idle-page-keeps-it ()
  "An idle page that was not a visit stays within its hold."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl--ws-put "alpha" :page-loaded-at 900)
    (agent-repl-page-unload-note-shown "alpha")
    ;; Act
    (agent-repl-page-unload-note-shown "beta")
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

(ert-deftest agent-repl-test-pu-switching-away-from-a-faulted-page-unloads-it ()
  "A faulted page the user stood on goes the moment they switch away."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (setq agent-repl-test-pu--status '(("alpha" . :turn-died)))
    (agent-repl-page-unload-note-shown "alpha")
    ;; Act
    (agent-repl-page-unload-note-shown "beta")
    ;; Assert
    (should (member "alpha" agent-repl-test-pu--dropped))))

(ert-deftest agent-repl-test-pu-showing-the-same-page-again-judges-nothing ()
  "A repeated show of the page already shown unloads nothing."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :severed)))
    (agent-repl-page-unload-note-shown "alpha")
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should-not agent-repl-test-pu--dropped)))

;;;; ---- Residency, as reported to the daemon ----

(defun agent-repl-test-pu--release (ws)
  "WS's residency's release arm, or `:not-loaded'."
  (let ((residency (agent-repl-page-unload-residency ws)))
    (if (eq (plist-get residency :arm) :not-loaded)
        :not-loaded
      (plist-get (plist-get (plist-get residency :value) :release) :arm))))

(ert-deftest agent-repl-test-pu-a-page-not-held-is-not-loaded ()
  "A workspace whose page the host does not hold reports it not loaded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil)
    ;; Act / Assert
    (should (eq (agent-repl-test-pu--release "alpha") :not-loaded))))

(ert-deftest agent-repl-test-pu-a-working-page-is-held-in-its-residency ()
  "A working workspace's loaded page reports held."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :thinking)))
    ;; Act / Assert
    (should (eq (agent-repl-test-pu--release "alpha") :held))))

(ert-deftest agent-repl-test-pu-a-page-with-no-status-is-held-in-its-residency ()
  "A workspace with no roster row yet reports held: nothing decides it."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . nil)))
    ;; Act / Assert
    (should (eq (agent-repl-test-pu--release "alpha") :held))))

(ert-deftest agent-repl-test-pu-an-idle-page-reports-its-holds-end ()
  "An idle page that is no visit reports the instant its hold ends."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-loaded-at 900)
    ;; Act
    (let ((residency (agent-repl-page-unload-residency "alpha")))
      ;; Assert
      (should (equal (plist-get (plist-get residency :value) :release)
                     '(:arm :at :value (:unix-ms 1400000)))))))

(ert-deftest agent-repl-test-pu-an-idle-visit-reports-on-leave ()
  "An idle page loaded only for a visit reports that it goes on the switch away."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (agent-repl--ws-put "alpha" :page-visit t)
    ;; Act / Assert
    (should (eq (agent-repl-test-pu--release "alpha") :on-leave))))

(ert-deftest agent-repl-test-pu-a-faulted-page-reports-on-leave ()
  "A page its status holds only while shown reports on the switch away."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :severed)))
    ;; Act / Assert
    (should (eq (agent-repl-test-pu--release "alpha") :on-leave))))

(ert-deftest agent-repl-test-pu-a-hidden-working-page-reports-on-leave ()
  "A hidden workspace holds its page only while shown, working or not."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :thinking))
          agent-repl-test-pu--hidden '("alpha"))
    ;; Act / Assert
    (should (eq (agent-repl-test-pu--release "alpha") :on-leave))))

(ert-deftest agent-repl-test-pu-a-pass-reports-each-residency-once ()
  "A pass reports every live workspace's residency, and the next sends nothing unchanged."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :thinking)))
    (agent-repl-page-unload-pass)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal (mapcar #'car agent-repl-test-pu--residencies) '("alpha")))))

(ert-deftest agent-repl-test-pu-a-changed-residency-is-reported-again ()
  "A residency that changed since its report is sent again."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :thinking)))
    (agent-repl-page-unload-pass)
    (setq agent-repl-test-pu--status '(("alpha" . :severed)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (eq (plist-get (cadr (car agent-repl-test-pu--residencies)) :arm) :not-loaded))))

(ert-deftest agent-repl-test-pu-a-report-that-did-not-land-is-sent-again ()
  "A residency the daemon did not take is not marked sent."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :thinking))
          agent-repl-test-pu--residency-lands nil)
    (agent-repl-page-unload-pass)
    (setq agent-repl-test-pu--residency-lands t)
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (= (length agent-repl-test-pu--residencies) 2))))

(ert-deftest agent-repl-test-pu-a-workspace-with-no-slot-reports-nothing ()
  "A workspace with no slot has no page to report."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (kill-buffer (agent-repl--ws-get "alpha" :frontend-buffer))
    ;; Act
    (agent-repl-page-unload-report-all)
    ;; Assert
    (should-not agent-repl-test-pu--residencies)))

(ert-deftest agent-repl-test-pu-an-unload-reports-the-page-not-loaded ()
  "The drop's landing reports the page not loaded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :severed)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (equal (car agent-repl-test-pu--residencies) '("alpha" (:arm :not-loaded :value nil))))))

(ert-deftest agent-repl-test-pu-a-show-reports-the-shown-page ()
  "Showing a workspace reports its residency."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil)
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should (eq (plist-get (cadr (car agent-repl-test-pu--residencies)) :arm) :loaded))))

(ert-deftest agent-repl-test-pu-a-link-up-forgets-every-report ()
  "A link-up may reach a daemon holding no residency, so every report is sent again."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :thinking)))
    (agent-repl-page-unload-pass)
    (let ((agent-repl-page-unload--timer 'running))
      (cl-letf (((symbol-function 'timerp) (lambda (x) (eq x 'running))))
        (agent-repl-page-unload-start)))
    ;; Act
    (agent-repl-page-unload-pass)
    ;; Assert
    (should (= (length agent-repl-test-pu--residencies) 2))))

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
