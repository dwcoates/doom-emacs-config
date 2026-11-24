;;; test-page-unload.el --- ERT tests for page-unload.el -*- lexical-binding: t; -*-

;;; Commentary:

;; A page is loaded only while its workspace is visible, which is every
;; workspace this editor holds, and while its session runs or it is shown;
;; a cold-gated page is held back until the user asks for it; and every drop of a page whose workspace stays open is
;; announced first.  The harness's host holds `agent-repl-test-pu--pages',
;; and its roster `agent-repl-test-pu--status'.
;;
;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-page-unload.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Harness ----

(defvar agent-repl-test-pu--dropped nil
  "Workspaces whose page was dropped, newest first.")

(defvar agent-repl-test-pu--pages nil
  "Workspaces whose page the harness's host holds.")

(defvar agent-repl-test-pu--held-back nil
  "Workspaces whose page the harness's host holds back.")

(defvar agent-repl-test-pu--status nil
  "Each workspace's roster status arm, (WS . ARM); a workspace absent is `:ready'.
\(WS . nil) is a workspace with no roster row yet.")

(defvar agent-repl-test-pu--dormant nil
  "Workspaces whose roster row carries the parked marker.")

(defvar agent-repl-test-pu--logged nil
  "Every INFO record's text, newest first.")

(defvar agent-repl-test-pu--report 'reported
  "How the harness's daemon answers an unload report.
`reported' answers success at once, `refused' answers failure at once, and
`pending' holds the answer in `agent-repl-test-pu--pending'.")

(defvar agent-repl-test-pu--pending nil
  "The held reports, newest first, each (WS ON-REPORTED ON-NOT-REPORTED).")

(defvar agent-repl-test-pu--events nil
  "Every report, dropped page, page put back and page held back, newest first.")

(defmacro agent-repl-test-pu--with (names &rest body)
  "Run BODY with live workspaces NAMES, each holding a slot and a page."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-test-pu--dropped nil)
           (agent-repl-test-pu--status nil)
           (agent-repl-test-pu--dormant nil)
           (agent-repl-test-pu--logged nil)
           (agent-repl-test-pu--report 'reported)
           (agent-repl-test-pu--pending nil)
           (agent-repl-test-pu--events nil)
           (agent-repl-test-pu--pages (copy-sequence ,names))
           (agent-repl-test-pu--held-back nil)
           (agent-repl-page-unload--shown nil)
           (buffers nil))
       (unwind-protect
           (progn
             (dolist (ws ,names)
               (let ((buf (generate-new-buffer (format " *test-pu-%s*" ws))))
                 (push buf buffers)
                 (agent-repl--ws-put ws :frontend-buffer buf)))
             (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () ,names))
                       ((symbol-function 'agent-repl-roster-status-for-ws)
                        (lambda (ws) (alist-get ws agent-repl-test-pu--status :ready nil #'equal)))
                       ((symbol-function 'agent-repl-roster-dormant-p)
                        (lambda (ws) (and (member ws agent-repl-test-pu--dormant) t)))
                       ((symbol-function 'agent-repl-host-report-page-unloading)
                        (agent-repl-test--page-unloading-reporter
                         (lambda () agent-repl-test-pu--report)
                         (lambda (ws) (push (list 'report ws) agent-repl-test-pu--events))
                         (lambda (held) (push held agent-repl-test-pu--pending))))
                       ((symbol-function 'agent-repl-webhost-has-page-p)
                        (lambda (ws) (and (member ws agent-repl-test-pu--pages) t)))
                       ((symbol-function 'agent-repl-webhost-remove)
                        (lambda (ws &optional _unloading)
                          (push (list 'drop ws) agent-repl-test-pu--events)
                          (push ws agent-repl-test-pu--dropped)
                          (setq agent-repl-test-pu--pages (delete ws agent-repl-test-pu--pages))))
                       ((symbol-function 'agent-repl-webhost-ensure-page)
                        (lambda (ws _why)
                          (push (list 'ensure ws) agent-repl-test-pu--events)
                          (setq agent-repl-test-pu--held-back (delete ws agent-repl-test-pu--held-back))
                          (cl-pushnew ws agent-repl-test-pu--pages :test #'equal)))
                       ((symbol-function 'agent-repl-webhost-hold-back)
                        (lambda (ws)
                          (push (list 'hold-back ws) agent-repl-test-pu--events)
                          (cl-pushnew ws agent-repl-test-pu--held-back :test #'equal)))
                       ((symbol-function 'agent-repl-webhost-held-back-p)
                        (lambda (ws) (and (member ws agent-repl-test-pu--held-back) t)))
                       ((symbol-function 'agent-repl--emit-info)
                        (agent-repl-test--record-into agent-repl-test-pu--logged)))
               ,@body))
         (mapc #'kill-buffer (seq-filter #'buffer-live-p buffers))))))

(defun agent-repl-test-pu--said-p (&rest parts)
  "Non-nil when one INFO record holds every one of PARTS."
  (seq-some (lambda (text) (seq-every-p (lambda (part) (string-search part text)) parts))
            agent-repl-test-pu--logged))

;;;; ---- Page reports ----

(ert-deftest agent-repl-test-pu-a-load-reports-its-reason ()
  "The one load path tells the daemon why the page was loaded."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (let ((loads nil))
      (cl-letf (((symbol-function 'agent-repl-page-report-loaded)
                 (lambda (ws why) (push (list ws why) loads))))
        ;; Act
        (agent-repl-page-unload-load "alpha" 'awake)
        ;; Assert
        (should (equal loads '(("alpha" awake))))))))

(ert-deftest agent-repl-test-pu-a-load-holds-the-page-with-its-reason ()
  "The one load path hands the host the reason, kept with the page itself."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (let ((held nil))
      (cl-letf (((symbol-function 'agent-repl-webhost-ensure-page)
                 (lambda (ws why) (push (list ws why) held)))
                ((symbol-function 'agent-repl-page-report-loaded) #'ignore))
        ;; Act
        (agent-repl-page-unload-load "alpha" 'slot)
        ;; Assert
        (should (equal held '(("alpha" slot))))))))

(ert-deftest agent-repl-test-pu-a-load-for-an-unknown-reason-holds-nothing ()
  "A load for a reason no load path names signals before any page is held."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (let ((held nil)
          (errors nil))
      (cl-letf (((symbol-function 'agent-repl-webhost-ensure-page)
                 (lambda (ws why) (push (list ws why) held)))
                ((symbol-function 'agent-repl--emit-error)
                 (agent-repl-test--record-into errors)))
        ;; Act
        (should-error (agent-repl-page-unload-load "alpha" 'whim))
        ;; Assert
        (should-not held)
        (should (seq-some (lambda (text) (string-search "unknown-reason ws=alpha why=whim" text)) errors))))))

(ert-deftest agent-repl-test-pu-a-dormant-release-reports-its-unload ()
  "A page released dormant off screen is reported unloaded as dormant."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (let ((unloads nil))
      (cl-letf (((symbol-function 'agent-repl-page-report-unloaded)
                 (lambda (ws why) (push (list ws why) unloads))))
        (push "alpha" agent-repl-test-pu--dormant)
        (agent-repl-page-unload-note-shown "alpha")
        ;; Act
        (agent-repl-page-unload-note-shown "beta")
        ;; Assert
        (should (equal unloads '(("alpha" dormant))))))))

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


;;;; ---- Loading the module ----

(ert-deftest agent-repl-test-pu-a-hot-reload-touches-no-page ()
  "Loading page-unload.el again loads, drops and reports no page."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (setq agent-repl-test-pu--pages nil)
    ;; Act
    (load (symbol-file (quote agent-repl-page-unload-note-shown) (quote defun)) nil t)
    ;; Assert
    (should (null agent-repl-test-pu--events))))

(ert-deftest agent-repl-test-pu-no-pass-runs-on-a-clock ()
  "Nothing weighs a page on a timer: the visibility alone decides, so the
module defines no pass, no hold and no grace."
  (dolist (retired '(agent-repl-page-unload-pass agent-repl-page-unload-start
                     agent-repl-page-unload-after agent-repl-page-unload-leave-grace
                     agent-repl-page-unload-working-cap agent-repl-page-unload-residency))
    (should-not (or (fboundp retired) (boundp retired)))))

;;;; ---- Which pages are wanted ----

(ert-deftest agent-repl-test-pu-a-held-workspaces-page-is-wanted-whatever-its-status ()
  "Every workspace held here is visible, so its page is wanted, idle or working."
  ;; Arrange
  (agent-repl-test-pu--with '("idle" "working" "faulted" "merged")
    (setq agent-repl-test-pu--status '(("working" . :thinking) ("faulted" . :vendor-fault)
                                       ("merged" . :merged)))
    ;; Act / Assert
    (should (seq-every-p #'agent-repl-page-unload-wanted-p '("idle" "working" "faulted" "merged")))))

(ert-deftest agent-repl-test-pu-a-page-with-no-row-yet-is-wanted ()
  "A workspace with no roster row yet stands at no gate, so its page is wanted."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . nil)))
    ;; Act / Assert
    (should (agent-repl-page-unload-wanted-p "alpha"))))

(ert-deftest agent-repl-test-pu-a-cold-gated-page-is-not-wanted ()
  "A cold-gated workspace's page is held back until the user asks for it."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :cold-gate)))
    ;; Act / Assert
    (should-not (agent-repl-page-unload-wanted-p "alpha"))))

;;;; ---- Showing a page ----

(ert-deftest agent-repl-test-pu-showing-an-unloaded-page-puts-it-back ()
  "Showing an unloaded page's slot puts the page back."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil)
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should (equal (list agent-repl-test-pu--events (agent-repl-page-unloaded-p "alpha"))
                   '(((ensure "alpha")) nil)))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.loaded" "ws=alpha" "reason=shown"))))

(ert-deftest agent-repl-test-pu-showing-an-unloaded-cold-gated-page-holds-it-back ()
  "Showing a cold-gated workspace's unloaded page holds it back instead of loading it."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil
          agent-repl-test-pu--status '(("alpha" . :cold-gate)))
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should (equal (list agent-repl-test-pu--events (agent-repl-page-unloaded-p "alpha"))
                   '(((hold-back "alpha")) t)))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.held-back" "ws=alpha"))))

(ert-deftest agent-repl-test-pu-showing-a-loaded-cold-gated-page-holds-nothing-back ()
  "A cold-gated workspace whose page is still loaded shows that page."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :cold-gate)))
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should-not agent-repl-test-pu--events)))

(ert-deftest agent-repl-test-pu-showing-a-held-page-puts-nothing-back ()
  "Showing a page the host still holds sends it nothing."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    ;; Act
    (agent-repl-page-unload-note-shown "alpha")
    ;; Assert
    (should-not agent-repl-test-pu--events)))

(ert-deftest agent-repl-test-pu-switching-away-drops-no-page ()
  "Switching to another page leaves the page left loaded: it is still visible."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-page-unload-note-shown "alpha")
    ;; Act
    (agent-repl-page-unload-note-shown "beta")
    ;; Assert
    (should (equal (list agent-repl-test-pu--events agent-repl-page-unload--shown)
                   '(nil "beta")))))

;;;; ---- The load button ----

(ert-deftest agent-repl-test-pu-a-load-request-loads-the-held-back-page ()
  "The load button's click puts the held-back page back."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil
          agent-repl-test-pu--status '(("alpha" . :cold-gate)))
    (agent-repl-page-unload-note-shown "alpha")
    (setq agent-repl-test-pu--events nil)
    ;; Act
    (agent-repl-page-unload-load-requested "alpha")
    ;; Assert
    (should (equal (list agent-repl-test-pu--events
                         (agent-repl-page-unloaded-p "alpha")
                         agent-repl-test-pu--held-back)
                   '(((ensure "alpha")) nil nil)))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.load-requested" "ws=alpha" "outcome=loaded"))))

(ert-deftest agent-repl-test-pu-a-load-request-for-a-loaded-page-loads-nothing ()
  "A click that crossed a load already made has nothing left to load."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--status '(("alpha" . :cold-gate)))
    ;; Act
    (agent-repl-page-unload-load-requested "alpha")
    ;; Assert
    (should-not agent-repl-test-pu--events)
    (should (agent-repl-test-pu--said-p "elisp.page-unload.load-requested" "ws=alpha"
                                        "outcome=already-loaded"))))

;;;; ---- A gate answered on screen ----

(ert-deftest agent-repl-test-pu-a-roster-push-loads-a-shown-held-back-page-once-its-gate-is-gone ()
  "A gate answered while its page is held back on screen loads the page."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil
          agent-repl-test-pu--status '(("alpha" . :cold-gate)))
    (agent-repl-page-unload-note-shown "alpha")
    (setq agent-repl-test-pu--events nil
          agent-repl-test-pu--status '(("alpha" . :ready)))
    ;; Act
    (let ((loaded (agent-repl-page-unload-on-roster-update)))
      ;; Assert
      (should (equal (list loaded agent-repl-test-pu--events) '(1 ((ensure "alpha"))))))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.loaded" "ws=alpha" "reason=ungated"))))

(ert-deftest agent-repl-test-pu-a-roster-push-keeps-a-shown-page-held-back-while-gated ()
  "A roster push loads nothing for a page held back while its gate still stands."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--pages nil
          agent-repl-test-pu--status '(("alpha" . :cold-gate)))
    (agent-repl-page-unload-note-shown "alpha")
    (setq agent-repl-test-pu--events nil)
    ;; Act
    (agent-repl-page-unload-on-roster-update)
    ;; Assert
    (should-not agent-repl-test-pu--events)))

(ert-deftest agent-repl-test-pu-a-roster-push-loads-no-page-held-back-off-screen ()
  "A page held back and left, its session dormant, is loaded only at its next show."
  ;; Arrange
  (agent-repl-test-pu--with '("alpha" "beta")
    (setq agent-repl-test-pu--pages '("beta")
          agent-repl-test-pu--status '(("alpha" . :cold-gate)))
    (agent-repl-page-unload-note-shown "alpha")
    (agent-repl-page-unload-note-shown "beta")
    (setq agent-repl-test-pu--events nil
          agent-repl-test-pu--status nil
          agent-repl-test-pu--dormant '("alpha"))
    ;; Act
    (agent-repl-page-unload-on-roster-update)
    ;; Assert
    (should-not (assoc 'ensure agent-repl-test-pu--events))))

(ert-deftest agent-repl-test-pu-a-roster-push-is-followed ()
  "Every roster push reaches the gate check."
  (should (memq #'agent-repl-page-unload-on-roster-update agent-repl-roster-update-functions)))

(provide 'test-page-unload)
;;; test-page-unload.el ends here

;;;; ---- A parked workspace holds no page unless it is shown ----

(ert-deftest agent-repl-test-page-unload-a-parked-workspaces-page-is-not-wanted ()
  "A parked workspace's slot is made with no page; its show loads it."
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--dormant '("alpha"))
    (should-not (agent-repl-page-unload-wanted-p "alpha"))))

(ert-deftest agent-repl-test-page-unload-a-live-workspaces-page-is-wanted ()
  "A workspace whose session runs on a shim has its page loaded at once."
  (agent-repl-test-pu--with '("alpha")
    (should (agent-repl-page-unload-wanted-p "alpha"))))

(ert-deftest agent-repl-test-page-unload-a-park-off-screen-unloads-the-page ()
  "A push that parks a workspace the host does not show unloads its page, announced."
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-page-unload-note-shown "beta")
    (setq agent-repl-test-pu--dormant '("alpha"))
    (agent-repl-page-unload-on-roster-update)
    (should (equal (reverse agent-repl-test-pu--events)
                   '((report "alpha") (drop "alpha"))))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.released" "alpha"))))

(ert-deftest agent-repl-test-page-unload-a-park-on-screen-keeps-the-page ()
  "The workspace the host shows keeps its page when its session parks."
  (agent-repl-test-pu--with '("alpha")
    (agent-repl-page-unload-note-shown "alpha")
    (setq agent-repl-test-pu--dormant '("alpha"))
    (agent-repl-page-unload-on-roster-update)
    (should-not agent-repl-test-pu--events)))

(ert-deftest agent-repl-test-page-unload-a-switch-away-from-a-parked-workspace-unloads-it ()
  "Showing another workspace unloads the parked one shown before it."
  (agent-repl-test-pu--with '("alpha" "beta")
    (setq agent-repl-test-pu--dormant '("alpha"))
    (agent-repl-page-unload-note-shown "alpha")
    (agent-repl-page-unload-note-shown "beta")
    (should (equal (reverse agent-repl-test-pu--events)
                   '((report "alpha") (drop "alpha"))))))

(ert-deftest agent-repl-test-page-unload-a-switch-away-from-a-live-workspace-keeps-it ()
  "Showing another workspace leaves a live one's page loaded."
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-page-unload-note-shown "alpha")
    (agent-repl-page-unload-note-shown "beta")
    (should-not agent-repl-test-pu--events)))

(ert-deftest agent-repl-test-page-unload-showing-a-parked-workspace-loads-its-page ()
  "A parked workspace's unloaded page comes back when the user shows it."
  (agent-repl-test-pu--with '("alpha")
    (setq agent-repl-test-pu--dormant '("alpha")
          agent-repl-test-pu--pages nil)
    (agent-repl-page-unload-note-shown "alpha")
    (should (equal agent-repl-test-pu--events '((ensure "alpha"))))))

(ert-deftest agent-repl-test-page-unload-a-keepalive-restore-off-screen-loads-nothing ()
  "A session up only for its keep-alive stays dormant, and loads no page."
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-page-unload-note-shown "beta")
    (setq agent-repl-test-pu--pages '("beta")
          agent-repl-test-pu--dormant '("alpha"))
    (agent-repl-page-unload-on-roster-update)
    (should-not agent-repl-test-pu--events)))

(ert-deftest agent-repl-test-page-unload-a-session-up-for-work-off-screen-loads-its-page ()
  "A session that comes up for work while visible loads its unloaded page."
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-page-unload-note-shown "beta")
    (setq agent-repl-test-pu--pages '("beta"))
    (should (= (agent-repl-page-unload-on-roster-update) 1))
    (should (equal agent-repl-test-pu--events '((ensure "alpha"))))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.loaded" "ws=alpha" "reason=awake"))))

(ert-deftest agent-repl-test-page-unload-a-gated-session-up-off-screen-loads-nothing ()
  "A session standing at its cold gate loads no page when it comes up."
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-page-unload-note-shown "beta")
    (setq agent-repl-test-pu--pages '("beta")
          agent-repl-test-pu--status '(("alpha" . :cold-gate)))
    (agent-repl-page-unload-on-roster-update)
    (should-not agent-repl-test-pu--events)))

(defun agent-repl-test-pu--calls-p (form fn)
  "Non-nil when FORM calls FN anywhere inside it, quoted forms aside."
  (cond
   ((not (consp form)) nil)
   ((eq (car form) 'quote) nil)
   ((eq (car form) fn) t)
   (t (let ((found nil) (rest form))
        (while (and (consp rest) (not found))
          (setq found (agent-repl-test-pu--calls-p (car rest) fn)
                rest (cdr rest)))
        found))))

(ert-deftest agent-repl-test-page-unload-only-the-one-loader-loads-a-page ()
  "`agent-repl-page-unload-load' is the one caller of the host's page load.
A second caller would load a page at a moment the page rule does not name."
  ;; Act
  (let (callers)
    (dolist (form agent-repl-test-pu--source-forms)
      (when (and (consp form) (memq (car form) '(defun defmacro cl-defun))
                 (agent-repl-test-pu--calls-p (cdddr form) 'agent-repl-webhost-ensure-page))
        (push (cadr form) callers)))
    ;; Assert
    (should (equal callers '(agent-repl-page-unload-load)))))

(ert-deftest agent-repl-test-page-unload-the-loader-scan-catches-a-second-caller ()
  "The scan finds a call to the host's page load nested anywhere in a body."
  (should (agent-repl-test-pu--calls-p
           '(when x (let ((y 1)) (agent-repl-webhost-ensure-page ws)))
           'agent-repl-webhost-ensure-page))
  (should-not (agent-repl-test-pu--calls-p
               '(when x '(agent-repl-webhost-ensure-page ws))
               'agent-repl-webhost-ensure-page)))

(ert-deftest agent-repl-test-page-unload-a-release-unparked-during-its-report-keeps-the-page ()
  "A session that came up while the unload was reported keeps its page."
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-page-unload-note-shown "beta")
    (setq agent-repl-test-pu--dormant '("alpha")
          agent-repl-test-pu--report 'pending)
    (agent-repl-page-unload-on-roster-update)
    (setq agent-repl-test-pu--dormant nil)
    (funcall (nth 1 (car agent-repl-test-pu--pending)))
    (should (member "alpha" agent-repl-test-pu--pages))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.release-kept" "alpha" "changed"))))

(ert-deftest agent-repl-test-page-unload-a-release-in-flight-is-not-asked-again ()
  "A second push while the unload is reported asks for no second report."
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-page-unload-note-shown "beta")
    (setq agent-repl-test-pu--dormant '("alpha")
          agent-repl-test-pu--report 'pending)
    (agent-repl-page-unload-on-roster-update)
    (agent-repl-page-unload-on-roster-update)
    (should (equal agent-repl-test-pu--events '((report "alpha"))))))

(ert-deftest agent-repl-test-page-unload-a-release-the-daemon-was-not-told-of-keeps-the-page ()
  "An unload whose report fails keeps the page, and says why."
  (agent-repl-test-pu--with '("alpha" "beta")
    (agent-repl-page-unload-note-shown "beta")
    (setq agent-repl-test-pu--dormant '("alpha")
          agent-repl-test-pu--report 'refused)
    (agent-repl-page-unload-on-roster-update)
    (should (member "alpha" agent-repl-test-pu--pages))
    (should (agent-repl-test-pu--said-p "elisp.page-unload.release-kept" "alpha" "not-reported"))))
