;;; test-find-file-workspace.el --- ERT tests for find-file-workspace.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for routing a visited file into the workspace owning its git root.
;;
;; Git is NEVER run here: root detection is injected through
;; `agent-repl-find-file-workspace-root-function', and the workspace open and
;; create verbs are injected the same way, so no test reaches the daemon, a
;; temp repository, or a real `git' process.  Window placement is asserted on
;; REAL Emacs windows in the batch frame.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-find-file-workspace.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;; Declared special so this file byte-compiles standalone: every one of these
;; is defined by a production module `test-helpers.el' loads at RUN time, which
;; is after the compiler has already read the `let' bindings below.
(defvar persp-names-cache)
(defvar agent-repl-find-file-workspace-enabled)
(defvar agent-repl-find-file-workspace-root-function)
(defvar agent-repl-find-file-workspace-open-function)
(defvar agent-repl-find-file-workspace-create-function)

(defun agent-repl-test--ffw-home-path (relative)
  "Return the canonical form of RELATIVE under the user's home directory."
  (agent-repl--path-canonical (expand-file-name relative "~")))

(defmacro agent-repl-test--ffw-with-file-buffer (var name &rest body)
  "Bind VAR to a live buffer visiting NAME under home, run BODY, then kill it."
  (declare (indent 2))
  `(let ((,var (generate-new-buffer " *agent-repl-ffw-file*")))
     (unwind-protect
         (progn
           (with-current-buffer ,var
             (setq buffer-file-name (expand-file-name ,name "~")))
           ,@body)
       (with-current-buffer ,var (set-buffer-modified-p nil))
       (kill-buffer ,var))))

(defmacro agent-repl-test--ffw-with-one-window (&rest body)
  "Run BODY with a single-window frame layout, restoring the layout after."
  (declare (indent 0))
  `(let ((conf (current-window-configuration)))
     (unwind-protect
         (progn (delete-other-windows) ,@body)
       (set-window-configuration conf))))

;;;; ---- Root detection ----

(ert-deftest agent-repl-test-ffw-root-for-file-returns-the-injected-root ()
  "A git root under home is the root the file routes to."
  (agent-repl-test--with-clean-state
    (let* ((root (agent-repl-test--ffw-home-path "ffw-repo"))
           (agent-repl-find-file-workspace-root-function (lambda (_dir) root)))
      (should (equal (agent-repl--ffw-root-for-file
                      (expand-file-name "ffw-repo/a.el" "~"))
                     root)))))

(ert-deftest agent-repl-test-ffw-root-for-file-nil-when-no-root ()
  "A file with no git root is not routed."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-find-file-workspace-root-function (lambda (_dir) nil)))
      (should-not (agent-repl--ffw-root-for-file
                   (expand-file-name "ffw-repo/a.el" "~"))))))

(ert-deftest agent-repl-test-ffw-root-for-file-nil-when-root-outside-home ()
  "A git root outside the user's home is not routed."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-find-file-workspace-root-function
           (lambda (_dir) "/opt/ffw-elsewhere")))
      (should-not (agent-repl--ffw-root-for-file "/opt/ffw-elsewhere/a.el")))))

(ert-deftest agent-repl-test-ffw-under-home-p-accepts-home-itself ()
  "Home is at-or-below home, so home itself routes."
  (should (agent-repl--ffw-under-home-p (agent-repl--path-canonical "~"))))

(ert-deftest agent-repl-test-ffw-under-home-p-rejects-a-sibling-prefix ()
  "A path that merely shares home's string prefix is outside home."
  (should-not (agent-repl--ffw-under-home-p
               (concat (agent-repl--path-canonical "~") "-not-mine"))))

;;;; ---- Window selection ----

(ert-deftest agent-repl-test-ffw-candidate-windows-excludes-panel-windows ()
  "A window showing an agent-repl panel is never a placement candidate."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-one-window
      (let ((panel (window-buffer (selected-window))))
        (cl-letf (((symbol-function 'agent-repl--agent-panel-buffer-p)
                   (lambda (&optional buf) (eq buf panel))))
          (should-not (agent-repl--ffw-candidate-windows)))))))

(ert-deftest agent-repl-test-ffw-candidate-windows-keeps-an-ordinary-window ()
  "A window showing an ordinary buffer is a placement candidate."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-one-window
      (cl-letf (((symbol-function 'agent-repl--agent-panel-buffer-p)
                 (lambda (&optional _buf) nil)))
        (should (equal (agent-repl--ffw-candidate-windows)
                       (list (selected-window))))))))

(ert-deftest agent-repl-test-ffw-largest-window-picks-the-biggest ()
  "The larger of two real windows wins."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-one-window
      (let* ((big (selected-window))
             (small (split-window big -5 'below))
             (chosen (agent-repl--ffw-largest-window (list small big))))
        (should (eq chosen big))))))

(ert-deftest agent-repl-test-ffw-largest-window-nil-for-no-windows ()
  "No candidates, no window."
  (should-not (agent-repl--ffw-largest-window nil)))

;;;; ---- Placement ----

(ert-deftest agent-repl-test-ffw-place-uses-the-largest-non-panel-window ()
  "The file lands in the largest window that is not an agent-repl panel."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-one-window
      (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
        (let* ((big (selected-window))
               (small (split-window big -5 'below))
               (panel (generate-new-buffer " *agent-repl-ffw-panel*")))
          (unwind-protect
              (progn
                (set-window-buffer small panel)
                (cl-letf (((symbol-function 'agent-repl--agent-panel-buffer-p)
                           (lambda (&optional b) (eq b panel))))
                  (should (eq (agent-repl--ffw-place buf) big))
                  (should (eq (window-buffer big) buf))))
            (kill-buffer panel)))))))

(ert-deftest agent-repl-test-ffw-place-splits-left-when-only-panels ()
  "With nothing but panels, the file takes the LEFT half and panels the right."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-one-window
      (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
        (let* ((panel-win (selected-window))
               (panel (window-buffer panel-win)))
          (cl-letf (((symbol-function 'agent-repl--agent-panel-buffer-p)
                     (lambda (&optional b) (eq b panel))))
            (let ((win (agent-repl--ffw-place buf)))
              (should (eq (window-buffer win) buf))
              (should (< (window-left-column win)
                         (window-left-column panel-win))))))))))

;;;; ---- Routing ----

(ert-deftest agent-repl-test-ffw-route-switches-to-an-open-workspace ()
  "An OPEN workspace owning the root is switched to and the file placed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-one-window
      (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
        (let* ((root (agent-repl-test--ffw-home-path "ffw-repo"))
               (agent-repl-find-file-workspace-root-function (lambda (_dir) root))
               (persp-names-cache '("ffw-ws"))
               (switched nil))
          (agent-repl--ws-put "ffw-ws" :project-dir root)
          (cl-letf (((symbol-function 'agent-repl--ws-switch)
                     (lambda (ws &rest _) (setq switched ws)))
                    ((symbol-function 'agent-repl--ws-current-name)
                     (lambda () "other"))
                    ((symbol-function 'agent-repl--agent-panel-buffer-p)
                     (lambda (&optional _b) nil)))
            (should (agent-repl--ffw-route buf))
            (should (equal switched "ffw-ws"))))))))

(ert-deftest agent-repl-test-ffw-route-opens-a-closed-workspace ()
  "A workspace that exists but is not open is asked to open first."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
      (let* ((root (agent-repl-test--ffw-home-path "ffw-repo"))
             (agent-repl-find-file-workspace-root-function (lambda (_dir) root))
             (persp-names-cache nil)
             (opened nil)
             (agent-repl-find-file-workspace-open-function
              (lambda (ws) (setq opened ws))))
        (agent-repl--ws-put "ffw-ws" :project-dir root)
        (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "other")))
          (agent-repl--ffw-route buf)
          (should (equal opened "ffw-ws")))))))

(ert-deftest agent-repl-test-ffw-route-creates-a-missing-workspace ()
  "No workspace owns the root, so one is created for it."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
      (let* ((root (agent-repl-test--ffw-home-path "ffw-repo"))
             (agent-repl-find-file-workspace-root-function (lambda (_dir) root))
             (persp-names-cache nil)
             (created nil)
             (agent-repl-find-file-workspace-create-function
              (lambda (dir) (setq created dir))))
        (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "other")))
          (agent-repl--ffw-route buf)
          (should (equal created root)))))))

(ert-deftest agent-repl-test-ffw-route-does-nothing-when-already-here ()
  "The file already current in its own workspace is left exactly alone."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
      (let* ((root (agent-repl-test--ffw-home-path "ffw-repo"))
             (agent-repl-find-file-workspace-root-function (lambda (_dir) root))
             (persp-names-cache '("ffw-ws"))
             (switched nil))
        (agent-repl--ws-put "ffw-ws" :project-dir root)
        (with-current-buffer buf
          (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                     (lambda () "ffw-ws"))
                    ((symbol-function 'agent-repl--ws-switch)
                     (lambda (ws &rest _) (setq switched ws))))
            (should-not (agent-repl--ffw-route buf))
            (should-not switched)))))))

(ert-deftest agent-repl-test-ffw-route-nil-for-a-file-with-no-root ()
  "A file with no git root is not routed at all."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
      (let ((agent-repl-find-file-workspace-root-function (lambda (_dir) nil)))
        (should-not (agent-repl--ffw-route buf))))))

(ert-deftest agent-repl-test-ffw-switch-and-place-nil-for-a-closed-workspace ()
  "A workspace whose tab never opened yields no placement."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
      (let ((persp-names-cache nil))
        (agent-repl--ws-put "ffw-ws" :project-dir "/tmp/ffw")
        (should-not (agent-repl--ffw-switch-and-place "ffw-ws" buf))))))

;;;; ---- The display advice ----

(ert-deftest agent-repl-test-ffw-advice-routes-a-file-buffer ()
  "A routed display answers with the buffer and never calls the primitive."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
      (let ((orig-calls 0))
        (cl-letf (((symbol-function 'agent-repl--ffw-route) (lambda (_b) 'placed)))
          (should (eq (agent-repl--ffw-display-advice
                       (lambda (&rest _) (cl-incf orig-calls)) buf)
                      buf))
          (should (= orig-calls 0)))))))

(ert-deftest agent-repl-test-ffw-advice-falls-through-when-routing-declines ()
  "An unrouted display falls through to the original primitive."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
      (let ((orig-calls 0))
        (cl-letf (((symbol-function 'agent-repl--ffw-route) (lambda (_b) nil)))
          (agent-repl--ffw-display-advice
           (lambda (&rest _) (cl-incf orig-calls)) buf)
          (should (= orig-calls 1)))))))

(ert-deftest agent-repl-test-ffw-advice-falls-through-when-disabled ()
  "The defcustom off means the primitive behaves exactly as it does unadvised."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
      (let ((orig-calls 0)
            (agent-repl-find-file-workspace-enabled nil))
        (cl-letf (((symbol-function 'agent-repl--ffw-route)
                   (lambda (_b) (error "routing must not run when disabled"))))
          (agent-repl--ffw-display-advice
           (lambda (&rest _) (cl-incf orig-calls)) buf)
          (should (= orig-calls 1)))))))

(ert-deftest agent-repl-test-ffw-advice-is-non-recursive ()
  "The display the routing itself performs must not re-enter the routing."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
      (let ((route-calls 0)
            (orig-calls 0))
        (cl-letf (((symbol-function 'agent-repl--ffw-route)
                   (lambda (b)
                     (cl-incf route-calls)
                     ;; The placement re-enters the advised primitive.
                     (agent-repl--ffw-display-advice
                      (lambda (&rest _) (cl-incf orig-calls)) b)
                     'placed)))
          (agent-repl--ffw-display-advice (lambda (&rest _) nil) buf)
          (should (= route-calls 1))
          (should (= orig-calls 1)))))))

(ert-deftest agent-repl-test-ffw-advice-ignores-a-non-file-buffer ()
  "A buffer visiting no file is never routed."
  (agent-repl-test--with-clean-state
    (let ((plain (generate-new-buffer " *agent-repl-ffw-plain*"))
          (orig-calls 0))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ffw-route)
                     (lambda (_b) (error "a non-file buffer must not route"))))
            (agent-repl--ffw-display-advice
             (lambda (&rest _) (cl-incf orig-calls)) plain)
            (should (= orig-calls 1)))
        (kill-buffer plain)))))

(ert-deftest agent-repl-test-ffw-target-buffer-resolves-a-buffer-name ()
  "A string naming a live file buffer resolves to that buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--ffw-with-file-buffer buf "ffw-repo/a.el"
      (should (eq (agent-repl--ffw-target-buffer (buffer-name buf)) buf)))))

(ert-deftest agent-repl-test-ffw-target-buffer-nil-for-a-filename-string ()
  "A filename handed to a display primitive names no buffer and is not routed."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--ffw-target-buffer "~/no-such-agent-repl-buffer.el"))))

(provide 'test-find-file-workspace)

;;; test-find-file-workspace.el ends here
