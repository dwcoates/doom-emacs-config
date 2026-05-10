;;; test-drawer.el --- ERT tests for drawer.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the claude-repl workspace drawer.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-drawer.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Helpers ----

(defmacro claude-repl-drawer-test--with-buffer (&rest body)
  "Run BODY in a temporary drawer buffer with `claude-repl-drawer-mode' active."
  (declare (indent 0))
  `(let ((buf (generate-new-buffer " *test-drawer*")))
     (unwind-protect
         (with-current-buffer buf
           (claude-repl-drawer-mode)
           ,@body)
       (when (buffer-live-p buf) (kill-buffer buf)))))

(defun claude-repl-drawer-test--register (ws &rest props)
  "Register WS in `claude-repl--workspaces' with PROPS plist."
  (puthash ws (copy-sequence props) claude-repl--workspaces))

;;;; ---- Multi-select ----

(ert-deftest claude-repl-drawer-test-toggle-mark-adds-and-removes ()
  "`toggle-mark' adds the entry on first press, removes on second."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws1" :priority "p1")
    (claude-repl-drawer-test--register "ws2" :priority "p2")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (claude-repl-drawer-toggle-mark)
      (should (claude-repl-drawer--marked-p "ws1"))
      ;; toggle-mark auto-advances; come back and unmark.
      (claude-repl-drawer--goto-workspace-line "ws1")
      (claude-repl-drawer-toggle-mark)
      (should-not (claude-repl-drawer--marked-p "ws1")))))

(ert-deftest claude-repl-drawer-test-target-workspaces-falls-back-to-point ()
  "`--target-workspaces' returns just the entry at point when no marks."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws1" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (should (equal (claude-repl-drawer--target-workspaces) '("ws1"))))))

(ert-deftest claude-repl-drawer-test-target-workspaces-uses-marks-when-set ()
  "`--target-workspaces' returns the marked-set when non-empty (ignoring point)."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws1" :priority "p1")
    (claude-repl-drawer-test--register "ws2" :priority "p2")
    (claude-repl-drawer-test--register "ws3" :priority "p3")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--ensure-marked-set)
      (puthash "ws1" t claude-repl-drawer--marked-set)
      (puthash "ws3" t claude-repl-drawer--marked-set)
      (let ((targets (claude-repl-drawer--target-workspaces)))
        (should (= (length targets) 2))
        (should (member "ws1" targets))
        (should (member "ws3" targets))))))

(ert-deftest claude-repl-drawer-test-bulk-nuke-iterates-marks ()
  "Bulk nuke iterates the marked-set when non-empty."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws1" :priority "p1")
    (claude-repl-drawer-test--register "ws2" :priority "p2")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--ensure-marked-set)
      (puthash "ws1" t claude-repl-drawer--marked-set)
      (puthash "ws2" t claude-repl-drawer--marked-set)
      (let ((nuked nil))
        (cl-letf (((symbol-function 'claude-repl-nuke-workspace)
                   (lambda (&optional ws) (push ws nuked))))
          (claude-repl-drawer-nuke))
        (should (member "ws1" nuked))
        (should (member "ws2" nuked))))))

(ert-deftest claude-repl-drawer-test-clear-marks-empties-set ()
  "`clear-marks' empties the marked-set."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--ensure-marked-set)
      (puthash "ws" t claude-repl-drawer--marked-set)
      (claude-repl-drawer-clear-marks)
      (should (zerop (claude-repl-drawer--marked-count))))))

(ert-deftest claude-repl-drawer-test-render-marked-uses-marked-glyph ()
  "Rendered marked entry's gutter contains the marked glyph."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--ensure-marked-set)
      (puthash "ws" t claude-repl-drawer--marked-set)
      (claude-repl-drawer--render)
      (should (string-match-p (regexp-quote claude-repl-drawer-marked-glyph)
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

;;;; ---- Expand-detail ----

(ert-deftest claude-repl-drawer-test-toggle-expand-adds-and-removes ()
  "TAB toggle adds the entry to the expanded-set on first press, removes on second."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p1" :project-dir "/tmp/")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (cl-letf (((symbol-function 'claude-repl-drawer--refresh-detail-cache) #'ignore))
        (claude-repl-drawer-toggle-expand)
        (should (claude-repl-drawer--expanded-p "ws"))
        (claude-repl-drawer-toggle-expand)
        (should-not (claude-repl-drawer--expanded-p "ws"))))))

(ert-deftest claude-repl-drawer-test-render-detail-lines-shows-cached-fields ()
  "When an entry is expanded, render emits its `:detail-*' fields."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws"
                                       :priority "p1"
                                       :project-dir "/tmp/"
                                       :detail-branch "feature/x"
                                       :detail-master-ahead 7
                                       :detail-last-commit "fix: thing"
                                       :detail-last-commit-time "5 minutes ago")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--ensure-expanded-set)
      (puthash "ws" t claude-repl-drawer--expanded-set)
      (claude-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "feature/x" text))
        (should (string-match-p "ahead master:" text))
        (should (string-match-p "fix: thing" text))
        (should (string-match-p "5 minutes ago" text))))))

(ert-deftest claude-repl-drawer-test-format-duration ()
  "`--format-duration' produces short human-readable strings."
  (should (equal (claude-repl-drawer--format-duration 30)   "30s ago"))
  (should (equal (claude-repl-drawer--format-duration 600)  "10m ago"))
  (should (equal (claude-repl-drawer--format-duration 7200) "2.0h ago"))
  (should (equal (claude-repl-drawer--format-duration 172800) "2.0d ago")))

;;;; ---- Per-entry action commands ----

(ert-deftest claude-repl-drawer-test-nuke-dispatches-to-entry ()
  "`claude-repl-drawer-nuke' invokes `claude-repl-nuke-workspace' with the entry."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "target" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((arg :unset))
        (cl-letf (((symbol-function 'claude-repl-nuke-workspace)
                   (lambda (&optional ws) (setq arg ws))))
          (claude-repl-drawer-nuke))
        (should (equal arg "target"))))))

(ert-deftest claude-repl-drawer-test-kill-dispatches-to-entry ()
  "`claude-repl-drawer-kill' invokes `claude-repl-kill-workspace' with the entry."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "target" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((arg :unset))
        (cl-letf (((symbol-function 'claude-repl-kill-workspace)
                   (lambda (&optional ws) (setq arg ws))))
          (claude-repl-drawer-kill))
        (should (equal arg "target"))))))

(ert-deftest claude-repl-drawer-test-interrupt-dispatches-to-entry ()
  "`claude-repl-drawer-interrupt' invokes `claude-repl-interrupt' with the entry."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "target" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((arg :unset))
        (cl-letf (((symbol-function 'claude-repl-interrupt)
                   (lambda (&optional ws) (setq arg ws))))
          (claude-repl-drawer-interrupt))
        (should (equal arg "target"))))))

(ert-deftest claude-repl-drawer-test-send-prompt-dispatches-to-entry ()
  "`claude-repl-drawer-send-prompt' calls `claude-repl--send' with prompt and entry."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "target" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((sent-prompt :unset)
            (sent-ws     :unset))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "hello world"))
                  ((symbol-function 'claude-repl--send)
                   (lambda (prompt ws &rest _)
                     (setq sent-prompt prompt sent-ws ws))))
          (claude-repl-drawer-send-prompt))
        (should (equal sent-prompt "hello world"))
        (should (equal sent-ws "target"))))))

(ert-deftest claude-repl-drawer-test-send-prompt-empty-skips-send ()
  "Empty prompt input skips the send entirely."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "target" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((called nil))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
                  ((symbol-function 'claude-repl--send)
                   (lambda (&rest _) (setq called t))))
          (claude-repl-drawer-send-prompt))
        (should-not called)))))

(ert-deftest claude-repl-drawer-test-action-no-ws-at-point-errors ()
  "Action commands signal user-error when there is no workspace at point."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (goto-char (point-min))
      (should-error (claude-repl-drawer-nuke) :type 'user-error)
      (should-error (claude-repl-drawer-kill) :type 'user-error)
      (should-error (claude-repl-drawer-interrupt) :type 'user-error))))

(ert-deftest claude-repl-drawer-test-priority-up-from-p1-to-p05 ()
  "`priority-up' cycles p1 → p05."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((args :unset))
        (cl-letf (((symbol-function 'claude-repl-set-priority)
                   (lambda (p ws) (setq args (list p ws)))))
          (claude-repl-drawer-priority-up))
        (should (equal args '("p05" "ws")))))))

(ert-deftest claude-repl-drawer-test-priority-down-from-p1-to-p2 ()
  "`priority-down' cycles p1 → p2."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((args :unset))
        (cl-letf (((symbol-function 'claude-repl-set-priority)
                   (lambda (p ws) (setq args (list p ws)))))
          (claude-repl-drawer-priority-down))
        (should (equal args '("p2" "ws")))))))

(ert-deftest claude-repl-drawer-test-priority-down-from-p3-to-nil ()
  "`priority-down' from p3 cycles to nil (sent as empty string to set-priority)."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p3")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((args :unset))
        (cl-letf (((symbol-function 'claude-repl-set-priority)
                   (lambda (p ws) (setq args (list p ws)))))
          (claude-repl-drawer-priority-down))
        (should (equal args '("" "ws")))))))

(ert-deftest claude-repl-drawer-test-priority-up-from-nil-to-p3 ()
  "`priority-up' from nil cycles to p3 (one step toward higher priority)."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((args :unset))
        (cl-letf (((symbol-function 'claude-repl-set-priority)
                   (lambda (p ws) (setq args (list p ws)))))
          (claude-repl-drawer-priority-up))
        (should (equal args '("p3" "ws")))))))

(ert-deftest claude-repl-drawer-test-toggle-hidden-active-to-hidden ()
  "Toggling a non-hidden entry calls `claude-repl--on-close' with the entry."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws1" :priority "p1" :repl-state :active)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((arg :unset))
        (cl-letf (((symbol-function 'claude-repl--on-close)
                   (lambda (&optional ws) (setq arg ws))))
          (claude-repl-drawer-toggle-hidden))
        (should (equal arg "ws1"))))))

(ert-deftest claude-repl-drawer-test-toggle-hidden-hidden-to-active ()
  "Toggling a `:hidden' entry calls `claude-repl--unhide-workspace' with the entry."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws1" :priority "p1" :repl-state :hidden)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      ;; Hidden ws renders in HIDDEN section — find it via direct goto.
      (should (claude-repl-drawer--goto-workspace-line "ws1"))
      (let ((arg :unset))
        (cl-letf (((symbol-function 'claude-repl--unhide-workspace)
                   (lambda (ws) (setq arg ws))))
          (claude-repl-drawer-toggle-hidden))
        (should (equal arg "ws1"))))))

(ert-deftest claude-repl-drawer-test-new-child-dispatches-to-entry ()
  "`claude-repl-drawer-new-child' calls create-worktree-workspace with `head'+entry."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "parent" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((args :unset))
        (cl-letf (((symbol-function 'claude-repl-create-worktree-workspace)
                   (lambda (base &optional ws) (setq args (list base ws)))))
          (claude-repl-drawer-new-child))
        (should (equal args '(head "parent")))))))

(ert-deftest claude-repl-drawer-test-new-fork-dispatches-to-entry ()
  "`claude-repl-drawer-new-fork' calls fork-worktree-workspace with the entry."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "parent" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((arg :unset))
        (cl-letf (((symbol-function 'claude-repl-fork-worktree-workspace)
                   (lambda (&optional ws) (setq arg ws))))
          (claude-repl-drawer-new-fork))
        (should (equal arg "parent"))))))

(ert-deftest claude-repl-drawer-test-merge-into-master-switches-then-calls ()
  "`claude-repl-drawer-merge-into-master' switches to entry, invokes merge, then restores."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "target" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((current "elsewhere")
            (switch-log nil)
            (merge-called nil))
        (cl-letf (((symbol-function '+workspace-current-name)
                   (lambda () current))
                  ((symbol-function '+workspace-switch)
                   (lambda (ws &rest _) (push ws switch-log) (setq current ws)))
                  ((symbol-function 'claude-repl-workspace-merge-current-into-source)
                   (lambda () (setq merge-called t))))
          (claude-repl-drawer-merge-into-master))
        (should merge-called)
        (should (equal (nreverse switch-log) '("target" "elsewhere")))))))

;;;; ---- Persistence across workspaces ----

(ert-deftest claude-repl-drawer-test-ensure-visible-noop-when-flag-nil ()
  "`--ensure-visible-on-persp-switch' is a no-op when the global flag is nil."
  (let ((claude-repl-drawer--global-visible-p nil)
        (called nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (&rest _) (setq called t) nil)))
      (claude-repl-drawer--ensure-visible-on-persp-switch)
      (should-not called))))

(ert-deftest claude-repl-drawer-test-ensure-visible-displays-when-flag-set ()
  "`--ensure-visible-on-persp-switch' displays the drawer when flag is set and it's not visible."
  (let ((claude-repl-drawer--global-visible-p t)
        (display-called nil))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
              ((symbol-function 'display-buffer)
               (lambda (&rest _) (setq display-called t) nil))
              ((symbol-function 'claude-repl-drawer--get-or-create-buffer)
               (lambda () (get-buffer-create " *test-drawer-buf*"))))
      (claude-repl-drawer--ensure-visible-on-persp-switch)
      (should display-called))
    (when-let ((b (get-buffer " *test-drawer-buf*"))) (kill-buffer b))))

(ert-deftest claude-repl-drawer-test-hide-clears-global-flag ()
  "`claude-repl-drawer-hide' clears the global visible-flag."
  (let ((claude-repl-drawer--global-visible-p t))
    (claude-repl-drawer-hide)
    (should-not claude-repl-drawer--global-visible-p)))

;;;; ---- Section partition + tree ----

(ert-deftest claude-repl-drawer-test-workspace-section-merged-dominates ()
  "Merged-branch workspaces land in :merged regardless of hidden state."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws"
                                       :branch-merged 'merged
                                       :repl-state    :hidden)
    (should (eq (claude-repl-drawer--workspace-section "ws") :merged))))

(ert-deftest claude-repl-drawer-test-workspace-section-hidden ()
  "Non-merged hidden workspaces land in :hidden."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :repl-state :hidden)
    (should (eq (claude-repl-drawer--workspace-section "ws") :hidden))))

(ert-deftest claude-repl-drawer-test-workspace-section-default-main ()
  "Workspaces with no flags default to :main."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws")
    (should (eq (claude-repl-drawer--workspace-section "ws") :main))))

(ert-deftest claude-repl-drawer-test-effective-parent-skips-merged-ancestor ()
  "Effective parent walks past merged ancestors to the first unmerged one."
  (claude-repl-test--with-clean-state
    (puthash "gp" '(:project-dir "/gp/" :branch-merged not-merged)
             claude-repl--workspaces)
    (puthash "p"  '(:project-dir "/p/"  :source-ws-dir "/gp/"
                    :branch-merged merged)
             claude-repl--workspaces)
    (puthash "c"  '(:project-dir "/c/"  :source-ws-dir "/p/")
             claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (equal (claude-repl-drawer--effective-parent "c" '("gp" "c"))
                     "gp")))))

(ert-deftest claude-repl-drawer-test-effective-parent-nil-when-parent-missing-from-section ()
  "Effective parent returns nil when no ancestor lives in SECTION-SET."
  (claude-repl-test--with-clean-state
    (puthash "p" '(:project-dir "/p/") claude-repl--workspaces)
    (puthash "c" '(:project-dir "/c/" :source-ws-dir "/p/")
             claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (null (claude-repl-drawer--effective-parent "c" '("c")))))))

(ert-deftest claude-repl-drawer-test-effective-parent-in-merged-direct-only ()
  "MERGED-section parent is direct source-ws when also in merged-set."
  (claude-repl-test--with-clean-state
    (puthash "p" '(:project-dir "/p/") claude-repl--workspaces)
    (puthash "c" '(:project-dir "/c/" :source-ws-dir "/p/")
             claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (equal (claude-repl-drawer--effective-parent-in-merged
                      "c" '("p" "c"))
                     "p")))))

(ert-deftest claude-repl-drawer-test-build-tree-roots-and-children ()
  "`--build-tree' returns sorted forest with sorted children."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "root" :priority "p1")
    (claude-repl-drawer-test--register "kid"  :priority "p2")
    (let* ((parent-fn (lambda (w) (when (equal w "kid") "root")))
           (forest (claude-repl-drawer--build-tree '("root" "kid") parent-fn)))
      (should (equal (mapcar #'car forest) '("root")))
      (should (equal (mapcar #'car (cdr (car forest))) '("kid"))))))

(ert-deftest claude-repl-drawer-test-section-headers-include-counts ()
  "Section header labels show entry counts."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "main-a")
    (claude-repl-drawer-test--register "main-b")
    (claude-repl-drawer-test--register "hid"     :repl-state    :hidden)
    (claude-repl-drawer-test--register "merged"  :branch-merged 'merged)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "MAIN (2)"   text))
        (should (string-match-p "HIDDEN (1)" text))
        (should (string-match-p "MERGED (1)" text))))))

(ert-deftest claude-repl-drawer-test-render-three-sections ()
  "Render emits MAIN, HIDDEN, and MERGED headers in that order."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "main-ws")
    (claude-repl-drawer-test--register "hidden-ws" :repl-state :hidden)
    (claude-repl-drawer-test--register "merged-ws" :branch-merged 'merged)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "MAIN" text))
        (should (string-match-p "HIDDEN" text))
        (should (string-match-p "MERGED" text))
        ;; MAIN must precede HIDDEN, HIDDEN must precede MERGED
        (should (< (string-match "MAIN" text)
                   (string-match "HIDDEN" text)))
        (should (< (string-match "HIDDEN" text)
                   (string-match "MERGED" text)))))))

(ert-deftest claude-repl-drawer-test-render-merged-ws-only-in-merged-section ()
  "A merged workspace appears in MERGED, not MAIN."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "merged-ws" :branch-merged 'merged)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
             (merged-pos (string-match "MERGED" text))
             (ws-pos     (string-match "merged-ws" text)))
        ;; merged-ws appears AFTER the MERGED header (not in MAIN above it)
        (should (and merged-pos ws-pos (> ws-pos merged-pos)))))))

;;;; ---- Sort + partition ----

(ert-deftest claude-repl-drawer-test-sort-by-priority ()
  "Sort places p05 before p1, then alphabetical within rank."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws-a" :priority "p1")
    (claude-repl-drawer-test--register "ws-b" :priority "p05")
    (claude-repl-drawer-test--register "ws-c" :priority "p1")
    (let ((sorted (claude-repl-drawer--sort '("ws-a" "ws-b" "ws-c"))))
      (should (equal sorted '("ws-b" "ws-a" "ws-c"))))))

(ert-deftest claude-repl-drawer-test-sort-unprioritized-last ()
  "Workspaces without :priority sort after every prioritized one."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws-x")
    (claude-repl-drawer-test--register "ws-y" :priority "p3")
    (let ((sorted (claude-repl-drawer--sort '("ws-x" "ws-y"))))
      (should (equal sorted '("ws-y" "ws-x"))))))

(ert-deftest claude-repl-drawer-test-partition-splits-hidden ()
  "Partition separates :hidden workspaces from visible."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "vis" :priority "p1")
    (claude-repl-drawer-test--register "gone" :priority "p1" :repl-state :hidden)
    (let ((parts (claude-repl-drawer--partition '("vis" "gone"))))
      (should (equal (car parts) '("vis")))
      (should (equal (cdr parts) '("gone"))))))

;;;; ---- Render ----

(ert-deftest claude-repl-drawer-test-render-empty-shows-both-sections ()
  "Empty registry still renders MAIN and HIDDEN headers with placeholders."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "MAIN" text))
        (should (string-match-p "HIDDEN" text))
        (should (string-match-p (regexp-quote claude-repl-drawer-empty-section-label)
                                text))))))

(ert-deftest claude-repl-drawer-test-render-contains-name ()
  "Render includes the workspace name in its line."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "feature-x" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (should (string-match-p "feature-x"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest claude-repl-drawer-test-render-contains-summary ()
  "Render shows :last-prompt-summary on the subtitle line."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "fx"
                                       :priority "p1"
                                       :last-prompt-summary "Refactor login flow")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (should (string-match-p "Refactor login flow"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest claude-repl-drawer-test-render-pending-summary ()
  "Pending summary renders an ellipsis placeholder."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "fx"
                                       :priority "p1"
                                       :last-prompt-summary-pending t)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (should (string-match-p "…"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest claude-repl-drawer-test-render-always-shows-both-section-headers ()
  "Both MAIN and HIDDEN sections always render, regardless of contents."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "vis" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "MAIN" text))
        (should (string-match-p "HIDDEN" text))))))

(ert-deftest claude-repl-drawer-test-render-empty-hidden-shows-none-placeholder ()
  "Empty HIDDEN section renders the (none) placeholder."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "vis" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (should (string-match-p (regexp-quote claude-repl-drawer-empty-section-label)
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest claude-repl-drawer-test-render-section-headers-styled ()
  "Section headers carry the `claude-repl-drawer-section-title' face."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (when (memq 'claude-repl-drawer-section-title
                      (let ((face (get-text-property (point) 'face)))
                        (if (listp face) face (list face))))
            (setq found t))
          (forward-char 1))
        (should found)))))

(ert-deftest claude-repl-drawer-test-render-attaches-workspace-property ()
  "Each rendered workspace block carries a `claude-repl-drawer-workspace' text property."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (goto-char (point-min))
      ;; The first character of the workspace block carries the property.
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (when (equal (get-text-property (point) 'claude-repl-drawer-workspace)
                       "alpha")
            (setq found t))
          (forward-char 1))
        (should found)))))

;;;; ---- Navigation ----

(ert-deftest claude-repl-drawer-test-next-moves-to-next-workspace ()
  "`claude-repl-drawer-next' walks past the MAIN header into successive workspaces."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "first" :priority "p1")
    (claude-repl-drawer-test--register "second" :priority "p2")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (goto-char (point-min))
      (claude-repl-drawer-next)
      (should (equal (claude-repl-drawer--workspace-at-point) "first"))
      (claude-repl-drawer-next)
      (should (equal (claude-repl-drawer--workspace-at-point) "second")))))

(ert-deftest claude-repl-drawer-test-show-positions-on-first-workspace ()
  "`claude-repl-drawer--goto-first-workspace' lands on the first workspace line."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (should (equal (claude-repl-drawer--workspace-at-point) "alpha")))))

(ert-deftest claude-repl-drawer-test-goto-workspace-line-finds-target ()
  "`claude-repl-drawer--goto-workspace-line' lands on the named workspace's block."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--register "beta"  :priority "p2")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (should (claude-repl-drawer--goto-workspace-line "beta"))
      (should (equal (claude-repl-drawer--workspace-at-point) "beta")))))

(ert-deftest claude-repl-drawer-test-goto-workspace-line-returns-nil-for-unknown ()
  "`claude-repl-drawer--goto-workspace-line' returns nil when the workspace isn't rendered."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (should-not (claude-repl-drawer--goto-workspace-line "ghost")))))

(ert-deftest claude-repl-drawer-test-prev-moves-back ()
  "`claude-repl-drawer-prev' moves up to the previous workspace block."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "first" :priority "p1")
    (claude-repl-drawer-test--register "second" :priority "p2")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (goto-char (point-min))
      (claude-repl-drawer-next)   ;; first
      (claude-repl-drawer-next)   ;; second
      (claude-repl-drawer-prev)   ;; back to first
      (should (equal (claude-repl-drawer--workspace-at-point) "first")))))

(ert-deftest claude-repl-drawer-test-next-stops-at-last-workspace ()
  "`claude-repl-drawer-next' on the last workspace stays on it."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "only" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (goto-char (point-min))
      (claude-repl-drawer-next)               ;; lands on "only"
      (should (equal (claude-repl-drawer--workspace-at-point) "only"))
      (let ((before (point)))
        (claude-repl-drawer-next)             ;; nothing further to go to
        (should (equal (claude-repl-drawer--workspace-at-point) "only"))
        (should (= (point) before))))))

;;;; ---- Visit ----

(ert-deftest claude-repl-drawer-test-visit-dispatches-workspace-switch ()
  "`claude-repl-drawer-visit' calls `+workspace-switch' with the selected name."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "target" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((switched-to nil))
        (cl-letf (((symbol-function '+workspace-switch)
                   (lambda (ws &rest _) (setq switched-to ws))))
          (claude-repl-drawer-visit))
        (should (equal switched-to "target"))))))

(ert-deftest claude-repl-drawer-test-visit-no-workspace-errors ()
  "`claude-repl-drawer-visit' on a line without a workspace signals user-error."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (goto-char (point-min))
      (should-error (claude-repl-drawer-visit) :type 'user-error))))

;;;; ---- Current-entry overlay + cursor ----

(ert-deftest claude-repl-drawer-test-entry-bounds-spans-block ()
  "`--entry-bounds-at-point' covers both header and summary lines."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let* ((bounds (claude-repl-drawer--entry-bounds-at-point))
             (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (should bounds)
        (should (string-match-p "alpha" text))
        (should (string-match-p "\n" text))))))

(ert-deftest claude-repl-drawer-test-entry-bounds-nil-on-non-workspace-line ()
  "`--entry-bounds-at-point' returns nil on section headers / blanks."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (goto-char (point-min))
      (should-not (claude-repl-drawer--entry-bounds-at-point)))))

(ert-deftest claude-repl-drawer-test-update-current-entry-overlay-positions ()
  "`--update-current-entry-overlay' creates an overlay at the entry's start."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--register "beta"  :priority "p2")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (should (claude-repl-drawer--goto-workspace-line "beta"))
      (claude-repl-drawer--update-current-entry-overlay)
      (let ((ov claude-repl-drawer--current-entry-overlay))
        (should (overlayp ov))
        (should (equal (get-text-property (overlay-start ov)
                                          'claude-repl-drawer-workspace)
                       "beta"))
        (should (equal (overlay-end ov)
                       (+ (overlay-start ov)
                          (length claude-repl-drawer-gutter))))))))

(ert-deftest claude-repl-drawer-test-update-current-entry-overlay-deletes-off-entry ()
  "Overlay is removed when point lands on a non-workspace line."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (claude-repl-drawer--update-current-entry-overlay)
      (should (overlayp claude-repl-drawer--current-entry-overlay))
      (goto-char (point-min)) ;; section header
      (claude-repl-drawer--update-current-entry-overlay)
      (should-not (overlay-buffer claude-repl-drawer--current-entry-overlay)))))

(ert-deftest claude-repl-drawer-test-cursor-hidden-at-col-0 ()
  "`--update-cursor' sets cursor-type nil at column 0."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--with-buffer
      (let ((inhibit-read-only t)) (insert "hello\n"))
      (goto-char (point-min))
      (claude-repl-drawer--update-cursor)
      (should (null cursor-type)))))

(ert-deftest claude-repl-drawer-test-cursor-visible-when-not-col-0 ()
  "`--update-cursor' sets cursor-type to 'box at non-zero columns."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--with-buffer
      (let ((inhibit-read-only t)) (insert "hello\n"))
      (goto-char (point-min))
      (forward-char 2)
      (claude-repl-drawer--update-cursor)
      (should (eq cursor-type 'box)))))

;;;; ---- Refresh-if-visible ----

(ert-deftest claude-repl-drawer-test-refresh-if-visible-no-buffer-noop ()
  "`claude-repl-drawer--refresh-if-visible' is a no-op when no buffer exists."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer claude-repl-drawer-buffer-name)))
      (when buf (kill-buffer buf)))
    (should-not (claude-repl-drawer--refresh-if-visible))))

;;;; ---- Window width ----

(ert-deftest claude-repl-drawer-test-window-width-is-fraction-of-frame ()
  "`claude-repl-drawer--window-width' returns the configured fraction of frame-width."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-drawer-width-fraction 0.20))
      (cl-letf (((symbol-function 'window-frame) (lambda (_) 'fake-frame))
                ((symbol-function 'frame-width)  (lambda (_) 200)))
        (should (= (claude-repl-drawer--window-width 'fake-window) 40))))))

(ert-deftest claude-repl-drawer-test-window-width-floor-is-one ()
  "Width never drops below 1 column even on degenerate frames."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-drawer-width-fraction 0.20))
      (cl-letf (((symbol-function 'window-frame) (lambda (_) 'fake-frame))
                ((symbol-function 'frame-width)  (lambda (_) 0)))
        (should (= (claude-repl-drawer--window-width 'fake-window) 1))))))

(ert-deftest claude-repl-drawer-test-window-width-tiny-fraction ()
  "Tiny fractions like 0.01 are honored (no implicit clamping in the helper)."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-drawer-width-fraction 0.01))
      (cl-letf (((symbol-function 'window-frame) (lambda (_) 'fake-frame))
                ((symbol-function 'frame-width)  (lambda (_) 200)))
        (should (= (claude-repl-drawer--window-width 'fake-window) 2))))))

(ert-deftest claude-repl-drawer-test-window-width-adds-depth-bonus ()
  "Width = base fraction + (max-depth × indent-per-level)."
  (claude-repl-test--with-clean-state
    (puthash "gp" '(:project-dir "/gp/") claude-repl--workspaces)
    (puthash "p"  '(:project-dir "/p/"  :source-ws-dir "/gp/")
             claude-repl--workspaces)
    (puthash "c"  '(:project-dir "/c/"  :source-ws-dir "/p/")
             claude-repl--workspaces)
    (let ((claude-repl-drawer-width-fraction    0.10)
          (claude-repl-drawer-indent-per-level  2))
      (cl-letf (((symbol-function 'window-frame) (lambda (_) 'fake-frame))
                ((symbol-function 'frame-width)  (lambda (_) 200))
                ((symbol-function 'claude-repl--ws-name-for-dir)
                 (lambda (dir)
                   (cond ((equal dir "/gp/") "gp")
                         ((equal dir "/p/")  "p")
                         (t nil)))))
        ;; Base = 0.10 × 200 = 20.  Max depth = 2 (c → p → gp).
        ;; Indent-per-level = 2.  Bonus = 2 × 2 = 4.  Total = 24.
        (should (= (claude-repl-drawer--window-width 'fake-window) 24))))))

(ert-deftest claude-repl-drawer-test-max-depth-walks-chain ()
  "`--max-depth' returns the longest source-chain hop count."
  (claude-repl-test--with-clean-state
    (puthash "root" '(:project-dir "/root/") claude-repl--workspaces)
    (puthash "mid"  '(:project-dir "/mid/"  :source-ws-dir "/root/")
             claude-repl--workspaces)
    (puthash "leaf" '(:project-dir "/leaf/" :source-ws-dir "/mid/")
             claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--ws-name-for-dir)
               (lambda (dir)
                 (cond ((equal dir "/root/") "root")
                       ((equal dir "/mid/")  "mid")
                       (t nil)))))
      (should (= (claude-repl-drawer--max-depth) 2)))))

(ert-deftest claude-repl-drawer-test-max-depth-zero-when-no-chains ()
  "Max-depth returns 0 when no workspace has a recorded source."
  (claude-repl-test--with-clean-state
    (puthash "alone" '(:project-dir "/a/") claude-repl--workspaces)
    (should (= (claude-repl-drawer--max-depth) 0))))

;;;; ---- Priority display ----

(ert-deftest claude-repl-drawer-test-priority-display-falls-back-to-string ()
  "Without a loaded image, priority renders as the raw string."
  (let ((claude-repl--priority-images nil))
    (should (equal (claude-repl-drawer--priority-display "p1") "p1"))))

(ert-deftest claude-repl-drawer-test-priority-display-uses-image-when-available ()
  "When an image spec exists, the priority string carries it as `display'."
  (let* ((fake-image '(image :type png :file "/tmp/fake.png"))
         (claude-repl--priority-images `(("p1" . ,fake-image))))
    (let ((result (claude-repl-drawer--priority-display "p1")))
      (should (equal result "p1"))
      (should (equal (get-text-property 0 'display result) fake-image)))))

(ert-deftest claude-repl-drawer-test-priority-display-nil-priority ()
  "Nil priority renders as the empty string so unprioritized workspaces don't carry a phantom space."
  (should (equal (claude-repl-drawer--priority-display nil) "")))

;;;; ---- Name face (per-state coloring) ----

(ert-deftest claude-repl-drawer-test-name-face-thinking-is-red ()
  "`:thinking' state colors the name with the thinking-red foreground."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :claude-state :thinking)
    (let ((face (claude-repl-drawer--name-face "ws")))
      (should (equal (plist-get face :foreground) claude-repl--color-thinking-red))
      (should (eq (plist-get face :weight) 'bold)))))

(ert-deftest claude-repl-drawer-test-name-face-done-is-green ()
  "`:done' state colors the name with the done-green foreground."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :claude-state :done)
    (should (equal (plist-get (claude-repl-drawer--name-face "ws") :foreground)
                   claude-repl--color-done-green))))

(ert-deftest claude-repl-drawer-test-name-face-idle-is-orange ()
  "`:idle' state colors the name with the idle-orange foreground."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :claude-state :idle)
    (should (equal (plist-get (claude-repl-drawer--name-face "ws") :foreground)
                   claude-repl--color-idle-orange))))

(ert-deftest claude-repl-drawer-test-name-face-no-state-falls-back ()
  "No claude-state falls back to the plain bold workspace-name face."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws")
    (should (eq (claude-repl-drawer--name-face "ws")
                'claude-repl-drawer-workspace-name))))

(ert-deftest claude-repl-drawer-test-name-face-dead-falls-back ()
  "`:repl-state :dead' falls back to plain bold; the hidden/dim treatment muting is layered separately."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws"
                                       :claude-state :thinking
                                       :repl-state :dead)
    (should (eq (claude-repl-drawer--name-face "ws")
                'claude-repl-drawer-workspace-name))))

;;;; ---- Layout: priority/name spacing ----

(ert-deftest claude-repl-drawer-test-render-space-between-priority-and-name ()
  "When a priority is present, a space separates the badge text from the name."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "feature" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (should (string-match-p "p1 feature"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest claude-repl-drawer-test-render-glyph-name-gap ()
  "Two spaces separate the state glyph from the name when no priority is set."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "lonely")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p (concat (regexp-quote
                                         claude-repl-drawer-state-icon-default)
                                        "  lonely")
                                text))))))

(ert-deftest claude-repl-drawer-test-render-glyph-priority-gap ()
  "Two spaces separate the state glyph from the priority badge."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "feat" :priority "p1" :claude-state :idle)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p
                 (concat (regexp-quote
                          (alist-get :idle claude-repl-drawer-state-icons))
                         "  p1")
                 text))))))

(ert-deftest claude-repl-drawer-test-render-blank-line-between-workspaces ()
  "Adjacent workspace entries are separated by a blank line."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--register "beta"  :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;; A `\n\n' must appear between the two workspace blocks.  The
        ;; precise location is the boundary between alpha's summary
        ;; line and beta's header line.
        (should (string-match-p "\n\n  .* beta" text))))))

;;;; ---- State icon palette defaults ----

(ert-deftest claude-repl-drawer-test-state-icons-defaults-applied ()
  "Reload-after-defcustom-change forces the latest palette to apply.
Without the explicit force-reset, defcustom would keep prior values for
already-bound symbols and palette tweaks would require an Emacs restart."
  (should (equal (alist-get :done       claude-repl-drawer-state-icons) "✅"))
  (should (equal (alist-get :thinking   claude-repl-drawer-state-icons) "⌛"))
  (should (equal (alist-get :idle       claude-repl-drawer-state-icons) "💤"))
  (should (equal (alist-get :init       claude-repl-drawer-state-icons) "⏳"))
  (should (equal (alist-get :stop-failed claude-repl-drawer-state-icons) "❗"))
  (should (equal (alist-get :dead       claude-repl-drawer-state-icons) "❌")))

;;;; ---- State glyph ----

(ert-deftest claude-repl-drawer-test-state-glyph-dead-overrides-claude-state ()
  ":repl-state :dead takes precedence over :claude-state for the glyph."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "zombie"
                                       :claude-state :thinking
                                       :repl-state :dead)
    (should (equal (claude-repl-drawer--state-glyph "zombie")
                   (alist-get :dead claude-repl-drawer-state-icons)))))

(ert-deftest claude-repl-drawer-test-state-glyph-from-claude-state ()
  "Glyph reflects :claude-state when :repl-state is not :dead."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "busy" :claude-state :thinking)
    (should (equal (claude-repl-drawer--state-glyph "busy")
                   (alist-get :thinking claude-repl-drawer-state-icons)))))

(provide 'test-drawer)
;;; test-drawer.el ends here
