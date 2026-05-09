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

(ert-deftest claude-repl-drawer-test-render-empty-shows-placeholder ()
  "Empty workspace registry renders a placeholder line."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (should (string-match-p "no claude-repl workspaces"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

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

(ert-deftest claude-repl-drawer-test-render-hidden-section-separator ()
  "Render inserts the hidden separator only when hidden workspaces exist."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "vis" :priority "p1")
    (claude-repl-drawer-test--register "gone" :priority "p1" :repl-state :hidden)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (should (string-match-p (regexp-quote claude-repl-drawer-hidden-separator)
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest claude-repl-drawer-test-render-no-separator-when-no-hidden ()
  "When there are no hidden workspaces, the separator is omitted."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "vis" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (should-not (string-match-p (regexp-quote claude-repl-drawer-hidden-separator)
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))))))

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
  "`claude-repl-drawer-next' moves from the first workspace to the second."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "first" :priority "p1")
    (claude-repl-drawer-test--register "second" :priority "p2")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (goto-char (point-min))
      ;; point-min already lies inside the "first" workspace block.
      (should (equal (claude-repl-drawer--workspace-at-point) "first"))
      (claude-repl-drawer-next)
      (should (equal (claude-repl-drawer--workspace-at-point) "second")))))

(ert-deftest claude-repl-drawer-test-prev-moves-back ()
  "`claude-repl-drawer-prev' moves up to the previous workspace block."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "first" :priority "p1")
    (claude-repl-drawer-test--register "second" :priority "p2")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (goto-char (point-min))
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
      (claude-repl-drawer-next) ;; lands on only
      (let ((before (point)))
        (claude-repl-drawer-next)
        (should (equal (claude-repl-drawer--workspace-at-point) "only"))
        (should (= (point) before))))))

;;;; ---- Visit ----

(ert-deftest claude-repl-drawer-test-visit-dispatches-workspace-switch ()
  "`claude-repl-drawer-visit' calls `+workspace-switch' with the selected name."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "target" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (goto-char (point-min))
      (claude-repl-drawer-next)
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

;;;; ---- Refresh-if-visible ----

(ert-deftest claude-repl-drawer-test-refresh-if-visible-no-buffer-noop ()
  "`claude-repl-drawer--refresh-if-visible' is a no-op when no buffer exists."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer claude-repl-drawer-buffer-name)))
      (when buf (kill-buffer buf)))
    (should-not (claude-repl-drawer--refresh-if-visible))))

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
  "Nil priority renders as a single space placeholder."
  (should (equal (claude-repl-drawer--priority-display nil) " ")))

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
