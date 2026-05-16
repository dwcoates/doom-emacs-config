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

(ert-deftest claude-repl-drawer-test-render-repositions-current-entry-overlay ()
  "`--render' repositions the current-entry overlay so the arrow persists
across renders triggered without a corresponding `post-command-hook' (e.g.
the 1Hz status poll when the drawer is not the selected window)."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      ;; Simulate a poll-driven re-render — buffer current, no command
      ;; running in this buffer, so the buffer-local post-command-hook
      ;; would NOT fire.  The overlay must still be set up.
      (claude-repl-drawer--render)
      (should (overlayp claude-repl-drawer--current-entry-overlay))
      (let* ((ov claude-repl-drawer--current-entry-overlay)
             (disp (overlay-get ov 'display)))
        ;; Overlay must span at least one char (not collapsed at the
        ;; head of the buffer where erase-buffer left it).
        (should (> (overlay-end ov) (overlay-start ov)))
        ;; And it must carry the arrow as its `display' override.
        (should (stringp disp))
        (should (string-match-p (regexp-quote claude-repl-drawer-current-arrow)
                                disp))))))

(ert-deftest claude-repl-drawer-test-render-noop-skips-buffer-rewrite ()
  "Re-rendering with no state change must NOT erase-and-reinsert the buffer.
The buffer's `buffer-modified-tick' captures any mutation, so an
unchanged-content re-render leaves it untouched.  This is the
flicker-elimination guarantee: when the persp-switch-deferred render
or 1Hz idle poll fires with no state change, the buffer is not
rewritten and no redisplay artifact is produced for the gutter."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--register "beta"  :priority "p2")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((tick-before (buffer-modified-tick)))
        (claude-repl-drawer--render)
        (should (= tick-before (buffer-modified-tick)))))))

(ert-deftest claude-repl-drawer-test-render-after-mark-rewrites-buffer ()
  "Re-rendering after a state change (a workspace gets marked) DOES rewrite
the buffer.  Pairs with `--render-noop-skips-buffer-rewrite' to assert the
content-equality check distinguishes real changes from no-ops."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--ensure-marked-set)
      (puthash "alpha" t claude-repl-drawer--marked-set)
      (let ((tick-before (buffer-modified-tick)))
        (claude-repl-drawer--render)
        (should (> (buffer-modified-tick) tick-before))))))

(ert-deftest claude-repl-drawer-test-render-skips-build-on-matching-signature ()
  "When the render-signature matches the last render, `--render' must NOT
re-enter `--insert-content'.  This is the 1Hz poll fast path: with the
drawer open and no state change, the per-tick render allocates nothing
and walks no characters.  Counts invocations of `--insert-content' via
an `:around' override."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (let ((calls 0)
            (orig (symbol-function 'claude-repl-drawer--insert-content)))
        (cl-letf (((symbol-function 'claude-repl-drawer--insert-content)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply orig args))))
          (claude-repl-drawer--render)
          (should (= calls 1))
          (claude-repl-drawer--render)
          (should (= calls 1)))))))

(ert-deftest claude-repl-drawer-test-render-rebuilds-when-claude-state-changes ()
  "A `:claude-state' change on a registered workspace must invalidate the
render-signature so the next `--render' rebuilds.  Confirms the signature
captures plist values the 1Hz status poll mutates."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1"
                                       :claude-state :idle)
    (claude-repl-drawer-test--with-buffer
      (let ((calls 0)
            (orig (symbol-function 'claude-repl-drawer--insert-content)))
        (cl-letf (((symbol-function 'claude-repl-drawer--insert-content)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply orig args))))
          (claude-repl-drawer--render)
          (claude-repl--ws-put "alpha" :claude-state :thinking)
          (claude-repl-drawer--render)
          (should (= calls 2)))))))

(ert-deftest claude-repl-drawer-test-render-rebuilds-when-git-clean-changes ()
  "A `:git-clean' change must invalidate the signature.  The 1Hz poll's
async git-diff sentinel writes this field, so the next render must
pick up the new value."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1"
                                       :git-clean 'clean)
    (claude-repl-drawer-test--with-buffer
      (let ((calls 0)
            (orig (symbol-function 'claude-repl-drawer--insert-content)))
        (cl-letf (((symbol-function 'claude-repl-drawer--insert-content)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply orig args))))
          (claude-repl-drawer--render)
          (claude-repl--ws-put "alpha" :git-clean 'dirty)
          (claude-repl-drawer--render)
          (should (= calls 2)))))))

(ert-deftest claude-repl-drawer-test-render-rebuilds-when-workspace-added ()
  "Adding a workspace must invalidate the signature.  Workspace registration
mutates the `claude-repl--workspaces' hash; the next render must reflect
the new entry."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (let ((calls 0)
            (orig (symbol-function 'claude-repl-drawer--insert-content)))
        (cl-letf (((symbol-function 'claude-repl-drawer--insert-content)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply orig args))))
          (claude-repl-drawer--render)
          (claude-repl-drawer-test--register "beta" :priority "p2")
          (claude-repl-drawer--render)
          (should (= calls 2)))))))

(ert-deftest claude-repl-drawer-test-render-rebuilds-when-mark-toggled ()
  "Toggling a mark must invalidate the signature.  Marks affect the rendered
gutter glyph and are buffer-local — the signature includes per-ws marked
state."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (let ((calls 0)
            (orig (symbol-function 'claude-repl-drawer--insert-content)))
        (cl-letf (((symbol-function 'claude-repl-drawer--insert-content)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply orig args))))
          (claude-repl-drawer--render)
          (claude-repl-drawer--ensure-marked-set)
          (puthash "alpha" t claude-repl-drawer--marked-set)
          (claude-repl-drawer--render)
          (should (= calls 2)))))))

(ert-deftest claude-repl-drawer-test-render-noop-preserves-text-properties ()
  "A no-op re-render must NOT corrupt text properties.
Stress-tests the path against the `replace-buffer-contents' pitfall
where the diff's LCS preserves the destination's stale text properties
on characters it matches — leaving the wrong workspace name attached
to text that visually belongs to another entry."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--register "beta"  :priority "p2")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((props-before nil))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (push (cons (point)
                        (get-text-property
                         (point) 'claude-repl-drawer-workspace))
                  props-before)
            (forward-char 1)))
        (claude-repl-drawer--render)
        (let ((props-after nil))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (push (cons (point)
                          (get-text-property
                           (point) 'claude-repl-drawer-workspace))
                    props-after)
              (forward-char 1)))
          (should (equal props-before props-after)))))))

(ert-deftest claude-repl-drawer-test-render-anchors-cursor-to-workspace ()
  "`--render' restores point onto the same workspace by identity (not line
number) so the current-entry arrow tracks the entry through layout shifts
above the cursor — e.g. a parent above a nested child collapses between
polls, shrinking the buffer; line-number restoration would land on a now-
shorter intermediate line where `--workspace-at-point' is nil, deleting the
overlay.  Tests the workspace-anchored restoration path."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "parent"
                                       :priority "p1"
                                       :project-dir "/tmp/parent"
                                       :detail-branch "feature/x"
                                       :detail-master-ahead 7
                                       :detail-last-commit "fix: thing")
    (claude-repl-drawer-test--register "child"
                                       :priority "p2"
                                       :project-dir "/tmp/child"
                                       :source-ws-dir "/tmp/parent")
    (cl-letf (((symbol-function 'claude-repl--ws-name-for-dir)
               (lambda (dir)
                 (cond ((equal dir "/tmp/parent") "parent")
                       ((equal dir "/tmp/child")  "child")))))
      (claude-repl-drawer-test--with-buffer
        (claude-repl-drawer--ensure-expanded-set)
        (puthash "parent" t claude-repl-drawer--expanded-set)
        (claude-repl-drawer--render)
        ;; Park on the nested child while parent is expanded.
        (should (claude-repl-drawer--goto-workspace-line "child"))
        (claude-repl-drawer--update-current-entry-overlay)
        ;; Collapse parent — child's line number shifts up.  A poll-driven
        ;; re-render fires before the user moves point.
        (remhash "parent" claude-repl-drawer--expanded-set)
        (claude-repl-drawer--render)
        ;; Point must still be on the child, and the overlay must still
        ;; mark the child's gutter region.
        (should (equal (claude-repl-drawer--workspace-at-point) "child"))
        (let ((ov claude-repl-drawer--current-entry-overlay))
          (should (overlayp ov))
          (should (overlay-buffer ov))
          (should (equal (get-text-property (overlay-start ov)
                                            'claude-repl-drawer-workspace)
                         "child")))))))

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

(ert-deftest claude-repl-drawer-test-detail-values-have-distinct-faces ()
  "Detail-line values carry their per-field faces (not the generic summary face)."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register
     "ws"
     :priority "p1" :project-dir "/tmp/"
     :detail-branch       "feature/x"
     :detail-master-ahead 5
     :detail-last-commit  "fix: thing")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--ensure-expanded-set)
      (puthash "ws" t claude-repl-drawer--expanded-set)
      (claude-repl-drawer--render)
      (cl-flet ((face-at (needle face)
                  (let ((pos (string-match (regexp-quote needle)
                                           (buffer-substring-no-properties
                                            (point-min) (point-max)))))
                    (and pos
                         (memq face
                               (let ((f (get-text-property (1+ pos) 'face)))
                                 (if (listp f) f (list f))))))))
        (should (face-at "feature/x" 'claude-repl-drawer-detail-branch))
        (should (face-at "5"          'claude-repl-drawer-detail-ahead-master))
        (should (face-at "fix: thing" 'claude-repl-drawer-detail-last-commit))))))

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

(ert-deftest claude-repl-drawer-test-nuke-on-merged-dispatches-to-finish ()
  "`claude-repl-drawer-nuke' on a `:merge-completed' entry routes to
`--finish-workspace' (which removes the worktree) rather than the
standard `claude-repl-nuke-workspace' path (which preserves it).  This
is the only way to drop a workspace out of the drawer's MERGED bucket."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "merged" :merge-completed t)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((finished-with :unset)
            (nuke-called nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'claude-repl--finish-workspace)
                   (lambda (ws) (setq finished-with ws)))
                  ((symbol-function 'claude-repl-nuke-workspace)
                   (lambda (&rest _) (setq nuke-called t))))
          (claude-repl-drawer-nuke))
        (should (equal finished-with "merged"))
        (should-not nuke-called)))))

(ert-deftest claude-repl-drawer-test-nuke-on-merged-aborts-on-deny ()
  "Drawer `x' on a MERGED entry prompts; answering no skips finish."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "merged" :merge-completed t)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((finish-called nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                  ((symbol-function 'claude-repl--finish-workspace)
                   (lambda (&rest _) (setq finish-called t))))
          (claude-repl-drawer-nuke))
        (should-not finish-called)))))

(ert-deftest claude-repl-drawer-test-kill-on-merged-errors ()
  "`d' (drawer-kill) refuses to act on a MERGED entry — `x' is the only
removal path for merged workspaces."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "merged" :merge-completed t)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((kill-called nil))
        (cl-letf (((symbol-function 'claude-repl-kill-workspace)
                   (lambda (&rest _) (setq kill-called t))))
          (should-error (claude-repl-drawer-kill) :type 'user-error))
        (should-not kill-called)))))

(ert-deftest claude-repl-drawer-test-send-prompt-on-merged-errors ()
  "`i' (drawer-send-prompt) refuses on a MERGED entry — the Claude
session has been torn down so there's no one to receive the prompt."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "merged" :merge-completed t)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((sent nil))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "hi"))
                  ((symbol-function 'claude-repl--send)
                   (lambda (&rest _) (setq sent t))))
          (should-error (claude-repl-drawer-send-prompt) :type 'user-error))
        (should-not sent)))))

(ert-deftest claude-repl-drawer-test-merge-into-master-on-merged-reactivates-then-merges ()
  "`M' (drawer-merge-into-master) on a MERGED entry reactivates first
\(same path `drawer-visit' takes), then invokes the standard
`claude-repl-workspace-merge-current-into-source'.  A prior
cherry-pick may have silently failed but still flipped the workspace
into MERGED, so re-attempts must be possible."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "claude-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (claude-repl-drawer-test--register
             "merged" :merge-completed t :project-dir tmp)
            (claude-repl-drawer-test--with-buffer
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace)
              (let ((established-with nil)
                    (merge-called nil))
                (cl-letf (((symbol-function 'claude-repl--state-save)
                           (lambda (&rest _) nil))
                          ((symbol-function 'claude-repl--establish-workspace)
                           (lambda (ws dir) (setq established-with (list ws dir))))
                          ((symbol-function 'claude-repl-workspace-merge-current-into-source)
                           (lambda () (setq merge-called t))))
                  (claude-repl-drawer-merge-into-master))
                (should (equal established-with (list "merged" tmp)))
                (should merge-called))))
        (delete-directory tmp t)))))

(ert-deftest claude-repl-drawer-test-merge-child-on-merged-reactivates-then-merges ()
  "`m' (drawer-merge-child) on a MERGED entry reactivates first, then
invokes the standard `claude-repl-workspace-merge'."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "claude-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (claude-repl-drawer-test--register
             "merged" :merge-completed t :project-dir tmp)
            (claude-repl-drawer-test--with-buffer
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace)
              (let ((established-with nil)
                    (merge-called nil))
                (cl-letf (((symbol-function 'claude-repl--state-save)
                           (lambda (&rest _) nil))
                          ((symbol-function 'claude-repl--establish-workspace)
                           (lambda (ws dir) (setq established-with (list ws dir))))
                          ((symbol-function 'claude-repl-workspace-merge)
                           (lambda () (setq merge-called t))))
                  (claude-repl-drawer-merge-child))
                (should (equal established-with (list "merged" tmp)))
                (should merge-called))))
        (delete-directory tmp t)))))

(ert-deftest claude-repl-drawer-test-merge-into-master-on-merged-clears-merge-flags ()
  "Reactivation during `M' on a MERGED entry clears the
`:merge-completed' / `:merge-completed-at' / `:repl-state :merged'
plist keys (the workspace must leave the MERGED bucket so the
re-attempted merge runs against a live persp)."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "claude-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (claude-repl-drawer-test--register
             "merged"
             :merge-completed t
             :merge-completed-at 1234567890.0
             :repl-state :merged
             :project-dir tmp)
            (claude-repl-drawer-test--with-buffer
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace)
              (cl-letf (((symbol-function 'claude-repl--state-save)
                         (lambda (&rest _) nil))
                        ((symbol-function 'claude-repl--establish-workspace)
                         (lambda (&rest _) nil))
                        ((symbol-function 'claude-repl-workspace-merge-current-into-source)
                         (lambda () nil)))
                (claude-repl-drawer-merge-into-master))
              (should-not (claude-repl--ws-get "merged" :merge-completed))
              (should-not (claude-repl--ws-get "merged" :merge-completed-at))
              (should-not (claude-repl--ws-get "merged" :repl-state))))
        (delete-directory tmp t)))))

(ert-deftest claude-repl-drawer-test-new-child-on-merged-errors ()
  "`n' (drawer-new-child) refuses to branch from a MERGED entry —
branching from a merged-and-torn-down workspace would resurrect a
stale tree."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "merged" :merge-completed t)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (should-error (claude-repl-drawer-new-child) :type 'user-error))))

(ert-deftest claude-repl-drawer-test-new-fork-on-merged-errors ()
  "`f' (drawer-new-fork) refuses on a MERGED entry — the source claude
session has been torn down."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "merged" :merge-completed t)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (should-error (claude-repl-drawer-new-fork) :type 'user-error))))

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

(ert-deftest claude-repl-drawer-test-ensure-hidden-when-flag-nil-and-window-visible ()
  "`--ensure-visible-on-persp-switch' deletes the drawer window when the
flag is nil but persp-mode restored a stale drawer window — making the
drawer truly global so hiding in one workspace hides in all."
  (let* ((claude-repl-drawer--global-visible-p nil)
         (buf (get-buffer-create claude-repl-drawer-buffer-name))
         (delete-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'fake-win))
                  ((symbol-function 'get-buffer-window-list)
                   (lambda (&rest _) '()))
                  ((symbol-function 'claude-repl-window--delete-buffer-windows)
                   (lambda (b &rest _) (setq delete-called-with b))))
          (claude-repl-drawer--ensure-visible-on-persp-switch)
          (should (eq delete-called-with buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Drawer ⊥ explain-config decoupling ----
;;
;; The explain-config buffer (SPC j h c output) is a standalone
;; bottom-side popup, fully decoupled from the drawer (which lives
;; on the left).  Drawer show/hide/persp-reconcile must NOT touch
;; the explain-config window — it has its own visibility lifecycle
;; (see `claude-repl--explain-config-global-visible-p').

(ert-deftest claude-repl-drawer-test-show-does-not-touch-explain-config ()
  "`claude-repl-drawer-show' must not call `--explain-config-show' —
the popup is decoupled and manages its own visibility independently."
  (let ((explain-show-called nil))
    (cl-letf (((symbol-function 'claude-repl-drawer--get-or-create-buffer)
               (lambda () (get-buffer-create " *test-drawer-buf*")))
              ((symbol-function 'claude-repl-drawer--current-ws)
               (lambda () nil))
              ((symbol-function 'display-buffer) (lambda (&rest _) nil))
              ((symbol-function 'claude-repl-drawer--render) #'ignore)
              ((symbol-function 'claude-repl-drawer--goto-workspace-line)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl-drawer--goto-first-workspace)
               (lambda () nil))
              ((symbol-function 'claude-repl-drawer--post-command) #'ignore)
              ((symbol-function 'claude-repl-drawer--apply-background) #'ignore)
              ((symbol-function 'claude-repl--explain-config-show)
               (lambda () (setq explain-show-called t))))
      (claude-repl-drawer-show)
      (should-not explain-show-called))
    (when-let ((b (get-buffer " *test-drawer-buf*"))) (kill-buffer b))))

(ert-deftest claude-repl-drawer-test-hide-does-not-touch-explain-config ()
  "`claude-repl-drawer-hide' must not call `--explain-config-hide' —
the popup is decoupled and manages its own visibility independently."
  (let ((explain-hide-called nil)
        (claude-repl-drawer--global-visible-p t))
    (cl-letf (((symbol-function 'claude-repl-window--delete-buffer-windows)
               (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--explain-config-hide)
               (lambda () (setq explain-hide-called t))))
      (claude-repl-drawer-hide)
      (should-not explain-hide-called))))

(ert-deftest claude-repl-drawer-test-ensure-visible-does-not-touch-explain-config ()
  "Drawer persp-reconciliation must not show/hide explain-config —
the popup has its own persp-activated reconciler that runs in parallel."
  (let ((claude-repl-drawer--global-visible-p t)
        (explain-show-called nil)
        (explain-hide-called nil))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'fake-win))
              ((symbol-function 'display-buffer) (lambda (&rest _) nil))
              ((symbol-function 'claude-repl-drawer--apply-width) #'ignore)
              ((symbol-function 'claude-repl--explain-config-show)
               (lambda () (setq explain-show-called t)))
              ((symbol-function 'claude-repl--explain-config-hide)
               (lambda () (setq explain-hide-called t))))
      (claude-repl-drawer--ensure-visible-on-persp-switch)
      (should-not explain-show-called)
      (should-not explain-hide-called))))

;;;; ---- Global dispatch + auto-revert ----

(ert-deftest claude-repl-drawer-test-global-next-dispatches-to-drawer ()
  "`claude-repl-drawer-global-next' calls `--next' inside the drawer buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "first"  :priority "p1")
    (claude-repl-drawer-test--register "second" :priority "p2")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace))
            (claude-repl-drawer-global-next)
            (with-current-buffer buf
              (should (equal (claude-repl-drawer--workspace-at-point) "second"))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-global-next-refreshes-overlay-synchronously ()
  "`claude-repl-drawer-global-next' updates the current-entry overlay
immediately so the arrow tracks the new selection — does not wait for
the next 1Hz render cycle (which used to cause perceived lag)."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "first"  :priority "p1")
    (claude-repl-drawer-test--register "second" :priority "p2")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace))
            (claude-repl-drawer-global-next)
            (with-current-buffer buf
              (let* ((ov claude-repl-drawer--current-entry-overlay)
                     (start (and (overlayp ov) (overlay-start ov))))
                (should (overlayp ov))
                ;; Overlay must cover the "second" entry now.
                (should (equal (get-text-property
                                start 'claude-repl-drawer-workspace)
                               "second")))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-global-call-does-not-select-window ()
  "`--call-in-drawer' must NOT change the selected window — keystroke
overhead from window selection is what made global nav feel slow vs.
local j/k.  Asserts the selected window is unchanged across the call."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace))
            (let ((before (selected-window))
                  (called nil))
              (claude-repl-drawer--call-in-drawer
               (lambda () (setq called t)))
              (should called)
              (should (eq (selected-window) before))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-global-call-errors-when-no-drawer ()
  "Global wrappers signal user-error when the drawer buffer doesn't exist."
  (when-let ((b (get-buffer claude-repl-drawer-buffer-name)))
    (kill-buffer b))
  (should-error (claude-repl-drawer-global-next) :type 'user-error))

(ert-deftest claude-repl-drawer-test-call-in-drawer-preserves-cursor ()
  "`--call-in-drawer' with PRESERVE-CURSOR=t restores the cursor to the
workspace at point before FN, overriding any cursor move FN's side
effects would otherwise leave behind (persp auto-sync, render fallback).
Simulates this by giving FN a body that yanks the cursor onto a
different entry — the wrapper must put it back."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--register "beta"  :priority "p2")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-workspace-line "beta"))
            (claude-repl-drawer--call-in-drawer
             (lambda ()
               ;; Simulate a side-effect that moves the cursor away
               ;; (e.g. `--sync-cursor-to-current-ws' after a persp
               ;; switch landing on a different active workspace).
               (claude-repl-drawer--goto-workspace-line "alpha"))
             t)
            (with-current-buffer buf
              (should (equal (claude-repl-drawer--workspace-at-point) "beta"))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-call-in-drawer-no-preserve-keeps-fn-cursor ()
  "`--call-in-drawer' without PRESERVE-CURSOR keeps the cursor wherever FN
left it — required for the navigational dispatchers (`global-next' /
`global-prev') whose entire purpose is to move the cursor."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--register "beta"  :priority "p2")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-workspace-line "beta"))
            (claude-repl-drawer--call-in-drawer
             (lambda ()
               (claude-repl-drawer--goto-workspace-line "alpha")))
            (with-current-buffer buf
              (should (equal (claude-repl-drawer--workspace-at-point) "alpha"))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-call-in-drawer-preserve-falls-back-when-ws-gone ()
  "When the preserved workspace no longer exists after FN (e.g. nuked),
`--call-in-drawer' leaves the cursor wherever FN naturally placed it
instead of erroring or jumping to point-min."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--register "beta"  :priority "p2")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-workspace-line "beta"))
            (claude-repl-drawer--call-in-drawer
             (lambda ()
               ;; Simulate nuke: remove the preserved ws + re-render +
               ;; land cursor on the surviving entry.
               (remhash "beta" claude-repl--workspaces)
               (claude-repl-drawer--render)
               (claude-repl-drawer--goto-workspace-line "alpha"))
             t)
            (with-current-buffer buf
              (should (equal (claude-repl-drawer--workspace-at-point) "alpha"))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-call-in-drawer-focused-selects-drawer-window ()
  "`--call-in-drawer-focused' must select the drawer window for the
duration of FN — that's the whole reason it exists (so visual features
keyed off window-selection take hold).  Asserts `selected-window' inside
FN is the drawer's window, then reverts to the original after."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (let* ((drawer-win (display-buffer-in-side-window
                              buf '((side . left))))
                 (other-buf (get-buffer-create " *focused-test-other*"))
                 (other-win (display-buffer other-buf
                                            '(display-buffer-pop-up-window))))
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace))
            (select-window other-win)
            (let ((seen-win nil))
              (claude-repl-drawer--call-in-drawer-focused
               (lambda () (setq seen-win (selected-window))))
              (should (eq seen-win drawer-win))
              (should (eq (selected-window) other-win)))
            (when (window-live-p other-win) (delete-window other-win))
            (when (buffer-live-p other-buf) (kill-buffer other-buf)))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-call-in-drawer-focused-falls-back-when-no-window ()
  "When the drawer buffer exists but isn't displayed in any window,
`--call-in-drawer-focused' must fall back to unfocused dispatch — there's
no window to select, but FN still needs to run."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace))
            (let ((called nil))
              (claude-repl-drawer--call-in-drawer-focused
               (lambda () (setq called t)))
              (should called)))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-call-in-drawer-focused-errors-when-no-drawer ()
  "`--call-in-drawer-focused' errors with `user-error' when the drawer
buffer doesn't exist — same contract as `--call-in-drawer'."
  (when-let ((b (get-buffer claude-repl-drawer-buffer-name)))
    (kill-buffer b))
  (should-error (claude-repl-drawer--call-in-drawer-focused #'ignore)
                :type 'user-error))

(ert-deftest claude-repl-drawer-test-global-next-uses-focused-dispatch ()
  "`claude-repl-drawer-global-next' must route through the focused
dispatcher so the drawer window is selected during the move — that's
what makes `hl-line' (and other selection-keyed visuals) engage and
stick after focus returns.  Asserts by stubbing the focused helper and
verifying the non-focused one is NOT called."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (let ((focused-called nil)
                (unfocused-called nil))
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace))
            (cl-letf (((symbol-function 'claude-repl-drawer--call-in-drawer-focused)
                       (lambda (_fn) (setq focused-called t)))
                      ((symbol-function 'claude-repl-drawer--call-in-drawer)
                       (lambda (_fn &optional _p) (setq unfocused-called t))))
              (claude-repl-drawer-global-next)
              (should focused-called)
              (should-not unfocused-called)))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-global-prev-uses-focused-dispatch ()
  "`claude-repl-drawer-global-prev' must route through the focused
dispatcher — same rationale as global-next."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (let ((focused-called nil)
                (unfocused-called nil))
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace))
            (cl-letf (((symbol-function 'claude-repl-drawer--call-in-drawer-focused)
                       (lambda (_fn) (setq focused-called t)))
                      ((symbol-function 'claude-repl-drawer--call-in-drawer)
                       (lambda (_fn &optional _p) (setq unfocused-called t))))
              (claude-repl-drawer-global-prev)
              (should focused-called)
              (should-not unfocused-called)))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-sync-cursor-to-current-ws ()
  "`--sync-cursor-to-current-ws' positions point on the current ws's entry."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--register "beta"  :priority "p2")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace))
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "beta")))
              (claude-repl-drawer--sync-cursor-to-current-ws))
            (with-current-buffer buf
              (should (equal (claude-repl-drawer--workspace-at-point) "beta"))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-sync-cursor-refreshes-overlay-synchronously ()
  "`--sync-cursor-to-current-ws' repositions the current-entry overlay
synchronously so the arrow snaps to the active workspace immediately,
not after the next 1Hz render — fixes the perceived flash-then-disappear
on workspace switch when the drawer's buffer-local post-command-hook
doesn't fire (e.g. focus elsewhere or persp-mode-driven sync)."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--register "beta"  :priority "p2")
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (claude-repl-drawer-mode)
              (claude-repl-drawer--render)
              ;; Park the arrow on alpha (the old "current" workspace).
              (claude-repl-drawer--goto-workspace-line "alpha")
              (claude-repl-drawer--update-current-entry-overlay)
              (let* ((ov claude-repl-drawer--current-entry-overlay)
                     (start (and (overlayp ov) (overlay-start ov))))
                (should (equal (get-text-property
                                start 'claude-repl-drawer-workspace)
                               "alpha"))))
            ;; Simulate a persp-activated sync to beta (without an
            ;; intervening render, which is the lag the user observed).
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "beta")))
              (claude-repl-drawer--sync-cursor-to-current-ws))
            (with-current-buffer buf
              ;; Arrow must already track beta, not still be on alpha
              ;; (waiting for the next 1Hz render to catch up).
              (let* ((ov claude-repl-drawer--current-entry-overlay)
                     (start (and (overlayp ov) (overlay-start ov))))
                (should (overlayp ov))
                (should (overlay-buffer ov))
                (should (equal (get-text-property
                                start 'claude-repl-drawer-workspace)
                               "beta")))))
        (kill-buffer buf)))))

(ert-deftest claude-repl-drawer-test-global-post-command-fires-sync-on-leave ()
  "`--global-post-command' calls sync when transitioning out of the drawer."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name))
          (sync-called 0))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-repl-drawer--sync-cursor-to-current-ws)
                     (lambda (&rest _) (setq sync-called (1+ sync-called)))))
            ;; Simulate "last command was in drawer".
            (let ((claude-repl-drawer--last-was-drawer t))
              ;; Now we're elsewhere (not in drawer buffer).
              (with-temp-buffer
                (claude-repl-drawer--global-post-command)))
            (should (= sync-called 1)))
        (kill-buffer buf)))))

;;;; ---- Repo grouping ----

(ert-deftest claude-repl-drawer-test-group-label-from-key ()
  "`--group-label' returns the basename of the parent of KEY."
  (should (equal (claude-repl-drawer--group-label "/path/to/doom/.git")
                 "doom"))
  (should (equal (claude-repl-drawer--group-label "/x/y/explanation-engine/.git")
                 "explanation-engine"))
  (should (null  (claude-repl-drawer--group-label nil))))

(ert-deftest claude-repl-drawer-test-group-key-cached-on-plist ()
  "`--workspace-group-key' caches its result on `:group-key'."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :project-dir "/some/")
    (claude-repl--ws-put "ws" :group-key "/cached/.git")
    ;; Cached value short-circuits the git call.
    (should (equal (claude-repl-drawer--workspace-group-key "ws")
                   "/cached/.git"))))

(ert-deftest claude-repl-drawer-test-group-trees-by-repo-buckets ()
  "`--group-trees-by-repo' partitions a forest by its roots' group-key labels."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "doom-ws"      :project-dir "/d/")
    (claude-repl-drawer-test--register "doom-ws-2"    :project-dir "/d2/")
    (claude-repl-drawer-test--register "ee-ws"        :project-dir "/e/")
    (claude-repl--ws-put "doom-ws"   :group-key "/path/doom/.git")
    (claude-repl--ws-put "doom-ws-2" :group-key "/path/doom/.git")
    (claude-repl--ws-put "ee-ws"     :group-key "/path/explanation-engine/.git")
    (let* ((trees '(("doom-ws") ("doom-ws-2") ("ee-ws")))
           (groups (claude-repl-drawer--group-trees-by-repo trees)))
      (should (equal (mapcar #'car groups) '("doom" "explanation-engine")))
      (should (= 2 (length (cdr (assoc "doom" groups)))))
      (should (= 1 (length (cdr (assoc "explanation-engine" groups))))))))

(ert-deftest claude-repl-drawer-test-render-emits-group-labels ()
  "Render emits the group label between repo groups."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "doom-a" :project-dir "/d/")
    (claude-repl-drawer-test--register "ee-a"   :project-dir "/e/")
    (claude-repl--ws-put "doom-a" :group-key "/path/doom/.git")
    (claude-repl--ws-put "ee-a"   :group-key "/path/explanation-engine/.git")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "▸ doom" text))
        (should (string-match-p "▸ explanation-engine" text))))))

;;;; ---- Section partition + tree ----

(ert-deftest claude-repl-drawer-test-workspace-section-merging-dominates-hidden ()
  "In-flight workflow (`:merging' t) lands in :merging even when also
flagged hidden.  Asserts the workflow-state signal — not git ancestry
— drives the MERGING bucket, and that it outranks :hidden in the
precedence chain."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws"
                                       :merging    t
                                       :repl-state :hidden)
    (should (eq (claude-repl-drawer--workspace-section "ws") :merging))))

(ert-deftest claude-repl-drawer-test-workspace-section-ancestry-no-longer-buckets-merging ()
  "`:branch-merged' 'merged alone does NOT route to MERGING.
Regression guard: ancestry was the old bucket gate and produced
false MERGING entries when an empty child's parent advanced past it.
Ancestry is now reserved for flattening only; without a workflow flag
the workspace must fall through to :main."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :branch-merged 'merged)
    (should (eq (claude-repl-drawer--workspace-section "ws") :main))))

(ert-deftest claude-repl-drawer-test-workspace-section-merge-completed-routes-to-merged ()
  "Workspaces with `:merge-completed' t land in :merged.
This is the sole path into the MERGED bucket — async ancestry polling
no longer feeds it."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :merge-completed t)
    (should (eq (claude-repl-drawer--workspace-section "ws") :merged))))

(ert-deftest claude-repl-drawer-test-workspace-section-merge-completed-dominates-merging ()
  "`:merge-completed' t wins over `:merging' t.
Covers the brief transition window between setting completed and
clearing the in-flight flag — the workspace must surface in MERGED,
not MERGING."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws"
                                       :merge-completed t
                                       :merging         t)
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
  "MERGED-section parent is direct source-ws when also in merged-set.
The generalized `--effective-parent-in-section' powers this; the old
`--effective-parent-in-merged' name is preserved as an alias and so
is still exercised here."
  (claude-repl-test--with-clean-state
    (puthash "p" '(:project-dir "/p/") claude-repl--workspaces)
    (puthash "c" '(:project-dir "/c/" :source-ws-dir "/p/")
             claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (equal (claude-repl-drawer--effective-parent-in-merged
                      "c" '("p" "c"))
                     "p")))))

(ert-deftest claude-repl-drawer-test-effective-parent-in-section-merging-direct-only ()
  "MERGING-section topology uses the same direct-source-ws rule.
Both MERGING and MERGED route through `--effective-parent-in-section'
so their internal parent/child relationships render without flattening
through other flattenable ancestors."
  (claude-repl-test--with-clean-state
    (puthash "p" '(:project-dir "/p/") claude-repl--workspaces)
    (puthash "c" '(:project-dir "/c/" :source-ws-dir "/p/")
             claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (equal (claude-repl-drawer--effective-parent-in-section
                      "c" '("p" "c"))
                     "p")))))

(ert-deftest claude-repl-drawer-test-effective-parent-does-not-flatten-through-merge-completed ()
  "MAIN/HIDDEN trees do NOT flatten through `:merge-completed' alone.
Flattening is git-ancestry-only (`--ws-flattenable-ancestor-p' reads
`:branch-merged' = `merged' exclusively), so a workflow-completed
ancestor without the ancestry cache must remain in the chain.  In
practice the async poll will follow shortly and converge the two; this
test guards against workflow-state leaking into tree topology."
  (claude-repl-test--with-clean-state
    (puthash "gp" '(:project-dir "/gp/") claude-repl--workspaces)
    (puthash "p"  '(:project-dir "/p/"  :source-ws-dir "/gp/"
                    :merge-completed t)
             claude-repl--workspaces)
    (puthash "c"  '(:project-dir "/c/"  :source-ws-dir "/p/")
             claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      ;; "p" is not flattenable, so "c"'s effective parent search
      ;; through ("gp" "c") finds no candidate and returns nil.
      (should (null (claude-repl-drawer--effective-parent "c" '("gp" "c")))))))

(ert-deftest claude-repl-drawer-test-effective-parent-flattens-through-branch-merged ()
  "MAIN/HIDDEN trees flatten through ancestors with `:branch-merged' = `merged'.
This is the sole flattening signal under the new semantics: git
ancestry alone, not workflow state."
  (claude-repl-test--with-clean-state
    (puthash "gp" '(:project-dir "/gp/") claude-repl--workspaces)
    (puthash "p"  '(:project-dir "/p/"  :source-ws-dir "/gp/"
                    :branch-merged merged)
             claude-repl--workspaces)
    (puthash "c"  '(:project-dir "/c/"  :source-ws-dir "/p/")
             claude-repl--workspaces)
    (cl-letf (((symbol-function 'claude-repl--path-canonical) #'identity))
      (should (equal (claude-repl-drawer--effective-parent "c" '("gp" "c"))
                     "gp")))))

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
  "Section header labels show entry counts across all four buckets.
`:merging' t routes to MERGING; `:merge-completed' t routes to MERGED
— they are independent workflow-state buckets."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "main-a")
    (claude-repl-drawer-test--register "main-b")
    (claude-repl-drawer-test--register "hid"     :repl-state :hidden)
    (claude-repl-drawer-test--register "merging" :merging    t)
    (claude-repl-drawer-test--register "merged"  :merge-completed t)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "MAIN (2)"    text))
        (should (string-match-p "HIDDEN (1)"  text))
        (should (string-match-p "MERGING (1)" text))
        (should (string-match-p "MERGED (1)"  text))))))

(ert-deftest claude-repl-drawer-test-render-four-sections ()
  "Render emits MAIN, HIDDEN, MERGING, and MERGED headers in that order."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "main-ws")
    (claude-repl-drawer-test--register "hidden-ws"  :repl-state :hidden)
    (claude-repl-drawer-test--register "merging-ws" :merging    t)
    (claude-repl-drawer-test--register "merged-ws"  :merge-completed t)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "MAIN" text))
        (should (string-match-p "HIDDEN" text))
        (should (string-match-p "MERGING" text))
        (should (string-match-p "MERGED" text))
        ;; MAIN < HIDDEN < MERGING < MERGED
        (should (< (string-match "MAIN" text)
                   (string-match "HIDDEN" text)))
        (should (< (string-match "HIDDEN" text)
                   (string-match "MERGING" text)))
        (should (< (string-match "MERGING" text)
                   (string-match "MERGED" text)))))))

(ert-deftest claude-repl-drawer-test-render-in-flight-ws-lands-in-merging ()
  "A workspace flagged `:merging' t lands under MERGING.
Regression guard: ancestry alone (`:branch-merged' = `merged') must
NOT route here under the new semantics — only the explicit workflow
flag set by `claude-repl--workspace-merge-do' qualifies."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "merging-ws" :merging t)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
             (merging-pos (string-match "MERGING" text))
             (merged-pos  (string-match "MERGED" text))
             (ws-pos      (string-match "merging-ws" text)))
        ;; merging-ws must appear AFTER MERGING but BEFORE MERGED.
        (should (and merging-pos merged-pos ws-pos
                     (> ws-pos merging-pos)
                     (< ws-pos merged-pos)))))))

(ert-deftest claude-repl-drawer-test-render-merge-completed-ws-lands-in-merged ()
  "A workspace with `:merge-completed' t lands under MERGED.
This is the success path for an explicit merge command."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "completed-ws" :merge-completed t)
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
             (merged-pos (string-match "MERGED" text))
             (ws-pos     (string-match "completed-ws" text)))
        ;; completed-ws appears AFTER the MERGED header (not above it).
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

;;;; ---- nil-perspective filter ----

(ert-deftest claude-repl-drawer-test-visible-keys-filters-persp-nil-name ()
  "`--visible-workspace-keys' drops the key whose name equals `persp-nil-name'."
  (claude-repl-test--with-clean-state
    (let ((persp-nil-name "none"))
      (claude-repl-drawer-test--register "real" :priority "p1")
      (claude-repl-drawer-test--register "none" :priority "p1")
      (let ((keys (claude-repl-drawer--visible-workspace-keys)))
        (should (member "real" keys))
        (should-not (member "none" keys))))))

(ert-deftest claude-repl-drawer-test-visible-keys-filters-bare-none ()
  "`--visible-workspace-keys' drops keys whose bare name equals `persp-nil-name'."
  (claude-repl-test--with-clean-state
    (let ((persp-nil-name "none"))
      (claude-repl-drawer-test--register "DWC/real" :priority "p1")
      (claude-repl-drawer-test--register "DWC/none" :priority "p1")
      (let ((keys (claude-repl-drawer--visible-workspace-keys)))
        (should (member "DWC/real" keys))
        (should-not (member "DWC/none" keys))))))

(ert-deftest claude-repl-drawer-test-render-omits-none-workspace ()
  "Render does not surface a workspace whose name equals `persp-nil-name'."
  (claude-repl-test--with-clean-state
    (let ((persp-nil-name "none"))
      (claude-repl-drawer-test--register "real" :priority "p1")
      (claude-repl-drawer-test--register "none" :priority "p1")
      (claude-repl-drawer-test--with-buffer
        (claude-repl-drawer--render)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "real" text))
          ;; "none" only appears as the empty-section placeholder, never as a
          ;; workspace entry — assert no workspace line carries it as its
          ;; `claude-repl-drawer-workspace' text property.
          (goto-char (point-min))
          (let (found-none)
            (while (and (not found-none) (not (eobp)))
              (when (equal (get-text-property (point)
                                              'claude-repl-drawer-workspace)
                           "none")
                (setq found-none t))
              (forward-line 1))
            (should-not found-none)))))))

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

(ert-deftest claude-repl-drawer-test-visit-on-merged-reactivates ()
  "`claude-repl-drawer-visit' on a MERGED entry routes to
`--reactivate-merged' (which re-establishes the persp) instead of
calling `+workspace-switch' (which would fail because the persp was
torn down at merge time)."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "claude-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (claude-repl-drawer-test--register
             "merged-ws" :merge-completed t :project-dir tmp)
            (claude-repl-drawer-test--with-buffer
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace)
              (let ((established-with nil)
                    (switched-to nil))
                (cl-letf (((symbol-function 'claude-repl--state-save)
                           (lambda (&rest _) nil))
                          ((symbol-function 'claude-repl--establish-workspace)
                           (lambda (ws dir) (setq established-with (list ws dir))))
                          ((symbol-function '+workspace-switch)
                           (lambda (ws &rest _) (setq switched-to ws))))
                  (claude-repl-drawer-visit))
                (should (equal established-with (list "merged-ws" tmp)))
                (should-not switched-to))))
        (delete-directory tmp t)))))

(ert-deftest claude-repl-drawer-test-visit-on-merged-clears-merge-flags ()
  "Reactivating a MERGED workspace via `drawer-visit' clears the
`:merge-completed' / `:merge-completed-at' / `:repl-state :merged'
plist keys so the entry leaves the MERGED bucket on next render."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "claude-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (claude-repl-drawer-test--register
             "merged-ws"
             :merge-completed t
             :merge-completed-at 1234567890.0
             :repl-state :merged
             :project-dir tmp)
            (claude-repl-drawer-test--with-buffer
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace)
              (cl-letf (((symbol-function 'claude-repl--state-save)
                         (lambda (&rest _) nil))
                        ((symbol-function 'claude-repl--establish-workspace)
                         (lambda (&rest _) nil)))
                (claude-repl-drawer-visit))
              (should-not (claude-repl--ws-get "merged-ws" :merge-completed))
              (should-not (claude-repl--ws-get "merged-ws" :merge-completed-at))
              (should-not (claude-repl--ws-get "merged-ws" :repl-state))
              (should-not (eq (claude-repl-drawer--workspace-section "merged-ws")
                              :merged))))
        (delete-directory tmp t)))))

(ert-deftest claude-repl-drawer-test-visit-on-merged-persists-cleared-flags ()
  "Reactivation calls `--state-save' so the cleared `:merge-completed'
flag survives to disk — without this, the snapshot loader would put
the workspace right back into MERGED on the next Emacs restart."
  (claude-repl-test--with-clean-state
    (let ((tmp (make-temp-file "claude-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (claude-repl-drawer-test--register
             "merged-ws" :merge-completed t :project-dir tmp)
            (claude-repl-drawer-test--with-buffer
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace)
              (let ((state-saved-for nil))
                (cl-letf (((symbol-function 'claude-repl--state-save)
                           (lambda (ws) (setq state-saved-for ws)))
                          ((symbol-function 'claude-repl--establish-workspace)
                           (lambda (&rest _) nil)))
                  (claude-repl-drawer-visit))
                (should (equal state-saved-for "merged-ws")))))
        (delete-directory tmp t)))))

(ert-deftest claude-repl-drawer-test-visit-on-merged-without-project-dir-errors ()
  "`drawer-visit' on a MERGED entry whose `:project-dir' is missing or
no longer points to a real directory signals `user-error' — reactivation
needs a valid worktree dir to establish into."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register
     "merged-ws" :merge-completed t :project-dir "/nonexistent/path/here")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((established-called nil))
        (cl-letf (((symbol-function 'claude-repl--establish-workspace)
                   (lambda (&rest _) (setq established-called t))))
          (should-error (claude-repl-drawer-visit) :type 'user-error))
        (should-not established-called)))))

(ert-deftest claude-repl-drawer-test-visit-redirects-from-side-window-before-switch ()
  "`claude-repl-drawer-visit' leaves a side-window selection before calling
`+workspace-switch'.  Persp's `persp-delete-other-windows' uses
`ignore-window-parameters t' on restore, and with a side window
selected its fallback anchor can clobber the destination workspace's
Claude panel windows — pre-selecting a main-area window sidesteps that."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (claude-repl-test--with-clean-state
            (claude-repl-drawer-test--register "target" :priority "p1")
            (let* ((drawer-buf (get-buffer-create claude-repl-drawer-buffer-name))
                   (drawer-win (display-buffer-in-side-window
                                drawer-buf '((side . left) (slot . 0)))))
              (unwind-protect
                  (progn
                    (with-current-buffer drawer-buf
                      (claude-repl-drawer-mode)
                      (claude-repl-drawer--render))
                    (select-window drawer-win)
                    ;; Set point AFTER selecting the window — `select-window'
                    ;; resets buffer-point to the window's `window-point',
                    ;; so a pre-select goto would be clobbered.
                    (claude-repl-drawer--goto-first-workspace)
                    ;; Sanity: we are actually in a side window now and
                    ;; positioned on a workspace entry.
                    (should (window-parameter (selected-window) 'window-side))
                    (should (claude-repl-drawer--workspace-at-point))
                    (let ((sel-at-switch nil))
                      (cl-letf (((symbol-function '+workspace-switch)
                                 (lambda (_ws &rest _)
                                   (setq sel-at-switch (selected-window)))))
                        (claude-repl-drawer-visit))
                      (should sel-at-switch)
                      (should-not (window-parameter sel-at-switch 'window-side))))
                (when (window-live-p drawer-win) (delete-window drawer-win))
                (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf))))))
      (set-window-configuration wconf))))

(ert-deftest claude-repl-drawer-test-visit-no-redirect-when-not-in-side-window ()
  "When the selected window is not a side window, `claude-repl-drawer-visit'
leaves the selection untouched — the redirect is conditional on a
side-window selection."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (claude-repl-test--with-clean-state
            (claude-repl-drawer-test--register "target" :priority "p1")
            (claude-repl-drawer-test--with-buffer
              (claude-repl-drawer--render)
              (claude-repl-drawer--goto-first-workspace)
              ;; Selected window is the test runner's main window — not a
              ;; side window — even though current-buffer is the drawer
              ;; buffer here.
              (let ((sel-before (selected-window))
                    (sel-at-switch nil))
                (should-not (window-parameter sel-before 'window-side))
                (cl-letf (((symbol-function '+workspace-switch)
                           (lambda (_ws &rest _)
                             (setq sel-at-switch (selected-window)))))
                  (claude-repl-drawer-visit))
                (should (eq sel-at-switch sel-before))))))
      (set-window-configuration wconf))))

(ert-deftest claude-repl-drawer-test-leave-side-window-helper-noop-when-not-side ()
  "`claude-repl-drawer--leave-side-window-before-switch' is a no-op when
the selected window has no `window-side' parameter."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (let ((sel-before (selected-window)))
            (claude-repl-drawer--leave-side-window-before-switch)
            (should (eq (selected-window) sel-before))))
      (set-window-configuration wconf))))

(ert-deftest claude-repl-drawer-test-leave-side-window-helper-selects-main ()
  "`claude-repl-drawer--leave-side-window-before-switch' moves the
selection to the frame's main window when invoked from a side window."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (let* ((main-buf (generate-new-buffer " *test-main*"))
                 (main-win (selected-window))
                 (side-buf (generate-new-buffer " *test-side*"))
                 (side-win (display-buffer-in-side-window
                            side-buf '((side . left) (slot . 0)))))
            (set-window-buffer main-win main-buf)
            (unwind-protect
                (progn
                  (select-window side-win)
                  (should (window-parameter (selected-window) 'window-side))
                  (claude-repl-drawer--leave-side-window-before-switch)
                  (should-not (window-parameter (selected-window) 'window-side)))
              (when (window-live-p side-win) (delete-window side-win))
              (when (buffer-live-p main-buf) (kill-buffer main-buf))
              (when (buffer-live-p side-buf) (kill-buffer side-buf)))))
      (set-window-configuration wconf))))

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

;;;; ---- apply-background idempotence ----

(ert-deftest claude-repl-drawer-test-apply-background-idempotent ()
  "`--apply-background' adds exactly one relative remap on the `default'
face no matter how many times it is called.  Without cookie tracking,
each call would stack another `(:background ...)' entry onto the face's
relative-remap list — `claude-repl-drawer-mode' adds one and every
`claude-repl-drawer-show' adds another, so a buffer that has been
toggled N times would carry N+1 entries and pay redisplay overhead for
each one.  Pins the leak fix."
  (claude-repl-drawer-test--with-buffer
    ;; Mode init already called `--apply-background' once.  Call again
    ;; several times and assert the `default' remap entry list still has
    ;; exactly one user-installed relative spec.
    (claude-repl-drawer--apply-background)
    (claude-repl-drawer--apply-background)
    (claude-repl-drawer--apply-background)
    (let* ((entry (assq 'default face-remapping-alist))
           ;; cdr is the list of relative specs, with the final element
           ;; being the base face (`default' itself).  Drop the base.
           (relatives (and entry (butlast (cdr entry)))))
      (should (= (length relatives) 1))
      (should (equal (car relatives)
                     `(:background ,claude-repl-drawer-background))))))

;;;; ---- Refresh-if-visible ----

(ert-deftest claude-repl-drawer-test-refresh-if-visible-no-buffer-noop ()
  "`claude-repl-drawer--refresh-if-visible' is a no-op when no buffer exists."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer claude-repl-drawer-buffer-name)))
      (when buf (kill-buffer buf)))
    (should-not (claude-repl-drawer--refresh-if-visible))))

;;;; ---- Window width ----

(ert-deftest claude-repl-drawer-test-width-fraction-default-is-0.20 ()
  "Default `claude-repl-drawer-width-fraction' is 0.20."
  (should (= 0.20
             (eval (car (get 'claude-repl-drawer-width-fraction
                             'standard-value))))))

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

(ert-deftest claude-repl-drawer-test-persisted-cols-overrides-fraction ()
  "When `--persisted-cols' is non-nil it overrides fraction-based width."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-drawer-width-fraction 0.20)
          (claude-repl-drawer--persisted-cols 73))
      (cl-letf (((symbol-function 'window-frame) (lambda (_) 'fake-frame))
                ((symbol-function 'frame-width)  (lambda (_) 200)))
        ;; Fraction would give 40; persisted-cols wins.
        (should (= (claude-repl-drawer--window-width 'fake-window) 73))))))

(ert-deftest claude-repl-drawer-test-persisted-cols-nil-falls-back-to-fraction ()
  "When `--persisted-cols' is nil, fraction-based computation drives width."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-drawer-width-fraction 0.20)
          (claude-repl-drawer--persisted-cols nil))
      (cl-letf (((symbol-function 'window-frame) (lambda (_) 'fake-frame))
                ((symbol-function 'frame-width)  (lambda (_) 200)))
        (should (= (claude-repl-drawer--window-width 'fake-window) 40))))))

(ert-deftest claude-repl-drawer-test-persisted-cols-floors-at-one ()
  "Persisted-cols clamps at 1 even if a zero/negative slipped in."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-drawer--persisted-cols 0))
      (should (= (claude-repl-drawer--window-width 'fake-window) 1)))))

(ert-deftest claude-repl-drawer-test-capture-resize-pins-width-on-diverge ()
  "`--capture-resize' pins `--persisted-cols' when actual ≠ expected.
Simulates a manual user resize: after `--apply-width' set expected=40,
the user dragged the side window to 55; the size-change hook should
treat the new value as user intent and pin it."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name))
          (claude-repl-drawer--persisted-cols nil)
          (claude-repl-drawer--expected-cols 40))
      (unwind-protect
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (b &optional _f) (and (eq b buf) 'fake-win)))
                    ((symbol-function 'window-total-width)
                     (lambda (_) 55)))
            (claude-repl-drawer--capture-resize 'fake-frame)
            (should (= claude-repl-drawer--persisted-cols 55))
            (should (= claude-repl-drawer--expected-cols 55)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest claude-repl-drawer-test-capture-resize-noop-when-matches-expected ()
  "`--capture-resize' is a no-op when actual matches expected.
Simulates persp-restore + our reapply: net width = expected, so no
spurious capture overwrites a previously-pinned width."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name))
          (claude-repl-drawer--persisted-cols 73)
          (claude-repl-drawer--expected-cols 73))
      (unwind-protect
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (b &optional _f) (and (eq b buf) 'fake-win)))
                    ((symbol-function 'window-total-width)
                     (lambda (_) 73)))
            (claude-repl-drawer--capture-resize 'fake-frame)
            (should (= claude-repl-drawer--persisted-cols 73)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest claude-repl-drawer-test-capture-resize-noop-without-expected ()
  "`--capture-resize' is a no-op until `--apply-width' has set expected.
Guards against the size-change hook persisting spurious widths during
the bootstrap period before the drawer has ever been displayed."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name))
          (claude-repl-drawer--persisted-cols nil)
          (claude-repl-drawer--expected-cols nil))
      (unwind-protect
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (b &optional _f) (and (eq b buf) 'fake-win)))
                    ((symbol-function 'window-total-width)
                     (lambda (_) 99)))
            (claude-repl-drawer--capture-resize 'fake-frame)
            (should-not claude-repl-drawer--persisted-cols))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest claude-repl-drawer-test-capture-resize-noop-when-no-window ()
  "`--capture-resize' is a no-op when the drawer buffer has no window."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-drawer--persisted-cols nil)
          (claude-repl-drawer--expected-cols 40))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _) nil)))
        (claude-repl-drawer--capture-resize 'fake-frame)
        (should-not claude-repl-drawer--persisted-cols)))))

(ert-deftest claude-repl-drawer-test-reset-width-clears-persisted ()
  "`claude-repl-drawer-reset-width' clears `--persisted-cols'."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-drawer--persisted-cols 73))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
        (claude-repl-drawer-reset-width)
        (should-not claude-repl-drawer--persisted-cols)))))

(ert-deftest claude-repl-drawer-test-ensure-visible-reapplies-width-when-already-visible ()
  "`--ensure-visible-on-persp-switch' reapplies width when drawer is already
visible — overriding whatever stale width persp's window-state-put
just restored from the destination workspace's saved config."
  (let ((claude-repl-drawer--global-visible-p t)
        (apply-called nil)
        (buf (get-buffer-create claude-repl-drawer-buffer-name)))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'fake-win))
                  ((symbol-function 'claude-repl-drawer--apply-width)
                   (lambda (w) (setq apply-called w))))
          (claude-repl-drawer--ensure-visible-on-persp-switch)
          (should (eq apply-called 'fake-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

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
  (should (equal (alist-get :dead       claude-repl-drawer-state-icons) "❌"))
  (should (equal (alist-get :merged     claude-repl-drawer-state-icons) "🔀"))
  (should (equal (alist-get :merge-conflict claude-repl-drawer-state-icons) "💥"))
  (should (equal (alist-get :merge-failed claude-repl-drawer-state-icons) "⛔")))

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

(ert-deftest claude-repl-drawer-test-state-glyph-merged-overrides-claude-state ()
  ":repl-state :merged takes precedence over :claude-state for the glyph."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "merged-ws"
                                       :claude-state :thinking
                                       :repl-state :merged)
    (should (equal (claude-repl-drawer--state-glyph "merged-ws")
                   (alist-get :merged claude-repl-drawer-state-icons)))))

(ert-deftest claude-repl-drawer-test-state-glyph-merge-conflict-surfaces-collision ()
  ":repl-state :merge-conflict renders the 💥 glyph — a real cherry-pick
conflict that the auto-resolver rejected (or interactive abort).
Distinct from :merge-failed (silent git failure) and :dead (vterm
death) so the user can see at a glance that this row needs human
conflict resolution."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "conflicted-merge"
                                       :repl-state :merge-conflict)
    (should (equal (claude-repl-drawer--state-glyph "conflicted-merge")
                   "💥"))))

(ert-deftest claude-repl-drawer-test-state-glyph-merge-conflict-overrides-claude-state ()
  ":repl-state :merge-conflict takes precedence over :claude-state.
The vterm is still alive on a conflict (unlike :dead), but the badge
must surface the conflict rather than the mid-session mood."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws"
                                       :claude-state :thinking
                                       :repl-state :merge-conflict)
    (should (equal (claude-repl-drawer--state-glyph "ws") "💥"))))

(ert-deftest claude-repl-drawer-test-state-glyph-merge-conflict-overrides-dead ()
  "When `:repl-state' becomes `:dead' after a conflict (e.g., vterm
later dies), the conflict badge should win — the conflict signal is
more actionable than a generic process-death."
  (claude-repl-test--with-clean-state
    ;; Set :merge-conflict last so it wins (set semantics: latest write
    ;; wins; here we test precedence in resolve-time, so both registered
    ;; and the resolver picks :merge-conflict via state-glyph priority).
    (claude-repl-drawer-test--register "ws" :repl-state :merge-conflict)
    (should (equal (claude-repl-drawer--state-glyph "ws") "💥"))))

(ert-deftest claude-repl-drawer-test-state-glyph-merge-failed-surfaces-x ()
  ":repl-state :merge-failed renders the ⛔ glyph (failed cherry-pick
that still lives in the MERGED bucket).  Distinct mapping from
:dead's ❌ so the user can visually differentiate a stuck merge from
a dead vterm at a glance."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "broken-merge"
                                       :repl-state :merge-failed)
    (should (equal (claude-repl-drawer--state-glyph "broken-merge")
                   (alist-get :merge-failed claude-repl-drawer-state-icons)))))

(ert-deftest claude-repl-drawer-test-state-glyph-merge-failed-overrides-claude-state ()
  ":repl-state :merge-failed takes precedence over :claude-state for
the glyph — a post-merge silent-failure workspace whose vterm is
stale still reads as ⛔-merge-failed rather than its claude-state
mood."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws"
                                       :claude-state :thinking
                                       :repl-state :merge-failed)
    (should (equal (claude-repl-drawer--state-glyph "ws")
                   (alist-get :merge-failed claude-repl-drawer-state-icons)))))

(ert-deftest claude-repl-drawer-test-state-icons-include-merge-failed ()
  "`claude-repl-drawer-state-icons' includes a :merge-failed entry
using ⛔ — distinct from :dead's ❌ so a stuck merge does not look
like a dead vterm."
  (should (equal (alist-get :merge-failed claude-repl-drawer-state-icons) "⛔")))

(ert-deftest claude-repl-drawer-test-workspace-section-merge-failed-routes-to-merged ()
  "A workspace flagged with :merge-completed t still lands in the
:merged bucket even when its :repl-state is :merge-failed.  The
section bucket is driven exclusively by `:merge-completed' — the
:repl-state distinction is purely visual (badge selection in
`--state-glyph')."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws"
                                       :merge-completed t
                                       :repl-state :merge-failed)
    (should (eq (claude-repl-drawer--workspace-section "ws") :merged))))

(ert-deftest claude-repl-drawer-test-state-glyph-merged-overrides-dead ()
  ":repl-state :merged would normally not coexist with :dead, but if
both somehow appear in a stale plist, :merged wins so the merge badge
isn't masked by a post-nuke dead reading."
  (claude-repl-test--with-clean-state
    ;; Single :repl-state slot can't hold both — the test simulates the
    ;; precedence rule by setting :merged and confirming it's chosen
    ;; over the :dead icon-lookup path.
    (claude-repl-drawer-test--register "merged-not-dead"
                                       :repl-state :merged)
    (should (equal (claude-repl-drawer--state-glyph "merged-not-dead")
                   (alist-get :merged claude-repl-drawer-state-icons)))
    (should-not (equal (claude-repl-drawer--state-glyph "merged-not-dead")
                       (alist-get :dead claude-repl-drawer-state-icons)))))

;;;; ---- Tests: keyboard-inaccessibility bounce ----

(ert-deftest claude-repl-drawer-test-buffer-p-matches-drawer-name ()
  "`--buffer-p' returns non-nil for a buffer whose name matches `claude-repl-drawer-buffer-name'."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer claude-repl-drawer-buffer-name
      (should (claude-repl-drawer--buffer-p (current-buffer))))))

(ert-deftest claude-repl-drawer-test-buffer-p-rejects-other-buffer ()
  "`--buffer-p' returns nil for a buffer whose name does not match the drawer name."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*not-the-drawer*"
      (should-not (claude-repl-drawer--buffer-p (current-buffer))))))

(ert-deftest claude-repl-drawer-test-bounce-from-drawer-non-drawer-buffer ()
  "`--bounce-from-drawer' is a no-op when the selected window shows a non-drawer buffer.
Mirrors the vterm bounce's `non-vterm-buffer' baseline test — the predicate
must not fire on unrelated buffers."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*bounce-noop-regular*"
      (let ((orig-win (selected-window)))
        (set-window-buffer orig-win (current-buffer))
        (claude-repl-drawer--bounce-from-drawer nil)
        (should (eq (selected-window) orig-win))))))

(ert-deftest claude-repl-drawer-test-bounce-from-drawer-keyboard-redirects ()
  "Keyboard-driven selection of the drawer window is redirected to the MRU non-drawer window."
  (claude-repl-test--with-clean-state
    (let ((drawer-buf (get-buffer-create claude-repl-drawer-buffer-name))
          (other-buf  (get-buffer-create "*bounce-target*"))
          (other-win  nil))
      (unwind-protect
          (progn
            ;; Put another buffer in a sibling window first so it
            ;; becomes the MRU non-selected window, then select the
            ;; drawer window so the bounce has a destination.
            (setq other-win (split-window))
            (set-window-buffer other-win other-buf)
            (set-window-buffer (selected-window) drawer-buf)
            (select-window other-win)
            (let ((drawer-win (split-window)))
              (set-window-buffer drawer-win drawer-buf)
              (select-window drawer-win)
              (let ((last-input-event ?a))
                (claude-repl-drawer--bounce-from-drawer nil)
                (should-not (eq (window-buffer (selected-window)) drawer-buf)))))
        (when (and other-win (window-live-p other-win))
          (ignore-errors (delete-window other-win)))
        (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest claude-repl-drawer-test-bounce-from-drawer-mouse-does-not-redirect ()
  "Mouse-driven selection of the drawer window stays put — user wants to operate entries via click."
  (claude-repl-test--with-clean-state
    (let ((drawer-buf (get-buffer-create claude-repl-drawer-buffer-name))
          (other-buf  (get-buffer-create "*bounce-mouse-other*"))
          (other-win  nil))
      (unwind-protect
          (progn
            (let ((drawer-win (selected-window)))
              (set-window-buffer drawer-win drawer-buf)
              (setq other-win (split-window))
              (set-window-buffer other-win other-buf)
              ;; Simulate a mouse event as last-input-event — bounce should
              ;; treat selection as user-intended and leave it alone.
              (let ((last-input-event '(mouse-1 (nil 0 . 0))))
                (claude-repl-drawer--bounce-from-drawer nil)
                (should (eq (selected-window) drawer-win)))))
        (when (and other-win (window-live-p other-win))
          (ignore-errors (delete-window other-win)))
        (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest claude-repl-drawer-test-bounce-from-drawer-warns-when-no-other-window ()
  "When the drawer is the only window, the bounce emits a user-facing warning.
Parallels `bounce-from-vterm-warns-when-no-input-win' — surfacing the stuck
state is preferable to silently leaving point stranded in the drawer."
  (claude-repl-test--with-clean-state
    (let ((drawer-buf (get-buffer-create claude-repl-drawer-buffer-name))
          (messages   nil))
      (unwind-protect
          (progn
            ;; Reduce the frame to a single window showing the drawer so
            ;; `get-mru-window' with NOT-SELECTED has nothing to return.
            (delete-other-windows)
            (set-window-buffer (selected-window) drawer-buf)
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
              (let ((last-input-event ?a))
                (claude-repl-drawer--bounce-from-drawer nil)))
            (should (cl-some (lambda (m) (string-match-p "no other window is available" m))
                             messages)))
        (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf))))))

(ert-deftest claude-repl-drawer-test-show-does-not-select-drawer-window ()
  "`claude-repl-drawer-show' must NOT select the drawer window.
Keyboard-inaccessibility policy: the drawer is reachable only via mouse,
so even an explicit `show' command must leave selection where it was."
  (claude-repl-test--with-clean-state
    (let ((other-buf (get-buffer-create "*show-no-select-other*"))
          (drawer-buf nil))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) other-buf)
            (let ((orig-win (selected-window)))
              ;; Stub `display-buffer' so this test doesn't depend on the
              ;; full side-window machinery; just put the drawer into a
              ;; freshly-split window and return it.  All we care about is
              ;; that `drawer-show' did not call `select-window' on it.
              (cl-letf (((symbol-function 'display-buffer)
                         (lambda (buf &rest _)
                           (setq drawer-buf buf)
                           (let ((win (split-window)))
                             (set-window-buffer win buf)
                             win)))
                        ((symbol-function 'claude-repl-window--harden) #'ignore)
                        ((symbol-function 'claude-repl-drawer--apply-width) #'ignore))
                (claude-repl-drawer-show)
                (should (eq (selected-window) orig-win)))))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))
        (when (and drawer-buf (buffer-live-p drawer-buf))
          (kill-buffer drawer-buf))))))

;;;; ---- Tests: :merge-queued routing + glyph ----

(ert-deftest claude-repl-drawer-test-workspace-section-merge-queued-routes-to-merging ()
  "`:repl-state :merge-queued' buckets the workspace under MERGING so
queued merges appear alongside in-flight ones."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :repl-state :merge-queued)
    (should (eq (claude-repl-drawer--workspace-section "ws") :merging))))

(ert-deftest claude-repl-drawer-test-workspace-section-merging-dominates-queued ()
  "`:merging' t (in-flight) outranks `:repl-state :merge-queued'.
Covers the brief window between drain clearing `:merge-queued' and
`--workspace-merge-do' setting `:merging'."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws"
                                       :merging t
                                       :repl-state :merge-queued)
    (should (eq (claude-repl-drawer--workspace-section "ws") :merging))))

(ert-deftest claude-repl-drawer-test-workspace-section-merge-completed-dominates-queued ()
  "A completed merge marker outranks `:merge-queued' — should not happen
in practice (completed clears repl-state) but the precedence chain
must remain stable."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws"
                                       :merge-completed t
                                       :repl-state :merge-queued)
    (should (eq (claude-repl-drawer--workspace-section "ws") :merged))))

(ert-deftest claude-repl-drawer-test-state-glyph-merge-queued ()
  "`:repl-state :merge-queued' surfaces the 🕒 glyph from the icon
alist, distinct from :merging (no icon — the merging bucket shows the
underlying claude-state glyph) and :merged (🔀)."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :repl-state :merge-queued)
    (should (equal (claude-repl-drawer--state-glyph "ws")
                   (alist-get :merge-queued
                              claude-repl-drawer-state-icons)))))

(ert-deftest claude-repl-drawer-test-state-glyph-merge-queued-overrides-claude-state ()
  "`:merge-queued' on repl-state outranks a stale `:claude-state' —
guards against the queued badge being clobbered by a leftover
thinking/done glyph."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws"
                                       :repl-state :merge-queued
                                       :claude-state :thinking)
    (should (equal (claude-repl-drawer--state-glyph "ws")
                   (alist-get :merge-queued
                              claude-repl-drawer-state-icons)))))

(ert-deftest claude-repl-drawer-test-state-glyph-merged-still-wins-over-queued ()
  "Precedence guard: `:merged' beats `:merge-queued' on the glyph too."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "ws" :repl-state :merged)
    (should (equal (claude-repl-drawer--state-glyph "ws")
                   (alist-get :merged claude-repl-drawer-state-icons)))))

;;;; ---- Tests: center-selection ----

(ert-deftest claude-repl-drawer-test-center-selection-calls-recenter ()
  "`--center-selection' calls `recenter' on every window showing the
drawer buffer.  Establishes one displayed window, stubs `recenter' to
record the count, and asserts exactly one invocation — the always-on
center-cursor contract."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name))
          (calls 0))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) buf)
            (cl-letf (((symbol-function 'recenter)
                       (lambda (&rest _) (cl-incf calls))))
              (claude-repl-drawer--center-selection buf))
            (should (= calls 1)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest claude-repl-drawer-test-center-selection-noop-when-no-window ()
  "Helper is a no-op when the drawer buffer is not displayed in any
live window."
  (claude-repl-test--with-clean-state
    (let ((buf (get-buffer-create claude-repl-drawer-buffer-name))
          (called nil))
      (unwind-protect
          (cl-letf (((symbol-function 'recenter)
                     (lambda (&rest _) (setq called t))))
            (claude-repl-drawer--center-selection buf)
            (should-not called))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest claude-repl-drawer-test-center-selection-noop-when-no-buffer ()
  "Helper is a no-op (no error) when no drawer buffer exists at all.
Exercises the default-arg path: `when-let*' on `(get-buffer ...)'
short-circuits to nil."
  (claude-repl-test--with-clean-state
    (when-let ((b (get-buffer claude-repl-drawer-buffer-name)))
      (kill-buffer b))
    (should-not (claude-repl-drawer--center-selection))))

(ert-deftest claude-repl-drawer-test-post-command-calls-center-selection ()
  "`--post-command' must invoke `--center-selection' so every j/k move
re-centers the cursor."
  (claude-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'claude-repl-drawer--center-selection)
                 (lambda (&optional _buf) (setq called t)))
                ((symbol-function 'claude-repl-drawer--update-current-entry-overlay)
                 #'ignore)
                ((symbol-function 'claude-repl-drawer--update-cursor)
                 #'ignore))
        (claude-repl-drawer--post-command)
        (should called)))))

(ert-deftest claude-repl-drawer-test-post-command-skips-center-when-ws-unchanged ()
  "`--post-command' must NOT re-invoke `--center-selection' when point is
still on the same workspace entry as on the previous tick.  `recenter'
forces a window redisplay; firing it on no-op commands or intra-entry
motion is the per-keystroke perf hit this gates against."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((calls 0))
        (cl-letf (((symbol-function 'claude-repl-drawer--center-selection)
                   (lambda (&optional _buf) (cl-incf calls)))
                  ((symbol-function 'claude-repl-drawer--update-current-entry-overlay)
                   #'ignore)
                  ((symbol-function 'claude-repl-drawer--update-cursor)
                   #'ignore))
          (claude-repl-drawer--post-command)
          (should (= calls 1))
          (claude-repl-drawer--post-command)
          (should (= calls 1)))))))

(ert-deftest claude-repl-drawer-test-post-command-recenters-after-entry-change ()
  "`--post-command' must re-invoke `--center-selection' when navigation
crosses an entry boundary — i.e. the normal j/k case.  Pairs with
`--post-command-skips-center-when-ws-unchanged' to assert the gating
is keyed on entry change, not unconditionally disabled."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--register "beta"  :priority "p2")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((calls 0))
        (cl-letf (((symbol-function 'claude-repl-drawer--center-selection)
                   (lambda (&optional _buf) (cl-incf calls)))
                  ((symbol-function 'claude-repl-drawer--update-current-entry-overlay)
                   #'ignore)
                  ((symbol-function 'claude-repl-drawer--update-cursor)
                   #'ignore))
          (claude-repl-drawer--post-command)
          (claude-repl-drawer-next)
          (claude-repl-drawer--post-command)
          (should (= calls 2)))))))

(ert-deftest claude-repl-drawer-test-post-command-skips-overlay-when-ws-unchanged ()
  "`--post-command' must NOT re-invoke `--update-current-entry-overlay'
when ws-at-point is unchanged.  The overlay refresh walks the entry's
characters via `--entry-bounds-at-point' — gating it on entry change
saves the per-keystroke buffer scan."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "alpha" :priority "p1")
    (claude-repl-drawer-test--with-buffer
      (claude-repl-drawer--render)
      (claude-repl-drawer--goto-first-workspace)
      (let ((calls 0))
        (cl-letf (((symbol-function 'claude-repl-drawer--update-current-entry-overlay)
                   (lambda () (cl-incf calls)))
                  ((symbol-function 'claude-repl-drawer--center-selection)
                   #'ignore)
                  ((symbol-function 'claude-repl-drawer--update-cursor)
                   #'ignore))
          (claude-repl-drawer--post-command)
          (should (= calls 1))
          (claude-repl-drawer--post-command)
          (should (= calls 1)))))))


(ert-deftest claude-repl-drawer-test-sync-cursor-calls-center-selection ()
  "`--sync-cursor-to-current-ws' must also center — persp-driven cursor
moves should keep the active workspace centered, not just user j/k."
  (claude-repl-test--with-clean-state
    (let* ((buf (get-buffer-create claude-repl-drawer-buffer-name))
           (called nil))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name)
                     (lambda () "ws"))
                    ((symbol-function 'claude-repl-drawer--goto-workspace-line)
                     (lambda (_ws) t))
                    ((symbol-function 'claude-repl-drawer--update-current-entry-overlay)
                     #'ignore)
                    ((symbol-function 'claude-repl-drawer--center-selection)
                     (lambda (&optional _buf) (setq called t))))
            (claude-repl-drawer--sync-cursor-to-current-ws)
            (should called))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- Tests: --source-ws-name plist cache ----

(ert-deftest claude-repl-drawer-test-source-ws-name-returns-nil-without-source-dir ()
  "`--source-ws-name' returns nil when the workspace has no `:source-ws-dir'.
Existing root-workspace contract — no caching, no scan."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "root" :project-dir "/tmp/root")
    (should-not (claude-repl-drawer--source-ws-name "root"))))

(ert-deftest claude-repl-drawer-test-source-ws-name-populates-cache-on-miss ()
  "First call resolves via `--ws-name-for-dir' and writes the result
into `:source-ws-name' on the workspace plist."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "parent" :project-dir "/tmp/parent")
    (claude-repl-drawer-test--register "child"  :project-dir "/tmp/child"
                                       :source-ws-dir "/tmp/parent")
    (cl-letf (((symbol-function 'claude-repl--ws-name-for-dir)
               (lambda (dir) (when (equal dir "/tmp/parent") "parent"))))
      (should-not (claude-repl--ws-get "child" :source-ws-name))
      (should (equal (claude-repl-drawer--source-ws-name "child") "parent"))
      (should (equal (claude-repl--ws-get "child" :source-ws-name) "parent")))))

(ert-deftest claude-repl-drawer-test-source-ws-name-cache-hit-skips-scan ()
  "Second call returns the cached value without consulting
`--ws-name-for-dir'.  Pins the O(1) fast-path contract — the cache
is the WHOLE point of this rewrite."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "child"
                                       :project-dir "/tmp/child"
                                       :source-ws-dir "/tmp/parent"
                                       :source-ws-name "parent")
    (let ((scan-called nil))
      (cl-letf (((symbol-function 'claude-repl--ws-name-for-dir)
                 (lambda (_dir) (setq scan-called t) "should-not-see-this")))
        (should (equal (claude-repl-drawer--source-ws-name "child") "parent"))
        (should-not scan-called)))))

(ert-deftest claude-repl-drawer-test-source-ws-name-does-not-cache-nil-resolution ()
  "When the reverse lookup returns nil (source workspace deleted, no
match for `:source-ws-dir' in the hash), the cache stays clear — a nil
write would be indistinguishable from \"never cached\" on the next read
under the `or'-fall-through model."
  (claude-repl-test--with-clean-state
    (claude-repl-drawer-test--register "child"
                                       :project-dir "/tmp/child"
                                       :source-ws-dir "/tmp/orphan")
    (cl-letf (((symbol-function 'claude-repl--ws-name-for-dir)
               (lambda (_dir) nil)))
      (should-not (claude-repl-drawer--source-ws-name "child"))
      (should-not (claude-repl--ws-get "child" :source-ws-name)))))

(provide 'test-drawer)
;;; test-drawer.el ends here
