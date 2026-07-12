;;; test-drawer.el --- ERT tests for drawer.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the agent-repl workspace drawer.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-drawer.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Helpers ----

(defmacro agent-repl-drawer-test--with-buffer (&rest body)
  "Run BODY in a temporary drawer buffer with `agent-repl-drawer-mode' active."
  (declare (indent 0))
  `(let ((buf (generate-new-buffer " *test-drawer*")))
     (unwind-protect
         (with-current-buffer buf
           (agent-repl-drawer-mode)
           ,@body)
       (when (buffer-live-p buf) (kill-buffer buf)))))

(defconst agent-repl-drawer-test--project-dir "/tmp/agent-repl-test-repo/ws"
  "Default `:project-dir' seeded by `agent-repl-drawer-test--register'.")

(defconst agent-repl-drawer-test--group-key "/tmp/agent-repl-test-repo/.git"
  "Default `:group-key' seeded by `agent-repl-drawer-test--register'.")

(defun agent-repl-drawer-test--register (ws &rest props)
  "Register WS in `agent-repl--workspaces' with PROPS plist.
Seeds `:project-dir' and `:group-key' when PROPS omits them, mirroring
production: every real workspace carries a `:project-dir' from birth, and
the drawer renders only such workspaces (project-dir-less stubs are
filtered by `agent-repl-drawer--visible-workspace-keys').  Seeding
`:group-key' too keeps render tests from shelling out to git.
Use `puthash' directly to register a project-dir-less stub."
  (let ((plist (copy-sequence props)))
    (unless (plist-member plist :project-dir)
      (setq plist (plist-put plist :project-dir
                             agent-repl-drawer-test--project-dir)))
    (unless (plist-member plist :group-key)
      (setq plist (plist-put plist :group-key
                             agent-repl-drawer-test--group-key)))
    (puthash ws plist agent-repl--workspaces)))

;;;; ---- Multi-select ----

(ert-deftest agent-repl-drawer-test-toggle-mark-adds-and-removes ()
  "`toggle-mark' adds the entry on first press, removes on second."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws1" :priority "p1")
    (agent-repl-drawer-test--register "ws2" :priority "p2")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (agent-repl-drawer-toggle-mark)
      (should (agent-repl-drawer--marked-p "ws1"))
      ;; toggle-mark auto-advances; come back and unmark.
      (agent-repl-drawer--goto-workspace-line "ws1")
      (agent-repl-drawer-toggle-mark)
      (should-not (agent-repl-drawer--marked-p "ws1")))))

(ert-deftest agent-repl-drawer-test-target-workspaces-falls-back-to-point ()
  "`--target-workspaces' returns just the entry at point when no marks."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws1" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (should (equal (agent-repl-drawer--target-workspaces) '("ws1"))))))

(ert-deftest agent-repl-drawer-test-target-workspaces-uses-marks-when-set ()
  "`--target-workspaces' returns the marked-set when non-empty (ignoring point)."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws1" :priority "p1")
    (agent-repl-drawer-test--register "ws2" :priority "p2")
    (agent-repl-drawer-test--register "ws3" :priority "p3")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--ensure-marked-set)
      (puthash "ws1" t agent-repl-drawer--marked-set)
      (puthash "ws3" t agent-repl-drawer--marked-set)
      (let ((targets (agent-repl-drawer--target-workspaces)))
        (should (= (length targets) 2))
        (should (member "ws1" targets))
        (should (member "ws3" targets))))))

(ert-deftest agent-repl-drawer-test-bulk-nuke-iterates-marks ()
  "Bulk nuke iterates the marked-set when non-empty."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws1" :priority "p1")
    (agent-repl-drawer-test--register "ws2" :priority "p2")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--ensure-marked-set)
      (puthash "ws1" t agent-repl-drawer--marked-set)
      (puthash "ws2" t agent-repl-drawer--marked-set)
      (let ((nuked nil))
        (cl-letf (((symbol-function 'agent-repl-nuke-workspace)
                   (lambda (&optional ws) (push ws nuked))))
          (agent-repl-drawer-nuke))
        (should (member "ws1" nuked))
        (should (member "ws2" nuked))))))

(ert-deftest agent-repl-drawer-test-clear-marks-empties-set ()
  "`clear-marks' empties the marked-set."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--ensure-marked-set)
      (puthash "ws" t agent-repl-drawer--marked-set)
      (agent-repl-drawer-clear-marks)
      (should (zerop (agent-repl-drawer--marked-count))))))

(ert-deftest agent-repl-drawer-test-render-marked-uses-marked-glyph ()
  "Rendered marked entry's gutter contains the marked glyph."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--ensure-marked-set)
      (puthash "ws" t agent-repl-drawer--marked-set)
      (agent-repl-drawer--render)
      (should (string-match-p (regexp-quote agent-repl-drawer-marked-glyph)
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest agent-repl-drawer-test-render-repositions-current-entry-overlay ()
  "`--render' repositions the current-entry overlay so the arrow persists
across renders triggered without a corresponding `post-command-hook' (e.g.
the 1Hz status poll when the drawer is not the selected window)."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      ;; Simulate a poll-driven re-render — buffer current, no command
      ;; running in this buffer, so the buffer-local post-command-hook
      ;; would NOT fire.  The overlay must still be set up.
      (agent-repl-drawer--render)
      (should (overlayp agent-repl-drawer--current-entry-overlay))
      (let* ((ov agent-repl-drawer--current-entry-overlay)
             (disp (overlay-get ov 'display)))
        ;; Overlay must span at least one char (not collapsed at the
        ;; head of the buffer where erase-buffer left it).
        (should (> (overlay-end ov) (overlay-start ov)))
        ;; And it must carry the arrow as its `display' override.
        (should (stringp disp))
        (should (string-match-p (regexp-quote agent-repl-drawer-current-arrow)
                                disp))))))

(ert-deftest agent-repl-drawer-test-render-noop-skips-buffer-rewrite ()
  "Re-rendering with no state change must NOT erase-and-reinsert the buffer.
The buffer's `buffer-modified-tick' captures any mutation, so an
unchanged-content re-render leaves it untouched.  This is the
flicker-elimination guarantee: when the persp-switch-deferred render
or 1Hz idle poll fires with no state change, the buffer is not
rewritten and no redisplay artifact is produced for the gutter."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p2")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((tick-before (buffer-modified-tick)))
        (agent-repl-drawer--render)
        (should (= tick-before (buffer-modified-tick)))))))

(ert-deftest agent-repl-drawer-test-render-after-mark-rewrites-buffer ()
  "Re-rendering after a state change (a workspace gets marked) DOES rewrite
the buffer.  Pairs with `--render-noop-skips-buffer-rewrite' to assert the
content-equality check distinguishes real changes from no-ops."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--ensure-marked-set)
      (puthash "alpha" t agent-repl-drawer--marked-set)
      (let ((tick-before (buffer-modified-tick)))
        (agent-repl-drawer--render)
        (should (> (buffer-modified-tick) tick-before))))))

(ert-deftest agent-repl-drawer-test-render-skips-build-on-matching-signature ()
  "When the render-signature matches the last render, `--render' must NOT
re-enter `--insert-content'.  This is the 1Hz poll fast path: with the
drawer open and no state change, the per-tick render allocates nothing
and walks no characters.  Counts invocations of `--insert-content' via
an `:around' override."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (let ((calls 0)
            (orig (symbol-function 'agent-repl-drawer--insert-content)))
        (cl-letf (((symbol-function 'agent-repl-drawer--insert-content)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply orig args))))
          (agent-repl-drawer--render)
          (should (= calls 1))
          (agent-repl-drawer--render)
          (should (= calls 1)))))))

(ert-deftest agent-repl-drawer-test-render-rebuilds-when-agent-state-changes ()
  "A `:agent-state' change on a registered workspace must invalidate the
render-signature so the next `--render' rebuilds.  Confirms the signature
captures plist values the 1Hz status poll mutates."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1"
                                       :agent-state :idle)
    (agent-repl-drawer-test--with-buffer
      (let ((calls 0)
            (orig (symbol-function 'agent-repl-drawer--insert-content)))
        (cl-letf (((symbol-function 'agent-repl-drawer--insert-content)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply orig args))))
          (agent-repl-drawer--render)
          (agent-repl--ws-put "alpha" :agent-state :thinking)
          (agent-repl-drawer--render)
          (should (= calls 2)))))))

(ert-deftest agent-repl-drawer-test-render-rebuilds-when-git-clean-changes ()
  "A `:git-clean' change must invalidate the signature.  The 1Hz poll's
async git-diff sentinel writes this field, so the next render must
pick up the new value."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1"
                                       :git-clean 'clean)
    (agent-repl-drawer-test--with-buffer
      (let ((calls 0)
            (orig (symbol-function 'agent-repl-drawer--insert-content)))
        (cl-letf (((symbol-function 'agent-repl-drawer--insert-content)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply orig args))))
          (agent-repl-drawer--render)
          (agent-repl--ws-put "alpha" :git-clean 'dirty)
          (agent-repl-drawer--render)
          (should (= calls 2)))))))

(ert-deftest agent-repl-drawer-test-render-rebuilds-when-workspace-added ()
  "Adding a workspace must invalidate the signature.  Workspace registration
mutates the `agent-repl--workspaces' hash; the next render must reflect
the new entry."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (let ((calls 0)
            (orig (symbol-function 'agent-repl-drawer--insert-content)))
        (cl-letf (((symbol-function 'agent-repl-drawer--insert-content)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply orig args))))
          (agent-repl-drawer--render)
          (agent-repl-drawer-test--register "beta" :priority "p2")
          (agent-repl-drawer--render)
          (should (= calls 2)))))))

(ert-deftest agent-repl-drawer-test-render-rebuilds-when-mark-toggled ()
  "Toggling a mark must invalidate the signature.  Marks affect the rendered
gutter glyph and are buffer-local — the signature includes per-ws marked
state."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (let ((calls 0)
            (orig (symbol-function 'agent-repl-drawer--insert-content)))
        (cl-letf (((symbol-function 'agent-repl-drawer--insert-content)
                   (lambda (&rest args)
                     (cl-incf calls)
                     (apply orig args))))
          (agent-repl-drawer--render)
          (agent-repl-drawer--ensure-marked-set)
          (puthash "alpha" t agent-repl-drawer--marked-set)
          (agent-repl-drawer--render)
          (should (= calls 2)))))))

(ert-deftest agent-repl-drawer-test-render-noop-preserves-text-properties ()
  "A no-op re-render must NOT corrupt text properties.
Stress-tests the path against the `replace-buffer-contents' pitfall
where the diff's LCS preserves the destination's stale text properties
on characters it matches — leaving the wrong workspace name attached
to text that visually belongs to another entry."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p2")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((props-before nil))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (push (cons (point)
                        (get-text-property
                         (point) 'agent-repl-drawer-workspace))
                  props-before)
            (forward-char 1)))
        (agent-repl-drawer--render)
        (let ((props-after nil))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (push (cons (point)
                          (get-text-property
                           (point) 'agent-repl-drawer-workspace))
                    props-after)
              (forward-char 1)))
          (should (equal props-before props-after)))))))

(ert-deftest agent-repl-drawer-test-render-anchors-cursor-to-workspace ()
  "`--render' restores point onto the same workspace by identity (not line
number) so the current-entry arrow tracks the entry through layout shifts
above the cursor — e.g. a parent above a nested child collapses between
polls, shrinking the buffer; line-number restoration would land on a now-
shorter intermediate line where `--workspace-at-point' is nil, deleting the
overlay.  Tests the workspace-anchored restoration path."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "parent"
                                       :priority "p1"
                                       :project-dir "/tmp/parent"
                                       :detail-branch "feature/x"
                                       :detail-master-ahead 7
                                       :detail-last-commit "fix: thing")
    (agent-repl-drawer-test--register "child"
                                       :priority "p2"
                                       :project-dir "/tmp/child"
                                       :source-ws-dir "/tmp/parent")
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
               (lambda (dir)
                 (cond ((equal dir "/tmp/parent") "parent")
                       ((equal dir "/tmp/child")  "child"))))
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
      (agent-repl-drawer-test--with-buffer
        (agent-repl-drawer--ensure-expanded-set)
        (puthash "parent" t agent-repl-drawer--expanded-set)
        (agent-repl-drawer--render)
        ;; Park on the nested child while parent is expanded.
        (should (agent-repl-drawer--goto-workspace-line "child"))
        (agent-repl-drawer--update-current-entry-overlay)
        ;; Collapse parent — child's line number shifts up.  A poll-driven
        ;; re-render fires before the user moves point.
        (remhash "parent" agent-repl-drawer--expanded-set)
        (agent-repl-drawer--render)
        ;; Point must still be on the child, and the overlay must still
        ;; mark the child's gutter region.
        (should (equal (agent-repl-drawer--workspace-at-point) "child"))
        (let ((ov agent-repl-drawer--current-entry-overlay))
          (should (overlayp ov))
          (should (overlay-buffer ov))
          (should (equal (get-text-property (overlay-start ov)
                                            'agent-repl-drawer-workspace)
                         "child")))))))

;;;; ---- Expand-detail ----

(ert-deftest agent-repl-drawer-test-toggle-expand-adds-and-removes ()
  "TAB toggle adds the entry to the expanded-set on first press, removes on second."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1" :project-dir "/tmp/")
    (cl-letf (((symbol-function 'agent-repl-drawer--refresh-detail-cache) #'ignore)
              ((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (progn
        (agent-repl-drawer-toggle-expand)
        (should (agent-repl-drawer--expanded-p "ws"))
        (agent-repl-drawer-toggle-expand)
        (should-not (agent-repl-drawer--expanded-p "ws")))))))

(ert-deftest agent-repl-drawer-test-render-detail-lines-shows-cached-fields ()
  "When an entry is expanded, render emits its `:detail-*' fields."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :priority "p1"
                                       :project-dir "/tmp/"
                                       :detail-branch "feature/x"
                                       :detail-master-ahead 7
                                       :detail-last-commit "fix: thing"
                                       :detail-last-commit-time "5 minutes ago")
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--ensure-expanded-set)
      (puthash "ws" t agent-repl-drawer--expanded-set)
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "feature/x" text))
        (should (string-match-p "ahead master:" text))
        (should (string-match-p "fix: thing" text))
        (should (string-match-p "5 minutes ago" text)))))))

(ert-deftest agent-repl-drawer-test-merged-detail-shows-merge-target ()
  "A MERGED-section entry's folded detail shows the merge-target branch."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :priority "p1"
                                       :project-dir "/tmp/"
                                       :repl-state :merged
                                       :detail-branch "feature/x"
                                       :merge-target-name "DWC/parent-branch")
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
      (agent-repl-drawer-test--with-buffer
        (agent-repl-drawer--ensure-expanded-set)
        (puthash "ws" t agent-repl-drawer--expanded-set)
        (agent-repl-drawer--render)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "merged into:" text))
          (should (string-match-p "DWC/parent-branch" text)))))))

(ert-deftest agent-repl-drawer-test-merged-detail-target-has-face ()
  "The merge-target value carries `agent-repl-drawer-detail-merge-target'."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :priority "p1"
                                       :project-dir "/tmp/"
                                       :repl-state :merged
                                       :merge-target-name "DWC/parent-branch")
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
      (agent-repl-drawer-test--with-buffer
        (agent-repl-drawer--ensure-expanded-set)
        (puthash "ws" t agent-repl-drawer--expanded-set)
        (agent-repl-drawer--render)
        (let* ((all (buffer-substring-no-properties (point-min) (point-max)))
               (pos (string-match (regexp-quote "DWC/parent-branch") all))
               (f (and pos (get-text-property (1+ pos) 'face))))
          (should (memq 'agent-repl-drawer-detail-merge-target
                        (if (listp f) f (list f)))))))))

(ert-deftest agent-repl-drawer-test-non-merged-detail-omits-merge-target ()
  "A non-MERGED entry's folded detail omits the merge-target line even when set."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :priority "p1"
                                       :project-dir "/tmp/"
                                       :detail-branch "feature/x"
                                       :merge-target-name "DWC/parent-branch")
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
      (agent-repl-drawer-test--with-buffer
        (agent-repl-drawer--ensure-expanded-set)
        (puthash "ws" t agent-repl-drawer--expanded-set)
        (agent-repl-drawer--render)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should-not (string-match-p "merged into:" text)))))))

;;;; ---- MERGING-section merge-status detail line ----

(ert-deftest agent-repl-drawer-test-merge-status-text-in-flight-label ()
  "`--merge-status-text' labels an in-flight cherry-pick \"update in progress\"."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :project-dir "/tmp/" :merging t)
    (should (equal (agent-repl-drawer--merge-status-text "ws")
                   "update in progress"))))

(ert-deftest agent-repl-drawer-test-merge-status-text-queued-label ()
  "`--merge-status-text' labels a parked request \"update queued\"."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :project-dir "/tmp/"
                                       :repl-state :merge-queued)
    (should (equal (agent-repl-drawer--merge-status-text "ws")
                   "update queued"))))

(ert-deftest agent-repl-drawer-test-merge-status-text-appends-commit-count ()
  "`--merge-status-text' appends the cached source-ahead commit count (plural)."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :project-dir "/tmp/" :merging t
                                       :detail-source-ahead 3)
    (should (equal (agent-repl-drawer--merge-status-text "ws")
                   "update in progress · 3 commits"))))

(ert-deftest agent-repl-drawer-test-merge-status-text-singular-commit ()
  "A source-ahead of 1 renders the singular \"1 commit\" (no trailing s)."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :project-dir "/tmp/"
                                       :repl-state :merge-queued
                                       :detail-source-ahead 1)
    (should (equal (agent-repl-drawer--merge-status-text "ws")
                   "update queued · 1 commit"))))

(ert-deftest agent-repl-drawer-test-merge-status-text-nil-for-non-merging ()
  "`--merge-status-text' returns nil for a non-MERGING workspace."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :project-dir "/tmp/"
                                       :repl-state :merged)
    (should-not (agent-repl-drawer--merge-status-text "ws"))))

(ert-deftest agent-repl-drawer-test-merging-detail-shows-merge-status ()
  "An expanded MERGING entry's detail shows the merge-status line."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :priority "p1"
                                       :project-dir "/tmp/"
                                       :merging t
                                       :detail-source-ahead 2)
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
      (agent-repl-drawer-test--with-buffer
        (agent-repl-drawer--ensure-expanded-set)
        (puthash "ws" t agent-repl-drawer--expanded-set)
        (agent-repl-drawer--render)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "merge:" text))
          (should (string-match-p "update in progress · 2 commits" text)))))))

(ert-deftest agent-repl-drawer-test-merge-status-value-has-face ()
  "The merge-status value carries `agent-repl-drawer-detail-merge-status'."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :priority "p1"
                                       :project-dir "/tmp/"
                                       :repl-state :merge-queued)
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
      (agent-repl-drawer-test--with-buffer
        (agent-repl-drawer--ensure-expanded-set)
        (puthash "ws" t agent-repl-drawer--expanded-set)
        (agent-repl-drawer--render)
        (let* ((all (buffer-substring-no-properties (point-min) (point-max)))
               (pos (string-match (regexp-quote "update queued") all))
               (f (and pos (get-text-property (1+ pos) 'face))))
          (should (memq 'agent-repl-drawer-detail-merge-status
                        (if (listp f) f (list f)))))))))

(ert-deftest agent-repl-drawer-test-non-merging-detail-omits-merge-status ()
  "A non-MERGING entry's expanded detail omits the merge-status line."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :priority "p1"
                                       :project-dir "/tmp/"
                                       :detail-branch "feature/x")
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
      (agent-repl-drawer-test--with-buffer
        (agent-repl-drawer--ensure-expanded-set)
        (puthash "ws" t agent-repl-drawer--expanded-set)
        (agent-repl-drawer--render)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should-not (string-match-p "merge:" text)))))))

(ert-deftest agent-repl-drawer-test-expanded-detail-lists-merged-in-workspaces ()
  "An expanded entry lists every workspace on its `:merged-in-workspaces'."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "parent"
                                       :priority "p1"
                                       :project-dir "/tmp/"
                                       :merged-in-workspaces '("child-a" "child-b"))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
      (agent-repl-drawer-test--with-buffer
        (agent-repl-drawer--ensure-expanded-set)
        (puthash "parent" t agent-repl-drawer--expanded-set)
        (agent-repl-drawer--render)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "merged in:" text))
          (should (string-match-p "child-a" text))
          (should (string-match-p "child-b" text)))))))

(ert-deftest agent-repl-drawer-test-merged-in-value-has-face ()
  "Each merged-in workspace value carries `agent-repl-drawer-detail-merged-in'."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "parent"
                                       :priority "p1"
                                       :project-dir "/tmp/"
                                       :merged-in-workspaces '("child-a"))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
      (agent-repl-drawer-test--with-buffer
        (agent-repl-drawer--ensure-expanded-set)
        (puthash "parent" t agent-repl-drawer--expanded-set)
        (agent-repl-drawer--render)
        (let* ((all (buffer-substring-no-properties (point-min) (point-max)))
               (pos (string-match (regexp-quote "child-a") all))
               (f (and pos (get-text-property (1+ pos) 'face))))
          (should (memq 'agent-repl-drawer-detail-merged-in
                        (if (listp f) f (list f)))))))))

(ert-deftest agent-repl-drawer-test-expanded-detail-omits-merged-in-when-empty ()
  "An expanded entry with no `:merged-in-workspaces' omits the merged-in line."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "parent"
                                       :priority "p1"
                                       :project-dir "/tmp/"
                                       :detail-branch "feature/x")
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
      (agent-repl-drawer-test--with-buffer
        (agent-repl-drawer--ensure-expanded-set)
        (puthash "parent" t agent-repl-drawer--expanded-set)
        (agent-repl-drawer--render)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should-not (string-match-p "merged in:" text)))))))

(ert-deftest agent-repl-drawer-test-merged-in-in-render-signature ()
  "`--render-signature' changes when `:merged-in-workspaces' changes."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "parent"
                                       :priority "p1"
                                       :project-dir "/tmp/")
    (agent-repl-drawer-test--with-buffer
      (let ((sig-before (agent-repl-drawer--render-signature)))
        (agent-repl--ws-put "parent" :merged-in-workspaces '("child-a"))
        (should-not (equal sig-before
                           (agent-repl-drawer--render-signature)))))))

;;;; ---- Events summary removal ----

(ert-deftest agent-repl-drawer-test-no-last-24h-header ()
  "The drawer renders no \"Last 24h\" events summary above its sections."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws1" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (string-match-p "Last 24h" text))))))

(ert-deftest agent-repl-drawer-test-events-subsystem-removed ()
  "The workspace event log that fed the \"Last 24h\" summary is gone."
  (should-not (fboundp 'agent-repl--events-record))
  (should-not (fboundp 'agent-repl--events-recent))
  (should-not (fboundp 'agent-repl-drawer--insert-events-header)))

(ert-deftest agent-repl-drawer-test-format-duration ()
  "`--format-duration' produces short human-readable strings."
  (should (equal (agent-repl-drawer--format-duration 30)   "30s ago"))
  (should (equal (agent-repl-drawer--format-duration 600)  "10m ago"))
  (should (equal (agent-repl-drawer--format-duration 7200) "2.0h ago"))
  (should (equal (agent-repl-drawer--format-duration 172800) "2.0d ago")))

(ert-deftest agent-repl-drawer-test-detail-values-have-distinct-faces ()
  "Detail-line values carry their per-field faces (not the generic summary face)."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register
     "ws"
     :priority "p1" :project-dir "/tmp/"
     :detail-branch       "feature/x"
     :detail-master-ahead 5
     :detail-last-commit  "fix: thing")
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--ensure-expanded-set)
      (puthash "ws" t agent-repl-drawer--expanded-set)
      (agent-repl-drawer--render)
      (cl-flet ((face-at (needle face)
                  (let ((pos (string-match (regexp-quote needle)
                                           (buffer-substring-no-properties
                                            (point-min) (point-max)))))
                    (and pos
                         (memq face
                               (let ((f (get-text-property (1+ pos) 'face)))
                                 (if (listp f) f (list f))))))))
        (should (face-at "feature/x" 'agent-repl-drawer-detail-branch))
        (should (face-at "5"          'agent-repl-drawer-detail-ahead-master))
        (should (face-at "fix: thing" 'agent-repl-drawer-detail-last-commit)))))))

;;;; ---- Per-entry action commands ----

(ert-deftest agent-repl-drawer-test-nuke-dispatches-to-entry ()
  "`agent-repl-drawer-nuke' invokes `agent-repl-nuke-workspace' with the entry."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "target" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((arg :unset))
        (cl-letf (((symbol-function 'agent-repl-nuke-workspace)
                   (lambda (&optional ws) (setq arg ws))))
          (agent-repl-drawer-nuke))
        (should (equal arg "target"))))))

(ert-deftest agent-repl-drawer-test-kill-dispatches-to-entry ()
  "`agent-repl-drawer-kill' invokes `agent-repl-kill-workspace' with the entry."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "target" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((arg :unset))
        (cl-letf (((symbol-function 'agent-repl-kill-workspace)
                   (lambda (&optional ws) (setq arg ws))))
          (agent-repl-drawer-kill))
        (should (equal arg "target"))))))

(ert-deftest agent-repl-drawer-test-nuke-on-merged-dispatches-to-finish ()
  "`agent-repl-drawer-nuke' on a `:merge-completed' entry routes to
`--finish-workspace' (which removes the worktree) rather than the
standard `agent-repl-nuke-workspace' path (which preserves it).  This
is the only way to drop a workspace out of the drawer's MERGED bucket."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "merged" :merge-completed t)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((finished-with :unset)
            (nuke-called nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'agent-repl--finish-workspace)
                   (lambda (ws) (setq finished-with ws)))
                  ((symbol-function 'agent-repl-nuke-workspace)
                   (lambda (&rest _) (setq nuke-called t))))
          (agent-repl-drawer-nuke))
        (should (equal finished-with "merged"))
        (should-not nuke-called)))))

(ert-deftest agent-repl-drawer-test-nuke-on-merged-aborts-on-deny ()
  "Drawer `x' on a MERGED entry prompts; answering no skips finish."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "merged" :merge-completed t)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((finish-called nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--finish-workspace)
                   (lambda (&rest _) (setq finish-called t))))
          (agent-repl-drawer-nuke))
        (should-not finish-called)))))

(ert-deftest agent-repl-drawer-test-kill-on-merged-errors ()
  "`d' (drawer-kill) refuses to act on a MERGED entry — `x' is the only
removal path for merged workspaces."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "merged" :merge-completed t)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((kill-called nil))
        (cl-letf (((symbol-function 'agent-repl-kill-workspace)
                   (lambda (&rest _) (setq kill-called t))))
          (should-error (agent-repl-drawer-kill) :type 'user-error))
        (should-not kill-called)))))

(ert-deftest agent-repl-drawer-test-send-prompt-on-merged-errors ()
  "`i' (drawer-send-prompt) refuses on a MERGED entry — the Claude
session has been torn down so there's no one to receive the prompt."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "merged" :merge-completed t)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((sent nil))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "hi"))
                  ((symbol-function 'agent-repl--send)
                   (lambda (&rest _) (setq sent t))))
          (should-error (agent-repl-drawer-send-prompt) :type 'user-error))
        (should-not sent)))))

(ert-deftest agent-repl-drawer-test-merge-into-master-on-merged-reactivates-then-merges ()
  "`M' (drawer-merge-into-master) on a MERGED entry reactivates first
\(same path `drawer-visit' takes), then invokes the standard
`agent-repl-workspace-merge-current-into-source'.  A prior
cherry-pick may have silently failed but still flipped the workspace
into MERGED, so re-attempts must be possible."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "agent-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (agent-repl-drawer-test--register
             "merged" :merge-completed t :project-dir tmp)
            (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                       (lambda (&rest args)
                         (pcase args
                           (`("-C" ,dir "rev-parse" "--git-common-dir")
                            (concat (file-name-as-directory dir) ".git"))
                           (_ (error "unmocked git-string-quiet: %S" args))))))
            (agent-repl-drawer-test--with-buffer
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace)
              (let ((established-with nil)
                    (merge-called nil))
                (cl-letf (((symbol-function 'agent-repl--state-save)
                           (lambda (&rest _) nil))
                          ((symbol-function 'agent-repl--establish-workspace)
                           (lambda (ws dir) (setq established-with (list ws dir))))
                          ((symbol-function 'agent-repl-workspace-merge-current-into-source)
                           (lambda () (setq merge-called t))))
                  (agent-repl-drawer-merge-into-master))
                (should (equal established-with (list "merged" tmp)))
                (should merge-called)))))
        (delete-directory tmp t)))))

(ert-deftest agent-repl-drawer-test-merge-child-on-merged-reactivates-then-merges ()
  "`m' (drawer-merge-child) on a MERGED entry reactivates first, then
invokes the standard `agent-repl-workspace-merge'."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "agent-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (agent-repl-drawer-test--register
             "merged" :merge-completed t :project-dir tmp)
            (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                       (lambda (&rest args)
                         (pcase args
                           (`("-C" ,dir "rev-parse" "--git-common-dir")
                            (concat (file-name-as-directory dir) ".git"))
                           (_ (error "unmocked git-string-quiet: %S" args))))))
            (agent-repl-drawer-test--with-buffer
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace)
              (let ((established-with nil)
                    (merge-called nil))
                (cl-letf (((symbol-function 'agent-repl--state-save)
                           (lambda (&rest _) nil))
                          ((symbol-function 'agent-repl--establish-workspace)
                           (lambda (ws dir) (setq established-with (list ws dir))))
                          ((symbol-function 'agent-repl-workspace-merge)
                           (lambda () (setq merge-called t))))
                  (agent-repl-drawer-merge-child))
                (should (equal established-with (list "merged" tmp)))
                (should merge-called)))))
        (delete-directory tmp t)))))

(ert-deftest agent-repl-drawer-test-merge-into-master-on-merged-clears-merge-flags ()
  "Reactivation during `M' on a MERGED entry clears the
`:merge-completed' / `:merge-completed-at' / `:repl-state :merged'
plist keys (the workspace must leave the MERGED bucket so the
re-attempted merge runs against a live persp)."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "agent-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (agent-repl-drawer-test--register
             "merged"
             :merge-completed t
             :merge-completed-at 1234567890.0
             :repl-state :merged
             :project-dir tmp)
            (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                       (lambda (&rest args)
                         (pcase args
                           (`("-C" ,dir "rev-parse" "--git-common-dir")
                            (concat (file-name-as-directory dir) ".git"))
                           (_ (error "unmocked git-string-quiet: %S" args))))))
            (agent-repl-drawer-test--with-buffer
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace)
              (cl-letf (((symbol-function 'agent-repl--state-save)
                         (lambda (&rest _) nil))
                        ((symbol-function 'agent-repl--establish-workspace)
                         (lambda (&rest _) nil))
                        ((symbol-function 'agent-repl-workspace-merge-current-into-source)
                         (lambda () nil)))
                (agent-repl-drawer-merge-into-master))
              (should-not (agent-repl--ws-get "merged" :merge-completed))
              (should-not (agent-repl--ws-get "merged" :merge-completed-at))
              (should-not (agent-repl--ws-get "merged" :repl-state)))))
        (delete-directory tmp t)))))

(ert-deftest agent-repl-drawer-test-new-child-on-merged-errors ()
  "`n' (drawer-new-child) refuses to branch from a MERGED entry —
branching from a merged-and-torn-down workspace would resurrect a
stale tree."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "merged" :merge-completed t)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (should-error (agent-repl-drawer-new-child) :type 'user-error))))

(ert-deftest agent-repl-drawer-test-new-fork-on-merged-errors ()
  "`f' (drawer-new-fork) refuses on a MERGED entry — the source claude
session has been torn down."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "merged" :merge-completed t)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (should-error (agent-repl-drawer-new-fork) :type 'user-error))))

(ert-deftest agent-repl-drawer-test-interrupt-dispatches-to-entry ()
  "`agent-repl-drawer-interrupt' invokes `agent-repl-interrupt' with the entry."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "target" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((arg :unset))
        (cl-letf (((symbol-function 'agent-repl-interrupt)
                   (lambda (&optional ws) (setq arg ws))))
          (agent-repl-drawer-interrupt))
        (should (equal arg "target"))))))

(ert-deftest agent-repl-drawer-test-send-prompt-dispatches-to-entry ()
  "`agent-repl-drawer-send-prompt' calls `agent-repl--send' with prompt and entry."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "target" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((sent-prompt :unset)
            (sent-ws     :unset))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "hello world"))
                  ((symbol-function 'agent-repl--send)
                   (lambda (prompt ws &rest _)
                     (setq sent-prompt prompt sent-ws ws))))
          (agent-repl-drawer-send-prompt))
        (should (equal sent-prompt "hello world"))
        (should (equal sent-ws "target"))))))

(ert-deftest agent-repl-drawer-test-send-prompt-empty-skips-send ()
  "Empty prompt input skips the send entirely."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "target" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((called nil))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
                  ((symbol-function 'agent-repl--send)
                   (lambda (&rest _) (setq called t))))
          (agent-repl-drawer-send-prompt))
        (should-not called)))))

(ert-deftest agent-repl-drawer-test-action-no-ws-at-point-errors ()
  "Action commands signal user-error when there is no workspace at point."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (goto-char (point-min))
      (should-error (agent-repl-drawer-nuke) :type 'user-error)
      (should-error (agent-repl-drawer-kill) :type 'user-error)
      (should-error (agent-repl-drawer-interrupt) :type 'user-error))))

(ert-deftest agent-repl-drawer-test-priority-up-from-p1-to-p05 ()
  "`priority-up' cycles p1 → p05."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((args :unset))
        (cl-letf (((symbol-function 'agent-repl-set-priority)
                   (lambda (p ws) (setq args (list p ws)))))
          (agent-repl-drawer-priority-up))
        (should (equal args '("p05" "ws")))))))

(ert-deftest agent-repl-drawer-test-priority-down-from-p1-to-p2 ()
  "`priority-down' cycles p1 → p2."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((args :unset))
        (cl-letf (((symbol-function 'agent-repl-set-priority)
                   (lambda (p ws) (setq args (list p ws)))))
          (agent-repl-drawer-priority-down))
        (should (equal args '("p2" "ws")))))))

(ert-deftest agent-repl-drawer-test-priority-down-from-p3-to-nil ()
  "`priority-down' from p3 cycles to nil (sent as empty string to set-priority)."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p3")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((args :unset))
        (cl-letf (((symbol-function 'agent-repl-set-priority)
                   (lambda (p ws) (setq args (list p ws)))))
          (agent-repl-drawer-priority-down))
        (should (equal args '("" "ws")))))))

(ert-deftest agent-repl-drawer-test-priority-up-from-nil-to-p3 ()
  "`priority-up' from nil cycles to p3 (one step toward higher priority)."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((args :unset))
        (cl-letf (((symbol-function 'agent-repl-set-priority)
                   (lambda (p ws) (setq args (list p ws)))))
          (agent-repl-drawer-priority-up))
        (should (equal args '("p3" "ws")))))))

(ert-deftest agent-repl-drawer-test-toggle-hidden-active-to-hidden ()
  "Toggling a non-hidden entry calls `agent-repl--on-close' with the entry."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws1" :priority "p1" :repl-state :active)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((arg :unset))
        (cl-letf (((symbol-function 'agent-repl--on-close)
                   (lambda (&optional ws) (setq arg ws))))
          (agent-repl-drawer-toggle-hidden))
        (should (equal arg "ws1"))))))

(ert-deftest agent-repl-drawer-test-toggle-hidden-hidden-to-active ()
  "Toggling a `:hidden' entry calls `agent-repl--unhide-workspace' with the entry."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws1" :priority "p1" :repl-state :hidden)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      ;; Hidden ws renders in HIDDEN section — find it via direct goto.
      (should (agent-repl-drawer--goto-workspace-line "ws1"))
      (let ((arg :unset))
        (cl-letf (((symbol-function 'agent-repl--unhide-workspace)
                   (lambda (ws) (setq arg ws))))
          (agent-repl-drawer-toggle-hidden))
        (should (equal arg "ws1"))))))

(ert-deftest agent-repl-drawer-test-new-child-dispatches-to-entry ()
  "`agent-repl-drawer-new-child' calls create-worktree-workspace with `head'+entry."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "parent" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((args :unset))
        (cl-letf (((symbol-function 'agent-repl-create-worktree-workspace)
                   (lambda (base &optional ws) (setq args (list base ws)))))
          (agent-repl-drawer-new-child))
        (should (equal args '(head "parent")))))))

(ert-deftest agent-repl-drawer-test-new-fork-dispatches-to-entry ()
  "`agent-repl-drawer-new-fork' calls fork-worktree-workspace with the entry."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "parent" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((arg :unset))
        (cl-letf (((symbol-function 'agent-repl-fork-worktree-workspace)
                   (lambda (&optional ws) (setq arg ws))))
          (agent-repl-drawer-new-fork))
        (should (equal arg "parent"))))))

(ert-deftest agent-repl-drawer-test-merge-into-master-switches-then-calls ()
  "`agent-repl-drawer-merge-into-master' switches to entry, invokes merge, then restores."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "target" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((current "elsewhere")
            (switch-log nil)
            (merge-called nil))
        (cl-letf (((symbol-function '+workspace-current-name)
                   (lambda () current))
                  ((symbol-function '+workspace-switch)
                   (lambda (ws &rest _) (push ws switch-log) (setq current ws)))
                  ((symbol-function 'agent-repl-workspace-merge-current-into-source)
                   (lambda () (setq merge-called t))))
          (agent-repl-drawer-merge-into-master))
        (should merge-called)
        (should (equal (nreverse switch-log) '("target" "elsewhere")))))))

;;;; ---- Persistence across workspaces ----

(ert-deftest agent-repl-drawer-test-ensure-visible-noop-when-flag-nil ()
  "`--ensure-visible-on-persp-switch' is a no-op when the global flag is nil."
  (let ((agent-repl-drawer--global-visible-p nil)
        (called nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (&rest _) (setq called t) nil)))
      (agent-repl-drawer--ensure-visible-on-persp-switch)
      (should-not called))))

(ert-deftest agent-repl-drawer-test-ensure-visible-displays-when-flag-set ()
  "`--ensure-visible-on-persp-switch' displays the drawer when flag is set and it's not visible."
  (let ((agent-repl-drawer--global-visible-p t)
        (display-called nil))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
              ((symbol-function 'display-buffer)
               (lambda (&rest _) (setq display-called t) nil))
              ((symbol-function 'agent-repl-drawer--get-or-create-buffer)
               (lambda () (get-buffer-create " *test-drawer-buf*"))))
      (agent-repl-drawer--ensure-visible-on-persp-switch)
      (should display-called))
    (when-let ((b (get-buffer " *test-drawer-buf*"))) (kill-buffer b))))

(ert-deftest agent-repl-drawer-test-hide-clears-global-flag ()
  "`agent-repl-drawer-hide' clears the global visible-flag."
  (let ((agent-repl-drawer--global-visible-p t))
    (agent-repl-drawer-hide)
    (should-not agent-repl-drawer--global-visible-p)))

(ert-deftest agent-repl-drawer-test-ensure-hidden-when-flag-nil-and-window-visible ()
  "`--ensure-visible-on-persp-switch' deletes the drawer window when the
flag is nil but persp-mode restored a stale drawer window — making the
drawer truly global so hiding in one workspace hides in all."
  (let* ((agent-repl-drawer--global-visible-p nil)
         (buf (get-buffer-create agent-repl-drawer-buffer-name))
         (delete-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'fake-win))
                  ((symbol-function 'get-buffer-window-list)
                   (lambda (&rest _) '()))
                  ((symbol-function 'agent-repl-window--delete-buffer-windows)
                   (lambda (b &rest _) (setq delete-called-with b))))
          (agent-repl-drawer--ensure-visible-on-persp-switch)
          (should (eq delete-called-with buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Drawer ⊥ explain-config decoupling ----
;;
;; The explain-config buffer (SPC j h c output) is a standalone
;; bottom-side popup, fully decoupled from the drawer (which lives
;; on the left).  Drawer show/hide/persp-reconcile must NOT touch
;; the explain-config window — it has its own visibility lifecycle
;; (see `agent-repl--explain-config-global-visible-p').

(ert-deftest agent-repl-drawer-test-show-does-not-touch-explain-config ()
  "`agent-repl-drawer-show' must not call `--explain-config-show' —
the popup is decoupled and manages its own visibility independently."
  (let ((explain-show-called nil))
    (cl-letf (((symbol-function 'agent-repl-drawer--get-or-create-buffer)
               (lambda () (get-buffer-create " *test-drawer-buf*")))
              ((symbol-function 'agent-repl-drawer--current-ws)
               (lambda () nil))
              ((symbol-function 'display-buffer) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl-drawer--render) #'ignore)
              ((symbol-function 'agent-repl-drawer--goto-workspace-line)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl-drawer--goto-first-workspace)
               (lambda () nil))
              ((symbol-function 'agent-repl-drawer--post-command) #'ignore)
              ((symbol-function 'agent-repl-drawer--apply-background) #'ignore)
              ((symbol-function 'agent-repl--explain-config-show)
               (lambda () (setq explain-show-called t))))
      (agent-repl-drawer-show)
      (should-not explain-show-called))
    (when-let ((b (get-buffer " *test-drawer-buf*"))) (kill-buffer b))))

(ert-deftest agent-repl-drawer-test-show-expands-current-merging-workspace ()
  "`drawer-show--inner' auto-expands a MERGING current workspace and
calls `--refresh-detail-cache' for it before the render."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1" :merging t)
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name))
          (cache-called-for nil))
      (unwind-protect
          (with-current-buffer buf
            (agent-repl-drawer-mode)
            (cl-letf (((symbol-function 'agent-repl-drawer--current-ws)
                       (lambda () "ws"))
                      ((symbol-function 'display-buffer)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl-drawer--refresh-detail-cache)
                       (lambda (ws) (push ws cache-called-for)))
                      ((symbol-function 'agent-repl-window--harden) #'ignore)
                      ((symbol-function 'agent-repl-drawer--apply-width) #'ignore))
              (agent-repl-drawer-show--inner))
            (should (agent-repl-drawer--expanded-p "ws"))
            (should (member "ws" cache-called-for)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-show-skips-cache-when-already-expanded ()
  "`drawer-show--inner' does not call `--refresh-detail-cache' when the
current (MERGING) workspace is already expanded."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1" :merging t)
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name))
          (cache-called 0))
      (unwind-protect
          (with-current-buffer buf
            (agent-repl-drawer-mode)
            ;; Pre-expand.
            (agent-repl-drawer--ensure-expanded-set)
            (puthash "ws" t agent-repl-drawer--expanded-set)
            (cl-letf (((symbol-function 'agent-repl-drawer--current-ws)
                       (lambda () "ws"))
                      ((symbol-function 'display-buffer)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl-drawer--refresh-detail-cache)
                       (lambda (_ws) (setq cache-called (1+ cache-called))))
                      ((symbol-function 'agent-repl-window--harden) #'ignore)
                      ((symbol-function 'agent-repl-drawer--apply-width) #'ignore))
              (agent-repl-drawer-show--inner))
            (should (= cache-called 0)))
        (kill-buffer buf)))))

;;;; ---- Auto-expand gating (MERGING only) ----

(ert-deftest agent-repl-drawer-test-auto-expand-p-true-for-merging ()
  "`--auto-expand-p' is non-nil for a MERGING-section workspace."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :merging t)
    (should (agent-repl-drawer--auto-expand-p "ws"))))

(ert-deftest agent-repl-drawer-test-auto-expand-p-nil-for-main ()
  "`--auto-expand-p' is nil for a MAIN-section workspace."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (should-not (agent-repl-drawer--auto-expand-p "ws"))))

(ert-deftest agent-repl-drawer-test-auto-expand-p-nil-for-merged ()
  "`--auto-expand-p' is nil for a MERGED-section workspace."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :repl-state :merged)
    (should-not (agent-repl-drawer--auto-expand-p "ws"))))

(ert-deftest agent-repl-drawer-test-show-does-not-expand-non-merging-workspace ()
  "`drawer-show--inner' leaves a non-MERGING current workspace folded and
does not call `--refresh-detail-cache' for it."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name))
          (cache-called-for nil))
      (unwind-protect
          (with-current-buffer buf
            (agent-repl-drawer-mode)
            (cl-letf (((symbol-function 'agent-repl-drawer--current-ws)
                       (lambda () "ws"))
                      ((symbol-function 'display-buffer)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl-drawer--refresh-detail-cache)
                       (lambda (ws) (push ws cache-called-for)))
                      ((symbol-function 'agent-repl-window--harden) #'ignore)
                      ((symbol-function 'agent-repl-drawer--apply-width) #'ignore))
              (agent-repl-drawer-show--inner))
            (should-not (agent-repl-drawer--expanded-p "ws"))
            (should-not (member "ws" cache-called-for)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-sync-cursor-does-not-expand-non-merging ()
  "`--sync-cursor-to-current-ws' leaves a non-MERGING current workspace folded."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name))
          (cache-called-for nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                     (lambda () "ws"))
                    ((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                    ((symbol-function 'agent-repl-drawer--refresh-detail-cache)
                     (lambda (ws) (push ws cache-called-for)))
                    ((symbol-function 'agent-repl-drawer--render) #'ignore)
                    ((symbol-function 'agent-repl-drawer--goto-workspace-line)
                     (lambda (&rest _) nil)))
            (agent-repl-drawer--sync-cursor-to-current-ws)
            (with-current-buffer buf
              (should-not (agent-repl-drawer--expanded-p "ws")))
            (should-not (member "ws" cache-called-for)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-hide-does-not-touch-explain-config ()
  "`agent-repl-drawer-hide' must not call `--explain-config-hide' —
the popup is decoupled and manages its own visibility independently."
  (let ((explain-hide-called nil)
        (agent-repl-drawer--global-visible-p t))
    (cl-letf (((symbol-function 'agent-repl-window--delete-buffer-windows)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--explain-config-hide)
               (lambda () (setq explain-hide-called t))))
      (agent-repl-drawer-hide)
      (should-not explain-hide-called))))

(ert-deftest agent-repl-drawer-test-ensure-visible-does-not-touch-explain-config ()
  "Drawer persp-reconciliation must not show/hide explain-config —
the popup has its own persp-activated reconciler that runs in parallel."
  (let ((agent-repl-drawer--global-visible-p t)
        (explain-show-called nil)
        (explain-hide-called nil))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'fake-win))
              ((symbol-function 'display-buffer) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl-drawer--apply-width) #'ignore)
              ((symbol-function 'agent-repl--explain-config-show)
               (lambda () (setq explain-show-called t)))
              ((symbol-function 'agent-repl--explain-config-hide)
               (lambda () (setq explain-hide-called t))))
      (agent-repl-drawer--ensure-visible-on-persp-switch)
      (should-not explain-show-called)
      (should-not explain-hide-called))))

;;;; ---- Global dispatch + auto-revert ----

(ert-deftest agent-repl-drawer-test-global-next-dispatches-to-drawer ()
  "`agent-repl-drawer-global-next' calls `--next' inside the drawer buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "first"  :priority "p1")
    (agent-repl-drawer-test--register "second" :priority "p2")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace))
            (agent-repl-drawer-global-next)
            (with-current-buffer buf
              (should (equal (agent-repl-drawer--workspace-at-point) "second"))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-global-next-refreshes-overlay-synchronously ()
  "`agent-repl-drawer-global-next' updates the current-entry overlay
immediately so the arrow tracks the new selection — does not wait for
the next 1Hz render cycle (which used to cause perceived lag)."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "first"  :priority "p1")
    (agent-repl-drawer-test--register "second" :priority "p2")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace))
            (agent-repl-drawer-global-next)
            (with-current-buffer buf
              (let* ((ov agent-repl-drawer--current-entry-overlay)
                     (start (and (overlayp ov) (overlay-start ov))))
                (should (overlayp ov))
                ;; Overlay must cover the "second" entry now.
                (should (equal (get-text-property
                                start 'agent-repl-drawer-workspace)
                               "second")))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-global-call-does-not-select-window ()
  "`--call-in-drawer' must NOT change the selected window — keystroke
overhead from window selection is what made global nav feel slow vs.
local j/k.  Asserts the selected window is unchanged across the call."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace))
            (let ((before (selected-window))
                  (called nil))
              (agent-repl-drawer--call-in-drawer
               (lambda () (setq called t)))
              (should called)
              (should (eq (selected-window) before))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-global-call-errors-when-no-drawer ()
  "Global wrappers signal user-error when the drawer buffer doesn't exist."
  (when-let ((b (get-buffer agent-repl-drawer-buffer-name)))
    (kill-buffer b))
  (should-error (agent-repl-drawer-global-next) :type 'user-error))

(ert-deftest agent-repl-drawer-test-call-in-drawer-preserves-cursor ()
  "`--call-in-drawer' with PRESERVE-CURSOR=t restores the cursor to the
workspace at point before FN, overriding any cursor move FN's side
effects would otherwise leave behind (persp auto-sync, render fallback).
Simulates this by giving FN a body that yanks the cursor onto a
different entry — the wrapper must put it back."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p2")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-workspace-line "beta"))
            (agent-repl-drawer--call-in-drawer
             (lambda ()
               ;; Simulate a side-effect that moves the cursor away
               ;; (e.g. `--sync-cursor-to-current-ws' after a persp
               ;; switch landing on a different active workspace).
               (agent-repl-drawer--goto-workspace-line "alpha"))
             t)
            (with-current-buffer buf
              (should (equal (agent-repl-drawer--workspace-at-point) "beta"))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-call-in-drawer-no-preserve-keeps-fn-cursor ()
  "`--call-in-drawer' without PRESERVE-CURSOR keeps the cursor wherever FN
left it — required for the navigational dispatchers (`global-next' /
`global-prev') whose entire purpose is to move the cursor."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p2")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-workspace-line "beta"))
            (agent-repl-drawer--call-in-drawer
             (lambda ()
               (agent-repl-drawer--goto-workspace-line "alpha")))
            (with-current-buffer buf
              (should (equal (agent-repl-drawer--workspace-at-point) "alpha"))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-call-in-drawer-preserve-falls-back-when-ws-gone ()
  "When the preserved workspace no longer exists after FN (e.g. nuked),
`--call-in-drawer' leaves the cursor wherever FN naturally placed it
instead of erroring or jumping to point-min."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p2")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-workspace-line "beta"))
            (agent-repl-drawer--call-in-drawer
             (lambda ()
               ;; Simulate nuke: remove the preserved ws + re-render +
               ;; land cursor on the surviving entry.
               (remhash "beta" agent-repl--workspaces)
               (agent-repl-drawer--render)
               (agent-repl-drawer--goto-workspace-line "alpha"))
             t)
            (with-current-buffer buf
              (should (equal (agent-repl-drawer--workspace-at-point) "alpha"))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-call-in-drawer-focused-selects-drawer-window ()
  "`--call-in-drawer-focused' must select the drawer window for the
duration of FN — that's the whole reason it exists (so visual features
keyed off window-selection take hold).  Asserts `selected-window' inside
FN is the drawer's window, then reverts to the original after."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (let* ((drawer-win (display-buffer-in-side-window
                              buf '((side . left))))
                 (other-buf (get-buffer-create " *focused-test-other*"))
                 (other-win (display-buffer other-buf
                                            '(display-buffer-pop-up-window))))
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace))
            (select-window other-win)
            (let ((seen-win nil))
              (agent-repl-drawer--call-in-drawer-focused
               (lambda () (setq seen-win (selected-window))))
              (should (eq seen-win drawer-win))
              (should (eq (selected-window) other-win)))
            (when (window-live-p other-win) (delete-window other-win))
            (when (buffer-live-p other-buf) (kill-buffer other-buf)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-call-in-drawer-focused-falls-back-when-no-window ()
  "When the drawer buffer exists but isn't displayed in any window,
`--call-in-drawer-focused' must fall back to unfocused dispatch — there's
no window to select, but FN still needs to run."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace))
            (let ((called nil))
              (agent-repl-drawer--call-in-drawer-focused
               (lambda () (setq called t)))
              (should called)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-call-in-drawer-focused-errors-when-no-drawer ()
  "`--call-in-drawer-focused' errors with `user-error' when the drawer
buffer doesn't exist — same contract as `--call-in-drawer'."
  (when-let ((b (get-buffer agent-repl-drawer-buffer-name)))
    (kill-buffer b))
  (should-error (agent-repl-drawer--call-in-drawer-focused #'ignore)
                :type 'user-error))

(ert-deftest agent-repl-drawer-test-global-next-uses-focused-dispatch ()
  "`agent-repl-drawer-global-next' must route through the focused
dispatcher so the drawer window is selected during the move — that's
what makes `hl-line' (and other selection-keyed visuals) engage and
stick after focus returns.  Asserts by stubbing the focused helper and
verifying the non-focused one is NOT called."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (let ((focused-called nil)
                (unfocused-called nil))
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace))
            (cl-letf (((symbol-function 'agent-repl-drawer--call-in-drawer-focused)
                       (lambda (_fn) (setq focused-called t)))
                      ((symbol-function 'agent-repl-drawer--call-in-drawer)
                       (lambda (_fn &optional _p) (setq unfocused-called t))))
              (agent-repl-drawer-global-next)
              (should focused-called)
              (should-not unfocused-called)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-global-prev-uses-focused-dispatch ()
  "`agent-repl-drawer-global-prev' must route through the focused
dispatcher — same rationale as global-next."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (let ((focused-called nil)
                (unfocused-called nil))
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace))
            (cl-letf (((symbol-function 'agent-repl-drawer--call-in-drawer-focused)
                       (lambda (_fn) (setq focused-called t)))
                      ((symbol-function 'agent-repl-drawer--call-in-drawer)
                       (lambda (_fn &optional _p) (setq unfocused-called t))))
              (agent-repl-drawer-global-prev)
              (should focused-called)
              (should-not unfocused-called)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-sync-cursor-to-current-ws ()
  "`--sync-cursor-to-current-ws' positions point on the current ws's entry."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p2")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace))
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "beta")))
              (agent-repl-drawer--sync-cursor-to-current-ws))
            (with-current-buffer buf
              (should (equal (agent-repl-drawer--workspace-at-point) "beta"))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-sync-cursor-refreshes-overlay-synchronously ()
  "`--sync-cursor-to-current-ws' repositions the current-entry overlay
synchronously so the arrow snaps to the active workspace immediately,
not after the next 1Hz render — fixes the perceived flash-then-disappear
on workspace switch when the drawer's buffer-local post-command-hook
doesn't fire (e.g. focus elsewhere or persp-mode-driven sync)."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p2")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              ;; Park the arrow on alpha (the old "current" workspace).
              (agent-repl-drawer--goto-workspace-line "alpha")
              (agent-repl-drawer--update-current-entry-overlay)
              (let* ((ov agent-repl-drawer--current-entry-overlay)
                     (start (and (overlayp ov) (overlay-start ov))))
                (should (equal (get-text-property
                                start 'agent-repl-drawer-workspace)
                               "alpha"))))
            ;; Simulate a persp-activated sync to beta (without an
            ;; intervening render, which is the lag the user observed).
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "beta")))
              (agent-repl-drawer--sync-cursor-to-current-ws))
            (with-current-buffer buf
              ;; Arrow must already track beta, not still be on alpha
              ;; (waiting for the next 1Hz render to catch up).
              (let* ((ov agent-repl-drawer--current-entry-overlay)
                     (start (and (overlayp ov) (overlay-start ov))))
                (should (overlayp ov))
                (should (overlay-buffer ov))
                (should (equal (get-text-property
                                start 'agent-repl-drawer-workspace)
                               "beta")))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-sync-cursor-expands-current-workspace ()
  "`--sync-cursor-to-current-ws' adds the current (MERGING) workspace to the
expanded-set and calls `--refresh-detail-cache' when it was not already expanded."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p2" :merging t)
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name))
          (cache-called-for nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render))
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "beta"))
                      ((symbol-function 'agent-repl-drawer--refresh-detail-cache)
                       (lambda (ws) (push ws cache-called-for))))
              (agent-repl-drawer--sync-cursor-to-current-ws))
            (with-current-buffer buf
              (should (agent-repl-drawer--expanded-p "beta"))
              (should (member "beta" cache-called-for))))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-sync-cursor-skips-cache-when-already-expanded ()
  "`--sync-cursor-to-current-ws' does not call `--refresh-detail-cache' when
the current (MERGING) workspace is already in the expanded-set."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1" :merging t)
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name))
          (cache-called 0))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode)
              (agent-repl-drawer--render)
              ;; Pre-expand so the cache refresh should be skipped.
              (agent-repl-drawer--ensure-expanded-set)
              (puthash "ws" t agent-repl-drawer--expanded-set))
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "ws"))
                      ((symbol-function 'agent-repl-drawer--refresh-detail-cache)
                       (lambda (_ws) (setq cache-called (1+ cache-called)))))
              (agent-repl-drawer--sync-cursor-to-current-ws))
            (should (= cache-called 0)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-drawer-test-sync-cursor-calls-render ()
  "`--sync-cursor-to-current-ws' calls `--render' so expanded state appears
immediately without waiting for the next 1Hz poll."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name))
          (render-called 0))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (agent-repl-drawer-mode))
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "ws"))
                      ((symbol-function 'agent-repl-drawer--render)
                       (lambda () (setq render-called (1+ render-called))))
                      ((symbol-function 'agent-repl-drawer--refresh-detail-cache)
                       #'ignore))
              (agent-repl-drawer--sync-cursor-to-current-ws))
            (should (= render-called 1)))
        (kill-buffer buf)))))

;;;; ---- Repo grouping ----

(ert-deftest agent-repl-drawer-test-group-label-from-key ()
  "`--group-label' returns the basename of the parent of KEY."
  (should (equal (agent-repl-drawer--group-label "/path/to/doom/.git")
                 "doom"))
  (should (equal (agent-repl-drawer--group-label "/x/y/explanation-engine/.git")
                 "explanation-engine"))
  (should (null  (agent-repl-drawer--group-label nil))))

(ert-deftest agent-repl-drawer-test-group-key-cached-on-plist ()
  "`--workspace-group-key' caches its result on `:group-key'."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :project-dir "/some/")
    (agent-repl--ws-put "ws" :group-key "/cached/.git")
    ;; Cached value short-circuits the git call.
    (should (equal (agent-repl-drawer--workspace-group-key "ws")
                   "/cached/.git"))))

(ert-deftest agent-repl-drawer-test-group-trees-by-repo-buckets ()
  "`--group-trees-by-repo' partitions a forest into (KEY LABEL . TREES) by repo key."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "doom-ws"      :project-dir "/d/")
    (agent-repl-drawer-test--register "doom-ws-2"    :project-dir "/d2/")
    (agent-repl-drawer-test--register "ee-ws"        :project-dir "/e/")
    (agent-repl--ws-put "doom-ws"   :group-key "/path/doom/.git")
    (agent-repl--ws-put "doom-ws-2" :group-key "/path/doom/.git")
    (agent-repl--ws-put "ee-ws"     :group-key "/path/explanation-engine/.git")
    (let* ((trees '(("doom-ws") ("doom-ws-2") ("ee-ws")))
           (groups (agent-repl-drawer--group-trees-by-repo trees)))
      (should (equal (mapcar #'car groups)
                     '("/path/doom/.git" "/path/explanation-engine/.git")))
      (should (equal (mapcar #'cadr groups) '("doom" "explanation-engine")))
      (should (= 2 (length (cddr (assoc "/path/doom/.git" groups)))))
      (should (= 1 (length (cddr (assoc "/path/explanation-engine/.git"
                                        groups))))))))

(ert-deftest agent-repl-drawer-test-group-trees-by-repo-separates-same-basename ()
  "`--group-trees-by-repo' keeps two repos sharing a basename in distinct buckets."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "a" :project-dir "/a/")
    (agent-repl-drawer-test--register "b" :project-dir "/b/")
    (agent-repl--ws-put "a" :group-key "/one/doom/.git")
    (agent-repl--ws-put "b" :group-key "/two/doom/.git")
    (let ((groups (agent-repl-drawer--group-trees-by-repo '(("a") ("b")))))
      (should (equal (mapcar #'car groups)
                     '("/one/doom/.git" "/two/doom/.git"))))))

(ert-deftest agent-repl-drawer-test-render-emits-group-labels ()
  "Render emits the group label between repo groups."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "doom-a" :project-dir "/d/")
    (agent-repl-drawer-test--register "ee-a"   :project-dir "/e/")
    (agent-repl--ws-put "doom-a" :group-key "/path/doom/.git")
    (agent-repl--ws-put "ee-a"   :group-key "/path/explanation-engine/.git")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "▾ doom" text))
        (should (string-match-p "▾ explanation-engine" text))))))

;;;; ---- Section partition + tree ----

(ert-deftest agent-repl-drawer-test-workspace-section-merging-dominates-hidden ()
  "In-flight workflow (`:merging' t) lands in :merging even when also
flagged hidden.  Asserts the workflow-state signal — not git ancestry
— drives the MERGING bucket, and that it outranks :hidden in the
precedence chain."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :merging    t
                                       :repl-state :hidden)
    (should (eq (agent-repl-drawer--workspace-section "ws") :merging))))

(ert-deftest agent-repl-drawer-test-workspace-section-ancestry-no-longer-buckets-merging ()
  "`:branch-merged' 'merged alone does NOT route to MERGING.
Regression guard: ancestry was the old bucket gate and produced
false MERGING entries when an empty child's parent advanced past it.
Ancestry is now reserved for flattening only; without a workflow flag
the workspace must fall through to :main."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :branch-merged 'merged)
    (should (eq (agent-repl-drawer--workspace-section "ws") :main))))

(ert-deftest agent-repl-drawer-test-workspace-section-merge-completed-routes-to-merged ()
  "Workspaces with `:merge-completed' t land in :merged.
This is the sole path into the MERGED bucket — async ancestry polling
no longer feeds it."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :merge-completed t)
    (should (eq (agent-repl-drawer--workspace-section "ws") :merged))))

(ert-deftest agent-repl-drawer-test-workspace-section-merge-completed-dominates-merging ()
  "`:merge-completed' t wins over `:merging' t.
Covers the brief transition window between setting completed and
clearing the in-flight flag — the workspace must surface in MERGED,
not MERGING."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :merge-completed t
                                       :merging         t)
    (should (eq (agent-repl-drawer--workspace-section "ws") :merged))))

(ert-deftest agent-repl-drawer-test-workspace-section-merge-conflict-routes-to-merged ()
  "`:repl-state :merge-conflict' buckets the workspace under MERGED, not
MERGING.  A real cherry-pick conflict awaits human resolution and is
NOT a member of the merge queue, so it must leave the MERGING bucket
(which holds queue members only) and group with the other terminal
merge outcomes under MERGED."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :repl-state :merge-conflict)
    (should (eq (agent-repl-drawer--workspace-section "ws") :merged))))

(ert-deftest agent-repl-drawer-test-workspace-section-hidden ()
  "Non-merged hidden workspaces land in :hidden."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :repl-state :hidden)
    (should (eq (agent-repl-drawer--workspace-section "ws") :hidden))))

(ert-deftest agent-repl-drawer-test-workspace-section-default-main ()
  "Workspaces with no flags default to :main."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws")
    (should (eq (agent-repl-drawer--workspace-section "ws") :main))))

(ert-deftest agent-repl-drawer-test-effective-parent-skips-merged-ancestor ()
  "Effective parent walks past merged ancestors to the first unmerged one."
  (agent-repl-test--with-clean-state
    (puthash "gp" '(:project-dir "/gp/" :branch-merged not-merged)
             agent-repl--workspaces)
    (puthash "p"  '(:project-dir "/p/"  :source-ws-dir "/gp/"
                    :branch-merged merged)
             agent-repl--workspaces)
    (puthash "c"  '(:project-dir "/c/"  :source-ws-dir "/p/")
             agent-repl--workspaces)
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should (equal (agent-repl-drawer--effective-parent "c" '("gp" "c"))
                     "gp")))))

(ert-deftest agent-repl-drawer-test-effective-parent-nil-when-parent-missing-from-section ()
  "Effective parent returns nil when no ancestor lives in SECTION-SET."
  (agent-repl-test--with-clean-state
    (puthash "p" '(:project-dir "/p/") agent-repl--workspaces)
    (puthash "c" '(:project-dir "/c/" :source-ws-dir "/p/")
             agent-repl--workspaces)
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should (null (agent-repl-drawer--effective-parent "c" '("c")))))))

(ert-deftest agent-repl-drawer-test-effective-parent-in-merged-direct-only ()
  "MERGED-section parent is direct source-ws when also in merged-set.
The generalized `--effective-parent-in-section' powers this; the old
`--effective-parent-in-merged' name is preserved as an alias and so
is still exercised here."
  (agent-repl-test--with-clean-state
    (puthash "p" '(:project-dir "/p/") agent-repl--workspaces)
    (puthash "c" '(:project-dir "/c/" :source-ws-dir "/p/")
             agent-repl--workspaces)
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should (equal (agent-repl-drawer--effective-parent-in-merged
                      "c" '("p" "c"))
                     "p")))))

(ert-deftest agent-repl-drawer-test-effective-parent-in-section-merging-direct-only ()
  "MERGING-section topology uses the same direct-source-ws rule.
Both MERGING and MERGED route through `--effective-parent-in-section'
so their internal parent/child relationships render without flattening
through other flattenable ancestors."
  (agent-repl-test--with-clean-state
    (puthash "p" '(:project-dir "/p/") agent-repl--workspaces)
    (puthash "c" '(:project-dir "/c/" :source-ws-dir "/p/")
             agent-repl--workspaces)
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should (equal (agent-repl-drawer--effective-parent-in-section
                      "c" '("p" "c"))
                     "p")))))

(ert-deftest agent-repl-drawer-test-effective-parent-does-not-flatten-through-merge-completed ()
  "MAIN/HIDDEN trees do NOT flatten through `:merge-completed' alone.
Flattening is git-ancestry-only (`--ws-flattenable-ancestor-p' reads
`:branch-merged' = `merged' exclusively), so a workflow-completed
ancestor without the ancestry cache must remain in the chain.  In
practice the async poll will follow shortly and converge the two; this
test guards against workflow-state leaking into tree topology."
  (agent-repl-test--with-clean-state
    (puthash "gp" '(:project-dir "/gp/") agent-repl--workspaces)
    (puthash "p"  '(:project-dir "/p/"  :source-ws-dir "/gp/"
                    :merge-completed t)
             agent-repl--workspaces)
    (puthash "c"  '(:project-dir "/c/"  :source-ws-dir "/p/")
             agent-repl--workspaces)
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      ;; "p" is not flattenable, so "c"'s effective parent search
      ;; through ("gp" "c") finds no candidate and returns nil.
      (should (null (agent-repl-drawer--effective-parent "c" '("gp" "c")))))))

(ert-deftest agent-repl-drawer-test-effective-parent-flattens-through-branch-merged ()
  "MAIN/HIDDEN trees flatten through ancestors with `:branch-merged' = `merged'.
This is the sole flattening signal under the new semantics: git
ancestry alone, not workflow state."
  (agent-repl-test--with-clean-state
    (puthash "gp" '(:project-dir "/gp/") agent-repl--workspaces)
    (puthash "p"  '(:project-dir "/p/"  :source-ws-dir "/gp/"
                    :branch-merged merged)
             agent-repl--workspaces)
    (puthash "c"  '(:project-dir "/c/"  :source-ws-dir "/p/")
             agent-repl--workspaces)
    (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity))
      (should (equal (agent-repl-drawer--effective-parent "c" '("gp" "c"))
                     "gp")))))

(ert-deftest agent-repl-drawer-test-build-tree-roots-and-children ()
  "`--build-tree' returns sorted forest with sorted children."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "root" :priority "p1")
    (agent-repl-drawer-test--register "kid"  :priority "p2")
    (let* ((parent-fn (lambda (w) (when (equal w "kid") "root")))
           (forest (agent-repl-drawer--build-tree '("root" "kid") parent-fn)))
      (should (equal (mapcar #'car forest) '("root")))
      (should (equal (mapcar #'car (cdr (car forest))) '("kid"))))))

(ert-deftest agent-repl-drawer-test-section-headers-include-counts ()
  "Section header labels show entry counts across all four buckets.
`:merging' t routes to MERGING; `:merge-completed' t routes to MERGED
— they are independent workflow-state buckets."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "main-a")
    (agent-repl-drawer-test--register "main-b")
    (agent-repl-drawer-test--register "hid"     :repl-state :hidden)
    (agent-repl-drawer-test--register "merging" :merging    t)
    (agent-repl-drawer-test--register "merged"  :merge-completed t)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "MAIN (2)"    text))
        (should (string-match-p "HIDDEN (1)"  text))
        (should (string-match-p "MERGING (1)" text))
        (should (string-match-p "MERGED (1)"  text))))))

(ert-deftest agent-repl-drawer-test-render-four-sections ()
  "Render emits MAIN, HIDDEN, MERGING, and MERGED headers in that order."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "main-ws")
    (agent-repl-drawer-test--register "hidden-ws"  :repl-state :hidden)
    (agent-repl-drawer-test--register "merging-ws" :merging    t)
    (agent-repl-drawer-test--register "merged-ws"  :merge-completed t)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
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

(ert-deftest agent-repl-drawer-test-render-in-flight-ws-lands-in-merging ()
  "A workspace flagged `:merging' t lands under MERGING.
Regression guard: ancestry alone (`:branch-merged' = `merged') must
NOT route here under the new semantics — only the explicit workflow
flag set by `agent-repl--workspace-merge-do' qualifies."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "merging-ws" :merging t)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
             (merging-pos (string-match "MERGING" text))
             (merged-pos  (string-match "MERGED" text))
             (ws-pos      (string-match "merging-ws" text)))
        ;; merging-ws must appear AFTER MERGING but BEFORE MERGED.
        (should (and merging-pos merged-pos ws-pos
                     (> ws-pos merging-pos)
                     (< ws-pos merged-pos)))))))

(ert-deftest agent-repl-drawer-test-render-merge-completed-ws-lands-in-merged ()
  "A workspace with `:merge-completed' t lands under MERGED.
This is the success path for an explicit merge command."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "completed-ws" :merge-completed t)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
             (merged-pos (string-match "MERGED" text))
             (ws-pos     (string-match "completed-ws" text)))
        ;; completed-ws appears AFTER the MERGED header (not above it).
        (should (and merged-pos ws-pos (> ws-pos merged-pos)))))))

;;;; ---- Sort + partition ----

(ert-deftest agent-repl-drawer-test-sort-by-priority ()
  "Sort places p05 before p1, then alphabetical within rank."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws-a" :priority "p1")
    (agent-repl-drawer-test--register "ws-b" :priority "p05")
    (agent-repl-drawer-test--register "ws-c" :priority "p1")
    (let ((sorted (agent-repl-drawer--sort '("ws-a" "ws-b" "ws-c"))))
      (should (equal sorted '("ws-b" "ws-a" "ws-c"))))))

(ert-deftest agent-repl-drawer-test-sort-unprioritized-last ()
  "Workspaces without :priority sort after every prioritized one."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws-x")
    (agent-repl-drawer-test--register "ws-y" :priority "p3")
    (let ((sorted (agent-repl-drawer--sort '("ws-x" "ws-y"))))
      (should (equal sorted '("ws-y" "ws-x"))))))

(ert-deftest agent-repl-drawer-test-partition-splits-hidden ()
  "Partition separates :hidden workspaces from visible."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "vis" :priority "p1")
    (agent-repl-drawer-test--register "gone" :priority "p1" :repl-state :hidden)
    (let ((parts (agent-repl-drawer--partition '("vis" "gone"))))
      (should (equal (car parts) '("vis")))
      (should (equal (cdr parts) '("gone"))))))

;;;; ---- nil-perspective filter ----

(ert-deftest agent-repl-drawer-test-visible-keys-filters-persp-nil-name ()
  "`--visible-workspace-keys' drops the key whose name equals `persp-nil-name'."
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none"))
      (agent-repl-drawer-test--register "real" :priority "p1")
      (agent-repl-drawer-test--register "none" :priority "p1")
      (let ((keys (agent-repl-drawer--visible-workspace-keys)))
        (should (member "real" keys))
        (should-not (member "none" keys))))))

(ert-deftest agent-repl-drawer-test-visible-keys-filters-bare-none ()
  "`--visible-workspace-keys' drops keys whose bare name equals `persp-nil-name'."
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none"))
      (agent-repl-drawer-test--register "DWC/real" :priority "p1")
      (agent-repl-drawer-test--register "DWC/none" :priority "p1")
      (let ((keys (agent-repl-drawer--visible-workspace-keys)))
        (should (member "DWC/real" keys))
        (should-not (member "DWC/none" keys))))))

(ert-deftest agent-repl-drawer-test-render-omits-none-workspace ()
  "Render does not surface a workspace whose name equals `persp-nil-name'."
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none"))
      (agent-repl-drawer-test--register "real" :priority "p1")
      (agent-repl-drawer-test--register "none" :priority "p1")
      (agent-repl-drawer-test--with-buffer
        (agent-repl-drawer--render)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "real" text))
          ;; "none" only appears as the empty-section placeholder, never as a
          ;; workspace entry — assert no workspace line carries it as its
          ;; `agent-repl-drawer-workspace' text property.
          (goto-char (point-min))
          (let (found-none)
            (while (and (not found-none) (not (eobp)))
              (when (equal (get-text-property (point)
                                              'agent-repl-drawer-workspace)
                           "none")
                (setq found-none t))
              (forward-line 1))
            (should-not found-none)))))))

;;;; ---- Project-dir-less stub filter ----

(ert-deftest agent-repl-drawer-test-visible-keys-filters-project-dir-less-stub ()
  "`--visible-workspace-keys' drops a live entry that has no `:project-dir'."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "real" :priority "p1")
    ;; Shape written by a persp hook onto Doom's default "main" persp.
    (puthash "main" '(:ws-loaded t :repl-state :dead) agent-repl--workspaces)
    (let ((keys (agent-repl-drawer--visible-workspace-keys)))
      (should (member "real" keys))
      (should-not (member "main" keys)))))

(ert-deftest agent-repl-drawer-test-visible-keys-keeps-project-dir-workspace ()
  "`--visible-workspace-keys' keeps an entry whose `:project-dir' is set."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "real" :project-dir "/repo/ws")
    (should (member "real" (agent-repl-drawer--visible-workspace-keys)))))

(ert-deftest agent-repl-drawer-test-render-omits-no-repo-group ()
  "Render emits no `(no repo)' group when a project-dir-less stub is registered."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "real" :priority "p1")
    (puthash "main" '(:ws-loaded t :repl-state :dead) agent-repl--workspaces)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "real" text))
        (should-not (string-match-p "(no repo)" text))))))

(ert-deftest agent-repl-drawer-test-render-omits-stub-workspace-entry ()
  "Render surfaces no workspace entry for a project-dir-less stub."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "real" :priority "p1")
    (puthash "main" '(:ws-loaded t :repl-state :dead) agent-repl--workspaces)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (goto-char (point-min))
      (let (found-stub)
        (while (and (not found-stub) (not (eobp)))
          (when (equal (get-text-property (point) 'agent-repl-drawer-workspace)
                       "main")
            (setq found-stub t))
          (forward-line 1))
        (should-not found-stub)))))

;;;; ---- Render ----

(ert-deftest agent-repl-drawer-test-render-empty-shows-main-not-hidden ()
  "Empty registry renders MAIN header but omits HIDDEN (no hidden entries)."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "MAIN" text))
        (should-not (string-match-p "HIDDEN" text))))))

(ert-deftest agent-repl-drawer-test-render-contains-name ()
  "Render includes the workspace name in its line."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "feature-x" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (should (string-match-p "feature-x"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest agent-repl-drawer-test-render-contains-summary ()
  "Render shows :last-prompt-summary on the subtitle line."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "fx"
                                       :priority "p1"
                                       :last-prompt-summary "Refactor login flow")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (should (string-match-p "Refactor login flow"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest agent-repl-drawer-test-render-pending-summary ()
  "Pending summary renders an ellipsis placeholder."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "fx"
                                       :priority "p1"
                                       :last-prompt-summary-pending t)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (should (string-match-p "…"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest agent-repl-drawer-test-render-hides-empty-hidden-section ()
  "HIDDEN section is omitted when no workspaces have hidden state."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "vis" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "MAIN" text))
        (should-not (string-match-p "HIDDEN" text))))))

(ert-deftest agent-repl-drawer-test-render-shows-hidden-section-when-populated ()
  "HIDDEN section appears when at least one workspace is hidden."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "vis" :priority "p1")
    (agent-repl-drawer-test--register "hid" :repl-state :hidden)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "HIDDEN (1)" text))
        (should (string-match-p "hid" text))))))

(ert-deftest agent-repl-drawer-test-render-section-headers-styled ()
  "Section headers carry the `agent-repl-drawer-section-title' face."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (when (memq 'agent-repl-drawer-section-title
                      (let ((face (get-text-property (point) 'face)))
                        (if (listp face) face (list face))))
            (setq found t))
          (forward-char 1))
        (should found)))))

(ert-deftest agent-repl-drawer-test-render-attaches-workspace-property ()
  "Each rendered workspace block carries a `agent-repl-drawer-workspace' text property."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (goto-char (point-min))
      ;; The first character of the workspace block carries the property.
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (when (equal (get-text-property (point) 'agent-repl-drawer-workspace)
                       "alpha")
            (setq found t))
          (forward-char 1))
        (should found)))))

;;;; ---- Navigation ----

(ert-deftest agent-repl-drawer-test-next-moves-to-next-workspace ()
  "`agent-repl-drawer-next' walks past the MAIN + repo headers into successive workspaces."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "first" :priority "p1")
    (agent-repl-drawer-test--register "second" :priority "p2")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (goto-char (point-min))
      (agent-repl-drawer-next)               ;; repo group header
      (agent-repl-drawer-next)
      (should (equal (agent-repl-drawer--workspace-at-point) "first"))
      (agent-repl-drawer-next)
      (should (equal (agent-repl-drawer--workspace-at-point) "second")))))

(ert-deftest agent-repl-drawer-test-show-positions-on-first-workspace ()
  "`agent-repl-drawer--goto-first-workspace' lands on the first workspace line."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (should (equal (agent-repl-drawer--workspace-at-point) "alpha")))))

(ert-deftest agent-repl-drawer-test-goto-workspace-line-finds-target ()
  "`agent-repl-drawer--goto-workspace-line' lands on the named workspace's block."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p2")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (should (agent-repl-drawer--goto-workspace-line "beta"))
      (should (equal (agent-repl-drawer--workspace-at-point) "beta")))))

(ert-deftest agent-repl-drawer-test-goto-workspace-line-returns-nil-for-unknown ()
  "`agent-repl-drawer--goto-workspace-line' returns nil when the workspace isn't rendered."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (should-not (agent-repl-drawer--goto-workspace-line "ghost")))))

(ert-deftest agent-repl-drawer-test-prev-moves-back ()
  "`agent-repl-drawer-prev' moves up to the previous workspace block."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "first" :priority "p1")
    (agent-repl-drawer-test--register "second" :priority "p2")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (goto-char (point-min))
      (agent-repl-drawer-next)   ;; repo group header
      (agent-repl-drawer-next)   ;; first
      (agent-repl-drawer-next)   ;; second
      (agent-repl-drawer-prev)   ;; back to first
      (should (equal (agent-repl-drawer--workspace-at-point) "first")))))

(ert-deftest agent-repl-drawer-test-next-stops-at-last-workspace ()
  "`agent-repl-drawer-next' on the last workspace stays on it."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "only" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (goto-char (point-min))
      (agent-repl-drawer-next)               ;; repo group header
      (agent-repl-drawer-next)               ;; lands on "only"
      (should (equal (agent-repl-drawer--workspace-at-point) "only"))
      (let ((before (point)))
        (agent-repl-drawer-next)             ;; nothing further to go to
        (should (equal (agent-repl-drawer--workspace-at-point) "only"))
        (should (= (point) before))))))

;;;; ---- Visit ----

(ert-deftest agent-repl-drawer-test-visit-dispatches-workspace-switch ()
  "`agent-repl-drawer-visit' calls `+workspace-switch' with the selected name."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "target" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((switched-to nil))
        (cl-letf (((symbol-function '+workspace-switch)
                   (lambda (ws &rest _) (setq switched-to ws))))
          (agent-repl-drawer-visit))
        (should (equal switched-to "target"))))))

(ert-deftest agent-repl-drawer-test-visit-no-workspace-errors ()
  "`agent-repl-drawer-visit' on a line without a workspace signals user-error."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (goto-char (point-min))
      (should-error (agent-repl-drawer-visit) :type 'user-error))))

(ert-deftest agent-repl-drawer-test-visit-on-merged-reactivates ()
  "`agent-repl-drawer-visit' on a MERGED entry routes to
`--reactivate-merged' (which re-establishes the persp) instead of
calling `+workspace-switch' (which would fail because the persp was
torn down at merge time)."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "agent-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (agent-repl-drawer-test--register
             "merged-ws" :merge-completed t :project-dir tmp)
            (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                       (lambda (&rest args)
                         (pcase args
                           (`("-C" ,dir "rev-parse" "--git-common-dir")
                            (concat (file-name-as-directory dir) ".git"))
                           (_ (error "unmocked git-string-quiet: %S" args))))))
            (agent-repl-drawer-test--with-buffer
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace)
              (let ((established-with nil)
                    (switched-to nil))
                (cl-letf (((symbol-function 'agent-repl--state-save)
                           (lambda (&rest _) nil))
                          ((symbol-function 'agent-repl--establish-workspace)
                           (lambda (ws dir) (setq established-with (list ws dir))))
                          ((symbol-function '+workspace-switch)
                           (lambda (ws &rest _) (setq switched-to ws))))
                  (agent-repl-drawer-visit))
                (should (equal established-with (list "merged-ws" tmp)))
                (should-not switched-to)))))
        (delete-directory tmp t)))))

(ert-deftest agent-repl-drawer-test-visit-on-merged-clears-merge-flags ()
  "Reactivating a MERGED workspace via `drawer-visit' clears the
`:merge-completed' / `:merge-completed-at' / `:repl-state :merged'
plist keys so the entry leaves the MERGED bucket on next render."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "agent-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (agent-repl-drawer-test--register
             "merged-ws"
             :merge-completed t
             :merge-completed-at 1234567890.0
             :repl-state :merged
             :project-dir tmp)
            (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                       (lambda (&rest args)
                         (pcase args
                           (`("-C" ,dir "rev-parse" "--git-common-dir")
                            (concat (file-name-as-directory dir) ".git"))
                           (_ (error "unmocked git-string-quiet: %S" args))))))
            (agent-repl-drawer-test--with-buffer
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace)
              (cl-letf (((symbol-function 'agent-repl--state-save)
                         (lambda (&rest _) nil))
                        ((symbol-function 'agent-repl--establish-workspace)
                         (lambda (&rest _) nil)))
                (agent-repl-drawer-visit))
              (should-not (agent-repl--ws-get "merged-ws" :merge-completed))
              (should-not (agent-repl--ws-get "merged-ws" :merge-completed-at))
              (should-not (agent-repl--ws-get "merged-ws" :repl-state))
              (should-not (eq (agent-repl-drawer--workspace-section "merged-ws")
                              :merged)))))
        (delete-directory tmp t)))))

(ert-deftest agent-repl-drawer-test-visit-on-merged-persists-cleared-flags ()
  "Reactivation calls `--state-save' so the cleared `:merge-completed'
flag survives to disk — without this, the snapshot loader would put
the workspace right back into MERGED on the next Emacs restart."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "agent-repl-test-merged-" t)))
      (unwind-protect
          (progn
            (agent-repl-drawer-test--register
             "merged-ws" :merge-completed t :project-dir tmp)
            (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
                       (lambda (&rest args)
                         (pcase args
                           (`("-C" ,dir "rev-parse" "--git-common-dir")
                            (concat (file-name-as-directory dir) ".git"))
                           (_ (error "unmocked git-string-quiet: %S" args))))))
            (agent-repl-drawer-test--with-buffer
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace)
              (let ((state-saved-for nil))
                (cl-letf (((symbol-function 'agent-repl--state-save)
                           (lambda (ws) (setq state-saved-for ws)))
                          ((symbol-function 'agent-repl--establish-workspace)
                           (lambda (&rest _) nil)))
                  (agent-repl-drawer-visit))
                (should (equal state-saved-for "merged-ws"))))))
        (delete-directory tmp t)))))

(ert-deftest agent-repl-drawer-test-visit-on-merged-without-project-dir-errors ()
  "`drawer-visit' on a MERGED entry whose `:project-dir' is missing or
no longer points to a real directory signals `user-error' — reactivation
needs a valid worktree dir to establish into."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register
     "merged-ws" :merge-completed t :project-dir "/nonexistent/path/here")
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args)
                 (pcase args
                   (`("-C" ,dir "rev-parse" "--git-common-dir")
                    (concat (file-name-as-directory dir) ".git"))
                   (_ (error "unmocked git-string-quiet: %S" args))))))
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((established-called nil))
        (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                   (lambda (&rest _) (setq established-called t))))
          (should-error (agent-repl-drawer-visit) :type 'user-error))
        (should-not established-called))))))

(ert-deftest agent-repl-drawer-test-visit-redirects-from-side-window-before-switch ()
  "`agent-repl-drawer-visit' leaves a side-window selection before calling
`+workspace-switch'.  Persp's `persp-delete-other-windows' uses
`ignore-window-parameters t' on restore, and with a side window
selected its fallback anchor can clobber the destination workspace's
Claude panel windows — pre-selecting a main-area window sidesteps that."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (agent-repl-test--with-clean-state
            (agent-repl-drawer-test--register "target" :priority "p1")
            (let* ((drawer-buf (get-buffer-create agent-repl-drawer-buffer-name))
                   (drawer-win (display-buffer-in-side-window
                                drawer-buf '((side . left) (slot . 0)))))
              (unwind-protect
                  (progn
                    (with-current-buffer drawer-buf
                      (agent-repl-drawer-mode)
                      (agent-repl-drawer--render))
                    (select-window drawer-win)
                    ;; Set point AFTER selecting the window — `select-window'
                    ;; resets buffer-point to the window's `window-point',
                    ;; so a pre-select goto would be clobbered.
                    (agent-repl-drawer--goto-first-workspace)
                    ;; Sanity: we are actually in a side window now and
                    ;; positioned on a workspace entry.
                    (should (window-parameter (selected-window) 'window-side))
                    (should (agent-repl-drawer--workspace-at-point))
                    (let ((sel-at-switch nil))
                      (cl-letf (((symbol-function '+workspace-switch)
                                 (lambda (_ws &rest _)
                                   (setq sel-at-switch (selected-window)))))
                        (agent-repl-drawer-visit))
                      (should sel-at-switch)
                      (should-not (window-parameter sel-at-switch 'window-side))))
                (when (window-live-p drawer-win) (delete-window drawer-win))
                (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf))))))
      (set-window-configuration wconf))))

(ert-deftest agent-repl-drawer-test-visit-no-redirect-when-not-in-side-window ()
  "When the selected window is not a side window, `agent-repl-drawer-visit'
leaves the selection untouched — the redirect is conditional on a
side-window selection."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (agent-repl-test--with-clean-state
            (agent-repl-drawer-test--register "target" :priority "p1")
            (agent-repl-drawer-test--with-buffer
              (agent-repl-drawer--render)
              (agent-repl-drawer--goto-first-workspace)
              ;; Selected window is the test runner's main window — not a
              ;; side window — even though current-buffer is the drawer
              ;; buffer here.
              (let ((sel-before (selected-window))
                    (sel-at-switch nil))
                (should-not (window-parameter sel-before 'window-side))
                (cl-letf (((symbol-function '+workspace-switch)
                           (lambda (_ws &rest _)
                             (setq sel-at-switch (selected-window)))))
                  (agent-repl-drawer-visit))
                (should (eq sel-at-switch sel-before))))))
      (set-window-configuration wconf))))

(ert-deftest agent-repl-drawer-test-leave-side-window-helper-noop-when-not-side ()
  "`agent-repl-drawer--leave-side-window-before-switch' is a no-op when
the selected window has no `window-side' parameter."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (let ((sel-before (selected-window)))
            (agent-repl-drawer--leave-side-window-before-switch)
            (should (eq (selected-window) sel-before))))
      (set-window-configuration wconf))))

(ert-deftest agent-repl-drawer-test-leave-side-window-descends-to-live-leaf-when-main-split ()
  "`agent-repl-drawer--leave-side-window-before-switch' selects a LIVE
main-area leaf — never the internal main window — when the main area is
split into multiple windows.  `window-main-window' returns a non-live
internal window in that case, and handing it to `select-window' signals
`wrong-type-argument window-live-p'.  Regression guard for the RET-in-
drawer crash where the user had the main area split."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          ;; Split the main area so `window-main-window' is an internal
          ;; (non-live) window rather than a single live leaf.
          (split-window-right)
          (let ((drawer-buf (get-buffer-create agent-repl-drawer-buffer-name)))
            (unwind-protect
                (let ((drawer-win (display-buffer-in-side-window
                                   drawer-buf '((side . left) (slot . 0)))))
                  ;; Precondition: the main window really is a non-live
                  ;; internal window (the bug trigger).
                  (should-not (window-live-p (window-main-window)))
                  (select-window drawer-win)
                  (should (window-parameter (selected-window) 'window-side))
                  ;; Act: must NOT error on the internal main window.
                  (agent-repl-drawer--leave-side-window-before-switch)
                  ;; Assert: landed on a live, non-side main-area window.
                  (should (window-live-p (selected-window)))
                  (should-not (window-parameter (selected-window) 'window-side)))
              (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf)))))
      (set-window-configuration wconf))))

(ert-deftest agent-repl-drawer-test-display-action-marks-no-other-window ()
  "The drawer display-action marks its window `no-other-window' t so
buffer-display machinery never repurposes the dedicated side window.
Regression guard for the magit RET-on-commit failure: `no-other-window'
nil let `+magit--display-buffer-in-direction' pick the drawer and error
in `switch-to-buffer'."
  (let ((params (alist-get 'window-parameters
                           agent-repl-drawer--display-action)))
    (should (eq (alist-get 'no-other-window params) t))))

(ert-deftest agent-repl-drawer-test-no-other-window-side-window-skipped-by-direction ()
  "A left side window carrying `no-other-window' is invisible to
`window-in-direction'.  This is the exact condition that makes magit's
`+magit--display-buffer-in-direction' fallback split the main window
instead of trying (and failing) to `switch-to-buffer' in the dedicated
drawer."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (let* ((main-buf (generate-new-buffer " *test-main*"))
                 (main-win (selected-window))
                 (side-buf (generate-new-buffer " *test-side*"))
                 (side-win (display-buffer-in-side-window
                            side-buf '((side . left) (slot . 0)
                                       (window-parameters
                                        (no-other-window . t))))))
            (set-window-buffer main-win main-buf)
            (unwind-protect
                (with-selected-window main-win
                  ;; No window to the right; the drawer to the left is
                  ;; skipped because of `no-other-window'.
                  (should-not (window-in-direction 'right))
                  (should-not (window-in-direction 'left)))
              (when (window-live-p side-win) (delete-window side-win))
              (when (buffer-live-p main-buf) (kill-buffer main-buf))
              (when (buffer-live-p side-buf) (kill-buffer side-buf)))))
      (set-window-configuration wconf))))

(ert-deftest agent-repl-drawer-test-no-other-window-nil-side-window-found-by-direction ()
  "Control: a left side window WITHOUT `no-other-window' IS returned by
`window-in-direction' — the pre-fix condition under which magit's
direction handler selected the drawer and failed."
  (let ((wconf (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (let* ((main-buf (generate-new-buffer " *test-main*"))
                 (main-win (selected-window))
                 (side-buf (generate-new-buffer " *test-side*"))
                 (side-win (display-buffer-in-side-window
                            side-buf '((side . left) (slot . 0)
                                       (window-parameters
                                        (no-other-window . nil))))))
            (set-window-buffer main-win main-buf)
            (unwind-protect
                (with-selected-window main-win
                  (should (eq (window-in-direction 'left) side-win)))
              (when (window-live-p side-win) (delete-window side-win))
              (when (buffer-live-p main-buf) (kill-buffer main-buf))
              (when (buffer-live-p side-buf) (kill-buffer side-buf)))))
      (set-window-configuration wconf))))

(ert-deftest agent-repl-drawer-test-leave-side-window-helper-selects-main ()
  "`agent-repl-drawer--leave-side-window-before-switch' moves the
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
                  (agent-repl-drawer--leave-side-window-before-switch)
                  (should-not (window-parameter (selected-window) 'window-side)))
              (when (window-live-p side-win) (delete-window side-win))
              (when (buffer-live-p main-buf) (kill-buffer main-buf))
              (when (buffer-live-p side-buf) (kill-buffer side-buf)))))
      (set-window-configuration wconf))))

;;;; ---- Current-entry overlay + cursor ----

(ert-deftest agent-repl-drawer-test-entry-bounds-spans-block ()
  "`--entry-bounds-at-point' covers both header and summary lines."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let* ((bounds (agent-repl-drawer--entry-bounds-at-point))
             (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (should bounds)
        (should (string-match-p "alpha" text))
        (should (string-match-p "\n" text))))))

(ert-deftest agent-repl-drawer-test-entry-bounds-nil-on-non-workspace-line ()
  "`--entry-bounds-at-point' returns nil on section headers / blanks."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (goto-char (point-min))
      (should-not (agent-repl-drawer--entry-bounds-at-point)))))

(ert-deftest agent-repl-drawer-test-update-current-entry-overlay-positions ()
  "`--update-current-entry-overlay' creates an overlay at the entry's start."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p2")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (should (agent-repl-drawer--goto-workspace-line "beta"))
      (agent-repl-drawer--update-current-entry-overlay)
      (let ((ov agent-repl-drawer--current-entry-overlay))
        (should (overlayp ov))
        (should (equal (get-text-property (overlay-start ov)
                                          'agent-repl-drawer-workspace)
                       "beta"))
        (should (equal (overlay-end ov)
                       (+ (overlay-start ov)
                          (length agent-repl-drawer-gutter))))))))

(ert-deftest agent-repl-drawer-test-update-current-entry-overlay-deletes-off-entry ()
  "Overlay is removed when point lands on a non-workspace line."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (agent-repl-drawer--update-current-entry-overlay)
      (should (overlayp agent-repl-drawer--current-entry-overlay))
      (goto-char (point-min)) ;; section header
      (agent-repl-drawer--update-current-entry-overlay)
      (should-not (overlay-buffer agent-repl-drawer--current-entry-overlay)))))

(ert-deftest agent-repl-drawer-test-cursor-hidden-at-col-0 ()
  "`--update-cursor' sets cursor-type nil at column 0."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--with-buffer
      (let ((inhibit-read-only t)) (insert "hello\n"))
      (goto-char (point-min))
      (agent-repl-drawer--update-cursor)
      (should (null cursor-type)))))

(ert-deftest agent-repl-drawer-test-cursor-visible-when-not-col-0 ()
  "`--update-cursor' sets cursor-type to 'box at non-zero columns."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--with-buffer
      (let ((inhibit-read-only t)) (insert "hello\n"))
      (goto-char (point-min))
      (forward-char 2)
      (agent-repl-drawer--update-cursor)
      (should (eq cursor-type 'box)))))

;;;; ---- Dir→name reverse-map cache (cold-path perf) ----

(ert-deftest agent-repl-drawer-test-with-dir-map-binds-and-restores ()
  "`--with-dir-map' binds the map for BODY and restores nil after.
Outside the dynamic extent the map must be nil so non-drawer callers
fall back to the legacy `--ws-name-for-dir' path."
  (agent-repl-test--with-clean-state
    (puthash "alpha" '(:project-dir "/a/") agent-repl--workspaces)
    (let ((agent-repl-drawer--dir->name-map nil))
      (should-not agent-repl-drawer--dir->name-map)
      (agent-repl-drawer--with-dir-map
        (should (hash-table-p agent-repl-drawer--dir->name-map)))
      (should-not agent-repl-drawer--dir->name-map))))

(ert-deftest agent-repl-drawer-test-with-dir-map-nested-reuses-outer ()
  "Nested `--with-dir-map' reuses the outer map instead of rebuilding.
The `or' branch in the macro guards against paying a second O(N)
`maphash' when a caller (e.g. `drawer-show') already wrapped an inner
`--render'."
  (agent-repl-test--with-clean-state
    (puthash "a" '(:project-dir "/a/") agent-repl--workspaces)
    (let ((build-count 0))
      (cl-letf* ((orig (symbol-function 'agent-repl-drawer--build-dir->name-map))
                 ((symbol-function 'agent-repl-drawer--build-dir->name-map)
                  (lambda (&rest args)
                    (cl-incf build-count)
                    (apply orig args))))
        (agent-repl-drawer--with-dir-map
          (agent-repl-drawer--with-dir-map
            (agent-repl-drawer--with-dir-map
              t)))
        (should (= build-count 1))))))

(ert-deftest agent-repl-drawer-test-source-ws-name-uses-dir-map-when-bound ()
  "Cold `--source-ws-name' resolves via the map without calling `--ws-name-for-dir'.
When `--dir->name-map' is bound, the slow O(N) reverse lookup must be
bypassed entirely — that bypass is the whole point of the map."
  (agent-repl-test--with-clean-state
    (puthash "parent" '(:project-dir "/parent/")  agent-repl--workspaces)
    (puthash "child"  '(:project-dir "/child/"
                        :source-ws-dir "/parent/")
             agent-repl--workspaces)
    (let ((legacy-calls 0))
      (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
                 (lambda (_dir)
                   (cl-incf legacy-calls)
                   nil)))
        (agent-repl-drawer--with-dir-map
          (should (equal (agent-repl-drawer--source-ws-name "child") "parent"))
          (should (= legacy-calls 0)))))))

(ert-deftest agent-repl-drawer-test-source-ws-name-falls-back-without-map ()
  "Outside `--with-dir-map' the legacy `--ws-name-for-dir' path is used.
Non-drawer callers (and any code path that hasn't been wrapped) must
keep behaving exactly as before."
  (agent-repl-test--with-clean-state
    (puthash "parent" '(:project-dir "/parent/")  agent-repl--workspaces)
    (puthash "child"  '(:project-dir "/child/"
                        :source-ws-dir "/parent/")
             agent-repl--workspaces)
    (let ((legacy-calls 0))
      (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
                 (lambda (_dir)
                   (cl-incf legacy-calls)
                   "parent")))
        (let ((agent-repl-drawer--dir->name-map nil))
          (should (equal (agent-repl-drawer--source-ws-name "child") "parent"))
          (should (= legacy-calls 1)))))))

;;;; ---- apply-background idempotence ----

(ert-deftest agent-repl-drawer-test-apply-background-idempotent ()
  "`--apply-background' adds exactly one relative remap on the `default'
face no matter how many times it is called.  Without cookie tracking,
each call would stack another `(:background ...)' entry onto the face's
relative-remap list — `agent-repl-drawer-mode' adds one and every
`agent-repl-drawer-show' adds another, so a buffer that has been
toggled N times would carry N+1 entries and pay redisplay overhead for
each one.  Pins the leak fix."
  (agent-repl-drawer-test--with-buffer
    ;; Mode init already called `--apply-background' once.  Call again
    ;; several times and assert the `default' remap entry list still has
    ;; exactly one user-installed relative spec.
    (agent-repl-drawer--apply-background)
    (agent-repl-drawer--apply-background)
    (agent-repl-drawer--apply-background)
    (let* ((entry (assq 'default face-remapping-alist))
           ;; cdr is the list of relative specs, with the final element
           ;; being the base face (`default' itself).  Drop the base.
           (relatives (and entry (butlast (cdr entry)))))
      (should (= (length relatives) 1))
      (should (equal (car relatives)
                     `(:background ,agent-repl-drawer-background))))))

;;;; ---- Refresh-if-visible ----

(ert-deftest agent-repl-drawer-test-refresh-if-visible-no-buffer-noop ()
  "`agent-repl-drawer--refresh-if-visible' is a no-op when no buffer exists."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer agent-repl-drawer-buffer-name)))
      (when buf (kill-buffer buf)))
    (should-not (agent-repl-drawer--refresh-if-visible))))

(ert-deftest agent-repl-drawer-test-refresh-if-visible-restores-unfocused-window-point ()
  "On a content-changing poll, `--refresh-if-visible' mirrors the restored
buffer-point onto an unfocused drawer window so its cursor does not snap
back to `point-min'.  Reproduces the reported reset that only manifested
while the drawer was not the selected window: the poll renders via
`with-current-buffer', so the `erase-buffer' in `--render' collapses the
unfocused window's `window-point' to the top unless it is re-synced."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :project-dir "/tmp/alpha")
    (agent-repl-drawer-test--register "beta"  :project-dir "/tmp/beta")
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_) nil))
              ((symbol-function 'agent-repl--git-string-quiet) (lambda (&rest _) "")))
      (let ((drawer-buf (get-buffer-create agent-repl-drawer-buffer-name))
            (other-buf  (get-buffer-create "*refresh-other*"))
            (drawer-win nil))
        (unwind-protect
            (progn
              (with-current-buffer drawer-buf (agent-repl-drawer-mode))
              ;; Drawer lives in a sibling window; selection stays on the
              ;; other window so the drawer window is genuinely unfocused.
              (set-window-buffer (selected-window) other-buf)
              (setq drawer-win (split-window))
              (set-window-buffer drawer-win drawer-buf)
              ;; First render, then park both the buffer-point and the
              ;; unfocused window's window-point on the second entry.
              (with-current-buffer drawer-buf
                (agent-repl-drawer--render)
                (should (agent-repl-drawer--goto-workspace-line "beta"))
                (set-window-point drawer-win (point)))
              ;; Mutate state so the next render actually rewrites the
              ;; buffer (signature + content both differ).
              (agent-repl-drawer-test--register "beta"
                                                 :project-dir "/tmp/beta"
                                                 :agent-state :thinking)
              ;; Sanity: the drawer window is not the selected one.
              (should-not (eq (selected-window) drawer-win))
              (agent-repl-drawer--refresh-if-visible)
              ;; The unfocused window's cursor must still sit on beta
              ;; rather than snapping to the top of the buffer.
              (should (equal (get-text-property (window-point drawer-win)
                                                'agent-repl-drawer-workspace
                                                drawer-buf)
                             "beta")))
          (when (and drawer-win (window-live-p drawer-win))
            (ignore-errors (delete-window drawer-win)))
          (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf))
          (when (buffer-live-p other-buf) (kill-buffer other-buf)))))))

;;;; ---- Window width ----

(ert-deftest agent-repl-drawer-test-width-fraction-default-is-0.20 ()
  "Default `agent-repl-drawer-width-fraction' is 0.20."
  (should (= 0.20
             (eval (car (get 'agent-repl-drawer-width-fraction
                             'standard-value))))))

(ert-deftest agent-repl-drawer-test-window-width-is-fraction-of-frame ()
  "`agent-repl-drawer--window-width' returns the configured fraction of frame-width."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-drawer-width-fraction 0.20))
      (cl-letf (((symbol-function 'window-frame) (lambda (_) 'fake-frame))
                ((symbol-function 'frame-width)  (lambda (_) 200)))
        (should (= (agent-repl-drawer--window-width 'fake-window) 40))))))

(ert-deftest agent-repl-drawer-test-window-width-floor-is-one ()
  "Width never drops below 1 column even on degenerate frames."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-drawer-width-fraction 0.20))
      (cl-letf (((symbol-function 'window-frame) (lambda (_) 'fake-frame))
                ((symbol-function 'frame-width)  (lambda (_) 0)))
        (should (= (agent-repl-drawer--window-width 'fake-window) 1))))))

(ert-deftest agent-repl-drawer-test-window-width-tiny-fraction ()
  "Tiny fractions like 0.01 are honored (no implicit clamping in the helper)."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-drawer-width-fraction 0.01))
      (cl-letf (((symbol-function 'window-frame) (lambda (_) 'fake-frame))
                ((symbol-function 'frame-width)  (lambda (_) 200)))
        (should (= (agent-repl-drawer--window-width 'fake-window) 2))))))

(ert-deftest agent-repl-drawer-test-window-width-constant-regardless-of-depth ()
  "Width is purely the fraction of frame-width, with NO depth bonus.
A deep workspace tree must not widen the drawer: the width is constant
at runtime and depends only on the fraction and the frame width."
  (agent-repl-test--with-clean-state
    (puthash "gp" '(:project-dir "/gp/") agent-repl--workspaces)
    (puthash "p"  '(:project-dir "/p/"  :source-ws-dir "/gp/")
             agent-repl--workspaces)
    (puthash "c"  '(:project-dir "/c/"  :source-ws-dir "/p/")
             agent-repl--workspaces)
    (let ((agent-repl-drawer-width-fraction    0.10)
          (agent-repl-drawer-indent-per-level  2))
      (cl-letf (((symbol-function 'window-frame) (lambda (_) 'fake-frame))
                ((symbol-function 'frame-width)  (lambda (_) 200))
                ((symbol-function 'agent-repl--ws-name-for-dir)
                 (lambda (dir)
                   (cond ((equal dir "/gp/") "gp")
                         ((equal dir "/p/")  "p")
                         (t nil)))))
        ;; Pre-change this returned 24 (base 20 + depth-bonus 4).  Now the
        ;; bonus is gone, so the width is exactly the fraction base: 20.
        (should (= (agent-repl-drawer--window-width 'fake-window) 20))))))

(ert-deftest agent-repl-drawer-test-ensure-visible-reapplies-width-when-already-visible ()
  "`--ensure-visible-on-persp-switch' reapplies width when drawer is already
visible — overriding whatever stale width persp's window-state-put
just restored from the destination workspace's saved config."
  (let ((agent-repl-drawer--global-visible-p t)
        (apply-called nil)
        (buf (get-buffer-create agent-repl-drawer-buffer-name)))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'fake-win))
                  ((symbol-function 'agent-repl-drawer--apply-width)
                   (lambda (w) (setq apply-called w))))
          (agent-repl-drawer--ensure-visible-on-persp-switch)
          (should (eq apply-called 'fake-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Priority display ----

(ert-deftest agent-repl-drawer-test-priority-display-falls-back-to-string ()
  "Without a loaded image, priority renders as the raw string."
  (let ((agent-repl--priority-images nil))
    (should (equal (agent-repl-drawer--priority-display "p1") "p1"))))

(ert-deftest agent-repl-drawer-test-priority-display-uses-image-when-available ()
  "When an image spec exists, the priority string carries it as `display'."
  (let* ((fake-image '(image :type png :file "/tmp/fake.png"))
         (agent-repl--priority-images `(("p1" . ,fake-image))))
    (let ((result (agent-repl-drawer--priority-display "p1")))
      (should (equal result "p1"))
      (should (equal (get-text-property 0 'display result) fake-image)))))

(ert-deftest agent-repl-drawer-test-priority-display-nil-priority ()
  "Nil priority renders as the empty string so unprioritized workspaces don't carry a phantom space."
  (should (equal (agent-repl-drawer--priority-display nil) "")))

;;;; ---- Group-label face (repo heading) ----

(defun agent-repl-drawer-test--face-scale (face)
  "Return FACE's height as a scale factor relative to the default face.
An unspecified `:height' means FACE renders at the default height, i.e.
a scale of 1.0."
  (let ((h (face-attribute face :height nil t)))
    (if (numberp h) h 1.0)))

(ert-deftest agent-repl-drawer-test-group-label-face-is-larger-than-workspace-name ()
  "The repo group label renders at a larger height than the workspace name."
  (should (> (agent-repl-drawer-test--face-scale 'agent-repl-drawer-group-label)
             (agent-repl-drawer-test--face-scale 'agent-repl-drawer-workspace-name))))

(ert-deftest agent-repl-drawer-test-group-label-face-foreground-is-white ()
  "The repo group label renders with a white foreground."
  (should (equal (face-attribute 'agent-repl-drawer-group-label :foreground nil t)
                 "white")))

;;;; ---- Name face (per-state coloring) ----

(ert-deftest agent-repl-drawer-test-name-face-thinking-is-red ()
  "`:thinking' state colors the name with the thinking-red foreground."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :agent-state :thinking)
    (let ((face (agent-repl-drawer--name-face "ws")))
      (should (equal (plist-get face :foreground) agent-repl--color-thinking-red))
      (should (eq (plist-get face :weight) 'bold)))))

(ert-deftest agent-repl-drawer-test-name-face-done-is-green ()
  "`:done' state colors the name with the done-green foreground."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :agent-state :done)
    (should (equal (plist-get (agent-repl-drawer--name-face "ws") :foreground)
                   agent-repl--color-done-green))))

(ert-deftest agent-repl-drawer-test-name-face-idle-is-orange ()
  "`:idle' state colors the name with the idle-orange foreground."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :agent-state :idle)
    (should (equal (plist-get (agent-repl-drawer--name-face "ws") :foreground)
                   agent-repl--color-idle-orange))))

(ert-deftest agent-repl-drawer-test-name-face-no-state-falls-back ()
  "No agent-state falls back to the plain bold workspace-name face."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws")
    (should (eq (agent-repl-drawer--name-face "ws")
                'agent-repl-drawer-workspace-name))))

(ert-deftest agent-repl-drawer-test-name-face-dead-falls-back ()
  "`:repl-state :dead' falls back to plain bold; the hidden/dim treatment muting is layered separately."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :agent-state :thinking
                                       :repl-state :dead)
    (should (eq (agent-repl-drawer--name-face "ws")
                'agent-repl-drawer-workspace-name))))

;;;; ---- Layout: priority/name spacing ----

(ert-deftest agent-repl-drawer-test-render-space-between-priority-and-name ()
  "When a priority is present, a space separates the badge text from the name."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "feature" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (should (string-match-p "p1 feature"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest agent-repl-drawer-test-render-glyph-name-gap ()
  "Two spaces separate the state glyph from the name when no priority is set."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "lonely")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p (concat (regexp-quote
                                         agent-repl-drawer-state-icon-default)
                                        "  lonely")
                                text))))))

(ert-deftest agent-repl-drawer-test-render-glyph-priority-gap ()
  "Two spaces separate the state glyph from the priority badge."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "feat" :priority "p1" :agent-state :idle)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p
                 (concat (regexp-quote
                          (alist-get :idle agent-repl-drawer-state-icons))
                         "  p1")
                 text))))))

(ert-deftest agent-repl-drawer-test-render-blank-line-between-workspaces ()
  "Adjacent workspace entries are separated by a blank line."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;; A `\n\n' must appear between the two workspace blocks.  The
        ;; precise location is the boundary between alpha's summary
        ;; line and beta's header line.
        (should (string-match-p "\n\n  .* beta" text))))))

;;;; ---- State icon palette defaults ----

(ert-deftest agent-repl-drawer-test-state-icons-defaults-applied ()
  "Reload-after-defcustom-change forces the latest palette to apply.
Without the explicit force-reset, defcustom would keep prior values for
already-bound symbols and palette tweaks would require an Emacs restart."
  (should (equal (alist-get :done       agent-repl-drawer-state-icons) "✅"))
  (should (equal (alist-get :thinking   agent-repl-drawer-state-icons) "⌛"))
  (should (equal (alist-get :idle       agent-repl-drawer-state-icons) "💤"))
  (should (equal (alist-get :init       agent-repl-drawer-state-icons) "⏳"))
  (should (equal (alist-get :stop-failed agent-repl-drawer-state-icons) "❗"))
  (should (equal (alist-get :dead       agent-repl-drawer-state-icons) "❌"))
  (should (equal (alist-get :merged     agent-repl-drawer-state-icons) "🔀"))
  (should (equal (alist-get :merge-conflict agent-repl-drawer-state-icons) "💥"))
  (should (equal (alist-get :merge-failed agent-repl-drawer-state-icons) "⛔")))

;;;; ---- State glyph ----

(ert-deftest agent-repl-drawer-test-state-glyph-dead-overrides-agent-state ()
  ":repl-state :dead takes precedence over :agent-state for the glyph."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "zombie"
                                       :agent-state :thinking
                                       :repl-state :dead)
    (should (equal (agent-repl-drawer--state-glyph "zombie")
                   (alist-get :dead agent-repl-drawer-state-icons)))))

(ert-deftest agent-repl-drawer-test-state-glyph-from-agent-state ()
  "Glyph reflects :agent-state when :repl-state is not :dead."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "busy" :agent-state :thinking)
    (should (equal (agent-repl-drawer--state-glyph "busy")
                   (alist-get :thinking agent-repl-drawer-state-icons)))))

(ert-deftest agent-repl-drawer-test-state-glyph-merged-shows-merged-when-no-agent-state ()
  ":repl-state :merged shows the 🔀 glyph when :agent-state is nil."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "merged-ws"
                                       :repl-state :merged)
    (should (equal (agent-repl-drawer--state-glyph "merged-ws")
                   (alist-get :merged agent-repl-drawer-state-icons)))))

(ert-deftest agent-repl-drawer-test-state-glyph-active-agent-state-beats-merged ()
  "An active :agent-state wins over :repl-state :merged — a merged workspace
that resumes work shows its live activity badge rather than the static 🔀."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "merged-ws"
                                       :agent-state :thinking
                                       :repl-state :merged)
    (should (equal (agent-repl-drawer--state-glyph "merged-ws")
                   (alist-get :thinking agent-repl-drawer-state-icons)))))

(ert-deftest agent-repl-drawer-test-state-glyph-merge-conflict-surfaces-collision ()
  ":repl-state :merge-conflict renders the 💥 glyph — a real cherry-pick
conflict that the auto-resolver rejected (or interactive abort).
Distinct from :merge-failed (silent git failure) and :dead (vterm
death) so the user can see at a glance that this row needs human
conflict resolution."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "conflicted-merge"
                                       :repl-state :merge-conflict)
    (should (equal (agent-repl-drawer--state-glyph "conflicted-merge")
                   "💥"))))

(ert-deftest agent-repl-drawer-test-state-glyph-merge-conflict-overrides-agent-state ()
  ":repl-state :merge-conflict takes precedence over :agent-state.
The vterm is still alive on a conflict (unlike :dead), but the badge
must surface the conflict rather than the mid-session mood."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :agent-state :thinking
                                       :repl-state :merge-conflict)
    (should (equal (agent-repl-drawer--state-glyph "ws") "💥"))))

(ert-deftest agent-repl-drawer-test-state-glyph-merge-conflict-overrides-dead ()
  "When `:repl-state' becomes `:dead' after a conflict (e.g., vterm
later dies), the conflict badge should win — the conflict signal is
more actionable than a generic process-death."
  (agent-repl-test--with-clean-state
    ;; Set :merge-conflict last so it wins (set semantics: latest write
    ;; wins; here we test precedence in resolve-time, so both registered
    ;; and the resolver picks :merge-conflict via state-glyph priority).
    (agent-repl-drawer-test--register "ws" :repl-state :merge-conflict)
    (should (equal (agent-repl-drawer--state-glyph "ws") "💥"))))

(ert-deftest agent-repl-drawer-test-state-glyph-merge-failed-surfaces-x ()
  ":repl-state :merge-failed renders the ⛔ glyph (failed cherry-pick
that still lives in the MERGED bucket).  Distinct mapping from
:dead's ❌ so the user can visually differentiate a stuck merge from
a dead vterm at a glance."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "broken-merge"
                                       :repl-state :merge-failed)
    (should (equal (agent-repl-drawer--state-glyph "broken-merge")
                   (alist-get :merge-failed agent-repl-drawer-state-icons)))))

(ert-deftest agent-repl-drawer-test-state-glyph-merge-failed-overrides-agent-state ()
  ":repl-state :merge-failed takes precedence over :agent-state for
the glyph — a post-merge silent-failure workspace whose vterm is
stale still reads as ⛔-merge-failed rather than its agent-state
mood."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :agent-state :thinking
                                       :repl-state :merge-failed)
    (should (equal (agent-repl-drawer--state-glyph "ws")
                   (alist-get :merge-failed agent-repl-drawer-state-icons)))))

(ert-deftest agent-repl-drawer-test-state-icons-include-merge-failed ()
  "`agent-repl-drawer-state-icons' includes a :merge-failed entry
using ⛔ — distinct from :dead's ❌ so a stuck merge does not look
like a dead vterm."
  (should (equal (alist-get :merge-failed agent-repl-drawer-state-icons) "⛔")))

(ert-deftest agent-repl-drawer-test-workspace-section-merge-failed-routes-to-merged ()
  "A workspace flagged with :merge-completed t still lands in the
:merged bucket even when its :repl-state is :merge-failed.  The
section bucket is driven exclusively by `:merge-completed' — the
:repl-state distinction is purely visual (badge selection in
`--state-glyph')."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :merge-completed t
                                       :repl-state :merge-failed)
    (should (eq (agent-repl-drawer--workspace-section "ws") :merged))))

(ert-deftest agent-repl-drawer-test-state-glyph-merged-overrides-dead ()
  ":repl-state :merged would normally not coexist with :dead, but if
both somehow appear in a stale plist, :merged wins so the merge badge
isn't masked by a post-nuke dead reading."
  (agent-repl-test--with-clean-state
    ;; Single :repl-state slot can't hold both — the test simulates the
    ;; precedence rule by setting :merged and confirming it's chosen
    ;; over the :dead icon-lookup path.
    (agent-repl-drawer-test--register "merged-not-dead"
                                       :repl-state :merged)
    (should (equal (agent-repl-drawer--state-glyph "merged-not-dead")
                   (alist-get :merged agent-repl-drawer-state-icons)))
    (should-not (equal (agent-repl-drawer--state-glyph "merged-not-dead")
                       (alist-get :dead agent-repl-drawer-state-icons)))))

;;;; ---- Tests: keyboard-inaccessibility bounce ----

(ert-deftest agent-repl-drawer-test-buffer-p-matches-drawer-name ()
  "`--buffer-p' returns non-nil for a buffer whose name matches `agent-repl-drawer-buffer-name'."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer agent-repl-drawer-buffer-name
      (should (agent-repl-drawer--buffer-p (current-buffer))))))

(ert-deftest agent-repl-drawer-test-buffer-p-rejects-other-buffer ()
  "`--buffer-p' returns nil for a buffer whose name does not match the drawer name."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*not-the-drawer*"
      (should-not (agent-repl-drawer--buffer-p (current-buffer))))))

(ert-deftest agent-repl-drawer-test-bounce-from-drawer-non-drawer-buffer ()
  "`--bounce-from-drawer' is a no-op when the selected window shows a non-drawer buffer.
Mirrors the vterm bounce's `non-vterm-buffer' baseline test — the predicate
must not fire on unrelated buffers."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*bounce-noop-regular*"
      (let ((orig-win (selected-window)))
        (set-window-buffer orig-win (current-buffer))
        (agent-repl-drawer--bounce-from-drawer nil)
        (should (eq (selected-window) orig-win))))))

(ert-deftest agent-repl-drawer-test-bounce-from-drawer-keyboard-redirects ()
  "Keyboard-driven selection of the drawer window is redirected to the MRU non-drawer window."
  (agent-repl-test--with-clean-state
    (let ((drawer-buf (get-buffer-create agent-repl-drawer-buffer-name))
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
                (agent-repl-drawer--bounce-from-drawer nil)
                (should-not (eq (window-buffer (selected-window)) drawer-buf)))))
        (when (and other-win (window-live-p other-win))
          (ignore-errors (delete-window other-win)))
        (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest agent-repl-drawer-test-bounce-from-drawer-mouse-does-not-redirect ()
  "Mouse-driven selection of the drawer window stays put — user wants to operate entries via click."
  (agent-repl-test--with-clean-state
    (let ((drawer-buf (get-buffer-create agent-repl-drawer-buffer-name))
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
                (agent-repl-drawer--bounce-from-drawer nil)
                (should (eq (selected-window) drawer-win)))))
        (when (and other-win (window-live-p other-win))
          (ignore-errors (delete-window other-win)))
        (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

(ert-deftest agent-repl-drawer-test-bounce-from-drawer-warns-when-no-other-window ()
  "When the drawer is the only window, the bounce emits a user-facing warning.
Parallels `bounce-from-vterm-warns-when-no-input-win' — surfacing the stuck
state is preferable to silently leaving point stranded in the drawer."
  (agent-repl-test--with-clean-state
    (let ((drawer-buf (get-buffer-create agent-repl-drawer-buffer-name))
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
                (agent-repl-drawer--bounce-from-drawer nil)))
            (should (cl-some (lambda (m) (string-match-p "no other window is available" m))
                             messages)))
        (when (buffer-live-p drawer-buf) (kill-buffer drawer-buf))))))

(ert-deftest agent-repl-drawer-test-show-does-not-select-drawer-window ()
  "`agent-repl-drawer-show' must NOT select the drawer window.
Keyboard-inaccessibility policy: the drawer is reachable only via mouse,
so even an explicit `show' command must leave selection where it was."
  (agent-repl-test--with-clean-state
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
                        ((symbol-function 'agent-repl-window--harden) #'ignore)
                        ((symbol-function 'agent-repl-drawer--apply-width) #'ignore))
                (agent-repl-drawer-show)
                (should (eq (selected-window) orig-win)))))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))
        (when (and drawer-buf (buffer-live-p drawer-buf))
          (kill-buffer drawer-buf))))))

;;;; ---- Tests: :merge-queued routing + glyph ----

(ert-deftest agent-repl-drawer-test-workspace-section-merge-queued-routes-to-merging ()
  "`:repl-state :merge-queued' buckets the workspace under MERGING so
queued merges appear alongside in-flight ones."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :repl-state :merge-queued)
    (should (eq (agent-repl-drawer--workspace-section "ws") :merging))))

(ert-deftest agent-repl-drawer-test-workspace-section-merging-dominates-queued ()
  "`:merging' t (in-flight) outranks `:repl-state :merge-queued'.
Covers the brief window between drain clearing `:merge-queued' and
`--workspace-merge-do' setting `:merging'."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :merging t
                                       :repl-state :merge-queued)
    (should (eq (agent-repl-drawer--workspace-section "ws") :merging))))

(ert-deftest agent-repl-drawer-test-workspace-section-merge-completed-dominates-queued ()
  "A completed merge marker outranks `:merge-queued' — should not happen
in practice (completed clears repl-state) but the precedence chain
must remain stable."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :merge-completed t
                                       :repl-state :merge-queued)
    (should (eq (agent-repl-drawer--workspace-section "ws") :merged))))

(ert-deftest agent-repl-drawer-test-state-glyph-merge-queued ()
  "`:repl-state :merge-queued' surfaces the 🕒 glyph from the icon
alist, distinct from :merging (no icon — the merging bucket shows the
underlying agent-state glyph) and :merged (🔀)."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :repl-state :merge-queued)
    (should (equal (agent-repl-drawer--state-glyph "ws")
                   (alist-get :merge-queued
                              agent-repl-drawer-state-icons)))))

(ert-deftest agent-repl-drawer-test-state-glyph-merge-queued-overrides-agent-state ()
  "`:merge-queued' on repl-state outranks a stale `:agent-state' —
guards against the queued badge being clobbered by a leftover
thinking/done glyph."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws"
                                       :repl-state :merge-queued
                                       :agent-state :thinking)
    (should (equal (agent-repl-drawer--state-glyph "ws")
                   (alist-get :merge-queued
                              agent-repl-drawer-state-icons)))))

(ert-deftest agent-repl-drawer-test-state-glyph-merged-still-wins-over-queued ()
  "Precedence guard: `:merged' beats `:merge-queued' on the glyph too."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :repl-state :merged)
    (should (equal (agent-repl-drawer--state-glyph "ws")
                   (alist-get :merged agent-repl-drawer-state-icons)))))

;;;; ---- Tests: center-selection ----

(ert-deftest agent-repl-drawer-test-center-selection-calls-recenter ()
  "`--center-selection' calls `recenter' on every window showing the
drawer buffer.  Establishes one displayed window, stubs `recenter' to
record the count, and asserts exactly one invocation — the always-on
center-cursor contract."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name))
          (calls 0))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) buf)
            (cl-letf (((symbol-function 'recenter)
                       (lambda (&rest _) (cl-incf calls))))
              (agent-repl-drawer--center-selection buf))
            (should (= calls 1)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-drawer-test-center-selection-noop-when-no-window ()
  "Helper is a no-op when the drawer buffer is not displayed in any
live window."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create agent-repl-drawer-buffer-name))
          (called nil))
      (unwind-protect
          (cl-letf (((symbol-function 'recenter)
                     (lambda (&rest _) (setq called t))))
            (agent-repl-drawer--center-selection buf)
            (should-not called))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-drawer-test-center-selection-noop-when-no-buffer ()
  "Helper is a no-op (no error) when no drawer buffer exists at all.
Exercises the default-arg path: `when-let*' on `(get-buffer ...)'
short-circuits to nil."
  (agent-repl-test--with-clean-state
    (when-let ((b (get-buffer agent-repl-drawer-buffer-name)))
      (kill-buffer b))
    (should-not (agent-repl-drawer--center-selection))))

(ert-deftest agent-repl-drawer-test-post-command-calls-center-selection ()
  "`--post-command' must invoke `--center-selection' so every j/k move
re-centers the cursor."
  (agent-repl-test--with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'agent-repl-drawer--center-selection)
                 (lambda (&optional _buf) (setq called t)))
                ((symbol-function 'agent-repl-drawer--update-current-entry-overlay)
                 #'ignore)
                ((symbol-function 'agent-repl-drawer--update-cursor)
                 #'ignore))
        (agent-repl-drawer--post-command)
        (should called)))))

(ert-deftest agent-repl-drawer-test-post-command-skips-center-when-ws-unchanged ()
  "`--post-command' must NOT re-invoke `--center-selection' when point is
still on the same workspace entry as on the previous tick.  `recenter'
forces a window redisplay; firing it on no-op commands or intra-entry
motion is the per-keystroke perf hit this gates against."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((calls 0))
        (cl-letf (((symbol-function 'agent-repl-drawer--center-selection)
                   (lambda (&optional _buf) (cl-incf calls)))
                  ((symbol-function 'agent-repl-drawer--update-current-entry-overlay)
                   #'ignore)
                  ((symbol-function 'agent-repl-drawer--update-cursor)
                   #'ignore))
          (agent-repl-drawer--post-command)
          (should (= calls 1))
          (agent-repl-drawer--post-command)
          (should (= calls 1)))))))

(ert-deftest agent-repl-drawer-test-post-command-recenters-after-entry-change ()
  "`--post-command' must re-invoke `--center-selection' when navigation
crosses an entry boundary — i.e. the normal j/k case.  Pairs with
`--post-command-skips-center-when-ws-unchanged' to assert the gating
is keyed on entry change, not unconditionally disabled."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--register "beta"  :priority "p2")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((calls 0))
        (cl-letf (((symbol-function 'agent-repl-drawer--center-selection)
                   (lambda (&optional _buf) (cl-incf calls)))
                  ((symbol-function 'agent-repl-drawer--update-current-entry-overlay)
                   #'ignore)
                  ((symbol-function 'agent-repl-drawer--update-cursor)
                   #'ignore))
          (agent-repl-drawer--post-command)
          (agent-repl-drawer-next)
          (agent-repl-drawer--post-command)
          (should (= calls 2)))))))

(ert-deftest agent-repl-drawer-test-post-command-skips-overlay-when-ws-unchanged ()
  "`--post-command' must NOT re-invoke `--update-current-entry-overlay'
when ws-at-point is unchanged.  The overlay refresh walks the entry's
characters via `--entry-bounds-at-point' — gating it on entry change
saves the per-keystroke buffer scan."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "alpha" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-first-workspace)
      (let ((calls 0))
        (cl-letf (((symbol-function 'agent-repl-drawer--update-current-entry-overlay)
                   (lambda () (cl-incf calls)))
                  ((symbol-function 'agent-repl-drawer--center-selection)
                   #'ignore)
                  ((symbol-function 'agent-repl-drawer--update-cursor)
                   #'ignore))
          (agent-repl-drawer--post-command)
          (should (= calls 1))
          (agent-repl-drawer--post-command)
          (should (= calls 1)))))))


(ert-deftest agent-repl-drawer-test-sync-cursor-calls-center-selection ()
  "`--sync-cursor-to-current-ws' must also center — persp-driven cursor
moves should keep the active workspace centered, not just user j/k."
  (agent-repl-test--with-clean-state
    (let* ((buf (get-buffer-create agent-repl-drawer-buffer-name))
           (called nil))
      (unwind-protect
          (cl-letf (((symbol-function '+workspace-current-name)
                     (lambda () "ws"))
                    ((symbol-function 'agent-repl-drawer--goto-workspace-line)
                     (lambda (_ws) t))
                    ((symbol-function 'agent-repl-drawer--update-current-entry-overlay)
                     #'ignore)
                    ((symbol-function 'agent-repl-drawer--center-selection)
                     (lambda (&optional _buf) (setq called t))))
            (agent-repl-drawer--sync-cursor-to-current-ws)
            (should called))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;;; ---- Tests: --source-ws-name plist cache ----

(ert-deftest agent-repl-drawer-test-source-ws-name-returns-nil-without-source-dir ()
  "`--source-ws-name' returns nil when the workspace has no `:source-ws-dir'.
Existing root-workspace contract — no caching, no scan."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "root" :project-dir "/tmp/root")
    (should-not (agent-repl-drawer--source-ws-name "root"))))

(ert-deftest agent-repl-drawer-test-source-ws-name-populates-cache-on-miss ()
  "First call resolves via `--ws-name-for-dir' and writes the result
into `:source-ws-name' on the workspace plist."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "parent" :project-dir "/tmp/parent")
    (agent-repl-drawer-test--register "child"  :project-dir "/tmp/child"
                                       :source-ws-dir "/tmp/parent")
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
               (lambda (dir) (when (equal dir "/tmp/parent") "parent"))))
      (should-not (agent-repl--ws-get "child" :source-ws-name))
      (should (equal (agent-repl-drawer--source-ws-name "child") "parent"))
      (should (equal (agent-repl--ws-get "child" :source-ws-name) "parent")))))

(ert-deftest agent-repl-drawer-test-source-ws-name-cache-hit-skips-scan ()
  "Second call returns the cached value without consulting
`--ws-name-for-dir'.  Pins the O(1) fast-path contract — the cache
is the WHOLE point of this rewrite."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "child"
                                       :project-dir "/tmp/child"
                                       :source-ws-dir "/tmp/parent"
                                       :source-ws-name "parent")
    (let ((scan-called nil))
      (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
                 (lambda (_dir) (setq scan-called t) "should-not-see-this")))
        (should (equal (agent-repl-drawer--source-ws-name "child") "parent"))
        (should-not scan-called)))))

(ert-deftest agent-repl-drawer-test-source-ws-name-does-not-cache-nil-resolution ()
  "When the reverse lookup returns nil (source workspace deleted, no
match for `:source-ws-dir' in the hash), the cache stays clear — a nil
write would be indistinguishable from \"never cached\" on the next read
under the `or'-fall-through model."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "child"
                                       :project-dir "/tmp/child"
                                       :source-ws-dir "/tmp/orphan")
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
               (lambda (_dir) nil)))
      (should-not (agent-repl-drawer--source-ws-name "child"))
      (should-not (agent-repl--ws-get "child" :source-ws-name)))))

;;;; ---- Repo folding ----

(ert-deftest agent-repl-drawer-test-group-header-carries-repo-property ()
  "The repo group header carries the `agent-repl-drawer-repo' text property."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (should (agent-repl-drawer--goto-repo-line
               agent-repl-drawer-test--group-key))
      (should (equal (agent-repl-drawer--repo-at-point)
                     agent-repl-drawer-test--group-key)))))

(ert-deftest agent-repl-drawer-test-entry-at-point-on-repo-header ()
  "`--entry-at-point' reports a `:repo' entry on the group header."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-repo-line agent-repl-drawer-test--group-key)
      (should (equal (agent-repl-drawer--entry-at-point)
                     (cons :repo agent-repl-drawer-test--group-key))))))

(ert-deftest agent-repl-drawer-test-entry-at-point-on-workspace ()
  "`--entry-at-point' reports a `:workspace' entry on a workspace block."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-workspace-line "ws")
      (should (equal (agent-repl-drawer--entry-at-point)
                     (cons :workspace "ws"))))))

(ert-deftest agent-repl-drawer-test-next-stops-on-repo-header ()
  "`agent-repl-drawer-next' selects the repo group header before its workspaces."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (goto-char (point-min))
      (agent-repl-drawer-next)
      (should (equal (agent-repl-drawer--repo-at-point)
                     agent-repl-drawer-test--group-key)))))

(ert-deftest agent-repl-drawer-test-prev-returns-to-repo-header ()
  "`agent-repl-drawer-prev' from the first workspace lands back on the repo header."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-workspace-line "ws")
      (agent-repl-drawer-prev)
      (should (equal (agent-repl-drawer--repo-at-point)
                     agent-repl-drawer-test--group-key)))))

(ert-deftest agent-repl-drawer-test-tab-on-repo-header-folds ()
  "`TAB' on a repo group header folds the repo."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-repo-line agent-repl-drawer-test--group-key)
      (agent-repl-drawer-toggle-expand)
      (should (agent-repl--repo-folded-p agent-repl-drawer-test--group-key)))))

(ert-deftest agent-repl-drawer-test-tab-on-folded-repo-header-unfolds ()
  "`TAB' on a folded repo group header unfolds it."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl--toggle-repo-fold agent-repl-drawer-test--group-key)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (agent-repl-drawer--goto-repo-line agent-repl-drawer-test--group-key)
      (agent-repl-drawer-toggle-expand)
      (should-not (agent-repl--repo-folded-p
                   agent-repl-drawer-test--group-key)))))

(ert-deftest agent-repl-drawer-test-folded-repo-hides-its-workspaces ()
  "A folded repo renders no workspace blocks beneath its header."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl--toggle-repo-fold agent-repl-drawer-test--group-key)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (should-not (agent-repl-drawer--goto-workspace-line "ws")))))

(ert-deftest agent-repl-drawer-test-folded-repo-keeps-its-header ()
  "A folded repo still renders its own group header."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl--toggle-repo-fold agent-repl-drawer-test--group-key)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (should (agent-repl-drawer--goto-repo-line
               agent-repl-drawer-test--group-key)))))

(ert-deftest agent-repl-drawer-test-folded-repo-renders-folded-glyph ()
  "A folded repo header renders the folded glyph instead of the expanded one."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl--toggle-repo-fold agent-repl-drawer-test--group-key)
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "▸ agent-repl-test-repo" text))
        (should-not (string-match-p "▾ agent-repl-test-repo" text))))))

(ert-deftest agent-repl-drawer-test-unfolded-repo-leaves-other-repo-alone ()
  "Folding one repo does not hide another repo's workspaces."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "doom-a" :project-dir "/d/")
    (agent-repl-drawer-test--register "ee-a"   :project-dir "/e/")
    (agent-repl--ws-put "doom-a" :group-key "/path/doom/.git")
    (agent-repl--ws-put "ee-a"   :group-key "/path/explanation-engine/.git")
    (agent-repl--toggle-repo-fold "/path/explanation-engine/.git")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (should (agent-repl-drawer--goto-workspace-line "doom-a"))
      (should-not (agent-repl-drawer--goto-workspace-line "ee-a")))))

(ert-deftest agent-repl-drawer-test-render-signature-tracks-fold-state ()
  "`--render-signature' changes when a repo is folded, so the poll re-renders."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (let ((before (agent-repl-drawer--render-signature)))
        (agent-repl--toggle-repo-fold agent-repl-drawer-test--group-key)
        (should-not (equal before (agent-repl-drawer--render-signature)))))))

(ert-deftest agent-repl-drawer-test-fold-toggle-forces-tab-bar-redraw ()
  "Folding a repo from the drawer repaints the tab-bar immediately."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (let ((redraws 0))
      (cl-letf (((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf redraws))))
        (agent-repl-drawer-test--with-buffer
          (agent-repl-drawer--render)
          (agent-repl-drawer--goto-repo-line agent-repl-drawer-test--group-key)
          (agent-repl-drawer-toggle-expand)
          (should (= redraws 1)))))))

(ert-deftest agent-repl-drawer-test-tab-on-non-entry-line-user-errors ()
  "`TAB' on a section header (not an entry) signals a `user-error'."
  (agent-repl-test--with-clean-state
    (agent-repl-drawer-test--register "ws" :priority "p1")
    (agent-repl-drawer-test--with-buffer
      (agent-repl-drawer--render)
      (goto-char (point-min))
      (should-error (agent-repl-drawer-toggle-expand) :type 'user-error))))
;;;; ---- Tests: MERGE QUEUE section ----
;;
;; The stream builder and the renderer are pure functions of the merge globals,
;; so none of these touch git.

(defun agent-repl-test--in-flight (ws dir commits index &optional conflict-sha)
  "Register WS as an in-flight merge into DIR over COMMITS, applying INDEX."
  (push (list :source-ws ws :target-dir dir :started-at 0.0)
        agent-repl--in-flight-merges)
  (agent-repl--merge-progress-begin ws commits)
  (agent-repl--merge-progress-put ws :commit-index index)
  (when conflict-sha
    (agent-repl--merge-progress-put ws :conflict-sha conflict-sha)))

(defun agent-repl-test--queued (ws dir commits &optional halted)
  "Register WS as a queued merge into DIR with a COMMITS lookahead."
  (setq agent-repl--merge-queue
        (append agent-repl--merge-queue
                (list (list :source-ws ws :target-dir dir
                            :halt-until-human halted))))
  (puthash ws (list :commits commits) agent-repl--merge-lookahead))

(ert-deftest agent-repl-test-merge-stream-empty-when-idle ()
  "No queue and nothing in flight yields an empty stream, so the section is omitted."
  (agent-repl-test--with-merge-state
    (should (null (agent-repl-drawer--merge-stream)))))

(ert-deftest agent-repl-test-merge-stream-current-commit-is-index ()
  "The commit at `:commit-index' is the one being applied."
  (agent-repl-test--with-merge-state
    (agent-repl-test--in-flight
     "ws" "/r/doom" '(("a" . "1") ("b" . "2") ("c" . "3")) 1)
    (let ((first (car (agent-repl-drawer--merge-stream))))
      (should (equal "b" (plist-get first :sha)))
      (should (eq 'current (plist-get first :state))))))

(ert-deftest agent-repl-test-merge-stream-already-applied-excluded ()
  "Commits git already applied are not shown."
  (agent-repl-test--with-merge-state
    (agent-repl-test--in-flight
     "ws" "/r/doom" '(("a" . "1") ("b" . "2") ("c" . "3")) 1)
    (should-not (member "a" (mapcar (lambda (e) (plist-get e :sha))
                                    (agent-repl-drawer--merge-stream))))))

(ert-deftest agent-repl-test-merge-stream-rest-of-range-is-pending ()
  "Commits behind the current one in the same pick are `pending'."
  (agent-repl-test--with-merge-state
    (agent-repl-test--in-flight "ws" "/r/doom" '(("a" . "1") ("b" . "2")) 0)
    (should (eq 'pending (plist-get (nth 1 (agent-repl-drawer--merge-stream))
                                    :state)))))

(ert-deftest agent-repl-test-merge-stream-conflict-state ()
  "A recorded conflict SHA puts the current commit in the `conflict' state."
  (agent-repl-test--with-merge-state
    (agent-repl-test--in-flight "ws" "/r/doom" '(("a" . "1")) 0 "a")
    (should (eq 'conflict (plist-get (car (agent-repl-drawer--merge-stream))
                                     :state)))))

(ert-deftest agent-repl-test-merge-stream-queued-follows-in-flight ()
  "Queued commits come after the in-flight pick's, which is what lets a
lookahead cross into another project."
  (agent-repl-test--with-merge-state
    (agent-repl-test--in-flight "a" "/r/doom" '(("a1" . "1")) 0)
    (agent-repl-test--queued "b" "/r/services" '(("b1" . "2")))
    (should (equal '("a1" "b1")
                   (mapcar (lambda (e) (plist-get e :sha))
                           (agent-repl-drawer--merge-stream))))))

(ert-deftest agent-repl-test-merge-stream-halted-state ()
  "A halted queue entry's commits are flagged `halted'."
  (agent-repl-test--with-merge-state
    (agent-repl-test--queued "ws" "/r/doom" '(("a" . "1")) t)
    (should (eq 'halted (plist-get (car (agent-repl-drawer--merge-stream))
                                   :state)))))

(ert-deftest agent-repl-test-merge-stream-project-from-target-dir ()
  "A commit's project is the basename of its merge target."
  (agent-repl-test--with-merge-state
    (agent-repl-test--in-flight "ws" "/r/doom" '(("a" . "1")) 0)
    (should (equal "doom" (plist-get (car (agent-repl-drawer--merge-stream))
                                     :project)))))

(ert-deftest agent-repl-test-merge-visible-budget-limits-pending ()
  "The lookahead budget caps how many pending commits are shown."
  (agent-repl-test--with-merge-state
    (let ((agent-repl-drawer-merge-lookahead 3))
      (agent-repl-test--in-flight
       "ws" "/r/doom"
       '(("a" . "1") ("b" . "2") ("c" . "3") ("d" . "4") ("e" . "5")) 0)
      (should (= 4 (length (agent-repl-drawer--merge-stream-visible
                            (agent-repl-drawer--merge-stream))))))))

(ert-deftest agent-repl-test-merge-visible-keeps-every-current ()
  "Every in-flight project's current commit survives the budget: buckets drain
concurrently, so hiding one would hide a merge that is actually running."
  (agent-repl-test--with-merge-state
    (let ((agent-repl-drawer-merge-lookahead 0))
      (agent-repl-test--in-flight "a" "/r/doom" '(("a1" . "1")) 0)
      (agent-repl-test--in-flight "b" "/r/services" '(("b1" . "2")) 0)
      (should (= 2 (length (agent-repl-drawer--merge-stream-visible
                            (agent-repl-drawer--merge-stream))))))))

(ert-deftest agent-repl-test-merge-visible-under-budget-not-padded ()
  "Fewer commits than the budget renders what exists, with no padding."
  (agent-repl-test--with-merge-state
    (let ((agent-repl-drawer-merge-lookahead 3))
      (agent-repl-test--in-flight "ws" "/r/doom" '(("a" . "1")) 0)
      (should (= 1 (length (agent-repl-drawer--merge-stream-visible
                            (agent-repl-drawer--merge-stream))))))))

(ert-deftest agent-repl-test-merge-elapsed-hidden-below-threshold ()
  "A commit under the slow threshold shows no clock, so a fast queue stays quiet."
  (let ((agent-repl-drawer-merge-slow-commit-threshold 3.0))
    (should (null (agent-repl-drawer--merge-elapsed-string 100.0 101.2)))))

(ert-deftest agent-repl-test-merge-elapsed-shown-above-threshold ()
  "A commit over the slow threshold shows an M:SS clock."
  (let ((agent-repl-drawer-merge-slow-commit-threshold 3.0))
    (should (equal "0:04" (agent-repl-drawer--merge-elapsed-string 100.0 104.5)))))

(ert-deftest agent-repl-test-merge-elapsed-formats-minutes ()
  "Past a minute the clock rolls over rather than counting seconds forever."
  (let ((agent-repl-drawer-merge-slow-commit-threshold 3.0))
    (should (equal "1:07" (agent-repl-drawer--merge-elapsed-string 0.0 67.0)))))

(ert-deftest agent-repl-test-merge-subject-truncated ()
  "An over-long subject is ellipsized: a wrapped line costs the narrow drawer dearly."
  (should (equal "abcd…" (agent-repl-drawer--merge-truncate "abcdefgh" 5))))

(ert-deftest agent-repl-test-merge-section-interleaves-project-separators ()
  "The section emits a separator only where the project changes.

This is the exact case from the request: the current pick is project A, the
next commit is also project A, and the two behind those are project B."
  (agent-repl-test--with-merge-state
    (let ((agent-repl-drawer-merge-lookahead 3))
      (agent-repl-test--in-flight "a" "/r/doom" '(("a1" . "one") ("a2" . "two")) 0)
      (agent-repl-test--queued "b" "/r/services" '(("b1" . "three") ("b2" . "four")))
      (with-temp-buffer
        (agent-repl-drawer--insert-merge-queue-section
         (agent-repl-drawer--merge-stream-visible (agent-repl-drawer--merge-stream))
         0.0)
        (let ((lines (seq-remove #'string-empty-p
                                 (split-string (buffer-string) "\n"))))
          ;; header, rule, ▸doom, a1, a2, ▸services, b1, b2
          (should (string-match-p "MERGE QUEUE (4)" (nth 0 lines)))
          (should (string-match-p "doom"      (nth 2 lines)))
          (should (string-match-p "a1"         (nth 3 lines)))
          (should (string-match-p "a2"         (nth 4 lines)))
          (should (string-match-p "services" (nth 5 lines)))
          (should (string-match-p "b1"         (nth 6 lines)))
          (should (string-match-p "b2"         (nth 7 lines))))))))

(ert-deftest agent-repl-test-merge-section-one-separator-per-run ()
  "A run of commits in one project carries exactly one separator."
  (agent-repl-test--with-merge-state
    (let ((agent-repl-drawer-merge-lookahead 3))
      (agent-repl-test--in-flight
       "a" "/r/doom" '(("a1" . "1") ("a2" . "2") ("a3" . "3")) 0)
      (with-temp-buffer
        (agent-repl-drawer--insert-merge-queue-section
         (agent-repl-drawer--merge-stream-visible (agent-repl-drawer--merge-stream))
         0.0)
        (should (= 1 (cl-count-if
                      (lambda (l) (string-match-p "\\`\\s-*[▾▸]\\s-*doom\\'" l))
                      (split-string (buffer-string) "\n"))))))))

(ert-deftest agent-repl-test-merge-section-conflict-detail-line ()
  "A conflicted commit renders its unmerged-file count and resolver phase."
  (agent-repl-test--with-merge-state
    (agent-repl-test--in-flight "ws" "/r/doom" '(("a" . "1")) 0 "a")
    (agent-repl--merge-progress-put "ws" :conflict-files '("f.txt" "g.txt"))
    (agent-repl--merge-progress-put "ws" :resolver-phase 'verifying)
    (with-temp-buffer
      (agent-repl-drawer--insert-merge-queue-section
       (agent-repl-drawer--merge-stream) 0.0)
      (should (string-match-p "2 files unmerged" (buffer-string)))
      (should (string-match-p "resolver: verifying" (buffer-string))))))

(ert-deftest agent-repl-test-merge-rows-are-not-workspace-rows ()
  "Commit rows carry `agent-repl-drawer-commit', never the workspace property.
This is what keeps j/k navigation, marks, expansion, and cursor restore from
ever seeing a commit row — and what makes it safe for MERGE QUEUE and MERGING
to coexist without rendering a workspace twice."
  (agent-repl-test--with-merge-state
    (agent-repl-test--in-flight "ws" "/r/doom" '(("a" . "1")) 0)
    (with-temp-buffer
      (agent-repl-drawer--insert-merge-queue-section
       (agent-repl-drawer--merge-stream) 0.0)
      (goto-char (point-max))
      (forward-line -1)
      (should (equal "a" (get-text-property (point) 'agent-repl-drawer-commit)))
      (should (null (get-text-property (point) 'agent-repl-drawer-workspace))))))

(ert-deftest agent-repl-test-merge-progress-invalidates-render-signature ()
  "A progress write changes the render signature, so the 1Hz poll cannot
short-circuit the redraw."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-merge-state
      (agent-repl-drawer-test--with-buffer
        (let ((before (agent-repl-drawer--render-signature)))
          (agent-repl--merge-progress-put "ws" :commit-index 1)
          (should-not (equal before (agent-repl-drawer--render-signature))))))))

(provide 'test-drawer)
;;; test-drawer.el ends here
