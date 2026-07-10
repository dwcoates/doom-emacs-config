;;; test-hide-project-dirs.el --- ERT tests for hide-project-dirs.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the project-dir-based workspace hiding toggle.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-hide-project-dirs.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Helpers ----

(defun agent-repl-hide-project-dirs-test--register (ws dir)
  "Register live WS in `agent-repl--workspaces' with `:project-dir' DIR."
  (puthash ws (list :project-dir dir) agent-repl--workspaces))

(defun agent-repl-hide-project-dirs-test--register-hidden-tombstone (ws dir)
  "Register WS as a tombstone marked `:hidden-project-dir' with `:project-dir' DIR."
  (puthash ws
           (list :project-dir dir
                 :nuked-at (current-time)
                 :hidden-project-dir t)
           agent-repl--workspaces))

(defmacro agent-repl-hide-project-dirs-test--with-prefixes (prefixes &rest body)
  "Run BODY with `agent-repl-hide-project-dirs' temporarily set to PREFIXES.
Also disables the toggle by default so individual tests can enable it
locally."
  (declare (indent 1))
  `(let ((agent-repl-hide-project-dirs ,prefixes)
         (agent-repl-hide-project-dirs-enabled nil))
     ,@body))

;;;; ---- Predicate: --ws-matches-p ----

(ert-deftest agent-repl-test-hide-project-dirs-matches-direct ()
  "ws-matches-p is non-nil when project-dir IS the prefix itself."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (agent-repl-hide-project-dirs-test--register
       "ws-cc" (expand-file-name "~/workspace/ChessCom"))
      (should (agent-repl--hide-project-dirs--ws-matches-p "ws-cc")))))

(ert-deftest agent-repl-test-hide-project-dirs-matches-nested ()
  "ws-matches-p is non-nil for a nested directory under the prefix."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (agent-repl-hide-project-dirs-test--register
       "ws-deep" (expand-file-name "~/workspace/ChessCom/services/api"))
      (should (agent-repl--hide-project-dirs--ws-matches-p "ws-deep")))))

(ert-deftest agent-repl-test-hide-project-dirs-does-not-match-sibling ()
  "ws-matches-p is nil for a sibling dir whose name shares the prefix's leading chars."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (agent-repl-hide-project-dirs-test--register
       "ws-sibling" (expand-file-name "~/workspace/ChessCom-archive"))
      (should-not (agent-repl--hide-project-dirs--ws-matches-p "ws-sibling")))))

(ert-deftest agent-repl-test-hide-project-dirs-does-not-match-unrelated ()
  "ws-matches-p is nil for a workspace whose project-dir lives elsewhere."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (agent-repl-hide-project-dirs-test--register
       "ws-other" (expand-file-name "~/code/some-other-project"))
      (should-not (agent-repl--hide-project-dirs--ws-matches-p "ws-other")))))

(ert-deftest agent-repl-test-hide-project-dirs-ignores-missing-project-dir ()
  "ws-matches-p returns nil when the workspace has no `:project-dir'."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (puthash "ws-no-dir" (list :priority "p1") agent-repl--workspaces)
      (should-not (agent-repl--hide-project-dirs--ws-matches-p "ws-no-dir")))))

(ert-deftest agent-repl-test-hide-project-dirs-matches-second-prefix ()
  "ws-matches-p hits any prefix in the configured list, not just the first."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/Other")
              (expand-file-name "~/workspace/ChessCom"))
      (agent-repl-hide-project-dirs-test--register
       "ws-cc" (expand-file-name "~/workspace/ChessCom/foo"))
      (should (agent-repl--hide-project-dirs--ws-matches-p "ws-cc")))))

;;;; ---- Selection: --matching-live-workspaces ----

(ert-deftest agent-repl-test-hide-project-dirs-matching-live-includes-match ()
  "--matching-live-workspaces returns a live workspace under a hide prefix."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (agent-repl-hide-project-dirs-test--register
       "ws-cc" (expand-file-name "~/workspace/ChessCom/a"))
      (should (member "ws-cc"
                      (agent-repl--hide-project-dirs--matching-live-workspaces))))))

(ert-deftest agent-repl-test-hide-project-dirs-matching-live-excludes-current ()
  "--matching-live-workspaces never returns the current workspace."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (agent-repl-hide-project-dirs-test--register
       "ws-cur" (expand-file-name "~/workspace/ChessCom/cur"))
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "ws-cur")))
        (should-not (member "ws-cur"
                            (agent-repl--hide-project-dirs--matching-live-workspaces)))))))

(ert-deftest agent-repl-test-hide-project-dirs-matching-live-excludes-non-match ()
  "--matching-live-workspaces omits workspaces outside every hide prefix."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (agent-repl-hide-project-dirs-test--register
       "ws-other" (expand-file-name "~/code/x"))
      (should-not (member "ws-other"
                          (agent-repl--hide-project-dirs--matching-live-workspaces))))))

(ert-deftest agent-repl-test-hide-project-dirs-matching-live-excludes-tombstone ()
  "--matching-live-workspaces omits tombstoned matches — only live ones count."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (agent-repl-hide-project-dirs-test--register-hidden-tombstone
       "ws-tomb" (expand-file-name "~/workspace/ChessCom/t"))
      (should-not (member "ws-tomb"
                          (agent-repl--hide-project-dirs--matching-live-workspaces))))))

;;;; ---- Selection: --hidden-workspace-names ----

(ert-deftest agent-repl-test-hide-project-dirs-hidden-names-includes-marked ()
  "--hidden-workspace-names returns workspaces carrying `:hidden-project-dir'."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--register-hidden-tombstone
     "ws-a" "/tmp/a")
    (should (member "ws-a" (agent-repl--hide-project-dirs--hidden-workspace-names)))))

(ert-deftest agent-repl-test-hide-project-dirs-hidden-names-excludes-unmarked ()
  "--hidden-workspace-names omits workspaces without the `:hidden-project-dir' marker."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--register "ws-live" "/tmp/live")
    (puthash "ws-plain-tomb"
             (list :project-dir "/tmp/pt" :nuked-at (current-time))
             agent-repl--workspaces)
    (should-not (member "ws-live"
                        (agent-repl--hide-project-dirs--hidden-workspace-names)))
    (should-not (member "ws-plain-tomb"
                        (agent-repl--hide-project-dirs--hidden-workspace-names)))))

(ert-deftest agent-repl-test-hide-project-dirs-hidden-names-sorted ()
  "--hidden-workspace-names returns marked workspaces in name order."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--register-hidden-tombstone "ws-c" "/tmp/c")
    (agent-repl-hide-project-dirs-test--register-hidden-tombstone "ws-a" "/tmp/a")
    (agent-repl-hide-project-dirs-test--register-hidden-tombstone "ws-b" "/tmp/b")
    (should (equal (agent-repl--hide-project-dirs--hidden-workspace-names)
                   '("ws-a" "ws-b" "ws-c")))))

;;;; ---- --hide ----

(ert-deftest agent-repl-test-hide-project-dirs-hide-marks-and-nukes ()
  "--hide stamps `:hidden-project-dir' and nukes every matching workspace."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (let ((nuked nil))
        (agent-repl-hide-project-dirs-test--register
         "ws-cc1" (expand-file-name "~/workspace/ChessCom/a"))
        (agent-repl-hide-project-dirs-test--register
         "ws-cc2" (expand-file-name "~/workspace/ChessCom/b"))
        (cl-letf (((symbol-function 'agent-repl--nuke-one-workspace)
                   (lambda (ws &optional _preserve) (push ws nuked))))
          (let ((hidden (agent-repl--hide-project-dirs--hide)))
            (should (equal (sort (copy-sequence hidden) #'string<)
                           '("ws-cc1" "ws-cc2")))
            (should (equal (sort nuked #'string<) '("ws-cc1" "ws-cc2")))
            (should (agent-repl--ws-get "ws-cc1" :hidden-project-dir))
            (should (agent-repl--ws-get "ws-cc2" :hidden-project-dir))))))))

(ert-deftest agent-repl-test-hide-project-dirs-hide-skips-non-match ()
  "--hide leaves workspaces outside the hide prefixes untouched."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (let ((nuked nil))
        (agent-repl-hide-project-dirs-test--register
         "ws-other" (expand-file-name "~/code/x"))
        (cl-letf (((symbol-function 'agent-repl--nuke-one-workspace)
                   (lambda (ws &optional _preserve) (push ws nuked))))
          (agent-repl--hide-project-dirs--hide)
          (should-not nuked)
          (should-not (agent-repl--ws-get "ws-other" :hidden-project-dir)))))))

(ert-deftest agent-repl-test-hide-project-dirs-hide-skips-current ()
  "--hide never marks or nukes the current workspace even when it matches."
  (agent-repl-test--with-clean-state
    (agent-repl-hide-project-dirs-test--with-prefixes
        (list (expand-file-name "~/workspace/ChessCom"))
      (let ((nuked nil))
        (agent-repl-hide-project-dirs-test--register
         "ws-cur" (expand-file-name "~/workspace/ChessCom/cur"))
        (agent-repl-hide-project-dirs-test--register
         "ws-cc2" (expand-file-name "~/workspace/ChessCom/b"))
        (cl-letf (((symbol-function '+workspace-current-name)
                   (lambda () "ws-cur"))
                  ((symbol-function 'agent-repl--nuke-one-workspace)
                   (lambda (ws &optional _preserve) (push ws nuked))))
          (agent-repl--hide-project-dirs--hide)
          (should (equal nuked '("ws-cc2")))
          (should-not (agent-repl--ws-get "ws-cur" :hidden-project-dir)))))))

;;;; ---- --restore ----

(ert-deftest agent-repl-test-hide-project-dirs-restore-establishes-and-clears ()
  "--restore re-establishes every hidden tombstone and clears the marker."
  (agent-repl-test--with-clean-state
    (let ((established nil))
      (agent-repl-hide-project-dirs-test--register-hidden-tombstone
       "ws-cc1" "/tmp/cc1")
      (agent-repl-hide-project-dirs-test--register-hidden-tombstone
       "ws-cc2" "/tmp/cc2")
      (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                 (lambda (ws _dir) (push ws established))))
        (let ((restored (agent-repl--hide-project-dirs--restore)))
          (should (equal restored '("ws-cc1" "ws-cc2")))
          (should (equal (sort established #'string<) '("ws-cc1" "ws-cc2")))
          (should-not (agent-repl--ws-get "ws-cc1" :hidden-project-dir))
          (should-not (agent-repl--ws-get "ws-cc2" :hidden-project-dir)))))))

(ert-deftest agent-repl-test-hide-project-dirs-restore-skips-plain-tombstone ()
  "--restore ignores tombstones that lack the `:hidden-project-dir' marker."
  (agent-repl-test--with-clean-state
    (let ((established nil))
      (puthash "ws-hand-nuked"
               (list :project-dir "/tmp/hn" :nuked-at (current-time))
               agent-repl--workspaces)
      (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                 (lambda (ws _dir) (push ws established))))
        (should-not (agent-repl--hide-project-dirs--restore))
        (should-not established)))))

;;;; ---- Toggle command ----

(ert-deftest agent-repl-test-toggle-hide-project-dirs-on-calls-hide ()
  "Toggling ON flips the flag and routes to --hide, not --restore."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-project-dirs-enabled nil)
          (hide-called 0)
          (restore-called 0))
      (cl-letf (((symbol-function 'agent-repl--hide-project-dirs--hide)
                 (lambda () (cl-incf hide-called) nil))
                ((symbol-function 'agent-repl--hide-project-dirs--restore)
                 (lambda () (cl-incf restore-called) nil))
                ((symbol-function 'agent-repl-save-workspace-snapshot) #'ignore)
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl-drawer--refresh-if-visible) #'ignore))
        (agent-repl-toggle-hide-project-dirs)
        (should agent-repl-hide-project-dirs-enabled)
        (should (= hide-called 1))
        (should (= restore-called 0))))))

(ert-deftest agent-repl-test-toggle-hide-project-dirs-off-calls-restore ()
  "Toggling OFF flips the flag and routes to --restore, not --hide."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-project-dirs-enabled t)
          (hide-called 0)
          (restore-called 0))
      (cl-letf (((symbol-function 'agent-repl--hide-project-dirs--hide)
                 (lambda () (cl-incf hide-called) nil))
                ((symbol-function 'agent-repl--hide-project-dirs--restore)
                 (lambda () (cl-incf restore-called) nil))
                ((symbol-function 'agent-repl-save-workspace-snapshot) #'ignore)
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function 'agent-repl-drawer--refresh-if-visible) #'ignore))
        (agent-repl-toggle-hide-project-dirs)
        (should-not agent-repl-hide-project-dirs-enabled)
        (should (= restore-called 1))
        (should (= hide-called 0))))))

(ert-deftest agent-repl-test-toggle-hide-project-dirs-refreshes-ui ()
  "Toggle forces a tab-bar repaint, a drawer refresh, and persists the state."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-project-dirs-enabled nil)
          (redraw-called 0)
          (refresh-called 0)
          (persist-called 0))
      (cl-letf (((symbol-function 'agent-repl--hide-project-dirs--hide) #'ignore)
                ((symbol-function 'agent-repl-save-workspace-snapshot)
                 (lambda () (cl-incf persist-called)))
                ((symbol-function 'agent-repl--force-tab-bar-redraw)
                 (lambda () (cl-incf redraw-called)))
                ((symbol-function 'agent-repl-drawer--refresh-if-visible)
                 (lambda () (cl-incf refresh-called))))
        (agent-repl-toggle-hide-project-dirs)
        (should (= redraw-called 1))
        (should (= refresh-called 1))
        (should (= persist-called 1))))))

(ert-deftest agent-repl-test-toggle-hide-project-dirs-errors-during-snapshot-load ()
  "Toggle refuses to run while a snapshot load is in progress, leaving the flag."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-project-dirs-enabled nil)
          (agent-repl--snapshot-load-state '(:queue nil)))
      (cl-letf (((symbol-function 'agent-repl--hide-project-dirs--hide)
                 (lambda () (error "should not be called")))
                ((symbol-function 'agent-repl--hide-project-dirs--restore)
                 (lambda () (error "should not be called"))))
        (should-error (agent-repl-toggle-hide-project-dirs) :type 'user-error)
        (should-not agent-repl-hide-project-dirs-enabled)))))

(provide 'test-hide-project-dirs)
;;; test-hide-project-dirs.el ends here
