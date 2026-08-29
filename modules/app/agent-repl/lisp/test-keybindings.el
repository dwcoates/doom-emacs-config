;;; test-keybindings.el --- ERT tests for keybindings.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the keybinding helpers and utility commands defined in
;; keybindings.el.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-keybindings.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'json)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: agent-repl--kill-before-workspace-delete ----

(ert-deftest agent-repl-test-kill-before-workspace-delete-when-running ()
  "kill-before-workspace-delete should call agent-repl-kill when the agent is
running and the kill targets the current workspace (no NAME arg means the
implicit target is the current workspace)."
  (let ((killed nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
              ((symbol-function 'agent-repl-kill) (lambda () (setq killed t))))
      (agent-repl--kill-before-workspace-delete)
      (should killed))))

(ert-deftest agent-repl-test-kill-before-workspace-delete-when-not-running ()
  "kill-before-workspace-delete should not call agent-repl-kill when the agent is not running."
  (let ((killed nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--agent-running-p) (lambda () nil))
              ((symbol-function 'agent-repl-kill) (lambda () (setq killed t))))
      (agent-repl--kill-before-workspace-delete)
      (should-not killed))))

(ert-deftest agent-repl-test-kill-before-workspace-delete-name-eq-current ()
  "When NAME equals the current workspace, the advice fires the kill."
  (let ((killed nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
              ((symbol-function 'agent-repl-kill) (lambda () (setq killed t))))
      (agent-repl--kill-before-workspace-delete "current-ws")
      (should killed))))

(ert-deftest agent-repl-test-kill-before-workspace-delete-name-not-current ()
  "When NAME refers to a non-current workspace, the advice MUST NOT kill the
current workspace's session.  This guards against the cross-workspace bug
where `(+workspace/kill other-ws)' would otherwise tear down current's
running session via `agent-repl--agent-running-p' (which inspects the
current workspace, not NAME)."
  (let ((killed nil))
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--agent-running-p) (lambda () t))
              ((symbol-function 'agent-repl-kill) (lambda () (setq killed t))))
      (agent-repl--kill-before-workspace-delete "other-ws")
      (should-not killed))))

;;;; ---- Tests: agent-repl--read-workspace ----

(ert-deftest agent-repl-test-read-workspace-returns-match ()
  "read-workspace should return the value from completing-read."
  (cl-letf (((symbol-function 'agent-repl--ws-list-names) (lambda () '("test-ws")))
            ((symbol-function 'completing-read)
             (lambda (_prompt coll &rest _) (car coll))))
    (should (equal (agent-repl--read-workspace "Pick: ") "test-ws"))))

;;;; ---- Tests: agent-repl--read-workspace-with-default ----

(ert-deftest agent-repl-test-read-workspace-with-default ()
  "read-workspace-with-default should pass current workspace as default."
  (let ((captured-default nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll _pred _require _hist _hist-var default)
                 (setq captured-default default)
                 default))
              ((symbol-function '+workspace-current-name) (lambda () "current-ws")))
      (let ((result (agent-repl--read-workspace-with-default "Pick: ")))
        (should (equal captured-default "current-ws"))
        (should (equal result "current-ws"))))))

;;;; ---- Tests: agent-repl--read-known-workspace ----

(ert-deftest agent-repl-test-read-known-workspace-no-workspaces ()
  "read-known-workspace signals user-error when no workspaces are registered."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl--read-known-workspace "Pick: ") :type 'user-error)))

(ert-deftest agent-repl-test-read-known-workspace-defaults-to-current ()
  "read-known-workspace defaults to the current workspace when registered."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
    (let ((captured-default nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws2"))
                ((symbol-function 'completing-read)
                 (lambda (_p _c _pr _r _h _hv default)
                   (setq captured-default default)
                   default)))
        (agent-repl--read-known-workspace "Pick: ")
        (should (equal captured-default "ws2"))))))

(ert-deftest agent-repl-test-read-known-workspace-no-default-when-current-not-registered ()
  "read-known-workspace passes nil default when current workspace is not registered."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((captured-default 'sentinel))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "stranger"))
                ((symbol-function 'completing-read)
                 (lambda (_p _c _pr _r _h _hv default)
                   (setq captured-default default)
                   "ws1")))
        (agent-repl--read-known-workspace "Pick: ")
        (should-not captured-default)))))

;;;; ---- Tests: agent-repl--killable-workspace-names ----

(ert-deftest agent-repl-test-killable-workspace-names/union-live-and-tabbar ()
  "killable-workspace-names returns the union of live ws and tab-bar names.
Live entries appear before any tab-bar-only entries; tab-bar entries
that duplicate a live name are dropped.  Order WITHIN the live set is
not guaranteed (`hash-table-keys' is unordered), so the live block is
checked as a set rather than a positional sequence."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "live1" :project-dir "/tmp/live1")
    (agent-repl--ws-put "live2" :project-dir "/tmp/live2")
    (cl-letf (((symbol-function '+workspace-list-names)
               (lambda () '("live1" "tabbar-only" "live2" "stray"))))
      (let* ((result (agent-repl--killable-workspace-names))
             (live-prefix (cl-subseq result 0 2))
             (extras-suffix (cl-subseq result 2)))
        ;; The first 2 entries are exactly the live set (order-agnostic).
        (should (equal (sort (copy-sequence live-prefix) #'string<)
                       '("live1" "live2")))
        ;; Tab-bar-only entries follow, in tab-bar order, with live names removed.
        (should (equal extras-suffix '("tabbar-only" "stray")))
        ;; No duplicates of live names.
        (should (= 1 (cl-count "live1" result :test #'equal)))
        (should (= 1 (cl-count "live2" result :test #'equal)))))))

(ert-deftest agent-repl-test-killable-workspace-names/excludes-tombstoned ()
  "killable-workspace-names omits tombstoned agent-repl entries whose
persp is also gone from the tab-bar.  A tombstoned entry whose persp
still exists IS included (via the tab-bar branch)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "tomb-no-persp" :project-dir "/tmp/a")
    (agent-repl--ws-put "tomb-no-persp" :killed-at (current-time))
    (agent-repl--ws-put "tomb-with-persp" :project-dir "/tmp/b")
    (agent-repl--ws-put "tomb-with-persp" :killed-at (current-time))
    (cl-letf (((symbol-function '+workspace-list-names)
               (lambda () '("tomb-with-persp"))))
      (let ((result (agent-repl--killable-workspace-names)))
        (should-not (member "tomb-no-persp" result))
        (should (member "tomb-with-persp" result))))))

(ert-deftest agent-repl-test-killable-workspace-names/empty-when-nothing-registered ()
  "killable-workspace-names returns empty when there are no live or tab-bar ws."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names) (lambda () nil)))
      (should-not (agent-repl--killable-workspace-names)))))

;;;; ---- Tests: agent-repl--read-killable-workspace ----

(ert-deftest agent-repl-test-read-killable-workspace/no-candidates ()
  "read-killable-workspace signals user-error when no live or tab-bar ws exist."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names) (lambda () nil)))
      (should-error (agent-repl--read-killable-workspace "Pick: ")
                    :type 'user-error))))

(ert-deftest agent-repl-test-read-killable-workspace/includes-tabbar-only-ws ()
  "read-killable-workspace offers tab-bar-only ws in the completion list."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names)
               (lambda () '("stray-persp")))
              ((symbol-function '+workspace-current-name) (lambda () "main"))
              ((symbol-function 'completing-read)
               (lambda (_p coll &rest _) (car coll))))
      (should (equal (agent-repl--read-killable-workspace "Pick: ")
                     "stray-persp")))))

(ert-deftest agent-repl-test-read-killable-workspace/defaults-to-current-when-tabbar-only ()
  "read-killable-workspace defaults to current ws when it's in the tab-bar
even if it has no live agent-repl entry."
  (agent-repl-test--with-clean-state
    (let ((captured-default nil))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("other" "current-persp")))
                ((symbol-function '+workspace-current-name)
                 (lambda () "current-persp"))
                ((symbol-function 'completing-read)
                 (lambda (_p _c _pr _r _h _hv default)
                   (setq captured-default default)
                   default)))
        (agent-repl--read-killable-workspace "Pick: ")
        (should (equal captured-default "current-persp"))))))

;;;; ---- Tests: agent-repl--kill-or-close-workspace ----

(ert-deftest agent-repl-test-kill-or-close-workspace/live-ws-runs-kill ()
  "kill-or-close-workspace runs the full kill teardown for a live ws."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "live" :project-dir "/tmp/live")
    (let ((killed nil)
          (persp-killed nil))
      (cl-letf (((symbol-function 'agent-repl--kill-one-workspace)
                 (lambda (ws &optional _preserve) (setq killed ws)))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws))))
        (let ((result (agent-repl--kill-or-close-workspace "live")))
          (should (eq result 'kill))
          (should (equal killed "live"))
          (should-not persp-killed))))))

(ert-deftest agent-repl-test-kill-or-close-workspace/tombstoned-ws-runs-persp-kill ()
  "kill-or-close-workspace falls back to +workspace/kill for a tombstoned ws
whose persp still exists.  MUST NOT call --kill-one-workspace — there
is no live agent-repl session to tear down."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "tomb" :project-dir "/tmp/tomb")
    (agent-repl--ws-put "tomb" :killed-at (current-time))
    (let ((killed nil)
          (persp-killed nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'agent-repl--kill-one-workspace)
                 (lambda (ws &optional _preserve) (setq killed ws)))
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws))))
        (let ((result (agent-repl--kill-or-close-workspace "tomb")))
          (should (eq result 'close))
          (should (equal persp-killed "tomb"))
          (should-not killed))))))

(ert-deftest agent-repl-test-kill-or-close-workspace/never-registered-ws-runs-persp-kill ()
  "kill-or-close-workspace handles a persp that was never agent-repl-registered.
Routes through +workspace/kill (no live entry, nothing to kill)."
  (agent-repl-test--with-clean-state
    (let ((killed nil)
          (persp-killed nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'agent-repl--kill-one-workspace)
                 (lambda (ws &optional _preserve) (setq killed ws)))
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws))))
        (let ((result (agent-repl--kill-or-close-workspace "never-known")))
          (should (eq result 'close))
          (should (equal persp-killed "never-known"))
          (should-not killed))))))

(ert-deftest agent-repl-test-kill-or-close-workspace/skips-persp-kill-when-persp-gone ()
  "kill-or-close-workspace MUST NOT call +workspace/kill when the persp is
already missing from the cache — that would emit the spurious
`'<ws>' workspace doesn't exist' warning in the echo area."
  (agent-repl-test--with-clean-state
    (let ((persp-killed nil)
          (persp-mode t))
      (cl-letf (((symbol-function '+workspace-exists-p) (lambda (_n) nil))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws))))
        (let ((result (agent-repl--kill-or-close-workspace "ghost")))
          (should (eq result 'close))
          (should-not persp-killed))))))

;;;; ---- Tests: agent-repl-set-priority ----

(ert-deftest agent-repl-test-set-priority-stores-value ()
  "set-priority should store the priority in workspace state."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (agent-repl-set-priority "p1")
      (should (equal (agent-repl--ws-get "ws1" :priority) "p1")))))

(ert-deftest agent-repl-test-set-priority-clears-on-empty ()
  "set-priority with empty string should clear the priority."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (agent-repl-set-priority "p2")
      (should (equal (agent-repl--ws-get "ws1" :priority) "p2"))
      (agent-repl-set-priority "")
      (should-not (agent-repl--ws-get "ws1" :priority)))))

(ert-deftest agent-repl-test-set-priority-messages ()
  "set-priority should display a message with the new priority."
  (agent-repl-test--with-clean-state
    (let ((msg nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'message) (lambda (fmt &rest args)
                                              (setq msg (apply #'format fmt args)))))
        (agent-repl-set-priority "p3")
        (should (string-match-p "p3" msg))
        (agent-repl-set-priority "")
        (should (string-match-p "cleared" msg))))))

;;;; ---- Tests: agent-repl-revert-and-eval-buffer ----

(ert-deftest agent-repl-test-revert-and-eval-buffer ()
  "revert-and-eval-buffer should call revert-buffer then eval-buffer."
  (let ((call-order nil))
    (cl-letf (((symbol-function 'revert-buffer)
               (lambda (&rest _) (push 'revert call-order)))
              ((symbol-function 'eval-buffer)
               (lambda (&rest _) (push 'eval call-order))))
      (agent-repl-revert-and-eval-buffer)
      ;; Order is reversed because we use push
      (should (equal call-order '(eval revert))))))

;;;; ---- Tests: agent-repl-reload-config ----

(ert-deftest agent-repl-test-reload-config-falls-back-when-no-ws ()
  "When the current workspace is unknown (nil), reload uses
`agent-repl--config-file' (the original load path)."
  (let ((loaded-file nil)
        (agent-repl--config-file "/tmp/fake/agent-repl/config.el"))
    (cl-letf (((symbol-function 'load-file)
               (lambda (f) (setq loaded-file f)))
              ((symbol-function '+workspace-current-name) (lambda () nil)))
      (agent-repl-reload-config)
      (should (equal loaded-file "/tmp/fake/agent-repl/config.el")))))

(ert-deftest agent-repl-test-reload-config-uses-ws-project-dir-when-config-exists ()
  "When the current workspace's `:project-dir' contains a
`modules/app/agent-repl/config.el', reload uses THAT path so a doom-config
worktree picks up its own checkout instead of the originally-loaded copy."
  (agent-repl-test--with-clean-state
    (let* ((tmp-root (make-temp-file "agent-repl-reload-test-" t))
           (config-rel "modules/app/agent-repl/config.el")
           (config-abs (expand-file-name config-rel tmp-root))
           (loaded-file nil)
           (agent-repl--config-file "/tmp/orig/config.el"))
      (unwind-protect
          (progn
            (make-directory (file-name-directory config-abs) t)
            (with-temp-file config-abs (insert ";; stub\n"))
            (agent-repl--ws-put "ws1" :project-dir tmp-root)
            (cl-letf (((symbol-function 'load-file)
                       (lambda (f) (setq loaded-file f)))
                      ((symbol-function '+workspace-current-name) (lambda () "ws1")))
              (agent-repl-reload-config)
              (should (equal loaded-file config-abs))))
        (delete-directory tmp-root t)))))

(ert-deftest agent-repl-test-reload-config-falls-back-when-ws-not-doom-config ()
  "When the current workspace's `:project-dir' is a real directory but
contains no `modules/app/agent-repl/config.el', reload falls back to
`agent-repl--config-file'."
  (agent-repl-test--with-clean-state
    (let* ((tmp-root (make-temp-file "agent-repl-reload-test-" t))
           (loaded-file nil)
           (agent-repl--config-file "/tmp/orig/config.el"))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir tmp-root)
            (cl-letf (((symbol-function 'load-file)
                       (lambda (f) (setq loaded-file f)))
                      ((symbol-function '+workspace-current-name) (lambda () "ws1")))
              (agent-repl-reload-config)
              (should (equal loaded-file "/tmp/orig/config.el"))))
        (delete-directory tmp-root t)))))

(ert-deftest agent-repl-test-reload-config-falls-back-when-no-project-dir ()
  "When the current workspace exists in the registry but has no
`:project-dir', reload falls back to `agent-repl--config-file'."
  (agent-repl-test--with-clean-state
    (let ((loaded-file nil)
          (agent-repl--config-file "/tmp/orig/config.el"))
      (agent-repl--ws-put "ws1" :some-other-key "value")
      (cl-letf (((symbol-function 'load-file)
                 (lambda (f) (setq loaded-file f)))
                ((symbol-function '+workspace-current-name) (lambda () "ws1")))
        (agent-repl-reload-config)
        (should (equal loaded-file "/tmp/orig/config.el"))))))

(ert-deftest agent-repl-test-decorate-priority-candidate-uses-image-display ()
  "decorate-priority-candidate returns a string whose `display' property
is the image spec itself (not a wrapper string), so completion
frameworks render the glyph in place of the textual key."
  (let* ((image-spec '(image :type png :data "fake"))
         (agent-repl--priority-images `(("p1" . ,image-spec)))
         (candidate (agent-repl--decorate-priority-candidate "p1"))
         (display (get-text-property 0 'display candidate)))
    (should (equal candidate "p1"))
    (should (eq (car-safe display) 'image))
    (should (eq display image-spec))))

(ert-deftest agent-repl-test-decorate-priority-candidate-fallback-when-no-image ()
  "decorate-priority-candidate returns the input unchanged when no image
is registered, so the prompt remains usable in image-less builds."
  (let ((agent-repl--priority-images nil))
    (let ((candidate (agent-repl--decorate-priority-candidate "p1")))
      (should (equal candidate "p1"))
      (should-not (get-text-property 0 'display candidate)))))

(ert-deftest agent-repl-test-read-priority-presents-remove-label-when-current-priority ()
  "read-priority appends the textual remove label as the last candidate
when DEFAULT is a real priority — i.e. the workspace already has
something to remove.  Bare empty string never appears in the
collection (it can't carry a `display' property)."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq captured-args args)
                 "p2")))
      (agent-repl--read-priority "Priority: " "p1")
      (let ((collection (nth 1 captured-args)))
        (should (member agent-repl--priority-remove-label collection))
        (should-not (member "" collection))))))

(ert-deftest agent-repl-test-read-priority-omits-remove-label-when-no-current-priority ()
  "read-priority omits the remove label when DEFAULT is empty or nil —
there is nothing to remove on a workspace that has no priority set."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq captured-args args)
                 "p1")))
      (agent-repl--read-priority "Priority: " "")
      (let ((collection (nth 1 captured-args)))
        (should-not (member agent-repl--priority-remove-label collection))))))

(ert-deftest agent-repl-test-read-priority-omits-remove-label-when-default-nil ()
  "read-priority omits the remove label when DEFAULT is nil — same
reasoning as the empty-string case, just the alternate spelling
callers may pass."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq captured-args args)
                 "p1")))
      (agent-repl--read-priority "Priority: " nil)
      (let ((collection (nth 1 captured-args)))
        (should-not (member agent-repl--priority-remove-label collection))))))

(ert-deftest agent-repl-test-read-priority-no-default-when-no-current-priority ()
  "When DEFAULT is empty, no priority is preselected — the user is
setting for the first time and there is no obvious default."
  (let ((captured-default 'unset))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq captured-default (nth 6 args))
                 "p1")))
      (agent-repl--read-priority "Priority: " "")
      (should (null captured-default)))))

(ert-deftest agent-repl-test-read-priority-default-passed-when-current-priority ()
  "When DEFAULT is a non-empty priority, it is forwarded to
completing-read so the existing priority is preselected."
  (let ((captured-default nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq captured-default (nth 6 args))
                 "p2")))
      (agent-repl--read-priority "Priority: " "p1")
      (should (equal captured-default "p1")))))

(ert-deftest agent-repl-test-read-priority-remove-label-maps-to-empty ()
  "Picking the remove label round-trips back to \"\" for the caller, so
downstream `string-empty-p' checks still detect the clear case."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) agent-repl--priority-remove-label)))
    (should (equal (agent-repl--read-priority "Priority: " "p1") ""))))

(ert-deftest agent-repl-test-read-priority-strips-text-properties ()
  "read-priority's return value is a plain string with no text properties
so callers don't accidentally persist image-display metadata into the
workspace plist."
  (let ((decorated (propertize "p1" 'display "fake-image")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) decorated)))
      (let ((result (agent-repl--read-priority "Priority: " "")))
        (should (equal result "p1"))
        (should-not (text-properties-at 0 result))))))

(ert-deftest agent-repl-test-read-priority-includes-all-priority-levels ()
  "read-priority offers every entry in `agent-repl-priority-levels' as
a candidate, with their original string content preserved (the image
is added via `display' property, not via key substitution)."
  (let ((captured-collection nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq captured-collection (nth 1 args))
                 "p1")))
      (agent-repl--read-priority "Priority: " "")
      (dolist (p agent-repl-priority-levels)
        (should (cl-find p captured-collection :test #'equal))))))

(ert-deftest agent-repl-test-set-priority-interactive-skips-ws-prompt ()
  "Interactive set-priority should NOT prompt for a workspace; it must always
target the current workspace.  Regression: an earlier iteration prompted
for both ws and priority via two completing-read calls, which slowed the
common case (`SPC j m p' on the focused ws).  This test mocks
`completing-read' so any second invocation would be observable, then
asserts only one happened."
  (agent-repl-test--with-clean-state
    (let ((completing-read-calls 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
                ((symbol-function 'agent-repl--state-save) (lambda (_) nil))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority) (lambda (_) nil))
                ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
                ((symbol-function 'message) (lambda (&rest _) nil))
                ((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (cl-incf completing-read-calls)
                   "p1")))
        (call-interactively #'agent-repl-set-priority)
        (should (= 1 completing-read-calls))
        (should (equal (agent-repl--ws-get "current-ws" :priority) "p1"))))))

(ert-deftest agent-repl-test-set-priority-persists-to-state ()
  "set-priority calls state-save so the badge survives restarts."
  (agent-repl-test--with-clean-state
    (let ((saved-ws nil))
      (cl-letf (((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq saved-ws ws)))
                ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (agent-repl-set-priority "p1")
        (should (equal (agent-repl--ws-get (+workspace-current-name) :priority) "p1"))
        (should (equal saved-ws (+workspace-current-name)))))))

(ert-deftest agent-repl-test-set-priority-clears-and-persists ()
  "Clearing priority (empty string) nils the plist field and still persists."
  (agent-repl-test--with-clean-state
    (let ((saved-ws nil))
      (agent-repl--ws-put (+workspace-current-name) :priority "p2")
      (cl-letf (((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq saved-ws ws)))
                ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (agent-repl-set-priority "")
        (should (null (agent-repl--ws-get (+workspace-current-name) :priority)))
        (should (equal saved-ws (+workspace-current-name)))))))

(ert-deftest agent-repl-test-set-priority-targets-explicit-ws ()
  "set-priority writes to the WS argument, not the current workspace."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
              ((symbol-function 'agent-repl--state-save) (lambda (_) nil))
              ((symbol-function 'agent-repl--reorder-workspace-by-priority) (lambda (_) nil))
              ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl-set-priority "p2" "other-ws")
      (should (equal (agent-repl--ws-get "other-ws" :priority) "p2"))
      (should-not (agent-repl--ws-get "current-ws" :priority)))))

(ert-deftest agent-repl-test-set-priority-state-save-uses-target-ws ()
  "set-priority persists state for the explicit WS target, not the current ws."
  (agent-repl-test--with-clean-state
    (let ((saved-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
                ((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq saved-ws ws)))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority) (lambda (_) nil))
                ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (agent-repl-set-priority "p1" "other-ws")
        (should (equal saved-ws "other-ws"))))))

(ert-deftest agent-repl-test-set-priority-reorders-tab-bar ()
  "set-priority calls reorder-workspace-by-priority so the tab-bar reflects the new rank."
  (agent-repl-test--with-clean-state
    (let ((reordered-ws nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--state-save) (lambda (_) nil))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (ws) (setq reordered-ws ws)))
                ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (agent-repl-set-priority "p1")
        (should (equal reordered-ws "ws1"))))))

(ert-deftest agent-repl-test-set-priority-logs-old-to-new-transition ()
  "set-priority logs the old -> new priority transition."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :priority "p2")
    (let ((logs nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--state-save) (lambda (_) nil))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority) (lambda (_) nil))
                ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
                ((symbol-function 'message) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (agent-repl-set-priority "p1")
        (should (cl-find-if (lambda (l)
                              (and (string-match-p "set-priority:" l)
                                   (string-match-p "p2 -> p1" l)))
                            logs))))))

(ert-deftest agent-repl-test-set-priority-logs-explicit-ws-flag ()
  "set-priority logs ws-explicit=t when called with an explicit WS argument."
  (agent-repl-test--with-clean-state
    (let ((logs nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
                ((symbol-function 'agent-repl--state-save) (lambda (_) nil))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority) (lambda (_) nil))
                ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
                ((symbol-function 'message) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (agent-repl-set-priority "p1" "other-ws")
        (should (cl-find-if (lambda (l) (string-match-p "ws-explicit=t" l)) logs))))))

(ert-deftest agent-repl-test-set-priority-logs-fallback-flag ()
  "set-priority logs ws-explicit=nil when WS defaults to the current workspace."
  (agent-repl-test--with-clean-state
    (let ((logs nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "current-ws"))
                ((symbol-function 'agent-repl--state-save) (lambda (_) nil))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority) (lambda (_) nil))
                ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
                ((symbol-function 'message) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--log)
                 (lambda (_ws fmt &rest args)
                   (push (apply #'format fmt args) logs))))
        (agent-repl-set-priority "p1")
        (should (cl-find-if (lambda (l) (string-match-p "ws-explicit=nil" l)) logs))))))

(ert-deftest agent-repl-test-set-priority-changes-existing-priority ()
  "set-priority overwrites a previously set priority on the same workspace."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--state-save) (lambda (_) nil))
              ((symbol-function 'agent-repl--reorder-workspace-by-priority) (lambda (_) nil))
              ((symbol-function 'force-mode-line-update) (lambda (&rest _) nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl-set-priority "p3")
      (should (equal (agent-repl--ws-get "ws1" :priority) "p3"))
      (agent-repl-set-priority "p1")
      (should (equal (agent-repl--ws-get "ws1" :priority) "p1")))))

;;;; ---- Tests: scroll-output-intercept-states (shared by surviving overrides) ----

;;; `agent-repl--scroll-output-intercept-states' was introduced for the
;;; now-removed `C-S-j' / `C-S-k' scroll-output chords (and their vterm
;;; shadow-key stripping); both are gone along with vterm.  The list
;;; survives because `agent-repl--install-workspace-jump-overrides'
;;; reuses it as-is, so a chord installed through it still needs to
;;; win lookup across every evil state.

(ert-deftest agent-repl-test-scroll-intercept-states-covers-normal-visual ()
  "`agent-repl--scroll-output-intercept-states' must include `normal'
and `visual' — the two states where `config.el's `:nv \"C-j\"' /
`:nv \"C-k\"' window-nav intercept aux maps live (the source of the
shift-translation shadow the surviving override installers defeat)."
  (should (memq 'normal agent-repl--scroll-output-intercept-states))
  (should (memq 'visual agent-repl--scroll-output-intercept-states)))

(ert-deftest agent-repl-test-scroll-intercept-states-covers-all-evil-states ()
  "Sanity: the intercept state list must cover every evil state so a
chord installed through it works regardless of which state is current.
A future trim that drops a state would silently re-break that chord there."
  (dolist (state '(normal visual insert emacs operator motion replace))
    (should (memq state agent-repl--scroll-output-intercept-states))))

;;;; ---- Tests: workspace-jump override install ----

;;; `M-1..M-9 / M-0' and `s-1..s-9 / s-0' must win lookup above:
;;;
;;;   - Doom default's `:n "s-9" -> +workspace/switch-to-final' in
;;;     `evil-normal-state-map' (last-workspace bug from normal state).
;;;   - Doom default's `"s-0" -> doom/reset-font-size' (font-resize bug).
;;;
;;; A plain `(map! :g ... )' global-map entry loses to both; the
;;; intercept-aux install is what wins.

(ert-deftest agent-repl-test-workspace-jump-chords-cover-mod-and-super-digits ()
  "`agent-repl--workspace-jump-chords' must enumerate the full 0-9 grid
across BOTH `M-' (Option/Meta) and `s-' (Cmd/Super).  Command `s-1..s-9'
map to the FIRST nine (`switch-to-0'..`switch-to-8'); Option `M-1..M-9'
map to the SECOND nine (`switch-to-9'..`switch-to-17'); `M-0'/`s-0' map
to `switch-to-final'.  Anything less leaves gaps that fall through to
whatever Doom's own defaults bound."
  (let ((expected
         '(("M-1" . agent-repl-workspace-switch-to-9)
           ("M-2" . agent-repl-workspace-switch-to-10)
           ("M-3" . agent-repl-workspace-switch-to-11)
           ("M-4" . agent-repl-workspace-switch-to-12)
           ("M-5" . agent-repl-workspace-switch-to-13)
           ("M-6" . agent-repl-workspace-switch-to-14)
           ("M-7" . agent-repl-workspace-switch-to-15)
           ("M-8" . agent-repl-workspace-switch-to-16)
           ("M-9" . agent-repl-workspace-switch-to-17)
           ("M-0" . agent-repl-workspace-switch-to-final)
           ("s-1" . agent-repl-workspace-switch-to-0)
           ("s-2" . agent-repl-workspace-switch-to-1)
           ("s-3" . agent-repl-workspace-switch-to-2)
           ("s-4" . agent-repl-workspace-switch-to-3)
           ("s-5" . agent-repl-workspace-switch-to-4)
           ("s-6" . agent-repl-workspace-switch-to-5)
           ("s-7" . agent-repl-workspace-switch-to-6)
           ("s-8" . agent-repl-workspace-switch-to-7)
           ("s-9" . agent-repl-workspace-switch-to-8)
           ("s-0" . agent-repl-workspace-switch-to-final))))
    (dolist (pair expected)
      (should (equal pair
                     (assoc (car pair) agent-repl--workspace-jump-chords))))))

(ert-deftest agent-repl-test-workspace-jump-s-9-routes-to-ninth-not-final ()
  "Regression: `s-9' (Cmd+9) must map to `agent-repl-workspace-switch-to-8'
\(NINTH workspace), not `+workspace/switch-to-final' or
`agent-repl-workspace-switch-to-final' (LAST workspace).  Symptom of
the regression was Cmd+9 landing on the last workspace from normal
state because Doom's `:n s-9' default leaked through."
  (should (eq (cdr (assoc "s-9" agent-repl--workspace-jump-chords))
              'agent-repl-workspace-switch-to-8)))

(ert-deftest agent-repl-test-workspace-jump-s-0-routes-to-final-not-font-resize ()
  "Regression: `s-0' (Cmd+0) must map to `agent-repl-workspace-switch-to-final',
not `doom/reset-font-size'.  Symptom of the regression was Cmd+0
emitting \"The font hasn't been resized\" because Doom's global
`s-0 -> doom/reset-font-size' default leaked through."
  (should (eq (cdr (assoc "s-0" agent-repl--workspace-jump-chords))
              'agent-repl-workspace-switch-to-final)))

(ert-deftest agent-repl-test-workspace-jump-m-1-routes-to-second-nine-first ()
  "`M-1' (Option+1) must map to `agent-repl-workspace-switch-to-9' (the
10th workspace / first of the SECOND nine), NOT `switch-to-0' (the
first workspace).  Guards the Option row addressing the second nine."
  (should (eq (cdr (assoc "M-1" agent-repl--workspace-jump-chords))
              'agent-repl-workspace-switch-to-9)))

(ert-deftest agent-repl-test-workspace-jump-m-9-routes-to-second-nine-last ()
  "`M-9' (Option+9) must map to `agent-repl-workspace-switch-to-17' (the
18th workspace / last of the SECOND nine), NOT `switch-to-8' (the
9th workspace) or `switch-to-final'.  Guards that the second nine tops
out at workspace 18, not the final workspace."
  (should (eq (cdr (assoc "M-9" agent-repl--workspace-jump-chords))
              'agent-repl-workspace-switch-to-17)))

(ert-deftest agent-repl-test-workspace-jump-s-1-routes-to-first-nine-first ()
  "`s-1' (Cmd+1) must still map to `agent-repl-workspace-switch-to-0'
(the first workspace) after the Option row moved to the second nine —
the Command row stays on the FIRST nine."
  (should (eq (cdr (assoc "s-1" agent-repl--workspace-jump-chords))
              'agent-repl-workspace-switch-to-0)))

(ert-deftest agent-repl-test-install-workspace-jump-installs-top-level ()
  "`--install-workspace-jump-overrides' must populate `general-override-mode-map'
at top level so the chords work in non-evil contexts and win above
any other minor-mode-map binding."
  (let ((general-override-mode-map (make-sparse-keymap)))
    (cl-letf (((symbol-function 'evil-get-auxiliary-keymap)
               (lambda (&rest _) (make-sparse-keymap))))
      (agent-repl--install-workspace-jump-overrides))
    (dolist (entry agent-repl--workspace-jump-chords)
      (should (eq (lookup-key general-override-mode-map (kbd (car entry)))
                  (cdr entry))))))

(ert-deftest agent-repl-test-install-workspace-jump-installs-intercept-aux ()
  "`--install-workspace-jump-overrides' must populate the evil intercept
aux map of `general-override-mode-map' for every state in
`agent-repl--scroll-output-intercept-states' -- this is what beats
Doom default's `:n s-9' (normal-state-map) binding, regardless of
which evil state is current."
  (let* ((general-override-mode-map (make-sparse-keymap))
         (aux-maps nil))
    (cl-letf (((symbol-function 'evil-get-auxiliary-keymap)
               (lambda (_keymap state &rest _)
                 (or (cdr (assq state aux-maps))
                     (let ((m (make-sparse-keymap)))
                       (push (cons state m) aux-maps)
                       m)))))
      (agent-repl--install-workspace-jump-overrides))
    (dolist (state agent-repl--scroll-output-intercept-states)
      (let ((aux (cdr (assq state aux-maps))))
        (should aux)
        (dolist (entry agent-repl--workspace-jump-chords)
          (should (eq (lookup-key aux (kbd (car entry)))
                      (cdr entry))))))))

(ert-deftest agent-repl-test-install-workspace-jump-skips-aux-without-evil ()
  "When `evil-get-auxiliary-keymap' is unbound (evil not loaded),
`--install-workspace-jump-overrides' must still install the top-level
binding without erroring."
  (let ((general-override-mode-map (make-sparse-keymap)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (not (eq sym 'evil-get-auxiliary-keymap)))))
      (agent-repl--install-workspace-jump-overrides))
    (dolist (entry agent-repl--workspace-jump-chords)
      (should (eq (lookup-key general-override-mode-map (kbd (car entry)))
                  (cdr entry))))))

(ert-deftest agent-repl-test-install-workspace-jump-is-idempotent ()
  "`--install-workspace-jump-overrides' must be idempotent -- the
merge-sentinel reload triggers re-load of `keybindings.el', so the
installer runs every reload.  Running it twice must leave the same
final bindings, not error and not duplicate state."
  (let ((general-override-mode-map (make-sparse-keymap)))
    (cl-letf (((symbol-function 'evil-get-auxiliary-keymap)
               (lambda (&rest _) (make-sparse-keymap))))
      (agent-repl--install-workspace-jump-overrides)
      (agent-repl--install-workspace-jump-overrides))
    (dolist (entry agent-repl--workspace-jump-chords)
      (should (eq (lookup-key general-override-mode-map (kbd (car entry)))
                  (cdr entry))))))


;;;; ---- Tests: the SPC j log-verbosity bindings ----
;;
;; `map!' is a no-op stub in batch, so a leader binding is not observable
;; through any keymap here.  The source text is: these assert that SPC j
;; D / L / V still name the three restored commands, which is exactly the
;; regression the dead `agent-repl-debug/*' family caused when it took
;; those keys with it.

(defconst agent-repl-test--keybindings-file
  (expand-file-name "keybindings.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path of keybindings.el, captured at LOAD time.
`load-file-name' is nil inside an ERT body, so the path cannot be resolved
when the test runs — only while this file is being loaded.")

(defun agent-repl-test--keybindings-source ()
  "Return the text of keybindings.el, for asserting on leader bindings."
  (with-temp-buffer
    (insert-file-contents agent-repl-test--keybindings-file)
    (buffer-string)))

(ert-deftest agent-repl-test-spc-j-d-binds-toggle-debug ()
  "SPC j D names `agent-repl-toggle-debug'."
  (should (string-match-p "\"D\" #'agent-repl-toggle-debug"
                          (agent-repl-test--keybindings-source))))

(ert-deftest agent-repl-test-spc-j-l-binds-set-log-file-level ()
  "SPC j L names `agent-repl-set-log-file-level'."
  (should (string-match-p "\"L\" #'agent-repl-set-log-file-level"
                          (agent-repl-test--keybindings-source))))

(ert-deftest agent-repl-test-spc-j-v-binds-toggle-verbose-to-disk ()
  "SPC j V names `agent-repl-toggle-verbose-to-disk'."
  (should (string-match-p "\"V\" #'agent-repl-toggle-verbose-to-disk"
                          (agent-repl-test--keybindings-source))))

(ert-deftest agent-repl-test-log-verbosity-bindings-name-no-debug-prefix ()
  "No `agent-repl-debug/' command survives in any binding."
  (should-not (string-match-p "agent-repl-debug/"
                              (agent-repl-test--keybindings-source))))
