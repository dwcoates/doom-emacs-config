;;; test-commands.el --- ERT tests for agent-repl commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for commands.el — user commands, file references, diff analysis,
;; and standalone interactive commands.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-commands.el -f ert-run-tests-batch-and-exit

;;; Code:

(defvar recentf-list)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- agent-repl--buffer-relative-path ----

(ert-deftest agent-repl-cmd-test-buffer-relative-path/non-file-buffer ()
  "buffer-relative-path signals user-error for non-file buffers."
  (agent-repl-test--with-temp-buffer " *test-no-file*"
    (should-error (agent-repl--buffer-relative-path) :type 'user-error)))

(ert-deftest agent-repl-cmd-test-buffer-relative-path/file-buffer ()
  "buffer-relative-path returns path relative to project root."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/src/foo.el"))
            ((symbol-function 'agent-repl--ws-dir)
             (lambda (_ws) "/project/"))
            ((symbol-function '+workspace-current-name)
             (lambda () "test-ws")))
    (should (equal (agent-repl--buffer-relative-path) "src/foo.el"))))

(ert-deftest agent-repl-cmd-test-buffer-relative-path/nested-subdir ()
  "buffer-relative-path works for deeply nested paths."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/a/b/c/deep.el"))
            ((symbol-function 'agent-repl--ws-dir)
             (lambda (_ws) "/project/"))
            ((symbol-function '+workspace-current-name)
             (lambda () "test-ws")))
    (should (equal (agent-repl--buffer-relative-path) "a/b/c/deep.el"))))

(ert-deftest agent-repl-cmd-test-buffer-relative-path/file-at-root ()
  "buffer-relative-path returns bare filename when file is at project root."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/file.el"))
            ((symbol-function 'agent-repl--ws-dir)
             (lambda (_ws) "/project/"))
            ((symbol-function '+workspace-current-name)
             (lambda () "test-ws")))
    (should (equal (agent-repl--buffer-relative-path) "file.el"))))

(ert-deftest agent-repl-cmd-test-buffer-relative-path/root-without-trailing-slash ()
  "buffer-relative-path works when ws-dir omits trailing slash."
  (cl-letf (((symbol-function 'buffer-file-name)
             (lambda (&optional _buf) "/project/src/bar.el"))
            ((symbol-function 'agent-repl--ws-dir)
             (lambda (_ws) "/project"))
            ((symbol-function '+workspace-current-name)
             (lambda () "test-ws")))
    (should (equal (agent-repl--buffer-relative-path) "src/bar.el"))))

;;;; ---- agent-repl--select-line-range ----

(ert-deftest agent-repl-cmd-test-select-line-range/single-line ()
  "With END-LINE omitted, the region spans exactly START-LINE."
  (with-temp-buffer
    (insert "l1\nl2\nl3\nl4\nl5\n")
    (agent-repl--select-line-range 3)
    (should (= (line-number-at-pos (region-beginning)) 3))
    (should (= (line-number-at-pos (region-end)) 3))))

(ert-deftest agent-repl-cmd-test-select-line-range/multi-line ()
  "A START..END range marks every line in the inclusive span."
  (with-temp-buffer
    (insert "l1\nl2\nl3\nl4\nl5\n")
    (agent-repl--select-line-range 2 4)
    (should (= (line-number-at-pos (region-beginning)) 2))
    (should (= (line-number-at-pos (region-end)) 4))))

(ert-deftest agent-repl-cmd-test-select-line-range/region-covers-full-lines ()
  "The region runs from START-LINE's bol to END-LINE's eol."
  (with-temp-buffer
    (insert "aaa\nbbb\nccc\nddd\n")
    (agent-repl--select-line-range 2 3)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "bbb\nccc"))))

(ert-deftest agent-repl-cmd-test-select-line-range/end-before-start-clamps ()
  "An END-LINE below START-LINE is clamped up to START-LINE (single line)."
  (with-temp-buffer
    (insert "l1\nl2\nl3\nl4\n")
    (agent-repl--select-line-range 3 1)
    (should (= (line-number-at-pos (region-beginning)) 3))
    (should (= (line-number-at-pos (region-end)) 3))))

(ert-deftest agent-repl-cmd-test-select-line-range/start-below-one-floors ()
  "A START-LINE below 1 is floored to the first line."
  (with-temp-buffer
    (insert "l1\nl2\nl3\n")
    (agent-repl--select-line-range 0)
    (should (= (line-number-at-pos (region-beginning)) 1))))

(ert-deftest agent-repl-cmd-test-select-line-range/point-at-start ()
  "Point is left at the beginning of START-LINE after selection."
  (with-temp-buffer
    (insert "l1\nl2\nl3\nl4\n")
    (agent-repl--select-line-range 2 4)
    (should (= (line-number-at-pos (point)) 2))
    (should (= (point) (line-beginning-position)))))

(ert-deftest agent-repl-cmd-test-select-line-range/widens-narrowed-buffer ()
  "A narrowed buffer is widened so out-of-restriction lines are reachable."
  (with-temp-buffer
    (insert "l1\nl2\nl3\nl4\nl5\n")
    (narrow-to-region (point-min) (progn (goto-char (point-min)) (line-end-position)))
    (agent-repl--select-line-range 4)
    (should (= (line-number-at-pos (region-beginning)) 4))))

;;;; ---- agent-repl-link-code ----

(ert-deftest agent-repl-cmd-test-link-code/opens-file-and-selects-range ()
  "link-code visits FILE and activates the requested line range in it."
  (let ((tmp (make-temp-file "agent-repl-link-" nil ".txt"
                             "one\ntwo\nthree\nfour\nfive\n")))
    (unwind-protect
        (progn
          (agent-repl-link-code tmp 2 4)
          (let ((buf (get-file-buffer tmp)))
            (should buf)
            (with-current-buffer buf
              (should (= (line-number-at-pos (region-beginning)) 2))
              (should (= (line-number-at-pos (region-end)) 4)))))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (delete-file tmp))))

(ert-deftest agent-repl-cmd-test-link-code/returns-a-window ()
  "link-code returns the window the file was displayed in."
  (let ((tmp (make-temp-file "agent-repl-link-" nil ".txt" "a\nb\nc\n")))
    (unwind-protect
        (should (window-live-p (agent-repl-link-code tmp 1)))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (delete-file tmp))))

(ert-deftest agent-repl-cmd-test-link-code/with-workspace-selects-line-range ()
  "link-code with WORKSPACE selects the line range inside the buffer."
  (let ((tmp (make-temp-file "agent-repl-link-" nil ".txt"
                             "one\ntwo\nthree\nfour\nfive\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) nil))
                  ((symbol-function 'agent-repl--ws-add-buffer) #'ignore))
          (agent-repl-link-code tmp 2 4 "my-ws")
          (let ((buf (get-file-buffer tmp)))
            (should buf)
            (with-current-buffer buf
              (should (= (line-number-at-pos (region-beginning)) 2))
              (should (= (line-number-at-pos (region-end)) 4)))))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (delete-file tmp))))

(ert-deftest agent-repl-cmd-test-link-code/with-workspace-adds-buffer-to-persp ()
  "link-code with WORKSPACE calls ws-add-buffer with the resolved persp."
  (let ((tmp (make-temp-file "agent-repl-link-" nil ".txt" "a\nb\nc\n"))
        (fake-persp (list 'fake-persp))
        (add-buffer-calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp)
                   (lambda (_ws) fake-persp))
                  ((symbol-function 'agent-repl--ws-add-buffer)
                   (lambda (buf persp switch)
                     (push (list buf persp switch) add-buffer-calls))))
          (agent-repl-link-code tmp 1 nil "my-ws")
          (should (= (length add-buffer-calls) 1))
          (should (eq (nth 1 (car add-buffer-calls)) fake-persp))
          (should (null (nth 2 (car add-buffer-calls)))))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (delete-file tmp))))

(ert-deftest agent-repl-cmd-test-link-code/with-workspace-returns-window ()
  "link-code with WORKSPACE returns the window the buffer was opened in."
  (let ((tmp (make-temp-file "agent-repl-link-" nil ".txt" "a\nb\nc\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) nil))
                  ((symbol-function 'agent-repl--ws-add-buffer) #'ignore))
          (let ((result (agent-repl-link-code tmp 1 nil "my-ws")))
            (should (window-live-p result))))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (delete-file tmp))))

(ert-deftest agent-repl-cmd-test-link-code/with-workspace-opens-buffer-in-window ()
  "link-code with WORKSPACE displays the buffer in a live window (actually opens it)."
  (let ((tmp (make-temp-file "agent-repl-link-" nil ".txt" "a\nb\nc\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) nil))
                  ((symbol-function 'agent-repl--ws-add-buffer) #'ignore))
          (agent-repl-link-code tmp 1 nil "my-ws")
          (let ((buf (get-file-buffer tmp)))
            (should (get-buffer-window buf))))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (delete-file tmp))))

(ert-deftest agent-repl-cmd-test-link-code/with-workspace-does-not-select-window ()
  "link-code with WORKSPACE never calls `select-window' (focus is not stolen)."
  (let ((tmp (make-temp-file "agent-repl-link-" nil ".txt" "a\nb\nc\n"))
        (select-calls 0)
        (real-select (symbol-function 'select-window)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) nil))
                  ((symbol-function 'agent-repl--ws-add-buffer) #'ignore)
                  ;; Count only NORECORD-less calls; `with-selected-window'
                  ;; (used to recenter) always passes 'norecord, so those are
                  ;; excluded — we only want to catch a focus-stealing select.
                  ((symbol-function 'select-window)
                   (lambda (win &optional norecord)
                     (unless norecord (cl-incf select-calls))
                     (funcall real-select win norecord))))
          (agent-repl-link-code tmp 1 nil "my-ws")
          (should (= select-calls 0)))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (delete-file tmp))))

(ert-deftest agent-repl-cmd-test-link-code/without-workspace-selects-window ()
  "link-code without WORKSPACE selects the displayed window (focus path)."
  (let ((tmp (make-temp-file "agent-repl-link-" nil ".txt" "a\nb\nc\n"))
        (select-calls 0)
        (real-select (symbol-function 'select-window)))
    (unwind-protect
        (cl-letf (((symbol-function 'select-window)
                   (lambda (win &optional norecord)
                     (unless norecord (cl-incf select-calls))
                     (funcall real-select win norecord))))
          (agent-repl-link-code tmp 1)
          (should (> select-calls 0)))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (delete-file tmp))))

(ert-deftest agent-repl-cmd-test-link-code/with-workspace-skips-add-when-persp-nil ()
  "link-code with WORKSPACE skips ws-add-buffer when persp resolves to nil."
  (let ((tmp (make-temp-file "agent-repl-link-" nil ".txt" "a\nb\nc\n"))
        (add-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) nil))
                  ((symbol-function 'agent-repl--ws-add-buffer)
                   (lambda (&rest _) (setq add-called t))))
          (agent-repl-link-code tmp 1 nil "missing-ws")
          (should (not add-called)))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (delete-file tmp))))

;;;; ---- agent-repl--format-file-ref ----

(ert-deftest agent-repl-cmd-test-format-file-ref/no-region ()
  "format-file-ref returns file:line when no region is active."
  (cl-letf (((symbol-function 'agent-repl--buffer-relative-path)
             (lambda () "src/foo.el"))
            ((symbol-function 'use-region-p)
             (lambda () nil))
            ((symbol-function 'line-number-at-pos)
             (lambda (&optional _pos) 42)))
    (should (equal (agent-repl--format-file-ref) "src/foo.el:42"))))

(ert-deftest agent-repl-cmd-test-format-file-ref/with-region ()
  "format-file-ref returns file:start-end when region is active."
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "line1\nline2\nline3\nline4\nline5\n")
    ;; Select lines 2-4
    (goto-char (point-min))
    (forward-line 1)
    (set-mark (point))
    (forward-line 2)
    (cl-letf (((symbol-function 'agent-repl--buffer-relative-path)
               (lambda () "src/foo.el")))
      (let ((result (agent-repl--format-file-ref)))
        ;; Should be file:startline-endline format
        (should (string-match "^src/foo\\.el:[0-9]+-[0-9]+$" result))
        ;; Mark should be deactivated
        (should-not (use-region-p))))))

(ert-deftest agent-repl-cmd-test-format-file-ref/single-line-region ()
  "format-file-ref with region on a single line returns same start and end."
  (with-temp-buffer
    (transient-mark-mode 1)
    (insert "line1\nline2\nline3\n")
    (goto-char (point-min))
    (forward-line 1)
    (set-mark (point))
    (end-of-line)
    (cl-letf (((symbol-function 'agent-repl--buffer-relative-path)
               (lambda () "test.el")))
      (let ((result (agent-repl--format-file-ref)))
        (should (equal result "test.el:2-2"))))))

(ert-deftest agent-repl-cmd-test-format-file-ref/first-line ()
  "format-file-ref at first line returns file:1."
  (cl-letf (((symbol-function 'agent-repl--buffer-relative-path)
             (lambda () "root.el"))
            ((symbol-function 'use-region-p)
             (lambda () nil))
            ((symbol-function 'line-number-at-pos)
             (lambda (&optional _pos) 1)))
    (should (equal (agent-repl--format-file-ref) "root.el:1"))))

;;;; ---- agent-repl--format-magit-hunk-ref ----

(ert-deftest agent-repl-cmd-test-format-magit-hunk-ref/basic ()
  "format-magit-hunk-ref returns file:start-end from magit hunk section."
  (let ((mock-section (record 'magit-section nil nil nil nil nil nil nil nil)))
    ;; Stub eieio-oref (the runtime function that `oref' expands to)
    ;; since `oref' is a macro and cannot be stubbed via cl-letf.
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () mock-section))
              ((symbol-function 'magit-file-at-point)
               (lambda () "src/main.go"))
              ((symbol-function 'eieio-oref)
               (lambda (_obj _slot) '(10 5)))
              ((symbol-function 'magit-toplevel)
               (lambda () "/project/"))
              ((symbol-function 'agent-repl--ws-dir)
               (lambda (_ws) "/project/"))
              ((symbol-function '+workspace-current-name)
               (lambda () "test-ws")))
      (should (equal (agent-repl--format-magit-hunk-ref) "src/main.go:10-14")))))

(ert-deftest agent-repl-cmd-test-format-magit-hunk-ref/single-line-hunk ()
  "format-magit-hunk-ref with a 1-line hunk returns start equal to end."
  (let ((mock-section (record 'magit-section nil nil nil nil nil nil nil nil)))
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () mock-section))
              ((symbol-function 'magit-file-at-point)
               (lambda () "file.py"))
              ((symbol-function 'eieio-oref)
               (lambda (_obj _slot) '(25 1)))
              ((symbol-function 'magit-toplevel)
               (lambda () "/repo/"))
              ((symbol-function 'agent-repl--ws-dir)
               (lambda (_ws) "/repo/"))
              ((symbol-function '+workspace-current-name)
               (lambda () "test-ws")))
      (should (equal (agent-repl--format-magit-hunk-ref) "file.py:25-25")))))

(ert-deftest agent-repl-cmd-test-format-magit-hunk-ref/different-roots ()
  "format-magit-hunk-ref computes relative path from ws-dir, not magit-toplevel."
  (let ((mock-section (record 'magit-section nil nil nil nil nil nil nil nil)))
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () mock-section))
              ((symbol-function 'magit-file-at-point)
               (lambda () "subdir/file.rs"))
              ((symbol-function 'eieio-oref)
               (lambda (_obj _slot) '(1 3)))
              ((symbol-function 'magit-toplevel)
               (lambda () "/workspace/project/"))
              ((symbol-function 'agent-repl--ws-dir)
               (lambda (_ws) "/workspace/project/"))
              ((symbol-function '+workspace-current-name)
               (lambda () "test-ws")))
      (should (equal (agent-repl--format-magit-hunk-ref) "subdir/file.rs:1-3")))))

;;;; ---- agent-repl--context-reference ----

(ert-deftest agent-repl-cmd-test-context-reference/non-magit-delegates-to-format-file-ref ()
  "context-reference delegates to format-file-ref when not in a magit mode."
  (with-temp-buffer
    (cl-letf (((symbol-function 'agent-repl--format-file-ref)
               (lambda () "src/foo.el:10")))
      (should (equal (agent-repl--context-reference) "src/foo.el:10")))))

(ert-deftest agent-repl-cmd-test-context-reference/magit-hunk-delegates-to-magit-ref ()
  "context-reference delegates to format-magit-hunk-ref in magit hunk context."
  (with-temp-buffer
    (let ((major-mode 'magit-diff-mode))
      (cl-letf (((symbol-function 'magit-section-match)
                 (lambda (_type) t))
                ((symbol-function 'agent-repl--format-magit-hunk-ref)
                 (lambda () "src/main.go:10-14"))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t)))
        (should (equal (agent-repl--context-reference) "src/main.go:10-14"))))))

(ert-deftest agent-repl-cmd-test-context-reference/magit-non-hunk-section ()
  "context-reference falls through to format-file-ref in magit non-hunk section."
  (with-temp-buffer
    (let ((major-mode 'magit-status-mode))
      (cl-letf (((symbol-function 'magit-section-match)
                 (lambda (_type) nil))
                ((symbol-function 'agent-repl--format-file-ref)
                 (lambda () "src/bar.el:5"))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t)))
        (should (equal (agent-repl--context-reference) "src/bar.el:5"))))))

;;;; ---- agent-repl--send-diff-analysis ----

(ert-deftest agent-repl-cmd-test-send-diff-analysis/formats-message ()
  "send-diff-analysis formats 'for the SPEC, PROMPT' and sends it."
  (let (sent-text)
    (cl-letf (((symbol-function 'agent-repl--send-to-agent)
               (lambda (text) (setq sent-text text))))
      (agent-repl--send-diff-analysis "unstaged changes (git diff)" "please explain the changes")
      (should (equal sent-text "for the unstaged changes (git diff), please explain the changes")))))

;;;; ---- agent-repl--resolve-change-spec ----

(ert-deftest agent-repl-cmd-test-resolve-change-spec/string-default ()
  "resolve-change-spec returns string default-spec when no override."
  (should (equal (agent-repl--resolve-change-spec
                  'worktree "unstaged changes (git diff)" nil)
                 "unstaged changes (git diff)")))

(ert-deftest agent-repl-cmd-test-resolve-change-spec/branch-returns-symbol ()
  "resolve-change-spec returns symbol for :use-branch-diff-spec."
  (should (eq (agent-repl--resolve-change-spec
               'branch :use-branch-diff-spec nil)
              'agent-repl-branch-diff-spec)))

(ert-deftest agent-repl-cmd-test-resolve-change-spec/override-takes-precedence ()
  "resolve-change-spec prefers override over default-spec."
  (defvar agent-repl-test--override-alist
    '((worktree . "OVERRIDDEN worktree spec")))
  (should (equal (agent-repl--resolve-change-spec
                  'worktree "default spec" 'agent-repl-test--override-alist)
                 "OVERRIDDEN worktree spec")))

(ert-deftest agent-repl-cmd-test-resolve-change-spec/override-missing-scope-falls-through ()
  "resolve-change-spec falls through to default when override has no entry for scope."
  (defvar agent-repl-test--partial-override
    '((staged . "overridden staged")))
  (should (equal (agent-repl--resolve-change-spec
                  'worktree "default worktree" 'agent-repl-test--partial-override)
                 "default worktree")))

(ert-deftest agent-repl-cmd-test-resolve-change-spec/override-branch-still-uses-symbol ()
  "resolve-change-spec returns branch symbol even with overrides that lack branch entry."
  (defvar agent-repl-test--no-branch-override
    '((worktree . "override")))
  (should (eq (agent-repl--resolve-change-spec
               'branch :use-branch-diff-spec 'agent-repl-test--no-branch-override)
              'agent-repl-branch-diff-spec)))

;;;; ---- agent-repl--send-to-agent ----

(ert-deftest agent-repl-cmd-test-send-to-agent/not-running-initializes-first ()
  "send-to-agent calls initialize-agent when the agent is not running."
  (let (init-called sent-text)
    (agent-repl-test--with-clean-state
      (agent-repl-test--use-vterm-frontend)
      (let ((fake-vterm-buf (get-buffer-create " *test-vterm*")))
        (unwind-protect
            (progn
              (agent-repl--ws-put "test-ws" :vterm-buffer fake-vterm-buf)
              (cl-letf (((symbol-function '+workspace-current-name)
                         (lambda () "test-ws"))
                        ((symbol-function 'agent-repl--agent-running-p)
                         (lambda (_ws) nil))
                        ((symbol-function 'agent-repl--initialize-agent)
                         (lambda (_ws) (setq init-called t)))
                        ((symbol-function 'agent-repl--send-input-to-vterm)
                         (lambda (_buf text) (setq sent-text text))))
                (agent-repl--send-to-agent "hello claude")
                (should init-called)
                (should (equal sent-text "hello claude"))))
          (kill-buffer fake-vterm-buf))))))

(ert-deftest agent-repl-cmd-test-send-to-agent/running-skips-init ()
  "send-to-agent skips initialize-agent when the agent is already running."
  (let (init-called sent-buf sent-text)
    (agent-repl-test--with-clean-state
      (agent-repl-test--use-vterm-frontend)
      (let ((fake-vterm-buf (get-buffer-create " *test-vterm*")))
        (unwind-protect
            (progn
              (agent-repl--ws-put "test-ws" :vterm-buffer fake-vterm-buf)
              (cl-letf (((symbol-function '+workspace-current-name)
                         (lambda () "test-ws"))
                        ((symbol-function 'agent-repl--agent-running-p)
                         (lambda (_ws) t))
                        ((symbol-function 'agent-repl--initialize-agent)
                         (lambda (_ws) (setq init-called t)))
                        ((symbol-function 'agent-repl--send-input-to-vterm)
                         (lambda (buf text)
                           (setq sent-buf buf sent-text text))))
                (agent-repl--send-to-agent "hello claude")
                (should-not init-called)
                (should (eq sent-buf fake-vterm-buf))
                (should (equal sent-text "hello claude"))))
          (kill-buffer fake-vterm-buf))))))

(ert-deftest agent-repl-cmd-test-send-to-agent/gui-routes-through-frontend ()
  "send-to-agent on a gui workspace dispatches via the frontend registry.
Booting a vterm here would put a second claude process on the same
directory as the workspace's daemon session."
  (let (init-called dispatched)
    (agent-repl-test--with-clean-state
      (agent-repl--ws-put "test-ws" :frontend 'gui)
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'agent-repl--initialize-agent)
                 (lambda (_ws) (setq init-called t)))
                ((symbol-function 'agent-repl--frontend-dispatch-send)
                 (lambda (ws input raw &optional _on-settle)
                   (setq dispatched (list ws input raw)))))
        (agent-repl--send-to-agent "hello claude")
        (should-not init-called)
        (should (equal dispatched '("test-ws" "hello claude" "hello claude")))))))

;;;; ---- agent-repl--establish-workspace frontend routing ----

(defmacro agent-repl-cmd-test--with-establish-stubs (&rest body)
  "Run BODY with `agent-repl--establish-workspace's side effects stubbed.
Leaves only the ws-plist writes and the final agent-boot branch live so
tests can observe which frontend boot fired.

The env hydration is stubbed but kept FAITHFUL: the boot
\(`agent-repl--frontend-boot-session') hydrates before it resolves the
frontend, and `:active-env' is one of the two axes it resolves against, so
a stub that wrote nothing would hand the resolution an unhydrated
workspace.  Stubbing it also keeps the restore path from writing a state
file into the fake project dir."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl--ws-create) #'ignore)
             ((symbol-function 'agent-repl--ws-frame-switch) #'ignore)
             ((symbol-function 'agent-repl--clean-frame-foreign-windows) #'ignore)
             ((symbol-function 'agent-repl--ws-register-project) #'ignore)
             ((symbol-function 'agent-repl--ws-run-switch-project-function) #'ignore)
             ((symbol-function 'agent-repl--most-recent-project-file) (lambda (_d) nil))
             ((symbol-function 'agent-repl--initialize-ws-env)
              (lambda (ws &optional _dir env)
                (agent-repl--ws-put ws :active-env (or env :bare-metal))))
             ((symbol-function 'agent-repl--hydrate-and-reorder-on-open) #'ignore))
     ,@body))

(ert-deftest agent-repl-cmd-test-establish-workspace/gui-ensures-daemon-session ()
  "Restoring a gui workspace ensures a background daemon session.
Booting a vterm would re-present (and re-stamp) the workspace as vterm,
silently undoing the user's frontend choice on every restart."
  (let (ensured init-called)
    (agent-repl-test--with-clean-state
      (agent-repl--ws-put "ws-gui" :frontend 'gui)
      (agent-repl-cmd-test--with-establish-stubs
        (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                   (lambda (ws) (setq ensured ws)))
                  ((symbol-function 'agent-repl--initialize-agent)
                   (lambda (&rest _) (setq init-called t))))
          (agent-repl--establish-workspace "ws-gui" "/tmp/ws-gui")
          (should (equal ensured "ws-gui"))
          (should-not init-called))))))

(ert-deftest agent-repl-cmd-test-establish-workspace/gui-skips-ensure-when-bound ()
  "A gui workspace already holding a daemon binding is not re-ensured."
  (let (ensured)
    (agent-repl-test--with-clean-state
      (agent-repl--ws-put "ws-gui" :frontend 'gui)
      (agent-repl--ws-put "ws-gui" :frontend-session-id "s_live")
      (agent-repl-cmd-test--with-establish-stubs
        (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                   (lambda (ws) (setq ensured ws)))
                  ((symbol-function 'agent-repl--initialize-agent) #'ignore))
          (agent-repl--establish-workspace "ws-gui" "/tmp/ws-gui")
          (should-not ensured))))))

(ert-deftest agent-repl-cmd-test-establish-workspace/vterm-boots-agent ()
  "Restoring a vterm workspace still pre-starts claude via initialize-agent."
  (let (init-ws ensured)
    (agent-repl-test--with-clean-state
      (agent-repl--ws-put "ws-vt" :frontend 'vterm)
      (agent-repl-cmd-test--with-establish-stubs
        (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                   (lambda (ws) (setq ensured ws)))
                  ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                  ((symbol-function 'agent-repl--initialize-agent)
                   (lambda (ws &optional _dir _env) (setq init-ws ws))))
          (agent-repl--establish-workspace "ws-vt" "/tmp/ws-vt")
          (should (equal init-ws "ws-vt"))
          (should-not ensured))))))

(ert-deftest agent-repl-cmd-test-establish-workspace/no-choice-restores-under-default ()
  "A restored workspace with no DELIBERATE frontend choice comes up in the gui.
This is the old-workspace case: every workspace predating the gui carries
an incidental `:frontend vterm' stamp in its state file, which the restore
deliberately ignores — so it follows `agent-repl-default-frontend' forward
rather than staying pinned to the vterm it happened to boot once."
  (let (ensured init-called)
    (agent-repl-test--with-clean-state
      ;; Arrange — no :frontend at all (what the restore leaves behind for a
      ;; workspace whose save carried no :frontend-explicit marker).
      (agent-repl-cmd-test--with-establish-stubs
        (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                   (lambda (ws) (setq ensured ws)))
                  ((symbol-function 'agent-repl--initialize-agent)
                   (lambda (&rest _) (setq init-called t))))
          ;; Act
          (agent-repl--establish-workspace "ws-old" "/tmp/ws-old")
          ;; Assert
          (should (equal ensured "ws-old"))
          (should-not init-called))))))

(ert-deftest agent-repl-cmd-test-establish-workspace/codex-restores-under-vterm ()
  "A restored codex workspace boots the vterm despite the gui default.
The gui drives only claude, so resolving the restore to the raw default
would hand the workspace a presentation that cannot run its agent at all.
The restore routes by CAPABILITY, not just by an explicit `:frontend'."
  (let (init-ws ensured)
    (agent-repl-test--with-clean-state
      ;; Arrange — a codex workspace carrying no deliberate frontend choice.
      (agent-repl--ws-put "ws-cx" :backend 'codex)
      (agent-repl-cmd-test--with-establish-stubs
        (cl-letf (((symbol-function 'agent-repl--frontend-ensure-session)
                   (lambda (ws) (setq ensured ws)))
                  ((symbol-function 'agent-repl--agent-running-p) (lambda (_ws) nil))
                  ((symbol-function 'agent-repl--initialize-agent)
                   (lambda (ws &optional _dir _env) (setq init-ws ws))))
          ;; Act
          (agent-repl--establish-workspace "ws-cx" "/tmp/ws-cx")
          ;; Assert
          (should (equal init-ws "ws-cx"))
          (should-not ensured))))))

;;;; ---- agent-repl-explain ----

(ert-deftest agent-repl-cmd-test-explain/sends-context-reference ()
  "explain sends 'please explain REF' to the agent."
  (let (sent-text)
    (cl-letf (((symbol-function 'agent-repl--context-reference)
               (lambda () "src/foo.el:42"))
              ((symbol-function 'agent-repl--send-to-agent)
               (lambda (text) (setq sent-text text))))
      (agent-repl-explain)
      (should (equal sent-text "please explain src/foo.el:42")))))

;;;; ---- agent-repl-explain-prompt ----

(ert-deftest agent-repl-cmd-test-explain-prompt/sends-user-input ()
  "explain-prompt sends user-provided text to the agent."
  (let (sent-text)
    (cl-letf (((symbol-function 'agent-repl--context-reference)
               (lambda () "src/foo.el:42"))
              ((symbol-function 'read-string)
               (lambda (_prompt _initial) "review src/foo.el:42 for bugs"))
              ((symbol-function 'agent-repl--send-to-agent)
               (lambda (text) (setq sent-text text))))
      (agent-repl-explain-prompt)
      (should (equal sent-text "review src/foo.el:42 for bugs")))))

(ert-deftest agent-repl-cmd-test-explain-prompt/prefills-context-reference ()
  "explain-prompt pre-fills the minibuffer with the context reference."
  (let (initial-input)
    (cl-letf (((symbol-function 'agent-repl--context-reference)
               (lambda () "src/bar.el:10-20"))
              ((symbol-function 'read-string)
               (lambda (_prompt initial) (setq initial-input initial) "anything"))
              ((symbol-function 'agent-repl--send-to-agent)
               (lambda (_text))))
      (agent-repl-explain-prompt)
      (should (equal initial-input "src/bar.el:10-20")))))

(ert-deftest agent-repl-cmd-test-explain-prompt/empty-input-does-not-send ()
  "explain-prompt does not send when user provides empty input."
  (let (sent-text)
    (cl-letf (((symbol-function 'agent-repl--context-reference)
               (lambda () "src/foo.el:1"))
              ((symbol-function 'read-string)
               (lambda (_prompt _initial) ""))
              ((symbol-function 'agent-repl--send-to-agent)
               (lambda (text) (setq sent-text text))))
      (agent-repl-explain-prompt)
      (should (null sent-text)))))


;;;; ---- agent-repl--send-interrupt-escape ----

(ert-deftest agent-repl-cmd-test-send-interrupt-escape/sends-two-escapes ()
  "send-interrupt-escape sends two escape key presses to vterm buffer."
  (let ((keys-sent '()))
    (agent-repl-test--with-temp-buffer " *test-vterm-interrupt*"
      (cl-letf (((symbol-function 'vterm-send-key)
                 (lambda (key) (push key keys-sent))))
        (agent-repl--send-interrupt-escape "test-ws" (current-buffer))
        (should (equal (nreverse keys-sent) '("<escape>" "<escape>")))))))

;;;; ---- agent-repl--enter-insert-mode ----

(ert-deftest agent-repl-cmd-test-enter-insert-mode/live-input-buffer ()
  "enter-insert-mode enters evil insert state in a live input buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-input-insert*"
      (let ((input-buf (current-buffer))
            (insert-called nil))
        (agent-repl--ws-put "test-ws" :input-buffer input-buf)
        (cl-letf (((symbol-function '+workspace-current-name)
                   (lambda () "test-ws"))
                  ((symbol-function 'evil-insert-state)
                   (lambda (&rest _) (setq insert-called t))))
          (agent-repl--enter-insert-mode "test-ws")
          (should insert-called))))))

(ert-deftest agent-repl-cmd-test-enter-insert-mode/never-sends-i-to-vterm ()
  "enter-insert-mode must NOT forward a literal \"i\" keystroke to the vterm.
Regression: sending \"i\" double-dispatched the mode switch and leaked a
stray \"i\" onto the agent's prompt line."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-input-no-i*"
      (let ((input-buf (current-buffer))
            (sent-string nil))
        (agent-repl--ws-put "test-ws" :input-buffer input-buf)
        (cl-letf (((symbol-function '+workspace-current-name)
                   (lambda () "test-ws"))
                  ((symbol-function 'evil-insert-state) #'ignore)
                  ((symbol-function 'vterm-send-string)
                   (lambda (str) (setq sent-string str))))
          (agent-repl--enter-insert-mode "test-ws")
          (should-not sent-string))))))

(ert-deftest agent-repl-cmd-test-enter-insert-mode/noop-when-ws-not-current ()
  "enter-insert-mode is a no-op when WS is not the current workspace.
A drawer-triggered interrupt on a background workspace must not steal
focus or flip a hidden buffer's evil state."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-input-bg*"
      (let ((input-buf (current-buffer))
            (insert-called nil))
        (agent-repl--ws-put "bg-ws" :input-buffer input-buf)
        (cl-letf (((symbol-function '+workspace-current-name)
                   (lambda () "other-ws"))
                  ((symbol-function 'evil-insert-state)
                   (lambda (&rest _) (setq insert-called t))))
          (agent-repl--enter-insert-mode "bg-ws")
          (should-not insert-called))))))

(ert-deftest agent-repl-cmd-test-enter-insert-mode/dead-input-buffer ()
  "enter-insert-mode is a no-op when the input buffer is dead."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-dead-input-buf*"))
          (insert-called nil))
      (agent-repl--ws-put "test-ws" :input-buffer buf)
      (kill-buffer buf)
      (cl-letf (((symbol-function '+workspace-current-name)
                 (lambda () "test-ws"))
                ((symbol-function 'evil-insert-state)
                 (lambda (&rest _) (setq insert-called t))))
        (agent-repl--enter-insert-mode "test-ws")
        (should-not insert-called)))))

;;;; ---- agent-repl-interrupt ----

(ert-deftest agent-repl-cmd-test-interrupt/sends-escape-when-vterm-live ()
  "interrupt sends escape keys when vterm is live."
  (let (escape-called)
    (agent-repl-test--with-clean-state
      (agent-repl-test--use-vterm-frontend)
      (let ((fake-vterm-buf (get-buffer-create " *test-interrupt-vterm*")))
        (unwind-protect
            (progn
              (agent-repl--ws-put "test-ws" :vterm-buffer fake-vterm-buf)
              ;; NOTE: do NOT stub `agent-repl--ws-get' here.  A blanket
              ;; stub that returns `fake-vterm-buf' for every key would
              ;; hand the interrupt path (via `--mark-agent-done') a
              ;; buffer where it expects other value types, risking
              ;; wrong-type errors.  The real `--ws-get' reads the value
              ;; `--ws-put' just stored for `:vterm-buffer' and returns
              ;; nil for unknown keys, which is exactly what the interrupt
              ;; path expects.
              (cl-letf (((symbol-function 'agent-repl--vterm-live-p)
                         (lambda () t))
                        ((symbol-function '+workspace-current-name)
                         (lambda () "test-ws"))
                        ((symbol-function 'agent-repl--send-interrupt-escape)
                         (lambda (_ws _buf) (setq escape-called t)))
                        ((symbol-function 'run-at-time)
                         (lambda (_time _repeat _fn _arg) nil)))
                (agent-repl-interrupt)
                (should escape-called)))
          (kill-buffer fake-vterm-buf))))))

(ert-deftest agent-repl-cmd-test-interrupt/noop-when-vterm-not-live ()
  "interrupt is a no-op when vterm is not live."
  ;; Arrange — a vterm-world test, so pin the frontend (the shipped
  ;; default is `gui'); no `with-clean-state' here to hold the binding.
  (let ((agent-repl-default-frontend 'vterm)
        escape-called)
    (cl-letf (((symbol-function 'agent-repl--vterm-live-p)
               (lambda () nil))
              ((symbol-function 'agent-repl--send-interrupt-escape)
               (lambda (_ws _buf) (setq escape-called t))))
      (agent-repl-interrupt)
      (should-not escape-called))))

(ert-deftest agent-repl-cmd-test-interrupt/marks-agent-state-done-when-vterm-live ()
  "interrupt sets the workspace's :agent-state to :done after sending escape."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((fake-vterm-buf (get-buffer-create " *test-interrupt-done-vterm*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :vterm-buffer fake-vterm-buf)
            (agent-repl--ws-set-agent-state "test-ws" :thinking)
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "test-ws"))
                      ((symbol-function 'agent-repl--send-interrupt-escape)
                       (lambda (_ws _buf) nil))
                      ((symbol-function 'run-at-time)
                       (lambda (_time _repeat _fn _arg) nil)))
              (agent-repl-interrupt)
              (should (eq (agent-repl--ws-get "test-ws" :agent-state) :done))))
        (kill-buffer fake-vterm-buf)))))

(ert-deftest agent-repl-cmd-test-interrupt/clears-stop-tracking-when-vterm-live ()
  "interrupt clears :stop-received and :pending-subagents after sending escape.
The interrupted turn will never see a Stop hook, so leftover tracking
state from the previous turn must be reset by Emacs."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let ((fake-vterm-buf (get-buffer-create " *test-interrupt-clear-vterm*")))
      (unwind-protect
          (progn
            (agent-repl--ws-put "test-ws" :vterm-buffer fake-vterm-buf)
            (agent-repl--ws-set-stop-received "test-ws" t)
            (agent-repl--ws-incf-pending-subagents "test-ws")
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "test-ws"))
                      ((symbol-function 'agent-repl--send-interrupt-escape)
                       (lambda (_ws _buf) nil))
                      ((symbol-function 'run-at-time)
                       (lambda (_time _repeat _fn _arg) nil)))
              (agent-repl-interrupt)
              (should-not (agent-repl--ws-stop-received-p "test-ws"))
              (should (= 0 (agent-repl--ws-pending-subagents "test-ws")))))
        (kill-buffer fake-vterm-buf)))))

(ert-deftest agent-repl-cmd-test-interrupt/does-not-mark-done-when-vterm-not-live ()
  "interrupt does not mark :done when vterm is not live.
No interrupt was actually delivered, so the state should not change."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (agent-repl--ws-put "test-ws" :vterm-buffer nil)
    (agent-repl--ws-set-agent-state "test-ws" :thinking)
    (cl-letf (((symbol-function '+workspace-current-name)
               (lambda () "test-ws"))
              ((symbol-function 'agent-repl--send-interrupt-escape)
               (lambda (_ws _buf) nil)))
      (agent-repl-interrupt)
      (should (eq (agent-repl--ws-get "test-ws" :agent-state) :thinking)))))

;;;; ---- agent-repl-update-pr ----

(ert-deftest agent-repl-cmd-test-update-pr/sends-prompt ()
  "update-pr sends the configured update-pr prompt to the agent."
  (let (sent-text)
    (cl-letf (((symbol-function 'agent-repl--send-to-agent)
               (lambda (text) (setq sent-text text))))
      (agent-repl-update-pr)
      (should (equal sent-text agent-repl-update-pr-prompt)))))

;;;; ---- agent-repl-rebase-onto-origin-master ----

(ert-deftest agent-repl-cmd-test-rebase-onto-origin-master/fetches-origin-in-ws-dir ()
  "rebase-onto-origin-master invokes async-git with `fetch origin' in the workspace dir."
  (let (label-arg git-root-arg args-arg)
    (cl-letf (((symbol-function '+workspace-current-name)
               (lambda () "test-ws"))
              ((symbol-function 'agent-repl--ws-dir)
               (lambda (_ws) "/project/"))
              ((symbol-function 'agent-repl--async-git)
               (lambda (label git-root args _callback)
                 (setq label-arg label
                       git-root-arg git-root
                       args-arg args))))
      (agent-repl-rebase-onto-origin-master)
      (should (equal label-arg "rebase-fetch"))
      (should (equal git-root-arg "/project/"))
      (should (equal args-arg '("fetch" "origin"))))))

(ert-deftest agent-repl-cmd-test-rebase-onto-origin-master/sends-prompt-on-fetch-success ()
  "On fetch success, callback sends the rebase prompt to the agent."
  (let (sent-text)
    (cl-letf (((symbol-function 'agent-repl--send-to-agent)
               (lambda (text) (setq sent-text text))))
      (agent-repl--rebase-onto-origin-master-callback "test-ws" t "fetch output")
      (should (equal sent-text agent-repl-rebase-onto-origin-master-prompt)))))

(ert-deftest agent-repl-cmd-test-rebase-onto-origin-master/skips-prompt-on-fetch-failure ()
  "On fetch failure, callback does NOT send the rebase prompt."
  (let ((send-called nil))
    (cl-letf (((symbol-function 'agent-repl--send-to-agent)
               (lambda (_text) (setq send-called t))))
      (agent-repl--rebase-onto-origin-master-callback "test-ws" nil "fatal: not a git repository")
      (should-not send-called))))

(ert-deftest agent-repl-cmd-test-rebase-onto-origin-master/callback-routes-through-async-git ()
  "Command's async-git callback dispatches via the named callback helper."
  (let (captured-callback sent-text)
    (cl-letf (((symbol-function '+workspace-current-name)
               (lambda () "test-ws"))
              ((symbol-function 'agent-repl--ws-dir)
               (lambda (_ws) "/project/"))
              ((symbol-function 'agent-repl--async-git)
               (lambda (_label _git-root _args callback)
                 (setq captured-callback callback)))
              ((symbol-function 'agent-repl--send-to-agent)
               (lambda (text) (setq sent-text text))))
      (agent-repl-rebase-onto-origin-master)
      (funcall captured-callback t "ok")
      (should (equal sent-text agent-repl-rebase-onto-origin-master-prompt)))))

;;;; ---- agent-repl--exclusion-symbol-to-flag ----

(ert-deftest agent-repl-cmd-test-exclusion-symbol-to-flag/single-word ()
  "no-self-certified maps to --self-certified."
  (should (equal (agent-repl--exclusion-symbol-to-flag 'no-self-certified)
                 "--self-certified")))

(ert-deftest agent-repl-cmd-test-exclusion-symbol-to-flag/multi-word ()
  "Dashes inside the flag name are preserved."
  (should (equal (agent-repl--exclusion-symbol-to-flag 'no-add-to-merge-queue)
                 "--add-to-merge-queue")))

(ert-deftest agent-repl-cmd-test-exclusion-symbol-to-flag/missing-prefix-errors ()
  "Symbol without the `no-' prefix raises an error."
  (should-error (agent-repl--exclusion-symbol-to-flag 'self-certified)))

;;;; ---- agent-repl--build-create-or-update-pr-prompt ----

(ert-deftest agent-repl-cmd-test-build-coup-prompt/no-exclusions-keeps-all-flags ()
  "With nil EXCLUDED, the prompt contains every base flag."
  (let ((agent-repl-create-or-update-pr-base-flags
         '("--patch" "--self-certified" "--add-to-merge-queue")))
    (should (equal (agent-repl--build-create-or-update-pr-prompt nil)
                   "/create-or-update-pr --patch --self-certified --add-to-merge-queue"))))

(ert-deftest agent-repl-cmd-test-build-coup-prompt/single-exclusion ()
  "A single exclusion drops just that flag."
  (let ((agent-repl-create-or-update-pr-base-flags
         '("--patch" "--self-certified" "--add-to-merge-queue")))
    (should (equal (agent-repl--build-create-or-update-pr-prompt '(no-self-certified))
                   "/create-or-update-pr --patch --add-to-merge-queue"))))

(ert-deftest agent-repl-cmd-test-build-coup-prompt/multiple-exclusions ()
  "Multiple exclusions drop each named flag."
  (let ((agent-repl-create-or-update-pr-base-flags
         '("--patch" "--self-certified" "--add-to-merge-queue")))
    (should (equal (agent-repl--build-create-or-update-pr-prompt
                    '(no-self-certified no-add-to-merge-queue))
                   "/create-or-update-pr --patch"))))

(ert-deftest agent-repl-cmd-test-build-coup-prompt/unknown-exclusion-errors ()
  "Excluding a flag not present in the base list signals an error."
  (let ((agent-repl-create-or-update-pr-base-flags '("--patch" "--self-certified")))
    (should-error (agent-repl--build-create-or-update-pr-prompt '(no-add-to-merge-queue)))))

;;;; ---- agent-repl-create-or-update-pr ----

(ert-deftest agent-repl-cmd-test-create-or-update-pr/no-args-sends-default ()
  "create-or-update-pr with no args sends the prompt built from base flags."
  (let (sent-text)
    (cl-letf (((symbol-function 'agent-repl--send-to-agent)
               (lambda (text) (setq sent-text text))))
      (agent-repl-create-or-update-pr)
      (should (equal sent-text
                     (agent-repl--build-create-or-update-pr-prompt nil))))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr/excluded-arg-omits-flag ()
  "create-or-update-pr called with EXCLUDED list drops those flags."
  (let (sent-text)
    (cl-letf (((symbol-function 'agent-repl--send-to-agent)
               (lambda (text) (setq sent-text text))))
      (agent-repl-create-or-update-pr '(no-self-certified))
      (should-not (string-match-p "--self-certified" sent-text)))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr/prefixes-input-buffer-contents ()
  "Non-empty input buffer contents are prepended to the prompt."
  (agent-repl-test--with-clean-state
    (let (sent-text)
      (agent-repl-test--with-temp-buffer " *test-coup-input*"
        (insert "do a thing")
        (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--send-to-agent)
                   (lambda (text) (setq sent-text text)))
                  ((symbol-function 'agent-repl--commit-input-buffer)
                   (lambda (&rest _) nil)))
          (agent-repl-create-or-update-pr)
          (should (equal sent-text
                         (concat "do a thing "
                                 (agent-repl--build-create-or-update-pr-prompt nil)))))))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr/empty-buffer-sends-bare-prompt ()
  "Empty input buffer leaves the base prompt unprefixed (no leading space)."
  (agent-repl-test--with-clean-state
    (let (sent-text)
      (agent-repl-test--with-temp-buffer " *test-coup-input*"
        (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--send-to-agent)
                   (lambda (text) (setq sent-text text))))
          (agent-repl-create-or-update-pr)
          (should (equal sent-text
                         (agent-repl--build-create-or-update-pr-prompt nil))))))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr/trims-trailing-whitespace-on-prefix ()
  "Trailing whitespace/newlines in the input buffer are trimmed before joining."
  (agent-repl-test--with-clean-state
    (let (sent-text)
      (agent-repl-test--with-temp-buffer " *test-coup-input*"
        (insert "do a thing  \n")
        (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--send-to-agent)
                   (lambda (text) (setq sent-text text)))
                  ((symbol-function 'agent-repl--commit-input-buffer)
                   (lambda (&rest _) nil)))
          (agent-repl-create-or-update-pr)
          (should (equal sent-text
                         (concat "do a thing "
                                 (agent-repl--build-create-or-update-pr-prompt nil)))))))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr/whitespace-only-buffer-treated-as-empty ()
  "An input buffer of only whitespace is treated as empty (no prefix, no commit)."
  (agent-repl-test--with-clean-state
    (let (sent-text commit-called)
      (agent-repl-test--with-temp-buffer " *test-coup-input*"
        (insert "   \n  ")
        (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--send-to-agent)
                   (lambda (text) (setq sent-text text)))
                  ((symbol-function 'agent-repl--commit-input-buffer)
                   (lambda (&rest _) (setq commit-called t))))
          (agent-repl-create-or-update-pr)
          (should (equal sent-text
                         (agent-repl--build-create-or-update-pr-prompt nil)))
          (should-not commit-called))))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr/commits-input-buffer-when-prefixed ()
  "When a prefix was used, the input buffer is committed (history + clear)."
  (agent-repl-test--with-clean-state
    (let (commit-args)
      (agent-repl-test--with-temp-buffer " *test-coup-input*"
        (insert "ship it")
        (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--send-to-agent) (lambda (_) nil))
                  ((symbol-function 'agent-repl--commit-input-buffer)
                   (lambda (ws buf raw clear-p)
                     (setq commit-args (list ws buf raw clear-p)))))
          (agent-repl-create-or-update-pr)
          (should commit-args)
          (should (equal (nth 0 commit-args) "test-ws"))
          (should (eq (nth 3 commit-args) t)))))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr/excluded-with-prefix-still-drops-flag ()
  "EXCLUDED flags are dropped even when an input buffer prefix is present."
  (agent-repl-test--with-clean-state
    (let (sent-text)
      (agent-repl-test--with-temp-buffer " *test-coup-input*"
        (insert "do a thing")
        (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
        (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                  ((symbol-function 'agent-repl--send-to-agent)
                   (lambda (text) (setq sent-text text)))
                  ((symbol-function 'agent-repl--commit-input-buffer)
                   (lambda (&rest _) nil)))
          (agent-repl-create-or-update-pr '(no-self-certified))
          (should (string-prefix-p "do a thing " sent-text))
          (should-not (string-match-p "--self-certified" sent-text)))))))

;;;; ---- agent-repl-create-or-update-pr-no-self-certified ----

(ert-deftest agent-repl-cmd-test-create-or-update-pr-no-self-certified/sends-prompt ()
  "no-self-certified wrapper sends a prompt that omits --self-certified."
  (let (sent-text)
    (cl-letf (((symbol-function 'agent-repl--send-to-agent)
               (lambda (text) (setq sent-text text))))
      (agent-repl-create-or-update-pr-no-self-certified)
      (should-not (string-match-p "--self-certified" sent-text)))))

;;;; ---- agent-repl-create-or-update-pr-paste ----

(ert-deftest agent-repl-cmd-test-create-or-update-pr-paste/inserts-prompt-at-point ()
  "paste variant inserts the full base prompt at point, wrapped in backticks."
  (with-temp-buffer
    (agent-repl-create-or-update-pr-paste)
    (should (equal (buffer-string)
                   (concat "`"
                           (agent-repl--build-create-or-update-pr-prompt nil)
                           "`")))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr-paste/wraps-in-backticks ()
  "paste variant wraps the inserted prompt in single backticks."
  (with-temp-buffer
    (agent-repl-create-or-update-pr-paste)
    (let ((s (buffer-string)))
      (should (string-prefix-p "`" s))
      (should (string-suffix-p "`" s)))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr-paste/does-not-send-to-agent ()
  "paste variant must not call `agent-repl--send-to-agent'."
  (let (send-called)
    (cl-letf (((symbol-function 'agent-repl--send-to-agent)
               (lambda (&rest _) (setq send-called t))))
      (with-temp-buffer
        (agent-repl-create-or-update-pr-paste)
        (should-not send-called)))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr-paste/excluded-arg-omits-flag ()
  "paste variant honors EXCLUDED and drops the named flag from the inserted text."
  (with-temp-buffer
    (agent-repl-create-or-update-pr-paste '(no-self-certified))
    (should-not (string-match-p "--self-certified" (buffer-string)))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr-paste/ignores-input-buffer-prefix ()
  "paste variant inserts the bare prompt — the workspace input buffer is not consulted."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer " *test-coup-paste-input*"
      (insert "do a thing")
      (agent-repl--ws-put "test-ws" :input-buffer (current-buffer))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "test-ws"))
                ((symbol-function 'agent-repl--commit-input-buffer)
                 (lambda (&rest _) (error "must not commit input buffer"))))
        (with-temp-buffer
          (agent-repl-create-or-update-pr-paste)
          (should (equal (buffer-string)
                         (concat "`"
                                 (agent-repl--build-create-or-update-pr-prompt nil)
                                 "`"))))))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr-paste/inserts-at-point-not-end ()
  "paste variant inserts at point, preserving surrounding buffer content."
  (with-temp-buffer
    (insert "before-AFTER")
    (goto-char (+ (point-min) 7))
    (agent-repl-create-or-update-pr-paste)
    (let ((expected (concat "before-`"
                            (agent-repl--build-create-or-update-pr-prompt nil)
                            "`AFTER")))
      (should (equal (buffer-string) expected)))))

;;;; ---- agent-repl-create-or-update-pr-no-self-certified-paste ----

(ert-deftest agent-repl-cmd-test-create-or-update-pr-no-self-certified-paste/omits-flag ()
  "no-self-certified paste wrapper inserts a prompt without --self-certified."
  (with-temp-buffer
    (agent-repl-create-or-update-pr-no-self-certified-paste)
    (should-not (string-match-p "--self-certified" (buffer-string)))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr-no-self-certified-paste/wraps-in-backticks ()
  "no-self-certified paste wrapper wraps the inserted prompt in single backticks."
  (with-temp-buffer
    (agent-repl-create-or-update-pr-no-self-certified-paste)
    (let ((s (buffer-string)))
      (should (string-prefix-p "`" s))
      (should (string-suffix-p "`" s)))))

(ert-deftest agent-repl-cmd-test-create-or-update-pr-no-self-certified-paste/does-not-send ()
  "no-self-certified paste wrapper does not invoke `agent-repl--send-to-agent'."
  (let (send-called)
    (cl-letf (((symbol-function 'agent-repl--send-to-agent)
               (lambda (&rest _) (setq send-called t))))
      (with-temp-buffer
        (agent-repl-create-or-update-pr-no-self-certified-paste)
        (should-not send-called)))))

;;;; ---- agent-repl-copy-reference ----

(ert-deftest agent-repl-cmd-test-copy-reference/copies-to-kill-ring ()
  "copy-reference puts file:line reference on kill ring."
  (cl-letf (((symbol-function 'agent-repl--format-file-ref)
             (lambda () "src/foo.el:42")))
    (agent-repl-copy-reference)
    (should (equal (car kill-ring) "src/foo.el:42"))))

;;;; ---- agent-repl-paste-clipboard ----

(ert-deftest agent-repl-cmd-test-paste-clipboard/inserts-at-point ()
  "paste-clipboard inserts the workspace's `:clipboard' text at point."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" (list :clipboard "hello world") agent-repl--workspaces)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (with-temp-buffer
        (agent-repl-paste-clipboard)
        (should (equal (buffer-string) "hello world"))))))

(ert-deftest agent-repl-cmd-test-paste-clipboard/errors-when-unset ()
  "paste-clipboard signals user-error when no clipboard text is set."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
    (puthash "ws1" '() agent-repl--workspaces)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (with-temp-buffer
        (should-error (agent-repl-paste-clipboard) :type 'user-error)))))

(ert-deftest agent-repl-cmd-test-paste-clipboard/does-not-touch-os-clipboard ()
  "paste-clipboard inserts only at point — kill-ring is left untouched."
  (let ((agent-repl--workspaces (make-hash-table :test 'equal))
        (kill-ring '("pre-existing")))
    (puthash "ws1" (list :clipboard "ws-text") agent-repl--workspaces)
    (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws1")))
      (with-temp-buffer
        (agent-repl-paste-clipboard)
        (should (equal (car kill-ring) "pre-existing"))))))

;;;; ---- agent-repl--diff-command-form (macro expansion) ----

(ert-deftest agent-repl-cmd-test-diff-commands/explain-diff-worktree-exists ()
  "Macro generates agent-repl-explain-diff-worktree."
  (should (fboundp 'agent-repl-explain-diff-worktree)))

(ert-deftest agent-repl-cmd-test-diff-commands/explain-diff-branch-exists ()
  "Macro generates agent-repl-explain-diff-branch."
  (should (fboundp 'agent-repl-explain-diff-branch)))

(ert-deftest agent-repl-cmd-test-diff-commands/all-scopes-generated ()
  "Macro generates all 5 scope commands for each family."
  (dolist (family '("explain-diff" "update-pr-diff" "run-tests"
                    "run-lint" "run-all" "test-quality" "test-coverage"))
    (dolist (scope '("worktree" "staged" "uncommitted" "head" "branch"))
      (let ((fn (intern (format "agent-repl-%s-%s" family scope))))
        (should (fboundp fn))))))

(ert-deftest agent-repl-cmd-test-diff-commands/explain-diff-worktree-sends ()
  "explain-diff-worktree sends correct message."
  (let (sent-spec sent-prompt)
    (cl-letf (((symbol-function 'agent-repl--send-diff-analysis)
               (lambda (spec prompt) (setq sent-spec spec sent-prompt prompt))))
      (agent-repl-explain-diff-worktree)
      (should (equal sent-spec "unstaged changes (git diff)"))
      (should (equal sent-prompt agent-repl-explain-diff-prompt)))))

(ert-deftest agent-repl-cmd-test-diff-commands/update-pr-diff-uses-override ()
  "update-pr-diff-worktree uses scope override instead of default."
  (let (sent-spec)
    (cl-letf (((symbol-function 'agent-repl--send-diff-analysis)
               (lambda (spec _prompt) (setq sent-spec spec))))
      (agent-repl-update-pr-diff-worktree)
      ;; Should use the override from agent-repl--update-pr-diff-scopes
      (should (string-match-p "UNSTAGED" sent-spec)))))

(ert-deftest agent-repl-cmd-test-diff-commands/branch-uses-custom-var ()
  "explain-diff-branch uses the agent-repl-branch-diff-spec custom variable."
  (let (sent-spec
        (agent-repl-branch-diff-spec "custom branch spec"))
    (cl-letf (((symbol-function 'agent-repl--send-diff-analysis)
               (lambda (spec _prompt) (setq sent-spec spec))))
      (agent-repl-explain-diff-branch)
      (should (equal sent-spec "custom branch spec")))))

;;;; ---- Customization defaults ----

(ert-deftest agent-repl-cmd-test-customization-defaults ()
  "All custom prompt variables are non-empty strings."
  (dolist (var '(agent-repl-branch-diff-spec
                 agent-repl-explain-diff-prompt
                 agent-repl-update-pr-diff-prompt
                 agent-repl-update-pr-prompt
                 agent-repl-run-tests-prompt
                 agent-repl-run-lint-prompt
                 agent-repl-run-all-prompt
                 agent-repl-test-quality-prompt
                 agent-repl-test-coverage-prompt))
    (should (stringp (symbol-value var)))
    (should (> (length (symbol-value var)) 0))))

(ert-deftest agent-repl-cmd-test-base-flags-default ()
  "Default base flags are a non-empty list of strings."
  (should (listp agent-repl-create-or-update-pr-base-flags))
  (should (> (length agent-repl-create-or-update-pr-base-flags) 0))
  (should (cl-every #'stringp agent-repl-create-or-update-pr-base-flags)))

(ert-deftest agent-repl-cmd-test-base-flags-default/includes-rebase ()
  "Default base flags include --rebase."
  (should (member "--rebase" agent-repl-create-or-update-pr-base-flags)))

(ert-deftest agent-repl-cmd-test-base-flags-default/excludes-skip-tests ()
  "Default base flags do not include --skip-tests."
  (should-not (member "--skip-tests" agent-repl-create-or-update-pr-base-flags)))

;;;; ---- agent-repl-nuke-workspace ----

(ert-deftest agent-repl-cmd-test-nuke-workspace/no-workspaces ()
  "nuke-workspace signals user-error when no live agent-repl ws AND no
tab-bar ws are available — the picker has no candidates to offer."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names) (lambda () nil)))
      (should-error (agent-repl-nuke-workspace) :type 'user-error))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/kills-session-and-tombstones-hashmap ()
  "nuke-workspace kills session, kills persp workspace, and tombstones hashmap entry.
Post-tombstone-refactor, the hash entry survives with `:nuked-at' stamped
rather than being removed; `--ws-live-p' is the predicate that filters
tombstones out of the drawer/picker."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (agent-repl--ws-put "doomed" :status :done)
    (let ((session-killed nil)
          (persp-killed nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session)
                 (lambda (ws) (setq session-killed ws)))
                ((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-workspace)
        (should (equal session-killed "doomed"))
        (should (equal persp-killed "doomed"))
        (should-not (agent-repl--ws-live-p "doomed"))
        (should (agent-repl--ws-get "doomed" :nuked-at))))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/no-confirmation-prompt ()
  "nuke-workspace MUST NOT prompt for confirmation.  Teardown is
immediate — persisted state.el is preserved so accidental invocations
are recoverable by reopening the project."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((prompted nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (setq prompted t) t))
                ((symbol-function 'yes-or-no-p)
                 (lambda (_prompt) (setq prompted t) t))
                ((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-workspace)
        (should-not prompted)
        (should-not (agent-repl--ws-live-p "doomed"))
        (should (agent-repl--ws-get "doomed" :nuked-at))))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/kills-git-proc ()
  "nuke-workspace kills an in-flight git-diff process."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((proc-deleted nil)
          (fake-proc (start-process "fake" nil "true")))
      (agent-repl--ws-put "doomed" :git-proc fake-proc)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "other"))
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore)
                ;; Stub `process-live-p' since "true" exits before the test
                ;; reaches the kill check, racing us to the assertion.
                ((symbol-function 'process-live-p) (lambda (_p) t))
                ((symbol-function 'delete-process)
                 (lambda (p) (setq proc-deleted p))))
        (agent-repl-nuke-workspace)
        (should proc-deleted)))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/kills-persp-workspace ()
  "nuke-workspace calls +workspace/kill to tear down the persp workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((killed-ws nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "other"))
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq killed-ws ws)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-workspace)
        (should (equal killed-ws "doomed"))))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/no-persp-still-tombstones-hashmap ()
  "nuke-workspace tombstones hashmap entry even when persp workspace doesn't exist."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ghost" :project-dir "/tmp/ghost")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "ghost"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'agent-repl--kill-session) #'ignore)
              ((symbol-function '+workspace-exists-p) (lambda (_n) nil))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (agent-repl-nuke-workspace)
      (should-not (agent-repl--ws-live-p "ghost"))
      (should (agent-repl--ws-get "ghost" :nuked-at)))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/skips-persp-kill-when-workspace-already-gone ()
  "When the persp is already gone from the cache, nuke MUST NOT call
`+workspace/kill' — that call would emit the user-visible warning
`'<ws>' workspace doesn't exist' in the echo area.

Pins the regression seen after a successful workspace merge: the
async merge flow double-closes the workspace (preemptive close in
`--workspace-merge-async', then the deferred success-callback close
in `--workspace-merge-do'), and the second close arrives with the
persp already torn down.  The existence guard MUST short-circuit on
this second pass instead of falling through to `+workspace/kill'.

Crucial detail: persp-mode's real `persp-get-by-name' returns the
keyword `:nil' (i.e. `persp-not-persp', a TRUTHY value) when the
persp is missing — so a guard that gates on `(persp-get-by-name ws)'
truthiness would NOT short-circuit.  The current implementation uses
`+workspace-exists-p' (cache membership) instead, which correctly
returns nil for a missing workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ghost" :project-dir "/tmp/ghost")
    (let ((kill-called nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "ghost"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session) #'ignore)
                ;; Simulate persp-mode's actual broken-guard behavior:
                ;; persp-get-by-name returns `:nil' for a missing persp.
                ((symbol-function 'persp-get-by-name)
                 (lambda (&rest _) :nil))
                ;; Workspace not in the names cache — the real signal
                ;; for "doesn't exist" that `+workspace-exists-p' reads.
                ((symbol-function '+workspace-exists-p) (lambda (_n) nil))
                ((symbol-function '+workspace/kill)
                 (lambda (_ws) (setq kill-called t)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-workspace)
        (should-not kill-called)))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/tombstones-hashmap-when-kill-session-errors ()
  "nuke-workspace still tombstones the hashmap entry when kill-session errors.
The teardown error must not prevent the tombstone — otherwise the entry
would stay `live' from `--ws-live-p''s perspective while its runtime
state is corrupted, leaving the drawer/picker showing a half-dead row."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "doomed"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'agent-repl--kill-session)
               (lambda (_ws) (error "simulated kill-session failure")))
              ((symbol-function '+workspace-exists-p) (lambda (_n) nil))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (agent-repl-nuke-workspace)
      (should-not (agent-repl--ws-live-p "doomed"))
      (should (agent-repl--ws-get "doomed" :nuked-at)))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/tombstones-hashmap-when-workspace-kill-errors ()
  "nuke-workspace still tombstones the hashmap entry when +workspace/kill errors."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "doomed"))
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'agent-repl--kill-session) #'ignore)
              ((symbol-function '+workspace-current-name) (lambda () "other"))
              ((symbol-function '+workspace-exists-p) (lambda (_n) t))
              ((symbol-function '+workspace/kill)
               (lambda (_ws) (error "simulated workspace-kill failure")))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (agent-repl-nuke-workspace)
      (should-not (agent-repl--ws-live-p "doomed"))
      (should (agent-repl--ws-get "doomed" :nuked-at)))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/preserves-state-file ()
  "nuke-workspace MUST preserve the per-project state.el so the
captured session-id survives the in-memory teardown.  The next time
the same project is opened, `--initialize-ws-env' reads this file and
launches Claude with `--continue', resuming the prior session.  A
nuke that wipes state.el would force a fresh session each time, which
is the regression this test pins."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "agent-nuke-" t)))
      (unwind-protect
          (let ((state-file (agent-repl--state-file tmpdir)))
            (agent-repl-test--seed-file state-file "(:session-id \"keep-abc\")")
            (agent-repl--ws-put "doomed" :project-dir tmpdir)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt _coll &rest _) "doomed"))
                      ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                      ;; Stub kill-session so its embedded state-save can't
                      ;; rewrite the file with empty session-id — we want
                      ;; to verify the up-front state-save (or the no-purge
                      ;; guarantee) preserves the seeded contents.
                      ((symbol-function 'agent-repl--kill-session) #'ignore)
                      ;; Stub state-save too so the seeded content is what
                      ;; the test asserts on; this isolates the "no purge"
                      ;; property from the orthogonal "save before tear
                      ;; down" property tested separately below.
                      ((symbol-function 'agent-repl--state-save) #'ignore)
                      ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                      ((symbol-function 'force-mode-line-update) #'ignore))
              (agent-repl-nuke-workspace)
              (should (file-exists-p state-file))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/saves-state-before-teardown ()
  "nuke-workspace runs `--state-save' BEFORE any teardown so session-id
is persisted even if a downstream step (kill-session, ws-del, persp
kill) signals.  Order assertion: state-save called at least once
before kill-session."
  (agent-repl-test--with-clean-state
    (let ((events nil))
      (agent-repl--ws-put "doomed" :project-dir "/tmp/whatever")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--state-save)
                 (lambda (_ws) (push 'state-save events)))
                ((symbol-function 'agent-repl--kill-session)
                 (lambda (_ws) (push 'kill-session events)))
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-workspace)
        ;; Reverse so events are in chronological order.
        (let ((ordered (reverse events)))
          (should (memq 'state-save ordered))
          (should (memq 'kill-session ordered))
          ;; state-save must precede kill-session.
          (should (< (cl-position 'state-save ordered)
                     (cl-position 'kill-session ordered))))))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/kills-workspace-buffers ()
  "nuke-workspace invokes kill-workspace-buffers so every persp buffer is torn down."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((kwb-arg nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function 'agent-repl--kill-workspace-buffers)
                 (lambda (ws) (setq kwb-arg ws)))
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-workspace)
        (should (equal kwb-arg "doomed"))))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/kills-buffers-even-when-kill-session-errors ()
  "nuke-workspace still sweeps persp buffers when kill-session throws.
kill-workspace-buffers lives in the `unwind-protect' cleanup so the
buffer sweep is not skipped by an earlier teardown failure."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((kwb-called nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session)
                 (lambda (_ws) (error "simulated kill-session failure")))
                ((symbol-function 'agent-repl--kill-workspace-buffers)
                 (lambda (_ws) (setq kwb-called t)))
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-workspace)
        (should kwb-called)))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/workspace-kill-runs-after-buffer-sweep ()
  "nuke-workspace kills the persp buffers BEFORE tearing down the persp itself.
Reversing the order would make the buffer sweep a no-op because the
persp would already be gone before the buffer sweep ran."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((call-order nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function 'agent-repl--kill-workspace-buffers)
                 (lambda (_ws) (push 'kwb call-order)))
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (_ws) (push 'persp-kill call-order)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-workspace)
        (should (equal (nreverse call-order) '(kwb persp-kill)))))))

;;;; ---- agent-repl-nuke-all-workspaces ----

(ert-deftest agent-repl-cmd-test-nuke-all/no-workspaces ()
  "nuke-all-workspaces signals user-error when hashmap is empty."
  (agent-repl-test--with-clean-state
    (should-error (agent-repl-nuke-all-workspaces) :type 'user-error)))

(ert-deftest agent-repl-cmd-test-nuke-all/aborts-on-deny ()
  "nuke-all-workspaces does nothing when user answers no."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
      (should-error (agent-repl-nuke-all-workspaces) :type 'user-error)
      (should (gethash "ws1" agent-repl--workspaces))
      (should (gethash "ws2" agent-repl--workspaces)))))

(ert-deftest agent-repl-cmd-test-nuke-all/iterates-every-workspace ()
  "nuke-all-workspaces tears down every registered workspace."
  (agent-repl-test--with-clean-state
    (dolist (n '("ws1" "ws2" "ws3"))
      (agent-repl--ws-put n :project-dir (format "/tmp/%s" n)))
    (let ((torn-down nil)
          (persp-mode nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session)
                 (lambda (ws) (push ws torn-down)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-all-workspaces)
        (should (= 3 (length torn-down)))
        (should (member "ws1" torn-down))
        (should (member "ws2" torn-down))
        (should (member "ws3" torn-down))
        ;; Post-tombstone: hash entries survive with `:nuked-at' but no
        ;; entry remains live.  Use the live-name helper as the assertion.
        (should-not (agent-repl--live-ws-names))))))

(ert-deftest agent-repl-cmd-test-nuke-all/prompt-includes-count ()
  "nuke-all-workspaces' confirmation prompt includes the workspace count."
  (agent-repl-test--with-clean-state
    (dolist (n '("a" "b"))
      (agent-repl--ws-put n :project-dir (format "/tmp/%s" n)))
    (let ((seen-prompt nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (setq seen-prompt prompt) nil)))
        (ignore-errors (agent-repl-nuke-all-workspaces))
        (should (string-match-p "ALL 2" seen-prompt))))))

;;;; ---- agent-repl-nuke-restored-workspaces ----

(ert-deftest agent-repl-cmd-test-nuke-restored/no-restored ()
  "nuke-restored-workspaces errors when the restored set is empty."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (should-error (agent-repl-nuke-restored-workspaces) :type 'user-error)))

(ert-deftest agent-repl-cmd-test-nuke-restored/aborts-on-deny ()
  "nuke-restored-workspaces does nothing when user answers no."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (push "ws1" agent-repl--restored-workspaces)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
      (should-error (agent-repl-nuke-restored-workspaces) :type 'user-error)
      (should (agent-repl--ws-get "ws1" :project-dir)))))

(ert-deftest agent-repl-cmd-test-nuke-restored/only-restored-are-torn-down ()
  "nuke-restored-workspaces tears down only restored workspaces, sparing manual ones."
  (agent-repl-test--with-clean-state
    (dolist (n '("restored1" "restored2" "manual"))
      (agent-repl--ws-put n :project-dir (format "/tmp/%s" n)))
    (setq agent-repl--restored-workspaces '("restored1" "restored2"))
    (let ((torn-down nil)
          (persp-mode nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session)
                 (lambda (ws) (push ws torn-down)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-restored-workspaces)
        (should (= 2 (length torn-down)))
        (should (member "restored1" torn-down))
        (should (member "restored2" torn-down))
        (should-not (member "manual" torn-down))
        ;; Manual workspace must remain live with its identity intact.
        (should (agent-repl--ws-live-p "manual"))
        (should (agent-repl--ws-get "manual" :project-dir))
        ;; Restored entries are tombstoned, not removed — `:project-dir'
        ;; is preserved across tombstone (identity key), so assert the
        ;; live-p flip and the `:nuked-at' stamp instead.
        (should-not (agent-repl--ws-live-p "restored1"))
        (should-not (agent-repl--ws-live-p "restored2"))
        (should (agent-repl--ws-get "restored1" :nuked-at))
        (should (agent-repl--ws-get "restored2" :nuked-at))))))

(ert-deftest agent-repl-cmd-test-nuke-restored/prompt-includes-count ()
  "nuke-restored-workspaces' confirmation prompt includes the restored count."
  (agent-repl-test--with-clean-state
    (dolist (n '("a" "b" "c"))
      (agent-repl--ws-put n :project-dir (format "/tmp/%s" n)))
    (setq agent-repl--restored-workspaces '("a" "b" "c"))
    (let ((seen-prompt nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (setq seen-prompt prompt) nil)))
        (ignore-errors (agent-repl-nuke-restored-workspaces))
        (should (string-match-p "3 restored" seen-prompt))))))

(ert-deftest agent-repl-cmd-test-nuke-restored/skips-stale-names ()
  "nuke-restored-workspaces ignores names in the restored list with no live ws.
Avoids a user-error on the unprompted path when a name was removed from
the live hash (e.g., by individual nuke) but stayed on the list."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "live" :project-dir "/tmp/live")
    (setq agent-repl--restored-workspaces '("live" "stale"))
    (let ((torn-down nil)
          (persp-mode nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session)
                 (lambda (ws) (push ws torn-down)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-restored-workspaces)
        (should (equal torn-down '("live")))))))

(ert-deftest agent-repl-cmd-test-nuke-one/drops-from-restored-list ()
  "Individual nuke removes the ws from `agent-repl--restored-workspaces'."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
    (setq agent-repl--restored-workspaces '("ws1" "ws2"))
    (let ((persp-mode nil))
      (cl-letf (((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl--nuke-one-workspace "ws1")
        (should (equal agent-repl--restored-workspaces '("ws2")))))))

(ert-deftest agent-repl-cmd-test-nuke-one/preserve-entry-keeps-hash ()
  "Calling `--nuke-one-workspace' with PRESERVE-ENTRY non-nil retains
the hash entry so the drawer's MERGED bucket can render it.  Every
other teardown step still runs."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "merged-ws" :project-dir "/tmp/merged-ws")
    (agent-repl--ws-put "merged-ws" :merge-completed t)
    (let ((persp-mode nil))
      (cl-letf (((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl--nuke-one-workspace "merged-ws" 'preserve-entry)
        (should (gethash "merged-ws" agent-repl--workspaces))
        (should (eq (agent-repl--ws-get "merged-ws" :merge-completed) t))))))

(ert-deftest agent-repl-cmd-test-nuke-one/no-preserve-tombstones-hash ()
  "Default `--nuke-one-workspace' (no PRESERVE-ENTRY) tombstones the
hash entry.  Guards against an accidental flip of the default that
would leak live ws plists past teardown.  Post-tombstone-refactor the
entry survives with `:nuked-at' stamped — `--ws-live-p' is the
predicate that keeps it out of every UI/runtime iterator."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((persp-mode nil))
      (cl-letf (((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl--nuke-one-workspace "ws1")
        (should-not (agent-repl--ws-live-p "ws1"))
        (should (agent-repl--ws-get "ws1" :nuked-at))))))

;;;; ---- register-merged-workspace + state-merge-completed-p ----

(ert-deftest agent-repl-cmd-test-register-merged-workspace/populates-hash ()
  "`--register-merged-workspace' creates a hash entry with
`:project-dir' and `:merge-completed' t even when the on-disk state
file is absent — the snapshot loader uses this to surface MERGED
entries from a snapshot whose state.el was deleted out-of-band."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--state-file-for-read)
               (lambda (_) "/nonexistent/state.el")))
      (agent-repl--register-merged-workspace "merged-ws" "/tmp/merged")
      (should (gethash "merged-ws" agent-repl--workspaces))
      (should (equal (agent-repl--ws-get "merged-ws" :project-dir) "/tmp/merged"))
      (should (eq (agent-repl--ws-get "merged-ws" :merge-completed) t)))))

(ert-deftest agent-repl-cmd-test-register-merged-workspace/hydrates-from-state ()
  "`--register-merged-workspace' pulls per-ws fields from state.el when
present: priority, worktree-p, merge-completed-at, etc., so the
post-restart MERGED entry shows the same metadata it had before quit."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "agent-repl-state-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (prin1 '(:project-dir "/tmp/merged"
                       :priority "p1"
                       :worktree-p t
                       :merge-completed t
                       :merge-completed-at 1234567890.0
                       :last-prompt-summary "did the thing")
                     (current-buffer)))
            (cl-letf (((symbol-function 'agent-repl--state-file-for-read)
                       (lambda (_) tmp)))
              (agent-repl--register-merged-workspace "merged-ws" "/tmp/merged")
              (should (equal (agent-repl--ws-get "merged-ws" :priority) "p1"))
              (should (eq (agent-repl--ws-get "merged-ws" :worktree-p) t))
              (should (eq (agent-repl--ws-get "merged-ws" :merge-completed) t))
              (should (= (agent-repl--ws-get "merged-ws" :merge-completed-at)
                         1234567890.0))
              (should (equal (agent-repl--ws-get "merged-ws" :last-prompt-summary)
                             "did the thing"))))
        (delete-file tmp)))))

(ert-deftest agent-repl-cmd-test-register-merged-workspace/clean-sets-merged-state ()
  "When the on-disk state has `:merge-completed t' but no `:merge-failed'
\(or `:merge-failed nil') AND the backward-compat probe reports the
merge as landed, `--register-merged-workspace' sets `:repl-state' to
`:merged' and leaves `:merge-failed' clear so the drawer shows 🔀."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "agent-repl-state-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (prin1 '(:project-dir "/tmp/merged"
                       :merge-completed t)
                     (current-buffer)))
            (cl-letf (((symbol-function 'agent-repl--state-file-for-read)
                       (lambda (_) tmp))
                      ((symbol-function 'agent-repl--detect-merge-actually-landed-p)
                       (lambda (_ws) t)))
              (agent-repl--register-merged-workspace "merged-ws" "/tmp/merged")
              (should (eq (agent-repl--ws-get "merged-ws" :repl-state) :merged))
              (should-not (agent-repl--ws-get "merged-ws" :merge-failed))))
        (delete-file tmp)))))

(ert-deftest agent-repl-cmd-test-register-merged-workspace/restores-persisted-merge-failed ()
  "When the on-disk state explicitly carries `:merge-failed t', the
registered workspace adopts `:repl-state :merge-failed' regardless of
the probe's current verdict — the user's prior signal is authoritative
and should not be overwritten by an opportunistic post-restart probe."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "agent-repl-state-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (prin1 '(:project-dir "/tmp/merged"
                       :merge-completed t
                       :merge-failed t)
                     (current-buffer)))
            (cl-letf (((symbol-function 'agent-repl--state-file-for-read)
                       (lambda (_) tmp))
                      ((symbol-function 'agent-repl--detect-merge-actually-landed-p)
                       (lambda (_ws) t)))
              (agent-repl--register-merged-workspace "merged-ws" "/tmp/merged")
              (should (eq (agent-repl--ws-get "merged-ws" :merge-failed) t))
              (should (eq (agent-repl--ws-get "merged-ws" :repl-state) :merge-failed))))
        (delete-file tmp)))))

(ert-deftest agent-repl-cmd-test-register-merged-workspace/probe-promotes-to-merge-failed ()
  "When the on-disk state has `:merge-completed t' but no
`:merge-failed' (legacy snapshot from before the flag existed) AND the
backward-compat probe reports the merge as NOT landed, the registered
workspace is promoted to `:repl-state :merge-failed' / `:merge-failed t'
so the drawer ❌ badge appears on first load even though no prior run
recorded the failure."
  (agent-repl-test--with-clean-state
    (let ((tmp (make-temp-file "agent-repl-state-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (prin1 '(:project-dir "/tmp/merged"
                       :merge-completed t)
                     (current-buffer)))
            (cl-letf (((symbol-function 'agent-repl--state-file-for-read)
                       (lambda (_) tmp))
                      ((symbol-function 'agent-repl--detect-merge-actually-landed-p)
                       (lambda (_ws) nil)))
              (agent-repl--register-merged-workspace "merged-ws" "/tmp/merged")
              (should (eq (agent-repl--ws-get "merged-ws" :merge-failed) t))
              (should (eq (agent-repl--ws-get "merged-ws" :repl-state) :merge-failed))))
        (delete-file tmp)))))

;;;; ---- snapshot-load: merge-failed restore -> establish + front-reorder ----

(defun agent-repl-test--write-merge-state (dir &rest extra-plist)
  "Write a state.el under DIR (under .claude/emacs/) with :merge-completed t.
Extra plist entries are merged into the file's plist."
  (let* ((emacs-dir (expand-file-name ".claude/emacs/" dir))
         (state-file (expand-file-name "state.el" emacs-dir))
         (plist (append (list :project-dir dir :merge-completed t)
                        extra-plist)))
    (make-directory emacs-dir t)
    (with-temp-file state-file
      (prin1 plist (current-buffer)))
    state-file))

(ert-deftest agent-repl-cmd-test-snapshot-load/merge-failed-establishes-and-fronts ()
  "When a snapshot entry's state.el is `:merge-completed t' AND the
register-merged probe flips `:merge-failed t', the loader promotes the
entry from drawer-only to a real tab-bar workspace via
`--establish-workspace' and moves it to the front of `persp-names-cache'
via `--reorder-workspace-to-front'."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (failed-dir (make-temp-file "agent-proj-failed-" t))
          (establish-calls nil)
          (front-calls nil))
      (agent-repl-test--write-merge-state failed-dir :merge-failed t)
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("failed-ws" . ,failed-dir)))
            (cl-letf (((symbol-function 'agent-repl--detect-merge-actually-landed-p)
                       (lambda (_ws) t))
                      ((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir) (push ws establish-calls)))
                      ((symbol-function 'agent-repl--reorder-workspace-to-front)
                       (lambda (ws) (push ws front-calls)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              (should (equal establish-calls '("failed-ws")))
              (should (equal front-calls '("failed-ws")))))
        (delete-file snapshot-file)
        (delete-directory failed-dir t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/clean-merge-stays-data-only ()
  "When a snapshot entry's state.el is `:merge-completed t' AND the
probe confirms the merge landed (no `:merge-failed'), the loader does
NOT call establish-workspace or reorder-to-front — clean merges stay in
the drawer-only MERGED bucket."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (clean-dir (make-temp-file "agent-proj-clean-" t))
          (establish-calls nil)
          (front-calls nil))
      (agent-repl-test--write-merge-state clean-dir)
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("clean-ws" . ,clean-dir)))
            (cl-letf (((symbol-function 'agent-repl--detect-merge-actually-landed-p)
                       (lambda (_ws) t))
                      ((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir) (push ws establish-calls)))
                      ((symbol-function 'agent-repl--reorder-workspace-to-front)
                       (lambda (ws) (push ws front-calls)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              (should-not establish-calls)
              (should-not front-calls)))
        (delete-file snapshot-file)
        (delete-directory clean-dir t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/probe-detected-failure-fronts ()
  "When state.el has `:merge-completed t' but NO `:merge-failed' flag
and the git-landing probe reports NOT landed (legacy silent failure),
the loader still promotes the entry to a real tab-bar workspace at the
front — the probe is authoritative for unflagged legacy state."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (probe-dir (make-temp-file "agent-proj-probe-" t))
          (establish-calls nil)
          (front-calls nil))
      (agent-repl-test--write-merge-state probe-dir)
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("probe-ws" . ,probe-dir)))
            (cl-letf (((symbol-function 'agent-repl--detect-merge-actually-landed-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir) (push ws establish-calls)))
                      ((symbol-function 'agent-repl--reorder-workspace-to-front)
                       (lambda (ws) (push ws front-calls)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              (should (equal establish-calls '("probe-ws")))
              (should (equal front-calls '("probe-ws")))))
        (delete-file snapshot-file)
        (delete-directory probe-dir t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/merge-failed-establish-error-isolated ()
  "An error inside the failed-merge restore's establish-workspace must
not abort the snapshot loader — the surrounding `condition-case'
swallows the signal so subsequent queue entries still get processed."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (failed-dir (make-temp-file "agent-proj-failed-" t))
          (later-dir (make-temp-file "agent-proj-later-" t))
          (front-calls nil))
      (agent-repl-test--write-merge-state failed-dir :merge-failed t)
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("failed-ws" . ,failed-dir)
                                            ("later-ws" . ,later-dir)))
            (cl-letf (((symbol-function 'agent-repl--detect-merge-actually-landed-p)
                       (lambda (_ws) t))
                      ((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir)
                         (when (equal ws "failed-ws")
                           (error "boom-during-establish"))))
                      ((symbol-function 'agent-repl--reorder-workspace-to-front)
                       (lambda (ws) (push ws front-calls)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              ;; Must not signal — error is swallowed inside the loader.
              (agent-repl-load-workspace-snapshot)
              ;; reorder-to-front was never reached because establish threw.
              (should-not front-calls)))
        (delete-file snapshot-file)
        (delete-directory failed-dir t)
        (delete-directory later-dir t)))))

(ert-deftest agent-repl-cmd-test-state-merge-completed-p/detects-flag ()
  "`--state-merge-completed-p' returns t when state.el carries
`:merge-completed' t and nil otherwise.  Powers the snapshot loader's
route-to-register-merged branch."
  (let ((tmp-merged (make-temp-file "agent-repl-state-" nil ".el"))
        (tmp-plain  (make-temp-file "agent-repl-state-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp-merged
            (prin1 '(:project-dir "/x" :merge-completed t) (current-buffer)))
          (with-temp-file tmp-plain
            (prin1 '(:project-dir "/x") (current-buffer)))
          (cl-letf (((symbol-function 'agent-repl--state-file-for-read)
                     (lambda (d) (cond ((equal d "/merged") tmp-merged)
                                       ((equal d "/plain")  tmp-plain)
                                       (t nil)))))
            (should (agent-repl--state-merge-completed-p "/merged"))
            (should-not (agent-repl--state-merge-completed-p "/plain"))))
      (delete-file tmp-merged)
      (delete-file tmp-plain))))

;;;; ---- agent-repl-kill-workspace ----

(ert-deftest agent-repl-cmd-test-kill-workspace/no-workspaces ()
  "kill-workspace signals user-error when no live agent-repl ws AND no
tab-bar ws are available — the picker has no candidates to offer."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function '+workspace-list-names) (lambda () nil)))
      (should-error (agent-repl-kill-workspace) :type 'user-error))))

(ert-deftest agent-repl-cmd-test-kill-workspace/no-confirmation-prompt ()
  "kill-workspace MUST NOT prompt for confirmation.  Teardown is
immediate — persisted state.el is preserved so accidental invocations
are recoverable by reopening the project."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((prompted nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) (setq prompted t) t))
                ((symbol-function 'yes-or-no-p)
                 (lambda (_prompt) (setq prompted t) t))
                ((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-kill-workspace)
        (should-not prompted)
        (should-not (agent-repl--ws-live-p "doomed"))
        (should (agent-repl--ws-get "doomed" :nuked-at))))))

(ert-deftest agent-repl-cmd-test-kill-workspace/kills-session-and-tombstones-hashmap ()
  "kill-workspace kills session, kills persp workspace, and tombstones hashmap entry.
Same tombstone semantics as nuke — `--ws-del' is the single teardown
primitive both routes through."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (agent-repl--ws-put "doomed" :status :done)
    (let ((session-killed nil)
          (persp-killed nil)
          (persp-mode t))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session)
                 (lambda (ws) (setq session-killed ws)))
                ((symbol-function '+workspace-current-name) (lambda () "other-ws"))
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-kill-workspace)
        (should (equal session-killed "doomed"))
        (should (equal persp-killed "doomed"))
        (should-not (agent-repl--ws-live-p "doomed"))
        (should (agent-repl--ws-get "doomed" :nuked-at))))))

(ert-deftest agent-repl-cmd-test-kill-workspace/preserves-state-file ()
  "kill-workspace must NOT unlink the .agent-repl-state file.
This is the whole point of the kill (vs nuke) split: priority and
per-environment session-id live in that file and need to survive a
kill so the workspace can be re-opened with its identity intact."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (make-temp-file "agent-kill-" t)))
      (unwind-protect
          (let ((state-file (agent-repl--state-file tmpdir)))
            (agent-repl-test--seed-file state-file "(:session-id \"keep-me\")")
            (agent-repl--ws-put "doomed" :project-dir tmpdir)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt _coll &rest _) "doomed"))
                      ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                      ((symbol-function 'agent-repl--kill-session) #'ignore)
                      ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                      ((symbol-function 'force-mode-line-update) #'ignore))
              (agent-repl-kill-workspace)
              (should (file-exists-p state-file))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-cmd-test-kill-workspace/kills-workspace-buffers ()
  "kill-workspace invokes kill-workspace-buffers so every persp buffer is torn down."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((kwb-arg nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function 'agent-repl--kill-workspace-buffers)
                 (lambda (ws) (setq kwb-arg ws)))
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-kill-workspace)
        (should (equal kwb-arg "doomed"))))))

(ert-deftest agent-repl-cmd-test-kill-workspace/kills-git-proc ()
  "kill-workspace kills an in-flight git-diff process."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doomed" :project-dir "/tmp/doomed")
    (let ((proc-deleted nil)
          (fake-proc (start-process "fake" nil "true")))
      (agent-repl--ws-put "doomed" :git-proc fake-proc)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "doomed"))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function '+workspace-current-name) (lambda () "other"))
                ((symbol-function 'persp-get-by-name) (lambda (_n) nil))
                ((symbol-function 'force-mode-line-update) #'ignore)
                ((symbol-function 'process-live-p) (lambda (_p) t))
                ((symbol-function 'delete-process)
                 (lambda (p) (setq proc-deleted p))))
        (agent-repl-kill-workspace)
        (should proc-deleted)))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/tabbar-only-routes-to-persp-kill ()
  "nuke-workspace on a tab-bar-only ws (agent already killed) routes
through `+workspace/kill' and does NOT call the agent-repl teardown.
The ws has no live `agent-repl--workspaces' entry but its persp is
still in `+workspace-list-names', so the picker offers it and the
dispatcher chooses the plain-kill branch."
  (agent-repl-test--with-clean-state
    (let ((persp-killed nil)
          (kill-session-called nil)
          (persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("stray-persp")))
                ((symbol-function '+workspace-current-name) (lambda () "main"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "stray-persp"))
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws)))
                ((symbol-function 'agent-repl--kill-session)
                 (lambda (_ws) (setq kill-session-called t)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-workspace)
        (should (equal persp-killed "stray-persp"))
        ;; The agent-repl teardown MUST NOT run for a non-live ws.
        (should-not kill-session-called)))))

(ert-deftest agent-repl-cmd-test-kill-workspace/tabbar-only-routes-to-persp-kill ()
  "kill-workspace on a tab-bar-only ws routes through `+workspace/kill'
\(symmetric with the nuke-workspace test above)."
  (agent-repl-test--with-clean-state
    (let ((persp-killed nil)
          (kill-session-called nil)
          (persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("stray-persp")))
                ((symbol-function '+workspace-current-name) (lambda () "main"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "stray-persp"))
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws)))
                ((symbol-function 'agent-repl--kill-session)
                 (lambda (_ws) (setq kill-session-called t)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-kill-workspace)
        (should (equal persp-killed "stray-persp"))
        (should-not kill-session-called)))))

(ert-deftest agent-repl-cmd-test-nuke-workspace/tombstoned-with-persp-routes-to-persp-kill ()
  "nuke-workspace on a tombstoned ws (agent killed but persp still in
tab-bar) routes through `+workspace/kill'.  The hash entry already has
`:nuked-at' set so `--ws-live-p' returns nil; the picker still offers
the ws via the tab-bar branch and the dispatcher does the plain kill."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "tomb" :project-dir "/tmp/tomb")
    (agent-repl--ws-put "tomb" :nuked-at (current-time))
    (let ((persp-killed nil)
          (kill-session-called nil)
          (persp-mode t))
      (cl-letf (((symbol-function '+workspace-list-names)
                 (lambda () '("tomb")))
                ((symbol-function '+workspace-current-name) (lambda () "main"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "tomb"))
                ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq persp-killed ws)))
                ((symbol-function 'agent-repl--kill-session)
                 (lambda (_ws) (setq kill-session-called t)))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl-nuke-workspace)
        (should (equal persp-killed "tomb"))
        (should-not kill-session-called)))))

;;;; ---- Tests: workspace snapshot save/load ----

(ert-deftest agent-repl-cmd-test-save-workspace-snapshot/writes-entries ()
  "save-workspace-snapshot writes `(NAME :project-dir DIR)' entries."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
            (agent-repl-save-workspace-snapshot)
            (let ((data (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
              (should (= 2 (length data)))
              (should (equal (plist-get (cdr (assoc "ws1" data)) :project-dir) "/tmp/ws1"))
              (should (equal (plist-get (cdr (assoc "ws2" data)) :project-dir) "/tmp/ws2"))))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-save-workspace-snapshot/skips-missing-project-dir ()
  "save-workspace-snapshot omits workspaces with no :project-dir."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (agent-repl--ws-put "ws2" :vterm-buffer nil)
            (agent-repl-save-workspace-snapshot)
            (let ((data (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
              (should (= 1 (length data)))
              (should (equal (plist-get (cdr (assoc "ws1" data)) :project-dir) "/tmp/ws1"))
              (should-not (assoc "ws2" data))))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-update-workspace-snapshot/forces-write-bypassing-safe-guard ()
  "update-workspace-snapshot writes the live roster even when the loader
hasn't run and the on-disk roster is larger — `save-workspace-snapshot'
aborts in that case, but the explicit update command bypasses the
safety check after a user confirmation."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file)
                (agent-repl--snapshot-loaded-p nil))
            ;; Seed a richer on-disk roster than the live hash.
            (agent-repl--write-sexp-file snapshot-file
                                          '(("old-a" :project-dir "/tmp/old-a")
                                            ("old-b" :project-dir "/tmp/old-b")
                                            ("old-c" :project-dir "/tmp/old-c")))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
              (agent-repl-update-workspace-snapshot))
            (let ((data (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
              (should (= 1 (length data)))
              (should (equal (plist-get (cdr (assoc "ws1" data)) :project-dir) "/tmp/ws1"))))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-update-workspace-snapshot/aborts-on-shrink-decline ()
  "update-workspace-snapshot aborts when the user declines the shrink
confirmation, leaving the on-disk file untouched."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          '(("old-a" :project-dir "/tmp/old-a")
                                            ("old-b" :project-dir "/tmp/old-b")))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
              (should-error (agent-repl-update-workspace-snapshot)
                            :type 'user-error))
            ;; File contents are unchanged — still the legacy seed layout.
            (let ((data (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
              (should (= 2 (length data)))))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-update-workspace-snapshot/no-prompt-when-not-shrinking ()
  "update-workspace-snapshot writes without confirmation when the live
roster is at least as large as the on-disk one."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (prompted nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          '(("old-a" :project-dir "/tmp/old-a")))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_prompt) (setq prompted t) t)))
              (agent-repl-update-workspace-snapshot))
            (should-not prompted)
            (let ((data (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
              (should (= 2 (length data)))))
        (delete-file snapshot-file)))))

;;;; ---- Tests: snapshot save/load preserves tab-bar order ----

(ert-deftest agent-repl-cmd-test-collect-snapshot-entries/orders-by-persp-cache ()
  "`--collect-snapshot-entries' returns entries in `persp-names-cache' order
so the saved snapshot mirrors the tab-bar order at save time — the third
tab when saving is the third entry on disk, and on subsequent load the
loader processes entries in file order, preserving the visual order
across Emacs restarts."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha"   :project-dir "/tmp/alpha")
    (agent-repl--ws-put "bravo"   :project-dir "/tmp/bravo")
    (agent-repl--ws-put "charlie" :project-dir "/tmp/charlie")
    (let ((persp-names-cache '("bravo" "alpha" "charlie")))
      (let ((entries (agent-repl--collect-snapshot-entries)))
        (should (equal (mapcar #'car entries) '("bravo" "alpha" "charlie")))))))

(ert-deftest agent-repl-cmd-test-collect-snapshot-entries/skips-persp-nil-name ()
  "Entries are taken from `persp-names-cache' in order, but `persp-nil-name'
\(the sentinel persp-mode keeps at the cache head) is skipped so it never
shows up in the snapshot."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :project-dir "/tmp/alpha")
    (agent-repl--ws-put "bravo" :project-dir "/tmp/bravo")
    (let ((persp-nil-name "none")
          (persp-names-cache '("none" "bravo" "alpha")))
      (let ((entries (agent-repl--collect-snapshot-entries)))
        (should (equal (mapcar #'car entries) '("bravo" "alpha")))))))

(ert-deftest agent-repl-cmd-test-collect-snapshot-entries/tombstoned-orphan-included-from-remainder ()
  "A tombstoned workspace NOT in `persp-names-cache' IS included after the
live prefix so its identity record survives restart."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :project-dir "/tmp/alpha")
    (puthash "orphan-tomb"
             (list :project-dir "/tmp/orphan" :nuked-at (current-time))
             agent-repl--workspaces)
    (let ((persp-names-cache '("alpha")))
      (let* ((entries (agent-repl--collect-snapshot-entries))
             (names (mapcar #'car entries)))
        (should (= 2 (length entries)))
        (should (equal (car names) "alpha"))
        (should (member "orphan-tomb" names))))))

(ert-deftest agent-repl-cmd-test-collect-snapshot-entries/live-orphan-excluded-when-cache-bound ()
  "A live workspace in the hash but NOT in `persp-names-cache' is dropped
when the cache is bound — saving it live would re-establish it as a new
tab on the next load even though it had no tab-bar presence at save time."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha"  :project-dir "/tmp/alpha")
    (agent-repl--ws-put "orphan" :project-dir "/tmp/orphan")
    (let ((persp-names-cache '("alpha")))
      (let* ((entries (agent-repl--collect-snapshot-entries))
             (names (mapcar #'car entries)))
        (should (= 1 (length entries)))
        (should (equal names '("alpha")))
        (should-not (assoc "orphan" entries))))))

(ert-deftest agent-repl-cmd-test-collect-snapshot-entries/no-persp-cache-falls-back ()
  "When `persp-names-cache' is nil (persp-mode not active or stubs), the
collector still emits every live entry — order is hash-traversal under
that fallback, but the entries themselves must not be dropped."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "alpha" :project-dir "/tmp/alpha")
    (agent-repl--ws-put "bravo" :project-dir "/tmp/bravo")
    ;; persp-names-cache is nil in the test-helpers stub — no explicit
    ;; binding needed; the default nil value triggers the fallback path.
    (let ((entries (agent-repl--collect-snapshot-entries)))
      (should (= 2 (length entries)))
      (should (assoc "alpha" entries))
      (should (assoc "bravo" entries)))))

(ert-deftest agent-repl-cmd-test-save-workspace-snapshot/persists-tab-bar-order ()
  "End-to-end: `save-workspace-snapshot' writes entries in the on-disk file
in the same order as `persp-names-cache', so a subsequent `read' returns
the workspaces in tab-bar order."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file)
                (persp-names-cache '("third" "first" "second")))
            (agent-repl--ws-put "first"  :project-dir "/tmp/first")
            (agent-repl--ws-put "second" :project-dir "/tmp/second")
            (agent-repl--ws-put "third"  :project-dir "/tmp/third")
            (agent-repl-save-workspace-snapshot)
            (let ((data (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :workspaces)))
              (should (equal (mapcar #'car data) '("third" "first" "second")))))
        (delete-file snapshot-file)))))

;;;; ---- Tests: snapshot hide-project-dirs persistence ----

(ert-deftest agent-repl-cmd-test-collect-snapshot-entries/hidden-tombstone-carries-marker ()
  "`--collect-snapshot-entries' tags a tombstone killed by the
hide-project-dirs toggle with `:hidden-project-dir' so the next session
can tell it apart from a hand-nuked workspace."
  (agent-repl-test--with-clean-state
    (puthash "ws-cc"
             (list :project-dir "/tmp/cc"
                   :nuked-at (current-time)
                   :hidden-project-dir t)
             agent-repl--workspaces)
    (let* ((entries (agent-repl--collect-snapshot-entries))
           (plist (cdr (assoc "ws-cc" entries))))
      (should (plist-get plist :nuked-at))
      (should (eq (plist-get plist :hidden-project-dir) t)))))

(ert-deftest agent-repl-cmd-test-collect-snapshot-entries/plain-tombstone-omits-marker ()
  "`--collect-snapshot-entries' does NOT add `:hidden-project-dir' to a
tombstone the user nuked by hand (no hide marker on the live plist)."
  (agent-repl-test--with-clean-state
    (puthash "ws-tomb"
             (list :project-dir "/tmp/t" :nuked-at (current-time))
             agent-repl--workspaces)
    (let* ((entries (agent-repl--collect-snapshot-entries))
           (plist (cdr (assoc "ws-tomb" entries))))
      (should (plist-get plist :nuked-at))
      (should-not (plist-member plist :hidden-project-dir)))))

(ert-deftest agent-repl-cmd-test-write-workspace-snapshot/round-trips-hide-enabled ()
  "A snapshot written while `agent-repl-hide-project-dirs-enabled' is t
reads back with `:hide-project-dirs-enabled' t."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (agent-repl-hide-project-dirs-enabled t))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (agent-repl-save-workspace-snapshot)
            (should (eq (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :hide-project-dirs-enabled)
                        t)))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-write-workspace-snapshot/round-trips-hide-disabled ()
  "A snapshot written while `agent-repl-hide-project-dirs-enabled' is nil
reads back with `:hide-project-dirs-enabled' nil."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (agent-repl-hide-project-dirs-enabled nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (agent-repl-save-workspace-snapshot)
            (should-not (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :hide-project-dirs-enabled)))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-read-workspace-snapshot/legacy-format-has-no-hide-flag ()
  "Reading a legacy list-of-entries snapshot reports `:hide-project-dirs-enabled'
as nil — the key predates that format."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file snapshot-file
                                          '(("ws1" :project-dir "/tmp/ws1")))
            (should-not (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :hide-project-dirs-enabled)))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/restores-hide-flag ()
  "load-workspace-snapshot restores `agent-repl-hide-project-dirs-enabled'
from the snapshot's `:hide-project-dirs-enabled' key."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (agent-repl-hide-project-dirs-enabled nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file
             snapshot-file
             `(:workspaces (("ws-cc" :project-dir "/tmp/cc"
                             :nuked-at ,(current-time)
                             :hidden-project-dir t))
               :hide-project-dirs-enabled t))
            (agent-repl-load-workspace-snapshot)
            (should (eq agent-repl-hide-project-dirs-enabled t)))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-snapshot-plist-key-from-raw/reads-key-from-plist ()
  "The shared plist-key reader returns KEY's value from a plist-shaped raw."
  ;; Act / Assert
  (should (eq (agent-repl--snapshot-plist-key-from-raw
               '(:workspaces nil :default-frontend gui) :default-frontend)
              'gui)))

(ert-deftest agent-repl-cmd-test-snapshot-plist-key-from-raw/absent-key-is-nil ()
  "A plist-shaped raw that simply lacks KEY reads as nil."
  ;; Act / Assert
  (should-not (agent-repl--snapshot-plist-key-from-raw
               '(:workspaces nil) :default-frontend)))

(ert-deftest agent-repl-cmd-test-snapshot-plist-key-from-raw/legacy-raw-is-nil ()
  "A legacy list-of-entries raw reads as nil for any plist-only key —
`legacy' and `key absent' collapse to the same answer."
  ;; Act / Assert
  (should-not (agent-repl--snapshot-plist-key-from-raw
               '(("ws1" :project-dir "/tmp/ws1")) :default-frontend)))

(ert-deftest agent-repl-cmd-test-write-workspace-snapshot/round-trips-default-frontend ()
  "A snapshot records the live `agent-repl-default-frontend' and reads it
back as `:default-frontend'."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (agent-repl-default-frontend 'gui))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (agent-repl-save-workspace-snapshot)
            (should (eq (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :default-frontend)
                        'gui)))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-read-workspace-snapshot/legacy-format-has-no-default-frontend ()
  "Reading a legacy list-of-entries snapshot reports `:default-frontend' as
nil — the key predates that format."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (progn
            (agent-repl--write-sexp-file snapshot-file
                                          '(("ws1" :project-dir "/tmp/ws1")))
            (should-not (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                   :default-frontend)))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/restores-default-frontend ()
  "load-workspace-snapshot restores `agent-repl-default-frontend' from the
snapshot's `:default-frontend' key, so workspaces created after a restart
are born under the last-chosen frontend."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (agent-repl-default-frontend 'vterm))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file
             snapshot-file
             `(:workspaces (("ws-cc" :project-dir "/tmp/cc"
                             :nuked-at ,(current-time)))
               :default-frontend gui))
            (agent-repl-load-workspace-snapshot)
            (should (eq agent-repl-default-frontend 'gui)))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/keeps-default-frontend-when-absent ()
  "A snapshot predating `:default-frontend' leaves the customized
`agent-repl-default-frontend' alone rather than stomping it with nil."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (agent-repl-default-frontend 'gui))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file
             snapshot-file
             `(:workspaces (("ws-cc" :project-dir "/tmp/cc"
                             :nuked-at ,(current-time)))))
            (agent-repl-load-workspace-snapshot)
            (should (eq agent-repl-default-frontend 'gui)))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/restores-hidden-tombstone-marker ()
  "load-workspace-snapshot carries `:hidden-project-dir' onto the restored
tombstone so a later unhide can re-establish it."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (agent-repl-hide-project-dirs-enabled nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file
             snapshot-file
             `(:workspaces (("ws-cc" :project-dir "/tmp/cc"
                             :nuked-at ,(current-time)
                             :hidden-project-dir t))
               :hide-project-dirs-enabled t))
            (agent-repl-load-workspace-snapshot)
            (should (agent-repl--ws-get "ws-cc" :hidden-project-dir))
            (should (agent-repl--ws-get "ws-cc" :nuked-at)))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/skips-priority-reorder-during-snapshot-load ()
  "`--establish-workspace' must NOT call `--reorder-workspace-by-priority'
while a snapshot load is in flight (`agent-repl--snapshot-load-state'
non-nil) — the loader visits entries in saved tab-bar order, and a
per-entry priority reseating would shuffle them back into priority
order, defeating the order preservation `--collect-snapshot-entries'
encodes on save."
  (agent-repl-test--with-clean-state
    (let ((reorder-calls nil))
      (cl-letf (((symbol-function 'persp-add-new)
                 (lambda (_ws) nil))
                ((symbol-function 'persp-frame-switch)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--clean-frame-foreign-windows)
                 (lambda (_ws) nil))
                ((symbol-function 'projectile-add-known-project)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--most-recent-project-file)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--load-display-state)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (ws) (push ws reorder-calls)))
                ((symbol-function 'agent-repl--initialize-agent)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--agent-running-p)
                 (lambda (_ws) t)))
        ;; Simulate an in-flight snapshot load.
        (let ((agent-repl--snapshot-load-state '(:queue nil)))
          (agent-repl--establish-workspace "ws-a" "/tmp/ws-a"))
        (should-not reorder-calls)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/applies-priority-reorder-outside-snapshot-load ()
  "Outside a snapshot load (`agent-repl--snapshot-load-state' nil),
`--establish-workspace' still applies the priority reorder — drawer-driven
restores and worktree hydration paths depend on the priority-slot
behavior for ad-hoc creations."
  (agent-repl-test--with-clean-state
    (let ((reorder-calls nil))
      (cl-letf (((symbol-function 'persp-add-new)
                 (lambda (_ws) nil))
                ((symbol-function 'persp-frame-switch)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--clean-frame-foreign-windows)
                 (lambda (_ws) nil))
                ((symbol-function 'projectile-add-known-project)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--most-recent-project-file)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--load-display-state)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (ws) (push ws reorder-calls)))
                ((symbol-function 'agent-repl--initialize-agent)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--agent-running-p)
                 (lambda (_ws) t)))
        (let ((agent-repl--snapshot-load-state nil))
          (agent-repl--establish-workspace "ws-a" "/tmp/ws-a"))
        (should (equal reorder-calls '("ws-a")))))))

;;;; ---- Tests: hydrate-and-reorder-on-open (shared opener step) ----

(ert-deftest agent-repl-cmd-test-hydrate-and-reorder-on-open/loads-before-reorder ()
  "`--hydrate-and-reorder-on-open' hydrates display state BEFORE reseating
by priority, so the reorder reads a freshly-hydrated `:priority' rather
than a stale one."
  (agent-repl-test--with-clean-state
    (let ((events nil))
      (cl-letf (((symbol-function 'agent-repl--load-display-state)
                 (lambda (&rest _) (push 'load events)))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (_ws) (push 'reorder events))))
        (let ((agent-repl--snapshot-load-state nil))
          (agent-repl--hydrate-and-reorder-on-open "ws-a" "/tmp/ws-a"))
        (let ((ordered (reverse events)))
          (should (< (cl-position 'load ordered)
                     (cl-position 'reorder ordered))))))))

(ert-deftest agent-repl-cmd-test-hydrate-and-reorder-on-open/passes-ws-and-root-to-load ()
  "`--hydrate-and-reorder-on-open' forwards its WS and PROJECT-ROOT
arguments verbatim to `--load-display-state'."
  (agent-repl-test--with-clean-state
    (let ((load-args nil))
      (cl-letf (((symbol-function 'agent-repl--load-display-state)
                 (lambda (ws root) (setq load-args (list ws root))))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (_ws) nil)))
        (let ((agent-repl--snapshot-load-state nil))
          (agent-repl--hydrate-and-reorder-on-open "ws-a" "/tmp/ws-a"))
        (should (equal load-args '("ws-a" "/tmp/ws-a")))))))

(ert-deftest agent-repl-cmd-test-hydrate-and-reorder-on-open/reorders-target-ws ()
  "`--hydrate-and-reorder-on-open' reseats the WS it was given (not some
other) when no snapshot load is in flight."
  (agent-repl-test--with-clean-state
    (let ((reorder-calls nil))
      (cl-letf (((symbol-function 'agent-repl--load-display-state)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (ws) (push ws reorder-calls))))
        (let ((agent-repl--snapshot-load-state nil))
          (agent-repl--hydrate-and-reorder-on-open "ws-a" "/tmp/ws-a"))
        (should (equal reorder-calls '("ws-a")))))))

(ert-deftest agent-repl-cmd-test-hydrate-and-reorder-on-open/skips-reorder-during-snapshot-load ()
  "`--hydrate-and-reorder-on-open' must NOT reseat by priority while a
snapshot load is in flight (`agent-repl--snapshot-load-state' non-nil),
preserving the loader's saved tab-bar order."
  (agent-repl-test--with-clean-state
    (let ((reorder-calls nil))
      (cl-letf (((symbol-function 'agent-repl--load-display-state)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (ws) (push ws reorder-calls))))
        (let ((agent-repl--snapshot-load-state '(:queue nil)))
          (agent-repl--hydrate-and-reorder-on-open "ws-a" "/tmp/ws-a"))
        (should-not reorder-calls)))))

(ert-deftest agent-repl-cmd-test-hydrate-and-reorder-on-open/still-hydrates-during-snapshot-load ()
  "`--hydrate-and-reorder-on-open' still hydrates display state during a
snapshot load even though it skips the reorder, so badges render while
order preservation is honored."
  (agent-repl-test--with-clean-state
    (let ((loaded nil))
      (cl-letf (((symbol-function 'agent-repl--load-display-state)
                 (lambda (&rest _) (setq loaded t)))
                ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                 (lambda (_ws) nil)))
        (let ((agent-repl--snapshot-load-state '(:queue nil)))
          (agent-repl--hydrate-and-reorder-on-open "ws-a" "/tmp/ws-a"))
        (should loaded)))))

;;;; ---- Tests: clean-frame-foreign-windows ----

(defun agent-repl-test--make-owned-buffer (name ws)
  "Create buffer NAME with `agent-repl--owning-workspace' set to WS."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq-local agent-repl--owning-workspace ws))
    buf))

(ert-deftest agent-repl-cmd-test-clean-frame-foreign-windows/native-agent-buffer-kept ()
  "Window showing a buffer owned by WS is kept (native-agent case)."
  (agent-repl-test--with-clean-state
    (let ((native (agent-repl-test--make-owned-buffer "*ws-native*" "ws-a"))
          (extra-win nil))
      (unwind-protect
          (progn
            (switch-to-buffer native)
            (setq extra-win (split-window))
            (set-window-buffer extra-win native)
            (agent-repl--clean-frame-foreign-windows "ws-a")
            (should (= 2 (length (window-list nil 'nomini))))
            (dolist (win (window-list nil 'nomini))
              (should (eq (window-buffer win) native))))
        (when (and extra-win (window-live-p extra-win))
          (ignore-errors (delete-window extra-win)))
        (when (buffer-live-p native) (kill-buffer native))))))

(ert-deftest agent-repl-cmd-test-clean-frame-foreign-windows/foreign-agent-buffer-detected ()
  "Window showing a buffer owned by a different workspace is scrubbed."
  (agent-repl-test--with-clean-state
    (let ((native  (agent-repl-test--make-owned-buffer "*ws-native*"  "ws-a"))
          (foreign (agent-repl-test--make-owned-buffer "*ws-foreign*" "ws-b"))
          (foreign-win nil))
      (unwind-protect
          (progn
            (switch-to-buffer native)
            (setq foreign-win (split-window))
            (set-window-buffer foreign-win foreign)
            ;; Mark as a agent-repl-style protected panel.
            (set-window-parameter foreign-win 'no-delete-other-windows t)
            (set-window-dedicated-p foreign-win t)
            (agent-repl--clean-frame-foreign-windows "ws-a")
            (let ((wins (window-list nil 'nomini)))
              (should (= 1 (length wins)))
              (should (eq (window-buffer (car wins)) native))))
        (when (and foreign-win (window-live-p foreign-win))
          (set-window-parameter foreign-win 'no-delete-other-windows nil)
          (set-window-dedicated-p foreign-win nil)
          (ignore-errors (delete-window foreign-win)))
        (when (buffer-live-p native)  (kill-buffer native))
        (when (buffer-live-p foreign) (kill-buffer foreign))))))

(ert-deftest agent-repl-cmd-test-clean-frame-foreign-windows/non-agent-buffer-kept ()
  "A non-agent buffer (no owning workspace) is treated as allowed."
  (agent-repl-test--with-clean-state
    (let ((regular (get-buffer-create "*regular-file*"))
          (extra-win nil))
      (unwind-protect
          (progn
            (with-current-buffer regular
              (should-not (buffer-local-value 'agent-repl--owning-workspace
                                              regular)))
            (switch-to-buffer regular)
            (setq extra-win (split-window))
            (set-window-buffer extra-win regular)
            (agent-repl--clean-frame-foreign-windows "ws-a")
            (should (= 2 (length (window-list nil 'nomini))))
            (dolist (win (window-list nil 'nomini))
              (should (eq (window-buffer win) regular))))
        (when (and extra-win (window-live-p extra-win))
          (ignore-errors (delete-window extra-win)))
        (when (buffer-live-p regular) (kill-buffer regular))))))

(ert-deftest agent-repl-cmd-test-clean-frame-foreign-windows/no-owning-workspace-buffer-kept ()
  "Mixed: native + no-owner buffer — only foreign is scrubbed; no-owner kept."
  (agent-repl-test--with-clean-state
    (let ((native  (agent-repl-test--make-owned-buffer "*ws-native*"  "ws-a"))
          (foreign (agent-repl-test--make-owned-buffer "*ws-foreign*" "ws-b"))
          (regular (get-buffer-create "*regular-file*"))
          (foreign-win nil)
          (regular-win nil))
      (unwind-protect
          (progn
            (switch-to-buffer native)
            (setq foreign-win (split-window))
            (set-window-buffer foreign-win foreign)
            (setq regular-win (split-window))
            (set-window-buffer regular-win regular)
            (agent-repl--clean-frame-foreign-windows "ws-a")
            (let* ((wins (window-list nil 'nomini))
                   (bufs (mapcar #'window-buffer wins)))
              (should (= 2 (length wins)))
              (should (memq native bufs))
              (should (memq regular bufs))
              (should-not (memq foreign bufs))))
        (dolist (w (list foreign-win regular-win))
          (when (and w (window-live-p w))
            (ignore-errors (delete-window w))))
        (when (buffer-live-p native)  (kill-buffer native))
        (when (buffer-live-p foreign) (kill-buffer foreign))
        (when (buffer-live-p regular) (kill-buffer regular))))))

(ert-deftest agent-repl-cmd-test-clean-frame-foreign-windows/swaps-when-all-foreign ()
  "When every window is foreign-agent, helper collapses to fallback."
  (agent-repl-test--with-clean-state
    (let ((foreign1 (agent-repl-test--make-owned-buffer "*ws-foreign-1*" "ws-b"))
          (foreign2 (agent-repl-test--make-owned-buffer "*ws-foreign-2*" "ws-c"))
          (fallback (get-buffer-create " *test-fallback*"))
          (extra-win nil))
      (unwind-protect
          (cl-letf (((symbol-function 'doom-fallback-buffer)
                     (lambda () fallback)))
            (switch-to-buffer foreign1)
            (set-window-parameter (selected-window) 'no-delete-other-windows t)
            (set-window-dedicated-p (selected-window) t)
            (setq extra-win (split-window))
            (set-window-buffer extra-win foreign2)
            (set-window-parameter extra-win 'no-delete-other-windows t)
            (set-window-dedicated-p extra-win t)
            (agent-repl--clean-frame-foreign-windows "ws-fresh")
            (let ((wins (window-list nil 'nomini)))
              (should (= 1 (length wins)))
              (should (eq (window-buffer (car wins)) fallback))
              ;; Surviving window must be writable for the next setup step.
              (should-not (window-dedicated-p (car wins)))))
        ;; Cleanup any leftover windows that survived a failed assertion.
        (dolist (w (window-list nil 'nomini))
          (set-window-parameter w 'no-delete-other-windows nil)
          (set-window-dedicated-p w nil))
        (when (and extra-win (window-live-p extra-win))
          (ignore-errors (delete-window extra-win)))
        (when (buffer-live-p foreign1) (kill-buffer foreign1))
        (when (buffer-live-p foreign2) (kill-buffer foreign2))))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/errors-when-file-missing ()
  "load-workspace-snapshot signals user-error when the snapshot file is absent."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-workspace-snapshot-file "/nonexistent/agent-snap.el"))
      (should-error (agent-repl-load-workspace-snapshot) :type 'user-error))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/establishes-each-entry ()
  "load-workspace-snapshot delegates to `agent-repl--establish-workspace'
once per existing entry, passing the snapshot's `ws' name."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t))
          (established nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a)
                                            ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (agent-repl-load-workspace-snapshot)
              (should (= 2 (length established)))
              (should (member "ws-a" established))
              (should (member "ws-b" established))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/tracks-restored-workspaces ()
  "load-workspace-snapshot records each successfully established entry on
`agent-repl--restored-workspaces' so a later
`agent-repl-nuke-restored-workspaces' can target only the restore batch.
Workspaces that go through the actually-establish branch (NOT the
already-ready short-circuit) must be tagged."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t)))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a)
                                            ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace) #'ignore)
                      ;; Force the establishing branch (not already-ready).
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ;; Stub the watchdog so loader doesn't actually wait.
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              (should (member "ws-a" agent-repl--restored-workspaces))
              ;; ws-b is awaited; advance it manually via the fully-loaded hook
              ;; to confirm the establishing-branch path tags it too.
              (run-hook-with-args 'agent-repl-ws-fully-loaded-functions "ws-a" nil)
              (run-hook-with-args 'agent-repl-ws-fully-loaded-functions "ws-b" nil)
              (should (member "ws-b" agent-repl--restored-workspaces))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/already-ready-not-tracked ()
  "A snapshot entry that hits the `already-ready' short-circuit must NOT be
tagged as restored.  Such workspaces were already alive before the loader
ran (the origin ws the user was sitting in, or any other ws the agent was
already up in before the 2s idle loader fired).  Tagging them would make
`nuke-restored-workspaces' incorrectly sweep the user's pre-existing
workspace."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-origin (make-temp-file "agent-proj-origin-" t))
          (dir-new (make-temp-file "agent-proj-new-" t))
          (ready-table (make-hash-table :test 'equal)))
      (puthash "ws-origin" t ready-table)
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-origin" . ,dir-origin)
                                            ("ws-new" . ,dir-new)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace) #'ignore)
                      ;; ws-origin hits the already-ready short-circuit;
                      ;; ws-new goes through the establishing branch.
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (ws) (gethash ws ready-table)))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              ;; Origin (already-ready) NOT tagged.
              (should-not (member "ws-origin" agent-repl--restored-workspaces))
              ;; Drive the establishing-branch entry to completion.
              (run-hook-with-args 'agent-repl-ws-fully-loaded-functions "ws-new" nil)
              ;; ws-new (actually established) IS tagged.
              (should (member "ws-new" agent-repl--restored-workspaces))))
        (delete-file snapshot-file)
        (delete-directory dir-origin t)
        (delete-directory dir-new t)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/skipped-not-tracked ()
  "A snapshot entry whose project-dir is gone is NOT added to the restored list.
Only entries the loader actually established (`:loaded') are tracked —
skipped entries (`:skipped' branch) must not pollute the set, otherwise
`nuke-restored-workspaces' would try to tear down ghosts."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (real-dir (make-temp-file "agent-proj-real-" t)))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-real" . ,real-dir)
                                            ("ws-gone" . "/nonexistent/path")))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace) #'ignore)
                      ;; Force the establishing branch (not already-ready).
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              ;; Advance ws-real via the fully-loaded hook.
              (run-hook-with-args 'agent-repl-ws-fully-loaded-functions "ws-real" nil)
              (should (member "ws-real" agent-repl--restored-workspaces))
              (should-not (member "ws-gone" agent-repl--restored-workspaces))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/accumulates-across-loads ()
  "Successive snapshot loads union (not replace) `agent-repl--restored-workspaces'.
Loading from-archive after a normal load must not drop the first batch's
restored names — both batches are restore-origin and the nuke-restored
path needs to see both."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t)))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace) #'ignore)
                      ;; Force the establishing branch (not already-ready)
                      ;; so the loader actually tags both workspaces.
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl--write-sexp-file snapshot-file `(("ws-a" . ,dir-a)))
              (agent-repl-load-workspace-snapshot)
              (run-hook-with-args 'agent-repl-ws-fully-loaded-functions "ws-a" nil)
              (agent-repl--write-sexp-file snapshot-file `(("ws-b" . ,dir-b)))
              (agent-repl-load-workspace-snapshot)
              (run-hook-with-args 'agent-repl-ws-fully-loaded-functions "ws-b" nil)
              (should (member "ws-a" agent-repl--restored-workspaces))
              (should (member "ws-b" agent-repl--restored-workspaces))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/skips-missing-dirs ()
  "load-workspace-snapshot does not establish entries whose directory is gone."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (real-dir (make-temp-file "agent-proj-real-" t))
          (established nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-real" . ,real-dir)
                                            ("ws-gone" . "/nonexistent/path")))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (agent-repl-load-workspace-snapshot)
              (should (equal established (list "ws-real")))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/does-not-flash ()
  "load-workspace-snapshot does NOT pulse tabs during bulk restore.
A flash storm would be noise; the loader bypasses the inherent-flash
jump path on purpose."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t))
          (flash-calls 0))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a)
                                            ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace) #'ignore)
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'agent-repl-flash-tab)
                       (lambda (&rest _) (cl-incf flash-calls)))
                      ((symbol-function 'agent-repl--flash-current-tab)
                       (lambda () (cl-incf flash-calls))))
              (agent-repl-load-workspace-snapshot)
              (should (zerop flash-calls))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

;;;; ---- Tests: snapshot startup/quit wrappers ----

(ert-deftest agent-repl-cmd-test-load-snapshot-on-startup/no-op-when-file-absent ()
  "Startup wrapper returns quietly when the snapshot file does not exist."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-workspace-snapshot-file "/nonexistent/agent-snap.el")
          (called nil))
      (cl-letf (((symbol-function 'agent-repl-load-workspace-snapshot)
                 (lambda () (setq called t))))
        (agent-repl--load-workspace-snapshot-on-startup)
        (should-not called)))))

(ert-deftest agent-repl-cmd-test-load-snapshot-on-startup/invokes-load-when-file-present ()
  "Startup wrapper calls the real loader when the snapshot file exists."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (called nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (cl-letf (((symbol-function 'agent-repl-load-workspace-snapshot)
                       (lambda () (setq called t))))
              (agent-repl--load-workspace-snapshot-on-startup)
              (should called)))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-load-snapshot-on-startup/swallows-errors ()
  "Startup wrapper must not propagate errors from the loader."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (cl-letf (((symbol-function 'agent-repl-load-workspace-snapshot)
                       (lambda () (error "boom"))))
              (agent-repl--load-workspace-snapshot-on-startup)
              (should t)))
        (delete-file snapshot-file)))))

;;;; ---- Tests: workspace snapshot path resolver ----

(ert-deftest agent-repl-cmd-test-workspace-snapshot-file-for-read-prefers-configured ()
  "workspace-snapshot-file-for-read returns the configured path when it exists."
  (let ((snapshot-file (make-temp-file "agent-snap-cur-")))
    (unwind-protect
        (let ((agent-repl-workspace-snapshot-file snapshot-file))
          (should (equal (agent-repl--workspace-snapshot-file-for-read)
                         snapshot-file)))
      (delete-file snapshot-file))))

(ert-deftest agent-repl-cmd-test-workspace-snapshot-file-for-read-falls-back-to-legacy ()
  "workspace-snapshot-file-for-read falls back to the legacy module-dir
path when the configured file is absent but the legacy file exists."
  (let* ((legacy (make-temp-file "agent-snap-legacy-"))
         (configured "/nonexistent/agent-snap.el"))
    (unwind-protect
        (let ((agent-repl-workspace-snapshot-file configured)
              (agent-repl--legacy-workspace-snapshot-file legacy))
          (should (equal (agent-repl--workspace-snapshot-file-for-read) legacy)))
      (delete-file legacy))))

(ert-deftest agent-repl-cmd-test-workspace-snapshot-file-for-read-defaults-to-configured ()
  "When neither the configured nor the legacy file exists, the resolver
returns the configured path so callers get a reasonable default
(e.g. for `unless (file-exists-p ...)' guards on startup)."
  (let ((agent-repl-workspace-snapshot-file "/nonexistent/configured.el")
        (agent-repl--legacy-workspace-snapshot-file "/nonexistent/legacy.el"))
    (should (equal (agent-repl--workspace-snapshot-file-for-read)
                   "/nonexistent/configured.el"))))

;;;; ---- Tests: workspace snapshot archival ----

(ert-deftest agent-repl-cmd-test-snapshot-archive/first-save-archives-prior ()
  "First save in this Emacs run copies the existing snapshot file into
the archive dir before overwriting (so the previous session's roster
is preserved)."
  (agent-repl-test--with-clean-state
    ;; Seed an existing snapshot on disk to represent the prior session.
    (let ((dir (file-name-directory agent-repl-workspace-snapshot-file)))
      (when (and dir (not (file-directory-p dir))) (make-directory dir t)))
    (with-temp-file agent-repl-workspace-snapshot-file
      (insert "((\"prior-ws\" :project-dir \"/tmp/prior\" :priority nil))"))
    (agent-repl--ws-put "new-ws" :project-dir "/tmp/new")
    (agent-repl-save-workspace-snapshot)
    (let* ((archive-dir (agent-repl--workspace-snapshot-archive-dir))
           (archives (and (file-directory-p archive-dir)
                          (directory-files archive-dir nil "\\.el\\'"))))
      (should (= 1 (length archives))))))

(ert-deftest agent-repl-cmd-test-snapshot-archive/subsequent-saves-skip ()
  "Subsequent saves in the same Emacs run do NOT create additional
archive files — the archive ran already this run."
  (agent-repl-test--with-clean-state
    (let ((dir (file-name-directory agent-repl-workspace-snapshot-file)))
      (when (and dir (not (file-directory-p dir))) (make-directory dir t)))
    (with-temp-file agent-repl-workspace-snapshot-file
      (insert "((\"prior\" :project-dir \"/tmp/prior\" :priority nil))"))
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (agent-repl-save-workspace-snapshot)   ; first save: archives prior
    (agent-repl-save-workspace-snapshot)   ; second save: must NOT archive again
    (agent-repl-save-workspace-snapshot)   ; third save: still no new archive
    (let* ((archive-dir (agent-repl--workspace-snapshot-archive-dir))
           (archives (and (file-directory-p archive-dir)
                          (directory-files archive-dir nil "\\.el\\'"))))
      (should (= 1 (length archives))))))

(ert-deftest agent-repl-cmd-test-snapshot-archive/no-prior-file-is-noop ()
  "When the snapshot file does not exist, the first save is just a
write — no archive is created (nothing to preserve)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (agent-repl-save-workspace-snapshot)
    (let ((archive-dir (agent-repl--workspace-snapshot-archive-dir)))
      (should-not (file-directory-p archive-dir)))))

(ert-deftest agent-repl-cmd-test-snapshot-archive/disabled-when-max-zero ()
  "Setting `agent-repl-workspace-snapshot-archive-max' to 0 disables
archival entirely — even when a prior file exists."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-workspace-snapshot-archive-max 0)
          (dir (file-name-directory agent-repl-workspace-snapshot-file)))
      (when (and dir (not (file-directory-p dir))) (make-directory dir t))
      (with-temp-file agent-repl-workspace-snapshot-file
        (insert "((\"prior\" :project-dir \"/tmp/prior\" :priority nil))"))
      (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
      (agent-repl-save-workspace-snapshot)
      (let ((archive-dir (agent-repl--workspace-snapshot-archive-dir)))
        (should-not (file-directory-p archive-dir))))))

(ert-deftest agent-repl-cmd-test-snapshot-archive/prunes-to-cap ()
  "Archive count is capped at `agent-repl-workspace-snapshot-archive-max'.
Older entries (lexicographically earliest filenames) are pruned."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-workspace-snapshot-archive-max 2)
          (archive-dir (agent-repl--workspace-snapshot-archive-dir)))
      (make-directory archive-dir t)
      ;; Pre-seed three "old" archives.
      (dolist (name '("20200101T000000.el"
                      "20200102T000000.el"
                      "20200103T000000.el"))
        (with-temp-file (expand-file-name name archive-dir) (insert "()")))
      ;; Seed the live snapshot so the next save triggers the once-per-run
      ;; archival, which copies that file in and then prunes.
      (let ((dir (file-name-directory agent-repl-workspace-snapshot-file)))
        (when (and dir (not (file-directory-p dir))) (make-directory dir t)))
      (with-temp-file agent-repl-workspace-snapshot-file
        (insert "((\"prior\" :project-dir \"/tmp/p\" :priority nil))"))
      (agent-repl-save-workspace-snapshot)
      ;; After pruning, only the two newest entries remain.  The new
      ;; archive (named after live file's mtime) is one of them; the
      ;; oldest pre-seeded entry is gone.
      (let ((remaining (directory-files archive-dir nil "\\.el\\'")))
        (should (= 2 (length remaining)))
        (should-not (member "20200101T000000.el" remaining))))))

;;;; ---- Tests: snapshot entry normalizer ----

(ert-deftest agent-repl-cmd-test-snapshot-entry-normalize/legacy-shape ()
  "Legacy `(NAME . DIR-STRING)' entries become `(NAME :project-dir DIR)'."
  (let ((n (agent-repl--snapshot-entry-normalize '("ws" . "/tmp/proj"))))
    (should (equal (car n) "ws"))
    (should (equal (plist-get (cdr n) :project-dir) "/tmp/proj"))
    (should (null (plist-get (cdr n) :priority)))))

(ert-deftest agent-repl-cmd-test-snapshot-entry-normalize/plist-shape ()
  "Plist entries are passed through unchanged (priority retained for
back-compat reads of older snapshot files even though new saves omit it)."
  (let ((n (agent-repl--snapshot-entry-normalize
            '("ws" :project-dir "/tmp/proj" :priority "p2"))))
    (should (equal (car n) "ws"))
    (should (equal (plist-get (cdr n) :project-dir) "/tmp/proj"))
    (should (equal (plist-get (cdr n) :priority) "p2"))))

;;;; ---- Tests: save-workspace-snapshot (plist format) ----

(ert-deftest agent-repl-cmd-test-save-workspace-snapshot/omits-priority ()
  "Save deliberately omits :priority from saved entries — the per-project
state file (`<root>/.claude/emacs/state.el') is the authoritative source."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
            (agent-repl--ws-put "ws-a" :priority "p1")
            (agent-repl-save-workspace-snapshot)
            (let* ((data (plist-get (agent-repl--read-workspace-snapshot snapshot-file)
                                    :workspaces))
                   (entry (assoc "ws-a" data)))
              (should entry)
              (should (equal (plist-get (cdr entry) :project-dir) "/tmp/a"))
              (should-not (plist-member (cdr entry) :priority))))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-save-workspace-snapshot/one-entry-per-line ()
  "Save writes each workspace entry on its own line for human-readable diffs.
The current format wraps entries in a `:workspaces' key inside a top-level
plist that also carries `:merge-queue'; the workspace list portion still
puts one entry per line so per-workspace diffs stay tight."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
            (agent-repl--ws-put "ws2" :project-dir "/tmp/ws2")
            (agent-repl--ws-put "ws3" :project-dir "/tmp/ws3")
            (agent-repl-save-workspace-snapshot)
            (let* ((raw (with-temp-buffer
                          (insert-file-contents snapshot-file)
                          (buffer-string)))
                   (ws-line-count
                    (cl-count-if (lambda (line)
                                   (string-match-p "(\"ws[0-9]+\"" line))
                                 (split-string raw "\n"))))
              ;; Each of the three workspace entries appears on its own
              ;; line inside the :workspaces sub-list (order is hash-key
              ;; dependent and intentionally not asserted here).
              (should (= 3 ws-line-count))
              ;; Round-trip cleanly through the new reader.
              (let ((parsed (agent-repl--read-workspace-snapshot snapshot-file)))
                (should (= 3 (length (plist-get parsed :workspaces)))))))
        (delete-file snapshot-file)))))

;;;; ---- Tests: read-workspace-snapshot (format normalizer) ----

(ert-deftest agent-repl-cmd-test-read-workspace-snapshot/legacy-list-format ()
  "Legacy `((NAME :project-dir DIR) ...)' files normalize to a plist with
the entries under :workspaces and a nil :merge-queue."
  (let ((file (make-temp-file "agent-snap-")))
    (unwind-protect
        (progn
          (agent-repl--write-sexp-file
           file '(("ws-a" :project-dir "/tmp/a")
                  ("ws-b" :project-dir "/tmp/b")))
          (let ((parsed (agent-repl--read-workspace-snapshot file)))
            (should (= 2 (length (plist-get parsed :workspaces))))
            (should (null (plist-get parsed :merge-queue)))))
      (delete-file file))))

(ert-deftest agent-repl-cmd-test-read-workspace-snapshot/plist-format ()
  "New plist files round-trip through the reader with both keys intact."
  (let ((file (make-temp-file "agent-snap-")))
    (unwind-protect
        (progn
          (agent-repl--write-sexp-file
           file '(:workspaces (("ws-a" :project-dir "/tmp/a"))
                  :merge-queue ((:source-ws "ws-a" :silent t :auto-resolve nil))))
          (let ((parsed (agent-repl--read-workspace-snapshot file)))
            (should (equal (plist-get parsed :workspaces)
                           '(("ws-a" :project-dir "/tmp/a"))))
            (should (equal (plist-get parsed :merge-queue)
                           '((:source-ws "ws-a" :silent t :auto-resolve nil))))))
      (delete-file file))))

(ert-deftest agent-repl-cmd-test-read-workspace-snapshot/missing-file-returns-nil ()
  "Reader returns nil for a non-existent file (no error)."
  (should-not (agent-repl--read-workspace-snapshot "/nonexistent/snap.el")))

;;;; ---- Tests: write-workspace-snapshot (merge-queue persistence) ----

(ert-deftest agent-repl-cmd-test-write-workspace-snapshot/persists-merge-queue ()
  "Writer round-trips `agent-repl--merge-queue' into the snapshot file
so a later read restores the FIFO."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file)
                (agent-repl--merge-queue
                 '((:source-ws "ws-a" :silent t :auto-resolve nil)
                   (:source-ws "ws-b" :silent nil :auto-resolve t))))
            (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
            (agent-repl--ws-put "ws-b" :project-dir "/tmp/b")
            (agent-repl-save-workspace-snapshot)
            (let* ((parsed (agent-repl--read-workspace-snapshot snapshot-file))
                   (mq (plist-get parsed :merge-queue)))
              (should (= 2 (length mq)))
              (should (equal (plist-get (nth 0 mq) :source-ws) "ws-a"))
              (should (eq    (plist-get (nth 0 mq) :silent) t))
              (should (eq    (plist-get (nth 0 mq) :auto-resolve) nil))
              (should (equal (plist-get (nth 1 mq) :source-ws) "ws-b"))
              (should (eq    (plist-get (nth 1 mq) :silent) nil))
              (should (eq    (plist-get (nth 1 mq) :auto-resolve) t))))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-write-workspace-snapshot/persists-merge-queue-target-dir ()
  "Writer round-trips each entry's `:target-dir' so the per-target bucket
partitioning survives a restart."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file)
                (agent-repl--merge-queue
                 '((:source-ws "ws-a" :silent t :auto-resolve t
                    :target-dir "/tmp/target-a"))))
            (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
            (agent-repl-save-workspace-snapshot)
            (let* ((parsed (agent-repl--read-workspace-snapshot snapshot-file))
                   (mq (plist-get parsed :merge-queue)))
              (should (equal (plist-get (nth 0 mq) :target-dir) "/tmp/target-a"))))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-write-workspace-snapshot/empty-merge-queue ()
  "An empty live queue writes an empty :merge-queue list — not omitted."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file)
                (agent-repl--merge-queue nil))
            (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
            (agent-repl-save-workspace-snapshot)
            (let ((parsed (agent-repl--read-workspace-snapshot snapshot-file)))
              ;; :merge-queue is present and explicitly an empty list.
              (should (plist-member parsed :merge-queue))
              (should (null (plist-get parsed :merge-queue)))))
        (delete-file snapshot-file)))))

;;;; ---- Tests: snapshot-restore-merge-queue ----

(ert-deftest agent-repl-cmd-test-snapshot-restore-merge-queue/repopulates-live-queue ()
  "Restore copies the saved entries into `agent-repl--merge-queue' in order."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--merge-queue nil))
      (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
      (agent-repl--ws-put "ws-b" :project-dir "/tmp/b")
      (agent-repl--snapshot-restore-merge-queue
       '((:source-ws "ws-a" :silent t :auto-resolve nil)
         (:source-ws "ws-b" :silent nil :auto-resolve t)))
      (should (= 2 (length agent-repl--merge-queue)))
      (should (equal (plist-get (nth 0 agent-repl--merge-queue) :source-ws) "ws-a"))
      (should (equal (plist-get (nth 1 agent-repl--merge-queue) :source-ws) "ws-b")))))

(ert-deftest agent-repl-cmd-test-snapshot-restore-merge-queue/preserves-target-dir ()
  "Restore carries each entry's `:target-dir' so the per-target sub-queue
partitioning survives the restart."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--merge-queue nil))
      (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
      (agent-repl--snapshot-restore-merge-queue
       '((:source-ws "ws-a" :silent t :auto-resolve t :target-dir "/tmp/target-a")))
      (should (equal (plist-get (car agent-repl--merge-queue) :target-dir)
                     "/tmp/target-a")))))

(ert-deftest agent-repl-cmd-test-snapshot-restore-merge-queue/remarks-queued-state ()
  "Restore re-applies `:repl-state :merge-queued' on each surviving ws so
the drawer's MERGING bucket shows them again post-restart."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--merge-queue nil))
      (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
      (agent-repl--snapshot-restore-merge-queue
       '((:source-ws "ws-a" :silent t :auto-resolve t)))
      (should (eq :merge-queued (agent-repl--ws-get "ws-a" :repl-state))))))

(ert-deftest agent-repl-cmd-test-snapshot-restore-merge-queue/drops-vanished-ws ()
  "Entries whose `:source-ws' no longer exists in `agent-repl--workspaces'
are dropped (a workspace was removed between sessions)."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--merge-queue nil))
      (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
      ;; ws-gone is not in the workspaces hash.
      (agent-repl--snapshot-restore-merge-queue
       '((:source-ws "ws-a" :silent t :auto-resolve t)
         (:source-ws "ws-gone" :silent t :auto-resolve t)))
      (should (= 1 (length agent-repl--merge-queue)))
      (should (equal (plist-get (car agent-repl--merge-queue) :source-ws) "ws-a")))))

(ert-deftest agent-repl-cmd-test-snapshot-restore-merge-queue/empty-input-noop ()
  "Restore with nil input leaves the live queue untouched."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--merge-queue
           '((:source-ws "preexisting" :silent nil :auto-resolve nil))))
      (agent-repl--snapshot-restore-merge-queue nil)
      ;; Existing queue is unchanged.
      (should (= 1 (length agent-repl--merge-queue))))))

;;;; ---- Tests: in-flight-merges persistence (round-trip + restore) ----

(ert-deftest agent-repl-cmd-test-read-workspace-snapshot/in-flight-merges-round-trip ()
  "Plist files with `:in-flight-merges' round-trip through the reader."
  (let ((file (make-temp-file "agent-snap-")))
    (unwind-protect
        (progn
          (agent-repl--write-sexp-file
           file '(:workspaces (("ws-a" :project-dir "/tmp/a"))
                  :merge-queue nil
                  :in-flight-merges ((:source-ws "ws-a" :target-dir "/tmp/a" :started-at 12345.0))))
          (let ((parsed (agent-repl--read-workspace-snapshot file)))
            (should (equal (plist-get parsed :in-flight-merges)
                           '((:source-ws "ws-a" :target-dir "/tmp/a" :started-at 12345.0))))))
      (delete-file file))))

(ert-deftest agent-repl-cmd-test-read-workspace-snapshot/in-flight-merges-absent-returns-nil ()
  "Files predating `:in-flight-merges' (or without that key) yield nil."
  (let ((file (make-temp-file "agent-snap-")))
    (unwind-protect
        (progn
          (agent-repl--write-sexp-file
           file '(:workspaces (("ws-a" :project-dir "/tmp/a"))
                  :merge-queue nil))
          (let ((parsed (agent-repl--read-workspace-snapshot file)))
            (should (null (plist-get parsed :in-flight-merges)))))
      (delete-file file))))

(ert-deftest agent-repl-cmd-test-write-workspace-snapshot/persists-in-flight-merges ()
  "Writer captures the live `agent-repl--in-flight-merges' alongside the roster."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file)
                (agent-repl--merge-queue nil)
                (agent-repl--in-flight-merges
                 '((:source-ws "ws-a" :target-dir "/tmp/a" :started-at 1.0)
                   (:source-ws "ws-b" :target-dir "/tmp/b" :started-at 2.0))))
            (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
            (agent-repl-save-workspace-snapshot)
            (let* ((parsed (agent-repl--read-workspace-snapshot snapshot-file))
                   (ifm (plist-get parsed :in-flight-merges)))
              (should (= 2 (length ifm)))
              (should (equal (plist-get (nth 0 ifm) :source-ws) "ws-a"))
              (should (equal (plist-get (nth 0 ifm) :target-dir) "/tmp/a"))
              (should (equal (plist-get (nth 1 ifm) :source-ws) "ws-b"))))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-write-workspace-snapshot/empty-in-flight-merges ()
  "An empty live in-flight list writes `:in-flight-merges' as an empty
list — present, not omitted."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-")))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file)
                (agent-repl--merge-queue nil)
                (agent-repl--in-flight-merges nil))
            (agent-repl--ws-put "ws-a" :project-dir "/tmp/a")
            (agent-repl-save-workspace-snapshot)
            (let ((parsed (agent-repl--read-workspace-snapshot snapshot-file)))
              (should (plist-member parsed :in-flight-merges))
              (should (null (plist-get parsed :in-flight-merges)))))
        (delete-file snapshot-file)))))

(ert-deftest agent-repl-cmd-test-snapshot-restore-in-flight-merges/repopulates-live-list ()
  "Restore copies saved entries into `agent-repl--in-flight-merges' as plain plists."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--in-flight-merges nil))
      (agent-repl--snapshot-restore-in-flight-merges
       '((:source-ws "ws-a" :target-dir "/tmp/a" :started-at 1.0)
         (:source-ws "ws-b" :target-dir "/tmp/b" :started-at 2.0)))
      (should (= 2 (length agent-repl--in-flight-merges)))
      (should (equal (plist-get (nth 0 agent-repl--in-flight-merges) :source-ws) "ws-a"))
      (should (equal (plist-get (nth 1 agent-repl--in-flight-merges) :target-dir) "/tmp/b")))))

(ert-deftest agent-repl-cmd-test-snapshot-restore-in-flight-merges/empty-input-noop ()
  "Restore with nil input leaves the live list untouched."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--in-flight-merges
           '((:source-ws "preexisting" :target-dir "/tmp/p" :started-at 1.0))))
      (agent-repl--snapshot-restore-in-flight-merges nil)
      (should (= 1 (length agent-repl--in-flight-merges))))))

;;;; ---- Tests: load-workspace-snapshot (merge-queue restoration) ----

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/restores-merge-queue ()
  "Loader populates `agent-repl--merge-queue' from the snapshot file's
:merge-queue at the end of the load (in `--snapshot-load-finish')."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (real-dir (make-temp-file "agent-proj-" t))
          (agent-repl--merge-queue nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file
             snapshot-file
             `(:workspaces (("ws-a" :project-dir ,real-dir))
               :merge-queue ((:source-ws "ws-a" :silent t :auto-resolve nil))))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws dir)
                         (agent-repl--ws-put ws :project-dir dir)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (agent-repl-load-workspace-snapshot)
              (should (= 1 (length agent-repl--merge-queue)))
              (should (equal (plist-get (car agent-repl--merge-queue) :source-ws)
                             "ws-a"))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

;;;; ---- Tests: drain-merge-queue interactive command ----

(ert-deftest agent-repl-cmd-test-drain-merge-queue/empty-queue-messages ()
  "With an empty queue the command emits a `message' and does not call
the internal drain."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--merge-queue nil)
          (drain-called nil))
      (cl-letf (((symbol-function 'agent-repl--drain-merge-queue)
                 (lambda () (setq drain-called t))))
        (agent-repl-drain-merge-queue)
        (should-not drain-called)))))

(ert-deftest agent-repl-cmd-test-drain-merge-queue/cherry-pick-active-does-not-block ()
  "The drain is per-target now, so a live cherry-pick in one target no
longer makes the command refuse — it still calls the internal drain,
which skips busy buckets and dispatches the free ones."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--merge-queue
           '((:source-ws "ws-a" :silent t :auto-resolve t :target-dir "/tmp/target")))
          (drain-called nil))
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--drain-merge-queue)
                 (lambda () (setq drain-called t))))
        ;; Must NOT signal, and must still reach the drain.
        (agent-repl-drain-merge-queue)
        (should drain-called)))))

(ert-deftest agent-repl-cmd-test-drain-merge-queue/dispatches-when-safe ()
  "With a non-empty queue the command calls `agent-repl--drain-merge-queue'
to dispatch the next eligible entry."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--merge-queue
           '((:source-ws "ws-a" :silent t :auto-resolve t :target-dir "/tmp/target")))
          (drain-called nil))
      (cl-letf (((symbol-function 'agent-repl--path-canonical) #'identity)
                ((symbol-function 'agent-repl--drain-merge-queue)
                 (lambda () (setq drain-called t))))
        (agent-repl-drain-merge-queue)
        (should drain-called)))))

;;;; ---- Tests: load-workspace-snapshot (back-compat + hydration + pending) ----

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/reads-legacy-format ()
  "Loader still accepts the legacy `(NAME . DIR-STRING)' shape."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (real-dir (make-temp-file "agent-proj-" t))
          (established nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file `(("ws-legacy" . ,real-dir)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws dir)
                         (push (cons ws dir) established)
                         (agent-repl--ws-put ws :project-dir dir)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (agent-repl-load-workspace-snapshot)
              (should (member (cons "ws-legacy" real-dir) established))
              (should (equal (agent-repl--ws-get "ws-legacy" :project-dir) real-dir))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/ignores-legacy-priority ()
  "Loader does NOT pass `:priority' to establish — priority is now
sourced from each project's state file, not the snapshot roster."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (real-dir (make-temp-file "agent-proj-" t))
          (call-arity nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file
             snapshot-file
             `(("ws-pri" :project-dir ,real-dir :priority "p1")))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (&rest args) (setq call-arity (length args))))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (agent-repl-load-workspace-snapshot)
              (should (= call-arity 2))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest agent-repl-cmd-test-load-workspace-snapshot/returns-to-origin-workspace ()
  "Loader switches back to the workspace that was active when it began."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (real-dir (make-temp-file "agent-proj-" t))
          (returned-to nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file
             snapshot-file `(("ws-a" :project-dir ,real-dir)))
            (cl-letf (((symbol-function '+workspace-current-name)
                       (lambda () "origin-ws"))
                      ((symbol-function '+workspace-exists-p) (lambda (_n) t))
                      ((symbol-function 'persp-frame-switch)
                       (lambda (name) (setq returned-to name)))
                      ((symbol-function 'agent-repl--establish-workspace) #'ignore)
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t)))
              (agent-repl-load-workspace-snapshot)
              (should (equal returned-to "origin-ws"))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/starts-claude-when-not-running ()
  "establish-workspace starts the agent for the workspace unless it's already running."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-" t)))
           (started-for nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--initialize-agent)
                     (lambda (ws &rest _) (setq started-for ws)))
                    ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore))
            (agent-repl--establish-workspace "test-ws" tmp-dir)
            (should (equal started-for "test-ws")))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/skips-claude-when-running ()
  "establish-workspace skips agent-init when the agent is already running for ws."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-" t)))
           (started nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--initialize-agent)
                     (lambda (&rest _) (setq started t)))
                    ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) t))
                    ((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore))
            (agent-repl--establish-workspace "test-ws" tmp-dir)
            (should-not started))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/calls-switch-project-function ()
  "establish-workspace invokes `+workspaces-switch-project-function' (magit lambda)."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-" t)))
           (called-with nil))
      (unwind-protect
          (let ((+workspaces-switch-project-function
                 (lambda (d) (setq called-with d))))
            (cl-letf (((symbol-function 'agent-repl--initialize-agent) #'ignore)
                      ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) nil))
                      ((symbol-function 'persp-add-new) #'ignore)
                      ((symbol-function 'persp-frame-switch) #'ignore)
                      ((symbol-function 'projectile-add-known-project) #'ignore))
              (agent-repl--establish-workspace "test-ws" tmp-dir)
              (should (equal (file-name-as-directory called-with) tmp-dir))))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/restores-fallback-default-directory ()
  "establish-workspace must not permanently mutate the shared fallback
buffer's `default-directory'.  The buffer is visible from every persp,
so a permanent mutation makes scratch report the last-loaded ws's project
root from every persp (a cross-persp bleed)."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-" t)))
           (fb (get-buffer-create " *test-fb-default-dir*"))
           (sentinel-dir "/sentinel-original-dir/"))
      (unwind-protect
          (cl-letf (((symbol-function 'doom-fallback-buffer) (lambda () fb))
                    ((symbol-function 'agent-repl--initialize-agent) #'ignore)
                    ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore))
            (with-current-buffer fb
              (setq default-directory sentinel-dir))
            (agent-repl--establish-workspace "test-ws" tmp-dir)
            (should (equal (buffer-local-value 'default-directory fb)
                           sentinel-dir)))
        (when (buffer-live-p fb) (kill-buffer fb))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/fallback-default-dir-visible-during-hook ()
  "While `+workspaces-switch-project-function' runs, the fallback buffer's
`default-directory' must reflect the project root (some Doom hooks
depend on it).  After the hook returns it's restored."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-" t)))
           (fb (get-buffer-create " *test-fb-default-dir-hook*"))
           (sentinel-dir "/sentinel-original-dir/")
           (observed-dir nil))
      (unwind-protect
          (let ((+workspaces-switch-project-function
                 (lambda (_dir)
                   (setq observed-dir
                         (buffer-local-value 'default-directory fb)))))
            (cl-letf (((symbol-function 'doom-fallback-buffer) (lambda () fb))
                      ((symbol-function 'agent-repl--initialize-agent) #'ignore)
                      ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) nil))
                      ((symbol-function 'persp-add-new) #'ignore)
                      ((symbol-function 'persp-frame-switch) #'ignore)
                      ((symbol-function 'projectile-add-known-project) #'ignore))
              (with-current-buffer fb
                (setq default-directory sentinel-dir))
              (agent-repl--establish-workspace "test-ws" tmp-dir)
              (should (equal observed-dir tmp-dir))
              (should (equal (buffer-local-value 'default-directory fb)
                             sentinel-dir))))
        (when (buffer-live-p fb) (kill-buffer fb))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/opens-recent-file ()
  "establish-workspace opens the most-recent file via `find-file' when one exists."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-" t)))
           (tmp-file (expand-file-name "hello.el" tmp-dir))
           (opened nil))
      (unwind-protect
          (progn
            (with-temp-file tmp-file (insert ";; placeholder"))
            (cl-letf (((symbol-function 'agent-repl--most-recent-project-file)
                       (lambda (_d) tmp-file))
                      ((symbol-function 'find-file)
                       (lambda (f) (setq opened f)))
                      ((symbol-function 'agent-repl--initialize-agent) #'ignore)
                      ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) nil))
                      ((symbol-function 'persp-add-new) #'ignore)
                      ((symbol-function 'persp-frame-switch) #'ignore)
                      ((symbol-function 'projectile-add-known-project) #'ignore))
              (agent-repl--establish-workspace "test-ws" tmp-dir)
              (should (equal opened tmp-file))))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/sets-workspace-project-param ()
  "establish-workspace must set the `+workspace-project' persp-parameter
to DIR on the newly added persp.  Without this, Doom's
`+workspaces-switch-to-project-h' (invoked by `SPC p p') cannot match
the existing workspace against the project root and falls through to
its uniquify-by-parent-dir branch, producing names like
`doom-worktrees/<ws>'.  Mirrors what Doom's own hook sets on line 588
of ui/workspaces/autoload/workspaces.el."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-wp-" t)))
           (fake-persp (vector 'fake-persp))
           (set-with-key nil)
           (set-with-val nil)
           (set-with-persp nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--initialize-agent) #'ignore)
                    ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'persp-add-new) (lambda (&rest _) fake-persp))
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore)
                    ((symbol-function 'set-persp-parameter)
                     (lambda (k v p)
                       (setq set-with-key k set-with-val v set-with-persp p))))
            (agent-repl--establish-workspace "test-ws" tmp-dir)
            (should (eq set-with-key '+workspace-project))
            (should (equal set-with-val tmp-dir))
            (should (eq set-with-persp fake-persp)))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/skips-workspace-project-param-when-add-new-returns-nil ()
  "establish-workspace must not call `set-persp-parameter' when
`persp-add-new' returns nil (test stubs commonly stub it to `#'ignore').
A nil persp would crash `set-persp-parameter'."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-wp-nil-" t)))
           (set-called nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--initialize-agent) #'ignore)
                    ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore)
                    ((symbol-function 'set-persp-parameter)
                     (lambda (&rest _) (setq set-called t))))
            (agent-repl--establish-workspace "test-ws" tmp-dir)
            (should-not set-called))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/skips-recent-when-gone ()
  "establish-workspace skips `find-file' when the recent file doesn't exist."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-" t)))
           (find-file-called nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--most-recent-project-file)
                     (lambda (_d) "/nonexistent/gone.el"))
                    ((symbol-function 'find-file)
                     (lambda (&rest _) (setq find-file-called t)))
                    ((symbol-function 'agent-repl--initialize-agent) #'ignore)
                    ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore))
            (agent-repl--establish-workspace "test-ws" tmp-dir)
            (should-not find-file-called))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/loads-display-state-from-state ()
  "establish-workspace calls `--load-display-state' with its ws name and dir
so the badge restores from the per-project state file rather than the roster."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-pri-" t)))
           (loaded-ws nil)
           (loaded-with nil))
      (unwind-protect
          (cl-letf (((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore)
                    ((symbol-function 'agent-repl--initialize-agent) #'ignore)
                    ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'agent-repl--load-display-state)
                     (lambda (ws d) (setq loaded-ws ws loaded-with d))))
            (agent-repl--establish-workspace "test-ws" tmp-dir)
            (should (equal loaded-ws "test-ws"))
            (should (equal (file-name-as-directory loaded-with) tmp-dir)))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/reorders-by-priority ()
  "establish-workspace calls `--reorder-workspace-by-priority' AFTER display-state
hydration so restored workspaces appear in priority order, matching the
behavior of `agent-repl-set-priority'.  Without this, snapshot entries
sit in file order even when state.el carries priorities."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-reorder-" t)))
           (events nil))
      (unwind-protect
          (cl-letf (((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch) #'ignore)
                    ((symbol-function 'projectile-add-known-project) #'ignore)
                    ((symbol-function 'agent-repl--initialize-agent) #'ignore)
                    ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) nil))
                    ((symbol-function 'agent-repl--load-display-state)
                     (lambda (&rest _) (push 'load events)))
                    ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                     (lambda (_ws) (push 'reorder events))))
            (agent-repl--establish-workspace "test-ws" tmp-dir)
            (let ((ordered (reverse events)))
              (should (memq 'load ordered))
              (should (memq 'reorder ordered))
              ;; Reorder must come after the load so it reads a real priority.
              (should (< (cl-position 'load ordered)
                         (cl-position 'reorder ordered)))))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-establish-workspace/activates-persp ()
  "establish-workspace calls `persp-frame-switch' with the snapshot's ws name
so persp-mode begins capturing a window configuration for that persp."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-est-" t)))
           (switched-to nil))
      (unwind-protect
          (cl-letf (((symbol-function 'persp-add-new) #'ignore)
                    ((symbol-function 'persp-frame-switch)
                     (lambda (n) (setq switched-to n)))
                    ((symbol-function 'projectile-add-known-project) #'ignore)
                    ((symbol-function 'agent-repl--initialize-agent) #'ignore)
                    ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) nil)))
            (agent-repl--establish-workspace "DC/CV-494738/worker-suite" tmp-dir)
            (should (equal switched-to "DC/CV-494738/worker-suite")))
        (delete-directory tmp-dir t)))))

;;;; ---- Tests: snapshot-load queue driver (after-ready hook) ----

(ert-deftest agent-repl-cmd-test-snapshot-load/refuses-concurrent-invocation ()
  "Calling load while a load is in progress signals user-error."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--snapshot-load-state (list :queue nil)))
      (should-error (agent-repl-load-workspace-snapshot) :type 'user-error))))

(ert-deftest agent-repl-cmd-test-snapshot-load/advances-on-ready-event ()
  "A ws-fully-loaded callback for the awaited ws advances the queue."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t))
          (established nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ;; Don't short-circuit on already-ready — force the
                      ;; hook-driven path.
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              ;; First entry established; loader is awaiting ws-a.
              (should (equal established '("ws-a")))
              (should (equal "ws-a"
                             (plist-get agent-repl--snapshot-load-state :awaiting)))
              ;; Simulate ws-fully-loaded signal for ws-a (no marker = happy path).
              (run-hook-with-args 'agent-repl-ws-fully-loaded-functions "ws-a" nil)
              (should (equal (sort (copy-sequence established) #'string<)
                             '("ws-a" "ws-b")))
              ;; Simulate ws-fully-loaded for ws-b → load finishes.
              (run-hook-with-args 'agent-repl-ws-fully-loaded-functions "ws-b" nil)
              (should-not agent-repl--snapshot-load-state)))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/ignores-foreign-ready ()
  "Ready signal for a workspace we're not awaiting does NOT advance the queue."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t))
          (established nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              ;; Foreign ws-fully-loaded — must NOT advance.
              (run-hook-with-args 'agent-repl-ws-fully-loaded-functions "some-other-ws" nil)
              (should (equal established '("ws-a")))
              (should (equal "ws-a"
                             (plist-get agent-repl--snapshot-load-state :awaiting)))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/already-ready-short-circuits ()
  "When `--snapshot-load-ws-ready-p' is t after establish, queue advances
without waiting for a hook fire."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t))
          (established nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              (should (equal (sort established #'string<) '("ws-a" "ws-b")))
              (should-not agent-repl--snapshot-load-state)))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/skip-missing-dir-does-not-wait ()
  "Missing-dir entries are skipped and the queue advances synchronously."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (real-dir (make-temp-file "agent-proj-" t))
          (established nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file
             snapshot-file
             `(("ws-gone" . "/nonexistent/path")
               ("ws-real" . ,real-dir)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              ;; ws-gone skipped; ws-real established (no wait).
              (should (equal established '("ws-real")))
              (should-not agent-repl--snapshot-load-state)))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/timeout-advances-queue ()
  "Per-entry watchdog firing advances past a wedged workspace."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t))
          (established nil)
          captured-timer-callback)
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir) (push ws established)))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (_secs _rep fn &rest args)
                         (setq captured-timer-callback (cons fn args))
                         'fake-timer))
                      ((symbol-function 'timerp) (lambda (_t) nil))
                      ((symbol-function 'cancel-timer) #'ignore))
              (agent-repl-load-workspace-snapshot)
              (should (equal established '("ws-a")))
              ;; Fire the timeout for ws-a manually.
              (apply (car captured-timer-callback) (cdr captured-timer-callback))
              (should (equal (sort (copy-sequence established) #'string<)
                             '("ws-a" "ws-b")))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/timeout-fires-fully-loaded-with-marker ()
  "Watchdog timeout for ws fires `ws-fully-loaded-functions' with the
`:timed-out' marker so observers can distinguish forced advance from
the happy-path advance."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t))
          (fired-with nil)
          captured-timer-callback)
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace) #'ignore)
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (_secs _rep fn &rest args)
                         (setq captured-timer-callback (cons fn args))
                         'fake-timer))
                      ((symbol-function 'timerp) (lambda (_t) nil))
                      ((symbol-function 'cancel-timer) #'ignore))
              (let ((agent-repl-ws-fully-loaded-functions
                     (list (lambda (ws marker)
                             (push (cons ws marker) fired-with))
                           ;; Plus the loader's own subscriber stays attached.
                           #'agent-repl--snapshot-load-on-loaded)))
                (agent-repl-load-workspace-snapshot)
                ;; Fire the timeout for ws-a manually.
                (apply (car captured-timer-callback) (cdr captured-timer-callback))
                ;; Hook fired for ws-a with :timed-out marker.
                (should (cl-some (lambda (e)
                                   (and (equal (car e) "ws-a")
                                        (eq (cdr e) :timed-out)))
                                 fired-with)))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/hook-detached-on-finish ()
  "After a successful load, `agent-repl--snapshot-load-on-loaded' is removed
from `agent-repl-ws-fully-loaded-functions'."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (real-dir (make-temp-file "agent-proj-" t)))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file `(("ws-a" . ,real-dir)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace) #'ignore)
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              (should-not (memq #'agent-repl--snapshot-load-on-loaded
                                agent-repl-ws-fully-loaded-functions))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/establish-error-advances ()
  "If `--establish-workspace' errors, the loader logs and advances anyway."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t))
          (attempts nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir)
                         (push ws attempts)
                         (when (equal ws "ws-a")
                           (error "boom"))))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              ;; Both entries attempted despite ws-a's error.
              (should (equal (sort (copy-sequence attempts) #'string<)
                             '("ws-a" "ws-b")))
              (should-not agent-repl--snapshot-load-state)))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/establish-error-bumps-load-error-not-loaded ()
  "An establish failure increments `:load-error', NOT `:loaded'."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t))
          finish-state)
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir)
                         (when (equal ws "ws-a") (error "boom"))))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl--snapshot-load-finish)
                       (lambda ()
                         (setq finish-state (copy-sequence agent-repl--snapshot-load-state))
                         (setq agent-repl--snapshot-load-state nil))))
              (agent-repl-load-workspace-snapshot)
              (should (= 1 (plist-get finish-state :loaded)))
              (should (= 1 (plist-get finish-state :load-error)))
              (should (= 0 (plist-get finish-state :skipped)))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/establish-error-skips-watchdog ()
  "An establish failure must NOT arm the per-entry watchdog timer —
no ws is alive to wait on, advance immediately."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t))
          (timer-calls 0))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir)
                         (when (equal ws "ws-a") (error "boom"))))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) (cl-incf timer-calls) 'fake-timer)))
              (agent-repl-load-workspace-snapshot)
              ;; Watchdog should NOT have been armed for ws-a (failure path)
              ;; and ws-b takes the already-ready short-circuit, so 0 timers.
              (should (= 0 timer-calls))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/state-omits-pre-load-window-snapshot ()
  "Loader state must not carry a captured window-configuration or
window-state for origin: persp-mode's own switch-away save (triggered
by the first `--establish-workspace's `persp-frame-switch') handles
origin's layout, so any extra capture is dead weight that risks
resurrecting foreign or dead buffers on restore."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (real-dir (make-temp-file "agent-proj-" t))
          (state-during-establish nil))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file `(("ws-a" . ,real-dir)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (&rest _)
                         (setq state-during-establish
                               (copy-sequence agent-repl--snapshot-load-state))))
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) t))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              (should-not (plist-member state-during-establish :origin-window-config))
              (should-not (plist-member state-during-establish :origin-window-state))))
        (delete-file snapshot-file)
        (delete-directory real-dir t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/top-level-error-finishes ()
  "An error inside `--snapshot-load-step' (outside establish-workspace) is
routed to `--snapshot-load-finish' so the hook detaches and state clears
instead of leaving a zombie loader."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t)))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file `(("ws-a" . ,dir-a)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace) #'ignore)
                      ;; Force a signal from the ready check — covers the path
                      ;; where neither establish-workspace nor finish itself
                      ;; raised, but a helper between them did.
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) (error "ready-check boom")))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              (should-not agent-repl--snapshot-load-state)
              (should-not (memq #'agent-repl--snapshot-load-on-loaded
                                agent-repl-ws-fully-loaded-functions))
              (should agent-repl--snapshot-loaded-p)))
        (delete-file snapshot-file)
        (delete-directory dir-a t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load/awaiting-nil-during-establish ()
  "Bug 6: `:awaiting' must remain nil for the duration of
`--establish-workspace' so a re-entrant ready event (today impossible,
latent if establish ever yields) is a no-op rather than advancing the
queue mid-call.  Asserts the contract by firing
`--snapshot-load-on-loaded' synchronously from inside the mocked
establish and verifying (a) `:awaiting' is nil at the re-entry point,
(b) the queue did NOT advance to the next ws during establish, and
(c) `:awaiting' is set to ws-a only after establish returned."
  (agent-repl-test--with-clean-state
    (let ((snapshot-file (make-temp-file "agent-snap-"))
          (dir-a (make-temp-file "agent-proj-a-" t))
          (dir-b (make-temp-file "agent-proj-b-" t))
          (established nil)
          (reentry-awaiting 'unset)
          (reentry-established 'unset))
      (unwind-protect
          (let ((agent-repl-workspace-snapshot-file snapshot-file))
            (agent-repl--write-sexp-file snapshot-file
                                          `(("ws-a" . ,dir-a) ("ws-b" . ,dir-b)))
            (cl-letf (((symbol-function 'agent-repl--establish-workspace)
                       (lambda (ws _dir)
                         (push ws established)
                         (when (equal ws "ws-a")
                           ;; Capture state at the moment of re-entry, then
                           ;; synchronously fire the loaded hook for ws-a
                           ;; from inside establish.  The handler must see
                           ;; `:awaiting' nil and short-circuit.
                           (setq reentry-awaiting
                                 (plist-get agent-repl--snapshot-load-state :awaiting))
                           (agent-repl--snapshot-load-on-loaded "ws-a")
                           ;; If the re-entry had advanced the queue, ws-b
                           ;; would have been pushed onto `established' here.
                           (setq reentry-established (copy-sequence established)))))
                      ;; Force the hook-driven path (not already-ready).
                      ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                       (lambda (_ws) nil))
                      ((symbol-function 'run-with-timer)
                       (lambda (&rest _) nil)))
              (agent-repl-load-workspace-snapshot)
              ;; (a) During re-entry, `:awaiting' was nil — the handler check
              ;; `(equal ws :awaiting)' failed, so the callback was a no-op.
              (should (null reentry-awaiting))
              ;; (b) The queue did NOT advance to ws-b while establish for
              ;; ws-a was still on the stack.
              (should (equal reentry-established '("ws-a")))
              ;; (c) After establish returned and the watchdog branch ran,
              ;; `:awaiting' is now ws-a — the loader is properly parked.
              (should (equal "ws-a"
                             (plist-get agent-repl--snapshot-load-state :awaiting)))
              ;; Sanity: ws-b was never established.
              (should (equal established '("ws-a")))))
        (delete-file snapshot-file)
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest agent-repl-cmd-test-snapshot-load-finish/idempotent ()
  "Calling `--snapshot-load-finish' twice is harmless: the second call
sees nil state and short-circuits without printing bogus counters."
  (agent-repl-test--with-clean-state
    (setq agent-repl--snapshot-load-state
          (list :queue nil :origin nil :awaiting nil
                :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
    (agent-repl--snapshot-load-finish)
    (should-not agent-repl--snapshot-load-state)
    ;; Second call must not error and must not re-set --snapshot-loaded-p
    ;; from a synthetic state (no state, no message).
    (agent-repl--snapshot-load-finish)
    (should-not agent-repl--snapshot-load-state)))

;;;; ---- agent-repl--snapshot-load-close-main ----

(ert-deftest agent-repl-cmd-test-snapshot-load-finish/closes-main-when-exists ()
  "After finish, the leftover `main' workspace artifact is nuked: the
persp is killed via `+workspace/kill' when it still exists."
  (agent-repl-test--with-clean-state
    (let ((killed nil))
      (setq agent-repl--snapshot-load-state
            (list :queue nil :origin nil :awaiting nil
                  :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
      (cl-letf (((symbol-function '+workspace-exists-p)
                 (lambda (name) (equal name "main")))
                ((symbol-function 'agent-repl--kill-workspace-buffers)
                 (lambda (_ws) nil))
                ((symbol-function '+workspace/kill)
                 (lambda (name) (setq killed name)))
                (+workspaces-main "main"))
        (agent-repl--snapshot-load-finish)
        (should (equal killed "main"))))))

(ert-deftest agent-repl-cmd-test-snapshot-load-finish/nuke-main-sweeps-persp-buffers ()
  "Nuking `main' invokes `agent-repl--kill-workspace-buffers' on the
persp so dashboard/scratch/file buffers don't survive the kill, AND
the sweep happens before the persp itself is killed (since
`+workspace/kill' would otherwise drop the persp's buffer list)."
  (agent-repl-test--with-clean-state
    (let ((call-order nil))
      (setq agent-repl--snapshot-load-state
            (list :queue nil :origin nil :awaiting nil
                  :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
      (cl-letf (((symbol-function '+workspace-exists-p)
                 (lambda (name) (equal name "main")))
                ((symbol-function 'agent-repl--kill-workspace-buffers)
                 (lambda (ws) (push (cons 'sweep ws) call-order)))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (push (cons 'persp-kill ws) call-order)))
                (+workspaces-main "main"))
        (agent-repl--snapshot-load-finish)
        (should (equal (nreverse call-order)
                       '((sweep . "main") (persp-kill . "main"))))))))

(ert-deftest agent-repl-cmd-test-snapshot-load-finish/main-missing-is-noop ()
  "Finish is a no-op for the nuke-main step when `main' doesn't exist:
neither the buffer sweep nor the persp kill fires."
  (agent-repl-test--with-clean-state
    (let ((sweep-calls 0)
          (kill-calls 0))
      (setq agent-repl--snapshot-load-state
            (list :queue nil :origin nil :awaiting nil
                  :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
      (cl-letf (((symbol-function '+workspace-exists-p) (lambda (_n) nil))
                ((symbol-function 'agent-repl--kill-workspace-buffers)
                 (lambda (_ws) (cl-incf sweep-calls)))
                ((symbol-function '+workspace/kill)
                 (lambda (_n) (cl-incf kill-calls)))
                (+workspaces-main "main"))
        (agent-repl--snapshot-load-finish)
        (should (= 0 sweep-calls))
        (should (= 0 kill-calls))))))

(ert-deftest agent-repl-cmd-test-snapshot-load-finish/close-main-error-swallowed ()
  "An error from `+workspace/kill' on main is logged but never propagated."
  (agent-repl-test--with-clean-state
    (setq agent-repl--snapshot-load-state
          (list :queue nil :origin nil :awaiting nil
                :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
    (cl-letf (((symbol-function '+workspace-exists-p)
               (lambda (name) (equal name "main")))
              ((symbol-function 'agent-repl--kill-workspace-buffers)
               (lambda (_ws) nil))
              ((symbol-function '+workspace/kill)
               (lambda (_n) (error "boom")))
              (+workspaces-main "main"))
      ;; Must not signal.
      (agent-repl--snapshot-load-finish)
      (should-not agent-repl--snapshot-load-state))))

(ert-deftest agent-repl-cmd-test-snapshot-load-finish/nuke-main-sweep-error-does-not-block-persp-kill ()
  "An error from the buffer sweep on main is swallowed AND does not block
the subsequent `+workspace/kill' — each step has its own condition-case
so a failing sweep can't strand the persp in the tabline."
  (agent-repl-test--with-clean-state
    (let ((killed nil))
      (setq agent-repl--snapshot-load-state
            (list :queue nil :origin nil :awaiting nil
                  :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
      (cl-letf (((symbol-function '+workspace-exists-p)
                 (lambda (name) (equal name "main")))
                ((symbol-function 'agent-repl--kill-workspace-buffers)
                 (lambda (_ws) (error "sweep boom")))
                ((symbol-function '+workspace/kill)
                 (lambda (ws) (setq killed ws)))
                (+workspaces-main "main"))
        ;; Must not signal.
        (agent-repl--snapshot-load-finish)
        (should (equal killed "main"))))))

(ert-deftest agent-repl-cmd-test-snapshot-load-finish/idempotent-skips-second-close-main ()
  "A second `--snapshot-load-finish' call (state already nil) must not
re-invoke the nuke-main step — close-main is part of the per-load
finalization, not a standalone teardown."
  (agent-repl-test--with-clean-state
    (let ((kill-calls 0))
      (setq agent-repl--snapshot-load-state
            (list :queue nil :origin nil :awaiting nil
                  :loaded 0 :skipped 0 :total 0 :timeout-timer nil))
      (cl-letf (((symbol-function '+workspace-exists-p)
                 (lambda (name) (equal name "main")))
                ((symbol-function 'agent-repl--kill-workspace-buffers)
                 (lambda (_ws) nil))
                ((symbol-function '+workspace/kill)
                 (lambda (_n) (cl-incf kill-calls)))
                (+workspaces-main "main"))
        (agent-repl--snapshot-load-finish)
        (should (= 1 kill-calls))
        ;; Second call: state is nil, the early `when' short-circuits before
        ;; the close-main call, so no extra kill fires.
        (agent-repl--snapshot-load-finish)
        (should (= 1 kill-calls))))))

;;;; ---- agent-repl-switch-to-project ----

;; Helper: mock run-at-time to execute the deferred thunk immediately so
;; switch-to-project tests can assert on find-file / load-display-state
;; effects without firing a real idle timer.
(defmacro agent-repl-test--with-sync-run-at-time (&rest body)
  "Execute BODY with `run-at-time' replaced by an immediate-call shim.
The shim invokes (funcall FN) for every (run-at-time TIME REPEAT FN)
call, making deferred closures synchronous in tests."
  `(cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat fn &rest _args) (funcall fn))))
     ,@body))

(ert-deftest agent-repl-cmd-test-switch-to-project/switches-then-hydrates ()
  "switch-to-project switches via projectile, then hydrates priority."
  (agent-repl-test--with-clean-state
    (let ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-switch-" t)))
          switched-with)
      (unwind-protect
          (progn
            (agent-repl-test--seed-file
             (agent-repl--state-file tmp-dir)
             (prin1-to-string '(:priority "p2")))
            (agent-repl-test--with-sync-run-at-time
              (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                         (lambda (project) (setq switched-with project)))
                        ((symbol-function 'agent-repl--most-recent-project-file)
                         (lambda (_d) nil))
                        ((symbol-function '+workspace-current-name)
                         (lambda () "switched-ws"))
                        ((symbol-function 'force-mode-line-update)
                         (lambda (&optional _all) nil))
                        ((symbol-function 'agent-repl-flash-tab)
                         (lambda (&rest _) nil)))
                (agent-repl-switch-to-project tmp-dir)
                (should (equal switched-with tmp-dir))
                (should (equal (agent-repl--ws-get "switched-ws" :priority) "p2")))))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-switch-to-project/flashes-activated-ws ()
  "switch-to-project pulses the activated workspace tab via flash-tab."
  (agent-repl-test--with-clean-state
    (let ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-switch-" t)))
          flashed-ws)
      (unwind-protect
          (agent-repl-test--with-sync-run-at-time
            (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                       (lambda (_p) nil))
                      ((symbol-function 'agent-repl--most-recent-project-file)
                       (lambda (_d) nil))
                      ((symbol-function '+workspace-current-name)
                       (lambda () "switched-ws"))
                      ((symbol-function 'force-mode-line-update)
                       (lambda (&optional _all) nil))
                      ((symbol-function 'agent-repl-flash-tab)
                       (lambda (ws &rest _) (setq flashed-ws ws))))
              (agent-repl-switch-to-project tmp-dir)
              (should (equal flashed-ws "switched-ws"))))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-switch-to-project/opens-most-recent-file ()
  "switch-to-project opens the most-recent project file when it exists."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-switch-" t)))
           (tmp-file (expand-file-name "hello.el" tmp-dir))
           (opened nil))
      (unwind-protect
          (progn
            (with-temp-file tmp-file (insert ";; placeholder"))
            (agent-repl-test--with-sync-run-at-time
              (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                         (lambda (_p) nil))
                        ((symbol-function 'agent-repl--most-recent-project-file)
                         (lambda (_d) tmp-file))
                        ((symbol-function 'find-file)
                         (lambda (f) (setq opened f)))
                        ((symbol-function '+workspace-current-name)
                         (lambda () "switched-ws"))
                        ((symbol-function 'force-mode-line-update)
                         (lambda (&optional _all) nil))
                        ((symbol-function 'agent-repl-flash-tab)
                         (lambda (&rest _) nil)))
                (agent-repl-switch-to-project tmp-dir)
                (should (equal opened tmp-file)))))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-switch-to-project/skips-most-recent-when-gone ()
  "switch-to-project skips find-file when the most-recent path doesn't exist."
  (agent-repl-test--with-clean-state
    (let ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-switch-" t)))
          (find-file-called nil))
      (unwind-protect
          (agent-repl-test--with-sync-run-at-time
            (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                       (lambda (_p) nil))
                      ((symbol-function 'agent-repl--most-recent-project-file)
                       (lambda (_d) "/nonexistent/gone.el"))
                      ((symbol-function 'find-file)
                       (lambda (&rest _) (setq find-file-called t)))
                      ((symbol-function '+workspace-current-name)
                       (lambda () "switched-ws"))
                      ((symbol-function 'force-mode-line-update)
                       (lambda (&optional _all) nil))
                      ((symbol-function 'agent-repl-flash-tab)
                       (lambda (&rest _) nil)))
              (agent-repl-switch-to-project tmp-dir)
              (should-not find-file-called)))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-switch-to-project/defers-find-file ()
  "switch-to-project defers find-file via run-at-time, not synchronously."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-switch-" t)))
           (tmp-file (expand-file-name "hello.el" tmp-dir))
           (opened nil)
           (timer-fired nil))
      (unwind-protect
          (progn
            (with-temp-file tmp-file (insert ";; placeholder"))
            (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                       (lambda (_p) nil))
                      ((symbol-function 'agent-repl--most-recent-project-file)
                       (lambda (_d) tmp-file))
                      ((symbol-function 'find-file)
                       (lambda (f) (setq opened f)))
                      ((symbol-function '+workspace-current-name)
                       (lambda () "switched-ws"))
                      ((symbol-function 'force-mode-line-update)
                       (lambda (&optional _all) nil))
                      ((symbol-function 'agent-repl-flash-tab)
                       (lambda (&rest _) nil))
                      ;; Capture the timer but do NOT fire it
                      ((symbol-function 'run-at-time)
                       (lambda (_time _repeat _fn &rest _args)
                         (setq timer-fired t))))
              (agent-repl-switch-to-project tmp-dir)
              ;; Timer was scheduled but find-file not yet called
              (should timer-fired)
              (should-not opened)))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-switch-to-project/reseats-by-priority ()
  "switch-to-project reseats the activated workspace by priority on `SPC p p',
via the shared `--hydrate-and-reorder-on-open' step, so an opened
workspace lands in priority order like the snapshot/worktree restore path."
  (agent-repl-test--with-clean-state
    (let ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-switch-" t)))
          reorder-calls)
      (unwind-protect
          (agent-repl-test--with-sync-run-at-time
            (cl-letf (((symbol-function 'projectile-switch-project-by-name)
                       (lambda (_p) nil))
                      ((symbol-function 'agent-repl--most-recent-project-file)
                       (lambda (_d) nil))
                      ((symbol-function 'agent-repl--load-display-state)
                       (lambda (&rest _) nil))
                      ((symbol-function 'agent-repl--reorder-workspace-by-priority)
                       (lambda (ws) (push ws reorder-calls)))
                      ((symbol-function '+workspace-current-name)
                       (lambda () "switched-ws"))
                      ((symbol-function 'force-mode-line-update)
                       (lambda (&optional _all) nil))
                      ((symbol-function 'agent-repl-flash-tab)
                       (lambda (&rest _) nil)))
              (let ((agent-repl--snapshot-load-state nil))
                (agent-repl-switch-to-project tmp-dir))
              (should (equal reorder-calls '("switched-ws")))))
        (delete-directory tmp-dir t)))))

;;;; ---- agent-repl--most-recent-project-file ----

(ert-deftest agent-repl-cmd-test-most-recent-project-file/returns-first-under-root ()
  "Returns the first `recentf-list' entry that lives under PROJECT-ROOT."
  (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-recent-" t)))
         (in1  (expand-file-name "a.el" tmp-dir))
         (in2  (expand-file-name "b.el" tmp-dir))
         (out  (expand-file-name "elsewhere.el" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file in1 (insert ""))
          (with-temp-file in2 (insert ""))
          (with-temp-file out (insert ""))
          (let ((recentf-list (list out in1 in2)))
            (should (equal (agent-repl--most-recent-project-file tmp-dir) in1))))
      (delete-directory tmp-dir t)
      (when (file-exists-p out) (delete-file out)))))

(ert-deftest agent-repl-cmd-test-most-recent-project-file/nil-when-none-match ()
  "Returns nil when no `recentf-list' entry lives under PROJECT-ROOT."
  (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-recent-" t)))
         (out     (expand-file-name "elsewhere.el" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file out (insert ""))
          (let ((recentf-list (list out)))
            (should-not (agent-repl--most-recent-project-file tmp-dir))))
      (delete-directory tmp-dir t)
      (when (file-exists-p out) (delete-file out)))))

(ert-deftest agent-repl-cmd-test-most-recent-project-file/boundary-safe ()
  "Does not mis-match `/p/foo' entries against project root `/p/foo-bar'."
  (let* ((parent  (file-name-as-directory (make-temp-file "agent-repl-recent-" t)))
         (foo     (file-name-as-directory (expand-file-name "foo" parent)))
         (foo-bar (file-name-as-directory (expand-file-name "foo-bar" parent)))
         (sibling (expand-file-name "x.el" foo)))
    (unwind-protect
        (progn
          (make-directory foo)
          (make-directory foo-bar)
          (with-temp-file sibling (insert ""))
          (let ((recentf-list (list sibling)))
            (should-not (agent-repl--most-recent-project-file foo-bar))))
      (delete-directory parent t))))

;;;; ---- agent-repl--most-recent-project-file: plist cache ----

(ert-deftest agent-repl-cmd-test-most-recent-project-file/prefers-plist-cache ()
  "Returns the :last-file plist entry when present and the file exists."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-plist-" t)))
           (cached-file (expand-file-name "cached.el" tmp-dir))
           (recentf-file (expand-file-name "recentf.el" tmp-dir)))
      (unwind-protect
          (progn
            (with-temp-file cached-file (insert ""))
            (with-temp-file recentf-file (insert ""))
            (agent-repl--ws-put "ws1" :project-dir tmp-dir)
            (agent-repl--ws-put "ws1" :last-file cached-file)
            (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
                       (lambda (_) "ws1")))
              (let ((recentf-list (list recentf-file)))
                (should (equal (agent-repl--most-recent-project-file tmp-dir)
                               cached-file)))))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-most-recent-project-file/falls-back-when-cache-gone ()
  "Falls back to recentf when :last-file cached path no longer exists."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-plist-" t)))
           (recentf-file (expand-file-name "recentf.el" tmp-dir)))
      (unwind-protect
          (progn
            (with-temp-file recentf-file (insert ""))
            (agent-repl--ws-put "ws1" :project-dir tmp-dir)
            (agent-repl--ws-put "ws1" :last-file "/nonexistent/gone.el")
            (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
                       (lambda (_) "ws1")))
              (let ((recentf-list (list recentf-file)))
                (should (equal (agent-repl--most-recent-project-file tmp-dir)
                               recentf-file)))))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-most-recent-project-file/falls-back-when-no-workspace ()
  "Falls back to recentf when no live workspace matches the project root."
  (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-plist-" t)))
         (recentf-file (expand-file-name "x.el" tmp-dir)))
    (unwind-protect
        (progn
          (with-temp-file recentf-file (insert ""))
          (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
                     (lambda (_) nil)))
            (let ((recentf-list (list recentf-file)))
              (should (equal (agent-repl--most-recent-project-file tmp-dir)
                             recentf-file)))))
      (delete-directory tmp-dir t))))

;;;; ---- agent-repl--record-last-file-visit ----

(ert-deftest agent-repl-cmd-test-record-last-file-visit/records-in-project ()
  "Records buffer-file-name as :last-file when file is inside the workspace project."
  (agent-repl-test--with-clean-state
    (let* ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-lfv-" t)))
           (project-file (expand-file-name "foo.el" tmp-dir)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir tmp-dir)
            (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                       (lambda () "ws1"))
                      ((symbol-function 'agent-repl--ws-dir)
                       (lambda (_ws) tmp-dir)))
              (let ((buffer-file-name project-file))
                (agent-repl--record-last-file-visit)
                (should (equal (agent-repl--ws-get "ws1" :last-file)
                               project-file)))))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-record-last-file-visit/ignores-outside-project ()
  "Does not write :last-file when the visited file is outside the workspace project."
  (agent-repl-test--with-clean-state
    (let ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-lfv-" t))))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir tmp-dir)
            (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                       (lambda () "ws1"))
                      ((symbol-function 'agent-repl--ws-dir)
                       (lambda (_ws) tmp-dir)))
              (let ((buffer-file-name "/some/other/place/file.el"))
                (agent-repl--record-last-file-visit)
                (should-not (agent-repl--ws-get "ws1" :last-file)))))
        (delete-directory tmp-dir t)))))

(ert-deftest agent-repl-cmd-test-record-last-file-visit/noop-with-no-ws ()
  "Does not error and writes nothing when there is no current registered workspace."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-current-name)
               (lambda () "nonexistent-ws")))
      (let ((buffer-file-name "/some/file.el"))
        ;; Should not error and should not create a hash entry
        (agent-repl--record-last-file-visit)
        (should-not (gethash "nonexistent-ws" agent-repl--workspaces))))))

;;;; ---- Tests: snapshot archive picker ----

(ert-deftest agent-repl-cmd-test-snapshot-file-ws-count/counts-entries ()
  "snapshot-file-ws-count returns the number of entries in the snapshot."
  (let ((f (make-temp-file "agent-snap-count-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert "((\"a\" :project-dir \"/tmp/a\") (\"b\" :project-dir \"/tmp/b\") (\"c\" :project-dir \"/tmp/c\"))"))
          (should (= 3 (agent-repl--snapshot-file-ws-count f))))
      (delete-file f))))

(ert-deftest agent-repl-cmd-test-snapshot-file-ws-count/zero-for-missing ()
  "snapshot-file-ws-count returns 0 for a missing file (graceful)."
  (should (= 0 (agent-repl--snapshot-file-ws-count "/nonexistent/snap.el"))))

(ert-deftest agent-repl-cmd-test-snapshot-candidate-label/contains-count-and-date ()
  "Candidate label embeds workspace count and a YYYY-MM-DD HH:MM mtime."
  (let ((f (make-temp-file "agent-snap-label-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert "((\"a\" :project-dir \"/tmp/a\") (\"b\" :project-dir \"/tmp/b\"))"))
          (let ((label (agent-repl--snapshot-candidate-label f)))
            (should (string-match-p (file-name-nondirectory f) label))
            (should (string-match-p "2ws" label))
            (should (string-match-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}"
                                    label))))
      (delete-file f))))

(ert-deftest agent-repl-cmd-test-snapshot-archive-candidates/current-and-archives ()
  "snapshot-archive-candidates returns the current file and every archived file."
  (let* ((dir (file-name-as-directory (make-temp-file "agent-snap-dir-" t)))
         (current (expand-file-name "workspaces.el" dir))
         (archive-dir (expand-file-name "workspaces-archive" dir))
         (archive-a (expand-file-name "20260510T184855.el" archive-dir))
         (archive-b (expand-file-name "20260505T094316.el" archive-dir)))
    (unwind-protect
        (let ((agent-repl-workspace-snapshot-file current))
          (make-directory archive-dir t)
          (with-temp-file current (insert "((\"cur\" :project-dir \"/tmp/c\"))"))
          (with-temp-file archive-a (insert "((\"a\" :project-dir \"/tmp/a\") (\"b\" :project-dir \"/tmp/b\"))"))
          (with-temp-file archive-b (insert "((\"x\" :project-dir \"/tmp/x\"))"))
          (let* ((candidates (agent-repl--snapshot-archive-candidates))
                 (paths (mapcar #'cdr candidates)))
            (should (= 3 (length candidates)))
            (should (member current paths))
            (should (member archive-a paths))
            (should (member archive-b paths))))
      (delete-directory dir t))))

(ert-deftest agent-repl-cmd-test-snapshot-archive-candidates/archives-newest-first ()
  "Archives are sorted newest-first (lexicographic on timestamped filename)."
  (let* ((dir (file-name-as-directory (make-temp-file "agent-snap-dir-" t)))
         (current (expand-file-name "workspaces.el" dir))
         (archive-dir (expand-file-name "workspaces-archive" dir))
         (older (expand-file-name "20260101T000000.el" archive-dir))
         (newer (expand-file-name "20260601T120000.el" archive-dir)))
    (unwind-protect
        (let ((agent-repl-workspace-snapshot-file current))
          (make-directory archive-dir t)
          (with-temp-file current (insert "()"))
          (with-temp-file older (insert "()"))
          (with-temp-file newer (insert "()"))
          (let* ((candidates (agent-repl--snapshot-archive-candidates))
                 (paths (mapcar #'cdr candidates)))
            ;; current first, then newer, then older
            (should (equal paths (list current newer older)))))
      (delete-directory dir t))))

(ert-deftest agent-repl-cmd-test-load-from-archive/loads-selected-file ()
  "load-from-archive invokes loader with the selected file's path."
  (let* ((dir (file-name-as-directory (make-temp-file "agent-snap-dir-" t)))
         (current (expand-file-name "workspaces.el" dir))
         (archive-dir (expand-file-name "workspaces-archive" dir))
         (chosen-archive (expand-file-name "20260510T184855.el" archive-dir))
         loaded-file)
    (unwind-protect
        (let ((agent-repl-workspace-snapshot-file current))
          (make-directory archive-dir t)
          (with-temp-file current (insert "()"))
          (with-temp-file chosen-archive (insert "()"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       ;; pick the archive entry (second, since current is first)
                       (cl-find-if (lambda (s) (string-match-p "20260510T184855" s))
                                   collection)))
                    ((symbol-function 'agent-repl-load-workspace-snapshot)
                     (lambda (file) (setq loaded-file file))))
            (agent-repl-load-workspace-snapshot-from-archive)
            (should (equal loaded-file chosen-archive))))
      (delete-directory dir t))))

(ert-deftest agent-repl-cmd-test-load-from-archive/errors-when-no-candidates ()
  "load-from-archive signals user-error when no snapshot files exist anywhere."
  (let ((agent-repl-workspace-snapshot-file "/nonexistent/snap.el")
        (agent-repl--legacy-workspace-snapshot-file "/nonexistent/legacy.el"))
    (cl-letf (((symbol-function 'agent-repl--workspace-snapshot-archive-dir)
               (lambda () "/nonexistent/archive/")))
      (should-error (agent-repl-load-workspace-snapshot-from-archive)
                    :type 'user-error))))

;;;; ---- Tests: snapshot startup-load scheduler ----

(ert-deftest agent-repl-cmd-test-schedule-snapshot-startup-load/schedules-idle-timer ()
  "schedule-snapshot-startup-load arms an idle timer with the configured delay."
  (let ((agent-repl-snapshot-startup-load-delay 1.5)
        captured-secs captured-repeat captured-fn)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (secs repeat fn &rest _)
                 (setq captured-secs secs captured-repeat repeat captured-fn fn))))
      (agent-repl--schedule-snapshot-startup-load)
      (should (= 1.5 captured-secs))
      (should-not captured-repeat)
      (should (eq captured-fn #'agent-repl--load-workspace-snapshot-on-startup)))))

(ert-deftest agent-repl-cmd-test-schedule-snapshot-startup-load/nil-delay-disables ()
  "Setting the delay to nil disables the startup load entirely."
  (let ((agent-repl-snapshot-startup-load-delay nil)
        called)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (setq called t))))
      (agent-repl--schedule-snapshot-startup-load)
      (should-not called))))

(ert-deftest agent-repl-cmd-test-schedule-snapshot-startup-load/installed-on-startup-hook ()
  "The scheduler is registered on `emacs-startup-hook' (module-load wires it)."
  (should (memq #'agent-repl--schedule-snapshot-startup-load
                emacs-startup-hook)))

;;;; ---- Tests: workspace snapshot save-guard (unloaded-clobber prevention) ----

(defmacro agent-repl-cmd-test--with-temp-snapshot-file (var &rest body)
  "Bind `agent-repl-workspace-snapshot-file' to a temp path and run BODY.
VAR receives the temp file path so BODY can inspect it.  Cleans up the
file and the archive directory the save path materialises beside it."
  (declare (indent 1))
  `(let* ((,var (make-temp-file "agent-repl-snap-guard-" nil ".el"))
          (agent-repl-workspace-snapshot-file ,var)
          (agent-repl--snapshot-loaded-p nil)
          (agent-repl--snapshot-archived-this-run nil))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var))
       (let ((archive (agent-repl--workspace-snapshot-archive-dir)))
         (when (file-directory-p archive) (delete-directory archive t))))))

(ert-deftest agent-repl-cmd-test-snapshot-save-safe-p/loaded-flag-passes ()
  "Once the loaded flag is set, save is always safe regardless of disk state."
  (agent-repl-cmd-test--with-temp-snapshot-file f
    (with-temp-file f
      (insert "((\"a\" :project-dir \"/tmp/a\") (\"b\" :project-dir \"/tmp/b\") (\"c\" :project-dir \"/tmp/c\"))"))
    (setq agent-repl--snapshot-loaded-p t)
    (should (agent-repl--snapshot-save-safe-p 1))))

(ert-deftest agent-repl-cmd-test-snapshot-save-safe-p/no-disk-file-passes ()
  "When no on-disk file exists, save is safe even if loader hasn't run."
  (agent-repl-cmd-test--with-temp-snapshot-file f
    (delete-file f)
    (should (agent-repl--snapshot-save-safe-p 1))))

(ert-deftest agent-repl-cmd-test-snapshot-save-safe-p/disk-smaller-passes ()
  "When loader hasn't run but on-disk roster is no larger than live, save is safe."
  (agent-repl-cmd-test--with-temp-snapshot-file f
    (with-temp-file f
      (insert "((\"a\" :project-dir \"/tmp/a\"))"))
    (should (agent-repl--snapshot-save-safe-p 1))
    (should (agent-repl--snapshot-save-safe-p 2))))

(ert-deftest agent-repl-cmd-test-snapshot-save-safe-p/unloaded-and-disk-larger-blocks ()
  "When loader hasn't run AND on-disk roster is larger than live, save is unsafe."
  (agent-repl-cmd-test--with-temp-snapshot-file f
    (with-temp-file f
      (insert "((\"a\" :project-dir \"/tmp/a\") (\"b\" :project-dir \"/tmp/b\") (\"c\" :project-dir \"/tmp/c\"))"))
    (should-not (agent-repl--snapshot-save-safe-p 1))
    (should-not (agent-repl--snapshot-save-safe-p 2))))

(ert-deftest agent-repl-cmd-test-snapshot-save/refuses-when-unloaded-and-shrinking ()
  "Save aborts (file unchanged) when loader hasn't run and write would shrink the roster."
  (agent-repl-test--with-clean-state
    (agent-repl-cmd-test--with-temp-snapshot-file f
      (let ((seed "((\"prior-a\" :project-dir \"/tmp/a\") (\"prior-b\" :project-dir \"/tmp/b\") (\"prior-c\" :project-dir \"/tmp/c\"))"))
        (with-temp-file f (insert seed))
        (agent-repl--ws-put "only-live" :project-dir "/tmp/live")
        (agent-repl-save-workspace-snapshot)
        (with-temp-buffer
          (insert-file-contents f)
          (should (equal (buffer-string) seed)))))))

(ert-deftest agent-repl-cmd-test-snapshot-save/proceeds-after-load ()
  "Save proceeds after the loader has run, even if live roster is smaller than disk."
  (agent-repl-test--with-clean-state
    (agent-repl-cmd-test--with-temp-snapshot-file f
      (with-temp-file f
        (insert "((\"prior-a\" :project-dir \"/tmp/a\") (\"prior-b\" :project-dir \"/tmp/b\"))"))
      (setq agent-repl--snapshot-loaded-p t)
      (agent-repl--ws-put "only-live" :project-dir "/tmp/live")
      (agent-repl-save-workspace-snapshot)
      (with-temp-buffer
        (insert-file-contents f)
        (should (string-match-p "only-live" (buffer-string)))
        (should-not (string-match-p "prior-a" (buffer-string)))))))

(ert-deftest agent-repl-cmd-test-snapshot-load/sets-loaded-flag-on-success ()
  "Successful load sets `agent-repl--snapshot-loaded-p' to t."
  (agent-repl-test--with-clean-state
    (agent-repl-cmd-test--with-temp-snapshot-file f
      (let ((tmp-dir (file-name-as-directory (make-temp-file "agent-repl-snap-dir-" t))))
        (unwind-protect
            (progn
              (with-temp-file f
                (prin1 (list (list "ws-a" :project-dir tmp-dir :priority nil))
                       (current-buffer)))
              (cl-letf (((symbol-function 'agent-repl--establish-workspace) #'ignore)
                        ((symbol-function 'agent-repl--snapshot-load-ws-ready-p)
                         (lambda (_ws) t)))
                (setq agent-repl--snapshot-loaded-p nil)
                (agent-repl-load-workspace-snapshot)
                (should agent-repl--snapshot-loaded-p)))
          (delete-directory tmp-dir t))))))

;;;; ---- Workspace cycling (agent-repl-switch-left/right) ----

(defmacro agent-repl-cmd-test--with-cycle-stubs (names current hidden-set
                                                  switched-to flashed protected-p
                                                  &rest body)
  "Bind `+workspace-list-names' / `-current-name' / `-switch' / flash to
fixtures.  NAMES is a list of workspace names, CURRENT is a string,
HIDDEN-SET is a list of names whose `:repl-state' is `:hidden' (the
filter target since hide-mode reimpl moved to persp-level enforcement),
SWITCHED-TO and FLASHED are place-symbols (boxed into single-cell lists)
the stubs push to.  PROTECTED-P is a boolean controlling
`+workspace--protected-p'."
  (declare (indent 7))
  `(cl-letf (((symbol-function 'agent-repl--ws-list-names) (lambda () ,names))
             ((symbol-function '+workspace-current-name) (lambda () ,current))
             ((symbol-function '+workspace--protected-p)
              (lambda (_name) ,protected-p))
             ((symbol-function 'agent-repl--ws-repl-state)
              (lambda (n) (when (member n ,hidden-set) :hidden)))
             ((symbol-function '+workspace-switch)
              (lambda (name &optional _auto-create) (push name ,switched-to)))
             ((symbol-function 'agent-repl--flash-current-tab)
              (lambda () (push t ,flashed))))
     ,@body))

(ert-deftest agent-repl-cmd-test-switch-right/cycles-to-next ()
  "switch-right with hide-mode off cycles to the next workspace."
  (let ((switched (list)) (flashed (list))
        (agent-repl-hide-mode-enabled nil))
    (agent-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "a" '() switched flashed nil
      (agent-repl-switch-right)
      (should (equal switched '("b"))))))

(ert-deftest agent-repl-cmd-test-switch-left/cycles-to-prev ()
  "switch-left with hide-mode off cycles to the previous workspace."
  (let ((switched (list)) (flashed (list))
        (agent-repl-hide-mode-enabled nil))
    (agent-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "b" '() switched flashed nil
      (agent-repl-switch-left)
      (should (equal switched '("a"))))))

(ert-deftest agent-repl-cmd-test-switch-right/wraps-around ()
  "switch-right from the last workspace wraps to the first."
  (let ((switched (list)) (flashed (list))
        (agent-repl-hide-mode-enabled nil))
    (agent-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "c" '() switched flashed nil
      (agent-repl-switch-right)
      (should (equal switched '("a"))))))

(ert-deftest agent-repl-cmd-test-switch-left/wraps-around ()
  "switch-left from the first workspace wraps to the last."
  (let ((switched (list)) (flashed (list))
        (agent-repl-hide-mode-enabled nil))
    (agent-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "a" '() switched flashed nil
      (agent-repl-switch-left)
      (should (equal switched '("c"))))))

(ert-deftest agent-repl-cmd-test-switch-right/skips-hidden-when-hide-on ()
  "With hide-mode on, switch-right skips workspaces whose `:repl-state' is `:hidden'."
  (let ((switched (list)) (flashed (list))
        (agent-repl-hide-mode-enabled t))
    (agent-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "a" '("b") switched flashed nil
      (agent-repl-switch-right)
      (should (equal switched '("c"))))))

(ert-deftest agent-repl-cmd-test-switch-left/skips-hidden-when-hide-on ()
  "With hide-mode on, switch-left skips workspaces whose `:repl-state' is `:hidden'."
  (let ((switched (list)) (flashed (list))
        (agent-repl-hide-mode-enabled t))
    (agent-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "c" '("b") switched flashed nil
      (agent-repl-switch-left)
      (should (equal switched '("a"))))))

(ert-deftest agent-repl-cmd-test-switch-right/single-visible-no-op ()
  "When only the current workspace is visible, switch-right does not switch."
  (let ((switched (list)) (flashed (list))
        (agent-repl-hide-mode-enabled t)
        ;; condition-case-unless-debug skips its handlers when
        ;; `debug-on-error' is set, which ert turns on by default.  Bind
        ;; it off so the user-error path is observable in tests.
        (debug-on-error nil))
    (agent-repl-cmd-test--with-cycle-stubs
        '("a" "b" "c") "a" '("b" "c") switched flashed nil
      (cl-letf (((symbol-function '+workspace-error)
                 (lambda (&rest _) nil)))
        (agent-repl-switch-right)
        (should-not switched)
        (should-not flashed)))))

(ert-deftest agent-repl-cmd-test-switch-right/protected-goes-to-main ()
  "When current workspace is protected, switch-right routes to +workspaces-main."
  (let ((switched (list)) (flashed (list))
        (agent-repl-hide-mode-enabled nil)
        (+workspaces-main "main"))
    (agent-repl-cmd-test--with-cycle-stubs
        '("nil") "nil" '() switched flashed t
      (agent-repl-switch-right)
      (should (equal switched '("main")))
      (should-not flashed))))

(ert-deftest agent-repl-cmd-test-switch-right/does-not-flash-destination ()
  "switch-right does NOT flash the destination tab.
Left/right cycling is high-frequency navigation and the flash becomes
noise; only identity-based jumps (`SPC p p', priority change,
worktree jump) flash."
  (let ((switched (list)) (flashed (list))
        (agent-repl-hide-mode-enabled nil))
    (agent-repl-cmd-test--with-cycle-stubs
        '("a" "b") "a" '() switched flashed nil
      (agent-repl-switch-right)
      (should-not flashed))))

;;;; ---- Hide-mode sweep ----

(defmacro agent-repl-cmd-test--with-sweep-stubs (current killed &rest body)
  "Stub `+workspace-current-name' to return CURRENT and replace
`agent-repl--nuke-one-workspace' with a recorder that pushes the named
ws onto KILLED (a place-symbol bound to a list)."
  (declare (indent 2))
  `(cl-letf (((symbol-function '+workspace-current-name) (lambda () ,current))
             ((symbol-function 'agent-repl--nuke-one-workspace)
              (lambda (ws &rest _) (push ws ,killed))))
     ,@body))

(ert-deftest agent-repl-cmd-test-sweep-hidden/kills-non-current-hidden ()
  "sweep-hidden-workspaces persp-kills every :hidden ws except the current one."
  (agent-repl-test--with-clean-state
    (let ((killed (list)))
      (agent-repl--ws-set-repl-state "ws-a" :hidden)
      (agent-repl--ws-set-repl-state "ws-b" :hidden)
      (agent-repl--ws-set-repl-state "ws-c" :inactive)
      (agent-repl-cmd-test--with-sweep-stubs "ws-c" killed
        (agent-repl--sweep-hidden-workspaces)
        (should (equal (sort killed #'string<) '("ws-a" "ws-b")))))))

(ert-deftest agent-repl-cmd-test-sweep-hidden/skips-current ()
  "sweep-hidden-workspaces never kills the current workspace, even if hidden."
  (agent-repl-test--with-clean-state
    (let ((killed (list)))
      (agent-repl--ws-set-repl-state "ws-a" :hidden)
      (agent-repl-cmd-test--with-sweep-stubs "ws-a" killed
        (agent-repl--sweep-hidden-workspaces)
        (should (null killed))))))

(ert-deftest agent-repl-cmd-test-sweep-hidden/skips-non-hidden ()
  "sweep-hidden-workspaces ignores workspaces with non-:hidden states."
  (agent-repl-test--with-clean-state
    (let ((killed (list)))
      (agent-repl--ws-set-repl-state "ws-a" :inactive)
      (agent-repl--ws-set-repl-state "ws-b" :active)
      (agent-repl--ws-set-repl-state "ws-c" :viewed)
      (agent-repl-cmd-test--with-sweep-stubs "ws-c" killed
        (agent-repl--sweep-hidden-workspaces)
        (should (null killed))))))

(ert-deftest agent-repl-cmd-test-sweep-hidden/forwards-to-nuke ()
  "sweep-hidden-workspaces calls nuke-one-workspace for each `:hidden' ws.
nuke-one-workspace always preserves the on-disk state file, so there's
no explicit purge flag to assert — just that nuke was called with the
right ws name."
  (agent-repl-test--with-clean-state
    (let ((received-args nil))
      (agent-repl--ws-set-repl-state "ws-a" :hidden)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-c"))
                ((symbol-function 'agent-repl--nuke-one-workspace)
                 (lambda (&rest args) (setq received-args args))))
        (agent-repl--sweep-hidden-workspaces)
        (should (equal received-args '("ws-a")))))))

(ert-deftest agent-repl-cmd-test-sweep-hidden/except-overrides-current ()
  "Explicit EXCEPT arg takes precedence over `+workspace-current-name'."
  (agent-repl-test--with-clean-state
    (let ((killed (list)))
      (agent-repl--ws-set-repl-state "ws-a" :hidden)
      (agent-repl--ws-set-repl-state "ws-b" :hidden)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-c"))
                ((symbol-function 'agent-repl--nuke-one-workspace)
                 (lambda (ws &rest _) (push ws killed))))
        ;; EXCEPT="ws-a" should keep ws-a alive even though current is ws-c.
        (agent-repl--sweep-hidden-workspaces "ws-a")
        (should (equal killed '("ws-b")))))))

;;;; ---- maybe-sweep-hidden-on-switch ----

(ert-deftest agent-repl-cmd-test-maybe-sweep/runs-when-hide-on ()
  "maybe-sweep-hidden-on-switch runs the sweep when hide-mode is enabled."
  (agent-repl-test--with-clean-state
    (let ((sweep-called 0)
          (agent-repl-hide-mode-enabled t))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-c"))
                ((symbol-function 'agent-repl--sweep-hidden-workspaces)
                 (lambda (&rest _) (cl-incf sweep-called))))
        (agent-repl--maybe-sweep-hidden-on-switch)
        (should (= sweep-called 1))))))

(ert-deftest agent-repl-cmd-test-maybe-sweep/skips-when-hide-off ()
  "maybe-sweep-hidden-on-switch is a no-op when hide-mode is disabled."
  (agent-repl-test--with-clean-state
    (let ((sweep-called 0)
          (agent-repl-hide-mode-enabled nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-c"))
                ((symbol-function 'agent-repl--sweep-hidden-workspaces)
                 (lambda (&rest _) (cl-incf sweep-called))))
        (agent-repl--maybe-sweep-hidden-on-switch)
        (should (= sweep-called 0))))))

(ert-deftest agent-repl-cmd-test-maybe-sweep/resets-arrived-hidden-to-inactive ()
  "Arriving on a `:hidden' workspace resets it to `:inactive' so the user
actively viewing it does not get it killed.  Independent of hide-mode flag."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-mode-enabled nil))
      (agent-repl--ws-set-repl-state "ws-a" :hidden)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-a"))
                ((symbol-function 'agent-repl--sweep-hidden-workspaces)
                 (lambda (&rest _) nil)))
        (agent-repl--maybe-sweep-hidden-on-switch)
        (should (eq (agent-repl--ws-repl-state "ws-a") :inactive))))))

(ert-deftest agent-repl-cmd-test-maybe-sweep/leaves-non-hidden-current-alone ()
  "maybe-sweep-hidden-on-switch does not touch repl-state if current is not :hidden."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-hide-mode-enabled nil))
      (agent-repl--ws-set-repl-state "ws-a" :active)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-a"))
                ((symbol-function 'agent-repl--sweep-hidden-workspaces)
                 (lambda (&rest _) nil)))
        (agent-repl--maybe-sweep-hidden-on-switch)
        (should (eq (agent-repl--ws-repl-state "ws-a") :active))))))

(ert-deftest agent-repl-cmd-test-maybe-sweep/explicit-ws-overrides-current ()
  "An explicit WS argument takes precedence over `+workspace-current-name'.
This is how `--on-workspace-switch' passes the ws captured at
hook-fire time, so the reset/sweep operate on the workspace that was
just switched to even if another switch raced ahead first."
  (agent-repl-test--with-clean-state
    (let ((swept-with nil)
          (agent-repl-hide-mode-enabled t))
      (agent-repl--ws-set-repl-state "ws-a" :hidden)
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "ws-c"))
                ((symbol-function 'agent-repl--sweep-hidden-workspaces)
                 (lambda (&optional except) (setq swept-with except))))
        (agent-repl--maybe-sweep-hidden-on-switch "ws-a")
        ;; ws-a was reset because it was the explicit arg, even though
        ;; +workspace-current-name returns "ws-c".
        (should (eq (agent-repl--ws-repl-state "ws-a") :inactive))
        (should (equal swept-with "ws-a"))))))

;;;; ---- nuke-one-workspace :last-killed-at stamping ----

(ert-deftest agent-repl-cmd-test-nuke-one/stamps-last-killed-at-on-ws-plist ()
  "`--nuke-one-workspace' records `:last-killed-at' on the ws plist so the
project picker (`SPC p p') can surface most-recently-killed projects
to the top and color the kill-date column."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((persp-mode nil))
      (cl-letf (((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function 'agent-repl--state-save) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl--nuke-one-workspace "ws1" 'preserve-entry)
        (should (agent-repl--ws-get "ws1" :last-killed-at))))))

(ert-deftest agent-repl-cmd-test-nuke-one/state-save-sees-last-killed-at ()
  "`--nuke-one-workspace' stamps `:last-killed-at' BEFORE the pre-teardown
state-save runs, so the on-disk state.el reflects the kill timestamp
even if downstream teardown errors before the redundant save fires."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/tmp/ws1")
    (let ((saw-killed-at nil)
          (persp-mode nil))
      (cl-letf (((symbol-function 'agent-repl--kill-session) #'ignore)
                ((symbol-function 'agent-repl--state-save)
                 (lambda (ws)
                   (setq saw-killed-at (agent-repl--ws-get ws :last-killed-at))))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (agent-repl--nuke-one-workspace "ws1" 'preserve-entry)
        (should saw-killed-at)))))

;;;; ---- Project picker (SPC p p) helpers ----

(ert-deftest agent-repl-cmd-test-picker-status-emoji/live-mirrors-drawer-glyph ()
  "When the project has a workspace, picker mirrors the drawer's glyph
for that workspace — keeps `SPC p p' and the drawer visually consistent
per-workspace rather than collapsing every live ws to a single 🟢."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-live" :agent-state :thinking)
    (let ((summary '(:workspace-name "ws-live" :live-p t
                     :last-killed-at (1 2 3) :has-state t)))
      (should (equal (agent-repl--picker-status-emoji summary)
                     (alist-get :thinking agent-repl-drawer-state-icons))))))

(ert-deftest agent-repl-cmd-test-picker-status-emoji/live-merge-conflict-wins ()
  "Picker mirrors `:merge-conflict' badge for a workspace mid-conflict.
Pins the drawer's repl-state precedence (💥 wins over :agent-state)
through the picker's mirror path."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-conflict" :agent-state :thinking)
    (agent-repl--ws-put "ws-conflict" :repl-state :merge-conflict)
    (let ((summary '(:workspace-name "ws-conflict" :live-p t
                     :has-state t)))
      (should (equal (agent-repl--picker-status-emoji summary) "💥")))))

(ert-deftest agent-repl-cmd-test-picker-status-emoji/live-dominates-killed-at ()
  "A live workspace's drawer glyph wins regardless of `:last-killed-at'
data on the summary — when `:workspace-name' is set the picker reads
the drawer glyph from the cached ws plist and never consults the
non-live 📁 fallback branch."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws-respawned" :agent-state :done)
    (let ((summary '(:workspace-name "ws-respawned"
                     :last-killed-at (1 2 3))))
      (should (equal (agent-repl--picker-status-emoji summary)
                     (alist-get :done agent-repl-drawer-state-icons))))))

(ert-deftest agent-repl-cmd-test-picker-status-emoji/no-workspace-folder ()
  "Without a live workspace the picker returns the neutral 📁 — the
status ladder collapses to live-vs-not-live because the picker does
NOT read the on-disk state file to distinguish historical kill /
dormant / never-opened.  Persisted kill/has-state hints on the summary
are ignored when no live workspace is present."
  (let ((summary '(:workspace-name nil :live-p nil
                   :last-killed-at (1 2 3))))
    (should (equal (agent-repl--picker-status-emoji summary) "📁"))))

(ert-deftest agent-repl-cmd-test-picker-status-emoji/no-workspace-nil-fields ()
  "A summary with no `:workspace-name' and nil date fields still falls
back to 📁 — the live-vs-not-live branch is the only distinction the
picker draws now that it consults only in-memory cached state."
  (let ((summary '(:workspace-name nil :live-p nil
                   :last-killed-at nil)))
    (should (equal (agent-repl--picker-status-emoji summary) "📁"))))

(ert-deftest agent-repl-cmd-test-picker-format-date/formats-real-time ()
  "Picker formats a real time value via `agent-repl--picker-date-format'.
Encoded in local time so `format-time-string' (which uses the local zone
by default) reproduces the calendar date we put in — encoding in UTC
and formatting in a non-UTC local zone would shift the date by a day."
  (let* ((time (encode-time 0 0 12 16 5 2026))
         (str (agent-repl--picker-format-date
               time agent-repl--picker-date-width
               'agent-repl-picker-created-face "----------")))
    (should (equal (substring-no-properties str) "2026-05-16"))))

(ert-deftest agent-repl-cmd-test-picker-format-date/placeholder-for-nil ()
  "When time is nil, picker emits a fixed-width dash placeholder so the
column aligns with rows that have a real date."
  (let ((str (agent-repl--picker-format-date
              nil 10 'agent-repl-picker-killed-face "----------")))
    (should (equal (substring-no-properties str) "----------"))
    (should (= (length (substring-no-properties str)) 10))))

(ert-deftest agent-repl-cmd-test-picker-format-date/applies-face ()
  "Picker propertizes the date string with the supplied face so the two
columns are visually distinct in the candidate list."
  (let ((str (agent-repl--picker-format-date
              nil 10 'agent-repl-picker-killed-face "----------")))
    (should (eq (get-text-property 0 'face str)
                'agent-repl-picker-killed-face))))

(ert-deftest agent-repl-cmd-test-picker-name-width/uses-longest-basename ()
  "When the longest basename exceeds the minimum, picker pads to that
length so date columns line up across all rows."
  (let* ((roots '("/p/a-short" "/p/this-is-a-much-longer-project-name")))
    (should (= (agent-repl--picker-name-width roots)
               (length "this-is-a-much-longer-project-name")))))

(ert-deftest agent-repl-cmd-test-picker-name-width/honors-minimum ()
  "When every basename is short, picker pads to the configured minimum so
short-name-only lists still get a readable column gutter."
  (let ((roots '("/p/a" "/p/b")))
    (should (= (agent-repl--picker-name-width roots)
               agent-repl--picker-name-min-width))))

(ert-deftest agent-repl-cmd-test-picker-time-greater-p/non-nil-vs-non-nil ()
  "Picker time comparison: newer non-nil value sorts before older."
  (should (agent-repl--picker-time-greater-p '(25000 0 0 0)
                                              '(20000 0 0 0)))
  (should-not (agent-repl--picker-time-greater-p '(20000 0 0 0)
                                                  '(25000 0 0 0))))

(ert-deftest agent-repl-cmd-test-picker-time-greater-p/nil-vs-non-nil ()
  "Picker time comparison treats nil as oldest — any real time wins."
  (should (agent-repl--picker-time-greater-p '(25000 0 0 0) nil))
  (should-not (agent-repl--picker-time-greater-p nil '(25000 0 0 0))))

(ert-deftest agent-repl-cmd-test-picker-sort-key/prefers-killed-over-created ()
  "Sort key prefers `:last-killed-at' so projects sort by their most-recent
kill, falling back to creation date only when never killed."
  (let ((summary '(:created-at (10000 0 0 0) :last-killed-at (20000 0 0 0))))
    (should (equal (agent-repl--picker-sort-key summary)
                   '(20000 0 0 0)))))

(ert-deftest agent-repl-cmd-test-picker-sort-key/falls-back-to-created ()
  "When `:last-killed-at' is nil, sort key uses `:created-at' so projects
that have never been killed still sort newest-first by creation."
  (let ((summary '(:created-at (10000 0 0 0) :last-killed-at nil)))
    (should (equal (agent-repl--picker-sort-key summary)
                   '(10000 0 0 0)))))

(ert-deftest agent-repl-cmd-test-project-has-live-workspace-p/matches ()
  "Returns t when any registered workspace points at the given root."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/proj/")
    (should (agent-repl--project-has-live-workspace-p "/tmp/proj"))))

(ert-deftest agent-repl-cmd-test-project-has-live-workspace-p/no-match ()
  "Returns nil when no registered workspace matches the given root."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/other/")
    (should-not (agent-repl--project-has-live-workspace-p "/tmp/proj"))))

(ert-deftest agent-repl-cmd-test-project-has-live-workspace-p/trailing-slash ()
  "Trailing-slash differences don't cause false negatives — both the
registered `:project-dir' and the queried root are normalized."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/proj")
    (should (agent-repl--project-has-live-workspace-p "/tmp/proj/"))))

(ert-deftest agent-repl-cmd-test-project-state-summary/reads-ws-plist-fields ()
  "Summary sources `:created-at', `:last-killed-at', `:priority' from
the live workspace plist — picker reads only in-memory cached state,
never the on-disk state file."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-summary-" t)))
          (created '(10000 0 0 0))
          (killed  '(20000 0 0 0)))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws-a" :project-dir tmpdir)
            (agent-repl--ws-put "ws-a" :created-at created)
            (agent-repl--ws-put "ws-a" :last-killed-at killed)
            (agent-repl--ws-put "ws-a" :priority "p1")
            (let ((summary (agent-repl--project-state-summary tmpdir)))
              (should (equal (plist-get summary :created-at) created))
              (should (equal (plist-get summary :last-killed-at) killed))
              (should (equal (plist-get summary :priority) "p1"))
              (should (plist-get summary :live-p))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-cmd-test-project-state-summary/no-workspace ()
  "Without a live workspace pointing at the project, summary's date and
priority fields are all nil — the picker deliberately does not consult
the state file on disk, so projects without an in-memory entry surface
with placeholder columns."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-no-state-" t))))
      (unwind-protect
          (let ((summary (agent-repl--project-state-summary tmpdir)))
            (should-not (plist-get summary :created-at))
            (should-not (plist-get summary :last-killed-at))
            (should-not (plist-get summary :priority))
            (should-not (plist-get summary :live-p)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-cmd-test-project-state-summary/ignores-on-disk-state ()
  "Pins the no-disk-IO contract: even when an on-disk state file exists
with full `:created-at' / `:last-killed-at' values, the summary
returns nil for those keys unless a live workspace also points at the
project.  Regression guard against re-introducing a state-file read
in the picker hot path."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-ignore-disk-" t))))
      (unwind-protect
          (progn
            (agent-repl-test--seed-file
             (agent-repl--state-file tmpdir)
             (prin1-to-string '(:created-at (10000 0 0 0)
                                :last-killed-at (20000 0 0 0)
                                :priority "p-disk")))
            (let ((summary (agent-repl--project-state-summary tmpdir)))
              (should-not (plist-get summary :created-at))
              (should-not (plist-get summary :last-killed-at))
              (should-not (plist-get summary :priority))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-cmd-test-project-state-summary/no-disk-read-when-no-workspace ()
  "Picker's summary path makes ZERO `agent-repl--read-sexp-file' calls
when no live workspace matches the project — anchors the cached-only
contract by counting actual function invocations rather than just
inspecting the returned plist."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-no-read-" t)))
          (read-count 0))
      (unwind-protect
          (cl-letf* ((orig (symbol-function 'agent-repl--read-sexp-file))
                     ((symbol-function 'agent-repl--read-sexp-file)
                      (lambda (&rest args)
                        (cl-incf read-count)
                        (apply orig args))))
            (agent-repl--project-state-summary tmpdir)
            (should (= read-count 0)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-cmd-test-project-state-summary/workspace-name-resolves ()
  "Summary's `:workspace-name' is the registered ws whose `:project-dir'
matches the queried root — picker uses this to mirror the drawer's
per-workspace glyph."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-wsname-" t))))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws-x" :project-dir tmpdir)
            (let ((summary (agent-repl--project-state-summary tmpdir)))
              (should (equal (plist-get summary :workspace-name) "ws-x"))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-cmd-test-project-state-summary/workspace-name-nil-when-no-ws ()
  "Summary's `:workspace-name' is nil when no workspace points at the
queried root — picker falls back to the project-level emoji ladder."
  (agent-repl-test--with-clean-state
    (let ((tmpdir (file-name-as-directory (make-temp-file "picker-no-ws-" t))))
      (unwind-protect
          (let ((summary (agent-repl--project-state-summary tmpdir)))
            (should-not (plist-get summary :workspace-name)))
        (delete-directory tmpdir t)))))

(ert-deftest agent-repl-cmd-test-build-project-picker-candidates/sorted-by-killed-at ()
  "Picker sorts entries most-recently-killed first.  Sort key is sourced
from each project's live workspace plist (cached in-memory) — no
state-file reads — so older kill ranks below newer kill regardless of
created-at."
  (agent-repl-test--with-clean-state
    (let* ((tmp-old (file-name-as-directory (make-temp-file "picker-old-kill-" t)))
           (tmp-new (file-name-as-directory (make-temp-file "picker-new-kill-" t))))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws-old" :project-dir tmp-old)
            (agent-repl--ws-put "ws-old" :created-at '(30000 0 0 0))
            (agent-repl--ws-put "ws-old" :last-killed-at '(10000 0 0 0))
            (agent-repl--ws-put "ws-new" :project-dir tmp-new)
            (agent-repl--ws-put "ws-new" :created-at '(10000 0 0 0))
            (agent-repl--ws-put "ws-new" :last-killed-at '(20000 0 0 0))
            (let* ((candidates (agent-repl--build-project-picker-candidates
                                (list tmp-old tmp-new)))
                   (roots (mapcar #'cdr candidates)))
              (should (equal (car roots) tmp-new))
              (should (equal (cadr roots) tmp-old))))
        (delete-directory tmp-old t)
        (delete-directory tmp-new t)))))

(ert-deftest agent-repl-cmd-test-build-project-picker-candidates/sorted-by-created-when-no-kill ()
  "Projects with no `:last-killed-at' sort among themselves by the
workspace plist's `:created-at' (newest-first).  Source-of-truth is
the in-memory hash, not the on-disk state file."
  (agent-repl-test--with-clean-state
    (let* ((tmp-old (file-name-as-directory (make-temp-file "picker-old-create-" t)))
           (tmp-new (file-name-as-directory (make-temp-file "picker-new-create-" t))))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws-old" :project-dir tmp-old)
            (agent-repl--ws-put "ws-old" :created-at '(10000 0 0 0))
            (agent-repl--ws-put "ws-new" :project-dir tmp-new)
            (agent-repl--ws-put "ws-new" :created-at '(20000 0 0 0))
            (let* ((candidates (agent-repl--build-project-picker-candidates
                                (list tmp-old tmp-new)))
                   (roots (mapcar #'cdr candidates)))
              (should (equal (car roots) tmp-new))
              (should (equal (cadr roots) tmp-old))))
        (delete-directory tmp-old t)
        (delete-directory tmp-new t)))))

(ert-deftest agent-repl-cmd-test-build-project-picker-candidates/non-live-sorts-last ()
  "A project without a live workspace has nil sort-key (no cached
dates).  It sinks below every project with a cached created-at — the
picker surfaces live/recently-touched workspaces and pushes
never-opened or fully-killed ones to the bottom."
  (agent-repl-test--with-clean-state
    (let* ((tmp-live (file-name-as-directory (make-temp-file "picker-live-" t)))
           (tmp-none (file-name-as-directory (make-temp-file "picker-none-" t))))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws-live" :project-dir tmp-live)
            (agent-repl--ws-put "ws-live" :created-at '(10000 0 0 0))
            (let* ((candidates (agent-repl--build-project-picker-candidates
                                (list tmp-none tmp-live)))
                   (roots (mapcar #'cdr candidates)))
              (should (equal (car roots) tmp-live))
              (should (equal (cadr roots) tmp-none))))
        (delete-directory tmp-live t)
        (delete-directory tmp-none t)))))

(ert-deftest agent-repl-cmd-test-build-project-picker-candidates/display-includes-emoji ()
  "Each candidate display string starts with the status emoji prefix so
users can scan the list at a glance.  A project with no live workspace
gets the neutral 📁 (the picker no longer distinguishes
killed/dormant/never-opened — that distinction required disk I/O)."
  (agent-repl-test--with-clean-state
    (let ((tmp (file-name-as-directory (make-temp-file "picker-display-" t))))
      (unwind-protect
          (let* ((candidates (agent-repl--build-project-picker-candidates
                              (list tmp)))
                 (display (substring-no-properties (car (car candidates)))))
            (should (string-prefix-p "📁" display)))
        (delete-directory tmp t)))))

(ert-deftest agent-repl-cmd-test-build-project-picker-candidates/display-mirrors-drawer-glyph ()
  "When a workspace points at the project, the picker's candidate display
opens with the drawer's per-workspace glyph rather than the legacy 🟢
\"any live ws\" emoji — pins the consistency the picker promises with
the drawer."
  (agent-repl-test--with-clean-state
    (let ((tmp (file-name-as-directory (make-temp-file "picker-glyph-mirror-" t))))
      (unwind-protect
          (progn
            (agent-repl--ws-put "live-ws" :project-dir tmp)
            (agent-repl--ws-put "live-ws" :agent-state :idle)
            (let* ((candidates (agent-repl--build-project-picker-candidates
                                (list tmp)))
                   (display (substring-no-properties (car (car candidates)))))
              (should (string-prefix-p
                       (alist-get :idle agent-repl-drawer-state-icons)
                       display))))
        (delete-directory tmp t)))))

(ert-deftest agent-repl-cmd-test-build-project-picker-candidates/display-aligns-columns ()
  "Picker pads the project-name column to a uniform width so the date
columns line up across rows even when basenames differ in length.

Both rows in this test are never-opened (no state file), so each
display contains two `----------' placeholders — one for created, one
for last-killed.  We anchor on the first placeholder occurrence in
each row; aligned columns mean that position is identical."
  (agent-repl-test--with-clean-state
    (let* ((short (file-name-as-directory (make-temp-file "ab-" t)))
           (long  (file-name-as-directory (make-temp-file "xyz-much-longer-basename-" t))))
      (unwind-protect
          (let* ((candidates (agent-repl--build-project-picker-candidates
                              (list short long)))
                 (displays (mapcar (lambda (c) (substring-no-properties (car c)))
                                   candidates))
                 (positions (mapcar (lambda (d)
                                      (string-match "----------" d))
                                    displays)))
            (should (apply #'= positions)))
        (delete-directory short t)
        (delete-directory long t)))))

(ert-deftest agent-repl-cmd-test-read-project-via-picker/captures-cdr ()
  "Picker returns the project root (cdr of the selected candidate), never
the propertized display string, regardless of the shape ivy passes to
the action closure."
  (agent-repl-test--with-clean-state
    (let ((tmp (file-name-as-directory (make-temp-file "picker-capture-" t))))
      (unwind-protect
          (cl-letf (((symbol-function 'projectile-relevant-known-projects)
                     (lambda () (list tmp)))
                    ((symbol-function 'ivy-read)
                     (lambda (_prompt candidates &rest args)
                       ;; Simulate ivy passing the cons cell into the
                       ;; action; the closure should setq the cdr.
                       (let ((action (plist-get args :action)))
                         (funcall action (car candidates))))))
            (should (equal (agent-repl--read-project-via-picker) tmp)))
        (delete-directory tmp t)))))

(ert-deftest agent-repl-cmd-test-read-project-via-picker/string-shape ()
  "Picker also handles the ivy-shape where the action receives the
display string rather than the cons cell — it falls back to assoc on
the candidate list."
  (agent-repl-test--with-clean-state
    (let ((tmp (file-name-as-directory (make-temp-file "picker-string-" t))))
      (unwind-protect
          (cl-letf (((symbol-function 'projectile-relevant-known-projects)
                     (lambda () (list tmp)))
                    ((symbol-function 'ivy-read)
                     (lambda (_prompt candidates &rest args)
                       (let ((action (plist-get args :action)))
                         ;; Pass just the display string.
                         (funcall action (car (car candidates)))))))
            (should (equal (agent-repl--read-project-via-picker) tmp)))
        (delete-directory tmp t)))))

;;;; ---- Indexed workspace switchers (M-1..M-9, M-0) ----
;;
;; Tests cover `agent-repl--workspace-switch-by-index' (private core)
;; and each of the public `agent-repl-workspace-switch-to-*' commands.
;; The wrappers are thin persp wrappers that intentionally ignore
;; `current-prefix-arg' — that's the property under test.

(defmacro agent-repl-cmd-test--with-switch-stubs (names switched-to &rest body)
  "Bind `+workspace-list-names' to NAMES and `+workspace-switch' to push
into SWITCHED-TO so the test can assert the destination passed by the
indexed switchers."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'agent-repl--ws-list-names) (lambda () ,names))
             ((symbol-function '+workspace-switch)
              (lambda (name &optional _auto-create) (push name ,switched-to))))
     ,@body))

(ert-deftest agent-repl-cmd-test-switch-by-index/picks-nth-name ()
  "switch-by-index dispatches `+workspace-switch' on the nth name."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d") switched
      (agent-repl--workspace-switch-by-index 2)
      (should (equal switched '("c"))))))

(ert-deftest agent-repl-cmd-test-switch-by-index/out-of-range-user-errors ()
  "switch-by-index signals `user-error' when no workspace exists at INDEX."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b") switched
      (should-error (agent-repl--workspace-switch-by-index 5)
                    :type 'user-error)
      (should-not switched))))

(ert-deftest agent-repl-cmd-test-switch-to-0/lands-on-first ()
  "switch-to-0 routes to the 1st workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c") switched
      (agent-repl-workspace-switch-to-0)
      (should (equal switched '("a"))))))

(ert-deftest agent-repl-cmd-test-switch-to-1/lands-on-second ()
  "switch-to-1 routes to the 2nd workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c") switched
      (agent-repl-workspace-switch-to-1)
      (should (equal switched '("b"))))))

(ert-deftest agent-repl-cmd-test-switch-to-2/lands-on-third ()
  "switch-to-2 routes to the 3rd workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c") switched
      (agent-repl-workspace-switch-to-2)
      (should (equal switched '("c"))))))

(ert-deftest agent-repl-cmd-test-switch-to-3/lands-on-fourth ()
  "switch-to-3 routes to the 4th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d") switched
      (agent-repl-workspace-switch-to-3)
      (should (equal switched '("d"))))))

(ert-deftest agent-repl-cmd-test-switch-to-4/lands-on-fifth ()
  "switch-to-4 routes to the 5th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e") switched
      (agent-repl-workspace-switch-to-4)
      (should (equal switched '("e"))))))

(ert-deftest agent-repl-cmd-test-switch-to-5/lands-on-sixth ()
  "switch-to-5 routes to the 6th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f") switched
      (agent-repl-workspace-switch-to-5)
      (should (equal switched '("f"))))))

(ert-deftest agent-repl-cmd-test-switch-to-6/lands-on-seventh ()
  "switch-to-6 routes to the 7th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g") switched
      (agent-repl-workspace-switch-to-6)
      (should (equal switched '("g"))))))

(ert-deftest agent-repl-cmd-test-switch-to-7/lands-on-eighth ()
  "switch-to-7 routes to the 8th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h") switched
      (agent-repl-workspace-switch-to-7)
      (should (equal switched '("h"))))))

(ert-deftest agent-repl-cmd-test-switch-to-8/lands-on-ninth ()
  "switch-to-8 routes to the 9th workspace — this is the regression
target for the M-9 misbehavior where it sometimes landed on the
final workspace instead."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j") switched
      (agent-repl-workspace-switch-to-8)
      (should (equal switched '("i"))))))

(ert-deftest agent-repl-cmd-test-switch-to-8/ignores-prefix-arg ()
  "switch-to-8 ignores `current-prefix-arg' — pressing a prefix-arg key
beforehand must not redirect the jump to a different index."
  (let ((switched (list))
        (current-prefix-arg 99))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i") switched
      (agent-repl-workspace-switch-to-8)
      (should (equal switched '("i"))))))

;;;; Second nine (Option M-1..M-9 -> workspaces 10-18)

(ert-deftest agent-repl-cmd-test-switch-to-9/lands-on-tenth ()
  "switch-to-9 routes to the 10th workspace (first of the second nine)."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j") switched
      (agent-repl-workspace-switch-to-9)
      (should (equal switched '("j"))))))

(ert-deftest agent-repl-cmd-test-switch-to-9/ignores-prefix-arg ()
  "switch-to-9 ignores `current-prefix-arg' — the second-nine wrappers
preserve the prefix-arg-free contract of the first nine."
  (let ((switched (list))
        (current-prefix-arg 99))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j") switched
      (agent-repl-workspace-switch-to-9)
      (should (equal switched '("j"))))))

(ert-deftest agent-repl-cmd-test-switch-to-10/lands-on-eleventh ()
  "switch-to-10 routes to the 11th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k") switched
      (agent-repl-workspace-switch-to-10)
      (should (equal switched '("k"))))))

(ert-deftest agent-repl-cmd-test-switch-to-11/lands-on-twelfth ()
  "switch-to-11 routes to the 12th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l") switched
      (agent-repl-workspace-switch-to-11)
      (should (equal switched '("l"))))))

(ert-deftest agent-repl-cmd-test-switch-to-12/lands-on-thirteenth ()
  "switch-to-12 routes to the 13th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m") switched
      (agent-repl-workspace-switch-to-12)
      (should (equal switched '("m"))))))

(ert-deftest agent-repl-cmd-test-switch-to-13/lands-on-fourteenth ()
  "switch-to-13 routes to the 14th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n") switched
      (agent-repl-workspace-switch-to-13)
      (should (equal switched '("n"))))))

(ert-deftest agent-repl-cmd-test-switch-to-14/lands-on-fifteenth ()
  "switch-to-14 routes to the 15th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o") switched
      (agent-repl-workspace-switch-to-14)
      (should (equal switched '("o"))))))

(ert-deftest agent-repl-cmd-test-switch-to-15/lands-on-sixteenth ()
  "switch-to-15 routes to the 16th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p") switched
      (agent-repl-workspace-switch-to-15)
      (should (equal switched '("p"))))))

(ert-deftest agent-repl-cmd-test-switch-to-16/lands-on-seventeenth ()
  "switch-to-16 routes to the 17th workspace."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q") switched
      (agent-repl-workspace-switch-to-16)
      (should (equal switched '("q"))))))

(ert-deftest agent-repl-cmd-test-switch-to-17/lands-on-eighteenth ()
  "switch-to-17 routes to the 18th workspace (last of the second nine)."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r") switched
      (agent-repl-workspace-switch-to-17)
      (should (equal switched '("r"))))))

(ert-deftest agent-repl-cmd-test-switch-to-final/lands-on-last ()
  "switch-to-final routes to the last name in the workspace list."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d") switched
      (agent-repl-workspace-switch-to-final)
      (should (equal switched '("d"))))))

(ert-deftest agent-repl-cmd-test-switch-to-final/ignores-prefix-arg ()
  "switch-to-final ignores `current-prefix-arg' — this is the regression
target for the M-0 case where `+workspace/switch-to' would consult the
prefix arg and the binding sometimes fell through to `text-scale-set'
with the \"The font hasn't been resized\" message."
  (let ((switched (list))
        (current-prefix-arg 3))
    (agent-repl-cmd-test--with-switch-stubs
        '("a" "b" "c" "d" "e") switched
      (agent-repl-workspace-switch-to-final)
      (should (equal switched '("e"))))))

(ert-deftest agent-repl-cmd-test-switch-to-final/empty-list-user-errors ()
  "switch-to-final signals `user-error' when no workspaces exist."
  (let ((switched (list)))
    (agent-repl-cmd-test--with-switch-stubs
        '() switched
      (should-error (agent-repl-workspace-switch-to-final)
                    :type 'user-error)
      (should-not switched))))

;;;; ---- Tests: indexed switchers vs. folded repos ----
;;
;; A repo folded in the drawer takes its workspaces out of the tab-bar,
;; and the indexed switchers index the SAME list — so the visible tab
;; numbers stay contiguous.  These pin that contract.

(ert-deftest agent-repl-cmd-test-switch-by-index/skips-folded-repo ()
  "switch-by-index numbers the VISIBLE tabs: a folded repo's workspaces
are not counted, so index 1 lands on the next unfolded workspace."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "a"  :group-key "/repos/doom/.git")
    (agent-repl--ws-put "ee" :group-key "/repos/explanation-engine/.git")
    (agent-repl--ws-put "b"  :group-key "/repos/doom/.git")
    (agent-repl--toggle-repo-fold "/repos/explanation-engine/.git")
    (let ((switched (list)))
      (cl-letf (((symbol-function 'agent-repl--ws-list-names)
                 (lambda () '("a" "ee" "b")))
                ((symbol-function 'agent-repl--ws-current-name)
                 (lambda () "a"))
                ((symbol-function '+workspace-switch)
                 (lambda (name &optional _auto-create) (push name switched))))
        (agent-repl--workspace-switch-by-index 1)
        (should (equal switched '("b")))))))

(ert-deftest agent-repl-cmd-test-switch-to-final/skips-folded-repo ()
  "switch-to-final lands on the last VISIBLE workspace, not a folded one."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "a"  :group-key "/repos/doom/.git")
    (agent-repl--ws-put "ee" :group-key "/repos/explanation-engine/.git")
    (agent-repl--toggle-repo-fold "/repos/explanation-engine/.git")
    (let ((switched (list)))
      (cl-letf (((symbol-function 'agent-repl--ws-list-names)
                 (lambda () '("a" "ee")))
                ((symbol-function 'agent-repl--ws-current-name)
                 (lambda () "a"))
                ((symbol-function '+workspace-switch)
                 (lambda (name &optional _auto-create) (push name switched))))
        (agent-repl-workspace-switch-to-final)
        (should (equal switched '("a")))))))

(ert-deftest agent-repl-cmd-test-workspace-cycle/skips-folded-repo ()
  "Left/right cycling skips the workspaces of a folded repo."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "a"  :group-key "/repos/doom/.git")
    (agent-repl--ws-put "ee" :group-key "/repos/explanation-engine/.git")
    (agent-repl--ws-put "b"  :group-key "/repos/doom/.git")
    (agent-repl--toggle-repo-fold "/repos/explanation-engine/.git")
    (let ((switched (list)))
      (cl-letf (((symbol-function 'agent-repl--ws-list-names)
                 (lambda () '("a" "ee" "b")))
                ((symbol-function 'agent-repl--ws-current-name)
                 (lambda () "a"))
                ((symbol-function 'agent-repl--ws-protected-p)
                 (lambda (_ws) nil))
                ((symbol-function '+workspace-switch)
                 (lambda (name &optional _auto-create) (push name switched))))
        (agent-repl--workspace-cycle +1)
        (should (equal switched '("b")))))))

(ert-deftest agent-repl-cmd-test-switch-to-N/is-interactive ()
  "Each indexed switcher is an interactive command — required for keymap
binding to invoke it via key press."
  (dolist (fn '(agent-repl-workspace-switch-to-0
                agent-repl-workspace-switch-to-1
                agent-repl-workspace-switch-to-2
                agent-repl-workspace-switch-to-3
                agent-repl-workspace-switch-to-4
                agent-repl-workspace-switch-to-5
                agent-repl-workspace-switch-to-6
                agent-repl-workspace-switch-to-7
                agent-repl-workspace-switch-to-8
                agent-repl-workspace-switch-to-final))
    (should (commandp fn))))

;;;; ---- Tests: workspace tab-order shuffles (extracted from +dwc/) ----

(ert-deftest agent-repl-test-workspace-push-to-back-is-command ()
  "agent-repl-workspace-push-to-back is interactively invokable."
  (should (commandp 'agent-repl-workspace-push-to-back)))

(ert-deftest agent-repl-test-workspace-pull-to-front-is-command ()
  "agent-repl-workspace-pull-to-front is interactively invokable."
  (should (commandp 'agent-repl-workspace-pull-to-front)))

(ert-deftest agent-repl-test-workspace-push-to-back-reorders-list ()
  "push-to-back moves the current workspace to the second-to-last position.
With ws-list (a b c d) and current=b, the result should be (a c b d)."
  (agent-repl-test--with-clean-state
    (let ((updated-names nil)
          (flash-called 0))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "b"))
                ((symbol-function 'persp-names-current-frame-fast-ordered)
                 (lambda () '("a" "b" "c" "d")))
                ((symbol-function 'persp-update-names-cache)
                 (lambda (names) (setq updated-names names)))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function '+workspace-switch) #'ignore)
                ((symbol-function 'agent-repl-flash-tab)
                 (lambda (_ws) (cl-incf flash-called))))
        (agent-repl-workspace-push-to-back)
        (should (equal updated-names '("a" "c" "b" "d")))
        (should (= flash-called 1))))))

(ert-deftest agent-repl-test-workspace-push-to-back-keeps-focus-when-asked ()
  "With KEEP-FOCUS non-nil, the function does NOT switch away from current."
  (agent-repl-test--with-clean-state
    (let ((switched-to nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "b"))
                ((symbol-function 'persp-names-current-frame-fast-ordered)
                 (lambda () '("a" "b" "c" "d")))
                ((symbol-function 'persp-update-names-cache) #'ignore)
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function '+workspace-switch)
                 (lambda (ws &rest _) (setq switched-to ws)))
                ((symbol-function 'agent-repl-flash-tab) #'ignore))
        (agent-repl-workspace-push-to-back t)
        (should-not switched-to)))))

(ert-deftest agent-repl-test-workspace-pull-to-front-reorders-list ()
  "pull-to-front moves the current workspace to the second position.
With ws-list (a b c d) and current=c, the result should be (a c b d)."
  (agent-repl-test--with-clean-state
    (let ((updated-names nil))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () "c"))
                ((symbol-function 'persp-names-current-frame-fast-ordered)
                 (lambda () '("a" "b" "c" "d")))
                ((symbol-function 'persp-update-names-cache)
                 (lambda (names) (setq updated-names names)))
                ((symbol-function 'agent-repl--force-tab-bar-redraw) #'ignore)
                ((symbol-function '+workspace-switch) #'ignore))
        (agent-repl-workspace-pull-to-front)
        (should (equal updated-names '("a" "c" "b" "d")))))))

;;;; ---- Tests: agent-repl-open-most-recent-workspace (moved from config.el) ----

(ert-deftest agent-repl-test-open-most-recent-switches-to-history-head ()
  "open-most-recent-workspace switches to the most recent unopened ws and records it."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("a" "b"))
          (agent-repl--opened-recent-workspaces nil)
          (switched nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "cur"))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq switched ws))))
        (agent-repl-open-most-recent-workspace)
        (should (equal switched "a"))
        (should (member "a" agent-repl--opened-recent-workspaces))))))

(ert-deftest agent-repl-test-open-most-recent-skips-already-opened ()
  "open-most-recent-workspace skips workspaces already opened this cycle."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("a" "b"))
          (agent-repl--opened-recent-workspaces '("a"))
          (switched nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "cur"))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq switched ws))))
        (agent-repl-open-most-recent-workspace)
        (should (equal switched "b"))))))

(ert-deftest agent-repl-test-open-most-recent-falls-back-to-all-names ()
  "open-most-recent-workspace falls back to the full ws list when history is empty."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history nil)
          (agent-repl--opened-recent-workspaces nil)
          (switched nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "cur"))
                ((symbol-function 'agent-repl--ws-all-names) (lambda () '("cur" "x")))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq switched ws))))
        (agent-repl-open-most-recent-workspace)
        (should (equal switched "x"))))))

(ert-deftest agent-repl-test-open-most-recent-resets-when-all-visited ()
  "open-most-recent-workspace resets the opened set and does not switch when none remain."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspace-history '("a"))
          (agent-repl--opened-recent-workspaces '("a"))
          (switched nil))
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "cur"))
                ((symbol-function 'agent-repl--ws-all-names) (lambda () '("cur" "a")))
                ((symbol-function 'agent-repl--ws-switch)
                 (lambda (ws &rest _) (setq switched ws)))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (agent-repl-open-most-recent-workspace)
        (should-not switched)
        (should-not agent-repl--opened-recent-workspaces)))))

(ert-deftest agent-repl-test-send-to-agent-transitions-permission-to-thinking ()
  "`agent-repl--send-to-agent' flips :permission -> :thinking after dispatching.
This predefined-prompt path goes straight to vterm and does NOT funnel
through `agent-repl--do-send'; the flip is inherited from the real
`agent-repl--send-input-to-vterm' (the lowest-level string-send
primitive), so only the bracketed transport beneath it is stubbed."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (agent-repl-test--with-temp-buffer "*agent-panel-send-to-agent-perm*"
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (agent-repl--ws-set-agent-state "ws1" :permission)
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) t))
                ((symbol-function 'agent-repl--send-input-bracketed) #'ignore))
        (agent-repl--send-to-agent "do the thing"))
      (should (eq (agent-repl--ws-agent-state "ws1") :thinking)))))

(ert-deftest agent-repl-test-send-to-agent-leaves-non-permission-state-unchanged ()
  "`agent-repl--send-to-agent' only transitions :permission, not other states."
  (agent-repl-test--with-clean-state
    (agent-repl-test--use-vterm-frontend)
    (agent-repl-test--with-temp-buffer "*agent-panel-send-to-agent-idle*"
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (agent-repl--ws-set-agent-state "ws1" :idle)
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
                ((symbol-function 'agent-repl--agent-running-p) (lambda (&rest _) t))
                ((symbol-function 'agent-repl--send-input-bracketed) #'ignore))
        (agent-repl--send-to-agent "do the thing"))
      (should (eq (agent-repl--ws-agent-state "ws1") :idle)))))

;;;; ---- agent-repl-kill-agent-process ----

(ert-deftest agent-repl-cmd-test-kill-agent-process/signals-term ()
  "kill-agent-process sends SIGTERM to the found agent pid via the boundary wrapper."
  (let ((sent nil))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--agent-process-pid) (lambda (_ws) 4242))
              ((symbol-function 'agent-repl--signal-process)
               (lambda (pid sig) (setq sent (cons pid sig)))))
      (agent-repl-kill-agent-process)
      (should (equal sent '(4242 . TERM))))))

(ert-deftest agent-repl-cmd-test-kill-agent-process/no-process-errors ()
  "kill-agent-process signals user-error and signals nothing when no agent process is found."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws1"))
              ((symbol-function 'agent-repl--agent-process-pid) (lambda (_ws) nil))
              ((symbol-function 'agent-repl--signal-process)
               (lambda (&rest _) (setq called t))))
      (should-error (agent-repl-kill-agent-process) :type 'user-error)
      (should-not called))))

(ert-deftest agent-repl-cmd-test-agent-process-pid/finds-claude-child ()
  "agent-process-pid returns the vterm shell's child whose comm matches claude, not a sibling."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-vterm-kill*")))
      (unwind-protect
          (cl-letf (((symbol-function 'get-buffer-process) (lambda (_b) 'fake-proc))
                    ((symbol-function 'process-id) (lambda (p) (when (eq p 'fake-proc) 100)))
                    ((symbol-function 'list-system-processes) (lambda () '(100 200 300)))
                    ((symbol-function 'process-attributes)
                     (lambda (pid)
                       (pcase pid
                         (100 '((comm . "zsh")    (ppid . 1)))
                         (200 '((comm . "node")   (ppid . 100)))
                         (300 '((comm . "claude") (ppid . 100)))))))
            (agent-repl--ws-put "ws1" :vterm-buffer buf)
            (should (equal (agent-repl--agent-process-pid "ws1") 300)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-cmd-test-agent-process-pid/version-comm-sole-child ()
  "agent-process-pid returns the shell's sole child even when its comm is a version string, not \"claude\" (the native-binary case)."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-vterm-kill2*")))
      (unwind-protect
          (cl-letf (((symbol-function 'get-buffer-process) (lambda (_b) 'fake-proc))
                    ((symbol-function 'process-id) (lambda (p) (when (eq p 'fake-proc) 100)))
                    ((symbol-function 'list-system-processes) (lambda () '(100 555)))
                    ((symbol-function 'process-attributes)
                     (lambda (pid)
                       (pcase pid
                         (100 '((comm . "zsh")     (ppid . 1)))
                         (555 '((comm . "2.1.206") (ppid . 100)))))))
            (agent-repl--ws-put "ws1" :vterm-buffer buf)
            (should (equal (agent-repl--agent-process-pid "ws1") 555)))
        (kill-buffer buf)))))

(ert-deftest agent-repl-cmd-test-agent-process-pid/no-children-nil ()
  "agent-process-pid returns nil when the vterm shell has no child process."
  (agent-repl-test--with-clean-state
    (let ((buf (get-buffer-create " *test-vterm-kill3*")))
      (unwind-protect
          (cl-letf (((symbol-function 'get-buffer-process) (lambda (_b) 'fake-proc))
                    ((symbol-function 'process-id) (lambda (p) (when (eq p 'fake-proc) 100)))
                    ((symbol-function 'list-system-processes) (lambda () '(100 999)))
                    ((symbol-function 'process-attributes)
                     (lambda (pid)
                       (pcase pid
                         (100 '((comm . "zsh")   (ppid . 1)))
                         (999 '((comm . "other") (ppid . 42)))))))
            (agent-repl--ws-put "ws1" :vterm-buffer buf)
            (should-not (agent-repl--agent-process-pid "ws1")))
        (kill-buffer buf)))))

(provide 'test-commands)

;;; test-commands.el ends here
