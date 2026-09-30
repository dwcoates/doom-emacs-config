;;; test-commands.el --- ERT tests for agent-repl commands.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-commands.el -f ert-run-tests-batch-and-exit
;;
;; The composer pipeline (`agent-repl--send') is stubbed and RECORDS what it
;; was handed, because that is exactly what these tests are about: every
;; canned prompt family composes TEXT and hands it to the one send pipeline
;; under ITS OWN origin.  The origin is the load-bearing assertion -- the
;; vocabulary is closed and durable so a stored turn traces back to the
;; editor situation that caused it, and a family sending the wrong value
;; silently corrupts that record.
;;
;; What is NOT here went with the code: the workspace snapshot and its
;; revival picker, client tab ordering, the interrupt family, and
;; close/kill/nuke (verbs.el owns them, and test-verbs.el covers them).

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defvar agent-repl-test-commands--sent nil
  "Submissions the stubbed pipeline received, as (ORIGIN TEXT WS).")

(defmacro agent-repl-test-commands--with (&rest body)
  "Run BODY with the composer pipeline stubbed and a current workspace."
  (declare (indent 0))
  `(let ((agent-repl-test-commands--sent nil))
     (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "ws-one"))
               ((symbol-function 'agent-repl--ws-current-log-name) (lambda () "ws-one"))
               ((symbol-function 'agent-repl--send)
                (lambda (origin &optional prompt ws _force)
                  (push (list origin prompt ws) agent-repl-test-commands--sent)
                  "key-1"))
               ((symbol-function 'message) (lambda (&rest _) nil)))
       ,@body)))

(defun agent-repl-test-commands--origin ()
  "Return the origin of the single recorded submission."
  (nth 0 (car agent-repl-test-commands--sent)))

(defun agent-repl-test-commands--text ()
  "Return the text of the single recorded submission."
  (nth 1 (car agent-repl-test-commands--sent)))

;;;; ---- Every family's own origin ----

(ert-deftest agent-repl-test-commands-diff-family-origin ()
  "The diff-analysis family sends `:command-diff-analysis'."
  (agent-repl-test-commands--with
    (agent-repl-explain-diff-worktree)
    (should (eq (agent-repl-test-commands--origin) :command-diff-analysis))))

(ert-deftest agent-repl-test-commands-explain-origin ()
  "`agent-repl-explain' sends `:command-explain-context'."
  (agent-repl-test-commands--with
    (cl-letf (((symbol-function 'agent-repl--context-reference) (lambda () "a.el:3")))
      (agent-repl-explain)
      (should (eq (agent-repl-test-commands--origin) :command-explain-context)))))

(ert-deftest agent-repl-test-commands-explain-prompt-origin ()
  "`agent-repl-explain-prompt' sends `:command-explain-prompt'."
  (agent-repl-test-commands--with
    (cl-letf (((symbol-function 'agent-repl--context-reference) (lambda () "a.el:3"))
              ((symbol-function 'read-string) (lambda (&rest _) "why is this here")))
      (agent-repl-explain-prompt)
      (should (eq (agent-repl-test-commands--origin) :command-explain-prompt)))))

(ert-deftest agent-repl-test-commands-update-pr-origin ()
  "`agent-repl-update-pr' sends `:command-update-pr'."
  (agent-repl-test-commands--with
    (agent-repl-update-pr)
    (should (eq (agent-repl-test-commands--origin) :command-update-pr))))

(ert-deftest agent-repl-test-commands-rebase-origin ()
  "The rebase dispatch sends `:command-rebase'."
  (agent-repl-test-commands--with
    (agent-repl--rebase-onto-origin-master-callback "ws-one" t "")
    (should (eq (agent-repl-test-commands--origin) :command-rebase))))

(ert-deftest agent-repl-test-commands-create-or-update-pr-origin ()
  "The PR command sends `:command-create-or-update-pr'."
  (agent-repl-test-commands--with
    (cl-letf (((symbol-function 'agent-repl--read-input-buffer) (lambda (_ws) "")))
      (agent-repl-create-or-update-pr)
      (should (eq (agent-repl-test-commands--origin) :command-create-or-update-pr)))))

;;;; ---- Composed text ----

(ert-deftest agent-repl-test-commands-diff-text-names-the-scope ()
  "A diff command's text names WHICH changes it means."
  (agent-repl-test-commands--with
    (agent-repl-explain-diff-staged)
    (should (string-match-p "staged changes" (agent-repl-test-commands--text)))))

(ert-deftest agent-repl-test-commands-diff-text-carries-the-family-prompt ()
  "A diff command's text carries its family's instruction."
  (agent-repl-test-commands--with
    (agent-repl-run-lint-head)
    (should (string-match-p (regexp-quote agent-repl-run-lint-prompt)
                            (agent-repl-test-commands--text)))))

(ert-deftest agent-repl-test-commands-branch-scope-uses-the-branch-spec ()
  "The branch scope resolves to `agent-repl-branch-diff-spec'."
  (agent-repl-test-commands--with
    (agent-repl-explain-diff-branch)
    (should (string-match-p (regexp-quote agent-repl-branch-diff-spec)
                            (agent-repl-test-commands--text)))))

(ert-deftest agent-repl-test-commands-update-pr-diff-uses-its-override ()
  "The update-pr-diff family overrides the standard scope wording."
  (agent-repl-test-commands--with
    (agent-repl-update-pr-diff-worktree)
    (should (string-match-p "Do not consider staged changes"
                            (agent-repl-test-commands--text)))))

(ert-deftest agent-repl-test-commands-explain-text-carries-the-reference ()
  "The explain command names the context it wants explained."
  (agent-repl-test-commands--with
    (cl-letf (((symbol-function 'agent-repl--context-reference) (lambda () "a.el:3-9")))
      (agent-repl-explain)
      (should (string-match-p "a\\.el:3-9" (agent-repl-test-commands--text))))))

(ert-deftest agent-repl-test-commands-explain-prompt-empty-sends-nothing ()
  "An empty answer to the explain prompt submits nothing."
  (agent-repl-test-commands--with
    (cl-letf (((symbol-function 'agent-repl--context-reference) (lambda () "a.el:3"))
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (agent-repl-explain-prompt)
      (should-not agent-repl-test-commands--sent))))

;;;; ---- Every family generates every scope ----

(ert-deftest agent-repl-test-commands-each-family-defines-every-scope ()
  "The macro generates one command per scope for every family.
Asserted as a table rather than one test per command: the claim is about
the GENERATOR, and thirty-five near-identical tests would assert the same
thing thirty-five times."
  (dolist (family '("explain-diff" "update-pr-diff" "run-tests" "run-lint"
                    "run-all" "test-quality" "test-coverage"))
    (dolist (scope '("worktree" "staged" "uncommitted" "head" "branch"))
      (should (fboundp (intern (format "agent-repl-%s-%s" family scope)))))))

;;;; ---- The PR prompt builder ----

(ert-deftest agent-repl-test-commands-pr-prompt-carries-every-base-flag ()
  "With nothing excluded the prompt carries the whole base flag list."
  (let ((prompt (agent-repl--build-create-or-update-pr-prompt nil)))
    (dolist (flag agent-repl-create-or-update-pr-base-flags)
      (should (string-match-p (regexp-quote flag) prompt)))))

(ert-deftest agent-repl-test-commands-pr-prompt-drops-an-excluded-flag ()
  "An exclusion removes exactly its own flag."
  (let ((prompt (agent-repl--build-create-or-update-pr-prompt '(no-self-certified))))
    (should-not (string-match-p "--self-certified" prompt))
    (should (string-match-p "--add-to-merge-queue" prompt))))

(ert-deftest agent-repl-test-commands-pr-prompt-names-the-skill ()
  "The prompt invokes the skill it means."
  (should (string-prefix-p "/create-or-update-pr"
                           (agent-repl--build-create-or-update-pr-prompt nil))))

(ert-deftest agent-repl-test-commands-pr-prompt-refuses-a-malformed-exclusion ()
  "An exclusion symbol not spelled `no-FLAG' is a caller defect."
  (should-error (agent-repl--build-create-or-update-pr-prompt '(self-certified))))

(ert-deftest agent-repl-test-commands-pr-prompt-refuses-an-unknown-exclusion ()
  "An exclusion matching no base flag is a typo that would silently keep it."
  (should-error (agent-repl--build-create-or-update-pr-prompt '(no-nonexistent))))

(ert-deftest agent-repl-test-commands-pr-prefixes-the-composer-text ()
  "The composer's current text is prepended to the PR prompt."
  (agent-repl-test-commands--with
    (cl-letf (((symbol-function 'agent-repl--read-input-buffer)
               (lambda (_ws) "focus on the codec  ")))
      (agent-repl-create-or-update-pr)
      (should (string-prefix-p "focus on the codec /create-or-update-pr"
                               (agent-repl-test-commands--text))))))

(ert-deftest agent-repl-test-commands-pr-paste-inserts-and-sends-nothing ()
  "The paste variant touches no workspace state and contacts nobody."
  (agent-repl-test-commands--with
    (with-temp-buffer
      (agent-repl-create-or-update-pr-paste)
      (should (string-match-p "`/create-or-update-pr" (buffer-string))))
    (should-not agent-repl-test-commands--sent)))

;;;; ---- The rebase fetch gate ----

(ert-deftest agent-repl-test-commands-rebase-fetch-failure-sends-nothing ()
  "A failed fetch skips the dispatch: rebasing onto a stale ref is worse."
  (agent-repl-test-commands--with
    (agent-repl--rebase-onto-origin-master-callback "ws-one" nil "network down")
    (should-not agent-repl-test-commands--sent)))

(ert-deftest agent-repl-test-commands-rebase-runs-fetch-first ()
  "The command fetches origin before asking the agent to rebase."
  (agent-repl-test-commands--with
    (let ((fetched nil))
      (cl-letf (((symbol-function 'agent-repl--ws-dir) (lambda (_ws) "/tmp/wt"))
                ((symbol-function 'agent-repl--async-git)
                 (lambda (_label root args _cb) (setq fetched (list root args)))))
        (agent-repl-rebase-onto-origin-master)
        (should (equal fetched '("/tmp/wt" ("fetch" "origin"))))))))

;;;; ---- File references ----

(ert-deftest agent-repl-test-commands-file-ref-without-a-region ()
  "Without a region the reference is file:line."
  (agent-repl-test--with-clean-state
    (with-temp-buffer
      (insert "one\ntwo\nthree\n")
      (goto-char (point-min))
      (forward-line 1)
      (cl-letf (((symbol-function 'agent-repl--buffer-relative-path) (lambda () "a.el"))
                ((symbol-function 'use-region-p) (lambda () nil)))
        (should (equal (agent-repl--format-file-ref) "a.el:2"))))))

(ert-deftest agent-repl-test-commands-file-ref-with-a-region ()
  "With a region the reference is file:start-end."
  (agent-repl-test--with-clean-state
    (with-temp-buffer
      (insert "one\ntwo\nthree\nfour\n")
      (cl-letf (((symbol-function 'agent-repl--buffer-relative-path) (lambda () "a.el"))
                ((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () (point-min)))
                ((symbol-function 'region-end) (lambda () (point-max)))
                ((symbol-function 'deactivate-mark) (lambda (&rest _) nil)))
        (should (equal (agent-repl--format-file-ref) "a.el:1-5"))))))

(ert-deftest agent-repl-test-commands-relative-path-refuses-a-non-file-buffer ()
  "A buffer visiting no file has no reference to format."
  (agent-repl-test-commands--with
    (with-temp-buffer
      (should-error (agent-repl--buffer-relative-path) :type 'user-error))))

(ert-deftest agent-repl-test-commands-copy-reference-kills-the-ref ()
  "Copying puts the reference on the kill ring."
  (agent-repl-test-commands--with
    (cl-letf (((symbol-function 'agent-repl--format-file-ref) (lambda () "a.el:7")))
      (let ((kill-ring nil))
        (agent-repl-copy-reference)
        (should (equal (current-kill 0) "a.el:7"))))))

(ert-deftest agent-repl-test-commands-copy-workspace-name-kills-the-name ()
  "Copying the workspace name puts it on the kill ring."
  (agent-repl-test-commands--with
    (let ((kill-ring nil))
      (agent-repl-copy-workspace-name)
      (should (equal (current-kill 0) "ws-one")))))

(ert-deftest agent-repl-test-commands-copy-workspace-name-refuses-without-one ()
  "Outside a workspace there is no name to copy."
  (agent-repl-test-commands--with
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () nil)))
      (should-error (agent-repl-copy-workspace-name) :type 'user-error))))

;;;; ---- Opening code through the shared popup ----

(ert-deftest agent-repl-test-commands-link-code-uses-the-shared-popup ()
  "Every open-a-file affordance goes through the ONE popup subroutine.
Divergence between its call sites is a defect by ruling, which is why
this site has no display logic of its own to test."
  (agent-repl-test-commands--with
    (let ((opened nil))
      (cl-letf (((symbol-function 'agent-repl-popup-open)
                 (lambda (path &optional line) (setq opened (list path line)) 'buf)))
        (agent-repl-link-code "/tmp/a.el" 12)
        (should (equal opened '("/tmp/a.el" 12)))))))

(ert-deftest agent-repl-test-commands-link-code-expands-the-path ()
  "The path is expanded, so a caller may pass a relative one."
  (agent-repl-test-commands--with
    (let ((opened nil))
      (cl-letf (((symbol-function 'agent-repl-popup-open)
                 (lambda (path &optional _line) (setq opened path) 'buf)))
        (agent-repl-link-code "a.el" 1)
        (should (file-name-absolute-p opened))))))

(ert-deftest agent-repl-test-commands-link-code-adopts-into-a-workspace ()
  "With a workspace given, the buffer joins that workspace's perspective."
  (agent-repl-test-commands--with
    (let ((adopted nil))
      (cl-letf (((symbol-function 'agent-repl-popup-open) (lambda (&rest _) 'buf))
                ((symbol-function 'agent-repl--ws-resolve-persp) (lambda (_ws) 'persp))
                ((symbol-function 'agent-repl--ws-add-buffer)
                 (lambda (buf persp &optional _nd) (setq adopted (list buf persp)))))
        (agent-repl-link-code "/tmp/a.el" 1 nil "ws-two")
        (should (equal adopted '(buf persp)))))))

;;;; ---- Onboarding a directory ----

(ert-deftest agent-repl-test-commands-add-project-registers-the-dir ()
  "Onboarding a directory REGISTERS it: the daemon mints the identity."
  (agent-repl-test-commands--with
    (let ((registered nil))
      (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
                ((symbol-function 'agent-repl--ws-register-project) (lambda (_d) nil))
                ((symbol-function 'agent-repl-link-primary) (lambda () 'conn))
                ((symbol-function 'agent-repl-host-register)
                 (lambda (conn dir on-done)
                   (setq registered (list conn dir))
                   (funcall on-done '(:id "new-id" :dir "/tmp/proj/"))))
                ((symbol-function 'agent-repl-switch-to-project) (lambda (_p) nil)))
        (agent-repl-add-project-workspace "/tmp/proj")
        (should (equal registered '(conn "/tmp/proj/")))))))

(ert-deftest agent-repl-test-commands-add-project-refuses-a-non-directory ()
  "A path that is not a directory is not a workspace."
  (agent-repl-test-commands--with
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) nil)))
      (should-error (agent-repl-add-project-workspace "/tmp/not-a-dir")
                    :type 'user-error))))

(defmacro agent-repl-test-commands--registering (answer &rest body)
  "Run BODY with `agent-repl-add-project-workspace' answered by ANSWER.
ANSWER is the ref `agent-repl-host-register' hands its callback, or nil
for a register the daemon refused."
  (declare (indent 1))
  `(agent-repl-test--with-clean-state
     (cl-letf (((symbol-function 'file-directory-p) (lambda (_d) t))
               ;; The register's landing waits for the roster to give the
               ;; minted ref a tab, so these tests deliver that arrival
               ;; themselves (`agent-repl-test-commands--tab-arrives').
               ((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil))
               ((symbol-function 'agent-repl--ws-register-project) (lambda (_d) nil))
               ((symbol-function 'agent-repl-link-primary) (lambda () 'conn))
               ((symbol-function 'agent-repl--ws-current-name) (lambda () "elsewhere"))
               ((symbol-function 'agent-repl--ws-current-log-name) (lambda () "elsewhere"))
               ((symbol-function 'run-at-time) (lambda (&rest _) nil))
               ((symbol-function 'message) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl-host-register)
                (lambda (_conn _dir on-done) (funcall on-done ,answer))))
       ,@body)))

(defun agent-repl-test-commands--tab-arrives (id name)
  "Deliver the roster arrival that gives the minted ID a tab called NAME.
A minted ref is the daemon's ANSWER; the workspace itself reaches Emacs
on the roster stream, and the landing waits for it
\(`agent-repl-verbs--pending-landing-fire')."
  (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id)
             (lambda (want) (and (equal want id) name))))
    (agent-repl-verbs--pending-landing-fire)))

(ert-deftest agent-repl-test-commands-add-project-arms-the-registered-panels ()
  "Registering a directory arms the minted workspace to SHOW ITSELF.
The switch waits for the answer, so by the time it runs the directory IS
a workspace and the landing arm is no longer a no-op."
  ;; Arrange
  (agent-repl-test-commands--registering (list :id "new-id" :dir "/tmp/proj/")
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
               (lambda (dir) (and (equal dir "/tmp/proj/") "registered")))
              ((symbol-function 'agent-repl--ws-switch-project) (lambda (_p) nil)))
      ;; Act
      (agent-repl-add-project-workspace "/tmp/proj")
      (agent-repl-test-commands--tab-arrives "new-id" "registered")
      ;; Assert
      (should (agent-repl--ws-get "registered" :pending-show-panels)))))

(ert-deftest agent-repl-test-commands-add-project-arms-nothing-before-the-tab ()
  "A register whose tab has not reached the roster arms NOTHING yet.
The directory is not a workspace until the roster opens its tab, so an
arm attempted here would be the no-op that left the user on magit."
  ;; Arrange
  (agent-repl-test-commands--registering (list :id "new-id" :dir "/tmp/proj/")
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
               (lambda (dir) (and (equal dir "/tmp/proj/") "registered")))
              ((symbol-function 'agent-repl--ws-switch-project) (lambda (_p) nil)))
      ;; Act
      (agent-repl-add-project-workspace "/tmp/proj")
      ;; Assert
      (should-not (agent-repl--ws-get "registered" :pending-show-panels)))))

(ert-deftest agent-repl-test-commands-add-project-lands-on-the-panel-not-magit ()
  "A registered workspace comes up on its own panel, never on magit status.
The recorded switch runs the real landing policy, so a magit status
opened over the new workspace's panel shows up here as a magit call."
  ;; Arrange
  (agent-repl-test-commands--registering (list :id "new-id" :dir "/tmp/proj/")
    (let (magit-dirs)
      (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
                 (lambda (dir) (and (equal dir "/tmp/proj/") "registered")))
                ((symbol-function 'doom-real-buffer-list) (lambda (&optional _b) nil))
                ((symbol-function 'agent-repl--magit-status-same-window)
                 (lambda (dir) (push dir magit-dirs)))
                ((symbol-function 'agent-repl--ws-switch-project)
                 (lambda (dir) (agent-repl--ws-switch-project-display dir))))
        ;; Act
        (agent-repl-add-project-workspace "/tmp/proj")
        (agent-repl-test-commands--tab-arrives "new-id" "registered")
        ;; Assert
        (should-not magit-dirs)))))

(ert-deftest agent-repl-test-commands-add-project-claims-its-arrival-as-registered ()
  "Registering a directory leaves `registered\=' for the tab about to arrive.
Opening a workspace opens its panels, whichever verb opened it (owner
ruling, 2026-09-13, item 6), and the roster row is where that happens."
  ;; Arrange
  (agent-repl-test-commands--registering (list :id "new-id" :dir "/tmp/proj/")
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir)
               (lambda (dir) (and (equal dir "/tmp/proj/") "registered")))
              ((symbol-function 'agent-repl--ws-switch-project) (lambda (_p) nil)))
      ;; Act
      (agent-repl-add-project-workspace "/tmp/proj")
      ;; Assert
      (should (equal (agent-repl--panels-take-arrival-reason "new-id") "registered")))))

(ert-deftest agent-repl-test-commands-add-project-refused-arms-nothing ()
  "A register the daemon REFUSED moves the user nowhere.
There is no workspace to come up on, so nothing is switched to and
nothing is armed."
  ;; Arrange
  (agent-repl-test-commands--registering nil
    (let (switched)
      (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_dir) "registered"))
                ((symbol-function 'agent-repl--ws-switch-project)
                 (lambda (dir) (push dir switched))))
        ;; Act
        (agent-repl-add-project-workspace "/tmp/proj")
        ;; Assert
        (should-not switched)
        (should-not (agent-repl--ws-get "registered" :pending-show-panels))))))

(defun agent-repl-test-commands--registering-phases (answer)
  "Return the progress phases `agent-repl-add-project-workspace' reported.
ANSWER is the ref the register answered with, or nil for a refusal."
  (let (phases)
    (cl-letf (((symbol-function 'agent-repl-workspace-progress-report)
               (lambda (kind phase &rest _) (push (cons kind phase) phases))))
      (agent-repl-test-commands--registering answer
        (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_dir) nil))
                  ((symbol-function 'agent-repl-verbs-select-minted) (lambda (_ref) nil)))
          (agent-repl-add-project-workspace "/tmp/proj"))))
    (nreverse phases)))

(ert-deftest agent-repl-test-commands-add-project-reports-the-request-leaving ()
  "The gesture leaves a mark before the daemon has answered anything."
  ;; Arrange / Act.
  (let ((phases (agent-repl-test-commands--registering-phases
                 '(:id "minted" :dir "/tmp/proj"))))
    ;; Assert.
    (should (equal (car phases) '(:register . :requested)))))

(ert-deftest agent-repl-test-commands-add-project-reports-its-completion ()
  "A register the daemon accepted says so, not only in the log."
  ;; Arrange / Act.
  (let ((phases (agent-repl-test-commands--registering-phases
                 '(:id "minted" :dir "/tmp/proj"))))
    ;; Assert.
    (should (member '(:register . :completed) phases))))

(ert-deftest agent-repl-test-commands-add-project-reports-a-refusal-loudly ()
  "A register that did not land is SAID, not merely logged: a silent refusal
is indistinguishable from a success that did not move the user."
  ;; Arrange / Act.
  (let ((phases (agent-repl-test-commands--registering-phases nil)))
    ;; Assert.
    (should (member '(:register . :failed) phases))))

(ert-deftest agent-repl-test-commands-add-project-claims-no-completion-on-a-refusal ()
  "A refused register never claims the workspace was registered."
  ;; Arrange / Act.
  (let ((phases (agent-repl-test-commands--registering-phases nil)))
    ;; Assert.
    (should-not (member '(:register . :completed) phases))))

;;;; ---- Navigation over roster order ----

(defun agent-repl-test-commands--row (name id at-ms &optional closed)
  "Return a decoded roster row for NAME carrying the last-selected AT-MS."
  (list :workspace (list :workspace (list :id id :dir (concat "/tmp/" name)))
        :attention nil :priority nil
        :last-selected (when at-ms (list :at-ms at-ms))
        :name (list :text name)
        :status (list :arm :ready :value nil)
        :current (list :current nil)
        :children nil
        :when nil
        :detail (list :branch nil :parent-branch nil :summary nil)
        :closed (list :closed (and closed t))))

(defun agent-repl-test-commands--roster (rows)
  "Return a decoded roster carrying ROWS in one repo section."
  (list :repository
        (list :sections (list (list :key (list :repository (list :id "r" :dir "/tmp"))
                                    :header (list :label (list :text "repo"))
                                    :rows (list :rows rows))))
        :task (list :sections nil)
        :recently-merged (list :header (list :label (list :text "m"))
                               :rows (list :rows nil))
        :current nil))

(defmacro agent-repl-test-commands--with-rows (rows history &rest body)
  "Run BODY over a roster of ROWS, with HISTORY as this session's switches.
Each row's name is its workspace, and its durable instant is read from
the row itself, as the roster's own lookup would."
  (declare (indent 2))
  `(let* ((rows ,rows)
          (agent-repl-roster-view (agent-repl-test-commands--roster rows))
          (agent-repl--workspace-history ,history))
     (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id)
                (lambda (id)
                  (cl-loop for row in rows
                           when (equal (plist-get (plist-get (plist-get row :workspace) :workspace) :id) id)
                           return (plist-get (plist-get row :name) :text))))
               ((symbol-function 'agent-repl-roster-last-selected-ms)
                (lambda (ws)
                  (cl-loop for row in rows
                           when (equal (plist-get (plist-get row :name) :text) ws)
                           return (plist-get (plist-get row :last-selected) :at-ms)))))
       ,@body)))

(ert-deftest agent-repl-test-commands-recent-names-order-by-the-durable-instant ()
  "Without session history, recency is the roster's durable last-selected
instant, so a fresh Emacs still knows the order."
  (agent-repl-test-commands--with-rows
      (list (agent-repl-test-commands--row "old" "id-old" 100)
            (agent-repl-test-commands--row "new" "id-new" 900))
      nil
    (should (equal (agent-repl--roster-recent-names) '("new" "old")))))

(ert-deftest agent-repl-test-commands-recent-names-put-session-history-first ()
  "This session's switches come before the durable instant: the same
order a close lands by."
  (agent-repl-test-commands--with-rows
      (list (agent-repl-test-commands--row "old" "id-old" 100)
            (agent-repl-test-commands--row "new" "id-new" 900))
      '("old")
    (should (equal (agent-repl--roster-recent-names) '("old" "new")))))

(ert-deftest agent-repl-test-commands-recent-names-skip-closed-rows ()
  "A closed workspace is not somewhere to switch to."
  (agent-repl-test-commands--with-rows
      (list (agent-repl-test-commands--row "open" "id-open" 100)
            (agent-repl-test-commands--row "shut" "id-shut" 900 t))
      nil
    (should (equal (agent-repl--roster-recent-names) '("open")))))

(ert-deftest agent-repl-test-commands-recent-names-sink-the-never-selected ()
  "A never-selected workspace sorts last rather than being dropped."
  (agent-repl-test-commands--with-rows
      (list (agent-repl-test-commands--row "never" "id-never" nil)
            (agent-repl-test-commands--row "seen" "id-seen" 500))
      nil
    (should (equal (agent-repl--roster-recent-names) '("seen" "never")))))

(ert-deftest agent-repl-test-commands-open-most-recent-skips-the-current ()
  "The command lands somewhere else: switching to where you are is a no-op."
  (let ((agent-repl--opened-recent-cycle nil)
        (switched nil))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "here"))
              ((symbol-function 'agent-repl--roster-recent-names)
               (lambda () '("here" "there")))
              ((symbol-function 'agent-repl--ws-switch) (lambda (ws) (setq switched ws)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl-open-most-recent-workspace)
      (should (equal switched "there")))))

(ert-deftest agent-repl-test-commands-open-most-recent-walks-the-cycle ()
  "Each call lands on a different workspace."
  (let ((agent-repl--opened-recent-cycle nil)
        (visited nil))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "here"))
              ((symbol-function 'agent-repl--roster-recent-names)
               (lambda () '("a" "b")))
              ((symbol-function 'agent-repl--ws-switch)
               (lambda (ws) (push ws visited)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl-open-most-recent-workspace)
      (agent-repl-open-most-recent-workspace)
      (should (equal (reverse visited) '("a" "b"))))))

(ert-deftest agent-repl-test-commands-open-most-recent-resets-when-exhausted ()
  "When every candidate has been visited the cycle resets."
  (let ((agent-repl--opened-recent-cycle '("a"))
        (switched nil))
    (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "here"))
              ((symbol-function 'agent-repl--roster-recent-names) (lambda () '("a")))
              ((symbol-function 'agent-repl--ws-switch) (lambda (ws) (setq switched ws)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (agent-repl-open-most-recent-workspace)
      (should-not switched)
      (should-not agent-repl--opened-recent-cycle))))

;;;; ---- Navigation follows the DRAWN tab order ----

;; The bar is drawn from `agent-repl-roster-tab-order' (roster.el), so that
;; is the list every one of these stubs, and the registry's own
;; `agent-repl--live-ws-names' is stubbed DIFFERENTLY on purpose wherever
;; both are in play: the two disagreeing is exactly the defect these cover.

(defmacro agent-repl-test-commands--with-bar (tabs current &rest body)
  "Run BODY with the tab bar drawing TABS and CURRENT the active workspace.
Binds `agent-repl-test-commands--switched' to the workspace switched to."
  (declare (indent 2))
  `(let ((agent-repl-test-commands--switched nil))
     (cl-letf (((symbol-function 'agent-repl-roster-tab-order) (lambda () ,tabs))
               ((symbol-function 'agent-repl--ws-current-name) (lambda () ,current))
               ((symbol-function 'agent-repl--ws-current-log-name) (lambda () ,current))
               ((symbol-function 'agent-repl--ws-switch)
                (lambda (ws &rest _) (setq agent-repl-test-commands--switched ws)))
               ((symbol-function 'message) (lambda (&rest _) nil)))
       ,@body)))

(defvar agent-repl-test-commands--switched nil
  "The workspace the stubbed switch boundary was handed.")

(ert-deftest agent-repl-test-commands-switch-right-follows-the-drawn-order ()
  "Cycling right lands on the tab drawn to the RIGHT of the current one."
  (agent-repl-test-commands--with-bar '("first" "second" "third") "first"
    (agent-repl-switch-right)
    (should (equal agent-repl-test-commands--switched "second"))))

(ert-deftest agent-repl-test-commands-switch-left-follows-the-drawn-order ()
  "Cycling left lands on the tab drawn to the LEFT of the current one."
  (agent-repl-test-commands--with-bar '("first" "second" "third") "third"
    (agent-repl-switch-left)
    (should (equal agent-repl-test-commands--switched "second"))))

(ert-deftest agent-repl-test-commands-switch-right-wraps-at-the-last-tab ()
  "Cycling right off the last tab wraps to the first."
  (agent-repl-test-commands--with-bar '("first" "second" "third") "third"
    (agent-repl-switch-right)
    (should (equal agent-repl-test-commands--switched "first"))))

(ert-deftest agent-repl-test-commands-switch-left-wraps-at-the-first-tab ()
  "Cycling left off the first tab wraps to the last."
  (agent-repl-test-commands--with-bar '("first" "second" "third") "first"
    (agent-repl-switch-left)
    (should (equal agent-repl-test-commands--switched "third"))))

(ert-deftest agent-repl-test-commands-cycle-never-reaches-a-pseudo-perspective ()
  "persp-mode's own perspectives are in the registry and NOT on the bar.
Cycling walked the registry, so off one end it switched to Doom's `main'
or signaled trying to reach `none' -- a splash screen with no tab
highlighted. The drawn order cannot carry either name."
  (cl-letf (((symbol-function 'agent-repl--live-ws-names)
             (lambda () '("main" "first" "none"))))
    (agent-repl-test-commands--with-bar '("first") "first"
      (agent-repl-switch-left)
      (should (equal agent-repl-test-commands--switched "first")))))

(ert-deftest agent-repl-test-commands-cycle-on-an-empty-bar-is-a-logged-no-op ()
  "With no tabs drawn there is no slot to count from, and no error either."
  (agent-repl-test-commands--with-bar nil "first"
    (agent-repl-switch-right)
    (should-not agent-repl-test-commands--switched)))

(ert-deftest agent-repl-test-commands-cycle-does-nothing-off-the-bar ()
  "A workspace with no tab has no position to cycle from."
  (agent-repl-test-commands--with-bar '("first" "second") "elsewhere"
    (agent-repl-switch-right)
    (should-not agent-repl-test-commands--switched)))

;;;; ---- A switch is DURABLE in the log ----

;; The switch records were emitted on the `agent-repl--log' (debug) rung, and
;; the durable threshold is `info' by default, so every switch the user made
;; left NOTHING on disk: realtest 4 drove four real chords, every one of them
;; switched correctly, and not one of them could be read back afterwards.  A
;; switch is a user action and a lifecycle edge, so it records at `info'.

(defvar agent-repl-test-commands--info nil
  "Records the stubbed `agent-repl--info' rung received, as (WS . MESSAGE).")

(defmacro agent-repl-test-commands--capturing-info (&rest body)
  "Run BODY with the `info' and `debug' logging rungs captured separately."
  (declare (indent 0))
  `(let ((agent-repl-test-commands--info nil)
         (agent-repl-test-commands--debug nil))
     (cl-letf (((symbol-function 'agent-repl--info)
                (lambda (ws fmt &rest args)
                  (push (cons ws (apply #'format fmt args))
                        agent-repl-test-commands--info)))
               ((symbol-function 'agent-repl--log)
                (lambda (ws fmt &rest args)
                  (push (cons ws (apply #'format fmt args))
                        agent-repl-test-commands--debug))))
       ,@body)))

(defvar agent-repl-test-commands--debug nil
  "Records the stubbed `agent-repl--log' (debug) rung received.")

(defun agent-repl-test-commands--info-messages ()
  "The messages the `info' rung received, oldest first."
  (mapcar #'cdr (reverse agent-repl-test-commands--info)))

(defun agent-repl-test-commands--debug-messages ()
  "The messages the debug rung received, oldest first."
  (mapcar #'cdr (reverse agent-repl-test-commands--debug)))

(ert-deftest agent-repl-test-commands-cycle-right-records-its-target ()
  "Cycling right writes `elisp.commands.cycle n=1 target=<ws>'."
  ;; Arrange / Act
  (agent-repl-test-commands--capturing-info
    (agent-repl-test-commands--with-bar '("first" "second" "third") "first"
      (agent-repl-switch-right))
    ;; Assert
    (should (member "elisp.commands.cycle n=1 target=second"
                    (agent-repl-test-commands--info-messages)))))

(ert-deftest agent-repl-test-commands-cycle-left-records-its-target ()
  "Cycling left writes `elisp.commands.cycle n=-1 target=<ws>'."
  ;; Arrange / Act
  (agent-repl-test-commands--capturing-info
    (agent-repl-test-commands--with-bar '("first" "second" "third") "third"
      (agent-repl-switch-left))
    ;; Assert
    (should (member "elisp.commands.cycle n=-1 target=second"
                    (agent-repl-test-commands--info-messages)))))

(ert-deftest agent-repl-test-commands-cycle-is-never-recorded-on-the-debug-rung ()
  "The cycle record must not sit below the default durable threshold.
This is the defect itself: on `agent-repl--log' the record never reached
disk, so a switch the user made could not be read back."
  ;; Arrange / Act
  (agent-repl-test-commands--capturing-info
    (agent-repl-test-commands--with-bar '("first" "second") "first"
      (agent-repl-switch-right))
    ;; Assert
    (should-not (cl-find-if (lambda (m) (string-prefix-p "elisp.commands.cycle " m))
                            (agent-repl-test-commands--debug-messages)))))

(ert-deftest agent-repl-test-commands-cycle-with-no-position-records-the-miss ()
  "A current workspace that is not on the bar records its no-op durably."
  ;; Arrange / Act
  (agent-repl-test-commands--capturing-info
    (agent-repl-test-commands--with-bar '("first" "second") "elsewhere"
      (agent-repl-switch-right))
    ;; Assert
    (should (member "elisp.commands.cycle-no-position n=1 tabs=2"
                    (agent-repl-test-commands--info-messages)))))

(ert-deftest agent-repl-test-commands-cycle-attributes-the-screened-log-name ()
  "The cycle record is attributed through `agent-repl--ws-log-name'.
The current workspace reaches the ladder from persp-mode, so a
placeholder that owns no sink must be screened to nil rather than
routed to a sink that does not exist."
  ;; Arrange / Act
  (agent-repl-test-commands--capturing-info
    (cl-letf (((symbol-function 'agent-repl--ws-log-name) (lambda (_ws) nil)))
      (agent-repl-test-commands--with-bar '("first" "second") "first"
        (agent-repl-switch-right)))
    ;; Assert
    (should (equal '(nil) (delete-dups
                           (mapcar #'car agent-repl-test-commands--info))))))

(ert-deftest agent-repl-test-commands-open-most-recent-records-its-target ()
  "The recent-workspace route records its target durably too."
  ;; Arrange
  (let ((agent-repl--opened-recent-cycle nil))
    ;; Act
    (agent-repl-test-commands--capturing-info
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "here"))
                ((symbol-function 'agent-repl--ws-log-name) (lambda (ws) ws))
                ((symbol-function 'agent-repl--roster-recent-names)
                 (lambda () '("a")))
                ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil)))
        (agent-repl-open-most-recent-workspace))
      ;; Assert
      (should (member "elisp.commands.open-most-recent target=a"
                      (agent-repl-test-commands--info-messages))))))

;;;; ---- The numerals index the drawn bar ----

(ert-deftest agent-repl-test-commands-switch-to-workspace-lands-on-the-nth-tab ()
  "N counts from 1 along the DRAWN order, so 2 is the second tab."
  (agent-repl-test-commands--with-bar '("first" "second" "third") "first"
    (agent-repl-switch-to-workspace 2)
    (should (equal agent-repl-test-commands--switched "second"))))

(ert-deftest agent-repl-test-commands-switch-to-workspace-past-the-end-is-a-no-op ()
  "A slot the bar does not draw is reported, not switched to."
  (agent-repl-test-commands--with-bar '("first" "second") "first"
    (agent-repl-switch-to-workspace 3)
    (should-not agent-repl-test-commands--switched)))

(ert-deftest agent-repl-test-commands-switch-to-workspace-offers-only-drawn-names ()
  "The picker cannot offer a target a chord could not reach."
  (let ((offered nil))
    (cl-letf (((symbol-function 'agent-repl--live-ws-names)
               (lambda () '("main" "first" "second" "none")))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _) (setq offered candidates) "first")))
      (agent-repl-test-commands--with-bar '("first" "second") "first"
        (agent-repl-switch-to-workspace nil)
        (should (equal offered '("first" "second")))))))

(ert-deftest agent-repl-test-commands-last-numeral-lands-on-the-last-tab ()
  "`M-9' on a three-tab bar means the tab at the end of the bar."
  (agent-repl-test-commands--with-bar '("first" "second" "third") "first"
    (agent-repl-switch-to-workspace-9)
    (should (equal agent-repl-test-commands--switched "third"))))

(ert-deftest agent-repl-test-commands-switch-to-workspace-records-its-target ()
  "A numeral chord writes `elisp.commands.switch-to-workspace n=N target=<ws>'."
  ;; Arrange / Act
  (agent-repl-test-commands--capturing-info
    (agent-repl-test-commands--with-bar '("first" "second" "third") "first"
      (agent-repl-switch-to-workspace 2))
    ;; Assert
    (should (member "elisp.commands.switch-to-workspace n=2 target=second"
                    (agent-repl-test-commands--info-messages)))))

(ert-deftest agent-repl-test-commands-switch-to-workspace-records-out-of-range ()
  "A press past the end of the bar records its miss durably."
  ;; Arrange / Act
  (agent-repl-test-commands--capturing-info
    (agent-repl-test-commands--with-bar '("first" "second") "first"
      (agent-repl-switch-to-workspace 3))
    ;; Assert
    (should (member "elisp.commands.switch-to-workspace-out-of-range n=3 tabs=2"
                    (agent-repl-test-commands--info-messages)))))

(ert-deftest agent-repl-test-commands-switch-to-workspace-records-an-empty-bar ()
  "An empty bar records its no-op durably."
  ;; Arrange / Act
  (agent-repl-test-commands--capturing-info
    (agent-repl-test-commands--with-bar nil "first"
      (agent-repl-switch-to-workspace 1))
    ;; Assert
    (should (member "elisp.commands.switch-to-workspace-no-tabs n=1"
                    (agent-repl-test-commands--info-messages)))))

(ert-deftest agent-repl-test-commands-switch-to-workspace-records-the-picker-choice ()
  "The completing-read route records the workspace the user chose."
  ;; Arrange / Act
  (agent-repl-test-commands--capturing-info
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "second")))
      (agent-repl-test-commands--with-bar '("first" "second") "first"
        (agent-repl-switch-to-workspace nil)))
    ;; Assert
    (should (member "elisp.commands.switch-to-workspace-chosen ws=second"
                    (agent-repl-test-commands--info-messages)))))

(ert-deftest agent-repl-test-commands-switch-to-project-offers-no-pseudo-perspectives ()
  "The `SPC p p' switcher never offers \"main\" or \"none\" as a workspace.
Its local-workspace source is `agent-repl--live-ws-names', which excludes
persp-mode's own perspectives at the source, so the candidates are real
workspaces only."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let ((persp-nil-name "none")
          (+workspaces-main "main")
          (offered nil))
      (agent-repl--ws-put "none" :repl-state :inactive)
      (agent-repl--ws-put "main" :agent-state :idle)
      (agent-repl--ws-put "real-ws" :project-dir "/tmp/real")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _)
                   (setq offered candidates)
                   "real-ws"))
                ((symbol-function 'agent-repl--ws-switch) (lambda (_ws) nil))
                ((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil)))
        ;; Act
        (agent-repl-switch-to-project))
      ;; Assert
      (should (equal offered '("real-ws"))))))

(ert-deftest agent-repl-test-commands-switch-to-project-refuses-with-no-workspaces ()
  "With nothing known -- no roster row and nothing live -- there is
nothing to switch to."
  (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () nil))
            ((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil)))
    (should-error (agent-repl-switch-to-project) :type 'user-error)))

;;;; ---- The `SPC p p' switcher offers EVERY KNOWN workspace ----

;; The switcher used to complete over `agent-repl--live-ws-names': the
;; workspaces with a perspective standing in THIS Emacs.  A closed
;; workspace, one the daemon knows that no roster push has been reconciled
;; for yet, and one with no local perspective are all real and were all
;; unreachable.  The daemon's roster is the source of what exists, so it is
;; the source of the candidate list (owner ruling, 2026-09-20).

(defvar agent-repl-test-commands--offered nil
  "Candidate strings the stubbed picker was handed.")

(defvar agent-repl-test-commands--opened nil
  "The `WorkspaceRef' the stubbed `OpenWorkspace' verb was handed.")

(defvar agent-repl-test-commands--landed nil
  "The (REF . LANDER) the stubbed pending landing was registered with.")

(defun agent-repl-test-commands--pick (answer)
  "Return a `completing-read' stub recording its candidates and answering ANSWER."
  (lambda (_prompt candidates &rest _)
    (setq agent-repl-test-commands--offered candidates)
    answer))

(defmacro agent-repl-test-commands--switcher (rows live &rest body)
  "Run BODY with the roster carrying ROWS and LIVE the local live names.
Binds `agent-repl-test-commands--offered' to the candidate strings the
picker was handed, `--opened' to the ref `OpenWorkspace' was given,
`--landed' to the (REF . LANDER) the pending landing was registered with
and `--switched' to the workspace the editor-local switch stood on."
  (declare (indent 2))
  `(let ((agent-repl-roster-view (agent-repl-test-commands--roster ,rows))
         (agent-repl-test-commands--offered nil)
         (agent-repl-test-commands--opened nil)
         (agent-repl-test-commands--landed nil)
         (agent-repl-test-commands--switched nil))
     (cl-letf (((symbol-function 'agent-repl--live-ws-names) (lambda () ,live))
               ((symbol-function 'agent-repl--ws-current-log-name) (lambda () nil))
               ((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--info) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--warn) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--ws-switch)
                (lambda (ws &rest _) (setq agent-repl-test-commands--switched ws)))
               ((symbol-function 'agent-repl-verb-open)
                (lambda (ref) (setq agent-repl-test-commands--opened ref)))
               ((symbol-function 'agent-repl-verbs-select-minted)
                (lambda (ref &optional lander)
                  (setq agent-repl-test-commands--landed (cons ref lander)))))
       ,@body)))

(ert-deftest agent-repl-test-commands-switcher-offers-a-closed-workspace ()
  "A closed workspace is a workspace you can switch to, so it is offered."
  ;; Arrange
  (agent-repl-test-commands--switcher
      (list (agent-repl-test-commands--row "shut" "id-shut" 100 t)) nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil))
              ((symbol-function 'completing-read)
               (agent-repl-test-commands--pick "shut (closed)")))
      ;; Act
      (agent-repl-switch-to-project)
      ;; Assert
      (should (equal agent-repl-test-commands--offered '("shut (closed)"))))))

(ert-deftest agent-repl-test-commands-switcher-offers-a-workspace-with-no-tab-here ()
  "The daemon knows it and this Emacs has not reconciled it: still offered."
  ;; Arrange
  (agent-repl-test-commands--switcher
      (list (agent-repl-test-commands--row "fresh" "id-fresh" 100)) nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil))
              ((symbol-function 'completing-read)
               (agent-repl-test-commands--pick "fresh (not open here)")))
      ;; Act
      (agent-repl-switch-to-project)
      ;; Assert
      (should (equal agent-repl-test-commands--offered '("fresh (not open here)"))))))

(ert-deftest agent-repl-test-commands-switcher-offers-a-local-workspace-off-the-roster ()
  "A live local workspace no roster push carries yet is never dropped."
  ;; Arrange
  (agent-repl-test-commands--switcher nil '("local")
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil))
              ((symbol-function 'agent-repl--ws-get) (lambda (_ws _key) nil))
              ((symbol-function 'completing-read)
               (agent-repl-test-commands--pick "local")))
      ;; Act
      (agent-repl-switch-to-project)
      ;; Assert
      (should (equal agent-repl-test-commands--offered '("local"))))))

(ert-deftest agent-repl-test-commands-switcher-lists-an-open-workspace-once ()
  "A roster row whose tab is standing here is ONE candidate, not two."
  ;; Arrange
  (agent-repl-test-commands--switcher
      (list (agent-repl-test-commands--row "here" "id-here" 100)) '("here")
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) "here"))
              ((symbol-function 'completing-read)
               (agent-repl-test-commands--pick "here")))
      ;; Act
      (agent-repl-switch-to-project)
      ;; Assert
      (should (equal agent-repl-test-commands--offered '("here"))))))

(ert-deftest agent-repl-test-commands-switcher-leaves-an-open-workspace-unaffixed ()
  "An open workspace carries no affix: it is the ordinary case."
  ;; Arrange / Act
  (let ((alist (agent-repl--switch-display-alist
                '((:name "here" :dir "/tmp/here" :ref nil :ws "here" :state :open)))))
    ;; Assert
    (should (equal (mapcar #'car alist) '("here")))))

(ert-deftest agent-repl-test-commands-switcher-qualifies-a-colliding-name ()
  "Names collide across repos, and `completing-read' answers with the string."
  ;; Arrange / Act
  (let ((alist (agent-repl--switch-display-alist
                '((:name "api" :dir "/a/api" :ref nil :ws "api" :state :open)
                  (:name "api" :dir "/b/api" :ref nil :ws nil :state :closed)))))
    ;; Assert
    (should (equal (mapcar #'car alist) '("api [/a/api]" "api [/b/api] (closed)")))))

(ert-deftest agent-repl-test-commands-switcher-switches-to-an-open-workspace ()
  "An open workspace is an editor-local switch and nothing more."
  ;; Arrange
  (agent-repl-test-commands--switcher
      (list (agent-repl-test-commands--row "here" "id-here" 100)) nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) "here"))
              ((symbol-function 'completing-read)
               (agent-repl-test-commands--pick "here")))
      ;; Act
      (agent-repl-switch-to-project)
      ;; Assert
      (should (equal agent-repl-test-commands--switched "here")))))

(ert-deftest agent-repl-test-commands-switcher-never-opens-a-workspace-already-here ()
  "Switching to a standing tab must cost no daemon round trip at all."
  ;; Arrange
  (agent-repl-test-commands--switcher
      (list (agent-repl-test-commands--row "here" "id-here" 100)) nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) "here"))
              ((symbol-function 'completing-read)
               (agent-repl-test-commands--pick "here")))
      ;; Act
      (agent-repl-switch-to-project)
      ;; Assert
      (should-not agent-repl-test-commands--opened))))

(ert-deftest agent-repl-test-commands-switcher-opens-a-workspace-with-no-tab ()
  "A workspace that is not here is reopened through `OpenWorkspace' itself."
  ;; Arrange
  (agent-repl-test-commands--switcher
      (list (agent-repl-test-commands--row "shut" "id-shut" 100 t)) nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil))
              ((symbol-function 'completing-read)
               (agent-repl-test-commands--pick "shut (closed)")))
      ;; Act
      (agent-repl-switch-to-project)
      ;; Assert
      (should (equal (plist-get agent-repl-test-commands--opened :id) "id-shut")))))

(ert-deftest agent-repl-test-commands-switcher-lands-on-the-reopened-tab-by-identity ()
  "The tab arrives on the roster push, so the landing waits for it BY REF."
  ;; Arrange
  (agent-repl-test-commands--switcher
      (list (agent-repl-test-commands--row "shut" "id-shut" 100 t)) nil
    (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil))
              ((symbol-function 'completing-read)
               (agent-repl-test-commands--pick "shut (closed)")))
      ;; Act
      (agent-repl-switch-to-project)
      ;; Assert
      (should (equal (plist-get (car agent-repl-test-commands--landed) :id) "id-shut"))
      (should (eq (cdr agent-repl-test-commands--landed)
                  #'agent-repl-verbs--land-on-tab)))))

(ert-deftest agent-repl-test-commands-switcher-refuses-a-candidate-with-no-ref ()
  "The ref is daemon-minted: a candidate without one cannot be opened."
  ;; Arrange
  (agent-repl-test-commands--switcher nil nil
    (cl-letf (((symbol-function 'agent-repl--switch-candidates)
               (lambda () '((:name "orphan" :dir nil :ref nil :ws nil :state :closed))))
              ((symbol-function 'completing-read)
               (agent-repl-test-commands--pick "orphan (closed)")))
      ;; Act / Assert
      (should-error (agent-repl-switch-to-project) :type 'user-error))))

;;;; ---- A project switch lands on the workspace's own panel ----

(ert-deftest agent-repl-test-commands-switch-to-project-arms-the-targets-panels ()
  "A switch to a workspace's directory arms that workspace to SHOW ITSELF.
The gui pre-creates a page without displaying it, so the view arrives
only through the `:pending-show-panels' drain -- and `SPC TAB n', which
stands on the workspace it just made by switching to the minted
worktree, otherwise landed the user on an empty frame."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_dir) "made"))
              ((symbol-function 'agent-repl--ws-current-name) (lambda () "elsewhere"))
              ((symbol-function 'agent-repl--ws-current-log-name) (lambda () "elsewhere"))
              ((symbol-function 'agent-repl--ws-switch-project) (lambda (_p) nil)))
      ;; Act
      (agent-repl--switch-project-arm-panels "/tmp/made/")
      ;; Assert
      (should (agent-repl--ws-get "made" :pending-show-panels)))))

(ert-deftest agent-repl-test-commands-switch-to-project-arms-before-it-switches ()
  "The arming happens BEFORE the switch, or the persp activation hook
schedules its drain and finds nothing set."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (let (armed-at-switch)
      (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_dir) "made"))
                ((symbol-function 'agent-repl--ws-current-name) (lambda () "elsewhere"))
                ((symbol-function 'agent-repl--ws-current-log-name) (lambda () "elsewhere"))
                ((symbol-function 'run-at-time) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--ws-switch-project)
                 (lambda (_p)
                   (setq armed-at-switch
                         (agent-repl--ws-get "made" :pending-show-panels)))))
        ;; Act
        (agent-repl-switch-to-project "/tmp/made/")
        ;; Assert
        (should armed-at-switch)))))

(ert-deftest agent-repl-test-commands-switch-to-project-logs-the-target-workspace ()
  "The explicit project target wins over the currently selected workspace."
  ;; Arrange
  (let (scopes)
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_dir) "target"))
              ((symbol-function 'agent-repl--ws-current-name) (lambda () "buffer-owner"))
              ((symbol-function 'agent-repl--log)
               (lambda (ws _fmt &rest _args) (push ws scopes)))
              ((symbol-function 'agent-repl--switch-project-arm-panels) #'ignore)
              ((symbol-function 'agent-repl--ws-switch-project) #'ignore)
              ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      ;; Act
      (agent-repl-switch-to-project "/tmp/target/")
      ;; Assert
      (should (equal scopes '("target"))))))

(ert-deftest agent-repl-test-commands-switch-to-project-arms-nothing-off-a-workspace ()
  "A plain project directory has no panel to show, so nothing is armed."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_dir) nil))
              ((symbol-function 'agent-repl--ws-current-name) (lambda () "elsewhere"))
              ((symbol-function 'agent-repl--ws-current-log-name) (lambda () "elsewhere")))
      ;; Act
      (agent-repl--switch-project-arm-panels "/tmp/plain/")
      ;; Assert
      (should-not (agent-repl--ws-get "elsewhere" :pending-show-panels)))))

(ert-deftest agent-repl-test-commands-switch-to-project-arms-nothing-when-already-there ()
  "Switching to the workspace you are ALREADY standing in activates no
perspective, so no drain is coming: a flag left standing would re-show
panels the user had dismissed the next time they arrived."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_dir) "mine"))
              ((symbol-function 'agent-repl--ws-current-name) (lambda () "mine"))
              ((symbol-function 'agent-repl--ws-current-log-name) (lambda () "mine")))
      ;; Act
      (agent-repl--switch-project-arm-panels "/tmp/mine/")
      ;; Assert
      (should-not (agent-repl--ws-get "mine" :pending-show-panels)))))

;;;; ---- The most-recent-file cache ----

(ert-deftest agent-repl-test-commands-most-recent-file-prefers-the-cache ()
  "The warm path answers from the plist without scanning recentf."
  (agent-repl-test--with-clean-state
    (let ((file (make-temp-file "agent-repl-recent")))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_d) "ws1")))
            (agent-repl--ws-put "ws1" :last-file file)
            (should (equal (agent-repl--most-recent-project-file "/tmp") file)))
        (delete-file file)))))

(ert-deftest agent-repl-test-commands-most-recent-file-ignores-a-deleted-cache ()
  "Both sources lag deletions, so the cached path is verified before use."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ws-name-for-dir) (lambda (_d) "ws1")))
      (agent-repl--ws-put "ws1" :last-file "/tmp/gone-12345.el")
      (let ((recentf-list nil))
        (should-not (agent-repl--most-recent-project-file "/tmp"))))))

(ert-deftest agent-repl-test-commands-most-recent-file-nil-without-a-root ()
  "No project root, no answer."
  (should-not (agent-repl--most-recent-project-file nil)))

;;;; ---- What is deliberately gone ----

(ert-deftest agent-repl-test-commands-defines-no-snapshot-machinery ()
  "The durable Emacs workspace-roster snapshot is gone, whole.
Asserted rather than merely deleted: THE DAEMON is the source of which
workspaces exist, and a re-introduced Emacs-side roster on disk could
only ever disagree with the pushed one."
  (dolist (sym '(agent-repl-save-workspace-snapshot
                 agent-repl-load-workspace-snapshot
                 agent-repl-update-workspace-snapshot
                 agent-repl--establish-workspace
                 agent-repl--read-workspace-via-picker
                 agent-repl--hydrate-and-reorder-on-open))
    (should-not (fboundp sym))))

(ert-deftest agent-repl-test-commands-defines-no-tab-ordering ()
  "Client-authored tab ordering and hiding are gone: tabs follow the roster.
`agent-repl-workspace-push-to-back' is deliberately NOT in this list any
more: the deprio close (`SPC o C') calls it, so it is live production
API.  It lives in panels.el beside its caller and shuffles the ROSTER's
order, which is why commands.el still defines no tab ordering of its own."
  (dolist (sym '(agent-repl-workspace-pull-to-front
                 agent-repl-workspace-switch-to-0
                 agent-repl-workspace-switch-to-final))
    (should-not (fboundp sym))))

(ert-deftest agent-repl-test-commands-defines-no-interrupt ()
  "No interrupt verb exists in the contract; the footer owns that gesture."
  (dolist (sym '(agent-repl-interrupt
                 agent-repl--interrupt-turn
                 agent-repl--interrupt-detached-agents
                 agent-repl-paste-clipboard))
    (should-not (fboundp sym))))

(provide 'test-commands)

;;; test-commands.el ends here
