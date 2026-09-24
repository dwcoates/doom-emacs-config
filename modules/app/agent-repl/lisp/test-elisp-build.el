;;; test-elisp-build.el --- ERT tests for elisp-build.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -batch -Q -l ert -l lisp/test-elisp-build.el -f ert-run-tests-batch-and-exit
;;
;; The elisp build is a CROSS-LANGUAGE contract: `proto/vocab/elisp-build.json'
;; is the vector the daemon's Go side asserts too, so both the pure build
;; function and the per-file hashing are held to every case in it.  The
;; pushed reload is driven with its external boundary (the `load') and the
;; heartbeat assertion stubbed, against a module root built in a temp dir.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

(defconst agent-repl-test-eb--vector-file
  (expand-file-name "../proto/vocab/elisp-build.json"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The cross-language elisp-build vector.")

(defun agent-repl-test-eb--cases ()
  "Return the vector's cases, each an alist with `name', `modules' and `want'."
  (alist-get 'cases
             (json-parse-string
              (with-temp-buffer
                (insert-file-contents agent-repl-test-eb--vector-file)
                (buffer-string))
              :object-type 'alist :array-type 'list :null-object nil)))

(defun agent-repl-test-eb--utf8-sha256 (content)
  "Return the SHA-256 of CONTENT's UTF-8 bytes."
  (secure-hash 'sha256 (encode-coding-string content 'utf-8)))

(defun agent-repl-test-eb--write-bytes (file content)
  "Write CONTENT to FILE as its UTF-8 bytes, creating its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (encode-coding-string content 'utf-8))
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) file nil 'silent))))

(defmacro agent-repl-test-eb--with-root (var &rest body)
  "Bind VAR to a fresh temp module root for BODY, deleted afterwards."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "agent-repl-eb-root" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun agent-repl-test-eb--write-root (root modules &optional absent)
  "Write a module root at ROOT: config.el naming MODULES, and their sources.
Every module in ABSENT is named by config.el but gets no file."
  (agent-repl-test-eb--write-bytes
   (expand-file-name "config.el" root)
   (concat ";;; config.el\n"
           (mapconcat (lambda (m) (format "(agent-repl--load-module \"%s\")\n" m))
                      modules "")))
  (dolist (m modules)
    (unless (member m absent)
      (agent-repl-test-eb--write-bytes
       (expand-file-name (format "lisp/%s.el" m) root)
       (format ";;; %s.el\n" m)))))

;;;; ---- Harness for the logger and the reload's boundaries ----

(defvar agent-repl-test-eb--logs nil
  "Log records captured by the harness, newest first: (LEVEL . TEXT).")

(defvar agent-repl-test-eb--messages nil
  "Echo-area messages captured by the harness, newest first.")

(defvar agent-repl-test-eb--loaded nil
  "Files the stubbed load boundary was asked to load, newest first.")

(defvar agent-repl-test-eb--heartbeat nil
  "What the stubbed heartbeat assertion answers.")

(defvar agent-repl-test-eb--heartbeat-ran nil
  "Non-nil once the stubbed heartbeat assertion ran.")

(defvar agent-repl-test-eb--timers nil
  "`run-at-time' calls captured by the harness, newest first: (SECS FN ARGS).")

(defvar agent-repl-test-eb--failing-loads nil
  "Base names whose load the stubbed boundary fails.")

(defun agent-repl-test-eb--logged-p (level substring)
  "Return non-nil when a LEVEL record containing SUBSTRING was captured."
  (and (seq-some (lambda (entry)
                   (and (eq (car entry) level) (string-search substring (cdr entry))))
                 agent-repl-test-eb--logs)
       t))

(defmacro agent-repl-test-eb--with-harness (&rest body)
  "Run BODY with the logger, `message', the load and the assertion stubbed."
  (declare (indent 0))
  `(let ((agent-repl-test-eb--logs nil)
         (agent-repl-test-eb--messages nil)
         (agent-repl-test-eb--loaded nil)
         (agent-repl-test-eb--heartbeat '(:armed (a) :rearmed nil :failed nil :unavailable nil))
         (agent-repl-test-eb--heartbeat-ran nil)
         (agent-repl-test-eb--timers nil)
         (agent-repl-test-eb--failing-loads nil)
         (agent-repl--elisp-module-builds agent-repl--elisp-module-builds))
     (cl-letf (((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args)
                  (push (cons :log (apply #'format fmt args)) agent-repl-test-eb--logs)))
               ((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args)
                  (push (cons :info (apply #'format fmt args)) agent-repl-test-eb--logs)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args)
                  (push (cons :error (apply #'format fmt args)) agent-repl-test-eb--logs)))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) agent-repl-test-eb--messages)))
               ((symbol-function 'run-at-time)
                (lambda (secs _repeat fn &rest args)
                  (push (list secs fn args) agent-repl-test-eb--timers)
                  (timer-create)))
               ((symbol-function 'agent-repl--elisp-reload-load-file)
                (lambda (file)
                  (push file agent-repl-test-eb--loaded)
                  (when (member (file-name-base file) agent-repl-test-eb--failing-loads)
                    (error "Boom in %s" (file-name-base file)))))
               ((symbol-function 'agent-repl--assert-heartbeat-armed)
                (lambda ()
                  (setq agent-repl-test-eb--heartbeat-ran t)
                  agent-repl-test-eb--heartbeat)))
       ,@body)))

;;;; ---- The build: the cross-language vector ----

(ert-deftest agent-repl-test-eb-vector-pure-build ()
  "The pure build reproduces every case of the vector."
  (dolist (case (agent-repl-test-eb--cases))
    ;; Arrange
    (let ((entries (delq nil
                         (mapcar (lambda (m)
                                   (let ((content (alist-get 'content m)))
                                     (and content
                                          (cons (alist-get 'name m)
                                                (agent-repl-test-eb--utf8-sha256 content)))))
                                 (alist-get 'modules case)))))
      ;; Act / Assert
      (should (equal (list (alist-get 'name case)
                           (agent-repl-elisp-build-of entries))
                     (list (alist-get 'name case) (alist-get 'want case)))))))

(ert-deftest agent-repl-test-eb-vector-per-file-hashing ()
  "Hashing each case's files, written as UTF-8 bytes, reproduces the vector."
  (dolist (case (agent-repl-test-eb--cases))
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (let ((names (mapcar (lambda (m) (alist-get 'name m)) (alist-get 'modules case))))
        (dolist (m (alist-get 'modules case))
          (when (alist-get 'content m)
            (agent-repl-test-eb--write-bytes
             (expand-file-name (format "lisp/%s.el" (alist-get 'name m)) root)
             (alist-get 'content m))))
        ;; Act
        (let ((build (agent-repl-elisp-build-of
                      (agent-repl-elisp-build-entries root names))))
          ;; Assert
          (should (equal (list (alist-get 'name case) build)
                         (list (alist-get 'name case) (alist-get 'want case)))))))))

(ert-deftest agent-repl-test-eb-vector-is-not-empty ()
  "The vector the two suites above iterate actually carries cases."
  ;; Arrange / Act / Assert
  (should (>= (length (agent-repl-test-eb--cases)) 5)))

;;;; ---- The current build ----

(ert-deftest agent-repl-test-eb-current-build-is-the-recorded-modules-build ()
  "The running Emacs's build is the build of what config.el recorded."
  (agent-repl-test-eb--with-harness
    ;; Arrange
    (setq agent-repl--elisp-module-builds '(("core" . "aa") ("status" . "bb")))
    ;; Act / Assert
    (should (equal (agent-repl-elisp-build)
                   (agent-repl-elisp-build-of '(("core" . "aa") ("status" . "bb")))))))

(ert-deftest agent-repl-test-eb-current-build-refuses-an-empty-record ()
  "No recorded module is a loud failure, never an empty build."
  (agent-repl-test-eb--with-harness
    ;; Arrange
    (setq agent-repl--elisp-module-builds nil)
    ;; Act / Assert
    (should-error (agent-repl-elisp-build))))

(ert-deftest agent-repl-test-eb-harness-records-the-loaded-module-set ()
  "The batch load of config.el recorded one entry per module source, core first."
  ;; Arrange / Act / Assert
  (should (equal (caar agent-repl--elisp-module-builds) "core")))

;;;; ---- config.el's module list ----

(ert-deftest agent-repl-test-eb-config-modules-in-load-order ()
  "The module list is config.el's top-level load forms, in order."
  (agent-repl-test-eb--with-root root
    ;; Arrange
    (agent-repl-test-eb--write-root root '("core" "wire-common" "status"))
    ;; Act / Assert
    (should (equal (agent-repl--elisp-config-modules (expand-file-name "config.el" root))
                   '("core" "wire-common" "status")))))

(ert-deftest agent-repl-test-eb-config-modules-skip-indented-mentions ()
  "Only top-level lines count: an indented or commented mention is no module."
  (agent-repl-test-eb--with-root root
    ;; Arrange
    (agent-repl-test-eb--write-bytes
     (expand-file-name "config.el" root)
     (concat "(agent-repl--load-module \"core\")\n"
             "  (agent-repl--load-module \"nested\")\n"
             ";; (agent-repl--load-module \"commented\")\n"))
    ;; Act / Assert
    (should (equal (agent-repl--elisp-config-modules (expand-file-name "config.el" root))
                   '("core")))))

(ert-deftest agent-repl-test-eb-config-modules-match-the-real-loader ()
  "The reader finds exactly the modules the batch load of config.el recorded."
  ;; Arrange
  (let ((config (expand-file-name "../../config.el"
                                  (file-name-directory agent-repl-test-eb--vector-file))))
    ;; Act / Assert
    (should (equal (agent-repl--elisp-config-modules config)
                   (mapcar #'car agent-repl--elisp-module-builds)))))

;;;; ---- The pushed reload: the root check ----

(ert-deftest agent-repl-test-eb-reload-refused-on-a-root-mismatch ()
  "A reload naming another checkout is refused at ERROR with both roots."
  (agent-repl-test-eb--with-harness
    ;; Arrange
    (let ((agent-repl--frontend-root "/ours/agent-repl/"))
      ;; Act
      (agent-repl-elisp-reload-handle '(:module-root "/theirs/agent-repl" :build "b1"))
      ;; Assert
      (should (agent-repl-test-eb--logged-p
               :error "reload-refused reason=root-mismatch pushed-root=\"/theirs/agent-repl/\" running-root=\"/ours/agent-repl/\"")))))

(ert-deftest agent-repl-test-eb-reload-refusal-tells-the-user ()
  "A refused reload is a user-visible message naming both roots."
  (agent-repl-test-eb--with-harness
    ;; Arrange
    (let ((agent-repl--frontend-root "/ours/agent-repl/"))
      ;; Act
      (agent-repl-elisp-reload-handle '(:module-root "/theirs/agent-repl" :build "b1"))
      ;; Assert
      (should (equal agent-repl-test-eb--messages
                     '("agent-repl: elisp reload REFUSED -- the deploy is for /theirs/agent-repl/, this Emacs runs /ours/agent-repl/"))))))

(ert-deftest agent-repl-test-eb-reload-refusal-loads-nothing ()
  "A refused reload schedules nothing and loads nothing."
  (agent-repl-test-eb--with-harness
    ;; Arrange
    (let ((agent-repl--frontend-root "/ours/agent-repl/"))
      ;; Act
      (agent-repl-elisp-reload-handle '(:module-root "/theirs/agent-repl" :build "b1"))
      ;; Assert
      (should (equal (list agent-repl-test-eb--timers agent-repl-test-eb--loaded)
                     '(nil nil))))))

(ert-deftest agent-repl-test-eb-reload-root-compared-as-directory-names ()
  "The same root spelled without its trailing slash is still this Emacs's."
  (agent-repl-test-eb--with-harness
    ;; Arrange
    (let ((agent-repl--frontend-root "/ours/agent-repl/"))
      ;; Act / Assert
      (should (agent-repl-elisp-reload-handle
               '(:module-root "/ours/agent-repl" :build "b1"))))))

(ert-deftest agent-repl-test-eb-reload-is-scheduled-out-of-the-filter ()
  "A reload for this root runs from a zero-delay timer, never in the filter."
  (agent-repl-test-eb--with-harness
    ;; Arrange
    (let ((agent-repl--frontend-root "/ours/agent-repl/"))
      ;; Act
      (agent-repl-elisp-reload-handle '(:module-root "/ours/agent-repl/" :build "b1"))
      ;; Assert
      (should (equal agent-repl-test-eb--timers
                     '((0 agent-repl--elisp-reload-run ("/ours/agent-repl/" "b1")))))
      (should (null agent-repl-test-eb--loaded)))))

;;;; ---- The pushed reload: the load ----

(ert-deftest agent-repl-test-eb-reload-loads-the-module-set-in-config-order ()
  "Every module config.el names is loaded from ROOT, in config.el's order."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core" "status" "autosave"))
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (equal (reverse agent-repl-test-eb--loaded)
                     (mapcar (lambda (m) (expand-file-name (format "lisp/%s.el" m) root))
                             '("core" "status" "autosave")))))))

(ert-deftest agent-repl-test-eb-reload-runs-the-heartbeat-assertion ()
  "The heartbeat assertion runs after the load."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core"))
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should agent-repl-test-eb--heartbeat-ran))))

(ert-deftest agent-repl-test-eb-reload-logs-the-heartbeat-counts ()
  "The assertion's armed/rearmed/failed/unavailable counts are logged."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core"))
      (setq agent-repl-test-eb--heartbeat '(:armed (a b) :rearmed nil :failed nil :unavailable nil))
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (agent-repl-test-eb--logged-p
               :info "reload-heartbeat armed=2 rearmed=0 failed=0 unavailable=0")))))

(ert-deftest agent-repl-test-eb-reload-failed-timers-are-an-error ()
  "A timer the assertion could not re-arm is an ERROR naming it."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core"))
      (setq agent-repl-test-eb--heartbeat '(:armed nil :rearmed nil :failed (status) :unavailable nil))
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (agent-repl-test-eb--logged-p :error "reload-heartbeat-failed keys=(status)")))))

(ert-deftest agent-repl-test-eb-reload-failed-timers-tell-the-user ()
  "A timer the assertion could not re-arm is a user-visible message."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core"))
      (setq agent-repl-test-eb--heartbeat '(:armed nil :rearmed nil :failed (status) :unavailable nil))
      ;; Act
      (agent-repl--elisp-reload-run root (agent-repl-elisp-build-of
                                          (agent-repl-elisp-build-entries root '("core"))))
      ;; Assert
      (should (equal agent-repl-test-eb--messages
                     '("agent-repl: elisp reload left required timers unarmed: status"))))))

(ert-deftest agent-repl-test-eb-reload-rearmed-timers-are-named-at-info ()
  "A timer the assertion re-armed is an INFO naming it, not an error."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core"))
      (setq agent-repl-test-eb--heartbeat '(:armed nil :rearmed (heartbeat) :failed nil :unavailable nil))
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (agent-repl-test-eb--logged-p :info "reload-heartbeat-rearmed keys=(heartbeat)")))))

(ert-deftest agent-repl-test-eb-reload-failing-module-is-an-error ()
  "A module whose load signals is logged at ERROR with its error."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core" "status" "autosave"))
      (setq agent-repl-test-eb--failing-loads '("status"))
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (agent-repl-test-eb--logged-p :error "reload-module-failed module=status")))))

(ert-deftest agent-repl-test-eb-reload-continues-past-a-failing-module ()
  "The modules after a failing one still load, and the assertion still runs."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core" "status" "autosave"))
      (setq agent-repl-test-eb--failing-loads '("status"))
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (equal (list (mapcar #'file-name-base (reverse agent-repl-test-eb--loaded))
                           agent-repl-test-eb--heartbeat-ran)
                     '(("core" "status" "autosave") t))))))

(ert-deftest agent-repl-test-eb-reload-reports-the-failures-together ()
  "Every failed module is reported in one ERROR and one user-visible line."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core" "status" "autosave"))
      (setq agent-repl-test-eb--failing-loads '("core" "autosave"))
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (agent-repl-test-eb--logged-p :error "reload-module-failures count=2"))
      (should (member "agent-repl: elisp reload: 2 module(s) failed to load: core, autosave"
                      agent-repl-test-eb--messages)))))

(ert-deftest agent-repl-test-eb-reload-absent-module-is-a-failure ()
  "A module config.el names without a file is reported and not loaded."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core" "ghost") '("ghost"))
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (equal (list (mapcar #'file-name-base agent-repl-test-eb--loaded)
                           (agent-repl-test-eb--logged-p :error "reload-module-absent module=ghost"))
                     (list '("core") t))))))

(ert-deftest agent-repl-test-eb-reload-never-loads-a-test-file ()
  "A `test-' module is refused at ERROR and never loaded."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core" "test-helpers"))
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (equal (list (mapcar #'file-name-base agent-repl-test-eb--loaded)
                           (agent-repl-test-eb--logged-p
                            :error "reload-test-module-refused module=test-helpers"))
                     (list '("core") t))))))

(ert-deftest agent-repl-test-eb-reload-unreadable-config-loads-nothing ()
  "A root with no readable config.el is an ERROR and loads nothing."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange: no config.el is written.
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (equal (list agent-repl-test-eb--loaded
                           (agent-repl-test-eb--logged-p :error "reload-config-unreadable"))
                     '(nil t))))))

(ert-deftest agent-repl-test-eb-reload-config-naming-no-module-is-an-error ()
  "A config.el naming no module is an ERROR, not a silent no-op."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '())
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (agent-repl-test-eb--logged-p :error "reload-config-names-no-module")))))

;;;; ---- The pushed reload: the recorded build ----

(ert-deftest agent-repl-test-eb-reload-recomputes-the-recorded-builds ()
  "After the reload the recorded builds are those of the files just loaded."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core" "status"))
      ;; Act
      (agent-repl--elisp-reload-run root "any")
      ;; Assert
      (should (equal agent-repl--elisp-module-builds
                     (agent-repl-elisp-build-entries root '("core" "status")))))))

(ert-deftest agent-repl-test-eb-reload-matching-build-is-no-error ()
  "A loaded build equal to the pushed one completes without an ERROR."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core" "status"))
      (let ((build (agent-repl-elisp-build-of
                    (agent-repl-elisp-build-entries root '("core" "status")))))
        ;; Act
        (agent-repl--elisp-reload-run root build)
        ;; Assert
        (should-not (seq-some (lambda (e) (eq (car e) :error)) agent-repl-test-eb--logs))))))

(ert-deftest agent-repl-test-eb-reload-build-mismatch-is-an-error ()
  "A loaded build other than the pushed one means the files moved: ERROR."
  (agent-repl-test-eb--with-harness
    (agent-repl-test-eb--with-root root
      ;; Arrange
      (agent-repl-test-eb--write-root root '("core"))
      ;; Act
      (agent-repl--elisp-reload-run root "the-deploy-built-something-else")
      ;; Assert
      (should (agent-repl-test-eb--logged-p
               :error "reload-build-mismatch pushed=\"the-deploy-built-something-else\"")))))

(provide 'test-elisp-build)

;;; test-elisp-build.el ends here
