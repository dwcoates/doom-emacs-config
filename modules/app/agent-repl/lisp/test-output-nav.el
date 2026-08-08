;;; test-output-nav.el --- ERT tests for output-nav.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for cycling the output feed from the input buffer.  Batch Emacs
;; has no xwidget support, so the script boundary
;; (`agent-repl--frontend-webview-execute-script') is mocked and the
;; assertions are about WHAT script each command builds and WHERE it is
;; sent -- the cycling itself is the webapp's (webapp/test/nav.test.ts).
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-output-nav.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Helpers -----------------------------------------------------------

(defmacro agent-repl-test--with-nav-ws (plist &rest body)
  "Run BODY with workspace \"navws\" registered carrying PLIST and current."
  (declare (indent 1))
  `(unwind-protect
       (progn
         (puthash "navws" (copy-sequence ,plist) agent-repl--workspaces)
         (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                    (lambda () "navws")))
           ,@body))
     (remhash "navws" agent-repl--workspaces)))

(defmacro agent-repl-test--capturing-nav-scripts (calls &rest body)
  "Run BODY with the webview script boundary mocked, collecting into CALLS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
              (lambda (b script) (push (cons b script) ,calls))))
     ,@body))

;;;; ---- The emitted script ------------------------------------------------

(ert-deftest agent-repl-test-output-nav-script-carries-class-and-forward-step ()
  "A next-cycle script names its class and steps the webapp forward."
  ;; Act
  (let ((script (agent-repl--output-nav-script "prompt" "next")))
    ;; Assert
    (should (string-match-p "agentReplNavigate('prompt', 1)" script))))

(ert-deftest agent-repl-test-output-nav-script-carries-backward-step ()
  "A prev-cycle script steps the webapp backward."
  ;; Act
  (let ((script (agent-repl--output-nav-script "prompt" "prev")))
    ;; Assert
    (should (string-match-p "agentReplNavigate('prompt', -1)" script))))

(ert-deftest agent-repl-test-output-nav-script-guards-on-the-hook ()
  "The script calls the hook only once the page has planted it.
A webview still navigating has none, which is expected rather than a
violated invariant."
  ;; Act
  (let ((script (agent-repl--output-nav-script "final" "next")))
    ;; Assert
    (should (string-prefix-p "window.agentReplNavigate && " script))))

(ert-deftest agent-repl-test-output-nav-script-rejects-an-unknown-class ()
  "A class the webapp cannot resolve is a bug here, so it raises."
  ;; Act / Assert
  (should-error (agent-repl--output-nav-script "thinking" "next")))

(ert-deftest agent-repl-test-output-nav-script-logs-an-invalid-class-and-direction ()
  "Invalid input records the rejected class, direction, and outcome."
  ;; Arrange
  (let ((logs nil))
    (cl-letf (((symbol-function 'agent-repl--log)
               (lambda (&rest args) (push args logs))))
      ;; Act / Assert
      (should-error (agent-repl--output-nav-script "thinking" "next" "navws")))
    (should (equal logs
                   (list (list "navws"
                               "output-nav-script: class=%S direction=%S outcome=invalid-class"
                               "thinking" "next"))))))

(ert-deftest agent-repl-test-output-nav-script-rejects-an-unknown-direction ()
  "A direction with no webapp step is a bug here, so it raises."
  ;; Act / Assert
  (should-error (agent-repl--output-nav-script "prompt" "sideways")))

;;;; ---- Dispatch ----------------------------------------------------------

(ert-deftest agent-repl-test-output-nav-targets-the-workspace-webview ()
  "The cycle script is evaluated in the current workspace's webview buffer."
  ;; Arrange
  (let ((buf (generate-new-buffer " *nav-webview*"))
        (calls nil))
    (unwind-protect
        (agent-repl-test--with-nav-ws (list :frontend-buffer buf)
          (agent-repl-test--capturing-nav-scripts calls
            ;; Act
            (agent-repl-output-next-prompt)
            ;; Assert
            (should (equal calls
                           (list (cons buf (agent-repl--output-nav-script
                                            "prompt" "next")))))))
      (kill-buffer buf))))

(ert-deftest agent-repl-test-output-nav-errors-without-a-webview ()
  "A workspace with no output panel is an expected state, so it user-errors."
  ;; Arrange
  (agent-repl-test--with-nav-ws nil
    ;; Act / Assert
    (should-error (agent-repl-output-next-prompt) :type 'user-error)))

(ert-deftest agent-repl-test-output-nav-errors-on-a-killed-webview ()
  "A recorded but dead webview buffer user-errors rather than being written to."
  ;; Arrange
  (let ((buf (generate-new-buffer " *nav-webview-dead*")))
    (kill-buffer buf)
    (agent-repl-test--with-nav-ws (list :frontend-buffer buf)
      ;; Act / Assert
      (should-error (agent-repl-output-next-prompt) :type 'user-error))))

(ert-deftest agent-repl-test-output-nav-logs-and-resignals-a-dispatch-error ()
  "A failed frontend boundary is logged against its workspace then re-signaled."
  ;; Arrange
  (let ((buf (generate-new-buffer " *nav-webview-dispatch-error*"))
        (logs nil))
    (unwind-protect
        (agent-repl-test--with-nav-ws (list :frontend-buffer buf)
          (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                     (lambda (&rest _) (error "frontend boundary failed")))
                    ((symbol-function 'agent-repl--log)
                     (lambda (&rest args) (push args logs))))
            ;; Act / Assert
            (should-error (agent-repl-output-next-prompt)))
          (should
           (cl-some
            (lambda (entry)
              (and (equal (car entry) "navws")
                   (string-match-p "outcome=dispatch-error" (nth 1 entry))))
            logs)))
      (kill-buffer buf))))

;;;; ---- The command surface -----------------------------------------------

(ert-deftest agent-repl-test-output-nav-defines-a-command-per-class-and-direction ()
  "Every (class, direction) pair has an interactive command."
  ;; Act / Assert
  (dolist (class agent-repl-output-nav-classes)
    (dolist (dir agent-repl-output-nav-directions)
      (let ((cmd (intern (format "agent-repl-output-%s-%s" dir class))))
        (should (commandp cmd))))))

(ert-deftest agent-repl-test-output-nav-response-command-cycles-final-bubbles ()
  "The response chord's command asks for the `final' class.
`final' rather than `response' is the purple bubble that ANSWERS a
prompt, as against the commentary between tool calls."
  ;; Arrange
  (let ((buf (generate-new-buffer " *nav-webview-final*"))
        (calls nil))
    (unwind-protect
        (agent-repl-test--with-nav-ws (list :frontend-buffer buf)
          (agent-repl-test--capturing-nav-scripts calls
            ;; Act
            (agent-repl-output-next-final)
            ;; Assert
            (should (string-match-p "'final', 1" (cdr (car calls))))))
      (kill-buffer buf))))

;;;; ---- The webapp contract -----------------------------------------------

(ert-deftest agent-repl-test-output-nav-hook-name-matches-webapp ()
  "The hook name lisp calls is the one the webapp plants on `window'.
webapp/src/nav.ts exports `NAV_HOOK'; a rename on either side silently
turns the chords into no-ops."
  ;; Arrange
  (let* ((ts (expand-file-name "webapp/src/nav.ts" agent-repl--frontend-root))
         (src (with-temp-buffer (insert-file-contents ts) (buffer-string))))
    ;; Act / Assert
    (should (string-match-p
             (format "NAV_HOOK = \"%s\"" (regexp-quote agent-repl-frontend-nav-hook))
             src))))

(ert-deftest agent-repl-test-output-nav-classes-match-webapp ()
  "Every class lisp can ask for is one the webapp's NAV_CLASSES resolves.
A class only this side knows would raise inside the page's hook."
  ;; Arrange
  (let* ((ts (expand-file-name "webapp/src/nav.ts" agent-repl--frontend-root))
         (src (with-temp-buffer (insert-file-contents ts) (buffer-string))))
    (should (string-match "NAV_CLASSES = \\[\\([^]]*\\)\\]" src))
    (let ((declared (match-string 1 src)))
      ;; Act / Assert
      (dolist (class agent-repl-output-nav-classes)
        (should (string-match-p (format "\"%s\"" (regexp-quote class)) declared))))))

;;;; ---- Keybindings -------------------------------------------------------

;; `map!' is a Doom macro that test-helpers.el stubs to a no-op in batch,
;; so the input map carries no `:ni' entries here to look up (and evil's
;; auxiliary keymaps, where they would really land, do not exist either).
;; The binding claims are therefore asserted against input.el's SOURCE --
;; the same tactic test-frontend.el uses to pin the webapp hook names, and
;; the place the claims actually live.

(defun agent-repl-test--input-source ()
  "Return input.el's source text."
  (let ((el (expand-file-name "input.el" agent-repl--frontend-root)))
    (with-temp-buffer (insert-file-contents el) (buffer-string))))

(defun agent-repl-test--binds-chord-p (source chord command)
  "Non-nil when SOURCE binds CHORD to COMMAND for both normal and insert."
  (string-match-p
   (format ":ni +\"%s\" +#'%s" (regexp-quote chord) (regexp-quote command))
   source))

(ert-deftest agent-repl-test-output-nav-input-map-binds-the-prompt-chords ()
  "`C-S-j' / `C-S-k' cycle prompts from the input buffer."
  ;; Arrange
  (let ((src (agent-repl-test--input-source)))
    ;; Act / Assert
    (should (agent-repl-test--binds-chord-p src "C-S-j" "agent-repl-output-next-prompt"))
    (should (agent-repl-test--binds-chord-p src "C-S-k" "agent-repl-output-prev-prompt"))))

(ert-deftest agent-repl-test-output-nav-input-map-binds-the-response-chords ()
  "`M-S-j' / `M-S-k' cycle the agent's response bubbles."
  ;; Arrange
  (let ((src (agent-repl-test--input-source)))
    ;; Act / Assert
    (should (agent-repl-test--binds-chord-p src "M-S-j" "agent-repl-output-next-final"))
    (should (agent-repl-test--binds-chord-p src "M-S-k" "agent-repl-output-prev-final"))))

(ert-deftest agent-repl-test-output-nav-input-map-binds-the-tool-chords ()
  "`C-M-S-j' / `C-M-S-k' cycle tool cards."
  ;; Arrange
  (let ((src (agent-repl-test--input-source)))
    ;; Act / Assert
    (should (agent-repl-test--binds-chord-p src "C-M-S-j" "agent-repl-output-next-tool"))
    (should (agent-repl-test--binds-chord-p src "C-M-S-k" "agent-repl-output-prev-tool"))))

(ert-deftest agent-repl-test-output-nav-binds-each-chord-in-insert-state-too ()
  "Every cycle chord is claimed for INSERT state, suppressing shift-translation.
`C-j' / `C-k' are bound on `override-global-map' (config.el) to
`evil-window-down' and `kill-visual-line'.  Emacs shift-translates a
shifted chord down to its unshifted form ONLY when the shifted one is
unbound, so a `C-S-k' left unclaimed in insert state would KILL THE LINE
the user is composing instead of cycling.  Claiming it is the guard,
which is why every entry is `:ni' and not `:n'."
  ;; Arrange
  (let ((src (agent-repl-test--input-source)))
    ;; Act / Assert
    (dolist (chord '("C-S-j" "C-S-k" "M-S-j" "M-S-k" "C-M-S-j" "C-M-S-k"))
      (should (string-match-p (format ":ni +\"%s\"" (regexp-quote chord)) src)))))

(provide 'test-output-nav)
;;; test-output-nav.el ends here

;;;; ---- The chord vacated for the feed search ------------------------------

;; `map!' is stubbed in batch (see above), so these assert against
;; input.el's source, which is where the claim lives.

(ert-deftest agent-repl-test-output-nav-history-search-moved-off-c-r ()
  "Prompt-history search answers to `C-M-r'.
It was moved off `C-r' so the output feed's incremental search
(webapp/src/search.ts) can have that chord, whose isearch reflex wants
it; `C-M-r' keeps the command reachable rather than dropping it to
`M-x'-only, since this is its ONLY binding."
  ;; Arrange
  (let ((src (agent-repl-test--input-source)))
    ;; Act / Assert
    (should (agent-repl-test--binds-chord-p src "C-M-r" "agent-repl-history-search"))))

(ert-deftest agent-repl-test-output-nav-leaves-c-r-unbound-for-the-feed-search ()
  "`C-r' is left unbound in the input map, vacated for the feed search.
Re-binding it to anything here would take back the chord this move
exists to give away."
  ;; Arrange
  (let ((src (agent-repl-test--input-source)))
    ;; Act / Assert
    (should-not (string-match-p ":ni +\"C-r\"" src))))
