;;; test-prompt-summary.el --- Tests for prompt-summary.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the last-prompt-summary feature: segment formatting,
;; truncation, output cleaning, ws/raw bookmarking, sentinel writeback,
;; and stale-summary dropping.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: prompt-summary-truncate ----

(ert-deftest claude-repl-test-prompt-summary-truncate-empty ()
  "Empty / nil input returns empty string."
  (should (equal (claude-repl--prompt-summary-truncate nil) ""))
  (should (equal (claude-repl--prompt-summary-truncate "") ""))
  (should (equal (claude-repl--prompt-summary-truncate "   \t\n") "")))

(ert-deftest claude-repl-test-prompt-summary-truncate-short-passthrough ()
  "Short single-line strings pass through unchanged."
  (let ((claude-repl-prompt-summary-max-length 60))
    (should (equal (claude-repl--prompt-summary-truncate "hello world")
                   "hello world"))))

(ert-deftest claude-repl-test-prompt-summary-truncate-collapses-whitespace ()
  "Multiline / multispace input collapses to single spaces."
  (let ((claude-repl-prompt-summary-max-length 60))
    (should (equal (claude-repl--prompt-summary-truncate "line1\nline2\n  line3")
                   "line1 line2 line3"))))

(ert-deftest claude-repl-test-prompt-summary-truncate-no-length-cap ()
  "Strings longer than the configured max still pass through unchanged
— the mode-line clips at the screen edge instead of an artificial cap."
  (let ((claude-repl-prompt-summary-max-length 10)
        (long "abcdefghijklmnopqrstuvwxyz"))
    (should (equal (claude-repl--prompt-summary-truncate long) long))))

;;;; ---- Tests: prompt-summary-clean ----

(ert-deftest claude-repl-test-prompt-summary-clean-trims ()
  "Leading/trailing whitespace is removed."
  (should (equal (claude-repl--prompt-summary-clean "  hello  \n") "hello")))

(ert-deftest claude-repl-test-prompt-summary-clean-strips-double-quotes ()
  "Wrapping double quotes are stripped."
  (should (equal (claude-repl--prompt-summary-clean "\"my title\"") "my title")))

(ert-deftest claude-repl-test-prompt-summary-clean-strips-single-quotes ()
  "Wrapping single quotes are stripped."
  (should (equal (claude-repl--prompt-summary-clean "'my title'") "my title")))

(ert-deftest claude-repl-test-prompt-summary-clean-strips-preamble ()
  "Common preambles like `Title:' are stripped."
  (should (equal (claude-repl--prompt-summary-clean "Title: hello world")
                 "hello world"))
  (should (equal (claude-repl--prompt-summary-clean "Summary: x")
                 "x")))

(ert-deftest claude-repl-test-prompt-summary-clean-strips-trailing-punct ()
  "Trailing terminal punctuation is removed."
  (should (equal (claude-repl--prompt-summary-clean "hello world.") "hello world"))
  (should (equal (claude-repl--prompt-summary-clean "hello!") "hello"))
  (should (equal (claude-repl--prompt-summary-clean "hello;") "hello")))

;;;; ---- Tests: prompt-summary-skip-p ----

(ert-deftest claude-repl-test-prompt-summary-skip-empty ()
  "Empty / whitespace inputs are skipped."
  (should (claude-repl--prompt-summary-skip-p ""))
  (should (claude-repl--prompt-summary-skip-p "   \n\t")))

(ert-deftest claude-repl-test-prompt-summary-skip-digits ()
  "Pure-digit inputs (permission selections) are skipped."
  (should (claude-repl--prompt-summary-skip-p "1"))
  (should (claude-repl--prompt-summary-skip-p "42")))

(ert-deftest claude-repl-test-prompt-summary-skip-yn ()
  "Single y/n / Y/N inputs are skipped."
  (should (claude-repl--prompt-summary-skip-p "y"))
  (should (claude-repl--prompt-summary-skip-p "N")))

(ert-deftest claude-repl-test-prompt-summary-skip-bare-slash-cmd ()
  "Bare slash commands without arguments are skipped."
  (should (claude-repl--prompt-summary-skip-p "/clear"))
  (should (claude-repl--prompt-summary-skip-p "/compact")))

(ert-deftest claude-repl-test-prompt-summary-skip-real-prompt-not-skipped ()
  "Real prompts are not skipped."
  (should-not (claude-repl--prompt-summary-skip-p "fix the auth bug"))
  (should-not (claude-repl--prompt-summary-skip-p "/init now please")))

;;;; ---- Tests: prompt-summary-segment ----

(ert-deftest claude-repl-test-prompt-summary-segment-no-ws ()
  "Segment is the empty string when no owning workspace is set."
  (claude-repl-test--with-clean-state
    (with-temp-buffer
      (should (equal (claude-repl--prompt-summary-segment) "")))))

(ert-deftest claude-repl-test-prompt-summary-segment-no-summary ()
  "Segment is empty when ws has no summary state."
  (claude-repl-test--with-clean-state
    (with-temp-buffer
      (setq-local claude-repl--owning-workspace "ws1")
      (should (equal (claude-repl--prompt-summary-segment) "")))))

(ert-deftest claude-repl-test-prompt-summary-segment-pending-shows-label-only ()
  "Pending segment shows only the configured label — no raw prompt prefix."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-prompt-summary-pending-label "prompt summarizing")
          (claude-repl-prompt-summary-max-length 60))
      (claude-repl--ws-put "ws1" :last-prompt-text "fix the auth bug")
      (claude-repl--ws-put "ws1" :last-prompt-summary-pending t)
      (with-temp-buffer
        (setq-local claude-repl--owning-workspace "ws1")
        (let ((seg (claude-repl--prompt-summary-segment)))
          (should (string-match-p "prompt summarizing" seg))
          (should-not (string-match-p "fix the auth bug" seg)))))))

(ert-deftest claude-repl-test-prompt-summary-segment-empty-when-no-state ()
  "Segment is empty (no fallback) when ws has neither summary nor pending."
  (claude-repl-test--with-clean-state
    (with-temp-buffer
      (setq-local claude-repl--owning-workspace "ws1")
      (should (equal (claude-repl--prompt-summary-segment) "")))))

(ert-deftest claude-repl-test-prompt-summary-segment-resolved-shows-summary ()
  "When summary is set, segment contains the summary."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :last-prompt-text "fix the auth bug")
    (claude-repl--ws-put "ws1" :last-prompt-summary "Auth Bug Fix")
    (with-temp-buffer
      (setq-local claude-repl--owning-workspace "ws1")
      (let ((seg (claude-repl--prompt-summary-segment)))
        (should (string-match-p "Auth Bug Fix" seg))))))

(ert-deftest claude-repl-test-prompt-summary-segment-summary-wins-over-pending ()
  "If both summary and pending are set, the summary takes precedence."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :last-prompt-text "fix bug")
    (claude-repl--ws-put "ws1" :last-prompt-summary "Bug Fix")
    (claude-repl--ws-put "ws1" :last-prompt-summary-pending t)
    (with-temp-buffer
      (setq-local claude-repl--owning-workspace "ws1")
      (let ((seg (claude-repl--prompt-summary-segment)))
        (should (string-match-p "Bug Fix" seg))
        (should-not (string-match-p "summarizing" seg))))))

;;;; ---- Tests: kickoff bookmarks ws + raw ----

(ert-deftest claude-repl-test-kickoff-prompt-summary-writes-pending-state ()
  "Kickoff sets :last-prompt-text and :last-prompt-summary-pending immediately."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-prompt-summary-enabled t))
      (cl-letf (((symbol-function 'claude-repl--prompt-summary-spawn)
                 (lambda (_ws _raw) 'fake-proc))
                ((symbol-function 'claude-repl--prompt-summary-redisplay)
                 #'ignore))
        (claude-repl--kickoff-prompt-summary "ws1" "do the thing")
        (should (equal (claude-repl--ws-get "ws1" :last-prompt-text) "do the thing"))
        (should (claude-repl--ws-get "ws1" :last-prompt-summary-pending))
        (should-not (claude-repl--ws-get "ws1" :last-prompt-summary))))))

(ert-deftest claude-repl-test-kickoff-prompt-summary-no-op-when-disabled ()
  "Kickoff is a no-op when the feature is disabled."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-prompt-summary-enabled nil)
          (spawn-called nil))
      (cl-letf (((symbol-function 'claude-repl--prompt-summary-spawn)
                 (lambda (_ws _raw) (setq spawn-called t))))
        (claude-repl--kickoff-prompt-summary "ws1" "do the thing")
        (should-not spawn-called)
        (should-not (claude-repl--ws-get "ws1" :last-prompt-text))))))

(ert-deftest claude-repl-test-kickoff-prompt-summary-skips-trivial-input ()
  "Kickoff skips trivial inputs (digit-only / y/n / bare-slash) without
overwriting the previous summary."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-prompt-summary-enabled t)
          (spawn-called nil))
      (claude-repl--ws-put "ws1" :last-prompt-summary "Previous Summary")
      (cl-letf (((symbol-function 'claude-repl--prompt-summary-spawn)
                 (lambda (_ws _raw) (setq spawn-called t))))
        (claude-repl--kickoff-prompt-summary "ws1" "1")
        (should-not spawn-called)
        (should (equal (claude-repl--ws-get "ws1" :last-prompt-summary)
                       "Previous Summary"))))))

(ert-deftest claude-repl-test-kickoff-prompt-summary-passes-bookmarked-ws-to-spawn ()
  "Kickoff forwards the WS argument unchanged to spawn (bookmarking)."
  (claude-repl-test--with-clean-state
    (let ((captured nil))
      (cl-letf (((symbol-function 'claude-repl--prompt-summary-spawn)
                 (lambda (ws raw) (setq captured (list ws raw))))
                ((symbol-function 'claude-repl--prompt-summary-redisplay)
                 #'ignore))
        (claude-repl--kickoff-prompt-summary "ws-target" "the prompt")
        (should (equal captured '("ws-target" "the prompt")))))))

;;;; ---- Tests: prompt-summary-spawn cwd ----

(ert-deftest claude-repl-test-prompt-summary-spawn-binds-temporary-default-directory ()
  "Spawn must invoke `make-process' with `default-directory' rebound to
`temporary-file-directory'.  Without this, the headless claude inherits
the calling workspace's project-dir, its hooks fire with that cwd, and
the sentinel watcher misattributes them — flipping :claude-state to :done
while the user's interactive Claude is still mid-turn."
  (let ((captured-cwd nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _plist)
                 (setq captured-cwd default-directory)
                 (make-marker)))
              ((symbol-function 'process-send-string) (lambda (&rest _) nil))
              ((symbol-function 'process-send-eof) (lambda (&rest _) nil))
              ((symbol-function 'claude-repl--log) (lambda (&rest _) nil)))
      (claude-repl--prompt-summary-spawn "ws1" "some raw prompt that is long enough")
      (should (equal (file-name-as-directory captured-cwd)
                     (file-name-as-directory temporary-file-directory))))))

;;;; ---- Tests: apply-prompt-summary (writeback + stale-drop) ----

(ert-deftest claude-repl-test-apply-prompt-summary-writes-when-raw-matches ()
  "Apply writes the summary when WS's :last-prompt-text still matches RAW."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :last-prompt-text "the prompt")
    (claude-repl--ws-put "ws1" :last-prompt-summary-pending t)
    (cl-letf (((symbol-function 'claude-repl--prompt-summary-redisplay)
               #'ignore))
      (claude-repl--prompt-summary-apply "ws1" "the prompt" "Short Title")
      (should (equal (claude-repl--ws-get "ws1" :last-prompt-summary)
                     "Short Title"))
      (should-not (claude-repl--ws-get "ws1" :last-prompt-summary-pending)))))

(ert-deftest claude-repl-test-apply-prompt-summary-drops-stale ()
  "Apply drops the summary when a newer prompt has overwritten the bookmark."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :last-prompt-text "the new prompt")
    (claude-repl--ws-put "ws1" :last-prompt-summary "Existing")
    (claude-repl--ws-put "ws1" :last-prompt-summary-pending t)
    (cl-letf (((symbol-function 'claude-repl--prompt-summary-redisplay)
               #'ignore))
      (claude-repl--prompt-summary-apply "ws1" "the OLD prompt" "Stale Title")
      (should (equal (claude-repl--ws-get "ws1" :last-prompt-summary) "Existing"))
      ;; Pending also unchanged — stale path doesn't touch it
      (should (claude-repl--ws-get "ws1" :last-prompt-summary-pending)))))

(ert-deftest claude-repl-test-apply-prompt-summary-redisplays-vterm ()
  "Apply forces a mode-line update on the workspace's vterm buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-panel-apply-redisp*"
      (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (claude-repl--ws-put "ws1" :last-prompt-text "raw")
      (let ((called nil))
        (cl-letf (((symbol-function 'force-mode-line-update)
                   (lambda (&rest _) (setq called t))))
          (claude-repl--prompt-summary-apply "ws1" "raw" "Title")
          (should called))))))

;;;; ---- Tests: sandbox-mode-line includes the segment ----

(ert-deftest claude-repl-test-sandbox-mode-line-has-eval-segment ()
  "sandbox-mode-line returns a list whose tail is the :eval segment."
  (let ((result (claude-repl--sandbox-mode-line nil nil)))
    (should (>= (length result) 2))
    ;; The :eval form is a cons cell starting with :eval.
    (let ((tail (car (last result))))
      (should (consp tail))
      (should (eq (car tail) :eval)))))

;;;; ---- Tests: do-send invokes kickoff ----

(ert-deftest claude-repl-test-do-send-kicks-off-prompt-summary ()
  "`claude-repl--do-send' invokes `claude-repl--kickoff-prompt-summary'
with the workspace and raw input."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-panel-do-send-summary*"
      (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (let ((kickoff-args nil))
        (cl-letf (((symbol-function 'claude-repl--send-input-to-vterm) #'ignore)
                  ((symbol-function 'claude-repl--run-send-posthooks) #'ignore)
                  ((symbol-function 'claude-repl--kickoff-prompt-summary)
                   (lambda (ws raw) (setq kickoff-args (list ws raw)))))
          (claude-repl--do-send "ws1" "decorated-input" "raw-input"))
        (should (equal kickoff-args '("ws1" "raw-input")))))))

;;;; ---- Tests: mode-line migration (attach-to-mode-line) ----

(ert-deftest claude-repl-test-attach-to-mode-line-appends-when-missing ()
  "attach-to-mode-line appends the :eval segment when not already present."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-panel-attach-missing*"
      (setq-local mode-line-format (list "BARE METAL: host"))
      (claude-repl--prompt-summary-attach-to-mode-line (current-buffer))
      (should (= (length mode-line-format) 2))
      (should (equal (car (last mode-line-format))
                     claude-repl--prompt-summary-mode-line-spec)))))

(ert-deftest claude-repl-test-attach-to-mode-line-idempotent ()
  "attach-to-mode-line does not double-append when called twice."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-panel-attach-idempotent*"
      (setq-local mode-line-format
                  (list "BARE METAL: host"
                        claude-repl--prompt-summary-mode-line-spec))
      (claude-repl--prompt-summary-attach-to-mode-line (current-buffer))
      (claude-repl--prompt-summary-attach-to-mode-line (current-buffer))
      (should (= (length mode-line-format) 2)))))

(ert-deftest claude-repl-test-attach-to-mode-line-skips-non-list ()
  "attach-to-mode-line leaves string mode-line-formats alone."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-panel-attach-string*"
      (setq-local mode-line-format "literal-string")
      (claude-repl--prompt-summary-attach-to-mode-line (current-buffer))
      (should (equal mode-line-format "literal-string")))))

(ert-deftest claude-repl-test-attach-all-walks-workspaces ()
  "attach-all attaches the segment to every live workspace vterm buffer."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-temp-buffer "*claude-panel-attach-all-1*"
      (setq-local mode-line-format (list "BARE METAL: a"))
      (claude-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (claude-repl-test--with-temp-buffer "*claude-panel-attach-all-2*"
        (setq-local mode-line-format (list "DOCKER SANDBOX: img"))
        (claude-repl--ws-put "ws2" :vterm-buffer (current-buffer))
        (claude-repl-prompt-summary-attach-all)
        (with-current-buffer "*claude-panel-attach-all-1*"
          (should (member claude-repl--prompt-summary-mode-line-spec
                          mode-line-format)))
        (with-current-buffer "*claude-panel-attach-all-2*"
          (should (member claude-repl--prompt-summary-mode-line-spec
                          mode-line-format)))))))

(ert-deftest claude-repl-test-attach-all-skips-dead-buffer ()
  "attach-all tolerates dead vterm buffers in the workspace table."
  (claude-repl-test--with-clean-state
    (let ((dead-buf (generate-new-buffer "*claude-panel-attach-dead*")))
      (kill-buffer dead-buf)
      (claude-repl--ws-put "ws-dead" :vterm-buffer dead-buf)
      ;; Should not signal.
      (claude-repl-prompt-summary-attach-all))))

(provide 'test-prompt-summary)
;;; test-prompt-summary.el ends here
