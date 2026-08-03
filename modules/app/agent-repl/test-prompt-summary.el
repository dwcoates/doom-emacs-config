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

(ert-deftest agent-repl-test-prompt-summary-truncate-empty ()
  "Empty / nil input returns empty string."
  (should (equal (agent-repl--prompt-summary-truncate nil) ""))
  (should (equal (agent-repl--prompt-summary-truncate "") ""))
  (should (equal (agent-repl--prompt-summary-truncate "   \t\n") "")))

(ert-deftest agent-repl-test-prompt-summary-truncate-short-passthrough ()
  "Short single-line strings pass through unchanged."
  (let ((agent-repl-prompt-summary-max-length 60))
    (should (equal (agent-repl--prompt-summary-truncate "hello world")
                   "hello world"))))

(ert-deftest agent-repl-test-prompt-summary-truncate-collapses-whitespace ()
  "Multiline / multispace input collapses to single spaces."
  (let ((agent-repl-prompt-summary-max-length 60))
    (should (equal (agent-repl--prompt-summary-truncate "line1\nline2\n  line3")
                   "line1 line2 line3"))))

(ert-deftest agent-repl-test-prompt-summary-truncate-no-length-cap ()
  "Strings longer than the configured max still pass through unchanged
— the mode-line clips at the screen edge instead of an artificial cap."
  (let ((agent-repl-prompt-summary-max-length 10)
        (long "abcdefghijklmnopqrstuvwxyz"))
    (should (equal (agent-repl--prompt-summary-truncate long) long))))

;;;; ---- Tests: prompt-summary-clean ----

(ert-deftest agent-repl-test-prompt-summary-clean-trims ()
  "Leading/trailing whitespace is removed (a declarative period is then
appended since the cleaned input lacks one)."
  (should (equal (agent-repl--prompt-summary-clean "  hello  \n") "hello.")))

(ert-deftest agent-repl-test-prompt-summary-clean-strips-double-quotes ()
  "Wrapping double quotes are stripped (a declarative period is then
appended since the cleaned input lacks one)."
  (should (equal (agent-repl--prompt-summary-clean "\"my title\"") "my title.")))

(ert-deftest agent-repl-test-prompt-summary-clean-strips-single-quotes ()
  "Wrapping single quotes are stripped (a declarative period is then
appended since the cleaned input lacks one)."
  (should (equal (agent-repl--prompt-summary-clean "'my title'") "my title.")))

(ert-deftest agent-repl-test-prompt-summary-clean-strips-preamble ()
  "Common preambles like `Title:' are stripped (a declarative period is
then appended since the cleaned input lacks one)."
  (should (equal (agent-repl--prompt-summary-clean "Title: hello world")
                 "hello world."))
  (should (equal (agent-repl--prompt-summary-clean "Summary: x")
                 "x.")))

(ert-deftest agent-repl-test-prompt-summary-clean-preserves-trailing-period ()
  "Trailing `.' is preserved — declarative mood requires the period to
survive cleaning."
  (should (equal (agent-repl--prompt-summary-clean "hello world.")
                 "hello world.")))

(ert-deftest agent-repl-test-prompt-summary-clean-strips-trailing-bang ()
  "Trailing `!' is stripped and replaced with the declarative period."
  (should (equal (agent-repl--prompt-summary-clean "hello!") "hello.")))

(ert-deftest agent-repl-test-prompt-summary-clean-strips-trailing-semicolon ()
  "Trailing `;' is stripped and replaced with the declarative period."
  (should (equal (agent-repl--prompt-summary-clean "hello;") "hello.")))

(ert-deftest agent-repl-test-prompt-summary-clean-strips-trailing-question-mark ()
  "Trailing `?' is stripped — declarative mood forbids the question mark
and the cleaner enforces it as a safety net against model disobedience."
  (should (equal (agent-repl--prompt-summary-clean "How does the cache work?")
                 "How does the cache work."))
  (should (equal (agent-repl--prompt-summary-clean "Why is auth failing?")
                 "Why is auth failing.")))

(ert-deftest agent-repl-test-prompt-summary-clean-adds-period-when-missing ()
  "When the model output ends with no terminal punctuation, the cleaner
appends a single `.' so the segment always reads as a declarative
sentence."
  (should (equal (agent-repl--prompt-summary-clean "Transport layer is being built")
                 "Transport layer is being built.")))

;;;; ---- Tests: prompt-summary-skip-p ----

(ert-deftest agent-repl-test-prompt-summary-skip-empty ()
  "Empty / whitespace inputs are skipped."
  (should (agent-repl--prompt-summary-skip-p ""))
  (should (agent-repl--prompt-summary-skip-p "   \n\t")))

(ert-deftest agent-repl-test-prompt-summary-skip-digits ()
  "Pure-digit inputs (permission selections) are skipped."
  (should (agent-repl--prompt-summary-skip-p "1"))
  (should (agent-repl--prompt-summary-skip-p "42")))

(ert-deftest agent-repl-test-prompt-summary-skip-yn ()
  "Single y/n / Y/N inputs are skipped."
  (should (agent-repl--prompt-summary-skip-p "y"))
  (should (agent-repl--prompt-summary-skip-p "N")))

(ert-deftest agent-repl-test-prompt-summary-skip-bare-slash-cmd ()
  "Bare slash commands without arguments are skipped."
  (should (agent-repl--prompt-summary-skip-p "/clear"))
  (should (agent-repl--prompt-summary-skip-p "/compact")))

(ert-deftest agent-repl-test-prompt-summary-skip-real-prompt-not-skipped ()
  "Real prompts are not skipped."
  (should-not (agent-repl--prompt-summary-skip-p "fix the auth bug"))
  (should-not (agent-repl--prompt-summary-skip-p "/init now please")))

;;;; ---- Tests: format-summary-relative-time ----

(ert-deftest agent-repl-test-format-summary-relative-time-nil ()
  "Nil SENT-AT returns nil so callers can suppress the prefix."
  (should-not (agent-repl--format-summary-relative-time nil)))

(ert-deftest agent-repl-test-format-summary-relative-time-non-number ()
  "Non-numeric SENT-AT returns nil (defensive guard)."
  (should-not (agent-repl--format-summary-relative-time "not-a-number")))

(ert-deftest agent-repl-test-format-summary-relative-time-under-a-minute ()
  "Elapsed time under one minute renders as `<1m ago'."
  (should (equal (agent-repl--format-summary-relative-time 1000.0 1030.0)
                 "<1m ago"))
  (should (equal (agent-repl--format-summary-relative-time 1000.0 1059.9)
                 "<1m ago")))

(ert-deftest agent-repl-test-format-summary-relative-time-exact-minute ()
  "Exactly one minute renders as `1m ago'."
  (should (equal (agent-repl--format-summary-relative-time 1000.0 1060.0)
                 "1m ago")))

(ert-deftest agent-repl-test-format-summary-relative-time-tens-of-minutes ()
  "Whole-minute granularity rounds down (10m, 30m)."
  (should (equal (agent-repl--format-summary-relative-time 1000.0
                                                            (+ 1000.0 (* 10 60)))
                 "10m ago"))
  (should (equal (agent-repl--format-summary-relative-time 1000.0
                                                            (+ 1000.0 (* 30 60) 30))
                 "30m ago")))

(ert-deftest agent-repl-test-format-summary-relative-time-just-under-an-hour ()
  "59 minutes still renders as minutes, not hours."
  (should (equal (agent-repl--format-summary-relative-time 1000.0
                                                            (+ 1000.0 (* 59 60)))
                 "59m ago")))

(ert-deftest agent-repl-test-format-summary-relative-time-exact-hour ()
  "Exactly one hour renders as `1hr 0m ago'."
  (should (equal (agent-repl--format-summary-relative-time 1000.0
                                                            (+ 1000.0 (* 60 60)))
                 "1hr 0m ago")))

(ert-deftest agent-repl-test-format-summary-relative-time-hour-and-minutes ()
  "Over an hour renders as `Xhr Ym ago' (example from the user's brief)."
  (should (equal (agent-repl--format-summary-relative-time 1000.0
                                                            (+ 1000.0 (* 62 60)))
                 "1hr 2m ago")))

(ert-deftest agent-repl-test-format-summary-relative-time-many-hours ()
  "Hours past 24 still render in raw hours (no day rollover)."
  (should (equal (agent-repl--format-summary-relative-time 1000.0
                                                            (+ 1000.0 (* 25 60 60) (* 30 60)))
                 "25hr 30m ago")))

(ert-deftest agent-repl-test-format-summary-relative-time-negative-clamps-to-zero ()
  "A SENT-AT in the future (clock drift) clamps to `<1m ago' rather
than rendering a nonsensical negative duration."
  (should (equal (agent-repl--format-summary-relative-time 2000.0 1000.0)
                 "<1m ago")))

;;;; ---- Tests: prompt-summary-segment ----

(ert-deftest agent-repl-test-prompt-summary-segment-no-ws ()
  "Segment is the empty string when no owning workspace is set."
  (agent-repl-test--with-clean-state
    (with-temp-buffer
      (should (equal (agent-repl--prompt-summary-segment) "")))))

(ert-deftest agent-repl-test-prompt-summary-segment-no-summary ()
  "Segment is empty when ws has no summary state."
  (agent-repl-test--with-clean-state
    (with-temp-buffer
      (setq-local agent-repl--owning-workspace "ws1")
      (should (equal (agent-repl--prompt-summary-segment) "")))))

(ert-deftest agent-repl-test-prompt-summary-segment-pending-shows-label-only ()
  "Pending segment shows only the configured label — no raw prompt prefix."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-prompt-summary-pending-label "prompt summarizing")
          (agent-repl-prompt-summary-max-length 60))
      (agent-repl--ws-put "ws1" :last-prompt-text "fix the auth bug")
      (agent-repl--ws-put "ws1" :last-prompt-summary-pending t)
      (with-temp-buffer
        (setq-local agent-repl--owning-workspace "ws1")
        (let ((seg (agent-repl--prompt-summary-segment)))
          (should (string-match-p "prompt summarizing" seg))
          (should-not (string-match-p "fix the auth bug" seg)))))))

(ert-deftest agent-repl-test-prompt-summary-segment-empty-when-no-state ()
  "Segment is empty (no fallback) when ws has neither summary nor pending."
  (agent-repl-test--with-clean-state
    (with-temp-buffer
      (setq-local agent-repl--owning-workspace "ws1")
      (should (equal (agent-repl--prompt-summary-segment) "")))))

(ert-deftest agent-repl-test-prompt-summary-segment-resolved-shows-summary ()
  "When summary is set, segment contains the summary."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :last-prompt-text "fix the auth bug")
    (agent-repl--ws-put "ws1" :last-prompt-summary "Auth Bug Fix")
    (with-temp-buffer
      (setq-local agent-repl--owning-workspace "ws1")
      (let ((seg (agent-repl--prompt-summary-segment)))
        (should (string-match-p "Auth Bug Fix" seg))))))

(ert-deftest agent-repl-test-prompt-summary-segment-summary-wins-over-pending ()
  "If both summary and pending are set, the summary takes precedence."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :last-prompt-text "fix bug")
    (agent-repl--ws-put "ws1" :last-prompt-summary "Bug Fix")
    (agent-repl--ws-put "ws1" :last-prompt-summary-pending t)
    (with-temp-buffer
      (setq-local agent-repl--owning-workspace "ws1")
      (let ((seg (agent-repl--prompt-summary-segment)))
        (should (string-match-p "Bug Fix" seg))
        (should-not (string-match-p "summarizing" seg))))))

(ert-deftest agent-repl-test-prompt-summary-segment-prefixes-timestamp ()
  "Segment prepends a relative `X ago' prefix before the summary when
:last-prompt-summary-at is populated."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :last-prompt-summary "Auth Bug Fix")
    (agent-repl--ws-put "ws1" :last-prompt-summary-at 1000.0)
    (cl-letf (((symbol-function 'float-time)
               (lambda (&rest _) (+ 1000.0 (* 10 60)))))
      (with-temp-buffer
        (setq-local agent-repl--owning-workspace "ws1")
        (let ((seg (agent-repl--prompt-summary-segment)))
          (should (string-match-p "10m ago" seg))
          (should (string-match-p "Auth Bug Fix" seg))
          ;; Timestamp comes BEFORE the summary text.
          (should (< (string-match "10m ago" seg)
                     (string-match "Auth Bug Fix" seg))))))))

(ert-deftest agent-repl-test-prompt-summary-segment-timestamp-is-light-grey ()
  "The `X ago' prefix is rendered in light grey, distinct from the blue
summary text."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :last-prompt-summary "Auth Bug Fix")
    (agent-repl--ws-put "ws1" :last-prompt-summary-at 1000.0)
    (cl-letf (((symbol-function 'float-time)
               (lambda (&rest _) (+ 1000.0 (* 10 60)))))
      (with-temp-buffer
        (setq-local agent-repl--owning-workspace "ws1")
        (let* ((seg (agent-repl--prompt-summary-segment))
               (pos (string-match "10m ago" seg))
               (face (get-text-property pos 'face seg)))
          (should (equal (plist-get face :foreground) "light grey")))))))

(ert-deftest agent-repl-test-prompt-summary-segment-pending-prefixes-timestamp ()
  "Pending segment also carries the `X ago' prefix so the user sees
when the in-flight summary was kicked off."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-prompt-summary-pending-label "prompt summarizing"))
      (agent-repl--ws-put "ws1" :last-prompt-summary-pending t)
      (agent-repl--ws-put "ws1" :last-prompt-summary-at 1000.0)
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&rest _) (+ 1000.0 (* 5 60)))))
        (with-temp-buffer
          (setq-local agent-repl--owning-workspace "ws1")
          (let ((seg (agent-repl--prompt-summary-segment)))
            (should (string-match-p "5m ago" seg))
            (should (string-match-p "prompt summarizing" seg))))))))

(ert-deftest agent-repl-test-prompt-summary-segment-omits-timestamp-when-missing ()
  "Segment omits the `X ago' prefix when :last-prompt-summary-at has not
been populated yet (e.g. summaries restored from an old state file)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :last-prompt-summary "Auth Bug Fix")
    (with-temp-buffer
      (setq-local agent-repl--owning-workspace "ws1")
      (let ((seg (agent-repl--prompt-summary-segment)))
        (should (string-match-p "Auth Bug Fix" seg))
        (should-not (string-match-p "ago" seg))))))

;;;; ---- Tests: kickoff bookmarks ws + raw ----

(ert-deftest agent-repl-test-kickoff-prompt-summary-writes-pending-state ()
  "Kickoff sets :last-prompt-text and :last-prompt-summary-pending immediately."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-prompt-summary-enabled t))
      (cl-letf (((symbol-function 'agent-repl--prompt-summary-spawn)
                 (lambda (_ws _raw) 'fake-proc))
                ((symbol-function 'agent-repl--prompt-summary-redisplay)
                 #'ignore))
        (agent-repl--kickoff-prompt-summary "ws1" "do the thing")
        (should (equal (agent-repl--ws-get "ws1" :last-prompt-text) "do the thing"))
        (should (agent-repl--ws-get "ws1" :last-prompt-summary-pending))
        (should-not (agent-repl--ws-get "ws1" :last-prompt-summary))))))

(ert-deftest agent-repl-test-kickoff-prompt-summary-no-op-when-disabled ()
  "Kickoff is a no-op when the feature is disabled."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-prompt-summary-enabled nil)
          (spawn-called nil))
      (cl-letf (((symbol-function 'agent-repl--prompt-summary-spawn)
                 (lambda (_ws _raw) (setq spawn-called t))))
        (agent-repl--kickoff-prompt-summary "ws1" "do the thing")
        (should-not spawn-called)
        (should-not (agent-repl--ws-get "ws1" :last-prompt-text))))))

(ert-deftest agent-repl-test-kickoff-prompt-summary-skips-trivial-input ()
  "Kickoff skips trivial inputs (digit-only / y/n / bare-slash) without
overwriting the previous summary."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-prompt-summary-enabled t)
          (spawn-called nil))
      (agent-repl--ws-put "ws1" :last-prompt-summary "Previous Summary")
      (cl-letf (((symbol-function 'agent-repl--prompt-summary-spawn)
                 (lambda (_ws _raw) (setq spawn-called t))))
        (agent-repl--kickoff-prompt-summary "ws1" "1")
        (should-not spawn-called)
        (should (equal (agent-repl--ws-get "ws1" :last-prompt-summary)
                       "Previous Summary"))))))

(ert-deftest agent-repl-test-kickoff-prompt-summary-clears-pending-on-spawn-failure ()
  "A failed process launch must not leave the workspace permanently pending."
  (agent-repl-test--with-clean-state
    (let ((persisted nil))
      (cl-letf (((symbol-function 'agent-repl--prompt-summary-spawn)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--prompt-summary-redisplay) #'ignore)
                ((symbol-function 'agent-repl--state-save)
                 (lambda (ws) (setq persisted ws))))
        (agent-repl--kickoff-prompt-summary "ws1" "do the thing")
        (should-not (agent-repl--ws-get "ws1" :last-prompt-summary-pending))
        (should (equal persisted "ws1"))))))

(ert-deftest agent-repl-test-kickoff-prompt-summary-passes-bookmarked-ws-to-spawn ()
  "Kickoff forwards the WS argument unchanged to spawn (bookmarking)."
  (agent-repl-test--with-clean-state
    (let ((captured nil))
      (cl-letf (((symbol-function 'agent-repl--prompt-summary-spawn)
                 (lambda (ws raw) (setq captured (list ws raw))))
                ((symbol-function 'agent-repl--prompt-summary-redisplay)
                 #'ignore))
        (agent-repl--kickoff-prompt-summary "ws-target" "the prompt")
        (should (equal captured '("ws-target" "the prompt")))))))

(ert-deftest agent-repl-test-kickoff-prompt-summary-stamps-summary-at ()
  "Kickoff stamps :last-prompt-summary-at with the current float-time so
the mode-line `X ago' prefix anchors against this send."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-prompt-summary-enabled t))
      (cl-letf (((symbol-function 'agent-repl--prompt-summary-spawn)
                 (lambda (_ws _raw) 'fake-proc))
                ((symbol-function 'agent-repl--prompt-summary-redisplay)
                 #'ignore)
                ((symbol-function 'float-time) (lambda (&rest _) 7654321.0)))
        (agent-repl--kickoff-prompt-summary "ws1" "do the thing")
        (should (equal (agent-repl--ws-get "ws1" :last-prompt-summary-at)
                       7654321.0))))))

(ert-deftest agent-repl-test-kickoff-prompt-summary-skipped-keeps-prior-summary-at ()
  "Skipped (digit-only/yn/bare-slash) kickoffs do NOT overwrite the
prior :last-prompt-summary-at, so the `X ago' prefix keeps tracking
the original summarized prompt."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-prompt-summary-enabled t))
      (agent-repl--ws-put "ws1" :last-prompt-summary "Previous Summary")
      (agent-repl--ws-put "ws1" :last-prompt-summary-at 5000.0)
      (cl-letf (((symbol-function 'agent-repl--prompt-summary-spawn)
                 (lambda (&rest _) (error "spawn must not run for skipped input")))
                ((symbol-function 'float-time) (lambda (&rest _) 9999.0)))
        (agent-repl--kickoff-prompt-summary "ws1" "1")
        (should (equal (agent-repl--ws-get "ws1" :last-prompt-summary-at)
                       5000.0))))))

;;;; ---- Tests: prompt-format forbids tool use / external investigation ----

(ert-deftest agent-repl-test-prompt-format-forbids-tool-use ()
  "Format must explicitly forbid the model from using any tools.
Pins the NO INVESTIGATION directive so the summarizer never spends
tokens on agentic exploration when the supplied text is enough."
  (let ((rendered (agent-repl--prompt-summary-build-input "the prompt")))
    (should (string-match-p "NO INVESTIGATION" rendered))
    (should (string-match-p "Do NOT use any tools" rendered))))

(ert-deftest agent-repl-test-prompt-format-forbids-filesystem-investigation ()
  "Format must explicitly forbid filesystem / external-state investigation.
Pins the specific carve-outs (read files, run commands, fetch URLs, etc.)
so a future tightening of the directive doesn't silently lose coverage of
a vector."
  (let ((rendered (agent-repl--prompt-summary-build-input "the prompt")))
    (should (string-match-p "read files" rendered))
    (should (string-match-p "run shell commands" rendered))
    (should (string-match-p "fetch URLs" rendered))))

(ert-deftest agent-repl-test-prompt-format-instructs-best-guess-on-ambiguity ()
  "Format must instruct the model to give a best-guess summary when the
supplied text is ambiguous rather than attempting to investigate.
Prevents the model from rationalizing tool use under uncertainty."
  (let ((rendered (agent-repl--prompt-summary-build-input "the prompt")))
    (should (string-match-p "best-guess summary" rendered))
    (should (string-match-p "never attempt to gather more information"
                            rendered))))

;;;; ---- Tests: prompt-format guards against responding to embedded prompt ----

(ert-deftest agent-repl-test-prompt-format-instructs-summarize-only ()
  "Format must tell the model to summarize, not respond to, the embedded prompt.
Pins the anti-injection framing: the model must treat the prompt as data,
never execute / answer / refuse / comply with it."
  (let ((rendered (agent-repl--prompt-summary-build-input
                   "what is 2+2? please answer")))
    ;; Must explicitly call out that the prompt is to be analyzed/summarized only.
    (should (string-match-p "\\(?:SUMMARIZE\\|summarize\\|analyze\\)" rendered))
    ;; Must explicitly forbid responding to the embedded prompt.
    (should (string-match-p "\\(?:NEVER\\|Do NOT\\|not\\) \\(?:answer\\|respond\\)"
                            rendered))
    ;; Must label the embedded prompt as data, not a directive.
    (should (string-match-p "\\(?:DATA\\|data\\), not \\(?:a directive\\|directives\\)"
                            rendered))))

;;;; ---- Tests: prompt-format frames the task as current top-level objective ----

(ert-deftest agent-repl-test-prompt-format-frames-current-top-level-objective ()
  "Format must frame the model's job as identifying the CURRENT top-level
objective of the conversation, not as merely summarizing the latest
prompt verbatim.  Pins the philosophy shift from `summarize the latest
prompt' to `track the user's current high-level effort'."
  (let ((rendered (agent-repl--prompt-summary-build-input "do the thing")))
    (should (string-match-p "CURRENT TOP-LEVEL OBJECTIVE" rendered))
    (should (string-match-p "highest-level effort" rendered))))

(ert-deftest agent-repl-test-prompt-format-forbids-granular-sub-step-summary ()
  "Format must instruct the model to roll tactical sub-steps up to their
parent top-level effort, not to summarize the latest prompt verbatim
when that prompt is a sub-step (`go for it', `how?', `run the tests')."
  (let ((rendered (agent-repl--prompt-summary-build-input "go for it")))
    (should (string-match-p "\\(?:TACTICAL SUB-STEPS\\|tactical sub-step\\)"
                            rendered))
    (should (string-match-p "HIGHER-LEVEL effort" rendered))))

(ert-deftest agent-repl-test-prompt-format-instructs-following-goal-pivots ()
  "Format must instruct the model that when the latest prompt pivots to
a new top-level effort, the summary should describe the NEW effort and
discard the OLD effort — never blend the two."
  (let ((rendered (agent-repl--prompt-summary-build-input "now do something different")))
    (should (string-match-p "\\(?:GOAL SHIFTS\\|PIVOTS\\|pivot\\)" rendered))
    (should (string-match-p "NEW effort" rendered))
    (should (string-match-p "\\(?:Discard\\|discard\\) the old\\(?:er\\)? effort"
                            rendered))))

(ert-deftest agent-repl-test-prompt-format-includes-worked-pivot-example ()
  "Format must contain the canonical worked example anchoring the
required granularity AND declarative-mood: the doom-config-sharing →
transport-layer pivot case.  Both WRONG formulations (old effort, and
right-effort-wrong-mood) and the RIGHT declarative-mood formulation are
pinned so the framing cannot be silently weakened."
  (let ((rendered (agent-repl--prompt-summary-build-input "the prompt")))
    ;; WRONG #1: captures the OLD effort.
    (should (string-match-p
             "Doom config sharing with brother is being simplified"
             rendered))
    ;; WRONG #2: right effort but wrong (interrogative, not declarative) mood.
    (should (string-match-p
             "How should the transport layer between machines be built\\?"
             rendered))
    ;; RIGHT: declarative-mood phrasing of the current top-level effort.
    (should (string-match-p
             "Transport layer for communicating with brother is being built\\."
             rendered))))

;;;; ---- Tests: prompt-format requires always-declarative mood ----

(ert-deftest agent-repl-test-prompt-format-requires-always-declarative-mood ()
  "Format must require the reminder to ALWAYS be phrased as a declarative
statement, ending with `.', regardless of the mood of the latest prompt.
Pins the always-declarative rule so a future relaxation back to mixed
mood cannot land silently."
  (let ((rendered (agent-repl--prompt-summary-build-input "fix the auth bug")))
    ;; The "ALWAYS" / "declarative" framing.
    (should (string-match-p "ALWAYS be phrased as a DECLARATIVE" rendered))
    ;; Explicit override: not the mood of the latest prompt.
    (should (string-match-p "regardless of the mood of the latest prompt"
                            rendered))
    ;; Interrogative mood is explicitly forbidden.
    (should (string-match-p
             "\\(?:NEVER end with \"\\?\"\\|interrogative mood is FORBIDDEN\\)"
             rendered))))

(ert-deftest agent-repl-test-prompt-format-output-section-requires-trailing-period ()
  "Output-formatting section must require a trailing `.' on the reminder.
Pins the wire-format requirement so the output rule and the MOOD rule
agree."
  (let ((rendered (agent-repl--prompt-summary-build-input "fix the auth bug")))
    (should (string-match-p "MUST end with a single \"\\.\"" rendered))))

;;;; ---- Tests: prompt-format forbids pronouns as subject/direct object ----

(ert-deftest agent-repl-test-prompt-format-forbids-pronoun-subject ()
  "Format must instruct the model never to use a pronoun as subject or
direct object of the reminder.  Pins both the rule itself and a worked
example so a future refactor doesn't accidentally weaken it."
  (let ((rendered (agent-repl--prompt-summary-build-input "why doesn't it work?")))
    ;; The rule itself.
    (should (string-match-p "NO PRONOUNS" rendered))
    (should (string-match-p "subject or direct object" rendered))
    ;; The canonical worked example from the user's directive.
    (should (string-match-p "Why doesn't it work\\?" rendered))
    (should (string-match-p "brief descriptor" rendered))
    ;; Possessive-position carve-out is explicitly preserved.
    (should (string-match-p "possessive position" rendered))))

;;;; ---- Tests: prompt-format includes CONTEXT section ----

(ert-deftest agent-repl-test-prompt-format-includes-context-header ()
  "Format must have a CONTEXT section that precedes the PROMPT block, so
the model reads the prior prompts before the latest one."
  (let ((rendered (agent-repl--prompt-summary-build-input "the prompt")))
    (should (string-match-p "CONTEXT" rendered))
    (should (string-match-p "PROMPT" rendered))
    ;; CONTEXT must appear before the labeled PROMPT block.
    (should (< (string-match "CONTEXT (recent prior prompts" rendered)
               (string-match "PROMPT (the latest user message" rendered)))))

(ert-deftest agent-repl-test-prompt-format-context-placeholder-when-empty ()
  "When no prior prompts are passed, the CONTEXT slot still renders a
human-readable placeholder rather than an empty block.  This keeps the
model from latching onto a blank slot as a sign of missing data."
  (let ((rendered (agent-repl--prompt-summary-build-input "the prompt" nil)))
    (should (string-match-p "no prior prompts" rendered))))

(ert-deftest agent-repl-test-prompt-format-context-entries-rendered ()
  "When context prompts are passed, each entry appears in the rendered
output, numbered and on its own line."
  (let* ((ctx '("first prior prompt" "second prior prompt"))
         (rendered (agent-repl--prompt-summary-build-input "the prompt" ctx)))
    (should (string-match-p "\\[1\\] first prior prompt" rendered))
    (should (string-match-p "\\[2\\] second prior prompt" rendered))))

;;;; ---- Tests: format-context entry rendering ----

(ert-deftest agent-repl-test-prompt-summary-format-context-empty ()
  "Empty/nil context yields the placeholder line."
  (should (string-match-p "no prior prompts"
                          (agent-repl--prompt-summary-format-context nil)))
  (should (string-match-p "no prior prompts"
                          (agent-repl--prompt-summary-format-context '()))))

(ert-deftest agent-repl-test-prompt-summary-format-context-collapses-whitespace ()
  "Multi-line context entries collapse to single-line form so the
formatted block doesn't smuggle newlines that confuse the numbered list."
  (let ((rendered (agent-repl--prompt-summary-format-context
                   '("line1\nline2\n  line3"))))
    (should (string-match-p "\\[1\\] line1 line2 line3" rendered))
    (should-not (string-match-p "line1\nline2" rendered))))

(ert-deftest agent-repl-test-prompt-summary-format-context-truncates ()
  "Context entries longer than the per-entry cap are truncated with an
ellipsis so a single huge prior prompt can't dominate the prompt budget."
  (let ((agent-repl-prompt-summary-context-truncate 10))
    (let ((rendered (agent-repl--prompt-summary-format-context
                     '("abcdefghijklmnop"))))
      (should (string-match-p "\\[1\\] abcdefghij…" rendered))
      (should-not (string-match-p "klmnop" rendered)))))

;;;; ---- Tests: context-count default ----

(ert-deftest agent-repl-test-prompt-summary-context-count-default-is-ten ()
  "Default `agent-repl-prompt-summary-context-count' is 10.
Pins the chosen default so future tweaks are deliberate — more context
improves goal-shift detection and reduces the temptation for the model
to investigate external state to fill in gaps."
  (should (equal (default-value 'agent-repl-prompt-summary-context-count) 10)))

;;;; ---- Tests: collect-context reads buffer-local input history ----

(ert-deftest agent-repl-test-prompt-summary-collect-context-takes-recent-n ()
  "Collect returns the last N prompts from the input buffer's history,
re-ordered oldest first (history stores newest first)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-collect-context-n*"
      (setq-local agent-repl--input-history
                  '("newest" "middle" "oldest" "way-older"))
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (let ((agent-repl-prompt-summary-context-count 3))
        (should (equal (agent-repl--prompt-summary-collect-context "ws1")
                       '("oldest" "middle" "newest")))))))

(ert-deftest agent-repl-test-prompt-summary-collect-context-fewer-than-n ()
  "When history has fewer entries than N, all entries are returned (no
padding), still oldest-first."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-collect-context-fewer*"
      (setq-local agent-repl--input-history '("newest" "older"))
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (let ((agent-repl-prompt-summary-context-count 5))
        (should (equal (agent-repl--prompt-summary-collect-context "ws1")
                       '("older" "newest")))))))

(ert-deftest agent-repl-test-prompt-summary-collect-context-no-input-buffer ()
  "Collect returns nil when WS has no input buffer registered."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-prompt-summary-context-count 3))
      (should-not (agent-repl--prompt-summary-collect-context "ws-missing")))))

(ert-deftest agent-repl-test-prompt-summary-collect-context-dead-buffer ()
  "Collect tolerates a dead input buffer (returns nil)."
  (agent-repl-test--with-clean-state
    (let ((dead (generate-new-buffer "*agent-panel-collect-context-dead*")))
      (kill-buffer dead)
      (agent-repl--ws-put "ws1" :input-buffer dead)
      (let ((agent-repl-prompt-summary-context-count 3))
        (should-not (agent-repl--prompt-summary-collect-context "ws1"))))))

(ert-deftest agent-repl-test-prompt-summary-collect-context-disabled-when-zero ()
  "When context-count is zero, collect returns nil even with history present."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-collect-context-zero*"
      (setq-local agent-repl--input-history '("newest"))
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (let ((agent-repl-prompt-summary-context-count 0))
        (should-not (agent-repl--prompt-summary-collect-context "ws1"))))))

(ert-deftest agent-repl-test-prompt-summary-collect-context-empty-history ()
  "Collect returns nil when input history is empty (just-created buffer)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-collect-context-empty*"
      (setq-local agent-repl--input-history nil)
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (let ((agent-repl-prompt-summary-context-count 3))
        (should-not (agent-repl--prompt-summary-collect-context "ws1"))))))

;;;; ---- Tests: spawn wires collected context into the rendered prompt ----

(ert-deftest agent-repl-test-prompt-summary-spawn-sends-context-from-history ()
  "Spawn must collect context from the workspace's input history and
include the prior prompts in the data sent to the model.  Pins the
end-to-end wiring from history → collect-context → build-input → process
stdin."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-spawn-context*"
      (setq-local agent-repl--input-history '("prior prompt one"))
      (agent-repl--ws-put "ws1" :input-buffer (current-buffer))
      (let ((captured-input nil)
            (agent-repl-prompt-summary-context-count 3))
        (cl-letf (((symbol-function 'agent-repl--prompt-summary-process-start)
                   (lambda (&rest _) (make-marker)))
                  ((symbol-function 'agent-repl--prompt-summary-process-send-input)
                   (lambda (_proc s) (setq captured-input s)))
                  ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
          (agent-repl--prompt-summary-spawn "ws1" "current raw prompt"))
        (should (stringp captured-input))
        (should (string-match-p "\\[1\\] prior prompt one" captured-input))
        (should (string-match-p "current raw prompt" captured-input))))))

(ert-deftest agent-repl-test-prompt-summary-spawn-cmd-from-ws-backend ()
  "Spawn builds its command via the workspace's resolved agent backend,
so a codex workspace summarizes via codex rather than the claude binary."
  ;; Private registry copy so the test's throwaway backend does not leak
  ;; into the module-level registry (which holds the real claude backend).
  (let ((agent-repl--backends (copy-hash-table agent-repl--backends)))
    (agent-repl-test--with-clean-state
      (agent-repl-register-backend
       (agent-repl-backend-create
        :name 'ps-backend :binary "ps-bin"
        :start-cmd-fn #'ignore
        :headless-cmd-fn (lambda (model _extra) (list "ps-bin" "run" model))))
      (agent-repl--ws-put "ws1" :project-dir "/tmp/p")
      (agent-repl--ws-put "ws1" :backend 'ps-backend)
      (let ((captured-cmd nil)
            (agent-repl-prompt-summary-model "some-model"))
        (cl-letf (((symbol-function 'agent-repl--prompt-summary-process-start)
                   (lambda (_ws _out-buf cmd _sentinel)
                     (setq captured-cmd cmd)
                     (make-marker)))
                  ((symbol-function 'agent-repl--prompt-summary-process-send-input)
                   (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
          (agent-repl--prompt-summary-spawn "ws1" "a raw prompt long enough"))
        (should (equal captured-cmd '("ps-bin" "run" "some-model")))))))

;;;; ---- Tests: prompt-summary-spawn cwd ----

(ert-deftest agent-repl-test-prompt-summary-spawn-binds-temporary-default-directory ()
  "Spawn must invoke the process-start wrapper with `default-directory' rebound to
`temporary-file-directory'.  Without this, the headless claude inherits
the calling workspace's project-dir, its hooks fire with that cwd, and
the sentinel watcher misattributes them — flipping :agent-state to :done
while the user's interactive Claude is still mid-turn."
  (let ((captured-cwd nil))
    (cl-letf (((symbol-function 'agent-repl--prompt-summary-process-start)
               (lambda (&rest _)
                 (setq captured-cwd default-directory)
                 (make-marker)))
              ((symbol-function 'agent-repl--prompt-summary-process-send-input)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--log) (lambda (&rest _) nil)))
      (agent-repl--prompt-summary-spawn "ws1" "some raw prompt that is long enough")
      (should (equal (file-name-as-directory captured-cwd)
                     (file-name-as-directory temporary-file-directory))))))

;;;; ---- Tests: apply-prompt-summary (writeback + stale-drop) ----

(ert-deftest agent-repl-test-apply-prompt-summary-writes-when-raw-matches ()
  "Apply writes the summary when WS's :last-prompt-text still matches RAW."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :last-prompt-text "the prompt")
    (agent-repl--ws-put "ws1" :last-prompt-summary-pending t)
    (cl-letf (((symbol-function 'agent-repl--prompt-summary-redisplay)
               #'ignore))
      (agent-repl--prompt-summary-apply "ws1" "the prompt" "Short Title")
      (should (equal (agent-repl--ws-get "ws1" :last-prompt-summary)
                     "Short Title"))
      (should-not (agent-repl--ws-get "ws1" :last-prompt-summary-pending)))))

(ert-deftest agent-repl-test-apply-prompt-summary-drops-stale ()
  "Apply drops the summary when a newer prompt has overwritten the bookmark."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :last-prompt-text "the new prompt")
    (agent-repl--ws-put "ws1" :last-prompt-summary "Existing")
    (agent-repl--ws-put "ws1" :last-prompt-summary-pending t)
    (cl-letf (((symbol-function 'agent-repl--prompt-summary-redisplay)
               #'ignore))
      (agent-repl--prompt-summary-apply "ws1" "the OLD prompt" "Stale Title")
      (should (equal (agent-repl--ws-get "ws1" :last-prompt-summary) "Existing"))
      ;; Pending also unchanged — stale path doesn't touch it
      (should (agent-repl--ws-get "ws1" :last-prompt-summary-pending)))))

(ert-deftest agent-repl-test-apply-prompt-summary-redisplays-frontend-buffer ()
  "Apply forces a mode-line update on the workspace's frontend (webview)
buffer.  `--prompt-summary-redisplay' reads `:frontend-buffer' now that
`:vterm-buffer' no longer exists."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-apply-redisp*"
      (agent-repl--ws-put "ws1" :frontend-buffer (current-buffer))
      (agent-repl--ws-put "ws1" :last-prompt-text "raw")
      (let ((called nil))
        (cl-letf (((symbol-function 'force-mode-line-update)
                   (lambda (&rest _) (setq called t))))
          (agent-repl--prompt-summary-apply "ws1" "raw" "Title")
          (should called))))))

;;;; ---- Tests: gui send-turn invokes kickoff ----

(ert-deftest agent-repl-test-gui-send-turn-kicks-off-prompt-summary ()
  "`agent-repl--gui-send-turn' invokes `agent-repl--kickoff-prompt-summary'
with the workspace and raw input.

This is the send path's actual entry point into the prompt-summary
feature now that the vterm frontend is gone: `agent-repl--do-send'
(input.el) is a pure one-line delegation to
`agent-repl--frontend-dispatch-send', which resolves the workspace's
registered frontend and calls its `:send-fn' — for the (only
remaining) gui frontend, that is `agent-repl--gui-send-turn'
\(frontend-client.el), which is where the kickoff call now lives."
  (agent-repl-test--with-clean-state
    (let ((kickoff-args nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-send-user-message)
                  (lambda (_ws _text ok _fail) (funcall ok "req") :pending))
                 ((symbol-function 'agent-repl--run-send-posthooks) #'ignore)
                ((symbol-function 'agent-repl--kickoff-prompt-summary)
                 (lambda (ws raw) (setq kickoff-args (list ws raw)))))
        (agent-repl--gui-send-turn "ws1" "decorated-input" "raw-input"))
      (should (equal kickoff-args '("ws1" "raw-input"))))))

;;;; ---- Tests: mode-line migration (attach-to-mode-line) ----

(ert-deftest agent-repl-test-attach-to-mode-line-appends-when-missing ()
  "attach-to-mode-line appends the :eval segment when not already present."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-attach-missing*"
      (setq-local mode-line-format (list "BARE METAL: host"))
      (agent-repl--prompt-summary-attach-to-mode-line (current-buffer))
      (should (= (length mode-line-format) 2))
      (should (equal (car (last mode-line-format))
                     agent-repl--prompt-summary-mode-line-spec)))))

(ert-deftest agent-repl-test-attach-to-mode-line-idempotent ()
  "attach-to-mode-line does not double-append when called twice."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-attach-idempotent*"
      (setq-local mode-line-format
                  (list "BARE METAL: host"
                        agent-repl--prompt-summary-mode-line-spec))
      (agent-repl--prompt-summary-attach-to-mode-line (current-buffer))
      (agent-repl--prompt-summary-attach-to-mode-line (current-buffer))
      (should (= (length mode-line-format) 2)))))

(ert-deftest agent-repl-test-attach-to-mode-line-skips-non-list ()
  "attach-to-mode-line leaves string mode-line-formats alone."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-attach-string*"
      (setq-local mode-line-format "literal-string")
      (agent-repl--prompt-summary-attach-to-mode-line (current-buffer))
      (should (equal mode-line-format "literal-string")))))

(ert-deftest agent-repl-test-attach-all-walks-workspaces ()
  "attach-all attaches the segment to every live workspace vterm buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-attach-all-1*"
      (setq-local mode-line-format (list "BARE METAL: a"))
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (agent-repl-test--with-temp-buffer "*agent-panel-attach-all-2*"
        (setq-local mode-line-format (list "BARE METAL: b"))
        (agent-repl--ws-put "ws2" :vterm-buffer (current-buffer))
        (agent-repl-prompt-summary-attach-all)
        (with-current-buffer "*agent-panel-attach-all-1*"
          (should (member agent-repl--prompt-summary-mode-line-spec
                          mode-line-format)))
        (with-current-buffer "*agent-panel-attach-all-2*"
          (should (member agent-repl--prompt-summary-mode-line-spec
                          mode-line-format)))))))

(ert-deftest agent-repl-test-attach-all-skips-dead-buffer ()
  "attach-all tolerates dead vterm buffers in the workspace table."
  (agent-repl-test--with-clean-state
    (let ((dead-buf (generate-new-buffer "*agent-panel-attach-dead*")))
      (kill-buffer dead-buf)
      (agent-repl--ws-put "ws-dead" :vterm-buffer dead-buf)
      ;; Should not signal.
      (agent-repl-prompt-summary-attach-all))))

(provide 'test-prompt-summary)
;;; test-prompt-summary.el ends here
