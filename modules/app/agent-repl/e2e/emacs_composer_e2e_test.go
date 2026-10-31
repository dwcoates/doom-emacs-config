package e2e

import (
	"encoding/json"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/integration/harness"
)

// AREA D of EMACS-LAYER-SPEC.md: THE COMPOSER AND PROMPT SUBMISSION.
//
// Scenario 20 (SubmitFromComposerYieldsAResponseRow) is NOT here: it is the
// layer's proof-of-life test and lives in `emacs_e2e_test.go` as
// TestEmacsProofOfLife, per the spec's "The proof-of-life test" section.
// This file carries scenarios 21 through 25, plus 25a.
//
// Every scenario here drives an ORDINARY interactive command through
// `call-interactively` (or presses its key), and reads what Emacs actually
// SUBMITTED back as data. The submitted text is observed at the module's own
// outbound RPC boundary rather than in the daemon's feed row, and that is
// deliberate: `agent-repl--prepare-input`'s docstring states the metaprompt
// span is meta-wrapped precisely SO THAT the daemon strips it from the drawn
// row. The drawn row is therefore the one place scenario 21's claim cannot
// be seen. The daemon's own acceptance is still asserted — the turn runs and
// the roster settles — so a green test still means the daemon took what
// Emacs sent.

// ---------------------------------------------------------------------------
// Area-local scaffolding
// ---------------------------------------------------------------------------

// emacsScenario is one booted Emacs with a daemon it launched itself, one
// registered workspace, and that workspace's panel open — the state every
// scenario in areas D and E starts from.
type emacsScenario struct {
	W *EmacsWorld
	E *Emacs

	// Name is the workspace name Emacs's own registry holds.
	Name string
	// Input is the name of that workspace's composer buffer.
	Input string
	// Repo is the scripted fake-git worktree the workspace was registered
	// against. NO REAL GIT RUNS: harness.NewRepoAt scripts the fake.
	Repo *harness.Repo
}

// newEmacsScenario brings the layer up to the point every area-D and area-E
// scenario begins at, driving only ordinary commands.
func newEmacsScenario(t *testing.T, options ...EmacsWorldOption) *emacsScenario {
	t.Helper()
	box := requireSandbox(t)
	w := NewEmacsWorld(t, box, options...)
	e := w.Emacs

	// EMACS spawns the daemon, through the module's own launcher.
	e.EnsureDaemon()

	repository := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "repo"))
	name := addProjectWorkspace(t, e, repository.Dir)

	// The panel is what materializes the composer buffer, so it is part of
	// the shared arrangement rather than a per-scenario act.
	e.Eval(`(agent-repl-frontend-open-panel)`)
	input := awaitInputBuffer(t, e, name)

	return &emacsScenario{W: w, E: e, Name: name, Input: input, Repo: repository}
}

// addProjectWorkspace registers DIR through `agent-repl-add-project-workspace'
// (`SPC TAB p') and returns the name Emacs's registry ended up holding.
//
// The name is READ BACK rather than predicted: the DAEMON mints the identity
// and Emacs echoes it, so a test that composed the name itself would be
// asserting its own arithmetic.
func addProjectWorkspace(t *testing.T, e *Emacs, dir string) string {
	t.Helper()
	before := decodeStrings(e.Eval(workspaceRegistryNamesForm))
	e.Eval(`(agent-repl-add-project-workspace ` + elispString(dir) + `)`)
	raw := e.AwaitEvalFor(emacsVerbBound,
		"the workspace to appear in Emacs's registry",
		workspaceRegistryNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == len(before)+1 })
	for _, name := range decodeStrings(raw) {
		if !contains(before, name) {
			// Registry publication precedes the landing that selects the new
			// workspace. Callers open its panel immediately, so wait for the
			// command's complete user-visible outcome instead of racing that edge.
			e.AwaitEvalFor(emacsVerbBound,
				"the newly registered workspace to become current",
				`(agent-repl--ws-current-name)`,
				func(raw json.RawMessage) bool { return decodeString(raw) == name })
			return name
		}
	}
	t.Fatalf("e2e: no new workspace name appeared for %s", dir)
	return ""
}

func contains(xs []string, want string) bool {
	for _, x := range xs {
		if x == want {
			return true
		}
	}
	return false
}

// awaitInputBuffer waits for the workspace's composer buffer to exist and
// returns its name.
func awaitInputBuffer(t *testing.T, e *Emacs, ws string) string {
	t.Helper()
	form := `(let ((buf (agent-repl--input-buffer ` + elispString(ws) + `)))
                   (if buf (buffer-name buf) ""))`
	raw := e.AwaitEvalFor(emacsBootBound, "the composer buffer to appear", form,
		func(raw json.RawMessage) bool { return decodeString(raw) != "" })
	return decodeString(raw)
}

func decodeString(raw json.RawMessage) string {
	if isJSONNull(raw) {
		return ""
	}
	var s string
	if err := json.Unmarshal(raw, &s); err != nil {
		return ""
	}
	return s
}

// ---------------------------------------------------------------------------
// The submission observer
// ---------------------------------------------------------------------------

// submissionSeparator joins a captured submission's origin, its delivery and
// its text. A unit separator cannot appear in composed prompt text.
const submissionSeparator = "\x1f"

// armSubmissionObserver advises the module's OWN outbound RPC verb,
// `agent-repl-rpc-submit-prompt', to record what Emacs submitted.
//
// This is an observer on a public contract boundary — the same shape as
// hooking `agent-repl-roster-update-functions' — not a reach into an
// internal. It is the only place scenario 21's claim is observable at all
// (see this file's header).
func armSubmissionObserver(t *testing.T, e *Emacs) {
	t.Helper()
	e.Eval(`(progn
             (defvar agent-repl-e2e--sent nil)
             (setq agent-repl-e2e--sent nil)
             (defun agent-repl-e2e--said-text (said)
               (mapconcat (lambda (block)
                            (or (plist-get (plist-get block :value) :text) ""))
                          (plist-get (plist-get said :content) :blocks)
                          ""))
             (defun agent-repl-e2e--record-send (_conn request &rest _)
               (push (concat (format "%s" (plist-get request :origin))
                             "` + submissionSeparator + `"
                             (let ((delivery (plist-get request :delivery)))
                               (if delivery (format "%s" delivery) ""))
                             "` + submissionSeparator + `"
                             (agent-repl-e2e--said-text (plist-get request :said)))
                     agent-repl-e2e--sent))
             (unless (advice-member-p 'agent-repl-e2e--record-send
                                      'agent-repl-rpc-submit-prompt)
               (advice-add 'agent-repl-rpc-submit-prompt :before
                           #'agent-repl-e2e--record-send))
             t)`)
}

// submission is one observed SubmitPrompt, split back into its parts.
// Delivery is the request's `:delivery' keyword as printed (":deferred"),
// empty for the ordinary delivery, which rides no `:delivery' key at all.
type submission struct {
	Origin   string
	Delivery string
	Text     string
}

// awaitSubmissions waits until exactly n submissions have been observed and
// returns them oldest-first.
func awaitSubmissions(t *testing.T, e *Emacs, n int, what string) []submission {
	t.Helper()
	raw := e.AwaitEval(what, `(reverse agent-repl-e2e--sent)`,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) >= n })
	lines := decodeStrings(raw)
	if len(lines) != n {
		t.Fatalf("observed %d submissions, want exactly %d: %q", len(lines), n, lines)
	}
	out := make([]submission, 0, len(lines))
	for _, line := range lines {
		origin, rest, found := strings.Cut(line, submissionSeparator)
		if !found {
			t.Fatalf("malformed observed submission %q", line)
		}
		delivery, text, found := strings.Cut(rest, submissionSeparator)
		if !found {
			t.Fatalf("malformed observed submission %q", line)
		}
		out = append(out, submission{Origin: origin, Delivery: delivery, Text: text})
	}
	return out
}

// observedSubmissionCount reads the observer's count without waiting, for the
// negative assertion scenario 23 makes.
func observedSubmissionCount(e *Emacs) int {
	return e.EvalInt(`(length agent-repl-e2e--sent)`)
}

// armSubmissionAnswerObserver advises the same public RPC boundary as
// `armSubmissionObserver' to record how the DAEMON ANSWERED each submission:
// the response's `:arm' (":success" or ":error"), or ":failure" when the
// transport failed. Each record is the submitted text and the answer, and
// it is pushed only AFTER the module's own callback has run, so observing
// an answer means everything the module does on it (an ingress write on a
// refusal, say) is already done. It depends on the helper
// `agent-repl-e2e--said-text' that `armSubmissionObserver' defines, so arm
// that one first.
func armSubmissionAnswerObserver(t *testing.T, e *Emacs) {
	t.Helper()
	e.Eval(`(progn
             (defvar agent-repl-e2e--answered nil)
             (setq agent-repl-e2e--answered nil)
             (defun agent-repl-e2e--record-answer (orig conn request &rest keys)
               (let ((on-response (plist-get keys :on-response))
                     (on-failure (plist-get keys :on-failure))
                     (text (agent-repl-e2e--said-text (plist-get request :said))))
                 (apply orig conn request
                        :on-response
                        (lambda (response)
                          (unwind-protect
                              (when on-response (funcall on-response response))
                            (push (concat text "` + submissionSeparator + `"
                                          (format "%s" (plist-get response :arm)))
                                  agent-repl-e2e--answered)))
                        :on-failure
                        (lambda (detail)
                          (unwind-protect
                              (when on-failure (funcall on-failure detail))
                            (push (concat text "` + submissionSeparator + `:failure")
                                  agent-repl-e2e--answered)))
                        keys)))
             (unless (advice-member-p 'agent-repl-e2e--record-answer
                                      'agent-repl-rpc-submit-prompt)
               (advice-add 'agent-repl-rpc-submit-prompt :around
                           #'agent-repl-e2e--record-answer))
             t)`)
}

// awaitSubmissionAnswer waits until the daemon's answer to the submission
// of TEXT has been observed, and returns it (":success", ":error" or
// ":failure").
func awaitSubmissionAnswer(t *testing.T, e *Emacs, text, what string) string {
	t.Helper()
	find := func(lines []string) (string, bool) {
		for _, line := range lines {
			if got, arm, found := strings.Cut(line, submissionSeparator); found && got == text {
				return arm, true
			}
		}
		return "", false
	}
	raw := e.AwaitEval(what, `agent-repl-e2e--answered`, func(raw json.RawMessage) bool {
		_, ok := find(decodeStrings(raw))
		return ok
	})
	arm, _ := find(decodeStrings(raw))
	return arm
}

// typeIntoComposer puts TEXT in the composer the way a user's typing leaves
// it: the buffer's whole contents, nothing else.
func typeIntoComposer(e *Emacs, buffer, text string) {
	e.Eval(`(with-current-buffer ` + elispString(buffer) + `
              (erase-buffer)
              (insert ` + elispString(text) + `)
              t)`)
}

// runInComposer invokes COMMAND as a command, with the composer current —
// which is where a user invoking it stands.
func runInComposer(e *Emacs, buffer, command string) {
	e.Eval(`(with-current-buffer ` + elispString(buffer) + `
              (call-interactively #'` + command + `)
              t)`)
}

// composerText reads the composer's contents back as data.
func composerText(e *Emacs, buffer string) string {
	return e.EvalString(`(with-current-buffer ` + elispString(buffer) + ` (buffer-string))`)
}

// ---------------------------------------------------------------------------
// 21. MetapromptIsComposedBeforeSubmission
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// 22. PrefixAndPostfixSendVariants
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// 23. DiscardInputClearsTheComposer
// ---------------------------------------------------------------------------

// TestEmacsDiscardInputClearsTheComposer is scenario 23: the composer empties
// and NOTHING was submitted.
//
// The negative half needs no probe window: `agent-repl-discard-input' is
// synchronous and returns before the readback, so a submission it made would
// already be in the observer. The observer is proved live by every other
// scenario in this file.
func TestEmacsDiscardInputClearsTheComposer(t *testing.T) {
	t.Parallel()
	s := newEmacsScenario(t)
	e := s.E
	armSubmissionObserver(t, e)
	typeIntoComposer(e, s.Input, "this draft is going nowhere")

	runInComposer(e, s.Input, "agent-repl-discard-input")

	if got := composerText(e, s.Input); got != "" {
		t.Errorf("the composer still holds %q after a discard, want it empty", got)
	}
	if got := observedSubmissionCount(e); got != 0 {
		t.Errorf("a discard submitted %d prompts, want none", got)
	}
}

// ---------------------------------------------------------------------------
// 24. DeferredPromptIsSubmittedDeferredAtOnce
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// 25. HistoryRecallRestoresTheLastPrompt
// ---------------------------------------------------------------------------

// TestEmacsHistoryRecallRestoresTheLastPrompt is scenario 25.
//
// History is pushed on ACCEPTANCE, not on the keystroke, so the recall waits
// on the composer being cleared — which is the acceptance's own visible act —
// rather than on a duration.
func TestEmacsHistoryRecallRestoresTheLastPrompt(t *testing.T) {
	t.Parallel()
	s := newEmacsScenario(t)
	e := s.E

	const submitted = "remember this one for the history"
	typeIntoComposer(e, s.Input, submitted)
	e.KeysIn(s.Input, "RET")

	e.AwaitEval("the daemon's acceptance to clear the composer",
		`(with-current-buffer `+elispString(s.Input)+` (buffer-string))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "" })

	runInComposer(e, s.Input, "agent-repl--history-prev")

	if got := composerText(e, s.Input); got != submitted {
		t.Errorf("history recall put %q in the composer, want the previously submitted %q",
			got, submitted)
	}
}

// installArmObserver defines `agent-repl-e2e--arm-of', which answers a
// workspace's current status arm out of the roster's own indexes.
//
// It reads `agent-repl-roster--rows-by-id' and resolves the id through the
// module's own `agent-repl--ws-by-ref-id', per the spec's readback table.
// Area E uses it too, which is why it is a function rather than an inline
// form.
func installArmObserver(t *testing.T, e *Emacs) {
	t.Helper()
	e.Eval(`(progn
             (defun agent-repl-e2e--arm-of (ws)
               (let (arm)
                 (maphash (lambda (id row)
                            (when (equal (agent-repl--ws-by-ref-id id) ws)
                              (setq arm (agent-repl-roster-row-status row))))
                          agent-repl-roster--rows-by-id)
                 arm))
             t)`)
}

// connectSession drives ONE warm-up turn from the composer and returns once
// the workspace's row has settled on `:done', so the scenario's own turn runs
// on a session that is up AND connected on all three hops.
//
// WHY A TURN AND NOT A WAIT. Before a session exists the daemon has seen no
// link, so neither the footer nor the topbar can claim a hop down: an open
// webview whose web stream has not yet attached reads exactly as one that
// has. Under load the fake SDK's turn (~50ms) can run and end inside that
// window, and every running arm it would have published is outranked by the
// `severed' the missing web hop draws (daemon.md invariant 11). `:done' is
// drawn only below that rung, so it is the latch: it stands only once the
// link is seen and both the host and the web streams are held.
//
// The arm is read off the module's own per-push hook rather than polled,
// because `:done' is not the row's resting arm: viewing the result moves it.
func connectSession(t *testing.T, s *emacsScenario) {
	t.Helper()
	e := s.E
	installArmObserver(t, e)
	e.Eval(`(progn
             (defvar agent-repl-e2e--warm-arms nil)
             (setq agent-repl-e2e--warm-arms nil)
             (defun agent-repl-e2e--record-warm-arm (_roster)
               (push (agent-repl-e2e--arm-of ` + elispString(s.Name) + `) agent-repl-e2e--warm-arms))
             (add-hook 'agent-repl-roster-update-functions #'agent-repl-e2e--record-warm-arm)
             t)`)
	typeIntoComposer(e, s.Input, "bring the session up")
	e.KeysIn(s.Input, "RET")
	e.AwaitTrue("the warm-up turn to settle on a connected session",
		`(and (memq :done agent-repl-e2e--warm-arms) t)`)
	e.Eval(`(progn
             (remove-hook 'agent-repl-roster-update-functions #'agent-repl-e2e--record-warm-arm)
             t)`)
}

// ---------------------------------------------------------------------------
// 25a. AttachedImageMarkerNeverRidesAsWords
// ---------------------------------------------------------------------------
