package e2e

import (
	"encoding/json"
	"image"
	"image/color"
	"image/png"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"claude-repld/integration/harness"
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
// (`SPC TAB C-n') and returns the name Emacs's registry ended up holding.
//
// The name is READ BACK rather than predicted: the DAEMON mints the identity
// and Emacs echoes it, so a test that composed the name itself would be
// asserting its own arithmetic.
func addProjectWorkspace(t *testing.T, e *Emacs, dir string) string {
	t.Helper()
	before := decodeStrings(e.Eval(workspaceNamesForm))
	e.Eval(`(agent-repl-add-project-workspace ` + elispString(dir) + `)`)
	raw := e.AwaitEvalFor(emacsVerbBound,
		"the workspace to appear in Emacs's registry",
		workspaceNamesForm,
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

// workspaceNamesForm reads Emacs's own workspace registry as DATA, per the
// spec's readback table ("workspace registry <- agent-repl--workspaces").
const workspaceNamesForm = `(let (names) (maphash (lambda (k v) (when (plist-get v :project-dir) (push k names))) agent-repl--workspaces) names)`

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

// TestEmacsMetapromptIsComposedBeforeSubmission is scenario 21.
//
// `agent-repl-send-with-metaprompt' is the ONE send site that prepends the
// on-demand read-directive, and it brackets it with the meta sentinels so a
// frontend knows the span was injected rather than typed. The claim is that
// the composition happened BEFORE submission — which is blessed; "verbatim"
// forbids only post-submission rewriting — so the submitted text is what
// carries it.
func TestEmacsMetapromptIsComposedBeforeSubmission(t *testing.T) {
	t.Parallel()
	s := newEmacsScenario(t)
	e := s.E
	armSubmissionObserver(t, e)

	// The sentinels and the metaprompt path are read as DATA from the module
	// rather than restated here: a second spelling of either would be a
	// second contract.
	open := e.EvalString(`agent-repl--meta-open`)
	closing := e.EvalString(`agent-repl--meta-close`)
	metapromptFile := e.EvalString(`agent-repl-metaprompt-file`)

	const body = "please summarize the module"
	typeIntoComposer(e, s.Input, body)
	runInComposer(e, s.Input, "agent-repl-send-with-metaprompt")

	sent := awaitSubmissions(t, e, 1, "the metaprompt send to reach the RPC boundary")[0]

	if want := ":user-sent-with-metaprompt"; sent.Origin != want {
		t.Errorf("submitted origin is %q, want %q", sent.Origin, want)
	}
	if !strings.HasPrefix(sent.Text, open) {
		t.Fatalf("submitted text does not open with the meta sentinel %q:\n%s", open, sent.Text)
	}
	span, rest, found := strings.Cut(strings.TrimPrefix(sent.Text, open), closing)
	if !found {
		t.Fatalf("submitted text carries no closing meta sentinel %q:\n%s", closing, sent.Text)
	}
	if !strings.Contains(span, metapromptFile) {
		t.Errorf("the marked span does not name the metaprompt file %q:\n%s", metapromptFile, span)
	}
	if !strings.Contains(rest, body) {
		t.Errorf("the user's own words did not survive composition; text after the span:\n%s", rest)
	}
}

// ---------------------------------------------------------------------------
// 22. PrefixAndPostfixSendVariants
// ---------------------------------------------------------------------------

// TestEmacsPrefixAndPostfixSendVariants is scenario 22: the decoration lands
// on the CORRECT SIDE. One table row per send site, because the sides are
// the two edge cases and a single test asserting both would hide which one
// regressed.
func TestEmacsPrefixAndPostfixSendVariants(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		// command is the ordinary interactive send site.
		command string
		// origin is the PromptOrigin that site owns; the vocabulary is
		// closed and one production site exists per origin.
		origin string
		// decoration names the defcustom holding the decoration, read as
		// data so this test never restates its value.
		decoration string
		// want composes the expected submission from decoration and body.
		want func(decoration, body string) string
	}{
		{
			name:       "prefix leads the body",
			command:    "agent-repl-send-with-prefix",
			origin:     ":user-sent-with-prefix",
			decoration: "agent-repl-send-prefix",
			want:       func(decoration, body string) string { return decoration + body },
		},
		{
			name:       "postfix trails the body",
			command:    "agent-repl-send-with-postfix",
			origin:     ":user-sent-with-postfix",
			decoration: "agent-repl-send-postfix",
			want:       func(decoration, body string) string { return body + decoration },
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// ARRANGE.
			s := newEmacsScenario(t)
			e := s.E
			armSubmissionObserver(t, e)
			decoration := e.EvalString(tc.decoration)
			const body = "decorate this one"
			typeIntoComposer(e, s.Input, body)

			// ACT.
			runInComposer(e, s.Input, tc.command)

			// ASSERT.
			sent := awaitSubmissions(t, e, 1, "the decorated send to reach the RPC boundary")[0]
			if sent.Origin != tc.origin {
				t.Errorf("submitted origin is %q, want %q", sent.Origin, tc.origin)
			}
			if want := tc.want(decoration, body); sent.Text != want {
				t.Errorf("submitted text is\n%q\nwant\n%q", sent.Text, want)
			}
		})
	}
}

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

// TestEmacsDeferredPromptIsSubmittedDeferredAtOnce is scenario 24.
//
// Owner ruling 2026-09-28: Emacs holds no prompt in memory. A mid-turn
// `agent-repl-queue-deferred-prompt' clears the composer and SUBMITS AT
// ONCE, asking for `:delivery :deferred'; the DAEMON holds it and runs it as
// its own turn when the running one ends. So the claim here is made at the
// RPC boundary, not on a queue: the deferral left Emacs while the first turn
// was still running, deferred, under the deferral's own origin.
//
// The deferral must be issued WHILE the turn is running, and "while running"
// is not something a Go-side poll can hit without racing the turn's end. So
// it is armed INSIDE Emacs on `agent-repl-roster-update-functions', the
// module's own per-push hook: it fires the moment the workspace's row first
// shows a running arm, types into the composer, and invokes the ordinary
// command. That is deterministic by construction.
func TestEmacsDeferredPromptIsSubmittedDeferredAtOnce(t *testing.T) {
	t.Parallel()
	s := newEmacsScenario(t)
	e := s.E
	armSubmissionObserver(t, e)
	installArmObserver(t, e)

	const deferred = "and then tell me about the roster"

	// ARRANGE: arm the mid-turn deferral. `agent-repl-e2e--deferred-sent'
	// records how many submissions the observer held IMMEDIATELY after the
	// command returned, and `agent-repl-e2e--deferred-composer' what the
	// composer held then, both captured inside Emacs in the same command
	// loop turn, so nothing later can change them before the readback.
	e.Eval(`(progn
             (defvar agent-repl-e2e--deferred-sent nil)
             (defvar agent-repl-e2e--deferred-composer nil)
             (setq agent-repl-e2e--deferred-sent nil
                   agent-repl-e2e--deferred-composer nil)
             (defun agent-repl-e2e--defer-when-running (_roster)
               (when (and (null agent-repl-e2e--deferred-sent)
                          (memq (agent-repl-e2e--arm-of ` + elispString(s.Name) + `)
                                agent-repl-roster-running-statuses))
                 (with-current-buffer (agent-repl--input-buffer ` + elispString(s.Name) + `)
                   (erase-buffer)
                   (insert ` + elispString(deferred) + `)
                   (call-interactively #'agent-repl-queue-deferred-prompt)
                   (setq agent-repl-e2e--deferred-composer (buffer-string)))
                 (setq agent-repl-e2e--deferred-sent (length agent-repl-e2e--sent))))
             (add-hook 'agent-repl-roster-update-functions
                       #'agent-repl-e2e--defer-when-running)
             t)`)

	// ACT: drive one ordinary turn from the composer, the way a user does;
	// the hook defers mid-turn.
	typeIntoComposer(e, s.Input, "hello from the deferred-prompt scenario")
	e.KeysIn(s.Input, "RET")

	// ASSERT: the deferral reached the RPC boundary before the command
	// returned, and cleared the composer.
	e.AwaitTrue("the deferred prompt to be issued during the running turn",
		`agent-repl-e2e--deferred-sent`)
	if n := e.EvalInt(`(or agent-repl-e2e--deferred-sent 0)`); n != 2 {
		t.Fatalf("%d submissions had reached the RPC boundary when the deferral returned, want 2 (the turn and the deferral, at once)", n)
	}
	if got := e.EvalString(`(or agent-repl-e2e--deferred-composer "")`); got != "" {
		t.Errorf("the composer held %q after the deferral, want it cleared", got)
	}

	sent := awaitSubmissions(t, e, 2, "the turn and the deferral to reach the RPC boundary")
	if sent[0].Delivery != "" {
		t.Errorf("the ordinary send asked for delivery %q, want the ordinary delivery (no :delivery key)", sent[0].Delivery)
	}
	if sent[1].Text != deferred {
		t.Errorf("the deferred submission is %q, want the deferred text %q", sent[1].Text, deferred)
	}
	if sent[1].Delivery != ":deferred" {
		t.Errorf("the deferred submission asked for delivery %q, want :deferred", sent[1].Delivery)
	}
	origin := e.EvalString(`(format "%s" agent-repl--prompt-queue-drain-origin)`)
	if sent[1].Origin != origin {
		t.Errorf("the deferred submission's origin is %q, want the deferral's own %q",
			sent[1].Origin, origin)
	}
}

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

// ---------------------------------------------------------------------------
// 25a. AttachedImageMarkerNeverRidesAsWords
// ---------------------------------------------------------------------------

// armSaidObserver advises the module's OWN outbound RPC verb to record the
// WHOLE submitted `UserSaid' -- every block and its arm -- rather than the
// concatenated text `armSubmissionObserver' keeps.
//
// The composer's attachment marker is the one claim the concatenated form
// cannot make: the question is which BLOCK a run of characters ended up in,
// and a join over the blocks has already thrown that away.
func armSaidObserver(t *testing.T, e *Emacs) {
	t.Helper()
	e.Eval(`(progn
             (defvar agent-repl-e2e--said nil)
             (defvar agent-repl-e2e--said-printed nil)
             (setq agent-repl-e2e--said nil)
             (setq agent-repl-e2e--said-printed "")
             (defun agent-repl-e2e--record-said (_conn request &rest _)
               (setq agent-repl-e2e--said (plist-get request :said))
               (setq agent-repl-e2e--said-printed
                     (format "%S" (plist-get request :said))))
             (defun agent-repl-e2e--said-blocks ()
               (plist-get (plist-get agent-repl-e2e--said :content) :blocks))
             (defun agent-repl-e2e--said-field (n field)
               (let* ((block (nth n (agent-repl-e2e--said-blocks)))
                      (value (plist-get block :value)))
                 (pcase field
                   (:arm (format "%s" (plist-get block :arm)))
                   (:text (or (plist-get value :text) ""))
                   (:media-type (or (plist-get value :media-type) ""))
                   (:path (or (plist-get (plist-get (plist-get value :location) :value)
                                         :path)
                              "")))))
             (unless (advice-member-p 'agent-repl-e2e--record-said
                                      'agent-repl-rpc-submit-prompt)
               (advice-add 'agent-repl-rpc-submit-prompt :before
                           #'agent-repl-e2e--record-said))
             t)`)
}

// saidField reads one field of the Nth block of the recorded `UserSaid'.
func saidField(e *Emacs, n int, field string) string {
	return e.EvalString(`(agent-repl-e2e--said-field ` + strconv.Itoa(n) + ` ` + field + `)`)
}

// writeScenarioPNG writes a real PNG at PATH, so the attachment names a file
// that exists the way a capture leaves one. NOTHING SHELLS OUT: the bytes are
// encoded here, the way every other fixture in this suite is scripted.
func writeScenarioPNG(t *testing.T, path string) {
	t.Helper()
	img := image.NewRGBA(image.Rect(0, 0, 2, 2))
	for x := 0; x < 2; x++ {
		for y := 0; y < 2; y++ {
			img.Set(x, y, color.RGBA{R: 0x8b, G: 0x5c, B: 0xf6, A: 0xff})
		}
	}
	file, err := os.Create(path)
	if err != nil {
		t.Fatalf("e2e: create fixture png %s: %v", path, err)
	}
	defer file.Close()
	if err := png.Encode(file, img); err != nil {
		t.Fatalf("e2e: encode fixture png %s: %v", path, err)
	}
}

// TestEmacsAttachedImageMarkerNeverRidesAsWords is scenario 25a.
//
// `agent-repl--image-insert-marker' draws `[image attached: NAME]' so the
// user can SEE what they attached, and clipboard-image.el's own contract is
// that "nothing about the image rides the words": the image travels as its
// own `ImageBlock{path, media_type}', stated by ARM, and the agent is never
// left inferring an attachment from a filename in the prose.
//
// The marker used to travel inside the text block anyway, because the
// composer read its buffer verbatim. The claim is only observable in the
// SUBMITTED `UserSaid' -- the drawn feed row shows one bubble either way --
// so it is read at the module's own RPC boundary, and the daemon's
// acceptance is still awaited through the composer clearing.
func TestEmacsAttachedImageMarkerNeverRidesAsWords(t *testing.T) {
	t.Parallel()
	// ARRANGE.
	s := newEmacsScenario(t)
	e := s.E
	armSaidObserver(t, e)

	// The capture directory is the MODULE's own answer, not a path spelled
	// twice: a second spelling would be a second contract.
	imageDir := e.EvalString(`(agent-repl--image-dir ` + elispString(s.Name) + `)`)
	if imageDir == "" {
		t.Fatal("e2e: the module reported no capture directory for the workspace")
	}
	imagePath := filepath.Join(imageDir, "clip-e2e.png")
	writeScenarioPNG(t, imagePath)

	const words = "what is in this picture?"
	e.Eval(`(with-current-buffer ` + elispString(s.Input) + `
              (erase-buffer)
              (insert ` + elispString(words) + `)
              (agent-repl-input-attach-image ` + elispString(imagePath) + ` "image/png")
              (agent-repl--image-insert-marker ` + elispString(imagePath) + ` ` +
		elispString(s.Name) + `)
              t)`)
	if got := composerText(e, s.Input); !strings.Contains(got, "[image attached") {
		t.Fatalf("the composer draws no attachment marker, so the scenario proves nothing:\n%q", got)
	}

	// ACT: the composer's own RET, the way a user submits.
	if want, got := "agent-repl-send", e.BindingForIn(s.Input, "RET"); got != want {
		t.Fatalf("composer RET resolves to %q, want %q", got, want)
	}
	e.KeysIn(s.Input, "RET")

	// ASSERT. The daemon's acceptance is what clears the composer.
	e.AwaitEval("the daemon's acceptance to clear the composer",
		`(with-current-buffer `+elispString(s.Input)+` (buffer-string))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "" })

	if got := e.EvalInt(`(length (agent-repl-e2e--said-blocks))`); got != 2 {
		t.Fatalf("the submission carried %d blocks, want the words and the image", got)
	}
	if got := saidField(e, 0, ":arm"); got != ":text" {
		t.Errorf("the first block's arm is %q, want %q", got, ":text")
	}
	if got := saidField(e, 0, ":text"); got != words {
		t.Errorf("the submitted text is\n%q\nwant exactly the typed words\n%q", got, words)
	}
	if got := saidField(e, 1, ":arm"); got != ":image" {
		t.Errorf("the second block's arm is %q, want %q", got, ":image")
	}
	if got := saidField(e, 1, ":path"); got != imagePath {
		t.Errorf("the image block names %q, want the attached file %q", got, imagePath)
	}
	if got := saidField(e, 1, ":media-type"); got != "image/png" {
		t.Errorf("the image block's media type is %q, want %q", got, "image/png")
	}
	// The whole UserSaid, printed: the marker is nowhere in it, not in the
	// text block and not smuggled into any other one.
	printed := e.EvalString(`agent-repl-e2e--said-printed`)
	if strings.Contains(printed, "[image attached") {
		t.Errorf("the attachment marker rode along inside the submitted UserSaid:\n%s", printed)
	}
}
