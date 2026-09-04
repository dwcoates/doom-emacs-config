// detachedbash_e2e_test.go — SPEC.md §C "Detached bash" (#31-38): real bash
// spools written by the fake SDK's `src/fake/scenarios/shell.ts`, tailed by
// the REAL sidecar (never a test), surfacing as daemon FeedRow frames over
// the real agentrepl.v1 wire.
//
// Grounding for every prompt driven and every shape asserted here:
//   - agent-shim/claude/shim/src/fake/scenarios/shell.ts — the mocked
//     vendor's own scenario declarations (prompt, emits, writes, arms).
//   - agent-shim/claude/shim/test/fake/golden-conformance.test.ts — the
//     capture-name -> mock-prompt mapping. SPEC.md's and
//     E2E-EVENT-INVENTORY.md's "golden scenario" names ARE THE CAPTURE
//     DIRECTORY NAMES, which are not always the literal prompt text (e.g.
//     the golden "bash-detached" is driven by the prompt "!bash-detach";
//     "bash-foreground-completed" by plain "!bash"; "bash-nonzero-exit" by
//     "!bash-fail"; "bash-image-output" by "!bash-image";
//     "bash-partial-output-with-spill" by "!bash-spill"). Using the capture
//     name itself as "!"+scenario would submit a prompt the fake SDK does
//     not recognize.
//   - proto/src/conversation/v1/agent_activity.proto (the AgentBash* family:
//     AgentBashStart/Update/Success/Failure, AgentBashOutput's three forms).
//   - proto/src/conversation/v1/detached_work.proto (AgentDetachedWork, the
//     DetachedWorkId minting rule).
//   - proto/src/frontend/v1/feed.proto (FeedRow.detached_shell/FeedShell for
//     the detached shape, FeedSimpleToolCall/FeedToolCallReturned for the
//     foreground shape).
//   - proto/src/frontend/v1/topbar.proto (TopbarUnmodeledToolWarningDetail).
//   - daemon/internal/resolve/feed/{toolcall.go,subagent.go} and
//     daemon/internal/resolve/topbar/{resolver.go,warnings.go} — read ONLY
//     to confirm the EXACT wire shape these facts settle into, per SPEC.md's
//     own license for this area ("confirm the exact emitted
//     FeedToolCallReturned.form when writing" for #37) — never to hunt
//     behavior bugs, and never adapted to a defect found this way (none
//     was; every fact below is confirmed production behavior, not a
//     workaround for one).
//
// AGENT vs SHELL spool termination (settled fact, binding on this file, per
// the dispatcher's own brief): only a SHELL spool is terminated by
// `EXIT=<code>` (sidecar.md, "Discovery scope" — "the detached-shell spool
// is a delta stream terminated by its `EXIT=<code>` line"). An AGENT
// transcript spool carries no such terminator. Every scenario driven in this
// file is a SHELL (`b*`-prefixed task id) — every settled assertion below
// expects an exit code; none of them is, or should be read as, an assertion
// about an agent spool's termination.
//
// #33 (CtrlBDetachOfForegroundWork) and #34 (CtrlBDetachOfForegroundSubagent)
// are written up to the point the real wire can reach and then t.Skip, per
// docs/overhaul/PROTO-CHANGES.md's "Landing 8" RULED-no-proto paragraph:
// Ctrl-b detach of foreground work has no daemon verb, is out of scope for
// this overhaul, and is recorded as a follow-up — the two tests stay
// skipped pointing there, not describing an open, undecided gap.
package e2e

import (
	"context"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// dbWorkspace registers a fresh workspace against a fresh scripted-git
// repo, the same pattern every area file in this suite uses (harness.NewRepo
// backs the daemon's OWN git facts with the scripted fake, which this file
// never inspects — these scenarios never touch git at all).
func dbWorkspace(t *testing.T, w *World) *workspacev1.WorkspaceRef {
	t.Helper()
	repo := harness.NewRepo(t)
	return harness.Register(t, w.Daemon, repo.Dir)
}

// dbOpenRootFeed opens a workspace's root feed and answers its initial page
// plus a tailing watch. Distinct from AwaitTurnEnded's private one-shot open
// (world_test.go) because this file needs to observe SEVERAL successive
// states of one upserted row (a shell bubble's live growth, then its
// settlement) rather than a single terminal row.
func dbOpenRootFeed(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) ([]*frontendv1.FeedRow, *harness.Stream[*frontendv1.FeedRow]) {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	stream := w.WatchFeedOn(w.Client(), success.GetWatch())
	return success.GetPage().GetSuccess().GetRows(), stream
}

// dbAwaitDetachedShell drives a root feed (its already-served initial page,
// then its tail) until a FeedRow.detached_shell whose drawn command line
// contains `commandSubstring` reaches a state `settled` accepts, and answers
// that shell plus whether a LIVE frame with non-empty spool text was
// observed strictly before the settled one — the suite's own evidence that
// the spool's incremental growth (daemon/internal/resolve/feed/subagent.go's
// drawDetachedShell, fed by the real sidecar tailing the real spool file the
// fake SDK wrote) was actually exercised, not merely that the final shape
// happens to be right.
//
// Matching by command SUBSTRING rather than by FeedId: the row's FeedId is
// daemon-minted and opaque, and this suite never decodes one — the drawn
// command line is the one stable anchor a caller can name in advance.
func dbAwaitDetachedShell(
	t *testing.T,
	ctx context.Context,
	initial []*frontendv1.FeedRow,
	stream *harness.Stream[*frontendv1.FeedRow],
	commandSubstring string,
	settled func(*frontendv1.FeedShellSettled) bool,
) (shell *frontendv1.FeedShell, sawLiveGrowth bool) {
	t.Helper()
	check := func(row *frontendv1.FeedRow) bool {
		sh := row.GetDetachedShell().GetShell()
		if sh == nil || !strings.Contains(sh.GetCommand().GetText(), commandSubstring) {
			return false
		}
		if sh.GetLive() != nil && sh.GetSpool().GetText() != "" {
			sawLiveGrowth = true
		}
		if s := sh.GetSettled(); s != nil && settled(s) {
			shell = sh
			return true
		}
		return false
	}
	for _, row := range initial {
		if check(row) {
			return
		}
	}
	harness.AwaitView(t, ctx, stream, "a detached shell matching "+commandSubstring, check)
	return
}

// dbExitedZero is the ordinary successful-shell-exit predicate every
// completed-detach test in this file settles on.
func dbExitedZero(s *frontendv1.FeedShellSettled) bool {
	return s.GetOutcome() != nil && s.GetExit() != nil && s.GetExit().GetCode() == 0
}

// dbAwaitSimpleToolCall drives a root feed until a FeedRow.activity.simple_tool_call
// named `toolName` whose input line contains `inputSubstring` reaches
// `returned` — the foreground counterpart of dbAwaitDetachedShell, for the
// tests that contrast a FOREGROUND bash round trip with the detached family
// (#35-38 never detach at all).
func dbAwaitSimpleToolCall(
	t *testing.T,
	ctx context.Context,
	initial []*frontendv1.FeedRow,
	stream *harness.Stream[*frontendv1.FeedRow],
	toolName, inputSubstring string,
	returned func(*frontendv1.FeedToolCallReturned) bool,
) *frontendv1.FeedToolCallReturned {
	t.Helper()
	check := func(row *frontendv1.FeedRow) bool {
		call := row.GetActivity().GetSimpleToolCall()
		if call == nil || call.GetName().GetText() != toolName {
			return false
		}
		if !strings.Contains(call.GetInput().GetText(), inputSubstring) {
			return false
		}
		r := call.GetReturned()
		return r != nil && returned(r)
	}
	for _, row := range initial {
		if call := row.GetActivity().GetSimpleToolCall(); call != nil && check(row) {
			return call.GetReturned()
		}
	}
	row := harness.AwaitView(t, ctx, stream, "a "+toolName+" tool call to return", check)
	return row.GetActivity().GetSimpleToolCall().GetReturned()
}

// ===========================================================================
// #31 — BashDetachedStartAndComplete.
//
// Scenario: `bash-detach` (golden "bash-detached", daemon.md's detached-work
// section / AgentBashOutput). shell.ts's BASH_DETACH backgrounds a Bash call
// from the outset (run_in_background: true), settles the TURN quickly (its
// `conclude()` fires right after the tool_result), and only THEN appends the
// spool's three lines and its `EXIT=0` terminator on separate ticks — so the
// turn's own terminal is not evidence the detached work is done; this test
// watches the shell bubble independently of the turn.
// ===========================================================================

func TestBashDetachedStartAndComplete(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	// Detached work outliving its turn opens a health fault; that is the
	// shape this test provokes.
	w.ExpectWarnings("daemon.health.open_fault")
	ws := dbWorkspace(t, w)

	// Opened BEFORE the prompt: a detached_shell row is an UPSERT (subagent.go's
	// publishShell replaces the row whole on every push), so opening the watch
	// only after the run finished could observe just the final settled push
	// and never the intervening live-with-growing-spool ones this test must see.
	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	turn := SubmitPrompt(t, w, ws, "!bash-detach")
	AwaitTurnEnded(t, w, ws, turn)

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	// BASH_DETACH's default command (no !bash-detach args): "for i in 1 2 3;
	// do echo line-$i; sleep 1; done" — matched by a substring stable across
	// the exact loop body.
	shell, sawLiveGrowth := dbAwaitDetachedShell(t, ctx, initial, stream, "for i in 1 2 3", dbExitedZero)
	if shell == nil {
		t.Fatalf("no detached shell settled with exit 0 within %s", DefaultTimeout)
	}
	if !sawLiveGrowth {
		t.Errorf("never observed a live frame with non-empty spool text before settlement — incremental spool growth (the sidecar tailing the fake SDK's real spool file) was not exercised")
	}
	if got := shell.GetSpool().GetText(); !strings.Contains(got, "line-3") {
		t.Errorf("settled shell spool = %q, want it to contain the scenario's last appended line %q", got, "line-3")
	}
}

// ===========================================================================
// #32 — BashDetachExplicitPoll.
//
// Scenario: `!bash-detach-poll` (shell.ts's BASH_DETACH_POLL). UNGROUNDED,
// INVENTED per testdata/captures/MANIFEST.md: `TaskOutput` is a declared
// vendor tool (every capture's init.tools lists it) but NO capture ever
// calls it — every recorded backgrounded run was checked by re-reading its
// spool path, never by an explicit retrieval tool. This scenario's poll
// tool_use/tool_result pairs are the mock's own invention, mirrored from the
// deleted daemon/e2e harness's `bashTaskOutcome`.
//
// The scenario's own "arms" comment claims the polls "reach no converter
// arm — TaskOutput is unregistered and folds to AgentUnmodeled." THAT CLAIM
// IS WRONG AND THE PROTO WINS: docs/overhaul/shim.md's "What the shim DROPS"
// paragraph names TaskOutput in the EXEMPT set — "dropped entirely, never
// emitted as `AgentUnmodeled`, never tripping the unmodeled warning" — and
// the converter implements exactly that (shim-sidecar/internal/convert/
// exempt.go's exemptTools, covered by settle_test.go's
// TestExemptToolCallProducesNoEntryAtAll). An exempt drop is a DECISION, not
// a modelling gap, so filing it as unmodeled would pollute the very warning
// built to surface real gaps.
//
// So the guarantee this test holds is the negative one, and it is stronger
// than the arbitrary line-text assertion it replaces: an explicit poll of a
// detached shell settles the shell and raises NO unmodeled-tool warning
// naming TaskOutput.
// ===========================================================================

func TestBashDetachExplicitPoll(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	// Detached work outliving its turn opens a health fault; that is the
	// shape this test provokes.
	w.ExpectWarnings("daemon.health.open_fault")
	ws := dbWorkspace(t, w)

	initial, feedStream := dbOpenRootFeed(t, w, ws)
	defer feedStream.Close()
	topbarStream := w.WatchTopbar(ws)
	defer topbarStream.Close()

	turn := SubmitPrompt(t, w, ws, "!bash-detach-poll")
	AwaitTurnEnded(t, w, ws, turn)

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	// BASH_DETACH_POLL's default command (no args): "tail -f build.log"; the
	// spool's last appended line before EXIT=0 is "done".
	shell, _ := dbAwaitDetachedShell(t, ctx, initial, feedStream, "tail -f build.log", dbExitedZero)
	if shell == nil {
		t.Fatalf("no detached shell settled with exit 0 within %s", DefaultTimeout)
	}
	if got := shell.GetSpool().GetText(); !strings.Contains(got, "done") {
		t.Errorf("settled shell spool = %q, want it to contain the scenario's last appended line %q", got, "done")
	}

	// The settled shell above is ordered AFTER both polls were converted, so
	// every topbar view the exempt drop could ever have provoked has already
	// been published or is in flight; the probe window covers the in-flight
	// tail. Every view seen — the initial one and any later push — must be
	// free of a TaskOutput unmodeled warning.
	dbExpectNoUnmodeledWarning(t, topbarStream, "TaskOutput")
}

// dbExpectNoUnmodeledWarning drains the topbar stream for one probe window and
// fails if any view carries an unmodeled-tool warning naming the given tool.
// Other warnings (the detached work's open fault) are expected and ignored.
func dbExpectNoUnmodeledWarning(t *testing.T, stream *harness.Stream[*frontendv1.TopbarView], tool string) {
	t.Helper()
	timer := time.NewTimer(harness.ProbeWindow)
	defer timer.Stop()
	for {
		select {
		case v, ok := <-stream.C:
			if !ok {
				return
			}
			for _, tw := range v.GetWarnings().GetWarnings() {
				if tw.GetUnmodeledTool().GetToolName().GetText() == tool {
					t.Fatalf("topbar carried an unmodeled-tool warning %q for the exempt tool %s, want none: the exempt set is dropped entirely, never filed as unmodeled",
						tw.GetLine().GetText(), tool)
				}
			}
		case <-timer.C:
			return
		}
	}
}

// ===========================================================================
// #33 — CtrlBDetachOfForegroundWork.
//
// RULED (docs/overhaul/PROTO-CHANGES.md, "Landing 8", the RULED-no-proto
// paragraph): "Ctrl-b detach of foreground work has no daemon verb; out of
// scope for the overhaul, recorded as a follow-up; the two e2e tests stay
// skipped pointing here." A real Ctrl-B detach of a LIVE foreground unit
// would require the daemon to call shim.v1 DetachForeground for real
// (shim.md: "DetachForeground {AgentActivityId}: Ctrl-B — moves in-flight
// turn work onto its own stream"), and no caller-facing agentrepl.v1 rpc or
// daemon-internal trigger for that call exists on this branch (confirmed:
// daemon/internal/sessionwatcher/fakes_test.go:268's fake client PANICS on
// it — "sessionwatcher must not call DetachForeground" — and
// proto/src/agentrepl/v1/endpoint_interrupt.proto is STOP-only). That gap is
// now the ruled, recorded reason this golden has no e2e path this wave, not
// an open question this test raises on its own.
//
// This test drives the scenario up to the reachable point (the live
// foreground Bash call actually running) and then skips, per the ruling
// above.
// ===========================================================================

func TestCtrlBDetachOfForegroundWork(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	ws := dbWorkspace(t, w)

	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	// "!ctrl-b" (shell.ts's CTRL_B) starts a FOREGROUND `tail -f
	// /var/log/system.log` and parks it live, waiting for a real
	// DetachForeground to resolve `awaitBackgrounded`. It never returns on
	// its own, so this test awaits the RUNNING state (never `returned` —
	// that would simply time out, since nothing here can make the call end).
	SubmitPrompt(t, w, ws, "!ctrl-b")

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	check := func(row *frontendv1.FeedRow) bool {
		call := row.GetActivity().GetSimpleToolCall()
		return call.GetName().GetText() == "Bash" &&
			strings.Contains(call.GetInput().GetText(), "tail -f /var/log/system.log") &&
			call.GetRunning() != nil
	}
	found := false
	for _, row := range initial {
		if check(row) {
			found = true
		}
	}
	if !found {
		harness.AwaitView(t, ctx, stream, "the live foreground Bash unit CTRL_B parks on", check)
	}

	t.Skip("RULED out of scope (docs/overhaul/PROTO-CHANGES.md, Landing 8, RULED-no-proto: \"Ctrl-b detach of foreground work has no daemon verb; out of scope for the overhaul, recorded as a follow-up; the two e2e tests stay skipped pointing here\") — see this test's header comment")
}

// ===========================================================================
// #34 — CtrlBDetachOfForegroundSubagent.
//
// Same ruling as #33 applies: docs/overhaul/PROTO-CHANGES.md, "Landing 8",
// RULED-no-proto paragraph — "Ctrl-b detach of foreground work has no
// daemon verb; out of scope for the overhaul, recorded as a follow-up; the
// two e2e tests stay skipped pointing here." Compounded here by a second,
// golden-specific gap: grepping agent-shim/claude/shim/src/fake/scenarios/
// subagents.ts finds no ctrl-b-shaped scenario at all for a subagent (only
// `subagent`, `subagent-detached`, `subagent-detached-live`,
// `subagent-detached-utterance`, `subagent-failed`, `cancel-all`,
// `usage-historical`) — shell.ts's CTRL_B backgrounds a BASH call, not a
// subagent spawn.
//
// This test drives the reachable half (an ordinary live subagent spawn) and
// then skips, per the ruling above.
// ===========================================================================

func TestCtrlBDetachOfForegroundSubagent(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	ws := dbWorkspace(t, w)

	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	// "!subagent" (subagents.ts's SUBAGENT) spawns an ordinary, awaited
	// subagent — the closest reachable analogue to "a live foreground
	// subagent", since no scenario backgrounds one on its own.
	SubmitPrompt(t, w, ws, "!subagent")

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	harness.AwaitView(t, ctx, stream, "a live subagent bubble", func(row *frontendv1.FeedRow) bool {
		for _, r := range initial {
			if r.GetActivity().GetSubagent() != nil {
				return true
			}
		}
		return row.GetActivity().GetSubagent() != nil
	})

	t.Skip("RULED out of scope (docs/overhaul/PROTO-CHANGES.md, Landing 8, RULED-no-proto: \"Ctrl-b detach of foreground work has no daemon verb; out of scope for the overhaul, recorded as a follow-up; the two e2e tests stay skipped pointing here\"), plus no fake-SDK scenario backgrounds a subagent the way shell.ts's CTRL_B backgrounds a Bash call — see this test's header comment")
}

// ===========================================================================
// #35 — BashForegroundCompleted.
//
// Golden "bash-foreground-completed" -> prompt "!bash" (shell.ts's BASH,
// golden-conformance.test.ts's mapping). An ORDINARY, non-detached round
// trip, contrasted against #31/#33's detached shapes: no detached_shell row
// should ever appear for this command.
// ===========================================================================

func TestBashForegroundCompleted(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	ws := dbWorkspace(t, w)

	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	turn := SubmitPrompt(t, w, ws, "!bash")
	AwaitTurnEnded(t, w, ws, turn)

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	// BASH's default command (no !bash args): "pwd; ls | head", stdout
	// "one\ntwo\n" — a COMPLETED command is drawn succeeded regardless of its
	// exit code (daemon/internal/resolve/feed/toolcall.go's bashOutcomeText:
	// "case *conversationv1.AgentBashSuccess_Completed: return true, ...").
	returned := dbAwaitSimpleToolCall(t, ctx, initial, stream, "Bash", "pwd; ls | head",
		func(r *frontendv1.FeedToolCallReturned) bool { return r.GetSucceeded() != nil })
	if returned == nil {
		t.Fatalf("no Bash tool call returned within %s", DefaultTimeout)
	}
	if got := returned.GetText().GetText(); !strings.Contains(got, "one") || !strings.Contains(got, "two") {
		t.Errorf("returned text = %q, want it to contain the scenario's stdout (\"one\", \"two\")", got)
	}

	for _, row := range initial {
		if row.GetDetachedShell() != nil {
			t.Errorf("an ordinary foreground bash round trip produced a detached_shell row: %v", row)
		}
	}
}

// ===========================================================================
// #36 — BashNonzeroExit.
//
// Golden "bash-nonzero-exit" -> prompt "!bash-fail" (shell.ts's BASH_FAIL).
// A non-zero exit is still a COMPLETED call, drawn SUCCEEDED — "the code is
// the command's verdict on itself, NOT a failure of the call" (sidecar.md's
// Gotchas; confirmed verbatim in toolcall.go's bashOcomeText, which returns
// `true` unconditionally for the Completed arm).
// ===========================================================================

func TestBashNonzeroExit(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	ws := dbWorkspace(t, w)

	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	turn := SubmitPrompt(t, w, ws, "!bash-fail")
	AwaitTurnEnded(t, w, ws, turn)

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	returned := dbAwaitSimpleToolCall(t, ctx, initial, stream, "Bash", "exit 3",
		func(r *frontendv1.FeedToolCallReturned) bool { return r.GetVerdict() != nil })
	if returned == nil {
		t.Fatalf("no Bash tool call returned within %s", DefaultTimeout)
	}
	if returned.GetSucceeded() == nil {
		t.Errorf("a non-zero-exit-but-completed command must draw succeeded (the exit code is not a call failure); got %v", returned.GetVerdict())
	}
	if got := returned.GetText().GetText(); !strings.Contains(got, "boom") {
		t.Errorf("returned text = %q, want it to contain the scenario's stderr (%q)", got, "boom")
	}
}

// ===========================================================================
// #37 — BashImageOutput.
//
// Golden "bash-image-output" -> prompt "!bash-image" (shell.ts's BASH_IMAGE).
// Per this area's explicit license to confirm the exact emitted
// FeedToolCallReturned.form (SPEC.md §C #37): read
// daemon/internal/resolve/feed/toolcall.go's bashOutputText, which handles
// ONLY the AgentBashOutput_Text form — an AgentBashOutput_Image form falls
// through its type assertion and yields an empty string, and textForm("")
// answers `nil`, which applyReturnedForm renders as the `none` arm
// (FeedToolCallNoOutput) rather than an empty text. So a bash call whose
// output is image data currently draws SUCCEEDED with NO output body at the
// frontend layer — this is the confirmed CURRENT shape (not a defect this
// area writer is asked to judge), and the assertion below pins exactly that,
// per the instruction not to adapt a test to a production behavior found by
// reading source, only to confirm the shape when the spec explicitly says to.
// ===========================================================================

func TestBashImageOutput(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	ws := dbWorkspace(t, w)

	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	turn := SubmitPrompt(t, w, ws, "!bash-image")
	AwaitTurnEnded(t, w, ws, turn)

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	returned := dbAwaitSimpleToolCall(t, ctx, initial, stream, "Bash", "screencapture",
		func(r *frontendv1.FeedToolCallReturned) bool { return r.GetVerdict() != nil })
	if returned == nil {
		t.Fatalf("no Bash tool call returned within %s", DefaultTimeout)
	}
	if returned.GetSucceeded() == nil {
		t.Errorf("verdict = %v, want succeeded", returned.GetVerdict())
	}
	if returned.GetNone() == nil {
		t.Errorf("form = %v, want `none` (FeedToolCallNoOutput) — image-form bash output currently draws no output body; see this test's header comment", returned.GetForm())
	}
}

// ===========================================================================
// #38 — BashPartialOutputWithSpill.
//
// Golden "bash-partial-output-with-spill" -> prompt "!bash-spill" (shell.ts's
// BASH_SPILL). The manifest flags this capture's own spool at 241MB, so this
// test — per SPEC.md's own instruction for this row — reads back only the
// daemon's small, store-durable COMPOSED SUMMARY line, never the spill file
// itself. bashOutputText (toolcall.go) appends the fixed phrase "<count>
// bytes more not shown" onto a partial extent's joined text; that phrase is
// the summary this test pins.
// ===========================================================================

func TestBashPartialOutputWithSpill(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	ws := dbWorkspace(t, w)

	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	turn := SubmitPrompt(t, w, ws, "!bash-spill")
	AwaitTurnEnded(t, w, ws, turn)

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	returned := dbAwaitSimpleToolCall(t, ctx, initial, stream, "Bash", "yes | head -100000",
		func(r *frontendv1.FeedToolCallReturned) bool { return r.GetVerdict() != nil })
	if returned == nil {
		t.Fatalf("no Bash tool call returned within %s", DefaultTimeout)
	}
	if returned.GetSucceeded() == nil {
		t.Errorf("a completed command must draw succeeded regardless of output size; got %v", returned.GetVerdict())
	}
	if got := returned.GetText().GetText(); !strings.Contains(got, "bytes more not shown") {
		t.Errorf("returned text = %q, want it to contain the daemon's composed truncation summary (\"bytes more not shown\") — never the 241MB spill file itself", got)
	}
}
