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
package e2e

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"agent-repld/integration/harness"
)

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

// THE SHELL BUBBLE IS A FEED (feed.proto, FeedRow.shell_head and
// FeedRow.detached_shell; landed by ac808eb57, "fold the detached shell into
// ONE canonical bubble", 2026-09-14). A detached shell draws TWO rows, never
// one:
//
//   - its HEAD (FeedRow.shell_head) on the PARENT feed — the command, the
//     clock and the state (live or settled), and NO spool;
//   - its spool BODY (FeedRow.detached_shell) on the shell's OWN sub-feed,
//     which the head row's own FeedId addresses — OpenFeed on that id is what
//     a client does when it expands the bubble.
//
// `detached_shell` is therefore never a top-level row, and a test that waits
// for one on the root feed waits until its bound expires. That is exactly
// what every detached test in this file did until the helpers below replaced
// the one that looked for it; daemon/integration made the same correction in
// e70d3068d.

// dbShellHead answers the row's shell HEAD when its drawn command line
// contains `commandSubstring`, and nil for any other row.
//
// Matching by command SUBSTRING rather than by FeedId: the row's FeedId is
// daemon-minted and opaque, and this suite never decodes one — the drawn
// command line is the one stable anchor a caller can name in advance.
func dbShellHead(row *frontendv1.FeedRow, commandSubstring string) *frontendv1.FeedShell {
	head := row.GetShellHead()
	if head == nil || !strings.Contains(head.GetCommand().GetText(), commandSubstring) {
		return nil
	}
	return head
}

// dbAwaitShellHead drives a root feed (its already-served initial page, then
// its tail) until the shell HEAD whose command contains `commandSubstring`
// reaches a state `accept` takes, and answers that head's ROW — the row, not
// only the shell, because its FeedId is the address of the bubble's body.
func dbAwaitShellHead(
	t *testing.T,
	ctx context.Context,
	initial []*frontendv1.FeedRow,
	stream *harness.Stream[*frontendv1.FeedRow],
	commandSubstring, what string,
	accept func(*frontendv1.FeedShell) bool,
) *frontendv1.FeedRow {
	t.Helper()
	check := func(row *frontendv1.FeedRow) bool {
		head := dbShellHead(row, commandSubstring)
		return head != nil && accept(head)
	}
	// THE PAGE IS READ NEWEST-LAST, so the LAST matching row is the head as it
	// now stands: an upserted head can appear once per push in a replayed
	// page, and an earlier copy may be a state it has since left.
	var found *frontendv1.FeedRow
	for _, row := range initial {
		if dbShellHead(row, commandSubstring) != nil {
			found = row
		}
	}
	if found != nil && check(found) {
		return found
	}
	return harness.AwaitView(t, ctx, stream, what+" ("+commandSubstring+")", check)
}

// dbOpenShellBody opens a shell bubble's own sub-feed — addressed by the head
// row's FeedId, exactly as a client expanding the bubble does — and answers
// its initial page plus a tailing watch. The caller closes the stream.
func dbOpenShellBody(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, head *frontendv1.FeedRow) ([]*frontendv1.FeedRow, *harness.Stream[*frontendv1.FeedRow]) {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{
		Workspace: ws,
		Feed:      head.GetId(),
	}))
	if err != nil {
		t.Fatalf("OpenFeed(the shell bubble's sub-feed): %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed(the shell bubble's sub-feed) = %v, want success: the head's own FeedId addresses its body", opened.Msg)
	}
	return success.GetPage().GetSuccess().GetRows(), w.WatchFeedOn(w.Client(), success.GetWatch())
}

// dbAwaitShellSpool drives a shell bubble's sub-feed until its spool BODY
// carries `spoolSubstring`, and answers the whole spool text as drawn.
func dbAwaitShellSpool(
	t *testing.T,
	ctx context.Context,
	initial []*frontendv1.FeedRow,
	stream *harness.Stream[*frontendv1.FeedRow],
	spoolSubstring string,
) string {
	t.Helper()
	var spool string
	check := func(row *frontendv1.FeedRow) bool {
		body := row.GetDetachedShell().GetShell()
		if body == nil || !strings.Contains(body.GetSpool().GetText(), spoolSubstring) {
			return false
		}
		spool = body.GetSpool().GetText()
		return true
	}
	for _, row := range initial {
		if check(row) {
			return spool
		}
	}
	harness.AwaitView(t, ctx, stream, "the shell bubble's spool body carrying "+spoolSubstring, check)
	return spool
}

// dbAwaitSettledShell waits for the shell whose command contains
// `commandSubstring` to settle in a state `settled` accepts, then opens its
// body and waits for the spool to carry `spoolSubstring`. It answers the
// settled HEAD and the spool as the body draws it.
//
// THE BODY IS READ AFTER THE HEAD SETTLES, and that is enough: publishShell
// (daemon/internal/resolve/feed/subagent.go) upserts the body on every push
// the head gets, so the sub-feed's own page, opened now, already carries the
// spool the settling push drew.
func dbAwaitSettledShell(
	t *testing.T,
	ctx context.Context,
	w *World,
	ws *workspacev1.WorkspaceRef,
	initial []*frontendv1.FeedRow,
	stream *harness.Stream[*frontendv1.FeedRow],
	commandSubstring string,
	settled func(*frontendv1.FeedShellSettled) bool,
	spoolSubstring string,
) (head *frontendv1.FeedShell, spool string) {
	t.Helper()
	row := dbAwaitShellHead(t, ctx, initial, stream, commandSubstring, "a settled detached shell head",
		func(sh *frontendv1.FeedShell) bool { return sh.GetSettled() != nil && settled(sh.GetSettled()) })
	bodyInitial, body := dbOpenShellBody(t, w, ws, row)
	defer body.Close()
	return row.GetShellHead(), dbAwaitShellSpool(t, ctx, bodyInitial, body, spoolSubstring)
}

// dbExitedZero is the ordinary successful-shell-exit predicate every
// completed-detach test in this file settles on.
func dbExitedZero(s *frontendv1.FeedShellSettled) bool {
	return s.GetOutcome() != nil && s.GetExit() != nil && s.GetExit().GetCode() == 0
}

// dbAwaitSimpleToolCall drives a root feed until a FeedRow.activity.simple_tool_call
// named `toolName` whose input line contains `inputSubstring` reaches
// `returned` — the foreground counterpart of dbAwaitSettledShell, for the
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
//
// THE DETACH GATE IS WHAT MAKES "LIVE GROWTH" A FACT RATHER THAN A RACE. The
// fake writes its whole spool within a few scheduler ticks, so without the
// gate the sidecar may read all three lines and the terminator in ONE poll and
// the bubble is never observably live-with-output at all. With
// AGENT_REPL_FAKE_DETACH_GATE set (fake/index.ts's DETACH_GATE_PATH_ENV) the
// run appends `line-1` and PARKS until this test creates the gate file, so the
// body carrying `line-1` under a head that is still LIVE is guaranteed by
// construction, and the rest of the spool and its settle can only follow the
// test's own act.
// ===========================================================================

// Seam: shell-spool
func TestBashDetachedStartAndComplete(t *testing.T) {
	t.Parallel()
	gatePath := filepath.Join(t.TempDir(), "detach-gate")
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		ExtraEnv: []string{dbDetachGatePathEnv + "=" + gatePath},
	}})
	// Detached work outliving its turn opens a health fault; that is the
	// shape this test provokes.
	w.ExpectWarnings("daemon.health.open_fault")
	ws := w.NewWorkspace(t)

	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	turn := SubmitPrompt(t, w, ws, "!bash-detach")
	AwaitTurnEnded(t, w, ws, turn)

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	// BASH_DETACH's default command (no !bash-detach args): "for i in 1 2 3;
	// do echo line-$i; sleep 1; done" — matched by a substring stable across
	// the exact loop body.
	const command = "for i in 1 2 3"

	// Assert: the bubble is drawn LIVE while the run is parked on the gate,
	// and its body already carries the first line — incremental spool growth
	// (the real sidecar tailing the real spool file the fake wrote) under a
	// head that has not settled.
	head := dbAwaitShellHead(t, ctx, initial, stream, command, "the live detached shell head",
		func(sh *frontendv1.FeedShell) bool { return sh.GetLive() != nil })
	bodyInitial, body := dbOpenShellBody(t, w, ws, head)
	defer body.Close()
	if got := dbAwaitShellSpool(t, ctx, bodyInitial, body, "line-1"); strings.Contains(got, "line-3") {
		t.Fatalf("the parked run's spool body = %q already carries line-3; the detach gate did not hold the run", got)
	}

	// Act: release the parked run.
	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("e2e: creating this test's own detach gate %s: %v", gatePath, err)
	}

	// Assert: the head settles with exit 0, and the body grows to the last
	// line the scenario appended.
	settled := dbAwaitShellHead(t, ctx, nil, stream, command, "the detached shell head settled with exit 0",
		func(sh *frontendv1.FeedShell) bool { return sh.GetSettled() != nil && dbExitedZero(sh.GetSettled()) })
	if settled.GetId().GetValue() != head.GetId().GetValue() {
		t.Errorf("the settled head is row %q, want the live head's own row %q: a settle re-pushes ONE head",
			settled.GetId().GetValue(), head.GetId().GetValue())
	}
	dbAwaitShellSpool(t, ctx, nil, body, "line-3")
}

// dbDetachGatePathEnv is the fake vendor's detach gate
// (agent-shim/claude/shim/src/fake/index.ts's DETACH_GATE_PATH_ENV): once set,
// `bash-detach` appends its first spool line and parks until the named path
// exists.
const dbDetachGatePathEnv = "AGENT_REPL_FAKE_DETACH_GATE"

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
	ws := w.NewWorkspace(t)

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
	// The body is awaited carrying "done": a settled head alone would not say
	// the spool's last line was ever drawn.
	dbAwaitSettledShell(t, ctx, w, ws, initial, feedStream, "tail -f build.log", dbExitedZero, "done")

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
// #35 — BashForegroundCompleted.
//
// Golden "bash-foreground-completed" -> prompt "!bash" (shell.ts's BASH,
// golden-conformance.test.ts's mapping). An ORDINARY, non-detached round
// trip, contrasted against #31/#33's detached shapes: no detached_shell row
// should ever appear for this command.
// ===========================================================================

// Seam: stream-tool-card
func TestBashForegroundCompleted(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	ws := w.NewWorkspace(t)

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
		if row.GetShellHead() != nil || row.GetDetachedShell() != nil {
			t.Errorf("an ordinary foreground bash round trip produced a detached shell bubble: %v", row)
		}
	}
}
