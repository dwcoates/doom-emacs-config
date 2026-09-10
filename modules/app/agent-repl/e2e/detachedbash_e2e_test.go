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
	// LANDING 16: the code is drawn, by name. The badge says the CALL was
	// fine; without the chip the card never said what the command's own
	// verdict on itself was. The element is FeedShellExit — the same one the
	// detached shell's settled head wears — and the scenario states 3.
	exit := returned.GetExit()
	if exit == nil {
		t.Fatalf("FeedToolCallReturned.exit = nil, want the exit code the command reported")
	}
	if exit.GetCode() != 3 {
		t.Errorf("FeedToolCallReturned.exit.code = %d, want 3 (the scenario's stated status)", exit.GetCode())
	}
}

// ===========================================================================
// #37 — BashImageOutput.
//
// Golden "bash-image-output" -> prompt "!bash-image" (shell.ts's BASH_IMAGE).
// Per this area's explicit license to confirm the exact emitted
// FeedToolCallReturned.form (SPEC.md §C #37).
//
// LANDING 16 CHANGED THIS SHAPE, AND THIS TEST WITH IT. The card used to draw
// SUCCEEDED with the `none` arm — nothing at all, indistinguishable from a
// command that printed nothing — because the form oneof had no image arm and
// the daemon's bashOutputText fell through its Text type assertion to "".
// It now draws the `image` arm carrying FeedImageBlock, the very block a
// prompt body's image uses, with the src the DAEMON composed from the bytes
// and media type the producers now carry. This test pins the arm BY NAME and
// the src's own shape, which is the whole of what the landing added.
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
	image := returned.GetImage()
	if image == nil {
		t.Fatalf("form = %T, want the `image` arm (FeedImageBlock); see this test's header comment", returned.GetForm())
	}
	// THE DAEMON RESOLVES THE REFERENCE: the client is handed something a
	// browser can load, never the raw bytes and a media type to assemble.
	if !strings.HasPrefix(image.GetSrc(), "data:image/png;base64,") {
		t.Errorf("image src = %q, want a data url the webview can load", image.GetSrc())
	}
	if image.GetSrc() == "data:image/png;base64," {
		t.Errorf("image src = %q, want the scenario's payload behind the prefix", image.GetSrc())
	}
	// The command line is the only caption the record affords.
	if got := image.GetAlt(); !strings.Contains(got, "screencapture") {
		t.Errorf("image alt = %q, want the command line as the caption", got)
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

// ===========================================================================
// Coverage extension — `!bash-detach-fail`: a detached shell ending non-zero.
// ===========================================================================

// TestBashDetachedNonzeroExit drives "!bash-detach-fail" (shell.ts's
// BASH_DETACH_FAIL): a backgrounded `echo error && exit 3` whose spool is
// terminated by `EXIT=3` and whose vendor `task_updated`/`task_notification`
// both say `status: "failed"`.
//
// THE PROTO SETTLES WHAT THAT MEANS, and it is NOT a failure arm.
// feed.proto's FeedShellSettled.outcome spells the completed arm as "The
// process exited on its own; the exit chip says how it went — a non-zero exit
// still COMPLETED, and 'failure' is the reader's judgment of the code, never
// an arm", and the oneof's only other arms are `cancelled` (stopped by hand)
// and `lost` (we stopped being able to see it). So the guarantee here is
// two-sided: the outcome arm is `completed`, and the fact that the run went
// badly rides ENTIRELY on FeedShellExit.code == 3 — the same shape #36 pins
// for the foreground non-zero case.
func TestBashDetachedNonzeroExit(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	// Detached work outliving its turn opens a health fault, as in #31/#32.
	w.ExpectWarnings("daemon.health.open_fault")
	ws := dbWorkspace(t, w)

	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	turn := SubmitPrompt(t, w, ws, "!bash-detach-fail")
	AwaitTurnEnded(t, w, ws, turn)

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	shell, _ := dbAwaitDetachedShell(t, ctx, initial, stream, "exit 3", func(s *frontendv1.FeedShellSettled) bool {
		return s.GetOutcome() != nil
	})
	if shell == nil {
		t.Fatalf("no detached shell for the failing command settled within %s", DefaultTimeout)
	}
	if shell.GetSettled().GetCompleted() == nil {
		t.Errorf("settled outcome = %v, want completed: a non-zero exit is still a COMPLETED run "+
			"(feed.proto, FeedShellSettled.outcome) — never the cancelled or lost arm",
			shell.GetSettled().GetOutcome())
	}
	if shell.GetSettled().GetExit() == nil {
		t.Fatalf("settled exit = UNSET, want the code the spool's `EXIT=3` terminator carried")
	}
	if got := shell.GetSettled().GetExit().GetCode(); got != 3 {
		t.Errorf("settled exit code = %d, want 3 (the scenario's own `EXIT=3` terminator)", got)
	}
	if got := shell.GetSpool().GetText(); !strings.Contains(got, "error") {
		t.Errorf("settled shell spool = %q, want the scenario's one appended line (%q)", got, "error")
	}
}

// ===========================================================================
// Coverage extension — `!bash-detach-live`: a detached shell left running.
// ===========================================================================

// TestBashDetachedLiveNeverSettles drives "!bash-detach-live" (shell.ts's
// BASH_DETACH_LIVE): a backgrounded `sleep 100000` whose spool gets one line
// and NO `EXIT=` terminator, and for which the vendor sends no terminal
// notification at all.
//
// # WHAT IS AND IS NOT ASSERTED HERE
//
// The matrix names this scenario the lever for the sidecar's staleness arms
// (conversation/v1/agent_activity.proto's DetachedLost{file_vanished |
// went_silent | swept_up}, drawn as feed.proto's FeedShellLost — "We stopped
// being able to see it — spool gone or silent past the shim's ruling; not
// known to have failed"). Reaching a LOST settle requires the sidecar's own
// staleness ruling to elapse or its sweep to run, and this world runs the
// sidecar with its PRODUCTION windows (a 30s grace, a 30m shell silence),
// which no test's budget can wait out. So this test pins the state that
// PRECEDES every one of those arms and that the registry
// must hold indefinitely: the shell stays LIVE with its unterminated
// spool's text, and settles into NOTHING — not completed, not lost — for as
// long as nothing stops it.
//
// GAP SINCE CLOSED: detachedlost_e2e_test.go drives all three DetachedLost
// arms off THIS scenario, buying the sidecar's own staleness windows through
// NewWorldWithSidecarStaleness. This test keeps its subject unchanged — the
// state that PRECEDES every one of those arms, under a sidecar running with
// PRODUCTION windows, where nothing is ever concluded.
func TestBashDetachedLiveNeverSettles(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	// A detached run that outlives its turn AND never ends: the open fault
	// this provokes is the whole point of the scenario.
	w.ExpectWarnings("daemon.health.open_fault")
	ws := dbWorkspace(t, w)

	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	turn := SubmitPrompt(t, w, ws, "!bash-detach-live")
	AwaitTurnEnded(t, w, ws, turn)

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	// Assert: the bubble is drawn LIVE, carrying the unterminated spool's line.
	live := dbAwaitLiveDetachedShell(t, ctx, initial, stream, "sleep 100000", "partial output with no terminator")
	if live.GetLive() == nil {
		t.Fatalf("detached shell state = %v, want live", live.GetState())
	}

	// Assert: it never settles. A claim of absence necessarily waits out a
	// bound rather than an event (harness.ProbeWindow, this suite's own
	// convention for a designed negative window).
	dbExpectShellNeverSettles(t, stream, "sleep 100000")
}

// dbAwaitLiveDetachedShell is dbAwaitDetachedShell's live counterpart: it
// answers the first detached_shell row whose command matches and whose spool
// already carries `spoolSubstring`, WITHOUT requiring a settled state — the
// shape a run that never terminates can only ever reach.
func dbAwaitLiveDetachedShell(
	t *testing.T,
	ctx context.Context,
	initial []*frontendv1.FeedRow,
	stream *harness.Stream[*frontendv1.FeedRow],
	commandSubstring, spoolSubstring string,
) *frontendv1.FeedShell {
	t.Helper()
	var found *frontendv1.FeedShell
	check := func(row *frontendv1.FeedRow) bool {
		sh := row.GetDetachedShell().GetShell()
		if sh == nil || !strings.Contains(sh.GetCommand().GetText(), commandSubstring) {
			return false
		}
		if !strings.Contains(sh.GetSpool().GetText(), spoolSubstring) {
			return false
		}
		found = sh
		return true
	}
	for _, row := range initial {
		if check(row) {
			return found
		}
	}
	harness.AwaitView(t, ctx, stream, "a live detached shell for "+commandSubstring+" carrying "+spoolSubstring, check)
	if found == nil {
		t.Fatalf("no live detached shell for %q observed within %s", commandSubstring, DefaultTimeout)
	}
	return found
}

// dbExpectShellNeverSettles drains the feed for one probe window and fails if
// the named detached shell ever pushes a settled state.
func dbExpectShellNeverSettles(t *testing.T, stream *harness.Stream[*frontendv1.FeedRow], commandSubstring string) {
	t.Helper()
	timer := time.NewTimer(harness.ProbeWindow)
	defer timer.Stop()
	for {
		select {
		case row, ok := <-stream.C:
			if !ok {
				return
			}
			sh := row.GetDetachedShell().GetShell()
			if sh == nil || !strings.Contains(sh.GetCommand().GetText(), commandSubstring) {
				continue
			}
			if s := sh.GetSettled(); s != nil {
				t.Fatalf("the never-ending detached shell settled with %v, want it to stay live: nothing "+
					"terminated its spool and no notification ever reported it done", s.GetOutcome())
			}
		case <-timer.C:
			return
		}
	}
}

// ===========================================================================
// Coverage extension — `!bash-hold`: a LIVE FOREGROUND shell.
// ===========================================================================

// TestBashHoldStaysForegroundUntilInterrupted drives "!bash-hold" (shell.ts's
// BASH_HOLD), the scenario minted specifically as "the lever for
// DetachForeground's `unsupported` refusal, which needs a GENUINELY LIVE
// foreground unit to refuse".
//
// # THE REFUSAL ITSELF IS NOT REACHABLE FROM THIS LAYER — GAP RECORDED
//
// DetachForeground is a shim.v1 verb (proto/src/shim/v1/service.proto:107,
// endpoint_detach_foreground.proto's DetachForegroundFailure.unsupported).
// NOTHING in agentrepl/v1 exposes it: the same ruling (docs/overhaul/
// PROTO-CHANGES.md "Landing 8", RULED-no-proto — "detach of in-flight
// foreground work has no daemon verb; out of scope for the overhaul") means
// an e2e test, which speaks only the daemon's caller-facing
// API, has no way to issue the call whose refusal it would assert. This test
// does NOT reach into the shim to fake one up; the `unsupported` arm stays
// uncovered and that is recorded here rather than papered over.
//
// What IS reachable, and what this test pins, is the precondition the
// scenario exists to establish and which no other scenario provides: a Bash
// unit that is live, FOREGROUND (no detached_shell row is ever drawn for it —
// BASH_HOLD passes neither `run_in_background` nor startTask, deliberately),
// and stays that way until a real daemon Interrupt ends the turn.
func TestBashHoldStaysForegroundUntilInterrupted(t *testing.T) {
	t.Parallel()
	w := NewWorld(t, WorldOpts{})
	ws := dbWorkspace(t, w)

	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	turn := SubmitPrompt(t, w, ws, "!bash-hold")

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	// Assert: the held unit is drawn as a live FOREGROUND tool call.
	held := func(row *frontendv1.FeedRow) bool {
		call := row.GetActivity().GetSimpleToolCall()
		return call.GetName().GetText() == "Bash" &&
			strings.Contains(call.GetInput().GetText(), "tail -f /var/log/system.log") &&
			call.GetRunning() != nil
	}
	sawHeld := false
	for _, row := range initial {
		if held(row) {
			sawHeld = true
		}
	}
	if !sawHeld {
		harness.AwaitView(t, ctx, stream, "the live FOREGROUND Bash unit BASH_HOLD parks on", held)
	}

	// Assert: it is foreground — the detachable-in-kind unit was never drawn
	// as detached work, because the pinned SDK offers no verb to detach it
	// (BASH_HOLD's own comment) and the daemon never issued one.
	dbExpectNoDetachedShell(t, stream, "tail -f /var/log/system.log")

	// Act: end it the one way this layer can — a real daemon Interrupt.
	resp, err := w.Client().Interrupt(w.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))
	if err != nil {
		t.Fatalf("Interrupt(turn): %v", err)
	}
	if resp.Msg.GetSuccess().GetInterruptedTurn() == nil {
		t.Fatalf("Interrupt(turn) = %v, want success.interrupted_turn", resp.Msg)
	}

	// Assert: the held turn's ONLY reachable terminal is the interrupted arm
	// (BASH_HOLD emits no result and no explicit terminal of its own).
	ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded()
	if ended.GetInterrupted() == nil {
		t.Fatalf("turn ended = %v, want turn_ended.interrupted: a held foreground bash concludes on nothing else", ended)
	}

	// Assert: AND THE CARD IS NO LONGER DRAWN RUNNING. The vendor returns no
	// `tool_result` for the call a stop landed inside, so the shim's own cut is
	// what settles the unit (convert/tools/bash.ts `cut`); without it this card
	// went on drawing a live shell inside a turn that had ended, which is what
	// the F42 playbook photographed.
	settled := harness.AwaitView(t, ctx, stream, "the held Bash unit to settle once the stop cut it",
		func(row *frontendv1.FeedRow) bool {
			call := row.GetActivity().GetSimpleToolCall()
			return call.GetName().GetText() == "Bash" &&
				strings.Contains(call.GetInput().GetText(), "tail -f /var/log/system.log") &&
				call.GetReturned() != nil
		})
	returned := settled.GetActivity().GetSimpleToolCall().GetReturned()
	// A STOP IS NOT THE CALL BREAKING: AgentBashInterrupted nests inside
	// AgentBashSuccess, so the badge reads succeeded and the body says how it
	// was cut. The same shape TestInterruptAfterTextDelta already pins for a
	// vendor-reported interrupt.
	if returned.GetSucceeded() == nil {
		t.Fatalf("the cut Bash call's verdict = %v, want succeeded", returned.GetVerdict())
	}
	if got := returned.GetText().GetText(); !strings.Contains(got, "interrupted by the user") {
		t.Fatalf("the cut Bash call's output = %q, want it to say it was interrupted by the user", got)
	}
}

// dbExpectNoDetachedShell drains the feed for one probe window and fails if a
// detached_shell row is ever drawn for the named command. The negative that
// makes "foreground" a claim rather than an assumption.
func dbExpectNoDetachedShell(t *testing.T, stream *harness.Stream[*frontendv1.FeedRow], commandSubstring string) {
	t.Helper()
	timer := time.NewTimer(harness.ProbeWindow)
	defer timer.Stop()
	for {
		select {
		case row, ok := <-stream.C:
			if !ok {
				return
			}
			sh := row.GetDetachedShell().GetShell()
			if sh != nil && strings.Contains(sh.GetCommand().GetText(), commandSubstring) {
				t.Fatalf("a detached_shell row was drawn for the HELD FOREGROUND command %q: nothing detached "+
					"it, and no daemon verb for DetachForeground exists on this branch", commandSubstring)
			}
		case <-timer.C:
			return
		}
	}
}

// ===========================================================================
// Coverage extension — `!vendor-backgrounded`: a foreground shell the vendor
// tracks as a task and has not detached.
// ===========================================================================

// TestVendorBackgroundedTaskStartIsNoDetachment drives "!vendor-backgrounded"
// (shell.ts's VENDOR_BACKGROUNDED): a FOREGROUND `Bash` — no
// `run_in_background`, no startTask-first launch — that the vendor
// nonetheless announces as a live `local_bash` task (`task_started` +
// `background_tasks_changed`) and whose spool it begins filling, then PARKS
// on `ctx.awaitBackgrounded(toolUseId)` until a caller's DetachForeground
// actually moves it.
//
// # THE DETACHMENT ITSELF IS NOT REACHABLE FROM THIS LAYER — GAP RECORDED
//
// The park is released only by shim.v1's DetachForeground
// (proto/src/shim/v1/service.proto:107). Nothing in agentrepl.v1 exposes it
// and the daemon never issues it (SCENARIO-MATRIX.md's own
// "DetachForeground applied to a foreground subagent" entry, and
// PROTO-CHANGES.md Landing 8's RULED-no-proto). So the second half of the
// scenario — the `backgroundedByUser: true` result, its `by_user`
// detachment, and AgentSuccess.backgrounded — cannot be provoked by a test
// that speaks only the daemon's caller-facing API, and this test does not
// reach into the shim to fake one up.
//
// # WHAT IS REACHABLE IS THE HALF THE PARK EXISTS TO EXPOSE
//
// The first half is a contract claim nothing else in the suite can provoke,
// because no other scenario announces a `local_bash` task for a call that
// has NOT left the turn. convert/detached.ts's `task_started` handler is
// written against exactly this shape:
//
//	"A SHELL TASK IS NOT A DETACHMENT YET. The vendor tracks a FOREGROUND
//	 shell as a task the moment it starts ... so `task_started` says nothing
//	 about whether the work left the turn, let alone why. The Bash result is
//	 the one record that states the cause ... Announcing `requested` here
//	 instead put a WRONG-CAUSE announcement on the stream ahead of the right
//	 one".
//
// Two facts follow, and both are asserted:
//
//  1. NO detached_shell row is drawn for the command. A resolver fed a
//     `requested` announcement off `task_started` would draw one here, with
//     the wrong cause, ahead of the vendor's own account — the precise
//     regression that comment names. `!bash-hold` pins the same negative for
//     a call the vendor never announced at all; this pins it for one it DID,
//     which is the harder half.
//  2. The unit's fate is UNSETTLED and FOREGROUND: it stays a running
//     FeedSimpleToolCall for the whole probe window. The work in flight is
//     neither concluded nor handed to a detached row — it is exactly where
//     the vendor left it, waiting on a detach this layer cannot issue.
func TestVendorBackgroundedTaskStartIsNoDetachment(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := dbWorkspace(t, w)

	initial, stream := dbOpenRootFeed(t, w, ws)
	defer stream.Close()

	// Act
	turn := SubmitPrompt(t, w, ws, "!vendor-backgrounded")

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()

	// Assert: the unit is drawn as a live FOREGROUND tool call of this turn.
	const command = "tail -f /var/log/system.log"
	running := func(row *frontendv1.FeedRow) bool {
		call := row.GetActivity().GetSimpleToolCall()
		return row.GetTurn().GetValue() == turn.GetValue() &&
			call.GetName().GetText() == "Bash" &&
			strings.Contains(call.GetInput().GetText(), command) &&
			call.GetRunning() != nil
	}
	sawRunning := false
	for _, row := range initial {
		if running(row) {
			sawRunning = true
		}
	}
	if !sawRunning {
		harness.AwaitView(t, ctx, stream, "the live FOREGROUND Bash unit the vendor is tracking as a task", running)
	}

	// Assert: for one probe window past that, the announced `local_bash`
	// task draws NO detached row and the unit never settles.
	//
	// The window is harness.ProbeWindow (500ms), the suite's standing bound
	// for a negative, and it is the right one here: `task_started` and
	// `background_tasks_changed` are emitted synchronously right behind the
	// tool_use whose row the wait above already observed round-tripping the
	// whole stack, so they are in the pipe ahead of this drain rather than
	// racing it.
	dbExpectForegroundUnitHolds(t, stream, command)
}

// dbExpectForegroundUnitHolds drains the feed for one probe window and fails
// if the named command is ever drawn as detached work, or if its foreground
// tool call leaves the running state. The two halves of "the vendor tracks it
// but has not moved it", checked together so one drain covers both.
func dbExpectForegroundUnitHolds(t *testing.T, stream *harness.Stream[*frontendv1.FeedRow], commandSubstring string) {
	t.Helper()
	timer := time.NewTimer(harness.ProbeWindow)
	defer timer.Stop()
	for {
		select {
		case row, ok := <-stream.C:
			if !ok {
				return
			}
			if sh := row.GetDetachedShell().GetShell(); sh != nil &&
				strings.Contains(sh.GetCommand().GetText(), commandSubstring) {
				t.Fatalf("a detached_shell row was drawn for %q from the vendor's `task_started` alone: "+
					"a shell task is not a detachment yet, and the Bash result is the one record that states "+
					"the cause (convert/detached.ts)", commandSubstring)
			}
			call := row.GetActivity().GetSimpleToolCall()
			if call == nil || !strings.Contains(call.GetInput().GetText(), commandSubstring) {
				continue
			}
			if call.GetRunning() == nil {
				t.Fatalf("the foreground Bash unit %q settled with %v, want it to stay running: the vendor "+
					"parked it awaiting a DetachForeground no caller-facing rpc can issue, so nothing has "+
					"ended it", commandSubstring, call.GetOutcome())
			}
		case <-timer.C:
			return
		}
	}
}
