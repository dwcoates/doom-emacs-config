// Package e2e — hibernation / keepalive area (SPEC.md section C, "Hibernation
// / keepalive", entries #44-46). Contract: docs/overhaul/daemon.md's
// "Hibernation is daemon POLICY..." / "SPAWN ON MOUNT" bullets (the
// "Package map" section) and its "Queue, holds, leases" bullet listing the
// three genuine daemon holds; docs/overhaul/shim.md's `Hibernate` rpc bullet
// (service.proto section) and its "Keep-alives (entirely shim-internal)"
// section; docs/overhaul/store.md's keep-alive-exclusion bullets; PROTO-
// CHANGES.md's `HibernateError.kind {turn_in_flight, compaction_failed
// {error}, no_session}` line.
//
// Scenario driven: the DEFAULT prose scenario, reached by submitting plain
// text with no "!name" prefix (SPEC.md section B: "a plain prompt with no
// !name prefix falls through to the default prose scenario, unchanged").
// None of these three tests needs a NAMED fake-SDK scenario — hibernation and
// keep-alives are session/process-level mechanisms the daemon and the real
// shim's `--fake` engine drive on their own, not something a turn's own
// prompt text selects. Two DOCUMENTED, already-tested levers the real shim's
// fake engine exposes for exactly this reason are used instead of any
// invented mechanism:
//
//   - AGENT_REPL_FAKE_TURN_GATE / AGENT_REPL_FAKE_TURN_GATE_TEXT
//     (agent-shim/claude/shim/src/main.ts... no: src/fake/index.ts's
//     TURN_GATE_PATH_ENV/TURN_GATE_TEXT_ENV) parks a turn whose prompt text
//     matches the gate until a file appears, so a test can arrange "this turn
//     is STILL RUNNING while the idle sweep fires" deterministically instead
//     of racing a mock that answers in microseconds. Already exercised by the
//     shim's own unit suite: agent-shim/claude/shim/test/fake/index.test.ts's
//     "the turn gate" describe block.
//   - AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS (src/main.ts's
//     FAKE_KEEPALIVE_INTERVAL_ENV) compresses the keep-alive cadence — four
//     minutes in production, against a five-minute vendor cache tier — to a
//     value this suite can wait out. Honored ONLY under `--fake`, which this
//     suite's daemon always runs with (world_test.go's NewWorld).
//
// UNREACHABLE, noted rather than fabricated (SPEC.md section F, item 1's
// disposition style: report a reachability question rather than force it):
// HibernateError.kind.compaction_failed{error} and HibernateError.kind.
// no_session. Hibernate carries no prompt text (it resolves before any turn
// exists), so — exactly like ruling 3's disposition of
// StartSessionFailure.cause.vendor_start_failed — no scenario-prompt selector
// could ever reach them. Unlike vendor_start_failed, the fake SDK's
// AGENT_REPL_FAKE_REFUSE lever's REFUSABLE set (src/fake/index.ts) does not
// include a Hibernate-refusing verb, and no other documented, already-tested
// lever forces the shim's own compaction call to fail or reports "no
// session" from inside a live Hibernate round trip. Provoking either for real
// would need a genuine race (killing the session at the exact instant the
// sweep calls Hibernate) this suite has no deterministic way to win, or a new
// production lever outside a compile-gate-only e2e test's mandate to add.
// This test therefore exercises turn_in_flight (reachable for real via the
// turn gate) and the plain success path, and leaves compaction_failed /
// no_session unexercised — an open question for the project lead, not a
// silently dropped arm.
package e2e

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// ---------------------------------------------------------------------------
// The turn gate (agent-shim/claude/shim/src/fake/index.ts's TURN_GATE_PATH_ENV
// / TURN_GATE_TEXT_ENV, documented and unit-tested there — see this file's
// header comment). A turn whose FULL submitted text matches the gate text
// does not begin emitting until the gate path exists on disk.
// ---------------------------------------------------------------------------

const (
	turnGatePathEnv = "AGENT_REPL_FAKE_TURN_GATE"
	turnGateTextEnv = "AGENT_REPL_FAKE_TURN_GATE_TEXT"
)

// ---------------------------------------------------------------------------
// The keep-alive cadence override (agent-shim/claude/shim/src/main.ts's
// FAKE_KEEPALIVE_INTERVAL_ENV — see this file's header comment).
// ---------------------------------------------------------------------------

const fakeKeepaliveIntervalEnv = "AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS"

// fakeKeepaliveIntervalMS is the compressed cadence TestKeepAliveNeverAppearsOnWire
// waits out. Small enough that keepAliveObservationWindow's margin stays
// generous, large enough to stay well clear of pollInterval (world_test.go,
// 20ms) so a slow CI host cannot make the override read as "faster than the
// suite polls".
const fakeKeepaliveIntervalMS = 100

// keepAliveObservationWindow bounds the wait for a SECOND durable cursor
// advance with NO second SubmitPrompt in between — the store's own proof
// that a real keep-alive turn's transcript bytes were durably ingested by
// the sidecar (see awaitCursorAdvance, world_test.go). Sized at 20x
// fakeKeepaliveIntervalMS, matched to this suite's other real-process
// margins (StoreOutageWindow is likewise an order of magnitude above the
// window it bounds) rather than an untested guess: the sidecar's own
// --poll-interval/--rescan-interval (50ms/200ms, world_test.go's
// startSidecar) both sit comfortably inside it.
const keepAliveObservationWindow = 2 * time.Second

// ---------------------------------------------------------------------------
// The idle-cutoff hibernation tests' own budget (SPEC.md section B, "Waits"):
// reused verbatim from HandoverChainTimeout rather than a same-value twin —
// see that constant's own doc comment (world_test.go). The shape matches:
// a real shim spawn, a real Hibernate/KillSession round trip, and (for
// TestRevivalAfterHibernate) a SECOND real StartSession(resume) spawn all
// chain onto one budget, exactly "two real process lifecycles on one
// budget".
// ---------------------------------------------------------------------------

const HibernationChainTimeout = HandoverChainTimeout

// hibernationIdleCutoffMS matches daemon/integration's own precedent
// (session_lifecycle_test.go's TestHibernationParksAnIdleSessionAndRevivesOnPrompt
// and its sibling tests all use IdleCutoffMS: 50) — small enough that the
// sweep fires promptly, and safe because the daemon's own drain/api.go caps
// its sweep interval at the idle cutoff itself (SweepEvery is never left
// larger than IdleCutoff), so a compressed cutoff also compresses how often
// the sweep looks.
const hibernationIdleCutoffMS = 50

// The daemon's own idle-sweep log operation and the two message strings this
// area asserts on (daemon/internal/drain/sweep.go's opSweep = "daemon.drain.
// sweep"; the exact wording is independently confirmed by daemon/integration/
// session_lifecycle_test.go's own header comment on
// TestHibernateTurnInFlightRefusalDefersTheStandDown, which quotes "a turn is
// in flight; deferring the hibernation" verbatim). This is the daemon's OWN
// structured log — unlike harness.AwaitShimLoggedRequest (which reads the
// FAKE shim's control-socket recorder and does not exist for the real shim
// this suite spawns), it is produced regardless of which shim is real.
const (
	hibernateSweepOp         = "daemon.drain.sweep"
	hibernateTurnInFlightMsg = "a turn is in flight; deferring the hibernation"
	hibernateSucceededMsg    = "hibernated an idle session"
)

// awaitHibernateSweepRecord polls a workspace's own daemon log sink until a
// hibernateSweepOp record with the given message appears, bounded by
// HibernationChainTimeout. It is this file's own bounded-wait primitive
// because harness.Daemon.AwaitWorkspaceLogRecord has no per-call timeout
// (it relies on the daemon's own unbounded context) — this area's own named
// budget is the "per-site override" SPEC.md section B calls for.
func awaitHibernateSweepRecord(t *testing.T, w *World, workspaceDir, what, message string) harness.LogRecord {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), HibernationChainTimeout)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		for _, r := range w.WorkspaceLog(workspaceDir, "daemon") {
			if r.Operation == hibernateSweepOp && r.Message == message {
				return r
			}
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			t.Fatalf("waiting for %s: %v", what, ctx.Err())
		}
	}
}

// shimDetached reports whether the host workspace view shows a live,
// existing session with no shim attached — the daemon.md-documented shape of
// a park ("the host stream keeps the workspace LIVE with the shim
// unattached, which is the whole of what a park is allowed to show").
func shimDetached(r *agentreplv1.WatchHostWorkspaceResponse) bool {
	live := r.GetHost().GetExisting().GetLive()
	return live != nil && !live.GetShimAttached()
}

// feedRows opens the workspace's feed and answers its current page's rows —
// a read (OpenFeed), never a store write; the grep gate only forbids the
// store's own WriteBatch verb and hand-authored vendor-transcript paths.
func feedRows(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) []*frontendv1.FeedRow {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	return success.GetPage().GetSuccess().GetRows()
}

// distinctTurnIDs answers the set of turn ids a feed page's rows attribute
// to any turn — used to assert a keep-alive turn contributes NONE.
func distinctTurnIDs(rows []*frontendv1.FeedRow) map[string]bool {
	out := map[string]bool{}
	for _, r := range rows {
		if id := r.GetTurn().GetValue(); id != "" {
			out[id] = true
		}
	}
	return out
}

// TestHibernateOnIdleCutoff — SPEC.md #44. Contract: docs/overhaul/daemon.md
// ("Hibernation is daemon POLICY (idle-cutoff sweep + implicit revive on
// prompt)... before standing a shim down for hibernation the daemon calls
// the shim's Hibernate directive") and PROTO-CHANGES.md's
// HibernateError.kind.turn_in_flight arm. Exercises, for real:
//
//  1. turn_in_flight: a turn parked on the documented turn gate holds real
//     work open across the compressed idle cutoff; the sweep must see the
//     shim's real refusal and defer rather than forcing a stand-down.
//  2. The plain success path once that turn closes and the session goes
//     idle again: the sweep's Hibernate/KillSession round trip succeeds, and
//     the host view settles on the documented park shape (live, shim
//     detached).
//
// compaction_failed and no_session are NOT exercised — see this file's
// header comment for why neither is reachable without fabrication.
func TestHibernateOnIdleCutoff(t *testing.T) {
	// Arrange
	gatePath := filepath.Join(t.TempDir(), "turn-gate")
	const gateText = "hibernation e2e turn gate: TestHibernateOnIdleCutoff"
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		IdleCutoffMS: hibernationIdleCutoffMS,
		ExtraEnv: []string{
			turnGatePathEnv + "=" + gatePath,
			turnGateTextEnv + "=" + gateText,
		},
	}})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	host := w.WatchHost(ws)
	defer host.Close()

	// Act: submit the gated turn. It parks before emitting anything, so it
	// stays genuinely in flight until this test releases it.
	gatedTurn := SubmitPrompt(t, w, ws, gateText)

	// Assert: the sweep, firing every hibernationIdleCutoffMS, sees a real
	// turn_in_flight refusal from the real shim and defers — never forcing a
	// stand-down out from under live work.
	awaitHibernateSweepRecord(t, w, ws.GetDir(),
		"the sweep deferring hibernation for the in-flight gated turn",
		hibernateTurnInFlightMsg)

	// Act: release the gate; the turn ends the ORDINARY way (the gate never
	// changes what the turn is — src/fake/index.ts's own doc comment).
	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("release the turn gate: %v", err)
	}
	AwaitTurnEnded(t, w, ws, gatedTurn)

	// Assert: with the turn closed, the session goes idle again and the SAME
	// sweep now succeeds — a real Hibernate ack followed by a graceful
	// (non-forced) KillSession.
	awaitHibernateSweepRecord(t, w, ws.GetDir(),
		"the idle sweep hibernating the now-idle session", hibernateSucceededMsg)

	// Assert: the host view settles on the documented park shape — the
	// session still exists (never severed, never dead) with its shim
	// detached.
	hostCtx, cancel := context.WithTimeout(w.Ctx(), HibernationChainTimeout)
	defer cancel()
	harness.AwaitView(t, hostCtx, host,
		"the host reporting the parked workspace with its shim detached", shimDetached)
}

// TestKeepAliveNeverAppearsOnWire — SPEC.md #45. Contract: docs/overhaul/
// shim.md's "Keep-alives (entirely shim-internal)" section ("nothing
// keep-alive-shaped exists on the wire (no PromptOrigin value, no rpc, no
// control-plane signal)... Keep-alive turns are first-class in the store as
// NEVER-SERVED: indexed so no page returns them") and docs/overhaul/store.md
// ("No keep-alive row is ever returned by any page").
//
// Asserts by ABSENCE, but only after PROVING a keep-alive turn actually ran:
// a compressed keep-alive cadence (AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS)
// makes a real keep-alive turn's transcript bytes land durably in the store
// with NO second SubmitPrompt from this test — read via the same durable
// cursor-advance proof driveScenarioToCompletion already uses
// (world_test.go's awaitCursorAdvance). Only once that is proven does the
// test assert the daemon's own feed carries no trace of it, and that the
// wire has no PromptOrigin arm a keep-alive could even be attributed to.
func TestKeepAliveNeverAppearsOnWire(t *testing.T) {
	// Arrange
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		ExtraEnv: []string{fmt.Sprintf("%s=%d", fakeKeepaliveIntervalEnv, fakeKeepaliveIntervalMS)},
	}})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: one real turn establishes a live session, whose keep-alive cadence
	// begins before StartSession even returns success (shim.md).
	turn1 := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")

	// Act: wait for a SECOND durable cursor advance with NO second prompt —
	// proof a real keep-alive turn's bytes were durably ingested.
	projectDir := harness.ProjectDir(w.DefaultConfigDir, ws.GetDir())
	cursorCtx, cancel := context.WithTimeout(w.Ctx(), keepAliveObservationWindow)
	defer cancel()
	baseline := cursorOffsetsUnder(w.Store.Cursors(t, cursorCtx), projectDir)
	awaitCursorAdvance(t, w, projectDir, baseline)

	// Assert: the daemon's own feed carries EXACTLY the one real turn — the
	// keep-alive turn that just, provably, ran is not a distinct feed row.
	turns := distinctTurnIDs(feedRows(t, w, ws))
	if want := (map[string]bool{turn1.GetValue(): true}); !reflect.DeepEqual(turns, want) {
		t.Fatalf("feed turns while a keep-alive is provably running = %v, want exactly %v", turns, want)
	}

	// Assert: structurally, there is no PromptOrigin arm a keep-alive could
	// ever be attributed to — every prompt any real client could submit
	// carries one of these named origins, and none of them names a
	// keep-alive (shim.md: "the one shared file... has no keep-alive value
	// on purpose").
	for _, name := range conversationv1.PromptOrigin_name {
		if strings.Contains(name, "KEEPALIVE") || strings.Contains(name, "KEEP_ALIVE") {
			t.Fatalf("conversation.v1.PromptOrigin carries a keep-alive arm (%s) — the control plane is supposed to have none", name)
		}
	}
}

// TestRevivalAfterHibernate — SPEC.md #46. Contract: the yield obligation
// (docs/overhaul/shim.md: "a real prompt submitted after trailing keep-alive
// turns → the served context excludes them") combined with daemon.md's
// SPAWN-ON-MOUNT / implicit-revive-on-prompt hibernation policy: "before
// standing a shim down for hibernation the daemon calls the shim's Hibernate
// directive — the shim compacts, then acks; only after the ack does the
// stand-down proceed, so revival never pays a cold context."
//
// Drives a real hibernate/revive round trip: one real turn, a real
// idle-cutoff hibernation (the plain success path TestHibernateOnIdleCutoff
// already proves in detail — this test only needs it as a precondition), and
// a second real turn submitted afterward, which the daemon must revive by
// resuming rather than refusing.
//
// GROUNDING NOTE: whether the vendor's own underlying conversation state was
// actually rolled back past any keep-alive turns is not independently
// observable through the fake SDK's scripted, stateless scenarios — each
// scenario's response is fixed by its name, not shaped by prior context, so
// no assertion here could distinguish "context was rewound" from "context
// was never examined." What IS asserted is every daemon-visible fact the
// yield obligation and the no-cold-context-on-revival guarantee actually
// promise: revival succeeds without a cold-context refusal (SubmitPrompt's
// own helper fails the test on any refusal), and the feed across the
// hibernate/revive boundary carries exactly the two real turns, in order,
// with nothing from the idle interval's real keep-alive activity leaking
// onto it.
func TestRevivalAfterHibernate(t *testing.T) {
	// Arrange
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{IdleCutoffMS: hibernationIdleCutoffMS}})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	host := w.WatchHost(ws)
	defer host.Close()

	// Act: one real turn, then let the compressed idle cutoff hibernate the
	// session for real.
	turn1 := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")
	awaitHibernateSweepRecord(t, w, ws.GetDir(),
		"the idle sweep hibernating the session before revival", hibernateSucceededMsg)
	hostCtx, cancel := context.WithTimeout(w.Ctx(), HibernationChainTimeout)
	defer cancel()
	harness.AwaitView(t, hostCtx, host,
		"the host reporting the parked workspace with its shim detached before revival", shimDetached)

	// Act: a real prompt after hibernation is an implicit revival
	// (daemon.md's SPAWN ON MOUNT / "implicit revive on prompt"). SubmitPrompt
	// (via driveScenarioToCompletion) fails the test on any refusal, so its
	// clean return already IS the assertion that revival paid no
	// cold-context cost.
	turn2 := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")

	// Assert: the feed shows exactly turn1 then turn2, in that order, with no
	// third turn (a leaked keep-alive, or anything else) ever appearing
	// between or around them.
	sawTurn1End := false
	for _, r := range feedRows(t, w, ws) {
		switch id := r.GetTurn().GetValue(); id {
		case "":
			// Non-turn rows (separators, cold gates, etc.) carry no turn id
			// and are not this assertion's subject.
		case turn1.GetValue():
			if r.GetTurnEnded() != nil {
				sawTurn1End = true
			}
		case turn2.GetValue():
			if !sawTurn1End {
				t.Fatalf("a feed row for the post-revival turn (%s) appeared before the pre-hibernation turn (%s) ended",
					turn2.GetValue(), turn1.GetValue())
			}
		default:
			t.Fatalf("the feed carries an unexpected turn id %q across the hibernate/revive boundary — want only %s and %s",
				id, turn1.GetValue(), turn2.GetValue())
		}
	}
	if !sawTurn1End {
		t.Fatalf("the feed never showed the pre-hibernation turn (%s) ending", turn1.GetValue())
	}
}
