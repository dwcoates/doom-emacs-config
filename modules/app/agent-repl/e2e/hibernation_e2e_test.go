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
	"database/sql"
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

	_ "modernc.org/sqlite"
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
	hibernateSweepOp = "daemon.drain.sweep"
	// The SWEEP's own freeness pre-check, which runs BEFORE any Hibernate
	// directive is sent (daemon/internal/drain/sweep.go:64-66). A session
	// with live work never reaches the shim at all, so this — not the
	// shim's own refusal — is the record a turn-in-flight deferral leaves.
	hibernateNotFreeMsg   = "the idle session is not free; deferring its hibernation"
	hibernateSucceededMsg = "hibernated an idle session"
)

// sweepMark is a position in a workspace's own daemon sink: the number of
// records it already held when the mark was taken. A wait carrying a mark
// reads only what the daemon wrote AFTER it, so a test can never be satisfied
// by a record that predates the act it is waiting on.
//
// THIS AREA NEEDS ONE, and the need is a measurement rather than a caution.
// The daemon spawns a session ON MOUNT (daemon.md's SPAWN ON MOUNT), and a
// mount-spawned session that is never prompted goes idle at
// hibernationIdleCutoffMS like any other — so on a quiet box the sweep stands
// that session down BEFORE the test has submitted anything. Observed, from
// TestRevivalAfterHibernate's own sink on an idle host: "hibernated an idle
// session" at T+0.368s, "the session was engaged inside the cutoff" (the
// prompt queue's engagement stamp, promptqueue/deliver.go's touchEngagement)
// at T+0.522s. An unmarked wait for that message therefore matched a
// stand-down of a session the test's first turn had not yet run on, and only
// a SATURATED box — where the first prompt lands inside the 50ms cutoff, so
// no such record exists — made the test wait for the hibernation it actually
// names. Waiting on the wrong fact is what made the pass load-dependent.
type sweepMark int

// markSweepLog answers the current position of a workspace's daemon sink.
func markSweepLog(w *World, workspaceDir string) sweepMark {
	return sweepMark(len(w.WorkspaceLog(workspaceDir, "daemon")))
}

// sweepRecordSince answers the first hibernateSweepOp record with the given
// message that the sink holds AT OR AFTER mark. It is a pure function so the
// marking rule itself is tested rather than only exercised.
func sweepRecordSince(records []harness.LogRecord, mark sweepMark, message string) (harness.LogRecord, bool) {
	if int(mark) < 0 {
		mark = 0
	}
	for i := int(mark); i < len(records); i++ {
		if records[i].Operation == hibernateSweepOp && records[i].Message == message {
			return records[i], true
		}
	}
	return harness.LogRecord{}, false
}

// awaitHibernateSweepRecordSince polls a workspace's own daemon log sink until
// a hibernateSweepOp record with the given message appears AFTER mark. It is
// this file's own bounded-wait primitive because
// harness.Daemon.AwaitWorkspaceLogRecord has no per-call timeout (it relies on
// the daemon's own unbounded context) — this area's own named budget is the
// "per-site override" SPEC.md section B calls for.
//
// A miss reports what the sweep DID decide, not merely that it did not decide
// what was wanted: every one of the sweep's own records is rendered, so a
// deferral (not free, already terminal, no session) is distinguishable from a
// sweep that never ran at all without a second run to find out.
func awaitHibernateSweepRecordSince(t *testing.T, w *World, workspaceDir string, mark sweepMark, what, message string) harness.LogRecord {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), HibernationChainTimeout)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if r, found := sweepRecordSince(w.WorkspaceLog(workspaceDir, "daemon"), mark, message); found {
			return r
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			t.Fatalf("waiting for %s: %v (the world's own budget: %v)\nthe sweep's own records for this workspace:\n%s",
				what, ctx.Err(), w.Ctx().Err(), sweepRecordDigest(w, workspaceDir))
		}
	}
}

// sweepRecordDigest renders every idle-sweep record a workspace's own daemon
// sink carries, so a missed hibernation reports WHAT THE SWEEP DECIDED.
func sweepRecordDigest(w *World, workspaceDir string) string {
	var b strings.Builder
	for i, r := range w.WorkspaceLog(workspaceDir, "daemon") {
		if r.Operation != hibernateSweepOp {
			continue
		}
		fmt.Fprintf(&b, "  [%d] %s %s %s %v\n", i, r.Timestamp, r.Level, r.Message, r.Context)
	}
	if b.Len() == 0 {
		return "  (the sweep recorded nothing at all about this workspace)"
	}
	return b.String()
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
// the shim's Hibernate directive"). Exercises, for real:
//
//  1. A turn parked on the documented turn gate holds real work open across
//     the compressed idle cutoff, and the sweep defers rather than forcing a
//     stand-down. The deferral is the SWEEP'S OWN freeness pre-check
//     (daemon/internal/drain/sweep.go:64-66), which short-circuits before
//     any Hibernate directive is sent — so HibernateError.turn_in_flight,
//     the shim-side refusal, is never reached from this path (recorded as an
//     open item in SPEC.md §G).
//  2. The plain success path once that turn closes and the session goes
//     idle again: the sweep's Hibernate/KillSession round trip succeeds, and
//     the host view settles on the documented park shape (live, shim
//     detached).
//
// compaction_failed and no_session are NOT exercised — see this file's
// header comment for why neither is reachable without fabrication.
func TestHibernateOnIdleCutoff(t *testing.T) {
	t.Parallel()
	// Arrange
	gatePath := filepath.Join(t.TempDir(), "turn-gate")
	const gateText = "hibernation e2e turn gate: TestHibernateOnIdleCutoff"
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		// HibernationChainTimeout IS THIS WORLD'S BUDGET, not just the
		// individual waits'. See that constant's own comment: a `WithTimeout`
		// derived from w.Ctx() cannot outlive w.Ctx(), so a 15s wait inside a
		// 5s world is bounded by whatever the world has left, and the bound
		// declared at the wait was never the bound in force. MEASURED: this
		// test's whole chain consumes 0.36s of the world budget in a quiet
		// full-package run at -parallel 8, and 2.15s at a 1-minute load of
		// 88 — 2.3x the ordinary DefaultTimeout margin away, close enough
		// that the first wait to outrun the remainder reports a bare
		// "context deadline exceeded" that says nothing about the sweep.
		Timeout:      HibernationChainTimeout,
		IdleCutoffMS: hibernationIdleCutoffMS,
		ExtraEnv: []string{
			turnGatePathEnv + "=" + gatePath,
			turnGateTextEnv + "=" + gateText,
		},
	}})
	// Standing the shim down at the idle cutoff leaves the session with no
	// producer, which opens a health fault; that park is this test's
	// subject. The link records beside it are the same act seen from the
	// connectivity layer: the shim PROCESS goes, so the client's socket drops
	// and it redials until the reap tells it the process is gone.
	//
	// THE STAND-DOWN'S KILL ROUND TRIP IS DECLARED TOO, for the reason
	// refusals_e2e_test.go's rfVendorStartFaultWarnings states: the sweep
	// bounds that call at drain.DefaultStandBound, and a saturated parallel
	// run can spend it (observed once). Fleet.KillSession already handles the
	// silence — "a shim that will not answer is not a reason to leave the
	// process running", so it stops the process anyway and records the
	// refusal as evidence — and an undeclared record whose observation is a
	// race is a flake, not a finding. `daemon.shimclient.exit` is the same
	// act at the process reaper: the shim can exit as its session ends,
	// ahead of the kill this stand-down sends it.
	w.ExpectWarnings("daemon.health.open_fault",
		"daemon.sessionwatcher.link_fault", "daemon.shimclient.redial",
		"daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.shimclient.exit")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	host := w.WatchHost(ws)
	defer host.Close()

	// Act: submit the gated turn. It parks before emitting anything, so it
	// stays genuinely in flight until this test releases it.
	//
	// THE MARK IS TAKEN FIRST, so neither assertion below can be satisfied by
	// the sweep's verdict on the MOUNT-SPAWNED session — which, never having
	// been prompted, is itself idle past the cutoff and gets stood down on a
	// fast box before this line runs (see sweepMark).
	mark := markSweepLog(w, ws.GetDir())
	gatedTurn := SubmitPrompt(t, w, ws, gateText)

	// Assert: the sweep, firing every hibernationIdleCutoffMS, sees a real
	// turn_in_flight refusal from the real shim and defers — never forcing a
	// stand-down out from under live work.
	awaitHibernateSweepRecordSince(t, w, ws.GetDir(), mark,
		"the sweep deferring hibernation for the in-flight gated turn",
		hibernateNotFreeMsg)

	// Act: release the gate; the turn ends the ORDINARY way (the gate never
	// changes what the turn is — src/fake/index.ts's own doc comment).
	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("release the turn gate: %v", err)
	}
	AwaitTurnEnded(t, w, ws, gatedTurn)

	// Assert: with the turn closed, the session goes idle again and the SAME
	// sweep now succeeds — a real Hibernate ack followed by a graceful
	// (non-forced) KillSession.
	awaitHibernateSweepRecordSince(t, w, ws.GetDir(), mark,
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
	// This scenario deliberately waits for an idle keep-alive after completing
	// a full real-process turn. Running it beside seven other process worlds
	// repeatedly exhausted the baseline GetSidecarCursors deadline before the
	// assertion's own observation window began. Keep this one world serialized
	// so the stated cursor bounds measure the product rather than host contention.
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
	// The baseline is one ordinary store RPC, not part of the post-baseline
	// keep-alive observation window. Give that RPC the suite's ordinary bound;
	// awaitCursorAdvanceWithin owns the exact 2s observation bound below.
	cursorCtx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	baseline := cursorOffsetsUnder(w.Store.Cursors(t, cursorCtx), projectDir)
	// ON keepAliveObservationWindow, NOT DefaultTimeout. The context above
	// bounded only the baseline read, so the wait this constant documents ran
	// on the suite-wide budget instead — the bound stated here was never the
	// bound in force.
	awaitCursorAdvanceWithin(t, w, projectDir, baseline, keepAliveObservationWindow)

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

// TestKeepAliveAnswerAfterVendorTurnNeverServed — the owner's leak of
// 2026-09-23. The vendor ran a turn of its OWN (a background task's
// notification) between the shim's keep-alive send and its answer; that
// turn's result closed the keep-alive early, and the keep-alive's answer then
// arrived untagged and was drawn as a green final-answer bubble.
//
// `!queue-vendor-turn` makes the mocked vendor run exactly such a turn ahead
// of the next send, and the compressed cadence makes that next send the
// shim's keep-alive. The mock ECHOES a prompt into its reply, so a served row
// of the keep-alive's answer would carry the keep-alive marker.
//
// SYNCHRONIZATION: the shim's own "closed a turn" record with keepalive=true
// proves the keep-alive was answered; a real prompt submitted AFTER it and
// seen to end in the feed proves every row the shim wrote before it — the
// keep-alive's included — reached the store, because the shim writes in
// order. Only then is absence asserted.
func TestKeepAliveAnswerAfterVendorTurnNeverServed(t *testing.T) {
	// Arrange
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		ExtraEnv: []string{fmt.Sprintf("%s=%d", fakeKeepaliveIntervalEnv, fakeKeepaliveIntervalMS)},
	}})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	queued := SubmitPrompt(t, w, ws, "!queue-vendor-turn")
	AwaitTurnEnded(t, w, ws, queued)

	// Act: the keep-alive beats, the vendor runs its own turn first, then
	// answers the keep-alive.
	w.Daemon.AwaitLogRecord(harness.WorkspaceLogPath(repo.Dir, "shim"), "a keep-alive turn to close",
		func(r harness.LogRecord) bool { return r.Message == "closed a turn" && r.Context["keepalive"] == true })
	after := SubmitPrompt(t, w, ws, "after the keep-alive")
	AwaitTurnEnded(t, w, ws, after)

	// Assert: the vendor's own turn is served; nothing of the keep-alive is.
	rows := feedRows(t, w, ws)
	var vendorTurnDrawn bool
	for _, row := range rows {
		drawn := row.String()
		if strings.Contains(drawn, "agent-repl:keepalive") {
			t.Errorf("feed row %v carries the keep-alive marker, want no keep-alive row served", row)
		}
		if strings.Contains(drawn, "A background task finished.") {
			vendorTurnDrawn = true
		}
	}
	if !vendorTurnDrawn {
		t.Errorf("feed drew no row for the vendor's own turn, want it served beside the hidden keep-alive")
	}
}

// TestKeepAliveStoresNothingOnEitherPlane — nothing of a keep-alive is stored
// (2026-09-23). The shim drops every keep-alive-tagged entry at its writer's
// door, and the sidecar skips the turn's transcript records by the marker plus
// the transcript's promptId and parentUuid links, so the real store holds no
// row of the keep-alive and neither plane ever meets the other's copy of a key
// under a different kind.
//
// SYNCHRONIZATION: the shim's "closed a turn" record with keepalive=true proves
// a keep-alive ran to its answer; a real prompt submitted after it, seen to end
// in the feed, proves the shim's ordered writer delivered everything before it;
// and a sidecar cursor advancing past the real turn's baseline proves the file
// plane read the keep-alive's transcript bytes, which precede it. Only then is
// absence asserted — in the store's own database, where an unserved row would
// be, since no read verb ever returns one.
func TestKeepAliveStoresNothingOnEitherPlane(t *testing.T) {
	// Arrange
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		ExtraEnv: []string{fmt.Sprintf("%s=%d", fakeKeepaliveIntervalEnv, fakeKeepaliveIntervalMS)},
	}})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	projectDir := harness.ProjectDir(w.DefaultConfigDir, ws.GetDir())
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")

	// Act: a keep-alive runs to its answer, then a real turn follows it.
	w.Daemon.AwaitLogRecord(harness.WorkspaceLogPath(repo.Dir, "shim"), "a keep-alive turn to close",
		func(r harness.LogRecord) bool { return r.Message == "closed a turn" && r.Context["keepalive"] == true })
	driveDocumentedPrompt(t, w, ws, w.DefaultConfigDir, "after the keep-alive")

	// Assert: no row of the keep-alive, on either arm it could have taken.
	db, err := sql.Open("sqlite", "file:"+w.Store.DBPath+"?mode=ro")
	if err != nil {
		t.Fatalf("e2e: opening the store database read-only: %v", err)
	}
	defer db.Close()
	var keepaliveKind, carryingMarker int
	if err := db.QueryRowContext(w.Ctx(), `SELECT COUNT(*) FROM entry WHERE kind = 'keepalive'`).Scan(&keepaliveKind); err != nil {
		t.Fatalf("e2e: counting keepalive-kind rows: %v", err)
	}
	if err := db.QueryRowContext(w.Ctx(),
		`SELECT COUNT(*) FROM entry WHERE instr(frame, CAST('agent-repl:keepalive' AS BLOB)) > 0`).Scan(&carryingMarker); err != nil {
		t.Fatalf("e2e: counting rows carrying the keep-alive marker: %v", err)
	}
	if keepaliveKind != 0 || carryingMarker != 0 {
		t.Errorf("the store holds %d keepalive-kind row(s) and %d row(s) carrying the keep-alive marker under %s, want none",
			keepaliveKind, carryingMarker, projectDir)
	}

	// Assert: neither plane was refused a kind change on any key.
	for _, source := range []struct {
		name    string
		records []harness.LogRecord
	}{
		{"store", harness.ReadLog(t, w.Store.LogPath)},
		{"sidecar", w.Sidecar.Log(t)},
	} {
		for _, r := range source.records {
			if strings.Contains(r.Message, "would change the row's kind") || strings.Contains(fmt.Sprint(r.Context), "upsert_changes_identity") {
				t.Errorf("the %s logged a kind-change refusal: %s %v", source.name, r.Message, r.Context)
			}
		}
	}
}

// TestPromptDuringKeepAliveTurnsOnce — a real prompt submitted the moment the
// shim has sent its own keep-alive (2026-09-23). The keep-alive is invisible
// outside the shim: a StartTurn landing while it runs waits INSIDE the shim and
// opens its turn once the keep-alive leaves the slot, so the daemon sees no
// refusal, re-drives nothing, and holds nothing.
//
// SYNCHRONIZATION: the shim's own "keepalive_submitted" record is the moment
// the prompt is submitted after. Whether the StartTurn then lands inside the
// keep-alive or just after it is the vendor's timing, and every assertion here
// holds either way: SubmitPrompt fails the test on any refusal, the turn must
// end, and the feed must carry exactly the real turns, each once.
func TestPromptDuringKeepAliveTurnsOnce(t *testing.T) {
	// Arrange
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		ExtraEnv: []string{fmt.Sprintf("%s=%d", fakeKeepaliveIntervalEnv, fakeKeepaliveIntervalMS)},
	}})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	turn1 := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")
	w.Daemon.AwaitLogRecord(harness.WorkspaceLogPath(repo.Dir, "shim"), "a keep-alive to be submitted",
		func(r harness.LogRecord) bool { return r.Context["outcome"] == "keepalive_submitted" })

	// Act
	during := SubmitPrompt(t, w, ws, "during the keep-alive")
	AwaitTurnEnded(t, w, ws, during)

	// Assert: exactly the two real turns, and the prompt drawn once.
	rows := feedRows(t, w, ws)
	if got, want := distinctTurnIDs(rows), (map[string]bool{turn1.GetValue(): true, during.GetValue(): true}); !reflect.DeepEqual(got, want) {
		t.Errorf("feed turns = %v, want exactly %v", got, want)
	}
	var drawn int
	for _, row := range rows {
		if row.GetUserPrompt() != nil && strings.Contains(row.String(), "during the keep-alive") {
			drawn++
		}
	}
	if drawn != 1 {
		t.Errorf("the prompt submitted during the keep-alive was drawn %d time(s), want exactly once", drawn)
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
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		// HibernationChainTimeout IS THIS WORLD'S BUDGET — the same reason
		// TestHibernateOnIdleCutoff states, with this test's own figures: it
		// spends 0.50s of the world budget quiet and 3.34s at a 1-minute load
		// of 88, leaving DefaultTimeout only ~1.5x margin over a chain the
		// file's own header already describes as two real process lifecycles
		// plus a third spawn.
		Timeout:      HibernationChainTimeout,
		IdleCutoffMS: hibernationIdleCutoffMS,
	}})
	// Standing a shim down and reviving it opens a health fault while the
	// session has no producer; that is what this test provokes. The link
	// records are the same stand-down seen from the connectivity layer — the
	// shim process really is gone, and it goes twice here.
	// The kill round trip is declared for the same reason
	// TestHibernateOnIdleCutoff declares it: the sweep bounds it, and a
	// saturated run can spend that bound.
	w.ExpectWarnings("daemon.health.open_fault",
		"daemon.sessionwatcher.link_fault", "daemon.shimclient.redial",
		"daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.shimclient.exit")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	host := w.WatchHost(ws)
	defer host.Close()

	// Act: one real turn, then let the compressed idle cutoff hibernate the
	// session for real.
	//
	// THE MARK IS TAKEN BEFORE THE TURN, so nothing the sweep decided before
	// this test acted can satisfy the wait below. What it cannot exclude is
	// the mount-spawned session's OWN stand-down (sweepMark), which lands
	// after the mark on a fast box; what it does guarantee is that a slow box,
	// where no such record exists, waits for the real post-turn hibernation on
	// this world's declared budget rather than on whatever a 5s default had
	// left — which is the whole of what made this test's pass load-dependent.
	mark := markSweepLog(w, ws.GetDir())
	turn1 := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")
	awaitHibernateSweepRecordSince(t, w, ws.GetDir(), mark,
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
	//
	// THE FEED BEGINS AT ITS NEWEST SEPARATION, and the revival's own
	// compaction draws one — so the pre-hibernation turn can be legitimately
	// BEHIND that divider and delivered not at all. That is the invariant
	// working, not a turn going missing, and it is the one thing this
	// assertion must allow for. Everything else it said still holds: no turn
	// but these two ever appears, no row of the post-revival turn is delivered
	// above a delivered row of the pre-hibernation one, and a pre-hibernation
	// turn that IS delivered is delivered with its ending.
	sawTurn1 := false
	sawTurn1End := false
	sawTurn2 := false
	for _, r := range feedRows(t, w, ws) {
		switch id := r.GetTurn().GetValue(); id {
		case "":
			// Non-turn rows (separators, cold gates, etc.) carry no turn id
			// and are not this assertion's subject.
		case turn1.GetValue():
			sawTurn1 = true
			if r.GetTurnEnded() != nil {
				sawTurn1End = true
			}
			if sawTurn2 {
				t.Fatalf("a feed row for the pre-hibernation turn (%s) was delivered below the post-revival turn (%s)",
					turn1.GetValue(), turn2.GetValue())
			}
		case turn2.GetValue():
			sawTurn2 = true
			if sawTurn1 && !sawTurn1End {
				t.Fatalf("a feed row for the post-revival turn (%s) appeared before the pre-hibernation turn (%s) ended",
					turn2.GetValue(), turn1.GetValue())
			}
		default:
			t.Fatalf("the feed carries an unexpected turn id %q across the hibernate/revive boundary — want only %s and %s",
				id, turn1.GetValue(), turn2.GetValue())
		}
	}
	if sawTurn1 && !sawTurn1End {
		t.Fatalf("the feed delivered the pre-hibernation turn (%s) without ever showing it end", turn1.GetValue())
	}
	if !sawTurn2 {
		t.Fatalf("the feed never showed the post-revival turn (%s)", turn2.GetValue())
	}
	t.Logf("across the hibernate/revive boundary the feed delivered the post-revival turn, and the pre-hibernation turn %s",
		map[bool]string{true: "with it", false: "not at all — it is behind the revival's own separation"}[sawTurn1])
}

// TestSweepRecordSinceIgnoresARecordBeforeTheMark is the marking rule itself,
// against the exact shape that made TestRevivalAfterHibernate's pass depend on
// how busy the box was: a "hibernated an idle session" record the daemon wrote
// BEFORE the test acted (the mount-spawned session's own stand-down) must not
// satisfy a wait for the hibernation of the session the test then ran a turn
// on. Against an unmarked scan of the same sink this is the record that was
// returned.
func TestSweepRecordSinceIgnoresARecordBeforeTheMark(t *testing.T) {
	t.Parallel()
	// Arrange
	records := []harness.LogRecord{
		{Operation: hibernateSweepOp, Message: hibernateSucceededMsg},
		{Operation: hibernateSweepOp, Message: "the workspace's session is already terminal"},
	}

	// Act
	_, found := sweepRecordSince(records, sweepMark(len(records)), hibernateSucceededMsg)

	// Assert
	if found {
		t.Fatalf("sweepRecordSince matched a record written before the mark; that record is about an act the test had not yet performed")
	}
}

// TestSweepRecordSinceFindsARecordAfterTheMark is the same rule's other half:
// the record the test's own act produces is the one returned.
func TestSweepRecordSinceFindsARecordAfterTheMark(t *testing.T) {
	t.Parallel()
	// Arrange
	before := []harness.LogRecord{{Operation: hibernateSweepOp, Message: hibernateSucceededMsg}}
	after := append(append([]harness.LogRecord{}, before...),
		harness.LogRecord{Operation: hibernateSweepOp, Message: hibernateSucceededMsg, Timestamp: "the one this test caused"})

	// Act
	got, found := sweepRecordSince(after, sweepMark(len(before)), hibernateSucceededMsg)

	// Assert
	if !found || got.Timestamp != "the one this test caused" {
		t.Fatalf("sweepRecordSince(mark=%d) = %v, %t; want the record written after the mark", len(before), got, found)
	}
}

// TestSweepRecordSinceIgnoresAnotherOperationAtTheSameMessage keeps the match
// bound to the idle sweep's OWN operation: a record from a different operation
// that happens to carry the same message is not the sweep's decision.
func TestSweepRecordSinceIgnoresAnotherOperationAtTheSameMessage(t *testing.T) {
	t.Parallel()
	// Arrange
	records := []harness.LogRecord{{Operation: "daemon.workspace.bring_up", Message: hibernateSucceededMsg}}

	// Act
	_, found := sweepRecordSince(records, 0, hibernateSucceededMsg)

	// Assert
	if found {
		t.Fatalf("sweepRecordSince matched a %q record; only %q is the idle sweep's own decision",
			"daemon.workspace.bring_up", hibernateSweepOp)
	}
}

// TestRevivalRunsInTheSessionsModeNotTheSummarizers is the hibernation
// contract's own posture clause, and it is here because a headless sandbox
// run caught it broken: after an idle-cutoff park and a revival, the
// topbar read `plan` and the revived turn ran under plan mode, which nobody
// chose.
//
// THE MECHANISM, END TO END. The daemon compacts before standing a shim down
// (`daemon.md`'s Hibernate directive, and `engine/compaction.ts`'s header).
// The compaction runs on a THROWAWAY query under `plan` so it can take no
// tools -- but that query RESUMES the user's own vendor session id, and the
// vendor records `permissionMode` on every `user` record it writes, so the
// summarizing prompt left `plan` as the last mode the transcript stated. A
// resume restores the conversation's posture from exactly that field
// (`engine/cold.ts`: "the permission mode: the last `user` line's
// permissionMode"), so the revived session came up in the summarizer's mode.
//
// The assertion is the REVIVED TURN'S OWN CONCLUSION, not a view: the fake's
// prose scenario echoes `[mode=<the mode the query is running under>]`
// (`fake/scenarios/prose.ts`), so this reads what the resumed query actually
// runs under rather than what a resolver decided to draw.
func TestRevivalRunsInTheSessionsModeNotTheSummarizers(t *testing.T) {
	t.Parallel()
	// Arrange: the same world TestRevivalAfterHibernate declares, for the same
	// reasons -- a compressed cutoff so the sweep really parks the session, and
	// the chain budget because this drives two real process lifecycles.
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		Timeout:      HibernationChainTimeout,
		IdleCutoffMS: hibernationIdleCutoffMS,
	}})
	w.ExpectWarnings("daemon.health.open_fault",
		"daemon.sessionwatcher.link_fault", "daemon.shimclient.redial",
		"daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.shimclient.exit")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	host := w.WatchHost(ws)
	defer host.Close()

	// Act: one real turn, the compressed cutoff's real park (which is what
	// runs the compaction), then a real prompt, which is the implicit revival.
	mark := markSweepLog(w, ws.GetDir())
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")
	awaitHibernateSweepRecordSince(t, w, ws.GetDir(), mark,
		"the idle sweep hibernating the session, which is what compacts it", hibernateSucceededMsg)
	hostCtx, cancel := context.WithTimeout(w.Ctx(), HibernationChainTimeout)
	defer cancel()
	harness.AwaitView(t, hostCtx, host,
		"the host reporting the parked workspace with its shim detached before revival", shimDetached)
	revived := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")

	// Assert: the revived turn ran under the session's own mode. `plan` is
	// named because it is the exact contamination -- the summarizer's mode,
	// and the value this read answered before the compaction stated the
	// session's own mode on the last record it appends.
	//
	// THE SESSION'S OWN MODE IS `auto` (owner ruling 2026-09-14), which is what
	// the shim now starts under when nothing states a mode. The guarantee is
	// unchanged: the revived turn runs under THE SESSION'S posture, whatever it
	// is, and never the summarizer's.
	ended := AwaitTurnEnded(t, w, ws, revived).GetTurnEnded()
	concluded := ended.GetConcluded()
	if concluded == nil {
		t.Fatalf("the revived turn's outcome = %v, want a Concluded terminal", ended)
	}
	markdown := tlResponseMarkdown(t, tlOpenRows(t, w, ws), concluded.GetAnswer())
	if !strings.Contains(markdown, "[mode=auto]") {
		t.Fatalf("the revived turn concluded %q, want it to run under [mode=auto]: a revival must "+
			"restore the session's own posture, and [mode=plan] would be the compaction summarizer's "+
			"throwaway query leaking through the transcript", markdown)
	}
}
