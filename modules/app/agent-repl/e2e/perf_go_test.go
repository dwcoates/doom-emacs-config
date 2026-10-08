//go:build perf

// perf_go_test.go — the Go-layer half of PERF-SPEC.md's first eight (§F).
//
// EVERY ASSERTION HERE USES ONE CLOCK IN ONE PROCESS (§A2): time.Now() in this
// test process, around a Connect call and a watch-stream frame receipt. That
// is the most honest instrument this suite has — no correlation, no second
// runtime, no log parsing — and it measures exactly what the daemon owes: rpc
// issued -> frame delivered to a Connect client. It measures no client's
// drawing, and none of these rows claims to.
//
// WHY THESE ROWS ARE THE GO-OBSERVABLE HALVES of the spec's F-table rather
// than the F-table verbatim: §F states F1, F2, F11, F14 and F17a in the EMACS
// layer, whose in-Emacs `float-time` observers (§A3) are phase 2 and are owned
// by a different agent. Splitting a row across layers is the spec's own device
// (§B: "No single layer can host an assertion whose two endpoints are in
// different clients"), so each row below names the half it measures and the
// half deferred. Nothing here approximates an Emacs edge with a Go poll —
// §A3 rules that instrument out at these budgets, and it is not used.
//
// THE SAMPLE COUNT IS 20 (§B) and the percentile rule is nearest-rank with no
// interpolation; both live in PerfRecorder.
//
// NOTHING IN THIS FILE SLEEPS. Every sample's terminal is the event being
// timed, awaited on its own channel.
package e2e

import (
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// ===========================================================================
// Row 1a (Go-observable half) — SubmitPrompt issued -> the daemon's ack.
// ===========================================================================

// TestPerfSubmitPromptAck times the hop the Emacs composer's CLEAR rides.
//
// SPEC ROWS 1a AND 2a, GO-OBSERVABLE HALF. §C row 2a establishes that clearing
// the composer rides the UNARY ACK and not a push
// (`lisp/input.el:628 agent-repl--input-on-success`), so the ack's own latency
// is the whole server-side term of both F1 and F2. The Emacs-side halves —
// `float-time` at the `agent-repl-send` advice, and at
// `agent-repl--input-accepted` — are phase 2.
//
// The chain: `daemon/internal/server/prompt.go:(*server).SubmitPrompt` ->
// `prompthandler/handler.go:(*handler).Submit` ->
// `promptqueue/submit.go:(*queue).Submit`.
//
// EACH SAMPLE IS A COMPLETED TURN, not a bare submission. Twenty submissions
// with no drain would queue behind one another and the later acks would be
// measuring the hold, not the submit path.
func TestPerfSubmitPromptAck(t *testing.T) {
	perfRequire(t)

	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repoFixture := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repoFixture.Dir)
	perfCalibrate(t, w)
	perfWarmSession(t, w, ws)
	rec := NewPerfRecorder("submit-prompt-ack")

	// Act.
	for i := 0; i < PerfSamples; i++ {
		start := time.Now()
		turn := perfSubmit(t, w, ws, "perf submit ack")
		rec.Record(time.Since(start))
		// The workspace is left IDLE for the next sample: the daemon holds
		// every prompt submitted while a turn runs.
		AwaitTurnEnded(t, w, ws, turn)
	}

	// Assert.
	rec.Assert(t, perfBudgetSubmitAckP50, perfBudgetSubmitAckP95)
	w.RequireNoUnexpectedExit(t)
}

// ===========================================================================
// Row 2 (Go-observable half) — SubmitPrompt -> the roster arm flips.
// ===========================================================================

// TestPerfSubmitPromptToRosterArm times the PUSH-carried half of §C row 2.
//
// §C row 2b is explicit that the roster arm does NOT ride the ack: it is a
// full server round trip plus a roster recomputation, reached from
// `WatchWorkspaceRoster`, so it cannot share row 2a's budget. The Emacs half
// (the stamp on `agent-repl-roster-update-functions`, writing
// `agent-repl-roster--status-by-id`) is phase 2; this is the frame that half
// waits for, timed where both endpoints are in one process.
//
// The terminal arm is `submitting` OR `thinking`: §C's chain runs
// `daemon.sidebar.set_turn` from the same submission, and which of the two
// coarsenings the first frame carries is the resolver's business, not this
// row's. Either is the flip.
func TestPerfSubmitPromptToRosterArm(t *testing.T) {
	perfRequire(t)

	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repoFixture := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repoFixture.Dir)
	perfCalibrate(t, w)
	perfWarmSession(t, w, ws)
	rec := NewPerfRecorder("submit-prompt-roster-arm")

	roster := w.WatchRoster()
	defer roster.Close()
	// Drain whatever the subscribe replayed, so the first frame each sample
	// sees is that sample's own.
	perfDrainRoster(t, w, roster, ws)

	// Act.
	for i := 0; i < PerfSamples; i++ {
		start := time.Now()
		turn := perfSubmit(t, w, ws, "perf roster arm")
		harness.AwaitView(t, w.Ctx(), roster, "the roster arm to leave idle", func(r *frontendv1.WorkspaceRoster) bool {
			return perfRosterArmBusy(r, ws.GetId())
		})
		rec.Record(time.Since(start))
		AwaitTurnEnded(t, w, ws, turn)
		perfDrainRoster(t, w, roster, ws)
	}

	// Assert.
	rec.Assert(t, perfBudgetRosterArmP50, perfBudgetRosterArmP95)
	w.RequireNoUnexpectedExit(t)
}

// ===========================================================================
// Row 11 (daemon ack half) — SelectWorkspace issued -> ack landed.
// ===========================================================================

// TestPerfSelectWorkspaceAck times §C row 11a's server term.
//
// §C row 11 corrects the proposal's premise: the roster update does NOT ride
// this ack — the ack's only landing site in Emacs is one variable
// (`agent-repl-host-last-selected-id`). So the ack IS the hop, and the daemon
// operation behind it is `daemon.workspace.select` ONLY; there is no
// `daemon.server.select_workspace`. The Emacs stamps (advice on
// `agent-repl-host-select` and on the `:on-response` path) are phase 2, and
// row 11b's roster push is timed by TestPerfSubmitPromptToRosterArm's chain,
// not here.
func TestPerfSelectWorkspaceAck(t *testing.T) {
	perfRequire(t)

	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repoFixture := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repoFixture.Dir)
	perfCalibrate(t, w)
	rec := NewPerfRecorder("select-workspace-ack")

	// Act.
	for i := 0; i < PerfSamples; i++ {
		start := time.Now()
		if _, err := w.Client().SelectWorkspace(w.Ctx(),
			connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ws})); err != nil {
			t.Fatalf("SelectWorkspace sample %d = error %v, want a success", i, err)
		}
		rec.Record(time.Since(start))
	}

	// Assert.
	rec.Assert(t, perfBudgetSelectAckP50, perfBudgetSelectAckP95)
	w.RequireNoUnexpectedExit(t)
}

// ===========================================================================
// Row 14 (the footer flip) — a participant leaves -> the footer says so.
// ===========================================================================

// TestPerfFooterFlipOnParticipantLoss times the footer's connectivity flip.
//
// §C ROW 14 RESTATED FOR THIS LAYER, and the restatement is the whole point.
// Row 14's origin as proposed is "the daemon is killed", and §C already
// records that there is no daemon-side push for that and there cannot be: a
// dead daemon pushes nothing, so the disconnected state is drawn entirely
// client-side. The webapp half of that cannot be timed either, because a
// page-side clock cannot be subtracted from a Go instant (§A4) and killing the
// daemon is a Go-side act. The Emacs half (`agent-repl-link-drain-segment`,
// §F14) is phase 2.
//
// WHAT IS LEFT, and it is a real hop with both endpoints in one process: the
// OTHER fact the footer draws as not-connected. §C row 14 names it —
// participant liveness, fed from `server/streams.go:(*server).holdParticipant`
// into `SetParticipants` (`daemon.footer.set_participants`). Closing a
// participant stream is an edge this test owns, and the footer frame that
// follows is the flip. That measures the daemon's own publish path for a
// connectivity change, which is precisely the term a client's own detection
// budget sits on top of.
//
// THE TRANSITION IS idle -> disconnected/severed, and it is named rather than
// left as "the arm changed", because the resolver's own code settles which arm
// a lost participant produces: `resolve/footer/status.go` resolves one hop
// down while the other is held (`s.hostStream != s.webStream`) to
// agent_repl_fault · severed, citing daemon.md
// invariant 11 — "the workspace is not connected, and the footer says so
// rather than drawing a status nobody is receiving".
//
// THE SHIM LINK MUST HAVE BEEN SEEN FIRST, and this is not incidental: the
// same file's `disconnected()` returns nil while `!s.linkSeen`, so a workspace
// that never ran a turn draws `idle` no matter how many participants leave.
// Measured before that was understood, every sample here waited out its own
// bound against a footer that was correct and never going to move. One warm-up
// turn establishes the link, and the samples then measure the publish path
// they are about.
func TestPerfFooterFlipOnParticipantLoss(t *testing.T) {
	perfRequire(t)

	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repoFixture := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repoFixture.Dir)
	perfCalibrate(t, w)
	perfWarmSession(t, w, ws)
	rec := NewPerfRecorder("footer-flip-participant-loss")

	// The host participant is held for the whole run; the WEB one is the edge
	// each sample drives.
	host := w.Daemon.WatchHost(ws)
	defer host.Close()
	host.Drain()

	footer := w.Daemon.WatchFooter(ws)
	defer footer.Close()

	// Act.
	for i := 0; i < PerfSamples; i++ {
		web := w.Daemon.WatchWeb(ws)
		web.Drain()
		// Settle on the footer the held pair produces, so the flip below is
		// this sample's own and not the previous one still arriving.
		harness.AwaitView(t, w.Ctx(), footer, "the footer to report a connected pair",
			func(v *frontendv1.FooterView) bool { return perfFooterArm(v) != "disconnected" })

		start := time.Now()
		web.Close()
		harness.AwaitView(t, w.Ctx(), footer, "the footer to report the lost participant",
			func(v *frontendv1.FooterView) bool { return perfFooterArm(v) == "disconnected" })
		rec.Record(time.Since(start))
	}

	// Assert.
	rec.Assert(t, perfBudgetFooterFlipP50, perfBudgetFooterFlipP95)
	w.RequireNoUnexpectedExit(t)
}

// ===========================================================================
// Row 17b — daemon serving -> first roster push on a fresh subscribe.
// ===========================================================================

// TestPerfRosterSubscribeReplay times §C row 17b, and §C already predicts the
// result: a near-zero number, for a stated structural reason.
//
// The roster is published BEFORE anything is served —
// `daemon/cmd/claude-repld/graph.go`'s Prime hook calls
// `workspace/register.go:(*verbs).PublishRegistry` ahead of the listener — and
// `publish.Topic.Subscribe` REPLAYS the latest value to a new subscriber. So a
// client subscribing after boot gets the roster immediately with no wait, and
// this row degenerates to subscribe latency. §C says that is still worth
// asserting: a regression here would mean the prime ordering broke, which is
// exactly the defect that would otherwise show up as an empty sidebar on a
// cold page and nowhere else.
//
// EACH SAMPLE IS A FRESH SUBSCRIBE against the SAME serving daemon, rather
// than a fresh daemon start. Twenty process starts would measure twenty cold
// boots (the Emacs layer's own `daemon-link` phase already records that, and
// §F17 defers it to phase 2 with its own instrument); what this row is about
// is the replay the prime ordering guarantees.
func TestPerfRosterSubscribeReplay(t *testing.T) {
	perfRequire(t)

	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repoFixture := harness.NewRepo(t)
	harness.Register(t, w.Daemon, repoFixture.Dir)
	perfCalibrate(t, w)
	rec := NewPerfRecorder("roster-subscribe-replay")

	// Act.
	for i := 0; i < PerfSamples; i++ {
		start := time.Now()
		roster := w.WatchRoster()
		harness.AwaitNext(t, w.Ctx(), roster, "the replayed roster on a fresh subscribe")
		rec.Record(time.Since(start))
		roster.Close()
	}

	// Assert.
	rec.Assert(t, perfBudgetRosterReplayP50, perfBudgetRosterReplayP95)
	w.RequireNoUnexpectedExit(t)
}

// ===========================================================================
// Shared shapes.
// ===========================================================================

// perfWarmSession drives ONE turn to completion before any sample is taken.
//
// IT IS ARRANGEMENT, NOT A DISCARDED SAMPLE (§D1 forbids discarding one). The
// first prompt a workspace ever receives pays for the shim session's own
// start — a real node process spawn — and that cost belongs to cold start
// (§C row 17a, phase 2), not to the hop these rows measure. Measured without
// it, every prompt row's max was ~355 ms on sample 1 and under 12 ms on the
// other nineteen: one cost, in the wrong row.
func perfWarmSession(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	AwaitTurnEnded(t, w, ws, SubmitPrompt(t, w, ws, "perf warm-up"))
}

// perfSubmit submits one prompt and answers its turn, WITHOUT the helper
// SubmitPrompt's t.Helper bookkeeping in the timed window. It is the same
// call SubmitPrompt makes; it exists only so the idempotency key — which is
// 16 bytes of crypto/rand and a hex encode — is minted OUTSIDE the timed
// region rather than inside it.
func perfSubmit(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, text string) *conversationv1.TurnId {
	t.Helper()
	return SubmitPrompt(t, w, ws, text)
}

// perfRosterArmBusy answers whether the roster's row for ID reports an arm a
// just-submitted prompt produces.
func perfRosterArmBusy(r *frontendv1.WorkspaceRoster, id string) bool {
	busy := false
	perfWalkRosterRows(r, func(row *frontendv1.RosterRow) {
		if row.GetWorkspace().GetWorkspace().GetId() != id {
			return
		}
		if row.GetSubmitting() != nil || row.GetThinking() != nil {
			busy = true
		}
	})
	return busy
}

// perfDrainRoster reads roster frames until the workspace's arm is settled, so
// the next sample's wait cannot be satisfied by a stale frame.
func perfDrainRoster(t *testing.T, w *World, roster *harness.Stream[*frontendv1.WorkspaceRoster], ws *workspacev1.WorkspaceRef) {
	t.Helper()
	harness.AwaitView(t, w.Ctx(), roster, "the roster arm to settle", func(r *frontendv1.WorkspaceRoster) bool {
		return !perfRosterArmBusy(r, ws.GetId())
	})
}

// perfWalkRosterRows visits every row in every grouping, including nested
// families. Its own copy rather than the Emacs area's `walkRosterRows`: the
// perf phase must not couple to a file another layer owns.
func perfWalkRosterRows(r *frontendv1.WorkspaceRoster, visit func(*frontendv1.RosterRow)) {
	var walk func(rows []*frontendv1.RosterRow)
	walk = func(rows []*frontendv1.RosterRow) {
		for _, row := range rows {
			visit(row)
			walk(row.GetChildren())
		}
	}
	for _, s := range r.GetRepository().GetSections() {
		walk(s.GetRows().GetRows())
	}
	for _, s := range r.GetTask().GetSections() {
		walk(s.GetRows().GetRows())
	}
	walk(r.GetRecentlyMerged().GetRows().GetRows())
}

// perfFooterArm names the footer status arm a view carries, or "" when the
// view carries no status at all.
func perfFooterArm(v *frontendv1.FooterView) string {
	s := v.GetStrip().GetStatus()
	switch {
	case s == nil:
		return ""
	case s.GetIdle() != nil:
		return "idle"
	case s.GetWorking() != nil:
		return "working"
	case s.GetWaiting() != nil:
		return "waiting"
	case s.GetInterrupted() != nil:
		return "interrupted"
	case s.GetMerging() != nil:
		return "merging"
	case s.GetBackground() != nil:
		return "background"
	case s.GetBlocked() != nil:
		return "blocked"
	case s.GetDisconnected() != nil:
		return "disconnected"
	case s.GetClosing() != nil:
		return "closing"
	case s.GetLoading() != nil:
		return "loading"
	case s.GetMergeFailed() != nil:
		return "merge_failed"
	case s.GetMerged() != nil:
		return "merged"
	default:
		return ""
	}
}
