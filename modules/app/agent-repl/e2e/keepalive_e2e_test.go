// Package e2e — the keep-alive rewind's SPAN INVARIANT, end to end.
//
// Contract: agent-shim/claude/shim/src/engine/keepalive.ts's "THE SPAN
// INVARIANT" (owner requirement, 2026-10-06). The shim's keep-alive rewinds
// the vendor context to an anchor (the last record of real conversation: an
// assistant or a user record of a real or genuine vendor-started turn), and
// before any rewind discards anything it asserts that every turn between the
// anchor and now is keep-alive material: the keep-alive's own turn, or a
// vendor turn the keep-alive's own rewind set off. Anything else REFUSES the
// rewind at ERROR and keeps the content.
//
// ORDINARY USE NEVER REFUSES, and that is what this file proves: a completed
// task's turn and an interrupted turn's own record both ADVANCE the anchor,
// so every scenario here rewinds exactly the keep-alive. The refusal itself
// (a send the vendor left no record of, or a record no turn owns) has no
// realistic path through a daemon world; the shim's unit suites drive it.
//
// Everything runs for real except the vendor (the shim's `--fake` engine) and
// git (the scripted fake); the keep-alive cadence is compressed by the shim's
// documented `AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS` lever, exactly as the
// hibernation area's keep-alive tests do.
//
// SYNCHRONIZATION: every wait is on a record the shim itself writes to its
// per-workspace log — a rewind's "REWINDING" record (with the span it
// discarded), the invariant's "REFUSED" record, a keep-alive's "closed a
// turn" — or on the turn's own terminal in the feed. Successive records are
// awaited strictly after one another (awaitShimRecords), so a count is never
// satisfied by a record written before the event it is meant to follow.
//
// THE KEEP-ALIVE BEFORE THE FIRST PROMPT is not driven here: a daemon world
// starts the shim's session with the first prompt, so no beat can precede it
// deterministically. The shim's unit and integration suites cover it.
//
// The shim's records are not part of the daemon harness's warning sweep (which
// reads the daemon's own pid), so each test reads the shim's log for a
// refusal itself.
package e2e

import (
	"database/sql"
	"fmt"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"

	_ "modernc.org/sqlite"
)

const (
	// The invariant's refusal, as engine/keepalive.ts records it.
	refusedRewindPrefix = "the keep-alive rewind is REFUSED"
	// Every rewind's own record, before a keep-alive beat or a real prompt.
	rewindingPrefix = "REWINDING the vendor context"
	// The fake vendor's ship-gns replay, run on each truncating resume once
	// `!stop-on-rewind` armed the session.
	// The CLI's own summary of the replayed stop, on both planes.
	stopReplaySummary = "didn't finish before the previous session ended"
	stopReplayMessage = "fake vendor replays a task STOPPED by the rewind and answers it in a turn of its OWN before the next send"
)

// keepaliveWorld is a world whose shims beat their keep-alive fast enough to
// observe, with one registered workspace.
func keepaliveWorld(t *testing.T) (*World, *workspacev1.WorkspaceRef, string) {
	t.Helper()
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		ExtraEnv: []string{fmt.Sprintf("%s=%d", fakeKeepaliveIntervalEnv, fakeKeepaliveIntervalMS)},
	}})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	return w, ws, harness.WorkspaceLogPath(repo.Dir, "shim")
}

// awaitShimRecords awaits COUNT records satisfying pred, each strictly after
// the one before it in the log's own order.
func awaitShimRecords(t *testing.T, w *World, path, what string, count int, pred func(harness.LogRecord) bool) []harness.LogRecord {
	t.Helper()
	var got []harness.LogRecord
	after := func(harness.LogRecord) bool { return true }
	previous := ""
	for len(got) < count {
		// The anchor is the previous match itself, which pred also accepts,
		// so it is excluded by its own line.
		excluded := previous
		r := w.Daemon.AwaitLogRecordAfter(path, fmt.Sprintf("%s (#%d of %d)", what, len(got)+1, count), after,
			func(c harness.LogRecord) bool { return c.Raw != excluded && pred(c) })
		got = append(got, r)
		previous = r.Raw
		raw := r.Raw
		after = func(c harness.LogRecord) bool { return c.Raw == raw }
	}
	return got
}

// isRewind reports a rewind record performed before `before` ("keepalive" or
// "real_prompt").
func isRewind(before string) func(harness.LogRecord) bool {
	return func(r harness.LogRecord) bool {
		return strings.HasPrefix(r.Message, rewindingPrefix) && r.Context["before"] == before
	}
}

func isRefusal(r harness.LogRecord) bool { return strings.HasPrefix(r.Message, refusedRewindPrefix) }

// spanTurns reads a span list (`discarded_span` or `offending`) off a record.
func spanTurns(t *testing.T, r harness.LogRecord, field string) []map[string]any {
	t.Helper()
	raw, ok := r.Context[field].([]any)
	if !ok {
		t.Fatalf("shim record %q carries no %s list: %v", r.Message, field, r.Context)
	}
	out := make([]map[string]any, 0, len(raw))
	for _, item := range raw {
		turn, ok := item.(map[string]any)
		if !ok {
			t.Fatalf("shim record %q: %s entry %v is not an object", r.Message, field, item)
		}
		out = append(out, turn)
	}
	return out
}

// spanKinds answers the kinds of a record's span list, in order.
func spanKinds(t *testing.T, r harness.LogRecord, field string) []string {
	t.Helper()
	var kinds []string
	for _, turn := range spanTurns(t, r, field) {
		kinds = append(kinds, fmt.Sprint(turn["kind"]))
	}
	return kinds
}

// shimRecords reads the shim's per-workspace log as it stands.
func shimRecords(t *testing.T, path string) []harness.LogRecord {
	t.Helper()
	return harness.ReadLog(t, path)
}

// TestKeepAliveRewindsOnlyKeepAliveMaterial — several beats in a row, then a
// real prompt: every rewind discards exactly one keep-alive turn, every one
// resumes at the same real anchor, and the prompt's own rewind is no
// different.
func TestKeepAliveRewindsOnlyKeepAliveMaterial(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, shimLog := keepaliveWorld(t)
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")
	beats := awaitShimRecords(t, w, shimLog, "a rewind before a keep-alive beat", 3, isRewind("keepalive"))

	// Act
	after := SubmitPrompt(t, w, ws, "after several keep-alives")
	AwaitTurnEnded(t, w, ws, after)
	prompt := w.Daemon.AwaitLogRecord(shimLog, "the real prompt's rewind", isRewind("real_prompt"))

	// Assert
	for i, rewind := range append(beats, prompt) {
		if got := spanKinds(t, rewind, "discarded_span"); fmt.Sprint(got) != "[keepalive]" {
			t.Errorf("rewind #%d discards span kinds %v, want exactly one keep-alive turn", i+1, got)
		}
		if got := fmt.Sprint(rewind.Context["anchor_turn_id"]); got != turn.GetValue() {
			t.Errorf("rewind #%d anchors on turn %q, want the real turn %q", i+1, got, turn.GetValue())
		}
	}
	for _, r := range shimRecords(t, shimLog) {
		if isRefusal(r) {
			t.Errorf("the shim refused a rewind %v, want every rewind's span to be keep-alive material", r.Context)
		}
	}
}

// TestPromptDuringKeepAliveRewindsPastTheKeepAliveAlone — a real prompt that
// arrives while a keep-alive is in flight waits it out, then rewinds past
// exactly that keep-alive to the last real turn.
func TestPromptDuringKeepAliveRewindsPastTheKeepAliveAlone(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, shimLog := keepaliveWorld(t)
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")
	awaitShimRecords(t, w, shimLog, "a keep-alive submitted after the real turn", 1,
		func(r harness.LogRecord) bool { return r.Context["outcome"] == "keepalive_submitted" })

	// Act
	during := SubmitPrompt(t, w, ws, "during a keep-alive")
	AwaitTurnEnded(t, w, ws, during)
	rewind := w.Daemon.AwaitLogRecord(shimLog, "the real prompt's rewind", isRewind("real_prompt"))

	// Assert
	if got := spanKinds(t, rewind, "discarded_span"); fmt.Sprint(got) != "[keepalive]" {
		t.Errorf("the real prompt's rewind discards span kinds %v, want exactly the keep-alive it waited out", got)
	}
	if got := fmt.Sprint(rewind.Context["anchor_turn_id"]); got != turn.GetValue() {
		t.Errorf("the real prompt's rewind anchors on turn %q, want the real turn %q", got, turn.GetValue())
	}
}

// TestCompletedTaskBesideKeepAliveAnchorsTheRewind — a background task that
// completes while a keep-alive is outstanding starts a genuine vendor turn. It
// is real conversation (ruled 2026-10-06): it is served, it anchors the next
// rewind, which keeps it and discards only the keep-alive, and nothing is
// refused.
func TestCompletedTaskBesideKeepAliveAnchorsTheRewind(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, shimLog := keepaliveWorld(t)
	queued := SubmitPrompt(t, w, ws, "!queue-vendor-turn")
	AwaitTurnEnded(t, w, ws, queued)

	// Act
	rewind := w.Daemon.AwaitLogRecord(shimLog, "the first rewind before a beat", isRewind("keepalive"))
	after := SubmitPrompt(t, w, ws, "after the completed task")
	AwaitTurnEnded(t, w, ws, after)

	// Assert
	if got := fmt.Sprint(rewind.Context["anchor_turn_id"]); got == queued.GetValue() || !strings.HasPrefix(got, "adopted-") {
		t.Errorf("the rewind anchors on turn %q, want the completed task's adopted turn", got)
	}
	if got := spanKinds(t, rewind, "discarded_span"); fmt.Sprint(got) != "[keepalive]" {
		t.Errorf("the rewind discards span kinds %v, want exactly the keep-alive", got)
	}
	for _, r := range shimRecords(t, shimLog) {
		if isRefusal(r) {
			t.Errorf("the shim refused a rewind %v, want the completed task's turn kept by anchoring on it", r.Context)
		}
	}
	var vendorTurnDrawn bool
	for _, row := range feedRows(t, w, ws) {
		if strings.Contains(row.String(), "A background task finished.") {
			vendorTurnDrawn = true
		}
	}
	if !vendorTurnDrawn {
		t.Errorf("the feed drew no row for the completed task's turn, want it served")
	}
}

// TestInterruptedTurnAnchorsTheRewindOnItsOwnRecord — interrupting a turn
// mid-tool is ordinary use: the turn's tool result lands after its last
// assistant record. That result is the turn's own real record, so it advances
// the anchor (ruled 2026-10-06), the next rewind keeps it, and nothing is
// refused.
func TestInterruptedTurnAnchorsTheRewindOnItsOwnRecord(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, shimLog := keepaliveWorld(t)
	turn := SubmitPrompt(t, w, ws, "!interrupt")
	w.Daemon.AwaitLogRecord(shimLog, "the interrupt scenario's tool call to be live",
		func(r harness.LogRecord) bool { return r.Message == "fake mid-tool interrupt turn" })
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
	AwaitTurnEnded(t, w, ws, turn)

	// Act
	rewind := w.Daemon.AwaitLogRecord(shimLog, "the first rewind before a beat", isRewind("keepalive"))

	// Assert
	if got := fmt.Sprint(rewind.Context["anchor_turn_id"]); got != turn.GetValue() {
		t.Errorf("the rewind anchors on turn %q, want the interrupted turn %q", got, turn.GetValue())
	}
	if got := spanKinds(t, rewind, "discarded_span"); fmt.Sprint(got) != "[keepalive]" {
		t.Errorf("the rewind discards span kinds %v, want exactly the keep-alive", got)
	}
	for _, r := range shimRecords(t, shimLog) {
		if isRefusal(r) {
			t.Errorf("the shim refused a rewind %v, want the interrupted turn's record anchored", r.Context)
		}
	}
}

// TestStopReplayedByEveryRewindNeverLoops — the ship-gns loop (2026-10-02):
// each rewind replaced the vendor query, the old query's task was reported
// stopped, and the vendor answered that stop in a turn of its own. Driven by
// the mock's `!stop-on-rewind` lever, every rewind must resume at the same
// real anchor, discard the stop's answer with the keep-alive, trip no
// refusal, and store and serve nothing of it.
func TestStopReplayedByEveryRewindNeverLoops(t *testing.T) {
	t.Parallel()
	cases := []struct {
		name   string
		assert func(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, shimLog, armed string, rewinds []harness.LogRecord)
	}{
		{
			name: "every rewind resumes at the one real anchor",
			assert: func(t *testing.T, _ *World, _ *workspacev1.WorkspaceRef, _, armed string, rewinds []harness.LogRecord) {
				for i, r := range rewinds {
					if got := fmt.Sprint(r.Context["anchor_turn_id"]); got != armed {
						t.Errorf("rewind #%d anchors on turn %q, want the arming real turn %q", i+1, got, armed)
					}
				}
			},
		},
		{
			// THE REAL SHAPE ANSWERS THE STOP WITH NO REPLY (a lone result), and
			// its notification is a transcript-only record the stream never
			// carries: the span the shim sees past the anchor is the keep-alive.
			name: "every rewind discards exactly the keep-alive",
			assert: func(t *testing.T, _ *World, _ *workspacev1.WorkspaceRef, _, _ string, rewinds []harness.LogRecord) {
				for i, r := range rewinds {
					if got := spanKinds(t, r, "discarded_span"); fmt.Sprint(got) != "[keepalive]" {
						t.Errorf("rewind #%d discards span kinds %v, want exactly the keep-alive", i+1, got)
					}
				}
			},
		},
		{
			name: "no rewind is refused",
			assert: func(t *testing.T, _ *World, _ *workspacev1.WorkspaceRef, shimLog, _ string, _ []harness.LogRecord) {
				for _, r := range shimRecords(t, shimLog) {
					if isRefusal(r) {
						t.Errorf("the shim refused a rewind %v, want the stop's answer judged keep-alive material", r.Context)
					}
				}
			},
		},
		{
			name: "nothing of the replayed stop is served",
			assert: func(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, _, _ string, _ []harness.LogRecord) {
				for _, row := range feedRows(t, w, ws) {
					if strings.Contains(row.String(), stopReplaySummary) {
						t.Errorf("feed row %v draws the replayed stop, want it never served", row)
					}
				}
			},
		},
		{
			name: "nothing of the replayed stop is stored",
			assert: func(t *testing.T, w *World, _ *workspacev1.WorkspaceRef, _, _ string, _ []harness.LogRecord) {
				db, err := sql.Open("sqlite", "file:"+w.Store.DBPath+"?mode=ro")
				if err != nil {
					t.Fatalf("e2e: opening the store database read-only: %v", err)
				}
				defer db.Close()
				rows, err := db.QueryContext(w.Ctx(),
					`SELECT kind, COUNT(*) FROM entry WHERE instr(frame, CAST(? AS BLOB)) > 0 GROUP BY kind`, stopReplaySummary)
				if err != nil {
					t.Fatalf("e2e: counting rows carrying the replayed stop: %v", err)
				}
				defer rows.Close()
				for rows.Next() {
					var kind string
					var count int
					if err := rows.Scan(&kind, &count); err != nil {
						t.Fatalf("e2e: reading a row count: %v", err)
					}
					t.Errorf("the store holds %d %q row(s) carrying the replayed stop, want none", count, kind)
				}
				if err := rows.Err(); err != nil {
					t.Fatalf("e2e: iterating row counts: %v", err)
				}
			},
		},
		{
			name: "no turn is opened for the stop's answer",
			assert: func(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, _, armed string, _ []harness.LogRecord) {
				turns := distinctTurnIDs(feedRows(t, w, ws))
				if len(turns) != 2 || !turns[armed] {
					t.Errorf("feed turns = %v, want exactly the arming turn %q and the prompt after the replays", turns, armed)
				}
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			w, ws, shimLog := keepaliveWorld(t)
			armed := SubmitPrompt(t, w, ws, "!stop-on-rewind")
			AwaitTurnEnded(t, w, ws, armed)

			// Act: three replays, each answered beside a keep-alive, then a
			// real prompt whose turn ending proves the ordered writer flushed
			// everything before it.
			awaitShimRecords(t, w, shimLog, "a stop replayed by a rewind", 3,
				func(r harness.LogRecord) bool { return r.Message == stopReplayMessage })
			rewinds := awaitShimRecords(t, w, shimLog, "a rewind before a keep-alive beat", 3, isRewind("keepalive"))
			after := SubmitPrompt(t, w, ws, "after the replays")
			AwaitTurnEnded(t, w, ws, after)

			// Assert
			tc.assert(t, w, ws, shimLog, armed.GetValue(), rewinds)
		})
	}
}

// TestRestartedShimCarriesItsKeepAlivesUntilARealReply — a restarted shim is
// a new process with no anchor: its keep-alives are carried (never rewound,
// never refused) until its own first real reply anchors the next rewind.
func TestRestartedShimCarriesItsKeepAlivesUntilARealReply(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws, shimLog := keepaliveWorld(t)
	// Restarting stands the shim down and brings it back: the same
	// declarations the hibernation round trip makes for the same stand-down.
	w.ExpectWarnings("daemon.health.open_fault",
		"daemon.sessionwatcher.link_fault", "daemon.shimclient.redial",
		"daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.shimclient.exit")
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")
	firstPID := w.Daemon.AwaitLogRecord(shimLog, "the first shim's own record",
		func(r harness.LogRecord) bool { return r.Message == "closed a turn" }).PID
	resp, err := w.Client().RestartWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("RestartWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace = %v, want success", resp.Msg)
	}
	restarted := func(r harness.LogRecord) bool { return r.PID != firstPID }
	carried := w.Daemon.AwaitLogRecord(shimLog, "the restarted shim carrying keep-alives with no anchor",
		func(r harness.LogRecord) bool {
			return restarted(r) && strings.HasPrefix(r.Message, "no rewind anchor exists")
		})

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")
	rewind := w.Daemon.AwaitLogRecord(shimLog, "the restarted shim's first rewind",
		func(r harness.LogRecord) bool { return restarted(r) && isRewind("keepalive")(r) })

	// Assert
	for _, r := range shimRecords(t, shimLog) {
		if r.Raw == carried.Raw {
			break
		}
		if restarted(r) && (strings.HasPrefix(r.Message, rewindingPrefix) || isRefusal(r)) {
			t.Errorf("the restarted shim rewound or refused before any anchor: %q %v", r.Message, r.Context)
		}
	}
	if got := fmt.Sprint(rewind.Context["anchor_turn_id"]); got != turn.GetValue() {
		t.Errorf("the restarted shim's first rewind anchors on turn %q, want its own first real turn %q", got, turn.GetValue())
	}
}
