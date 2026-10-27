package promptqueue

import (
	"context"
	"errors"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// since is the fixed instant every TestHeldSince case measures against.
var since = instant

// TestHeldSince covers heldSince's three filters: a held session act is
// skipped regardless of when it queued, a prompt queued strictly before the
// cut is skipped, and the survivors come back in their original queue order.
func TestHeldSince(t *testing.T) {
	tests := []struct {
		name     string
		standing []wsm.HeldPrompt
		want     []ids.TurnID
	}{
		{
			name: "a session act held prompt is skipped even though queued after since",
			standing: []wsm.HeldPrompt{
				{Turn: "act1", QueuedAt: since.Add(time.Minute), Act: &wsm.HeldAct{Kind: wsm.ActModel, Value: "model-x"}},
			},
			want: nil,
		},
		{
			name: "a prompt queued strictly before since is skipped",
			standing: []wsm.HeldPrompt{
				{Turn: "t0", QueuedAt: since.Add(-time.Minute)},
			},
			want: nil,
		},
		{
			name: "survivors are kept in their original queue order, skips interleaved",
			standing: []wsm.HeldPrompt{
				{Turn: "t0", QueuedAt: since.Add(-time.Minute)},                                                     // before since: skipped
				{Turn: "act1", QueuedAt: since.Add(time.Minute), Act: &wsm.HeldAct{Kind: wsm.ActModel, Value: "m"}}, // session act: skipped
				{Turn: "t1", QueuedAt: since},                                                                       // at since: kept
				{Turn: "t2", QueuedAt: since.Add(2 * time.Minute)},                                                  // after since: kept
			},
			want: []ids.TurnID{"t1", "t2"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: tt.standing is the arrangement.
			// Act.
			got := heldSince(tt.standing, since)
			// Assert.
			if !sameTurns(got, tt.want) {
				t.Fatalf("heldSince = %v, want %v", got, tt.want)
			}
		})
	}
}

// seedHeld parks a standing held prompt directly in the fake DB, queued at
// WHEN, bypassing Submit and classification: RollBack only reads and
// tombstones held prompts, so the harness does not need a running turn or a
// judged verdict behind it.
func seedHeld(t *testing.T, h *harness, turn ids.TurnID, when time.Time) {
	t.Helper()
	if err := h.db.PutHeldPrompt(context.Background(), wsm.HeldPrompt{
		Workspace: theWorkspace,
		Turn:      turn,
		Said:      userSaid("rolled-back prompt"),
		Origin:    "PROMPT_ORIGIN_USER_SENT",
		QueuedAt:  when,
	}); err != nil {
		t.Fatalf("seedHeld(%s): %v", turn, err)
	}
}

func TestRollBackSuccess(t *testing.T) {
	// Arrange: two prompts queued at or after since, a perform that succeeds.
	h := newHarness(t)
	seedHeld(t, h, "t1", since)
	seedHeld(t, h, "t2", since.Add(time.Minute))
	before := h.holds.pushCount()
	calls := 0
	perform := func(context.Context) error {
		calls++
		return nil
	}

	// Act.
	err := h.q.RollBack(context.Background(), theWorkspace, since, []ids.TurnID{"t1", "t2"}, perform)

	// Assert.
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if calls != 1 {
		t.Fatalf("perform was called %d times, want exactly once", calls)
	}
	for _, turn := range []ids.TurnID{"t1", "t2"} {
		tomb := h.db.hold(turn).Tombstone
		if tomb == nil || tomb.Kind != tombstoneRolledBack {
			t.Fatalf("tombstone(%s) = %+v, want a rolled_back tombstone", turn, tomb)
		}
	}
	if h.holds.pushCount() <= before {
		t.Fatal("the tray must be republished after a rollback")
	}
	if !logged(h.log.Records(), "info", opRollBack, "the rollback was performed and the held prompts queued since were dropped") {
		t.Fatalf("records = %+v, want the rollback's success logged at info", h.log.Records())
	}
}

// TestASubmissionDuringARollbackIsDeliveredOnlyOnceItSettles proves no
// prompt is delivered while the conversation is being cut, with the delivery
// lock free for the rewind (call.go): a submission made during PERFORM is
// held, and is delivered once the rollback has settled.
func TestASubmissionDuringARollbackIsDeliveredOnlyOnceItSettles(t *testing.T) {
	// Arrange: one held prompt to roll back, and a perform that submits.
	h := newHarness(t)
	seedHeld(t, h, "t1", since)
	var during Disposition
	var startedDuring []ids.TurnID
	perform := func(context.Context) error {
		var err error
		during, err = h.q.Submit(context.Background(), submission("t-other", "a concurrent submission"))
		if err != nil {
			t.Errorf("Submit during the rewind: %v", err)
		}
		startedDuring = h.sender.started()
		return nil
	}

	// Act
	err := h.q.RollBack(context.Background(), theWorkspace, since, []ids.TurnID{"t1"}, perform)

	// Assert
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if during.Delivered || len(startedDuring) != 0 {
		t.Fatalf("submission during the rewind = %+v with %v started, want it held", during, startedDuring)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t-other" {
		t.Fatalf("started = %v, want the held submission delivered once the rollback settled", started)
	}
}

func TestRollBackHoldsChangedRefusesWithoutPerforming(t *testing.T) {
	// Arrange: t1 is planned for the rollback, but by the time it runs another
	// prompt (t2) has since queued behind it, so the live holds no longer
	// match the plan.
	h := newHarness(t)
	seedHeld(t, h, "t1", since)
	seedHeld(t, h, "t2", since.Add(time.Minute))
	performed := false
	perform := func(context.Context) error {
		performed = true
		return nil
	}

	// Act.
	err := h.q.RollBack(context.Background(), theWorkspace, since, []ids.TurnID{"t1"}, perform)

	// Assert.
	if !errors.Is(err, ErrHoldsChanged) {
		t.Fatalf("RollBack = %v, want ErrHoldsChanged", err)
	}
	if performed {
		t.Fatal("perform must never run when the held prompts changed since the rollback was planned")
	}
	if !logged(h.log.Records(), "info", opRollBack, "the rollback was refused: the held prompts changed since it was planned") {
		t.Fatalf("records = %+v, want the refusal logged at info", h.log.Records())
	}
}

func TestRollBackPerformErrorLeavesHoldsUntouched(t *testing.T) {
	// Arrange: perform (the vendor-side rollback) fails.
	h := newHarness(t)
	seedHeld(t, h, "t1", since)
	causeErr := errors.New("the vendor refused the rollback")
	perform := func(context.Context) error { return causeErr }

	// Act.
	err := h.q.RollBack(context.Background(), theWorkspace, since, []ids.TurnID{"t1"}, perform)

	// Assert: the code returns perform's error directly, unwrapped.
	if !errors.Is(err, causeErr) {
		t.Fatalf("RollBack = %v, want the perform error returned directly", err)
	}
	if tomb := h.db.hold("t1").Tombstone; tomb != nil {
		t.Fatalf("tombstone(t1) = %+v, want the hold untouched when perform fails", tomb)
	}
	if !logged(h.log.Records(), "info", opRollBack, "the rollback was not performed; the held prompts stand") {
		t.Fatalf("records = %+v, want the perform failure logged at info", h.log.Records())
	}
}

func TestRollBackTombstoneFailureIsErrorAndSurfaces(t *testing.T) {
	// Arrange: perform succeeds, but retiring the held prompt's tombstone
	// fails durably.
	h := newHarness(t)
	seedHeld(t, h, "t1", since)
	h.db.tombstoneErr = errors.New("the database is read-only")
	perform := func(context.Context) error { return nil }

	// Act.
	err := h.q.RollBack(context.Background(), theWorkspace, since, []ids.TurnID{"t1"}, perform)

	// Assert: a failed tombstone write is surfaced, and the held prompt that
	// could not be retired stays queued: the retirement is all or nothing.
	if err == nil {
		t.Fatal("a failed tombstone write must be surfaced")
	}
	if tomb := h.db.hold("t1").Tombstone; tomb != nil {
		t.Fatalf("tombstone(t1) = %+v, want the hold still standing after the failed retirement", tomb)
	}
	if !logged(h.log.Records(), "error", opRollBack, "the held prompts the rollback dropped could not be retired; they stay queued") {
		t.Fatalf("records = %+v, want the tombstone failure logged at error", h.log.Records())
	}
}

// TestARollbackDuringATurnStartPerformsOnceTheStartSettles covers a stop
// that lands while its turn's StartTurn stands: the rewind waits out the
// call rather than being made beside it.
func TestARollbackDuringATurnStartPerformsOnceTheStartSettles(t *testing.T) {
	// Arrange
	h := newHarness(t)
	done := make(chan error, 1)
	performedBeside := false
	h.sender.startHook = func() {
		h.sender.startHook = nil
		go func() {
			done <- h.q.RollBack(context.Background(), theWorkspace, since, nil, func(context.Context) error {
				call, _ := h.q.standingCall(theWorkspace)
				performedBeside = call.what != "rollback"
				return nil
			})
		}()
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "go")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	err := <-done

	// Assert
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if performedBeside {
		t.Fatal("the rewind was performed beside the standing start")
	}
}

// putHold parks HELD, standing, in the fake DB for theWorkspace.
func putHold(t *testing.T, h *harness, held wsm.HeldPrompt) {
	t.Helper()
	held.Workspace = theWorkspace
	if held.Said == nil {
		held.Said = userSaid("held " + string(held.Turn))
	}
	if err := h.db.PutHeldPrompt(context.Background(), held); err != nil {
		t.Fatalf("putHold(%s): %v", held.Turn, err)
	}
}

// TestRollBackHeld covers the stop key taking back a held prompt: only while
// a turn runs, only the newest prompt the user sent, only when no agent
// output was drawn since it was queued, and never one a delivery claims.
func TestRollBackHeld(t *testing.T) {
	sender := ids.WorkspaceID("ws-peer")
	user := func(turn ids.TurnID, at time.Time) wsm.HeldPrompt {
		return wsm.HeldPrompt{Turn: turn, Origin: "PROMPT_ORIGIN_USER_SENT", QueuedAt: at}
	}
	tests := []struct {
		name string
		// holds are parked in queue order.
		holds []wsm.HeldPrompt
		// running is the turn in flight, empty for none.
		running ids.TurnID
		// answeredAt is when the feed last drew agent output; zero for never.
		answeredAt time.Time
		// claimed is a hold a standing delivery call claims, empty for none.
		claimed ids.TurnID
		// want is the hold taken back, empty for none.
		want ids.TurnID
		// level and message are the record the decision is logged as.
		level, message string
	}{
		{
			name:    "the newest user prompt nothing answered is taken back",
			holds:   []wsm.HeldPrompt{user("t1", since), user("t2", since.Add(time.Minute))},
			running: "t0", answeredAt: since.Add(30 * time.Second),
			want:  "t2",
			level: "info", message: "nothing answered the held prompt since it was queued, so it was taken back",
		},
		{
			name:    "a webapp user prompt is the user's too",
			holds:   []wsm.HeldPrompt{{Turn: "t1", Origin: "PROMPT_ORIGIN_WEBAPP_USER_SENT", QueuedAt: since}},
			running: "t0",
			want:    "t1",
			level:   "info", message: "nothing answered the held prompt since it was queued, so it was taken back",
		},
		{
			name: "a session act or another sender's prompt behind it is passed over",
			holds: []wsm.HeldPrompt{
				user("t1", since),
				{Turn: "t2", Origin: "PROMPT_ORIGIN_AGENT_SENT", SentBy: &sender, QueuedAt: since.Add(time.Minute)},
				{Turn: "t3", Origin: "PROMPT_ORIGIN_MERGE_QUEUE_NOTICE", QueuedAt: since.Add(2 * time.Minute)},
				{Turn: "t4", Origin: "PROMPT_ORIGIN_USER_SENT", QueuedAt: since.Add(3 * time.Minute), Act: &wsm.HeldAct{Kind: wsm.ActModel, Value: "m"}},
			},
			running: "t0",
			want:    "t1",
			level:   "info", message: "nothing answered the held prompt since it was queued, so it was taken back",
		},
		{
			name:    "agent output drawn after it was queued keeps it held",
			holds:   []wsm.HeldPrompt{user("t1", since)},
			running: "t0", answeredAt: since.Add(time.Second),
			level: "info", message: "agent output was drawn after the held prompt was queued, so it stays held",
		},
		{
			name:    "agent output drawn the instant it was queued keeps it held",
			holds:   []wsm.HeldPrompt{user("t1", since)},
			running: "t0", answeredAt: since,
			level: "info", message: "agent output was drawn after the held prompt was queued, so it stays held",
		},
		{
			name:  "no turn running takes nothing back",
			holds: []wsm.HeldPrompt{user("t1", since)},
			level: "debug", message: "no turn runs, so no held prompt is taken back",
		},
		{
			name:    "no prompt the user sent stands",
			holds:   []wsm.HeldPrompt{{Turn: "t1", Origin: "PROMPT_ORIGIN_MERGE_QUEUE_NOTICE", QueuedAt: since}},
			running: "t0",
			level:   "debug", message: "no prompt the user sent stands held",
		},
		{
			name:    "a hold a delivery call claims is left to the delivery",
			holds:   []wsm.HeldPrompt{user("t1", since)},
			running: "t0", claimed: "t1",
			level: "info", message: "the held prompt is being delivered to the shim, so it is not taken back",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			for _, held := range tt.holds {
				putHold(t, h, held)
			}
			if tt.running != "" {
				running := tt.running
				h.watcher.takeFacts(&running)
			}
			h.feed.answeredAt = tt.answeredAt
			if tt.claimed != "" {
				h.q.state(theWorkspace).call = &shimCall{what: "start_turn", turn: tt.claimed, holds: []ids.TurnID{tt.claimed}}
			}
			before := h.holds.pushCount()

			// Act.
			got, ok, err := h.q.RollBackHeld(context.Background(), theWorkspace)

			// Assert.
			if err != nil {
				t.Fatalf("RollBackHeld: %v", err)
			}
			if ok != (tt.want != "") || got.Turn != tt.want {
				t.Fatalf("RollBackHeld = (%q, %v), want (%q, %v)", got.Turn, ok, tt.want, tt.want != "")
			}
			for _, held := range tt.holds {
				tomb := h.db.hold(held.Turn).Tombstone
				switch {
				case held.Turn == tt.want && (tomb == nil || tomb.Kind != tombstoneRolledBack):
					t.Fatalf("tombstone(%s) = %+v, want a rolled_back tombstone", held.Turn, tomb)
				case held.Turn != tt.want && tomb != nil:
					t.Fatalf("tombstone(%s) = %+v, want it still standing", held.Turn, tomb)
				}
			}
			if pushed := h.holds.pushCount() > before; pushed != (tt.want != "") {
				t.Fatalf("tray republished = %v, want %v", pushed, tt.want != "")
			}
			if tt.want != "" && !proto.Equal(got.Said, userSaid("held "+string(tt.want))) {
				t.Fatalf("taken-back words = %v, want the held prompt's", got.Said)
			}
			if !logged(h.log.Records(), tt.level, opRollBackHeld, tt.message) {
				t.Fatalf("records = %+v, want %s %q", h.log.Records(), tt.level, tt.message)
			}
		})
	}
}

// TestRollBackHeldFailures covers the two store failures: each is answered
// as an error, logged, and leaves the hold standing.
func TestRollBackHeldFailures(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{
			name:    "the holds cannot be read",
			arrange: func(h *harness) { h.db.heldErr = errors.New("disk gone") },
			message: "the held prompts could not be read; none was taken back",
		},
		{
			name:    "the hold cannot be retired",
			arrange: func(h *harness) { h.db.tombstoneErr = errors.New("disk gone") },
			message: "the drop was refused: the hold could not be retired",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			putHold(t, h, wsm.HeldPrompt{Turn: "t1", Origin: "PROMPT_ORIGIN_USER_SENT", QueuedAt: since})
			running := ids.TurnID("t0")
			h.watcher.takeFacts(&running)
			tt.arrange(h)

			// Act.
			_, ok, err := h.q.RollBackHeld(context.Background(), theWorkspace)

			// Assert.
			if err == nil || ok {
				t.Fatalf("RollBackHeld = (ok %v, err %v), want a failure", ok, err)
			}
			h.db.heldErr = nil
			if tomb := h.db.hold("t1").Tombstone; tomb != nil {
				t.Fatalf("tombstone = %+v, want the hold still standing", tomb)
			}
			if !logged(h.log.Records(), "error", opRollBackHeld, tt.message) {
				t.Fatalf("records = %+v, want the failure logged at error", h.log.Records())
			}
		})
	}
}
