package ssm

import (
	"context"
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/workspace/merge"
)

// fakeQueue is a merge.Queue whose Snapshot is whatever the test arranged.
type fakeQueue struct {
	snapshot  map[string][]merge.Request
	subscribe func(repo string) (<-chan merge.Request, func())
}

func (f *fakeQueue) Publish(context.Context, string, merge.Request) (merge.Position, error) {
	return merge.Position{}, errors.New("fakeQueue: Publish is not part of any test's arrangement")
}

func (f *fakeQueue) Subscribe(repo string) (<-chan merge.Request, func()) {
	if f.subscribe != nil {
		return f.subscribe(repo)
	}
	ch := make(chan merge.Request)
	return ch, func() { close(ch) }
}

func (f *fakeQueue) Snapshot() map[string][]merge.Request { return f.snapshot }

// fakeSessionAuthority records the workspaces whose turn a lease stopped and
// whose session a merged transition stood down, and can be armed to refuse
// either.
type fakeSessionAuthority struct {
	stopped     []string
	err         error
	tornDown    []string
	teardownErr error
	// teardownHook runs inside TeardownMerged, so a test can observe what the
	// rest of the daemon looks like at the instant the teardown is entered.
	teardownHook func(workspace string)
}

func (f *fakeSessionAuthority) InterruptForMerge(_ context.Context, workspace string) error {
	f.stopped = append(f.stopped, workspace)
	return f.err
}

func (f *fakeSessionAuthority) TeardownMerged(workspace string) error {
	f.tornDown = append(f.tornDown, workspace)
	if f.teardownHook != nil {
		f.teardownHook(workspace)
	}
	return f.teardownErr
}

// openLeaseTest arranges a wired manager plus a lease over it.
func openLeaseTest(t *testing.T, ws string) (*Manager, *MergeLease, *fakeQueue, *fakeSessionAuthority, *capLog) {
	t.Helper()
	m, cl, _ := openTest(t, fakeResolver{"s1": ws})
	q := &fakeQueue{}
	in := &fakeSessionAuthority{}
	l, err := NewMergeLease(MergeLeaseConfig{Manager: m, Queue: q, Interrupter: in})
	if err != nil {
		t.Fatalf("NewMergeLease: %v", err)
	}
	return m, l, q, in, cl
}

func TestNewMergeLeaseRequiresEveryDependency(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	tests := []struct {
		name string
		cfg  MergeLeaseConfig
		want string
	}{
		{
			name: "nil manager",
			cfg:  MergeLeaseConfig{Queue: &fakeQueue{}, Interrupter: &fakeSessionAuthority{}},
			want: "Manager is required",
		},
		{
			name: "nil queue",
			cfg:  MergeLeaseConfig{Manager: m, Interrupter: &fakeSessionAuthority{}},
			want: "Queue is required",
		},
		{
			name: "nil interrupter",
			cfg:  MergeLeaseConfig{Manager: m, Queue: &fakeQueue{}},
			want: "Interrupter is required",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			l, err := NewMergeLease(tt.cfg)

			// Assert.
			if err == nil {
				t.Fatalf("NewMergeLease(%s) = %v, want an error", tt.name, l)
			}
			if l != nil {
				t.Fatalf("NewMergeLease(%s) returned a lease alongside its error", tt.name)
			}
			if !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("NewMergeLease(%s) error = %q, want it to mention %q", tt.name, err, tt.want)
			}
		})
	}
}

func TestNewMergeLeaseRefusesASecondQueue(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := openLeaseTest(t, "ws1")

	// Act.
	_, err := NewMergeLease(MergeLeaseConfig{Manager: m, Queue: &fakeQueue{}, Interrupter: &fakeSessionAuthority{}})

	// Assert.
	if err == nil {
		t.Fatal("NewMergeLease with a second queue = nil, want an error")
	}
	if !strings.Contains(err.Error(), "already bound") {
		t.Fatalf("NewMergeLease error = %q, want it to name the already-bound queue", err)
	}
}

func TestMergeLeaseAcquireHoldsAndInterrupts(t *testing.T) {
	// Arrange.
	m, l, _, in, cl := openLeaseTest(t, "ws1")

	// Act.
	release, err := l.Acquire(context.Background(), "ws1")

	// Assert.
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	if !l.Held("ws1") {
		t.Fatal("Held(ws1) = false after a successful Acquire")
	}
	if !m.MergeLeaseHeld("ws1") {
		t.Fatal("MergeLeaseHeld(ws1) = false after a successful Acquire")
	}
	if len(in.stopped) != 1 || in.stopped[0] != "ws1" {
		t.Fatalf("interrupted = %v, want exactly [ws1]", in.stopped)
	}
	if !cl.contains("merge lease decision=acquire workspace=ws1") {
		t.Fatal("the acquisition was not recorded through the canonical logger")
	}
	release()
}

func TestMergeLeaseReleaseHandsTheShimBack(t *testing.T) {
	// Arrange.
	_, l, _, _, cl := openLeaseTest(t, "ws1")
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}

	// Act.
	release()

	// Assert.
	if l.Held("ws1") {
		t.Fatal("Held(ws1) = true after release")
	}
	if !cl.contains("merge lease decision=release workspace=ws1") {
		t.Fatal("the release was not recorded through the canonical logger")
	}
}

func TestMergeLeaseReleaseIsIdempotent(t *testing.T) {
	// Arrange.
	_, l, _, _, cl := openLeaseTest(t, "ws1")
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}

	// Act.
	release()
	release()

	// Assert. A second call must not report a failure about a window the first
	// one legitimately closed.
	if cl.contains("merge lease RELEASE FAILED") {
		t.Fatal("a repeated release reported a failure; the release func is not once-only")
	}
	if n := cl.count("merge lease decision=release workspace=ws1"); n != 1 {
		t.Fatalf("release logged %d times, want exactly 1", n)
	}
}

func TestMergeLeaseAcquireRefusesASecondHolder(t *testing.T) {
	// Arrange.
	_, l, _, in, cl := openLeaseTest(t, "ws1")
	if _, err := l.Acquire(context.Background(), "ws1"); err != nil {
		t.Fatalf("first Acquire: %v", err)
	}

	// Act.
	release, err := l.Acquire(context.Background(), "ws1")

	// Assert.
	if err == nil {
		t.Fatal("a second Acquire on a held workspace = nil, want an error")
	}
	if release != nil {
		t.Fatal("the refused Acquire returned a release func")
	}
	if len(in.stopped) != 1 {
		t.Fatalf("interrupted = %v, want the losing Acquire to have stopped nobody's turn", in.stopped)
	}
	if !cl.contains("merge lease decision=reject workspace=ws1") {
		t.Fatal("the refusal was not recorded through the canonical logger")
	}
}

func TestMergeLeaseAcquireRejectsAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	_, l, _, in, cl := openLeaseTest(t, "ws1")

	// Act.
	release, err := l.Acquire(context.Background(), "")

	// Assert.
	if err == nil {
		t.Fatal("Acquire(\"\") = nil, want an error")
	}
	if release != nil {
		t.Fatal("Acquire(\"\") returned a release func")
	}
	if len(in.stopped) != 0 {
		t.Fatalf("interrupted = %v, want nothing stopped for an unnamed workspace", in.stopped)
	}
	if !cl.contains("merge lease decision=reject_validation") {
		t.Fatal("the validation refusal was not recorded through the canonical logger")
	}
}

func TestMergeLeaseAcquireRollsBackAnUndeliverableInterrupt(t *testing.T) {
	// Arrange.
	m, l, _, in, cl := openLeaseTest(t, "ws1")
	in.err = errors.New("shim is gone")

	// Act.
	release, err := l.Acquire(context.Background(), "ws1")

	// Assert.
	if err == nil {
		t.Fatal("Acquire with an undeliverable interrupt = nil, want an error")
	}
	if release != nil {
		t.Fatal("the failed Acquire returned a release func")
	}
	if m.MergeLeaseHeld("ws1") {
		t.Fatal("the claim survived an undeliverable interrupt; the rollback did not run")
	}
	if !cl.contains("merge lease decision=rollback workspace=ws1") {
		t.Fatal("the rollback was not recorded through the canonical logger")
	}
}

func TestMergeLeaseHeldReachesTheWorkspaceState(t *testing.T) {
	// Arrange.
	m, l, _, _, _ := openLeaseTest(t, "ws1")
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	// Act.
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1"); !got.GetMergeLeaseHeld() {
		t.Fatal("WorkspaceState.merge_lease_held = false while the lease is held")
	}
	release()
	if got := mustCurrent(t, m, "ws1"); got.GetMergeLeaseHeld() {
		t.Fatal("WorkspaceState.merge_lease_held = true after the lease was released")
	}
}

// A connectivity-edge push used to be hand-built beside the funnel, which
// silently dropped the merge facts stampMergeFactsLocked owns. Every outgoing
// WorkspaceState now goes through workspaceMessageLocked, so the edge-driven
// push must carry the held lease like any other frame.
func TestConnectivityEdgePushCarriesTheHeldLease(t *testing.T) {
	// Arrange — a held lease, then subscribe so only edge-driven pushes land.
	m, l, _, _, _ := openLeaseTest(t, "ws1")
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("Apply: %v", err)
	}
	if _, err := l.Acquire(context.Background(), "ws1"); err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	states, cancel := m.Subscribe()
	defer cancel()

	// Act — a connectivity edge, whose push runs publishCompositeLocked.
	if err := m.ApplySessionConnectivity("ws1", "s1", "g1", SessionConnectivityConnecting, "test_edge"); err != nil {
		t.Fatalf("ApplySessionConnectivity: %v", err)
	}

	// Assert — the edge-driven frame carries the lease fact.
	select {
	case msg := <-states:
		if !msg.GetMergeLeaseHeld() {
			t.Fatal("connectivity-edge push dropped merge_lease_held; the hand-built bypass is back")
		}
	default:
		t.Fatal("the connectivity edge pushed no WorkspaceState")
	}
}

func TestMergeLeaseSurvivesAReopen(t *testing.T) {
	// Arrange.
	m, cl, path := openTest(t, fakeResolver{"s1": "ws1"})
	l, err := NewMergeLease(MergeLeaseConfig{Manager: m, Queue: &fakeQueue{}, Interrupter: &fakeSessionAuthority{}})
	if err != nil {
		t.Fatalf("NewMergeLease: %v", err)
	}
	if _, err := l.Acquire(context.Background(), "ws1"); err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	if err := m.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act.
	reopened, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: fakeResolver{"s1": "ws1"}})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { reopened.Close() })

	// Assert.
	if !reopened.MergeLeaseHeld("ws1") {
		t.Fatal("the held lease did not survive the reopen; the workspace was silently re-opened to user prompts")
	}
	if !cl.contains("merge lease ledger restored with 1 OPEN window(s)") {
		t.Fatal("the restored open window was not recorded through the canonical logger")
	}
}

// A daemon bounce mid-merge leaves the workspace's lease window open with no
// holder alive to release it. The next Acquire — Drain resuming the durable
// queue head, or the user re-issuing the merge — must ADOPT that window rather
// than wedge on the unique open-window index, which is exactly how every merge
// of a bounced workspace failed forever with "shim lease unavailable".
func TestMergeLeaseAcquireAdoptsAnOrphanedWindow(t *testing.T) {
	// Arrange — a held lease whose daemon dies (Close without release).
	m, cl, path := openTest(t, fakeResolver{"s1": "ws1"})
	l, err := NewMergeLease(MergeLeaseConfig{Manager: m, Queue: &fakeQueue{}, Interrupter: &fakeSessionAuthority{}})
	if err != nil {
		t.Fatalf("NewMergeLease: %v", err)
	}
	if _, err := l.Acquire(context.Background(), "ws1"); err != nil {
		t.Fatalf("first Acquire: %v", err)
	}
	if err := m.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	reopened, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: fakeResolver{"s1": "ws1"}})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { reopened.Close() })
	in := &fakeSessionAuthority{}
	rl, err := NewMergeLease(MergeLeaseConfig{Manager: reopened, Queue: &fakeQueue{}, Interrupter: in})
	if err != nil {
		t.Fatalf("NewMergeLease over the reopened manager: %v", err)
	}

	// Act — the resumed merge acquires over the orphaned window.
	release, err := rl.Acquire(context.Background(), "ws1")

	// Assert — adopted, not wedged, and still exactly one window in the ledger.
	if err != nil {
		t.Fatalf("Acquire over the orphaned window = %v, want adoption", err)
	}
	if release == nil {
		t.Fatal("the adopting Acquire returned no release func")
	}
	if !cl.contains("merge lease decision=adopt workspace=ws1") {
		t.Fatal("the adoption was not recorded through the canonical logger")
	}
	var windows int
	if err := reopened.db.QueryRow(
		`SELECT COUNT(*) FROM merge_lease WHERE workspace='ws1'`).Scan(&windows); err != nil {
		t.Fatalf("count windows: %v", err)
	}
	if windows != 1 {
		t.Fatalf("ledger holds %d windows after adoption, want the original one alone", windows)
	}
	if len(in.stopped) != 1 {
		t.Fatalf("interrupted = %v, want the adopting merge to stop the workspace's turn once", in.stopped)
	}
}

// The adopted window must close like any other on release, handing the shim
// back to the user with the ORIGINAL acquiring edge preserved for provenance.
func TestMergeLeaseReleaseClosesAnAdoptedWindow(t *testing.T) {
	// Arrange — an orphaned window adopted by a post-reopen Acquire.
	m, cl, path := openTest(t, fakeResolver{"s1": "ws1"})
	l, err := NewMergeLease(MergeLeaseConfig{Manager: m, Queue: &fakeQueue{}, Interrupter: &fakeSessionAuthority{}})
	if err != nil {
		t.Fatalf("NewMergeLease: %v", err)
	}
	if _, err := l.Acquire(context.Background(), "ws1"); err != nil {
		t.Fatalf("first Acquire: %v", err)
	}
	if err := m.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	reopened, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: fakeResolver{"s1": "ws1"}})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { reopened.Close() })
	rl, err := NewMergeLease(MergeLeaseConfig{Manager: reopened, Queue: &fakeQueue{}, Interrupter: &fakeSessionAuthority{}})
	if err != nil {
		t.Fatalf("NewMergeLease over the reopened manager: %v", err)
	}
	release, err := rl.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("adopting Acquire: %v", err)
	}

	// Act.
	release()

	// Assert — the one window is closed and the workspace is open to prompts.
	if reopened.MergeLeaseHeld("ws1") {
		t.Fatal("the lease is still held after releasing the adopted window")
	}
	var open int
	if err := reopened.db.QueryRow(
		`SELECT COUNT(*) FROM merge_lease WHERE workspace='ws1' AND released_at IS NULL`).Scan(&open); err != nil {
		t.Fatalf("count open windows: %v", err)
	}
	if open != 0 {
		t.Fatalf("ledger holds %d open windows after release, want none", open)
	}
}

// A live in-process holder must still exclude a second acquire: adoption is
// for windows whose holder died with a prior daemon, never a bypass of the
// double-merge protection.
func TestMergeLeaseAcquireStillRefusesALiveHolderAfterAdoption(t *testing.T) {
	// Arrange — an adopted window whose holder is alive in this process.
	m, cl, path := openTest(t, fakeResolver{"s1": "ws1"})
	l, err := NewMergeLease(MergeLeaseConfig{Manager: m, Queue: &fakeQueue{}, Interrupter: &fakeSessionAuthority{}})
	if err != nil {
		t.Fatalf("NewMergeLease: %v", err)
	}
	if _, err := l.Acquire(context.Background(), "ws1"); err != nil {
		t.Fatalf("first Acquire: %v", err)
	}
	if err := m.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	reopened, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: fakeResolver{"s1": "ws1"}})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { reopened.Close() })
	rl, err := NewMergeLease(MergeLeaseConfig{Manager: reopened, Queue: &fakeQueue{}, Interrupter: &fakeSessionAuthority{}})
	if err != nil {
		t.Fatalf("NewMergeLease over the reopened manager: %v", err)
	}
	if _, err := rl.Acquire(context.Background(), "ws1"); err != nil {
		t.Fatalf("adopting Acquire: %v", err)
	}

	// Act.
	release, err := rl.Acquire(context.Background(), "ws1")

	// Assert.
	if err == nil {
		t.Fatal("a second Acquire over a live adopted holder = nil, want an error")
	}
	if release != nil {
		t.Fatal("the refused Acquire returned a release func")
	}
	if !cl.contains("already held by a live merge in this process") {
		t.Fatal("the live-holder refusal was not recorded through the canonical logger")
	}
}

func TestConversationSourceAt(t *testing.T) {
	// Arrange.
	m, l, _, _, _ := openLeaseTest(t, "ws1")
	before := m.clock()
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	m.mu.Lock()
	held := m.mergeLeases["ws1"][0].acquiredAt
	m.mu.Unlock()
	release()
	m.mu.Lock()
	released := m.mergeLeases["ws1"][0].releasedAt
	m.mu.Unlock()

	tests := []struct {
		name string
		ws   string
		ts   int64
		want frontendv1.ConversationSource
	}{
		{"inside the window", "ws1", held, frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE},
		{"before the window", "ws1", before - 1, frontendv1.ConversationSource_CONVERSATION_SOURCE_USER},
		{"on the releasing edge", "ws1", released, frontendv1.ConversationSource_CONVERSATION_SOURCE_USER},
		{"after the window", "ws1", released + 1000, frontendv1.ConversationSource_CONVERSATION_SOURCE_USER},
		{"a workspace with no ledger at all", "ws2", held, frontendv1.ConversationSource_CONVERSATION_SOURCE_USER},
		{"an unplaceable item on a workspace that never merged", "ws2", 0, frontendv1.ConversationSource_CONVERSATION_SOURCE_USER},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := m.ConversationSourceAt(tt.ws, tt.ts)

			// Assert.
			if err != nil {
				t.Fatalf("ConversationSourceAt(%s, %d): %v", tt.ws, tt.ts, err)
			}
			if got != tt.want {
				t.Fatalf("ConversationSourceAt(%s, %d) = %v, want %v", tt.ws, tt.ts, got, tt.want)
			}
		})
	}
}

func TestConversationSourceAtRejectsUnplaceableItems(t *testing.T) {
	// Arrange. The workspace must have a ledger for the timestamp to matter at
	// all: with no window ever opened the answer is USER at every instant.
	m, l, _, _, cl := openLeaseTest(t, "ws1")
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	release()
	tests := []struct {
		name string
		ws   string
		ts   int64
	}{
		{"empty workspace", "", 1000},
		{"zero timestamp", "ws1", 0},
		{"negative timestamp", "ws1", -1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := m.ConversationSourceAt(tt.ws, tt.ts)

			// Assert.
			if err == nil {
				t.Fatalf("ConversationSourceAt(%q, %d) = %v, want an error", tt.ws, tt.ts, got)
			}
			if got != frontendv1.ConversationSource_CONVERSATION_SOURCE_UNSPECIFIED {
				t.Fatalf("ConversationSourceAt(%q, %d) = %v alongside its error, want UNSPECIFIED", tt.ws, tt.ts, got)
			}
			if !cl.contains("conversation source decision=reject_validation") {
				t.Fatal("the refusal was not recorded through the canonical logger")
			}
		})
	}
}

func TestMergeQueueFactsReachTheWorkspaceState(t *testing.T) {
	// Arrange.
	m, _, q, _, _ := openLeaseTest(t, "ws1")
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("Apply: %v", err)
	}
	tests := []struct {
		name         string
		snapshot     map[string][]merge.Request
		wantPosition int32
		wantDepth    int32
	}{
		{
			name:         "not enqueued",
			snapshot:     map[string][]merge.Request{"repo": {{Workspace: "other"}}},
			wantPosition: 0,
			wantDepth:    1,
		},
		{
			name:         "at the head",
			snapshot:     map[string][]merge.Request{"repo": {{Workspace: "ws1"}, {Workspace: "other"}}},
			wantPosition: 1,
			wantDepth:    2,
		},
		{
			name:         "behind a sibling worktree",
			snapshot:     map[string][]merge.Request{"repo": {{Workspace: "other"}, {Workspace: "ws1"}}},
			wantPosition: 2,
			wantDepth:    2,
		},
		{
			name:         "an empty queue",
			snapshot:     map[string][]merge.Request{},
			wantPosition: 0,
			wantDepth:    0,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			q.snapshot = tt.snapshot

			// Act.
			got := mustCurrent(t, m, "ws1")

			// Assert.
			if got.GetMergeQueuePosition() != tt.wantPosition {
				t.Fatalf("merge_queue_position = %d, want %d", got.GetMergeQueuePosition(), tt.wantPosition)
			}
			if tt.wantPosition != 0 && got.GetMergeQueueDepth() != tt.wantDepth {
				t.Fatalf("merge_queue_depth = %d, want %d", got.GetMergeQueueDepth(), tt.wantDepth)
			}
		})
	}
}

func TestMergeQueueFactsAreZeroWithoutAQueue(t *testing.T) {
	// Arrange. A daemon with no merge subsystem at all: nothing can be
	// enqueued on a queue that does not exist.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	// Act.
	got := mustCurrent(t, m, "ws1")

	// Assert.
	if got.GetMergeQueuePosition() != 0 || got.GetMergeQueueDepth() != 0 || got.GetMergeLeaseHeld() {
		t.Fatalf("merge facts = (%d, %d, %t), want all zero without a merge subsystem",
			got.GetMergeQueuePosition(), got.GetMergeQueueDepth(), got.GetMergeLeaseHeld())
	}
}
