package merge

import (
	"context"
	"os"
	"path/filepath"
	"testing"
)

// --- fixtures -----------------------------------------------------------

const testRepoKey = "/repos/alpha/.git"

func newTestQueue(t *testing.T) (*FileQueue, string) {
	t.Helper()
	dir := t.TempDir()
	q, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	return q, dir
}

func testRequest(name string) Request {
	return Request{
		Workspace:    "/ws/" + name,
		Name:         name,
		SourceBranch: "feature/" + name,
		SourceDir:    "/src/" + name,
		TargetDir:    "/target",
	}
}

// --- construction -------------------------------------------------------

func TestNewFileQueueRequiresDependencies(t *testing.T) {
	tests := []struct {
		name    string
		dir     string
		logf    func(string, ...any)
		wantErr bool
	}{
		{name: "complete", dir: t.TempDir(), logf: func(string, ...any) {}, wantErr: false},
		{name: "no dir", dir: "", logf: func(string, ...any) {}, wantErr: true},
		{name: "no logger", dir: t.TempDir(), logf: nil, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			q, err := NewFileQueue(tc.dir, tc.logf)

			// Assert.
			if tc.wantErr {
				if err == nil {
					t.Fatalf("NewFileQueue() error = nil, want error")
				}
				return
			}
			if err != nil || q == nil {
				t.Fatalf("NewFileQueue() = %v, %v, want a queue", q, err)
			}
		})
	}
}

// --- publish ------------------------------------------------------------

func TestPublishRejectsBadInput(t *testing.T) {
	tests := []struct {
		name string
		repo string
		req  Request
	}{
		{name: "empty repo key", repo: "", req: testRequest("a")},
		{name: "invalid request", repo: testRepoKey, req: Request{Name: "a"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			q, dir := newTestQueue(t)

			// Act.
			pos, err := q.Publish(context.Background(), tc.repo, tc.req)

			// Assert — refused with no durable record written.
			if err == nil {
				t.Fatalf("Publish() error = nil, want error")
			}
			if pos != (Position{}) {
				t.Fatalf("Publish() position = %+v, want zero", pos)
			}
			if entries, _ := os.ReadDir(dir); len(entries) != 0 {
				t.Fatalf("queue dir has %d entries, want none", len(entries))
			}
		})
	}
}

func TestPublishReturnsIncreasingPositions(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)

	// Act.
	first, err := q.Publish(context.Background(), testRepoKey, testRequest("a"))
	if err != nil {
		t.Fatalf("Publish(a): %v", err)
	}
	second, err := q.Publish(context.Background(), testRepoKey, testRequest("b"))
	if err != nil {
		t.Fatalf("Publish(b): %v", err)
	}

	// Assert — 1-based index, depth counts the head.
	if first != (Position{Index: 1, Depth: 1, Repo: testRepoKey}) {
		t.Fatalf("first = %+v, want index 1 depth 1", first)
	}
	if second != (Position{Index: 2, Depth: 2, Repo: testRepoKey}) {
		t.Fatalf("second = %+v, want index 2 depth 2", second)
	}
}

func TestPublishKeepsRepositoriesOnSeparateQueues(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)

	// Act.
	if _, err := q.Publish(context.Background(), testRepoKey, testRequest("a")); err != nil {
		t.Fatalf("Publish(a): %v", err)
	}
	other, err := q.Publish(context.Background(), "/repos/beta/.git", testRequest("b"))
	if err != nil {
		t.Fatalf("Publish(b): %v", err)
	}

	// Assert — a second repository starts its own queue at index 1.
	if other.Index != 1 || other.Depth != 1 {
		t.Fatalf("other = %+v, want index 1 depth 1", other)
	}
}

// --- durability ---------------------------------------------------------

func TestPublishedEntriesSurviveANewQueueOverTheSameDir(t *testing.T) {
	// Arrange — publish through one queue, then model a daemon bounce.
	q, dir := newTestQueue(t)
	if _, err := q.Publish(context.Background(), testRepoKey, testRequest("a")); err != nil {
		t.Fatalf("Publish(a): %v", err)
	}
	if _, err := q.Publish(context.Background(), testRepoKey, testRequest("b")); err != nil {
		t.Fatalf("Publish(b): %v", err)
	}

	// Act — the next daemon reads the same directory.
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	snap := next.Snapshot()

	// Assert — both entries, in publish order.
	got := snap[testRepoKey]
	if len(got) != 2 {
		t.Fatalf("snapshot = %+v, want 2 entries for %s", snap, testRepoKey)
	}
	if got[0].Name != "a" || got[1].Name != "b" {
		t.Fatalf("snapshot order = %q, %q, want a, b", got[0].Name, got[1].Name)
	}
}

// THE RUN'S NAME IS DURABLE. A bounce mid-queue must interrupt the run the user
// is watching rather than replace it, and the id is the only part of a run that
// can be written down at all.
func TestAPublishedEntryCarriesItsRunIDAcrossABounce(t *testing.T) {
	// Arrange — a request whose run publisher is alive at publish time.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	run, err := NewRunStatus(&recordingSink{}, t.Logf, req.Workspace, testClock())
	if err != nil {
		t.Fatalf("NewRunStatus: %v", err)
	}
	req.Run = run
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Act — the next daemon reads the same directory.
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}

	// Assert.
	got := next.Snapshot()[testRepoKey]
	if len(got) != 1 {
		t.Fatalf("snapshot = %+v, want 1 entry", got)
	}
	if got[0].RunID != run.RunID() {
		t.Fatalf("replayed RunID = %q, want the admission's %q", got[0].RunID, run.RunID())
	}
}

// A replayed entry carries NO publisher: a *RunStatus is bound to the process's
// sink and clock, and one restored from disk would publish into a dead one.
func TestAReplayedEntryCarriesNoRunPublisher(t *testing.T) {
	// Arrange.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	run, err := NewRunStatus(&recordingSink{}, t.Logf, req.Workspace, testClock())
	if err != nil {
		t.Fatalf("NewRunStatus: %v", err)
	}
	req.Run = run
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Act.
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}

	// Assert.
	if got := next.Snapshot()[testRepoKey][0].Run; got != nil {
		t.Fatalf("replayed Run = %+v, want nil", got)
	}
}

// --- the status watermark -----------------------------------------------

// publishedRun publishes req under a live run and returns the run's id, which
// is the key every watermark call is made against.
func publishedRun(t *testing.T, q *FileQueue, req Request) (Request, string) {
	t.Helper()
	run, err := NewRunStatus(&recordingSink{}, t.Logf, req.Workspace, testClock())
	if err != nil {
		t.Fatalf("NewRunStatus: %v", err)
	}
	req.Run = run
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	return req, run.RunID()
}

// THE GUARANTEE: the watermark survives the bounce, so the next boot's resume
// has a floor to seed above.
func TestARecordedWatermarkSurvivesABounce(t *testing.T) {
	// Arrange.
	q, dir := newTestQueue(t)
	_, runID := publishedRun(t, q, testRequest("a"))

	// Act.
	if err := q.RecordStatusWatermark(testRepoKey, runID, 9000); err != nil {
		t.Fatalf("RecordStatusWatermark: %v", err)
	}
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}

	// Assert.
	got := next.Snapshot()[testRepoKey]
	if len(got) != 1 {
		t.Fatalf("snapshot = %+v, want 1 entry", got)
	}
	if got[0].StatusWatermarkMs != 9000 {
		t.Fatalf("replayed StatusWatermarkMs = %d, want 9000", got[0].StatusWatermarkMs)
	}
}

// An entry that never recorded a watermark replays with none, which is the
// "no floor to respect" case a resume serves from now() alone.
func TestAnEntryWithNoRecordedWatermarkReplaysWithZero(t *testing.T) {
	// Arrange.
	q, dir := newTestQueue(t)
	publishedRun(t, q, testRequest("a"))

	// Act.
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}

	// Assert.
	got := next.Snapshot()[testRepoKey]
	if len(got) != 1 {
		t.Fatalf("snapshot = %+v, want 1 entry", got)
	}
	if got[0].StatusWatermarkMs != 0 {
		t.Fatalf("replayed StatusWatermarkMs = %d, want 0", got[0].StatusWatermarkMs)
	}
}

// THE VIOLATION EDGE: a watermark may only rise. Writing a lower one would hand
// the next boot a floor beneath statuses already on the wire.
func TestRecordStatusWatermarkRefusesARegression(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)
	_, runID := publishedRun(t, q, testRequest("a"))
	if err := q.RecordStatusWatermark(testRepoKey, runID, 9000); err != nil {
		t.Fatalf("RecordStatusWatermark(9000): %v", err)
	}

	// Act.
	err := q.RecordStatusWatermark(testRepoKey, runID, 8999)

	// Assert.
	if err == nil {
		t.Fatal("RecordStatusWatermark(8999) error = nil, want the regression refused")
	}
}

// Repeating the same value is a regression too: the watermark is the floor a
// resume adds one to, and two runs' statuses must not be able to share it.
func TestRecordStatusWatermarkRefusesTheSameValueTwice(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)
	_, runID := publishedRun(t, q, testRequest("a"))
	if err := q.RecordStatusWatermark(testRepoKey, runID, 9000); err != nil {
		t.Fatalf("RecordStatusWatermark(9000): %v", err)
	}

	// Act.
	err := q.RecordStatusWatermark(testRepoKey, runID, 9000)

	// Assert.
	if err == nil {
		t.Fatal("RecordStatusWatermark(9000) twice error = nil, want the repeat refused")
	}
}

// A retired entry is a no-op: the terminal status the post-merge hook
// republishes lands after Complete dropped the record, and there is no longer a
// resume for a watermark to protect.
func TestRecordStatusWatermarkOnARetiredEntryIsANoOp(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)
	req, runID := publishedRun(t, q, testRequest("a"))
	if err := q.Complete(testRepoKey, req); err != nil {
		t.Fatalf("Complete: %v", err)
	}

	// Act.
	err := q.RecordStatusWatermark(testRepoKey, runID, 9000)

	// Assert.
	if err != nil {
		t.Fatalf("RecordStatusWatermark on a retired entry = %v, want a no-op", err)
	}
}

// The watermark is keyed on the run, so a call with no run id names no entry and
// is refused rather than raising whichever entry happened to be first.
func TestRecordStatusWatermarkRefusesAnEmptyRunID(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)
	publishedRun(t, q, testRequest("a"))

	// Act.
	err := q.RecordStatusWatermark(testRepoKey, "", 9000)

	// Assert.
	if err == nil {
		t.Fatal("RecordStatusWatermark(\"\") error = nil, want the empty run id refused")
	}
}

// Recording a watermark must not disturb the identity Complete matches the head
// by, or a merge would fail to retire the entry it just finished.
func TestRecordStatusWatermarkLeavesTheEntryCompletable(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)
	req, runID := publishedRun(t, q, testRequest("a"))
	if err := q.RecordStatusWatermark(testRepoKey, runID, 9000); err != nil {
		t.Fatalf("RecordStatusWatermark: %v", err)
	}

	// Act.
	err := q.Complete(testRepoKey, req)

	// Assert.
	if err != nil {
		t.Fatalf("Complete after a watermark write = %v, want the head retired", err)
	}
}

func TestSnapshotOmitsRepositoriesWithNoOutstandingEntries(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if err := q.Complete(testRepoKey, req); err != nil {
		t.Fatalf("Complete: %v", err)
	}

	// Act.
	snap := q.Snapshot()

	// Assert.
	if len(snap) != 0 {
		t.Fatalf("snapshot = %+v, want empty", snap)
	}
}

func TestSnapshotSurfacesAnUnreadableDurableRecord(t *testing.T) {
	// Arrange — a corrupt entry file under a repository directory.
	q, dir := newTestQueue(t)
	repoDir := filepath.Join(dir, "deadbeef")
	if err := os.MkdirAll(repoDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(repoDir, queueFilePrefix+"x.json"), []byte("{not json"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act + Assert — a queue that cannot read its record must never report
	// itself empty, so the loudest available response is the correct one.
	defer func() {
		if recover() == nil {
			t.Fatalf("Snapshot() over a corrupt record did not panic")
		}
	}()
	q.Snapshot()
}

// --- delivery -----------------------------------------------------------

func TestSubscribeDeliversTheHeadAndWaitsForItsCompletion(t *testing.T) {
	// Arrange — two entries on one repository.
	q, _ := newTestQueue(t)
	first, second := testRequest("a"), testRequest("b")
	if _, err := q.Publish(context.Background(), testRepoKey, first); err != nil {
		t.Fatalf("Publish(a): %v", err)
	}
	if _, err := q.Publish(context.Background(), testRepoKey, second); err != nil {
		t.Fatalf("Publish(b): %v", err)
	}
	ch, cancel := q.Subscribe(testRepoKey)
	defer cancel()

	// Act — take the head, then assert nothing else arrives until it completes.
	got := <-ch
	if got != first {
		t.Fatalf("first delivery = %+v, want %+v", got, first)
	}
	select {
	case extra := <-ch:
		t.Fatalf("second entry delivered before the head completed: %+v", extra)
	default:
	}
	if err := q.Complete(testRepoKey, first); err != nil {
		t.Fatalf("Complete(a): %v", err)
	}

	// Assert — the next entry follows the completion.
	if got := <-ch; got != second {
		t.Fatalf("second delivery = %+v, want %+v", got, second)
	}
}

func TestSubscribeDeliversAnEntryPublishedAfterSubscription(t *testing.T) {
	// Arrange — a live subscription on an empty queue.
	q, _ := newTestQueue(t)
	ch, cancel := q.Subscribe(testRepoKey)
	defer cancel()

	// Act.
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Assert.
	if got := <-ch; got != req {
		t.Fatalf("delivery = %+v, want %+v", got, req)
	}
}

func TestSubscribeCancelClosesTheStream(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)
	ch, cancel := q.Subscribe(testRepoKey)

	// Act.
	cancel()

	// Assert — the stream drains to closed.
	if _, open := <-ch; open {
		t.Fatalf("stream still open after cancel")
	}
}

func TestSubscribeTwiceForOneRepositoryPanics(t *testing.T) {
	// Arrange — one repository already owned by a drain.
	q, _ := newTestQueue(t)
	_, cancel := q.Subscribe(testRepoKey)
	defer cancel()

	// Act + Assert — a second owner would put two cherry-picks on one target.
	defer func() {
		if recover() == nil {
			t.Fatalf("second Subscribe did not panic")
		}
	}()
	q.Subscribe(testRepoKey)
}

func TestSubscribeRefusesAnEmptyRepoKey(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)

	// Act + Assert.
	defer func() {
		if recover() == nil {
			t.Fatalf("Subscribe(\"\") did not panic")
		}
	}()
	q.Subscribe("")
}

// --- completion ---------------------------------------------------------

func TestCompleteRemovesTheDurableRecord(t *testing.T) {
	// Arrange.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Act.
	if err := q.Complete(testRepoKey, req); err != nil {
		t.Fatalf("Complete: %v", err)
	}

	// Assert — nothing left on disk for the next boot to replay.
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	if snap := next.Snapshot(); len(snap) != 0 {
		t.Fatalf("snapshot after complete = %+v, want empty", snap)
	}
}

func TestCompleteRejectsAnythingButTheHead(t *testing.T) {
	tests := []struct {
		name    string
		publish []Request
		arg     Request
	}{
		{name: "empty queue", publish: nil, arg: testRequest("a")},
		{name: "not the head", publish: []Request{testRequest("a"), testRequest("b")}, arg: testRequest("b")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			q, _ := newTestQueue(t)
			for _, req := range tc.publish {
				if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
					t.Fatalf("Publish: %v", err)
				}
			}

			// Act.
			err := q.Complete(testRepoKey, tc.arg)

			// Assert — refused, and the queue is left exactly as it was.
			if err == nil {
				t.Fatalf("Complete() error = nil, want error")
			}
			if got := len(q.Snapshot()[testRepoKey]); got != len(tc.publish) {
				t.Fatalf("depth after refused Complete = %d, want %d", got, len(tc.publish))
			}
		})
	}
}

func TestCompleteSurfacesARemoveFailureWithoutAdvancing(t *testing.T) {
	// Arrange — the durable record is gone from under the queue, so its
	// removal fails.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if err := os.RemoveAll(filepath.Join(dir, filepath.Base(q.repoDir(testRepoKey)))); err != nil {
		t.Fatalf("remove repo dir: %v", err)
	}

	// Act.
	err := q.Complete(testRepoKey, req)

	// Assert — loud failure, head retained rather than silently advanced.
	if err == nil {
		t.Fatalf("Complete() error = nil, want error")
	}
	if got := len(q.Snapshot()[testRepoKey]); got != 1 {
		t.Fatalf("depth after failed Complete = %d, want 1", got)
	}
}

// --- the terminal word a run could not publish --------------------------

// A record that could not be re-published faithfully is refused at the MARK,
// not discovered at the boot that has to say it.
func TestMarkTerminalRefusesARecordItCouldNotRepublish(t *testing.T) {
	tests := []struct {
		name    string
		term    TerminalStatus
		wantErr bool
	}{
		{
			name: "merged",
			term: TerminalStatus{Outcome: OutcomeMerged, Cause: "cherry-pick landed on target"},
		},
		{
			name: "merged carrying an after-action failure",
			term: TerminalStatus{Outcome: OutcomeMerged, Cause: "cherry-pick landed on target", AfterActionError: "the turn never ended"},
		},
		{
			name: "failed",
			term: TerminalStatus{Outcome: OutcomeFailed, Cause: "shim lease unavailable"},
		},
		{
			name:    "a parking outcome is not terminal",
			term:    TerminalStatus{Outcome: OutcomeConflict, Cause: "conflicted"},
			wantErr: true,
		},
		{
			name:    "no cause",
			term:    TerminalStatus{Outcome: OutcomeMerged},
			wantErr: true,
		},
		{
			name:    "a failed run carries no after-action error",
			term:    TerminalStatus{Outcome: OutcomeFailed, Cause: "shim lease unavailable", AfterActionError: "the turn never ended"},
			wantErr: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			q, _ := newTestQueue(t)
			req := testRequest("a")
			if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
				t.Fatalf("Publish: %v", err)
			}

			// Act.
			err := q.MarkTerminal(testRepoKey, req, tc.term)

			// Assert.
			if tc.wantErr {
				if err == nil {
					t.Fatalf("MarkTerminal(%+v) error = nil, want error", tc.term)
				}
				return
			}
			if err != nil {
				t.Fatalf("MarkTerminal(%+v) error = %v", tc.term, err)
			}
		})
	}
}

// THE MARK IS NOT AN ACK. The entry stays outstanding, which is what makes the
// next boot find it at all.
func TestMarkTerminalLeavesTheEntryOutstanding(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Act.
	if err := q.MarkTerminal(testRepoKey, req, TerminalStatus{Outcome: OutcomeMerged, Cause: "cherry-pick landed on target"}); err != nil {
		t.Fatalf("MarkTerminal: %v", err)
	}

	// Assert.
	if got := len(q.Snapshot()[testRepoKey]); got != 1 {
		t.Fatalf("depth after MarkTerminal = %d, want the entry still outstanding", got)
	}
}

// The whole point of the record: a daemon that could not say the terminal word
// hands it to the next one.
func TestAMarkedTerminalSurvivesANewQueueOverTheSameDir(t *testing.T) {
	// Arrange — mark through one queue, then model a daemon bounce.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	want := TerminalStatus{
		Outcome:          OutcomeMerged,
		Cause:            "cherry-pick landed on target",
		AfterActionError: "the after-merge action did not complete",
	}
	if err := q.MarkTerminal(testRepoKey, req, want); err != nil {
		t.Fatalf("MarkTerminal: %v", err)
	}
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	replayed := next.Snapshot()[testRepoKey]
	if len(replayed) != 1 {
		t.Fatalf("snapshot = %+v, want 1 entry", replayed)
	}

	// Act.
	got, pending, err := next.PendingTerminal(testRepoKey, replayed[0])

	// Assert.
	if err != nil {
		t.Fatalf("PendingTerminal: %v", err)
	}
	if !pending || got != want {
		t.Fatalf("PendingTerminal() = %+v, %v, want %+v, true", got, pending, want)
	}
}

// The ordinary entry — every entry whose run has not reached a terminal — is
// unchanged by the field existing.
func TestPendingTerminalIsAbsentForAnOrdinaryEntry(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}

	// Act.
	got, pending, err := q.PendingTerminal(testRepoKey, req)

	// Assert.
	if err != nil {
		t.Fatalf("PendingTerminal: %v", err)
	}
	if pending {
		t.Fatalf("PendingTerminal() = %+v, true, want no pending terminal", got)
	}
}

// A retried mark must leave ONE entry carrying ONE word, not two of either.
func TestMarkTerminalTwiceLeavesOneRecord(t *testing.T) {
	// Arrange.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	want := TerminalStatus{Outcome: OutcomeMerged, Cause: "cherry-pick landed on target"}

	// Act — the same terminal word is recorded twice.
	if err := q.MarkTerminal(testRepoKey, req, want); err != nil {
		t.Fatalf("first MarkTerminal: %v", err)
	}
	if err := q.MarkTerminal(testRepoKey, req, want); err != nil {
		t.Fatalf("second MarkTerminal: %v", err)
	}

	// Assert — one durable entry, carrying that one word.
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	replayed := next.Snapshot()[testRepoKey]
	if len(replayed) != 1 {
		t.Fatalf("snapshot = %+v, want exactly 1 entry", replayed)
	}
	got, pending, err := next.PendingTerminal(testRepoKey, replayed[0])
	if err != nil || !pending || got != want {
		t.Fatalf("PendingTerminal() = %+v, %v, %v, want %+v, true, nil", got, pending, err, want)
	}
}

// A mark aimed at anything but the head is a violated single-ownership
// invariant, exactly as it is for Complete.
func TestMarkTerminalRefusesAHeadMismatch(t *testing.T) {
	// Arrange.
	q, _ := newTestQueue(t)
	if _, err := q.Publish(context.Background(), testRepoKey, testRequest("a")); err != nil {
		t.Fatalf("Publish(a): %v", err)
	}
	behind := testRequest("b")
	if _, err := q.Publish(context.Background(), testRepoKey, behind); err != nil {
		t.Fatalf("Publish(b): %v", err)
	}

	// Act.
	err := q.MarkTerminal(testRepoKey, behind, TerminalStatus{Outcome: OutcomeMerged, Cause: "cherry-pick landed on target"})

	// Assert.
	if err == nil {
		t.Fatal("MarkTerminal() on a non-head entry error = nil, want error")
	}
}

// Completing a marked entry is what an eventually-published terminal word does,
// and it must drop the record along with the entry.
func TestCompleteDropsAMarkedEntry(t *testing.T) {
	// Arrange.
	q, dir := newTestQueue(t)
	req := testRequest("a")
	if _, err := q.Publish(context.Background(), testRepoKey, req); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if err := q.MarkTerminal(testRepoKey, req, TerminalStatus{Outcome: OutcomeFailed, Cause: "shim lease unavailable"}); err != nil {
		t.Fatalf("MarkTerminal: %v", err)
	}

	// Act.
	if err := q.Complete(testRepoKey, req); err != nil {
		t.Fatalf("Complete: %v", err)
	}

	// Assert — nothing survives for a later boot to replay.
	next, err := NewFileQueue(dir, t.Logf)
	if err != nil {
		t.Fatalf("NewFileQueue: %v", err)
	}
	if got := next.Snapshot()[testRepoKey]; len(got) != 0 {
		t.Fatalf("snapshot after Complete = %+v, want empty", got)
	}
}
