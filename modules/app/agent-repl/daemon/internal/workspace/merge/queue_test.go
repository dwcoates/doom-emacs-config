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
