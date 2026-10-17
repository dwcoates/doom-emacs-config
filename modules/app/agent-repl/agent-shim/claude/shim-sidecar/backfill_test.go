package main

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

// backfill_test.go — a backfill request reads a named conversation no shim
// holds, once, through the drain path (backfill.go).

// requestBackfill drops a request naming sessions, exactly as a writer does:
// written under .tmp, then renamed.
func (h *harness) requestBackfill(t *testing.T, name string, body string) string {
	t.Helper()
	dir := filepath.Join(h.state, backfillDirName)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", dir, err)
	}
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path+".tmp", []byte(body), 0o644); err != nil {
		t.Fatalf("write %s.tmp: %v", path, err)
	}
	if err := os.Rename(path+".tmp", path); err != nil {
		t.Fatalf("rename %s.tmp: %v", path, err)
	}
	return path
}

// closedTranscriptInCycle writes a closed workspace's transcript and begins a
// cycle, so the transcript is dormant.
func closedTranscriptInCycle(t *testing.T, h *harness, session string) string {
	t.Helper()
	h.closedWorkspace(t, session)
	path := h.inactiveTranscript(t, session, promptLine, assistantLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	return path
}

func TestABackfilledClosedWorkspacesTranscriptIsReadToItsEnd(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := closedTranscriptInCycle(t, h, "sess-idle")
	h.requestBackfill(t, "req", "sess-idle\n")

	// Act.
	h.tick()

	// Assert.
	w, watched := h.sc.watchers[path]
	if !watched {
		t.Fatal("the backfilled transcript is not watched")
	}
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	if got := w.tailer.Offset(); got != info.Size() {
		t.Fatalf("read to offset %d, want the file's end %d", got, info.Size())
	}
}

func TestABackfilledTranscriptIsDroppedOnceReadAndSilent(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := closedTranscriptInCycle(t, h, "sess-idle")
	h.requestBackfill(t, "req", "sess-idle\n")
	h.tick()

	// Act.
	h.advance(h.sc.tracker.Windows().AgentSilence + time.Minute)
	h.tick()

	// Assert.
	if _, watched := h.sc.watchers[path]; watched {
		t.Fatal("the backfilled transcript is still watched after it was read and went silent")
	}
	if _, dormant := h.sc.dormant[path]; !dormant {
		t.Fatal("the backfilled transcript was not returned to dormant")
	}
}

func TestABackfillRequestIsConsumed(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	closedTranscriptInCycle(t, h, "sess-idle")
	request := h.requestBackfill(t, "req", "sess-idle\n")

	// Act.
	h.tick()

	// Assert.
	if _, err := os.Stat(request); !os.IsNotExist(err) {
		t.Fatalf("the applied request is still there (stat err %v)", err)
	}
}

func TestABackfillRequestIsStatedOnceAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	closedTranscriptInCycle(t, h, "sess-idle")
	h.requestBackfill(t, "req", "sess-idle\n")

	// Act.
	h.tick()
	h.tick()

	// Assert.
	h.requireOnce(t, "backfill-request", "info")
}

func TestABackfillOfAnActiveConversationLeavesItActive(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.activate(t, "sess-live")
	h.transcript(t, "sess-live", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.requestBackfill(t, "req", "sess-live\n")

	// Act.
	h.tick()

	// Assert.
	if _, draining := h.sc.draining["sess-live"]; draining {
		t.Fatal("a backfill moved a live conversation to draining")
	}
}

func TestAnUnusableBackfillRequestIsSetAside(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	closedTranscriptInCycle(t, h, "sess-idle")
	request := h.requestBackfill(t, "req", "sess-idle sess-other\n")

	// Act.
	h.tick()

	// Assert.
	if _, err := os.Stat(request + ".rejected"); err != nil {
		t.Fatalf("the unusable request was not set aside: %v", err)
	}
}

func TestAnUnusableBackfillRequestIsStatedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	closedTranscriptInCycle(t, h, "sess-idle")
	h.requestBackfill(t, "req", "\n\n")

	// Act.
	h.tick()
	h.tick()

	// Assert.
	h.requireOnce(t, "backfill-request", "error")
}

func TestAnUnusableBackfillRequestBackfillsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := closedTranscriptInCycle(t, h, "sess-idle")
	h.requestBackfill(t, "req", "sess-idle sess-other\n")

	// Act.
	h.tick()

	// Assert.
	if _, watched := h.sc.watchers[path]; watched {
		t.Fatal("a rejected request's conversation was backfilled")
	}
}

func TestABackfillRequestStillBeingWrittenIsNotRead(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := closedTranscriptInCycle(t, h, "sess-idle")
	dir := filepath.Join(h.state, backfillDirName)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, "req.tmp"), []byte("sess-idle\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	h.tick()

	// Assert.
	if _, watched := h.sc.watchers[path]; watched {
		t.Fatal("a .tmp request was applied before it was renamed")
	}
}

func TestParseBackfillRequest(t *testing.T) {
	tests := []struct {
		name    string
		body    string
		want    []string
		wantErr bool
	}{
		{name: "one id per line", body: "a\nb\n", want: []string{"a", "b"}},
		{name: "blank lines and padding are allowed", body: "\n  a  \n\nb", want: []string{"a", "b"}},
		{name: "two ids on one line are refused", body: "a b\n", wantErr: true},
		{name: "a request naming nothing is refused", body: "\n \n", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table row.

			// Act.
			got, err := parseBackfillRequest([]byte(tt.body))

			// Assert.
			if (err != nil) != tt.wantErr {
				t.Fatalf("err = %v, wantErr %t", err, tt.wantErr)
			}
			if len(got) != len(tt.want) {
				t.Fatalf("got %v, want %v", got, tt.want)
			}
			for i := range got {
				if got[i] != tt.want[i] {
					t.Fatalf("got %v, want %v", got, tt.want)
				}
			}
		})
	}
}
