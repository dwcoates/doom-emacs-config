package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// promptRow is a user prompt row saying TEXT.
func promptRow(id, text string) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{
		Id:    &frontendv1.FeedId{Value: id},
		Order: &frontendv1.FeedRowOrder{Key: "1"},
		Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{
			Author: &frontendv1.FeedUserPromptAuthor{Label: text},
		}},
	}
}

func TestStampRevision(t *testing.T) {
	tests := []struct {
		name  string
		a, b  *frontendv1.FeedRow
		equal bool
	}{
		{name: "equal rows carry equal revisions", a: promptRow("r", "hello"), b: promptRow("r", "hello"), equal: true},
		{name: "rows differing in content carry different revisions", a: promptRow("r", "hello"), b: promptRow("r", "hello again"), equal: false},
		{
			name: "a revision already on the row is not part of its digest",
			a:    promptRow("r", "hello"),
			b: func() *frontendv1.FeedRow {
				row := promptRow("r", "hello")
				row.Revision = &frontendv1.FeedRowRevision{Digest: "stale"}
				return row
			}(),
			equal: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			errA, errB := stampRevision(tt.a), stampRevision(tt.b)

			// Assert.
			if errA != nil || errB != nil {
				t.Fatalf("stampRevision: %v, %v", errA, errB)
			}
			if tt.a.GetRevision().GetDigest() == "" {
				t.Fatal("the row was stamped with an empty revision")
			}
			if got := tt.a.GetRevision().GetDigest() == tt.b.GetRevision().GetDigest(); got != tt.equal {
				t.Fatalf("revisions %q and %q: equal = %v, want %v", tt.a.GetRevision().GetDigest(), tt.b.GetRevision().GetDigest(), got, tt.equal)
			}
		})
	}
}

func TestStampRevisionRefusesARowThatCannotBeEncoded(t *testing.T) {
	// Arrange: a string field that is not UTF-8 cannot be encoded.
	row := promptRow("r", "\xff")

	// Act.
	err := stampRevision(row)

	// Assert.
	if err == nil {
		t.Fatal("stampRevision accepted a row that cannot be encoded")
	}
	if row.GetRevision() != nil {
		t.Fatalf("revision = %v, want none stamped on a failure", row.GetRevision())
	}
}

func TestAPublishedRowCarriesItsRevision(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.sendAt(settledResponse("unit-1", "the answer"), 100)

	// Assert.
	row := h.rowByID(rootFeed(), h.responseRowID("unit-1"))
	want := cloneRow(t, row)
	if err := stampRevision(want); err != nil {
		t.Fatalf("stampRevision: %v", err)
	}
	if row.GetRevision().GetDigest() == "" || row.GetRevision().GetDigest() != want.GetRevision().GetDigest() {
		t.Fatalf("revision = %q, want the digest of the row as published (%q)", row.GetRevision().GetDigest(), want.GetRevision().GetDigest())
	}
}

func TestARowThatChangedIsPublishedAtANewRevision(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendAt(settledResponse("unit-1", "first words"), 100)
	id := h.responseRowID("unit-1")
	before := h.rowByID(rootFeed(), id).GetRevision().GetDigest()

	// Act.
	h.sendAt(settledResponse("unit-1", "first words, and more"), 100)

	// Assert.
	if after := h.rowByID(rootFeed(), id).GetRevision().GetDigest(); after == before {
		t.Fatalf("revision stayed %q though the row changed", after)
	}
}

func TestARowRestatedUnchangedKeepsItsRevision(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendAt(settledResponse("unit-1", "the answer"), 100)
	id := h.responseRowID("unit-1")
	before := h.rowByID(rootFeed(), id).GetRevision().GetDigest()

	// Act.
	h.sendAt(settledResponse("unit-1", "the answer"), 100)

	// Assert.
	if !h.hasRecord("debug", "daemon.feed.row_unchanged") {
		t.Fatal("the unchanged restatement was published again")
	}
	if after := h.rowByID(rootFeed(), id).GetRevision().GetDigest(); after != before {
		t.Fatalf("revision %q became %q though the row did not change", before, after)
	}
}

func TestARowThatCannotBeDigestedIsRecordedAndNotPublished(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	row := promptRow("row-undigestable", "\xff")

	// Act.
	h.resolver.mu.Lock()
	h.resolver.upsert(h.resolver.state(testWorkspace), placement{feed: rootFeed()}, row, true)
	h.resolver.mu.Unlock()

	// Assert.
	if !h.hasRecord("error", "daemon.feed.row_revision_failed") {
		t.Fatal("the undigestable row was not recorded at ERROR")
	}
	if _, held := h.findRow(rootFeed(), "row-undigestable"); held {
		t.Fatal("the undigestable row was stored and published")
	}
}

func TestRevisionsAreIdenticalAcrossADaemonRestart(t *testing.T) {
	// Arrange: one daemon draws the conversation live.
	live := newHarness(t)
	live.deliverPromptAt("turn-1", "the prompt", 100)
	live.sendAt(settledResponse("unit-1", "the answer"), 200)
	want := map[string]string{}
	for _, row := range live.rows(rootFeed()) {
		want[row.GetId().GetValue()] = row.GetRevision().GetDigest()
	}

	// Act: a successor draws the same entries from the store's page.
	restarted := newHarness(t)
	restarted.replay(placedPage(&conversationv1.HistoryFloor{},
		pagedEntry{atMs: 200, entry: frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: settledResponse("unit-1", "the answer")},
		})},
		pagedEntry{atMs: 100, entry: promptEntry("turn-1", "the prompt")},
	))

	// Assert.
	for id, revision := range want {
		if got := restarted.rowByID(rootFeed(), id).GetRevision().GetDigest(); got != revision {
			t.Fatalf("row %s: revision %q after the restart, %q before", id, got, revision)
		}
	}
}
