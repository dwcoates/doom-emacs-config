package feed

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/sourcescan"
)

func TestRestateRowPublishesTheEditedSnapshotAndLeavesTheStoredRowAlone(t *testing.T) {
	// Arrange: a response row is stored on the root feed.
	h := newHarness(t)
	h.main(settledProse("unit-1", "an answer"))
	h.resolver.mu.Lock()
	s := h.resolver.state(testWorkspace)
	f := h.resolver.feed(s, feedid.Feed{Root: true})
	id := s.answerRows["unit-1"].GetValue()
	stored := f.rows[id]

	// Act.
	pushed := h.resolver.restateRow(s, placement{feed: feedid.Feed{Root: true}}, stored, true, unclonable{
		operation: "test.unclonable", message: "unused", context: dlog.Context{},
	}, func(row *frontendv1.FeedRow) {
		row.GetActivity().GetResponse().FinalAnswer = true
	})
	republished := f.rows[id]
	h.resolver.mu.Unlock()

	// Assert.
	if !pushed {
		t.Fatal("restateRow = false, want the row re-pushed")
	}
	if stored.GetActivity().GetResponse().GetFinalAnswer() {
		t.Fatal("the stored row was edited in place, want only the snapshot edited")
	}
	if !republished.GetActivity().GetResponse().GetFinalAnswer() {
		t.Fatal("the feed's row lacks the edit, want the edited snapshot published")
	}
}

func TestEveryStoredRowRestatementGoesThroughRestateRow(t *testing.T) {
	// Arrange: the only production files allowed to snapshot a row by hand are
	// the publication path itself and the restatement helper.
	allowed := map[string]bool{"resolver.go": true, "restate.go": true}
	var offenders []string

	// Act.
	for _, file := range sourcescan.Production(t) {
		if allowed[file.Name] {
			continue
		}
		if strings.Contains(string(file.Source), "proto.Clone(") {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled row restatements (use restateRow): %s", strings.Join(offenders, ", "))
	}
}
