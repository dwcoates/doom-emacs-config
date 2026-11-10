package wsm

import (
	"context"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"
)

// conflictedCommitRow is a rebase commit tab row as a build before agents
// drew only in their own tabs stored it: its narration, and the retired
// conflict section.
func conflictedCommitRow(t *testing.T) []byte {
	t.Helper()
	commit := withRetiredBytes(&frontendv1.FeedMergeTabRebaseCommit{
		Lines: []*frontendv1.FeedMergeRebaseLine{{Line: &frontendv1.FeedMergeRebaseLine_Text{Text: "conflicted in 1 file(s)"}}},
	}, retiredRebaseCommitConflictTag, marshalled(t, &frontendv1.FeedMergeTabConflictResolution{
		Files: []*frontendv1.FeedMergeConflictFile{{Path: "work.txt"}},
	}))
	return marshalled(t, &frontendv1.FeedRow{Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{
		Kind: &frontendv1.FeedMergeTab_RebaseCommit{RebaseCommit: commit},
	}}})
}

func TestDropRetiredConflictSection(t *testing.T) {
	plainCommit := &frontendv1.FeedRow{Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{
		Kind: &frontendv1.FeedMergeTab_RebaseCommit{RebaseCommit: &frontendv1.FeedMergeTabRebaseCommit{}},
	}}}
	tests := []struct {
		name        string
		raw         func(t *testing.T) []byte
		wantChanged bool
	}{
		{name: "a commit tab with a conflict section", raw: conflictedCommitRow, wantChanged: true},
		{name: "a commit tab without one", raw: func(t *testing.T) []byte { return marshalled(t, plainCommit) }},
		{name: "a merge tab of another kind", raw: func(t *testing.T) []byte {
			return marshalled(t, &frontendv1.FeedRow{Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{
				Kind: &frontendv1.FeedMergeTab_Tests{Tests: &frontendv1.FeedMergeTabTests{}},
			}}})
		}},
		{name: "a row that is no merge tab", raw: func(t *testing.T) []byte {
			return marshalled(t, &frontendv1.FeedRow{Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{}}})
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			raw := tt.raw(t)

			// Act
			out, changed, err := dropRetiredConflictSection(raw)

			// Assert
			if err != nil || changed != tt.wantChanged {
				t.Fatalf("dropRetiredConflictSection = (changed %v, %v), want (changed %v, nil)", changed, err, tt.wantChanged)
			}
			row := &frontendv1.FeedRow{}
			if err := proto.Unmarshal(out, row); err != nil {
				t.Fatal(err)
			}
			if unknown := row.GetMergeTab().GetRebaseCommit().ProtoReflect().GetUnknown(); len(unknown) != 0 {
				t.Fatalf("the commit tab still carries unknown fields %x", unknown)
			}
			if !changed && string(out) != string(raw) {
				t.Fatal("a row with nothing to drop was re-encoded")
			}
		})
	}
}

func TestDropRetiredConflictSectionKeepsTheCommitsNarration(t *testing.T) {
	// Act
	out, _, err := dropRetiredConflictSection(conflictedCommitRow(t))

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(out, row); err != nil {
		t.Fatal(err)
	}
	lines := row.GetMergeTab().GetRebaseCommit().GetLines()
	if len(lines) != 1 || lines[0].GetText() != "conflicted in 1 file(s)" {
		t.Fatalf("the commit's narration = %v, want its one line kept", lines)
	}
}

func TestDropRetiredConflictSectionRefusesARowThatDoesNotDecode(t *testing.T) {
	// Act
	_, _, err := dropRetiredConflictSection([]byte{0xff, 0xff})

	// Assert
	if err == nil {
		t.Fatal("dropRetiredConflictSection read a row that does not decode")
	}
}

func TestTheMigrationDropsStoredConflictSectionsAndKeepsTheRest(t *testing.T) {
	// Arrange -- a layout-111 file holding a conflicted commit tab beside a prompt.
	path := fixtureAt(t, 111)
	prompt := marshalled(t, &frontendv1.FeedRow{Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{}}})
	execRaw(t, path, `INSERT INTO durable_feed_rows (workspace_id, row_id, plane, order_key, row) VALUES ('ws-1', 'commit-1', 0, 'k1', ?)`,
		conflictedCommitRow(t))
	execRaw(t, path, `INSERT INTO durable_feed_rows (workspace_id, row_id, plane, order_key, row) VALUES ('ws-1', 'prompt-1', 0, 'k2', ?)`, prompt)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-111 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	stored := scalar[[]byte](t, s, `SELECT row FROM durable_feed_rows WHERE row_id = 'commit-1'`)
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(stored, row); err != nil {
		t.Fatal(err)
	}
	if unknown := row.GetMergeTab().GetRebaseCommit().ProtoReflect().GetUnknown(); len(unknown) != 0 {
		t.Fatalf("the stored commit tab still carries its conflict section: %x", unknown)
	}
	if got := scalar[[]byte](t, s, `SELECT row FROM durable_feed_rows WHERE row_id = 'prompt-1'`); string(got) != string(prompt) {
		t.Fatal("the prompt row was rewritten")
	}
}
