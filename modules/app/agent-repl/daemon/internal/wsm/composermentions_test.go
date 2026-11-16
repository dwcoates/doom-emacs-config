package wsm

import (
	"context"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
)

func sortedMentions(ms []ComposerMention) []ComposerMention {
	out := slices.Clone(ms)
	slices.SortFunc(out, func(a, b ComposerMention) int {
		return strings.Compare(string(a.Kind)+"/"+a.Name, string(b.Kind)+"/"+b.Name)
	})
	return out
}

func TestComposerMentions(t *testing.T) {
	tests := []struct {
		name  string
		puts  [][]ComposerMention
		wantA []ComposerMention
	}{
		{name: "a workspace with none recorded has none"},
		{
			name:  "a mention is read back whole",
			puts:  [][]ComposerMention{{{Kind: ComposerMentionCommand, Name: "merge-queue", MentionedAtMs: 10}}},
			wantA: []ComposerMention{{Kind: ComposerMentionCommand, Name: "merge-queue", MentionedAtMs: 10}},
		},
		{
			name: "a later mention moves the instant forward",
			puts: [][]ComposerMention{
				{{Kind: ComposerMentionWorkspace, Name: "alpha", MentionedAtMs: 10}},
				{{Kind: ComposerMentionWorkspace, Name: "alpha", MentionedAtMs: 20}},
			},
			wantA: []ComposerMention{{Kind: ComposerMentionWorkspace, Name: "alpha", MentionedAtMs: 20}},
		},
		{
			name: "an earlier mention leaves the later instant standing",
			puts: [][]ComposerMention{
				{{Kind: ComposerMentionWorkspace, Name: "alpha", MentionedAtMs: 20}},
				{{Kind: ComposerMentionWorkspace, Name: "alpha", MentionedAtMs: 10}},
			},
			wantA: []ComposerMention{{Kind: ComposerMentionWorkspace, Name: "alpha", MentionedAtMs: 20}},
		},
		{
			name: "one name of each kind is two records",
			puts: [][]ComposerMention{{
				{Kind: ComposerMentionWorkspace, Name: "clear", MentionedAtMs: 5},
				{Kind: ComposerMentionCommand, Name: "clear", MentionedAtMs: 6},
			}},
			wantA: []ComposerMention{
				{Kind: ComposerMentionCommand, Name: "clear", MentionedAtMs: 6},
				{Kind: ComposerMentionWorkspace, Name: "clear", MentionedAtMs: 5},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			a := testWorkspace(t, s).ID
			b := testWorkspace(t, s).ID
			for _, put := range tt.puts {
				for i := range put {
					put[i].Workspace = a
				}
				if err := s.PutComposerMentions(context.Background(), put); err != nil {
					t.Fatalf("PutComposerMentions: %v", err)
				}
			}
			if err := s.PutComposerMentions(context.Background(), []ComposerMention{
				{Workspace: b, Kind: ComposerMentionCommand, Name: "other", MentionedAtMs: 1},
			}); err != nil {
				t.Fatalf("PutComposerMentions(b): %v", err)
			}

			// Act
			got, err := s.ComposerMentions(context.Background())

			// Assert
			if err != nil {
				t.Fatalf("ComposerMentions: %v", err)
			}
			var gotA []ComposerMention
			for _, m := range got {
				if m.Workspace == a {
					m.Workspace = ""
					gotA = append(gotA, m)
				}
			}
			if !slices.Equal(sortedMentions(gotA), sortedMentions(tt.wantA)) {
				t.Fatalf("workspace a's mentions = %+v, want %+v", gotA, tt.wantA)
			}
		})
	}
}

func TestAMalformedComposerMentionIsRefused(t *testing.T) {
	tests := []struct {
		name string
		m    ComposerMention
	}{
		{name: "an unknown kind", m: ComposerMention{Kind: "skill", Name: "x", MentionedAtMs: 1}},
		{name: "an empty name", m: ComposerMention{Kind: ComposerMentionCommand, MentionedAtMs: 1}},
		{name: "no instant", m: ComposerMention{Kind: ComposerMentionCommand, Name: "x"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, rec := testStore(t)
			ws := testWorkspace(t, s).ID
			good := ComposerMention{Workspace: ws, Kind: ComposerMentionCommand, Name: "good", MentionedAtMs: 1}
			tt.m.Workspace = ws

			// Act
			err := s.PutComposerMentions(context.Background(), []ComposerMention{good, tt.m})

			// Assert
			if err == nil {
				t.Fatal("PutComposerMentions accepted a malformed mention")
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM composer_mentions`); n != 0 {
				t.Fatalf("%d rows written, want none: the batch is refused whole", n)
			}
			records := dlog.FilterRecords(rec.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.put_composer_mentions"})
			if len(records) != 1 || records[0].Context["mentions"] != 2 || records[0].Context["error"] == nil {
				t.Fatalf("records = %+v, want one ERROR naming the batch and its cause", rec.Records())
			}
		})
	}
}

func TestAWorkspacesComposerMentionsGoWithIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	if err := s.PutComposerMentions(context.Background(), []ComposerMention{
		{Workspace: ws, Kind: ComposerMentionCommand, Name: "x", MentionedAtMs: 1},
	}); err != nil {
		t.Fatalf("PutComposerMentions: %v", err)
	}

	// Act
	if _, err := s.db().ExecContext(context.Background(), `DELETE FROM workspaces WHERE id = ?`, ws); err != nil {
		t.Fatalf("delete: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM composer_mentions`); n != 0 {
		t.Fatalf("%d mention rows, want none after the workspace went", n)
	}
}

// TestTheMigrationAddsTheComposerMentionsTable pins the layout-151 step: a
// file written before mentions were durable carries the table afterwards,
// with nothing recorded.
func TestTheMigrationAddsTheComposerMentionsTable(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	if got := scalar[int](t, handle.(*store), `SELECT count(*) FROM composer_mentions`); got != 0 {
		t.Fatalf("%d migrated rows, want none", got)
	}
}
