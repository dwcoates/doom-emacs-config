package wsm

import (
	"context"
	"reflect"
	"testing"

	"agent-repld/internal/dlog"
)

// untoldStores are the two untold-notice tables through their public methods,
// so every case below holds for both: the read and the write are one each
// (untold.go), and a table read or written any other way would fail here.
func untoldStores(t *testing.T) []struct {
	name   string
	op     string
	record func(s *store, subscriber WorkspaceID, subject string, turn TurnID, sentences []string) error
	read   func(s *store, subscriber WorkspaceID, subject string) (TurnID, []string, bool)
} {
	t.Helper()
	ctx := context.Background()
	return []struct {
		name   string
		op     string
		record func(s *store, subscriber WorkspaceID, subject string, turn TurnID, sentences []string) error
		read   func(s *store, subscriber WorkspaceID, subject string) (TurnID, []string, bool)
	}{
		{name: "merge queue", op: "daemon.wsm.record_untold_merge_notice",
			record: func(s *store, sub WorkspaceID, subject string, turn TurnID, said []string) error {
				return s.RecordUntoldMergeNotice(ctx, UntoldMergeNotice{Subscriber: sub, Repo: RepoKey(subject), Turn: turn, Sentences: said})
			},
			read: func(s *store, sub WorkspaceID, subject string) (TurnID, []string, bool) {
				n, found, err := s.UntoldMergeNotice(ctx, sub, RepoKey(subject))
				if err != nil {
					t.Fatalf("UntoldMergeNotice: %v", err)
				}
				return n.Turn, n.Sentences, found
			}},
		{name: "workspace", op: "daemon.wsm.record_untold_workspace_notice",
			record: func(s *store, sub WorkspaceID, subject string, turn TurnID, said []string) error {
				return s.RecordUntoldWorkspaceNotice(ctx, UntoldWorkspaceNotice{Subscriber: sub, Followed: WorkspaceID(subject), Turn: turn, Sentences: said})
			},
			read: func(s *store, sub WorkspaceID, subject string) (TurnID, []string, bool) {
				n, found, err := s.UntoldWorkspaceNotice(ctx, sub, WorkspaceID(subject))
				if err != nil {
					t.Fatalf("UntoldWorkspaceNotice: %v", err)
				}
				return n.Turn, n.Sentences, found
			}},
	}
}

// untoldSubject is a subject the table's key can hold: a registered
// workspace for the workspace table (its foreign key), a queue key otherwise.
func untoldSubject(t *testing.T, s *store, table string) string {
	t.Helper()
	if table == "workspace" {
		return string(testWorkspaceNamed(t, s, "followed").ID)
	}
	return "/repos/doom/.git"
}

func TestEveryUntoldTableRoundTripsAndReplaces(t *testing.T) {
	for _, tt := range untoldStores(t) {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			sub := testWorkspace(t, s).ID
			subject := untoldSubject(t, s, tt.name)
			if err := tt.record(s, sub, subject, "turn-1", []string{"one."}); err != nil {
				t.Fatal(err)
			}

			// Act
			if err := tt.record(s, sub, subject, "", []string{"one.", "two."}); err != nil {
				t.Fatal(err)
			}
			turn, said, found := tt.read(s, sub, subject)

			// Assert
			if !found || turn != "" || !reflect.DeepEqual(said, []string{"one.", "two."}) {
				t.Fatalf("untold = (%q, %v, %v), want the newer notice, its failed submission and both sentences", turn, said, found)
			}
		})
	}
}

func TestEveryUntoldTableAnswersNothingForASubjectWithNone(t *testing.T) {
	for _, tt := range untoldStores(t) {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			sub := testWorkspace(t, s).ID

			// Act
			_, _, found := tt.read(s, sub, untoldSubject(t, s, tt.name))

			// Assert
			if found {
				t.Fatal("an untold notice was answered where none was recorded")
			}
		})
	}
}

func TestEveryUntoldTableRefusesANoticeOfNoChange(t *testing.T) {
	for _, tt := range untoldStores(t) {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			sub := testWorkspace(t, s).ID
			subject := untoldSubject(t, s, tt.name)

			// Act
			err := tt.record(s, sub, subject, "turn-1", nil)

			// Assert
			_, _, found := tt.read(s, sub, subject)
			if err == nil || found {
				t.Fatalf("record = %v, found %v; want a refusal and nothing written", err, found)
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: tt.op}); !ok {
				t.Fatalf("no error record under %s: %+v", tt.op, log.Records())
			}
		})
	}
}
