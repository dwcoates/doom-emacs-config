package workspace

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/wsm"
)

func TestParkedReadsTheHibernationTerminal(t *testing.T) {
	for _, tc := range []struct {
		name    string
		session *wsm.Session
		want    bool
	}{
		{
			name:    "the idle sweep stood the session down",
			session: &wsm.Session{Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalHibernated}},
			want:    true,
		},
		{
			name:    "the session died some other way",
			session: &wsm.Session{Terminal: &wsm.SessionTerminal{Kind: "shim_died"}},
			want:    false,
		},
		{
			name:    "the session is alive",
			session: &wsm.Session{},
			want:    false,
		},
		{
			name:    "there is no session record at all",
			session: nil,
			want:    false,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			if tc.session != nil {
				session := *tc.session
				session.Workspace = "w1"
				f.db.sessions["w1"] = session
			}

			// Act.
			got, err := f.verbs.(*verbs).parked(context.Background(), "w1")

			// Assert.
			if err != nil {
				t.Fatalf("parked: %v", err)
			}
			if got != tc.want {
				t.Fatalf("parked = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestAnUnreadableSessionRecordIsNeverReadAsNotParked pins the discipline the
// rest of the daemon holds: "could not tell" is never the benign answer.
func TestAnUnreadableSessionRecordIsNeverReadAsNotParked(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.sessionErr = errors.New("boom")

	// Act.
	_, err := f.verbs.(*verbs).parked(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("parked() answered no error for an unreadable session record")
	}
}

func TestUnparkLiftsTheParkFromBothSessionScopedViews(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	f.verbs.(*verbs).unpark("w1")

	// Assert.
	if len(f.topbarParked) != 1 || f.topbarParked[0] {
		t.Fatalf("topbar parked = %v, want [false]", f.topbarParked)
	}
	if len(f.footer.parked) != 1 || f.footer.parked[0] {
		t.Fatalf("footer parked = %v, want [false]", f.footer.parked)
	}
}
