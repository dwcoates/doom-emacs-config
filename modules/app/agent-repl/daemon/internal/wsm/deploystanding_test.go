package wsm

import (
	"context"
	"testing"
)

// owingQueue answers a store with REPO's queue owing a check, so the queue
// has its row.
func owingQueue(t *testing.T) (*store, RepoKey) {
	t.Helper()
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	if _, err := s.OweMediation(context.Background(), repo, instant, ""); err != nil {
		t.Fatalf("OweMediation: %v", err)
	}
	return s, repo
}

func TestAStandingDeployCheckRoundTripsUnbriefed(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)
	ctx := context.Background()

	// Act
	if err := s.RecordStandingDeployCheck(ctx, repo, StandingIntake, "ws-intake"); err != nil {
		t.Fatalf("RecordStandingDeployCheck: %v", err)
	}
	got, found, err := s.StandingDeployCheckOf(ctx, repo, StandingIntake)

	// Assert
	if err != nil || !found || got != (StandingDeployCheck{Workspace: "ws-intake"}) {
		t.Fatalf("StandingDeployCheckOf = (%+v, %v, %v), want ws-intake unbriefed", got, found, err)
	}
}

func TestAQueueWithNoStandingWorkspaceInARoleAnswersNone(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)

	// Act
	_, found, err := s.StandingDeployCheckOf(context.Background(), repo, StandingFixer)

	// Assert
	if err != nil || found {
		t.Fatalf("StandingDeployCheckOf = (%v, %v), want none", found, err)
	}
}

func TestMarkStandingBriefedMarksTheRolesWorkspace(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)
	ctx := context.Background()
	if err := s.RecordStandingDeployCheck(ctx, repo, StandingFixer, "ws-r"); err != nil {
		t.Fatal(err)
	}

	// Act
	err := s.MarkStandingBriefed(ctx, repo, StandingFixer)

	// Assert
	got, _, readErr := s.StandingDeployCheckOf(ctx, repo, StandingFixer)
	if err != nil || readErr != nil || !got.Briefed {
		t.Fatalf("MarkStandingBriefed = %v, then %+v (%v), want it briefed", err, got, readErr)
	}
}

func TestMarkStandingBriefedRefusesARoleWithNoWorkspace(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)

	// Act
	err := s.MarkStandingBriefed(context.Background(), repo, StandingFixer)

	// Assert
	if err == nil {
		t.Fatal("MarkStandingBriefed = nil, want the missing workspace refused")
	}
}

func TestReplacingAStandingWorkspaceLeavesTheNewOneUnbriefed(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)
	ctx := context.Background()
	if err := s.RecordStandingDeployCheck(ctx, repo, StandingFixer, "ws-old"); err != nil {
		t.Fatal(err)
	}
	if err := s.MarkStandingBriefed(ctx, repo, StandingFixer); err != nil {
		t.Fatal(err)
	}

	// Act
	err := s.RecordStandingDeployCheck(ctx, repo, StandingFixer, "ws-new")

	// Assert
	got, _, readErr := s.StandingDeployCheckOf(ctx, repo, StandingFixer)
	if err != nil || readErr != nil || got != (StandingDeployCheck{Workspace: "ws-new"}) {
		t.Fatalf("after the replace %+v (%v, %v), want ws-new unbriefed", got, err, readErr)
	}
}

func TestAStandingDeployCheckRefusesWhatItCannotRecord(t *testing.T) {
	tests := []struct {
		name string
		role DeployCheckRoleName
		ws   WorkspaceID
	}{
		{name: "a role outside the three", role: "auditor", ws: "ws"},
		{name: "no workspace", role: StandingIntake, ws: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, repo := owingQueue(t)
			ctx := context.Background()

			// Act
			err := s.RecordStandingDeployCheck(ctx, repo, tt.role, tt.ws)

			// Assert
			_, found, _ := s.StandingDeployCheckOf(ctx, repo, StandingIntake)
			if err == nil || found {
				t.Fatalf("RecordStandingDeployCheck = %v, recorded %v; want it refused and nothing written", err, found)
			}
		})
	}
}

// quietRun notes N quiet checks of REPO, answering each one's compaction.
func quietRun(t *testing.T, s *store, repo RepoKey, n int) []bool {
	t.Helper()
	var out []bool
	for range n {
		compact, err := s.NoteQuietDeployCheck(context.Background(), repo)
		if err != nil {
			t.Fatalf("NoteQuietDeployCheck: %v", err)
		}
		out = append(out, compact)
	}
	return out
}

func TestTheQuietRunCompactsOnlyAfterWork(t *testing.T) {
	tests := []struct {
		name  string
		armed bool
		want  []bool
	}{
		{name: "a queue no check worked on never compacts", want: []bool{false, false, false, false}},
		{name: "a dispatch or escalation compacts at the third quiet check, once", armed: true, want: []bool{false, false, true, false}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, repo := owingQueue(t)
			if tt.armed {
				if err := s.NoteDeployCheckWork(context.Background(), repo); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got := quietRun(t, s, repo, 4)

			// Assert
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

func TestWorkMidRunStartsTheQuietRunOver(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)
	ctx := context.Background()
	if err := s.NoteDeployCheckWork(ctx, repo); err != nil {
		t.Fatal(err)
	}
	quietRun(t, s, repo, 2)

	// Act
	if err := s.NoteDeployCheckWork(ctx, repo); err != nil {
		t.Fatal(err)
	}
	got := quietRun(t, s, repo, 3)

	// Assert
	if got[0] || got[1] || !got[2] {
		t.Fatalf("got %v, want the run counted afresh to its third", got)
	}
}

func TestTheQuietRunRefusesAQueueWithNoRow(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())

	// Act
	_, quietErr := s.NoteQuietDeployCheck(ctx, repo)
	workErr := s.NoteDeployCheckWork(ctx, repo)

	// Assert
	if quietErr == nil || workErr == nil {
		t.Fatalf("NoteQuietDeployCheck = %v, NoteDeployCheckWork = %v; want both refused", quietErr, workErr)
	}
}

func TestOweCompaction(t *testing.T) {
	tests := []struct {
		name      string
		vendor    string
		owedFirst *OwedStartWork
		want      bool
		wantOwed  OwedStartWork
	}{
		{name: "a workspace with a conversation owes it", vendor: "v-1", want: true, wantOwed: OwedStartWork{Compact: true}},
		{name: "a workspace with no conversation owes nothing", vendor: ""},
		{name: "a workspace owing a summary keeps it", vendor: "v-1", owedFirst: &OwedStartWork{SummaryOf: "v-0"}, wantOwed: OwedStartWork{SummaryOf: "v-0"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			ws := testWorkspace(t, s)
			sess := Session{Workspace: ws.ID, HostSessionID: "h", VendorSessionID: tt.vendor, StartedAt: instant, LastEngagementAt: instant}
			if err := s.BindSession(ctx, sess, tt.owedFirst); err != nil {
				t.Fatal(err)
			}

			// Act
			owed, err := s.OweCompaction(ctx, ws.ID)

			// Assert
			got, found, readErr := s.OwedStartWork(ctx, ws.ID)
			if err != nil || readErr != nil || owed != tt.want || got != tt.wantOwed || found != (tt.wantOwed != OwedStartWork{}) {
				t.Fatalf("OweCompaction = (%v, %v), owed (%+v, %v, %v); want (%v), owed %+v", owed, err, got, found, readErr, tt.want, tt.wantOwed)
			}
		})
	}
}

func TestAWorkspaceReadsWhetherItIsAStandingDeployCheck(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)
	ctx := context.Background()
	standing, other := testWorkspace(t, s), testWorkspace(t, s)
	if err := s.RecordStandingDeployCheck(ctx, repo, StandingFixer, standing.ID); err != nil {
		t.Fatal(err)
	}

	// Act
	gotStanding, err1 := s.Workspace(ctx, standing.ID)
	gotOther, err2 := s.Workspace(ctx, other.ID)

	// Assert
	if err1 != nil || err2 != nil || !gotStanding.StandingDeployCheck || gotOther.StandingDeployCheck {
		t.Fatalf("StandingDeployCheck = (%v, %v), errors (%v, %v); want only the recorded one", gotStanding.StandingDeployCheck, gotOther.StandingDeployCheck, err1, err2)
	}
}
