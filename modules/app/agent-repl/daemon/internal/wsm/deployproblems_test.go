package wsm

import (
	"context"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

var sinkWarning = ProblemKey{Operation: "agent-repl.elisp-core-log-central-fallback", Message: "elisp.core.log-central-fallback"}

func TestADeployProblemIsOwnedFixedAndTakenOverOnItsRecurrence(t *testing.T) {
	tests := []struct {
		name         string
		arrange      func(t *testing.T, s *store, repo RepoKey)
		wantOwner    WorkspaceID
		wantFixed    bool
		wantFindings []string
	}{
		{name: "a dispatched problem is owned and open",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				own(t, s, repo, "remedy-1", "the sink warning", instant)
			},
			wantOwner: "remedy-1", wantFindings: []string{"the sink warning"}},
		{name: "its owner's deploy marks it fixed",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				own(t, s, repo, "remedy-1", "the sink warning", instant)
				if n, err := s.FixDeployProblemsOf(context.Background(), "remedy-1", instant.Add(time.Hour)); err != nil || n != 1 {
					t.Fatalf("FixDeployProblemsOf = (%d, %v), want one fixed", n, err)
				}
			},
			wantOwner: "remedy-1", wantFixed: true, wantFindings: []string{"the sink warning"}},
		{name: "a recurrence's new owner reopens it with the earlier findings kept",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				own(t, s, repo, "remedy-1", "the sink warning", instant)
				if _, err := s.FixDeployProblemsOf(context.Background(), "remedy-1", instant.Add(time.Hour)); err != nil {
					t.Fatal(err)
				}
				own(t, s, repo, "remedy-2", "it recurred on a direct landing", instant.Add(2*time.Hour))
			},
			wantOwner: "remedy-2", wantFindings: []string{"the sink warning", "it recurred on a direct landing"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())

			// Act
			tt.arrange(t, s, repo)

			// Assert
			problems, err := s.DeployProblems(context.Background(), repo)
			if err != nil || len(problems) != 1 {
				t.Fatalf("DeployProblems = (%+v, %v), want one", problems, err)
			}
			p := problems[0]
			if p.Key != sinkWarning || p.Owner != tt.wantOwner || (p.FixedAt != nil) != tt.wantFixed || !p.OpenedAt.Equal(instant) {
				t.Fatalf("problem = %+v, want owner %s fixed %v opened at %v", p, tt.wantOwner, tt.wantFixed, instant)
			}
			if got := strings.Split(p.Findings, "\n"); strings.Join(got, "|") != strings.Join(tt.wantFindings, "|") {
				t.Fatalf("findings = %q, want %q", got, tt.wantFindings)
			}
		})
	}
}

// own makes OWNER the owner of the sink warning in REPO at AT.
func own(t *testing.T, s *store, repo RepoKey, owner WorkspaceID, findings string, at time.Time) {
	t.Helper()
	if err := s.OwnDeployProblems(context.Background(), repo, []ProblemKey{sinkWarning}, owner, findings, at); err != nil {
		t.Fatalf("OwnDeployProblems: %v", err)
	}
}

func TestOwningDeployProblemsRefusesAnIncompleteOwnership(t *testing.T) {
	tests := []struct {
		name     string
		keys     []ProblemKey
		owner    WorkspaceID
		findings string
	}{
		{name: "no group", owner: "remedy", findings: "x"},
		{name: "no owner", keys: []ProblemKey{sinkWarning}, findings: "x"},
		{name: "no findings", keys: []ProblemKey{sinkWarning}, owner: "remedy", findings: " "},
		{name: "a group with no operation", keys: []ProblemKey{{Message: "m"}}, owner: "remedy", findings: "x"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			repo := RepoKey(t.TempDir())

			// Act
			err := s.OwnDeployProblems(context.Background(), repo, tt.keys, tt.owner, tt.findings, instant)

			// Assert
			problems, _ := s.DeployProblems(context.Background(), repo)
			if err == nil || len(problems) != 0 {
				t.Fatalf("OwnDeployProblems = %v, problems %+v; want a refusal and nothing written", err, problems)
			}
			if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.own_deploy_problems"}); !ok {
				t.Fatalf("no ERROR record of the refusal: %+v", logs.Records())
			}
		})
	}
}

func TestACheckWindowIsRecordedOnItsCheckAndClearedByTheNext(t *testing.T) {
	window := []WindowGroup{{Operation: sinkWarning.Operation, Message: sinkWarning.Message, Class: WindowRecurred}}
	tests := []struct {
		name        string
		then        func(t *testing.T, s *store, repo RepoKey)
		wantWindow  bool
		wantThrough time.Time
	}{
		{name: "the told check carries its window", wantWindow: true, wantThrough: instant.Add(time.Minute)},
		{name: "a newer deploy's check starts with none, the instant kept",
			then:       func(t *testing.T, s *store, repo RepoKey) { oweAt(t, s, repo, instant.Add(2*time.Minute)) },
			wantWindow: false, wantThrough: instant.Add(time.Minute)},
		{name: "a released check leaves none, the instant kept",
			then: func(t *testing.T, s *store, repo RepoKey) {
				if _, err := s.ReleaseMediation(context.Background(), repo, instant); err != nil {
					t.Fatal(err)
				}
			},
			wantWindow: false, wantThrough: instant.Add(time.Minute)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			oweAt(t, s, repo, instant)
			if ok, err := s.RecordMediationWindow(ctx, repo, instant, CheckWindow{Start: instant, Through: instant.Add(time.Minute), Groups: window}); err != nil || !ok {
				t.Fatalf("RecordMediationWindow = (%v, %v)", ok, err)
			}

			// Act
			if tt.then != nil {
				tt.then(t, s, repo)
			}

			// Assert
			got, found, err := s.MediationWindowOf(ctx, repo)
			if err != nil || found != tt.wantWindow || (found && (len(got.Groups) != 1 || got.Groups[0] != window[0] || !got.Start.Equal(instant))) {
				t.Fatalf("MediationWindowOf = (%+v, %v, %v), want found %v", got, found, err, tt.wantWindow)
			}
			through, ok, err := s.CheckedThrough(ctx, repo)
			if err != nil || !ok || !through.Equal(tt.wantThrough) {
				t.Fatalf("CheckedThrough = (%v, %v, %v), want %v", through, ok, err, tt.wantThrough)
			}
		})
	}
}

func TestARepositoryNeverCheckedHasReadNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, ok, err := s.CheckedThrough(context.Background(), RepoKey(t.TempDir()))

	// Assert
	if err != nil || ok {
		t.Fatalf("CheckedThrough = (%v, %v), want nothing read", ok, err)
	}
}

func TestAWindowForACheckNoLongerOwedIsNotRecorded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	oweAt(t, s, repo, instant)

	// Act
	ok, err := s.RecordMediationWindow(ctx, repo, instant.Add(-time.Hour), CheckWindow{Start: instant, Through: instant.Add(time.Minute)})

	// Assert
	if err != nil || ok {
		t.Fatalf("RecordMediationWindow = (%v, %v), want nothing recorded for a check no longer owed", ok, err)
	}
	if _, ok, _ := s.CheckedThrough(ctx, repo); ok {
		t.Fatal("the instant read to moved for a check no longer owed")
	}
}

func TestAWindowThatDoesNotRunForwardIsRefused(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	oweAt(t, s, repo, instant)

	// Act
	_, err := s.RecordMediationWindow(ctx, repo, instant, CheckWindow{Start: instant.Add(time.Minute), Through: instant})

	// Assert
	if err == nil {
		t.Fatal("RecordMediationWindow recorded a window that runs backward")
	}
	if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.record_mediation_window"}); !ok {
		t.Fatalf("no ERROR record of the refusal: %+v", logs.Records())
	}
}
