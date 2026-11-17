package wsm

import (
	"context"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

var sinkWarning = ProblemKey{Operation: "agent-repl.elisp-core-log-central-fallback", Message: "elisp.core.log-central-fallback"}

func TestADeployProblemIsTakenOnFixedAndReopenedOnItsRecurrence(t *testing.T) {
	tests := []struct {
		name         string
		arrange      func(t *testing.T, s *store, repo RepoKey)
		wantFixed    bool
		wantFindings []string
		wantFixes    int
	}{
		{name: "a dispatched problem is open",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				takeOn(t, s, repo, "the sink warning", instant)
			},
			wantFindings: []string{"the sink warning"}},
		{name: "the mediator's deploy marks it fixed",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				takeOn(t, s, repo, "the sink warning", instant)
				if n, err := s.FixDeployProblems(context.Background(), repo, instant.Add(time.Hour)); err != nil || n != 1 {
					t.Fatalf("FixDeployProblems = (%d, %v), want one fixed", n, err)
				}
			},
			wantFixed: true, wantFindings: []string{"the sink warning"}, wantFixes: 1},
		{name: "a recurrence reopens it with the earlier findings kept",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				takeOn(t, s, repo, "the sink warning", instant)
				if _, err := s.FixDeployProblems(context.Background(), repo, instant.Add(time.Hour)); err != nil {
					t.Fatal(err)
				}
				takeOn(t, s, repo, "it recurred on a direct landing", instant.Add(2*time.Hour))
			},
			wantFindings: []string{"the sink warning", "it recurred on a direct landing"}, wantFixes: 1},
		{name: "every deployed fix is counted",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				for i := range 3 {
					takeOn(t, s, repo, "again", instant.Add(time.Duration(i)*time.Hour))
					if _, err := s.FixDeployProblems(context.Background(), repo, instant.Add(time.Duration(i)*time.Hour+time.Minute)); err != nil {
						t.Fatal(err)
					}
				}
			},
			wantFixed: true, wantFindings: []string{"again", "again", "again"}, wantFixes: 3},
		{name: "another repository's fix leaves it open",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				takeOn(t, s, repo, "the sink warning", instant)
				if n, err := s.FixDeployProblems(context.Background(), RepoKey(t.TempDir()), instant.Add(time.Hour)); err != nil || n != 0 {
					t.Fatalf("FixDeployProblems = (%d, %v), want none fixed", n, err)
				}
			},
			wantFindings: []string{"the sink warning"}},
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
			if p.Key != sinkWarning || (p.FixedAt != nil) != tt.wantFixed || !p.OpenedAt.Equal(instant) {
				t.Fatalf("problem = %+v, want fixed %v opened at %v", p, tt.wantFixed, instant)
			}
			if got := strings.Split(p.Findings, "\n"); strings.Join(got, "|") != strings.Join(tt.wantFindings, "|") {
				t.Fatalf("findings = %q, want %q", got, tt.wantFindings)
			}
			if p.Fixes != tt.wantFixes {
				t.Fatalf("fixes = %d, want %d", p.Fixes, tt.wantFixes)
			}
		})
	}
}

// takeOn records that the mediator took on the sink warning in REPO at AT.
func takeOn(t *testing.T, s *store, repo RepoKey, findings string, at time.Time) {
	t.Helper()
	if err := s.TakeOnDeployProblems(context.Background(), repo, []ProblemKey{sinkWarning}, findings, at); err != nil {
		t.Fatalf("TakeOnDeployProblems: %v", err)
	}
}

func TestTakingOnDeployProblemsRefusesAnIncompleteCall(t *testing.T) {
	tests := []struct {
		name     string
		keys     []ProblemKey
		findings string
		at       time.Time
	}{
		{name: "no group", findings: "x", at: instant},
		{name: "no findings", keys: []ProblemKey{sinkWarning}, findings: " ", at: instant},
		{name: "no instant", keys: []ProblemKey{sinkWarning}, findings: "x"},
		{name: "a group with no operation", keys: []ProblemKey{{Message: "m"}}, findings: "x", at: instant},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			repo := RepoKey(t.TempDir())

			// Act
			err := s.TakeOnDeployProblems(context.Background(), repo, tt.keys, tt.findings, tt.at)

			// Assert
			problems, _ := s.DeployProblems(context.Background(), repo)
			if err == nil || len(problems) != 0 {
				t.Fatalf("TakeOnDeployProblems = %v, problems %+v; want a refusal and nothing written", err, problems)
			}
			if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.take_on_deploy_problems"}); !ok {
				t.Fatalf("no ERROR record of the refusal: %+v", logs.Records())
			}
		})
	}
}

func TestFixingDeployProblemsWithNoInstantIsRefused(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	repo := RepoKey(t.TempDir())
	takeOn(t, s, repo, "the sink warning", instant)

	// Act
	_, err := s.FixDeployProblems(context.Background(), repo, time.Time{})

	// Assert
	problems, _ := s.DeployProblems(context.Background(), repo)
	if err == nil || len(problems) != 1 || problems[0].FixedAt != nil {
		t.Fatalf("FixDeployProblems = %v, problems %+v; want a refusal and the problem still open", err, problems)
	}
	if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.fix_deploy_problems"}); !ok {
		t.Fatalf("no ERROR record of the refusal: %+v", logs.Records())
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
