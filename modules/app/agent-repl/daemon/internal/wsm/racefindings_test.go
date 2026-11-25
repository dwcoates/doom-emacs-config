package wsm

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

var raceTest = RaceFindingRef{Suite: "e2e", Test: "TestLanding"}

// failedIn is LANDING's gate failure under DIGEST in MERGE at AT.
func failedIn(merge, digest string, at time.Time) GateTestFailure {
	return GateTestFailure{Ref: raceTest, InputsDigest: digest, Merge: merge, FailedAt: at, Failure: "boom in " + merge, Artifacts: "/archive/" + merge}
}

// recordFailures records FAILURES in REPO, failing the test on an error.
func recordFailures(t *testing.T, s *store, repo RepoKey, failures ...GateTestFailure) {
	t.Helper()
	if err := s.RecordGateFailures(context.Background(), repo, failures); err != nil {
		t.Fatalf("RecordGateFailures: %v", err)
	}
}

// promote passes LANDING under DIGEST in REPO, answering its promotions.
func promote(t *testing.T, s *store, repo RepoKey, digest string) []Promotion {
	t.Helper()
	got, err := s.PromoteRaces(context.Background(), repo, []TestPass{{Ref: raceTest, InputsDigest: digest}})
	if err != nil {
		t.Fatalf("PromoteRaces: %v", err)
	}
	return got
}

func TestAPassUnderAFailuresDigestPromotesItToARaceFinding(t *testing.T) {
	tests := []struct {
		name         string
		arrange      func(t *testing.T, s *store, repo RepoKey)
		passDigest   string
		wantPromoted bool
		wantNew      bool
		wantReopened bool
		wantMerges   []string
		wantRecur    int
		wantFailure  string
	}{
		{name: "a pass under the same digest is a new finding",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				recordFailures(t, s, repo, failedIn("merge-a", "d1", instant))
			},
			passDigest: "d1", wantPromoted: true, wantNew: true, wantMerges: []string{"merge-a"}, wantRecur: 1, wantFailure: "boom in merge-a"},
		{name: "a pass under another digest promotes nothing",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				recordFailures(t, s, repo, failedIn("merge-a", "d1", instant))
			},
			passDigest: "d2"},
		{name: "a pass with no failure behind it promotes nothing",
			arrange:    func(t *testing.T, s *store, repo RepoKey) {},
			passDigest: "d1"},
		{name: "another merge's failure is a recurrence of the same finding",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				recordFailures(t, s, repo, failedIn("merge-a", "d1", instant))
				promote(t, s, repo, "d1")
				recordFailures(t, s, repo, failedIn("merge-b", "d2", instant.Add(time.Hour)))
			},
			passDigest: "d2", wantPromoted: true, wantMerges: []string{"merge-a", "merge-b"}, wantRecur: 2, wantFailure: "boom in merge-b"},
		{name: "a recurrence after the fix deployed reopens it",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				recordFailures(t, s, repo, failedIn("merge-a", "d1", instant))
				promote(t, s, repo, "d1")
				if err := s.TakeOnRaceFindings(context.Background(), repo, []RaceFindingRef{raceTest}, instant.Add(time.Minute)); err != nil {
					t.Fatal(err)
				}
				if _, err := s.FixRaceFindings(context.Background(), repo, instant.Add(2*time.Minute)); err != nil {
					t.Fatal(err)
				}
				recordFailures(t, s, repo, failedIn("merge-b", "d2", instant.Add(time.Hour)))
			},
			passDigest: "d2", wantPromoted: true, wantReopened: true, wantMerges: []string{"merge-a", "merge-b"}, wantRecur: 2, wantFailure: "boom in merge-b"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			tt.arrange(t, s, repo)

			// Act
			got := promote(t, s, repo, tt.passDigest)

			// Assert
			if !tt.wantPromoted {
				if len(got) != 0 {
					t.Fatalf("promotions = %+v, want none", got)
				}
				return
			}
			if len(got) != 1 {
				t.Fatalf("promotions = %+v, want one", got)
			}
			p := got[0]
			if p.New != tt.wantNew || p.Reopened != tt.wantReopened {
				t.Fatalf("promotion new %v reopened %v, want %v %v", p.New, p.Reopened, tt.wantNew, tt.wantReopened)
			}
			f := p.Finding
			if !slices.Equal(f.Merges, tt.wantMerges) || f.Recurrences != tt.wantRecur || f.Failure != tt.wantFailure || !f.Open() {
				t.Fatalf("finding = %+v, want open, merges %v, %d recurrences, failure %q", f, tt.wantMerges, tt.wantRecur, tt.wantFailure)
			}
		})
	}
}

func TestAPromotedFailureIsForgottenAsACandidate(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	recordFailures(t, s, repo, failedIn("merge-a", "d1", instant))
	promote(t, s, repo, "d1")

	// Act
	again := promote(t, s, repo, "d1")

	// Assert
	findings, err := s.RaceFindings(context.Background(), repo)
	if err != nil || len(findings) != 1 || findings[0].Recurrences != 1 || len(again) != 0 {
		t.Fatalf("findings = (%+v, %v), second promotions %+v; want one recurrence, a candidate promoted once", findings, err, again)
	}
}

func TestAFindingTakenOnStaysAtWorkAcrossARecurrence(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	recordFailures(t, s, repo, failedIn("merge-a", "d1", instant))
	promote(t, s, repo, "d1")
	if err := s.TakeOnRaceFindings(context.Background(), repo, []RaceFindingRef{raceTest}, instant.Add(time.Minute)); err != nil {
		t.Fatal(err)
	}
	recordFailures(t, s, repo, failedIn("merge-b", "d2", instant.Add(time.Hour)))

	// Act
	got := promote(t, s, repo, "d2")

	// Assert
	if len(got) != 1 || got[0].Finding.Open() || got[0].Finding.Recurrences != 2 {
		t.Fatalf("promotions = %+v, want the finding still taken on, with its recurrence", got)
	}
}

func TestFixingRaceFindingsMarksOnlyThoseTakenOn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	other := RaceFindingRef{Suite: "daemon", Test: "TestOther"}
	recordFailures(t, s, repo, failedIn("merge-a", "d1", instant),
		GateTestFailure{Ref: other, InputsDigest: "o1", Merge: "merge-a", FailedAt: instant})
	promote(t, s, repo, "d1")
	if _, err := s.PromoteRaces(context.Background(), repo, []TestPass{{Ref: other, InputsDigest: "o1"}}); err != nil {
		t.Fatal(err)
	}
	if err := s.TakeOnRaceFindings(context.Background(), repo, []RaceFindingRef{raceTest}, instant.Add(time.Minute)); err != nil {
		t.Fatal(err)
	}

	// Act
	n, err := s.FixRaceFindings(context.Background(), repo, instant.Add(time.Hour))

	// Assert
	findings, _ := s.RaceFindings(context.Background(), repo)
	fixed := map[string]bool{}
	for _, f := range findings {
		fixed[f.Ref.String()] = f.FixedAt != nil && f.Fixes == 1
	}
	if err != nil || n != 1 || !fixed[raceTest.String()] || fixed[other.String()] {
		t.Fatalf("FixRaceFindings = (%d, %v), fixed %v; want only the taken-on finding fixed", n, err, fixed)
	}
}

func TestTakingOnAnUnknownRaceFindingWritesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	recordFailures(t, s, repo, failedIn("merge-a", "d1", instant))
	promote(t, s, repo, "d1")
	unknown := RaceFindingRef{Suite: "daemon", Test: "TestNobodySaw"}

	// Act
	err := s.TakeOnRaceFindings(context.Background(), repo, []RaceFindingRef{raceTest, unknown}, instant.Add(time.Minute))

	// Assert
	var refused *UnknownRaceFindingError
	findings, _ := s.RaceFindings(context.Background(), repo)
	if !errors.As(err, &refused) || refused.Ref != unknown || !findings[0].Open() {
		t.Fatalf("TakeOnRaceFindings = %v, findings %+v; want the unknown one refused and nothing taken on", err, findings)
	}
}

func TestCheckRaceFindingsNamesTheFirstUnknownOne(t *testing.T) {
	tests := []struct {
		name string
		refs []RaceFindingRef
		want *RaceFindingRef
	}{
		{name: "every finding held", refs: []RaceFindingRef{raceTest}},
		{name: "an unknown one", refs: []RaceFindingRef{raceTest, {Suite: "daemon", Test: "TestX"}}, want: &RaceFindingRef{Suite: "daemon", Test: "TestX"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			recordFailures(t, s, repo, failedIn("merge-a", "d1", instant))
			promote(t, s, repo, "d1")

			// Act
			err := s.CheckRaceFindings(context.Background(), repo, tt.refs)

			// Assert
			var refused *UnknownRaceFindingError
			if tt.want == nil && err != nil || tt.want != nil && (!errors.As(err, &refused) || refused.Ref != *tt.want) {
				t.Fatalf("CheckRaceFindings = %v, want unknown %v", err, tt.want)
			}
		})
	}
}

func TestAGateFailurePastItsKeepIsForgotten(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	recordFailures(t, s, repo, failedIn("merge-a", "d1", instant))

	// Act
	recordFailures(t, s, repo, failedIn("merge-b", "d2", instant.Add(GateFailureKeep+time.Hour)))

	// Assert
	if got := promote(t, s, repo, "d1"); len(got) != 0 {
		t.Fatalf("promotions = %+v, want the aged-out failure forgotten", got)
	}
}

func TestIncompleteRaceRecordsAreRefusedAtError(t *testing.T) {
	tests := []struct {
		name string
		op   string
		call func(s *store, repo RepoKey) error
	}{
		{name: "a gate failure with no digest", op: "daemon.wsm.record_gate_failures", call: func(s *store, repo RepoKey) error {
			return s.RecordGateFailures(context.Background(), repo, []GateTestFailure{{Ref: raceTest, Merge: "m", FailedAt: instant}})
		}},
		{name: "a gate failure with no test", op: "daemon.wsm.record_gate_failures", call: func(s *store, repo RepoKey) error {
			return s.RecordGateFailures(context.Background(), repo, []GateTestFailure{{Ref: RaceFindingRef{Suite: "e2e"}, InputsDigest: "d", Merge: "m", FailedAt: instant}})
		}},
		{name: "a gate failure with no merge", op: "daemon.wsm.record_gate_failures", call: func(s *store, repo RepoKey) error {
			return s.RecordGateFailures(context.Background(), repo, []GateTestFailure{{Ref: raceTest, InputsDigest: "d", FailedAt: instant}})
		}},
		{name: "a pass with no digest", op: "daemon.wsm.promote_races", call: func(s *store, repo RepoKey) error {
			_, err := s.PromoteRaces(context.Background(), repo, []TestPass{{Ref: raceTest}})
			return err
		}},
		{name: "a take-on naming nothing", op: "daemon.wsm.take_on_race_findings", call: func(s *store, repo RepoKey) error {
			return s.TakeOnRaceFindings(context.Background(), repo, nil, instant)
		}},
		{name: "a fix with no instant", op: "daemon.wsm.fix_race_findings", call: func(s *store, repo RepoKey) error {
			_, err := s.FixRaceFindings(context.Background(), repo, time.Time{})
			return err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			repo := RepoKey(t.TempDir())

			// Act
			err := tt.call(s, repo)

			// Assert
			findings, _ := s.RaceFindings(context.Background(), repo)
			if err == nil || len(findings) != 0 {
				t.Fatalf("err = %v, findings %+v; want a refusal and nothing written", err, findings)
			}
			if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: tt.op}); !ok {
				t.Fatalf("no ERROR record under %s: %+v", tt.op, logs.Records())
			}
		})
	}
}

func TestTheRaceFindingsLayoutSeedsOnlyTheCheckedRepository(t *testing.T) {
	tests := []struct {
		name  string
		query string
		want  int
	}{
		{name: "the checked repository holds every seeded race", query: `SELECT count(*) FROM queue_race_findings WHERE repo_key = '/Users/x/.config/doom/.git'`, want: 9},
		{name: "another repository holds none", query: `SELECT count(*) FROM queue_race_findings WHERE repo_key = '/tmp/other'`},
		{name: "the adoption race names its six merges", query: `SELECT count(*) FROM queue_race_finding_merges WHERE test = 'TestAdoptWebWorkspaceRefusesParticipantNotExpectedForAClientNotOpenAtAnnouncement'`, want: 6},
		{name: "every seed is open", query: `SELECT count(*) FROM queue_race_findings WHERE taken_on_at IS NOT NULL OR fixed_at IS NOT NULL`},
		{name: "no candidate is seeded", query: `SELECT count(*) FROM queue_gate_failures`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := fixtureAt(t, 213)
			execRaw(t, path, `INSERT INTO merge_queue_repos (repo_key, paused) VALUES ('/Users/x/.config/doom/.git', 0), ('/tmp/other', 0)`)

			// Act
			execRaw(t, path, raceFindingsDDL)

			// Assert
			if got := rawScalar[int](t, path, tt.query); got != tt.want {
				t.Fatalf("%s = %d, want %d", tt.query, got, tt.want)
			}
		})
	}
}
