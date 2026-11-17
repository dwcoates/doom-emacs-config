package wsm

import (
	"context"
	"testing"
	"time"
)

// aFixerTurn is a fixer's brief for a dispatch a test records.
var aFixerTurn = FixerTurn{ID: "turn-fixer", Brief: "fix the deploy's problems", Origin: 1}

// fixerTurnOf reads the fixer's brief REPO's owed check records.
func fixerTurnOf(t *testing.T, s *store, repo RepoKey) FixerTurn {
	t.Helper()
	m, err := s.MediationOf(context.Background(), repo)
	if err != nil || m == nil {
		t.Fatalf("MediationOf = (%+v, %v), want a check owed", m, err)
	}
	return m.FixerTurn
}

func TestADispatchRecordsTheFixersBriefWithIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	oweAt(t, s, repo, instant)

	// Act
	dispatchAt(t, s, repo, instant)

	// Assert
	if got := fixerTurnOf(t, s, repo); got != aFixerTurn {
		t.Fatalf("FixerTurn = %+v, want %+v", got, aFixerTurn)
	}
}

func TestAFixersBriefMustNameItsTurnAndText(t *testing.T) {
	tests := []struct {
		name string
		turn FixerTurn
	}{
		{name: "no turn", turn: FixerTurn{Brief: "fix it"}},
		{name: "no text", turn: FixerTurn{ID: "turn-1"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			oweAt(t, s, repo, instant)

			// Act
			dispatched, dispatchErr := s.DispatchMediation(ctx, repo, instant, instant, "remedy", tt.turn)
			recorded, recordErr := s.RecordFixerTurn(ctx, repo, instant, "remedy", tt.turn)

			// Assert
			m, _ := s.MediationOf(ctx, repo)
			if dispatched || dispatchErr == nil || recorded || recordErr == nil || m.Dispatched || m.FixerTurn != (FixerTurn{}) {
				t.Fatalf("DispatchMediation = (%v, %v), RecordFixerTurn = (%v, %v), check %+v; want both refused and nothing written", dispatched, dispatchErr, recorded, recordErr, m)
			}
			if len(log.Records()) == 0 {
				t.Fatal("want the refusals recorded")
			}
		})
	}
}

func TestRecordFixerTurn(t *testing.T) {
	later := FixerTurn{ID: "turn-later", Brief: "the gate failed; fix it", Origin: 2}
	tests := []struct {
		name     string
		dispatch bool
		ws       WorkspaceID
		owed     time.Time
		want     bool
		wantTurn FixerTurn
	}{
		{name: "the fixer the check is dispatched to takes its later brief", dispatch: true, ws: "remedy", owed: instant, want: true, wantTurn: later},
		{name: "another workspace's brief is not the check's", dispatch: true, ws: "other", owed: instant, wantTurn: aFixerTurn},
		{name: "a check dispatched to no fixer records none", ws: "remedy", owed: instant},
		{name: "an older deploy's check records nothing on the newer", dispatch: true, ws: "remedy", owed: instant.Add(-time.Minute), wantTurn: aFixerTurn},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			oweAt(t, s, repo, instant)
			if tt.dispatch {
				dispatchAt(t, s, repo, instant)
			}

			// Act
			got, err := s.RecordFixerTurn(context.Background(), repo, tt.owed, tt.ws, later)

			// Assert
			if err != nil || got != tt.want || fixerTurnOf(t, s, repo) != tt.wantTurn {
				t.Fatalf("RecordFixerTurn = (%v, %v), turn %+v; want %v, %+v", got, err, fixerTurnOf(t, s, repo), tt.want, tt.wantTurn)
			}
		})
	}
}

func TestANewerDeployCarriesTheFixersBriefOnlyWithTheDispatch(t *testing.T) {
	tests := []struct {
		name       string
		fixDeploys bool
		want       FixerTurn
	}{
		{name: "the fix's own deploy owes a check no fixer is dispatched for", fixDeploys: true},
		{name: "another deploy carries the dispatch and its brief on", want: aFixerTurn},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			oweAt(t, s, repo, instant)
			dispatchAt(t, s, repo, instant)
			if tt.fixDeploys {
				finishDeploying(t, s, repo, "remedy", instant.Add(30*time.Second))
			}

			// Act
			if _, err := s.OweMediation(ctx, repo, instant.Add(time.Minute), ""); err != nil {
				t.Fatal(err)
			}

			// Assert
			if got := fixerTurnOf(t, s, repo); got != tt.want {
				t.Fatalf("FixerTurn = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestAReleasedCheckForgetsTheFixersBrief(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	oweAt(t, s, repo, instant)
	dispatchAt(t, s, repo, instant)
	if _, err := s.ReleaseMediation(ctx, repo, instant); err != nil {
		t.Fatal(err)
	}

	// Act
	oweAt(t, s, repo, instant.Add(time.Hour))

	// Assert
	if got := fixerTurnOf(t, s, repo); got != (FixerTurn{}) {
		t.Fatalf("FixerTurn = %+v, want none on a fresh check", got)
	}
}
