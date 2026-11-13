package wsm

import (
	"context"
	"testing"
	"time"
)

func TestARepositoryWithNoDeployOwesNoMediation(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	m, err := s.MediationOf(context.Background(), RepoKey(t.TempDir()))

	// Assert
	if err != nil || m != nil {
		t.Fatalf("MediationOf = (%+v, %v), want none", m, err)
	}
}

func TestADeployOwesAMediationUnprompted(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())

	// Act
	if err := s.OweMediation(ctx, repo, instant, "0.431.0"); err != nil {
		t.Fatalf("OweMediation: %v", err)
	}
	m, err := s.MediationOf(ctx, repo)

	// Assert
	if err != nil || m == nil || !m.OwedAt.Equal(instant) || m.Version != "0.431.0" || m.Prompted() {
		t.Fatalf("MediationOf = (%+v, %v), want the deploy's mediation, unprompted", m, err)
	}
}

func TestAMediationIsPromptedOnlyWhileItIsTheOneOwed(t *testing.T) {
	tests := []struct {
		name string
		owed time.Time
		want bool
	}{
		{name: "the mediation owed", owed: instant, want: true},
		{name: "a mediation a newer deploy replaced", owed: instant.Add(-time.Minute)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			if err := s.OweMediation(ctx, repo, instant, ""); err != nil {
				t.Fatal(err)
			}

			// Act
			marked, err := s.MarkMediationPrompted(ctx, repo, tt.owed, instant.Add(time.Second), "turn-1")

			// Assert
			m, rerr := s.MediationOf(ctx, repo)
			if err != nil || rerr != nil || marked != tt.want || m.Prompted() != tt.want || (m.Turn == "turn-1") != tt.want {
				t.Fatalf("marked %v, prompted %v in %q (%v, %v); want %v", marked, m.Prompted(), m.Turn, err, rerr, tt.want)
			}
		})
	}
}

func TestReleaseMediationClearsOnlyTheMediationItNames(t *testing.T) {
	tests := []struct {
		name     string
		release  time.Time
		wantOwed bool
	}{
		{name: "a decision on the deploy owed releases it", release: instant},
		{name: "a decision on an older deploy releases nothing", release: instant.Add(-time.Minute), wantOwed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			if err := s.OweMediation(ctx, repo, instant, ""); err != nil {
				t.Fatal(err)
			}

			// Act
			released, err := s.ReleaseMediation(ctx, repo, tt.release)

			// Assert
			m, rerr := s.MediationOf(ctx, repo)
			if err != nil || rerr != nil || released == tt.wantOwed || (m != nil) != tt.wantOwed {
				t.Fatalf("released %v, owed %+v (%v, %v); want owed %v", released, m, err, rerr, tt.wantOwed)
			}
		})
	}
}

func TestANewerDeployReplacesTheMediationOwedAndOwesItsPrompt(t *testing.T) {
	// Arrange: the first deploy's mediation was prompted.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	if err := s.OweMediation(ctx, repo, instant, "0.1.0"); err != nil {
		t.Fatal(err)
	}
	if _, err := s.MarkMediationPrompted(ctx, repo, instant, instant.Add(time.Second), "turn-1"); err != nil {
		t.Fatal(err)
	}

	// Act
	if err := s.OweMediation(ctx, repo, instant.Add(time.Minute), "0.2.0"); err != nil {
		t.Fatal(err)
	}

	// Assert
	m, err := s.MediationOf(ctx, repo)
	if err != nil || !m.OwedAt.Equal(instant.Add(time.Minute)) || m.Version != "0.2.0" || m.Prompted() || m.Turn != "" {
		t.Fatalf("MediationOf = (%+v, %v), want the newer deploy's, unprompted", m, err)
	}
}

func TestOweMediationRefusesADeployWithNoInstantAndWritesNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())

	// Act
	err := s.OweMediation(ctx, repo, time.Time{}, "")

	// Assert
	m, rerr := s.MediationOf(ctx, repo)
	if err == nil || rerr != nil || m != nil {
		t.Fatalf("OweMediation = %v, owed %+v; want a refusal and nothing owed", err, m)
	}
	if !hasRecord(log, "error", "daemon.wsm.owe_mediation", "refused a mediation that names no deploy instant") {
		t.Fatalf("no error record of the refusal")
	}
}
