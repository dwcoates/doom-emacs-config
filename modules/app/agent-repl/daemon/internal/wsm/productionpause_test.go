package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

// pausedAt pauses REPO's production checks at AT, failing the test on error.
func pausedAt(t *testing.T, s *store, repo RepoKey, at time.Time) []ProductionCheck {
	t.Helper()
	dropped, err := s.PauseProduction(context.Background(), repo, at)
	if err != nil {
		t.Fatalf("PauseProduction = %v", err)
	}
	return dropped
}

func TestPausingDropsEveryUndecidedCheckAndKeepsTheDecided(t *testing.T) {
	// Arrange: a decided span, a told one, a due one, and one owed but not due.
	s, _ := testStore(t)
	ctx := context.Background()
	repo, key := productionRepo(t)
	readThrough(t, s, key, instant)
	decided, told, due, owed := instant.Add(time.Hour), instant.Add(2*time.Hour), instant.Add(3*time.Hour), instant.Add(4*time.Hour)
	toldCheck(t, s, repo, decided)
	if err := s.DecideProductionCheck(ctx, repo, decided, decided.Add(3*time.Minute), ProductionNothingToDo, "quiet", nil); err != nil {
		t.Fatal(err)
	}
	toldCheck(t, s, repo, told)
	if _, o, err := s.OweProductionCheck(ctx, repo, due, ""); err != nil || o != ProductionSpanOwed {
		t.Fatalf("OweProductionCheck = (%v, %v)", o, err)
	}
	if _, err := s.MarkProductionChecksDue(ctx, repo, due.Add(time.Minute)); err != nil {
		t.Fatal(err)
	}
	if _, o, err := s.OweProductionCheck(ctx, repo, owed, ""); err != nil || o != ProductionSpanOwed {
		t.Fatalf("OweProductionCheck = (%v, %v)", o, err)
	}

	// Act
	dropped := pausedAt(t, s, repo, owed.Add(time.Minute))

	// Assert
	t.Run("the undecided checks are answered oldest first", func(t *testing.T) {
		if len(dropped) != 3 || !dropped[0].Until.Equal(told) || !dropped[1].Until.Equal(due) || !dropped[2].Until.Equal(owed) {
			t.Fatalf("dropped = %+v, want the told, due and owed spans in order", dropped)
		}
	})
	t.Run("the told check names its turn", func(t *testing.T) {
		if dropped[0].Turn != "turn-1" || !dropped[0].Prompted() {
			t.Fatalf("the told check = %+v, want it prompted in turn-1", dropped[0])
		}
	})
	t.Run("no check is left to tell", func(t *testing.T) {
		if c, err := s.ProductionCheckOf(ctx, repo); err != nil || c != nil {
			t.Fatalf("ProductionCheckOf = (%+v, %v), want none", c, err)
		}
	})
	t.Run("the decided check is kept", func(t *testing.T) {
		prev, found, err := s.PreviousProductionSpan(ctx, repo, told)
		if err != nil || !found || !prev.Until.Equal(decided) {
			t.Fatalf("PreviousProductionSpan = (%+v, %v, %v), want the decided span", prev, found, err)
		}
	})
	t.Run("a dropped check can no longer be marked told", func(t *testing.T) {
		if marked, err := s.MarkProductionCheckPrompted(ctx, repo, due, owed.Add(2*time.Minute), "turn-2"); err != nil || marked {
			t.Fatalf("MarkProductionCheckPrompted = (%v, %v), want nothing marked", marked, err)
		}
	})
	t.Run("the repository stands paused", func(t *testing.T) {
		pause, err := s.ProductionPauseOf(ctx, repo)
		if err != nil || !pause.Paused || !pause.Since.Equal(owed.Add(time.Minute)) {
			t.Fatalf("ProductionPauseOf = (%+v, %v), want paused since the pause", pause, err)
		}
	})
}

func TestAPauseOrResumeOutOfTurnIsRefusedAndChangesNothing(t *testing.T) {
	cases := []struct {
		name string
		// arrange brings the repository to the standing the action meets.
		arrange func(t *testing.T, s *store, repo RepoKey)
		// act is the refused action.
		act        func(s *store, repo RepoKey) error
		want       error
		wantPaused bool
	}{
		{
			name:    "a pause of paused checks",
			arrange: func(t *testing.T, s *store, repo RepoKey) { pausedAt(t, s, repo, instant) },
			act: func(s *store, repo RepoKey) error {
				_, err := s.PauseProduction(context.Background(), repo, instant.Add(time.Hour))
				return err
			},
			want: ErrProductionPaused, wantPaused: true,
		},
		{
			name:    "a resume of a repository never paused",
			arrange: func(*testing.T, *store, RepoKey) {},
			act: func(s *store, repo RepoKey) error {
				return s.ResumeProduction(context.Background(), repo, instant)
			},
			want: ErrProductionNotPaused,
		},
		{
			name: "a resume of checks already resumed",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				pausedAt(t, s, repo, instant)
				if err := s.ResumeProduction(context.Background(), repo, instant.Add(time.Minute)); err != nil {
					t.Fatal(err)
				}
			},
			act: func(s *store, repo RepoKey) error {
				return s.ResumeProduction(context.Background(), repo, instant.Add(time.Hour))
			},
			want: ErrProductionNotPaused,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo, _ := productionRepo(t)
			tc.arrange(t, s, repo)

			// Act
			err := tc.act(s, repo)

			// Assert
			if !errors.Is(err, tc.want) {
				t.Fatalf("err = %v, want %v", err, tc.want)
			}
			pause, err := s.ProductionPauseOf(context.Background(), repo)
			if err != nil || pause.Paused != tc.wantPaused || (tc.wantPaused && !pause.Since.Equal(instant)) {
				t.Fatalf("ProductionPauseOf = (%+v, %v), want paused %v from the first pause", pause, err, tc.wantPaused)
			}
		})
	}
}

func TestSpansTouchingAPauseAreSkippedAndTheChecksRestartAtTheFirstLandingAfterTheResume(t *testing.T) {
	// Arrange: the checks read through INSTANT, then pause, resume, and see
	// four landings: one in the pause, then three after the resume.
	s, _ := testStore(t)
	ctx := context.Background()
	repo, key := productionRepo(t)
	readThrough(t, s, key, instant)
	pause, inPause, resume := instant.Add(time.Hour), instant.Add(2*time.Hour), instant.Add(3*time.Hour)
	firstAfter, secondAfter := instant.Add(4*time.Hour), instant.Add(5*time.Hour)
	pausedAt(t, s, repo, pause)

	// Act
	_, duringPause, errPause := s.OweProductionCheck(ctx, repo, inPause, "")
	errResume := s.ResumeProduction(ctx, repo, resume)
	_, atFirst, errFirst := s.OweProductionCheck(ctx, repo, firstAfter, "")
	check, atSecond, errSecond := s.OweProductionCheck(ctx, repo, secondAfter, "0.3.0")

	// Assert
	for _, err := range []error{errPause, errResume, errFirst, errSecond} {
		if err != nil {
			t.Fatal(err)
		}
	}
	t.Run("a span ending during the pause is skipped", func(t *testing.T) {
		if duringPause != ProductionSpanPaused {
			t.Fatalf("owing = %v, want %v", duringPause, ProductionSpanPaused)
		}
	})
	t.Run("the first landing after the resume closes a span begun in the pause, skipped", func(t *testing.T) {
		if atFirst != ProductionSpanBeganInPause {
			t.Fatalf("owing = %v, want %v", atFirst, ProductionSpanBeganInPause)
		}
	})
	t.Run("the next span is owed from the first landing after the resume", func(t *testing.T) {
		if atSecond != ProductionSpanOwed || !check.Since.Equal(firstAfter) || !check.Until.Equal(secondAfter) {
			t.Fatalf("owing %v, span %+v; want owed %v through %v", atSecond, check, firstAfter, secondAfter)
		}
	})
	t.Run("no skipped span was ever owed", func(t *testing.T) {
		if _, err := s.MarkProductionChecksDue(ctx, repo, secondAfter.Add(time.Minute)); err != nil {
			t.Fatal(err)
		}
		c, err := s.ProductionCheckOf(ctx, repo)
		if err != nil || c == nil || !c.Until.Equal(secondAfter) {
			t.Fatalf("ProductionCheckOf = (%+v, %v), want only the span owed after the resume", c, err)
		}
	})
}

func TestASpanStartsAfterEveryCheckAPauseDropped(t *testing.T) {
	// Arrange: a span owed through ONE, dropped by a pause, then resumed with
	// nothing landing between.
	s, _ := testStore(t)
	ctx := context.Background()
	repo, key := productionRepo(t)
	readThrough(t, s, key, instant)
	one, resume, landing := instant.Add(time.Hour), instant.Add(2*time.Hour), instant.Add(3*time.Hour)
	if _, o, err := s.OweProductionCheck(ctx, repo, one, ""); err != nil || o != ProductionSpanOwed {
		t.Fatalf("OweProductionCheck = (%v, %v)", o, err)
	}
	pausedAt(t, s, repo, one.Add(time.Minute))
	if err := s.ResumeProduction(ctx, repo, resume); err != nil {
		t.Fatal(err)
	}

	// Act
	check, owing, err := s.OweProductionCheck(ctx, repo, landing, "")

	// Assert: the span starts where the dropped one ended, which is before
	// the resume, so it is skipped rather than reaching back over INSTANT.
	if err != nil || owing != ProductionSpanBeganInPause || !check.Since.Equal(one) {
		t.Fatalf("OweProductionCheck = (%+v, %v, %v), want a span from %v skipped", check, owing, err, one)
	}
}
