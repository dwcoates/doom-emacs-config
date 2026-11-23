package wsm

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"
)

// productionRepo is a repository key for the production check tests, and
// its normalized form.
func productionRepo(t *testing.T) (RepoKey, RepoKey) {
	t.Helper()
	repo := RepoKey(t.TempDir())
	key, err := normalizeRepoKey(repo)
	if err != nil {
		t.Fatal(err)
	}
	return repo, key
}

// readThrough records that REPO's last deploy check read through AT.
func readThrough(t *testing.T, s *store, key RepoKey, at time.Time) {
	t.Helper()
	if _, err := s.db().ExecContext(context.Background(),
		`INSERT INTO merge_queue_repos (repo_key, paused, checked_through) VALUES (?, 0, ?)
		 ON CONFLICT(repo_key) DO UPDATE SET checked_through = excluded.checked_through`, string(key), nanos(at)); err != nil {
		t.Fatal(err)
	}
}

// toldCheck owes, makes due and tells the span ending UNTIL.
func toldCheck(t *testing.T, s *store, repo RepoKey, until time.Time) {
	t.Helper()
	ctx := context.Background()
	if _, owed, err := s.OweProductionCheck(ctx, repo, until, ""); err != nil || !owed {
		t.Fatalf("OweProductionCheck = (%v, %v)", owed, err)
	}
	if _, err := s.MarkProductionChecksDue(ctx, repo, until.Add(time.Minute)); err != nil {
		t.Fatal(err)
	}
	if told, err := s.MarkProductionCheckPrompted(ctx, repo, until, until.Add(2*time.Minute), "turn-1"); err != nil || !told {
		t.Fatalf("MarkProductionCheckPrompted = (%v, %v)", told, err)
	}
}

func TestAProductionSpanRunsFromTheLastReadToTheLanding(t *testing.T) {
	cases := []struct {
		name string
		// read is how long before the landing the last deploy check read
		// to; zero for a repository never checked.
		read time.Duration
		// lastSpan is how long before the landing the previous production
		// span ended; zero for none.
		lastSpan time.Duration
		// wantSince is how long before the landing the span starts; zero
		// for no span owed.
		wantSince time.Duration
	}{
		{name: "a span starts where the last deploy check read to", read: time.Hour, wantSince: time.Hour},
		{name: "a span starts where the previous span ended when that is later", read: 2 * time.Hour, lastSpan: 30 * time.Minute, wantSince: 30 * time.Minute},
		{name: "a repository never checked owes no span"},
		{name: "a landing that began before the last read owes no span", read: -time.Minute},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo, key := productionRepo(t)
			landing := instant.Add(3 * time.Hour)
			if tc.read != 0 {
				readThrough(t, s, key, landing.Add(-tc.read))
			}
			if tc.lastSpan != 0 {
				if _, owed, err := s.OweProductionCheck(ctx, repo, landing.Add(-tc.lastSpan), ""); err != nil || !owed {
					t.Fatalf("the previous span = (%v, %v)", owed, err)
				}
			}

			// Act
			check, owed, err := s.OweProductionCheck(ctx, repo, landing, "0.2.0")

			// Assert
			if err != nil || owed != (tc.wantSince != 0) {
				t.Fatalf("OweProductionCheck = (%+v, %v, %v), want owed %v", check, owed, err, tc.wantSince != 0)
			}
			if owed && (!check.Since.Equal(landing.Add(-tc.wantSince)) || !check.Until.Equal(landing) || check.Version != "0.2.0") {
				t.Fatalf("the span = %+v, want %v through %v at 0.2.0", check, landing.Add(-tc.wantSince), landing)
			}
		})
	}
}

func TestAProductionCheckIsToldOnlyOnceDue(t *testing.T) {
	// Arrange: two spans owed, neither due.
	s, _ := testStore(t)
	ctx := context.Background()
	repo, key := productionRepo(t)
	readThrough(t, s, key, instant)
	first, second := instant.Add(time.Hour), instant.Add(2*time.Hour)
	for _, until := range []time.Time{first, second} {
		if _, owed, err := s.OweProductionCheck(ctx, repo, until, ""); err != nil || !owed {
			t.Fatalf("OweProductionCheck = (%v, %v)", owed, err)
		}
	}
	before, err := s.ProductionCheckOf(ctx, repo)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	n, err := s.MarkProductionChecksDue(ctx, repo, second.Add(time.Minute))
	after, err2 := s.ProductionCheckOf(ctx, repo)

	// Assert: nothing is told before its landing's deploy ends, and then the
	// oldest span first.
	if before != nil {
		t.Fatalf("before any deploy ended the check to decide = %+v, want none", before)
	}
	if err != nil || err2 != nil || n != 2 || after == nil || !after.Until.Equal(first) || after.DueAt == nil {
		t.Fatalf("after = (%+v, %d, %v, %v), want both due and the first to decide", after, n, err, err2)
	}
}

func TestAProductionCheckComparesAgainstTheSpanBefore(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo, key := productionRepo(t)
	readThrough(t, s, key, instant)
	first, second := instant.Add(time.Hour), instant.Add(2*time.Hour)
	for _, until := range []time.Time{first, second} {
		if _, _, err := s.OweProductionCheck(ctx, repo, until, ""); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	before, found, err := s.PreviousProductionSpan(ctx, repo, second)
	none, foundNone, err2 := s.PreviousProductionSpan(ctx, repo, first)

	// Assert
	if err != nil || !found || !before.Since.Equal(instant) || !before.Until.Equal(first) {
		t.Fatalf("the span before the second = (%+v, %v, %v), want the first", before, found, err)
	}
	if err2 != nil || foundNone {
		t.Fatalf("the span before the first = (%+v, %v, %v), want none", none, foundNone, err2)
	}
}

func TestAProductionDecisionRecordsItsChildrenAndTheirSites(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo, key := productionRepo(t)
	readThrough(t, s, key, instant)
	until := instant.Add(time.Hour)
	toldCheck(t, s, repo, until)
	a, b := testWorkspaceNamed(t, s, "child-a"), testWorkspaceNamed(t, s, "child-b")

	// Act
	err := s.DecideProductionCheck(ctx, repo, until, until.Add(time.Hour), ProductionDispatched, "two fixes", []ProductionChild{
		{Workspace: a.ID, Kind: ProductionRemediation, Sites: []string{"aaaaaaaaaaaa", "bbbbbbbbbbbb"}},
		{Workspace: b.ID, Kind: ProductionInvestigation, Sites: []string{"cccccccccccc"}},
	})

	// Assert
	owned, err2 := s.OwnedSites(ctx, repo)
	children, err3 := s.ProductionChildrenOf(ctx, repo)
	next, err4 := s.ProductionCheckOf(ctx, repo)
	if err != nil || err2 != nil || err3 != nil || err4 != nil {
		t.Fatalf("errors = %v, %v, %v, %v", err, err2, err3, err4)
	}
	if owned["aaaaaaaaaaaa"] != a.ID || owned["bbbbbbbbbbbb"] != a.ID || owned["cccccccccccc"] != b.ID || len(owned) != 3 {
		t.Fatalf("owned sites = %v, want a's two and b's one", owned)
	}
	if len(children) != 2 || children[0].Kind != ProductionRemediation || children[1].Kind != ProductionInvestigation || !slices.Equal(children[0].Sites, []string{"aaaaaaaaaaaa", "bbbbbbbbbbbb"}) {
		t.Fatalf("children = %+v, want a remediation and an investigation with their sites", children)
	}
	if next != nil {
		t.Fatalf("the check to decide after the decision = %+v, want none", next)
	}
}

func TestAProductionDecisionIsRefusedWhole(t *testing.T) {
	cases := []struct {
		name string
		// held is a site an open child already owns before the decision.
		held string
		// sites are each dispatched child's sites.
		sites [][]string
		// told is whether the check was told before the decision.
		told bool
		// wantErr checks the refusal.
		wantErr func(error) bool
	}{
		{
			name: "a site an open child owns", held: "aaaaaaaaaaaa", sites: [][]string{{"aaaaaaaaaaaa"}}, told: true,
			wantErr: func(err error) bool { var e *SiteOwnedError; return errors.As(err, &e) && e.Holder != "" },
		},
		{
			name: "a site two children name", sites: [][]string{{"bbbbbbbbbbbb"}, {"bbbbbbbbbbbb"}}, told: true,
			wantErr: func(err error) bool {
				var e *SiteOwnedError
				return errors.As(err, &e) && e.Holder == "" && e.Index == 1
			},
		},
		{
			name: "a check never told", sites: [][]string{{"cccccccccccc"}},
			wantErr: func(err error) bool { return errors.Is(err, ErrNoProductionCheckOwed) },
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo, key := productionRepo(t)
			if tc.held != "" {
				readThrough(t, s, key, instant.Add(-time.Hour))
				toldCheck(t, s, repo, instant)
				holder := testWorkspaceNamed(t, s, "holder")
				if err := s.DecideProductionCheck(ctx, repo, instant, instant, ProductionDispatched, "held", []ProductionChild{{Workspace: holder.ID, Kind: ProductionRemediation, Sites: []string{tc.held}}}); err != nil {
					t.Fatal(err)
				}
			}
			readThrough(t, s, key, instant)
			until := instant.Add(time.Hour)
			if tc.told {
				toldCheck(t, s, repo, until)
			} else if _, _, err := s.OweProductionCheck(ctx, repo, until, ""); err != nil {
				t.Fatal(err)
			}
			var children []ProductionChild
			for i, sites := range tc.sites {
				ws := testWorkspaceNamed(t, s, "child-"+string(rune('a'+i)))
				children = append(children, ProductionChild{Workspace: ws.ID, Kind: ProductionRemediation, Sites: sites})
			}
			ownedBefore, _ := s.OwnedSites(ctx, repo)

			// Act
			err := s.DecideProductionCheck(ctx, repo, until, until, ProductionDispatched, "refused", children)

			// Assert: refused, and nothing of it was recorded.
			ownedAfter, _ := s.OwnedSites(ctx, repo)
			open, _ := s.ProductionChildrenOf(ctx, repo)
			if !tc.wantErr(err) || len(ownedAfter) != len(ownedBefore) || len(open) != len(ownedBefore) {
				t.Fatalf("DecideProductionCheck = %v, owned %v (before %v), open %d; want the refusal and nothing recorded", err, ownedAfter, ownedBefore, len(open))
			}
		})
	}
}

func TestAFoldMovesTheFoldedChildsSitesToTheSurvivor(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo, key := productionRepo(t)
	readThrough(t, s, key, instant)
	until := instant.Add(time.Hour)
	toldCheck(t, s, repo, until)
	a, b := testWorkspaceNamed(t, s, "child-a"), testWorkspaceNamed(t, s, "child-b")
	if err := s.DecideProductionCheck(ctx, repo, until, until, ProductionDispatched, "two", []ProductionChild{
		{Workspace: a.ID, Kind: ProductionRemediation, Sites: []string{"aaaaaaaaaaaa"}},
		{Workspace: b.ID, Kind: ProductionRemediation, Sites: []string{"bbbbbbbbbbbb"}},
	}); err != nil {
		t.Fatal(err)
	}

	// Act
	err := s.FoldProductionChild(ctx, a.ID, b.ID, until.Add(time.Hour))

	// Assert
	owned, _ := s.OwnedSites(ctx, repo)
	folded, _, _ := s.ProductionChildOf(ctx, a.ID)
	if err != nil || owned["aaaaaaaaaaaa"] != b.ID || owned["bbbbbbbbbbbb"] != b.ID || folded.Ending != ProductionChildFolded || folded.Open() {
		t.Fatalf("fold = %v, owned %v, folded child %+v; want both sites the survivor's and the folded child ended", err, owned, folded)
	}
}

func TestAFoldIsRefusedForAnythingButTwoOpenChildren(t *testing.T) {
	cases := []struct {
		name string
		// folded and surviving pick the fold's sides from the open child,
		// a stranger, and an ended child.
		folded, surviving string
	}{
		{name: "a child folded into itself", folded: "open", surviving: "open"},
		{name: "a workspace that is no child", folded: "stranger", surviving: "open"},
		{name: "a survivor that is no child", folded: "open", surviving: "stranger"},
		{name: "a child that already ended", folded: "ended", surviving: "open"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo, key := productionRepo(t)
			readThrough(t, s, key, instant)
			until := instant.Add(time.Hour)
			toldCheck(t, s, repo, until)
			named := map[string]WorkspaceID{
				"open": testWorkspaceNamed(t, s, "open").ID, "stranger": testWorkspaceNamed(t, s, "stranger").ID, "ended": testWorkspaceNamed(t, s, "ended").ID,
			}
			if err := s.DecideProductionCheck(ctx, repo, until, until, ProductionDispatched, "two", []ProductionChild{
				{Workspace: named["open"], Kind: ProductionRemediation, Sites: []string{"aaaaaaaaaaaa"}},
				{Workspace: named["ended"], Kind: ProductionRemediation, Sites: []string{"bbbbbbbbbbbb"}},
			}); err != nil {
				t.Fatal(err)
			}
			if _, err := s.EndProductionChild(ctx, named["ended"], ProductionChildClosed, until); err != nil {
				t.Fatal(err)
			}

			// Act
			err := s.FoldProductionChild(ctx, named[tc.folded], named[tc.surviving], until.Add(time.Hour))

			// Assert
			owned, _ := s.OwnedSites(ctx, repo)
			if !errors.Is(err, ErrNotFoldable) || owned["aaaaaaaaaaaa"] != named["open"] {
				t.Fatalf("fold = %v, owned %v; want ErrNotFoldable and nothing moved", err, owned)
			}
		})
	}
}

func TestAnEndedChildReleasesItsSites(t *testing.T) {
	cases := []struct {
		name      string
		how       ProductionChildEnding
		wantFixed bool
	}{
		{name: "a landed child's sites are recorded fixed", how: ProductionChildLanded, wantFixed: true},
		{name: "a closed child's sites are released unfixed", how: ProductionChildClosed},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo, key := productionRepo(t)
			readThrough(t, s, key, instant)
			until := instant.Add(time.Hour)
			toldCheck(t, s, repo, until)
			child := testWorkspaceNamed(t, s, "child")
			if err := s.DecideProductionCheck(ctx, repo, until, until, ProductionDispatched, "one", []ProductionChild{{Workspace: child.ID, Kind: ProductionRemediation, Sites: []string{"aaaaaaaaaaaa"}}}); err != nil {
				t.Fatal(err)
			}
			at := until.Add(time.Hour)

			// Act
			ended, err := s.EndProductionChild(ctx, child.ID, tc.how, at)
			again, err2 := s.EndProductionChild(ctx, child.ID, tc.how, at)

			// Assert
			owned, _ := s.OwnedSites(ctx, repo)
			fixed, _ := s.FixedSites(ctx, repo)
			gotFixed := len(fixed) == 1 && fixed[0].Site == "aaaaaaaaaaaa" && fixed[0].Workspace == child.ID && fixed[0].LandedAt.Equal(at)
			if err != nil || err2 != nil || !ended || again || len(owned) != 0 || gotFixed != tc.wantFixed || (!tc.wantFixed && len(fixed) != 0) {
				t.Fatalf("ended %v then %v (%v, %v), owned %v, fixed %+v; want it ended once, its sites released, fixed %v", ended, again, err, err2, owned, fixed, tc.wantFixed)
			}
		})
	}
}

func TestAProductionMediatorIsItsWorkspacesRole(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo, _ := productionRepo(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.RecordStandingMediator(ctx, MediatorProduction, repo, ws.ID)
	recorded, found, err2 := s.StandingMediatorOf(ctx, MediatorProduction, repo)
	_, deployFound, err3 := s.StandingMediatorOf(ctx, MediatorDeploy, repo)
	read, err4 := s.Workspace(ctx, ws.ID)

	// Assert
	if err != nil || err2 != nil || err3 != nil || err4 != nil {
		t.Fatalf("errors = %v, %v, %v, %v", err, err2, err3, err4)
	}
	if !found || recorded.Workspace != ws.ID || deployFound || read.Mediator != MediatorProduction {
		t.Fatalf("production mediator = (%+v, %v), deploy found %v, workspace role %q; want only the production role", recorded, found, deployFound, read.Mediator)
	}
}
