package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

// sampleGather is a gather by CALLER on TARGETS, made at instant.
func sampleGather(caller WorkspaceID, key string, targets ...Workspace) Gather {
	g := Gather{ID: NewGatherID(), Caller: caller, CallerName: "caller", CallerDir: "/c", Key: key, PromptText: "status?",
		CreatedAt: instant, DeadlineAt: instant.Add(5 * time.Minute)}
	for _, t := range targets {
		g.Targets = append(g.Targets, GatherTarget{Workspace: t.ID, Name: t.Name, Dir: t.Dir, Turn: NewTurnID()})
	}
	return g
}

func TestPutGatherRefusesAnInconsistentGather(t *testing.T) {
	tests := []struct {
		name   string
		break_ func(*Gather)
	}{
		{"no key", func(g *Gather) { g.Key = "" }},
		{"no target", func(g *Gather) { g.Targets = nil }},
		{"a deadline not after its creation", func(g *Gather) { g.DeadlineAt = g.CreatedAt }},
		{"a first update already told", func(g *Gather) { g.FirstTold = &GatherFirstTold{At: instant, Trigger: GatherTimedOut} }},
		{"a target put resolved", func(g *Gather) { g.Targets[0].Resolution = &GatherResolution{Kind: GatherClosed, At: instant} }},
		{"a target with no turn", func(g *Gather) { g.Targets[0].Turn = "" }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			caller := testWorkspace(t, s)
			target := testWorkspaceNamed(t, s, "target")
			g := sampleGather(caller.ID, "k", target)
			tc.break_(&g)

			// Act
			err := s.PutGather(context.Background(), g)

			// Assert
			if err == nil {
				t.Fatal("PutGather recorded an inconsistent gather")
			}
			if !loggedOperation(log, "daemon.wsm.put_gather", "error") {
				t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
			}
			if n := scalar[int](t, s, `SELECT COUNT(*) FROM gathers`); n != 0 {
				t.Fatalf("%d gathers written by a refused put", n)
			}
		})
	}
}

func TestPutGatherThenGatherReadsItBack(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	caller := testWorkspace(t, s)
	a, b := testWorkspaceNamed(t, s, "a"), testWorkspaceNamed(t, s, "b")
	g := sampleGather(caller.ID, "k", a, b)
	ctx := context.Background()

	// Act
	if err := s.PutGather(ctx, g); err != nil {
		t.Fatalf("PutGather: %v", err)
	}
	got, err := s.Gather(ctx, g.ID)

	// Assert
	if err != nil {
		t.Fatalf("Gather: %v", err)
	}
	if got.Key != "k" || got.PromptText != "status?" || !got.DeadlineAt.Equal(g.DeadlineAt) || got.FirstTold != nil {
		t.Fatalf("Gather = %+v, want it as put", got)
	}
	if len(got.Targets) != 2 || got.Targets[0].Name != "a" || got.Targets[1].Name != "b" || got.Targets[0].Turn != g.Targets[0].Turn {
		t.Fatalf("targets = %+v, want both in the order named", got.Targets)
	}
}

func TestPutGatherRefusesAKeyAnotherGatherHolds(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	caller := testWorkspace(t, s)
	target := testWorkspaceNamed(t, s, "target")
	ctx := context.Background()
	if err := s.PutGather(ctx, sampleGather(caller.ID, "k", target)); err != nil {
		t.Fatalf("first PutGather: %v", err)
	}

	// Act
	err := s.PutGather(ctx, sampleGather(caller.ID, "k", target))

	// Assert
	if !errors.Is(err, ErrDuplicateGather) {
		t.Fatalf("PutGather = %v, want ErrDuplicateGather", err)
	}
}

func TestGatherOfAnUnknownIDIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.Gather(context.Background(), "nope")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("Gather = %v, want ErrNotFound", err)
	}
}

func TestGatherByKey(t *testing.T) {
	tests := []struct {
		name      string
		key       string
		wantFound bool
	}{
		{"the key a gather holds", "k", true},
		{"a key no gather holds", "other", false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			caller := testWorkspace(t, s)
			g := sampleGather(caller.ID, "k", testWorkspaceNamed(t, s, "target"))
			if err := s.PutGather(context.Background(), g); err != nil {
				t.Fatalf("PutGather: %v", err)
			}

			// Act
			got, found, err := s.GatherByKey(context.Background(), tc.key)

			// Assert
			if err != nil || found != tc.wantFound || (found && got.ID != g.ID) {
				t.Fatalf("GatherByKey = %+v, %v, %v; want found=%v", got, found, err, tc.wantFound)
			}
		})
	}
}

func TestResolveGatherTargetResolvesOnce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	caller := testWorkspace(t, s)
	target := testWorkspaceNamed(t, s, "target")
	g := sampleGather(caller.ID, "k", target)
	ctx := context.Background()
	if err := s.PutGather(ctx, g); err != nil {
		t.Fatalf("PutGather: %v", err)
	}

	// Act
	first, err1 := s.ResolveGatherTarget(ctx, g.ID, target.ID, GatherResolution{Kind: GatherAnswered, At: instant, Text: "done"})
	second, err2 := s.ResolveGatherTarget(ctx, g.ID, target.ID, GatherResolution{Kind: GatherClosed, At: instant})

	// Assert
	if err1 != nil || err2 != nil || !first || second {
		t.Fatalf("ResolveGatherTarget = (%v, %v), (%v, %v); want only the first to resolve", first, err1, second, err2)
	}
	got, _ := s.Gather(ctx, g.ID)
	if r := got.Targets[0].Resolution; r == nil || r.Kind != GatherAnswered || r.Text != "done" {
		t.Fatalf("resolution = %+v, want the first answer kept", r)
	}
}

func TestResolveGatherTargetRefusesAnInconsistentResolution(t *testing.T) {
	tests := []struct {
		name string
		r    GatherResolution
	}{
		{"an answer with no text", GatherResolution{Kind: GatherAnswered, At: instant}},
		{"a close with text", GatherResolution{Kind: GatherClosed, At: instant, Text: "x"}},
		{"an undeclared kind", GatherResolution{Kind: "vanished", At: instant}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			caller := testWorkspace(t, s)
			target := testWorkspaceNamed(t, s, "target")
			g := sampleGather(caller.ID, "k", target)
			if err := s.PutGather(context.Background(), g); err != nil {
				t.Fatalf("PutGather: %v", err)
			}

			// Act
			resolved, err := s.ResolveGatherTarget(context.Background(), g.ID, target.ID, tc.r)

			// Assert
			if err == nil || resolved {
				t.Fatalf("ResolveGatherTarget = %v, %v; want it refused", resolved, err)
			}
			if !loggedOperation(log, "daemon.wsm.resolve_gather_target", "error") {
				t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
			}
			if got, _ := s.Gather(context.Background(), g.ID); !got.Targets[0].Outstanding() {
				t.Fatal("the refused resolution was written")
			}
		})
	}
}

func TestMarkGatherFirstToldMarksEveryResolutionItCarriedTold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	caller := testWorkspace(t, s)
	a, b := testWorkspaceNamed(t, s, "a"), testWorkspaceNamed(t, s, "b")
	g := sampleGather(caller.ID, "k", a, b)
	ctx := context.Background()
	if err := s.PutGather(ctx, g); err != nil {
		t.Fatalf("PutGather: %v", err)
	}
	if _, err := s.ResolveGatherTarget(ctx, g.ID, a.ID, GatherResolution{Kind: GatherAnswered, At: instant, Text: "x"}); err != nil {
		t.Fatalf("ResolveGatherTarget: %v", err)
	}

	// Act
	err := s.MarkGatherFirstTold(ctx, g.ID, GatherFirstTold{At: instant.Add(time.Minute), Trigger: GatherTimedOut})

	// Assert
	if err != nil {
		t.Fatalf("MarkGatherFirstTold: %v", err)
	}
	got, _ := s.Gather(ctx, g.ID)
	if got.FirstTold == nil || got.FirstTold.Trigger != GatherTimedOut {
		t.Fatalf("FirstTold = %+v, want timed out", got.FirstTold)
	}
	if got.Targets[0].ToldAt == nil || got.Targets[1].ToldAt != nil {
		t.Fatalf("told = %v, %v; want only the resolved target told", got.Targets[0].ToldAt, got.Targets[1].ToldAt)
	}
}

func TestMarkGatherFirstToldKeepsTheFirstTelling(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	caller := testWorkspace(t, s)
	g := sampleGather(caller.ID, "k", testWorkspaceNamed(t, s, "a"))
	ctx := context.Background()
	if err := s.PutGather(ctx, g); err != nil {
		t.Fatalf("PutGather: %v", err)
	}
	if err := s.MarkGatherFirstTold(ctx, g.ID, GatherFirstTold{At: instant, Trigger: GatherAllResolved}); err != nil {
		t.Fatalf("first mark: %v", err)
	}

	// Act
	err := s.MarkGatherFirstTold(ctx, g.ID, GatherFirstTold{At: instant.Add(time.Hour), Trigger: GatherTimedOut})

	// Assert
	got, _ := s.Gather(ctx, g.ID)
	if err != nil || got.FirstTold.Trigger != GatherAllResolved || !got.FirstTold.At.Equal(instant) {
		t.Fatalf("FirstTold = %+v, %v; want the first telling kept", got.FirstTold, err)
	}
}

func TestMarkGatherFirstToldRefusesAnUndeclaredTrigger(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	caller := testWorkspace(t, s)
	g := sampleGather(caller.ID, "k", testWorkspaceNamed(t, s, "a"))
	if err := s.PutGather(context.Background(), g); err != nil {
		t.Fatalf("PutGather: %v", err)
	}

	// Act
	err := s.MarkGatherFirstTold(context.Background(), g.ID, GatherFirstTold{At: instant, Trigger: "whenever"})

	// Assert
	if err == nil {
		t.Fatal("an undeclared trigger was recorded")
	}
	if !loggedOperation(log, "daemon.wsm.mark_gather_first_told", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
}

func TestMarkGatherTargetToldMarksOnlyAResolvedTarget(t *testing.T) {
	tests := []struct {
		name     string
		resolve  bool
		wantTold bool
	}{
		{"a resolved target", true, true},
		{"an outstanding target", false, false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			caller := testWorkspace(t, s)
			target := testWorkspaceNamed(t, s, "a")
			g := sampleGather(caller.ID, "k", target)
			ctx := context.Background()
			if err := s.PutGather(ctx, g); err != nil {
				t.Fatalf("PutGather: %v", err)
			}
			if tc.resolve {
				if _, err := s.ResolveGatherTarget(ctx, g.ID, target.ID, GatherResolution{Kind: GatherClosed, At: instant}); err != nil {
					t.Fatalf("ResolveGatherTarget: %v", err)
				}
			}

			// Act
			err := s.MarkGatherTargetTold(ctx, g.ID, target.ID, instant)

			// Assert
			got, _ := s.Gather(ctx, g.ID)
			if err != nil || (got.Targets[0].ToldAt != nil) != tc.wantTold {
				t.Fatalf("MarkGatherTargetTold = %v, told %v; want told=%v", err, got.Targets[0].ToldAt, tc.wantTold)
			}
		})
	}
}

func TestUnsettledGathersAnswersWhatIsStillOwed(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	caller := testWorkspace(t, s)
	target := testWorkspaceNamed(t, s, "a")
	ctx := context.Background()
	pending := sampleGather(caller.ID, "pending", target)
	owedLater := sampleGather(caller.ID, "owed-later", target)
	settled := sampleGather(caller.ID, "settled", target)
	for _, g := range []Gather{pending, owedLater, settled} {
		if err := s.PutGather(ctx, g); err != nil {
			t.Fatalf("PutGather: %v", err)
		}
	}
	if err := s.MarkGatherFirstTold(ctx, owedLater.ID, GatherFirstTold{At: instant, Trigger: GatherTimedOut}); err != nil {
		t.Fatalf("mark: %v", err)
	}
	if _, err := s.ResolveGatherTarget(ctx, owedLater.ID, target.ID, GatherResolution{Kind: GatherClosed, At: instant}); err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if _, err := s.ResolveGatherTarget(ctx, settled.ID, target.ID, GatherResolution{Kind: GatherClosed, At: instant}); err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if err := s.MarkGatherFirstTold(ctx, settled.ID, GatherFirstTold{At: instant, Trigger: GatherAllResolved}); err != nil {
		t.Fatalf("mark: %v", err)
	}

	// Act
	got, err := s.UnsettledGathers(ctx)

	// Assert
	if err != nil {
		t.Fatalf("UnsettledGathers: %v", err)
	}
	keys := map[string]bool{}
	for _, g := range got {
		keys[g.Key] = true
	}
	if len(got) != 2 || !keys["pending"] || !keys["owed-later"] {
		t.Fatalf("UnsettledGathers = %v, want the pending one and the one owing a later answer", keys)
	}
}

func TestGathersOutstandingOnAnswersOnlyGathersWaitingOnTheWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	caller := testWorkspace(t, s)
	a, b := testWorkspaceNamed(t, s, "a"), testWorkspaceNamed(t, s, "b")
	ctx := context.Background()
	onA := sampleGather(caller.ID, "on-a", a)
	onB := sampleGather(caller.ID, "on-b", b)
	resolvedA := sampleGather(caller.ID, "resolved-a", a)
	for _, g := range []Gather{onA, onB, resolvedA} {
		if err := s.PutGather(ctx, g); err != nil {
			t.Fatalf("PutGather: %v", err)
		}
	}
	if _, err := s.ResolveGatherTarget(ctx, resolvedA.ID, a.ID, GatherResolution{Kind: GatherClosed, At: instant}); err != nil {
		t.Fatalf("resolve: %v", err)
	}

	// Act
	got, err := s.GathersOutstandingOn(ctx, a.ID)

	// Assert
	if err != nil || len(got) != 1 || got[0].ID != onA.ID {
		t.Fatalf("GathersOutstandingOn = %+v, %v; want only the gather still waiting on a", got, err)
	}
}

func TestForgettingTheCallerDropsItsGathers(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	caller := testWorkspace(t, s)
	g := sampleGather(caller.ID, "k", testWorkspaceNamed(t, s, "a"))
	if err := s.PutGather(context.Background(), g); err != nil {
		t.Fatalf("PutGather: %v", err)
	}

	// Act
	corrupt(t, s, `DELETE FROM workspaces WHERE id = ?`, caller.ID)

	// Assert
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM gather_targets`); n != 0 {
		t.Fatalf("%d targets outlived their gather's caller", n)
	}
}

func TestForgettingATargetKeepsItsRowForTheGather(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	caller := testWorkspace(t, s)
	target := testWorkspaceNamed(t, s, "a")
	g := sampleGather(caller.ID, "k", target)
	if err := s.PutGather(context.Background(), g); err != nil {
		t.Fatalf("PutGather: %v", err)
	}

	// Act
	corrupt(t, s, `DELETE FROM workspaces WHERE id = ?`, target.ID)

	// Assert
	got, err := s.Gather(context.Background(), g.ID)
	if err != nil || len(got.Targets) != 1 || !got.Targets[0].Outstanding() {
		t.Fatalf("Gather = %+v, %v; want the forgotten target still outstanding, to resolve closed", got, err)
	}
}

func TestGatherSettledAndAllResolved(t *testing.T) {
	told := instant
	answered := &GatherResolution{Kind: GatherAnswered, At: instant, Text: "x"}
	tests := []struct {
		name                      string
		g                         Gather
		wantSettled, wantResolved bool
	}{
		{"nothing told", Gather{Targets: []GatherTarget{{Resolution: answered}}}, false, true},
		{"first told, a resolution untold", Gather{FirstTold: &GatherFirstTold{}, Targets: []GatherTarget{{Resolution: answered}}}, false, true},
		{"first told, an outstanding target", Gather{FirstTold: &GatherFirstTold{}, Targets: []GatherTarget{{}}}, false, false},
		{"everything told", Gather{FirstTold: &GatherFirstTold{}, Targets: []GatherTarget{{Resolution: answered, ToldAt: &told}}}, true, true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act, Assert
			if tc.g.Settled() != tc.wantSettled || tc.g.AllResolved() != tc.wantResolved {
				t.Fatalf("Settled, AllResolved = %v, %v; want %v, %v", tc.g.Settled(), tc.g.AllResolved(), tc.wantSettled, tc.wantResolved)
			}
		})
	}
}
