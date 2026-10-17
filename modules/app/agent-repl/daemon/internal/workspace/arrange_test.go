package workspace

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func subsectionID(id string) *ids.SubsectionID {
	sub := ids.SubsectionID(id)
	return &sub
}

func TestTheArrangementVerbsWriteAndRepublish(t *testing.T) {
	tests := []struct {
		name string
		op   string
		act  func(context.Context, Verbs) error
		want string
	}{
		{name: "create a subsection", op: opCreateSubsection, want: "CreateSubsection repo-1 later",
			act: func(ctx context.Context, v Verbs) error { return v.CreateSubsection(ctx, "repo-1", "later") }},
		{name: "rename a subsection", op: opRenameSubsection, want: "RenameSubsection sub-1 sooner",
			act: func(ctx context.Context, v Verbs) error { return v.RenameSubsection(ctx, "sub-1", "sooner") }},
		{name: "delete a subsection", op: opDeleteSubsection, want: "DeleteSubsection sub-1",
			act: func(ctx context.Context, v Verbs) error { return v.DeleteSubsection(ctx, "sub-1") }},
		{name: "move a subsection", op: opMoveSubsection, want: "MoveSubsection sub-1 sub-2",
			act: func(ctx context.Context, v Verbs) error { return v.MoveSubsection(ctx, "sub-1", subsectionID("sub-2")) }},
		{name: "move a repository", op: opMoveRepository, want: "MoveRepository repo-1 ",
			act: func(ctx context.Context, v Verbs) error { return v.MoveRepository(ctx, "repo-1", nil) }},
		{name: "place a workspace", op: opPlaceWorkspace, want: "PlaceWorkspace w1 sub-1 w2",
			act: func(ctx context.Context, v Verbs) error {
				before := ids.WorkspaceID("w2")
				return v.PlaceWorkspace(ctx, "w1", subsectionID("sub-1"), &before)
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.workspace("w2", t.TempDir())

			// Act
			err := tt.act(context.Background(), f.verbs)

			// Assert
			if err != nil {
				t.Fatalf("%s: %v", tt.name, err)
			}
			if want := []string{tt.want}; !slices.Equal(f.db.arrangements, want) {
				t.Fatalf("writes = %v, want %v", f.db.arrangements, want)
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
			}
			if !hasRecord(f, dlog.LevelInfo, tt.op) {
				t.Fatalf("the change was not recorded at info under %s", tt.op)
			}
		})
	}
}

func TestTheArrangementVerbsAnswerTheStoresRefusalsOnTheirArms(t *testing.T) {
	tests := []struct {
		name string
		err  error
		arm  string
	}{
		{name: "an unknown subsection", err: &wsm.UnknownSubsectionError{ID: "sub-1"}, arm: ArmUnknownSubsection},
		{name: "an unknown repository", err: fmt.Errorf("x: %w", wsm.ErrUnknownRepository), arm: ArmUnknownRepository},
		{name: "an unknown workspace", err: fmt.Errorf("x: %w", wsm.ErrNotFound), arm: ArmUnknownWorkspace},
		{name: "a blank name", err: wsm.ErrBlankSubsectionName, arm: ArmBlankName},
		{name: "two repositories", err: wsm.ErrOtherRepository, arm: ArmOtherRepository},
		{name: "an anchor elsewhere", err: wsm.ErrAnchorElsewhere, arm: ArmAnchorElsewhere},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.arrangeErr = tt.err

			// Act
			err := f.verbs.PlaceWorkspace(context.Background(), "w1", subsectionID("sub-1"), nil)

			// Assert
			refusal := asRefusal(t, err, tt.arm)
			if refusal.Rpc != "ArrangeSidebar" {
				t.Fatalf("refusal rpc = %q, want ArrangeSidebar", refusal.Rpc)
			}
			if len(f.sidebar.registries) != 0 {
				t.Fatalf("a refused arrangement republished %d times", len(f.sidebar.registries))
			}
			if loggedAtError(f, opPlaceWorkspace) {
				t.Fatal("an ordinary refusal was logged at error")
			}
		})
	}
}

func TestAnUnknownSubsectionRefusalNamesTheSubsection(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.arrangeErr = &wsm.UnknownSubsectionError{ID: "sub-gone"}

	// Act
	err := f.verbs.DeleteSubsection(context.Background(), "sub-gone")

	// Assert
	refusal := asRefusal(t, err, ArmUnknownSubsection)
	ref, ok := refusal.Fields["subsection"].(*agentreplv1.SubsectionRef)
	if !ok || ref.GetId() != "sub-gone" {
		t.Fatalf("refusal fields = %v, want the subsection ref sub-gone", refusal.Fields)
	}
}

func TestTheArrangementVerbsSurfaceAStoreFailure(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.arrangeErr = errors.New("disk on fire")

	// Act
	err := f.verbs.CreateSubsection(context.Background(), "repo-1", "later")

	// Assert
	if !errors.Is(err, f.db.arrangeErr) {
		t.Fatalf("error = %v, want the store's failure", err)
	}
	if _, ok := AsRefusal(err); ok {
		t.Fatal("a store failure was answered as a refusal")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("a failed arrangement republished %d times", len(f.sidebar.registries))
	}
	rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opCreateSubsection})
	if !ok {
		t.Fatal("the store failure was not logged at error")
	}
	if rec.Context["repo_id"] != "repo-1" || rec.Context["cause"] != "disk on fire" {
		t.Fatalf("the failure's record context = %v", rec.Context)
	}
}

func TestPlaceWorkspaceRefusesAWorkspaceThisDaemonDoesNotServe(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.owner.standing = StandingTransferringAway

	// Act
	err := f.verbs.PlaceWorkspace(context.Background(), "w1", nil, nil)

	// Assert
	asRefusal(t, err, ArmTransferringAway)
	if len(f.db.arrangements) != 0 {
		t.Fatalf("a refused placement wrote %v", f.db.arrangements)
	}
}

func TestPlaceWorkspaceRefusesAnUnknownAnchor(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	absent := ids.WorkspaceID("absent")

	// Act
	err := f.verbs.PlaceWorkspace(context.Background(), "w1", nil, &absent)

	// Assert
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.db.arrangements) != 0 {
		t.Fatalf("a refused placement wrote %v", f.db.arrangements)
	}
}

func TestTheRepublishedRosterCarriesTheSubsections(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.subsections = []wsm.Subsection{{ID: "sub-1", Repo: "repo-1", Name: "later", Place: 1}}

	// Act
	if err := f.verbs.RenameSubsection(context.Background(), "sub-1", "sooner"); err != nil {
		t.Fatalf("RenameSubsection: %v", err)
	}

	// Assert
	if got := f.sidebar.registries[0].Subsections; !slices.Equal(got, f.db.subsections) {
		t.Fatalf("republished subsections = %v, want %v", got, f.db.subsections)
	}
}
