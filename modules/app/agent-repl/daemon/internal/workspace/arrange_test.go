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
			act: func(ctx context.Context, v Verbs) error {
				_, err := v.CreateSubsection(ctx, "repo-1", "later")
				return err
			}},
		{name: "rename a subsection", op: opRenameSubsection, want: "RenameSubsection sub-1 sooner",
			act: func(ctx context.Context, v Verbs) error { return v.RenameSubsection(ctx, "sub-1", "sooner") }},
		{name: "delete a subsection", op: opDeleteSubsection, want: "DeleteSubsection sub-1",
			act: func(ctx context.Context, v Verbs) error { return v.DeleteSubsection(ctx, "sub-1") }},
		{name: "move a subsection", op: opMoveSubsection, want: "MoveSubsection sub-1 before subsection sub-2",
			act: func(ctx context.Context, v Verbs) error {
				return v.MoveSubsection(ctx, "sub-1", wsm.BeforeSubsection{ID: "sub-2"})
			}},
		{name: "move a subsection before a workspace", op: opMoveSubsection, want: "MoveSubsection sub-1 before workspace w2",
			act: func(ctx context.Context, v Verbs) error {
				return v.MoveSubsection(ctx, "sub-1", wsm.BeforeWorkspace{ID: "w2"})
			}},
		{name: "move a repository", op: opMoveRepository, want: "MoveRepository repo-1 ",
			act: func(ctx context.Context, v Verbs) error { return v.MoveRepository(ctx, "repo-1", nil) }},
		{name: "place workspaces", op: opPlaceWorkspace, want: "PlaceWorkspaces [w1] sub-1 before workspace w2",
			act: func(ctx context.Context, v Verbs) error {
				return v.PlaceWorkspaces(ctx, []ids.WorkspaceID{"w1"}, subsectionID("sub-1"), wsm.BeforeWorkspace{ID: "w2"})
			}},
		{name: "place workspaces before a subsection", op: opPlaceWorkspace, want: "PlaceWorkspaces [w1]  before subsection sub-1",
			act: func(ctx context.Context, v Verbs) error {
				return v.PlaceWorkspaces(ctx, []ids.WorkspaceID{"w1"}, nil, wsm.BeforeSubsection{ID: "sub-1"})
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
		{name: "a child placed alone", err: wsm.ErrHasParent, arm: ArmHasParent},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.arrangeErr = tt.err

			// Act
			err := f.verbs.PlaceWorkspaces(context.Background(), []ids.WorkspaceID{"w1"}, subsectionID("sub-1"), wsm.AtEnd{})

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
	_, err := f.verbs.CreateSubsection(context.Background(), "repo-1", "later")

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

func TestPlaceWorkspacesRefusesAWorkspaceThisDaemonDoesNotServe(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.owner.standing = StandingTransferringAway

	// Act
	err := f.verbs.PlaceWorkspaces(context.Background(), []ids.WorkspaceID{"w1"}, nil, wsm.AtEnd{})

	// Assert
	asRefusal(t, err, ArmTransferringAway)
	if len(f.db.arrangements) != 0 {
		t.Fatalf("a refused placement wrote %v", f.db.arrangements)
	}
}

func TestPlaceWorkspacesRefusesAnUnknownAnchor(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	absent := ids.WorkspaceID("absent")

	// Act
	err := f.verbs.PlaceWorkspaces(context.Background(), []ids.WorkspaceID{"w1"}, nil, wsm.BeforeWorkspace{ID: absent})

	// Assert
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.db.arrangements) != 0 {
		t.Fatalf("a refused placement wrote %v", f.db.arrangements)
	}
}

func TestMoveSubsectionRefusesAnUnknownWorkspaceAnchor(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	err := f.verbs.MoveSubsection(context.Background(), "sub-1", wsm.BeforeWorkspace{ID: "absent"})

	// Assert
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.db.arrangements) != 0 {
		t.Fatalf("a refused move wrote %v", f.db.arrangements)
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

func TestCreateSubsectionAnswersTheSubsectionCreated(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	sub, err := f.verbs.CreateSubsection(context.Background(), "repo-1", "later")

	// Assert
	if err != nil {
		t.Fatalf("CreateSubsection: %v", err)
	}
	if sub.ID != "sub-new" || sub.Name != "later" {
		t.Fatalf("created = %+v, want sub-new named later", sub)
	}
}

func TestPlaceWorkspacesRefusesAnyWorkspaceThisDaemonDoesNotKnow(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	err := f.verbs.PlaceWorkspaces(context.Background(), []ids.WorkspaceID{"w1", "absent"}, nil, wsm.AtEnd{})

	// Assert
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.db.arrangements) != 0 {
		t.Fatalf("a refused placement wrote %v", f.db.arrangements)
	}
}

func TestSidebarArrangementReadsTheRegistrysArrangement(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	got, err := f.verbs.SidebarArrangement(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("SidebarArrangement: %v", err)
	}
	if len(got) != 1 || len(got[0].Entries) != 1 || got[0].Entries[0].Workspace.Workspace.ID != "w1" {
		t.Fatalf("arrangement = %+v, want the one workspace in its repository", got)
	}
}

func TestSidebarArrangementSurfacesAnUnreadableRegistry(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.listWorkspacesErr = errors.New("disk on fire")

	// Act
	_, err := f.verbs.SidebarArrangement(context.Background())

	// Assert
	if err == nil {
		t.Fatal("SidebarArrangement read an unreadable registry")
	}
	if !loggedAtError(f, opSidebarArrangement) {
		t.Fatal("the failed read was not logged at error")
	}
}
