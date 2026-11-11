package workspace

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// placedSpec is a standard create asked for by workspace REQUESTER and filed
// into the existing subsection "later".
func placedSpec(t *testing.T, f *fixture) CreateSpec {
	t.Helper()
	spec := standardSpec(t, f)
	spec.Requester = requesterIn(t, f, spec.RepoDir)
	spec.Placement = &CreatePlacement{Subsection: "later"}
	return spec
}

// requesterIn registers the workspace that files a create, in REPODIR, and
// answers its id.
func requesterIn(t *testing.T, f *fixture, repoDir string) *ids.WorkspaceID {
	t.Helper()
	ws := f.workspaceInRepo(t, "intake", repoDir)
	return &ws.ID
}

// registeredFactsOf answers the facts the create of workspace ID registered
// its row with, nil when none was.
func registeredFactsOf(f *fixture, id ids.WorkspaceID) *wsm.RegisterFacts {
	for i := range f.db.registered {
		if f.db.registered[i].CreationJob == id {
			return &f.db.registered[i]
		}
	}
	return nil
}

// refusalOf unwraps err as the typed refusal it must be.
func refusalOf(t *testing.T, err error) *Refusal {
	t.Helper()
	var refusal *Refusal
	if !errors.As(err, &refusal) {
		t.Fatalf("err = %v, want a typed refusal", err)
	}
	return refusal
}

func TestCreateWithAPlacementRegistersTheRowPlacedAndAddsNoFilingNote(t *testing.T) {
	// Arrange
	f := newFixture(t)
	placementBrief(f)
	spec := placedSpec(t, f)
	withSubsection(f)

	// Act
	created, err := f.verbs.Create(context.Background(), spec)

	// Assert
	if err != nil {
		t.Fatalf("Create: %v", err)
	}
	facts := registeredFactsOf(f, created.ID)
	if facts == nil || facts.Placement == nil || facts.Placement.Into != "later" || facts.Placement.CreatedBy != *spec.Requester {
		t.Fatalf("registered facts = %+v, want the placement into %q created by the requester", facts, "later")
	}
	if got := sentText(f); got != "fix the login bug" {
		t.Fatalf("the first message = %q, want the prompt with no filing note: the creator filed the workspace", got)
	}
	if created.Subsection == nil {
		t.Fatalf("the created record has no subsection, want it born placed")
	}
}

func TestCreateWithANewSubsectionRegistersItsGatherAndCreator(t *testing.T) {
	// Arrange
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.Requester = requesterIn(t, f, spec.RepoDir)
	spec.Placement = &CreatePlacement{NewSubsection: &wsm.NewSubsectionPlacement{Name: "auth work", Gather: []string{"a", "b"}}}

	// Act
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert
	if err != nil {
		t.Fatalf("Create: %v", err)
	}
	got := f.db.registered[len(f.db.registered)-1].Placement
	if got == nil || got.New == nil || got.New.Name != "auth work" || len(got.New.Gather) != 2 || got.CreatedBy != *spec.Requester {
		t.Fatalf("registered placement = %+v, want the new subsection with its gather created by the requester", got)
	}
}

func TestACreateWithoutAPlacementStillFilesItself(t *testing.T) {
	tests := []struct {
		name      string
		placement *CreatePlacement
		want      bool
	}{
		{name: "no placement", placement: nil, want: true},
		{name: "a placement", placement: &CreatePlacement{Subsection: "x"}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			spec := CreateSpec{Placement: tt.placement}

			// Act
			got := spec.filesItself()

			// Assert
			if got != tt.want {
				t.Fatalf("filesItself = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCreateBornHiddenRegistersTheRowHidden(t *testing.T) {
	tests := []struct {
		name       string
		hidden     bool
		wantHidden bool
	}{
		{name: "hidden", hidden: true, wantHidden: true},
		{name: "visible", hidden: false, wantHidden: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			spec := standardSpec(t, f)
			spec.Hidden = tt.hidden

			// Act
			created, err := f.verbs.Create(context.Background(), spec)

			// Assert
			if err != nil {
				t.Fatalf("Create: %v", err)
			}
			if got := f.db.registered[len(f.db.registered)-1].Hidden; got != tt.wantHidden {
				t.Fatalf("registered Hidden = %v, want %v", got, tt.wantHidden)
			}
			if created.Hidden != tt.wantHidden {
				t.Fatalf("created.Hidden = %v, want %v", created.Hidden, tt.wantHidden)
			}
		})
	}
}

func TestCreateRefusesAPlacementItsShapeCannotCarry(t *testing.T) {
	owner := ids.WorkspaceID("w-owner")
	tests := []struct {
		name    string
		arrange func(spec *CreateSpec)
		wantArm string
	}{
		{name: "asked by the user", arrange: func(spec *CreateSpec) { spec.Requester = nil }, wantArm: ArmPlacementRequiresWorkspace},
		{name: "a child", arrange: func(spec *CreateSpec) { spec.Parent = &owner }, wantArm: ArmPlacementOnChild},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			spec := placedSpec(t, f)
			tt.arrange(&spec)

			// Act
			_, err := f.verbs.Create(context.Background(), spec)

			// Assert
			if got := refusalOf(t, err); got.Arm != tt.wantArm {
				t.Fatalf("refusal arm = %q, want %q", got.Arm, tt.wantArm)
			}
			if len(f.git.created) != 0 || len(f.db.registered) != 0 {
				t.Fatalf("created %+v and registered %d, want nothing made", f.git.created, len(f.db.registered))
			}
			if len(operationRecords(f, dlog.LevelInfo, opRefusal)) != 1 {
				t.Fatalf("want one INFO %s record, got %+v", opRefusal, f.log.logger.Records())
			}
		})
	}
}

func TestValidatePlacementRefusesAMergeWorkspace(t *testing.T) {
	// Arrange
	f := newFixture(t)
	spec := placedSpec(t, f)
	spec.MergeOf = &wsm.MergeOrigin{}

	// Act
	err := validatePlacement(f.log.logger, spec)

	// Assert
	if got := refusalOf(t, err); got.Arm != ArmPlacementOnMergeWorkspace {
		t.Fatalf("refusal arm = %q, want %q", got.Arm, ArmPlacementOnMergeWorkspace)
	}
}

func TestValidatePlacementPanicsOnAPlacementNamingBothOrNeither(t *testing.T) {
	tests := []struct {
		name      string
		placement CreatePlacement
	}{
		{name: "neither", placement: CreatePlacement{}},
		{name: "both", placement: CreatePlacement{Subsection: "a", NewSubsection: &wsm.NewSubsectionPlacement{Name: "b"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			spec := standardSpec(t, f)
			spec.Requester = requesterIn(t, f, spec.RepoDir)
			spec.Placement = &tt.placement
			defer func() {
				// Assert
				if recover() == nil {
					t.Fatal("validatePlacement did not panic")
				}
			}()

			// Act
			_ = validatePlacement(f.log.logger, spec)
		})
	}
}

func TestCreateAnswersTheRegistrysPlacementRefusalOnItsArmBeforeAnythingIsMade(t *testing.T) {
	tests := []struct {
		name       string
		err        error
		wantArm    string
		wantFields map[string]string
	}{
		{name: "an unknown subsection", err: &wsm.UnknownPlacementSubsectionError{Name: "nowhere"}, wantArm: ArmUnknownPlacementSubsection, wantFields: map[string]string{"name": "nowhere"}},
		{name: "a blank new name", err: wsm.ErrBlankSubsectionName, wantArm: ArmBlankPlacementSubsectionName},
		{name: "an unknown gathered workspace", err: &wsm.GatherUnknownError{Name: "ghost"}, wantArm: ArmGatherUnknownWorkspace, wantFields: map[string]string{"name": "ghost"}},
		{name: "a gathered workspace that cannot be gathered", err: &wsm.GatherRefusedError{Workspace: "w-x", Name: "x", Why: "it is closed"}, wantArm: ArmGatherNotGatherable},
		{name: "a child", err: wsm.ErrPlacementOnChild, wantArm: ArmPlacementOnChild},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			spec := placedSpec(t, f)
			f.db.refusePlacementErr = tt.err

			// Act
			_, err := f.verbs.Create(context.Background(), spec)

			// Assert
			refusal := refusalOf(t, err)
			if refusal.Arm != tt.wantArm {
				t.Fatalf("refusal arm = %q, want %q", refusal.Arm, tt.wantArm)
			}
			for field, want := range tt.wantFields {
				if got := refusal.Fields[field]; got != want {
					t.Fatalf("refusal field %q = %v, want %q", field, got, want)
				}
			}
			if len(f.git.created) != 0 || len(f.db.registered) != 0 {
				t.Fatalf("created %+v and registered %d, want the refusal to come before anything is made", f.git.created, len(f.db.registered))
			}
			if len(operationRecords(f, dlog.LevelInfo, opRefusal)) != 1 {
				t.Fatalf("want one INFO %s record, got %+v", opRefusal, f.log.logger.Records())
			}
		})
	}
}

func TestCreateFailsOnAPlacementCheckThatCannotRead(t *testing.T) {
	// Arrange
	f := newFixture(t)
	spec := placedSpec(t, f)
	f.db.refusePlacementErr = errors.New("disk I/O error")

	// Act
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert
	if err == nil || errors.As(err, new(*Refusal)) {
		t.Fatalf("Create = %v, want a failure that is no typed refusal", err)
	}
	if len(f.git.created) != 0 {
		t.Fatalf("created %+v, want nothing made", f.git.created)
	}
	if !hasRecord(f, dlog.LevelError, opCreate) {
		t.Fatalf("want an ERROR %s record naming the failed read, got %+v", opCreate, f.log.logger.Records())
	}
}

func TestCreateUndoesItselfWhenTheRegistryRefusesThePlacementAtRegister(t *testing.T) {
	// Arrange: the registry accepts the check, then the register transaction refuses.
	f := newFixture(t)
	spec := placedSpec(t, f)
	f.db.registerErr = &wsm.GatherRefusedError{Workspace: "w-x", Name: "x", Why: "it is already in a subsection"}

	// Act
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert
	if got := refusalOf(t, err); got.Arm != ArmGatherNotGatherable {
		t.Fatalf("refusal arm = %q, want %q", got.Arm, ArmGatherNotGatherable)
	}
	if len(f.git.nuked) != 1 {
		t.Fatalf("nuked %+v, want the refused create's worktree undone", f.git.nuked)
	}
	if len(operationRecords(f, dlog.LevelInfo, opRefusal)) != 1 {
		t.Fatalf("want one INFO %s record, got %+v", opRefusal, f.log.logger.Records())
	}
}
