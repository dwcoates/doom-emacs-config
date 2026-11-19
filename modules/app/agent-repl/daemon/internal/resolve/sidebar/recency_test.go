package sidebar_test

import (
	"errors"
	"testing"

	"google.golang.org/protobuf/proto"

	"agentrepl/protohelpers/rosterwalk"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
)

func olderGutter(presses uint32) *frontendv1.RosterOpenRowRecencyGutter {
	return &frontendv1.RosterOpenRowRecencyGutter{Reach: &frontendv1.RosterOpenRowRecencyGutter_Older{
		Older: &frontendv1.RosterOpenRowRecencyGutterOlder{Presses: presses}}}
}

func newerGutter(presses uint32) *frontendv1.RosterOpenRowRecencyGutter {
	return &frontendv1.RosterOpenRowRecencyGutter{Reach: &frontendv1.RosterOpenRowRecencyGutter_Newer{
		Newer: &frontendv1.RosterOpenRowRecencyGutterNewer{Presses: presses}}}
}

// gutters answers every open row's gutter in R's latest roster, by workspace,
// rows with none absent.
func gutters(t *testing.T, r sidebar.Resolver) map[ids.WorkspaceID]*frontendv1.RosterOpenRowRecencyGutter {
	t.Helper()
	out := map[ids.WorkspaceID]*frontendv1.RosterOpenRowRecencyGutter{}
	for _, section := range latest(t, r).GetRepository().GetSections() {
		for _, row := range rosterwalk.FlattenOpen(rosterwalk.UnsectionedRows(section)) {
			if g := row.GetRecencyGutter(); g != nil {
				out[ids.WorkspaceID(row.GetRow().GetWorkspace().GetWorkspace().GetId())] = g
			}
		}
	}
	return out
}

func assertGutters(t *testing.T, got, want map[ids.WorkspaceID]*frontendv1.RosterOpenRowRecencyGutter) {
	t.Helper()
	if len(got) != len(want) {
		t.Fatalf("gutters = %v, want %v", got, want)
	}
	for id, w := range want {
		if !proto.Equal(got[id], w) {
			t.Errorf("%s's gutter = %v, want %v", id, got[id], w)
		}
	}
}

func TestAReportedReachDrawsEachNamedRowsGutter(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(numberingRegistry())
	defer r.AttachRecencyReach()()

	// Act
	err := r.ReportRecencyReach(sidebar.RecencyReach{"a1": olderGutter(1), "p": newerGutter(2)})

	// Assert
	if err != nil {
		t.Fatalf("ReportRecencyReach: %v", err)
	}
	assertGutters(t, gutters(t, r), map[ids.WorkspaceID]*frontendv1.RosterOpenRowRecencyGutter{
		"a1": olderGutter(1), "p": newerGutter(2),
	})
}

func TestAReportReplacesTheReachWhole(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(numberingRegistry())
	defer r.AttachRecencyReach()()
	if err := r.ReportRecencyReach(sidebar.RecencyReach{"a1": olderGutter(1), "p": newerGutter(2)}); err != nil {
		t.Fatalf("ReportRecencyReach: %v", err)
	}

	// Act
	err := r.ReportRecencyReach(sidebar.RecencyReach{"k": olderGutter(3)})

	// Assert
	if err != nil {
		t.Fatalf("ReportRecencyReach: %v", err)
	}
	assertGutters(t, gutters(t, r), map[ids.WorkspaceID]*frontendv1.RosterOpenRowRecencyGutter{"k": olderGutter(3)})
}

func TestAReachNamingNoOpenRowIsDrawnNowhere(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(numberingRegistry())
	defer r.AttachRecencyReach()()

	// Act
	err := r.ReportRecencyReach(sidebar.RecencyReach{"gone": olderGutter(1)})

	// Assert
	if err != nil {
		t.Fatalf("ReportRecencyReach: %v", err)
	}
	assertGutters(t, gutters(t, r), map[ids.WorkspaceID]*frontendv1.RosterOpenRowRecencyGutter{})
}

func TestAReportWithNoStreamIsRefusedAndDrawsNothing(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(numberingRegistry())

	// Act
	err := r.ReportRecencyReach(sidebar.RecencyReach{"a1": olderGutter(1)})

	// Assert
	if !errors.Is(err, sidebar.ErrNoEmacsStream) {
		t.Fatalf("ReportRecencyReach = %v, want ErrNoEmacsStream", err)
	}
	assertGutters(t, gutters(t, r), map[ids.WorkspaceID]*frontendv1.RosterOpenRowRecencyGutter{})
}

func TestTheStreamsReleaseClearsEveryGutter(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(numberingRegistry())
	release := r.AttachRecencyReach()
	if err := r.ReportRecencyReach(sidebar.RecencyReach{"a1": olderGutter(1)}); err != nil {
		t.Fatalf("ReportRecencyReach: %v", err)
	}

	// Act
	release()

	// Assert
	assertGutters(t, gutters(t, r), map[ids.WorkspaceID]*frontendv1.RosterOpenRowRecencyGutter{})
	if err := r.ReportRecencyReach(sidebar.RecencyReach{"a1": olderGutter(1)}); !errors.Is(err, sidebar.ErrNoEmacsStream) {
		t.Fatalf("a report after the release = %v, want ErrNoEmacsStream", err)
	}
}

func TestASupersededStreamsReleaseLeavesItsSuccessorsReach(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(numberingRegistry())
	first := r.AttachRecencyReach()
	defer r.AttachRecencyReach()()
	if err := r.ReportRecencyReach(sidebar.RecencyReach{"a1": olderGutter(1)}); err != nil {
		t.Fatalf("ReportRecencyReach: %v", err)
	}

	// Act
	first()

	// Assert
	assertGutters(t, gutters(t, r), map[ids.WorkspaceID]*frontendv1.RosterOpenRowRecencyGutter{"a1": olderGutter(1)})
}

func TestANewStreamStartsWithNoReach(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(numberingRegistry())
	defer r.AttachRecencyReach()()
	if err := r.ReportRecencyReach(sidebar.RecencyReach{"a1": olderGutter(1)}); err != nil {
		t.Fatalf("ReportRecencyReach: %v", err)
	}

	// Act
	defer r.AttachRecencyReach()()

	// Assert
	assertGutters(t, gutters(t, r), map[ids.WorkspaceID]*frontendv1.RosterOpenRowRecencyGutter{})
}
