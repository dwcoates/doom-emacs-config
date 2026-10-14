package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"
)

func TestRegisterWorkspaceRecordsAMergeWorkspacesForkHead(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	dir := t.TempDir()
	origin := MergeOrigin{Requester: requester.ID, Request: NewDetachedMergeID(), ForkHead: "f00d"}

	// Act
	ws, _, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{
		Name: "sample-merge", Branch: "sample-merge", ParentBranch: "master", Parent: &requester.ID, RepoDir: dir,
		MergeOf: &origin,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	loaded, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if loaded.MergeOf == nil || !reflect.DeepEqual(*loaded.MergeOf, origin) {
		t.Fatalf("MergeOf = %+v, want %+v", loaded.MergeOf, origin)
	}
}

func TestAMergeWorkspaceForkedBeforeForkHeadsWereRecordedReadsAnEmptyForkHead(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)

	// Act
	ws := registerMergeWorkspace(t, s, requester.ID, NewDetachedMergeID())
	got := scalar[*string](t, s, `SELECT merge_fork_head FROM workspaces WHERE id = ?`, ws.ID)

	// Assert
	if got != nil {
		t.Fatalf("merge_fork_head = %q, want NULL", *got)
	}
}

func TestAForkHeadOnARowThatIsNoMergeWorkspaceIsRefused(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	corrupt(t, s, `UPDATE workspaces SET merge_fork_head = 'f00d' WHERE id = ?`, ws.ID)

	// Act
	_, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "merge_fork_head" {
		t.Fatalf("Workspace = %v, want a merge_fork_head DecodeError", err)
	}
}

func TestAWorkspaceHasNoLandingUntilOneIsRecorded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	ws := testWorkspace(t, s)
	loaded, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil || loaded.Landed != nil {
		t.Fatalf("Workspace = %+v, %v; want no landing", loaded.Landed, err)
	}
}

func TestSetLandedRoundTripsALanding(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	landing := Landing{Head: "f00d", At: time.Unix(1_700_000_000, 0).UTC()}

	// Act
	err := s.SetLanded(context.Background(), ws.ID, &landing)
	loaded, readErr := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil || readErr != nil {
		t.Fatalf("SetLanded = %v, Workspace = %v", err, readErr)
	}
	if loaded.Landed == nil || loaded.Landed.Head != landing.Head || !loaded.Landed.At.Equal(landing.At) {
		t.Fatalf("Landed = %+v, want %+v", loaded.Landed, landing)
	}
}

func TestSetLandedWithNilClearsTheLanding(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.SetLanded(context.Background(), ws.ID, &Landing{Head: "f00d", At: time.Unix(100, 0)}); err != nil {
		t.Fatalf("SetLanded: %v", err)
	}

	// Act
	err := s.SetLanded(context.Background(), ws.ID, nil)
	loaded, readErr := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil || readErr != nil || loaded.Landed != nil {
		t.Fatalf("SetLanded = %v, Workspace = %+v, %v; want no landing", err, loaded.Landed, readErr)
	}
}

func TestSetLandedRefusesAnIncompleteLanding(t *testing.T) {
	tests := []struct {
		name    string
		landing Landing
	}{
		{name: "no commit", landing: Landing{At: time.Unix(100, 0)}},
		{name: "no instant", landing: Landing{Head: "f00d"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := s.SetLanded(context.Background(), ws.ID, &tt.landing)

			// Assert
			if err == nil {
				t.Fatal("SetLanded accepted an incomplete landing")
			}
			if got := scalar[*string](t, s, `SELECT landed_head FROM workspaces WHERE id = ?`, ws.ID); got != nil {
				t.Fatalf("landed_head = %q after a refused landing, want NULL", *got)
			}
			found := false
			for _, r := range log.Records() {
				if r.Level == "error" && r.Operation == "daemon.wsm.set_landed" && r.Context["workspace"] == string(ws.ID) {
					found = true
				}
			}
			if !found {
				t.Fatalf("no daemon.wsm.set_landed ERROR naming the workspace in %+v", log.Records())
			}
		})
	}
}

func TestSetLandedOnAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetLanded(context.Background(), "nobody", &Landing{Head: "f00d", At: time.Unix(100, 0)})

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetLanded = %v, want ErrNotFound", err)
	}
}

func TestAWorkspaceRowWithHalfALandingIsRefused(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	corrupt(t, s, `UPDATE workspaces SET landed_head = 'f00d' WHERE id = ?`, ws.ID)

	// Act
	_, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "landed_head" {
		t.Fatalf("Workspace = %v, want a landed_head DecodeError", err)
	}
}
