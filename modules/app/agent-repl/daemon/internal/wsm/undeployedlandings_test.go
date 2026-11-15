package wsm

import (
	"context"
	"reflect"
	"testing"
	"time"
)

// undeployedLanding is merge workspace mergeWS's landing, at second at.
func undeployedLanding(mergeWS WorkspaceID, at int64) UndeployedLanding {
	return UndeployedLanding{MergeWorkspace: mergeWS, MergeName: string(mergeWS) + "-name", LandedAt: time.Unix(at, 0).UTC()}
}

// recordLandings records each landing, failing the test on an error.
func recordLandings(t *testing.T, s DB, landings ...UndeployedLanding) {
	t.Helper()
	for _, landing := range landings {
		if err := s.RecordUndeployedLanding(context.Background(), landing); err != nil {
			t.Fatalf("RecordUndeployedLanding(%s): %v", landing.MergeWorkspace, err)
		}
	}
}

func TestAnUndeployedLandingIsTakenWhole(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	want := undeployedLanding("mws-a", 100)
	recordLandings(t, s, want)

	// Act
	got, err := s.TakeDeployedLandings(context.Background(), false)

	// Assert
	if err != nil || !reflect.DeepEqual(got, []UndeployedLanding{want}) {
		t.Fatalf("TakeDeployedLandings = %+v, %v; want exactly %+v", got, err, want)
	}
}

func TestRecordingTheSameLandingTwiceKeepsTheFirst(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first := undeployedLanding("mws-a", 100)
	second := first
	second.MergeName = "another-name"
	recordLandings(t, s, first)

	// Act
	err := s.RecordUndeployedLanding(context.Background(), second)

	// Assert
	got, takeErr := s.TakeDeployedLandings(context.Background(), false)
	if err != nil || takeErr != nil || !reflect.DeepEqual(got, []UndeployedLanding{first}) {
		t.Fatalf("after recording twice: err %v, took %+v, %v; want only the first", err, got, takeErr)
	}
}

func TestADeployCoversEveryLandingBeforeIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordLandings(t, s, undeployedLanding("mws-a", 100), undeployedLanding("mws-b", 200))

	// Act
	covered, err := s.CoverUndeployedLandings(context.Background())

	// Assert
	got, takeErr := s.TakeDeployedLandings(context.Background(), true)
	if err != nil || covered != 2 || takeErr != nil || len(got) != 2 {
		t.Fatalf("CoverUndeployedLandings = %d, %v; took %+v, %v; want both covered", covered, err, got, takeErr)
	}
}

func TestALandingAfterTheDeployCommittedIsNotTakenByItsEnd(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordLandings(t, s, undeployedLanding("mws-a", 100))
	if _, err := s.CoverUndeployedLandings(context.Background()); err != nil {
		t.Fatalf("CoverUndeployedLandings: %v", err)
	}
	later := undeployedLanding("mws-b", 200)
	recordLandings(t, s, later)

	// Act
	got, err := s.TakeDeployedLandings(context.Background(), true)

	// Assert
	rest, restErr := s.TakeDeployedLandings(context.Background(), false)
	if err != nil || len(got) != 1 || got[0].MergeWorkspace != "mws-a" {
		t.Fatalf("TakeDeployedLandings(covered) = %+v, %v; want only mws-a", got, err)
	}
	if restErr != nil || !reflect.DeepEqual(rest, []UndeployedLanding{later}) {
		t.Fatalf("the uncovered landing = %+v, %v; want %+v left for the next deploy", rest, restErr, later)
	}
}

func TestTakenLandingsAreOldestFirst(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordLandings(t, s, undeployedLanding("mws-b", 200), undeployedLanding("mws-c", 100), undeployedLanding("mws-a", 100))

	// Act
	got, err := s.TakeDeployedLandings(context.Background(), false)

	// Assert
	var order []WorkspaceID
	for _, landing := range got {
		order = append(order, landing.MergeWorkspace)
	}
	if err != nil || !reflect.DeepEqual(order, []WorkspaceID{"mws-a", "mws-c", "mws-b"}) {
		t.Fatalf("order = %v, %v; want oldest first, by merge workspace on a tie", order, err)
	}
}

func TestATakenLandingIsTakenOnce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordLandings(t, s, undeployedLanding("mws-a", 100))
	if _, err := s.TakeDeployedLandings(context.Background(), false); err != nil {
		t.Fatalf("first TakeDeployedLandings: %v", err)
	}

	// Act
	got, err := s.TakeDeployedLandings(context.Background(), false)

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("second TakeDeployedLandings = %+v, %v; want nothing left", got, err)
	}
}
