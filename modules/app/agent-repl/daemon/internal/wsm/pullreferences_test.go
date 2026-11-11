package wsm

import (
	"context"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/pullrefs"
)

var (
	gnsPull  = pullrefs.Ref{Target: pullrefs.Target{Owner: "ChessCom", Repo: "gns-server"}, Number: "613"}
	doomPull = pullrefs.Ref{Target: pullrefs.Target{Owner: "dodge", Repo: "doom"}, Number: "613"}
)

func TestPullReferencesAreNoneUntilRecorded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, err := s.PullReferences(context.Background(), ws.ID)

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("PullReferences = (%+v, %v), want none", got, err)
	}
}

func TestRecordPullReferenceAnswersThemInTheOrderFirstNamed(t *testing.T) {
	tests := []struct {
		name    string
		records []pullrefs.Ref
		want    []pullrefs.Ref
	}{
		{"one pull request", []pullrefs.Ref{gnsPull}, []pullrefs.Ref{gnsPull}},
		{"two in the order named", []pullrefs.Ref{doomPull, gnsPull}, []pullrefs.Ref{doomPull, gnsPull}},
		{"a pull request named again keeps its first place", []pullrefs.Ref{doomPull, gnsPull, doomPull}, []pullrefs.Ref{doomPull, gnsPull}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			ctx := context.Background()

			// Act
			for i, ref := range tc.records {
				if err := s.RecordPullReference(ctx, ws.ID, ref, instant.Add(time.Duration(i)*time.Minute)); err != nil {
					t.Fatalf("RecordPullReference: %v", err)
				}
			}

			// Assert
			got, err := s.PullReferences(ctx, ws.ID)
			if err != nil || !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("PullReferences = (%+v, %v), want %+v", got, err, tc.want)
			}
		})
	}
}

func TestPullReferencesAreTheWorkspacesOwn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	ctx := context.Background()
	if err := s.RecordPullReference(ctx, other.ID, gnsPull, instant); err != nil {
		t.Fatalf("RecordPullReference: %v", err)
	}

	// Act
	got, err := s.PullReferences(ctx, ws.ID)

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("PullReferences = (%+v, %v), want none of another workspace's", got, err)
	}
}

func TestRecordPullReferenceRefusesAnUnknownWorkspaceAndLogs(t *testing.T) {
	// Arrange
	s, logs := testStore(t)

	// Act
	err := s.RecordPullReference(context.Background(), "ws-nope", gnsPull, instant)

	// Assert
	if err == nil {
		t.Fatal("RecordPullReference accepted a workspace the registry does not hold")
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM pull_references`); n != 0 {
		t.Fatalf("pull_references holds %d rows after a refusal", n)
	}
	if !loggedError(logs.Records(), "daemon.wsm.record_pull_reference") {
		t.Fatalf("the refusal was not logged at error: %+v", logs.Records())
	}
}

func TestRecordPullReferenceRefusesAMalformedNumberAndLogs(t *testing.T) {
	tests := []struct {
		name   string
		number string
	}{
		{"an empty number", ""},
		{"a leading zero", "0613"},
		{"a letter", "61a"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			ws := testWorkspace(t, s)
			ref := gnsPull
			ref.Number = tc.number

			// Act
			err := s.RecordPullReference(context.Background(), ws.ID, ref, instant)

			// Assert
			if err == nil {
				t.Fatalf("RecordPullReference accepted number %q", tc.number)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM pull_references`); n != 0 {
				t.Fatalf("pull_references holds %d rows after a refusal", n)
			}
			if !loggedError(logs.Records(), "daemon.wsm.record_pull_reference") {
				t.Fatalf("the refusal was not logged at error: %+v", logs.Records())
			}
		})
	}
}
