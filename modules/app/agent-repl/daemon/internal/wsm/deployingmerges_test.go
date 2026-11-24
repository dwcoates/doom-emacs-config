package wsm

import (
	"context"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// deployingMergeOf is merge workspace WS's landing in the queue keyed REPO,
// still deploying since second AT.
func deployingMergeOf(repo RepoKey, ws WorkspaceID, at int64) DeployingMerge {
	return DeployingMerge{
		Repo: repo, Workspace: ws, Lease: LeaseID("lease-" + ws), Credited: "requester", Async: true, Name: string(ws) + "-name",
		Branch: "feature", TargetDir: "/w/doom", ActiveSince: time.Unix(at-60, 0).UTC(), DeployingSince: time.Unix(at, 0).UTC(), Timed: true,
	}
}

// canonicalRepo is a queue key the store keeps as it is spelled.
func canonicalRepo(t *testing.T) RepoKey {
	t.Helper()
	key, err := normalizeRepoKey(RepoKey(t.TempDir()))
	if err != nil {
		t.Fatalf("normalizeRepoKey: %v", err)
	}
	return key
}

func TestADeployingMergeIsReadAsItWasRecorded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := canonicalRepo(t)
	want := deployingMergeOf(repo, "mws-a", 100)

	// Act
	replaced, err := s.RecordDeployingMerge(context.Background(), want)

	// Assert
	got, readErr := s.DeployingMergeOf(context.Background(), repo)
	if err != nil || replaced != nil || readErr != nil || got == nil || !reflect.DeepEqual(*got, want) {
		t.Fatalf("record = (%+v, %v), read = (%+v, %v); want %+v recorded with nothing replaced", replaced, err, got, readErr, want)
	}
}

func TestAQueueWithNoDeployingMergeReadsNone(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := canonicalRepo(t)

	// Act
	got, err := s.DeployingMergeOf(context.Background(), repo)

	// Assert
	if err != nil || got != nil {
		t.Fatalf("DeployingMergeOf = (%+v, %v), want none", got, err)
	}
}

func TestANewerDeployingMergeReplacesTheQueuesAndAnswersIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := canonicalRepo(t)
	first, second := deployingMergeOf(repo, "mws-a", 100), deployingMergeOf(repo, "mws-b", 200)
	if _, err := s.RecordDeployingMerge(context.Background(), first); err != nil {
		t.Fatal(err)
	}

	// Act
	replaced, err := s.RecordDeployingMerge(context.Background(), second)

	// Assert
	got, readErr := s.DeployingMergeOf(context.Background(), repo)
	if err != nil || replaced == nil || !reflect.DeepEqual(*replaced, first) {
		t.Fatalf("record = (%+v, %v), want the first answered as replaced", replaced, err)
	}
	if readErr != nil || got == nil || !reflect.DeepEqual(*got, second) {
		t.Fatalf("read = (%+v, %v), want only the second", got, readErr)
	}
}

func TestEndingADeployingMergeTakesItOnce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := canonicalRepo(t)
	want := deployingMergeOf(repo, "mws-a", 100)
	if _, err := s.RecordDeployingMerge(context.Background(), want); err != nil {
		t.Fatal(err)
	}

	// Act
	first, firstErr := s.EndDeployingMerge(context.Background(), repo)
	second, secondErr := s.EndDeployingMerge(context.Background(), repo)

	// Assert
	if firstErr != nil || first == nil || !reflect.DeepEqual(*first, want) {
		t.Fatalf("first end = (%+v, %v), want %+v", first, firstErr, want)
	}
	if secondErr != nil || second != nil {
		t.Fatalf("second end = (%+v, %v), want nothing left to end", second, secondErr)
	}
}

func TestADeployingMergeNamingNoInstantIsRefused(t *testing.T) {
	tests := []struct {
		name  string
		blank func(*DeployingMerge)
	}{
		{name: "no admission instant", blank: func(m *DeployingMerge) { m.ActiveSince = time.Time{} }},
		{name: "no deploying instant", blank: func(m *DeployingMerge) { m.DeployingSince = time.Time{} }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			repo := canonicalRepo(t)
			m := deployingMergeOf(repo, "mws-a", 100)
			tt.blank(&m)

			// Act
			_, err := s.RecordDeployingMerge(context.Background(), m)

			// Assert
			got, readErr := s.DeployingMergeOf(context.Background(), repo)
			if err == nil || readErr != nil || got != nil {
				t.Fatalf("record = %v, read = (%+v, %v); want the record refused and nothing written", err, got, readErr)
			}
			if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.record_deploying_merge",
				Message: "refused a deploying merge that names no instant"}); !ok {
				t.Fatalf("records = %v, want the refusal at ERROR", logs.Records())
			}
		})
	}
}
