package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

// someActions is one before and one after prompt, as an async request names them.
var someActions = MergeActions{Before: []string{"run the linters"}, After: []string{"post the summary"}}

// detachedSource is an async request for branch feature carrying ACTIONS.
func detachedSource(actions MergeActions) MergeSource {
	return MergeSource{Kind: MergeSourceBranch, Branch: "feature", Detach: NewDetachedMergeID(), DetachMaker: "maker", DetachName: "sample-merge", Actions: actions}
}

func TestAnAsyncRequestKeepsItsMergeActionsInTheQueue(t *testing.T) {
	tests := []struct {
		name    string
		actions MergeActions
	}{
		{name: "both actions", actions: someActions},
		{name: "a before action alone", actions: MergeActions{Before: []string{"run the linters"}}},
		{name: "an after action alone", actions: MergeActions{After: []string{"post the summary"}}},
		{name: "no action", actions: MergeActions{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())

			// Act
			err := s.RequestMerge(context.Background(), repo, ws.ID, detachedSource(tt.actions), time.Unix(100, 0))
			queue, readErr := s.MergeQueue(context.Background(), repo)

			// Assert
			if err != nil || readErr != nil || len(queue) != 1 {
				t.Fatalf("RequestMerge = %v, MergeQueue = %+v, %v; want one request", err, queue, readErr)
			}
			if got := queue[0].Source.Actions; !reflect.DeepEqual(got, tt.actions) {
				t.Fatalf("Actions = %+v, want %+v", got, tt.actions)
			}
		})
	}
}

func TestAnAsyncRequestsMergeActionsSurviveARestartBeforeItsHandOff(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	first, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	ws := testWorkspace(t, first.(*store))
	repo := RepoKey(t.TempDir())
	if err := first.RequestMerge(context.Background(), repo, ws.ID, detachedSource(someActions), time.Unix(100, 0)); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act
	second, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { second.Close() })
	queue, err := second.MergeQueue(context.Background(), repo)

	// Assert
	if err != nil || len(queue) != 1 {
		t.Fatalf("MergeQueue = %+v, %v; want the one request", queue, err)
	}
	if got := queue[0].Source.Actions; !reflect.DeepEqual(got, someActions) {
		t.Fatalf("Actions after the restart = %+v, want %+v", got, someActions)
	}
}

func TestMergeActionsOnARequestThatRunsInPlaceAreInvalid(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	source := MergeSource{Kind: MergeSourceOwnBranch, Actions: someActions}

	// Act
	err := s.RequestMerge(context.Background(), repo, ws.ID, source, time.Unix(100, 0))
	queue, _ := s.MergeQueue(context.Background(), repo)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "merge actions on the request") {
		t.Fatalf("RequestMerge = %v, want the in-place request's actions refused", err)
	}
	if len(queue) != 0 {
		t.Fatalf("MergeQueue = %+v, want nothing recorded", queue)
	}
}

func TestAStoredMergeActionListThatIsNotAListIsRefused(t *testing.T) {
	tests := []struct {
		name   string
		column string
	}{
		{name: "the before actions", column: "source_actions_before"},
		{name: "the after actions", column: "source_actions_after"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())
			if err := s.RequestMerge(context.Background(), repo, ws.ID, detachedSource(someActions), time.Unix(100, 0)); err != nil {
				t.Fatalf("RequestMerge: %v", err)
			}
			corrupt(t, s, `UPDATE merge_queue SET `+tt.column+` = 'not json' WHERE workspace_id = ?`, ws.ID)

			// Act
			_, err := s.MergeQueue(context.Background(), repo)

			// Assert
			var decode *DecodeError
			if !errors.As(err, &decode) || decode.Table != "merge_queue" || decode.Field != tt.column {
				t.Fatalf("MergeQueue = %v, want a %s decode error", err, tt.column)
			}
		})
	}
}

func TestHandOffDetachedMergeLeavesTheMergeWorkspacesRequestWithoutActions(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	source := detachedSource(someActions)
	if err := s.RequestMerge(context.Background(), repo, requester.ID, source, time.Unix(100, 0)); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	forked := registerMergeWorkspace(t, s, requester.ID, source.Detach)

	// Act
	err := s.HandOffDetachedMerge(context.Background(), repo, requester.ID, source.Detach, forked.ID, "sample-merge", "", time.Unix(200, 0))

	// Assert
	queue, readErr := s.MergeQueue(context.Background(), repo)
	if err != nil || readErr != nil || len(queue) != 1 || queue[0].Workspace != forked.ID {
		t.Fatalf("HandOffDetachedMerge = %v, MergeQueue = %+v, %v; want the merge workspace's entry", err, queue, readErr)
	}
	if got := queue[0].Source.Actions; !reflect.DeepEqual(got, MergeActions{}) {
		t.Fatalf("the merge workspace's request carries %+v, want none: its creation job holds them", got)
	}
}

func TestAStoredMergeOriginAnswersNoLandingOfItsOwn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	dir := t.TempDir()
	origin := MergeOrigin{Requester: requester.ID, Request: NewDetachedMergeID(), ForkHead: "f00d",
		TargetDir: "/repo/main", ParentBranch: "master", Actions: someActions}

	// Act
	ws, _, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{
		Name: "sample-merge", Branch: "sample-merge", ParentBranch: "master", Parent: &requester.ID, RepoDir: dir, MergeOf: &origin,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	loaded, err := s.Workspace(context.Background(), ws.ID)

	// Assert: the landing is transport-only, carried by the creation job.
	want := MergeOrigin{Requester: origin.Requester, Request: origin.Request, ForkHead: origin.ForkHead}
	if err != nil || loaded.MergeOf == nil || !reflect.DeepEqual(*loaded.MergeOf, want) {
		t.Fatalf("MergeOf = %+v, %v; want %+v", loaded.MergeOf, err, want)
	}
}
