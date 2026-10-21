package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"google.golang.org/protobuf/proto"
)

// promptHook is a prompt hook of TEXT.
func promptHook(text string) *repoconfigv1.DeployHook {
	return &repoconfigv1.DeployHook{Hook: &repoconfigv1.DeployHook_Prompt{Prompt: &repoconfigv1.DeployHookPrompt{Text: text}}}
}

// scriptHook is a script hook of PATH bounded at SECONDS.
func scriptHook(path string, seconds uint32) *repoconfigv1.DeployHook {
	return &repoconfigv1.DeployHook{Hook: &repoconfigv1.DeployHook_Script{Script: &repoconfigv1.DeployHookScript{Path: path, TimeoutSeconds: seconds}}}
}

// someHooks is a pre-deploy prompt and a post-deploy script, as an async
// request names them.
var someHooks = DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{promptHook("run the linters")}, PostDeploy: []*repoconfigv1.DeployHook{scriptHook("bin/notify.sh", 30)}}

// sameHooks reports whether two DeployHooks hold equal hooks, stage by stage.
func sameHooks(a, b DeployHooks) bool {
	same := func(x, y []*repoconfigv1.DeployHook) bool {
		if len(x) != len(y) {
			return false
		}
		for i := range x {
			if !proto.Equal(x[i], y[i]) {
				return false
			}
		}
		return true
	}
	return same(a.PreDeploy, b.PreDeploy) && same(a.PostDeploy, b.PostDeploy)
}

// detachedSource is an async request for branch feature carrying HOOKS.
func detachedSource(hooks DeployHooks) MergeSource {
	return MergeSource{Kind: MergeSourceBranch, Branch: "feature", Detach: NewDetachedMergeID(), DetachMaker: "maker", DetachName: "sample-merge", Hooks: hooks}
}

func TestAnAsyncRequestKeepsItsDeployHooksInTheQueue(t *testing.T) {
	tests := []struct {
		name  string
		hooks DeployHooks
	}{
		{name: "both stages", hooks: someHooks},
		{name: "a pre-deploy hook alone", hooks: DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{promptHook("run the linters")}}},
		{name: "a post-deploy script alone", hooks: DeployHooks{PostDeploy: []*repoconfigv1.DeployHook{scriptHook("bin/notify.sh", 30)}}},
		{name: "no hook", hooks: DeployHooks{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())

			// Act
			err := s.RequestMerge(context.Background(), repo, ws.ID, detachedSource(tt.hooks), time.Unix(100, 0))
			queue, readErr := s.MergeQueue(context.Background(), repo)

			// Assert
			if err != nil || readErr != nil || len(queue) != 1 {
				t.Fatalf("RequestMerge = %v, MergeQueue = %+v, %v; want one request", err, queue, readErr)
			}
			if got := queue[0].Source.Hooks; !sameHooks(got, tt.hooks) {
				t.Fatalf("Hooks = %+v, want %+v", got, tt.hooks)
			}
		})
	}
}

func TestAnAsyncRequestsDeployHooksSurviveARestartBeforeItsHandOff(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	first, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	ws := testWorkspace(t, first.(*store))
	repo := RepoKey(t.TempDir())
	if err := first.RequestMerge(context.Background(), repo, ws.ID, detachedSource(someHooks), time.Unix(100, 0)); err != nil {
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
	if got := queue[0].Source.Hooks; !sameHooks(got, someHooks) {
		t.Fatalf("Hooks after the restart = %+v, want %+v", got, someHooks)
	}
}

func TestDeployHooksOnARequestThatRunsInPlaceAreInvalid(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	source := MergeSource{Kind: MergeSourceOwnBranch, Hooks: someHooks}

	// Act
	err := s.RequestMerge(context.Background(), repo, ws.ID, source, time.Unix(100, 0))
	queue, _ := s.MergeQueue(context.Background(), repo)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "deploy hooks on the request") {
		t.Fatalf("RequestMerge = %v, want the in-place request's hooks refused", err)
	}
	if len(queue) != 0 {
		t.Fatalf("MergeQueue = %+v, want nothing recorded", queue)
	}
}

func TestAStoredDeployHookListThatIsNotAListIsRefused(t *testing.T) {
	tests := []struct {
		name   string
		column string
	}{
		{name: "the pre-deploy hooks", column: "source_pre_deploy_hooks"},
		{name: "the post-deploy hooks", column: "source_post_deploy_hooks"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())
			if err := s.RequestMerge(context.Background(), repo, ws.ID, detachedSource(someHooks), time.Unix(100, 0)); err != nil {
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

func TestHandOffDetachedMergeLeavesTheMergeWorkspacesRequestWithoutHooks(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	source := detachedSource(someHooks)
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
	if got := queue[0].Source.Hooks; !got.Empty() {
		t.Fatalf("the merge workspace's request carries %+v, want none: its creation job holds them", got)
	}
}

func TestAStoredMergeOriginAnswersNoLandingOfItsOwn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	dir := t.TempDir()
	origin := MergeOrigin{Requester: requester.ID, Request: NewDetachedMergeID(), ForkHead: "f00d",
		TargetDir: "/repo/main", ParentBranch: "master", Hooks: someHooks}

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
