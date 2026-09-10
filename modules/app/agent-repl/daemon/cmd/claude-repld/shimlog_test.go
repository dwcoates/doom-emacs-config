package main

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/wsm"
)

type fakeShimLogWorkspaceStore struct {
	workspace wsm.Workspace
	err       error
}

func (f fakeShimLogWorkspaceStore) WorkspaceByDir(context.Context, string) (wsm.Workspace, error) {
	return f.workspace, f.err
}

type fakeShimLogRelauncher struct {
	mu     sync.Mutex
	calls  []shimLogRelaunchCall
	err    error
	called chan ids.WorkspaceID
	block  map[ids.WorkspaceID]<-chan struct{}
}

type shimLogRelaunchCall struct {
	workspace ids.WorkspaceID
	reason    rollout.RelaunchReason
}

func (f *fakeShimLogRelauncher) RelaunchShim(ctx context.Context, ws ids.WorkspaceID, reason rollout.RelaunchReason) error {
	f.mu.Lock()
	f.calls = append(f.calls, shimLogRelaunchCall{workspace: ws, reason: reason})
	f.mu.Unlock()
	if f.called != nil {
		f.called <- ws
	}
	if release := f.block[ws]; release != nil {
		select {
		case <-release:
		case <-ctx.Done():
			return ctx.Err()
		}
	}
	return f.err
}

func (f *fakeShimLogRelauncher) Calls() []shimLogRelaunchCall {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]shimLogRelaunchCall(nil), f.calls...)
}

func TestForceShimLogRollReportsEveryOutcome(t *testing.T) {
	tests := []struct {
		name          string
		lookupErr     error
		relaunchErr   error
		wantCalls     int
		wantLastLevel string
		wantText      string
	}{
		{name: "success", wantCalls: 1, wantLastLevel: "info", wantText: "rolled the shim"},
		{name: "workspace lookup failure", lookupErr: errors.New("workspace missing"), wantLastLevel: "error", wantText: "could not resolve"},
		{name: "rollout failure", relaunchErr: errors.New("relaunch refused"), wantCalls: 1, wantLastLevel: "error", wantText: "could not roll"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()
			db := fakeShimLogWorkspaceStore{
				workspace: wsm.Workspace{ID: ids.WorkspaceID("ws-1")},
				err:       tc.lookupErr,
			}
			relauncher := &fakeShimLogRelauncher{err: tc.relaunchErr}
			req := dlog.ShimRollRequest{Dir: "/worktree", LogID: "log-1", SizeBytes: 11, HardBytes: 10, Log: log}

			// Act.
			forceShimLogRoll(context.Background(), req, db, relauncher)

			// Assert.
			calls := relauncher.Calls()
			if len(calls) != tc.wantCalls {
				t.Fatalf("relaunch calls = %d, want %d", len(calls), tc.wantCalls)
			}
			if len(calls) == 1 && calls[0] != (shimLogRelaunchCall{workspace: ids.WorkspaceID("ws-1"), reason: rollout.ReasonShimLogCeiling}) {
				t.Fatalf("relaunch call = %+v, want workspace ws-1 and reason %q", calls[0], rollout.ReasonShimLogCeiling)
			}
			records := log.Records()
			if first := records[0]; first.Level != "debug" || first.Operation != shimLogRollOperation || first.Context["log_id"] != "log-1" {
				t.Fatalf("first record = %+v, want the attributed request-boundary debug record", first)
			}
			last := records[len(records)-1]
			if last.Level != tc.wantLastLevel || !strings.Contains(last.Message, tc.wantText) {
				t.Fatalf("last record = %+v, want level %q containing %q", last, tc.wantLastLevel, tc.wantText)
			}
		})
	}
}

func TestRunShimLogRollsDoesNotSerializeDifferentWorkspaces(t *testing.T) {
	// Arrange.
	ctx, cancel := context.WithCancel(context.Background())
	requests := make(chan dlog.ShimRollRequest, 2)
	started := make(chan ids.WorkspaceID, 2)
	releaseFirst := make(chan struct{})
	relauncher := &fakeShimLogRelauncher{
		called: started,
		block:  map[ids.WorkspaceID]<-chan struct{}{ids.WorkspaceID("ws-1"): releaseFirst},
	}
	db := routingShimLogWorkspaceStore{byDir: map[string]wsm.Workspace{
		"/one": {ID: ids.WorkspaceID("ws-1")},
		"/two": {ID: ids.WorkspaceID("ws-2")},
	}}
	done := make(chan error, 1)
	go func() { done <- runShimLogRolls(ctx, requests, db, relauncher) }()
	log := dlog.NewTestLogger()

	// Act.
	requests <- dlog.ShimRollRequest{Dir: "/one", Log: log}
	requests <- dlog.ShimRollRequest{Dir: "/two", Log: log}
	first := <-started
	second := <-started
	close(releaseFirst)
	cancel()
	err := <-done

	// Assert.
	if first == second || (first != ids.WorkspaceID("ws-1") && second != ids.WorkspaceID("ws-1")) || (first != ids.WorkspaceID("ws-2") && second != ids.WorkspaceID("ws-2")) {
		t.Fatalf("started workspaces = %q, %q, want ws-1 and ws-2 before releasing ws-1", first, second)
	}
	if err != nil {
		t.Fatalf("runShimLogRolls() error = %v, want nil after cancellation", err)
	}
}

type routingShimLogWorkspaceStore struct {
	byDir map[string]wsm.Workspace
}

func (f routingShimLogWorkspaceStore) WorkspaceByDir(_ context.Context, dir string) (wsm.Workspace, error) {
	return f.byDir[dir], nil
}

func TestRunShimLogRollsRefusesAClosedRequestChannel(t *testing.T) {
	// Arrange.
	requests := make(chan dlog.ShimRollRequest)
	close(requests)

	// Act.
	err := runShimLogRolls(context.Background(), requests, fakeShimLogWorkspaceStore{}, &fakeShimLogRelauncher{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "request channel closed") {
		t.Fatalf("runShimLogRolls() error = %v, want closed-channel error", err)
	}
}
