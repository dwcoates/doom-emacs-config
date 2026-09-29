package main

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/bounce"
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
	// decision is what the registry answers.
	decision bounce.Decision
	// ends, when set, is the bounce's end, told to done before the call
	// returns: a bounce the registry ran at once.
	ends  bool
	endOf error
}

type shimLogRelaunchCall struct {
	workspace ids.WorkspaceID
	reason    rollout.RelaunchReason
	force     bool
}

func (f *fakeShimLogRelauncher) BounceShim(ctx context.Context, ws ids.WorkspaceID, reason rollout.RelaunchReason, force bool, done func(error)) (bounce.Decision, error) {
	f.mu.Lock()
	f.calls = append(f.calls, shimLogRelaunchCall{workspace: ws, reason: reason, force: force})
	f.mu.Unlock()
	if f.called != nil {
		f.called <- ws
	}
	if release := f.block[ws]; release != nil {
		select {
		case <-release:
		case <-ctx.Done():
			return bounce.Decision{}, ctx.Err()
		}
	}
	if f.err != nil {
		return bounce.Decision{}, f.err
	}
	if f.ends {
		done(f.endOf)
	}
	return f.decision, nil
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
		relauncher    *fakeShimLogRelauncher
		wantCalls     int
		wantLastLevel string
		wantText      string
	}{
		{name: "registered for the freeness edge", relauncher: &fakeShimLogRelauncher{decision: bounce.Decision{TurnInFlight: true}},
			wantCalls: 1, wantLastLevel: "info", wantText: "took the shim-log roll"},
		{name: "bounced at once and ended", relauncher: &fakeShimLogRelauncher{decision: bounce.Decision{Now: true}, ends: true},
			wantCalls: 1, wantLastLevel: "info", wantText: "took the shim-log roll"},
		{name: "workspace lookup failure", lookupErr: errors.New("workspace missing"), relauncher: &fakeShimLogRelauncher{},
			wantLastLevel: "error", wantText: "could not resolve"},
		{name: "registry refusal", relauncher: &fakeShimLogRelauncher{err: errors.New("relaunch refused")},
			wantCalls: 1, wantLastLevel: "error", wantText: "refused the shim-log roll"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()
			db := fakeShimLogWorkspaceStore{
				workspace: wsm.Workspace{ID: ids.WorkspaceID("ws-1")},
				err:       tc.lookupErr,
			}
			req := dlog.ShimRollRequest{Dir: "/worktree", LogID: "log-1", SizeBytes: 11, HardBytes: 10, Log: log}

			// Act.
			forceShimLogRoll(context.Background(), req, db, tc.relauncher)

			// Assert.
			calls := tc.relauncher.Calls()
			if len(calls) != tc.wantCalls {
				t.Fatalf("bounce calls = %d, want %d", len(calls), tc.wantCalls)
			}
			if len(calls) == 1 && calls[0] != (shimLogRelaunchCall{workspace: ids.WorkspaceID("ws-1"), reason: rollout.ReasonShimLogCeiling}) {
				t.Fatalf("bounce call = %+v, want workspace ws-1, reason %q, unforced", calls[0], rollout.ReasonShimLogCeiling)
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

func TestTheShimLogRollRecordsHowItsBounceEnded(t *testing.T) {
	tests := []struct {
		name      string
		endOf     error
		wantLevel string
		wantText  string
	}{
		{name: "the bounce ran", wantLevel: "info", wantText: "rolled the shim"},
		{name: "the bounce failed", endOf: errors.New("prelaunch refused"), wantLevel: "error", wantText: "could not roll"},
		{name: "the bounce was unregistered", endOf: bounce.ErrUnregistered, wantLevel: "info", wantText: "nothing is left to roll"},
		{name: "the bounce was handed across", endOf: bounce.ErrHandedAcross, wantLevel: "info", wantText: "handed across"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()
			db := fakeShimLogWorkspaceStore{workspace: wsm.Workspace{ID: ids.WorkspaceID("ws-1")}}
			relauncher := &fakeShimLogRelauncher{decision: bounce.Decision{Now: true}, ends: true, endOf: tc.endOf}
			req := dlog.ShimRollRequest{Dir: "/worktree", LogID: "log-1", Log: log}

			// Act.
			forceShimLogRoll(context.Background(), req, db, relauncher)

			// Assert.
			for _, r := range log.Records() {
				if r.Level == tc.wantLevel && strings.Contains(r.Message, tc.wantText) {
					if tc.wantLevel == "error" && r.Context["cause"] != tc.endOf.Error() {
						t.Fatalf("record = %+v, want the bounce's cause", r)
					}
					return
				}
			}
			t.Fatalf("records = %+v, want a %s record containing %q", log.Records(), tc.wantLevel, tc.wantText)
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
