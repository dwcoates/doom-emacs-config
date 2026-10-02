package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

func TestAnEmacsStreamIsJudgedByItsProcessIdentity(t *testing.T) {
	tests := []struct {
		name           string
		isNew          bool
		wantRedisplays int
	}{
		{name: "a new Emacs process stands the day's digest again", isNew: true, wantRedisplays: 1},
		{name: "a reconnect of the same Emacs changes nothing", isNew: false, wantRedisplays: 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.EditorInstances.isNew = tc.isNew
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			// Act
			emacsDaemonStream(t, h, ctx, unfocusedEditor())

			// Assert
			if got := h.EditorInstances.seen; len(got) != 1 || got[0] != testEditorInstance().GetValue() {
				t.Fatalf("instances judged = %v, want the stream's own", got)
			}
			if h.NewsDigest.redisplays != tc.wantRedisplays {
				t.Fatalf("redisplays = %d, want %d", h.NewsDigest.redisplays, tc.wantRedisplays)
			}
		})
	}
}

func TestAFailedRedisplayLeavesTheEmacsStreamServing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.EditorInstances.isNew = true
	h.NewsDigest.redisplayErr = errors.New("store gone")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	emacsDaemonStream(t, h, ctx, focusedEditor())

	// Assert
	if !h.Focus.Focused() {
		t.Fatal("the stream did not serve after a failed redisplay")
	}
}

func TestAnUnrecordableEmacsInstanceRefusesTheStream(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.EditorInstances.err = errors.New("store gone")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	stream, err := h.Client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{
		Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{
			ElispBuild: "elisp-test", Focus: unfocusedEditor(), Instance: testEditorInstance()}},
	}))
	if err == nil {
		stream.Receive()
		err = stream.Err()
	}

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("WatchDaemon = %v, want internal", err)
	}
	if h.NewsDigest.redisplays != 0 {
		t.Fatalf("redisplays = %d, want none for an Emacs the daemon could not judge", h.NewsDigest.redisplays)
	}
}

func TestAWebviewStreamIsNeverJudgedAsAnEmacs(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.EditorInstances.isNew = true
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	stream, err := h.Client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{
		Client: &agentreplv1.WatchDaemonRequest_Webview{Webview: &agentreplv1.WatchDaemonWebview{}},
	}))
	if err != nil {
		t.Fatalf("WatchDaemon: %v", err)
	}
	_ = stream

	// Assert
	if len(h.EditorInstances.seen) != 0 || h.NewsDigest.redisplays != 0 {
		t.Fatalf("judged %v and redisplayed %d times for a webview", h.EditorInstances.seen, h.NewsDigest.redisplays)
	}
}

// startupOpening is a startup event a test can recognize.
func startupOpening(n uint32) *agentreplv1.DaemonStartupEvent {
	return &agentreplv1.DaemonStartupEvent{Event: &agentreplv1.DaemonStartupEvent_Opening{
		Opening: &agentreplv1.DaemonStartupOpening{Workspaces: n}}}
}

func TestANewEmacsStreamIsToldItsStartup(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.EditorInstances.isNew = true
	h.Startup.events = []*agentreplv1.DaemonStartupEvent{startupOpening(3), startupOpening(4)}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	stream := emacsDaemonStream(t, h, ctx, unfocusedEditor())

	// Assert: the events arrive in order, among the standing pushes.
	var got []uint32
	for len(got) < 2 && stream.Receive() {
		if opening := stream.Msg().GetStartup().GetOpening(); opening != nil {
			got = append(got, opening.GetWorkspaces())
		}
	}
	if len(got) != 2 || got[0] != 3 || got[1] != 4 {
		t.Fatalf("startup events = %v (stream err %v), want [3 4]", got, stream.Err())
	}
}

func TestAReconnectingEmacsRunsNoStartup(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.EditorInstances.isNew = false
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	emacsDaemonStream(t, h, ctx, unfocusedEditor())

	// Assert
	h.Startup.mu.Lock()
	defer h.Startup.mu.Unlock()
	if h.Startup.runs != 0 {
		t.Fatalf("startup runs = %d, want none for a reconnect", h.Startup.runs)
	}
}
