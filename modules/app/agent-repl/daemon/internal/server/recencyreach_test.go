package server

import (
	"context"
	"errors"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/ids"
)

// streamReleaseBound bounds a cancelled WatchDaemon stream's handler
// returning and running its releases: 41µs measured on 2026-11-19 on an idle
// host, the bound left at a quarter second for a goroutine waiting its turn
// on a host the merge gate loads.
const streamReleaseBound = 250 * time.Millisecond

// olderReach is a reach of ID by `s-[` in PRESSES.
func olderReach(id string, presses uint32) *agentreplv1.RecencyReach {
	return &agentreplv1.RecencyReach{
		Workspace: &workspacev1.WorkspaceRef{Id: id, Dir: "/repo/" + id},
		Direction: &agentreplv1.RecencyReach_Older{Older: &agentreplv1.RecencyReachOlder{Presses: presses}},
	}
}

// newerReach is a reach of ID by `s-]` in PRESSES.
func newerReach(id string, presses uint32) *agentreplv1.RecencyReach {
	return &agentreplv1.RecencyReach{
		Workspace: &workspacev1.WorkspaceRef{Id: id, Dir: "/repo/" + id},
		Direction: &agentreplv1.RecencyReach_Newer{Newer: &agentreplv1.RecencyReachNewer{Presses: presses}},
	}
}

func reportRecencyReach(t *testing.T, h *harness, reaches ...*agentreplv1.RecencyReach) (*agentreplv1.ReportRecencyReachResponse, error) {
	t.Helper()
	resp, err := h.Client.ReportRecencyReach(context.Background(),
		connect.NewRequest(&agentreplv1.ReportRecencyReachRequest{Reaches: reaches}))
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

func TestAnEmacsStreamStandsAnEmptyRecencyReach(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	emacsDaemonStream(t, h, ctx, unfocusedEditor())

	// Assert
	reach, standing := h.Sidebar.heldRecencyReach()
	if !standing || len(reach) != 0 {
		t.Fatalf("held reach = (%v, %v), want an empty reach standing", reach, standing)
	}
}

func TestReportRecencyReachInstallsEachGutter(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	emacsDaemonStream(t, h, ctx, unfocusedEditor())

	// Act
	resp, err := reportRecencyReach(t, h, olderReach("a", 1), newerReach("b", 2))

	// Assert
	if err != nil || resp.GetSuccess() == nil {
		t.Fatalf("ReportRecencyReach = (%v, %v), want success", resp, err)
	}
	reach, _ := h.Sidebar.heldRecencyReach()
	if got := reach[ids.WorkspaceID("a")].GetOlder().GetPresses(); got != 1 {
		t.Errorf("a's gutter = %v, want older in 1", reach["a"])
	}
	if got := reach[ids.WorkspaceID("b")].GetNewer().GetPresses(); got != 2 {
		t.Errorf("b's gutter = %v, want newer in 2", reach["b"])
	}
}

func TestReportRecencyReachWithNoEmacsStreamIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := reportRecencyReach(t, h, olderReach("a", 1))

	// Assert
	if err != nil || resp.GetError().GetNoEmacsStream() == nil {
		t.Fatalf("ReportRecencyReach = (%v, %v), want the no_emacs_stream arm", resp, err)
	}
	if _, standing := h.Sidebar.heldRecencyReach(); standing {
		t.Fatal("a refused report stood a reach")
	}
}

func TestTheEmacsStreamsEndClearsItsRecencyReach(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Sidebar.recencyReleased = make(chan struct{}, 1)
	ctx, cancel := context.WithCancel(context.Background())
	emacsDaemonStream(t, h, ctx, unfocusedEditor())
	if _, err := reportRecencyReach(t, h, olderReach("a", 1)); err != nil {
		t.Fatalf("ReportRecencyReach: %v", err)
	}

	// Act
	cancel()

	// Assert
	select {
	case <-h.Sidebar.recencyReleased:
	case <-time.After(streamReleaseBound):
		t.Fatalf("the stream's end did not release its reach within %v", streamReleaseBound)
	}
	if _, standing := h.Sidebar.heldRecencyReach(); standing {
		t.Fatal("the stream's end left its reach standing")
	}
}

func TestAMalformedRecencyReachIsInvalid(t *testing.T) {
	cases := []struct {
		name    string
		reaches []*agentreplv1.RecencyReach
	}{
		{name: "no workspace", reaches: []*agentreplv1.RecencyReach{{
			Direction: &agentreplv1.RecencyReach_Older{Older: &agentreplv1.RecencyReachOlder{Presses: 1}},
		}}},
		{name: "a workspace reached twice", reaches: []*agentreplv1.RecencyReach{olderReach("a", 1), newerReach("a", 1)}},
		{name: "no direction", reaches: []*agentreplv1.RecencyReach{{Workspace: &workspacev1.WorkspaceRef{Id: "a", Dir: "/repo/a"}}}},
		{name: "zero older presses", reaches: []*agentreplv1.RecencyReach{olderReach("a", 0)}},
		{name: "zero newer presses", reaches: []*agentreplv1.RecencyReach{newerReach("a", 0)}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			emacsDaemonStream(t, h, ctx, unfocusedEditor())

			// Act
			_, err := reportRecencyReach(t, h, tc.reaches...)

			// Assert
			var cerr *connect.Error
			if !errors.As(err, &cerr) || cerr.Code() != connect.CodeInvalidArgument {
				t.Fatalf("ReportRecencyReach error = %v, want InvalidArgument", err)
			}
			if reach, _ := h.Sidebar.heldRecencyReach(); len(reach) != 0 {
				t.Fatalf("held reach = %v, want the invalid report to change nothing", reach)
			}
		})
	}
}
