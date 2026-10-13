package main

import (
	"context"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"connectrpc.com/connect"
)

func openTabsWorkspace(id string) *agentreplv1.OpenTabsWorkspace {
	return &agentreplv1.OpenTabsWorkspace{
		Workspace: &workspacev1.WorkspaceRef{Id: id, Dir: "/w/" + id},
		Page:      &agentreplv1.OpenTabsWorkspace_None{None: &agentreplv1.OpenTabsPageNone{}},
	}
}

// receiveAll drains an OpenTabs stream, returning its frames and its end.
func receiveAll(stream *connect.ServerStreamForClient[agentreplv1.OpenTabsResponse]) ([]*agentreplv1.OpenTabsResponse, error) {
	var frames []*agentreplv1.OpenTabsResponse
	for stream.Receive() {
		frames = append(frames, stream.Msg())
	}
	return frames, stream.Err()
}

// The fake answers every workspace ready, in the request's order, then
// finishes: a daemon whose every workspace opens at once.
func TestOpenTabsAnswersEveryWorkspaceReadyThenFinishes(t *testing.T) {
	// Arrange.
	_, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	req := &agentreplv1.OpenTabsRequest{Workspaces: []*agentreplv1.OpenTabsWorkspace{
		openTabsWorkspace("a"), openTabsWorkspace("b"),
	}}

	// Act.
	stream, err := client.OpenTabs(context.Background(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("open OpenTabs: %v", err)
	}
	defer stream.Close()
	frames, err := receiveAll(stream)

	// Assert.
	if err != nil {
		t.Fatalf("OpenTabs ended in %v, want a clean end", err)
	}
	var got []string
	for _, f := range frames {
		switch frame := f.GetFrame().(type) {
		case *agentreplv1.OpenTabsResponse_Opened:
			if frame.Opened.GetReady().GetRow().GetWorkspace().GetWorkspace().GetId() != frame.Opened.GetWorkspace().GetId() {
				t.Fatalf("opened %s carries no row of its own: %v", frame.Opened.GetWorkspace().GetId(), frame.Opened)
			}
			if frame.Opened.GetReady() == nil {
				t.Fatalf("opened %s is not ready: %v", frame.Opened.GetWorkspace().GetId(), frame.Opened)
			}
			got = append(got, "opened:"+frame.Opened.GetWorkspace().GetId())
		case *agentreplv1.OpenTabsResponse_Finished:
			got = append(got, "finished")
		default:
			t.Fatalf("unexpected frame %v", f)
		}
	}
	want := []string{"opened:a", "opened:b", "finished"}
	if len(got) != len(want) || got[0] != want[0] || got[1] != want[1] || got[2] != want[2] {
		t.Fatalf("frames = %v, want %v", got, want)
	}
}

// A request breaching the contract is refused before anything is sent.
func TestOpenTabsRefusesAMalformedRequest(t *testing.T) {
	cases := []struct {
		name string
		req  *agentreplv1.OpenTabsRequest
	}{
		{name: "no workspaces", req: &agentreplv1.OpenTabsRequest{}},
		{name: "a workspace with no page arm", req: &agentreplv1.OpenTabsRequest{
			Workspaces: []*agentreplv1.OpenTabsWorkspace{{Workspace: &workspacev1.WorkspaceRef{Id: "a", Dir: "/w/a"}}},
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			_, baseURL := newTestServer(t)
			client := newTestClient(t, baseURL)

			// Act.
			stream, err := client.OpenTabs(context.Background(), connect.NewRequest(tc.req))
			if err != nil {
				t.Fatalf("open OpenTabs: %v", err)
			}
			defer stream.Close()
			frames, err := receiveAll(stream)

			// Assert.
			if len(frames) != 0 || connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("OpenTabs = %d frames, %v; want none, invalid_argument", len(frames), err)
			}
		})
	}
}
