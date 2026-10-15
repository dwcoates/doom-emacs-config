package harness

import (
	"context"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// hostPush builds a host push whose session is live, with ATTACHED and, when
// OPEN, the composer open; LIVE false leaves the session ended.
func hostPush(live, attached, open bool) *agentreplv1.WatchHostWorkspaceResponse {
	existing := &agentreplv1.HostSessionExisting{}
	if live {
		session := &agentreplv1.HostSessionLive{ShimAttached: attached}
		if open {
			session.Composer = &agentreplv1.HostSessionLive_Open{Open: &agentreplv1.HostComposerOpen{}}
		} else {
			session.Composer = &agentreplv1.HostSessionLive_Merging{Merging: &agentreplv1.HostComposerMerging{}}
		}
		existing.Standing = &agentreplv1.HostSessionExisting_Live{Live: session}
	} else {
		existing.Standing = &agentreplv1.HostSessionExisting_Terminal{Terminal: &agentreplv1.HostSessionTerminal{}}
	}
	return &agentreplv1.WatchHostWorkspaceResponse{Push: &agentreplv1.WatchHostWorkspaceResponse_Host{
		Host: &agentreplv1.HostWorkspace{Session: &agentreplv1.HostWorkspace_Existing{Existing: existing}},
	}}
}

func TestSessionServes(t *testing.T) {
	tests := []struct {
		name string
		push *agentreplv1.WatchHostWorkspaceResponse
		want bool
	}{
		{name: "live, attached and open serves", push: hostPush(true, true, true), want: true},
		{name: "a detached shim does not", push: hostPush(true, false, true), want: false},
		{name: "a composer not open does not", push: hostPush(true, true, false), want: false},
		{name: "an ended session does not", push: hostPush(false, false, false), want: false},
		{name: "a push carrying no host view does not", push: &agentreplv1.WatchHostWorkspaceResponse{}, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act / Assert
			if got := SessionServes(tc.push); got != tc.want {
				t.Fatalf("SessionServes = %t, want %t", got, tc.want)
			}
		})
	}
}

func TestAwaitSessionServesAnswersTheFirstServingPush(t *testing.T) {
	// Arrange
	pushes := make(chan *agentreplv1.WatchHostWorkspaceResponse, 2)
	pushes <- hostPush(true, false, true)
	serving := hostPush(true, true, true)
	pushes <- serving
	s := &Stream[*agentreplv1.WatchHostWorkspaceResponse]{C: pushes}

	// Act
	got := AwaitSessionServes(t, context.Background(), s)

	// Assert
	if got != serving {
		t.Fatalf("answered %v, want the serving push", got)
	}
}
