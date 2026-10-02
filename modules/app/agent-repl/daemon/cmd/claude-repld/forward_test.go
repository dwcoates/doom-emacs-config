package main

import (
	"slices"
	"testing"

	"claude-repld/internal/ids"
)

// recordingRelay is a workspace.HostRelay that records feed tail returns.
type recordingRelay struct {
	tailReturns []ids.WorkspaceID
}

func (r *recordingRelay) OpenInEditor(ids.WorkspaceID, string, *uint32) {}
func (r *recordingRelay) ReloadWebapp(ids.WorkspaceID)                  {}
func (r *recordingRelay) PublishHostWorkspace(ids.WorkspaceID)          {}
func (r *recordingRelay) ReturnFeedToTail(ws ids.WorkspaceID) {
	r.tailReturns = append(r.tailReturns, ws)
}

func TestRelayForwarderCarriesReturnFeedToTailToTheBoundRelay(t *testing.T) {
	// Arrange.
	target := &recordingRelay{}
	var f relayForwarder
	f.bind(target)

	// Act.
	f.ReturnFeedToTail("w1")

	// Assert.
	if !slices.Equal(target.tailReturns, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("tail returns = %v, want [w1]", target.tailReturns)
	}
}
