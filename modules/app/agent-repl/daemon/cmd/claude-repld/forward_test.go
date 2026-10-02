package main

import (
	"context"
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

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

// fixedHistory is a feed.HistorySource answering one page.
type fixedHistory struct{ page *conversationv1.HistoryPage }

func (f fixedHistory) ReadHistory(context.Context, ids.WorkspaceID, *conversationv1.AgentId, *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	return f.page, nil
}

func TestHistoryForwarderReadsThroughTheBoundFleet(t *testing.T) {
	// Arrange.
	want := &conversationv1.HistoryPage{}
	var f historyForwarder
	f.bind(fixedHistory{page: want})

	// Act.
	got, err := f.ReadHistory(context.Background(), "w1", nil, nil)

	// Assert.
	if err != nil || got != want {
		t.Fatalf("ReadHistory = %v, %v; want the fleet's page", got, err)
	}
}

func TestHistoryForwarderBeforeTheFleetIsAnError(t *testing.T) {
	// Arrange.
	var f historyForwarder

	// Act.
	_, err := f.ReadHistory(context.Background(), "w1", nil, nil)

	// Assert.
	if err == nil {
		t.Fatal("ReadHistory before the fleet was bound answered no error")
	}
}
