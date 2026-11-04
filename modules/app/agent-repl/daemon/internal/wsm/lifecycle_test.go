package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestWorkspaceOriginatorIsReadBack(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.SetWorkspaceOriginator(context.Background(), ws.ID, "emoji_ack")

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceOriginator: %v", err)
	}
	got, found, err := s.WorkspaceOriginator(context.Background(), ws.ID)
	if err != nil || !found || got != "emoji_ack" {
		t.Fatalf("WorkspaceOriginator = %q, %v, %v; want the recorded originator", got, found, err)
	}
}

func TestWorkspaceOriginatorOfARegisteredWorkspaceIsNotFound(t *testing.T) {
	// Arrange: registered, never created, so nothing recorded who made it.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, found, err := s.WorkspaceOriginator(context.Background(), ws.ID)

	// Assert
	if err != nil || found || got != "" {
		t.Fatalf("WorkspaceOriginator = %q, %v, %v; want none", got, found, err)
	}
}

func TestWorkspaceOriginatorRefusals(t *testing.T) {
	tests := []struct {
		name     string
		act      func(s *store, ws WorkspaceID) error
		notFound bool
	}{
		{name: "an originator naming none is a defect", act: func(s *store, ws WorkspaceID) error {
			return s.SetWorkspaceOriginator(context.Background(), ws, "")
		}},
		{name: "a second originator is refused", act: func(s *store, ws WorkspaceID) error {
			if err := s.SetWorkspaceOriginator(context.Background(), ws, "emoji_ack"); err != nil {
				t.Fatalf("first SetWorkspaceOriginator: %v", err)
			}
			return s.SetWorkspaceOriginator(context.Background(), ws, "user_or_workspace")
		}},
		{name: "an originator of an unknown workspace is not found", act: func(s *store, _ WorkspaceID) error {
			return s.SetWorkspaceOriginator(context.Background(), WorkspaceID("absent"), "emoji_ack")
		}, notFound: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := tt.act(s, ws.ID)

			// Assert
			if err == nil {
				t.Fatal("err = nil, want a refusal")
			}
			if tt.notFound != errors.Is(err, ErrNotFound) {
				t.Fatalf("err = %v, want not-found %v", err, tt.notFound)
			}
		})
	}
}

const (
	announcedPR   = "https://github.com/acme/app/pull/7"
	announcingMsg = "https://acme.slack.com/archives/C1/p1730000000123456"
)

func TestAnOwnedLinkAnnouncesAStoredLink(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()
	if _, err := s.SetWorkspaceLink(ctx, ws.ID, LinkSet{URL: announcedPR, Label: "The pull request", Owned: true}); err != nil {
		t.Fatal(err)
	}

	// Act
	links, err := s.SetWorkspaceLink(ctx, ws.ID, LinkSet{URL: announcingMsg, Label: "The pull request's announcement", Owned: true, Announces: announcedPR})

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceLink: %v", err)
	}
	if len(links) != 2 || links[0].URL != announcingMsg || links[0].Announces != announcedPR || links[1].Announces != "" {
		t.Fatalf("links = %+v, want the announcement naming the pull request and the pull request announcing nothing", links)
	}
	read, err := s.WorkspaceLinks(ctx, ws.ID)
	if err != nil || read[0].Announces != announcedPR {
		t.Fatalf("WorkspaceLinks = %+v, %v; want the announcement read back", read, err)
	}
}

func TestARelabelSetsTheAnnouncementAnew(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()
	if _, err := s.SetWorkspaceLink(ctx, ws.ID, LinkSet{URL: announcedPR, Label: "The pull request", Owned: true}); err != nil {
		t.Fatal(err)
	}
	if _, err := s.SetWorkspaceLink(ctx, ws.ID, LinkSet{URL: announcingMsg, Label: "The announcement", Owned: true, Announces: announcedPR}); err != nil {
		t.Fatal(err)
	}

	// Act
	links, err := s.SetWorkspaceLink(ctx, ws.ID, LinkSet{URL: announcingMsg, Label: "The announcement again", Owned: true})

	// Assert
	if err != nil || links[0].Announces != "" {
		t.Fatalf("links = %+v, %v; want the relabel to announce nothing", links, err)
	}
}

func TestAnAnnouncementRefusals(t *testing.T) {
	tests := []struct {
		name    string
		set     LinkSet
		unknown bool
	}{
		{name: "announcing a url the workspace does not hold", set: LinkSet{URL: announcingMsg, Label: "The announcement", Owned: true, Announces: announcedPR}, unknown: true},
		{name: "a reference that announces", set: LinkSet{URL: announcingMsg, Label: "The announcement", Announces: announcedPR}},
		{name: "a link announcing itself", set: LinkSet{URL: announcingMsg, Label: "The announcement", Owned: true, Announces: announcingMsg}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			_, err := s.SetWorkspaceLink(context.Background(), ws.ID, tt.set)

			// Assert
			var unknown *UnknownAnnouncedLinkError
			if err == nil || errors.As(err, &unknown) != tt.unknown {
				t.Fatalf("err = %v, want a refusal (unknown announced link %v)", err, tt.unknown)
			}
			links, readErr := s.WorkspaceLinks(context.Background(), ws.ID)
			if readErr != nil || len(links) != 0 {
				t.Fatalf("links = %+v, %v; want nothing stored", links, readErr)
			}
		})
	}
}

func TestFinishProgressIsRecordedReplacedAndCleared(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()

	// Act
	first := s.PutFinishProgress(ctx, ws.ID, FinishProgress{HookIndex: 0})
	second := s.PutFinishProgress(ctx, ws.ID, FinishProgress{HookIndex: 1, Turn: "turn-1"})
	got, found, readErr := s.WorkspaceFinishProgress(ctx, ws.ID)
	clearErr := s.ClearFinishProgress(ctx, ws.ID)
	_, foundAfter, afterErr := s.WorkspaceFinishProgress(ctx, ws.ID)

	// Assert
	if first != nil || second != nil || readErr != nil || clearErr != nil || afterErr != nil {
		t.Fatalf("errors = %v, %v, %v, %v, %v", first, second, readErr, clearErr, afterErr)
	}
	if !found || got.HookIndex != 1 || got.Turn != "turn-1" {
		t.Fatalf("progress = %+v (found %v), want the second record", got, found)
	}
	if foundAfter {
		t.Fatal("progress found after the clear")
	}
}

func TestFinishProgressRefusals(t *testing.T) {
	tests := []struct {
		name     string
		ws       func(WorkspaceID) WorkspaceID
		progress FinishProgress
		notFound bool
	}{
		{name: "a negative hook is a defect", ws: func(ws WorkspaceID) WorkspaceID { return ws }, progress: FinishProgress{HookIndex: -1}},
		{name: "an unknown workspace is not found", ws: func(WorkspaceID) WorkspaceID { return "absent" }, progress: FinishProgress{}, notFound: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := s.PutFinishProgress(context.Background(), tt.ws(ws.ID), tt.progress)

			// Assert
			if err == nil || tt.notFound != errors.Is(err, ErrNotFound) {
				t.Fatalf("err = %v, want a refusal (not found %v)", err, tt.notFound)
			}
		})
	}
}

func TestClearingNoFinishProgressIsNoChange(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.ClearFinishProgress(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("ClearFinishProgress = %v, want no change", err)
	}
}
