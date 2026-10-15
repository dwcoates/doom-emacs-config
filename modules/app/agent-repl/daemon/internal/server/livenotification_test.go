package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
)

func claimLiveNotification(h *harness) (*agentreplv1.ClaimLiveNotificationResponse, error) {
	resp, err := h.Client.ClaimLiveNotification(context.Background(),
		connect.NewRequest(&agentreplv1.ClaimLiveNotificationRequest{}))
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

func TestClaimLiveNotificationAnswersTheLiveBannersWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Banners.claim = desktopnotify.Claim{Live: true, Target: desktopnotify.Target{ID: testWorkspaceID, Dir: testWorkspaceDir}}

	// Act
	resp, err := claimLiveNotification(h)

	// Assert
	if err != nil {
		t.Fatalf("ClaimLiveNotification: %v", err)
	}
	got := resp.GetSuccess().GetLive().GetWorkspace()
	if got.GetId() != string(testWorkspaceID) || got.GetDir() != testWorkspaceDir {
		t.Fatalf("result = %v, want live naming the banner's workspace", resp.GetResult())
	}
}

func TestClaimLiveNotificationAnswersNoneWhenNoBannerIsLive(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := claimLiveNotification(h)

	// Assert
	if err != nil || resp.GetSuccess().GetNone() == nil {
		t.Fatalf("ClaimLiveNotification = (%v, %v), want success none", resp, err)
	}
}

func TestClaimLiveNotificationWhileEmacsIsUnfocusedAnswersTheRefusalArm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Banners.err = desktopnotify.ErrEditorUnfocused

	// Act
	resp, err := claimLiveNotification(h)

	// Assert
	if err != nil || resp.GetError().GetEditorUnfocused() == nil {
		t.Fatalf("ClaimLiveNotification = (%v, %v), want editor_unfocused", resp, err)
	}
}

func TestClaimLiveNotificationSurfacesAFailedPlatformQueryAsInternal(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	h.Banners.err = errors.New("ask the banner program terminal-notifier which banners it still holds: exit status 1")

	// Act
	_, err := claimLiveNotification(h)

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("ClaimLiveNotification = %v, want Connect Internal", err)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opClaimLiveNotification})
	if !ok || rec.Context["cause"] != h.Banners.err.Error() {
		t.Fatalf("ERROR record = %+v (found=%v), want the failure's cause", rec, ok)
	}
}
