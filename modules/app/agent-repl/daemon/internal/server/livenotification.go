package server

import (
	"context"
	"errors"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/wsm"
)

// opClaimLiveNotification is the operation ClaimLiveNotification's records
// carry.
const opClaimLiveNotification = "daemon.server.claim_live_notification"

// Banners is the desktop banners a focus claim chooses among
// (desktopnotify.Notifier): the claim itself, and the selection that lets
// every held banner go.
type Banners interface {
	// Claim answers the newest held banner still live and lets go of every
	// banner it read; desktopnotify.ErrEditorUnfocused while Emacs reads as
	// unfocused.
	Claim(ctx context.Context) (desktopnotify.Claim, error)
	// Forget lets go of every held banner, for reason.
	Forget(reason string)
}

// ClaimLiveNotification answers the workspace of the newest desktop banner
// still live, which the Emacs that just came forward selects. A claim while
// Emacs reads as unfocused is refused, and a failure to ask the platform which
// banners it holds is Connect Internal: neither consumes a banner.
func (s *server) ClaimLiveNotification(
	ctx context.Context,
	_ *connect.Request[agentreplv1.ClaimLiveNotificationRequest],
) (*connect.Response[agentreplv1.ClaimLiveNotificationResponse], error) {
	claim, err := s.deps.Banners.Claim(ctx)
	switch {
	case errors.Is(err, desktopnotify.ErrEditorUnfocused):
		return connect.NewResponse(&agentreplv1.ClaimLiveNotificationResponse{
			Result: &agentreplv1.ClaimLiveNotificationResponse_Error{Error: &agentreplv1.ClaimLiveNotificationError{
				Cause: &agentreplv1.ClaimLiveNotificationError_EditorUnfocused{
					EditorUnfocused: &agentreplv1.ClaimLiveNotificationEditorUnfocused{},
				},
			}},
		}), nil
	case err != nil:
		return nil, fail(s.log, opClaimLiveNotification, err)
	}
	success := &agentreplv1.ClaimLiveNotificationSuccess{
		Outcome: &agentreplv1.ClaimLiveNotificationSuccess_None{None: &agentreplv1.ClaimLiveNotificationNone{}},
	}
	if claim.Live {
		success.Outcome = &agentreplv1.ClaimLiveNotificationSuccess_Live{Live: &agentreplv1.ClaimLiveNotificationLive{
			Workspace: wsm.FamilyRef(claim.Target).Ref(),
		}}
	}
	return connect.NewResponse(&agentreplv1.ClaimLiveNotificationResponse{
		Result: &agentreplv1.ClaimLiveNotificationResponse_Success{Success: success},
	}), nil
}
