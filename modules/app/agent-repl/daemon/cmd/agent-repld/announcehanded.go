package main

import (
	"context"
	"fmt"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/rollout"
	"agent-repld/internal/wsm"
)

// acceptHandedBound bounds an older daemon's announcement of a move to the
// newest daemon: a loopback unary answered from one state-database read and
// an in-memory insert. A loopback DaemonHealth round trip measured 6-7ms
// over 20 calls on an idle host (2026-11-16); the bound leaves a loaded
// host's state-database read its room. One that fires fails the move, which
// is offered again.
const acceptHandedBound = time.Second

// announceHanded tells the daemon at ADDRESS that OUTGOING is about to move
// WS there (rollout.Deps.AnnounceHanded). Its not_newest refusal is
// rollout.ErrNotNewest; any other refusal or failure is an error naming it.
func announceHanded(ctx context.Context, address string, ws wsm.Workspace, outgoing ids.InstanceID, expected rollout.Participants) error {
	ctx, cancel := context.WithTimeout(ctx, acceptHandedBound)
	defer cancel()
	client := newDaemonClient(address)
	resp, err := client.AcceptHandedWorkspace(ctx, connect.NewRequest(&agentreplv1.AcceptHandedWorkspaceRequest{
		Workspace:      &workspacev1.WorkspaceRef{Id: string(ws.ID), Dir: ws.Dir},
		OutgoingDaemon: string(outgoing),
		ExpectedHost:   expected.Host,
		ExpectedWeb:    expected.Web,
	}))
	if err != nil {
		return fmt.Errorf("announce the move of %s to %s (bound %s): %w", ws.ID, address, acceptHandedBound, err)
	}
	if refused := resp.Msg.GetError(); refused != nil {
		if refused.GetNotNewest() != nil {
			return rollout.ErrNotNewest
		}
		return fmt.Errorf("the daemon at %s refused the move of %s: %v", address, ws.ID, refused)
	}
	return nil
}
