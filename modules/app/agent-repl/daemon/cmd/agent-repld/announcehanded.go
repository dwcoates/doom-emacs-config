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
// an in-memory insert. Measured 2026-11-16 13:31 on the live daemon under a
// load average of 10 with three test runs going: a loopback DaemonHealth
// round trip had a worst of 18ms over 60 calls, and GetWorkspaceStatus, which
// reads the workspace's row as this call does, a worst of 9ms over 60. The
// bound is a little under three times the worse of the two. One that fires
// fails the move, which stays served where it is and is offered again.
const acceptHandedBound = 50 * time.Millisecond

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
