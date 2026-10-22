package wsm

import "context"

// This file is the durable half of a workspace subscriber's UNTOLD NOTICE
// (internal/workspacesubs): the notice it was last submitted about one
// followed workspace, which may not have been told yet, and every change that
// notice says. It is durable for the reason mergenotices.go gives: a deploy
// restarts the daemon, and an in-memory untold notice was forgotten at every
// restart.

// workspaceUntoldNoticesDDL is the layout-70 addition: one row per subscriber
// and followed workspace (untold.go). The fresh-file schema appends this same
// text (schema.go), so a migrated file and a created one cannot differ.
const workspaceUntoldNoticesDDL = `
CREATE TABLE workspace_untold_notices (
  subscriber TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  followed   TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  turn_id    TEXT NOT NULL,
  sentences  TEXT NOT NULL,
  PRIMARY KEY (subscriber, followed)
);
`

// UntoldWorkspaceNotice is a subscriber's latest notice about one followed
// workspace that may not have been told yet.
type UntoldWorkspaceNotice struct {
	// Subscriber is the workspace whose agent the notice is for.
	Subscriber WorkspaceID
	// Followed is the workspace the notice is about.
	Followed WorkspaceID
	// Turn is the turn the notice was submitted as; empty when its submission
	// failed and nothing was submitted.
	Turn TurnID
	// Sentences states every change the notice says, oldest first.
	Sentences []string
}

// UntoldWorkspaceNotice answers SUBSCRIBER's untold notice about FOLLOWED, and
// whether one is recorded.
func (s *store) UntoldWorkspaceNotice(ctx context.Context, subscriber, followed WorkspaceID) (UntoldWorkspaceNotice, bool, error) {
	turn, sentences, found, err := s.readUntold(ctx, workspaceUntold, "daemon.wsm.untold_workspace_notice", subscriber, string(followed))
	if err != nil || !found {
		return UntoldWorkspaceNotice{}, false, err
	}
	return UntoldWorkspaceNotice{Subscriber: subscriber, Followed: followed, Turn: turn, Sentences: sentences}, true, nil
}

// RecordUntoldWorkspaceNotice records N as its subscriber's untold notice
// about its followed workspace, replacing the one recorded before. A notice
// that says no change is refused.
func (s *store) RecordUntoldWorkspaceNotice(ctx context.Context, n UntoldWorkspaceNotice) error {
	return s.recordUntold(ctx, workspaceUntold, "daemon.wsm.record_untold_workspace_notice", n.Subscriber, string(n.Followed), n.Turn, n.Sentences)
}
