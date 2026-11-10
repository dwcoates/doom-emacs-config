package workspace

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// A FORK IS MADE AT ITS FIRST BRING-UP (docs/protobuf-design/fork-lineage.md).
//
// The create records a pending fork and nothing else. The bring-up spawns the
// child's shim, then — while the PARENT's turn sequence is held, so the parent
// starts no turn — asks the parent's running shim where the fork ends
// (shim.v1 GetForkPoint) and the child's shim to make the vendor's fork there
// (shim.v1 ForkSession). The fork's vendor id and lineage are recorded in one
// step before the session is resumed, so a resume refused as cold, or a start
// that fails and is retried, resumes the SAME fork rather than forking twice.
//
// The vendor's fork leaves the conversation's content exactly as the parent
// sent it, so the fork's first request reads the parent's whole prefix from the
// prompt cache; its feed reads the inherited past from the parent's book.

// DeliveryExclusion holds a workspace's turn sequence while work runs: the
// prompt queue's Exclusively.
type DeliveryExclusion interface {
	// Exclusively runs PERFORM while no prompt is delivered to WS.
	Exclusively(ctx context.Context, ws ids.WorkspaceID, what string, perform func(context.Context) error) error
}

// forkCall names the held work in the prompt queue's record.
const forkCall = "fork"

// ErrForkRefused wraps every reason a pending fork could not be made: the
// parent has no conversation, its shim cannot name a point, or the child's
// shim refused the fork. The pending fork stands, and the next bring-up tries
// again.
var ErrForkRefused = errors.New("workspace: the fork could not be made")

// ErrForkNothingFinished is the parent's shim answering that the turn in
// flight is its conversation's first (shim.v1.GetForkPointNothingFinished):
// a fork carries only finished turns, so this one would carry nothing. The
// create asks first (ProbeForkPoint) and refuses on it, before anything is
// made; at a bring-up it wraps ErrForkRefused as every other reason does.
var ErrForkNothingFinished = errors.New("workspace: the fork's parent has no finished turn")

// madeFork is what one fork answered: the vendor id to resume, and its lineage.
type madeFork struct {
	vendorSessionID string
	lineage         wsm.ForkLineage
}

// makeFork makes WS's pending fork through CLIENT (the child's held shim) and
// records it, answering the vendor session id the bring-up resumes.
func (f *Fleet) makeFork(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client, pending wsm.PendingFork) (string, error) {
	log = log.With(dlog.Context{"parent": string(pending.Parent)})
	if pending.Own() {
		return f.makeOwnFork(ctx, log, ws, client, pending)
	}
	parentRecord, err := f.deps.DB.Workspace(ctx, pending.Parent)
	if err != nil {
		log.Error(opBringUp, "the fork's parent workspace could not be read", dlog.Context{"cause": err.Error()})
		return "", fmt.Errorf("%w: read the parent %q: %w", ErrForkRefused, pending.Parent, err)
	}
	parentConv, err := readRecordedSession(ctx, f.deps.DB, pending.Parent)
	if err != nil {
		log.Error(opBringUp, "the fork's parent session could not be read", dlog.Context{"cause": err.Error()})
		return "", fmt.Errorf("%w: read the parent's session: %w", ErrForkRefused, err)
	}
	if !parentConv.hasConversation() {
		log.Error(opBringUp, "the fork's parent has no conversation to fork", dlog.Context{"parent_session_recorded": parentConv.recorded})
		return "", fmt.Errorf("%w: workspace %q has no conversation to fork", ErrForkRefused, pending.Parent)
	}
	// THE PARENT'S TRANSCRIPT IS WHERE ITS SESSION WAS SPAWNED: the recorded
	// ConfigDir, never the routing or the reader's selection, which can name a
	// root the parent has not been restarted into. The child's shim imports
	// the transcript from it when the child spends from another account.
	parentRoot := parentConv.session.ConfigDir
	if parentRoot == "" {
		log.Error(opBringUp, "the fork's parent conversation records no account root", dlog.Context{
			"parent_vendor_session_id": parentConv.session.VendorSessionID,
		})
		return "", fmt.Errorf("%w: workspace %q records no account root for its conversation", ErrForkRefused, pending.Parent)
	}
	if f.deps.Deliveries == nil {
		// Every production fleet is wired with the queue; a fleet without it
		// could fork a parent mid-turn, so it fails rather than forking.
		err := errors.New("workspace: the fleet holds no delivery exclusion to fork under")
		log.Error(opBringUp, "the fleet was built without a delivery exclusion; no fork can be made", dlog.Context{"cause": err.Error()})
		return "", err
	}

	var made madeFork
	err = f.deps.Deliveries.Exclusively(ctx, pending.Parent, forkCall, func(ctx context.Context) error {
		point, err := f.forkPoint(ctx, log, pending.Parent)
		if err != nil {
			return err
		}
		made, err = forkThrough(ctx, log, client, ws, pending.Parent, parentRecord.Dir, parentRoot, parentConv.session.VendorSessionID, point)
		return err
	})
	if err != nil {
		return "", err
	}
	if err := f.deps.DB.ResolveFork(ctx, made.vendorSessionID, made.lineage); err != nil {
		log.Error(opBringUp, "the fork was made but could not be recorded; the next bring-up forks again", dlog.Context{
			"vendor_session_id": made.vendorSessionID, "cause": err.Error(),
		})
		return "", fmt.Errorf("record the fork of %q: %w", pending.Parent, err)
	}
	log.Info(opBringUp, "the fork was made and recorded; the session resumes it", dlog.Context{
		"vendor_session_id": made.vendorSessionID, "parent_book": made.lineage.ParentBook,
		"through_ms": made.lineage.ThroughMs, "uninherited_turns": len(made.lineage.Uninherited),
	})
	return made.vendorSessionID, nil
}

// makeOwnFork makes WS's fork of one of its own conversations, the one
// PENDING names, through CLIENT (WS's held shim), and records it.
//
// THE WHOLE CONVERSATION IS FORKED, UNDER NO HOLD. The conversation copied is
// the one the bind just stepped the workspace out of: its session was stopped
// before the bind, and the shim being brought up is the only one the
// workspace has, so nothing drives the copied conversation and no turn of it
// can be in flight. Its transcript sits under the account root the
// workspace's session records, which the bind kept.
func (f *Fleet) makeOwnFork(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client, pending wsm.PendingFork) (string, error) {
	log = log.With(dlog.Context{"parent_vendor_session_id": pending.ParentVendorSessionID})
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error(opBringUp, "the workspace whose own conversation is forked could not be read", dlog.Context{"cause": err.Error()})
		return "", fmt.Errorf("%w: read the workspace %q: %w", ErrForkRefused, ws, err)
	}
	conv, err := readRecordedSession(ctx, f.deps.DB, ws)
	if err != nil {
		log.Error(opBringUp, "the session of the workspace whose own conversation is forked could not be read", dlog.Context{"cause": err.Error()})
		return "", fmt.Errorf("%w: read the workspace's session: %w", ErrForkRefused, err)
	}
	root := conv.session.ConfigDir
	if root == "" {
		log.Error(opBringUp, "the workspace whose own conversation is forked records no account root", nil)
		return "", fmt.Errorf("%w: workspace %q records no account root for its conversation", ErrForkRefused, ws)
	}
	point := wholeForkPoint(f.now().UnixMilli())
	made, err := forkThrough(ctx, log, client, ws, ws, record.Dir, root, pending.ParentVendorSessionID, point)
	if err != nil {
		return "", err
	}
	if err := f.deps.DB.ResolveFork(ctx, made.vendorSessionID, made.lineage); err != nil {
		log.Error(opBringUp, "the own fork was made but could not be recorded; the next bring-up forks again", dlog.Context{
			"vendor_session_id": made.vendorSessionID, "cause": err.Error(),
		})
		return "", fmt.Errorf("record the own fork of %q: %w", pending.ParentVendorSessionID, err)
	}
	log.Info(opBringUp, "the workspace's own conversation was forked and recorded; the session resumes the fork", dlog.Context{
		"vendor_session_id": made.vendorSessionID, "parent_book": made.lineage.ParentBook, "through_ms": made.lineage.ThroughMs,
	})
	return made.vendorSessionID, nil
}

// wholeForkPoint is the point of a fork that carries a whole conversation
// nothing drives, bounded at AT.
func wholeForkPoint(at int64) *shimv1.GetForkPointSuccess {
	return &shimv1.GetForkPointSuccess{
		Point:   &shimv1.ForkPoint{Extent: &shimv1.ForkPoint_Whole{Whole: &shimv1.ForkPointWhole{}}},
		Through: &conversationv1.ConversationThrough{AtMs: at},
	}
}

// forkPoint answers where a fork of PARENT ends: the parent's running shim's
// answer, or the whole transcript bounded at now when no shim runs it — a
// conversation nothing drives has no turn in flight.
func (f *Fleet) forkPoint(ctx context.Context, log dlog.Logger, parent ids.WorkspaceID) (*shimv1.GetForkPointSuccess, error) {
	client, live := f.Client(parent)
	if !live {
		log.Info(opBringUp, "the fork's parent has no running shim; the whole transcript is forked", nil)
		return wholeForkPoint(f.now().UnixMilli()), nil
	}
	resp, err := client.GetForkPoint(ctx, &shimv1.GetForkPointRequest{})
	if err != nil {
		log.Error(opBringUp, "the fork's parent shim could not be asked for the fork point", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("%w: ask the parent's shim for the fork point: %w", ErrForkRefused, err)
	}
	switch result := resp.GetResult().(type) {
	case *shimv1.GetForkPointResponse_Success:
		return result.Success, nil
	case *shimv1.GetForkPointResponse_Failure:
		if result.Failure.GetNothingFinished() != nil {
			log.Info(opBringUp, "the fork's parent is in its conversation's first turn; there is no finished turn to fork", dlog.Context{
				"cause": result.Failure.GetDetail(),
			})
			return nil, fmt.Errorf("%w: %w: %s", ErrForkRefused, ErrForkNothingFinished, result.Failure.GetDetail())
		}
		log.Error(opBringUp, "the fork's parent shim could name no fork point", dlog.Context{
			"cause": result.Failure.GetDetail(), "arm": fmt.Sprintf("%T", result.Failure.GetCause()),
		})
		return nil, fmt.Errorf("%w: the parent's shim named no fork point: %s", ErrForkRefused, result.Failure.GetDetail())
	default:
		log.Error(opBringUp, "the fork's parent shim answered no outcome", nil)
		return nil, fmt.Errorf("%w: the parent's shim answered GetForkPoint with no outcome", ErrForkRefused)
	}
}

// ProbeForkPoint asks whether a fork of PARENT taken now could be made,
// answering the error forkPoint would: ErrForkNothingFinished for a parent in
// its conversation's first turn. It reads and holds nothing; the create asks
// it before anything is named, recorded or materialized, and the bring-up
// still names the point it forks at under the parent's hold.
func (f *Fleet) ProbeForkPoint(ctx context.Context, log dlog.Logger, parent ids.WorkspaceID) error {
	_, err := f.forkPoint(ctx, log, parent)
	return err
}

// forkThrough asks the child's shim for the vendor's fork at POINT of the
// parent filed under PARENTROOT and answers what it made.
func forkThrough(ctx context.Context, log dlog.Logger, client shimclient.Client, ws, parent ids.WorkspaceID, parentDir, parentRoot, parentVendorSessionID string, point *shimv1.GetForkPointSuccess) (madeFork, error) {
	resp, err := client.ForkSession(ctx, &shimv1.ForkSessionRequest{
		ParentVendorSessionId: parentVendorSessionID,
		ParentWorkspaceDir:    parentDir,
		ParentAccountRoot:     parentRoot,
		Point:                 point.GetPoint(),
	})
	if err != nil {
		log.Error(opBringUp, "the child's shim could not be asked for the fork", dlog.Context{"cause": err.Error()})
		return madeFork{}, fmt.Errorf("%w: ask the child's shim for the fork: %w", ErrForkRefused, err)
	}
	switch result := resp.GetResult().(type) {
	case *shimv1.ForkSessionResponse_Success:
		uninherited := make([]ids.TurnID, 0, len(point.GetUninheritedTurns()))
		for _, turn := range point.GetUninheritedTurns() {
			uninherited = append(uninherited, ids.TurnID(turn.GetValue()))
		}
		return madeFork{
			vendorSessionID: result.Success.GetVendorSessionId(),
			lineage: wsm.ForkLineage{
				Workspace:   ws,
				Parent:      parent,
				ParentBook:  result.Success.GetParentBook().GetValue(),
				ThroughMs:   point.GetThrough().GetAtMs(),
				Uninherited: uninherited,
			},
		}, nil
	case *shimv1.ForkSessionResponse_Failure:
		log.Error(opBringUp, "the child's shim refused the fork", dlog.Context{
			"cause": result.Failure.GetDetail(), "arm": fmt.Sprintf("%T", result.Failure.GetCause()),
			"parent_vendor_session_id": parentVendorSessionID, "parent_dir": parentDir, "parent_account_root": parentRoot,
		})
		return madeFork{}, fmt.Errorf("%w: the child's shim refused it: %s", ErrForkRefused, result.Failure.GetDetail())
	default:
		log.Error(opBringUp, "the child's shim answered the fork with no outcome", nil)
		return madeFork{}, fmt.Errorf("%w: the child's shim answered ForkSession with no outcome", ErrForkRefused)
	}
}

// noteLineage tells the feed where WS's inherited past is read from, from the
// recorded lineage: nothing for a workspace that is no fork.
func (f *Fleet) noteLineage(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) error {
	lineage, err := f.deps.DB.Lineage(ctx, ws)
	if err != nil {
		log.Error(opBringUp, "the workspace's fork lineage could not be read", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("read the fork lineage of %q: %w", ws, err)
	}
	f.deps.Feed.NoteLineage(ws, lineageSegments(lineage))
	return nil
}

// lineageSegments is the recorded lineage as the feed pages it.
func lineageSegments(lineage []wsm.ForkLineage) []feed.LineageSegment {
	if len(lineage) == 0 {
		return nil
	}
	out := make([]feed.LineageSegment, 0, len(lineage))
	for _, link := range lineage {
		uninherited := make(map[ids.TurnID]bool, len(link.Uninherited))
		for _, turn := range link.Uninherited {
			uninherited[turn] = true
		}
		out = append(out, feed.LineageSegment{
			Workspace:   link.Parent,
			Book:        &conversationv1.AgentId{Value: link.ParentBook},
			ThroughMs:   link.ThroughMs,
			Uninherited: uninherited,
		})
	}
	return out
}
