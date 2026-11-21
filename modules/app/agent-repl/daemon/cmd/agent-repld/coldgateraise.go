package main

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/workspace"
)

// coldGateQueue is the merge queue as a cold gate's raise tells it
// (merge.Orchestrator).
type coldGateQueue interface {
	ColdGateRaised(ws ids.WorkspaceID)
	ColdGateStoodAfterFailedAnswer(ws ids.WorkspaceID)
}

// coldGateRaiseSinks is everything a workspace's cold gate raise is told to.
type coldGateRaiseSinks struct {
	// cache is the cache-lapse keeper (cachelapse.Keeper.ColdGateRaised).
	cache func(ws ids.WorkspaceID, judged bool)
	// followers tells the workspace's followers the session went cold.
	followers func(ws ids.WorkspaceID)
	// queue answers the merge queue, nil until it is wired.
	queue func() coldGateQueue
	// answerDeployCheck answers the gate of a standing deploy check.
	answerDeployCheck func(ctx context.Context, ws ids.WorkspaceID) error
	// detach runs the deploy check's answer off the raise (inflight.Go).
	detach func(func())
	log    dlog.Logger
}

// raised tells every sink of WS's gate standing, for RAISE.
//
// A GATE AN ANSWER JUST FAILED ON IS ANSWERED BY NOBODY AUTOMATICALLY
// (workspace.ColdGateRaise.AnsweredAutomatically): the queue keeps the merge
// at its place without answering, and no deploy check answer runs, so a
// failing re-open is never retried in a loop (2026-11-21).
func (s coldGateRaiseSinks) raised(ws ids.WorkspaceID, raise workspace.ColdGateRaise) {
	s.cache(ws, raise.Judged())
	// A GATE A SHIM JUDGED JUST NOW IS NEWS to the workspace's followers; one
	// raised again from its record is not.
	if raise.Judged() {
		s.followers(ws)
	}
	automatic := raise.AnsweredAutomatically()
	if queue := s.queue(); queue != nil {
		if automatic {
			queue.ColdGateRaised(ws)
		} else {
			queue.ColdGateStoodAfterFailedAnswer(ws)
		}
	}
	if !automatic {
		return
	}
	// A GATE ON A STANDING DEPLOY CHECK IS ANSWERED AT ONCE, off the raise
	// (owner ruling, 2026-11-17): no deploy check waits on one.
	s.detach(func() {
		if err := s.answerDeployCheck(context.Background(), ws); err != nil {
			s.log.Error("daemon.workspace.deploy_checks", "the cold gate on a standing deploy check could not be answered", dlog.Context{"workspace": string(ws), "cause": err.Error()})
		}
	})
}

// detachInflight is coldGateRaiseSinks.detach in production.
func detachInflight(fn func()) { inflight.Go(fn) }
