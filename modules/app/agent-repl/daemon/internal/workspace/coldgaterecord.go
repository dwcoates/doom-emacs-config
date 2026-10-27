package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/opentabs"
	"agent-repld/internal/wsm"
)

// A WORKSPACE AT ITS COLD GATE RUNS NO SHIM until the user answers the gate
// (owner ruling, 2026-10-27). The gate is the shim's judgment, made once at a
// start (askToStartSession's cold refusal); the fleet records it (wsm
// ColdGateRecord), raises it, and stands the shim down. From then on the gate
// is raised from the record with no shim behind it -- at every start, the
// boot's and a handover successor's included -- and its conversation is read
// from the store (ReadHistory). Only an answer spawns a shim: the one the
// remediated resume runs through (ResumeCold).
//
// THE RECORD IS RETIRED when no gate can stand any more: a session coming up
// (sessionUp, which every answer's resume reaches, a clear's fresh
// conversation included) and a rebind to another conversation. Forgetting the
// workspace drops it with the workspace's row.

// recordGate records the gate the shim judged for WS. A record that cannot be
// written is an ERROR and leaves the gate standing in memory: this daemon
// stands it as before, and a later start asks the shim again.
func (f *Fleet) recordGate(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, rec wsm.ColdGateRecord) {
	if err := f.deps.DB.SetColdGate(ctx, ws, rec); err != nil {
		log.Error(opColdGate, "could not record the cold gate; it stands for this daemon only, and a later start asks the shim again",
			dlog.Context{"vendor_session_id": rec.VendorSessionID, "cause": err.Error()})
		return
	}
	log.Debug(opColdGate, "recorded the cold gate", dlog.Context{"vendor_session_id": rec.VendorSessionID})
}

// raiseRecordedGate raises WS's recorded cold gate with no shim, answering
// whether one was recorded. It runs under the workspace's start gate.
func (f *Fleet) raiseRecordedGate(ctx context.Context, ws ids.WorkspaceID) (bool, error) {
	log := f.logFor(ctx, ws)
	rec, found, err := f.deps.DB.ColdGate(ctx, ws)
	if err != nil {
		log.Error(opColdGate, "could not read the workspace's recorded cold gate; it is not started",
			dlog.Context{"cause": err.Error()})
		return false, fmt.Errorf("start session for %q: read the recorded cold gate: %w", ws, err)
	}
	if !found {
		return false, nil
	}
	f.raiseColdGate(ws, rec.VendorSessionID, rec.Cold, rec.ConfigDir)
	f.deps.Steps(ws, opentabs.Step{Kind: opentabs.StepColdGate})
	log.Info(opColdGate, "raised the workspace's recorded cold gate; no shim is started until it is answered",
		dlog.Context{"vendor_session_id": rec.VendorSessionID, "context_tokens": rec.Cold.GetContextTokens()})
	return true, nil
}

// retireRecordedGate retires WS's recorded cold gate, WHY naming the reason
// for the record. A failure is recorded at ERROR and answered.
func (f *Fleet) retireRecordedGate(ctx context.Context, ws ids.WorkspaceID, why string) error {
	cleared, err := f.deps.DB.ClearColdGate(ctx, ws)
	if err != nil {
		f.logFor(ctx, ws).Error(opColdGate, "could not retire the workspace's recorded cold gate; a later start raises it again",
			dlog.Context{"why": why, "cause": err.Error()})
		return fmt.Errorf("retire the recorded cold gate of %q: %w", ws, err)
	}
	if cleared {
		f.logFor(ctx, ws).Info(opColdGate, "retired the workspace's recorded cold gate: "+why, nil)
	}
	return nil
}

// standDownAtGate stands down the shim a standing cold gate leaves held with no
// session, keeping the gate. A shim a session runs on, a gate an answer has
// taken (its resume runs through this shim), and a workspace with no gate or no
// shim are left alone: each is checked under the fleet's lock in the same step
// that takes the shim out of the map.
func (f *Fleet) standDownAtGate(ctx context.Context, ws ids.WorkspaceID) {
	log := f.logFor(ctx, ws)
	f.mu.Lock()
	gate, gated := f.coldGates[ws]
	session, held := f.sessions[ws]
	switch {
	case !gated || gate.answering:
		f.mu.Unlock()
		log.Debug(opColdGate, "no unanswered cold gate stands; the shim is not stood down", dlog.Context{"gated": gated})
		return
	case !held || session.sessionStarted:
		f.mu.Unlock()
		log.Debug(opColdGate, "the gate holds no idle shim to stand down", dlog.Context{"held": held})
		return
	}
	delete(f.sessions, ws)
	f.movedLocked()
	f.mu.Unlock()
	if err := f.teardown(ctx, ws, session, false, "the workspace stands at its cold gate, which runs no shim until it is answered"); err != nil {
		log.Error(opColdGate, "the cold gate's shim could not be stood down", dlog.Context{"cause": err.Error()})
		return
	}
	log.Info(opColdGate, "stood the cold gate's shim down; the gate stands until it is answered", nil)
}

// heldForAnswer answers the shim an answered gate's resume runs through: the
// one still held at the gate, or one spawned for the answer, held with no
// session. It runs under the workspace's start gate, so no other start spawns
// beside it, and leaves the taken gate standing: only the resume's outcome
// retires it or raises it again.
func (f *Fleet) heldForAnswer(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, record wsm.Workspace, previous wsm.Session, resume ColdResume) (*live, error) {
	gate := f.startGate(ws)
	gate.Lock()
	defer gate.Unlock()
	if held, ok := f.idleHeldForAnswer(ws); ok {
		return held, nil
	}
	configDir := accountRootFor(f.deps.Accounts, record.Dir, previous)
	hostSessionID := previous.HostSessionID
	if hostSessionID == "" {
		hostSessionID = wsm.NewHostSessionID()
	}
	src := source{VendorSessionID: resume.VendorSessionID, ColdRemediation: resume.Remediation}.withRecord(record)
	log.Info(opColdGate, "the answered gate has no shim; spawning the one its resume runs through", dlog.Context{"vendor_session_id": resume.VendorSessionID})
	client, path, err := f.reuseOrBringUp(ctx, stampSession(log, hostSessionID), ws, record.Dir, f.deps.SocketPath(ws), configDir, hostSessionID, src)
	if err != nil {
		return nil, fmt.Errorf("re-open session for %q: bring up the shim the answer resumes through: %w", ws, err)
	}
	if path == pathAdopted {
		// A SHIM ALREADY RUNS A SESSION ON THIS CONVERSATION: there is nothing
		// cold left to answer, and a resume would be refused already_started.
		return nil, refuse(log, "AnswerColdGate", ArmNoSession,
			fmt.Sprintf("a running shim already holds workspace %q's session; the answered gate has nothing to re-open", ws), false)
	}
	held, ok := f.idleHeldForAnswer(ws)
	if !ok || held.client != client {
		return nil, fmt.Errorf("re-open session for %q: %w", ws, ErrShimTaken)
	}
	return held, nil
}

// idleHeldForAnswer answers the shim held for WS with no session on it and a
// live process, as a copy of its entry.
func (f *Fleet) idleHeldForAnswer(ws ids.WorkspaceID) (*live, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	session, ok := f.sessions[ws]
	if !ok || session.sessionStarted {
		return nil, false
	}
	if !session.held() {
		return nil, false
	}
	held := *session
	return &held, true
}
