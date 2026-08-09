// bootsweepverdict.go is THE CONTROLLER-LESS ENTRY POINT into the composite
// state: the one way a verdict reached by something that is NOT a session
// controller can reach a workspace's pushed state.
//
// WHY IT HAS TO EXIST AT ALL. Every other writer on this file's two axes is a
// live session controller speaking for its own generation, and the model is
// built on exactly that: a connectivity edge is a CLAIM ("this generation can
// or cannot operate the session now"), and a runtime fault is scoped to the
// generation that observed it. The boot sweep is neither. It walks the registry
// once, decides a surviving session is not wired, and makes no claim on the
// session at all — it does not bring it up, does not own it, and deliberately
// does not want to.
//
// So both existing doors are correctly shut to it:
//
//   - ApplySessionConnectivity is refused by validateConnectivityTransition.
//     At successor boot the session sits `hibernated` under the PREDECESSOR's
//     generation; the same generation cannot move ("hibernated generation is
//     retired") and a new one must enter `connecting` first — which is a
//     bring-up claim the sweep did not make and must not fake.
//   - ApplyRuntimeFault is refused by its stale-controller guard, because a
//     hibernated workspace has no current generation for a fault to belong to.
//
// THE CARVE-OUT, STATED AS AN INVARIANT. A boot-sweep verdict PREDATES EVERY
// GENERATION BY CONSTRUCTION: the sweep runs once per daemon boot, strictly
// before anything has brought the session up, and it concludes about a session
// no generation owns. It may therefore attach to a generation-less workspace,
// and only to one:
//
//  1. the workspace's current lifecycle row must be `hibernated` — a live or
//     connecting generation owns its own session and this door is shut;
//  2. the row must name the session the verdict is about;
//  3. the fault attaches to the identity of that hibernated row, so the next
//     bring-up (a new generation entering `connecting`) stops resolving it
//     without anything having to close it — the verdict SELF-RETIRES the
//     instant the situation it described stops being true;
//  4. re-attaching the identical standing verdict is a no-op, which is what
//     makes "exactly once per boot" hold across a daemon restarted twice with
//     no bring-up in between.
//
// NOTHING HERE WEAKENS THE GENERAL REFUSALS. validateConnectivityTransition is
// untouched and ApplyRuntimeFault still refuses every generation-less fault:
// this is a separate, named, boot-scoped door, not a hole in either of theirs.
package ssm

import (
	"errors"
	"fmt"
)

// BootSweepFaultComponent is the component every boot-sweep verdict is filed
// under. It is the discriminator resolveComposite uses to read THIS fault and
// no other on a hibernated workspace, so it is a constant rather than a string
// spelled at each site.
const BootSweepFaultComponent = "boot-sweep"

var (
	// ErrBootSweepVerdictNoLifecycle reports a verdict about a workspace the
	// SSM has never recorded a lifecycle row for. Its pushed state is the
	// generation-less hibernated fallback, which resolves no composite, so the
	// verdict has nowhere to land and saying so is the only honest answer.
	ErrBootSweepVerdictNoLifecycle = errors.New("ssm: boot-sweep verdict has no session-connectivity row to attach to")
	// ErrBootSweepVerdictLiveGeneration reports a verdict aimed at a workspace
	// whose current lifecycle row is NOT hibernated. A generation that owns its
	// session speaks for itself; the boot-scoped door is shut to anything else.
	ErrBootSweepVerdictLiveGeneration = errors.New("ssm: boot-sweep verdict cannot attach to a workspace with a current controller generation")
)

// ApplyBootSweepVerdict attaches one boot-sweep verdict to a workspace no
// controller generation owns, WITHOUT claiming a generation and WITHOUT moving
// the connectivity axis. The session stays `hibernated`, which is the truth;
// the verdict rides the composite's fault axis, so the pushed WorkspaceState
// names which conclusion the sweep reached instead of resting on the anonymous
// `daemon_restart` cause its predecessor's hibernation wrote.
//
// verdict is one of the server package's BootSweepUnwired* constants. It is
// both the fault type and the cause kind: the fault type is what makes two
// different verdicts two different windows, and the cause kind is what the
// frontend renders.
func (m *Manager) ApplyBootSweepVerdict(workspace, sessionID, verdict string) error {
	m.logf("ssm: boot-sweep verdict ws=%q session=%q verdict=%q branch=enter", workspace, sessionID, verdict)
	switch {
	case workspace == "":
		err := fmt.Errorf("ssm: boot-sweep verdict got an empty workspace")
		m.logf("ssm: boot-sweep verdict REJECTED ws=%q session=%q verdict=%q branch=validation error=%q", workspace, sessionID, verdict, err)
		return err
	case sessionID == "":
		err := fmt.Errorf("ssm: boot-sweep verdict for workspace %q got an empty agent-repl session id", workspace)
		m.logf("ssm: boot-sweep verdict REJECTED ws=%q session=%q verdict=%q branch=validation error=%q", workspace, sessionID, verdict, err)
		return err
	case verdict == "":
		err := fmt.Errorf("ssm: boot-sweep verdict for workspace %q session %q got an empty verdict", workspace, sessionID)
		m.logf("ssm: boot-sweep verdict REJECTED ws=%q session=%q verdict=%q branch=validation error=%q", workspace, sessionID, verdict, err)
		return err
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	tx, err := m.db.Begin()
	if err != nil {
		err = fmt.Errorf("ssm: begin boot-sweep verdict ws=%q session=%q verdict=%q: %w", workspace, sessionID, verdict, err)
		m.logf("ssm: boot-sweep verdict ERROR ws=%q session=%q verdict=%q branch=begin error=%q", workspace, sessionID, verdict, err)
		return err
	}
	defer tx.Rollback()

	lifecycle, err := latestConnectivity(tx, workspace)
	if err != nil {
		err = fmt.Errorf("ssm: read current connectivity for boot-sweep verdict ws=%q session=%q verdict=%q: %w", workspace, sessionID, verdict, err)
		m.logf("ssm: boot-sweep verdict ERROR ws=%q session=%q verdict=%q branch=read-connectivity error=%q", workspace, sessionID, verdict, err)
		return err
	}
	if !lifecycle.found {
		err := fmt.Errorf("%w: workspace=%q session=%q verdict=%q", ErrBootSweepVerdictNoLifecycle, workspace, sessionID, verdict)
		m.logf("ssm: boot-sweep verdict REJECTED ws=%q session=%q verdict=%q branch=no-lifecycle error=%q", workspace, sessionID, verdict, err)
		return err
	}
	if lifecycle.state != SessionConnectivityHibernated {
		err := fmt.Errorf("%w: workspace=%q current_session=%q current_generation=%q current_connectivity=%q verdict_session=%q verdict=%q",
			ErrBootSweepVerdictLiveGeneration, workspace, lifecycle.sessionID, lifecycle.generationID, lifecycle.state, sessionID, verdict)
		m.logf("ssm: boot-sweep verdict REJECTED ws=%q session=%q verdict=%q current_connectivity=%q branch=live-generation error=%q",
			workspace, sessionID, verdict, lifecycle.state, err)
		return err
	}
	if lifecycle.sessionID != sessionID {
		err := fmt.Errorf("%w: workspace=%q current_session=%q current_generation=%q verdict_session=%q verdict=%q",
			ErrStaleControllerGeneration, workspace, lifecycle.sessionID, lifecycle.generationID, sessionID, verdict)
		m.logf("ssm: boot-sweep verdict REJECTED ws=%q session=%q verdict=%q current_session=%q branch=other-session error=%q",
			workspace, sessionID, verdict, lifecycle.sessionID, err)
		return err
	}

	wasOpen, priorImpact, err := faultWindowTop(tx, workspace, lifecycle.generationID, BootSweepFaultComponent, verdict)
	if err != nil {
		err = fmt.Errorf("ssm: read boot-sweep verdict window ws=%q session=%q generation=%q verdict=%q: %w",
			workspace, sessionID, lifecycle.generationID, verdict, err)
		m.logf("ssm: boot-sweep verdict ERROR ws=%q session=%q verdict=%q branch=read-window error=%q", workspace, sessionID, verdict, err)
		return err
	}
	if wasOpen {
		if priorImpact != FaultImpactConnectivity {
			err := fmt.Errorf("ssm: boot-sweep verdict window has impact %q, want %q: workspace=%q generation=%q verdict=%q",
				priorImpact, FaultImpactConnectivity, workspace, lifecycle.generationID, verdict)
			m.logf("ssm: boot-sweep verdict ERROR ws=%q session=%q verdict=%q prior_impact=%q branch=impact-mismatch error=%q",
				workspace, sessionID, verdict, priorImpact, err)
			return err
		}
		// ALREADY STANDING, so this boot has nothing to add. The situation the
		// verdict describes has not changed — the same session is still on the
		// same retired generation with the same conclusion against it — and
		// re-appending would be a second window for one fact.
		m.logf("ssm: boot-sweep verdict ws=%q session=%q generation=%q verdict=%q branch=already-standing — the verdict from an earlier boot is still attached to this retired generation, so nothing is appended",
			workspace, sessionID, lifecycle.generationID, verdict)
		return nil
	}

	at := m.nextAt()
	if _, err := tx.Exec(
		`INSERT INTO session_fault(
			workspace, agent_repl_session_id, controller_generation_id,
			component, fault_type, impact, open, cause_kind, at
		) VALUES (?,?,?,?,?,?,?,?,?)`,
		workspace, sessionID, string(lifecycle.generationID),
		BootSweepFaultComponent, verdict, string(FaultImpactConnectivity), boolInt(true), verdict, at,
	); err != nil {
		err = fmt.Errorf("ssm: append boot-sweep verdict ws=%q session=%q generation=%q verdict=%q at=%d: %w",
			workspace, sessionID, lifecycle.generationID, verdict, at, err)
		m.logf("ssm: boot-sweep verdict ERROR ws=%q session=%q verdict=%q at=%d branch=append error=%q", workspace, sessionID, verdict, at, err)
		return err
	}
	if err := tx.Commit(); err != nil {
		err = fmt.Errorf("ssm: commit boot-sweep verdict ws=%q session=%q generation=%q verdict=%q at=%d: %w",
			workspace, sessionID, lifecycle.generationID, verdict, at, err)
		m.logf("ssm: boot-sweep verdict ERROR ws=%q session=%q verdict=%q at=%d branch=commit error=%q", workspace, sessionID, verdict, at, err)
		return err
	}
	m.logf("ssm: boot-sweep verdict ws=%q session=%q generation=%q verdict=%q connectivity=%q at=%d branch=applied — the session stays hibernated (it is) and the pushed state now names the conclusion the sweep reached",
		workspace, sessionID, lifecycle.generationID, verdict, lifecycle.state, at)
	return m.publishCompositeLocked(workspace, verdict)
}
