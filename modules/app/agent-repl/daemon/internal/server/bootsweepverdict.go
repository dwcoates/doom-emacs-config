// bootsweepverdict.go is where a boot-sweep verdict STOPS BEING A LOG LINE.
//
// bootsweep.go reaches four different conclusions about sessions it finished
// with and did not wire, and hands each to its Unwired hook. This is what the
// hook is wired to. It fans one verdict out to BOTH surfaces that can carry it,
// because neither alone is the whole account:
//
//   - THE PUSHED STATE (the SSM's controller-less entry point). Without it the
//     workspace's row goes on resting on the anonymous `daemon_restart` cause
//     every surviving session's hibernation carries, so a reader is told the
//     daemon restarted and nothing about THIS session. The session stays
//     hibernated — it is — and the verdict rides the composite's fault axis.
//   - THE HOST ACTION (retained, delivered, acknowledged). It is the only
//     surface that puts a sentence in front of the person: the pushed state is
//     a classification, and a classification nobody is looking at when it
//     changes is still, in practice, silence.
//
// BOTH HALVES ALWAYS RUN, AND NEITHER FAILURE HIDES THE OTHER. A verdict that
// reached one surface and not the other is a partial account and is reported as
// one: the errors are joined, never short-circuited and never swallowed, and
// the sweep logs the joined result against the session and the verdict.
package server

import (
	"context"
	"errors"
	"fmt"
)

// BootSweepStateSink attaches a verdict to the workspace's pushed state. It is
// satisfied by *ssm.Manager's ApplyBootSweepVerdict.
type BootSweepStateSink interface {
	ApplyBootSweepVerdict(workspace, sessionID, verdict string) error
}

// BootSweepHostSink hands a verdict to the Emacs host as a retained action. It
// is satisfied by the workspace-create manager's SurfaceBootSweepVerdict.
type BootSweepHostSink interface {
	SurfaceBootSweepVerdict(ctx context.Context, workspace, sessionID, verdict, reason string) error
}

// BootSweepVerdictRouter routes one classified verdict to every surface that
// carries it. Construct it with every field set: an unset sink is a verdict
// that reaches half the user, which is the failure this whole path exists to
// end, so it is refused at the door rather than degraded silently.
type BootSweepVerdictRouter struct {
	State BootSweepStateSink
	Host  BootSweepHostSink
	// Ctx bounds the host delivery. Nil uses context.Background(), because a
	// verdict outlives the sweep that produced it: the action is retained and
	// redelivered, so cancelling the enqueue would discard the account rather
	// than defer it.
	Ctx  context.Context
	Logf func(string, ...any)
}

// bootSweepVerdictReasons is the display-ready sentence each verdict is
// rendered as, composed HERE because the sweep is the only thing that knows
// what its own branches mean. HostBootSweepSessionUnwired.reason is rendered
// verbatim by the host, so these are whole sentences addressed to a person, not
// tokens.
var bootSweepVerdictReasons = map[string]string{
	BootSweepUnwiredNoLiveShim: "its agent process is gone — neither a connection nor a session lock was found, " +
		"so nothing is driving it until the workspace is opened again",
	BootSweepUnwiredLockHeldWithoutConnection: "a live agent process still holds its session lock but never reconnected " +
		"within the redial window, so no duplicate was started and the session is not being driven",
	BootSweepUnwiredProbeFailed: "the daemon could not tell whether its agent process is connected — the connection " +
		"probe failed twice — so the session was left alone rather than guessed about",
	BootSweepUnwiredLockProbeFailed: "the daemon could not tell whether its agent process is alive — the session-lock " +
		"probe failed — so the session was left alone rather than guessed about",
}

// BootSweepVerdictReason returns the display-ready sentence for verdict.
//
// THE VERDICT TOKEN RIDES THE SENTENCE because HostBootSweepSessionUnwired
// carries no separate field for it and its contract asks for "which verdict
// fired, with its evidence". A user reading the sentence gets the evidence; a
// user grepping a log for the sweep's own line gets the token that joins them.
//
// An unknown verdict is an ERROR, never a substituted generic sentence: a new
// branch of the sweep that nobody taught this map about must be loud at the
// first verdict it produces, not quietly rendered as one of the four that
// already exist.
func BootSweepVerdictReason(verdict string) (string, error) {
	reason, ok := bootSweepVerdictReasons[verdict]
	if !ok {
		return "", fmt.Errorf("server: boot-sweep verdict %q has no display reason; every BootSweepUnwired* constant needs one", verdict)
	}
	return fmt.Sprintf("%s (boot-sweep verdict %s)", reason, verdict), nil
}

// Route is the BootSweeper.Unwired hook. It composes the verdict's sentence and
// drives both surfaces, returning the joined outcome.
func (r *BootSweepVerdictRouter) Route(workspace, sessionID, verdict string) error {
	logf := r.Logf
	if logf == nil {
		return fmt.Errorf("server: boot-sweep verdict router for ws %s session %s verdict %s has no logger; a verdict routed without an account of the routing is the silence this path exists to end",
			workspace, sessionID, verdict)
	}
	logf("server: boot-sweep verdict routing ws=%s session=%s verdict=%s branch=enter", workspace, sessionID, verdict)
	if r.State == nil || r.Host == nil {
		err := fmt.Errorf("server: boot-sweep verdict router is incomplete (state_sink=%t host_sink=%t); ws=%s session=%s verdict=%s reaches only part of the user",
			r.State != nil, r.Host != nil, workspace, sessionID, verdict)
		logf("server: boot-sweep verdict routing REJECTED ws=%s session=%s verdict=%s branch=incomplete-router error=%v", workspace, sessionID, verdict, err)
		return err
	}
	reason, err := BootSweepVerdictReason(verdict)
	if err != nil {
		logf("server: boot-sweep verdict routing REJECTED ws=%s session=%s verdict=%s branch=unknown-verdict error=%v", workspace, sessionID, verdict, err)
		return err
	}

	var failures []error
	if err := r.State.ApplyBootSweepVerdict(workspace, sessionID, verdict); err != nil {
		err = fmt.Errorf("server: boot-sweep verdict %s for ws %s session %s did not reach the pushed workspace state: %w",
			verdict, workspace, sessionID, err)
		logf("server: boot-sweep verdict routing ws=%s session=%s verdict=%s branch=state-sink-failed error=%v", workspace, sessionID, verdict, err)
		failures = append(failures, err)
	} else {
		logf("server: boot-sweep verdict routing ws=%s session=%s verdict=%s branch=state-sink-applied — the workspace's pushed state now names this conclusion instead of the anonymous restart cause",
			workspace, sessionID, verdict)
	}

	ctx := r.Ctx
	if ctx == nil {
		ctx = context.Background()
	}
	if err := r.Host.SurfaceBootSweepVerdict(ctx, workspace, sessionID, verdict, reason); err != nil {
		err = fmt.Errorf("server: boot-sweep verdict %s for ws %s session %s did not reach the host: %w",
			verdict, workspace, sessionID, err)
		logf("server: boot-sweep verdict routing ws=%s session=%s verdict=%s branch=host-sink-failed error=%v", workspace, sessionID, verdict, err)
		failures = append(failures, err)
	} else {
		logf("server: boot-sweep verdict routing ws=%s session=%s verdict=%s branch=host-sink-surfaced reason=%q", workspace, sessionID, verdict, reason)
	}
	return errors.Join(failures...)
}
