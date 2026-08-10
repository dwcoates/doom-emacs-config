// openfailure.go carries the ONE thing an EARLY-ACKED open still owes its user.
//
// An `open_workspace' command used to be acked only once the workspace's whole
// bring-up had finished, so a bring-up failure had a natural place to land: the
// nack. That coupling is what collapsed the frontend command pipeline — a
// bring-up costs seconds, the editor re-sends an unacked open every few
// seconds, and arrivals outran the service rate permanently — so the ack now
// means ACCEPTED and the bring-up runs behind it (server/openbringup.go).
//
// Moving the wait off the command MUST NOT move the failure off the user. The
// bring-up ladder already publishes most failures itself: a resolved bring-up
// failure pushes a start-failed card and closes the connectivity axis
// (bringupescape.go, resolveStartFailed). This entry point exists for the
// outcomes the opener classifies AFTER that ladder returns — a continuity
// verdict on an exact resume, a deadline the opener itself bounds — which the
// ladder never sees and which, before the early ack, only the nack reported.
//
// It pushes the card under the SAME stable identity the ladder uses, so a
// failure that both surfaces describe is ONE card the second write updates,
// never two accounts of one failure.
//
// IT DOES NOT TOUCH THE CONNECTIVITY AXIS, deliberately. That axis is the
// bring-up ladder's to close, its edges are validated transitions, and a second
// `unavailable' edge for a failure the ladder already resolved would be
// rejected by the state machine and logged as a rejection — noise describing a
// duplicate, not a fact anyone is missing.
package sessioncontroller

import (
	"errors"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
)

// RecordOpenFailure publishes a bring-up failure that arrived after its
// `open_workspace' command was already acked.
//
// A HIBERNATED session is not a failure and is refused here rather than
// carded: the revival gate is an expected outcome with its own pushed
// SessionView, and a failure card for it would show a continuity error for a
// session that is merely asleep. Callers are expected to filter it too; this
// is the backstop, and it is loud.
func (m *Manager) RecordOpenFailure(workspace string, err error) {
	if workspace == "" || err == nil {
		m.errorf("session-controller: RecordOpenFailure called with workspace=%q err=%v — an open failure with no workspace or no cause names nothing and cannot be published",
			workspace, err)
		return
	}
	if errors.Is(err, errclass.ErrSessionHibernated) {
		m.logf("session-controller: open bring-up for ws=%q ended at the revival gate, not in failure; no failure card is published for a session that is merely asleep",
			workspace)
		return
	}
	m.mu.Lock()
	d := m.byWS[workspace]
	m.mu.Unlock()
	if d == nil || d.consumer == nil {
		if errors.Is(err, errclass.ErrResumeTargetVanished) {
			// NOT a fresh failure: the vanished-resume fence (vanishedresume.go)
			// already carded this session's terminal refusal once, loudly, at the
			// bring-up that established it — and, since that card is PERSISTED,
			// the account stands for every later reader too rather than only for
			// whoever was connected at that instant. Every open attempt against a fenced
			// session refuses BEFORE any controller exists, so it lands here on
			// every retry — an open never stops being retried by its caller — and
			// logging each arrival at ERROR would repeat one already-reported fact
			// for as long as the fence stands. The open caller still deserves to
			// know why nothing was published here, so the FIRST arrival per
			// session per boot says so at WARN; every later one says the same
			// thing at DEBUG, naming the fence either way.
			m.recordFencedOpenRefusal(workspace, err)
			return
		}
		// LOUD, not silent: the failure is real and the user will not see this
		// one. It happens when the bring-up tore its own controller down, in
		// which case the ladder's own card already stands — but the daemon
		// cannot prove that from here, so it says what it could not publish.
		m.errorf("session-controller: open bring-up FAILED ws=%q with no live controller to publish a failure card onto; the cause is recorded here only: %v",
			workspace, err)
		return
	}
	m.logf("session-controller: open bring-up FAILED ws=%q session=%s generation=%s after the open was acked; publishing a failure card: %v",
		workspace, d.sessionID, d.generationID, err)
	d.consumer.pushFailure(d.consumer.startFailedUUID(), openFailureCard(m.logf, err))
}

// recordFencedOpenRefusal reports an open attempt whose bring-up failure IS the
// vanished-resume fence's own terminal refusal, arriving here because that
// refusal happens before any controller exists (see vanishedresume.go). The
// FIRST such arrival per session per boot is WARN, so an operator watching
// this workspace still learns once that its opens are being refused by the
// fence; every later arrival this boot is DEBUG, because the fact was already
// reported and the fence's own bring-up-time warnf already carded it.
//
// "Per boot" falls out of openFenceRefusals starting empty in New: a
// restarted daemon re-derives the fence from disk on its first bring-up and
// this map re-warns once for the new boot, same as the fence itself does.
func (m *Manager) recordFencedOpenRefusal(workspace string, err error) {
	m.mu.Lock()
	first := !m.openFenceRefusals[workspace]
	m.openFenceRefusals[workspace] = true
	m.mu.Unlock()
	if first {
		m.warnf("session-controller: open bring-up ws=%q refused by the vanished-resume fence; no live controller exists to publish a failure card onto, but the fence already published this session's terminal failure card once — later refusals against this fence are logged at debug: %v",
			workspace, err)
		return
	}
	dlog.Tag(dlog.Logf(m.logf), "level", string(dlog.LevelDebug))(
		"session-controller: open bring-up ws=%q refused by the vanished-resume fence again; already reported at warn this boot: %v",
		workspace, err)
}

// openFailureCard renders a late open failure WITHOUT degrading what the nack
// used to carry.
//
// A failed exact resume holds TYPED continuity evidence — which conversation,
// which config root, which transcript was searched for — that the command
// classifier turns into a session_resume_failed card the client renders as a
// continuity error. Flattening that to prose would be a real loss of error
// detail, so it is classified whenever it is there.
//
// Everything else becomes a start-failed card rather than going through the
// general classifier, because the general classifier's unknown-error arm logs a
// LOUD fallthrough for a gap in its vocabulary — and a bring-up that simply did
// not come up is not such a gap. It is the same card the ladder publishes for
// the same event, under the same identity.
func openFailureCard(logf func(string, ...any), err error) *frontendv1.FailureCardView {
	var resume errclass.SessionResumeFailureDetailer
	if errors.As(err, &resume) {
		return errclass.Command(logf, err)
	}
	return errclass.StartFailed(err.Error())
}
