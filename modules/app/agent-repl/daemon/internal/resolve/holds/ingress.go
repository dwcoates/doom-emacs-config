package holds

import (
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
)

// THE INGRESS PROMPTS (frontend.v1 HeldIngressPrompt): prompts an editor wrote
// to the held-prompt ingress while no daemon answered it, which this daemon
// has read and its queue has not accepted yet. The ingress (internal/
// heldingress) states them; the feed resolver draws them at the held section's
// tail until each settles into a held prompt or a prompt row.

// IngressWaitKind is what an ingress entry waits on: the arm its card's badge
// stands for.
type IngressWaitKind int

// The kinds an ingress entry can wait on.
const (
	// IngressAwaitingIntake: nothing refused the entry; it is not yet offered
	// to the queue, waits behind an earlier entry, or waits for its retry.
	IngressAwaitingIntake IngressWaitKind = iota
	// IngressNoSession: the queue refused it for want of a session.
	IngressNoSession
	// IngressMerge: the queue refused it while a merge is in flight.
	IngressMerge
	// IngressColdGate: the queue refused it at the session's cold gate.
	IngressColdGate
	// IngressMovedAway: the workspace is moving to another daemon.
	IngressMovedAway
	// IngressRefused: the queue refused it by a fault.
	IngressRefused
)

// IngressWait is what an ingress entry waits on, with the sentence that says
// it in full, empty when the badge's words say it all.
type IngressWait struct {
	Kind   IngressWaitKind
	Detail string
}

// IngressEntry is one ingress entry the queue has not accepted.
type IngressEntry struct {
	// Key is the entry's idempotency key: its identity until it is accepted.
	Key string
	// Said is what the user said, whole.
	Said *conversationv1.UserSaid
	// QueuedAt is when the editor wrote the entry.
	QueuedAt time.Time
	// Wait is what the entry waits on.
	Wait IngressWait
}

// IngressSink is what the held-prompt ingress tells the held section. The feed
// resolver implements it.
type IngressSink interface {
	// SetIngressPending installs a workspace's unaccepted ingress entries,
	// whole, in the order they were written. The ingress calls it after every
	// sweep that read an entry of the workspace, or that read none after one
	// that did.
	SetIngressPending(ws ids.WorkspaceID, entries []IngressEntry)
	// ClaimIngress names the turn the entry KEY is being submitted as, BEFORE
	// the queue sees the submission. The section drops the entry in the same
	// publication that draws that turn as a held prompt or a prompt row. The
	// prompt handler calls it for every re-driven submission.
	ClaimIngress(ws ids.WorkspaceID, key string, turn ids.TurnID)
	// DropIngress removes the entry KEY at once: the queue answered it
	// (accepted, or already had it), so it waits on nothing.
	DropIngress(ws ids.WorkspaceID, key string)
}

// IngressPrompt projects one ingress entry onto its card. The wait kinds are
// a closed set this package declares, so a kind outside it is a defect, and it
// panics rather than draw a badge that says something false.
func IngressPrompt(e IngressEntry) *frontendv1.HeldIngressPrompt {
	badge := &frontendv1.HeldIngressPromptBadge{}
	switch e.Wait.Kind {
	case IngressAwaitingIntake:
		badge.Label = "waiting to be queued"
		badge.StandsFor = &frontendv1.HeldIngressPromptBadge_AwaitingIntake{AwaitingIntake: &frontendv1.HeldIngressPromptBadgeAwaitingIntake{}}
	case IngressNoSession:
		badge.Label = "waiting for the session"
		badge.StandsFor = &frontendv1.HeldIngressPromptBadge_NoSession{NoSession: &frontendv1.HeldIngressPromptBadgeNoSession{}}
	case IngressMerge:
		badge.Label = "merge running"
		badge.StandsFor = &frontendv1.HeldIngressPromptBadge_Merge{Merge: &frontendv1.HeldIngressPromptBadgeMerge{}}
	case IngressColdGate:
		badge.Label = "cold gate"
		badge.StandsFor = &frontendv1.HeldIngressPromptBadge_ColdGate{ColdGate: &frontendv1.HeldIngressPromptBadgeColdGate{}}
	case IngressMovedAway:
		badge.Label = "workspace moving"
		badge.StandsFor = &frontendv1.HeldIngressPromptBadge_MovedAway{MovedAway: &frontendv1.HeldIngressPromptBadgeMovedAway{}}
	case IngressRefused:
		badge.Label = "not yet queued"
		badge.StandsFor = &frontendv1.HeldIngressPromptBadge_Refused{Refused: &frontendv1.HeldIngressPromptBadgeRefused{}}
	default:
		panic(fmt.Sprintf("holds: ingress entry %q waits on kind %d, which has no badge", e.Key, int(e.Wait.Kind)))
	}
	if e.Wait.Detail != "" {
		detail := e.Wait.Detail
		badge.Detail = &detail
	}
	return &frontendv1.HeldIngressPrompt{
		Said:     e.Said,
		QueuedAt: &frontendv1.HeldIngressPromptQueuedAt{AtMs: e.QueuedAt.UnixMilli()},
		Badge:    badge,
	}
}
