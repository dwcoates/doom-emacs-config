// Package deployprogress is the vocabulary of a DEPLOY'S PROGRESS as the
// footer draws it (owner request, 2026-09-27: deploy feedback moves out of the
// webapp's restarting banner and into the footer's activity line).
//
// THREE PARTIES SHARE ONE SPELLING HERE WITHOUT IMPORTING EACH OTHER. The
// deploy says every phase up to the handover; the rollout controller says a
// handover that could not finish, and — in the SUCCESSOR, whose streams are
// the only ones left once the old daemon's end at the transfer — says
// `updated`; the footer resolver draws whatever it was told on every
// workspace's strip. It is a leaf of its own for the same reason
// internal/bounce is: the parties that ask and the party that draws must not
// drift into two vocabularies for one line.
package deployprogress

import (
	"claude-repld/internal/ids"
)

// Phase is where a deploy has got to.
type Phase int

// The phases, in the order a deploy passes through them. A phase the footer
// derives rather than being told — a workspace WAITING on its own work while
// the daemon moves off it — is not here: it is the footer's own reading of
// the one live-work set, never a fact a second party states.
const (
	// Building is the build into staging.
	Building Phase = iota + 1
	// Installing is the fresh build being installed over the running one.
	Installing
	// RestartingServices is out-of-date services being restarted.
	RestartingServices
	// HandingOver is the daemon moving every workspace to its successor.
	HandingOver
	// Updated is the deploy done. MOMENTARY: the footer retires it itself.
	Updated
)

// Valid reports whether the phase is one of the phases above.
func (p Phase) Valid() bool { return p >= Building && p <= Updated }

// String spells the phase for the records.
func (p Phase) String() string {
	switch p {
	case Building:
		return "building"
	case Installing:
		return "installing"
	case RestartingServices:
		return "restarting_services"
	case HandingOver:
		return "handing_over"
	case Updated:
		return "updated"
	default:
		return "unknown"
	}
}

// Component is one deployable component the line names.
type Component string

// The components the line can name, spelled as the wire's component arms.
const (
	Store   Component = "store"
	Sidecar Component = "sidecar"
	Daemon  Component = "daemon"
	Shim    Component = "shim"
	Webapp  Component = "webapp"
)

// Note is one thing a deploy deferred on one workspace.
type Note string

// The notes.
const (
	// ShimWhenIdle is a stale shim registered behind its workspace's work: it
	// is replaced when the session is idle, because an unforced deploy never
	// ends a turn.
	ShimWhenIdle Note = "shim_when_idle"
)

// Progress is one statement of a deploy's progress, which REPLACES the last.
type Progress struct {
	// Phase is where the deploy has got to. Required.
	Phase Phase
	// Components names what the phase acts on: every component being built
	// for Building, every service being restarted for RestartingServices.
	// Empty for the other phases.
	Components []Component
	// Draining states that every workspace leaves this daemon AT ITS OWN
	// FREENESS (an unforced handover or restart): a workspace with a turn or
	// detached work in flight then draws the waiting phase with its counts
	// instead, and the counts fall as the work drains.
	Draining bool
	// Notes are the deferred-component notes, per workspace. A workspace not
	// named has none.
	Notes map[ids.WorkspaceID][]Note
}

// Sink is the ONE entry point a deploy's progress is published through. The
// footer resolver implements it, and publishes the line on every workspace's
// strip. A nil progress CLEARS the line: the deploy stopped short of a phase
// that ends it (a failure, which the fault line carries, or a handover that
// could not finish).
type Sink interface {
	SetDeployProgress(progress *Progress)
}
