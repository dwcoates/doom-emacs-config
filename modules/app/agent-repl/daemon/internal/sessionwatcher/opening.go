package sessionwatcher

import (
	"errors"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// HISTORY IS REPLAYED ONLY WHEN A WORKSPACE IS OPENED OR A TRANSCRIPT IS
// SELECTED, AND ONLY THE FIRST PAGE (owner rule, 2026-09-23).
//
// A watch that is opened without a known_through pointer is answered with the
// agent's FIRST PAGE: the newest 200 entries of its book, every one of them
// re-resolved by the feed, the footer and the roster. On a feed that already
// holds those rows that is not a no-op — the history effects fire again
// (`clear_confirmed`, a re-applied `delivery_bound_moved`,
// `subagent_without_start`, a burst of `detached_unknown_unit`) and the webapp
// redraws ten to fifteen times. So the only question a new watcher has to be
// told is WHETHER the daemon's views already hold this conversation:
//
//   - They do not when the workspace is being OPENED in this daemon (nothing
//     of it has ever been drawn here) or when a different transcript has just
//     been SELECTED for it (the bind reset the feed). Those are the two replays,
//     and each has exactly one constructor: WorkspaceOpened and
//     TranscriptSelected.
//   - Every other watcher REPLACES one that was serving the same conversation
//     — a restart, a revival, a rollout's relaunch, a cold gate's re-open — and
//     resumes from the pointers its predecessor was served (ResumeFrom).
//
// The zero Opening is REFUSED by Start: a watcher whose caller did not decide
// would silently replay, which is exactly the defect this type exists to make
// unrepresentable.

// Pointers is the newest pointer a watcher was served on each of its watches:
// what a successor watcher passes as known_through so its opening pages are
// catch-ups rather than repaints.
type Pointers struct {
	// Main is the MAIN agent's watch's pointer, nil when that watch was never
	// served an entry.
	Main *conversationv1.HistoryPointer
	// Agents is every other watched agent's pointer, keyed by AgentId.value.
	// An agent with no key was never served an entry.
	Agents map[string]*conversationv1.HistoryPointer
}

// replayCause names why a watcher serves first pages. Empty is a resume.
type replayCause string

const (
	// replayWorkspaceOpened is the workspace's first opening in this daemon.
	replayWorkspaceOpened replayCause = "workspace_opened"
	// replayTranscriptSelected is a transcript selected for the workspace.
	replayTranscriptSelected replayCause = "transcript_selected"
)

// Opening is how a new watcher's watches begin: a REPLAY of the first page, or
// a RESUME from its predecessor's pointers. Build one with WorkspaceOpened,
// TranscriptSelected or ResumeFrom; the zero value is refused.
type Opening struct {
	replay  replayCause
	resumed bool
	from    Pointers
}

// WorkspaceOpened is the replay a workspace's OPENING earns: the daemon's views
// hold none of this conversation, so every watch opens on its first page. It
// has ONE production caller (the fleet's opening decision), and a structural
// test in internal/workspace holds it to that.
func WorkspaceOpened() Opening { return Opening{replay: replayWorkspaceOpened} }

// TranscriptSelected is the replay a transcript SELECTION (`SPC j c`, the
// BindWorkspaceSession verb) earns: the bind reset the feed and the workspace
// now runs a different conversation, so every watch opens on its first page.
// It has ONE production caller, held to it the same way.
func TranscriptSelected() Opening { return Opening{replay: replayTranscriptSelected} }

// ResumeFrom is every other opening: the watcher replaces one that served the
// same conversation, and each watch catches up from the pointer that watcher
// was served. A watch with no pointer was never served an entry, so its first
// page carries nothing the views already hold.
func ResumeFrom(from Pointers) Opening {
	return Opening{resumed: true, from: from.clone()}
}

// Replays reports whether this opening serves first pages.
func (o Opening) Replays() bool { return o.replay != "" }

// From is the pointers a resume starts from; a replay starts from none.
func (o Opening) From() Pointers { return o.from.clone() }

// String names the opening for a log record.
func (o Opening) String() string {
	if o.replay != "" {
		return string(o.replay)
	}
	if o.resumed {
		return "resumed"
	}
	return "undecided"
}

// errUndecidedOpening is Start's refusal of a zero Opening.
var errUndecidedOpening = errors.New("sessionwatcher: the opening is undecided; a watcher must be told whether it replays (WorkspaceOpened, TranscriptSelected) or resumes (ResumeFrom)")

// validate refuses the zero Opening.
func (o Opening) validate() error {
	if o.replay == "" && !o.resumed {
		return errUndecidedOpening
	}
	return nil
}

// clone copies the pointer map so a caller's later writes never reach a
// watcher's own state, and so a watcher's snapshot never aliases its map.
func (p Pointers) clone() Pointers {
	out := Pointers{Main: p.Main, Agents: make(map[string]*conversationv1.HistoryPointer, len(p.Agents))}
	for id, ptr := range p.Agents {
		if ptr != nil {
			out.Agents[id] = ptr
		}
	}
	return out
}
