package sessionwatcher

import (
	"errors"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// NO WATCH REPLAYS HISTORY (owner ruling, feed paging on demand, 2026-10-02;
// it supersedes the 2026-09-23 rule that replayed the first page on an
// opening). A watch the daemon holds no pointer of opens tail_only, and one it
// holds a pointer of catches up from it; history reaches the views only when a
// reader asks for a page (the feed's ReadHistory). What a new watcher must
// still be told is WHETHER the daemon's views already hold this conversation,
// because that decides which pointers it opens from:
//
//   - They do not when the workspace is being OPENED in this daemon (nothing
//     of it has ever been drawn here) or when a different transcript has just
//     been SELECTED for it (the bind reset the feed). Those two open every
//     watch tail_only, and each has exactly one constructor: WorkspaceOpened
//     and TranscriptSelected.
//   - Every other watcher REPLACES one that was serving the same conversation
//     — a restart, a revival, a rollout's relaunch, a cold gate's re-open — and
//     resumes from the pointers its predecessor was served (ResumeFrom).
//
// The zero Opening is REFUSED by Start: a watcher whose caller did not decide
// would silently open from no pointers, which is exactly the defect this type
// exists to make unrepresentable.

// Pointers is the newest pointer a watcher was served on each of its watches:
// what a successor watcher passes as known_through so its opening pages carry
// only what was written since.
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
// hold none of this conversation, so every watch opens tail_only. It
// has ONE production caller (the fleet's opening decision), and a structural
// test in internal/workspace holds it to that.
func WorkspaceOpened() Opening { return Opening{replay: replayWorkspaceOpened} }

// TranscriptSelected is the replay a transcript SELECTION (`SPC j c`, the
// BindWorkspaceSession verb) earns: the bind reset the feed and the workspace
// now runs a different conversation, so every watch opens tail_only.
// It has ONE production caller, held to it the same way.
func TranscriptSelected() Opening { return Opening{replay: replayTranscriptSelected} }

// ResumeFrom is every other opening: the watcher replaces one that served the
// same conversation, and each watch catches up from the pointer that watcher
// was served. A watch with no pointer was never served an entry, so it opens
// tail_only.
func ResumeFrom(from Pointers) Opening {
	return Opening{resumed: true, from: from.clone()}
}

// Replays reports whether this opening starts from NO pointers: the views hold
// none of this conversation. The name is the record field's; no opening
// replays history any more.
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
