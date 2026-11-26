// Package children is the children tracker: the ONE reader of which
// workspaces are a parent's children and how each stands. From the registry
// the roster draws (sidebar.WithRegistrySink), each child's status arm and its
// canonical color (sidebar.WithArmSink) and each child's summary
// (wssummary.Keeper), it draws a child in three places, all from the same
// facts:
//
//   - its bubble in its parent's root feed (frontend.v1.FeedChild), one row
//     per child, re-published whole whenever anything it draws changes;
//   - the parent's footer ⇄ chip and panel while it is OPEN
//     (footer.SetOpenChildren);
//   - the parent's summary menu once it is NOT open
//     (topbar.SetClosedChildren), the one that stopped being open most
//     recently first.
//
// A CHILD IS DRAWN IN EXACTLY ONE OF THE FOOTER AND THE MENU, decided by one
// predicate (open), so the two can never both list it nor both miss it. A
// child whose workspace the daemon removed keeps its bubble, its name no
// longer a door, and leaves the menu.
package children

import (
	"slices"
	"strings"
	"sync"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dirpath"
	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/textline"
	"agent-repld/internal/wsm"
)

// op is the operation every record of this package carries.
const op = "daemon.children"

// Glyph is the kind glyph every child bubble's head carries.
const Glyph = "⇄"

// Feed is where a child's bubble is drawn.
type Feed interface {
	UpsertDurable(ws ids.WorkspaceID, feed feedid.Feed, row *frontendv1.FeedRow)
}

// Footer takes a parent's open children.
type Footer interface {
	SetOpenChildren(parent ids.WorkspaceID, children []footer.OpenChild)
}

// Topbar takes a parent's children that are not open.
type Topbar interface {
	SetClosedChildren(parent ids.WorkspaceID, children []topbar.ClosedChild)
}

// Deps are the tracker's collaborators.
type Deps struct {
	Feed   Feed
	Footer Footer
	Topbar Topbar
	// Home abbreviates a landing target's checkout to `~/...`.
	Home string
	// Now stamps a child's removal: the registry that no longer carries it.
	Now func() time.Time
	Log dlog.Logger
}

// arm is a child's published status arm and its canonical color.
type arm struct {
	name, tone string
}

// Tracker is the children tracker.
type Tracker struct {
	deps Deps

	mu sync.Mutex
	// records are every workspace of the last registry, and every child
	// removed since, as it last stood.
	records map[ids.WorkspaceID]wsm.Workspace
	// repos are the last registry's repositories.
	repos map[wsm.RepoID]wsm.Repository
	// removed are the children the registry stopped carrying, and when.
	removed map[ids.WorkspaceID]time.Time
	// summaries are each workspace's summary.
	summaries map[ids.WorkspaceID]wsm.WorkspaceSummary
	// arms are each workspace's status arm.
	arms map[ids.WorkspaceID]arm
	// drawn is each child's bubble as last published.
	drawn map[ids.WorkspaceID]*frontendv1.FeedChild
	// open and closed are each parent's footer and menu lists as last
	// handed, so an unchanged list is not handed again.
	open   map[ids.WorkspaceID][]footer.OpenChild
	closed map[ids.WorkspaceID][]topbar.ClosedChild
}

// New builds the tracker.
func New(deps Deps) *Tracker {
	if deps.Feed == nil || deps.Footer == nil || deps.Topbar == nil || deps.Now == nil || deps.Log == nil {
		panic("children: New requires Feed, Footer, Topbar, Now and Log")
	}
	return &Tracker{deps: deps,
		records: map[ids.WorkspaceID]wsm.Workspace{}, repos: map[wsm.RepoID]wsm.Repository{},
		removed: map[ids.WorkspaceID]time.Time{}, summaries: map[ids.WorkspaceID]wsm.WorkspaceSummary{},
		arms: map[ids.WorkspaceID]arm{}, drawn: map[ids.WorkspaceID]*frontendv1.FeedChild{},
		open: map[ids.WorkspaceID][]footer.OpenChild{}, closed: map[ids.WorkspaceID][]topbar.ClosedChild{}}
}

// SetRegistry takes the registry the roster just installed
// (sidebar.RegistrySink). A child it no longer carries was removed now.
func (t *Tracker) SetRegistry(reg sidebar.Registry) {
	t.mu.Lock()
	defer t.mu.Unlock()
	seen := map[ids.WorkspaceID]bool{}
	parents := map[ids.WorkspaceID]bool{}
	for _, ws := range reg.Workspaces {
		seen[ws.ID] = true
		if prev, ok := t.records[ws.ID]; ok && prev.Parent != nil && (ws.Parent == nil || *ws.Parent != *prev.Parent) {
			parents[*prev.Parent] = true
		}
		t.records[ws.ID] = ws
		delete(t.removed, ws.ID)
		if ws.Parent != nil {
			parents[*ws.Parent] = true
		}
	}
	for id, ws := range t.records {
		if seen[id] {
			continue
		}
		if ws.Parent == nil {
			delete(t.records, id)
			continue
		}
		if _, gone := t.removed[id]; !gone {
			t.removed[id] = t.deps.Now()
			parents[*ws.Parent] = true
		}
	}
	t.repos = map[wsm.RepoID]wsm.Repository{}
	for _, repo := range reg.Repositories {
		t.repos[repo.ID] = repo
	}
	for parent := range parents {
		t.redrawLocked(parent)
	}
}

// SetArm takes WS's status arm move (sidebar.ArmSink).
func (t *Tracker) SetArm(ws ids.WorkspaceID, name, tone string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.arms[ws] = arm{name: name, tone: tone}
	t.redrawParentLocked(ws)
}

// SetSummary takes WS's summary (wssummary.Keeper); the zero summary
// retracts it.
func (t *Tracker) SetSummary(ws ids.WorkspaceID, summary wsm.WorkspaceSummary) {
	t.mu.Lock()
	defer t.mu.Unlock()
	if summary.Short == "" {
		delete(t.summaries, ws)
	} else {
		t.summaries[ws] = summary
	}
	t.redrawParentLocked(ws)
}

// redrawParentLocked redraws the parent of WS, if WS is a child.
func (t *Tracker) redrawParentLocked(ws ids.WorkspaceID) {
	if record, ok := t.records[ws]; ok && record.Parent != nil {
		t.redrawLocked(*record.Parent)
	}
}

// childrenLocked answers PARENT's children, oldest first.
func (t *Tracker) childrenLocked(parent ids.WorkspaceID) []wsm.Workspace {
	var out []wsm.Workspace
	for _, ws := range t.records {
		if ws.Parent != nil && *ws.Parent == parent {
			out = append(out, ws)
		}
	}
	slices.SortFunc(out, func(a, b wsm.Workspace) int {
		if c := a.CreatedAt.Compare(b.CreatedAt); c != 0 {
			return c
		}
		return strings.Compare(string(a.ID), string(b.ID))
	})
	return out
}

// redrawLocked publishes every bubble of PARENT's children that changed, and
// hands its footer and menu lists when they changed.
func (t *Tracker) redrawLocked(parent ids.WorkspaceID) {
	var open []footer.OpenChild
	type ended struct {
		child topbar.ClosedChild
		at    time.Time
	}
	var closed []ended
	for _, child := range t.childrenLocked(parent) {
		bubble := t.bubbleLocked(child)
		if !proto.Equal(bubble, t.drawn[child.ID]) {
			t.drawn[child.ID] = bubble
			t.deps.Feed.UpsertDurable(parent, feedid.Feed{Root: true}, Row(parent, child.ID, bubble))
			t.deps.Log.Debug(op, "drew a child's bubble in its parent's feed", dlog.Context{"parent": string(parent), "child": string(child.ID)})
		}
		_, removed := t.removed[child.ID]
		switch {
		case removed:
		case !child.Closed:
			open = append(open, t.openChildLocked(child))
		default:
			closed = append(closed, ended{child: topbar.ClosedChild{Ref: child.Ref(), Name: child.Name,
				ShortSummary: t.summaries[child.ID].Short, Merged: child.MergedAt != nil}, at: endedAt(child)})
		}
	}
	slices.SortStableFunc(closed, func(a, b ended) int { return b.at.Compare(a.at) })
	menu := make([]topbar.ClosedChild, 0, len(closed))
	for _, e := range closed {
		menu = append(menu, e.child)
	}
	if !slices.EqualFunc(open, t.open[parent], sameOpen) {
		t.open[parent] = open
		t.deps.Footer.SetOpenChildren(parent, open)
	}
	if !slices.EqualFunc(menu, t.closed[parent], sameClosed) {
		t.closed[parent] = menu
		t.deps.Topbar.SetClosedChildren(parent, menu)
	}
}

func sameOpen(a, b footer.OpenChild) bool {
	return a.Ref.GetId() == b.Ref.GetId() && a.Name == b.Name && a.Status == b.Status && a.Tone == b.Tone &&
		a.ShortSummary == b.ShortSummary && a.WaitingOnUser == b.WaitingOnUser
}

func sameClosed(a, b topbar.ClosedChild) bool {
	return a.Ref.GetId() == b.Ref.GetId() && a.Name == b.Name && a.ShortSummary == b.ShortSummary && a.Merged == b.Merged
}

// endedAt is when CHILD stopped being open: its landing, else its close.
func endedAt(child wsm.Workspace) time.Time {
	switch {
	case child.MergedAt != nil:
		return *child.MergedAt
	case child.Closure != nil:
		return child.Closure.At
	}
	return time.Time{}
}

// openChildLocked is CHILD as the parent's footer panel lists it.
func (t *Tracker) openChildLocked(child wsm.Workspace) footer.OpenChild {
	a := t.arms[child.ID]
	return footer.OpenChild{Ref: child.Ref(), Name: child.Name, Status: strings.ReplaceAll(a.name, "_", " "), Tone: a.tone,
		ShortSummary: t.summaries[child.ID].Short, WaitingOnUser: a.name == "permission" || a.name == "question"}
}

// bubbleLocked is CHILD's bubble as it stands.
func (t *Tracker) bubbleLocked(child wsm.Workspace) *frontendv1.FeedChild {
	removedAt, removed := t.removed[child.ID]
	open := !child.Closed && !removed
	head := &frontendv1.FeedChildHead{
		Glyph:    &frontendv1.FeedChildGlyph{Icon: Glyph},
		Name:     &frontendv1.FeedChildName{Text: child.Name},
		Standing: t.standingLocked(child, removed, removedAt),
		Runtime:  &frontendv1.FeedChildRuntime{StartedAtMs: child.CreatedAt.UnixMilli()},
	}
	if !removed {
		head.Name.Select = &frontendv1.FeedChildNameSelect{Target: child.Ref()}
	}
	if target, ok := t.targetLocked(child); ok {
		head.Target = &frontendv1.FeedChildTarget{Text: target}
	}
	if !open {
		end := endedAt(child)
		if removed && end.IsZero() {
			end = removedAt
		}
		ms := end.UnixMilli()
		head.Runtime.EndedAtMs = &ms
	}
	out := &frontendv1.FeedChild{Head: head}
	if summary, ok := t.summaries[child.ID]; ok {
		out.Summary = &frontendv1.FeedChildSummary{ShortSummary: &frontendv1.FeedChildShortSummary{Text: summary.Short}}
		if summary.Long != "" {
			out.Summary.LongSummary = &frontendv1.FeedChildLongSummary{Markdown: textline.Bullets(summary.Long)}
		}
	}
	if a, ok := t.arms[child.ID]; ok && open {
		out.Border = &frontendv1.FeedChildBorder{Tone: a.tone}
	}
	return out
}

// standingLocked is CHILD's standing: removed, landed, closed, its merge in
// flight (read off its status arm, as its row draws it), or open.
func (t *Tracker) standingLocked(child wsm.Workspace, removed bool, removedAt time.Time) *frontendv1.FeedChildStanding {
	switch {
	case removed:
		return &frontendv1.FeedChildStanding{Standing: &frontendv1.FeedChildStanding_Removed{Removed: &frontendv1.FeedChildStandingRemoved{RemovedAtMs: removedAt.UnixMilli()}}}
	case child.Closed && child.MergedAt != nil:
		return &frontendv1.FeedChildStanding{Standing: &frontendv1.FeedChildStanding_Merged{Merged: &frontendv1.FeedChildStandingMerged{}}}
	case child.Closed:
		return &frontendv1.FeedChildStanding{Standing: &frontendv1.FeedChildStanding_Closed{Closed: &frontendv1.FeedChildStandingClosed{}}}
	}
	switch t.arms[child.ID].name {
	case "merging":
		return &frontendv1.FeedChildStanding{Standing: &frontendv1.FeedChildStanding_Merging{Merging: &frontendv1.FeedChildStandingMerging{
			Stage: &frontendv1.FeedChildStandingMerging_Merging{Merging: &frontendv1.FeedChildStandingMergingMerging{}}}}}
	case "merge_queued":
		return &frontendv1.FeedChildStanding{Standing: &frontendv1.FeedChildStanding_Merging{Merging: &frontendv1.FeedChildStandingMerging{
			Stage: &frontendv1.FeedChildStandingMerging_Waiting{Waiting: &frontendv1.FeedChildStandingMergingWaiting{}}}}}
	}
	return &frontendv1.FeedChildStanding{Standing: &frontendv1.FeedChildStanding_Open{Open: &frontendv1.FeedChildStandingOpen{}}}
}

// targetLocked is the checkout CHILD lands in: the workspace it lands into,
// else its repository's main checkout. False when neither can be read.
func (t *Tracker) targetLocked(child wsm.Workspace) (string, bool) {
	if child.LandsInto != nil {
		into, ok := t.records[*child.LandsInto]
		if !ok {
			return "", false
		}
		return dirpath.Tilde(into.Dir, t.deps.Home), true
	}
	repo, ok := t.repos[child.Repo]
	if !ok {
		return "", false
	}
	return dirpath.Tilde(repo.Dir, t.deps.Home), true
}

// Row is CHILD's bubble as its row in PARENT's root feed, keyed by the child.
func Row(parent, child ids.WorkspaceID, bubble *frontendv1.FeedChild) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{
		Id:  feedid.Encode(feedid.Ref{WS: parent, Feed: feedid.Feed{Root: true}, Row: feedid.RowKey{Kind: feedid.KindChild, ID: string(child)}}),
		Row: &frontendv1.FeedRow_Child{Child: bubble},
	}
}
