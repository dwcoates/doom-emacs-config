package footer

import (
	"fmt"
	"sort"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE ⇄ CHIP AND ITS PANEL ARE THE FEED'S ASYNC MERGE BUBBLES, READ BACK
// (frontend.v1.FooterExpandedMerges). The merge orchestrator draws every merge
// as a bubble in the requester's feed: a head row on the root feed and tab
// rows on the bubble's sub-feed. The feed resolver hands each of those rows
// here as it stands (OnMergeRow) and as it goes (OnMergeRowRetired), so the
// panel is a function of exactly what the feed draws, and the two can never
// disagree about which merges are in flight, what step each is on, or where
// its bubble is.
//
//   - A row JOINS when the feed holds an ASYNC head carrying the `update` arm:
//     queued or merging in its hidden merge workspace.
//   - It LEAVES when that head carries a result (landed, failed, abandoned),
//     or the feed retires the head (a reset empties the feed).
//   - Its STEP is the bubble's current top-level tab: the newest one still
//     live, else the newest one drawn, "newest" by the feed's own order key.
//
// A SYNC head is never a row: it is the workspace's own merge, which the
// strip's merging status narrates.

// mergeRow is one async merge in flight, as the feed draws its head.
type mergeRow struct {
	// lease is the merge's ledger id: its head row's key and its sub-feed's.
	lease ids.LeaseID
	// entry is the head row's FeedId, which the row's jump names.
	entry *frontendv1.FeedId
	// label is the head's branch line.
	label string
	// startedAtMs is the head's clock instant.
	startedAtMs int64
	// orderKey is the head's feed order key, which the panel draws in.
	orderKey string
	// jump is the click resolution last recorded for this row.
	jump jumpMemo
}

// mergeTab is one top-level tab of a merge bubble's sub-feed.
type mergeTab struct {
	// orderKey is the tab row's feed order key: which tab is newest.
	orderKey string
	// words is the tab's drawn label with its round ("committing (2)").
	words string
	// settled reports the tab's state arm is settled.
	settled bool
	// waiting reports the tab's state arm is waiting on the user.
	waiting bool
}

// mergeBubbles is the footer's reading of the feed's merge bubbles.
type mergeBubbles struct {
	// rows are the async merges in flight, by ledger id.
	rows map[ids.LeaseID]*mergeRow
	// tabs are the top-level tabs drawn for each ledger, by tab row id. A tab
	// may stand before its head does (a restore draws rows in its own order),
	// so tabs are kept apart from the rows.
	tabs map[ids.LeaseID]map[string]*mergeTab
	// resolved are the ledgers whose head stood with a result, or that were
	// SYNC, so a tab drawn after its head is not kept for a merge no row draws.
	resolved map[ids.LeaseID]struct{}
}

func newMergeBubbles() mergeBubbles {
	return mergeBubbles{
		rows:     map[ids.LeaseID]*mergeRow{},
		tabs:     map[ids.LeaseID]map[string]*mergeTab{},
		resolved: map[ids.LeaseID]struct{}{},
	}
}

// settle drops LEASE's row and tabs, and marks it resolved.
func (m *mergeBubbles) settle(lease ids.LeaseID) {
	delete(m.rows, lease)
	delete(m.tabs, lease)
	m.resolved[lease] = struct{}{}
}

// OnMergeRow takes one merge bubble row the feed now holds for merge LEASE:
// its head (a root-feed FeedMerge activity) or one of its tabs (a FeedMergeTab
// row of its sub-feed). It is the feed resolver's Deps.MergeRow. Any other row
// is a caller defect, recorded at ERROR and taken as nothing.
//
// CALLED UNDER THE FEED RESOLVER'S LOCK, as OnEntryPlaced is. It takes this
// resolver's lock and nothing else, and nothing here calls back into the feed.
func (r *resolver) OnMergeRow(ws ids.WorkspaceID, lease ids.LeaseID, row *frontendv1.FeedRow) {
	if head := row.GetActivity().GetMerge(); head != nil {
		r.onMergeHead(ws, lease, row, head)
		return
	}
	if tab := row.GetMergeTab(); tab != nil {
		r.onMergeTab(ws, lease, row, tab)
		return
	}
	r.workspaceLog(ws).Error("daemon.footer.merge_row_unknown",
		"the feed handed the footer a row that is neither a merge head nor a merge tab; the merges panel takes nothing from it",
		dlog.Context{"lease": string(lease), "row": row.GetId().GetValue()})
}

// onMergeHead advances LEASE's row from its head.
func (r *resolver) onMergeHead(ws ids.WorkspaceID, lease ids.LeaseID, row *frontendv1.FeedRow, merge *frontendv1.FeedMerge) {
	async := merge.GetHead().GetMode().GetAsync() != nil
	inFlight := merge.GetUpdate() != nil
	r.mutate(ws, "daemon.footer.on_merge_head", "the footer took a merge bubble's head",
		dlog.Context{"lease": string(lease), "row": row.GetId().GetValue(), "async": async, "in_flight": inFlight},
		func(s *wsState) {
			if !async || !inFlight {
				s.merges.settle(lease)
				return
			}
			drawn, ok := s.merges.rows[lease]
			if !ok {
				drawn = &mergeRow{lease: lease}
				s.merges.rows[lease] = drawn
			}
			drawn.entry = row.GetId()
			drawn.label = merge.GetHead().GetLabel().GetText()
			drawn.startedAtMs = merge.GetHead().GetRuntime().GetStartedAtMs()
			drawn.orderKey = row.GetOrder().GetKey()
		})
}

// onMergeTab records one of LEASE's tabs. A subtab (a row with a parent) is
// drawn inside a top-level tab, whose own state already says what it is doing
// (frontend.v1.FeedMergeTab: a parent tab is waiting on the user while its
// subtab's agent is), so only top-level tabs are kept.
func (r *resolver) onMergeTab(ws ids.WorkspaceID, lease ids.LeaseID, row *frontendv1.FeedRow, tab *frontendv1.FeedMergeTab) {
	if row.GetParent() != nil {
		return
	}
	settled, waiting := mergeTabState(row.GetId().GetValue(), tab)
	words := mergeTabWords(tab.GetLabel())
	r.mutate(ws, "daemon.footer.on_merge_tab", "the footer took a merge bubble's top-level tab",
		dlog.Context{"lease": string(lease), "row": row.GetId().GetValue(), "words": words, "settled": settled, "waiting": waiting},
		func(s *wsState) {
			if _, resolved := s.merges.resolved[lease]; resolved {
				return
			}
			tabs, ok := s.merges.tabs[lease]
			if !ok {
				tabs = map[string]*mergeTab{}
				s.merges.tabs[lease] = tabs
			}
			tabs[row.GetId().GetValue()] = &mergeTab{
				orderKey: row.GetOrder().GetKey(),
				words:    words,
				settled:  settled,
				waiting:  waiting,
			}
		})
}

// OnMergeRowRetired takes the feed's retirement of merge LEASE's row ID: a
// retired head takes its merge's row away, a retired tab its step. It is the
// feed resolver's Deps.MergeRowRetired.
func (r *resolver) OnMergeRowRetired(ws ids.WorkspaceID, lease ids.LeaseID, id string) {
	r.mutate(ws, "daemon.footer.on_merge_row_retired", "the footer took the feed's retirement of a merge bubble row",
		dlog.Context{"lease": string(lease), "row": id}, func(s *wsState) {
			if drawn, ok := s.merges.rows[lease]; ok && drawn.entry.GetValue() == id {
				delete(s.merges.rows, lease)
				delete(s.merges.tabs, lease)
				return
			}
			delete(s.merges.tabs[lease], id)
		})
}

// mergeTabState answers whether TAB's state arm is settled, and whether it is
// waiting on the user.
//
// READ OFF THE SCHEMA, NOT A LIST OF KINDS. Every tab kind carries a `state`
// oneof whose arms are the shared badge leaves (frontend.v1.FeedMergeTab:
// "every state oneof is live, settled, or waiting on the user"), so the set
// leaf's message type is the answer for every kind, a kind added later
// included. A tab with no kind, or a kind with no state stated, is a producer
// defect, and it panics rather than draw a step by a guess.
func mergeTabState(id string, tab *frontendv1.FeedMergeTab) (settled, waiting bool) {
	msg := tab.ProtoReflect()
	kindField := msg.WhichOneof(msg.Descriptor().Oneofs().ByName("kind"))
	if kindField == nil {
		panic(fmt.Sprintf("footer: merge tab %q carries no kind (frontend.v1.FeedMergeTab.kind)", id))
	}
	kind := msg.Get(kindField).Message()
	states := kind.Descriptor().Oneofs().ByName("state")
	if states == nil {
		panic(fmt.Sprintf("footer: merge tab %q's kind %s has no state oneof", id, kindField.Name()))
	}
	stateField := kind.WhichOneof(states)
	if stateField == nil {
		panic(fmt.Sprintf("footer: merge tab %q's %s states no state", id, kindField.Name()))
	}
	leaf := stateField.Message().FullName()
	return leaf == settledLeaf, leaf == waitingLeaf
}

var (
	settledLeaf protoreflect.FullName = (&frontendv1.FeedMergeTabSettled{}).ProtoReflect().Descriptor().FullName()
	waitingLeaf protoreflect.FullName = (&frontendv1.FeedMergeTabWaitingOnUser{}).ProtoReflect().Descriptor().FullName()
)

// mergeTabWords is a top-level tab's label as words, with its round beyond the
// first ("committing (2)"), the way the bubble's strip decorates it.
func mergeTabWords(label *frontendv1.FeedMergeTabLabel) string {
	words := label.GetText()
	if label.GetRound() > 1 {
		return fmt.Sprintf("%s (%d)", words, label.GetRound())
	}
	return words
}

// currentTab answers LEASE's current top-level tab: the newest one still
// live, else the newest one drawn. Nil while the bubble has drawn no tab.
func (m *mergeBubbles) currentTab(lease ids.LeaseID) *mergeTab {
	var newestLive, newest *mergeTab
	for _, tab := range m.tabs[lease] {
		if newest == nil || tab.orderKey > newest.orderKey {
			newest = tab
		}
		if !tab.settled && (newestLive == nil || tab.orderKey > newestLive.orderKey) {
			newestLive = tab
		}
	}
	if newestLive != nil {
		return newestLive
	}
	return newest
}

// mergeRowsDrawn answers the async merges in flight, in the feed's order.
func (s *wsState) mergeRowsDrawn() []*mergeRow {
	rows := make([]*mergeRow, 0, len(s.merges.rows))
	for _, row := range s.merges.rows {
		rows = append(rows, row)
	}
	sort.Slice(rows, func(i, j int) bool { return rows[i].orderKey < rows[j].orderKey })
	return rows
}

// mergesChip renders the ⇄ chip: the panel's row count, and the waiting glyph
// while any row waits on the user. Nil when no async merge is in flight.
func (s *wsState) mergesChip() *frontendv1.FooterChipMerges {
	rows := s.mergeRowsDrawn()
	if len(rows) == 0 {
		return nil
	}
	chip := &frontendv1.FooterChipMerges{Count: uint32(len(rows))}
	waiting := 0
	for _, row := range rows {
		if tab := s.merges.currentTab(row.lease); tab != nil && tab.waiting {
			waiting++
		}
	}
	if waiting > 0 {
		chip.WaitingOnUser = &frontendv1.FooterChipMergesWaitingOnUser{Count: uint32(waiting)}
	}
	return chip
}

// mergesPanel renders the ⇄ panel: one jump row per async merge in flight,
// each jumping to its bubble's head.
func (s *wsState) mergesPanel() *frontendv1.FooterExpandedMerges {
	out := &frontendv1.FooterExpandedMerges{}
	for _, row := range s.mergeRowsDrawn() {
		jump := jumpTo(row.entry)
		tab := s.merges.currentTab(row.lease)
		s.noteJump(&row.jump, "merge", string(row.lease), jump, dlog.Context{"has_step": tab != nil})
		drawn := &frontendv1.FooterMergeRow{
			Label:   &frontendv1.FooterMergeRowLabel{Text: row.label},
			Runtime: &frontendv1.FooterMergeRowRuntime{StartedAtMs: row.startedAtMs},
			Work:    &frontendv1.FooterWorkId{Value: string(row.lease)},
			Jump:    jump,
		}
		if tab != nil {
			drawn.Step = &frontendv1.FooterMergeRowStep{Text: tab.words}
			if tab.waiting {
				drawn.WaitingOnUser = &frontendv1.FooterMergeRowWaitingOnUser{}
			}
		}
		out.Rows = append(out.Rows, drawn)
	}
	return out
}
