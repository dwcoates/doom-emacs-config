package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// The operation this file's records carry.
const opWorkspaceReset = "daemon.feed.workspace_reset"

// ResetWorkspace empties ONE workspace's feed, so the workspace is as it would
// be if its feed had never been opened.
//
// IT BELONGS TO A BIND AND TO NOTHING ELSE. A bind is the one verb that
// changes WHICH vendor conversation a workspace runs, and a feed carrying the
// previous conversation's rows is then a feed showing a conversation the
// workspace no longer has — the owner's "I selected one, it did not reload the
// transcript". A RESTART, a revival, a hibernation wake and every other
// bring-up resume the SAME conversation, and they keep their rows exactly as
// they are: replaying onto standing rows is an upsert, which is what makes a
// restart look like nothing happened.
//
// A RESET IS NOT A PAGE REPLACE. A page replace re-serves the same
// conversation and carries the reader's own doing across it — the folds a
// reader opened survive it by owner ruling. A reset ends the conversation
// those folds belong to, so it takes them with it: every row is RETIRED on the
// wire (the dual of an upsert, the same removal a retired row publishes), and
// a client that drops a row drops everything it held for that row.
//
// WHAT IT DROPS, and what each drop means for the module holding it:
//
//   - Every row of every feed, root and sub-feed alike, each published as a
//     removal so an already-open reader empties LIVE rather than keeping the
//     old conversation painted until it reloads.
//   - The DELIVERY BOUND, which is not stored anywhere of its own: it is read
//     off the newest context-cutting separation row (see pages.go), so
//     dropping the rows drops the bound and with it every row the bound was
//     withholding. Nothing can be resurrected by a later push, because nothing
//     is left to resurrect.
//   - The SERVED asks: the permission rows, their daemon-side standing tokens,
//     the calls they gated, and the question batches. An answer arriving for
//     an ask of the conversation that was unbound now finds no served ask and
//     is REFUSED by the ordinary path, rather than being forwarded to a
//     session that never asked it.
//   - The OUTPUT ADDRESS. It names a merge tab of the session that was just
//     stopped; it cannot mean anything on a different conversation, so the
//     next rows land on the root feed until a lease holder addresses them
//     again.
//   - The SYNTHESIZED rows — the cold gate, merge heads, session separations,
//     accepted-prompt mirrors, command panels. They are rows, and they go the
//     way of every other row. The gate the BIND's own start raises is drawn
//     after this reset, which is why the reset runs before the new session
//     starts.
//   - The final-response SELECTION SET and its markdown, so a reply-to-a-past
//     -response can never name an answer of a conversation this workspace no
//     longer runs; the submit path refuses such a feedid rather than
//     delivering an empty prefix.
//   - The standing `final_answer_unresolved` FAULT, retracted through its own
//     close so the footer stops drawing a line about a turn of a conversation
//     that is gone, and every armed stall window, stopped so no timer of the
//     old conversation can raise a fault against the new one.
//   - The fork's `portedDrawn` mark, so a forked workspace's ported parent
//     conversation is REDRAWN above the newly bound conversation. The ported
//     conversation belongs to the WORKSPACE's origin, not to the vendor
//     conversation it happens to run, and a reset that kept the mark would
//     drop it for good.
//
// WHAT IT DELIBERATELY KEEPS:
//
//   - The feed SHELLS and their subscribers. A tail is an open connection: the
//     rows are what the conversation owns, the connection is not, and deleting
//     the feed would leave a live reader attached to nothing. Every shell is
//     emptied in place, so the reader sees the removals and then the new
//     conversation's rows on the same tail.
//   - Each feed's publication SEQUENCE and its retained log. A watch token
//     pins to a sequence; rewinding the counter would make a minted token
//     ambiguous and a pinned replay wrong. The removals take the next
//     sequences, exactly as any other publication does.
//   - The minted WATCH TOKENS, for the same reason: a reader that opened
//     before the bind keeps streaming across it.
//   - `synthSeq` and `stallSeq`, which are monotonic on purpose: a synthesized
//     id must not be re-minted onto a row a slow reader may still be holding,
//     and a stall window's number must not repeat while a timer of the old
//     conversation is still in flight toward the mutex.
//   - Each reader's walk, re-parked at the start of the now-empty feed. The
//     walk is the connection's, not the conversation's, and a `next` after a
//     reset answers "nothing older" rather than the refusal a dropped walk
//     would make of it.
func (r *resolver) ResetWorkspace(ws ids.WorkspaceID, because string) {
	r.mu.Lock()
	defer r.mu.Unlock()

	log := r.logger(ws)
	old, held := r.workspaces[ws]
	if !held {
		log.Info(opWorkspaceReset,
			"a workspace's feed was reset before it had drawn anything; there was nothing to empty",
			dlog.Context{"because": because})
		return
	}

	// THE TIMERS AND THE FAULT COME FIRST, while the state that registered
	// them is still the state this resolver answers for: both reach OUTSIDE
	// this package — a stall window onto the clock, the fault onto the footer
	// — and dropping the state without retracting them would leave the old
	// conversation speaking through the new one.
	for unit := range old.stalls {
		r.disarmAnswerStall(old, unit)
	}
	r.closeAnswerFault(old, because)

	fresh := newWSState(ws)
	rows := 0
	for key, f := range old.feeds {
		rows += r.emptyFeed(f)
		fresh.feeds[key] = f
		fresh.feedAddrs[key] = old.feedAddrs[key]
	}
	for reader, w := range old.readers {
		fresh.readers[reader] = &walk{feedKey: w.feedKey, oldest: 0, standing: w.standing}
	}
	fresh.synthSeq = old.synthSeq
	fresh.stallSeq = old.stallSeq
	r.workspaces[ws] = fresh

	log.Info(opWorkspaceReset,
		"a workspace's feed was emptied: every row was retired and every accumulation dropped",
		dlog.Context{
			"because": because,
			"feeds":   len(old.feeds),
			"rows":    rows,
			"readers": len(old.readers),
		})
}

// emptyFeed retires every row of one feed and answers how many it retired.
//
// The removal is published exactly as `retire` publishes one — a new sequence,
// an entry in the retained log, an enqueue to every tail — because it IS the
// same event: the row is gone and every reader, connected now or connecting
// against a pinned token later, must be told once. The rows go in feed order,
// so a reader drops them top-down rather than in map order.
func (r *resolver) emptyFeed(f *feedState) int {
	dropped := len(f.order)
	for _, id := range f.order {
		f.seq++
		removal := &frontendv1.FeedRow{
			Id:  &frontendv1.FeedId{Value: id},
			Row: &frontendv1.FeedRow_Removed{Removed: &frontendv1.FeedRowRemoved{}},
		}
		f.log = append(f.log, &loggedRow{seq: f.seq, row: removal})
		for sub := range f.subs {
			sub.enqueue(removal)
		}
	}
	if len(f.log) > f.retention {
		f.log = f.log[len(f.log)-f.retention:]
	}
	f.order = nil
	f.rank = map[string]rowRank{}
	f.rows = map[string]*frontendv1.FeedRow{}
	f.nonDurable = map[string]bool{}
	f.superseded = map[string]bool{}
	// THE TRUNCATION NOTICE GOES TOO. It says older history exists above the
	// oldest row the replay delivered, and it was a statement about the
	// conversation whose rows have just been retired.
	f.historyMore = nil
	return dropped
}
