package feed

import (
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
)

// ROW ORDER: WHERE A ROW BELONGS, NEVER WHEN IT ARRIVED (owner ruling
// 2026-09-27: a late row is placed where it would have been had it not been
// late).
//
// Every row carries frontend.v1 FeedRowOrder: an opaque key, minted HERE at the
// row's first draw and never changed, which a feed's rows sort by — in this
// resolver's own order (feedState.order), in every page, and on every client,
// which draws the order verbatim. The key is built from facts that survive a
// daemon restart, so a restarted daemon mints the identical key for a row drawn
// from the same stored entry:
//
//	<class><at_ms:16 hex><ordinal:8 hex><sub:8 hex>   a row drawn from an entry
//	<base>.<n:8 hex>                                  a row the daemon made itself
//
//   - CLASS is the row's plane family: a fork's ported conversation (0), its
//     inherited past (1), and the workspace's own conversation (2), history and
//     live alike. The fork's past precedes everything the fork produced by
//     construction (lineage.go), so it sorts first whatever its places say.
//   - AT_MS AND ORDINAL are the entry's conversation place (conversation.v1
//     ConversationPlace, served as HistoryEntryAt.place): where the entry sits
//     in its conversation, which the store keeps from the entry's first stated
//     place, so re-reads and restarts serve the same one.
//   - SUB is the row's index among the rows first drawn from that one entry in
//     that one feed (an agent prompt's ends, a tool card and its detached
//     head).
//   - A ROW THE DAEMON MAKES ITSELF — a turn's end it composes, a mirrored
//     prompt, a divider, a panel — or one drawn from an entry whose serving side
//     stated no place, FOLLOWS the row before it: it takes that row's entry
//     base and the next follow count on it, so it sorts just after that row
//     and every earlier follower, and before the next entry's rows. The row it
//     follows is the one a replay last placed while a replay draws, and the
//     feed's newest row of its class otherwise; with no such row, a live one
//     follows the moment it was made (orderNowBase).
//
// The fixed-width hex fields make key order the numeric order of the places,
// and '.' (0x2E) sorts below every hex digit, so a follower lands between its
// base and the next entry. Every character is printable ASCII, so byte order
// and a client's code-unit order agree.

// orderClass is the leading character of a key: the plane family.
func orderClass(p rowPlane) byte {
	switch p {
	case planePorted:
		return '0'
	case planeInherited:
		return '1'
	}
	return '2'
}

// followSep separates a follower's base from its follow count.
const followSep = '.'

// entryBase is the key prefix every row drawn from one entry shares.
func entryBase(class byte, place *conversationv1.ConversationPlace) string {
	return fmt.Sprintf("%c%016x%08x", class, uint64(place.GetAtMs()), place.GetOrdinal())
}

// baseOf is the entry base a key was minted on: the whole key for an entry's
// row, the part before the separator for a follower.
func baseOf(key string) string {
	if i := strings.IndexByte(key, followSep); i >= 0 {
		return key[:i]
	}
	return key
}

// usablePlace answers the entry place a row may be keyed from: the place in
// force, or nil when none was stated. A stated place that is not positive is a
// producer breaking the contract (ConversationPlace.at_ms is "always
// positive"); it is recorded at ERROR and the row is ordered as one whose
// place was never stated, rather than sorted to the feed's top.
func (r *resolver) usablePlace(s *wsState, f *feedState, id string) *conversationv1.ConversationPlace {
	place := s.entryPlace
	if place == nil {
		return nil
	}
	if place.GetAtMs() <= 0 {
		r.logger(s.id).Error("daemon.feed.place_invalid",
			"an entry was served with a non-positive conversation place; its row follows the row before it instead",
			dlog.Context{"feed": f.key, "row": id, "at_ms": place.GetAtMs(), "ordinal": place.GetOrdinal()})
		return nil
	}
	return place
}

// mintOrder mints the order key of a row being drawn for the first time, and
// answers how it was minted for the placement record.
func (r *resolver) mintOrder(s *wsState, f *feedState, id string) (string, string) {
	class := orderClass(s.plane)
	if place := r.usablePlace(s, f, id); place != nil {
		base := entryBase(class, place)
		sub := f.entryRows[base]
		f.entryRows[base] = sub + 1
		return fmt.Sprintf("%s%08x", base, sub), "entry"
	}
	base := baseOf(r.predecessor(s, f, class))
	if base == "" {
		base = r.orderNowBase(s, class)
	}
	n := f.followers[base] + 1
	f.followers[base] = n
	how := "follows"
	if s.inEntry {
		// THE PROTOCOL'S STATED FALLBACK (HistoryEntryAt.place: "a consumer
		// then orders the entry by its own receipt and records that it did").
		how = "entry_unplaced"
	}
	return fmt.Sprintf("%s%c%08x", base, followSep, n), how
}

// orderNowBase is the base a row follows when its feed holds no row of its
// class to follow: the moment it is made, as a conversation place. History is
// loaded only on a reader's request (book.go), so a feed can hold nothing yet
// while its conversation has a long past, and a daemon-made row keyed at the
// top of the feed would be served with the conversation's oldest page and
// stand above all of it. Keyed at the moment it was made, it sorts after every
// entry written before it and before every entry written after it.
//
// A REPLAY'S rows and a fork's past keep the bare class base: they are drawn
// from what the page placed, never at a moment of the daemon's own.
func (r *resolver) orderNowBase(s *wsState, class byte) string {
	if s.plane != planeLive {
		return string(class)
	}
	return entryBase(class, &conversationv1.ConversationPlace{AtMs: r.deps.Now().UnixMilli()})
}

// predecessor is the key of the row a row the daemon makes itself follows:
// while a replay draws, the row that replay last placed in this feed (a
// replayed turn's end follows its turn, not rows drawn live since); otherwise
// the feed's newest row of the same class. Empty when there is none.
func (r *resolver) predecessor(s *wsState, f *feedState, class byte) string {
	if s.plane != planeLive {
		if key, ok := s.replayTail[f.key]; ok && key[0] == class {
			return key
		}
	}
	for i := len(f.order) - 1; i >= 0; i-- {
		if key := f.rank[f.order[i]].key; key != "" && key[0] == class {
			return key
		}
	}
	return ""
}

// stampOrder writes a row's order onto the snapshot about to be published.
//
// A KEY NEVER CHANGES. A family restating a row it copied carries the key that
// row already had; one carrying any OTHER key is this resolver contradicting
// itself, recorded at ERROR — and the row keeps its original key, so a reader
// never sees it move.
func (r *resolver) stampOrder(s *wsState, f *feedState, id string, snapshot *frontendv1.FeedRow, key string) {
	if carried := snapshot.GetOrder().GetKey(); carried != "" && carried != key {
		r.logger(s.id).Error("daemon.feed.order_changed",
			"a row was restated with an order key other than the one it was placed with; it keeps its original key",
			dlog.Context{"feed": f.key, "row": id, "key": key, "restated_key": carried})
	}
	snapshot.Order = &frontendv1.FeedRowOrder{Key: key}
}
