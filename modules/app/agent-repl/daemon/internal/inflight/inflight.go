// Package inflight is THE daemon's single answer to "does this workspace have
// work in flight?" — for every meaning of work, not just a turn.
//
// # Why one package exists
//
// The question had at least five answers and they disagreed:
//
//   - refreshStaleShim gated a shim roll on `turn_in_flight || active_turn_ids`.
//   - Server.sweepable gated a SIGTERM on `WorkspaceState.turn_active` alone,
//     so an IDLE_ASYNC workspace — idle turn, live background task — was swept.
//   - hibernate()'s settledness guard consulted the resolved render state and
//     the durable turn claims, and nothing about live tasks.
//   - close and merge consulted nothing at all.
//   - the scheduled-shutdown drain consulted a turn flag plus a task COUNT.
//
// Five derivations of one fact is five chances to bounce over somebody's work,
// and the count in the fifth is its own error class: a count cannot tell "the
// same two things are still running" from "one died and a new one started".
// That is precisely how a full shim fleet loss passed for a clean restart on
// 2026-08-10 — the process count was equal across a total kill-and-respawn.
//
// # So the answer is a SET OF IDENTIFIED ITEMS
//
// Never a boolean and never a count. Every member carries a stable identity
// under which it can be recognised again on the far side of a bounce, which is
// the whole basis of the before/after manifest in manifest.go.
//
// # ABSENCE IS NOT EMPTINESS
//
// A Set is a SUM, not a slice. `Answered` with no items means the daemon asked
// and the workspace has nothing running — a licence to proceed. `Unanswered`
// means nobody could say, and NO licence follows from silence. This is the same
// rule ShimHello.live_task_set encodes on the wire (a present-but-empty
// LiveTaskSet versus an absent one) and the same rule bounceledger.Judge
// follows when a lock probe fails. Reading an unanswered set as empty is the
// exact defect this package exists to make unrepresentable.
//
// # STRUCTURAL, NOT PROBABILISTIC
//
// Nothing here has a timer, a grace window, or a "probably settled by now".
// Membership changes only on explicit edges — a work item started, a work item
// ended — and emptiness is a fact the daemon owns rather than a state it ages
// into.
package inflight

import (
	"errors"
	"fmt"
	"sort"
	"strings"
)

// Kind names WHICH plane a work item lives on. The three are not
// interchangeable and are never folded together: they are killed by different
// events, recovered by different mechanisms, and rendered by different
// surfaces.
type Kind string

const (
	// KindTurn is a live agent turn: a prompt accepted and awaiting its SDK
	// result. Its identity is the turn id the durable claim ledger keys on
	// (ssm.ActiveTurnIDs), so a turn adopted from a previous daemon is named
	// by the same value the daemon that started it used.
	KindTurn Kind = "turn"
	// KindTask is a live background/async task: a spawned agent, a detached
	// shell. Its identity is the task id TaskStarted and TaskEnded both carry
	// and that ShimHello.live_task_set answers in.
	//
	// IT IS THE MEMBER THE OLD ANSWERS ALL MISSED. A shim running only
	// detached work has no turn, so every turn-shaped gate read it as idle at
	// exactly the instant it decided whether to kill the process.
	KindTask Kind = "task"
	// KindQuery is the live SDK query() invocation the shim owns
	// (ShimHello.query_instance_id). It is the thing that actually gets killed
	// when a query terminates unexpectedly, and before this package nothing
	// tracked it as in-flight STATE — only as an identity stamped onto
	// accounting rows.
	KindQuery Kind = "query"
)

// Kinds lists the closed vocabulary in a stable order, so a manifest's
// per-kind breakdown does not depend on map iteration.
var Kinds = []Kind{KindTurn, KindTask, KindQuery}

// Valid reports whether k is one of the closed set. An unrecognised kind is
// refused at construction rather than carried into a manifest nobody can read.
func (k Kind) Valid() bool {
	for _, known := range Kinds {
		if k == known {
			return true
		}
	}
	return false
}

// Item is ONE identified unit of in-flight work.
//
// ID is mandatory and is the reason this is not a count. Detail is free text
// for the human reading a manifest — a tool name, a prompt fragment — and is
// never compared, so a detail that changed across a bounce cannot be mistaken
// for a different item.
type Item struct {
	Kind   Kind   `json:"kind"`
	ID     string `json:"id"`
	Detail string `json:"detail,omitempty"`
}

// Key is the item's comparable identity: kind and id, never detail.
func (i Item) Key() string { return string(i.Kind) + ":" + i.ID }

// String renders the item for a log line.
func (i Item) String() string {
	if i.Detail == "" {
		return i.Key()
	}
	return i.Key() + " (" + i.Detail + ")"
}

// ErrItemUnidentified marks the one construction refusal that is always a
// programming defect: a work item with no identity. Such an item could not be
// recognised on the far side of a bounce, so admitting it would silently
// reintroduce counting.
var ErrItemUnidentified = errors.New("inflight: a work item with no identity cannot be a set member")

// ErrItemKindUnknown marks a member whose kind is outside the closed set.
var ErrItemKindUnknown = errors.New("inflight: a work item's kind is outside the closed vocabulary")

// Set is a workspace's complete in-flight answer at one instant.
//
// IT IS A SUM OF TWO ARMS and the zero value is deliberately the UNANSWERED
// one: a Set nobody filled in must never read as "nothing is running". The
// fields are unexported so the only way to obtain an answered set is through
// Answered, which validates every member's identity.
type Set struct {
	workspace string
	// answered distinguishes the arms. False is "no one could say".
	answered bool
	// reason states WHY the set is unanswered, in the words a log line needs.
	// It is required on the unanswered arm, so silence always carries an
	// account of itself.
	reason string
	items  []Item
}

// Unanswered builds the arm that says nothing is known. The reason is
// mandatory: an unexplained unknown is indistinguishable from a forgotten
// field, and callers act on this arm by REFUSING to proceed, which they must
// be able to justify in a log.
func Unanswered(workspace, reason string) Set {
	if strings.TrimSpace(reason) == "" {
		reason = "NO REASON WAS RECORDED — the in-flight set is unknown and its cause was not stated, which is itself a defect"
	}
	return Set{workspace: workspace, reason: reason}
}

// Answered builds the arm that says this IS the complete set, which may be
// empty. Every member must carry a kind from the closed vocabulary and a
// non-empty identity; a violation is returned, never dropped, because dropping
// it would understate what is running.
//
// Members are deduplicated by Key and sorted, so two resolutions of the same
// facts produce byte-identical manifests.
func Answered(workspace string, items ...Item) (Set, error) {
	seen := make(map[string]Item, len(items))
	order := make([]string, 0, len(items))
	for _, item := range items {
		if !item.Kind.Valid() {
			return Set{}, fmt.Errorf("%w: workspace %q item kind %q", ErrItemKindUnknown, workspace, item.Kind)
		}
		if strings.TrimSpace(item.ID) == "" {
			return Set{}, fmt.Errorf("%w: workspace %q kind %q", ErrItemUnidentified, workspace, item.Kind)
		}
		key := item.Key()
		if _, dup := seen[key]; dup {
			continue
		}
		seen[key] = item
		order = append(order, key)
	}
	sort.Strings(order)
	out := make([]Item, 0, len(order))
	for _, key := range order {
		out = append(out, seen[key])
	}
	return Set{workspace: workspace, answered: true, items: out}, nil
}

// MustAnswered is Answered for call sites that construct their own members and
// therefore cannot produce an invalid one. It panics rather than returning a
// set, because a silent fallback here would be the counting bug again.
func MustAnswered(workspace string, items ...Item) Set {
	set, err := Answered(workspace, items...)
	if err != nil {
		panic(err)
	}
	return set
}

// Workspace names the workspace the answer is about.
func (s Set) Workspace() string { return s.workspace }

// Known reports whether the set is the ANSWERED arm. It is deliberately not
// called "Empty": the question a caller must ask first is whether there is an
// answer at all.
func (s Set) Known() bool { return s.answered }

// Reason states why an answered=false set is unknown. It is "" on the answered
// arm.
func (s Set) Reason() string { return s.reason }

// Items returns the members, or nil on the unanswered arm. The slice is a copy:
// a caller that appended to it would be editing the authority.
func (s Set) Items() []Item {
	if !s.answered || len(s.items) == 0 {
		return nil
	}
	return append([]Item(nil), s.items...)
}

// Has reports whether an item with this identity is a member. It is false on
// the unanswered arm — "I cannot say" is not "yes" either.
func (s Set) Has(item Item) bool {
	if !s.answered {
		return false
	}
	for _, member := range s.items {
		if member.Key() == item.Key() {
			return true
		}
	}
	return false
}

// OfKind returns the members of one kind, in the set's stable order.
func (s Set) OfKind(kind Kind) []Item {
	if !s.answered {
		return nil
	}
	var out []Item
	for _, member := range s.items {
		if member.Kind == kind {
			out = append(out, member)
		}
	}
	return out
}

// Blocks is THE consumer-facing verdict: may an operation that would interrupt
// this workspace's work proceed?
//
// It returns true — blocked — for BOTH a non-empty answered set and an
// unanswered one, and the two carry different reasons. Making one function
// answer both is the whole point: a caller cannot accidentally handle only the
// case it thought of, because there is no second accessor that reports
// "not busy" for an unknown.
func (s Set) Blocks() (blocked bool, why string) {
	if !s.answered {
		return true, "the in-flight set is UNKNOWN, and unknown is not empty: " + s.reason
	}
	if len(s.items) == 0 {
		return false, "the in-flight set is empty and this workspace holds no turn, task or query"
	}
	return true, "the workspace holds " + s.Summary()
}

// Summary renders the members for a log line: identities, never a bare count.
// The count is included only as a prefix to the identities it summarizes.
func (s Set) Summary() string {
	if !s.answered {
		return "UNKNOWN (" + s.reason + ")"
	}
	if len(s.items) == 0 {
		return "nothing in flight"
	}
	parts := make([]string, 0, len(s.items))
	for _, item := range s.items {
		parts = append(parts, item.String())
	}
	return fmt.Sprintf("%d in flight: %s", len(parts), strings.Join(parts, ", "))
}

// Settled is PROOF, carried in the type system, that a workspace held no
// in-flight work when its set was resolved.
//
// It exists so that "this workspace is done" and "this workspace holds live
// work" cannot be held at once by construction. A teardown gate takes a
// Settled rather than a bool, and the only way to obtain one is Set.Settled()
// on an ANSWERED, EMPTY set — so there is no expression anywhere that produces
// settledness from an unknown, from a count, or from a turn flag alone.
type Settled struct {
	workspace string
	// proven is false on the zero value, which is what makes a forgotten
	// Settled useless rather than dangerous.
	proven bool
}

// Settled returns the proof, and false when there is none. An unanswered set
// never yields one.
func (s Set) Settled() (Settled, bool) {
	if !s.answered || len(s.items) != 0 {
		return Settled{}, false
	}
	return Settled{workspace: s.workspace, proven: true}, true
}

// Workspace names the workspace the proof is about. A caller comparing this
// against the workspace it is about to tear down cannot act on another
// workspace's proof.
func (p Settled) Workspace() string { return p.workspace }

// Proven reports whether this value is a real proof rather than a zero value.
func (p Settled) Proven() bool { return p.proven }

// Union folds several partial answers into one.
//
// AN UNANSWERED PART POISONS THE WHOLE, and that is the point: a union that
// knew the turns but not the tasks would report a complete set it does not
// have. The first unanswered part's reason is carried forward so the log names
// which plane went dark rather than saying only that something did.
func Union(workspace string, parts ...Set) Set {
	var items []Item
	for _, part := range parts {
		if !part.Known() {
			return Unanswered(workspace, fmt.Sprintf("a component answer for workspace %q is unknown, so the union is unknown: %s", part.Workspace(), part.Reason()))
		}
		items = append(items, part.items...)
	}
	set, err := Answered(workspace, items...)
	if err != nil {
		// Unreachable through the constructors — every part was validated by
		// Answered already — but a union that could not be formed is reported
		// as unknown rather than as empty, which is the safe direction.
		return Unanswered(workspace, fmt.Sprintf("the component answers could not be unioned: %v", err))
	}
	return set
}
