// Package feedid encodes and decodes FeedId, the frontend's opaque row and
// feed address.
//
// The scheme is a versioned, delimiter-safe base64url string: the same input
// yields the same id across pushes, restarts and replays, and nothing outside
// this package parses one. See ARCHITECTURE.md "FeedId scheme".
package feedid

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/notimpl"
)

// Feed addresses one feed within a workspace: the root feed, one agent's
// sub-feed, or one merge bubble's sub-feed. Exactly one of the three is set.
type Feed struct {
	// Root marks the workspace's top-level feed.
	Root bool
	// Agent, when set, addresses a subagent bubble's sub-feed.
	Agent *conversationv1.AgentId
	// Merge, when set, addresses a merge bubble's sub-feed, keyed by the merge
	// lease that owns it.
	Merge *LeaseID
}

// LeaseID is the merge lease a merge sub-feed belongs to. It mirrors
// wsm.LeaseID; feedid keeps its own newtype because it sits below wsm in the
// dependency order and may not import it.
type LeaseID string

// Ref is a fully qualified row address: which workspace, which feed, which
// row.
type Ref struct {
	// WS is the workspace the row belongs to.
	WS WorkspaceID
	// Feed is the feed within that workspace.
	Feed Feed
	// Row identifies the row within that feed.
	Row RowKey
}

// WorkspaceID mirrors wsm.WorkspaceID for the same dependency-order reason as
// LeaseID.
type WorkspaceID string

// RowKind is the closed set of row kinds a FeedId can address.
type RowKind string

// The row kinds, one per feed row shape the resolver synthesizes.
const (
	KindPrompt           RowKind = "prompt"
	KindActivity         RowKind = "activity"
	KindTurnEnded        RowKind = "turn_ended"
	KindPermission       RowKind = "permission"
	KindQuestion         RowKind = "question"
	KindSeparation       RowKind = "separation"
	KindColdGate         RowKind = "cold_gate"
	KindMergeHead        RowKind = "merge_head"
	KindMergeTab         RowKind = "merge_tab"
	KindDetachedSubagent RowKind = "detached_subagent"
	KindDetachedShell    RowKind = "detached_shell"
	KindSynth            RowKind = "synth"
)

// RowKey identifies one row within one feed.
type RowKey struct {
	// Kind is the row's kind.
	Kind RowKind
	// ID is the kind's primary key: an activity id, a turn id, a permission id.
	ID string
	// Sub is the kind's secondary key, empty when the kind has none. A subagent
	// bubble row is Kind=activity, ID=<spawn unit>, Sub=<created agent id>.
	Sub string
}

// Encode renders a Ref as the opaque FeedId the frontend carries. The encoding
// is stable: the same Ref always yields the same value.
func Encode(ref Ref) *frontendv1.FeedId {
	return nil
}

// Decode parses a FeedId back into its Ref. An id from a different scheme
// version, or one that does not round-trip, is an error the caller surfaces as
// a NotFound refusal rather than guessing.
func Decode(id *frontendv1.FeedId) (Ref, error) {
	return Ref{}, notimpl.Err
}

// EncodeFeed renders a Feed as the FeedId that addresses the feed itself,
// which is what OpenFeed's optional feed field carries. The root feed's
// encoding is the one OpenFeed omits.
func EncodeFeed(ws WorkspaceID, feed Feed) *frontendv1.FeedId {
	return nil
}

// DecodeFeed parses a feed-addressing FeedId. A subagent bubble row decodes to
// Feed{Agent: created}; a merge head decodes to Feed{Merge: lease}.
func DecodeFeed(id *frontendv1.FeedId) (WorkspaceID, Feed, error) {
	return "", Feed{}, notimpl.Err
}
