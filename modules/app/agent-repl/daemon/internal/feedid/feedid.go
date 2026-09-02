// Package feedid encodes and decodes FeedId, the frontend's opaque row and
// feed address.
//
// The scheme is a versioned, delimiter-safe base64url string: the same input
// yields the same id across pushes, restarts and replays, and nothing outside
// this package parses one. See ARCHITECTURE.md "FeedId scheme".
package feedid

import (
	"encoding/base64"
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
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

// LeaseID is the merge lease a merge sub-feed belongs to — an alias of the
// one spelling in internal/ids, which feedid and wsm share.
type LeaseID = ids.LeaseID

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

// WorkspaceID is the workspace an addressed row belongs to — the same alias
// of internal/ids that wsm exposes.
type WorkspaceID = ids.WorkspaceID

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

// AllRowKinds is every row kind, in declaration order. Decode accepts exactly
// these; anything else is a malformed id rather than a kind to guess at.
var AllRowKinds = []RowKind{
	KindPrompt,
	KindActivity,
	KindTurnEnded,
	KindPermission,
	KindQuestion,
	KindSeparation,
	KindColdGate,
	KindMergeHead,
	KindMergeTab,
	KindDetachedSubagent,
	KindDetachedShell,
	KindSynth,
}

func knownKind(k RowKind) bool {
	for _, want := range AllRowKinds {
		if k == want {
			return true
		}
	}
	return false
}

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

// version is the scheme version every encoded id carries. Decode refuses any
// other version rather than reinterpreting an id it does not own.
const version = "1"

// prefix separates the version from the payload. It is not in the base64url
// alphabet, so it can never occur inside the payload.
const prefix = "f" + version + "."

// sep delimits the payload's fields. It cannot occur in a base64url payload
// and is never produced by the encoder for any input, so the encoding is
// delimiter-safe for arbitrary ids.
const sep = "\x1f"

// feed discriminators, one per Feed arm.
const (
	feedRoot  = "r"
	feedAgent = "a"
	feedMerge = "m"
)

func encodeFeedFields(feed Feed) (kind, value string, err error) {
	set := 0
	if feed.Root {
		set++
		kind, value = feedRoot, ""
	}
	if feed.Agent != nil {
		set++
		kind, value = feedAgent, feed.Agent.GetValue()
	}
	if feed.Merge != nil {
		set++
		kind, value = feedMerge, string(*feed.Merge)
	}
	if set != 1 {
		return "", "", fmt.Errorf("feedid: exactly one feed arm must be set, %d are", set)
	}
	return kind, value, nil
}

func decodeFeedFields(kind, value string) (Feed, error) {
	switch kind {
	case feedRoot:
		if value != "" {
			return Feed{}, fmt.Errorf("feedid: root feed carries a value %q", value)
		}
		return Feed{Root: true}, nil
	case feedAgent:
		if value == "" {
			return Feed{}, fmt.Errorf("feedid: agent feed carries no agent id")
		}
		return Feed{Agent: &conversationv1.AgentId{Value: value}}, nil
	case feedMerge:
		if value == "" {
			return Feed{}, fmt.Errorf("feedid: merge feed carries no lease id")
		}
		lease := LeaseID(value)
		return Feed{Merge: &lease}, nil
	default:
		return Feed{}, fmt.Errorf("feedid: unknown feed discriminator %q", kind)
	}
}

func encode(ws WorkspaceID, feed Feed, row RowKey) (*frontendv1.FeedId, error) {
	if ws == "" {
		return nil, fmt.Errorf("feedid: workspace id is empty")
	}
	feedKind, feedValue, err := encodeFeedFields(feed)
	if err != nil {
		return nil, err
	}
	fields := []string{string(ws), feedKind, feedValue, string(row.Kind), row.ID, row.Sub}
	for i, f := range fields {
		if strings.Contains(f, sep) {
			return nil, fmt.Errorf("feedid: field %d contains the payload delimiter", i)
		}
	}
	payload := strings.Join(fields, sep)
	return &frontendv1.FeedId{Value: prefix + base64.RawURLEncoding.EncodeToString([]byte(payload))}, nil
}

func decode(id *frontendv1.FeedId) (WorkspaceID, Feed, RowKey, error) {
	if id == nil {
		return "", Feed{}, RowKey{}, fmt.Errorf("feedid: nil feed id")
	}
	value := id.GetValue()
	if !strings.HasPrefix(value, prefix) {
		return "", Feed{}, RowKey{}, fmt.Errorf("feedid: unknown scheme version in %q", value)
	}
	raw, err := base64.RawURLEncoding.DecodeString(strings.TrimPrefix(value, prefix))
	if err != nil {
		return "", Feed{}, RowKey{}, fmt.Errorf("feedid: payload is not base64url: %w", err)
	}
	fields := strings.Split(string(raw), sep)
	if len(fields) != 6 {
		return "", Feed{}, RowKey{}, fmt.Errorf("feedid: payload has %d fields, want 6", len(fields))
	}
	ws := WorkspaceID(fields[0])
	if ws == "" {
		return "", Feed{}, RowKey{}, fmt.Errorf("feedid: payload carries no workspace id")
	}
	feed, err := decodeFeedFields(fields[1], fields[2])
	if err != nil {
		return "", Feed{}, RowKey{}, err
	}
	return ws, feed, RowKey{Kind: RowKind(fields[3]), ID: fields[4], Sub: fields[5]}, nil
}

// Encode renders a Ref as the opaque FeedId the frontend carries. The encoding
// is stable: the same Ref always yields the same value. A Ref the scheme
// cannot address — no workspace, no feed arm, an unknown row kind — returns
// nil, because a caller that would send a guessed id is the failure this
// package exists to prevent.
func Encode(ref Ref) *frontendv1.FeedId {
	if !knownKind(ref.Row.Kind) {
		return nil
	}
	id, err := encode(ref.WS, ref.Feed, ref.Row)
	if err != nil {
		return nil
	}
	return id
}

// Decode parses a FeedId back into its Ref. An id from a different scheme
// version, or one that does not round-trip, is an error the caller surfaces as
// a NotFound refusal rather than guessing.
func Decode(id *frontendv1.FeedId) (Ref, error) {
	ws, feed, row, err := decode(id)
	if err != nil {
		return Ref{}, err
	}
	if !knownKind(row.Kind) {
		return Ref{}, fmt.Errorf("feedid: unknown row kind %q", row.Kind)
	}
	return Ref{WS: ws, Feed: feed, Row: row}, nil
}

// EncodeFeed renders a Feed as the FeedId that addresses the feed itself,
// which is what OpenFeed's optional feed field carries. The root feed's
// encoding is the one OpenFeed omits.
func EncodeFeed(ws WorkspaceID, feed Feed) *frontendv1.FeedId {
	id, err := encode(ws, feed, RowKey{})
	if err != nil {
		return nil
	}
	return id
}

// DecodeFeed parses a feed-addressing FeedId. A subagent bubble row decodes to
// Feed{Agent: created}; a merge head decodes to Feed{Merge: lease}.
func DecodeFeed(id *frontendv1.FeedId) (WorkspaceID, Feed, error) {
	ws, feed, row, err := decode(id)
	if err != nil {
		return "", Feed{}, err
	}
	switch {
	case row.Kind == "" && row.ID == "" && row.Sub == "":
		// The feed's own address.
		return ws, feed, nil
	case row.Kind == KindActivity && row.Sub != "":
		// A subagent bubble row addresses the created agent's sub-feed.
		return ws, Feed{Agent: &conversationv1.AgentId{Value: row.Sub}}, nil
	case row.Kind == KindMergeHead:
		if row.ID == "" {
			return "", Feed{}, fmt.Errorf("feedid: merge head carries no lease id")
		}
		lease := LeaseID(row.ID)
		return ws, Feed{Merge: &lease}, nil
	default:
		return "", Feed{}, fmt.Errorf("feedid: row kind %q addresses no sub-feed", row.Kind)
	}
}
