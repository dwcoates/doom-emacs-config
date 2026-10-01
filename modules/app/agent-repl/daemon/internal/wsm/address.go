package wsm

import (
	"encoding/json"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
)

// Feed addresses are persisted as their STRUCTURE, not as an encoded FeedId.
// A FeedId is the frontend's opaque wire spelling and its scheme may change;
// the durable record keeps the parts so a stored address never depends on the
// encoder's version.
type storedFeed struct {
	// Root marks the workspace's top-level feed.
	Root bool `json:"root,omitempty"`
	// Agent is the subagent sub-feed's agent id, nil when the feed is not one.
	Agent *string `json:"agent,omitempty"`
	// Merge is the merge sub-feed's owning lease, nil when the feed is not one.
	Merge *string `json:"merge,omitempty"`
	// Shell is the detached shell sub-feed's owning work id, nil when the feed
	// is not one.
	Shell *string `json:"shell,omitempty"`
}

// storedRow is a row key's persisted form.
type storedRow struct {
	// Kind is the row's kind.
	Kind string `json:"kind"`
	// ID is the kind's primary key.
	ID string `json:"id"`
	// Sub is the kind's secondary key, empty when the kind has none.
	Sub string `json:"sub,omitempty"`
}

// storedRef is a fully qualified row address's persisted form.
type storedRef struct {
	// WS is the workspace the row belongs to.
	WS string `json:"ws"`
	// Feed is the feed within that workspace.
	Feed storedFeed `json:"feed"`
	// Row identifies the row within that feed.
	Row storedRow `json:"row"`
}

// storedAddress is an output address's persisted form.
//
// A RETIRED KEY IS IGNORED ON READ. Addresses written before the root-feed
// mirror was removed may carry `"mirror": true`; the decoder skips a key this
// struct does not name, so such a row reads as the address it names, and no
// row draws on the root feed by it any longer. Nothing is migrated: the value
// is one JSON column, and the next write of the row drops the key.
type storedAddress struct {
	// Feed is the feed rows land in.
	Feed storedFeed `json:"feed"`
	// Parent is the row they nest under, nil for top-level rows.
	Parent *storedRef `json:"parent,omitempty"`
}

// toStoredFeed renders a feed, refusing one that does not name exactly one arm.
func toStoredFeed(f feedid.Feed) (storedFeed, error) {
	out := storedFeed{Root: f.Root}
	arms := 0
	if f.Root {
		arms++
	}
	if f.Agent != nil {
		arms++
		v := f.Agent.GetValue()
		out.Agent = &v
	}
	if f.Merge != nil {
		arms++
		v := string(*f.Merge)
		out.Merge = &v
	}
	if f.Shell != nil {
		arms++
		v := string(*f.Shell)
		out.Shell = &v
	}
	if arms != 1 {
		return storedFeed{}, fmt.Errorf("wsm: a feed address names exactly one of root, agent, merge or shell, got %d", arms)
	}
	return out, nil
}

// fromStoredFeed rebuilds a feed, failing the read when the stored value does
// not name exactly one arm.
func fromStoredFeed(table, row string, s storedFeed) (feedid.Feed, error) {
	out := feedid.Feed{Root: s.Root}
	arms := 0
	if s.Root {
		arms++
	}
	if s.Agent != nil {
		arms++
		out.Agent = &conversationv1.AgentId{Value: *s.Agent}
	}
	if s.Merge != nil {
		arms++
		lease := LeaseID(*s.Merge)
		out.Merge = &lease
	}
	if s.Shell != nil {
		arms++
		shell := feedid.ShellID(*s.Shell)
		out.Shell = &shell
	}
	if arms != 1 {
		return feedid.Feed{}, &DecodeError{Table: table, Row: row, Field: "feed", Err: fmt.Errorf("a feed address names exactly one of root, agent, merge or shell, got %d", arms)}
	}
	return out, nil
}

// toStoredRef renders a row address.
func toStoredRef(ref feedid.Ref) (storedRef, error) {
	feed, err := toStoredFeed(ref.Feed)
	if err != nil {
		return storedRef{}, err
	}
	if ref.Row.Kind == "" {
		return storedRef{}, errors.New("wsm: a row address carries a row kind")
	}
	return storedRef{
		WS:   string(ref.WS),
		Feed: feed,
		Row:  storedRow{Kind: string(ref.Row.Kind), ID: ref.Row.ID, Sub: ref.Row.Sub},
	}, nil
}

// fromStoredRef rebuilds a row address, failing the read on an unknown row kind
// — the kinds are a closed set, so an unrecognized one is corruption.
func fromStoredRef(table, row string, s storedRef) (feedid.Ref, error) {
	feed, err := fromStoredFeed(table, row, s.Feed)
	if err != nil {
		return feedid.Ref{}, err
	}
	kind := feedid.RowKind(s.Row.Kind)
	if !knownRowKind(kind) {
		return feedid.Ref{}, &DecodeError{Table: table, Row: row, Field: "row_kind", Err: fmt.Errorf("unknown row kind %q", s.Row.Kind)}
	}
	return feedid.Ref{WS: WorkspaceID(s.WS), Feed: feed, Row: feedid.RowKey{Kind: kind, ID: s.Row.ID, Sub: s.Row.Sub}}, nil
}

// rowKinds is the closed set feedid declares. A stored kind outside it is a
// decode failure, never a row silently rendered as something else.
var rowKinds = map[feedid.RowKind]struct{}{
	feedid.KindPrompt:           {},
	feedid.KindActivity:         {},
	feedid.KindTurnEnded:        {},
	feedid.KindPermission:       {},
	feedid.KindQuestion:         {},
	feedid.KindSeparation:       {},
	feedid.KindColdGate:         {},
	feedid.KindMergeHead:        {},
	feedid.KindMergeTab:         {},
	feedid.KindDetachedSubagent: {},
	feedid.KindDetachedShell:    {},
	feedid.KindShellHead:        {},
	feedid.KindSynth:            {},
}

// knownRowKind reports whether kind is one of feedid's declared kinds.
func knownRowKind(kind feedid.RowKind) bool {
	_, ok := rowKinds[kind]
	return ok
}

// encodeRef renders an optional row address for its nullable column.
func encodeRef(ref *feedid.Ref) (any, error) {
	if ref == nil {
		return nil, nil
	}
	stored, err := toStoredRef(*ref)
	if err != nil {
		return nil, err
	}
	raw, err := json.Marshal(stored)
	if err != nil {
		return nil, fmt.Errorf("wsm: encode row address: %w", err)
	}
	return string(raw), nil
}

// decodeRef parses an optional row address from its nullable column.
func decodeRef(table, row string, raw string) (*feedid.Ref, error) {
	var stored storedRef
	if err := json.Unmarshal([]byte(raw), &stored); err != nil {
		return nil, &DecodeError{Table: table, Row: row, Field: "target", Err: err}
	}
	ref, err := fromStoredRef(table, row, stored)
	if err != nil {
		return nil, err
	}
	return &ref, nil
}

// encodeAddress renders an optional output address for its nullable column.
func encodeAddress(addr *OutputAddress) (any, error) {
	if addr == nil {
		return nil, nil
	}
	feed, err := toStoredFeed(addr.Feed)
	if err != nil {
		return nil, err
	}
	stored := storedAddress{Feed: feed}
	if addr.Parent != nil {
		parent, err := toStoredRef(*addr.Parent)
		if err != nil {
			return nil, err
		}
		stored.Parent = &parent
	}
	raw, err := json.Marshal(stored)
	if err != nil {
		return nil, fmt.Errorf("wsm: encode output address: %w", err)
	}
	return string(raw), nil
}

// decodeAddress parses an optional output address from its nullable column.
func decodeAddress(table, row string, raw string) (*OutputAddress, error) {
	var stored storedAddress
	if err := json.Unmarshal([]byte(raw), &stored); err != nil {
		return nil, &DecodeError{Table: table, Row: row, Field: "address", Err: err}
	}
	feed, err := fromStoredFeed(table, row, stored.Feed)
	if err != nil {
		return nil, err
	}
	out := &OutputAddress{Feed: feed}
	if stored.Parent != nil {
		parent, err := fromStoredRef(table, row, *stored.Parent)
		if err != nil {
			return nil, err
		}
		out.Parent = &parent
	}
	return out, nil
}
