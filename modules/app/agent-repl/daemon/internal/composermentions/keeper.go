// Package composermentions keeps the names each workspace's own final
// responses mentioned, which order the composer's completions in Emacs: a
// slash command or a workspace the workspace itself talked about is offered
// before one it never did, the most recently mentioned first
// (agentrepl.v1.HostWorkspaceMentions).
//
// READ WHEN THE ANSWER IS RECORDED. The feed resolver hands every final
// response's settled markdown here as it records the response its turn's
// answer (feed.Deps.AnswerSettled), live and on every replay alike. Its slash
// words (SlashWords) and the open workspaces' names it mentions (Mentions) are
// the mentions.
//
// A MENTION'S INSTANT IS THE RESPONSE'S. Each name keeps the settle instant of
// the latest response that mentioned it, and an instant only ever moves
// forward, so the order is the same whatever order responses are read in: a
// boot replaying an older response never makes its names newer, and two
// answers read concurrently cannot race.
//
// DURABLE, SO A RESTART HYDRATES EMACS. Every change is written to the
// workspace state (wsm's composer_mentions) and New loads the whole record,
// so the record outlives the newest page a boot replays. Each workspace's
// record is published on its own topic (Topic), which the host stream serves
// to Emacs: the record in force first, then each change.
package composermentions

import (
	"cmp"
	"context"
	"fmt"
	"slices"
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
	"agent-repld/internal/wsm"
)

// The keeper's operations, one per logical branch.
const (
	opLoad     = "daemon.composer_mentions.load"
	opAnswered = "daemon.composer_mentions.answered"
)

// Store is the durable record (wsm).
type Store interface {
	// PutComposerMentions records mentions, each instant only moving forward.
	PutComposerMentions(ctx context.Context, mentions []wsm.ComposerMention) error
	// ComposerMentions loads every workspace's recorded mentions.
	ComposerMentions(ctx context.Context) ([]wsm.ComposerMention, error)
}

// Deps are the keeper's collaborators.
type Deps struct {
	// Store keeps the record across restarts.
	Store Store
	// WorkspaceNames answers the names of the workspaces open now, which a
	// response's words are matched against. Called off every lock the
	// keeper holds.
	WorkspaceNames func() []string
	// Log is the keeper's canonical logger.
	Log dlog.Logger
}

// record is one workspace's mentions: each kind's names, by the instant
// (epoch ms) of the latest response mentioning each.
type record map[wsm.ComposerMentionKind]map[string]int64

// Keeper keeps every workspace's mentions.
type Keeper struct {
	deps Deps

	mu      sync.Mutex
	records map[ids.WorkspaceID]record
	topics  map[ids.WorkspaceID]*publish.Topic[*agentreplv1.HostWorkspaceMentions]
}

// New builds a keeper holding the whole durable record. It panics on a
// missing collaborator, because a half-wired keeper is a boot defect; a
// record that cannot be read is logged and returned.
func New(ctx context.Context, deps Deps) (*Keeper, error) {
	if deps.Store == nil || deps.WorkspaceNames == nil || deps.Log == nil {
		panic("composermentions: New requires Store, WorkspaceNames and Log")
	}
	stored, err := deps.Store.ComposerMentions(ctx)
	if err != nil {
		deps.Log.Error(opLoad, "the recorded composer mentions could not be read", dlog.WithCause(dlog.Context{}, err))
		return nil, fmt.Errorf("composermentions: load the record: %w", err)
	}
	k := &Keeper{
		deps:    deps,
		records: map[ids.WorkspaceID]record{},
		topics:  map[ids.WorkspaceID]*publish.Topic[*agentreplv1.HostWorkspaceMentions]{},
	}
	for _, m := range stored {
		k.recordLocked(m.Workspace).put(m.Kind, m.Name, m.MentionedAtMs)
	}
	deps.Log.Info(opLoad, "loaded the recorded composer mentions",
		dlog.Context{"mentions": len(stored), "workspaces": len(k.records)})
	return k, nil
}

// Answered records the mentions of MARKDOWN, workspace WS's final response
// that settled at SETTLEDATMS (epoch ms), publishes the workspace's record
// when it changed, and writes the change durably. A response with no settle
// instant is a defect upstream, logged and recorded nowhere; a failed write
// is logged, and the record in hand keeps the change.
func (k *Keeper) Answered(ws ids.WorkspaceID, markdown string, settledAtMs int64) {
	fields := dlog.Context{"workspace": string(ws), "settled_at_ms": settledAtMs}
	if settledAtMs <= 0 {
		k.deps.Log.Error(opAnswered, "a final response reached the composer mentions with no settle instant; its mentions are not recorded", fields)
		return
	}
	found := map[wsm.ComposerMentionKind][]string{
		wsm.ComposerMentionCommand:   SlashWords(markdown),
		wsm.ComposerMentionWorkspace: Mentions(markdown, k.deps.WorkspaceNames()),
	}

	k.mu.Lock()
	rec := k.recordLocked(ws)
	var changed []wsm.ComposerMention
	for kind, names := range found {
		for _, name := range names {
			if rec.put(kind, name, settledAtMs) {
				changed = append(changed, wsm.ComposerMention{Workspace: ws, Kind: kind, Name: name, MentionedAtMs: settledAtMs})
			}
		}
	}
	if len(changed) > 0 {
		k.topicLocked(ws).Publish(rec.view())
	}
	k.mu.Unlock()

	fields["commands"] = len(found[wsm.ComposerMentionCommand])
	fields["workspaces"] = len(found[wsm.ComposerMentionWorkspace])
	fields["changed"] = len(changed)
	if len(changed) == 0 {
		k.deps.Log.Debug(opAnswered, "the final response mentioned nothing newer than the record holds", fields)
		return
	}
	if err := k.deps.Store.PutComposerMentions(context.Background(), changed); err != nil {
		k.deps.Log.Error(opAnswered, "the final response's mentions could not be recorded durably; a restart forgets them", dlog.WithCause(fields, err))
		return
	}
	k.deps.Log.Debug(opAnswered, "recorded the final response's mentions", fields)
}

// Topic answers workspace WS's mentions topic, holding its record in force so
// every subscriber is handed the record first. A workspace that never
// mentioned anything holds none: a record only grows, so there is nothing an
// empty one would tell a subscriber that its absence does not.
func (k *Keeper) Topic(ws ids.WorkspaceID) *publish.Topic[*agentreplv1.HostWorkspaceMentions] {
	k.mu.Lock()
	defer k.mu.Unlock()
	return k.topicLocked(ws)
}

// recordLocked answers WS's record, creating it empty. Called with k.mu held,
// or before the keeper is shared.
func (k *Keeper) recordLocked(ws ids.WorkspaceID) record {
	rec, ok := k.records[ws]
	if !ok {
		rec = record{}
		k.records[ws] = rec
	}
	return rec
}

// topicLocked answers WS's topic, creating it with WS's record published when
// it holds a name. Called with k.mu held.
func (k *Keeper) topicLocked(ws ids.WorkspaceID) *publish.Topic[*agentreplv1.HostWorkspaceMentions] {
	topic, ok := k.topics[ws]
	if !ok {
		topic = &publish.Topic[*agentreplv1.HostWorkspaceMentions]{}
		if rec := k.recordLocked(ws); !rec.empty() {
			topic.Publish(rec.view())
		}
		k.topics[ws] = topic
	}
	return topic
}

// empty reports whether the record holds no name.
func (r record) empty() bool {
	for _, names := range r {
		if len(names) > 0 {
			return false
		}
	}
	return true
}

// put moves NAME's instant of KIND forward to AT, reporting whether it moved.
func (r record) put(kind wsm.ComposerMentionKind, name string, at int64) bool {
	names, ok := r[kind]
	if !ok {
		names = map[string]int64{}
		r[kind] = names
	}
	if names[name] >= at {
		return false
	}
	names[name] = at
	return true
}

// view is the record as the host stream carries it: each kind's names, the
// most recently mentioned first, a tie by name.
func (r record) view() *agentreplv1.HostWorkspaceMentions {
	return &agentreplv1.HostWorkspaceMentions{
		Commands:   newestFirst(r[wsm.ComposerMentionCommand]),
		Workspaces: newestFirst(r[wsm.ComposerMentionWorkspace]),
	}
}

// newestFirst answers NAMES's keys, the latest instant first, a tie by name.
func newestFirst(names map[string]int64) []string {
	out := make([]string, 0, len(names))
	for name := range names {
		out = append(out, name)
	}
	slices.SortFunc(out, func(a, b string) int {
		return cmp.Or(cmp.Compare(names[b], names[a]), cmp.Compare(a, b))
	})
	return out
}
