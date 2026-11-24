// Package composerselections keeps every candidate the reader took from a
// composer's completion -- a slash command, a workspace name, a Slack channel
// -- which order the composer's completions in Emacs: the candidate taken most
// recently is offered first (agentrepl.v1.HostComposerSelections).
//
// THE RECORD IS THE USER'S. Emacs reports each taking (ReportComposerSelection)
// and the keeper holds one record for the whole daemon, so a command taken in
// one workspace's composer is offered first in every other's.
//
// THE DAEMON STAMPS THE INSTANT. Each selection is stamped as it is recorded,
// strictly later than every earlier stamp (the later of now and the last stamp
// plus one), so two selections never tie and a clock step backwards never
// reorders what was already taken.
//
// DURABLE BEFORE IT IS PUBLISHED. A selection is written to the workspace
// state (wsm's composer_selections) before the record in hand changes, so a
// failed write leaves the record and every subscriber as they were. New loads
// the whole record, and the one topic (Topic) is served on every workspace's
// host stream: the record in force first, then each change.
package composerselections

import (
	"context"
	"fmt"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/publish"
	"agent-repld/internal/recency"
	"agent-repld/internal/wsm"
)

// The keeper's operations, one per logical branch.
const (
	opLoad     = "daemon.composer_selections.load"
	opSelected = "daemon.composer_selections.selected"
)

// Store is the durable record (wsm).
type Store interface {
	// PutComposerSelection records a selection as last taken at its instant.
	PutComposerSelection(ctx context.Context, sel wsm.ComposerSelection) error
	// ComposerSelections loads every recorded selection.
	ComposerSelections(ctx context.Context) ([]wsm.ComposerSelection, error)
}

// Deps are the keeper's collaborators.
type Deps struct {
	// Store keeps the record across restarts.
	Store Store
	// Now is the wall clock a selection is stamped from.
	Now func() time.Time
	// Log is the keeper's canonical logger.
	Log dlog.Logger
}

// Keeper keeps the record of composer selections.
type Keeper struct {
	deps Deps

	mu     sync.Mutex
	record map[wsm.ComposerSelectionKind]map[string]int64
	last   int64
	topic  publish.Topic[*agentreplv1.HostComposerSelections]
}

// New builds a keeper holding the whole durable record, published when it
// holds a name. It panics on a missing collaborator, because a half-wired
// keeper is a boot defect; a record that cannot be read is logged and
// returned.
func New(ctx context.Context, deps Deps) (*Keeper, error) {
	if deps.Store == nil || deps.Now == nil || deps.Log == nil {
		panic("composerselections: New requires Store, Now and Log")
	}
	stored, err := deps.Store.ComposerSelections(ctx)
	if err != nil {
		deps.Log.Error(opLoad, "the recorded composer selections could not be read", dlog.WithCause(dlog.Context{}, err))
		return nil, fmt.Errorf("composerselections: load the record: %w", err)
	}
	k := &Keeper{deps: deps, record: map[wsm.ComposerSelectionKind]map[string]int64{}}
	for _, sel := range stored {
		k.putLocked(sel.Kind, sel.Name, sel.SelectedAtMs)
	}
	if len(stored) > 0 {
		k.topic.Publish(k.viewLocked())
	}
	deps.Log.Info(opLoad, "loaded the recorded composer selections", dlog.Context{"selections": len(stored)})
	return k, nil
}

// Selected records that the reader took NAME from a composer's KIND
// completion, as the newest selection of all: it is stamped, written durably,
// then held and published. A failed write is logged and returned, and leaves
// the record in hand and its subscribers as they were. KIND and NAME are
// validated by the caller; a malformed one is refused by the store.
func (k *Keeper) Selected(ctx context.Context, kind wsm.ComposerSelectionKind, name string) error {
	k.mu.Lock()
	defer k.mu.Unlock()
	at := max(k.deps.Now().UnixMilli(), k.last+1)
	fields := dlog.Context{"kind": string(kind), "name": name, "selected_at_ms": at}
	if err := k.deps.Store.PutComposerSelection(ctx, wsm.ComposerSelection{Kind: kind, Name: name, SelectedAtMs: at}); err != nil {
		k.deps.Log.Error(opSelected, "the composer selection could not be recorded; the order stands as it was", dlog.WithCause(fields, err))
		return err
	}
	k.putLocked(kind, name, at)
	k.topic.Publish(k.viewLocked())
	k.deps.Log.Debug(opSelected, "recorded the composer selection", fields)
	return nil
}

// Topic answers the record's topic, holding the record in force so every
// subscriber is handed it first. A record that never held a name publishes
// none: the record only grows, so an empty one tells a subscriber nothing its
// absence does not.
func (k *Keeper) Topic() *publish.Topic[*agentreplv1.HostComposerSelections] {
	return &k.topic
}

// putLocked sets NAME's instant of KIND to AT and moves the last stamp
// forward to it. Called with k.mu held, or before the keeper is shared.
func (k *Keeper) putLocked(kind wsm.ComposerSelectionKind, name string, at int64) {
	names, ok := k.record[kind]
	if !ok {
		names = map[string]int64{}
		k.record[kind] = names
	}
	names[name] = at
	k.last = max(k.last, at)
}

// viewLocked is the record as the host stream carries it: each kind's names,
// the most recently taken first. Called with k.mu held.
func (k *Keeper) viewLocked() *agentreplv1.HostComposerSelections {
	return &agentreplv1.HostComposerSelections{
		Commands:   recency.NewestFirst(k.record[wsm.ComposerSelectionCommand]),
		Workspaces: recency.NewestFirst(k.record[wsm.ComposerSelectionWorkspace]),
		Channels:   recency.NewestFirst(k.record[wsm.ComposerSelectionChannel]),
	}
}
