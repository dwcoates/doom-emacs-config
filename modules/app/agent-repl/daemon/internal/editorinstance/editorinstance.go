// Package editorinstance tells a FULL EMACS RESTART apart from a reconnect.
//
// Every Emacs WatchDaemon carries the Emacs process's identity
// (agentrepl.v1.WatchDaemonEmacs.instance), minted once when Emacs starts and
// the same on every stream it opens until it exits. The daemon records the
// last one it saw DURABLY (wsm's editor_instance), so a daemon restart under a
// live Emacs reads that Emacs's reconnect as the same Emacs, and only a new
// process reads as new. A new Emacs begins agent-repl's session here (Starts).
// What else a new Emacs is owed (the day's digest stood
// again; the startup bring-up) is the caller's to do.
//
// Only the daemon that SERVES, on a WRITING state handle, judges: a joining
// successor holds a read-only state client and the incumbent already recorded
// the Emacs that is attaching to it, so a successor answers "not new" without
// writing. Both are asked because they flip at different moments: a successor
// can serve intake before its handle is promoted, and a write on the read-only
// handle would fail the attaching Emacs's stream (2026-10-07, the e2e handover
// at freeness never promoted). A handle never returns to read-only once
// promoted, so asking it first cannot race the write.
package editorinstance

import (
	"context"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
)

// op is the operation every record here is written under.
const op = "daemon.editorinstance.connected"

// Starts is told of every new Emacs process: it begins agent-repl's session
// (internal/agentreplsession).
type Starts interface {
	EditorStarted(ctx context.Context, at time.Time)
}

// Store is the durable record of the last Emacs instance seen.
type Store interface {
	// NoteEditorInstance records instance and answers whether it differs from
	// the last one recorded.
	NoteEditorInstance(ctx context.Context, instance string, at time.Time) (bool, error)
	// ReadOnly reports whether the handle refuses writes right now.
	ReadOnly() bool
}

// Tracker judges each Emacs WatchDaemon's instance.
type Tracker struct {
	store  Store
	serves func() bool
	now    func() time.Time
	log    dlog.Logger
	starts Starts
}

// New builds a tracker, refusing a missing collaborator.
func New(store Store, serves func() bool, now func() time.Time, log dlog.Logger, starts Starts) (*Tracker, error) {
	switch {
	case store == nil:
		return nil, errors.New("editorinstance: a store is required")
	case serves == nil:
		return nil, errors.New("editorinstance: a serving test is required")
	case now == nil:
		return nil, errors.New("editorinstance: a clock is required")
	case log == nil:
		return nil, errors.New("editorinstance: a logger is required")
	case starts == nil:
		return nil, errors.New("editorinstance: a session to begin is required")
	}
	return &Tracker{store: store, serves: serves, now: now, log: log, starts: starts}, nil
}

// Connected records the instance an Emacs WatchDaemon carried and answers
// whether it is a NEW Emacs process. An error is the store failing, recorded
// here at ERROR.
func (t *Tracker) Connected(ctx context.Context, instance string) (bool, error) {
	fields := dlog.Context{"instance": instance}
	if !t.serves() {
		t.log.Debug(op, "this daemon does not serve yet; an attaching Emacs is the one the incumbent recorded", fields)
		return false, nil
	}
	if t.store.ReadOnly() {
		t.log.Debug(op, "this daemon serves but its state handle is not promoted yet; an attaching Emacs is the one the incumbent recorded", fields)
		return false, nil
	}
	at := t.now()
	isNew, err := t.store.NoteEditorInstance(ctx, instance, at)
	if err != nil {
		fields["cause"] = err.Error()
		t.log.Error(op, "the Emacs instance could not be recorded", fields)
		return false, fmt.Errorf("editorinstance: record %q: %w", instance, err)
	}
	if isNew {
		t.log.Info(op, "a new Emacs process connected", fields)
		// A NEW EMACS BEGINS AGENT-REPL'S SESSION; a reconnect, and a daemon
		// restart under the same Emacs, keep the one standing.
		t.starts.EditorStarted(ctx, at)
	} else {
		t.log.Debug(op, "the same Emacs process reconnected", fields)
	}
	return isNew, nil
}
