// Package dlog is the daemon's one logging API.
//
// Nothing in the daemon writes a diagnostic through fmt, log, slog or an ad
// hoc logger; every record goes through a Logger obtained here. The durable
// sink is authoritative and synchronous, the terminal is a mirror and is not.
// See daemon/AGENTS.md "Logging" and docs/overhaul/daemon.md.
package dlog

import (
	"claude-repld/internal/notimpl"
)

// Context is one record's structured context: the resolved inputs, the branch
// taken, and the cause. Values are JSON-encodable scalars, slices or maps.
type Context map[string]any

// Logger emits the daemon's canonical JSONL records. Every method takes the
// operation name (of the form "daemon.<package>.<verb>"), a human-readable
// message, and the record's structured context.
//
// An implementation persists synchronously to its resolved durable sink and
// mirrors to the shared terminal sink without blocking on it.
type Logger interface {
	// Debug records the ordinary path: function entry, and each branch that
	// selects a different nontrivial block, call, state transition or outcome.
	Debug(operation, message string, ctx Context)
	// Info records a notable but unexceptional milestone.
	Info(operation, message string, ctx Context)
	// Warn records a warning, including the unlanded-error-arm refusals whose
	// operation is "daemon.refusal.unlanded_arm".
	Warn(operation, message string, ctx Context)
	// Error records a failure, exactly once, by the layer that owns it.
	Error(operation, message string, ctx Context)
	// With returns a Logger that stamps every record with these context keys in
	// addition to the per-record ones. It is how a workspace-bound component
	// carries its workspace and session identity.
	With(ctx Context) Logger
}

// Surfaces is the set of sinks the daemon opens at boot and hands to every
// component: the global service log, the restart-scoped run log, and the one
// shared terminal mirror. A workspace-bound Logger comes from Workspace.
//
// ARCHITECTURE.md fixes the responsibilities but not the fields; the minimum
// the contract implies is a resolver from a workspace directory to that
// workspace's durable sink, plus the global logger for genuinely
// workspace-less events, so those two are what this interface exposes.
type Surfaces interface {
	// Global is the service logger. Only events conceptually unrelated to
	// every workspace and agent session may use it: failing to resolve a known
	// workspace is an invariant violation, never a reason to write globally.
	Global() Logger
	// Workspace resolves the logger whose durable sink is
	// <dir>/.claude/emacs/daemon.log (via the canonical symlink). It fails
	// rather than falling back to the global sink.
	Workspace(dir string) (Logger, error)
	// ShimSink borrows the already-open shim log sink for one workspace, to be
	// passed as the spawned shim's fd 3. The handle is non-closeable by the
	// borrower; the surfaces own its lifetime.
	ShimSink(dir string) (Borrowed, error)
	// ClientLog persists a console-less client's diagnostic record into that
	// workspace's durable sink (the ClientLog rpc's landing place).
	ClientLog(dir string, record ClientRecord) error
	// Close flushes and closes every sink the daemon opened.
	Close() error
}

// Borrowed is a non-closeable handle on a sink the surfaces own. Close is a
// no-op so a borrower's defer cannot take the sink down under its owner.
type Borrowed interface {
	// File is the underlying descriptor, suitable for a child's fd 3.
	File() uintptr
	// Close is a no-op; the surfaces own the sink's lifetime.
	Close() error
}

// ClientRecord is one client-supplied diagnostic, as the ClientLog rpc carries
// it. ARCHITECTURE.md does not fix its fields; the minimum the contract
// implies is the client's kind, the level, the message and its context.
type ClientRecord struct {
	// ClientKind names the client that produced the record ("webapp",
	// "sidecar", "host").
	ClientKind string
	// Level is the record's severity as the client classified it.
	Level string
	// Operation is the client's operation name, already namespaced.
	Operation string
	// Message is the human-readable line.
	Message string
	// Context is the record's structured context.
	Context Context
}

// OpenSurfaces opens the daemon's log surfaces under the state root's logs
// directory. runLog is the restart-scoped run log path; verbose gates the
// terminal mirror for verbose records.
func OpenSurfaces(runLog string, verbose bool) (Surfaces, error) {
	return nil, notimpl.Err
}
