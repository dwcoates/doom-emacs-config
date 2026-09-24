// Package dlog is the daemon's one logging API.
//
// Nothing in the daemon writes a diagnostic through fmt, log, slog or an ad
// hoc logger; every record goes through a Logger obtained here. The durable
// sink is authoritative and synchronous, the terminal is a mirror and is not.
// See daemon/AGENTS.md "Logging" and docs/overhaul/daemon.md.
package dlog

import "os"

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
// component: the global service log, the size-rotated run log, and the one
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
	// BindWorkspaceIDs installs the lookup from a workspace directory to that
	// workspace's daemon-minted ids.WorkspaceID. Every workspace record's
	// workspace_id and every minted sink name comes from it, so it is bound
	// once the state client is open and before any workspace-owned record.
	// Until it is bound, Workspace, ShimSink and ClientLog all refuse: a
	// workspace record is never attributed to a path-derived stand-in.
	BindWorkspaceIDs(lookup WorkspaceIDLookup)
	// Workspace resolves the logger whose durable sink is
	// <dir>/.claude/emacs/daemon.log (via the canonical symlink). It fails
	// rather than falling back to the global sink.
	Workspace(dir string) (Logger, error)
	// WorkspaceOrCentral answers a workspace's logger TOTALLY: the workspace's
	// own durable sink when its directory can host one, and otherwise the
	// central sink with `unroutable_workspace` naming the workspace the record
	// is about. It exists for the callers whose work must not stop because one
	// workspace's directory is a scratch path or has been deleted — the idle
	// sweep, the request boundary — and the condition is recorded once per
	// workspace rather than once per record.
	WorkspaceOrCentral(dir string) Logger
	// ShimSink borrows the already-open shim log sink for one workspace, to be
	// passed as the spawned shim's fd 3. The handle is non-closeable by the
	// borrower; the surfaces own its lifetime.
	ShimSink(dir string) (Borrowed, error)
	// ShimRollRequests carries one request when a shim target first reaches its
	// hard ceiling. The consumer relaunches that workspace's shim through the
	// ordinary turn-boundary roll; no request is repeated for the same target.
	ShimRollRequests() <-chan ShimRollRequest
	// ClientLog persists a console-less client's diagnostic record into that
	// workspace's durable sink (the ClientLog rpc's landing place).
	ClientLog(dir string, record ClientRecord) error
	// Evict releases one workspace's sinks when the workspace closes. The
	// canonical links and their targets stay on disk; only the descriptors go.
	// Evicting a workspace with no open sinks is success.
	Evict(dir string) error
	// DetachDir marks a workspace directory the daemon is about to REMOVE: no
	// sink of it creates, re-points or reads anything inside it from then on,
	// and its records keep landing in the same daemon-owned targets.
	DetachDir(dir string) error
	// AttachDir lifts DetachDir once a worktree exists at that path again.
	AttachDir(dir string) error
	// Close flushes and closes every sink the daemon opened.
	Close() error
}

// ShimRollRequest asks the daemon's session owner to replace a shim whose log
// target reached the hard ceiling. Dir is the registry lookup key; the log id
// is included for diagnostics and never substituted for a workspace id.
type ShimRollRequest struct {
	Dir       string
	LogID     string
	SizeBytes int64
	HardBytes int64
	// Log is already bound to the owning workspace. The consumer must use it
	// for every result so a failed registry lookup cannot misroute the error to
	// the global run log.
	Log Logger
}

// Borrowed is a non-closeable handle on a sink the surfaces own. Close is a
// no-op so a borrower's defer cannot take the sink down under its owner.
type Borrowed interface {
	// File is the sink's own open file, suitable for a child's fd 3 via
	// exec.Cmd.ExtraFiles. It is the surfaces' file, NOT a copy: a borrower
	// must never wrap the descriptor in an os.File of its own, because that
	// second owner's finalizer would close the sink's fd out from under
	// everybody (and the freed fd number is then handed to unrelated opens).
	File() *os.File
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
	// Timestamp is the instant the CLIENT observed, in RFC 3339. It may carry
	// any offset, including a UTC "Z": the daemon parses it and renders it in
	// the local zone before persisting, so a forwarded record interleaves with
	// the daemon records around it. Empty means the client sent none and the
	// daemon stamps its arrival instead, saying so in the record's context.
	Timestamp string
	// Verbose is the record's own verbosity class. It is independent of
	// severity: a client may emit an informational record only while tracing.
	Verbose bool
}
