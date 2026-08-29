// Package handler is the sidecar's record→record layer: pure functions with ZERO
// IO that turn decoded file records into the store entries shim-store persists.
//
// A handler owns a converter and does three things with a batch of frames:
// ATTRIBUTES each frame (whose book, which file, which offset), asks the
// converter what the record IS, and DEFERS the one record whose meaning depends
// on a line that may not be written yet.
//
// IT NEVER DECIDES WHETHER A RECORD IS INTERESTING. Curation is a downstream
// concern; ingestion's only job is that every byte on disk ends up in the store
// as a protobuf shape — as a page line, a run frame, or durable residue.
package handler

import (
	"path/filepath"
	"strings"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// Producer is the fixed WriteBatch producer identity for the sidecar.
const Producer = convert.Producer

// Context / Kind live in the tail package (tailer-owned attribution); aliased
// here so handler code reads naturally.
type Context = tail.Context

// attribute builds the conversion attribution for one frame.
//
// IDENTITY IS DERIVED FROM THE FILE, NOT FROM THE RECORD (R9). The main agent's
// id is the transcript FILE's session uuid — the `<session>.jsonl` basename —
// and never the per-record `sessionId`, which diverges from the runtime's answer
// in ~22% of records. Deriving it means a re-read after a restart lands on the
// same book with nothing to recover.
func attribute(ctx *Context, offset int64) convert.Attribution {
	main := mainAgentID(ctx)
	at := convert.Attribution{
		VendorSessionID: main,
		MainAgentID:     main,
		Path:            ctx.Path,
		Offset:          offset,
		TaskID:          ctx.TaskID,
	}
	switch ctx.Kind {
	case tail.KindSessionTranscript:
		// The session's own book is the main agent's.
		at.AgentID = main
	case tail.KindAgentTranscript:
		// A subagent's constituents form ITS OWN book. Its identity is the
		// vendor `agentId`, which the records carry and the file name repeats.
		at.AgentID = agentIDFromPath(ctx.Path)
		at.Backgrounded = backgroundedSpawn(ctx)
	}
	return at
}

// mainAgentID resolves the session's main agent.
//
// The tailer supplies it as the file-path attribution; when it did not, the path
// is the fallback, because a book that cannot be named is a record that cannot be
// served and that must not depend on a field the vendor rewrites.
func mainAgentID(ctx *Context) string {
	if ctx.SessionID != "" {
		return ctx.SessionID
	}
	return sessionIDFromPath(ctx.Path)
}

// sessionIDFromPath reads the session uuid out of a transcript path.
//
// `projects/<project>/<session>.jsonl` names it directly; a subagent transcript
// at `projects/<project>/<session>/subagents/agent-<id>.jsonl` names it as the
// directory two levels above.
func sessionIDFromPath(path string) string {
	if path == "" {
		return ""
	}
	base := strings.TrimSuffix(filepath.Base(path), ".jsonl")
	if !strings.HasPrefix(base, "agent-") {
		return base
	}
	// .../<session>/subagents/agent-<id>.jsonl
	return filepath.Base(filepath.Dir(filepath.Dir(path)))
}

// agentIDFromPath reads a subagent's vendor id out of its transcript file name.
func agentIDFromPath(path string) string {
	base := strings.TrimSuffix(filepath.Base(path), ".jsonl")
	return strings.TrimPrefix(base, "agent-")
}

// backgroundedSpawn reports whether this file's agent was spawned into the
// background, which makes it its OWN top_level rather than the session's main
// agent.
//
// DERIVED FROM WHAT THE SEAM SUPPLIES TODAY: a backgrounded agent is the one the
// vendor gives an `a*` task spool, so a task id in the `a` space is the evidence.
// A dedicated `SpawnBackgrounded` field on tail.Context would state it directly;
// until the reader supplies one this derivation is the honest available answer,
// and it is read defensively — an empty task id simply means "not backgrounded".
func backgroundedSpawn(ctx *Context) bool {
	return strings.HasPrefix(ctx.TaskID, "a")
}

// logResidue records every record that landed with no path to a page.
//
// IT IS THE RUNNING MEASURE of how much of what the vendor writes this schema
// does not carry. An unserved item is by construction not a servable frame, so it
// never reaches a page and is invisible to the user — a legitimate outcome, and
// the one worth counting.
func logResidue(log *logging.Bound, ctx *Context, entries []*storev1.StoreEntry) {
	for _, entry := range entries {
		if entry.GetAgentUpdate().GetUnservedItem() == nil {
			continue
		}
		log.With(logging.Context{Operation: "residue", Path: ctx.Path, Task: ctx.TaskID}).
			LogVerbose("record stored as an unserved item: %s", convert.Describe(entry))
	}
}

// lookahead returns the decoded record that FOLLOWS a frame in the file, or nil
// past the end of the batch. It exists for exactly one record: a compaction
// boundary, whose summary the harness writes as the following line.
func lookahead(frames []tail.Frame, i int) map[string]any {
	if i < 0 || i >= len(frames) {
		return nil
	}
	return frames[i].Obj
}
