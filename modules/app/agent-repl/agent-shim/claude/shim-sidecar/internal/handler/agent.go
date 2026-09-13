package handler

// agent.go — a subagent's sidechain transcript, which is the same JSONL shape as
// a session transcript and is therefore read by the same converter.
//
// WHAT DIFFERS IS THE BOOK, AND ONLY THE BOOK. A subagent's constituents form ITS
// OWN book, keyed by the vendor `agentId`; the SPAWN that created it is a line in
// the PARENT's book. Nothing has to be correlated with the session transcript
// that launched it and nothing has to survive a restart for the association to
// hold, because both identities are derived from the file path.
//
// A sidechain can itself launch detached work — a subagent spawning a subagent —
// and those launches are read off the tool result exactly as in any other file.

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// AgentTranscriptHandler reads a subagent's sidechain transcript.
type AgentTranscriptHandler struct {
	conv *convert.Converter
	log  *logging.Bound
	// obs is the reader's callbacks, adopted one at a time and installed on the
	// converter once, at construction: a converter has exactly one observer, and
	// two independent adoptions must not overwrite each other.
	obs *seamObserver
	// coords are the FILE COORDINATES of the last batch this handler read, and
	// mainAgent the agent whose stream the file belongs to.
	//
	// THEY EXIST FOR THE SEAM-MINTED TERMINAL, exactly as the shell handler's
	// do. A backgrounded subagent arrives through an `a*` task spool, and a
	// terminal the READER concludes for it (a LOST sweep) is built from an
	// attribution that must carry a real write identity: without one, every such
	// terminal in the process would digest "producer||0|settle:<run>" and the
	// store — whose absorption IS write_id equality — would swallow the second
	// as a replay of the first.
	coords    fileCoords
	mainAgent string
}

// NewAgentTranscriptHandler builds a handler with its own converter.
func NewAgentTranscriptHandler(log *logging.Bound) *AgentTranscriptHandler {
	log.With(logging.Context{Operation: "agent-handler-new"}).LogVerbose("constructing agent transcript handler")
	obs := &seamObserver{}
	conv := convert.New(log)
	conv.SetObserver(obs)
	return &AgentTranscriptHandler{conv: conv, log: log, obs: obs}
}

// Handle implements tail.Handler.
func (h *AgentTranscriptHandler) Handle(frames []tail.Frame, ctx *Context) []*storev1.StoreEntry {
	h.obs.bind(ctx)
	h.log.With(handleCtx("agent-handle", ctx)).
		LogVerbose("handling frames=%d records_observed=%d", len(frames), ctx.RecordsObserved)
	// WHERE WE READ IS A READER FACT, not a conversion outcome, so it is
	// remembered before the identity check below: a spool whose book never
	// resolved was still READ, and a terminal minted for it must say so at a
	// real position.
	h.rememberCoords(ctx)
	if ctx.AgentID == "" {
		// A SIDECHAIN'S BOOK IS ITS AGENT, and its agent is the spawning call
		// named in the meta file. A transcript whose meta has not been read is
		// HELD by the reader, so arriving here without an identity means the hold
		// was skipped — and converting anyway would put this agent's whole book
		// under the empty id, or under its filename, which the other plane would
		// never agree with. Refused, loudly, rather than mis-filed.
		h.log.With(handleErr("agent-handle", ctx)).Log(
			"subagent transcript reached the handler with no agent identity; its meta file names the spawning call and must be read before it is tailed, so nothing is converted for it")
		return nil
	}
	out := convertFrames(h.conv, h.log, frames, ctx)
	h.log.With(handleCtx("agent-handle", ctx)).
		LogVerbose("handled frames=%d entries=%d", len(frames), len(out))
	return out
}

// rememberCoords records where this handler has read to, so a terminal the
// READER concludes can be stated at a real file position rather than at the
// zero value every such terminal would otherwise share.
func (h *AgentTranscriptHandler) rememberCoords(ctx *Context) {
	h.coords = fileCoords{Path: ctx.Path, FileID: ctx.FileID, Offset: ctx.BytesObserved}
	if ctx.MainAgentID != "" {
		h.mainAgent = ctx.MainAgentID
	}
}

// Conv exposes the handler's converter, so the reader can read the per-file
// facts the conversion accumulated — the never-persisted residue tally the
// catch-up summary states.
func (h *AgentTranscriptHandler) Conv() *convert.Converter { return h.conv }
