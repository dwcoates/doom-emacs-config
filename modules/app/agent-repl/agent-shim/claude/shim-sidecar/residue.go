// residue.go holds the handler for a CLAIMED spool whose kind is recognized
// but whose conversion deliberately does not exist yet — the w* workflow spool,
// with workflow kicked for this wave (R-S4).
//
// Its bytes are declared residue and are never persisted (cycle.go
// withholdResidue); what it exists for is the TERMINAL a claimed run is owed.
// An UNCLAIMED spool, and one whose task id carries no a/b/w prefix, is never
// read at all (held.go): nothing renders it.
package main

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/handler"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// declaredResidueHandler ingests a file whose kind IS recognized but whose
// conversion deliberately does not exist yet — the w* workflow spool, with
// workflow kicked for this wave (R-S4).
//
// IT IS NOT residueHandler. That one states "no conversion could be selected",
// which is a failure to classify; this one states a DECLARED kind, so a reader
// can tell the two apart without reading prose: the bytes land as
// `vendor_specific{kind}` rather than as `unparsed`, and the day workflow
// ingestion lands, every one of these rows is findable by that kind.
//
// IT OWES A STOPPED RUN ITS TERMINAL FOR THE SAME REASON residueHandler DOES,
// and the reason is not about the bytes. A w* spool that reaches THIS handler
// rather than the demoted residue one is a spool a spawning call CLAIMED — the
// unowned ones lapse and are demoted — so the reader knows the run and its
// owner, and a unit for it is open in every reader downstream. Refusing the
// terminal left that unit open forever, which is the one outcome a person's
// stop must never produce. Kicking workflow CONVERSION says nothing about
// whether a workflow run can be stopped.
type declaredResidueHandler struct {
	// RunOutput holds the run's bytes and file coordinates and spells the
	// reader-concluded terminals, exactly as it does for every other spool
	// handler. It parses nothing: the terminal's identities all come from the
	// reader and its output is the spool's bytes verbatim, so the entries this
	// handler mints off the file are still declared residue and nothing else.
	*handler.RunOutput
	kind string
	log  *logging.Bound
}

func newDeclaredResidueHandler(kind string, log *logging.Bound) *declaredResidueHandler {
	return &declaredResidueHandler{RunOutput: handler.NewRunOutput(log), kind: kind, log: log}
}

// Handle implements tail.Handler.
func (h *declaredResidueHandler) Handle(frames []tail.Frame, ctx *tail.Context) []*storev1.StoreEntry {
	// Remembered on EVERY batch, empty or not: a terminal built with no file id
	// and no offset digests one write identity for every such terminal in the
	// process, and the store absorbs all but the first as a replay.
	h.RememberCoords(ctx)
	out := make([]*storev1.StoreEntry, 0, len(frames))
	for _, frame := range frames {
		// Accumulated verbatim and bounded, so a stop arriving later settles the
		// run carrying what it actually said rather than claiming not_observed
		// for a spool this process read in full.
		h.Remember(ctx, frame.Raw)
		at := convert.Attribution{
			Path:   ctx.Path,
			FileID: ctx.FileID,
			Offset: frame.Offset,
			TaskID: ctx.TaskID,
		}
		// The attribution carries no RecordUUID — a spool's bytes are a byte
		// range, not a record — so convert.ResidueKey keys it
		// `residue:file:<path>:<offset>`, which is exactly what R-S4 pins.
		out = append(out, convert.VendorSpecificEntry(at, h.kind, map[string]any{
			"path":   ctx.Path,
			"offset": float64(frame.Offset),
			"output": string(frame.Raw),
		}))
		h.log.With(logging.Context{
			Operation: "declared-residue", Path: ctx.Path, TaskID: ctx.TaskID,
			FileID: ctx.FileID, Offset: logging.Off(frame.Offset),
		}).LogVerbose("ingested %d byte(s) as declared residue kind=%s", len(frame.Raw), h.kind)
	}
	return out
}

// CancelTerminal implements the reader's cancelTerminalSink for a declared-kind
// spool: a person's stop, spelled as the run's terminal.
//
// IT REFUSES ONLY WHAT RunOutput REFUSES — a stop naming no spawning call.
func (h *declaredResidueHandler) CancelTerminal(taskID, run, ownerAgentID string, settledAtMs int64) []*storev1.StoreEntry {
	return h.RunOutput.Cancelled(taskID, run, ownerAgentID, settledAtMs)
}
