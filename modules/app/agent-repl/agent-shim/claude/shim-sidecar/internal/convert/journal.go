package convert

// journal.go — WORKFLOW IS KICKED THIS WAVE.
//
// The vocabulary stays in the contract and the files are still discovered and
// cursor-tailed — nothing on disk is ever dropped — but no workflow FEATURE is
// implemented, so every journal record converts to residue rather than to an
// AgentWorkflow frame nobody consumes yet. Filing them as `unknown` would say
// "we do not model this", which is false; the model exists and the FEATURE does
// not. So they are vendor_specific: understood, deliberately not carried, and the
// follow-up they ask for is the workflow wave itself.

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// JournalRecord converts one workflow-journal object.
//
// A journal holds exactly two record shapes — {started, key, agentId} and
// {result, key, agentId, result} — and nothing run-scoped: NOTHING IN A JOURNAL
// EVER SAYS THE RUN FINISHED, which is why no terminal is minted from one.
func (c *Converter) JournalRecord(record map[string]any, at Attribution, runID string) []*storev1.StoreEntry {
	kind := str(record["type"])
	switch kind {
	case "started", "result":
		c.log.With(logging.Context{Operation: "journal-record", Path: at.Path, Task: at.TaskID}).
			LogVerbose("workflow journal %s record run=%s agent=%s at offset=%d held as residue (workflow is kicked this wave)",
				kind, runID, str(record["agentId"]), at.Offset)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "workflow_journal/"+kind, record)}
	default:
		c.log.With(logging.Context{Operation: "journal-record", Path: at.Path, Level: "warn"}).
			Log("workflow journal record type=%q at offset=%d is not one of the two journal shapes; stored as unknown residue", kind, at.Offset)
		return []*storev1.StoreEntry{UnknownEntry(at, kind, "type", record)}
	}
}

// WorkflowSpool converts a `w*.output` workflow spool's bytes.
//
// Same disposition as the journal: discovered, tailed, and held as residue until
// the workflow wave, so no byte the vendor wrote is lost in the meantime.
func (c *Converter) WorkflowSpool(at Attribution, output string) *storev1.StoreEntry {
	c.log.With(logging.Context{Operation: "workflow-spool", Path: at.Path, Task: at.TaskID}).
		LogVerbose("workflow spool bytes=%d at offset=%d held as residue (workflow is kicked this wave)", len(output), at.Offset)
	return VendorSpecificEntry(at, "workflow_spool", map[string]any{
		"task_id": at.TaskID,
		"path":    at.Path,
		"offset":  float64(at.Offset),
		"output":  output,
	})
}
