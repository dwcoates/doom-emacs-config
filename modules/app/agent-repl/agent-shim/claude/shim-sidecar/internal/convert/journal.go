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
	at.RecordUUID = str(record["uuid"])
	kind := str(record["type"])
	switch kind {
	case "started", "result":
		c.log.With(at.ctxFor("journal-record")).With(logging.Context{BookAgentID: str(record["agentId"])}).
			LogVerbose("workflow journal %s record for run=%s held as residue (workflow is kicked this wave)", kind, runID)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "workflow_journal/"+kind, record)}
	default:
		c.log.With(at.ctxWarn("journal-record")).
			Log("workflow journal record type=%q is not one of the two journal shapes; stored as unknown residue", kind)
		return []*storev1.StoreEntry{UnknownEntry(at, kind, "type", record)}
	}
}
