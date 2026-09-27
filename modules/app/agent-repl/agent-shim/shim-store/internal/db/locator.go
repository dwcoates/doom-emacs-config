package db

import (
	"context"
	"database/sql"
	"strings"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

// locator.go — the vendor task locator ↔ agent pairing (the `vendor_task`
// table), and the lineage-scoped lookup the shim asks it through.
//
// WHY IT EXISTS. A subagent's vendor task id is the agent's own LOCATOR and
// comes back when the agent is resumed, but the resume's `task_started` names
// the call that woke it (a `SendMessage`), not the spawn whose `tool_use_id`
// IS the agent. A shim that restarted since the spawn has no way to name the
// running agent from the stream alone. The sidecar reads both halves of the
// pairing from the vendor's own files and writes them with the agent's rows;
// this file persists that and answers for it.
//
// THE STORE NEVER DERIVES ONE FROM THE OTHER. A pairing is recorded exactly as
// the producer stated it and read back exactly as recorded; the store only
// scopes the answer to the caller's lineage, as every session-scoped read does.

// agentByVendorTaskSQL binds (session, vendor task id). It drives from the
// locator — the table's primary key prefix, so one seek — and keeps only the
// agents the session's lineage reaches. At package scope so the suite EXPLAINs
// the production text itself.
const agentByVendorTaskSQL = sessionLineageCTE + `
	  SELECT v.agent_id FROM vendor_task v
	  WHERE v.vendor_task_id = ? AND v.agent_id IN (SELECT agent_id FROM lineage)
	  ORDER BY v.agent_id ASC`

// validateAgentLocator refuses a pairing that names nothing on either side.
// BOTH HALVES ARE REQUIRED: a locator with no agent pairs nothing, and an agent
// with no locator is a pairing no lookup could ever reach.
func validateAgentLocator(l *storev1.AgentLocator, index int) error {
	field := "agent_locators[" + itoa(index) + "]"
	if l == nil {
		return invalidFieldf(field, "agent locator is unset")
	}
	if l.GetVendorTaskId() == "" {
		return invalidFieldf(field+".vendor_task_id", "vendor_task_id is empty — a pairing names the vendor's locator it is looked up by")
	}
	if l.GetAgent().GetValue() == "" {
		return invalidFieldf(field+".agent", "agent is unset or empty — a pairing names the agent the locator resolves to")
	}
	return nil
}

// applyAgentLocators records one batch's pairings inside the batch's own
// transaction. A pairing already on record is ABSORBED, never rewritten: the
// row is the pair itself, so there is nothing a re-statement could change.
func (d *DB) applyAgentLocators(ctx context.Context, tx *sql.Tx, locators []*storev1.AgentLocator, now int64) error {
	for _, l := range locators {
		if _, err := tx.ExecContext(ctx, `
INSERT INTO vendor_task(vendor_task_id, agent_id, recorded_at_ms) VALUES (?, ?, ?)
ON CONFLICT(vendor_task_id, agent_id) DO NOTHING`,
			l.GetVendorTaskId(), l.GetAgent().GetValue(), now); err != nil {
			return storagef(err, "recording the pairing of vendor task %s with agent %s", l.GetVendorTaskId(), l.GetAgent().GetValue())
		}
	}
	return nil
}

// AgentByVendorTask answers WHICH AGENT a vendor task locator names within the
// lineage of `session` — the caller's main agent.
//
// THREE ANSWERS. One agent: `found` is true. None within the lineage: `found`
// is false with no error — an ordinary answer, recorded at info. More than one:
// the pairing's invariant is broken (one locator names one agent), and the
// store refuses to choose, as a storage failure recorded at ERROR naming every
// candidate.
//
// NEVER UNSCOPED. An empty session or locator is refused before any statement
// runs, exactly as LiveWork refuses an empty session.
func (d *DB) AgentByVendorTask(ctx context.Context, session, vendorTaskID string) (agentID string, found bool, err error) {
	base := logging.Fields{Operation: "store.db.agent-by-vendor-task", Table: "vendor_task", AgentID: session, TaskID: vendorTaskID}
	if session == "" {
		return "", false, d.refuse(base, invalidSitef(SiteSessionEmpty, "session", "session: an unscoped locator lookup could name another session's agent"))
	}
	if vendorTaskID == "" {
		return "", false, d.refuse(base, invalidFieldf("vendor_task_id", "vendor_task_id is empty — the lookup names the locator it resolves"))
	}
	started := d.mono()
	agents, err := d.scanStrings(ctx, agentByVendorTaskSQL, session, vendorTaskID)
	if err != nil {
		return "", false, d.refuse(base, storagef(err, "reading the agent paired with vendor task %s", vendorTaskID))
	}
	d.observeQuery(StatementAgentByVendorTask, "vendor_task", base, started, int64(len(agents)))
	d.traceStatement(ctx, StatementAgentByVendorTask, "vendor_task", base, int64(len(agents)))
	switch len(agents) {
	case 0:
		d.log.Log(base, "no agent in this session's lineage is paired with vendor task %s", vendorTaskID)
		return "", false, nil
	case 1:
		d.log.Log(base, "vendor task %s names agent %s", vendorTaskID, agents[0])
		return agents[0], true, nil
	default:
		return "", false, d.refuse(base, storagef(errAmbiguousLocator,
			"vendor task %s is paired with %d agents of one lineage (%s)", vendorTaskID, len(agents), strings.Join(agents, ", ")))
	}
}
