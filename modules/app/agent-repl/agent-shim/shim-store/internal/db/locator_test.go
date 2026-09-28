package db

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// ---- the vendor task pairing: persistence ----

func agentLocator(taskID, agentID string) *storev1.AgentLocator {
	return &storev1.AgentLocator{VendorTaskId: taskID, Agent: &conversationv1.AgentId{Value: agentID}}
}

// writeLocatorsOK writes one interactive batch carrying entries and pairings,
// which must succeed.
func writeLocatorsOK(t *testing.T, d *DB, locators []*storev1.AgentLocator, entries ...*storev1.StoreEntry) WriteResult {
	t.Helper()
	result, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive,
		&storev1.EntryBatch{Entries: entries, AgentLocators: locators}, nil)
	if err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}
	return result
}

func TestWriteBatchPersistsAnAgentLocator(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeLocatorsOK(t, d, []*storev1.AgentLocator{agentLocator("a1b2", "toolu_spawn")},
		pageEntry("w1", "u1", "toolu_spawn", frameItem(activityFrame("toolu_spawn", "act-1", prose()))))

	// Assert
	if got := scalar[string](t, d, `SELECT agent_id FROM vendor_task WHERE vendor_task_id = 'a1b2'`); got != "toolu_spawn" {
		t.Fatalf("vendor_task agent = %q, want toolu_spawn", got)
	}
}

func TestWriteBatchAbsorbsARestatedAgentLocator(t *testing.T) {
	// Arrange: every batch of a subagent's transcript restates its pairing.
	d, _ := newStore(t)
	writeLocatorsOK(t, d, []*storev1.AgentLocator{agentLocator("a1b2", "toolu_spawn")},
		pageEntry("w1", "u1", "toolu_spawn", frameItem(activityFrame("toolu_spawn", "act-1", prose()))))

	// Act
	writeLocatorsOK(t, d, []*storev1.AgentLocator{agentLocator("a1b2", "toolu_spawn")},
		pageEntry("w2", "u2", "toolu_spawn", frameItem(activityFrame("toolu_spawn", "act-2", prose()))))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM vendor_task`); got != 1 {
		t.Fatalf("vendor_task rows = %d, want 1", got)
	}
}

func TestWriteBatchAcceptsABatchCarryingOnlyAnAgentLocator(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeLocatorsOK(t, d, []*storev1.AgentLocator{agentLocator("a1b2", "toolu_spawn")})

	// Assert
	if result.Locators != 1 {
		t.Fatalf("Locators = %d, want 1", result.Locators)
	}
}

func TestWriteBatchRefusesAMalformedAgentLocatorWhole(t *testing.T) {
	tests := []struct {
		name    string
		locator *storev1.AgentLocator
		field   string
	}{
		{name: "unset", locator: nil, field: "agent_locators[0]"},
		{name: "empty locator", locator: agentLocator("", "toolu_spawn"), field: "agent_locators[0].vendor_task_id"},
		{name: "unset agent", locator: &storev1.AgentLocator{VendorTaskId: "a1b2"}, field: "agent_locators[0].agent"},
		{name: "empty agent", locator: agentLocator("a1b2", ""), field: "agent_locators[0].agent"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			_, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, &storev1.EntryBatch{
				Entries:       []*storev1.StoreEntry{pageEntry("w1", "u1", "toolu_spawn", frameItem(activityFrame("toolu_spawn", "act-1", prose())))},
				AgentLocators: []*storev1.AgentLocator{test.locator},
			}, nil)

			// Assert
			if !errors.Is(err, ErrInvalid) || RefusalField(err) != test.field {
				t.Fatalf("err = %v (field %q), want ErrInvalid naming %s", err, RefusalField(err), test.field)
			}
			if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry`); got != 0 {
				t.Fatalf("entry rows = %d, want 0: a refused batch commits nothing", got)
			}
		})
	}
}

// ---- the vendor task pairing: the lineage-scoped lookup ----

// spawnedWithLocator books `agent` as a subagent of `main` and pairs `task`
// with it, as the two planes together do.
func spawnedWithLocator(t *testing.T, d *DB, main, agent, task string) {
	t.Helper()
	writeOK(t, d, pageEntry("w-"+agent, "u-"+agent, main, frameItem(activityFrame(main, "act-"+agent, subagentStart(agent)))))
	writeLocatorsOK(t, d, []*storev1.AgentLocator{agentLocator(task, agent)})
}

func TestAgentByVendorTaskFindsTheAgentPairedWithinTheLineage(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	spawnedWithLocator(t, d, "agent-main", "toolu_spawn", "a1b2")

	// Act
	agent, found, err := d.AgentByVendorTask(ctx(), "agent-main", "a1b2")

	// Assert
	if err != nil || !found || agent != "toolu_spawn" {
		t.Fatalf("AgentByVendorTask = (%q, %t, %v), want (toolu_spawn, true, nil)", agent, found, err)
	}
	s.assertLogged(t, "info", "vendor task a1b2 names agent toolu_spawn")
}

func TestAgentByVendorTaskAnswersNotFoundForAnUnpairedLocator(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	spawnedWithLocator(t, d, "agent-main", "toolu_spawn", "a1b2")

	// Act
	agent, found, err := d.AgentByVendorTask(ctx(), "agent-main", "ffff")

	// Assert
	if err != nil || found || agent != "" {
		t.Fatalf("AgentByVendorTask = (%q, %t, %v), want (\"\", false, nil)", agent, found, err)
	}
	s.assertLogged(t, "info", "no agent in this session's lineage is paired with vendor task ffff")
}

func TestAgentByVendorTaskAnswersNotFoundForAnotherSessionsAgent(t *testing.T) {
	// Arrange: the locator is paired, but with an agent of a different session.
	d, _ := newStore(t)
	spawnedWithLocator(t, d, "agent-other", "toolu_spawn", "a1b2")
	writeOK(t, d, pageEntry("w-main", "u-main", "agent-main", frameItem(activityFrame("agent-main", "act-main", prose()))))

	// Act
	agent, found, err := d.AgentByVendorTask(ctx(), "agent-main", "a1b2")

	// Assert
	if err != nil || found || agent != "" {
		t.Fatalf("AgentByVendorTask = (%q, %t, %v), want not found: the pairing belongs to another lineage", agent, found, err)
	}
}

func TestAgentByVendorTaskRefusesAnIncompleteRequest(t *testing.T) {
	tests := []struct {
		name    string
		session string
		task    string
		field   string
	}{
		{name: "no session", session: "", task: "a1b2", field: "session"},
		{name: "no locator", session: "agent-main", task: "", field: "vendor_task_id"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			_, found, err := d.AgentByVendorTask(ctx(), test.session, test.task)

			// Assert
			if !errors.Is(err, ErrInvalid) || RefusalField(err) != test.field || found {
				t.Fatalf("err = %v (field %q, found %t), want ErrInvalid naming %s", err, RefusalField(err), found, test.field)
			}
		})
	}
}

func TestAgentByVendorTaskRefusesALocatorPairedWithTwoAgentsOfOneLineage(t *testing.T) {
	// Arrange: the pairing's invariant is one agent per locator; the store
	// refuses to choose between two.
	d, s := newStore(t)
	spawnedWithLocator(t, d, "agent-main", "toolu_one", "a1b2")
	spawnedWithLocator(t, d, "agent-main", "toolu_two", "a1b2")

	// Act
	_, found, err := d.AgentByVendorTask(ctx(), "agent-main", "a1b2")

	// Assert
	if !errors.Is(err, ErrStorage) || !errors.Is(err, errAmbiguousLocator) || found {
		t.Fatalf("err = %v (found %t), want a storage failure naming the ambiguity", err, found)
	}
	s.assertLogged(t, "error", "toolu_one, toolu_two")
}

func TestTheLocatorLookupBuildsNoAutomaticIndex(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	plan := queryPlan(t, d, agentByVendorTaskSQL, "agent-main", "a1b2")

	// Assert
	assertNoAutomaticIndex(t, "the locator lookup", plan)
}
