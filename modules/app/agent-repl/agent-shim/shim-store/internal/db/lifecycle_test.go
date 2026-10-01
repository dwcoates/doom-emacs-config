package db

import (
	"database/sql"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	"google.golang.org/protobuf/proto"
)

// The ARM IS THE ISOLATION, so the column holds the arm's name. A new arm that
// nobody teaches this function about must land as NULL rather than as a wrong
// name, which is the one answer a reader can tell apart from a real value.
func TestIsolationKindNamesTheSetArm(t *testing.T) {
	tests := []struct {
		name   string
		prompt *conversationv1.AgentSubagentPrompt
		want   sql.NullString
	}{
		{
			name:   "no prompt at all",
			prompt: nil,
			want:   sql.NullString{},
		},
		{
			name:   "a prompt that sets no isolation arm",
			prompt: &conversationv1.AgentSubagentPrompt{},
			want:   sql.NullString{},
		},
		{
			name: "the none arm",
			prompt: &conversationv1.AgentSubagentPrompt{
				Isolation: &conversationv1.AgentSubagentPrompt_None{None: &conversationv1.AgentSubagentIsolationNone{}},
			},
			want: sql.NullString{String: "none", Valid: true},
		},
		{
			name: "the worktree arm",
			prompt: &conversationv1.AgentSubagentPrompt{
				Isolation: &conversationv1.AgentSubagentPrompt_Worktree{Worktree: &conversationv1.AgentSubagentIsolationWorktree{}},
			},
			want: sql.NullString{String: "worktree", Valid: true},
		},
		{
			name: "the remote arm",
			prompt: &conversationv1.AgentSubagentPrompt{
				Isolation: &conversationv1.AgentSubagentPrompt_Remote{Remote: &conversationv1.AgentSubagentIsolationRemote{}},
			},
			want: sql.NullString{String: "remote", Valid: true},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act
			got := isolationKind(test.prompt)

			// Assert
			if got != test.want {
				t.Fatalf("isolationKind = %+v, want %+v", got, test.want)
			}
		})
	}
}

// An ABSENT optional string is NULL; a PRESENT but empty one is the empty
// string. Collapsing the two would erase a producer's explicit "".
func TestOptionalStringKeepsPresenceApartFromEmptiness(t *testing.T) {
	empty := ""
	value := "nightly"
	tests := []struct {
		name  string
		value *string
		want  sql.NullString
	}{
		{name: "absent", value: nil, want: sql.NullString{}},
		{name: "present and empty", value: &empty, want: sql.NullString{String: "", Valid: true}},
		{name: "present and set", value: &value, want: sql.NullString{String: "nightly", Valid: true}},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act
			got := optionalString(test.value)

			// Assert
			if got != test.want {
				t.Fatalf("optionalString = %+v, want %+v", got, test.want)
			}
		})
	}
}

// nullableString has no presence to read, so an empty value IS the absence.
func TestNullableStringTreatsEmptyAsNull(t *testing.T) {
	tests := []struct {
		name  string
		value string
		want  sql.NullString
	}{
		{name: "empty", value: "", want: sql.NullString{}},
		{name: "set", value: "do the thing", want: sql.NullString{String: "do the thing", Valid: true}},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act
			got := nullableString(test.value)

			// Assert
			if got != test.want {
				t.Fatalf("nullableString = %+v, want %+v", got, test.want)
			}
		})
	}
}

// A spawn depth of ZERO is a real depth, and must not read back as "unstated".
func TestOptionalUint32KeepsAPresentZeroApartFromAbsence(t *testing.T) {
	zero := uint32(0)
	three := uint32(3)
	tests := []struct {
		name  string
		value *uint32
		want  sql.NullInt64
	}{
		{name: "absent", value: nil, want: sql.NullInt64{}},
		{name: "present zero", value: &zero, want: sql.NullInt64{Int64: 0, Valid: true}},
		{name: "present three", value: &three, want: sql.NullInt64{Int64: 3, Valid: true}},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act
			got := optionalUint32(test.value)

			// Assert
			if got != test.want {
				t.Fatalf("optionalUint32 = %+v, want %+v", got, test.want)
			}
		})
	}
}

func TestRequestedModelIsNullWhenNoModelWasAskedFor(t *testing.T) {
	tests := []struct {
		name   string
		prompt *conversationv1.AgentSubagentPrompt
		want   sql.NullString
	}{
		{name: "no prompt at all", prompt: nil, want: sql.NullString{}},
		{name: "a prompt that names no model", prompt: &conversationv1.AgentSubagentPrompt{}, want: sql.NullString{}},
		{
			name: "a prompt that names one",
			prompt: &conversationv1.AgentSubagentPrompt{
				RequestedModel: &conversationv1.AgentModel{Name: "opus"},
			},
			want: sql.NullString{String: "opus", Valid: true},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act
			got := requestedModel(test.prompt)

			// Assert
			if got != test.want {
				t.Fatalf("requestedModel = %+v, want %+v", got, test.want)
			}
		})
	}
}

// ---- lineage from a SETTLED spawn ----
//
// A file-plane delivery of a synchronous spawn is only ever its conclusion, so
// the success is the one frame that names the spawner.

// subagentSuccess is the settled spawn unit naming the agent it created.
func subagentSuccess(createdAgentID string) *conversationv1.AgentSubagent {
	success := &conversationv1.AgentSubagentSuccess{}
	if createdAgentID != "" {
		success.CreatedAgentId = &conversationv1.AgentId{Value: createdAgentID}
	}
	return &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Success{Success: success}}
}

func TestASettledSpawnRecordsTheSpawnerOfAnAgentItCreated(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "toolu_sub", subagentSuccess("toolu_sub")))))

	// Assert
	if got := scalar[string](t, d, `SELECT spawned_by_agent FROM agent WHERE agent_id = 'toolu_sub'`); got != "agent-main" {
		t.Fatalf("spawned_by_agent = %q, want the agent whose book holds the spawn", got)
	}
}

func TestASettledSpawnCompletesTheLineageOfAnAgentFirstSeenWithoutOne(t *testing.T) {
	// Arrange: the subagent's own transcript was copied before its spawner's
	// result, so its row exists with no spawner.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "toolu_sub", frameItem(activityFrame("toolu_sub", "act-1", prose()))))

	// Act
	writeOK(t, d, pageEntry("w2", "u2", "agent-main", frameItem(activityFrame("agent-main", "toolu_sub", subagentSuccess("toolu_sub")))))

	// Assert
	if got := scalar[string](t, d, `SELECT spawned_by_agent FROM agent WHERE agent_id = 'toolu_sub'`); got != "agent-main" {
		t.Fatalf("spawned_by_agent = %q, want the spawner the settled spawn named", got)
	}
}

func TestASettledSpawnKeepsTheMetadataItsStartRecorded(t *testing.T) {
	// Arrange: the stream plane's start recorded the prompt's isolation.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "toolu_sub", subagentStart("toolu_sub")))))

	// Act
	writeOK(t, d, pageEntry("w2", "u1", "agent-main", frameItem(activityFrame("agent-main", "toolu_sub", subagentSuccess("toolu_sub")))))

	// Assert: lineage and nothing else — the conclusion erases no start column.
	if got := scalar[string](t, d, `SELECT isolation FROM agent WHERE agent_id = 'toolu_sub'`); got != "worktree" {
		t.Fatalf("isolation = %q, want the start's worktree kept", got)
	}
}

func TestASettledSpawnNamingNoCreatedAgentRecordsNoLineage(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeOK(t, d, pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "toolu_sub", subagentSuccess("")))))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM agent WHERE spawned_by_agent IS NOT NULL`); got != 0 {
		t.Fatalf("agents with a spawner = %d, want 0 — an unnamed created agent is never invented", got)
	}
}

func TestASettledSpawnWhoseLineageCannotBeWrittenFailsTheBatchLoudly(t *testing.T) {
	// Arrange: the agent table refuses the lineage write.
	d, s := newStore(t)
	if _, err := d.sql.ExecContext(ctx(), `CREATE TRIGGER refuse_lineage BEFORE INSERT ON agent
	  WHEN NEW.spawned_by_agent IS NOT NULL BEGIN SELECT RAISE(ABORT, 'lineage refused'); END`); err != nil {
		t.Fatalf("seed trigger: %v", err)
	}

	// Act
	_, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive,
		batch(pageEntry("w1", "u1", "agent-main", frameItem(activityFrame("agent-main", "toolu_sub", subagentSuccess("toolu_sub"))))), nil)

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	s.assertLogged(t, "error", "recording the spawner of settled spawn")
}

// ---- the commission columns read back (commissionRow.commission) ----

// everyPromptField is a commission stating every field AgentSubagentPrompt has,
// with the isolation the columns can carry whole.
func everyPromptField() *conversationv1.AgentSubagentPrompt {
	description, subagentType, name := "fix the shim", "opus-medium", "fixer"
	return &conversationv1.AgentSubagentPrompt{
		Description:      &description,
		Text:             "go",
		SubagentType:     &subagentType,
		RequestedName:    &name,
		RequestedModel:   &conversationv1.AgentModel{Name: "opus"},
		ForkedFromCaller: true,
		Isolation:        &conversationv1.AgentSubagentPrompt_Worktree{Worktree: &conversationv1.AgentSubagentIsolationWorktree{}},
	}
}

// spawnWith records a spawn of AGENT commissioned with PROMPT, as a start frame
// in the main agent's book.
func spawnWith(t *testing.T, d *DB, agent string, prompt *conversationv1.AgentSubagentPrompt) {
	t.Helper()
	start := subagentStart(agent)
	start.GetStart().Prompt = prompt
	writeOK(t, d, pageEntry("spawn-"+agent, "activity:"+agent, "agent-main", frameItem(activityFrame("agent-main", agent, start))))
}

func TestTheCommissionRoundTripsEveryRecordedPromptField(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	spawnWith(t, d, "toolu_spawn", everyPromptField())

	// Act
	commission, found, err := d.AgentCommission(ctx(), "toolu_spawn")

	// Assert
	if err != nil || !found || !proto.Equal(commission, everyPromptField()) {
		t.Fatalf("commission = %v (found %t, err %v), want the spawn's own prompt %v", commission, found, err, everyPromptField())
	}
}

// THE MAPPING HAS NO COMPILER: a field AgentSubagentPrompt gains that the round
// trip above does not set fails here, so it is mapped (or ruled out) rather than
// silently dropped.
func TestTheRoundTripFixtureSetsEveryPromptField(t *testing.T) {
	// Arrange
	fixture := everyPromptField().ProtoReflect()
	fields := fixture.Descriptor().Fields()

	// Act
	var unset []string
	for i := 0; i < fields.Len(); i++ {
		field := fields.Get(i)
		if oneof := field.ContainingOneof(); oneof != nil && !oneof.IsSynthetic() {
			if fixture.WhichOneof(oneof) == nil {
				unset = append(unset, string(oneof.Name()))
			}
			continue
		}
		if !fixture.Has(field) {
			unset = append(unset, string(field.Name()))
		}
	}

	// Assert
	if len(unset) != 0 {
		t.Fatalf("the round-trip fixture leaves %v unset; every AgentSubagentPrompt field must be mapped to an agent column or ruled out", unset)
	}
}

func TestARemoteIsolationIsAnsweredWithItsHandlesUnset(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	url, task := "https://remote/s", "rt-1"
	prompt := everyPromptField()
	prompt.Isolation = &conversationv1.AgentSubagentPrompt_Remote{Remote: &conversationv1.AgentSubagentIsolationRemote{SessionUrl: &url, RemoteTaskId: &task}}
	spawnWith(t, d, "toolu_spawn", prompt)

	// Act
	commission, _, err := d.AgentCommission(ctx(), "toolu_spawn")

	// Assert
	remote := commission.GetRemote()
	if err != nil || remote == nil || remote.SessionUrl != nil || remote.RemoteTaskId != nil {
		t.Fatalf("isolation = %v (err %v), want the remote arm with no handles", commission.GetIsolation(), err)
	}
}

func TestAnUnknownRecordedIsolationIsAStorageFailure(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	spawnWith(t, d, "toolu_spawn", everyPromptField())
	if _, err := d.sql.Exec(`UPDATE agent SET isolation = 'teleport' WHERE agent_id = 'toolu_spawn'`); err != nil {
		t.Fatalf("corrupt the isolation column: %v", err)
	}

	// Act
	_, _, err := d.AgentCommission(ctx(), "toolu_spawn")

	// Assert
	if !errors.Is(err, ErrStorage) || !errors.Is(err, errUnknownIsolation) {
		t.Fatalf("err = %v, want a storage failure naming the unknown isolation", err)
	}
}
