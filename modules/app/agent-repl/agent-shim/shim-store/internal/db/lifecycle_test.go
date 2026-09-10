package db

import (
	"database/sql"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
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
