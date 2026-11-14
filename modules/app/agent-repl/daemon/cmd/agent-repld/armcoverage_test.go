package main

import (
	"errors"
	"fmt"
	"slices"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
)

// uncasedArms answers, in declaration order, every arm of EMPTY's oneof ONEOF
// that none of CASED sets: the arms a refusal describer's table has no case
// for. Every refusal describer's coverage test asks it, so a describer's
// table and its message's arms are compared one way everywhere. A oneof
// EMPTY does not declare is a bug in the test, and panics.
func uncasedArms(empty proto.Message, oneof string, cased []proto.Message) []protoreflect.Name {
	descriptor := empty.ProtoReflect().Descriptor().Oneofs().ByName(protoreflect.Name(oneof))
	if descriptor == nil {
		panic(fmt.Sprintf("%s declares no oneof %q", empty.ProtoReflect().Descriptor().FullName(), oneof))
	}
	set := map[protoreflect.Name]bool{}
	for _, msg := range cased {
		if arm := msg.ProtoReflect().WhichOneof(descriptor); arm != nil {
			set[arm.Name()] = true
		}
	}
	var out []protoreflect.Name
	fields := descriptor.Fields()
	for i := range fields.Len() {
		if name := fields.Get(i).Name(); !set[name] {
			out = append(out, name)
		}
	}
	return out
}

func TestUncasedArms(t *testing.T) {
	closed := &agentreplv1.SetWorkspaceOneShotError{Cause: &agentreplv1.SetWorkspaceOneShotError_Closed{Closed: &agentreplv1.SetWorkspaceOneShotClosed{}}}
	all := make([]proto.Message, 0, len(oneShotRefusalCases))
	for _, tt := range oneShotRefusalCases {
		all = append(all, tt.err)
	}
	tests := []struct {
		name  string
		cased []proto.Message
		want  []protoreflect.Name
	}{
		{name: "every arm cased answers none", cased: all, want: nil},
		{name: "no case answers every arm in declaration order", cased: nil,
			want: []protoreflect.Name{"unknown_workspace", "workspace_ref_mismatch", "closed", "no_creation_job", "requester_may_not_set", "main_workspace"}},
		{name: "a case with no arm set counts for nothing", cased: []proto.Message{closed, &agentreplv1.SetWorkspaceOneShotError{}},
			want: []protoreflect.Name{"unknown_workspace", "workspace_ref_mismatch", "no_creation_job", "requester_may_not_set", "main_workspace"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := uncasedArms(&agentreplv1.SetWorkspaceOneShotError{}, "cause", tt.cased)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("uncasedArms = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestUncasedArmsPanicsOnAnUndeclaredOneof(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("uncasedArms did not panic on a oneof the message does not declare")
		}
	}()

	// Act
	uncasedArms(&agentreplv1.SetWorkspaceOneShotError{}, "reason", nil)
}

// armed answers a copy of EMPTY with its one oneof's arm ARM set to VALUE, an
// empty message of the arm's type when VALUE is nil: the one way a refusal
// case is built, so a table names each arm by the name the proto gives it. An
// arm EMPTY does not declare, or a VALUE of another type, is a bug in the
// test, and panics.
func armed(empty proto.Message, arm string, value proto.Message) proto.Message {
	msg := proto.Clone(empty)
	m := msg.ProtoReflect()
	field := m.Descriptor().Fields().ByName(protoreflect.Name(arm))
	if field == nil || field.ContainingOneof() == nil || field.Message() == nil {
		panic(fmt.Sprintf("%s declares no message arm %q", m.Descriptor().FullName(), arm))
	}
	v := m.NewField(field)
	if value != nil {
		if value.ProtoReflect().Descriptor() != field.Message() {
			panic(fmt.Sprintf("arm %q holds a %s, not a %s", arm, field.Message().FullName(), value.ProtoReflect().Descriptor().FullName()))
		}
		v = protoreflect.ValueOfMessage(value.ProtoReflect())
	}
	m.Set(field, v)
	return msg
}

// describedRefusal is one rpc error's describer with its cases: one per arm
// of the error's oneof ONEOF, each with the sentence the describer says.
type describedRefusal struct {
	name     string
	cases    []refusalCase
	empty    proto.Message
	oneof    string
	describe func(proto.Message) (string, error)
}

// testDescribedRefusals holds each of REFUSALS to the describer rule: every
// case's arm says its sentence, every arm has a case, and an error with no
// arm set is the undescribed-arm bug.
func testDescribedRefusals(t *testing.T, refusals []describedRefusal) {
	t.Helper()
	for _, verb := range refusals {
		for _, tt := range verb.cases {
			t.Run(verb.name+"/says/"+tt.name, func(t *testing.T) {
				// Act
				got, err := verb.describe(tt.err)

				// Assert
				if err != nil || got != tt.want {
					t.Fatalf("describe = %q, %v; want %q", got, err, tt.want)
				}
			})
		}
		t.Run(verb.name+"/covers every arm", func(t *testing.T) {
			// Arrange
			cased := make([]proto.Message, 0, len(verb.cases))
			for _, tt := range verb.cases {
				cased = append(cased, tt.err)
			}

			// Act / Assert
			for _, missing := range uncasedArms(verb.empty, verb.oneof, cased) {
				t.Errorf("%s arm %s has no case: give its describer a sentence for it and a case here", verb.empty.ProtoReflect().Descriptor().Name(), missing)
			}
		})
		t.Run(verb.name+"/no arm is a bug", func(t *testing.T) {
			// Act
			got, err := verb.describe(verb.empty)

			// Assert
			if !errors.Is(err, errUndescribedArm) || got != "" {
				t.Fatalf("describe = %q, %v; want the undescribed-arm bug", got, err)
			}
		})
	}
}

func TestArmed(t *testing.T) {
	tests := []struct {
		name  string
		arm   string
		value proto.Message
		want  proto.Message
	}{
		{name: "a nil value sets an empty arm", arm: "closed",
			want: &agentreplv1.SetWorkspaceOneShotError{Cause: &agentreplv1.SetWorkspaceOneShotError_Closed{Closed: &agentreplv1.SetWorkspaceOneShotClosed{}}}},
		{name: "a value is set as the arm", arm: "workspace_ref_mismatch", value: &agentreplv1.SetWorkspaceOneShotWorkspaceRefMismatch{RegistryDir: "/w"},
			want: &agentreplv1.SetWorkspaceOneShotError{Cause: &agentreplv1.SetWorkspaceOneShotError_WorkspaceRefMismatch{WorkspaceRefMismatch: &agentreplv1.SetWorkspaceOneShotWorkspaceRefMismatch{RegistryDir: "/w"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := armed(&agentreplv1.SetWorkspaceOneShotError{}, tt.arm, tt.value)

			// Assert
			if !proto.Equal(got, tt.want) {
				t.Fatalf("armed = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestArmedPanicsOnAMisnamedArm(t *testing.T) {
	tests := []struct {
		name  string
		arm   string
		value proto.Message
	}{
		{name: "an arm the message does not declare", arm: "no_such_arm"},
		{name: "a value of another type", arm: "closed", value: &agentreplv1.SetWorkspaceOneShotMainWorkspace{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			defer func() {
				// Assert
				if recover() == nil {
					t.Fatal("armed did not panic")
				}
			}()

			// Act
			armed(&agentreplv1.SetWorkspaceOneShotError{}, tt.arm, tt.value)
		})
	}
}

// armedWith is armed with the arm's scalar FIELDS set by name, for an arm
// whose facts a sentence says. A field the arm does not declare is a bug in
// the test, and panics.
func armedWith(empty proto.Message, arm string, fields map[string]any) proto.Message {
	msg := armed(empty, arm, nil)
	m := msg.ProtoReflect()
	value := m.Get(m.Descriptor().Fields().ByName(protoreflect.Name(arm))).Message()
	for name, v := range fields {
		f := value.Descriptor().Fields().ByName(protoreflect.Name(name))
		if f == nil {
			panic(fmt.Sprintf("%s declares no field %q", value.Descriptor().FullName(), name))
		}
		value.Set(f, protoreflect.ValueOf(v))
	}
	return msg
}

// sharedWorkspaceCases are the cases of the shared workspace arms ARMS of
// EMPTY, said about the workspace "ws" as its describer says them, AGAIN
// being what the verb says to do once a restart allows.
func sharedWorkspaceCases(empty proto.Message, again string, arms ...string) []refusalCase {
	var out []refusalCase
	for _, arm := range arms {
		switch arm {
		case "unknown_workspace":
			out = append(out, refusalCase{name: arm, err: armed(empty, arm, nil), want: "the daemon has no workspace ws"})
		case "workspace_ref_mismatch":
			out = append(out, refusalCase{name: arm, err: armedWith(empty, arm, map[string]any{"registry_dir": "/w/ws"}),
				want: "ws lives at /w/ws, not at the directory the request named"})
		case "transferring_away":
			out = append(out, refusalCase{name: arm, err: armedWith(empty, arm, map[string]any{"address": "127.0.0.1:9"}),
				want: "the daemon is restarting and handed ws to its successor at 127.0.0.1:9; " + again})
		case "not_yet_adopted":
			out = append(out, refusalCase{name: arm, err: armed(empty, arm, nil),
				want: "the daemon is restarting and has not taken ws over yet; " + again + " once the restart finishes"})
		default:
			panic(fmt.Sprintf("%q is no shared workspace arm", arm))
		}
	}
	return out
}

// theFourWorkspaceArms are the arms nearly every workspace verb shares.
var theFourWorkspaceArms = []string{"unknown_workspace", "workspace_ref_mismatch", "transferring_away", "not_yet_adopted"}

func TestArmedWith(t *testing.T) {
	// Arrange
	want := &agentreplv1.SetWorkspaceOneShotError{Cause: &agentreplv1.SetWorkspaceOneShotError_WorkspaceRefMismatch{WorkspaceRefMismatch: &agentreplv1.SetWorkspaceOneShotWorkspaceRefMismatch{RegistryDir: "/w"}}}

	// Act
	got := armedWith(&agentreplv1.SetWorkspaceOneShotError{}, "workspace_ref_mismatch", map[string]any{"registry_dir": "/w"})

	// Assert
	if !proto.Equal(got, want) {
		t.Fatalf("armedWith = %v, want %v", got, want)
	}
}

func TestArmedWithPanicsOnAnUndeclaredField(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("armedWith did not panic")
		}
	}()

	// Act
	armedWith(&agentreplv1.SetWorkspaceOneShotError{}, "workspace_ref_mismatch", map[string]any{"no_such_field": "x"})
}
