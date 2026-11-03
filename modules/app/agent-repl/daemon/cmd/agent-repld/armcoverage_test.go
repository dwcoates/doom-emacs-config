package main

import (
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
