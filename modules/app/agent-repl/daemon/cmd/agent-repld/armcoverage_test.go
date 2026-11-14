package main

import (
	"errors"
	"fmt"
	"slices"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/reflect/protoregistry"
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

// armVariants answers one message of descriptor MD per way its oneofs can be
// set, recursively: each arm of each oneof, and each such variant of every
// singular message field that has oneofs of its own, so a nested choice (a
// parent's hidden or closed state, GitHub's failure) is enumerated too. A
// message with no oneof anywhere in it is one empty variant.
func armVariants(md protoreflect.MessageDescriptor, depth int) []protoreflect.Message {
	out := []protoreflect.Message{newGenerated(md)}
	if depth > 4 {
		return out
	}
	fields := md.Fields()
	for i := range fields.Len() {
		f := fields.Get(i)
		if f.Message() == nil || f.IsList() || f.IsMap() || f.ContainingOneof() != nil || !hasOneofs(f.Message(), 0) {
			continue
		}
		var next []protoreflect.Message
		for _, base := range out {
			for _, v := range armVariants(f.Message(), depth+1) {
				m := cloneMessage(base)
				m.Set(f, protoreflect.ValueOfMessage(v))
				next = append(next, m)
			}
		}
		out = next
	}
	oneofs := md.Oneofs()
	for i := range oneofs.Len() {
		o := oneofs.Get(i)
		var next []protoreflect.Message
		for _, base := range out {
			arms := o.Fields()
			for j := range arms.Len() {
				arm := arms.Get(j)
				if arm.Message() == nil {
					m := cloneMessage(base)
					m.Set(arm, m.NewField(arm))
					next = append(next, m)
					continue
				}
				for _, v := range armVariants(arm.Message(), depth+1) {
					m := cloneMessage(base)
					m.Set(arm, protoreflect.ValueOfMessage(v))
					next = append(next, m)
				}
			}
		}
		out = next
	}
	return out
}

// hasOneofs reports whether MD or a singular message field of it declares a
// oneof.
func hasOneofs(md protoreflect.MessageDescriptor, depth int) bool {
	if md.Oneofs().Len() > 0 {
		return true
	}
	if depth > 4 {
		return false
	}
	fields := md.Fields()
	for i := range fields.Len() {
		f := fields.Get(i)
		if f.Message() != nil && !f.IsList() && !f.IsMap() && hasOneofs(f.Message(), depth+1) {
			return true
		}
	}
	return false
}

// newGenerated answers a new message of MD, of its generated type, so a
// describer's type switch sees the arms it names.
func newGenerated(md protoreflect.MessageDescriptor) protoreflect.Message {
	mt, err := protoregistry.GlobalTypes.FindMessageByName(md.FullName())
	if err != nil {
		panic(fmt.Sprintf("no generated type for %s: %v", md.FullName(), err))
	}
	return mt.New()
}

// cloneMessage answers a deep copy of M.
func cloneMessage(m protoreflect.Message) protoreflect.Message {
	return proto.Clone(m.Interface()).ProtoReflect()
}

func TestArmVariants(t *testing.T) {
	tests := []struct {
		name string
		md   protoreflect.MessageDescriptor
		want int
	}{
		{name: "a message with no oneof is one empty variant", md: (&agentreplv1.SetWorkspaceOneShotClosed{}).ProtoReflect().Descriptor(), want: 1},
		{name: "each arm of a oneof is a variant", md: (&agentreplv1.SetWorkspaceOneShotError{}).ProtoReflect().Descriptor(), want: 6},
		{name: "a nested oneof is enumerated within its arm", md: (&agentreplv1.SetWorkspaceHiddenError{}).ProtoReflect().Descriptor(), want: 5 + 3},
		{name: "a singular field's oneof is enumerated", md: (&agentreplv1.CommentOnPullRequestNotPosted{}).ProtoReflect().Descriptor(), want: 5},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := armVariants(tt.md, 0)

			// Assert
			if len(got) != tt.want {
				t.Fatalf("%d variants, want %d", len(got), tt.want)
			}
		})
	}
}

// everyDescribedRefusal is every rpc error the CLI describes, each with its
// describer.
func everyDescribedRefusal() []describedRefusal {
	all := []describedRefusal{
		{name: "prompt", empty: &agentreplv1.SubmitPromptError{}, oneof: "reason",
			describe: func(e proto.Message) (string, error) {
				return describePromptRefusal("ws", e.(*agentreplv1.SubmitPromptError))
			}},
		{name: "gather", empty: &agentreplv1.GatherWorkspacesError{}, oneof: "cause",
			describe: func(e proto.Message) (string, error) {
				return describeGatherRefusal(e.(*agentreplv1.GatherWorkspacesError))
			}},
		{name: "gather status", empty: &agentreplv1.GetGatherStatusError{}, oneof: "cause",
			describe: func(e proto.Message) (string, error) {
				return describeGatherStatusRefusal("g", e.(*agentreplv1.GetGatherStatusError))
			}},
	}
	for _, group := range [][]describedRefusal{workspaceRefusals, workspaceSettingRefusals, lifecycleRefusals, createRefusals, queueRefusals, subscriptionRefusals, runtimeRefusals} {
		all = append(all, group...)
	}
	return all
}

// TestEveryDescribedRpcSaysEveryArm holds every rpc whose answer the CLI
// describes (each answerDescribed call site's response) to a sentence for
// every arm of its error, enumerated off the proto itself, nested choices
// included: an arm added to the proto without a sentence fails here, with
// no table to remember to extend.
func TestEveryDescribedRpcSaysEveryArm(t *testing.T) {
	// Arrange
	byError := map[protoreflect.FullName]describedRefusal{}
	for _, d := range everyDescribedRefusal() {
		byError[d.empty.ProtoReflect().Descriptor().FullName()] = d
	}
	responses := describedResponses(t)
	if len(responses) == 0 {
		t.Fatal("found no described response to check")
	}
	for _, response := range responses {
		t.Run(response, func(t *testing.T) {
			mt, err := protoregistry.GlobalTypes.FindMessageByName(protoreflect.FullName("agentrepl.v1." + response))
			if err != nil {
				t.Fatalf("no message agentrepl.v1.%s: %v", response, err)
			}
			field := mt.Descriptor().Fields().ByName("error")
			if field == nil || field.Message() == nil {
				t.Fatalf("%s has no error arm to describe", response)
			}
			d, ok := byError[field.Message().FullName()]
			if !ok {
				t.Fatalf("%s's describer is not in everyDescribedRefusal", field.Message().FullName())
			}

			// Act / Assert
			oneof := field.Message().Oneofs().ByName(protoreflect.Name(d.oneof))
			if oneof == nil {
				t.Fatalf("%s declares no oneof %q", field.Message().FullName(), d.oneof)
			}
			for _, v := range armVariants(field.Message(), 0) {
				got, err := d.describe(v.Interface())
				if err != nil || got == "" {
					t.Errorf("%v: describe = %q, %v; want a sentence", v.Interface(), got, err)
				}
			}
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
