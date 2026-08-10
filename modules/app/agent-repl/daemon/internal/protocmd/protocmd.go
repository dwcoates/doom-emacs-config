// Package protocmd reads the schema-carried facts about session commands and
// model markers back off the generated descriptors.
//
// WHY THIS PACKAGE EXISTS. The literals it returns are not traffic — no frame
// carries them — but every process needs the same answer to them: the daemon
// to recognize a submitted prompt as a session command, the frontends to
// spell and label one, and everything downstream of the shim to tell the
// CLI's `<synthetic>` marker apart from a real model id. They were previously
// restated once per runtime and kept aligned by review alone, which is the
// arrangement where one corrected spelling leaves the others stale.
//
// The proto files now carry them as enum-value options, and this package is
// the Go half of reading them back. There is no Go copy of any literal here:
// every value returned comes out of the descriptor that protoc emitted from
// the .proto, so a schema edit reaches this package with no Go edit at all.
//
// A MISSING OPTION PANICS RATHER THAN DEGRADING. These are build-time facts,
// fixed the moment the bindings were generated: an absent spec means the
// schema and the binary disagree, which no run-time branch can repair and no
// caller can sensibly handle. Returning a zero literal instead would hand the
// recognizer an empty string that matches nothing and the picker a model
// nothing can spawn — the exact silent-wrong-answer failures the schema move
// was made to end. The panic fires on first use, which is process start, and
// the accompanying tests assert the invariant so it never reaches a run.
package protocmd

import (
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/types/descriptorpb"
)

// Spec is one session command's schema facts: how it is spelled and whether
// text following the name belongs to it.
type Spec struct {
	// Literal is the command as the user types it, leading slash included,
	// and equally the form a reader is shown.
	Literal string
	// TakesArgs reports whether `<literal> <text>` is this command with an
	// argument rather than a prompt that happens to start with the literal.
	// False is the safe side: a command that takes no argument is recognized
	// only as an entire prompt, so "/status of the build" stays a prompt.
	TakesArgs bool
}

// SessionCommandSpecs returns the spec of every session command the schema
// names, keyed by its enum value.
//
// SESSION_COMMAND_UNSPECIFIED is absent from the map, matching the schema: it
// names no command, so it has no literal and a recognizer has nothing to match
// it against. A caller iterating this map therefore iterates exactly the real
// commands, with no sentinel to skip.
func SessionCommandSpecs() map[frontendv1.SessionCommand]Spec {
	values := frontendv1.SessionCommand(0).Descriptor().Values()
	specs := make(map[frontendv1.SessionCommand]Spec, values.Len())
	for i := 0; i < values.Len(); i++ {
		value := values.Get(i)
		command := frontendv1.SessionCommand(value.Number())
		if command == frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED {
			continue
		}
		options := enumValueOptions(value)
		if !proto.HasExtension(options, frontendv1.E_SessionCommandSpec) {
			panic(fmt.Sprintf(
				"protocmd: %s carries no session_command_spec option; the schema and these bindings disagree",
				value.FullName()))
		}
		spec, ok := proto.GetExtension(options, frontendv1.E_SessionCommandSpec).(*frontendv1.SessionCommandSpec)
		if !ok || spec == nil {
			panic(fmt.Sprintf(
				"protocmd: %s session_command_spec option is not a SessionCommandSpec",
				value.FullName()))
		}
		if spec.GetLiteral() == "" {
			panic(fmt.Sprintf(
				"protocmd: %s session_command_spec carries an empty literal",
				value.FullName()))
		}
		specs[command] = Spec{Literal: spec.GetLiteral(), TakesArgs: spec.GetTakesArgs()}
	}
	return specs
}

// SyntheticModelLiteral returns the exact string the CLI reports when it is
// not running a real nameable model.
//
// It is a MARKER, never an id: nothing can be spawned under it and no picker
// may offer it, so every site that commits or displays a reported model has to
// compare against this value first.
func SyntheticModelLiteral() string {
	value := corev1.ModelMarker_MODEL_MARKER_SYNTHETIC.Descriptor().
		Values().ByNumber(protoreflect.EnumNumber(corev1.ModelMarker_MODEL_MARKER_SYNTHETIC))
	if value == nil {
		panic("protocmd: MODEL_MARKER_SYNTHETIC is absent from its own enum descriptor")
	}
	options := enumValueOptions(value)
	if !proto.HasExtension(options, corev1.E_ModelMarkerLiteral) {
		panic("protocmd: MODEL_MARKER_SYNTHETIC carries no model_marker_literal option; the schema and these bindings disagree")
	}
	literal, ok := proto.GetExtension(options, corev1.E_ModelMarkerLiteral).(string)
	if !ok || literal == "" {
		panic("protocmd: MODEL_MARKER_SYNTHETIC model_marker_literal option is empty or not a string")
	}
	return literal
}

// enumValueOptions narrows a descriptor's options to the concrete message the
// extension accessors need. The descriptor API types them as the generic
// proto.Message, and a wrong dynamic type here means the bindings were not
// generated from these protos at all.
func enumValueOptions(value protoreflect.EnumValueDescriptor) *descriptorpb.EnumValueOptions {
	options, ok := value.Options().(*descriptorpb.EnumValueOptions)
	if !ok {
		panic(fmt.Sprintf("protocmd: %s options are %T, not *descriptorpb.EnumValueOptions",
			value.FullName(), value.Options()))
	}
	return options
}
