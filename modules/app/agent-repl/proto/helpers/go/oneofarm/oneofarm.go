// Package oneofarm names the arm a protobuf oneof carries, by the proto's own
// field name, so a test records and compares arms as data rather than through
// a hand-written switch that drifts when the proto adds or retires an arm.
package oneofarm

import (
	"fmt"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
)

// Field answers the field of the arm set in m's oneof named oneof, or nil
// when no arm is set. A oneof m does not declare is a caller's bug and
// panics: the caller names the oneof, and the proto decides whether it exists.
func Field(m protoreflect.Message, oneof string) protoreflect.FieldDescriptor {
	o := m.Descriptor().Oneofs().ByName(protoreflect.Name(oneof))
	if o == nil {
		panic(fmt.Sprintf("oneofarm: %s declares no oneof %q", m.Descriptor().FullName(), oneof))
	}
	return m.WhichOneof(o)
}

// Name answers the field name of the arm set in m's oneof named oneof, and
// false when no arm is set. It panics as Field does.
func Name(m proto.Message, oneof string) (string, bool) {
	field := Field(m.ProtoReflect(), oneof)
	if field == nil {
		return "", false
	}
	return string(field.Name()), true
}
