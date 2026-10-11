// Package protoarm reads which arm of a message's oneof is set, for the
// resolvers that name an arm in a record, a key or a status cell.
//
// Each of those sites used to resolve the oneof by name and ask which arm is
// set by hand, and a site that named a oneof the message does not declare
// handed WhichOneof a nil descriptor and panicked with no word of which
// oneof it was. Set panics with the message and the oneof named instead.
package protoarm

import (
	"fmt"

	"google.golang.org/protobuf/reflect/protoreflect"
)

// Set answers the field of M's set arm in the oneof named ONEOF, and false
// when no arm is set.
//
// ONEOF is a name the caller takes from the contract, so a message that
// declares no such oneof is a programming error: Set panics rather than
// answer "unset" for a oneof that does not exist.
func Set(m protoreflect.Message, oneof protoreflect.Name) (protoreflect.FieldDescriptor, bool) {
	od := m.Descriptor().Oneofs().ByName(oneof)
	if od == nil {
		panic(fmt.Sprintf("protoarm: %s declares no oneof %q", m.Descriptor().FullName(), oneof))
	}
	field := m.WhichOneof(od)
	return field, field != nil
}

// Name answers the name of M's set arm in the oneof named ONEOF, and false
// when no arm is set. It panics as Set does.
func Name(m protoreflect.Message, oneof protoreflect.Name) (protoreflect.Name, bool) {
	field, ok := Set(m, oneof)
	if !ok {
		return "", false
	}
	return field.Name(), true
}
