package convert

import (
	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/reflect/protoregistry"
)

// newFromRegistry returns a fresh, mutable instance of the concrete generated
// message registered for md. The nested messages the converter builds are all
// generated data.v1 / core.v1 types (never dynamic), so the global registry
// always resolves them. A missing registration is a build-time impossibility
// (the stubs are linked in), so we panic loudly rather than paper over it.
func newFromRegistry(md protoreflect.MessageDescriptor) protoreflect.Message {
	mt, err := protoregistry.GlobalTypes.FindMessageByName(md.FullName())
	if err != nil {
		panic("convert: unregistered message type " + string(md.FullName()) + ": " + err.Error())
	}
	return mt.New()
}
