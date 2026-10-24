// Package protofile reads the request files the daemon hands the runner (a
// testrun.v1.TestRunRequest, a testrun.v1.RedundancyRequest).
//
// A REQUEST THE RUNNER DOES NOT FULLY UNDERSTAND IS REFUSED. The daemon is the
// main checkout's build and the runner is the workspace's own, so a worktree
// older than the daemon can be handed a request with fields it predates. Go's
// protobuf keeps such fields as unknown bytes and reads the rest, which would
// run something narrower than was asked (a whole-suite target dropped, a
// coverage flag ignored) and report it as passed. Any unknown field anywhere in
// the message refuses the request instead, naming where it was.
package protofile

import (
	"fmt"
	"os"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
)

// Read decodes the file at path into m, refusing a file that holds a field m
// does not know.
func Read(path string, m proto.Message) error {
	data, err := os.ReadFile(path)
	if err != nil {
		return fmt.Errorf("read %s: %w", path, err)
	}
	if err := proto.Unmarshal(data, m); err != nil {
		return fmt.Errorf("decode %s: %w", path, err)
	}
	if where := unknown(m.ProtoReflect()); where != "" {
		return fmt.Errorf("%s holds a field this runner does not know (in %s): the worktree predates the daemon's request; rebase it onto the main branch", path, where)
	}
	return nil
}

// unknown answers the full name of the first message under m that carries
// unknown fields, or "" when none does.
func unknown(m protoreflect.Message) string {
	if len(m.GetUnknown()) > 0 {
		return string(m.Descriptor().FullName())
	}
	found := ""
	m.Range(func(fd protoreflect.FieldDescriptor, v protoreflect.Value) bool {
		switch {
		case fd.IsList() && fd.Message() != nil:
			list := v.List()
			for i := 0; i < list.Len() && found == ""; i++ {
				found = unknown(list.Get(i).Message())
			}
		case fd.IsMap() && fd.MapValue().Message() != nil:
			v.Map().Range(func(_ protoreflect.MapKey, mv protoreflect.Value) bool {
				found = unknown(mv.Message())
				return found == ""
			})
		case fd.Message() != nil && !fd.IsList() && !fd.IsMap():
			found = unknown(v.Message())
		}
		return found == ""
	})
	return found
}
