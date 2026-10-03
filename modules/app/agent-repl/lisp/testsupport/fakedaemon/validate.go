package main

import (
	"fmt"
	"strings"

	v1 "agentrepl/proto/agentrepl/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
)

// THE VALIDATION INVARIANT (teamlead prompt, standing conventions): an unset
// non-optional field is ILLEGAL, everywhere, immediately — a request carrying
// one is answered with an error at once.  protojson will not do this for us:
// a missing message field is perfectly legal JSON, and an enum left at its
// UNSPECIFIED zero is simply the proto3 default.
//
// So the fake enforces it explicitly, on every request, the way the real
// daemon must.  This is the half of the round-trip check that catches an
// encoder which forgot a field rather than one which misspelled it (the
// misspelling half is the strict protojson codec).
func validateRequest(msg proto.Message) error {
	return validateMessage(msg.ProtoReflect(), string(msg.ProtoReflect().Descriptor().Name()), 0)
}

// validateWatchDaemonRequest is the WatchDaemon refusal the real daemon
// performs: the generic invariant (the `client` oneof must name an arm, and an
// Emacs arm's REQUIRED `focus` must be set with its arm named), and an Emacs
// arm's REQUIRED elisp_build, which protojson cannot distinguish from "nobody
// filled this in" because an empty string is the proto3 default.
func validateWatchDaemonRequest(req *v1.WatchDaemonRequest) error {
	if err := validateRequest(req); err != nil {
		return err
	}
	if emacs := req.GetEmacs(); emacs != nil && emacs.GetElispBuild() == "" {
		return fmt.Errorf("WatchDaemonRequest.emacs.elisp_build: an empty build is illegal")
	}
	if emacs := req.GetEmacs(); emacs != nil && emacs.GetInstance().GetValue() == "" {
		return fmt.Errorf("WatchDaemonRequest.emacs.instance: this Emacs process's identity is required")
	}
	return nil
}

func validateMessage(m protoreflect.Message, path string, depth int) error {
	if depth >= maxFillDepth {
		return nil
	}
	desc := m.Descriptor()

	for i := 0; i < desc.Oneofs().Len(); i++ {
		od := desc.Oneofs().Get(i)
		if od.IsSynthetic() {
			// The `optional` keyword's encoding: absence is legal there.
			continue
		}
		if m.WhichOneof(od) == nil {
			return fmt.Errorf("%s.%s: an unset oneof is illegal", path, od.Name())
		}
	}

	for i := 0; i < desc.Fields().Len(); i++ {
		fd := desc.Fields().Get(i)
		if fd.IsList() || fd.IsMap() || fd.HasOptionalKeyword() {
			// Repeated fields may legally be empty; `optional` expresses
			// absence by presence, which is the contract's own PRESENCE,
			// NEVER SENTINELS rule.
			continue
		}
		fieldPath := path + "." + string(fd.Name())

		if fd.Kind() == protoreflect.EnumKind {
			// An UNSPECIFIED enum value is the wire's "nobody filled this in".
			zero := fd.Enum().Values().ByNumber(0)
			if zero != nil && strings.HasSuffix(string(zero.Name()), "_UNSPECIFIED") &&
				m.Get(fd).Enum() == 0 {
				return fmt.Errorf("%s: %s is illegal", fieldPath, zero.Name())
			}
			continue
		}

		if fd.Kind() != protoreflect.MessageKind {
			continue
		}
		if fd.ContainingOneof() != nil {
			// The oneof check above already proved an arm is set; validate
			// only the arm that actually is.
			if m.WhichOneof(fd.ContainingOneof()) != fd {
				continue
			}
		} else if !m.Has(fd) {
			return fmt.Errorf("%s: a non-optional message field is unset", fieldPath)
		}
		if err := validateMessage(m.Get(fd).Message(), fieldPath, depth+1); err != nil {
			return err
		}
	}
	return nil
}
