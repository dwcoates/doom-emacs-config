package frontend

import (
	"strings"
	"testing"

	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/reflect/protoregistry"

	_ "agentrepl/proto/agentshim/core/v1"
	_ "agentrepl/proto/agentshim/data/v1"
	_ "agentrepl/proto/frontend/v1"
)

// Lineage is a MESSAGE's fact and nothing else's, and these tests hold the
// schema to that structurally rather than by review.
//
// The failure they exist to prevent is silent. If a record that renders as
// nothing — a turn boundary, a heartbeat, a latency sample — acquires a
// top_level_message_id, it becomes a phantom feed row: a page query asking for
// ten messages returns ten distinct owner ids, several of which are not
// messages, and the user gets a short screen with no error anywhere. Nothing
// in protoc can see that, so it is pinned here.

const (
	lineageMessage = "agentshim.frontend.v1.MessageLineage"
	lineageOwner   = "agentshim.frontend.v1.Message"
	lineageField   = "lineage"
)

// forEachMessage visits every message descriptor in every registered file,
// nested types included.
func forEachMessage(t *testing.T, visit func(protoreflect.MessageDescriptor)) {
	t.Helper()
	var walk func(protoreflect.MessageDescriptors)
	walk = func(ms protoreflect.MessageDescriptors) {
		for i := 0; i < ms.Len(); i++ {
			m := ms.Get(i)
			visit(m)
			walk(m.Messages())
		}
	}
	protoregistry.GlobalFiles.RangeFiles(func(fd protoreflect.FileDescriptor) bool {
		if !strings.HasPrefix(string(fd.Package()), "agentshim.") {
			return true
		}
		walk(fd.Messages())
		return true
	})
}

// TestMessageLineageHasExactlyOneCarrier is the structural form of "category-C
// records carry no lineage".
//
// It does not enumerate the category-C records, because an enumeration goes
// stale the moment a new one is added. It asserts the stronger property that
// makes the enumeration unnecessary: MessageLineage is reachable from exactly
// ONE field on the whole contract. A record that is not a Message therefore has
// nowhere to put one, which is what "structural rather than an empty string"
// means.
func TestMessageLineageHasExactlyOneCarrier(t *testing.T) {
	// Arrange
	var carriers []string

	// Act
	forEachMessage(t, func(m protoreflect.MessageDescriptor) {
		fields := m.Fields()
		for i := 0; i < fields.Len(); i++ {
			f := fields.Get(i)
			if f.Kind() != protoreflect.MessageKind && f.Kind() != protoreflect.GroupKind {
				continue
			}
			if string(f.Message().FullName()) == lineageMessage {
				carriers = append(carriers, string(m.FullName())+"."+string(f.Name()))
			}
		}
	})

	// Assert
	want := lineageOwner + "." + lineageField
	if len(carriers) != 1 || carriers[0] != want {
		t.Fatalf("MessageLineage is carried by %v, want exactly [%s]: a second carrier means some record that is not a Message can claim a top_level_message_id, and a page query would return it as a phantom feed row", carriers, want)
	}
}

// TestEventCarriesNoOwnershipVocabulary names the trap directly.
//
// agentshim.core.v1.Event is the durable record union, and its payload arms
// are the category-C records — session and turn boundaries, heartbeats, latency
// samples, claim bridges, query lifecycle, usage observations, rewinds,
// file-plane diagnostics — alongside the ones that do compose messages. Hanging
// ownership on the EVENT rather than on the message-bearing arms is the one
// edit that would give every category-C record a lineage, so the event union is
// pinned as free of that vocabulary in any spelling.
func TestEventCarriesNoOwnershipVocabulary(t *testing.T) {
	// Arrange
	d, err := protoregistry.GlobalFiles.FindDescriptorByName("agentshim.core.v1.Event")
	if err != nil {
		t.Fatalf("resolve Event: %v", err)
	}
	env, ok := d.(protoreflect.MessageDescriptor)
	if !ok {
		t.Fatalf("agentshim.core.v1.Event resolved to %T, want a message descriptor", d)
	}
	banned := []string{lineageField, "top_level_message_id", "parent_message_id"}

	// Act / Assert
	fields := env.Fields()
	for i := 0; i < fields.Len(); i++ {
		name := string(fields.Get(i).Name())
		for _, b := range banned {
			if name == b {
				t.Fatalf("Event declares %q: every category-C record is an Event arm, so ownership on the union makes turn boundaries and heartbeats into phantom feed rows. Lineage belongs on the message-bearing arms only", name)
			}
		}
	}
}
