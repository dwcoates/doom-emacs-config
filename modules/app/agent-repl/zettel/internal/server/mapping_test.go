package server

import (
	"testing"
	"time"

	zettelv1 "agentrepl/proto/zettel/v1"
	"agentrepl/zettel/internal/graph"

	"google.golang.org/protobuf/reflect/protoreflect"
)

// TestMappingCoversEveryField fails when a field is added to a protocol
// message the mapping translates and the mapping is not taught it: every
// field of each mapped message must be set by the mapping from a fully
// populated graph value.
func TestMappingCoversEveryField(t *testing.T) {
	context := "around"
	full := graph.Node{
		ID: "n", Thread: "t",
		Parent:   graph.Parent{Of: &graph.AskedOf{Node: "p", Revision: 2, Quote: &graph.Quote{Text: "q", Context: &context}}},
		Question: "question", Answer: graph.Revision{Number: 2, Markdown: "md", WrittenAt: time.UnixMilli(5)},
		CreatedAt: time.UnixMilli(4), Sightings: 3,
	}
	pending := graph.Pending{ID: "e", Thread: "t", Turn: "turn", Parent: full.Parent, Question: "q", BegunAt: time.UnixMilli(9)}
	for name, msg := range map[string]protoreflect.ProtoMessage{
		"Node":            NodeToProto(full),
		"PendingExchange": PendingToProto(pending),
		"NodeParentNode":  ParentToProto(full.Parent).GetNode(),
		"NodeQuote":       ParentToProto(full.Parent).GetNode().GetQuote(),
	} {
		t.Run(name, func(t *testing.T) {
			// Arrange
			fields := msg.ProtoReflect().Descriptor().Fields()

			// Act
			for i := 0; i < fields.Len(); i++ {
				// Assert
				if !msg.ProtoReflect().Has(fields.Get(i)) {
					t.Errorf("%s.%s is not mapped", name, fields.Get(i).Name())
				}
			}
		})
	}
}

func TestParentRoundTrips(t *testing.T) {
	context := "c"
	cases := []struct {
		name   string
		parent graph.Parent
	}{
		{"thread start", graph.Parent{}},
		{"asked of a node", graph.Parent{Of: &graph.AskedOf{Node: "n", Revision: 1}}},
		{"quoted without context", graph.Parent{Of: &graph.AskedOf{Node: "n", Revision: 1, Quote: &graph.Quote{Text: "q"}}}},
		{"quoted with context", graph.Parent{Of: &graph.AskedOf{Node: "n", Revision: 3, Quote: &graph.Quote{Text: "q", Context: &context}}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			wire := ParentToProto(tc.parent)

			// Act
			back, field, ok := ParentFromProto(wire)

			// Assert
			if !ok {
				t.Fatalf("round trip refused at %s", field)
			}
			if (back.Of == nil) != (tc.parent.Of == nil) {
				t.Fatalf("back = %+v, want %+v", back, tc.parent)
			}
			if back.Of != nil {
				if back.Of.Node != tc.parent.Of.Node || back.Of.Revision != tc.parent.Of.Revision {
					t.Errorf("back = %+v, want %+v", back.Of, tc.parent.Of)
				}
				if (back.Of.Quote == nil) != (tc.parent.Of.Quote == nil) {
					t.Errorf("quote presence differs")
				}
				if back.Of.Quote != nil && (tc.parent.Of.Quote.Context == nil) != (back.Of.Quote.Context == nil) {
					t.Errorf("context presence differs")
				}
			}
		})
	}
}

func TestParentFromProtoRefusesAnUnsetOrigin(t *testing.T) {
	// Arrange
	wire := &zettelv1.NodeParent{}

	// Act
	_, field, ok := ParentFromProto(wire)

	// Assert
	if ok || field != "parent" {
		t.Errorf("ok=%v field=%q, want a refusal at parent", ok, field)
	}
}
