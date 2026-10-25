package wsm

import (
	"regexp"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/sourcescan"
)

// withUnknown is MSG carrying the raw fields RAW as unknown ones.
func withUnknown(msg protoreflect.ProtoMessage, raw []byte) protoreflect.Message {
	m := msg.ProtoReflect()
	m.SetUnknown(raw)
	return m
}

func bytesField(num protowire.Number, value string) []byte {
	b := protowire.AppendTag(nil, num, protowire.BytesType)
	return protowire.AppendString(b, value)
}

func TestTakeUnknown(t *testing.T) {
	tests := []struct {
		name      string
		raw       []byte
		wantValue string
		wantFound bool
		wantKept  []byte
	}{
		{name: "an absent field is not found and nothing changes", raw: bytesField(7, "other"), wantKept: bytesField(7, "other")},
		{name: "the field is taken and the rest kept", raw: append(bytesField(1, "retired"), bytesField(7, "other")...), wantValue: "retired", wantFound: true, wantKept: bytesField(7, "other")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			m := withUnknown(&frontendv1.FeedResponseProse{}, tt.raw)

			// Act
			value, found, err := takeUnknown(m, 1)

			// Assert
			if err != nil {
				t.Fatalf("takeUnknown: %v", err)
			}
			if string(value) != tt.wantValue || found != tt.wantFound || string(m.GetUnknown()) != string(tt.wantKept) {
				t.Fatalf("takeUnknown = (%q, %v), unknown left %x; want (%q, %v), %x", value, found, m.GetUnknown(), tt.wantValue, tt.wantFound, tt.wantKept)
			}
		})
	}
}

func TestTakeUnknownRefusesAFieldOfAnotherWireType(t *testing.T) {
	// Arrange: field 1 as a varint.
	raw := protowire.AppendVarint(protowire.AppendTag(nil, 1, protowire.VarintType), 5)
	m := withUnknown(&frontendv1.FeedResponseProse{}, raw)

	// Act
	_, _, err := takeUnknown(m, 1)

	// Assert
	if err == nil {
		t.Fatal("takeUnknown took a varint field as a retired length-delimited one")
	}
}

func TestHasUnknown(t *testing.T) {
	// Arrange
	m := withUnknown(&frontendv1.FeedResponseProse{}, bytesField(1, "retired"))

	// Act / Assert
	if !hasUnknown(m, 1) || hasUnknown(m, 2) {
		t.Fatalf("hasUnknown(1), hasUnknown(2) = %v, %v; want true, false", hasUnknown(m, 1), hasUnknown(m, 2))
	}
	if len(m.GetUnknown()) == 0 {
		t.Fatal("hasUnknown removed the field it was asked about")
	}
}

// TestWalkMessagesVisitsParentsBeforeChildren pins the walk's order and
// reach: a nested message and a list element are each visited, after the
// message holding them.
func TestWalkMessagesVisitsParentsBeforeChildren(t *testing.T) {
	// Arrange
	row := &frontendv1.FeedRow{Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{
		Label: &frontendv1.FeedMergeTabLabel{},
	}}}
	var visited []protoreflect.FullName

	// Act
	changed, err := walkMessages(row.ProtoReflect(), func(m protoreflect.Message) (bool, error) {
		visited = append(visited, m.Descriptor().FullName())
		return m.Descriptor().FullName() == "frontend.v1.FeedMergeTabLabel", nil
	})

	// Assert
	if err != nil || !changed {
		t.Fatalf("walkMessages = (%v, %v), want a change and no error", changed, err)
	}
	want := []protoreflect.FullName{"frontend.v1.FeedRow", "frontend.v1.FeedMergeTab", "frontend.v1.FeedMergeTabLabel"}
	if len(visited) != len(want) {
		t.Fatalf("visited %v, want %v", visited, want)
	}
	for i := range want {
		if visited[i] != want[i] {
			t.Fatalf("visited %v, want %v", visited, want)
		}
	}
}

// TestOnlyTheSharedLoopRewritesStoredRows keeps every layout step's row
// rewrite on rewriteDurableRows: a step re-encoding rows its own way is a
// second copy of the loop to drift.
func TestOnlyTheSharedLoopRewritesStoredRows(t *testing.T) {
	// Arrange
	writes := regexp.MustCompile(`UPDATE durable_feed_rows SET row =`)

	// Act
	var offenders []string
	for _, file := range sourcescan.Production(t) {
		if file.Name != "rowrewrite.go" && writes.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v rewrite stored rows by hand; use rewriteDurableRows", offenders)
	}
}
