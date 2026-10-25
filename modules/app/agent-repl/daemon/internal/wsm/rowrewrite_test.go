package wsm

import (
	"errors"
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

func varintField(num protowire.Number, value uint64) []byte {
	return protowire.AppendVarint(protowire.AppendTag(nil, num, protowire.VarintType), value)
}

func TestPeekUnknown(t *testing.T) {
	tests := []struct {
		name      string
		raw       []byte
		wantValue string
		wantFound bool
		wantErr   bool
	}{
		{name: "an absent field is not found", raw: bytesField(7, "other")},
		{name: "a present field is answered", raw: append(bytesField(7, "other"), bytesField(1, "retired")...), wantValue: "retired", wantFound: true},
		{name: "a field of another wire type is refused", raw: varintField(1, 5), wantErr: true},
		{name: "unknown bytes that do not parse are refused", raw: []byte{0x0a, 0x09}, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			m := withUnknown(&frontendv1.FeedResponseProse{}, tt.raw)

			// Act
			value, found, err := peekUnknown(m, 1)

			// Assert
			if (err != nil) != tt.wantErr || string(value) != tt.wantValue || found != tt.wantFound {
				t.Fatalf("peekUnknown = (%q, %v, %v), want (%q, %v, error %v)", value, found, err, tt.wantValue, tt.wantFound, tt.wantErr)
			}
			if string(m.GetUnknown()) != string(tt.raw) {
				t.Fatalf("peekUnknown changed the unknown fields to %x", m.GetUnknown())
			}
		})
	}
}

func TestDropUnknown(t *testing.T) {
	tests := []struct {
		name      string
		raw       []byte
		wantFound bool
		wantKept  []byte
	}{
		{name: "an absent field changes nothing", raw: bytesField(7, "other"), wantKept: bytesField(7, "other")},
		{name: "a length-delimited field is dropped and the rest kept", raw: append(bytesField(1, "retired"), bytesField(7, "other")...), wantFound: true, wantKept: bytesField(7, "other")},
		{name: "a varint field is dropped too", raw: append(varintField(1, 5), bytesField(7, "other")...), wantFound: true, wantKept: bytesField(7, "other")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			m := withUnknown(&frontendv1.FeedResponseProse{}, tt.raw)

			// Act
			found, err := dropUnknown(m, 1)

			// Assert
			if err != nil || found != tt.wantFound || string(m.GetUnknown()) != string(tt.wantKept) {
				t.Fatalf("dropUnknown = (%v, %v), unknown left %x; want (%v, nil), %x", found, err, m.GetUnknown(), tt.wantFound, tt.wantKept)
			}
		})
	}
}

func TestCarryUnknown(t *testing.T) {
	refused := errors.New("not a shape this step carries")
	tests := []struct {
		name        string
		raw         []byte
		successor   bool
		carryErr    error
		wantChanged bool
		wantErr     bool
		wantCarried string
		wantKept    []byte
	}{
		{name: "an absent field carries nothing", raw: bytesField(7, "other"), wantKept: bytesField(7, "other")},
		{name: "a field is carried and removed", raw: append(bytesField(1, "retired"), bytesField(7, "other")...), wantChanged: true, wantCarried: "retired", wantKept: bytesField(7, "other")},
		{name: "a field beside its successor is refused untouched", raw: bytesField(1, "retired"), successor: true, wantErr: true, wantKept: bytesField(1, "retired")},
		{name: "a carry that refuses leaves the field", raw: bytesField(1, "retired"), carryErr: refused, wantErr: true, wantCarried: "retired", wantKept: bytesField(1, "retired")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			m := withUnknown(&frontendv1.FeedResponseProse{}, tt.raw)
			var carried string

			// Act
			changed, err := carryUnknown(m, 1, tt.successor, func(v []byte) error {
				carried = string(v)
				return tt.carryErr
			})

			// Assert
			if changed != tt.wantChanged || (err != nil) != tt.wantErr || carried != tt.wantCarried {
				t.Fatalf("carryUnknown = (%v, %v) carrying %q, want (%v, error %v) carrying %q", changed, err, carried, tt.wantChanged, tt.wantErr, tt.wantCarried)
			}
			if string(m.GetUnknown()) != string(tt.wantKept) {
				t.Fatalf("unknown left %x, want %x", m.GetUnknown(), tt.wantKept)
			}
		})
	}
}

// TestRetiredFieldsAreCarriedThroughCarryUnknown keeps every step's carry of
// a retired field on the one helper, which removes the field only after its
// successor is written: a hand-rolled peek-then-take is a second copy to
// drift, and the one this replaced stripped the field from a row it then
// refused.
func TestRetiredFieldsAreCarriedThroughCarryUnknown(t *testing.T) {
	// Arrange
	handRolled := regexp.MustCompile(`peekUnknown\(`)

	// Act
	var offenders []string
	for _, file := range sourcescan.Production(t) {
		if file.Name != "rowrewrite.go" && handRolled.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v peek a retired field themselves; carry it through carryUnknown", offenders)
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
