package history

import (
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"testing"

	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// goldenDir holds the stored-line cases the shim's TypeScript reader reads too.
const goldenDir = "../../../../testdata/history-golden"

func TestEntryAtRendersEverySharedGoldenCase(t *testing.T) {
	files, err := filepath.Glob(filepath.Join(goldenDir, "*.json"))
	if err != nil {
		t.Fatalf("listing the golden cases: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no golden cases under %s", goldenDir)
	}
	for _, file := range files {
		t.Run(filepath.Base(file), func(t *testing.T) {
			// Arrange
			raw, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("reading: %v", err)
			}
			var golden struct {
				Line  json.RawMessage `json:"line"`
				Entry json.RawMessage `json:"entry"`
			}
			if err := json.Unmarshal(raw, &golden); err != nil {
				t.Fatalf("decoding: %v", err)
			}
			line, want := &storev1.StoreLineAt{}, &conversationv1.HistoryEntryAt{}
			if err := protojson.Unmarshal(golden.Line, line); err != nil {
				t.Fatalf("decoding the line: %v", err)
			}
			if err := protojson.Unmarshal(golden.Entry, want); err != nil {
				t.Fatalf("decoding the entry: %v", err)
			}

			// Act
			got, err := EntryAt(line)

			// Assert
			if err != nil {
				t.Fatalf("EntryAt: %v", err)
			}
			if !proto.Equal(got, want) {
				t.Fatalf("EntryAt = %v, want %v", got, want)
			}
		})
	}
}

func line(at string, item *storev1.StoreAgentItem) *storev1.StoreLineAt {
	return &storev1.StoreLineAt{At: &storev1.StoreItemPointer{Value: at}, Line: &storev1.StorePageLine{AgentItem: item}}
}

func prompt() *storev1.StoreAgentItem {
	return &storev1.StoreAgentItem{Item: &storev1.StoreAgentItem_AgentPrompt{AgentPrompt: &conversationv1.AgentPrompt{}}}
}

func TestEntryAtRefusesAnUndrawableLine(t *testing.T) {
	tests := []struct {
		name string
		line *storev1.StoreLineAt
	}{
		{name: "no pointer", line: &storev1.StoreLineAt{Line: &storev1.StorePageLine{AgentItem: prompt()}}},
		{name: "an empty pointer", line: line("", prompt())},
		{name: "no content", line: &storev1.StoreLineAt{At: &storev1.StoreItemPointer{Value: "p-1"}}},
		{name: "no item arm", line: line("p-1", &storev1.StoreAgentItem{})},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := EntryAt(tt.line)

			// Assert
			if !errors.Is(err, ErrUndrawable) {
				t.Fatalf("EntryAt err = %v, want ErrUndrawable", err)
			}
		})
	}
}

func head(between bool, steps bool) *storev1.StoreTurnHead {
	h := &storev1.StoreTurnHead{Line: line("p-9", prompt())}
	switch {
	case between && steps:
		h.Between = &storev1.StoreTurnHead_StepsBetween{StepsBetween: &storev1.StoreTurnHeadStepsBetween{}}
	case between:
		h.Between = &storev1.StoreTurnHead_Adjacent{Adjacent: &storev1.StoreTurnHeadAdjacent{}}
	}
	return h
}

func TestPage(t *testing.T) {
	more := &storev1.ReadAgentPageMore{LastItem: &storev1.StoreItemPointer{Value: "p-1"}}
	tests := []struct {
		name    string
		heads   []*storev1.StoreTurnHead
		more    *storev1.ReadAgentPageMore
		floor   bool
		want    *conversationv1.HistoryPage
		wantErr bool
	}{
		{
			name:  "a floor page",
			floor: true,
			want: &conversationv1.HistoryPage{
				Entries:  []*conversationv1.HistoryEntryAt{{At: &conversationv1.HistoryPointer{Value: "p-2"}, Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{}}}}},
				Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
			},
		},
		{
			name: "a page with more below it names where to walk from",
			more: more,
			want: &conversationv1.HistoryPage{
				Entries:  []*conversationv1.HistoryEntryAt{{At: &conversationv1.HistoryPointer{Value: "p-2"}, Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{}}}}},
				Boundary: &conversationv1.HistoryPage_More{More: &conversationv1.HistoryMore{LastEntry: &conversationv1.HistoryPointer{Value: "p-1"}}},
			},
		},
		{
			name:  "turn heads keep their between arm",
			heads: []*storev1.StoreTurnHead{head(true, false), head(true, true)},
			floor: true,
			want: &conversationv1.HistoryPage{
				Entries: []*conversationv1.HistoryEntryAt{{At: &conversationv1.HistoryPointer{Value: "p-2"}, Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{}}}}},
				Heads: []*conversationv1.HistoryTurnHead{
					{Entry: &conversationv1.HistoryEntryAt{At: &conversationv1.HistoryPointer{Value: "p-9"}, Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{}}}}, Between: &conversationv1.HistoryTurnHead_Adjacent{Adjacent: &conversationv1.HistoryTurnHeadAdjacent{}}},
					{Entry: &conversationv1.HistoryEntryAt{At: &conversationv1.HistoryPointer{Value: "p-9"}, Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{}}}}, Between: &conversationv1.HistoryTurnHead_StepsBetween{StepsBetween: &conversationv1.HistoryTurnHeadStepsBetween{}}},
				},
				Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
			},
		},
		{name: "no boundary is undrawable", wantErr: true},
		{name: "both boundaries are undrawable", more: more, floor: true, wantErr: true},
		{name: "more with no pointer is undrawable", more: &storev1.ReadAgentPageMore{}, wantErr: true},
		{name: "a turn head with no between arm is undrawable", heads: []*storev1.StoreTurnHead{head(false, false)}, floor: true, wantErr: true},
		{name: "a turn head with no line is undrawable", heads: []*storev1.StoreTurnHead{{Between: &storev1.StoreTurnHead_Adjacent{Adjacent: &storev1.StoreTurnHeadAdjacent{}}}}, floor: true, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			lines := []*storev1.StoreLineAt{line("p-2", prompt())}

			// Act
			got, err := Page(lines, tt.heads, tt.more, tt.floor)

			// Assert
			if tt.wantErr {
				if !errors.Is(err, ErrUndrawable) {
					t.Fatalf("Page err = %v, want ErrUndrawable", err)
				}
				return
			}
			if err != nil {
				t.Fatalf("Page: %v", err)
			}
			if !proto.Equal(got, tt.want) {
				t.Fatalf("Page = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestOpeningAndContinuationPagesRefuseNoPage(t *testing.T) {
	if _, err := OpeningPage(nil); !errors.Is(err, ErrUndrawable) {
		t.Fatalf("OpeningPage(nil) err = %v, want ErrUndrawable", err)
	}
	if _, err := ContinuationPage(nil); !errors.Is(err, ErrUndrawable) {
		t.Fatalf("ContinuationPage(nil) err = %v, want ErrUndrawable", err)
	}
}
