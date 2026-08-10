package sessioncontroller

import (
	"strings"
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// withholdTestItem is one user item carrying the uuid the assertions name it by.
func withholdTestItem(uuid string) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{
		Uuid:   uuid,
		Source: frontendv1.ConversationSource_CONVERSATION_SOURCE_USER,
		Item: &frontendv1.ConversationItem_UserMessage{UserMessage: &datav1.ApiUserMessage{
			Content: &datav1.ApiUserMessage_ContentString{ContentString: uuid},
		}},
	}
}

func withholdTestDelta(uuids ...string) *frontendv1.ConversationDelta {
	cd := &frontendv1.ConversationDelta{ThroughSeq: 7}
	for _, u := range uuids {
		cd.Items = append(cd.Items, withholdTestItem(u))
	}
	return cd
}

func keptUUIDs(cd *frontendv1.ConversationDelta) []string {
	var out []string
	for _, it := range cd.GetItems() {
		out = append(out, it.GetUuid())
	}
	return out
}

// withholdTestConsumer is a consumer with nothing but its logging path wired:
// withholdItems reads no other field.
func withholdTestConsumer(cl *logCapture) *consumer {
	return &consumer{logf: cl.logf}
}

func TestWithholdItemsKeepsTheItemsItsJudgeKeeps(t *testing.T) {
	tests := []struct {
		name     string
		uuids    []string
		withhold map[string]bool
		want     []string
	}{
		{
			name:  "no verdict withholds anything",
			uuids: []string{"a", "b", "c"},
			want:  []string{"a", "b", "c"},
		},
		{
			name:     "a withhold in the middle keeps delta order around it",
			uuids:    []string{"a", "b", "c"},
			withhold: map[string]bool{"b": true},
			want:     []string{"a", "c"},
		},
		{
			name:     "the first item withheld",
			uuids:    []string{"a", "b"},
			withhold: map[string]bool{"a": true},
			want:     []string{"b"},
		},
		{
			name:     "the last item withheld",
			uuids:    []string{"a", "b"},
			withhold: map[string]bool{"b": true},
			want:     []string{"a"},
		},
		{
			name:     "every item withheld empties the delta",
			uuids:    []string{"a", "b"},
			withhold: map[string]bool{"a": true, "b": true},
			want:     nil,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			cl := &logCapture{}
			c := withholdTestConsumer(cl)
			cd := withholdTestDelta(tc.uuids...)

			// Act
			withheld := c.withholdItems(cd, func(it *frontendv1.ConversationItem) withholdVerdict {
				if tc.withhold[it.GetUuid()] {
					return withholdItem("withheld " + it.GetUuid())
				}
				return keepItem
			})

			// Assert
			got := keptUUIDs(cd)
			if strings.Join(got, ",") != strings.Join(tc.want, ",") {
				t.Errorf("kept %v, want %v", got, tc.want)
			}
			if withheld != len(tc.withhold) {
				t.Errorf("withheld count = %d, want %d", withheld, len(tc.withhold))
			}
		})
	}
}

func TestWithholdItemsLogsEveryWithheldItemsReason(t *testing.T) {
	// Arrange: a silent drop is indistinguishable from a lost record.
	cl := &logCapture{}
	c := withholdTestConsumer(cl)
	cd := withholdTestDelta("a", "b")

	// Act
	c.withholdItems(cd, func(it *frontendv1.ConversationItem) withholdVerdict {
		return withholdItem("withheld " + it.GetUuid())
	})

	// Assert
	for _, want := range []string{"withheld a", "withheld b"} {
		if !cl.contains(want) {
			t.Errorf("no log line accounts for %q", want)
		}
	}
}

func TestWithholdItemsPassesTheReasonThroughVerbatim(t *testing.T) {
	// Arrange: a reason carrying % verbs must not be re-expanded by the logger.
	cl := &logCapture{}
	c := withholdTestConsumer(cl)
	cd := withholdTestDelta("a")

	// Act
	c.withholdItems(cd, func(*frontendv1.ConversationItem) withholdVerdict {
		return withholdItem(`withheld ws="100%" seq=%d`)
	})

	// Assert
	if !cl.contains(`withheld ws="100%" seq=%d`) {
		t.Error("the reason was not logged verbatim")
	}
}

func TestWithholdItemsWithholdsSilentlyOnAnEmptyReason(t *testing.T) {
	// Arrange: the aggregate-accounting curator (keepaliveexclude.go) reports
	// its census itself rather than one line per item.
	cl := &logCapture{}
	c := withholdTestConsumer(cl)
	cd := withholdTestDelta("a")

	// Act
	withheld := c.withholdItems(cd, func(*frontendv1.ConversationItem) withholdVerdict {
		return withholdItem("")
	})

	// Assert
	if withheld != 1 {
		t.Errorf("withheld count = %d, want 1", withheld)
	}
	if len(cd.GetItems()) != 0 {
		t.Errorf("kept %d item(s), want none", len(cd.GetItems()))
	}
	if n := cl.count(""); n != 0 {
		t.Errorf("logged %d line(s) for a silent withholding, want none", n)
	}
}

func TestWithholdItemsLeavesAnEmptyDeltaAlone(t *testing.T) {
	// Arrange: an emptied delta is still pushed for its through_seq, so a later
	// curator must find it intact rather than judged again.
	cl := &logCapture{}
	c := withholdTestConsumer(cl)
	cd := withholdTestDelta()

	// Act
	withheld := c.withholdItems(cd, func(*frontendv1.ConversationItem) withholdVerdict {
		t.Error("the judge ran on a delta with no items")
		return keepItem
	})

	// Assert
	if withheld != 0 {
		t.Errorf("withheld count = %d, want 0", withheld)
	}
	if cd.GetThroughSeq() != 7 {
		t.Errorf("through_seq = %d, want the delta's own 7", cd.GetThroughSeq())
	}
}

func TestWithholdItemsToleratesANilDelta(t *testing.T) {
	// Arrange: conversationDeltaFromEvent yields nil for a non-conversational
	// event, and every curator runs on whatever it returns.
	cl := &logCapture{}
	c := withholdTestConsumer(cl)

	// Act
	withheld := c.withholdItems(nil, func(*frontendv1.ConversationItem) withholdVerdict {
		t.Error("the judge ran on a nil delta")
		return keepItem
	})

	// Assert
	if withheld != 0 {
		t.Errorf("withheld count = %d, want 0", withheld)
	}
}
