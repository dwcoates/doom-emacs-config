package topbar

import (
	"slices"
	"testing"

	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// optionNames are the off chip's menu rows by label, in order.
func optionNames(chip *frontendv1.TopbarQueueBypass) []string {
	var names []string
	for _, option := range chip.GetOff().GetOptions() {
		names = append(names, option.GetDisplayName())
	}
	return names
}

// The four contexts, as the workspace verbs offer them.
var (
	fullMain      = &frontendv1.TopbarQueueBypassContext{Context: &frontendv1.TopbarQueueBypassContext_FullMain{FullMain: &frontendv1.TopbarQueueBypassContextFullMain{}}}
	compactedMain = &frontendv1.TopbarQueueBypassContext{Context: &frontendv1.TopbarQueueBypassContext_CompactedMain{CompactedMain: &frontendv1.TopbarQueueBypassContextCompactedMain{}}}
	existing      = &frontendv1.TopbarQueueBypassContext{Context: &frontendv1.TopbarQueueBypassContext_Existing{Existing: &frontendv1.TopbarQueueBypassContextExisting{}}}
	existingPlus  = &frontendv1.TopbarQueueBypassContext{Context: &frontendv1.TopbarQueueBypassContext_ExistingPlusCompactedMain{ExistingPlusCompactedMain: &frontendv1.TopbarQueueBypassContextExistingPlusCompactedMain{}}}
)

func TestQueueBypassChipProjectsTheStanding(t *testing.T) {
	tests := []struct {
		name        string
		chip        QueueBypass
		wantNil     bool
		wantOn      bool
		wantHeldBy  string
		wantOptions []string
	}{
		{name: "a work account draws no chip", chip: QueueBypass{}, wantNil: true},
		{name: "held here draws the lit chip with no menu", chip: QueueBypass{Offered: true, Holds: true}, wantOn: true},
		{name: "held by a sibling draws its name", chip: QueueBypass{Offered: true, HeldBy: "other"}, wantHeldBy: "other"},
		{name: "free with nothing parked offers the two main-context options", chip: QueueBypass{Offered: true, Offers: []*frontendv1.TopbarQueueBypassContext{fullMain, compactedMain}},
			wantOptions: []string{"Full main context", "Compacted main context"}},
		{name: "free with a bypass conversation parked offers all four", chip: QueueBypass{Offered: true, Offers: []*frontendv1.TopbarQueueBypassContext{fullMain, compactedMain, existing, existingPlus}},
			wantOptions: []string{"Full main context", "Compacted main context", "Existing bypass context", "Existing bypass plus compacted main"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := queueBypassChip(tt.chip)

			// Assert.
			if (got == nil) != tt.wantNil {
				t.Fatalf("chip = %v, want nil %v", got, tt.wantNil)
			}
			if got.GetOn() != nil != tt.wantOn {
				t.Fatalf("on = %v, want %v", got.GetOn() != nil, tt.wantOn)
			}
			if name := got.GetHeldElsewhere().GetWorkspaceName(); name != tt.wantHeldBy {
				t.Fatalf("held by = %q, want %q", name, tt.wantHeldBy)
			}
			if names := optionNames(got); !slices.Equal(names, tt.wantOptions) {
				t.Fatalf("options = %q, want %q", names, tt.wantOptions)
			}
		})
	}
}

func TestQueueBypassOptionsEchoEachOfferVerbatim(t *testing.T) {
	// Arrange.
	offers := []*frontendv1.TopbarQueueBypassContext{existingPlus, fullMain}

	// Act.
	options := queueBypassOptions(offers)

	// Assert.
	if len(options) != len(offers) {
		t.Fatalf("options = %v, want one row per offer", options)
	}
	for i, option := range options {
		if !proto.Equal(option.GetContext(), offers[i]) {
			t.Fatalf("row %d context = %v, want %v", i, option.GetContext(), offers[i])
		}
	}
}

func TestQueueBypassLabelRefusesAnOfferNamingNoArm(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("queueBypassLabel did not refuse an offer naming no arm")
		}
	}()

	// Act.
	queueBypassLabel(&frontendv1.TopbarQueueBypassContext{})
}
