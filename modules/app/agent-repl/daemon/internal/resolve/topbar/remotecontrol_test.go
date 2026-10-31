package topbar

import (
	"strings"
	"testing"
)

func TestRemoteControlChipProjectsTheMode(t *testing.T) {
	tests := []struct {
		name        string
		on          bool
		wantTooltip string
	}{
		{"on", true, remoteControlOnTooltip},
		{"off", false, remoteControlOffTooltip},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			chip := remoteControlChip(tt.on)

			// Assert.
			if (chip.GetOn() != nil) != tt.on || (chip.GetOff() != nil) == tt.on {
				t.Fatalf("chip = %v, want the %s arm alone", chip, tt.name)
			}
			if chip.GetTooltip().GetText() != tt.wantTooltip {
				t.Fatalf("tooltip = %q, want %q", chip.GetTooltip().GetText(), tt.wantTooltip)
			}
		})
	}
}

func TestEachRemoteControlTooltipSaysWhatAClickDoes(t *testing.T) {
	tests := []struct {
		name    string
		tooltip string
		click   string
	}{
		{"on", remoteControlOnTooltip, "click to turn off"},
		{"off", remoteControlOffTooltip, "click to make this session reachable from claude.ai"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table. Act, Assert.
			if !strings.Contains(tt.tooltip, tt.click) {
				t.Fatalf("tooltip %q does not say %q", tt.tooltip, tt.click)
			}
		})
	}
}

func TestTheChipDrawsTheModeTheNamingStated(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.r.SetNaming(testWS, Naming{Name: "main (doom)", ConfigDir: "/Users/dev/.claude", RemoteControl: true})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Assert.
	if chip := h.view(t).GetRemoteControl(); chip.GetOn() == nil {
		t.Fatalf("chip = %v, want on", chip)
	}
}

func TestTheChipIsDrawnWithNoSession(t *testing.T) {
	// Arrange: the workspace facts alone, no session started.
	h := newHarness(t)

	// Act.
	h.r.SetNaming(testWS, Naming{Name: "topic", ConfigDir: "/Users/dev/.claude"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Assert.
	if chip := h.view(t).GetRemoteControl(); chip.GetOff() == nil {
		t.Fatalf("chip = %v, want off on a strip with no session", chip)
	}
}

func TestAModeChangeIsPublished(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.SetRemoteControl(testWS, true)

	// Assert.
	if chip := h.view(t).GetRemoteControl(); chip.GetOn() == nil {
		t.Fatalf("chip = %v, want on after the change", chip)
	}
}

func TestAModeChangeKeepsTheName(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.SetRemoteControl(testWS, true)

	// Assert.
	if got := h.view(t).GetWorkspaceName().GetText(); got != "fix-flaky-reconnect" {
		t.Fatalf("name = %q, want the naming's name kept", got)
	}
}

func TestARenameKeepsTheMode(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetRemoteControl(testWS, true)

	// Act.
	h.r.SetName(testWS, "renamed")

	// Assert.
	if chip := h.view(t).GetRemoteControl(); chip.GetOn() == nil {
		t.Fatalf("chip = %v, want the mode kept across a rename", chip)
	}
}
