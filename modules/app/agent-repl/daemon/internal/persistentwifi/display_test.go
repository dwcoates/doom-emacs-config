package persistentwifi

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
)

func TestQuietWanted(t *testing.T) {
	tests := []struct {
		name     string
		lid      lidKind
		mode     modeKnown
		standing bool
		want     bool
	}{
		{name: "closed and on enters", lid: lidClosed, mode: modeIsOn, want: true},
		{name: "closed and off stays out", lid: lidClosed, mode: modeIsOff},
		{name: "closed and off leaves", lid: lidClosed, mode: modeIsOff, standing: true},
		{name: "closed with an unknown mode keeps a standing state", lid: lidClosed, mode: modeUnknown, standing: true, want: true},
		{name: "closed with an unknown mode enters nothing", lid: lidClosed, mode: modeUnknown},
		{name: "open and on leaves", lid: lidOpen, mode: modeIsOn, standing: true},
		{name: "no lid never enters", lid: lidNone, mode: modeIsOn},
		{name: "an unwatched lid leaves", lid: lidUnwatched, mode: modeIsOn, standing: true},
		{name: "an unheard lid leaves even with an unknown mode", lid: lidUnheard, mode: modeUnknown, standing: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := quietWanted(tt.lid, tt.mode, tt.standing)

			// Assert
			if got != tt.want {
				t.Fatalf("quietWanted(%v, %v, %v) = %v, want %v", tt.lid, tt.mode, tt.standing, got, tt.want)
			}
		})
	}
}

func TestFormatLevelIsTheShortestDecimal(t *testing.T) {
	tests := []struct {
		level float64
		want  string
	}{
		{level: 0, want: "0"},
		{level: 0.81, want: "0.81"},
		{level: 1, want: "1"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act and assert
			if got := formatLevel(tt.level); got != tt.want {
				t.Fatalf("formatLevel(%v) = %q, want %q", tt.level, got, tt.want)
			}
		})
	}
}

func TestUpdateMovesTheQuietStateByTheLid(t *testing.T) {
	tests := []struct {
		name        string
		lid         lidKind
		action      string
		quietBefore bool
		script      func(*harness)
		wantArm     string
		wantFreezer []string
		wantQuiet   bool
	}{
		{name: "on with the lid closed sleeps the display and freezes", lid: lidClosed, action: "on",
			script:  func(h *harness) { h.r.on(argDisplaySleep, answer{}) },
			wantArm: "slept", wantFreezer: []string{"freeze:update"}, wantQuiet: true},
		{name: "on with the lid open moves nothing", lid: lidOpen, action: "on",
			wantArm: "unchanged", wantFreezer: []string{"thaw:update"}},
		{name: "off with the quiet state standing wakes and thaws", lid: lidClosed, action: "off", quietBefore: true,
			script:  func(h *harness) { h.r.on(argDisplayWake, answer{}) },
			wantArm: "woke", wantFreezer: []string{"thaw:update"}},
		{name: "on with the display sleep failing answers failed and stays quiet", lid: lidClosed, action: "on",
			script:  func(h *harness) { h.r.on(argDisplaySleep, answer{out: "refused", code: 1}) },
			wantArm: "failed", wantFreezer: []string{"freeze:update"}, wantQuiet: true},
		{name: "off with the wake failing answers failed and leaves anyway", lid: lidClosed, action: "off", quietBefore: true,
			script:  func(h *harness) { h.r.on(argDisplayWake, answer{out: "refused", code: 1}) },
			wantArm: "failed", wantFreezer: []string{"thaw:update"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			before, after := pmsetOff, pmsetOn
			if tt.action == "off" {
				before, after = pmsetOn, pmsetOff
			}
			h := newHarness(t).withLid(tt.lid)
			h.r.on(argPmsetRead, answer{out: before}, answer{out: after})
			h.r.on(argPorts, answer{out: ports})
			if tt.action == "on" {
				h.r.on(argSummary, answer{out: summary(true, hotspot)})
			} else {
				h.r.on(argSummary, answer{out: summary(true, "Home")})
			}
			scriptPower(h.r, map[string]string{"on": "1", "off": "0"}[tt.action])
			if tt.quietBefore {
				h.c.quiet, h.c.displayAsleep = true, true
			}
			if tt.script != nil {
				tt.script(h)
			}

			// Act
			resp := h.c.Update(context.Background(), update(tt.action))

			// Assert
			display := resp.GetSuccess().GetDisplay()
			if display == nil || displayArm(display) != tt.wantArm {
				t.Fatalf("display = %v (calls %v), want %s", display, h.r.ran(), tt.wantArm)
			}
			if !slices.Equal(h.freezer.calls, tt.wantFreezer) {
				t.Fatalf("freezer calls %v, want %v", h.freezer.calls, tt.wantFreezer)
			}
			if got := resp.GetSuccess().GetState().GetQuiet() != nil; got != tt.wantQuiet {
				t.Fatalf("standing quiet = %v, want %v", got, tt.wantQuiet)
			}
			if tt.wantArm == "failed" && len(h.records(dlog.LevelError, opDisplay)) != 1 {
				t.Fatalf("ERROR records = %+v, want the one failed display step", h.records(dlog.LevelError, opDisplay))
			}
		})
	}
}

func TestRefreshUndoesAnEarlierBuildsLidDim(t *testing.T) {
	tests := []struct {
		name       string
		level      *float64
		getErr     error
		clearErr   error
		toolExists bool
		setFails   bool
		wantSet    bool
		wantKept   bool
		wantErrors int
		wantInfos  int
	}{
		{name: "no dim kept touches nothing", toolExists: true},
		{name: "a kept dim is set back and cleared", level: ptr(0.6), toolExists: true, wantSet: true, wantInfos: 1},
		{name: "an unreadable dim is recorded", getErr: errors.New("disk"), toolExists: true, wantErrors: 1},
		{name: "a missing tool keeps the dim", level: ptr(0.6), wantKept: true, wantErrors: 1},
		{name: "a failed set keeps the dim", level: ptr(0.6), toolExists: true, setFails: true, wantSet: true, wantKept: true, wantErrors: 1},
		{name: "a failed clear is recorded", level: ptr(0.6), toolExists: true, clearErr: errors.New("locked"), wantSet: true, wantKept: true, wantErrors: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t).standing(pmsetOff, summary(true, "Home"))
			h.dims.level, h.dims.getErr, h.dims.clearErr = tt.level, tt.getErr, tt.clearErr
			h.exists[tools.Brightness] = tt.toolExists
			if tt.setFails {
				h.r.on(argBrightness("0.6"), answer{out: "IOKit", code: 1})
			} else {
				h.r.on(argBrightness("0.6"), answer{})
			}

			// Act
			h.c.Refresh(context.Background())

			// Assert
			if got := h.r.count(argBrightness("0.6")) == 1; got != tt.wantSet {
				t.Fatalf("set the level = %v (calls %v), want %v", got, h.r.ran(), tt.wantSet)
			}
			if got := h.dims.kept() != nil; got != tt.wantKept {
				t.Fatalf("dim kept = %v, want %v", got, tt.wantKept)
			}
			if got := len(h.records(dlog.LevelError, opDisplay)); got != tt.wantErrors {
				t.Fatalf("ERROR records = %d, want %d", got, tt.wantErrors)
			}
			if got := len(h.records(dlog.LevelInfo, opDisplay)); got != tt.wantInfos {
				t.Fatalf("INFO records = %d, want %d", got, tt.wantInfos)
			}
		})
	}
}

func ptr(f float64) *float64 { return &f }
