package persistentwifi

import (
	"context"
	"errors"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

func TestDisplayMoveFor(t *testing.T) {
	cases := []struct {
		name   string
		lid    lidKind
		mode   modeKnown
		dimmed bool
		want   displayMove
	}{
		{name: "a closed lid with the mode on dims", lid: lidClosed, mode: modeIsOn, want: moveDim},
		{name: "a closed lid with a dim in force stays dimmed", lid: lidClosed, mode: modeIsOn, dimmed: true, want: moveNone},
		{name: "a closed lid with the mode off is left alone", lid: lidClosed, mode: modeIsOff, want: moveNone},
		{name: "a closed lid with the mode unknown is left alone", lid: lidClosed, mode: modeUnknown, want: moveNone},
		{name: "an open lid with the mode on is left alone", lid: lidOpen, mode: modeIsOn, want: moveNone},
		{name: "an open lid with a dim in force restores", lid: lidOpen, mode: modeIsOn, dimmed: true, want: moveRestore},
		{name: "an open lid restores whatever the mode", lid: lidOpen, mode: modeUnknown, dimmed: true, want: moveRestore},
		{name: "the mode off with a dim in force restores under a closed lid", lid: lidClosed, mode: modeIsOff, dimmed: true, want: moveRestore},
		{name: "no lid with a dim in force restores", lid: lidNone, mode: modeIsOn, dimmed: true, want: moveRestore},
		{name: "an unwatched lid with the mode on is left alone", lid: lidUnwatched, mode: modeIsOn, want: moveNone},
		{name: "an unheard lid with a dim in force and the mode on is left alone", lid: lidUnheard, mode: modeIsOn, dimmed: true, want: moveNone},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := displayMoveFor(tc.lid, tc.mode, tc.dimmed)

			// Assert.
			if got != tc.want {
				t.Fatalf("displayMoveFor() = %s, want %s", got, tc.want)
			}
		})
	}
}

func TestParseBrightness(t *testing.T) {
	cases := []struct {
		out     string
		want    float64
		wantErr bool
	}{
		{out: "0.81\n", want: 0.81},
		{out: "0", want: 0},
		{out: "1", want: 1},
		{out: "1.5", wantErr: true},
		{out: "-0.1", wantErr: true},
		{out: "NaN", wantErr: true},
		{out: "usage: mac-brightness", wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.out, func(t *testing.T) {
			// Act.
			got, err := parseBrightness(tc.out)

			// Assert.
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("parseBrightness(%q) = (%v, %v), want (%v, error %v)", tc.out, got, err, tc.want, tc.wantErr)
			}
		})
	}
}

func TestFormatLevelIsTheShortestDecimal(t *testing.T) {
	cases := map[float64]string{0: "0", 0.81: "0.81", 1: "1"}
	for level, want := range cases {
		t.Run(want, func(t *testing.T) {
			// Act, Assert.
			if got := formatLevel(level); got != want {
				t.Fatalf("formatLevel(%v) = %q, want %q", level, got, want)
			}
		})
	}
}

// keep stands a kept pre-close level in the store.
func (h *harness) keep(level float64) *harness {
	h.dims.level = &level
	return h
}

func TestUpdateMovesTheDisplayByTheLid(t *testing.T) {
	cases := []struct {
		name      string
		action    string
		pmset     string
		lid       lidKind
		kept      *float64
		want      func(*agentreplv1.UpdatePersistentWifiModeDisplay) bool
		wantCalls []string
		wantKept  *float64
	}{
		{name: "turning on with the lid open leaves the brightness alone", action: "on", pmset: pmsetOff, lid: lidOpen,
			want: func(d *agentreplv1.UpdatePersistentWifiModeDisplay) bool { return d.GetUnchanged() != nil }},
		{name: "turning on with the lid closed keeps the level and dims", action: "on", pmset: pmsetOff, lid: lidClosed,
			want:      func(d *agentreplv1.UpdatePersistentWifiModeDisplay) bool { return d.GetDimmed().GetRestoreLevel() == 0.81 },
			wantCalls: []string{argBrightnessRead, argLidDim}, wantKept: ptr(0.81)},
		{name: "turning off while dimmed restores the kept level", action: "off", pmset: pmsetOn, lid: lidClosed, kept: ptr(0.81),
			want:      func(d *agentreplv1.UpdatePersistentWifiModeDisplay) bool { return d.GetRestored().GetLevel() == 0.81 },
			wantCalls: []string{argBrightness("0.81")}},
		{name: "turning off with the lid open and no dim leaves the brightness alone", action: "off", pmset: pmsetOn, lid: lidOpen,
			want: func(d *agentreplv1.UpdatePersistentWifiModeDisplay) bool { return d.GetUnchanged() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			on := tc.action == "on"
			// Turning on finds the hotspot already joined and turning off finds
			// another network, so neither runs a hotspot step's tools.
			link := summary(true, hotspot)
			if !on {
				link = summary(true, "Home")
			}
			h := newHarness(t).standing(tc.pmset, link).withLid(tc.lid)
			h.dims.level = tc.kept
			value := "0"
			if on {
				value = "1"
			}
			scriptPower(h.r, value)
			h.r.on(argBrightnessRead, answer{out: "0.81\n"})
			h.r.on(argLidDim, answer{})
			h.r.on(argBrightness("0.81"), answer{})

			// Act.
			resp := h.c.Update(context.Background(), update(tc.action))

			// Assert.
			display := resp.GetSuccess().GetDisplay()
			if !tc.want(display) {
				t.Fatalf("display = %v", display)
			}
			var calls []string
			for _, c := range h.r.ran() {
				if strings.HasPrefix(c, tools.Brightness) {
					calls = append(calls, c)
				}
			}
			if strings.Join(calls, "|") != strings.Join(tc.wantCalls, "|") {
				t.Fatalf("brightness calls = %q, want %q", calls, tc.wantCalls)
			}
			if got := h.dims.kept(); (got == nil) != (tc.wantKept == nil) || (got != nil && *got != *tc.wantKept) {
				t.Fatalf("kept level = %v, want %v", got, tc.wantKept)
			}
			if got := resp.GetSuccess().GetState().GetLidDim(); (got == nil) != (tc.wantKept == nil) {
				t.Fatalf("standing lid_dim = %v, want set=%v", got, tc.wantKept != nil)
			}
			if errs := h.records(dlog.LevelError, opDisplay); len(errs) != 0 {
				t.Fatalf("display ERROR records = %+v, want none", errs)
			}
		})
	}
}

func ptr(f float64) *float64 { return &f }

func TestMoveDisplayFailuresAreRecordedAndAnsweredFailed(t *testing.T) {
	cases := []struct {
		name       string
		lid        lidKind
		mode       modeKnown
		arrange    func(*harness)
		wantDetail string
		wantKept   *float64
		wantDim    bool
	}{
		{name: "an unreadable kept level moves nothing", lid: lidOpen, mode: modeIsOn,
			arrange:    func(h *harness) { h.dims.getErr = errors.New("database is locked") },
			wantDetail: "the kept pre-close brightness could not be read"},
		{name: "a level that cannot be kept is not dimmed", lid: lidClosed, mode: modeIsOn,
			arrange: func(h *harness) {
				h.r.on(argBrightnessRead, answer{out: "0.5"})
				h.dims.putErr = errors.New("disk full")
			},
			wantDetail: "could not be kept"},
		{name: "a dim the tool refuses forgets the kept level", lid: lidClosed, mode: modeIsOn,
			arrange: func(h *harness) {
				h.r.on(argBrightnessRead, answer{out: "0.5"})
				h.r.on(argLidDim, answer{out: "no display", code: 1})
			},
			wantDetail: "could not be dimmed"},
		{name: "a dim the tool refuses whose level cannot be forgotten stays in force", lid: lidClosed, mode: modeIsOn,
			arrange: func(h *harness) {
				h.r.on(argBrightnessRead, answer{out: "0.5"})
				h.r.on(argLidDim, answer{out: "no display", code: 1})
				h.dims.clearErr = errors.New("database is locked")
			},
			wantDetail: "could not be dimmed", wantKept: ptr(0.5), wantDim: true},
		{name: "a restore the tool refuses keeps the level", lid: lidOpen, mode: modeIsOn,
			arrange: func(h *harness) {
				h.keep(0.5)
				h.r.on(argBrightness("0.5"), answer{out: "no display", code: 1})
			},
			wantDetail: "could not be restored", wantKept: ptr(0.5), wantDim: true},
		{name: "a restore whose level cannot be cleared still ends the dim", lid: lidOpen, mode: modeIsOn,
			arrange: func(h *harness) {
				h.keep(0.5)
				h.r.on(argBrightness("0.5"), answer{})
				h.dims.clearErr = errors.New("database is locked")
			},
			wantDetail: "restored to 0.5, but the kept level could not be cleared", wantKept: ptr(0.5)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t).withLid(tc.lid)
			tc.arrange(h)

			// Act.
			var d *agentreplv1.UpdatePersistentWifiModeDisplay
			func() {
				h.c.mu.Lock()
				defer h.c.mu.Unlock()
				d = h.c.moveDisplayLocked(context.Background(), tc.mode, "test")
			}()

			// Assert.
			if !strings.Contains(d.GetFailed().GetDetail(), tc.wantDetail) {
				t.Fatalf("display = %v, want failed naming %q", d, tc.wantDetail)
			}
			errs := h.records(dlog.LevelError, opDisplay)
			if len(errs) != 1 || errs[0].Context["trigger"] != "test" || errs[0].Context["lid"] != h.c.lid.name() {
				t.Fatalf("display ERROR records = %+v, want one naming the trigger and the lid", errs)
			}
			if got := h.dims.kept(); (got == nil) != (tc.wantKept == nil) || (got != nil && *got != *tc.wantKept) {
				t.Fatalf("kept level = %v, want %v", got, tc.wantKept)
			}
			if (h.c.dim != nil) != tc.wantDim {
				t.Fatalf("dim in force = %v, want %v", h.c.dim, tc.wantDim)
			}
		})
	}
}
