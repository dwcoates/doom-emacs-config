package merge

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/cpugov"
	"agent-repld/internal/wsm"
)

func TestDisableRacingIsDurableAndNarrowsTheGovernor(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.o.DisableRacing(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("DisableRacing: %v", err)
	}
	if racing, _ := h.db.MergeRacing(context.Background()); !racing.Disabled {
		t.Fatal("racing is not durably disabled after DisableRacing")
	}
	if got := h.governor.Mode(); got != (cpugov.Mode{Power: cpugov.PowerAC, RacingDisabled: true}) {
		t.Fatalf("governor mode = %+v, want racing disabled on AC power", got)
	}
	if _, ok := h.recordFor("info", opDisableRacing); !ok {
		t.Fatalf("no INFO %s record for the disable", opDisableRacing)
	}
}

func TestEnableRacingIsDurableAndWidensTheGovernor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.racing = wsm.RacingSwitch{Disabled: true, Since: time.Unix(1, 0)}
	h.governor.SetRacingDisabled(true)

	// Act
	err := h.o.EnableRacing(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("EnableRacing: %v", err)
	}
	if racing, _ := h.db.MergeRacing(context.Background()); racing.Disabled {
		t.Fatal("racing is still durably disabled after EnableRacing")
	}
	if got := h.governor.Window(); got != cpugov.QueueWindow {
		t.Fatalf("governor window = %d, want the top two on AC power", got)
	}
	if rec, ok := h.recordFor("info", opEnableRacing); !ok || rec.Context["disabled_since"] != time.Unix(1, 0) {
		t.Fatalf("INFO %s record = %v (%v), want one naming when racing was disabled", opEnableRacing, rec.Context, ok)
	}
}

func TestARacingFlipThatCannotReadTheSwitchLeavesTheGovernorAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	want := errors.New("state database unreadable")
	h.db.racingErr = want

	// Act
	err := h.o.DisableRacing(context.Background())

	// Assert
	if !errors.Is(err, want) {
		t.Fatalf("DisableRacing = %v, want the read's own failure", err)
	}
	if h.governor.Mode().RacingDisabled {
		t.Fatal("the governor narrowed for a flip that never reached the state database")
	}
}

func TestRecoverStatesInheritedDisabledRacing(t *testing.T) {
	tests := []struct {
		name       string
		disabled   bool
		wantRecord bool
	}{
		{name: "disabled racing is stated at boot", disabled: true, wantRecord: true},
		{name: "enabled racing states nothing", disabled: false, wantRecord: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			if tt.disabled {
				h.db.racing = wsm.RacingSwitch{Disabled: true, Since: time.Unix(1, 0)}
			}

			// Act
			err := h.o.Recover(context.Background())

			// Assert
			if err != nil {
				t.Fatalf("Recover: %v", err)
			}
			if _, got := h.recordFor("info", opDisableRacing); got != tt.wantRecord {
				t.Fatalf("INFO %s record at boot = %v, want %v", opDisableRacing, got, tt.wantRecord)
			}
		})
	}
}

func TestRecoverPassesAnUnreadableRacingSwitchThrough(t *testing.T) {
	// Arrange
	h := newHarness(t)
	want := errors.New("state database unreadable")
	h.db.racingErr = want

	// Act
	err := h.o.Recover(context.Background())

	// Assert
	if !errors.Is(err, want) {
		t.Fatalf("Recover = %v, want the read's own failure", err)
	}
}
