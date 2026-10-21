package merge

import (
	"context"
	"errors"
	"testing"
	"time"
)

// scriptedSwitch is an operator switch whose state, failures and applied
// flips the test holds.
type scriptedSwitch struct {
	on       bool
	since    time.Time
	readErr  error
	writeErr error
	writes   int
	applied  []bool
}

func (s *scriptedSwitch) operatorSwitch() operatorSwitch {
	return operatorSwitch{
		read: func(context.Context) (bool, time.Time, error) { return s.on, s.since, s.readErr },
		write: func(_ context.Context, on bool, at time.Time) error {
			s.writes++
			if s.writeErr != nil {
				return s.writeErr
			}
			s.on, s.since = on, at
			return nil
		},
		apply:      func(on bool) { s.applied = append(s.applied, on) },
		sinceField: "on_since",
		on: switchSide{op: "test.on", refusedArm: "already_on", refusedReason: "already on",
			refusedMessage: "refused a flip on", askedBy: "test.on_arm", message: "turned on"},
		off: switchSide{op: "test.off", refusedArm: "not_on", refusedReason: "not on",
			refusedMessage: "refused a flip off", askedBy: "test.off_arm", message: "turned off"},
		inheritedMessage: "inherited on",
	}
}

func TestFlipTurnsASwitchAndAppliesIt(t *testing.T) {
	since := time.Unix(7, 0)
	tests := []struct {
		name      string
		from      bool
		to        bool
		wantOp    string
		wantSince bool
	}{
		{name: "on", from: false, to: true, wantOp: "test.on"},
		{name: "off", from: true, to: false, wantOp: "test.off", wantSince: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			sw := &scriptedSwitch{on: tt.from}
			if tt.from {
				sw.since = since
			}

			// Act
			err := h.o.flip(context.Background(), sw.operatorSwitch(), tt.to)

			// Assert
			if err != nil {
				t.Fatalf("flip: %v", err)
			}
			if sw.on != tt.to || len(sw.applied) != 1 || sw.applied[0] != tt.to {
				t.Fatalf("switch on = %v applied = %v, want %v applied once", sw.on, sw.applied, tt.to)
			}
			rec, ok := h.recordFor("info", tt.wantOp)
			if !ok || rec.Context["asked_by"] != tt.wantOp+"_arm" {
				t.Fatalf("INFO %s record = %v (%v), want one naming its asking arm", tt.wantOp, rec.Context, ok)
			}
			if _, has := rec.Context["on_since"]; has != tt.wantSince {
				t.Fatalf("INFO record carries on_since = %v, want %v", has, tt.wantSince)
			}
		})
	}
}

func TestFlipRefusesAFlipThatChangesNothing(t *testing.T) {
	tests := []struct {
		name    string
		on      bool
		wantArm string
		wantOp  string
	}{
		{name: "on while on", on: true, wantArm: "already_on", wantOp: "test.on"},
		{name: "off while off", on: false, wantArm: "not_on", wantOp: "test.off"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			sw := &scriptedSwitch{on: tt.on}

			// Act
			err := h.o.flip(context.Background(), sw.operatorSwitch(), tt.on)

			// Assert
			var refused *RefusalError
			if !errors.As(err, &refused) || refused.Arm != tt.wantArm {
				t.Fatalf("error = %v, want the %s refusal", err, tt.wantArm)
			}
			if sw.writes != 0 || len(sw.applied) != 0 {
				t.Fatalf("writes = %d applied = %v, want nothing written or applied", sw.writes, sw.applied)
			}
			if rec, ok := h.recordFor("warn", tt.wantOp); !ok || rec.Context["arm"] != tt.wantArm {
				t.Fatalf("WARN %s record = %v (%v), want one naming the arm", tt.wantOp, rec.Context, ok)
			}
		})
	}
}

func TestFlipPassesAFailureThroughAndAppliesNothing(t *testing.T) {
	tests := []struct {
		name      string
		readErr   error
		writeErr  error
		wantWrite int
	}{
		{name: "an unreadable switch", readErr: errors.New("state database unreadable"), wantWrite: 0},
		{name: "an unwritable switch", writeErr: errors.New("state database read-only"), wantWrite: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			sw := &scriptedSwitch{readErr: tt.readErr, writeErr: tt.writeErr}
			want := errors.Join(tt.readErr, tt.writeErr)

			// Act
			err := h.o.flip(context.Background(), sw.operatorSwitch(), true)

			// Assert
			if err == nil || (tt.readErr != nil && !errors.Is(err, tt.readErr)) || (tt.writeErr != nil && !errors.Is(err, tt.writeErr)) {
				t.Fatalf("error = %v, want %v", err, want)
			}
			if sw.writes != tt.wantWrite || len(sw.applied) != 0 {
				t.Fatalf("writes = %d applied = %v, want %d writes and nothing applied", sw.writes, sw.applied, tt.wantWrite)
			}
			if _, ok := h.recordFor("info", "test.on"); ok {
				t.Fatal("a failed flip was recorded as applied")
			}
		})
	}
}

// everyOperatorSwitch is every operator switch the orchestrator exposes, with
// its Orchestrator verbs. A new switch joins this list.
var everyOperatorSwitch = []struct {
	name string
	sw   func(*orchestrator) operatorSwitch
	on   func(Orchestrator) error
	off  func(Orchestrator) error
}{
	{
		name: "the racing switch", sw: (*orchestrator).racingSwitch,
		on:  func(o Orchestrator) error { return o.DisableRacing(context.Background()) },
		off: func(o Orchestrator) error { return o.EnableRacing(context.Background()) },
	},
}

// TestEveryOperatorSwitchVerbFlipsItsSwitch pins that each Orchestrator verb
// is its switch's flip: the switch reads on after the on verb and off after
// the off verb, and each verb refuses with its switch's own arm, so a verb
// hand-rolled beside flip fails here.
func TestEveryOperatorSwitchVerbFlipsItsSwitch(t *testing.T) {
	for _, tt := range everyOperatorSwitch {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			sw := tt.sw(h.o)

			// Act
			errOn := tt.on(h.o)
			onAfterOn, _, _ := sw.read(context.Background())
			errOnAgain := tt.on(h.o)
			errOff := tt.off(h.o)
			onAfterOff, _, _ := sw.read(context.Background())
			errOffAgain := tt.off(h.o)

			// Assert
			if errOn != nil || errOff != nil || !onAfterOn || onAfterOff {
				t.Fatalf("on = (%v, %v) off = (%v, %v), want on then off", errOn, onAfterOn, errOff, onAfterOff)
			}
			var refused *RefusalError
			if !errors.As(errOnAgain, &refused) || refused.Arm != sw.on.refusedArm {
				t.Fatalf("a second on = %v, want the %s refusal", errOnAgain, sw.on.refusedArm)
			}
			if !errors.As(errOffAgain, &refused) || refused.Arm != sw.off.refusedArm {
				t.Fatalf("a second off = %v, want the %s refusal", errOffAgain, sw.off.refusedArm)
			}
		})
	}
}

func TestRecordInheritedStatesASwitchInheritedOn(t *testing.T) {
	since := time.Unix(7, 0)
	tests := []struct {
		name       string
		on         bool
		wantRecord bool
	}{
		{name: "on is stated", on: true, wantRecord: true},
		{name: "off states nothing", on: false, wantRecord: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			sw := &scriptedSwitch{on: tt.on, since: since}

			// Act
			err := h.o.recordInherited(context.Background(), sw.operatorSwitch())

			// Assert
			if err != nil {
				t.Fatalf("recordInherited: %v", err)
			}
			rec, got := h.recordFor("info", "test.on")
			if got != tt.wantRecord {
				t.Fatalf("INFO test.on record = %v, want %v", got, tt.wantRecord)
			}
			if got && rec.Context["on_since"] != since {
				t.Fatalf("record context = %v, want on_since %v", rec.Context, since)
			}
		})
	}
}

func TestRecordInheritedPassesAnUnreadableSwitchThrough(t *testing.T) {
	// Arrange
	h := newHarness(t)
	want := errors.New("state database unreadable")
	sw := &scriptedSwitch{readErr: want}

	// Act
	err := h.o.recordInherited(context.Background(), sw.operatorSwitch())

	// Assert
	if !errors.Is(err, want) {
		t.Fatalf("recordInherited = %v, want the read's own failure", err)
	}
	if _, ok := h.recordFor("info", "test.on"); ok {
		t.Fatal("an unreadable switch was recorded as inherited")
	}
}
