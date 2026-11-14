package persistentwifi

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"

	"github.com/godbus/dbus/v5"
)

func upowerSignal(path dbus.ObjectPath, iface string, changed map[string]dbus.Variant, invalidated []string) *dbus.Signal {
	return &dbus.Signal{Path: path, Name: propsInterface + "." + propsChanged, Body: []any{iface, changed, invalidated}}
}

func TestOnBatteryChange(t *testing.T) {
	tests := []struct {
		name                         string
		sig                          *dbus.Signal
		wantBattery, wantInvalid, ok bool
	}{
		{name: "onto battery", sig: upowerSignal(upowerPath, upowerInterface, map[string]dbus.Variant{"OnBattery": dbus.MakeVariant(true)}, nil), wantBattery: true, ok: true},
		{name: "onto AC", sig: upowerSignal(upowerPath, upowerInterface, map[string]dbus.Variant{"OnBattery": dbus.MakeVariant(false)}, nil), ok: true},
		{name: "OnBattery invalidated", sig: upowerSignal(upowerPath, upowerInterface, map[string]dbus.Variant{}, []string{"OnBattery"}), wantInvalid: true, ok: true},
		{name: "another property", sig: upowerSignal(upowerPath, upowerInterface, map[string]dbus.Variant{"LidIsClosed": dbus.MakeVariant(true)}, nil)},
		{name: "another object", sig: upowerSignal("/org/freedesktop/UPower/devices/battery_BAT0", upowerInterface, map[string]dbus.Variant{"OnBattery": dbus.MakeVariant(true)}, nil)},
		{name: "a value that is no bool", sig: upowerSignal(upowerPath, upowerInterface, map[string]dbus.Variant{"OnBattery": dbus.MakeVariant("yes")}, nil)},
		{name: "no signal", sig: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			battery, invalid, ok := onBatteryChange(tt.sig)

			// Assert
			if battery != tt.wantBattery || invalid != tt.wantInvalid || ok != tt.ok {
				t.Fatalf("onBatteryChange() = (%v, %v, %v), want (%v, %v, %v)", battery, invalid, ok, tt.wantBattery, tt.wantInvalid, tt.ok)
			}
		})
	}
}

// fakeUPower answers OnBattery from a script and hands out scripted signals.
type fakeUPower struct {
	reads   []bool
	readErr error
	signals chan *dbus.Signal
}

func (f *fakeUPower) OnBattery() (bool, error) {
	if f.readErr != nil {
		return false, f.readErr
	}
	b := f.reads[0]
	if len(f.reads) > 1 {
		f.reads = f.reads[1:]
	}
	return b, nil
}

func (f *fakeUPower) Signals() <-chan *dbus.Signal { return f.signals }

func TestWatchUPowerHearsTheStateThenEveryChange(t *testing.T) {
	// Arrange
	bus := &fakeUPower{reads: []bool{false, true}, signals: make(chan *dbus.Signal, 4)}
	bus.signals <- upowerSignal(upowerPath, upowerInterface, map[string]dbus.Variant{"OnBattery": dbus.MakeVariant(true)}, nil)
	bus.signals <- upowerSignal(upowerPath, upowerInterface, map[string]dbus.Variant{"LidIsClosed": dbus.MakeVariant(true)}, nil)
	bus.signals <- upowerSignal(upowerPath, upowerInterface, map[string]dbus.Variant{}, []string{"OnBattery"})
	close(bus.signals)
	var heard []PowerSource

	// Act
	err := watchUPower(context.Background(), bus, func(p PowerSource) { heard = append(heard, p) })

	// Assert: AC read, battery changed, the invalidation read again.
	if !reflect.DeepEqual(heard, []PowerSource{PowerAC, PowerBattery, PowerBattery}) {
		t.Fatalf("heard %v, want AC, battery, battery", heard)
	}
	if err == nil || !strings.Contains(err.Error(), "connection hearing UPower closed") {
		t.Fatalf("watchUPower() = %v, want the closed bus reported", err)
	}
}

func TestWatchUPowerReportsAnUnreadableState(t *testing.T) {
	// Arrange
	bus := &fakeUPower{readErr: errors.New("UPower is not running"), signals: make(chan *dbus.Signal)}

	// Act
	err := watchUPower(context.Background(), bus, func(PowerSource) { t.Fatal("heard a power source never read") })

	// Assert
	if err == nil || !strings.Contains(err.Error(), "UPower is not running") {
		t.Fatalf("watchUPower() = %v, want the read's failure", err)
	}
}

func TestWatchUPowerEndsQuietlyWithItsContext(t *testing.T) {
	// Arrange
	bus := &fakeUPower{reads: []bool{false}, signals: make(chan *dbus.Signal)}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	err := watchUPower(ctx, bus, func(PowerSource) {})

	// Assert
	if err != nil {
		t.Fatalf("watchUPower() = %v, want nil once its context ends", err)
	}
}
