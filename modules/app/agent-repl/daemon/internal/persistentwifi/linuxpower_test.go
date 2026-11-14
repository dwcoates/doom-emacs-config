package persistentwifi

import (
	"context"
	"testing"

	"github.com/godbus/dbus/v5"
)

func propsSignal(path dbus.ObjectPath, iface string, changed map[string]dbus.Variant, invalidated []string) *dbus.Signal {
	return &dbus.Signal{Path: path, Name: propsInterface + "." + propsChanged, Body: []any{iface, changed, invalidated}}
}

func TestInhibitorChange(t *testing.T) {
	tests := []struct {
		name string
		sig  *dbus.Signal
		want bool
	}{
		{name: "BlockInhibited changed", sig: propsSignal(logindPath, logindInterface, map[string]dbus.Variant{"BlockInhibited": dbus.MakeVariant("sleep")}, nil), want: true},
		{name: "DelayInhibited invalidated", sig: propsSignal(logindPath, logindInterface, map[string]dbus.Variant{}, []string{"DelayInhibited"}), want: true},
		{name: "another logind property", sig: propsSignal(logindPath, logindInterface, map[string]dbus.Variant{"IdleHint": dbus.MakeVariant(true)}, nil)},
		{name: "another interface's properties", sig: propsSignal(logindPath, "org.freedesktop.login1.Session", map[string]dbus.Variant{"BlockInhibited": dbus.MakeVariant("")}, nil)},
		{name: "another object", sig: propsSignal("/org/freedesktop/login1/session/1", logindInterface, map[string]dbus.Variant{"BlockInhibited": dbus.MakeVariant("")}, nil)},
		{name: "a malformed body", sig: &dbus.Signal{Path: logindPath, Name: propsInterface + "." + propsChanged, Body: []any{logindInterface}}},
		{name: "no signal", sig: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := inhibitorChange(tt.sig)

			// Assert
			if got != tt.want {
				t.Fatalf("inhibitorChange() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestFollowInhibitorChangesReportsChangesThenAClosedBus(t *testing.T) {
	// Arrange
	signals := make(chan *dbus.Signal, 4)
	out := make(chan error, 1)
	signals <- propsSignal(logindPath, logindInterface, map[string]dbus.Variant{"IdleHint": dbus.MakeVariant(true)}, nil)
	signals <- propsSignal(logindPath, logindInterface, map[string]dbus.Variant{"BlockInhibited": dbus.MakeVariant("sleep")}, nil)
	close(signals)
	done := make(chan struct{})

	// Act
	// inflight: joined: the test waits on done.
	go func() { defer close(done); followInhibitorChanges(context.Background(), signals, out) }()
	var changes int
	var last error
	for last == nil {
		if last = <-out; last == nil {
			changes++
		}
	}
	<-done

	// Assert
	if changes != 1 {
		t.Fatalf("changes = %d before %v, want the one inhibitor change, the other property ignored", changes, last)
	}
}

func TestNotifyChangeCoalescesAChangeNotYetTaken(t *testing.T) {
	// Arrange
	out := make(chan error, 1)

	// Act
	notifyChange(out)
	notifyChange(out)

	// Assert
	if len(out) != 1 || <-out != nil {
		t.Fatal("two changes not yet taken were not one report")
	}
}

func TestPowerChangesHearsLogindOnLinux(t *testing.T) {
	// Act
	cfg := Config{Platform: "linux"}
	src := cfg.PowerChanges()

	// Assert
	if src == nil || cfg.PowerChangesSource() == PowerPrefsDir {
		t.Fatal("Linux's power changes are not heard from logind")
	}
}
