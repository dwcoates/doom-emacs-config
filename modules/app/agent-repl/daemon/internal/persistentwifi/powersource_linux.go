//go:build linux

package persistentwifi

import (
	"context"
	"fmt"

	"github.com/godbus/dbus/v5"
)

// SystemPowerSource hears UPower's OnBattery on the system bus (upower.go).
func SystemPowerSource() PowerSourceWatcher { return upowerSource{} }

type upowerSource struct{}

func (upowerSource) Watch(ctx context.Context, heard func(PowerSource)) error {
	conn, err := dbus.ConnectSystemBus()
	if err != nil {
		return fmt.Errorf("persistentwifi: reach the system bus to hear UPower: %w", err)
	}
	defer conn.Close()
	// SUBSCRIBE, THEN READ: a change between the two is heard as a change.
	if err := conn.AddMatchSignal(
		dbus.WithMatchObjectPath(upowerPath),
		dbus.WithMatchInterface(propsInterface),
		dbus.WithMatchMember(propsChanged),
		dbus.WithMatchArg(0, upowerInterface),
	); err != nil {
		return fmt.Errorf("persistentwifi: subscribe to UPower's PropertiesChanged: %w", err)
	}
	signals := make(chan *dbus.Signal, 16)
	conn.Signal(signals)
	return watchUPower(ctx, upowerConn{conn: conn, signals: signals}, heard)
}

// upowerConn is the real upowerBus.
type upowerConn struct {
	conn    *dbus.Conn
	signals chan *dbus.Signal
}

func (u upowerConn) OnBattery() (bool, error) {
	v, err := u.conn.Object(upowerName, upowerPath).GetProperty(upowerInterface + "." + upowerOnBattery)
	if err != nil {
		return false, err
	}
	b, ok := v.Value().(bool)
	if !ok {
		return false, fmt.Errorf("OnBattery is %T, not a bool", v.Value())
	}
	return b, nil
}

func (u upowerConn) Signals() <-chan *dbus.Signal { return u.signals }
