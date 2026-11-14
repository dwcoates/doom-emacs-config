package persistentwifi

import (
	"context"
	"encoding/binary"
	"errors"
	"io"
	"reflect"
	"strings"
	"sync"
	"testing"
)

// A /proc/bus/input/devices excerpt: a keyboard, then the ACPI lid switch.
const inputDevicesFixture = `I: Bus=0011 Vendor=0001 Product=0001 Version=ab41
N: Name="AT Translated Set 2 keyboard"
H: Handlers=sysrq kbd event0 leds
B: EV=120013

I: Bus=0019 Vendor=0000 Product=0005 Version=0000
N: Name="Lid Switch"
H: Handlers=event3
B: EV=21
B: SW=1
`

func TestLidDeviceIn(t *testing.T) {
	tests := []struct {
		name    string
		devices string
		want    string
	}{
		{name: "the device whose switches name SW_LID", devices: inputDevicesFixture, want: "event3"},
		{name: "a machine with no lid switch", devices: strings.Split(inputDevicesFixture, "\n\n")[0], want: ""},
		{name: "a switch device without SW_LID (a tablet-mode switch)", devices: "N: Name=\"Tablet Mode\"\nH: Handlers=event9\nB: SW=2\n", want: ""},
		{name: "a multi-word bitmap reads SW_LID from its last word", devices: "H: Handlers=kbd event5\nB: SW=10 1\n", want: "event5"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := lidDeviceIn(tt.devices)

			// Assert
			if got != tt.want {
				t.Fatalf("lidDeviceIn() = %q, want %q", got, tt.want)
			}
		})
	}
}

// inputEvent encodes one 64-bit input_event.
func inputEvent(typ, code uint16, value int32) []byte {
	raw := make([]byte, inputEventSize)
	binary.LittleEndian.PutUint16(raw[16:], typ)
	binary.LittleEndian.PutUint16(raw[18:], code)
	binary.LittleEndian.PutUint32(raw[20:], uint32(value))
	return raw
}

func TestLidEvent(t *testing.T) {
	tests := []struct {
		name   string
		raw    []byte
		want   Lid
		wantOK bool
	}{
		{name: "the lid closing", raw: inputEvent(evSW, swLid, 1), want: LidClosed, wantOK: true},
		{name: "the lid opening", raw: inputEvent(evSW, swLid, 0), want: LidOpen, wantOK: true},
		{name: "another switch is no lid event", raw: inputEvent(evSW, 1, 1)},
		{name: "a sync report is no lid event", raw: inputEvent(0, 0, 0)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := lidEvent(tt.raw)

			// Assert
			if got != tt.want || ok != tt.wantOK {
				t.Fatalf("lidEvent() = (%v, %v), want (%v, %v)", got, ok, tt.want, tt.wantOK)
			}
		})
	}
}

func TestLidFromSwitches(t *testing.T) {
	if lidFromSwitches([]byte{1, 0}) != LidClosed || lidFromSwitches([]byte{2, 0}) != LidOpen || lidFromSwitches(nil) != LidOpen {
		t.Fatal("lidFromSwitches did not read SW_LID's bit")
	}
}

// eventPipe is an io.ReadCloser over a pipe, as a device fd is.
func eventPipe() (*io.PipeReader, *io.PipeWriter) { return io.Pipe() }

func TestWatchLidEventsHearsTheStateThenEveryLidEvent(t *testing.T) {
	// Arrange
	r, w := eventPipe()
	ctx, cancel := context.WithCancel(context.Background())
	var mu sync.Mutex
	var heard []Lid
	done := make(chan error, 1)
	got := make(chan struct{}, 8)

	// Act
	// inflight: joined: the test waits on done.
	go func() {
		done <- watchLidEvents(ctx, r, LidOpen, func(l Lid) {
			mu.Lock()
			heard = append(heard, l)
			mu.Unlock()
			got <- struct{}{}
		})
	}()
	<-got
	_, _ = w.Write(append(append(inputEvent(0, 0, 0), inputEvent(evSW, swLid, 1)...), inputEvent(evSW, swLid, 0)...))
	<-got
	<-got
	cancel()
	err := <-done

	// Assert
	mu.Lock()
	defer mu.Unlock()
	if err != nil || !reflect.DeepEqual(heard, []Lid{LidOpen, LidClosed, LidOpen}) {
		t.Fatalf("heard %v, err %v; want open (read), closed, open", heard, err)
	}
}

func TestWatchLidEventsReportsAnEndedDevice(t *testing.T) {
	// Arrange
	r, w := eventPipe()
	_ = w.Close()

	// Act
	err := watchLidEvents(context.Background(), r, LidOpen, func(Lid) {})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "event device ended") {
		t.Fatalf("watchLidEvents() = %v, want the device's end reported", err)
	}
}

func TestWatchLidEventsReportsAFailedRead(t *testing.T) {
	// Arrange
	r, w := eventPipe()
	_ = w.CloseWithError(errors.New("input/output error"))

	// Act
	err := watchLidEvents(context.Background(), r, LidOpen, func(Lid) {})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "input/output error") {
		t.Fatalf("watchLidEvents() = %v, want the read's failure", err)
	}
}
