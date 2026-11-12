package hostpower

import (
	"errors"
	"testing"
)

// fixed is a PowerSource that answers one reading.
type fixed struct {
	power Power
	err   error
}

func (f fixed) Power() (Power, error) { return f.power, f.err }

func TestOnBattery(t *testing.T) {
	unreadable := errors.New("no reader")
	cases := []struct {
		name    string
		source  fixed
		want    bool
		wantErr error
	}{
		{name: "a host on its battery", source: fixed{power: PowerBattery}, want: true},
		{name: "a host on AC power", source: fixed{power: PowerAC}, want: false},
		{name: "a source read as unknown", source: fixed{power: PowerUnknown}, want: false},
		{name: "a source that cannot be read answers its error", source: fixed{err: unreadable}, wantErr: unreadable},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := OnBattery(tc.source)

			// Assert
			if !errors.Is(err, tc.wantErr) || got != tc.want {
				t.Fatalf("OnBattery = %v, %v; want %v, %v", got, err, tc.want, tc.wantErr)
			}
		})
	}
}
