package cpugov

import (
	"encoding/binary"
	"testing"
)

func loadavgBytes(load1 uint32, scale int64, size int) []byte {
	raw := make([]byte, size)
	if size >= 4 {
		binary.LittleEndian.PutUint32(raw[0:4], load1)
	}
	if size >= 24 {
		binary.LittleEndian.PutUint64(raw[16:24], uint64(scale))
	}
	return raw
}

func TestParseLoadavg(t *testing.T) {
	tests := []struct {
		name    string
		raw     []byte
		want    float64
		wantErr bool
	}{
		{name: "the one-minute average over its scale", raw: loadavgBytes(5*2048, 2048, 24), want: 5},
		{name: "a value of another size", raw: loadavgBytes(1, 2048, 16), wantErr: true},
		{name: "no scale", raw: loadavgBytes(1, 0, 24), wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := parseLoadavg(tt.raw)

			// Assert.
			if (err != nil) != tt.wantErr {
				t.Fatalf("parseLoadavg error = %v, want error %v", err, tt.wantErr)
			}
			if got != tt.want {
				t.Fatalf("parseLoadavg = %v, want %v", got, tt.want)
			}
		})
	}
}
