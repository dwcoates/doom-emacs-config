package proc

import "testing"

func TestParseLoadAvg(t *testing.T) {
	tests := []struct {
		in      string
		want    float64
		wantErr bool
	}{
		{in: "{ 3.12 4.56 5.67 }", want: 3.12},
		{in: "{ 168.46 158.25 119.26 }\n", want: 168.46},
		{in: "{ }", wantErr: true},
		{in: "{ x 1 2 }", wantErr: true},
	}
	for _, tt := range tests {
		// Act
		got, err := ParseLoadAvg(tt.in)

		// Assert
		if (err != nil) != tt.wantErr || (!tt.wantErr && got != tt.want) {
			t.Errorf("ParseLoadAvg(%q) = %v, %v", tt.in, got, err)
		}
	}
}
