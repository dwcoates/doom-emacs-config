package grantedclock

import (
	"strings"
	"testing"
	"time"
)

func TestParseCPUPressureSomeReadsTheSomeTotal(t *testing.T) {
	tests := []struct {
		name    string
		body    string
		want    time.Duration
		wantErr string
	}{
		{name: "the some total is read in microseconds",
			body: "some avg10=14.31 avg60=2.58 avg300=0.54 total=2690646\nfull avg10=14.31 avg60=2.58 avg300=0.54 total=2690645\n",
			want: 2690646 * time.Microsecond},
		{name: "the full line is never read for it",
			body: "full avg10=0.00 avg60=0.00 avg300=0.00 total=9\nsome avg10=0.00 avg60=0.00 avg300=0.00 total=66\n",
			want: 66 * time.Microsecond},
		{name: "a file with no some line is refused", body: "full avg10=0.00 total=9\n",
			wantErr: "no some line"},
		{name: "a some line with no total is refused", body: "some avg10=0.00 avg60=0.00\n",
			wantErr: "carries no total"},
		{name: "a total that is not a number is refused", body: "some avg10=0.00 total=12x\n",
			wantErr: `the some line's total "12x"`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: tt.body.

			// Act
			got, err := parseCPUPressureSome(tt.body)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("parseCPUPressureSome() = %s, %v, want an error naming %q", got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("parseCPUPressureSome() = %s, %v, want %s", got, err, tt.want)
			}
		})
	}
}
