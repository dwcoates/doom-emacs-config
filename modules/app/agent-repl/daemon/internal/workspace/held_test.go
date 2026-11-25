package workspace

import (
	"testing"
)

func TestLiveHeldIsWhetherItsClientIsReaped(t *testing.T) {
	tests := []struct {
		name   string
		reaped bool
		want   bool
	}{
		{name: "a live client is held", reaped: false, want: true},
		{name: "a reaped client is not held", reaped: true, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			entry := &live{client: &fakeClient{reaped: tt.reaped}}

			// Act.
			got := entry.held()

			// Assert.
			if got != tt.want {
				t.Fatalf("held() = %v, want %v", got, tt.want)
			}
		})
	}
}
