package childgroup

import (
	"os"
	"testing"
)

func TestStartStamp(t *testing.T) {
	tests := []struct {
		name      string
		pid       int
		wantFound bool
	}{
		{"this process has a stamp", os.Getpid(), true},
		{"a pid no process holds has none", 0x7ffffff0, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			stamp, found, err := StartStamp(tt.pid)

			// Assert
			if err != nil {
				t.Fatalf("StartStamp(%d) = error %v", tt.pid, err)
			}
			if found != tt.wantFound || (found && stamp <= 0) {
				t.Fatalf("StartStamp(%d) = %d, %v, want found %v and a positive stamp", tt.pid, stamp, found, tt.wantFound)
			}
		})
	}
}
