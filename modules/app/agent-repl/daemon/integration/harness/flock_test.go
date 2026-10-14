package harness

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestTakeExclusiveCallsOnBusyOnlyWhenTheLockIsHeld(t *testing.T) {
	tests := []struct {
		name     string
		heldBy   bool
		wantBusy bool
	}{
		{name: "a free lock", heldBy: false, wantBusy: false},
		{name: "a held lock", heldBy: true, wantBusy: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "x.lock")
			var holder *os.File
			if tt.heldBy {
				var err error
				if holder, err = os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o600); err != nil {
					t.Fatalf("open: %v", err)
				}
				if err := takeExclusive(holder, nil); err != nil {
					t.Fatalf("hold: %v", err)
				}
			}
			f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o600)
			if err != nil {
				t.Fatalf("open: %v", err)
			}
			defer f.Close()
			busy := make(chan struct{})
			done := make(chan error, 1)

			// Act
			go func() { done <- takeExclusive(f, func() { close(busy) }) }()
			if tt.heldBy {
				<-busy
				holder.Close()
			}

			// Assert
			if err := <-done; err != nil {
				t.Fatalf("takeExclusive: %v", err)
			}
			select {
			case <-busy:
				if !tt.wantBusy {
					t.Fatal("onBusy ran for a free lock")
				}
			default:
				if tt.wantBusy {
					t.Fatal("onBusy never ran for a held lock")
				}
			}
		})
	}
}

// TestEveryBlockingHarnessLockGoesThroughTakeExclusive pins the call sites:
// a blocking LOCK_EX written by hand outside takeExclusive is a drifted copy.
func TestEveryBlockingHarnessLockGoesThroughTakeExclusive(t *testing.T) {
	for _, file := range []string{"runroot.go", "flock.go"} {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		for i, line := range strings.Split(string(body), "\n") {
			if strings.Contains(line, "syscall.LOCK_EX)") && !strings.Contains(line, "return syscall.Flock(int(f.Fd()), syscall.LOCK_EX)") {
				t.Fatalf("%s:%d takes a blocking flock by hand: %s", file, i+1, strings.TrimSpace(line))
			}
		}
	}
}
