package harness

import (
	"errors"
	"fmt"
	"io"
	"syscall"
	"testing"
)

func TestDaemonHungUp(t *testing.T) {
	cases := []struct {
		name string
		err  error
		want bool
	}{
		{"end of stream before the request", io.EOF, true},
		{"end of stream inside the request", io.ErrUnexpectedEOF, true},
		{"broken pipe answering", fmt.Errorf("write: %w", syscall.EPIPE), true},
		{"connection reset", fmt.Errorf("read: %w", syscall.ECONNRESET), true},
		{"any other failure", errors.New("bad frame"), false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := daemonHungUp(tc.err)

			// Assert
			if got != tc.want {
				t.Fatalf("daemonHungUp(%v) = %v, want %v", tc.err, got, tc.want)
			}
		})
	}
}
