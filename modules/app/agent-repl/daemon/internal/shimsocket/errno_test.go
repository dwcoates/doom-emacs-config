package shimsocket

import (
	"errors"
	"fmt"
	"net"
	"os"
	"syscall"
	"testing"
)

func TestGoneReadsEveryNoServerShapeAsGone(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want bool
	}{
		{name: "a refused dial", err: &net.OpError{Op: "dial", Net: "unix", Err: os.NewSyscallError("connect", syscall.ECONNREFUSED)}, want: true},
		{name: "an absent path", err: &net.OpError{Op: "dial", Net: "unix", Err: os.NewSyscallError("connect", syscall.ENOENT)}, want: true},
		{name: "os.ErrNotExist", err: fmt.Errorf("stat: %w", os.ErrNotExist), want: true},
		{name: "a peer lost before the credential read", err: fmt.Errorf("peer credential: %w", fmt.Errorf("getsockopt LOCAL_PEERPID: %w", syscall.ENOTCONN)), want: true},
		{name: "a lost peer through a net.OpError", err: &net.OpError{Op: "read", Net: "unix", Err: syscall.ENOTCONN}, want: true},
		{name: "a transport error wrapping a refused dial", err: fmt.Errorf("unavailable: %w", &net.OpError{Op: "dial", Net: "unix", Err: syscall.ECONNREFUSED}), want: true},
		{name: "a timeout", err: &net.OpError{Op: "dial", Net: "unix", Err: os.ErrDeadlineExceeded}, want: false},
		{name: "a permission refusal", err: &net.OpError{Op: "dial", Net: "unix", Err: syscall.EACCES}, want: false},
		{name: "an unrelated error", err: errors.New("boom"), want: false},
		{name: "no error", err: nil, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Gone(tt.err)

			// Assert.
			if got != tt.want {
				t.Fatalf("Gone(%v) = %v, want %v", tt.err, got, tt.want)
			}
		})
	}
}
