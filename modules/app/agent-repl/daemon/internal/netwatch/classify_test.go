package netwatch

import (
	"net/netip"
	"testing"
)

func TestRoutable(t *testing.T) {
	tests := []struct {
		addr string
		want bool
	}{
		{"192.168.1.5", true},
		{"2601:640:8000::1", true},
		{"::ffff:10.0.0.2", true},
		{"127.0.0.1", false},
		{"::1", false},
		{"169.254.10.1", false},
		{"fe80::1", false},
		{"0.0.0.0", false},
		{"::", false},
		{"224.0.0.251", false},
	}
	for _, tt := range tests {
		t.Run(tt.addr, func(t *testing.T) {
			// Act.
			got := routable(netip.MustParseAddr(tt.addr))

			// Assert.
			if got != tt.want {
				t.Fatalf("routable(%s) = %v, want %v", tt.addr, got, tt.want)
			}
		})
	}
}

func TestRoutableRefusesTheZeroAddr(t *testing.T) {
	// Act, Assert.
	if routable(netip.Addr{}) {
		t.Fatal("routable(zero Addr) = true, want false")
	}
}

func TestInterfaceNameFallsBackToTheIndexOfAGoneInterface(t *testing.T) {
	// Act.
	got := interfaceName(1 << 30)

	// Assert.
	if got != "1073741824" {
		t.Fatalf("interfaceName(gone) = %q, want the index", got)
	}
}
