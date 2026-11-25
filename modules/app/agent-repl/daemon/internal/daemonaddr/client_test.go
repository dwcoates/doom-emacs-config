package daemonaddr

import (
	"testing"
)

func TestClientAddressNamesTheSharedAddressWheneverOneIsShared(t *testing.T) {
	tests := []struct {
		name    string
		shared  string
		private string
		want    string
	}{
		{name: "a shared address is told in place of the private one", shared: "127.0.0.1:1", private: "127.0.0.1:2", want: "127.0.0.1:1"},
		{name: "with no shared address the private one is told", private: "127.0.0.1:2", want: "127.0.0.1:2"},
		{name: "no daemon is told as none, even with a shared address", shared: "127.0.0.1:1", want: ""},
		{name: "no daemon and no shared address is told as none", want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := ClientAddress(tt.shared, tt.private)

			// Assert
			if got != tt.want {
				t.Fatalf("ClientAddress(%q, %q) = %q, want %q", tt.shared, tt.private, got, tt.want)
			}
		})
	}
}
