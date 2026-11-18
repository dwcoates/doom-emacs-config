package gnsurl

import "testing"

func TestIsHost(t *testing.T) {
	tests := []struct {
		name string
		host string
		want bool
	}{
		{name: "the deployed GNS host", host: "gns.int.sgns.chess-platform.com", want: true},
		{name: "a host in upper case", host: "GNS.EXAMPLE.COM", want: true},
		{name: "a host naming gns past its first label", host: "knowledge.gns.example.com", want: true},
		{name: "a host with no gns in it", host: "github.com", want: false},
		{name: "an empty host", host: "", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			got := IsHost(tt.host)

			// Assert
			if got != tt.want {
				t.Errorf("IsHost(%q) = %v, want %v", tt.host, got, tt.want)
			}
		})
	}
}
