package workspace

import (
	"strings"
	"testing"

	"agent-repld/internal/sourcescan"
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

// TestEveryReapedCheckGoesThroughHeld pins that the fleet reads a client's
// reaping only through live.held, so a hand-rolled check cannot drift from it.
func TestEveryReapedCheckGoesThroughHeld(t *testing.T) {
	// Arrange, Act.
	var sites []string
	for _, file := range sourcescan.Production(t) {
		for _, line := range strings.Split(string(file.Source), "\n") {
			if strings.Contains(line, "client.Reaped()") {
				sites = append(sites, file.Name+":"+strings.TrimSpace(line))
			}
		}
	}

	// Assert.
	if len(sites) != 1 || sites[0] != "sessions.go:_, reaped := l.client.Reaped()" {
		t.Fatalf("reaped checks outside live.held: %v", sites)
	}
}
