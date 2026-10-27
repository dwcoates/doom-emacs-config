package workspace

import (
	"os"
	"path/filepath"
	"strings"
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

// TestEveryReapedCheckGoesThroughHeld pins that the fleet reads a client's
// reaping only through live.held, so a hand-rolled check cannot drift from it.
func TestEveryReapedCheckGoesThroughHeld(t *testing.T) {
	// Arrange.
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	var sites []string
	for _, file := range files {
		if strings.HasSuffix(file, "_test.go") {
			continue
		}
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		for _, line := range strings.Split(string(body), "\n") {
			if strings.Contains(line, "client.Reaped()") {
				sites = append(sites, file+":"+strings.TrimSpace(line))
			}
		}
	}

	// Assert.
	if len(sites) != 1 || !strings.HasPrefix(sites[0], "sessions.go:_, reaped := l.client.Reaped()") {
		t.Fatalf("reaped checks outside live.held: %v", sites)
	}
}
