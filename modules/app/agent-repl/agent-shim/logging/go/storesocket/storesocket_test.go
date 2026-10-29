package storesocket

import "testing"

func TestReadPath(t *testing.T) {
	tests := []struct {
		name  string
		store string
		want  string
	}{
		{name: "the live store's socket", store: "/Users/u/.cache/agent-repl/sock/store.sock", want: "/Users/u/.cache/agent-repl/sock/store-read.sock"},
		{name: "a test harness's private socket", store: "/tmp/run-7/s.sock", want: "/tmp/run-7/s-read.sock"},
		{name: "a socket named without the suffix", store: "/tmp/run-7/store", want: "/tmp/run-7/store-read.sock"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := ReadPath(tt.store)

			// Assert
			if got != tt.want {
				t.Fatalf("ReadPath(%q) = %q, want %q", tt.store, got, tt.want)
			}
		})
	}
}
