package main

import "testing"

func TestWorkspaceIDFromListenSocket(t *testing.T) {
	tests := []struct {
		name   string
		listen string
		want   string
		refuse bool
	}{
		{
			name:   "the plain socket the daemon mints",
			listen: "/tmp/sock/0123456789abcdef.sock",
			want:   "0123456789abcdef",
		},
		{
			name:   "a rollout generation's socket names the same workspace",
			listen: "/tmp/sock/0123456789abcdef.n2.sock",
			want:   "0123456789abcdef",
		},
		{
			name:   "a basename that is not a workspace id is refused",
			listen: "/tmp/sock/shim.sock",
			refuse: true,
		},
		{
			name:   "the eight-character directory hash is not a workspace id",
			listen: "/tmp/sock/40569ff2.sock",
			refuse: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got, err := workspaceIDFromListenSocket(tc.listen)

			// Assert.
			if tc.refuse {
				if err == nil {
					t.Fatalf("workspaceIDFromListenSocket(%q) = %q, want a refusal", tc.listen, got)
				}
				return
			}
			if err != nil {
				t.Fatalf("workspaceIDFromListenSocket(%q) error = %v", tc.listen, err)
			}
			if got != tc.want {
				t.Fatalf("workspaceIDFromListenSocket(%q) = %q, want %q", tc.listen, got, tc.want)
			}
		})
	}
}
