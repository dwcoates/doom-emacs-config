package dirpath

import (
	"strings"
	"testing"
)

func TestAbsolute(t *testing.T) {
	tests := []struct {
		name    string
		path    string
		home    string
		want    string
		wantErr string
	}{
		{name: "a tilde alone is home", path: "~", home: "/Users/me", want: "/Users/me"},
		{name: "a tilde-slash path is beneath home", path: "~/.config/doom", home: "/Users/me", want: "/Users/me/.config/doom"},
		{name: "a tilde-slash path is cleaned", path: "~/a/../b/", home: "/Users/me", want: "/Users/me/b"},
		{name: "an absolute path is taken as written, cleaned", path: "/repo/./x/", home: "/Users/me", want: "/repo/x"},
		{name: "an absolute path needs no home", path: "/repo", home: "", want: "/repo"},
		{name: "a relative path is refused", path: "repo/x", home: "/Users/me", wantErr: "is not an absolute path"},
		{name: "a dot path is refused", path: "./repo", home: "/Users/me", wantErr: "is not an absolute path"},
		{name: "another user's home is refused", path: "~bob/repo", home: "/Users/me", wantErr: "another user's home directory"},
		{name: "a tilde with no home is refused", path: "~/repo", home: "", wantErr: "is not an absolute one"},
		{name: "a tilde with a relative home is refused", path: "~/repo", home: "me", wantErr: "is not an absolute one"},
		{name: "an empty path is refused", path: "", home: "/Users/me", wantErr: "an empty path"},
		{name: "a tilde inside a path is not expanded", path: "/repo/~/x", home: "/Users/me", want: "/repo/~/x"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: tt.path and tt.home.

			// Act.
			got, err := Absolute(tt.path, tt.home)

			// Assert.
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("Absolute(%q, %q) = (%q, %v), want an error containing %q", tt.path, tt.home, got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("Absolute(%q, %q) = (%q, %v), want %q", tt.path, tt.home, got, err, tt.want)
			}
		})
	}
}
