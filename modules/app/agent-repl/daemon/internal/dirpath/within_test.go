package dirpath

import (
	"testing"
)

func TestWithin(t *testing.T) {
	tests := []struct {
		name      string
		path, dir string
		want      bool
	}{
		{name: "the directory itself", path: "/a/b", dir: "/a/b", want: true},
		{name: "a path beneath it", path: "/a/b/c/d", dir: "/a/b", want: true},
		{name: "a sibling sharing its prefix", path: "/a/bc", dir: "/a/b", want: false},
		{name: "its parent", path: "/a", dir: "/a/b", want: false},
		{name: "an unrelated path", path: "/x/y", dir: "/a/b", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Within(tt.path, tt.dir)

			// Assert
			if got != tt.want {
				t.Fatalf("Within(%q, %q) = %v, want %v", tt.path, tt.dir, got, tt.want)
			}
		})
	}
}
