package vitestmeasure

import (
	"strings"
	"testing"
)

func TestMutateMatcher(t *testing.T) {
	tests := []struct {
		name  string
		globs []string
		path  string
		want  bool
	}{
		{name: "a file at any depth under **", globs: []string{"src/**/*.ts"}, path: "src/engine/session.ts", want: true},
		{name: "a file directly under **", globs: []string{"src/**/*.ts"}, path: "src/main.ts", want: true},
		{name: "another extension", globs: []string{"src/**/*.ts"}, path: "src/main.js", want: false},
		{name: "outside the glob's directory", globs: []string{"src/**/*.ts"}, path: "test/a.test.ts", want: false},
		{name: "a negated directory", globs: []string{"src/**/*.ts", "!src/fake/**"}, path: "src/fake/sdk.ts", want: false},
		{name: "a star never crosses a slash", globs: []string{"src/*.ts"}, path: "src/engine/session.ts", want: false},
		{name: "a question mark is one character", globs: []string{"src/?.ts"}, path: "src/a.ts", want: true},
		{name: "a dot is literal", globs: []string{"src/a.ts"}, path: "src/abts", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			m, err := newMutateMatcher(tt.globs)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			got := m.match(tt.path)

			// Assert
			if got != tt.want {
				t.Fatalf("match(%q) under %v = %t, want %t", tt.path, tt.globs, got, tt.want)
			}
		})
	}
}

func TestMutateMatcherRefusesGlobsThatIncludeNothing(t *testing.T) {
	// Act
	_, err := newMutateMatcher([]string{"!src/fake/**"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "include nothing") {
		t.Fatalf("err = %v, want the globs refused", err)
	}
}
