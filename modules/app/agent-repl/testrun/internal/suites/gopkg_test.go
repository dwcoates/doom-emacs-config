package suites

import "testing"

func TestPackageGroup(t *testing.T) {
	tests := []struct {
		name, rel, tags, want string
	}{
		{"the ordinary pass", "a/b", "", "m:a/b"},
		{"a tagged pass names its tags", "a/b", "integration", "m:a/b[integration]"},
		{"the module root", ".", "", "m:."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := packageGroup("m", tt.rel, tt.tags)

			// Assert
			if got != tt.want {
				t.Fatalf("packageGroup = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestAPackagesGroupIsItsPackageGroup(t *testing.T) {
	// Arrange
	p := goPkg{Suite: "m", Rel: "a", Tags: "integration"}

	// Act
	got := p.group()

	// Assert
	if want := packageGroup("m", "a", "integration"); got != want {
		t.Fatalf("group = %q, want %q", got, want)
	}
}
