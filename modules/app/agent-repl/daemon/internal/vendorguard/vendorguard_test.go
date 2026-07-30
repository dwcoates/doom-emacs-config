package vendorguard

import (
	"strings"
	"testing"
)

func TestForbidden(t *testing.T) {
	tests := []struct {
		name  string
		value string
		set   bool
		want  bool
	}{
		{name: "unset permits", set: false, want: false},
		{name: "empty permits", set: true, value: "", want: false},
		{name: "one forbids", set: true, value: "1", want: true},
		{name: "any non-empty value forbids", set: true, value: "0", want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			if tc.set {
				t.Setenv(EnvVar, tc.value)
			} else {
				t.Setenv(EnvVar, "")
				// t.Setenv cannot unset; the empty case is equivalent here and
				// is covered by its own row above.
			}
			// Act
			got := Forbidden()
			// Assert
			if got != tc.want {
				t.Fatalf("Forbidden() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestCheckPermitsWhenUnset(t *testing.T) {
	// Arrange
	t.Setenv(EnvVar, "")
	// Act
	err := Check("site")
	// Assert
	if err != nil {
		t.Fatalf("Check() = %v, want nil", err)
	}
}

func TestCheckNamesTheVariable(t *testing.T) {
	// Arrange
	t.Setenv(EnvVar, "1")
	// Act
	err := Check("site")
	// Assert
	if err == nil || !strings.Contains(err.Error(), EnvVar) {
		t.Fatalf("Check() = %v, want an error naming %s", err, EnvVar)
	}
}

func TestCheckNamesTheBlockedSite(t *testing.T) {
	// Arrange
	t.Setenv(EnvVar, "1")
	// Act
	err := Check("sessioncontroller.spawnClassifier")
	// Assert
	if err == nil || !strings.Contains(err.Error(), "sessioncontroller.spawnClassifier") {
		t.Fatalf("Check() = %v, want an error naming the blocked site", err)
	}
}
