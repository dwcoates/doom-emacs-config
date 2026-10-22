package refusalarm

import "testing"

// TestNestedIsItsArmName pins that a Nested value is exactly the arm name it
// was made from, which is what the rpc layer looks the oneof field up by.
func TestNestedIsItsArmName(t *testing.T) {
	cases := []struct {
		name string
		arm  string
	}{
		{name: "a snake_case arm", arm: "not_default_branch"},
		{name: "a one-word arm", arm: "slug"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			nested := Nested(tc.arm)

			// Act
			got := string(nested)

			// Assert
			if got != tc.arm {
				t.Fatalf("Nested(%q) = %q, want the arm name unchanged", tc.arm, got)
			}
		})
	}
}
