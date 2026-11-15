package reusewhy

import "testing"

func TestEveryReasonIsNamedOnceAndCarriesNoTallySeparator(t *testing.T) {
	// Arrange
	seen := map[string]bool{}

	for _, why := range All {
		t.Run(why, func(t *testing.T) {
			// Assert: a tally joins its parts with ", " and a count with " ",
			// so a reason holding a comma would split when read back.
			if seen[why] {
				t.Fatalf("%q is named twice", why)
			}
			seen[why] = true
			for _, r := range why {
				if r == ',' {
					t.Fatalf("%q holds a comma", why)
				}
			}
		})
	}
}
