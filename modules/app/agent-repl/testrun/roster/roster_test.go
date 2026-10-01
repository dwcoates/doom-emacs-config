package roster

import "testing"

func TestNamesAreUniqueAndInRosterOrder(t *testing.T) {
	// Act
	names := Names()

	// Assert
	seen := map[string]bool{}
	for i, n := range names {
		if seen[n] {
			t.Fatalf("suite %q is listed twice", n)
		}
		seen[n] = true
		if Suites[i].Name != n {
			t.Fatalf("Names()[%d] = %q, want roster order", i, n)
		}
	}
}

func TestLookup(t *testing.T) {
	tests := []struct {
		name   string
		wantOK bool
	}{
		{"ert", true},
		{"e2e-emacs", true},
		{"nope", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			s, ok := Lookup(tt.name)

			// Assert
			if ok != tt.wantOK || (ok && s.Name != tt.name) {
				t.Fatalf("Lookup(%q) = %+v, %v", tt.name, s, ok)
			}
		})
	}
}

func TestOnlyTheSandboxSuiteMayDecline(t *testing.T) {
	// The exit-77 disposition exists for a precondition the run cannot meet
	// (the e2e sandbox image); any other suite exiting 77 is a failure.
	for _, s := range Suites {
		if s.MayDecline != (s.Name == "e2e-emacs") {
			t.Errorf("%s MayDecline = %v", s.Name, s.MayDecline)
		}
	}
}
