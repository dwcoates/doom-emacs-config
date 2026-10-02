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

func TestEveryHarnessTestsAScriptInTheModulesBin(t *testing.T) {
	// Arrange / Act
	names := Harnesses()

	// Assert
	if len(names) == 0 {
		t.Fatal("no suite is marked as a bin/ harness")
	}
	for _, n := range names {
		s, _ := Lookup(n)
		isHarnessKind := s.Kind == Script || s.Kind == SplitScript
		if !isHarnessKind || len(s.Path) < len("bin/test-") || s.Path[:len("bin/test-")] != "bin/test-" {
			t.Errorf("%s is marked a bin/ harness but runs %q", n, s.Path)
		}
	}
}

func TestOnlyScriptSuitesHaveAWidth(t *testing.T) {
	// Every other kind is split into one-core units; a width there would be
	// silently ignored by its unit builder.
	for _, s := range Suites {
		if s.Slots != 0 && s.Kind != Script {
			t.Errorf("suite %s has width %d but is not a Script suite", s.Name, s.Slots)
		}
	}
}

func TestTheSandboxSuiteHoldsTheCoresItsContainerUses(t *testing.T) {
	// Arrange
	s, ok := Lookup("e2e-emacs")

	// Assert
	if !ok || s.Slots != 4 {
		t.Fatalf("e2e-emacs = %+v, want a width of 4: the VM its parallelism bound was measured on", s)
	}
}
