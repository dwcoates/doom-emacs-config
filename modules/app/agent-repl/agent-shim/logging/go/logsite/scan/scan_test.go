package scan

import (
	"strings"
	"testing"

	"agentrepl/logging/logsite"
)

func fixtureSites(t *testing.T) []Site {
	t.Helper()
	sites, err := Scan(Spec{
		Runtime: "daemon", Module: "example.com/mod", Root: "testdata/mod",
		Calls: []Call{{Selector: "Error", Args: 3, Template: 1, Context: 2}},
	})
	if err != nil {
		t.Fatalf("Scan: %v", err)
	}
	return sites
}

func siteOf(t *testing.T, sites []Site, template string) Site {
	t.Helper()
	for _, s := range sites {
		if s.Template == template {
			return s
		}
	}
	t.Fatalf("no site writes %q; found %v", template, sites)
	return Site{}
}

func TestScanFindsEveryConstantTemplateWithItsFunction(t *testing.T) {
	sites := fixtureSites(t)
	tests := []struct {
		name, template, function string
	}{
		{name: "literal in a pointer method", template: "plain", function: "example.com/mod.(*T).Run"},
		{name: "constant concatenation", template: "tick {n}", function: "example.com/mod.(*T).Run"},
		{name: "closure keyed by its enclosing method", template: "in a closure {shim}", function: "example.com/mod.(*T).Run"},
		{name: "another package's constant", template: "shared constant", function: "example.com/mod.(*T).Run"},
		{name: "value method", template: "missing {gone}", function: "example.com/mod.T.Value"},
		{name: "package level", template: "at package level", function: "example.com/mod.init"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			s := siteOf(t, sites, tc.template)

			// Assert
			if s.Function != tc.function {
				t.Errorf("function = %q, want %q", s.Function, tc.function)
			}
			if want := logsite.Key("daemon", tc.function, tc.template); s.Key != want {
				t.Errorf("key = %q, want %q", s.Key, want)
			}
		})
	}
}

func TestScanSkipsNonConstantTemplatesAndTestFiles(t *testing.T) {
	// Act
	sites := fixtureSites(t)

	// Assert
	for _, s := range sites {
		if s.Template == "not a constant" || s.Template == "a test's call" {
			t.Errorf("scanned %q at %s, which is no log call of the module", s.Template, s.Position())
		}
	}
}

func TestScanReadsTheContextLiteralsFields(t *testing.T) {
	// Arrange
	sites := fixtureSites(t)

	// Act
	literal, dynamic := siteOf(t, sites, "tick {n}"), siteOf(t, sites, "dynamic {x}")

	// Assert
	if !literal.LiteralContext || len(literal.Fields) != 1 || literal.Fields[0] != "n" {
		t.Errorf("literal context = %v %v, want [n]", literal.LiteralContext, literal.Fields)
	}
	if dynamic.LiteralContext {
		t.Errorf("a variable context read as a literal")
	}
}

func TestCheckFindsEveryFault(t *testing.T) {
	// Arrange
	errs := Check(fixtureSites(t))
	var text []string
	for _, e := range errs {
		text = append(text, e.Error())
	}
	joined := strings.Join(text, "\n")
	tests := []struct {
		name, want string
	}{
		{name: "missing placeholder", want: "names {gone}, which the call's context does not hold"},
		{name: "unprovable placeholder", want: `"dynamic {x}" names placeholders, so its call's context must be a literal`},
		{name: "unparsed template", want: "never closes"},
		{name: "one template twice in a function", want: `write the template "twice", so they are one site`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Assert
			if !strings.Contains(joined, tc.want) {
				t.Errorf("Check found no %s; it found:\n%s", tc.name, joined)
			}
		})
	}
	if len(errs) != len(tests) {
		t.Errorf("Check found %d faults, want %d:\n%s", len(errs), len(tests), joined)
	}
}

func TestCheckFindsAKeyCollision(t *testing.T) {
	// Arrange
	sites := []Site{
		{File: "a.go", Line: 1, Function: "p.F", Template: "x", Key: "aaaaaaaaaaaa"},
		{File: "b.go", Line: 2, Function: "p.G", Template: "y", Key: "aaaaaaaaaaaa"},
	}

	// Act
	errs := Check(sites)

	// Assert
	if len(errs) != 1 || !strings.Contains(errs[0].Error(), "hash to the key aaaaaaaaaaaa") {
		t.Errorf("Check = %v, want one collision", errs)
	}
}
