package logsite

import (
	"errors"
	"reflect"
	"strings"
	"testing"
	"time"
)

func TestKeyIsTwelveHexOfTheSha256OfRuntimeFunctionAndTemplate(t *testing.T) {
	// Arrange
	// The value is pinned (printf 'daemon\0pkg.F\0hello {name}' | shasum -a
	// 256), and every runtime's port pins the same one, so all derive one key.
	runtime, function, template := "daemon", "pkg.F", "hello {name}"

	// Act
	key := Key(runtime, function, template)

	// Assert
	if len(key) != KeyLength {
		t.Fatalf("key %q has %d characters, want %d", key, len(key), KeyLength)
	}
	if key != Key(runtime, function, template) {
		t.Errorf("the key of one site differs between calls")
	}
	if want := "074892b7d933"; key != want {
		t.Errorf("Key = %q, want %q", key, want)
	}
}

func TestKeyDiffersWithEachOfItsInputs(t *testing.T) {
	base := Key("daemon", "pkg.F", "t")
	tests := []struct {
		name string
		key  string
	}{
		{name: "runtime", key: Key("store", "pkg.F", "t")},
		{name: "function", key: Key("daemon", "pkg.G", "t")},
		{name: "template", key: Key("daemon", "pkg.F", "u")},
		{name: "a boundary moved between inputs", key: Key("daemon", "pkg.Ft", "")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act and Assert
			if tc.key == base {
				t.Errorf("changing the %s left the key %q unchanged", tc.name, base)
			}
		})
	}
}

func TestPlaceholdersAnswersEachNameOnceInOrder(t *testing.T) {
	tests := []struct {
		name     string
		template string
		want     []string
	}{
		{name: "none", template: "plain text", want: nil},
		{name: "two", template: "shim {shim} after {elapsed}", want: []string{"shim", "elapsed"}},
		{name: "repeated", template: "{a} and {a} and {b}", want: []string{"a", "b"}},
		{name: "doubled braces are literal", template: "{{not}} {real}", want: []string{"real"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := Placeholders(tc.template)

			// Assert
			if err != nil {
				t.Fatalf("Placeholders(%q): %v", tc.template, err)
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Errorf("Placeholders(%q) = %v, want %v", tc.template, got, tc.want)
			}
		})
	}
}

func TestPlaceholdersRefusesATemplateThatDoesNotParse(t *testing.T) {
	tests := []struct {
		name     string
		template string
		cause    string
	}{
		{name: "unclosed", template: "open {shim", cause: "never closes"},
		{name: "empty", template: "a {} b", cause: "not a lowercase identifier"},
		{name: "uppercase", template: "a {Shim} b", cause: "not a lowercase identifier"},
		{name: "spaced", template: "a { shim } b", cause: "not a lowercase identifier"},
		{name: "lone close", template: "a } b", cause: "lone closing brace"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := Placeholders(tc.template)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.cause) {
				t.Errorf("Placeholders(%q) error = %v, want one saying %q", tc.template, err, tc.cause)
			}
		})
	}
}

func TestRenderFieldsFillsEachPlaceholderFromItsField(t *testing.T) {
	tests := []struct {
		name     string
		template string
		fields   map[string]any
		want     string
	}{
		{name: "no placeholders", template: "plain", fields: nil, want: "plain"},
		{name: "string and duration", template: "shim {shim} after {elapsed}",
			fields: map[string]any{"shim": "s1", "elapsed": 3 * time.Second}, want: "shim s1 after 3s"},
		{name: "error", template: "failed: {cause}", fields: map[string]any{"cause": errors.New("boom")}, want: "failed: boom"},
		{name: "number", template: "n={n}", fields: map[string]any{"n": 42}, want: "n=42"},
		{name: "literal braces", template: "{{x}} is {x}", fields: map[string]any{"x": 1}, want: "{x} is 1"},
		{name: "extra fields are ignored", template: "{a}", fields: map[string]any{"a": "A", "b": "B"}, want: "A"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := RenderFields(tc.template, tc.fields)

			// Assert
			if err != nil {
				t.Fatalf("RenderFields: %v", err)
			}
			if got != tc.want {
				t.Errorf("RenderFields(%q) = %q, want %q", tc.template, got, tc.want)
			}
		})
	}
}

func TestRenderFieldsRefusesAPlaceholderWithNoField(t *testing.T) {
	// Act
	_, err := RenderFields("shim {shim} after {elapsed}", map[string]any{"shim": "s1"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "{elapsed}") {
		t.Errorf("error = %v, want one naming {elapsed}", err)
	}
}

func TestNormalizeFunctionKeepsOnlyTheEnclosingNamedFunction(t *testing.T) {
	tests := []struct {
		name, full, want string
	}{
		{name: "function", full: "agent-repld/internal/merge.dispatch", want: "agent-repld/internal/merge.dispatch"},
		{name: "pointer method", full: "agent-repld/internal/merge.(*orchestrator).Mediate", want: "agent-repld/internal/merge.(*orchestrator).Mediate"},
		{name: "value method", full: "agentrepl/logging.Selection.Note", want: "agentrepl/logging.Selection.Note"},
		{name: "closure", full: "agent-repld/internal/merge.(*orchestrator).Mediate.func1", want: "agent-repld/internal/merge.(*orchestrator).Mediate"},
		{name: "nested closure", full: "a/b.F.func2.3", want: "a/b.F"},
		{name: "go wrapper", full: "a/b.F.gowrap1", want: "a/b.F"},
		{name: "defer wrapper", full: "a/b.(*T).M.deferwrap2", want: "a/b.(*T).M"},
		{name: "generic", full: "a/b.Map[...].func1", want: "a/b.Map"},
		{name: "generic receiver", full: "a/b.(*Set[go.shape.string]).Add", want: "a/b.(*Set).Add"},
		{name: "package variable", full: "a/b.glob..func1", want: "a/b.init"},
		{name: "init closure", full: "a/b.init.0.func1", want: "a/b.init"},
		{name: "main", full: "main.main.func1", want: "main.main"},
		{name: "dotted domain", full: "golang.org/x/y.F.func1", want: "golang.org/x/y.F"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := NormalizeFunction(tc.full)

			// Assert
			if got != tc.want {
				t.Errorf("NormalizeFunction(%q) = %q, want %q", tc.full, got, tc.want)
			}
		})
	}
}

// logVia is a stand-in logging helper: Function skips its frames.
func logVia(c *Callers) string { return c.Function() }

func TestCallersAnswersTheFirstFunctionOutsideTheHelpers(t *testing.T) {
	// Arrange
	direct := NewCallers()

	// Act
	viaClosure := func() string { return direct.Function() }()

	// Assert
	if got, want := viaClosure, "agentrepl/logging/logsite.TestCallersAnswersTheFirstFunctionOutsideTheHelpers"; got != want {
		t.Errorf("a call from a closure resolved to %q, want its enclosing test %q", got, want)
	}
}

func TestCallersSkipsEveryHelperFrame(t *testing.T) {
	// Arrange
	c := NewCallers("agentrepl/logging/logsite")

	// Act
	got := func() string { return logVia(c) }()

	// Assert
	// Every frame up to the test runner lies in this package, so the first
	// frame outside it is the testing package's.
	if !strings.HasPrefix(got, "testing.") {
		t.Errorf("Function = %q, want the first frame outside the helper package", got)
	}
}

func TestCallersAnswersTheSameFunctionFromItsCache(t *testing.T) {
	// Arrange
	c := NewCallers()
	resolve := func() string { return c.Function() }

	// Act
	first, second := resolve(), resolve()

	// Assert
	if first != second {
		t.Errorf("one call site resolved to %q, then %q", first, second)
	}
}

func TestValidKeyAcceptsOnlyTwelveLowercaseHex(t *testing.T) {
	tests := []struct {
		name, key string
		want      bool
	}{
		{name: "a derived key", key: Key("daemon", "p.F", "t"), want: true},
		{name: "empty", key: "", want: false},
		{name: "short", key: "abc123", want: false},
		{name: "long", key: "0123456789abc", want: false},
		{name: "uppercase", key: "0123456789AB", want: false},
		{name: "not hex", key: "0123456789ag", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act and Assert
			if got := ValidKey(tc.key); got != tc.want {
				t.Errorf("ValidKey(%q) = %v, want %v", tc.key, got, tc.want)
			}
		})
	}
}
