package errclass

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// fixturePath is the checked-in cross-language color contract. It lives beside
// the protos rather than inside any one language's tree, because it is a
// contract among three renderers and giving it a home inside one of them would
// make that one the author of the other two's palettes.
const fixturePath = "../../../proto/vocab/render-colors.json"

// colorFixture is the shape of render-colors.json. The `_comment` keys are
// prose for a reader and are deliberately not decoded.
type colorFixture struct {
	Colors       []string          `json:"colors"`
	Precedence   []string          `json:"precedence"`
	RenderStates map[string]string `json:"render_states"`
	ErrorClasses map[string]string `json:"error_classes"`
}

func loadFixture(t *testing.T) colorFixture {
	t.Helper()
	raw, err := os.ReadFile(filepath.FromSlash(fixturePath))
	if err != nil {
		t.Fatalf("read the color fixture: %v", err)
	}
	var f colorFixture
	if err := json.Unmarshal(raw, &f); err != nil {
		t.Fatalf("decode the color fixture: %v", err)
	}
	return f
}

// validColor reports whether c is one of the five, or the explicit "none".
func validColor(f colorFixture, c string) bool {
	if c == "none" {
		return true
	}
	for _, known := range f.Colors {
		if known == c {
			return true
		}
	}
	return false
}

func TestEveryRenderStateHasAFixtureRow(t *testing.T) {
	// Arrange: the GENERATED name table is the authority on the enum's
	// membership, so a state added to the proto with no color assignment
	// fails here rather than reaching a frontend that must invent one.
	f := loadFixture(t)
	// Act + Assert.
	for value, name := range frontendv1.RenderState_name {
		if _, ok := f.RenderStates[name]; !ok {
			t.Errorf("RenderState %s (%d) has no row in the color fixture", name, value)
		}
	}
}

func TestTheFixtureHasNoRenderStateRowsOutsideTheEnum(t *testing.T) {
	// Arrange: the other direction — a row for a state that no longer exists
	// is dead weight that reads as coverage.
	f := loadFixture(t)
	known := map[string]bool{}
	for _, name := range frontendv1.RenderState_name {
		known[name] = true
	}
	// Act + Assert.
	for name := range f.RenderStates {
		if !known[name] {
			t.Errorf("the color fixture carries %s, which is not a RenderState", name)
		}
	}
}

func TestEveryErrorClassHasAFixtureRow(t *testing.T) {
	// Arrange.
	f := loadFixture(t)
	// Act + Assert.
	for value, name := range frontendv1.ErrorClass_name {
		if _, ok := f.ErrorClasses[name]; !ok {
			t.Errorf("ErrorClass %s (%d) has no row in the color fixture", name, value)
		}
	}
}

func TestEveryFixtureColorIsOneOfTheFive(t *testing.T) {
	// Arrange: a sixth color would be a sixth vocabulary, which is the thing
	// the five-color contract exists to stop.
	f := loadFixture(t)
	// Act + Assert.
	for name, color := range f.RenderStates {
		if !validColor(f, color) {
			t.Errorf("RenderState %s takes %q, which is not one of the five (or none)", name, color)
		}
	}
	for name, color := range f.ErrorClasses {
		if !validColor(f, color) {
			t.Errorf("ErrorClass %s takes %q, which is not one of the five (or none)", name, color)
		}
	}
}

func TestTheFixturePrecedenceMatchesTheSSMRanks(t *testing.T) {
	// Arrange: the SSM's SQL prec table is the SOLE precedence authority; the
	// color fixture may restate it but never reorder it. Divergence here means
	// a frontend would paint a weaker claim over a stronger one.
	f := loadFixture(t)
	want := []string{"blue", "purple", "red", "yellow", "green"}
	// Act + Assert.
	if len(f.Precedence) != len(want) {
		t.Fatalf("precedence = %v, want %v", f.Precedence, want)
	}
	for i, c := range want {
		if f.Precedence[i] != c {
			t.Fatalf("precedence[%d] = %q, want %q (the SSM's rank order)", i, f.Precedence[i], c)
		}
	}
}

func TestThePrecedenceCoversExactlyTheFiveColors(t *testing.T) {
	// Arrange: a color declared but never ranked has no defined behavior when
	// it meets another.
	f := loadFixture(t)
	// Act + Assert.
	if len(f.Precedence) != len(f.Colors) {
		t.Fatalf("precedence ranks %d colors but %d are declared", len(f.Precedence), len(f.Colors))
	}
	ranked := map[string]bool{}
	for _, c := range f.Precedence {
		ranked[c] = true
	}
	for _, c := range f.Colors {
		if !ranked[c] {
			t.Errorf("color %q is declared but never ranked", c)
		}
	}
}

func TestTheDeprecatedStopFailedTakesTheColorItAlwaysMeant(t *testing.T) {
	// Arrange: STOP_FAILED is deprecated but still mappable — an old daemon
	// binary can push it. It must resolve to the purple VENDOR_BLOCKED means,
	// because that is what it always was: a turn ended on something only a
	// human or the vendor can release.
	f := loadFixture(t)
	// Act + Assert.
	if got, want := f.RenderStates["RENDER_STATE_STOP_FAILED"], f.RenderStates["RENDER_STATE_VENDOR_BLOCKED"]; got != want {
		t.Fatalf("STOP_FAILED takes %q but VENDOR_BLOCKED takes %q", got, want)
	}
}

func TestTheInternalClassTakesTheSameColorAsTheStatesItDescribes(t *testing.T) {
	// Arrange: card color IS state color. An INTERNAL failure resolves the
	// workspace blue, so its card must be blue — a purple workspace explained
	// by a red card is exactly the drift this table prevents.
	f := loadFixture(t)
	// Act + Assert.
	if got, want := f.ErrorClasses["ERROR_CLASS_INTERNAL"], f.RenderStates["RENDER_STATE_DEGRADED"]; got != want {
		t.Fatalf("ERROR_CLASS_INTERNAL takes %q but the degraded state it describes takes %q", got, want)
	}
}

func TestTheApiClassTakesTheSameColorAsTheStateItDescribes(t *testing.T) {
	// Arrange: an API failure resolves the workspace vendor-blocked.
	f := loadFixture(t)
	// Act + Assert.
	if got, want := f.ErrorClasses["ERROR_CLASS_API"], f.RenderStates["RENDER_STATE_VENDOR_BLOCKED"]; got != want {
		t.Fatalf("ERROR_CLASS_API takes %q but the vendor-blocked state it describes takes %q", got, want)
	}
}

func TestEveryTypeBelongsToAColoredClass(t *testing.T) {
	// Arrange: a type whose class has no color would render an uncolored
	// card, which is a failure that does not look like one.
	f := loadFixture(t)
	logf, _ := capture()
	// Act + Assert: exercise the class each construction path assigns.
	for _, item := range []*frontendv1.SystemFailureItem{
		Command(logf, ErrShimNack),
		Death(logf, DeathReasonShimDied),
		Degraded("shim-store", "boom", 0),
		ConnectionDegraded("no traffic"),
	} {
		name := frontendv1.ErrorClass_name[int32(item.GetErrorClass())]
		if f.ErrorClasses[name] == "none" {
			t.Errorf("failure %q carries class %s, which has no color", item.GetErrorType(), name)
		}
	}
}

func TestNoConstructionPathEmitsAnUnspecifiedClass(t *testing.T) {
	// Arrange: UNSPECIFIED is the absence of a classification. Emitting it
	// would be the classifier declining to classify, silently.
	logf, _ := capture()
	// Act + Assert.
	for _, item := range []*frontendv1.SystemFailureItem{
		Command(logf, ErrShimNack),
		Command(logf, errUnknownForVocabTest{}),
		Death(logf, "some ancient reason"),
		Degraded("shim-store", "boom", 0),
		ConnectionDegraded("no traffic"),
	} {
		if item.GetErrorClass() == frontendv1.ErrorClass_ERROR_CLASS_UNSPECIFIED {
			t.Errorf("failure %q was emitted with an UNSPECIFIED class", item.GetErrorType())
		}
	}
}

// errUnknownForVocabTest is an error matching no sentinel, so the fallthrough
// path is covered by the class assertion above.
type errUnknownForVocabTest struct{}

func (errUnknownForVocabTest) Error() string { return "unclassifiable" }
