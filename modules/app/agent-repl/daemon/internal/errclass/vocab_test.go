package errclass

import (
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
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
	Colors     []string `json:"colors"`
	Precedence []string `json:"precedence"`
	// RenderStates is what EVERY surface starts from. A surface may diverge
	// from it, but only by declaring the divergence in SurfaceOverrides.
	RenderStates map[string]string `json:"render_states"`
	// SurfaceOverrides is surface name -> state name -> color. The proto is
	// the authority on which state names exist, so validating the section is
	// this package's job even though no Go renderer reads it.
	SurfaceOverrides map[string]map[string]string `json:"surface_overrides"`
	ErrorClasses     map[string]string            `json:"error_classes"`
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

// validColor reports whether c is one of the six, or the explicit "none".
func validColor(f colorFixture, c string) bool {
	if c == "none" {
		return true
	}
	return slices.Contains(f.Colors, c)
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

func TestEveryToneHasAFixtureRow(t *testing.T) {
	// Arrange: the class enum left the wire — each kind arm states its own side
	// now — and the daemon resolves that side to one of exactly two tones. Both
	// must still be rows in the shared table, because a card takes its color
	// from the same place the workspace dot takes its own.
	f := loadFixture(t)
	// Act + Assert.
	for class, tone := range map[string]string{
		"ERROR_CLASS_INTERNAL": ToneLocal,
		"ERROR_CLASS_API":      ToneVendor,
	} {
		got, ok := f.ErrorClasses[class]
		if !ok {
			t.Errorf("%s has no row in the color fixture, but the daemon still resolves that side to %q", class, tone)
			continue
		}
		if got != tone {
			t.Errorf("%s is %q in the color fixture, but the daemon resolves that side to %q", class, got, tone)
		}
	}
}

func TestEveryFixtureColorIsOneOfTheSix(t *testing.T) {
	// Arrange: a SEVENTH color would be a seventh vocabulary, which is the
	// thing the color contract exists to stop. Teal was added as the sixth
	// only because blue was carrying two incompatible jobs — "asleep on
	// purpose" and "the substrate is broken" — and a color that fires on both
	// means neither.
	f := loadFixture(t)
	// Act + Assert.
	for name, color := range f.RenderStates {
		if !validColor(f, color) {
			t.Errorf("RenderState %s takes %q, which is not one of the six (or none)", name, color)
		}
	}
	for name, color := range f.ErrorClasses {
		if !validColor(f, color) {
			t.Errorf("ErrorClass %s takes %q, which is not one of the six (or none)", name, color)
		}
	}
}

func TestEverySurfaceOverrideNamesARealRenderState(t *testing.T) {
	// Arrange: the generated name table is the authority on the enum's
	// membership for overrides exactly as it is for the shared assignment. An
	// override keyed on a state that does not exist is a divergence nobody can
	// honor, and it would sit in the contract reading as coverage.
	f := loadFixture(t)
	known := map[string]bool{}
	for _, name := range frontendv1.RenderState_name {
		known[name] = true
	}
	// Act + Assert.
	for surface, overrides := range f.SurfaceOverrides {
		for name := range overrides {
			if !known[name] {
				t.Errorf("surface %s overrides %s, which is not a RenderState", surface, name)
			}
		}
	}
}

func TestEverySurfaceOverrideSpendsOneOfTheSix(t *testing.T) {
	// Arrange: an override exists to ADD a color to a surface that cannot
	// otherwise say anything — the Emacs tab bar has no badge and no glyph, so
	// a merge running there renders as no state at all. "none" is therefore
	// not a legal override: it would be a second way to say what the shared
	// table already says, and a seventh color would be a second vocabulary.
	f := loadFixture(t)
	// Act + Assert.
	for surface, overrides := range f.SurfaceOverrides {
		for name, color := range overrides {
			if !slices.Contains(f.Colors, color) {
				t.Errorf("surface %s paints %s %q, which is not one of the six", surface, name, color)
			}
		}
	}
}

func TestEverySurfaceOverrideActuallyDiverges(t *testing.T) {
	// Arrange: an override that restates the shared color is not an override.
	// It reads as a deliberate divergence to anyone auditing this file, and it
	// would silently outlive a change to the shared row it duplicates.
	f := loadFixture(t)
	// Act + Assert.
	for surface, overrides := range f.SurfaceOverrides {
		for name, color := range overrides {
			if shared := f.RenderStates[name]; shared == color {
				t.Errorf("surface %s overrides %s to %q, which is already the shared color", surface, name, shared)
			}
		}
	}
}

func TestTheInFlightMergeStatesAreTheOnlyTabBarOverrides(t *testing.T) {
	// Arrange: the Emacs tab bar spends red on the three merge states with no
	// verdict yet, because red already means "work is in flight and you cannot
	// act". MERGE_CONFLICT is deliberately excluded — it is waiting on the
	// USER — and MERGED / MERGE_FAILED are terminal. Pinning the exact set
	// here keeps a later "while we're at it" from quietly reddening a merge
	// state that is not in flight.
	f := loadFixture(t)
	want := map[string]string{
		"RENDER_STATE_MERGE_ENQUEUING": "red",
		"RENDER_STATE_MERGE_QUEUED":    "red",
		"RENDER_STATE_MERGING":         "red",
	}
	// Act + Assert.
	got := f.SurfaceOverrides["emacs_tab_bar"]
	if len(got) != len(want) {
		t.Fatalf("emacs_tab_bar overrides %v, want exactly %v", got, want)
	}
	for name, color := range want {
		if got[name] != color {
			t.Errorf("emacs_tab_bar paints %s %q, want %q", name, got[name], color)
		}
	}
}

func TestTheWebappTakesNoSurfaceOverride(t *testing.T) {
	// Arrange: the webapp's rail carries a glyph and a status word beside
	// every dot, so the merge pipeline already reports itself there without
	// spending a color. Its status indications stay on the shared assignment.
	f := loadFixture(t)
	// Act + Assert.
	if overrides, ok := f.SurfaceOverrides["webapp"]; ok {
		t.Fatalf("the webapp declares overrides %v; its dots read the shared assignment", overrides)
	}
}

func TestTheFixturePrecedenceMatchesTheSSMRanks(t *testing.T) {
	// Arrange: the SSM's SQL prec table is the SOLE precedence authority; the
	// color fixture may restate it but never reorder it. Divergence here means
	// a frontend would paint a weaker claim over a stronger one.
	f := loadFixture(t)
	want := []string{"blue", "teal", "purple", "red", "yellow", "green"}
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

func TestThePrecedenceCoversExactlyTheSixColors(t *testing.T) {
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

func TestTheContextCutStatesTakeTheColorThinkingTakes(t *testing.T) {
	// Arrange: CLEARING and COMPACTING say exactly what THINKING says about
	// what the user cannot do — the agent is busy, a prompt cannot land. Only
	// the phase WORD distinguishes them, so a color of their own would invent
	// a distinction the vocabulary does not have.
	f := loadFixture(t)
	want := f.RenderStates["RENDER_STATE_THINKING"]
	// Act + Assert.
	for _, name := range []string{"RENDER_STATE_CLEARING", "RENDER_STATE_COMPACTING"} {
		if got := f.RenderStates[name]; got != want {
			t.Errorf("%s takes %q but THINKING takes %q", name, got, want)
		}
	}
}

func TestHibernatedDoesNotTakeTheColorSeveredTakes(t *testing.T) {
	// Arrange: the two used to be ONE state, and the whole reason for the split
	// is that they must never render alike. A hibernation is a choice we made
	// to reclaim memory; a severance is evidence the substrate broke. If this
	// ever passes by taking the same color, blue is back to meaning nothing.
	f := loadFixture(t)
	// Act + Assert.
	if hib, sev := f.RenderStates["RENDER_STATE_HIBERNATED"], f.RenderStates["RENDER_STATE_SEVERED"]; hib == sev {
		t.Fatalf("HIBERNATED and SEVERED both take %q; the split exists precisely so they differ", hib)
	}
}

func TestHibernatedRanksBetweenBlueAndPurple(t *testing.T) {
	// Arrange: teal's rank restates the SSM's `hibernated` at 15 — below the
	// blue band (severed 12, starting 14) and above purple's 20. Below GREEN
	// would be the tempting mistake, and it would let a stale `thinking` row
	// from the turn a workspace was hibernated after mask a workspace that is
	// genuinely asleep.
	f := loadFixture(t)
	rank := map[string]int{}
	for i, c := range f.Precedence {
		rank[c] = i
	}
	// Act + Assert.
	teal := f.RenderStates["RENDER_STATE_HIBERNATED"]
	if !(rank[f.RenderStates["RENDER_STATE_SEVERED"]] < rank[teal] && rank[teal] < rank[f.RenderStates["RENDER_STATE_VENDOR_BLOCKED"]]) {
		t.Fatalf("hibernated's color %q ranks %d, not strictly between severed's %d and vendor-blocked's %d",
			teal, rank[teal], rank[f.RenderStates["RENDER_STATE_SEVERED"]], rank[f.RenderStates["RENDER_STATE_VENDOR_BLOCKED"]])
	}
}

func TestHibernatedOutranksEveryGreenState(t *testing.T) {
	// Arrange: the actionability claim teal makes is blue's, not green's — you
	// cannot interact with a hibernated workspace without paying a bring-up
	// first, which is exactly what green promises you do not have to do.
	f := loadFixture(t)
	rank := map[string]int{}
	for i, c := range f.Precedence {
		rank[c] = i
	}
	teal := rank[f.RenderStates["RENDER_STATE_HIBERNATED"]]
	// Act + Assert.
	if green := rank[f.RenderStates["RENDER_STATE_IDLE"]]; teal >= green {
		t.Fatalf("hibernated ranks %d and green ranks %d; a hibernated workspace must never be masked by a green claim", teal, green)
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

func TestEveryTypeResolvesToAColoredTone(t *testing.T) {
	// Arrange: a type whose tone has no color would render an uncolored card,
	// which is a failure that does not look like one.
	f := loadFixture(t)
	// Act + Assert: the WHOLE vocabulary, not a sample of construction paths —
	// the tone is now a total function of the type, so every type can be asked.
	for _, typ := range AllTypes() {
		tone := Tone(typ)
		if tone == "none" || !validColor(f, tone) {
			t.Errorf("failure %q resolves to tone %q, which is not a color the shared table names", typ, tone)
		}
	}
}

func TestNoConstructionPathEmitsAnUnkindedCard(t *testing.T) {
	// Arrange: an unset kind arm is the absence of a classification — it is
	// what UNSPECIFIED used to be — and emitting one would be the classifier
	// declining to classify, silently. A renderer receiving it has a failure
	// with nothing to draw and no way to say why.
	logf, _ := capture()
	// Act + Assert.
	for _, item := range []*frontendv1.FailureCardView{
		Command(logf, ErrShimNack),
		Command(logf, errUnknownForVocabTest{}),
		Death(logf, "s_1", "some ancient reason", 0),
		Degraded("shim-store", "boom", 0),
		ConnectionDegraded("shim-connection", "no traffic"),
	} {
		if _, ok := TypeOf(item.GetKind()); !ok {
			t.Errorf("a construction path emitted a card whose kind arm names no daemon failure type: %v", item.GetKind())
		}
	}
}

// errUnknownForVocabTest is an error matching no sentinel, so the fallthrough
// path is covered by the class assertion above.
type errUnknownForVocabTest struct{}

func (errUnknownForVocabTest) Error() string { return "unclassifiable" }
