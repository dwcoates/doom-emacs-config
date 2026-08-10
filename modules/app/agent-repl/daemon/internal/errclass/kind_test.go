package errclass

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// The two tables in kind.go are each other's inverse. Nothing but this test
// keeps them so: a type added to the vocabulary with no arm, or an arm wired to
// the wrong type, is otherwise invisible until a card renders as the wrong
// failure.
func TestEveryTypeRoundTripsThroughItsKindArm(t *testing.T) {
	// Arrange.
	for _, typ := range AllTypes() {
		t.Run(string(typ), func(t *testing.T) {
			// Act.
			kind := kindFor(typ)
			// Assert.
			if kind == nil {
				t.Fatalf("no kind arm for %q", typ)
			}
			got, ok := TypeOf(kind)
			if !ok {
				t.Fatalf("kind arm %T for %q names no type on the way back", kind.GetKind(), typ)
			}
			if got != typ {
				t.Fatalf("%q round-tripped to %q", typ, got)
			}
		})
	}
}

func TestNoTwoTypesShareAKindArm(t *testing.T) {
	// Arrange: two types on one arm would make the inverse ambiguous, and the
	// round-trip above would still pass for whichever one the switch reached
	// first.
	seen := map[string]Type{}
	// Act + Assert.
	for _, typ := range AllTypes() {
		arm := kindArmName(kindFor(typ))
		if prior, dup := seen[arm]; dup {
			t.Fatalf("types %q and %q both map to kind arm %s", prior, typ, arm)
		}
		seen[arm] = typ
	}
}

func TestTypeOfRejectsAnUnsetKind(t *testing.T) {
	// Arrange: an unset arm is the ABSENCE of a classification, never a
	// classification of "unknown".
	// Act.
	_, ok := TypeOf(&frontendv1.FailureKind{})
	// Assert.
	if ok {
		t.Fatal("an unset kind arm was reported as a named failure type")
	}
}

func TestTypeOfRejectsAClientLocalKind(t *testing.T) {
	// Arrange: the arms numbered from 100 up are the FRONTEND's own failures
	// about its own transport. The daemon never mints one, and reading one back
	// as a daemon type would launder a frontend's failure through the daemon.
	kind := &frontendv1.FailureKind{
		Kind: &frontendv1.FailureKind_DaemonUnreachable{
			DaemonUnreachable: &frontendv1.FailureDaemonUnreachable{},
		},
	}
	// Act.
	_, ok := TypeOf(kind)
	// Assert.
	if ok {
		t.Fatal("a client-local kind was reported as a daemon failure type")
	}
}

func TestCardOpensFresh(t *testing.T) {
	// Arrange: a fresh card has not settled, and OPEN is an arm rather than a
	// zero timestamp.
	// Act.
	card := Card(TypeShimDegraded, "no traffic")
	// Assert.
	if card.GetOpen() == nil {
		t.Fatalf("lifecycle = %T, want the open arm", card.GetLifecycle())
	}
}

func TestCardCarriesTheProseForItsType(t *testing.T) {
	// Arrange.
	// Act.
	card := Card(TypeShimDegraded, "no traffic")
	// Assert.
	if got, want := card.GetMessage(), prose[TypeShimDegraded]; got != want {
		t.Fatalf("message = %q, want %q", got, want)
	}
}

func TestCardCarriesTheRawAccountVerbatim(t *testing.T) {
	// Arrange: the evidence rode source_detail before and rides detail now;
	// nothing about it is summarized on the way through.
	// Act.
	card := Card(TypeShimDegraded, "no shim traffic for 30s (>20s window)")
	// Assert.
	if got, want := card.GetDetail(), "no shim traffic for 30s (>20s window)"; got != want {
		t.Fatalf("detail = %q, want %q", got, want)
	}
}

func TestCardPanicsForATypeOutsideTheVocabulary(t *testing.T) {
	// Arrange: a card with no kind reaches a renderer as a failure with nothing
	// to draw, so the defect is raised here rather than shipped.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("Card returned for a type outside the vocabulary instead of panicking")
		}
	}()
	// Act.
	Card(Type("not.a.real.type"), "")
}

func TestResolveSettlesTheCardAtItsClosingInstant(t *testing.T) {
	// Arrange.
	card := Card(TypeShimDegraded, "no traffic")
	// Act.
	Resolve(card, 4242)
	// Assert.
	if !IsResolved(card) {
		t.Fatalf("lifecycle = %T, want the resolved arm", card.GetLifecycle())
	}
	if got := ResolvedAtMs(card); got != 4242 {
		t.Fatalf("resolved_at_ms = %d, want 4242", got)
	}
}

func TestIsResolvedIsFalseForAnOpenCard(t *testing.T) {
	// Arrange.
	card := Card(TypeShimDegraded, "no traffic")
	// Act + Assert.
	if IsResolved(card) {
		t.Fatal("a fresh card reported itself resolved")
	}
}

func TestResolvedCardWithAZeroInstantIsStillResolved(t *testing.T) {
	// Arrange: this is exactly the case the magic zero could not express. The
	// arm carries the verdict, so a genuinely-zero closing instant is no longer
	// indistinguishable from an open card.
	card := Card(TypeShimDegraded, "no traffic")
	// Act.
	Resolve(card, 0)
	// Assert.
	if !IsResolved(card) {
		t.Fatal("a resolved card with a zero closing instant reported itself open")
	}
}

func TestToneIsTheVendorColorForAnApiType(t *testing.T) {
	// Arrange + Act + Assert.
	if got := Tone(TypeAPIRateLimit); got != ToneVendor {
		t.Fatalf("tone = %q, want %q", got, ToneVendor)
	}
}

func TestToneIsTheLocalColorForAnInternalType(t *testing.T) {
	// Arrange + Act + Assert.
	if got := Tone(TypeShimDegraded); got != ToneLocal {
		t.Fatalf("tone = %q, want %q", got, ToneLocal)
	}
}

func TestFooterRowCarriesTheCardsOwnSentence(t *testing.T) {
	// Arrange: the row and the card must read the same. Composing a second
	// sentence for the row is how two surfaces come to describe one failure
	// differently.
	card := Card(TypeShimDegraded, "no traffic")
	// Act.
	row := FooterRow(card, "uuid-1")
	// Assert.
	if got, want := row.GetMessage(), card.GetMessage(); got != want {
		t.Fatalf("row message = %q, card message = %q", got, want)
	}
}

func TestFooterRowResolvesTheTone(t *testing.T) {
	// Arrange.
	card := Card(TypeAPIRateLimit, "")
	// Act.
	row := FooterRow(card, "uuid-1")
	// Assert.
	if got := row.GetTone(); got != ToneVendor {
		t.Fatalf("row tone = %q, want %q", got, ToneVendor)
	}
}

func TestFooterRowIsNilForNoCard(t *testing.T) {
	// Arrange: a caller clears the row by passing what it was given.
	// Act.
	row := FooterRow(nil, "uuid-1")
	// Assert.
	if row != nil {
		t.Fatalf("row = %v, want nil", row)
	}
}

func TestTypeNameIsEmptyForAnUnkindedCard(t *testing.T) {
	// Arrange: the absence is the fact, and it is not dressed up as a type
	// nobody classified.
	// Act.
	got := TypeName(&frontendv1.FailureCardView{})
	// Assert.
	if got != "" {
		t.Fatalf("type name = %q, want it empty", got)
	}
}

// kindArmName names a kind's set arm by its Go wrapper type, which is unique
// per arm. It exists so the uniqueness test can key on the arm itself rather
// than on the type the inverse table claims for it.
func kindArmName(kind *frontendv1.FailureKind) string {
	return string(kind.ProtoReflect().WhichOneof(
		kind.ProtoReflect().Descriptor().Oneofs().ByName("kind"),
	).Name())
}

// TestTerminalStampsTheTerminalArm: a card with no closing edge says so through
// its lifecycle arm rather than through its prose.
func TestTerminalStampsTheTerminalArm(t *testing.T) {
	// Arrange.
	card := Card(TypeSessionStartFailed, "gone")

	// Act.
	Terminal(card)

	// Assert.
	if !IsTerminal(card) {
		t.Fatalf("lifecycle = %T, want the terminal arm", card.GetLifecycle())
	}
}

// TestTerminalRefusesToUnsettleAResolvedCard: a failure that already stopped
// being true must never be restated as one that never ends.
func TestTerminalRefusesToUnsettleAResolvedCard(t *testing.T) {
	// Arrange.
	card := Card(TypeShimDegraded, "quiet")
	Resolve(card, 1234)

	// Act.
	Terminal(card)

	// Assert.
	if IsTerminal(card) {
		t.Fatal("a resolved card was overwritten with the terminal arm")
	}
	if ResolvedAtMs(card) != 1234 {
		t.Fatalf("resolved_at_ms = %d, want the original 1234", ResolvedAtMs(card))
	}
}

// TestIsTerminalIsFalseForAFreshCard: a fresh card is OPEN, and an open card
// invites the retry a terminal one forbids.
func TestIsTerminalIsFalseForAFreshCard(t *testing.T) {
	// Arrange / Act.
	card := Card(TypeSessionStartFailed, "gone")

	// Assert.
	if IsTerminal(card) {
		t.Fatal("a fresh card reported itself terminal")
	}
}
