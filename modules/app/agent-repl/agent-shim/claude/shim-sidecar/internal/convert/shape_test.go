package convert

import (
	"encoding/json"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/types/known/structpb"
)

// decode renders a JSON literal the way the converter's own decoder would, so a
// case reads as the vendor's line rather than as a Go literal.
func decodeAny(t *testing.T, literal string) any {
	t.Helper()
	var value any
	if err := json.Unmarshal([]byte(literal), &value); err != nil {
		t.Fatalf("decoding %s: %v", literal, err)
	}
	return value
}

func TestKeyStructureRendersScalarsAsTheirTypeNotTheirValue(t *testing.T) {
	// Arrange.
	table := []struct {
		name string
		json string
		want string
	}{
		{"string", `{"a":"hello"}`, `{a:string}`},
		{"number", `{"a":42}`, `{a:number}`},
		{"bool", `{"a":true}`, `{a:bool}`},
		{"null", `{"a":null}`, `{a:null}`},
	}

	for _, tc := range table {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := KeyStructure(decodeAny(t, tc.json))

			// Assert.
			if got != tc.want {
				t.Fatalf("KeyStructure = %q, want %q", got, tc.want)
			}
		})
	}
}

// TWO LINES THAT DIFFER ONLY IN A VALUE ARE ONE SHAPE. That is the whole reason
// the catalog is bounded by the vendor's API rather than by traffic.
func TestTwoLinesDifferingOnlyInValuesShareAShape(t *testing.T) {
	// Arrange.
	first := decodeAny(t, `{"a":"hello","b":1}`)
	second := decodeAny(t, `{"a":"goodbye","b":9999}`)

	// Act.
	got, want := KeyStructure(first), KeyStructure(second)

	// Assert.
	if got != want {
		t.Fatalf("KeyStructure = %q and %q, want one shape for two values", got, want)
	}
}

// THE RENDERING SORTS ITS KEYS, so the shape does not depend on the order the
// vendor happened to serialize them in.
func TestKeyStructureIsIndependentOfKeyOrder(t *testing.T) {
	// Arrange.
	first := decodeAny(t, `{"z":"x","a":"x","m":"x"}`)
	second := decodeAny(t, `{"m":"x","z":"x","a":"x"}`)

	// Act.
	got, want := KeyStructure(first), KeyStructure(second)

	// Assert.
	if got != want || got != `{a:string,m:string,z:string}` {
		t.Fatalf("KeyStructure = %q and %q, want the sorted rendering for both", got, want)
	}
}

// AN ARRAY CONTRIBUTES ONE MERGED ELEMENT SHAPE: the union of its elements'
// keys, so a list of ten near-identical blocks is one shape and not ten.
func TestAnArrayMergesItsElementsIntoOneShape(t *testing.T) {
	// Arrange.
	value := decodeAny(t, `{"content":[{"type":"text","text":"a"},{"type":"tool_use","id":"x"}]}`)

	// Act.
	got := KeyStructure(value)

	// Assert.
	if got != `{content:[{id:string,text:string,type:string}]}` {
		t.Fatalf("KeyStructure = %q, want the merged element shape", got)
	}
}

func TestAnEmptyArrayRendersAsAnEmptyElement(t *testing.T) {
	// Arrange.
	value := decodeAny(t, `{"content":[]}`)

	// Act.
	got := KeyStructure(value)

	// Assert.
	if got != `{content:[]}` {
		t.Fatalf("KeyStructure = %q, want an empty element", got)
	}
}

// A POSITION THAT TOOK SEVERAL FORMS RENDERS THEM ALL, sorted and joined, so a
// union is a fact the catalog states rather than one it picks a winner from.
func TestAPositionThatTookTwoFormsRendersBoth(t *testing.T) {
	// Arrange.
	value := decodeAny(t, `{"a":["x",1]}`)

	// Act.
	got := KeyStructure(value)

	// Assert.
	if got != `{a:[number|string]}` {
		t.Fatalf("KeyStructure = %q, want both scalar forms", got)
	}
}

func TestNestingRecursesAllTheWayDown(t *testing.T) {
	// Arrange.
	value := decodeAny(t, `{"a":{"b":{"c":{"d":"deep"}}}}`)

	// Act.
	got := KeyStructure(value)

	// Assert.
	if got != `{a:{b:{c:{d:string}}}}` {
		t.Fatalf("KeyStructure = %q, want the whole nesting", got)
	}
}

// A MAP KEYED BY ID MUST NOT EXPLODE INTO ONE SHAPE PER ID. Every clause of the
// generated-id rule gets its own case, and each ALSO asserts the wildcard's
// value shape survives — a key replaced is not a subtree dropped.
func TestAGeneratedIdKeyBecomesTheWildcard(t *testing.T) {
	// Arrange.
	table := []struct {
		name string
		json string
	}{
		{"uuid", `{"3f2504e0-4f89-11d3-9a0c-0305e82c3301":{"n":1}}`},
		{"tool use id", `{"toolu_01ABCdefGHIjklMNO":{"n":1}}`},
		{"message id", `{"msg_01ABCdefGHIjklMNO":{"n":1}}`},
		{"long hex run", `{"a1b2c3d4e5f60718":{"n":1}}`},
		{"pure digits", `{"12345":{"n":1}}`},
	}

	for _, tc := range table {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := KeyStructure(decodeAny(t, tc.json))

			// Assert.
			if got != `{*:{n:number}}` {
				t.Fatalf("KeyStructure = %q, want the id wildcarded and its shape kept", got)
			}
		})
	}
}

// THE RULE IS NARROW ON PURPOSE: a vocabulary key that merely looks id-ish is a
// name the catalog must keep, or the shape stops describing the vendor's API.
func TestAVocabularyKeyIsNeverWildcarded(t *testing.T) {
	// Arrange.
	table := []struct {
		name string
		json string
		want string
	}{
		{"a short hex-looking word", `{"deadbeef":1}`, `{deadbeef:number}`},
		{"a bare prefix with no id", `{"toolu_":1}`, `{toolu_:number}`},
		{"an ordinary name", `{"sessionId":1}`, `{sessionId:number}`},
		{"a uuid missing a group", `{"3f2504e0-4f89-11d3-9a0c":1}`, `{3f2504e0-4f89-11d3-9a0c:number}`},
	}

	for _, tc := range table {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := KeyStructure(decodeAny(t, tc.json))

			// Assert.
			if got != tc.want {
				t.Fatalf("KeyStructure = %q, want %q", got, tc.want)
			}
		})
	}
}

// TWO ID KEYS IN ONE MAP MERGE UNDER THE ONE WILDCARD, exactly as an array's
// elements do, so an id-keyed map of a thousand entries is still one shape.
func TestTwoIdKeysInOneMapMergeUnderTheWildcard(t *testing.T) {
	// Arrange.
	value := decodeAny(t, `{"toolu_01A":{"a":1},"toolu_01B":{"b":"x"}}`)

	// Act.
	got := KeyStructure(value)

	// Assert.
	if got != `{*:{a:number,b:string}}` {
		t.Fatalf("KeyStructure = %q, want the two id entries merged", got)
	}
}

func TestTheShapeHashIsSha256OverTheRendering(t *testing.T) {
	// Arrange. A known rendering, so a change to the digest is a visible break.
	structure := `{a:string}`

	// Act.
	got := ShapeHash(structure)

	// Assert.
	if got != "88f14f654473ae6daf9e3e8710a042bf1c77d421bad47d2ee0ef0a1be4a08bf9" {
		t.Fatalf("ShapeHash = %q, want SHA-256 of the rendering in lowercase hex", got)
	}
}

func TestTwoDifferentRenderingsHashDifferently(t *testing.T) {
	// Arrange, Act.
	first, second := ShapeHash(`{a:string}`), ShapeHash(`{a:number}`)

	// Assert.
	if first == second {
		t.Fatal("two renderings collided; the catalog would merge two shapes into one row")
	}
}

// ---- the observation a withheld entry contributes ----

// vendorEntry builds one vendor_specific residue entry over a JSON literal.
func vendorEntry(t *testing.T, kind, literal string) *storev1.StoreEntry {
	t.Helper()
	var raw map[string]any
	if err := json.Unmarshal([]byte(literal), &raw); err != nil {
		t.Fatalf("decoding %s: %v", literal, err)
	}
	s, err := structpb.NewStruct(raw)
	if err != nil {
		t.Fatalf("structpb.NewStruct: %v", err)
	}
	return &storev1.StoreEntry{Entry: &storev1.StoreEntry_AgentUpdate{
		AgentUpdate: &storev1.StoreAgentUpdate{AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{
			UnservedItem: &storev1.StoreUnservedItem{
				UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{
					VendorSpecific: &storev1.StoreVendorSpecific{Kind: kind, Raw: s},
				},
			},
		}},
	}}
}

func TestResidueShapeNamesTheWithheldTallysOwnLabel(t *testing.T) {
	// Arrange. The catalog's kind and the log's label must be one string, or a
	// `--kind` filter matches nothing the summary said.
	entry := vendorEntry(t, "hook_success", `{"a":"x"}`)

	// Act.
	got, ok := ResidueShape(entry, 1000)

	// Assert.
	if !ok || got.GetKind() != ResidueLabel(entry) {
		t.Fatalf("kind = %q, want the residue label %q", got.GetKind(), ResidueLabel(entry))
	}
}

func TestResidueShapeCarriesTheStructureAndItsHash(t *testing.T) {
	// Arrange.
	entry := vendorEntry(t, "hook_success", `{"a":"x"}`)

	// Act.
	got, ok := ResidueShape(entry, 1000)

	// Assert.
	if !ok || got.GetKeyStructure() != `{a:string}` || got.GetShapeHash() != ShapeHash(`{a:string}`) {
		t.Fatalf("observation = %v, want the rendering and its hash", got)
	}
}

func TestResidueShapeCarriesTheObserversInstant(t *testing.T) {
	// Arrange. first_seen and last_seen come from the OBSERVER's clock.
	entry := vendorEntry(t, "hook_success", `{"a":"x"}`)

	// Act.
	got, _ := ResidueShape(entry, 1700000000000)

	// Assert.
	if got.GetSeenMs() != 1700000000000 {
		t.Fatalf("seen_ms = %d, want the caller's instant", got.GetSeenMs())
	}
}

func TestResidueShapeCarriesTheExample(t *testing.T) {
	// Arrange. One readable line per shape is what makes the catalog actionable.
	entry := vendorEntry(t, "hook_success", `{"a":"x"}`)

	// Act.
	got, _ := ResidueShape(entry, 1000)

	// Assert.
	if string(got.GetFirstExample()) != `{"a":"x"}` {
		t.Fatalf("first_example = %q, want the payload as compact JSON", got.GetFirstExample())
	}
}

// BYTES THAT ARE NOT JSON HAVE NO KEY STRUCTURE and share one row, whose
// example says why the parse failed.
func TestUnparsableBytesShareTheOneUnparsableShape(t *testing.T) {
	// Arrange.
	entry := &storev1.StoreEntry{Entry: &storev1.StoreEntry_AgentUpdate{
		AgentUpdate: &storev1.StoreAgentUpdate{AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{
			UnservedItem: &storev1.StoreUnservedItem{
				UnservedItem: &storev1.StoreUnservedItem_Unparsed{
					Unparsed: &storev1.StoreUnparsed{Raw: "not json at all"},
				},
			},
		}},
	}}

	// Act.
	got, ok := ResidueShape(entry, 1000)

	// Assert.
	if !ok || got.GetKeyStructure() != UnparsableStructure {
		t.Fatalf("key_structure = %q, want %q", got.GetKeyStructure(), UnparsableStructure)
	}
	if string(got.GetFirstExample()) != "not json at all" {
		t.Fatalf("first_example = %q, want the bytes that failed to parse", got.GetFirstExample())
	}
}

// A TYPED ENTRY CONTRIBUTES NOTHING. The catalog is about what was NOT stored.
func TestATypedEntryContributesNoObservation(t *testing.T) {
	// Arrange.
	entry := &storev1.StoreEntry{Entry: &storev1.StoreEntry_AgentUpdate{
		AgentUpdate: &storev1.StoreAgentUpdate{AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{
			ServeableFrame: &storev1.StorePageLine{},
		}},
	}}

	// Act.
	_, ok := ResidueShape(entry, 1000)

	// Assert.
	if ok {
		t.Fatal("a typed entry produced a shape observation; the catalog is what was NOT stored")
	}
}
