package db

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// observation builds one catalog observation; a test overrides only the field
// its edge case is about.
func observation(hash, kind, structure, example string, seenMs int64) *storev1.ShapeObservation {
	return &storev1.ShapeObservation{
		ShapeHash:    hash,
		Kind:         kind,
		KeyStructure: structure,
		FirstExample: []byte(example),
		SeenMs:       seenMs,
	}
}

// writeShapes commits one shapes-only batch, which the store accepts precisely
// because a catalog observation is a durable contribution of its own.
func writeShapes(t *testing.T, d *DB, shapes ...*storev1.ShapeObservation) WriteResult {
	t.Helper()
	result, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, &storev1.EntryBatch{}, shapes)
	if err != nil {
		t.Fatalf("WriteBatch with %d shape(s): %v", len(shapes), err)
	}
	return result
}

func TestFirstObservationOfAShapeInsertsItWithItsExample(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)

	// Act.
	writeShapes(t, d, observation("h1", "attachment/hook_success", `{a:string}`, `{"a":"x"}`, 1000))

	// Assert.
	if got := scalar[int](t, d, `SELECT count FROM residue_shapes WHERE shape_hash = 'h1'`); got != 1 {
		t.Fatalf("count = %d, want 1 on the first observation", got)
	}
	if got := scalar[string](t, d, `SELECT first_example FROM residue_shapes WHERE shape_hash = 'h1'`); got != `{"a":"x"}` {
		t.Fatalf("first_example = %q, want the raw line verbatim", got)
	}
}

func TestASecondObservationKeepsTheFirstExample(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)
	writeShapes(t, d, observation("h1", "unknown:type:widget", `{a:string}`, `{"a":"first"}`, 1000))

	// Act.
	writeShapes(t, d, observation("h1", "unknown:type:widget", `{a:string}`, `{"a":"second"}`, 2000))

	// Assert.
	if got := scalar[string](t, d, `SELECT first_example FROM residue_shapes WHERE shape_hash = 'h1'`); got != `{"a":"first"}` {
		t.Fatalf("first_example = %q, want the example from the FIRST insert", got)
	}
}

func TestASecondObservationRaisesTheCount(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)
	writeShapes(t, d, observation("h1", "unparsed", `{a:string}`, "raw", 1000))

	// Act.
	writeShapes(t, d, observation("h1", "unparsed", `{a:string}`, "raw", 2000))

	// Assert.
	if got := scalar[int](t, d, `SELECT count FROM residue_shapes WHERE shape_hash = 'h1'`); got != 2 {
		t.Fatalf("count = %d, want 2 after a second observation", got)
	}
}

func TestASecondObservationRaisesLastSeenAndKeepsFirstSeen(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)
	writeShapes(t, d, observation("h1", "unparsed", `{a:string}`, "raw", 1000))

	// Act.
	writeShapes(t, d, observation("h1", "unparsed", `{a:string}`, "raw", 2000))

	// Assert.
	if got := scalar[int64](t, d, `SELECT first_seen_ms FROM residue_shapes WHERE shape_hash = 'h1'`); got != 1000 {
		t.Fatalf("first_seen_ms = %d, want 1000 kept from the insert", got)
	}
	if got := scalar[int64](t, d, `SELECT last_seen_ms FROM residue_shapes WHERE shape_hash = 'h1'`); got != 2000 {
		t.Fatalf("last_seen_ms = %d, want 2000", got)
	}
}

// A REPLAYED BATCH CARRIES AN OLD INSTANT. Taking it unconditionally would walk
// the row's last-seen backwards, which is why the update takes a maximum.
func TestAnOlderObservationDoesNotWalkLastSeenBackwards(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)
	writeShapes(t, d, observation("h1", "unparsed", `{a:string}`, "raw", 5000))

	// Act.
	writeShapes(t, d, observation("h1", "unparsed", `{a:string}`, "raw", 1000))

	// Assert.
	if got := scalar[int64](t, d, `SELECT last_seen_ms FROM residue_shapes WHERE shape_hash = 'h1'`); got != 5000 {
		t.Fatalf("last_seen_ms = %d, want the newer 5000 kept", got)
	}
}

func TestTheCatalogListingFiltersByKind(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)
	writeShapes(t, d,
		observation("h1", "unparsed", `{a:string}`, "raw", 1000),
		observation("h2", "attachment/hook_success", `{b:string}`, "raw", 2000))
	kind := "unparsed"

	// Act.
	rows, err := d.ResidueShapes(ctx(), &kind, 0, false)

	// Assert.
	if err != nil {
		t.Fatalf("ResidueShapes: %v", err)
	}
	if len(rows) != 1 || rows[0].GetShapeHash() != "h1" {
		t.Fatalf("rows = %v, want only the unparsed shape", rows)
	}
}

func TestTheCatalogListingHonorsTheLimit(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)
	writeShapes(t, d,
		observation("h1", "unparsed", `{a:string}`, "raw", 1000),
		observation("h2", "unparsed", `{b:string}`, "raw", 2000),
		observation("h3", "unparsed", `{c:string}`, "raw", 3000))

	// Act.
	rows, err := d.ResidueShapes(ctx(), nil, 2, false)

	// Assert.
	if err != nil {
		t.Fatalf("ResidueShapes: %v", err)
	}
	if len(rows) != 2 {
		t.Fatalf("rows = %d, want the requested 2", len(rows))
	}
}

func TestTheCatalogListingWithholdsTheExampleByDefault(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)
	writeShapes(t, d, observation("h1", "unparsed", `{a:string}`, "the raw line", 1000))

	// Act.
	rows, err := d.ResidueShapes(ctx(), nil, 0, false)

	// Assert.
	if err != nil {
		t.Fatalf("ResidueShapes: %v", err)
	}
	if len(rows) != 1 || len(rows[0].GetFirstExample()) != 0 {
		t.Fatalf("first_example = %q, want it withheld unless asked for", rows[0].GetFirstExample())
	}
}

func TestTheCatalogListingServesTheExampleWhenAsked(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)
	writeShapes(t, d, observation("h1", "unparsed", `{a:string}`, "the raw line", 1000))

	// Act.
	rows, err := d.ResidueShapes(ctx(), nil, 0, true)

	// Assert.
	if err != nil {
		t.Fatalf("ResidueShapes: %v", err)
	}
	if len(rows) != 1 || string(rows[0].GetFirstExample()) != "the raw line" {
		t.Fatalf("first_example = %q, want the stored line", rows[0].GetFirstExample())
	}
}

func TestTheCatalogListingOrdersNewestSeenFirst(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)
	writeShapes(t, d,
		observation("h1", "unparsed", `{a:string}`, "raw", 1000),
		observation("h2", "unparsed", `{b:string}`, "raw", 3000),
		observation("h3", "unparsed", `{c:string}`, "raw", 2000))

	// Act.
	rows, err := d.ResidueShapes(ctx(), nil, 0, false)

	// Assert.
	if err != nil {
		t.Fatalf("ResidueShapes: %v", err)
	}
	if len(rows) != 3 || rows[0].GetShapeHash() != "h2" || rows[2].GetShapeHash() != "h1" {
		t.Fatalf("order = %v, want newest-seen first", rows)
	}
}

func TestAnEmptyKindFilterIsRefusedRatherThanReadAsAbsence(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)
	empty := ""

	// Act.
	_, err := d.ResidueShapes(ctx(), &empty, 0, false)

	// Assert.
	if err == nil {
		t.Fatal("ResidueShapes accepted an empty kind filter, want a refusal")
	}
}

// EVERY OBSERVATION IS VALIDATED BEFORE THE TRANSACTION OPENS, so a malformed
// one refuses the batch whole rather than half-committing the records beside it.
func TestAMalformedObservationRefusesTheWholeBatch(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)
	table := []struct {
		name  string
		shape *storev1.ShapeObservation
	}{
		{"no hash", observation("", "unparsed", `{a:string}`, "raw", 1000)},
		{"no kind", observation("h1", "", `{a:string}`, "raw", 1000)},
		{"no key structure", observation("h1", "unparsed", "", "raw", 1000)},
		{"no seen instant", observation("h1", "unparsed", `{a:string}`, "raw", 0)},
		{"unset observation", nil},
	}

	for _, tc := range table {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			_, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, &storev1.EntryBatch{}, []*storev1.ShapeObservation{tc.shape})

			// Assert.
			if err == nil {
				t.Fatal("WriteBatch accepted the observation, want a refusal")
			}
			if got := scalar[int](t, d, `SELECT COUNT(*) FROM residue_shapes`); got != 0 {
				t.Fatalf("residue_shapes rows = %d, want nothing committed", got)
			}
		})
	}
}

// A BATCH CARRYING ONLY SHAPES IS A REAL WRITE. The observations are the whole
// point of the request, so refusing it as empty would drop them.
func TestABatchOfNothingButShapesIsAccepted(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)

	// Act.
	result := writeShapes(t, d, observation("h1", "unparsed", `{a:string}`, "raw", 1000))

	// Assert.
	if result.Shapes != 1 {
		t.Fatalf("result.Shapes = %d, want 1", result.Shapes)
	}
}

func TestABatchOfNothingAtAllIsStillRefused(t *testing.T) {
	// Arrange.
	d, _ := newStore(t)

	// Act.
	_, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, &storev1.EntryBatch{}, nil)

	// Assert.
	if err == nil {
		t.Fatal("WriteBatch accepted a batch with neither entries, cursor nor shapes")
	}
}
