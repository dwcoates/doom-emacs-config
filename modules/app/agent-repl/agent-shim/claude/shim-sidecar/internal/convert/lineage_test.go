package convert

import "testing"

func TestALineageHoldsItsRoots(t *testing.T) {
	// Arrange, Act.
	l := newLineage("r1", "r2")

	// Assert.
	if !l.has("r1") || !l.has("r2") {
		t.Fatalf("lineage = %v, want both roots held", l)
	}
}

func TestALineageNeverHoldsTheEmptyUUID(t *testing.T) {
	// Arrange.
	l := newLineage("")

	// Act.
	l.add("")

	// Assert.
	if l.has("") || len(l) != 0 {
		t.Fatalf("lineage = %v, want the empty uuid never held", l)
	}
}

func TestALineageExtendsOnlyThroughAParentItHolds(t *testing.T) {
	tests := []struct {
		name       string
		uuid       string
		parent     string
		wantJoined bool
	}{
		{name: "a child of a root", uuid: "c1", parent: "r1", wantJoined: true},
		{name: "a child of a stranger", uuid: "c1", parent: "elsewhere", wantJoined: false},
		{name: "a record naming no parent", uuid: "c1", parent: "", wantJoined: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			l := newLineage("r1")

			// Act.
			joined := l.extend(tt.uuid, tt.parent)

			// Assert.
			if joined != tt.wantJoined || l.has(tt.uuid) != tt.wantJoined {
				t.Fatalf("extend(%q, %q) = %v, held = %v, want %v", tt.uuid, tt.parent, joined, l.has(tt.uuid), tt.wantJoined)
			}
		})
	}
}

func TestALineageFollowsAChainInFileOrder(t *testing.T) {
	// Arrange.
	l := newLineage("b1")

	// Act.
	l.extend("a1", "b1")
	l.extend("a2", "a1")

	// Assert.
	if !l.has("a2") {
		t.Fatalf("lineage = %v, want the grandchild held", l)
	}
}

// TestEveryDescentTrackerIsALineage pins the shared shape: the keep-alive rule
// and a waiting compaction cut both track descent through the one type, so a
// hand-rolled set at either site fails to compile here.
func TestEveryDescentTrackerIsALineage(t *testing.T) {
	// Arrange, Act.
	var keepalive lineage = newKeepaliveScope().records
	var cut lineage = (&pendingCompaction{descendants: newLineage("b1")}).descendants

	// Assert.
	if keepalive == nil || !cut.has("b1") {
		t.Fatalf("keep-alive records = %v, cut descendants = %v, want both built as lineages", keepalive, cut)
	}
}
