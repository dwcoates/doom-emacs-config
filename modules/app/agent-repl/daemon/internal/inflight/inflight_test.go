package inflight

import (
	"errors"
	"strings"
	"testing"
)

// TestUnansweredZeroValueIsNotEmpty pins the single most important property:
// a Set nobody filled in must never read as "nothing is running".
func TestUnansweredZeroValueIsNotEmpty(t *testing.T) {
	// Arrange
	var set Set
	// Act
	blocked, why := set.Blocks()
	// Assert
	if !blocked {
		t.Fatalf("Blocks() = false for the zero value; an unfilled set must block, why=%q", why)
	}
}

// TestUnansweredBlocksAndNamesItsReason covers the explicit unknown arm.
func TestUnansweredBlocksAndNamesItsReason(t *testing.T) {
	// Arrange
	set := Unanswered("/w", "the shim never answered the live-task query")
	// Act
	blocked, why := set.Blocks()
	// Assert
	if !blocked {
		t.Fatal("Blocks() = false for an unanswered set")
	}
	if !strings.Contains(why, "UNKNOWN") || !strings.Contains(why, "never answered") {
		t.Fatalf("Blocks() why = %q, want it to name UNKNOWN and carry the reason", why)
	}
}

// TestUnansweredWithNoReasonStillRecordsTheOmission covers the reason being
// mandatory: a blank one is replaced by an account of the omission rather than
// silently accepted.
func TestUnansweredWithNoReasonStillRecordsTheOmission(t *testing.T) {
	// Arrange / Act
	set := Unanswered("/w", "   ")
	// Assert
	if !strings.Contains(set.Reason(), "NO REASON WAS RECORDED") {
		t.Fatalf("Reason() = %q, want the recorded-omission sentence", set.Reason())
	}
}

// TestAnsweredEmptyDoesNotBlock covers the one arm that licenses a teardown.
func TestAnsweredEmptyDoesNotBlock(t *testing.T) {
	// Arrange
	set, err := Answered("/w")
	if err != nil {
		t.Fatalf("Answered: %v", err)
	}
	// Act
	blocked, why := set.Blocks()
	// Assert
	if blocked {
		t.Fatalf("Blocks() = true for an answered empty set, why=%q", why)
	}
}

// TestAnsweredWithMembersBlocks covers a workspace holding real work.
func TestAnsweredWithMembersBlocks(t *testing.T) {
	// Arrange
	set, err := Answered("/w", Item{Kind: KindTask, ID: "task-1"})
	if err != nil {
		t.Fatalf("Answered: %v", err)
	}
	// Act
	blocked, why := set.Blocks()
	// Assert
	if !blocked {
		t.Fatal("Blocks() = false while a live task is held")
	}
	if !strings.Contains(why, "task:task-1") {
		t.Fatalf("Blocks() why = %q, want the identity named, not a count alone", why)
	}
}

// TestAnsweredRefusesAnUnidentifiedItem pins the no-counting rule at
// construction: an item with no identity could not be recognised after a
// bounce, so it may not be a member.
func TestAnsweredRefusesAnUnidentifiedItem(t *testing.T) {
	// Arrange / Act
	_, err := Answered("/w", Item{Kind: KindTurn, ID: "  "})
	// Assert
	if !errors.Is(err, ErrItemUnidentified) {
		t.Fatalf("Answered err = %v, want ErrItemUnidentified", err)
	}
}

// TestAnsweredRefusesAnUnknownKind covers the closed vocabulary.
func TestAnsweredRefusesAnUnknownKind(t *testing.T) {
	// Arrange / Act
	_, err := Answered("/w", Item{Kind: Kind("compaction"), ID: "c-1"})
	// Assert
	if !errors.Is(err, ErrItemKindUnknown) {
		t.Fatalf("Answered err = %v, want ErrItemKindUnknown", err)
	}
}

// TestAnsweredDeduplicatesByIdentity covers two reports of one item.
func TestAnsweredDeduplicatesByIdentity(t *testing.T) {
	// Arrange / Act
	set, err := Answered("/w",
		Item{Kind: KindTask, ID: "t", Detail: "first"},
		Item{Kind: KindTask, ID: "t", Detail: "second"})
	if err != nil {
		t.Fatalf("Answered: %v", err)
	}
	// Assert
	if got := len(set.Items()); got != 1 {
		t.Fatalf("len(Items()) = %d, want 1", got)
	}
}

// TestAnsweredSeparatesKindsSharingAnID covers a turn and a task that happen to
// carry the same id string: they are two items, because the key is kind+id.
func TestAnsweredSeparatesKindsSharingAnID(t *testing.T) {
	// Arrange / Act
	set, err := Answered("/w",
		Item{Kind: KindTask, ID: "x"},
		Item{Kind: KindTurn, ID: "x"})
	if err != nil {
		t.Fatalf("Answered: %v", err)
	}
	// Assert
	if got := len(set.Items()); got != 2 {
		t.Fatalf("len(Items()) = %d, want 2", got)
	}
}

// TestItemsIsACopy pins that a caller cannot edit the authority.
func TestItemsIsACopy(t *testing.T) {
	// Arrange
	set := MustAnswered("/w", Item{Kind: KindTurn, ID: "turn-1"})
	// Act
	got := set.Items()
	got[0].ID = "mutated"
	// Assert
	if set.Items()[0].ID != "turn-1" {
		t.Fatalf("Items() handed out the backing array; set now reads %q", set.Items()[0].ID)
	}
}

// TestHasIsFalseOnTheUnknownArm pins that "I cannot say" is not "yes" either.
func TestHasIsFalseOnTheUnknownArm(t *testing.T) {
	// Arrange
	set := Unanswered("/w", "no answer")
	// Act / Assert
	if set.Has(Item{Kind: KindTurn, ID: "turn-1"}) {
		t.Fatal("Has() = true on an unanswered set")
	}
}

// TestSettledProofOnlyComesFromAnAnsweredEmptySet is the structural half: the
// three inputs that must NOT yield a settledness proof.
func TestSettledProofRequiresAnAnsweredEmptySet(t *testing.T) {
	tests := []struct {
		name string
		set  Set
		want bool
	}{
		{name: "answered and empty proves settledness", set: MustAnswered("/w"), want: true},
		{name: "answered with a member proves nothing", set: MustAnswered("/w", Item{Kind: KindQuery, ID: "q"}), want: false},
		{name: "unanswered proves nothing", set: Unanswered("/w", "silent"), want: false},
		{name: "the zero value proves nothing", set: Set{}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			proof, ok := tt.set.Settled()
			// Assert
			if ok != tt.want {
				t.Fatalf("Settled() ok = %v, want %v", ok, tt.want)
			}
			if proof.Proven() != tt.want {
				t.Fatalf("Settled() proof.Proven() = %v, want %v", proof.Proven(), tt.want)
			}
		})
	}
}

// TestSettledProofCarriesItsWorkspace covers a gate comparing the proof's
// workspace against the one it is about to tear down.
func TestSettledProofCarriesItsWorkspace(t *testing.T) {
	// Arrange
	set := MustAnswered("/ws/a")
	// Act
	proof, ok := set.Settled()
	// Assert
	if !ok || proof.Workspace() != "/ws/a" {
		t.Fatalf("proof workspace = %q ok=%v, want /ws/a true", proof.Workspace(), ok)
	}
}

// TestUnionOfAnsweredPartsMerges covers the ordinary composition.
func TestUnionOfAnsweredPartsMerges(t *testing.T) {
	// Arrange
	turns := MustAnswered("/w", Item{Kind: KindTurn, ID: "turn-1"})
	tasks := MustAnswered("/w", Item{Kind: KindTask, ID: "task-1"})
	// Act
	got := Union("/w", turns, tasks)
	// Assert
	if !got.Known() || len(got.Items()) != 2 {
		t.Fatalf("Union = %s, want two known members", got.Summary())
	}
}

// TestUnionIsUnknownWhenAnyPartIs pins that a partial answer poisons the whole
// rather than understating what is running.
func TestUnionIsUnknownWhenAnyPartIs(t *testing.T) {
	// Arrange
	turns := MustAnswered("/w", Item{Kind: KindTurn, ID: "turn-1"})
	tasks := Unanswered("/w", "the shim omitted live_task_set")
	// Act
	got := Union("/w", turns, tasks)
	// Assert
	if got.Known() {
		t.Fatalf("Union = %s, want UNKNOWN when a component is unknown", got.Summary())
	}
	if !strings.Contains(got.Reason(), "omitted live_task_set") {
		t.Fatalf("Union reason = %q, want the failing component's own reason carried forward", got.Reason())
	}
}

// TestSummaryNamesIdentitiesNotOnlyACount covers the log-line contract.
func TestSummaryNamesIdentitiesNotOnlyACount(t *testing.T) {
	// Arrange
	set := MustAnswered("/w",
		Item{Kind: KindTask, ID: "task-1", Detail: "shell"},
		Item{Kind: KindQuery, ID: "q-9"})
	// Act
	got := set.Summary()
	// Assert
	for _, want := range []string{"task:task-1", "query:q-9", "shell"} {
		if !strings.Contains(got, want) {
			t.Fatalf("Summary() = %q, want it to contain %q", got, want)
		}
	}
}

// TestOfKindFiltersToOnePlane covers the per-plane read the manifest uses.
func TestOfKindFiltersToOnePlane(t *testing.T) {
	// Arrange
	set := MustAnswered("/w",
		Item{Kind: KindTask, ID: "a"},
		Item{Kind: KindTurn, ID: "b"},
		Item{Kind: KindTask, ID: "c"})
	// Act
	got := set.OfKind(KindTask)
	// Assert
	if len(got) != 2 {
		t.Fatalf("OfKind(task) = %v, want two members", got)
	}
}

// TestEqualSeparatesAnEmptyAnswerFromAnUnknownOne pins the distinction the
// drain hold depends on: a workspace with nothing running is not the same
// answer as a workspace nobody could answer for.
func TestEqualSeparatesAnEmptyAnswerFromAnUnknownOne(t *testing.T) {
	// Arrange
	empty := MustAnswered("/w")
	unknown := Unanswered("/w", "nobody could say")
	// Act / Assert
	if empty.Equal(unknown) {
		t.Fatal("Equal() = true for an answered-empty set against an unanswered one")
	}
}

// TestEqualIsTrueForTheSameIdentities covers the no-change compare a drain
// re-read makes.
func TestEqualIsTrueForTheSameIdentities(t *testing.T) {
	// Arrange
	a := MustAnswered("/w", Item{Kind: KindTask, ID: "t1", Detail: "first look"})
	b := MustAnswered("/w", Item{Kind: KindTask, ID: "t1", Detail: "second look"})
	// Act / Assert
	if !a.Equal(b) {
		t.Fatal("Equal() = false for the same identity under a changed detail")
	}
}

// TestEqualIsFalseWhenTheMembersWereReplaced is the lesson a count cannot
// teach: same size, different things running.
func TestEqualIsFalseWhenTheMembersWereReplaced(t *testing.T) {
	// Arrange
	before := MustAnswered("/w", Item{Kind: KindTask, ID: "t1"})
	after := MustAnswered("/w", Item{Kind: KindTask, ID: "t2"})
	// Act / Assert
	if before.Equal(after) {
		t.Fatal("Equal() = true across a replaced member; the count matched but the identity did not")
	}
}

// TestEqualIsFalseAcrossWorkspaces covers one workspace's answer being read as
// another's.
func TestEqualIsFalseAcrossWorkspaces(t *testing.T) {
	// Arrange / Act / Assert
	if MustAnswered("/ws/a").Equal(MustAnswered("/ws/b")) {
		t.Fatal("Equal() = true across two different workspaces")
	}
}
