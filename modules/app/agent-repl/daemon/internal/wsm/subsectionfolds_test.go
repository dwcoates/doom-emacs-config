package wsm

import (
	"context"
	"database/sql"
	"errors"
	"strings"
	"testing"

	"agent-repld/internal/sourcescan"
)

// subsectionNow reads subsection ID back through the arrangement.
func subsectionNow(t *testing.T, s *store, id SubsectionID) Subsection {
	t.Helper()
	arrangement, err := s.ReadArrangement(context.Background())
	if err != nil {
		t.Fatalf("ReadArrangement: %v", err)
	}
	for _, sub := range arrangement.Subsections {
		if sub.ID == id {
			return sub
		}
	}
	t.Fatalf("subsection %s is not in the arrangement", id)
	return Subsection{}
}

func TestSetSubsectionFoldedRecordsTheFold(t *testing.T) {
	for _, folded := range []bool{true, false} {
		t.Run(map[bool]string{true: "collapsed", false: "expanded"}[folded], func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
			sub := mustSubsection(t, s, ws.Repo, "later")
			if err := s.SetSubsectionFolded(context.Background(), sub.ID, !folded); err != nil {
				t.Fatalf("SetSubsectionFolded: %v", err)
			}

			// Act
			err := s.SetSubsectionFolded(context.Background(), sub.ID, folded)

			// Assert
			if err != nil {
				t.Fatalf("SetSubsectionFolded: %v", err)
			}
			if got := subsectionNow(t, s, sub.ID).Folded; got != folded {
				t.Fatalf("folded = %v, want %v", got, folded)
			}
		})
	}
}

func TestANewSubsectionIsExpanded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)

	// Act
	sub := mustSubsection(t, s, ws.Repo, "later")

	// Assert
	if subsectionNow(t, s, sub.ID).Folded {
		t.Fatal("a new subsection is folded")
	}
}

func TestSetSubsectionFoldedRefusesAnUnknownSubsection(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetSubsectionFolded(context.Background(), "absent", true)

	// Assert
	var unknown *UnknownSubsectionError
	if !errors.As(err, &unknown) || unknown.ID != "absent" || !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetSubsectionFolded = %v, want an UnknownSubsectionError naming absent", err)
	}
}

func TestExpandSubsectionHolding(t *testing.T) {
	tests := []struct {
		name string
		pick func(selectionFoldWorld) Workspace
		want bool
	}{
		{name: "a root expands its collapsed subsection", pick: func(w selectionFoldWorld) Workspace { return w.root }, want: true},
		{name: "a child expands its family's subsection", pick: func(w selectionFoldWorld) Workspace { return w.child }, want: true},
		{name: "a workspace in no subsection expands nothing", pick: func(w selectionFoldWorld) Workspace { return w.other }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			w := newSelectionFoldWorld(t, s)
			ws := tt.pick(w)

			// Act
			got, err := s.ExpandSubsectionHolding(context.Background(), ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("ExpandSubsectionHolding: %v", err)
			}
			if got != tt.want {
				t.Errorf("expanded = %v, want %v", got, tt.want)
			}
			if folded := subsectionNow(t, s, w.sub.ID).Folded; folded != (tt.want == false) {
				t.Errorf("subsection folded = %v, want %v", folded, tt.want == false)
			}
			if !repositoryFolded(t, s, ws.Repo) {
				t.Error("the repository's own fold was expanded")
			}
		})
	}
}

func TestExpandSubsectionHoldingOfAnOpenSubsectionTouchesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newSelectionFoldWorld(t, s)
	if err := s.SetSubsectionFolded(context.Background(), w.sub.ID, false); err != nil {
		t.Fatalf("SetSubsectionFolded: %v", err)
	}

	// Act
	got, err := s.ExpandSubsectionHolding(context.Background(), w.root.ID)

	// Assert
	if err != nil || got {
		t.Fatalf("ExpandSubsectionHolding = %v, %v; want false, nil", got, err)
	}
}

func TestExpandSubsection(t *testing.T) {
	tests := []struct {
		name       string
		folded     bool
		id         func(Subsection) SubsectionID
		want       bool
		wantFolded bool
	}{
		{name: "a collapsed subsection is expanded", folded: true, id: func(sub Subsection) SubsectionID { return sub.ID }, want: true},
		{name: "an open subsection is left open", id: func(sub Subsection) SubsectionID { return sub.ID }},
		{name: "an unknown subsection touches nothing", folded: true, id: func(Subsection) SubsectionID { return "absent" }, wantFolded: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
			sub := mustSubsection(t, s, ws.Repo, "later")
			if err := s.SetSubsectionFolded(context.Background(), sub.ID, tt.folded); err != nil {
				t.Fatalf("SetSubsectionFolded: %v", err)
			}

			// Act
			var got bool
			err := s.write(context.Background(), "test.expand_subsection", nil, func(ctx context.Context, tx *sql.Tx) error {
				var err error
				got, err = expandSubsection(ctx, tx, tt.id(sub))
				return err
			})

			// Assert
			if err != nil {
				t.Fatalf("expandSubsection: %v", err)
			}
			if got != tt.want {
				t.Errorf("expanded = %v, want %v", got, tt.want)
			}
			if folded := subsectionNow(t, s, sub.ID).Folded; folded != tt.wantFolded {
				t.Errorf("subsection folded = %v, want %v", folded, tt.wantFolded)
			}
		})
	}
}

// TestEverySubsectionExpansionAsksExpandSubsection pins that no store write
// hand-rolls a subsection's expansion: a placement, a move, a selection and
// a turn's start all expand through expandSubsection.
func TestEverySubsectionExpansionAsksExpandSubsection(t *testing.T) {
	// Arrange
	const expansion = "UPDATE subsections SET folded = 0"

	for _, file := range sourcescan.Production(t) {
		// Act
		n := strings.Count(string(file.Source), expansion)

		// Assert
		if want := map[bool]int{true: 1, false: 0}[strings.HasSuffix(file.Name, "subsectionfolds.go")]; n != want {
			t.Errorf("%s states the subsection expansion %d times, want %d", file.Name, n, want)
		}
	}
}
