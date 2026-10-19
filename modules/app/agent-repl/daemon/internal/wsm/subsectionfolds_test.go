package wsm

import (
	"context"
	"errors"
	"testing"
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
