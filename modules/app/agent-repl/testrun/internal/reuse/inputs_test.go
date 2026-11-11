package reuse

import (
	"slices"
	"testing"
)

func TestAddAllPlacesEveryPlacementOfTheOtherSet(t *testing.T) {
	// Arrange
	a, b := NewInputSet(), NewInputSet()
	for _, err := range []error{a.AddFile("x/a.go"), b.AddFile("y/b.go"), b.AddDir("y"), b.AddFlatDir("z"), b.AddTree("t"), b.AddGenerated("g")} {
		if err != nil {
			t.Fatal(err)
		}
	}

	// Act
	a.AddAll(b)

	// Assert
	if !slices.Equal(a.Files(), []string{"x/a.go", "y/b.go"}) || !slices.Equal(a.Dirs(), []string{"y"}) ||
		!slices.Equal(a.FlatDirs(), []string{"z"}) || !slices.Equal(a.Trees(), []string{"t"}) || !slices.Equal(a.Generated(), []string{"g"}) {
		t.Fatalf("set = files %v dirs %v flats %v trees %v generated %v", a.Files(), a.Dirs(), a.FlatDirs(), a.Trees(), a.Generated())
	}
	if len(b.Files()) != 1 {
		t.Fatal("AddAll changed the other set")
	}
}
