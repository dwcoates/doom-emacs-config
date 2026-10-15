package wsm

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

func TestListRowsDecodesEveryRowInOrder(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	registerIn(t, s, arrangeRepo(t, "beta"), "b", nil)
	registerIn(t, s, arrangeRepo(t, "alpha"), "a", nil)

	// Act
	got, err := listRows(context.Background(), s.db(), `SELECT `+repositoryColumns+` FROM repositories ORDER BY name`, scanRepository)

	// Assert
	if err != nil {
		t.Fatalf("listRows: %v", err)
	}
	if len(got) != 2 || got[0].Name != "alpha" || got[1].Name != "beta" {
		t.Fatalf("rows = %+v, want alpha then beta", got)
	}
}

func TestListRowsAnswersNothingForNoRows(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := listRows(context.Background(), s.db(), `SELECT `+repositoryColumns+` FROM repositories`, scanRepository)

	// Assert
	if err != nil || got != nil {
		t.Fatalf("listRows = (%v, %v), want (nil, nil)", got, err)
	}
}

func TestListRowsFailsWholeWhenOneRowDoesNotDecode(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	registerIn(t, s, arrangeRepo(t, "one"), "a", nil)
	registerIn(t, s, arrangeRepo(t, "two"), "b", nil)
	broken := errors.New("this row does not decode")
	calls := 0
	scan := func(row interface{ Scan(...any) error }) (Repository, error) {
		calls++
		repo, err := scanRepository(row)
		if calls == 2 {
			return Repository{}, broken
		}
		return repo, err
	}

	// Act
	got, err := listRows(context.Background(), s.db(), `SELECT `+repositoryColumns+` FROM repositories`, scan)

	// Assert
	if !errors.Is(err, broken) || got != nil {
		t.Fatalf("listRows = (%v, %v), want (nil, the decode failure)", got, err)
	}
}

func TestListRowsFailsAQueryThatCannotRun(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := listRows(context.Background(), s.db(), `SELECT nothing FROM nowhere`, scanRepository)

	// Assert
	if err == nil {
		t.Fatal("listRows ran a query against a table that does not exist")
	}
}

// TestRowDecodersListThroughListRows keeps every many-row read that decodes
// with a scan helper on the one loop: a hand-rolled `for rows.Next()` that
// calls a scanX(rows) decoder is a second copy of listRows to drift.
func TestRowDecodersListThroughListRows(t *testing.T) {
	// Arrange
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	handRolled := regexp.MustCompile(`for rows\.Next\(\) \{\s*\w+, err := scan[A-Z]\w*\(rows\)`)

	// Act
	var offenders []string
	for _, file := range files {
		if strings.HasSuffix(file, "_test.go") {
			continue
		}
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		if handRolled.Match(src) {
			offenders = append(offenders, file)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v decode rows in a hand-rolled loop; use listRows", offenders)
	}
}
