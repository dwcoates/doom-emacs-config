package wsm

import (
	"context"
	"errors"
	"regexp"
	"strings"
	"testing"

	"agent-repld/internal/sourcescan"
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
	handRolled := regexp.MustCompile(`for rows\.Next\(\) \{\s*\w+, err := scan[A-Z]\w*\(rows\)`)

	// Act
	var offenders []string
	for _, file := range sourcescan.Production(t) {
		if handRolled.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v decode rows in a hand-rolled loop; use listRows", offenders)
	}
}

// TestTheWorkspaceReadsLookUpTheirDerivedColumnsByKey pins the plan of the
// select every workspace read shares: each derived column is a keyed SEARCH,
// never a SCAN of a table inside the per-row subquery. The queue column once
// scanned the registry per queue row per workspace, and a list of ~800 held
// the store's one connection for over a second (2026-10-24).
func TestTheWorkspaceReadsLookUpTheirDerivedColumnsByKey(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	rows, err := s.db().QueryContext(context.Background(), `EXPLAIN QUERY PLAN SELECT `+workspaceColumns+` FROM workspaces ORDER BY created_at, id`)
	if err != nil {
		t.Fatalf("EXPLAIN: %v", err)
	}
	defer rows.Close()
	var plan []string
	for rows.Next() {
		var id, parent, unused int
		var detail string
		if err := rows.Scan(&id, &parent, &unused, &detail); err != nil {
			t.Fatalf("scan plan: %v", err)
		}
		plan = append(plan, detail)
	}
	if err := rows.Err(); err != nil {
		t.Fatalf("plan rows: %v", err)
	}

	// Assert: the outer SCAN of workspaces is the read itself; nothing under
	// it scans a table. The root subsection's recursive walk scans `up`, its
	// own few rows of ancestors, which is no table.
	scans := 0
	for _, step := range plan {
		if strings.HasPrefix(step, "SCAN ") && step != "SCAN up" {
			scans++
		}
	}
	if scans != 1 {
		t.Fatalf("plan scans %d tables, want only the outer workspaces read; plan:\n%s", scans, strings.Join(plan, "\n"))
	}
}
