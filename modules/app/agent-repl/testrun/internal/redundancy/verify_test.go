package redundancy

import (
	"reflect"
	"strings"
	"testing"
)

func matrixFixture() *Measurement {
	return &Measurement{
		Cases: []Case{
			{ID: "a.test.ts:keep", Cost: 1 * ms},
			{ID: "a.test.ts:del cheap", Cost: 1 * ms},
			{ID: "a.test.ts:del dear", Cost: 9 * ms},
			{ID: "b.test.ts:pinned", Cost: 1 * ms, Pinned: "asserts a log record"},
		},
		Mutants: []MutantResult{
			// Killed by a kept case: settled.
			{Key: "m1", KilledBy: []string{"a.test.ts:keep", "a.test.ts:del dear"}, CoveredBy: []string{"a.test.ts:keep", "a.test.ts:del dear"}},
			// Killed only by deleted cases: the cheaper goes back.
			{Key: "m2", KilledBy: []string{"a.test.ts:del dear", "a.test.ts:del cheap"}, CoveredBy: []string{"a.test.ts:del dear", "a.test.ts:del cheap"}},
			// Reached by a deleted case, killed by none: nothing lost.
			{Key: "m3", CoveredBy: []string{"a.test.ts:del dear"}},
			// Reached by no deleted case: not counted.
			{Key: "m4", KilledBy: []string{"a.test.ts:keep"}, CoveredBy: []string{"a.test.ts:keep"}},
		},
	}
}

func TestVerifyMatrix(t *testing.T) {
	// Act
	v, err := VerifyMatrix(matrixFixture(), []string{"a.test.ts:del cheap", "a.test.ts:del dear", "b.test.ts:pinned"}, " > ")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	wantBack := []PutBack{{Case: "b.test.ts:pinned", Mutant: "(pinned: asserts a log record)"}, {Case: "a.test.ts:del cheap", Mutant: "m2"}}
	if !reflect.DeepEqual(v.PutBack, wantBack) {
		t.Fatalf("put back %v, want %v", v.PutBack, wantBack)
	}
	if !reflect.DeepEqual(v.Proven, []string{"a.test.ts:del dear"}) {
		t.Fatalf("proven %v", v.Proven)
	}
	if v.Reached != 3 || v.SettledByTier != 1 || v.SurvivedDeleted != 1 {
		t.Fatalf("verification = %+v", v)
	}
}

func TestVerifyMatrixKeepsAPutBackCaseForLaterMutants(t *testing.T) {
	// Arrange: once del cheap is back for m2, it settles m5 as a kept case.
	m := matrixFixture()
	m.Mutants = append(m.Mutants, MutantResult{Key: "m5", KilledBy: []string{"a.test.ts:del cheap"}, CoveredBy: []string{"a.test.ts:del cheap"}})

	// Act
	v, err := VerifyMatrix(m, []string{"a.test.ts:del cheap", "a.test.ts:del dear"}, " > ")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(v.PutBack) != 1 || v.SettledByTier != 1 {
		t.Fatalf("verification = %+v", v)
	}
}

func TestVerifyMatrixNamesAPrefixForEveryCaseItBegins(t *testing.T) {
	// Arrange
	m := &Measurement{Cases: []Case{{ID: "f.ts:d > x"}, {ID: "f.ts:d > y"}, {ID: "f.ts:dz"}}}

	// Act
	v, err := VerifyMatrix(m, []string{"f.ts:d"}, " > ")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(v.Deleted, []string{"f.ts:d > x", "f.ts:d > y"}) {
		t.Fatalf("deleted = %v", v.Deleted)
	}
}

func TestVerifyMatrixRefusesAnUnknownCase(t *testing.T) {
	// Act
	_, err := VerifyMatrix(matrixFixture(), []string{"gone.test.ts:x"}, " > ")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "holds none of: gone.test.ts:x") {
		t.Fatalf("err = %v", err)
	}
}

func TestVerifyMatrixKeepsADeletedCaseReachingAnUnattributedMutant(t *testing.T) {
	// Arrange: a timeout no test is named for, reached by a deleted case.
	m := matrixFixture()
	m.Mutants = []MutantResult{{Key: "mt", Status: "unattributed", CoveredBy: []string{"a.test.ts:del dear", "a.test.ts:keep"}}}

	// Act
	v, err := VerifyMatrix(m, []string{"a.test.ts:del dear"}, " > ")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(v.PutBack, []PutBack{{Case: "a.test.ts:del dear", Mutant: "mt"}}) {
		t.Fatalf("put back %v", v.PutBack)
	}
}
