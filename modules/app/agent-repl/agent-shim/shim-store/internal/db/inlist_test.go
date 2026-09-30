package db

import "testing"

func TestExpandInListFillsOnePlaceholderPerID(t *testing.T) {
	tests := []struct {
		name string
		n    int
		want string
	}{
		{name: "one id", n: 1, want: "SELECT x FROM t WHERE k IN (?)"},
		{name: "three ids", n: 3, want: "SELECT x FROM t WHERE k IN (?,?,?)"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			query := "SELECT x FROM t WHERE k IN (%s)"

			// Act
			got := expandInList(query, test.n)

			// Assert
			if got != test.want {
				t.Fatalf("expandInList = %q, want %q", got, test.want)
			}
		})
	}
}

func TestExpandInListPanicsOnAnEmptyList(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("expandInList(0) did not panic; an empty IN list must be refused by its caller first")
		}
	}()

	// Act
	expandInList("SELECT x FROM t WHERE k IN (%s)", 0)
}
