package db

import (
	"errors"
	"strings"
	"testing"
)

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

func TestIDListArgsBindsEveryAskedID(t *testing.T) {
	// Arrange
	ids := []string{"a", "b"}

	// Act
	args, err := idListArgs(ids, "ids", "none", "empty")

	// Assert
	if err != nil {
		t.Fatalf("idListArgs: %v", err)
	}
	if len(args) != 2 || args[0] != "a" || args[1] != "b" {
		t.Fatalf("args = %v, want [a b]", args)
	}
}

func TestIDListArgsRefusesAMalformedList(t *testing.T) {
	tests := []struct {
		name   string
		ids    []string
		field  string
		detail string
	}{
		{name: "no ids", ids: nil, field: "ids", detail: "none"},
		{name: "an empty id", ids: []string{"a", ""}, field: "ids[1]", detail: "empty"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange / Act
			_, err := idListArgs(test.ids, "ids", "none", "empty")

			// Assert
			if !errors.Is(err, ErrInvalid) || RefusalField(err) != test.field || !strings.HasSuffix(err.Error(), test.detail) {
				t.Fatalf("err = %v (field %q), want ErrInvalid naming %s with detail %q", err, RefusalField(err), test.field, test.detail)
			}
		})
	}
}
