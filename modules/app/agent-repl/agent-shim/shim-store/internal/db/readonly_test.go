package db

import (
	"errors"
	"reflect"
	"slices"
	"testing"
)

// TestReadOnlyExposesOnlyPageReads pins the read-only face's method set: a
// method added beside the three reads is a change to what the daemon may do
// to the store, and must be made here on purpose.
func TestReadOnlyExposesOnlyPageReads(t *testing.T) {
	// Arrange
	want := []string{"NewestPage", "PageBefore", "PageThrough"}

	// Act
	typ := reflect.TypeOf(ReadOnly{})
	var got []string
	for i := range typ.NumMethod() {
		got = append(got, typ.Method(i).Name)
	}

	// Assert
	if !slices.Equal(got, want) {
		t.Fatalf("ReadOnly's methods = %v, want exactly %v", got, want)
	}
}

func TestReadOnlyNewestPage(t *testing.T) {
	tests := []struct {
		name      string
		book      string
		stated    bool
		wantLines int
		wantErr   error
	}{
		{name: "a book with lines is served newest first", book: "agent-1", wantLines: 3},
		{name: "a stated book the store holds nothing for is the empty page", book: "agent-unwritten", stated: true},
		{name: "a named book the store never heard of is an unknown agent", book: "agent-unwritten", wantErr: ErrUnknownAgent},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)
			seedBook(t, d, "agent-1", 3)

			// Act
			page, err := d.ReadOnly().NewestPage(ctx(), tt.book, tt.stated)

			// Assert
			if tt.wantErr != nil {
				if !errors.Is(err, tt.wantErr) {
					t.Fatalf("NewestPage err = %v, want %v", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("NewestPage: %v", err)
			}
			if len(page.GetLines()) != tt.wantLines || page.GetFloor() == nil {
				t.Fatalf("page = %d lines, floor %v; want %d lines at the floor", len(page.GetLines()), page.GetFloor() != nil, tt.wantLines)
			}
		})
	}
}

func TestReadOnlyPageBeforeWalksOlder(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 3)

	// Act
	page, err := d.ReadOnly().PageBefore(ctx(), "agent-1", pointers[2])

	// Assert
	if err != nil {
		t.Fatalf("PageBefore: %v", err)
	}
	if len(page.GetLines()) != 2 || page.GetLines()[0].GetAt().GetValue() != pointers[1].GetValue() {
		t.Fatalf("lines = %v, want the two older lines, newest first", page.GetLines())
	}
}

func TestReadOnlyPageThroughRefusesANonPositiveInstant(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedBook(t, d, "agent-1", 1)

	// Act
	_, err := d.ReadOnly().PageThrough(ctx(), "agent-1", 0)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("PageThrough err = %v, want ErrInvalid", err)
	}
}
