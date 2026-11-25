package main

import (
	"flag"
	"io"
	"regexp"
	"slices"
	"testing"
)

func TestRepeatedFlagCollectsEveryValueInOrder(t *testing.T) {
	// Arrange
	var got []string
	fs := flag.NewFlagSet("t", flag.ContinueOnError)
	fs.Var(repeatedFlag{values: &got, noun: "url"}, "show", "")

	// Act
	err := fs.Parse([]string{"-show", "https://a", "-show", "https://b"})

	// Assert
	if err != nil || !slices.Equal(got, []string{"https://a", "https://b"}) {
		t.Fatalf("parse = %v, values %q; want both values in order", err, got)
	}
}

func TestRepeatedFlagRefusesABlankValueByItsNoun(t *testing.T) {
	// Arrange
	var got []string
	fs := flag.NewFlagSet("t", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	fs.Var(repeatedFlag{values: &got, noun: "selection"}, "go", "")

	// Act
	err := fs.Parse([]string{"-go", " "})

	// Assert
	if err == nil || !regexp.MustCompile(`an empty selection`).MatchString(err.Error()) || len(got) != 0 {
		t.Fatalf("parse = %v, values %q; want the refusal naming the noun and nothing collected", err, got)
	}
}
