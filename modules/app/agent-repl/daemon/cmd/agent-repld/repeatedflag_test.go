package main

import (
	"flag"
	"io"
	"os"
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

// TestEveryRepeatedFlagIsTheSharedType pins that no command hand-rolls its own
// repeated string flag beside repeatedFlag.
func TestEveryRepeatedFlagIsTheSharedType(t *testing.T) {
	// Arrange
	setter := regexp.MustCompile(`\*f\.values = append\(\*f\.values, value\)`)
	files, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the package: %v", err)
	}

	// Act
	var offenders []string
	for _, file := range files {
		name := file.Name()
		if name == "repeatedflag.go" || !regexp.MustCompile(`^[^.]+\.go$`).MatchString(name) || regexp.MustCompile(`_test\.go$`).MatchString(name) {
			continue
		}
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		if setter.Match(source) {
			offenders = append(offenders, name)
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these files hand-roll a repeated flag; use repeatedFlag: %v", offenders)
	}
}
