package prompthandler

import (
	"regexp"
	"testing"
)

func TestMintKeyIsThePrefixAndAFreshRandomSuffix(t *testing.T) {
	// Arrange
	shape := regexp.MustCompile(`^link-unresolved-[0-9a-f]{32}$`)

	// Act
	a, errA := MintKey("link-unresolved")
	b, errB := MintKey("link-unresolved")

	// Assert
	if errA != nil || errB != nil {
		t.Fatalf("MintKey: %v, %v", errA, errB)
	}
	if !shape.MatchString(a) || !shape.MatchString(b) {
		t.Fatalf("keys = %q, %q; want the prefix and 32 hex characters", a, b)
	}
	if a == b {
		t.Fatalf("two mints answered the same key %q", a)
	}
}
