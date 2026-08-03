package logging

import (
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"testing"
	"time"
)

// fixturePath is the checked-in cross-language timestamp contract. It lives
// beside the protos rather than inside any one language's tree, because it is a
// contract among three runtimes and giving it a home inside one of them would
// make that one the author of the other two's representation.
const fixturePath = "../../../proto/vocab/log-timestamp.json"

// timestampFixture is the shape of log-timestamp.json. The `_comment` keys are
// prose for a reader and are deliberately not decoded.
type timestampFixture struct {
	Zone             string            `json:"zone"`
	FractionalDigits int               `json:"fractional_digits"`
	Pattern          string            `json:"pattern"`
	Layouts          map[string]string `json:"layouts"`
	Example          struct {
		Instant       string `json:"instant"`
		RenderedInUTC string `json:"rendered_in_utc"`
	} `json:"example"`
}

func loadFixture(t *testing.T) timestampFixture {
	t.Helper()
	raw, err := os.ReadFile(filepath.FromSlash(fixturePath))
	if err != nil {
		t.Fatalf("read the timestamp fixture: %v", err)
	}
	var f timestampFixture
	if err := json.Unmarshal(raw, &f); err != nil {
		t.Fatalf("decode the timestamp fixture: %v", err)
	}
	return f
}

func TestLayoutMatchesTheCrossLanguageFixture(t *testing.T) {
	// Arrange
	f := loadFixture(t)

	// Act + Assert: a Go-side edit that the other two runtimes did not follow
	// fails here rather than in a log nobody can interleave.
	if TimestampLayout != f.Layouts["go"] {
		t.Fatalf("TimestampLayout = %q, fixture = %q", TimestampLayout, f.Layouts["go"])
	}
}

func TestFixtureExampleRendersExactlyAsSpecified(t *testing.T) {
	// Arrange: the fixture's rendering is the one a UTC-zoned machine produces.
	f := loadFixture(t)
	at, err := time.Parse(time.RFC3339Nano, f.Example.Instant)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	rendered := at.In(time.UTC).Format(TimestampLayout)

	// Assert
	if rendered != f.Example.RenderedInUTC {
		t.Fatalf("rendered = %q, fixture = %q", rendered, f.Example.RenderedInUTC)
	}
}

func TestTimestampMatchesTheFixturePattern(t *testing.T) {
	// Arrange: a whole second, whose fractional digits RFC3339Nano would drop.
	f := loadFixture(t)
	pattern := regexp.MustCompile(f.Pattern)

	// Act
	rendered := Timestamp(time.Date(2026, 7, 28, 12, 34, 56, 0, time.UTC))

	// Assert
	if !pattern.MatchString(rendered) {
		t.Fatalf("rendered = %q, want the fixture pattern %q", rendered, f.Pattern)
	}
}

func TestTimestampConvertsAUTCInstantToTheLocalZone(t *testing.T) {
	// Arrange
	at := time.Date(2026, 7, 28, 12, 34, 56, 789000000, time.UTC)

	// Act
	rendered := Timestamp(at)

	// Assert: the wall clock is the local one, so a UTC-held instant is
	// rewritten rather than passed through with a "Z".
	if rendered != at.Local().Format(TimestampLayout) {
		t.Fatalf("rendered = %q, want %q", rendered, at.Local().Format(TimestampLayout))
	}
}

func TestTimestampTruncatesBelowTheFixturePrecision(t *testing.T) {
	// Arrange: a sub-microsecond instant, which the fixed width cannot carry.
	f := loadFixture(t)
	at := time.Date(2026, 7, 28, 12, 34, 56, 123_999, time.UTC)

	// Act
	rendered := Timestamp(at)

	// Assert: the field stays exactly fractional_digits wide.
	if got := len(rendered) - len("2026-07-28T12:34:56.") - len("+00:00"); got != f.FractionalDigits {
		t.Fatalf("fractional digits = %d, fixture = %d (%q)", got, f.FractionalDigits, rendered)
	}
}
