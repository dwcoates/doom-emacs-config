package daemonrules

import (
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalNewsdigestDir is the package these rules hold, under the daemon module's root.
const internalNewsdigestDir = "internal/newsdigest"

// TestEveryModelWrittenLinkPassesValidLink holds the package to one link
// check: a source file checking a link's scheme on its own fails here.
func TestEveryModelWrittenLinkPassesValidLink(t *testing.T) {
	sourcescan.InPackage(t, internalNewsdigestDir)
	// Act
	checks := sourcescan.Count(t, internalNewsdigestDir, `Scheme != "https"`)

	// Assert
	if checks != 1 {
		t.Fatalf("the https check appears %d times in the package, want once, in validLink", checks)
	}
}

// TestEveryModelAnswerIsDecodedByDecodeAnswer holds the package's parsers to
// one strict decode: a parser building its own decoder fails here.
func TestEveryModelAnswerIsDecodedByDecodeAnswer(t *testing.T) {
	sourcescan.InPackage(t, internalNewsdigestDir)
	// Act
	counts := []int{sourcescan.Count(t, internalNewsdigestDir, "stripFence(text)"), sourcescan.Count(t, internalNewsdigestDir, "decodeAnswer(text, &a)")}

	// Assert
	if counts[0] != 1 || counts[1] != 3 {
		t.Fatalf("strict decoders = %d and decodeAnswer calls = %d, want one decoder in decodeAnswer called by the three parsers", counts[0], counts[1])
	}
}

// TestEveryContextEndOfARunIsRecordedByContextEnded holds run to one record
// of a run its context ended.
func TestEveryContextEndOfARunIsRecordedByContextEnded(t *testing.T) {
	sourcescan.InPackage(t, internalNewsdigestDir)
	// Act
	counts := []int{sourcescan.Count(t, internalNewsdigestDir, "ended with its context"), sourcescan.Count(t, internalNewsdigestDir, "contextEnded(log, ")}

	// Assert
	if counts[0] != 1 || counts[1] != 3 {
		t.Fatalf("records = %d and calls = %d, want one record in contextEnded and three exits through it", counts[0], counts[1])
	}
}
