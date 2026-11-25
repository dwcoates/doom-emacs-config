package daemonrules

import (
	"os"
	"regexp"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalResolveFooterDir is the package these rules hold, under the daemon module's root.
const internalResolveFooterDir = "internal/resolve/footer"

// EVERY READING OF A SPAWN'S PROMPT INTO A ROW GOES THROUGH takeCommission AND
// takeUpdate: a hand-rolled site elsewhere in chips.go would let the spawning
// call's stream and the run's own describe one run differently.
func TestChipsReadsAPromptOnlyThroughTheSharedHelpers(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFooterDir)
	// Arrange
	source, err := os.ReadFile("chips.go")
	if err != nil {
		t.Fatalf("read chips.go: %v", err)
	}

	// Act
	beats := strings.Count(string(source), "GetPrompt().GetDescription()")
	commissions := strings.Count(string(source), "row.description = commission.GetDescription()")

	// Assert
	if beats != 1 || commissions != 1 {
		t.Fatalf("chips.go reads a beat's description at %d sites and a commission's at %d, want 1 each (takeUpdate, takeCommission)", beats, commissions)
	}
}

// EVERY ROW OPENED AHEAD OF ITS DESCRIPTION IS minimalAgentRow's: the generic
// label is spelled in one row literal, so a hand-rolled minimal row elsewhere
// fails here rather than drifting.
func TestMinimalRowsAreBuiltOnlyByTheSharedHelper(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFooterDir)
	for _, file := range []string{"chips.go", "livework.go", "netresume.go"} {
		// Arrange
		source, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}

		// Act
		literals := strings.Count(string(source), "label:        subagentLabel(nil)") +
			strings.Count(string(source), "label: subagentLabel(nil)")

		// Assert
		want := 0
		if file == "chips.go" {
			want = 1
		}
		if literals != want {
			t.Fatalf("%s builds a generic-label row literal at %d sites, want %d", file, literals, want)
		}
	}
}

// TestNoFooterSourceSpellsALiveItemPrefix pins the footer's live-work records
// to the session watcher's one spelling of an item's name
// (sessionwatcher.NameAgent and its siblings): a hand-written prefix here
// would drift from the names every other record uses.
func TestNoFooterSourceSpellsALiveItemPrefix(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFooterDir)
	// Arrange
	literal := regexp.MustCompile(`"(agent|shell|monitor|pending):"`)
	// Act
	for _, file := range sourcescan.Production(t, internalResolveFooterDir) {
		// Assert
		if loc := literal.FindIndex(file.Source); loc != nil {
			t.Errorf("%s spells %s itself; use the sessionwatcher.Name* prefix", file.Name, file.Source[loc[0]:loc[1]])
		}
	}
}

// EVERY RETRY SCHEDULE THE FOOTER DRAWS CONVERTS ITS COUNT THROUGH ONE HELPER:
// a site adding one to a schedule's attempt by hand is a second spelling of
// the convention, which drifts.
func TestNoFooterSourceConvertsARetryCountByHand(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFooterDir)
	// Arrange, Act.
	n := sourcescan.Count(t, internalResolveFooterDir, "GetAttempt()) + 1")

	// Assert.
	if n != 0 {
		t.Fatalf("hand-converted retry counts = %d, want every site through attemptOfRetry", n)
	}
}

func TestEveryUntimedRetryArmGoesThroughObserveUntimedRetry(t *testing.T) {
	sourcescan.InPackage(t, internalResolveFooterDir)
	// Arrange, Act: the one retryState built with a now or awaitingNetwork
	// clause is observeUntimedRetry's.
	nows := sourcescan.Count(t, internalResolveFooterDir, "now:             u.now,")
	awaits := sourcescan.Count(t, internalResolveFooterDir, "awaitingNetwork: u.awaitingNetwork,")
	handNows := sourcescan.Count(t, internalResolveFooterDir, "now:     true,")
	handAwaits := sourcescan.Count(t, internalResolveFooterDir, "awaitingNetwork: true,\n\t\t}")

	// Assert.
	if nows != 1 || awaits != 1 || handNows != 0 || handAwaits != 0 {
		t.Fatalf("untimed retry builds = %d/%d, hand-built = %d/%d, want every untimed arm through observeUntimedRetry", nows, awaits, handNows, handAwaits)
	}
}
