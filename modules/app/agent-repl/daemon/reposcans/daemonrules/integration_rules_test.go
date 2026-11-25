package daemonrules

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// integrationDir is the package these rules hold, under the daemon module's root.
const integrationDir = "integration"

func TestEveryStrayCheckGoesThroughRequireNoStrays(t *testing.T) {
	sourcescan.InPackage(t, integrationDir)
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob the package's tests: %v", err)
	}
	for _, name := range files {
		// Act.
		raw, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}

		// Assert: a leak check reads StrayPIDs only through the helper (or to
		// prove, before an act, that there is something to reclaim). The
		// helper's own read is the one line that may.
		for _, line := range strings.Split(string(raw), "\n") {
			if strings.Contains(line, "StrayPIDs(); len(") && !strings.Contains(line, "== 0") && strings.TrimSpace(line) != "if left := d.StrayPIDs(); len(left) > 0 {" {
				t.Fatalf("%s checks for strays by hand (%q); use requireNoStrays", name, strings.TrimSpace(line))
			}
		}
	}
}

// TestNoTestHandRollsTheBootWifiStanding pins that every wait for the settled
// boot standing goes through harness.AwaitBootWifiStanding: a hand-rolled
// predicate drifts (one took the first standing, its lid unheard, and failed
// on the lid's later push, 2026-11-14).
func TestNoTestHandRollsTheBootWifiStanding(t *testing.T) {
	sourcescan.InPackage(t, integrationDir)
	// Arrange: the predicate's body, assembled so this file does not hold it.
	predicate := "GetPersistentWifi()" + ".GetLidOpen() != nil"
	files, err := filepath.Glob("*_test.go")
	if err != nil || len(files) == 0 {
		t.Fatalf("list the package's test files = %v, %v, want them", files, err)
	}

	for _, file := range files {
		// Act
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}

		// Assert
		if strings.Contains(string(src), predicate) {
			t.Errorf("%s hand-rolls %q, want harness.AwaitBootWifiStanding", file, predicate)
		}
	}
}

// TestTheDurableQueueIsReadThroughOneHelper holds every integration test that
// reads the merge queue's stored order to durableQueueOrder, so no test
// hand-rolls its own read of the table.
func TestTheDurableQueueIsReadThroughOneHelper(t *testing.T) {
	sourcescan.InPackage(t, integrationDir)
	// Arrange: the suite's TEST sources, which is where the helper and every
	// read it replaces live (the suite has no production source, so
	// sourcescan.Production, which skips tests, does not apply).
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("list the suite's sources: %v", err)
	}
	var files []string
	for _, entry := range entries {
		if strings.HasSuffix(entry.Name(), "_test.go") {
			files = append(files, entry.Name())
		}
	}

	// Act
	var sites []string
	for _, file := range files {
		code, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		for range strings.Count(string(code), "SELECT workspace_id FROM merge_queue ORDER BY seq") {
			sites = append(sites, file)
		}
	}

	// Assert: the helper's own query alone; this rule's literal lives in
	// reposcans, outside the files it reads.
	if want := []string{"merge_test.go"}; strings.Join(sites, ",") != strings.Join(want, ",") {
		t.Fatalf("queue order reads in %v, want only the helper's in %v", sites, want)
	}
}

func TestEveryAwaitOfAMergesEndReadsIsMergeTerminal(t *testing.T) {
	sourcescan.InPackage(t, integrationDir)
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatal(err)
	}
	for _, file := range files {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("reading %s: %v", file, err)
		}

		// Act / Assert: the predicate is spelled once, in isMergeTerminal.
		if n := strings.Count(string(body), "GetSuccess() != nil || m.GetError() != nil") + strings.Count(string(body), "GetMerge().GetSuccess() != nil || row.GetActivity().GetMerge().GetError() != nil"); n > 0 && file != "support_merge_test.go" {
			t.Fatalf("%s spells a merge's end by hand %d times instead of isMergeTerminal", file, n)
		}
	}
}

// TestNoRPCAssertionReadsAResponseAfterAFailedCall pins that no failure line
// formats resp.Msg in a branch a failed rpc reaches: such a site panics on the
// very failure it reports. requireRPCAnswered is the shared step instead.
func TestNoRPCAssertionReadsAResponseAfterAFailedCall(t *testing.T) {
	sourcescan.InPackage(t, integrationDir)
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("Glob: %v", err)
	}
	handRolled := regexp.MustCompile(`err != nil \|\| (\w+)\.Msg[^\n]*\{\n\s*t\.(?:Fatalf|Errorf)\([^\n]*`)

	// Act.
	var sites []string
	for _, file := range files {
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("ReadFile %s: %v", file, err)
		}
		if file == "support_rpc_test.go" {
			continue
		}
		for _, m := range handRolled.FindAllSubmatch(body, -1) {
			failureLine := m[0][bytes.LastIndexByte(m[0], '\n'):]
			if regexp.MustCompile(`\b` + string(m[1]) + `\.Msg\b`).Match(failureLine) {
				sites = append(sites, fmt.Sprintf("%s: %s", file, bytes.TrimSpace(failureLine)))
			}
		}
	}

	// Assert.
	if len(sites) != 0 {
		t.Fatalf("hand-rolled rpc success checks read the response after a failed call: %v; use requireRPCAnswered or requireRPCSuccess", sites)
	}
}

// TestEveryScriptedClaudeIsWrittenByOneHelper holds the suites that script
// the vendor to scriptedClaudeEnv: a hand-written fake claude fails here.
func TestEveryScriptedClaudeIsWrittenByOneHelper(t *testing.T) {
	sourcescan.InPackage(t, integrationDir)
	for _, file := range []string{"news_digest_test.go", "update_classifier_prompt_test.go"} {
		t.Run(file, func(t *testing.T) {
			// Act.
			src, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("read %s: %v", file, err)
			}

			// Assert.
			if strings.Contains(string(src), `"claude")`) || !strings.Contains(string(src), "scriptedClaudeEnv(") {
				t.Fatalf("%s writes its fake claude by hand", file)
			}
		})
	}
}

// TestEveryFeedReopenLoopIsOpenFeedUntil pins the one home of "re-open the
// feed on each row its tail delivers until the page matches": a test that
// hand-rolls the loop beside openFeedUntil fails here.
func TestEveryFeedReopenLoopIsOpenFeedUntil(t *testing.T) {
	sourcescan.InPackage(t, integrationDir)
	// Arrange.
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	var sites []string
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), "_test.go") {
			continue
		}
		src, err := os.ReadFile(e.Name())
		if err != nil {
			t.Fatal(err)
		}
		// Spelled in two parts so this test's own source is not a site.
		for range strings.Count(string(src), `"a feed change`+` toward `) {
			sites = append(sites, e.Name())
		}
	}

	// Assert.
	if strings.Join(sites, ",") != "support_test.go" {
		t.Fatalf("feed re-open loops in %v, want only openFeedUntil's in support_test.go", sites)
	}
}
