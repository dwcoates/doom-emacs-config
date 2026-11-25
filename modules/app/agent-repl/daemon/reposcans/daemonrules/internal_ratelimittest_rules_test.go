package daemonrules

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalRatelimittestDir is the package these rules hold, under the daemon module's root.
const internalRatelimittestDir = "internal/ratelimittest"

// offendingTestFiles answers every daemon test file outside the ratelimittest
// package and outside this rule file, whose own text spells every needle,
// whose text contains any of NEEDLES: the one walk every spelling guard
// below shares. A test runs in its package's directory (runtime.Caller's file
// carries no absolute path under -trimpath).
func offendingTestFiles(t *testing.T, needles ...string) []string {
	t.Helper()
	here, err := os.Getwd()
	if err != nil {
		t.Fatalf("the working directory, which is this package's: %v", err)
	}
	root := filepath.Join(here, "..", "..")
	thisRuleFile := filepath.Join(root, "reposcans", "daemonrules", "internal_ratelimittest_rules_test.go")
	var offenders []string
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, "_test.go") || filepath.Dir(path) == here || path == thisRuleFile {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		text := string(body)
		for _, needle := range needles {
			if strings.Contains(text, needle) {
				offenders = append(offenders, path)
				break
			}
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walk the daemon's tests: %v", err)
	}
	return offenders
}

// THE FIGURES HAVE ONE SPELLING in the daemon's tests, as the windows do: a
// test file that sets an event's utilization by hand fails here.
func TestNoTestHandRollsAnEventsFigures(t *testing.T) {
	sourcescan.InPackage(t, internalRatelimittestDir)
	// Act
	offenders := offendingTestFiles(t, "UtilizationPercent = &", "UtilizationPercent: &")

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("test files set a rate-limit event's figures by hand instead of through ratelimittest.Figured: %v", offenders)
	}
}

// THE BUILDERS ARE THE ONE SPELLING of a rate-limit window in the daemon's
// tests: a test file that builds a window's oneof arm by hand fails here.
func TestNoTestHandRollsARateLimitWindow(t *testing.T) {
	sourcescan.InPackage(t, internalRatelimittestDir)
	// Act
	offenders := offendingTestFiles(t, "SessionRateLimitType_")

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("test files build a rate-limit window by hand instead of through ratelimittest: %v", offenders)
	}
}

// THE WALK FINDS A FILE THAT HOLDS A NEEDLE, so a guard above cannot pass by
// walking nothing.
func TestOffendingTestFilesFindsAFileHoldingTheNeedle(t *testing.T) {
	sourcescan.InPackage(t, internalRatelimittestDir)
	// Act: every daemon test file names the testing package.
	offenders := offendingTestFiles(t, "\"testing\"")

	// Assert
	if len(offenders) == 0 {
		t.Fatalf("the walk found no daemon test file importing testing; it walked nothing")
	}
}

func TestNoTestHandRollsAPlanUsageRead(t *testing.T) {
	sourcescan.InPackage(t, internalRatelimittestDir)
	// Act
	offenders := offendingTestFiles(t, "SessionUpdate_PlanUsage{", "SessionPlanUsageWindow{", "SessionPlanUsageMonthlySpend{")

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("test files build a plan-usage read by hand instead of through ratelimittest.PlanUsage: %v", offenders)
	}
}
