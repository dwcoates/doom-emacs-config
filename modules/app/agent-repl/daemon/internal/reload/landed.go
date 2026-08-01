package reload

import (
	"context"
	"fmt"
	"regexp"
	"strings"
)

// A merge lands by CHERRY-PICK, so the commits it added to the target are new
// SHAs that exist nowhere else. Nothing hands this package the range they
// occupy: merge.Request carries the merge's geometry (source branch, target
// worktree) but not the target's HEAD movement, and merge.PostMergeHook is
// handed the request alone.
//
// The range is therefore RE-DERIVED from the target, using the annotation the
// merge driver's own `cherry-pick -x` writes into every commit it creates:
//
//	(cherry picked from commit <40-hex>)
//
// Walking back from the target's HEAD, a commit belongs to the merge that just
// landed when it carries that annotation AND the commit the annotation names is
// reachable from the merged source branch. The second half is what makes the
// derivation sound on a repository whose history is FULL of such annotations
// (every previous workspace merge left them): a previous merge's commits are
// picks of commits on some OTHER branch, which are not ancestors of this
// source branch, so the walk stops at the first one.

// cherryPickAnnotationRE matches the annotation `git cherry-pick -x` writes,
// the same pattern merge.Driver uses to read its own picks back.
var cherryPickAnnotationRE = regexp.MustCompile(`\(cherry picked from commit ([0-9a-f]{40})\)`)

// maxLandedScan bounds the walk back from HEAD.
//
// It is a hard CEILING, not a window: a merge that appears to have landed this
// many commits is not a big merge, it is a derivation that has lost the plot
// (an annotation pattern matching history it should not), and continuing would
// hand deploy-all.sh a range spanning half the repository. Hitting it is an
// error, never a truncated answer.
const maxLandedScan = 200

// landed describes the commits one merge just added to its target.
type landed struct {
	// Count is how many commits landed. Zero is a legitimate answer: a merge
	// whose commits were ALREADY incorporated adds nothing, and there is then
	// nothing to redeploy.
	Count int
	// Spec is the `base..head` range those commits occupy, empty when Count is
	// zero. It is passed to deploy-all.sh --elisp verbatim and used for the
	// changed-path query.
	Spec string
}

// landedRange derives the range req's cherry-picks occupy in targetDir.
func landedRange(ctx context.Context, targetDir, sourceBranch string) (landed, error) {
	if sourceBranch == "" {
		return landed{}, fmt.Errorf("reload: landedRange needs a source branch")
	}
	// One `git log` for the whole scan window rather than one per commit: this
	// runs on the post-merge path and the per-commit form would be 200 process
	// spawns. \x1f separates the SHA from the body, \x1e separates records, so
	// neither can be confused with anything a commit message may contain.
	out, err := gitCapture(ctx, targetDir, "log",
		fmt.Sprintf("-n%d", maxLandedScan), "--format=%H%x1f%B%x1e", "HEAD")
	if err != nil {
		return landed{}, fmt.Errorf("reload: read target history in %s: %w", targetDir, err)
	}
	shas, bodies, err := parseLogRecords(out)
	if err != nil {
		return landed{}, fmt.Errorf("reload: parse target history in %s: %w", targetDir, err)
	}
	count := 0
	for i, sha := range shas {
		match := cherryPickAnnotationRE.FindStringSubmatch(bodies[i])
		if match == nil {
			break
		}
		fromSource, err := gitSucceeds(ctx, targetDir, "merge-base", "--is-ancestor", match[1], sourceBranch)
		if err != nil {
			return landed{}, fmt.Errorf("reload: test whether %s came from %s: %w", sha, sourceBranch, err)
		}
		if !fromSource {
			break
		}
		count++
	}
	if count == 0 {
		return landed{}, nil
	}
	if count >= len(shas) {
		// Either the scan ceiling was hit, or the target's ENTIRE history is
		// picks of this source branch. Both are impossible for the repository
		// this feature exists for (its root commit is nobody's cherry-pick), so
		// this is a violated invariant and fails hard rather than guessing a
		// base that does not exist.
		return landed{}, fmt.Errorf("reload: every one of the %d scanned commits in %s reads as a pick of %s, so the merge's base is unidentifiable",
			len(shas), targetDir, sourceBranch)
	}
	return landed{Count: count, Spec: shas[count] + ".." + shas[0]}, nil
}

// parseLogRecords splits the \x1e-separated log output into parallel SHA and
// body slices.
func parseLogRecords(out string) ([]string, []string, error) {
	var shas, bodies []string
	for _, record := range strings.Split(out, "\x1e") {
		record = strings.Trim(record, "\n")
		if record == "" {
			continue
		}
		sha, body, ok := strings.Cut(record, "\x1f")
		if !ok {
			return nil, nil, fmt.Errorf("log record %q carries no field separator", record)
		}
		shas = append(shas, sha)
		bodies = append(bodies, body)
	}
	return shas, bodies, nil
}

// changedPaths lists the repository-relative paths the range touched.
func changedPaths(ctx context.Context, targetDir, spec string) ([]string, error) {
	out, err := gitCapture(ctx, targetDir, "diff", "--name-only", spec)
	if err != nil {
		return nil, fmt.Errorf("reload: list paths changed by %s in %s: %w", spec, targetDir, err)
	}
	var paths []string
	for _, line := range strings.Split(out, "\n") {
		line = strings.TrimSpace(line)
		if line != "" {
			paths = append(paths, line)
		}
	}
	return paths, nil
}
