package gitclient

import (
	"context"
	"fmt"
	"strconv"
	"strings"

	"agent-repld/internal/dlog"
)

// Upstream is the remote branch a local branch tracks.
type Upstream struct {
	// Remote is the remote's name, e.g. "origin": what a fetch names.
	Remote string
	// Ref is the upstream's short name, e.g. "origin/master": what a count
	// names.
	Ref string
}

// upstreamFormat prints a branch's ref name, its upstream's remote and its
// upstream's short name, NUL-separated. The ref name is printed so a branch
// that does not exist (no line at all) is told apart from one that tracks
// nothing (a line with both upstream fields empty).
const upstreamFormat = "%(refname)%00%(upstream:remotename)%00%(upstream:short)"

// Upstream reports the remote branch a local branch tracks. It is a PROBE: a
// branch that tracks nothing is an ordinary answer (false, nil error), never an
// error record. A branch that does not exist is a failure, because the caller
// named a branch it believed was there.
func (c *client) Upstream(ctx context.Context, repoDir, branch string) (Upstream, bool, error) {
	const operation = "daemon.gitclient.upstream"
	out, err := c.run(ctx, operation, repoDir, "for-each-ref", "--format="+upstreamFormat, "refs/heads/"+branch)
	if err != nil {
		return Upstream{}, false, err
	}
	fields := strings.Split(out, "\x00")
	if out == "" || strings.Contains(out, "\n") || len(fields) != 3 || fields[0] != "refs/heads/"+branch {
		cause := fmt.Errorf("gitclient: branch %q in %s: for-each-ref answered %q, want one line naming the branch", branch, repoDir, out)
		if out == "" {
			cause = fmt.Errorf("gitclient: branch %q does not exist in %s", branch, repoDir)
		}
		c.log.Global().Error(operation, "the branch's upstream could not be read", dlog.Context{
			"dir": repoDir, "branch": branch, "stdout": out, "cause": cause.Error(),
		})
		return Upstream{}, false, cause
	}
	remote, ref := fields[1], fields[2]
	if remote == "" && ref == "" {
		return Upstream{}, false, nil
	}
	if remote == "" || ref == "" {
		cause := fmt.Errorf("gitclient: branch %q in %s has a half-configured upstream (remote %q, ref %q)", branch, repoDir, remote, ref)
		c.log.Global().Error(operation, "the branch's upstream is half configured", dlog.Context{
			"dir": repoDir, "branch": branch, "remote": remote, "ref": ref, "cause": cause.Error(),
		})
		return Upstream{}, false, cause
	}
	return Upstream{Remote: remote, Ref: ref}, true, nil
}

// AheadBehind counts the commits reachable from local and not from upstream
// (ahead), and from upstream and not from local (behind).
func (c *client) AheadBehind(ctx context.Context, dir, local, upstream string) (int, int, error) {
	const operation = "daemon.gitclient.ahead_behind"
	out, err := c.run(ctx, operation, dir, "rev-list", "--left-right", "--count", "--end-of-options", local+"..."+upstream)
	if err != nil {
		return 0, 0, err
	}
	ahead, behind, err := parseLeftRightCount(out)
	if err != nil {
		cause := fmt.Errorf("gitclient: counting %s...%s in %s: %w", local, upstream, dir, err)
		c.log.Global().Error(operation, "git's left-right count could not be read", dlog.Context{
			"dir": dir, "local": local, "upstream": upstream, "stdout": out, "cause": cause.Error(),
		})
		return 0, 0, cause
	}
	return ahead, behind, nil
}

// parseLeftRightCount reads `rev-list --left-right --count`'s "LEFT\tRIGHT".
func parseLeftRightCount(out string) (int, int, error) {
	left, right, found := strings.Cut(out, "\t")
	if !found {
		return 0, 0, fmt.Errorf("answer %q is not two tab-separated counts", out)
	}
	ahead, err := strconv.Atoi(left)
	if err != nil || ahead < 0 {
		return 0, 0, fmt.Errorf("left count %q is not a count", left)
	}
	behind, err := strconv.Atoi(right)
	if err != nil || behind < 0 {
		return 0, 0, fmt.Errorf("right count %q is not a count", right)
	}
	return ahead, behind, nil
}
