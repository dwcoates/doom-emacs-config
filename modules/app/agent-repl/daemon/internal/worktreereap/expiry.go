package worktreereap

import (
	"bufio"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE EXPIRY RULE. A linked worktree that survived every safety gate but is
// not landed (dirty, conflicting or unlanded) is removed once it has existed
// for ExpireAfter of UNBROKEN REPOSITORY ACTIVITY: its clock starts at the
// later of its birth and the end of the repository's most recent idle day
// (RepoIdleGap with no activity anywhere in the repository), so a repository
// nobody touched for a day -- a weekend, a holiday -- restarts every clock.
//
// Nothing an expired worktree holds is lost: it is first preserved as a
// commit under refs/agent-repl/reaped/, and its branch is kept.

// RepoIdleGap is "a full day without any activity in the repository": a gap
// this long between two signs of activity, or between the last one and now,
// restarts every worktree's expiry clock.
const RepoIdleGap = 24 * time.Hour

// reapedRefPrefix is where an expired worktree's preservation commit lives.
const reapedRefPrefix = "refs/agent-repl/reaped/"

// repoActivity is every instant the repository shows somebody at work: every
// reflog entry under the common dir (every ref's log, and every linked
// worktree's HEAD log), and, for each of the repository's registered
// workspaces, the record's creation, last activity, last selection and merge
// instants.
//
// A repository whose reflogs are absent (reflogs turned off, nothing written
// yet) has only the registry's instants. A reftable repository keeps its logs
// in a binary table this reader does not parse, and is REFUSED rather than read
// as having no history.
func repoActivity(commonDir string, repo ids.RepoID, workspaces []wsm.Workspace) ([]time.Time, error) {
	if _, err := os.Stat(filepath.Join(commonDir, "reftable")); err == nil {
		return nil, fmt.Errorf("worktreereap: %s keeps its refs in a reftable, whose reflogs cannot be read", commonDir)
	} else if !errors.Is(err, os.ErrNotExist) {
		return nil, fmt.Errorf("worktreereap: reading %s: %w", commonDir, err)
	}

	var instants []time.Time
	var logs []string
	err := filepath.WalkDir(filepath.Join(commonDir, "logs"), func(path string, entry fs.DirEntry, err error) error {
		switch {
		case errors.Is(err, os.ErrNotExist) && path == filepath.Join(commonDir, "logs"):
			return fs.SkipAll
		case err != nil:
			return err
		case !entry.IsDir():
			logs = append(logs, path)
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("worktreereap: walking the reflogs of %s: %w", commonDir, err)
	}
	linked, err := os.ReadDir(filepath.Join(commonDir, "worktrees"))
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return nil, fmt.Errorf("worktreereap: listing the linked worktrees of %s: %w", commonDir, err)
	}
	for _, entry := range linked {
		if entry.IsDir() {
			logs = append(logs, filepath.Join(commonDir, "worktrees", entry.Name(), "logs", "HEAD"))
		}
	}
	for _, path := range logs {
		read, err := readReflog(path)
		if err != nil {
			return nil, err
		}
		instants = append(instants, read...)
	}

	for _, ws := range workspaces {
		if ws.Repo != repo {
			continue
		}
		instants = append(instants, ws.CreatedAt)
		for _, stamp := range []*time.Time{ws.LastActivityAt, ws.LastSelectedAt, ws.MergedAt} {
			if stamp != nil {
				instants = append(instants, *stamp)
			}
		}
	}
	return instants, nil
}

// readReflog answers the instant of every entry of one reflog file. A file
// that is not there has no entries (a linked worktree with reflogs off); a
// line that is not a reflog entry is an error, never skipped.
//
// An entry is `<old> <new> <name> <<email>> <unix seconds> <tz>[\t<message>]`.
func readReflog(path string) ([]time.Time, error) {
	file, err := os.Open(path)
	if errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("worktreereap: reading the reflog %s: %w", path, err)
	}
	defer file.Close()

	var instants []time.Time
	scanner := bufio.NewScanner(file)
	scanner.Buffer(make([]byte, 64*1024), 1024*1024)
	for line := 1; scanner.Scan(); line++ {
		header, _, _ := strings.Cut(scanner.Text(), "\t")
		_, stamp, found := cutLast(header, ">")
		fields := strings.Fields(stamp)
		if !found || len(fields) != 2 {
			return nil, fmt.Errorf("worktreereap: line %d of the reflog %s is not a reflog entry: %q", line, path, scanner.Text())
		}
		seconds, err := strconv.ParseInt(fields[0], 10, 64)
		if err != nil {
			return nil, fmt.Errorf("worktreereap: line %d of the reflog %s has an unreadable time %q: %w", line, path, fields[0], err)
		}
		instants = append(instants, time.Unix(seconds, 0))
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("worktreereap: reading the reflog %s: %w", path, err)
	}
	return instants, nil
}

// cutLast is strings.Cut at the LAST occurrence of sep.
func cutLast(s, sep string) (string, string, bool) {
	i := strings.LastIndex(s, sep)
	if i < 0 {
		return s, "", false
	}
	return s[:i], s[i+len(sep):], true
}

// activeSince answers when the repository's current unbroken run of activity
// began: walking back from now, the first instant followed by a gap of at
// least RepoIdleGap ends the walk. A repository idle for the whole gap right
// now answers now itself, so nothing in it expires until somebody is back. An
// instant later than now (a clock that disagrees) counts as now.
func activeSince(instants []time.Time, now time.Time) time.Time {
	sorted := append([]time.Time(nil), instants...)
	sort.Slice(sorted, func(i, j int) bool { return sorted[i].After(sorted[j]) })
	since := now
	for _, at := range sorted {
		if at.After(now) {
			continue
		}
		if since.Sub(at) >= RepoIdleGap {
			return since
		}
		since = at
	}
	return since
}

// bornAt is when a linked worktree was created: the mtime of the `commondir`
// file git writes into the worktree's admin directory once, at `worktree add`.
// It is required: an admin directory without it is not a linked worktree's.
func bornAt(admin string) (time.Time, error) {
	path := filepath.Join(admin, "commondir")
	info, err := os.Stat(path)
	if err != nil {
		return time.Time{}, fmt.Errorf("worktreereap: reading the worktree's birth from %s: %w", path, err)
	}
	return info.ModTime(), nil
}

// reapedRef is the ref an expired worktree's preservation commit is created
// at: its admin directory's name (git's own unique id for the worktree) and
// the sweep's instant, so two expiries never claim one ref.
func reapedRef(admin string, at time.Time) string {
	return reapedRefPrefix + filepath.Base(admin) + "/" + at.UTC().Format("20060102T150405Z")
}
