// Package gocache keeps cmd/go's daily trim of the host's Go build cache out
// of every unit's time limit.
//
// THE TRIM RUNS AT THE END OF EVERY GO COMMAND THAT BUILDS. cmd/go's
// Builder.Do closes the cache on its way out, and closing it walks every
// entry of the cache once trim.txt says the last completed trim is a day old
// (cmd/go/internal/cache.DiskCache.Trim). Nothing serializes that walk across
// processes, and trim.txt is written only once a walk completes. On
// 2026-11-10 the shared cache was 156 GiB and its walk took 19s, so once the
// trim came due in a merge gate every compile-only build unit, bounded under
// a second, spent its time limit on the walk and was killed: 128 of them,
// each kill leaving trim.txt stale for the next.
//
// So before a unit starts, the run makes sure the trim is not due before the
// unit's deadline, trimming the cache itself when it would be. The unit is
// then killed at that same deadline (run.Runner), so no go command it runs
// can be alive when the trim comes due: the walk cannot land inside a unit.
package gocache

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"agentrepl/testrun/internal/command"
	"agentrepl/testrun/internal/run"
)

// TrimFile is cmd/go's record of the cache's last completed trim, in unix
// seconds, inside the cache directory.
const TrimFile = "trim.txt"

const (
	// TrimInterval is how old the last trim may be before a go command trims
	// again (cmd/go's trimInterval).
	TrimInterval = 24 * time.Hour
	// FutureSlack is how far in the future a trim record may lie before cmd/go
	// reads it as corrupt and trims anyway (cmd/go's mtimeInterval).
	FutureSlack = time.Hour
)

// TrimBound is the time limit of the run's own trim: three times the 19s the
// walk of the host's 156 GiB cache took on 2026-11-10 (trim.txt's recorded
// start, 10:53:09Z, against its write, 10:53:28Z), on a host already loaded.
// Every trim logs its duration, so the bound is re-measured from the run log.
const TrimBound = 57 * time.Second

// Guard is the run's keeper of one cache directory.
type Guard struct {
	// Dir is the cache every unit's go commands use (`go env GOCACHE`).
	Dir   string
	Clock run.Clock
	// Trim has cmd/go trim the cache at dir, within ctx (GoTrim).
	Trim func(ctx context.Context, dir string) error
	// Bound is the run's own trim's time limit (TrimBound).
	Bound time.Duration
	Log   *run.Log
}

// Deadline answers when a unit bounded by bound must be killed, once the
// cache's trim is not due before then. When it would be, the cache is
// trimmed first, and the deadline counts from the trim's end.
func (g *Guard) Deadline(unit string, bound time.Duration) (time.Time, error) {
	if g.Bound <= 0 {
		panic(fmt.Sprintf("gocache: the guard of %s has no trim bound (%v); every wait is bounded", g.Dir, g.Bound))
	}
	now := g.Clock.Now()
	last, err := g.lastTrim()
	if err != nil {
		return time.Time{}, err
	}
	if untrimmedThrough(last, now, bound) {
		return now.Add(bound), nil
	}
	g.Log.Infof("go build cache: cmd/go's daily trim of %s (last trim %s) falls due before unit %s's time limit of %s ends, so the run trims it first, bounded at %s", g.Dir, describe(last), unit, bound, g.Bound)
	ctx, cancel := context.WithTimeout(context.Background(), g.Bound)
	defer cancel()
	began := g.Clock.Now()
	if err := g.Trim(ctx, g.Dir); err != nil {
		if errors.Is(ctx.Err(), context.DeadlineExceeded) {
			return time.Time{}, fmt.Errorf("gocache: trimming %s before unit %s did not finish within its bound of %s: %w", g.Dir, unit, g.Bound, err)
		}
		return time.Time{}, fmt.Errorf("gocache: trimming %s before unit %s failed: %w", g.Dir, unit, err)
	}
	now = g.Clock.Now()
	last, err = g.lastTrim()
	if err != nil {
		return time.Time{}, err
	}
	if !untrimmedThrough(last, now, bound) {
		return time.Time{}, fmt.Errorf("gocache: cmd/go trimmed %s before unit %s, but its trim record still says %s, so the trim falls due within the unit's time limit of %s", g.Dir, unit, describe(last), bound)
	}
	g.Log.Infof("go build cache: trimmed %s in %s; its next trim falls due at %s", g.Dir, now.Sub(began), last.Add(TrimInterval).UTC().Format(time.RFC3339))
	return now.Add(bound), nil
}

// untrimmedThrough reports whether every go command that ends between now
// and now+bound skips the trim, by cmd/go's own test: the last trim is less
// than TrimInterval old and no more than FutureSlack ahead. The zero last (no
// record, or one cmd/go cannot read) never is.
func untrimmedThrough(last, now time.Time, bound time.Duration) bool {
	return !last.IsZero() && now.Add(bound).Before(last.Add(TrimInterval)) && now.After(last.Add(-FutureSlack))
}

// lastTrim reads the cache's trim record. A missing record, or one cmd/go
// would not parse, is the zero time: cmd/go trims on either.
func (g *Guard) lastTrim() (time.Time, error) {
	path := filepath.Join(g.Dir, TrimFile)
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return time.Time{}, nil
	}
	if err != nil {
		return time.Time{}, fmt.Errorf("gocache: read the trim record %s: %w", path, err)
	}
	secs, err := strconv.ParseInt(strings.TrimSpace(string(data)), 10, 64)
	if err != nil {
		return time.Time{}, nil
	}
	return time.Unix(secs, 0), nil
}

func describe(last time.Time) string {
	if last.IsZero() {
		return "missing or unreadable"
	}
	return last.UTC().Format(time.RFC3339)
}

// GoTrim has cmd/go trim the cache at dir: `go build -n` of a package with
// no dependencies plans nothing, but closing the cache at its end trims it
// when the trim is due, exactly as any build would. GOPATH mode keeps any
// go.mod above dir, and the toolchain switch it could name, out of it.
func GoTrim(ctx context.Context, dir string) error {
	cmd := command.Bounded(ctx, "go", "build", "-buildvcs=false", "-n", "unsafe")
	cmd.Dir = dir
	cmd.Env = append(os.Environ(), "GOCACHE="+dir, "GO111MODULE=off", "GOWORK=off", "GOFLAGS=")
	out, err := cmd.CombinedOutput()
	return command.Outcome(ctx, TrimBound, "go build -n unsafe in "+dir, "", out, err)
}

// Dir answers the cache directory go commands started with env use.
func Dir(env []string) (string, error) {
	cmd := exec.Command("go", "env", "GOCACHE")
	cmd.Env = env
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("gocache: go env GOCACHE: %w", err)
	}
	dir := strings.TrimSpace(string(out))
	if dir == "" || dir == "off" {
		return "", fmt.Errorf("gocache: go env GOCACHE answered %q; every unit's go commands need a build cache", dir)
	}
	return dir, nil
}
