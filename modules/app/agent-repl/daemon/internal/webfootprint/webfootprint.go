// Package webfootprint records the memory footprint of every WebKit web
// content process on the host, once a minute.
//
// WHY IT EXISTS (2026-10-15). Every workspace page is an iframe of the one
// webhost, so all of them live in ONE web content process. On 2026-10-15
// that process had run 25 hours and stood at a 10 GB footprint (1.8 GB
// resident), and its main thread stalled every 30 seconds for 0.4-1.5s
// (`webhost.stall`). Nothing recorded the footprint, so whether it grows
// with the day, and whether the stalls follow it, could not be read from
// the logs.
//
// The footprint is `footprint`'s phys_footprint: what the kernel charges
// the process, compressed and swapped pages included, which resident size
// alone misses by a factor of five here.
package webfootprint

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"strconv"
	"strings"
	"time"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
)

// Every is how often the footprints are sampled.
const Every = time.Minute

// processName is the web content process's command name.
const processName = "com.apple.WebKit.WebContent"

// runDir is the directory both tools run in; neither reads it.
const runDir = "/"

const op = "daemon.webfootprint"

// Runner runs one tool and answers its combined output and exit code.
type Runner interface {
	Run(ctx context.Context, dir string, argv []string) (string, int, error)
}

// Deps are the sampler's collaborators, every one required.
type Deps struct {
	Runner Runner
	Clock  clock.Clock
	Log    dlog.Logger
}

// Sampler samples the web content processes' footprints.
type Sampler struct {
	deps Deps
}

// New builds a sampler.
func New(deps Deps) (*Sampler, error) {
	if deps.Runner == nil || deps.Clock == nil || deps.Log == nil {
		return nil, errors.New("webfootprint: a runner, a clock and a logger are required")
	}
	return &Sampler{deps: deps}, nil
}

// Process is one web content process's sample.
type Process struct {
	PID            int
	FootprintBytes int64
	ResidentBytes  int64
	Elapsed        string
}

// Run samples every Every until ctx ends.
func (s *Sampler) Run(ctx context.Context) {
	_ = clock.Repeat(ctx, s.deps.Clock, Every, Every, s.Sample)
}

// Sample records one sample: every web content process's footprint, the
// largest first, in one record. A tool that cannot be run or read is
// recorded at ERROR and the sample is skipped.
func (s *Sampler) Sample(ctx context.Context) {
	procs, err := s.list(ctx)
	if err != nil {
		s.deps.Log.Error(op, "could not list the web content processes", dlog.Context{"cause": err.Error()})
		return
	}
	for i := range procs {
		bytes, err := s.footprint(ctx, procs[i].PID)
		if err != nil {
			s.deps.Log.Error(op, "could not read a web content process's footprint", dlog.Context{
				"pid": procs[i].PID, "cause": err.Error(),
			})
			return
		}
		procs[i].FootprintBytes = bytes
	}
	sort.Slice(procs, func(i, j int) bool { return procs[i].FootprintBytes > procs[j].FootprintBytes })
	listed := make([]map[string]any, 0, len(procs))
	for _, p := range procs {
		listed = append(listed, map[string]any{
			"pid": p.PID, "footprint_mb": p.FootprintBytes >> 20, "resident_mb": p.ResidentBytes >> 20, "elapsed": p.Elapsed,
		})
	}
	largest := int64(0)
	if len(procs) > 0 {
		largest = procs[0].FootprintBytes >> 20
	}
	s.deps.Log.Info(op, fmt.Sprintf("the largest web content process's footprint is %d MB", largest), dlog.Context{
		"processes": listed, "largest_footprint_mb": largest,
	})
}

// list answers every web content process with its resident size and age.
func (s *Sampler) list(ctx context.Context) ([]Process, error) {
	out, code, err := s.deps.Runner.Run(ctx, runDir, []string{"ps", "-A", "-o", "pid=,rss=,etime=,comm="})
	if err != nil {
		return nil, fmt.Errorf("run ps: %w", err)
	}
	if code != 0 {
		return nil, fmt.Errorf("ps exited %d: %s", code, strings.TrimSpace(out))
	}
	return ParsePS(out)
}

// footprint answers PID's phys_footprint in bytes.
func (s *Sampler) footprint(ctx context.Context, pid int) (int64, error) {
	out, code, err := s.deps.Runner.Run(ctx, runDir, []string{"footprint", "-f", "bytes", "-p", strconv.Itoa(pid)})
	if err != nil {
		return 0, fmt.Errorf("run footprint: %w", err)
	}
	if code != 0 {
		return 0, fmt.Errorf("footprint exited %d: %s", code, strings.TrimSpace(out))
	}
	return ParseFootprint(out)
}

// ParsePS answers the web content processes in `ps -o pid=,rss=,etime=,comm=`
// output; rss is in KiB. A line that names a web content process but does not
// parse is an error, never a skipped process.
func ParsePS(out string) ([]Process, error) {
	var procs []Process
	for line := range strings.SplitSeq(out, "\n") {
		fields := strings.Fields(line)
		if len(fields) < 4 || !strings.HasSuffix(strings.Join(fields[3:], " "), processName) {
			continue
		}
		pid, err := strconv.Atoi(fields[0])
		if err != nil {
			return nil, fmt.Errorf("ps line %q: pid: %w", line, err)
		}
		rss, err := strconv.ParseInt(fields[1], 10, 64)
		if err != nil {
			return nil, fmt.Errorf("ps line %q: rss: %w", line, err)
		}
		procs = append(procs, Process{PID: pid, ResidentBytes: rss << 10, Elapsed: fields[2]})
	}
	return procs, nil
}

// ParseFootprint answers the phys_footprint in `footprint -f bytes` output.
func ParseFootprint(out string) (int64, error) {
	for line := range strings.SplitSeq(out, "\n") {
		rest, ok := strings.CutPrefix(strings.TrimSpace(line), "phys_footprint:")
		if !ok {
			continue
		}
		fields := strings.Fields(rest)
		if len(fields) != 2 || fields[1] != "B" {
			return 0, fmt.Errorf("footprint line %q is not a byte count", line)
		}
		return strconv.ParseInt(fields[0], 10, 64)
	}
	return 0, errors.New("footprint output names no phys_footprint")
}
