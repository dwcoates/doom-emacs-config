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
// The footprint is the kernel's phys_footprint: what it charges the
// process, compressed and swapped pages included, which resident size alone
// misses by a factor of five here. It is read through the metrics
// collector's kernel reader (procmetrics), so the log record and the
// agentrepl_process_footprint_bytes series read the same figure, and no tool
// is spawned to read it.
package webfootprint

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"time"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/procmetrics"
)

// Every is how often the footprints are sampled.
const Every = time.Minute

// processName is the web content process's name.
const processName = "com.apple.WebKit.WebContent"

const op = "daemon.webfootprint"

// Deps are the sampler's collaborators, every one required.
type Deps struct {
	Reader procmetrics.Reader
	Clock  clock.Clock
	Log    dlog.Logger
}

// Sampler samples the web content processes' footprints.
type Sampler struct {
	deps Deps
}

// New builds a sampler.
func New(deps Deps) (*Sampler, error) {
	if deps.Reader == nil || deps.Clock == nil || deps.Log == nil {
		return nil, errors.New("webfootprint: a reader, a clock and a logger are required")
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
// largest first, in one record. A read that fails is recorded at ERROR and
// the sample is skipped; a process that exits while it is read is left out.
func (s *Sampler) Sample(context.Context) {
	procs, err := s.list()
	if err != nil {
		s.deps.Log.Error(op, "could not list the web content processes", dlog.Context{"cause": err.Error()})
		return
	}
	read := procs[:0]
	for _, p := range procs {
		u, err := s.deps.Reader.Usage(p.PID)
		if errors.Is(err, procmetrics.ErrGone) {
			continue
		}
		if err != nil {
			s.deps.Log.Error(op, "could not read a web content process's footprint", dlog.Context{
				"pid": p.PID, "cause": err.Error(),
			})
			return
		}
		p.FootprintBytes, p.ResidentBytes = int64(u.FootprintBytes), int64(u.ResidentBytes)
		read = append(read, p)
	}
	procs = read
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

// list answers every web content process on the host with its age.
func (s *Sampler) list() ([]Process, error) {
	pids, err := s.deps.Reader.PIDs()
	if err != nil {
		return nil, err
	}
	now := s.deps.Clock.Now()
	var procs []Process
	for _, pid := range pids {
		id, err := s.deps.Reader.Identity(pid)
		if errors.Is(err, procmetrics.ErrGone) {
			continue
		}
		if err != nil {
			return nil, err
		}
		if id.Name != processName {
			continue
		}
		started := time.UnixMicro(id.StartUnixMicros)
		procs = append(procs, Process{PID: pid, Elapsed: FormatElapsed(now.Sub(started))})
	}
	return procs, nil
}

// FormatElapsed renders an age the way `ps -o etime` does, which is how
// these records have always spelled it: [[dd-]hh:]mm:ss.
func FormatElapsed(d time.Duration) string {
	if d < 0 {
		d = 0
	}
	total := int64(d / time.Second)
	days, rem := total/86400, total%86400
	hours, rem := rem/3600, rem%3600
	minutes, seconds := rem/60, rem%60
	switch {
	case days > 0:
		return fmt.Sprintf("%d-%02d:%02d:%02d", days, hours, minutes, seconds)
	case hours > 0:
		return fmt.Sprintf("%02d:%02d:%02d", hours, minutes, seconds)
	default:
		return fmt.Sprintf("%02d:%02d", minutes, seconds)
	}
}
