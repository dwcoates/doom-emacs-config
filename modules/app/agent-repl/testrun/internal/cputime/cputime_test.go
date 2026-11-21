package cputime

import (
	"agentrepl/testrun/internal/cores"
	"bytes"
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/testtimes"
)

// reads scripts a sampler's reads: each read answers the next growth, or the
// next error; the clock advances one second per read.
type reads struct {
	growth []map[string]float64
	errs   []error
	i      int
	now    time.Time
}

func (r *reads) read() (map[string]float64, error) {
	i := r.i
	r.i++
	r.now = r.now.Add(time.Second)
	if i < len(r.errs) && r.errs[i] != nil {
		return nil, r.errs[i]
	}
	return r.growth[i], nil
}

func newSampler(r *reads, log *run.Log) *Sampler {
	return &Sampler{Read: r.read, Now: func() time.Time { return r.now }, Log: log, Pid: 7}
}

func TestSamplerKeepsEachOfThisRunsUnitsIntervals(t *testing.T) {
	// Arrange: the first read is the baseline; another run's unit (8/x) is
	// on the host too.
	t0 := time.Unix(1_800_000_000, 0)
	r := &reads{now: t0, growth: []map[string]float64{
		{"7/a": 5},
		{"7/a": 1, "7/b": 0.5, "8/x": 3},
		{"7/a": 0.25, "7/b": 0},
	}}
	s := newSampler(r, &run.Log{Out: &bytes.Buffer{}, Err: &bytes.Buffer{}})

	// Act
	for range r.growth {
		s.read()
	}

	// Assert
	wantA := []testtimes.CPUSlice{
		{From: t0.Add(time.Second), To: t0.Add(2 * time.Second), CPU: 1},
		{From: t0.Add(2 * time.Second), To: t0.Add(3 * time.Second), CPU: 0.25},
	}
	if got := s.Take("a"); !reflect.DeepEqual(got, wantA) {
		t.Fatalf("Take(a) = %+v; want %+v", got, wantA)
	}
	if got := s.Take("b"); len(got) != 1 || got[0].CPU != 0.5 {
		t.Fatalf("Take(b) = %+v; want its one interval of 0.5s", got)
	}
	if got := s.Take("a"); got != nil {
		t.Fatalf("a second Take(a) = %+v; want nothing, the first took them", got)
	}
	if _, kept := s.slices["8/x"]; kept {
		t.Fatalf("another run's unit 8/x was kept: %+v", s.slices)
	}
}

func TestSamplerCountsAFailedReadsIntervalInTheNextAndLogsIt(t *testing.T) {
	// Arrange
	t0 := time.Unix(1_800_000_000, 0)
	r := &reads{now: t0,
		growth: []map[string]float64{{}, nil, {"7/a": 2}},
		errs:   []error{nil, errors.New("proc_info answered EIO"), nil},
	}
	errOut := &bytes.Buffer{}
	s := newSampler(r, &run.Log{Out: &bytes.Buffer{}, Err: errOut})

	// Act
	for range r.growth {
		s.read()
	}

	// Assert: one interval from the last good read, and the failure logged
	// with the instants it spans.
	want := []testtimes.CPUSlice{{From: t0.Add(time.Second), To: t0.Add(3 * time.Second), CPU: 2}}
	if got := s.Take("a"); !reflect.DeepEqual(got, want) {
		t.Fatalf("Take(a) = %+v; want %+v", got, want)
	}
	logged := errOut.String()
	for _, part := range []string{"ERROR", "reading the units' CPU", "proc_info answered EIO", t0.Add(time.Second).Format(time.RFC3339Nano)} {
		if !strings.Contains(logged, part) {
			t.Fatalf("log %q does not say %q", logged, part)
		}
	}
}

func TestSamplerTagsAUnitWithThisRunsPid(t *testing.T) {
	s := &Sampler{Pid: 42}
	if got, want := s.Tag("daemon:integration#00"), cores.UnitTag(42, "daemon:integration#00"); got != want {
		t.Fatalf("Tag = %q, want the shared unit tag %q", got, want)
	}
}

func TestSamplerRunReadsAtEveryTickUntilItsContextEnds(t *testing.T) {
	// Arrange
	t0 := time.Unix(1_800_000_000, 0)
	r := &reads{now: t0, growth: []map[string]float64{{}, {"7/a": 1}, {"7/a": 1}}}
	ticks := make(chan time.Time)
	s := newSampler(r, &run.Log{Out: &bytes.Buffer{}, Err: &bytes.Buffer{}})
	s.Ticks = ticks
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})

	// Act: the baseline read, then two ticks.
	go func() { s.Run(ctx); close(done) }()
	ticks <- time.Time{}
	ticks <- time.Time{}
	cancel()
	<-done

	// Assert
	if got := s.Take("a"); len(got) != 2 {
		t.Fatalf("Take(a) = %+v; want two intervals", got)
	}
}
