package webfootprint

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/procmetrics"
)

// now is the sampler's clock; every fixture process started relative to it.
var now = time.Unix(1_000_000, 0)

// fakeReader is a scripted kernel.
type fakeReader struct {
	ids      map[int]procmetrics.Identity
	usage    map[int]procmetrics.Usage
	pidsErr  error
	idErr    map[int]error
	usageErr map[int]error
}

func (f *fakeReader) PIDs() ([]int, error) {
	if f.pidsErr != nil {
		return nil, f.pidsErr
	}
	var out []int
	for pid := range f.ids {
		out = append(out, pid)
	}
	return out, nil
}

func (f *fakeReader) Identity(pid int) (procmetrics.Identity, error) {
	if err := f.idErr[pid]; err != nil {
		return procmetrics.Identity{}, err
	}
	return f.ids[pid], nil
}

func (f *fakeReader) Usage(pid int) (procmetrics.Usage, error) {
	if err := f.usageErr[pid]; err != nil {
		return procmetrics.Usage{}, err
	}
	return f.usage[pid], nil
}

func (f *fakeReader) Args(int) ([]string, error)            { return nil, procmetrics.ErrGone }
func (f *fakeReader) Host(string) (procmetrics.Host, error) { return procmetrics.Host{}, nil }

func webContent(pid int, age time.Duration) procmetrics.Identity {
	return procmetrics.Identity{PID: pid, Name: processName, StartUnixMicros: now.Add(-age).UnixMicro()}
}

func newSampler(t *testing.T, r *fakeReader) (*Sampler, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	s, err := New(Deps{Reader: r, Clock: clocktest.New(now), Log: log})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return s, log
}

func TestSampleRecordsEveryProcessLargestFirst(t *testing.T) {
	// Arrange
	s, log := newSampler(t, &fakeReader{
		ids: map[int]procmetrics.Identity{
			1: webContent(1, time.Minute),
			2: webContent(2, 2*time.Minute),
			3: {PID: 3, Name: "cfprefsd"},
		},
		usage: map[int]procmetrics.Usage{
			1: {FootprintBytes: 1 << 20, ResidentBytes: 1 << 20},
			2: {FootprintBytes: 10 << 30, ResidentBytes: 2 << 20},
		},
	})

	// Act
	s.Sample(context.Background())

	// Assert
	r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: op})
	if !ok {
		t.Fatalf("records = %+v, want the sample at INFO", log.Records())
	}
	procs := r.Context["processes"].([]map[string]any)
	if r.Context["largest_footprint_mb"] != int64(10240) || len(procs) != 2 || procs[0]["pid"] != 2 {
		t.Fatalf("context = %+v, want pid 2 first at 10240 MB", r.Context)
	}
	if procs[0]["resident_mb"] != int64(2) || procs[0]["elapsed"] != "02:00" {
		t.Fatalf("pid 2 = %+v, want 2 MB resident and 02:00 elapsed", procs[0])
	}
}

func TestSampleWithNoWebContentProcessRecordsAnEmptySample(t *testing.T) {
	// Arrange
	s, log := newSampler(t, &fakeReader{ids: map[int]procmetrics.Identity{412: {PID: 412, Name: "cfprefsd"}}})

	// Act
	s.Sample(context.Background())

	// Assert
	r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: op})
	if !ok || r.Context["largest_footprint_mb"] != int64(0) {
		t.Fatalf("records = %+v, want an empty sample at INFO", log.Records())
	}
}

func TestSampleLeavesOutAProcessThatExitedWhileRead(t *testing.T) {
	// Arrange
	s, log := newSampler(t, &fakeReader{
		ids:      map[int]procmetrics.Identity{1: webContent(1, time.Minute), 2: webContent(2, time.Minute)},
		usage:    map[int]procmetrics.Usage{1: {FootprintBytes: 1 << 20}},
		usageErr: map[int]error{2: procmetrics.ErrGone},
	})

	// Act
	s.Sample(context.Background())

	// Assert
	r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: op})
	if !ok {
		t.Fatalf("records = %+v, want the sample at INFO", log.Records())
	}
	if _, failed := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: op}); failed {
		t.Fatalf("an exited process was recorded as a failure")
	}
	if procs := r.Context["processes"].([]map[string]any); len(procs) != 1 || procs[0]["pid"] != 1 {
		t.Fatalf("processes = %+v, want only the live one", procs)
	}
}

func TestSampleRecordsAReadFailureAtErrorAndSkipsTheSample(t *testing.T) {
	denied := errors.New("operation not permitted")
	tests := []struct {
		name    string
		reader  *fakeReader
		message string
	}{
		{name: "the listing fails", reader: &fakeReader{pidsErr: denied}, message: "could not list the web content processes"},
		{
			name:    "an identity read fails",
			reader:  &fakeReader{ids: map[int]procmetrics.Identity{1: webContent(1, time.Minute)}, idErr: map[int]error{1: denied}},
			message: "could not list the web content processes",
		},
		{
			name:    "a footprint read fails",
			reader:  &fakeReader{ids: map[int]procmetrics.Identity{1: webContent(1, time.Minute)}, usageErr: map[int]error{1: denied}},
			message: "could not read a web content process's footprint",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := newSampler(t, tt.reader)

			// Act
			s.Sample(context.Background())

			// Assert
			r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: tt.message})
			if !ok || r.Context["cause"] == "" {
				t.Fatalf("records = %+v, want %q at ERROR with its cause", log.Records(), tt.message)
			}
			if _, sampled := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: op}); sampled {
				t.Fatalf("a failed sample was still recorded at INFO")
			}
		})
	}
}

func TestFormatElapsed(t *testing.T) {
	tests := []struct {
		d    time.Duration
		want string
	}{
		{d: 59 * time.Second, want: "00:59"},
		{d: time.Hour + 2*time.Minute + 3*time.Second, want: "01:02:03"},
		{d: 25*time.Hour + 30*time.Second, want: "1-01:00:30"},
		{d: -time.Second, want: "00:00"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act / Assert
			if got := FormatElapsed(tt.d); got != tt.want {
				t.Fatalf("FormatElapsed(%v) = %q, want %q", tt.d, got, tt.want)
			}
		})
	}
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	// Act
	_, err := New(Deps{Clock: clocktest.New(now), Log: dlog.NewTestLogger()})

	// Assert
	if err == nil {
		t.Fatalf("New accepted a sampler with no reader")
	}
}
