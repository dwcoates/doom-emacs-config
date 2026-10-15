package webfootprint

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
)

const webContent = "/System/Library/Frameworks/WebKit.framework/Versions/A/XPCServices/com.apple.WebKit.WebContent.xpc/Contents/MacOS/com.apple.WebKit.WebContent"

// fakeRunner answers each tool by its name.
type fakeRunner struct {
	answers map[string]answer
}

type answer struct {
	out  string
	code int
	err  error
}

func (f *fakeRunner) Run(_ context.Context, _ string, argv []string) (string, int, error) {
	key := argv[0]
	if argv[0] == "footprint" {
		key = "footprint " + argv[len(argv)-1]
	}
	a, ok := f.answers[key]
	if !ok {
		return "", 0, errors.New("unexpected tool " + strings.Join(argv, " "))
	}
	return a.out, a.code, a.err
}

func newSampler(t *testing.T, answers map[string]answer) (*Sampler, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	s, err := New(Deps{Runner: &fakeRunner{answers: answers}, Clock: clocktest.New(time.Unix(0, 0)), Log: log})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return s, log
}

func footprintOut(bytes string) string {
	return "    49152 B   0 B   0 B   1   Owned physical footprint (unmapped)\n    phys_footprint: " + bytes + " B\n    phys_footprint_peak: 99 B\n"
}

func TestParsePS(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    []Process
		wantErr bool
	}{
		{
			name: "keeps only web content processes",
			out:  "83365 5496384 01-01:01:56 " + webContent + "\n  412 1024 10:00 /usr/sbin/cfprefsd\n",
			want: []Process{{PID: 83365, ResidentBytes: 5496384 << 10, Elapsed: "01-01:01:56"}},
		},
		{name: "answers none for no web content process", out: "  412 1024 10:00 /usr/sbin/cfprefsd\n"},
		{name: "refuses a web content line whose pid does not parse", out: "x 1 1:00 " + webContent, wantErr: true},
		{name: "refuses a web content line whose rss does not parse", out: "1 x 1:00 " + webContent, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParsePS(tt.out)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("err = %v, want error %v", err, tt.wantErr)
			}
			if len(got) != len(tt.want) || (len(got) == 1 && got[0] != tt.want[0]) {
				t.Fatalf("got %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestParseFootprint(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    int64
		wantErr bool
	}{
		{name: "reads phys_footprint, never its peak", out: footprintOut("10869950440"), want: 10869950440},
		{name: "refuses output with no phys_footprint", out: "nothing here\n", wantErr: true},
		{name: "refuses a phys_footprint not in bytes", out: "phys_footprint: 10 GB\n", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseFootprint(tt.out)

			// Assert
			if (err != nil) != tt.wantErr || got != tt.want {
				t.Fatalf("ParseFootprint = (%d, %v), want (%d, error %v)", got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func TestSampleRecordsEveryProcessLargestFirst(t *testing.T) {
	// Arrange
	s, log := newSampler(t, map[string]answer{
		"ps":          {out: "1 1024 1:00 " + webContent + "\n2 2048 2:00 " + webContent + "\n"},
		"footprint 1": {out: footprintOut("1048576")},
		"footprint 2": {out: footprintOut("10737418240")},
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
}

func TestSampleWithNoWebContentProcessRecordsAnEmptySample(t *testing.T) {
	// Arrange
	s, log := newSampler(t, map[string]answer{"ps": {out: "  412 1024 10:00 /usr/sbin/cfprefsd\n"}})

	// Act
	s.Sample(context.Background())

	// Assert
	r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: op})
	if !ok || r.Context["largest_footprint_mb"] != int64(0) {
		t.Fatalf("records = %+v, want an empty sample at INFO", log.Records())
	}
}

func TestSampleRecordsAToolFailureAtErrorAndSkipsTheSample(t *testing.T) {
	tests := []struct {
		name    string
		answers map[string]answer
		message string
	}{
		{name: "ps cannot run", answers: map[string]answer{"ps": {err: errors.New("spawn failed")}}, message: "could not list the web content processes"},
		{name: "ps exits non-zero", answers: map[string]answer{"ps": {out: "denied", code: 1}}, message: "could not list the web content processes"},
		{
			name:    "footprint cannot run",
			answers: map[string]answer{"ps": {out: "1 1 1:00 " + webContent}, "footprint 1": {err: errors.New("spawn failed")}},
			message: "could not read a web content process's footprint",
		},
		{
			name:    "footprint exits non-zero",
			answers: map[string]answer{"ps": {out: "1 1 1:00 " + webContent}, "footprint 1": {out: "no such process", code: 1}},
			message: "could not read a web content process's footprint",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := newSampler(t, tt.answers)

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

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	// Act
	_, err := New(Deps{Clock: clocktest.New(time.Unix(0, 0)), Log: dlog.NewTestLogger()})

	// Assert
	if err == nil {
		t.Fatalf("New accepted a sampler with no runner")
	}
}
