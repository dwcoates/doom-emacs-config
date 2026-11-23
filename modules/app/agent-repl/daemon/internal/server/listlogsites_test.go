package server

import (
	"context"
	"errors"
	"os"
	"strings"
	"sync"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/structpb"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/logsites"
)

// fakeLogSites answers ListLogSites's windows as the test scripts, and
// records every window asked.
type fakeLogSites struct {
	mu     sync.Mutex
	answer logsites.Answer
	err    error
	asked  []logSitesAsk
}

// logSitesAsk is one window a fakeLogSites was asked.
type logSitesAsk struct {
	since, until time.Time
	minimum      string
}

func (f *fakeLogSites) Window(_ context.Context, since, until time.Time, minimum string) (logsites.Answer, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.asked = append(f.asked, logSitesAsk{since: since, until: until, minimum: minimum})
	return f.answer, f.err
}

// logSitesAt is an hour every window of these tests is read around.
var logSitesAt = time.Date(2026, 11, 21, 10, 0, 0, 0, time.UTC)

func TestListLogSitesAsksTheTallyForTheRequestsWindow(t *testing.T) {
	until := logSitesAt.Add(time.Hour).UnixMilli()
	tests := []struct {
		name string
		req  *agentreplv1.ListLogSitesRequest
		want logSitesAsk
	}{
		{
			name: "a whole window at a minimum level",
			req: &agentreplv1.ListLogSitesRequest{
				Window:       &agentreplv1.ListLogSitesWindow{SinceMs: logSitesAt.UnixMilli(), UntilMs: &until},
				MinimumLevel: &agentreplv1.ListLogSitesMinimumLevel{Level: &agentreplv1.ListLogSitesMinimumLevel_Warn{Warn: &agentreplv1.ClientLogLevelWarn{}}},
			},
			want: logSitesAsk{since: logSitesAt, until: logSitesAt.Add(time.Hour), minimum: "warn"},
		},
		{
			name: "no minimum level asks for every level",
			req:  &agentreplv1.ListLogSitesRequest{Window: &agentreplv1.ListLogSitesWindow{SinceMs: logSitesAt.UnixMilli(), UntilMs: &until}},
			want: logSitesAsk{since: logSitesAt, until: logSitesAt.Add(time.Hour)},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.ListLogSites(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("ListLogSites: %v", err)
			}
			got := h.LogSites.asked
			if len(got) != 1 || !got[0].since.Equal(tt.want.since) || !got[0].until.Equal(tt.want.until) || got[0].minimum != tt.want.minimum {
				t.Fatalf("asked %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestListLogSitesAnswersTheTallysSites(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.LogSites.answer = logsites.Answer{
		From: logSitesAt, To: logSitesAt.Add(time.Hour), Partial: true,
		Sites: []logsites.Site{{
			Key: "0123456789ab", Template: "said {what}", Runtime: "daemon", Level: "error", Operation: "daemon.test", Count: 3,
			FirstSeen: logSitesAt.Add(time.Minute), LastSeen: logSitesAt.Add(2 * time.Minute),
			SampleAt: logSitesAt.Add(2 * time.Minute), SampleMessage: "said x", SampleWorkspace: "ws-1", SampleContext: `{"what":"x"}`,
		}},
	}

	// Act
	resp, err := h.Client.ListLogSites(context.Background(), connect.NewRequest(&agentreplv1.ListLogSitesRequest{
		Window: &agentreplv1.ListLogSitesWindow{SinceMs: logSitesAt.UnixMilli()},
	}))

	// Assert
	if err != nil {
		t.Fatalf("ListLogSites: %v", err)
	}
	ws := "ws-1"
	sampleContext, _ := structpb.NewStruct(map[string]any{"what": "x"})
	want := &agentreplv1.ListLogSitesSuccess{
		Covered: &agentreplv1.ListLogSitesCovered{Extent: &agentreplv1.ListLogSitesCovered_Partial{Partial: &agentreplv1.ListLogSitesCoveredPartial{
			SinceMs: logSitesAt.UnixMilli(), UntilMs: logSitesAt.Add(time.Hour).UnixMilli(),
		}}},
		Sites: []*agentreplv1.LogSite{{
			Site: &agentreplv1.LogSiteKey{Hex: "0123456789ab"}, Template: "said {what}", Runtime: "daemon",
			Level:     &agentreplv1.LogSiteLevel{Level: &agentreplv1.LogSiteLevel_Error{Error: &agentreplv1.ClientLogLevelError{}}},
			Operation: "daemon.test", Count: 3,
			FirstSeenMs: logSitesAt.Add(time.Minute).UnixMilli(), LastSeenMs: logSitesAt.Add(2 * time.Minute).UnixMilli(),
			Sample: &agentreplv1.LogSiteSample{AtMs: logSitesAt.Add(2 * time.Minute).UnixMilli(), Message: "said x", WorkspaceId: &ws, Context: sampleContext},
		}},
	}
	if got := resp.Msg.GetSuccess(); !proto.Equal(got, want) {
		t.Fatalf("success = %v, want %v", got, want)
	}
}

func TestListLogSitesAnswersAWholeWindowAsWhole(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.LogSites.answer = logsites.Answer{From: logSitesAt, To: logSitesAt.Add(time.Hour)}

	// Act
	resp, err := h.Client.ListLogSites(context.Background(), connect.NewRequest(&agentreplv1.ListLogSitesRequest{
		Window: &agentreplv1.ListLogSitesWindow{SinceMs: logSitesAt.UnixMilli()},
	}))

	// Assert
	whole := resp.Msg.GetSuccess().GetCovered().GetWhole()
	if err != nil || whole.GetSinceMs() != logSitesAt.UnixMilli() || len(resp.Msg.GetSuccess().GetSites()) != 0 {
		t.Fatalf("ListLogSites = (%v, %v), want a whole window with no site", resp.Msg, err)
	}
}

func TestListLogSitesAnswersAnUnrecordedWindowAsItsErrorArm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.LogSites.err = &logsites.UnrecordedWindowError{Oldest: logSitesAt}

	// Act
	resp, err := h.Client.ListLogSites(context.Background(), connect.NewRequest(&agentreplv1.ListLogSitesRequest{
		Window: &agentreplv1.ListLogSitesWindow{SinceMs: logSitesAt.Add(-48 * time.Hour).UnixMilli()},
	}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnrecordedWindow().GetOldestSinceMs() != logSitesAt.UnixMilli() {
		t.Fatalf("ListLogSites = (%v, %v), want the unrecorded window naming the oldest hour", resp.Msg, err)
	}
}

func TestListLogSitesRefusesAMalformedRequest(t *testing.T) {
	before := logSitesAt.Add(-time.Hour).UnixMilli()
	tests := []struct {
		name  string
		req   *agentreplv1.ListLogSitesRequest
		tally error
		want  string
	}{
		{name: "no window", req: &agentreplv1.ListLogSitesRequest{}, want: "window: is required"},
		{name: "an end before its start", req: &agentreplv1.ListLogSitesRequest{Window: &agentreplv1.ListLogSitesWindow{SinceMs: logSitesAt.UnixMilli(), UntilMs: &before}}, want: "precedes since_ms"},
		{name: "a minimum level naming no level", req: &agentreplv1.ListLogSitesRequest{
			Window: &agentreplv1.ListLogSitesWindow{SinceMs: logSitesAt.UnixMilli()}, MinimumLevel: &agentreplv1.ListLogSitesMinimumLevel{},
		}, want: "minimum_level: names no level"},
		{name: "a window the tally finds malformed", req: &agentreplv1.ListLogSitesRequest{Window: &agentreplv1.ListLogSitesWindow{SinceMs: logSitesAt.UnixMilli()}},
			tally: &logsites.MalformedWindowError{Reason: "bad"}, want: "window: bad"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.LogSites.err = tt.tally

			// Act
			_, err := h.Client.ListLogSites(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("ListLogSites = %v, want InvalidArgument naming %q", err, tt.want)
			}
		})
	}
}

func TestListLogSitesFailsWhenTheTallyFails(t *testing.T) {
	tests := []struct {
		name   string
		answer logsites.Answer
		err    error
		cause  string
	}{
		{name: "the tally's own failure", err: errors.New("disk full"), cause: "disk full"},
		{name: "a sample context that is no JSON object", answer: logsites.Answer{Sites: []logsites.Site{{Key: "0123456789ab", Level: "info", SampleContext: "[1]"}}}, cause: "is not a JSON object"},
		{name: "a level the tally does not keep", answer: logsites.Answer{Sites: []logsites.Site{{Key: "0123456789ab", Level: "fatal", SampleContext: "{}"}}}, cause: "no level the tally keeps"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			h.LogSites.answer, h.LogSites.err = tt.answer, tt.err

			// Act
			_, err := h.Client.ListLogSites(context.Background(), connect.NewRequest(&agentreplv1.ListLogSitesRequest{
				Window: &agentreplv1.ListLogSitesWindow{SinceMs: logSitesAt.UnixMilli()},
			}))

			// Assert
			if connect.CodeOf(err) != connect.CodeInternal {
				t.Fatalf("ListLogSites = %v, want Internal", err)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "ListLogSites", Message: "the rpc failed"})
			if !ok || !strings.Contains(rec.Context["cause"].(string), tt.cause) {
				t.Fatalf("records = %+v, want the failure at ERROR naming %q", log.Records(), tt.cause)
			}
		})
	}
}

func TestNewRefusesAMissingLogSiteTally(t *testing.T) {
	// Arrange
	var built Deps
	newHarness(t, func(d *Deps) { built = *d })
	built.LogSites = nil

	// Act
	_, err := New(built)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the log site tally") {
		t.Fatalf("New() error = %v, want it to name the log site tally", err)
	}
}

func TestLogSitesMinimumRoundTripsEveryLevel(t *testing.T) {
	for _, name := range []string{"debug", "info", "warn", "error"} {
		t.Run(name, func(t *testing.T) {
			// Act
			m, ok := LogSitesMinimum(name)
			back, backOK := LogSitesMinimumName(m)

			// Assert
			if !ok || !backOK || back != name {
				t.Fatalf("LogSitesMinimum(%q) = (%v, %v), its name (%q, %v); want the level back", name, m, ok, back, backOK)
			}
		})
	}
}

func TestLogSitesMinimumRefusesWhatNamesNoLevel(t *testing.T) {
	// Act
	m, ok := LogSitesMinimum("fatal")
	name, nameOK := LogSitesMinimumName(&agentreplv1.ListLogSitesMinimumLevel{})

	// Assert
	if ok || m != nil || nameOK || name != "" {
		t.Fatalf("an unknown name = (%v, %v), an unset arm = (%q, %v); want both refused", m, ok, name, nameOK)
	}
}

// TestEveryMinimumLevelArmIsSpelledOnlyInTheOneTable fails a second
// hand-kept mapping of the minimum level's arms in the server
// (logscli_test.go holds the CLI to none).
func TestEveryMinimumLevelArmIsSpelledOnlyInTheOneTable(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("listlogsites.go")
	if err != nil {
		t.Fatalf("read listlogsites.go: %v", err)
	}

	// Act
	got := strings.Count(string(src), "ListLogSitesMinimumLevel_")

	// Assert
	if got != 4 {
		t.Fatalf("listlogsites.go spells a minimum level arm %d times, want 4, each once in logSitesMinimums", got)
	}
}
