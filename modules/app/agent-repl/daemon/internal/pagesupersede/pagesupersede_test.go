package pagesupersede

import (
	"fmt"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

const (
	ws    = ids.WorkspaceID("w1")
	other = ids.WorkspaceID("w2")
)

// served is one Served call.
type served struct {
	page string
	ws   ids.WorkspaceID
}

func newTracker(t *testing.T) (*Tracker, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	tr, err := New(Deps{Log: log})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return tr, log
}

func raised(log *dlog.TestLogger) []dlog.Record {
	return dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: "error"})
}

func TestServedRaisesOnlyASupersededPage(t *testing.T) {
	tests := []struct {
		name  string
		calls []served
		want  []string
	}{
		{name: "a page's own subscriptions are ordinary", calls: []served{{"a", ws}, {"a", ws}, {"a", ws}}},
		{name: "a newer page taking over is ordinary", calls: []served{{"a", ws}, {"b", ws}, {"b", ws}}},
		{name: "a replaced page subscribing again is raised", calls: []served{{"a", ws}, {"b", ws}, {"a", ws}}, want: []string{"a"}},
		{name: "a replaced page is raised once however often it subscribes", calls: []served{{"a", ws}, {"b", ws}, {"a", ws}, {"a", ws}, {"a", ws}}, want: []string{"a"}},
		{name: "pages of different workspaces never supersede each other", calls: []served{{"a", ws}, {"b", other}, {"a", ws}, {"b", other}}},
		{name: "every replaced page is raised", calls: []served{{"a", ws}, {"b", ws}, {"c", ws}, {"a", ws}, {"b", ws}}, want: []string{"a", "b"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			tr, log := newTracker(t)

			// Act
			for _, c := range tt.calls {
				tr.Served(c.page, c.ws)
			}

			// Assert
			var got []string
			for _, r := range raised(log) {
				got = append(got, fmt.Sprint(r.Context["page"]))
			}
			if fmt.Sprint(got) != fmt.Sprint(tt.want) {
				t.Fatalf("raised pages %v, want %v", got, tt.want)
			}
		})
	}
}

func TestServedRecordsTheSupersessionsDiagnosis(t *testing.T) {
	// Arrange
	tr, log := newTracker(t)
	for _, p := range []string{"a", "b", "c"} {
		tr.Served(p, ws)
	}

	// Act
	tr.Served("a", ws)

	// Assert
	got := raised(log)
	if len(got) != 1 || got[0].Operation != op {
		t.Fatalf("records = %+v, want one %s error", log.Records(), op)
	}
	want := dlog.Context{"page": "a", "workspace": "w1", "superseded_by": "c", "pages_since": 2}
	for k, v := range want {
		if got[0].Context[k] != v {
			t.Fatalf("context[%q] = %#v, want %#v (whole: %+v)", k, got[0].Context[k], v, got[0].Context)
		}
	}
}

func TestServedForgetsPagesBeyondRemembered(t *testing.T) {
	// Arrange: the first page, then Remembered newer ones, which push it out.
	tr, log := newTracker(t)
	tr.Served("first", ws)
	for i := range Remembered {
		tr.Served(fmt.Sprintf("p%d", i), ws)
	}

	// Act: the forgotten page serves again (as a new page, so not raised),
	// then p5, still remembered and long replaced, serves again.
	tr.Served("first", ws)
	tr.Served("p5", ws)

	// Assert
	var got []string
	for _, r := range raised(log) {
		got = append(got, fmt.Sprint(r.Context["page"]))
	}
	if fmt.Sprint(got) != "[p5]" {
		t.Fatalf("raised pages %v, want [p5]", got)
	}
}

func TestNewRequiresALogger(t *testing.T) {
	// Act
	_, err := New(Deps{})

	// Assert
	if err == nil {
		t.Fatal("New with no logger succeeded, want an error")
	}
}
