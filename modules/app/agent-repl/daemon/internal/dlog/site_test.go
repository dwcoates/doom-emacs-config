package dlog_test

import (
	"errors"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agentrepl/logging/logsite"
)

// sitePackage is this test package's path, as a calling function names it.
const sitePackage = "agent-repld/internal/dlog_test."

// only answers the one record LOG captured, failing the test otherwise.
func only(t *testing.T, log *dlog.TestLogger) dlog.Record {
	t.Helper()
	records := log.Records()
	if len(records) != 1 {
		t.Fatalf("records = %+v, want exactly one", records)
	}
	return records[0]
}

func TestADirectCallIsKeyedByTheFunctionItIsWrittenIn(t *testing.T) {
	tests := []struct {
		name     string
		template string
		emit     func(dlog.Logger)
	}{
		{name: "debug", template: "a debug record", emit: func(l dlog.Logger) { l.Debug("daemon.test.site", "a debug record", nil) }},
		{name: "info", template: "an info record", emit: func(l dlog.Logger) { l.Info("daemon.test.site", "an info record", nil) }},
		{name: "warn", template: "a warn record", emit: func(l dlog.Logger) { l.Warn("daemon.test.site", "a warn record", nil) }},
		{name: "error", template: "an error record", emit: func(l dlog.Logger) { l.Error("daemon.test.site", "an error record", nil) }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()

			// Act: the call is written in a closure of this test, so it is
			// keyed by the test function.
			tc.emit(log)

			// Assert.
			rec := only(t, log)
			want := logsite.Key(dlog.RuntimeDaemon, sitePackage+"TestADirectCallIsKeyedByTheFunctionItIsWrittenIn", tc.template)
			if rec.Site != want || rec.Template != tc.template {
				t.Fatalf("site, template = %q, %q; want %q, %q", rec.Site, rec.Template, want, tc.template)
			}
		})
	}
}

func TestADerivedLoggersCallIsKeyedByTheFunctionItIsWrittenIn(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	log.With(dlog.Context{"bound": true}).Info("daemon.test.site", "through a derived logger", nil)

	// Assert.
	want := logsite.Key(dlog.RuntimeDaemon, sitePackage+"TestADerivedLoggersCallIsKeyedByTheFunctionItIsWrittenIn", "through a derived logger")
	if rec := only(t, log); rec.Site != want {
		t.Fatalf("site = %q, want %q", rec.Site, want)
	}
}

// forward is a helper that logs the message its caller wrote.
func forward(log dlog.Logger, message logsite.Message) {
	log.Log(dlog.LevelInfo, "daemon.test.site", message, nil)
}

func TestAForwardedMessageIsKeyedByTheFunctionThatMadeIt(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	forward(log, logsite.Msg("forwarded by a helper"))

	// Assert.
	want := logsite.Key(dlog.RuntimeDaemon, sitePackage+"TestAForwardedMessageIsKeyedByTheFunctionThatMadeIt", "forwarded by a helper")
	if rec := only(t, log); rec.Site != want || rec.Level != dlog.LevelInfo {
		t.Fatalf("record = %+v, want an info record keyed %q, not by the helper", rec, want)
	}
}

func TestAMessageRendersFromItsNamedValues(t *testing.T) {
	tests := []struct {
		name string
		emit func(dlog.Logger)
		want string
	}{
		{name: "a value of the call's context",
			emit: func(l dlog.Logger) {
				l.Info("daemon.test.render", "shim {shim} timed out", dlog.Context{"shim": "s-1"})
			},
			want: "shim s-1 timed out"},
		{name: "a value the logger was bound with",
			emit: func(l dlog.Logger) {
				l.With(dlog.Context{"shim": "s-2"}).Info("daemon.test.render", "shim {shim} timed out", nil)
			},
			want: "shim s-2 timed out"},
		{name: "an error value reads as its text",
			emit: func(l dlog.Logger) {
				l.Error("daemon.test.render", "failed: {cause}", dlog.Context{"cause": errors.New("boom")})
			},
			want: "failed: boom"},
		{name: "a number reads as it prints",
			emit: func(l dlog.Logger) { l.Info("daemon.test.render", "{n} rows", dlog.Context{"n": 3}) },
			want: "3 rows"},
		{name: "a doubled brace is a literal one",
			emit: func(l dlog.Logger) { l.Info("daemon.test.render", "a {{literal}} brace", nil) },
			want: "a {literal} brace"},
		{name: "a forwarded message renders too",
			emit: func(l dlog.Logger) {
				l.Log(dlog.LevelWarn, "daemon.test.render", logsite.Msg("{what} is held"), dlog.Context{"what": "the queue"})
			},
			want: "the queue is held"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()

			// Act.
			tc.emit(log)

			// Assert.
			if rec := only(t, log); rec.Message != tc.want {
				t.Fatalf("message = %q, want %q", rec.Message, tc.want)
			}
		})
	}
}

// panicOf answers what F panicked with, or nil.
func panicOf(f func()) (got any) {
	defer func() { got = recover() }()
	f()
	return nil
}

func TestAPlaceholderTheRecordDoesNotHoldPanicsNamingTheOperation(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	got := panicOf(func() { log.Info("daemon.test.missing", "shim {shim} timed out", dlog.Context{"other": 1}) })

	// Assert.
	text, _ := got.(string)
	if !strings.Contains(text, "daemon.test.missing") || !strings.Contains(text, "{shim}") {
		t.Fatalf("panic = %v, want one naming the operation and the placeholder", got)
	}
}

func TestAMessageNoMsgMadePanics(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	got := panicOf(func() { log.Log(dlog.LevelInfo, "daemon.test.zero", logsite.Message{}, nil) })

	// Assert.
	if text, _ := got.(string); !strings.Contains(text, "logsite.Msg") {
		t.Fatalf("panic = %v, want one naming logsite.Msg", got)
	}
}
