package profiling

import (
	"agentrepl/logging/logsite"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

func TestArmRefusesAMissingHomeWithNoOverride(t *testing.T) {
	// Arrange
	getenv := func(string) string { return "" }
	home := func() (string, error) { return "", errors.New("no home") }

	// Act
	_, err := Arm("daemon", &fakeLevel{}, func(Event) {}, getenv, home)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the profiles root needs a home directory: no home") {
		t.Fatalf("err = %v, want the missing home named", err)
	}
}

func TestArmUsesTheOverrideWithoutAHome(t *testing.T) {
	// Arrange
	root := filepath.Join(t.TempDir(), "profiles")
	getenv := func(k string) string {
		if k == RootEnvironment {
			return root
		}
		return ""
	}
	home := func() (string, error) { return "", errors.New("no home") }

	// Act
	stop, err := Arm("daemon", &fakeLevel{}, func(Event) {}, getenv, home)

	// Assert
	if err != nil {
		t.Fatalf("Arm: %v", err)
	}
	stop()
}

func TestArmRefusesAnIncompleteProfiler(t *testing.T) {
	// Arrange
	getenv := func(string) string { return "" }

	// Act
	_, err := Arm("daemon", nil, func(Event) {}, getenv, os.UserHomeDir)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "build the runtime profiler") {
		t.Fatalf("err = %v, want the profiler refused", err)
	}
}

func TestArmsStopWaitsForTheWatcherToRemoveItsMarker(t *testing.T) {
	// Arrange
	root := t.TempDir()
	getenv := func(k string) string {
		if k == RootEnvironment {
			return root
		}
		return ""
	}
	var events []Event

	// Act
	stop, err := Arm("store", &fakeLevel{}, func(e Event) { events = append(events, e) }, getenv, os.UserHomeDir)
	if err != nil {
		t.Fatalf("Arm: %v", err)
	}
	stop()

	// Assert: the stop waits for the watcher, which removes its marker.
	left, _ := filepath.Glob(filepath.Join(root, ArmedDir, "store-*"))
	if len(left) != 0 || len(events) != 0 {
		t.Fatalf("markers %v, events %+v after the stop; want none of either with no session", left, events)
	}
}

func TestDescribe(t *testing.T) {
	until := time.Unix(1000300, 0)
	tests := []struct {
		name               string
		event              Event
		operation, level   string
		template, rendered string
		fields             []Field
	}{
		{name: "a start is info", event: Event{Kind: Started, Session: "s1", Files: []string{"a"}},
			operation: "profiling.started", level: "info",
			template: "runtime profiling started into the open session", rendered: "runtime profiling started into the open session",
			fields: []Field{{"session", "s1"}, {"files", []string{"a"}}}},
		{name: "a stop is info", event: Event{Kind: Stopped, Session: "s1", Reason: "exit", Files: []string{"a", "b"}},
			operation: "profiling.stopped", level: "info",
			template: "runtime profiling stopped and wrote its profiles", rendered: "runtime profiling stopped and wrote its profiles",
			fields: []Field{{"session", "s1"}, {"reason", "exit"}, {"files", []string{"a", "b"}}}},
		{name: "a level window opened names the runtime", event: Event{Kind: LevelOpened, Session: "s1", Level: "debug", Until: until},
			operation: "profiling.level_opened", level: "info",
			template: "the profiling session raised the {process}'s log level for its window", rendered: "the profiling session raised the store's log level for its window",
			fields: []Field{{"session", "s1"}, {"process", "store"}, {"level", "debug"}, {"until", int64(1000300)}}},
		{name: "a level window closed names the runtime", event: Event{Kind: LevelClosed, Session: "s1", Level: "debug", Reason: "session_closed", Until: until},
			operation: "profiling.level_closed", level: "info",
			template: "the profiling session's log level window closed; the {process} is back at info", rendered: "the profiling session's log level window closed; the store is back at info",
			fields: []Field{{"session", "s1"}, {"process", "store"}, {"level", "debug"}, {"reason", "session_closed"}, {"until", int64(1000300)}}},
		{name: "a whole-life start names its runtime and file", event: Event{Kind: ExitStarted, Files: []string{"/p/store-7.cpu.pprof"}},
			operation: "profiling.exit_started", level: "info",
			template: "the {process} profiles its whole life and writes the profile when it stops", rendered: "the store profiles its whole life and writes the profile when it stops",
			fields: []Field{{"process", "store"}, {"files", []string{"/p/store-7.cpu.pprof"}}}},
		{name: "a whole-life stop names its reason and file", event: Event{Kind: ExitStopped, Reason: "exit", Files: []string{"/p/store-7.cpu.pprof"}},
			operation: "profiling.exit_stopped", level: "info",
			template: "the {process} stopped and wrote its whole-life profile", rendered: "the store stopped and wrote its whole-life profile",
			fields: []Field{{"process", "store"}, {"reason", "exit"}, {"files", []string{"/p/store-7.cpu.pprof"}}}},
		{name: "a failure is error with its cause", event: Event{Kind: Failed, Session: "s1", Reason: "start", Err: errors.New("boom")},
			operation: "profiling.failed", level: "error",
			template: "runtime profiling could not do what the session asked", rendered: "runtime profiling could not do what the session asked",
			fields: []Field{{"session", "s1"}, {"reason", "start"}, {"cause", "boom"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Describe("store", tt.event)

			// Assert
			if got.Operation != tt.operation || got.Level != tt.level || got.Message.Template() != tt.template {
				t.Fatalf("Describe = %+v, want operation %q level %q template %q", got, tt.operation, tt.level, tt.template)
			}
			if fn := got.Message.Function(); fn != "agentrepl/logging/profiling.Describe" {
				t.Errorf("message keyed by %q, want Describe", fn)
			}
			if rendered, err := logsite.RenderFields(tt.template, got.Context()); err != nil || rendered != tt.rendered {
				t.Errorf("rendered = %q, %v; want %q", rendered, err, tt.rendered)
			}
			if !reflect.DeepEqual(got.Fields, tt.fields) {
				t.Errorf("fields = %v, want %v", got.Fields, tt.fields)
			}
		})
	}
}

func TestDescribeRefusesAnUnknownKind(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("Describe worded an event of no known kind")
		}
	}()

	// Act
	Describe("daemon", Event{Kind: "rebooted"})
}

func TestDescriptionContext(t *testing.T) {
	// Arrange
	d := Description{Fields: []Field{{"session", "s1"}, {"files", []string{"a"}}}}

	// Act
	got := d.Context()

	// Assert
	if len(got) != 2 || got["session"] != "s1" || len(got["files"].([]string)) != 1 {
		t.Fatalf("Context = %v", got)
	}
}
