package profiling

import (
	"errors"
	"os"
	"path/filepath"
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
		name  string
		event Event
		want  Description
	}{
		{name: "a start is info", event: Event{Kind: Started, Session: "s1", Files: []string{"a"}},
			want: Description{"profiling.started", "info", "runtime profiling started into the open session", []Field{{"session", "s1"}, {"files", []string{"a"}}}}},
		{name: "a stop is info", event: Event{Kind: Stopped, Session: "s1", Reason: "exit", Files: []string{"a", "b"}},
			want: Description{"profiling.stopped", "info", "runtime profiling stopped and wrote its profiles", []Field{{"session", "s1"}, {"reason", "exit"}, {"files", []string{"a", "b"}}}}},
		{name: "a level window opened names the runtime", event: Event{Kind: LevelOpened, Session: "s1", Level: "debug", Until: until},
			want: Description{"profiling.level_opened", "info", "the profiling session raised the store's log level for its window", []Field{{"session", "s1"}, {"level", "debug"}, {"until", int64(1000300)}}}},
		{name: "a level window closed names the runtime", event: Event{Kind: LevelClosed, Session: "s1", Level: "debug", Reason: "session_closed", Until: until},
			want: Description{"profiling.level_closed", "info", "the profiling session's log level window closed; the store is back at info", []Field{{"session", "s1"}, {"level", "debug"}, {"reason", "session_closed"}, {"until", int64(1000300)}}}},
		{name: "a failure is error with its cause", event: Event{Kind: Failed, Session: "s1", Reason: "start", Err: errors.New("boom")},
			want: Description{"profiling.failed", "error", "runtime profiling could not do what the session asked", []Field{{"session", "s1"}, {"reason", "start"}, {"cause", "boom"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Describe("store", tt.event)

			// Assert
			if got.Operation != tt.want.Operation || got.Level != tt.want.Level || got.Message != tt.want.Message ||
				got.Text() != tt.want.Text() {
				t.Fatalf("Describe = %+v, want %+v", got, tt.want)
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

func TestDescriptionText(t *testing.T) {
	// Arrange
	d := Description{Message: "m", Fields: []Field{{"session", "s1"}, {"files", []string{"a", "b"}}, {"until", int64(7)}}}

	// Act
	got := d.Text()

	// Assert
	if got != "m session=s1 files=a,b until=7" {
		t.Fatalf("Text = %q", got)
	}
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
