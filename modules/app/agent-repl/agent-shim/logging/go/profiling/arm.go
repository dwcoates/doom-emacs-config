package profiling

import (
	"agentrepl/logging/logsite"
	"fmt"
)

// Arm starts RUNTIME's runtime profiling the way every Go runtime does: it
// resolves the profiles root (Root, from GETENV and HOME), builds the runtime's
// Profiler with LEVEL as its live log threshold and EVENTS as its record, and
// runs Watch on its own goroutine. Off, it costs that one parked goroutine.
// The returned stop ends any running profile and waits for the watcher to
// leave.
//
// A runtime started with ExitProfileEnvironment set profiles its whole life
// into that directory instead (StartExitProfile), and the returned stop
// writes the profile.
func Arm(runtime string, level LevelWindow, events func(Event), getenv func(string) string, home func() (string, error)) (func(), error) {
	if dir := getenv(ExitProfileEnvironment); dir != "" {
		stop, err := StartExitProfile(runtime, dir, events)
		if err != nil {
			return nil, fmt.Errorf("start the whole-life profile: %w", err)
		}
		return stop, nil
	}
	dir, err := home()
	if err != nil && getenv(RootEnvironment) == "" {
		return nil, fmt.Errorf("the profiles root needs a home directory: %w", err)
	}
	root, err := Root(getenv, dir)
	if err != nil {
		return nil, fmt.Errorf("resolve the profiles root: %w", err)
	}
	p, err := New(Options{Runtime: runtime, Root: root, Events: events, Level: level})
	if err != nil {
		return nil, fmt.Errorf("build the runtime profiler: %w", err)
	}
	done := make(chan struct{})
	left := make(chan struct{})
	go func() {
		defer close(left)
		Watch(p, done)
	}()
	return func() { close(done); <-left }, nil
}

// Field is one named value of an event's record.
type Field struct {
	Key   string
	Value any
}

// Description is how every Go runtime records an event: its operation, which
// the runtime prefixes with its own namespace when its logger asks for one,
// its level, its message and its fields. The message is a log site's
// template (logging-contract.md "Log sites"), made here once per kind, so
// every runtime forwards it to its logger and the record is keyed by
// Describe; each placeholder it names is one of the fields.
type Description struct {
	Operation string
	Level     string
	Message   logsite.Message
	Fields    []Field
}

// Describe words E for RUNTIME (the name Options.Runtime gives): lifecycle
// transitions at info, a failure at error.
func Describe(runtime string, e Event) Description {
	session := Field{"session", e.Session}
	process := Field{"process", runtime}
	switch e.Kind {
	case Started:
		return Description{"profiling.started", "info", logsite.Msg("runtime profiling started into the open session"),
			[]Field{session, {"files", e.Files}}}
	case Stopped:
		return Description{"profiling.stopped", "info", logsite.Msg("runtime profiling stopped and wrote its profiles"),
			[]Field{session, {"reason", e.Reason}, {"files", e.Files}}}
	case LevelOpened:
		return Description{"profiling.level_opened", "info", logsite.Msg("the profiling session raised the {process}'s log level for its window"),
			[]Field{session, process, {"level", e.Level}, {"until", e.Until.Unix()}}}
	case LevelClosed:
		return Description{"profiling.level_closed", "info", logsite.Msg("the profiling session's log level window closed; the {process} is back at info"),
			[]Field{session, process, {"level", e.Level}, {"reason", e.Reason}, {"until", e.Until.Unix()}}}
	case ExitStarted:
		return Description{"profiling.exit_started", "info", logsite.Msg("the {process} profiles its whole life and writes the profile {files} when it stops"),
			[]Field{process, {"files", e.Files}}}
	case ExitStopped:
		return Description{"profiling.exit_stopped", "info", logsite.Msg("the {process} stopped and wrote its whole-life profile {files}"),
			[]Field{process, {"reason", e.Reason}, {"files", e.Files}}}
	case Failed:
		return Description{"profiling.failed", "error", logsite.Msg("runtime profiling could not do what the session asked"),
			[]Field{session, {"reason", e.Reason}, {"cause", fmt.Sprint(e.Err)}}}
	}
	panic(fmt.Sprintf("profiling: an event of unknown kind %q", e.Kind))
}

// Context is the description's fields as a map, the record's named values.
func (d Description) Context() map[string]any {
	ctx := make(map[string]any, len(d.Fields))
	for _, f := range d.Fields {
		ctx[f.Key] = f.Value
	}
	return ctx
}
