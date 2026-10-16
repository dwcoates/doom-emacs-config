package profiling

import (
	"fmt"
	"strings"
)

// Arm starts RUNTIME's runtime profiling the way every Go runtime does: it
// resolves the profiles root (Root, from GETENV and HOME), builds the runtime's
// Profiler with LEVEL as its live log threshold and EVENTS as its record, and
// runs Watch on its own goroutine. Off, it costs that one parked goroutine.
// The returned stop ends any running profile and waits for the watcher to
// leave.
func Arm(runtime string, level LevelWindow, events func(Event), getenv func(string) string, home func() (string, error)) (func(), error) {
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
// its level, its message and its fields.
type Description struct {
	Operation string
	Level     string
	Message   string
	Fields    []Field
}

// Describe words E for RUNTIME (the name Options.Runtime gives): lifecycle
// transitions at info, a failure at error.
func Describe(runtime string, e Event) Description {
	session := Field{"session", e.Session}
	switch e.Kind {
	case Started:
		return Description{"profiling.started", "info", "runtime profiling started into the open session",
			[]Field{session, {"files", e.Files}}}
	case Stopped:
		return Description{"profiling.stopped", "info", "runtime profiling stopped and wrote its profiles",
			[]Field{session, {"reason", e.Reason}, {"files", e.Files}}}
	case LevelOpened:
		return Description{"profiling.level_opened", "info", "the profiling session raised the " + runtime + "'s log level for its window",
			[]Field{session, {"level", e.Level}, {"until", e.Until.Unix()}}}
	case LevelClosed:
		return Description{"profiling.level_closed", "info", "the profiling session's log level window closed; the " + runtime + " is back at info",
			[]Field{session, {"level", e.Level}, {"reason", e.Reason}, {"until", e.Until.Unix()}}}
	case Failed:
		return Description{"profiling.failed", "error", "runtime profiling could not do what the session asked",
			[]Field{session, {"reason", e.Reason}, {"cause", fmt.Sprint(e.Err)}}}
	}
	panic(fmt.Sprintf("profiling: an event of unknown kind %q", e.Kind))
}

// Text is the description as one line, for a logger whose context is its
// message: the message, then each field as key=value (files joined by
// commas).
func (d Description) Text() string {
	var b strings.Builder
	b.WriteString(d.Message)
	for _, f := range d.Fields {
		value := f.Value
		if files, ok := value.([]string); ok {
			value = strings.Join(files, ",")
		}
		fmt.Fprintf(&b, " %s=%v", f.Key, value)
	}
	return b.String()
}

// Context is the description's fields as a map, for a logger with
// structured context.
func (d Description) Context() map[string]any {
	ctx := make(map[string]any, len(d.Fields))
	for _, f := range d.Fields {
		ctx[f.Key] = f.Value
	}
	return ctx
}
