package logging

import (
	"fmt"
	"strconv"
	"strings"
	"sync"
	"time"
)

// UntilEnvironment names the Unix second a non-info AGENT_REPL_LOG_LEVEL
// lasts until. proto/vocab/log-level-window.json holds the rule every runtime
// answers identically.
const UntilEnvironment = "AGENT_REPL_LOG_LEVEL_UNTIL"

// WindowMax is the longest a level other than info may last: a level window
// ends at most this long after the runtime reads it.
const WindowMax = 5 * time.Minute

// Outcome names what a runtime made of its startup level setting.
type Outcome string

const (
	// OutcomeDefault means nothing other than info was asked for.
	OutcomeDefault Outcome = "default"
	// OutcomeHonored means the requested level holds until its window ends.
	OutcomeHonored Outcome = "honored"
	// OutcomeNoExpiry means a level other than info came without a window.
	OutcomeNoExpiry Outcome = "no_expiry"
	// OutcomeExpired means the requested window had already ended.
	OutcomeExpired Outcome = "expired"
	// OutcomeBeyondWindow means the window ended later than WindowMax allows.
	OutcomeBeyondWindow Outcome = "beyond_window"
	// OutcomeWindowEnded means a running window ended and the level reverted.
	OutcomeWindowEnded Outcome = "window_ended"
)

// Selection is a runtime's startup level decision.
type Selection struct {
	// Requested is the AGENT_REPL_LOG_LEVEL value as read, empty when unset.
	Requested string
	// RequestedUntil is the AGENT_REPL_LOG_LEVEL_UNTIL value as read.
	RequestedUntil string
	// Level is the level the runtime starts at.
	Level Level
	// Until is when Level ends; zero when Level is info and never ends.
	Until time.Time
	// Outcome names the decision.
	Outcome Outcome
}

// SelectLevel decides the startup level from the two environment values at
// instant now. A level other than info holds only inside an unexpired window
// no longer than WindowMax; anything else starts at info. An unknown level or
// a malformed UNTIL is a bootstrap error.
func SelectLevel(level, until string, now time.Time) (Selection, error) {
	sel := Selection{Requested: level, RequestedUntil: until, Level: LevelInfo, Outcome: OutcomeDefault}
	parsed, err := ParseLevel(level)
	if err != nil {
		return Selection{}, err
	}
	var end time.Time
	if trimmed := strings.TrimSpace(until); trimmed != "" {
		seconds, err := strconv.ParseInt(trimmed, 10, 64)
		if err != nil {
			return Selection{}, fmt.Errorf("%s must be a Unix second, got %q", UntilEnvironment, until)
		}
		end = time.Unix(seconds, 0)
	}
	switch {
	case parsed == LevelInfo:
	case end.IsZero():
		sel.Outcome = OutcomeNoExpiry
	case !now.Before(end):
		sel.Outcome = OutcomeExpired
	case end.Sub(now) > WindowMax:
		sel.Outcome = OutcomeBeyondWindow
	default:
		sel.Level, sel.Until, sel.Outcome = parsed, end, OutcomeHonored
	}
	return sel, nil
}

// Note is the info record a runtime writes about its startup selection, or
// ok=false when nothing other than info was asked for.
func (s Selection) Note() (message string, ok bool) {
	switch s.Outcome {
	case OutcomeHonored:
		return fmt.Sprintf("log level %s until %s", s.Level, s.Until.Format(time.RFC3339)), true
	case OutcomeNoExpiry:
		return fmt.Sprintf("log level %q ignored without %s; starting at info", s.Requested, UntilEnvironment), true
	case OutcomeExpired:
		return fmt.Sprintf("log level %q ignored: its window ended at %s; starting at info", s.Requested, s.RequestedUntil), true
	case OutcomeBeyondWindow:
		return fmt.Sprintf("log level %q ignored: its window ends at %s, more than %s away; starting at info", s.Requested, s.RequestedUntil, WindowMax), true
	default:
		return "", false
	}
}

// Context is the structured evidence of the startup note.
func (s Selection) Context() map[string]any {
	ctx := map[string]any{
		"requested_level": s.Requested,
		"requested_until": s.RequestedUntil,
		"level":           s.Level.String(),
		"outcome":         string(s.Outcome),
	}
	if !s.Until.IsZero() {
		ctx["until"] = s.Until.Unix()
	}
	return ctx
}

// String renders the level in the shared vocabulary.
func (l Level) String() string {
	switch l {
	case LevelDebug:
		return "debug"
	case LevelInfo:
		return "info"
	case LevelWarn:
		return "warn"
	case LevelError:
		return "error"
	default:
		return fmt.Sprintf("level(%d)", uint8(l))
	}
}

// Expiry reports a level window that ended: the level it held and when it
// ended. The caller records it at info.
type Expiry struct {
	// From is the level the window held.
	From Level
	// Until is when the window ended.
	Until time.Time
}

// Message is the info record text for the revert.
func (e Expiry) Message() string {
	return fmt.Sprintf("log level %s window ended at %s; reverted to info", e.From, e.Until.Format(time.RFC3339))
}

// Context is the structured evidence of the revert.
func (e Expiry) Context() map[string]any {
	return map[string]any{
		"from_level": e.From.String(),
		"until":      e.Until.Unix(),
		"level":      LevelInfo.String(),
		"outcome":    string(OutcomeWindowEnded),
	}
}

// Window is a runtime's live threshold: a level that, when it is not info,
// reverts to info by itself once its window ends. Safe for concurrent use;
// share one Window across every logger copy of a runtime.
type Window struct {
	mu    sync.Mutex
	level Level
	until time.Time
	now   func() time.Time
}

// NewWindow is the live threshold a Selection starts. now is the clock the
// window's end is checked against.
func NewWindow(sel Selection, now func() time.Time) *Window {
	if now == nil {
		panic("logging: nil window clock")
	}
	return &Window{level: sel.Level, until: sel.Until, now: now}
}

// FixedWindow is a threshold that never ends, for tests and harnesses.
func FixedWindow(level Level) *Window {
	return &Window{level: level, now: time.Now}
}

// Allows reports whether a record at candidate passes the threshold now. When
// this call is the first to find the window ended, the window reverts to info
// and the Expiry is returned for the caller to record at info; every later
// call answers from info alone.
func (w *Window) Allows(candidate string) (bool, *Expiry) {
	w.mu.Lock()
	var expired *Expiry
	if !w.until.IsZero() && !w.now().Before(w.until) {
		expired = &Expiry{From: w.level, Until: w.until}
		w.level, w.until = LevelInfo, time.Time{}
	}
	level := w.level
	w.mu.Unlock()
	return level.Allows(candidate), expired
}

// Level is the threshold in force, without checking the window's end.
func (w *Window) Level() Level {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.level
}
