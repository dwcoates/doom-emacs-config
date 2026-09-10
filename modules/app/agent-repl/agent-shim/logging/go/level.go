package logging

import (
	"fmt"
	"strings"
)

// Level is the ordered severity shared by every Go logging runtime. A logger
// persists and mirrors a record when the record is at least this severe.
type Level uint8

const (
	LevelDebug Level = iota
	LevelInfo
	LevelWarn
	LevelError
)

// ParseLevel parses AGENT_REPL_LOG_LEVEL. An empty value means info, the
// process-wide default required by the logging contract. Anything else is a
// bootstrap error rather than a value to reinterpret.
func ParseLevel(value string) (Level, error) {
	switch strings.TrimSpace(value) {
	case "":
		return LevelInfo, nil
	case "debug":
		return LevelDebug, nil
	case "info":
		return LevelInfo, nil
	case "warn":
		return LevelWarn, nil
	case "error":
		return LevelError, nil
	default:
		return 0, fmt.Errorf("AGENT_REPL_LOG_LEVEL must be debug, info, warn, or error, got %q", value)
	}
}

// Allows reports whether a record at candidate passes this threshold.
// Candidate is validated even when it would be filtered so a misspelled
// severity can never disappear silently.
func (l Level) Allows(candidate string) bool {
	var level Level
	switch candidate {
	case "debug":
		level = LevelDebug
	case "info":
		level = LevelInfo
	case "warn":
		level = LevelWarn
	case "error":
		level = LevelError
	default:
		panic(fmt.Sprintf("logging: invalid level %q", candidate))
	}
	return level >= l
}
