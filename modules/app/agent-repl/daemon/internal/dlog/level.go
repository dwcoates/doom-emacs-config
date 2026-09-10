package dlog

import "fmt"

// LevelEnvironment is the one process switch governing which daemon records
// reach both durable storage and the terminal mirror.
const LevelEnvironment = "AGENT_REPL_LOG_LEVEL"

// levelThreshold is the minimum severity admitted by a surfaces instance.
type levelThreshold int

const (
	thresholdDebug levelThreshold = iota
	thresholdInfo
	thresholdWarn
	thresholdError
)

// parseLevel resolves the process setting. Empty is the contract's info
// default; every other unrecognized value is a boot refusal.
func parseLevel(raw string) (levelThreshold, error) {
	switch raw {
	case "":
		return thresholdInfo, nil
	case LevelDebug:
		return thresholdDebug, nil
	case LevelInfo:
		return thresholdInfo, nil
	case LevelWarn:
		return thresholdWarn, nil
	case LevelError:
		return thresholdError, nil
	default:
		return 0, fmt.Errorf("%s=%q is not one of debug, info, warn, error", LevelEnvironment, raw)
	}
}

// enabled reports whether a record at severity is admitted by the threshold.
// Callers validate foreign levels before asking; daemon levels are closed
// constants, so an unknown value is a programming error and panics loudly.
func (t levelThreshold) enabled(severity string) bool {
	var record levelThreshold
	switch severity {
	case LevelDebug:
		record = thresholdDebug
	case LevelInfo:
		record = thresholdInfo
	case LevelWarn:
		record = thresholdWarn
	case LevelError:
		record = thresholdError
	default:
		panic("dlog: unknown record level " + severity)
	}
	return record >= t
}
