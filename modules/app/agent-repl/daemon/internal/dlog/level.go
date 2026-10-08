package dlog

import "agentrepl/logging"

// LevelEnvironment is the one process switch governing which daemon records
// reach both durable storage and the terminal mirror. A level other than info
// holds only inside the window logging.UntilEnvironment names
// (proto/vocab/log-level-window.json).
const LevelEnvironment = "AGENT_REPL_LOG_LEVEL"

// levelWindowOperation is the operation of every level window record: the
// startup decision and a window that ended.
const levelWindowOperation = "daemon.dlog.level_window"

// parseLevel resolves a fixed threshold. Empty is the contract's info
// default; every other unrecognized value is a boot refusal.
func parseLevel(raw string) (logging.Level, error) {
	return logging.ParseLevel(raw)
}

// admits reports whether a record at severity passes the live threshold. The
// first call to find a level window ended records the revert at info; the
// window is info by then, so that record cannot end it again. Daemon levels
// are closed constants and foreign levels are validated before asking, so an
// unknown severity is a programming error and panics loudly.
func (s *surfaces) admits(severity string) bool {
	allowed, ended := s.window.Allows(severity)
	if ended != nil {
		s.Global().Info(levelWindowOperation, ended.Message(), Context(ended.Context()))
	}
	return allowed
}
