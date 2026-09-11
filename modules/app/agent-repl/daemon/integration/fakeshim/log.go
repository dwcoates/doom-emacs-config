package main

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"time"
)

// logSink writes the shim's JSONL records to the descriptor the daemon handed
// it (fd 3 by contract). A write failure is reported to stderr rather than
// swallowed, because a poisoned log sink is itself a fault the suite asserts.
type logSink struct {
	mu sync.Mutex
	f  *os.File
	// workspaceID is THE DAEMON'S MINTED 16-hex workspace id, read off the
	// --listen socket basename exactly as the real shim reads it
	// (agent-shim/claude/shim/src/main.ts workspaceIdFromListenSocket). Every
	// record carries it so a workspace's shim records group with its daemon
	// records under one workspace_id.
	workspaceID  string
	workspaceDir string
}

func newLogSink(fd int) *logSink {
	if fd < 0 {
		return &logSink{}
	}
	return &logSink{f: os.NewFile(uintptr(fd), "shim.log")}
}

// bind stamps the workspace identity every subsequent record carries.
func (l *logSink) bind(workspaceID, workspaceDir string) {
	if l == nil {
		return
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	l.workspaceID = workspaceID
	l.workspaceDir = workspaceDir
}

func (l *logSink) write(operation string, context map[string]any) {
	if l == nil || l.f == nil {
		return
	}
	l.mu.Lock()
	workspaceID, workspaceDir := l.workspaceID, l.workspaceDir
	l.mu.Unlock()
	rec := map[string]any{
		"timestamp": time.Now().UTC().Format(time.RFC3339Nano),
		"runtime":   "shim",
		"level":     "debug",
		"pid":       os.Getpid(),
		"operation": "shim.fake." + operation,
		"context":   context,
	}
	if workspaceID != "" {
		rec["workspace_id"] = workspaceID
	}
	if workspaceDir != "" {
		rec["workspace_dir"] = workspaceDir
	}
	line, err := json.Marshal(rec)
	if err != nil {
		os.Stderr.WriteString("fakeshim: cannot encode log record: " + err.Error() + "\n")
		return
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	if _, err := l.f.Write(append(line, '\n')); err != nil {
		os.Stderr.WriteString("fakeshim: log sink write failed: " + err.Error() + "\n")
	}
}

func (l *logSink) close() {
	if l == nil || l.f == nil {
		return
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	l.f.Close()
}

// listenSocketID matches the daemon's socket naming: the minted 16-hex
// workspace id, with the rollout generation suffix a replacement shim serves.
var listenSocketID = regexp.MustCompile(`^([0-9a-f]{16})(\.n\d+)?$`)

// workspaceIDFromListenSocket reads the daemon's minted workspace id out of
// the socket the daemon told this shim to serve. It is the SAME derivation the
// real shim performs, and the same refusal: a socket this build cannot read a
// workspace id out of means the two disagree about the layout, and a record
// attributed to a guessed id is worse than a shim that will not start.
func workspaceIDFromListenSocket(listen string) (string, error) {
	stem := strings.TrimSuffix(filepath.Base(listen), ".sock")
	match := listenSocketID.FindStringSubmatch(stem)
	if match == nil {
		return "", fmt.Errorf(
			"fakeshim: the --listen socket %q is not named after a workspace id; expected <16 hex characters>[.n<generation>].sock, which is how the daemon names it",
			listen)
	}
	return match[1], nil
}
