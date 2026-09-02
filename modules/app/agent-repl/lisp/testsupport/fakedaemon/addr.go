package main

import (
	"fmt"
	"os"
	"path/filepath"
)

// stateDirEnv is the ONE state root every system in this project shares
// (COMMON.md, "STATE ROOT").  The fake refuses to start without it rather
// than falling back to ~/.claude-emacs: a fallback here would let a test run
// write into the developer's real state tree.
const stateDirEnv = "AGENT_REPL_STATE_DIR"

// addrFileName is the daemon-discovery file every client reads
// (COMMON.md, "DAEMON ADDRESS").
const addrFileName = "daemon.addr"

func stateDir() (string, error) {
	dir := os.Getenv(stateDirEnv)
	if dir == "" {
		return "", fmt.Errorf("%s is unset; refusing to start (no fallback state root exists)", stateDirEnv)
	}
	return dir, nil
}

func addrFilePath(dir string) string { return filepath.Join(dir, addrFileName) }

// writeAddrFile writes "127.0.0.1:<port>\n" to <state dir>/daemon.addr by
// atomic rename, exactly as the real daemon does.
func writeAddrFile(dir, address string) error {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("create state dir %s: %w", dir, err)
	}
	tmp, err := os.CreateTemp(dir, ".daemon.addr-")
	if err != nil {
		return fmt.Errorf("create temp addr file: %w", err)
	}
	tmpName := tmp.Name()
	if _, err := tmp.WriteString(address + "\n"); err != nil {
		tmp.Close()
		os.Remove(tmpName)
		return fmt.Errorf("write temp addr file: %w", err)
	}
	if err := tmp.Close(); err != nil {
		os.Remove(tmpName)
		return fmt.Errorf("close temp addr file: %w", err)
	}
	if err := os.Rename(tmpName, addrFilePath(dir)); err != nil {
		os.Remove(tmpName)
		return fmt.Errorf("rename temp addr file: %w", err)
	}
	return nil
}

// removeAddrFile deletes daemon.addr on orderly exit.  An already-absent file
// is not an error: another instance may legitimately have replaced it.
func removeAddrFile(dir string) error {
	err := os.Remove(addrFilePath(dir))
	if err != nil && !os.IsNotExist(err) {
		return err
	}
	return nil
}
