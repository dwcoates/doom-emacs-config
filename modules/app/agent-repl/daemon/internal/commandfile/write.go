package commandfile

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
)

// Write drops one command file into the ingress directory dir, as every
// producer does: the whole array is written under a dot-prefixed temp name the
// glob does not match, then renamed onto a unique `workspace_commands_<hex>.json`
// in one step, so the ingress never sees a half-written file. It answers the
// file's base name, which is the name the ingress claims and quarantines it
// under.
//
// THE ARRAY IS HELD TO THE INGRESS'S OWN PARSE before anything touches the
// disk: a file this function writes is one the ingress accepts, and a request
// it would quarantine is refused here instead, with nothing written.
func Write(dir string, entries []Entry) (string, error) {
	data, err := json.Marshal(entries)
	if err != nil {
		return "", fmt.Errorf("commandfile: encode the command array: %w", err)
	}
	if _, err := parse(data); err != nil {
		return "", fmt.Errorf("commandfile: refusing to write a command file the ingress would quarantine: %w", err)
	}
	var suffix [16]byte
	if _, err := rand.Read(suffix[:]); err != nil {
		return "", fmt.Errorf("commandfile: mint a file name: %w", err)
	}
	name := "workspace_commands_" + hex.EncodeToString(suffix[:]) + ".json"
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("commandfile: create %q: %w", dir, err)
	}
	tmp, err := os.CreateTemp(dir, ".workspace_commands_*.json")
	if err != nil {
		return "", fmt.Errorf("commandfile: create a temp file in %q: %w", dir, err)
	}
	tmpPath := tmp.Name()
	_, writeErr := tmp.Write(data)
	if err := errors.Join(writeErr, tmp.Close()); err != nil {
		return "", discard(tmpPath, fmt.Errorf("commandfile: write %q: %w", tmpPath, err))
	}
	if err := os.Rename(tmpPath, filepath.Join(dir, name)); err != nil {
		return "", discard(tmpPath, fmt.Errorf("commandfile: publish %q: %w", name, err))
	}
	return name, nil
}

// discard removes the temp file a failed write left behind and answers the
// failure, with a failed removal joined to it rather than dropped.
func discard(path string, cause error) error {
	if err := os.Remove(path); err != nil {
		return errors.Join(cause, fmt.Errorf("commandfile: remove %q: %w", path, err))
	}
	return cause
}
