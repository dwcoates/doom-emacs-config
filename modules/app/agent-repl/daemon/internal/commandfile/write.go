package commandfile

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"

	"claude-repld/internal/atomicfile"
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
	if err := atomicfile.Replace(filepath.Join(dir, name), data, atomicfile.Options{Pattern: ".workspace_commands_*.json"}); err != nil {
		return "", fmt.Errorf("commandfile: publish %q: %w", name, err)
	}
	return name, nil
}
