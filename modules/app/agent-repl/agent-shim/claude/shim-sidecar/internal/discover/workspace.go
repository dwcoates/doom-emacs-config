package discover

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"

	sharedlogging "agentrepl/logging"
)

// ResolveWorkspace reads the authoritative cwd from a target session's main
// transcript or transcript-shaped target. The lossy project slug is used only
// to locate that source; it is never decoded into a path or promoted as truth.
func ResolveWorkspace(target Target) (string, string, error) {
	if target.ConfigRoot == "" || target.SessionID == "" {
		return "", "", fmt.Errorf("workspace resolution requires a config-root transcript target")
	}
	projectsRoot := filepath.Join(target.ConfigRoot, "projects")
	rel, err := filepath.Rel(projectsRoot, target.Path)
	if err != nil || rel == "." || rel == ".." || filepath.IsAbs(rel) {
		return "", "", fmt.Errorf("resolve project directory for %q beneath %q", target.Path, projectsRoot)
	}
	segments := splitPath(rel)
	if len(segments) < 2 || segments[0] == ".." {
		return "", "", fmt.Errorf("target %q is not inside one project directory", target.Path)
	}
	projectDir := filepath.Join(projectsRoot, segments[0])
	transcript := target.Path
	if filepath.Base(target.Path) == "journal.jsonl" {
		transcript = filepath.Join(projectDir, target.SessionID+".jsonl")
	}
	workspaceDir, err := transcriptCWD(transcript)
	if err != nil {
		return "", "", err
	}
	workspaceID, err := sharedlogging.WorkspaceID(workspaceDir)
	if err != nil {
		return "", "", err
	}
	return workspaceDir, workspaceID, nil
}

func splitPath(path string) []string {
	var out []string
	for path != "." && path != string(filepath.Separator) {
		dir, base := filepath.Split(path)
		if base != "" {
			out = append([]string{base}, out...)
		}
		path = filepath.Clean(dir)
	}
	return out
}

func transcriptCWD(path string) (cwd string, err error) {
	file, err := os.Open(path)
	if err != nil {
		return "", fmt.Errorf("open transcript %q for workspace attribution: %w", path, err)
	}
	defer func() {
		if closeErr := file.Close(); closeErr != nil {
			err = errors.Join(err, fmt.Errorf("close transcript %q after reading its cwd: %w", path, closeErr))
		}
	}()
	reader := bufio.NewReader(file)
	for {
		line, readErr := reader.ReadBytes('\n')
		line = bytes.TrimSpace(line)
		if len(line) != 0 {
			if cwd, found, err := cwdToken(line); err != nil {
				return "", fmt.Errorf("transcript %q has an invalid absolute cwd: %w", path, err)
			} else if found {
				return filepath.Clean(cwd), nil
			}
		}
		if readErr != nil {
			if errors.Is(readErr, io.EOF) {
				return "", fmt.Errorf("transcript %q does not yet contain a cwd", path)
			}
			return "", fmt.Errorf("read transcript %q for workspace attribution: %w", path, readErr)
		}
	}
}

// cwdToken reads only as far as the top-level cwd value. The enclosing record
// may still be growing: once the complete JSON string token is present, the
// sidecar can attribute the file and let the ordinary tailer durably carry the
// incomplete record bytes without converting them early.
func cwdToken(line []byte) (string, bool, error) {
	decoder := json.NewDecoder(bytes.NewReader(line))
	start, err := decoder.Token()
	if err != nil || start != json.Delim('{') {
		return "", false, nil
	}
	for decoder.More() {
		key, err := decoder.Token()
		if err != nil {
			return "", false, nil
		}
		if key == "cwd" {
			var cwd string
			if err := decoder.Decode(&cwd); err != nil {
				return "", false, err
			}
			if cwd == "" || !filepath.IsAbs(cwd) {
				return "", false, fmt.Errorf("cwd must be a non-empty absolute path")
			}
			return cwd, true, nil
		}
		var ignored json.RawMessage
		if err := decoder.Decode(&ignored); err != nil {
			return "", false, nil
		}
	}
	return "", false, nil
}
