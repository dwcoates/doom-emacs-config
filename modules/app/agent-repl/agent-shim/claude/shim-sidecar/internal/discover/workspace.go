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

// ErrNoCWDYet is the attribution read's answer for a transcript that is on
// disk and records no cwd YET. The vendor writes an unchained
// `queue-operation` line, which carries no cwd, BEFORE the first cwd-bearing
// record of a session, so a transcript discovered between those two writes is
// an ordinary start, not a fault: the caller holds it and re-reads it.
var ErrNoCWDYet = errors.New("it does not yet contain a cwd")

// Attribution is the workspace a session's transcript is filed against.
type Attribution struct {
	// Dir is the workspace directory: a cwd the transcript itself records.
	Dir string
	// ID is Dir's canonical workspace correlation key.
	ID string
	// FirstCWDFallback reports that no cwd in the transcript encodes to the
	// project folder the file lives in, so Dir is the transcript's FIRST cwd.
	FirstCWDFallback bool
}

// ResolveWorkspace reads the authoritative cwd from a target session's main
// transcript or transcript-shaped target.
//
// A SESSION CAN CHANGE DIRECTORY, so its transcript can record several cwds,
// and the vendor files the transcript under the project folder of the cwd it
// is now kept for. The attribution is the cwd whose vendor encoding
// (VendorProjectSlug) names the folder the file lives in: a cwd is ENCODED and
// compared, and the lossy slug is never decoded into a path. When no cwd
// encodes to that folder the transcript's first cwd is used, and the
// attribution says so, so the caller can state it; ingestion is never held
// back for it.
func ResolveWorkspace(target Target) (Attribution, error) {
	if target.ConfigRoot == "" || target.SessionID == "" {
		return Attribution{}, fmt.Errorf("workspace resolution requires a config-root transcript target")
	}
	projectsRoot := filepath.Join(target.ConfigRoot, "projects")
	rel, err := filepath.Rel(projectsRoot, target.Path)
	if err != nil || rel == "." || rel == ".." || filepath.IsAbs(rel) {
		return Attribution{}, fmt.Errorf("resolve project directory for %q beneath %q", target.Path, projectsRoot)
	}
	segments := splitPath(rel)
	if len(segments) < 2 || segments[0] == ".." {
		return Attribution{}, fmt.Errorf("target %q is not inside one project directory", target.Path)
	}
	projectDir := filepath.Join(projectsRoot, segments[0])
	transcript := target.Path
	if filepath.Base(target.Path) == "journal.jsonl" {
		transcript = filepath.Join(projectDir, target.SessionID+".jsonl")
	}
	workspaceDir, matched, err := transcriptCWD(transcript, segments[0])
	if err != nil {
		return Attribution{}, err
	}
	workspaceID, err := sharedlogging.WorkspaceID(workspaceDir)
	if err != nil {
		return Attribution{}, err
	}
	return Attribution{Dir: workspaceDir, ID: workspaceID, FirstCWDFallback: !matched}, nil
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

// transcriptCWD answers the first cwd in the transcript whose vendor encoding
// is projectSlug, with matched true; failing that, the transcript's first cwd
// with matched false. A transcript holding no cwd yet answers ErrNoCWDYet.
func transcriptCWD(path, projectSlug string) (cwd string, matched bool, err error) {
	file, err := os.Open(path)
	if err != nil {
		return "", false, fmt.Errorf("open transcript %q for workspace attribution: %w", path, err)
	}
	defer func() {
		if closeErr := file.Close(); closeErr != nil {
			err = errors.Join(err, fmt.Errorf("close transcript %q after reading its cwd: %w", path, closeErr))
		}
	}()
	first := ""
	reader := bufio.NewReader(file)
	for {
		line, readErr := reader.ReadBytes('\n')
		line = bytes.TrimSpace(line)
		if len(line) != 0 {
			if found, ok, err := cwdToken(line); err != nil {
				return "", false, fmt.Errorf("transcript %q has an invalid absolute cwd: %w", path, err)
			} else if ok {
				found = filepath.Clean(found)
				if sharedlogging.VendorProjectSlug(found) == projectSlug {
					return found, true, nil
				}
				if first == "" {
					first = found
				}
			}
		}
		if readErr != nil {
			if !errors.Is(readErr, io.EOF) {
				return "", false, fmt.Errorf("read transcript %q for workspace attribution: %w", path, readErr)
			}
			if first == "" {
				return "", false, fmt.Errorf("transcript %q: %w", path, ErrNoCWDYet)
			}
			return first, false, nil
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
