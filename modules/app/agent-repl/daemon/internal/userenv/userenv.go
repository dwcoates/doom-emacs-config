// Package userenv refreshes a spawned daemon's per-user settings from the
// file the user's shell environment is kept in.
//
// A SETTING SET AFTER THE DAEMON STARTED MUST REACH ITS SUCCESSOR. Emacs
// starts the first daemon with its own environment, which Doom loads from the
// env file `doom env` writes (`doom-env-file`), and every later daemon a
// rollout spawns used to copy its predecessor's environment whole. So a
// setting added to the shell profile after Emacs started (the persistent-wifi
// hotspot, 2026-11-12) never reached any daemon until a bounce. Now every
// successor takes each per-user setting fresh from the env file Emacs named
// (EnvFile), so `doom env` then a deploy delivers it.
//
// THE SETTINGS ARE THE ONES THE INSTALLER DECLARES: every requirement of kind
// `env` in agent-repl's requirements.json (SettingNames), the one place the
// per-user variables are listed. A setting Emacs states on the spawn itself
// (EmacsStated) is Emacs's, and the file never overrides it.
package userenv

import (
	"encoding/json"
	"fmt"
	"os"
	"sort"
	"strings"
)

// EnvFile names the env file Emacs states on the daemon's spawn: Doom's
// `doom-env-file`. Unset, the daemon was not started by Emacs (a test, the
// sandbox) and its successors inherit its environment unchanged.
const EnvFile = "AGENT_REPL_USER_ENV_FILE"

// EmacsStated names the variables Emacs states on the spawn itself,
// comma-separated, which the env file never overrides.
const EmacsStated = "AGENT_REPL_EMACS_STATED_ENV"

// SettingNames is every `env` requirement's variable in the requirements
// manifest at path, sorted.
func SettingNames(path string) ([]string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("userenv: read the requirements manifest: %w", err)
	}
	var manifest struct {
		Requirements []struct {
			Kind string `json:"kind"`
			Name string `json:"name"`
		} `json:"requirements"`
	}
	if err := json.Unmarshal(data, &manifest); err != nil {
		return nil, fmt.Errorf("userenv: %s is not a requirements manifest: %w", path, err)
	}
	var names []string
	for _, r := range manifest.Requirements {
		if r.Kind != "env" {
			continue
		}
		if r.Name == "" {
			return nil, fmt.Errorf("userenv: %s declares an env requirement with no name", path)
		}
		names = append(names, r.Name)
	}
	sort.Strings(names)
	return names, nil
}

// ParseEnvFile reads a Doom env file: comment lines (`;;`) and one
// parenthesized list of double-quoted NAME=VALUE strings, `\"` and `\\`
// escaped. A later entry of one name wins, as Doom's own loader lets it.
func ParseEnvFile(data []byte) (map[string]string, error) {
	text := string(data)
	open := strings.Index(text, "(")
	if open < 0 {
		return nil, fmt.Errorf("userenv: the env file holds no list of variables")
	}
	out := map[string]string{}
	for i := open + 1; i < len(text); i++ {
		switch c := text[i]; {
		case c == ')':
			return out, nil
		case c == '"':
			var b strings.Builder
			j := i + 1
			for ; j < len(text) && text[j] != '"'; j++ {
				if text[j] == '\\' && j+1 < len(text) {
					j++
				}
				b.WriteByte(text[j])
			}
			if j >= len(text) {
				return nil, fmt.Errorf("userenv: the env file ends inside a string")
			}
			entry := b.String()
			name, value, ok := strings.Cut(entry, "=")
			if !ok || name == "" {
				return nil, fmt.Errorf("userenv: the env file's entry %q is not NAME=VALUE", entry)
			}
			out[name] = value
			i = j
		case c == ' ' || c == '\n' || c == '\t' || c == '\r':
		default:
			return nil, fmt.Errorf("userenv: the env file has %q where a string or the list's end belongs", string(c))
		}
	}
	return nil, fmt.Errorf("userenv: the env file's list never ends")
}

// Refresh is environ with each of names taken from file: set to the file's
// value, or removed when the file no longer has it, so the file is the one
// statement of every per-user setting. A name environ's EmacsStated lists is
// left as it is.
func Refresh(environ, names []string, file map[string]string) []string {
	stated := map[string]bool{}
	for _, kv := range environ {
		if v, ok := strings.CutPrefix(kv, EmacsStated+"="); ok {
			for _, n := range strings.Split(v, ",") {
				if n = strings.TrimSpace(n); n != "" {
					stated[n] = true
				}
			}
		}
	}
	refresh := map[string]bool{}
	for _, n := range names {
		if !stated[n] {
			refresh[n] = true
		}
	}
	out := make([]string, 0, len(environ)+len(names))
	for _, kv := range environ {
		name, _, _ := strings.Cut(kv, "=")
		if !refresh[name] {
			out = append(out, kv)
		}
	}
	for _, n := range names {
		if v, ok := file[n]; ok && refresh[n] {
			out = append(out, n+"="+v)
		}
	}
	return out
}

// Change is one setting a refresh moved, for the spawn's record.
type Change struct {
	Name, Was, Now string
}

// Successor answers the environment a spawned daemon starts with: environ
// refreshed from the env file EnvFile names, under the settings the manifest
// at manifestPath declares, and the settings it changed. With no EnvFile in
// environ it is environ unchanged. An env file or manifest that cannot be
// read is the caller's failure: a successor that would silently keep stale
// settings is the defect this package exists to end.
func Successor(environ []string, manifestPath string) ([]string, []Change, error) {
	path := ""
	for _, kv := range environ {
		if v, ok := strings.CutPrefix(kv, EnvFile+"="); ok {
			path = v
		}
	}
	if path == "" {
		return environ, nil, nil
	}
	names, err := SettingNames(manifestPath)
	if err != nil {
		return nil, nil, err
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, nil, fmt.Errorf("userenv: read the env file %s: %w", path, err)
	}
	file, err := ParseEnvFile(data)
	if err != nil {
		return nil, nil, fmt.Errorf("userenv: %s: %w", path, err)
	}
	out := Refresh(environ, names, file)
	return out, changes(environ, out, names), nil
}

// changes lists each of names whose value differs between before and after.
func changes(before, after, names []string) []Change {
	b, a := index(before), index(after)
	var out []Change
	for _, n := range names {
		if b[n] != a[n] {
			out = append(out, Change{Name: n, Was: b[n], Now: a[n]})
		}
	}
	return out
}

func index(environ []string) map[string]string {
	out := map[string]string{}
	for _, kv := range environ {
		if n, v, ok := strings.Cut(kv, "="); ok {
			out[n] = v
		}
	}
	return out
}
