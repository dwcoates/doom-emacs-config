package reuse

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"regexp"
	"sort"
	"strconv"
)

// secretEnvName matches a variable whose value a record or a log never holds
// in the clear: a token, a password, a key.
var secretEnvName = regexp.MustCompile(`(?i)(TOKEN|SECRET|PASSWORD|PASSWD|CREDENTIAL|AUTH|COOKIE|(^|_)KEY($|_))`)

// RedactedEnv is env as a record stores it and a log prints it: a secret's
// value is replaced by a short digest of it, so a change to it still shows as
// a change, and its value is never written.
func RedactedEnv(env map[string]string) map[string]string {
	out := make(map[string]string, len(env))
	for name, value := range env {
		if secretEnvName.MatchString(name) {
			sum := sha256.Sum256([]byte(value))
			value = "<redacted " + hex.EncodeToString(sum[:])[:8] + ">"
		}
		out[name] = value
	}
	return out
}

// EnvChange is one environment variable whose value differs from a record's,
// both values redacted (RedactedEnv).
type EnvChange struct {
	Name string
	// Old is the record's value, and OldSet whether the record's environment
	// held the variable at all; OldKnown is false for a record made before
	// records carried their environment, whose values are unknown.
	Old              string
	OldSet, OldKnown bool
	// New is this run's value, and NewSet whether this run's environment
	// holds the variable.
	New    string
	NewSet bool
}

// EnvChanges are the variables of now (a key's Facts.Env) whose values
// differ from the record's, sorted by name. A record with no environment
// stored predates it: every variable its manifest names differently is a
// change of unknown old value.
func EnvChanges(rec Record, now map[string]string, changed []string) []EnvChange {
	redacted := RedactedEnv(now)
	var out []EnvChange
	if rec.Env == nil {
		for _, entry := range changed {
			name, ok := cutEnvEntry(entry)
			if !ok {
				continue
			}
			v, set := redacted[name]
			out = append(out, EnvChange{Name: name, New: v, NewSet: set})
		}
		return out
	}
	names := map[string]bool{}
	for name := range redacted {
		names[name] = true
	}
	for name := range rec.Env {
		names[name] = true
	}
	for name := range names {
		old, oldSet := rec.Env[name]
		v, set := redacted[name]
		if old == v && oldSet == set {
			continue
		}
		out = append(out, EnvChange{Name: name, Old: old, OldSet: oldSet, OldKnown: true, New: v, NewSet: set})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Name < out[j].Name })
	return out
}

// envEntryPrefix is how a manifest names a key's environment variable.
const envEntryPrefix = "@env "

// cutEnvEntry answers the variable a manifest entry names, if it names one.
func cutEnvEntry(entry string) (string, bool) {
	if len(entry) <= len(envEntryPrefix) || entry[:len(envEntryPrefix)] != envEntryPrefix {
		return "", false
	}
	return entry[len(envEntryPrefix):], true
}

// String spells the change for a log line: `GOFLAGS was "-p=1", is now
// "-race"`.
func (c EnvChange) String() string {
	old := "unknown (its record predates stored environments)"
	if c.OldKnown {
		old = envValue(c.Old, c.OldSet)
	}
	return fmt.Sprintf("%s was %s, is now %s", c.Name, old, envValue(c.New, c.NewSet))
}

func envValue(v string, set bool) string {
	if !set {
		return "unset"
	}
	return strconv.Quote(v)
}
