// Package claudesettings reads what a Claude config root's own settings file
// states, for the facts the daemon must know before any session reports them.
//
// THE ONE FACT TODAY IS THE STARTING EFFORT LEVEL. The topbar's effort
// selector shows the CURRENT level and never a placeholder (owner ruling,
// 2026-10-01), and before any pick that level is what the session's settings
// persist: `effortLevel` at the top of `<config root>/settings.json`, or the
// per-model override `modelSettings.<canonical model>.effortLevel` that the
// vendor's own /effort writes. The vendor reads the same file at session start
// (sdk.d.ts `Settings.effortLevel`, `Settings.modelSettings`).
//
// UNSET IS AN ANSWER, NOT A GUESS. A root with no settings file, or one whose
// file names no level, states nothing; the vendor's own per-model default then
// applies, and no file the daemon can read states that default. The reader
// reports the absence as such and never substitutes a level.
package claudesettings

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/effortlevel"
)

// FileName is the settings file inside a config root.
const FileName = "settings.json"

// EnvVar is the vendor's environment override of the effort level. The
// daemon's environment is the shim's (the supervisor passes it through), so
// the daemon reads the same value the vendor does.
const EnvVar = "CLAUDE_CODE_EFFORT_LEVEL"

// Effort is the effort level a config root's settings file persists, under
// the environment override the vendor applies over it.
type Effort struct {
	// Path is the file the facts were read from, absent or not. It is what a
	// log record names as the level's source.
	Path string
	// Default is the top-level `effortLevel`, UNSPECIFIED when the file states
	// none.
	Default conversationv1.AgentEffortLevel
	// PerModel is `modelSettings.<model>.effortLevel` for every model that
	// states one, keyed by the vendor's canonical model name.
	PerModel map[string]conversationv1.AgentEffortLevel
	// Env is the CLAUDE_CODE_EFFORT_LEVEL override when it names a level.
	Env conversationv1.AgentEffortLevel
	// EnvUnset reports CLAUDE_CODE_EFFORT_LEVEL=unset (or auto): the vendor
	// then sends no level at all, whatever the files persist.
	EnvUnset bool
}

// Source names where a starting level came from.
type Source string

const (
	// SourceUnset is a file that states no level for the model.
	SourceUnset Source = "unset"
	// SourceModelSettings is the per-model override.
	SourceModelSettings Source = "model_settings"
	// SourceDefault is the top-level `effortLevel`.
	SourceDefault Source = "effort_level"
	// SourceEnv is the CLAUDE_CODE_EFFORT_LEVEL override.
	SourceEnv Source = "env"
	// SourceEnvUnset is CLAUDE_CODE_EFFORT_LEVEL=unset: no level is sent.
	SourceEnvUnset Source = "env_unset"
)

// ReadEffort reads the effort facts of CONFIGDIR's settings file. A missing
// file is no error: it states nothing, exactly as a file without the keys
// does. An unreadable or malformed file IS one, and the caller surfaces it.
//
// A level the vendor's vocabulary does not carry is an error too: the file is
// the vendor's, and a spelling the daemon cannot map is a contract change to
// report, never a level to drop silently.
//
// THE ENVIRONMENT OUTRANKS THE FILES. The vendor documents its applied level
// as taken "after env overrides" (sdk.d.ts, SDKSystemMessage.effort), and its
// CLI states that CLAUDE_CODE_EFFORT_LEVEL "overrides effort for this
// session"; `unset` and `auto` mean no level is sent.
func ReadEffort(configDir string) (Effort, error) {
	return readEffort(configDir, os.Getenv)
}

// readEffort is ReadEffort over an injected environment.
func readEffort(configDir string, getenv func(string) string) (Effort, error) {
	path := filepath.Join(configDir, FileName)
	out := Effort{Path: path, PerModel: map[string]conversationv1.AgentEffortLevel{}}
	switch value := getenv(EnvVar); value {
	case "":
	case "unset", "auto":
		out.EnvUnset = true
	default:
		level, err := effortlevel.Parse(value)
		if err != nil {
			return out, fmt.Errorf("claudesettings: %s: %w", EnvVar, err)
		}
		out.Env = level
	}
	raw, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return out, nil
	}
	if err != nil {
		return out, fmt.Errorf("claudesettings: read %s: %w", path, err)
	}
	// Decode ONLY the effort keys: the settings file is a large, evolving
	// vendor document, and a struct naming more of it would break every time
	// the CLI adds a field.
	var doc struct {
		EffortLevel   string `json:"effortLevel"`
		ModelSettings map[string]struct {
			EffortLevel string `json:"effortLevel"`
		} `json:"modelSettings"`
	}
	if err := json.Unmarshal(raw, &doc); err != nil {
		return out, fmt.Errorf("claudesettings: parse %s: %w", path, err)
	}
	if out.Default, err = effortlevel.Parse(doc.EffortLevel); err != nil {
		return out, fmt.Errorf("claudesettings: %s effortLevel: %w", path, err)
	}
	for model, settings := range doc.ModelSettings {
		parsed, err := effortlevel.Parse(settings.EffortLevel)
		if err != nil {
			return out, fmt.Errorf("claudesettings: %s modelSettings[%q].effortLevel: %w", path, model, err)
		}
		if parsed != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
			out.PerModel[model] = parsed
		}
	}
	return out, nil
}

// For answers the level the file persists for MODEL, and where it came from:
// the per-model override when one matches, else the top-level level, else
// unset. MODEL is the session's model as the vendor reported it, in whatever
// spelling: a dated id or a `[1m]` suffix matches its canonical key, as the
// vendor's own lookup does.
func (e Effort) For(model string) (conversationv1.AgentEffortLevel, Source) {
	if e.EnvUnset {
		return conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED, SourceEnvUnset
	}
	if e.Env != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		return e.Env, SourceEnv
	}
	if lvl, ok := e.PerModel[model]; ok {
		return lvl, SourceModelSettings
	}
	// Sorted, so two keys that canonicalize alike resolve the same way on
	// every read rather than by map order.
	keys := make([]string, 0, len(e.PerModel))
	for key := range e.PerModel {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	canonical := Canonical(model)
	for _, key := range keys {
		if Canonical(key) == canonical {
			return e.PerModel[key], SourceModelSettings
		}
	}
	if e.Default != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		return e.Default, SourceDefault
	}
	return conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED, SourceUnset
}

// The spellings a model id carries beyond its canonical name.
var (
	// providerPrefix is a Bedrock id's region and vendor prefix
	// ("us.anthropic.", "anthropic.").
	providerPrefix = regexp.MustCompile(`^(?:[a-z]{2,}\.)?anthropic\.`)
	// bedrockVersion is a Bedrock id's version suffix ("-v1:0", "-v2").
	bedrockVersion = regexp.MustCompile(`-v\d+(?::\d+)?$`)
	// datedSuffix is a model id's release-date suffix ("-20251001").
	datedSuffix = regexp.MustCompile(`-\d{8}$`)
)

// Canonical is MODEL in the spelling the vendor keys `modelSettings` by: its
// context-window suffix ("[1m]"), its Vertex version ("@20251001"), its
// Bedrock prefix and version ("us.anthropic.", "-v1:0") and its release date
// dropped. The vendor matches "its dated, [1m], Bedrock and Vertex spellings"
// to the canonical key (sdk.d.ts, Settings.modelSettings).
func Canonical(model string) string {
	if i := strings.IndexAny(model, "[@"); i >= 0 {
		model = model[:i]
	}
	model = providerPrefix.ReplaceAllString(model, "")
	model = bedrockVersion.ReplaceAllString(model, "")
	return datedSuffix.ReplaceAllString(model, "")
}
