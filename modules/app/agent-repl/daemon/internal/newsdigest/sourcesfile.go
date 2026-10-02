package newsdigest

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
)

// EnvSources names a JSON file of sources that REPLACES DefaultSources. It is
// a TEST knob: the integration suite points it at fixture servers, so no test
// reads Anthropic's real sources.
const EnvSources = "AGENT_REPL_NEWS_DIGEST_SOURCES"

// sourceFile is one source as the sources file spells it.
type sourceFile struct {
	Key    string `json:"key"`
	Name   string `json:"name"`
	URL    string `json:"url"`
	Home   string `json:"home"`
	Format string `json:"format"`
}

// formatsByName are the format spellings the sources file takes.
var formatsByName = map[string]Format{
	"atom": FormatAtom, "rss": FormatRSS, "npm": FormatNPM, "changelog": FormatChangelog, "page": FormatPage,
}

// LoadSources reads a sources file: a JSON array of {key, name, url, home,
// format}. An unreadable file, an unknown field or format, or a list New
// would refuse is an error, never a partial list.
func LoadSources(path string) ([]Source, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("newsdigest: read the sources file: %w", err)
	}
	dec := json.NewDecoder(bytes.NewReader(raw))
	dec.DisallowUnknownFields()
	var listed []sourceFile
	if err := dec.Decode(&listed); err != nil {
		return nil, fmt.Errorf("newsdigest: the sources file %s did not decode: %w", path, err)
	}
	if len(listed) == 0 {
		return nil, fmt.Errorf("newsdigest: the sources file %s lists no source", path)
	}
	sources := make([]Source, 0, len(listed))
	for _, l := range listed {
		format, ok := formatsByName[l.Format]
		if !ok {
			return nil, fmt.Errorf("newsdigest: source %q names the unknown format %q", l.Key, l.Format)
		}
		sources = append(sources, Source{Key: l.Key, Name: l.Name, URL: l.URL, Home: l.Home, Format: format})
	}
	if err := validateSources(sources); err != nil {
		return nil, err
	}
	return sources, nil
}
