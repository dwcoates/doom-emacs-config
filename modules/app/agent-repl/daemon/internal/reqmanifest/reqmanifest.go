// Package reqmanifest reads the shape of agent-repl's requirements manifests
// that the daemon and its harness share: the root manifest (requirements.json
// at the module root) and the manifests it includes, as bin/requirements.sh
// loads them.
package reqmanifest

import (
	"encoding/json"
	"fmt"
	"os"
)

// Includes answers the manifests the root manifest at PATH includes,
// relative to the module root.
func Includes(path string) ([]string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("reqmanifest: read the requirements manifest: %w", err)
	}
	var m struct {
		Includes []string `json:"includes"`
	}
	if err := json.Unmarshal(data, &m); err != nil {
		return nil, fmt.Errorf("reqmanifest: parse the requirements manifest %s: %w", path, err)
	}
	return m.Includes, nil
}
