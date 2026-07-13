// Package account reports WHICH Claude account a config root is logged
// into.
//
// The topbar needs this. A session runs as whatever account its
// CLAUDE_CONFIG_DIR selects, and "which account am I about to spend
// tokens as" is not something a user should have to infer from a
// workspace path — especially with two accounts in play, where the wrong
// one is both easy to reach and expensive to notice.
//
// The identity lives in .claude.json, not in the credential store: the
// credentials themselves sit in the OS keychain, but the CLI records the
// human-readable account beside them. Two roots, two rules, both verified
// against the live install:
//
//   - CLAUDE_CONFIG_DIR set   -> <configDir>/.claude.json
//   - CLAUDE_CONFIG_DIR unset -> $HOME/.claude.json
//
// The unset case is deliberately NOT $HOME/.claude/.claude.json. ~/.claude
// is the default config ROOT, but the identity file is its sibling, and
// looking for it inside would report every default-account session as
// logged out.
package account

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
)

// identityFile is the CLI's own name for the file, in both roots.
const identityFile = ".claude.json"

// Identity is who a config root is logged in as.
type Identity struct {
	// ConfigDir is the CLAUDE_CONFIG_DIR this was read for, "" meaning the
	// CLI's own default root.
	ConfigDir string `json:"config_dir"`
	// Email is the logged-in account, or "" when the root has no login.
	Email string `json:"email"`
}

// LoggedIn reports whether the root has an account at all.
func (i Identity) LoggedIn() bool { return i.Email != "" }

// Path returns the identity file backing CONFIGDIR.
func Path(configDir string) (string, error) {
	if configDir != "" {
		return filepath.Join(configDir, identityFile), nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("account: locating home for the default config root: %w", err)
	}
	return filepath.Join(home, identityFile), nil
}

// Read returns CONFIGDIR's identity.
//
// A root that has never been logged into yields an empty Email and NO
// error: "logged out" is a state the topbar renders, not a failure to
// report. A root whose identity file exists but cannot be parsed IS an
// error — that is a corrupt install, and reporting it as "logged out"
// would send the user off to fix the wrong problem.
func Read(configDir string) (Identity, error) {
	id := Identity{ConfigDir: configDir}

	path, err := Path(configDir)
	if err != nil {
		return id, err
	}
	raw, err := os.ReadFile(path) //nolint:gosec // path is daemon-derived, not user input
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return id, nil
		}
		return id, fmt.Errorf("account: reading %s: %w", path, err)
	}

	// Decode ONLY the account block. .claude.json is a large, evolving
	// document holding plenty the daemon has no business parsing, and a
	// struct that named more of it would break every time the CLI adds a
	// field.
	var doc struct {
		OAuthAccount struct {
			EmailAddress string `json:"emailAddress"`
		} `json:"oauthAccount"`
	}
	if err := json.Unmarshal(raw, &doc); err != nil {
		return id, fmt.Errorf("account: parsing %s: %w", path, err)
	}
	id.Email = doc.OAuthAccount.EmailAddress
	return id, nil
}
