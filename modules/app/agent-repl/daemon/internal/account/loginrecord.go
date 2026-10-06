package account

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"time"
)

// LoginRecord is what a config root's identity file says about its account's
// last login, as the vendor wrote it. Comparing two readings is how the daemon
// tells that a login made through its own login flow completed: nothing parses
// the login TUI (internal/login), but a completed OAuth login rewrites the
// root's `oauthAccount` block, profile fetch stamp and all.
type LoginRecord struct {
	// Block is the `oauthAccount` block, verbatim; empty when the root names
	// no account.
	Block string
	// ProfileFetchedAt is the vendor's stamp of its last fetch of the
	// account's profile (`oauthAccount.profileFetchedAt`, epoch ms), which a
	// login performs; zero when the block carries none.
	ProfileFetchedAt time.Time
}

// ReadLoginRecord reads configDir's login record. A root with no identity
// file, or one naming no account, has an empty record and no error; a file
// that cannot be read or parsed is an error, as it is for Read.
func ReadLoginRecord(configDir string) (LoginRecord, error) {
	if configDir == "" {
		return LoginRecord{}, errors.New("account: ReadLoginRecord requires a config dir")
	}
	path := filepath.Join(configDir, identityFile)
	raw, err := os.ReadFile(path) //nolint:gosec // daemon-derived path, never client input
	if errors.Is(err, fs.ErrNotExist) {
		return LoginRecord{}, nil
	}
	if err != nil {
		return LoginRecord{}, fmt.Errorf("account: reading %s: %w", path, err)
	}
	// Decode ONLY the account block, for the reason Read does.
	var doc struct {
		OAuthAccount json.RawMessage `json:"oauthAccount"`
	}
	if err := json.Unmarshal(raw, &doc); err != nil {
		return LoginRecord{}, fmt.Errorf("account: parsing %s: %w", path, err)
	}
	if len(doc.OAuthAccount) == 0 || string(doc.OAuthAccount) == "null" {
		return LoginRecord{}, nil
	}
	var stamp struct {
		ProfileFetchedAt *int64 `json:"profileFetchedAt"`
	}
	if err := json.Unmarshal(doc.OAuthAccount, &stamp); err != nil {
		return LoginRecord{}, fmt.Errorf("account: parsing the oauthAccount block of %s: %w", path, err)
	}
	record := LoginRecord{Block: string(doc.OAuthAccount)}
	if stamp.ProfileFetchedAt != nil {
		record.ProfileFetchedAt = time.UnixMilli(*stamp.ProfileFetchedAt).UTC()
	}
	return record, nil
}
