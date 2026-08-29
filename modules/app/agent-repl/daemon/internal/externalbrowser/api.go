// Package externalbrowser opens a link in the pinned external browser profile.
//
// It is the OpenExternal verb's whole implementation: a clicked link never
// navigates the webview, and the browser profile is pinned so a link does not
// land in whatever window happened to be frontmost.
package externalbrowser

import (
	"context"

	"claude-repld/internal/notimpl"
)

// Opener opens links externally.
type Opener interface {
	// Open launches url in the pinned profile. A refused or malformed url is
	// an error the caller surfaces; nothing is opened silently.
	Open(ctx context.Context, url string) error
}

// New builds the opener. profile names the pinned browser profile;
// ARCHITECTURE.md does not fix its spelling, so the minimum the contract
// implies is the platform-specific profile identifier the launcher needs.
func New(profile string) (Opener, error) {
	return nil, notimpl.Err
}
