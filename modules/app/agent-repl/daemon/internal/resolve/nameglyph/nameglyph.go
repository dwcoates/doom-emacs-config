// Package nameglyph is THE ONE PLACE the glyph drawn before a workspace's name
// is decided.
//
// The name is drawn in the sidebar's row (frontend.v1.RosterRowName.glyph)
// and on the topbar (frontend.v1.TopbarWorkspaceName.glyph), and the editor's
// workspace switcher draws the row's. Both resolvers ask this package, so the
// two surfaces can never draw different glyphs for one workspace.
//
// A glyph is a mark of how the workspace came to be, which its name never
// carries: a name is a slug every verb resolves.
package nameglyph

import "agent-repld/internal/wsm"

// EmojiAck is the glyph a workspace the Slack emoji-ack reaction system
// created wears before its name: the thumbs up the reaction itself is.
const EmojiAck = "👍"

// Of answers the glyph drawn before WS's name, and false when it wears none.
func Of(ws wsm.Workspace) (string, bool) {
	if ws.EmojiAck {
		return EmojiAck, true
	}
	return "", false
}
