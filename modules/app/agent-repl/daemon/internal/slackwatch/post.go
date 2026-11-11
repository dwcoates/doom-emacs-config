package slackwatch

import (
	"encoding/json"
	"fmt"
	"regexp"
	"strings"

	"agent-repld/internal/wsm"
)

// DefaultPostLimit is how many posts a subscription makes before it pauses
// for the user's approval (agentrepl.v1.SlackThreadPosts), the gns-sockets
// plugin's default.
const DefaultPostLimit = 20

// leadingEmoji is a post's mandatory opening: one shortcode, then a space,
// then the message. Two shortcodes back to back (`:a::b: x`) do not match,
// since the first is followed by a colon rather than a space.
var leadingEmoji = regexp.MustCompile(`^:([a-z0-9_+'-]+): \S`)

// reservedEmojiPrefix opens every shortcode the daemon's own announcements
// use; an agent's post may not open with one.
const reservedEmojiPrefix = "peepo_"

// emojiRefusal answers why TEXT's opening breaks the emoji rule: "" when it
// keeps it, else the reserved shortcode it opens with, or "-" when no single
// shortcode and space open it.
func emojiRefusal(text string) string {
	m := leadingEmoji.FindStringSubmatch(text)
	if m == nil {
		return "-"
	}
	if strings.HasPrefix(m[1], reservedEmojiPrefix) {
		return m[1]
	}
	return ""
}

// bannerText is the small context line above every post the daemon makes,
// naming agent-repl and the workspace, so a reader of the thread tells a
// workspace's post from a person's.
func bannerText(workspace string) string {
	return fmt.Sprintf(":satellite_antenna: *agent-repl*  ·  `%s`", workspace)
}

// postBlocks is the Block Kit body of one post: the banner, then TEXT with
// its footer carrying NONCE (wsm.SlackPostFooter), which is how the daemon
// knows the post as its own when it comes back on the echo pipe.
func postBlocks(workspace, nonce, text string) []byte {
	body := strings.TrimRight(text, "\n ") + "\n\n" + wsm.SlackPostFooter(workspace, nonce)
	blocks := []map[string]any{
		{"type": "context", "elements": []map[string]any{{"type": "mrkdwn", "text": bannerText(workspace)}}},
		{"type": "section", "text": map[string]any{"type": "mrkdwn", "text": body}},
	}
	out, err := json.Marshal(blocks)
	if err != nil {
		// Every value is a string the daemon built.
		panic(fmt.Sprintf("slackwatch: encode a post's blocks: %v", err))
	}
	return out
}

// modeWords says what a workspace in MODE does in the thread, for the
// announcements.
func modeWords(mode wsm.SlackThreadMode) string {
	switch mode {
	case wsm.SlackThreadModeListen:
		return "will read this thread and not post"
	case wsm.SlackThreadModeRespond:
		return fmt.Sprintf("may reply here on its own (at most %d posts before pausing for approval)", DefaultPostLimit)
	case wsm.SlackThreadModeInteract:
		return fmt.Sprintf("will act on requests in this thread and report back here (at most %d posts before pausing for approval)", DefaultPostLimit)
	}
	panic(fmt.Sprintf("slackwatch: no mode %q", mode))
}

// The daemon's own announcements in a thread; each opens with a reserved
// shortcode.
func attachAnnouncement(mode wsm.SlackThreadMode) string {
	return fmt.Sprintf(":peepo_wave_happy: agent attached in *%s* mode: it %s.", mode, modeWords(mode))
}

func modeChangeAnnouncement(mode wsm.SlackThreadMode) string {
	return fmt.Sprintf(":peepo_ride_goose_cowboy: agent mode changed to *%s*: it %s.", mode, modeWords(mode))
}

func detachAnnouncement() string {
	return ":peepo_run_cry: agent detached: this thread is no longer followed. Subscribing again resumes it."
}

func limitAnnouncement() string {
	return fmt.Sprintf(":peepo_run_cry: agent reached its limit of %d posts in this thread and now only reads it, until its user switches it back to respond or interact.", DefaultPostLimit)
}
