package newsdigest

// Format is how a watched source is read, and so how what is NEW in it is
// told apart from what the previous run already read.
type Format int

// The formats. A FEED (atom, rss, npm, changelog) has entries with stable ids,
// and an entry is new when its id was not read before. A PAGE has none: its
// text is compared against the previous run's, and only the changed text is
// new.
const (
	formatUnset Format = iota
	// FormatAtom is an Atom feed (GitHub's releases.atom).
	FormatAtom
	// FormatRSS is an RSS 2.0 feed (the status page's history).
	FormatRSS
	// FormatNPM is an npm registry document: its `time` map dates every
	// published version.
	FormatNPM
	// FormatChangelog is a markdown changelog whose entries are its `## `
	// headings.
	FormatChangelog
	// FormatPage is an HTML page with no feed; its text is diffed.
	FormatPage
)

// String names the format for the log.
func (f Format) String() string {
	switch f {
	case FormatAtom:
		return "atom"
	case FormatRSS:
		return "rss"
	case FormatNPM:
		return "npm"
	case FormatChangelog:
		return "changelog"
	case FormatPage:
		return "page"
	default:
		return "unset"
	}
}

// Source is one watched source.
type Source struct {
	// Key is the source's durable identity: its snapshot is stored under it.
	// Never reused for a different source.
	Key string
	// Name is the display name the overlay's source row draws.
	Name string
	// URL is what the daemon fetches.
	URL string
	// Home is the page a person opens for the source: the overlay's source
	// link, and the link of an entry that carries none of its own.
	Home string
	// Format is how the source is read.
	Format Format
}

// DefaultSources are the watched sources, in the order the overlay's source
// rows draw them: docs/protobuf-design/news-digest.md "Watched sources"
// (verified reachable 2026-10-02).
var DefaultSources = []Source{
	{Key: "agent-sdk-typescript-releases", Name: "Agent SDK (TypeScript) releases",
		URL:    "https://github.com/anthropics/claude-agent-sdk-typescript/releases.atom",
		Home:   "https://github.com/anthropics/claude-agent-sdk-typescript/releases",
		Format: FormatAtom},
	{Key: "agent-sdk-python-releases", Name: "Agent SDK (Python) releases",
		URL:    "https://github.com/anthropics/claude-agent-sdk-python/releases.atom",
		Home:   "https://github.com/anthropics/claude-agent-sdk-python/releases",
		Format: FormatAtom},
	{Key: "agent-sdk-npm", Name: "Agent SDK npm versions",
		URL:    "https://registry.npmjs.org/@anthropic-ai/claude-agent-sdk",
		Home:   "https://www.npmjs.com/package/@anthropic-ai/claude-agent-sdk",
		Format: FormatNPM},
	{Key: "claude-code-releases", Name: "Claude Code releases",
		URL:    "https://github.com/anthropics/claude-code/releases.atom",
		Home:   "https://github.com/anthropics/claude-code/releases",
		Format: FormatAtom},
	{Key: "claude-code-changelog", Name: "Claude Code changelog",
		URL:    "https://raw.githubusercontent.com/anthropics/claude-code/main/CHANGELOG.md",
		Home:   "https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md",
		Format: FormatChangelog},
	{Key: "anthropic-status-history", Name: "Anthropic status history",
		URL:    "https://status.anthropic.com/history.rss",
		Home:   "https://status.anthropic.com/history",
		Format: FormatRSS},
	{Key: "anthropic-news", Name: "Anthropic news",
		URL:    "https://www.anthropic.com/news",
		Home:   "https://www.anthropic.com/news",
		Format: FormatPage},
	{Key: "claude-platform-release-notes", Name: "Claude platform release notes",
		URL:    "https://docs.claude.com/en/release-notes/overview",
		Home:   "https://docs.claude.com/en/release-notes/overview",
		Format: FormatPage},
	{Key: "claude-code-changelog-docs", Name: "Claude Code changelog (docs)",
		URL:    "https://docs.claude.com/en/docs/claude-code/changelog",
		Home:   "https://docs.claude.com/en/docs/claude-code/changelog",
		Format: FormatPage},
	{Key: "model-deprecations", Name: "Model deprecations",
		URL:    "https://docs.claude.com/en/docs/about-claude/model-deprecations",
		Home:   "https://docs.claude.com/en/docs/about-claude/model-deprecations",
		Format: FormatPage},
	{Key: "claude-apps-release-notes", Name: "Claude apps release notes",
		URL:    "https://support.claude.com/en/articles/12138966-release-notes",
		Home:   "https://support.claude.com/en/articles/12138966-release-notes",
		Format: FormatPage},
}
