// Package slackfmt converts standard Markdown to Slack's own formatting: Block
// Kit blocks, or mrkdwn text. It is the daemon's port of the send-to-slack
// skill's format_message.py, so every post the daemon makes is formatted by
// the same rules a hand-sent post is.
//
// Callers compose plain GitHub-flavored Markdown (`[text](url)`, `**bold**`,
// `## Heading`, `- bullet`) and render it to one of:
//
//   - BLOCK KIT (RenderBlocks): headers, dividers, sections, and rich_text
//     lists that carry nested bullets as real nesting.
//   - MRKDWN (RenderMrkdwn): one string with `<url|text>`, `*bold*` and
//     leading `•` bullets. Slack strips a line's leading spaces, so mrkdwn
//     flattens nesting; it is chosen only for a body that pings someone,
//     because a `<@U...>` mention notifies only there (Render).
//
// THE SUPPORTED SUBSET: `#` headings, `- `/`* ` bullets indented two spaces
// per level, `**bold**`, `_italic_`, “ `code` “, `[label](url)`, a `---`
// divider, and ``` fences whose content is literal. A pull request URL, bare
// or behind another label, is relabelled `#<number>`; a bare `#<number>` is
// linked to Renderer.PRRepo's pull URL when one is configured.
//
// A BODY THAT IS NOT SLACK-NATIVE IS REFUSED (*NotSlackNativeError), never
// rendered: a list or tree drawn by hand with glyphs (`•`, `├──`), or a pull
// request named in a form no hyperlink can be built from. Slack would show
// those characters literally.
//
// The skill's HTML rendering is not ported: it exists only for the clipboard,
// and the daemon never copies a post for a person to paste.
package slackfmt

import (
	"fmt"
	"regexp"
	"strings"
)

// linkLabelAtom is one atom of a Markdown link label: any character that is not
// a bracket, or a whole bracketed run. A label carrying bracketed text of its
// own ("[Game Review] Takeaways") would otherwise stop at its first "]".
const linkLabelAtom = `(?:[^\[\]]|\[[^\[\]]*\])`

var (
	linkRE      = regexp.MustCompile(`\[(` + linkLabelAtom + `+)\]\(([^)]+)\)`)
	mdLinkFull  = regexp.MustCompile(`^\[(` + linkLabelAtom + `+)\]\(([^)]*)\)$`)
	boldRE      = regexp.MustCompile(`\*\*(.+?)\*\*`)
	italBoldRE  = regexp.MustCompile(`\*\*\*(.+?)\*\*\*`)
	fenceRE     = regexp.MustCompile("^(\\s*)```\\s*(\\S*)\\s*$")
	bulletRE    = regexp.MustCompile(`^(\s*)[-*]\s+(.*)$`)
	headingRE   = regexp.MustCompile(`^(#+)\s+(.*)$`)
	inlineCode  = regexp.MustCompile("`[^`]*`")
	protectURL  = regexp.MustCompile(`(?:https?|mailto):\S+`)
	boxDrawing  = regexp.MustCompile(`[\x{2500}-\x{257f}]`)
	mentionRE   = regexp.MustCompile(`<@[UW][A-Z0-9]+>`)
	pullURLRE   = regexp.MustCompile(`^https://github\.com/[\w.-]+/[\w.-]+/pull/(\d+)`)
	pullURLAny  = regexp.MustCompile(`https://github\.com/[\w.-]+/[\w.-]+/pull/(\d+)`)
	codedNumber = regexp.MustCompile("`\\s*#\\d+\\s*`")
	prWordNum   = regexp.MustCompile(`(?i)\b(?:PRs?|pull requests?)\s+\d+\b`)
	repoRef     = regexp.MustCompile(`[\w.-]+/[\w.-]+#\d+|\b[\w.-]+#\d+`)

	// protectedSpan is a span a bare reference is never rewritten inside:
	// inline code, a Markdown link, an already-rendered Slack link, or a URL.
	protectedSpan = regexp.MustCompile("`[^`]*`" +
		`|\[` + linkLabelAtom + `*\]\([^)]*\)` +
		`|<[^<>|]*(?:\|[^<>]*)?>` +
		`|(?:https?|mailto):\S+`)

	// bareRefCandidate is `#` plus digits ending at a word boundary. Whether it
	// is a BARE reference also depends on the character before it
	// (bareRefs), which RE2 cannot look behind for.
	bareRefCandidate = regexp.MustCompile(`#(\d+)\b`)

	inlineBlocksRE = regexp.MustCompile(`\[([^\]]+)\]\(([^)]+)\)` +
		`|\*\*\*([^*]+)\*\*\*` +
		`|\*\*([^*]+)\*\*` +
		"|`([^`]+)`" +
		`|_([^_]+)_`)
)

// urlTrailingPunct is the punctuation a bare URL may be trailed by in prose.
const urlTrailingPunct = ".,;:!?)]"

// bulletGlyphs draw a list marker by hand.
const bulletGlyphs = "•◦▪▫‣⁃∙●○■□►▸▹➤"

// NotSlackNativeError is a body whose structure would reach Slack as literal
// characters rather than as Slack's own formatting, so it is refused instead
// of rendered.
type NotSlackNativeError struct {
	// Reason says what is wrong and how to write it instead.
	Reason string
	// Lines names each offending line.
	Lines []string
}

func (e *NotSlackNativeError) Error() string {
	if len(e.Lines) == 0 {
		return "slackfmt: " + e.Reason
	}
	return "slackfmt: " + e.Reason + ":\n  " + strings.Join(e.Lines, "\n  ")
}

// Renderer renders Markdown for Slack.
type Renderer struct {
	// PRRepo is the GitHub `owner/name` a bare `#<number>` is linked into.
	// Empty links no bare reference: a body carrying one is refused, because
	// it would reach Slack as an unlinked number.
	PRRepo string
}

// Payload is a rendered body: exactly one of Blocks or Text is set.
type Payload struct {
	// Blocks is the Block Kit rendering, nil for a mrkdwn body.
	Blocks []map[string]any
	// Text is the mrkdwn rendering, empty for a Block Kit body.
	Text string
}

// Render renders MD the way a post is delivered: as Block Kit, unless the body
// pings someone. A `<@U...>` mention notifies only in mrkdwn, so such a body
// renders as mrkdwn, and since mrkdwn draws a list only as literal `•`
// characters, a mention-bearing body that also holds a list is refused.
func (r Renderer) Render(md string) (Payload, error) {
	if mentionRE.MatchString(md) {
		text, err := r.RenderMrkdwn(md, false)
		return Payload{Text: text}, err
	}
	blocks, err := r.RenderBlocks(md)
	return Payload{Blocks: blocks}, err
}

// PullNumber answers the number of a GitHub pull request URL.
func PullNumber(url string) (string, bool) {
	m := pullURLRE.FindStringSubmatch(url)
	if m == nil {
		return "", false
	}
	return m[1], true
}

// splitLines splits MD as Python's str.splitlines does for "\n" and "\r\n":
// no trailing empty line for a body ending in a newline.
func splitLines(md string) []string {
	if md == "" {
		return nil
	}
	lines := strings.Split(strings.TrimSuffix(md, "\n"), "\n")
	for i, line := range lines {
		lines[i] = strings.TrimSuffix(line, "\r")
	}
	return lines
}

// proseLine is one line outside a fence, with its inline code removed.
type proseLine struct {
	number int
	line   string
	prose  string
}

// proseLines answers every line outside a fence, where prose is the line with
// its inline code spans removed. Code is literal content, so a check on what
// the author wrote as prose reads this.
func proseLines(md string) []proseLine {
	var out []proseLine
	inFence := false
	for i, line := range splitLines(md) {
		if fenceRE.MatchString(line) {
			inFence = !inFence
			continue
		}
		if !inFence {
			out = append(out, proseLine{number: i + 1, line: line, prose: inlineCode.ReplaceAllString(line, "")})
		}
	}
	return out
}

// handDrawnStructure describes every line that draws a list or tree by hand,
// outside fenced and inline code.
func handDrawnStructure(md string) []string {
	var problems []string
	for _, p := range proseLines(md) {
		trimmed := strings.TrimLeft(p.prose, " \t")
		if trimmed != "" {
			lead := []rune(trimmed)[0]
			if strings.ContainsRune(bulletGlyphs, lead) {
				problems = append(problems, fmt.Sprintf("line %d: hand-drawn bullet %q: %s", p.number, string(lead), strings.TrimSpace(p.line)))
				continue
			}
		}
		if glyph := boxDrawing.FindString(p.prose); glyph != "" {
			problems = append(problems, fmt.Sprintf("line %d: hand-drawn tree glyph %q: %s", p.number, glyph, strings.TrimSpace(p.line)))
		}
	}
	return problems
}

// bareRefs answers the [start, end, numberStart, numberEnd] index of every
// bare reference in TEXT: `#` plus digits, not glued to a word character, a
// slash, or another `#` before it.
func bareRefs(text string) [][]int {
	var out [][]int
	for _, m := range bareRefCandidate.FindAllStringSubmatchIndex(text, -1) {
		if m[0] > 0 {
			prev := text[m[0]-1]
			if prev == '/' || prev == '#' || prev == '_' || prev >= 0x80 || isASCIIAlnum(prev) {
				continue
			}
		}
		out = append(out, m)
	}
	return out
}

func isASCIIAlnum(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b >= '0' && b <= '9')
}

// unlinkedPRReferences describes every line naming a pull request that would
// reach Slack unlinked: a `#<number>` in inline code or a heading, a
// `PR <number>` without its `#`, a `<repo>#<number>`, or, with no PRRepo to
// link into, a bare `#<number>`.
func (r Renderer) unlinkedPRReferences(md string) []string {
	var problems []string
	for _, p := range proseLines(md) {
		stripped := protectURL.ReplaceAllString(linkRE.ReplaceAllString(p.prose, ""), "")
		reason := ""
		switch {
		case codedNumber.MatchString(p.line):
			reason = "a '#<number>' in inline code is never hyperlinked"
		case headingRE.MatchString(p.line) && (len(bareRefs(stripped)) > 0 || pullURLAny.MatchString(p.line)):
			reason = "a heading cannot carry a hyperlink"
		case prWordNum.MatchString(stripped):
			reason = "a pull request named without its '#<number>' is never hyperlinked"
		case repoRef.MatchString(stripped):
			reason = "a '<repo>#<number>' reference names a repository that cannot be resolved"
		case r.PRRepo == "" && len(bareRefs(stripped)) > 0:
			reason = "a bare '#<number>' has no repository to link into; link it as '[#<number>](url)'"
		}
		if reason != "" {
			problems = append(problems, fmt.Sprintf("line %d: %s: %s", p.number, reason, strings.TrimSpace(p.line)))
		}
	}
	return problems
}

// requireSlackNative refuses a body that draws its lists or trees by hand, or
// names a pull request no hyperlink can be built for. Every rendering calls it
// first.
func (r Renderer) requireSlackNative(md string) error {
	if problems := handDrawnStructure(md); len(problems) > 0 {
		return &NotSlackNativeError{
			Reason: "the body draws lists by hand; write each bullet as a '- ' Markdown list item, indented two spaces per nesting level",
			Lines:  problems,
		}
	}
	if problems := r.unlinkedPRReferences(md); len(problems) > 0 {
		return &NotSlackNativeError{
			Reason: "the body names a pull request that would reach Slack unlinked; write each as its name followed by a '#<number>' reference outside code and headings",
			Lines:  problems,
		}
	}
	return nil
}

// linkifyRefs rewrites every pull request reference into a link labelled
// `#<number>`: a bare `#<number>` (with a PRRepo) and a pull request URL,
// bare or behind any other label. An authored `[#<number>](url)` and every
// other protected span pass through verbatim.
func (r Renderer) linkifyRefs(text string) string {
	var out strings.Builder
	last := 0
	for _, m := range protectedSpan.FindAllStringIndex(text, -1) {
		out.WriteString(r.linkBareRefs(text[last:m[0]]))
		out.WriteString(relabelPullSpan(text[m[0]:m[1]]))
		last = m[1]
	}
	out.WriteString(r.linkBareRefs(text[last:]))
	return out.String()
}

// linkBareRefs links every bare reference in TEXT into PRRepo, leaving TEXT
// as it is when no PRRepo is configured (requireSlackNative refused such a
// body already).
func (r Renderer) linkBareRefs(text string) string {
	if r.PRRepo == "" {
		return text
	}
	var out strings.Builder
	last := 0
	for _, m := range bareRefs(text) {
		number := text[m[2]:m[3]]
		out.WriteString(text[last:m[0]])
		out.WriteString("[#" + number + "](https://github.com/" + r.PRRepo + "/pull/" + number + ")")
		last = m[1]
	}
	out.WriteString(text[last:])
	return out.String()
}

// relabelPullSpan answers a protected span with a pull request URL labelled
// `#<number>`, or the span unchanged when it holds none.
func relabelPullSpan(span string) string {
	if link := mdLinkFull.FindStringSubmatch(span); link != nil {
		url := strings.TrimSpace(link[2])
		pull := pullURLRE.FindStringSubmatch(url)
		if pull != nil && link[1] != "#"+pull[1] {
			return fmt.Sprintf("[#%s](%s)", pull[1], url)
		}
		return span
	}
	pull := pullURLRE.FindStringSubmatch(span)
	if pull == nil {
		return span
	}
	url := strings.TrimRight(span, urlTrailingPunct)
	return fmt.Sprintf("[#%s](%s)", pull[1], url) + span[len(url):]
}

// takeFence consumes the fenced block opening at lines[i], answering its
// literal content and the index just past the closing fence. The opening
// fence's own indentation is stripped from every content line; an unclosed
// fence runs to the end of the input.
func takeFence(lines []string, i int) (string, int) {
	indent := len(fenceRE.FindStringSubmatch(lines[i])[1])
	var body []string
	i++
	for i < len(lines) && !fenceRE.MatchString(lines[i]) {
		line := lines[i]
		if indent > 0 && len(line) >= indent && strings.TrimSpace(line[:indent]) == "" {
			line = line[indent:]
		}
		body = append(body, line)
		i++
	}
	return strings.Join(body, "\n"), min(i+1, len(lines))
}

// toMrkdwnInline converts links and bold; backticks and underscores survive
// untouched, since mrkdwn spells them the same way.
func toMrkdwnInline(text string) string {
	text = linkRE.ReplaceAllString(text, "<$2|$1>")
	text = italBoldRE.ReplaceAllString(text, "_*${1}*_")
	return boldRE.ReplaceAllString(text, "*${1}*")
}

// RenderMrkdwn renders MD as mrkdwn text. mrkdwn has no list syntax, so its
// bullets are literal `•` characters; allowLists false refuses a body holding
// a list rather than render it that way.
func (r Renderer) RenderMrkdwn(md string, allowLists bool) (string, error) {
	if err := r.requireSlackNative(md); err != nil {
		return "", err
	}
	if !allowLists {
		for _, n := range r.parse(md) {
			if n.kind == nodeBullets {
				return "", &NotSlackNativeError{Reason: "this body renders as plain text, which carries a list only as literal '•' characters rather than as a Slack list; send the list separately from the part that needs plain text"}
			}
		}
	}
	var out []string
	lines := splitLines(md)
	for i := 0; i < len(lines); {
		raw := lines[i]
		if fenceRE.MatchString(raw) {
			var body string
			body, i = takeFence(lines, i)
			// The delimiters go at column 0: Slack opens a code block only on
			// an unindented fence.
			out = append(out, "```", body, "```")
			continue
		}
		i++
		line := r.linkifyRefs(strings.TrimRight(raw, " \t"))
		switch {
		case strings.TrimSpace(line) == "":
			out = append(out, "")
		case strings.TrimSpace(line) == "---":
			out = append(out, "──")
		case headingRE.MatchString(line):
			out = append(out, "*"+toMrkdwnInline(strings.TrimSpace(headingRE.FindStringSubmatch(line)[2]))+"*")
		case bulletRE.MatchString(line):
			m := bulletRE.FindStringSubmatch(line)
			out = append(out, m[1]+"• "+toMrkdwnInline(m[2]))
		default:
			out = append(out, toMrkdwnInline(line))
		}
	}
	return strings.Join(out, "\n"), nil
}

// nodeKind is one structural node's kind.
type nodeKind int

const (
	nodeFence nodeKind = iota
	nodeDivider
	nodeHeading
	nodeBullets
	nodeParagraph
)

// bullet is one list item: its nesting level and its text.
type bullet struct {
	level int
	text  string
}

// node is one structural node of a body.
type node struct {
	kind nodeKind
	// text is a fence's body, a heading's text, or a paragraph's text.
	text string
	// bullets is one run of consecutive bullets.
	bullets []bullet
}

// parse splits a body into its structural nodes, in order. Bullet and
// paragraph text carries its references already linkified; heading text is
// left as authored.
func (r Renderer) parse(md string) []node {
	var nodes []node
	lines := splitLines(md)
	for i := 0; i < len(lines); {
		line := lines[i]
		stripped := strings.TrimSpace(line)
		switch {
		case stripped == "":
			i++
		case fenceRE.MatchString(line):
			var body string
			body, i = takeFence(lines, i)
			nodes = append(nodes, node{kind: nodeFence, text: body})
		case stripped == "---":
			nodes = append(nodes, node{kind: nodeDivider})
			i++
		case headingRE.MatchString(line):
			nodes = append(nodes, node{kind: nodeHeading, text: strings.TrimSpace(headingRE.FindStringSubmatch(line)[2])})
			i++
		case bulletRE.MatchString(line):
			var run []bullet
			for i < len(lines) {
				m := bulletRE.FindStringSubmatch(lines[i])
				if m == nil {
					break
				}
				run = append(run, bullet{level: len(m[1]) / 2, text: r.linkifyRefs(m[2])})
				i++
			}
			nodes = append(nodes, node{kind: nodeBullets, bullets: run})
		default:
			nodes = append(nodes, node{kind: nodeParagraph, text: r.linkifyRefs(line)})
			i++
		}
	}
	return nodes
}

// parseInlineBlocks converts inline Markdown to Block Kit rich_text inline
// elements.
func parseInlineBlocks(text string) []map[string]any {
	var elements []map[string]any
	add := func(el map[string]any) {
		if el["text"] != "" || el["url"] != nil {
			elements = append(elements, el)
		}
	}
	last := 0
	for _, m := range inlineBlocksRE.FindAllStringSubmatchIndex(text, -1) {
		if m[0] > last {
			add(map[string]any{"type": "text", "text": text[last:m[0]]})
		}
		group := func(n int) (string, bool) {
			if m[2*n] < 0 {
				return "", false
			}
			return text[m[2*n]:m[2*n+1]], true
		}
		if label, ok := group(1); ok {
			url, _ := group(2)
			add(map[string]any{"type": "link", "url": url, "text": label})
		} else if s, ok := group(3); ok {
			add(map[string]any{"type": "text", "text": s, "style": map[string]any{"bold": true, "italic": true}})
		} else if s, ok := group(4); ok {
			add(map[string]any{"type": "text", "text": s, "style": map[string]any{"bold": true}})
		} else if s, ok := group(5); ok {
			add(map[string]any{"type": "text", "text": s, "style": map[string]any{"code": true}})
		} else if s, ok := group(6); ok {
			add(map[string]any{"type": "text", "text": s, "style": map[string]any{"italic": true}})
		}
		last = m[1]
	}
	if last < len(text) {
		add(map[string]any{"type": "text", "text": text[last:]})
	}
	return elements
}

// bulletsToRichText renders one run of bullets as a rich_text block whose
// lists carry each nesting level as its own indent.
func bulletsToRichText(bullets []bullet) map[string]any {
	var lists []any
	for i := 0; i < len(bullets); {
		level := bullets[i].level
		var group []any
		for i < len(bullets) && bullets[i].level == level {
			group = append(group, map[string]any{"type": "rich_text_section", "elements": parseInlineBlocks(bullets[i].text)})
			i++
		}
		lists = append(lists, map[string]any{"type": "rich_text_list", "style": "bullet", "indent": level, "elements": group})
	}
	return map[string]any{"type": "rich_text", "elements": lists}
}

// preformattedBlock is a monospace, multi-line block Slack renders untouched.
func preformattedBlock(body string) map[string]any {
	return map[string]any{
		"type": "rich_text",
		"elements": []any{map[string]any{
			"type":     "rich_text_preformatted",
			"elements": []any{map[string]any{"type": "text", "text": body}},
		}},
	}
}

// RenderBlocks renders MD as Block Kit blocks.
func (r Renderer) RenderBlocks(md string) ([]map[string]any, error) {
	if err := r.requireSlackNative(md); err != nil {
		return nil, err
	}
	var blocks []map[string]any
	for _, n := range r.parse(md) {
		switch n.kind {
		case nodeFence:
			blocks = append(blocks, preformattedBlock(n.text))
		case nodeDivider:
			blocks = append(blocks, map[string]any{"type": "divider"})
		case nodeHeading:
			blocks = append(blocks, map[string]any{"type": "header", "text": map[string]any{"type": "plain_text", "text": n.text, "emoji": true}})
		case nodeBullets:
			blocks = append(blocks, bulletsToRichText(n.bullets))
		case nodeParagraph:
			blocks = append(blocks, map[string]any{"type": "section", "text": map[string]any{"type": "mrkdwn", "text": toMrkdwnInline(n.text)}})
		default:
			panic(fmt.Sprintf("slackfmt: parse produced an unknown node kind %d", n.kind))
		}
	}
	return blocks, nil
}
