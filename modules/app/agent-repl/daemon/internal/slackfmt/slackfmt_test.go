package slackfmt

import (
	"errors"
	"reflect"
	"strings"
	"testing"
)

const pr7477 = "https://github.com/ChessCom/explanation-engine/pull/7477"
const pr8033 = "https://github.com/ChessCom/explanation-engine/pull/8033"

// ee links bare references into the repository format_message.py links into,
// so these cases read as the skill's own.
var ee = Renderer{PRRepo: "ChessCom/explanation-engine"}

const bulletWithFence = "- a signal verdict, with the query behind it\n" +
	"  ```\n" +
	"  sum by (version) (rate(m{env=\"staging\",error_type=\"x\"}[30m])) * 60\n" +
	"  ```\n" +
	"- the next signal\n"

const nestedBullets = "- the top-level item\n  - the item nested under it\n"

func mustMrkdwn(t *testing.T, r Renderer, md string) string {
	t.Helper()
	out, err := r.RenderMrkdwn(md, true)
	if err != nil {
		t.Fatalf("RenderMrkdwn(%q) = %v", md, err)
	}
	return out
}

func mustBlocks(t *testing.T, r Renderer, md string) []map[string]any {
	t.Helper()
	out, err := r.RenderBlocks(md)
	if err != nil {
		t.Fatalf("RenderBlocks(%q) = %v", md, err)
	}
	return out
}

// elementsOf answers a block's "elements", empty for a block without them.
func elementsOf(block map[string]any) []map[string]any {
	raw, _ := block["elements"].([]any)
	var out []map[string]any
	for _, el := range raw {
		out = append(out, el.(map[string]any))
	}
	return out
}

func preformattedBodies(blocks []map[string]any) []string {
	var out []string
	for _, block := range blocks {
		for _, el := range elementsOf(block) {
			if el["type"] == "rich_text_preformatted" {
				out = append(out, elementsOf(el)[0]["text"].(string))
			}
		}
	}
	return out
}

func TestRenderMrkdwn(t *testing.T) {
	tests := []struct {
		name string
		md   string
		want string
	}{
		{"leaves fenced content literal", "```\n**not bold** [not a link](url) _not italic_\n```\n", "```\n**not bold** [not a link](url) _not italic_\n```"},
		{"keeps indentation inside a fence", "```\nif x:\n    return 1\n```\n", "```\nif x:\n    return 1\n```"},
		{"drops a fence's language tag", "```promql\nup\n```\n", "```\nup\n```"},
		{"linkifies a bare reference", "Merged #7477\n", "Merged <" + pr7477 + "|#7477>"},
		{"leaves an already linked reference alone", "Merged [#7477](" + pr7477 + ")\n", "Merged <" + pr7477 + "|#7477>"},
		{"leaves a rendered slack link alone", "Merged <" + pr7477 + "|#7477>\n", "Merged <" + pr7477 + "|#7477>"},
		{"never mangles a channel name", "posted in #general\n", "posted in #general"},
		{"never mangles a hex color", "the accent is #1a2b3c\n", "the accent is #1a2b3c"},
		{"never mangles an issue url fragment", "https://github.com/ChessCom/explanation-engine/issues/12#7477\n", "https://github.com/ChessCom/explanation-engine/issues/12#7477"},
		{"keeps a pull link labelled with its number", "Title [#8033](" + pr8033 + ")\n", "Title <" + pr8033 + "|#8033>"},
		{"relabels a pull link behind another label", "Title [the PR title](" + pr8033 + ")\n", "Title <" + pr8033 + "|#8033>"},
		{"links a bare pull url", "Title " + pr8033 + "\n", "Title <" + pr8033 + "|#8033>"},
		{"keeps punctuation trailing a bare pull url", "Title (" + pr8033 + ").\n", "Title (<" + pr8033 + "|#8033>)."},
		{"keeps a pull url's sub-path", "Title " + pr8033 + "/files\n", "Title <" + pr8033 + "/files|#8033>"},
		{"links another repository's pull url", "see https://github.com/other/repo/pull/5\n", "see <https://github.com/other/repo/pull/5|#5>"},
		{"leaves a non-github pull path alone", "[x](https://example.com/pull/5)\n", "<https://example.com/pull/5|x>"},
		{"leaves an authored number label on a non-pull url", "[#12](https://example.com/x)\n", "<https://example.com/x|#12>"},
		{"allows a pull reference inside a fence", "```\nPR 8033 `#8033`\n```\n", "```\nPR 8033 `#8033`\n```"},
		{"never linkifies inside a fence", "```\ngit show #7477\n```\n", "```\ngit show #7477\n```"},
		{"carries nesting only as leading spaces", nestedBullets, "• the top-level item\n  • the item nested under it"},
		{"leaves a reference inside a bracketed label alone", "[[Game Review] #123](https://n/1)", "<https://n/1|[Game Review] #123>"},
		{"renders a heading bold", "## Shipped\n", "*Shipped*"},
		{"renders a divider", "---\n", "──"},
		{"renders bold and bold italic", "**a** and ***b***\n", "*a* and _*b*_"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := mustMrkdwn(t, ee, tc.md)

			// Assert.
			if got != tc.want {
				t.Fatalf("RenderMrkdwn(%q) = %q, want %q", tc.md, got, tc.want)
			}
		})
	}
}

func TestRenderMrkdwnMovesAnIndentedFenceToColumnZero(t *testing.T) {
	// Act.
	got := mustMrkdwn(t, ee, bulletWithFence)

	// Assert: Slack opens a code block only on an unindented fence.
	var fences []string
	for line := range strings.SplitSeq(got, "\n") {
		if strings.Contains(line, "```") {
			fences = append(fences, line)
		}
	}
	if !reflect.DeepEqual(fences, []string{"```", "```"}) {
		t.Fatalf("fence lines = %q, want two unindented fences", fences)
	}
}

func TestRenderMrkdwnRefusesAListWhenListsAreNotAllowed(t *testing.T) {
	// Act.
	_, err := ee.RenderMrkdwn("ping <@U012ABCDEF>\n- item\n", false)

	// Assert.
	var refused *NotSlackNativeError
	if !errors.As(err, &refused) || !strings.Contains(err.Error(), "send the list separately") {
		t.Fatalf("RenderMrkdwn() = %v, want the list refused", err)
	}
}

func TestRenderMrkdwnWithoutListsStillRendersProse(t *testing.T) {
	// Act.
	got, err := ee.RenderMrkdwn("ping <@U012ABCDEF> now\n", false)

	// Assert.
	if err != nil || got != "ping <@U012ABCDEF> now" {
		t.Fatalf("RenderMrkdwn() = (%q, %v), want the prose", got, err)
	}
}

func TestRenderBlocksFences(t *testing.T) {
	tests := []struct {
		name string
		md   string
		want []string
	}{
		{"renders a fence as preformatted", bulletWithFence, []string{`sum by (version) (rate(m{env="staging",error_type="x"}[30m])) * 60`}},
		{"keeps a multi-line fence one block", "```\nline one\nline two\n```\n", []string{"line one\nline two"}},
		{"drops a fence's language tag", "```promql\nup\n```\n", []string{"up"}},
		{"runs an unclosed fence to the end", "```\ntrailing code\n", []string{"trailing code"}},
		{"never linkifies inside a fence", "```\n#7477\n```\n", []string{"#7477"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := preformattedBodies(mustBlocks(t, ee, tc.md))

			// Assert.
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("preformatted bodies = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestRenderBlocksFenceClosesTheEnclosingList(t *testing.T) {
	// Act.
	blocks := mustBlocks(t, ee, bulletWithFence)

	// Assert.
	var kinds []string
	for _, block := range blocks {
		for _, el := range elementsOf(block) {
			kinds = append(kinds, el["type"].(string))
		}
	}
	want := []string{"rich_text_list", "rich_text_preformatted", "rich_text_list"}
	if !reflect.DeepEqual(kinds, want) {
		t.Fatalf("element kinds = %q, want %q", kinds, want)
	}
}

func TestRenderBlocksGivesANestedBulletItsOwnListLevel(t *testing.T) {
	// Act.
	blocks := mustBlocks(t, ee, nestedBullets)

	// Assert.
	var indents []int
	for _, block := range blocks {
		for _, el := range elementsOf(block) {
			if el["type"] == "rich_text_list" {
				indents = append(indents, el["indent"].(int))
			}
		}
	}
	if !reflect.DeepEqual(indents, []int{0, 1}) {
		t.Fatalf("list indents = %v, want [0 1]", indents)
	}
}

func TestRenderBlocksLinkifiesABareReferenceInABullet(t *testing.T) {
	// Act.
	blocks := mustBlocks(t, ee, "- landed #7477\n")

	// Assert.
	var links []map[string]any
	for _, block := range blocks {
		for _, list := range elementsOf(block) {
			for _, section := range elementsOf(list) {
				for _, el := range section["elements"].([]map[string]any) {
					if el["type"] == "link" {
						links = append(links, el)
					}
				}
			}
		}
	}
	want := []map[string]any{{"type": "link", "url": pr7477, "text": "#7477"}}
	if !reflect.DeepEqual(links, want) {
		t.Fatalf("links = %v, want %v", links, want)
	}
}

func TestRenderBlocksRendersEveryNodeKind(t *testing.T) {
	// Act.
	blocks := mustBlocks(t, ee, "## Title\n\nprose **bold**\n\n---\n")

	// Assert.
	want := []map[string]any{
		{"type": "header", "text": map[string]any{"type": "plain_text", "text": "Title", "emoji": true}},
		{"type": "section", "text": map[string]any{"type": "mrkdwn", "text": "prose *bold*"}},
		{"type": "divider"},
	}
	if !reflect.DeepEqual(blocks, want) {
		t.Fatalf("RenderBlocks() = %v, want %v", blocks, want)
	}
}

func TestToMrkdwnInline(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{"plain label", "[Open Rank](https://n/1)", "<https://n/1|Open Rank>"},
		{"label opening with brackets", "[[Game Review] Takeaway Positions](https://n/1)", "<https://n/1|[Game Review] Takeaway Positions>"},
		{"label with brackets inside", "[Categories [for] All Moves](https://n/1)", "<https://n/1|Categories [for] All Moves>"},
		{"two bracketed links on one line", "[[A] one](https://n/1) and [[B] two](https://n/2)", "<https://n/1|[A] one> and <https://n/2|[B] two>"},
		{"bracketed runs that are not links", "see [1] and [2] for context", "see [1] and [2] for context"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := toMrkdwnInline(tc.text)

			// Assert.
			if got != tc.want {
				t.Fatalf("toMrkdwnInline(%q) = %q, want %q", tc.text, got, tc.want)
			}
		})
	}
}

func TestParse(t *testing.T) {
	tests := []struct {
		name string
		md   string
		want []node
	}{
		{
			"yields every node kind in order",
			"## Title\n\nprose #7477\n\n- top\n  - nested\n---\n```\ncode\n```\n",
			[]node{
				{kind: nodeHeading, text: "Title"},
				{kind: nodeParagraph, text: "prose [#7477](" + pr7477 + ")"},
				{kind: nodeBullets, bullets: []bullet{{0, "top"}, {1, "nested"}}},
				{kind: nodeDivider},
				{kind: nodeFence, text: "code"},
			},
		},
		{
			"splits bullet runs at a non-bullet line",
			"- a\nbetween\n- b\n",
			[]node{
				{kind: nodeBullets, bullets: []bullet{{0, "a"}}},
				{kind: nodeParagraph, text: "between"},
				{kind: nodeBullets, bullets: []bullet{{0, "b"}}},
			},
		},
		{"skips blank lines", "\n\n   \n", nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := ee.parse(tc.md)

			// Assert.
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("parse(%q) = %+v, want %+v", tc.md, got, tc.want)
			}
		})
	}
}

func TestHandDrawnStructureFlagsALineLedByABulletGlyph(t *testing.T) {
	for _, line := range []string{"• a hand-drawn bullet", "  ◦ a nested one", "▪ square", "‣ triangle", "● disc"} {
		t.Run(line, func(t *testing.T) {
			// Act.
			got := handDrawnStructure("intro\n" + line + "\n")

			// Assert.
			trimmed := strings.TrimSpace(line)
			want := []string{"line 2: hand-drawn bullet \"" + string([]rune(trimmed)[0]) + "\": " + trimmed}
			if !reflect.DeepEqual(got, want) {
				t.Fatalf("handDrawnStructure() = %q, want %q", got, want)
			}
		})
	}
}

func TestHandDrawnStructureFlagsBoxDrawingAnywhereInProse(t *testing.T) {
	for _, line := range []string{"├── child", "│   └── leaf", "1 Root ─ note"} {
		t.Run(line, func(t *testing.T) {
			// Act.
			got := handDrawnStructure(line + "\n")

			// Assert.
			if len(got) != 1 || !strings.HasPrefix(got[0], "line 1: hand-drawn tree glyph") {
				t.Fatalf("handDrawnStructure(%q) = %q, want one tree glyph", line, got)
			}
		})
	}
}

func TestHandDrawnStructureAcceptsNativeListsAndCode(t *testing.T) {
	for _, md := range []string{
		"- a Markdown bullet\n  - nested\n",
		"```\n├── src\n• literal\n```\n",
		"the tree glyph `├──` is quoted as code\n",
		"a mid-line • is prose, not a marker\n",
	} {
		t.Run(md, func(t *testing.T) {
			// Act.
			got := handDrawnStructure(md)

			// Assert.
			if len(got) != 0 {
				t.Fatalf("handDrawnStructure(%q) = %q, want none", md, got)
			}
		})
	}
}

// renderings is every rendering a refusal must hold for.
var renderings = map[string]func(Renderer, string) error{
	"mrkdwn": func(r Renderer, md string) error { _, err := r.RenderMrkdwn(md, true); return err },
	"blocks": func(r Renderer, md string) error { _, err := r.RenderBlocks(md); return err },
}

func TestEveryRenderingRefusesHandDrawnStructure(t *testing.T) {
	tests := []struct {
		name     string
		md       string
		fragment string
	}{
		{"a hand-drawn bullet", "• first\n• second\n", "'- ' Markdown list item"},
		{"a hand-drawn tree", "root\n├── child\n", "line 2: hand-drawn tree glyph"},
	}
	for _, tc := range tests {
		for name, render := range renderings {
			t.Run(tc.name+"/"+name, func(t *testing.T) {
				// Act.
				err := render(ee, tc.md)

				// Assert.
				var refused *NotSlackNativeError
				if !errors.As(err, &refused) || !strings.Contains(err.Error(), tc.fragment) {
					t.Fatalf("render(%q) = %v, want a refusal naming %q", tc.md, err, tc.fragment)
				}
			})
		}
	}
}

func TestEveryRenderingRefusesAnUnlinkablePullReference(t *testing.T) {
	tests := []struct {
		name     string
		r        Renderer
		md       string
		fragment string
	}{
		{"a number in inline code", ee, "see `#8033` here\n", "line 1: a '#<number>' in inline code"},
		{"a bare number in a heading", ee, "ok\n## Landed #8033\n", "line 2: a heading cannot carry a hyperlink"},
		{"a pull url in a heading", ee, "## Landed " + pr8033 + "\n", "line 1: a heading cannot carry a hyperlink"},
		{"a pull request named PR", ee, "merged PR 8033\n", "line 1: a pull request named without its '#<number>'"},
		{"a pull request named in words", ee, "merged pull request 8033\n", "line 1: a pull request named without"},
		{"a repository-qualified reference", ee, "definitions#7477\n", "line 1: a '<repo>#<number>' reference"},
		{"an owner-qualified reference", ee, "ChessCom/definitions#7477\n", "line 1: a '<repo>#<number>' reference"},
		{"a bare number with no repository to link into", Renderer{}, "Merged #7477\n", "line 1: a bare '#<number>' has no repository"},
	}
	for _, tc := range tests {
		for name, render := range renderings {
			t.Run(tc.name+"/"+name, func(t *testing.T) {
				// Act.
				err := render(tc.r, tc.md)

				// Assert.
				var refused *NotSlackNativeError
				if !errors.As(err, &refused) || !strings.Contains(err.Error(), "reach Slack unlinked") || !strings.Contains(err.Error(), tc.fragment) {
					t.Fatalf("render(%q) = %v, want an unlinked-reference refusal naming %q", tc.md, err, tc.fragment)
				}
			})
		}
	}
}

func TestRendererWithoutRepositoryLeavesAnAuthoredNumberLinkAlone(t *testing.T) {
	// Act.
	got := mustMrkdwn(t, Renderer{}, "Merged [#7477]("+pr7477+")\n")

	// Assert.
	if got != "Merged <"+pr7477+"|#7477>" {
		t.Fatalf("RenderMrkdwn() = %q, want the authored link", got)
	}
}

func TestRender(t *testing.T) {
	tests := []struct {
		name       string
		md         string
		wantText   string
		wantBlocks bool
	}{
		{"renders an ordinary body as blocks", "- one\n", "", true},
		{"renders a body that pings someone as mrkdwn", "ping <@U012ABCDEF> now\n", "ping <@U012ABCDEF> now", false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := ee.Render(tc.md)

			// Assert.
			if err != nil || got.Text != tc.wantText || (got.Blocks != nil) != tc.wantBlocks {
				t.Fatalf("Render(%q) = (%+v, %v), want text %q and blocks %v", tc.md, got, err, tc.wantText, tc.wantBlocks)
			}
		})
	}
}

func TestRenderRefusesAMentionBesideAList(t *testing.T) {
	// Act.
	_, err := ee.Render("ping <@U012ABCDEF>\n- item\n")

	// Assert.
	var refused *NotSlackNativeError
	if !errors.As(err, &refused) {
		t.Fatalf("Render() = %v, want the list refused", err)
	}
}

func TestPullNumber(t *testing.T) {
	tests := []struct {
		name   string
		url    string
		want   string
		wantOK bool
	}{
		{"a pull request url", pr8033, "8033", true},
		{"a pull request's files tab", pr8033 + "/files", "8033", true},
		{"an issue url", "https://github.com/a/b/issues/3", "", false},
		{"another host", "https://example.com/pull/5", "", false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, ok := PullNumber(tc.url)

			// Assert.
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("PullNumber(%q) = (%q, %v), want (%q, %v)", tc.url, got, ok, tc.want, tc.wantOK)
			}
		})
	}
}
