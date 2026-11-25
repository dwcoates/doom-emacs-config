package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/commandcatalog/catalogtest"
)

// THE TEXT'S SKILL LINKS. A code span naming one of the workspace's skills
// draws as a link to the skill's file, in a response and a prompt alike, and
// a catalog stated after a row was drawn links that row's names.

const skillFile = "/h/w/.claude/skills/ceac-resource-usage/SKILL.md"

const skillLink = "[/ceac-resource-usage](" + skillFile + ")"

// skillCatalog is a catalog holding the skill `ceac-resource-usage` with its
// file, and the built-in `compact`, which has none.
func skillCatalog() *conversationv1.SessionCommandCatalog {
	catalog := catalogtest.Named("ceac-resource-usage", "compact")
	file := skillFile
	catalog.Commands[0].SkillFile = &file
	return catalog
}

// promptMarkdown answers the drawn markdown of turn TURN's prompt's first
// text block.
func promptMarkdown(h *harness, turn string) string {
	h.t.Helper()
	blocks := h.promptRow(turn).GetUserPrompt().GetSuccess().GetBody().GetBlocks()
	if len(blocks) == 0 {
		h.t.Fatalf("prompt %s drew no block", turn)
	}
	return plainMarkdown(h.t, blocks[0].GetText().GetProse())
}

func TestAResponseLinksTheSkillsItNames(t *testing.T) {
	cases := []struct {
		name     string
		markdown string
		want     string
	}{
		{name: "a known skill is linked", markdown: "ran `ceac-resource-usage`", want: "ran " + skillLink},
		{name: "a slashed known skill is linked", markdown: "ran `/ceac-resource-usage`", want: "ran " + skillLink},
		{name: "a command with no file stays code", markdown: "ran `compact`", want: "ran `compact`"},
		{name: "an unknown name stays code", markdown: "ran `nothing-known`", want: "ran `nothing-known`"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.stateCatalog(skillCatalog())

			// Act.
			parsed := settleResponse(h, tc.markdown)

			// Assert.
			if got := plainMarkdown(t, parsed); got != tc.want {
				t.Fatalf("drawn markdown = %q, want %q", got, tc.want)
			}
			if got := parsed.GetSource().GetMarkdown(); got != tc.markdown {
				t.Fatalf("source = %q, want the text as written", got)
			}
		})
	}
}

func TestAPromptLinksTheSkillsItNames(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.stateCatalog(skillCatalog())

	// Act.
	h.deliverPrompt("turn-1", "use `ceac-resource-usage` here")

	// Assert.
	if got := promptMarkdown(h, "turn-1"); got != "use "+skillLink+" here" {
		t.Fatalf("drawn prompt = %q, want the skill linked", got)
	}
}

func TestNoCatalogLinksNoSkill(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	parsed := settleResponse(h, "ran `ceac-resource-usage`")

	// Assert.
	if got := plainMarkdown(t, parsed); got != "ran `ceac-resource-usage`" {
		t.Fatalf("drawn markdown = %q, want the name left as code", got)
	}
}

func TestAnArrivingResponseLeavesItsTrailingSkillName(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.stateCatalog(skillCatalog())

	// Act: the text so far ends on a code span its next push may reopen.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "`ceac-resource-usage` then `ceac-resource-usage`"}, nil), nil, nil)

	// Assert.
	got := plainMarkdown(t, h.response().GetUpdate().GetProse().GetProse())
	if want := skillLink + " then `ceac-resource-usage`"; got != want {
		t.Fatalf("drawn markdown = %q, want %q", got, want)
	}
}

func TestASkillLinkIsSpelledFromHome(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.deps.Home = "/h"
	h.stateCatalog(skillCatalog())

	// Act.
	parsed := settleResponse(h, "`ceac-resource-usage`")

	// Assert.
	if got, want := plainMarkdown(t, parsed), "[/ceac-resource-usage](~/w/.claude/skills/ceac-resource-usage/SKILL.md)"; got != want {
		t.Fatalf("drawn markdown = %q, want %q", got, want)
	}
}

// A CATALOG STATED AFTER A ROW WAS DRAWN links the row's skill names: the
// replay at daemon start draws before any session states its catalog.
func TestACatalogStatedAfterTheRowsLinksTheirSkills(t *testing.T) {
	// Arrange: a response and a prompt drawn before any catalog.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "use `ceac-resource-usage`")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: "ran `ceac-resource-usage`"},
		}, nil), nil, nil)

	// Act.
	h.stateCatalog(skillCatalog())

	// Assert.
	var response *frontendv1.FeedResponse
	for _, row := range h.rows(rootFeed()) {
		if r := row.GetActivity().GetResponse(); r != nil {
			response = r
		}
	}
	if got := plainMarkdown(t, response.GetSuccess().GetProse().GetProse()); got != "ran "+skillLink {
		t.Fatalf("relinked response = %q, want the skill linked", got)
	}
	if got := promptMarkdown(h, "turn-1"); got != "use "+skillLink {
		t.Fatalf("relinked prompt = %q, want the skill linked", got)
	}
	if !h.hasRecord("debug", "daemon.feed.skills_relinked") {
		t.Fatal("the relink was not recorded")
	}
}

// A CATALOG THAT LINKS NOTHING NEW publishes no row: a row with no skill name
// is not upserted again.
func TestACatalogNamingNoSkillOfARowRepublishesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	settleResponse(h, "ran `compact`")
	before := h.response()

	// Act.
	h.stateCatalog(skillCatalog())

	// Assert.
	if got := plainMarkdown(t, h.response().GetSuccess().GetProse().GetProse()); got != "ran `compact`" {
		t.Fatalf("drawn markdown = %q, want it unchanged", got)
	}
	if h.response() != before {
		t.Fatal("a row naming no skill was published again")
	}
}

func TestEachParsedProseVisitsEveryTextOfARow(t *testing.T) {
	// Arrange: a prompt with two text blocks and an image between them.
	row := &frontendv1.FeedRow{Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: userPromptOf(
		drawnText("one"),
		&frontendv1.FeedUserPromptBlock{Block: &frontendv1.FeedUserPromptBlock_Image{Image: &frontendv1.FeedImageBlock{Src: "x"}}},
		drawnText("two"),
	)}}
	var seen []string

	// Act.
	eachParsedProse(row.ProtoReflect(), func(parsed *frontendv1.ParsedProse) {
		seen = append(seen, parsed.GetSource().GetMarkdown())
	})

	// Assert.
	if len(seen) != 2 || seen[0] != "one" || seen[1] != "two" {
		t.Fatalf("visited %q, want [one two]", seen)
	}
}
