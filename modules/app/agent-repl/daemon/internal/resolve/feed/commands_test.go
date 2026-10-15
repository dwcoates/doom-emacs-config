package feed

import (
	"context"
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/commandcatalog/catalogtest"
	"agent-repld/internal/dlog"
)

// THE PROMPT'S KNOWN COMMANDS. A user-prompt row names every catalog command
// its text holds in slashed form; the list is stamped on every publication and
// re-published when the session states a catalog.

// commandNames reads a user-prompt row's stamped command names, in order.
func commandNames(row *frontendv1.FeedRow) []string {
	var names []string
	for _, command := range row.GetUserPrompt().GetCommands() {
		names = append(names, command.GetName())
	}
	return names
}

// stateCatalog installs CATALOG in the harness's table and routes it to the
// resolver, in the order the session watcher does.
func (h *harness) stateCatalog(catalog *conversationv1.SessionCommandCatalog) {
	h.t.Helper()
	h.catalogs.OnCommandCatalog(testWorkspace, catalog)
	h.resolver.OnSessionUpdate(testWorkspace, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_CommandCatalog{CommandCatalog: catalog},
	})
}

// userPromptOf is a user-prompt row drawing BLOCKS.
func userPromptOf(blocks ...*frontendv1.FeedUserPromptBlock) *frontendv1.FeedUserPrompt {
	return &frontendv1.FeedUserPrompt{Result: &frontendv1.FeedUserPrompt_Success{Success: &frontendv1.FeedUserPromptSuccess{
		Body: &frontendv1.FeedUserPromptBody{Blocks: blocks},
	}}}
}

// drawnText is one drawn text block.
func drawnText(text string) *frontendv1.FeedUserPromptBlock {
	return &frontendv1.FeedUserPromptBlock{Block: &frontendv1.FeedUserPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Text: text}}}
}

func TestPromptCommandsNamesTheCatalogCommandsTheTextSlashes(t *testing.T) {
	aliased := catalogtest.Named("merge-queue", "compact")
	aliased.Commands[1].Aliases = []string{"squash"}
	cases := []struct {
		name    string
		prompt  *frontendv1.FeedUserPrompt
		catalog *conversationv1.SessionCommandCatalog
		want    []string
	}{
		{
			name:    "a slashed name is named",
			prompt:  userPromptOf(drawnText("/merge-queue own")),
			catalog: catalogtest.Named("merge-queue"),
			want:    []string{"merge-queue"},
		},
		{
			name:    "a slashed alias is named by the alias",
			prompt:  userPromptOf(drawnText("then /squash it")),
			catalog: aliased,
			want:    []string{"squash"},
		},
		{
			name:    "a name the text holds without its slash is not named",
			prompt:  userPromptOf(drawnText("the merge-queue is busy")),
			catalog: catalogtest.Named("merge-queue"),
			want:    nil,
		},
		{
			name:    "a slashed word the catalog does not hold is not named",
			prompt:  userPromptOf(drawnText("look in /usr/local")),
			catalog: catalogtest.Named("merge-queue"),
			want:    nil,
		},
		{
			name:    "names across blocks are sorted without repeats",
			prompt:  userPromptOf(drawnText("/merge-queue then /compact"), drawnText("and /merge-queue again")),
			catalog: catalogtest.Named("merge-queue", "compact"),
			want:    []string{"compact", "merge-queue"},
		},
		{
			name: "a block that is not text names nothing",
			prompt: userPromptOf(&frontendv1.FeedUserPromptBlock{Block: &frontendv1.FeedUserPromptBlock_Quote{
				Quote: &frontendv1.FeedQuoteBlock{Text: "/merge-queue"},
			}}),
			catalog: catalogtest.Named("merge-queue"),
			want:    nil,
		},
		{
			name:    "no catalog names nothing",
			prompt:  userPromptOf(drawnText("/merge-queue own")),
			catalog: nil,
			want:    nil,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			row := &frontendv1.FeedRow{Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: tc.prompt}}

			// Act.
			stampPromptCommands(row, tc.catalog)

			// Assert.
			if got := commandNames(row); !slices.Equal(got, tc.want) {
				t.Fatalf("commands = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestStampPromptCommandsLeavesARowThatIsNotAUserPromptAlone(t *testing.T) {
	// Arrange.
	row := &frontendv1.FeedRow{Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
		Body: &frontendv1.FeedAgentPromptBody{Blocks: []*frontendv1.FeedAgentPromptBlock{{
			Block: &frontendv1.FeedAgentPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Text: "/merge-queue"}},
		}}},
	}}}

	// Act.
	stampPromptCommands(row, catalogtest.Named("merge-queue"))

	// Assert.
	if row.GetUserPrompt() != nil {
		t.Fatal("an agent prompt became a user prompt")
	}
}

func TestADeliveredPromptIsStampedFromTheCatalogInHand(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.stateCatalog(catalogtest.Named("merge-queue"))

	// Act.
	h.deliverPrompt("turn-1", "/merge-queue own")

	// Assert.
	if got := commandNames(h.promptRow("turn-1")); !slices.Equal(got, []string{"merge-queue"}) {
		t.Fatalf("commands = %v, want [merge-queue]", got)
	}
}

// A CATALOG STATED AFTER THE PROMPT IS A PUBLICATION: a reader already
// following the feed is pushed the prompt row, now naming its command.
func TestACatalogStatedAfterThePromptRepublishesItWithItsCommands(t *testing.T) {
	// Arrange: a prompt drawn before any catalog, and a reader following.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "/merge-queue own")
	if got := commandNames(h.promptRow("turn-1")); got != nil {
		t.Fatalf("precondition: commands = %v before any catalog", got)
	}
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tailRows(ctx, tail)

	// Act: the catalog, then a marker row that bounds the read.
	h.stateCatalog(catalogtest.Named("merge-queue"))
	h.deliverPrompt("turn-2", "marker")

	// Assert: the prompt row was pushed again, naming its command.
	for row := range rows {
		switch row.GetId().GetValue() {
		case h.promptRowID("turn-1"):
			if got := commandNames(row); !slices.Equal(got, []string{"merge-queue"}) {
				t.Fatalf("republished commands = %v, want [merge-queue]", got)
			}
			return
		case h.promptRowID("turn-2"):
			t.Fatal("the catalog did not republish the prompt row")
		}
	}
	t.Fatal("the tail closed before the marker row")
}

// A CATALOG RESTATED UNCHANGED PUBLISHES NOTHING: every row upserts to an
// equal snapshot, which is churn.
func TestACatalogRestatedUnchangedRepublishesNoPrompt(t *testing.T) {
	// Arrange: a stamped prompt, and a reader following.
	h := newHarness(t)
	h.stateCatalog(catalogtest.Named("merge-queue"))
	h.deliverPrompt("turn-1", "/merge-queue own")
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tailRows(ctx, tail)

	// Act: the same catalog again, then a marker row.
	h.stateCatalog(catalogtest.Named("merge-queue"))
	h.deliverPrompt("turn-2", "marker")

	// Assert: the marker is the first row pushed.
	for row := range rows {
		if got := row.GetId().GetValue(); got != h.promptRowID("turn-2") {
			t.Fatalf("pushed %s before the marker, want nothing", got)
		}
		return
	}
	t.Fatal("the tail closed before the marker row")
}

func TestARestampIsLoggedWithTheRowsItConsidered(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "/merge-queue own")
	h.deliverPrompt("turn-2", "no command")

	// Act.
	h.stateCatalog(catalogtest.Named("merge-queue", "compact"))

	// Assert.
	records := dlog.FilterRecords(h.log.Records(), dlog.RecordQuery{Operation: "daemon.feed.prompt_commands_restamped"})
	if len(records) != 1 {
		t.Fatalf("restamp records = %d, want 1", len(records))
	}
	if got := records[0].Context["rows"]; got != 2 {
		t.Fatalf("rows = %v, want 2", got)
	}
	if got := records[0].Context["commands"]; got != 2 {
		t.Fatalf("commands = %v, want 2", got)
	}
}
