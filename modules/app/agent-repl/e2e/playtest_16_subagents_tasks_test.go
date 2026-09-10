//go:build playtest

package e2e

import (
	"fmt"
	"strings"
	"testing"
)

// OWNER 16 of PLAYTEST-PLAN.md's partition: G49-G52 -- the subagent bubble and
// its caret, the detached placements, the fan-wide cancel, and the task board
// and send-message deliveries.
//
// WHAT THIS SECTION INHERITED. PLAYTEST-SPEC.md's closing section hands this
// owner one open remainder from `00-feed-tail`: clicking a subagent bubble's
// `[data-expand]` caret did not make the bubble read as open. Half of that was
// the capture's own staleness and is gone with the paint gate; the other half
// was REAL and is fixed at the source in the webapp's stylesheet
// (`.agent-panel[hidden] { display: none }`). `[hidden]` is a USER-AGENT rule
// and `.agent-panel { display: flex }` is an author rule of equal specificity,
// so the author rule won: the caret flipped `data-expanded`, swapped its glyph
// and cancelled the tail while the sub-feed's rows STAYED ON THE GLASS.
//
// jsdom cannot see that -- measured, its `getComputedStyle` answers `none` for
// a hidden element whatever the author sheet says -- so the webapp suite pins
// the SHEET (`webapp/test/feed/bubble.test.ts`) and the shell's own caret
// (`webapp/test/integration/feed-routing.integration.test.ts`), and the
// BROWSER'S OWN ANSWER is asserted here, in the real `xwidget-webkit` webview,
// by `TestPlaytest16SubagentBubble`. That is the one place the cascade this
// defect lived in is actually resolved by WebKit.
//
// EVERY STEP CARRIES A PROGRAMMATIC ASSERTION and a capture is taken only after
// it passed, which is the plan's rule. Where a family has several rows the
// steps are ONE table-driven loop in ONE world, addressing a row by its ORDINAL
// among the family's rows -- "the open one" stops meaning anything the moment a
// second one exists.

// ---------------------------------------------------------------------------
// The page's own hooks, named once.
// ---------------------------------------------------------------------------

const (
	// pt16SyncBubble is a SYNCHRONOUS subagent's bubble row: an activity unit.
	pt16SyncBubble = `[data-feed-row][data-row-kind="activity"][data-unit="subagent"]`
	// pt16DetachedBubble is a DETACHED subagent's bubble row: its own row kind,
	// because a detached spawn outlives the turn that made it (feed.proto's
	// `detached_subagent` wrapper, which carries the SAME drawn head).
	pt16DetachedBubble = `[data-feed-row][data-row-kind="detachedSubagent"]`
	// pt16RootFeed is the root feed's own container. A sub-feed is any other.
	pt16RootFeed = `[data-feed="root"]`
)

// pt16BubbleOpen is the page-side predicate for "this bubble is OPEN, and the
// browser agrees".
//
// FOUR THINGS, and the fourth is the whole point. `data-expanded` on the row,
// `aria-expanded` on the control, the caret's own glyph -- and the sub-feed
// panel's COMPUTED display, which is the only one of the four that says the
// reader can actually see it. The first three were all true while the defect
// stood.
func pt16BubbleOpen(rowSel string) string {
	return pt16FoldPredicate(rowSel, true)
}

// pt16BubbleShut is its negative, and it is not merely "not open": the rows the
// sub-feed drew are STILL IN THE DOM (bubble.ts keeps the last drawn DOM so a
// re-expand is cheap to look at), so a fold that hides nothing leaves a page
// that satisfies every attribute assertion and shows the reader an open bubble.
func pt16BubbleShut(rowSel string) string {
	return pt16FoldPredicate(rowSel, false)
}

func pt16FoldPredicate(rowSel string, open bool) string {
	return `(function () {
                   var row = document.querySelector(` + jsString(rowSel) + `);
                   if (!row) { return false; }
                   var toggle = row.querySelector('[data-expand]');
                   var panel = row.querySelector('[data-subfeed]');
                   if (!toggle || !panel) { return false; }
                   var shown = window.getComputedStyle(panel).display !== "none";
                   return row.getAttribute("data-expanded") === ` + jsString(fmt.Sprint(open)) + `
                          && toggle.getAttribute("aria-expanded") === ` + jsString(fmt.Sprint(open)) + `
                          && toggle.textContent.trim() === ` + jsString(pt16Caret(open)) + `
                          && shown === ` + fmt.Sprint(open) + `;
                 })()`
}

// pt16Caret is the glyph the toggle wears in each state, verbatim from
// `webapp/src/feed/bubble.ts` -- read from the page, never guessed at.
func pt16Caret(open bool) string {
	if open {
		return "▾"
	}
	return "▸"
}

// pt16SubFeedRows counts the rows a bubble's OWN sub-feed is carrying. It is
// the claim `00-feed-tail` makes about the second tail, made here about the
// bubble the reader opened.
func pt16SubFeedRows(rowSel string) string {
	return `document.querySelectorAll(` +
		jsString(rowSel+` [data-feed]:not([data-feed="root"]) [data-feed-row]`) + `).length`
}

// pt16NoRootRowSays is "no TOP-LEVEL row of the root feed carries this text".
//
// IT IS ONLY EVER ASKED AFTER THE POSITIVE. A wait for an absence passes
// vacuously while the thing is still in flight, so every use below first waits
// for the utterance to be drawn INSIDE the bubble and only then asks that the
// root feed never carried it.
func pt16NoRootRowSays(text string) string {
	return `(function () {
                   var root = document.querySelector(` + jsString(pt16RootFeed) + `);
                   if (!root) { return false; }
                   var rows = root.querySelectorAll(':scope > [data-feed-row]');
                   for (var i = 0; i < rows.length; i += 1) {
                     if (rows[i].getAttribute("data-row-kind") === "detachedSubagent") { continue; }
                     if (rows[i].textContent.indexOf(` + jsString(text) + `) >= 0) { return false; }
                   }
                   return true;
                 })()`
}

// ---------------------------------------------------------------------------
// G49 -- `!subagent`: the bubble is drawn once, and its caret folds it.
// ---------------------------------------------------------------------------

// TestPlaytest16SubagentBubble is G49, and it is also the remainder
// PLAYTEST-SPEC.md left this owner: the caret is clicked OPEN and clicked SHUT
// again, and both are asserted the way a reader experiences them -- against
// WebKit's own resolved `display`, not against an attribute that was true the
// whole time the fold was broken.
func TestPlaytest16SubagentBubble(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "16-subagent-bubble",
		"Plan G49. A synchronous subagent is commissioned, its bubble is drawn ONCE in the feed, and its "+
			"caret is clicked open and shut -- with the browser's own computed display asserted at each "+
			"turn, which is the remainder PLAYTEST-SPEC.md hands this owner.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with its panel opened",
		"the webapp mounted its feed host and drew its footer status word off the daemon's own push")

	// THE COMMISSION. `!subagent` runs the Agent tool synchronously: the
	// subagent's own assistant and user messages ride the SAME stream as the
	// main agent's, and the router folds them into this bubble's sub-feed
	// rather than into the top-level feed.
	s.submit(t, "!subagent")
	s.awaitInPage(t, "the subagent bubble to be drawn", `document.querySelector(`+jsString(pt16SyncBubble)+`)`)
	s.awaitArm(t, s.Name, "the turn to settle", emGHISettledArms...)
	s.awaitInPage(t, "the bubble's head to state a settled outcome",
		`document.querySelector(`+jsString(pt16SyncBubble+` .subagent-head[data-state]`)+`)`)

	// EXACTLY ONE BUBBLE, which is G49's own words ("bubble once"). The
	// subagent emits an Agent tool_use, its own nested Read call, its own
	// prose and a completed AgentOutput; every one of those is a chance for a
	// second top-level row to appear for the same commission.
	s.awaitInPage(t, "exactly one subagent bubble to stand for the whole commission",
		`document.querySelectorAll(`+jsString(pt16SyncBubble)+`).length === 1`)
	s.awaitInPage(t, "the bubble to be drawn SHUT, a sync subagent shipping no fold",
		pt16BubbleShut(pt16SyncBubble))
	s.awaitTailClearsFooter(t)
	outcome := s.readInPage(t, "the bubble head's settled arm",
		`document.querySelector(`+jsString(pt16SyncBubble+` .subagent-head`)+`).getAttribute("data-state")`)
	p.capture("subagent-bubble-collapsed",
		"`!subagent` submitted with composer RET and its turn allowed to settle",
		fmt.Sprintf("exactly ONE `[data-unit=\"subagent\"]` row stands for the commission, its head reads "+
			"`data-state=%q`, and the fold reads shut on all four counts -- the row's `data-expanded`, the "+
			"control's `aria-expanded`, the caret glyph `%s`, and WebKit's own computed `display: none` on "+
			"the sub-feed panel", outcome, pt16Caret(false)),
		"The feed carries the `!subagent` PROMPT BUBBLE and, beneath it, ONE subagent bubble drawn as a "+
			"single collapsed head line: a dot, the agent's label, its commission, a token figure, a "+
			"duration and the word \"done\", with a RIGHT-POINTING caret at its left. THERE IS NOTHING "+
			"BENEATH THE HEAD -- no nested rows, no panel, no box. A second subagent bubble, or any nested "+
			"row showing while the caret points right, is the defect this playbook exists for.")

	// THE CARET, OPENED. The click is issued exactly once however many polls
	// its answer takes (`pageClickOnce`), which matters for a TOGGLE above all.
	s.clickInPage(t, "the subagent bubble's caret", pt16SyncBubble+` [data-expand]`)
	s.awaitInPage(t, "the bubble to read OPEN on the row, the control, the glyph and the glass",
		pt16BubbleOpen(pt16SyncBubble))
	s.awaitInPage(t, "the bubble's own sub-feed to carry rows of its own",
		pt16SubFeedRows(pt16SyncBubble)+` > 0`)
	s.awaitTailClearsFooter(t)
	opened := s.readInPage(t, "the sub-feed's row count", pt16SubFeedRows(pt16SyncBubble))
	p.capture("subagent-bubble-expanded",
		"the bubble's `[data-expand]` caret clicked once",
		fmt.Sprintf("the fold reads OPEN on all four counts -- `data-expanded=\"true\"`, "+
			"`aria-expanded=\"true\"`, the caret glyph `%s`, and a computed display that is NOT `none` -- "+
			"and the bubble's own sub-feed carries %s rows", pt16Caret(true), opened),
		"The same subagent bubble is now drawn OPEN: the caret points DOWN and a bordered panel hangs "+
			"beneath the head carrying the subagent's OWN conversation -- its commission as a prompt "+
			"bubble, its reading of the module's conventions, and its report. Those rows are INSIDE the "+
			"panel, indented under the head, never siblings of it in the main feed.")

	// AND SHUT AGAIN, which is the half that was broken. The sub-feed's rows
	// are still in the DOM after this click; what must be true is that the
	// reader cannot see them.
	s.clickInPage(t, "the subagent bubble's caret a second time", pt16SyncBubble+` [data-expand]`)
	s.awaitInPage(t, "the bubble to read SHUT again on all four counts",
		pt16BubbleShut(pt16SyncBubble))
	s.awaitInPage(t, "the sub-feed's rows to still be HELD, a collapse abandoning the tail and not the DOM",
		pt16SubFeedRows(pt16SyncBubble)+` > 0`)
	s.awaitTailClearsFooter(t)
	p.capture("subagent-bubble-refolded",
		"the same caret clicked a second time",
		"the fold reads shut on all four counts again WHILE the sub-feed's rows are still held in the "+
			"DOM -- so `display: none` is the only thing standing between the reader and them, which is "+
			"exactly the guarantee that was missing",
		"The bubble is back to ONE collapsed head line with a right-pointing caret and NOTHING drawn "+
			"beneath it -- the picture must be indistinguishable from `subagent-bubble-collapsed`. THE "+
			"DEFECT'S OWN PICTURE: if the subagent's nested rows are still visible under a "+
			"right-pointing caret, the fold is hiding nothing and this is the failure to file.")
}

// ---------------------------------------------------------------------------
// G50 -- the detached placements: settled, mid-flight, failed, and live.
// ---------------------------------------------------------------------------

// pt16Placement is one row of the detached-placement family.
type pt16Placement struct {
	// name is the capture's own name and the scenario's, minus the bang.
	prompt string
	// state is the `data-state` arm the bubble's head must reach.
	state string
	// stop says whether this bubble must offer the detached stop control --
	// only LIVE detached work outlives its turn, so only it can be stopped
	// from the row.
	stop bool
	// arms is what the roster may read once this row's turn is over. Most
	// placements settle; `!subagent-detached-live` raises a gated call of its
	// OWN after concluding -- which is the whole point of leaving an agent
	// live -- so the arm it lands on is the ask's, not a settled one.
	arms []string
	// asserted and expected are the manifest's two sentences.
	asserted string
	expected string
}

// TestPlaytest16SubagentPlacements is G50.
//
// ONE WORLD, ONE WORKSPACE, FOUR TURNS, in this order deliberately: the two
// scenarios that leave their agent LIVE FOREVER come last, and the one that
// raises a gated call under the subagent comes last of all, because an ask
// standing on the workspace is not a state the next row's submit should have to
// step around.
func TestPlaytest16SubagentPlacements(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "16-subagent-placements",
		"Plan G50. The four detached placements a subagent bubble wears -- settled, failed, mid-flight "+
			"with an utterance that must stay OFF the top level, and live with its own stop -- driven as "+
			"one table against one accumulating feed.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with its panel opened",
		"the webapp mounted its feed host and drew its footer status word off the daemon's own push")

	rows := []pt16Placement{
		{
			prompt: "!subagent-detached",
			state:  "succeeded",
			asserted: "a `detachedSubagent` row was drawn, and its head reached `data-state=\"succeeded\"` " +
				"from the vendor's own `task_notification` -- which lands AFTER the turn concluded, " +
				"which is what detached means",
			expected: "Beneath the `!subagent-detached` prompt bubble, ONE collapsed subagent head with a " +
				"green-ish done dot, the sweep's description, a token figure, a duration and the word " +
				"\"done\". The turn's own conclusion prose (\"Dispatched the agent to the background.\") " +
				"sits in its own assistant bubble; the agent's completion is on the HEAD, not in it.",
		},
		{
			prompt: "!subagent-failed",
			state:  "failed",
			asserted: "a second `detachedSubagent` row was drawn and its head reached `data-state=\"failed\"` " +
				"-- the failing notification's own arm, which is NOT the `lost` arm and must not read " +
				"like one",
			expected: "A THIRD subagent head, beneath the earlier two, with a RED error dot and the word " +
				"\"failed\". It must NOT read \"lost sight of\": losing sight of an agent and an agent " +
				"failing are two different statements and the schema keeps them apart.",
		},
		{
			prompt: "!subagent-detached-live",
			state:  "live",
			stop:   true,
			arms:   append(append([]string{}, emGHISettledArms...), ":permission"),
			asserted: "a `detachedSubagent` row was drawn LIVE and its head offers the detached stop " +
				"control -- the affordance ONLY live detached work carries, a synchronous spawn being " +
				"stopped by stopping the turn instead",
			expected: "The newest subagent head is LIVE -- a breathing dot and a clock counting up -- and " +
				"carries a \"stop\" control at its right-hand end. Every earlier bubble in the feed is " +
				"still drawn above it in the order it arrived.",
		},
	}

	for _, row := range rows {
		name := strings.TrimPrefix(row.prompt, "!")
		// The row this turn drew is the LAST detached bubble in the feed: the
		// family accumulates, so "the newest" is the only unambiguous address.
		last := `document.querySelectorAll('` + pt16DetachedBubble + `')[document.querySelectorAll('` +
			pt16DetachedBubble + `').length - 1]`

		arms := row.arms
		if arms == nil {
			arms = emGHISettledArms
		}
		s.submit(t, row.prompt)
		s.awaitArm(t, s.Name, "the turn to be over", arms...)
		s.awaitInPageFor(t, playtestAskBound, "the newest detached bubble's head to reach "+row.state,
			last+` && `+last+`.querySelector(`+jsString(`.subagent-head[data-state="`+row.state+`"]`)+`) !== null`)

		if row.stop {
			s.awaitInPage(t, "the live detached head to offer its own stop",
				last+`.querySelector('.subagent-head [data-interrupt]') !== null`)
		}

		s.awaitTailClearsFooter(t)
		p.capture(name, fmt.Sprintf("`%s` submitted with composer RET", row.prompt), row.asserted, row.expected)
	}

	// AND THE SECTION'S OWN INVARIANT, once every placement is in: each turn
	// drew EXACTLY ONE bubble, so no placement has quietly doubled.
	s.awaitInPage(t, "one detached bubble per commission and no more",
		fmt.Sprintf(`document.querySelectorAll('%s').length === %d`, pt16DetachedBubble, len(rows)))
	p.note("the four placements all drawn",
		fmt.Sprintf("the root feed carries exactly %d `detachedSubagent` rows -- one per commission, "+
			"none doubled", len(rows)))
}

// ---------------------------------------------------------------------------
// G51 -- `!cancel-all`: the fan-wide stop, and the count it names.
// ---------------------------------------------------------------------------

// pt16FanCount is how many detached items `!cancel-all` leaves live: two
// background agents and one background shell. It is the fake scenario's own
// arrangement (`fake/scenarios/subagents.ts`, CANCEL_ALL) and the number
// `mergequeue_e2e_test.go`'s TestFanWideCancel asserts off the daemon, so the
// note the page draws must name the same one.
const pt16FanCount = 3

// pt16LiveAgents is how many of those three are AGENTS, which is what the ⚙
// chip counts -- the shell is the $ chip's. The two numbers differing is the
// whole reason the stop's own count is worth photographing.
const pt16LiveAgents = 2

// TestPlaytest16FanWideCancel is G51.
func TestPlaytest16FanWideCancel(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "16-fan-wide-cancel",
		"Plan G51. Three detached items are left live in one turn -- two agents and a shell -- and the "+
			"agents panel's fan-wide stop ends all three in ONE call, naming the count it reached.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	s.submit(t, "!cancel-all")
	s.awaitArm(t, s.Name, "the setup turn to settle", emGHISettledArms...)

	// THE CHIPS ARE THE DAEMON'S COUNTS, never the page's tally of its own
	// rows, so what is asserted is the two figures the daemon pushed.
	s.awaitInPageFor(t, playtestAskBound, "the footer's agents chip to count the live agents",
		fmt.Sprintf(`(function () {
                   var chip = document.querySelector('.footer-chip[data-chip="agents"]');
                   return chip !== null && chip.textContent.trim().indexOf("%d") >= 0;
                 })()`, pt16LiveAgents))
	s.awaitInPage(t, "the footer's shells chip to count the live shell",
		`(function () {
                   var chip = document.querySelector('.footer-chip[data-chip="shells"]');
                   return chip !== null && chip.textContent.trim().indexOf("1") >= 0;
                 })()`)

	// THE PANEL. The fan-wide stop lives in the AGENTS panel's header, because
	// the rows beneath it are exactly what the click ends.
	s.clickInPage(t, "the footer's agents chip", `.footer-chip[data-chip="agents"]`)
	s.awaitInPage(t, "the agents panel to open with a row per live agent",
		fmt.Sprintf(`document.querySelectorAll('[data-panel="agents"] [data-row]').length >= %d`, pt16LiveAgents))
	s.awaitInPage(t, "the panel to offer the fan-wide stop",
		`document.querySelector('.footer-stop-all [data-interrupt]') !== null`)
	chips := s.readInPage(t, "the footer's live-work chips",
		`Array.prototype.map.call(document.querySelectorAll('.footer-chip'), function (c) { return c.textContent.trim(); }).join(" ")`)
	p.capture("agents-panel-live",
		"`!cancel-all` submitted with composer RET and the footer's ⚙ chip clicked",
		fmt.Sprintf("the chips read %q -- the agents chip counting %d and the shells chip 1, the daemon's "+
			"own figures -- and the opened agents panel carries a row per live agent above a \"stop all\" "+
			"control", chips, pt16LiveAgents),
		"The progress footer is expanded into its AGENTS panel: a header with a \"stop all\" button and, "+
			"beneath it, TWO rows -- \"Fan item one\" and \"Fan item two\" -- each with a live dot, a "+
			"label, a token figure and a counting clock. The footer strip above still shows the chips, "+
			"with the ⚙ chip reading 2 and the $ chip reading 1.")

	// THE STOP. One click, one Interrupt with the `all_agents` target, and the
	// note it leaves behind names the count -- THREE, not the two the ⚙ chip
	// showed, because the fan-wide target reaches every live DETACHED item and
	// the shell is one of them.
	s.clickInPage(t, "the agents panel's fan-wide stop", `.footer-stop-all [data-interrupt]`)
	s.awaitInPageFor(t, playtestAskBound, "the stop to answer with the count it reached",
		fmt.Sprintf(`(function () {
                   var note = document.querySelector('.footer-stop-note[data-stop-outcome="interruptedDetached"]');
                   return note !== null && note.textContent.trim() === "stopped %d agents";
                 })()`, pt16FanCount))
	s.awaitInPage(t, "no refusal to have been drawn at the control",
		`document.querySelector('.footer-stop-all .refusal') === null`)
	p.capture("fan-wide-cancelled",
		"the agents panel's \"stop all\" clicked once",
		fmt.Sprintf("the control answered `[data-stop-outcome=\"interruptedDetached\"]` reading exactly "+
			"\"stopped %d agents\" with no refusal beside it -- the SAME count "+
			"`mergequeue_e2e_test.go`'s TestFanWideCancel reads off the daemon", pt16FanCount),
		fmt.Sprintf("Beside the \"stop all\" button the footer now reads \"stopped %d agents\". THE COUNT "+
			"MUST BE %d AND NOT %d: the ⚙ chip counted agents only, while the fan-wide stop reaches every "+
			"live detached item, the background shell included. No red refusal text is drawn at the "+
			"control.", pt16FanCount, pt16FanCount, pt16LiveAgents))
}

// ---------------------------------------------------------------------------
// G52 -- the task board, and the three send-message deliveries.
// ---------------------------------------------------------------------------

// pt16TaskRows addresses the footer tasks panel's rows.
const pt16TaskRows = `[data-panel="tasks"] [data-task-status]`

// pt16DeliveryRow addresses the i-th agent-prompt row's delivery marker. A
// send draws on the SENDER's feed as an agent-addressed prompt (feed.proto's
// `agent_prompt`), and the delivery marker is the outcome of the send.
func pt16DeliveryRow(i int) string {
	return fmt.Sprintf(
		`document.querySelectorAll('[data-feed-row][data-row-kind="agentPrompt"]')[%d]`, i)
}

// pt16Delivery is one row of the send-message family.
type pt16Delivery struct {
	prompt string
	// arm is the `data-delivery` arm the marker must carry.
	arm string
	// refused says whether the marker also wears the shared refusal class and
	// carries the producer's own words beside it.
	refused  bool
	asserted string
	expected string
}

// TestPlaytest16TasksAndMessages is G52.
func TestPlaytest16TasksAndMessages(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "16-tasks-and-messages",
		"Plan G52. The task board's three acts -- created, changed, and an update the board REFUSED -- "+
			"read off the footer's own checklist, and then the three send-message deliveries, with the "+
			"refused one drawn apart from the two that landed.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	// -----------------------------------------------------------------------
	// The task board. A TRACKER TASK HAS NO FEED BUBBLE -- feed.proto retired
	// that arm outright ("tracker tasks draw in the FOOTER's checklist only")
	// -- so every assertion here is on the footer's ☑ chip and its panel, and
	// a task row appearing in the feed would be the contract's own violation.
	// -----------------------------------------------------------------------
	s.submit(t, "!task-create")
	s.awaitArm(t, s.Name, "the create turn to settle", emGHISettledArms...)
	s.awaitInPageFor(t, playtestAskBound, "the footer's tasks chip to read none of two done",
		`(function () {
                   var chip = document.querySelector('.footer-chip[data-chip="tasks"]');
                   return chip !== null && chip.textContent.trim().indexOf("0/2") >= 0;
                 })()`)
	s.clickInPage(t, "the footer's tasks chip", `.footer-chip[data-chip="tasks"]`)
	s.awaitInPage(t, "the checklist to carry both tasks, neither begun",
		`document.querySelectorAll('`+pt16TaskRows+`').length === 2 &&
                 document.querySelectorAll('[data-panel="tasks"] [data-task-status="pending"]').length === 2`)
	// EVERY ROW SAYS WHAT IT IS. A checklist of bare glyphs is what this
	// panel drew until the acts stopped erasing a subject they did not name,
	// and it is invisible to a status assertion.
	s.awaitInPage(t, "every checklist row to carry the subject the tracker echoed",
		`Array.prototype.every.call(document.querySelectorAll('`+pt16TaskRows+`'),
                   function (r) { return r.textContent.trim().length > 1; })`)
	// A TRACKER TASK HAS NO FEED ROW. feed.proto retired that arm outright, so
	// the assertion is on the SUBJECT the tracker echoed rather than on an arm
	// name that no longer exists and would pass vacuously.
	s.awaitInPage(t, "no top-level feed row to carry a task's subject",
		pt16NoRootRowSays("Land the converter"))
	p.capture("tasks-created",
		"`!task-create` submitted with composer RET and the footer's ☑ chip clicked",
		"the ☑ chip reads `0/2`, the opened checklist carries TWO rows both `[data-task-status=\"pending\"]`, "+
			"and NO feed row was drawn for either act -- a tracker task lives in the footer only",
		"The progress footer is expanded into its TASKS panel: two checklist rows, \"Land the converter\" "+
			"and \"Land the store writer\", each with an EMPTY checkbox glyph, and the ☑ chip in the strip "+
			"above reading 0/2. The feed shows the `!task-create` prompt and the turn's conclusion prose "+
			"and NOTHING ELSE -- no task card, no task bubble.")

	s.submit(t, "!task-change")
	s.awaitArm(t, s.Name, "the change turn to settle", emGHISettledArms...)
	s.awaitInPageFor(t, playtestAskBound, "the first task to move to running",
		`document.querySelectorAll('`+pt16TaskRows+`')[0].getAttribute("data-task-status") === "running"`)
	s.awaitInPage(t, "the moved task to keep the subject its create established",
		`document.querySelectorAll('`+pt16TaskRows+`')[0].textContent.indexOf("Land the converter") >= 0`)
	p.capture("tasks-changed",
		"`!task-change` submitted with composer RET",
		"the checklist's FIRST row moved to `[data-task-status=\"running\"]` while the second stayed "+
			"pending -- the tracker's own `statusChange` from `pending` to `in_progress`",
		"The first checklist row now wears the RUNNING glyph (a breathing marker, not an empty box) and "+
			"reads in its active form; the second row is unchanged and still empty-boxed. The ☑ chip "+
			"still reads 0/2 -- running is not done.")

	s.submit(t, "!task-reject")
	s.awaitArm(t, s.Name, "the rejected turn to settle", emGHISettledArms...)
	// A REJECTION ADDS NOTHING AND CHANGES NOTHING, which is `AgentTaskRejected`'s
	// own sentence. The refused act names task 9, which the tracker does not
	// hold and nobody has ever named, so the checklist must be exactly what it
	// was -- neither a ticked row for the status the act asked for, nor a
	// phantom row with no words in it.
	s.awaitInPageFor(t, playtestAskBound, "the turn's own prose to say the board refused",
		`document.body.innerText.indexOf("The board rejected the update.") >= 0`)
	s.awaitInPage(t, "the checklist to be exactly the two tasks the board actually holds",
		`document.querySelectorAll('`+pt16TaskRows+`').length === 2 &&
                 document.querySelectorAll('[data-panel="tasks"] [data-task-status="completed"]').length === 0`)
	s.awaitInPage(t, "the tasks chip's denominator to count only those two",
		`document.querySelector('.footer-chip[data-chip="tasks"]').textContent.indexOf("0/2") >= 0`)
	statuses := s.readInPage(t, "the checklist's arms",
		`Array.prototype.map.call(document.querySelectorAll('`+pt16TaskRows+`'), function (r) {
                   return r.getAttribute("data-task-status"); }).join(",")`)
	p.capture("tasks-rejected",
		"`!task-reject` submitted with composer RET -- an update the board refuses",
		fmt.Sprintf("the turn's prose says the board refused, the checklist's arms are still [%s] with "+
			"NOTHING ticked, and the ☑ chip still reads `0/2` -- a rejection adds nothing and changes "+
			"nothing, which is `AgentTaskRejected`'s own sentence", statuses),
		"The feed carries the `!task-reject` prompt and the answer \"The board rejected the update.\", "+
			"and the checklist below is UNCHANGED: the same two rows, the first running and the second "+
			"empty-boxed, and the ☑ chip still reading 0/2. NEITHER a ticked row (the page believing a "+
			"refusal) NOR a third row with no words beside its glyph (a phantom entry for a task the "+
			"tracker says it does not have) may appear.")

	// -----------------------------------------------------------------------
	// The three deliveries. Two landings and a refusal, and the plan's own
	// requirement is that the refusal is DRAWN APART from the two that landed.
	// -----------------------------------------------------------------------
	deliveries := []pt16Delivery{
		{
			prompt: "!send-message",
			arm:    "queuedToLive",
			asserted: "an `agentPrompt` row was drawn on the SENDER's feed carrying " +
				"`[data-delivery=\"queuedToLive\"]` -- the recipient was already running, so the message " +
				"waits for its next tool round",
			expected: "An ORANGE-bordered prompt bubble in the feed, addressed to the recipient agent, with " +
				"the words \"queued for the live recipient\" beneath its body. It is visibly a PROMPT " +
				"bubble like the user's own, not a tool card -- one agent addressing another.",
		},
		{
			prompt: "!send-message-resumed",
			arm:    "resumedRecipient",
			asserted: "a second `agentPrompt` row carries `[data-delivery=\"resumedRecipient\"]` -- the " +
				"discriminator the vendor sets only when it had to resume an idle agent from its " +
				"transcript",
			expected: "A SECOND orange prompt bubble beneath the first, reading \"resumed the recipient\". " +
				"The distinction from the bubble above it matters and must be legible: that one queued a " +
				"message for something already running, this one WOKE something that was not.",
		},
		{
			prompt:  "!send-message-refused",
			arm:     "refused",
			refused: true,
			asserted: "a third `agentPrompt` row carries `[data-delivery=\"refused\"]`, wears the shared " +
				"`.refused` class the two landings do NOT, and carries the producer's own words in their " +
				"own element beside this client's wording",
			expected: "A THIRD orange prompt bubble reading \"refused — never delivered\", drawn APART from " +
				"the two above it -- the refusal marker is visually distinct (the shared refusal " +
				"treatment) rather than looking like a third kind of landing, and the vendor's own " +
				"sentence about the stopped agent is drawn beside it. The past tense matters: nobody " +
				"reading this should wait for a delivery.",
		},
	}

	for i, row := range deliveries {
		sel := pt16DeliveryRow(i)
		s.submit(t, row.prompt)
		s.awaitArm(t, s.Name, "the send turn to settle", emGHISettledArms...)
		s.awaitInPageFor(t, playtestAskBound, "the send's own row to carry its delivery arm",
			sel+` && `+sel+`.querySelector(`+jsString(`.prompt-delivery[data-delivery="`+row.arm+`"]`)+`) !== null`)
		s.awaitInPage(t, "the refusal treatment to be present exactly on the refused delivery",
			`(`+sel+`.querySelector('.prompt-delivery.refused') !== null) === `+fmt.Sprint(row.refused))
		if row.refused {
			s.awaitInPage(t, "the producer's own words to be drawn in their own element",
				sel+`.querySelector('.prompt-delivery .prompt-refusal-reason') !== null && `+
					sel+`.querySelector('.prompt-delivery .prompt-refusal-reason').textContent.trim() !== ""`)
		}
		s.awaitTailClearsFooter(t)
		p.capture(strings.TrimPrefix(row.prompt, "!"),
			fmt.Sprintf("`%s` submitted with composer RET", row.prompt),
			row.asserted, row.expected)
	}
}

// TestPlaytest16SubagentUtterance is G50's mid-flight case, in a world of its
// own.
//
// WHY ITS OWN WORLD RATHER THAN A ROW OF THE TABLE ABOVE. It is the only
// placement whose subject is what the bubble's SUB-FEED carries, so it is the
// only one that has to be photographed OPEN -- and an expansion grows the feed
// below the fold without moving the view (see the report's UI note). A
// single-turn world keeps the whole bubble on screen without asking the
// product to scroll differently.
func TestPlaytest16SubagentUtterance(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "16-subagent-utterance",
		"Plan G50's mid-flight case. A detached subagent left LIVE says something of its own AFTER its "+
			"turn ended, and that line belongs to the agent's own feed -- drawn inside its bubble, and "+
			"never as a bubble of the main conversation.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	const utterance = "Still sweeping; found something interesting."
	s.submit(t, "!subagent-detached-utterance")
	s.awaitArm(t, s.Name, "the turn to settle", emGHISettledArms...)
	s.awaitInPageFor(t, playtestAskBound, "the detached bubble to be drawn LIVE",
		`document.querySelector(`+jsString(pt16DetachedBubble+` .subagent-head[data-state="live"]`)+`)`)

	// THE POSITIVE FIRST. A wait for an ABSENCE passes vacuously while the
	// thing is still in flight, so the utterance is found inside the sub-feed
	// before the root feed is asked whether it ever carried it.
	s.clickInPage(t, "the live bubble's caret", pt16DetachedBubble+` [data-expand]`)
	s.awaitInPage(t, "the bubble to read OPEN on the row, the control, the glyph and the glass",
		pt16BubbleOpen(pt16DetachedBubble))
	s.awaitInPageFor(t, playtestAskBound, "the agent's post-turn utterance to be drawn in its own sub-feed",
		`document.querySelector(`+jsString(pt16DetachedBubble+` [data-feed]:not([data-feed="root"])`)+
			`).textContent.indexOf(`+jsString(utterance)+`) >= 0`)
	s.awaitInPage(t, "no top-level row of the root feed to carry that utterance",
		pt16NoRootRowSays(utterance))
	p.capture("subagent-detached-utterance",
		"`!subagent-detached-utterance` submitted with composer RET and the bubble's caret clicked",
		"the bubble reads open on all four counts, its own sub-feed carries the agent's post-turn line, "+
			"and NO top-level row of the root feed carries it",
		"The newest bubble is drawn OPEN with a live (breathing) dot and a counting clock on its head, "+
			"and beneath the head, INSIDE its panel, the agent's own line \"Still sweeping; found "+
			"something interesting.\". THAT LINE MUST NOT ALSO APPEAR AS A TOP-LEVEL FEED BUBBLE: a live "+
			"subagent's prose belongs to its own feed, and a copy of it sitting in the main conversation "+
			"is the defect this playbook exists to catch.")
}
