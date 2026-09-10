//go:build playtest

package e2e

import (
	"fmt"
	"testing"
)

// OWNER 15 of PLAYTEST-PLAN.md's partition: F46-F48 -- skills, hooks and
// automation. TABLE-DRIVEN: one loop per family, one world per family, one
// capture of the SETTLED card per row.
//
// Every row here is a fake-SDK scenario the ordinary e2e suite already drives
// against the daemon's Connect API (skills_e2e_test.go, hooks_e2e_test.go,
// remainder_e2e_test.go, mcpmonitors_e2e_test.go). What those suites cannot
// see is the webapp's DRAWING of the card, which is this file's subject. So a
// row's assertion is made INSIDE the page, on the DOM the webapp's own layer
// suite queries (`data-unit`, `data-state`, `data-chip`, `.topbar-warnings`),
// scoped to the turn that was just submitted, and only then is the picture
// taken.
//
// A ROW'S TURN IS FOUND, NEVER COUNTED. The composer's submission goes through
// Emacs, so the page did not mint the TurnId and `data-mine` is never set. The
// last `[data-row-kind="userPrompt"]` row's `data-turn` is the turn just
// submitted, and every card assertion is scoped to it -- so a card left by an
// earlier row of the same family can never satisfy a later row's wait.
//
// THE NEGATIVE ROWS ARE ASSERTED, NOT SKIPPED. Four scenarios draw NO card by
// contract -- a succeeded hook and a cancelled hook (bubbles.go's drawHook:
// "quiet automation stays quiet"), injected memory and injected skills (a
// file-plane fact the vendor never streams), and an artifact LIST (feed.proto:
// "Only a PUBLISH draws"). Each waits for the turn's own terminal row and then
// asserts the specific absence, so the picture a reviewer gets is of a feed
// that correctly drew nothing where nothing belongs.

// p15Row is one scenario of a family: the prompt, the in-page predicate over
// `turn` (the just-submitted turn's id, bound by p15Scoped) that must hold once
// the turn's terminal row has been drawn, and the two manifest sentences.
type p15Row struct {
	name      string
	prompt    string
	predicate string
	asserted  string
	expected  string
}

// p15Scoped wraps a JavaScript predicate so it runs with `turn` bound to the
// last user-prompt row's `data-turn`, and answers false -- with the reason --
// when no such row exists yet.
func p15Scoped(predicate string) string {
	return `(function () {
	          var prompts = document.querySelectorAll('[data-feed-row][data-row-kind="userPrompt"]');
	          var last = prompts[prompts.length - 1];
	          if (!last) { return false; }
	          var turn = last.getAttribute("data-turn");
	          if (!turn) { return false; }
	          var of = function (selector) { return document.querySelector('[data-turn="' + turn + '"]' + selector); };
	          var ended = of('[data-row-kind="turnEnded"]');
	          if (!ended) { return false; }
	          return (` + predicate + `);
	        })()`
}

// p15RunFamily drives one family's rows through one world, in order: submit
// with composer RET, wait for the roster arm to settle, wait for the in-page
// predicate (which itself requires the turn's terminal row), then capture.
func p15RunFamily(t *testing.T, s *playtestScenario, rows []p15Row) {
	t.Helper()
	for _, row := range rows {
		s.submit(t, row.prompt)
		arm := s.awaitArm(t, s.Name, "the turn for "+row.prompt+" to settle", emGHISettledArms...)
		s.awaitInPage(t, row.prompt+": "+row.asserted, p15Scoped(row.predicate))
		// THE PICTURE IS OF THE ROW THIS STEP IS ABOUT. Every family here
		// submits row after row into ONE feed, so from the first row whose
		// content overflows the viewport onward, the card a step just asserted
		// is in the picture only if the feed followed its tail. Run 1 caught
		// exactly that: `03-findings` was pixel-identical to `02-plan` -- the
		// findings card was in the DOM, the predicate passed, and the screen
		// still showed the previous row. So the tail being ON SCREEN and clear
		// of the progress footer is asserted before every capture, which makes
		// a stale picture a red step rather than a review of the wrong card.
		s.awaitTailClearsFooter(t)
		s.Book.capture(row.name, "`"+row.prompt+"` submitted with composer RET, and the turn settled on "+arm,
			row.asserted+" (asserted inside the page, scoped to this turn's own `data-turn`, after its `turnEnded` row was drawn)",
			row.expected)
	}
}

// p15NoRow answers a predicate that the just-submitted turn drew NO row of the
// given unit -- the specific negative, stated on the turn's own rows.
func p15NoRow(unit string) string {
	return fmt.Sprintf(`of('[data-unit=%q]') === null`, unit)
}

// TestPlaytestSkills is plan F.46: the skill card in its loaded and failed
// arms, and the two injected-context scenarios that draw nothing.
func TestPlaytestSkills(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "15-skills",
		"Plan F.46. The teal skill card loaded and failed, then injected memory and injected skills, "+
			"which are file-plane facts and draw no card at all.")
	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	p15RunFamily(t, s, []p15Row{
		{
			name:   "skill-loaded",
			prompt: "!skill",
			predicate: `of('[data-unit="skill"][data-state="loaded"]') !== null &&
			            of('[data-unit="skill"] .skill-allowances') !== null &&
			            of('[data-unit="skill"] .tool-name').textContent.indexOf("fake-skill") >= 0`,
			asserted: "the turn's skill row is in the `loaded` arm, carries an allowances line, and its invocation line names `fake-skill`",
			expected: "Beneath the `!skill` prompt bubble, a TEAL skill card whose head is the invocation line naming " +
				"`fake-skill` with a green `loaded` badge, a folded `document` section under it, and an " +
				"`allows:` line naming the two allowed tools. The turn's response bubble follows it.",
		},
		{
			name:   "skill-failed",
			prompt: "!skill-fail",
			predicate: `of('[data-unit="skill"][data-state="failed"]') !== null &&
			            of('[data-unit="skill"] .skill-failed').textContent === "Error: no such skill: absent-skill"`,
			asserted: "the turn's skill row is in the `failed` arm and its composed reason reads exactly `Error: no such skill: absent-skill`",
			expected: "A second skill card, below the first, whose head names `absent-skill` with a red `failed` " +
				"badge and, under it, the reason line `Error: no such skill: absent-skill` verbatim. No document " +
				"section and no `allows:` line: a skill that did not resolve brought nothing.",
		},
		{
			name:      "memory-injected-draws-nothing",
			prompt:    "!memory",
			predicate: p15NoRow("skill") + ` && of('[data-unit="response"][data-state="success"]') !== null`,
			asserted:  "the turn drew a settled response and NO skill row: injected memory is a file-plane fact",
			expected: "The `!memory` prompt bubble followed directly by a plain response bubble. NO card of any " +
				"kind between them: the injected memory is written to the transcript and never streamed.",
		},
		{
			name:      "skills-injected-draws-nothing",
			prompt:    "!skills-injected",
			predicate: p15NoRow("skill") + ` && of('[data-unit="response"][data-state="success"]') !== null`,
			asserted:  "the turn drew a settled response and NO skill row: injected skills are a file-plane fact",
			expected: "The `!skills-injected` prompt bubble followed directly by a plain response bubble, with no " +
				"skill card between them. The two skill cards from the first two rows are still above.",
		},
	})
}

// TestPlaytestHooks is plan F.47: a hook card only where the contract draws
// one -- the blocked and the failed arms -- and nothing for a hook that
// succeeded or was cancelled.
func TestPlaytestHooks(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "15-hooks",
		"Plan F.47. A succeeded hook draws nothing, a blocked hook draws the loud refusal card, a failed hook "+
			"draws the card with its exit chip and output, and a cancelled hook draws nothing.")
	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	p15RunFamily(t, s, []p15Row{
		{
			name:      "hook-success-draws-nothing",
			prompt:    "!hook-success",
			predicate: p15NoRow("hook") + ` && of('[data-unit="response"][data-state="success"]') !== null`,
			asserted:  "the turn drew a settled response and NO hook row: a succeeded hook stays quiet",
			expected: "The `!hook-success` prompt bubble, the gated Read's own tool card, and the response " +
				"bubble. NO hook card anywhere: the hook let the read through and quiet automation stays quiet.",
		},
		{
			name:   "hook-blocked",
			prompt: "!hook-blocked",
			predicate: `of('[data-unit="hook"][data-state="blocked"] .tool-hook-blocked') !== null &&
			            of('[data-unit="hook"]').textContent.indexOf("the suite failed after the edit") >= 0`,
			asserted: "the turn's hook row is in the `blocked` arm, wears the loud `.tool-hook-blocked` treatment, and carries the reason `the suite failed after the edit`",
			expected: "A LOUD hook card -- visibly different from the ordinary grey tool cards -- whose headline " +
				"says the PostToolUse:Edit hook blocked, with a `gated:` link to the Edit call's own card and the " +
				"reason `the suite failed after the edit` beneath. The turn still concluded: the response bubble follows.",
		},
		{
			name:   "hook-failed",
			prompt: "!hook-failed",
			predicate: `of('[data-unit="hook"][data-state="failed"] .hook-exit[data-exit-code="1"]') !== null &&
			            of('[data-unit="hook"]').textContent.indexOf("Failed to run: no interpreter on PATH.") >= 0`,
			asserted: "the turn's hook row is in the `failed` arm with an `exit 1` chip beside the headline and the output `Failed to run: no interpreter on PATH.`",
			expected: "An ORDINARY-toned hook card (not the loud blocked treatment) for the SessionStart hook, " +
				"an `exit 1` chip in red beside its headline, and the output line `Failed to run: no interpreter " +
				"on PATH.` below the divider. No `gated:` link: a startup hook gates no call.",
		},
		{
			name:      "hook-cancelled-draws-nothing",
			prompt:    "!hook-cancelled",
			predicate: p15NoRow("hook") + ` && of('[data-unit="response"][data-state="success"]') !== null`,
			asserted:  "the turn drew a settled response and NO hook row: a cancelled hook draws nothing",
			expected: "The `!hook-cancelled` prompt bubble, the Edit call's own tool card, and the response " +
				"bubble, with NO hook card for this turn. The blocked and failed cards from the earlier rows " +
				"are still above and unchanged.",
		},
	})
}

// TestPlaytestAutomation is plan F.48: the plan and findings cards, the
// worktree dividers, the footer's live-work chips for monitors and the pending
// wakeup, the artifact card, and the topbar's unmodeled-tool warning.
func TestPlaytestAutomation(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "15-automation",
		"Plan F.48. Plan mode, findings, worktree enter/exit, cron, monitors, a scheduled wakeup and its stop "+
			"(the footer chip present then retired), artifact publish/list, and an unmodeled MCP tool.")
	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	p15RunFamily(t, s, []p15Row{
		{
			name:   "plan",
			prompt: "!plan",
			predicate: `of('[data-unit="plan"][data-state="planned"] .plan-planned .plan-prose') !== null &&
			            of('[data-unit="plan"] .plan-edit') !== null &&
			            document.querySelectorAll('[data-feed-row][data-unit="plan"]').length === 1`,
			asserted: "the turn's plan row is in the `planned` arm, carries the rendered plan prose and an edit affordance, and is the feed's ONLY plan row -- feed.proto: the enter and the exit coalesce onto ONE FeedId",
			expected: "ONE plan card (the enter and exit calls coalesce onto it) with a green `plan` badge, the " +
				"three-step plan rendered as a markdown list, and an edit link to the plan file under it. " +
				"The prose written under plan mode and the response bubble sit around it.",
		},
		{
			name:   "findings",
			prompt: "!findings",
			predicate: `of('[data-unit="findings"]') !== null &&
			            of('[data-unit="findings"]').querySelectorAll('[data-finding]').length === 3 &&
			            of('[data-unit="findings"] [data-finding][data-verdict="confirmed"][data-outcome="fixed"]') !== null &&
			            of('[data-unit="findings"] [data-finding][data-verdict="plausible"][data-outcome="skipped"]') !== null`,
			asserted: "the turn's findings row carries exactly three findings, the first CONFIRMED and fixed, the second PLAUSIBLE and skipped",
			expected: "A findings card listing THREE findings: `orphan tool_result dropped` with a red `confirmed` " +
				"badge and a green `fixed` badge, the write-id collision with an amber `plausible` badge and a " +
				"muted `skipped` badge, and the stderr-mirror finding with NO verdict badge and a muted `no change` " +
				"badge. Each names its file and category.",
		},
		{
			name:   "worktree-keep",
			prompt: "!worktree-keep",
			predicate: `document.querySelector('[data-row-kind="separation"][data-state="worktreeEntered"] .sep-worktree') !== null &&
			            document.querySelector('[data-row-kind="separation"][data-state="worktreeLeft"] .sep-worktree[data-left="kept"]') !== null`,
			asserted: "the feed carries a `worktreeEntered` divider and a `worktreeLeft` divider whose outcome is `kept`",
			expected: "TWO horizontal dividers in the worktree accent: the first labelled as entering " +
				"`/w/worktrees/experiment` on `offline/experiment`, the second as leaving it with the path " +
				"still shown as a jump target -- the tree was KEPT. No token figures beside either: a worktree " +
				"move cuts no context.",
		},
		{
			name:      "worktree-remove",
			prompt:    "!worktree-remove",
			predicate: `document.querySelector('[data-row-kind="separation"][data-state="worktreeLeft"] .sep-worktree[data-left="removed"] .sep-discarded') !== null`,
			asserted:  "the feed carries a `worktreeLeft` divider whose outcome is `removed`, with the discard line drawn",
			expected: "Two more worktree dividers: entering `/w/worktrees/throwaway`, then leaving it with a LOUD " +
				"discard line stating the 3 files and 1 commit thrown away, and no path link -- there is nowhere " +
				"left to go.",
		},
		{
			name:      "cron",
			prompt:    "!cron",
			predicate: p15NoRow("simpleToolCall") + ` && of('[data-unit="response"][data-state="success"]') !== null`,
			asserted:  "the turn drew a settled response and NO tool card: cron acts reach the footer's chip only, and the job was deleted inside the same turn",
			expected: "The `!cron` prompt bubble followed directly by the response bubble `Created, listed and " +
				"deleted a cron job.` No card for any of the three acts, and NO ◷ chip in the footer: the job " +
				"was created and deleted within the turn, so nothing is scheduled once it settled.",
		},
		{
			name:   "monitor-deadline",
			prompt: "!monitor-deadline",
			predicate: `document.querySelector('.footer-chip[data-chip="monitors"]') !== null &&
			            parseInt(document.querySelector('.footer-chip[data-chip="monitors"]').textContent.replace(/[^0-9]/g, ""), 10) >= 1`,
			asserted: "the footer's ◉ monitors chip is drawn with a count of at least one, the deadline monitor still being live after the turn",
			expected: "The footer's right-hand chips now carry a ◉ MONITORS chip with a count of 1. The feed shows " +
				"the prompt bubble and the response `Monitoring until the deadline.` and no monitor card, " +
				"since a monitor lives in the footer rather than the feed.",
		},
		{
			name:   "monitor-persistent",
			prompt: "!monitor-persistent",
			predicate: `document.querySelector('.footer-chip[data-chip="monitors"]') !== null &&
			            parseInt(document.querySelector('.footer-chip[data-chip="monitors"]').textContent.replace(/[^0-9]/g, ""), 10) >= 1`,
			asserted: "the footer's ◉ monitors chip is still drawn with a count of at least one after the persistent monitor was started",
			expected: "The ◉ monitors chip is still in the footer, its count now the number of live monitors " +
				"(2 if the deadline monitor from the previous row is still standing). The response reads " +
				"`Monitoring until something stops it.`",
		},
		{
			name:   "wakeup-scheduled",
			prompt: "!wakeup-schedule",
			predicate: `document.querySelector('.footer-substatus[data-arm="wakeup"]') !== null &&
			            document.querySelector('.footer-chip[data-chip="crons"]') !== null`,
			asserted: "the footer's status carries the `wakeup` sub-status and its ◷ chip counts the pending wakeup",
			expected: "THE FOOTER IS THE SUBJECT: its status cell says the session is waiting on a WAKEUP, with a " +
				"countdown ticking down from about 20 minutes, and the right-hand chips carry a ◷ chip counting " +
				"1 scheduled job. The feed shows only the prompt and the response `Scheduled the wakeup.`",
		},
		{
			name:   "wakeup-stopped",
			prompt: "!wakeup-stop",
			predicate: `document.querySelector('.footer-substatus[data-arm="wakeup"]') === null &&
			            document.querySelector('.footer-chip[data-chip="crons"]') === null`,
			asserted: "the footer's `wakeup` sub-status is gone and the ◷ chip is no longer drawn: the stop retired both",
			expected: "The ◷ chip from the previous picture is GONE from the footer and the status cell no longer " +
				"mentions a wakeup -- it reads idle/done as it did before the schedule. The ◉ monitors chip " +
				"remains. The response reads `Stopped the wakeup loop.`",
		},
		{
			name:   "artifact-published",
			prompt: "!artifact-publish",
			predicate: `of('[data-unit="artifact"][data-state="published"] .artifact-url') !== null &&
			            of('[data-unit="artifact"]').textContent.indexOf("https://claude.ai/code/artifact/00000000-0000-4000-8000-000000000000") >= 0`,
			asserted: "the turn's artifact row is in the `published` arm and draws the artifact's URL",
			expected: "An artifact card whose heading carries the 📊 favicon and the title `Offline Report`, a green " +
				"`published` badge, and the URL `https://claude.ai/code/artifact/00000000-0000-4000-8000-" +
				"000000000000` drawn as a link. The response `Published the artifact.` follows.",
		},
		{
			name:      "artifact-list-draws-nothing",
			prompt:    "!artifact-list",
			predicate: p15NoRow("artifact") + ` && of('[data-unit="response"][data-state="success"]') !== null`,
			asserted:  "the turn drew a settled response and NO artifact row: only a publish draws",
			expected: "The `!artifact-list` prompt bubble followed directly by the response `Listed the artifacts.` " +
				"and no card between them. The published artifact card from the previous row is still above.",
		},
		{
			name:   "unmodeled-tool-warning",
			prompt: "!unmodeled",
			predicate: `document.querySelector('[data-component="topbar"] .topbar-warnings .topbar-warning-count') !== null &&
			            document.querySelector('[data-component="topbar"] .topbar-warnings .topbar-warning-count').textContent === "1" &&
			            ` + p15NoRow("simpleToolCall"),
			asserted: "the topbar draws its ⚠ warning chip with a count of exactly 1 and the turn drew NO tool-call row for the unmodeled tool",
			expected: "THE TOPBAR IS THE SUBJECT: a ⚠ warning chip with the count 1 has appeared in it. The feed " +
				"shows the prompt and the response `The MCP tool echoed.` and NO tool card for `mcp__echo__echo`: " +
				"an unmodeled tool is a warning, never a failure and never a row.",
		},
	})
}
