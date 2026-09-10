//go:build playtest

package e2e

import (
	"encoding/json"
	"strings"
	"testing"
)

// OWNER 9 of PLAYTEST-PLAN.md's partition: D25-D28 -- the feed families a
// PROSE turn produces. Markdown with the daemon-wrapped tree in it, a turn
// stopped by the user's own interrupt, the three ways a vendor query dies,
// and the separator a session rotation draws.
//
// ONE TABLE AND ONE LOOP, because the plan says sections D-H are
// table-driven by ruling and must be one loop each. What differs row to row
// is the ACT and what it asserts, so that is the row's function; the loop
// itself only mints the row's world, runs it, and photographs the result.
//
// EVERY ROW GETS ITS OWN WORKSPACE, and that is not tidiness. Three of these
// rows KILL the workspace's vendor session -- the query deaths end it and the
// interrupt bounces it -- so a row sharing a workspace with the one before it
// would be submitting into a session another row had already destroyed. One
// Emacs and one daemon still serve all of them, which is what keeps this a
// single world.

// p09VendorBlockedArm is the arm the roster carries once the vendor's query
// has died under a workspace.
//
// It is the SIDEBAR resolver's own decision, not this file's reading of the
// failure: `daemon/internal/resolve/sidebar/resolver.go` sets `vendorBlocked`
// on a `SessionUpdate_QueryDied`, and that is what the arm is derived from.
// It is deliberately not in `emGHISettledArms`: a query death is not a turn
// that ended.
const p09VendorBlockedArm = ":vendor-blocked"

// p09ReadInPage reads a STRING out of the webview, for the one thing this
// owner has to COMPARE rather than merely wait for: the topbar's session line
// before and after a rotation.
//
// It is `awaitInPageFor`'s shape rather than its body because `pageYes`
// answers one of two words and a comparison needs the value itself. The
// convergence is the same and for the same reason: the probe is
// ASYNCHRONOUS, so each call re-issues the script and answers the previous
// issue's value (see `playtestProbeSetup`), which is why this polls instead
// of reading once.
//
// An empty value is NOT an answer. A missing element, an element still blank,
// and a thrown expression all keep the wait unsatisfied, so a caller that
// returns holds text the page really drew -- and a wait that runs out prints
// the `no: ...` the script produced, which carries the throw.
func p09ReadInPage(t *testing.T, s *playtestScenario, what, expression string) string {
	t.Helper()
	script := `(function () {
                   try {
                     var v = (` + expression + `);
                     return v === null || v === undefined || v === "" ? "" : "ok:" + v;
                   } catch (e) { return "no: " + e; }
                 })()`
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	raw := s.E.AwaitEvalFor(playtestPageBound, what,
		`(agent-repl-playtest--probe `+elispString(s.Name)+` `+elispString(script)+`)`,
		func(raw json.RawMessage) bool { return strings.HasPrefix(decodeString(raw), "ok:") })
	return strings.TrimPrefix(decodeString(raw), "ok:")
}

// p09Row is one feed family: the prompt that produces it, the acts and
// assertions that prove the product drew it, and the sentence a reviewer
// holds the picture to.
//
// `act` takes the row back so the prompt lives in the table and not in a
// closure: the loop writes the manifest's act column from the same field the
// act submits, so the two can never say different things.
type p09Row struct {
	// name is the row's identity: its repository, and the stem of its
	// capture.
	name string
	// prompt is the fake SDK scenario this family comes from.
	prompt string
	// capture is the picture's own name under this playbook's directory.
	capture string
	// act runs the row's user acts and every programmatic assertion the
	// capture rests on. It returns only when they have all passed.
	act func(t *testing.T, s *playtestScenario, ws string, row p09Row)
	// asserted is what the row PROVED, and expected is what the reviewer
	// must see. The first is a fact by the time it is written; the second is
	// the reviewer's judgment and is never asserted.
	asserted string
	expected string
}

// p09ResponseRow is a settled assistant answer in the feed, which is where
// D25's markdown is drawn and D28's warm-up is awaited.
const p09ResponseRow = `[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]`

// p09TerminalRow is the row that says how a turn ENDED. Its outcome, its arm
// and its cause all live on a descendant BODY rather than on the row element
// (`webapp/src/feed/rows/turn-ended.ts`), which is why every selector below
// is two parts.
const p09TerminalRow = `[data-feed-row][data-row-kind="turnEnded"]`

// p09SessionLine is the topbar's session identity line -- which vendor
// session, which account root, which model
// (`daemon/internal/resolve/topbar/resolver.go`'s `sessionLine`).
//
// It is scoped to the REVEAL PANEL it is drawn in rather than left bare,
// because that is where the product puts it: the strip has no session line of
// its own, and the panel is the only thing that ever holds one.
const p09SessionLine = `.topbar-reveal[data-reveal="session"] .topbar-session-line`

// p09EofCauseWords and p09IteratorCauseWords are the sentences the webapp
// draws under a query-death headline, copied from `QUERY_CAUSE_WORDS` in
// `webapp/src/feed/rows/turn-ended.ts`.
//
// They are MANIFEST PROSE and nothing else -- what the row ASSERTS is the
// `data-query-cause` arm, which is the contract. They are spelled here so the
// reviewer is told the product's own words rather than a paraphrase of them:
// the first review of these pictures looked for "unexpected EOF" against a
// page that says "the agent's stream ended without a close", which is a
// mismatch in the sentence and not in the paint.
const (
	p09EofCauseWords      = "the agent's stream ended without a close"
	p09IteratorCauseWords = "the agent sdk's iterator failed"
)

// p09QueryDeathAct is the act the three D27 rows share.
//
// The three differ ONLY in the cause the producer named and in whether an ask
// was open when the query died, so they are one function and three rows
// rather than three functions: a copy per cause would let one of them drift
// into asserting something the other two do not.
func p09QueryDeathAct(cause string, midAsk bool) func(*testing.T, *playtestScenario, string, p09Row) {
	return func(t *testing.T, s *playtestScenario, ws string, row p09Row) {
		s.submit(t, row.prompt)
		s.awaitInPage(t, "the turn to end as an ERRORED terminal row rather than a conclusion",
			`document.querySelector('`+p09TerminalRow+` [data-turn-error="queryDied"]')`)
		// THE CAUSE BY NAME. "Errored" alone would pass on any failure at
		// all; the cause is the half that says WHICH fault the producer
		// reported, and it is the whole reason the arm carries one.
		s.awaitInPage(t, "the terminal row to name the cause the producer reported",
			`document.querySelector('`+p09TerminalRow+` [data-query-cause="`+cause+`"]')`)
		if midAsk {
			// The ask was OPEN when the query died, so the product must
			// resolve it rather than leave a card offering buttons nobody
			// can answer.
			s.awaitInPage(t, "the open permission ask to be drawn answered, denied by the user",
				`document.querySelector('[data-feed-row][data-row-kind="permission"] [data-permission-verdict="deniedByUser"]')`)
		}
		s.awaitArm(t, ws, "the vendor to be blocked after the query died", p09VendorBlockedArm)
		s.awaitInPage(t, "the footer to keep saying what the session's state is",
			`document.querySelector(".footer-status") &&
             document.querySelector(".footer-status").textContent.trim() !== ""`)
	}
}

// p09QueryDeathAsserted and p09QueryDeathExpected are the manifest halves the
// three D27 rows share, with the cause word spelled per row.
func p09QueryDeathAsserted(cause string) string {
	return "the terminal row carries `[data-turn-error=\"queryDied\"]` and `[data-query-cause=\"" + cause +
		"\"]`, the roster arm is " + p09VendorBlockedArm + ", and the footer's status line is non-empty"
}

// p09QueryDeathExpected words the picture from the PRODUCT'S OWN sentences
// rather than from this file's paraphrase of them: the headline is the
// daemon's, drawn verbatim (`drawFeedTurnErrorHeadline`), and CAUSEWORDS is
// the webapp's `QUERY_CAUSE_WORDS` entry for the arm. A manifest that invented
// its own wording would make every future rewording of the product read as a
// picture that does not match its sentence.
func p09QueryDeathExpected(causeWords string) string {
	return "Beneath the prompt bubble the turn's TERMINAL ROW is drawn as an ERRORED end — the error " +
		"hue, not the green of a conclusion. Its headline says THE QUERY DIED and names the fault, and " +
		"on a smaller line directly BENEATH the headline is the cause sentence \"" + causeWords +
		"\". The footer's leftmost cells read BLOCKED and QUERY DIED, and its status line says the " +
		"next prompt restarts the session."
}

// TestPlaytestFeedProseFamilies is plan D25-D28.
func TestPlaytestFeedProseFamilies(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "09-feed-prose",
		"Plan D25-D28. The feed families a prose turn draws: the markdown showcase with the "+
			"daemon-wrapped tree in it, an interrupted terminal row, the three ways a vendor query "+
			"dies, and the separator a session rotation leaves behind.")

	rows := []p09Row{
		{
			name:    "md",
			prompt:  "!md",
			capture: "md-showcase",
			act: func(t *testing.T, s *playtestScenario, ws string, row p09Row) {
				s.submit(t, row.prompt)
				s.awaitInPage(t, "the markdown answer to settle in the feed",
					`document.querySelector('`+p09ResponseRow+`')`)
				// THE CONSTRUCTS, EACH ON ITS OWN. A single "the bubble has
				// text" wait would pass on a bubble carrying the showcase's
				// raw source, which is precisely the failure a markdown
				// renderer has.
				s.awaitInPage(t, "the showcase's heading to be a real heading rather than a literal `#`",
					`document.querySelector('`+p09ResponseRow+` .bubble-body h1')`)
				s.awaitInPage(t, "the Go fence to be drawn as a code block",
					`document.querySelector('`+p09ResponseRow+` .bubble-body pre.md-code code')`)
				s.awaitInPage(t, "the bulleted list to be drawn as a list",
					`document.querySelector('`+p09ResponseRow+` .bubble-body ul li')`)
				// THE BLOCKQUOTE AND THE RULE, which the manifest sentence
				// sends the reviewer looking for. Without these two waits the
				// sentence asks for something no assertion had established,
				// and the `hr` in particular is a HAIRLINE
				// (`.md hr { border-top: 1px solid var(--border) }`) that a
				// reviewer cannot honestly swear to from the picture alone --
				// so the DOM is what proves it is there and the picture is
				// only asked to agree.
				s.awaitInPage(t, "the blockquote to be drawn as a quote rather than a literal `>`",
					`document.querySelector('`+p09ResponseRow+` .bubble-body blockquote')`)
				s.awaitInPage(t, "the thematic break to be drawn as a rule rather than three dashes",
					`document.querySelector('`+p09ResponseRow+` .bubble-body hr')`)
				// THE WRAP ARRIVES AS LINES, which is why a count says
				// something. The daemon wraps the tree to 105 columns before
				// it serves it (`daemon/internal/resolve/feed/tree.go`), so
				// the showcase's two over-long branches reach the page as
				// SIX drawn lines -- root, 1.1, its continuation, 1.1.1, 1.2
				// and its continuation -- and a page drawing five is a page
				// that lost a continuation.
				s.awaitInPage(t, "the daemon-wrapped tree to arrive as six drawn lines",
					`document.querySelectorAll('[data-unit="response"] .mp-tree .mp-line:not(.mp-blank)').length >= 6`)
				// AND THE RAILS SURVIVED THE WRAP. A continuation whose
				// prefix starts with two rails is the load-bearing claim:
				// the wrapped branch's own rail AND the rail of the branch
				// it hangs under, which is what makes `1.1.1` still read as
				// a child of `1.1`.
				s.awaitInPage(t, "a continuation line whose prefix carries the two rails of the branches around it",
					`Array.prototype.some.call(
                       document.querySelectorAll('[data-unit="response"] .mp-line .mp-prefix'),
                       function (p) { return p.textContent.indexOf("│   │") === 0; })`)
				s.awaitArm(t, ws, "the turn to settle", emGHISettledArms...)
			},
			asserted: "the settled response bubble carries an `h1`, a `pre.md-code code`, a `ul li`, a " +
				"`blockquote` and an `hr`; the tree drew at least six non-blank `.mp-line`s, one of them " +
				"a continuation whose `.mp-prefix` begins `│   │`; the roster arm settled",
			expected: "The response bubble shows \"Markdown showcase\" as a LARGE HEADING (not a literal `#`), " +
				"a BULLETED LIST, an ORDERED LIST numbered 1. and 2., a BLOCKQUOTE behind a left bar, a " +
				"monospaced FENCED CODE BLOCK carrying the Go line, and — between that fence and the " +
				"\"A numbered tree\" heading — a faint HAIRLINE RULE across the bubble. Beneath that " +
				"heading is a Unicode tree whose branch 1.1 runs onto a SECOND LINE: that continuation " +
				"line starts with two vertical rails `│   │` aligned EXACTLY under the rails of the " +
				"lines around it, and `└── 1.1.1` hangs beneath it. Branch 1.2's continuation is " +
				"indented under its own text with NO rail, because nothing follows it. Every connector " +
				"must be unbroken: no gap in a vertical rail, and no line sheared back to column 0. " +
				"The bubble is CUT OFF mid-tree with its own scrollbar down the right edge, and that is " +
				"the product: a bubble stops at 25 of its own lines (`--feed-cap-lines`) and scrolls " +
				"past that, so the showcase's closing line is below the cap rather than missing. The " +
				"root line's tree emoji draws as an emoji where the image has an emoji font and as a " +
				"TOFU BOX where it does not; either way that is the image rather than the product.",
		},
		{
			name:    "interrupt",
			prompt:  "!interrupt",
			capture: "interrupted-terminal",
			act: func(t *testing.T, s *playtestScenario, ws string, row p09Row) {
				s.submit(t, row.prompt)
				// The fake parks INSIDE a live Bash tool call until it is
				// interrupted, so "the turn is running" here is a fact and
				// not a race this run happened to win.
				s.awaitArm(t, ws, "the turn to be running before the interrupt", emGHIRunningArms...)

				// THE INTERRUPTING VERB IS A RESTART WITH A PREFIX ARGUMENT.
				// There is no `agent-repl-interrupt` symbol at all -- that is
				// EMACS-LAYER-SPEC.md's contract -- so `SPC o C-c` with `C-u`
				// is the act, and the command takes FORCE as its first
				// documented argument.
				s.E.Eval(`(agent-repl-restart-workspace t ` + elispString(ws) + `)`)
				s.awaitArm(t, ws, "the roster arm to settle interrupted", ":interrupted")

				// THE RESTART BOUNCES THE WEBVIEW, so the page under the
				// probe is a NEW one. Probing before it has mounted would
				// read the old document or none at all, and the failure
				// would look like a product that never draws an interrupted
				// row.
				s.awaitPageMounted(t)
				s.awaitInPage(t, "the turn's end to be drawn as an INTERRUPTION rather than a conclusion",
					`document.querySelector('`+p09TerminalRow+` [data-arm="interrupted"]')`)
			},
			asserted: "the arm ran, `agent-repl-restart-workspace` with FORCE settled it `:interrupted`, and " +
				"the re-mounted page draws a `turnEnded` row whose body carries `[data-arm=\"interrupted\"]`",
			expected: "Beneath the `!interrupt` prompt bubble and a BASH TOOL CALL row, the turn's TERMINAL " +
				"ROW reads \"interrupted\" as the turn's end — neither a green conclusion nor a red " +
				"error. The footer and the workspace's tab are SETTLED, not still running.",
		},
		{
			name:     "query-eof",
			prompt:   "!query-eof",
			capture:  "query-eof-terminal",
			act:      p09QueryDeathAct("unexpectedEof", false),
			asserted: p09QueryDeathAsserted("unexpectedEof"),
			expected: p09QueryDeathExpected(p09EofCauseWords),
		},
		{
			name:     "query-fail",
			prompt:   "!query-fail",
			capture:  "query-fail-terminal",
			act:      p09QueryDeathAct("iteratorFailure", false),
			asserted: p09QueryDeathAsserted("iteratorFailure"),
			expected: p09QueryDeathExpected(p09IteratorCauseWords),
		},
		{
			name:    "query-eof-mid-ask",
			prompt:  "!query-eof-mid-ask",
			capture: "query-eof-mid-ask-terminal",
			act:     p09QueryDeathAct("unexpectedEof", true),
			asserted: p09QueryDeathAsserted("unexpectedEof") + ", and the permission card carries " +
				"`[data-permission-verdict=\"deniedByUser\"]`",
			expected: p09QueryDeathExpected(p09EofCauseWords) + " BENEATH that terminal row, the BASH " +
				"PERMISSION CARD is drawn ANSWERED — \"Claude wants to run Bash\" over the verdict badge " +
				"\"denied by user\" in the error hue, with NO buttons still being offered — and beneath the " +
				"card the Bash tool row carries a \"denied\" badge over its `git status` command line.",
		},
		{
			name:    "rotate",
			prompt:  "!rotate",
			capture: "rotate-separator",
			act: func(t *testing.T, s *playtestScenario, ws string, row p09Row) {
				// THE WARM-UP IS THE ROW'S OWN, and it is required rather
				// than decorative: the topbar draws nothing until the session
				// has something to say about the account and the model, which
				// is after the first turn on a cold workspace. There is no
				// "before" to compare a rotated session line against until
				// one turn has run.
				s.submit(t, "warm the topbar up for the playtest")
				s.awaitInPage(t, "the warm-up answer to settle, which is what makes the topbar draw",
					`document.querySelector('`+p09ResponseRow+`')`)
				s.awaitArm(t, ws, "the warm-up turn to settle", emGHISettledArms...)

				// THE SESSION LINE IS BEHIND A REVEAL, so the reader has to
				// OPEN it -- and that is the product's design rather than
				// this playbook's inconvenience: the strip carries the
				// account chip and the title, and `bindTitleSessionReveal`
				// makes both of them the door onto the session's own identity
				// line (`webapp/src/topbar/strip.ts`). Nothing draws
				// `.topbar-session-line` until that door is opened, which is
				// what the first run of this row found by waiting two seconds
				// for an element that was never going to exist.
				//
				// It is opened ONCE, before the rotate, and left open: the
				// reveal layer survives every topbar push and re-draws itself
				// from the NEW push's view (`reveals.refresh()` in
				// topbar.ts), so leaving it open is also what proves the
				// rotated identity reaches a reveal a reader already had up.
				s.clickInPage(t, "the topbar's session anchor", `[data-reveal-anchor="session"]`)
				before := p09ReadInPage(t, s, "the topbar's session line before the rotate",
					`document.querySelector('`+p09SessionLine+`') &&
                     document.querySelector('`+p09SessionLine+`').textContent.trim()`)

				s.submit(t, row.prompt)
				s.awaitInPage(t, "the rotation to draw its separation row",
					`document.querySelector('[data-feed-row][data-row-kind="separation"] [data-arm="cleared"]')`)
				// A NEW IDENTITY, not merely a redrawn line. The rotation's
				// whole claim is that the session the page now speaks for is
				// a DIFFERENT one, so the line is held to being non-empty
				// AND to having changed.
				s.awaitInPage(t, "the topbar to show a session identity different from the one before the rotate",
					`(function () {
                       var el = document.querySelector('`+p09SessionLine+`');
                       if (!el) { return false; }
                       var now = el.textContent.trim();
                       return now !== "" && now !== `+jsString(before)+`;
                     })()`)
				s.awaitArm(t, ws, "the rotating turn to settle", emGHISettledArms...)
			},
			asserted: "the feed drew a `separation` row whose body carries `[data-arm=\"cleared\"]`; the " +
				"session reveal opened by clicking `[data-reveal-anchor=\"session\"]` is still open and " +
				"its `.topbar-session-line` is non-empty and no longer the text read before the rotate; " +
				"and the arm settled",
			expected: "A FULL-WIDTH SEPARATOR RULE carrying a \"cleared\" label sits between the `!rotate` " +
				"prompt bubble and the response \"Cleared the conversation.\". The earlier warm-up " +
				"bubbles are STILL ABOVE it — a rotation separates the conversation, it does not erase " +
				"the feed. Hanging under the topbar is the SESSION REVEAL, a small panel opened before " +
				"the rotate and still open, carrying one line of the form " +
				"`<vendor session id> · <account root> · <model>`; the session id in it is the NEW one " +
				"the rotation minted, not the one the panel opened with.",
		},
	}

	for _, row := range rows {
		repository := s.repoAt(t, "repo-"+row.name)
		ws := s.register(t, repository.Dir)
		// SELECTED, then re-pointed: `agent-repl-send` submits to the CURRENT
		// workspace, and the playbook's own `Name` is what every in-page
		// probe reads its webview from. Both have to move together or the
		// row would type into one workspace and photograph another.
		emGHISelect(t, s.E, repository.Dir, ws)
		s.Name = ws
		s.openPanel(t)

		row.act(t, s, ws, row)

		// The capture comes last, which is the plan's rule: every assertion
		// above has already passed, so a reviewer is never handed a picture
		// of a world that was already broken.
		s.Book.capture(row.capture, "`"+row.prompt+"` submitted with composer RET into a workspace of its own",
			row.asserted, row.expected)

		// AND THEN THE ROW'S WORKSPACE IS CLOSED, which is a user act this
		// playbook performs deliberately and not a cleanup.
		//
		// WHY, measured in the sandbox run of this playbook: rows 25-27 --
		// five workspaces with five LIVE webviews -- passed and captured,
		// and the SIXTH row's page stalled at boot. Its `AdoptWebWorkspace`
		// reached the daemon roughly a second late, its page stream
		// (`WatchPage`) never reached the daemon at all -- no
		// `daemon.feed.open_page` in that workspace's daemon log and no
		// forwarded webapp log -- and the probe read `rows=0 text=""`.
		// Every page pins exactly one HTTP/1.1 connection for its whole life
		// (the WatchPage mux), all xwidget webviews of one Emacs share ONE
		// WebKit network process, and WebKit caps a host at six connections,
		// so the sixth live page's connections queue forever.
		//
		// That is a PRODUCT DEFECT, and it is FILED for the lead rather than
		// worked around silently: it is a cross-system transport/frontend
		// matter and not this section's. What this playbook owes is not to
		// DEPEND on it. Closing the row's workspace through the ordinary
		// verb is the user act that releases its webview, so at most one
		// page is live when the next row boots.
		if want, got := "agent-repl-close-workspace", s.E.LeaderBinding("j d"); got != want {
			t.Fatalf("SPC j d resolves to %q, want %q", got, want)
		}
		s.E.Eval(`(agent-repl-close-workspace ` + elispString(ws) + `)`)
		s.E.AwaitEvalFor(emacsVerbBound, "the closed workspace's webview buffer to be gone",
			`(null (get-buffer (agent-repl--frontend-webview-buffer-name `+elispString(ws)+`)))`,
			func(raw json.RawMessage) bool { return !isJSONNull(raw) })
		s.E.AwaitEvalFor(emacsVerbBound, "the closed workspace's tab to go away",
			emacsWSTablineNamesForm,
			func(raw json.RawMessage) bool { return !containsString(decodeStrings(raw), ws) })
		s.Book.note("`SPC j d` pressed to close the row's workspace once its picture was taken",
			"the webview buffer is gone and the name left `agent-repl--ws-tabline-names`, so the "+
				"next row's page boots with at most one other live webview")
	}
}
