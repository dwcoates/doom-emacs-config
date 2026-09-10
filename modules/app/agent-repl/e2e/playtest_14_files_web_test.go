//go:build playtest

package e2e

import (
	"fmt"
	"testing"
)

// OWNER 14 of PLAYTEST-PLAN.md's partition: F44-F45 -- the file tools and
// the web tools, one table per family, one capture of the SETTLED card per
// row.
//
// Every row here is a fake-SDK scenario the daemon-level suite already
// drives (filetools_e2e_test.go, webtools_e2e_test.go, remainder's
// TestWebFetch/TestWebSearch), and each row's assertion is the DRAWN
// counterpart of the fact that suite pins on the wire: the card's tool name,
// its settled state and verdict, the output FORM the daemon chose
// (`data-output-form`), and the element the form draws -- the omitted line,
// the diff lines, the diagnostics rows, the link rows. The wire fact is the
// daemon's; the drawn fact is the webapp's; a picture that disagrees with
// the sentence written from both is a defect in the paint.
//
// ONE WORLD PER FAMILY, and the rows run IN ORDER in it, so the feed grows
// by one prompt bubble, one tool card and one response bubble per row. A
// row's assertion therefore addresses the card by its ORDINAL -- the N-th
// tool card in the feed is the N-th row's -- rather than by "the last one",
// which would pass on a row that drew no card at all if the previous row's
// were still standing.

// playtestToolCardSelector is every simple tool call row in the root feed,
// in feed order. `data-unit` is the FeedRow's own unit arm name, mirrored
// onto the row chrome by feed-view.ts.
const playtestToolCardSelector = `[data-feed="root"] [data-feed-row][data-unit="simpleToolCall"]`

// playtestToolRow is one row of a tool-family table.
type playtestToolRow struct {
	// scenario is the fake's scenario name; the prompt submitted is `!` +
	// scenario, which is how the fake's registry selects it.
	scenario string
	// tool is the text the card's head draws as the tool's name.
	tool string
	// form is the output form the daemon must have chosen for this
	// result, which the card states verbatim in `data-output-form`.
	form string
	// extra is a JavaScript predicate over `card` (the settled card's row
	// element) that pins the form's own drawn element -- the omitted line's
	// text, the diff lines, the diagnostics rows. Empty means the form alone
	// is the whole drawn claim.
	extra string
	// asserted names, for the manifest, what extra proved.
	asserted string
	// expected is the sentence the reviewer checks the picture against.
	expected string
}

// playtestToolCardSettled is the predicate for the ORDINAL-th tool card
// being this row's, settled with the succeeded verdict and the stated output
// form, and satisfying the row's own extra predicate.
//
// The verdict is the succeeded one for every row here: a 302 is a served
// answer (webtools_e2e_test.go), a truncated read is a read that came back,
// and diagnostics are a consequence hung off a change that succeeded.
func playtestToolCardSettled(ordinal int, row playtestToolRow) string {
	extra := row.extra
	if extra == "" {
		extra = "true"
	}
	return fmt.Sprintf(`(function () {
              var cards = document.querySelectorAll(%s);
              if (cards.length !== %d) { return false; }
              var card = cards[%d];
              var name = card.querySelector(".tool-name");
              if (!name || name.textContent !== %s) { return false; }
              if (card.getAttribute("data-state") !== "returned") { return false; }
              if (card.querySelector(".tool-card").getAttribute("data-verdict") !== "succeeded") { return false; }
              if (card.querySelector(".tool-card").getAttribute("data-output-form") !== %s) { return false; }
              return (%s);
            })()`,
		jsString(playtestToolCardSelector), ordinal, ordinal-1, jsString(row.tool), jsString(row.form), extra)
}

// runToolFamily drives one family's rows in one world, in order, asserting
// each row's settled card in the page and photographing it.
func runToolFamily(t *testing.T, s *playtestScenario, rows []playtestToolRow) {
	t.Helper()
	for i, row := range rows {
		ordinal := i + 1
		s.submit(t, "!"+row.scenario)
		s.awaitInPage(t, fmt.Sprintf("the %s card for `!%s` to settle in the page with the %s output form", row.tool, row.scenario, row.form),
			playtestToolCardSettled(ordinal, row))
		// THE TURN CONCLUDED, not only the card: the card settles when the
		// tool result lands, and the closing prose and the roster's finish
		// edge come after it. A picture taken between the two would show a
		// tab still painted running under a settled card.
		s.awaitInPage(t, fmt.Sprintf("the closing response bubble of `!%s` to settle", row.scenario),
			fmt.Sprintf(`document.querySelectorAll('[data-feed="root"] [data-feed-row][data-unit="response"][data-state="success"]').length === %d`, ordinal))
		s.awaitArm(t, s.Name, fmt.Sprintf("the turn for `!%s` to settle", row.scenario), emGHISettledArms...)
		s.Book.capture(row.scenario,
			fmt.Sprintf("`!%s` submitted with composer RET and its turn allowed to conclude", row.scenario),
			fmt.Sprintf("tool card %d in the root feed names `%s`, carries `data-state=returned`, `data-verdict=succeeded` and `data-output-form=%s`; %s; the closing response bubble settled and the roster arm is settled",
				ordinal, row.tool, row.form, row.asserted),
			row.expected)
	}
}

// omittedText is a predicate over `card` for the omitted line's exact text.
func omittedText(text string) string {
	return `card.querySelector(".tool-omitted") !== null && card.querySelector(".tool-omitted").textContent === ` + jsString(text)
}

const (
	noOmitted      = `card.querySelector(".tool-omitted") === null`
	noDiagnostics  = `card.querySelector(".tool-diagnostics") === null`
	hasDiagnostics = `card.querySelectorAll(".tool-diagnostic").length > 0`
	noOutputBody   = `card.querySelector("[data-output-body]") === null`
	hasCodeSpans   = `card.querySelectorAll(".tool-read-output .hljs span").length > 0`
	hasTextOutput  = `card.querySelector(".tool-output") !== null && card.querySelector(".tool-output").textContent.trim() !== ""`
)

// diffKinds is a predicate over `card` for the drawn diff's line kinds, in
// order and exhaustively — `[data-diff-line]` carries the ARM's own name, so
// this counts what the card actually painted rather than that it painted
// something. A phantom trailing addition (a file's terminating newline drawn
// as a further, blank added line) is a defect this shape catches and a bare
// "has diff lines" cannot.
func diffKinds(kinds ...string) string {
	list := "["
	for i, kind := range kinds {
		if i > 0 {
			list += ","
		}
		list += jsString(kind)
	}
	list += "]"
	return `(function () {
                var want = ` + list + `;
                var got = Array.prototype.map.call(card.querySelectorAll("[data-diff-line]"),
                  function (l) { return l.getAttribute("data-diff-line"); });
                return got.length === want.length && got.every(function (k, i) { return k === want[i]; });
              })()`
}

// linkRows is a predicate over `card` for the search's drawn result rows: how
// many there are, and which of them are CLICKABLE. A row whose anchor carries
// an empty href is counted as un-clickable here on purpose — a dead link drawn
// as a live one is exactly the defect this row exists to photograph.
func linkRows(total, clickable int) string {
	return fmt.Sprintf(`(function () {
                var rows = card.querySelectorAll(".tool-link-row");
                if (rows.length !== %d) { return false; }
                var live = 0;
                for (var i = 0; i < rows.length; i++) {
                  var a = rows[i].querySelector("a[data-external-link]");
                  if (a && a.getAttribute("href")) { live++; }
                }
                return live === %d;
              })()`, total, clickable)
}

// playtestFileRows is plan F.44: the file tools, one row per fake scenario.
var playtestFileRows = []playtestToolRow{
	{scenario: "read", tool: "Read", form: "code",
		extra:    hasCodeSpans + " && " + noOmitted,
		asserted: "the code output carries paint spans and NO omitted line (a whole read has nothing further to fetch)",
		expected: "A grey `Read` card: its input line is a muted file path, and below the dashed divider the whole file's lines are drawn as highlighted code with NO 'showing N of M lines' footer. The tab is settled and the closing prose bubble sits beneath the card."},
	{scenario: "read-head", tool: "Read", form: "code",
		extra:    hasCodeSpans + " && " + omittedText("showing 2 of 4 lines"),
		asserted: "the omitted line reads exactly 'showing 2 of 4 lines' -- the HEAD extent's own wording, a count of what was shown against the total",
		expected: "A grey `Read` card whose code block holds the file's first two lines, and beneath the block a muted footer reading exactly 'showing 2 of 4 lines'."},
	{scenario: "read-range", tool: "Read", form: "code",
		extra:    hasCodeSpans + " && " + omittedText("lines 2-3 of 4"),
		asserted: "the omitted line reads exactly 'lines 2-3 of 4' -- a RANGE names the window it read, which is a different claim from a head's count and is worded differently on purpose",
		expected: "A grey `Read` card whose code block holds the middle slice of the file (`export const two` and `export const three`), and beneath it a muted footer reading exactly 'lines 2-3 of 4'."},
	{scenario: "read-truncated", tool: "Read", form: "code",
		extra:    hasCodeSpans + " && " + omittedText("showing 2 of 4 lines"),
		asserted: "the omitted line reads exactly 'showing 2 of 4 lines', composed from the head's total_lines",
		expected: "A grey `Read` card whose code block holds two lines and whose footer reads exactly 'showing 2 of 4 lines'."},
	{scenario: "read-image", tool: "Read", form: "none",
		extra:    noOutputBody,
		asserted: "the card draws NO output body at all: an image read carries no extent this wave, so the `none` form is the settled shape",
		expected: "A grey `Read` card for `/w/s/shot.png` with a green ok badge and NOTHING below the head: no divider content, no image, no code. The absence is the contract (AgentReadSuccess has no image extent this wave); an image drawn here would be a defect."},
	{scenario: "write-create", tool: "Write", form: "diff",
		extra:    diffKinds("header", "added") + " && " + noDiagnostics,
		asserted: "the drawn diff is EXACTLY a hunk header and ONE added line: a created one-line file is one addition, and its terminating newline is not a second, blank one",
		expected: "A grey `Write` card whose output is a DIFF of exactly two rows: the hunk header `@@ -1,0 +1,1 @@` and one green `+export const fresh = true;`. There is NO blank green `+` row beneath it. No diagnostics under the card."},
	{scenario: "write-update", tool: "Write", form: "diff",
		extra:    diffKinds("header", "context", "context", "context", "added") + " && " + noDiagnostics,
		asserted: "the drawn diff is EXACTLY a header, the producer's three lines of context and one added line -- a write's patch is the producer's own diff, so it carries the same context on either plane",
		expected: "A grey `Write` card whose output is a DIFF of five rows: the header `@@ -2,3 +2,4 @@`, three unpainted context lines (`export const two = 2;`, `three`, `four`), and one green `+export const five = 5;`. NO red removal: this update only appends. No diagnostics under the card."},
	{scenario: "edit", tool: "Edit", form: "diff",
		extra:    diffKinds("header", "removed", "added") + " && " + noDiagnostics,
		asserted: "the drawn diff is EXACTLY a header, the removed line and the added line -- a replacement of one line, with no context around it because the vendor's patch states none",
		expected: "A grey `Edit` card whose output is a DIFF of three rows: the header, a red `-export const two = 2;` and a green `+export const two = 22;`. No diagnostics under the card."},
	{scenario: "ide-diagnostics", tool: "Edit", form: "diff",
		extra:    diffKinds("header", "context", "removed", "added", "context") + " && " + hasDiagnostics,
		asserted: "the drawn diff is the vendor's own hunk -- header, a context line, the removal, the addition, a context line -- AND at least one diagnostic row hangs off this Edit card",
		expected: "A grey `Edit` card: a five-row DIFF (header, context, red removal, green addition, context), and UNDER it a muted diagnostics row reading the file, line, severity and the typescript message. The diagnostics belong to THIS Edit card, not to a separate card."},
	{scenario: "ide-diagnostics-write", tool: "Write", form: "diff",
		extra:    diffKinds("header", "added") + " && " + hasDiagnostics,
		asserted: "the drawn diff is EXACTLY a header and ONE added line (no phantom blank addition for the terminating newline) AND at least one diagnostic row hangs off this card NAMED Write",
		expected: "A grey `Write` card (NOT an Edit card): a DIFF of the header and one green `+export const created = missing;` with NO blank `+` row after it, and UNDER it a muted diagnostics row naming the typescript error. The diagnostics hang off the Write -- this is the arm a production defect once folded onto Edit."},
	{scenario: "grep-content", tool: "Grep", form: "lines",
		extra:    hasTextOutput + " && " + omittedText("3 more lines not shown"),
		asserted: "the lines output is non-empty and the omitted line reads exactly '3 more lines not shown' -- the EXACT remainder the shim subtracted (5 total, 2 returned)",
		expected: "A grey `Grep` card whose input line is the pattern in accent monospace without a '$', two matched lines below the divider, and a muted footer reading exactly '3 more lines not shown'."},
	{scenario: "grep-files", tool: "Grep", form: "lines",
		extra:    hasTextOutput + " && " + noOmitted,
		asserted: "the lines output is non-empty and NO omitted floor is drawn (every matched file is present)",
		expected: "A grey `Grep` card listing the one matched file path below the divider and NO muted footer of any kind beneath it."},
	{scenario: "grep-count", tool: "Grep", form: "text",
		extra:    hasTextOutput,
		asserted: "the text output is a non-empty composed count",
		expected: "A grey `Grep` card whose output is a single plain summary line stating the match count, and nothing else below the divider."},
	{scenario: "glob", tool: "Glob", form: "lines",
		extra:    hasTextOutput + " && " + omittedText("5 more paths not shown"),
		asserted: "the omitted line reads exactly '5 more paths not shown' -- the EXACT arm, not the 'at least' floor, because the fake reports countIsComplete",
		expected: "A grey `Glob` card whose input line is the pattern `**/*.ts`, two matched paths below the divider, and a muted footer reading exactly '5 more paths not shown' -- NOT 'at least 5 more'."},
}

// playtestWebRows is plan F.45: the web tools, one row per fake scenario.
var playtestWebRows = []playtestToolRow{
	{scenario: "web-fetch", tool: "WebFetch", form: "text",
		extra:    hasTextOutput + ` && card.querySelector("a[data-external-link][href='https://example.com/docs']") !== null`,
		asserted: "the input line is a hyperlink to https://example.com/docs and the text output is non-empty",
		expected: "A grey `WebFetch` card whose input line is the URL drawn as a link, and below the divider the served answer opening with the status line '200 OK' followed by the page summary."},
	{scenario: "web-fetch-redirect", tool: "WebFetch", form: "text",
		extra: hasTextOutput +
			` && card.querySelector(".tool-output").textContent.indexOf("302 Found\n\n") === 0` +
			` && card.querySelector(".tool-output").textContent.indexOf("REDIRECT DETECTED: The URL redirects to a different host.") !== -1` +
			` && card.querySelector("a[data-external-link][href='https://api.example.com/methods']") !== null` +
			` && card.querySelector("a[data-external-link][href*='docs.example.com']") === null`,
		asserted: "the text opens with '302 Found', keeps the vendor's redirect instruction verbatim, and the input link names the asked-for URL (api.example.com), never the redirect destination",
		expected: "A grey `WebFetch` card with a green ok badge (a 302 is a served answer, not a failure): the input link reads api.example.com/methods, and the output opens '302 Found' and then carries the vendor's 'REDIRECT DETECTED' text verbatim."},
	{scenario: "web-search", tool: "WebSearch", form: "links",
		extra: linkRows(3, 2) +
			` && card.querySelector(".tool-link-row a[data-external-link][href='https://docs.example.com/reference/']") !== null` +
			` && card.querySelector(".tool-link-row a[data-external-link][href='https://docs.example.com/changelog/']") !== null`,
		asserted: "the links output draws THREE rows, exactly TWO of them carrying a live href, and those two are the group's own pages (reference, changelog) -- a hit group is flattened into a row per page, never collapsed into one dead row",
		expected: "A grey `WebSearch` card whose input line is the query `example api reference`, and below the divider THREE rows: two blue clickable titles ('Example API reference', 'Example changelog') and then one plain commentary line. There is no blank or invisible row among them."},
}

// TestPlaytestFileTools is plan F.44.
func TestPlaytestFileTools(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "14-files-web/44-files",
		"Plan F.44. Every file-tool scenario in one workspace, in order, and one picture of each "+
			"settled card: the read extents and the truncation line, the image read's empty output, "+
			"the create/update/edit diffs, the diagnostics hanging off the Edit and off the Write, and "+
			"the grep and glob forms.")
	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)
	runToolFamily(t, s, playtestFileRows)
}

// TestPlaytestWebTools is plan F.45.
func TestPlaytestWebTools(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "14-files-web/45-web",
		"Plan F.45. The three web-tool scenarios in one workspace, in order, and one picture of each "+
			"settled card: the fetch, the redirected fetch drawn as a served answer, and the search's link rows.")
	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)
	runToolFamily(t, s, playtestWebRows)
}
