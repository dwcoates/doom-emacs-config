//go:build playtest

package e2e

import (
	"fmt"
	"testing"
	"time"

	"claude-repld/integration/harness"
)

// OWNER 11 of PLAYTEST-PLAN.md's partition: D33-D36 -- the footer's
// allowance line, the account-usage outcome arms, the footer's status text
// under the context bookkeeping scenarios, and the MCP rows with their
// health.
//
// TABLE-DRIVEN, ONE LOOP PER PLAYBOOK, ONE CAPTURE PER ROW, which is the
// plan's rule for sections D-H. Every row is a scenario the fake SDK's
// registry already carries (agent-shim/claude/shim/src/fake/scenarios/
// session.ts), and every row's programmatic assertion is the DRAWN SHAPE the
// Go e2e suite already pins on the wire (sessionfacts_e2e_test.go,
// compaction_e2e_test.go, producerfaults_e2e_test.go, mcpmonitors_e2e_test.go)
// -- read here off the page's own `data-*` hooks through the substrate's
// probe, so a picture is only ever taken of a footer that the page has
// already been proved to have drawn.
//
// THE ALLOWANCE LINE IS A TRANSIENT, AND THE ARRANGEMENT BELOW IS WHY. The
// five-hour and seven-day events draw their figure the instant they land,
// and the shim then REPROBES account usage at the turn's own close
// (engine/session.ts reprobeSessionFacts). With the fake's default
// `available` answer that reprobe files 41%/63% -- both under the footer's
// 0.8 newsworthiness gate -- so the line retires inside the same turn, and
// sessionfacts_e2e_test.go catches it only through a subscription queue no
// webview has. A capture cannot race that. So D33 first switches the
// account-usage answer to `service_unavailable`: an UNREAD sample leaves
// the figures on hand standing (daemon/internal/resolve/footer/resolver.go
// observeAccountUsage) and is itself a reason the line is news (activity.go
// rateLine), so the event's own figure survives the close and the picture
// is of a settled footer. The unread caveat is therefore in the DOM of every
// D33 picture, BESIDE the figures, and the manifest says so.
//
// THE STRIP CUTS THE LINE OFF, AND EVERY MANIFEST SENTENCE BELOW SAYS WHERE.
// The footer dock is capped at the widest response bubble's width
// (styles.css `.pfooter`, `max-width: var(--agent-bubble-cap)`) and its one
// elastic cell ellipsizes rather than wrapping, because the design fixes the
// dock at one line. MEASURED off these captures at the playtest's fixed
// 1280x1024: the cell holds about `session 82% · resets in 59m` and no more,
// so the weekly allowance, the unread caveat and the context-budget
// sentence's tail are all in the DOM -- asserted, every one of them -- and
// none of them are on the glass. That is a product finding this section
// FILES rather than fixes: how the strip should carry a line it cannot fit
// (wrap, hand it to the expansion sheet, shorten it) is a design decision.
// The sentences here state what the picture actually shows, so a reviewer
// judges the paint rather than re-deriving the truncation each time.
//
// `resets in 0m` WAS filed here and is now FIXED at the source. catalogs.ts
// used to fix the sampled windows at absolute instants in 2026-08-29 /
// 2026-09-02, so a countdown drawn off the SAMPLE read `0m` while one drawn
// off a rate-limit EVENT (minted at `now + 3600s`) counted down properly. The
// fixture now states both reset instants as OFFSETS from the fake's own clock
// (five hours and seven days), so a sampled countdown is always in the future
// and stays deterministic under an injected clock. The unread-arm section
// below asserts the drawn countdown is not `0m`, so the rot cannot return.

// fsRow is one row of a footer-and-sidebar table: what the user submits,
// the DRAWN fact the capture waits on, and what the picture must show.
type fsRow struct {
	// name is the capture's file name.
	name string
	// prompts are submitted in order, each awaited to its own settle,
	// before the row's predicate is waited on. D36's rows submit a scenario
	// and then `/mcp`, the command that draws the catalog.
	prompts []string
	// what names the drawn fact the row waits on, for the failure message.
	what string
	// predicate is the JavaScript expression that must hold in the page
	// before the capture is taken.
	predicate string
	// expected is the manifest sentence.
	expected string
}

// fsScenario is a playtestScenario that counts the turns it has driven, so
// each submit can wait for ITS OWN response bubble rather than any settled
// one: `awaitArm` on the settled set is satisfied at once by the arm the
// PREVIOUS turn left behind, which is why 00-feed-tail waits on the bubble
// first and this does the same.
type fsScenario struct {
	*playtestScenario
	turns  int
	panels int
}

func newFsScenario(t *testing.T, name, purpose string) *fsScenario {
	t.Helper()
	return &fsScenario{playtestScenario: newPlaytestScenario(t, name, purpose)}
}

// fsResponseRows is the settled response bubbles on the root feed.
const fsResponseRows = `document.querySelectorAll('[data-feed="root"] [data-feed-row][data-unit="response"][data-state="success"]').length`

// fsCommandPanels counts the /mcp panels drawn on the root feed.
const fsCommandPanels = `document.querySelectorAll('[data-feed="root"] [data-feed-row] [data-panel="mcp"]').length`

// drive submits one prompt with composer RET and waits for the turn it
// minted to settle -- its response bubble drawn on the standing tail and the
// roster arm settled -- or, for `/mcp`, for the panel the daemon answered
// with to be drawn. Nothing is captured here.
func (s *fsScenario) drive(t *testing.T, prompt string) {
	t.Helper()
	if prompt == "/mcp" {
		s.panels++
		s.submit(t, prompt)
		s.awaitInPage(t, fmt.Sprintf("/mcp panel %d to be drawn on the feed", s.panels),
			fmt.Sprintf(`%s === %d`, fsCommandPanels, s.panels))
		return
	}
	s.turns++
	s.submit(t, prompt)
	s.awaitInPage(t, fmt.Sprintf("turn %d's response bubble to settle on the standing tail", s.turns),
		fmt.Sprintf(`%s === %d`, fsResponseRows, s.turns))
	s.awaitArm(t, s.Name, "the turn to settle", emGHISettledArms...)
}

// run drives every row of a table and captures each one, once, after its
// own predicate held.
func (s *fsScenario) run(t *testing.T, rows []fsRow) {
	t.Helper()
	for _, row := range rows {
		for _, prompt := range row.prompts {
			s.drive(t, prompt)
		}
		s.awaitInPage(t, row.what, row.predicate)
		s.Book.capture(row.name,
			fmt.Sprintf("submitted `%s` with composer RET and let it settle", fsJoin(row.prompts)),
			fmt.Sprintf("the turn settled (its response bubble is on the standing tail and the roster arm is settled), and then %s", row.what),
			row.expected)
	}
}

func fsJoin(prompts []string) string {
	out := ""
	for i, p := range prompts {
		if i > 0 {
			out += "`, then `"
		}
		out += p
	}
	return out
}

// fsPercent is the JavaScript for one allowance cell's drawn percentage, or
// "" when that allowance is not drawn.
func fsPercent(allowance string) string {
	return `(function () { var el = document.querySelector('.footer-allowance[data-allowance="` + allowance + `"] [data-datum="percent"]');
                         return el ? el.textContent : ""; })()`
}

// fsCountdown is the JavaScript for one allowance cell's drawn countdown, or
// "" when that allowance is not drawn.
//
// A SAMPLED window's countdown is what caught the fixture rot: catalogs.ts
// used to fix its reset instants absolutely, so this read ` · resets in 0m`
// while a rate-limit EVENT's countdown ran. The fixture now states the reset
// as an offset from the fake's own clock, and fsRunningCountdown pins that.
func fsCountdown(allowance string) string {
	return `(function () { var el = document.querySelector('.footer-allowance[data-allowance="` + allowance + `"] [data-countdown]');
                         return el ? el.textContent : ""; })()`
}

// fsRunningCountdown holds when one allowance's countdown is drawn AND has not
// run out -- the sampled window resets in the future, never at `0m`.
func fsRunningCountdown(allowance string) string {
	return fsCountdown(allowance) + ` !== "" && ` + fsCountdown(allowance) + ` !== " \u00b7 resets in 0m"`
}

// fsAllowanceArm is the JavaScript for one allowance cell's status arm, or
// "" when it carries none.
func fsAllowanceArm(allowance string) string {
	return `(function () { var el = document.querySelector('.footer-allowance[data-allowance="` + allowance + `"]');
                         return el ? (el.getAttribute("data-arm") || "") : ""; })()`
}

// fsUnread is the JavaScript for the unread caveat's drawn sample arm, or ""
// when no caveat is drawn.
const fsUnread = `(function () { var el = document.querySelector('.footer-allowance-unread');
                                return el ? el.getAttribute("data-sample") : ""; })()`

// fsUnreadText is the JavaScript for the caveat's own text.
const fsUnreadText = `(function () { var el = document.querySelector('.footer-allowance-unread');
                                    return el ? el.textContent : ""; })()`

// fsNoRateLine holds when the footer draws no rate-limit line at all.
const fsNoRateLine = `document.querySelector('.footer-activity-rate-limited') === null`

// fsStatusArm is the footer's status arm as drawn.
const fsStatusArm = `document.querySelector('.footer-status').getAttribute("data-arm")`

// fsBudgetText is the drawn context-budget text, or "" when none stands.
const fsBudgetText = `(function () { var el = document.querySelector('.footer-activity-context-budget');
                                    return el ? el.textContent : ""; })()`

// The fake's own figures, as the footer draws them: catalogs.ts
// fakeAccountUsage files five_hour 41 and seven_day 63; the five-hour event
// carries 82 and the seven-day event 91 (session.ts).
const (
	fsStandingSession = "41%"
	fsStandingWeekly  = "63%"
	fsEventSession    = "82%"
	fsEventWeekly     = "91%"
)

// The caveat sentences the strip draws (webapp/src/footer/strip.ts
// ALLOWANCE_UNREAD_SENTENCES), restated here so the manifest tells the
// reviewer the exact words.
const (
	fsUnreadService     = "usage unread — the usage service did not answer"
	fsUnreadWindow      = "usage unread — no five-hour window was reported"
	fsUnreadUtilization = "usage unread — no utilization figure was reported"
	fsUnreadSampling    = "usage unread — the sampling failed"
)

// The context-budget warning the invented scenario carries verbatim
// (session.ts CONTEXT_BUDGET_WARNING; compaction_e2e_test.go pins it).
const fsBudgetWarning = "The conversation is approaching its context window budget."

// fsOverageOperation is the record the footer resolver writes when it drops
// the overage window, which has no allowance cell in the contract
// (resolver.go observeRateLimitStatus). It is the scenario's whole
// observable contract, so it is asserted rather than left in the log.
const fsOverageOperation = "daemon.footer.rate_limit_overage"

// awaitDaemonWarn waits for a warn record by operation in the workspace's
// own daemon log.
//
// The bound is playtestPageBound and not a wider one: the record is written
// when the event lands, which is BEFORE the turn's close this is only ever
// called after, so the wait covers nothing but the log sink's own flush.
func awaitDaemonWarn(t *testing.T, workspaceDir, operation string) harness.LogRecord {
	t.Helper()
	path := harness.WorkspaceLogPath(workspaceDir, "daemon")
	deadline := time.Now().Add(playtestPageBound)
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		for _, r := range harness.ReadLog(t, path) {
			if r.Operation == operation && r.Level == "warn" {
				return r
			}
		}
		if time.Now().After(deadline) {
			t.Fatalf("no warn record with operation %s in %s within %s", operation, path, playtestPageBound)
		}
		<-ticker.C
	}
}

// ---------------------------------------------------------------------------
// D33. Rate limits: the footer's allowance line, and the overage warning.
// ---------------------------------------------------------------------------

func TestPlaytestFooterRateLimits(t *testing.T) {
	t.Parallel()
	s := newFsScenario(t, "11-rate-limits",
		"Plan D.33. The vendor's rate-limit events on the five-hour and seven-day windows, drawn on the "+
			"footer's allowance line, and the overage window the contract has no cell for. THE UNREAD CAVEAT "+
			"IS IN EVERY PICTURE BY ARRANGEMENT: the shim reprobes account usage at each turn's close, and "+
			"with a readable answer that reprobe retires the event's figure inside the same turn, so the "+
			"account-usage answer is switched to `service_unavailable` first. An unread sample leaves the "+
			"figures on hand standing, which is what makes the event's own figure photographable.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	// ARRANGEMENT, not a row: the unread sample that keeps the event figures
	// standing. Its own drawn consequence is asserted so the arrangement is
	// known to have taken, and it gets a note rather than a picture because
	// D34 photographs exactly this state.
	s.drive(t, "!usage-service-unavailable")
	s.awaitInPage(t, "the unread caveat to stand beside the session-start figures",
		fsUnread+` === "serviceUnavailable" && `+fsPercent("session")+` === "`+fsStandingSession+`" && `+
			fsPercent("weekly")+` === "`+fsStandingWeekly+`"`)
	p.note("`!usage-service-unavailable` submitted and settled, so every later turn-close reprobe answers UNREAD",
		fmt.Sprintf("the footer draws `.footer-allowance-unread[data-sample=\"serviceUnavailable\"]` beside the standing %s / %s figures the session-start probe filed",
			fsStandingSession, fsStandingWeekly))

	s.run(t, []fsRow{
		{
			name:    "five-hour-allowance",
			prompts: []string{"!rate-limit-five-hour"},
			what:    "the SESSION allowance to draw the event's 82% under the allowed_warning arm, with the weekly figure and the unread caveat still beside it",
			predicate: fsPercent("session") + ` === "` + fsEventSession + `" && ` + fsAllowanceArm("session") + ` === "allowedWarning" && ` +
				fsPercent("weekly") + ` === "` + fsStandingWeekly + `" && ` + fsUnread + ` === "serviceUnavailable"`,
			expected: "The footer's activity cell (the wide cell between the `done` cell and the empty turn-clock cell) " +
				"reads `session 82% · resets in 59m` -- the figure and the word `resets` in the WARNING tone and bold, " +
				"because 82% is newsworthy and the vendor's verdict was allowed_warning -- and is then CUT OFF at the " +
				"cell's edge with an ellipsis. The weekly figure and `" + fsUnreadService + "` follow it in the DOM (this " +
				"step asserts both) and are NOT on the glass: the strip cannot fit the line, which is this section's " +
				"filed finding rather than a fault in the paint. The feed above carries the `!usage-service-unavailable` " +
				"and `!rate-limit-five-hour` prompt bubbles with their prose answers.",
		},
		{
			name:    "seven-day-allowance",
			prompts: []string{"!rate-limit-seven-day"},
			what:    "the WEEKLY allowance to draw the event's 91% under the allowed_warning arm, with the session figure and the unread caveat still beside it",
			predicate: fsPercent("weekly") + ` === "` + fsEventWeekly + `" && ` + fsAllowanceArm("weekly") + ` === "allowedWarning" && ` +
				fsPercent("session") + ` === "` + fsEventSession + `" && ` + fsUnread + ` === "serviceUnavailable"`,
			expected: "The footer's activity cell is UNCHANGED on the glass from the previous picture -- `session 82% · " +
				"resets in 59m` in the warning tone, cut off at the cell's edge. The weekly allowance now carries the " +
				"event's 91% under the same warning arm, and it is asserted in the DOM by this step, but the strip has " +
				"no room to draw it: the ONLY visible difference between this picture and the last is the feed, which " +
				"has gained the `!rate-limit-seven-day` prompt bubble and the prose `The seven_day window is 91% used.`",
		},
	})

	// The overage window: no cell in the contract, dropped LOUDLY. The warn
	// record is the scenario's whole observable contract and the picture is
	// the negative -- the footer with nothing added to it.
	s.drive(t, "!rate-limit")
	record := awaitDaemonWarn(t, repository.Dir, fsOverageOperation)
	s.awaitInPage(t, "the footer to still draw exactly the two allowance cells the contract has, and no third",
		`document.querySelectorAll('.footer-allowance').length === 2 && `+fsPercent("session")+` === "`+fsEventSession+`" && `+
			fsPercent("weekly")+` === "`+fsEventWeekly+`"`)
	p.capture("overage-dropped", "submitted `!rate-limit` (the overage window) with composer RET and let it settle",
		fmt.Sprintf("the daemon wrote the warn record `%s` (%q) to the workspace's own log, and the footer still draws exactly two `.footer-allowance` cells with the previous figures",
			fsOverageOperation, record.Message),
		"The footer's activity cell is UNCHANGED from the previous picture -- `session 82% · resets in 59m` in the "+
			"warning tone, cut off at the cell's edge -- and NOWHERE on the strip is there a third allowance cell or "+
			"any mention of overage. The feed carries one more prompt bubble, `!rate-limit`, with the prose `The "+
			"account is approaching its overage threshold.` beneath it.")
}

// ---------------------------------------------------------------------------
// D34. The five `!usage-*` outcome arms: the unread caveat beside the
// standing figures, and the available arm that retires it.
// ---------------------------------------------------------------------------

func TestPlaytestFooterUsageOutcomes(t *testing.T) {
	t.Parallel()
	s := newFsScenario(t, "11-usage-outcomes",
		"Plan D.34. The account-usage probe's five outcome arms, each after a readable sample: the four unread "+
			"reasons draw `.footer-allowance-unread` BESIDE the figures the last readable sample filed (never "+
			"instead of them), and `opus_absent` -- an available answer with one optional window missing -- reads "+
			"again and retires the line entirely, because the fake's figures are under the newsworthiness gate. "+
			"Landing 13 wanted the unread caveat's first sighting in a real webview, and this is it: the caveat is "+
			"drawn, with its exact words and beside the standing figures, in every one of these pages -- and NONE of "+
			"it reaches the glass, because the strip's one elastic cell ellipsizes before it. That is the finding.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	// ARRANGEMENT: a readable sample the unread arms must leave standing.
	// Its drawn consequence is a NEGATIVE -- 41%/63% are under the gate, so
	// no line is drawn -- and that negative is asserted so the later rows'
	// figures are known to be figures the daemon actually holds.
	s.drive(t, "!usage-available")
	s.awaitInPage(t, "the footer to draw NO rate-limit line for sub-threshold figures", fsNoRateLine)
	p.note("`!usage-available` submitted and settled, so the daemon holds a READ sample of 41% / 63%",
		"the footer draws no `.footer-activity-rate-limited` at all: both figures are under the 0.8 newsworthiness gate")

	unread := func(sample, sentence string) string {
		return fsUnread + ` === "` + sample + `" && ` + fsUnreadText + ` === "` + sentence + `" && ` +
			fsPercent("session") + ` === "` + fsStandingSession + `" && ` + fsPercent("weekly") + ` === "` + fsStandingWeekly + `" && ` +
			fsRunningCountdown("session") + ` && ` + fsRunningCountdown("weekly")
	}
	beside := func(sentence string) string {
		return "The footer's activity cell reads `session 41% · resets in 4h 59m |` in the plain tone (41% is not " +
			"newsworthy, so not bold) and is CUT OFF there with an ellipsis. `weekly 63%` and `" + sentence + "` follow " +
			"in the DOM -- this step asserts the caveat's exact words and both standing figures -- and the strip has no " +
			"room to draw them. The countdown is LIVE and runs off the fake's sampled window, which resets five hours " +
			"after the fake's own now, so the exact minutes depend on when the capture was taken. What the picture must " +
			"show is the figures STANDING (the caveat never replaced them), a countdown that is NOT `0m`, and the cell " +
			"ending in an ellipsis rather than in a bare `session 41%`."
	}

	s.run(t, []fsRow{
		{
			name:      "service-unavailable",
			prompts:   []string{"!usage-service-unavailable"},
			what:      "the service_unavailable caveat to be drawn beside the standing 41% / 63%",
			predicate: unread("serviceUnavailable", fsUnreadService),
			expected:  beside(fsUnreadService),
		},
		{
			name:      "window-unavailable",
			prompts:   []string{"!usage-available", "!usage-window-unavailable"},
			what:      "the window_unavailable caveat to be drawn beside the standing 41% / 63%",
			predicate: unread("windowUnavailable", fsUnreadWindow),
			expected:  beside(fsUnreadWindow),
		},
		{
			name:      "utilization-unavailable",
			prompts:   []string{"!usage-available", "!usage-utilization-unavailable"},
			what:      "the utilization_unavailable caveat to be drawn beside the standing 41% / 63%",
			predicate: unread("utilizationUnavailable", fsUnreadUtilization),
			expected:  beside(fsUnreadUtilization),
		},
		{
			name:    "sampling-failure",
			prompts: []string{"!usage-available", "!usage-sampling-failure"},
			what:    "the sampling_failure caveat, carrying the shim's own cause, to be drawn beside the standing 41% / 63%",
			predicate: fsUnread + ` === "samplingFailure" && ` + fsUnreadText + `.indexOf("` + fsUnreadSampling + `: ") === 0 && ` +
				fsPercent("session") + ` === "` + fsStandingSession + `" && ` + fsPercent("weekly") + ` === "` + fsStandingWeekly + `"`,
			expected: beside(fsUnreadSampling+": <the shim's own account of what threw>") +
				" The cause after the colon is the shim's verbatim error text, asserted non-empty here and, like the " +
				"rest of the caveat, off the glass.",
		},
		{
			name:      "opus-absent-retires-the-line",
			prompts:   []string{"!usage-opus-absent"},
			what:      "the rate-limit line to be gone entirely: an absent optional window is a READ, and the figures are under the gate",
			predicate: fsNoRateLine,
			expected: "The footer's activity cell is EMPTY: no `session`, no `weekly`, and no `usage unread` caveat -- the " +
				"sample read again, so the caveat is retired, and with both figures under the newsworthiness gate the " +
				"line it rode on is gone with it. The status word still reads `idle`. The feed carries the whole " +
				"sequence of `!usage-*` prompt bubbles, the last being `!usage-opus-absent` with the prose `The " +
				"account-usage probe now answers with the opus_absent shape.`",
		},
	})
}

// ---------------------------------------------------------------------------
// D35. The context bookkeeping scenarios: the footer's status text.
// ---------------------------------------------------------------------------

func TestPlaytestFooterContextStatus(t *testing.T) {
	t.Parallel()
	s := newFsScenario(t, "11-context-status",
		"Plan D.35. Three vendor attachments and what the footer says about each. Two are BOOKKEEPING the "+
			"ruling keeps off every surface -- the generic CLI tip and the token-count reminder mint no row and "+
			"no footer line -- and the third is the (invented, ungrounded) context-budget warning, which is "+
			"footer-only by ruling (Landing 8) and stands as the idle activity line verbatim.")

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	nothingStanding := fsStatusArm + ` === "idle" && ` + fsBudgetText + ` === "" && ` + fsNoRateLine
	s.run(t, []fsRow{
		{
			name:      "context-tip-draws-nothing",
			prompts:   []string{"!context-tip"},
			what:      "the footer to read idle with NO context-budget line minted from a generic CLI tip",
			predicate: nothingStanding,
			expected: "The footer's status word reads `idle` and its activity cell is EMPTY: no `context` sentence of any " +
				"kind, no allowance, nothing. The feed carries exactly the `!context-tip` prompt bubble and the prose `The " +
				"CLI offered a tip.` beneath it, and NO row for the tip itself.",
		},
		{
			name:      "tokens-reminder-draws-nothing",
			prompts:   []string{"!tokens-reminder"},
			what:      "the footer to read idle with NO context-budget line minted from a token-count reminder",
			predicate: nothingStanding,
			expected: "The footer's status word reads `idle` and its activity cell is still EMPTY. The feed carries two " +
				"prompt bubbles now, the second `!tokens-reminder` with the prose `The CLI restated the token budget.`, " +
				"and NO row for the reminder and no token count anywhere on the strip's activity cell.",
		},
		{
			name:      "context-budget-warning-stands",
			prompts:   []string{"!context-budget-warning"},
			what:      "the footer's idle activity line to carry the context-budget warning verbatim",
			predicate: fsStatusArm + ` === "idle" && ` + fsBudgetText + ` === "` + fsBudgetWarning + `"`,
			expected: "The footer's status word reads `idle` and its activity cell now carries the warning, drawn as " +
				"far as the cell reaches -- `The conversation is approaching its…` -- with the rest of `" + fsBudgetWarning +
				"` and its relative age in the DOM (this step asserts the sentence verbatim) and off the glass. The feed " +
				"carries three prompt bubbles, the last `!context-budget-warning` with the prose `The CLI warned that " +
				"the context budget is filling.`, and still NO row for the warning: it is footer-only.",
		},
	})
}

// ---------------------------------------------------------------------------
// D36. The MCP rows with their health.
// ---------------------------------------------------------------------------

// fsMcpRows holds when the NEWEST /mcp panel on the feed draws exactly the
// fake's five servers, each under the health catalogs.ts declares for it.
const fsMcpRows = `(function () {
        var panels = document.querySelectorAll('[data-feed="root"] [data-feed-row] [data-panel="mcp"]');
        if (panels.length === 0) { return false; }
        var panel = panels[panels.length - 1];
        var want = { "echo": "connected", "broken": "failed", "needs-login": "needsAuth", "slow": "pending", "switched-off": "disabled" };
        var rows = panel.querySelectorAll('.mcp-row');
        if (rows.length !== 5) { return false; }
        for (var i = 0; i < rows.length; i++) {
          var name = rows[i].querySelector('.panel-row-label').textContent;
          if (want[name] !== rows[i].getAttribute('data-mcp-status')) { return false; }
          delete want[name];
        }
        var broken = panel.querySelector('.mcp-row[data-mcp-status="failed"] .mcp-detail');
        return Object.keys(want).length === 0 && broken !== null && broken.textContent === "spawn ENOENT";
      })()`

func TestPlaytestMcpRows(t *testing.T) {
	t.Parallel()
	s := newFsScenario(t, "11-mcp",
		"Plan D.36. The MCP servers and their healths. THE PRODUCT DRAWS THEM AS THE `/mcp` COMMAND PANEL, a row "+
			"on the feed (webapp/src/panels/panels.ts drawMcpPanelView), not in the workspace sidebar; the plan's "+
			"word `sidebar` is the plan's, and the rows photographed here are the ones the product has. Two "+
			"catalogs: all five healths, and the narrowing to the one healthy server -- after which every row still "+
			"STANDS, because a narrowed catalog states nothing about the servers it omits.")

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	rows := "five rows, one per health: `echo` with a `connected` badge in the ok tone, `broken` with a `failed` " +
		"badge in the error tone and the detail line `spawn ENOENT` under it, `needs-login` with a `needs auth` " +
		"badge, `slow` with a `starting` badge, and `switched-off` with a `disabled` badge drawn dimmed."
	s.run(t, []fsRow{
		{
			name:      "mcp-all-healths",
			prompts:   []string{"!mcp-all", "/mcp"},
			what:      "the /mcp panel to draw the five-server catalog with each health as declared",
			predicate: fsMcpRows,
			expected: "The feed carries the `!mcp-all` prompt bubble, its prose `Every MCP health is now reported.`, and " +
				"beneath them the /mcp COMMAND PANEL: " + rows,
		},
		{
			name:      "mcp-healthy-keeps-the-rows",
			prompts:   []string{"!mcp-healthy", "/mcp"},
			what:      "a second /mcp panel to draw ALL FIVE rows still, healths unchanged, after the catalog narrowed to the healthy server",
			predicate: fsMcpRows,
			expected: "Beneath the first panel the feed carries the `!mcp-healthy` prompt bubble, its prose `Only the " +
				"healthy MCP server is now reported.`, and a SECOND /mcp panel identical to the first: " + rows +
				" Nothing was dropped: a narrowed catalog says nothing about the servers it omits, so their rows stand " +
				"with the healths last stated.",
		},
	})
}
