//go:build playtest

package e2e

import (
	"fmt"
	"strings"
	"testing"
)

// OWNER 17 of PLAYTEST-PLAN.md's partition: section H -- every `!fail-*` and
// every `!api-*` scenario, one capture of the turn's terminal per row.
//
// ONE TABLE, ONE LOOP, ONE WORLD. The table is failurearms_e2e_test.go's and
// turnlifecycle_e2e_test.go's, restated row for row: the arm's NAME as the
// webapp spells it on `[data-arm]` (feed.proto's oneof member in camel case)
// and the EXACT headline the daemon composes in
// daemon/internal/resolve/feed/turnended.go, which feed.proto has the client
// draw verbatim. Every one of these is a distinct sentence, and a collapsed
// one is exactly the defect class the e2e table was strengthened against --
// so the assertion here is EQUALITY on the drawn text, never "non-empty".
//
// The rows run in ONE world, back to back, so the feed accumulates: the
// picture of row N carries the terminals of rows 1..N-1 above it, which is
// what a user who keeps hitting the vendor's failures actually sees. The
// terminal under test is therefore always THE NEWEST -- the last
// `turnEnded` row of the root feed -- and every in-page assertion counts the
// terminals first, so a row whose turn never ended cannot be satisfied by an
// earlier row's terminal.
//
// THE ROSTER ARM IS NOT THE FINISH SIGNAL HERE, and that is deliberate. The
// sidebar resolver latches `vendor_blocked` on six of these failures and
// clears it only on a session start (daemon/internal/resolve/sidebar/
// resolver.go), so after the first such row the tab reads `:vendor-blocked`
// for the rest of the world and cannot say when a later turn ended. The turn's
// end is read off the feed -- the terminal row IS the liveness anchor
// (webapp/src/feed/rows/turn-ended.ts) -- and the roster arm is awaited on the
// settled set only so the picture is of a quiescent tab, with the arm it read
// recorded in the manifest.

// playtestFailureArmRow is one row of section H.
type playtestFailureArmRow struct {
	// scenario is the fake's registry name; the prompt typed is `!scenario`.
	scenario string
	// arm is the terminal's `[data-arm]` -- the feed.proto oneof member as the
	// webapp spells it. "interrupted" is the one non-errored outcome.
	arm string
	// headline is the daemon's exact sentence; empty for the interrupted arm,
	// whose row carries no headline element and reads "interrupted".
	headline string
	// vendor is the vendor's own sentence drawn beneath the headline
	// (`FeedTurnEndedErrored.message`); empty means none is drawn.
	vendor string
	// work says what became of the work in flight when the stop landed:
	// `survives` asserts the partial answer is still on the feed, settled;
	// `none` asserts NO response row was fabricated for this turn; `unasserted`
	// makes no claim (the api family's failed assistant message is not this
	// section's subject).
	work string
}

const (
	playtestWorkSurvives   = "survives"
	playtestWorkNone       = "none"
	playtestWorkUnasserted = "unasserted"
)

// playtestFailureArmRows is the whole of section H, in the fake's own registry
// order (failures.ts FAILURE_SCENARIOS). `fail-marker` is deliberately NOT a
// row: its prompt is the merge pipeline's `e2e-fail-this-turn` gate, not a
// `!` scenario a user types, and SCENARIO-MATRIX.md marks it not captureable.
var playtestFailureArmRows = []playtestFailureArmRow{
	// The five turnlifecycle_e2e_test.go drives. Their sentences are
	// turnended.go's producerErrorArm, which that file asserts only as
	// non-empty; they are pinned exactly here.
	{scenario: "fail-execution", arm: "executionError", headline: "the run broke while executing",
		vendor: "the turn raised during execution", work: playtestWorkSurvives},
	{scenario: "fail-max-turns", arm: "maxTurns", headline: "stopped at the turn limit",
		vendor: "the turn hit its max-turns ceiling", work: playtestWorkSurvives},
	{scenario: "fail-budget", arm: "maxBudget", headline: "stopped at the budget",
		vendor: "the turn exhausted its usd budget", work: playtestWorkSurvives},
	{scenario: "fail-structured-output", arm: "turnFailed", headline: "the run ended: structured_output_retry_exhausted",
		vendor: "the structured-output retries were exhausted", work: playtestWorkSurvives},
	// The ten `turn_failed` stops of TestTerminalReasonTurnFailedArms.
	{scenario: "fail-blocking-limit", arm: "turnFailed", headline: "an account-level block stopped the run",
		vendor: "the account's blocking limit was reached", work: playtestWorkSurvives},
	{scenario: "fail-rapid-refill", arm: "turnFailed", headline: "the account's refill-rate breaker tripped — this is a wait, not a fault",
		vendor: "the rapid-refill breaker tripped", work: playtestWorkSurvives},
	{scenario: "fail-prompt-too-long", arm: "turnFailed", headline: "the prompt was too long to send — the context must be cut first",
		vendor: "the prompt exceeded the context window", work: playtestWorkSurvives},
	{scenario: "fail-image", arm: "turnFailed", headline: "an image in the request could not be processed",
		vendor: "an attached image was rejected", work: playtestWorkSurvives},
	{scenario: "fail-model", arm: "turnFailed", headline: "the model errored in a way the API did not classify",
		vendor: "the requested model is unavailable", work: playtestWorkSurvives},
	{scenario: "fail-malformed-tool-use", arm: "turnFailed", headline: "the model's tool calls could not be parsed and the attempts ran out",
		vendor: "the model produced malformed tool input on every retry", work: playtestWorkSurvives},
	{scenario: "fail-tool-deferred", arm: "turnFailed", headline: "the run ended waiting on a deferred tool call",
		vendor: "the turn deferred a tool call to the host", work: playtestWorkSurvives},
	{scenario: "fail-tool-deferred-unavailable", arm: "turnFailed", headline: "the run ended on a tool call deferred to something unavailable",
		vendor: "the deferred tool is not available to this host", work: playtestWorkSurvives},
	{scenario: "fail-turn-setup", arm: "turnFailed", headline: "the run could not be set up and never reached the model",
		vendor: "the turn could not be set up", work: playtestWorkSurvives},
	// The one stop that is NOT a failure (TestTurnAbortedWhileToolsRunning):
	// a user stop through the tools is drawn as the user's act.
	{scenario: "fail-aborted-tools", arm: "interrupted", work: playtestWorkSurvives},
	// The two Stop-hook arms: the same sentence reached two ways, and
	// deliberately DISTINCT from hook_stopped's below.
	{scenario: "fail-stop-hook", arm: "stopHookPrevented", headline: "a Stop hook ended the run",
		vendor: "a Stop hook prevented the turn from finishing", work: playtestWorkSurvives},
	{scenario: "fail-hook-stopped", arm: "turnFailed", headline: "a hook ended the run",
		vendor: "a hook stopped the turn", work: playtestWorkSurvives},
	// The fake emits NO assistant content here, so an empty bubble would be an
	// invention (TestTurnStopContinuationPrevented's specific negative).
	{scenario: "fail-continuation-prevented", arm: "stopHookPrevented", headline: "a Stop hook ended the run",
		vendor: "continuation was prevented", work: playtestWorkNone},
	// The twelve api classes of TestApiRequestFailedArms: twelve arms, twelve
	// distinct sentences, no evidence clause.
	{scenario: "api-429", arm: "rateLimited", headline: "rate limited by the vendor",
		vendor: "Rate limited; retry after 30 seconds.", work: playtestWorkUnasserted},
	{scenario: "api-529", arm: "overloaded", headline: "the vendor API is overloaded",
		vendor: "The API is overloaded.", work: playtestWorkUnasserted},
	{scenario: "api-401", arm: "authenticationFailed", headline: "the credential was rejected — sign in again",
		vendor: "Authentication failed.", work: playtestWorkUnasserted},
	{scenario: "api-403", arm: "permissionDenied", headline: "the credential lacks permission for this request",
		vendor: "Permission denied for this request.", work: playtestWorkUnasserted},
	{scenario: "api-400", arm: "invalidRequest", headline: "the vendor refused the request as malformed",
		vendor: "The request was invalid.", work: playtestWorkUnasserted},
	{scenario: "api-413", arm: "requestTooLarge", headline: "the request exceeded the vendor's size limit",
		vendor: "The request was too large.", work: playtestWorkUnasserted},
	{scenario: "api-404", arm: "notFound", headline: "the model or resource does not exist",
		vendor: "The requested model was not found.", work: playtestWorkUnasserted},
	{scenario: "api-500", arm: "internal", headline: "the vendor API hit its own internal error",
		vendor: "The service raised.", work: playtestWorkUnasserted},
	{scenario: "api-billing", arm: "billingError", headline: "the account could not be charged — check your billing",
		vendor: "The account has a billing problem.", work: playtestWorkUnasserted},
	{scenario: "api-oauth-org", arm: "oauthOrgNotAllowed", headline: "your organization does not allow this OAuth access",
		vendor: "This organization is not permitted to use OAuth here.", work: playtestWorkUnasserted},
	{scenario: "api-max-output", arm: "maxOutputTokens", headline: "the request asked for more output than the model will produce",
		vendor: "The response hit the max output tokens.", work: playtestWorkUnasserted},
	{scenario: "api-unmodeled", arm: "vendorUnmodeled", headline: "the vendor reported an error class we do not model yet",
		vendor: "An error class this build does not model.", work: playtestWorkUnasserted},
}

// playtestFailureSettledArms is the set the tab may rest on once one of these
// turns has ended. `:vendor-blocked` is in it because six rows latch it and it
// then outlives the turn (see the file comment); `:interrupted` because the
// tools-phase abort is a user stop. The arm actually read is recorded, not
// chosen.
var playtestFailureSettledArms = append([]string{playtestVendorBlockedArm}, emGHISettledArms...)

// playtestTerminalJS answers a JavaScript expression that is true exactly when
// the root feed holds `count` terminal rows and the NEWEST is the arm and
// headline the row states. Counting first is what binds the check to THIS
// turn: an earlier row's terminal cannot satisfy a later row's wait.
func playtestTerminalJS(count int, row playtestFailureArmRow) string {
	var text string
	if row.arm == "interrupted" {
		text = `body.textContent === "interrupted"`
	} else {
		text = `(function () {
		            var cause = body.querySelector(".turn-ended-cause");
		            if (!cause || cause.textContent !== ` + jsString(row.headline) + `) { return false; }
		            var vendor = body.querySelector(".turn-ended-vendor");
		            return !!vendor && vendor.textContent === ` + jsString(row.vendor) + `;
		          })()`
	}
	return `(function () {
	            var ends = document.querySelectorAll('[data-feed="root"] [data-feed-row][data-row-kind="turnEnded"]');
	            if (ends.length !== ` + fmt.Sprint(count) + `) { return false; }
	            var body = ends[ends.length - 1].querySelector(".turn-ended");
	            if (!body || body.getAttribute("data-arm") !== ` + jsString(row.arm) + `) { return false; }
	            return ` + text + `;
	          })()`
}

// playtestNewestTurnJS is the turn id the newest terminal row carries, as a
// JavaScript expression; the work-in-flight checks key on it so a response
// bubble from an earlier row cannot stand in for this row's.
const playtestNewestTurnJS = `(function () {
	            var ends = document.querySelectorAll('[data-feed="root"] [data-feed-row][data-row-kind="turnEnded"]');
	            return ends[ends.length - 1].getAttribute("data-turn");
	          })()`

// playtestSurvivingWorkJS is true when THIS turn's partial answer is on the
// root feed and settled -- exactly the row faAssertPartialWorkSurvives reads
// off the daemon, seen from the page.
func playtestSurvivingWorkJS() string {
	return `(function () {
	            var turn = ` + playtestNewestTurnJS + `;
	            var rows = document.querySelectorAll('[data-feed="root"] [data-feed-row][data-row-kind="activity"][data-unit="response"][data-turn="' + turn + '"]');
	            if (rows.length !== 1) { return false; }
	            return rows[0].getAttribute("data-state") === "success" &&
	                   rows[0].textContent.indexOf(` + jsString(faPartialWork) + `) !== -1;
	          })()`
}

// playtestNoWorkJS is true when THIS turn drew no response row at all.
func playtestNoWorkJS() string {
	return `(function () {
	            var turn = ` + playtestNewestTurnJS + `;
	            return document.querySelectorAll('[data-feed="root"] [data-feed-row][data-row-kind="activity"][data-unit="response"][data-turn="' + turn + '"]').length === 0;
	          })()`
}

// TestPlaytestFailureArms is section H: one loop over every failure and API
// arm, one picture of each terminal.
func TestPlaytestFailureArms(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "17-failure-arms",
		"Plan section H. Every `!fail-*` and every `!api-*` scenario, run back to back in one "+
			"workspace; after each, the newest terminal row is asserted by ARM NAME and EXACT HEADLINE "+
			"before its picture is taken. Every headline is a distinct sentence; a collapsed one is the defect.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with its panel open",
		"the panel's webview is live, the feed host is mounted and the webapp drew its footer")

	for i, row := range playtestFailureArmRows {
		count := i + 1
		prompt := "!" + row.scenario
		s.submit(t, prompt)

		// THE MANDATORY ASSERTION: the arm by name and the headline verbatim,
		// on the newest terminal, read out of the live page.
		s.awaitInPage(t, fmt.Sprintf("terminal %d to be drawn as %s reading %q", count, row.arm, row.headline),
			playtestTerminalJS(count, row))

		var fate string
		switch row.work {
		case playtestWorkSurvives:
			s.awaitInPage(t, "this turn's partial answer to be on the feed and settled", playtestSurvivingWorkJS())
			fate = fmt.Sprintf("exactly one settled response row for this turn carries %q", faPartialWork)
		case playtestWorkNone:
			s.awaitInPage(t, "this turn to have drawn no response row", playtestNoWorkJS())
			fate = "no response row exists for this turn"
		default:
			fate = "the fate of the failed assistant message is not this row's subject"
		}

		arm := s.awaitArm(t, name, "the tab to rest on a settled arm", playtestFailureSettledArms...)

		var asserted, expected string
		if row.arm == "interrupted" {
			asserted = fmt.Sprintf("the newest of %d terminal rows carries `data-arm=\"interrupted\"` and reads exactly `interrupted`; %s; the roster arm read %s",
				count, fate, arm)
			expected = "The BOTTOM-MOST row of the feed is the terminal, in the MUTED register (not the error color), " +
				"reading exactly \"interrupted\" -- a user stop, never drawn as a failure. Directly above it, under the " +
				"`" + prompt + "` prompt bubble, the assistant's response bubble \"" + faPartialWork + "\" is still present and settled."
		} else {
			asserted = fmt.Sprintf("the newest of %d terminal rows carries `data-arm=%q`, its `.turn-ended-cause` reads exactly %q and its `.turn-ended-vendor` reads exactly %q; %s; the roster arm read %s",
				count, row.arm, row.headline, row.vendor, fate, arm)
			expected = "The BOTTOM-MOST row of the feed is the terminal, drawn in the FAILURE REGISTER (the error color), " +
				"and its first line reads exactly: \"" + row.headline + "\". Beneath that line, muted, the vendor's own " +
				"sentence: \"" + row.vendor + "\"."
			switch row.work {
			case playtestWorkSurvives:
				expected += " Above the terminal, under the `" + prompt + "` prompt bubble, the assistant's response bubble \"" +
					faPartialWork + "\" is STILL VISIBLE: the work in flight survived the stop."
			case playtestWorkNone:
				expected += " Between the `" + prompt + "` prompt bubble and the terminal there is NO response bubble: the fake " +
					"emitted no assistant content and nothing was invented."
			}
			if row.arm == "vendorUnmodeled" {
				expected += " The vendor's own class name `unknown` is drawn between the headline and the vendor sentence."
			}
			if row.arm == "rateLimited" || row.arm == "overloaded" {
				expected += " A retry line follows the vendor sentence (a countdown or \"ready to retry\")."
			}
		}
		if count > 1 {
			expected += fmt.Sprintf(" The %d earlier rows' prompts and terminals are above, each with its own distinct sentence.", count-1)
		}
		p.capture(strings.ReplaceAll(row.scenario, "/", "-"),
			fmt.Sprintf("`%s` typed into the composer and submitted with RET", prompt),
			asserted, expected)
	}
}
