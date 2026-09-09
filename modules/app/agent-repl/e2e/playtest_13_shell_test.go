//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"strings"
	"testing"
)

// OWNER 13 of PLAYTEST-PLAN.md's partition: F42-F43 -- the shell family and
// the detached-shell family, TABLE-DRIVEN, one loop per family, one capture
// of the settled card per row plus the live state where one exists.
//
// Grounding, the same as detachedbash_e2e_test.go's: the fake SDK's own
// `src/fake/scenarios/shell.ts` for what each prompt emits and writes, and
// `webapp/src/feed/cards/{tool-call,shell}.ts` for the attributes the drawn
// card states about itself (`data-state`, `data-verdict`,
// `data-output-form`, `data-exit-code`). Every assertion below is on those
// attributes, read from inside the real webview, and the sentence a reviewer
// checks is written from what the page said at the instant of the capture.
//
// TWO FACTS THE PLAN'S ROW WORDING DID NOT KNOW, both settled by the schema
// and RECORDED here rather than papered over:
//
//   - A DETACHED SHELL HAS NO SUB-FEED. "A SHELL IS NOT A FEED. It has no
//     rows, only output, so unlike a subagent bubble there is nothing to open
//     as a sub-feed: the body rides the row itself" (cards/shell.ts). So the
//     plan's "its sub-feed opened" step is asserted as the NEGATIVE it really
//     is -- the row carries no `[data-expand]` caret and no `[data-subfeed]`
//     panel -- and the spool box on the row is the whole of what a user sees.
//   - A FOREGROUND CARD DRAWS NO EXIT CODE, AND NO IMAGE. `FeedToolCallReturned`
//     (frontend/v1/feed.proto) carries a verdict and an output FORM and
//     nothing about an exit code; only the detached `FeedShell` has an exit
//     chip. And the form oneof has no image arm, so `!bash-image` settles
//     on the `none` form -- the shape detachedbash_e2e_test.go's #37 already
//     pins. Both are filed in this owner's report; neither is fixable here
//     without a proto change.

// playtestBashCardJS answers the `.tool-card` of the Bash tool call whose
// drawn text contains COMMAND, or null. Rows are matched by their command
// text because every row of one family shares one feed and the FeedId is
// daemon-minted and opaque, the same rule detachedbash_e2e_test.go states.
func playtestBashCardJS(command string) string {
	return `(function () {
	  var rows = document.querySelectorAll('[data-feed-row][data-row-kind="activity"][data-unit="simpleToolCall"]');
	  for (var i = 0; i < rows.length; i++) {
	    var name = rows[i].querySelector('.tool-name');
	    if (!name || name.textContent.trim() !== 'Bash') { continue; }
	    if (rows[i].textContent.indexOf(` + jsString(command) + `) < 0) { continue; }
	    return rows[i].querySelector('.tool-card');
	  }
	  return null;
	})()`
}

// playtestShellBubbleJS answers the `.shell-bubble` of the detached shell row
// whose drawn command contains COMMAND, or null.
func playtestShellBubbleJS(command string) string {
	return `(function () {
	  var rows = document.querySelectorAll('[data-feed-row][data-row-kind="detachedShell"]');
	  for (var i = 0; i < rows.length; i++) {
	    if (rows[i].textContent.indexOf(` + jsString(command) + `) < 0) { continue; }
	    return rows[i].querySelector('.shell-bubble');
	  }
	  return null;
	})()`
}

// readInPage answers a JavaScript expression's string value from the page.
// It is the probe's two-eval shape with the answer itself returned, for
// reading an attribute INTO a manifest sentence at capture time -- the same
// reason `armPaint` exists beside `awaitArm`.
func (s *playtestScenario) readInPage(t *testing.T, what, expression string) string {
	t.Helper()
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	var answer string
	s.E.AwaitEvalFor(playtestPageBound, what,
		`(agent-repl-playtest--probe `+elispString(s.Name)+` `+
			elispString(`(function () { try { return "ok:" + String(`+expression+`); } catch (e) { return "no: " + e; } })()`)+`)`,
		func(raw json.RawMessage) bool {
			got := decodeString(raw)
			if !strings.HasPrefix(got, "ok:") {
				return false
			}
			answer = strings.TrimPrefix(got, "ok:")
			return true
		})
	return answer
}

// playtestShellRow is one row of the foreground family's table.
type playtestShellRow struct {
	prompt  string
	command string
	// live, when set, is the JavaScript predicate for the card's LIVE state
	// and the sentence its picture must match; the row is photographed live
	// before it is settled.
	live         string
	liveExpected string
	// settle, when set, is the act that settles a row that would otherwise
	// hold forever.
	settle func(t *testing.T, s *playtestScenario)
	// settled is the predicate for the card's settled shape, over the card
	// element bound to `card`.
	settled  string
	expected string
}

// TestPlaytestShellFamily is plan F.42: `!bash`, `!bash-hold`, `!bash-fail`,
// `!bash-timeout`, `!bash-spill` and `!bash-image`, one loop, one world.
func TestPlaytestShellFamily(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "13-shell",
		"Plan F.42. The foreground shell family, table-driven: each prompt's tool card photographed "+
			"in its settled shape, and `!bash-hold` photographed live first.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)

	rows := []playtestShellRow{
		{
			prompt:  "!bash",
			command: "pwd; ls | head",
			settled: `card.getAttribute("data-state") === "returned" &&
			          card.getAttribute("data-verdict") === "succeeded" &&
			          card.getAttribute("data-output-form") === "text" &&
			          card.querySelector("[data-output-body]").textContent.indexOf("one") >= 0 &&
			          card.querySelector("[data-output-body]").textContent.indexOf("two") >= 0`,
			expected: "A tool card titled `Bash` with the command line `pwd; ls | head`, a SUCCEEDED badge, " +
				"and an output body carrying the two lines `one` and `two`. No exit code is drawn on a " +
				"foreground card: the wire carries none (FeedToolCallReturned has no exit field).",
		},
		{
			prompt:  "!bash-hold",
			command: "tail -f /var/log/system.log",
			live:    `card.getAttribute("data-state") === "running"`,
			liveExpected: "A tool card titled `Bash` with the command line `tail -f /var/log/system.log` in its " +
				"RUNNING state: a running marker and no output body, and the footer says a turn is in flight.",
			settle: func(t *testing.T, s *playtestScenario) {
				// The one interrupting act this layer has: `SPC o C-c` with
				// `C-u`, supplied as the command's own FORCE argument
				// (EMACS-LAYER-SPEC.md, "There is no interrupt command").
				if want, got := "agent-repl-restart-workspace", s.E.LeaderBinding("o C-c"); got != want {
					t.Fatalf("SPC o C-c resolves to %q, want %q", got, want)
				}
				s.E.Eval(`(agent-repl-restart-workspace t ` + elispString(s.Name) + `)`)
				s.awaitArm(t, s.Name, "the held turn to settle interrupted", ":interrupted")
			},
			settled: `document.querySelector('[data-feed-row][data-row-kind="turnEnded"] [data-arm="interrupted"]') !== null`,
			expected: "The feed carries the turn's INTERRUPTED terminal row beneath the `Bash` card. The card " +
				"itself is drawn in whatever state the row's assertion column records (`data-state`), since the " +
				"held call emitted no result of its own; the footer is idle again.",
		},
		{
			prompt:  "!bash-fail",
			command: "exit 3",
			settled: `card.getAttribute("data-state") === "returned" &&
			          card.getAttribute("data-verdict") === "succeeded" &&
			          card.querySelector("[data-output-body]").textContent.indexOf("boom") >= 0`,
			expected: "A tool card titled `Bash` with the command line `exit 3`, a SUCCEEDED badge (a non-zero " +
				"exit is the command's verdict on itself, never a failure of the call), and an output body " +
				"carrying `boom`. NO exit code is drawn anywhere on the card: the foreground wire carries none.",
		},
		{
			prompt:  "!bash-timeout",
			command: "sleep 600",
			settled: `card.getAttribute("data-state") === "returned" &&
			          card.getAttribute("data-verdict") === "succeeded" &&
			          card.querySelector("[data-output-body]").textContent.indexOf("timed out after") >= 0 &&
			          (function () { var sh = ` + playtestShellBubbleJS("sleep 600") + `;
			                         return sh !== null && sh.getAttribute("data-state") === "live"; })()`,
			expected: "TWO things for one command. The `Bash` tool card for `sleep 600` is SUCCEEDED with an " +
				"output body that says it `timed out after` the scenario's two minutes, and beneath it a " +
				"DETACHED SHELL row for the same command is drawn LIVE -- a running dot, a ticking clock, " +
				"a spool box with `still going`, a stop button, and NO exit chip -- because the vendor " +
				"auto-backgrounded the run rather than killing it.",
		},
		{
			prompt:  "!bash-spill",
			command: "yes | head -100000",
			settled: `card.getAttribute("data-state") === "returned" &&
			          card.getAttribute("data-verdict") === "succeeded" &&
			          card.querySelector("[data-output-body]").textContent.indexOf("bytes more not shown") >= 0`,
			expected: "A tool card titled `Bash` with the command line `yes | head -100000`, a SUCCEEDED badge, " +
				"and an output body of a few `y` lines ending in the daemon's own truncation notice: " +
				"`<count> bytes more not shown`. The spill file itself is never drawn.",
		},
		{
			prompt:  "!bash-image",
			command: "screencapture",
			settled: `card.getAttribute("data-state") === "returned" &&
			          card.getAttribute("data-verdict") === "succeeded" &&
			          card.getAttribute("data-output-form") === "none"`,
			expected: "A tool card titled `Bash` with the command line `screencapture -x -` and a SUCCEEDED " +
				"badge, and NO OUTPUT BODY AT ALL -- no image, no text. FILED, NOT A PASS: the plan " +
				"expects the image rendered, and it cannot be: FeedToolCallReturned's form oneof has no " +
				"image arm, so the daemon's bashOutputText drops the image form to an empty text and " +
				"the card settles on `none` (the shape detachedbash_e2e_test.go's #37 pins).",
		},
	}

	for _, row := range rows {
		cardOf := playtestBashCardJS(row.command)
		s.submit(t, row.prompt)
		s.awaitInPage(t, "the Bash card for "+row.command+" to be drawn", cardOf+` !== null`)

		if row.live != "" {
			s.awaitInPage(t, "the Bash card for "+row.command+" to be live",
				`(function () { var card = `+cardOf+`; return card !== null && (`+row.live+`); })()`)
			state := s.readInPage(t, "the live card's state", `(`+cardOf+`).getAttribute("data-state")`)
			p.capture(playtestStep(row.prompt, "live"), "`"+row.prompt+"` submitted with composer RET",
				fmt.Sprintf("the `Bash` card for `%s` reads `data-state=%q` at the instant of the capture", row.command, state),
				row.liveExpected)
		}

		if row.settle != nil {
			row.settle(t, s)
		}
		s.awaitInPage(t, "the Bash card for "+row.command+" to settle",
			`(function () { var card = `+cardOf+`; return card !== null && (`+row.settled+`); })()`)
		settled := s.awaitArm(t, name, "the turn to settle", emGHISettledArms...)
		state := s.readInPage(t, "the settled card's state",
			`(function () { var c = `+cardOf+`; return c.getAttribute("data-state") + "/" + c.getAttribute("data-verdict") + "/" + c.getAttribute("data-output-form"); })()`)
		p.capture(playtestStep(row.prompt, "settled"), "`"+row.prompt+"` run to its settled shape",
			fmt.Sprintf("the `Bash` card for `%s` reads state/verdict/form `%s` and the roster arm is %s", row.command, state, settled),
			row.expected)
	}
}

// playtestStep names a capture after its prompt: `bash-fail-settled`.
func playtestStep(prompt, phase string) string {
	return strings.TrimPrefix(prompt, "!") + "-" + phase
}

// playtestDetachedRow is one row of the detached family's table.
type playtestDetachedRow struct {
	prompt  string
	command string
	// spool is a line the live spool must already carry before the live
	// picture is taken, when the scenario writes one before it settles.
	spool string
	// exit is the code the settled chip must carry; -1 for a row that never
	// settles.
	exit     int
	live     string
	expected string
}

// TestPlaytestDetachedShellFamily is plan F.43: `!bash-detach`,
// `!bash-detach-poll`, `!bash-detach-fail`, `!bash-detach-live`, one loop,
// one world. Each row is photographed once while its detached row is present
// -- with the state the page reports at that instant written into the
// assertion column, since the live window is the scenario's own schedule --
// and once settled, where it settles.
func TestPlaytestDetachedShellFamily(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "13-detached-shell",
		"Plan F.43. The detached shell family, table-driven: each backgrounded command's row while it "+
			"runs, and its settled shape with the exit chip. A shell row has NO sub-feed (cards/shell.ts): "+
			"the spool box on the row is the whole of it, and that is asserted as a negative.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)

	rows := []playtestDetachedRow{
		{
			prompt: "!bash-detach", command: "for i in 1 2 3", exit: 0,
			live: "The detached shell row for `for i in 1 2 3; do echo line-$i; sleep 1; done`: a `$` command line, " +
				"a running dot and a ticking clock while live, a spool box with the `line-N` lines that have " +
				"arrived so far, and a stop button. No caret and no sub-feed: the spool IS the body.",
			expected: "The same row SETTLED: the outcome word `completed`, a green `exit 0` chip in the head, the " +
				"spool box carrying `line-1`, `line-2`, `line-3`, and no stop button.",
		},
		{
			prompt: "!bash-detach-poll", command: "tail -f build.log", exit: 0,
			live: "The detached shell row for `tail -f build.log`, live or already settled per the assertion " +
				"column, with a spool box that carries `compiling`, then `linking`, then `done` as they land.",
			expected: "The same row SETTLED with a green `exit 0` chip and the spool `compiling` / `linking` / " +
				"`done`. NO `TaskOutput` tool card is drawn anywhere: the polls are an exempt drop, never a " +
				"card and never an unmodeled warning.",
		},
		{
			prompt: "!bash-detach-fail", command: "echo error", exit: 3,
			live: "The detached shell row for `echo error && exit 3`, live or already settled per the assertion " +
				"column; its spool carries the one line `error`.",
			expected: "The same row SETTLED: the outcome word is still `completed` (a non-zero exit is not an arm), " +
				"and the exit chip is a RED `exit 3`. The spool box carries `error`.",
		},
		{
			prompt: "!bash-detach-live", command: "sleep 100000", spool: "partial output with no terminator", exit: -1,
			live: "The detached shell row for `sleep 100000` LIVE, and it stays live: a running dot, a ticking " +
				"clock, a stop button, NO exit chip, and a spool box carrying `partial output with no " +
				"terminator`. Nothing ever settles it.",
		},
	}

	for _, row := range rows {
		bubbleOf := playtestShellBubbleJS(row.command)
		s.submit(t, row.prompt)
		spoolWait := `true`
		if row.spool != "" {
			spoolWait = `sh.querySelector(".shell-spool") !== null && sh.querySelector(".shell-spool").textContent.indexOf(` + jsString(row.spool) + `) >= 0`
		}
		s.awaitInPage(t, "the detached shell row for "+row.command+" to be drawn",
			`(function () { var sh = `+bubbleOf+`; return sh !== null && (`+spoolWait+`); })()`)
		// THE NEGATIVE THE PLAN'S WORDING BECOMES: no sub-feed to open.
		s.awaitInPage(t, "the detached shell row for "+row.command+" to carry no caret and no sub-feed panel",
			`(function () { var sh = `+bubbleOf+`; var r = sh.closest("[data-feed-row]");
			               return r.querySelector("[data-expand]") === null && r.querySelector("[data-subfeed]") === null; })()`)
		state := s.readInPage(t, "the shell row's state", `(`+bubbleOf+`).getAttribute("data-state")`)
		p.capture(playtestStep(row.prompt, "row"), "`"+row.prompt+"` submitted with composer RET",
			fmt.Sprintf("a `detachedShell` row for `%s` is drawn, reads `data-state=%q` at the instant of the capture, "+
				"and carries NO `[data-expand]` caret and NO `[data-subfeed]` panel", row.command, state),
			row.live)

		settledArm := s.awaitArm(t, name, "the turn that started the detached run to settle", emGHISettledArms...)
		if row.exit < 0 {
			p.note("`"+row.prompt+"`'s turn settled while its shell stays live",
				fmt.Sprintf("the roster arm is %s and the shell row still reads `data-state=\"live\"`; the scenario "+
					"writes no `EXIT=` line, so no settled picture exists for this row", settledArm))
			continue
		}
		s.awaitInPage(t, fmt.Sprintf("the detached shell row for %s to settle with exit %d", row.command, row.exit),
			`(function () { var sh = `+bubbleOf+`; return sh !== null && sh.getAttribute("data-state") === "completed" &&
			               sh.querySelector(".shell-exit") !== null &&
			               sh.querySelector(".shell-exit").getAttribute("data-exit-code") === "`+fmt.Sprint(row.exit)+`" &&
			               sh.querySelector("[data-interrupt]") === null; })()`)
		if row.prompt == "!bash-detach-poll" {
			s.awaitInPage(t, "no TaskOutput tool card to be drawn for the exempt polls",
				`Array.prototype.every.call(document.querySelectorAll('[data-unit="simpleToolCall"] .tool-name'),
				   function (n) { return n.textContent.trim() !== "TaskOutput"; })`)
		}
		p.capture(playtestStep(row.prompt, "settled"), "`"+row.prompt+"`'s spool terminated and its notification delivered",
			fmt.Sprintf("the `detachedShell` row for `%s` reads `data-state=\"completed\"`, its `.shell-exit` chip carries "+
				"`data-exit-code=\"%d\"`, and its stop control is gone; the roster arm is %s", row.command, row.exit, settledArm),
			row.expected)
	}
}
