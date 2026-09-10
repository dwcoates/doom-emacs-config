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
// THREE FACTS THE PLAN'S ROW WORDING DID NOT KNOW, each settled by the schema
// or by the producers and RECORDED here rather than papered over:
//
//   - A DETACHED SHELL HAS NO SUB-FEED. "A SHELL IS NOT A FEED. It has no
//     rows, only output, so unlike a subagent bubble there is nothing to open
//     as a sub-feed: the body rides the row itself" (cards/shell.ts). So the
//     plan's "its sub-feed opened" step is asserted as the NEGATIVE it really
//     is -- the row carries no `[data-expand]` caret and no `[data-subfeed]`
//     panel -- and the spool box on the row is the whole of what a user sees.
//   - A FOREGROUND CARD NOW DRAWS ITS IMAGE AND ITS EXIT CODE. Landing 16 gave
//     `FeedToolCallReturned` the `image` form (the feed's shared
//     FeedImageBlock) and an `exit` chip (the detached shell's own
//     FeedShellExit), so `!bash-image` draws the picture and `!bash-fail`
//     draws a red `exit 3`. Both were filed by this owner as proto needs and
//     both landed; the rows below assert the landed shapes.
//   - A TIMED-OUT COMMAND'S CARD DOES NOT SETTLE, AND THAT IS THE CONTRACT.
//     The vendor auto-backgrounds rather than killing, so its receipt names a
//     `backgroundTaskId` and the shim answers no terminal for it at all: "A
//     BACKGROUNDED COMMAND DID NOT END, IT MOVED" (convert/tools/bash.ts),
//     pinned by that module's own integration test. So `!bash-timeout` is
//     photographed as the TWO rows it really is -- a `Bash` card still running
//     and a detached shell row live beneath it -- and the remainder (the card
//     has no arm that says the work MOVED) is filed rather than asserted.

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
			          card.querySelector("[data-output-body]").textContent.indexOf("two") >= 0 &&
			          card.querySelector(".shell-exit") === null`,
			expected: "A tool card titled `Bash` with the command line `pwd; ls | head`, a green `done` badge, " +
				"and an output body carrying the two lines `one` and `two`. NO exit chip in the head: this " +
				"result states no status at all, and absence draws no chip rather than a green `exit 0` the " +
				"shell never reported.",
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
				// A FORCED RESTART BOUNCES THE SHIM, and the composer is
				// CLOSED while it does: the gate reads `:restarting` and
				// refuses every submission with "composer closed: restarting"
				// (lisp/input.el). The interrupted arm says the turn ended, not
				// that the workspace came back -- so the act is not over until
				// the gate reopens, and the next row's submit would otherwise
				// race the bounce. It did, the moment the captures stopped
				// costing two seconds each. The bound is the layer's own
				// default, the same one its `agent-repl-restart-workspace`
				// scenario waits on (emacs_interrupt_e2e_test.go).
				s.E.AwaitEval("the composer gate to reopen after the forced restart",
					`(symbol-name (agent-repl-host-composer-gate `+elispString(s.Name)+`))`,
					func(raw json.RawMessage) bool { return decodeString(raw) != ":restarting" })
				// The bounce reloads the webview, so the page this playbook
				// reads must be mounted again before the next row asserts on
				// it.
				s.Input = awaitInputBuffer(t, s.E, s.Name)
				s.awaitPageMounted(t)
			},
			// THE STOP SETTLES THE CALL IT LANDED INSIDE. The vendor returns no
			// `tool_result` for a held command -- the captured `interrupt`
			// session records the stop as a bare `[Request interrupted by
			// user]` line and nothing else -- so the shim's own cut is what
			// settles the unit (convert/tools/bash.ts `cut`). Without it the
			// card went on drawing a running shell inside a turn that had
			// ended, which is what this playbook found.
			settled: `card.getAttribute("data-state") === "returned" &&
			          card.getAttribute("data-verdict") === "succeeded" &&
			          card.getAttribute("data-output-form") === "text" &&
			          card.querySelector("[data-output-body]").textContent.indexOf("interrupted by the user") >= 0 &&
			          document.querySelector('[data-feed-row][data-row-kind="turnEnded"] [data-arm="interrupted"]') !== null`,
			expected: "The `Bash` card for `tail -f /var/log/system.log` is SETTLED: a green `done` badge (a stop " +
				"is not the call breaking) and an output body reading `interrupted by the user` and nothing " +
				"else -- no output was ever returned, so none is drawn. Beneath it the turn's INTERRUPTED " +
				"terminal row, and the footer idle again. NOTHING is still drawn running.",
		},
		{
			prompt:  "!bash-fail",
			command: "exit 3",
			settled: `card.getAttribute("data-state") === "returned" &&
			          card.getAttribute("data-verdict") === "succeeded" &&
			          card.querySelector("[data-output-body]").textContent.indexOf("boom") >= 0 &&
			          card.querySelector(".shell-exit") !== null &&
			          card.querySelector(".shell-exit").getAttribute("data-exit-code") === "3" &&
			          card.querySelector(".shell-exit").className.indexOf("err") >= 0`,
			expected: "A tool card titled `Bash` with the command line `exit 3`, a green `done` badge (a non-zero " +
				"exit is the command's verdict on itself, never a failure of the call), a RED `exit 3` chip in " +
				"the head beside that badge, and an output body carrying `boom`. The chip is the very one a " +
				"detached shell wears: the two cards are the same command told twice.",
		},
		{
			prompt:  "!bash-timeout",
			command: "sleep 600",
			// THE ROW'S FACTS ARE AWAITED APART, each under its own name, so a
			// failure says WHICH of the two halves was missing rather than
			// reporting one unreadable conjunction. This cost a whole run to
			// learn: the joint predicate's diagnosis could not say whether the
			// detached row was absent or the card had wrongly settled.
			settle: func(t *testing.T, s *playtestScenario) {
				s.awaitInPage(t, "the detached shell row for sleep 600 to be drawn live",
					`(function () { var sh = `+playtestShellBubbleJS("sleep 600")+`;
					               return sh !== null && sh.getAttribute("data-state") === "live"; })()`)
				s.awaitInPage(t, "the detached shell row for sleep 600 to carry the spool the scenario wrote",
					`(function () { var sh = `+playtestShellBubbleJS("sleep 600")+`;
					               return sh.querySelector(".shell-spool") !== null &&
					                      sh.querySelector(".shell-spool").textContent.indexOf("still going") >= 0; })()`)
				s.awaitInPage(t, "the Bash card for sleep 600 to be STILL RUNNING, its work having moved rather than ended",
					`(function () { var card = `+playtestBashCardJS("sleep 600")+`;
					               return card !== null && card.getAttribute("data-state") === "running"; })()`)
			},
			// THE CARD DOES NOT SETTLE, AND THAT IS THE CONTRACT. The receipt
			// names a `backgroundTaskId`, so NEITHER plane answers a terminal
			// for it -- "A BACKGROUNDED COMMAND DID NOT END, IT MOVED" -- and
			// the work goes on as the detached row above. The card staying
			// `running` is the truth about the command; what it cannot say is
			// that the run MOVED, which is filed.
			settled: `card.getAttribute("data-state") === "running" &&
			          (function () { var sh = ` + playtestShellBubbleJS("sleep 600") + `;
			                         return sh !== null && sh.getAttribute("data-state") === "live" &&
			                                sh.querySelector(".shell-exit") === null &&
			                                sh.querySelector(".shell-spool").textContent.indexOf("still going") >= 0; })()`,
			expected: "TWO ROWS FOR ONE COMMAND, and the picture must show both. The `Bash` tool card for " +
				"`sleep 600` is still drawn RUNNING -- a running marker, no badge, no output body -- and " +
				"beneath it a DETACHED SHELL row for the same command is LIVE: a `$` command line, a running " +
				"dot, a ticking clock, a spool box carrying `still going`, a stop button, and NO exit chip. " +
				"The vendor auto-backgrounded the run rather than killing it, so the command really is still " +
				"going; the card simply has no way to say the work moved.",
		},
		{
			prompt:  "!bash-spill",
			command: "yes | head -100000",
			settled: `card.getAttribute("data-state") === "returned" &&
			          card.getAttribute("data-verdict") === "succeeded" &&
			          card.querySelector("[data-output-body]").textContent.indexOf("bytes more not shown") >= 0`,
			expected: "A tool card titled `Bash` with the command line `yes | head -100000`, a green `done` badge, " +
				"and an output body of a few `y` lines ending in the daemon's own truncation notice: " +
				"`<count> bytes more not shown`. The spill file itself is never drawn.",
		},
		{
			prompt:  "!bash-image",
			command: "screencapture",
			settled: `card.getAttribute("data-state") === "returned" &&
			          card.getAttribute("data-verdict") === "succeeded" &&
			          card.getAttribute("data-output-form") === "image" &&
			          card.querySelector(".tool-image-output img") !== null &&
			          card.querySelector(".tool-image-output img").getAttribute("src").indexOf("data:image/png;base64,") === 0 &&
			          card.querySelector(".tool-image-output img").getAttribute("alt").indexOf("screencapture") >= 0`,
			expected: "A tool card titled `Bash` with the command line `screencapture -x -`, a green `done` badge, " +
				"and AN IMAGE where every other card's output body sits -- so the output area holds a single " +
				"faint DOT and no text at all. The dot is the whole picture on purpose: the payload is the " +
				"1x1 PNG the `bash-image-output` capture recorded verbatim, and a bigger stand-in would be " +
				"the fake diverging from the vendor it mirrors. That it is an IMAGE and not text is asserted " +
				"rather than eyeballed (`data-output-form=\"image\"`, an `<img>` whose src is the data url the " +
				"DAEMON composed, captioned with the command line); what the picture shows is that the card " +
				"draws it in the output slot every other form uses.",
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
				"spool box carrying `line-1`, `line-2`, `line-3` and the spool's own `EXIT=0` line, and NO stop " +
				"button. ABOVE IT, a `Bash` tool card for the same command is drawn `running...` and stays that " +
				"way: the command was launched into the background, so no plane answers a terminal for the call " +
				"and the card has no arm that says the work MOVED. Two rows for one command is the shape, and " +
				"the remainder is filed.",
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

	// The rows that reached a settled state, re-checked once the whole family
	// has run; see the loop's tail.
	var settled []playtestDetachedRow

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
		settled = append(settled, row)
	}

	// A SETTLED ROW STAYS SETTLED, asserted at the END rather than at the
	// instant of settling, because that is where it broke: the pictures of the
	// LATER rows showed an already-`completed` run drawn live again -- an
	// orange dot, a `quiet for Ns` progress note and a stop button on a run
	// whose spool holds `EXIT=0`. Every per-row assertion above had passed,
	// each one reading the row a moment after it settled, so only a check made
	// once the world has moved on can catch a settled state being walked back.
	for _, row := range settled {
		s.awaitInPage(t, "the settled shell row for "+row.command+" to have STAYED settled",
			`(function () { var sh = `+playtestShellBubbleJS(row.command)+`;
			               return sh !== null && sh.getAttribute("data-state") === "completed" &&
			                      sh.querySelector("[data-interrupt]") === null; })()`)
	}
	p.note("every settled shell row is still settled at the end of the run",
		fmt.Sprintf("%d rows that reached `completed` still read `completed` and still carry no stop control", len(settled)))
}
