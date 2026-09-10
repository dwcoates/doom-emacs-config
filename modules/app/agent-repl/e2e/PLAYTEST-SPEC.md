# The playtest: driving the real application headlessly and photographing it

`PLAYTEST-PLAN.md` is the owner's plan — 63 playbooks in sections A–K. This
document is how the plan is implemented on top of the Emacs client layer
(`EMACS-LAYER-SPEC.md`), what the capture mechanism is and why it is that
one, and what a reviewer does with the output.

Run it with:

```bash
modules/app/agent-repl/bin/playtest.sh                 # every playbook
modules/app/agent-repl/bin/playtest.sh -run TestPlaytestTabArm   # one, by name
```

The playbooks live one file per `PLAYTEST-PLAN.md` owner —
`e2e/playtest_NN_<subject>_test.go`, writing under `playtest/NN-<subject>/` —
and everything they share is in `e2e/playtest_scenario_test.go` (the world,
the page probe, the arm helpers) and `e2e/playtest_capture_test.go` (the
capture mechanism). An owner adds a file and shares nothing else.

The playbooks are behind the `playtest` build tag, so the ordinary
`go test ./e2e` never starts one: a playbook holds an Emacs slot for its
whole length and writes files a human then has to read, which is not work a
merge gate should be doing.

## The fake SDK is the only vendor, and that is structural

`AGENT_REPL_FORBID_VENDOR_CALLS=1` is on the Emacs process, and the daemon,
the shim, the store and the sidecar all inherit it — so every vendor entry
point refuses loudly rather than reaching Anthropic (`AGENTS.md`, "No real
Claude/Anthropic calls from tests"). `AGENT_REPL_FAKE=1` travels the same
way and reaches the real TypeScript shim as `--fake`, so the shim is real
and only the SDK behind it is not. The git is the scripted fake git the
world installs ahead of Emacs on PATH; no real git process runs anywhere.

None of that is the playtest's own arrangement. It is `NewEmacsWorld`'s, and
a playbook inherits it by being an Emacs-layer scenario.

## Functional first, pictures second

This is the plan's rule and the harness enforces its shape:

- **Every step carries a programmatic assertion**, and every one of them is
  an assertion the Emacs layer's own scenarios already make — the roster arm
  awaits (`emGHIAwaitStatus`), the composer binding check, the registry
  readback, the panel-window await, the daemon-side roster cross-check
  (`awaitDaemonRoster`). Nothing new was invented to assert with.
- **A step that cannot start fails right there.** A void function, a missing
  binding, a daemon that never answers: the harness's ordinary failure path
  reports the elisp error text, and the failure artifacts now carry the tail
  of Emacs's own `*Messages*` buffer beside the pty output, the Xvfb log, the
  state root and the world's daemon/shim/store/sidecar logs.
- **A capture is optional.** It is taken only where the step's subject is
  VISUAL — the tab bar's painted state and the webapp's rendering — and only
  AFTER that step's assertion passed. `playbook.note` records a
  functional-only step in the manifest with no image, so the sequence a
  reviewer follows is whole without giving them pictures to read that decide
  nothing.

## The capture mechanism, and the xwidget result

**`x-export-frames` cannot be used, and it was measured rather than assumed.**
Emacs 27+ can export its own frame as a PNG from inside the process, which
needs no tool in the image at all, so it was tried first. It re-renders the
frame through EMACS'S OWN REDISPLAY, and the agent-repl panel is an
`xwidget-webkit` webview — a real GTK child widget that WebKit paints, not
something Emacs draws. Measured in this image on a page with a solid red body
and green text: the exported PNG carried the header line, the mode line and
the frame's chrome, and an **empty white rectangle** where the webview was.
The webapp is the whole reason a screenshot is wanted, so a capture that
cannot see it is not a capture.

**The X framebuffer sees it.** The same page read out of the X server's own
screen memory carried 405,401 red and 20,820 green pixels — the webview,
painted, along with the GTK menu bar and toolbar the frame export also
misses.

**Nothing in the image can read that memory either, so Xvfb writes it to
disk.** `xwd`, ImageMagick, `scrot` and `xdotool` are all absent, verified
from inside the container rather than inferred from the Dockerfile. So
`startXvfb` passes `-fbdir`, which makes Xvfb mmap screen 0's framebuffer to
a file; a capture reads that file. **No image change was needed**: `-fbdir`
is a runtime flag, so the sandbox image is unchanged by this work.

It is passed on EVERY display rather than only for a playtest — one code
path rather than two. The cost is 5 MiB of the container's own tmpfs per
display (1280x1024x32 plus a 3232-byte XWD header), against a measured
whole-run peak of 1.22 GiB at two concurrent displays, and it is the same
pages the server would otherwise have held anonymously.

The file is X's own XWD, decoded in Go rather than converted by a tool
(`playtest_capture_test.go`). That is not a workaround for the missing tool:
the geometry check and the blank-frame floor are read off the decoded pixels
anyway, so a converter would only have added a dependency between two things
the harness already does. Masks, byte order and the colormap are READ from
the file, and every shape the decoder does not handle is refused loudly.

### Geometry

**1280x1024, fixed**, and DERIVED from `xvfbScreen` rather than restated, so
the screen the display is started at and the geometry the manifest declares
cannot drift apart. Every playbook sizes its Emacs frame to fill it before
anything is drawn, so the pictures compare run to run and the webview is laid
out at the size it is photographed at.

### The blank-frame floor

Measured, at that geometry:

| what | distinct colors |
| --- | --- |
| an Xvfb with nothing on it | 1 |
| Emacs with a blank webview | 1485 |
| Emacs with the page painted | 1647 |

The floor is **64 distinct colors** — 23x below the weakest drawn frame ever
observed and 64x above the blank one, so it is a floor rather than a
threshold anyone has to tune. It catches the failure that would otherwise
waste a whole review (a frame that never appeared, an Xvfb that died, a
capture from the wrong screen) and claims nothing about WHAT was drawn.
Pixel-golden comparison is not attempted anywhere: it is brittle against font
hinting, a scrollbar, a clock, and every other honest difference between two
runs of the same product.

### A capture is of a settled screen, of a redrawn one, and of the CURRENT DOM

Three things happen before the picture is taken, in this order: the frame is
redrawn, the page's own frames are waited for and the frame is redrawn again,
and only then is the framebuffer read until it settles. They are listed here
smallest first.

- **A redisplay is forced.** A user's Emacs redisplays constantly because a
  user generates events; this one is driven entirely over the server socket
  and sits on an Xvfb nobody types into, so between two evals it can be
  several state changes behind what it has DRAWN. Measured: the first
  tab-bar captures showed no tab at all after a workspace was registered.
  A capture asks for the redraw a user's keystrokes would have asked for.
  It only makes Emacs draw what it already decided — a tab bar still wrong
  afterwards is wrong in the product, and one is (see below).
- **The framebuffer is read, a full redisplay apart, until it has held still
  for a whole 50ms window.** A screenshot of a frame mid-redraw is half of
  one state and half of another. Two agreeing reads are not enough for that,
  twice over:

  - Reads with nothing driven between them agree trivially about a screen
    Emacs has simply not repainted. Measured, on the tab bar after a roster
    push opened a new tab: with the redraws above already done, the bar still
    showed the tab set from BEFORE the push in three registrations of four,
    and one more `(redraw-frame) (redisplay t)` showed the new one every
    time. So a full redisplay sits between the reads, each read follows an
    eval of its own with the poll interval between them — on pgtk the pixels
    reach the X server only when GTK's main loop runs, which is after an eval
    has answered and never inside it — and the round count is logged.
  - Reads a few milliseconds apart agree about a frame still ARRIVING at the
    X server. Measured, in two real runs: `04-arm-link-severed` was declared
    settled at 2ms carrying the webview alone with every piece of Emacs's own
    chrome blank white, and `04-arm-detached-settled` at 5ms with a correctly
    green tab bar over a webview still showing the previous state. Both were
    whole on the glass about 22ms later. So the window is 50ms — three
    periods of a 60Hz display frame — and any change restarts it.

  This is a PATIENCE BUDGET rather than an assertion: a surface that
  is genuinely animating never settles, and refusing to photograph it would
  refuse exactly the states a playtest exists to show — so a capture that
  runs out its budget takes the last read and SAYS SO in the manifest. The
  cursor's own blink is switched off in the playbook's setup rather than
  waited out, because a blinking cursor alone would make every capture run
  its whole budget.

  The window and the paint gate below are not the same gate wearing two
  names. The gate proves the PAGE produced a frame for the DOM the step
  asserted; the window proves the SCREEN then stopped changing. The blank
  chrome above was Emacs's own drawing in flight, which no page-side gate can
  see.
- **The page's own frames are waited for, between the two redraws.** An
  `xwidget-webkit` webview on X is OFFSCREEN-RENDERED: WebKit paints into a
  GTK offscreen surface, and those pixels reach the glass only when Emacs's
  redisplay copies that surface while drawing the xwidget's glyph. So a
  redraw copies whatever the surface held at that instant, and if WebKit has
  taken the DOM change but not yet produced a frame for it, the picture is of
  the PREVIOUS page state under an assertion that legitimately passed —
  while the frame WebKit produces a moment later raises a damage signal whose
  INCREMENTAL redisplay swaps in a buffer carrying a current webview and none
  of the chrome. Both were observed, in three runs of one owner's section: an
  empty feed whose DOM held two settled bubbles, and a current webview with
  no tab bar and no mode lines. Neither is a torn frame, which is why the
  settle never caught them — the stale screen is perfectly still, so two
  reads agree and the capture reports `settled=true` on a lie.

  So a capture drives `requestAnimationFrame` twice through the page probe,
  keyed by a token minted after the redraw was forced, and waits on the
  layer's own `AwaitEval` polling until the page reports both frames
  delivered. Two is the smallest count that proves anything: the first
  callback runs BEFORE that frame is painted. Measured across owner 7's three
  playbooks and this one, over three consecutive runs — twenty-one gates —
  the gate answered in 44–144ms, mean 76ms, so a capture costs about a
  fourteenth of its own 2s settle budget more than it did, and a whole
  playbook a fraction of a second.
  A workspace with no live webview answers its own word and the wait accepts
  it, because several playbooks photograph a frame with no page in it.

  `playtest_00_feed_tail_test.go`'s last step is this mechanism photographing
  itself: the page appends a full-viewport magenta region, the DOM says so, a
  capture is taken immediately, and the decoded pixels are counted —
  957,676 of them exactly magenta with the gate, and zero without it.

## Driving the page

The webapp exposes no readiness flag — no `data-ready`, no global — so
nothing waits on one that does not exist. The page is live when the footer's
status word is non-empty, which is the same signal the webapp layer's own
suite uses: it is empty until the daemon's `WatchFooter` push has arrived and
been rendered. Every other in-page wait is on a `data-*` hook that suite
already asserts.

`xwidget-webkit-execute-script` is ASYNCHRONOUS, so a probe is two evals:
each call issues the script again and answers what the previous issue's
callback stored, and the Go side polls it through the layer's own
`AwaitEval`. That converges within one poll and never sleeps. A probe that
answers "no" carries its own diagnosis — the page's url, its feed-row count,
its failure cards and the head of its text — which is how the boot defect
below was found from a wait that merely timed out.

## What a reviewer does

Each playbook writes `<artifacts>/playtest/<playbook>/MANIFEST.md` beside its
PNGs. The manifest's row per step carries the act, the assertion that PASSED,
the image if there is one, and the sentence the picture must match.

**The tab-arm sentences are the module's own decision, not this file's guess
at it.** `captureArm` re-reads the arm at the instant of the capture, refuses
if it has moved off the arm the step is about, and writes the sentence from
`agent-repl-status-color-table` and `agent-repl--color-by-name` — the two
tables `AGENTS.md` names as the one source for tab coloring. So "the tab must
carry a RED disc" is what the product decided a millisecond earlier, and a
picture that disagrees is a defect in the paint rather than in the sentence.

The mechanical gate is green when every capture exists, is a valid PNG of the
declared geometry, and is not blank. The visual gate is a vision-capable read
of the PNGs against the manifest. A capture whose picture does not match its
sentence is a defect.

## What this has already found

Three defects, all invisible to every existing suite because no suite before
this one looked at the running application.

1. **The webapp could not boot at all.** `log()` refuses to emit without an
   installed sink, and both `shellElements` and `mountFailureOverlay` log as
   their first statement — and both ran before `setLogger`. Every page threw
   out of `boot` on its own first log line with `overlay` still null, so the
   failure went to `console.error` and the page came up EMPTY AND SILENT.
   FIXED, with `webapp/test/main-boot-logger.test.ts` pinning it.

2. **FIXED — the root feed's live tail never opened.** `OpenFeed` succeeds and its
   page paints, but `WatchFeed` is never issued: the daemon logs
   `daemon.feed.open_page` once and never a `WatchFeed` stream, while it
   publishes the rows. So every row produced after the page is invisible
   until the page is reloaded — proved by driving `location.reload()` in the
   same webview after a turn settled, which draws both bubbles within 120ms.
   The failure overlay stays empty, so `watchStream` never saw the stream
   end. ROOT CAUSE: a browser caps a host at about six HTTP/1.1 connections
   and a server-streaming Connect call pins one for its whole life; the page
   opened six before the feed's, so `WatchFeed` was the seventh and queued in
   the browser forever. Measured from inside the live page: streams 1-6
   reached the daemon within 9ms, streams 7-9 produced no daemon record at
   all, and a plain same-origin GET with six held timed out after 5s.
   Fixed by the `WatchPage` mux — every subscription rides one stream, so the
   failure is unrepresentable rather than unlikely — and proved in the real
   webview by `playtest_00_feed_tail_test.go`.

3. **The tab bar does not follow the roster.** After registering one
   workspace the bar draws NO tab, while `agent-repl--ws-tabline-names`
   already reports it; after registering a second, the bar draws the FIRST
   one and an empty cell; and with `agent-repl-roster-status-for-ws` reading
   `:thinking` at the instant of the capture — which the module's own color
   table paints RED — the tab is painted green. All three survive a forced
   redisplay. OPEN.

## The substrate's own precondition

Sixteen of the plan's twenty owners photograph things the webapp draws in its
FEED, so all of them rest on one precondition: the feed's live tail is
served. `playtest_00_feed_tail_test.go` is that precondition's own playbook —
not an owner's — and it proves in the real `xwidget-webkit` webview that the
sidebar, topbar, hold tray and footer all draw, that the root feed tails live
rows with NO reload, and that an expanded subagent bubble's own nested feed
carries rows too. That last one is the case no fixed connection budget could
ever have held, since every expanded bubble opens another tail.

It is a permanent playbook rather than a one-off check: it is the thing that
would go red first if the page ever again opens a stream off the client
instead of the mux.

## One open question this raised, half of it since answered

Clicking a subagent bubble's `[data-expand]` caret was reported here as not
making the bubble read as open: `data-expanded` never became true and the
bubble was *photographed still drawn collapsed*, while a non-root feed
container WAS carrying rows.

The photographed half was the capture's own staleness, not the product. With
the paint gate above, the same step's picture shows the bubble drawn OPEN
with its nested rows beneath the head. What remains open is only which
attribute carries that state — `data-expanded` is not it — which is the
webapp's own business in its bubble rather than anything about the
transport. It stays recorded in `00-feed-tail`'s manifest rather than
asserted, and owner 16 (G49–52, subagents) inherits that remainder.
