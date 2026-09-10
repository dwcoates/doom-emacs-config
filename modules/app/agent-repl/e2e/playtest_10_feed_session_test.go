//go:build playtest

package e2e

import (
	"fmt"
	"testing"
)

// OWNER 10 of PLAYTEST-PLAN.md's partition: D29-D32 -- the slash-command
// family, the compaction family, the vendor's unsolicited model fallback, and
// the three fast-mode states.
//
// ONE WORLD, ONE TABLE, ONE LOOP, which is the plan's own rule for sections
// D-H ("must be ONE loop each, not hand-written per scenario"). Every row
// below is one scripted user act -- a prompt typed into the composer and
// submitted with RET -- against the SAME Emacs, the same daemon and the same
// session, so the picture a row produces also carries every row before it and
// a reviewer reads the session's whole history in the last frame.
//
// WHY ONE SESSION AND NOT ONE PER ROW. Three of these families are STANDING
// SESSION FACTS rather than turn events: fast mode sticks, the model fallback
// sticks, and a context cut is a divider in the history it cut. A world per
// row would photograph each of them on a session that had never done anything
// else, which is the one arrangement that cannot show a fact STICKING.
//
// THE ROWS ARE ORDERED, and the order is the plan's: 29 slash, 30 compaction,
// 31 model fallback, 32 fast mode. Nothing in a later row disturbs an earlier
// row's evidence -- the compaction dividers stay in the feed, the fallback
// model stays selected, and the fast-mode cell is the only thing the last
// three rows move.

// ---------------------------------------------------------------------------
// THE EXACT STRINGS, taken from the suites that own them
// ---------------------------------------------------------------------------

// The answering prose each slash scenario concludes with, copied from the Go
// e2e suite that owns those assertions (`slashcommands_e2e_test.go`
// TestVendorAnsweredSlashCommand, TestSlashShapeANamed, TestSlashShapeAUnnamed)
// rather than restated from the fake's source, so the two cannot drift into
// disagreeing about the same turn.
const (
	pt10SlashProse         = "Answered the slash command locally."
	pt10SlashShapeANamed   = "Recorded the Shape-A bookkeeping for /merge."
	pt10SlashShapeAUnnamed = "Recorded the withheld-unnamed bookkeeping."
)

// The compaction family's own strings, from `compaction_e2e_test.go`
// (TestCompactionDirected, TestCompactionAuto, TestCompactionFailed).
const (
	pt10CompactSummary     = "Compacted the conversation."
	pt10CompactAutoSummary = "The conversation was compacted automatically."
	pt10CompactFailedError = "the summarizing request was rejected"
)

// The two wordings a compacted divider's own label opens with, from the daemon
// that composes them (`daemon/internal/resolve/feed/separation.go`
// compactionLabel) and pinned end to end by `compaction_e2e_test.go`
// TestCompactionDirected and TestCompactionAuto.
//
// THE TRIGGER IS ON THE GLASS, and this playbook once said it was not. The
// divider carries no trigger FIELD -- FeedContextCutCompacted has none -- so
// the manifest sentence claimed a reader could not tell a compaction they asked
// for from one that happened to them. The picture says otherwise: the daemon
// composes the distinction into `label.text` precisely because, in its own
// words, "drawing the two identically is the most misleading thing this divider
// can do". So the two wordings are asserted here, one divider each.
const (
	pt10CompactRequestedLabel = "context compacted on request"
	pt10CompactAutomaticLabel = "context compacted automatically"
)

// The fast-mode family's own conclusion prose, composed by the fake verbatim
// into both the assistant block and `result.result`
// (`agent-shim/claude/shim/src/fake/scenarios/session.ts` fastModeScenario),
// and asserted by `turnlifecycle_e2e_test.go` TestFastMode for the `on` arm.
const (
	pt10FastOnProse       = "Fast mode is on."
	pt10FastOffProse      = "Fast mode is off."
	pt10FastCooldownProse = "Fast mode is cooldown."
)

// The labels the fast-mode cell draws, from `webapp/src/topbar/fast-mode.ts`
// FAST_MODE_LABELS. They are the whole subject of D32: `cooldown` is
// deliberately NOT folded into `off`, because off is a setting somebody chose
// and cooldown is the vendor saying "not right now".
const (
	pt10FastLabelOn       = "fast"
	pt10FastLabelOff      = "fast off"
	pt10FastLabelCooldown = "fast cooling"
)

// pt10FallbackModelDisplayName is what the model selector's button DRAWS once
// the vendor has swapped the session onto its fallback model.
//
// THE DRAWN TEXT IS A DISPLAY NAME, NOT THE MODEL NAME, and this constant is
// deliberately the former. `!model-fallback` swaps a default-model session
// (`fake-opus-4-8`) onto `fake-sonnet-5`
// (`fake/scenarios/session.ts` MODEL_FALLBACK), and that model IS in the
// fake's catalog (`fake/catalogs.ts`) carrying `displayName: "Fake Sonnet"` --
// so `drawTopbarModelSelector` draws `u.selected.displayName`
// (`webapp/src/topbar/model.ts`) and the string on the glass is "Fake Sonnet".
// Asserting `fake-sonnet-5` on the button would assert something the product
// does not draw and never has.
//
// It is still an EXACT assertion of the swap rather than a weakened one: the
// session's own default model is the catalog's `Fake Opus` row, so "Fake
// Sonnet" on that button is reachable only through the fallback.
//
// THE MISSING HOOK THIS ROW ORIGINALLY REPORTED IS NOW LANDED, and the row
// asserts it too: `webapp/src/topbar/model.ts` SELECTED_MODEL_ATTRIBUTE puts
// `data-model` on the `.topbar-model` CONTROL (the wrap, not the button)
// carrying `AgentModel.name` -- the same echo token `[data-model-option]`
// carries on the offered rows. So the strip now names the model in force in a
// vocabulary a reader can check, and D31 pins BOTH: the display name a human
// sees and the model name underneath it.
const pt10FallbackModelDisplayName = "Fake Sonnet"

// pt10FallbackModelName is the model on the WIRE, which `data-model` carries.
const pt10FallbackModelName = "fake-sonnet-5"

// pt10DefaultModelDisplayName is the display name the SAME button carries
// before the fallback -- the catalog row for `FAKE_DEFAULT_MODEL`. It is named
// so the fallback row's manifest sentence can say what the picture must NOT
// still read.
const pt10DefaultModelDisplayName = "Fake Opus"

// pt10FallbackConclusion is the prose the fallback turn concludes with, from
// `fake/scenarios/session.ts` MODEL_FALLBACK's own `conclude` call. It names
// the model on the WIRE -- `fake-sonnet-5` -- which is the one place in this
// row's evidence that literal appears at all, the strip drawing the catalog's
// display name for it instead.
const pt10FallbackConclusion = "Answered on fake-sonnet-5 after the fallback."

// ---------------------------------------------------------------------------
// THE PAGE PREDICATES
// ---------------------------------------------------------------------------

// pt10SettledResponseWithProse is the predicate for "the answering response
// bubble settled, carrying exactly this prose".
//
// It is scoped to a bubble in the SUCCESS state, which is `data-state` mirrored
// off `FeedTurnActivityResponse`'s own result arm
// (`webapp/src/feed/cards/response.ts`), so a response still streaming cannot
// satisfy it. The prose is matched against the bubble's rendered text because
// the daemon ships markdown and the bubble renders it -- every string here is
// plain prose, so the rendered text carries it verbatim.
//
// EVERY ROW'S PROSE IS DISTINCT, which is what makes a shared session safe:
// the predicate cannot be satisfied by a bubble an earlier row left behind.
func pt10SettledResponseWithProse(prose string) string {
	return `Array.prototype.some.call(
                  document.querySelectorAll('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]'),
                  function (bubble) { return bubble.textContent.indexOf(` + jsString(prose) + `) !== -1; })`
}

// pt10UserPromptWithText is the predicate for "the user's own prompt bubble
// for THIS row arrived on the standing tail".
//
// It is the first thing every row asserts, and it is the assertion that
// separates "the prompt never reached the daemon" from "the turn produced the
// wrong thing" -- two failures a wait on the response alone would report
// identically.
func pt10UserPromptWithText(text string) string {
	return `Array.prototype.some.call(
                  document.querySelectorAll('[data-feed-row][data-row-kind="userPrompt"]'),
                  function (bubble) { return bubble.textContent.indexOf(` + jsString(text) + `) !== -1; })`
}

// pt10CompactedSeparations is the predicate for "the feed carries EXACTLY N
// compacted context-cut dividers".
//
// EXACT, NOT "AT LEAST", AND THAT WAS BOUGHT WITH A DEFECT. An `>=` count
// passed a page that drew EVERY successful compaction TWICE -- two identical
// dividers per cut, the second one opening onto an empty summary -- so the
// assertion agreed with a picture no reader would accept. An exact count is
// the one that fails on a duplicate, and a lower bound is the one that cannot.
//
// COUNTED, NOT POSITIONED. Two rows of this table compact, and the second
// cannot be told from the first by any attribute the divider carries -- both
// are `[data-row-kind="separation"][data-state="compacted"]`, and their
// summaries are folded away by default (`separation.ts` initialFold, off the
// wire's own `folded`). So the second compaction's own evidence is that the
// feed now holds TWO of them, which is a fact about the feed rather than about
// where a row landed in it.
func pt10CompactedSeparations(exactly int) string {
	return fmt.Sprintf(
		`document.querySelectorAll('[data-feed-row][data-row-kind="separation"][data-state="compacted"] .sep-compacted').length === %d`,
		exactly)
}

// pt10CompactedLabelsOpeningWith is the predicate for "EXACTLY N compacted
// dividers open their label with this wording".
//
// COUNTED AND EXACT, for the same reason `pt10CompactedSeparations` is: a
// lower bound cannot see a divider drawn twice, and this predicate is the one
// that says WHICH cut each drawn divider is about. `.sep-label` carries the
// daemon's composed text and then the size change appended beside it
// (`webapp/src/feed/rows/separation.ts` drawFeedSessionSeparationLabel), so the
// wording is asserted as the label's OPENING rather than as its whole text.
func pt10CompactedLabelsOpeningWith(opening string, exactly int) string {
	return fmt.Sprintf(`Array.prototype.filter.call(
                  document.querySelectorAll('[data-feed-row][data-row-kind="separation"][data-state="compacted"] .sep-label'),
                  function (label) { return label.textContent.trim().indexOf(%s) === 0; }).length === %d`,
		jsString(opening), exactly)
}

// pt10SummariesAllCarryText is the predicate for "every compaction summary
// drawn on this page says something".
//
// IT IS A NEGATIVE, AND IT EXISTS BECAUSE A PICTURE CAUGHT ONE. An EMPTY
// summary bubble under a divider is a divider drawn for a compaction whose
// account never arrived: the reader is offered a fold that opens onto nothing.
// A COUNT OF DIVIDERS CANNOT SEE THAT -- the empty one is a well-formed row --
// so the text is asserted directly, across every summary on the page rather
// than the newest, because it is the DUPLICATE that came up empty.
const pt10SummariesAllCarryText = `Array.prototype.every.call(
                  document.querySelectorAll('.sep-compacted .sep-summary'),
                  function (summary) { return summary.textContent.trim() !== ""; })`

// pt10ContextFigureDrawn is the predicate for "the topbar's context budget
// figure is drawn and says something".
//
// `.topbar-context-figure` is the hook the webapp's own DOM contract names for
// it (`webapp/src/topbar/context-chip.ts`), and the daemon fills it for every
// ready workspace (`daemon/internal/resolve/topbar/resolver.go` contextChip
// -> `TopbarContextChip.text`), so a compaction that moved the context is
// drawn HERE and nowhere else on the strip. The figure is the daemon's own
// formatted text; this asserts it is present rather than what number it is,
// because the number is the vendor's and the playtest scripts no context size.
const pt10ContextFigureDrawn = `document.querySelector(".topbar-context-figure") &&
         document.querySelector(".topbar-context-figure").textContent.trim() !== ""`

// pt10FastCell is the predicate for the fast-mode cell in one named state,
// carrying that state's own label.
//
// BOTH HALVES, AND THE LABEL IS THE POINT. The arm alone would pass on a cell
// that drew cooldown with off's words, which is the exact confusion
// `fast-mode.ts` says it exists to avoid ("Drawing them the same would invite
// a reader to go looking for a switch that cannot take effect").
// THE SELECTOR IS BUILT IN GO AND QUOTED ONCE, which is not a style choice:
// splicing `jsString(state)` INTO an already-quoted selector literal produced
// `'.topbar-fast[data-fast-mode='on']'` -- a syntax error that made every
// issue of the script answer null, so the wait failed with "last value was
// null" rather than with anything about the page. A predicate that cannot
// parse diagnoses nothing, so the whole selector is one Go string handed to
// `jsString` exactly once.
func pt10FastCell(state, label string) string {
	selector := `.topbar-fast[data-fast-mode="` + state + `"]`
	return `(function () {
                  var cell = document.querySelector(` + jsString(selector) + `);
                  return cell !== null && cell.textContent.trim() === ` + jsString(label) + `;
                })()`
}

// pt10ModelButtonReads is the predicate for the model selector's button
// carrying exactly one display name, on a button that has a selection at all.
//
// `data-unselected` is checked too rather than left implied: an unselected
// button draws the `MODEL_PLACEHOLDER` word, and a page that lost its
// selection entirely would otherwise be diagnosed as a wrong model rather than
// as no model.
func pt10ModelButtonReads(displayName string) string {
	return `(function () {
                  var button = document.querySelector(".topbar-model-button");
                  return button !== null &&
                         !button.hasAttribute("data-unselected") &&
                         button.textContent.trim() === ` + jsString(displayName) + `;
                })()`
}

// ---------------------------------------------------------------------------
// THE TABLE
// ---------------------------------------------------------------------------

// pt10Wait is one programmatic in-page assertion a row makes: what is being
// waited for, in words a failure prints, and the predicate that decides it.
type pt10Wait struct {
	what       string
	expression string
}

// pt10Row is one act in the playbook: a prompt, the assertions the act must
// satisfy before any picture is taken, and the sentence a reviewer holds the
// picture to.
type pt10Row struct {
	// name is the capture's own name under the playbook's artifact directory.
	name string
	// plan is the PLAYTEST-PLAN.md playbook this row belongs to, recorded in
	// the manifest so a reviewer reads the plan's own numbering.
	plan string
	// prompt is what the user types into the composer.
	prompt string
	// settled is the answering prose the turn must conclude with, asserted on
	// the DRAWN bubble. Empty means this row's subject is not the prose.
	settled string
	// waits are the row's remaining in-page assertions, made after the turn
	// has settled.
	waits []pt10Wait
	// asserted is the manifest's account of what the row PROVED
	// programmatically, and expected is the sentence the picture must match.
	asserted string
	expected string
}

// TestPlaytestFeedSession is D29-D32.
func TestPlaytestFeedSession(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "10-feed-session",
		"Plan D29-D32. The slash-command family, the compaction family, the vendor's unsolicited model "+
			"fallback and the three fast-mode states -- every one of them driven through the composer "+
			"against ONE session, so a standing session fact is photographed on a session that has a "+
			"history rather than on a fresh one.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with its panel opened",
		"the webapp mounted its feed host, drew its footer status word off the daemon's own push, and "+
			"filed nothing on the failure overlay")

	rows := []pt10Row{
		// -------------------------------------------------------------------
		// D29 -- the slash-command family.
		//
		// THERE IS NO SLASH FEED ROW, and that is the contract rather than a
		// gap in this table. `slashcommands_e2e_test.go`'s own header settles
		// it off frontend/v1/failure.proto and docs/overhaul/daemon.md: a
		// slash command the vendor answers itself is entry-less
		// vendor-specific residue, which "has no feed-row arm of its own -- so
		// a slash command the vendor answers itself surfaces on the wire only
		// as an ORDINARY concluded turn, never as a distinct unit". The two
		// Shape-A scenarios are FILE-PLANE ONLY for their bookkeeping write
		// and produce no SessionUpdate at all. So the row the webapp draws for
		// a slash record is the answering RESPONSE bubble, and that is what
		// these three rows assert -- each on its own exact prose, which is the
		// only thing that distinguishes the three turns on the glass.
		// -------------------------------------------------------------------
		{
			name:    "slash-vendor-answered",
			plan:    "D29",
			prompt:  "!slash",
			settled: pt10SlashProse,
			asserted: "the user's own prompt bubble arrived on the standing tail, the roster arm ran and " +
				"settled, and a `[data-unit=\"response\"][data-state=\"success\"]` bubble carries exactly " +
				"the prose `" + pt10SlashProse + "`",
			expected: "The feed carries the `!slash` PROMPT BUBBLE and, beneath it, an assistant bubble reading " +
				"\"" + pt10SlashProse + "\". THERE IS NO SEPARATE SLASH ROW and there must not be: the " +
				"vendor answered the command itself, which the contract draws as an ordinary concluded " +
				"turn. The answering prose IS the whole surface of the slash record.",
		},
		{
			// `merge` rather than the scenario's own `compact` default,
			// following TestSlashShapeANamed: a command name chosen to prove
			// the name is genuinely a parameter.
			name:    "slash-shape-a-named",
			plan:    "D29",
			prompt:  "!slash-shape-a merge",
			settled: pt10SlashShapeANamed,
			asserted: "the prompt bubble arrived, the turn settled, and the answering bubble carries exactly " +
				"`" + pt10SlashShapeANamed + "` -- the conclusion `slashcommands_e2e_test.go` " +
				"TestSlashShapeANamed pins for the same scenario",
			expected: "A THIRD pair of bubbles: the `!slash-shape-a merge` prompt and an assistant bubble " +
				"reading \"" + pt10SlashShapeANamed + "\", naming the command that was passed. " +
				"The CLI's own bookkeeping record is a file-plane write with no wire signal, so again " +
				"NOTHING but the prose is drawn for it. The `!slash` pair is still above.",
		},
		{
			name:    "slash-shape-a-unnamed",
			plan:    "D29",
			prompt:  "!slash-shape-a-unnamed",
			settled: pt10SlashShapeAUnnamed,
			asserted: "the prompt bubble arrived, the turn settled, and the answering bubble carries exactly " +
				"`" + pt10SlashShapeAUnnamed + "`",
			expected: "The unnamed counterpart: a prompt bubble and an assistant bubble reading " +
				"\"" + pt10SlashShapeAUnnamed + "\", NAMING NO COMMAND -- the negative the named row " +
				"exists beside. Three slash pairs are now in the feed, in order.",
		},

		// -------------------------------------------------------------------
		// D30 -- the compaction family.
		//
		// The divider is the subject, and the context budget beside it. The
		// summary is folded on a first draw (the wire's own `folded`), so the
		// picture shows a rule, a label with the size change on it, and a
		// closed "summary" toggle -- which is what a reader gets, and
		// therefore what the manifest asks for.
		// -------------------------------------------------------------------
		{
			name:    "compact-directed",
			plan:    "D30",
			prompt:  "!compact",
			settled: pt10CompactSummary,
			waits: []pt10Wait{
				{"exactly ONE compacted context-cut divider to be drawn in the feed", pt10CompactedSeparations(1)},
				{
					"the divider's own label to say the compaction was ASKED FOR",
					pt10CompactedLabelsOpeningWith(pt10CompactRequestedLabel, 1),
				},
				{"the compaction summary to say something rather than open onto nothing", pt10SummariesAllCarryText},
				{"the topbar's context budget figure to be drawn", pt10ContextFigureDrawn},
			},
			asserted: "a `[data-row-kind=\"separation\"][data-state=\"compacted\"]` row holding " +
				"`.sep-compacted` is in the feed, its `.sep-label` opens with `" + pt10CompactRequestedLabel +
				"`, `.topbar-context-figure` carries a figure, and the turn concluded with " +
				"`" + pt10CompactSummary + "`",
			expected: "A CONTEXT-CUT DIVIDER is drawn across the feed beneath the slash pairs: a coloured rule " +
				"with a centred muted label under it READING `" + pt10CompactRequestedLabel + " · took …` " +
				"-- the label says the compaction was ASKED FOR -- carrying the size change beside it as " +
				"`before → after`, and a closed `▸ summary` toggle (the summary is folded on " +
				"a first draw). THE TOPBAR CARRIES A CONTEXT FIGURE at its right, and it is YELLOW -- " +
				"the one coloured number in the strip, and the one thing on it a reader is meant to find " +
				"without looking. A GREY figure there is a DEFECT: it was one, and the picture is what " +
				"caught it. The divider must be VISIBLE, not merely present.",
		},
		{
			name:    "compact-auto",
			plan:    "D30",
			prompt:  "!compact-auto",
			settled: pt10CompactAutoSummary,
			waits: []pt10Wait{
				{"exactly TWO compacted dividers, the first still standing and neither of them doubled", pt10CompactedSeparations(2)},
				{
					// THE TWO CUTS ARE TOLD APART, and one each is the whole
					// assertion: a divider that took the other's wording would
					// pass a count and lie to the reader.
					"exactly ONE of the two dividers to still say the compaction was ASKED FOR",
					pt10CompactedLabelsOpeningWith(pt10CompactRequestedLabel, 1),
				},
				{
					"exactly ONE of them to say this one happened ON ITS OWN",
					pt10CompactedLabelsOpeningWith(pt10CompactAutomaticLabel, 1),
				},
				{"both compaction summaries to say something rather than open onto nothing", pt10SummariesAllCarryText},
				{"the topbar's context budget figure to still be drawn", pt10ContextFigureDrawn},
			},
			asserted: "the feed now holds TWO compacted separation rows rather than one, ONE `.sep-label` " +
				"opening with `" + pt10CompactRequestedLabel + "` and ONE with `" + pt10CompactAutomaticLabel +
				"`, the topbar still draws its context figure, and the turn concluded with " +
				"`" + pt10CompactAutoSummary + "`",
			expected: "EXACTLY TWO context-cut dividers are visible -- ONE PER CUT -- with the auto-compaction's " +
				"turn between them, each with its own rule, label and CLOSED summary toggle, and NOTHING " +
				"drawn open beneath either toggle. FOUR dividers here -- each cut drawn twice -- or a " +
				"fold that opens onto an EMPTY bubble, is a DEFECT and not a variation. The topbar " +
				"still carries its context figure, in yellow. THE TWO DIVIDERS DO NOT READ THE SAME: the " +
				"upper one says `" + pt10CompactRequestedLabel + "` and the lower one says " +
				"`" + pt10CompactAutomaticLabel + "`. That distinction is the point of the picture -- a " +
				"compaction the reader ASKED FOR and one that happened to them are not the same event, " +
				"and two dividers worded alike would be the most misleading thing this row could draw. " +
				"(This sentence once said the trigger was NOT on the glass. It is; the sentence was wrong " +
				"and the product was right.)",
		},
		{
			name:   "compact-failed",
			plan:   "D30",
			prompt: "!compact-failed",
			waits: []pt10Wait{
				{
					"the compaction-failed divider to be drawn carrying the vendor's own rejection wording",
					`(function () {
                       var failed = document.querySelector('[data-feed-row][data-row-kind="separation"][data-state="compactionFailed"] [data-compaction-failed="true"]');
                       return failed !== null && failed.textContent.trim() === ` + jsString(pt10CompactFailedError) + `;
                     })()`,
				},
				{
					// NOTHING WAS CUT, so there is no size change to draw
					// beside this label -- the wire leaves `tokens` unset and
					// `separation.ts` draws no figure for an absent one. The
					// negative is asserted because the whole difference
					// between this divider and the two above it is that this
					// one reports a compaction that DID NOT HAPPEN.
					"the failed divider to carry NO size change, nothing having been cut",
					`document.querySelectorAll('[data-feed-row][data-row-kind="separation"][data-state="compactionFailed"] .sep-tokens').length === 0`,
				},
				{"the two successful dividers to still be standing, still undoubled", pt10CompactedSeparations(2)},
				{"their summaries to still say something", pt10SummariesAllCarryText},
			},
			asserted: "a `[data-state=\"compactionFailed\"]` separation row carries exactly " +
				"`" + pt10CompactFailedError + "`, carries NO `.sep-tokens` figure, and the two " +
				"compacted dividers above it are untouched",
			expected: "A THIRD DIVIDER, and it is drawn in the slot a compacted one would have taken -- the same " +
				"rule geometry -- but it reports a compaction that DID NOT HAPPEN: it states " +
				"\"" + pt10CompactFailedError + "\" and its label carries NO `before → after` size " +
				"change, because nothing was cut. Its rule takes the failed accent rather than the " +
				"compacted one, so it must not be mistakable for the two above it. There is no folded " +
				"summary on it: there is no summary.",
		},

		// -------------------------------------------------------------------
		// D31 -- the vendor's unsolicited model fallback.
		//
		// Nothing called SetSessionModel; the vendor swapped the model on its
		// own and said so in a `system:model_refusal_fallback` record, which
		// the shim folds into the ONE authoritative fact
		// (`engine/session.ts`: "SessionUpdate.model_changed is stated by
		// SetSessionModel, or by the vendor ... so one place is
		// authoritative"). The topbar's own button is where that lands.
		// -------------------------------------------------------------------
		{
			name:    "model-fallback",
			plan:    "D31",
			prompt:  "!model-fallback",
			settled: pt10FallbackConclusion,
			waits: []pt10Wait{
				{
					"the topbar's model selector to name the fallback model the vendor swapped to",
					pt10ModelButtonReads(pt10FallbackModelDisplayName),
				},
				{
					// THE NAME, NOT THE LABEL. The button's text is a display
					// name and two catalog rows may share one, so this is the
					// assertion that actually identifies the model in force.
					"the model control to carry the fallback model's own NAME, not only its display label",
					`(function () {
                       var control = document.querySelector(".topbar-model");
                       return control !== null &&
                              control.getAttribute("data-model") === ` + jsString(pt10FallbackModelName) + `;
                     })()`,
				},
			},
			asserted: "`.topbar-model-button` reads exactly `" + pt10FallbackModelDisplayName + "` and carries " +
				"no `data-unselected`, AND `.topbar-model` carries `data-model=\"" + pt10FallbackModelName +
				"\"` -- the model `TestModelChanged` pins for this scenario, named on the strip in the " +
				"same vocabulary the offered rows use, and reachable from this session's own default " +
				"(`" + pt10DefaultModelDisplayName + "`) only through the fallback",
			expected: "THE TOPBAR'S MODEL SELECTOR NOW NAMES THE FALLBACK MODEL: its button reads " +
				"\"" + pt10FallbackModelDisplayName + "\" and must NOT still read " +
				"\"" + pt10DefaultModelDisplayName + "\", which is what the session started on. " +
				"The feed carries the `!model-fallback` prompt and the answer stating the swap. " +
				"WHAT THE BUTTON DRAWS IS THE CATALOG'S DISPLAY NAME AND NOT THE MODEL NAME: the wire " +
				"carries `" + pt10FallbackModelName + "`, the catalog gives that model the display " +
				"name \"" + pt10FallbackModelDisplayName + "\", and the button draws the display name. " +
				"So a reviewer will NOT see the literal `" + pt10FallbackModelName + "` anywhere on " +
				"the strip, and must not treat its absence as a defect -- the name is carried as the " +
				"control's own `data-model` attribute, which this row asserts and which no picture can " +
				"show.",
		},

		// -------------------------------------------------------------------
		// D32 -- the three fast-mode states, one capture each.
		//
		// The cell is a LABEL and never a control: nothing on the contract sets
		// fast mode. So each row's evidence is the arm the vendor stated and
		// the words the cell chose for it, and the whole subject is that
		// COOLDOWN IS NOT DRAWN AS OFF.
		// -------------------------------------------------------------------
		{
			name:    "fast-on",
			plan:    "D32",
			prompt:  "!fast-on",
			settled: pt10FastOnProse,
			waits: []pt10Wait{
				{"the topbar's fast-mode cell to read the ON state", pt10FastCell("on", pt10FastLabelOn)},
			},
			asserted: "`.topbar-fast[data-fast-mode=\"on\"]` is drawn and its label is exactly " +
				"`" + pt10FastLabelOn + "`, beside the conclusion `" + pt10FastOnProse + "` that says " +
				"which state the vendor reported",
			expected: "THE TOPBAR CARRIES A FAST-MODE CELL READING \"fast\", in the strip's ordinary " +
				"foreground rather than the muted grey of the cells around it -- on is the one state " +
				"that changes what sending a prompt does, and it is the only one given any emphasis.",
		},
		{
			name:    "fast-off",
			plan:    "D32",
			prompt:  "!fast-off",
			settled: pt10FastOffProse,
			waits: []pt10Wait{
				{"the topbar's fast-mode cell to read the OFF state", pt10FastCell("off", pt10FastLabelOff)},
			},
			asserted: "`.topbar-fast[data-fast-mode=\"off\"]` is drawn and its label is exactly " +
				"`" + pt10FastLabelOff + "`, beside the conclusion `" + pt10FastOffProse + "`",
			expected: "THE SAME CELL NOW READS \"fast off\", in MUTED UPRIGHT text -- a setting somebody chose. " +
				"It must no longer read \"fast\". The vendor's reason for it (`preference`) is the " +
				"cell's tooltip and is deliberately NOT in the label, so nothing but the two words is " +
				"drawn.",
		},
		{
			name:    "fast-cooldown",
			plan:    "D32",
			prompt:  "!fast-cooldown",
			settled: pt10FastCooldownProse,
			waits: []pt10Wait{
				{"the topbar's fast-mode cell to read the COOLDOWN state", pt10FastCell("cooldown", pt10FastLabelCooldown)},
				{
					// THE SPECIFIC NEGATIVE the contract exists for. The
					// matrix names it outright for `!fast-cooldown`: the strip
					// does NOT draw cooldown as `off`, which would offer a
					// switch that cannot take effect.
					"the cell to be drawing cooldown as ITS OWN state and not as off",
					`document.querySelector('.topbar-fast[data-fast-mode="off"]') === null`,
				},
			},
			asserted: "`.topbar-fast[data-fast-mode=\"cooldown\"]` is drawn with the label " +
				"`" + pt10FastLabelCooldown + "`, and NO `[data-fast-mode=\"off\"]` cell is on the page " +
				"-- the specific negative the fast-mode contract exists for",
			expected: "THE CELL NOW READS \"fast cooling\", DIMMED AND ITALIC -- and that treatment is the whole " +
				"subject of this picture. It must be VISIBLY DISTINCT from the previous capture's " +
				"plain upright \"fast off\": cooldown is the vendor saying \"not right now, and it " +
				"will come back on its own\", where off is a setting somebody chose, and drawing them " +
				"the same would send a reader looking for a switch that cannot take effect. " +
				"`styles.css` gives `[data-fast-mode=\"cooldown\"]` `font-style: italic` and " +
				"`opacity: 0.7`; the picture is what says the reader gets it.",
		},
	}

	for _, row := range rows {
		row := row
		// ONE WORLD AND ONE LOOP, per PLAYTEST-PLAN.md's rule for sections
		// D-H. `t.Run` here is SUBSTEP SCOPING ONLY -- it is sequential, it
		// takes no world of its own, and it exists so a failing row names
		// itself in the test output instead of being one of eleven
		// indistinguishable failures against the same test name.
		t.Run(row.plan+"/"+row.name, func(t *testing.T) {
			// THE PROMPT IS TYPED AND SUBMITTED WITH RET, and `submit` asserts
			// the composer's own RET binding before pressing it.
			s.submit(t, row.prompt)
			s.awaitInPage(t, "the "+row.name+" prompt bubble to arrive on the standing tail",
				pt10UserPromptWithText(row.prompt))

			// THIS ROW'S OWN DRAWN EVIDENCE IS WHAT SCOPES THE ROW, and the
			// ARM IS NOT, which was MEASURED rather than reasoned about.
			//
			// Every row shares one session, so the workspace is already on a
			// settled arm when a row starts and a bare settled-arm wait could
			// be answered by the PREVIOUS row's finish edge. The obvious
			// guard -- wait for a running arm first -- does not work here and
			// must not be reinstated: the fake answers faster than the roster
			// can be observed running, so it failed with "await the
			// slash-vendor-answered turn to be genuinely in flight: never
			// satisfied within 5s; last value was `:done`". The turn had
			// already finished. That is the product being fast, not wrong.
			//
			// So the row is scoped by evidence only IT can produce: every
			// row's answering prose is distinct, and every row's remaining
			// hooks (the Nth compacted divider, the compactionFailed arm, the
			// model button's new name, the fast cell's own state) are
			// reachable only once that row's turn has landed. Those waits run
			// FIRST, and the settled-arm wait then follows them -- by which
			// point this row's answer is already on the page, so a settled arm
			// is unambiguously this row's.
			if row.settled != "" {
				s.awaitInPage(t, "the answering response bubble for "+row.name+" to settle with its own prose",
					pt10SettledResponseWithProse(row.settled))
			}
			for _, wait := range row.waits {
				s.awaitInPage(t, wait.what, wait.expression)
			}
			s.awaitArm(t, s.Name, "the "+row.name+" turn to settle", emGHISettledArms...)

			p.capture(row.name,
				fmt.Sprintf("%s: `%s` typed into the composer and submitted with RET", row.plan, row.prompt),
				row.asserted, row.expected)
		})
	}

	// THE SESSION'S ARM AT THE END, recorded rather than photographed: the
	// last row's capture already carries the screen, and this says the world
	// the pictures were taken in was never left mid-turn.
	final := s.awaitArm(t, name, "the session to be left on a settled arm", emGHISettledArms...)
	p.note("every row in the table run against the one session",
		fmt.Sprintf("the workspace is left on %s, and every one of the %d rows above asserted its own "+
			"drawn evidence before its picture was taken", final, len(rows)))
}
