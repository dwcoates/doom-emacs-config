# WEBAPP digest

DERIVED from figma-to-idl-redesign.md at 86fd2b543 — the canonical record WINS on any conflict. Do not edit; regenerate.

Scope: everything in the design record that bears on the webapp — the
`frontend.v1` components it draws, the `agentrepl.v1` verbs it calls, the
figma→idl and server-driven-UI rules it must honor, and the rendering
conventions that are its own responsibility.

---

## 1. THE GOVERNING PRINCIPLES

### figma→idl: one file per UI component, one message per component's props

`frontend.v1` is the figma→idl specification for the frontend: one `.proto`
file per UI component, each component's props one message, RESOLVED BY THE
SERVER and RENDERED VERBATIM by the client. `agentrepl.v1` is the API the
webapp actually talks to, and it is COMPOSED OF `frontend.v1` messages carried
on component-dedicated endpoints — the sidebar talks to a sidebar endpoint,
likewise topbar, footer, feed. There is no god-endpoint.

### The drawn/called test

What the client DRAWS is `frontend.v1`. What the client CALLS is
`agentrepl.v1`. Requests are called, not drawn, so requests use shared typed
identities (`WorkspaceRef`, `FeedId`); views use per-component element
wrappers. `frontend.v1` never imports `agentrepl.v1`.

### figma→idl takes PRECEDENCE over no-respell at the backend→frontend boundary

`frontend.v1` is composed ONLY of element messages shaped for the drawn
element. When an element's props derive from an internal type (a
`conversation.v1` record, a daemon-internal fact), the DAEMON RESOLVES a
frontend-shaped message for that element — a deliberate re-spelling of the
internal fact into UI vocabulary. This is safe here and nowhere else because
the daemon is the single resolver, the frontend copy is a resolved VALUE
re-published on every change, and the frontend is never a second AUTHOR of the
fact. Typed identities are still imported, never respelled — they are join
keys, not props.

### Every field is an element message; the message tree IS the UI tree

A component's view message contains NO dangling primitives. Every field —
including addressing — is wrapped in a dedicated, appropriately-named message,
so the UI's subcomponents are implicit in the schema and no field is conflated
with a neighbor. When the SAME fact appears in two component views, EACH
component wraps it in ITS OWN message rather than sharing one: duplicate
information in dedicated messages implies separate UI subcomponents.
Renderers read one message per drawn box.

### The client NEVER derives

The recurring defect the redesign removes is client-side derivation. Concretely
banned in the webapp: applying precedence across windows to pick a status
(the daemon picks one); comparing ids to decide "you are here" in a queue (the
daemon ships ahead/current/behind); resolving labels from bare ids (labels are
drawn elements); rounding or formatting figures (daemon-formatted strings);
deciding finality by position (the producer names the answering response);
mapping a status enum to a treatment through a client table (arms are the
treatment); accumulating deltas into a cell (views push whole).

### Views are pushed WHOLE; there are no deltas on the wire

Every component is pushed as its complete current state. A partial-replace or
event-shaped oneof was proposed for the topbar and the footer tokens cell and
REJECTED both times: an event shape models the TRANSPORT (a change sequence),
not the VIEW; the client would accumulate updates, ordering would become
load-bearing inside one component, and a reconnect would lose fields with no
whole push to recover from. If a ticker's rate ever matters, the fix is
daemon-side coalescing.

### PUSH CADENCE (stated once on FooterView, the convention for every view)

EVENT-DRIVEN, WHOLE-VIEW, NO TICKS: push the whole view on any resolved
change; push nothing when nothing changed; the client TICKS LOCALLY from
shipped instants; bursts are coalescible because the wire carries states, not
events.

### NO KEEPALIVE FRAMES ANYWHERE (the 3b convention, retracted)

An in-band keepalive arm was landed as a convention and then RETRACTED. Stream
messages carry views only. The three silences are detected where the knowledge
lives: source-of-truth silence by the DAEMON (surfaced as footer status /
degraded windows); a dead daemon↔client pipe by the CLIENT LIBRARY (unary
calls fail loudly, streams error); a wedged publisher behind a healthy
connection by the DAEMON'S OWN WATCHDOG (DaemonHealth). The webapp does NOT
time frame cadence per stream.

### Presence, never sentinels

Every element of a view is ALWAYS SET; "nothing to show" is expressed INSIDE
the element (an empty warnings list is the daemon saying nothing is wrong). An
UNSET element is a malformed frame, not a state. Nilable MESSAGE fields carry
the explicit `optional` keyword so maybe-absent reads off the schema at a
glance.

### Transport: Connect over one HTTP/2 connection

`agentrepl.v1` is a Connect service (not gRPC) because of the clients — an
xwidget WebKit view and elisp. Browser fetch cannot do gRPC bidi. All
endpoints MULTIPLEX over the ONE HTTP/2 connection a webview already opens, so
endpoint-per-component is an API fact, not a connection-per-component fact.
Commands are ordinary unary requests, which deletes the hand-rolled
correlation layer (requestId, pending-ack map, ack-vs-push races) the old
WebSocket protocol needed. Connect serves binary or JSON per client; elisp
uses JSON.

### Rendering topology

ONE WKWebView xwidget PER WORKSPACE, bound for life, in its own pinned Emacs
buffer, each with its own already-workspace-scoped socket. Consolidation to a
single webview was refused. Therefore per-workspace addressing is the
stream's: `workspace` and `fence` left every `frontend.v1` view (YAGNI), and
the only global stream is the roster.

### Every response is `oneof result { success | error }`

Errors are typed messages in band, never status codes. UNHEALTHY / DENIED /
NOTHING-RUNNING are ANSWERS inside success, never errors (the domain-outcome
rule).

---

## 2. THE agentrepl.v1 VERBS THE WEBAPP CALLS

Seven sections, 30 rpcs at stage-3 close. Every rpc returns
`<RpcName>Response` — no rpc returns a foreign type directly; a stream of a
frontend view wraps that view as its single field.

**FEED**
- `OpenFeed { WorkspaceRef; optional FeedId }` → success `{ newest FeedPage;
  FeedWatchToken }`. UNSET feed = the root feed; SET = a subagent bubble's
  sub-feed. The token PINS THE TAIL exactly after the answered page.
- `WatchFeed { FeedWatchToken }` → stream of one whole `FeedRow` per frame
  (upsert by id). Workspace-scoped by mint. The stream is STANDING ACROSS
  TURNS — a turn's end is the `FeedTurnEnded` ROW, never the stream
  concluding; any non-client-cancelled end is a transport failure.
- `GetFeedPage { WorkspaceRef; optional FeedId; first | next }` → older pages
  of ONE feed. NO CURSOR EXISTS ON THE WIRE: the daemon holds each container's
  walk position (one webview per workspace = one reader per container);
  `first` resets to the newest page, `next` continues older, and a `next` with
  no walk standing is a refusal, not an empty page. No page-size parameter.
- `SubmitPrompt { UserSaid }` → success `{ turn { TurnId } | command_panel }`.
  The client sends NORMAL user requests; the daemon MIGHT answer that it
  handled the input as a programmatic command. The webapp does not know what
  is programmatically handled. A HELD prompt is a `turn` success (the tray
  shows the hold, not an error). Carries a client-minted `idempotency_key` —
  the one verb where a duplicate is costly. FIRST REFUSAL ARM: `merging` — a
  prompt arriving after a merge began is REFUSED outright, never held.
- `Interrupt { WorkspaceRef; oneof target { turn | detached(FeedId) } }` —
  one user intent, one verb; the detached target is the FeedId echoed as
  served. Success arms include `nothing_running` as an ANSWER.
- `AnswerPermission { WorkspaceRef; FeedId permission; allow_once |
  allow_standing | deny{optional reason} }` → success empty. The card's new
  state arrives as the feed's push. `allow_standing` is legal only when the
  card carried `standing_offered`. THE STANDING ECHO TOKEN NEVER REACHES THE
  CLIENT — the daemon holds it.
- `AnswerQuestion { WorkspaceRef; FeedId question; repeated { question_text
  echoed; chosen labels echoed; optional other_text } }` → success empty.
  Answers ECHO THE VALUES SERVED, never positions or tokens.
- `AnswerColdGate { WorkspaceRef; FeedId gate; pay | clear | compact{model,
  scope} }` → success empty; the resolution arrives as the feed's push. No
  `WatchColdGate` exists — the gate streams and pages as a row.

**SIDEBAR**
- `WatchWorkspaceRoster {}` → stream of `WorkspaceRoster` whole. THE ONE
  GLOBAL STREAM (empty request on purpose).
- `CreateWorkspace { RepositoryRef; optional UserSaid initial_prompt;
  optional base_ref }` — THE DAEMON NAMES AND CREATES (slug → branch →
  worktree dir, runs the git, registers). No host materialization round-trip.
- `OpenWorkspace`, `CloseWorkspace`, `MergeWorkspace`, `RestartWorkspace`,
  `KillWorkspace`, `NukeWorkspace` — each `{ WorkspaceRef } → { success{} |
  error }`.
- `SelectWorkspace { WorkspaceRef }` — idempotent; the roster stream carries
  the new `current`. ALSO CLEARS `RosterRow.attention`.

**TOPBAR**: `WatchTopbar { WorkspaceRef }` → stream of `TopbarView` whole;
`SetModel { WorkspaceRef; AgentModel }` — the model is a TYPED ECHO TOKEN
minted by the backend inside each served option and handed back unchanged.

**FOOTER**: `WatchFooter { WorkspaceRef }` → stream of `FooterView` whole.

**DAEMON-HOLD TRAY**: `WatchDaemonHolds { WorkspaceRef }` → stream of
`DaemonHoldTray` whole; `UpdateHeldPrompt { WorkspaceRef; TurnId; release |
drop }` (release = deliver NOW, interrupting if that is what delivery takes;
drop = discard, composer takes the text back; doing nothing is the normal
path); `AnswerHeldOffer` — addressed by OFFER KIND, not a token (a workspace
has at most one offer of a kind standing).

**HOST** (Emacs's, not the webapp's): `RegisterWorkspace`, `SelectWorkspace`,
`WatchHostWorkspace`. The daemon NEVER calls Emacs; there is no daemon→host
command loop (`ReportHostAction` never exists).

**DAEMON ADMIN**: `UpdateShutdownSchedule`, `UpdateMergeQueue`,
`DaemonHealth`, `SessionHealth`, `ClientLog`.
- `ClientLog { WorkspaceRef; ClientLogRecord { level; operation; message;
  Struct context } }` — THE WEBAPP IS ITS CALLER (Emacs never calls it). The
  webapp runs in an xwidget whose JS console is invisible and unpersisted, so
  without this relay a webapp malfunction leaves no evidence anywhere. The
  `Struct context` is the accepted untyped exception: per-call-site
  diagnostics, written verbatim to the daemon's on-disk log, nothing routes on
  or renders it.

### Identity in requests

`WorkspaceRef { id; dir }` and `RepositoryRef { id; dir }` live in the
`workspace.v1` LEAF PACKAGE. A PATH IS NEVER AN IDENTITY: the client provides
the dir string in any spelling, the DAEMON MINTS the id, and every later
request echoes the ref. `dir` is the normalized directory and is NOT to be
used as an identifier. `frontend.v1` embeds these imported refs in
`RosterRowWorkspace`, `RosterCurrentWorkspace`, `RosterRepoKey`, and
`FeedMergeQueueWorkspace` rather than carrying bare strings.

---

## 3. THE FEED

### FeedId: one opaque daemon-minted string, and the client routes by nothing else

The typed identity spaces are FULLY HIDDEN from the frontend — "the frontend
only cares what bubble the thing needs to go into". The daemon ENCODES the
identity of what the row DRAWS (a unit, a task id, an agent id, an ask id, or
a daemon fact for a synthesized row) and DECODES it on echo; mint and resolve
are encode/decode, no id table, stable across pushes and restarts. The one
typed survivor on a row is the `TurnId` stamp, so a client can highlight its
OWN prompt by matching the id `SubmitPrompt` returned it.

UPSERTS ARE UNIVERSAL: every row replaces WHOLE by id. What varies is only
which identity the id was minted from, under the rule ONE ROW PER DRAWN
SUBJECT (a response row per unit; ONE task bubble fed by many acts; a subagent
bubble keyed by the agent, not its spawn call).

### Feed-within-feed

A subagent bubble — sync OR detached, one `FeedSubagent` component — IS a
SUB-FEED: same row vocabulary, its own connection and pages. The bubble row's
own FeedId IS the sub-feed's address. So the webapp opens the root feed on
view-open and opens ONE CONNECTION PER EXPANDED BUBBLE (`OpenFeed(id)` →
`WatchFeed(token)`), abandoning tokens on collapse.

TWO KINDS OF NESTING, and only one is topology:
- REAL SUB-FEEDS (agent bubbles): the connection IS the placement; rows carry
  no parent.
- PRESENTATION NESTING (`parent`): merge-phase rows, work drawn under a skill
  heading. The webapp's parent-routing applies ONLY to this.

The identifier-only-row alternative (a subagent row carrying nothing but its
id, content fetched over its own stream) was weighed and DECLINED on two
grounds: HISTORY (most bubbles in a feed are settled; identifier-only rows
would demand a live connection per settled bubble just to draw "✓ Explore ·
2:10") and STREAM UNIFORMITY (the head is the parent's fact about its child).
What survives of the instinct IS the design: the identifier is how expand
reaches the content; only the collapsed head rides the parent.

### The row taxonomy

`FeedRow` arms: user_prompt / agent_prompt / activity / turn_ended /
detached_subagent / detached_shell / permission / question / separation /
cold_gate.

- PROMPTS ARE TWO SIBLING KINDS. `FeedUserPrompt` and `FeedAgentPrompt` —
  conceptually the same act (an agent sends a prompt to another agent vs a
  user sends one to an agent). The agent one wears an ORANGE BORDER and
  appears on the SENDER's feed as the send and the RECIPIENT's as the
  delivery. This relocated SendMessage out of activity.
- `FeedTurnActivity` = SYNCHRONOUS TURN PROGRESS, whoever drives it:
  response | simple_tool_call | skill | merge | subagent(sync) | artifact |
  hook | plan | findings. MERGE IS ACTIVITY by ruling — from the user's
  perspective the turn has not concluded while a merge is in flight.
- DETACHED WRAPPERS wrap THE SAME drawn component their sync forms use
  (`FeedSubagent`, `FeedShell`): sync-vs-detached is PLACEMENT, never a second
  drawing.
- THINKING IS DROPPED from the feed entirely (footer only).
- UNMODELED IS DROPPED from the feed → the topbar warning dropdown.
- TASKS ARE FOOTER-ONLY: `FeedTask` and the whole family DIED; the tracker
  draws solely as the footer's ☑ chip + expanded checklist.
- WORKFLOWS ARE DEFERRED WHOLESALE: no feed support, no footer support, no
  frontend.v1 support.

### Terminal-as-row, and liveness is structural on the data

`FeedTurnEnded` is a ROW streamed and paged like any other:
`{ ended_at_ms; concluded { FeedId answer } | errored | interrupted }`.
LIVENESS IS STRUCTURAL ON THE DATA — no terminal row for the current turn =
live. `WatchFeed` stays standing across turns; a dead connection stays a
transport failure. History must be able to replay HOW a settled turn ended,
which is why a bare stream close could never carry it.

- `concluded.answer` is the FINAL-ANSWER BORDER TARGET (the old green border,
  which the client used to assign by position at render time).
- `errored` arms are `conversation.v1` api.proto's vendor error taxonomy
  RESPELLED figma→idl — rate_limited/overloaded carry `optional retry_after_ms`
  SO THE CLIENT TICKS A COUNTDOWN — plus max_tokens, max_output_tokens,
  refusal, query_died, billing_error, model_not_found, oauth_org_not_allowed,
  and the two stop-hook terminals. Generic headline+detail strings were
  REJECTED: error information is specific to the error types.
- `interrupted` is the acknowledged-stop accusation.

### The row kinds, as the webapp draws them

**FeedUserPrompt** — author label + body of the feed's shared DRAWN BLOCK
VOCABULARY (`FeedTextBlock {text}` | `FeedImageBlock {src, alt}` |
`FeedUnsupportedBlock {kind}`). The image `src` is a URL THE WEBVIEW CAN
FETCH — a resolution only the daemon can make; the client never sniffs `://`.

**FeedAgentPrompt** — a composed address line ("→ Explore" sender-side, "from
Plan" recipient-side) plus the body in the same block vocabulary. A recorded
DEPARTURE: the old SendMessage card NEVER drew the body (relays run long);
under the "same as a normal user prompt" ruling the body IS drawn, with
fold/cap as CLIENT PRESENTATION.

**FeedResponse** — the prose bubble. `{ optional FeedResponseUsageStamp;
oneof result { update { prose } | success { prose } | error { prose } } }`,
prose being markdown. The usage stamp rides the ENVELOPE and is a
DAEMON-FORMATTED STRING: live while arriving, final once settled,
last-observed on a broken bubble; absence draws NO STAMP, never a zero. The
error arm carries NO reason on purpose — why a response died is the turn
terminal row's fact; the arm only marks which bubble was cut short and keeps
its partial prose drawn. THE BUBBLE APPEARS ON `start`, one frame earlier than
the old renderer's first-text-delta behavior.

**FeedSimpleToolCall** — ONE shared grey-bubble shell generalizing
read/write/edit/grep/glob/foreground-bash/WebFetch/WebSearch. Drawn: head
(purple tool name + status badge) / ONE COMPOSED INPUT LINE / dashed divider /
capped output. GENERICIZED BY RULING — "unless there needs to be specially
supported information, it should be genericized" — so THE CLIENT HOLDS NO
PER-TOOL KNOWLEDGE and a tool-specific affordance is a schema change by
design.
- The daemon owns per-tool input phrasing, including a write's
  created-vs-updated wording. The input line may carry an `optional link`,
  drawn as a HYPERLINK — all URLs are clickable.
- Outcome: `running { last_progress }` | `returned` | `denied`.
- `FeedToolCallReturned { verdict succeeded|failed; form text | code{spans;
  omitted} | diff{arm-typed lines} | lines{lines; omitted} | links{link rows} ;
  optional diagnostics; runtime }`. THE ARMS ARE PRESENTATION FORMS, NOT
  TOOLS.
- Read's head-of-N and glob's at-least floor collapse to the DAEMON-COMPOSED
  `omitted` SENTENCE ("lines 400-499 of 4,312", "9 more not shown", "at least
  42 more", "1.2 MB more not shown · full output at /path"). The client
  DRAWS, never COMPARES.
- Bash's exit becomes the verdict badge; a bash interrupt cause becomes a
  daemon-composed trailing line in the text output ("[stopped by user]").
- `FeedCodeSpan.paint_class` is a STRING naming a paint class the stylesheet
  must have a rule for (owed as a closed arm set). `FeedDiffLine` is ARM-TYPED
  for color (header|added|removed|context), text prefix-free.
- `diagnostics` (IDE/LSP findings) arrive as composed lines drawn BELOW the
  output on a LATER RE-PUSH of the already-settled card — a consequence frame;
  no frame ever says "none are coming".
- `runtime` is a DAEMON-COMPOSED SENTENCE ("ran 4.2 s") on the settled card —
  a FROZEN clock, unset when either instant is missing rather than ticking or
  invented.
- HEARTBEATS: the daemon relays the latest vendor beat into
  `running.last_progress` by re-pushing the row, and THE CLIENT TICKS "quiet
  for N s" LOCALLY from that instant — no cadence timing, no client threshold.
  The in-between state ("beats stopped, shim has not ruled") is deliberately
  unrepresented.

**FeedSkill** — the TEAL document card. `{ invocation (composed "/skill args"
line); running | loaded { document (SKILL.md markdown, FOLDED) | allowances
(composed consent line) } | failed { composed reason } | denied }`. Its own
card because its body is a markdown document with a fold, and the teal wash
marks a conversation-of-its-own. NOTHING DELIMITS A SKILL'S SCOPE at the
source, so drawing subsequent work under a skill heading is a PRESENTATION
choice the client makes on its own authority.

**FeedSubagent** — the collapsed head riding the PARENT feed. `{ label;
optional description; optional tokens (formatted running sum); runtime
{started_at_ms}; live { last_progress } | settled { ended_at_ms; succeeded |
failed | cancelled | lost } }`. `lost` keeps its own word: we stopped seeing
it, not known failed. The body is the sub-feed, opened by the row's FeedId.

**FeedShell** — `{ command; runtime; optional spool { tail text; composed
omitted line }; live { last_progress — SPOOL GROWTH IS THE BEAT } | settled
{ ended_at_ms; optional exit{code}; completed | cancelled | lost } }`.
SNAPSHOT SEMANTICS: the daemon caps and REPLACES the tail WHOLE; THE CLIENT
APPENDS NOTHING (the old offset-append machinery stays dead). NO `failed`
outcome arm — a non-zero exit still COMPLETED; the exit chip carries the
verdict. The old "page older spool inside the bubble" affordance is DROPPED:
display implies no retrieval.

**FeedPermission** — the consent card. `{ headline (VENDOR-RENDERED sentence,
not composed from tool+arguments); optional subtitle; optional trigger note;
arguments (composed preview lines); optional standing_offered (EMPTY MARKER —
its presence draws the "always allow" button); open | answered { at_ms;
allowed_once | allowed_standing | denied_by_user | denied_by_policy{composed
reason} } | abandoned }`. A DENIAL IS AN ANSWER; a policy denial is worded
never to read as the user's act.

**FeedQuestion** — the choice card. 1–4 items, each { header chip; text (an
ECHO VALUE); single_select | multi_select over options whose labels are ECHO
VALUES; optional descriptions }. State: `open | answered { at_ms; per-question
{header; chosen labels; optional other_text} } | expired { at_ms }`. THE
FREE-TEXT ESCAPE IS ALWAYS DRAWN whether or not the agent asked for one.
Expiry is the producer's idle timeout, drawn as EXPIRED, never pending
forever. Answers ride the row so a COLD REPAINT of a settled card has
something to draw from.

**FeedColdGate** — the cold-context gate as a ROW across the feed's tail,
owning the composer while standing: headline, cost, parenthetical, and three
buttons with a compact submenu (model + scope). `{ standing { context_tokens;
last_request; model; compact menu } | resolved { at_ms; pay | clear |
compact{model} } }`. THE ONE RECORDED DEPARTURE FROM THE DAEMON-FORMATS
PRECEDENT: this component ships DATA, NOT PROSE — the daemon serves raw facts
(token count, last-request instant, the model) and THE CLIENT OWNS WORDING,
FORMATTING AND TICKING, bounded to this component because its facts are counts
and instants. Absence of the arm IS "no gate". The compaction scope is ONE
canonical enum ALL | PROMPTS | RESPONSES; the menu's `repeated scopes` are the
offered radios and the verb echoes one back.

**FeedSessionSeparation** (renamed and generalized from `FeedContextCut`) —
one DIVIDER row kind whose `kind` oneof is { cleared, compacted,
worktree_entered, worktree_left }, label composed by the daemon, `tokens`
(before/after, both daemon-formatted strings) optional and set on the context
arms only. A clear reloads system prompt/skills/memory, so `after` is small,
not zero; the client does NO arithmetic and NO unit rounding. A compaction is
a COMPRESSION of the previous conversation, not a summary of what was
discarded, and carries the summary markdown plus a fold.
STRUCTURAL INVARIANT (owed to the fanout's `/structural-invariants` pass): ONE
RENDERER SUBROUTINE draws EVERY separation arm; an arm selects only accent
color and label/payload text. A per-arm divider renderer is a DEFECT. Worktree
arms carry path (a jump target through the shared editor-popup subroutine —
dired for a directory), branch, and the kept|removed outcome with a loud
composed discard line, blue accent.

**FeedMerge** — a tabbed phase bubble. Head = branch line + clock +
live/settled; body = a TAB STRIP (`queue ✓ │ rebase ✓ │ conflicts ✓ 3 │
tests ● 8/12`) with the selected tab's nested rows beneath. Phases are
APPEND-ONLY and each is MONOTONIC (live once, settled once); a repeat pass is
a SECOND TAB with the round in its daemon-resolved label ("conflicts (2)"). A
tab appears because work BEGAN — there is no pending tab. A queued merge is
just another phase, typically the FIRST tab; its content is a SNAPSHOT
REPLACED WHOLE on every publish, carrying `ahead / current / behind` so "you
are here" is NEVER DERIVED by comparing ids. The head workspace is shown
NOTHING about the queue. The tab BADGE draws from exactly two fields — its
label and its state arm — with the detail coming from the kind arm's resolved
evidence: no enum, no client mapping. Nested rows parent to the PHASE, not the
merge.

**FeedArtifact** — a response-styled PURPLE publish bubble: composed heading
(favicon emoji + title, filename fallback), then `publishing | published
{ clickable url } | failed { composed reason }`. ONLY A PUBLISH DRAWS; a list
produces no row. A redeploy UPSERTS the same bubble.

**FeedHook** — failures only. A SUCCEEDED hook draws NOTHING (35k of them —
quiet automation stays quiet); live runs fill the footer's hook activity.
`{ headline; optional gated_call (FeedId link to the refused call's card);
blocked { the hook's refusal text — LOUD } | failed { exit chip; capped
output } }`.

**FeedPlan** — ONE purple response-styled bubble upserting planning →
planned → failed on a SINGLE FeedId (the daemon coalesces the vendor's
unpaired enter/exit units by the episode invariant). The document is RENDERED
MARKDOWN, never raw text; EnterPlanMode draws NO tool card anywhere. The
bubble is READ-ONLY — revisions go through the composer. THE ONE AFFORDANCE is
the ✎ EDIT BUTTON, present iff the exit named the plan file, whose click THE
WEBAPP RAISES TO THE HOST: Emacs opens `FeedPlanEditTarget.path` as a doom
popup, right side, half width. Exit-with-no-enter is legal (a session started
in plan permission mode) and the bubble is then born planned.

**FeedFindings** — the purple defect-list bubble: composed heading, rows with
verdict badge (confirmed|plausible) / category chip / location / summary /
FOLDED failure scenario / outcome badge (fixed|skipped|no_change_needed).
RICH DECORATION IS THE WEBAPP'S: no glyphs ride the wire — badges and chips
are typed arms the client styles. EVERY LOCATION IS A JUMP TARGET through the
shared editor-popup subroutine. NO client sugar (no per-row fix buttons, no
rating): the vendor has no findings-feedback verb.

### FeedPage

`{ rows; oneof edge { has_more (EMPTY — purely "older rows exist", no cursor)
| at_start }; FeedBreadcrumbs }`. `FeedBreadcrumb { FeedId target; label }`,
outermost first, empty for the top-level feed. The LAST BREADCRUMB IS the
container the page is of, so `scope` on the page was a second spelling and
survives only as a request address. Breadcrumb labels are DRAWN ELEMENTS —
bare ids would make the client resolve labels, a derivation. A page that could
not be completed is the PAGE's error, not a row's.

---

## 4. THE FOOTER

### The strip

`Status | SubStatus | StatusActivity | Clock | TOKENS CELL | LIVE-WORK CHIPS
(right-aligned)`. Resolution INCREASES left → right: the coarsest state first,
finer next, the daemon's rich dynamic line last.

**LEGALITY BY CONSTRUCTION (the restructure).** The three sibling cells became
ONE TREE: each `FooterStatus` arm declares exactly the sub-status steps and
activity kinds legal while it stands, so an illegal pairing (a wakeup
countdown under merging) is UNREPRESENTABLE rather than forbidden by comment.
The webapp SWITCHES PER STATUS ARM. Leaf payload messages (notification,
wakeup, retrying, …) are SHARED across the per-status oneofs — one drawn cell,
one payload vocabulary; the oneof types carry the legality. Status/substatus
are oneofs, never enums (the standing state-enum prohibition).

**Status arms**: idle, thinking, waiting, interrupted, merging, background,
blocked, closing, loading, disconnected. Color per arm from the shared
vocabulary, resolved by the daemon. `interrupted` and `loading` are MOMENTARY,
cleared by the next update.

**Notable sub-statuses**: waiting · wakeup (the ScheduleWakeup FALLBACK),
waiting · permission, waiting · question, waiting · cold_gate, waiting ·
interrupting; blocked · billing, blocked · query_died; closing · blocked;
loading · {memory | invoked | discovered | listing}; interrupted · {by_user |
host_shutdown}.

**StatusActivity** is the RICH CELL — typed kinds whose payload is mostly
composed text: merging_commit, hook, retrying, authenticating,
blocked_on_user, rate_limited, context_budget, wakeup, notification,
interrupting, context_injected, close_blocked. The FREE-TEXT `note` ESCAPE WAS
DELETED: an activity the daemon can name but has no arm for is a MODELING GAP;
the fix is the arm, not a fallback string.

**THREE STATUS-INDEPENDENT ACTIVITIES** appear in EVERY status arm's oneof:
notification, rate_limited, context_budget. The DAEMON selects ONE standing
activity per push by a stated precedence ladder: notification OUTRANKS
everything; status-bound kinds rank next by the daemon's judgment;
rate_limited is second-lowest; context_budget is lowest (shown only when
nothing else stands).

**`FooterStatusActivity.at`** — when the activity began standing, on the
ENVELOPE (per-status wrapper) so every arm carries it by construction. THE
CLIENT TICKS THE RELATIVE AGE. A push without it is a loud daemon fault.
Wakeup's FUTURE deadline (`wake_at_ms`) stands apart as its own fact and ticks
DOWN.

**ACTIVITY OPTIONALITY IS EVIDENCE-GATED**: required where a producer always
has a line (waiting, loading), optional elsewhere with the WHY stated at the
field.

### RENDERING CONVENTIONS stated once on the family banner

- Status and substatus render LOWERCASE ASCII WITH SPACES, NEVER UNDERSCORES.
- Activity is the RICH cell — colorized, formatted, ideally one line.
- A STATICALLY TYPED DATUM inside an activity message SIGNALS that datum
  deserves COLOR in the rendered line.
- A status arm with NO substatus MERGES that cell into the status cell;
  ACTIVITY ABSORBS FREE WIDTH.

### The tokens cell and the chips

The cell is ONE FIGURE — the turn's uncached input — plus two glyphs (alarm ⚠
when the expensive-turn threshold trips, accounting verdict badge). THINKING
LEAVES THE STRIP.

The chips are presence-gated on liveness / non-emptiness (an unset chip is NOT
DRAWN): ⚙ agents (count), ☑ tasks (done/total FRACTION — both counts always
drawn, explicitly NOT a completeness oneof), $ shells (count), 👁 monitors
(count), ⏱ crons (count). THE UNMODELED CHIP WAS REMOVED — detached-unmodeled
surfacing belongs to the topbar's warning dropdown.

### The expanded panels

Cell and chips are CLICK TARGETS and the SELECTION IS WEBVIEW-LOCAL, so THE
DAEMON SHIPS EVERY EXPANDED PANEL FULLY RESOLVED ON EVERY PUSH (the
folded-menu convention) and THE CLIENT DRAWS WHICHEVER THE SELECTION PICKS.
Panels stay bare (not `optional`) precisely because they are always resolved.

- TOKENS: NOT a list — heterogeneous facts, ONE DEDICATED LINE-ELEMENT EACH
  (uncached input, cache read, cache write, output, thinking INDENTED under
  output, first-token latency, the alarm sentence when tripped, the verdict
  line with evidence text). Lines are ALWAYS SET with optional values so the
  panel shape is STABLE MID-TURN.
- AGENTS / SHELLS / TASKS / CRONS / MONITORS: TRUE LISTS (repeated of one row
  type).
  - Subagent rows: label / description / tokens / runtime — each A JUMP
    TARGET (a FeedId).
  - Shell rows: command / runtime — jump targets.
  - Task rows: the checklist. The "checkbox" is NOT A BOOL but the drawn
    projection of the tracker's status (pending | running{active_form} |
    completed). NO jump target — `FooterTaskRow.target` was retired when
    `FeedTask` died. A DELETED task is prescribed as ROW OMISSION: the daemon
    drops the row and the next whole-list push carries the tracker without it.
  - Monitor rows: description / runtime clock (CLIENT TICKS from
    started_at_ms) / optional persistent marker. DELIBERATELY NOT jump targets
    — monitors have no bubble.
  - Cron rows: vendor-composed human schedule / daemon-truncated prompt /
    RECURRING and DURABLE markers / a COUNTDOWN — the daemon resolves the
    next-fire INSTANT from the cron expression and ships epoch ms; THE CLIENT
    TICKS "5m 12s". Not jump targets.

All figures throughout the footer are DAEMON-FORMATTED STRINGS — no client
rounding. All clocks carry ONLY the start instant and the client ticks.

---

## 5. THE TOPBAR

`TopbarView` is element messages and nothing else: title, session line, model
selector, connectivity, warning strip, token breakdown. NO ONEOFS among the
view's elements — none are mutually exclusive, because a push is the WHOLE
topbar AS IT NOW STANDS, never an event naming what changed.

- MODEL SELECTOR: `{ optional selected; repeated options }` where each option
  carries the `AgentModel` TYPED ECHO TOKEN. `model_display` (a string) is
  gone — THE SELECTION IS THE WHOLE OPTION. `SetModel` echoes the token back;
  a client that invents a value is TYPED as wrong.
- TOKEN BREAKDOWN: nested INSIDE `TopbarView` (one level deeper, not a
  sibling stream, not a fetch-on-open). It is ALWAYS POPULATED, like a folded
  section still carrying its rows, so OPENING THE MENU NEEDS NO ROUND-TRIP. It
  is the SESSION's accounting; the footer's tokens are the TURN's — different
  values, architecturally uncoupled, never a shared type.
- CONNECTIVITY: `tone` is a COLOR-CLASS NAME from the shared
  `proto/vocab/render-colors.json` vocabulary — a rendering token, not a
  state.
- WARNING STRIP: the last warnings newest-first, daemon-capped. Each
  `TopbarWarning = { line (the dropdown row's sentence); oneof detail — the
  CLICK'S OVERLAY CONTENT }`:
  - `accounting` { composed evidence lines } — the topbar's own resolved copy,
    now that an OVERLAY draws it.
  - `unmodeled_tool` { tool_name; ABBREVIATED LEGIBLE argument_lines } — NEVER
    A DUMP, never drawn as a failure. One warning per distinct tool name. This
    is the home of unmodeled tools: aware but not loud, comprehensible for
    remediation, and NOT in the feed where it would read as part of the
    conversation.
  - `detached_unmodeled` { tool_name; started_at_ms } — one warning per live
    item, the fact the footer chips deliberately dropped.
  - `session_fault` { component; detail } and `degraded_window` { component;
    reason; began_at_ms; open | closed{ended_at_ms, dropped_count} } — one
    warning per fault/window; a healthy pull retracts on the next push.

An EXEMPT tool must NEVER trip the unmodeled warning. Exempt set: TaskStop,
TaskOutput, TaskGet, TaskList, ToolSearch, NotebookEdit, the background-shell
peek, skip_transcript-marked ambient tasks, REPL, ListMcpResources,
ReadMcpResource, RefreshMcpTools, SendFeedback, ClaudeDesign, Projects,
ShowOnboardingRolePicker, ProposeSkills.

---

## 6. THE SIDEBAR / ROSTER

THE DAEMON OWNS THE ROSTER. Emacs's contribution collapsed to two commands
(`RegisterWorkspace`, `SelectWorkspace`); the roster is a daemon-RESOLVED
`frontend.v1` view flowing in exactly ONE DIRECTION, daemon → webapp. The old
three-spellings-of-one-status path (Emacs authors → webapp maps) is gone.

- SCOPE IS GLOBAL: the roster stream carries no workspace; every webview
  watches the same roster and the daemon's stream order is the only order.
- `WorkspaceRoster` carries BOTH GROUPINGS FULLY RESOLVED (`repository` and
  `task` as siblings, no longer a oneof) and the client draws the pane its
  LOCAL preference picks — a SELECTION BETWEEN RESOLVED VIEWS, like folding,
  never a derivation.
- ROSTER UI PREFERENCES ARE WEBVIEW-LOCAL: grouping mode, section folds, and
  the nav cursor LEFT the roster entirely. A shared fold would fold every
  webview; the cursor is where YOUR keyboard is. `SetWorkspaceRosterView`
  never exists.
- `revision` and `boot_id` and the epoch/monotonicity rules are DELETED — they
  guarded an out-of-order STATE publish that no longer exists. The webapp's
  `rosterFromFrame` staleness check goes with them.
- `RosterRow { workspace(ref); name; status; current; children; when; detail;
  closed; attention }`. `RosterRowWhen` is a ONEOF — `last_selected{at_ms}` |
  `merged{at_ms}` — CHOSEN BY THE DAEMON (precedence resolved server-side; the
  webapp's when-column precedence code is deleted). `RosterRowDetail` is three
  line messages (branch, parent branch, summary), present/absent by MESSAGE
  PRESENCE, never empty string.
- `RosterRowAttention` (32) is an EMPTY PRESENCE MARKER set by the daemon on a
  push notification and cleared on the EXISTING `SelectWorkspace` verb.
  **THE CANONICAL BLINK CADENCE IS SPECIFIED ONCE ON THIS MESSAGE: TWO BLINKS,
  500 ms ON/OFF, THEN STEADY.** The webapp sidebar and the Emacs tab-bar BOTH
  implement exactly that spec and cite the message; DIVERGENCE IS A DEFECT
  (user-mandated code-level consistency, the editor-popup precedent).
- Hibernation left the contract entirely: `RosterRowStatusHibernated` and
  `FooterStatusAsleep` are DELETED. A parked workspace is INDISTINGUISHABLE
  from an idle one on every surface — it presents as `live` with
  `shim_attached=false`, the footer shows `idle`, the roster shows the
  ordinary dot. The distinction surfaces ONLY as the cold gate, when it has a
  cost. TEAL DIES; the palette contracts to five colors, cross-system.

---

## 7. THE DAEMON-HOLD TRAY

Its own component and its own stream, drawn as a "pending" tray AT THE FEED'S
TAIL, ABOVE THE FOOTER. The FEED KNOWS NOTHING ABOUT IT: the feed is history +
the live turn (scrolls, pages, appends); the tray is the FUTURE
(WHOLE-LIST-REPLACED on every change) and the feed's paging never sees a
queued entry.

`DaemonHoldTray { heading; repeated item { HeldPrompt | HeldOffer } }`.
- `HeldPrompt { TurnId; UserSaid said; queued_at; classification (5 arms);
  hold (4 arms) }`. A held prompt IS a `UserSaid` not yet forwarded — one
  canonical form client → daemon → tray → shim → record. The four real holds
  are shutdown drain, keep-alive turn, session-starting, build refresh.
- `HeldOffer { merge_dequeue { composed headline sentence } }` — the two
  answers are `AnswerHeldOffer`'s ARMS, never fields of the card.
- HELD PROMPTS BLOCK A CLOSE: a held prompt is undelivered user intent and a
  close may never silently discard it. The user clears a hold via the tray's
  release/drop verbs.

THE DAEMON MINTS `TurnId`, not the client. Under server-driven UI the client
RECONCILES NOTHING: optimistic rows and the pending-request map go away; unary
responses answer requests; the daemon STAMPS feed rows of a turn with the
TurnId so a client that wants to highlight its own prompt matches the id it
was returned. Every other effect is a pushed view update.

---

## 8. COMMAND PANELS

There is NO `RunCommandPanel` and NO command-panel section. The client sends
NORMAL user requests; the daemon MIGHT answer that it handled one
programmatically, returning a panel in `SubmitPrompt`'s success. THE WEBAPP
SHOULD NOT KNOW WHAT IS PROGRAMMATICALLY HANDLED — the recognition table lives
only in the daemon, transparently. A `DaemonInterceptedCommandItem` was
deleted for the same reason.

Panels shipped, each its own component file, carrying RESOLVED DATA, NOT
LAYOUT — **the webapp has the agency to determine each panel's rendering**:
- `StatusPanelView` (label/value rows)
- `TodosPanelView` (pending | running | completed rows)
- `AgentsPanelView` (name + optional description)
- `McpPanelView` (name + connected | failed{detail} | needs_auth | pending |
  disabled — the arm IS the badge; the webapp owns the treatment)
- `ContextPanelView` (heading'd sections of label/tokens/share/depth rows)
- `HelpPanelView` (command + optional description)

DROPPED as unproducible headlessly: /doctor, /hooks, /release-notes, /export,
/memory, /permissions. LEFT THE DAEMON at the vetting pass: /cost and /usage
(they fall through to the vendor). NO-PANEL commands: /clear and /compact (the
separation row is the outcome), /model (topbar), and the act/flow commands.

---

## 9. CROSS-CUTTING RENDERING CONVENTIONS THE WEBAPP OWNS

1. **Clocks tick client-side from shipped instants.** Every runtime, age and
   countdown carries only an INSTANT (started_at_ms, at, wake_at_ms,
   next-fire). Elapsed figures on the wire were rejected twice: a second
   authority for a derivable value, arriving at the producer's heartbeat
   cadence, so a drawn clock would jump to network timing instead of ticking.

2. **The blink cadence** — two blinks, 500 ms on/off, then steady — specified
   once on `RosterRowAttention` and implemented identically by the webapp
   sidebar and the Emacs tab-bar.

3. **No underscores** — footer status and substatus render lowercase ASCII
   with spaces.

4. **The shared editor-popup subroutine.** ONE shared webapp link component
   and ONE shared Emacs subroutine ("open path[:line] in a doom popup, right
   side, half width") back EVERY jump-to-source affordance: the plan bubble's
   ✎ edit button, every `FeedFindings` location, and the worktree separation
   paths (dired for a directory). Code-level consistency is mandated; this is
   a fanout implementation requirement.

5. **One separation renderer.** ONE subroutine draws every
   `FeedSessionSeparation` arm; the arm selects only accent color and label /
   payload text. A per-arm divider renderer is a defect.

6. **All URLs are clickable** — tool-call input links, WebSearch link rows,
   artifact URLs.

7. **Rich decoration is the webapp's.** No glyphs ride the wire; badges and
   chips are typed arms the client styles.

8. **Highlighting moved to the daemon.** The daemon parses and the client
   PAINTS SPANS IT IS HANDED. Consequence: `languageForPath` and
   `highlightCode` leave the webapp, and highlight.js plus its twenty grammars
   leave the bundle. The daemon needs a grammar set at least as wide or files
   silently lose highlighting they have today.

9. **Fold and cap are client presentation.** Long prompt bodies, skill
   documents, findings' failure scenarios and compaction summaries fold
   client-side.

10. **Draw the arm, never a sentinel.** A `redacted` thinking block draws
    nothing at all rather than an empty card. Absence of a usage stamp draws
    no stamp, never a zero. Absence of the cold-gate arm is "no gate".

---

## 10. WHAT LEAVES THE WEBAPP (named removal work)

- `hibernation.ts` whole (741 lines), the hibernation gate DOM/CSS, its
  adapter/store/command arms, and the teal CSS variables.
- `highlight.ts` (`languageForPath`, `highlightCode`) and highlight.js with
  its twenty grammars.
- `render.ts`'s per-tool branches — the eight sites that dug `command`,
  `file_path`, `pattern`, `summary`, `skill`, `status` out of an untyped
  Struct by string key. The client draws headline/input/output verbatim.
- `render.ts`'s `finalResponses` border assignment (finality is now
  `FeedTurnEnded.concluded.answer`).
- The separate `BubbleTyping` line under a bubble (`async-render.ts`) — a
  subagent's streaming is its own sub-feed's rows.
- `async-bubble.ts`'s 3-tier identity ladder and its offset-checked append
  handling (the shell spool is a whole-replaced tail).
- `store.ts`'s preview-id-then-record-uuid switch deduped by hand — the shim
  mints ONE id per unit for its whole life, and rows are keyed by FeedId.
- `command-dispatch.ts`'s `onAck` pending-request map and optimistic rows.
- `rosterFromFrame`'s `revision`/`boot_id` staleness check; the when-column
  precedence code.
- The queued-card rendering in the feed renderer → a tray component.
- The fence byte-compare gate.
- The client's activity-precedence application over ProgressView's windows.
- `asyncShape` / `classifyAsyncSource` — the daemon switches on the arm.

---

## 11. IMPLEMENTATION INVARIANTS BINDING THE WEBAPP

1. **UNSET NON-OPTIONAL FIELDS ARE ILLEGAL, EVERYWHERE, IMMEDIATELY.** For
   RESPONSES AND STREAM PUSHES a non-optional field MUST be set, and a
   consumer receiving one unset RAISES A LOUD ERROR ITSELF — on a stream there
   is no producer to answer. Sized to be caught during integration
   remediation. For requests the webapp sends, an illegal request is answered
   with an ERROR at once, never "handled", never defaulted.

2. **PROTO→CODE MAPPING.** Every MESSAGE gets one core "base" function per
   language where validation lives ONCE (unset non-optional fields and
   required-semantics empty strings are ERRORS; an unset oneof is an ERROR BY
   DEFAULT, a documented fallback only where the schema comment explicitly
   sanctions absence). Every NON-PRIMITIVE use site (message-typed field,
   oneof arm) gets its own dedicated TESTABLE function delegating to the
   child's base. Primitives get no wrappers. NO class-per-message mandate —
   the requirement is dedicated testable functions and separated concerns; the
   anti-goal is a million unnamespaced `Handle<A><B><C>` functions.

3. **LOGGING.** Debug on every logical branch; warnings at WARNING, errors at
   ERROR. Integration/e2e runs enable ≥WARNING BEFORE tests and PERUSE the
   logs EVEN ON GREEN RUNS; every warning is remediated to zero (fixed or
   deliberately downgraded), never left standing.

4. **IMPLEMENTERS NEVER CHANGE PROTOBUFS.** A needed change is a REQUEST to
   the webapp orchestrator, who triages to the lead; the lead pauses all
   systems, lands the change, rebuilds bindings, and resumes with a new
   foundation commit SHA.

5. **THE TEST RULE (reconciliation).** Any test referencing a DELETED symbol,
   or a RESPELLED one (pointing at a genuinely different structure, not a mere
   rename), is DELETED, never adapted. Pure renames adapt mechanically. An
   adaptation that would require deciding what behavior should NOW be is a
   surfaced gap, not an adaptation. Replacement INTEGRATION coverage is
   prescribed into `docs/implementation/webapp.md`; unit coverage is NOT
   prescribed — it falls out of the mapping convention above.

6. **Store is NUKED, never migrated.** No durable-compatibility argument
   preserves any shape anywhere.

Webapp reconciliation merged green (4901 tests) at the reconciliation pass;
its dead-code inventory, integration replacement specs and reconciliation
gotchas live in `docs/implementation/webapp.md`.

## 12. The graceful-rollout handover (post-freeze increment)

- NEW WEB LINK section: `WatchWebWorkspace { WorkspaceRef }` is the
  webview's standing daemon-link stream; its `transferred { address }`
  push means the old daemon released this workspace.
- The webview's obligation, IN ORDER: connect to the address, call
  `AdoptWebWorkspace { WorkspaceRef }` there, and only after success
  cancel the old connection's streams — connect-new-first, so no gap is
  observable; the persisted feed page position makes re-attach
  evidence-free (cold open = newest page only).
- The adopt is a rendezvous with Emacs's `AdoptHostWorkspace`; all
  expected participants succeed together.
- OWED derived refusal arms at the wave: `transferring_away { address }`
  from the old daemon (self-heal from the refusal) and `not_yet_adopted {}`
  from the new.

## 13. The merge bubble as a sub-feed (post-freeze increment)

- PARITY INVARIANT: the merge bubble uses the SAME sub-feed plumbing as
  the subagent bubble (expand → OpenFeed(id) → WatchFeed; collapse →
  abandon); a merge-specific nested-content loader is a defect.
- The body renderer is the one legitimate difference: a TAB STRIP over
  FeedMergeTab rows — resolved tabs (queue snapshot; tests suites with
  paint-class colored spans; landing narration lines) draw the row's
  content; agentic tabs (rebase, remediation, action) draw the sub-feed
  rows parented to them; parked tabs show the composed line + paused
  badge, and the user's prompts land there as ordinary rows.
- Content is LAZY: collapsed = head only; settled merges page on demand.
