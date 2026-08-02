/**
 * progress-footer — THE consolidated in-flight indicator (F1).
 *
 * One raised, flat, segmented dock at the bottom of the feed column, capped to
 * the same width as the widest response bubble, replacing every scattered
 * progress signal the webapp used to derive for itself: the thinking / working
 * / retrying tail rows, the pulse machinery that chose between them, and the
 * live duration/token stats row they orbited.
 *
 * It DERIVES NOTHING. Its whole input is the daemon-resolved `ProgressInput`
 * (`ProgressView`), plus the two rosters relocated here from the topbar and the
 * running-tool liveness the store already banks from `HeartbeatView`. The
 * webapp's job is to render what the daemon resolved.
 *
 * SHAPE (the V4 "segmented dock" from design-progress-footer-samples.html): a
 * grabber notch over a row of bordered cells —
 *
 *   phase │ activity detail (grows) │ turn clock │ token cell │ counters
 *
 * with an error row under it when an error stands, and a detail sheet that
 * opens on a click. Thin by default: one line of cells.
 *
 * TICKING. The turn clock and the running tool's elapsed are the only values
 * that change without a frame arriving, and they are repainted through the
 * `TaskTimer` slot discipline (`paintTurnTimer`) — one span rewritten per
 * second, never a re-render of the footer, and never of the feed.
 */
import { AGENTS_SPEC, agentsMenuHtml } from "./agents.js";
import { BreathState, BreathingTicker, breathColor } from "./breathing.js";
import { CounterEntry, CounterSpec, isActive } from "./counter-menu.js";
import { formatCountdown, formatElapsed } from "./duration.js";
import { escapeHtml } from "./highlight.js";
import { mergeFacts } from "./merge-status.js";
import type {
  InterruptInput,
  MergeStatus,
  ProgressInput,
  WebRenderState,
  WebSessionConnectivity,
  WebSessionStatus,
} from "./state-adapter.js";
import { ConversationItem, ToolItem } from "./store.js";
import { TASKS_SPEC, tasksMenuHtml } from "./tasks.js";
import { IDLE_LABEL, TIMER_SLOT } from "./timer.js";
import { compactTokens } from "./tokens.js";

/** Which of the footer's counter overlays is open, and whether it is expanded. */
export interface FooterDisclosure {
  agentsOpen: boolean;
  tasksOpen: boolean;
  /** Whether the detail sheet is showing. */
  expanded: boolean;
}

/** Everything one footer render needs. */
export interface FooterInput {
  /** The daemon's resolved view, or null before the first one lands. */
  progress: ProgressInput | null;
  /**
   * THE workspace's resolved phase (F5), read off the same `WorkspaceState` the
   * Emacs tab bar and the sidebar dot read. `null` before the first one lands,
   * which collapses the phase cell rather than naming a phase nothing resolved.
   *
   * It arrives here rather than on `progress` because a phase carried in two
   * messages is a phase kept in two copies, and the stale copy is what read
   * "starting" against an already-green tab.
   */
  renderState: WebRenderState | null;
  /**
   * THE structured merge status, or null when no merge run touches this
   * workspace. It is the ONLY merge input the footer takes.
   *
   * It arrives on the SAME `WorkspaceState` as `renderState` for the reason
   * that field's own note gives: a merge fact carried in a second message is a
   * merge fact kept in two copies, and the stale copy is the one that reads
   * "2/3" against an already-merged workspace. The flat queue pair that used to
   * sit here was exactly that second copy, and it is retired.
   */
  mergeStatus: MergeStatus | null;
  /** Daemon-resolved route reliability for the same WorkspaceState. */
  connectivity: WebSessionConnectivity | null;
  /** Activity fact retained beneath a non-operational connectivity verdict. */
  sessionStatus: WebSessionStatus;
  /** The session's subagent roster, relocated from the topbar. */
  agents: readonly CounterEntry[];
  /** The session's task roster, relocated from the topbar. */
  tasks: readonly CounterEntry[];
  /**
   * The feed's items, for the ONE thing the ProgressView does not carry: which
   * tool is running right now and how long it has been going (the store banks
   * that from the `HeartbeatView` relay).
   */
  items: readonly ConversationItem[];
  /** The turn clock's current reading, baked in so a fresh render shows it. */
  timerLabel: string;
}

/** The phase word and accent class the footer's anchor cell wears. */
export interface PhaseLabel {
  word: string;
  /**
   * Accent class stem. The state tones map onto the colors the tab-bar and the
   * sidebar dots speak, so a workspace never reads one way here and another
   * there:
   *   `thinking`   red    a turn is in flight
   *   `blocked`    purple the vendor or the account has stopped this session
   *   `async`      yellow no foreground turn, live detached work
   *   `ok`         green  ready
   *   `error`      blue   the route is compromised on our side
   *   `hibernated` teal   nothing is wired, and NOTHING IS WRONG
   *   `muted`      quiet chrome (merge phases), which spend no color
   *
   * `hibernated` is a tone of its own rather than a reuse of `error`, and that
   * is the correction the split exists for: a deliberately sleeping workspace
   * was wearing the same error-red word as a dead shim, so the footer told the
   * user to act on the single most routine event in the system.
   */
  tone: "thinking" | "blocked" | "async" | "retry" | "error" | "ok" | "hibernated" | "muted";
  /**
   * Whether the phase word BREATHES (the agent is actively working).
   *
   * This was `spinning` and drove a rotating arc beside the word. The motion is
   * now the word's own gentle size oscillation (see breathing.ts) — the claim
   * it makes is identical, so every arm below keeps the value it had.
   */
  breathing: boolean;
}

/**
 * The workspace's resolved render state as the footer's anchor word, tone, and
 * breath — read off the ONE authority, not off a copy.
 *
 * The vocabulary is CLOSED, so a new render state is a compile error here
 * rather than a footer that silently shows nothing. Merge phases get the quiet
 * muted tone: a merge is chrome the sidebar leads on, and the footer only
 * echoes it.
 */
export function phaseLabel(state: WebRenderState): PhaseLabel {
  switch (state) {
    // The FIRST half of a turn: the daemon has the prompt and the shim has not
    // acked it yet. Breathing and red exactly as thinking is, because the claim
    // about what the user can do is identical. The word is the whole point of
    // the split — it stops `thinking` from being said while the agent has not
    // been handed anything.
    case "submitting":
      return { word: "submitting", tone: "thinking", breathing: true };
    case "thinking":
      return { word: "thinking", tone: "thinking", breathing: true };
    // RED like thinking, and breathing for the same reason: the agent is busy
    // and a prompt cannot land. The word is the whole distinction — it says
    // the turn is doing something OTHER than producing the answer, which is
    // why these outrank thinking in the SSM's rank table.
    case "clearing":
      return { word: "clearing", tone: "thinking", breathing: true };
    case "compacting":
      return { word: "compacting", tone: "thinking", breathing: true };
    case "permission":
      return { word: "permission", tone: "retry", breathing: false };
    case "done":
      return { word: "done", tone: "ok", breathing: false };
    // GREEN like done: an interrupted turn is a CONCLUDED turn — the user
    // asked for the stop, got it, and can prompt again immediately. The word
    // is the distinction; the color claim is the same.
    case "interrupted":
      return { word: "interrupted", tone: "ok", breathing: false };
    // GREEN, not muted: idle and ready are the same claim as done — the
    // route works and the agent is available. Greying them said "nothing to
    // see here" about a workspace that was ready to be used.
    case "idle":
      return { word: "ready", tone: "ok", breathing: false };
    case "ready":
      return { word: "ready", tone: "ok", breathing: false };
    // YELLOW: no foreground turn, but detached work continues.
    case "idle_async":
      return { word: "monitoring", tone: "async", breathing: true };
    // BLUE: the route is compromised on our side. `starting` keeps its breath
    // because bring-up really is in progress.
    case "init":
      return { word: "starting", tone: "error", breathing: true };
    // PURPLE: blocked until a human or the vendor acts. It does NOT breathe —
    // every other phase that breathes is work happening, and animating this one
    // would say the opposite of what it means.
    case "vendor_blocked":
      return { word: "blocked", tone: "blocked", breathing: false };
    // BLUE, and pointedly NOT breathing. `starting` breathes because a bring-up is
    // really under way; severed is the opposite claim — nothing is wired,
    // nothing is coming, and something on our side broke — and a breath would
    // say work is happening.
    case "severed":
      return { word: "severed", tone: "error", breathing: false };
    // TEAL, still, and BENIGN. The tone is the whole point of this arm: this
    // state inherited `dormant`'s "error" tone, which put an error-shaped word
    // in the footer of a workspace whose only sin was being asleep on purpose.
    // Nothing here needs acting on — the next prompt pays one bring-up and
    // everything comes back.
    case "hibernated":
      return { word: "hibernated", tone: "hibernated", breathing: false };
    case "dead":
      return { word: "dead", tone: "error", breathing: false };
    case "degraded":
      return { word: "degraded", tone: "error", breathing: false };
    // THE FIRST INSTANT OF A MERGE ATTEMPT, before it is durably enqueued. It
    // BREATHES, exactly as `merging` does and unlike `merge_queued`: something is
    // actively happening (the daemon is resolving the geometry and writing the
    // queue entry), where a queued merge is waiting for someone else to finish.
    case "merge_enqueuing":
      return { word: "merge enqueuing", tone: "muted", breathing: true };
    case "merging":
      return { word: "merging", tone: "muted", breathing: true };
    case "merge_queued":
      return { word: "merge queued", tone: "muted", breathing: false };
    case "merge_conflict":
      return { word: "merge conflict", tone: "error", breathing: false };
    case "merge_failed":
      return { word: "merge failed", tone: "error", breathing: false };
    case "merged":
      return { word: "merged", tone: "ok", breathing: false };
    default: {
      const never: never = state;
      throw new Error(`progress-footer: unhandled render state ${String(never)}`);
    }
  }
}

/**
 * The phase cell as the STRUCTURED merge status names it, or null to leave the
 * cell to `phaseLabel` and the flat render state.
 *
 * This exists for the two phases the render state cannot name: the daemon now
 * runs the pre- and post-merge prompts itself, and both arrive inside the
 * merge's own `merging` render state. Without the override the footer says
 * "merging" through a stretch where nothing is being merged at all — the
 * session is being driven by a prompt the user cannot see or interrupt.
 *
 * It overrides the OTHER phases too, deliberately: the status and the render
 * state are stamped on the same message and agree, so preferring the status
 * everywhere means one source is read rather than two that could drift. The
 * tones map onto `phaseLabel`'s own vocabulary, so a merge reads identically
 * whichever path named it.
 */
export function mergeStatusPhaseLabel(status: MergeStatus | null): PhaseLabel | null {
  const facts = mergeFacts(status);
  if (facts === null) return null;
  return { word: facts.word, tone: facts.tone, breathing: facts.breathing };
}

/**
 * The structured merge status as the footer's chip, or null when there is no
 * figure to show.
 *
 * It is a CHIP rather than more words in the phase cell, and that is
 * deliberate: the phase cell's geometry is fixed to the widest phrase the
 * CLOSED phase vocabulary can produce, while a run's arithmetic is unbounded
 * numbers. Read left to right the pair still says "merging 2/4"; putting the
 * digits inside the phase cell would have made every phase's cell wider forever
 * to accommodate figures that are usually absent.
 *
 * It is the ONLY merge chip now. The flat `mergeQueueChip` it used to
 * supersede-when-present is gone with the fields that fed it, so there is no
 * second arithmetic that could render beside or instead of this one.
 */
export function mergeStatusChip(status: MergeStatus | null): MergeQueueChip | null {
  const facts = mergeFacts(status);
  if (facts === null || facts.count === "") return null;
  return { text: facts.count, title: facts.countTitle };
}

/** The merge queue's place, as the footer's chip beside the phase. */
export interface MergeQueueChip {
  /** `position/depth`, e.g. `2/3`. */
  text: string;
  /** The hover line, which spells out what the two figures mean. */
  title: string;
}

/** The interrupt window as the footer's chip beside the phase. */
export interface InterruptChip {
  /** The word the chip shows; the three outcomes never share one. */
  text: string;
  /**
   * Accent class stem. Two of the three outcomes are SUCCESSES and take the
   * calm `ok` green; only a stop that could not be delivered is an error.
   */
  tone: "ok" | "error";
  /** The hover line, which says what the word means without abbreviating. */
  title: string;
}

/**
 * Return the active phase that contradicts an ALREADY_COMPLETE interrupt
 * verdict, or null when the pair is coherent.
 *
 * The shim's verdict means no foreground turn exists. Thinking and the two
 * context-cut phases all claim one does; permission can cover the same stale
 * turn and is reconciled at the daemon boundary for the same reason. Keeping
 * this check beside the renderer makes any future regression a canonical log
 * event instead of a visual puzzle.
 */
export function alreadyCompletePhaseViolation(
  state: WebRenderState | null,
  outcome: InterruptInput["outcome"] | null,
): WebRenderState | null {
  if (outcome !== "already_complete") return null;
  switch (state) {
    case "thinking":
    case "clearing":
    case "compacting":
    case "permission":
      return state;
    default:
      return null;
  }
}

/**
 * The interrupt chip, or null when no interrupt window is open (I1).
 *
 * The daemon opens the window on the shim's ack and clears it when the next
 * turn starts, so this holds NO state of its own: a frame with the window
 * closed renders no chip, and there is nothing left behind to clear.
 *
 * The three outcomes read APART on purpose. `interrupted` and
 * `already complete` are both successes — the user asked for the turn to be
 * over, and it is — while only a stop that could not be delivered is a
 * failure. Painting the second as the third is exactly the confusion the
 * outcome enum exists to end.
 */
export function interruptChip(p: ProgressInput): InterruptChip | null {
  const w = p.interrupt;
  if (w === null) return null;
  switch (w.outcome) {
    case "interrupted":
      return { text: "interrupted", tone: "ok", title: "the turn was stopped" };
    case "already_complete":
      return {
        text: "already finished",
        tone: "ok",
        title: "no turn was running — it had already finished",
      };
    case "failed":
      return { text: "stop failed", tone: "error", title: "the stop could not be delivered" };
    default: {
      const never: never = w.outcome;
      throw new Error(`progress-footer: unhandled interrupt outcome ${String(never)}`);
    }
  }
}

/** One live activity, as the detail cell shows it. */
export interface Activity {
  text: string;
  /** Accent class stem, one per window so they read apart at a glance. */
  tone: "tool" | "thinking" | "blocked" | "retry" | "error" | "muted";
  /**
   * Pre-escaped markup for the cell, when the rung needs MORE than one color
   * across its line. Only the rate-limit rung uses it: each allowance's figures
   * carry their own severity accent, which a single cell-wide tone cannot say.
   * Absent means the plain `text` is escaped and shown, which is every other
   * rung.
   */
  html?: string;
}

/**
 * The single activity the detail cell shows, or null when nothing is live.
 *
 * PRECEDENCE, most-blocking first: an auth prompt and a rate limit are the
 * agent stopped dead, a session parked on the user is the agent waiting on
 * YOU, a retry is the agent stalled, a compaction and a hook are the agent
 * busy with something other than the answer, and a running tool is ordinary
 * work. Exactly one speaks, so the cell never stacks two accounts of the same
 * dead air — the whole point of consolidating the old tail rows.
 *
 * The `blocked` window sits above the retry/compaction/tool band because
 * "nothing will happen until you act" outranks any account of the agent being
 * busy. It sits BELOW auth and rate limits, which are the same statement with
 * a specific remedy attached.
 */
export function activityDetail(input: FooterInput, nowMs: number): Activity | null {
  const p = input.progress;
  if (p === null) return null;
  if (p.authenticating !== null) {
    return { text: authText(p.authenticating.detail), tone: "error" };
  }
  if (rateLimitIsNews(p)) {
    return rateLimitActivity(p, nowMs);
  }
  if (p.blocked !== null) {
    return { text: p.blocked.detail, tone: "retry" };
  }
  if (p.retrying !== null) {
    return { text: `retrying · ${p.retrying.detail}`, tone: "retry" };
  }
  // THE MERGE, above the hook and the running tool. While a merge owns the
  // session the user cannot prompt it, so the one thing the cell can usefully
  // say is what the merge is doing with it — the commit in hand, or the
  // pre/post-merge prompt the daemon is running. A tool call under a merge is
  // that prompt's own work; naming the tool instead spent the slot on a detail
  // of something the cell had not yet said was happening.
  const merge = mergeFacts(input.mergeStatus);
  if (merge !== null && merge.activity !== "") {
    // `muted` for the in-flight band and `error` for a stopped run, matching
    // the tones the phase word beside it already wears. `ok` has no activity
    // to carry (a settled merge is doing nothing), so it never reaches here.
    return { text: merge.activity, tone: merge.tone === "error" ? "error" : "muted" };
  }
  // NO COMPACTION RUNG. The compaction has its own PHASE WORD now
  // (phaseLabel's `compacting`), and the detail cell's job is to say what is
  // live BESIDES the phase — repeating it here spent the one activity slot
  // restating the word immediately to its left, and buried the hook or tool
  // that was the only thing the cell could have added.
  // ProgressView.compacting stays on the wire and in the expansion sheet.
  if (p.hook !== null) {
    return { text: `hook · ${p.hook.detail}`, tone: "muted" };
  }
  const tool = runningTool(input.items);
  if (tool !== null) {
    return { text: `${tool.toolName} · ${toolElapsed(tool, nowMs)}`, tone: "tool" };
  }
  return null;
}

/**
 * An activity as the cell's inner markup: the rung's own markup when it carries
 * per-part accents, else its plain text escaped. The escaping stays here so a
 * rung that ships no markup can never leak one.
 */
export function activityBody(a: Activity): string {
  return a.html ?? escapeHtml(a.text);
}

/** The auth window's line, or a bare label when it carries no detail. */
function authText(detail: string): string {
  return detail === "" ? "authenticating…" : `auth · ${detail}`;
}

/**
 * The statuses that are the vendor ALLOWING the request. Everything else on the
 * wire (`rejected`, or a pre-status empty string) is the vendor refusing.
 *
 * `allowed_warning` is the vendor saying the request SUCCEEDED while the
 * allowance runs low, which is a very different claim from a refusal — the old
 * line painted both an outright red "rate-limited", which read a working
 * session as stopped.
 */
const WARNING_RATE_LIMIT_STATUSES = new Set(["allowed", "allowed_warning"]);

/** The separator between one allowance's two facts, exactly as specified. */
const RATE_LIMIT_SEP = " • ";

/** The separator BETWEEN the two allowances, which are separate readings. */
const RATE_LIMIT_GROUP_SEP = "  |  ";

/**
 * The severity ladder, floors ascending. An allowance is read at the HIGHEST
 * floor it has reached, so the rung escalates through four colors on the way to
 * spent rather than being either "fine" or "alarming":
 *
 *   <= 50%             green   plenty left
 *   > 50%, <= 75%      yellow  past the halfway mark
 *   > 75%, < 90%       orange  running low
 *   >= 90%             red     effectively spent
 *
 * A refusal (a status outside `WARNING_RATE_LIMIT_STATUSES`) is red regardless
 * of the figure: the vendor has stopped the session, which outranks whatever
 * the utilization says.
 */
export type RateLimitAccent =
  | "pfooter-rl-ok"
  | "pfooter-rl-warn"
  | "pfooter-rl-high"
  | "pfooter-rl-limit";

/** The accent an allowance that has reached no floor wears. */
const RATE_LIMIT_ACCENT_OK: RateLimitAccent = "pfooter-rl-ok";

const RATE_LIMIT_ACCENTS: ReadonlyArray<{
  reached: (utilization: number) => boolean;
  accent: RateLimitAccent;
}> = [
  // The one INCLUSIVE floor: an allowance sitting exactly on 90% is spent, not
  // merely running low.
  { reached: (u) => u >= 0.9, accent: "pfooter-rl-limit" },
  { reached: (u) => u > 0.75, accent: "pfooter-rl-high" },
  { reached: (u) => u > 0.5, accent: "pfooter-rl-warn" },
];

export function rateLimitAccent(r: NonNullable<ProgressInput["rateLimited"]>): RateLimitAccent {
  if (!WARNING_RATE_LIMIT_STATUSES.has(r.status)) return "pfooter-rl-limit";
  return RATE_LIMIT_ACCENTS.find((rung) => rung.reached(r.utilization))?.accent ?? RATE_LIMIT_ACCENT_OK;
}

/** The ladder as an ORDER, quietest first, for picking the loudest of several. */
const RATE_LIMIT_SEVERITY: readonly RateLimitAccent[] = [
  RATE_LIMIT_ACCENT_OK,
  "pfooter-rl-warn",
  "pfooter-rl-high",
  "pfooter-rl-limit",
];

/** The loudest accent any reported allowance wears, for a figure-less rung. */
function worstRateLimitAccent(p: ProgressInput): RateLimitAccent {
  let worst = RATE_LIMIT_ACCENT_OK;
  for (const { key } of RATE_LIMIT_ALLOWANCES) {
    const r = p[key];
    if (r === null) continue;
    const accent = rateLimitAccent(r);
    if (RATE_LIMIT_SEVERITY.indexOf(accent) > RATE_LIMIT_SEVERITY.indexOf(worst)) worst = accent;
  }
  return worst;
}

/** The two allowances the vendor bills, in the order the rung names them. */
const RATE_LIMIT_ALLOWANCES = [
  { key: "rateLimited", label: "Session" },
  { key: "rateLimitedWeekly", label: "Weekly" },
] as const;

/**
 * Whether the rate-limit rung has NEWS — a report the reader should be shown
 * ahead of the ordinary activity the cell would otherwise carry.
 *
 * Only an ACTIVE window counts. A quiet allowance still ships its figures (so
 * the rung can name both readings side by side once one of them speaks up), but
 * on its own it is not a reason to displace the running tool from the cell.
 */
export function rateLimitIsNews(p: ProgressInput): boolean {
  return RATE_LIMIT_ALLOWANCES.some((a) => p[a.key]?.active === true);
}

/**
 * The rate-limit rung: for EACH allowance the vendor bills, how much of it is
 * spent and how long until it resets — the facts a reader can actually act on.
 *
 * The two allowances are named APART. They used to share one line labeled
 * "Session", so a weekly figure deep into its allowance read as the session's:
 * a reader saw "91%" against a session they had barely touched, with no way to
 * tell which of the two the number belonged to.
 *
 * Each allowance's figures carry their OWN accent (see `rateLimitAccent`),
 * which is why this rung ships markup rather than a single cell-wide tone — the
 * whole point is that the two can read very differently at the same moment. The
 * labels stay muted so the figures are what the eye lands on.
 *
 * An allowance the vendor never reported is absent entirely, and either of a
 * reported allowance's parts drops out when its own figure is missing. With
 * nothing left to say the rung falls back to a worded line, since an empty cell
 * would drop the fact that a rate-limit window is open at all.
 */
export function rateLimitActivity(p: ProgressInput, nowMs: number): Activity {
  const groups: Array<{ accent: RateLimitAccent; parts: Array<[string, string]> }> = [];
  for (const { key, label } of RATE_LIMIT_ALLOWANCES) {
    const r = p[key];
    if (r === null) continue;
    const parts: Array<[string, string]> = [];
    if (r.utilization > 0) parts.push([`${label} usage: `, `${Math.round(r.utilization * 100)}%`]);
    // The deadline is a COUNTDOWN, not a wall-clock reading: the weekly window
    // can be days out, where a time of day says nothing about the wait.
    if (r.resetsAt > 0) {
      parts.push([`${label} reset: `, formatCountdown(r.resetsAt * 1000 - nowMs)]);
    }
    if (parts.length > 0) groups.push({ accent: rateLimitAccent(r), parts });
  }
  if (groups.length === 0) {
    // The LOUDEST accent among the reported allowances, not a neutral one: a
    // refusal that carries no figures is still a session stopped dead, and
    // painting it green would say the opposite of what it means.
    const accent = worstRateLimitAccent(p);
    const text = "Rate limit reported";
    return { text, tone: "muted", html: `<span class="${accent}">${escapeHtml(text)}</span>` };
  }
  const text = groups
    .map((g) => g.parts.map(([label, value]) => `${label}${value}`).join(RATE_LIMIT_SEP))
    .join(RATE_LIMIT_GROUP_SEP);
  const html = groups
    .map((g) =>
      g.parts
        .map(([label, value]) => `${escapeHtml(label)}<span class="${g.accent}">${escapeHtml(value)}</span>`)
        .join(escapeHtml(RATE_LIMIT_SEP)),
    )
    .join(escapeHtml(RATE_LIMIT_GROUP_SEP));
  return { text, tone: "muted", html };
}

/**
 * The tool call running at the feed's tail, or null when none is. Scans
 * tail-first and takes the newest unsettled call, which is the one whose
 * heartbeat is still arriving.
 */
export function runningTool(items: readonly ConversationItem[]): ToolItem | null {
  for (let i = items.length - 1; i >= 0; i--) {
    const item = items[i];
    if (item.kind !== "tool") continue;
    if (item.result) continue;
    if (item.toolName === "") continue; // a result-only shell awaiting its pair
    return item;
  }
  return null;
}

/**
 * How long the running tool has been going: the daemon's own heartbeat clock
 * when one has arrived (authoritative, and survives a reconnect), else this
 * tab's reading of the call's start stamp.
 */
export function toolElapsed(tool: ToolItem, nowMs: number): string {
  if (tool.progressElapsedS !== undefined) {
    return formatElapsed(tool.progressElapsedS * 1000);
  }
  return formatElapsed(nowMs - Date.parse(tool.ts));
}

/**
 * The token cell: the CURRENT TURN's new input tokens, with the thinking
 * ticker beside it while the model is reasoning.
 *
 * There is deliberately no output figure — the running output-token count is
 * explicitly unwanted here, and the session-wide total lives in the topbar's
 * tokens chip.
 *
 * BETWEEN TURNS it reads the idle dash, exactly as the clock cell does. The
 * turn's final figure is not lost by clearing: it is stamped into the
 * top-right corner of the final-response bubble that turn produced (see
 * `resultMeta`), where it persists and replays with the conversation. A
 * footer that kept showing it would be reporting a turn the feed has already
 * accounted for, and would go on doing so until the next turn overwrote it.
 */
export function tokenCellHtml(p: ProgressInput): string {
  const parts: string[] = [];
  if (p.inputTokens > 0) {
    parts.push(`<span class="info-tokens">${escapeHtml(compactTokens(p.inputTokens))} in</span>`);
  } else if (p.thinkingTokens === 0) {
    parts.push(`<span class="info-tokens">${escapeHtml(IDLE_LABEL)}</span>`);
  }
  if (p.thinkingTokens > 0) {
    parts.push(
      `<span class="pfooter-thinking-tokens">${escapeHtml(compactTokens(p.thinkingTokens))} thought</span>`,
    );
  }
  return parts.join(" ");
}

/**
 * The counters cluster: the two rosters relocated from the topbar, plus the
 * queue-depth and pending-permission badges.
 *
 * The rosters go through the SHARED `counter-menu` facade (`agentsMenuHtml` /
 * `tasksMenuHtml`) rather than a footer-local reimplementation, so the chips
 * look and behave exactly as they did in the topbar and cannot drift from it.
 * Every element hides at zero: a badge over nothing is chrome with no news.
 */
export function countersHtml(input: FooterInput, open: FooterDisclosure): string {
  const parts: string[] = [];
  const agents = agentsMenuHtml(input.agents, open.agentsOpen);
  if (agents !== "") parts.push(agents);
  const tasks = tasksMenuHtml(input.tasks, open.tasksOpen);
  if (tasks !== "") parts.push(tasks);
  const p = input.progress;
  if (p !== null && p.queueDepth > 0) {
    parts.push(
      `<span class="pfooter-badge queued" title="prompts the daemon is holding">${p.queueDepth} queued</span>`,
    );
  }
  if (p !== null && p.pendingPermissions > 0) {
    parts.push(
      `<span class="pfooter-badge perm" title="permission prompts waiting on you">${p.pendingPermissions} perm</span>`,
    );
  }
  return parts.join(" ");
}

/**
 * The expansion sheet: one row per fact the collapsed strip had no cell for.
 *
 * It is the home for the detail the thin strip drops — every OPEN window with
 * its age, and the counts the cluster shows only as bare numbers. The task
 * roster and the per-model token breakdown join it when their deferred work
 * lands.
 */
export function sheetHtml(input: FooterInput, nowMs: number): string {
  const p = input.progress;
  if (p === null) return "";
  const rows: string[] = [];
  const windows: ReadonlyArray<[string, ProgressInput["compacting"]]> = [
    ["compacting", p.compacting],
    ["retrying", p.retrying],
    ["authenticating", p.authenticating],
    ["hook", p.hook],
    ["blocked", p.blocked],
  ];
  for (const [name, w] of windows) {
    if (w === null) continue;
    const age = w.sinceMs > 0 ? ` (${formatElapsed(nowMs - w.sinceMs)})` : "";
    const detail = w.detail === "" ? "" : ` — ${w.detail}`;
    rows.push(`<span>${escapeHtml(`${name}${age}${detail}`)}</span>`);
  }
  // The sheet says the rate limit in the SAME words and the same accents as the
  // strip's rung: one fact reading two ways in one footer is how the old
  // "rate-limited" line survived a rewording of the cell above it.
  //
  // It shows BOTH allowances whenever either has been reported, including the
  // quiet case the strip's rung declines to displace a running tool for — the
  // sheet is where the detail the thin strip drops belongs, and "how much of my
  // week is left" is exactly what a reader opens it to find out.
  if (p.rateLimited !== null || p.rateLimitedWeekly !== null) {
    rows.push(`<span>${activityBody(rateLimitActivity(p, nowMs))}</span>`);
  }
  // The merge's full account, including the parts the thin strip drops: the
  // run's arithmetic when the chip had no room, and the commit or prompt in
  // hand when a louder rung displaced it from the activity cell.
  const merge = mergeFacts(input.mergeStatus);
  if (merge !== null) {
    const parts = [merge.word, merge.count, merge.activity].filter((part) => part !== "");
    rows.push(`<span>${escapeHtml(parts.join(" · "))}</span>`);
  }
  if (p.liveTaskCount > 0) {
    rows.push(`<span>${p.liveTaskCount} live task${p.liveTaskCount === 1 ? "" : "s"}</span>`);
  }
  if (p.ttftMs > 0) {
    rows.push(`<span>first token in ${escapeHtml(formatElapsed(p.ttftMs))}</span>`);
  }
  if (rows.length === 0) {
    rows.push(`<span class="pfooter-sheet-empty">nothing else in flight</span>`);
  }
  return `<div class="pfooter-sheet">${rows.join("")}</div>`;
}

/**
 * The error row: a persistent line carrying the daemon's classified failure,
 * standing until the next turn starts. When the failure names a feed item the
 * row is a button, because clicking it scrolls the feed to that card — the row
 * is the only way to find a failure that has already scrolled away.
 *
 * Its color is the failure's CLASS, from the same five-color assignment the
 * card and the workspace dot take. It used to be a hardcoded red no other
 * surface consulted, so the footer could contradict the very card it pointed
 * at.
 */
export function errorRowHtml(p: ProgressInput): string {
  const f = p.failure;
  if (f === null) return "";
  const addressable = f.uuid !== "";
  const attrs = addressable
    ? ` role="button" tabindex="0" data-pfooter-error-uuid="${escapeHtml(f.uuid)}" title="show the error in the feed"`
    : "";
  const classes = ["pfooter-error", `failure-${f.errorClass.toLowerCase()}`];
  if (addressable) classes.push("addressable");
  return `<div class="${classes.join(" ")}"${attrs}>${escapeHtml(f.message)}</div>`;
}

/**
 * The merge note as a standing row under the strip, or "" when the run has
 * nothing to add.
 *
 * It is a ROW rather than a cell because both notes it carries are sentences,
 * not figures: a failure's classified cause, and a landed merge whose
 * after-action did not succeed. Neither fits a cell sized for a phase word, and
 * neither may be hidden behind the expansion — a merge that stopped is exactly
 * the state a user opens the tab to understand.
 *
 * It sits BESIDE the progress failure row rather than inside it: that row is
 * the daemon's classified turn failure, and a merge note is a different fact
 * with a different lifetime.
 */
export function mergeNoteRowHtml(status: MergeStatus | null): string {
  const facts = mergeFacts(status);
  if (facts === null || facts.note === "") return "";
  return (
    `<div class="pfooter-merge-note ${facts.tone}">${escapeHtml(facts.note)}</div>`
  );
}

/** Marks the footer's whole clickable strip, so the expansion toggle delegates. */
export const FOOTER_STRIP_ATTR = "data-pfooter-strip";

/**
 * The whole dock. Returns "" before the daemon has resolved anything, so the
 * slot collapses rather than showing an empty chrome bar.
 */
export function footerHtml(
  input: FooterInput,
  open: FooterDisclosure,
  nowMs: number = Date.now(),
  breath: BreathState = { shade: 0, elapsedMs: 0 },
): string {
  const p = input.progress;
  if (p === null) return "";
  const cells: string[] = [];
  // The phase cell is present only once a state has actually been resolved.
  // Before that there is no phase to name, and naming one anyway is exactly
  // the fabrication the stale mirror used to commit.
  // The structured merge status names the phase where it has one, because it
  // can name the two the render state cannot (before-action, after-action).
  const phase =
    mergeStatusPhaseLabel(input.mergeStatus) ??
    (input.renderState === null ? null : phaseLabel(input.renderState));
  if (phase !== null) {
    const secondary =
      input.connectivity !== null &&
      input.connectivity !== "operational" &&
      input.sessionStatus !== null
        ? `<span class="pfooter-secondary">${escapeHtml(input.sessionStatus.replaceAll("_", " "))}</span>`
        : "";
    // A working phase wears the WORD as its own liveness signal — no arc beside
    // it. Both of the breath's channels are inline styles rather than classes,
    // and neither is optional:
    //
    //   the NEGATIVE animation-delay resumes the oscillation exactly where the
    //   previous element left it, which is what stops the rewrite this function
    //   performs every chrome frame from restarting the breath on every frame
    //   (and, with it, on every arriving ProgressView);
    //
    //   the color is the ramp stop the arrival counter is standing on, which is
    //   the channel that DOES move per arrival, and it overrides the phase
    //   tone's own hue for as long as the word is working.
    //
    // A resting phase renders exactly as before: bare text in its tone color.
    const word = phase.breathing
      ? `<span class="pfooter-breath" style="color:${breathColor(breath.shade)};` +
        `animation-delay:-${Math.round(breath.elapsedMs)}ms">${escapeHtml(phase.word)}</span>`
      : escapeHtml(phase.word);
    cells.push(
      `<div class="pfooter-cell pfooter-phase ${phase.tone}">${word}${secondary}</div>`,
    );
  }
  // The merge figures sit immediately right of the phase word they qualify, so
  // the pair reads as one statement: "merge queued 2/3". One source only —
  // the run's own status.
  const queue = mergeStatusChip(input.mergeStatus);
  if (queue !== null) {
    cells.push(
      `<div class="pfooter-cell pfooter-merge-queue" title="${escapeHtml(queue.title)}">` +
        `${escapeHtml(queue.text)}</div>`,
    );
  }
  // The interrupt chip sits immediately right of the phase: it qualifies the
  // phase word rather than competing with the activity cell, which stays the
  // home of what is happening NOW. It is a CELL like every other footer
  // signal, and it is present exactly while the daemon's window is open.
  const chip = interruptChip(p);
  if (chip !== null) {
    cells.push(
      `<div class="pfooter-cell pfooter-interrupt ${chip.tone}" title="${escapeHtml(chip.title)}">` +
        `${escapeHtml(chip.text)}</div>`,
    );
  }
  // The activity cell also HOSTS the grabber notch, which is absolutely
  // positioned against it (see `.pfooter-grab`) so the notch centers on the
  // middle section rather than on the whole dock. This cell is pushed
  // unconditionally, so the notch renders in every state the footer shows,
  // including the one where the activity text is empty.
  const activity = activityDetail(input, nowMs);
  cells.push(
    `<div class="pfooter-cell pfooter-grow ${activity ? activity.tone : "muted"}">` +
      `${activity ? activityBody(activity) : ""}` +
      `<div class="pfooter-grab" aria-hidden="true"></div></div>`,
  );
  // The clock cell always renders while a turn is in flight: it owns the one
  // span the tick repaints, so dropping it would leave the tick nothing to
  // write to. Off-turn it shows the idle label rather than disappearing, so the
  // strip's cell geometry does not jump on every turn boundary.
  const label = p.turnStartedAtMs > 0 ? input.timerLabel : IDLE_LABEL;
  cells.push(
    `<div class="pfooter-cell pfooter-clock">` +
      `<span class="info-time" ${TIMER_SLOT}>${escapeHtml(label)}</span></div>`,
  );
  const tokens = tokenCellHtml(p);
  if (tokens !== "") cells.push(`<div class="pfooter-cell pfooter-tokens">${tokens}</div>`);
  const counters = countersHtml(input, open);
  if (counters !== "") cells.push(`<div class="pfooter-cell pfooter-counters">${counters}</div>`);

  return (
    `<div class="pfooter" role="status" aria-live="polite">` +
    `<div class="pfooter-cells" ${FOOTER_STRIP_ATTR} role="button" tabindex="0" ` +
    `aria-expanded="${open.expanded}" title="click for detail">${cells.join("")}</div>` +
    mergeNoteRowHtml(input.mergeStatus) +
    errorRowHtml(p) +
    (open.expanded ? sheetHtml(input, nowMs) : "") +
    `</div>`
  );
}

// --- click delegation --------------------------------------------------------

/** The subset of an element the footer's click classifier reads. */
export interface FooterClickTarget {
  closest(selector: string): { getAttribute(name: string): string | null } | null;
}

/** One footer click's meaning. */
export type FooterClick =
  | { kind: "toggle-menu"; menu: "agents" | "tasks" }
  | { kind: "reveal-agent"; agentId: string }
  | { kind: "reveal-task"; taskId: string }
  | { kind: "reveal-error"; uuid: string }
  | { kind: "toggle-expand" };

/**
 * Classify a click inside the footer, or null for a click on nothing
 * actionable.
 *
 * The roster verbs are checked BEFORE the strip's expansion toggle: a counter
 * chip and its rows live inside the strip, so testing the strip first would
 * swallow every roster click into an expansion. The error row is likewise its
 * own verb, since it sits outside the strip entirely.
 */
export function footerClickAction(target: FooterClickTarget): FooterClick | null {
  if (target.closest("[data-agents-toggle]")) return { kind: "toggle-menu", menu: "agents" };
  if (target.closest("[data-tasks-toggle]")) return { kind: "toggle-menu", menu: "tasks" };
  const agentId = target.closest(".agent-row")?.getAttribute("data-agent-id");
  if (agentId) return { kind: "reveal-agent", agentId };
  const taskId = target.closest(".task-row")?.getAttribute("data-task-id");
  if (taskId) return { kind: "reveal-task", taskId };
  const uuid = target
    .closest("[data-pfooter-error-uuid]")
    ?.getAttribute("data-pfooter-error-uuid");
  if (uuid) return { kind: "reveal-error", uuid };
  if (target.closest(`[${FOOTER_STRIP_ATTR}]`)) return { kind: "toggle-expand" };
  return null;
}

/** Whether either roster has anything to show (the cluster's own gate). */
export function hasLiveCounters(input: FooterInput): boolean {
  return input.agents.some(isActive) || input.tasks.some(isActive);
}

/** The two counter specs the footer hosts, for the caller's reveal notices. */
export const FOOTER_COUNTER_SPECS: Readonly<Record<"agents" | "tasks", CounterSpec>> = {
  agents: AGENTS_SPEC,
  tasks: TASKS_SPEC,
};

// --- the DOM owner -----------------------------------------------------------

/**
 * The footer's slot renderer: owns one element and rewrites it per frame, with
 * the once-a-second clock tick going through `paintTurnTimer` instead.
 */
export class ProgressFooter {
  private open: FooterDisclosure = { agentsOpen: false, tasksOpen: false, expanded: false };
  /**
   * The breathing phase word's bookkeeping. It lives on the OWNER rather than
   * in the markup for the same reason the disclosure does: `render` rewrites
   * the dock wholesale, so anything held in the DOM is lost every frame — and
   * for the breath that loss is precisely the reset the design forbids.
   */
  private readonly breath = new BreathingTicker();

  constructor(
    private readonly el: HTMLElement,
    private readonly now: () => number = () => Date.now(),
  ) {}

  /** Rewrite the dock. Every value it interpolates is escaped in the builders. */
  render(input: FooterInput): void {
    // Step the color ramp FIRST, so this render paints the shade belonging to
    // the view it is rendering rather than the previous one's.
    this.breath.observe(input.progress);
    const now = this.now();
    this.el.innerHTML = footerHtml(input, this.open, now, this.breath.state(now));
  }

  /**
   * Repaint JUST the turn clock's span.
   *
   * The tick is deliberately NOT a re-render: rewriting the dock once a second
   * would churn the counter overlays and the sheet in service of one changing
   * digit, and re-rendering the FEED for it (which the nuked stats row's
   * ancestor once did) would be far worse. Same discipline as the topbar's old
   * `TaskTimer` slot.
   */
  paintTurnTimer(label: string): void {
    const slot = this.el.querySelector(`[${TIMER_SLOT}]`);
    if (slot) slot.textContent = label;
  }

  /** Which overlay is open, or null. Flipping one closes any other. */
  setMenu(menu: "agents" | "tasks" | null): void {
    this.open = {
      ...this.open,
      agentsOpen: menu === "agents",
      tasksOpen: menu === "tasks",
    };
  }

  /** Flip the detail sheet. */
  toggleExpanded(): void {
    this.open = { ...this.open, expanded: !this.open.expanded };
  }

  /** Close every overlay (a click-away, or the feed taking focus). */
  closeMenus(): void {
    this.open = { ...this.open, agentsOpen: false, tasksOpen: false };
  }

  /** The current disclosure, for the caller's own bookkeeping. */
  disclosure(): FooterDisclosure {
    return this.open;
  }
}
