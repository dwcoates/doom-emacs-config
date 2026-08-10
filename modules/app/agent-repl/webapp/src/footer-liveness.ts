/**
 * footer-liveness — THE one choke point at which the progress footer's
 * live-activity figures acquire their provenance.
 *
 * WHY IT EXISTS. The footer used to render whatever the store last held. A
 * page whose socket had gone, or whose workspace no longer had a live session
 * behind it, therefore went on painting the last frame it ever received — a
 * `monitoring` phase word, "1 live task", subagent rows with clocks — for as
 * long as the tab stayed open. Every one of those figures was a claim about
 * RIGHT NOW made out of a memory, and nothing on screen said so.
 *
 * THE SHAPE, not a check. "Clear the footer when the link is down" as a rule
 * applied at each render site is a rule that the next figure added to the
 * footer will not know about. So liveness is carried BY THE DATA instead: the
 * footer's whole live payload (`FooterInput`) is BRANDED, and the brand's
 * symbol is private to this module. `resolveFooterLiveness` is consequently the
 * only expression in the program that can produce one, and the only thing it
 * hands back when the link is down or the workspace is unwired is the
 * `unknown` arm, WHICH CARRIES NO DATA AT ALL.
 *
 * That is the whole guarantee, and it is a compile-time one: a renderer holding
 * the `unknown` arm has no progress view, no roster, no items and no clock
 * reading to paint, so "paint the last value we saw" is not a mistake the
 * footer can make — there is no expression for it. A figure added by later work
 * (the first-class task roster's rows, say) inherits the gate by construction,
 * because it can only be reached through the arm that resolved live.
 *
 * "the last value we saw" is therefore not a state this vocabulary can express:
 * a figure is resolved from a live source, or it is absent.
 */
import type { CounterEntry } from "./counter-menu.js";
import type { FooterAgentRow } from "./progress-footer.js";
import type { MergeStatus, ProgressInput, WebRenderState } from "./state-adapter.js";
import type { ConversationItem } from "./store.js";
import type { ClientLogContext } from "./protocol.js";
import type { ClientLogLevel } from "./wslog.js";

/**
 * The brand. It is `declare const`, so it exists only in the type system and
 * no module — this one included — can write the key at runtime. Not exported,
 * so no other module can even name it: that is what makes `FooterInput`
 * unconstructible outside `resolveFooterLiveness` below.
 */
declare const RESOLVED_FROM_LIVE: unique symbol;

/**
 * WHY the footer cannot verify its live figures right now.
 *
 * The three are ordered as `resolveFooterLiveness` tests them, most
 * fundamental first: a dead socket makes every later question unanswerable,
 * and an unwired workspace makes the view's own freshness moot.
 */
export type LivenessGap =
  /** This page's link to the daemon is not current, so nothing is arriving. */
  | "link-down"
  /**
   * The daemon has no live session for the workspace — the wired axis's closed
   * half (`severed` / `hibernated` / `dead`), the same fact the held-prompt
   * drain gate consults through `drainableRenderState`.
   */
  | "workspace-unwired"
  /** No `ProgressView` has been published for this workspace at all. */
  | "view-absent";

/** The gap in the reader's words, for the strip's one muted cell. */
export const LIVENESS_GAP_TEXT: Readonly<Record<LivenessGap, string>> = {
  "link-down": "link to the daemon is down — live activity cannot be verified",
  "workspace-unwired": "no live session for this workspace — live activity cannot be verified",
  "view-absent": "no live view for this workspace — live activity cannot be verified",
};

/** The raw parts one footer render is built from, before they are resolved. */
export interface FooterParts {
  /** The daemon's resolved view, or null before the first one lands. */
  progress: ProgressInput | null;
  renderState: WebRenderState | null;
  mergeStatus: MergeStatus | null;
  agents: readonly FooterAgentRow[];
  tasks: readonly CounterEntry[];
  items: readonly ConversationItem[];
  timerLabel: string;
}

/**
 * Everything one footer render needs, AND the proof that all of it was
 * resolved from a currently live source. The brand is why the second half of
 * that sentence is enforced rather than asserted.
 */
export type FooterInput = Omit<FooterParts, "progress"> & {
  /**
   * Non-nullable, unlike the raw part it comes from: an absent view is the
   * `view-absent` gap, which is the `unknown` arm, which never reaches here.
   */
  progress: ProgressInput;
  readonly [RESOLVED_FROM_LIVE]: true;
};

/**
 * A footer render's input: figures resolved from a live source, or the reason
 * there are none. There is deliberately no third arm and no payload on the
 * second — "stale" is not a thing the footer can be handed.
 */
export type FooterLiveness =
  | { readonly provenance: "live"; readonly live: FooterInput }
  | { readonly provenance: "unknown"; readonly gap: LivenessGap };

/** What the resolve needs to know beyond the parts themselves. */
export interface LivenessProbe {
  /** Whether this page's socket is current (a live link to the daemon). */
  linkUp: boolean;
  /** Whether the daemon has a live session for this workspace. */
  wired: boolean;
}

/**
 * THE choke point: turn the raw parts into either live figures or a named gap.
 *
 * The single cast below is the brand's only mint. It is safe precisely because
 * this is the only place the two gates have both been passed, and it is the
 * only reason a `FooterInput` can exist anywhere in the program.
 */
export function resolveFooterLiveness(probe: LivenessProbe, parts: FooterParts): FooterLiveness {
  if (!probe.linkUp) return { provenance: "unknown", gap: "link-down" };
  if (!probe.wired) return { provenance: "unknown", gap: "workspace-unwired" };
  const progress = parts.progress;
  if (progress === null) return { provenance: "unknown", gap: "view-absent" };
  return {
    provenance: "live",
    live: { ...parts, progress } as FooterInput,
  };
}

/** How long a standing gap waits before it says so again. */
export const LIVENESS_LOG_INTERVAL_MS = 60_000;

/** The record's sink — `clog`'s shape, so the voice matches its neighbours. */
export type LivenessLogSink = (
  level: ClientLogLevel,
  message: string,
  context: ClientLogContext,
) => void;

/**
 * The gap's announcement: ONCE on the edge into a gap, and at most once per
 * `LIVENESS_LOG_INTERVAL_MS` while the same gap stands.
 *
 * It is edge-triggered rather than per-frame because the footer renders on the
 * chrome cadence, and a per-frame record would bury the event it is reporting
 * under thousands of copies of itself. It is ALSO interval-repeated rather than
 * strictly once, because a gap that stands for an hour is a fact a reader
 * scanning any window of the log deserves to find.
 *
 * The record is a `warn` and not swallowed anywhere: the footer going silent is
 * the honest outcome, but it is never a quiet one.
 */
export class FooterLivenessLog {
  private gap: LivenessGap | null = null;
  private lastAtMs = 0;

  constructor(
    private readonly log: LivenessLogSink,
    private readonly intervalMs: number = LIVENESS_LOG_INTERVAL_MS,
  ) {}

  /** Observe one render's resolution, emitting the record it earns. */
  observe(liveness: FooterLiveness, nowMs: number, sessionId: string): void {
    if (liveness.provenance === "live") {
      // The recovery edge resets the gate rather than logging: the repopulated
      // footer is itself the report that the figures are verifiable again, and
      // the next gap must announce itself immediately rather than wait out an
      // interval left over from the previous one.
      this.gap = null;
      return;
    }
    const same = this.gap === liveness.gap;
    if (same && nowMs - this.lastAtMs < this.intervalMs) return;
    this.gap = liveness.gap;
    this.lastAtMs = nowMs;
    this.log(
      "warn",
      `footer live activity cleared: ${liveness.gap} — the page cannot verify ` +
        `what this workspace is doing session=${sessionId}`,
      {
        operation: "webapp.footer-liveness-gap",
        liveness_gap: liveness.gap,
        session_id: sessionId,
      },
    );
  }
}
