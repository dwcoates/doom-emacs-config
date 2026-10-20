/**
 * switch-latency — HOW LONG A SWITCH TO THIS PAGE, AND THIS PAGE'S BOOT, TOOK.
 *
 * Two of the user-facing latencies in proto/vocab/latencies.json end in this
 * page, so this page measures them and hands them to latency-report.ts:
 *
 *   - A SWITCH starts in Emacs, at the keystroke, and Emacs hands its instant
 *     to the webhost with the show (host/protocol.ts `switchStartedAtMs`). It
 *     ends here:
 *       - `switch_repeat` when the page had already drawn its conversation:
 *         at the second animation frame after the show, once the first frame
 *         showing the page has painted (the moment `page.shown-frame` records);
 *       - `switch_first` when it had not: at its conversation drawn
 *         (feed/conversation-drawn.ts), the moment a never-shown page stops
 *         being blank. A page hidden again before it drew measures nothing:
 *         that switch never finished.
 *     Both clocks are the host's wall clock (wall-latency.ts).
 *   - `page_boot` runs from this page booting to its conversation drawn, on
 *     the page's own monotonic clock.
 */
import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { CONVERSATION_DRAWN_ATTRIBUTE } from "./feed/conversation-drawn.js";
import { pageMutationObserver } from "./dom.js";
import { nextFrame } from "./frame.js";
import type { Presence } from "./presence.js";
import { recordWallLatency, type RecordLatency } from "./wall-latency.js";

export type { RecordLatency } from "./wall-latency.js";

/** The clocks, frames and drawn mark the latencies are measured with. */
export interface SwitchLatencyEnv {
  /** Wall-clock Unix milliseconds, the clock Emacs stamps a switch with. */
  readonly wallNow: () => number;
  /** Monotonic milliseconds. */
  readonly monoNow: () => number;
  /** Run FN on the next animation frame. */
  readonly frame: (fn: () => void) => void;
  /** Whether the page has drawn its conversation. */
  readonly drawn: () => boolean;
  /** Call FN once the page has drawn its conversation; answers the cancel. */
  readonly onDrawn: (fn: () => void) => () => void;
}

/** The page's own clocks, frames and drawn mark. */
export function pageSwitchLatencyEnv(doc: Document): SwitchLatencyEnv {
  const drawn = (): boolean => doc.documentElement.hasAttribute(CONVERSATION_DRAWN_ATTRIBUTE);
  return {
    wallNow: () => Date.now(),
    monoNow: () => performance.now(),
    frame: nextFrame,
    drawn,
    onDrawn(fn) {
      if (drawn()) {
        fn();
        return () => undefined;
      }
      const watch = pageMutationObserver(doc, () => {
        if (!drawn()) return;
        watch.disconnect();
        fn();
      });
      watch.observe(doc.documentElement, { attributes: true, attributeFilter: [CONVERSATION_DRAWN_ATTRIBUTE] });
      return () => watch.disconnect();
    },
  };
}

/** Measure every switch to the page PRESENCE describes; answers the stop. */
export function watchSwitchLatency(presence: Presence, env: SwitchLatencyEnv, record: RecordLatency): () => void {
  let shown = presence.shown();
  let cancelFirst: (() => void) | undefined;
  const stop = presence.onChange(() => {
    const now = presence.shown();
    const edge = now && !shown;
    if (!now && shown) {
      // HIDDEN AGAIN: a first switch still waiting on the drawn page never
      // finished, so it measures nothing.
      cancelFirst?.();
      cancelFirst = undefined;
    }
    shown = now;
    if (!edge) return;
    const startedAtMs = presence.switchStartedAt();
    if (startedAtMs === null) return;
    if (env.drawn()) {
      env.frame(() => {
        env.frame(() => recordWallLatency(ReportedLatencyKind.SWITCH_REPEAT, startedAtMs, env.wallNow(), record));
      });
      return;
    }
    cancelFirst = env.onDrawn(() => {
      cancelFirst = undefined;
      recordWallLatency(ReportedLatencyKind.SWITCH_FIRST, startedAtMs, env.wallNow(), record);
    });
  });
  return () => {
    stop();
    cancelFirst?.();
  };
}

/** Measure this page's boot, begun at BOOTED_AT (monotonic ms); answers the stop. */
export function watchPageBoot(bootedAtMs: number, env: SwitchLatencyEnv, record: RecordLatency): () => void {
  return env.onDrawn(() => record(ReportedLatencyKind.PAGE_BOOT, (env.monoNow() - bootedAtMs) / 1000));
}
