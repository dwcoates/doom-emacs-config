/**
 * attention — the roster row's unseen-notification marker, and THE ONE
 * CADENCE it blinks on.
 *
 * THE CADENCE IS THE CONTRACT, not a decoration. `frontend.v1.
 * RosterRowAttention` states it once, for every surface that draws the marker:
 * TWO blinks — 500 ms on, 500 ms off, twice — then a STEADY marker until the
 * daemon clears it. The Emacs tab bar implements the same spec from the same
 * message, and a divergence between the two is a defect rather than a taste
 * difference, which is why the numbers below are constants with the message
 * named beside them instead of values inlined into a stylesheet.
 *
 * WHY A REGISTRY RATHER THAN PER-ELEMENT STATE. The roster is
 * WHOLE-REPLACED on every push, so every row element is thrown away and
 * redrawn — several times a second while a workspace is busy. A blink that
 * lived on the element would restart on each of those pushes and never reach
 * its steady state, which is the one thing the cadence must not do. So the
 * blink's PHASE lives here, keyed by workspace id, and survives the redraws;
 * the elements are re-attached to it each pass. The blink therefore starts on
 * the marker's FIRST APPEARANCE and ends when the marker leaves.
 *
 * WHY ELEMENTS ARE A SET. Both groupings are drawn on every push (one hidden),
 * so one workspace has TWO row elements, and switching grouping must not show
 * a marker mid-cadence in one pane and steady in the other. They are driven
 * together, off the one phase.
 */
import { log } from "../log.js";

/** One blink phase, per `frontend.v1.RosterRowAttention`. */
export const ATTENTION_PHASE_MS = 500;

/** How many full blinks precede the steady marker, per the same message. */
export const ATTENTION_BLINKS = 2;

/** The phases the cadence walks before going steady: on, off, on, off. */
export const ATTENTION_PHASES = ATTENTION_BLINKS * 2;

/** The timer seam, so a suite can drive the cadence without a real clock. */
export interface BlinkTimers {
  setTimeout(fn: () => void, ms: number): number;
  clearTimeout(handle: number): void;
}

/** The page's own timers, which is what production always uses. */
export const WINDOW_TIMERS: BlinkTimers = {
  setTimeout: (fn, ms) => globalThis.setTimeout(fn, ms) as unknown as number,
  clearTimeout: (handle) => globalThis.clearTimeout(handle),
};

/**
 * What `data-blink` says.
 *
 * THE SETTLED MARKER IS LIT, not a third visual state: the cadence is two
 * blinks and then a STEADY marker, and steady means the marker STANDS — so it
 * reads `on` like every other lit phase. That the cadence is over is a
 * separate fact, reported on `data-settled`, and no surface paints it.
 */
export type BlinkState = "on" | "off";

interface Entry {
  /** How many phases have elapsed; `ATTENTION_PHASES` means settled. */
  phase: number;
  /** The row elements drawn for this workspace in the CURRENT pass. */
  elements: Set<HTMLElement>;
  /** The pending phase advance, or null once the marker is steady. */
  handle: number | null;
  /** Whether this pass has seen the marker (see `beginPass`/`endPass`). */
  seen: boolean;
}

/**
 * The live attention markers, by `WorkspaceRef.id`.
 *
 * A drawing pass is bracketed: `beginPass()`, one `mark()` per row element
 * that carries the marker, then `endPass()`. Anything not marked in the pass
 * has had its marker cleared by the daemon and is forgotten — so a marker that
 * comes back later blinks again, which is right: it is a NEW notification.
 */
export class AttentionRegistry {
  private readonly entries = new Map<string, Entry>();

  constructor(private readonly timers: BlinkTimers = WINDOW_TIMERS) {}

  /** Open a pass: every entry is unseen and holds no elements until marked. */
  beginPass(): void {
    for (const entry of this.entries.values()) {
      entry.seen = false;
      entry.elements.clear();
    }
  }

  /**
   * ELEMENT carries the marker for workspace ID.
   *
   * A first appearance starts the cadence; a later pass simply re-attaches the
   * element to the phase already running, which is what stops a re-push from
   * restarting the blink.
   */
  mark(id: string, element: HTMLElement): void {
    let entry = this.entries.get(id);
    if (entry === undefined) {
      log.debug("starting an attention blink", {
        operation: "sidebar.attention.start",
        context: { workspace: id, phases: ATTENTION_PHASES, phase_ms: ATTENTION_PHASE_MS },
      });
      entry = { phase: 0, elements: new Set(), handle: null, seen: true };
      this.entries.set(id, entry);
      entry.elements.add(element);
      this.paint(entry);
      this.schedule(id, entry);
      return;
    }
    entry.seen = true;
    entry.elements.add(element);
    this.paint(entry);
  }

  /** Close a pass: markers the roster no longer carries are dropped. */
  endPass(): void {
    for (const [id, entry] of [...this.entries]) {
      if (entry.seen) continue;
      log.debug("an attention marker was cleared", {
        operation: "sidebar.attention.cleared",
        context: { workspace: id, phase: entry.phase },
      });
      this.stop(entry);
      this.entries.delete(id);
    }
  }

  /** The state ID's marker is drawing in, or null if it carries none. */
  stateOf(id: string): BlinkState | null {
    const entry = this.entries.get(id);
    return entry === undefined ? null : blinkState(entry.phase);
  }

  /** Drop every marker and its pending timer. The mount's teardown. */
  dispose(): void {
    for (const entry of this.entries.values()) this.stop(entry);
    this.entries.clear();
  }

  private schedule(id: string, entry: Entry): void {
    entry.handle = this.timers.setTimeout(() => {
      entry.handle = null;
      entry.phase += 1;
      this.paint(entry);
      // The last phase is the steady marker: nothing further is scheduled, so
      // the marker simply stands until the daemon clears it.
      if (entry.phase < ATTENTION_PHASES) this.schedule(id, entry);
    }, ATTENTION_PHASE_MS);
  }

  private paint(entry: Entry): void {
    const state = blinkState(entry.phase);
    const settled = entry.phase >= ATTENTION_PHASES;
    for (const element of entry.elements) {
      element.setAttribute("data-blink", state);
      element.setAttribute("data-settled", settled ? "true" : "false");
    }
  }

  private stop(entry: Entry): void {
    if (entry.handle !== null) this.timers.clearTimeout(entry.handle);
    entry.handle = null;
    entry.elements.clear();
  }
}

/** Which state a phase index draws: even on, odd off, and settled lit. */
export function blinkState(phase: number): BlinkState {
  if (phase >= ATTENTION_PHASES) return "on";
  return phase % 2 === 0 ? "on" : "off";
}
