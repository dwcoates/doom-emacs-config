/**
 * The log level window every TypeScript runtime answers identically.
 *
 * A level other than info is a WINDOW, never a standing setting:
 * `AGENT_REPL_LOG_LEVEL` selects debug, warn or error only together with
 * `AGENT_REPL_LOG_LEVEL_UNTIL`, the Unix second the window ends at, no more
 * than five minutes after the runtime reads it. Anything else starts at info,
 * and a window that ends while the runtime runs reverts to info by itself.
 * `agent-shim/logging/go` (window.go) holds the same answer for Go and
 * `core.el` for elisp; `proto/vocab/log-level-window.json` is the seam the
 * three are asserted against.
 */

export type WindowLevel = "debug" | "info" | "warn" | "error";

/** The environment variable naming a level window's end. */
export const UNTIL_ENV = "AGENT_REPL_LOG_LEVEL_UNTIL";

/** The longest a level other than info may last, in seconds. */
export const WINDOW_SECONDS = 300;

const LEVELS: readonly WindowLevel[] = ["debug", "info", "warn", "error"];
const RANK: Readonly<Record<WindowLevel, number>> = { debug: 0, info: 1, warn: 2, error: 3 };

/** What a runtime made of its startup level setting. */
export type LevelOutcome = "default" | "honored" | "no_expiry" | "expired" | "beyond_window";

/** A runtime's startup level decision. */
export interface LevelSelection {
  /** The level setting as read; undefined when unset. */
  requested: string | undefined;
  /** The window setting as read; undefined when unset. */
  requestedUntil: string | undefined;
  /** The level the runtime starts at. */
  level: WindowLevel;
  /** When `level` ends, in epoch milliseconds; null when it never ends. */
  untilMs: number | null;
  /** The decision. */
  outcome: LevelOutcome;
}

function describe(value: string): string {
  return JSON.stringify(value);
}

/** Validate one level name; LABEL names where it came from in the refusal. */
export function parseWindowLevel(value: string, label: string): WindowLevel {
  if ((LEVELS as readonly string[]).includes(value)) return value as WindowLevel;
  throw new Error(`${label} must be one of ${LEVELS.join("|")}; got ${describe(value)}`);
}

/**
 * Decide the startup level from LEVEL and UNTIL (Unix seconds) at NOW_MS.
 * An unset LEVEL is info. An unknown level or an UNTIL that is not a decimal
 * integer is a refusal. LABEL names the level's source in a refusal.
 */
export function selectLevel(
  level: string | undefined,
  until: string | undefined,
  nowMs: number,
  label = "AGENT_REPL_LOG_LEVEL",
): LevelSelection {
  const parsed = level === undefined ? "info" : parseWindowLevel(level, label);
  let endMs: number | null = null;
  if (until !== undefined && until !== "") {
    if (!/^[+-]?[0-9]+$/.test(until)) throw new Error(`${UNTIL_ENV} must be a Unix second; got ${describe(until)}`);
    endMs = Number(until) * 1000;
  }
  const selection: LevelSelection = { requested: level, requestedUntil: until, level: "info", untilMs: null, outcome: "default" };
  if (parsed === "info") return selection;
  if (endMs === null) return { ...selection, outcome: "no_expiry" };
  if (nowMs >= endMs) return { ...selection, outcome: "expired" };
  if (endMs - nowMs > WINDOW_SECONDS * 1000) return { ...selection, outcome: "beyond_window" };
  return { ...selection, level: parsed, untilMs: endMs, outcome: "honored" };
}

/**
 * The info record text when the runtime started at info although another
 * level was asked for; null when it started where it was asked to (the
 * default, or an honored window, whose volume states it).
 */
export function selectionNote(selection: LevelSelection): string | null {
  const requested = describe(selection.requested ?? "");
  switch (selection.outcome) {
    case "no_expiry":
      return `log level ${requested} ignored without ${UNTIL_ENV}; starting at info`;
    case "expired":
      return `log level ${requested} ignored: its window ended at ${selection.requestedUntil}; starting at info`;
    case "beyond_window":
      return `log level ${requested} ignored: its window ends at ${selection.requestedUntil}, more than ${WINDOW_SECONDS}s away; starting at info`;
    case "default":
    case "honored":
      return null;
  }
}

/** The structured evidence of a startup selection. */
export function selectionContext(selection: LevelSelection): Record<string, unknown> {
  return {
    requested_level: selection.requested ?? "",
    requested_until: selection.requestedUntil ?? "",
    effective_level: selection.level,
    outcome: selection.outcome,
    ...(selection.untilMs === null ? {} : { until: Math.floor(selection.untilMs / 1000) }),
  };
}

/** A level window that ended: the level it held and when. */
export interface LevelExpiry {
  from: WindowLevel;
  untilMs: number;
}

/** The info record text for a revert. */
export function expiryMessage(expiry: LevelExpiry): string {
  return `log level ${expiry.from} window ended at ${new Date(expiry.untilMs).toISOString()}; reverted to info`;
}

/** The structured evidence of a revert. */
export function expiryContext(expiry: LevelExpiry): Record<string, unknown> {
  return { from_level: expiry.from, until: Math.floor(expiry.untilMs / 1000), effective_level: "info", outcome: "window_ended" };
}

/**
 * A runtime's live threshold. When it is not info it reverts to info once its
 * window ends; the first `allows` call to find that hands the expiry back for
 * the caller to record at info.
 */
export class LevelWindow {
  private current: WindowLevel;
  private untilMs: number | null;

  constructor(selection: Pick<LevelSelection, "level" | "untilMs">, private readonly now: () => number = Date.now) {
    this.current = selection.level;
    this.untilMs = selection.untilMs;
  }

  /** A window that never ends. */
  static fixed(level: WindowLevel): LevelWindow {
    return new LevelWindow({ level, untilMs: null });
  }

  /** Whether a record at LEVEL passes now, and the expiry this call found. */
  allows(level: WindowLevel): { allowed: boolean; ended: LevelExpiry | null } {
    let ended: LevelExpiry | null = null;
    if (this.untilMs !== null && this.now() >= this.untilMs) {
      ended = { from: this.current, untilMs: this.untilMs };
      this.current = "info";
      this.untilMs = null;
    }
    return { allowed: RANK[level] >= RANK[this.current], ended };
  }

  /** The threshold in force, without checking the window's end. */
  level(): WindowLevel {
    return this.current;
  }
}
