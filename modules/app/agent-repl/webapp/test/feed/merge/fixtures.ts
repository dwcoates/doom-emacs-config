/**
 * The merge suites' shared fixtures: tab rows, merge heads, a sub-feed view.
 *
 * NOT A SUITE — vitest collects only `*.test.ts`. It exists so the strip, the
 * body, the queue and the suites suites all build a tab the same way, from the
 * generated schemas rather than by hand.
 */
import { create } from "@bufbuild/protobuf";
import {
  FeedIdSchema,
  FeedMergeSchema,
  FeedMergeTabSchema,
  FeedRowSchema,
  type FeedBreadcrumb,
  type FeedMerge,
  type FeedRow,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { SubfeedView } from "../../../src/feed/renderers.js";
import { orderFor } from "../../feed-order.js";

/** A feed id. */
export function id(value: string): FeedId {
  return create(FeedIdSchema, { value });
}
type FeedId = ReturnType<typeof create<typeof FeedIdSchema>>;

/** How a tab fixture is asked for: its kind, its state, its payload. */
export interface TabSpec {
  kind:
    | "queue"
    | "prePrompt"
    | "rebasing"
    | "conflicts"
    | "tests"
    | "fixes"
    | "committing"
    | "updatingMain"
    | "postPrompt";
  /** `waitingOnUser` only for the agentic kinds that carry it (conflicts, fixes). */
  state: "live" | "settled" | "waitingOnUser";
  /** For `settled`. */
  outcome?: "succeeded" | "failed";
  /** For a settled failure. */
  summary?: string;
  /** When the tab's work began; `TAB_STARTED_AT_MS` unless stated. */
  startedAtMs?: bigint;
  /** For `settled`, when it settled; `TAB_ENDED_AT_MS` unless stated. */
  endedAtMs?: bigint;
  label?: string;
  round?: number;
  /** Extra kind-level fields (`queue`, `progress`, `lines`, `suites`, `log`, …). */
  payload?: Record<string, unknown>;
}

/** When a fixture tab's work began, unless the spec says otherwise. */
export const TAB_STARTED_AT_MS = 1_000n;
/** When a settled fixture tab settled, unless the spec says otherwise. */
export const TAB_ENDED_AT_MS = 5_000n;

/** The smallest legal queue snapshot: this workspace, waiting, alone. */
const DEFAULT_QUEUE = {
  ahead: [],
  current: {
    workspace: { ref: { id: "mine", dir: "/w/mine" } },
    label: { text: "mine" },
    status: { case: "waiting" as const, value: { stageEnteredAtMs: TAB_STARTED_AT_MS } },
  },
  behind: [],
};

/**
 * The smallest legal payload each kind with a REQUIRED message carries, so a
 * fixture that says nothing about it is still a legal tab rather than a
 * malformed view.
 */
const REQUIRED_PAYLOADS: Readonly<Record<string, Record<string, unknown>>> = {
  queue: { queue: DEFAULT_QUEUE },
  rebasing: { progress: { replayed: 0, total: 1 } },
  committing: { subject: { text: "Merge branch 'mine'" } },
  updatingMain: { step: { step: { case: "fetching", value: {} } } },
  fixes: { attempt: { attempt: 1, maxAttempts: 3 } },
};

/** A merge-tab row, built from the generated schemas. */
export function tabRow(rowId: string, spec: TabSpec): FeedRow {
  const startedAtMs = spec.startedAtMs ?? TAB_STARTED_AT_MS;
  const state =
    spec.state === "live"
      ? { case: "live" as const, value: { startedAtMs } }
      : spec.state === "waitingOnUser"
        ? { case: "waitingOnUser" as const, value: { startedAtMs } }
        : {
          case: "settled" as const,
          value: {
            startedAtMs,
            endedAtMs: spec.endedAtMs ?? TAB_ENDED_AT_MS,
            outcome:
              spec.outcome === "failed"
                ? { case: "failed" as const, value: { summary: spec.summary ?? "it failed" } }
                : { case: "succeeded" as const, value: {} },
          },
        };
  // A kind with a required message MUST carry it; the default supplies the
  // smallest legal one, and anything the spec states wins.
  const payload = { ...(REQUIRED_PAYLOADS[spec.kind] ?? {}), ...(spec.payload ?? {}) };
  return create(FeedRowSchema, {
    id: id(rowId),
    order: orderFor(rowId),
    row: {
      case: "mergeTab",
      value: create(FeedMergeTabSchema, {
        label: { text: spec.label ?? spec.kind, round: spec.round ?? 1 },
        kind: {
          case: spec.kind,
          value: { state, ...payload },
        },
      } as never),
    },
  });
}

/** A merge head message. */
export function mergeHead(
  result:
    | { case: "update" }
    | { case: "success"; endedAtMs: bigint; commit: string }
    | { case: "failed"; endedAtMs: bigint; summary: string }
    | { case: "abandoned"; endedAtMs: bigint; summary: string },
  opts: { icon?: string; startedAtMs?: bigint; label?: string } = {},
): FeedMerge {
  const arm =
    result.case === "update"
      ? { case: "update" as const, value: {} }
      : result.case === "success"
        ? {
            case: "success" as const,
            value: { endedAtMs: result.endedAtMs, commit: result.commit },
          }
        : result.case === "failed"
          ? {
              case: "error" as const,
              value: {
                endedAtMs: result.endedAtMs,
                reason: { case: "failed" as const, value: { summary: result.summary } },
              },
            }
          : {
              case: "error" as const,
              value: {
                endedAtMs: result.endedAtMs,
                reason: { case: "abandoned" as const, value: { summary: result.summary } },
              },
            };
  return create(FeedMergeSchema, {
    head: {
      glyph: { icon: opts.icon ?? "merge" },
      label: { text: opts.label ?? "branch → master" },
      runtime: { startedAtMs: opts.startedAtMs ?? 0n },
      fold: { folded: true, decidedBy: { case: "daemon", value: {} } },
    },
    result: arm,
  } as never);
}

/** An ordinary row parented to a tab (an agentic tab's content). */
export function childRow(rowId: string, parent: string, text = "hi"): FeedRow {
  return create(FeedRowSchema, {
    id: id(rowId),
    order: orderFor(rowId),
    parent: { row: id(parent) },
    row: {
      case: "activity",
      value: {
        unit: {
          case: "response",
          value: { result: { case: "success", value: { prose: { markdown: text } } } },
        },
      },
    },
  });
}

/** A hand-driven `SubfeedView`: the seam the body renderer draws from. */
export class FakeSubfeed implements SubfeedView {
  private current: FeedRow[];
  private crumbs: FeedBreadcrumb[] = [];
  private readonly listeners = new Set<() => void>();
  /** Every row this view was asked to draw, in order. */
  readonly drawn: string[] = [];
  composerSlot?: HTMLElement;

  constructor(rows: readonly FeedRow[] = []) {
    this.current = [...rows];
  }

  rows(): readonly FeedRow[] {
    return this.current;
  }

  onChange(fn: () => void): () => void {
    this.listeners.add(fn);
    return () => this.listeners.delete(fn);
  }

  /** The ordinary row path, stubbed: one marked element per row. */
  drawRow(row: FeedRow): HTMLElement {
    const value = row.id?.value ?? "unset";
    this.drawn.push(value);
    const el = document.createElement("div");
    el.setAttribute("data-feed-row", value);
    return el;
  }

  breadcrumbs(): readonly FeedBreadcrumb[] {
    return this.crumbs;
  }

  /** Replace the rows and announce, exactly as a push does. */
  push(rows: readonly FeedRow[]): void {
    this.current = [...rows];
    for (const fn of [...this.listeners]) fn();
  }

  setBreadcrumbs(crumbs: readonly FeedBreadcrumb[]): void {
    this.crumbs = [...crumbs];
    for (const fn of [...this.listeners]) fn();
  }
}
