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
    | "merge"
    | "conflicts"
    | "tests"
    | "fixes"
    | "postPrompt";
  state: "live" | "parked" | "settled";
  /** For `settled`. */
  outcome?: "succeeded" | "failed";
  /** For a settled failure. */
  summary?: string;
  /** For `parked`. */
  line?: string;
  label?: string;
  round?: number;
  /** Extra kind-level fields (`queue`, `lines`, `suites`). */
  payload?: Record<string, unknown>;
}

/** The smallest legal queue snapshot: this workspace, waiting, alone. */
const DEFAULT_QUEUE = {
  ahead: [],
  current: {
    workspace: { ref: { id: "mine", dir: "/w/mine" } },
    label: { text: "mine" },
    status: { case: "waiting" as const, value: {} },
  },
  behind: [],
};

/** A merge-tab row, built from the generated schemas. */
export function tabRow(rowId: string, spec: TabSpec): FeedRow {
  const state =
    spec.state === "live"
      ? { case: "live" as const, value: {} }
      : spec.state === "parked"
        ? { case: "parked" as const, value: { line: { text: spec.line ?? "parked" } } }
        : {
            case: "settled" as const,
            value: {
              endedAtMs: 5_000n,
              outcome:
                spec.outcome === "failed"
                  ? { case: "failed" as const, value: { summary: spec.summary ?? "it failed" } }
                  : { case: "succeeded" as const, value: {} },
            },
          };
  // A queue tab MUST carry its snapshot; a fixture that omits one would be a
  // malformed view rather than a queue tab, so the default supplies the
  // smallest legal one.
  const payload =
    spec.kind === "queue" && spec.payload?.["queue"] === undefined
      ? { ...(spec.payload ?? {}), queue: DEFAULT_QUEUE }
      : (spec.payload ?? {});
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
      fold: { folded: true },
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
