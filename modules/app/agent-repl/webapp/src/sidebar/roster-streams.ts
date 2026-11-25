/**
 * roster-streams — THE CLIENT'S COPY OF THE ROSTER'S FOUR SNAPSHOT STREAMS
 * (frontend.v1.RosterPush; owner ruling, 2026-11-16).
 *
 * The roster is four streams on one push: the workspaces, the merge queue,
 * the Recently Merged summary and the closed rows. NEVER A DIFF: every stream
 * a push carries is that stream's whole snapshot and REPLACES the copy held
 * here, and a stream a push leaves out is UNCHANGED, never empty. So the
 * client cannot drift from the daemon: what is held is always, stream by
 * stream, the last whole snapshot the daemon sent.
 *
 * THE FIRST PUSH OF A SUBSCRIPTION CARRIES EVERY STREAM. One that does not
 * breaks the contract and is refused whole (a `MalformedView`, which the
 * stream pipeline logs and files as an unreadable frame), and nothing of it
 * is kept: a later push still finds no baseline and is refused in turn until
 * the stream is opened again. A run that ends forgets its copy (`reset`), so
 * the next run's first push is its baseline.
 *
 * EVERY SNAPSHOT A PUSH CARRIES IS APPLIED BEFORE ANYTHING IS DRAWN: `apply`
 * answers one immutable `RosterStreams` holding every stream as of the push,
 * so a drawing never holds one side of a transition between streams without
 * the other (the daemon moves a workspace between streams in one push).
 *
 * THE REVISION IS THE PUSH'S (`RosterPush.revision`): holding every stream of
 * the push at R is holding the whole roster as of R, which is what a verb's
 * answered revision is compared against.
 */
import type {
  RosterClosedRows,
  RosterMergedSection,
  RosterMergeQueueSection,
  RosterPush,
  WorkspaceRoster,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { WatchWorkspaceRosterResponse } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import { createLogger } from "../log.js";
import { MalformedView } from "../rpc/malformed.js";
import { requireCase, requireMessage, unreachablePushArm } from "../rpc/strict.js";

const log = createLogger("webapp.sidebar.roster-streams");

/** Every stream of the roster as of one push: what the client holds and draws. */
export interface RosterStreams {
  /** The push's revision: every stream here is the daemon's as of it. */
  readonly revision: bigint;
  /** The WORKSPACES stream: open and backlogged workspaces, threads, selection, the rail's chrome. */
  readonly workspaces: WorkspaceRoster;
  /** The MERGE QUEUE stream: every repository's active queue. */
  readonly mergeQueue: RosterMergeQueueSection;
  /** The RECENTLY MERGED stream: the band's summary, never a merged row. */
  readonly recentlyMerged: RosterMergedSection;
  /** The CLOSED stream: the closed, never-merged rows, drawn by no rail section. */
  readonly closed: RosterClosedRows;
}

/** The streams' names, as the log and a refusal spell them. */
export type RosterStreamName = "workspaces" | "merge_queue" | "recently_merged" | "closed";

/** The streams in the wire's order. */
export const ROSTER_STREAMS: readonly RosterStreamName[] = ["workspaces", "merge_queue", "recently_merged", "closed"];

/** One subscription's copy of the streams. */
export interface RosterIntake {
  /**
   * Apply every snapshot PUSH carries over the copy held, and answer every
   * stream as of PUSH. A first push missing a stream is refused (`MalformedView`)
   * and changes nothing.
   */
  apply(push: RosterPush): RosterStreams;
  /** The run ended: the next push is a baseline again. */
  reset(): void;
  /** The streams as of the last push applied, or null before the run's first. */
  latest(): RosterStreams | null;
}

/** The streams PUSH carries, by name. */
export function carriedStreams(push: RosterPush): RosterStreamName[] {
  const carried: RosterStreamName[] = [];
  if (push.workspaces !== undefined) carried.push("workspaces");
  if (push.mergeQueue !== undefined) carried.push("merge_queue");
  if (push.recentlyMerged !== undefined) carried.push("recently_merged");
  if (push.closed !== undefined) carried.push("closed");
  return carried;
}

/**
 * The streams of A that are not the very snapshots B holds: the ones a push
 * replaced between the two. Snapshots are replaced whole, never patched, so
 * an unchanged stream is the same object.
 */
export function changedStreams(a: RosterStreams | null, b: RosterStreams): RosterStreamName[] {
  if (a === null) return [...ROSTER_STREAMS];
  const changed: RosterStreamName[] = [];
  if (a.workspaces !== b.workspaces) changed.push("workspaces");
  if (a.mergeQueue !== b.mergeQueue) changed.push("merge_queue");
  if (a.recentlyMerged !== b.recentlyMerged) changed.push("recently_merged");
  if (a.closed !== b.closed) changed.push("closed");
  return changed;
}

/** The path a push is refused at. */
const PUSH_PATH = "WatchWorkspaceRosterResponse.streams";

/** A fresh copy of the roster's streams, for one subscription. */
export function createRosterIntake(): RosterIntake {
  let held: RosterStreams | null = null;
  return {
    apply(push: RosterPush): RosterStreams {
      if (held === null) {
        const carried = carriedStreams(push);
        const missing = ROSTER_STREAMS.filter((name) => !carried.includes(name));
        if (missing.length > 0) {
          // REFUSED WHOLE: the stream pipeline logs and files the refusal
          // (`rpc.stream-frame-undecodable`), with this path and detail.
          throw new MalformedView(
            PUSH_PATH,
            `the first push of a subscription carries every stream; revision ${push.revision} lacks ${missing.join(", ")}`,
          );
        }
      }
      const next: RosterStreams = {
        revision: push.revision,
        workspaces: push.workspaces ?? requireHeld(held).workspaces,
        mergeQueue: push.mergeQueue ?? requireHeld(held).mergeQueue,
        recentlyMerged: push.recentlyMerged ?? requireHeld(held).recentlyMerged,
        closed: push.closed ?? requireHeld(held).closed,
      };
      log.debug("applied a roster push's snapshots", {
        operation: "sidebar.roster-push",
        context: { revision: String(push.revision), carried: carriedStreams(push), baseline: held === null },
      });
      held = next;
      return next;
    },
    reset(): void {
      held = null;
    },
    latest: () => held,
  };
}

/** The copy held, which a push after the first always has. */
function requireHeld(held: RosterStreams | null): RosterStreams {
  if (held === null) throw new Error("invariant: a push leaving a stream out follows a first push that carried it");
  return held;
}

/**
 * The roster push a stream RESPONSE carries. A top-level arm this build has
 * no case for is forward-compat skew (`unreachablePushArm`); `ending` is the
 * stream pipeline's to consume and never reaches here.
 */
export function rosterPushOf(response: WatchWorkspaceRosterResponse): RosterPush {
  const push = requireCase(response.push, "WatchWorkspaceRosterResponse.push");
  if (push.case !== "streams") return unreachablePushArm("WatchWorkspaceRosterResponse.push", push.case);
  return requireMessage(push.value, PUSH_PATH);
}
