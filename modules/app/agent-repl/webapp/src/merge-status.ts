/**
 * merge-status — the ONE reading of `WorkspaceState.merge_status` every surface
 * that renders a merge draws from.
 *
 * THREE surfaces show a merge (the progress footer's phase cell and chips, the
 * sidebar's current row, and the composer's merge gate) and they must not each
 * derive their own account of it. The failure mode is specific and has happened
 * before with the flat fields: the footer said "merging", the rail's dot said
 * queued, and the composer's notice said neither — three readings of one fact,
 * disagreeing because each was computed where it was shown.
 *
 * So this file computes the facts ONCE, as plain text and a severity, and the
 * surfaces choose which of them they have room for. It renders no markup and
 * escapes nothing: every consumer escapes at its own boundary, exactly as it
 * already does for every other string it shows.
 *
 * WHICH ARM IS SET IS THE PHASE (see `MergeStatus`), so there is no phase word
 * to keep in sync with the payload and no field that is meaningless for the
 * phase in flight. The switch below is exhaustive over the oneof, which makes a
 * new daemon-side phase a compile error here rather than a merge that silently
 * renders as nothing.
 *
 * The FLAT fields (`mergePhase`, `mergeQueuePosition`, `mergeQueueDepth`) are
 * untouched by this file and still render through their own paths. Retiring
 * them is a later wave; until then a workspace can carry both, and the flat
 * paths stay the fallback for a daemon that has not yet stamped a status.
 */
import type { MergeStatus } from "./frontend-proto.js";

/**
 * How loud a merge phase is, in the vocabulary the footer's phase cell and the
 * sidebar's dot already speak.
 *
 * `muted` is the whole in-flight band: a merge is chrome that spends no color,
 * exactly as `phaseLabel` decided for the flat phases. Only a phase that has
 * STOPPED the run spends `error`, and only a settled one spends `ok`.
 */
export type MergeTone = "muted" | "error" | "ok";

/** One merge run's status, as facts a surface can render. */
export interface MergeFacts {
  /** The phase word, in the footer's lower-case phase-cell voice. */
  word: string;
  tone: MergeTone;
  /**
   * Whether the phase is WORK IN FLIGHT, which drives the footer's breath and
   * the rail's spinning recycle glyph.
   *
   * A queued merge does not breathe: it is waiting on another workspace, not
   * doing anything. Neither do the three settled phases.
   */
  breathing: boolean;
  /**
   * The run's arithmetic as a chip: `landed/total` while commits are moving,
   * `position/depth` while enqueued, the commit count once merged, and "" for
   * a phase that counts nothing.
   */
  count: string;
  /** What `count` means, spelled out for the chip's hover line. */
  countTitle: string;
  /**
   * The one line naming what the run is DOING right now — the commit in hand,
   * or the pre/post-merge prompt the daemon is running — or "" for a phase
   * whose word already says everything.
   */
  activity: string;
  /**
   * A standing note the phase word cannot carry: a failure's classified cause,
   * or a landed merge whose after-action did not succeed. "" when there is
   * nothing to add.
   */
  note: string;
}

/** How much of a sha a human reads. */
const SHA_DISPLAY_LENGTH = 7;

/** A commit sha as it is shown: the leading bytes, or "" when there is none. */
export function shortSha(sha: string): string {
  return sha.slice(0, SHA_DISPLAY_LENGTH);
}

/** `<sha> <subject>`, dropping whichever half the daemon did not supply. */
function commitLabel(sha: string, subject: string): string {
  return [shortSha(sha), subject].filter((part) => part !== "").join(" ");
}

/** `landed/total`, or "" when the run has no plan to count against yet. */
function commitCount(landed: number, total: number): string {
  return total > 0 ? `${String(landed)}/${String(total)}` : "";
}

/** `N commits`, singular-correct. */
function commitPlural(n: number): string {
  return `${String(n)} commit${n === 1 ? "" : "s"}`;
}

/**
 * The renderable facts of a merge run, or null when no merge touches this
 * workspace.
 *
 * Null is the ordinary case — most workspaces are not merging — and every
 * caller tests it as one null check rather than by interrogating a phase.
 */
export function mergeFacts(status: MergeStatus | null): MergeFacts | null {
  if (status === null) return null;
  const phase = status.phase;
  switch (phase.case) {
    // WAITING on other workspaces contending for the same repository. It does
    // not breathe: nothing about THIS workspace is happening.
    case "enqueued": {
      const { position, depth } = phase.value;
      return {
        word: "merge queued",
        tone: "muted",
        breathing: false,
        count: depth > 0 ? `${String(position)}/${String(depth)}` : "",
        countTitle:
          depth > 0
            ? `this workspace is ${String(position)} of ${String(depth)} waiting to merge into this repository`
            : "",
        activity: "",
        note: "",
      };
    }
    // The daemon running the workspace's PRE-merge prompt. The prompt is the
    // activity: it is the only account of what the session is being made to do
    // under a lease the user cannot prompt through.
    case "beforeAction":
      return {
        word: "merge before-action",
        tone: "muted",
        breathing: true,
        count: "",
        countTitle: "",
        activity: promptActivity("before-action", phase.value.prompt),
        note: "",
      };
    case "cherryPicking": {
      const { commitsTotal, commitsLanded, currentSha, currentSubject } = phase.value;
      const commit = commitLabel(currentSha, currentSubject);
      return {
        word: "merging",
        tone: "muted",
        breathing: true,
        count: commitCount(commitsLanded, commitsTotal),
        countTitle: `${String(commitsLanded)} of ${commitPlural(commitsTotal)} landed on the merge target`,
        activity: commit === "" ? "" : `picking · ${commit}`,
        note: "",
      };
    }
    case "testing": {
      const { commitsTotal, commitsLanded, currentSha, currentSubject } = phase.value;
      const commit = commitLabel(currentSha, currentSubject);
      return {
        word: "merge testing",
        tone: "muted",
        breathing: true,
        count: commitCount(commitsLanded, commitsTotal),
        countTitle: `${String(commitsLanded)} of ${commitPlural(commitsTotal)} landed, under test before the merge settles`,
        activity: commit === "" ? "" : `testing · ${commit}`,
        note: "",
      };
    }
    // PARKED ON A HUMAN. The conflicted commit's subject is the fact worth the
    // activity slot: "merge conflict" alone never said WHICH commit to go and
    // look at.
    case "conflict": {
      const { conflictedSha, conflictedSubject, commitsTotal, commitsLanded } = phase.value;
      const commit = commitLabel(conflictedSha, conflictedSubject);
      return {
        word: "merge conflict",
        tone: "error",
        breathing: false,
        count: commitCount(commitsLanded, commitsTotal),
        countTitle: `${String(commitsLanded)} of ${commitPlural(commitsTotal)} landed before the conflict`,
        activity: commit === "" ? "" : `conflict · ${commit}`,
        note: "",
      };
    }
    case "afterAction":
      return {
        word: "merge after-action",
        tone: "muted",
        breathing: true,
        count: "",
        countTitle: "",
        activity: promptActivity("after-action", phase.value.prompt),
        note: "",
      };
    // SETTLED. A failed after-action is a NOTE on a landed merge, never a
    // failure of it: the commits are on the target either way, and painting
    // this red would tell the user to undo something that worked.
    case "merged": {
      const { commitsTotal, afterActionError } = phase.value;
      return {
        word: "merged",
        tone: "ok",
        breathing: false,
        count: commitsTotal > 0 ? String(commitsTotal) : "",
        countTitle: commitsTotal > 0 ? `${commitPlural(commitsTotal)} merged` : "",
        activity: "",
        note: afterActionError === "" ? "" : `after-action failed: ${afterActionError}`,
      };
    }
    case "failed": {
      const { cause, commitsTotal, commitsLanded, failingSha, failingSubject } = phase.value;
      const commit = commitLabel(failingSha, failingSubject);
      return {
        word: "merge failed",
        tone: "error",
        breathing: false,
        count: commitCount(commitsLanded, commitsTotal),
        countTitle: `${String(commitsLanded)} of ${commitPlural(commitsTotal)} landed before the run stopped`,
        activity: commit === "" ? "" : `failed on · ${commit}`,
        // The daemon's classified cause, verbatim. It is the only answer to
        // "why", and a run that stopped without one is a run the user cannot
        // act on — so the fallback names the absence rather than hiding it.
        note: cause === "" ? "the daemon reported no cause" : cause,
      };
    }
    default: {
      const never: never = phase;
      throw new Error(`merge-status: unhandled merge phase ${JSON.stringify(never)}`);
    }
  }
}

/**
 * A pre/post-merge prompt as the activity line.
 *
 * The prompt is shown, not summarized: it is what the daemon is making this
 * session do while the user cannot prompt it, and a paraphrase of that is worse
 * than nothing. A prompt the daemon did not supply degrades to the bare label
 * rather than a dangling separator.
 */
function promptActivity(label: string, prompt: string): string {
  return prompt === "" ? `${label} · running` : `${label} · ${prompt}`;
}

/**
 * A merge status as one log field: `<phase>/<runId>@<updatedAtMs>`, or `none`.
 *
 * Kept beside the facts so every surface that logs a merge logs it the same
 * way, and so a record can be correlated with the daemon's own run.
 */
export function mergeStatusLogValue(status: MergeStatus | null): string {
  if (status === null) return "none";
  return `${status.phase.case}/${status.runId}@${String(status.updatedAtMs)}`;
}
