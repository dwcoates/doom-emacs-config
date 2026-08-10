/**
 * The held-prompt accumulator: what a prompt typed while the backend is down
 * becomes, instead of an immediate refusal.
 *
 * WHY IT EXISTS. A full backend bounce — store, sidecar, daemon, shim roll — is
 * supposed to be imperceptible. It was not: a prompt submitted across the
 * outage window hit a socket that could not carry it and came back as a failure
 * card, so the one thing a user does with this page was the one thing the
 * bounce broke. The words were then GONE, because the composer had already
 * cleared its draft.
 *
 * WHAT IT IS NOT. It is not an ack. A queued prompt is drawn as PENDING and
 * nothing here ever tells the store a prompt was accepted — the daemon is the
 * only thing that may say that, and it says it by acking the real
 * `SubmitPromptCmd` this queue eventually sends.
 *
 * THE DRAIN GATE IS REVIVAL, NOT RECONNECT. A reconnected socket proves the
 * daemon is up; it proves nothing about whether THIS workspace has a live
 * session behind it. Firing a held prompt into a durable-replay-only workspace
 * — one the fenced-honesty work stamps `severed` or `hibernated` on the wired
 * axis — would submit into a conversation with no controller reading it. So the
 * queue drains only once `revived` reports a live session view for the
 * workspace, and holds otherwise.
 *
 * THE HOLD IS BOUNDED. A workspace that never revives must not swallow a
 * prompt forever and must not pretend one is still coming: past
 * `revivalBoundMs` every held prompt is failed INDIVIDUALLY, with the reason.
 * Nothing queued here is ever dropped silently — every entry leaves this queue
 * either as a real submitted command or as its own failure.
 */

import type { PromptOrigin } from "./frontend-command.js";
import type { WebRenderState } from "./state-adapter.js";

/**
 * The render states that mean NOTHING IS ATTACHED to the workspace.
 *
 * Taken from the wired axis's own law (`ssm/wired.go`): `severed` and
 * `hibernated` are the two closed halves — no live backend session, one
 * claiming breakage and one claiming a deliberate teardown — and every other
 * color is a guarantee that the substrate is wired. `dead` is added because a
 * terminal session is not coming back to read anything either.
 */
const UNWIRED_RENDER_STATES: ReadonlySet<WebRenderState> = new Set<WebRenderState>([
  "severed",
  "hibernated",
  "dead",
]);

/**
 * Whether a held prompt may be submitted into a workspace showing STATE.
 *
 * A null state is NOT drainable: it means no revisioned `WorkspaceState` has
 * been applied at all, so there is no evidence either way, and firing a prompt
 * on no evidence is exactly the durable-replay-only case this gate exists for.
 */
export function drainableRenderState(state: WebRenderState | null): boolean {
  return state !== null && !UNWIRED_RENDER_STATES.has(state);
}

/** One prompt the user submitted while the backend could not carry it. */
export interface QueuedPrompt {
  /**
   * This entry's local identity: the key its pending bubble is filed under and
   * the discriminator its failure card is keyed on, so N held prompts render as
   * N pending bubbles and (on expiry) N distinct failures rather than one card
   * standing in for all of them.
   *
   * Deliberately NOT a request id: no command has been sent, and a value that
   * looked like one could be reconciled against by a daemon receipt.
   */
  readonly queueId: string;
  readonly workspace: string;
  readonly text: string;
  readonly promptOrigin: PromptOrigin;
  /** When the prompt was taken in — the clock the revival bound runs off. */
  readonly queuedAtMs: number;
}

export interface PromptQueueDeps {
  /**
   * Whether the transport cannot carry a command right now. True during a
   * bounce, which is what makes a submitted prompt queue rather than fail.
   */
  linkDown: () => boolean;
  /**
   * Whether WORKSPACE has a LIVE session view — the wired half of the axis. The
   * drain gate; see the module comment on why reconnect is not enough.
   */
  revived: (workspace: string) => boolean;
  /** Draw ENTRY in the feed as visibly pending. Never an ack. */
  echo: (entry: QueuedPrompt) => void;
  /**
   * Take ENTRY's pending bubble back down. Run when the entry leaves the queue
   * for either reason: the real submit files its own bubble under the real
   * request id, and an expired entry is replaced by its failure card.
   */
  retract: (entry: QueuedPrompt) => void;
  /** Send ENTRY as a real `SubmitPromptCmd`; rejects exactly as the ack does. */
  submit: (entry: QueuedPrompt) => Promise<void>;
  /** Surface ENTRY's own honest failure. One call per lost prompt. */
  fail: (entry: QueuedPrompt, reason: string) => void;
  now: () => number;
  /**
   * Arm the revival deadline. Injected so a test fires the bound explicitly
   * rather than waiting a minute of wall clock for it.
   */
  schedule?: (fn: () => void, ms: number) => void;
  /** How long a held prompt waits for its workspace to come back. */
  revivalBoundMs?: number;
}

/** The bounce window the daemon's restart announcement promises to fit in. */
export const DEFAULT_REVIVAL_BOUND_MS = 60_000;

function causeText(err: unknown): string {
  return err instanceof Error ? err.message : String(err);
}

export class PromptQueue {
  /** Per-workspace FIFO. Order within a workspace is the user's typing order. */
  private readonly queues = new Map<string, QueuedPrompt[]>();
  /** Workspaces with a drain in flight, so two link-up edges cannot interleave. */
  private readonly draining = new Set<string>();
  private seq = 0;

  constructor(private readonly deps: PromptQueueDeps) {}

  private get boundMs(): number {
    return this.deps.revivalBoundMs ?? DEFAULT_REVIVAL_BOUND_MS;
  }

  /**
   * Offer one submitted prompt to the queue, reporting whether the queue TOOK
   * it. False means the link is up and the caller must submit normally — this
   * queue never stands between a working socket and a prompt.
   *
   * A prompt is also taken while a drain is still running for the workspace,
   * even though the link is by then up: appending behind the entries still
   * being sent is what keeps the user's order, where submitting directly would
   * overtake them.
   */
  offer(workspace: string, text: string, promptOrigin: PromptOrigin): boolean {
    const held = this.queues.get(workspace);
    const busy = this.draining.has(workspace) || (held !== undefined && held.length > 0);
    if (!this.deps.linkDown() && !busy) return false;
    const entry: QueuedPrompt = {
      queueId: `held:${++this.seq}`,
      workspace,
      text,
      promptOrigin,
      queuedAtMs: this.deps.now(),
    };
    const queue = held ?? [];
    if (held === undefined) this.queues.set(workspace, queue);
    queue.push(entry);
    this.deps.echo(entry);
    // The deadline is armed per entry, not per queue: each held prompt owes the
    // user an answer within the bound counted from ITS OWN submission, and a
    // single queue-wide timer armed by the first entry would leave a later one
    // waiting past its bound with nothing scheduled to speak for it.
    const schedule = this.deps.schedule ?? ((fn: () => void, ms: number): void => {
      setTimeout(fn, ms);
    });
    schedule(() => {
      void this.drain(workspace);
    }, this.boundMs);
    return true;
  }

  /** WORKSPACE's held prompts, oldest first. */
  pending(workspace: string): readonly QueuedPrompt[] {
    return this.queues.get(workspace) ?? [];
  }

  /**
   * Send WORKSPACE's held prompts, in order, if its session is back.
   *
   * Wire this to whatever proves the backend is current again (the socket's
   * snapshot-adopted edge) and to nothing else — it is safe to call when there
   * is nothing held, when the workspace has not revived, and while a drain is
   * already running.
   *
   * A LINK THAT DROPS MID-DRAIN STOPS THE DRAIN. The entries not yet sent stay
   * queued in order for the next revival, rather than being fired into a socket
   * that just went away and failed one by one.
   */
  async drain(workspace: string): Promise<void> {
    const queue = this.queues.get(workspace);
    if (queue === undefined || queue.length === 0) return;
    if (this.draining.has(workspace)) return;
    if (!this.deps.revived(workspace)) {
      this.expireOverdue(workspace, queue);
      return;
    }
    this.draining.add(workspace);
    try {
      while (queue.length > 0 && this.deps.revived(workspace)) {
        const entry = queue[0];
        if (entry === undefined) break;
        queue.shift();
        // The pending bubble comes down FIRST: the submit below files its own
        // bubble under the real request id, and leaving this one standing would
        // show the same prompt twice.
        this.deps.retract(entry);
        try {
          await this.deps.submit(entry);
        } catch (err) {
          // A refused held prompt is a lost prompt, and the user watched it sit
          // there pending. It gets its own account rather than the silence that
          // an unhandled rejection would have been.
          this.deps.fail(entry, causeText(err));
        }
      }
    } finally {
      this.draining.delete(workspace);
      if (queue.length === 0) this.queues.delete(workspace);
    }
  }

  /**
   * Fail every held prompt whose bound has passed, leaving any younger ones
   * queued for a revival that may still arrive.
   */
  private expireOverdue(workspace: string, queue: QueuedPrompt[]): void {
    const deadlineAt = this.deps.now() - this.boundMs;
    while (queue.length > 0) {
      const entry = queue[0];
      if (entry === undefined || entry.queuedAtMs > deadlineAt) break;
      queue.shift();
      this.deps.retract(entry);
      this.deps.fail(
        entry,
        `the session did not come back within ${Math.round(this.boundMs / 1000)}s; ` +
          "the prompt was never sent",
      );
    }
    if (queue.length === 0) this.queues.delete(workspace);
  }
}
