/**
 * engine/network-resume.ts — resuming a background subagent a network outage
 * killed (owner ruling, 2026-09-27).
 *
 * THE INCIDENT. A DNS outage made the vendor end a running background agent
 * ("Agent terminated early due to an API error: API Error: Can't reach the API
 * server — check your internet or DNS (ENOTFOUND)"). The shim folded the failed
 * `task_notification` into the agent's failure terminal and nothing more; the
 * agent stayed failed until a person noticed and resumed it by hand.
 *
 * THE RULE.
 *
 * - ONLY A NETWORK FAILURE IS RESUMED ({@link classifyAgentFailure}). The
 *   vendor's structured error class decides first; the connection code and the
 *   HTTP status next; the vendor's prose only when nothing structured settles
 *   it. Auth, billing, quota, overload, rate limits, invalid requests and model
 *   errors are never resumed.
 * - ONE PROBE LOOP PER PROCESS ({@link NetworkResume}). Every waiting agent
 *   shares one fixed-interval timer ({@link NETWORK_RESUME_PROBE_INTERVAL_MS},
 *   no backoff); the timer exists only while something waits, and
 *   {@link NetworkResume.stop} cancels it at stand-down.
 * - THE WAIT IS BOUNDED. {@link NETWORK_RESUME_WINDOW_MS} from the failure,
 *   after which the wait gives up at ERROR and the agent keeps its failure.
 * - ONE RESUME PER FAILURE EVENT, and the window restarts only when the resumed
 *   run made PROGRESS: at least one model-authored response of that agent after
 *   the resume was delivered. A resumed run that fails again without progress
 *   keeps its ORIGINAL window, so a loop of resume-then-fail ends when that
 *   window does — each new window needs the model to have answered, which it
 *   cannot while the API is unreachable.
 * - THE RESUME IS THE VENDOR'S OWN CONTINUATION. The pinned SDK declares no
 *   route that delivers a prompt to a named agent (engine/turn.ts,
 *   `promptAgent`), so the shim asks the MAIN agent to continue each agent with
 *   `SendMessage` — the same call a person made by hand in the incident, which
 *   the vendor answers by resuming the same agent from its own transcript.
 * - THE WAIT IS VISIBLE ON THE SESSION STREAM (footer-activity-tiers.md,
 *   landed change 2). Every change to the waiting set is stated WHOLE as
 *   `SessionUpdate.network_resume_waits`, and every wait that ends is stated
 *   once as `SessionUpdate.network_resume_outcome`, through the injected
 *   {@link NetworkResumeDeps.emit}. Visibility only: nothing here reads what
 *   it states back, so the rule above is unchanged by it.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { isAgentTaskType } from "../convert/detached.js";
import { detachedWorkId } from "../convert/ids.js";
import { conversationv1 } from "../proto.js";
import type { KeepaliveScheduler } from "./keepalive.js";
import { networkResumePrompt } from "./network-resume-prompt.js";
import type { SdkMessage } from "../sdk/types.js";

const LOGGER = bindLog({ component: "shim-engine-network-resume", operation: "shim.engine.network_resume" });

/** How often the one probe loop asks whether the API is reachable. FIXED: no backoff. */
export const NETWORK_RESUME_PROBE_INTERVAL_MS = 5_000;

/** How long after a failure the shim keeps waiting for the API before it gives up. */
export const NETWORK_RESUME_WINDOW_MS = 30 * 60 * 1_000;

// ---------------------------------------------------------------------------
// Classification
// ---------------------------------------------------------------------------

/**
 * The vendor's error classes (`SDKAssistantMessageError`) that are NOT a
 * network outage. Each is a refusal the API ANSWERED, so reaching the API again
 * changes nothing about it.
 */
const NON_NETWORK_CLASSES: ReadonlySet<string> = new Set([
  "authentication_failed",
  "oauth_org_not_allowed",
  "account_on_hold",
  "verification_required",
  "billing_error",
  "rate_limit",
  "overloaded",
  "invalid_request",
  "model_not_found",
  "max_output_tokens",
  "cloud_credential_error",
]);

/**
 * The socket errors that mean the API host could not be reached at all.
 *
 * DNS (`ENOTFOUND`, `EAI_AGAIN`), a refused or reset connection, a connect that
 * timed out, and a network or host the kernel has no route to.
 */
export const NETWORK_ERROR_CODES: readonly string[] = [
  "ENOTFOUND",
  "EAI_AGAIN",
  "ECONNREFUSED",
  "ECONNRESET",
  "ETIMEDOUT",
  "ENETUNREACH",
  "EHOSTUNREACH",
  "ENETDOWN",
];

const NETWORK_CODE_PATTERN = new RegExp(`\\b(${NETWORK_ERROR_CODES.join("|")})\\b`);

/** The vendor's own sentences for "the API could not be reached". */
const NETWORK_PROSE: readonly RegExp[] = [/can['’]t reach the api server/i, /\bconnection error\b/i];

/** The vendor's `(error type <class>)` suffix on a failed agent's summary. */
const ERROR_TYPE_PATTERN = /\(error type ([a-z_]+)\)/i;

/** Everything the shim learned about one failed agent run. */
export interface FailureEvidence {
  /** The vendor's structured error class (`SDKAssistantMessageError`), when one was stated. */
  readonly errorClass?: string;
  /** A structured socket error code, when the vendor stated one. */
  readonly connectionCode?: string;
  /** The HTTP status the API answered with, when it answered at all. */
  readonly httpStatus?: number;
  /** The vendor's own prose for the failure: the error notice, then the notification summary. */
  readonly text?: string;
}

/** Which piece of evidence decided a classification. */
export type ClassificationBasis = "error_class" | "connection_code" | "http_status" | "text";

/** The verdict on one failure, and what it rested on. */
export interface FailureClassification {
  readonly network: boolean;
  readonly basis: ClassificationBasis;
  readonly detail: string;
}

/**
 * Whether a failed agent run failed BECAUSE THE API WAS UNREACHABLE.
 *
 * STRUCTURE FIRST, PROSE LAST:
 *
 * 1. A NON-NETWORK error class is final: the API answered with a refusal.
 * 2. A structured connection code decides next: a network code resumes, any
 *    other (a TLS failure, say) does not.
 * 3. An HTTP status means the API ANSWERED, so it was reachable.
 * 4. Only then the prose, which is where the vendor puts the socket code for
 *    the classes it leaves ambiguous (`server_error`, `unknown`): an
 *    `(error type …)` suffix naming a non-network class is still final, and a
 *    network code or the vendor's "can't reach the API server" sentence
 *    resumes.
 */
export function classifyAgentFailure(evidence: FailureEvidence): FailureClassification {
  const { errorClass, connectionCode, httpStatus, text } = evidence;
  if (errorClass !== undefined && NON_NETWORK_CLASSES.has(errorClass)) {
    return { network: false, basis: "error_class", detail: `the vendor classed the failure ${errorClass}` };
  }
  if (connectionCode !== undefined) {
    const network = NETWORK_ERROR_CODES.includes(connectionCode);
    return {
      network,
      basis: "connection_code",
      detail: network
        ? `the connection failed with ${connectionCode}`
        : `the connection failed with ${connectionCode}, which is not a reachability failure`,
    };
  }
  if (httpStatus !== undefined) {
    return { network: false, basis: "http_status", detail: `the API answered with HTTP ${String(httpStatus)}` };
  }
  const prose = text ?? "";
  const statedClass = ERROR_TYPE_PATTERN.exec(prose)?.[1]?.toLowerCase();
  if (statedClass !== undefined && NON_NETWORK_CLASSES.has(statedClass)) {
    return { network: false, basis: "text", detail: `the vendor's summary names the class ${statedClass}` };
  }
  const code = NETWORK_CODE_PATTERN.exec(prose)?.[1];
  if (code !== undefined) {
    return { network: true, basis: "text", detail: `the vendor's words name ${code}` };
  }
  if (NETWORK_PROSE.some((pattern) => pattern.test(prose))) {
    return { network: true, basis: "text", detail: "the vendor's words say the API could not be reached" };
  }
  return { network: false, basis: "text", detail: "nothing the vendor stated names an unreachable API" };
}

// ---------------------------------------------------------------------------
// The waiting set and its one probe loop
// ---------------------------------------------------------------------------

/** What one reachability probe answered. */
export interface ProbeAnswer {
  readonly reachable: boolean;
  /** How it knows, for the log. */
  readonly detail: string;
}

/** A reachability probe. It never throws: a failure to reach is its answer. */
export type ReachabilityProbe = () => Promise<ProbeAnswer>;

/** What delivering one resume prompt came to. */
export type ResumeDelivery =
  | { readonly kind: "delivered" }
  /** The main agent is working; the resume waits for it to be idle. */
  | { readonly kind: "busy"; readonly detail: string }
  /** The session can never take the prompt (no query, a dead session). */
  | { readonly kind: "unavailable"; readonly detail: string };

/** Everything {@link NetworkResume} does not own. */
export interface NetworkResumeDeps {
  readonly probe: ReachabilityProbe;
  /** Submits the resume prompt as the main agent's next turn. */
  readonly deliver: (prompt: string) => Promise<ResumeDelivery>;
  readonly nowMs: () => number;
  readonly scheduler: KeepaliveScheduler;
  /**
   * States one session fact on the session's standing stream: the WHOLE
   * waiting set (`network_resume_waits`) each time it changes, and one
   * `network_resume_outcome` per wait that ends.
   */
  readonly emit: (update: conversationv1.SessionUpdate) => void;
  readonly intervalMs?: number;
  readonly windowMs?: number;
}

/** One agent run the shim saw start. */
interface KnownRun {
  /** The run's own call (the spawn, or the `SendMessage` that resumed it): its `DetachedWorkId` value. */
  readonly toolUseId: string;
  readonly taskId: string;
  readonly description: string;
}

/**
 * One agent's resume history, from its first network failure until it ends
 * some other way or the shim gives up on it.
 */
interface Chain {
  readonly taskId: string;
  description: string;
  /** When the current window opened: the failure, or the last one after progress. */
  windowStartMs: number;
  /** When the last resume was delivered, if one was. */
  resumedAtMs?: number;
  /** Whether the resumed run has answered with the model since that resume. */
  progressSinceResume: boolean;
  /** How many resumes this chain delivered. */
  resumes: number;
}

/** One agent waiting for the API. */
interface Waiting {
  /**
   * This wait's own identity: a per-process generation, minted when the wait
   * opens. Two waits of the same agent (one given up, a later one opened for a
   * new failure) are different waits, and every record names which.
   */
  readonly id: number;
  readonly chain: Chain;
  readonly failedAtMs: number;
  /** The failed run's `DetachedWorkId` value: the handle its failure terminal retired. */
  readonly work: string;
  /**
   * How this wait ended, once it has: set by {@link NetworkResume.endWait}, the
   * ONE place a wait leaves the standing set on the wire, so a second ending
   * (a delivery that completes after `expire()` already gave the wait up) is
   * seen and never stated. `superseded` is a wait a newer failure of the same
   * agent replaced while it stood (see {@link NetworkResume.enqueue}); the
   * session stream has no arm for it, so it is recorded and never stated.
   */
  ended?: WaitEnd["case"] | "superseded";
}

/** How one wait ended, as the session stream states it. */
type WaitEnd = conversationv1.SessionNetworkResumeOutcome["outcome"];

/**
 * The shim's one network-resume state: which agent runs it saw, what each
 * failed with, which wait for the API, and the one probe loop they share.
 *
 * FED EVERY SDK MESSAGE ({@link observe}); it reads four shapes and ignores the
 * rest. It holds only per-agent state that ends with the agent, so it is bounded
 * by the agents this session ran.
 */
export class NetworkResume {
  private readonly intervalMs: number;
  private readonly windowMs: number;
  /** A run's `tool_use_id` (spawn or `SendMessage`) → the agent it runs. */
  private readonly runs = new Map<string, KnownRun>();
  /** The last error each agent's own messages stated. */
  private readonly evidence = new Map<string, FailureEvidence>();
  private readonly chains = new Map<string, Chain>();
  /**
   * The standing waits, at most one per agent, keyed by the agent's task id.
   * A wait leaves it only through {@link removeWait}, which removes THAT wait
   * and never whichever wait the agent holds now.
   */
  private readonly waiting = new Map<string, Waiting>();
  private nextWaitId = 1;
  private handle: unknown;
  private probing = false;
  private stopped = false;

  constructor(private readonly deps: NetworkResumeDeps) {
    this.intervalMs = deps.intervalMs ?? NETWORK_RESUME_PROBE_INTERVAL_MS;
    this.windowMs = deps.windowMs ?? NETWORK_RESUME_WINDOW_MS;
  }

  /** The agents waiting for the API now, in arrival order. */
  waitingTaskIds(): string[] {
    return [...this.waiting.keys()];
  }

  /** Whether the one probe loop is scheduled. */
  looping(): boolean {
    return this.handle !== undefined;
  }

  /** Read one SDK message for what it says about an agent run. */
  observe(message: SdkMessage): void {
    if (this.stopped) return;
    if (message.type === "assistant") {
      this.noteAssistant(message);
      return;
    }
    if (message.type !== "system") return;
    switch (message.subtype) {
      case "task_started":
        this.noteStarted(message);
        return;
      case "task_notification":
        this.noteNotification(message);
        return;
      default:
        return;
    }
  }

  /**
   * Stand down: cancel the loop and abandon every wait.
   *
   * Each abandoned agent keeps the failure its terminal already states; the
   * record names it, so a stand-down during an outage is never silent.
   */
  stop(reason: string): void {
    if (this.stopped) return;
    this.stopped = true;
    this.cancelLoop();
    const abandoned = this.waiting.size;
    for (const entry of this.waiting.values()) {
      LOGGER.info(
        {
          task_id: entry.chain.taskId,
          description: entry.chain.description,
          waited_ms: this.deps.nowMs() - entry.failedAtMs,
          reason,
          outcome: "abandoned",
        },
        "the shim is standing down; the agent waiting to be resumed after a network outage keeps its failure",
      );
      this.endWait(entry, {
        case: "abandoned",
        value: create(conversationv1.SessionNetworkResumeAbandonedSchema, { reason }),
      });
    }
    this.waiting.clear();
    if (abandoned > 0) this.stateWaits();
    this.chains.clear();
    this.runs.clear();
    this.evidence.clear();
  }

  // -- what the stream says ---------------------------------------------------

  private noteStarted(message: Extract<SdkMessage, { subtype: "task_started" }>): void {
    if (!isAgentTaskType(message.task_type)) return;
    if (message.tool_use_id === undefined || message.tool_use_id === "") return;
    this.runs.set(message.tool_use_id, {
      toolUseId: message.tool_use_id,
      taskId: message.task_id,
      description: message.description,
    });
    // A NEW RUN STARTS WITH NO ERROR. What an earlier run of the same agent said
    // is not what this one will fail with.
    this.evidence.delete(message.task_id);
    const chain = this.chains.get(message.task_id);
    if (chain !== undefined) {
      LOGGER.debug(
        { task_id: message.task_id, tool_use_id: message.tool_use_id, resumes: chain.resumes },
        "the resumed agent's run started",
      );
    }
  }

  private noteAssistant(message: Extract<SdkMessage, { type: "assistant" }>): void {
    const parent = message.parent_tool_use_id;
    if (parent === null) return;
    const run = this.runs.get(parent);
    if (run === undefined) return;
    if (typeof message.error === "string") {
      const text = noticeText(message.message);
      this.evidence.set(run.taskId, { errorClass: message.error, ...(text === undefined ? {} : { text }) });
      return;
    }
    // PROGRESS IS THE MODEL ANSWERING. A synthetic notice is the vendor's own
    // prose, never a response the API produced.
    if (message.message.model === "<synthetic>") return;
    const chain = this.chains.get(run.taskId);
    if (chain === undefined || chain.resumedAtMs === undefined || chain.progressSinceResume) return;
    chain.progressSinceResume = true;
    LOGGER.debug({ task_id: run.taskId, resumes: chain.resumes }, "the resumed agent answered with the model; it made progress");
  }

  private noteNotification(message: Extract<SdkMessage, { subtype: "task_notification" }>): void {
    const taskId = message.task_id;
    const run = [...this.runs.values()].find((known) => known.taskId === taskId);
    if (message.status !== "failed") {
      if (this.chains.delete(taskId)) {
        LOGGER.info(
          { task_id: taskId, status: message.status },
          "an agent resumed after a network outage ended without failing again",
        );
      }
      this.forgetAgent(taskId);
      return;
    }
    if (run === undefined) {
      // NOT AN AGENT THIS PROCESS SAW START: a shell, or work a predecessor
      // started. Nothing here can address it.
      LOGGER.debug({ task_id: taskId }, "a failed task this process never saw start as an agent; it is not considered for a resume");
      return;
    }
    const stated = this.evidence.get(taskId) ?? {};
    const text = [stated.text, message.summary].filter((part) => part !== undefined && part !== "").join("\n");
    const verdict = classifyAgentFailure({ ...stated, ...(text === "" ? {} : { text }) });
    if (!verdict.network) {
      LOGGER.info(
        { task_id: taskId, basis: verdict.basis, detail: verdict.detail, vendor_error: stated.errorClass, outcome: "not_resumed" },
        "a background agent failed, and not because the API was unreachable; it is not resumed",
      );
      this.chains.delete(taskId);
      this.forgetAgent(taskId);
      return;
    }
    this.enqueue(run, this.failedRun(run, message.tool_use_id), verdict);
  }

  /**
   * The run a failure notification ended: the one its `tool_use_id` names,
   * else the agent's LATEST run — the handle the fold's failure terminal
   * retires (`convert/detached.ts`, the remembered `tool_use_id`), which after
   * a resume is the resuming `SendMessage`'s, not the spawn's.
   */
  private failedRun(first: KnownRun, statedToolUseId: string | undefined): KnownRun {
    const stated = statedToolUseId === undefined ? undefined : this.runs.get(statedToolUseId);
    if (stated?.taskId === first.taskId) return stated;
    let latest = first;
    for (const run of this.runs.values()) {
      if (run.taskId === first.taskId) latest = run;
    }
    return latest;
  }

  // -- the wait ---------------------------------------------------------------

  private enqueue(run: KnownRun, failed: KnownRun, verdict: FailureClassification): void {
    const now = this.deps.nowMs();
    const existing = this.chains.get(run.taskId);
    let chain: Chain;
    if (existing === undefined) {
      chain = { taskId: run.taskId, description: run.description, windowStartMs: now, progressSinceResume: false, resumes: 0 };
      this.chains.set(run.taskId, chain);
    } else {
      chain = existing;
      chain.description = run.description;
      if (chain.progressSinceResume) {
        chain.windowStartMs = now;
        LOGGER.info(
          { task_id: run.taskId, resumes: chain.resumes },
          "the resumed agent made progress before failing again; its wait for the API starts a fresh window",
        );
      } else {
        LOGGER.info(
          { task_id: run.taskId, resumes: chain.resumes, window_started_at_ms: chain.windowStartMs },
          "the resumed agent failed again without progress; its wait keeps the window its first failure opened",
        );
      }
      chain.progressSinceResume = false;
    }
    const givesUpAtMs = chain.windowStartMs + this.windowMs;
    if (now >= givesUpAtMs) {
      // NO WAIT OPENED, so the session stream is told nothing: an outcome is
      // stated only for a wait that stood in the set.
      this.giveUp(chain, now, now, "its window had already run out when it failed again");
      return;
    }
    this.supersede(run.taskId);
    const wait: Waiting = { id: this.nextWaitId++, chain, failedAtMs: now, work: failed.toolUseId };
    this.waiting.set(run.taskId, wait);
    LOGGER.info(
      {
        task_id: run.taskId,
        wait_id: wait.id,
        description: run.description,
        basis: verdict.basis,
        detail: verdict.detail,
        gives_up_at_ms: givesUpAtMs,
        probe_interval_ms: this.intervalMs,
        waiting: this.waiting.size,
        outcome: "waiting",
      },
      "a background agent failed because the API was unreachable; it waits to be resumed once the API is reachable",
    );
    this.stateWaits();
    this.ensureLoop();
  }

  private ensureLoop(): void {
    if (this.handle !== undefined || this.stopped) return;
    this.handle = this.deps.scheduler.setInterval(() => {
      void this.tick();
    }, this.intervalMs);
    LOGGER.debug({ interval_ms: this.intervalMs }, "the network-resume probe loop started");
  }

  private cancelLoop(): void {
    if (this.handle === undefined) return;
    this.deps.scheduler.clearInterval(this.handle);
    this.handle = undefined;
    LOGGER.debug({}, "the network-resume probe loop stopped");
  }

  /** One beat of the loop. Exposed for the suite's scheduler; production reaches it by timer only. */
  async tick(): Promise<void> {
    if (this.stopped) return;
    this.expire();
    if (this.waiting.size === 0) {
      this.cancelLoop();
      return;
    }
    // ONE PROBE AT A TIME. A beat that lands while the previous probe is still
    // out skips rather than stacking a second one on a slow network.
    if (this.probing) {
      LOGGER.logVerbose({ waiting: this.waiting.size }, "a reachability probe is still out; this beat is skipped");
      return;
    }
    this.probing = true;
    let answer: ProbeAnswer;
    try {
      answer = await this.deps.probe();
    } catch (err) {
      // A PROBE IS NOT SUPPOSED TO THROW — an unreachable API is its answer, not
      // its failure — so a throw is a defect in the probe, recorded here and read
      // as "not reachable" for this beat; the next beat asks again.
      const cause = err instanceof Error ? err.message : String(err);
      LOGGER.error({ cause, waiting: this.waiting.size }, "the reachability probe threw instead of answering");
      answer = { reachable: false, detail: `the probe threw: ${cause}` };
    } finally {
      this.probing = false;
    }
    if (this.stopped) return;
    if (!answer.reachable) {
      LOGGER.logVerbose({ waiting: this.waiting.size, detail: answer.detail }, "the API is still unreachable");
      return;
    }
    await this.resumeDue(answer);
  }

  /** Give up on every wait whose window has run out. */
  private expire(): void {
    const now = this.deps.nowMs();
    let expired = 0;
    for (const entry of [...this.waiting.values()]) {
      if (now < entry.chain.windowStartMs + this.windowMs) continue;
      this.removeWait(entry);
      this.giveUp(entry.chain, entry.failedAtMs, now, "the API stayed unreachable for the whole window");
      this.endWait(entry, gaveUp());
      expired += 1;
    }
    if (expired > 0) this.stateWaits();
  }

  private giveUp(chain: Chain, failedAtMs: number, now: number, why: string): void {
    this.chains.delete(chain.taskId);
    this.forgetAgent(chain.taskId);
    LOGGER.error(
      {
        task_id: chain.taskId,
        description: chain.description,
        waited_ms: now - failedAtMs,
        window_ms: this.windowMs,
        window_started_at_ms: chain.windowStartMs,
        resumes: chain.resumes,
        detail: why,
        outcome: "gave_up",
      },
      "gave up resuming a background agent a network outage cut off; it keeps its failure",
    );
  }

  private async resumeDue(answer: ProbeAnswer): Promise<void> {
    const due = [...this.waiting.values()];
    const prompt = networkResumePrompt(due.map((entry) => ({ taskId: entry.chain.taskId, description: entry.chain.description })));
    let delivery: ResumeDelivery;
    try {
      delivery = await this.deps.deliver(prompt);
    } catch (err) {
      delivery = { kind: "unavailable", detail: err instanceof Error ? err.message : String(err) };
    }
    if (this.stopped) return;
    const now = this.deps.nowMs();
    switch (delivery.kind) {
      case "busy":
        LOGGER.logVerbose(
          { waiting: due.length, detail: delivery.detail },
          "the API is reachable but the main agent is working; the resume waits for it to be idle",
        );
        return;
      case "unavailable": {
        let ended = 0;
        for (const entry of due) {
          if (this.endedMeanwhile(entry, delivery.kind)) continue;
          this.removeWait(entry);
          this.giveUp(entry.chain, entry.failedAtMs, now, `the resume could not be delivered: ${delivery.detail}`);
          this.endWait(entry, gaveUp());
          ended += 1;
        }
        if (ended > 0) this.stateWaits();
        this.cancelLoopIfIdle();
        return;
      }
      case "delivered": {
        let ended = 0;
        for (const entry of due) {
          // A wait that ended while the delivery was out is answered alone: its
          // resume went out, so it is still logged as delivered, but it moves no
          // history and removes no wait, since the agent's standing wait (if one
          // opened meanwhile) belongs to a newer failure.
          const late = this.endedMeanwhile(entry, delivery.kind);
          if (!late) {
            this.removeWait(entry);
            entry.chain.resumes += 1;
            entry.chain.resumedAtMs = now;
            entry.chain.progressSinceResume = false;
            ended += 1;
          }
          LOGGER.info(
            {
              task_id: entry.chain.taskId,
              wait_id: entry.id,
              description: entry.chain.description,
              waited_ms: now - entry.failedAtMs,
              resumes: entry.chain.resumes,
              probe: answer.detail,
              late,
              outcome: "resumed",
            },
            "the API is reachable again; asked the main agent to continue the background agent a network outage cut off",
          );
          this.endWait(entry, resumed());
        }
        if (ended > 0) this.stateWaits();
        this.cancelLoopIfIdle();
        return;
      }
    }
  }

  // -- what the session stream is told ----------------------------------------

  /** State the WHOLE waiting set, in the order the waits opened. */
  private stateWaits(): void {
    const entries = [...this.waiting.values()];
    this.say(
      "networkResumeWaits",
      { waiting: entries.length, task_ids: entries.map((entry) => entry.chain.taskId) },
      () =>
        create(conversationv1.SessionUpdateSchema, {
          update: {
            case: "networkResumeWaits",
            value: create(conversationv1.SessionNetworkResumeWaitsSchema, {
              waits: entries.map((entry) =>
                create(conversationv1.SessionNetworkResumeWaitSchema, {
                  work: detachedWorkId(entry.work),
                  failedAtMs: BigInt(entry.failedAtMs),
                  givesUpAtMs: BigInt(entry.chain.windowStartMs + this.windowMs),
                  resumesDelivered: entry.chain.resumes,
                }),
              ),
            }),
          },
        }),
    );
  }

  /**
   * End one wait on the session stream: EXACTLY ONE OUTCOME PER WAIT.
   *
   * A delivery is awaited, and the probe loop's next beat may run `expire()`
   * meanwhile, so a wait can be given up while its resume is still out; the
   * delivery then completes for a wait that no longer stands. The rule itself
   * is unchanged by that (the late resume is still delivered and logged); only
   * the wire is guarded, here, on the same single-threaded state `expire()`
   * mutates, so the stream never states a second ending for one wait.
   */
  private endWait(entry: Waiting, end: WaitEnd): void {
    if (entry.ended !== undefined) {
      LOGGER.info(
        { task_id: entry.chain.taskId, wait_id: entry.id, work: entry.work, suppressed: end.case, already: entry.ended },
        "a network-resume wait that had already ended is not stated ended again on the session stream",
      );
      return;
    }
    entry.ended = end.case;
    const { work } = entry;
    this.say("networkResumeOutcome", { task_id: entry.chain.taskId, work, end: end.case }, () =>
      create(conversationv1.SessionUpdateSchema, {
        update: {
          case: "networkResumeOutcome",
          value: create(conversationv1.SessionNetworkResumeOutcomeSchema, { work: detachedWorkId(work), outcome: end }),
        },
      }),
    );
  }

  /**
   * Build one update and hand it to the session stream.
   *
   * A FAILURE HERE IS RECORDED AT ERROR AND GOES NO FURTHER: it is raised from
   * inside the vendor stream's observation or the probe loop's beat, and
   * neither may be broken by a consumer's view of the wait, which is what the
   * frame is. The wait itself carries on exactly as the rule says.
   */
  private say(arm: string, context: Record<string, unknown>, build: () => conversationv1.SessionUpdate): void {
    try {
      this.deps.emit(build());
    } catch (err) {
      LOGGER.error(
        { ...context, arm, cause: err instanceof Error ? err.message : String(err) },
        "could not state a network-resume wait on the session stream",
      );
    }
  }

  /**
   * Remove ONE wait from the standing set: this wait, by its own identity.
   *
   * THE AGENT'S TASK ID IS NOT THE WAIT. A wait already gone (given up while
   * its delivery was out) may have been followed by a NEWER wait of the same
   * agent, and removing by task id would silently take that one instead, which
   * then never states an outcome and is never resumed. A removal of a wait
   * that is not the one standing is a defect in the caller, so it throws.
   */
  private removeWait(entry: Waiting): void {
    const standing = this.waiting.get(entry.chain.taskId);
    if (standing !== entry) {
      const detail = `wait ${String(entry.id)} of agent ${entry.chain.taskId} is not the standing wait (${standing === undefined ? "none stands" : `wait ${String(standing.id)} stands`})`;
      LOGGER.error(
        { task_id: entry.chain.taskId, wait_id: entry.id, standing_wait_id: standing?.id, cause: detail },
        "a network-resume wait was removed that is not the one standing",
      );
      throw new Error(detail);
    }
    this.waiting.delete(entry.chain.taskId);
  }

  /**
   * Retire the agent's standing wait, if one stands, because a NEWER failure of
   * the same agent is opening its own: the agent ran again while it waited (a
   * resume someone else delivered) and failed again. The old wait is marked
   * ended, so a delivery still out for it answers for it alone and never for
   * the newer one. ON THE WIRE IT IS ENDED BY THE NEW WAITING EDGE for the
   * same agent, which the restated set already expresses (ruled 2026-09-30:
   * no outcome arm, no proto change).
   */
  private supersede(taskId: string): void {
    const standing = this.waiting.get(taskId);
    if (standing === undefined) return;
    this.removeWait(standing);
    standing.ended = "superseded";
    LOGGER.info(
      { task_id: taskId, wait_id: standing.id, work: standing.work, next_wait_id: this.nextWaitId },
      "the agent failed again while a wait for it stood; the new waiting edge for the same agent ends that wait, as the restated waiting set expresses, with no outcome of its own",
    );
  }

  /**
   * Whether a wait a delivery was started for ENDED while the delivery was
   * out (its window ran out meanwhile). Such a delivery answers for its own
   * wait only: whatever the agent holds now (a newer wait, a newer history)
   * belongs to a later failure and keeps its own lifecycle.
   */
  private endedMeanwhile(entry: Waiting, delivery: ResumeDelivery["kind"]): boolean {
    if (entry.ended === undefined) return false;
    const standing = this.waiting.get(entry.chain.taskId);
    LOGGER.info(
      {
        task_id: entry.chain.taskId,
        wait_id: entry.id,
        ended: entry.ended,
        delivery,
        standing_wait_id: standing?.id,
      },
      "a resume delivery finished after the wait it was started for had ended; it ends only that wait and never a newer one",
    );
    return true;
  }

  private cancelLoopIfIdle(): void {
    if (this.waiting.size === 0) this.cancelLoop();
  }

  /** Drop the run joins and evidence of an agent nothing will resume. */
  private forgetAgent(taskId: string): void {
    if (this.chains.has(taskId)) return;
    this.evidence.delete(taskId);
    for (const [toolUseId, run] of this.runs) {
      if (run.taskId === taskId) this.runs.delete(toolUseId);
    }
  }
}

/** The prose of a vendor error notice, or `undefined` when it has none. */
function noticeText(message: unknown): string | undefined {
  const content = (message as { content?: unknown } | undefined)?.content;
  const text =
    typeof content === "string"
      ? content
      : Array.isArray(content)
        ? content
            .filter((block) => (block as { type?: unknown }).type === "text")
            .map((block) => (block as { text?: unknown }).text)
            .filter((part): part is string => typeof part === "string")
            .join("")
        : "";
  const trimmed = text.trim();
  return trimmed === "" ? undefined : trimmed;
}

/** The resumed arm of an outcome. */
function resumed(): WaitEnd {
  return { case: "resumed", value: create(conversationv1.SessionNetworkResumeResumedSchema, {}) };
}

/** The give-up arm of an outcome. */
function gaveUp(): WaitEnd {
  return { case: "gaveUp", value: create(conversationv1.SessionNetworkResumeGaveUpSchema, {}) };
}
