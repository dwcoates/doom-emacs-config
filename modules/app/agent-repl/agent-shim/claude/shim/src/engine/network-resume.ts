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
 */
import { bindLog } from "../log.js";
import { isAgentTaskType } from "../convert/detached.js";
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
  readonly intervalMs?: number;
  readonly windowMs?: number;
}

/** One agent run the shim saw start. */
interface KnownRun {
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
  readonly chain: Chain;
  readonly failedAtMs: number;
}

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
  private readonly waiting = new Map<string, Waiting>();
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
    }
    this.waiting.clear();
    this.chains.clear();
    this.runs.clear();
    this.evidence.clear();
  }

  // -- what the stream says ---------------------------------------------------

  private noteStarted(message: Extract<SdkMessage, { subtype: "task_started" }>): void {
    if (!isAgentTaskType(message.task_type)) return;
    if (message.tool_use_id === undefined || message.tool_use_id === "") return;
    this.runs.set(message.tool_use_id, { taskId: message.task_id, description: message.description });
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
    this.enqueue(run, verdict);
  }

  // -- the wait ---------------------------------------------------------------

  private enqueue(run: KnownRun, verdict: FailureClassification): void {
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
      this.giveUp(chain, now, now, "its window had already run out when it failed again");
      return;
    }
    this.waiting.set(run.taskId, { chain, failedAtMs: now });
    LOGGER.info(
      {
        task_id: run.taskId,
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
    for (const entry of [...this.waiting.values()]) {
      if (now < entry.chain.windowStartMs + this.windowMs) continue;
      this.waiting.delete(entry.chain.taskId);
      this.giveUp(entry.chain, entry.failedAtMs, now, "the API stayed unreachable for the whole window");
    }
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
      case "unavailable":
        for (const entry of due) {
          this.waiting.delete(entry.chain.taskId);
          this.giveUp(entry.chain, entry.failedAtMs, now, `the resume could not be delivered: ${delivery.detail}`);
        }
        this.cancelLoopIfIdle();
        return;
      case "delivered":
        for (const entry of due) {
          this.waiting.delete(entry.chain.taskId);
          entry.chain.resumes += 1;
          entry.chain.resumedAtMs = now;
          entry.chain.progressSinceResume = false;
          LOGGER.info(
            {
              task_id: entry.chain.taskId,
              description: entry.chain.description,
              waited_ms: now - entry.failedAtMs,
              resumes: entry.chain.resumes,
              probe: answer.detail,
              outcome: "resumed",
            },
            "the API is reachable again; asked the main agent to continue the background agent a network outage cut off",
          );
        }
        this.cancelLoopIfIdle();
        return;
    }
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
