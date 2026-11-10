/**
 * engine/parked-keepalive.ts — keep a PARKED conversation's cache warm.
 *
 * A workspace that took the queue bypass mode keeps two conversations, and
 * only one runs as its session; the other is parked (owner request,
 * 2026-11-10: both are kept warm in tandem). The daemon times the parked
 * conversation's beats from its own clock and asks this shim for each one
 * (shim.v1.SendKeepaliveRequest.parked).
 *
 * THE BEAT RUNS ON A QUERY OF ITS OWN, never the session's: the session's
 * query owns its identity, its turn slot and its anchor, and a parked
 * conversation is none of those. The query resumes the parked conversation
 * under the profile its own session runs with, because the prompt cache is
 * keyed on the system prompt, sends the keep-alive prompt, reads its one
 * result, and is closed (engine/side-query.ts). Nothing of it is stored: no
 * entry reaches the writer, and the sidecar skips the transcript's keep-alive
 * records by their marker as it skips the session's own.
 *
 * A BEAT NEVER BUILDS ON AN EARLIER BEAT. Every beat appends a keep-alive turn
 * to the parked transcript, so the next one resumes THROUGH the last record
 * before the trailing keep-alive turns ({@link parkedResumeAnchor}), the same
 * declared `resumeSessionAt` surface the session's own rewind uses: the
 * transcript never holds more than the one keep-alive the latest beat sent.
 *
 * THE CACHE JUDGMENT IS THE SHIM'S, read off the transcript's last request and
 * the tier it bought (engine/cold.ts), as the session's own beat judges its
 * cache: a lapsed cache is answered `cache_lapsed`, because a keep-alive then
 * would buy the full-price read the cold gate exists to make the user decide on.
 */
import type { shimv1 } from "../proto.js";
import { bindLog } from "../log.js";
import type { QueryLike } from "../sdk/types.js";
import { readTranscriptFacts } from "./cold.js";
import { keepalivePromptText } from "./keepalive.js";
import { profileFromProto, type SessionProfile } from "./profile.js";
import type { PromptQueue } from "./prompt-queue.js";
import { isKeepaliveRecord, isPromptRecord, readTranscriptChain, type ChainRecord } from "./rollback.js";
import { askSideQuery } from "./side-query.js";
import { transcriptPath } from "./vendor-paths.js";

const LOGGER = bindLog({ component: "shim-engine-parked-keepalive", operation: "shim.engine.parked_keepalive" });

/** How one parked beat ended. THE KIND IS THE OUTCOME. */
export type ParkedBeatOutcome =
  /** The keep-alive was answered: the cache is warm again. */
  | { readonly kind: "sent" }
  /** The cache had already lapsed at LAPSEDATMS (0: the conversation never reached the model). */
  | { readonly kind: "lapsed"; readonly lapsedAtMs: number }
  /** The beat could not be made: no transcript, a broken chain, or a query that did not answer. */
  | { readonly kind: "failed"; readonly detail: string };

/** What a parked beat builds its query with. */
export interface ParkedBeatQuery {
  readonly profile: SessionProfile;
  readonly vendorSessionId: string;
  readonly model?: string;
  readonly resumeSessionAt?: string;
  readonly prompt: PromptQueue;
  readonly abortController: AbortController;
}

/** What a parked beat needs that it does not own. */
export interface ParkedBeatDeps {
  /** The account root this shim runs under (`CLAUDE_CONFIG_DIR`). */
  readonly configDir: string;
  /** This shim's working directory: where the workspace's transcripts are filed. */
  readonly cwd: string;
  readonly nowMs: () => number;
  /** Build the beat's own query: the session's query factory, with its fixed policy. */
  readonly createQuery: (spec: ParkedBeatQuery) => Promise<QueryLike>;
}

/** The counter every parked keep-alive prompt carries, so no two are the same (engine/keepalive.ts). */
let parkedKeepaliveCount = 0;

/** Send one keep-alive to the parked conversation PARKED names. */
export async function beatParkedConversation(
  deps: ParkedBeatDeps,
  parked: shimv1.SendKeepaliveParked,
): Promise<ParkedBeatOutcome> {
  const vendorSessionId = parked.vendorSessionId;
  const context = { vendor_session_id: vendorSessionId };
  const file = transcriptPath(deps.configDir, deps.cwd, vendorSessionId);
  const facts = readTranscriptFacts(file);
  if (facts === undefined) {
    const detail = `no transcript at ${file}`;
    LOGGER.error({ ...context, transcript: file }, "the parked conversation's keep-alive was not sent: its transcript is not on disk");
    return { kind: "failed", detail };
  }
  if (facts.lastRequestAtMs === 0) {
    LOGGER.info(context, "the parked conversation never reached the model; it holds no cache to keep");
    return { kind: "lapsed", lapsedAtMs: 0 };
  }
  const lapsesAtMs = facts.lastRequestAtMs + facts.cacheTtlMs;
  if (deps.nowMs() > lapsesAtMs) {
    LOGGER.info({ ...context, lapsed_at_ms: lapsesAtMs }, "the parked conversation's cache had already lapsed; no keep-alive was sent");
    return { kind: "lapsed", lapsedAtMs: lapsesAtMs };
  }
  const chain = readTranscriptChain(file);
  if (chain.kind !== "ok") {
    const detail = chain.kind === "unreadable" ? chain.detail : `no transcript at ${file}`;
    LOGGER.error({ ...context, transcript: file, cause: detail }, "the parked conversation's keep-alive was not sent: its chain could not be read");
    return { kind: "failed", detail };
  }
  const resumeSessionAt = parkedResumeAnchor(chain.transcript.chain);
  const profile = profileFromProto(parked.profile);
  parkedKeepaliveCount += 1;
  const asked = await askSideQuery(
    (prompt, abortController) =>
      deps.createQuery({
        profile,
        vendorSessionId,
        ...(facts.lastModel === undefined ? {} : { model: facts.lastModel }),
        ...(resumeSessionAt === undefined ? {} : { resumeSessionAt }),
        prompt,
        abortController,
      }),
    keepalivePromptText(parkedKeepaliveCount),
  );
  switch (asked.kind) {
    case "answered":
      LOGGER.info({ ...context, resume_session_at: resumeSessionAt ?? "" }, "kept the parked conversation's cache warm");
      return { kind: "sent" };
    case "errorResult":
      return failed(context, `the vendor answered the keep-alive with an error: ${asked.words}`);
    case "noResult":
      return failed(context, "the keep-alive's query ended without a result");
    case "unbuilt":
      return failed(context, `the keep-alive's query could not be built: ${asked.error}`);
    case "broken":
      return failed(context, `the keep-alive's query broke while it was read: ${asked.error}`);
  }
}

function failed(context: Record<string, string>, detail: string): ParkedBeatOutcome {
  LOGGER.error({ ...context, cause: detail }, "the parked conversation's keep-alive failed");
  return { kind: "failed", detail };
}

/**
 * Where a parked beat resumes: THROUGH the last main-thread record before the
 * trailing run of keep-alive turns on CHAIN, so the beat never builds on an
 * earlier one. Undefined when the chain ends on no keep-alive turn: the beat
 * resumes the whole conversation.
 */
export function parkedResumeAnchor(chain: readonly ChainRecord[]): string | undefined {
  let cut: number | undefined;
  for (let at = chain.length - 1; at >= 0; at -= 1) {
    const record = chain[at];
    if (record === undefined) break;
    if (isKeepaliveRecord(record)) {
      cut = at;
      continue;
    }
    if (isPromptRecord(record)) break;
  }
  if (cut === undefined) return undefined;
  for (let at = cut - 1; at >= 0; at -= 1) {
    const record = chain[at];
    if (record === undefined) break;
    if (record.isSidechain === true) continue;
    if (record.type === "assistant" || record.type === "user") return record.uuid;
  }
  throw new Error("parked keep-alive: the transcript's keep-alive turns follow no record of a real turn to resume at");
}
