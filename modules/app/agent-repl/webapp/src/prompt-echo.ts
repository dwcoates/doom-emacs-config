/**
 * prompt-echo — HOW LONG A SUBMITTED PROMPT TOOK TO BE DRAWN.
 *
 * `prompt_echo` (proto/vocab/latencies.json) starts in Emacs, when the person
 * submits, and ends in this page, when it draws the prompt's user-prompt row.
 * Emacs stamps the submit instant on the request; the daemon hands it back on
 * the web stream (`WatchWebWorkspaceResponse.prompt_echo`) keyed by the turn
 * the prompt minted, which is the `FeedRow.turn` the row is stamped with.
 *
 * THE ROW AND THE ECHO TRAVEL DIFFERENT STREAMS, so either may arrive first.
 * Whichever comes first waits for the other under the turn; the pair measures
 * once, from the submit instant to the row's FIRST draw, on the host's wall
 * clock (wall-latency.ts). A row redrawn later measures nothing more.
 *
 * Both waits are bounded (`PROMPT_ECHO_MEMORY`): the page draws every prompt of
 * its history on boot, and an echo whose row this page never draws (a prompt
 * sent into a subagent's feed) would otherwise wait forever. The oldest go
 * first; the latest prompt is the one being timed.
 */
import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { log } from "./log.js";
import { recordWallLatency, type RecordLatency } from "./wall-latency.js";

/** How many unpaired draws, and unpaired echoes, are kept. */
export const PROMPT_ECHO_MEMORY = 64;

/** The page's prompt echo timing. */
export interface PromptEcho {
  /** The daemon handed back TURN's submit instant. */
  echoed(turn: string, submittedAtMs: number): void;
  /** The page drew the user-prompt row stamped with TURN. */
  drawn(turn: string): void;
}

/** Time prompt echoes on WALLNOW, recording each through RECORD. */
export function createPromptEcho(wallNow: () => number, record: RecordLatency): PromptEcho {
  const echoes = new Map<string, number>();
  const draws = new Map<string, number>();
  const timed = new Map<string, true>();

  /** Keep TURN's VALUE in INTO, dropping the oldest past the bound. */
  const remember = <V>(into: Map<string, V>, turn: string, value: V): void => {
    into.set(turn, value);
    if (into.size <= PROMPT_ECHO_MEMORY) return;
    into.delete(into.keys().next().value as string);
  };

  const measure = (turn: string, submittedAtMs: number, drawnAtMs: number, first: "echo" | "row"): void => {
    remember(timed, turn, true);
    log.debug("timed a prompt's echo", {
      operation: "latency.prompt-echo",
      context: { turn, first, seconds: (drawnAtMs - submittedAtMs) / 1000 },
    });
    recordWallLatency(ReportedLatencyKind.PROMPT_ECHO, submittedAtMs, drawnAtMs, record);
  };

  return {
    echoed(turn, submittedAtMs) {
      if (timed.has(turn)) return;
      const drawnAt = draws.get(turn);
      if (drawnAt === undefined) {
        remember(echoes, turn, submittedAtMs);
        return;
      }
      draws.delete(turn);
      measure(turn, submittedAtMs, drawnAt, "row");
    },
    drawn(turn) {
      if (timed.has(turn) || draws.has(turn)) return;
      const now = wallNow();
      const submittedAt = echoes.get(turn);
      if (submittedAt === undefined) {
        remember(draws, turn, now);
        return;
      }
      echoes.delete(turn);
      measure(turn, submittedAt, now, "echo");
    },
  };
}

let installed: PromptEcho | undefined;

/** Make ECHO the page's prompt echo timing; answers the uninstall. */
export function installPromptEcho(echo: PromptEcho): () => void {
  installed = echo;
  return () => {
    if (installed === echo) installed = undefined;
  };
}

/**
 * The feed drew a user-prompt row stamped with TURN. A page with no timing
 * installed (a test mounting the feed alone) times nothing.
 */
export function userPromptDrawn(turn: string | undefined): void {
  if (turn === undefined || turn === "") return;
  installed?.drawn(turn);
}
