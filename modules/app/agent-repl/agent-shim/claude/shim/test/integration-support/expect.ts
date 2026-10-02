/**
 * test/integration-support/expect.ts — read one arm out of a response, loudly.
 *
 * Every shim.v1 response is a oneof, and the assertions in these suites are
 * about WHICH ARM came back. Reading an arm with `?.` and comparing the result
 * to `undefined` turns "the shim refused with the wrong cause" into "expected
 * undefined to be X", which says nothing about what actually happened. These
 * helpers throw with the arm that WAS set and its detail, so a failing test
 * reports the shim's actual answer.
 */
import type { conversationv1, shimv1 } from "../../src/proto.js";

function wrongArm(rpc: string, got: string | undefined, detail?: string): Error {
  return new Error(
    `${rpc}: expected the success arm, got ${got ?? "an UNSET oneof"}${
      detail === undefined || detail === "" ? "" : ` — ${detail}`
    }`,
  );
}

/** The started session, or a failure naming the cause the shim answered with. */
export function sessionStarted(
  response: shimv1.StartSessionResponse,
): conversationv1.SessionStarted {
  if (response.result.case !== "success") {
    throw wrongArm(
      "StartSession",
      response.result.case === "failure"
        ? `failure.${response.result.value.cause.case ?? "unset"}`
        : response.result.case,
      response.result.case === "failure" ? response.result.value.detail : undefined,
    );
  }
  const started = response.result.value.session;
  if (started === undefined) throw new Error("StartSession success carried no SessionStarted");
  return started;
}

/** The failure arm's case name, or a failure saying the call succeeded. */
export function startSessionCause(response: shimv1.StartSessionResponse): string {
  if (response.result.case !== "failure") {
    throw new Error(`StartSession: expected a failure, got ${response.result.case ?? "an unset oneof"}`);
  }
  return response.result.value.cause.case ?? "unset";
}

/** The refusal's own detail — the text the daemon relays verbatim. */
export function startSessionDetail(response: shimv1.StartSessionResponse): string {
  if (response.result.case !== "failure") {
    throw new Error(`StartSession: expected a failure, got ${response.result.case ?? "an unset oneof"}`);
  }
  return response.result.value.detail;
}

/** The `cold` evidence a refusal carried. */
export function startSessionCold(response: shimv1.StartSessionResponse): conversationv1.SessionCold {
  if (response.result.case !== "failure" || response.result.value.cause.case !== "cold") {
    throw new Error(
      `StartSession: expected failure.cold, got ${startSessionCause(response)}`,
    );
  }
  return response.result.value.cause.value;
}

/** The delivered prompt. */
export function turnStarted(response: shimv1.StartTurnResponse): conversationv1.AgentPrompt {
  if (response.result.case !== "success") {
    throw wrongArm(
      "StartTurn",
      response.result.case === "failure"
        ? `failure.${response.result.value.kind.case ?? "unset"}`
        : response.result.case,
      response.result.case === "failure" ? response.result.value.detail : undefined,
    );
  }
  const prompt = response.result.value.prompt;
  if (prompt === undefined) throw new Error("StartTurn success carried no AgentPrompt");
  return prompt;
}

/** The StartTurn refusal's kind. */
export function startTurnKind(response: shimv1.StartTurnResponse): string {
  if (response.result.case !== "failure") {
    throw new Error(`StartTurn: expected a failure, got ${response.result.case ?? "an unset oneof"}`);
  }
  return response.result.value.kind.case ?? "unset";
}

/** Assert UpdateAgent was accepted. */
export function updateAccepted(response: shimv1.UpdateAgentResponse): void {
  if (response.result.case !== "success") {
    throw wrongArm(
      "UpdateAgent",
      response.result.case === "failure"
        ? `failure.${response.result.value.kind.case ?? "unset"}`
        : response.result.case,
      response.result.case === "failure" ? response.result.value.detail : undefined,
    );
  }
}

/** The UpdateAgent refusal's kind. */
export function updateAgentKind(response: shimv1.UpdateAgentResponse): string {
  if (response.result.case !== "failure") {
    throw new Error(`UpdateAgent: expected a failure, got ${response.result.case ?? "an unset oneof"}`);
  }
  return response.result.value.kind.case ?? "unset";
}

/** How the turn was killed. */
export function turnKilled(response: shimv1.KillTurnResponse): conversationv1.TurnKilled {
  if (response.result.case !== "success") {
    throw wrongArm(
      "KillTurn",
      response.result.case === "failure"
        ? `failure.${response.result.value.cause.case ?? "unset"}`
        : response.result.case,
      response.result.case === "failure" ? response.result.value.detail : undefined,
    );
  }
  const killed = response.result.value.killed;
  if (killed === undefined) throw new Error("KillTurn success carried no TurnKilled");
  return killed;
}

/** The KillTurn refusal's cause. */
export function killTurnCause(response: shimv1.KillTurnResponse): string {
  if (response.result.case !== "failure") {
    throw new Error(`KillTurn: expected a failure, got ${response.result.case ?? "an unset oneof"}`);
  }
  return response.result.value.cause.case ?? "unset";
}

/** How the session was killed. */
export function sessionKilled(response: shimv1.KillSessionResponse): conversationv1.SessionKilled {
  if (response.result.case !== "success") {
    throw wrongArm(
      "KillSession",
      response.result.case === "failure"
        ? `failure.${response.result.value.cause.case ?? "unset"}`
        : response.result.case,
      response.result.case === "failure" ? response.result.value.detail : undefined,
    );
  }
  const closed = response.result.value.closed;
  if (closed === undefined) throw new Error("KillSession success carried no SessionKilled");
  return closed;
}

/** The KillSession refusal's cause. */
export function killSessionCause(response: shimv1.KillSessionResponse): string {
  if (response.result.case !== "failure") {
    throw new Error(`KillSession: expected a failure, got ${response.result.case ?? "an unset oneof"}`);
  }
  return response.result.value.cause.case ?? "unset";
}

/** The live work a KillSession refusal named. */
export function killSessionLive(response: shimv1.KillSessionResponse): conversationv1.SessionLive {
  if (response.result.case !== "failure" || response.result.value.cause.case !== "live") {
    throw new Error(`KillSession: expected failure.live, got ${killSessionCause(response)}`);
  }
  return response.result.value.cause.value;
}

/** One page of history. */
export function historyPage(response: shimv1.ReadHistoryResponse): conversationv1.HistoryPage {
  if (response.result.case !== "success") {
    throw wrongArm(
      "ReadHistory",
      response.result.case === "failure"
        ? `failure.${response.result.value.kind.case ?? "unset"}`
        : response.result.case,
      response.result.case === "failure" ? response.result.value.detail : undefined,
    );
  }
  const page = response.result.value.page;
  if (page === undefined) throw new Error("ReadHistory success carried no HistoryPage");
  return page;
}

/** The ReadHistory refusal's kind. */
export function readHistoryKind(response: shimv1.ReadHistoryResponse): string {
  if (response.result.case !== "failure") {
    throw new Error(`ReadHistory: expected a failure, got ${response.result.case ?? "an unset oneof"}`);
  }
  return response.result.value.kind.case ?? "unset";
}

/** The SetSessionModel refusal's cause. */
export function setModelCause(response: shimv1.SetSessionModelResponse): string {
  if (response.result.case !== "failure") {
    throw new Error(
      `SetSessionModel: expected a failure, got ${response.result.case ?? "an unset oneof"}`,
    );
  }
  return response.result.value.cause.case ?? "unset";
}

/** Assert SetSessionModel succeeded. */
export function setModelAccepted(response: shimv1.SetSessionModelResponse): void {
  if (response.result.case !== "success") {
    throw wrongArm(
      "SetSessionModel",
      response.result.case === "failure"
        ? `failure.${response.result.value.cause.case ?? "unset"}`
        : response.result.case,
      response.result.case === "failure" ? response.result.value.detail : undefined,
    );
  }
}

/** Assert SetSessionPermissionMode succeeded. */
export function setPermissionModeAccepted(
  response: shimv1.SetSessionPermissionModeResponse,
): void {
  if (response.result.case !== "success") {
    throw wrongArm(
      "SetSessionPermissionMode",
      response.result.case === "failure"
        ? `failure.${response.result.value.kind.case ?? "unset"}`
        : response.result.case,
      response.result.case === "failure" ? response.result.value.detail : undefined,
    );
  }
}

/** The `SessionCold` a SetSessionModel refusal carries — the switch's cost. */
export function setModelCold(
  response: shimv1.SetSessionModelResponse,
): conversationv1.SessionCold {
  if (response.result.case !== "failure" || response.result.value.cause.case !== "cold") {
    throw new Error(
      `SetSessionModel: expected failure.cold, got ${response.result.case ?? "an unset oneof"}`,
    );
  }
  const cold = response.result.value.cause.value;
  if (cold === undefined) throw new Error("SetSessionModel: failure.cold carries no SessionCold");
  return cold;
}

/** The level a SetSessionEffort success put in effect; throws on a refusal. */
export function setEffortAccepted(
  response: shimv1.SetSessionEffortResponse,
): conversationv1.AgentEffortLevel {
  if (response.result.case !== "success") {
    throw wrongArm(
      "SetSessionEffort",
      response.result.case === "failure"
        ? `failure.${response.result.value.cause.case ?? "unset"}`
        : response.result.case,
      response.result.case === "failure" ? response.result.value.detail : undefined,
    );
  }
  const changed = response.result.value.effortChanged;
  if (changed === undefined) throw new Error("SetSessionEffort: success carries no effort_changed");
  return changed.effectiveEffort;
}

/** The SetSessionEffort refusal's arm. */
export function setEffortCause(response: shimv1.SetSessionEffortResponse): string {
  if (response.result.case !== "failure") {
    throw new Error(`SetSessionEffort: expected a failure, got ${response.result.case ?? "an unset oneof"}`);
  }
  return response.result.value.cause.case ?? "unset";
}

/** The SetSessionPermissionMode refusal's arm. */
export function setPermissionModeCause(
  response: shimv1.SetSessionPermissionModeResponse,
): string {
  if (response.result.case !== "failure") {
    throw new Error(
      `SetSessionPermissionMode: expected a failure, got ${response.result.case ?? "an unset oneof"}`,
    );
  }
  return response.result.value.kind.case ?? "unset";
}

/** The Hibernate error's kind. */
export function hibernateKind(response: shimv1.HibernateResponse): string {
  if (response.result.case !== "error") {
    throw new Error(`Hibernate: expected an error, got ${response.result.case ?? "an unset oneof"}`);
  }
  return response.result.value.kind.case ?? "unset";
}

/** Assert Hibernate acked. */
export function hibernateAcked(response: shimv1.HibernateResponse): void {
  if (response.result.case !== "success") {
    throw wrongArm(
      "Hibernate",
      response.result.case === "error"
        ? `error.${response.result.value.kind.case ?? "unset"}`
        : "an unset oneof",
    );
  }
}

/** The StopBash refusal's kind. */
export function stopBashKind(response: shimv1.StopBashResponse): string {
  if (response.result.case !== "failure") {
    throw new Error(`StopBash: expected a failure, got ${response.result.case ?? "an unset oneof"}`);
  }
  return response.result.value.kind.case ?? "unset";
}

/** Assert StopBash succeeded. */
export function stopBashAccepted(response: shimv1.StopBashResponse): void {
  if (response.result.case !== "success") {
    throw wrongArm(
      "StopBash",
      response.result.case === "failure"
        ? `failure.${response.result.value.kind.case ?? "unset"}`
        : response.result.case,
      response.result.case === "failure" ? response.result.value.detail : undefined,
    );
  }
}

/** The DetachForeground refusal's kind. */
export function detachForegroundKind(response: shimv1.DetachForegroundResponse): string {
  if (response.result.case !== "failure") {
    throw new Error(
      `DetachForeground: expected a failure, got ${response.result.case ?? "an unset oneof"}`,
    );
  }
  return response.result.value.kind.case ?? "unset";
}

/** Assert DetachForeground succeeded. */
export function detachForegroundAccepted(response: shimv1.DetachForegroundResponse): void {
  if (response.result.case !== "success") {
    throw wrongArm(
      "DetachForeground",
      response.result.case === "failure"
        ? `failure.${response.result.value.kind.case ?? "unset"}`
        : response.result.case,
      response.result.case === "failure" ? response.result.value.detail : undefined,
    );
  }
}

// ---------------------------------------------------------------------------
// stream frames
// ---------------------------------------------------------------------------

/**
 * The `SessionUpdate` a WatchSession frame carries.
 *
 * Raises on the `session_started` re-announcement rather than skipping it: a
 * caller that wants updates asks for updates, and a helper that quietly
 * swallowed the other arm would hide a frame ordering defect.
 */
export function sessionUpdate(frame: shimv1.WatchSessionResponse): conversationv1.SessionUpdate {
  if (frame.frame.case !== "update") {
    throw new Error(
      `WatchSession pushed a ${frame.frame.case ?? "unset"} frame where a SessionUpdate was expected`,
    );
  }
  return frame.frame.value;
}

/** The `SessionStarted` re-announcement a WatchSession frame carries. */
export function sessionStartedFrame(
  frame: shimv1.WatchSessionResponse,
): conversationv1.SessionStarted {
  if (frame.frame.case !== "sessionStarted") {
    throw new Error(
      `WatchSession pushed a ${frame.frame.case ?? "unset"} frame where the re-announcement was expected`,
    );
  }
  return frame.frame.value;
}

/** The arm name of a WatchSession frame. */
export function sessionUpdateArm(frame: shimv1.WatchSessionResponse): string {
  return sessionUpdate(frame).update.case ?? "unset";
}

/** The opening page of a WatchAgent stream. */
export function watchAgentPage(frame: shimv1.WatchAgentResponse): conversationv1.HistoryPage {
  if (frame.frame.case !== "page") {
    throw new Error(`WatchAgent: expected the opening page, got ${frame.frame.case ?? "an unset oneof"}`);
  }
  return frame.frame.value;
}

/** A tailed entry of a WatchAgent stream. */
export function watchAgentEntry(frame: shimv1.WatchAgentResponse): conversationv1.HistoryEntryAt {
  if (frame.frame.case !== "entry") {
    throw new Error(`WatchAgent: expected an entry, got ${frame.frame.case ?? "an unset oneof"}`);
  }
  return frame.frame.value;
}

/** The `AgentFrame` inside a history entry, when it is one. */
export function entryFrame(entry: conversationv1.HistoryEntryAt): conversationv1.AgentFrame | null {
  const inner = entry.entry?.entry;
  return inner?.case === "agentFrame" ? inner.value : null;
}

/** The `AgentPrompt` inside a history entry, when it is one. */
export function entryPrompt(entry: conversationv1.HistoryEntryAt): conversationv1.AgentPrompt | null {
  const inner = entry.entry?.entry;
  return inner?.case === "userPrompt" ? inner.value : null;
}

/** The `AgentUpdate` arm name of a WatchAgent entry, or null when it is not one. */
export function entryUpdateArm(entry: conversationv1.HistoryEntryAt): string | null {
  const frame = entryFrame(entry);
  if (frame?.result.case !== "update") return null;
  return frame.result.value.update.case ?? "unset";
}

/** The `AgentBash` a WatchBash frame carries. */
export function bashFrame(frame: shimv1.WatchBashResponse): conversationv1.AgentBash {
  const bash = frame.bash;
  if (bash === undefined) {
    throw new Error("WatchBash pushed a frame with no AgentBash — the non-optional rule");
  }
  return bash;
}
