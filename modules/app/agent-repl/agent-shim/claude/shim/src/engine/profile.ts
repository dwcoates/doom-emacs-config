/**
 * What kind of session the shim drives (shim.v1 SessionProfile), as the engine
 * and the query factories carry it.
 *
 * THE DAEMON ALWAYS NAMES IT. An agent session is the workspace agent with the
 * user's settings, the coding tools and the agent metaprompt; a chat session
 * answers questions in a zettel thread with no filesystem settings, no tools,
 * and the daemon's own system prompt verbatim.
 */
import type { shimv1 } from "../proto.js";

/**
 * How the agent's workspace stands within its repository
 * (shim.v1.SessionProfileAgentStanding), which the system prompt states last
 * (metaprompt.ts, standingSection).
 */
export type WorkspaceStanding = "main_workspace" | "linked_workspace";

/** An agent working in a workspace. */
export interface AgentProfile {
  readonly kind: "agent";
  /** Whether the workspace is its repository's main workspace. */
  readonly standing: WorkspaceStanding;
}

/** A chat answering questions under the daemon's system prompt. */
export interface ChatProfile {
  readonly kind: "chat";
  /** The whole system prompt, verbatim. Never blank. */
  readonly systemPrompt: string;
}

export type SessionProfile = AgentProfile | ChatProfile;

/**
 * Translate a validated request's profile. An unset profile or arm here is an
 * invariant violation: validateStartSessionRequest refuses both before the
 * engine ever sees the request.
 */
export function profileFromProto(profile: shimv1.SessionProfile | undefined): SessionProfile {
  switch (profile?.kind.case) {
    case "agent":
      return { kind: "agent", standing: standingFromProto(profile.kind.value.standing) };
    case "chat":
      return { kind: "chat", systemPrompt: profile.kind.value.systemPrompt?.text ?? "" };
    default:
      throw new Error("shim session: StartSession reached the engine with no session profile");
  }
}

/**
 * Translate a validated agent profile's standing. An unset standing here is an
 * invariant violation: validateStartSessionRequest refuses it first.
 */
function standingFromProto(standing: shimv1.SessionProfileAgentStanding | undefined): WorkspaceStanding {
  switch (standing?.kind.case) {
    case "mainWorkspace":
      return "main_workspace";
    case "linkedWorkspace":
      return "linked_workspace";
    default:
      throw new Error("shim session: StartSession reached the engine with an agent profile that names no workspace standing");
  }
}
