/**
 * engine/identity.ts — the main agent's identity, and what a vendor id rotation
 * does to it.
 *
 * OWNER: the session-engine agent.
 *
 * RESPONSIBILITY. The main agent's `AgentId` is the conversation's ORIGINAL
 * vendor session id (R9). This module mints it on the first fresh start,
 * PERSISTS it under `$AGENT_REPL_STATE_DIR/shim/<workspace-key>/agent-id`, and
 * reports it unchanged on every later StartSession — including after a rotation
 * that gave the conversation a new resume handle. Persistence is what makes the
 * identity survive a bounce: without it, a restarted shim would mint a second
 * AgentId for one conversation and split its book at the restart.
 *
 * WHAT ROTATION CHANGES, AND WHAT IT MUST NOT. A `/clear` retires the vendor
 * transcript identity and mints a new one, and `SessionIdentityRotated` reports
 * BOTH ids. Only the RESUME HANDLE moves; the AgentId does not. The engine
 * agent settles the exact rotation/fork/resume rule empirically against real
 * file behavior and writes the finding into shim.md's mock section.
 */
export interface AgentIdentityStore {
  /** The main agent's persisted id for this workspace, or absence on a first run. */
  read(): Promise<string | undefined>;
  /** Persist the original vendor session id as this conversation's agent id. */
  write(originalVendorSessionId: string): Promise<void>;
}
