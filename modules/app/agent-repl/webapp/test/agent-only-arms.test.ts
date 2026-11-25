import { describe, expect, it } from "vitest";
import { CreateWorkspaceErrorSchema } from "../../proto/gen/ts/agentrepl/v1/endpoint_create_workspace_pb";
import { SubmitPromptErrorSchema } from "../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import { AGENT_ONLY_REFUSAL_ARMS, userRefusalArms } from "./agent-only-arms.js";
import { oneofArms } from "./arms.js";

describe("the agent-only refusal arms", () => {
  it.each([
    ["createWorkspace", oneofArms(CreateWorkspaceErrorSchema, "cause")],
    ["submitPrompt", oneofArms(SubmitPromptErrorSchema, "reason")],
  ] as const)("names only arms %s's refusal oneof declares", (rpc, declared) => {
    const missing = [...AGENT_ONLY_REFUSAL_ARMS[rpc]].filter((arm) => !declared.includes(arm));
    expect(missing).toEqual([]);
  });

  it("takes an rpc's agent-only arms out of what its call site words", () => {
    expect(userRefusalArms("submitPrompt", ["noSession", "slashCommandFromAgent"])).toEqual([
      "noSession",
    ]);
  });

  it("keeps every arm of an rpc that has no agent-only ones", () => {
    expect(userRefusalArms("closeWorkspace", ["blocked", "unknownWorkspace"])).toEqual([
      "blocked",
      "unknownWorkspace",
    ]);
  });
});
