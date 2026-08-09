import type { TokenUtilization } from "../src/frontend-proto.js";
import { create } from "@bufbuild/protobuf";
import {
  SessionTokenUtilizationSchema,
  TokenUsageTotalsSchema,
  TokenUtilizationSchema,
  TokenUtilizationSubagentSchema,
  VendorTokenUsageSchema,
  type SessionTokenUtilization,
  type TokenUtilization as GeneratedTokenUtilization,
} from "../../proto/gen/ts/agentshim/frontend/v1/durable_pb";
import { ApiUsageSchema } from "../../proto/gen/ts/agentshim/data/v1/tools_pb";

/** One ungrouped response, defaulted with complete identity and lineage. */
export function ungroupedResponse(over: Partial<TokenUtilization> = {}): TokenUtilization {
  return {
    agentReplSessionId: "session",
    claudeSessionId: "claude",
    rootTurnId: "turn",
    apiRequestId: "request",
    apiMessageId: "message",
    model: "opus",
    actor: "subagent",
    subagent: {
      agentId: "",
      parentToolUseId: "",
      parentAgentId: "parent-agent",
      subagentType: "research",
      taskDescription: "inspect evidence",
    },
    usage: {
      inputTokens: 11,
      outputTokens: 7,
      cacheReadInputTokens: 70,
      cacheCreationInputTokens: 3,
      serviceTier: "standard",
      speed: "normal",
      inferenceGeo: "us",
      iterations: [],
      rawUsage: {},
    },
    ...over,
  };
}

/** Generated-wire form of an ungrouped response. */
export function generatedUngroupedResponse(over: Partial<TokenUtilization> = {}): GeneratedTokenUtilization {
  const response = ungroupedResponse(over);
  if (response.subagent === undefined) throw new Error("ungrouped response fixture requires subagent lineage");
  return create(TokenUtilizationSchema, {
    agentReplSessionId: response.agentReplSessionId,
    claudeSessionId: response.claudeSessionId,
    rootTurnId: response.rootTurnId,
    ...(response.apiRequestId === undefined ? {} : { apiRequestId: response.apiRequestId }),
    apiMessageId: response.apiMessageId,
    model: response.model,
    actor: { case: "subagent", value: create(TokenUtilizationSubagentSchema, response.subagent) },
    usage: create(VendorTokenUsageSchema, {
      inputTokens: BigInt(response.usage.inputTokens),
      outputTokens: BigInt(response.usage.outputTokens),
      cacheReadInputTokens: BigInt(response.usage.cacheReadInputTokens),
      cacheCreationInputTokens: BigInt(response.usage.cacheCreationInputTokens),
      serviceTier: response.usage.serviceTier,
      speed: response.usage.speed,
      inferenceGeo: response.usage.inferenceGeo,
      rawUsage: create(ApiUsageSchema),
    }),
  });
}

/** Generated session aggregate with exact ungrouped response records. */
export function generatedSessionUtilization(responses: GeneratedTokenUtilization[]): SessionTokenUtilization {
  const input = responses.reduce((sum, response) => sum + (response.usage?.inputTokens ?? 0n), 0n);
  const output = responses.reduce((sum, response) => sum + (response.usage?.outputTokens ?? 0n), 0n);
  return create(SessionTokenUtilizationSchema, {
    allAgents: create(TokenUsageTotalsSchema, { inputTokens: input, outputTokens: output }),
    mainAgent: create(TokenUsageTotalsSchema),
    // The daemon resolves the canonical economics onto this aggregate; the
    // renderer requires them rather than re-partitioning the vendor buckets.
    allAgentsTokens: { inputMisses: { unwritten: input }, outputTokens: output },
    mainAgentTokens: {},
    ungroupedSubagentResponses: responses,
  });
}
