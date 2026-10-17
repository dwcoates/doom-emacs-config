/**
 * addresses — the agent-to-agent headers a test builds a FeedAgentPrompt with,
 * so every suite spells one header one way.
 */
import { create } from "@bufbuild/protobuf";
import {
  FeedAgentPromptAddressSchema,
  type FeedAgentPromptAddress,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";

/** A prompt that went OUT to the subagent NAME (named by its requested name). */
export function sentTo(name: string): FeedAgentPromptAddress {
  return create(FeedAgentPromptAddressSchema, {
    name: { text: name },
    direction: { case: "sent", value: { recipient: { case: "agent", value: { identity: { case: "named", value: {} } } } } },
  });
}

/** A prompt that came IN from the agent NAME (the session's main agent). */
export function receivedFrom(name: string): FeedAgentPromptAddress {
  return create(FeedAgentPromptAddressSchema, {
    name: { text: name },
    direction: { case: "received", value: { agent: { identity: { case: "mainAgent", value: {} } } } },
  });
}

/** A send that reached the agent-repl workspace NAME, in another session. */
export function sentToWorkspace(name: string): FeedAgentPromptAddress {
  return create(FeedAgentPromptAddressSchema, {
    name: { text: name },
    direction: {
      case: "sent",
      value: { recipient: { case: "peerSession", value: { recipient: { case: "workspace", value: {} } } } },
    },
  });
}
