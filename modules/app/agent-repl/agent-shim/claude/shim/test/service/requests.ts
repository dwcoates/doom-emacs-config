/**
 * Minimal LEGAL requests for every shim.v1 verb, shared by the routing and
 * validation suites.
 *
 * Kept in one place so a validation suite that asserts a refusal and a routing
 * suite that asserts delivery are working from the SAME notion of "legal": a
 * routing test that quietly used an illegal request would assert that the
 * handler forwarded something validation should have stopped.
 */
import { create } from "@bufbuild/protobuf";
import { conversationv1, shimv1 } from "../../src/proto.js";

/** A permission mode with its oneof arm set. */
export function permissionMode(): conversationv1.AgentPermissionMode {
  return create(conversationv1.AgentPermissionModeSchema, {
    mode: { case: "default", value: create(conversationv1.AgentPermissionModeDefaultSchema, {}) },
  });
}

/** One utterance with one text block — the smallest thing that is a prompt. */
export function said(text = "hello"): conversationv1.UserSaid {
  return create(conversationv1.UserSaidSchema, {
    content: create(conversationv1.UserContentSchema, {
      blocks: [
        create(conversationv1.UserContentBlockSchema, {
          block: { case: "text", value: create(conversationv1.TextBlockSchema, { text }) },
        }),
      ],
    }),
  });
}

export function startSessionRequest(): shimv1.StartSessionRequest {
  return create(shimv1.StartSessionRequestSchema, {
    source: {
      case: "fresh",
      value: create(shimv1.StartSessionFreshSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-opus-5" }),
        permissionMode: permissionMode(),
      }),
    },
  });
}

export function watchSessionRequest(): shimv1.WatchSessionRequest {
  return create(shimv1.WatchSessionRequestSchema, {});
}

export function setSessionModelRequest(): shimv1.SetSessionModelRequest {
  return create(shimv1.SetSessionModelRequestSchema, {
    model: create(conversationv1.AgentModelSchema, { name: "claude-opus-5" }),
    coldThresholdTokens: 100n,
  });
}

export function setSessionEffortRequest(): shimv1.SetSessionEffortRequest {
  return create(shimv1.SetSessionEffortRequestSchema, { effort: conversationv1.AgentEffortLevel.HIGH });
}

export function setSessionPermissionModeRequest(): shimv1.SetSessionPermissionModeRequest {
  return create(shimv1.SetSessionPermissionModeRequestSchema, { permissionMode: permissionMode() });
}

export function hibernateRequest(): shimv1.HibernateRequest {
  return create(shimv1.HibernateRequestSchema, {});
}

export function killSessionRequest(): shimv1.KillSessionRequest {
  return create(shimv1.KillSessionRequestSchema, { force: false });
}

export function startTurnRequest(): shimv1.StartTurnRequest {
  return create(shimv1.StartTurnRequestSchema, {
    turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
    said: said(),
    origin: conversationv1.PromptOrigin.USER_SENT,
  });
}

export function watchAgentRequest(): shimv1.WatchAgentRequest {
  return create(shimv1.WatchAgentRequestSchema, {});
}

export function updateAgentRequest(): shimv1.UpdateAgentRequest {
  return create(shimv1.UpdateAgentRequestSchema, {
    input: create(conversationv1.AgentInputSchema, {
      input: { case: "stop", value: create(conversationv1.AgentStopSchema, {}) },
    }),
  });
}

export function killTurnRequest(): shimv1.KillTurnRequest {
  return create(shimv1.KillTurnRequestSchema, {
    turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
  });
}

/** A rollback cutting before `toBefore`, dropping `dropped`, keeping or restoring the files. */
export function rollBackSessionRequestFor(
  toBefore: string,
  dropped: readonly string[],
  files: "keep" | "restore" = "keep",
): shimv1.RollBackSessionRequest {
  return create(shimv1.RollBackSessionRequestSchema, {
    toBefore: create(conversationv1.TurnIdSchema, { value: toBefore }),
    droppedTurns: dropped.map((value) => create(conversationv1.TurnIdSchema, { value })),
    files:
      files === "keep"
        ? { case: "keepFiles", value: create(shimv1.RollBackSessionKeepFilesSchema, {}) }
        : { case: "restoreFiles", value: create(shimv1.RollBackSessionRestoreFilesSchema, {}) },
  });
}

export function rollBackSessionRequest(): shimv1.RollBackSessionRequest {
  return rollBackSessionRequestFor("turn-1", ["turn-1", "turn-2"]);
}

export function watchBashRequest(): shimv1.WatchBashRequest {
  return create(shimv1.WatchBashRequestSchema, {
    work: create(conversationv1.DetachedWorkIdSchema, { value: "b1" }),
  });
}

export function stopBashRequest(): shimv1.StopBashRequest {
  return create(shimv1.StopBashRequestSchema, {
    work: create(conversationv1.DetachedWorkIdSchema, { value: "b1" }),
  });
}

export function detachForegroundRequest(): shimv1.DetachForegroundRequest {
  return create(shimv1.DetachForegroundRequestSchema, {
    unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_1" }),
  });
}

export function readHistoryRequest(): shimv1.ReadHistoryRequest {
  return create(shimv1.ReadHistoryRequestSchema, {
    position: { case: "first", value: create(shimv1.ReadHistoryFirstSchema, {}) },
  });
}

export function gatherTitleDigestRequest(): shimv1.GatherTitleDigestRequest {
  return create(shimv1.GatherTitleDigestRequestSchema, {});
}

export function getWorkflowRequest(): shimv1.GetWorkflowRequest {
  return create(shimv1.GetWorkflowRequestSchema, {
    work: create(conversationv1.DetachedWorkIdSchema, { value: "w1" }),
  });
}

export function watchWorkflowRequest(): shimv1.WatchWorkflowRequest {
  return create(shimv1.WatchWorkflowRequestSchema, {
    watch: create(shimv1.WorkflowWatchTokenSchema, { value: "tok" }),
  });
}

export function stopWorkflowRequest(): shimv1.StopWorkflowRequest {
  return create(shimv1.StopWorkflowRequestSchema, {
    work: create(conversationv1.DetachedWorkIdSchema, { value: "w1" }),
  });
}
