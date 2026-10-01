/**
 * The per-request base validators: that each accepts its legal request and
 * refuses the specific illegal shape it owns.
 */
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { describe, expect, it } from "vitest";
import { conversationv1, shimv1 } from "../../../src/proto.js";
import * as validate from "../../../src/service/validate/requests.js";
import * as requests from "../requests.js";

function codeOf(act: () => void): Code | undefined {
  try {
    act();
    return undefined;
  } catch (err) {
    return ConnectError.from(err).code;
  }
}

describe("validateStartSessionRequest", () => {
  it("accepts a fresh start naming a model and a mode", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => validate.validateStartSessionRequest(requests.startSessionRequest())),
    ).toBeUndefined();
  });

  it("accepts a resume naming a vendor session", () => {
    // Arrange.
    const request = create(shimv1.StartSessionRequestSchema, {
      source: {
        case: "resume",
        value: create(shimv1.StartSessionResumeSchema, { vendorSessionId: "v-1" }),
      },
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateStartSessionRequest(request))).toBeUndefined();
  });

  it("refuses a request that is neither fresh nor resume", () => {
    // Arrange.
    const request = create(shimv1.StartSessionRequestSchema, {});

    // Act, Assert.
    expect(codeOf(() => validate.validateStartSessionRequest(request))).toBe(Code.InvalidArgument);
  });

  it("refuses a resume naming no vendor session", () => {
    // Arrange.
    const request = create(shimv1.StartSessionRequestSchema, {
      source: {
        case: "resume",
        value: create(shimv1.StartSessionResumeSchema, { vendorSessionId: "" }),
      },
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateStartSessionRequest(request))).toBe(Code.InvalidArgument);
  });

  it("accepts a fresh start naming NO model: the SDK's default takes effect", () => {
    // Optional since landing 7; SessionStarted.effective_model reports what
    // the SDK chose.
    // Arrange.
    const request = create(shimv1.StartSessionRequestSchema, {
      source: {
        case: "fresh",
        value: create(shimv1.StartSessionFreshSchema, {
          permissionMode: requests.permissionMode(),
        }),
      },
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateStartSessionRequest(request))).toBeUndefined();
  });

  it("still refuses a fresh start whose model is SET but named nothing", () => {
    // Saying nothing and meaning to say something are different requests.
    // Arrange.
    const request = create(shimv1.StartSessionRequestSchema, {
      source: {
        case: "fresh",
        value: create(shimv1.StartSessionFreshSchema, {
          model: create(conversationv1.AgentModelSchema, { name: "" }),
          permissionMode: requests.permissionMode(),
        }),
      },
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateStartSessionRequest(request))).toBe(Code.InvalidArgument);
  });

  it("refuses a fresh start with an unset permission mode", () => {
    // Arrange.
    const request = create(shimv1.StartSessionRequestSchema, {
      source: {
        case: "fresh",
        value: create(shimv1.StartSessionFreshSchema, {
          model: create(conversationv1.AgentModelSchema, { name: "m" }),
        }),
      },
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateStartSessionRequest(request))).toBe(Code.InvalidArgument);
  });
});

describe("validateWatchSessionRequest", () => {
  it("accepts the empty request the contract declares", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => validate.validateWatchSessionRequest(requests.watchSessionRequest())),
    ).toBeUndefined();
  });
});

describe("validateSetSessionModelRequest", () => {
  it("accepts a request naming a model", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => validate.validateSetSessionModelRequest(requests.setSessionModelRequest())),
    ).toBeUndefined();
  });

  it("refuses a request naming no model", () => {
    // Arrange.
    const request = create(shimv1.SetSessionModelRequestSchema, {});

    // Act, Assert.
    expect(codeOf(() => validate.validateSetSessionModelRequest(request))).toBe(
      Code.InvalidArgument,
    );
  });

  it("refuses a cold remediation with no arm set", () => {
    // Arrange.
    const request = create(shimv1.SetSessionModelRequestSchema, {
      model: create(conversationv1.AgentModelSchema, { name: "m" }),
      coldRemediation: create(conversationv1.SessionColdRemediationSchema, {}),
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateSetSessionModelRequest(request))).toBe(
      Code.InvalidArgument,
    );
  });
});

describe("validateSetSessionPermissionModeRequest", () => {
  it("refuses a request whose mode oneof is unset", () => {
    // Arrange.
    const request = create(shimv1.SetSessionPermissionModeRequestSchema, {
      permissionMode: create(conversationv1.AgentPermissionModeSchema, {}),
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateSetSessionPermissionModeRequest(request))).toBe(
      Code.InvalidArgument,
    );
  });
});

describe("validateHibernateRequest", () => {
  it("accepts the empty request the contract declares", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => validate.validateHibernateRequest(requests.hibernateRequest())),
    ).toBeUndefined();
  });
});

describe("validateKillSessionRequest", () => {
  it("accepts force:false, which is a value and not an absence", () => {
    // Arrange.
    const request = create(shimv1.KillSessionRequestSchema, { force: false });

    // Act, Assert.
    expect(codeOf(() => validate.validateKillSessionRequest(request))).toBeUndefined();
  });
});

describe("validateStartTurnRequest", () => {
  it("accepts a fully stated turn", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => validate.validateStartTurnRequest(requests.startTurnRequest())),
    ).toBeUndefined();
  });

  it("refuses a turn with no id, which the shim never mints for itself", () => {
    // Arrange.
    const request = create(shimv1.StartTurnRequestSchema, {
      said: requests.said(),
      origin: conversationv1.PromptOrigin.USER_SENT,
      pageSize: 5,
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateStartTurnRequest(request))).toBe(Code.InvalidArgument);
  });

  it("refuses a turn with an unstated origin", () => {
    // Arrange.
    const request = create(shimv1.StartTurnRequestSchema, {
      turn: create(conversationv1.TurnIdSchema, { value: "t" }),
      said: requests.said(),
      pageSize: 5,
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateStartTurnRequest(request))).toBe(Code.InvalidArgument);
  });

  it("refuses a known_through pointer that is present but empty", () => {
    // Arrange.
    const request = create(shimv1.StartTurnRequestSchema, {
      turn: create(conversationv1.TurnIdSchema, { value: "t" }),
      said: requests.said(),
      origin: conversationv1.PromptOrigin.USER_SENT,
      pageSize: 5,
      knownThrough: create(conversationv1.HistoryPointerSchema, { value: "" }),
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateStartTurnRequest(request))).toBe(Code.InvalidArgument);
  });
});

describe("validateWatchAgentRequest", () => {
  it("accepts an UNSET target, which means the session's prompt thread", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => validate.validateWatchAgentRequest(requests.watchAgentRequest())),
    ).toBeUndefined();
  });

  it("refuses a target that is present but empty", () => {
    // Arrange.
    const request = create(shimv1.WatchAgentRequestSchema, {
      target: create(conversationv1.AgentIdSchema, { value: "" }),
      pageSize: 5,
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateWatchAgentRequest(request))).toBe(Code.InvalidArgument);
  });

  it("refuses a page budget of nothing", () => {
    // Arrange.
    const request = create(shimv1.WatchAgentRequestSchema, { pageSize: 0 });

    // Act, Assert.
    expect(codeOf(() => validate.validateWatchAgentRequest(request))).toBe(Code.InvalidArgument);
  });
});

describe("validateUpdateAgentRequest", () => {
  it("refuses a request carrying no input at all", () => {
    // Arrange.
    const request = create(shimv1.UpdateAgentRequestSchema, {});

    // Act, Assert.
    expect(codeOf(() => validate.validateUpdateAgentRequest(request))).toBe(Code.InvalidArgument);
  });
});

describe("validateKillTurnRequest", () => {
  it("refuses a kill naming no turn", () => {
    // Arrange.
    const request = create(shimv1.KillTurnRequestSchema, { force: true });

    // Act, Assert.
    expect(codeOf(() => validate.validateKillTurnRequest(request))).toBe(Code.InvalidArgument);
  });
});

describe("validateRollBackSessionRequest", () => {
  it("accepts a legal rollback", () => {
    // Arrange.
    const request = requests.rollBackSessionRequest();

    // Act, Assert.
    expect(codeOf(() => validate.validateRollBackSessionRequest(request))).toBeUndefined();
  });

  it("refuses a rollback naming no turn to cut before", () => {
    // Arrange.
    const request = requests.rollBackSessionRequest();
    request.toBefore = undefined;

    // Act, Assert.
    expect(codeOf(() => validate.validateRollBackSessionRequest(request))).toBe(Code.InvalidArgument);
  });

  it("refuses a dropped turn with an empty id", () => {
    // Arrange.
    const request = requests.rollBackSessionRequest();
    request.droppedTurns.push(create(conversationv1.TurnIdSchema, { value: "" }));

    // Act, Assert.
    expect(codeOf(() => validate.validateRollBackSessionRequest(request))).toBe(Code.InvalidArgument);
  });

  it("refuses dropped turns that do not begin with the turn cut before", () => {
    // Arrange.
    const request = requests.rollBackSessionRequest();
    request.droppedTurns = [create(conversationv1.TurnIdSchema, { value: "turn-2" })];

    // Act, Assert.
    expect(codeOf(() => validate.validateRollBackSessionRequest(request))).toBe(Code.InvalidArgument);
  });

  it("refuses a rollback that says neither keep nor restore", () => {
    // Arrange.
    const request = requests.rollBackSessionRequest();
    request.files = { case: undefined };

    // Act, Assert.
    expect(codeOf(() => validate.validateRollBackSessionRequest(request))).toBe(Code.InvalidArgument);
  });
});

describe("validateWatchBashRequest", () => {
  it("refuses a watch naming no work", () => {
    // Arrange.
    const request = create(shimv1.WatchBashRequestSchema, {});

    // Act, Assert.
    expect(codeOf(() => validate.validateWatchBashRequest(request))).toBe(Code.InvalidArgument);
  });
});

describe("validateStopBashRequest", () => {
  it("refuses a stop naming no work", () => {
    // Arrange.
    const request = create(shimv1.StopBashRequestSchema, {});

    // Act, Assert.
    expect(codeOf(() => validate.validateStopBashRequest(request))).toBe(Code.InvalidArgument);
  });
});

describe("validateDetachForegroundRequest", () => {
  it("refuses a detach naming no unit", () => {
    // Arrange.
    const request = create(shimv1.DetachForegroundRequestSchema, {});

    // Act, Assert.
    expect(codeOf(() => validate.validateDetachForegroundRequest(request))).toBe(
      Code.InvalidArgument,
    );
  });
});

describe("validateReadHistoryRequest", () => {
  it("accepts a read from the newest page", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => validate.validateReadHistoryRequest(requests.readHistoryRequest())),
    ).toBeUndefined();
  });

  it("accepts a read continuing from a served pointer", () => {
    // Arrange.
    const request = create(shimv1.ReadHistoryRequestSchema, {
      pageSize: 5,
      position: {
        case: "after",
        value: create(conversationv1.HistoryPointerSchema, { value: "p-1" }),
      },
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateReadHistoryRequest(request))).toBeUndefined();
  });

  it("refuses a read that states no position", () => {
    // Arrange.
    const request = create(shimv1.ReadHistoryRequestSchema, { pageSize: 5 });

    // Act, Assert.
    expect(codeOf(() => validate.validateReadHistoryRequest(request))).toBe(Code.InvalidArgument);
  });

  it("accepts a read of the book as it stood at an instant", () => {
    // Arrange.
    const request = create(shimv1.ReadHistoryRequestSchema, {
      pageSize: 5,
      position: { case: "through", value: create(conversationv1.ConversationThroughSchema, { atMs: 1_000n }) },
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateReadHistoryRequest(request))).toBeUndefined();
  });

  it("refuses a read through a bound that names no instant", () => {
    // Arrange.
    const request = create(shimv1.ReadHistoryRequestSchema, {
      pageSize: 5,
      position: { case: "through", value: create(conversationv1.ConversationThroughSchema, { atMs: 0n }) },
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateReadHistoryRequest(request))).toBe(Code.InvalidArgument);
  });
});

describe("validateStartSessionRequest cold remediation", () => {
  /** The refusal's message, so a field path can be asserted. */
  function messageOf(act: () => void): string | undefined {
    try {
      act();
      return undefined;
    } catch (err) {
      return ConnectError.from(err).message;
    }
  }

  /** A resume naming a vendor session, with whatever remediation is given. */
  function resumeWith(
    coldRemediation: conversationv1.SessionColdRemediation | undefined,
  ): shimv1.StartSessionRequest {
    return create(shimv1.StartSessionRequestSchema, {
      source: {
        case: "resume",
        value: create(shimv1.StartSessionResumeSchema, {
          vendorSessionId: "vendor-1",
          coldRemediation,
        }),
      },
    });
  }

  it("accepts a resume whose remediation names an arm", () => {
    // Arrange.
    const request = resumeWith(
      create(conversationv1.SessionColdRemediationSchema, {
        remediation: { case: "pay", value: create(conversationv1.SessionColdPaySchema, {}) },
      }),
    );

    // Act, Assert.
    expect(codeOf(() => validate.validateStartSessionRequest(request))).toBeUndefined();
  });

  it("refuses a resume whose remediation has no arm set", () => {
    // Arrange.
    const request = resumeWith(create(conversationv1.SessionColdRemediationSchema, {}));

    // Act, Assert.
    expect(codeOf(() => validate.validateStartSessionRequest(request))).toBe(Code.InvalidArgument);
  });

  it("names the resume's own remediation path so the refusal is actionable", () => {
    // Arrange.
    const request = resumeWith(create(conversationv1.SessionColdRemediationSchema, {}));

    // Act, Assert.
    expect(messageOf(() => validate.validateStartSessionRequest(request))).toContain(
      "start_session.resume.cold_remediation",
    );
  });
});

describe("validateWatchAgentRequest known_through", () => {
  it("refuses a known_through pointer that is present but empty", () => {
    // Arrange.
    const request = create(shimv1.WatchAgentRequestSchema, {
      pageSize: 5,
      knownThrough: create(conversationv1.HistoryPointerSchema, { value: "" }),
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateWatchAgentRequest(request))).toBe(Code.InvalidArgument);
  });

  it("accepts a known_through pointer the store served", () => {
    // Arrange.
    const request = create(shimv1.WatchAgentRequestSchema, {
      pageSize: 5,
      knownThrough: create(conversationv1.HistoryPointerSchema, { value: "p-1" }),
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateWatchAgentRequest(request))).toBeUndefined();
  });
});

describe("validateUpdateAgentRequest target", () => {
  it("refuses a target that is present but empty", () => {
    // An empty id is a sentinel, not the unset that addresses the main agent.
    // Arrange.
    const request = create(shimv1.UpdateAgentRequestSchema, {
      target: create(conversationv1.AgentIdSchema, { value: "" }),
      input: create(conversationv1.AgentInputSchema, {
        input: { case: "stop", value: create(conversationv1.AgentStopSchema, {}) },
      }),
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateUpdateAgentRequest(request))).toBe(Code.InvalidArgument);
  });

  it("accepts a named sub-agent target", () => {
    // Arrange.
    const request = create(shimv1.UpdateAgentRequestSchema, {
      target: create(conversationv1.AgentIdSchema, { value: "a-1" }),
      input: create(conversationv1.AgentInputSchema, {
        input: { case: "stop", value: create(conversationv1.AgentStopSchema, {}) },
      }),
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateUpdateAgentRequest(request))).toBeUndefined();
  });
});

describe("validateReadHistoryRequest target", () => {
  it("refuses a target that is present but empty", () => {
    // Arrange.
    const request = create(shimv1.ReadHistoryRequestSchema, {
      target: create(conversationv1.AgentIdSchema, { value: "" }),
      pageSize: 5,
      position: { case: "first", value: create(shimv1.ReadHistoryFirstSchema, {}) },
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateReadHistoryRequest(request))).toBe(Code.InvalidArgument);
  });

  it("accepts a read scoped to a named sub-agent", () => {
    // Arrange.
    const request = create(shimv1.ReadHistoryRequestSchema, {
      target: create(conversationv1.AgentIdSchema, { value: "a-1" }),
      pageSize: 5,
      position: { case: "first", value: create(shimv1.ReadHistoryFirstSchema, {}) },
    });

    // Act, Assert.
    expect(codeOf(() => validate.validateReadHistoryRequest(request))).toBeUndefined();
  });
});
