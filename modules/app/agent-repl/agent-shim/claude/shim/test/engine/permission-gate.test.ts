/**
 * The one vendor gate: two meanings, one mechanism.
 *
 * WHAT THIS GUARDS: that the vendor is never left blocked and never told
 * something the user did not say. The two failure modes being excluded are an
 * unresolved `canUseTool` promise (which wedges the vendor process outright)
 * and an unvalidated echo (which would have the shim choose an option on the
 * user's behalf — the one outcome a permission gate must never produce).
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import type { PersistEntry } from "../../src/store/persistence.js";
import type { PermissionResultLike, PermissionUpdateLike } from "../../src/sdk/types.js";
import {
  ASK_USER_QUESTION_TOOL,
  DEFAULT_PERMISSION_MODE,
  fromStanding,
  fromVendorPermissionMode,
  PermissionGate,
  toQuestionBatch,
  toStanding,
  toVendorAnswers,
  toVendorPermissionMode,
  validateAnswers,
} from "../../src/engine/permission-gate.js";

const AGENT = create(conversationv1.AgentIdSchema, { value: "agent-1" });

/** The one subagent this session has announced. */
const SUBAGENT = create(conversationv1.AgentIdSchema, { value: "agent-sub" });

function gateWith(
  keepalive: (agentId: conversationv1.AgentId) => boolean = () => false,
): { gate: PermissionGate; written: PersistEntry[]; modes: conversationv1.AgentPermissionMode[] } {
  const written: PersistEntry[] = [];
  const modes: conversationv1.AgentPermissionMode[] = [];
  const gate = new PermissionGate({
    mainAgentId: () => AGENT,
    agentFor: (vendorAgentId) => (vendorAgentId === SUBAGENT.value ? SUBAGENT : undefined),
    persist: (entries) => written.push(...entries),
    keepalive,
    nowMs: () => 1000,
    onPermissionModeSet: (mode) => modes.push(mode),
  });
  return { gate, written, modes };
}

const QUESTION_INPUT = {
  questions: [
    {
      question: "What type of pizza do you want?",
      header: "Pizza",
      multiSelect: false,
      options: [
        { label: "Pepperoni", description: "spicy cured meat" },
        { label: "Margherita", description: "fresh basil" },
      ],
    },
  ],
};

function callOptions(overrides: Record<string, unknown> = {}): Parameters<PermissionGate["canUseTool"]>[2] {
  return {
    signal: new AbortController().signal,
    toolUseID: "toolu_1",
    requestId: "req_1",
    ...overrides,
  };
}

function answers(chosen: string[], freeText?: string): conversationv1.AgentQuestionAnswers {
  return create(conversationv1.AgentQuestionAnswersSchema, {
    answers: [
      create(conversationv1.AgentQuestionSelectionSchema, {
        question: create(conversationv1.AgentQuestionTextSchema, {
          text: "What type of pizza do you want?",
        }),
        chosen: chosen.map((label) =>
          create(conversationv1.AgentQuestionChoiceSchema, {
            label: create(conversationv1.AgentQuestionOptionLabelSchema, { label }),
          }),
        ),
        ...(freeText === undefined
          ? {}
          : { freeText: create(conversationv1.AgentQuestionFreeTextSchema, { text: freeText }) }),
      }),
    ],
  });
}

describe("permission mode, both ways", () => {
  const cases: [conversationv1.AgentPermissionMode["mode"]["case"], string][] = [
    ["default", "default"],
    ["acceptEdits", "acceptEdits"],
    ["bypass", "bypassPermissions"],
    ["plan", "plan"],
    ["dontAsk", "dontAsk"],
    ["auto", "auto"],
  ];
  for (const [arm, vendor] of cases) {
    it(`maps ${String(arm)} to the vendor's ${vendor}`, () => {
      expect(toVendorPermissionMode(fromVendorPermissionMode(vendor as never))).toBe(vendor);
    });
  }

  it("maps the vendor's auto onto the auto arm", () => {
    expect(fromVendorPermissionMode("auto").mode.case).toBe("auto");
  });

  it("maps the auto arm onto the vendor's auto", () => {
    expect(
      toVendorPermissionMode(
        create(conversationv1.AgentPermissionModeSchema, {
          mode: { case: "auto", value: create(conversationv1.AgentPermissionModeAutoSchema, {}) },
        }),
      ),
    ).toBe("auto");
  });

  it("names auto as the mode a session runs under when nothing states one", () => {
    expect(DEFAULT_PERMISSION_MODE).toBe("auto");
  });

  it("refuses a mode with no arm rather than sending a default the caller did not ask for", () => {
    expect(() => toVendorPermissionMode(create(conversationv1.AgentPermissionModeSchema, {}))).toThrow(
      /no arm set/,
    );
  });
});

describe("the standing token", () => {
  const suggestions: PermissionUpdateLike[] = [
    { type: "addRules", destination: "userSettings", behavior: "allow", rules: [{ toolName: "Bash", ruleContent: "git status" }] },
  ];

  it("states the rule typed, so a consumer can SAY what the grant would do", () => {
    const standing = toStanding(suggestions);

    expect(standing.changes[0]?.change.case).toBe("addRules");
  });

  it("round-trips back to the vendor's own vocabulary", () => {
    expect(fromStanding(toStanding(suggestions))).toEqual(suggestions);
  });

  it("round-trips a mode change", () => {
    const update: PermissionUpdateLike[] = [{ type: "setMode", destination: "session", mode: "acceptEdits" }];

    expect(fromStanding(toStanding(update))).toEqual(update);
  });

  it("round-trips added directories", () => {
    const update: PermissionUpdateLike[] = [
      { type: "addDirectories", destination: "localSettings", directories: ["/tmp/x"] },
    ];

    expect(fromStanding(toStanding(update))).toEqual(update);
  });
});

describe("the question batch", () => {
  it("reads the asked question and its header", () => {
    const batch = toQuestionBatch(QUESTION_INPUT);

    expect([batch.questions[0]?.question?.text, batch.questions[0]?.header]).toEqual([
      "What type of pizza do you want?",
      "Pizza",
    ]);
  });

  it("reads a single-select as single-select", () => {
    expect(toQuestionBatch(QUESTION_INPUT).questions[0]?.choices.case).toBe("singleSelect");
  });

  it("reads a multi-select as multi-select", () => {
    const input = { questions: [{ ...QUESTION_INPUT.questions[0], multiSelect: true }] };

    expect(toQuestionBatch(input).questions[0]?.choices.case).toBe("multiSelect");
  });

  it("refuses an input with no questions array rather than asking a question nobody asked", () => {
    expect(() => toQuestionBatch({})).toThrow(/carries no `questions` array/);
  });
});

describe("serializing answers for the vendor", () => {
  it("keys the answer by the question TEXT", () => {
    expect(toVendorAnswers(answers(["Pepperoni"]))).toEqual({
      "What type of pizza do you want?": "Pepperoni",
    });
  });

  it("comma-joins a multi-select", () => {
    expect(toVendorAnswers(answers(["Pepperoni", "Margherita"]))["What type of pizza do you want?"]).toBe(
      "Pepperoni, Margherita",
    );
  });

  it("puts the free text LAST, which is the residue rule's inverse", () => {
    expect(toVendorAnswers(answers(["Pepperoni"], "extra cheese"))["What type of pizza do you want?"]).toBe(
      "Pepperoni, extra cheese",
    );
  });
});

describe("validating an echo", () => {
  const batch = toQuestionBatch(QUESTION_INPUT);

  it("accepts an answer that names an asked question and one of its options", () => {
    expect(validateAnswers(batch, answers(["Pepperoni"]))).toBeUndefined();
  });

  it("refuses an answer to a question that is not open", () => {
    const other = create(conversationv1.AgentQuestionAnswersSchema, {
      answers: [
        create(conversationv1.AgentQuestionSelectionSchema, {
          question: create(conversationv1.AgentQuestionTextSchema, { text: "something else" }),
        }),
      ],
    });

    expect(validateAnswers(batch, other)).toMatch(/is open/);
  });

  it("refuses a label that is not one of the question's options", () => {
    expect(validateAnswers(batch, answers(["Anchovy"]))).toMatch(/is not an option/);
  });

  it("refuses two choices on a single-select", () => {
    expect(validateAnswers(batch, answers(["Pepperoni", "Margherita"]))).toMatch(/single-select/);
  });

  it("accepts a free text with no chosen label, because free text is by definition not an option", () => {
    expect(validateAnswers(batch, answers([], "none of these"))).toBeUndefined();
  });
});

describe("a question through the gate", () => {
  it("writes the start frame under the ask's own tool_use_id", async () => {
    const { gate, written } = gateWith();
    void gate.canUseTool(ASK_USER_QUESTION_TOOL, QUESTION_INPUT, callOptions());
    await Promise.resolve();

    expect(written[0]?.upsertKey).toBe("question:toolu_1");
  });

  it("BLOCKS until it is answered", async () => {
    const { gate } = gateWith();
    let settled = false;
    void gate.canUseTool(ASK_USER_QUESTION_TOOL, QUESTION_INPUT, callOptions()).then(() => {
      settled = true;
    });
    await Promise.resolve();

    expect(settled).toBe(false);
  });

  it("resolves with the answers keyed the vendor's way", async () => {
    const { gate } = gateWith();
    const pending = gate.canUseTool(ASK_USER_QUESTION_TOOL, QUESTION_INPUT, callOptions());
    await Promise.resolve();

    gate.answerQuestion(create(conversationv1.AgentQuestionIdSchema, { value: "toolu_1" }), answers(["Pepperoni"]));

    const result = (await pending) as Extract<PermissionResultLike, { behavior: "allow" }>;
    expect(result.updatedInput?.answers).toEqual({ "What type of pizza do you want?": "Pepperoni" });
  });

  it("settles the unit with the answered outcome", async () => {
    const { gate, written } = gateWith();
    void gate.canUseTool(ASK_USER_QUESTION_TOOL, QUESTION_INPUT, callOptions());
    await Promise.resolve();

    gate.answerQuestion(create(conversationv1.AgentQuestionIdSchema, { value: "toolu_1" }), answers(["Pepperoni"]));

    const settled = written[1]?.item;
    const question =
      settled?.kind === "frame" && settled.frame.result.case === "update"
        ? settled.frame.result.value.update
        : undefined;
    expect(question?.case === "question" && question.value.result.case === "success"
      ? question.value.result.value.outcome.case
      : undefined).toBe("answered");
  });

  it("refuses an answer for a question that is not open", () => {
    const { gate } = gateWith();

    expect(gate.answerQuestion(create(conversationv1.AgentQuestionIdSchema, { value: "nope" }), answers(["Pepperoni"]))).toBe(
      "no_open_ask",
    );
  });

  it("refuses an answer naming a DIFFERENT open question as answer_mismatch", async () => {
    const { gate } = gateWith();
    void gate.canUseTool(ASK_USER_QUESTION_TOOL, QUESTION_INPUT, callOptions());
    await Promise.resolve();

    expect(
      gate.answerQuestion(
        create(conversationv1.AgentQuestionIdSchema, { value: "nobody-asked" }),
        answers(["Pepperoni"]),
      ),
    ).toBe("answer_mismatch");
  });

  it("refuses a mismatched echo rather than guessing", async () => {
    const { gate } = gateWith();
    void gate.canUseTool(ASK_USER_QUESTION_TOOL, QUESTION_INPUT, callOptions());
    await Promise.resolve();

    expect(
      gate.answerQuestion(create(conversationv1.AgentQuestionIdSchema, { value: "toolu_1" }), answers(["Anchovy"])),
    ).toBe("answer_mismatch");
  });

  it("leaves the vendor blocked after a mismatched echo, so the ask can still be answered", async () => {
    const { gate } = gateWith();
    void gate.canUseTool(ASK_USER_QUESTION_TOOL, QUESTION_INPUT, callOptions());
    await Promise.resolve();
    gate.answerQuestion(create(conversationv1.AgentQuestionIdSchema, { value: "toolu_1" }), answers(["Anchovy"]));

    expect(gate.pendingCount).toBe(1);
  });
});

// A MULTI-SELECT ANSWER IS A LIST END TO END. `chosen` is a repeated field, and
// the vendor's one-string-per-question join happens EXACTLY ONCE, in the
// `updatedInput` this gate hands the SDK. The frame this gate settles the ask
// with keeps the list — nothing downstream re-derives it from the joined string,
// which question.proto's retired tag 4 says cannot be split back.
describe("the settled ask keeps the picks as a list", () => {
  const MULTI_INPUT = {
    questions: [
      {
        question: "Which suites should run?",
        header: "Suites",
        multiSelect: true,
        options: [
          { label: "Unit", description: "the vitest suites" },
          { label: "Integration", description: "the shim.v1 suite" },
          { label: "Elisp, batch", description: "the ert suites" },
        ],
      },
    ],
  };

  function multiAnswers(chosen: string[]): conversationv1.AgentQuestionAnswers {
    return create(conversationv1.AgentQuestionAnswersSchema, {
      answers: [
        create(conversationv1.AgentQuestionSelectionSchema, {
          question: create(conversationv1.AgentQuestionTextSchema, { text: "Which suites should run?" }),
          chosen: chosen.map((label) =>
            create(conversationv1.AgentQuestionChoiceSchema, {
              label: create(conversationv1.AgentQuestionOptionLabelSchema, { label }),
            }),
          ),
        }),
      ],
    });
  }

  async function settledChosen(chosen: string[]): Promise<string[]> {
    const { gate, written } = gateWith();
    void gate.canUseTool(ASK_USER_QUESTION_TOOL, MULTI_INPUT, callOptions());
    await Promise.resolve();

    gate.answerQuestion(create(conversationv1.AgentQuestionIdSchema, { value: "toolu_1" }), multiAnswers(chosen));

    const settled = written[1]?.item;
    const update =
      settled?.kind === "frame" && settled.frame.result.case === "update"
        ? settled.frame.result.value.update
        : undefined;
    if (update?.case !== "question" || update.value.result.case !== "success") {
      throw new Error("the ask did not settle on its success arm");
    }
    const outcome = update.value.result.value.outcome;
    if (outcome.case !== "answered") {
      throw new Error("the ask did not settle as answered");
    }
    return outcome.value.answers[0]?.chosen.map((choice) => choice.label?.label ?? "") ?? [];
  }

  it("settles two picks as TWO chosen labels", async () => {
    expect(await settledChosen(["Unit", "Integration"])).toEqual(["Unit", "Integration"]);
  });

  it("settles one pick as one chosen label", async () => {
    expect(await settledChosen(["Unit"])).toEqual(["Unit"]);
  });

  it("keeps a label that CONTAINS a comma as one pick, never split at it", async () => {
    expect(await settledChosen(["Elisp, batch"])).toEqual(["Elisp, batch"]);
  });
});

describe("a permission through the gate", () => {
  it("keys the ask by the GATED call, so consent joins the work it gates", async () => {
    const { gate, written } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions());
    await Promise.resolve();

    expect(written[0]?.upsertKey).toBe("permission:toolu_1");
  });

  it("uses the VENDOR's own prompt wording", async () => {
    const { gate, written } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions({ title: "Run a command", displayName: "Bash", description: "git status" }));
    await Promise.resolve();

    const item = written[0]?.item;
    const update =
      item?.kind === "frame" && item.frame.result.case === "update" ? item.frame.result.value.update : undefined;
    const start =
      update?.case === "permission" && update.value.result.case === "start" ? update.value.result.value : undefined;
    expect([start?.prompt?.title, start?.prompt?.displayName, start?.prompt?.description]).toEqual([
      "Run a command",
      "Bash",
      "git status",
    ]);
  });

  it("states the trigger's blocked path when the vendor named one", async () => {
    const { gate, written } = gateWith();
    void gate.canUseTool("Write", {}, callOptions({ blockedPath: "/etc/passwd" }));
    await Promise.resolve();

    const item = written[0]?.item;
    const update =
      item?.kind === "frame" && item.frame.result.case === "update" ? item.frame.result.value.update : undefined;
    const start =
      update?.case === "permission" && update.value.result.case === "start" ? update.value.result.value : undefined;
    expect(start?.trigger?.blockedPath?.path).toBe("/etc/passwd");
  });

  it("leaves the trigger unset when the vendor stated no trigger facts", async () => {
    const { gate, written } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions());
    await Promise.resolve();

    const item = written[0]?.item;
    const update =
      item?.kind === "frame" && item.frame.result.case === "update" ? item.frame.result.value.update : undefined;
    const start =
      update?.case === "permission" && update.value.result.case === "start" ? update.value.result.value : undefined;
    expect(start?.trigger).toBeUndefined();
  });

  it("allows once without sending the vendor any standing rules", async () => {
    const { gate } = gateWith();
    const pending = gate.canUseTool("Bash", {}, callOptions());
    await Promise.resolve();

    gate.decidePermission(
      create(conversationv1.AgentPermissionDecisionSchema, {
        ask: create(conversationv1.AgentPermissionIdSchema, { value: "toolu_1" }),
        decision: {
          case: "allowed",
          value: create(conversationv1.AgentPermissionAllowedSchema, {
            scope: { case: "once", value: create(conversationv1.AgentPermissionAllowedOnceSchema, {}) },
          }),
        },
      }),
    );

    expect(await pending).toEqual({ behavior: "allow" });
  });

  it("echoes a standing grant back to the vendor as updatedPermissions", async () => {
    const { gate } = gateWith();
    // The ask OFFERS the standing the decision below echoes: a grant is
    // validated against the offer, so an ask with no suggestions can only
    // produce a once-allow.
    const pending = gate.canUseTool("Bash", {}, callOptions({ suggestions: [{ type: "addRules", destination: "session", behavior: "allow", rules: [{ toolName: "Bash" }] }] }));
    await Promise.resolve();

    gate.decidePermission(
      create(conversationv1.AgentPermissionDecisionSchema, {
        ask: create(conversationv1.AgentPermissionIdSchema, { value: "toolu_1" }),
        decision: {
          case: "allowed",
          value: create(conversationv1.AgentPermissionAllowedSchema, {
            scope: {
              case: "standing",
              value: create(conversationv1.AgentPermissionAllowedStandingSchema, {
                standing: toStanding([
                  { type: "addRules", destination: "session", behavior: "allow", rules: [{ toolName: "Bash" }] },
                ]),
              }),
            },
          }),
        },
      }),
    );

    const result = (await pending) as Extract<PermissionResultLike, { behavior: "allow" }>;
    expect(result.updatedPermissions).toEqual([
      { type: "addRules", destination: "session", behavior: "allow", rules: [{ toolName: "Bash" }] },
    ]);
  });

  it("restates a standing grant's mode change to the session", async () => {
    const { gate, modes } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions({ suggestions: [{ type: "setMode", destination: "session", mode: "acceptEdits" }] }));
    await Promise.resolve();

    gate.decidePermission(
      create(conversationv1.AgentPermissionDecisionSchema, {
        ask: create(conversationv1.AgentPermissionIdSchema, { value: "toolu_1" }),
        decision: {
          case: "allowed",
          value: create(conversationv1.AgentPermissionAllowedSchema, {
            scope: {
              case: "standing",
              value: create(conversationv1.AgentPermissionAllowedStandingSchema, {
                standing: toStanding([{ type: "setMode", destination: "session", mode: "acceptEdits" }]),
              }),
            },
          }),
        },
      }),
    );

    expect(modes[0]?.mode.case).toBe("acceptEdits");
  });

  it("refuses a standing grant the ask never offered as answer_mismatch", async () => {
    // THE STANDING IS AN ECHO TOKEN. An ask with no suggestions offered no
    // standing at all, so a grant carrying one is an answer to a question the
    // gate is not holding — and accepting it would install rules the vendor
    // never proposed.
    const { gate, modes } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions());
    await Promise.resolve();

    const outcome = gate.decidePermission(
      create(conversationv1.AgentPermissionDecisionSchema, {
        ask: create(conversationv1.AgentPermissionIdSchema, { value: "toolu_1" }),
        decision: {
          case: "allowed",
          value: create(conversationv1.AgentPermissionAllowedSchema, {
            scope: {
              case: "standing",
              value: create(conversationv1.AgentPermissionAllowedStandingSchema, {
                standing: toStanding([{ type: "setMode", destination: "session", mode: "acceptEdits" }]),
              }),
            },
          }),
        },
      }),
    );

    expect([outcome, modes.length]).toEqual(["answer_mismatch", 0]);
  });

  it("refuses a standing grant ALTERED from the offer as answer_mismatch", async () => {
    // The offer is one add-rule; the grant appends a set_mode. A gate that
    // accepted the difference would let a caller change the session's
    // permission mode through a grant nobody offered.
    const { gate, modes } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions({ suggestions: [{ type: "addRules", destination: "session", behavior: "allow", rules: [{ toolName: "Bash" }] }] }));
    await Promise.resolve();

    const outcome = gate.decidePermission(
      create(conversationv1.AgentPermissionDecisionSchema, {
        ask: create(conversationv1.AgentPermissionIdSchema, { value: "toolu_1" }),
        decision: {
          case: "allowed",
          value: create(conversationv1.AgentPermissionAllowedSchema, {
            scope: {
              case: "standing",
              value: create(conversationv1.AgentPermissionAllowedStandingSchema, {
                standing: toStanding([{ type: "addRules", destination: "session", behavior: "allow", rules: [{ toolName: "Bash" }] }, { type: "setMode", destination: "session", mode: "acceptEdits" }]),
              }),
            },
          }),
        },
      }),
    );

    expect([outcome, modes.length]).toEqual(["answer_mismatch", 0]);
  });

  it("denies with the user's own message", async () => {
    const { gate } = gateWith();
    const pending = gate.canUseTool("Bash", {}, callOptions());
    await Promise.resolve();

    gate.decidePermission(
      create(conversationv1.AgentPermissionDecisionSchema, {
        ask: create(conversationv1.AgentPermissionIdSchema, { value: "toolu_1" }),
        decision: {
          case: "denied",
          value: create(conversationv1.AgentPermissionDeniedByUserSchema, { message: "not that one" }),
        },
      }),
    );

    expect(await pending).toEqual({ behavior: "deny", message: "not that one" });
  });

  it("refuses a decision for a permission that is not open", () => {
    const { gate } = gateWith();

    expect(
      gate.decidePermission(
        create(conversationv1.AgentPermissionDecisionSchema, {
          ask: create(conversationv1.AgentPermissionIdSchema, { value: "nope" }),
          decision: {
            case: "denied",
            value: create(conversationv1.AgentPermissionDeniedByUserSchema, { message: "x" }),
          },
        }),
      ),
    ).toBe("no_open_ask");
  });

  it("refuses a decision naming a DIFFERENT open permission as answer_mismatch", async () => {
    // The two arms are different facts: no_open_ask means there is nothing to
    // answer, answer_mismatch means the daemon should answer the ask in hand.
    const { gate } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions());
    await Promise.resolve();

    expect(
      gate.decidePermission(
        create(conversationv1.AgentPermissionDecisionSchema, {
          ask: create(conversationv1.AgentPermissionIdSchema, { value: "an-id-nobody-asked-under" }),
          decision: {
            case: "denied",
            value: create(conversationv1.AgentPermissionDeniedByUserSchema, { message: "x" }),
          },
        }),
      ),
    ).toBe("answer_mismatch");
  });
});

describe("what the fold is told about an open ask", () => {
  it("names a pending permission", async () => {
    const { gate } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions());
    await Promise.resolve();

    expect(gate.pendingAsk("toolu_1")).toEqual({ kind: "permission" });
  });

  it("names a pending question", async () => {
    const { gate } = gateWith();
    void gate.canUseTool(ASK_USER_QUESTION_TOOL, QUESTION_INPUT, callOptions());
    await Promise.resolve();

    expect(gate.pendingAsk("toolu_1")).toEqual({ kind: "question" });
  });

  it("answers absence for a call with no open ask — the by_policy discriminator", () => {
    expect(gateWith().gate.pendingAsk("toolu_9")).toBeUndefined();
  });
});

describe("standing down", () => {
  it("RESOLVES a pending permission as denied", async () => {
    const { gate } = gateWith();
    const pending = gate.canUseTool("Bash", {}, callOptions());
    await Promise.resolve();

    gate.standDown("the session was killed");

    expect(await pending).toEqual({ behavior: "deny", message: "the session was killed" });
  });

  it("RESOLVES a pending question as denied", async () => {
    const { gate } = gateWith();
    const pending = gate.canUseTool(ASK_USER_QUESTION_TOOL, QUESTION_INPUT, callOptions());
    await Promise.resolve();

    gate.standDown("the session was killed");

    expect(await pending).toEqual({ behavior: "deny", message: "the session was killed" });
  });

  it("settles a stood-down question as UNANSWERED", async () => {
    const { gate, written } = gateWith();
    void gate.canUseTool(ASK_USER_QUESTION_TOOL, QUESTION_INPUT, callOptions());
    await Promise.resolve();

    gate.standDown("x");

    const settled = written[1]?.item;
    const update =
      settled?.kind === "frame" && settled.frame.result.case === "update"
        ? settled.frame.result.value.update
        : undefined;
    expect(update?.case === "question" && update.value.result.case === "success"
      ? update.value.result.value.outcome.case
      : undefined).toBe("unanswered");
  });

  it("reports how many callbacks it unwedged", async () => {
    const { gate } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions());
    void gate.canUseTool("Write", {}, callOptions({ toolUseID: "toolu_2" }));
    await Promise.resolve();

    expect(gate.standDown("x")).toBe(2);
  });

  it("leaves nothing pending", async () => {
    const { gate } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions());
    await Promise.resolve();
    gate.standDown("x");

    expect(gate.pendingCount).toBe(0);
  });
});

/**
 * noteVendorDenial / deniedCall: the memory of a denial the GATE did not
 * itself decide (a vendor `permission_denied` record the fold relays), kept
 * so `deniedCall` answers the same regardless of which half of the shim saw
 * it. Session wiring routes `session.deniedCall` to `gate.deniedCall` and
 * calls `gate.noteVendorDenial` on a relayed denial, but no unit scenario
 * currently exercises that vendor-denial path, so neither method had ever
 * run in the unit suite.
 */
describe("noteVendorDenial and deniedCall", () => {
  it("is not denied before any denial is noted", () => {
    const { gate } = gateWith();

    expect(gate.deniedCall("toolu_1")).toBe(false);
  });

  it("remembers a vendor denial the gate never asked about", () => {
    const { gate } = gateWith();

    gate.noteVendorDenial("toolu_1");

    expect(gate.deniedCall("toolu_1")).toBe(true);
    expect(gate.deniedCall("toolu_2")).toBe(false);
  });

  it("ignores an empty tool_use_id", () => {
    const { gate } = gateWith();

    gate.noteVendorDenial("");

    expect(gate.deniedCall("")).toBe(false);
  });
});

describe("whose book an ask lands on", () => {
  it("writes a permission ask on the SUBAGENT that raised it", async () => {
    // Arrange.
    const { gate, written } = gateWith();

    // Act.
    void gate.canUseTool("Bash", { command: "ls" }, callOptions({ agentID: SUBAGENT.value }));
    await Promise.resolve();

    // Assert.
    expect(written[0]?.agentId.value).toBe(SUBAGENT.value);
  });

  it("writes a question on the SUBAGENT that raised it", async () => {
    // Arrange.
    const { gate, written } = gateWith();

    // Act.
    void gate.canUseTool(
      ASK_USER_QUESTION_TOOL,
      QUESTION_INPUT,
      callOptions({ agentID: SUBAGENT.value }),
    );
    await Promise.resolve();

    // Assert.
    expect(written[0]?.agentId.value).toBe(SUBAGENT.value);
  });

  it("leaves a backgrounded subagent's ask untagged while the main agent's keep-alive runs", async () => {
    // Arrange: the keep-alive is the running vendor turn, and it did not spawn
    // the subagent that asks.
    const { gate, written } = gateWith((agentId) => agentId.value === AGENT.value);

    // Act.
    void gate.canUseTool("Bash", { command: "ls" }, callOptions({ agentID: SUBAGENT.value }));
    await Promise.resolve();

    // Assert.
    expect(written[0]?.keepalive).toBe(false);
  });

  it("tags an ask the keep-alive's own turn raised", async () => {
    // Arrange.
    const { gate, written } = gateWith((agentId) => agentId.value === AGENT.value);

    // Act.
    void gate.canUseTool("Bash", { command: "ls" }, callOptions());
    await Promise.resolve();

    // Assert.
    expect(written[0]?.keepalive).toBe(true);
  });

  it("leaves a main-agent ask on the main agent's book", async () => {
    // Arrange.
    const { gate, written } = gateWith();

    // Act.
    void gate.canUseTool("Bash", { command: "ls" }, callOptions());
    await Promise.resolve();

    // Assert.
    expect(written[0]?.agentId.value).toBe(AGENT.value);
  });

  it("lands an ask under an UNKNOWN agent on the main agent rather than dropping it", async () => {
    // The vendor is blocked on this callback; an ask nobody can see never
    // resolves and wedges the process.
    const { gate, written } = gateWith();

    // Act.
    void gate.canUseTool("Bash", { command: "ls" }, callOptions({ agentID: "agent-nobody" }));
    await Promise.resolve();

    // Assert.
    expect(written[0]?.agentId.value).toBe(AGENT.value);
  });
});

/**
 * The refusal arms and the unlanded oneof paths.
 *
 * Every case below is a REFUSAL or a shape the happy-path scenarios never
 * produce: a vocabulary word the vendor added that this build has no spelling
 * for, an echo that is not the offer, a decision with no arm. Each one asserts
 * the observable outcome — the thrown message, the returned outcome word, or
 * the exact field written — rather than merely reaching the line.
 */
describe("the standing token, on words this build does not know", () => {
  it("marks a destination this build has no spelling for UNSPECIFIED rather than guessing one", () => {
    // Arrange: a destination word from a vendor newer than this build.
    const suggestion = {
      type: "addRules",
      destination: "enterpriseSettings",
      behavior: "allow",
      rules: [{ toolName: "Bash" }],
    } as unknown as PermissionUpdateLike;

    // Act.
    const standing = toStanding([suggestion]);

    // Assert.
    expect(standing.changes[0]?.destination).toBe(conversationv1.AgentPermissionDestination.UNSPECIFIED);
  });

  it("marks a behavior this build has no spelling for UNSPECIFIED rather than guessing one", () => {
    // Arrange.
    const suggestion = {
      type: "addRules",
      destination: "session",
      behavior: "confirm",
      rules: [{ toolName: "Bash" }],
    } as unknown as PermissionUpdateLike;

    // Act.
    const change = toStanding([suggestion]).changes[0]?.change;

    // Assert.
    expect(change?.case === "addRules" ? change.value.behavior : undefined).toBe(
      conversationv1.AgentPermissionBehavior.UNSPECIFIED,
    );
  });

  it("round-trips replaced rules", () => {
    const update: PermissionUpdateLike[] = [
      { type: "replaceRules", destination: "projectSettings", behavior: "deny", rules: [{ toolName: "Write" }] },
    ];

    expect(fromStanding(toStanding(update))).toEqual(update);
  });

  it("round-trips removed rules", () => {
    const update: PermissionUpdateLike[] = [
      { type: "removeRules", destination: "cliArg", behavior: "ask", rules: [{ toolName: "Read" }] },
    ];

    expect(fromStanding(toStanding(update))).toEqual(update);
  });

  it("round-trips removed directories", () => {
    const update: PermissionUpdateLike[] = [
      { type: "removeDirectories", destination: "userSettings", directories: ["/tmp/y"] },
    ];

    expect(fromStanding(toStanding(update))).toEqual(update);
  });
});

describe("reading an echoed standing token back", () => {
  it("refuses a destination with no vendor spelling rather than picking one", () => {
    // Arrange: UNSPECIFIED is exactly what toStanding writes for an unknown word.
    const standing = create(conversationv1.AgentPermissionStandingSchema, {
      changes: [
        create(conversationv1.AgentPermissionChangeSchema, {
          destination: conversationv1.AgentPermissionDestination.UNSPECIFIED,
          change: {
            case: "addRules",
            value: create(conversationv1.AgentPermissionRulesAddedSchema, {}),
          },
        }),
      ],
    });

    // Act + Assert.
    expect(() => fromStanding(standing)).toThrow(/has no vendor spelling/);
  });

  it("falls back to ask for a behavior with no vendor spelling, the most restrictive of the three", () => {
    const standing = create(conversationv1.AgentPermissionStandingSchema, {
      changes: [
        create(conversationv1.AgentPermissionChangeSchema, {
          destination: conversationv1.AgentPermissionDestination.SESSION,
          change: {
            case: "addRules",
            value: create(conversationv1.AgentPermissionRulesAddedSchema, {
              behavior: conversationv1.AgentPermissionBehavior.UNSPECIFIED,
            }),
          },
        }),
      ],
    });

    expect(fromStanding(standing)[0]?.type === "addRules" ? fromStanding(standing)[0] : undefined).toMatchObject({
      behavior: "ask",
    });
  });

  it("refuses a set_mode change that carries no mode", () => {
    const standing = create(conversationv1.AgentPermissionStandingSchema, {
      changes: [
        create(conversationv1.AgentPermissionChangeSchema, {
          destination: conversationv1.AgentPermissionDestination.SESSION,
          change: { case: "setMode", value: create(conversationv1.AgentPermissionModeSetSchema, {}) },
        }),
      ],
    });

    expect(() => fromStanding(standing)).toThrow(/carries no mode/);
  });

  it("refuses a change with no arm at all", () => {
    const standing = create(conversationv1.AgentPermissionStandingSchema, {
      changes: [
        create(conversationv1.AgentPermissionChangeSchema, {
          destination: conversationv1.AgentPermissionDestination.SESSION,
        }),
      ],
    });

    expect(() => fromStanding(standing)).toThrow(/carries no arm/);
  });
});

describe("the question batch, on inputs that do not have the shape", () => {
  it("refuses a question with no header rather than asking a question nobody asked", () => {
    expect(() => toQuestionBatch({ questions: [{ question: "why?" }] })).toThrow(
      /no question text or header/,
    );
  });

  it("refuses an option with no description", () => {
    const input = {
      questions: [{ question: "why?", header: "Why", options: [{ label: "because" }] }],
    };

    expect(() => toQuestionBatch(input)).toThrow(/no label or description/);
  });

  it("carries an option's preview when the vendor stated one", () => {
    const input = {
      questions: [
        {
          question: "why?",
          header: "Why",
          options: [{ label: "because", description: "d", preview: "a diff" }],
        },
      ],
    };

    const choices = toQuestionBatch(input).questions[0]?.choices;

    expect(choices?.case === "singleSelect" ? choices.value.options[0]?.preview : undefined).toBe("a diff");
  });

  it("treats a question with no options array as a question with no options", () => {
    const choices = toQuestionBatch({ questions: [{ question: "why?", header: "Why" }] }).questions[0]?.choices;

    expect(choices?.case === "singleSelect" ? choices.value.options.length : undefined).toBe(0);
  });
});

describe("serializing an answer that names nothing", () => {
  it("refuses an answer whose question carries no text", () => {
    const empty = create(conversationv1.AgentQuestionAnswersSchema, {
      answers: [create(conversationv1.AgentQuestionSelectionSchema, {})],
    });

    expect(() => toVendorAnswers(empty)).toThrow(/names no question/);
  });

  it("serializes a chosen option that carries no label as the empty string", () => {
    const unlabelled = create(conversationv1.AgentQuestionAnswersSchema, {
      answers: [
        create(conversationv1.AgentQuestionSelectionSchema, {
          question: create(conversationv1.AgentQuestionTextSchema, { text: "q" }),
          chosen: [create(conversationv1.AgentQuestionChoiceSchema, {})],
        }),
      ],
    });

    expect(toVendorAnswers(unlabelled)).toEqual({ q: "" });
  });
});

describe("validating an echo, on the shapes the happy path never produces", () => {
  it("accepts two labels on a MULTI-select, which is what multi-select means", () => {
    const batch = toQuestionBatch({ questions: [{ ...QUESTION_INPUT.questions[0], multiSelect: true }] });

    expect(validateAnswers(batch, answers(["Pepperoni", "Margherita"]))).toBeUndefined();
  });

  it("refuses an answer whose question carries no text, reporting the empty text", () => {
    const batch = toQuestionBatch(QUESTION_INPUT);
    const nameless = create(conversationv1.AgentQuestionAnswersSchema, {
      answers: [create(conversationv1.AgentQuestionSelectionSchema, {})],
    });

    expect(validateAnswers(batch, nameless)).toBe('no question with the text "" is open');
  });

  it("refuses every label against a question whose choices arm is unset, because it offers none", () => {
    // Arrange: a batch built by hand — the vendor's own inputs always set an arm.
    const batch = create(conversationv1.AgentQuestionBatchSchema, {
      questions: [
        create(conversationv1.AgentQuestionAskedSchema, {
          question: create(conversationv1.AgentQuestionTextSchema, { text: "q" }),
          header: "Q",
        }),
      ],
    });
    const answer = create(conversationv1.AgentQuestionAnswersSchema, {
      answers: [
        create(conversationv1.AgentQuestionSelectionSchema, {
          question: create(conversationv1.AgentQuestionTextSchema, { text: "q" }),
          chosen: [
            create(conversationv1.AgentQuestionChoiceSchema, {
              label: create(conversationv1.AgentQuestionOptionLabelSchema, { label: "anything" }),
            }),
          ],
        }),
      ],
    });

    expect(validateAnswers(batch, answer)).toBe('"anything" is not an option of "q"');
  });

  it("treats a chosen choice with no label as the empty label, which no option offers", () => {
    const batch = toQuestionBatch(QUESTION_INPUT);
    const answer = create(conversationv1.AgentQuestionAnswersSchema, {
      answers: [
        create(conversationv1.AgentQuestionSelectionSchema, {
          question: create(conversationv1.AgentQuestionTextSchema, {
            text: "What type of pizza do you want?",
          }),
          chosen: [create(conversationv1.AgentQuestionChoiceSchema, {})],
        }),
      ],
    });

    expect(validateAnswers(batch, answer)).toBe(
      '"" is not an option of "What type of pizza do you want?"',
    );
  });

  it("treats an OPTION with no label as offering the empty label, so an empty choice matches it", () => {
    const batch = create(conversationv1.AgentQuestionBatchSchema, {
      questions: [
        create(conversationv1.AgentQuestionAskedSchema, {
          question: create(conversationv1.AgentQuestionTextSchema, { text: "q" }),
          header: "Q",
          choices: {
            case: "singleSelect",
            value: create(conversationv1.AgentQuestionSingleSelectSchema, {
              options: [create(conversationv1.AgentQuestionOptionSchema, {})],
            }),
          },
        }),
      ],
    });
    const answer = create(conversationv1.AgentQuestionAnswersSchema, {
      answers: [
        create(conversationv1.AgentQuestionSelectionSchema, {
          question: create(conversationv1.AgentQuestionTextSchema, { text: "q" }),
          chosen: [create(conversationv1.AgentQuestionChoiceSchema, {})],
        }),
      ],
    });

    expect(validateAnswers(batch, answer)).toBeUndefined();
  });
});

describe("the bound on the denial memory", () => {
  it("forgets the oldest denial once the bound is passed, so it never becomes a second history", () => {
    // Arrange: the bound is 256; note one more than that.
    const { gate } = gateWith();

    // Act.
    for (let index = 0; index <= 256; index += 1) gate.noteVendorDenial(`toolu_${index}`);

    // Assert.
    expect([gate.deniedCall("toolu_0"), gate.deniedCall("toolu_1"), gate.deniedCall("toolu_256")]).toEqual([
      false,
      true,
      true,
    ]);
  });
});

describe("a permission decision the gate cannot apply", () => {
  it("reports no_open_ask for a decision that names no ask at all", () => {
    const { gate } = gateWith();

    expect(
      gate.decidePermission(create(conversationv1.AgentPermissionDecisionSchema, {})),
    ).toBe("no_open_ask");
  });

  it("REFUSES a standing allow that carries no standing", async () => {
    // Arrange.
    const { gate } = gateWith();
    void gate.canUseTool(
      "Bash",
      {},
      callOptions({ suggestions: [{ type: "addRules", destination: "session", behavior: "allow", rules: [{ toolName: "Bash" }] }] }),
    );
    await Promise.resolve();

    // Act.
    const outcome = gate.decidePermission(
      create(conversationv1.AgentPermissionDecisionSchema, {
        ask: create(conversationv1.AgentPermissionIdSchema, { value: "toolu_1" }),
        decision: {
          case: "allowed",
          value: create(conversationv1.AgentPermissionAllowedSchema, {
            scope: {
              case: "standing",
              value: create(conversationv1.AgentPermissionAllowedStandingSchema, {}),
            },
          }),
        },
      }),
    );

    // Assert: the ask is still open, so the vendor is still blocked on a real answer.
    expect([outcome, gate.pendingCount]).toEqual(["answer_mismatch", 1]);
  });

  it("REFUSES a decision with no arm rather than inventing one", async () => {
    const { gate } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions());
    await Promise.resolve();

    const outcome = gate.decidePermission(
      create(conversationv1.AgentPermissionDecisionSchema, {
        ask: create(conversationv1.AgentPermissionIdSchema, { value: "toolu_1" }),
      }),
    );

    expect([outcome, gate.pendingCount]).toEqual(["answer_mismatch", 1]);
  });
});

describe("the trigger facts, one arm at a time", () => {
  async function triggerOf(overrides: Record<string, unknown>): Promise<conversationv1.AgentPermissionTrigger | undefined> {
    const { gate, written } = gateWith();
    void gate.canUseTool("Bash", {}, callOptions(overrides));
    await Promise.resolve();
    const item = written[0]?.item;
    const update =
      item?.kind === "frame" && item.frame.result.case === "update" ? item.frame.result.value.update : undefined;
    return update?.case === "permission" && update.value.result.case === "start"
      ? update.value.result.value.trigger
      : undefined;
  }

  it("states the matched ask rule and its content when the vendor named one", async () => {
    const trigger = await triggerOf({
      matchedAskRule: { source: "projectSettings", toolName: "Bash", ruleContent: "git push" },
    });

    expect([trigger?.askRule?.source, trigger?.askRule?.toolName, trigger?.askRule?.ruleContent]).toEqual([
      "projectSettings",
      "Bash",
      "git push",
    ]);
  });

  it("leaves the rule content unset when the matched rule named none", async () => {
    const trigger = await triggerOf({ matchedAskRule: { source: "cliArg", toolName: "Bash" } });

    expect(trigger?.askRule?.ruleContent).toBeUndefined();
  });

  it("states the vendor's decision reason as the trigger note", async () => {
    const trigger = await triggerOf({ decisionReason: "the classifier could not decide" });

    expect([trigger?.note?.text, trigger?.blockedPath, trigger?.askRule]).toEqual([
      "the classifier could not decide",
      undefined,
      undefined,
    ]);
  });
});

/**
 * The unknown-word fallbacks, on the two rule arms the round-trip tests reach
 * only with words this build already knows. A vendor newer than this build
 * spells a behavior we have never seen, and each arm must degrade the same way:
 * UNSPECIFIED going out, `ask` coming back — never a guessed grant.
 */
describe("rule replacement and removal, on behavior words this build does not know", () => {
  const unknownBehavior = (type: "replaceRules" | "removeRules"): PermissionUpdateLike =>
    ({
      type,
      destination: "session",
      behavior: "confirm",
      rules: [{ toolName: "Bash" }],
    }) as unknown as PermissionUpdateLike;

  const echoed = (
    type: "replaceRules" | "removeRules",
  ): conversationv1.AgentPermissionStanding =>
    create(conversationv1.AgentPermissionStandingSchema, {
      changes: [
        create(conversationv1.AgentPermissionChangeSchema, {
          destination: conversationv1.AgentPermissionDestination.SESSION,
          change:
            type === "replaceRules"
              ? {
                  case: "replaceRules",
                  value: create(conversationv1.AgentPermissionRulesReplacedSchema, {
                    behavior: conversationv1.AgentPermissionBehavior.UNSPECIFIED,
                  }),
                }
              : {
                  case: "removeRules",
                  value: create(conversationv1.AgentPermissionRulesRemovedSchema, {
                    behavior: conversationv1.AgentPermissionBehavior.UNSPECIFIED,
                  }),
                },
        }),
      ],
    });

  it("marks a replacement's unknown behavior UNSPECIFIED rather than guessing one", () => {
    const change = toStanding([unknownBehavior("replaceRules")]).changes[0]?.change;

    expect(change?.case === "replaceRules" ? change.value.behavior : undefined).toBe(
      conversationv1.AgentPermissionBehavior.UNSPECIFIED,
    );
  });

  it("marks a removal's unknown behavior UNSPECIFIED rather than guessing one", () => {
    const change = toStanding([unknownBehavior("removeRules")]).changes[0]?.change;

    expect(change?.case === "removeRules" ? change.value.behavior : undefined).toBe(
      conversationv1.AgentPermissionBehavior.UNSPECIFIED,
    );
  });

  it("reads an unspecified replacement behavior back as ask, the most restrictive of the three", () => {
    expect(fromStanding(echoed("replaceRules"))[0]).toMatchObject({
      type: "replaceRules",
      behavior: "ask",
    });
  });

  it("reads an unspecified removal behavior back as ask, the most restrictive of the three", () => {
    expect(fromStanding(echoed("removeRules"))[0]).toMatchObject({
      type: "removeRules",
      behavior: "ask",
    });
  });
});
