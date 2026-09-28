/**
 * The ask suites' shared fixtures: a scripted `AgentRepl` answering the three
 * ANSWER verbs, and a row context addressed to one card's row.
 *
 * NOT A SUITE — imported by the ask suites, never run on its own. It exists so
 * every ask suite scripts its verb the same way and so the row context (whose
 * `row.id` is what every one of these verbs echoes) is built once.
 */
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  AnswerPermissionResponseSchema,
  type AnswerPermissionRequest,
  type AnswerPermissionResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_permission_pb";
import {
  AnswerQuestionResponseSchema,
  type AnswerQuestionRequest,
  type AnswerQuestionResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_question_pb";
import {
  AnswerColdGateResponseSchema,
  type AnswerColdGateRequest,
  type AnswerColdGateResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_cold_gate_pb";
import {
  FeedIdSchema,
  FeedRowSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import type { FailureSink } from "../../../src/failure/sink.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { testAppContext } from "../../rpc/app-context.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { orderFor } from "../../feed-order.js";

export const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
export const ROW_ID = "ask-1";

const SINK: FailureSink = { report: () => {}, retract: () => {} };

/** The error a scripted failure throws: coded when the script names a code. */
function scriptedFailure(script: AskScript): Error {
  if (script.failCode === undefined) return new Error("no route to daemon");
  return new ConnectError(script.failMessage ?? "no route to daemon", script.failCode);
}

/** What the scripted daemon answers, and what it was asked. */
export interface AskScript {
  permission?: AnswerPermissionResponse;
  question?: AnswerQuestionResponse;
  coldGate?: AnswerColdGateResponse;
  /** Throw instead of answering, standing in for a call that did not land. */
  fail?: boolean;
  /**
   * The code the scripted throw carries.
   *
   * A plain `Error` becomes `internal` — a daemon that ANSWERED with a failure
   * — which is not the same condition as a daemon that could not be reached,
   * and the two are drawn differently.
   */
  failCode?: Code;
  /** What the coded throw says; the daemon's own sentence reaches the client. */
  failMessage?: string;
}

export interface AskCalls {
  permission: AnswerPermissionRequest[];
  question: AnswerQuestionRequest[];
  coldGate: AnswerColdGateRequest[];
}

export interface AskHarness {
  rc: RowContext;
  calls: AskCalls;
}

/** A row context on a card row, with the three ANSWER verbs scripted. */
export function askHarness(script: AskScript = {}, previous?: HTMLElement): AskHarness {
  const calls: AskCalls = { permission: [], question: [], coldGate: [] };
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      answerPermission: (req) => {
        calls.permission.push(req);
        if (script.fail === true) throw scriptedFailure(script);
        return (
          script.permission ??
          create(AnswerPermissionResponseSchema, { result: { case: "success", value: {} } })
        );
      },
      answerQuestion: (req) => {
        calls.question.push(req);
        if (script.fail === true) throw scriptedFailure(script);
        return (
          script.question ??
          create(AnswerQuestionResponseSchema, { result: { case: "success", value: {} } })
        );
      },
      answerColdGate: (req) => {
        calls.coldGate.push(req);
        if (script.fail === true) throw scriptedFailure(script);
        return (
          script.coldGate ??
          create(AnswerColdGateResponseSchema, { result: { case: "success", value: {} } })
        );
      },
    });
  });
  return {
    calls,
    rc: {
      ctx: testAppContext({
        client: createAgentReplClient(transport),
        workspace: WORKSPACE,
        ticker: createTicker(1000),
        failures: SINK,
        composerEnabled: false,
      }),
      feed: "root",
      row: create(FeedRowSchema, { id: create(FeedIdSchema, { value: ROW_ID }), order: orderFor(ROW_ID) }),
      revealRow: async () => false,
      previous,
    },
  };
}

/** Let a scripted answer settle; the router hands it back on a timer. */
export async function settle(advance: (ms: number) => Promise<unknown>): Promise<void> {
  for (let i = 0; i < 30; i += 1) await advance(0);
}
