// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { SetEffortResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_effort_pb";
import { AgentEffortLevel } from "../../../proto/gen/ts/conversation/v1/api_pb";
import type { SentenceTable } from "../../src/rpc/refuse.js";
import { sendPick } from "../../src/topbar/pick.js";
import { RecordingSink, appContext, topbarContext } from "./fixtures.js";

const here = path.dirname(fileURLToPath(import.meta.url));

const CAUSES = { noSession: () => "no session" } as unknown as SentenceTable;

/** Send one SetEffort pick against a daemon answering ANSWER. */
async function send(
  answer: () => ReturnType<typeof create<typeof SetEffortResponseSchema>>,
  onRefused?: (arm: string) => void,
  sink = new RecordingSink(),
) {
  const { host, tc } = topbarContext(appContext({ setEffort: answer }, sink));
  const closed = vi.fn();
  tc.reveals.close = closed;
  const wrap = document.createElement("div");
  host.append(wrap);
  const button = document.createElement("button");
  const row = document.createElement("button");
  await sendPick(
    {
      rpc: "SetEffort",
      send: (client) => client.setEffort({ workspace: tc.ctx.workspace, effort: AgentEffortLevel.HIGH }),
      schema: SetEffortResponseSchema,
      causes: CAUSES,
      malformedOperation: "topbar.test-pick",
      unreadableOperation: "topbar.test-malformed-refusal",
      onRefused,
    },
    tc,
    wrap,
    button,
    row,
  );
  return { wrap, button, closed, sink };
}

describe("sendPick", () => {
  it("closes the reveal and draws nothing on success", async () => {
    // Arrange / Act
    const { wrap, closed } = await send(() =>
      create(SetEffortResponseSchema, { result: { case: "success", value: {} } }),
    );
    // Assert
    expect([closed.mock.calls.length, wrap.querySelector(".refusal")]).toEqual([1, null]);
  });

  it("draws the typed refusal at the control and re-enables the button", async () => {
    // Arrange / Act
    const { wrap, button } = await send(() =>
      create(SetEffortResponseSchema, { result: { case: "error", value: { cause: { case: "noSession", value: {} } } } }),
    );
    // Assert
    expect([wrap.querySelector(".refusal")?.getAttribute("data-arm"), button.disabled]).toEqual(["noSession", false]);
  });

  it("hands the refused arm to the selector's own follow-up", async () => {
    // Arrange
    const onRefused = vi.fn();
    // Act
    await send(
      () => create(SetEffortResponseSchema, { result: { case: "error", value: { cause: { case: "noSession", value: {} } } } }),
      onRefused,
    );
    // Assert
    expect(onRefused).toHaveBeenCalledWith("noSession");
  });

  it("states a transport failure at the control", async () => {
    // Arrange / Act
    const { wrap } = await send(() => {
      throw new Error("no route to the daemon");
    });
    // Assert
    expect(wrap.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });

  it("files an answer with no result arm as machinery and draws nothing", async () => {
    // Arrange / Act
    const { wrap, sink } = await send(() => create(SetEffortResponseSchema, {}));
    // Assert
    expect([sink.reported.map((k) => k.kind.case), wrap.querySelector(".refusal")]).toEqual([
      ["frameUndecodable"],
      null,
    ]);
  });
});

describe("the selectors share the one pick path", () => {
  it.each(["src/topbar/model.ts", "src/topbar/effort.ts", "src/topbar/permission-mode.ts"])(
    "%s sends through sendPick and never calls an rpc itself",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", "..", file), "utf8");
      // Act / Assert
      expect([source.includes("sendPick("), source.includes("callUnary(")]).toEqual([true, false]);
    },
  );
});
