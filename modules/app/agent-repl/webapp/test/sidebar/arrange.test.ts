// @vitest-environment jsdom
import { readdirSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  ArrangeSidebarResponseSchema,
  type ArrangeSidebarRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_arrange_sidebar_pb";
import type { RosterStreams } from "../../src/sidebar/roster-streams.js";
import {
  arrangeSidebarRefusal,
  buildArrangeSidebarRequest,
  arrangementVerb,
  createArrangeHold,
  heldStep,
  sendArrangement,
  sendHeld,
  sendHeldSteps,
  successRevision,
  type HeldStep,
} from "../../src/sidebar/arrange.js";
import { spyLog } from "../log-spy.js";
import { appContext, roster, sidebarContext } from "./harness.js";
import { codeOf } from "../source-text.js";

/** Every roster stream as of the push at REVISION. */
function at(revision: number): RosterStreams {
  return roster({ revision: BigInt(revision) });
}

describe("the arrangement hold", () => {
  it("lets go at the roster revision an arrangement answered", () => {
    const drawn: number[] = [];
    const hold = createArrangeHold((roster) => drawn.push(Number(roster.revision)));
    hold.offer(at(1));
    hold.await();
    hold.settle(2n);
    hold.offer(at(2));
    expect([drawn, hold.holding()]).toEqual([[1, 2], false]);
  });

  it("records its letting go as the sidebar's arrangement hold", () => {
    const debug = spyLog("debug");
    const hold = createArrangeHold(() => undefined);
    hold.offer(at(1));
    hold.drag();
    hold.cancel();
    expect(debug.mock.calls.some(([, fields]) => fields?.operation === "sidebar.arrange.hold-released")).toBe(true);
    debug.mockRestore();
  });
});

describe("the arrangement request", () => {
  it("carries the change it was built from", () => {
    const request = buildArrangeSidebarRequest({ case: "deleteSubsection", value: { subsection: { id: "sub-1" } } });
    expect([request.change.case, request.change.case === "deleteSubsection" ? request.change.value.subsection?.id : null]).toEqual([
      "deleteSubsection",
      "sub-1",
    ]);
  });

  it.each([
    ["unknownRepository", "repository"],
    ["unknownSubsection", "subsection"],
    ["blankName", "name"],
    ["otherRepository", "own repository"],
    ["anchorElsewhere", "try again"],
    ["noOpenWorkspace", "open workspace"],
    ["mainWorkspaceNotSortable", "main workspace"],
    ["noRepositoryBlock", "try again"],
  ])("says what the %s arm means", (arm, words) => {
    expect(arrangeSidebarRefusal({ case: arm, value: {} } as never)).toContain(words);
  });
});

describe("sending an arrangement", () => {
  it("holds the drawing until the answered revision arrives", async () => {
    let sent: ArrangeSidebarRequest | null = null;
    const sc = sidebarContext(
      appContext({
        arrangeSidebar: (request) => {
          sent = request;
          return create(ArrangeSidebarResponseSchema, { result: { case: "success", value: { rosterRevision: 7n } } });
        },
      }),
    );
    const drawn: number[] = [];
    sc.arrange.hold = createArrangeHold((roster) => drawn.push(Number(roster.revision)));
    sc.arrange.hold.offer(at(6));

    const took = await sendArrangement(sc, sc.arrange.status, { case: "deleteSubsection", value: { subsection: { id: "sub-1" } } });

    expect([took, (sent as ArrangeSidebarRequest | null)?.change.case, sc.arrange.hold.holding()]).toEqual([
      true,
      "deleteSubsection",
      true,
    ]);
    sc.arrange.hold.offer(at(7));
    expect(drawn).toEqual([6, 7]);
  });

  it("draws a refusal beside the control and lets the hold go", async () => {
    const sc = sidebarContext(
      appContext({
        arrangeSidebar: () =>
          create(ArrangeSidebarResponseSchema, {
            result: { case: "error", value: { cause: { case: "blankName", value: {} } } },
          }),
      }),
    );
    const host = document.createElement("div");
    host.appendChild(sc.arrange.status);

    const took = await sendArrangement(sc, sc.arrange.status, {
      case: "renameSubsection",
      value: { subsection: { id: "sub-1" }, name: " " },
    });

    expect([took, host.querySelector('.refusal[data-arm="blankName"]') !== null, sc.arrange.hold.holding()]).toEqual([
      false,
      true,
      false,
    ]);
  });
});

describe("sending a held verb", () => {
  /** A context whose ArrangeSidebar answers RESPONSE, with a recorded hold drawn at revision 6. */
  function heldContext(response: Parameters<typeof create<typeof ArrangeSidebarResponseSchema>>[1]) {
    const sc = sidebarContext(appContext({ arrangeSidebar: () => create(ArrangeSidebarResponseSchema, response) }));
    const drawn: number[] = [];
    sc.arrange.hold = createArrangeHold((roster) => drawn.push(Number(roster.revision)));
    sc.arrange.hold.offer(at(6));
    return { sc, drawn };
  }

  /** The verb sendHeld is handed: ArrangeSidebar, its revision read by REVISION. */
  function verb(revision: () => bigint | null) {
    return {
      rpc: "ArrangeSidebar",
      call: (client: Parameters<Parameters<typeof sendHeld>[2]["call"]>[0]) =>
        client.arrangeSidebar(buildArrangeSidebarRequest({ case: "deleteSubsection", value: { subsection: { id: "sub-1" } } })),
      schema: ArrangeSidebarResponseSchema,
      refusalText: (cause: unknown) => arrangeSidebarRefusal(cause as never),
      revision,
    };
  }

  it("holds the drawing until the revision its success names arrives", async () => {
    const { sc, drawn } = heldContext({ result: { case: "success", value: {} } });

    const took = await sendHeld(sc, sc.arrange.status, verb(() => 8n));

    expect([took, sc.arrange.hold.holding()]).toEqual([true, true]);
    sc.arrange.hold.offer(at(8));
    expect(drawn).toEqual([6, 8]);
  });

  it("lets the hold go at once when its success names no revision", async () => {
    const { sc, drawn } = heldContext({ result: { case: "success", value: {} } });

    const took = await sendHeld(sc, sc.arrange.status, verb(() => null));

    expect([took, sc.arrange.hold.holding(), drawn]).toEqual([true, false, [6, 6]]);
  });

  it("lets the hold go at once on a refusal, never reading a revision", async () => {
    const { sc } = heldContext({ result: { case: "error", value: { cause: { case: "blankName", value: {} } } } });
    const revision = vi.fn(() => 8n);

    const took = await sendHeld(sc, sc.arrange.status, verb(revision));

    expect([took, sc.arrange.hold.holding(), revision.mock.calls.length]).toEqual([false, false, 0]);
  });

  it("is the only place the sidebar awaits its arrangement hold", () => {
    const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/sidebar");
    const awaiting = readdirSync(dir)
      .filter((file) => file.endsWith(".ts"))
      .flatMap((file) => (readFileSync(path.join(dir, file), "utf8").match(/hold\.await\(\)/g) ?? []).map(() => file));
    expect(awaiting).toEqual(["arrange.ts"]);
  });
});

describe("sending held steps", () => {
  /** A context whose hold is recorded, drawn at revision 6, its ArrangeSidebar answering each of RESPONSES in turn. */
  function stepsContext(responses: Parameters<typeof create<typeof ArrangeSidebarResponseSchema>>[1][]) {
    const asked: ArrangeSidebarRequest[] = [];
    const sc = sidebarContext(
      appContext({
        arrangeSidebar: (request) => {
          asked.push(request);
          const response = responses[asked.length - 1];
          if (response === undefined) throw new Error("test: no answer left for this ask");
          return create(ArrangeSidebarResponseSchema, response);
        },
      }),
    );
    const drawn: number[] = [];
    sc.arrange.hold = createArrangeHold((roster) => drawn.push(Number(roster.revision)));
    sc.arrange.hold.offer(at(6));
    const host = document.createElement("div");
    host.appendChild(sc.arrange.status);
    return { sc, asked, drawn, host };
  }

  /** A step deleting subsection ID. */
  const step = (id: string): HeldStep => heldStep(arrangementVerb({ case: "deleteSubsection", value: { subsection: { id } } }));
  const success = (revision: bigint) => ({ result: { case: "success" as const, value: { rosterRevision: revision } } });
  const refusal = { result: { case: "error" as const, value: { cause: { case: "blankName" as const, value: {} } } } };
  const subsectionOf = (request: ArrangeSidebarRequest): string | undefined =>
    request.change.case === "deleteSubsection" ? request.change.value.subsection?.id : undefined;

  it("asks every step in order and holds the drawing until the last step's revision", async () => {
    // Arrange
    const { sc, asked, drawn } = stepsContext([success(7n), success(8n)]);

    // Act
    const took = await sendHeldSteps(sc, sc.arrange.status, [step("one"), step("two")]);
    sc.arrange.hold.offer(at(7));
    const heldAtTheFirst = sc.arrange.hold.holding();
    sc.arrange.hold.offer(at(8));

    // Assert
    expect([took, asked.map(subsectionOf), heldAtTheFirst, drawn, sc.arrange.hold.holding()]).toEqual([
      true,
      ["one", "two"],
      true,
      [6, 8],
      false,
    ]);
  });

  it("never asks a step after one that was refused, and answers false", async () => {
    // Arrange
    const { sc, asked, host } = stepsContext([refusal, success(8n)]);

    // Act
    const took = await sendHeldSteps(sc, sc.arrange.status, [step("one"), step("two")]);

    // Assert
    expect([took, asked.map(subsectionOf), host.querySelector('.refusal[data-arm="blankName"]') !== null, sc.arrange.hold.holding()]).toEqual([
      false,
      ["one"],
      true,
      false,
    ]);
  });

  it("holds until an earlier step's revision when a later step is refused", async () => {
    // Arrange
    const { sc, drawn } = stepsContext([success(7n), refusal]);

    // Act
    const took = await sendHeldSteps(sc, sc.arrange.status, [step("one"), step("two")]);
    const held = sc.arrange.hold.holding();
    sc.arrange.hold.offer(at(7));

    // Assert
    expect([took, held, drawn, sc.arrange.hold.holding()]).toEqual([false, true, [6, 7], false]);
  });

  it("refuses a sequence of no steps, leaving the hold alone", async () => {
    // Arrange
    const { sc } = stepsContext([]);

    // Act / Assert
    await expect(sendHeldSteps(sc, sc.arrange.status, [])).rejects.toThrow("invariant: a held sequence asks at least one step");
    expect(sc.arrange.hold.holding()).toBe(false);
  });

  it("is what sendHeld asks through, so a held verb has one way to fire", () => {
    // Arrange
    const source = codeOf(readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/sidebar/arrange.ts"), "utf8"));

    // Act
    const fired = source.match(/fireVerb\(/g) ?? [];

    // Assert
    expect([fired.length, /return sendHeldSteps\(sc, control, \[heldStep\(verb\)\]\)/.test(source)]).toEqual([1, true]);
  });
});

describe("the arrangement verb", () => {
  it("asks ArrangeSidebar for the change and reads the revision its success names", async () => {
    // Arrange
    const asked: ArrangeSidebarRequest[] = [];
    const sc = sidebarContext(
      appContext({
        arrangeSidebar: (request) => {
          asked.push(request);
          return create(ArrangeSidebarResponseSchema, { result: { case: "success", value: { rosterRevision: 4n } } });
        },
      }),
    );

    // Act
    const outcome = await heldStep(arrangementVerb({ case: "deleteSubsection", value: { subsection: { id: "sub-1" } } })).fire(sc, sc.arrange.status);

    // Assert
    expect([outcome, asked.map((request) => request.change.case)]).toEqual([{ took: true, revision: 4n }, ["deleteSubsection"]]);
  });
});

describe("successRevision", () => {
  it("answers the roster revision a success names", () => {
    expect(successRevision(create(ArrangeSidebarResponseSchema, { result: { case: "success", value: { rosterRevision: 7n } } }))).toBe(7n);
  });

  it("answers null for a refusal", () => {
    expect(
      successRevision(create(ArrangeSidebarResponseSchema, { result: { case: "error", value: { cause: { case: "blankName", value: {} } } } })),
    ).toBeNull();
  });

  it("answers null for a response naming no outcome", () => {
    expect(successRevision(create(ArrangeSidebarResponseSchema, {}))).toBeNull();
  });

  it.each(["arrange.ts", "rename.ts", "nest.ts"])("is how %s reads its held verb's revision", (file) => {
    // Arrange
    const source = codeOf(readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "..", "src", "sidebar", file), "utf8"));

    // Act
    const reads = [source.includes("revision: successRevision"), source.includes(".rosterRevision : null")];

    // Assert
    expect(reads).toEqual([true, file === "arrange.ts"]);
  });
});
