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
import { WorkspaceRosterSchema, type WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  arrangeSidebarRefusal,
  buildArrangeSidebarRequest,
  createArrangeHold,
  sendArrangement,
  sendHeld,
} from "../../src/sidebar/arrange.js";
import { log } from "../../src/log.js";
import { appContext, sidebarContext } from "./harness.js";

/** A roster carrying nothing but REVISION. */
function at(revision: number): WorkspaceRoster {
  return create(WorkspaceRosterSchema, { revision: BigInt(revision) });
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
    const debug = vi.spyOn(log, "debug");
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
