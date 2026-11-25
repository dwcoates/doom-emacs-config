// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SetWorkspaceParentErrorSchema,
  SetWorkspaceParentResponseSchema,
  type SetWorkspaceParentRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_parent_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { buildNestRequest, buildUnnestRequest, sendNest, sendUnnest, setWorkspaceParentRefusal } from "../../src/sidebar/nest.js";
import {
  ArrangeSidebarResponseSchema,
  type ArrangeSidebarRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_arrange_sidebar_pb";
import type { ArrangeChange } from "../../src/sidebar/arrange.js";
import { createArrangeHold } from "../../src/sidebar/arrange.js";
import { spyLog } from "../log-spy.js";
import { refusalSentence } from "../../src/rpc/refusal.js";
import { oneofArms } from "../arms.js";
import { appContext, roster, sidebarContext } from "./harness.js";

const CHILD = create(WorkspaceRefSchema, { id: "ws-child", dir: "/w/child" });
const PARENT = create(WorkspaceRefSchema, { id: "ws-parent", dir: "/w/parent" });

/** A context whose SetWorkspaceParent answers RESPONSE and records what it was asked, its hold drawn at revision 6. */
function nestContext(response: Parameters<typeof create<typeof SetWorkspaceParentResponseSchema>>[1]) {
  const asked: SetWorkspaceParentRequest[] = [];
  const sc = sidebarContext(
    appContext({
      setWorkspaceParent: (request) => {
        asked.push(request);
        return create(SetWorkspaceParentResponseSchema, response);
      },
    }),
  );
  const drawn: number[] = [];
  sc.arrange.hold = createArrangeHold((roster) => drawn.push(Number(roster.revision)));
  sc.arrange.hold.offer(roster({ revision: 6n }));
  const host = document.createElement("div");
  host.appendChild(sc.arrange.status);
  return { sc, asked, drawn, host };
}

describe("the nesting request", () => {
  it("names the workspace and its new parent", () => {
    const request = buildNestRequest(CHILD, PARENT);
    expect([request.workspace?.id, request.parent.case === "childOf" ? request.parent.value.parent?.id : null]).toEqual([
      "ws-child",
      "ws-parent",
    ]);
  });

  it("keeps where the workspace lands", () => {
    expect(buildNestRequest(CHILD, PARENT).landingTarget).toBeUndefined();
  });
});

describe("a refused nesting's words", () => {
  const own = oneofArms(SetWorkspaceParentErrorSchema, "cause").filter(
    (arm) => refusalSentence("SetWorkspaceParent", { case: arm, value: { registryDir: "/d", address: "a" } }) === undefined,
  );

  it.each(own)("words the %s arm", (arm) => {
    const value =
      arm === "parentNotOpen" ? { parent: PARENT, state: { case: "backlogged", value: {} } } : arm === "parentRefMismatch" ? { registryDir: "/r" } : {};
    expect(setWorkspaceParentRefusal({ case: arm, value } as never)).not.toBe("");
  });

  it("names the registry's directory for a parent whose directory disagrees", () => {
    expect(setWorkspaceParentRefusal({ case: "parentRefMismatch", value: { registryDir: "/reg/parent" } } as never)).toContain("/reg/parent");
  });

  it("names the backlogged parent", () => {
    expect(
      setWorkspaceParentRefusal({ case: "parentNotOpen", value: { parent: PARENT, state: { case: "backlogged", value: {} } } } as never),
    ).toContain("parent");
  });

  it("refuses an arm it does not know as a breach", () => {
    expect(() => setWorkspaceParentRefusal({ case: "somethingNew", value: {} } as never)).toThrow();
  });
});

describe("sending a nesting", () => {
  it("asks the daemon to make the workspace the parent's child", async () => {
    const { sc, asked } = nestContext({ result: { case: "success", value: { rosterRevision: 7n } } });
    await sendNest(sc, sc.arrange.status, CHILD, PARENT);
    expect(asked.map((r) => [r.workspace?.id, r.parent.case === "childOf" ? r.parent.value.parent?.id : null])).toEqual([["ws-child", "ws-parent"]]);
  });

  it("holds the drawing until the revision the nesting answered", async () => {
    const { sc, drawn } = nestContext({ result: { case: "success", value: { rosterRevision: 7n } } });
    const took = await sendNest(sc, sc.arrange.status, CHILD, PARENT);
    const heldBefore = sc.arrange.hold.holding();
    sc.arrange.hold.offer(roster({ revision: 7n }));
    expect([took, heldBefore, drawn, sc.arrange.hold.holding()]).toEqual([true, true, [6, 7], false]);
  });

  it("records the nesting it sends, naming both workspaces", async () => {
    const info = spyLog("info");
    const { sc } = nestContext({ result: { case: "success", value: { rosterRevision: 7n } } });
    await sendNest(sc, sc.arrange.status, CHILD, PARENT);
    expect(info.mock.calls.find(([, fields]) => fields?.operation === "sidebar.nest.send")?.[1]?.context).toEqual({
      workspace: "ws-child",
      parent: "ws-parent",
    });
    info.mockRestore();
  });

  it("draws a refusal beside the control, lets the hold go and answers false", async () => {
    const { sc, host } = nestContext({ result: { case: "error", value: { cause: { case: "cycle", value: {} } } } });
    const took = await sendNest(sc, sc.arrange.status, CHILD, PARENT);
    expect([took, host.querySelector('.refusal[data-arm="cycle"]') !== null, sc.arrange.hold.holding()]).toEqual([false, true, false]);
  });

  it("records a refusal through the verbs' log, naming both workspaces and the arm", async () => {
    const warn = spyLog("warn");
    const { sc } = nestContext({ result: { case: "error", value: { cause: { case: "cycle", value: {} } } } });
    await sendNest(sc, sc.arrange.status, CHILD, PARENT);
    expect(warn.mock.calls.find(([, fields]) => fields?.operation === "sidebar.verbs.refused")?.[1]?.context).toMatchObject({
      workspace: "ws-child",
      parent: "ws-parent",
      rpc: "SetWorkspaceParent",
      arm: "cycle",
    });
    warn.mockRestore();
  });
});

describe("the top-level request", () => {
  it("names the workspace and the top level", () => {
    const request = buildUnnestRequest(CHILD);
    expect([request.workspace?.id, request.parent.case]).toEqual(["ws-child", "topLevel"]);
  });

  it("keeps where the workspace lands", () => {
    expect(buildUnnestRequest(CHILD).landingTarget).toBeUndefined();
  });
});

describe("sending a nested workspace to the top level", () => {
  /** CHILD placed in no subsection just before workspace ws-b. */
  const PLACEMENT: ArrangeChange = {
    case: "placeWorkspace",
    value: { workspace: CHILD, into: { case: "noSubsection", value: {} }, at: { case: "before", value: { id: "ws-b", dir: "/w/b" } } },
  };

  /** A context answering SetWorkspaceParent with RETARGET and ArrangeSidebar with PLACE, recording every ask in order; its hold drawn at 6. */
  function unnestContext(
    retarget: Parameters<typeof create<typeof SetWorkspaceParentResponseSchema>>[1],
    place: Parameters<typeof create<typeof ArrangeSidebarResponseSchema>>[1],
  ) {
    const asked: (SetWorkspaceParentRequest | ArrangeSidebarRequest)[] = [];
    const sc = sidebarContext(
      appContext({
        setWorkspaceParent: (request) => {
          asked.push(request);
          return create(SetWorkspaceParentResponseSchema, retarget);
        },
        arrangeSidebar: (request) => {
          asked.push(request);
          return create(ArrangeSidebarResponseSchema, place);
        },
      }),
    );
    const drawn: number[] = [];
    sc.arrange.hold = createArrangeHold((roster) => drawn.push(Number(roster.revision)));
    sc.arrange.hold.offer(roster({ revision: 6n }));
    const host = document.createElement("div");
    host.appendChild(sc.arrange.status);
    return { sc, asked, drawn, host };
  }

  /** What each ask of ASKED was, in order. */
  const kinds = (asked: (SetWorkspaceParentRequest | ArrangeSidebarRequest)[]): string[] =>
    asked.map((request) => ("parent" in request ? `parent:${request.parent.case}` : `arrange:${request.change.case}`));

  it("retargets the workspace to the top level, then places it where it was dropped", async () => {
    const { sc, asked } = unnestContext({ result: { case: "success", value: { rosterRevision: 7n } } }, { result: { case: "success", value: { rosterRevision: 8n } } });
    const took = await sendUnnest(sc, sc.arrange.status, CHILD, PLACEMENT);
    expect([took, kinds(asked)]).toEqual([true, ["parent:topLevel", "arrange:placeWorkspace"]]);
  });

  it("holds the drawing until the placement's revision, never drawing the roster between the two", async () => {
    const { sc, drawn } = unnestContext({ result: { case: "success", value: { rosterRevision: 7n } } }, { result: { case: "success", value: { rosterRevision: 8n } } });
    await sendUnnest(sc, sc.arrange.status, CHILD, PLACEMENT);
    sc.arrange.hold.offer(roster({ revision: 7n }));
    const heldAtTheRetarget = sc.arrange.hold.holding();
    sc.arrange.hold.offer(roster({ revision: 8n }));
    expect([heldAtTheRetarget, drawn]).toEqual([true, [6, 8]]);
  });

  it("asks no placement when the retarget is refused, drawing its refusal and letting the hold go", async () => {
    const { sc, asked, host } = unnestContext(
      { result: { case: "error", value: { cause: { case: "coldGate", value: {} } } } },
      { result: { case: "success", value: { rosterRevision: 8n } } },
    );
    const took = await sendUnnest(sc, sc.arrange.status, CHILD, PLACEMENT);
    expect([took, kinds(asked), host.querySelector('.refusal[data-arm="coldGate"]') !== null, sc.arrange.hold.holding()]).toEqual([
      false,
      ["parent:topLevel"],
      true,
      false,
    ]);
  });

  it("draws a refused placement and holds until the retarget's revision, which stands", async () => {
    const { sc, host } = unnestContext(
      { result: { case: "success", value: { rosterRevision: 7n } } },
      { result: { case: "error", value: { cause: { case: "anchorElsewhere", value: {} } } } },
    );
    const took = await sendUnnest(sc, sc.arrange.status, CHILD, PLACEMENT);
    const held = sc.arrange.hold.holding();
    sc.arrange.hold.offer(roster({ revision: 7n }));
    expect([took, host.querySelector('.refusal[data-arm="anchorElsewhere"]') !== null, held, sc.arrange.hold.holding()]).toEqual([
      false,
      true,
      true,
      false,
    ]);
  });

  it("records the move it sends, naming the workspace and where it goes", async () => {
    const info = spyLog("info");
    const { sc } = unnestContext({ result: { case: "success", value: { rosterRevision: 7n } } }, { result: { case: "success", value: { rosterRevision: 8n } } });
    await sendUnnest(sc, sc.arrange.status, CHILD, PLACEMENT);
    expect(info.mock.calls.find(([, fields]) => fields?.operation === "sidebar.nest.unnest")?.[1]?.context).toEqual({
      workspace: "ws-child",
      into: "noSubsection",
      at: "before",
    });
    info.mockRestore();
  });

  it("records a refused retarget through the verbs' log, naming the workspace and the arm", async () => {
    const warn = spyLog("warn");
    const { sc } = unnestContext(
      { result: { case: "error", value: { cause: { case: "coldGate", value: {} } } } },
      { result: { case: "success", value: { rosterRevision: 8n } } },
    );
    await sendUnnest(sc, sc.arrange.status, CHILD, PLACEMENT);
    expect(warn.mock.calls.find(([, fields]) => fields?.operation === "sidebar.verbs.refused")?.[1]?.context).toMatchObject({
      workspace: "ws-child",
      rpc: "SetWorkspaceParent",
      arm: "coldGate",
    });
    warn.mockRestore();
  });

  it.each([
    { name: "another workspace", placement: { ...PLACEMENT, value: { ...PLACEMENT.value, workspace: PARENT } } as ArrangeChange },
    { name: "no placement", placement: { case: "deleteSubsection", value: { subsection: { id: "s" } } } as ArrangeChange },
  ])("refuses a placement of $name, asking nothing", async ({ placement }) => {
    const { sc, asked } = unnestContext({ result: { case: "success", value: { rosterRevision: 7n } } }, { result: { case: "success", value: { rosterRevision: 8n } } });
    await expect(sendUnnest(sc, sc.arrange.status, CHILD, placement)).rejects.toThrow("invariant: a workspace made top-level is placed by a placement of itself");
    expect(asked).toEqual([]);
  });
});
