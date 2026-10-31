// @vitest-environment jsdom
// The webhost's load button for a held-back page. See src/host/load-button.ts.
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { ConnectError, Code, createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  LoadPageInEditorResponseSchema,
  type LoadPageInEditorRequest,
  type LoadPageInEditorResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_load_page_in_editor_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../src/clock.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { CONTROL_SELECTOR, type Control } from "../../src/control.js";
import {
  LOAD_BUTTON_LABEL,
  WEBHOST_LOAD_ID,
  createWebhostLoadButton,
  type WebhostLoadButton,
} from "../../src/host/load-button.js";
import { captureLogRecords, forwardedRecords, recordFor } from "../log-capture.js";
import { testAppContext } from "../rpc/app-context.js";

let el: HTMLElement;
let load: WebhostLoadButton;
let asked: LoadPageInEditorRequest[];
/** What the daemon answers the next call: a response, or an error it throws. */
let answer: () => LoadPageInEditorResponse;

const SUCCESS = (): LoadPageInEditorResponse =>
  create(LoadPageInEditorResponseSchema, { result: { case: "success", value: {} } });

beforeEach(() => {
  el = document.createElement("div");
  document.body.replaceChildren(el);
  asked = [];
  answer = SUCCESS;
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      loadPageInEditor: (req) => {
        asked.push(req);
        return answer();
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    ticker: createTicker(),
    failures: { report: () => {}, retract: () => {} },
    workspace: create(WorkspaceRefSchema, { id: "webhost", dir: "" }),
    composerEnabled: false,
  });
  load = createWebhostLoadButton({ el, ctx });
});

afterEach(() => {
  load.dispose();
});

/** The button drawn. */
function button(): Control {
  const found = el.querySelector<Control>(CONTROL_SELECTOR);
  if (found === null) throw new Error("no load button");
  return found;
}

/** Let the click's call settle. */
async function settled(): Promise<void> {
  for (let i = 0; i < 10; i += 1) await Promise.resolve();
  await new Promise((resolve) => setTimeout(resolve, 0));
}

describe("the webhost's load button", () => {
  it("is the load element, hidden until a held-back page is shown", () => {
    expect([el.id, el.hidden]).toEqual([WEBHOST_LOAD_ID, true]);
  });

  it("stands while a held-back page is shown", () => {
    // Act
    load.setShown({ id: "a", dir: "/ws/a" });

    // Assert
    expect([el.hidden, button().textContent, button().disabled]).toEqual([false, LOAD_BUTTON_LABEL, false]);
  });

  it("hides once no held-back page is shown", () => {
    // Arrange
    load.setShown({ id: "a", dir: "/ws/a" });

    // Act
    load.setShown(null);

    // Assert
    expect(el.hidden).toBe(true);
  });

  it("asks the daemon for the shown page on a click", async () => {
    // Arrange
    load.setShown({ id: "a", dir: "/ws/a" });

    // Act
    button().click();
    await settled();

    // Assert
    expect(asked.map((r) => [r.workspace?.id, r.workspace?.dir])).toEqual([["a", "/ws/a"]]);
  });

  it("stays disabled after a relayed click, until the page takes its place", async () => {
    // Arrange
    load.setShown({ id: "a", dir: "/ws/a" });

    // Act
    button().click();
    await settled();

    // Assert
    expect(button().disabled).toBe(true);
  });

  it("asks once for a second click while the first is in flight", async () => {
    // Arrange
    load.setShown({ id: "a", dir: "/ws/a" });

    // Act
    button().click();
    button().click();
    await settled();

    // Assert
    expect(asked).toHaveLength(1);
  });

  it("is given back, and records the refusal at warn, when the daemon refuses", async () => {
    // Arrange
    const capture = captureLogRecords();
    answer = () =>
      create(LoadPageInEditorResponseSchema, {
        result: { case: "error", value: { cause: { case: "noEditor", value: {} } } },
      });
    load.setShown({ id: "a", dir: "/ws/a" });

    // Act
    button().click();
    await settled();

    // Assert
    const record = recordFor(await forwardedRecords(capture), "webhost.load-button.refused");
    expect([button().disabled, record.level.case, record.context?.outcome]).toEqual([false, "warn", "noEditor"]);
  });

  it("is given back, and records the failure at warn, when the call fails", async () => {
    // Arrange
    const capture = captureLogRecords();
    answer = () => {
      throw new ConnectError("down", Code.Unavailable);
    };
    load.setShown({ id: "a", dir: "/ws/a" });

    // Act
    button().click();
    await settled();

    // Assert
    const record = recordFor(await forwardedRecords(capture), "webhost.load-button.refused");
    expect([button().disabled, record.level.case]).toEqual([false, "warn"]);
  });

  it("records its drawing at info", async () => {
    // Arrange
    const capture = captureLogRecords();

    // Act
    load.setShown({ id: "a", dir: "/ws/a" });

    // Assert
    const record = recordFor(await forwardedRecords(capture), "webhost.load-button.drawn");
    expect(record.level.case).toBe("info");
  });

  it("asks nothing once disposed", async () => {
    // Arrange
    load.setShown({ id: "a", dir: "/ws/a" });
    load.dispose();

    // Act
    button().click();
    await settled();

    // Assert
    expect([asked, el.hidden]).toEqual([[], true]);
  });
});
