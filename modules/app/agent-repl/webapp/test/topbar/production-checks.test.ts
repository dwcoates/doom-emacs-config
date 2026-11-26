// @vitest-environment jsdom
//
// The production-checks chip. One test per arm of its standing oneof, the
// unset and unnameable arms, the tooltip and the repository it requires, the
// pulse glyph, and the click that pauses or resumes the repository's checks
// through UpdateProductionPause.
import { type Control } from "../../src/control.js";
import { describe, expect, it } from "vitest";
import { create, equals } from "@bufbuild/protobuf";
import {
  TopbarProductionChecksSchema,
  type TopbarProductionChecks,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import {
  UpdateProductionPauseErrorSchema,
  UpdateProductionPauseResponseSchema,
  type UpdateProductionPauseRequest,
  type UpdateProductionPauseResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_production_pause_pb";
import { RepositoryRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { DISC_VIEW_BOX } from "../../src/topbar/disc-glyph.js";
import { PULSE_GLYPH_PATHS, drawTopbarProductionChecks } from "../../src/topbar/production-checks.js";
import { oneofArms } from "../arms.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { RecordingSink, appContext, topbarContext } from "./fixtures.js";

/** The repository every chip carries. */
const REPOSITORY = create(RepositoryRefSchema, { id: "repo-1", dir: "/repos/one" });

/** Draw the chip over a context whose rpcs nothing scripts. */
const draw = (u: TopbarProductionChecks): HTMLElement => drawTopbarProductionChecks(u, topbarContext().tc);

/** A chip with the given standing arm, the repository and a tooltip. */
function chip(standing: TopbarProductionChecks["standing"]): TopbarProductionChecks {
  return { ...create(TopbarProductionChecksSchema, { repository: REPOSITORY, tooltip: { text: "the daemon's words" } }), standing };
}

const running = { case: "running", value: {} } as TopbarProductionChecks["standing"];
const paused = { case: "paused", value: {} } as TopbarProductionChecks["standing"];

describe("drawTopbarProductionChecks", () => {
  it("marks running checks for the green disc", () => {
    expect(draw(chip(running)).getAttribute("data-standing")).toBe("running");
  });

  it("marks paused checks for the white disc", () => {
    expect(draw(chip(paused)).getAttribute("data-standing")).toBe("paused");
  });

  it("refuses a chip whose standing is unset", () => {
    expect(() => draw(chip({ case: undefined }))).toThrow(MalformedView);
  });

  it("refuses a standing arm this bundle cannot name", () => {
    expect(() => draw(chip({ case: "half", value: {} } as never))).toThrow(MalformedView);
  });

  it("carries the daemon's tooltip verbatim", () => {
    expect(draw(chip(running)).title).toBe("the daemon's words");
  });

  it("refuses a chip with no tooltip", () => {
    const bare = { ...create(TopbarProductionChecksSchema, { repository: REPOSITORY }), standing: running };
    expect(() => draw(bare)).toThrow(MalformedView);
  });

  it("refuses a chip with no repository to act on", () => {
    const bare = { ...create(TopbarProductionChecksSchema, { tooltip: { text: "t" } }), standing: running };
    expect(() => draw(bare)).toThrow(MalformedView);
  });

  it("draws the pulse glyph's paths", () => {
    const paths = draw(chip(running)).querySelectorAll("svg.topbar-production-checks-glyph > path");
    expect([...paths].map((p) => p.getAttribute("d"))).toEqual([...PULSE_GLYPH_PATHS]);
  });

  it("draws its disc inside the glyph's own svg, on the shared disc's viewBox", () => {
    const svg = draw(chip(running)).querySelector("svg.topbar-production-checks-glyph");
    expect([svg?.getAttribute("viewBox"), svg?.querySelector("circle.topbar-production-checks-disc") !== null]).toEqual([DISC_VIEW_BOX, true]);
  });

  it("labels the button with what a click does while running", () => {
    expect(draw(chip(running)).querySelector(".topbar-production-checks-button")?.getAttribute("aria-label")).toBe("pause production checks");
  });

  it("labels the button with what a click does while paused", () => {
    expect(draw(chip(paused)).querySelector(".topbar-production-checks-button")?.getAttribute("aria-label")).toBe("resume production checks");
  });
});

/** A success answer. */
const success = (): UpdateProductionPauseResponse =>
  create(UpdateProductionPauseResponseSchema, { result: { case: "success", value: { applied: { case: "paused", value: { droppedSpans: 1 } } } } });

/** An error answer naming CAUSE. */
const refused = (cause: { case: string; value: unknown }): UpdateProductionPauseResponse =>
  create(UpdateProductionPauseResponseSchema, { result: { case: "error", value: { cause: cause as never } } });

/** Settle every microtask and the router transport's own hop. */
const settle = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

/**
 * Mount a chip drawn STANDING over a context whose UpdateProductionPause is
 * ANSWER, click it, and let the answer land.
 */
async function clickWith(
  answer: (req: UpdateProductionPauseRequest) => UpdateProductionPauseResponse,
  standing = running,
  failures = new RecordingSink(),
): Promise<{ host: HTMLElement; button: Control; failures: RecordingSink }> {
  const { host, tc } = topbarContext(appContext({ updateProductionPause: answer }, failures));
  host.append(drawTopbarProductionChecks(chip(standing), tc));
  const button = host.querySelector<Control>(".topbar-production-checks-button")!;
  button.click();
  await settle();
  return { host, button, failures };
}

describe("clicking the production-checks chip", () => {
  it("pauses running checks", async () => {
    let sent: string | undefined;
    await clickWith((req) => {
      sent = req.action.case;
      return success();
    });
    expect(sent).toBe("pause");
  });

  it("resumes paused checks", async () => {
    let sent: string | undefined;
    await clickWith((req) => {
      sent = req.action.case;
      return success();
    }, paused);
    expect(sent).toBe("resume");
  });

  it("sends the repository the chip carries", async () => {
    let sent: UpdateProductionPauseRequest["repository"];
    await clickWith((req) => {
      sent = req.repository;
      return success();
    });
    expect(sent !== undefined && equals(RepositoryRefSchema, sent, REPOSITORY)).toBe(true);
  });

  it("logs the click at info with the standing it showed and the action it sent", async () => {
    const capture = captureLogRecords();
    await clickWith(() => success());
    const record = await forwardedRecord(capture, "topbar.production-checks-switch");
    expect([record.level.case, record.context]).toEqual(["info", expect.objectContaining({ drawn: "running", action: "pause" })]);
  });

  it("logs what the daemon applied at info", async () => {
    const capture = captureLogRecords();
    await clickWith(() => success());
    const record = await forwardedRecord(capture, "topbar.production-checks-switched");
    expect([record.level.case, record.context]).toEqual(["info", expect.objectContaining({ applied: "paused" })]);
  });

  it("draws nothing on success, because the new standing arrives on the push", async () => {
    const { host } = await clickWith(() => success());
    expect(host.querySelector(".refusal")).toBeNull();
  });

  it("reports a success naming nothing applied as an unreadable frame", async () => {
    const { failures } = await clickWith(() =>
      create(UpdateProductionPauseResponseSchema, { result: { case: "success", value: {} } }),
    );
    expect(failures.reported).toHaveLength(1);
  });

  const sentences: Readonly<Record<string, string>> = {
    alreadyPaused: "the production checks are already paused",
    notPaused: "the production checks are not paused",
    unknownRepository: "the daemon holds no such repository",
    checksOff: "this repository turns no production checks on",
  };

  for (const arm of oneofArms(UpdateProductionPauseErrorSchema, "cause")) {
    it(`states and words the ${arm} refusal at the chip`, async () => {
      const { host } = await clickWith(() => refused({ case: arm, value: {} }));
      const refusal = host.querySelector(".topbar-production-checks > .refusal");
      expect([refusal?.getAttribute("data-arm"), refusal?.textContent]).toEqual([arm, sentences[arm]]);
    });
  }

  it("states a transport failure at the chip when the call never reached the daemon", async () => {
    const { host } = await clickWith(() => {
      throw new Error("no route to the daemon");
    });
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("failed");
  });

  it("gives the button back once the call answered", async () => {
    const { button } = await clickWith(() => refused({ case: "notPaused", value: {} }));
    expect(button.disabled).toBe(false);
  });
});
