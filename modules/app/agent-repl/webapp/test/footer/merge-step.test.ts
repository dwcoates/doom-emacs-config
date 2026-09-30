// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { FooterStatusActivityMergeStepSchema } from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  SUITE_EDGE_GLYPHS,
  drawFooterStatusActivityMergeStep,
} from "../../src/footer/merge-step.js";
import { oneofArms } from "../arms.js";

const PATH = "FooterStatusMergingSalient.kind.mergeStep";

/** Draw the merge step line INIT describes. */
function draw(init: MessageInitShape<typeof FooterStatusActivityMergeStepSchema>): HTMLElement {
  return drawFooterStatusActivityMergeStep(create(FooterStatusActivityMergeStepSchema, init), PATH);
}

/** A step of CASE carrying VALUE, built by name for the arm tables. */
function step(stepCase: string, value: Record<string, unknown>): HTMLElement {
  return draw({ step: { case: stepCase, value } } as never);
}

/** A legal value for every arm, so the table below draws each one. */
const ARM_VALUES: Readonly<Record<string, Record<string, unknown>>> = {
  enqueued: { workspaceName: "fix-reconnect", step: "testing" },
  preprocessing: { text: "update the changelog" },
  rebasing: { line: { case: "running", value: { text: "git rebase --continue" } } },
  conflictResolution: { commitSubject: "fix the reconnect loop", files: 3 },
  testing: { name: "webapp", edge: { case: "started", value: {} } },
  fixing: { suites: ["webapp"] },
  committing: { subject: "Merge branch 'fix-reconnect'" },
  updatingMain: { step: { case: "fetching", value: {} } },
  postprocessing: { text: "deploy it" },
};

describe("drawFooterStatusActivityMergeStep: every arm", () => {
  it("has a value for every arm the contract declares", () => {
    expect(Object.keys(ARM_VALUES).sort()).toEqual(oneofArms(FooterStatusActivityMergeStepSchema, "step").sort());
  });

  it.each(Object.entries(ARM_VALUES))("stamps the %s arm as data-step", (arm, value) => {
    expect(step(arm, value).getAttribute("data-step")).toBe(arm);
  });

  it("refuses a step with no arm", () => {
    expect(() => draw({})).toThrow(MalformedView);
  });
});

describe("the enqueued line", () => {
  it("names the merge being worked on and its step", () => {
    expect(step("enqueued", ARM_VALUES.enqueued).textContent).toBe("fix-reconnect: testing");
  });
});

describe("the prompt lines", () => {
  it("draws the before-merge prompt's text verbatim", () => {
    expect(step("preprocessing", { text: "update the changelog" }).textContent).toBe("update the changelog");
  });

  it("draws the after-merge prompt's text verbatim", () => {
    expect(step("postprocessing", { text: "deploy it" }).textContent).toBe("deploy it");
  });
});

describe("the rebasing line", () => {
  it("draws the running command verbatim", () => {
    const el = step("rebasing", { line: { case: "running", value: { text: "git rebase --continue" } } });
    expect(el.textContent).toBe("git rebase --continue");
  });

  it("marks a running command's line", () => {
    const el = step("rebasing", { line: { case: "running", value: { text: "x" } } });
    expect(el.getAttribute("data-line")).toBe("running");
  });

  it("draws the failure verbatim", () => {
    const el = step("rebasing", { line: { case: "failed", value: { text: "error: could not apply 4f2a1c" } } });
    expect(el.textContent).toBe("error: could not apply 4f2a1c");
  });

  it("marks a failure's line", () => {
    const el = step("rebasing", { line: { case: "failed", value: { text: "x" } } });
    expect(el.getAttribute("data-line")).toBe("failed");
  });

  it("refuses a rebasing line with no arm", () => {
    expect(() => step("rebasing", {})).toThrow(MalformedView);
  });
});

describe("the conflict resolution line", () => {
  it("names the commit and how many files conflict", () => {
    expect(step("conflictResolution", ARM_VALUES.conflictResolution).textContent).toBe(
      "fix the reconnect loop: 3 files",
    );
  });

  it("says one file in the singular", () => {
    expect(step("conflictResolution", { commitSubject: "tidy", files: 1 }).textContent).toBe("tidy: 1 file");
  });

  it("colours the file count as a count", () => {
    const el = step("conflictResolution", ARM_VALUES.conflictResolution);
    expect(el.querySelector('[data-datum="count"]')?.textContent).toBe("3");
  });
});

describe("the testing line", () => {
  it("draws a suite starting as '▶ name'", () => {
    expect(step("testing", { name: "webapp", edge: { case: "started", value: {} } }).textContent).toBe(
      `${SUITE_EDGE_GLYPHS.started} webapp`,
    );
  });

  it("draws a suite passing as '✓ name'", () => {
    expect(step("testing", { name: "webapp", edge: { case: "passed", value: {} } }).textContent).toBe(
      `${SUITE_EDGE_GLYPHS.passed} webapp`,
    );
  });

  it("paints a passed suite green", () => {
    const el = step("testing", { name: "webapp", edge: { case: "passed", value: {} } });
    expect(el.classList.contains("tone-green")).toBe(true);
  });

  it("draws a suite failing as '✗ name'", () => {
    expect(step("testing", { name: "webapp", edge: { case: "failed", value: {} } }).textContent).toBe(
      `${SUITE_EDGE_GLYPHS.failed} webapp`,
    );
  });

  it("paints a failed suite red", () => {
    const el = step("testing", { name: "webapp", edge: { case: "failed", value: {} } });
    expect(el.classList.contains("tone-red")).toBe(true);
  });

  it("leaves a starting suite unpainted", () => {
    const el = step("testing", { name: "webapp", edge: { case: "started", value: {} } });
    expect([el.classList.contains("tone-green"), el.classList.contains("tone-red")]).toEqual([false, false]);
  });

  it("stamps the edge", () => {
    const el = step("testing", { name: "webapp", edge: { case: "failed", value: {} } });
    expect(el.getAttribute("data-edge")).toBe("failed");
  });

  it("refuses a suite line with no edge", () => {
    expect(() => step("testing", { name: "webapp" })).toThrow(MalformedView);
  });
});

describe("the fixing line", () => {
  it("names the suites being fixed, in the gate's order", () => {
    expect(step("fixing", { suites: ["daemon unit", "webapp"] }).textContent).toBe("daemon unit, webapp");
  });
});

describe("the committing line", () => {
  it("draws the merge commit's first line verbatim", () => {
    expect(step("committing", ARM_VALUES.committing).textContent).toBe("Merge branch 'fix-reconnect'");
  });
});

describe("the updating main line", () => {
  it("says it is fetching", () => {
    expect(step("updatingMain", { step: { case: "fetching", value: {} } }).textContent).toBe("fetching");
  });

  it("says which commit it is fast-forwarding to", () => {
    const el = step("updatingMain", { step: { case: "fastForwarding", value: { commit: "4f2a1c" } } });
    expect(el.textContent).toBe("fast-forwarding to 4f2a1c");
  });

  it("colours the commit as a sha", () => {
    const el = step("updatingMain", { step: { case: "fastForwarding", value: { commit: "4f2a1c" } } });
    expect(el.querySelector('[data-datum="sha"]')?.textContent).toBe("4f2a1c");
  });

  it("refuses an update with no step", () => {
    expect(() => step("updatingMain", {})).toThrow(MalformedView);
  });
});
