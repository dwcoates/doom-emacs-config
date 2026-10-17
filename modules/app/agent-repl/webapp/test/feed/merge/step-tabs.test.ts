// @vitest-environment jsdom
import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  OpenInEditorResponseSchema,
  type OpenInEditorRequest,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_open_in_editor_pb";
import {
  OpenWorkspaceResponseSchema,
  type OpenWorkspaceRequest,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_open_workspace_pb";
import {
  SelectWorkspaceResponseSchema,
  type SelectWorkspaceRequest,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { scriptedAppContext } from "../../rpc/app-context.js";
import { RecordingSink, WORKSPACE, harness, mergeRow, rowContext } from "../harness.js";
import { cascadedValue, installStylesheet } from "../../stylesheet.js";
import {
  FeedMergeRebaseLineSchema,
  FeedMergeRebaseReplayingSchema,
  FeedMergeTabCommittingSchema,
  FeedMergeTabRebaseCommitSchema,
  FeedMergeTabRebasingSchema,
  FeedMergeTabTestsRunFixingSchema,
  FeedMergeTabUpdatingMainSchema,
  FeedMergeUpdatingMainStepSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  drawFeedMergeConflictFiles,
  drawFeedMergeTabCommitting,
  drawFeedMergeTabFixingAttempt,
  drawFeedMergeTabRebaseCommit,
  drawFeedMergeTabRebasing,
  drawFeedMergeTabUpdatingMain,
  drawFeedMergeUpdatingMainStep,
} from "../../../src/feed/merge/step-tabs.js";
import { oneofArms } from "../../arms.js";

const LIVE = { case: "live" as const, value: {} };

/** A narration sentence. */
const text = (value: string) => create(FeedMergeRebaseLineSchema, { line: { case: "text", value } });

/** The tip every replay header in this file stands on, in full. */
const TIP = "4f2a1c0123456789abcdef";

/** A replay header onto master, whose checkout is the workspace ws-main. */
const REPLAYING = create(FeedMergeRebaseReplayingSchema, {
  commits: 5,
  branch: "ABC/fix",
  target: { branch: "master", repositoryDir: "/r/main", workspace: { id: "ws-main", dir: "/r/main" } },
  tip: TIP,
  tipShortSha: "4f2a1c012345",
});

/** A row context with no scripted daemon, for draws that click nothing. */
const rc = () => rowContext(harness().ctx, mergeRow("m1"));

describe("drawFeedMergeTabRebasing", () => {
  it("draws the progress as the daemon's two figures", () => {
    const u = create(FeedMergeTabRebasingSchema, { state: LIVE, progress: { replayed: 3, total: 7 } });
    expect(drawFeedMergeTabRebasing(u, "rebasing", rc()).querySelector("[data-merge-progress]")?.textContent).toBe("3/7");
  });

  it("draws each narration line verbatim, oldest first", () => {
    const u = create(FeedMergeTabRebasingSchema, {
      state: LIVE,
      progress: { replayed: 2, total: 2 },
      lines: [text("replaying 1/2 · tidy"), text("replaying 2/2 · fix")],
    });
    const el = drawFeedMergeTabRebasing(u, "rebasing", rc());
    expect([...el.querySelectorAll(".merge-line")].map((line) => line.textContent)).toEqual([
      "replaying 1/2 · tidy",
      "replaying 2/2 · fix",
    ]);
  });

  it("delimits the narration through the ONE shared list rule", () => {
    const u = create(FeedMergeTabRebasingSchema, { state: LIVE, progress: { replayed: 0, total: 1 } });
    expect(drawFeedMergeTabRebasing(u, "rebasing", rc()).querySelector(".merge-lines")?.classList.contains("list-rows")).toBe(
      true,
    );
  });

  it("does not count the narration lines for its figure", () => {
    const u = create(FeedMergeTabRebasingSchema, {
      state: LIVE,
      progress: { replayed: 1, total: 5 },
      lines: [text("a"), text("b"), text("c")],
    });
    expect(drawFeedMergeTabRebasing(u, "rebasing", rc()).querySelector("[data-merge-progress]")?.textContent).toBe("1/5");
  });

  it("refuses a rebasing tab with no progress", () => {
    const u = create(FeedMergeTabRebasingSchema, { state: LIVE });
    expect(() => drawFeedMergeTabRebasing(u, "rebasing", rc())).toThrow(MalformedView);
  });
});

describe("drawFeedMergeTabCommitting", () => {
  it("draws the merge commit's subject verbatim", () => {
    const u = create(FeedMergeTabCommittingSchema, { state: LIVE, subject: { text: "Merge branch 'fix'" } });
    expect(drawFeedMergeTabCommitting(u, "committing").textContent).toBe("Merge branch 'fix'");
  });

  it("refuses a committing tab with no subject", () => {
    const u = create(FeedMergeTabCommittingSchema, { state: LIVE });
    expect(() => drawFeedMergeTabCommitting(u, "committing")).toThrow(MalformedView);
  });
});

describe("drawFeedMergeTabUpdatingMain", () => {
  it("draws every step the contract declares", () => {
    expect([...oneofArms(FeedMergeUpdatingMainStepSchema, "step")].sort()).toEqual(["fastForwarding", "fetching"]);
  });

  it("says it is fetching", () => {
    const u = create(FeedMergeTabUpdatingMainSchema, { state: LIVE, step: { step: { case: "fetching", value: {} } } });
    expect(drawFeedMergeTabUpdatingMain(u, "updating_main").textContent).toBe("fetching");
  });

  it("says which commit it is fast-forwarding to", () => {
    const u = create(FeedMergeTabUpdatingMainSchema, {
      state: LIVE,
      step: { step: { case: "fastForwarding", value: { commit: "4f2a1c" } } },
    });
    expect(drawFeedMergeTabUpdatingMain(u, "updating_main").textContent).toBe("fast-forwarding to 4f2a1c");
  });

  it("stamps the step's arm", () => {
    const u = create(FeedMergeTabUpdatingMainSchema, {
      state: LIVE,
      step: { step: { case: "fastForwarding", value: { commit: "4f2a1c" } } },
    });
    expect(drawFeedMergeTabUpdatingMain(u, "updating_main").getAttribute("data-update-step")).toBe("fastForwarding");
  });

  it("refuses an updating main tab with no step", () => {
    const u = create(FeedMergeTabUpdatingMainSchema, { state: LIVE });
    expect(() => drawFeedMergeTabUpdatingMain(u, "updating_main")).toThrow(MalformedView);
  });

  it("refuses a step that names no arm", () => {
    expect(() => drawFeedMergeUpdatingMainStep(create(FeedMergeUpdatingMainStepSchema, {}), "step")).toThrow(
      MalformedView,
    );
  });

  it("refuses a step arm a newer daemon set, rather than drawing a default", () => {
    const u = create(FeedMergeUpdatingMainStepSchema, { step: { case: "fetching", value: {} } });
    (u as unknown as { step: { case: string; value: unknown } }).step = { case: "rewinding", value: {} };
    expect(() => drawFeedMergeUpdatingMainStep(u, "step")).toThrow(MalformedView);
  });
});

describe("drawFeedMergeTabFixingAttempt", () => {
  it("draws the attempt as 'attempt 2/3'", () => {
    const u = create(FeedMergeTabTestsRunFixingSchema, { state: LIVE, attempt: { attempt: 2, maxAttempts: 3 } });
    expect(drawFeedMergeTabFixingAttempt(u, "tests_run_fixing").textContent).toBe("attempt 2/3");
  });

  it("refuses a fixes tab with no attempt", () => {
    const u = create(FeedMergeTabTestsRunFixingSchema, { state: LIVE });
    expect(() => drawFeedMergeTabFixingAttempt(u, "tests_run_fixing")).toThrow(MalformedView);
  });
});

describe("drawFeedMergeTabRebaseCommit", () => {
  it("draws the commit's narration lines verbatim, oldest first", () => {
    const u = create(FeedMergeTabRebaseCommitSchema, {
      state: LIVE,
      lines: [text("conflicted in 1 file(s)"), text("replayed 1/2")],
    });
    expect([...drawFeedMergeTabRebaseCommit(u, "rebase_commit").querySelectorAll(".merge-line")].map((e) => e.textContent)).toEqual([
      "conflicted in 1 file(s)",
      "replayed 1/2",
    ]);
  });

  it("refuses a commit narration line that is a replay header", () => {
    const u = create(FeedMergeTabRebaseCommitSchema, { state: LIVE, lines: [{ line: { case: "replaying", value: REPLAYING } }] });
    expect(() => drawFeedMergeTabRebaseCommit(u, "rebase_commit")).toThrow(MalformedView);
  });

  it("refuses a commit narration line with no arm", () => {
    const u = create(FeedMergeTabRebaseCommitSchema, { state: LIVE, lines: [{}] });
    expect(() => drawFeedMergeTabRebaseCommit(u, "rebase_commit")).toThrow(MalformedView);
  });
});

describe("drawFeedMergeRebaseReplaying", () => {
  /** The rebasing tab drawn over one replay header, in a page that answers both clicks. */
  function header(replaying: typeof REPLAYING = REPLAYING) {
    const calls = { openWorkspace: [] as OpenWorkspaceRequest[], selectWorkspace: [] as SelectWorkspaceRequest[], openInEditor: [] as OpenInEditorRequest[] };
    const ctx = scriptedAppContext(
      {
        openWorkspace: (req) => {
          calls.openWorkspace.push(req);
          return create(OpenWorkspaceResponseSchema, { result: { case: "success", value: {} } });
        },
        selectWorkspace: (req) => {
          calls.selectWorkspace.push(req);
          return create(SelectWorkspaceResponseSchema, { result: { case: "success", value: {} } });
        },
        openInEditor: (req) => {
          calls.openInEditor.push(req);
          return create(OpenInEditorResponseSchema, { result: { case: "success", value: {} } });
        },
      },
      { workspace: WORKSPACE, failures: new RecordingSink() },
    );
    const u = create(FeedMergeTabRebasingSchema, {
      state: LIVE,
      progress: { replayed: 0, total: 5 },
      lines: [{ line: { case: "replaying", value: replaying } }],
    });
    const el = drawFeedMergeTabRebasing(u, "rebasing", rowContext(ctx, mergeRow("m1")));
    return { line: el.querySelector<HTMLElement>('[data-merge-line="replaying"]')!, calls };
  }

  it("reads as the replay's sentence, the tip by its short hash", () => {
    expect(header().line.textContent).toBe("replaying 5 commits of ABC/fix onto master at 4f2a1c012345");
  });

  it("draws the branch and the target as the replay's two references", () => {
    const refs = [...header().line.querySelectorAll(".merge-ref")].map((e) => e.textContent);
    expect(refs).toEqual(["ABC/fix", "master"]);
  });

  it("draws the tip as the shared commit link", () => {
    const link = header().line.querySelector(".merge-commit-link");
    expect([link?.tagName, link?.textContent, link?.hasAttribute("data-merge-commit")]).toEqual(["A", "4f2a1c012345", true]);
  });

  it("opens the target branch's log at the full tip when the tip is clicked", async () => {
    const { line, calls } = header();
    line.querySelector<HTMLElement>(".merge-commit-link")!.click();
    await vi.waitFor(() => expect(calls.openInEditor).toHaveLength(1));
    const target = calls.openInEditor[0]?.target;
    expect(target?.case === "commitLog" ? [target.value.repositoryDir, target.value.branch, target.value.commit] : null).toEqual([
      "/r/main",
      "master",
      TIP,
    ]);
  });

  it("opens the target's workspace and then switches to it when the target is clicked", async () => {
    const { line, calls } = header();
    line.querySelector<HTMLElement>("[data-merge-target]")!.click();
    await vi.waitFor(() => expect(calls.selectWorkspace).toHaveLength(1));
    expect([calls.openWorkspace.map((r) => r.workspace?.id), calls.selectWorkspace.map((r) => r.workspace?.id)]).toEqual([
      ["ws-main"],
      ["ws-main"],
    ]);
  });

  it("draws the target as text with no switch when its checkout is no workspace", () => {
    const { line } = header(
      create(FeedMergeRebaseReplayingSchema, {
        commits: 5,
        branch: "ABC/fix",
        target: { branch: "master", repositoryDir: "/r/main" },
        tip: TIP,
        tipShortSha: "4f2a1c012345",
      }),
    );
    const target = line.querySelectorAll(".merge-ref")[1];
    expect([target?.tagName, target?.hasAttribute("data-merge-target")]).toEqual(["SPAN", false]);
  });

  it("refuses a replay header with no target", () => {
    const u = create(FeedMergeTabRebasingSchema, {
      state: LIVE,
      progress: { replayed: 0, total: 5 },
      lines: [{ line: { case: "replaying", value: { ...REPLAYING, target: undefined } } }],
    });
    expect(() => drawFeedMergeTabRebasing(u, "rebasing", rc())).toThrow(MalformedView);
  });

  it("refuses a rebasing narration line with no arm", () => {
    const u = create(FeedMergeTabRebasingSchema, { state: LIVE, progress: { replayed: 0, total: 1 }, lines: [{}] });
    expect(() => drawFeedMergeTabRebasing(u, "rebasing", rc())).toThrow(MalformedView);
  });
});

describe("the replay header's paint", () => {
  function painted() {
    const teardownCss = installStylesheet();
    const u = create(FeedMergeTabRebasingSchema, {
      state: LIVE,
      progress: { replayed: 0, total: 5 },
      lines: [{ line: { case: "replaying", value: REPLAYING } }],
    });
    const el = drawFeedMergeTabRebasing(u, "rebasing", rc());
    document.body.append(el);
    return {
      line: el.querySelector<HTMLElement>('[data-merge-line="replaying"]')!,
      teardown: () => {
        el.remove();
        teardownCss();
      },
    };
  }

  it("draws the branch and the target in the merge purple", () => {
    const { line, teardown } = painted();
    try {
      expect([...line.querySelectorAll<HTMLElement>(".merge-ref")].map((e) => cascadedValue(e, "color"))).toEqual([
        "var(--blocked)",
        "var(--blocked)",
      ]);
    } finally {
      teardown();
    }
  });

  it("underlines the target", () => {
    const { line, teardown } = painted();
    try {
      expect(cascadedValue(line.querySelector<HTMLElement>("[data-merge-target]")!, "text-decoration")).toBe("underline");
    } finally {
      teardown();
    }
  });

  it("underlines the tip in a grey darker than the muted text", () => {
    const { line, teardown } = painted();
    try {
      const tip = line.querySelector<HTMLElement>(".merge-commit-link")!;
      expect([cascadedValue(tip, "text-decoration"), cascadedValue(tip, "color")]).toEqual([
        "underline",
        "color-mix(in srgb, var(--muted) 80%, black)",
      ]);
    } finally {
      teardown();
    }
  });
});

describe("drawFeedMergeConflictFiles", () => {
  it("draws each conflicted path verbatim, in git's order", () => {
    const u = create(FeedMergeTabRebaseCommitSchema, { conflict: { files: [{ path: "b.go" }, { path: "a.go" }] } });
    expect(
      [...drawFeedMergeConflictFiles(u.conflict?.files ?? []).querySelectorAll(".merge-conflict-file")].map((e) => e.textContent),
    ).toEqual(["b.go", "a.go"]);
  });
});
