/**
 * The automation family: plan mode, findings, worktrees, cron, notifications,
 * monitors, wakeups, artifacts, and the unmodeled MCP tool.
 *
 * Each family's test names the ONE field that separates its arms — the exit
 * action, the disabled reason, the persistence flag, the act.
 */
import { describe, expect, it } from "vitest";

import { driveScenario, recordsOfType, toolUseResults, toolUses } from "../harness.js";

const results = async (prompt: string): Promise<Record<string, unknown>[]> => {
  const driven = await driveScenario([prompt]);
  return toolUseResults(driven.transcript()) as Record<string, unknown>[];
};

describe("plan mode", () => {
  it("enters and exits in one turn", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!plan"]);

    // Assert
    expect(toolUses(driven).map((t) => t.name)).toEqual(["EnterPlanMode", "ExitPlanMode"]);
  });

  it("names the file the plan was saved to", async () => {
    // Arrange + Act
    const exit = (await results("!plan"))[1] as { filePath: string };

    // Assert
    expect(exit.filePath).toMatch(/plans\/offline-plan\.md$/);
  });

  it("records the exit as an attachment that says the plan file EXISTS", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!plan"]);
    const attachment = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as {
      type: string;
      planExists: boolean;
    };

    // Assert
    expect({ type: attachment.type, exists: attachment.planExists }).toEqual({
      type: "plan_mode_exit",
      exists: true,
    });
  });
});

describe("report findings", () => {
  it("carries both verdicts in one call, so neither arm is unreachable", async () => {
    // Arrange + Act
    const reported = (await results("!findings"))[0] as { findings: { verdict?: string }[] };

    // Assert
    expect(reported.findings.map((f) => f.verdict)).toEqual(["CONFIRMED", "PLAUSIBLE", undefined]);
  });

  it("carries all three outcomes in one call", async () => {
    // Arrange + Act
    const reported = (await results("!findings"))[0] as { findings: { outcome?: string }[] };

    // Assert
    expect(reported.findings.map((f) => f.outcome)).toEqual(["fixed", "skipped", "no_change_needed"]);
  });

  it("reports the count and the effort level the review ran at", async () => {
    // Arrange + Act
    const reported = (await results("!findings"))[0];

    // Assert
    expect({ count: reported.count, level: reported.level }).toEqual({ count: 3, level: "high" });
  });
});

describe("worktrees", () => {
  it("reports a kept worktree with action keep and no discard counts", async () => {
    // Arrange + Act
    const exit = (await results("!worktree-keep"))[1];

    // Assert
    expect({ action: exit.action, files: exit.discardedFiles }).toEqual({
      action: "keep",
      files: undefined,
    });
  });

  it("reports a removed worktree with the discarded file and commit counts", async () => {
    // Arrange + Act
    const exit = (await results("!worktree-remove"))[1];

    // Assert
    expect({ action: exit.action, files: exit.discardedFiles, commits: exit.discardedCommits }).toEqual({
      action: "remove",
      files: 3,
      commits: 1,
    });
  });
});

describe("cron", () => {
  it("exercises all three acts in one turn", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cron"]);

    // Assert
    expect(toolUses(driven).map((t) => t.name)).toEqual(["CronCreate", "CronList", "CronDelete"]);
  });

  it("reports the created job as recurring with a human schedule", async () => {
    // Arrange + Act
    const created = (await results("!cron"))[0];

    // Assert
    expect({ recurring: created.recurring, human: created.humanSchedule }).toEqual({
      recurring: true,
      human: "every Monday at 9:00am",
    });
  });
});

describe("push notifications", () => {
  it("reports a sent notification with no disabled reason", async () => {
    // Arrange + Act
    const sent = (await results("!push-sent"))[0];

    // Assert
    expect({ sent: sent.pushSent, reason: sent.disabledReason }).toEqual({
      sent: true,
      reason: undefined,
    });
  });

  it("names each of the three declined reasons", async () => {
    // Arrange + Act
    const reasons: unknown[] = [];
    for (const prompt of ["!push-config-off", "!push-user-present", "!push-no-transport"]) {
      reasons.push(((await results(prompt))[0]).disabledReason);
    }

    // Assert
    expect(reasons).toEqual(["config_off", "user_present", "no_transport"]);
  });
});

describe("monitors", () => {
  it("reports a deadline monitor with a finite timeout", async () => {
    // Arrange + Act
    const monitor = (await results("!monitor-deadline"))[0];

    // Assert
    expect({ ms: monitor.timeoutMs, persistent: monitor.persistent }).toEqual({
      ms: 600_000,
      persistent: false,
    });
  });

  it("reports a persistent monitor with a zero timeout", async () => {
    // Arrange + Act
    const monitor = (await results("!monitor-persistent"))[0];

    // Assert. Zero and `persistent: true` together are the arm; either alone
    // would be ambiguous.
    expect({ ms: monitor.timeoutMs, persistent: monitor.persistent }).toEqual({
      ms: 0,
      persistent: true,
    });
  });
});

describe("what a Monitor call itself states", () => {
  /** The input of the one `Monitor` call a prompt makes. */
  const monitorInput = async (prompt: string): Promise<Record<string, unknown>> => {
    const driven = await driveScenario([prompt]);
    const call = toolUses(driven).find((block) => (block as { name?: string }).name === "Monitor");
    return (call as unknown as { input: Record<string, unknown> }).input;
  };

  it("states the deadline monitor's description ON THE CALL, which is where the shim reads it", async () => {
    // Arrange + Act
    const input = await monitorInput("!monitor-deadline");

    // Assert
    expect(input.description).toBe("build log");
  });

  it("spells the deadline as `timeout_ms`, as MonitorInput declares it", async () => {
    // Arrange + Act
    const input = await monitorInput("!monitor-deadline");

    // Assert
    expect(input.timeout_ms).toBe(600_000);
  });

  it("states the persistent monitor's description on the call too", async () => {
    // Arrange + Act
    const input = await monitorInput("!monitor-persistent");

    // Assert
    expect(input.description).toBe("echo watch");
  });

  it("carries `timeout_ms` on the persistent call even though persistence ignores it", async () => {
    // MonitorInput declares it REQUIRED; a call omitting it is a shape the
    // vendor's own schema rejects.
    const input = await monitorInput("!monitor-persistent");

    // Assert
    expect(input.timeout_ms).toBe(0);
  });

  it("names the persistent watch's source as a `ws` url, the only declared socket field", async () => {
    // Arrange + Act
    const input = await monitorInput("!monitor-persistent");

    // Assert
    expect((input.ws as { url?: string }).url).toBe("ws://127.0.0.1:8787/echo");
  });
});

describe("scheduled wakeups", () => {
  it("reports a scheduled wakeup with its unclamped delay", async () => {
    // Arrange + Act
    const wakeup = (await results("!wakeup-schedule"))[0];

    // Assert
    expect({ delay: wakeup.clampedDelaySeconds, clamped: wakeup.wasClamped }).toEqual({
      delay: 1_200,
      clamped: false,
    });
  });

  it("reports a stop with the count it cancelled", async () => {
    // Arrange + Act
    const stopped = (await results("!wakeup-stop"))[0];

    // Assert
    expect({ stopped: stopped.stopped, cancelled: stopped.cancelledWakeups }).toEqual({
      stopped: true,
      cancelled: 1,
    });
  });
});

describe("artifacts", () => {
  it("reports a publish with the url and the source path", async () => {
    // Arrange + Act
    const published = (await results("!artifact-publish"))[0];

    // Assert
    expect({ hasUrl: typeof published.url === "string", path: published.path }).toEqual({
      hasUrl: true,
      path: "/tmp/offline-report.html",
    });
  });

  it("writes the vendor's frame-link sidebar record, unchained", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!artifact-publish"]);
    const link = driven.transcript().find((l) => l.type === "frame-link");

    // Assert. Metadata lines carry no uuid and never become a parent.
    expect({ present: link !== undefined, uuid: link?.uuid }).toEqual({ present: true, uuid: undefined });
  });

  it("reports a list with both an owned and a shared row", async () => {
    // Arrange + Act
    const listed = (await results("!artifact-list"))[0] as { artifacts: { rel?: string }[] };

    // Assert
    expect(listed.artifacts.map((a) => a.rel)).toEqual(["mine", "shared"]);
  });
});

describe("an MCP tool", () => {
  it("calls the echo server's tool", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!mcp-tool"]);

    // Assert
    expect((toolUses(driven)[0] as { name: string }).name).toBe("mcp__echo__echo");
  });

  it("answers with the server's text payload", async () => {
    // Arrange + Act
    const echoed = (await results("!mcp-tool"))[0];

    // Assert
    expect(Object.keys(echoed).sort()).toEqual(["content", "isError"].sort());
  });
});

describe("an unmodeled tool", () => {
  it("calls a tool no converter owns and no MCP server serves", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!unmodeled"]);

    // Assert
    expect((toolUses(driven)[0] as { name: string }).name).toBe("StructuredOutput");
  });

  it("answers with an opaque payload rather than a modeled shape", async () => {
    // Arrange + Act
    const answered = (await results("!unmodeled"))[0];

    // Assert
    expect(Object.keys(answered).sort()).toEqual(["content", "isError"].sort());
  });
});
