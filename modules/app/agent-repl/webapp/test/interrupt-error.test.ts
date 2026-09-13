import { afterEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  InterruptErrorSchema,
  type InterruptError,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import { MalformedView } from "../src/rpc/malformed.js";
import { ForwardingLogger, setLogger } from "../src/log.js";
import { registerWorkspaceMoved } from "../src/rpc/moved.js";
import {
  INTERRUPT_ERROR_ARMS,
  interruptErrorSentence,
  logInterruptRefusal,
  type InterruptErrorKind,
} from "../src/interrupt-error.js";

/** Every `InterruptError.kind` arm, read off the generated schema. */
const SCHEMA_ARMS: readonly string[] = (
  InterruptErrorSchema.oneofs.find((oneof) => oneof.name === "kind")?.fields ?? []
).map((field) => field.localName);

/** The refusal's set arm, built from an arm name and its own init. */
function kindOf(arm: string, value: Record<string, unknown> = {}): InterruptErrorKind {
  const error: InterruptError = create(InterruptErrorSchema, {
    kind: { case: arm as never, value: value as never },
  });
  return error.kind as InterruptErrorKind;
}

describe("INTERRUPT_ERROR_ARMS: the arm set is the schema's", () => {
  it("names every arm the proto declares", () => {
    expect([...INTERRUPT_ERROR_ARMS].sort()).toEqual([...SCHEMA_ARMS].sort());
  });

  it("names the eight arms landing 4 derived from the daemon's refusal sites", () => {
    expect([...INTERRUPT_ERROR_ARMS].sort()).toEqual(
      [
        "confirmRequired",
        "unknownWorkspace",
        "workspaceRefMismatch",
        "transferringAway",
        "notYetAdopted",
        "notDetachedWork",
        "noSession",
        "shimRefused",
      ].sort(),
    );
  });
});

describe("interruptErrorSentence: every arm words itself", () => {
  it.each(SCHEMA_ARMS)("gives %s a sentence of its own", (arm) => {
    expect(interruptErrorSentence(kindOf(arm), "InterruptError.kind")).not.toBe("");
  });

  it("words the arms distinctly, one refusal never reading as another", () => {
    const sentences = SCHEMA_ARMS.map((arm) =>
      interruptErrorSentence(kindOf(arm), "InterruptError.kind"),
    );
    expect(new Set(sentences).size).toBe(SCHEMA_ARMS.length);
  });

  it("names the registry's own directory on a ref mismatch", () => {
    expect(
      interruptErrorSentence(
        kindOf("workspaceRefMismatch", { registryDir: "/w/other" }),
        "InterruptError.kind",
      ),
    ).toContain("/w/other");
  });

  it("names the successor's address on a transfer", () => {
    expect(
      interruptErrorSentence(
        kindOf("transferringAway", { address: "127.0.0.1:9931" }),
        "InterruptError.kind",
      ),
    ).toContain("127.0.0.1:9931");
  });

  it("relays the shim's own account of its refusal", () => {
    expect(
      interruptErrorSentence(
        kindOf("shimRefused", { detail: "the task had already ended" }),
        "InterruptError.kind",
      ),
    ).toContain("the task had already ended");
  });

  it("counts one live agent in the singular", () => {
    expect(
      interruptErrorSentence(kindOf("confirmRequired", { liveAgentCount: 1n }), "InterruptError.kind"),
    ).toBe("1 live agent would also stop");
  });

  it("counts several live agents in the plural", () => {
    expect(
      interruptErrorSentence(kindOf("confirmRequired", { liveAgentCount: 3n }), "InterruptError.kind"),
    ).toBe("3 live agents would also stop");
  });

  it("refuses an arm this build has no wording for", () => {
    expect(() =>
      interruptErrorSentence({ case: "quarantined", value: {} } as never, "InterruptError.kind"),
    ).toThrow(MalformedView);
  });
});

describe("logInterruptRefusal: the one line a refused stop leaves behind", () => {
  it("warns with the arm's own case and sentence, under the caller's operation", () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    logInterruptRefusal(kindOf("noSession"), "this workspace has no session to interrupt", "footer.turn-stop-refused");
    expect(
      lines.some(
        ([level, line]) =>
          level === "warn" &&
          line.includes("noSession") &&
          line.includes("footer.turn-stop-refused"),
      ),
    ).toBe(true);
  });
});

/**
 * THE STOP GOES THROUGH THE ONE REFUSAL HOOK.
 *
 * The cross-cutting four are not this module's to word, and the hook that
 * words them is also the one that raises the page-wide "workspace moved"
 * notice. This module used to spell those four out itself, which meant a stop
 * refused as `transferring_away` drew a sentence beside the stop button and
 * told the rest of the page nothing. (Audit 1 item 18, ruled.)
 */
describe("interruptErrorSentence: the cross-cutting four go through the hook", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("raises the page-wide moved notice on transferring_away", () => {
    // Arrange
    const moved = vi.fn();
    const unregister = registerWorkspaceMoved(moved);
    // Act
    interruptErrorSentence(
      kindOf("transferringAway", { address: "127.0.0.1:9931" }),
      "InterruptError.kind",
    );
    unregister();
    // Assert
    expect(moved).toHaveBeenCalledWith("127.0.0.1:9931");
  });

  it("raises no moved notice for any other arm", () => {
    // Arrange
    const moved = vi.fn();
    const unregister = registerWorkspaceMoved(moved);
    // Act
    for (const arm of SCHEMA_ARMS.filter((a) => a !== "transferringAway")) {
      interruptErrorSentence(kindOf(arm), "InterruptError.kind");
    }
    unregister();
    // Assert
    expect(moved).not.toHaveBeenCalled();
  });

  it("words the cross-cutting arms exactly as the shared hook does", () => {
    // Arrange / Act / Assert: one vocabulary, so a stop and a sidebar verb
    // refused the same way read the same way.
    expect(interruptErrorSentence(kindOf("unknownWorkspace"), "InterruptError.kind")).toBe(
      "the daemon does not know this workspace",
    );
  });
});
