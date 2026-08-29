/**
 * R9: the main agent's identity, and what a rotation must not do to it.
 *
 * WHAT THIS GUARDS: that one conversation has exactly one AgentId for its whole
 * life. The failure mode being excluded is a second AgentId minted at a restart
 * or a rotation, which does not crash — it silently SPLITS the conversation's
 * book, and every entry written before the split becomes unreachable under the
 * name the consumer holds.
 */
import { mkdtempSync, readFileSync, writeFileSync, mkdirSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import {
  agentIdPath,
  createAgentIdentityStore,
  mintVendorSessionId,
  resolveOriginal,
  SessionIdentity,
  vendorLinkPath,
} from "../../src/engine/identity.js";

function scratch(): string {
  return mkdtempSync(path.join(os.tmpdir(), "shim-identity-"));
}

const WORKSPACE = "abc12345";

describe("the persisted main agent identity", () => {
  it("reports absence when nothing has been persisted for the workspace", async () => {
    const store = createAgentIdentityStore(scratch(), WORKSPACE);

    expect(await store.read()).toBeUndefined();
  });

  it("persists the original id, the workspace key and the mint instant", async () => {
    const state = scratch();
    const store = createAgentIdentityStore(state, WORKSPACE, () => 1234);

    await store.write("vendor-1");

    expect(JSON.parse(readFileSync(agentIdPath(state, WORKSPACE), "utf8"))).toEqual({
      original_vendor_session_id: "vendor-1",
      workspace_key: WORKSPACE,
      minted_at_ms: 1234,
    });
  });

  it("reads back exactly what it persisted", async () => {
    const store = createAgentIdentityStore(scratch(), WORKSPACE);
    await store.write("vendor-1");

    expect(await store.read()).toBe("vendor-1");
  });

  it("RAISES on a record with no original id rather than reporting absence", async () => {
    const state = scratch();
    const file = agentIdPath(state, WORKSPACE);
    mkdirSync(path.dirname(file), { recursive: true });
    writeFileSync(file, JSON.stringify({ workspace_key: WORKSPACE }), "utf8");

    // Reporting absence here would mint a SECOND identity for one conversation.
    await expect(createAgentIdentityStore(state, WORKSPACE).read()).rejects.toThrow(
      /no original_vendor_session_id/,
    );
  });
});

describe("a fresh conversation", () => {
  it("adopts the pre-minted vendor session id as the AgentId", async () => {
    const identity = await SessionIdentity.fresh(
      createAgentIdentityStore(scratch(), WORKSPACE),
      () => "minted-1",
    );

    expect(identity.agentId.value).toBe("minted-1");
  });

  it("persists that id before anything can rotate it", async () => {
    const state = scratch();

    await SessionIdentity.fresh(createAgentIdentityStore(state, WORKSPACE), () => "minted-1");

    expect(
      (JSON.parse(readFileSync(agentIdPath(state, WORKSPACE), "utf8")) as { original_vendor_session_id: string })
        .original_vendor_session_id,
    ).toBe("minted-1");
  });

  it("mints a distinct id each time", () => {
    expect(mintVendorSessionId()).not.toBe(mintVendorSessionId());
  });
});

describe("a resumed conversation", () => {
  it("reports the PERSISTED id, not the resume handle", async () => {
    const store = createAgentIdentityStore(scratch(), WORKSPACE);
    await store.write("original-1");

    const identity = await SessionIdentity.resume(store, "rotated-2");

    expect(identity.agentId.value).toBe("original-1");
  });

  it("still reports the resume handle as the vendor session id in force", async () => {
    const store = createAgentIdentityStore(scratch(), WORKSPACE);
    await store.write("original-1");

    const identity = await SessionIdentity.resume(store, "rotated-2");

    expect(identity.vendorSessionId).toBe("rotated-2");
  });

  it("ADOPTS the resume id as the original when no record was persisted", async () => {
    // The R9 resume rule: a resume never rotates (every real transcript's
    // sessionId equals its filename), so the id being resumed IS the original.
    const identity = await SessionIdentity.resume(
      createAgentIdentityStore(scratch(), WORKSPACE),
      "resume-1",
    );

    expect(identity.agentId.value).toBe("resume-1");
  });

  it("persists the adopted id so the next start does not re-derive it", async () => {
    const state = scratch();

    await SessionIdentity.resume(createAgentIdentityStore(state, WORKSPACE), "resume-1");

    expect(await createAgentIdentityStore(state, WORKSPACE).read()).toBe("resume-1");
  });
});

describe("a vendor id rotation", () => {
  it("KEEPS the AgentId", async () => {
    const identity = await SessionIdentity.fresh(
      createAgentIdentityStore(scratch(), WORKSPACE),
      () => "original-1",
    );

    await identity.rotate("rotated-2");

    expect(identity.agentId.value).toBe("original-1");
  });

  it("moves the vendor session id in force", async () => {
    const identity = await SessionIdentity.fresh(
      createAgentIdentityStore(scratch(), WORKSPACE),
      () => "original-1",
    );

    await identity.rotate("rotated-2");

    expect(identity.vendorSessionId).toBe("rotated-2");
  });

  it("reports both ids on the update", async () => {
    const identity = await SessionIdentity.fresh(
      createAgentIdentityStore(scratch(), WORKSPACE),
      () => "original-1",
    );

    const update = await identity.rotate("rotated-2");

    expect(update.update).toEqual({
      case: "identityRotated",
      value: expect.objectContaining({
        previousVendorSessionId: "original-1",
        vendorSessionId: "rotated-2",
      }),
    });
  });

  it("writes the link the transcript files do not carry", async () => {
    const state = scratch();
    const identity = await SessionIdentity.fresh(
      createAgentIdentityStore(state, WORKSPACE),
      () => "original-1",
    );

    await identity.rotate("rotated-2");

    expect(
      (JSON.parse(readFileSync(vendorLinkPath(state, WORKSPACE, "rotated-2"), "utf8")) as {
        original_vendor_session_id: string;
      }).original_vendor_session_id,
    ).toBe("original-1");
  });

  it("leaves `reason` unset — the declared surface names no producer for it", async () => {
    const identity = await SessionIdentity.fresh(
      createAgentIdentityStore(scratch(), WORKSPACE),
      () => "original-1",
    );

    const update = await identity.rotate("rotated-2");

    expect(Object.keys(update.update.value ?? {})).not.toContain("reason");
  });
});

describe("resolving a vendor id to its book, from files alone", () => {
  it("answers a rotated id with the original it was linked to", async () => {
    const state = scratch();
    const identity = await SessionIdentity.fresh(
      createAgentIdentityStore(state, WORKSPACE),
      () => "original-1",
    );
    await identity.rotate("rotated-2");

    expect(resolveOriginal(state, WORKSPACE, "rotated-2")).toBe("original-1");
  });

  it("answers an unrotated id with itself", () => {
    expect(resolveOriginal(scratch(), WORKSPACE, "never-rotated")).toBe("never-rotated");
  });
});
