import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FooterActivityTransientOverEnduringSchema,
  FooterTransientWindow,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import {
  TRANSIENT_WINDOW_MS,
  createEditorNotices,
  editorNoticeTransient,
  liveEditorNotice,
  withEditorNotice,
} from "../../src/footer/editor-notice.js";

const AT = 1_700_000_000_000;

/** The unpinned tiers with a daemon transient raised at AT_MS, or none. */
function unpinned(atMs: number | null) {
  return create(FooterActivityTransientOverEnduringSchema, {
    enduring: {},
    ...(atMs === null
      ? {}
      : {
          transient: {
            at: { atMs: BigInt(atMs) },
            expiry: { expiresAtMs: BigInt(atMs + TRANSIENT_WINDOW_MS) },
            kind: { case: "apiRestored", value: {} },
          },
        }),
  });
}

describe("the transient window", () => {
  it("is the proto's one window, five seconds", () => {
    // Arrange / Act / Assert
    expect(TRANSIENT_WINDOW_MS).toBe(FooterTransientWindow.MS);
    expect(TRANSIENT_WINDOW_MS).toBe(5_000);
  });
});

describe("createEditorNotices", () => {
  it("holds no notice until the editor raises one", () => {
    // Arrange / Act
    const notices = createEditorNotices();

    // Assert
    expect(notices.current()).toBeNull();
  });

  it("holds the newest notice raised", () => {
    // Arrange
    const notices = createEditorNotices();

    // Act
    notices.raise("editing held prompt", AT);
    notices.raise("2 prompts waiting for the daemon", AT + 1);

    // Assert
    expect(notices.current()).toEqual({ text: "2 prompts waiting for the daemon", atMs: AT + 1 });
  });

  it("tells every subscriber of each notice", () => {
    // Arrange
    const notices = createEditorNotices();
    let told = 0;
    notices.subscribe(() => (told += 1));

    // Act
    notices.raise("send: no daemon connection", AT);

    // Assert
    expect(told).toBe(1);
  });
});

describe("editorNoticeTransient", () => {
  it("is an editor_notice transient at the arrival instant, expiring one window later", () => {
    // Arrange / Act
    const transient = editorNoticeTransient({ text: "editing held prompt", atMs: AT });

    // Assert
    expect(transient.at?.atMs).toBe(BigInt(AT));
    expect(transient.expiry?.expiresAtMs).toBe(BigInt(AT + TRANSIENT_WINDOW_MS));
    expect(transient.kind).toEqual({ case: "editorNotice", value: expect.objectContaining({ text: "editing held prompt" }) });
  });
});

describe("withEditorNotice: the newest transient wins", () => {
  it("leaves the push unchanged when there is no notice", () => {
    // Arrange
    const u = unpinned(AT);

    // Act / Assert
    expect(withEditorNotice(u, null)).toBe(u);
  });

  it("draws the notice when the push carries no transient", () => {
    // Arrange / Act
    const merged = withEditorNotice(unpinned(null), { text: "n", atMs: AT });

    // Assert
    expect(merged.transient?.kind.case).toBe("editorNotice");
  });

  it("draws the notice over an older daemon transient", () => {
    // Arrange / Act
    const merged = withEditorNotice(unpinned(AT - 1), { text: "n", atMs: AT });

    // Assert
    expect(merged.transient?.kind.case).toBe("editorNotice");
  });

  it("keeps a newer daemon transient over the notice", () => {
    // Arrange
    const u = unpinned(AT + 1);

    // Act / Assert
    expect(withEditorNotice(u, { text: "n", atMs: AT })).toBe(u);
  });

  it("keeps the daemon transient when both landed at the same instant", () => {
    // Arrange
    const u = unpinned(AT);

    // Act / Assert
    expect(withEditorNotice(u, { text: "n", atMs: AT })).toBe(u);
  });
});

describe("liveEditorNotice", () => {
  it("answers a notice still inside its window", () => {
    // Arrange
    const notice = { text: "n", atMs: AT };

    // Act / Assert
    expect(liveEditorNotice(notice, AT + TRANSIENT_WINDOW_MS - 1)).toBe(notice);
  });

  it("answers null once the window has passed", () => {
    // Arrange / Act / Assert
    expect(liveEditorNotice({ text: "n", atMs: AT }, AT + TRANSIENT_WINDOW_MS)).toBeNull();
  });

  it("answers null with no notice", () => {
    // Arrange / Act / Assert
    expect(liveEditorNotice(null, AT)).toBeNull();
  });
});
