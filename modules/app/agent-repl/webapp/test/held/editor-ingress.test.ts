// The editor's own held-prompt ingress entries, as the host last handed them.
// See src/held/editor-ingress.ts.
import { describe, expect, it, vi } from "vitest";
import { createEditorIngressView } from "../../src/held/editor-ingress.js";
import { editorIngressEntry } from "../feed/harness.js";

describe("createEditorIngressView", () => {
  it("starts empty", () => {
    expect(createEditorIngressView().entries()).toEqual([]);
  });

  it("answers the list last set, whole", () => {
    // Arrange
    const view = createEditorIngressView();
    view.set([editorIngressEntry("k-1"), editorIngressEntry("k-2")]);
    // Act
    view.set([editorIngressEntry("k-2")]);
    // Assert
    expect(view.entries().map((e) => e.key)).toEqual(["k-2"]);
  });

  it("tells every subscriber of each change", () => {
    // Arrange
    const view = createEditorIngressView();
    const listener = vi.fn();
    view.subscribe(listener);
    // Act
    view.set([editorIngressEntry("k-1")]);
    // Assert
    expect(listener).toHaveBeenCalledTimes(1);
  });

  it("tells a subscriber nothing once it unsubscribed", () => {
    // Arrange
    const view = createEditorIngressView();
    const listener = vi.fn();
    view.subscribe(listener)();
    // Act
    view.set([editorIngressEntry("k-1")]);
    // Assert
    expect(listener).not.toHaveBeenCalled();
  });
});
