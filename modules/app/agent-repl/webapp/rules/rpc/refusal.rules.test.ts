/**
 * EVERY GESTURE SITE RECORDS A REFUSAL THE NEXT VIEW ANSWERS THROUGH
 * `sendGesture` (`src/rpc/refusal.ts`'s `noteRefusedArm`), never by hand.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("noteRefusedArm", () => {
  it.each([
    ["src/chat/chat.tsx", "chat.gesture-refused"],
    ["src/sidebar/threads.ts", "sidebar.app-refused"],
  ])("is how %s records a refusal the next view answers, through sendGesture", (file, operation) => {
    // Arrange
    const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../..", file), "utf8");

    // Assert: every gesture goes through rpc/gesture.ts's sendGesture, which
    // is noteRefusedArm's one caller there (rules/rpc/gesture.rules.test.ts).
    expect(source).toContain(`const GESTURE_REFUSED = "${operation}";`);
    expect(source).toContain("sendGesture(");
    expect(source).not.toMatch(/was refused: \$\{cause\}/);
  });
});
