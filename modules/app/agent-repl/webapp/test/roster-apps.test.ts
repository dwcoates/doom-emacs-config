import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { WorkspaceRosterSchema } from "../../proto/gen/ts/frontend/v1/sidebar_pb";
import { showsChat } from "../src/sidebar/threads.js";
import { rosterApps } from "./roster-apps.js";

describe("rosterApps", () => {
  it.each([
    ["Workspaces", undefined, false],
    ["Chats", [], true],
  ])("shows %s, marked once in the menu", (title, chat, chatShown) => {
    // Act
    const roster = create(WorkspaceRosterSchema, rosterApps(chat));

    // Assert
    expect(roster.appSwitcher?.title?.text).toBe(title);
    expect(showsChat(roster.appSwitcher!)).toBe(chatShown);
    expect(roster.appSwitcher?.options.filter((o) => o.shown !== undefined).map((o) => o.label?.text)).toEqual([title]);
  });

  it("draws each thread a row under the zettel threads directory", () => {
    // Act
    const roster = create(WorkspaceRosterSchema, rosterApps([{ id: "t-1", name: "why", summary: "light", current: true }]));

    // Assert
    const [row] = roster.threads!.rows;
    expect(row.thread).toMatchObject({ id: "t-1", dir: "/zettel/threads/why" });
    expect(row.name?.text).toBe("why");
    expect(row.summary?.text).toBe("light");
    expect(row.current).toBeDefined();
    expect(roster.threads?.newThread?.label).toBe("New thread");
  });
});
