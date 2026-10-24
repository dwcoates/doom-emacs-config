/**
 * THE ROSTER'S APP BOXES every test roster carries: the rail title's switcher
 * (`WorkspaceRoster.app_switcher`) and the Chat app's thread section
 * (`WorkspaceRoster.threads`). The sidebar requires both on every roster, so
 * every test roster builder spreads `rosterApps()` rather than spelling them.
 */
import type { MessageInitShape } from "@bufbuild/protobuf";
import type { WorkspaceRosterSchema } from "../../proto/gen/ts/frontend/v1/sidebar_pb";

/** One thread row of a test roster. */
export interface TestThread {
  id: string;
  name: string;
  summary?: string;
  current?: boolean;
}

type RosterInit = MessageInitShape<typeof WorkspaceRosterSchema>;

/**
 * The app boxes of a roster showing Workspaces, or, when CHAT is given, the
 * Chat app with those threads.
 */
export function rosterApps(chat?: TestThread[]): Pick<RosterInit, "appSwitcher" | "threads"> {
  const showsChat = chat !== undefined;
  const app = (which: "workspaces" | "chat") => ({ app: { case: which, value: {} } }) as const;
  return {
    appSwitcher: {
      title: { text: showsChat ? "Chat" : "Workspaces" },
      shown: app(showsChat ? "chat" : "workspaces"),
      options: [
        { label: { text: "Workspaces" }, hint: { text: "agent-repl" }, app: app("workspaces"), shown: showsChat ? undefined : {} },
        { label: { text: "Chat" }, hint: { text: "zettel" }, app: app("chat"), shown: showsChat ? {} : undefined },
      ],
    },
    threads: {
      newThread: { label: "New thread" },
      rows: (chat ?? []).map((t) => ({
        thread: { id: t.id, dir: `/zettel/threads/${t.name}` },
        name: { text: t.name },
        summary: t.summary === undefined ? undefined : { text: t.summary },
        current: t.current === true ? {} : undefined,
      })),
    },
  };
}
