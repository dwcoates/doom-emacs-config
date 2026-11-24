# The version hover's workspaces entry

The owner ruled (2026-11-23) that the sidebar's version hover drops its
"pages" and "agents" lines for one "workspaces" entry whose hover opens a child
popup of three counts: "workspaces visible" (of the open ones), "workspace
shims" (up, not parked), and "workspace loaded webpages" (pages the web view
holds, never eligibility). The word "loaded" is never used for a count of
visible workspaces.

Design mode: autonomous.

## Assumptions

- The producer is the daemon (`memorypopup`), the consumer the webapp's rail
  (`sidebar/memory.ts`). Emacs does not read the overview.
- The change is non-additive: `SidebarMemoryOverview.pages` and `.agents`
  are retired, with their messages.

## Landed changes

### SidebarMemoryOverview.workspaces (frontend/v1/sidebar_memory.proto)

- WHAT: `SidebarMemoryOverview` reserves fields 1 (`pages`) and 3 (`agents`)
  and gains `workspaces` (6), a `SidebarMemoryWorkspaces` holding one element
  message per child-popup line: `SidebarMemoryWorkspacesVisible` (visible, of
  open), `SidebarMemoryWorkspacesShims` (up, with the shims' summed and
  average footprint) and `SidebarMemoryWorkspacesPages` (pages held, with the
  web content process's footprint). `SidebarMemoryPages`,
  `SidebarMemoryAgents`, `SidebarMemoryAgentsNone` and
  `SidebarMemoryAgentsRunning` are deleted: nothing else referenced them.
- WHY: the owner's ruling, and each count reads the one fact its metric reads
  (visibility for `agentrepl_workspaces_visible`, the sessions census for
  `agentrepl_sessions_up`, the held-page table for `agentrepl_pages_loaded`),
  so the hover and the metrics cannot disagree. The old `pages.loaded` counted
  visible workspaces as loaded, which has been false since visibility only
  makes a page eligible.
- WHERE THE MEMORY FIGURES WENT: the web content process's footprint is on the
  webpages line and the shims' total and average on the shims line, the lines
  whose counts they belong to.
- ALSO: `SidebarWorkspaceMemoryPage` (a row hover's page line) now says loaded
  exactly when the web view holds the page, from the same table; its comments
  no longer claim a page is held while visible.
- CONSEQUENCES: the webapp draws the entry first, before the stalls line,
  through the row panel's details mechanism (`detail-more.ts` `childPopup`),
  and the over-the-feed mirror wires its copy as it wires a row panel's. The
  entry's own words are the three counts in short form.
- KNOWN GAP: the loaded-page count is Emacs's report of the pages it holds
  (`ReportPageState`). The web host's own frame count is only logged by its
  load census, so a host call that failed could leave the two apart without
  any record; a cross-check is dispatched separately.

Obviated-declaration sweep: the four messages above, deleted; nothing else.
