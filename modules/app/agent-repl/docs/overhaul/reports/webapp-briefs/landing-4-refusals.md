# landing-4-refusals — typed `<Rpc>Error` arms at every already-merged call site

Read first, whole: the preamble (§2 refusal rule, §5b hooks). Landing 4 gave every agentrepl.v1
`<Rpc>Error` a typed `cause` oneof: the cross-cutting four on every per-workspace rpc
(unknown_workspace, workspace_ref_mismatch{registry_dir}, transferring_away{address},
not_yet_adopted) plus per-rpc arms. Read each endpoint proto under proto/src/agentrepl/v1 for the
rpcs below and the generated `<Rpc>ErrorSchema` in proto/gen/ts/agentrepl/v1.

Scope — make every refusal site render every arm as `.refusal[data-arm="<case>"]` with a short
per-arm sentence carrying the arm's fact (registry_dir / address / any per-arm text), an unset
cause = MalformedView, and tests that ENUMERATE the arms from the schema so a later arm fails:
- src/rpc/unary.ts: keep the transport/malformed handling; add ONE shared helper
  `refusalSentence(rpcName, cause)` in src/rpc/refusal.ts for the cross-cutting four (used by
  every site; per-rpc arms get their sentence at the site) — one implementation, no drift.
- src/sidebar/verbs.ts + create.ts + tasks.ts: OpenWorkspace, CloseWorkspace (keep `blocked` →
  footer pointer), KillWorkspace, NukeWorkspace, MergeWorkspace, RestartWorkspace,
  SetWorkspacePriority, AssignWorkspaceTask, CreateWorkspace, CreateTask, UpdateTask, SelectWorkspace.
- src/tray/held-prompt.ts, held-offer.ts: UpdateHeldPrompt, AnswerHeldOffer.
- src/composer/composer.ts: SubmitPrompt (keep `merging`; add the rest; text preserved always).
- src/panels/refused.ts: RequestCommandSupport.
- src/link.ts: OpenExternal, OpenInEditor.
- src/footer/stop.ts and src/feed/rows/subagent.ts ONLY if the landing-3 agent's merge left an
  Interrupt arm unrendered (check first; do not duplicate its work).
Commit atomically per module; `npm test` and `npm run typecheck` green; report per §9 listing every
rpc and arm covered.
