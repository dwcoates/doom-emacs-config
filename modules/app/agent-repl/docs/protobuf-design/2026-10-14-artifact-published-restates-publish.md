# A published artifact's success restates its publish

The finished artifact card's heading is "favicon emoji + title", but the
favicon is stated only by the call (AgentArtifactStart's publish act). The
daemon drew it from the start it held in memory, so a consumer that never
processed the start — a replay, or a live intake that served the settle first —
drew a card with no favicon. Observed in an e2e run on 2026-10-14
(TestArtifactHeadingKeepsTheFavicon): the daemon's log holds two Success
draws for the unit and no Start draw.

Owner rulings: contract changes are decided and carried through by the
implementing agent (2026-10-13); this change is part of the test suite
runtime remediation (docs/investigations/2026-10-14-test-suite-runtime-remediation.md, P2).

## Landed changes

### AgentArtifactPublished.publish, and contract revision 2

- WHAT: `conversation.v1.AgentArtifactPublished` gains the optional
  `publish` (`AgentArtifactPublish`), restated whole from the call;
  `AgentActivityContract` gains
  `ARTIFACT_PUBLISHED_RESTATES_ITS_PUBLISH = 2`.
- WHY: SETTLES_STAND_ALONE already promised that every settled arm restates
  its start, an artifact call's act included, but the success arm had no
  field to carry it; the failure arm already restates its act.
- WHY INSIDE `AgentArtifactPublished`, not a sibling `oneof act` on
  `AgentArtifactSuccess` (the failure's shape): beside `outcome`, an act
  oneof could pair `published` with a restated `list`, a combination with no
  meaning. Inside the published arm the restated act can only be a publish.
  `AgentArtifactListed` restates nothing: a listing draws no card.
- WHY A NEW REVISION: the daemon grades a missing restatement by the
  producer's stamped revision (resolve/feed standsAlone). Rows stamped 1 by
  producers before this change lack the field as expected old data; a row
  stamped 2 without it is a producer defect, recorded at ERROR.
- Consequences: the shim (src/convert/tools/artifact.ts) and the sidecar
  (internal/convert/results.go) fill the field and stamp revision 2; the
  daemon's drawArtifact draws the heading from the restated publish, with
  the outcome's title winning, and falls back to the held start only for a
  row below revision 2. No webapp change: the card's heading is resolved by
  the daemon.
