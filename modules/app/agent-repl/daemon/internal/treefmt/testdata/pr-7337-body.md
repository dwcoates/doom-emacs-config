### Jira Ticket Link:

https://chesscom.atlassian.net/browse/CV-506405

### PR Description / Notes:

#### Original motivation
<pre>
1. 🎯 Desired functionality.
├── 1.1. A vs-coach board reads move feedback, candidate moves, and threats for one Game Point from a single call.
├── 1.2. Each candidate move arrives with its own classification, not just a score, so a client filters per suggestion arrow.
└── 1.3. 🧪 Worked example.
    ├── 1.3.1. <mark><b>NewGame</b></mark> with <mark><b>search_parameters.num_lines = 3</b></mark>.
    └── 1.3.2. <a href="https://github.com/ChessCom/explanation-engine/blob/slack-cee-ceac-integration-shj/explanation-engine/src/features/feature_analysis_annotations.cpp">AnalysisAnnotations</a> with <mark><b>enable_pv_classifications = true</b></mark> returns the classification, three classified candidate moves, and the threats in one response.

2. 🔀 Best current alternative.
├── 2.1. <mark><b>RunSearch{num_lines: 3}</b></mark> paired with <mark><b>MoveClassification</b></mark> and <mark><b>SimpleThreats</b></mark>, which is what Android does today.
│   ├── 2.1.1. 🧪 The client loops <mark><b>RunSearch(depth_limit = 12/14/16/18/20)</b></mark>, calls <mark><b>MoveClassification</b></mark> after each, and calls <mark><b>GetThreats</b></mark> separately.
│   └── 2.1.2. The gap is that <mark><b>RunSearch</b></mark> and <mark><b>MoveClassification</b></mark> search the same position, so it is paid for twice.
└── 2.2. <mark><b>MoveClassification</b></mark> alone, which caps candidates at two.
    └── 2.2.1. <mark><b>active</b></mark> and <mark><b>alternative</b></mark> describe one move each whatever <mark><b>num_lines</b></mark> says, so a third arrow is unobtainable.
</pre>

#### Existing limitation
<pre>
1. 🔒 The response shape caps candidates at two, and both are the wrong moves for arrows.
├── 1.1. <a href="https://github.com/ChessCom/explanation-engine/blob/slack-cee-ceac-integration-shj/explanation-engine/src/features/feature_move_classification.cpp#L74-L106">compute_move_classification</a> fills only <mark><b>active</b></mark> and <mark><b>alternative</b></mark>, neither repeated.
└── 1.2. Both name moves from the PREVIOUS position, so neither is a move available at the Game Point.

2. 🧩 The staging that a classification needs was private to one feature.
├── 1.1. The search ladder lived inline in <mark><b>MoveClassificationFeature</b></mark>, so a second classifying feature had no way to reuse it.
└── 2.2. <mark><b>SimpleThreatsFeature</b></mark> kept its threat computation and protobuf fill in private members, so threats could not be reported by any other response.
</pre>

#### Implementation strategy
<pre>
1. 🧯 Backwards compatibility.
├── 1.1. <mark><b>active</b></mark>, <mark><b>alternative</b></mark>, and <mark><b>progress</b></mark> keep their field numbers, types, and meanings, so a caller that does nothing sees no change.
├── 1.2. <mark><b>pv_classifications</b></mark> is opt-in behind <mark><b>enable_pv_classifications</b></mark> and left unset otherwise.
└── 1.3. <mark><b>AnalysisAnnotations</b></mark> is a new feature id, so nothing existing is rerouted through it.

2. 🗂️ Game state is the single source for reported lines.
├── 2.1. <a href="https://github.com/ChessCom/explanation-engine/blob/slack-cee-ceac-integration-shj/explanation-engine/src/features/feature_move_classification.cpp#L49-L67">fill_pv_classifications</a> reads the Game Point's strong-scored children rather than a search's return value.
└── 2.2. A streamed response and an unstreamed one therefore report the same thing from the same state.

3. 🧱 Each part of a bundled response is the owning feature's own response.
├── 3.1. <mark><b>AnalysisAnnotationsResponse</b></mark> embeds <mark><b>MoveClassificationResponse</b></mark> and <mark><b>SimpleThreatsResponse</b></mark> verbatim.
└── 3.2. The api tests assert equality against those features called directly, rather than restating expected values.
</pre>

#### Code added
<pre>
1. ➕ <a href="https://github.com/ChessCom/explanation-engine/blob/slack-cee-ceac-integration-shj/explanation-engine/src/features/feature_analysis_annotations.h">AnalysisAnnotationsFeature</a>.
├── 1.1. Motivation: a board needs every annotation for one Game Point without paying for three calls.
└── 1.2. Function: composes the move classification and the threats into one response, forwarding the opt-in flag through.

2. ➕ <a href="https://github.com/ChessCom/explanation-engine/blob/slack-cee-ceac-integration-shj/explanation-engine/src/features/classification_stages.h">ClassificationStagesFeature</a>.
├── 2.1. Motivation: two features now need the same search staging before they can classify anything.
└── 2.2. Function: owns when a subclass's <mark><b>push_frame</b></mark> is called and with what progress, once when unstreamed and once per stage when streamed.

3. ➕ <a href="https://github.com/ChessCom/explanation-engine/blob/slack-cee-ceac-integration-shj/explanation-engine/src/features/feature_move_classification.cpp#L49-L67">fill_pv_classifications</a>.
├── 3.1. Motivation: the response needs one classified entry per candidate move available at the Game Point.
└── 3.2. Function: walks the strong-scored children up to the configured line count, pairing each child's classification with a line beginning at that child's own move.

4. ➕ <a href="https://github.com/ChessCom/explanation-engine/blob/slack-cee-ceac-integration-shj/explanation-engine/src/search_opts.h">search_progress</a>.
├── 4.1. Motivation: <mark><b>RunSearch</b></mark> computed its progress fraction inline, and a second caller needed the same arithmetic.
└── 4.2. Function: returns the largest of the depth, nodes, and time fractions, clamped to <mark><b>[0, 1]</b></mark>.
</pre>

#### Code changed
<pre>
1. 🔁 <a href="https://github.com/ChessCom/explanation-engine/blob/slack-cee-ceac-integration-shj/explanation-engine/src/features/feature_move_classification.cpp#L74-L106">compute_move_classification</a>.
├── 1.1. Change: takes an explicit <mark><b>include_pv_classifications</b></mark> parameter and fills the new wrapper when it is set.
└── 1.2. Resolution: the parameter has no default, so every call site states its intent rather than inheriting one.

2. 🔁 <a href="https://github.com/ChessCom/explanation-engine/blob/slack-cee-ceac-integration-shj/explanation-engine/src/features/feature_simple_threats.h">SimpleThreatsFeature</a>.
├── 2.1. Change: its threat computation and protobuf fill moved out of private members into free functions.
└── 2.2. Resolution: the feature now calls the same two functions <mark><b>AnalysisAnnotations</b></mark> does, so neither reimplements the other.

3. 🔁 <a href="https://github.com/ChessCom/explanation-engine/blob/slack-cee-ceac-integration-shj/explanation-engine/src/fill_continuations.h">ContinuationOptions</a>.
├── 3.1. Change: gained <mark><b>played_move_limits()</b></mark> and <mark><b>null_move_limits()</b></mark>, and <mark><b>eval_items</b></mark> now takes a <mark><b>SearchOpts</b></mark>.
└── 3.2. Resolution: the limits a ContinuationOptions implies are derived from it rather than reassembled per call site.

4. 🔁 An unstreamed run now stamps <mark><b>progress</b></mark> 1 on its single response.
└── 4.1. Resolution: matches what the field already documented and what a streamed run's final response does.
</pre>

#### Testing
<pre>
1. 🧪 Response-shape coverage.
├── 1.1. Ordering, per-entry principal variation, per-entry classification, the configured-line-count cap, the opt-in gate, the root case, and the forced-move case.
└── 1.2. A pawn endgame outside the opening book separates the best candidate from the inferior one, since candidates in the opening all classify as book moves.

2. 🔗 Composition coverage.
└── 2.1. <mark><b>AnalysisAnnotations</b></mark> is asserted equal to <mark><b>MoveClassification</b></mark> and <mark><b>SimpleThreats</b></mark> called directly, so the embedded parts cannot drift from their sources.

3. 🌊 Streaming coverage.
├── 3.1. Per-stage arrival, monotonic progress, the exactly-one terminal <mark><b>progress</b></mark>, and every configured variation present in every response at two and at three lines.
└── 3.2. ⚠️ A depth-contiguity test is present and tagged <mark><b>[!shouldfail]</b></mark>, because streaming reports at stage depths rather than at every depth the engine passes.
    └── 3.2.1. It is written against the contract we want, so it turns red the moment the feature reports contiguously and the tag comes off.
</pre>

#### Side Questions

- `.claude/skills/create-or-update-protobufs/SKILL.md` carries a pre-existing commit from an earlier session making figma-to-idl the duplication default, unrelated to this feature.
- `explanation-engine/unit_tests/test_engine.cpp` gains a characterization test for the per-depth watcher gate in `uci::advance`, which pins existing behavior rather than changing it.

#### Resources

- Kickoff Slack thread: https://chesscom.slack.com/archives/C020S8V7VG8/p1785934805367749
- Definitions PR (merged): https://github.com/ChessCom/definitions/pull/9634
- Closed per-request-search-params proposal: https://github.com/ChessCom/definitions/pull/9592
- Streaming was introduced by CV-494646: https://github.com/ChessCom/explanation-engine/pull/6248

#### For More Human Review

- `explanation-engine/src/features/classification_stages.h:94` sets the streamed stage depths to `{2, target_depth / 2, target_depth}`, carried over unchanged from #6248, so a depth-20 run reports at depths 2, 10 and 20 only.
- `explanation-engine/api_tests/features/analysis_annotations.cpp:224` is tagged `[!shouldfail]` and documents a contract the code does not meet, so it reports as an expected failure rather than passing.
- `explanation-engine/src/features/feature_analysis_annotations.cpp` recomputes threats on every streamed stage, relying on the node's threats registry to make the repeat a cache read rather than a recomputation.
- `explanation-engine/src/version/expected_definitions_version.h:1` pins a definitions commit that must be the squash-merge master SHA before this lands.
- `explanation-engine/src/features/classification_stages.h:98` declares a stack array sized from a compile-time `look_back` and fills it from a parent walk.
