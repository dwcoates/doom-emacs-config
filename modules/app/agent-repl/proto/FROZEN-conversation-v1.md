# FROZEN: `agentshim.conversation.v1` — the whole wire contract

**Everything below is what the shim and sidecar PRODUCE, the store PERSISTS, and
the daemon and webapp CONSUME. Nothing else crosses a wire.**

The rule that decides membership, in the user's words: *agentshim is for
everything in the store, and only everything in the store.*

---

## What is DELETED

Every proto whose job was mapping Claude's SDK or JSONL shapes directly into
protobuf. They exist because a census enumerated a disk format; they are not a
model of anything.

| file | messages | why it goes |
|---|---|---|
| `data/v1/transcript.proto` | 85 | `TranscriptLine`, `LineEnvelope`, the 15 line kinds — a 1:1 mirror of the on-disk key space. The sidecar parses JSONL into the contract below, so the LINE structure never crosses a wire. |
| `data/v1/stream.proto` | 77 | `ClaudeStreamMessage`, `UserMessage`, `AssistantMessage`, `ResultMessage`, `Usage` — a mirror of the SDK stream. The shim converts at the edge. |
| `data/v1/tools.proto` | 101 | `ApiUserMessage`, `ApiAssistantMessage`, and 17 `ContentBlock` arms named for Anthropic's API surface. Replaced by the neutral content model below. |
| `data/v1/journal.proto` | 3 | `JournalRecord` — a disk-format mirror for workflow journals. |
| `data/v1/unknown.proto` | 1 | `UnknownRecord` — survives in substance as `UnknownEntry` below. |

**267 messages deleted.** Whatever the producers need in order to PARSE those
formats is their own internal business, in their own language, and is not a
schema.

---

## `entry.proto` — the durable record

```proto
syntax = "proto3";
package agentshim.conversation.v1;
option go_package = "agentrepl/proto/agentshim/conversation/v1;conversationv1";

// One durable record, whose FIRST question is not "what kind of thing is this"
// but "can we render it at all".
//
// The split is binary at the top for a reason: a page query selects the
// `message` arm and nothing else, so an entry we cannot render has no path into
// a page. That is not a rule the query applies — it is the only arm it can name.
message Entry {
  // Which conversation this belongs to. The vendor's session identity, which is
  // the only thing every producer and the store already agree on.
  string session_id = 1;

  // The store's own monotonic position, assigned at write. Readers order by it
  // and anchor pages by it; producers never supply it.
  uint64 seq = 2;

  // WHICH PRODUCER observed this, for attribution and nothing else. It does not
  // change how an entry is read — an entry means the same thing whichever plane
  // carried it — but when two accounts disagree, this is what says who said
  // what.
  // FIXME: why is Plane in Entry? might be useful within shim+store+sidecar,
  //        i'll grant that, but it seems like my resulting inuition is that
  //        MessageEntry+BookkeepingEntry should be the only things making it
  //        over the wire into the daemon (e.g., the daemon has no need to know
  //        about plane, dedup_key, etc. Let's vet that idea. if it's true, then
  //        we should really have agentshim.internal and agentshim.external, and
  //        external should have a message like Entry that contains a oneof
  //        containing message+bookkeeping, and internal contains everything
  //        else (including UnsupportedEntry, and thus the oneof in Entry looks
  //        like `oneof entry { ExternalEntry external = 1; InternalEntry
  //        internal = 2 }`
  Plane plane = 3;

  // Whether this entry is retained at all. EPHEMERAL entries bypass the store
  // entirely and are never replayed: live-typing deltas and liveness signals
  // exist to be seen once, and persisting them would store millions of
  // fragments of a message the file plane will deliver whole.
  Retention retention = 4;

  // The producer's wall clock at observation, in unix millis. Distinct from
  // `seq`, which orders; this one is what a reader DISPLAYS, and the two can
  // legitimately disagree when a producer is catching up on history.
  int64 produced_at_ms = 5;

  // The key that makes a re-read idempotent. Both producers re-read their
  // sources after a restart, so the same record can arrive twice; the store
  // keeps the first. Empty means the store derives one from the entry's own
  // identity.
  string dedup_key = 6;

  // THE TEST THAT DECIDES THESE ARMS, in one question: if you PRINTED the
  // conversation to read it, would this appear?
  //
  //   yes                      -> message
  //   no, but we understand it -> bookkeeping
  //   we could not tell        -> unsupported
  //
  // Put another way: a MESSAGE is something the conversation CONSISTS OF, and
  // BOOKKEEPING is a fact ABOUT the conversation. Content versus evidence.
  //
  // The reverse readings are the useful ones. Something is NOT a message when
  // nothing in the feed would be missing had it never arrived. Something is NOT
  // bookkeeping when a reader scrolling back would notice a hole where it
  // should have been.
  oneof entry {
    // A record BELONGING to a conversation message — the thing the feed draws
    // and a page counts.
    //
    // It has an identity the user could point at, it accumulates over several
    // records that share that identity, and it is what "ten messages" counts.
    // A context cut and a failure card are messages by this test: a reader must
    // see WHERE the conversation was cut, and a failure card is a thing the
    // user reads and acts on.
    MessageEntry message = 40;

    // A fact ABOUT the session rather than a part of the conversation. We
    // understand it completely and nothing in the feed corresponds to it.
    //
    // It is not uncounted because it is unimportant — it drives the footer, the
    // session state machine, accounting and the turn ledger. It is uncounted
    // because it is not IN the transcript: you cannot point at a turn boundary.
    //
    // Separated from `message` rather than filtered out of it, so a turn
    // boundary has no field capable of naming a message and therefore cannot
    // become a phantom page slot. Misfiling one is not a wrong value — it is an
    // unbuildable record.
    //
    // THE FRONTEND NEVER SEES THESE. Bookkeeping reaches a client only after
    // the daemon has resolved it into a view, which is what makes it evidence
    // rather than content.
    BookkeepingEntry bookkeeping = 41;

    // Something we could not place, preserved verbatim so the decision is
    // reversible. The printing test cannot be applied to it — that is what
    // makes it unsupported rather than either arm above.
    UnsupportedEntry unsupported = 42;
  }
}

// Which producer observed an entry.
message Plane {
  oneof plane {
    // The shim, watching the SDK as it runs. First to know, and the only source
    // for anything that has not been written to disk yet.
    PlaneStream stream = 1;
    // The sidecar, reading what the vendor wrote to disk. Authoritative for
    // conversation content, because it is what the vendor itself recorded.
    PlaneFile file = 2;
    // The daemon, stating something it inferred rather than observed. Held
    // apart so an inference can never be mistaken for a reading.
    PlaneSynthetic synthetic = 3;
  }
}
message PlaneStream {}
message PlaneFile {}
message PlaneSynthetic {}

// Whether an entry is retained.
message Retention {
  oneof retention {
    // Written, replayed, and pageable.
    RetentionDurable durable = 1;
    // Delivered live and never stored. It bypasses the store entirely rather
    // than being written and filtered, so there is no path by which a delta
    // reaches a page.
    RetentionEphemeral ephemeral = 2;
  }
}
message RetentionDurable {}
message RetentionEphemeral {}
```

---

## `message.proto` — records that belong to a message

```proto
// One record BELONGING to one message. Several of these share a message_id and
// fold, in seq order, into the single thing the feed draws.
//
// THE THREE IDS ARE THE SAME ON EVERY RECORD, whatever its payload does with
// the message. That is deliberate: "opens", "updates" and "composes" stop being
// three record SHAPES and become three payloads. An earlier draft split them
// into a composing shape and a message shape, which put the ids at two
// different levels and left the record that CLOSES a message sitting in the arm
// named for identity.
message MessageEntry {
  // The message this record belongs to. Records sharing it fold together, so a
  // consumer needs no correlation pass to attach an update to what it updates.
  string message_id = 1;

  // The feed row that message belongs to, and the value a page groups by.
  //
  // EQUALS message_id when the message is itself a feed row; names the
  // containing message when nested, so a subagent and everything inside it
  // share one value and therefore one page slot.
  //
  // Denormalized on purpose: it is derivable by walking parents, and storing it
  // is precisely why a page of ten messages costs one indexed pass. It MUST
  // equal the root of the parent chain; a write that disagrees is corruption,
  // not a variant.
  string top_level_message_id = 2;

  // The message immediately containing this one — ONE HOP, never the root.
  //
  // EMPTY means the message sits directly in the feed, in which case
  // top_level_message_id is its own id. Absence is the fact itself, not a
  // placeholder: a message whose parent could not be resolved is a producer
  // fault, never a blank.
  //
  // AT A CONTEXT COMPACTION the vendor's physical parent chain is CUT — the
  // boundary line carries no parent — and a separate logical pointer holds the
  // only link back across it. The producer resolves this field from that
  // pointer in that case; reading only the physical chain would make
  // pre-compaction history unreachable by any walk.
  string parent_message_id = 3;

  // Who this message is FROM, resolved by the producer rather than inferred by
  // a reader from which payload arm is set.
  MessageAuthor author = 4;

  oneof payload {
    // ---- Records that OPEN the message they name ----

    // Something a person typed. The opening of a turn.
    UserSaid user_said = 10;

    // Something the agent said: its content blocks, and the usage its response
    // reported.
    AgentSaid agent_said = 11;

    // A permission the agent ASKED FOR. It is a message because the agent
    // really did ask and the user really does answer — it is a conversational
    // act, not a dialog the daemon invented.
    PermissionAsked permission_asked = 12;

    // Something went wrong, stated as a card the user reads and acts on.
    FailureRaised failure_raised = 13;

    // The conversation was CUT here. It is a message because a reader must see
    // where, rather than merely finding the history shorter than they left it.
    ContextCut context_cut = 14;

    // A slash command the DAEMON answered instead of the agent. It carries the
    // command's identity and no text at all: there is no field an argument
    // could ride in, so no surface can leak what the user typed after it.
    DaemonAnsweredCommand daemon_answered_command = 15;

    // Work that DETACHED from the turn and now runs alongside it — a subagent,
    // a background shell, a workflow. It is a feed row naming itself, so a page
    // of ten rows is ten bounded things rather than ten trees.
    DetachedWorkStarted detached_work_started = 16;

    // ---- Records that UPDATE a message already opened ----
    // They carry the same message_id, so attaching them needs no correlation
    // and costs no additional page slot.

    // Output accumulating into detached work already open.
    DetachedWorkProgressed detached_work_progressed = 20;

    // Detached work reached an end, with the outcome it reached.
    DetachedWorkEnded detached_work_ended = 21;

    // The user answered a permission the agent asked for.
    PermissionAnswered permission_answered = 22;

    // A tool the agent called returned. It carries the message_id of the
    // message that MADE the call, so the result folds onto that tool card.
    //
    // `author` therefore stays the AGENT on this record: the field says who the
    // MESSAGE is from, and the message is the agent's response. The vendor
    // files tool results under user-role records, which is the accident this
    // arm exists to not inherit.
    ToolReturned tool_returned = 23;

    // ---- Records that COMPOSE a message, arriving before it is whole ----

    // A fragment of content still arriving. EPHEMERAL by retention, so it is
    // delivered live and never stored: the durable record of the same content
    // is the completed message the file plane writes. Defined in
    // ephemeral.proto, apart from every durable body.
    ContentArriving content_arriving = 30;
  }
}

// Who a message is from.
message MessageAuthor {
  oneof author {
    // A person.
    AuthorUser user = 1;
    // The agent, in the main conversation.
    AuthorAgent agent = 2;
    // A detached agent — a subagent — speaking inside its own work.
    AuthorDetachedAgent detached_agent = 3;
    // The daemon itself, for things it answered or synthesized. Held apart so a
    // card the daemon wrote is never mistaken for something the agent said.
    AuthorDaemon daemon = 4;
  }
}
message AuthorUser {}
message AuthorAgent {}
message AuthorDetachedAgent {
  // The detached work this agent is running as, so its emissions can be routed
  // to the card without a second correlation.
  string detached_work_message_id = 1;
}
message AuthorDaemon {}
```

---

## `content.proto` — the neutral content model

```proto
// THE BLOCK SET IS NARROWED PER SITE, and there is no shared `Content`.
//
// One union with every block arm would make a user message carrying reasoning,
// or a tool result carrying a tool call, representable and meaningless. Three
// unions, each holding only what can legitimately appear where it appears, make
// those unbuildable instead of merely wrong. The cost is a duplicated arm list;
// the repo's own rule is duplicate over share, because mutual exclusivity is
// worth more than deduplication.
//
// NEUTRAL BY DESIGN. The vendor's own content model has seventeen block kinds
// named for its API surface — MCP tool use, server tool use, web search result,
// code execution result, container upload. Those are all the same two facts
// wearing different names: a tool was CALLED, and a tool RETURNED. This models
// the facts and lets the tool's own name carry the rest.

// What a PERSON said. No reasoning and no tool calls, because a person produces
// neither.
message UserContent {
  repeated UserContentBlock blocks = 1;
}

message UserContentBlock {
  oneof block {
    // Words the person typed.
    TextBlock text = 1;
    // An image they pasted or attached.
    ImageBlock image = 2;
    // A block whose kind we do not model. It renders as nothing and is kept so
    // the decision is reversible.
    UnsupportedBlock unsupported = 3;
  }
}

// What the AGENT said in one response: prose, the reasoning behind it, and the
// tools it decided to call, in the order it produced them.
message AgentContent {
  repeated AgentContentBlock blocks = 1;
}

message AgentContentBlock {
  oneof block {
    // Words meant for the reader.
    TextBlock text = 1;
    // Reasoning the agent showed. Held apart from text because a client may
    // legitimately collapse or hide it, and cannot do that if it is text.
    ThinkingBlock thinking = 2;
    // The agent called a tool.
    ToolCallBlock tool_call = 3;
    // A block whose kind we do not model.
    UnsupportedBlock unsupported = 4;
  }
}

// What a TOOL returned. Narrow like UserContent and separate from it on
// purpose: the vendor delivers tool results inside user-role records, and
// reusing the user's own union here would preserve that accident in a schema
// built to erase it. A tool result is not something a person said.
message ToolResultContent {
  repeated ToolResultContentBlock blocks = 1;
}

message ToolResultContentBlock {
  oneof block {
    // The tool's textual output.
    TextBlock text = 1;
    // An image the tool produced — a screenshot, a rendered chart.
    ImageBlock image = 2;
    // A block whose kind we do not model.
    UnsupportedBlock unsupported = 3;
  }
}

message TextBlock {
  string text = 1;
}

message ThinkingBlock {
  // The reasoning itself. May be empty when the vendor redacted it, which is
  // different from the agent not having reasoned — see `redacted`.
  string text = 1;
  // The vendor withheld the content. Stated rather than left as empty text, so
  // a client can say "reasoning was hidden" instead of showing nothing.
  bool redacted = 2;
}

message ToolCallBlock {
  // The id the RESULT will name. This is the only correlation between a call
  // and its result, and it comes from the vendor.
  string tool_call_id = 1;
  // The tool as the agent named it — `Bash`, `Read`, an MCP tool's full name.
  // A plain string because the set is open: MCP servers add tools at runtime,
  // so an enum here would be wrong within a day.
  string tool_name = 2;
  // The arguments, as the agent supplied them. Structured rather than a string
  // because a client renders fields, and re-parsing a string to find them is a
  // second parser that can disagree with the first.
  google.protobuf.Struct arguments = 3;
}

// A tool returning is NOT a block. It is `ToolReturned` in payloads.proto, an
// UPDATE arm on the message that made the call. Modeling it as a block would
// have required an author to own it, and the only author on offer was the user
// — who did not run the tool.

message ImageBlock {
  // Where the image lives. A path or URL rather than bytes: a conversation
  // record is replayed many times, and inlining megabytes into something
  // replayed is how a feed becomes slow.
  string source = 1;
  string media_type = 2;
}

message UnsupportedBlock {
  // What the vendor called it, so a later schema knows what to model.
  string kind = 1;
  // The block entire and verbatim, so nothing is lost by not understanding it.
  google.protobuf.Struct raw = 2;
}
```

---

## `payloads.proto` — the message payload bodies

```proto
message UserSaid {
  // What they typed, and anything they attached. NOT a bare TextBlock: a person
  // pastes images, and a single block could not hold one alongside their words.
  UserContent content = 1;
}

message AgentSaid {
  // The response entire: prose, reasoning, and tool calls, in order.
  AgentContent content = 1;
  // What this response cost, in the ONE canonical token shape. Carried because
  // it is evidence about THIS message; the footer's aggregate figures are
  // resolved by the daemon from many of these and are not this.
  TokenUsage usage = 2;
  // The model that produced it, since a conversation can span models and the
  // cost of a message is not readable without knowing which. It sits beside the
  // usage rather than inside it because it describes the RESPONSE, not the
  // counters.
  string model = 3;
  // Why the agent stopped. A oneof rather than a string because the set is
  // closed and a client branches on it.
  StopReason stop_reason = 4;
}

// A tool the agent called has returned. An UPDATE arm rather than a block,
// carrying the message_id of the message that made the call, so the result
// folds onto the tool card without a correlation pass and costs no page slot.
//
// The vendor delivers these inside user-role records. That is a transport
// accident of its API, and this shape is where it stops.
message ToolReturned {
  // The call this answers, which the agent supplied when it made the call.
  string tool_call_id = 1;
  // What the tool returned, for the reader.
  ToolResultContent content = 2;
  // The tool FAILED. A separate fact from empty content, because a tool that
  // returned nothing and a tool that errored are different things to render.
  bool is_error = 3;
}

message StopReason {
  oneof reason {
    // The agent finished speaking.
    StopEndTurn end_turn = 1;
    // The agent called a tool and is waiting for it.
    StopToolCall tool_call = 2;
    // The response hit its length ceiling.
    StopMaxTokens max_tokens = 3;
    // Something interrupted it.
    StopInterrupted interrupted = 4;
    // The vendor gave a reason we do not model.
    StopUnsupported unsupported = 5;
  }
}
message StopEndTurn {}
message StopToolCall {}
message StopMaxTokens {}
message StopInterrupted {}
message StopUnsupported { string reason = 1; }

message PermissionAsked {
  // What the agent wants to do, so the user can answer knowing what they allow.
  ToolCallBlock requested = 1;
}

message PermissionAnswered {
  oneof answer {
    PermissionAllowed allowed = 1;
    PermissionDenied denied = 2;
    // The question went away without an answer — the turn ended, the session
    // stopped. Distinct from denial, which is a decision the user made.
    PermissionAbandoned abandoned = 3;
  }
}
message PermissionAllowed {
  // The user allowed this kind of call from now on, not just this one.
  bool for_session = 1;
}
message PermissionDenied {
  // What the user said when denying, which the agent reads as feedback.
  string reason = 1;
}
message PermissionAbandoned {}

message FailureRaised {
  // What went wrong, in the user's terms rather than the system's.
  string summary = 1;
  // The underlying detail, for someone who wants it. Held apart from the
  // summary so a client can show one without the other.
  string detail = 2;
  // Whether anything can be done about it, resolved by the producer rather than
  // guessed by a renderer from the text.
  FailureRecovery recovery = 3;
}
message FailureRecovery {
  oneof recovery {
    // It will retry itself; the user does nothing.
    RecoveryAutomatic automatic = 1;
    // The user must act.
    RecoveryUserAction user_action = 2;
    // Nothing can be done; the work is lost.
    RecoveryNone none = 3;
  }
}
message RecoveryAutomatic {}
message RecoveryUserAction { string action = 1; }
message RecoveryNone {}

message ContextCut {
  oneof cut {
    // Everything before this was discarded outright.
    ContextCleared cleared = 1;
    // Everything before this was summarized into what follows.
    ContextCompacted compacted = 2;
  }
}
message ContextCleared {}
message ContextCompacted {
  // The summary that replaced the history, which the feed shows in its place so
  // the cut is not a hole. AgentContent because the summary is the agent's own
  // prose about what it is discarding.
  AgentContent summary = 1;
  int64 tokens_before = 2;
  int64 tokens_after = 3;
}

message DaemonAnsweredCommand {
  // WHICH command, and nothing else. There is deliberately no text field: the
  // argument a user typed after a command must never reach a surface that
  // renders it.
  SessionCommand command = 1;
}

message DetachedWorkStarted {
  // The tool call that spawned it. Every detachment originates from one, and
  // this is what draws the line from the card back to the call.
  string origin_tool_call_id = 1;
  // What to call it in the feed, resolved by the producer.
  string label = 2;
  DetachedWorkKind kind = 3;
}
message DetachedWorkKind {
  oneof kind {
    // A subagent: a whole conversation happening elsewhere.
    DetachedAgent agent = 1;
    // A background shell command.
    DetachedShell shell = 2;
    // A workflow with its own journal.
    DetachedWorkflow workflow = 3;
    // We could not tell, which is stated rather than guessed.
    DetachedUnclassified unclassified = 4;
  }
}
message DetachedAgent {}
message DetachedShell {}
message DetachedWorkflow {}
message DetachedUnclassified {
  // The tool we could not classify, so the card can still name it.
  string tool_name = 1;
}

message DetachedWorkProgressed {
  // Output since the last record, appended in seq order. A DELTA rather than
  // the whole spool, because the whole spool re-sent on every update is how a
  // long-running shell costs more to watch than to run.
  string output = 1;
}

message DetachedWorkEnded {
  oneof outcome {
    DetachedSucceeded succeeded = 1;
    DetachedFailed failed = 2;
    // It was stopped on purpose.
    DetachedCancelled cancelled = 3;
    // We stopped hearing from it. DISTINCT from failure: we do not know that it
    // failed, only that we cannot see it any more.
    DetachedLost lost = 4;
  }
}
message DetachedSucceeded { string summary = 1; }
message DetachedFailed { string summary = 1; }
message DetachedCancelled {}
message DetachedLost {
  // How we concluded it: the file vanished, it went silent, a sweep found it.
  // Carried so "we watched it exit" is distinguishable from "we stopped
  // hearing from it".
  string inference = 1;
}

```

---

## `tokens.proto` — the one canonical token shape

Moved here from `agentshim.frontend.v1`, unchanged. Its own comment already says
the shim translates vendor usage into it at the boundary and stops there — and
that boundary is now this package. `frontend.v1` imports it rather than defining
it, so there is exactly one place this system states what a request cost.

```proto
// THE ONE CANONICAL TOKEN SHAPE, and the only representation in which this
// system states what a request cost.
//
// IT IS ORGANIZED BY ECONOMICS, NOT BY THE VENDOR'S FIELD NAMES. The vendor
// reports three disjoint input counters whose names describe WHERE the tokens
// went, not what they were charged, and reading any one of them as "the cost"
// is the mistake this shape makes unrepresentable.
//
// THE EXPENSIVE SUM IS STRUCTURAL HERE: it is `input_misses` — both of its
// fields, together, because both missed the cache — rather than an addition a
// reader has to know to perform. That is the whole reason for the nesting.
//
// RATES ARE NOT STORED. The cache-hit / cache-write / fresh-input partition is
// three quotients over these same counters, so it is DERIVED at the point of
// use by the daemon (`internal/tokenusage`) and never persisted alongside the
// counters it is computed from.
message TokenUsage {
  // What the prompt cache served: the cheap bucket.
  TokenCacheHits input_hits = 1;
  // What the prompt cache did not serve: the expensive buckets, together.
  TokenCacheMisses input_misses = 2;
  // Generated tokens, including extended thinking where the API includes it.
  // There is no output cache, so this is a plain total with no partition.
  uint64 output_tokens = 3;
}

// The prompt input this request did not have to process, because the prompt
// cache already held it.
message TokenCacheHits {
  // Prompt-prefix tokens served from the prompt cache (vendor
  // `cache_read_input_tokens`). Billed at the cache-read rate.
  uint64 read = 1;
}

// The prompt input this request processed fresh, split by whether processing it
// also placed it in the cache. BOTH FIELDS ARE EXPENSIVE and their sum is the
// figure every cost judgment in this system reads.
message TokenCacheMisses {
  // Tokens processed fresh that entered the cache as they were processed
  // (vendor `cache_creation_input_tokens`). Billed at 1.25x the base input rate
  // — the base price plus the cache-write premium.
  uint64 written = 1;
  // Tokens processed fresh that never entered the cache at all (vendor
  // `input_tokens`). Billed at the base input rate.
  uint64 unwritten = 2;
}
```

---

## `commands.proto` — the closed session-command set

Moved here from `agentshim.frontend.v1`'s `slash-menu.proto`, unchanged, for the
same reason `tokens.proto` moved: `DaemonAnsweredCommand` is a durable record
that names one of these, and a stored record cannot depend on the daemon's
resolved output surface. The MENU that renders the set is a frontend concern;
the set is not. `slash-menu.proto`, `errors.proto` and `prompt-queue.proto` now
import it.

The 30 enum values and the `SessionCommandSpec` option that carries each
command's literal are byte-for-byte what they were — see the file itself rather
than a second copy here. This is one of the few enums the repo permits: the
forbidden case is a STATE enum, and a command identity is not a state of
anything.

---

## `ephemeral.proto` — the one thing that is never written

Its own file, so "nothing in here ever touches the store" is a fact about the
FILE rather than a rule spread across records. It was buried among the durable
payload bodies, which is exactly how its retention went unexamined.

```proto
// A fragment of a message still arriving, delivered live and never stored.
//
// THE ONLY EPHEMERAL MESSAGE IN THIS PACKAGE. It bypasses the store rather than
// being written and filtered out later, so there is no path by which a typing
// delta reaches a page. The durable record of the same content is the completed
// message the file plane writes afterwards, and a consumer REPLACES the preview
// with it rather than appending beside it.
//
// Persisting these was never on the table: a single response produces thousands
// of fragments of one message that arrives whole moments later.
message ContentArriving {
  // Which block of the message this extends.
  uint32 block_index = 1;
  oneof fragment {
    string text = 2;
    string thinking = 3;
    // Tool arguments, arriving as the agent composes them. A string because it
    // is INCOMPLETE JSON until the last fragment lands; typing it as Struct
    // would claim it parses when it does not yet.
    string arguments_json = 4;
  }
}
```

---

## `bookkeeping.proto` — facts about the session

```proto
// A fact about the session that renders as nothing.
//
// THERE IS NO MESSAGE ID ANYWHERE IN THIS MESSAGE. A page is ten messages, so a
// boundary that could name one would silently spend a slot and the user would
// see a short page with nothing logged. Here it has nowhere to put one.
//
// IT IS STILL RETRIEVED, just never COUNTED: the session state machine,
// accounting and the turn ledger read these by SEQ RANGE, which is a different
// query with a different shape from a page.
message BookkeepingEntry {
  oneof kind {
    // The session began, with the configuration it began under.
    SessionBegan session_began = 1;
    // The session ended.
    SessionEnded session_ended = 2;

    // A turn began. Every turn produces one, so an owner here would make every
    // turn in the conversation a phantom page slot.
    TurnBegan turn_began = 3;
    // A turn ended, and how.
    TurnEnded turn_ended = 4;

    // Something is alive and working. It reports liveness, never content.
    Heartbeat heartbeat = 5;

    // How long a response took, retained for analysis on replay. It measures
    // the conversation rather than participating in it.
    ResponseTiming response_timing = 6;

    // A diagnostic about the READER rather than the read. Consumers must never
    // render it as conversation material.
    ProducerDiagnostic producer_diagnostic = 7;

    // The vendor's session identity changed, with what it changed from. Kept so
    // a rotated id is reconciled with the conversation it continues rather than
    // appearing as a new one.
    SessionIdentityChanged session_identity_changed = 8;

    // A usage measurement taken at a turn boundary. Evidence the daemon
    // resolves into the figure a footer shows; the raw measurement never
    // reaches a client.
    UsageObserved usage_observed = 9;
  }
}

message SessionBegan {
  string model = 1;
  string cwd = 2;
}
message SessionEnded {
  oneof reason {
    SessionEndedNormally normally = 1;
    SessionEndedByError by_error = 2;
    // The harness went away underneath it.
    SessionEndedByShutdown by_shutdown = 3;
  }
}
message SessionEndedNormally {}
message SessionEndedByError { string detail = 1; }
message SessionEndedByShutdown {}

message TurnBegan {
  // The submission this turn answers, so a turn can be tied to the prompt that
  // caused it without a correlation pass.
  string turn_id = 1;
}
message TurnEnded {
  string turn_id = 1;
  oneof outcome {
    // The turn completed on its own.
    TurnCompleted completed = 2;
    // The user stopped it. An ACCUSATION the evidence must support: only a
    // user-commanded stop the producer saw acknowledged sets this.
    TurnInterrupted interrupted = 3;
    // It stopped and we do not know why. Distinct from interrupted, which
    // claims a cause.
    TurnEndedUnexplained unexplained = 4;
  }
}
message TurnCompleted {}
message TurnInterrupted {}
message TurnEndedUnexplained { string inference = 1; }

message Heartbeat {
  // What is alive, so a footer can say which work is still running.
  repeated string live_work_ids = 1;
}

message ResponseTiming {
  string message_id = 1;
  int64 first_token_ms = 2;
  int64 total_ms = 3;
}

message ProducerDiagnostic {
  string operation = 1;
  string detail = 2;
}

message SessionIdentityChanged {
  string previous_session_id = 1;
  string reason = 2;
}

message UsageObserved {
  // The turn this measurement was taken at the boundary of.
  string turn_id = 1;
  // The measurement, in the same canonical shape a response carries, so a turn
  // total and a message cost are the same units and can be compared without a
  // conversion nobody would remember to write.
  TokenUsage usage = 2;
}
```

---

## `unsupported.proto` — what we could not place

```proto
// An entry we do not render, held for posterity.
//
// SINGULAR because one Entry carries one of these. The arms are one concept —
// "unsupported" — split only because they imply different follow-ups.
//
// THIS ARM IS WHAT MAKES EAGER CONVERSION SAFE. Anything a producer could not
// map still lands durably and whole, so converting at the edge costs no
// fidelity: the decision stays reversible from stored data.
//
// The payloads are deliberately thin. They exist for debugging and for a future
// schema to mine; when one is genuinely needed, its shape is expanded then
// rather than guessed at now.
message UnsupportedEntry {
  oneof entry {
    // A fact specific to one vendor, understood but not carried into a
    // vendor-agnostic feed. The follow-up is a CONVERTER, if it turns out to be
    // portable after all.
    VendorSpecificEntry vendor_specific = 1;

    // A record we PARSED but do not MODEL. The follow-up is a MODEL.
    UnknownEntry unknown = 2;

    // A record we could not PARSE at all — a failure rather than a gap.
    UnparsedEntry unparsed = 3;
  }
}

message VendorSpecificEntry {
  // What the vendor called it.
  string kind = 1;
  // The record entire and verbatim.
  google.protobuf.Struct raw = 2;
}

message UnknownEntry {
  // The discriminator we did not recognize, and where we read it from, so a
  // later schema knows what to model and where to look.
  string discriminator = 1;
  string discriminator_field = 2;
  google.protobuf.Struct raw = 3;
}

message UnparsedEntry {
  // Where it came from and why it failed, so the failure is investigable rather
  // than merely counted.
  string source = 1;
  uint64 offset = 2;
  string parse_error = 3;
  // The bytes, verbatim.
  string raw = 4;
}
```

---

## What is NOT here, deliberately

- **View resolutions** — topbar, footer, sidebar, token menus, gates, merge
  status. They stay in `agentshim.frontend.v1`, because they are resolved by the
  daemon for a component to render verbatim and are never stored.
- **Transport plumbing** — `StoreWrite`, `Subscribe`, `ReplayRequest`,
  `MessagePageRequest`, `ShimHello`. They stay in `agentshim.core.v1`, because
  they carry entries rather than being entries.
- **Anything a producer needs in order to PARSE** its source. That is the
  producer's internal business, in its own language, and is not a schema.
