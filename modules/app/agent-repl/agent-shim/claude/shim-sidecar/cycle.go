// cycle.go is the sidecar's production cycle and the invariant that governs it.
//
// THE STORE-UNREACHABLE INVARIANT. Every production cycle BEGINS with a
// successful GetSidecarCursors. Any store error — a refused or unreachable
// cursor read, a refused or unreachable batch write — SUSPENDS ALL PRODUCTION
// until a full recover-cursors-then-rescan succeeds. While production is
// suspended the sidecar reads nothing, loudly.
//
// WHY IT IS STRUCTURAL RATHER THAN A RETRY AROUND A SPECIAL CASE. A tailer's
// read position can only ever come from a cursor the store handed us, because:
//
//   - `cursors` is nil unless a cycle recovered it, and only `recover` sets it;
//   - `watchTargets` is the only thing that builds a tailer, and it asserts
//     that. TWO callers reach it and no third may be added without extending
//     this list: `rescan`, the full periodic enumeration, and `discoverChanged`,
//     the per-poll directory-change probe that exists so a NEW transcript is
//     read within one poll rather than one rescan. The contract was once
//     spelled "rescan is the only thing that builds a tailer"; the probe
//     EXTENDS it explicitly rather than slipping around it, and it is bound by
//     every clause below exactly as the rescan is;
//   - suspension DROPS every tailer, so a resumed cycle rebuilds each one from
//     the position this cycle's store handed it — never from a remembered one;
//   - after construction a tailer advances only through Commit, which the poll
//     loop calls only on a durable WriteBatch success;
//   - every periodic action runs through `producing`, so nothing polls, sweeps
//     or rescans while production is suspended.
//
// Boot ordering is therefore irrelevant by construction: a store that starts
// late simply means the first cycle has not begun yet, which is the same state
// a store that dies mid-run puts the sidecar into, handled by the same code.
//
// WHAT THIS REPLACES, AND WHY IT MATTERS. Cursor recovery once ran exactly at
// boot and its failure path was a silent cold start: the sidecar logged
// "cursor recovery failed (starting cold)" and re-read every watched file from
// offset 0. That is a fallback masking a down dependency, and it re-ingested
// whole conversations in production. The honest behavior while the store is
// unreachable is producing NOTHING, loudly.
//
// "COLD" SURVIVES ONLY AS ITS TRUTHFUL CASE: a REACHED store that genuinely
// holds no cursor for a file. That is the newly-discovered-transcript backfill
// and it reads from offset 0 exactly as it should.
package main

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"math/rand"
	"os"
	"path/filepath"
	"sort"
	"sync"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/identity"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"
	"agentrepl/shim-claude-sidecar/internal/storeclient"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// The recovery ladder: an immediate first attempt, then 250ms doubling to a
// ceiling the ladder holds FOREVER. There is no attempt budget and no terminal
// state — production that is suspended is production that is being recovered,
// for as long as the store stays unreachable.
//
// BOTH RUNGS ARE OVERRIDABLE, exactly as every other window in this process is
// (--poll-interval, --rescan-interval, --unowned-spool-window, the four
// --stale-* windows). They were the only ones that were not, which meant the
// outage subjects — which run a REAL sidecar process, so the injected clock the
// unit tests drive does not reach them — had no way to exercise the ladder
// except by waiting out real rungs of it.
const (
	recoverBackoffMin = 250 * time.Millisecond
	recoverBackoffMax = 10 * time.Second
)

// recoverJitterFraction spreads each armed delay over ±20% of its backoff, so a
// fleet of sidecars that lost the same store does not rediscover it in lockstep.
const recoverJitterFraction = 0.2

// recoverTick is how often Run asks whether a recovery attempt is due.
//
// THE LADDER IS A DEADLINE THE LOOP COMPARES AGAINST, NOT AN EVENT SOMEONE MUST
// REMEMBER TO SCHEDULE. It used to be one re-armable timer, which made the whole
// ladder depend on a Stop/drain/Reset dance executing correctly on every
// transition: a single lost re-arm silenced recovery entirely, with no state
// left in the process saying a recovery was owed. That is exactly what a
// production store bounce produced. A deadline cannot be lost — the ticker that
// checks it runs unconditionally for the life of the process — so the worst a
// mishandled transition costs is one tick of latency.
const recoverTick = 50 * time.Millisecond

// rpcTimeout bounds one store call. A call that cannot finish inside it is a
// store that is not answering, which is a suspension like any other.
const rpcTimeout = 30 * time.Second

// shutdownSettle is how long a store call that is ALREADY ON THE WIRE is given
// to answer after a termination signal, before its context is cancelled.
//
// CANCELLING A SENT WRITE DOES NOT WITHDRAW IT. The store's transaction is its
// own: by the time this process cancels, the batch may already be committing,
// and the server learns the client is gone only when net/http notices the
// closed connection — which, for a process that is exiting, is AFTER it has
// exited. So an instant cancel does not undo the write; all it destroys is
// THIS PROCESS'S KNOWLEDGE of whether the write landed, and it leaves the store
// still committing a batch nobody is waiting for. Waiting a beat for the answer
// costs nothing and buys the truth: a durable success advances the cursor
// normally, and a genuine failure is stated as one.
//
// IT IS SHORT, BECAUSE A WEDGED STORE MUST NOT HOLD THE PROCESS. A healthy
// WriteBatch answers in single-digit milliseconds, and the latest answer ever
// observed after a signal was ~50ms (a store finishing a withdrawn commit under
// a deliberately overcommitted box); this is a 5x margin on that, and it spends
// under a tenth of the 3s a signalled sidecar's shutdown is held to
// (integration/sigterm_wedged_write_test.go), so a store that has stopped
// answering still lets the process leave promptly.
const shutdownSettle = 250 * time.Millisecond

// watched is one file the sidecar is currently reading.
type watched struct {
	target discover.Target
	tailer *tail.Tailer
	// ctx is the attribution the tailer hands the converter. It is kept here
	// because ONE OF ITS FACTS IS NOT KNOWN AT WATCH TIME: whether the spawn
	// behind this file was BACKGROUNDED is read off a launch in ANOTHER file,
	// which the reader may not have reached yet. See refreshSpawnFacts.
	ctx *tail.Context
	// vanished records that the file has gone missing, so the disappearance is
	// stated once rather than on every poll of a file that is still absent.
	vanished bool
	// terminated records that this file's OWN terminal was read off it (a
	// spool's `[exited with code N]` / `[killed]`). A file that disappears
	// afterwards took nothing with it, which is what keeps a reaped task spool
	// out of the WARNINGs.
	terminated bool
}

// missClearInterval bounds how long a remembered identity miss can outlive a
// link write the directory fingerprint failed to see (see rescan).
const missClearInterval = 5 * time.Minute

type sidecar struct {
	// lastMissClear is when rescan last ran the unconditional Refresh.
	lastMissClear time.Time

	options Options
	store   *storeclient.Client
	disc    *discover.Discoverer
	tracker *stale.Tracker
	log     *logging.Bound

	// shutdown is the context every store rpc is tied to, so a SIGTERM that
	// arrives while a call is in flight ends THAT CALL rather than being read
	// only once rpcTimeout expires — but it ends it after shutdownSettle, not
	// instantly, because a write already on the wire cannot be recalled.
	//
	// NOTHING IS LOST EITHER WAY, and this is the whole reason the shutdown may
	// be abrupt: the store commits a batch's records and its cursor advance in
	// ONE transaction, so it ends up holding both or neither. A write this
	// process never got an answer for is therefore not a torn state, it is an
	// UNKNOWN one — and the next boot resolves it by reading whichever cursor
	// the store holds and re-reading the durable bytes past it into the
	// identical deterministic write ids / upsert keys.
	//
	// Nil only in tests that drive the cycle's steps directly.
	shutdown context.Context

	watchers map[string]*watched // by resolved path
	owners   *ownerIndex
	held     *heldSpools
	// identity resolves a transcript's vendor session id to the conversation's
	// ORIGINAL id — the shim-minted main AgentId — through the identity files
	// the shim writes. It is refreshed on the rescan interval, beside discovery.
	identity *identity.Index
	// rotationHeld is every main transcript held UNREAD because its book is
	// owed by a rotation link the shim has not written yet (rotation.go), by
	// resolved path. Process-scoped: the hold is a fact about the disk, which a
	// store outage does not change.
	rotationHeld map[string]discover.Target

	// settling holds the files whose converter READ a run's own terminal in the
	// batch currently in flight. The tracker is only told once that batch is
	// DURABLE: a run untracked against a write that never committed would be
	// re-read, restate its terminal, and be neither tracked nor concludable in
	// between.
	settling map[string]string // resolved path -> run activity id

	// stopped holds the tasks a person stopped whose spool was not being read
	// when the stop arrived, with the instant the stop was observed. ONE VALUE
	// PER TASK: a stop is a single fact and restating it changes nothing.
	//
	// IT IS NOT DROPPED ON SUSPENSION, unlike `settling`: a pending stop is a
	// fact read out of a transcript, not a promise about a write in flight, and
	// the transcript's own cursor did not advance either — so re-reading will
	// report it again and the map absorbs the repeat.
	stopped map[string]int64

	// cursors is CYCLE-SCOPED: recovered as the first act of every production
	// cycle and dropped the moment production is suspended, so a tailer can
	// never be built from a stale — or absent — recovery.
	cursors map[string]*storev1.CursorState // by resolved path; nil while suspended
	// parked holds the files the store REFUSED an invalid_request for (ruling
	// R-S2), by resolved path. Nothing more is read from one for the LIFE OF
	// THE PROCESS: the refusal is a PRODUCER DEFECT, so re-reading the same
	// durable bytes re-mints the same rejected batch forever — a tight
	// identical replay loop that makes no progress and drowns the log.
	//
	// IT IS ONE FILE, NEVER THE PLANE: a malformed conversion of one transcript
	// says nothing about any other file, so every other tailer keeps reading.
	// The parked file's cursor stays exactly where the store has it, so a fixed
	// sidecar resumes from the same byte.
	//
	// IT IS PROCESS-SCOPED RATHER THAN CYCLE-SCOPED, unlike the tailers: an
	// outage in between does not make the defect go away, so a suspension that
	// drops every tailer must not quietly un-park the file the store already
	// told us it cannot accept.
	parked map[string]bool
	// residueWithheld tallies the RESIDUE records the write path classified and
	// did not store, keyed by the tailed file's `dev:inode` identity and then by
	// residue label. Inferred batches name no file and tally under the empty
	// key. It is process-scoped, like the withholding itself: the reader states
	// it as one summary per file at the end of the startup catch-up window.
	residueWithheld map[string]map[string]int
	// shapeCatalogued is every residue SHAPE HASH this process has already
	// contributed an observation for. It is what makes `new_shapes` mean "first
	// seen by this process" rather than "seen again", and it is process-scoped
	// for the same reason the tally is: a cycle that dropped its tailers did not
	// make the vendor stop emitting the shape.
	shapeCatalogued map[string]bool
	// newShapes counts, per tailed file's `dev:inode` identity, the shapes this
	// process saw for the FIRST time. Inferred batches name no file and count
	// under the empty key, exactly as the withheld tally does.
	newShapes map[string]int
	// defects counts how many times each file's OWN defect has now been
	// observed, keyed by the file's `dev:inode` identity so a rename does not
	// buy the same defect a fresh count.
	//
	// A PARK ALREADY STATES THE DEFECT ONCE, and for a file that stays parked
	// that is the end of it. But a park is not permanent: `rekeyRotations`
	// UN-PARKS a file whose refusal was a book move the shim's link files have
	// since contradicted, and if the re-read is refused all over again the
	// defect is restated. When the identity that decides the book oscillates,
	// that is a per-poll record for a condition that never changes — the exact
	// drowning the park exists to prevent, arriving through the un-park door
	// instead. So a repeat of the SAME defect for the SAME file is restated
	// only on powers of two, and every record carries the running count: a
	// defect that never stops being true stays visible, logarithmically,
	// rather than becoming the log's entire content.
	defects map[string]*fileDefect
	// watchedThisPass counts the files ONE rescan started watching, so the
	// pass can state a count rather than a record per file. The change probe
	// increments it too — `watch` is shared — but only the rescan reads it, and
	// the rescan zeroes it as its first act, so what it reports is always the
	// rescan's own tally.
	watchedThisPass int
	// rewound remembers which files have had their one boot rewind, so the
	// bounded backward scan happens once per file per process rather than on
	// every reconnect.
	//
	// KEYED BY THE FILE'S IDENTITY, NOT ITS PATH. "Once per file per boot" is a
	// statement about a FILE, and the vendor may rename one under us at any
	// moment: keyed by path, a rename bought the same file a second rewind and
	// a second re-read of its in-progress turn.
	rewound map[string]bool
	// rewindWalked and rewindSkipped count what the boot rewind DID with the
	// corpus: how many transcripts could still have been carrying a turn in
	// flight and were scanned backward, and how many were at rest and were
	// watched from their cursor with no backward scan and no re-read. They are
	// stated as one INFO summary at the catch-up edge, beside the other
	// summaries the boot walk owes, because the per-file decision is verbose
	// and thousands of verbose lines say nothing about the SHAPE of the walk.
	rewindWalked  int
	rewindSkipped int
	// Workspace attribution is read once per session's main transcript. Many
	// sidechain files can share it, so rediscovery reuses the proven identity.
	workspaceBySession map[string]workspaceAttribution
	workspaceFailures  map[string]string

	nextAttemptAt  time.Time
	attempting     bool
	backoff        time.Duration
	attempts       int
	suspendedSince time.Time
	bootSwept      bool
	// processStartMs is when production first began (the first successful cycle),
	// captured off the cycle's own clock. It is the boundary between an item that
	// was ALREADY stale in the historical corpus a restart re-derives — backlog,
	// summarized once per class as startup catch-up — and one that arose while
	// the sidecar ran steady-state, which is stated per item. Zero until the
	// first cycle sets it; a store bounce that re-enters beginCycle leaves it
	// where it is, so the boundary never moves.
	processStartMs int64
	// catchupSpools and catchupWorkspaces tally the backlog demotions and
	// backlog workspace-attribution failures of ONE rescan pass, so the pass
	// states one summary per class rather than one warning per file — the same
	// flood the discover-meta holds already leveled, seen here through the
	// hold-expired and resolve-transcript-workspace paths.
	catchupSpools     catchupTally
	catchupWorkspaces catchupTally
	// catchupBookConflicts tallies the legacy book-conflict entries the store
	// SKIPPED during startup catch-up: a corrected converter re-ingesting the
	// pre-existing corpus writes an already-stored key under its now-right book,
	// which the store keeps-and-skips. It is expected on a restart re-scan, so it
	// is summarized once per pass rather than warned per entry, exactly as the
	// spool and workspace backlogs are; a skip in steady state is warned instead.
	catchupBookConflicts catchupTally
	// drainedPass records that one poll pass walked every watcher to
	// completion, and catchupEnded that the startup catch-up window has been
	// closed off that fact. Both are process-lifetime latches: the boot walk
	// happens once, and a later store bounce must not reopen a window whose
	// summaries were already stated.
	drainedPass  bool
	catchupEnded bool
	// pass is the poll walk currently in progress, spread across as many ticks
	// as its slice bound needs. Nil between passes: the next tick opens a fresh
	// one over whatever is watched then.
	pass *pollPass
	// suspensionStated remembers that the WARNING opening this outage has been
	// written. THE OUTAGE IS STATED ONCE, and a process that starts with no
	// store is in an outage exactly like one whose store died mid-run — so the
	// record belongs to the first FAILED attempt rather than to a transition
	// between two live cycles, which is the transition a fresh process never
	// makes and which therefore used to leave a boot outage entirely silent.
	suspensionStated bool

	// backoffMin and backoffMax are the recovery ladder's floor and ceiling,
	// resolved once at construction from Options so nothing downstream has to
	// know that zero means "the package default".
	backoffMin time.Duration
	backoffMax time.Duration

	// now and jitter are the cycle's clock and its backoff spread, injectable so
	// the ladder is tested by advancing a fake clock rather than by waiting.
	now    func() time.Time
	jitter func(time.Duration) time.Duration
	// bootTimeMs is the machine boot time, injectable for the boot sweep's test.
	bootTimeMs func() int64
}

// pollPass is ONE walk over every watcher, which on a boot walk is minutes of
// reading and therefore cannot be one tick's work.
//
// WHY A PASS IS SLICED AT ALL. `pollAll` used to walk the whole watched set to
// completion inside one tick. During a restart's corpus walk that tick lasted
// minutes, and nothing else on the poll timer ran while it did: not the change
// probe, so no new transcript was discovered, and not the first poll of a file
// that had been discovered, so nothing new was read either. Realtest 9, sweep
// rt-run37: a fresh workspace's turn concluded at 23:46:41, inside a boot walk
// that ran from 23:44:21 to 23:46:45, and its answer rows reached the store at
// ~23:47:44. A pass that yields the tick back keeps discovery and a new file's
// first read on their own one-second clock however large the corpus is.
//
// THE PASS IS THE UNIT `drainedPass` IS ABOUT, not the tick. The startup
// catch-up window closes on a pass that walked EVERY watcher, and that fact now
// spans slices: `seen` is the roster the pass enrolled and `pending` what it has
// left, so the window closes when the last of them has been polled and not one
// tick earlier.
type pollPass struct {
	// pending are the paths this pass has still to poll, in walk order.
	pending []string
	// seen is every path this pass has enrolled, polled or not. It is what makes
	// a mid-pass discovery distinguishable from a watcher the pass already
	// walked.
	seen map[string]bool
}

func newSidecar(options Options, log *logging.Bound) *sidecar {
	s := &sidecar{
		options:            options,
		store:              storeclient.New(options.StoreSocket, log.With(logging.Context{Component: "storeclient"})),
		disc:               discover.New(options.ConfigRoots, options.SpoolRoot, log.With(logging.Context{Component: "discover"})),
		tracker:            stale.New(options.Stale, log.With(logging.Context{Component: "stale"})),
		log:                log,
		watchers:           map[string]*watched{},
		settling:           map[string]string{},
		stopped:            map[string]int64{},
		parked:             map[string]bool{},
		residueWithheld:    map[string]map[string]int{},
		shapeCatalogued:    map[string]bool{},
		newShapes:          map[string]int{},
		defects:            map[string]*fileDefect{},
		rewound:            map[string]bool{},
		workspaceBySession: map[string]workspaceAttribution{},
		workspaceFailures:  map[string]string{},
		rotationHeld:       map[string]discover.Target{},
		// A fresh sidecar is simply a sidecar whose first cycle has not begun
		// yet, with its first attempt due immediately. That is all "boot" means.
		now:        time.Now,
		jitter:     jitterBackoff,
		bootTimeMs: bootTimeMillis,
	}
	s.backoffMin, s.backoffMax = resolveBackoff(options.RecoverBackoffMin, options.RecoverBackoffMax)
	s.identity = identity.New(options.StateDir, log.With(logging.Context{Component: "identity"}))
	s.owners = newOwnerIndex(log.With(logging.Context{Component: "owner"}))
	s.held = newHeldSpools(options.UnownedSpoolWindow, log.With(logging.Context{Component: "held"}))
	s.suspendedSince = s.now()
	s.nextAttemptAt = s.now()
	return s
}

// Run drives the cycle until a termination signal.
//
// THE SIGNAL CANCELS IN-FLIGHT STORE CALLS. The cycle runs on this goroutine,
// so a sidecar blocked inside a WriteBatch could not reach this select until
// the call returned — which for a wedged store meant waiting out rpcTimeout
// before shutdown even began. The signal is therefore watched on its own
// goroutine, and all it does is cancel the context every store rpc derives
// from: the blocked call returns at once, the cycle unwinds through its normal
// paths, and this loop leaves by ctx.Done() with the usual shutdown record.
// catchupOperations are the operations whose per-item INFO records are the
// BOOT WALK RESTATING THE CORPUS rather than news.
//
// A restarted sidecar re-derives the owner's whole historical corpus from
// files: every transcript is rewound, every watcher picks up its restored
// bytes, every spawning call in those bytes is re-read, every unclaimed spool
// is re-held, and every long-dead run is re-concluded. One boot generation on
// the owner's machine held 9,337 `launch`, 9,337 `record-spawn`, 7,128
// `hold-spool`, 6,748 `tail-pickup`, 6,172 `boot-rewind` and 4,789
// `lost-policy` INFO records, and rolled the 64 MB durable log five times over.
// None of it is news: it is the same inverted-pyramid flood the rescan-driven
// holds and the LOST tracker already level, arriving through the operations
// that read the corpus — and, with them, the per-spool claim and skip decisions
// a restart re-derives for every spool on disk.
//
// During catch-up each is stated at DEBUG and tallied; the window's end states
// one INFO `catchup-summary` per operation carrying the count. After the window
// closes each is INFO per record exactly as before, because then it IS news.
var catchupOperations = []string{
	"launch",
	"record-spawn",
	"hold-spool",
	"spool-claim",
	"spool-skip",
	"tail-pickup",
	"boot-rewind",
	"lost-policy",
}

func (s *sidecar) Run(stop <-chan os.Signal) error {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	s.shutdown = ctx
	// Buffered so the watcher never blocks, and read only after ctx.Done() —
	// which happens-after the send — so the name is there without a race.
	signalled := make(chan os.Signal, 1)
	watching := make(chan struct{})
	defer close(watching)
	go func() {
		select {
		case sig := <-stop:
			signalled <- sig
			cancel()
		case <-watching:
		}
	}()

	pollT := time.NewTicker(s.options.PollInterval)
	rescanT := time.NewTicker(s.options.RescanInterval)
	sweepT := time.NewTicker(s.options.RescanInterval)
	// The ladder's heartbeat. It runs for the life of the process, whatever
	// production is doing, so suspended production is always being recovered.
	attemptT := time.NewTicker(recoverTick)
	defer pollT.Stop()
	defer rescanT.Stop()
	defer sweepT.Stop()
	defer attemptT.Stop()

	for {
		select {
		case <-ctx.Done():
			s.log.With(logging.Context{Operation: "shutdown"}).Log("received signal=%s; shutting the sidecar down", <-signalled)
			s.store.Close()
			return nil
		case <-attemptT.C:
			s.attemptDue()
		case <-rescanT.C:
			s.producing(s.rescan)
		case <-pollT.C:
			// DISCOVERY FIRST, THEN THE READ, so a file the probe finds on this
			// tick has its first records picked up on this same tick rather than
			// one poll later. The two go through `producing` SEPARATELY: the
			// probe can itself suspend production (a store that cannot answer for
			// a newly discovered file's cursor), and pollAll must not run against
			// the cursors that suspension just dropped.
			s.producing(s.discoverChanged)
			s.producing(s.pollAll)
			s.endCatchupOnFirstDrainedPass()
		case <-sweepT.C:
			s.producing(s.sweep)
		}
	}
}

// producing runs one unit of work, and only while production is live. EVERY
// periodic action goes through here, so "reads while the store is unreachable"
// is one decision in one place rather than a condition each caller remembers.
func (s *sidecar) producing(work func()) {
	if s.cursors != nil {
		work()
	}
}

// requireCursors asserts the invariant every production step depends on: no
// file is read, and no tailer is built, without this cycle's recovered cursors
// in hand. A violation is a bug in this cycle, so it fails hard rather than
// quietly cold-starting.
func (s *sidecar) requireCursors(what string) {
	if s.cursors == nil {
		panic(fmt.Sprintf("sidecar: %s with production suspended — a tailer's position may only come from a cursor this cycle's store handed us", what))
	}
}

// attemptDue runs the LADDER: it attempts recovery whenever production is
// suspended and the armed deadline has passed, and does nothing otherwise.
func (s *sidecar) attemptDue() {
	if s.cursors != nil || s.attempting || s.now().Before(s.nextAttemptAt) {
		return
	}
	s.attempt()
}

// attempt tries to begin a production cycle. It is SINGLE-FLIGHT so two ticks
// cannot race two cursor recoveries onto one cycle.
func (s *sidecar) attempt() {
	s.attempting = true
	defer func() { s.attempting = false }()
	if err := s.beginCycle(); err != nil {
		if s.interrupted(err) {
			// The shutdown cancelled the recovery. Nothing was read and nothing
			// was written, so there is no outage to open and no ladder to climb.
			s.log.With(logging.Context{Operation: "shutdown"}).Log(
				"shutdown interrupted a cursor recovery; the next boot recovers it")
			return
		}
		s.attempts++
		s.backoff = nextBackoff(s.backoff, s.backoffMin, s.backoffMax)
		delay := s.jitter(s.backoff)
		// A process whose FIRST cycle never began is a suspended process, and
		// its outage is opened here: nothing else has a transition to report.
		s.stateSuspension("recover-cursors", err)
		// THE LADDER'S LEVELS DESCEND WITH THE OUTAGE. Reading no files IS the
		// whole file plane stopped, so every attempt is recorded — but the FIRST
		// refusal of an outage is the one an operator must not miss and is an
		// ERROR, while the attempts after it are the same known outage still
		// running and are WARNINGS. Both carry the attempt ordinal and the delay
		// armed before the next try, so the ladder's progress is filterable
		// without reading a sentence, and neither is verbose: an outage that
		// only showed up with verbose emission on is an outage nobody sees.
		level := "warn"
		if s.attempts == 1 {
			level = "error"
		}
		s.log.With(logging.Context{
			Operation: "recover-cursors", Level: level,
			Attempt: logging.Attempt(s.attempts), BackoffMs: logging.BackoffMs(delay),
		}).Log("recovery attempt %d failed, retrying in %s while reading no files: %v", s.attempts, delay, err)
		s.nextAttemptAt = s.now().Add(delay)
		return
	}
}

// stateSuspension writes the ONE WARNING that opens an outage, and only the
// first time it is owed. Both entrances to suspension come through here — a
// live cycle losing its store, and a first cycle that never began — so "the
// outage is stated once" is a property of one function rather than a rule two
// call sites have to keep agreeing on.
func (s *sidecar) stateSuspension(operation string, cause error) {
	if s.suspensionStated {
		return
	}
	s.suspensionStated = true
	// The caller owns the causal error; this record owns the transition, and
	// names the dependency the file plane is now waiting on.
	s.log.With(logging.Context{
		Operation: "production-suspended", Level: "warn",
		StoreSocket: s.options.StoreSocket, Attempt: logging.Attempt(s.attempts),
	}).Log("production suspended after operation=%s; reading no files until a full cursor recovery succeeds: %v", operation, cause)
}

// beginCycle is the first act of every production cycle: recover the cursors,
// then build this cycle's tailers from them, and only then start reading. Any
// failure leaves production suspended with nothing read, which is the whole
// point — a cycle that could write but had no recovery state is exactly the
// cold start this design removes.
func (s *sidecar) beginCycle() error {
	ctx, cancel := s.rpcContext()
	defer cancel()
	cursors, err := s.store.Cursors(ctx, "")
	if err != nil {
		// storeclient owns the causal record with its rpc and refusal detail.
		return fmt.Errorf("recovering cursors: %w", err)
	}
	s.cursors = indexCursorsByFileID(cursors)
	// The outage is over, so the next one gets its own opening WARNING.
	s.suspensionStated = false
	// THE CATCH-UP BOUNDARY IS THE FIRST PRODUCTION CYCLE. Everything already on
	// disk when reading first begins is historical backlog; a stale conclusion
	// about it is a catch-up summary, not a per-item warning. It is set once — a
	// store bounce re-enters this path, and moving the boundary then would
	// reclassify runs a later cycle already summarized. Both the LOST tracker
	// (its own package) and the rescan-driven paths key on the same instant.
	if s.processStartMs == 0 {
		s.processStartMs = s.now().UnixMilli()
		s.tracker.SetProcessStart(s.processStartMs)
	}
	s.log.With(logging.Context{Operation: "recover-cursors", StoreSocket: s.options.StoreSocket}).Log(
		"production cycle begins: recovered %d cursor(s) from the store", len(s.cursors))

	// Reading may begin now, and not one statement earlier.
	s.rescan()

	// The boot sweep is about MACHINE boot rather than about this cycle, so it
	// runs once per process — but its conclusions are records, so it can only
	// run once there is a store to write them to.
	if !s.bootSwept {
		s.bootSwept = true
		s.emit("boot sweep", s.lostEntries(s.tracker.BootSweep(s.bootTimeMs(), s.now().UnixMilli())))
	}
	s.reportResumed()
	return nil
}

// suspend stops all production and arms an immediate recovery attempt. It is
// idempotent, because one poll pass can surface the same dead store through
// several failed writes.
func (s *sidecar) suspend(operation string, cause error) {
	if s.cursors == nil {
		return
	}
	s.cursors = nil
	// A terminal whose batch never committed is a terminal the store never saw,
	// so the promise to untrack its run goes with the cycle that made it.
	s.settling = map[string]string{}
	// EVERY TAILER GOES WITH THE CURSORS. A tailer that outlived its cycle would
	// resume from a position the NEXT cycle's store never handed us, which is
	// the one thing the invariant forbids.
	s.watchers = map[string]*watched{}
	// THE PASS GOES WITH THE WATCHERS IT WAS WALKING. Its roster names tailers
	// that no longer exist, and a resumed cycle rebuilds every one of them from
	// the position the new cycle's store hands it — so the walk starts over,
	// which is also why an outage cannot close the catch-up window on a pass
	// that never finished.
	s.pass = nil
	s.store.Close()
	s.suspendedSince = s.now()
	s.attempts = 0
	s.backoff = 0
	s.nextAttemptAt = s.now()
	s.stateSuspension(operation, cause)
}

// noteStoreErr suspends production for any store error. There is no connection
// to consult any more: a refusal and a transport failure both mean this cycle
// cannot be trusted to have a store behind it, and the invariant's price for
// being wrong is re-ingesting a conversation.
func (s *sidecar) noteStoreErr(operation string, err error) {
	if err == nil {
		return
	}
	if s.interrupted(err) {
		// The shutdown cancelled the call; the store never said anything is
		// wrong, and the caller has already stated the replay.
		return
	}
	if _, invalid := storeclient.InvalidRequest(err); invalid {
		// AN INVALID REQUEST IS NOT AN OUTAGE (ruling R-S2). The store was
		// reachable, answered, and rejected THESE BYTES; suspending the whole
		// file plane over one malformed batch would stop every other file for a
		// defect in one, and the retry the suspension exists to arrange cannot
		// help. The caller decides what to park; the error is still returned,
		// never swallowed.
		return
	}
	s.suspend(operation, err)
}

// park stops reading ONE file after the store refused its batch as an
// invalid_request, and states the defect once.
//
// THE RECORD IS THE WHOLE POINT. A batch the store can never accept is a bug in
// this producer, and the only way anyone finds it is the field the store named,
// the write ids of the batch it rejected, and the file position they were read
// at — so all three ride dedicated keys rather than prose.
func (s *sidecar) park(path string, w *watched, result tail.PollResult, field string, cause error) {
	s.parked[path] = true
	count, state := s.countDefect(result.Next.GetFileId(), path, field)
	if !state {
		return
	}
	s.log.With(logging.Context{
		Operation: "producer-defect", Level: "error", Path: path,
		FileID:      result.Next.GetFileId(),
		TaskID:      w.target.TaskID,
		Offset:      logging.Off(result.Next.GetOffset()),
		RefusalKind: string(storeclient.RefusalInvalidRequest),
		RefusalSite: storeclient.WriteBatchSite,
		Field:       field,
		WriteIDs:    writeIDsOf(result.Entries),
		Repeat:      logging.Repeat(count),
	}).Log(
		"the store refused %d record(s) from this file as an invalid_request; a retry of the same bytes cannot help, so THIS FILE is parked for the life of the process (its cursor stays where the store has it) while every other file keeps being read (observed %d time(s) for this file): %v",
		len(result.Entries), count, cause)
}

// fileDefect is one file's running defect tally.
type fileDefect struct {
	count int
	field string
}

// countDefect records one more occurrence of a file's defect and answers the
// running count together with whether THIS occurrence is stated.
//
// A NEW FIELD IS ALWAYS A NEW DEFECT, and always stated: the store named a
// different part of the batch, so it is a different bug and suppressing it
// would hide the second one behind the first. A repeat of the same field is
// stated on powers of two, so a defect stuck in an un-park loop reports 1, 2,
// 4, 8 ... rather than once per poll forever.
func (s *sidecar) countDefect(fileID, path, field string) (int, bool) {
	key := fileID
	if key == "" {
		// An inferred record names no file position. Falling back to the path
		// keeps the tally per subject rather than collapsing every unfiled
		// defect onto one counter.
		key = path
	}
	seen := s.defects[key]
	if seen == nil {
		seen = &fileDefect{field: field}
		s.defects[key] = seen
	}
	if seen.field != field {
		seen.field = field
		seen.count = 1
		return seen.count, true
	}
	seen.count++
	return seen.count, isPowerOfTwo(seen.count)
}

// isPowerOfTwo answers whether n is 1, 2, 4, 8 ... — the restatement ladder.
func isPowerOfTwo(n int) bool { return n > 0 && n&(n-1) == 0 }

// writeIDsOf names every record of a batch. A batch is refused WHOLE, so
// naming one of its records would misreport what the store rejected.
func writeIDsOf(entries []*storev1.StoreEntry) []string {
	out := make([]string, 0, len(entries))
	for _, e := range entries {
		out = append(out, e.GetWriteId())
	}
	return out
}

// reportResumed closes the outage window in the log. A cycle that began on its
// first attempt spent no time suspended and reports nothing.
func (s *sidecar) reportResumed() {
	if s.attempts == 0 {
		return
	}
	downMs := s.now().Sub(s.suspendedSince).Milliseconds()
	s.log.With(logging.Context{
		Operation: "production-resumed", StoreSocket: s.options.StoreSocket,
		Attempt: logging.Attempt(s.attempts),
	}).Log(
		"production resumed: the store was unreachable for %dms across %d failed recovery attempt(s), during which no file was read",
		downMs, s.attempts)
	s.attempts = 0
}

// rescan discovers targets across EVERY root and creates a tailer for each new
// one, seeded from the cursor THIS cycle's store handed us.
//
// It is the FULL enumeration and the backstop: whatever the per-poll change
// probe beside it misses — a directory whose mtime granularity swallowed a
// write, a spool tree that appeared under a root the probe does not stat — is
// found here, at this interval, exactly as it was before the probe existed.
func (s *sidecar) rescan() {
	s.requireCursors("rescan")
	now := s.now()
	// One summary per stale class the pass caught up on, stated at the end of
	// the pass — deferred so an abandoned pass (a store that went unreachable
	// mid-scan) still summarizes what it demoted before it stopped, rather than
	// stranding the count in a tally the gated re-scan will never re-accumulate.
	defer s.flushCatchupSummaries(now.UnixMilli())
	s.watchedThisPass = 0
	// THE IDENTITY RECORDS ARE RE-READ BEFORE ANYTHING IS DISCOVERED OR
	// RE-KEYED, so a rotation that happened since the last pass is already
	// known when the transcript it produced is first seen.
	//
	// OPTIMIZATION (2026-09-24, owner rule: never remove without asking). An
	// unconditional Refresh drops every remembered miss, and the rekey right
	// after it then re-globs every shim directory once per watcher: ~5 s of CPU
	// per 30 s rescan with 2085 watchers and 132 shim directories (a 30 s pprof
	// put 60% of the sidecar under Resolve's glob from here). A miss can only be
	// falsified by a link file landing, which moves its directory's mtime, so
	// the rescan keeps the misses unless that happened; the full clear still
	// runs every missClearInterval as the net under a write inside the
	// fingerprint's mtime tick.
	if now.Sub(s.lastMissClear) >= missClearInterval {
		s.identity.Refresh()
		s.lastMissClear = now
	} else {
		s.identity.RefreshKeepingMisses()
	}
	s.rekeyRotations()
	s.refreshSpawnFacts()
	if _, ok := s.watchTargets(s.disc.Scan(), now); !ok {
		return
	}
	s.reportRescan()
}

// discoverChanged is the PER-POLL discovery path: it stats the directories the
// globs enumerate and watches whatever a directory whose mtime moved turns out
// to hold, on this tick rather than at the next rescan.
//
// WHY IT EXISTS. Discovery ran only on the rescan interval, so a transcript the
// vendor created one instant after a scan waited out the whole 30s before a
// single byte of it was read. Realtest 9, sweep rt-run36: a fresh workspace's
// first turn concluded at 23:22:44, the vendor wrote its transcript at 23:22:44,
// and tail-pickup came at 23:23:14. The prompt, the turn end and the
// final-answer mark were all already drawn by the other planes; the assistant's
// ANSWER TEXT, which only this process reads, was thirty seconds late.
//
// IT IS NOT A NARROWER SCAN. Nothing was removed from what discovery looks at —
// narrowing it "only serves to obfuscate inefficiency" (owner's standing rule) —
// and the full Scan still runs at its own interval, refreshing holds, meta
// re-checks and every shape this probe does not reach.
//
// THE IDENTITY RECORDS ARE RE-READ BEFORE ANY NEW FILE IS WATCHED, exactly as
// rescan does it and for the same reason: a transcript watched before its
// rotation link file is visible books its records under the vendor's new id,
// which the store then refuses for the life of that file. The refresh happens
// only on a tick that actually found something, so an idle poll still costs one
// stat per candidate directory and nothing else.
func (s *sidecar) discoverChanged() {
	s.requireCursors("discoverChanged")
	changed := s.disc.ScanChanged()
	if len(changed) == 0 {
		return
	}
	now := s.now()
	defer s.flushCatchupSummaries(now.UnixMilli())
	s.identity.RefreshKeepingMisses()
	for _, dir := range changed {
		watched, ok := s.watchTargets(dir.Targets, now)
		if watched > 0 {
			// THE ONE RECORD THIS PATH STATES AT NORMAL VERBOSITY, and only when
			// the change led somewhere: a directory that changed and held nothing
			// new says nothing, and the per-tick "I looked" is debug, in the
			// discoverer itself.
			s.log.With(logging.Context{
				Operation: "discover-change", Path: dir.Dir, Repeat: logging.Repeat(watched),
			}).Log("a watched directory changed since the last poll: %d newly discovered file(s) under it are being read from this tick rather than at the next rescan", watched)
		}
		if !ok {
			// The store could not answer for a file's cursor, so production is
			// suspended and every remaining target would be watched with cursors
			// this cycle no longer has. Same abandonment as the rescan's.
			return
		}
	}
}

// watchTargets builds a tailer for each target not already watched, seeded from
// the cursor THIS cycle's store handed us. It answers how many files it started
// watching, and whether the pass may continue.
//
// THIS IS THE ONLY PLACE A TAILER IS EVER BUILT, which is why it asserts the
// invariant: a tailer built without a recovered cursor map starts at offset 0,
// and that silent cold start is the bug the whole cycle exists to prevent. Both
// discovery paths — the full rescan and the change probe — come through here, so
// the assertion covers both by construction rather than by each remembering it.
func (s *sidecar) watchTargets(targets []discover.Target, now time.Time) (int, bool) {
	s.requireCursors("watchTargets")
	watched := 0
	for _, target := range targets {
		if _, ok := s.watchers[target.Path]; ok {
			continue
		}
		if target.MetaMissing {
			// discover.withMeta already stated this once; the target stays
			// discovered and is re-checked on the next rescan.
			continue
		}
		resolved, ok := s.resolveTarget(target, now)
		if !ok {
			continue
		}
		if s.awaitsRotationLink(resolved) {
			continue
		}
		if resolved.WorkspaceDir != "" {
			s.log.RegisterFile(logging.Context{
				Path: resolved.Path, WorkspaceDir: resolved.WorkspaceDir,
				WorkspaceID: resolved.WorkspaceID, ClaudeSessionID: resolved.ClaudeSessionID,
			})
		}
		identity, err := tail.Identity(resolved.Path)
		if err != nil {
			// A FILE WHOSE IDENTITY CANNOT BE READ IS NOT WATCHED. Its cursor is
			// keyed by that identity, so building a tailer without one would
			// start it at offset 0 — a silent cold start on a file the store may
			// well hold a position for, which is the one thing this cycle
			// exists to prevent. It stays discovered and is retried on the next
			// rescan.
			s.log.With(logging.Context{
				Operation: "watch", Path: resolved.Path, TaskID: resolved.TaskID, Level: "warn",
			}).Log("not watching this file yet: its identity could not be read, and a tailer may only be built from the cursor that identity keys: %v", err)
			continue
		}
		cursor, reached := s.cursorFor(resolved, identity)
		if !reached {
			// The store could not answer for this file, so there is no position
			// to build a tailer from and the suspension is already open. The
			// REST OF THE PASS IS ABANDONED rather than continued: production is
			// suspended now, and every remaining target would be watched with
			// cursors this cycle no longer has.
			return watched, false
		}
		s.watch(resolved, identity, cursor, now)
		watched++
	}
	return watched, true
}

// reportRescan states, ONCE PER PASS, what the pass did to the watched set.
//
// The per-file `watch` record is verbose because this process has no age bound
// on discovery: every transcript ever written under either config root is
// watched forever, so on a working machine that is thousands of records per
// boot describing files nothing will ever append to again. The lifecycle fact
// an operator actually reads off the log is HOW MANY — the size of the watched
// set, and whether this pass grew it — so that is what stands at normal
// verbosity. A pass that changed nothing says nothing.
func (s *sidecar) reportRescan() {
	if s.watchedThisPass == 0 {
		return
	}
	s.log.With(logging.Context{
		Operation: "rescan", Repeat: logging.Repeat(s.watchedThisPass),
	}).Log("this rescan started watching %d newly discovered file(s); %d file(s) are now watched",
		s.watchedThisPass, len(s.watchers))
}

// refreshSpawnFacts re-reads, for every watched file, the one attribution fact
// that lives in ANOTHER file: whether the spawn behind it was backgrounded.
//
// DISCOVERY ORDER IS NOT CAUSAL ORDER. A subagent's sidechain transcript can be
// discovered before the parent transcript's launch result has been read — on a
// restart it usually is — and the flag was frozen at watch time, so that file's
// records named the wrong top_level for the life of the process while the same
// agent's task spool named the right one. Re-reading it every rescan is what
// makes the two agree however the two files were discovered.
//
// IT ONLY EVER TURNS ON. The launch is the sole evidence either way, and once
// observed it does not stop being true; unlearning it on a pass where the owner
// index was momentarily silent would flip an agent's top_level mid-stream.
func (s *sidecar) refreshSpawnFacts() {
	for path, w := range s.watchers {
		if w.ctx == nil || w.ctx.SpawnBackgrounded {
			continue
		}
		if !s.owners.backgroundedFor(w.target) {
			continue
		}
		w.ctx.SpawnBackgrounded = true
		s.log.With(logging.Context{
			Operation: "spawn-backgrounded", Path: path, TaskID: w.target.TaskID, AgentID: w.target.AgentID,
		}).Log("the spawn behind this file is now known to have been backgrounded; its frames name the subagent itself as top_level")
	}
}

// cursorFor answers the position THIS CYCLE'S store holds for a file, and
// whether the store answered at all.
//
// THE CYCLE'S SNAPSHOT IS NOT THE WHOLE ANSWER. Cursors are recovered once, when
// the cycle begins, so a file that appears afterwards — a spool whose hold
// expired, a transcript the vendor RENAMED mid-cycle — is simply absent from it.
// Absent from a snapshot is not the same fact as "the store holds none", and
// treating the two as one is precisely the silent cold start the store-unreachable
// invariant exists to forbid: the renamed file was re-read from zero and its whole
// conversation re-converted, masked only because the write ids are deterministic.
//
// So a miss ASKS THE STORE for that one identity. Only a reached store's empty
// answer means offset zero, which is the honest backfill path; a store that could
// not answer suspends production and the file is left unwatched until a cycle
// that has a store behind it.
func (s *sidecar) cursorFor(target discover.Target, identity string) (*storev1.CursorState, bool) {
	s.requireCursors("cursorFor")
	if cursor := s.cursors[identity]; cursor != nil {
		return cursor, true
	}
	ctx, cancel := s.rpcContext()
	defer cancel()
	cursors, err := s.store.Cursors(ctx, identity)
	if err != nil {
		// storeclient owns the causal record with its rpc and refusal detail.
		s.noteStoreErr("recover-cursors", err)
		if s.interrupted(err) {
			// THE SHUTDOWN WITHDREW THE RECOVERY, so the store never said it
			// could not answer. The file is left unwatched exactly as it would
			// be by the exit one instant later, and the next boot asks for this
			// position again. Stating it as a fault would accuse a store that
			// was fine, which is the rule commit fd8105ee0 settled on the write
			// path and this is the same rule on the cursor path.
			s.log.With(logging.Context{
				Operation: "shutdown", Path: target.Path, TaskID: target.TaskID, FileID: identity,
			}).Log("shutdown withdrew this file's cursor recovery; it is not watched and the next boot recovers its position")
			return nil, false
		}
		s.log.With(logging.Context{
			Operation: "recover-cursors", Path: target.Path, TaskID: target.TaskID,
			FileID: identity, Level: "warn",
		}).Log("not watching this file: the store could not say what position it holds for it, and a tailer may only be built from a position the store handed us: %v", err)
		return nil, false
	}
	for _, cursor := range cursors {
		if cursor.GetFileId() != identity {
			continue
		}
		// Remembered for the rest of the cycle, so one late-appearing file costs
		// one rpc rather than one per rescan.
		s.cursors[identity] = cursor
		s.log.With(logging.Context{
			Operation: "recover-cursors", Path: target.Path, FileID: identity,
			Offset: logging.Off(cursor.GetOffset()),
		}).Log("a file discovered after this cycle began has a stored position; it resumes there rather than from zero")
		return cursor, true
	}
	s.log.With(logging.Context{
		Operation: "recover-cursors", Path: target.Path, FileID: identity,
	}).LogVerbose("the store holds no cursor for this file; it is read from zero")
	return nil, true
}

// watch builds one tailer for a resolved target and starts reading it.
//
// `identity` is the file's own dev:inode and `cursor` the position THIS CYCLE'S
// store holds under it — nil only when the store was reached and genuinely holds
// none. Neither is derived from the path, which the vendor may rename under us
// at any moment.
func (s *sidecar) watch(target discover.Target, identity string, cursor *storev1.CursorState, now time.Time) {
	s.requireCursors("watch")
	bound := s.log.With(logging.Context{
		Component: "tail", Path: target.Path, TaskID: target.TaskID,
		VendorSessionID: target.SessionID, AgentID: target.AgentID,
		WorkspaceDir: target.WorkspaceDir, WorkspaceID: target.WorkspaceID,
		ClaudeSessionID: target.ClaudeSessionID,
	})
	ctx := &tail.Context{
		SessionID:         target.SessionID,
		WorkspaceDir:      target.WorkspaceDir,
		WorkspaceID:       target.WorkspaceID,
		ClaudeSessionID:   target.ClaudeSessionID,
		Path:              target.Path,
		Kind:              target.Kind,
		AgentID:           s.bookFor(target),
		AgentType:         target.Meta.AgentType,
		MainAgentID:       s.mainAgentFor(target),
		SpawnBackgrounded: s.owners.backgroundedFor(target),
		MetaPath:          target.MetaPath,
		ConfigRoots:       s.disc.ConfigRoots(),
		TaskID:            target.TaskID,
		SpoolDir:          target.SpoolDir,
		RunID:             target.RunID,
		RunActivityID:     s.owners.activityFor(target.TaskID),
	}
	tailer := tail.New(target.Path, target.Codec(), s.newHandler(target.Kind, bound), ctx, bound)
	if cursor != nil {
		tailer.Restore(cursor)
		s.rewindOnce(target, identity, tailer, now)
	}
	s.watchers[target.Path] = &watched{target: target, tailer: tailer, ctx: ctx}
	s.trackDetached(target, now)
	// PER FILE, SO VERBOSE. This process watches every transcript under both
	// config roots with no age bound, which on a developer's machine is
	// thousands of long-dead files; one normal-verbosity record each made
	// every boot cost megabytes of log that said nothing but "still here".
	// The COUNTS are the lifecycle fact and rescan states those; WHICH file,
	// and of what kind, is per-record detail.
	bound.With(logging.Context{Operation: "watch"}).LogVerbose("watching %s", target.Kind)
	s.watchedThisPass++
	// A stop that arrived while this spool was held is NOT applied here, even
	// though the spool now has a reader.
	//
	// A CANCELLED TERMINAL OWES THE OUTPUT THE RUN PRODUCED, and at this instant
	// the handler has read no byte of the spool: it has neither the output nor
	// the file position the terminal's write identity is digested from (R-S1).
	// Minting here settled a stopped run with an EMPTY output and, before the
	// file coordinates were carried, with a write identity every such terminal
	// in the process shared. The stop stays pending and pollAll applies it as
	// soon as this file's first batch is DURABLE, which is the first moment
	// there is anything true to say.
	if target.TaskID != "" {
		if _, pending := s.stopped[target.TaskID]; pending {
			bound.With(logging.Context{Operation: "cancel-terminal"}).LogVerbose(
				"the stopped task's spool is now being read; its terminal is minted once the spool's first batch is durable and can state the output")
		}
	}
}

// mainAgentFor answers which BOOK a watched file's records belong to, resolving
// the vendor session id the file plane sees through the shim's identity files.
//
// A `/clear` ROTATES THE VENDOR SESSION ID AND NOTHING ELSE. The conversation's
// AgentId is minted once and never moves (R9), so the rotated transcript — a
// new file under a new id — is still the SAME book. The vendor's files carry no
// lineage at all, so the answer comes from the link file the shim leaves at
// `<state>/shim/<workspace>/vendor-id/<new-id>.json`; without it the reader
// books the rotated records under the new id, the store refuses the batch
// ("would move the row from book A to book B"), and the file's cursor never
// advances again.
//
// THE RESOLUTION IS STATED ONCE PER TRANSCRIPT, at watch time, because it is
// the fact that decides where everything that file ever produces lands.
func (s *sidecar) mainAgentFor(target discover.Target) string {
	observed := s.owners.mainAgentFor(target)
	if observed == "" {
		return ""
	}
	resolved := s.identity.Resolve(observed)
	if resolved.Rotated(observed) {
		s.log.With(logging.Context{
			Operation: "identity-resolve", Path: target.Path,
			VendorSessionID: observed, BookAgentID: resolved.Original, AgentID: resolved.Original,
		}).Log(
			"this transcript's vendor session id is a ROTATED one: the shim's link file (workspace %s) names %s as the conversation's original id, so its records are booked there and not under the id the file is named by",
			resolved.WorkspaceKey, resolved.Original)
		return resolved.Original
	}
	s.log.With(logging.Context{
		Operation: "identity-resolve", Path: target.Path,
		VendorSessionID: observed, BookAgentID: observed, AgentID: observed,
	}).LogVerbose("this transcript's book is its own vendor session id (source=%s)", resolved.Source)
	return observed
}

// rekeyRotations re-resolves every watched file's book, and moves it when the
// answer has changed since the file was first watched.
//
// NOT EVERY CHANGE IS A MOVE. A file watched with NO book — a spool aged into
// residue before the launch line naming its owner was read — is given one here
// for the first time, and there is nothing to move it from. That arm is stated
// at INFO; only a book that leaves one non-empty book for another is the WARN
// a person has to act on.
//
// DISCOVERY ORDER IS NOT CAUSAL ORDER, exactly as it is not for refreshSpawnFacts
// beside it: the rotated transcript can be discovered in the window before its
// link file is visible to this process, and the book was frozen at watch time.
// Re-reading it is what makes a mid-tail rotation land in the right book without
// a restart.
//
// IT RUNS ON EVERY POLL, not only on every rescan, and pollAll states why: a
// record read under a book the link file has already superseded is never read
// again, so the resolution must share the read's clock rather than discovery's.
// Running it twice per rescan tick costs nothing — an unchanged book is a map
// lookup and returns before it writes a word.
//
// IT UNPARKS A FILE THE STORE REFUSED. A park is otherwise permanent and that is
// right — a batch the store can never accept is a producer defect and re-reading
// it is a tight identical loop. A book move is the ONE refusal that stops being
// true: the bytes were refused for naming the wrong book, the cursor did not
// advance, and the identical bytes now convert under the book the store already
// holds those rows in. Re-reading them is therefore progress rather than a
// replay, and the same rows are superseded rather than duplicated because a
// record's write and upsert identities are digested from its file position,
// which has not moved.
func (s *sidecar) rekeyRotations() {
	// ONE QUESTION FOR THE WHOLE WATCHED SET, ASKED OF THE LINK DIRECTORIES
	// RATHER THAN OF EVERY ID. Resolve is an in-memory lookup and remembers the
	// ids nothing links; this is the event that makes a remembered miss wrong,
	// and it costs one readdir plus a stat per shim workspace however many
	// watchers there are. Re-globbing per id instead is what made a steady-state
	// sidecar hold most of a core and stretched a poll tick to seconds.
	s.identity.RecheckLinks()
	for path, w := range s.watchers {
		if w.ctx == nil {
			continue
		}
		observed := s.owners.mainAgentFor(w.target)
		if observed == "" {
			continue
		}
		resolved := s.identity.Resolve(observed)
		if resolved.Original == "" || resolved.Original == w.ctx.MainAgentID {
			continue
		}
		previous := w.ctx.MainAgentID
		// AN ABSENCE OF EVIDENCE NEVER MOVES A FILE OFF A BOOK IT ALREADY HAS.
		// SourceUnrecorded is not a fact about this transcript; it is the R9
		// resume DEFAULT this package falls back to when no identity record
		// names the id — and the resolver documents that answer as the one that
		// "goes stale the instant a rotation writes one". Letting it override an
		// established book made this pass move a transcript on the strength of a
		// file that was not there yet: on the owner's machine the reader booked
		// 4da5f881 out of 90a1151f at 11:06:53 with source=unrecorded, then the
		// shim's link file appeared and moved every record straight back at
		// 11:06:55 with source=vendor_link. Two WARN book moves, both spurious,
		// for a book that never actually changed.
		//
		// A FIRST attribution from `unrecorded` is still taken below: there is no
		// book to override, and the resume rule is the right default for a file
		// nothing has named.
		if previous != "" && resolved.Source == identity.SourceUnrecorded {
			s.log.With(logging.Context{
				Operation: "identity-rekey", Path: path,
				VendorSessionID: observed, BookAgentID: previous, AgentID: previous,
			}).LogVerbose(
				"no identity record names %s, so its book stays %s; the resume default never moves a file off a book that evidence gave it",
				observed, previous)
			continue
		}
		w.ctx.MainAgentID = resolved.Original
		if !s.parked[path] {
			if previous == "" {
				// A FIRST ATTRIBUTION IS NOT A MOVE, and must not be stated as
				// one. A task spool is routinely discovered BEFORE the launch
				// result that names its owner is converted, so it is watched
				// with no book at all and this pass is what finally gives it
				// one. Nothing is leaving a book, nothing is being superseded
				// across books, and there is no defect for anybody to act on —
				// so it is the ordinary fact that it is, at info.
				//
				// IT ALSO NAMES ITS SOURCE RATHER THAN THE IDENTITY FILES. The
				// answer for a spool comes from the OWNER INDEX (the spawning
				// call's book), which for a subagent's spawn is legitimately a
				// tool_use_id and not a vendor session id at all; the identity
				// files never named it, and saying they did sent a reader
				// hunting for a rotation that never happened.
				s.log.With(logging.Context{
					Operation: "identity-rekey", Path: path,
					VendorSessionID: observed, BookAgentID: resolved.Original, AgentID: resolved.Original,
				}).Log(
					"this file was watched before anything named its owner, and is now booked under %s for the first time (resolution source=%s); no records move, because none had a book to move from",
					resolved.Original, resolved.Source)
				continue
			}
			s.log.With(logging.Context{
				Operation: "identity-rekey", Path: path, Level: "warn",
				VendorSessionID: observed, BookAgentID: resolved.Original, AgentID: resolved.Original,
			}).Log(
				"%s is now named as this transcript's original vendor session id (resolution source=%s); its records move from book %s to that one, and nothing already written is duplicated because their write ids are digested from file positions that did not move",
				resolved.Original, resolved.Source, previous)
			continue
		}
		delete(s.parked, path)
		s.log.With(logging.Context{
			Operation: "identity-remap", Path: path, Level: "warn",
			VendorSessionID: observed, BookAgentID: resolved.Original, AgentID: resolved.Original,
			RefusalKind: string(storeclient.RefusalInvalidRequest), RefusalSite: storeclient.WriteBatchSite,
			Field: "entries.upsert_key",
		}).Log(
			"the refusal that parked this file was a BOOK MOVE, and the shim's link file has since named %s as the conversation's original vendor session id: the file is un-parked and re-read from the cursor the store still holds, so the same bytes are now written to the book they belong to instead of to %s",
			resolved.Original, previous)
	}
}

// bookFor answers whose book a watched file's records land in.
//
// A TRANSCRIPT NAMES ITS OWN AGENT — discovery reads it out of the path. An a*
// SPOOL DOES NOT: it is a backgrounded subagent's transcript delivered through
// the task spool, so its path names a TASK and nothing else. Its book is the
// SPAWNING CALL's tool_use_id, which is the identity a subagent is announced
// under on both planes and the same one its `agent-<id>.meta.json` states as
// toolUseId — resolved once by the owner index from the launch the converter
// observed (R-S4).
//
// WITHOUT THIS THE BOOK IS EMPTY. The agent-transcript attribution has no
// filename fallback on purpose (naming a book by `agent-<id>` would give one
// agent two books, one per plane, that no consumer could reconcile), so an a*
// spool tailed with no agent id produced records for a book nobody can open.
func (s *sidecar) bookFor(target discover.Target) string {
	if target.AgentID != "" {
		return target.AgentID
	}
	if target.Kind != tail.KindAgentTranscript || target.TaskID == "" {
		return ""
	}
	run := s.owners.activityFor(target.TaskID)
	if run == "" {
		// A spool whose owner is unresolved is HELD rather than tailed, so
		// reaching here without one is a reader defect and is stated as one.
		s.log.With(logging.Context{
			Operation: "spool-book", Level: "error", Path: target.Path, TaskID: target.TaskID,
		}).Log("an agent spool is being tailed with no spawning call resolved; its records would name no book")
		return ""
	}
	s.log.With(logging.Context{
		Operation: "spool-book", Path: target.Path, TaskID: target.TaskID, AgentID: run,
	}).LogVerbose("the agent spool's book is its spawning call")
	return run
}

// rewindOnce applies the boot rewind: ONE bounded backward scan per file per
// process, moving a restored cursor back to the first record of the in-progress
// turn so the converter's in-memory joins re-warm over one re-read turn.
//
// THE REWIND IS FOR FILES THAT CAN CARRY A TURN IN FLIGHT, NOT FOR THE CORPUS.
// The joins it re-warms exist only for a turn this reader was half-way through
// when it stopped. A transcript that last grew hours ago, whose restored cursor
// already sits at its end, cannot be holding one: nothing has been appended
// since long before this process existed, and there is no half-converted turn
// to re-read. Rewinding it anyway bought nothing and cost the whole boot walk —
// realtest 9, sweep rt-run37: 1353 transcripts rewound and re-read, 1132 of them
// last grown over 108 hours earlier, a startup catch-up that ran 2m24s, and a
// new workspace's answer rows reaching the store a minute after its turn ended.
//
// DISCOVERY IS NOT NARROWED BY THIS. Every file is still enumerated, classified
// and watched; a file at rest is simply watched FROM ITS CURSOR, with no
// backward scan and no re-read. The moment it grows, the ordinary poll reads
// the new bytes exactly as it always did.
//
// Spools are never rewound: they carry no turns, and their deltas are already
// offset-carrying.
func (s *sidecar) rewindOnce(target discover.Target, identity string, tailer *tail.Tailer, now time.Time) {
	if s.rewound[identity] {
		return
	}
	s.rewound[identity] = true
	switch target.Kind {
	case tail.KindSessionTranscript, tail.KindAgentTranscript:
	default:
		s.log.With(logging.Context{Operation: "boot-rewind", Path: target.Path}).
			LogVerbose("no rewind for kind=%s: it carries no turns", target.Kind)
		return
	}
	if reason, atRest := s.atRest(target.Path, tailer.Offset(), now); atRest {
		s.rewindSkipped++
		s.log.With(logging.Context{Operation: "boot-rewind", Path: target.Path}).LogVerbose(
			"no rewind: %s, so no turn of it can be in flight; it is watched from its cursor and not re-read", reason)
		return
	}
	s.rewindWalked++
	tailer.RewindToTurnStart(tail.DefaultRewindWindow, tail.IsUserPromptRecord)
}

// atRest answers whether a transcript can be ruled out as carrying a turn in
// flight, and says in words why.
//
// TWO FACTS HAVE TO HOLD, AND THE SECOND IS THE ONE THAT MAKES IT SAFE. The
// file must have stopped growing longer ago than the LOST tracker's own
// agent-silence window — the bound this process already uses for "an agent has
// stopped working on this", reused rather than duplicated as a second knob —
// AND its restored cursor must already be at the file's end. A cursor BEHIND
// the end means there are durable bytes this reader has not converted, so the
// turn they belong to is exactly the half-converted one the rewind is for, and
// the file is rewound however old it is.
func (s *sidecar) atRest(path string, offset int64, now time.Time) (string, bool) {
	info, err := os.Stat(path)
	if err != nil {
		// NOTHING HERE CAN PROVE THE FILE COLD, so it is rewound exactly as it
		// was before this test existed. The read path states the failure again
		// with its own context; this states why the rewind ran unconditionally.
		s.log.With(logging.Context{Operation: "boot-rewind", Path: path, Level: "warn"}).Log(
			"rewinding without the at-rest test: this file's mtime and size could not be read: %v", err)
		return "", false
	}
	silence := s.tracker.Windows().AgentSilence
	age := now.Sub(info.ModTime())
	if age <= silence {
		return "", false
	}
	if offset < info.Size() {
		return "", false
	}
	return fmt.Sprintf("this file last grew %s ago, beyond the %s agent-silence window, and its restored cursor is already at its end (offset %d of %d bytes)",
		age.Truncate(time.Second), silence, offset, info.Size()), true
}

// trackDetached starts the LOST policy's clock for a file that IS a detached
// run. A transcript is not one: it is an agent's own record, and its silence is
// not a conclusion about anything.
func (s *sidecar) trackDetached(target discover.Target, now time.Time) {
	if target.TaskID == "" || target.SessionID != "" {
		return
	}
	work := stale.Work{
		Path:          target.Path,
		TaskID:        target.TaskID,
		Kind:          target.Kind,
		OwnerAgentID:  s.owners.agentFor(target.TaskID),
		RunActivityID: s.owners.activityFor(target.TaskID),
	}
	if info, err := os.Stat(target.Path); err == nil {
		work.LastActivityMs = info.ModTime().UnixMilli()
	}
	s.tracker.Observe(work, now.UnixMilli())
}

// pollAll polls every watched file once, writes any batch, and commits the
// cursor only after that write was DURABLE. It is reachable only while
// production is live, so a file is never read without somewhere to put what it
// says.
//
// THE BOOKS ARE RE-RESOLVED HERE, IN THE SAME PASS THAT READS THE BYTES, and
// not only on the rescan tick beside discovery. A rotation's link file appears
// without warning, the reader polls four times for every rescan, and a record
// converted under the id its file is NAMED by is wrong FOREVER: an accepted
// batch advances the cursor, and a file that was never parked is never re-read,
// so nothing afterwards revisits the book those rows landed in. Resolving the
// book on a slower clock than the read is therefore not a staleness bound, it
// is a window in which one conversation is silently split across two books —
// the very split ("would move the row from book A to book B") this resolution
// exists to prevent. Tying it to the read closes the window: no byte is
// converted under an identity the disk has already contradicted.
//
// IT IS CHEAP, because identity.Index answers a known id from its maps and only
// an id NO record names costs a lookup on disk — and that miss is exactly the
// answer a rotation invalidates, so it is the one that must never be cached.
// The heavier full Refresh (which also retires records the shim has REMOVED)
// stays on the rescan tick, where discovery's own scan already is.
func (s *sidecar) pollAll() {
	s.requireCursors("pollAll")
	s.rekeyRotations()
	// AFTER the re-key, which has just asked the link directories whether a
	// link landed: a held rotation is released by the very next read.
	s.reexamineRotationHolds(s.now())
	nowMs := s.now().UnixMilli()
	s.enrollWatchers()
	slice := s.pollSlice()
	deadline := s.now().Add(slice)
	polled := 0
	for s.pass != nil && len(s.pass.pending) > 0 {
		// THE FIRST WATCHER OF A TICK IS ALWAYS POLLED, whatever the slice says:
		// a bound so small that it expires before any work is done would be a
		// pass that never advances, which is a corpus never read.
		if polled > 0 && !s.now().Before(deadline) {
			s.log.With(logging.Context{
				Operation: "poll-slice", Repeat: logging.Repeat(polled),
			}).LogVerbose(
				"this tick's %s slice is spent after %d watcher(s); %d remain and the pass resumes at them on the next tick",
				slice, polled, len(s.pass.pending))
			return
		}
		path := s.pass.pending[0]
		s.pass.pending = s.pass.pending[1:]
		w, watching := s.watchers[path]
		if !watching {
			// The watcher went away mid-pass (a LOST run whose file vanished).
			// It has still been WALKED — there is nothing left to read from it —
			// so the pass may still drain.
			continue
		}
		polled++
		if s.parked[path] {
			continue
		}
		result, err := w.tailer.Poll()
		if err != nil {
			s.pollFailed(path, w, err, nowMs)
			continue
		}
		if !result.Changed {
			continue
		}
		skips, err := s.writeBatch(result)
		if err != nil {
			if s.interrupted(err) {
				// The process is going away; storeWrite stated the
				// interrupted write, and THIS reader's cursor stayed where it
				// was — whatever the store ends up doing with the batch, the
				// next boot resumes from the cursor the store holds.
				return
			}
			if field, invalid := storeclient.InvalidRequest(err); invalid {
				// A PRODUCER DEFECT, NOT AN OUTAGE (ruling R-S2). The store can
				// never accept these bytes, so retrying them is a tight
				// identical loop; the file is parked and every other file
				// keeps being read. Production is NOT suspended — nothing is
				// wrong with the store.
				s.park(path, w, result, field, err)
				continue
			}
			// Honest sad path: nothing was committed, so the cursor does not
			// move and the same durable bytes are re-read next cycle.
			s.log.With(logging.Context{
				Operation: "store-write", Path: path, TaskID: w.target.TaskID,
				Offset: logging.Off(result.Next.GetOffset()), Level: "error",
			}).Log("cursor not advanced after %d record(s): %v", len(result.Entries), err)
			// The write did not merely fail, it suspended production. Abandon
			// the pass rather than reading the rest with nowhere to put it.
			return
		}
		w.tailer.Commit(result)
		s.noteSkips(path, skips, nowMs)
		s.applySettled()
		// A stop that arrived before this file had a reader is applied HERE,
		// once a batch of it is durable: only now does its handler hold the
		// output the cancelled terminal owes and the file position the
		// terminal's write identity is digested from.
		if w.target.TaskID != "" {
			s.applyStop(w.target.TaskID)
		}
		if w.vanished {
			// A file that is readable again was a rename race; the tracker
			// clears its own grace clock on the activity below.
			w.vanished = false
		}
		s.tracker.Activity(path, fileActivityMs(path, nowMs))
		s.log.With(logging.Context{
			Operation: "tail-pickup", Path: path, TaskID: w.target.TaskID,
			FileID: result.Next.GetFileId(), Offset: logging.Off(result.Next.GetOffset()),
		}).Log("picked up %d record(s) kind=%s", len(result.Entries), w.target.Kind)
		if result.More && s.pass != nil {
			// THE BATCH STOPPED AT ITS BOUND, NOT AT THE FILE'S END. The file goes
			// back to the head of the pass, so it is read again on this tick in
			// the next bounded write rather than one poll interval later — each
			// write stays small enough not to hold the store, and the slice
			// deadline above still yields the tick to discovery.
			s.pass.pending = append([]string{path}, s.pass.pending...)
			s.log.With(logging.Context{
				Operation: "tail-bounded", Path: path, TaskID: w.target.TaskID,
				Offset: logging.Off(result.Next.GetOffset()),
			}).LogVerbose("the batch stopped at its bound with bytes unread past it; the file is read again at once")
		}
	}
	if s.pass == nil {
		// PRODUCTION WAS SUSPENDED FROM INSIDE THE LOOP — a cancelled terminal's
		// write found the store gone — and `suspend` retired the pass along with
		// the tailers it was walking. That pass did not drain the corpus, so the
		// catch-up window stays open exactly as it does for every other
		// abandonment.
		return
	}
	// EVERY WATCHER WAS POLLED — across however many slices the pass took. Only
	// this exit reaches here: an abandoned pass and a spent slice both return
	// early above, and neither has drained the corpus. The pass is retired so
	// the next tick enrolls the watched set afresh.
	s.pass = nil
	s.drainedPass = true
}

// pollSliceFraction is the share of ONE poll interval a single pass may spend
// before it yields the tick back. A half leaves the other half for the change
// probe, the rekey and the tick's own overhead, and it means a resumed pass
// gets a fresh slice every interval rather than starving.
const pollSliceFraction = 2

// pollSlice is how long one tick's poll may run for.
//
// IT IS DERIVED, NOT CONFIGURED. Every window in this process that an operator
// can move is one more thing that can be set to a value the invariant does not
// survive; this one is a fraction of the poll interval the operator already
// chose, so a faster poll automatically takes finer slices and the relationship
// between them cannot be misconfigured.
func (s *sidecar) pollSlice() time.Duration {
	interval := s.options.PollInterval
	if interval <= 0 {
		interval = DefaultPollInterval
	}
	return interval / pollSliceFraction
}

// enrollWatchers brings the watched set into the current pass, starting one if
// none is open.
//
// A FILE DISCOVERED MID-PASS GOES TO THE FRONT, and that is the whole point of
// the ordering. The boot walk's pass is thousands of watchers long; a transcript
// the change probe found on this tick would otherwise wait behind all of them,
// which is precisely the minutes-late answer the probe exists to prevent. It is
// enrolled at the head, read on the tick it was found, and the boot walk resumes
// behind it.
//
// EVERY OTHER WATCHER IS WALKED IN A STABLE ORDER, because a pass that resumes
// must not re-walk what it already did nor skip what it has not: `seen` is the
// pass's roster and `pending` what is left of it, so "every watcher, exactly
// once, across as many slices as it takes" is a property of the two together
// rather than of map iteration luck.
func (s *sidecar) enrollWatchers() {
	if s.pass == nil {
		s.pass = &pollPass{seen: make(map[string]bool, len(s.watchers))}
	}
	fresh := make([]string, 0, len(s.watchers)-len(s.pass.seen))
	for path := range s.watchers {
		if s.pass.seen[path] {
			continue
		}
		s.pass.seen[path] = true
		fresh = append(fresh, path)
	}
	if len(fresh) == 0 {
		return
	}
	sort.Strings(fresh)
	s.pass.pending = append(fresh, s.pass.pending...)
}

// endCatchupOnFirstDrainedPass closes the startup catch-up window the first time
// a poll pass has walked every watcher to completion.
//
// THAT PASS IS THE BOOT WALK. The cycle's first rescan discovers the whole
// corpus and rewinds it; the first poll pass that runs all the way through has
// picked up every restored byte and converted every spawning call in it. A pass
// that was abandoned — a store outage, a shutdown — has not, so the window stays
// open and the backlog it still owes is leveled rather than restated.
func (s *sidecar) endCatchupOnFirstDrainedPass() {
	if s.catchupEnded || !s.drainedPass {
		return
	}
	s.catchupEnded = true
	s.log.EndCatchup()
	s.stateBootRewindSummary()
	s.summarizeWithheldResidue()
	// THE END OF CATCH-UP IS AN EDGE, AND IT IS STATED. It is written after the
	// summaries, so a reader that has seen this record has seen every total the
	// window owed, and from here on every one of the six operations is news
	// stated per item. It is also the one edge a test can wait on to be inside
	// steady state rather than racing the boot walk.
	s.log.With(logging.Context{Operation: "catchup-end"}).Log(
		"startup catch-up is over: the first poll pass drained the corpus, and every catch-up operation is stated per record from here")
}

// stateBootRewindSummary states, once at the catch-up edge, HOW MUCH OF THE
// CORPUS THE BOOT REWIND ACTUALLY RE-READ.
//
// The per-file decision is verbose on both arms — a boot walk makes it once per
// transcript, which on the owner's machine is over a thousand lines — so nothing
// at normal verbosity would otherwise say whether the walk re-read two files or
// two thousand. That number is the whole cost of the walk, and it is the first
// thing to look at when a restart is slow. A process that rewound nothing and
// skipped nothing states nothing, exactly as every other catch-up summary does.
func (s *sidecar) stateBootRewindSummary() {
	if s.rewindWalked == 0 && s.rewindSkipped == 0 {
		return
	}
	s.log.With(logging.Context{
		Operation: "boot-rewind-summary", Repeat: logging.Repeat(s.rewindWalked + s.rewindSkipped),
	}).Log(
		"the boot rewind scanned %d transcript(s) that could still be carrying a turn in flight; %d more were at rest — last grown beyond the %s agent-silence window with the cursor already at their end — and were watched from their cursor without a backward scan or a re-read",
		s.rewindWalked, s.rewindSkipped, s.tracker.Windows().AgentSilence)
}

// summarizeWithheldResidue states ONE INFO record per file for the residue the
// write path withheld during the boot walk, carrying the counts by residue
// label.
//
// PER FILE, AND AT THIS EDGE, for the same reason every other catch-up summary
// is: the per-line withholding records are DEBUG and always will be (they are
// the steady state, not news), so nothing at INFO would otherwise say the boot
// walk read a quarter-million residue lines and stored none of them. The file is
// the unit because the volume question is always "which file produced it".
//
// THE INFERRED BATCHES HAVE NO FILE and are summarized separately, under the
// empty tally key: they were not read at a file position, so there is no path to
// name.
//
// It states nothing for a file that withheld nothing, exactly as EndCatchup
// states nothing for an operation that demoted nothing.
func (s *sidecar) summarizeWithheldResidue() {
	for _, w := range s.watchers {
		s.stateWithheld(w.target.Path, w.target.TaskID, s.residueWithheld[w.tailer.FileID()], s.newShapes[w.tailer.FileID()])
	}
	s.stateWithheld("", "", s.residueWithheld[""], s.newShapes[""])
}

// stateWithheld writes one summary for one tally, and nothing for an empty one.
func (s *sidecar) stateWithheld(path, taskID string, tally map[string]int, newShapes int) {
	if len(tally) == 0 {
		return
	}
	total := 0
	for _, count := range tally {
		total += count
	}
	where := "in this file"
	if path == "" {
		where = "in this process's inferred records, which name no file"
	}
	s.log.With(logging.Context{
		Operation: "residue-drop-summary", Path: path, TaskID: taskID,
		Repeat: logging.Repeat(total),
	}).Log("startup catch-up read and classified %d residue record(s) %s and stored none of them, cataloguing %d shape(s) this process had not seen before: %s",
		total, where, newShapes, residueCounts(tally))
}

// residueCounts renders a tally in label order, so two summaries of the same
// counts read identically.
func residueCounts(tally map[string]int) string {
	labels := make([]string, 0, len(tally))
	for label := range tally {
		labels = append(labels, label)
	}
	sort.Strings(labels)
	out := ""
	for _, label := range labels {
		if out != "" {
			out += ", "
		}
		out += fmt.Sprintf("%s=%d", label, tally[label])
	}
	return out
}

// RunSettled records that a converter read a detached run's OWN terminal off
// its file. It is the reader's half of the terminal seam.
//
// IT DOES NOT UNTRACK THE RUN YET. The terminal is a record like any other, and
// it is only true of the store once the batch carrying it is durable; until then
// this is a promise the poll loop keeps in applySettled.
func (s *sidecar) RunSettled(path, run string) {
	s.settling[path] = run
	// THE TERMINAL IS ALSO WHAT MAKES A LATER DISAPPEARANCE ORDINARY. A spool
	// the vendor reaps after its task ends carried `[exited with code N]` or
	// `[killed]` before it went, so nothing was outstanding; the watcher
	// remembers that so pollFailed can say so instead of warning.
	if w := s.watchers[path]; w != nil {
		w.terminated = true
	}
}

// applySettled tells the LOST policy about every terminal whose batch just
// became durable. A settled run is never swept: LOST is the answer for a run we
// stopped seeing, never for one we watched finish.
func (s *sidecar) applySettled() {
	for path, run := range s.settling {
		delete(s.settling, path)
		if !s.tracker.Open(path) {
			continue
		}
		s.tracker.Settle(path)
		s.log.With(logging.Context{Operation: "run-settled", Path: path, ActivityID: run}).
			Log("the run's own terminal was read from its file and is durable; it can no longer be concluded LOST")
	}
}

// fileActivityMs answers when a file last actually GREW, which is what the LOST
// policy's clock means. Our own read is not activity: a file full of bytes
// written before the last reboot is not alive because we got round to reading
// it, and stamping the read time onto it would make swept_up unreachable for
// exactly the runs it exists to conclude. The fallback is used only when the
// file cannot be stat'd, which the next poll will surface as its own failure.
func fileActivityMs(path string, fallback int64) int64 {
	info, err := os.Stat(path)
	if err != nil {
		return fallback
	}
	return info.ModTime().UnixMilli()
}

// reasonTreeRemoved is one of the `file-vanished` record's discriminators: the
// file did not merely disappear, the DIRECTORY holding it went too. None of the
// discriminators is a LOST arm and none reaches the wire — DetachedLost still
// carries file_vanished — they are the fact that says whether an operator has
// anything to look at. The vocabulary itself lives in the stale package, which
// carries it through to the conclusion.
const (
	reasonTreeRemoved = stale.BenignTreeRemoved
	reasonEnded       = stale.BenignEnded
	reasonFullyRead   = stale.BenignFullyRead
)

// vanishReason answers why a vanished file took NOTHING with it, or "" when it
// may have taken bytes past the committed offset.
//
// A FILE THAT VANISHES AFTER EVERYTHING IN IT WAS READ LOST NOTHING. Three ways
// that can be known, checked in the order of how much each says:
//
//   - the DIRECTORY went too, so nobody unlinked a file out from under us;
//   - the spool had already written its TERMINATOR, so the run it carried was
//     concluded before the file went (the vendor reaps a task's output file
//     after the task ends, which is exactly this case);
//   - the committed offset equals the SIZE the last poll saw, so every byte the
//     file ever held is already durable.
//
// A tailer that never completed a poll cannot answer the third question, and a
// file whose size GREW past the committed offset before it went plainly had
// outstanding bytes; both keep the WARNING, which is the loss the level exists
// for.
func vanishReason(path string, w *watched) string {
	if treeRemoved(path) {
		return reasonTreeRemoved
	}
	if w.terminated {
		return reasonEnded
	}
	if w.tailer != nil {
		if size, sized := w.tailer.LastSize(); sized && w.tailer.Offset() >= size {
			return reasonFullyRead
		}
	}
	return ""
}

// treeRemoved answers whether a vanished file's DIRECTORY is gone as well.
//
// ONLY A DEFINITE ABSENCE COUNTS. A stat that fails for any other reason — a
// permission change, an unresponsive mount — is not evidence the tree was
// removed, and reading it as such would quietly downgrade a genuine unlink to an
// ordinary end. Anything but ErrNotExist therefore answers false and the record
// keeps its warning.
func treeRemoved(path string) bool {
	_, err := os.Stat(filepath.Dir(path))
	return errors.Is(err, fs.ErrNotExist)
}

// pollFailed narrates one file's read failure and, for a file that vanished,
// starts the LOST policy's grace clock.
//
// THE VANISHED FILE'S TAILER IS KEPT. Its handler is the only converter that
// can spell this run's terminal (seam.go looks the run's file up among the
// watchers), so dropping it here would turn every file_vanished conclusion into
// "no terminal for the LOST run" and leave the run open in every reader
// downstream. It is dropped in lostEntries, once its terminal has been stated.
func (s *sidecar) pollFailed(path string, w *watched, err error, nowMs int64) {
	if os.IsNotExist(err) {
		reason := vanishReason(path, w)
		s.tracker.MarkVanished(path, nowMs, reason)
		if w.vanished {
			s.log.With(logging.Context{Operation: "file-vanished", Path: path, TaskID: w.target.TaskID}).
				LogVerbose("the vanished file is still absent; its grace window has not decided yet")
			return
		}
		w.vanished = true
		if reason != "" {
			// A FILE THAT VANISHED WITH NOTHING OUTSTANDING IS AN ORDINARY END.
			// Either nobody unlinked a file out from under the reader (the
			// directory holding it went too), or the file had already given up
			// everything it ever held — a spool past its terminator, a file read
			// to its last observed byte and committed there. There is nothing for
			// an operator to act on, so it is stated rather than warned, and
			// `reason` carries the discriminator so the cases are filterable
			// apart without reading prose.
			s.log.With(logging.Context{Operation: "file-vanished", Path: path, TaskID: w.target.TaskID, Reason: reason}).
				Log("the watched file vanished with nothing outstanding (%s); the committed offset is the last thing it had", reason)
			return
		}
		s.log.With(logging.Context{Operation: "file-vanished", Path: path, TaskID: w.target.TaskID, Level: "warn"}).
			Log("the watched file vanished; any bytes appended past the committed offset went with it")
		return
	}
	s.log.With(logging.Context{Operation: "poll", Path: path, TaskID: w.target.TaskID, Level: "error"}).
		Log("tail poll failed: %v", err)
}

// writeBatch sends one tailer batch — the records plus the cursor advance that
// must become durable WITH them.
//
// THE CURSOR RIDES WITH THE RECORDS ON PURPOSE. Split them and a crash between
// the two either loses records (cursor advanced first) or duplicates them
// (records first); only the second is survivable, by the deterministic write_id
// on every record, which is a recovery rather than a guarantee.
func (s *sidecar) writeBatch(result tail.PollResult) ([]storeclient.SkippedEntry, error) {
	return s.storeWrite("tailer batch", &storev1.EntryBatch{
		Entries:       result.Entries,
		CursorAdvance: result.Next,
	})
}

// sweep states the LOST conclusions whose windows expired and writes their
// terminals.
func (s *sidecar) sweep() {
	s.requireCursors("sweep")
	s.emit("lost sweep", s.lostEntries(s.tracker.Sweep(s.bootTimeMs(), s.now().UnixMilli())))
}

// emit writes inferred records as a single CURSOR-LESS batch: they were not
// read at a file position, so there is no reader position that becomes durable
// with them and nothing that could advance one wrongly.
func (s *sidecar) emit(what string, entries []*storev1.StoreEntry) {
	if len(entries) == 0 {
		return
	}
	skips, err := s.storeWrite(what, &storev1.EntryBatch{Entries: entries})
	if err != nil {
		if s.interrupted(err) {
			// storeWrite already stated the shutdown; there is no outage here.
			return
		}
		if field, invalid := storeclient.InvalidRequest(err); invalid {
			// These conclusions name no file position — they were inferred, not
			// read — so there is no tailer to park. The defect is stated and
			// they are simply not restated, because re-minting the same
			// rejected records on the next sweep is the identical loop R-S2
			// forbids.
			s.log.With(logging.Context{
				Operation: "producer-defect", Level: "error",
				RefusalKind: string(storeclient.RefusalInvalidRequest),
				RefusalSite: storeclient.WriteBatchSite,
				Field:       field, WriteIDs: writeIDsOf(entries),
			}).Log("the store refused %d inferred %s record(s) as an invalid_request; a retry of the same records cannot help: %v", len(entries), what, err)
			return
		}
		s.log.With(logging.Context{Operation: "store-write", Level: "error"}).
			Log("%s write failed for %d record(s); production is suspended and the conclusions are restated on the next cycle: %v", what, len(entries), err)
		return
	}
	// The write was durable. Inferred records name no file, so a skip here is
	// never catch-up backlog: it is unexpected and warned per entry.
	s.warnUnexpectedSkips("inferred "+what, skips)
}

// rpcContext bounds one store call by rpcTimeout AND ties it to the process's
// shutdown, so the deadline is the ceiling rather than the only way out.
//
// THE SHUTDOWN IS NOT THE CALL'S PARENT, DELIBERATELY. A context that is simply
// a child of s.shutdown dies the instant the signal lands, which abandons a
// write the store may be committing right then and leaves this process unable
// to say whether it landed. The call is instead given shutdownSettle after the
// signal to answer for itself, and only then cancelled — the ceiling on how
// long a shutdown may wait for the store, with rpcTimeout still capping the
// call as a whole.
func (s *sidecar) rpcContext() (context.Context, context.CancelFunc) {
	parent := s.shutdown
	if parent == nil {
		return context.WithTimeout(context.Background(), rpcTimeout)
	}
	ctx, cancel := context.WithTimeout(context.WithoutCancel(parent), rpcTimeout)
	done := make(chan struct{})
	go func() {
		select {
		case <-done:
			return
		case <-parent.Done():
		}
		settle := time.NewTimer(shutdownSettle)
		defer settle.Stop()
		select {
		case <-done:
		case <-settle.C:
			cancel()
		}
	}()
	return ctx, func() {
		close(done)
		cancel()
	}
}

// interrupted reports that this error is the shutdown cancelling an in-flight
// store call rather than anything wrong with the store. Callers skip their
// outage narration for it; storeWrite states the one INFO record.
func (s *sidecar) interrupted(err error) bool {
	return err != nil && s.shutdown != nil && s.shutdown.Err() != nil && errors.Is(err, context.Canceled)
}

// withholdResidue is the RESIDUE RULE, applied where nothing can route around
// it: only TYPED entries are persisted, and every residue arm — `vendor_specific`
// of any kind, `unknown`, and the `unparsed` bytes an unowned spool ingests — is
// classified, counted, and not written.
//
// IT SITS IMMEDIATELY ABOVE THE ONE WRITE PATH ON PURPOSE. Residue is minted by
// the converter, by three handlers and by the detached-stop seam, so a filter at
// any producer is a filter that the next producer forgets. `storeWrite` is the
// sidecar's only door to the store, so a rule enforced here is a rule about the
// SIDECAR rather than about the callers that happen to exist today.
//
// NOTHING IS SILENCED AND NOTHING IS READ LESS. The line was framed, converted
// and classified before it reached here; the withholding is stated per record at
// DEBUG and rolled into the per-file `residue-drop-summary` at INFO. And nothing
// is unrecoverable: the sidecar's sources are the vendor's own durable files, so
// the day a residue arm earns a model, the file is simply re-read.
//
// THE CURSOR STILL ADVANCES. It rides the batch, not the entries, so a batch
// whose every record was residue still commits the reader's position — the bytes
// were read, and re-reading them would produce the same nothing.
func (s *sidecar) withholdResidue(batch *storev1.EntryBatch) ([]*storev1.StoreEntry, []*storev1.ShapeObservation) {
	entries := batch.GetEntries()
	fileID := batch.GetCursorAdvance().GetFileId()
	kept := entries[:0]
	var shapes []*storev1.ShapeObservation
	// inBatch dedupes the observations WITHIN this batch: a boot walk reads
	// thousands of lines of one shape, and one observation per line would send
	// the catalog the very volume the catalog exists to avoid storing. The
	// store's own count still rises once per batch, which is the honest thing
	// the wire can carry.
	inBatch := map[string]bool{}
	for _, e := range entries {
		if !convert.IsResidue(e) {
			kept = append(kept, e)
			continue
		}
		label := convert.ResidueLabel(e)
		tally := s.residueWithheld[fileID]
		if tally == nil {
			tally = map[string]int{}
			s.residueWithheld[fileID] = tally
		}
		tally[label]++
		// THE SHAPE IS WHAT SURVIVES THE WITHHOLDING. The bytes are not stored,
		// so the key structure is the only thing left that says the vendor emits
		// this line at all (shape.go, owner ruling 2026-09-13).
		if shape, ok := convert.ResidueShape(e, s.now().UnixMilli()); ok && !inBatch[shape.GetShapeHash()] {
			inBatch[shape.GetShapeHash()] = true
			shapes = append(shapes, shape)
			if !s.shapeCatalogued[shape.GetShapeHash()] {
				s.shapeCatalogued[shape.GetShapeHash()] = true
				s.newShapes[fileID]++
			}
		}
		// IT ANNOUNCES NO ROW, so it carries no upsert_key: the whole point is
		// that nothing was stored, and a record naming a key nobody can look up
		// is the untraceable announcement the field-set contract forbids.
		s.log.With(logging.Context{
			Operation: "residue-drop", FileID: fileID, Reason: label,
		}).LogVerbose("residue %s is never persisted; the record was read and classified and is not stored", label)
	}
	return kept, shapes
}

// storeWrite is the sidecar's ONLY path to the store. Routing every write
// through here is what makes an unreachable store impossible to miss.
func (s *sidecar) storeWrite(what string, batch *storev1.EntryBatch) ([]storeclient.SkippedEntry, error) {
	kept, shapes := s.withholdResidue(batch)
	batch.Entries = kept
	if len(batch.GetEntries()) == 0 && batch.GetCursorAdvance() == nil && len(shapes) == 0 {
		// Nothing to store, no position to advance and no shape to catalogue. A
		// batch that was ONLY residue is not an empty write to make: the store
		// has nothing to do with it, and asking anyway would spend an rpc per
		// residue line. A batch carrying a SHAPE still goes: the observation is
		// the one durable thing the withheld line leaves behind.
		return nil, nil
	}
	ctx, cancel := s.rpcContext()
	defer cancel()
	skipped, err := s.store.WriteBatch(ctx, batch, shapes)
	if s.interrupted(err) {
		// NOT AN OUTAGE AND NOT SWALLOWED: the error still returns, but the
		// store was fine. What it did with the batch is unknown — it may have
		// committed records and cursor together after this process stopped
		// listening — and either way the next boot resumes from the cursor the
		// store holds and re-reads the durable bytes past it.
		s.log.With(logging.Context{Operation: "shutdown"}).Log(
			"shutdown interrupted a write that had not answered within %s of the signal; whether the store committed it is UNKNOWN and nothing is lost either way, because records and cursor ride one transaction: the next boot resumes from whichever cursor the store holds (%s, %d record(s))",
			shutdownSettle, what, len(batch.GetEntries()))
		return nil, err
	}
	s.noteStoreErr(what, err)
	return skipped, err
}

// noteSkips records the legacy book-conflict entries the store skipped for a
// batch read from `path`. A skip during STARTUP CATCH-UP is the corrected
// converter re-ingesting already-stored content under its now-right book:
// expected, and folded into ONE per-pass summary (flushCatchupSummaries) rather
// than warned per entry. A skip in STEADY STATE is unexpected — nothing should
// re-book a live row — so it is warned per entry.
func (s *sidecar) noteSkips(path string, skips []storeclient.SkippedEntry, nowMs int64) {
	if len(skips) == 0 {
		return
	}
	mtimeMs := fileActivityMs(path, nowMs)
	if s.isBacklog(mtimeMs) {
		for range skips {
			s.catchupBookConflicts.add(mtimeMs)
		}
		s.log.With(logging.Context{
			Operation: "book-conflict-skip", Path: path, Reason: "legacy_book_conflict", Level: "debug",
		}).LogVerbose("store skipped %d legacy book-conflict entrie(s) during startup catch-up; the stored rows are kept and this is summarized rather than stated one by one", len(skips))
		return
	}
	s.warnUnexpectedSkips("tailer batch from "+path, skips)
}

// warnUnexpectedSkips states one WARN per legacy book-conflict skip that arose
// where a skip is NOT expected — steady-state reads and inferred records. It is
// the sad-path record for "the store kept a row this write tried to re-book";
// nothing is swallowed, and the write still succeeded for every other entry.
func (s *sidecar) warnUnexpectedSkips(what string, skips []storeclient.SkippedEntry) {
	for _, skip := range skips {
		s.log.With(logging.Context{
			Operation: "book-conflict-skip", Reason: "legacy_book_conflict", Level: "warn",
		}).Log("store skipped a legacy book-conflict entry in steady state (%s): upsert_key=%q is stored under book %q and this write would have moved it to %q; the stored row is kept -- nothing should re-book a live row",
			what, skip.UpsertKey, skip.FromBook, skip.ToBook)
	}
}

// indexCursorsByFileID keys recovered cursors by the FILE'S OWN IDENTITY for
// tailer restore.
//
// THE IDENTITY IS WHAT SURVIVES THE VENDOR'S RENAMES, and that is the whole
// reason the store keys its cursor row by file_id rather than by path. Keying
// this index by path instead made a rename look like a file nobody had ever
// read: the new path found no cursor, the tailer was built at offset 0, and the
// entire conversation was re-converted and re-written — absorbed by the store
// only because the write ids are deterministic. `path` on a CursorState is where
// the file was last SEEN, which is a thing to display and never a thing to key.
func indexCursorsByFileID(cursors []*storev1.CursorState) map[string]*storev1.CursorState {
	out := make(map[string]*storev1.CursorState, len(cursors))
	for _, cursor := range cursors {
		if cursor.GetFileId() != "" {
			out[cursor.GetFileId()] = cursor
		}
	}
	return out
}

// jitterBackoff spreads d over ±recoverJitterFraction of itself. A zero delay
// (the immediate retry a fresh suspension arms) stays immediate.
func jitterBackoff(d time.Duration) time.Duration {
	if d <= 0 {
		return 0
	}
	spread := float64(d) * recoverJitterFraction
	return time.Duration(float64(d) - spread + 2*spread*rand.Float64())
}

// resolveBackoff answers the ladder's effective floor and ceiling. ZERO IS HOW
// THE CALLER SAYS "UNSET", exactly as it is for every other window this process
// takes, and the default is filled in here so there is one place that knows it.
// A ceiling below the floor is not a configuration this function repairs: main
// refuses it at bootstrap, and the clamp below only keeps a ladder built any
// other way from climbing past its own floor.
func resolveBackoff(min, max time.Duration) (time.Duration, time.Duration) {
	if min <= 0 {
		min = recoverBackoffMin
	}
	if max <= 0 {
		max = recoverBackoffMax
	}
	if max < min {
		max = min
	}
	return min, max
}

// nextBackoff doubles d from the ladder's floor up to its ceiling.
func nextBackoff(d, min, max time.Duration) time.Duration {
	if d == 0 {
		return min
	}
	d *= 2
	if d > max {
		return max
	}
	return d
}

// bootTimeMillis returns the machine boot time in unix millis, or 0 when it is
// unavailable. The kernel interface that answers it is per-platform (see
// boottime_darwin.go and boottime_linux.go); the "unavailable is 0" contract
// the boot sweep reads is stated once, here.
//
// DERIVED ONCE, AND ONCE IS THE POINT. The machine's boot instant is a FIXED
// FACT that cannot change while this process runs, and the sweep compares it
// against file mtimes, which are fixed too — so the comparison is only
// well-defined if the boot side is one value.
//
// Re-deriving it per sweep made it a MOVING quantity on linux, where there is
// no latched kern.boottime and the instant is computed as `time.Now() -
// sysinfo.Uptime`: `Uptime` is whole SECONDS, so two derivations a moment apart
// legitimately differ by up to a second, and any wall-clock step between them
// moves it by the whole step. A run's `swept_up` eligibility
// (internal/stale/stale.go) is `mtime < bootMs`, so a file sitting near the
// boundary flipped between "still running" and LOST across passes with nothing
// in the world having changed — and a `lost.swept_up` terminal is written over
// work that may still be producing. Darwin's kern.boottime is already latched
// at boot, so the latch costs nothing there and fixes linux.
//
// ONLY A SUCCESSFUL DERIVATION IS LATCHED. Latching a failure would disable the
// boot sweep for the life of the process off one bad syscall; a failure is
// returned as the same 0 as before, and the next call tries again — which is
// also what keeps BootSweep's "boot time unavailable" warning reachable.
var bootTime struct {
	mu sync.Mutex
	ms int64
}

// derivePlatformBootTime is the kernel interface bootTimeMillis latches. It is
// a variable so a test can substitute a DRIFTING derivation and prove the latch
// holds one answer; production never reassigns it.
var derivePlatformBootTime = platformBootTimeMillis

func bootTimeMillis() int64 {
	bootTime.mu.Lock()
	defer bootTime.mu.Unlock()
	if bootTime.ms > 0 {
		return bootTime.ms
	}
	ms, err := derivePlatformBootTime()
	if err != nil || ms < 0 {
		return 0
	}
	bootTime.ms = ms
	return ms
}
