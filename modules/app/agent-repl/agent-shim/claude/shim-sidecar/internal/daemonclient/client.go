// Package daemonclient owns the shim-claude-sidecar's ClientLog integration
// boundary. It resolves the daemon's current loopback address from the shared
// state root and reads the daemon-minted ref from the global workspace roster,
// so a daemon handover changes both the destination and the authoritative ref
// without restarting the launchd-managed sidecar.
package daemonclient

import (
	"context"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"connectrpc.com/connect"
	"google.golang.org/protobuf/types/known/structpb"
)

const requestTimeout = 2 * time.Second

// workspaceRefFreshness bounds how long a directory's daemon-minted ref is
// reused before the roster is read again for it.
//
// A DIRECTORY'S WORKSPACE REF IS ONLY EVER USED WHILE THE ROSTER STILL HOLDS
// IT. The roster moves under this cache without telling it: a workspace is
// forgotten and the SAME directory is registered again under a NEW id, which
// realtest 9 (sweep rt-run39) caught costing a whole session's file-scoped
// diagnostics — every one of them addressed to a workspace the daemon had
// dropped eight minutes earlier, refused, and written unattributed. Nothing
// pushes that change here, so the freshness bound is what makes the staleness
// window finite. It is the sidecar's default poll interval (main.go's
// DefaultPollInterval, which this package cannot import): a ref may be at most
// one poll pass behind the roster, which is the same granularity every other
// fact this process attributes a pickup with is already read at.
const workspaceRefFreshness = time.Second

// readinessTimeout bounds the loopback connect that decides whether the daemon
// is live. A booting daemon has bound its listener and published daemon.addr
// but is not yet accepting, so the connect either fails fast or the kernel
// queues it; either way a short bound keeps a readiness probe from riding the
// full request timeout on every retry rung.
const readinessTimeout = 250 * time.Millisecond

// cachedWorkspaceRef is one directory's daemon-minted ref together with the
// instant the roster that named it was delivered. The instant is the whole
// point: a ref carries no expiry of its own, and the roster it came from can
// have moved on without anything telling this process.
type cachedWorkspaceRef struct {
	ref    *workspacev1.WorkspaceRef
	readAt time.Time
}

// Client resolves daemon.addr and forwards one file-scoped diagnostic through
// agentrepl.v1.AgentRepl.ClientLog. The HTTP client is shared so sequential
// records reuse the loopback connection; the generated Connect client is
// rebuilt from the current address because daemon handover changes it.
type Client struct {
	addrPath string
	http     *http.Client

	// now is the clock every cached ref's age is measured against, injected by
	// the tests so freshness is decided rather than waited out.
	now func() time.Time
	// freshness is workspaceRefFreshness, held per Client so a test can state
	// the bound it is exercising instead of racing the real one.
	freshness time.Duration

	mu sync.Mutex
	// cachedAddress is the daemon the whole workspace cache belongs to. A
	// different address is a DIFFERENT ROSTER, so the entire cache is dropped
	// rather than consulted: ids are minted per daemon and mean nothing across
	// a handover.
	cachedAddress string
	// workspaces holds one entry PER NORMALIZED DIRECTORY. It used to be a
	// single slot, so two directories forwarding in turn re-read the roster for
	// every record while a re-registered directory kept its dead ref forever.
	workspaces map[string]cachedWorkspaceRef
	// refReplaced is told, once, that a directory's ref moved from one
	// daemon-minted id to another. It is the ordinary re-registration, so it is
	// an INFO the owner can join the two ids on, never a warning.
	refReplaced func(dir, oldID, newID string)
	// seenServing is the set of addresses this Client has ever seen ACCEPTING —
	// a successful Ready probe or a successful Forward — keyed by the bare
	// "host:port" the address names. daemon.addr publishes an address before
	// its listener answers, so an address absent from this set is presumed
	// BOOTING regardless of its advertised pid's liveness; see classifyForward.
	seenServing map[string]struct{}
}

// New constructs the forwarding boundary for one resolved agent-repl state
// root. An empty state root is an invariant violation: there is then no honest
// place from which to resolve the daemon that owns sidecar.log.
func New(stateDir string) *Client {
	if strings.TrimSpace(stateDir) == "" {
		panic("sidecar daemonclient: state directory is required")
	}
	return &Client{
		addrPath: filepath.Join(stateDir, "daemon.addr"),
		http: &http.Client{
			Transport: &http.Transport{Proxy: nil},
			Timeout:   requestTimeout,
		},
		now:        time.Now,
		freshness:  workspaceRefFreshness,
		workspaces: map[string]cachedWorkspaceRef{},
	}
}

// Ready reports whether the daemon has PUBLISHED a live address: daemon.addr
// exists and names a loopback host:port, and a loopback connect to it succeeds
// within readinessTimeout. A booting daemon binds its listener and writes
// daemon.addr at boot steps 5 and 6 but does not reach http.Server.Serve until
// its reconciliation finishes, so the address can be published while the
// listener still queues rather than accepts; a plain connect is the cheapest
// honest signal that it is answering. The probed address is returned even when
// it is not yet live, so the logger can key its records on the destination it
// would have used.
func (c *Client) Ready() (string, bool) {
	address := c.addrPath
	raw, err := os.ReadFile(c.addrPath)
	if err != nil {
		return address, false
	}
	// daemon.addr's first line is the bare address; an optional "pid=<n>" line
	// may follow it (see daemonaddr.ReadAdvertisement). Readiness needs only the
	// address, so it takes the first line and ignores the rest.
	address = addressLine(string(raw))
	if err := validateAddress(address); err != nil {
		return address, false
	}
	conn, err := net.DialTimeout("tcp", address, readinessTimeout)
	if err != nil {
		return address, false
	}
	_ = conn.Close()
	c.markServing(address)
	return address, true
}

// markServing records that address has now been seen accepting at least
// once, for classifyForward's per-address boot tolerance.
func (c *Client) markServing(address string) {
	if address == "" {
		return
	}
	c.mu.Lock()
	if c.seenServing == nil {
		c.seenServing = map[string]struct{}{}
	}
	c.seenServing[address] = struct{}{}
	c.mu.Unlock()
}

// hasSeenServing reports whether address has ever been seen accepting.
func (c *Client) hasSeenServing(address string) bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	_, ok := c.seenServing[address]
	return ok
}

// Forward resolves the current daemon and sends one record. The returned
// address is the rate-limit identity the logger uses if err is non-nil. Before
// daemon.addr can be read, its path is the only destination identity available.
func (c *Client) Forward(record logging.ForwardRecord) (string, error) {
	address := c.addrPath
	raw, err := os.ReadFile(c.addrPath)
	if err != nil {
		// The advertisement itself is gone — exactly the "addr file is absent
		// ... since the attempt began" half of the not-there invariant (see
		// logging.ErrForwardTargetNotThere): there is no target to be stuck
		// against, so this is a transient, not a fault.
		return address, fmt.Errorf("read daemon address %q: %w: %w", c.addrPath, err, logging.ErrForwardTargetNotThere)
	}
	// daemon.addr's first line is the bare address; an optional "pid=<n>" line
	// may follow it (see daemonaddr.ReadAdvertisement, mirrored locally by
	// parseAdvertisement). usedAddress/usedPID name the target THIS ATTEMPT is
	// about to dial, so a later failure can tell whether that specific target
	// is still there.
	usedAddress, usedPID, usedPIDKnown := parseAdvertisement(string(raw))
	address = usedAddress
	if err := validateAddress(address); err != nil {
		return address, fmt.Errorf("daemon address %q from %s: %w", address, c.addrPath, err)
	}
	contextValue, err := structpb.NewStruct(wireContext(record.Context))
	if err != nil {
		return address, fmt.Errorf("encode context for %s: %w", record.Operation, err)
	}
	requestRecord := &agentreplv1.ClientLogRecord{
		Operation: record.Operation,
		Message:   record.Message,
		Context:   contextValue,
		Timestamp: record.Timestamp,
		Verbose:   record.Verbose,
		Runtime: &agentreplv1.ClientLogRecord_Sidecar{
			Sidecar: &agentreplv1.ClientLogRuntimeSidecar{},
		},
	}
	setLevel(requestRecord, record.Level)
	client := agentreplv1connect.NewAgentReplClient(c.http, "http://"+address)
	ctx, cancel := context.WithTimeout(context.Background(), requestTimeout)
	defer cancel()
	dir, err := normalizeWorkspaceDir(record.WorkspaceDir)
	if err != nil {
		return address, c.classifyForward(err, usedAddress, usedPID, usedPIDKnown)
	}
	workspace, err := c.resolveWorkspace(ctx, client, address, dir)
	if err != nil {
		return address, c.classifyForward(err, usedAddress, usedPID, usedPIDKnown)
	}
	departed, err := c.deliver(ctx, client, address, workspace, requestRecord)
	if err == nil {
		c.markServing(address)
		return address, nil
	}
	c.expireWorkspace(dir)
	if !departed {
		return address, c.classifyForward(err, usedAddress, usedPID, usedPIDKnown)
	}
	// THE ROSTER MOVED UNDER THE REF THIS ATTEMPT USED, and the refusal says so
	// in the daemon's own words. It is the ordinary shape of a directory being
	// forgotten and registered again — a workspace closed and re-created over
	// the same path — and the record is still perfectly attributable: the
	// roster names that directory RIGHT NOW, under a new id. So the cached ref
	// is dropped (above), the roster is read once more, and the record is sent
	// again with what the roster holds. Reporting the refusal without that one
	// re-read is what cost realtest 9 a whole session's diagnostics.
	fresh, err := c.resolveWorkspace(ctx, client, address, dir)
	if err != nil {
		return address, c.classifyForward(err, usedAddress, usedPID, usedPIDKnown)
	}
	if _, err := c.deliver(ctx, client, address, fresh, requestRecord); err != nil {
		// ONE RE-READ, NOT A LADDER. A refusal that survives the fresh ref is a
		// directory the roster genuinely no longer holds, which the sentinel
		// below already spells and forwardLoop deliberately does not retry.
		c.expireWorkspace(dir)
		return address, c.classifyForward(err, usedAddress, usedPID, usedPIDKnown)
	}
	c.markServing(address)
	return address, nil
}

// deliver sends one attributed record and reads the daemon's answer.
//
// `departed` marks the ONE refusal the caller may act on: the daemon does not
// hold the workspace this ref names. That is a fact about the REF, not about
// the record, so the caller can re-resolve and try again; every other failure
// is about the transport or the record and is final for this attempt.
func (c *Client) deliver(
	ctx context.Context,
	client agentreplv1connect.AgentReplClient,
	address string,
	workspace *workspacev1.WorkspaceRef,
	requestRecord *agentreplv1.ClientLogRecord,
) (departed bool, err error) {
	response, err := client.ClientLog(ctx, connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: workspace,
		Record:    requestRecord,
	}))
	if err != nil {
		return false, fmt.Errorf("ClientLog at %s: %w", address, err)
	}
	switch response.Msg.GetResult().(type) {
	case *agentreplv1.ClientLogResponse_Success:
		return false, nil
	case *agentreplv1.ClientLogResponse_Error:
		if response.Msg.GetError().GetUnknownWorkspace() != nil {
			// THE WORKSPACE DEPARTED WHILE THIS RECORD WAS IN FLIGHT, or the
			// ref came from a roster that has since moved. Either way the id
			// this attempt named is dead. The caller re-resolves once; if the
			// directory is genuinely gone from the roster, this same sentinel
			// is what stops the forwarder for it — forwardLoop does not retry
			// it, narrates it at DEBUG, and persists the record UNATTRIBUTED in
			// the global sink, so nothing is lost.
			return true, fmt.Errorf("ClientLog at %s: workspace %q is no longer registered: %w",
				address, workspace.GetId(), logging.ErrForwardWorkspaceUnresolvable)
		}
		return false, fmt.Errorf("ClientLog at %s was refused", address)
	default:
		return false, fmt.Errorf("ClientLog at %s returned neither success nor error", address)
	}
}

// classifyForward marks a connection/dial failure with
// logging.ErrForwardTargetNotThere when the daemon THIS ATTEMPT dialed is
// provably gone: its advertised pid has died, or daemon.addr no longer names
// the same address (a replacement daemon published, or the file itself
// vanished) since the attempt began. Failing that, it marks the failure with
// logging.ErrForwardTargetBooting when the address this attempt dialed has
// never once been seen accepting (Ready or a prior successful Forward): a
// daemon publishes its address and pid before its listener answers, so an
// alive-but-unreachable pid at a never-served address is presumed BOOTING,
// not stuck. It leaves every other failure unchanged — including a
// connection/dial failure against an address that WAS previously seen
// accepting and whose advertised pid is still alive, which is a genuinely
// stuck daemon and must remain a real WARN candidate — and it never
// reclassifies an app-level failure (an explicit ClientLog refusal, a
// malformed roster) that was never a dial failure to begin with.
func (c *Client) classifyForward(err error, usedAddress string, usedPID int, usedPIDKnown bool) error {
	if err == nil || connect.CodeOf(err) != connect.CodeUnavailable {
		return err
	}
	raw, readErr := os.ReadFile(c.addrPath)
	if readErr != nil {
		// The advertisement disappeared between the attempt and the failure:
		// the target is gone, not merely unreachable.
		return fmt.Errorf("%w: %w", err, logging.ErrForwardTargetNotThere)
	}
	curAddress, curPID, curPIDKnown := parseAdvertisement(string(raw))
	notThere := curAddress != usedAddress ||
		(usedPIDKnown && !processAlive(usedPID)) ||
		(!usedPIDKnown && curPIDKnown && !processAlive(curPID))
	if notThere {
		return fmt.Errorf("%w: %w", err, logging.ErrForwardTargetNotThere)
	}
	if !c.hasSeenServing(usedAddress) {
		return fmt.Errorf("%w: %w", err, logging.ErrForwardTargetBooting)
	}
	return err
}

// resolveWorkspace answers the daemon-minted ref for one NORMALIZED directory,
// from the cache while the roster it came from is still recent enough to stand
// for the roster now, and from the roster itself otherwise.
func (c *Client) resolveWorkspace(
	ctx context.Context,
	client agentreplv1connect.AgentReplClient,
	address string,
	wanted string,
) (*workspacev1.WorkspaceRef, error) {
	if cached := c.cachedWorkspace(address, wanted); cached != nil {
		return cached, nil
	}

	streamCtx, stopStream := context.WithCancel(ctx)
	stream, err := client.WatchWorkspaceRoster(streamCtx,
		connect.NewRequest(&agentreplv1.WatchWorkspaceRosterRequest{}))
	if err != nil {
		stopStream()
		return nil, fmt.Errorf("WatchWorkspaceRoster at %s: %w", address, err)
	}
	defer func() {
		stopStream()
		_ = stream.Close()
	}()
	// The roster topic replays its current, COMPLETE snapshot to every fresh
	// subscriber as the first streamed message (see the daemon's serveTopic).
	// So the first delivered roster is authoritative for "which workspaces
	// exist right now": if it does not name this dir, the dir is not a real
	// workspace -- a temp-root, or an unknown path -- and it will never appear.
	// Conclude UNRESOLVABLE at once rather than holding the standing stream open
	// to the request deadline waiting for a workspace that will never register.
	// This is distinct from the stream never delivering a snapshot at all (it
	// errored or never connected), which stays a transport failure below.
	if stream.Receive() {
		switch stream.Msg().GetPush().(type) {
		case *agentreplv1.WatchWorkspaceRosterResponse_Roster:
		case *agentreplv1.WatchWorkspaceRosterResponse_Ending:
			// THE DAEMON IS STANDING DOWN BEFORE IT DELIVERED A ROSTER: a
			// planned exit, not an answer. It says nothing about whether the
			// dir is a workspace, so it is never read as "absent"; the daemon
			// that sent it is on its way out, which is the restart transient
			// ErrForwardTargetNotThere names, retried against whichever daemon
			// daemon.addr names next.
			return nil, fmt.Errorf("WatchWorkspaceRoster at %s ended as the daemon stood down in a planned exit, before delivering a roster for workspace %q: %w",
				address, wanted, logging.ErrForwardTargetNotThere)
		default:
			return nil, fmt.Errorf("WatchWorkspaceRoster at %s delivered a frame with no push arm this client reads (%T), not a roster for workspace %q",
				address, stream.Msg().GetPush(), wanted)
		}
		ref, err := workspaceRefInRoster(stream.Msg().GetRoster(), wanted)
		if err != nil {
			return nil, fmt.Errorf("resolve workspace ref from roster at %s: %w", address, err)
		}
		if ref != nil {
			c.cacheWorkspace(address, wanted, ref)
			return copyWorkspaceRef(ref), nil
		}
		return nil, fmt.Errorf("workspace %q is absent from the delivered roster at %s: %w", wanted, address, logging.ErrForwardWorkspaceUnresolvable)
	}
	if err := stream.Err(); err != nil {
		return nil, fmt.Errorf("WatchWorkspaceRoster at %s ended before delivering a roster for workspace %q: %w", address, wanted, err)
	}
	return nil, fmt.Errorf("WatchWorkspaceRoster at %s ended before delivering a roster for workspace %q", address, wanted)
}

// normalizeWorkspaceDir matches the daemon registry's spelling: absolute and
// clean, with the deepest existing ancestor symlink-resolved. A transcript can
// outlive its deleted worktree, so requiring the leaf itself to exist would
// make the roster's preserved ref impossible to match.
func normalizeWorkspaceDir(dir string) (string, error) {
	if strings.TrimSpace(dir) == "" {
		return "", fmt.Errorf("a workspace directory is required")
	}
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", fmt.Errorf("resolve workspace directory %q: %w", dir, err)
	}
	abs = filepath.Clean(abs)
	rest := ""
	head := abs
	for {
		if resolved, err := filepath.EvalSymlinks(head); err == nil {
			return filepath.Clean(filepath.Join(resolved, rest)), nil
		}
		parent := filepath.Dir(head)
		if parent == head {
			return "", fmt.Errorf("resolve an existing ancestor of workspace directory %q", dir)
		}
		rest = filepath.Join(filepath.Base(head), rest)
		head = parent
	}
}

func workspaceRefInRoster(roster *frontendv1.WorkspaceRoster, wantedDir string) (*workspacev1.WorkspaceRef, error) {
	var found *workspacev1.WorkspaceRef
	visit := func(ref *workspacev1.WorkspaceRef) error {
		if ref == nil || filepath.Clean(ref.GetDir()) != wantedDir {
			return nil
		}
		if ref.GetId() == "" || ref.GetDir() == "" {
			return fmt.Errorf("the matching roster row carries an incomplete workspace ref")
		}
		if found != nil && found.GetId() != ref.GetId() {
			return fmt.Errorf("workspace dir %q appears under both %q and %q", wantedDir, found.GetId(), ref.GetId())
		}
		found = ref
		return nil
	}
	for _, row := range rosterwalk.AllRows(roster) {
		if err := visit(row.GetWorkspace().GetWorkspace()); err != nil {
			return nil, err
		}
	}
	return found, nil
}

// SetRefReplacedObserver installs the sink told that a directory's cached ref
// was replaced by a different daemon-minted id — the roster forgot the
// workspace and registered the same directory again. It is installed after
// construction because the logger that states it is built FROM this Client.
func (c *Client) SetRefReplacedObserver(observe func(dir, oldID, newID string)) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.refReplaced = observe
}

// cachedWorkspace answers a directory's cached ref only while it may still
// stand for the roster: same daemon, and read within the freshness bound. A
// ref past the bound is left in place rather than deleted, so the resolve that
// replaces it can still name the id it replaced.
func (c *Client) cachedWorkspace(address, dir string) *workspacev1.WorkspaceRef {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.cachedAddress != address {
		c.dropAllLocked(address)
		return nil
	}
	entry, ok := c.workspaces[dir]
	if !ok || entry.ref == nil {
		return nil
	}
	if c.now().Sub(entry.readAt) >= c.freshness {
		return nil
	}
	return copyWorkspaceRef(entry.ref)
}

func (c *Client) cacheWorkspace(address, dir string, ref *workspacev1.WorkspaceRef) {
	c.mu.Lock()
	if c.cachedAddress != address {
		c.dropAllLocked(address)
	}
	previous := c.workspaces[dir]
	c.workspaces[dir] = cachedWorkspaceRef{ref: copyWorkspaceRef(ref), readAt: c.now()}
	observe := c.refReplaced
	c.mu.Unlock()
	if observe == nil || previous.ref == nil || previous.ref.GetId() == ref.GetId() {
		return
	}
	// OUTSIDE THE LOCK, because the observer writes a log record and this
	// Client is the log's own forwarding boundary.
	observe(dir, previous.ref.GetId(), ref.GetId())
}

// dropAllLocked forgets every cached ref because the daemon changed. Ids are
// minted per daemon, so not one of them means anything against the new one.
func (c *Client) dropAllLocked(address string) {
	c.cachedAddress = address
	c.workspaces = map[string]cachedWorkspaceRef{}
}

// expireWorkspace marks one directory's ref unusable while KEEPING it, so the
// resolve that replaces it can still state which id it replaced.
func (c *Client) expireWorkspace(dir string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	entry, ok := c.workspaces[dir]
	if !ok {
		return
	}
	entry.readAt = time.Time{}
	c.workspaces[dir] = entry
}

func copyWorkspaceRef(ref *workspacev1.WorkspaceRef) *workspacev1.WorkspaceRef {
	if ref == nil {
		return nil
	}
	return &workspacev1.WorkspaceRef{Id: ref.GetId(), Dir: ref.GetDir()}
}

// wireContext translates the logger's typed list values into Struct's JSON
// value vocabulary. The logging contract keeps write_ids as []string inside Go;
// structpb deliberately accepts only []any for a repeated JSON value.
func wireContext(in map[string]any) map[string]any {
	out := make(map[string]any, len(in))
	for key, value := range in {
		switch typed := value.(type) {
		case []string:
			values := make([]any, len(typed))
			for i, item := range typed {
				values[i] = item
			}
			out[key] = values
		default:
			out[key] = value
		}
	}
	return out
}

// addressLine is the bare "host:port" a daemon.addr payload advertises: its
// first line, trimmed. A legacy bare-address file is exactly its first line.
func addressLine(raw string) string {
	first, _, _ := strings.Cut(raw, "\n")
	return strings.TrimSpace(first)
}

// pidLinePrefix marks the second line of a daemon.addr advertisement, which
// carries the advertising daemon's process id. This mirrors
// daemon/internal/daemonaddr's ReadAdvertisement, which this package cannot
// import: daemonaddr lives under a different module's internal tree, so the
// on-disk shape is duplicated here rather than shared.
const pidLinePrefix = "pid="

// parseAdvertisement parses a daemon.addr payload into its bare address and,
// when present, its advertiser's pid. pidKnown is false for a legacy
// bare-address file, which names no pid — the same forward-compatible
// contract daemonaddr.ParseAdvertisement implements.
func parseAdvertisement(raw string) (address string, pid int, pidKnown bool) {
	address = addressLine(raw)
	for i, line := range strings.Split(raw, "\n") {
		if i == 0 {
			continue
		}
		trimmed := strings.TrimSpace(line)
		if rest, ok := strings.CutPrefix(trimmed, pidLinePrefix); ok {
			if parsed, err := strconv.Atoi(strings.TrimSpace(rest)); err == nil {
				pid, pidKnown = parsed, true
			}
		}
	}
	return address, pid, pidKnown
}

// processAlive reports whether pid names a live process, via the null signal:
// ESRCH means the process is gone, EPERM means it exists but is owned by
// someone else (still alive), and a nil error means it exists and is ours to
// signal. This mirrors the liveness check daemon/internal/shimclient already
// uses for an adopted shim's process group.
func processAlive(pid int) bool {
	if pid <= 0 {
		return false
	}
	err := syscall.Kill(pid, 0)
	return err == nil || errors.Is(err, syscall.EPERM)
}

func validateAddress(address string) error {
	if address == "" {
		return fmt.Errorf("address is empty")
	}
	host, _, err := net.SplitHostPort(address)
	if err != nil {
		return fmt.Errorf("not host:port: %w", err)
	}
	ip := net.ParseIP(host)
	if ip == nil || !ip.IsLoopback() {
		return fmt.Errorf("host %q is not a loopback IP", host)
	}
	return nil
}

func setLevel(record *agentreplv1.ClientLogRecord, level string) {
	switch level {
	case "debug":
		record.Level = &agentreplv1.ClientLogRecord_Debug{Debug: &agentreplv1.ClientLogLevelDebug{}}
	case "info":
		record.Level = &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}}
	case "warn":
		record.Level = &agentreplv1.ClientLogRecord_Warn{Warn: &agentreplv1.ClientLogLevelWarn{}}
	case "error":
		record.Level = &agentreplv1.ClientLogRecord_Error{Error: &agentreplv1.ClientLogLevelError{}}
	default:
		panic(fmt.Sprintf("sidecar daemonclient: unsupported log level %q", level))
	}
}
