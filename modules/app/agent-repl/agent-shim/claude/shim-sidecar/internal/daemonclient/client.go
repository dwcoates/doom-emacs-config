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
	"agentrepl/shim-claude-sidecar/internal/logging"
	"connectrpc.com/connect"
	"google.golang.org/protobuf/types/known/structpb"
)

const requestTimeout = 2 * time.Second

// readinessTimeout bounds the loopback connect that decides whether the daemon
// is live. A booting daemon has bound its listener and published daemon.addr
// but is not yet accepting, so the connect either fails fast or the kernel
// queues it; either way a short bound keeps a readiness probe from riding the
// full request timeout on every retry rung.
const readinessTimeout = 250 * time.Millisecond

// Client resolves daemon.addr and forwards one file-scoped diagnostic through
// agentrepl.v1.AgentRepl.ClientLog. The HTTP client is shared so sequential
// records reuse the loopback connection; the generated Connect client is
// rebuilt from the current address because daemon handover changes it.
type Client struct {
	addrPath string
	http     *http.Client

	mu            sync.Mutex
	cachedAddress string
	cachedDir     string
	cachedRef     *workspacev1.WorkspaceRef
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
	workspace, err := c.resolveWorkspace(ctx, client, address, record.WorkspaceDir)
	if err != nil {
		return address, c.classifyForward(err, usedAddress, usedPID, usedPIDKnown)
	}
	response, err := client.ClientLog(ctx, connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: workspace,
		Record:    requestRecord,
	}))
	if err != nil {
		c.invalidateWorkspace(address, workspace.GetId())
		return address, c.classifyForward(fmt.Errorf("ClientLog at %s: %w", address, err), usedAddress, usedPID, usedPIDKnown)
	}
	switch response.Msg.GetResult().(type) {
	case *agentreplv1.ClientLogResponse_Success:
		c.markServing(address)
		return address, nil
	case *agentreplv1.ClientLogResponse_Error:
		c.invalidateWorkspace(address, workspace.GetId())
		return address, fmt.Errorf("ClientLog at %s was refused", address)
	default:
		c.invalidateWorkspace(address, workspace.GetId())
		return address, fmt.Errorf("ClientLog at %s returned neither success nor error", address)
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

func (c *Client) resolveWorkspace(
	ctx context.Context,
	client agentreplv1connect.AgentReplClient,
	address string,
	dir string,
) (*workspacev1.WorkspaceRef, error) {
	wanted, err := normalizeWorkspaceDir(dir)
	if err != nil {
		return nil, err
	}
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
	var walk func([]*frontendv1.RosterRow) error
	walk = func(rows []*frontendv1.RosterRow) error {
		for _, row := range rows {
			if err := visit(row.GetWorkspace().GetWorkspace()); err != nil {
				return err
			}
			if err := walk(row.GetChildren()); err != nil {
				return err
			}
		}
		return nil
	}
	for _, section := range roster.GetRepository().GetSections() {
		if err := walk(section.GetRows().GetRows()); err != nil {
			return nil, err
		}
	}
	for _, section := range roster.GetTask().GetSections() {
		if err := walk(section.GetRows().GetRows()); err != nil {
			return nil, err
		}
	}
	if err := walk(roster.GetRecentlyMerged().GetRows().GetRows()); err != nil {
		return nil, err
	}
	return found, nil
}

func (c *Client) cachedWorkspace(address, dir string) *workspacev1.WorkspaceRef {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.cachedAddress != address || c.cachedDir != dir || c.cachedRef == nil {
		return nil
	}
	return copyWorkspaceRef(c.cachedRef)
}

func (c *Client) cacheWorkspace(address, dir string, ref *workspacev1.WorkspaceRef) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.cachedAddress = address
	c.cachedDir = dir
	c.cachedRef = copyWorkspaceRef(ref)
}

func (c *Client) invalidateWorkspace(address, id string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.cachedAddress == address && c.cachedRef != nil && c.cachedRef.GetId() == id {
		c.cachedAddress = ""
		c.cachedDir = ""
		c.cachedRef = nil
	}
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
