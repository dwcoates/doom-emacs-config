// Package daemonclient owns the shim-claude-sidecar's ClientLog integration
// boundary. It resolves the daemon's current loopback address from the shared
// state root and reads the daemon-minted ref from the global workspace roster,
// so a daemon handover changes both the destination and the authoritative ref
// without restarting the launchd-managed sidecar.
package daemonclient

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
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

// Forward resolves the current daemon and sends one record. The returned
// address is the rate-limit identity the logger uses if err is non-nil. Before
// daemon.addr can be read, its path is the only destination identity available.
func (c *Client) Forward(record logging.ForwardRecord) (string, error) {
	address := c.addrPath
	raw, err := os.ReadFile(c.addrPath)
	if err != nil {
		return address, fmt.Errorf("read daemon address %q: %w", c.addrPath, err)
	}
	address = strings.TrimSpace(string(raw))
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
		return address, err
	}
	response, err := client.ClientLog(ctx, connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: workspace,
		Record:    requestRecord,
	}))
	if err != nil {
		c.invalidateWorkspace(address, workspace.GetId())
		return address, fmt.Errorf("ClientLog at %s: %w", address, err)
	}
	switch response.Msg.GetResult().(type) {
	case *agentreplv1.ClientLogResponse_Success:
		return address, nil
	case *agentreplv1.ClientLogResponse_Error:
		c.invalidateWorkspace(address, workspace.GetId())
		return address, fmt.Errorf("ClientLog at %s was refused", address)
	default:
		c.invalidateWorkspace(address, workspace.GetId())
		return address, fmt.Errorf("ClientLog at %s returned neither success nor error", address)
	}
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
	for stream.Receive() {
		ref, err := workspaceRefInRoster(stream.Msg().GetRoster(), wanted)
		if err != nil {
			return nil, fmt.Errorf("resolve workspace ref from roster at %s: %w", address, err)
		}
		if ref != nil {
			c.cacheWorkspace(address, wanted, ref)
			return copyWorkspaceRef(ref), nil
		}
	}
	if err := stream.Err(); err != nil {
		return nil, fmt.Errorf("WatchWorkspaceRoster at %s ended before workspace %q appeared: %w", address, wanted, err)
	}
	return nil, fmt.Errorf("WatchWorkspaceRoster at %s ended before workspace %q appeared", address, wanted)
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
