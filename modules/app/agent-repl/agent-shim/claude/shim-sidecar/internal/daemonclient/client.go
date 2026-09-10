// Package daemonclient owns the shim-claude-sidecar's ClientLog integration
// boundary. It resolves the daemon's current loopback address from the shared
// state root for every forwarded record, so a daemon handover changes the
// destination without restarting the launchd-managed sidecar.
package daemonclient

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
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
	contextValue, err := structpb.NewStruct(record.Context)
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
	response, err := client.ClientLog(ctx, connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: record.WorkspaceID, Dir: record.WorkspaceDir},
		Record:    requestRecord,
	}))
	if err != nil {
		return address, fmt.Errorf("ClientLog at %s: %w", address, err)
	}
	switch response.Msg.GetResult().(type) {
	case *agentreplv1.ClientLogResponse_Success:
		return address, nil
	case *agentreplv1.ClientLogResponse_Error:
		return address, fmt.Errorf("ClientLog at %s was refused", address)
	default:
		return address, fmt.Errorf("ClientLog at %s returned neither success nor error", address)
	}
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
