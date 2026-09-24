package main

import (
	"context"
	"crypto/tls"
	"errors"
	"flag"
	"fmt"
	"io"
	"net"
	"net/http"
	"strings"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"claude-repld/internal/daemonaddr"
	"claude-repld/internal/envc"
	"claude-repld/internal/stateroot"
)

// THE `deploy` VERB: `claude-repld deploy [-force] [-state-dir DIR]`.
//
// It is a THIN CALLER of the serving daemon's Deploy rpc and decides nothing:
// the daemon builds, judges staleness and restarts what is out of date (see
// internal/deploy). The verb prints one line per component decision and exits
// non-zero on any refusal or failure, with the daemon's own words.

// deployVerb is the verb's name on the command line.
const deployVerb = "deploy"

// deployCaller is the slice of the daemon's client the verb uses.
type deployCaller interface {
	Deploy(ctx context.Context, req *connect.Request[agentreplv1.DeployRequest]) (*connect.Response[agentreplv1.DeployResponse], error)
}

// deployDialer builds a client for the daemon at a loopback address.
type deployDialer func(address string) deployCaller

// dialDaemon is the production dialer: h2c on the daemon's one origin.
func dialDaemon(address string) deployCaller {
	client := &http.Client{Transport: &http2.Transport{
		AllowHTTP: true,
		DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
			var dialer net.Dialer
			return dialer.DialContext(ctx, network, addr)
		},
	}}
	return agentreplv1connect.NewAgentReplClient(client, "http://"+address)
}

// runDeployVerb runs the verb and answers the process's exit status.
func runDeployVerb(ctx context.Context, args []string, dial deployDialer, out, errOut io.Writer) int {
	fs := flag.NewFlagSet(deployVerb, flag.ContinueOnError)
	fs.SetOutput(errOut)
	force := fs.Bool("force", false, "do not wait for in-flight work: bounce every stale shim and hand the daemon over at once (ENDS RUNNING TURNS)")
	stateDir := fs.String("state-dir", "", "state root, overriding $AGENT_REPL_STATE_DIR")
	if err := fs.Parse(args); err != nil {
		return exitFailure
	}
	if fs.NArg() != 0 {
		fmt.Fprintf(errOut, "claude-repld deploy: unexpected arguments: %s\n", strings.Join(fs.Args(), " "))
		return exitFailure
	}
	layout, err := stateroot.Root(*stateDir, envc.Load().WithStateDir(*stateDir).StateDir())
	if err != nil {
		fmt.Fprintf(errOut, "claude-repld deploy: resolve the state root: %v\n", err)
		return exitFailure
	}
	advert, err := daemonaddr.ReadAdvertisement(layout.DaemonAddr())
	if err != nil {
		fmt.Fprintf(errOut, "claude-repld deploy: no daemon is serving: read %s: %v\n", layout.DaemonAddr(), err)
		return exitFailure
	}
	if advert.Address == "" {
		fmt.Fprintf(errOut, "claude-repld deploy: no daemon is serving: %s names no address\n", layout.DaemonAddr())
		return exitFailure
	}
	resp, err := dial(advert.Address).Deploy(ctx, connect.NewRequest(&agentreplv1.DeployRequest{Force: *force}))
	if err != nil {
		fmt.Fprintf(errOut, "claude-repld deploy: the daemon at %s failed the deploy: %v\n", advert.Address, err)
		return exitFailure
	}
	switch result := resp.Msg.GetResult().(type) {
	case *agentreplv1.DeployResponse_Success:
		for _, component := range result.Success.GetComponents() {
			line, err := describeOutcome(component)
			if err != nil {
				fmt.Fprintf(errOut, "claude-repld deploy: %v\n", err)
				return exitFailure
			}
			fmt.Fprintln(out, line)
		}
		return exitSuccess
	case *agentreplv1.DeployResponse_Error:
		fmt.Fprintf(errOut, "claude-repld deploy: NOT DEPLOYED: %s\n", describeRefusal(result.Error))
		return exitFailure
	default:
		fmt.Fprintf(errOut, "claude-repld deploy: the daemon answered with no result arm\n")
		return exitFailure
	}
}

// componentName is a component's name on the verb's output.
func componentName(c agentreplv1.DeployComponent) string {
	return strings.ToLower(strings.TrimPrefix(c.String(), "DEPLOY_COMPONENT_"))
}

// errUnnamedOutcome is an outcome whose arm this verb cannot name.
var errUnnamedOutcome = errors.New("the daemon answered an outcome with no arm")

// describeOutcome renders one component's decision as one line.
func describeOutcome(o *agentreplv1.DeployComponentOutcome) (string, error) {
	head := fmt.Sprintf("%s build=%s", componentName(o.GetComponent()), o.GetBuild())
	switch arm := o.GetOutcome().(type) {
	case *agentreplv1.DeployComponentOutcome_UpToDate:
		return head + " up-to-date", nil
	case *agentreplv1.DeployComponentOutcome_Restarted:
		return head + " restarted", nil
	case *agentreplv1.DeployComponentOutcome_HandingOver:
		h := arm.HandingOver
		return fmt.Sprintf("%s handing-over workspaces=%d busy=%d forced=%t", head, h.GetWorkspaces(), h.GetBusy(), h.GetForced()), nil
	case *agentreplv1.DeployComponentOutcome_Shims:
		parts := make([]string, 0, len(arm.Shims.GetBounces()))
		for _, b := range arm.Shims.GetBounces() {
			switch when := b.GetWhen().(type) {
			case *agentreplv1.DeployShimBounce_BouncedNow:
				parts = append(parts, fmt.Sprintf("%s:bounced-now(forced=%t)", b.GetWorkspace(), when.BouncedNow.GetForced()))
			case *agentreplv1.DeployShimBounce_Registered:
				parts = append(parts, fmt.Sprintf("%s:registered(turn=%t,detached=%d)", b.GetWorkspace(),
					when.Registered.GetTurnInFlight(), when.Registered.GetDetachedWork()))
			default:
				return "", fmt.Errorf("%w: a shim bounce for %s names no when", errUnnamedOutcome, b.GetWorkspace())
			}
		}
		return head + " shims " + strings.Join(parts, " "), nil
	case *agentreplv1.DeployComponentOutcome_ReloadPushed:
		return fmt.Sprintf("%s reload-pushed recipients=%d", head, arm.ReloadPushed.GetRecipients()), nil
	case *agentreplv1.DeployComponentOutcome_DeferredToSuccessor:
		return head + " deferred-to-successor", nil
	default:
		return "", fmt.Errorf("%w: %s", errUnnamedOutcome, componentName(o.GetComponent()))
	}
}

// describeRefusal renders the daemon's typed refusal in its own words.
func describeRefusal(e *agentreplv1.DeployError) string {
	switch cause := e.GetCause().(type) {
	case *agentreplv1.DeployError_BuildFailed:
		b := cause.BuildFailed
		return fmt.Sprintf("the %s build failed (output in %s):\n%s", b.GetStep(), b.GetLog(), b.GetDetail())
	case *agentreplv1.DeployError_AlreadyDeploying:
		return "a deploy is already running; ask again when it ends"
	case *agentreplv1.DeployError_AlreadyRollingOut:
		return "a handover is already in flight, waiting on " + strings.Join(cause.AlreadyRollingOut.GetWaitingOn(), ", ")
	case *agentreplv1.DeployError_Joining:
		return "the daemon is a successor still joining a handover"
	case *agentreplv1.DeployError_ServiceRestartFailed:
		s := cause.ServiceRestartFailed
		return fmt.Sprintf("the %s did not come back onto the fresh build: %s", componentName(s.GetComponent()), s.GetDetail())
	case *agentreplv1.DeployError_InstallFailed:
		i := cause.InstallFailed
		return fmt.Sprintf("the %s build could not be installed: %s", componentName(i.GetComponent()), i.GetDetail())
	default:
		return "the daemon refused with no cause arm"
	}
}
