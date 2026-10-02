package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"net/http"
	"sort"
	"strings"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/types/dynamicpb"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/envc"
	"claude-repld/internal/stateroot"
)

// THE `call` VERB: `claude-repld call [-state-dir DIR] [-workspace DIR] <Method> [JSON]`.
//
// ONE DOOR TO EVERY UNARY RPC OF THE SERVING DAEMON. The verb reads the
// AgentRepl service from its own descriptor, so a new rpc needs no code here:
// the request is the method's input message, written as protojson (default
// `{}`), and the answer is printed as protojson. It decides nothing -- the
// daemon does -- and exits non-zero when the call fails or the daemon answers
// the response's `error` arm, so a script can branch on it.
//
// `-workspace DIR` fills the request's `workspace` field (a WorkspaceRef) when
// the request has one and the JSON left it unset: the daemon-minted id is
// read off the daemon's own roster by the workspace's directory, because a
// client never derives an id from a path.
//
// Server-streaming rpcs are refused by name: a stream has no single answer to
// print, and the verbs that need one (merge-queue) own their own reading.

// callVerb is the verb's name on the command line.
const callVerb = "call"

// callInvoker sends one unary call of METHOD to the daemon at ADDRESS.
type callInvoker func(ctx context.Context, address string, method protoreflect.MethodDescriptor, req *dynamicpb.Message) (*dynamicpb.Message, error)

// callRosterLookup answers the roster's WorkspaceRef for the workspace whose
// canonical directory is DIR.
type callRosterLookup func(ctx context.Context, address, dir string) (*workspacev1.WorkspaceRef, error)

// callDeps are the verb's seams onto the daemon, injected so the verb is
// tested without one.
type callDeps struct {
	invoke callInvoker
	lookup callRosterLookup
	// address answers the serving daemon's address for a state-dir flag.
	address func(stateDir string) (string, error)
}

// productionCallDeps reaches the real serving daemon.
func productionCallDeps() callDeps {
	return callDeps{
		invoke: invokeUnary,
		lookup: lookupWorkspaceRef,
		address: func(stateDir string) (string, error) {
			layout, err := stateroot.Root(stateDir, envc.Load().WithStateDir(stateDir).StateDir())
			if err != nil {
				return "", fmt.Errorf("resolve the state root: %w", err)
			}
			return servingAddress(layout)
		},
	}
}

// agentReplService is the service the verb calls into.
func agentReplService() protoreflect.ServiceDescriptor {
	return agentreplv1.File_agentrepl_v1_service_proto.Services().ByName("AgentRepl")
}

// runCallVerb runs the verb and answers the process's exit status.
func runCallVerb(ctx context.Context, args []string, deps callDeps, out, errOut io.Writer) int {
	fs := flag.NewFlagSet(callVerb, flag.ContinueOnError)
	fs.SetOutput(errOut)
	stateDir := fs.String("state-dir", "", "state root, overriding $AGENT_REPL_STATE_DIR")
	workspaceDir := fs.String("workspace", "", "fill the request's workspace from this workspace directory")
	fs.Usage = func() {
		fmt.Fprintln(errOut, "usage: claude-repld call [-state-dir DIR] [-workspace DIR] <Method> [JSON]")
		fmt.Fprintln(errOut, "unary methods: "+strings.Join(unaryMethodNames(), " "))
	}
	if err := fs.Parse(args); err != nil {
		return exitFailure
	}
	if fs.NArg() < 1 || fs.NArg() > 2 {
		fs.Usage()
		return exitFailure
	}
	method := agentReplService().Methods().ByName(protoreflect.Name(fs.Arg(0)))
	if method == nil {
		fmt.Fprintf(errOut, "claude-repld call: AgentRepl has no method %q\n", fs.Arg(0))
		fs.Usage()
		return exitFailure
	}
	if method.IsStreamingServer() || method.IsStreamingClient() {
		fmt.Fprintf(errOut, "claude-repld call: %s is a stream, and call sends only unary rpcs\n", method.Name())
		return exitFailure
	}
	body := "{}"
	if fs.NArg() == 2 {
		body = fs.Arg(1)
	}
	req := dynamicpb.NewMessage(method.Input())
	if err := protojson.Unmarshal([]byte(body), req); err != nil {
		fmt.Fprintf(errOut, "claude-repld call: the request is not a %s: %v\n", method.Input().FullName(), err)
		return exitFailure
	}
	address, err := deps.address(*stateDir)
	if err != nil {
		fmt.Fprintf(errOut, "claude-repld call: %v\n", err)
		return exitFailure
	}
	if *workspaceDir != "" {
		if err := fillWorkspace(ctx, deps, address, req, *workspaceDir); err != nil {
			fmt.Fprintf(errOut, "claude-repld call: %v\n", err)
			return exitFailure
		}
	}
	resp, err := deps.invoke(ctx, address, method, req)
	if err != nil {
		fmt.Fprintf(errOut, "claude-repld call: the daemon at %s failed %s: %v\n", address, method.Name(), err)
		return exitFailure
	}
	text, err := protojson.MarshalOptions{Multiline: true, EmitUnpopulated: false}.Marshal(resp)
	if err != nil {
		fmt.Fprintf(errOut, "claude-repld call: render the %s answer: %v\n", method.Name(), err)
		return exitFailure
	}
	fmt.Fprintln(out, string(text))
	if answeredError(resp) {
		fmt.Fprintf(errOut, "claude-repld call: the daemon answered %s with its error arm\n", method.Name())
		return exitFailure
	}
	return exitSuccess
}

// fillWorkspace sets REQ's `workspace` field from the roster row of DIR, when
// the request carries such a field and the JSON left it unset.
func fillWorkspace(ctx context.Context, deps callDeps, address string, req *dynamicpb.Message, dir string) error {
	field := req.Descriptor().Fields().ByName("workspace")
	if field == nil || field.Message() == nil ||
		field.Message().FullName() != (&workspacev1.WorkspaceRef{}).ProtoReflect().Descriptor().FullName() {
		return fmt.Errorf("-workspace was given, but %s has no workspace field", req.Descriptor().FullName())
	}
	if req.Has(field) {
		return nil
	}
	ref, err := deps.lookup(ctx, address, canonicalDir(dir))
	if err != nil {
		return err
	}
	dyn := dynamicpb.NewMessage(field.Message())
	raw, err := proto.Marshal(ref)
	if err != nil {
		return fmt.Errorf("encode the workspace reference: %w", err)
	}
	if err := proto.Unmarshal(raw, dyn); err != nil {
		return fmt.Errorf("decode the workspace reference: %w", err)
	}
	req.Set(field, protoreflect.ValueOfMessage(dyn))
	return nil
}

// answeredError reports whether RESP's outcome oneof set its `error` arm.
func answeredError(resp *dynamicpb.Message) bool {
	field := resp.Descriptor().Fields().ByName("error")
	return field != nil && resp.Has(field)
}

// unaryMethodNames lists the service's unary rpcs, sorted, for the usage line.
func unaryMethodNames() []string {
	methods := agentReplService().Methods()
	var names []string
	for i := 0; i < methods.Len(); i++ {
		m := methods.Get(i)
		if !m.IsStreamingServer() && !m.IsStreamingClient() {
			names = append(names, string(m.Name()))
		}
	}
	sort.Strings(names)
	return names
}

// invokeUnary is the production invoker: a connect client over dynamic
// messages, built for the one method.
func invokeUnary(ctx context.Context, address string, method protoreflect.MethodDescriptor, req *dynamicpb.Message) (*dynamicpb.Message, error) {
	return invokeUnaryOver(ctx, h2cClient(), "http://"+address, method, req)
}

// invokeUnaryOver sends REQ as METHOD to BASE over CLIENT.
func invokeUnaryOver(ctx context.Context, client *http.Client, base string, method protoreflect.MethodDescriptor, req *dynamicpb.Message) (*dynamicpb.Message, error) {
	procedure := fmt.Sprintf("/%s/%s", method.Parent().FullName(), method.Name())
	caller := connect.NewClient[dynamicpb.Message, dynamicpb.Message](client, base+procedure,
		connect.WithSchema(method),
		connect.WithResponseInitializer(func(_ connect.Spec, message any) error {
			dyn, ok := message.(*dynamicpb.Message)
			if !ok {
				return fmt.Errorf("the response holder is a %T, not a dynamic message", message)
			}
			*dyn = *dynamicpb.NewMessage(method.Output())
			return nil
		}))
	resp, err := caller.CallUnary(ctx, connect.NewRequest(req))
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

// errRosterFound ends the roster read once the first roster has been looked
// at; it never escapes lookupWorkspaceRef.
var errRosterFound = errors.New("roster read")

// lookupWorkspaceRef reads the daemon's roster once and answers the row whose
// canonical directory is DIR.
func lookupWorkspaceRef(ctx context.Context, address, dir string) (*workspacev1.WorkspaceRef, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	var ref *workspacev1.WorkspaceRef
	err := dialMergeQueueDaemon(address).WatchRoster(ctx, func(roster *frontendv1.WorkspaceRoster) error {
		if row := findRosterRow(roster, dir); row != nil {
			ref = row.GetWorkspace().GetWorkspace()
		}
		return errRosterFound
	})
	if err != nil && !errors.Is(err, errRosterFound) {
		return nil, fmt.Errorf("read the daemon's roster: %w", err)
	}
	if ref == nil {
		return nil, fmt.Errorf("the daemon's roster holds no workspace at %s", dir)
	}
	return ref, nil
}
