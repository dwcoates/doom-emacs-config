package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"sort"
	"strings"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/envc"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/protoarm"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/stateroot"
)

// THE AGENT API'S COMMANDS: four nouns over the serving daemon's rpcs.
//
//	agent-repld workspace    create | async-merge | prompt | set | merge | open | close | kill
//	                         compact | status | list | subscribe | unsubscribe | gate
//	agent-repld merge-queue  status | dequeue | move-up | move-down | pause | resume
//	                         disable-racing | enable-racing | subscribe | unsubscribe
//	agent-repld editor       eval
//	agent-repld runtime      rollouts | health
//
// EVERY COMMAND IS ONE RPC (a `set` with several flags is one rpc per flag,
// in order), and its answer is printed as protojson. The daemon decides
// everything; the command never touches git. A command exits 0 on success,
// exitRefused when the daemon answers the response's `error` arm, and
// exitFailure when the call itself fails.
//
// WHO ASKED IS THE CALLING WORKSPACE: the workspace whose worktree contains
// the caller's working directory, read off the daemon's roster. A command
// run from a directory that is no workspace is refused, unless `-as-user`
// says the user asks (a person at a shell, never an agent).
//
// A WORKSPACE IS NAMED by its roster name, by a directory (anything with a
// `/`), or as `here` for the calling workspace.
//
// NOTHING IS DEFAULTED that the contract makes a choice: a create names its
// repository, topology and context; a merge names its placement.

// The nouns' names on the command line.
const (
	workspaceNoun  = "workspace"
	mergeQueueNoun = "merge-queue"
	editorNoun     = "editor"
	runtimeNoun    = "runtime"
)

// exitRefused is a command the daemon answered with its error arm.
const exitRefused = 6

// here names the calling workspace, and its repository.
const here = "here"

// cliDeps are the commands' seams onto the daemon, injected so the commands
// are tested without one.
type cliDeps struct {
	// address answers the serving daemon's address for a state-dir flag.
	address func(stateDir string) (string, error)
	// client is the daemon's client at an address.
	client func(address string) agentreplv1connect.AgentReplClient
	// roster reads the daemon's roster once.
	roster func(ctx context.Context, client agentreplv1connect.AgentReplClient) (*frontendv1.WorkspaceRoster, error)
	getwd  func() (string, error)
	// mintKey mints an idempotency key.
	mintKey func(prefix string) (string, error)
}

// productionCLIDeps reaches the real serving daemon.
func productionCLIDeps() cliDeps {
	return cliDeps{
		address: func(stateDir string) (string, error) {
			layout, err := stateroot.Root(stateDir, envc.Load().WithStateDir(stateDir).StateDir())
			if err != nil {
				return "", fmt.Errorf("resolve the state root: %w", err)
			}
			return servingAddress(layout)
		},
		client:  newDaemonClient,
		roster:  readRoster,
		getwd:   os.Getwd,
		mintKey: prompthandler.MintKey,
	}
}

// readRoster reads the first roster the daemon pushes.
func readRoster(ctx context.Context, client agentreplv1connect.AgentReplClient) (*frontendv1.WorkspaceRoster, error) {
	stream, err := openDaemonStream(ctx, client.WatchWorkspaceRoster, &agentreplv1.WatchWorkspaceRosterRequest{})
	if err != nil {
		return nil, err
	}
	defer stream.Close()
	if stream.Receive() {
		if roster := stream.Msg().GetRoster(); roster != nil {
			return roster, nil
		}
		return nil, errors.New("the daemon's first roster frame carried no roster")
	}
	if err := stream.Err(); err != nil {
		return nil, err
	}
	return nil, errors.New("the roster stream ended before its first frame")
}

// cliCommand is one subcommand of a noun.
type cliCommand struct {
	usage string
	run   func(c *cliCall, args []string) int
}

// cliNouns are every noun's subcommands.
var cliNouns = map[string]map[string]cliCommand{
	workspaceNoun: {
		"create":      {usage: "create -repository here|DIR -topology primary|child [-owner WS] [-owner-notice TEXT] -context fresh|fork [-fork WS] [-one-shot] [-prompt TEXT] [-name NAME] [-model M] [-before-merge TEXT] [-after-merge TEXT] [-as-user]", run: workspaceCreate},
		"async-merge": {usage: "async-merge -name NAME -branch BRANCH (-maker WS | -maker-subagent ID) -context-fork WS -target primary|maker -placement back|front [-instruction TEXT] [-close-after WS] [-cleanup-worktree] [-compact-after] [-before-merge TEXT] [-after-merge TEXT] [-version-part minor|patch -changelog-line TEXT]", run: workspaceAsyncMerge},
		"prompt":      {usage: "prompt WS TEXT", run: workspacePrompt},
		"set":         {usage: "set WS [-hidden|-shown] [-priority p05|p1|p2|p3|none] [-parent WS|-top-level] [-one-shot|-not-one-shot] [-await WS|-not-awaiting] [-as-user]", run: workspaceSet},
		"merge":       {usage: "merge WS -placement back|front [-merged-upstream] [-version-part minor|patch -changelog-line TEXT] [-as-user]", run: workspaceMerge},
		"open":        {usage: "open WS", run: workspaceOpen},
		"close":       {usage: "close WS [-as-user]", run: workspaceClose},
		"kill":        {usage: "kill WS", run: workspaceKill},
		"compact":     {usage: "compact WS [-instructions TEXT] [-as-user]", run: workspaceCompact},
		"status":      {usage: "status WS", run: workspaceStatus},
		"list":        {usage: "list", run: workspaceList},
		"subscribe":   {usage: "subscribe WS -events turn_started,turn_ended,needs_input,landed,closed", run: workspaceSubscribe},
		"unsubscribe": {usage: "unsubscribe WS", run: workspaceUnsubscribe},
		"gate":        {usage: "gate WS pay|clear|compact|compact-and-resume [-model M -scope all|prompts|responses]", run: workspaceGate},
	},
	mergeQueueNoun: {
		"status":         {usage: "status [-repository here|DIR]", run: queueStatus},
		"dequeue":        {usage: "dequeue WS", run: queueDequeue},
		"move-up":        {usage: "move-up WS", run: queueMove(true)},
		"move-down":      {usage: "move-down WS", run: queueMove(false)},
		"pause":          {usage: "pause [-repository here|DIR]", run: queuePause(true)},
		"resume":         {usage: "resume [-repository here|DIR]", run: queuePause(false)},
		"disable-racing": {usage: "disable-racing", run: queueRacing(false)},
		"enable-racing":  {usage: "enable-racing", run: queueRacing(true)},
		"subscribe":      {usage: "subscribe -detail every_change|advances|depletes [-repository here|DIR]", run: queueSubscribe},
		"unsubscribe":    {usage: "unsubscribe [-repository here|DIR]", run: queueUnsubscribe},
	},
	editorNoun: {
		"eval": {usage: "eval [-workspace WS] TEXT", run: editorEval},
	},
	runtimeNoun: {
		"rollouts": {usage: "rollouts hold|release", run: runtimeRollouts},
		"health":   {usage: "health", run: runtimeHealth},
	},
}

// runNoun runs one noun's subcommand and answers the process's exit status.
func runNoun(ctx context.Context, noun string, args []string, deps cliDeps, out, errOut io.Writer) int {
	commands := cliNouns[noun]
	if len(args) == 0 || commands[args[0]].run == nil {
		fmt.Fprintf(errOut, "usage: agent-repld %s <command>\n", noun)
		names := make([]string, 0, len(commands))
		for name := range commands {
			names = append(names, name)
		}
		sort.Strings(names)
		for _, name := range names {
			fmt.Fprintf(errOut, "  agent-repld %s %s\n", noun, commands[name].usage)
		}
		return exitFailure
	}
	command := commands[args[0]]
	c := &cliCall{ctx: ctx, deps: deps, out: out, errOut: errOut, name: noun + " " + args[0], usage: command.usage}
	return command.run(c, args[1:])
}

// cliCall is one command's run.
type cliCall struct {
	ctx    context.Context
	deps   cliDeps
	out    io.Writer
	errOut io.Writer
	name   string
	usage  string

	client agentreplv1connect.AgentReplClient
	roster *frontendv1.WorkspaceRoster
}

// flags is the command's flag set, carrying the shared -state-dir.
func (c *cliCall) flags() (*flag.FlagSet, *string) {
	fs := flag.NewFlagSet(c.name, flag.ContinueOnError)
	fs.SetOutput(c.errOut)
	fs.Usage = func() { fmt.Fprintf(c.errOut, "usage: agent-repld %s %s\n", strings.Fields(c.name)[0], c.usage) }
	return fs, fs.String("state-dir", "", "state root, overriding $AGENT_REPL_STATE_DIR")
}

// parse parses ARGS, interleaving positionals and flags: every flag is read
// wherever it stands, and the positionals answered in order.
func (c *cliCall) parse(fs *flag.FlagSet, args []string) ([]string, bool) {
	var positionals []string
	for {
		if err := fs.Parse(args); err != nil {
			return nil, false
		}
		if fs.NArg() == 0 {
			return positionals, true
		}
		positionals = append(positionals, fs.Arg(0))
		args = fs.Args()[1:]
	}
}

// dial connects to the serving daemon.
func (c *cliCall) dial(stateDir string) bool {
	address, err := c.deps.address(stateDir)
	if err != nil {
		return c.failed("%v", err)
	}
	c.client = c.deps.client(address)
	return true
}

// failed reports a failure of the command itself, and answers false.
func (c *cliCall) failed(format string, a ...any) bool {
	fmt.Fprintf(c.errOut, "agent-repld %s: %s\n", c.name, fmt.Sprintf(format, a...))
	return false
}

// usageFailed reports a malformed command and answers its exit.
func (c *cliCall) usageFailed(format string, a ...any) int {
	c.failed(format, a...)
	fmt.Fprintf(c.errOut, "usage: agent-repld %s %s\n", strings.Fields(c.name)[0], c.usage)
	return exitFailure
}

// readRoster reads the roster once per command.
func (c *cliCall) readRoster() (*frontendv1.WorkspaceRoster, bool) {
	if c.roster != nil {
		return c.roster, true
	}
	roster, err := c.deps.roster(c.ctx, c.client)
	if err != nil {
		return nil, c.failed("read the daemon's roster: %v", err)
	}
	c.roster = roster
	return roster, true
}

// calling answers the calling workspace's ref: the workspace whose worktree
// contains the working directory.
func (c *cliCall) calling() (*workspacev1.WorkspaceRef, bool) {
	cwd, err := c.deps.getwd()
	if err != nil {
		return nil, c.failed("read the working directory: %v", err)
	}
	roster, ok := c.readRoster()
	if !ok {
		return nil, false
	}
	row := callingRow(roster, canonicalDir(cwd))
	if row == nil {
		return nil, c.failed("%s is inside no workspace the daemon serves; run this from a workspace, or pass -as-user when the user asks", cwd)
	}
	return row.GetWorkspace().GetWorkspace(), true
}

// named answers the workspace NAME names: `here`, a directory, or a roster
// name, which must name exactly one workspace.
func (c *cliCall) named(name string) (*workspacev1.WorkspaceRef, bool) {
	if name == here {
		return c.calling()
	}
	roster, ok := c.readRoster()
	if !ok {
		return nil, false
	}
	if strings.Contains(name, "/") {
		row := findRosterRow(roster, canonicalDir(name))
		if row == nil {
			return nil, c.failed("the daemon's roster holds no workspace at %s", name)
		}
		return row.GetWorkspace().GetWorkspace(), true
	}
	var found []*frontendv1.RosterRow
	for _, row := range rosterwalk.AllRows(roster) {
		if row.GetName().GetText() == name {
			found = append(found, row)
		}
	}
	switch len(found) {
	case 0:
		return nil, c.failed("the daemon's roster holds no workspace named %q", name)
	case 1:
		return found[0].GetWorkspace().GetWorkspace(), true
	}
	return nil, c.failed("%d workspaces are named %q; name one by its directory", len(found), name)
}

// requester answers who asks: the user when ASUSER, else the calling
// workspace.
func (c *cliCall) requester(asUser bool) (*agentreplv1.WorkspaceRequester, bool) {
	if asUser {
		return &agentreplv1.WorkspaceRequester{Asker: &agentreplv1.WorkspaceRequester_User{User: &agentreplv1.WorkspaceRequesterUser{}}}, true
	}
	ref, ok := c.calling()
	if !ok {
		return nil, false
	}
	return &agentreplv1.WorkspaceRequester{Asker: &agentreplv1.WorkspaceRequester_Workspace{Workspace: &agentreplv1.WorkspaceRequesterWorkspace{Workspace: ref}}}, true
}

// repository answers the repository NAME names: `here` (the calling
// workspace's, off its status) or a main-checkout directory.
func (c *cliCall) repository(name string) (*workspacev1.RepositoryRef, bool) {
	if name != here {
		return &workspacev1.RepositoryRef{Dir: canonicalDir(name)}, true
	}
	ref, ok := c.calling()
	if !ok {
		return nil, false
	}
	resp, err := c.client.GetWorkspaceStatus(c.ctx, connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{Workspace: ref}))
	if err != nil {
		return nil, c.failed("read the calling workspace's status: %v", err)
	}
	repository := resp.Msg.GetSuccess().GetStatus().GetRepository()
	if repository.GetId() == "" {
		return nil, c.failed("the daemon names no repository for the calling workspace: %v", resp.Msg.GetResult())
	}
	return repository, true
}

// answer prints an rpc's answer and answers the command's exit.
func answer[Res any](c *cliCall, resp *connect.Response[Res], err error) int {
	if err != nil {
		c.failed("the daemon failed the call: %v", err)
		return exitFailure
	}
	msg, ok := any(resp.Msg).(proto.Message)
	if !ok {
		c.failed("the answer is a %T, no message", resp.Msg)
		return exitFailure
	}
	text, err := protojson.MarshalOptions{Multiline: true}.Marshal(msg)
	if err != nil {
		c.failed("render the answer: %v", err)
		return exitFailure
	}
	fmt.Fprintln(c.out, string(text))
	if refusedAnswer(msg) {
		fmt.Fprintf(c.errOut, "agent-repld %s: the daemon refused\n", c.name)
		return exitRefused
	}
	return exitSuccess
}

// refusedAnswer reports whether MSG set its `error` arm.
func refusedAnswer(msg proto.Message) bool {
	m := msg.ProtoReflect()
	field := m.Descriptor().Fields().ByName(protoreflect.Name("error"))
	return field != nil && m.Has(field)
}

// optionalSaid is TEXT as a prompt, nil when it is empty.
func optionalSaid(text string) *conversationv1.UserSaid {
	if text == "" {
		return nil
	}
	return feed.SaidText(text)
}

// mergeActions are the before and after actions a create or an async merge
// names, nil when it names neither.
func mergeActions(before, after string) *agentreplv1.CreateWorkspaceMergeActions {
	if before == "" && after == "" {
		return nil
	}
	return &agentreplv1.CreateWorkspaceMergeActions{BeforeWsMerge: optionalSaid(before), PostprocessingPrompt: optionalSaid(after)}
}

// placement reads a -placement flag; nothing is defaulted.
func placement(text string) (*agentreplv1.MergeQueuePlacement, error) {
	switch text {
	case "back":
		return &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Back{Back: &agentreplv1.MergeQueuePlacementBack{}}}, nil
	case "front":
		return &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Front{Front: &agentreplv1.MergeQueuePlacementFront{}}}, nil
	}
	return nil, fmt.Errorf("-placement is back or front, not %q", text)
}

// versionIncrement reads -version-part and -changelog-line, nil when neither
// is given.
func versionIncrement(part, line string) (*agentreplv1.MergeWorkspaceVersionIncrement, error) {
	if part == "" && line == "" {
		return nil, nil
	}
	out := &agentreplv1.MergeWorkspaceVersionIncrement{ChangelogLine: line}
	switch part {
	case "minor":
		out.Increment = &agentreplv1.MergeWorkspaceVersionIncrement_Minor{Minor: &agentreplv1.MergeWorkspaceVersionIncrementMinor{}}
	case "patch":
		out.Increment = &agentreplv1.MergeWorkspaceVersionIncrement_Patch{Patch: &agentreplv1.MergeWorkspaceVersionIncrementPatch{}}
	default:
		return nil, fmt.Errorf("-version-part is minor or patch, not %q", part)
	}
	return out, nil
}

// ---- workspace -----------------------------------------------------------------

func workspaceCreate(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	repository := fs.String("repository", "", "here, or the repository's main checkout")
	topology := fs.String("topology", "", "primary, or child of -owner")
	owner := fs.String("owner", "", "the owner a child is cut from and lands into")
	ownerNotice := fs.String("owner-notice", "", "the notice to an owner that lands the child")
	contextOrigin := fs.String("context", "", "fresh, or fork of -fork")
	fork := fs.String("fork", "", "the workspace whose conversation is forked")
	oneShot := fs.Bool("one-shot", false, "a one-shot, which lands itself when done")
	prompt := fs.String("prompt", "", "the first prompt")
	name := fs.String("name", "", "the workspace's name; unset derives it from the prompt")
	model := fs.String("model", "", "the session's model; unset is the daemon's")
	before := fs.String("before-merge", "", "the action run before the workspace's merge")
	after := fs.String("after-merge", "", "the action run after the workspace's merge")
	asUser := fs.Bool("as-user", false, "the user asks, not the calling workspace")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 0 || *repository == "" || *topology == "" || *contextOrigin == "" {
		return c.usageFailed("-repository, -topology and -context are required, and nothing is positional")
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	req := &agentreplv1.CreateWorkspaceRequest{}
	var good bool
	if req.Repository, good = c.repository(*repository); !good {
		return exitFailure
	}
	if req.Requester, good = c.requester(*asUser); !good {
		return exitFailure
	}
	shape := &agentreplv1.WorkspaceTopology{}
	switch *topology {
	case "primary":
		shape.Shape = &agentreplv1.WorkspaceTopology_Primary{Primary: &agentreplv1.WorkspaceTopologyPrimary{}}
	case "child":
		ownerRef, ok := c.named(*owner)
		if *owner == "" || !ok {
			return c.usageFailed("a child names its -owner")
		}
		child := &agentreplv1.WorkspaceTopologyChild{Owner: ownerRef}
		if *ownerNotice != "" {
			child.OwnerNotice = &agentreplv1.WorkspaceTopologyChildOwnerNotice{Text: *ownerNotice}
		}
		shape.Shape = &agentreplv1.WorkspaceTopology_Child{Child: child}
	default:
		return c.usageFailed("-topology is primary or child, not %q", *topology)
	}
	switch *contextOrigin {
	case "fresh":
		req.ContextOrigin = &agentreplv1.WorkspaceContextOrigin{Origin: &agentreplv1.WorkspaceContextOrigin_Fresh{Fresh: &agentreplv1.WorkspaceContextOriginFresh{}}}
	case "fork":
		forkRef, ok := c.named(*fork)
		if *fork == "" || !ok {
			return c.usageFailed("a fork names the workspace it forks with -fork")
		}
		req.ContextOrigin = &agentreplv1.WorkspaceContextOrigin{Origin: &agentreplv1.WorkspaceContextOrigin_Fork{Fork: &agentreplv1.WorkspaceContextOriginFork{Workspace: forkRef}}}
	default:
		return c.usageFailed("-context is fresh or fork, not %q", *contextOrigin)
	}
	if *oneShot {
		if *prompt == "" {
			return c.usageFailed("a one-shot names its -prompt")
		}
		req.Form = &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{Prompt: feed.SaidText(*prompt), Topology: shape}}
	} else {
		standard := &agentreplv1.CreateWorkspaceStandard{InitialPrompt: optionalSaid(*prompt), Topology: shape, MergeActions: mergeActions(*before, *after)}
		if *name != "" {
			standard.Name = name
		}
		req.Form = &agentreplv1.CreateWorkspaceRequest_Standard{Standard: standard}
	}
	if *model != "" {
		req.Model = model
	}
	resp, err := c.client.CreateWorkspace(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

func workspaceAsyncMerge(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	name := fs.String("name", "", "the merge workspace's name")
	branch := fs.String("branch", "", "the branch that lands")
	maker := fs.String("maker", "", "the workspace whose agent cut the branch")
	subagent := fs.String("maker-subagent", "", "the subagent that made the branch")
	contextFork := fs.String("context-fork", "", "the maker, whose conversation the merge workspace forks")
	target := fs.String("target", "", "primary, or maker")
	place := fs.String("placement", "", "back or front")
	instruction := fs.String("instruction", "", "how the merge workspace prepares the branch")
	closeAfter := fs.String("close-after", "", "a workspace closed once the merge lands")
	cleanup := fs.Bool("cleanup-worktree", false, "remove the branch's worktree once it lands")
	compact := fs.Bool("compact-after", false, "compact the calling workspace once it lands")
	before := fs.String("before-merge", "", "the merge workspace's action before the landing")
	after := fs.String("after-merge", "", "the merge workspace's action after the landing")
	part := fs.String("version-part", "", "minor or patch, for a merge onto agent-repl's master")
	line := fs.String("changelog-line", "", "the version tag's changelog line")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 0 || *contextFork == "" || (*maker == "") == (*subagent == "") {
		return c.usageFailed("exactly one of -maker and -maker-subagent, and -context-fork, are required")
	}
	where, err := placement(*place)
	if err != nil {
		return c.usageFailed("%v", err)
	}
	version, err := versionIncrement(*part, *line)
	if err != nil {
		return c.usageFailed("%v", err)
	}
	async := &agentreplv1.CreateWorkspaceAsyncMerge{Name: *name, Placement: where, CleanupWorktree: *cleanup, CompactAfter: *compact,
		MergeActions: mergeActions(*before, *after), VersionIncrement: version}
	switch *target {
	case "primary":
		async.Target = &agentreplv1.CreateWorkspaceAsyncMergeTarget{Target: &agentreplv1.CreateWorkspaceAsyncMergeTarget_Primary{Primary: &agentreplv1.CreateWorkspaceAsyncMergeTargetPrimary{}}}
	case "maker":
		async.Target = &agentreplv1.CreateWorkspaceAsyncMergeTarget{Target: &agentreplv1.CreateWorkspaceAsyncMergeTarget_Maker{Maker: &agentreplv1.CreateWorkspaceAsyncMergeTargetMaker{}}}
	default:
		return c.usageFailed("-target is primary or maker, not %q", *target)
	}
	if *instruction != "" {
		async.Instruction = instruction
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	source := &agentreplv1.CreateWorkspaceAsyncMergeSource{Branch: *branch, Maker: &agentreplv1.CreateWorkspaceAsyncMergeMaker{}}
	if *subagent != "" {
		source.Maker.Maker = &agentreplv1.CreateWorkspaceAsyncMergeMaker_Subagent{Subagent: &agentreplv1.CreateWorkspaceAsyncMergeMakerSubagent{AgentId: *subagent}}
	} else {
		makerRef, ok := c.named(*maker)
		if !ok {
			return exitFailure
		}
		source.Maker.Maker = &agentreplv1.CreateWorkspaceAsyncMergeMaker_Workspace{Workspace: &agentreplv1.CreateWorkspaceAsyncMergeMakerWorkspace{Workspace: makerRef}}
	}
	async.Source = source
	if *closeAfter != "" {
		ref, ok := c.named(*closeAfter)
		if !ok {
			return exitFailure
		}
		async.CloseAfter = &agentreplv1.CreateWorkspaceAsyncMergeCloseAfter{Workspace: ref}
	}
	forkRef, ok := c.named(*contextFork)
	if !ok {
		return exitFailure
	}
	repository, ok := c.repository(here)
	if !ok {
		return exitFailure
	}
	requester, ok := c.requester(false)
	if !ok {
		return exitFailure
	}
	req := &agentreplv1.CreateWorkspaceRequest{Repository: repository, Requester: requester,
		ContextOrigin: &agentreplv1.WorkspaceContextOrigin{Origin: &agentreplv1.WorkspaceContextOrigin_Fork{Fork: &agentreplv1.WorkspaceContextOriginFork{Workspace: forkRef}}},
		Form:          &agentreplv1.CreateWorkspaceRequest_AsyncMerge{AsyncMerge: async}}
	resp, err := c.client.CreateWorkspace(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

// workspaceArg parses a command of one workspace and answers it.
func (c *cliCall) workspaceArg(fs *flag.FlagSet, stateDir *string, args []string) (*workspacev1.WorkspaceRef, []string, bool) {
	positionals, ok := c.parse(fs, args)
	if !ok {
		return nil, nil, false
	}
	if len(positionals) == 0 {
		c.usageFailed("the command names its workspace")
		return nil, nil, false
	}
	if !c.dial(*stateDir) {
		return nil, nil, false
	}
	ref, ok := c.named(positionals[0])
	return ref, positionals[1:], ok
}

func workspacePrompt(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	if len(rest) != 1 || strings.TrimSpace(rest[0]) == "" {
		return c.usageFailed("the command names the prompt's text, once")
	}
	key, err := c.deps.mintKey("agent-prompt")
	if err != nil {
		c.failed("%v", err)
		return exitFailure
	}
	resp, err := c.client.SubmitPrompt(c.ctx, connect.NewRequest(&agentreplv1.SubmitPromptRequest{Workspace: ref, Said: feed.SaidText(rest[0]),
		IdempotencyKey: key, Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT}))
	return answer(c, resp, err)
}

func workspaceSet(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	hidden := fs.Bool("hidden", false, "hide the workspace")
	shown := fs.Bool("shown", false, "show the workspace")
	priority := fs.String("priority", "", "p05, p1, p2, p3, or none")
	parent := fs.String("parent", "", "retarget the workspace into this owner")
	topLevel := fs.Bool("top-level", false, "retarget the workspace onto the primary branch")
	oneShot := fs.Bool("one-shot", false, "mark the workspace a one-shot")
	notOneShot := fs.Bool("not-one-shot", false, "unmark the workspace a one-shot")
	await := fs.String("await", "", "mark the workspace awaiting this workspace")
	notAwaiting := fs.Bool("not-awaiting", false, "clear what the workspace awaits")
	asUser := fs.Bool("as-user", false, "the user asks, not the calling workspace")
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	if len(rest) != 0 || (*hidden && *shown) || (*parent != "" && *topLevel) || (*oneShot && *notOneShot) || (*await != "" && *notAwaiting) {
		return c.usageFailed("each setting is set one way, and nothing else is positional")
	}
	var calls []func() int
	switch {
	case *hidden:
		calls = append(calls, func() int {
			resp, err := c.client.SetWorkspaceHidden(c.ctx, connect.NewRequest(&agentreplv1.SetWorkspaceHiddenRequest{Workspace: ref,
				Visibility: &agentreplv1.SetWorkspaceHiddenRequest_Hide{Hide: &agentreplv1.SetWorkspaceHiddenHide{}}}))
			return answer(c, resp, err)
		})
	case *shown:
		calls = append(calls, func() int {
			resp, err := c.client.SetWorkspaceHidden(c.ctx, connect.NewRequest(&agentreplv1.SetWorkspaceHiddenRequest{Workspace: ref,
				Visibility: &agentreplv1.SetWorkspaceHiddenRequest_Show{Show: &agentreplv1.SetWorkspaceHiddenShow{}}}))
			return answer(c, resp, err)
		})
	}
	if *priority != "" {
		level, err := priorityOf(*priority)
		if err != nil {
			return c.usageFailed("%v", err)
		}
		calls = append(calls, func() int {
			resp, err := c.client.SetWorkspacePriority(c.ctx, connect.NewRequest(&agentreplv1.SetWorkspacePriorityRequest{Workspace: ref, Priority: level}))
			return answer(c, resp, err)
		})
	}
	switch {
	case *parent != "":
		owner, ok := c.named(*parent)
		if !ok {
			return exitFailure
		}
		calls = append(calls, func() int {
			resp, err := c.client.SetWorkspaceParent(c.ctx, connect.NewRequest(&agentreplv1.SetWorkspaceParentRequest{Workspace: ref,
				Parent: &agentreplv1.SetWorkspaceParentRequest_ChildOf{ChildOf: &agentreplv1.SetWorkspaceParentChild{Parent: owner}}}))
			return answer(c, resp, err)
		})
	case *topLevel:
		calls = append(calls, func() int {
			resp, err := c.client.SetWorkspaceParent(c.ctx, connect.NewRequest(&agentreplv1.SetWorkspaceParentRequest{Workspace: ref,
				Parent: &agentreplv1.SetWorkspaceParentRequest_TopLevel{TopLevel: &agentreplv1.SetWorkspaceParentTopLevel{}}}))
			return answer(c, resp, err)
		})
	}
	if *oneShot || *notOneShot || *await != "" || *notAwaiting {
		requester, ok := c.requester(*asUser)
		if !ok {
			return exitFailure
		}
		if *oneShot || *notOneShot {
			calls = append(calls, func() int {
				resp, err := c.client.SetWorkspaceOneShot(c.ctx, connect.NewRequest(&agentreplv1.SetWorkspaceOneShotRequest{Workspace: ref, Requester: requester, OneShot: *oneShot}))
				return answer(c, resp, err)
			})
		}
		switch {
		case *await != "":
			awaited, ok := c.named(*await)
			if !ok {
				return exitFailure
			}
			calls = append(calls, func() int {
				resp, err := c.client.SetWorkspaceCorrespondence(c.ctx, connect.NewRequest(&agentreplv1.SetWorkspaceCorrespondenceRequest{Workspace: ref, Requester: requester,
					Mark: &agentreplv1.SetWorkspaceCorrespondenceRequest_Awaiting{Awaiting: &agentreplv1.SetWorkspaceCorrespondenceAwaiting{Awaited: awaited}}}))
				return answer(c, resp, err)
			})
		case *notAwaiting:
			calls = append(calls, func() int {
				resp, err := c.client.SetWorkspaceCorrespondence(c.ctx, connect.NewRequest(&agentreplv1.SetWorkspaceCorrespondenceRequest{Workspace: ref, Requester: requester,
					Mark: &agentreplv1.SetWorkspaceCorrespondenceRequest_Clear{Clear: &agentreplv1.SetWorkspaceCorrespondenceClear{}}}))
				return answer(c, resp, err)
			})
		}
	}
	if len(calls) == 0 {
		return c.usageFailed("the command names at least one setting")
	}
	for _, call := range calls {
		if code := call(); code != exitSuccess {
			return code
		}
	}
	return exitSuccess
}

// priorityOf reads a -priority flag: a level, or none to clear it.
func priorityOf(text string) (*agentreplv1.WorkspacePriority, error) {
	switch text {
	case "p05":
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P05{P05: &agentreplv1.WorkspacePriorityP05{}}}, nil
	case "p1":
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P1{P1: &agentreplv1.WorkspacePriorityP1{}}}, nil
	case "p2":
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P2{P2: &agentreplv1.WorkspacePriorityP2{}}}, nil
	case "p3":
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P3{P3: &agentreplv1.WorkspacePriorityP3{}}}, nil
	case "none":
		return nil, nil
	}
	return nil, fmt.Errorf("-priority is p05, p1, p2, p3 or none, not %q", text)
}

func workspaceMerge(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	place := fs.String("placement", "", "back or front")
	upstream := fs.Bool("merged-upstream", false, "the branch already merged upstream through its pull request")
	part := fs.String("version-part", "", "minor or patch, for a merge onto agent-repl's master")
	line := fs.String("changelog-line", "", "the version tag's changelog line")
	asUser := fs.Bool("as-user", false, "the user asks, not the calling workspace")
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	if len(rest) != 0 {
		return c.usageFailed("nothing but the workspace is positional")
	}
	where, err := placement(*place)
	if err != nil {
		return c.usageFailed("%v", err)
	}
	version, err := versionIncrement(*part, *line)
	if err != nil {
		return c.usageFailed("%v", err)
	}
	requester, ok := c.requester(*asUser)
	if !ok {
		return exitFailure
	}
	landing := &agentreplv1.MergeWorkspaceOneShot{Landing: &agentreplv1.MergeWorkspaceOneShot_OwnBranch{OwnBranch: &agentreplv1.MergeWorkspaceOneShotOwnBranch{}}}
	if *upstream {
		landing.Landing = &agentreplv1.MergeWorkspaceOneShot_MergedUpstream{MergedUpstream: &agentreplv1.MergeWorkspaceOneShotMergedUpstream{}}
	}
	resp, err := c.client.MergeWorkspace(c.ctx, connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ref, Requester: requester,
		Placement: where, VersionIncrement: version, Mode: &agentreplv1.MergeWorkspaceRequest_OneShot{OneShot: landing}}))
	return answer(c, resp, err)
}

func workspaceOpen(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	key, err := c.deps.mintKey("agent-open")
	if err != nil {
		c.failed("%v", err)
		return exitFailure
	}
	resp, err := c.client.OpenWorkspace(c.ctx, connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ref, OpId: key}))
	return answer(c, resp, err)
}

func workspaceClose(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	asUser := fs.Bool("as-user", false, "the user asks, not the calling workspace")
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	requester, ok := c.requester(*asUser)
	if !ok {
		return exitFailure
	}
	resp, err := c.client.CloseWorkspace(c.ctx, connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: ref, Requester: requester}))
	return answer(c, resp, err)
}

func workspaceKill(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	resp, err := c.client.KillWorkspace(c.ctx, connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: ref}))
	return answer(c, resp, err)
}

func workspaceCompact(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	instructions := fs.String("instructions", "", "what the summary keeps")
	asUser := fs.Bool("as-user", false, "the user asks, not the calling workspace")
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	requester, ok := c.requester(*asUser)
	if !ok {
		return exitFailure
	}
	req := &agentreplv1.CompactWorkspaceRequest{Workspace: ref, Requester: requester}
	if *instructions != "" {
		req.Instructions = &agentreplv1.CompactWorkspaceInstructions{Text: *instructions}
	}
	resp, err := c.client.CompactWorkspace(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

func workspaceStatus(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	resp, err := c.client.GetWorkspaceStatus(c.ctx, connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{Workspace: ref}))
	return answer(c, resp, err)
}

// listedWorkspace is one workspace as `workspace list` prints it.
type listedWorkspace struct {
	Name    string `json:"name"`
	ID      string `json:"id"`
	Dir     string `json:"dir"`
	Status  string `json:"status"`
	Section string `json:"section"`
	Calling bool   `json:"calling,omitempty"`
}

// workspaceList prints every workspace the roster carries, one JSON object a
// line: its name, id, directory, the roster status it draws, and the section
// it is drawn in (open, hidden, or merged). `status WS` reads one in full.
func workspaceList(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 0 {
		return c.usageFailed("nothing is positional")
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	roster, ok := c.readRoster()
	if !ok {
		return exitFailure
	}
	callingID := ""
	if cwd, err := c.deps.getwd(); err == nil {
		if row := callingRow(roster, canonicalDir(cwd)); row != nil {
			callingID = row.GetWorkspace().GetWorkspace().GetId()
		}
	}
	section := func(rows []*frontendv1.RosterRow, name string) []listedWorkspace {
		var out []listedWorkspace
		for _, row := range rows {
			ref := row.GetWorkspace().GetWorkspace()
			out = append(out, listedWorkspace{Name: row.GetName().GetText(), ID: ref.GetId(), Dir: ref.GetDir(),
				Status: rowStatus(row), Section: name, Calling: ref.GetId() == callingID})
		}
		return out
	}
	var open []*frontendv1.RosterRow
	for _, row := range rosterwalk.OpenRows(roster) {
		open = append(open, row.GetRow())
	}
	listed := section(open, "open")
	listed = append(listed, section(rosterwalk.HiddenRows(roster), "hidden")...)
	listed = append(listed, section(rosterwalk.MergedRows(roster), "merged")...)
	encoder := json.NewEncoder(c.out)
	for _, ws := range listed {
		if err := encoder.Encode(ws); err != nil {
			c.failed("print the list: %v", err)
			return exitFailure
		}
	}
	return exitSuccess
}

// rowStatus names a row's status arm by its proto field name, "unset" when it
// states none.
func rowStatus(row *frontendv1.RosterRow) string {
	arm, ok := protoarm.Name(row.ProtoReflect(), "status")
	if !ok {
		return "unset"
	}
	return string(arm)
}

// noticeEvents are the -events spellings.
var noticeEvents = map[string]agentreplv1.WorkspaceNoticeEvent{
	"turn_started": agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_TURN_STARTED,
	"turn_ended":   agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_TURN_ENDED,
	"needs_input":  agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_NEEDS_INPUT,
	"landed":       agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_LANDED,
	"closed":       agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_CLOSED,
}

func workspaceSubscribe(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	events := fs.String("events", "", "comma-separated: turn_started, turn_ended, needs_input, landed, closed")
	followed, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	var wanted []agentreplv1.WorkspaceNoticeEvent
	for _, name := range strings.Split(*events, ",") {
		event, ok := noticeEvents[strings.TrimSpace(name)]
		if !ok {
			return c.usageFailed("%q is no event", name)
		}
		wanted = append(wanted, event)
	}
	subscriber, ok := c.calling()
	if !ok {
		return exitFailure
	}
	resp, err := c.client.SubscribeWorkspace(c.ctx, connect.NewRequest(&agentreplv1.SubscribeWorkspaceRequest{Subscriber: subscriber, Followed: followed, Events: wanted}))
	return answer(c, resp, err)
}

func workspaceUnsubscribe(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	followed, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	subscriber, ok := c.calling()
	if !ok {
		return exitFailure
	}
	resp, err := c.client.UnsubscribeWorkspace(c.ctx, connect.NewRequest(&agentreplv1.UnsubscribeWorkspaceRequest{Subscriber: subscriber, Followed: followed}))
	return answer(c, resp, err)
}

// compactScopes are the -scope spellings.
var compactScopes = map[string]conversationv1.SessionCompactScope{
	"all":       conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL,
	"prompts":   conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_PROMPTS,
	"responses": conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_RESPONSES,
}

func workspaceGate(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	model := fs.String("model", "", "the summarizer, for a compaction")
	scope := fs.String("scope", "", "all, prompts or responses, for a compaction")
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	if len(rest) != 1 {
		return c.usageFailed("the command names the gate's answer")
	}
	req := &agentreplv1.AnswerColdGateRequest{Workspace: ref}
	compaction := func() (*agentreplv1.AnswerColdGateCompact, bool) {
		level, known := compactScopes[*scope]
		if *model == "" || !known {
			c.usageFailed("a compaction names its -model and its -scope")
			return nil, false
		}
		return &agentreplv1.AnswerColdGateCompact{Model: &conversationv1.AgentModel{Name: *model}, Scope: level}, true
	}
	switch rest[0] {
	case "pay":
		req.Choice = &agentreplv1.AnswerColdGateRequest_Pay{Pay: &agentreplv1.AnswerColdGatePay{}}
	case "clear":
		req.Choice = &agentreplv1.AnswerColdGateRequest_Clear{Clear: &agentreplv1.AnswerColdGateClear{}}
	case "compact":
		compact, ok := compaction()
		if !ok {
			return exitFailure
		}
		req.Choice = &agentreplv1.AnswerColdGateRequest_Compact{Compact: compact}
	case "compact-and-resume":
		compact, ok := compaction()
		if !ok {
			return exitFailure
		}
		req.Choice = &agentreplv1.AnswerColdGateRequest_CompactAndResume{CompactAndResume: compact}
	default:
		return c.usageFailed("the gate is answered pay, clear, compact or compact-and-resume, not %q", rest[0])
	}
	status, err := c.client.GetWorkspaceStatus(c.ctx, connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{Workspace: ref}))
	if err != nil {
		c.failed("read the workspace's status: %v", err)
		return exitFailure
	}
	gate := status.Msg.GetSuccess().GetStatus().GetStanding().GetOpen().GetActivity().GetColdGate().GetGate()
	if gate == nil {
		c.failed("no cold gate stands on the workspace: %v", status.Msg.GetResult())
		return exitRefused
	}
	req.Gate = gate
	resp, err := c.client.AnswerColdGate(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

// ---- merge-queue ---------------------------------------------------------------

// scopeFlag reads an optional -repository scope: unset is every repository.
func (c *cliCall) scopeFlag(repository string) (*workspacev1.RepositoryRef, bool) {
	if repository == "" {
		return nil, true
	}
	return c.repository(repository)
}

func queueStatus(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	repository := fs.String("repository", "", "here, or a main checkout; unset is every repository")
	if _, ok := c.parse(fs, args); !ok || !c.dial(*stateDir) {
		return exitFailure
	}
	scope, ok := c.scopeFlag(*repository)
	if !ok {
		return exitFailure
	}
	resp, err := c.client.GetMergeQueueStatus(c.ctx, connect.NewRequest(&agentreplv1.GetMergeQueueStatusRequest{Repository: scope}))
	return answer(c, resp, err)
}

func queueDequeue(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	resp, err := c.client.UpdateMergeQueue(c.ctx, connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: ref}}}))
	return answer(c, resp, err)
}

func queueMove(up bool) func(c *cliCall, args []string) int {
	return func(c *cliCall, args []string) int {
		fs, stateDir := c.flags()
		ref, rest, ok := c.workspaceArg(fs, stateDir, args)
		if !ok || len(rest) != 0 {
			return exitFailure
		}
		req := &agentreplv1.UpdateMergeQueueRequest{Action: &agentreplv1.UpdateMergeQueueRequest_MoveDown{MoveDown: &agentreplv1.UpdateMergeQueueMoveDown{Workspace: ref}}}
		if up {
			req.Action = &agentreplv1.UpdateMergeQueueRequest_MoveUp{MoveUp: &agentreplv1.UpdateMergeQueueMoveUp{Workspace: ref}}
		}
		resp, err := c.client.UpdateMergeQueue(c.ctx, connect.NewRequest(req))
		return answer(c, resp, err)
	}
}

func queuePause(pause bool) func(c *cliCall, args []string) int {
	return func(c *cliCall, args []string) int {
		fs, stateDir := c.flags()
		repository := fs.String("repository", "", "here, or a main checkout; unset is every repository")
		if _, ok := c.parse(fs, args); !ok || !c.dial(*stateDir) {
			return exitFailure
		}
		scope, ok := c.scopeFlag(*repository)
		if !ok {
			return exitFailure
		}
		req := &agentreplv1.UpdateMergeQueueRequest{Action: &agentreplv1.UpdateMergeQueueRequest_Resume{Resume: &agentreplv1.UpdateMergeQueueResume{Repository: scope}}}
		if pause {
			req.Action = &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{Repository: scope}}
		}
		resp, err := c.client.UpdateMergeQueue(c.ctx, connect.NewRequest(req))
		return answer(c, resp, err)
	}
}

func queueRacing(enable bool) func(c *cliCall, args []string) int {
	return func(c *cliCall, args []string) int {
		fs, stateDir := c.flags()
		if _, ok := c.parse(fs, args); !ok || !c.dial(*stateDir) {
			return exitFailure
		}
		req := &agentreplv1.UpdateMergeQueueRequest{Action: &agentreplv1.UpdateMergeQueueRequest_DisableRacing{DisableRacing: &agentreplv1.UpdateMergeQueueDisableRacing{}}}
		if enable {
			req.Action = &agentreplv1.UpdateMergeQueueRequest_EnableRacing{EnableRacing: &agentreplv1.UpdateMergeQueueEnableRacing{}}
		}
		resp, err := c.client.UpdateMergeQueue(c.ctx, connect.NewRequest(req))
		return answer(c, resp, err)
	}
}

// queueDetails are the -detail spellings.
var queueDetails = map[string]*agentreplv1.SubscribeMergeQueueDetail{
	"every_change": {Level: &agentreplv1.SubscribeMergeQueueDetail_EveryChange{EveryChange: &agentreplv1.SubscribeMergeQueueEveryChange{}}},
	"advances":     {Level: &agentreplv1.SubscribeMergeQueueDetail_Advances{Advances: &agentreplv1.SubscribeMergeQueueAdvances{}}},
	"depletes":     {Level: &agentreplv1.SubscribeMergeQueueDetail_Depletes{Depletes: &agentreplv1.SubscribeMergeQueueDepletes{}}},
}

func queueSubscribe(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	detail := fs.String("detail", "", "every_change, advances or depletes")
	repository := fs.String("repository", "", "here, or a main checkout; unset is every repository")
	if _, ok := c.parse(fs, args); !ok {
		return exitFailure
	}
	level, known := queueDetails[*detail]
	if !known {
		return c.usageFailed("-detail is every_change, advances or depletes, not %q", *detail)
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	scope, ok := c.scopeFlag(*repository)
	if !ok {
		return exitFailure
	}
	subscriber, ok := c.calling()
	if !ok {
		return exitFailure
	}
	resp, err := c.client.SubscribeMergeQueue(c.ctx, connect.NewRequest(&agentreplv1.SubscribeMergeQueueRequest{Subscriber: subscriber, Repository: scope, Detail: level}))
	return answer(c, resp, err)
}

func queueUnsubscribe(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	repository := fs.String("repository", "", "here, or a main checkout; unset is every repository")
	if _, ok := c.parse(fs, args); !ok || !c.dial(*stateDir) {
		return exitFailure
	}
	scope, ok := c.scopeFlag(*repository)
	if !ok {
		return exitFailure
	}
	subscriber, ok := c.calling()
	if !ok {
		return exitFailure
	}
	resp, err := c.client.UnsubscribeMergeQueue(c.ctx, connect.NewRequest(&agentreplv1.UnsubscribeMergeQueueRequest{Subscriber: subscriber, Repository: scope}))
	return answer(c, resp, err)
}

// ---- editor --------------------------------------------------------------------

func editorEval(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	workspaceName := fs.String("workspace", here, "the workspace whose buffer is current")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 1 || strings.TrimSpace(positionals[0]) == "" {
		return c.usageFailed("the command names the elisp to evaluate, once")
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	ref, ok := c.named(*workspaceName)
	if !ok {
		return exitFailure
	}
	resp, err := c.client.EvaluateInEditor(c.ctx, connect.NewRequest(&agentreplv1.EvaluateInEditorRequest{Workspace: ref, Text: positionals[0]}))
	return answer(c, resp, err)
}

// ---- runtime -------------------------------------------------------------------

func runtimeRollouts(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 1 {
		return c.usageFailed("the command names hold or release")
	}
	req := &agentreplv1.UpdateRolloutsRequest{}
	switch positionals[0] {
	case "hold":
		req.Action = &agentreplv1.UpdateRolloutsRequest_Hold{Hold: &agentreplv1.UpdateRolloutsHold{}}
	case "release":
		req.Action = &agentreplv1.UpdateRolloutsRequest_Release{Release: &agentreplv1.UpdateRolloutsRelease{}}
	default:
		return c.usageFailed("the rollouts are held or released, not %q", positionals[0])
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	resp, err := c.client.UpdateRollouts(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

func runtimeHealth(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	if _, ok := c.parse(fs, args); !ok || !c.dial(*stateDir) {
		return exitFailure
	}
	resp, err := c.client.DaemonHealth(c.ctx, connect.NewRequest(&agentreplv1.DaemonHealthRequest{}))
	if err != nil {
		c.failed("the daemon failed the call: %v", err)
		return exitFailure
	}
	text, err := protojson.MarshalOptions{Multiline: true}.Marshal(resp.Msg)
	if err != nil {
		c.failed("render the answer: %v", err)
		return exitFailure
	}
	fmt.Fprintln(c.out, string(text))
	return exitSuccess
}
