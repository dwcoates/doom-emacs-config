package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strconv"
	"strings"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/envc"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/prwatch"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/stateroot"
	"agent-repld/internal/workspace"
	"agent-repld/internal/workspacesubs"
	"agent-repld/internal/wsm"
)

// THE AGENT API'S COMMANDS: six nouns over the serving daemon's rpcs.
//
//	agent-repld workspace    create | async-merge | prompt | set | finished | open | close | kill
//	                         compact | status | list | subscribe | unsubscribe | gate
//	                         subsections | place
//	agent-repld merge-queue  status | dequeue | move-up | move-down | pause | resume
//	                         subscribe | unsubscribe
//	agent-repld repository   register
//	agent-repld editor       eval
//	agent-repld runtime      deploy-hold | roll-out-landing | health | prune-storage
//	                         reap-worktrees
//	agent-repld test         run (testcli.go)
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
	repositoryNoun = "repository"
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
	// client is the daemon's client at an address, with OPTIONS applied (a dry
	// run's interceptor).
	client func(address string, options ...connect.ClientOption) agentreplv1connect.AgentReplClient
	// roster reads the daemon's roster once.
	roster func(ctx context.Context, client agentreplv1connect.AgentReplClient) (*frontendv1.WorkspaceRoster, error)
	getwd  func() (string, error)
	// mintKey mints an idempotency key.
	mintKey func(prefix string) (string, error)
	// now is the current instant, which a relative time flag counts back from.
	now func() time.Time
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
		now:     time.Now,
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
		"create":                 {usage: "create -repository here|DIR -relationship child|peer [-parent WS] [-lands-into WS|main] [-origin-master] -context fresh|fork [-fork WS] [-one-shot] [-prompt TEXT] [-name NAME] [-model M] [-priority p05|p1|p2|p3] [-pre-deploy-prompt TEXT] [-pre-deploy-script PATH:SECONDS] [-post-deploy-prompt TEXT] [-post-deploy-script PATH:SECONDS] [-slack-thread URL] [-originator emoji-ack|user-or-workspace] [-as-user]", run: workspaceCreate},
		"async-merge":            {usage: "async-merge -name NAME -branch BRANCH (-maker WS | -maker-subagent ID) -context-fork WS -target primary|maker -placement back|front|behind=MERGE [-instruction TEXT] [-close-after WS] [-cleanup-worktree] [-compact-after] [-pre-deploy-prompt TEXT] [-pre-deploy-script PATH:SECONDS] [-post-deploy-prompt TEXT] [-post-deploy-script PATH:SECONDS] [-version-part minor|patch -changelog-line TEXT]", run: workspaceAsyncMerge},
		"prompt":                 {usage: "prompt [-as-user] WS TEXT", run: workspacePrompt},
		"gather":                 {usage: "gather -workspaces A,B,C -prompt TEXT [-as-user]", run: workspaceGather},
		"gather-status":          {usage: "gather-status GATHER", run: workspaceGatherStatus},
		"set":                    {usage: "set WS [-hidden|-shown] [-priority p05|p1|p2|p3|none] [-parent WS|-top-level] [-lands-into WS|main] [-one-shot|-not-one-shot] [-await WS|-not-awaiting] [-name NAME] [-remote-control on|off] [-queue-bypass on|off [-context full|compacted|existing|existing-plus-compacted]] [-as-user]", run: workspaceSet},
		"summary":                {usage: "summary WS -text SUMMARY", run: workspaceSummary},
		"link":                   {usage: "link WS (-url URL -label TEXT -owned [-announces URL]|-reference | -remove URL | -show URL [-show URL]... | -clear)", run: workspaceLink},
		"finished":               {usage: "finished WS -placement back|front|behind=MERGE [-version-part minor|patch -changelog-line TEXT] [-as-user]", run: workspaceFinished},
		"open":                   {usage: "open WS", run: workspaceOpen},
		"close":                  {usage: "close WS [-as-user]", run: workspaceClose},
		"kill":                   {usage: "kill WS", run: workspaceKill},
		"select":                 {usage: "select WS", run: workspaceSelect},
		"restart":                {usage: "restart WS [-when-idle]", run: workspaceRestart},
		"park":                   {usage: "park WS|-all", run: workspacePark},
		"interrupt":              {usage: "interrupt WS [-all-agents] [-confirm-agents]", run: workspaceInterrupt},
		"compact":                {usage: "compact WS [-instructions TEXT] [-as-user]", run: workspaceCompact},
		"status":                 {usage: "status WS", run: workspaceStatus},
		"list":                   {usage: "list", run: workspaceList},
		"find":                   {usage: "find [-repository here|DIR] [-since WHEN] [-until WHEN] [-standing open|closed|landed] [-text TEXT] [-created-by WS] [-limit N|-all]", run: workspaceFind},
		"subscribe":              {usage: "subscribe WS -events " + allNoticeEvents, run: workspaceSubscribe},
		"unsubscribe":            {usage: "unsubscribe WS", run: workspaceUnsubscribe},
		"pr-subscribe":           {usage: "pr-subscribe WS -pr URL|#N|OWNER/REPO#N [-events " + allPullRequestEvents + "]", run: workspacePullRequestSubscribe},
		"pr-unsubscribe":         {usage: "pr-unsubscribe WS -pr URL|OWNER/REPO#N", run: workspacePullRequestUnsubscribe},
		"pr-subscriptions":       {usage: "pr-subscriptions WS", run: workspacePullRequestSubscriptions},
		"pr-comment":             {usage: "pr-comment WS -pr URL|OWNER/REPO#N (-body TEXT | -body-file PATH) [-in-reply-to COMMENT_ID]", run: workspacePullRequestComment},
		"gh-queue-subscribe":     {usage: "gh-queue-subscribe WS [-repository OWNER/REPO] [-branch BRANCH]", run: workspaceGithubQueueSubscribe},
		"gh-queue-unsubscribe":   {usage: "gh-queue-unsubscribe WS -queue OWNER/REPO@BRANCH", run: workspaceGithubQueueUnsubscribe},
		"gh-queue-subscriptions": {usage: "gh-queue-subscriptions WS", run: workspaceGithubQueueSubscriptions},
		"notify":                 {usage: "notify WS TEXT", run: workspaceNotify},
		"gate":                   {usage: "gate WS pay|clear|compact|compact-and-resume [-model M -scope all|prompts|responses]", run: workspaceGate},
		"subsections":            {usage: "subsections WS", run: workspaceSubsections},
		"place":                  {usage: "place WS... -subsection NAME|-new-subsection NAME", run: workspacePlace},
	},
	mergeQueueNoun: {
		"status":      {usage: "status [-repository here|DIR]", run: queueStatus},
		"dequeue":     {usage: "dequeue MERGE|here", run: queueDequeue},
		"move-up":     {usage: "move-up MERGE|here", run: queueMove(true)},
		"move-down":   {usage: "move-down MERGE|here", run: queueMove(false)},
		"pause":       {usage: "pause [-repository here|DIR] [-hard]", run: queuePause(true)},
		"resume":      {usage: "resume [-repository here|DIR]", run: queuePause(false)},
		"subscribe":   {usage: "subscribe [-repository here|DIR]", run: queueSubscribe},
		"unsubscribe": {usage: "unsubscribe [-repository here|DIR]", run: queueUnsubscribe},
	},
	repositoryNoun: {
		"register": {usage: "register here|PATH", run: repositoryRegister},
	},
	editorNoun: {
		"eval": {usage: "eval [-workspace WS] TEXT", run: editorEval},
	},
	testNoun: {
		"run":        {usage: "run [-workspace WS] [-worktree DIR] [-go DIR[=TestA,TestB/sub/path]]... [-vitest FILE[=FULL NAME]]... [-ert FILE[=test-a,test-b]]... [-harness SCRIPT[=group1,group2]]... [-suite NAME]... [-every-suite] [-coverage] [-record] [-runs N [-stop-at-first-failure]] [-cpu-load N -as-user]", run: testRun},
		"redundancy": {usage: "redundancy go|vitest|report [-workspace WS] FLAGS (testrun redundancy's own flags)", run: testRedundancy},
	},
	runtimeNoun: {
		"deploy-hold":      {usage: "deploy-hold hold|release -repository here|DIR", run: runtimeDeployHold},
		"roll-out-landing": {usage: "roll-out-landing [-deploy LEDGER] [-daemon ADDRESS]", run: runtimeRollOutLanding},
		"health":           {usage: "health", run: runtimeHealth},
		"prune-storage":    {usage: "prune-storage [-preview]", run: runtimePruneStorage},
		"reap-worktrees":   {usage: "reap-worktrees [-preview] [-force]", run: runtimeReapWorktrees},
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
	// dryRun is the command's -dry-run flag, set by flags.
	dryRun *bool
}

// flags is the command's flag set, carrying the shared -state-dir and
// -dry-run.
func (c *cliCall) flags() (*flag.FlagSet, *string) {
	fs := flag.NewFlagSet(c.name, flag.ContinueOnError)
	fs.SetOutput(c.errOut)
	fs.Usage = func() {
		fmt.Fprintf(c.errOut, "usage: agent-repld %s %s [-dry-run]\n", strings.Fields(c.name)[0], c.usage)
	}
	c.dryRun = fs.Bool("dry-run", false, "print every request that would change something instead of sending it")
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
	c.dialAddress(address)
	return true
}

// dialAddress makes the command's client for the daemon at ADDRESS.
func (c *cliCall) dialAddress(address string) {
	var options []connect.ClientOption
	if c.dryRun != nil && *c.dryRun {
		options = append(options, connect.WithInterceptors(dryRunInterceptor(c.out)))
	}
	c.client = c.deps.client(address, options...)
}

// noted tells the caller what the command did on its behalf that the answer
// alone would not show.
func (c *cliCall) noted(format string, a ...any) {
	fmt.Fprintf(c.errOut, "agent-repld %s: %s\n", c.name, fmt.Sprintf(format, a...))
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
	ref, cwd, ok := c.callingRef()
	if ok && ref == nil {
		return nil, c.failed("%s is inside no workspace the daemon serves; run this from a workspace, or pass -as-user when the user asks", cwd)
	}
	return ref, ok
}

// callerIfAny answers the calling workspace's ref as calling does, and nil
// when the working directory is inside no workspace the daemon serves: a
// read that marks the caller's own record (FindWorkspacesRequest.caller)
// answers for a caller outside every worktree too.
func (c *cliCall) callerIfAny() (*workspacev1.WorkspaceRef, bool) {
	ref, _, ok := c.callingRef()
	return ref, ok
}

// callingRef answers the ref of the workspace whose worktree contains the
// working directory, nil when none does, with that directory.
func (c *cliCall) callingRef() (*workspacev1.WorkspaceRef, string, bool) {
	cwd, err := c.deps.getwd()
	if err != nil {
		return nil, "", c.failed("read the working directory: %v", err)
	}
	roster, ok := c.readRoster()
	if !ok {
		return nil, cwd, false
	}
	row := callingRow(roster, canonicalDir(cwd))
	if row == nil {
		return nil, cwd, true
	}
	return row.GetWorkspace().GetWorkspace(), cwd, true
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
	return c.repositoryOf(ref)
}

// repositoryOf answers the repository workspace REF sits in, off its status.
func (c *cliCall) repositoryOf(ref *workspacev1.WorkspaceRef) (*workspacev1.RepositoryRef, bool) {
	resp, err := c.client.GetWorkspaceStatus(c.ctx, connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{Workspace: ref}))
	if err != nil {
		return nil, c.failed("read the status of workspace %s: %v", ref.GetId(), err)
	}
	repository := resp.Msg.GetSuccess().GetStatus().GetRepository()
	if repository.GetId() == "" {
		return nil, c.failed("the daemon names no repository for workspace %s: %v", ref.GetId(), resp.Msg.GetResult())
	}
	return repository, true
}

// arrangedRepositoryOf answers the sidebar arrangement of the repository
// workspace REF sits in: its unsectioned families and its subsections, each
// with the families in it, in drawn order.
func (c *cliCall) arrangedRepositoryOf(ref *workspacev1.WorkspaceRef) (*agentreplv1.ArrangedRepository, bool) {
	repository, ok := c.repositoryOf(ref)
	if !ok {
		return nil, false
	}
	resp, err := c.client.GetSidebarArrangement(c.ctx, connect.NewRequest(&agentreplv1.GetSidebarArrangementRequest{}))
	if err != nil {
		return nil, c.failed("read the sidebar arrangement: %v", err)
	}
	for _, arranged := range resp.Msg.GetRepositories() {
		if arranged.GetRepository().GetId() == repository.GetId() {
			return arranged, true
		}
	}
	return nil, c.failed("the sidebar arrangement holds no repository %s (%s)", repository.GetId(), repository.GetDir())
}

// answer prints an rpc's answer and answers the command's exit.
func answer[Res any](c *cliCall, resp *connect.Response[Res], err error) int {
	return answerDescribed(c, resp, err, func(*Res) (string, error) { return "the daemon refused", nil })
}

// answerDescribed is answer whose refusal line on stderr is WHY's sentence
// for the refused answer, so the reason reaches a caller that reads only
// stderr. A refusal WHY has no sentence for is a bug: it is reported as one,
// and the command still exits refused.
func answerDescribed[Res any](c *cliCall, resp *connect.Response[Res], err error, why func(*Res) (string, error)) int {
	if errors.Is(err, errDryRun) {
		return exitSuccess
	}
	if err != nil {
		c.failed("the daemon failed the call: %v", err)
		return exitFailure
	}
	msg, ok := any(resp.Msg).(proto.Message)
	if !ok {
		c.failed("the answer is a %T, no message", resp.Msg)
		return exitFailure
	}
	if !c.print(msg) {
		return exitFailure
	}
	if refusedAnswer(msg) {
		line, err := why(resp.Msg)
		if err != nil {
			c.failed("%v", err)
			return exitRefused
		}
		c.failed("%s", line)
		return exitRefused
	}
	return exitSuccess
}

// print prints MSG as multiline protojson, every command's output.
func (c *cliCall) print(msg proto.Message) bool {
	text, err := protojson.MarshalOptions{Multiline: true}.Marshal(msg)
	if err != nil {
		return c.failed("render the answer: %v", err)
	}
	fmt.Fprintln(c.out, string(text))
	return true
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

// hookList is one stage's deploy hooks as its flags name them, in the order
// given: each -<stage>-prompt and -<stage>-script flag appends one hook.
type hookList struct {
	hooks []*repoconfigv1.Hook
}

// promptFlag appends a prompt hook to its list.
type promptFlag struct{ list *hookList }

func (f promptFlag) String() string { return "" }
func (f promptFlag) Set(text string) error {
	if strings.TrimSpace(text) == "" {
		return errors.New("a prompt hook is never blank")
	}
	f.list.hooks = append(f.list.hooks, lifecycleconfig.PromptHook(text))
	return nil
}

// scriptFlag appends a script hook, PATH:SECONDS, to its list.
type scriptFlag struct{ list *hookList }

func (f scriptFlag) String() string { return "" }
func (f scriptFlag) Set(value string) error {
	path, seconds, found := strings.Cut(value, ":")
	if !found || strings.TrimSpace(path) == "" {
		return fmt.Errorf("a script hook is PATH:SECONDS, not %q", value)
	}
	timeout, err := strconv.ParseUint(seconds, 10, 32)
	if err != nil || timeout < 1 {
		return fmt.Errorf("a script hook's timeout is a whole number of seconds, at least 1, not %q", seconds)
	}
	f.list.hooks = append(f.list.hooks, lifecycleconfig.ScriptHook(path, uint32(timeout)))
	return nil
}

// hookFlags registers the four deploy-hook flags on FS and answers the two
// stages' lists they fill.
func hookFlags(fs *flag.FlagSet) (pre, post *hookList) {
	pre, post = &hookList{}, &hookList{}
	fs.Var(promptFlag{pre}, "pre-deploy-prompt", "a prompt hook run before the local merge, in place of the repository's; repeatable")
	fs.Var(scriptFlag{pre}, "pre-deploy-script", "a script hook PATH:SECONDS run before the local merge, in place of the repository's; repeatable")
	fs.Var(promptFlag{post}, "post-deploy-prompt", "a prompt hook run after the deploy stage, in place of the repository's; repeatable")
	fs.Var(scriptFlag{post}, "post-deploy-script", "a script hook PATH:SECONDS run after the deploy stage, in place of the repository's; repeatable")
	return pre, post
}

// deployHooks are the hooks a create or an async merge names, nil when it
// names none.
func deployHooks(pre, post *hookList) *agentreplv1.CreateWorkspaceDeployHooks {
	if len(pre.hooks) == 0 && len(post.hooks) == 0 {
		return nil
	}
	return &agentreplv1.CreateWorkspaceDeployHooks{PreDeploy: pre.hooks, PostDeploy: post.hooks}
}

// placement reads a -placement flag: back, front, or behind=MERGE (directly
// behind the merge the queue lists under that name). Nothing is defaulted.
func placement(text string) (*agentreplv1.MergeQueuePlacement, error) {
	if merge, behind := strings.CutPrefix(text, "behind="); behind {
		if strings.TrimSpace(merge) == "" {
			return nil, fmt.Errorf("-placement behind= names no merge")
		}
		return &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Behind{Behind: &agentreplv1.MergeQueuePlacementBehind{Merge: merge}}}, nil
	}
	switch text {
	case "back":
		return &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Back{Back: &agentreplv1.MergeQueuePlacementBack{}}}, nil
	case "front":
		return &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Front{Front: &agentreplv1.MergeQueuePlacementFront{}}}, nil
	}
	return nil, fmt.Errorf("-placement is back, front or behind=MERGE, not %q", text)
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
	relationship := fs.String("relationship", "", "child of -parent, or peer")
	parent := fs.String("parent", "", "the parent that owns and coordinates a child")
	landsInto := fs.String("lands-into", "", "main, or the workspace whose branch the work is cut from and lands into; unset is the relationship's default")
	originMaster := fs.Bool("origin-master", false, "cut a workspace landing on main from origin's primary branch, fetched now")
	contextOrigin := fs.String("context", "", "fresh, or fork of -fork")
	fork := fs.String("fork", "", "the workspace whose conversation is forked")
	oneShot := fs.Bool("one-shot", false, "a one-shot, which lands itself when done")
	prompt := fs.String("prompt", "", "the first prompt")
	name := fs.String("name", "", "the workspace's name; unset derives it from the prompt")
	model := fs.String("model", "", "the session's model; unset is the daemon's")
	priority := fs.String("priority", "", "p05, p1, p2 or p3; unset is unprioritized")
	pre, post := hookFlags(fs)
	slackThread := fs.String("slack-thread", "", "the link of the Slack message the workspace acts on; its hooks read it as AGENT_REPL_SLACK_THREAD and {{slack_thread}}")
	originator := fs.String("originator", originatorUserOrWorkspace, "who creates the workspace: emoji-ack (the Slack emoji-ack system, with -slack-thread) or user-or-workspace")
	hidden := fs.Bool("hidden", false, "born hidden: the workspace is never drawn in the sidebar until it is shown")
	subsection := fs.String("subsection", "", "file the workspace into this existing subsection of its repository, by name")
	newSubsection := fs.String("new-subsection", "", "file the workspace into a new subsection of this name, with -gather's workspaces")
	gather := fs.String("gather", "", "comma-separated names of open, unsectioned, top-level workspaces gathered into -new-subsection")
	asUser := fs.Bool("as-user", false, "the user asks, not the calling workspace")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	placed, err := createPlacement(*subsection, *newSubsection, *gather)
	if err != nil {
		return c.usageFailed("%v", err)
	}
	if len(positionals) != 0 || *repository == "" || *relationship == "" || *contextOrigin == "" {
		return c.usageFailed("-repository, -relationship and -context are required, and nothing is positional")
	}
	if *originMaster && *landsInto != landsIntoMain {
		return c.usageFailed("-origin-master cuts a workspace landing on main, so it goes with -lands-into main")
	}
	stated, known := originatorFlag(*originator)
	if !known {
		return c.usageFailed("-originator is %s or %s, not %q", originatorEmojiAck, originatorUserOrWorkspace, *originator)
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	req := &agentreplv1.CreateWorkspaceRequest{Originator: stated}
	var good bool
	if req.Repository, good = c.repository(*repository); !good {
		return exitFailure
	}
	if req.Requester, good = c.requester(*asUser); !good {
		return exitFailure
	}
	kind := &agentreplv1.WorkspaceRelationship{}
	switch *relationship {
	case "peer":
		if *parent != "" {
			return c.usageFailed("a peer has no -parent")
		}
		kind.Kind = &agentreplv1.WorkspaceRelationship_Peer{Peer: &agentreplv1.WorkspaceRelationshipPeer{}}
	case "child":
		parentRef, ok := c.named(*parent)
		if *parent == "" || !ok {
			return c.usageFailed("a child names its -parent")
		}
		kind.Kind = &agentreplv1.WorkspaceRelationship_Child{Child: &agentreplv1.WorkspaceRelationshipChild{Parent: parentRef}}
	default:
		return c.usageFailed("-relationship is child or peer, not %q", *relationship)
	}
	landing, ok := c.landingTarget(*landsInto, *originMaster)
	if !ok {
		return exitFailure
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
		form := &agentreplv1.CreateWorkspaceOneShot{Prompt: feed.SaidText(*prompt), Relationship: kind, LandingTarget: landing}
		if *name != "" {
			form.Name = name
		}
		req.Form = &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: form}
	} else {
		standard := &agentreplv1.CreateWorkspaceStandard{InitialPrompt: optionalSaid(*prompt), Relationship: kind, LandingTarget: landing, DeployHooks: deployHooks(pre, post)}
		if *name != "" {
			standard.Name = name
		}
		req.Form = &agentreplv1.CreateWorkspaceRequest_Standard{Standard: standard}
	}
	if *model != "" {
		req.Model = model
	}
	if *priority != "" {
		level, err := priorityOf(*priority)
		if err != nil || level == nil {
			return c.usageFailed("-priority is p05, p1, p2 or p3, not %q", *priority)
		}
		req.Priority = level
	}
	if *slackThread != "" {
		req.SlackOrigin = &agentreplv1.CreateWorkspaceSlackOrigin{ThreadUrl: *slackThread}
	}
	if *hidden {
		req.Hidden = hidden
	}
	req.Placement = placed
	// A DAEMON HANDING OVER names its successor, and the client sends the
	// same create there once (newRedirectingClient), as it does every call.
	resp, err := c.client.CreateWorkspace(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

// createPlacement reads -subsection, -new-subsection and -gather: nil when
// none is given, an existing subsection for -subsection, and a new one
// gathering -gather's comma-separated names for -new-subsection. The two
// subsection flags are alternatives, and -gather belongs to the new one.
func createPlacement(subsection, newSubsection, gather string) (*agentreplv1.CreateWorkspacePlacement, error) {
	switch {
	case subsection != "" && newSubsection != "":
		return nil, errors.New("-subsection and -new-subsection are alternatives")
	case gather != "" && newSubsection == "":
		return nil, errors.New("-gather goes with -new-subsection")
	case subsection != "":
		return &agentreplv1.CreateWorkspacePlacement{Target: &agentreplv1.CreateWorkspacePlacement_Subsection{Subsection: subsection}}, nil
	case newSubsection != "":
		fresh := &agentreplv1.CreateWorkspaceNewSubsection{Name: newSubsection}
		if gather != "" {
			for _, name := range strings.Split(gather, ",") {
				if name = strings.TrimSpace(name); name == "" {
					return nil, fmt.Errorf("-gather names workspaces separated by commas, not %q", gather)
				}
				fresh.Gather = append(fresh.Gather, name)
			}
		}
		return &agentreplv1.CreateWorkspacePlacement{Target: &agentreplv1.CreateWorkspacePlacement_NewSubsection{NewSubsection: fresh}}, nil
	}
	return nil, nil
}

// landsIntoMain is -lands-into's spelling of the primary branch.
const landsIntoMain = "main"

// landingTarget reads -lands-into: nil when unset (the relationship's
// default), the primary branch for `main` (cut from origin when FROMORIGIN),
// and otherwise the named workspace. A name that resolves to nothing has
// already been reported.
func (c *cliCall) landingTarget(text string, fromOrigin bool) (*agentreplv1.WorkspaceLandingTarget, bool) {
	switch text {
	case "":
		return nil, true
	case landsIntoMain:
		primary := &agentreplv1.WorkspaceLandingTargetPrimary{}
		if fromOrigin {
			primary.FromOrigin = &agentreplv1.WorkspaceLandingTargetPrimaryFromOrigin{}
		}
		return &agentreplv1.WorkspaceLandingTarget{Target: &agentreplv1.WorkspaceLandingTarget_Primary{Primary: primary}}, true
	}
	into, ok := c.named(text)
	if !ok {
		return nil, false
	}
	return &agentreplv1.WorkspaceLandingTarget{Target: &agentreplv1.WorkspaceLandingTarget_Workspace{Workspace: &agentreplv1.WorkspaceLandingTargetWorkspace{Workspace: into}}}, true
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
	pre, post := hookFlags(fs)
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
		DeployHooks: deployHooks(pre, post), VersionIncrement: version}
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
	userOrWorkspace, _ := originatorFlag(originatorUserOrWorkspace)
	req := &agentreplv1.CreateWorkspaceRequest{Repository: repository, Requester: requester, Originator: userOrWorkspace,
		ContextOrigin: &agentreplv1.WorkspaceContextOrigin{Origin: &agentreplv1.WorkspaceContextOrigin_Fork{Fork: &agentreplv1.WorkspaceContextOriginFork{Workspace: forkRef}}},
		Form:          &agentreplv1.CreateWorkspaceRequest_AsyncMerge{AsyncMerge: async}}
	resp, err := c.client.CreateWorkspace(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

// workspaceArg parses a command of one workspace and answers it.
func (c *cliCall) workspaceArg(fs *flag.FlagSet, stateDir *string, args []string) (*workspacev1.WorkspaceRef, []string, bool) {
	_, ref, rest, ok := c.namedWorkspaceArg(fs, stateDir, args)
	return ref, rest, ok
}

// namedWorkspaceArg is workspaceArg also answering the workspace as the
// caller named it, for a command that speaks of it in the caller's words.
func (c *cliCall) namedWorkspaceArg(fs *flag.FlagSet, stateDir *string, args []string) (string, *workspacev1.WorkspaceRef, []string, bool) {
	positionals, ok := c.targetArgs(fs, stateDir, args, "workspace")
	if !ok {
		return "", nil, nil, false
	}
	ref, ok := c.named(positionals[0])
	return positionals[0], ref, positionals[1:], ok
}

// targetArgs parses ARGS for a command whose first positional names its
// target (WHAT, for the usage failure) and dials the daemon, answering the
// positionals, the target first.
func (c *cliCall) targetArgs(fs *flag.FlagSet, stateDir *string, args []string, what string) ([]string, bool) {
	positionals, ok := c.parse(fs, args)
	if !ok {
		return nil, false
	}
	if len(positionals) == 0 {
		c.usageFailed("the command names its %s", what)
		return nil, false
	}
	if !c.dial(*stateDir) {
		return nil, false
	}
	return positionals, true
}

func workspacePrompt(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	asUser := fs.Bool("as-user", false, "the user explicitly asked for this send: lifts the fan-out cap")
	name, ref, rest, ok := c.namedWorkspaceArg(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	if len(rest) != 1 || strings.TrimSpace(rest[0]) == "" {
		return c.usageFailed("the command names the prompt's text, once")
	}
	// THE SENDER IS THE CALLING WORKSPACE: the recipient's feed heads the
	// prompt with it.
	sender, ok := c.calling()
	if !ok {
		return exitFailure
	}
	key, err := c.deps.mintKey("agent-prompt")
	if err != nil {
		c.failed("%v", err)
		return exitFailure
	}
	resp, err := c.client.SubmitPrompt(c.ctx, connect.NewRequest(&agentreplv1.SubmitPromptRequest{Workspace: ref, Said: feed.SaidText(rest[0]),
		IdempotencyKey: key, Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, SentBy: sender, AsUser: *asUser}))
	return answerDescribed(c, resp, err, func(msg *agentreplv1.SubmitPromptResponse) (string, error) {
		return describePromptRefusal(name, msg.GetError())
	})
}

func workspaceSet(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	hidden := fs.Bool("hidden", false, "hide the workspace")
	shown := fs.Bool("shown", false, "show the workspace")
	priority := fs.String("priority", "", "p05, p1, p2, p3, or none")
	parent := fs.String("parent", "", "make the workspace a child of this parent")
	topLevel := fs.Bool("top-level", false, "make the workspace a top-level peer")
	landsInto := fs.String("lands-into", "", "main, or the workspace its merge lands into; unset with -parent or -top-level re-applies the default")
	oneShot := fs.Bool("one-shot", false, "mark the workspace a one-shot")
	notOneShot := fs.Bool("not-one-shot", false, "unmark the workspace a one-shot")
	await := fs.String("await", "", "mark the workspace awaiting this workspace")
	notAwaiting := fs.Bool("not-awaiting", false, "clear what the workspace awaits")
	name := fs.String("name", "", "rename the workspace: its name only, never its branch or directory")
	remoteControl := fs.String("remote-control", "", "on or off: whether the workspace's session is reachable from claude.ai")
	queueBypass := fs.String("queue-bypass", "", "on or off: whether the workspace holds its repository's queue bypass mode")
	bypassContext := fs.String("context", "", "with -queue-bypass on: full, compacted, existing or existing-plus-compacted, how the bypass session gets its context")
	asUser := fs.Bool("as-user", false, "the user asks, not the calling workspace")
	named, ref, rest, ok := c.namedWorkspaceArg(fs, stateDir, args)
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
	if *parent != "" || *topLevel || *landsInto != "" {
		retarget := &agentreplv1.SetWorkspaceParentRequest{Workspace: ref,
			Parent: &agentreplv1.SetWorkspaceParentRequest_Unchanged{Unchanged: &agentreplv1.SetWorkspaceParentUnchanged{}}}
		switch {
		case *parent != "":
			owner, ok := c.named(*parent)
			if !ok {
				return exitFailure
			}
			retarget.Parent = &agentreplv1.SetWorkspaceParentRequest_ChildOf{ChildOf: &agentreplv1.SetWorkspaceParentChild{Parent: owner}}
		case *topLevel:
			retarget.Parent = &agentreplv1.SetWorkspaceParentRequest_TopLevel{TopLevel: &agentreplv1.SetWorkspaceParentTopLevel{}}
		}
		if retarget.LandingTarget, ok = c.landingTarget(*landsInto, false); !ok {
			return exitFailure
		}
		calls = append(calls, func() int {
			resp, err := c.client.SetWorkspaceParent(c.ctx, connect.NewRequest(retarget))
			return answerDescribed(c, resp, err, func(msg *agentreplv1.SetWorkspaceParentResponse) (string, error) {
				return describeRetargetRefusal(named, msg.GetError())
			})
		})
	}
	if *name != "" {
		calls = append(calls, func() int {
			resp, err := c.client.SetWorkspaceName(c.ctx, connect.NewRequest(&agentreplv1.SetWorkspaceNameRequest{Workspace: ref, Name: *name}))
			return answer(c, resp, err)
		})
	}
	if *remoteControl != "" {
		req, err := remoteControlRequest(ref, *remoteControl)
		if err != nil {
			return c.usageFailed("%v", err)
		}
		calls = append(calls, func() int {
			resp, err := c.client.UpdateRemoteControlMode(c.ctx, connect.NewRequest(req))
			return answer(c, resp, err)
		})
	}
	if *queueBypass != "" || *bypassContext != "" {
		req, err := queueBypassRequest(ref, *queueBypass, *bypassContext)
		if err != nil {
			return c.usageFailed("%v", err)
		}
		calls = append(calls, func() int {
			resp, err := c.client.UpdateQueueBypassMode(c.ctx, connect.NewRequest(req))
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
				return answerDescribed(c, resp, err, func(msg *agentreplv1.SetWorkspaceOneShotResponse) (string, error) {
					return describeOneShotRefusal(named, msg.GetError())
				})
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

// The -originator spellings.
const (
	originatorEmojiAck        = "emoji-ack"
	originatorUserOrWorkspace = "user-or-workspace"
)

// originatorFlag reads an -originator spelling into the create's originator,
// false for a spelling that names none.
func originatorFlag(spelled string) (*agentreplv1.WorkspaceOriginator, bool) {
	switch spelled {
	case originatorEmojiAck:
		return &agentreplv1.WorkspaceOriginator{Originator: &agentreplv1.WorkspaceOriginator_EmojiAck{EmojiAck: &agentreplv1.WorkspaceOriginatorEmojiAck{}}}, true
	case originatorUserOrWorkspace:
		return &agentreplv1.WorkspaceOriginator{Originator: &agentreplv1.WorkspaceOriginator_UserOrWorkspace{UserOrWorkspace: &agentreplv1.WorkspaceOriginatorUserOrWorkspace{}}}, true
	}
	return nil, false
}

// workspaceSummary sets the one-sentence summary of a workspace's task that
// its topbar draws (agentrepl.v1.SetWorkspaceSummary).
func workspaceSummary(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	text := fs.String("text", "", "the summary: one imperative sentence of at most fifteen words")
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	if len(rest) != 0 {
		return c.usageFailed("nothing is positional")
	}
	if *text == "" {
		return c.usageFailed("-text names the summary")
	}
	resp, err := c.client.SetWorkspaceSummary(c.ctx, connect.NewRequest(&agentreplv1.SetWorkspaceSummaryRequest{Workspace: ref, Summary: *text}))
	return answer(c, resp, err)
}

// workspaceLink changes one of a workspace's links, the topbar summary's
// menu: store or relabel one (-url with -label, and -owned or -reference),
// remove one (-remove), name
// exactly the ones the menu shows (-show, repeated), or clear them (-clear).
// The answer carries every link and whether the menu is full.
func workspaceLink(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	url := fs.String("url", "", "the link to store or relabel")
	label := fs.String("label", "", "the link's label: two to eight words, one sentence")
	owned := fs.Bool("owned", false, "the workspace's work made what the link points at (a pull request it opened): the daemon follows an owned pull request")
	reference := fs.Bool("reference", false, "the link points at something apart from the workspace's work (the thread or ticket it came from)")
	announces := fs.String("announces", "", "with -owned: the url of a link the workspace holds that this one announces (a Slack post saying a pull request was created names that pull request)")
	remove := fs.String("remove", "", "the url of the link to remove")
	var show []string
	fs.Var(repeatedFlag{values: &show, noun: "url"}, "show", "a url the menu shows; repeat it for each, at most five")
	clear := fs.Bool("clear", false, "remove every link")
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	req, problem := linkRequest(ref, linkSet{url: *url, label: *label, owned: *owned, reference: *reference, announces: *announces}, *remove, show, *clear)
	if len(rest) != 0 || problem != "" {
		if problem == "" {
			problem = "nothing is positional"
		}
		return c.usageFailed("%s", problem)
	}
	resp, err := c.client.ChangeWorkspaceLinks(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

// linkSet is workspaceLink's set flags.
type linkSet struct {
	url, label, announces string
	owned, reference      bool
}

// given reports whether any set flag was named.
func (s linkSet) given() bool {
	return s.url != "" || s.label != "" || s.owned || s.reference || s.announces != ""
}

// linkRequest reads workspaceLink's flags into the request for REF, or names
// why they do not make one: exactly one change, and a set names its url, its
// label, and exactly one of -owned and -reference.
func linkRequest(ref *workspacev1.WorkspaceRef, set linkSet, remove string, show []string, clear bool) (*agentreplv1.ChangeWorkspaceLinksRequest, string) {
	changes := 0
	for _, given := range []bool{set.given(), remove != "", len(show) > 0, clear} {
		if given {
			changes++
		}
	}
	if changes != 1 {
		return nil, "name exactly one change: -url with -label, -remove, -show, or -clear"
	}
	req := &agentreplv1.ChangeWorkspaceLinksRequest{Workspace: ref}
	switch {
	case set.given():
		if set.url == "" || set.label == "" {
			return nil, "a set names both -url and -label"
		}
		if set.owned == set.reference {
			return nil, "a set names exactly one of -owned (the workspace's work made it) and -reference (it only points at it)"
		}
		if set.announces != "" && !set.owned {
			return nil, "-announces goes with -owned: only a link the workspace's work made announces another"
		}
		ownership := workspace.LinkOwnershipView(set.owned)
		if set.announces != "" {
			ownership.GetOwned().Announces = &agentreplv1.WorkspaceLinkAnnouncement{Url: set.announces}
		}
		req.Change = &agentreplv1.ChangeWorkspaceLinksRequest_Set{Set: &agentreplv1.ChangeWorkspaceLinksSet{Url: set.url, Label: set.label, Ownership: ownership}}
	case remove != "":
		req.Change = &agentreplv1.ChangeWorkspaceLinksRequest_Remove{Remove: &agentreplv1.ChangeWorkspaceLinksRemove{Url: remove}}
	case len(show) > 0:
		req.Change = &agentreplv1.ChangeWorkspaceLinksRequest_Show{Show: &agentreplv1.ChangeWorkspaceLinksShow{Urls: show}}
	default:
		req.Change = &agentreplv1.ChangeWorkspaceLinksRequest_Clear{Clear: &agentreplv1.ChangeWorkspaceLinksClear{}}
	}
	return req, ""
}

// remoteControlRequest reads a -remote-control flag into the request that
// turns workspace REF's remote control on or off. An agent names the mode it
// wants, never a toggle, so the answer is the same whatever the mode was.
func remoteControlRequest(ref *workspacev1.WorkspaceRef, text string) (*agentreplv1.UpdateRemoteControlModeRequest, error) {
	req := &agentreplv1.UpdateRemoteControlModeRequest{Workspace: ref}
	switch text {
	case "on":
		req.Action = &agentreplv1.UpdateRemoteControlModeRequest_On{On: &agentreplv1.UpdateRemoteControlModeOn{}}
	case "off":
		req.Action = &agentreplv1.UpdateRemoteControlModeRequest_Off{Off: &agentreplv1.UpdateRemoteControlModeOff{}}
	default:
		return nil, fmt.Errorf("-remote-control is on or off, not %q", text)
	}
	return req, nil
}

// queueBypassRequest reads the -queue-bypass and -context flags into the
// request that takes or releases workspace REF's queue bypass mode. An agent
// names the mode it wants, never a toggle; a context is named only when the
// mode is taken, and an on naming none keeps the behavior from before the
// menu.
func queueBypassRequest(ref *workspacev1.WorkspaceRef, mode, bypassContext string) (*agentreplv1.UpdateQueueBypassModeRequest, error) {
	req := &agentreplv1.UpdateQueueBypassModeRequest{Workspace: ref}
	switch mode {
	case "on":
		on := &agentreplv1.UpdateQueueBypassModeOn{}
		if bypassContext != "" {
			named, err := queueBypassContextOf(bypassContext)
			if err != nil {
				return nil, err
			}
			on.Context = named
		}
		req.Action = &agentreplv1.UpdateQueueBypassModeRequest_On{On: on}
	case "off":
		if bypassContext != "" {
			return nil, fmt.Errorf("-context is named only with -queue-bypass on")
		}
		req.Action = &agentreplv1.UpdateQueueBypassModeRequest_Off{Off: &agentreplv1.UpdateQueueBypassModeOff{}}
	case "":
		return nil, fmt.Errorf("-context is named only with -queue-bypass on")
	default:
		return nil, fmt.Errorf("-queue-bypass is on or off, not %q", mode)
	}
	return req, nil
}

// queueBypassContextOf reads a -context flag into the context it names.
func queueBypassContextOf(text string) (*frontendv1.TopbarQueueBypassContext, error) {
	switch text {
	case "full":
		return workspace.QueueBypassContextFullMain.Proto(), nil
	case "compacted":
		return workspace.QueueBypassContextCompactedMain.Proto(), nil
	case "existing":
		return workspace.QueueBypassContextExisting.Proto(), nil
	case "existing-plus-compacted":
		return workspace.QueueBypassContextExistingPlusCompactedMain.Proto(), nil
	}
	return nil, fmt.Errorf("-context is full, compacted, existing or existing-plus-compacted, not %q", text)
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

func workspaceFinished(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	place := fs.String("placement", "", "back or front")
	part := fs.String("version-part", "", "minor or patch, for a deploy onto a versioned repository's main branch")
	line := fs.String("changelog-line", "", "the version tag's changelog line")
	asUser := fs.Bool("as-user", false, "the user asks, not the calling workspace")
	named, ref, rest, ok := c.namedWorkspaceArg(fs, stateDir, args)
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
	resp, err := c.client.FinishWorkspace(c.ctx, connect.NewRequest(&agentreplv1.FinishWorkspaceRequest{Workspace: ref, Requester: requester,
		Placement: where, VersionIncrement: version}))
	return answerDescribed(c, resp, err, func(msg *agentreplv1.FinishWorkspaceResponse) (string, error) {
		return describeFinishRefusal(named, msg.GetError())
	})
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
	return onWorkspace(c, args, func(ref *workspacev1.WorkspaceRef) (*connect.Response[agentreplv1.KillWorkspaceResponse], error) {
		return c.client.KillWorkspace(c.ctx, connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: ref}))
	})
}

// workspaceSelect makes WS the workspace the editor and the webapp show.
func workspaceSelect(c *cliCall, args []string) int {
	return onWorkspace(c, args, func(ref *workspacev1.WorkspaceRef) (*connect.Response[agentreplv1.SelectWorkspaceResponse], error) {
		return c.client.SelectWorkspace(c.ctx, connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref}))
	})
}

// workspaceRestart bounces WS's session and reloads its page, at once, or
// with -when-idle once WS's running turn and detached work end
// (endpoint_restart_workspace.proto).
func workspaceRestart(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	whenIdle := fs.Bool("when-idle", false, "restart once the workspace is idle, interrupting nothing")
	ref, ok := c.onlyWorkspace(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	resp, err := c.client.RestartWorkspace(c.ctx, connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: ref, WhenIdle: *whenIdle}))
	return answer(c, resp, err)
}

// workspacePark parks WS's session as the keep-alive cycle parks one, or with
// -all every open workspace's (endpoint_park_workspace.proto).
func workspacePark(c *cliCall, args []string) int {
	if !slices.Contains(args, "-all") {
		return onWorkspace(c, args, func(ref *workspacev1.WorkspaceRef) (*connect.Response[agentreplv1.ParkWorkspaceResponse], error) {
			return c.client.ParkWorkspace(c.ctx, connect.NewRequest(&agentreplv1.ParkWorkspaceRequest{Workspace: ref}))
		})
	}
	fs, stateDir := c.flags()
	fs.Bool("all", false, "park every open workspace")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 0 {
		return c.usageFailed("-all names no workspace")
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	roster, ok := c.readRoster()
	if !ok {
		return exitFailure
	}
	// EVERY OPEN WORKSPACE IS ASKED, and the worst exit is the command's: one
	// refusal never stops the rest.
	exit := exitSuccess
	for _, row := range rosterwalk.OpenRows(roster) {
		ref := row.GetRow().GetWorkspace().GetWorkspace()
		resp, err := c.client.ParkWorkspace(c.ctx, connect.NewRequest(&agentreplv1.ParkWorkspaceRequest{Workspace: ref}))
		if code := answer(c, resp, err); code > exit {
			exit = code
		}
	}
	return exit
}

// workspaceInterrupt stops WS's running turn, or with -all-agents every live
// detached agent. A turn stop while agents are live is refused once, naming
// them; -confirm-agents stops them with it.
func workspaceInterrupt(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	allAgents := fs.Bool("all-agents", false, "stop every live detached agent instead of the turn")
	confirmAgents := fs.Bool("confirm-agents", false, "stop the live agents along with the turn")
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	if len(rest) != 0 || (*allAgents && *confirmAgents) {
		return c.usageFailed("the command names its workspace, with -all-agents or -confirm-agents but not both")
	}
	req := &agentreplv1.InterruptRequest{Workspace: ref, ConfirmAgents: *confirmAgents,
		Target: &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}}}
	if *allAgents {
		req.Target = &agentreplv1.InterruptRequest_AllAgents{AllAgents: &agentreplv1.InterruptAllAgents{}}
	}
	resp, err := c.client.Interrupt(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

// onWorkspace runs a verb that names one workspace and takes nothing else:
// it resolves the workspace from ARGS, refuses anything after it, and answers
// CALL's reply to the ref. Every such verb is this, so none parses its
// workspace on its own.
func onWorkspace[Res any](c *cliCall, args []string, call func(*workspacev1.WorkspaceRef) (*connect.Response[Res], error)) int {
	fs, stateDir := c.flags()
	ref, ok := c.onlyWorkspace(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	resp, err := call(ref)
	return answer(c, resp, err)
}

// onlyWorkspace parses ARGS against FS for a verb that names one workspace
// and nothing after it, refusing anything after it. It is onWorkspace's parse,
// shared by a verb that also takes flags of its own.
func (c *cliCall) onlyWorkspace(fs *flag.FlagSet, stateDir *string, args []string) (*workspacev1.WorkspaceRef, bool) {
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return nil, false
	}
	if len(rest) != 0 {
		c.usageFailed("the command names its workspace and nothing after it")
		return nil, false
	}
	return ref, true
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

// defaultFindLimit is how many workspaces `workspace find` answers unless
// -limit or -all says otherwise: enough to cover recent work, few enough that
// an agent reads the answer whole.
const defaultFindLimit = 50

// workspaceFind prints the workspaces the daemon's registry holds, open or
// closed, narrowed by the flags (agentrepl.v1.FindWorkspaces), each with its
// name and its summary, the most recently active first.
func workspaceFind(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	repository := fs.String("repository", "", "here, or a main checkout; unset is every repository")
	since := fs.String("since", "", "only workspaces active at or after WHEN: an age (90m, 36h, 7d) or a date (2026-10-20, RFC 3339)")
	until := fs.String("until", "", "only workspaces created at or before WHEN, spelled as -since is")
	standing := fs.String("standing", "", "only workspaces open, closed (landed or not), or landed")
	text := fs.String("text", "", "only workspaces whose name, branch or summary contains TEXT, case-insensitively")
	createdBy := fs.String("created-by", "", "only workspaces whose creation WS's agent asked for")
	limit := fs.Int("limit", defaultFindLimit, "answer at most N workspaces")
	all := fs.Bool("all", false, "answer every match, with no limit")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 0 {
		return c.usageFailed("nothing is positional")
	}
	limited := false
	fs.Visit(func(f *flag.Flag) { limited = limited || f.Name == "limit" })
	if limited && *all {
		return c.usageFailed("-limit and -all are exclusive")
	}
	req := &agentreplv1.FindWorkspacesRequest{}
	if !*all {
		if *limit < 1 {
			return c.usageFailed("-limit is at least 1; -all answers every match")
		}
		n := uint32(*limit)
		req.Limit = &n
	}
	window := &agentreplv1.FindWorkspacesWindow{}
	for _, bound := range []struct {
		flag, value string
		into        **int64
	}{{"since", *since, &window.SinceMs}, {"until", *until, &window.UntilMs}} {
		if bound.value == "" {
			continue
		}
		at, err := parseWhen(bound.value, c.deps.now())
		if err != nil {
			return c.usageFailed("-%s: %v", bound.flag, err)
		}
		ms := at.UnixMilli()
		*bound.into = &ms
	}
	if window.SinceMs != nil || window.UntilMs != nil {
		req.Window = window
	}
	switch *standing {
	case "":
	case "open":
		req.Standing = &agentreplv1.FindWorkspacesStanding{Standing: &agentreplv1.FindWorkspacesStanding_Open{Open: &agentreplv1.FindWorkspacesStandingOpen{}}}
	case "closed":
		req.Standing = &agentreplv1.FindWorkspacesStanding{Standing: &agentreplv1.FindWorkspacesStanding_Closed{Closed: &agentreplv1.FindWorkspacesStandingClosed{}}}
	case "landed":
		req.Standing = &agentreplv1.FindWorkspacesStanding{Standing: &agentreplv1.FindWorkspacesStanding_Landed{Landed: &agentreplv1.FindWorkspacesStandingLanded{}}}
	default:
		return c.usageFailed("-standing is open, closed or landed, not %q", *standing)
	}
	if *text != "" {
		req.Text = text
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	scope, ok := c.scopeFlag(*repository)
	if !ok {
		return exitFailure
	}
	req.Repository = scope
	if *createdBy != "" {
		if req.CreatedBy, ok = c.named(*createdBy); !ok {
			return exitFailure
		}
	}
	if req.Caller, ok = c.callerIfAny(); !ok {
		return exitFailure
	}
	resp, err := c.client.FindWorkspaces(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

// parseWhen reads a time flag: an age counted back from NOW (a Go duration
// such as 90m or 36h, or a whole number of days such as 7d), a date
// (2006-01-02, local midnight), or an RFC 3339 instant.
func parseWhen(value string, now time.Time) (time.Time, error) {
	if days, ok := strings.CutSuffix(value, "d"); ok {
		if n, err := strconv.Atoi(days); err == nil && n >= 0 {
			return now.AddDate(0, 0, -n), nil
		}
	}
	if age, err := time.ParseDuration(value); err == nil && age >= 0 {
		return now.Add(-age), nil
	}
	if at, err := time.ParseInLocation("2006-01-02", value, time.Local); err == nil {
		return at, nil
	}
	if at, err := time.Parse(time.RFC3339, value); err == nil {
		return at, nil
	}
	return time.Time{}, fmt.Errorf("%q is no age (90m, 36h, 7d), date (2006-01-02) or RFC 3339 instant", value)
}

// workspaceSubsections prints the sidebar arrangement of the repository WS
// sits in (agentrepl.v1.ArrangedRepository): its subsections by name, each
// with the workspaces in it, and its unsectioned workspaces, in drawn order.
// It is what a new workspace reads to choose the subsection it belongs in.
func workspaceSubsections(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	ref, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	if len(rest) != 0 {
		return c.usageFailed("the command names its workspace and nothing after it")
	}
	arranged, ok := c.arrangedRepositoryOf(ref)
	if !ok || !c.print(arranged) {
		return exitFailure
	}
	return exitSuccess
}

// workspacePlace files the families of the workspaces it names, in order, at
// the bottom of a subsection of their repository: the one named -subsection
// (ArrangeSidebar's place_workspaces), or a new one named -new-subsection
// that the calling workspace's agent creates for them
// (create_agent_subsection), which the daemon deletes once it holds no open
// workspace.
func workspacePlace(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	name := fs.String("subsection", "", "the existing subsection's name, as `subsections` prints it")
	newName := fs.String("new-subsection", "", "the name of a subsection to create for the workspaces")
	first, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	existing, created := strings.TrimSpace(*name) != "", strings.TrimSpace(*newName) != ""
	if existing == created {
		return c.usageFailed("the command names one destination, with -subsection NAME or -new-subsection NAME")
	}
	refs := []*workspacev1.WorkspaceRef{first}
	for _, positional := range rest {
		ref, ok := c.named(positional)
		if !ok {
			return exitFailure
		}
		refs = append(refs, ref)
	}
	if created {
		creator, ok := c.calling()
		if !ok {
			return exitFailure
		}
		resp, err := c.client.ArrangeSidebar(c.ctx, connect.NewRequest(&agentreplv1.ArrangeSidebarRequest{
			ByAgent: true,
			Change: &agentreplv1.ArrangeSidebarRequest_CreateAgentSubsection{CreateAgentSubsection: &agentreplv1.ArrangeCreateAgentSubsection{
				CreatedBy: creator, Name: *newName, Workspaces: refs,
			}}}))
		return answer(c, resp, err)
	}
	arranged, ok := c.arrangedRepositoryOf(refs[0])
	if !ok {
		return exitFailure
	}
	subsection, ok := c.subsectionNamed(arranged, *name)
	if !ok {
		return exitFailure
	}
	resp, err := c.client.ArrangeSidebar(c.ctx, connect.NewRequest(&agentreplv1.ArrangeSidebarRequest{
		ByAgent: true,
		Change: &agentreplv1.ArrangeSidebarRequest_PlaceWorkspaces{PlaceWorkspaces: &agentreplv1.ArrangePlaceWorkspaces{
			Workspaces: refs,
			Into:       &agentreplv1.ArrangePlaceWorkspaces_Subsection{Subsection: subsection},
			At:         &agentreplv1.ArrangePlaceWorkspaces_End{End: &agentreplv1.ArrangeEnd{}},
		}}}))
	return answer(c, resp, err)
}

// subsectionNamed answers the ref of ARRANGED's one subsection named NAME,
// compared after trimming as the daemon trims a subsection's name. No
// subsection, or two, by that name is refused, naming the ones there are.
func (c *cliCall) subsectionNamed(arranged *agentreplv1.ArrangedRepository, name string) (*agentreplv1.SubsectionRef, bool) {
	name = strings.TrimSpace(name)
	var (
		found *agentreplv1.SubsectionRef
		count int
		names []string
	)
	for _, entry := range arranged.GetEntries() {
		sub := entry.GetSubsection()
		if sub == nil {
			continue
		}
		names = append(names, strconv.Quote(sub.GetName()))
		if sub.GetName() == name {
			found = sub.GetSubsection()
			count++
		}
	}
	switch count {
	case 0:
		return nil, c.failed("repository %s has no subsection named %q; its subsections are [%s]",
			arranged.GetName(), name, strings.Join(names, ", "))
	case 1:
		return found, true
	}
	return nil, c.failed("repository %s has %d subsections named %q; rename one so the name picks one", arranged.GetName(), count, name)
}

// workspaceList prints every workspace the sidebar roster draws, in every
// repository, one agentrepl.v1.FoundWorkspace a line: the record `workspace
// find` answers, asked for with on_roster, so the line carries the
// workspace's name, ref, branch, repository, summary, standing, timestamps,
// the roster section and live status it is drawn with, and calling for the
// caller's own record. `status WS` reads one in full.
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
	caller, ok := c.callerIfAny()
	if !ok {
		return exitFailure
	}
	resp, err := c.client.FindWorkspaces(c.ctx, connect.NewRequest(&agentreplv1.FindWorkspacesRequest{
		OnRoster: &agentreplv1.FindWorkspacesOnRoster{}, Caller: caller}))
	if err != nil || resp.Msg.GetError() != nil {
		return answer(c, resp, err)
	}
	complete := resp.Msg.GetSuccess().GetComplete()
	if complete == nil {
		c.failed("the daemon answered a find with no limit as %v, not a complete extent", resp.Msg)
		return exitFailure
	}
	for _, ws := range complete.GetWorkspaces() {
		text, err := protojson.Marshal(ws)
		if err != nil {
			c.failed("render workspace %s: %v", ws.GetName(), err)
			return exitFailure
		}
		say(c.out, "%s\n", text)
	}
	return exitSuccess
}

// noticeEvents are the -events spellings of workspace subscribe: each event
// the store declares, by the name it records it under, so a new event is
// spelled here with no second list to keep in step.
var noticeEvents = func() map[string]agentreplv1.WorkspaceNoticeEvent {
	out := map[string]agentreplv1.WorkspaceNoticeEvent{}
	for _, e := range wsm.AllWorkspaceEvents().Members() {
		out[e.String()] = workspacesubs.EventProto(e)
	}
	return out
}()

// allNoticeEvents names every -events spelling, for the usage.
var allNoticeEvents = wsm.AllWorkspaceEvents().String()

func workspaceSubscribe(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	events := fs.String("events", "", "comma-separated: "+strings.ReplaceAll(allNoticeEvents, ",", ", "))
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

// pullRequestEvents are the -events spellings of pr-subscribe: each event
// the store declares, by the name it records it under, so a new event is
// spelled here with no second list to keep in step.
var pullRequestEvents = func() map[string]agentreplv1.PullRequestNoticeEvent {
	out := map[string]agentreplv1.PullRequestNoticeEvent{}
	for _, e := range wsm.AllPullRequestEvents().Members() {
		out[e.String()] = prwatch.EventProto(e)
	}
	return out
}()

// allPullRequestEvents is pr-subscribe's default: every activity kind.
var allPullRequestEvents = wsm.AllPullRequestEvents().String()

// workspacePullRequestSubscribe subscribes WS's agent to a pull request: a
// url, `#N` in WS's repository, or OWNER/REPO#N.
func workspacePullRequestSubscribe(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	pr := fs.String("pr", "", "the pull request: a url, #N in the workspace's repository, or OWNER/REPO#N")
	events := fs.String("events", allPullRequestEvents, "comma-separated: "+strings.ReplaceAll(allPullRequestEvents, ",", ", "))
	subscriber, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	var wanted []agentreplv1.PullRequestNoticeEvent
	for _, name := range strings.Split(*events, ",") {
		event, ok := pullRequestEvents[strings.TrimSpace(name)]
		if !ok {
			return c.usageFailed("%q is no pull-request event", name)
		}
		wanted = append(wanted, event)
	}
	if strings.TrimSpace(*pr) == "" {
		return c.usageFailed("-pr names the pull request")
	}
	req := &agentreplv1.SubscribePullRequestRequest{Subscriber: subscriber, Events: wanted,
		PullRequest: &agentreplv1.SubscribePullRequestRequest_Text{Text: *pr}}
	if key, err := wsm.ParsePullRequestKey(*pr); err == nil {
		req.PullRequest = &agentreplv1.SubscribePullRequestRequest_Ref{Ref: prwatch.RefProto(key)}
	}
	resp, err := c.client.SubscribePullRequest(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

// pullRequestFlag reads a -pr naming a pull request exactly: a url or
// OWNER/REPO#N.
func pullRequestFlag(text string) (*agentreplv1.PullRequestRef, string) {
	if key, ok := prwatch.ParseURL(text); ok {
		return prwatch.RefProto(key), ""
	}
	if key, err := wsm.ParsePullRequestKey(text); err == nil {
		return prwatch.RefProto(key), ""
	}
	return nil, fmt.Sprintf("-pr is a github.com pull request url or OWNER/REPO#N, not %q", text)
}

func workspacePullRequestUnsubscribe(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	pr := fs.String("pr", "", "the pull request: a url or OWNER/REPO#N")
	subscriber, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	ref, problem := pullRequestFlag(*pr)
	if problem != "" {
		return c.usageFailed("%s", problem)
	}
	resp, err := c.client.UnsubscribePullRequest(c.ctx, connect.NewRequest(&agentreplv1.UnsubscribePullRequestRequest{Subscriber: subscriber, PullRequest: ref}))
	return answer(c, resp, err)
}

func workspacePullRequestSubscriptions(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	subscriber, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	resp, err := c.client.ListPullRequestSubscriptions(c.ctx, connect.NewRequest(&agentreplv1.ListPullRequestSubscriptionsRequest{Subscriber: subscriber}))
	return answer(c, resp, err)
}

// workspaceGithubQueueSubscribe subscribes WS's agent to a GitHub merge
// queue: -repository's (WS's own repository when unset), on -branch (its
// default branch when unset).
func workspaceGithubQueueSubscribe(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	repository := fs.String("repository", "", "the repository as OWNER/REPO; the workspace's own when unset")
	branch := fs.String("branch", "", "the branch whose queue is followed; the repository's default branch when unset")
	subscriber, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	req := &agentreplv1.SubscribeGithubMergeQueueRequest{Subscriber: subscriber,
		Repository: &agentreplv1.SubscribeGithubMergeQueueRequest_Subscribers{Subscribers: &agentreplv1.SubscribeGithubMergeQueueSubscribersRepository{}}}
	if *repository != "" {
		owner, repo, found := strings.Cut(*repository, "/")
		if !found || owner == "" || repo == "" || strings.Contains(repo, "/") {
			return c.usageFailed("-repository is OWNER/REPO, not %q", *repository)
		}
		req.Repository = &agentreplv1.SubscribeGithubMergeQueueRequest_Named{Named: &agentreplv1.GithubRepositoryRef{Owner: owner, Repo: repo}}
	}
	if *branch != "" {
		req.Branch = branch
	}
	resp, err := c.client.SubscribeGithubMergeQueue(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

func workspaceGithubQueueUnsubscribe(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	queue := fs.String("queue", "", "the queue as OWNER/REPO@BRANCH")
	subscriber, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	key, err := wsm.ParseGithubMergeQueueKey(*queue)
	if err != nil {
		return c.usageFailed("-queue is OWNER/REPO@BRANCH, not %q", *queue)
	}
	resp, err := c.client.UnsubscribeGithubMergeQueue(c.ctx, connect.NewRequest(&agentreplv1.UnsubscribeGithubMergeQueueRequest{Subscriber: subscriber, MergeQueue: prwatch.QueueRefProto(key)}))
	return answer(c, resp, err)
}

func workspaceGithubQueueSubscriptions(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	subscriber, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	resp, err := c.client.ListGithubMergeQueueSubscriptions(c.ctx, connect.NewRequest(&agentreplv1.ListGithubMergeQueueSubscriptionsRequest{Subscriber: subscriber}))
	return answer(c, resp, err)
}

// workspacePullRequestComment posts a comment on a pull request for WS,
// through the daemon, so the comment is never told back to WS.
func workspacePullRequestComment(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	pr := fs.String("pr", "", "the pull request: a url or OWNER/REPO#N")
	body := fs.String("body", "", "the comment's markdown")
	bodyFile := fs.String("body-file", "", "a file holding the comment's markdown")
	inReplyTo := fs.Uint64("in-reply-to", 0, "a line comment's id: the comment is a reply in its thread")
	ws, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	ref, problem := pullRequestFlag(*pr)
	if problem != "" {
		return c.usageFailed("%s", problem)
	}
	if (*body == "") == (*bodyFile == "") {
		return c.usageFailed("name exactly one of -body and -body-file")
	}
	markdown := *body
	if *bodyFile != "" {
		raw, err := os.ReadFile(*bodyFile)
		if err != nil {
			return c.usageFailed("read -body-file: %v", err)
		}
		markdown = string(raw)
	}
	req := &agentreplv1.CommentOnPullRequestRequest{Workspace: ws, PullRequest: ref,
		Placement: &agentreplv1.CommentOnPullRequestRequest_Conversation{Conversation: &agentreplv1.CommentOnPullRequestConversation{Markdown: markdown}}}
	if *inReplyTo != 0 {
		req.Placement = &agentreplv1.CommentOnPullRequestRequest_Reply{Reply: &agentreplv1.CommentOnPullRequestReply{InReplyToCommentId: *inReplyTo, Markdown: markdown}}
	}
	resp, err := c.client.CommentOnPullRequest(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

// workspaceNotify posts a desktop banner attributed to WS: titled by WS's
// name, TEXT below it, its click bringing Emacs forward on WS
// (endpoint_notify_workspace.proto). WS may be the caller's own workspace or
// any other.
func workspaceNotify(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	ws, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return exitFailure
	}
	if len(rest) != 1 || strings.TrimSpace(rest[0]) == "" {
		return c.usageFailed("the command names the banner's text, once")
	}
	resp, err := c.client.NotifyWorkspace(c.ctx, connect.NewRequest(&agentreplv1.NotifyWorkspaceRequest{Workspace: ws, Message: rest[0]}))
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
		// THE STANDING, AS THE CLI PRINTS EVERY ANSWER: compact protojson,
		// never a Go struct dump of the whole response.
		standing, err := protojson.Marshal(status.Msg.GetSuccess().GetStatus().GetStanding())
		if err != nil {
			c.failed("no cold gate stands on the workspace, and its standing could not be printed: %v", err)
			return exitRefused
		}
		c.failed("no cold gate stands on the workspace; it stands %s", standing)
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

// queuedArg answers the merge a queue verb names: `here`, the calling
// workspace's own merge, or a merge's NAME as the queues list it, which must
// name exactly one merge across every queue.
//
// THE QUEUE NAMES ITS MERGES, NOT THE ROSTER. An async merge runs in a hidden
// workspace, and the roster leaves hidden workspaces out, so a verb that read
// the name off the roster could address no async merge at all (2026-10-22:
// `merge-queue move-down pt3-sentences` answered "the daemon's roster holds no
// workspace named" while the queue listed it waiting).
func (c *cliCall) queuedArg(fs *flag.FlagSet, stateDir *string, args []string) (*workspacev1.WorkspaceRef, []string, bool) {
	positionals, ok := c.targetArgs(fs, stateDir, args, "merge")
	if !ok {
		return nil, nil, false
	}
	name := positionals[0]
	if name == here {
		ref, ok := c.calling()
		return ref, positionals[1:], ok
	}
	resp, err := c.client.GetMergeQueueStatus(c.ctx, connect.NewRequest(&agentreplv1.GetMergeQueueStatusRequest{}))
	if err != nil {
		return nil, nil, c.failed("read the merge queues: %v", err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		return nil, nil, c.failed("the daemon refused to read the merge queues: %v", resp.Msg.GetResult())
	}
	var found []*workspacev1.WorkspaceRef
	for _, queue := range success.GetQueues() {
		for _, entry := range queue.GetEntries() {
			if entry.GetName() == name {
				found = append(found, entry.GetWorkspace())
			}
		}
	}
	switch len(found) {
	case 0:
		return nil, nil, c.failed("no merge in any queue is named %q", name)
	case 1:
		return found[0], positionals[1:], true
	}
	return nil, nil, c.failed("%d merges in the queues are named %q", len(found), name)
}

func queueDequeue(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	ref, rest, ok := c.queuedArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	return c.updateMergeQueue(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: ref}}})
}

// updateMergeQueue sends one merge-queue action, naming the calling workspace
// as its requester: the one way every queue verb asks, so none can act on a
// queue without the daemon recording who asked.
func (c *cliCall) updateMergeQueue(req *agentreplv1.UpdateMergeQueueRequest) int {
	requester, ok := c.requester(false)
	if !ok {
		return exitFailure
	}
	req.Requester = requester
	resp, err := c.client.UpdateMergeQueue(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

func queueMove(up bool) func(c *cliCall, args []string) int {
	return func(c *cliCall, args []string) int {
		fs, stateDir := c.flags()
		ref, rest, ok := c.queuedArg(fs, stateDir, args)
		if !ok || len(rest) != 0 {
			return exitFailure
		}
		req := &agentreplv1.UpdateMergeQueueRequest{Action: &agentreplv1.UpdateMergeQueueRequest_MoveDown{MoveDown: &agentreplv1.UpdateMergeQueueMoveDown{Workspace: ref}}}
		if up {
			req.Action = &agentreplv1.UpdateMergeQueueRequest_MoveUp{MoveUp: &agentreplv1.UpdateMergeQueueMoveUp{Workspace: ref}}
		}
		return c.updateMergeQueue(req)
	}
}

func queuePause(pause bool) func(c *cliCall, args []string) int {
	return func(c *cliCall, args []string) int {
		fs, stateDir := c.flags()
		repository := fs.String("repository", "", "here, or a main checkout; unset is every repository")
		hard := false
		if pause {
			fs.BoolVar(&hard, "hard", false, "a hard stop: also interrupt the turns the queue's merges drive")
		}
		if _, ok := c.parse(fs, args); !ok || !c.dial(*stateDir) {
			return exitFailure
		}
		scope, ok := c.scopeFlag(*repository)
		if !ok {
			return exitFailure
		}
		req := &agentreplv1.UpdateMergeQueueRequest{Action: &agentreplv1.UpdateMergeQueueRequest_Resume{Resume: &agentreplv1.UpdateMergeQueueResume{Repository: scope}}}
		if pause {
			req.Action = &agentreplv1.UpdateMergeQueueRequest_Pause{Pause: &agentreplv1.UpdateMergeQueuePause{Repository: scope, Hard: hard}}
		}
		return c.updateMergeQueue(req)
	}
}

// queueSubscribe has the caller told of a merge put in line, a merge leaving
// the queue and each deploy's end, in one short line each.
func queueSubscribe(c *cliCall, args []string) int {
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
	resp, err := c.client.SubscribeMergeQueue(c.ctx, connect.NewRequest(&agentreplv1.SubscribeMergeQueueRequest{Subscriber: subscriber, Repository: scope}))
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

func runtimeDeployHold(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	repository := fs.String("repository", "", "here, or a main checkout")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 1 || *repository == "" {
		return c.usageFailed("the command names hold or release, and -repository is required")
	}
	req := &agentreplv1.UpdateDeployHoldRequest{}
	switch positionals[0] {
	case "hold":
		req.Action = &agentreplv1.UpdateDeployHoldRequest_Hold{Hold: &agentreplv1.UpdateDeployHoldHold{}}
	case "release":
		req.Action = &agentreplv1.UpdateDeployHoldRequest_Release{Release: &agentreplv1.UpdateDeployHoldRelease{}}
	default:
		return c.usageFailed("a repository's deploys are held or released, not %q", positionals[0])
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	scope, ok := c.scopeFlag(*repository)
	if !ok {
		return exitFailure
	}
	req.Repository = scope
	resp, err := c.client.UpdateDeployHold(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

// repositoryRegister registers the repository PATH is inside with agent-repl,
// and opens its main worktree as a workspace. PATH is any path inside the
// repository, a relative one counted from the working directory, and `here` is
// the working directory itself. It needs no calling workspace: registering a
// repository is how an agent in a directory agent-repl does not yet know gets
// it known. The daemon resolves the main worktree through git and answers
// `already_known` for a repository it has; its error arms exit refused.
func repositoryRegister(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 1 {
		return c.usageFailed("the command names the one path inside the repository, or here")
	}
	cwd, err := c.deps.getwd()
	if err != nil {
		c.failed("read the working directory: %v", err)
		return exitFailure
	}
	path := cwd
	if positionals[0] != here {
		path = positionals[0]
		if !filepath.IsAbs(path) {
			path = filepath.Join(cwd, path)
		}
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	resp, err := c.client.RegisterRepository(c.ctx, connect.NewRequest(&agentreplv1.RegisterRepositoryRequest{Path: filepath.Clean(path)}))
	return answer(c, resp, err)
}

// runtimePruneStorage runs the daemon's storage sweep now. -preview asks the
// sweep to judge and answer what it would delete, deleting nothing; it is
// not -dry-run, which stops the request from being sent at all.
func runtimePruneStorage(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	preview := fs.Bool("preview", false, "answer what the sweep would delete, deleting nothing")
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
	resp, err := c.client.PruneStorage(c.ctx, connect.NewRequest(&agentreplv1.PruneStorageRequest{DryRun: *preview}))
	return answer(c, resp, err)
}

// runtimeReapWorktrees runs the daemon's landed-worktree reaper now. -preview
// asks the reaper to judge and answer what it would do, changing nothing (it
// is not -dry-run, which stops the request from being sent at all); -force
// takes closed workspaces' worktrees without waiting out the reaper's idle and
// expiry windows.
func runtimeReapWorktrees(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	preview := fs.Bool("preview", false, "answer what the reaper would do, changing nothing")
	force := fs.Bool("force", false, "take closed workspaces' worktrees without waiting out the idle and expiry windows")
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
	resp, err := c.client.ReapWorktrees(c.ctx, connect.NewRequest(&agentreplv1.ReapWorktreesRequest{DryRun: *preview, Force: *force}))
	return answer(c, resp, err)
}

// runtimeRollOutLanding is a deploy hook's call. -daemon dials the daemon
// running the deploy (AGENT_REPL_DEPLOY_DAEMON) rather than the one the
// address file names, which a handover can make the other one; a ledger is
// known only to the daemon that runs it.
func runtimeRollOutLanding(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	deploy := fs.String("deploy", "", "the deploy's ledger, as AGENT_REPL_DEPLOY_LEDGER names it; unset starts the rollout at once")
	daemon := fs.String("daemon", "", "the address of the daemon running the deploy, as AGENT_REPL_DEPLOY_DAEMON names it; unset dials the serving daemon")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 0 {
		return c.usageFailed("nothing is positional")
	}
	if *daemon != "" {
		c.dialAddress(*daemon)
	} else if !c.dial(*stateDir) {
		return exitFailure
	}
	req := &agentreplv1.RollOutLandingRequest{}
	if *deploy != "" {
		req.Deploy = &agentreplv1.DeployLedger{Ledger: *deploy}
	}
	resp, err := c.client.RollOutLanding(c.ctx, connect.NewRequest(req))
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
