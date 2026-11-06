// Package ghclient is the daemon's ONE way to GitHub: it runs `gh api` as the
// user's own `gh` login and answers the HTTP status, headers and body GitHub
// sent, or a typed failure. It reaches both of GitHub's APIs: REST (Get,
// Post) and GraphQL (GraphQL), which alone exposes merge queues.
//
// THE STATUS LINE IS THE ANSWER, NEVER THE EXIT CODE. `gh api --include`
// writes GitHub's status line, headers and body to stdout whatever GitHub
// answered, and exits 1 for every status that is not 2xx, a 304 included. So
// a run that wrote a status line is GitHub's answer, and only a run that
// wrote none (no network, no login, no `gh`) is a failure to reach GitHub.
//
// THE TOKEN NEVER PASSES THROUGH THE DAEMON. `gh` reads it from the keyring;
// nothing here sees it, logs it or stores it.
//
// EVERY FAILURE IS RECORDED HERE, ONCE, the ownership point: a caller that
// propagates a *Failure does not record it again.
package ghclient

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/textproto"
	"os"
	"sort"
	"strconv"
	"strings"
	"time"

	"agent-repld/internal/childproc"
	"agent-repld/internal/dlog"
)

// ghBinary is the `gh` every call runs: "gh", found on PATH, unless the build
// named another. Test builds of the daemon link a fake in here
// (`-ldflags -X agent-repld/internal/ghclient.ghBinary=<path>`), so a test
// daemon cannot reach GitHub whatever its PATH holds.
var ghBinary = "gh"

// Binary answers the `gh` executable this build runs.
func Binary() string { return ghBinary }

// Response is GitHub's answer to a call that reached it with a 2xx or a 304.
type Response struct {
	// Status is the HTTP status: 200, 201, or 304 (NotModified).
	Status int
	// Header is the response's headers.
	Header http.Header
	// Body is the response's body; empty for a 304.
	Body []byte
}

// NotModified reports whether GitHub answered 304 to a conditional read.
func (r Response) NotModified() bool { return r.Status == http.StatusNotModified }

// ETag is the response's entity tag, the value a later conditional read
// sends; empty when GitHub sent none.
func (r Response) ETag() string { return r.Header.Get("ETag") }

// Kind is why a call failed.
type Kind string

// The kinds.
const (
	// KindUnauthorized is a 401: the `gh` login's credentials were refused.
	KindUnauthorized Kind = "unauthorized"
	// KindNotFound is a 404: the resource is not visible to the login.
	KindNotFound Kind = "not_found"
	// KindRateLimited is a 403 or 429 with the rate limit spent.
	KindRateLimited Kind = "rate_limited"
	// KindUnreachable is no answer at all, or a 5xx.
	KindUnreachable Kind = "unreachable"
	// KindRefused is any other 4xx: GitHub understood and refused.
	KindRefused Kind = "refused"
)

// Failure is a call that did not get a 2xx or a 304.
type Failure struct {
	Kind Kind
	// Method and Path are the call.
	Method string
	Path   string
	// Status is the HTTP status; zero when GitHub was not reached.
	Status int
	// Message is GitHub's message for a status, or what `gh` said when
	// GitHub was not reached.
	Message string
	// ResetsAt is when a spent rate limit resets; zero for other kinds.
	ResetsAt time.Time
}

func (f *Failure) Error() string {
	if f.Status == 0 {
		return fmt.Sprintf("ghclient: %s %s: GitHub was not reached: %s", f.Method, f.Path, f.Message)
	}
	return fmt.Sprintf("ghclient: %s %s: HTTP %d (%s): %s", f.Method, f.Path, f.Status, f.Kind, f.Message)
}

// Client calls GitHub's REST and GraphQL APIs.
type Client interface {
	// Get reads PATH (relative to the API root, query included). A non-empty
	// ETAG makes the read conditional, and an unchanged resource answers a
	// Response whose NotModified is true.
	Get(ctx context.Context, path, etag string) (Response, error)
	// Post creates under PATH with the string FIELDS as its JSON body.
	Post(ctx context.Context, path string, fields map[string]string) (Response, error)
	// GraphQL runs QUERY with VARIABLES against GitHub's GraphQL API. GitHub
	// answers a GraphQL error with an HTTP 200 beside whatever data it could
	// resolve, so an answer with data carries its errors for the caller to
	// judge per path, and only an answer with no data at all is a *Failure.
	GraphQL(ctx context.Context, query string, variables map[string]any) (GraphQLResponse, error)
}

// GraphQLResponse is GitHub's answer to a GraphQL call that resolved data.
type GraphQLResponse struct {
	// Data is the `data` object, never null.
	Data json.RawMessage
	// Errors is every error GitHub reported beside the data: each names the
	// path of the field it nulled. Empty when the whole query resolved.
	Errors []GraphQLError
}

// GraphQLError is one error GitHub reported for a GraphQL call.
type GraphQLError struct {
	// Type is GitHub's error type (`NOT_FOUND`, `RATE_LIMITED`, `FORBIDDEN`);
	// empty when GitHub named none.
	Type string
	// Message is GitHub's message.
	Message string
	// Path is the field the error nulled, from the query's root, each step
	// spelled as text (`repository`, `pr12`).
	Path []string
}

// Kind is the failure kind the error's type is.
func (e GraphQLError) Kind() Kind {
	switch e.Type {
	case "NOT_FOUND":
		return KindNotFound
	case "RATE_LIMITED":
		return KindRateLimited
	}
	return KindRefused
}

// graphqlPath is the one path every GraphQL call posts to.
const graphqlPath = "graphql"

// Config is a client's collaborators.
type Config struct {
	// Binary is the `gh` to run; empty is Binary().
	Binary string
	// Log records every failure, and every 304 at debug.
	Log dlog.Logger
}

// New builds a client.
func New(cfg Config) (Client, error) {
	if cfg.Log == nil {
		return nil, errors.New("ghclient: Log is required")
	}
	if cfg.Binary == "" {
		cfg.Binary = ghBinary
	}
	return &client{cfg: cfg}, nil
}

type client struct {
	cfg Config
}

func (c *client) Get(ctx context.Context, path, etag string) (Response, error) {
	args := []string{"api", "--include", "--method", "GET", path}
	if etag != "" {
		args = append(args, "--header", "If-None-Match: "+etag)
	}
	return c.call(ctx, "daemon.ghclient.get", http.MethodGet, path, args, nil)
}

func (c *client) Post(ctx context.Context, path string, fields map[string]string) (Response, error) {
	args := []string{"api", "--include", "--method", "POST", path}
	for _, name := range sortedKeys(fields) {
		args = append(args, "--raw-field", name+"="+fields[name])
	}
	return c.call(ctx, "daemon.ghclient.post", http.MethodPost, path, args, nil)
}

// GraphQL posts the query and its variables as the JSON body `gh` reads from
// stdin, so no variable passes through an argument's quoting or `gh`'s
// guessing of a field's type.
func (c *client) GraphQL(ctx context.Context, query string, variables map[string]any) (GraphQLResponse, error) {
	const op = "daemon.ghclient.graphql"
	body, err := json.Marshal(map[string]any{"query": query, "variables": variables})
	if err != nil {
		// Variables are the daemon's own strings and numbers; one that does
		// not encode is a defect.
		panic(fmt.Sprintf("ghclient: encode a GraphQL request: %v", err))
	}
	resp, err := c.call(ctx, op, http.MethodPost, graphqlPath, []string{"api", "--include", "--method", "POST", graphqlPath, "--input", "-"}, body)
	if err != nil {
		return GraphQLResponse{}, err
	}
	var shaped struct {
		Data   json.RawMessage `json:"data"`
		Errors []struct {
			Type    string `json:"type"`
			Message string `json:"message"`
			Path    []any  `json:"path"`
		} `json:"errors"`
	}
	fields := dlog.Context{"method": http.MethodPost, "path": graphqlPath, "status": resp.Status}
	if err := json.Unmarshal(resp.Body, &shaped); err != nil {
		f := &Failure{Kind: KindUnreachable, Method: http.MethodPost, Path: graphqlPath, Status: resp.Status, Message: "GitHub's GraphQL answer is not JSON: " + err.Error()}
		c.cfg.Log.Error(op, "GitHub's GraphQL answer cannot be read", dlog.Merge(fields, dlog.Context{"kind": string(f.Kind), "cause": err.Error()}))
		return GraphQLResponse{}, f
	}
	out := GraphQLResponse{Data: shaped.Data}
	for _, e := range shaped.Errors {
		g := GraphQLError{Type: e.Type, Message: e.Message}
		for _, step := range e.Path {
			g.Path = append(g.Path, fmt.Sprint(step))
		}
		out.Errors = append(out.Errors, g)
	}
	if len(shaped.Data) == 0 || string(shaped.Data) == "null" {
		f := &Failure{Kind: KindRefused, Method: http.MethodPost, Path: graphqlPath, Status: resp.Status, Message: "GitHub's GraphQL answer carries no data"}
		if len(out.Errors) > 0 {
			f.Kind, f.Message = out.Errors[0].Kind(), out.Errors[0].Message
		}
		c.cfg.Log.Error(op, "GitHub refused the GraphQL call", dlog.Merge(fields, dlog.Context{"kind": string(f.Kind), "message": f.Message, "errors": len(out.Errors)}))
		return GraphQLResponse{}, f
	}
	for _, e := range out.Errors {
		c.cfg.Log.Error(op, "GitHub nulled a field of the GraphQL answer", dlog.Merge(fields, dlog.Context{"type": e.Type, "message": e.Message, "field": strings.Join(e.Path, ".")}))
	}
	return out, nil
}

// call runs `gh ARGS`, with STDIN as its input when set, and judges its
// answer.
func (c *client) call(ctx context.Context, op, method, path string, args []string, stdin []byte) (Response, error) {
	cmd := childproc.CommandContext(ctx, childproc.Stable, c.cfg.Binary, args...)
	if stdin != nil {
		cmd.Stdin = bytes.NewReader(stdin)
	}
	cmd.Env = append(os.Environ(), "GH_PROMPT_DISABLED=1", "NO_COLOR=1", "GH_NO_UPDATE_NOTIFIER=1")
	fields := dlog.Context{"method": method, "path": path}
	// The output goes to files, never pipes, as every captured child's does
	// (childproc.Run).
	stdout, stderr, runErr, captureErr := childproc.Run(cmd, nil)
	if ctx.Err() != nil {
		// The daemon stopped the call: no fact about GitHub.
		return Response{}, ctx.Err()
	}
	if captureErr != nil {
		f := &Failure{Kind: KindUnreachable, Method: method, Path: path, Message: captureErr.Error()}
		c.cfg.Log.Error(op, "could not capture gh's output", dlog.Merge(fields, dlog.Context{"kind": string(f.Kind), "cause": captureErr.Error()}))
		return Response{}, f
	}
	resp, parsed := parse([]byte(stdout))
	if !parsed {
		detail := strings.TrimSpace(stderr)
		if detail == "" && runErr != nil {
			detail = runErr.Error()
		}
		f := &Failure{Kind: KindUnreachable, Method: method, Path: path, Message: detail}
		c.cfg.Log.Error(op, "GitHub could not be reached", dlog.Merge(fields, dlog.Context{"kind": string(f.Kind), "stderr": detail}))
		return Response{}, f
	}
	fields["status"] = resp.Status
	fields["rate_remaining"] = resp.Header.Get("X-RateLimit-Remaining")
	if resp.Status/100 == 2 || resp.NotModified() {
		if resp.NotModified() {
			c.cfg.Log.Debug(op, "GitHub answered the conditional read unchanged", fields)
		}
		return resp, nil
	}
	f := judge(method, path, resp)
	fields["kind"], fields["message"] = string(f.Kind), f.Message
	if !f.ResetsAt.IsZero() {
		fields["resets_at"] = f.ResetsAt.Format(time.RFC3339)
	}
	c.cfg.Log.Error(op, "GitHub refused the call", fields)
	return Response{}, f
}

// judge turns a status GitHub answered that is neither 2xx nor 304 into its
// failure.
func judge(method, path string, resp Response) *Failure {
	f := &Failure{Method: method, Path: path, Status: resp.Status, Message: messageOf(resp.Body)}
	remaining := resp.Header.Get("X-RateLimit-Remaining")
	switch {
	case resp.Status == http.StatusUnauthorized:
		f.Kind = KindUnauthorized
	case resp.Status == http.StatusNotFound:
		f.Kind = KindNotFound
	case (resp.Status == http.StatusForbidden || resp.Status == http.StatusTooManyRequests) && (remaining == "0" || resp.Status == http.StatusTooManyRequests):
		f.Kind = KindRateLimited
		if reset, err := strconv.ParseInt(resp.Header.Get("X-RateLimit-Reset"), 10, 64); err == nil {
			f.ResetsAt = time.Unix(reset, 0).UTC()
		}
	case resp.Status >= 500:
		f.Kind = KindUnreachable
	default:
		f.Kind = KindRefused
	}
	return f
}

// parse reads `gh api --include` output: a status line, headers, a blank
// line, and the body. PARSED is false when the output starts with no status
// line, which is how a run that never reached GitHub looks.
func parse(out []byte) (resp Response, parsed bool) {
	reader := textproto.NewReader(bufio.NewReader(bytes.NewReader(out)))
	line, err := reader.ReadLine()
	if err != nil || !strings.HasPrefix(line, "HTTP/") {
		return Response{}, false
	}
	_, rest, _ := strings.Cut(line, " ")
	code, _, _ := strings.Cut(rest, " ")
	status, err := strconv.Atoi(code)
	if err != nil {
		return Response{}, false
	}
	header, err := reader.ReadMIMEHeader()
	if err != nil && len(header) == 0 {
		return Response{}, false
	}
	var body bytes.Buffer
	if _, err := body.ReadFrom(reader.R); err != nil {
		return Response{}, false
	}
	return Response{Status: status, Header: http.Header(header), Body: bytes.TrimSpace(body.Bytes())}, true
}

// messageOf is GitHub's `message` from an error body, or the body itself.
func messageOf(body []byte) string {
	var shaped struct {
		Message string `json:"message"`
	}
	if err := json.Unmarshal(body, &shaped); err == nil && shaped.Message != "" {
		return shaped.Message
	}
	return strings.TrimSpace(string(body))
}

// sortedKeys is M's keys in order, so a call's arguments never depend on map
// iteration.
func sortedKeys(m map[string]string) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}
