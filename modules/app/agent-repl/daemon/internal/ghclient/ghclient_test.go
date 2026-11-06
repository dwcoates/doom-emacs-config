package ghclient

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// GITHUB IS NEVER CALLED: every run is a fake `gh` that writes a scripted
// answer and records the arguments it was given.

// fakeGH writes a `gh` that prints STDOUT, writes STDERR, records its
// arguments one per line and its stdin beside them (stdinOf), and exits
// CODE. It answers the binary and where the
// arguments are recorded.
func fakeGH(t *testing.T, stdout, stderr string, code int) (string, string) {
	t.Helper()
	dir := t.TempDir()
	out, errOut, argsFile := filepath.Join(dir, "out"), filepath.Join(dir, "err"), filepath.Join(dir, "args")
	for path, text := range map[string]string{out: stdout, errOut: stderr} {
		if err := os.WriteFile(path, []byte(text), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	script := "#!/bin/sh\nfor a in \"$@\"; do printf '%s\\n' \"$a\" >> '" + argsFile + "'; done\ncat > '" + filepath.Join(dir, "stdin") + "'\ncat '" + out + "'\ncat '" + errOut + "' >&2\nexit " + strconv.Itoa(code) + "\n"
	bin := filepath.Join(dir, "gh")
	if err := os.WriteFile(bin, []byte(script), 0o700); err != nil {
		t.Fatal(err)
	}
	return bin, argsFile
}

// answer is `gh api --include` output for STATUS with HEADERS and BODY.
func answer(status string, headers []string, body string) string {
	return "HTTP/2.0 " + status + "\r\n" + strings.Join(headers, "\r\n") + "\r\n\r\n" + body
}

func newClient(t *testing.T, bin string) (Client, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	c, err := New(Config{Binary: bin, Log: log})
	if err != nil {
		t.Fatal(err)
	}
	return c, log
}

func TestGetAnswersWhatGitHubSent(t *testing.T) {
	tests := []struct {
		name         string
		out          string
		code         int
		wantStatus   int
		wantBody     string
		wantETag     string
		wantModified bool
	}{
		{name: "a 200 carries its body and entity tag", out: answer("200 OK", []string{`ETag: W/"a"`}, `[{"id":1}]`),
			wantStatus: 200, wantBody: `[{"id":1}]`, wantETag: `W/"a"`, wantModified: true},
		{name: "a 304 is an unchanged answer though gh exits 1", out: answer("304 Not Modified", []string{`ETag: W/"a"`}, ""), code: 1,
			wantStatus: 304, wantETag: `W/"a"`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin, _ := fakeGH(t, tt.out, "", tt.code)
			c, _ := newClient(t, bin)

			// Act
			resp, err := c.Get(context.Background(), "repos/o/r/pulls/1", "")

			// Assert
			if err != nil || resp.Status != tt.wantStatus || string(resp.Body) != tt.wantBody || resp.ETag() != tt.wantETag || resp.NotModified() == tt.wantModified {
				t.Fatalf("Get = %+v, %v; want status %d body %q etag %q", resp, err, tt.wantStatus, tt.wantBody, tt.wantETag)
			}
		})
	}
}

func TestGetSendsItsEntityTag(t *testing.T) {
	// Arrange
	bin, argsFile := fakeGH(t, answer("304 Not Modified", nil, ""), "", 1)
	c, _ := newClient(t, bin)

	// Act
	_, err := c.Get(context.Background(), "repos/o/r/pulls/1/reviews?per_page=100", `W/"tag"`)

	// Assert
	args, _ := os.ReadFile(argsFile)
	want := "api\n--include\n--method\nGET\nrepos/o/r/pulls/1/reviews?per_page=100\n--header\nIf-None-Match: W/\"tag\"\n"
	if err != nil || string(args) != want {
		t.Fatalf("Get = %v ran %q, want %q", err, args, want)
	}
}

func TestPostSendsItsFieldsInOrder(t *testing.T) {
	// Arrange
	bin, argsFile := fakeGH(t, answer("201 Created", nil, `{"id":7}`), "", 0)
	c, _ := newClient(t, bin)

	// Act
	resp, err := c.Post(context.Background(), "repos/o/r/issues/1/comments", map[string]string{"z": "last", "body": "line one\nline two"})

	// Assert
	args, _ := os.ReadFile(argsFile)
	want := "api\n--include\n--method\nPOST\nrepos/o/r/issues/1/comments\n--raw-field\nbody=line one\nline two\n--raw-field\nz=last\n"
	if err != nil || resp.Status != 201 || string(args) != want {
		t.Fatalf("Post = %+v, %v ran %q, want %q", resp, err, args, want)
	}
}

func TestACallThatFailsIsTypedAndRecorded(t *testing.T) {
	reset := time.Unix(1_800_000_000, 0).UTC()
	tests := []struct {
		name    string
		out     string
		stderr  string
		want    Failure
		message string
	}{
		{name: "a 401 is unauthorized", out: answer("401 Unauthorized", nil, `{"message":"Bad credentials"}`),
			want: Failure{Kind: KindUnauthorized, Status: 401, Message: "Bad credentials"}, message: "GitHub refused the call"},
		{name: "a 404 is not found", out: answer("404 Not Found", nil, `{"message":"Not Found"}`),
			want: Failure{Kind: KindNotFound, Status: 404, Message: "Not Found"}, message: "GitHub refused the call"},
		{name: "a 403 with the limit spent is rate limited until its reset", out: answer("403 Forbidden", []string{"X-RateLimit-Remaining: 0", "X-RateLimit-Reset: 1800000000"}, `{"message":"API rate limit exceeded"}`),
			want: Failure{Kind: KindRateLimited, Status: 403, Message: "API rate limit exceeded", ResetsAt: reset}, message: "GitHub refused the call"},
		{name: "a 403 with limit left is a refusal", out: answer("403 Forbidden", []string{"X-RateLimit-Remaining: 12"}, `{"message":"Resource not accessible"}`),
			want: Failure{Kind: KindRefused, Status: 403, Message: "Resource not accessible"}, message: "GitHub refused the call"},
		{name: "a 422 is a refusal", out: answer("422 Unprocessable Entity", nil, `{"message":"Validation Failed"}`),
			want: Failure{Kind: KindRefused, Status: 422, Message: "Validation Failed"}, message: "GitHub refused the call"},
		{name: "a 502 is unreachable", out: answer("502 Bad Gateway", nil, "upstream"),
			want: Failure{Kind: KindUnreachable, Status: 502, Message: "upstream"}, message: "GitHub refused the call"},
		{name: "no status line is unreachable with what gh said", stderr: "error connecting to api.github.com\n",
			want: Failure{Kind: KindUnreachable, Message: "error connecting to api.github.com"}, message: "GitHub could not be reached"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin, _ := fakeGH(t, tt.out, tt.stderr, 1)
			c, log := newClient(t, bin)
			tt.want.Method, tt.want.Path = "GET", "repos/o/r/pulls/1"

			// Act
			_, err := c.Get(context.Background(), "repos/o/r/pulls/1", "")

			// Assert
			var f *Failure
			if !errors.As(err, &f) || !reflect.DeepEqual(*f, tt.want) {
				t.Fatalf("Get = %v, want %+v", err, tt.want)
			}
			record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.ghclient.get", Message: tt.message})
			if !ok || record.Context["path"] != "repos/o/r/pulls/1" || record.Context["kind"] != string(tt.want.Kind) {
				t.Fatalf("no error record naming the path and kind: %+v", log.Records())
			}
		})
	}
}

func TestACallTheDaemonStoppedIsNoFailure(t *testing.T) {
	// Arrange
	bin, _ := fakeGH(t, "", "", 1)
	c, log := newClient(t, bin)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, err := c.Get(ctx, "repos/o/r/pulls/1", "")

	// Assert
	if !errors.Is(err, context.Canceled) || len(log.Records()) != 0 {
		t.Fatalf("Get = %v with records %+v, want the cancellation and nothing recorded", err, log.Records())
	}
}

// stdinOf is what the fake `gh` whose arguments ARGSFILE records read.
func stdinOf(t *testing.T, argsFile string) string {
	t.Helper()
	b, err := os.ReadFile(filepath.Join(filepath.Dir(argsFile), "stdin"))
	if err != nil {
		t.Fatal(err)
	}
	return string(b)
}

func TestGraphQLPostsItsQueryAndVariablesOnStdin(t *testing.T) {
	// Arrange
	bin, argsFile := fakeGH(t, answer("200 OK", nil, `{"data":{"viewer":{"login":"u"}}}`), "", 0)
	c, _ := newClient(t, bin)

	// Act
	_, err := c.GraphQL(context.Background(), "query($n:Int!){viewer{login}}", map[string]any{"n": 7, "owner": "o"})

	// Assert
	args, _ := os.ReadFile(argsFile)
	wantArgs := "api\n--include\n--method\nPOST\ngraphql\n--input\n-\n"
	wantBody := `{"query":"query($n:Int!){viewer{login}}","variables":{"n":7,"owner":"o"}}`
	if err != nil || string(args) != wantArgs || stdinOf(t, argsFile) != wantBody {
		t.Fatalf("GraphQL = %v ran %q reading %q, want %q reading %q", err, args, stdinOf(t, argsFile), wantArgs, wantBody)
	}
}

func TestGraphQLAnswersItsDataAndEveryErrorBesideIt(t *testing.T) {
	tests := []struct {
		name       string
		body       string
		wantData   string
		wantErrors []GraphQLError
	}{
		{name: "a whole answer carries no errors", body: `{"data":{"a":1}}`, wantData: `{"a":1}`},
		{name: "a partial answer carries each error with its path", body: `{"data":{"repository":{"pr1":null,"pr2":{"n":2}}},"errors":[{"type":"NOT_FOUND","message":"gone","path":["repository","pr1"]}]}`,
			wantData: `{"repository":{"pr1":null,"pr2":{"n":2}}}`, wantErrors: []GraphQLError{{Type: "NOT_FOUND", Message: "gone", Path: []string{"repository", "pr1"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin, _ := fakeGH(t, answer("200 OK", nil, tt.body), "", 0)
			c, log := newClient(t, bin)

			// Act
			got, err := c.GraphQL(context.Background(), "{a}", nil)

			// Assert
			if err != nil || string(got.Data) != tt.wantData || !reflect.DeepEqual(got.Errors, tt.wantErrors) {
				t.Fatalf("GraphQL = %+v, %v, want data %s errors %+v", got, err, tt.wantData, tt.wantErrors)
			}
			for _, e := range tt.wantErrors {
				record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.ghclient.graphql", Message: "GitHub nulled a field of the GraphQL answer"})
				if !ok || record.Context["type"] != e.Type || record.Context["field"] != strings.Join(e.Path, ".") {
					t.Fatalf("no error record naming the nulled field: %+v", log.Records())
				}
			}
		})
	}
}

func TestAGraphQLCallThatFailsIsTypedAndRecorded(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    Failure
		message string
	}{
		{name: "no data and a NOT_FOUND error is not found", out: answer("200 OK", nil, `{"data":null,"errors":[{"type":"NOT_FOUND","message":"Could not resolve"}]}`),
			want: Failure{Kind: KindNotFound, Status: 200, Message: "Could not resolve"}, message: "GitHub refused the GraphQL call"},
		{name: "no data and a RATE_LIMITED error is rate limited", out: answer("200 OK", nil, `{"errors":[{"type":"RATE_LIMITED","message":"API rate limit exceeded"}]}`),
			want: Failure{Kind: KindRateLimited, Status: 200, Message: "API rate limit exceeded"}, message: "GitHub refused the GraphQL call"},
		{name: "no data and an error of another type is a refusal", out: answer("200 OK", nil, `{"errors":[{"message":"Parse error"}]}`),
			want: Failure{Kind: KindRefused, Status: 200, Message: "Parse error"}, message: "GitHub refused the GraphQL call"},
		{name: "no data and no error is a refusal", out: answer("200 OK", nil, `{}`),
			want: Failure{Kind: KindRefused, Status: 200, Message: "GitHub's GraphQL answer carries no data"}, message: "GitHub refused the GraphQL call"},
		{name: "a body that is not JSON is unreachable", out: answer("200 OK", nil, `<html>`),
			want: Failure{Kind: KindUnreachable, Status: 200, Message: "GitHub's GraphQL answer is not JSON: invalid character '<' looking for beginning of value"}, message: "GitHub's GraphQL answer cannot be read"},
		{name: "a 401 is unauthorized", out: answer("401 Unauthorized", nil, `{"message":"Bad credentials"}`),
			want: Failure{Kind: KindUnauthorized, Status: 401, Message: "Bad credentials"}, message: "GitHub refused the call"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin, _ := fakeGH(t, tt.out, "", 0)
			c, log := newClient(t, bin)
			tt.want.Method, tt.want.Path = "POST", "graphql"

			// Act
			_, err := c.GraphQL(context.Background(), "{a}", nil)

			// Assert
			var f *Failure
			if !errors.As(err, &f) || !reflect.DeepEqual(*f, tt.want) {
				t.Fatalf("GraphQL = %v, want %+v", err, tt.want)
			}
			record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.ghclient.graphql", Message: tt.message})
			if !ok || record.Context["path"] != "graphql" || record.Context["kind"] != string(tt.want.Kind) {
				t.Fatalf("no error record naming the path and kind: %+v", log.Records())
			}
		})
	}
}
