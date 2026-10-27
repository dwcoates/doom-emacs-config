// Package fakestore stands in for the record store's read-only face
// (storeread.v1.StoreRead) in the daemon's integration suite.
//
// THE REAL STORE IS ONE PROCESS EVERY SHIM WRITES AND THE DAEMON READS, and it
// outlives them all: a workspace at its cold gate runs no shim, yet its
// conversation is read from the store. The fake keeps that shape with files:
// each fake shim persists every book it keeps (fakeshim's book) under
// Dir(store socket) whenever the book changes, and the harness serves those
// files on the read-only socket beside the store socket (storesocket.ReadPath),
// paged exactly as the fake shim pages its own ReadHistory -- both use Page and
// PageThrough here.
package fakestore

import (
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"net"
	"net/http"
	"net/url"
	"os"
	"path/filepath"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	storereadv1 "agentrepl/proto/storeread/v1"
	"agentrepl/proto/storeread/v1/storereadv1connect"
)

// DefaultPageSize is the page size of a book that states none: the real
// store's.
const DefaultPageSize = 50

// MainAgentID is the agent id every fake shim's main agent carries in its
// frames; MainBook is the workspace's own book it is kept under.
const MainAgentID = "main"

// MainBook is the book the fake shim of workspace WS states, and keeps its
// main agent's history under: unique per workspace, as the real book is.
func MainBook(ws string) string {
	return MainAgentID + "@" + ws
}

// ResumeEntries files a resumed session's history, stated NEWEST FIRST as a
// shim profile states it, as the book holds it: OLDEST FIRST, each entry at a
// resume pointer and a recorded place in its order, and the turn TURNS names
// at its index (newest first too; absent or "" is an unstamped entry).
func ResumeEntries(newestFirst [][]byte, turns []string) ([]*conversationv1.HistoryEntryAt, error) {
	var out []*conversationv1.HistoryEntryAt
	for i := len(newestFirst) - 1; i >= 0; i-- {
		entry := &conversationv1.HistoryEntry{}
		if err := proto.Unmarshal(newestFirst[i], entry); err != nil {
			return nil, fmt.Errorf("fakestore: resume history entry %d does not decode: %w", i, err)
		}
		at := &conversationv1.HistoryEntryAt{
			At:    &conversationv1.HistoryPointer{Value: fmt.Sprintf("resume-%d", i)},
			Entry: entry,
			Place: &conversationv1.HistoryEntryAt_RecordedPlace{RecordedPlace: &conversationv1.ConversationPlace{
				AtMs: int64(len(newestFirst) - i),
			}},
		}
		if i < len(turns) && turns[i] != "" {
			at.Turn = &conversationv1.TurnId{Value: turns[i]}
		}
		out = append(out, at)
	}
	return out, nil
}

// Dir is where the books of the store serving STORESOCKET are kept.
func Dir(storeSocket string) string {
	return storeSocket + ".books"
}

// bookFile is one book as it is kept: its entries OLDEST FIRST, each a
// conversation.v1.HistoryEntryAt, and the page size its pages are cut at.
type bookFile struct {
	PageSize int      `json:"page_size"`
	Entries  []string `json:"entries"`
}

func path(dir, book string) string {
	return filepath.Join(dir, url.PathEscape(book)+".json")
}

// Write keeps BOOK's ENTRIES (oldest first), paged at PAGESIZE, replacing what
// was kept. The write is atomic: a reader sees the old book or the new one.
func Write(dir, book string, pageSize int, entries []*conversationv1.HistoryEntryAt) error {
	file := bookFile{PageSize: pageSize}
	for _, at := range entries {
		raw, err := proto.Marshal(at)
		if err != nil {
			return fmt.Errorf("fakestore: encode an entry of book %q: %w", book, err)
		}
		file.Entries = append(file.Entries, base64.StdEncoding.EncodeToString(raw))
	}
	body, err := json.Marshal(file)
	if err != nil {
		return fmt.Errorf("fakestore: encode book %q: %w", book, err)
	}
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return fmt.Errorf("fakestore: make the book directory: %w", err)
	}
	tmp, err := os.CreateTemp(dir, ".book-*")
	if err != nil {
		return fmt.Errorf("fakestore: write book %q: %w", book, err)
	}
	if _, err := tmp.Write(body); err != nil {
		_ = tmp.Close()
		_ = os.Remove(tmp.Name())
		return fmt.Errorf("fakestore: write book %q: %w", book, err)
	}
	if err := tmp.Close(); err != nil {
		_ = os.Remove(tmp.Name())
		return fmt.Errorf("fakestore: write book %q: %w", book, err)
	}
	if err := os.Rename(tmp.Name(), path(dir, book)); err != nil {
		return fmt.Errorf("fakestore: write book %q: %w", book, err)
	}
	return nil
}

// Read answers BOOK's kept entries (oldest first) and page size, false when no
// book of that name is kept.
func Read(dir, book string) (int, []*conversationv1.HistoryEntryAt, bool, error) {
	body, err := os.ReadFile(path(dir, book))
	if errors.Is(err, fs.ErrNotExist) {
		return 0, nil, false, nil
	}
	if err != nil {
		return 0, nil, false, fmt.Errorf("fakestore: read book %q: %w", book, err)
	}
	var file bookFile
	if err := json.Unmarshal(body, &file); err != nil {
		return 0, nil, false, fmt.Errorf("fakestore: decode book %q: %w", book, err)
	}
	entries := make([]*conversationv1.HistoryEntryAt, 0, len(file.Entries))
	for i, encoded := range file.Entries {
		raw, err := base64.StdEncoding.DecodeString(encoded)
		if err != nil {
			return 0, nil, false, fmt.Errorf("fakestore: decode entry %d of book %q: %w", i, book, err)
		}
		at := &conversationv1.HistoryEntryAt{}
		if err := proto.Unmarshal(raw, at); err != nil {
			return 0, nil, false, fmt.Errorf("fakestore: decode entry %d of book %q: %w", i, book, err)
		}
		entries = append(entries, at)
	}
	return file.PageSize, entries, true, nil
}

// Page is one page of ENTRIES (oldest first): the newest page with no AFTER,
// else the entries placed before the one AFTER names, at most SIZE of them,
// newest first. False is a pointer naming no entry.
func Page(entries []*conversationv1.HistoryEntryAt, after *conversationv1.HistoryPointer, size int) (*conversationv1.HistoryPage, bool) {
	end := len(entries)
	if after != nil {
		end = -1
		for i, at := range entries {
			if at.GetAt().GetValue() == after.GetValue() {
				end = i
			}
		}
		if end < 0 {
			return nil, false
		}
	}
	start := max(end-size, 0)
	page := &conversationv1.HistoryPage{}
	for i := end - 1; i >= start; i-- {
		page.Entries = append(page.Entries, entries[i])
	}
	if start == 0 {
		page.Boundary = &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}
	} else {
		page.Boundary = &conversationv1.HistoryPage_More{More: &conversationv1.HistoryMore{LastEntry: entries[start].GetAt()}}
	}
	return page, true
}

// PageThrough is the page of ENTRIES (oldest first) read THROUGH an instant:
// the newest entries placed at or before THROUGHMS, as the store's bounded
// read serves a fork's inherited past. An entry placed nowhere counts as
// placed at the conversation's start.
func PageThrough(entries []*conversationv1.HistoryEntryAt, throughMs int64, size int) *conversationv1.HistoryPage {
	end := len(entries)
	for end > 0 && entries[end-1].GetRecordedPlace().GetAtMs() > throughMs {
		end--
	}
	var after *conversationv1.HistoryPointer
	if end < len(entries) {
		after = entries[end].GetAt()
	}
	page, _ := Page(entries, after, size)
	return page
}

// Server answers storeread.v1.StoreRead from the books kept under its
// directory.
type Server struct {
	storereadv1connect.UnimplementedStoreReadHandler
	dir         string
	defaultSize int
}

// ReadHistoryPage serves one page of a kept book. A book nothing kept is the
// empty page when the request states it, and unknown otherwise, as the real
// store answers.
func (s *Server) ReadHistoryPage(_ context.Context, req *connect.Request[storereadv1.ReadHistoryPageRequest]) (*connect.Response[storereadv1.ReadHistoryPageResponse], error) {
	book := req.Msg.GetBook().GetValue()
	size, entries, kept, err := Read(s.dir, book)
	if err != nil {
		return failure(err.Error(), &storereadv1.ReadHistoryPageFailure_StorageFailure{StorageFailure: &storereadv1.ReadHistoryPageStorageFailure{}}), nil
	}
	if size <= 0 {
		size = s.defaultSize
	}
	switch position := req.Msg.GetPosition().(type) {
	case *storereadv1.ReadHistoryPageRequest_Newest:
		if !kept && position.Newest.GetStated() == nil {
			return failure(fmt.Sprintf("fakestore: no book %q is kept", book), &storereadv1.ReadHistoryPageFailure_UnknownAgent{UnknownAgent: &storereadv1.ReadHistoryPageUnknownAgent{}}), nil
		}
		page, _ := Page(entries, nil, size)
		return success(page), nil
	case *storereadv1.ReadHistoryPageRequest_After:
		page, ok := Page(entries, position.After, size)
		if !ok {
			return failure(fmt.Sprintf("fakestore: the pointer %q names no entry of book %q", position.After.GetValue(), book), &storereadv1.ReadHistoryPageFailure_StalePointer{StalePointer: &storereadv1.ReadHistoryPageStalePointer{}}), nil
		}
		return success(page), nil
	case *storereadv1.ReadHistoryPageRequest_Through:
		if !kept {
			return failure(fmt.Sprintf("fakestore: no book %q is kept", book), &storereadv1.ReadHistoryPageFailure_UnknownAgent{UnknownAgent: &storereadv1.ReadHistoryPageUnknownAgent{}}), nil
		}
		return success(PageThrough(entries, position.Through.GetAtMs(), size)), nil
	default:
		return failure("fakestore: the request names no position", &storereadv1.ReadHistoryPageFailure_InvalidRequest{InvalidRequest: &storereadv1.ReadHistoryPageInvalidRequest{Field: "position"}}), nil
	}
}

func success(page *conversationv1.HistoryPage) *connect.Response[storereadv1.ReadHistoryPageResponse] {
	return connect.NewResponse(&storereadv1.ReadHistoryPageResponse{Result: &storereadv1.ReadHistoryPageResponse_Success{Success: &storereadv1.ReadHistoryPageSuccess{Page: page}}})
}

func failure(detail string, kind any) *connect.Response[storereadv1.ReadHistoryPageResponse] {
	f := &storereadv1.ReadHistoryPageFailure{Detail: detail}
	switch k := kind.(type) {
	case *storereadv1.ReadHistoryPageFailure_UnknownAgent:
		f.Kind = k
	case *storereadv1.ReadHistoryPageFailure_StalePointer:
		f.Kind = k
	case *storereadv1.ReadHistoryPageFailure_StorageFailure:
		f.Kind = k
	case *storereadv1.ReadHistoryPageFailure_InvalidRequest:
		f.Kind = k
	default:
		panic(fmt.Sprintf("fakestore: a failure arm this fake does not name: %T", kind))
	}
	return connect.NewResponse(&storereadv1.ReadHistoryPageResponse{Result: &storereadv1.ReadHistoryPageResponse_Failure{Failure: f}})
}

// Serve binds the read-only face on SOCKET, answering from the books under
// DIR paged at DEFAULTSIZE when a book states none, and answers its stop,
// which reports how serving ended: nil for the stop itself.
func Serve(socket, dir string, defaultSize int) (func() error, error) {
	ln, err := net.Listen("unix", socket)
	if err != nil {
		return nil, fmt.Errorf("fakestore: listen on %s: %w", socket, err)
	}
	mux := http.NewServeMux()
	mux.Handle(storereadv1connect.NewStoreReadHandler(&Server{dir: dir, defaultSize: defaultSize}))
	srv := &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{})}
	served := make(chan error, 1)
	go func() { served <- srv.Serve(ln) }()
	return func() error {
		closeErr := srv.Close()
		serveErr := <-served
		if errors.Is(serveErr, http.ErrServerClosed) {
			serveErr = nil
		}
		return errors.Join(closeErr, serveErr)
	}, nil
}
