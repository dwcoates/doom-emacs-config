package harness

import (
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"testing"

	"agent-repld/internal/dlog"
)

// fakePipeDaemon is a test's stand-in for a daemon on the test log pipe.
type fakePipeDaemon struct {
	t    *testing.T
	conn net.Conn
}

// dialFakePipeDaemon connects as a daemon whose logs directory is dir and
// waits for the collector's ack, as dlog's dial does.
func dialFakePipeDaemon(t *testing.T, dir string, pid int) *fakePipeDaemon {
	t.Helper()
	p := testLogPipe(t)
	conn, err := net.Dial("unix", p.path)
	if err != nil {
		t.Fatalf("dial the collector: %v", err)
	}
	t.Cleanup(func() { _ = conn.Close() })
	d := &fakePipeDaemon{t: t, conn: conn}
	d.send(dlog.PipeHello, dir, strconv.Itoa(pid))
	ack := make([]byte, 1)
	if _, err := io.ReadFull(conn, ack); err != nil || ack[0] != pipeAck {
		t.Fatalf("hello ack = %q, %v; want the ack", ack, err)
	}
	return d
}

func (d *fakePipeDaemon) send(kind byte, key, body string) {
	d.t.Helper()
	if _, err := d.conn.Write(dlog.AppendPipeFrame(nil, kind, key, []byte(body))); err != nil {
		d.t.Fatalf("send a %q frame: %v", kind, err)
	}
}

// pipedLogsDir makes a logs directory the collector routes for the test.
func pipedLogsDir(t *testing.T) string {
	t.Helper()
	dir := filepath.Join(ShortTempDir(t), "logs")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(testLogPipe(t).route(dir))
	return dir
}

// emptyFile creates a log file as a piped daemon leaves it: present, empty.
func emptyFile(t *testing.T, path string) {
	t.Helper()
	if err := os.WriteFile(path, nil, 0o644); err != nil {
		t.Fatal(err)
	}
}

func TestPipedReadSeesEveryRecordWhoseWriteHasReturned(t *testing.T) {
	// Arrange
	dir := pipedLogsDir(t)
	path := filepath.Join(dir, "daemon.run.log")
	emptyFile(t, path)
	d := dialFakePipeDaemon(t, dir, 101)
	d.send(dlog.PipeOpen, path, "")
	var want strings.Builder

	for i := 0; i < 200; i++ {
		line := "{\"n\":" + strconv.Itoa(i) + "}\n"
		want.WriteString(line)

		// Act
		d.send(dlog.PipeRecord, path, line)
		got, piped := pipedRead(t, path, 0)

		// Assert
		if !piped || string(got) != want.String() {
			t.Fatalf("after record %d: piped %v, read %q; want every record written so far", i, piped, got)
		}
	}
}

func TestConcurrentPipedReadsEachSeeEveryRecordWrittenBeforeThem(t *testing.T) {
	// Arrange: readers drain over and over while the daemon streams, so drains
	// land in the middle of one another.
	dir := pipedLogsDir(t)
	path := filepath.Join(dir, "daemon.run.log")
	emptyFile(t, path)
	d := dialFakePipeDaemon(t, dir, 109)
	d.send(dlog.PipeOpen, path, "")
	pipe := testLogPipe(t)
	var sent atomic.Int64
	stop := make(chan struct{})
	var readers sync.WaitGroup
	failures := make(chan string, 8)

	// Act
	for r := 0; r < 8; r++ {
		readers.Add(1)
		go func() {
			defer readers.Done()
			for {
				select {
				case <-stop:
					return
				default:
				}
				before := sent.Load()
				if err := pipe.barrier(resolvedPath(dir)); err != nil {
					failures <- err.Error()
					return
				}
				body, _ := pipe.read(resolvedPath(path), 0)
				if got := int64(strings.Count(string(body), "\n")); got < before {
					failures <- fmt.Sprintf("a read saw %d records after %d had been written", got, before)
					return
				}
			}
		}()
	}
	for i := 0; i < 2000; i++ {
		d.send(dlog.PipeRecord, path, "{}\n")
		sent.Add(1)
	}
	close(stop)
	readers.Wait()

	// Assert
	close(failures)
	for f := range failures {
		t.Fatal(f)
	}
}

func TestPipedReadLeavesAFileThePipeNeverOpenedOnDisk(t *testing.T) {
	// Arrange
	dir := pipedLogsDir(t)
	path := filepath.Join(dir, "agent-repl-x-shim-1.log")
	emptyFile(t, path)
	dialFakePipeDaemon(t, dir, 102)

	// Act
	_, piped := pipedRead(t, path, 0)

	// Assert
	if piped {
		t.Fatal("a file never opened over the pipe was read from memory; want it read from disk")
	}
}

func TestPipedReadResolvesALinkToItsTarget(t *testing.T) {
	// Arrange
	dir := pipedLogsDir(t)
	target := filepath.Join(dir, "agent-repl-x-daemon-1.log")
	emptyFile(t, target)
	link := filepath.Join(t.TempDir(), "daemon.log")
	if err := os.Symlink(target, link); err != nil {
		t.Fatal(err)
	}
	d := dialFakePipeDaemon(t, dir, 103)
	d.send(dlog.PipeOpen, target, "")
	d.send(dlog.PipeRecord, target, "{}\n")

	// Act
	got, piped := pipedRead(t, link, 0)

	// Assert
	if !piped || string(got) != "{}\n" {
		t.Fatalf("read through the link = %q, piped %v; want the target's record", got, piped)
	}
}

func TestPipedReadAnswersOnlyTheBytesAfterTheOffset(t *testing.T) {
	// Arrange
	dir := pipedLogsDir(t)
	path := filepath.Join(dir, "daemon.run.log")
	emptyFile(t, path)
	d := dialFakePipeDaemon(t, dir, 104)
	d.send(dlog.PipeOpen, path, "")
	d.send(dlog.PipeRecord, path, "{\"a\":1}\n{\"b\":2}\n")

	// Act
	got, _ := pipedRead(t, path, int64(len("{\"a\":1}\n")))

	// Assert
	if string(got) != "{\"b\":2}\n" {
		t.Fatalf("read from the offset = %q, want only what follows it", got)
	}
}

func TestPipedTerminalAnswersOneDaemonsMirror(t *testing.T) {
	// Arrange
	dir := pipedLogsDir(t)
	d := dialFakePipeDaemon(t, dir, 105)
	d.send(dlog.PipeTerminal, "", "mirrored\n")

	// Act
	got, err := pipedTerminal(105, dir)

	// Assert
	if err != nil || string(got) != "mirrored\n" {
		t.Fatalf("pipedTerminal = %q, %v; want the daemon's mirror", got, err)
	}
}

func TestMaterializePipedLogsWritesTheRecordsIntoTheirFiles(t *testing.T) {
	// Arrange
	dir := pipedLogsDir(t)
	path := filepath.Join(dir, "daemon.run.log")
	emptyFile(t, path)
	d := dialFakePipeDaemon(t, dir, 106)
	d.send(dlog.PipeOpen, path, "")
	d.send(dlog.PipeRecord, path, "{}\n")

	// Act
	err := materializePipedLogs(dir)

	// Assert
	body, rerr := os.ReadFile(path)
	if err != nil || rerr != nil || string(body) != "{}\n" {
		t.Fatalf("materialize = %v; file = %q, %v; want the record on disk", err, body, rerr)
	}
}

func TestReleasingTheLastRouteForgetsTheDirectorysRecords(t *testing.T) {
	// Arrange
	p := testLogPipe(t)
	dir := filepath.Join(ShortTempDir(t), "logs")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(dir, "daemon.run.log")
	emptyFile(t, path)
	release := p.route(dir)
	d := dialFakePipeDaemon(t, dir, 107)
	d.send(dlog.PipeOpen, path, "")
	d.send(dlog.PipeRecord, path, "{}\n")
	if _, piped := pipedRead(t, path, 0); !piped {
		t.Fatal("the record never reached the collector")
	}

	// Act
	release()

	// Assert
	if _, kept := p.read(resolvedPath(path), 0); kept {
		t.Fatal("the released directory's records are still held")
	}
}

func TestAConnectionThatEndsInsideAFrameFailsTheNextDrain(t *testing.T) {
	// Arrange
	dir := pipedLogsDir(t)
	path := filepath.Join(dir, "daemon.run.log")
	emptyFile(t, path)
	d := dialFakePipeDaemon(t, dir, 108)
	frame := dlog.AppendPipeFrame(nil, dlog.PipeRecord, path, []byte("{}\n"))
	if _, err := d.conn.Write(frame[:len(frame)-1]); err != nil {
		t.Fatal(err)
	}
	_ = d.conn.Close()

	// Act
	err := testLogPipe(t).barrier(resolvedPath(dir))

	// Assert
	if err == nil {
		t.Fatal("a connection that ended inside a frame drained clean; want the truncation reported")
	}
}
