package imageorigin

import (
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
)

// pngBytes is a one-pixel PNG, so a served body is a real image rather than a
// string that happens to be bytes.
var pngBytes = []byte{
	0x89, 'P', 'N', 'G', 0x0d, 0x0a, 0x1a, 0x0a,
	0x00, 0x00, 0x00, 0x0d, 'I', 'H', 'D', 'R',
	0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
	0x08, 0x06, 0x00, 0x00, 0x00, 0x1f, 0x15, 0xc4,
	0x89,
}

// newOrigin builds an origin over a capturing logger.
func newOrigin(t *testing.T) (*Origin, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	origin, err := New(log)
	if err != nil {
		t.Fatalf("build the origin: %v", err)
	}
	return origin, log
}

// writeImage writes the sample PNG into a fresh temp dir and answers its path.
func writeImage(t *testing.T) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "clip.png")
	if err := os.WriteFile(path, pngBytes, 0o644); err != nil {
		t.Fatalf("write the sample image: %v", err)
	}
	return path
}

// TestNewRefusesWithoutALogger covers the one construction refusal: an origin
// with no logger could not record the failures it is required to record.
func TestNewRefusesWithoutALogger(t *testing.T) {
	// Arrange, Act.
	origin, err := New(nil)

	// Assert.
	if err == nil {
		t.Fatalf("New(nil) built an origin %v, want a refusal", origin)
	}
}

// TestRegisterAnswersASourceBeneathTheRoute covers the happy path: the source
// is root-relative and beneath the route, so the webapp loads it from the same
// origin it was itself served from.
func TestRegisterAnswersASourceBeneathTheRoute(t *testing.T) {
	// Arrange.
	origin, _ := newOrigin(t)

	// Act.
	src, err := origin.Register("/tmp/a/clip.png", "image/png")

	// Assert.
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	if !strings.HasPrefix(src, Route) {
		t.Errorf("the source is %q, want it beneath %q", src, Route)
	}
}

// TestRegisterIsIdempotentForOnePath covers the redraw: a feed redraws the
// same record repeatedly, and the registry must not grow an entry per redraw.
func TestRegisterIsIdempotentForOnePath(t *testing.T) {
	// Arrange.
	origin, _ := newOrigin(t)
	first, err := origin.Register("/tmp/a/clip.png", "image/png")
	if err != nil {
		t.Fatalf("the first Register: %v", err)
	}

	// Act.
	second, err := origin.Register("/tmp/a/clip.png", "image/png")

	// Assert.
	if err != nil {
		t.Fatalf("the second Register: %v", err)
	}
	if first != second {
		t.Errorf("registering one path twice gave %q then %q, want one source", first, second)
	}
	if got := len(origin.entries); got != 1 {
		t.Errorf("the registry holds %d entries, want 1", got)
	}
}

// TestRegisterDistinguishesTwoPaths covers the opposite of idempotence: two
// attachments must not collide onto one id.
func TestRegisterDistinguishesTwoPaths(t *testing.T) {
	// Arrange.
	origin, _ := newOrigin(t)
	first, err := origin.Register("/tmp/a/clip.png", "image/png")
	if err != nil {
		t.Fatalf("the first Register: %v", err)
	}

	// Act.
	second, err := origin.Register("/tmp/a/other.png", "image/png")

	// Assert.
	if err != nil {
		t.Fatalf("the second Register: %v", err)
	}
	if first == second {
		t.Errorf("two paths both registered as %q, want two sources", first)
	}
}

// TestRegisterRefusesAnUnusablePath covers each reference the origin cannot
// make servable. One case per refusal.
func TestRegisterRefusesAnUnusablePath(t *testing.T) {
	tests := []struct {
		name      string
		path      string
		mediaType string
		want      string
	}{
		{name: "no path at all", path: "", mediaType: "image/png", want: "carries no path"},
		{name: "a relative path", path: "a/clip.png", mediaType: "image/png", want: "is not absolute"},
		{name: "no media type", path: "/tmp/a/clip.png", mediaType: "", want: "states no media type"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			origin, _ := newOrigin(t)

			// Act.
			src, err := origin.Register(test.path, test.mediaType)

			// Assert.
			if err == nil {
				t.Fatalf("Register answered %q, want a refusal", src)
			}
			if !strings.Contains(err.Error(), test.want) {
				t.Errorf("the refusal is %q, want it to name %q", err, test.want)
			}
			if got := len(origin.entries); got != 0 {
				t.Errorf("a refused reference left %d entries in the registry, want 0", got)
			}
		})
	}
}

// TestHandlerServesARegisteredImage covers the read: the bytes are the file's
// and the content type is the RECORD's rather than a sniffed one.
func TestHandlerServesARegisteredImage(t *testing.T) {
	// Arrange.
	origin, _ := newOrigin(t)
	path := writeImage(t)
	src, err := origin.Register(path, "image/png")
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	recorder := httptest.NewRecorder()

	// Act.
	origin.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, src, nil))

	// Assert.
	if recorder.Code != http.StatusOK {
		t.Fatalf("the origin answered %d, want 200", recorder.Code)
	}
	if got := recorder.Header().Get("Content-Type"); got != "image/png" {
		t.Errorf("the content type is %q, want the record's %q", got, "image/png")
	}
	if got := recorder.Body.Bytes(); string(got) != string(pngBytes) {
		t.Errorf("the origin served %d bytes, want the file's %d", len(got), len(pngBytes))
	}
}

// TestHandlerRefusesAnUnregisteredID is the arbitrary-file-read refusal: a
// path nobody's conversation carried has no id, so nothing can ask for it.
func TestHandlerRefusesAnUnregisteredID(t *testing.T) {
	// Arrange.
	origin, _ := newOrigin(t)
	recorder := httptest.NewRecorder()

	// Act.
	origin.Handler().ServeHTTP(recorder,
		httptest.NewRequest(http.MethodGet, Route+"0000000000000000000000000000000000000000000000000000000000000000", nil))

	// Assert.
	if recorder.Code != http.StatusNotFound {
		t.Fatalf("the origin answered %d for an unregistered id, want 404", recorder.Code)
	}
}

// TestHandlerReportsARegisteredImageThatVanished covers the file deleted after
// it was drawn: the refusal is recorded at error level, never a blank 200.
func TestHandlerReportsARegisteredImageThatVanished(t *testing.T) {
	// Arrange.
	origin, log := newOrigin(t)
	path := writeImage(t)
	src, err := origin.Register(path, "image/png")
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	if err := os.Remove(path); err != nil {
		t.Fatalf("remove the sample image: %v", err)
	}
	recorder := httptest.NewRecorder()

	// Act.
	origin.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, src, nil))

	// Assert.
	if recorder.Code != http.StatusNotFound {
		t.Fatalf("the origin answered %d for a vanished file, want 404", recorder.Code)
	}
	if !recordedAtLevel(log, "error", "could not be opened") {
		t.Errorf("the vanished file was not recorded at error level; records: %v", log.Records())
	}
}

// TestHandlerRefusesAWriteMethod covers the origin's read-only shape.
func TestHandlerRefusesAWriteMethod(t *testing.T) {
	// Arrange.
	origin, _ := newOrigin(t)
	path := writeImage(t)
	src, err := origin.Register(path, "image/png")
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	recorder := httptest.NewRecorder()

	// Act.
	origin.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodPost, src, nil))

	// Assert.
	if recorder.Code != http.StatusMethodNotAllowed {
		t.Fatalf("the origin answered %d to a POST, want 405", recorder.Code)
	}
}

// recordedAtLevel reports whether the logger captured a record at LEVEL whose
// message carries FRAGMENT.
func recordedAtLevel(log *dlog.TestLogger, level, fragment string) bool {
	for _, record := range log.Records() {
		if record.Level == level && strings.Contains(record.Message, fragment) {
			return true
		}
	}
	return false
}
