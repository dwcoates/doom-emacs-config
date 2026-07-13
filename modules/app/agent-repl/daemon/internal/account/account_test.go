package account

import (
	"os"
	"path/filepath"
	"testing"
)

// writeIdentity drops a .claude.json carrying EMAIL into dir. An empty
// email writes a document with no oauthAccount block at all, which is what
// a config root looks like before its first login.
func writeIdentity(t *testing.T, dir, email string) {
	t.Helper()
	doc := `{"hasCompletedOnboarding":true}`
	if email != "" {
		doc = `{"hasCompletedOnboarding":true,"oauthAccount":{"emailAddress":"` + email + `"}}`
	}
	if err := os.WriteFile(filepath.Join(dir, identityFile), []byte(doc), 0o600); err != nil {
		t.Fatalf("write identity: %v", err)
	}
}

func TestPath_ConfigDirHoldsItsOwnIdentity(t *testing.T) {
	// Arrange / Act
	got, err := Path("/somewhere/.claude-chesscom")

	// Assert
	if err != nil {
		t.Fatalf("Path: %v", err)
	}
	if want := "/somewhere/.claude-chesscom/.claude.json"; got != want {
		t.Errorf("Path: got %q, want %q", got, want)
	}
}

func TestPath_DefaultRootIdentityIsHomesSibling(t *testing.T) {
	// Arrange: the default account's identity is $HOME/.claude.json, NOT
	// $HOME/.claude/.claude.json — the latter does not exist on a real
	// install, and looking there would report every default session as
	// logged out.
	home := t.TempDir()
	t.Setenv("HOME", home)

	// Act
	got, err := Path("")

	// Assert
	if err != nil {
		t.Fatalf("Path: %v", err)
	}
	if want := filepath.Join(home, ".claude.json"); got != want {
		t.Errorf("Path: got %q, want %q", got, want)
	}
}

func TestRead_ReportsTheLoggedInEmail(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	writeIdentity(t, dir, "dodge@chess.com")

	// Act
	id, err := Read(dir)

	// Assert
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if id.Email != "dodge@chess.com" {
		t.Errorf("Email: got %q, want %q", id.Email, "dodge@chess.com")
	}
	if id.ConfigDir != dir {
		t.Errorf("ConfigDir: got %q, want %q", id.ConfigDir, dir)
	}
	if !id.LoggedIn() {
		t.Error("LoggedIn: got false, want true")
	}
}

func TestRead_TwoRootsReportTwoAccounts(t *testing.T) {
	// Arrange: the whole point of the config-dir split is that two roots
	// hold two different logins at the same time.
	personal, work := t.TempDir(), t.TempDir()
	writeIdentity(t, personal, "dodge.w.coates@gmail.com")
	writeIdentity(t, work, "dodge@chess.com")

	// Act
	gotPersonal, err := Read(personal)
	if err != nil {
		t.Fatalf("Read personal: %v", err)
	}
	gotWork, err := Read(work)
	if err != nil {
		t.Fatalf("Read work: %v", err)
	}

	// Assert
	if gotPersonal.Email != "dodge.w.coates@gmail.com" {
		t.Errorf("personal: got %q", gotPersonal.Email)
	}
	if gotWork.Email != "dodge@chess.com" {
		t.Errorf("work: got %q", gotWork.Email)
	}
}

func TestRead_MissingIdentityIsLoggedOutNotAnError(t *testing.T) {
	// Arrange: a config root that has never been logged into has no
	// identity file at all. That is a state the topbar renders, not a
	// failure to report.
	dir := t.TempDir()

	// Act
	id, err := Read(dir)

	// Assert
	if err != nil {
		t.Fatalf("Read: got error %v, want nil for a never-logged-in root", err)
	}
	if id.LoggedIn() {
		t.Errorf("LoggedIn: got true, want false (Email=%q)", id.Email)
	}
}

func TestRead_IdentityWithoutAnAccountBlockIsLoggedOut(t *testing.T) {
	// Arrange: onboarding writes .claude.json before any login happens, so
	// the file can exist with no oauthAccount in it.
	dir := t.TempDir()
	writeIdentity(t, dir, "")

	// Act
	id, err := Read(dir)

	// Assert
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if id.LoggedIn() {
		t.Errorf("LoggedIn: got true, want false (Email=%q)", id.Email)
	}
}

func TestRead_CorruptIdentitySurfacesAsAnError(t *testing.T) {
	// Arrange: a file that exists but will not parse is a broken install.
	// Reporting it as "logged out" would send the user to re-login, which
	// is the wrong fix.
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, identityFile), []byte("{not json"), 0o600); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act
	_, err := Read(dir)

	// Assert
	if err == nil {
		t.Error("Read: got nil error, want a parse failure")
	}
}

func TestRead_IgnoresTheRestOfTheDocument(t *testing.T) {
	// Arrange: .claude.json is a large evolving document. Naming more of
	// it than the account block would break on every CLI release.
	dir := t.TempDir()
	doc := `{"numStartups":42,"tipsHistory":{"x":1},"projects":{"/a":{"allowedTools":[]}},
	         "oauthAccount":{"accountUuid":"u","emailAddress":"dodge@chess.com","organizationName":"n"}}`
	if err := os.WriteFile(filepath.Join(dir, identityFile), []byte(doc), 0o600); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act
	id, err := Read(dir)

	// Assert
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if id.Email != "dodge@chess.com" {
		t.Errorf("Email: got %q, want %q", id.Email, "dodge@chess.com")
	}
}
