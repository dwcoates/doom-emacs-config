/**
 * local-failure — the webapp's OWN classifier, covering the one category it is
 * allowed to classify (the daemon's unreachability) and the namespace
 * partition that keeps it from classifying anything else. One edge per test.
 */
import { describe, expect, it } from "vitest";
import {
  CLIENT_FAILURE_TYPES,
  CLIENT_PREFIX,
  clientFailure,
  controlPlaneFailure,
  daemonReachableFailure,
  daemonUnreachableFailure,
  isClientType,
  sessionGoneFailure,
} from "../src/local-failure.js";

describe("the namespace partition", () => {
  it.each(CLIENT_FAILURE_TYPES)("gives %s the reserved client prefix", (type) => {
    // Arrange / Act — a bare type from this side would mean the frontend
    // re-classified something the daemon already decided.
    // Assert
    expect(isClientType(type)).toBe(true);
  });

  it("rejects a daemon type as one of its own", () => {
    // Arrange / Act — the other direction: a `client.` type on the wire would
    // mean a frontend's failure was laundered through the daemon.
    // Assert
    expect(isClientType("shim.rejected")).toBe(false);
  });

  it("names the prefix the daemon reserves for it", () => {
    // Arrange / Act / Assert
    expect(CLIENT_PREFIX).toBe("client.");
  });
});

describe("locally-classified failures", () => {
  it("are always INTERNAL class", () => {
    // Arrange / Act — a transport fault implicates nothing about the account,
    // so an API-class local failure would be this end guessing at something
    // only the daemon can see.
    const f = clientFailure("client.daemon_unreachable", "gone");
    // Assert
    expect(f.errorClass).toBe("INTERNAL");
  });

  it("derive their uuid from the type, so a repeat reconciles in place", () => {
    // Arrange / Act — a reconnect loop appending a card per attempt would
    // bury the feed under its own alarm.
    const first = clientFailure("client.daemon_unreachable", "gone");
    const second = clientFailure("client.daemon_unreachable", "gone again");
    // Assert
    expect(first.uuid).toBe(second.uuid);
  });

  it("give two DIFFERENT conditions different uuids", () => {
    // Arrange / Act
    const a = clientFailure("client.daemon_unreachable", "x");
    const b = clientFailure("client.session_gone", "y");
    // Assert
    expect(a.uuid).not.toBe(b.uuid);
  });
});

describe("daemonUnreachableFailure", () => {
  it("reads the close code, which the handler used to discard", () => {
    // Arrange / Act — the code is the only evidence separating a daemon
    // restart from a network drop.
    const f = daemonUnreachableFailure(1006, "");
    // Assert
    expect(f.sourceDetail).toContain("close=1006");
  });

  it("carries the close reason when the server gave one", () => {
    // Arrange / Act
    const f = daemonUnreachableFailure(1001, "server shutting down");
    // Assert
    expect(f.sourceDetail).toContain("server shutting down");
  });

  it("says the daemon closed the connection on a clean shutdown code", () => {
    // Arrange / Act
    const f = daemonUnreachableFailure(1001, "");
    // Assert
    expect(f.message).toContain("the daemon closed the connection");
  });

  it("says the connection was lost on an abnormal close", () => {
    // Arrange / Act — 1006 is the browser's synthesized "no close frame",
    // which is a network drop rather than a daemon that said goodbye.
    const f = daemonUnreachableFailure(1006, "");
    // Assert
    expect(f.message).toContain("lost the connection");
  });

  it("opens the window unresolved", () => {
    // Arrange / Act
    const f = daemonUnreachableFailure(1006, "");
    // Assert
    expect(f.resolvedAtMs).toBe(0);
  });
});

describe("daemonReachableFailure", () => {
  it("closes the SAME card the unreachable edge opened", () => {
    // Arrange / Act — one card that settles, not two that accumulate.
    const opened = daemonUnreachableFailure(1006, "");
    const closed = daemonReachableFailure(1700000000000);
    // Assert
    expect(closed.uuid).toBe(opened.uuid);
  });

  it("stamps the resolution", () => {
    // Arrange / Act
    const f = daemonReachableFailure(1700000000000);
    // Assert
    expect(f.resolvedAtMs).toBe(1700000000000);
  });
});

describe("controlPlaneFailure", () => {
  it("names the action in the user's terms, not the endpoint's", () => {
    // Arrange / Act — "POST /accounts/switch failed" explains nothing to the
    // person who clicked a menu item.
    const f = controlPlaneFailure("the account switch", new Error("503"));
    // Assert
    expect(f.message).toBe("the account switch failed");
  });

  it("carries a thrown Error's message as the cause", () => {
    // Arrange / Act — the raw cause is what the transient notice line has no
    // room for, and it used to reach `clog` and stop there.
    const f = controlPlaneFailure("the login request", new Error("connection refused"));
    // Assert
    expect(f.sourceDetail).toBe("connection refused");
  });

  it("stringifies a non-Error throw rather than dropping it", () => {
    // Arrange / Act — a rejected fetch is not obliged to throw an Error, and
    // an empty detail would lose the only evidence there is.
    const f = controlPlaneFailure("the remediation request", "gateway timeout");
    // Assert
    expect(f.sourceDetail).toBe("gateway timeout");
  });

  it("gives two different actions two different cards", () => {
    // Arrange / Act — keying every control-plane failure alike would let a
    // failed login overwrite a failed remediation.
    const login = controlPlaneFailure("the login request", new Error("x"));
    const account = controlPlaneFailure("the account switch", new Error("x"));
    // Assert
    expect(login.uuid).not.toBe(account.uuid);
  });

  it("reconciles a retried action onto its own card", () => {
    // Arrange / Act — pressing the same button twice is one condition.
    const first = controlPlaneFailure("the account switch", new Error("503"));
    const second = controlPlaneFailure("the account switch", new Error("504"));
    // Assert
    expect(first.uuid).toBe(second.uuid);
  });

  it("classifies in the frontend's own namespace", () => {
    // Arrange / Act — the daemon never sees these calls fail, so the type
    // must be one the daemon can never emit.
    const f = controlPlaneFailure("the login request", new Error("x"));
    // Assert
    expect(f.errorType).toBe("client.control_plane_failed");
  });

  it("opens unresolved, because nothing retries it on the user's behalf", () => {
    // Arrange / Act
    const f = controlPlaneFailure("the login request", new Error("x"));
    // Assert
    expect(f.resolvedAtMs).toBe(0);
  });
});

describe("sessionGoneFailure", () => {
  it("names the session that vanished", () => {
    // Arrange / Act
    const f = sessionGoneFailure("s_abc");
    // Assert
    expect(f.sourceDetail).toContain("s_abc");
  });

  it("never resolves, because no amount of waiting brings it back", () => {
    // Arrange / Act
    const f = sessionGoneFailure("s_abc");
    // Assert
    expect(f.resolvedAtMs).toBe(0);
  });
});
