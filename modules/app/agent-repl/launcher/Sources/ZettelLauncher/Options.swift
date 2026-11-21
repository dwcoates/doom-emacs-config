import Foundation

/// What one run of the app does.
public enum Mode: Equatable {
    /// Run as the launcher: resident, post the daemons' banners until
    /// stopped; opened by hand, open Emacs and hand over to the resident one.
    case run
    /// Report whether macOS lets Zettel notify, and exit: 0 when it may,
    /// 1 when it may not (bin/requirements.sh's notification_permission
    /// check).
    case checkNotifications
    /// Ask macOS for permission to notify (its prompt, the first time) and
    /// exit as checkNotifications does.
    case requestNotifications
}

/// The launcher's command line. launchd's app.zettel.launcher passes
/// `--resident`; an open by hand (Spotlight, Finder, the Dock) or by a
/// banner's click passes nothing.
public struct Options: Equatable {
    /// The agent-repl state root whose `daemon.addr` names the daemon.
    public var stateDir: URL
    /// The launcher's JSONL log.
    public var logPath: URL
    /// The control socket of the socket holder that holds the socket the
    /// launcher accepts banners on (app.zettel.sockethold.launcher).
    public var holder: String
    /// The run directory: the one-instance lock and the build report live
    /// here, beside every service's ($AGENT_REPL_LOCK_DIR, else
    /// ~/.cache/agent-repl/run).
    public var runDir: URL
    /// Started by launchd to run in the background. An instance started any
    /// other way was opened by the user, so it brings Emacs forward.
    public var resident: Bool
    /// What this run does.
    public var mode: Mode

    public init(stateDir: URL, logPath: URL, holder: String, runDir: URL, resident: Bool, mode: Mode = .run) {
        self.stateDir = stateDir
        self.logPath = logPath
        self.holder = holder
        self.runDir = runDir
        self.resident = resident
        self.mode = mode
    }
}

/// An argument the app does not know, or a flag missing its value.
public struct OptionsError: Error, Equatable, CustomStringConvertible {
    public let description: String
}

/// Parses the arguments after the program name. The state root defaults as
/// the daemon's does: $AGENT_REPL_STATE_DIR, else ~/.claude-emacs; the run
/// directory as every service's: $AGENT_REPL_LOCK_DIR, else
/// ~/.cache/agent-repl/run. macOS adds
/// a `-psn_…` argument to an app it launches from Finder on older systems;
/// it is ignored.
public func parseOptions(_ arguments: [String], environment: [String: String], home: URL) throws -> Options {
    let stateDefault = environment["AGENT_REPL_STATE_DIR"].map { URL(fileURLWithPath: $0) }
        ?? home.appendingPathComponent(".claude-emacs")
    let runDefault = environment["AGENT_REPL_LOCK_DIR"].map { URL(fileURLWithPath: $0) }
        ?? home.appendingPathComponent(".cache/agent-repl/run")
    var options = Options(
        stateDir: stateDefault,
        logPath: home.appendingPathComponent(".cache/agent-repl/log/launcher.log"),
        holder: home.appendingPathComponent(".cache/agent-repl/sock/launcher-holder.sock").path,
        runDir: runDefault,
        resident: false)
    var rest = arguments[...]
    while let argument = rest.popFirst() {
        switch argument {
        case "--resident":
            options.resident = true
        case "--check-notifications":
            options.mode = .checkNotifications
        case "--request-notifications":
            options.mode = .requestNotifications
        case "--state-dir", "--log", "--holder", "--run-dir":
            guard let value = rest.popFirst() else {
                throw OptionsError(description: "\(argument) needs a path")
            }
            switch argument {
            case "--state-dir": options.stateDir = URL(fileURLWithPath: value)
            case "--log": options.logPath = URL(fileURLWithPath: value)
            case "--holder": options.holder = value
            default: options.runDir = URL(fileURLWithPath: value)
            }
        case let other where other.hasPrefix("-psn_"):
            continue
        default:
            throw OptionsError(description: "unknown argument \(argument)")
        }
    }
    return options
}
