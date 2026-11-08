import Foundation

/// The app's command line. launchd's com.agentrepl.notifier passes
/// `--resident`; an open by hand (Spotlight, Finder) or by a banner's click
/// passes nothing.
public struct Options: Equatable {
    /// The agent-repl state root whose `daemon.addr` names the daemon.
    public var stateDir: URL
    /// The app's JSONL log.
    public var logPath: URL
    /// Started by launchd to run in the background. An instance started any
    /// other way was opened by the user, so it brings Emacs forward.
    public var resident: Bool

    public init(stateDir: URL, logPath: URL, resident: Bool) {
        self.stateDir = stateDir
        self.logPath = logPath
        self.resident = resident
    }
}

/// An argument the app does not know, or a flag missing its value.
public struct OptionsError: Error, Equatable, CustomStringConvertible {
    public let description: String
}

/// Parses the arguments after the program name. The state root defaults as
/// the daemon's does: $AGENT_REPL_STATE_DIR, else ~/.claude-emacs. macOS adds
/// a `-psn_…` argument to an app it launches from Finder on older systems;
/// it is ignored.
public func parseOptions(_ arguments: [String], environment: [String: String], home: URL) throws -> Options {
    let stateDefault = environment["AGENT_REPL_STATE_DIR"].map { URL(fileURLWithPath: $0) }
        ?? home.appendingPathComponent(".claude-emacs")
    var options = Options(
        stateDir: stateDefault,
        logPath: home.appendingPathComponent(".cache/agent-repl/log/notifier.log"),
        resident: false)
    var rest = arguments[...]
    while let argument = rest.popFirst() {
        switch argument {
        case "--resident":
            options.resident = true
        case "--state-dir", "--log":
            guard let value = rest.popFirst() else {
                throw OptionsError(description: "\(argument) needs a path")
            }
            if argument == "--state-dir" {
                options.stateDir = URL(fileURLWithPath: value)
            } else {
                options.logPath = URL(fileURLWithPath: value)
            }
        case let other where other.hasPrefix("-psn_"):
            continue
        default:
            throw OptionsError(description: "unknown argument \(argument)")
        }
    }
    return options
}
