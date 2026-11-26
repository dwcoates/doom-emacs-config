import CryptoKit
import Foundation

/// A record's severity, as the logging contract spells it.
public enum LogLevel: String, Sendable {
    case debug, info, warn, error
}

/// One structured record's operation-specific evidence.
public typealias LogContext = [String: any Sendable]

/// One record, rendered: what the canonical logger writes. The call that made
/// it names a literal template and its named values (`Logger.log`); `message`
/// is the template rendered from them, `site` the call's key.
public struct LogEntry: Sendable {
    public let level: LogLevel
    public let operation: String
    public let site: String
    public let template: String
    public let message: String
    public let context: LogContext

    public init(level: LogLevel, operation: String, site: String, template: String, message: String, context: LogContext) {
        self.level = level
        self.operation = operation
        self.site = site
        self.template = template
        self.message = message
        self.context = context
    }
}

/// Where a record goes: the app's one canonical logging path.
public protocol Logger: Sendable {
    /// Takes one rendered record.
    func write(_ entry: LogEntry)
}

extension Logger {
    /// Logs one record at `level`. THE TEMPLATE IS A STRING LITERAL (a
    /// `StaticString` accepts nothing else): its `{name}` placeholders are
    /// filled from `context`, which keeps every value as a field too
    /// (logging-contract.md "Log sites"). The record's site is keyed by the
    /// calling file's module-qualified name and function.
    public func log(_ level: LogLevel, _ operation: String, _ template: StaticString, _ context: LogContext = [:],
                    file: StaticString = #fileID, function: StaticString = #function) {
        let text = template.description
        let caller = logSiteCaller(file: file.description, function: function.description)
        write(LogEntry(
            level: level, operation: operation, site: logSiteKey(runtime: launcherRuntime, function: caller, template: text),
            template: text, message: renderLogTemplate(text, context), context: context))
    }
    public func info(_ operation: String, _ template: StaticString, _ context: LogContext = [:],
                     file: StaticString = #fileID, function: StaticString = #function) {
        log(.info, operation, template, context, file: file, function: function)
    }
    public func debug(_ operation: String, _ template: StaticString, _ context: LogContext = [:],
                      file: StaticString = #fileID, function: StaticString = #function) {
        log(.debug, operation, template, context, file: file, function: function)
    }
    public func warn(_ operation: String, _ template: StaticString, _ context: LogContext = [:],
                     file: StaticString = #fileID, function: StaticString = #function) {
        log(.warn, operation, template, context, file: file, function: function)
    }
    public func error(_ operation: String, _ template: StaticString, _ context: LogContext = [:],
                      file: StaticString = #fileID, function: StaticString = #function) {
        log(.error, operation, template, context, file: file, function: function)
    }
}

/// The function a site is keyed by: the calling file's module-qualified name
/// (`ZettelLauncher/Clicks`) and the function written in it. Swift recovers
/// no enclosing type at compile time, so the file stands for it.
public func logSiteCaller(file: String, function: String) -> String {
    let stem = file.hasSuffix(".swift") ? String(file.dropLast(".swift".count)) : file
    return "\(stem).\(function)"
}

/// A site's key: the first 12 hex characters of the sha256 of the runtime,
/// the function and the template, joined by NUL bytes. Every runtime derives
/// it identically (`Key("daemon", "pkg.F", "hello {name}")` is `074892b7d933`).
public func logSiteKey(runtime: String, function: String, template: String) -> String {
    let joined = Data([runtime, function, template].joined(separator: "\u{0}").utf8)
    return String(SHA256.hash(data: joined).map { String(format: "%02x", $0) }.joined().prefix(12))
}

/// One piece of a parsed template.
public enum LogTemplatePiece: Equatable, Sendable {
    case literal(String)
    case placeholder(String)
}

/// A template that does not parse: a defect of the call that wrote it.
public struct LogTemplateError: Error, CustomStringConvertible, Equatable {
    public let template: String
    public let problem: String
    public var description: String { "the log template \"\(template)\" \(problem)" }
}

/// Splits `template` into literal runs and `{name}` placeholders. A literal
/// brace is written doubled (`{{`, `}}`); a placeholder is a lowercase
/// identifier (`[a-z_][a-z0-9_]*`).
public func parseLogTemplate(_ template: String) throws -> [LogTemplatePiece] {
    let chars = Array(template)
    var pieces: [LogTemplatePiece] = []
    var run = ""
    var i = 0
    func isIdentifier(_ name: String) -> Bool {
        guard let first = name.unicodeScalars.first, first == "_" || ("a"..."z").contains(first) else { return false }
        return name.unicodeScalars.allSatisfy { $0 == "_" || ("a"..."z").contains($0) || ("0"..."9").contains($0) }
    }
    while i < chars.count {
        let c = chars[i]
        let next: Character? = i + 1 < chars.count ? chars[i + 1] : nil
        if c == "{" && next == "{" {
            run.append("{"); i += 2
        } else if c == "}" && next == "}" {
            run.append("}"); i += 2
        } else if c == "{" {
            guard let close = chars[(i + 1)...].firstIndex(of: "}") else {
                throw LogTemplateError(template: template, problem: "opens a placeholder that never closes (a literal brace is written {{)")
            }
            let name = String(chars[(i + 1)..<close])
            guard isIdentifier(name) else {
                throw LogTemplateError(template: template, problem: "names the placeholder {\(name)}, which is not a lowercase identifier (a literal brace is written {{)")
            }
            if !run.isEmpty { pieces.append(.literal(run)); run = "" }
            pieces.append(.placeholder(name))
            i = close + 1
        } else if c == "}" {
            throw LogTemplateError(template: template, problem: "has a lone closing brace (a literal brace is written }})")
        } else {
            run.append(c); i += 1
        }
    }
    if !run.isEmpty { pieces.append(.literal(run)) }
    return pieces
}

/// Renders `template` from `context`. A template that does not parse, or
/// names a placeholder the context does not hold, is a defect of the call:
/// it fails loudly rather than writing a record that disagrees with its
/// fields.
public func renderLogTemplate(_ template: String, _ context: LogContext) -> String {
    let pieces: [LogTemplatePiece]
    do { pieces = try parseLogTemplate(template) } catch { fatalError("\(error)") }
    return pieces.map { piece in
        switch piece {
        case .literal(let text): return text
        case .placeholder(let name):
            guard let value = context[name] else {
                fatalError("the log template \"\(template)\" names {\(name)}, which the record's values do not hold")
            }
            return String(describing: value)
        }
    }.joined()
}

/// The runtime name every record of this app carries.
public let launcherRuntime = "launcher"

/// Renders `date` in the shared log timestamp representation
/// (`proto/vocab/log-timestamp.json`): RFC 3339, `zone`'s local time, exactly
/// six fractional digits, a `±HH:MM` offset.
public func logTimestamp(_ date: Date, zone: TimeZone = .current) -> String {
    var calendar = Calendar(identifier: .gregorian)
    calendar.timeZone = zone
    let parts = calendar.dateComponents([.year, .month, .day, .hour, .minute, .second, .nanosecond], from: date)
    let micros = (parts.nanosecond ?? 0) / 1_000
    let offset = zone.secondsFromGMT(for: date)
    let sign = offset < 0 ? "-" : "+"
    let magnitude = abs(offset)
    return String(
        format: "%04d-%02d-%02dT%02d:%02d:%02d.%06d%@%02d:%02d",
        parts.year!, parts.month!, parts.day!, parts.hour!, parts.minute!, parts.second!,
        micros, sign, magnitude / 3600, (magnitude % 3600) / 60)
}

/// Renders one record as the contract's JSONL object, without its newline.
public func logLine(
    _ entry: LogEntry, at date: Date, pid: Int32, zone: TimeZone = .current
) -> Data {
    let record: [String: Any] = [
        "timestamp": logTimestamp(date, zone: zone),
        "runtime": launcherRuntime,
        "level": entry.level.rawValue,
        "verbosity": "normal",
        "operation": entry.operation,
        "message": entry.message,
        "site": entry.site,
        "template": entry.template,
        "context": entry.context.mapValues { jsonValue($0) },
        "pid": Int(pid),
    ]
    // Every value above is JSON-representable by construction (jsonValue), so
    // a failed serialization is a broken invariant, never a record to drop.
    return try! JSONSerialization.data(withJSONObject: record, options: [.sortedKeys, .withoutEscapingSlashes])
}

/// Maps a context value onto JSON: numbers, strings and booleans as they are,
/// anything else by its description.
private func jsonValue(_ value: any Sendable) -> Any {
    switch value {
    case let v as String: return v
    case let v as Bool: return v
    case let v as Int: return v
    case let v as Int32: return Int(v)
    case let v as Int64: return v
    case let v as UInt64: return v
    case let v as Double: return v
    default: return String(describing: value)
    }
}

/// The app's canonical logger: one JSONL file, appended to across launches
/// and rotated at the contract's 64 MiB cap into five generations
/// (`<path>.1` newest through `<path>.5` oldest), rotation happening before
/// the append that would cross the cap.
public final class FileLogger: Logger, @unchecked Sendable {
    public static let capBytes: UInt64 = 64 * 1024 * 1024
    public static let generations = 5

    private let path: URL
    private let cap: UInt64
    private let pid: Int32
    private let lock = NSLock()

    public init(path: URL, cap: UInt64 = FileLogger.capBytes, pid: Int32 = getpid()) {
        self.path = path
        self.cap = cap
        self.pid = pid
    }

    public func write(_ entry: LogEntry) {
        var line = logLine(entry, at: Date(), pid: pid)
        line.append(0x0A)
        lock.lock()
        defer { lock.unlock() }
        do {
            try append(line)
        } catch {
            // The sink itself failed: stderr is launchd's capture of this
            // process (app.zettel.launcher's StandardErrorPath), the one
            // place left that a person reads.
            FileHandle.standardError.write(Data("launcher: log sink \(path.path) failed: \(error)\n".utf8))
            FileHandle.standardError.write(line)
        }
    }

    private func append(_ line: Data) throws {
        let fm = FileManager.default
        try fm.createDirectory(at: path.deletingLastPathComponent(), withIntermediateDirectories: true)
        let size = (try? fm.attributesOfItem(atPath: path.path)[.size] as? UInt64) ?? 0
        if size > 0 && size + UInt64(line.count) > cap {
            try rotate()
        }
        if !fm.fileExists(atPath: path.path) {
            fm.createFile(atPath: path.path, contents: nil)
        }
        let handle = try FileHandle(forWritingTo: path)
        defer { try? handle.close() }
        try handle.seekToEnd()
        try handle.write(contentsOf: line)
    }

    private func rotate() throws {
        let fm = FileManager.default
        let oldest = generation(FileLogger.generations)
        if fm.fileExists(atPath: oldest.path) {
            try fm.removeItem(at: oldest)
        }
        for n in stride(from: FileLogger.generations - 1, through: 1, by: -1) {
            let from = generation(n)
            if fm.fileExists(atPath: from.path) {
                try fm.moveItem(at: from, to: generation(n + 1))
            }
        }
        try fm.moveItem(at: path, to: generation(1))
    }

    private func generation(_ n: Int) -> URL {
        URL(fileURLWithPath: path.path + ".\(n)")
    }
}

/// The content hash of the executable at `url`: lowercase hex SHA-256, the
/// name every agent-repl component's build goes by (buildreport.HashFile).
public func executableBuild(at url: URL) throws -> String {
    SHA256.hash(data: try Data(contentsOf: url)).map { String(format: "%02x", $0) }.joined()
}
