import CryptoKit
import Foundation

/// A record's severity, as the logging contract spells it.
public enum LogLevel: String, Sendable {
    case debug, info, warn, error
}

/// One structured record's operation-specific evidence.
public typealias LogContext = [String: any Sendable]

/// Where a record goes: the app's one canonical logging path.
public protocol Logger: Sendable {
    /// Records one structured line.
    func log(_ level: LogLevel, _ operation: String, _ message: String, _ context: LogContext)
}

extension Logger {
    public func info(_ operation: String, _ message: String, _ context: LogContext = [:]) {
        log(.info, operation, message, context)
    }
    public func debug(_ operation: String, _ message: String, _ context: LogContext = [:]) {
        log(.debug, operation, message, context)
    }
    public func error(_ operation: String, _ message: String, _ context: LogContext = [:]) {
        log(.error, operation, message, context)
    }
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
    level: LogLevel, operation: String, message: String, context: LogContext,
    at date: Date, pid: Int32, zone: TimeZone = .current
) -> Data {
    let record: [String: Any] = [
        "timestamp": logTimestamp(date, zone: zone),
        "runtime": launcherRuntime,
        "level": level.rawValue,
        "verbosity": "normal",
        "operation": operation,
        "message": message,
        "context": context.mapValues { jsonValue($0) },
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

    public func log(_ level: LogLevel, _ operation: String, _ message: String, _ context: LogContext) {
        var line = logLine(level: level, operation: operation, message: message, context: context, at: Date(), pid: pid)
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
