import Foundation
import SwiftProtobuf

/// One connection's bytes, as the banner exchange reads and writes them.
public protocol ByteChannel: Sendable {
    /// Reads up to `count` bytes; empty at the end of the stream.
    func read(upTo count: Int) throws -> Data
    /// Writes every byte of `data`.
    func write(_ data: Data) throws
}

/// The exchange's framing failed: the stream ended early, or a length is
/// past what a banner can be.
public struct FramingError: Error, Equatable, CustomStringConvertible {
    public let description: String
}

/// The largest message the exchange takes: a banner is a title, a body and
/// two identifiers, far below this.
public let maxDelimitedBytes = 1 << 20

/// Reads one message in protobuf's length-delimited form: a varint byte
/// count, then the message (launcher.v1's framing; Go's protodelim).
public func readDelimited<M: SwiftProtobuf.Message>(_ type: M.Type, from channel: ByteChannel) throws -> M {
    var length = 0
    var shift = 0
    while true {
        let byte = try channel.read(upTo: 1)
        guard let b = byte.first else {
            throw FramingError(description: "the stream ended inside a message's length")
        }
        length |= Int(b & 0x7f) << shift
        if b & 0x80 == 0 { break }
        shift += 7
        guard shift < 35 else { throw FramingError(description: "a message's length is longer than a varint can be") }
    }
    guard length <= maxDelimitedBytes else {
        throw FramingError(description: "a message of \(length) bytes is past the \(maxDelimitedBytes) a banner can be")
    }
    var body = Data()
    while body.count < length {
        let chunk = try channel.read(upTo: length - body.count)
        guard !chunk.isEmpty else {
            throw FramingError(description: "the stream ended \(length - body.count) bytes before the message did")
        }
        body.append(chunk)
    }
    return try M(serializedBytes: body)
}

/// Writes one message in protobuf's length-delimited form.
public func writeDelimited(_ message: SwiftProtobuf.Message, to channel: ByteChannel) throws {
    let body: Data = try message.serializedData()
    var frame = Data()
    var length = body.count
    repeat {
        var b = UInt8(length & 0x7f)
        length >>= 7
        if length != 0 { b |= 0x80 }
        frame.append(b)
    } while length != 0
    frame.append(body)
    try channel.write(frame)
}

/// A ByteChannel over one connected socket, which it closes when released.
public final class SocketChannel: ByteChannel, @unchecked Sendable {
    public let fd: Int32

    /// Takes `fd`, bounding each read and write at `timeout` seconds.
    public init(fd: Int32, timeout: TimeInterval) throws {
        self.fd = fd
        var tv = timeval(tv_sec: Int(timeout), tv_usec: Int32((timeout - Double(Int(timeout))) * 1_000_000))
        for option in [SO_RCVTIMEO, SO_SNDTIMEO] {
            guard setsockopt(fd, SOL_SOCKET, option, &tv, socklen_t(MemoryLayout<timeval>.size)) == 0 else {
                let cause = String(cString: strerror(errno))
                close(fd)
                throw FramingError(description: "bound the connection's reads and writes: \(cause)")
            }
        }
        var on: Int32 = 1
        setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &on, socklen_t(MemoryLayout<Int32>.size))
    }

    deinit { close(fd) }

    public func read(upTo count: Int) throws -> Data {
        var buffer = [UInt8](repeating: 0, count: count)
        let n = Darwin.read(fd, &buffer, count)
        guard n >= 0 else { throw FramingError(description: "read the connection: \(String(cString: strerror(errno)))") }
        return Data(buffer[0..<n])
    }

    public func write(_ data: Data) throws {
        var sent = 0
        while sent < data.count {
            let n = data.withUnsafeBytes { raw in Darwin.write(fd, raw.baseAddress! + sent, data.count - sent) }
            guard n > 0 else { throw FramingError(description: "write the connection: \(String(cString: strerror(errno)))") }
            sent += n
        }
    }
}
