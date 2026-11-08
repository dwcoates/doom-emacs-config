import Foundation
import SwiftProtobuf

/// The daemon's Connect service, whose procedures this app calls.
public let agentReplService = "agentrepl.v1.AgentRepl"

/// A Connect call that failed: the transport, an HTTP status, or the
/// protocol's own error (its code and message).
public struct ConnectError: Error, CustomStringConvertible, Equatable {
    public let code: String
    public let message: String
    public var description: String { "connect \(code): \(message)" }
}

/// Frames one message in a Connect streaming envelope: a flags byte (0 for a
/// message) and the message's big-endian 32-bit length, then the message.
public func envelope(_ payload: Data, flags: UInt8 = 0) -> Data {
    var framed = Data([flags])
    let length = UInt32(payload.count).bigEndian
    withUnsafeBytes(of: length) { framed.append(contentsOf: $0) }
    framed.append(payload)
    return framed
}

/// One envelope read off a Connect stream.
public enum Envelope: Equatable {
    /// A message frame's payload.
    case message(Data)
    /// The end-of-stream frame's JSON: `{}` for a clean end, or
    /// `{"error": {...}}`.
    case end(Data)
}

/// Splits complete envelopes off the front of `buffer`, leaving any partial
/// one in place for more bytes.
public func takeEnvelopes(_ buffer: inout Data) -> [Envelope] {
    var out: [Envelope] = []
    while buffer.count >= 5 {
        let start = buffer.startIndex
        let flags = buffer[start]
        let length = buffer[(start + 1)..<(start + 5)].reduce(UInt32(0)) { ($0 << 8) | UInt32($1) }
        guard buffer.count >= 5 + Int(length) else { break }
        let payload = Data(buffer[(start + 5)..<(start + 5 + Int(length))])
        buffer = Data(buffer[(start + 5 + Int(length))...])
        out.append(flags & 0x02 != 0 ? .end(payload) : .message(payload))
    }
    return out
}

/// Reads a Connect error object (`{"code": ..., "message": ...}`), whether
/// bare (a unary error body) or under `error` (an end-of-stream frame). Nil
/// for an end-of-stream frame that carries no error.
public func connectError(fromJSON data: Data) -> ConnectError? {
    guard let object = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else {
        return ConnectError(code: "unknown", message: String(decoding: data, as: UTF8.self))
    }
    let body = (object["error"] as? [String: Any]) ?? (object["code"] != nil ? object : nil)
    guard let body else { return nil }
    return ConnectError(code: body["code"] as? String ?? "unknown", message: body["message"] as? String ?? "")
}

/// The daemon's Connect endpoint at one advertised address, over HTTP/1.1
/// (the daemon serves Connect on HTTP/1.1 and h2c alike), with binary
/// protobuf bodies.
public struct ConnectClient: Sendable {
    public let address: String
    private let session: URLSession

    public init(address: String, session: URLSession = .shared) {
        self.address = address
        self.session = session
    }

    private func request(_ method: String, contentType: String) -> URLRequest {
        var request = URLRequest(url: URL(string: "http://\(address)/\(agentReplService)/\(method)")!)
        request.httpMethod = "POST"
        request.setValue(contentType, forHTTPHeaderField: "Content-Type")
        request.setValue("1", forHTTPHeaderField: "Connect-Protocol-Version")
        request.timeoutInterval = .infinity
        return request
    }

    /// Calls one unary procedure.
    public func unary<Req: SwiftProtobuf.Message, Resp: SwiftProtobuf.Message>(
        _ method: String, _ message: Req, timeout: TimeInterval
    ) async throws -> Resp {
        var request = request(method, contentType: "application/proto")
        request.timeoutInterval = timeout
        request.httpBody = try message.serializedData()
        let (data, response) = try await session.data(for: request)
        let status = (response as? HTTPURLResponse)?.statusCode ?? 0
        guard status == 200 else {
            throw connectError(fromJSON: data) ?? ConnectError(code: "http_\(status)", message: String(decoding: data, as: UTF8.self))
        }
        return try Resp(serializedBytes: data)
    }

    /// Opens one server-streaming procedure, answering its messages in order.
    /// The stream finishes cleanly on a clean end-of-stream frame, and throws
    /// the protocol's error, an HTTP status, or a transport failure otherwise.
    public func serverStream<Req: SwiftProtobuf.Message, Resp: SwiftProtobuf.Message>(
        _ method: String, _ message: Req
    ) -> AsyncThrowingStream<Resp, Error> {
        var request = request(method, contentType: "application/connect+proto")
        let session = self.session
        return AsyncThrowingStream { continuation in
            let task = Task {
                do {
                    request.httpBody = envelope(try message.serializedData())
                    let (bytes, response) = try await session.bytes(for: request)
                    let status = (response as? HTTPURLResponse)?.statusCode ?? 0
                    guard status == 200 else {
                        var body = Data()
                        for try await byte in bytes { body.append(byte) }
                        throw connectError(fromJSON: body) ?? ConnectError(code: "http_\(status)", message: "")
                    }
                    var buffer = Data()
                    for try await byte in bytes {
                        buffer.append(byte)
                        guard buffer.count >= 5 else { continue }
                        for frame in takeEnvelopes(&buffer) {
                            switch frame {
                            case .message(let payload):
                                continuation.yield(try Resp(serializedBytes: payload))
                            case .end(let json):
                                if let error = connectError(fromJSON: json) { throw error }
                                continuation.finish()
                                return
                            }
                        }
                    }
                    throw ConnectError(code: "unavailable", message: "the stream ended with no end-of-stream frame")
                } catch {
                    continuation.finish(throwing: error)
                }
            }
            continuation.onTermination = { _ in task.cancel() }
        }
    }
}
