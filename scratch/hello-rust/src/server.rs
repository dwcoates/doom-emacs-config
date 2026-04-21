use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};

use hello_rust::hello;

const ADDR: &str = "127.0.0.1:7878";

fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind(ADDR)?;
    println!("listening on http://{ADDR}");

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                if let Err(e) = handle(stream) {
                    eprintln!("connection error: {e}");
                }
            }
            Err(e) => eprintln!("accept error: {e}"),
        }
    }
    Ok(())
}

fn handle(mut stream: TcpStream) -> std::io::Result<()> {
    let mut reader = BufReader::new(stream.try_clone()?);
    let mut request_line = String::new();
    reader.read_line(&mut request_line)?;

    let mut parts = request_line.split_whitespace();
    let method = parts.next().unwrap_or("");
    let path = parts.next().unwrap_or("");

    let (status, body) = match (method, path) {
        ("GET", "/hello") => ("200 OK", hello()),
        ("GET", _) => ("404 Not Found", String::from("not found")),
        _ => ("405 Method Not Allowed", String::from("method not allowed")),
    };

    let response = format!(
        "HTTP/1.1 {status}\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
        body.len()
    );
    stream.write_all(response.as_bytes())?;
    stream.flush()?;
    Ok(())
}
