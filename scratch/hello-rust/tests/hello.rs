use hello_rust::hello;

#[test]
fn hello_returns_greeting() {
    assert_eq!(hello(), "Hello, world!");
}
