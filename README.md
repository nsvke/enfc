# ENFC: A Statically Typed Compiler Implementation in Rust

enfc is a experimental statically-typed, ahead-of-time (AOT) compiled systems programming language. It is designed to be minimal, predictable, and capable of direct C-FFI and POSIX syscall integrations.

Currently, enfc compiles to C as an Intermediate Representation (IR) and leverages system compilers (gcc/clang) to generate native binaries.

## Build and Usage

You can directly run enfc in a reproducible, isolated environment using Nix:

```bash
# Run a specific .enf file directly
nix run github:nsvke/enfc -- source.enf

# Or enter a pure development shell with all dependencies
nix develop
```

If you prefer building from source using standard Rust tooling:

```bash
# Clone the repository
git clone https://github.com/nsvke/enfc
cd enfc
cargo build --release

# The binary will be available at target/release/enfc
./target/release/enfc main.enf
```

## Quick Example: TCP Server

Here is a minimal HTTP/TCP server written in enf:

```enf
fun main() int {
    print("Starting server on port 8080...\n");
    val server_fd = std_net_socket();

    std_net_bind(server_fd, 8080);
    std_net_listen(server_fd, 10);

    val response = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nConnection: close\r\n\r\nHello from enfc!";
    val response_len = std_str_len(response);

    whl true {
        val client_fd = std_net_accept(server_fd);
        if client_fd >= 0 {
            std_net_send(client_fd, response, response_len);
            std_net_close(client_fd);
        }
    }
    ret 0;
}
```

## 📝 License

Distributed under the MIT License. See `LICENSE` for more information.
