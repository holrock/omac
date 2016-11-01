# omac (WIP)

omca is port of [camo](https://github.com/atmos/camo) for ocaml.

```sh
opam install conf-libev # optional
opam install lwt ssl cohttp cryptokit alcotest
make
LWT_LOG=debug ./main.native
# start server localhost:8999
# http://localhost:8999/ed2c9ebb2e767518a3b59a06c176b4e5f3a7f675?url=http%3A%2F%2Fwww.vim.org%2Fimages%2Fvim_header.gif
```

## setting

omac read settings from env values

* PORT
* OMAC\_KEY (key for hmac digest, should be overwrite)
* OMAC\_MAX\_REDIRECTS
* OMAC\_SOCKET\_TIMEOUT
* OMAC\_KEEP\_ALIVE
* OMAC\_ACCEPT
* OMAC\_ACCEPT\_ENCODING
* OMAC\_HEADER\_VIA
* OMAC\_CONTENT\_LENGTH\_LIIT
