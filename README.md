# Spam ban bot

an example can be see at [@android_declarative](https://t.me/android_declarative)

```bash
docker build -t idkjs/watchcat-bot-base -f Base.Dockerfile .
docker push idkjs/watchcat-bot-base
```

Requires unreleased lib

```sh
opam repository add y2k git://github.com/y2k/opam
opam pin y2k-telegraml 3.2.1
opam update && opam install y2k-telegraml
```
