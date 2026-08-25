# a2a-protocol

CLOS Agent2Agent protocol for cl-stack (A2A **1.0**).

JSON-RPC methods are **PascalCase** (`SendMessage`, `GetTask`, …). Dispatch also accepts slash aliases (`message/send`, `tasks/get`, …).

```lisp
(asdf:load-system "a2a-protocol")

(let ((agent (a2a-protocol:make-a2a-agent :name "echo")))
  (a2a-protocol:send-message
   agent (a2a-protocol:make-a2a-message :text "hi")))
```

Card well-known path is `/.well-known/agent-card.json` (alias `agent.json`). Bindings live in `a2a-backend-*`.

Part of [cl-stack](https://github.com/egao1980/cl-stack) agent-wire ([brief](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/a2a.md)). Tracks [#186](https://github.com/egao1980/cl-stack/issues/186).

CI: canned [`cl-repository`](https://github.com/egao1980/cl-repository) (`test-system.yml` / `setup-client` + `ci`). Deps from `ghcr.io/egao1980/cl-systems`.

## License

MIT
