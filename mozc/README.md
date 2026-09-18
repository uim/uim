# uim-mozc-helper

`uim-mozc-helper` is a small bridge process between uim and
`mozc_server`. uim talks to it with line-oriented S-expressions on
stdin/stdout (modelled on the `mozc_emacs_helper` protocol, but with
uim-specific field names and commands) and it
talks to `mozc_server` directly over Mozc's Unix domain socket IPC
using the protocol buffer definitions in `protocol/` and `ipc/`.

`mozc_emacs_helper` is used only as the "server keeper": it knows the
path of `mozc_server`, launches it when needed and handles version
mismatches. Once the server is running, every command goes through
the direct IPC connection.

## Protocol buffer definitions

The `.proto` files are verbatim copies from the Mozc repository
(<https://github.com/google/mozc>), BSD-3-Clause licensed:

- `src/protocol/commands.proto`
- `src/protocol/candidate_window.proto`
- `src/protocol/config.proto`
- `src/protocol/engine_builder.proto`
- `src/protocol/user_dictionary_storage.proto`
- `src/ipc/ipc.proto`

Copied from commit `cbbb6e1bd181cb9f3b409622d916a53a35400ec7`
(2026-09-17). They are wire compatible with older and newer
`mozc_server` as long as Mozc keeps field numbers stable. To
resynchronize, replace the files and rebuild; no code changes are
needed because the S-expression conversion is reflection based.

## Protocol between uim and uim-mozc-helper

`uim-mozc-helper` stays silent on stdout until the first request,
because uim launches it through `process-io`, which inspects the
child's early stdout to detect `exec` failures. uim greets it first
with `(EVENT_ID Hello)` and the helper answers with:

    ((event-id . EVENT_ID)(version . "MOZC_VERSION")(config . ((preedit-method . roman)))(uim-mozc-helper . t)(uim-version . "..."))

or, when mozc_server can't be reached:

    ((event-id . EVENT_ID)(error . no-server)(message . "..."))

Other requests are single lines. Responses are single lines of the form:

    ((event-id . EVENT_ID)(session-id . SESSION_ID)(output . OUTPUT))

or, on failure:

    ((event-id . EVENT_ID)(error . ERROR_SYMBOL)(message . "..."))

Requests:

- `(EVENT_ID Hello)` (must be sent first; returns the greeting above)
- `(EVENT_ID CreateSession)`
- `(EVENT_ID DeleteSession SESSION_ID)`
- `(EVENT_ID SendKey SESSION_ID KEY...)` (a convenience for other
  clients; the bundled `scm/mozc.scm` sends keys via `SendInput` with a
  `KeyEvent` alist instead) where each `KEY` is an
  integer (Unicode key code), a string (`key_string`), a modifier
  symbol (`shift`, `ctrl`, `alt`), a special key symbol (`enter`,
  `henkan`, `page-up`, ...; any `KeyEvent.SpecialKey` name) or an
  alist describing a `KeyEvent`.
- `(EVENT_ID SendCommand SESSION_ID ALIST)` where `ALIST` describes a
  `SessionCommand`, e.g. `((type . select-candidate) (id . 3))`.
- `(EVENT_ID SendInput SESSION_ID ALIST)` where `ALIST` describes a
  whole `Input`, e.g. `((type . get-config))`.

`OUTPUT` and every alist follow the `mozc_emacs_helper` conventions:
field names are lower-cased with `_` replaced by `-`, booleans are
Scheme `#t`/`#f`, enums are symbols, 64-bit integers are strings, repeated
fields are lists and messages are alists.

## Origin and credits

The Scheme side of this Mozc support derives from the **MacUIM** project
by Etsushi Kato (<https://github.com/e-kato/macuim>), which shipped a uim-mozc
implementation under the BSD-3-Clause "uim Project" license:

- `scm/mozc-custom.scm` and `scm/mozc-key-custom.scm` are taken from
  MacUIM (`Mozc/scm/`) almost verbatim; only the `uim-mozc-helper`
  related customs were added.
- `scm/mozc.scm` is a rewrite of MacUIM's `Mozc/scm/mozc.scm`. The
  original used a native plugin (MacUIM's `Mozc/uim/mozc.c`) linked
  against the Mozc client library; this version instead drives
  `mozc_server` through `uim-mozc-helper`, so uim itself needs no Mozc
  build dependency. That native plugin is not used here.
- `uim-mozc-helper.cpp` is mostly new, but several parts are derived
  from Mozc (Copyright Google Inc., BSD-3-Clause), as its file header
  records: the protobuf-to-S-expression printer (`PrintMessage` and
  friends) from `src/unix/emacs/mozc_emacs_helper_lib.cc`,
  `GetUserProfileDirectory` from `base/system_util.cc`, and `IsValidKey`
  plus the IPC socket path/name and request framing from
  `ipc/ipc_path_manager.cc` and `ipc/unix_ipc.cc`, and smaller idioms
  (the usage-data stripping in `Response()` from that emacs helper's
  `RemoveUsageData`, and the `CREATE_SESSION` capability/application-info
  setup from `client/client.cc`). The S-expression reader, the
  alist-to-protobuf filler and the uim-facing protocol are original.

## Future work

This helper links its own copy of the Mozc protocol buffers and speaks
Mozc's Unix domain socket IPC directly, because Mozc currently offers no
stable, minimal client interface for third parties. The Mozc maintainers
are discussing providing one (`mozc_client_cli` / `libmozc_client.a`):

- <https://github.com/google/mozc/discussions/1362#discussioncomment-14579138>

Once such an official interface ships, `uim-mozc-helper` should be
reworked to use it instead of the bundled `protocol/` and `ipc/` copies
and the hand-rolled IPC, which removes the need to track Mozc's internal
protocol here.
