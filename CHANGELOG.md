## 0.3.0 (2025-02-13)

- feat(renderer): add breaks in large diagnostics ([#63](https://github.com/johnyob/grace/pull/63))
- feat(renderer): check for tty when rendering ([#60](https://github.com/johnyob/grace/pull/60))
- fix(renderer): off-by-one in `lines_of_labels` ([#65](https://github.com/johnyob/grace/pull/65))
- fix(renderer): create 0-sized segment on empty label range ([#64](https://github.com/johnyob/grace/pull/64))
- fix(renderer): renderer multi-line labels when message is empty ([#53](https://github.com/johnyob/grace/pull/53))
- fix(renderer): fix errors that occur with special zero-width segments ([#41](https://github.com/johnyob/grace/pull/41))
- fix(renderer): use `Format.pp_infinity` in `Message.to_string` for OCaml >5.2 ([#40](https://github.com/johnyob/grace/pull/40))
- refactor!: removes base/core dependency ([#58](https://github.com/johnyob/grace/pull/58))

### BREAKING CHANGE

* Many base/core interfaces from `Grace` have been replaced with custom ones.
* `Source.reader` has been removed. Use `Source.Reader.t` instead. 

## 0.2.0 (2024-05-28)

* fix(renderer): remove uncessary underlines when printing a unique 'multi-line `Top` marker' ([#31](https://github.com/johnyob/grace/pull/31))
* fix(renderer): replace unicode chars with ASCII in `Config.ascii` ([#27](https://github.com/johnyob/grace/pull/27))
* feat(renderer): add `NO_COLOR` and `TERM` support to `Config` ([#8](https://github.com/johnyob/grace/pull/8))
* feat(core,renderer): add support for error codes ([#30](https://github.com/johnyob/grace/pull/30))
* feat(renderer): add support for UTF8 encoding 🚀 ([#25](https://github.com/johnyob/grace/pull/25))
* feat(renderer): re-introduce support for compact diagnostic rendering ([#28](https://github.com/johnyob/grace/pull/28))
* refactor(renderer)!: move `grace.renderer` library to `grace.ansi_renderer` ([#29](https://github.com/johnyob/grace/pull/29))

### BREAKING CHANGE

* `Grace_rendering` has been removed. Use `Grace_ansi_renderer` instead.


## 0.1.0 (2024-01-03)

Initial release 🎉
