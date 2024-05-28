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