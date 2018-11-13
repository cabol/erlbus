# Change Log

## [v0.2.2](https://github.com/cabol/erlbus/tree/v0.2.2) (2018-11-13)
[Full Changelog](https://github.com/cabol/erlbus/compare/v0.2.1...v0.2.2)

**Closed issues:**

- erlang:get\_stacktrace\(\) stop working on v21 [\#40](https://github.com/cabol/erlbus/issues/40)
- Please publish to hex.pm [\#37](https://github.com/cabol/erlbus/issues/37)

## [v0.2.1](https://github.com/cabol/erlbus/tree/v0.2.1) (2017-02-16)
[Full Changelog](https://github.com/cabol/erlbus/compare/0.2.0...v0.2.1)

**Closed issues:**

- Fix tests to use rebar3 directly instead of call ct\_run [\#38](https://github.com/cabol/erlbus/issues/38)

**Merged pull requests:**

- \[\#38\] – Fix tests to use rebar3 directly instead of call ct\_run [\#39](https://github.com/cabol/erlbus/pull/39) ([cabol](https://github.com/cabol))

## [0.2.0](https://github.com/cabol/erlbus/tree/0.2.0) (2016-05-22)
[Full Changelog](https://github.com/cabol/erlbus/compare/0.1.0...0.2.0)

**Fixed bugs:**

- Fix chat example regarding with changes in new version 0.2.0 [\#30](https://github.com/cabol/erlbus/issues/30)

**Closed issues:**

- Replace current build tool `erlang.mk` by `rebar3` [\#26](https://github.com/cabol/erlbus/issues/26)
- Implement distributed tests [\#24](https://github.com/cabol/erlbus/issues/24)
- Fix Makefile.rebar to use rebar3 [\#19](https://github.com/cabol/erlbus/issues/19)

**Merged pull requests:**

- General fixes and code optimizations. [\#35](https://github.com/cabol/erlbus/pull/35) ([cabol](https://github.com/cabol))
- Refactor messages to use maps instead of records. [\#34](https://github.com/cabol/erlbus/pull/34) ([cabol](https://github.com/cabol))
- Fixed `proplist\_to\_record` macro, added point\_to\_point example, fixed version to 1.0.0. [\#33](https://github.com/cabol/erlbus/pull/33) ([cabol](https://github.com/cabol))
- Fixed EDoc and optimize `ebus:subscribers/2` and `ebus:topics/1` functions. [\#32](https://github.com/cabol/erlbus/pull/32) ([cabol](https://github.com/cabol))
- Fixed chat example regarding to version 0.2.0 changes [\#31](https://github.com/cabol/erlbus/pull/31) ([ferigis](https://github.com/ferigis))
- Version Bump to 0.2.0 [\#29](https://github.com/cabol/erlbus/pull/29) ([cabol](https://github.com/cabol))

## [0.1.0](https://github.com/cabol/erlbus/tree/0.1.0) (2016-01-06)
**Implemented enhancements:**

- Implement sharding distribution model using `jumping consistent hash` on top of `gproc` [\#27](https://github.com/cabol/erlbus/issues/27)

**Fixed bugs:**

- Fix compatibility with Erlang/OTP 18 [\#17](https://github.com/cabol/erlbus/issues/17)

**Merged pull requests:**

- Fixed ebus\_handler to support handle\_fun/1 [\#28](https://github.com/cabol/erlbus/pull/28) ([cabol](https://github.com/cabol))
- Fixed ebus\_handler to allow create new handlers/pools receiving a fun… [\#23](https://github.com/cabol/erlbus/pull/23) ([cabol](https://github.com/cabol))
- Chat Example : Switching to cowboy 1.0 in order to support previous E… [\#22](https://github.com/cabol/erlbus/pull/22) ([ferigis](https://github.com/ferigis))
- Chat Example : Unsubscribing from the channel when terminate [\#21](https://github.com/cabol/erlbus/pull/21) ([ferigis](https://github.com/ferigis))
- Issue \#17: replace function now/0 by os:timestamp/0, for compatibilit… [\#18](https://github.com/cabol/erlbus/pull/18) ([cabol](https://github.com/cabol))
- Adding Chat Example [\#15](https://github.com/cabol/erlbus/pull/15) ([ferigis](https://github.com/ferigis))
- Fixes to ebus\_pg2 module. [\#14](https://github.com/cabol/erlbus/pull/14) ([cabol](https://github.com/cabol))
- Refactoring functions: get\_subscribers -\> subscribers and get\_channel… [\#13](https://github.com/cabol/erlbus/pull/13) ([cabol](https://github.com/cabol))
- Fixed 'ebus:sub/2,3' and 'ebus:unsub/2,3' to receive either a single … [\#12](https://github.com/cabol/erlbus/pull/12) ([cabol](https://github.com/cabol))
- Code Dialyzed. Added pool\_hanler to pub\_sub example. [\#11](https://github.com/cabol/erlbus/pull/11) ([cabol](https://github.com/cabol))
- Fixed indentation issue. [\#10](https://github.com/cabol/erlbus/pull/10) ([cabol](https://github.com/cabol))
- Cabol.version 0.1 [\#9](https://github.com/cabol/erlbus/pull/9) ([cabol](https://github.com/cabol))
- Fixed indentation in .app.src file from pub\_sub example. Added image … [\#8](https://github.com/cabol/erlbus/pull/8) ([cabol](https://github.com/cabol))
- Fixed indentation. [\#7](https://github.com/cabol/erlbus/pull/7) ([cabol](https://github.com/cabol))
- Added pub\_sub example. Fixed build files, updated erlang.mk. [\#6](https://github.com/cabol/erlbus/pull/6) ([cabol](https://github.com/cabol))
- Added rebar.config.script to fetch dependencies on-demand when using … [\#5](https://github.com/cabol/erlbus/pull/5) ([cabol](https://github.com/cabol))
- Added status function to ebus\_handler module. Fixed README. [\#4](https://github.com/cabol/erlbus/pull/4) ([cabol](https://github.com/cabol))
- Fixed anonymous handler in ebus\_handler. Fixed documentation. Added C… [\#3](https://github.com/cabol/erlbus/pull/3) ([cabol](https://github.com/cabol))
- Cabol.version 0.1 [\#2](https://github.com/cabol/erlbus/pull/2) ([cabol](https://github.com/cabol))
- Fixed README [\#1](https://github.com/cabol/erlbus/pull/1) ([cabol](https://github.com/cabol))



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*