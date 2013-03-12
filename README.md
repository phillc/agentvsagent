# Agent vs. Agent

Agent vs. Agent is intended to be a collection of simple games that can be played by computer programs developed by people of various levels of experience. Agent vs. Agent targets small groups of people, and attempts to provide the tools necessary to conduct competitive agent vs agent combat. The ultimate goal is to help facilitate learning, whether that be through exploring new languages, learning new concepts, or smashing your co-workers egos.

# This is an alpha release

## Goals

* Privately deployable
* Support many languages
* Human playable
* Low barrier to entry

## Installation

* Install node: [via package manager](https://github.com/joyent/node/wiki/Installing-Node.js-via-package-manager)
* npm install -g agentvsagent
* run `ava` for command options

## Starting an agent

* Clone or download the repository.
* Use the files in the dist/hearts/<language>/ directory

## Tips

* Basic
  * Isolate your logic to the starter agent in a different file/module, to reduce conflict if the api changes.
  * Keep your agent under source control
* Advanced
  * Unit test at least your supporting code

## FAQ

* Time that each agent is allowed should be low enough to make it difficult to come up with an always perfect solution, yet high enough that a novice and/or an implementation in a slower language can be competitive.

## Potential game ideas

* Othello (Reversi)
* Spades
* Bridge


## TODO

* Data validations
* Shooting the moon
* Timeout management
* Clean up after games are done (memory leak)
* Human player(s)
* Server will crash if a player disconnects mid game, or pass in null for a card

## Contributing

### Mac OSX installation instructions:

* Fork project on github and clone
* run `npm install`
* run tests via `make test`

The command `make setup` has a more complete setup, including thrift

## In need of

Sample agents for languages

