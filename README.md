# Agent vs. Agent

Agent vs. Agent is intended to be a collection of simple games that can be played by computer programs developed by people of various levels of experience. Agent vs. Agent targets small groups of people, and attempts to provide the tools necessary to conduct competitive agent vs agent combat. The ultimate goal is to help facilitate learning, whether that be through exploring new languages, learning new concepts, or smashing your co-workers egos.

# This is an alpha release

* The server isn't stable... it will leak memory (games don't clean up after them selves), it will crash when a client does something unexpected (like disconnect in the middle of a turn...), it will still assign disconnected players to games (and then crash).
* The transport layer may change. Seems like a http transport would be easier to manage.

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

## Potential game ideas

* Team games: Spades, Bridge
* Variable # of players: 
* Pathing: Othello (Reversi), Blokus, Tsuro

## Contributing

### Mac OSX installation instructions:

* Fork project on github and clone
* run `npm install`
* run tests via `make test`

The command `make setup` has a more complete setup, including thrift

## In need of

Sample agents for languages


## Credits

* Thrift: http://thrift.apache.org/
* Cards: https://github.com/selfthinker/CSS-Playing-Cards
