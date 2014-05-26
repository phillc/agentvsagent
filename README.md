# Agent vs. Agent

Agent vs. Agent is intended to be a collection of simple games that can be played by computer programs developed by people of various levels of experience. Agent vs. Agent targets small groups of people, and attempts to provide the tools necessary to conduct competitive agent vs agent combat. The ultimate goal is to help facilitate learning, whether that be through exploring new languages, learning new concepts, or smashing your co-workers egos.

# Current games

* Hearts

[![Build Status](https://travis-ci.org/phillc/agentvsagent.png)](https://travis-ci.org/phillc/agentvsagent)

# This is an alpha release

* The server isn't stable... it will leak memory (games don't clean up after them selves), it will crash when a client does something unexpected (like disconnect in the middle of a turn...), it will still assign disconnected players to games (and then crash).
* The transport layer may change. The API might change.
* I want to make the barrier to entry much lower, but I haven't tackled it yet.
* See the milestones for more information.

## Goals

* Privately deployable: decentralized for low infrastructure costs
* Support many languages: for a larger audience
* Human playable games: to be able to see how your work is doing
* Low barrier to entry: so anyone can start a server and challenge others

## Installation

* Install node: [via package manager](https://github.com/joyent/node/wiki/Installing-Node.js-via-package-manager)
* npm install -g agentvsagent
* run `ava` for command options

## Starting an agent

* Clone or download this repository.
* Copy the files in the dist/::game::/::language::/ directory

## Tips

* Basic
  * Isolate your logic to the starter agent in a different file/module, to reduce conflict if the api changes.
  * Keep your agent under source control
* Advanced
  * Unit test at least your supporting code

## Your first hearts agent

Simple heuristics should make a hearts agent very competitive. The sample agents already provide a list of cards that are valid to play, and then they choose from that list randomly.

* Your first step could be to take that list of valid cards, and sort them from low to high, and play the lowest card.
* Make another agent, and play cards from high to low and compare the results.
* Eventually, add a condition that if the Queen of Spades can be played safely, do so.
* Your next heuristic could be as simple as playing the highest card that can be played safely, or play the highest card if the trick has no points.

From there, you should be able to code against other scenarios as you observe the behaviors of your agent while playing against it in the web UI.

## Potential game ideas

* Team games: Spades, Bridge
* Variable # of players:
* Pathing: Othello (Reversi), Blokus, Tsuro, Cyclades
* Hnefatafl

## Contributing

### Mac OSX installation instructions:

* Fork project on github and clone
* run `npm install`
* run tests via `make test`

The command `make setup` has a more complete setup, including thrift

## In need of

Sample agents for languages

## Known major issues

* When a bot disconnects, the server still attempts to send them a message and will cause an exception

## Changelog

# 0.0.16

* Engine reports final score
* Engine reports moon shooting

# 0.0.15

* Fixes bug where the server would crash if an agent disconnected too early
* Fixes bug where --hearts-max-points was ignored

# 0.0.14

* Reduced number of exceptions to one

# 0.0.12

* Changed position from an enumerable to a string *turns out to be potentially backwards incompatible*

# 0.0.11

* Implemented turn time
* Nicer error messages
* Cleaned up server input
* Cleaned up Sample agents

# 0.0.10

* Implemented validation of legal plays

# 0.0.9

* Haskell bot

# 0.0.6

* Hearts human player implemented


## Credits

* Thrift: http://thrift.apache.org/
* Cards: https://github.com/selfthinker/CSS-Playing-Cards
