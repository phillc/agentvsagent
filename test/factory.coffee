Arena = require "../src/server/arena"
HeartsBuilder = require "../src/server/hearts/builder"
Agent = require "../src/server/agent"
und = require 'underscore'

exports.createArena = createArena = (options={}) ->
  builder = options.builder || new HeartsBuilder()
  new Arena(builder, [])

exports.createGame = createGame = (options={}) ->
  arena = options.arena || createArena(options)

  players = und.map([0..arena.numberOfPlayers], -> arena.createPlayer())

  game = arena.createGame(players)

  game

exports.createAgent = createAgent = (options={}) ->
  new Agent()
