Arena = require "../lib/arena"
HeartsFactory = require "../lib/hearts/factory"
und = require 'underscore'

exports.createArena = createArena = (options={}) ->
  factory = options.factory || new HeartsFactory()
  new Arena(factory)

exports.createGame = createGame = (options={}) ->
  arena = options.arena || createArena(options)

  players = und.map([0..arena.numberOfPlayers], -> arena.createPlayer())

  game = arena.createGame(players)

  game

