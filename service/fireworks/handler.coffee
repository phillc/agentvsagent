logger = require '../../lib/logger'
{EventEmitter} = require 'events'

module.exports = class Handler extends EventEmitter
