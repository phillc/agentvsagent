Arena = require '../../../hearts/arena'
IdGenerator = require '../../../hearts/idgenerator'
Handler = require '../../../hearts/service/handler'
require("chai").should()

describe "Handler", ->
  beforeEach ->
    @arena = new Arena()
    @handler = new Handler(@arena)

  describe "#enter_arena", ->
    # it "does not blow up", (done) ->
    #   @handler.enter_arena done
