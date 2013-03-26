angular.module("AgentVsAgent.filters", [])
  .filter "cardSuitClass", ->
    (input) ->
      switch input
        when "clubs" then "clubs"

  .filter "cardRankClass", ->
    (input) ->
      "rank-#{input}".toLowerCase()

  .filter "cardSuitSymbol", ->
    (input) ->
      switch input
        when "clubs" then "&clubs;"
