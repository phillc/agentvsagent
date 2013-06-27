angular.module("AgentVsAgent.filters", [])
  .filter "cardSuitClass", ->
    (input) ->
      switch input
        when AgentVsAgent.Suit.CLUBS then "clubs"
        when AgentVsAgent.Suit.DIAMONDS then "diams"
        when AgentVsAgent.Suit.SPADES then "spades"
        when AgentVsAgent.Suit.HEARTS then "hearts"

  .filter "rankName", ->
    (input) ->
      switch input
        when AgentVsAgent.Rank.TWO then "2"
        when AgentVsAgent.Rank.THREE then "3"
        when AgentVsAgent.Rank.FOUR then "4"
        when AgentVsAgent.Rank.FIVE then "5"
        when AgentVsAgent.Rank.SIX then "6"
        when AgentVsAgent.Rank.SEVEN then "7"
        when AgentVsAgent.Rank.EIGHT then "8"
        when AgentVsAgent.Rank.NINE then "9"
        when AgentVsAgent.Rank.TEN then "10"
        when AgentVsAgent.Rank.JACK then "j"
        when AgentVsAgent.Rank.QUEEN then "q"
        when AgentVsAgent.Rank.KING then "k"
        when AgentVsAgent.Rank.ACE then "a"
