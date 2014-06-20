require "./game"

Game.do_move do |game|
  game.log "HEY:"
  game.log game.moves
  game.log game.hands

  playables = game.hands.each.with_object({}) do |(position, cards), acc|
    playable_cards = cards.select do |card|
      card["rank"] == game.piles[card["suit"]] + 1
    end
    acc[position] = playable_cards unless playable_cards.empty?
  end

  clear_hints = playables.each.with_object([]) do |(position, cards), acc|
    clear_cards_by_rank = cards.select do |card|
      game.hands[position].select{|c| c["rank"] == card["rank"] }.size == 1
    end
    clear_cards_by_suit = cards.select do |card|
      game.hands[position].select{|c| c["suit"] == card["suit"] }.size == 1
    end

    clear_cards_by_rank.each do |card|
      acc << { position: position, rank: card["rank"] }
    end
    clear_cards_by_suit.each do |card|
      acc << { position: position, suit: card["suit"] }
    end
  end

  puts ">>>>> clear hints"
  puts clear_hints.inspect
  puts "---"

  if clear_hints.any?
    hint = clear_hints.shuffle.first

    puts "hint>>>"
    puts hint
    { hint: hint }
  else
    puts "discarding..."
    { discard: 0 }
  end
end

Game.play
