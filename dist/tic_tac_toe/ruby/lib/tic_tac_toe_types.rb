#
# Autogenerated by Thrift Compiler (0.9.0)
#
# DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
#

require 'thrift'

module AgentVsAgent
  module Position
    X = 1
    O = 2
    VALUE_MAP = {1 => "X", 2 => "O"}
    VALID_VALUES = Set.new([X, O]).freeze
  end

  module GameStatus
    NEXT_MOVE = 1
    END_GAME = 2
    VALUE_MAP = {1 => "NEXT_MOVE", 2 => "END_GAME"}
    VALID_VALUES = Set.new([NEXT_MOVE, END_GAME]).freeze
  end

  class EntryRequest
    include ::Thrift::Struct, ::Thrift::Struct_Union
    VERSION = 1

    FIELDS = {
      VERSION => {:type => ::Thrift::Types::STRING, :name => 'version', :default => %q"0.0.11"}
    }

    def struct_fields; FIELDS; end

    def validate
      raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field version is unset!') unless @version
    end

    ::Thrift::Struct.generate_accessors self
  end

  class Ticket
    include ::Thrift::Struct, ::Thrift::Struct_Union
    GAMEID = 1
    AGENTID = 2

    FIELDS = {
      GAMEID => {:type => ::Thrift::Types::STRING, :name => 'gameId'},
      AGENTID => {:type => ::Thrift::Types::STRING, :name => 'agentId'}
    }

    def struct_fields; FIELDS; end

    def validate
      raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field gameId is unset!') unless @gameId
      raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field agentId is unset!') unless @agentId
    end

    ::Thrift::Struct.generate_accessors self
  end

  class EntryResponse
    include ::Thrift::Struct, ::Thrift::Struct_Union
    TICKET = 1
    MESSAGE = 2

    FIELDS = {
      TICKET => {:type => ::Thrift::Types::STRUCT, :name => 'ticket', :class => ::AgentVsAgent::Ticket, :optional => true},
      MESSAGE => {:type => ::Thrift::Types::STRING, :name => 'message', :optional => true}
    }

    def struct_fields; FIELDS; end

    def validate
    end

    ::Thrift::Struct.generate_accessors self
  end

  class GameInfo
    include ::Thrift::Struct, ::Thrift::Struct_Union
    POSITION = 1

    FIELDS = {
      POSITION => {:type => ::Thrift::Types::I32, :name => 'position', :enum_class => ::AgentVsAgent::Position}
    }

    def struct_fields; FIELDS; end

    def validate
      raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field position is unset!') unless @position
      unless @position.nil? || ::AgentVsAgent::Position::VALID_VALUES.include?(@position)
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Invalid value of field position!')
      end
    end

    ::Thrift::Struct.generate_accessors self
  end

  class GameResult
    include ::Thrift::Struct, ::Thrift::Struct_Union
    WINNER = 1

    FIELDS = {
      WINNER => {:type => ::Thrift::Types::I32, :name => 'winner', :enum_class => ::AgentVsAgent::Position}
    }

    def struct_fields; FIELDS; end

    def validate
      raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field winner is unset!') unless @winner
      unless @winner.nil? || ::AgentVsAgent::Position::VALID_VALUES.include?(@winner)
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Invalid value of field winner!')
      end
    end

    ::Thrift::Struct.generate_accessors self
  end

  class MoveResult
    include ::Thrift::Struct, ::Thrift::Struct_Union
    OPPONENT = 1
    STATUS = 2

    FIELDS = {
      OPPONENT => {:type => ::Thrift::Types::LIST, :name => 'opponent', :element => {:type => ::Thrift::Types::I32}},
      STATUS => {:type => ::Thrift::Types::I32, :name => 'status', :enum_class => ::AgentVsAgent::GameStatus}
    }

    def struct_fields; FIELDS; end

    def validate
      raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field opponent is unset!') unless @opponent
      raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field status is unset!') unless @status
      unless @status.nil? || ::AgentVsAgent::GameStatus::VALID_VALUES.include?(@status)
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Invalid value of field status!')
      end
    end

    ::Thrift::Struct.generate_accessors self
  end

  class GameAbortedException < ::Thrift::Exception
    include ::Thrift::Struct, ::Thrift::Struct_Union
    def initialize(message=nil)
      super()
      self.message = message
    end

    MESSAGE = 1

    FIELDS = {
      MESSAGE => {:type => ::Thrift::Types::STRING, :name => 'message'}
    }

    def struct_fields; FIELDS; end

    def validate
      raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field message is unset!') unless @message
    end

    ::Thrift::Struct.generate_accessors self
  end

end
