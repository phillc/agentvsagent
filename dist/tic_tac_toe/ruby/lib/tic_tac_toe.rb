#
# Autogenerated by Thrift Compiler (0.9.0)
#
# DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
#

require 'thrift'
require 'tic_tac_toe_types'

module AgentVsAgent
  module TicTacToe
    class Client
      include ::Thrift::Client

      def enter_arena(request)
        send_enter_arena(request)
        return recv_enter_arena()
      end

      def send_enter_arena(request)
        send_message('enter_arena', Enter_arena_args, :request => request)
      end

      def recv_enter_arena()
        result = receive_message(Enter_arena_result)
        return result.success unless result.success.nil?
        raise ::Thrift::ApplicationException.new(::Thrift::ApplicationException::MISSING_RESULT, 'enter_arena failed: unknown result')
      end

      def get_game_info(ticket)
        send_get_game_info(ticket)
        return recv_get_game_info()
      end

      def send_get_game_info(ticket)
        send_message('get_game_info', Get_game_info_args, :ticket => ticket)
      end

      def recv_get_game_info()
        result = receive_message(Get_game_info_result)
        return result.success unless result.success.nil?
        raise result.ex1 unless result.ex1.nil?
        raise ::Thrift::ApplicationException.new(::Thrift::ApplicationException::MISSING_RESULT, 'get_game_info failed: unknown result')
      end

      def make_move(ticket, coordinates)
        send_make_move(ticket, coordinates)
        return recv_make_move()
      end

      def send_make_move(ticket, coordinates)
        send_message('make_move', Make_move_args, :ticket => ticket, :coordinates => coordinates)
      end

      def recv_make_move()
        result = receive_message(Make_move_result)
        return result.success unless result.success.nil?
        raise result.ex1 unless result.ex1.nil?
        raise ::Thrift::ApplicationException.new(::Thrift::ApplicationException::MISSING_RESULT, 'make_move failed: unknown result')
      end

      def get_game_result(ticket)
        send_get_game_result(ticket)
        return recv_get_game_result()
      end

      def send_get_game_result(ticket)
        send_message('get_game_result', Get_game_result_args, :ticket => ticket)
      end

      def recv_get_game_result()
        result = receive_message(Get_game_result_result)
        return result.success unless result.success.nil?
        raise result.ex2 unless result.ex2.nil?
        raise ::Thrift::ApplicationException.new(::Thrift::ApplicationException::MISSING_RESULT, 'get_game_result failed: unknown result')
      end

    end

    class Processor
      include ::Thrift::Processor

      def process_enter_arena(seqid, iprot, oprot)
        args = read_args(iprot, Enter_arena_args)
        result = Enter_arena_result.new()
        result.success = @handler.enter_arena(args.request)
        write_result(result, oprot, 'enter_arena', seqid)
      end

      def process_get_game_info(seqid, iprot, oprot)
        args = read_args(iprot, Get_game_info_args)
        result = Get_game_info_result.new()
        begin
          result.success = @handler.get_game_info(args.ticket)
        rescue ::AgentVsAgent::GameAbortedException => ex1
          result.ex1 = ex1
        end
        write_result(result, oprot, 'get_game_info', seqid)
      end

      def process_make_move(seqid, iprot, oprot)
        args = read_args(iprot, Make_move_args)
        result = Make_move_result.new()
        begin
          result.success = @handler.make_move(args.ticket, args.coordinates)
        rescue ::AgentVsAgent::GameAbortedException => ex1
          result.ex1 = ex1
        end
        write_result(result, oprot, 'make_move', seqid)
      end

      def process_get_game_result(seqid, iprot, oprot)
        args = read_args(iprot, Get_game_result_args)
        result = Get_game_result_result.new()
        begin
          result.success = @handler.get_game_result(args.ticket)
        rescue ::AgentVsAgent::GameAbortedException => ex2
          result.ex2 = ex2
        end
        write_result(result, oprot, 'get_game_result', seqid)
      end

    end

    # HELPER FUNCTIONS AND STRUCTURES

    class Enter_arena_args
      include ::Thrift::Struct, ::Thrift::Struct_Union
      REQUEST = 1

      FIELDS = {
        REQUEST => {:type => ::Thrift::Types::STRUCT, :name => 'request', :class => ::AgentVsAgent::EntryRequest}
      }

      def struct_fields; FIELDS; end

      def validate
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field request is unset!') unless @request
      end

      ::Thrift::Struct.generate_accessors self
    end

    class Enter_arena_result
      include ::Thrift::Struct, ::Thrift::Struct_Union
      SUCCESS = 0

      FIELDS = {
        SUCCESS => {:type => ::Thrift::Types::STRUCT, :name => 'success', :class => ::AgentVsAgent::EntryResponse}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      ::Thrift::Struct.generate_accessors self
    end

    class Get_game_info_args
      include ::Thrift::Struct, ::Thrift::Struct_Union
      TICKET = 1

      FIELDS = {
        TICKET => {:type => ::Thrift::Types::STRUCT, :name => 'ticket', :class => ::AgentVsAgent::Ticket}
      }

      def struct_fields; FIELDS; end

      def validate
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field ticket is unset!') unless @ticket
      end

      ::Thrift::Struct.generate_accessors self
    end

    class Get_game_info_result
      include ::Thrift::Struct, ::Thrift::Struct_Union
      SUCCESS = 0
      EX1 = 1

      FIELDS = {
        SUCCESS => {:type => ::Thrift::Types::STRUCT, :name => 'success', :class => ::AgentVsAgent::GameInfo},
        EX1 => {:type => ::Thrift::Types::STRUCT, :name => 'ex1', :class => ::AgentVsAgent::GameAbortedException}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      ::Thrift::Struct.generate_accessors self
    end

    class Make_move_args
      include ::Thrift::Struct, ::Thrift::Struct_Union
      TICKET = 1
      COORDINATES = 2

      FIELDS = {
        TICKET => {:type => ::Thrift::Types::STRUCT, :name => 'ticket', :class => ::AgentVsAgent::Ticket},
        COORDINATES => {:type => ::Thrift::Types::LIST, :name => 'coordinates', :element => {:type => ::Thrift::Types::I32}}
      }

      def struct_fields; FIELDS; end

      def validate
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field ticket is unset!') unless @ticket
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field coordinates is unset!') unless @coordinates
      end

      ::Thrift::Struct.generate_accessors self
    end

    class Make_move_result
      include ::Thrift::Struct, ::Thrift::Struct_Union
      SUCCESS = 0
      EX1 = 1

      FIELDS = {
        SUCCESS => {:type => ::Thrift::Types::STRUCT, :name => 'success', :class => ::AgentVsAgent::MoveResult},
        EX1 => {:type => ::Thrift::Types::STRUCT, :name => 'ex1', :class => ::AgentVsAgent::GameAbortedException}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      ::Thrift::Struct.generate_accessors self
    end

    class Get_game_result_args
      include ::Thrift::Struct, ::Thrift::Struct_Union
      TICKET = 1

      FIELDS = {
        TICKET => {:type => ::Thrift::Types::STRUCT, :name => 'ticket', :class => ::AgentVsAgent::Ticket}
      }

      def struct_fields; FIELDS; end

      def validate
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Required field ticket is unset!') unless @ticket
      end

      ::Thrift::Struct.generate_accessors self
    end

    class Get_game_result_result
      include ::Thrift::Struct, ::Thrift::Struct_Union
      SUCCESS = 0
      EX2 = 1

      FIELDS = {
        SUCCESS => {:type => ::Thrift::Types::STRUCT, :name => 'success', :class => ::AgentVsAgent::GameResult},
        EX2 => {:type => ::Thrift::Types::STRUCT, :name => 'ex2', :class => ::AgentVsAgent::GameAbortedException}
      }

      def struct_fields; FIELDS; end

      def validate
      end

      ::Thrift::Struct.generate_accessors self
    end

  end

end
