open Ctypes
open Core

(* https://grpc.github.io/grpc/core/grpc_8h.html#afd22cfbc549db65ee265335c3264a57b *)
module Types (F : Ctypes.TYPE) = struct
  open F

  open struct
    let enum_handler name idx = failwithf "unexpected enum value in %s: %Ld" name idx ()
    let enum_field name = constant name int64_t
    let enum' name enumr = enum ~typedef:true name ~unexpected:(enum_handler name) enumr
  end

  module Struct (N : sig
    val name : string
  end) =
  struct
    type t

    let t : t structure typ = structure N.name
  end

  module Annon_struct (N : sig
    val name : string
  end) =
  struct
    type t

    let t : t structure typ =
      let t' = structure @@ Printf.sprintf "_%s" N.name in
      typedef t' N.name
    ;;
  end

  module Union (N : sig
    val name : string
  end) =
  struct
    type t

    let t : t union typ = union N.name
  end

  module Abstruct (N : sig
    val name : string
  end) =
  struct
    type t;;

    ignore N.name

    let t = ptr void
  end

  module Common = struct
    let voidfn = static_funptr @@ void @-> returning void
  end

  type serving_status_update

  let grpc_max_completion_queue_pluckers =
    constant "GRPC_MAX_COMPLETION_QUEUE_PLUCKERS" int
  ;;

  module Slice = struct
    module Inlined = struct
      include Struct (struct
        let name = "grpc_slice_inlined"
      end)

      let grpc_slice_inline_extra_size = Ctypes.(sizeof @@ ptr void)

      let grpc_slice_inlined_size =
        Ctypes.(sizeof size_t + sizeof (ptr uint8_t) - 1 + grpc_slice_inline_extra_size)
      ;;

      let length = field t "length" uint8_t
      let bytes = field t "bytes" @@ array grpc_slice_inlined_size uint8_t
      let () = seal t
    end

    module Refcounted = struct
      include Struct (struct
        let name = "grpc_slice_refcounted"
      end)

      let length = field t "length" size_t
      let bytes = field t "bytes" @@ ptr uint8_t
      let () = seal t
    end

    module Data = struct
      type t

      let t : t union typ = union "grpc_slice_data"
      let inlined = field t "inlined" Inlined.t
      let refcounted = field t "refcounted" Refcounted.t
      let () = seal t
    end

    include Struct (struct
      let name = "grpc_slice"
    end)

    let refcount = field t "refcount" @@ ptr Refcounted.t
    let data = field t "data" Data.t
    let () = seal t

    module Buffer = struct
      open struct
        let slice_t = t
      end

      include Annon_struct (struct
        let name = "grpc_slice_buffer"
      end)

      let base_slices = field t "base_slices" @@ ptr slice_t
      let slices = field t "slices" @@ ptr slice_t
      let count = field t "count" size_t
      let capacity = field t "capacity" size_t
      let length = field t "length" size_t
      let inlined = field t "inlined" @@ array 8 slice_t
      let () = seal t
    end
  end

  module Compression = struct
    let request_algorithm_md_key =
      const "GRPC_COMPRESSION_REQUEST_ALGORITHM_MD_KEY" string
    ;;

    module Algorithm = struct
      type t =
        | NONE
        | DEFLATE
        | GZIP
        | ALGORITHMS_COUNT

      let none = enum_field "GRPC_COMPRESS_NONE"
      let deflate = enum_field "GRPC_COMPRESS_DEFLATE"
      let gzip = enum_field "GRPC_COMPRESS_GZIP"
      let algorithms_count = enum_field "GRPC_COMPRESS_ALGORITHMS_COUNT"

      let t =
        enum'
          "grpc_compression_algorithm"
          [ NONE, none; DEFLATE, deflate; GZIP, gzip; ALGORITHMS_COUNT, algorithms_count ]
      ;;
    end

    module Level = struct
      type t =
        | NONE
        | LOW
        | MED
        | HIGH
        | COUNT

      let none = enum_field "GRPC_COMPRESS_LEVEL_NONE"
      let low = enum_field "GRPC_COMPRESS_LEVEL_LOW"
      let high = enum_field "GRPC_COMPRESS_LEVEL_HIGH"
      let count = enum_field "GRPC_COMPRESS_LEVEL_COUNT"

      let t =
        enum' "grpc_compression_level" [ NONE, none; LOW, low; HIGH, high; COUNT, count ]
      ;;
    end

    module Options = struct
      module Default_algorithm = struct
        include Struct (struct
          let name = "grpc_compression_options_default_algorithm"
        end)

        let is_set = field t "is_set" int
        let algorithm = field t "algorithm" Algorithm.t
        let () = seal t
      end

      module Default_level = struct
        include Struct (struct
          let name = "grpc_compression_options_default_level"
        end)

        let is_set = field t "is_set" int
        let level = field t "level" Level.t
        let () = seal t
      end

      include Struct (struct
        let name = "grpc_compression_options"
      end)

      let default_algorithm = field t "default_algorithm" Default_algorithm.t
      let default_level = field t "default_level" Default_level.t
      let () = seal t
    end
  end

  module Byte_buffer = struct
    module Data = struct
      module Compressed = struct
        include Struct (struct
          let name = "grpc_compressed_buffer"
        end)

        let compression = field t "compression" Compression.Algorithm.t
        let slice_buffer = field t "slice_buffer" Slice.Buffer.t
        let () = seal t
      end

      include Union (struct
        let name = "grpc_byte_buffer_data"
      end)

      let reserved = field t "reserved" @@ array 8 (ptr void)
      let raw = field t "raw" Compressed.t
      let () = seal t
    end

    module Type = struct
      type t = RAW

      let raw = enum_field "GRPC_BB_RAW"
      let t = enum' "grpc_byte_buffer_type" [ RAW, raw ]
    end

    include Struct (struct
      let name = "grpc_byte_buffer"
    end)

    let reserved = field t "reserved" @@ ptr void
    let typ = field t "type" Type.t
    let data = field t "data" Data.t
    let () = seal t
  end

  (** https://grpc.github.io/grpc/core/structgrpc__metadata.html *)
  module Metadata = struct
    include Struct (struct
      let name = "grpc_metadata"
    end)

    let key = field t "key" Slice.t
    let value = field t "value" Slice.t
    let () = seal t

    module Array = struct
      include Annon_struct (struct
        let name = "grpc_metadata_array"
      end)

      let count = field t "count" size_t
      let capacity = field t "capacity" size_t
      let metadata = field t "metadata" @@ ptr t
      let () = seal t
    end
  end

  (** https://grpc.github.io/grpc/core/structgrpc__arg.html *)
  module Arg = struct
    module Type = struct
      type t =
        | STRING
        | INTEGER
        | POINTER

      let string = enum_field "GRPC_ARG_STRING"
      let integer = enum_field "GRPC_ARG_INTEGER"
      let pointer = enum_field "GRPC_ARG_POINTER"
      let t = enum' "grpc_arg_type" [ STRING, string; INTEGER, integer; POINTER, pointer ]
    end

    (** https://grpc.github.io/grpc/core/uniongrpc__arg_1_1grpc__arg__value.html *)
    module Value = struct
      module Pointer = struct
        include Struct (struct
          let name = "grpc_arg_pointer"
        end)

        let p = field t "p" @@ ptr void
        let () = seal t
      end

      (* https://grpc.github.io/grpc/core/structgrpc__arg_1_1grpc__arg__value_1_1grpc__arg__pointer.html *)
      include Union (struct
        let name = "grpc_arg_value"
      end)

      let string = field t "string" @@ string
      let integer = field t "integer" int
      let pointer = field t "pointer" @@ Pointer.t
      let () = seal t
    end

    include Annon_struct (struct
      let name = "grpc_arg"
    end)

    let typ = field t "type" Type.t
    let key = field t "key" @@ string
    let value = field t "value" Value.t
    let () = seal t

    module Pointer_vtable = struct
      include Struct (struct
        let name = "grpc_arg_pointer_vtable"
      end)

      let cmp = field t "cmp" @@ static_funptr @@ ptr void @-> ptr void @-> returning int
      let copy = field t "copy" @@ static_funptr @@ ptr void @-> returning (ptr void)

      let destroy =
        field t "destroy" @@ static_funptr @@ ptr void @-> returning (ptr void)
      ;;

      let () = seal t
    end
  end

  (** https://grpc.github.io/grpc/core/structgpr__timespec.html *)
  module Timespec = struct
    module Clock_type = struct
      type t =
        | MONOTONIC
        | REALTIME
        | PRECISE
        | TIMESPAN

      let monotonic = enum_field "GPR_CLOCK_MONOTONIC"
      let realtime = enum_field "GPR_CLOCK_REALTIME"
      let precise = enum_field "GPR_CLOCK_PRECISE"
      let timespan = enum_field "GPR_TIMESPAN"

      let t =
        enum'
          "gpr_clock_type"
          [ MONOTONIC, monotonic
          ; REALTIME, realtime
          ; PRECISE, precise
          ; TIMESPAN, timespan
          ]
      ;;
    end

    include Struct (struct
      let name = "gpr_timespec"
    end)

    let tv_sec = field t "tv_sec" @@ int64_t
    let tv_nsec = field t "tv_nsec" int32_t
    let clock_type = field t "clock_type" Clock_type.t
    let () = seal t
  end

  (** https://grpc.github.io/grpc/core/structgrpc__call__details.html *)
  module Call_details = struct
    include Annon_struct (struct
      let name = "grpc_call_details"
    end)

    let methd = field t "method" Slice.t
    let host = field t "host" Slice.t
    let deadline = field t "deadline" Timespec.t
    let flags = field t "flags" uint32_t
    let () = seal t
  end

  module Channel = struct
    module Arg = struct
      include Annon_struct (struct
        let name = "grpc_channel_args"
      end)

      let num_args = field t "num_args" size_t
      let args = field t "args" @@ ptr Arg.t
      let () = seal t
    end

    module Credentials = struct
      (* TODO: https://grpc.github.io/grpc/core/grpc__security_8h.html *)

      let t = ptr void
      (* include Abstruct (struct *)
      (* let name = "grpc_channel_credentials" *)
      (* end) *)
    end

    module Info = struct
      include Annon_struct (struct
        let name = "grpc_channel_info"
      end)

      let lb_policy_name = field t "lb_policy_name" @@ ptr string
      let service_config_json = field t "service_config_json" @@ ptr string
      let () = seal t
    end

    let t = ptr void
  end

  module Call = struct
    let t = ptr void

    module Error = struct
      type t =
        | OK
        | ERROR
        | ERROR_NOT_ON_SERVER
        | ERROR_NOT_ON_CLIENT
        | ERROR_ALREADY_ACCEPTED
        | ERROR_ALREADY_INVOKED
        | ERROR_NOT_INVOKED
        | ERROR_ALREADY_FINISHED
        | ERROR_TOO_MANY_OPERATIONS
        | ERROR_INVALID_FLAGS
        | ERROR_INVALID_METADATA
        | ERROR_INVALID_MESSAGE
        | ERROR_NOT_SERVER_COMPLETION_QUEUE
        | ERROR_BATCH_TOO_BIG
        | ERROR_PAYLOAD_TYPE_MISMATCH
        | ERROR_COMPLETION_QUEUE_SHUTDOWN

      let ok = enum_field "GRPC_CALL_OK"
      let error = enum_field "GRPC_CALL_ERROR"
      let error_not_on_server = enum_field "GRPC_CALL_ERROR_NOT_ON_SERVER"
      let error_not_on_client = enum_field "GRPC_CALL_ERROR_NOT_ON_CLIENT"
      let error_already_accepted = enum_field "GRPC_CALL_ERROR_ALREADY_ACCEPTED"
      let error_already_invoked = enum_field "GRPC_CALL_ERROR_ALREADY_INVOKED"
      let error_not_invoked = enum_field "GRPC_CALL_ERROR_NOT_INVOKED"
      let error_already_finished = enum_field "GRPC_CALL_ERROR_ALREADY_FINISHED"
      let error_too_many_operations = enum_field "GRPC_CALL_ERROR_TOO_MANY_OPERATIONS"
      let error_invalid_flags = enum_field "GRPC_CALL_ERROR_INVALID_FLAGS"
      let error_invalid_metadata = enum_field "GRPC_CALL_ERROR_INVALID_METADATA"
      let error_invalid_message = enum_field "GRPC_CALL_ERROR_INVALID_MESSAGE"

      let error_not_server_completion_queue =
        enum_field "GRPC_CALL_ERROR_NOT_SERVER_COMPLETION_QUEUE"
      ;;

      let error_batch_too_big = enum_field "GRPC_CALL_ERROR_BATCH_TOO_BIG"
      let error_payload_type_mismatch = enum_field "GRPC_CALL_ERROR_PAYLOAD_TYPE_MISMATCH"

      let error_completion_queue_shutdown =
        enum_field "GRPC_CALL_ERROR_COMPLETION_QUEUE_SHUTDOWN"
      ;;

      let t =
        enum'
          "grpc_call_error"
          [ OK, ok
          ; ERROR, error
          ; ERROR_NOT_ON_SERVER, error_not_on_server
          ; ERROR_NOT_ON_CLIENT, error_not_on_client
          ; ERROR_ALREADY_ACCEPTED, error_already_accepted
          ; ERROR_ALREADY_INVOKED, error_already_invoked
          ; ERROR_NOT_INVOKED, error_not_invoked
          ; ERROR_ALREADY_FINISHED, error_already_finished
          ; ERROR_TOO_MANY_OPERATIONS, error_too_many_operations
          ; ERROR_INVALID_FLAGS, error_invalid_flags
          ; ERROR_INVALID_METADATA, error_invalid_metadata
          ; ERROR_INVALID_MESSAGE, error_invalid_message
          ; ERROR_NOT_SERVER_COMPLETION_QUEUE, error_not_server_completion_queue
          ; ERROR_BATCH_TOO_BIG, error_batch_too_big
          ; ERROR_PAYLOAD_TYPE_MISMATCH, error_payload_type_mismatch
          ; ERROR_COMPLETION_QUEUE_SHUTDOWN, error_completion_queue_shutdown
          ]
      ;;
    end
  end

  module Completion = struct
    module Type = struct
      type t =
        | QUEUE_SHUTDOWN
        | QUEUE_TIMEOUT
        | OP_COMPLETE

      let queue_shutdown = enum_field "GRPC_QUEUE_SHUTDOWN"
      let queue_timeout = enum_field "GRPC_QUEUE_TIMEOUT"
      let op_complete = enum_field "GRPC_OP_COMPLETE"

      let t =
        enum'
          "grpc_completion_type"
          [ QUEUE_SHUTDOWN, queue_shutdown
          ; QUEUE_TIMEOUT, queue_timeout
          ; OP_COMPLETE, op_complete
          ]
      ;;
    end

    module Queue = struct
      let t = ptr void
      (* include Abstruct (struct *)
      (* let name = "grpc_completion_queue" *)
      (* end) *)

      module Attributes = struct
        let t = ptr void
      end
      (* Abstruct (struct *)
      (* let name = "grpc_completion_queue_attributes" *)
      (* end) *)

      module Factory = struct
        let t = ptr void
      end
      (* Abstruct (struct *)
      (* let name = "grpc_completion_queue_factory" *)
      (* end) *)

      module Functor = struct
        include Struct (struct
          let name = "grpc_completion_queue_functor"
        end)

        let functor_run =
          field t "functor_run" @@ static_funptr @@ ptr t @-> int @-> returning void
        ;;

        let inlineable = field t "inlineable" int
        let internal_success = field t "internal_success" int
        let internal_next = field t "internal_next" @@ ptr t
        let () = seal t
      end

      module Type = struct
        let t = ptr void
      end
      (* Abstruct (struct *)
      (* let name = "grpc_completion_queue_type" *)
      (* end) *)
    end
  end

  module CQ_types = struct
    module Completion = struct
      type t =
        | NEXT
        | PLUCK
        | CALLBACK

      let next = enum_field "GRPC_CQ_NEXT"
      let pluck = enum_field "GRPC_CQ_PLUCK"
      let callback = enum_field "GRPC_CQ_CALLBACK"

      let t =
        enum' "grpc_cq_completion_type" [ NEXT, next; PLUCK, pluck; CALLBACK, callback ]
      ;;
    end

    module Polling = struct
      type t =
        | DEFAULT_POLLING
        | NON_LISTENING
        | NON_POLLING

      let default_polling = enum_field "GRPC_CQ_DEFAULT_POLLING"
      let non_listening = enum_field "GRPC_CQ_NON_LISTENING"
      let non_polling = enum_field "GRPC_CQ_NON_POLLING"

      let t =
        enum'
          "grpc_cq_polling_type"
          [ DEFAULT_POLLING, default_polling
          ; NON_LISTENING, non_listening
          ; NON_POLLING, non_polling
          ]
      ;;
    end
  end

  module Event = struct
    include Struct (struct
      let name = "grpc_event"
    end)

    let typ = field t "type" Completion.Type.t
    let success = field t "success" int
    let tag = field t "tag" @@ ptr void
    let () = seal t
  end

  module Op = struct
    let t = ptr void
    (* include Abstruct (struct *)
    (* let name = "grpc_op" *)
    (* end) *)

    module Type = struct
      type t =
        | SEND_INITIAL_METADATA
        | SEND_MESSAGE
        | SEND_CLOSE_FROM_CLIENT
        | SEND_STATUS_FROM_SERVER
        | RECV_INITIAL_METADATA
        | RECV_MESSAGE
        | RECV_STATUS_ON_CLIENT
        | RECV_CLOSE_ON_SERVER

      let send_initial_metadata = enum_field "GRPC_OP_SEND_INITIAL_METADATA"
      let send_message = enum_field "GRPC_OP_SEND_MESSAGE"
      let send_close_from_client = enum_field "GRPC_OP_SEND_CLOSE_FROM_CLIENT"
      let send_status_from_server = enum_field "GRPC_OP_SEND_STATUS_FROM_SERVER"
      let recv_initial_metadata = enum_field "GRPC_OP_RECV_INITIAL_METADATA"
      let recv_message = enum_field "GRPC_OP_RECV_MESSAGE"
      let recv_status_on_client = enum_field "GRPC_OP_RECV_STATUS_ON_CLIENT"
      let recv_close_on_server = enum_field "GRPC_OP_RECV_CLOSE_ON_SERVER"

      let t =
        enum'
          "grpc_op_type"
          [ SEND_INITIAL_METADATA, send_initial_metadata
          ; SEND_MESSAGE, send_message
          ; SEND_CLOSE_FROM_CLIENT, send_close_from_client
          ; SEND_STATUS_FROM_SERVER, send_status_from_server
          ; RECV_INITIAL_METADATA, recv_initial_metadata
          ; RECV_MESSAGE, recv_message
          ; RECV_STATUS_ON_CLIENT, recv_status_on_client
          ; RECV_CLOSE_ON_SERVER, recv_close_on_server
          ]
      ;;
    end
  end

  module Socket = struct
    module Factorty = struct
      let t = ptr void
    end
    (* Abstruct (struct *)
    (* let name = "grpc_socket_factory" *)
    (* end) *)

    module Mutator = struct
      let t = ptr void
    end
    (* Abstruct (struct *)
    (* let name = "grpc_socket_factory" *)
    (* end) *)
  end

  module Connectivity_state = struct
    type t =
      | IDLE
      | CONNECTING
      | READY
      | TRANSIENT_FAILURE
      | SHUTDOWN

    let idle = enum_field "GRPC_CHANNEL_IDLE"
    let connecting = enum_field "GRPC_CHANNEL_CONNECTING"
    let ready = enum_field "GRPC_CHANNEL_READY"
    let transient_failure = enum_field "GRPC_CHANNEL_TRANSIENT_FAILURE"
    let shutdown = enum_field "GRPC_CHANNEL_SHUTDOWN"

    let t =
      enum'
        "grpc_connectivity_state"
        [ IDLE, idle
        ; CONNECTING, connecting
        ; READY, ready
        ; TRANSIENT_FAILURE, transient_failure
        ; SHUTDOWN, shutdown
        ]
    ;;
  end

  module Status_code = struct
    type t =
      | OK
      | CANCELLED
      | UNKNOWN
      | INVALID_ARGUMENT
      | DEADLINE_EXCEEDED
      | NOT_FOUND
      | ALREADY_EXISTS
      | PERMISSION_DENIED
      | UNAUTHENTICATED
      | RESOURCE_EXHAUSTED
      | FAILED_PRECONDITION
      | ABORTED
      | OUT_OF_RANGE
      | UNIMPLEMENTED
      | INTERNAL
      | UNAVAILABLE
      | DATA_LOSS
      | DO_NOT_USE

    let fld name = enum_field @@ Printf.sprintf "GRPC_STATUS_%s" name
    let ok = fld "OK"
    let cancelled = fld "CANCELLED"
    let unknown = fld "UNKNOWN"
    let invalid_argument = fld "INVALID_ARGUMENT"
    let deadline_exceeded = fld "DEADLINE_EXCEEDED"
    let not_found = fld "NOT_FOUND"
    let already_exists = fld "ALREADY_EXISTS"
    let permission_denied = fld "PERMISSION_DENIED"
    let unauthenticated = fld "UNAUTHENTICATED"
    let resource_exhausted = fld "RESOURCE_EXHAUSTED"
    let failed_precondition = fld "FAILED_PRECONDITION"
    let aborted = fld "ABORTED"
    let out_of_range = fld "OUT_OF_RANGE"
    let unimplemented = fld "UNIMPLEMENTED"
    let internal = fld "INTERNAL"
    let unavailable = fld "UNAVAILABLE"
    let data_loss = fld "DATA_LOSS"
    let _do_not_use = fld "_DO_NOT_USE"

    let t =
      enum'
        "grpc_status_code"
        [ OK, ok
        ; CANCELLED, cancelled
        ; UNKNOWN, unknown
        ; INVALID_ARGUMENT, invalid_argument
        ; DEADLINE_EXCEEDED, deadline_exceeded
        ; NOT_FOUND, not_found
        ; ALREADY_EXISTS, already_exists
        ; PERMISSION_DENIED, permission_denied
        ; UNAUTHENTICATED, unauthenticated
        ; RESOURCE_EXHAUSTED, resource_exhausted
        ; FAILED_PRECONDITION, failed_precondition
        ; ABORTED, aborted
        ; OUT_OF_RANGE, out_of_range
        ; UNIMPLEMENTED, unimplemented
        ; INTERNAL, internal
        ; UNAVAILABLE, unavailable
        ; DATA_LOSS, data_loss
        ; DO_NOT_USE, _do_not_use
        ]
    ;;
  end

  module Serving_status_update = struct
    include Annon_struct (struct
      let name = "grpc_serving_status_update"
    end)

    let code = field t "code" Status_code.t
    let error_message = field t "error_message" string
    let () = seal t
  end

  module Census_context = struct
    let t = ptr void
  end

  module Resource_quota = struct
    let t = ptr void
  end

  module Server = struct
    let t = ptr void

    module Register_method_payload_handling = struct
      type t =
        | NONE
        | READ_INITIAL_BYTE_BUFFER

      let none = enum_field "GRPC_SRM_PAYLOAD_NONE"

      let read_initial_byte_buffer =
        enum_field "GRPC_SRM_PAYLOAD_READ_INITIAL_BYTE_BUFFER"
      ;;

      let t =
        enum'
          "grpc_server_register_method_payload_handling"
          [ NONE, none; READ_INITIAL_BYTE_BUFFER, read_initial_byte_buffer ]
      ;;
    end

    module Config_fetcher = struct
      let t = ptr void
    end
    (* Abstruct (struct *)
    (* let name = "grpc_server_config_fetcher" *)
    (* end) *)

    module Credentials = struct
      let t = ptr void
    end
    (* Abstruct (struct *)
    (* let name = "grpc_server_credentials" *)
    (* end) *)

    module Xds_status_notifier = struct
      include Annon_struct (struct
        let name = "grpc_server_xds_status_notifier"
      end)

      let on_serving_status_update =
        field t "on_serving_status_update"
        @@ static_funptr
        @@ ptr void
        @-> string
        (* @-> Serving_status_update.t *)
        @-> returning void
      ;;

      let user_data = field t "user_data" @@ ptr void
      let () = seal t
    end
  end
end
