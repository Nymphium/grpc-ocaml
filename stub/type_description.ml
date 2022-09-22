open Ctypes

(* https://grpc.github.io/grpc/core/grpc_8h.html#afd22cfbc549db65ee265335c3264a57b *)
module Types (F : Ctypes.TYPE) = struct
  open F
  module F = F

  open struct
    let enum_handler name idx =
      failwith @@ Printf.sprintf "unexpected enum value in %s: %Ld" name idx
    ;;

    let enum_field' prefix name = constant (Printf.sprintf "%s_%s" prefix name) int64_t
    let enum_field name = constant name int64_t
    let enum' name enumr = enum ~typedef:true name ~unexpected:(enum_handler name) enumr

    (** Helper module for "tagged" struct:
   ```c
   typedef struct foo {
     int bar;
   } foo
   ```

   should be encoded as:

   ```ocaml
   module Foo = struct
     include Struct(struct let name = "foo" end)
     let bar = "bar" <-. int
     let () = seal t
   end
   ```
    **)
    module Struct (N : sig
      val name : string
    end) =
    struct
      type t

      let t : t structure typ = structure N.name
      let ( <-. ) name ty = field t name ty
    end

    module Anon_struct (N : sig
      val name : string
    end) =
    struct
      type t

      let (t : t structure typ), anon =
        let t' = structure @@ Printf.sprintf "__anon_%s" N.name in
        typedef t' N.name, t'
      ;;

      let ( <-. ) name ty = field t name ty
    end

    module Union (N : sig
      val name : string
    end) =
    struct
      type t

      let t : t union typ = union N.name
      let ( <-. ) name ty = field t name ty
    end

    module Abs (N : sig end) =
    (* : sig *)
    (* type t *)

    (* val t : t typ *)
    (* end *)
    struct
      type t = unit ptr

      let t : t typ = ptr void
    end
  end

  module Common = struct
    let voidfn = static_funptr @@ void @-> returning void
  end

  type serving_status_update

  let grpc_max_completion_queue_pluckers =
    constant "GRPC_MAX_COMPLETION_QUEUE_PLUCKERS" int64_t
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

      let length = "length" <-. uint8_t

      (** XXX: precise data size *)
      let bytes = "bytes" <-. ptr uint8_t

      (* let bytes = "bytes" <-. array grpc_slice_inlined_size uint8_t *)
      let () = seal t
    end

    module Refcounted = struct
      include Struct (struct
        let name = "grpc_slice_refcounted"
      end)

      let length = "length" <-. size_t
      let bytes = "bytes" <-. ptr uint8_t
      let () = seal t
    end

    module Data = struct
      include Union (struct
        let name = "grpc_slice_data"
      end)

      let inlined = "inlined" <-. Inlined.t
      let refcounted = "refcounted" <-. Refcounted.t
      let () = seal t
    end

    module Ref_whom = struct
      type t =
        | TAIL
        | HEAD
        | BOTH
      [@@deriving show { with_path = false }]

      let enum_field = enum_field' "GRPC_SLICE_REF"
      let tail = enum_field "TAIL"
      let head = enum_field "HEAD"
      let both = enum_field "BOTH"
      let t = enum' "grpc_slice_ref_whom" [ TAIL, tail; HEAD, head; BOTH, both ]
    end

    include Struct (struct
      let name = "grpc_slice"
    end)

    let refcount = "refcount" <-. Refcounted.t
    let data = "data" <-. Data.t
    let () = seal t

    module Buffer = struct
      open struct
        let slice_t = t
      end

      include Struct (struct
        let name = "grpc_slice_buffer"
      end)

      let base_slices = "base_slices" <-. ptr slice_t
      let slices = "slices" <-. ptr slice_t
      let count = "count" <-. size_t
      let capacity = "capacity" <-. size_t
      let length = "length" <-. size_t
      let inlined = "inlined" <-. array 8 slice_t
      let () = seal t
    end
  end

  module Compression = struct
    module Algorithm = struct
      type t =
        | NONE
        | DEFLATE
        | GZIP
        | ALGORITHMS_COUNT
      [@@deriving show { with_path = false }]

      let enum_field = enum_field' "GRPC_COMPRESS"
      let none = enum_field "NONE"
      let deflate = enum_field "DEFLATE"
      let gzip = enum_field "GZIP"
      let algorithms_count = enum_field "ALGORITHMS_COUNT"

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
      [@@deriving show { with_path = false }]

      let enum_field = enum_field' "GRPC_COMPRESS_LEVEL"
      let none = enum_field "NONE"
      let low = enum_field "LOW"
      let high = enum_field "HIGH"
      let count = enum_field "COUNT"

      let t =
        enum' "grpc_compression_level" [ NONE, none; LOW, low; HIGH, high; COUNT, count ]
      ;;
    end

    module Options = struct
      module Default_algorithm = struct
        include Struct (struct
          let name = "grpc_compression_options_default_algorithm"
        end)

        let is_set = "is_set" <-. int
        let algorithm = "algorithm" <-. Algorithm.t
        let () = seal t
      end

      module Default_level = struct
        include Struct (struct
          let name = "grpc_compression_options_default_level"
        end)

        let is_set = "is_set" <-. int
        let level = "level" <-. Level.t
        let () = seal t
      end

      include Struct (struct
        let name = "grpc_compression_options"
      end)

      let default_algorithm = "default_algorithm" <-. Default_algorithm.t
      let default_level = "default_level" <-. Default_level.t
      let () = seal t
    end
  end

  module Byte_buffer = struct
    module Data = struct
      module Compressed = struct
        include Struct (struct
          let name = "grpc_compressed_buffer"
        end)

        let compression = "compression" <-. Compression.Algorithm.t
        let slice_buffer = "slice_buffer" <-. Slice.Buffer.t
        let () = seal t
      end

      include Union (struct
        let name = "grpc_byte_buffer_data"
      end)

      let reserved = "reserved" <-. array 8 (ptr void)
      let raw = "raw" <-. Compressed.t
      let () = seal t
    end

    module Type = struct
      type t = RAW [@@deriving show { with_path = false }]

      let raw = enum_field "GRPC_BB_RAW"
      let t = enum' "grpc_byte_buffer_type" [ RAW, raw ]
    end

    include Struct (struct
      let name = "grpc_byte_buffer"
    end)

    let reserved = "reserved" <-. ptr void
    let typ = "type" <-. Type.t
    let data = "data" <-. Data.t
    let () = seal t

    module Reader = struct
      module Current = struct
        include Union (struct
          let name = "grpc_byte_buffer_reader_current"
        end)

        let index = "index" <-. uint
        let () = seal t
      end

      open struct
        let u = t
      end

      include Struct (struct
        let name = "grpc_byte_buffer_reader"
      end)

      let buffer_in = "buffer_in" <-. ptr u
      let buffer_out = "buffer_out" <-. ptr u
      let current = "current" <-. Current.t
      let () = seal t
    end
  end

  (** https://grpc.github.io/grpc/core/structgrpc__metadata.html *)
  module Metadata = struct
    include Struct (struct
      let name = "grpc_metadata"
    end)

    let value = "value" <-. Slice.t
    let key = "key" <-. Slice.t
    let internal_data = "internal_data" <-. array 4 @@ ptr void
    let () = seal t

    module Array = struct
      open struct
        let md = t
      end

      include Anon_struct (struct
        let name = "grpc_metadata_array"
      end)

      let metadata = "metadata" <-. ptr md
      let capacity = "capacity" <-. size_t
      let count = "count" <-. size_t
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
      [@@deriving show { with_path = false }]

      let enum_field = enum_field' "GRPC_ARG"
      let string = enum_field "STRING"
      let integer = enum_field "INTEGER"
      let pointer = enum_field "POINTER"
      let t = enum' "grpc_arg_type" [ STRING, string; INTEGER, integer; POINTER, pointer ]
    end

    (** @see https://grpc.github.io/grpc/core/group__grpc__arg__keys.html *)
    module Key = struct
      let c name = constant name string

      (* let arrow_reuseport = c "GRPC_ARG_ALLOW_REUSEPORT" *)
      (* let authorization_policy_provider = c "GRPC_ARG_AUTHORIZATION_POLICY_PROVIDER" *)
      (* let client_idele_timeout_ms = c "GRPC_ARG_CLIENT_IDLE_TIMEOUT_MS" *)
      (* let default_authority = c "GRPC_ARG_DEFAULT_AUTHORITY" *)
      (* let disable_client_authority_filter = c "GRPC_ARG_DISABLE_CLIENT_AUTHORITY_FILTER" *)
      (* let dns_ares_query_timeout_ms = c "GRPC_ARG_DNS_ARES_QUERY_TIMEOUT_MS" *)
      (* let dns_enable_srv_queries = c "GRPC_ARG_DNS_ENABLE_SRV_QUERIES" *)

      (* let dns_min_time_between_resolutions_ms = *)
      (* c "GRPC_ARG_DNS_MIN_TIME_BETWEEN_RESOLUTIONS_MS" *)
      (* ;; *)

      (* let enable_census = c "GRPC_ARG_ENABLE_CENSUS" *)
      (* let enable_channelz = c "GRPC_ARG_ENABLE_CHANNELZ" *)
      (* let enable_deadline_checks = c "GRPC_ARG_ENABLE_DEADLINE_CHECKS" *)
      (* let enable_http_proxy = c "GRPC_ARG_ENABLE_HTTP_PROXY" *)
      (* let enable_load_reporting = c "GRPC_ARG_ENABLE_LOAD_REPORTING" *)
      (* let enable_per_message_compression = c "GRPC_ARG_ENABLE_PER_MESSAGE_COMPRESSION" *)
      (* let enable_per_message_decompression = c "GRPC_ARG_ENABLE_PER_MESSAGE_DECOMPRESSION" *)
      (* let enable_retries = c "GRPC_ARG_ENABLE_RETRIES" *)
      (* let expand_wildcard_addrs = c "GRPC_ARG_EXPAND_WILDCARD_ADDRS" *)
    end

    (** https://grpc.github.io/grpc/core/uniongrpc__arg_1_1grpc__arg__value.html *)
    module Value = struct
      module Pointer = struct
        include Struct (struct
          let name = "grpc_arg_pointer"
        end)

        let p = "p" <-. ptr void
        let () = seal t
      end

      (* https://grpc.github.io/grpc/core/structgrpc__arg_1_1grpc__arg__value_1_1grpc__arg__pointer.html *)
      include Union (struct
        let name = "grpc_arg_value"
      end)

      let string = "string" <-. string
      let integer = "integer" <-. int
      let pointer = "pointer" <-. Pointer.t
      let () = seal t
    end

    include Anon_struct (struct
      let name = "grpc_arg"
    end)

    let typ = "type" <-. Type.t
    let key = "key" <-. string
    let value = "value" <-. Value.t
    let () = seal t

    module Pointer_vtable = struct
      include Struct (struct
        let name = "grpc_arg_pointer_vtable"
      end)

      let cmp = "cmp" <-. static_funptr @@ ptr void @-> ptr void @-> returning int
      let copy = "copy" <-. static_funptr @@ ptr void @-> returning (ptr void)
      let destroy = "destroy" <-. static_funptr @@ ptr void @-> returning (ptr void)
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
      [@@deriving show { with_path = false }]

      let enum_field = enum_field' "GPR"
      let monotonic = enum_field "CLOCK_MONOTONIC"
      let realtime = enum_field "CLOCK_REALTIME"
      let precise = enum_field "CLOCK_PRECISE"
      let timespan = enum_field "TIMESPAN"

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

    let tv_sec = "tv_sec" <-. int64_t
    let tv_nsec = "tv_nsec" <-. int32_t
    let clock_type = "clock_type" <-. Clock_type.t
    let () = seal t
  end

  module Log = struct
    module Severity = struct
      type t =
        | DEBUG
        | ERROR
        | INFO
      [@@deriving show { with_path = false }]

      let enum_field = enum_field' "GPR_LOG_SEVERITY"
      let debug = enum_field "DEBUG"
      let info = enum_field "INFO"
      let error = enum_field "ERROR"
      let t = enum' "gpr_log_severity" [ DEBUG, debug; INFO, info; ERROR, error ]
    end

    module Func_args = struct
      include Struct (struct
        let name = "gpr_log_func_args"
      end)

      let file = "file" <-. string
      let line = "line" <-. int
      let severity = "severity" <-. Severity.t
      let message = "message" <-. string
      let () = seal t
    end

    let log_func = Foreign.funptr @@ ptr Func_args.t @-> returning void
  end

  (** https://grpc.github.io/grpc/core/structgrpc__call__details.html *)
  module Call_details = struct
    include Anon_struct (struct
      let name = "grpc_call_details"
    end)

    let methd = "method" <-. Slice.t
    let host = "host" <-. Slice.t
    let deadline = "deadline" <-. Timespec.t
    let flags = "flags" <-. uint32_t
    let () = seal t
  end

  module Channel = struct
    module Arg = struct
      include Anon_struct (struct
        let name = "grpc_channel_args"
      end)

      let num_args = "num_args" <-. size_t
      let args = "args" <-. ptr Arg.t
      let () = seal t
    end

    module Credentials = Abs ()

    module Info = struct
      include Anon_struct (struct
        let name = "grpc_channel_info"
      end)

      let lb_policy_name = "lb_policy_name" <-. ptr string
      let service_config_json = "service_config_json" <-. ptr string
      let () = seal t
    end

    include Abs ()
  end

  module Call = struct
    include Abs ()

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
      [@@deriving show { with_path = false }]

      let enum_field = enum_field' "GRPC_CALL"
      let enum_efield = enum_field' "GRPC_CALL_ERROR"
      let ok = enum_field "OK"
      let error = enum_field "ERROR"
      let error_not_on_server = enum_efield "NOT_ON_SERVER"
      let error_not_on_client = enum_efield "NOT_ON_CLIENT"
      let error_already_accepted = enum_efield "ALREADY_ACCEPTED"
      let error_already_invoked = enum_efield "ALREADY_INVOKED"
      let error_not_invoked = enum_efield "NOT_INVOKED"
      let error_already_finished = enum_efield "ALREADY_FINISHED"
      let error_too_many_operations = enum_efield "TOO_MANY_OPERATIONS"
      let error_invalid_flags = enum_efield "INVALID_FLAGS"
      let error_invalid_metadata = enum_efield "INVALID_METADATA"
      let error_invalid_message = enum_efield "INVALID_MESSAGE"
      let error_not_server_completion_queue = enum_efield "NOT_SERVER_COMPLETION_QUEUE"
      let error_batch_too_big = enum_efield "BATCH_TOO_BIG"
      let error_payload_type_mismatch = enum_efield "PAYLOAD_TYPE_MISMATCH"
      let error_completion_queue_shutdown = enum_efield "COMPLETION_QUEUE_SHUTDOWN"

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
      [@@deriving show { with_path = false }]

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
      include Abs ()
      module Attributes = Abs ()
      module Factory = Abs ()

      (* module Functor = struct *)
      (* include Struct (struct *)
      (* let name = "grpc_completion_queue_functor" *)
      (* end) *)

      (* let functor_run = *)
      (* "functor_run" <-. static_funptr @@ ptr t @-> int @-> returning void *)
      (* ;; *)

      (* let inlineable = "inlineable" <-. int *)
      (* let internal_success = "internal_success" <-. int *)
      (* let internal_next = "internal_next" <-. ptr t *)
      (* let () = seal t *)
      (* end *)

      module Type = Abs ()
    end
  end

  module CQ_types = struct
    module Completion = struct
      type t =
        | NEXT
        | PLUCK
        | CALLBACK
      [@@deriving show { with_path = false }]

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
      [@@deriving show { with_path = false }]

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

    let typ = "type" <-. Completion.Type.t
    let success = "success" <-. int
    let tag = "tag" <-. ptr void
    let () = seal t
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
    [@@deriving show { with_path = false }]

    let enum_field = enum_field' "GRPC_STATUS"
    let ok = enum_field "OK"
    let cancelled = enum_field "CANCELLED"
    let unknown = enum_field "UNKNOWN"
    let invalid_argument = enum_field "INVALID_ARGUMENT"
    let deadline_exceeded = enum_field "DEADLINE_EXCEEDED"
    let not_found = enum_field "NOT_FOUND"
    let already_exists = enum_field "ALREADY_EXISTS"
    let permission_denied = enum_field "PERMISSION_DENIED"
    let unauthenticated = enum_field "UNAUTHENTICATED"
    let resource_exhausted = enum_field "RESOURCE_EXHAUSTED"
    let failed_precondition = enum_field "FAILED_PRECONDITION"
    let aborted = enum_field "ABORTED"
    let out_of_range = enum_field "OUT_OF_RANGE"
    let unimplemented = enum_field "UNIMPLEMENTED"
    let internal = enum_field "INTERNAL"
    let unavailable = enum_field "UNAVAILABLE"
    let data_loss = enum_field "DATA_LOSS"
    let _do_not_use = enum_field "_DO_NOT_USE"

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

  module Op = struct
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
      [@@deriving show { with_path = false }]

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

    module Data = struct
      include Union (struct
        let name = "grpc_op_data"
      end)

      let reserved = "reserved" <-. ptr void

      module Send_initial_metadata = struct
        module Maybe_compression_level = struct
          include Struct (struct
            let name = "grpc_op_send_initial_metadata_maybe_compression_level"
          end)

          let is_set = "is_set" <-. uint8_t
          let level = "level" <-. Compression.Level.t
          let () = seal t
        end

        include Struct (struct
          let name = "grpc_op_send_initial_metadata"
        end)

        let count = "count" <-. size_t
        let metadata = "metadata" <-. ptr Metadata.t

        let maybe_compression_level =
          "maybe_compression_level" <-. Maybe_compression_level.t
        ;;

        let () = seal t
      end

      module Send_message = struct
        include Struct (struct
          let name = "grpc_op_send_message"
        end)

        let send_message = "send_message" <-. ptr Byte_buffer.t
        let () = seal t
      end

      module Send_status_from_server = struct
        include Struct (struct
          let name = "grpc_op_send_status_from_server"
        end)

        let trailing_metadata_count = "trailing_metadata_count" <-. size_t
        let trailing_metadata = "trailing_metadata" <-. ptr Metadata.t
        let status = "status" <-. Status_code.t
        let status_details = "status_details" <-. ptr Slice.t
        let () = seal t
      end

      module Recv_initial_metadata = struct
        include Struct (struct
          let name = "grpc_op_recv_initial_metadata"
        end)

        let recv_initial_metadata = "recv_initial_metadata" <-. ptr Metadata.Array.t
        let () = seal t
      end

      module Recv_message = struct
        include Struct (struct
          let name = "grpc_op_recv_message"
        end)

        let recv_message = "recv_message" <-. ptr (ptr Byte_buffer.t)
        let () = seal t
      end

      module Recv_status_on_client = struct
        include Struct (struct
          let name = "grpc_op_recv_status_on_client"
        end)

        let trailing_metadata = "trailing_metadata" <-. ptr Metadata.Array.t
        let status = "status" <-. ptr Status_code.t
        let status_details = "status_details" <-. ptr Slice.t
        let error_string = "error_string" <-. ptr string
        let () = seal t
      end

      module Recv_close_on_server = struct
        include Struct (struct
          let name = "grpc_op_recv_close_on_server"
        end)

        let cancelled = "cancelled" <-. ptr int
        let () = seal t
      end

      let send_initial_metadata = "send_initial_metadata" <-. Send_initial_metadata.t
      let send_message = "send_message" <-. Send_message.t

      let send_status_from_server =
        "send_status_from_server" <-. Send_status_from_server.t
      ;;

      let recv_initial_metadata = "recv_initial_metadata" <-. Recv_initial_metadata.t
      let recv_message = "recv_message" <-. Recv_message.t
      let recv_status_on_client = "recv_status_on_client" <-. Recv_status_on_client.t
      let recv_close_on_server = "recv_close_on_server" <-. Recv_close_on_server.t
      let () = seal t
    end

    include Struct (struct
      let name = "grpc_op"
    end)

    let op = "op" <-. Type.t
    let flags = "flags" <-. uint32_t
    let reserved = "reserved" <-. ptr void
    let data = "data" <-. Data.t
    let () = seal t
  end

  module Socket = struct
    module Factorty = Abs ()
    module Mutator = Abs ()
  end

  module Connectivity_state = struct
    type t =
      | IDLE
      | CONNECTING
      | READY
      | TRANSIENT_FAILURE
      | SHUTDOWN
    [@@deriving show { with_path = false }]

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

  (* module Serving_status_update = struct *)
  (* include Anon_struct (struct *)
  (* let name = "grpc_serving_status_update" *)
  (* end) *)

  (* let error_message = "error_message" <-. string *)
  (* let code = "code" <-. Status_code.t *)
  (* let () = seal t *)
  (* end *)

  module Census_context = Abs ()
  module Resource_quota = Abs ()

  module Server = struct
    include Abs ()

    module Register_method_payload_handling = struct
      type t =
        | NONE
        | READ_INITIAL_BYTE_BUFFER
      [@@deriving show { with_path = false }]

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

    module Config_fetcher = Abs ()
    module Credentials = Abs ()

    (* module Xds_status_notifier = struct *)
    (* include Anon_struct (struct *)
    (* let name = "grpc_server_xds_status_notifier" *)
    (* end) *)

    (* let user_data = "user_data" <-. ptr void *)

    (* let on_serving_status_update = *)
    (* "on_serving_status_update" *)
    (* <-. static_funptr *)
    (* @@ ptr void *)
    (* @-> string *)
    (* (* XXX: should be "Serving_status_update.t" *) *)
    (* (* @-> Serving_status_update.t *) *)
    (* @-> ptr void *)
    (* @-> returning void *)
    (* ;; *)

    (* let () = seal t *)
    (* end *)
  end

  module Propagation_bits = struct
    type t = Unsigned.uint32 const

    let deadline = constant "GRPC_PROPAGATE_DEADLINE" uint32_t
    let census_stats_context = constant "GRPC_PROPAGATE_CENSUS_STATS_CONTEXT" uint32_t
    let census_tracing_context = constant "GRPC_PROPAGATE_CENSUS_TRACING_CONTEXT" uint32_t
    let cancellation = constant "GRPC_PROPAGATE_CANCELLATION" uint32_t
    let defaults = constant "GRPC_PROPAGATE_DEFAULTS" uint32_t
  end

  module Sync = struct
    module Mu = Abs ()
    module Cv = Abs ()
    module Once = Abs ()
  end

  module Atm = struct
    let t = typedef intptr_t "gpr_atm"
  end

  module Gpr_event = struct
    include Anon_struct (struct
      let name = "gpr_event"
    end)

    let state = "state" <-. Atm.t
    let () = seal t
  end

  module Gpr_refcount = struct
    include Anon_struct (struct
      let name = "gpr_refcount"
    end)

    let count = "count" <-. Atm.t
    let () = seal t
  end

  module Gpr_stats_counter = struct
    include Anon_struct (struct
      let name = "gpr_stats_counter"
    end)

    let value = "value" <-. Atm.t
    let () = seal t
  end

  module Flags = struct
    module Write = struct
      type t = Unsigned.uint32 const

      let buffer_hint = constant "GRPC_WRITE_BUFFER_HINT" uint32_t
      let no_compress = constant "GRPC_WRITE_NO_COMPRESS" uint32_t
      let through = constant "GRPC_WRITE_THROUGH" uint32_t
      let used_mask = constant "GRPC_WRITE_USED_MASK" uint32_t
    end
  end
end
