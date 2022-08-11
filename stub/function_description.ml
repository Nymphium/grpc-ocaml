open Ctypes

module Functions (F : Ctypes.FOREIGN) = struct
  open F
  module T = Types_generated

  let ( @:: ) name t = foreign name t
  let init = "grpc_init" @:: void @-> returning void
  let shutdown = "grpc_shutdown" @:: void @-> returning void
  let is_initialized = "grpc_is_initialized" @:: void @-> returning int
  let version_s = "grpc_version_string" @:: void @-> returning string
  let g_stands_for = "grpc_g_stands_for" @:: void @-> returning string
  let is_binary_header = "grpc_is_binary_header" @:: T.Slice.t @-> returning int
  let trace_set_enabled = "grpc_tracer_set_enabled" @:: string @-> int @-> returning int

  let lame_client_channel_create =
    foreign "grpc_lame_client_channel_create"
    @@ string
    @-> T.Status_code.t
    @-> string
    @-> returning T.Channel.t
  ;;

  module Metadata = struct
    let t = ptr T.Metadata.Array.t
    let init = "grpc_metadata_array_init" @:: t @-> returning void
    let destroy = "grpc_metadata_array_destroy" @:: t @-> returning void
  end

  module Resource_quota = struct
    let t = T.Resource_quota.t

    let arg_vtable =
      foreign "grpc_resource_quota_arg_vtable"
      @@ void
      @-> returning (ptr T.Arg.Pointer_vtable.t)
    ;;

    let create = "grpc_resource_quota_create" @:: string @-> returning t
    let ref = "grpc_resource_quota_ref" @:: t @-> returning void
    let resize = "grpc_resource_quota_resize" @:: t @-> size_t @-> returning void

    let set_max_threads =
      "grpc_resource_quota_set_max_threads" @:: t @-> int @-> returning void
    ;;

    let unref = "grpc_resource_quota_unref" @:: t @-> returning void
  end

  module Server = struct
    let t = T.Server.t

    let add_http2_port =
      foreign "grpc_server_add_http2_port"
      @@ t
      @-> string
      @-> T.Server.Credentials.t
      @-> returning int
    ;;

    let cancel_all_calls = "grpc_server_cancel_all_calls" @:: t @-> returning void

    let create =
      "grpc_server_create" @:: ptr_opt T.Channel.Arg.t @-> ptr void @-> returning t
    ;;

    let destroy = "grpc_server_destroy" @:: t @-> returning void

    let register_completion_queue =
      foreign "grpc_server_register_completion_queue"
      @@ t
      @-> T.Completion.Queue.t
      @-> ptr void
      @-> returning void
    ;;

    let register_method =
      foreign "grpc_server_register_method"
      @@ t
      @-> string
      @-> string
      @-> T.Server.Register_method_payload_handling.t
      @-> uint32_t
      @-> returning (ptr void)
    ;;

    let request_call =
      foreign "grpc_server_request_call"
      @@ t
      @-> ptr T.Call.t
      @-> ptr T.Call_details.t
      @-> ptr T.Metadata.Array.t
      @-> T.Completion.Queue.t
      @-> T.Completion.Queue.t
      @-> ptr void
      @-> returning T.Call.Error.t
    ;;

    let request_registered_call =
      foreign "grpc_server_request_registered_call"
      @@ t
      @-> ptr void
      @-> ptr T.Call.t
      @-> ptr T.Timespec.t
      @-> ptr T.Metadata.Array.t
      @-> ptr (ptr T.Byte_buffer.t)
      @-> T.Completion.Queue.t
      @-> T.Completion.Queue.t
      @-> ptr void
      @-> returning T.Call.Error.t
    ;;

    let set_config_fetcher =
      foreign "grpc_server_set_config_fetcher"
      @@ t
      @-> T.Server.Config_fetcher.t
      @-> returning void
    ;;

    let shutdown_and_notify =
      foreign "grpc_server_shutdown_and_notify"
      @@ t
      @-> T.Completion.Queue.t
      @-> ptr void
      @-> returning void
    ;;

    let start = "grpc_server_start" @:: t @-> returning void

    module Config_fetcher = struct
      let destoy =
        foreign "grpc_server_config_fetcher_destroy"
        @@ T.Server.Config_fetcher.t
        @-> returning void
      ;;

      (* let xds_create = *)
      (* foreign "grpc_server_config_fetcher_xds_create" *)
      (* @@ T.Server.Xds_status_notifier.t *)
      (* @-> ptr T.Channel.Arg.t *)
      (* @-> returning T.Server.Config_fetcher.t *)
      (* ;; *)
    end

    module Credentials = struct
      let c = T.Server.Credentials.t
      let release = "grpc_server_credentials_release" @:: c @-> returning void

      let create_insecure =
        "grpc_insecure_server_credentials_create" @:: void @-> returning c
      ;;
    end
  end

  module Call = struct
    let t = T.Call.t
    let error = T.Call.Error.t
    let arena_alloc = "grpc_call_arena_alloc" @:: t @-> size_t @-> returning (ptr void)
    let cancel = "grpc_call_cancel" @:: t @-> ptr void @-> returning error

    let cancel_with_status =
      foreign "grpc_call_cancel_with_status"
      @@ t
      @-> T.Status_code.t
      @-> string
      @-> ptr void
      @-> returning error
    ;;

    let error_to_string = "grpc_call_error_to_string" @:: error @-> returning string

    let failed_before_recv_message =
      "grpc_call_failed_before_recv_message" @:: t @-> returning int
    ;;

    let get_peer = "grpc_call_get_peer" @:: t @-> returning string
    let ref = "grpc_call_ref" @:: t @-> returning void

    let start_batch =
      foreign "grpc_call_start_batch"
      @@ t
      @-> ptr T.Op.t
      @-> size_t
      @-> ptr void
      @-> ptr void
      @-> returning error
    ;;

    let unref = "grpc_call_unref" @:: t @-> returning void

    module Census = struct
      let c = T.Census_context.t
      let get_context = "grpc_census_call_get_context" @:: t @-> returning c
      let set_context = "grpc_census_call_set_context" @:: t @-> c @-> returning void
    end

    module Details = struct
      let t = T.Call_details.t
      let init = "grpc_call_details_init" @:: ptr t @-> returning void
      let destroy = "grpc_call_details_destroy" @:: ptr t @-> returning void
    end
  end

  let register_plugin =
    foreign "grpc_register_plugin"
    @@ T.Common.voidfn
    @-> T.Common.voidfn
    @-> returning void
  ;;

  module Channel = struct
    let t = T.Channel.t

    let create =
      foreign "grpc_channel_create"
      @@ string
      @-> T.Channel.Credentials.t
      @-> ptr_opt T.Channel.Arg.t
      @-> returning t
    ;;

    let create_call =
      foreign "grpc_channel_create_call"
      @@ t
      @-> T.Call.t
      @-> uint32_t
      @-> T.Completion.Queue.t
      @-> T.Slice.t
      @-> ptr T.Slice.t
      @-> T.Timespec.t
      @-> ptr void
      @-> returning T.Call.t
    ;;

    let create_registered_call =
      foreign "grpc_channel_create_registered_call"
      @@ T.Channel.t
      @-> T.Call.t
      @-> uint32_t
      @-> T.Completion.Queue.t
      @-> ptr void
      @-> T.Timespec.t
      @-> ptr void
      @-> returning T.Call.t
    ;;

    let destroy = "grpc_channel_destroy" @:: T.Channel.t @-> returning void

    let get_info =
      foreign "grpc_channel_get_info"
      @@ T.Channel.t
      @-> ptr T.Channel.Info.t
      @-> returning void
    ;;

    let get_target = "grpc_channel_get_target" @:: T.Channel.t @-> returning string

    let num_extra_connectivity_watchers =
      foreign "grpc_channel_num_external_connectivity_watchers"
      @@ T.Channel.t
      @-> returning int
    ;;

    let reset_connect_backoff =
      "grpc_channel_reset_connect_backoff" @:: T.Channel.t @-> returning void
    ;;

    module Credentials = struct
      let c = T.Channel.Credentials.t
      let release = "grpc_channel_credentials_release" @:: c @-> returning void
      let create_insecure = "grpc_insecure_credentials_create" @:: void @-> returning c
    end

    module Connectivity = struct
      let support_watcher =
        foreign "grpc_channel_support_connectivity_watcher"
        @@ T.Channel.t
        @-> returning int
      ;;

      module State = struct
        let check =
          foreign "grpc_channel_check_connectivity_state"
          @@ T.Channel.t
          @-> int
          @-> returning T.Connectivity_state.t
        ;;

        let watch =
          foreign "grpc_channel_watch_connectivity_state"
          @@ T.Channel.t
          @-> T.Connectivity_state.t
          @-> T.Timespec.t
          @-> T.Completion.Queue.t
          @-> ptr void
          @-> returning void
        ;;
      end
    end
  end

  module Channelz = struct
    let get_channel = "grpc_channelz_get_channel" @:: intptr_t @-> returning string
    let get_server = "grpc_channelz_get_server" @:: intptr_t @-> returning string

    let get_server_sockets =
      foreign "grpc_channelz_get_server_sockets"
      @@ intptr_t
      @-> intptr_t
      @-> intptr_t
      @-> returning string
    ;;

    let get_servers = "grpc_channelz_get_servers" @:: intptr_t @-> returning string
    let get_socket = "grpc_channelz_get_socket" @:: intptr_t @-> returning string
    let get_subchannel = "grpc_channelz_get_subchannel" @:: intptr_t @-> returning string

    let get_top_channels =
      "grpc_channelz_get_top_channels" @:: intptr_t @-> returning string
    ;;
  end

  module Completion_queue = struct
    let t = T.Completion.Queue.t

    let create =
      foreign "grpc_completion_queue_create"
      @@ T.Completion.Queue.Factory.t
      @-> T.Completion.Queue.Attributes.t
      @-> ptr void
      @-> returning t
    ;;

    (* let create_for_callback = *)
    (* foreign "grpc_completion_queue_create_for_callback" *)
    (* @@ ptr T.Completion.Queue.Functor.t *)
    (* @-> ptr void *)
    (* @-> returning t *)
    (* ;; *)

    let create_for_next =
      "grpc_completion_queue_create_for_next" @:: ptr void @-> returning t
    ;;

    let create_for_pluck =
      "grpc_completion_queue_create_for_pluck" @:: ptr void @-> returning t
    ;;

    let destroy = "grpc_completion_queue_destroy" @:: t @-> returning void

    let factory_lookup =
      foreign "grpc_completion_queue_factory_lookup"
      @@ T.Completion.Queue.Attributes.t
      @-> returning T.Completion.Queue.Factory.t
    ;;

    let next =
      foreign "grpc_completion_queue_next"
      @@ t
      @-> T.Timespec.t
      @-> ptr void
      @-> returning T.Event.t
    ;;

    let pluck =
      foreign "grpc_completion_queue_pluck"
      @@ t
      @-> ptr void
      @-> T.Timespec.t
      @-> ptr void
      @-> returning T.Event.t
    ;;

    let shutdown = "grpc_completion_queue_shutdown" @:: t @-> returning void

    module Thread_local_cache = struct
      let flush =
        foreign "grpc_completion_queue_thread_local_cache_flush"
        @@ t
        @-> ptr (ptr void)
        @-> ptr int
        @-> returning int
      ;;

      let init = "grpc_completion_queue_thread_local_cache_init" @:: t @-> returning void
    end
  end

  (* let dump_xds_configs = "grpc_dump_xds_configs" @:: void @-> returning T.Slice.t *)

  module Header = struct
    let key_is_legal = "grpc_header_key_is_legal" @:: T.Slice.t @-> returning int

    let nonbin_value_is_legal =
      "grpc_header_nonbin_value_is_legal" @:: T.Slice.t @-> returning int
    ;;
  end

  module Slice = struct
    open struct
      module M = T.Slice

      let t = M.t
    end

    let start_ptr = "GRPC_SLICE_START_PTR" @:: t @-> returning (ptr uint8_t)
    let length = "GRPC_SLICE_LENGTH" @:: t @-> returning size_t
    let set_length = "GRPC_SLICE_SET_LENGTH" @:: t @-> size_t @-> returning void
    let end_ptr = "GRPC_SLICE_END_PTR" @:: t @-> returning (ptr uint8_t)
    let is_empty = "GRPC_SLICE_IS_EMPTY" @:: t @-> returning int
    let empty = "grpc_empty_slice" @:: void @-> returning t

    let buf_start_eq =
      "grpc_slice_buf_start_eq" @:: t @-> ptr void @-> size_t @-> returning int
    ;;

    let chr = "grpc_slice_chr" @:: t @-> char @-> returning int
    let cmp = "grpc_slice_cmp" @:: t @-> t @-> returning int
    let copy = "grpc_slice_copy" @:: t @-> returning t
    let eq = "grpc_slice_eq" @:: t @-> t @-> returning int

    let from_copied_buffer =
      "grpc_slice_from_copied_buffer" @:: string @-> size_t @-> returning t
    ;;

    let from_copied_string = "grpc_slice_from_copied_string" @:: string @-> returning t

    let from_static_buffer =
      "grpc_slice_from_static_buffer" @:: ptr void @-> size_t @-> returning t
    ;;

    let from_static_string = "grpc_slice_from_static_string" @:: string @-> returning t
    let is_equlivalent = "grpc_slice_is_equivalent" @:: t @-> t @-> returning int
    let malloc = "grpc_slice_malloc" @:: size_t @-> returning t
    let malloc_large = "grpc_slice_malloc_large" @:: size_t @-> returning t

    let new' =
      foreign "grpc_slice_new"
      @@ ptr void
      @-> size_t
      @-> Ctypes.(static_funptr @@ ptr void @-> returning void)
      @-> returning t
    ;;

    let new_with_len =
      foreign "grpc_slice_new_with_len"
      @@ ptr void
      @-> size_t
      @-> Ctypes.(static_funptr @@ ptr void @-> size_t @-> returning void)
      @-> returning t
    ;;

    let new_with_user_data =
      foreign "grpc_slice_new_with_user_data"
      @@ ptr void
      @-> size_t
      @-> Ctypes.(static_funptr @@ ptr void @-> returning void)
      @-> ptr void
      @-> returning t
    ;;

    let rchr = "grpc_slice_rchr" @:: t @-> char @-> returning int
    let ref = "grpc_slice_ref" @:: t @-> returning t
    let slice = "grpc_slice_slice" @:: t @-> t @-> returning int
    let split_head = "grpc_slice_split_head" @:: ptr t @-> size_t @-> returning t
    let split_tail = "grpc_slice_split_tail" @:: ptr t @-> size_t @-> returning t

    let split_tail_maybe_ref =
      foreign "grpc_slice_split_tail_maybe_ref"
      @@ ptr t
      @-> size_t
      @-> T.Slice.Ref_whom.t
      @-> returning t
    ;;

    let str_cmp = "grpc_slice_str_cmp" @:: t @-> string @-> returning int
    let sub = "grpc_slice_sub" @:: t @-> size_t @-> size_t @-> returning t
    let sub_no_ref = "grpc_slice_sub_no_ref" @:: t @-> size_t @-> size_t @-> returning t
    let to_c_string = "grpc_slice_to_c_string" @:: t @-> returning string
    let unref = "grpc_slice_unref" @:: t @-> returning void

    module Buffer = struct
      open struct
        module U = M
        module M = T.Slice.Buffer

        let u = t
        let t = ptr M.t
      end

      let init = "grpc_slice_buffer_init" @:: t @-> returning void
      let destroy = "grpc_slice_buffer_destroy" @:: t @-> returning void
      let add = "grpc_slice_buffer_add" @:: t @-> u @-> returning void
      let add_indexed = "grpc_slice_buffer_add_indexed" @:: t @-> u @-> returning size_t
      let addn = "grpc_slice_buffer_addn" @:: t @-> ptr u @-> size_t @-> returning void

      let tiny_add =
        "grpc_slice_buffer_tiny_add" @:: t @-> size_t @-> returning (ptr uint8_t)
      ;;

      let pop = "grpc_slice_buffer_pop" @:: t @-> returning void
      let reset_and_unref = "grpc_slice_buffer_reset_and_unref" @:: t @-> returning void
      let swap = "grpc_slice_buffer_swap" @:: t @-> t @-> returning void
      let move_into = "grpc_slice_buffer_move_into" @:: t @-> t @-> returning void

      let trim_end =
        "grpc_slice_buffer_trim_end" @:: t @-> size_t @-> t @-> returning void
      ;;

      let move_first =
        "grpc_slice_buffer_move_first" @:: t @-> size_t @-> t @-> returning void
      ;;

      let move_first_no_ref =
        "grpc_slice_buffer_move_first_no_ref" @:: t @-> size_t @-> t @-> returning void
      ;;

      let move_first_into_buffer =
        "grpc_slice_buffer_move_first_into_buffer"
        @:: t
        @-> size_t
        @-> ptr void
        @-> returning void
      ;;

      let take_first = "grpc_slice_buffer_take_first" @:: t @-> returning u

      let undo_take_first =
        "grpc_slice_buffer_undo_take_first" @:: t @-> u @-> returning void
      ;;
    end
  end

  module Byte_buffer = struct
    open struct
      module M = T.Byte_buffer

      let t = ptr M.t
    end

    let create_raw =
      "grpc_raw_byte_buffer_create" @:: ptr T.Slice.t @-> size_t @-> returning t
    ;;

    let create_raw_compressed =
      "grpc_raw_compressed_byte_buffer_create"
      @:: ptr T.Slice.t
      @-> size_t
      @-> T.Compression.Algorithm.t
      @-> returning t
    ;;

    let copy = "grpc_byte_buffer_copy" @:: t @-> returning t
    let length = "grpc_byte_buffer_length" @:: t @-> returning size_t
    let destroy = "grpc_byte_buffer_destroy" @:: t @-> returning void

    let raw_from_reader =
      "grpc_raw_byte_buffer_from_reader" @:: ptr M.Reader.t @-> returning t
    ;;

    module Reader = struct
      open struct
        module M = T.Byte_buffer.Reader

        let u = t
        let t = ptr M.t
      end

      let init = "grpc_byte_buffer_reader_init" @:: t @-> u @-> returning int
      let destroy = "grpc_byte_buffer_reader_destroy" @:: t @-> returning void
      let next = "grpc_byte_buffer_reader_next" @:: t @-> ptr T.Slice.t @-> returning int

      (* let peek = *)
      (* "grpc_byte_buffer_reader_peek" @:: t @-> ptr (ptr T.Slice.t) @-> returning int *)
      (* ;; *)

      let readall = "grpc_byte_buffer_reader_readall" @:: t @-> returning T.Slice.t
    end
  end

  module Timespec = struct
    module T = T.Timespec

    let t = T.t
    let zero = "gpr_time_0" @:: T.Clock_type.t @-> returning t
    let inf_future = "gpr_inf_future" @:: T.Clock_type.t @-> returning t
    let inf_past = "gpr_inf_past" @:: T.Clock_type.t @-> returning t
    let init = "gpr_time_init" @:: void @-> returning void
    let now = "gpr_now" @:: T.Clock_type.t @-> returning t

    let convert_clock_type =
      "gpr_convert_clock_type" @:: t @-> T.Clock_type.t @-> returning t
    ;;

    let cmp = "gpr_time_cmp" @:: t @-> t @-> returning int
    let max = "gpr_time_max" @:: t @-> t @-> returning t
    let min = "gpr_time_min" @:: t @-> t @-> returning t
    let add = "gpr_time_add" @:: t @-> t @-> returning t
    let sub = "gpr_time_sub" @:: t @-> t @-> returning t

    let from_micros =
      "gpr_time_from_micros" @:: int64_t @-> T.Clock_type.t @-> returning t
    ;;

    let from_nanos = "gpr_time_from_nanos" @:: int64_t @-> T.Clock_type.t @-> returning t

    let from_millis =
      "gpr_time_from_millis" @:: int64_t @-> T.Clock_type.t @-> returning t
    ;;

    let from_seconds =
      "gpr_time_from_seconds" @:: int64_t @-> T.Clock_type.t @-> returning t
    ;;

    let from_minutes =
      "gpr_time_from_minutes" @:: int64_t @-> T.Clock_type.t @-> returning t
    ;;

    let from_hours = "gpr_time_from_hours" @:: int64_t @-> T.Clock_type.t @-> returning t
    let to_millis = "gpr_time_to_millis" @:: t @-> returning int32_t
    let similar = "gpr_time_similar" @:: t @-> t @-> t @-> returning int
    let until = "gpr_sleep_until" @:: t @-> returning void
    let to_micros = "gpr_timespec_to_micros" @:: t @-> returning double
  end

  module Log = struct
    open struct
      module M = T.Log
    end

    let init = "gpr_log_verbosity_init" @:: void @-> returning void
    let should_log = "gpr_should_log" @:: M.Severity.t @-> returning int
    let set_log_level = "gpr_set_log_verbosity" @:: M.Severity.t @-> returning void

    let string_of_severity =
      "gpr_log_severity_string" @:: M.Severity.t @-> returning string
    ;;

    let set_log_func = "gpr_set_log_function" @:: M.log_func @-> returning void

    let message =
      "gpr_log_message" @:: string @-> int @-> M.Severity.t @-> string @-> returning void
    ;;

    (* let log = *)
    (* "gpr_log" *)
    (* @:: string *)
    (* @-> int *)
    (* @-> M.Severity.t *)
    (* @-> string *)
    (* @-> ptr void *)
    (* @-> returning void *)
    (* ;; *)
  end

  module Alloc = struct
    let malloc = "gpr_malloc" @:: size_t @-> returning (ptr void)
    let zalloc = "gpr_zalloc" @:: size_t @-> returning (ptr void)
    let free = "gpr_free" @:: ptr void @-> returning void
    let realloc = "gpr_realloc" @:: ptr void @-> size_t @-> returning (ptr void)

    let malloc_aligned =
      "gpr_malloc_aligned" @:: size_t @-> size_t @-> returning (ptr void)
    ;;

    let free_aligned = "gpr_free_aligned" @:: ptr void @-> returning void
  end

  module Cv = struct
    open struct
      module M = T.Sync

      let t = M.Cv.t
    end

    let broadcast = "gpr_cv_broadcast" @:: t @-> returning void
    let destroy = "gpr_cv_destroy" @:: t @-> returning void
    let init = "gpr_cv_init" @:: t @-> returning void
    let signal = "gpr_cv_signal" @:: t @-> returning void
    let wait = "gpr_cv_wait" @:: t @-> M.Mu.t @-> T.Timespec.t @-> returning int
  end

  module Event = struct
    open struct
      module M = T.Gpr_event

      let t = ptr M.t
    end

    let get = "gpr_event_get" @:: t @-> returning (ptr void)
    let init = "gpr_event_init" @:: t @-> returning void
    let set = "gpr_event_set" @:: t @-> ptr void @-> returning void
    let wait = "gpr_event_wait" @:: t @-> T.Timespec.t @-> returning (ptr void)
  end

  module Mu = struct
    open struct
      let t = T.Sync.Mu.t
    end

    let destroy = "gpr_mu_destroy" @:: t @-> returning void
    let init = "gpr_mu_init" @:: t @-> returning void
    let lock = "gpr_mu_lock" @:: t @-> returning void
    let trylock = "gpr_mu_trylock" @:: t @-> returning int
    let unlock = "gpr_mu_unlock" @:: t @-> returning void
  end

  module Once = struct
    open struct
      module M = T.Sync

      let t = M.Once.t
    end

    let init =
      "gpr_once_init"
      @:: t
      @-> Ctypes.(static_funptr @@ void @-> returning void)
      @-> returning void
    ;;
  end
end
