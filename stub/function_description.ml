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
      @-> T.Call.t
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
      @-> T.Call.t
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

      let xds_create =
        foreign "grpc_server_config_fetcher_xds_create"
        @@ T.Server.Xds_status_notifier.t
        @-> ptr T.Channel.Arg.t
        @-> returning T.Server.Config_fetcher.t
      ;;
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

    let create_for_callback =
      foreign "grpc_completion_queue_create_for_callback"
      @@ ptr T.Completion.Queue.Functor.t
      @-> ptr void
      @-> returning t
    ;;

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

  let dump_xds_configs = "grpc_dump_xds_configs" @:: void @-> returning T.Slice.t

  module Header = struct
    let key_is_legal = "grpc_header_key_is_legal" @:: T.Slice.t @-> returning int

    let nonbin_value_is_legal =
      "grpc_header_nonbin_value_is_legal" @:: T.Slice.t @-> returning int
    ;;
  end

  module Slice = struct
    let t = T.Slice.t
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

    let from_seconts =
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
end
