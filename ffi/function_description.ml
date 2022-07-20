open Ctypes

module Functions (F : Ctypes.FOREIGN) = struct
  open F
  module T = Types_generated

  let init = foreign "grpc_init" @@ void @-> returning void
  let shutdown = foreign "grpc_shutdown" @@ void @-> returning void
  let is_initialized = foreign "grpc_is_initialized" @@ void @-> returning int
  let version_s = foreign "grpc_version_string" @@ void @-> returning string
  let g_stands_for = foreign "grpc_g_stands_for" @@ void @-> returning string
  let is_binary_header = foreign "grpc_is_binary_header" @@ T.Slice.t @-> returning int

  let trace_set_enabled =
    foreign "grpc_tracer_set_enabled" @@ string @-> int @-> returning int
  ;;

  let lame_client_channel_create =
    foreign "grpc_lame_client_channel_create"
    @@ string
    @-> T.Status_code.t
    @-> string
    @-> returning T.Channel.t
  ;;

  module Metadata = struct
    let t = ptr T.Metadata.Array.t
    let init = foreign "grpc_metadata_array_init" @@ t @-> returning void
    let destroy = foreign "grpc_metadata_array_destroy" @@ t @-> returning void
  end

  module Resource_quota = struct
    let t = T.Resource_quota.t

    let arg_vtable =
      foreign "grpc_resource_quota_arg_vtable"
      @@ void
      @-> returning (ptr T.Arg.Pointer_vtable.t)
    ;;

    let create = foreign "grpc_resource_quota_create" @@ string @-> returning t
    let ref = foreign "grpc_resource_quota_ref" @@ t @-> returning void
    let resize = foreign "grpc_resource_quota_resize" @@ t @-> size_t @-> returning void

    let set_max_threads =
      foreign "grpc_resource_quota_set_max_threads" @@ t @-> int @-> returning void
    ;;

    let unref = foreign "grpc_resource_quota_unref" @@ t @-> returning void
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

    let cancel_all_calls = foreign "grpc_server_cancel_all_calls" @@ t @-> returning void

    let create =
      foreign "grpc_server_create" @@ ptr_opt T.Channel.Arg.t @-> ptr void @-> returning t
    ;;

    let destroy = foreign "grpc_server_destroy" @@ t @-> returning void

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

    let start = foreign "grpc_server_start" @@ t @-> returning void

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
      let release = foreign "grpc_server_credentials_release" @@ c @-> returning void

      let create_insecure =
        foreign "grpc_insecure_server_credentials_create" @@ void @-> returning c
      ;;
    end
  end

  module Call = struct
    let t = T.Call.t
    let error = T.Call.Error.t

    let arena_alloc =
      foreign "grpc_call_arena_alloc" @@ t @-> size_t @-> returning (ptr void)
    ;;

    let cancel = foreign "grpc_call_cancel" @@ t @-> ptr void @-> returning error

    let cancel_with_status =
      foreign "grpc_call_cancel_with_status"
      @@ t
      @-> T.Status_code.t
      @-> string
      @-> ptr void
      @-> returning error
    ;;

    let error_to_string =
      foreign "grpc_call_error_to_string" @@ error @-> returning string
    ;;

    let failed_before_recv_message =
      foreign "grpc_call_failed_before_recv_message" @@ t @-> returning int
    ;;

    let get_peer = foreign "grpc_call_get_peer" @@ t @-> returning string
    let ref = foreign "grpc_call_ref" @@ t @-> returning void

    let start_batch =
      foreign "grpc_call_start_batch"
      @@ t
      @-> T.Op.t
      @-> size_t
      @-> ptr void
      @-> ptr void
      @-> returning error
    ;;

    let unref = foreign "grpc_call_unref" @@ t @-> returning void

    module Census = struct
      let c = T.Census_context.t
      let get_context = foreign "grpc_census_call_get_context" @@ t @-> returning c

      let set_context =
        foreign "grpc_census_call_set_context" @@ t @-> c @-> returning void
      ;;
    end

    module Details = struct
      let t = T.Call_details.t
      let init = foreign "grpc_call_details_init" @@ ptr t @-> returning void
      let destroy = foreign "grpc_call_details_destroy" @@ ptr t @-> returning void
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

    let destroy = foreign "grpc_channel_destroy" @@ T.Channel.t @-> returning void

    let get_info =
      foreign "grpc_channel_get_info"
      @@ T.Channel.t
      @-> ptr T.Channel.Info.t
      @-> returning void
    ;;

    let get_target = foreign "grpc_channel_get_target" @@ T.Channel.t @-> returning string

    let num_extra_connectivity_watchers =
      foreign "grpc_channel_num_external_connectivity_watchers"
      @@ T.Channel.t
      @-> returning int
    ;;

    let reset_connect_backoff =
      foreign "grpc_channel_reset_connect_backoff" @@ T.Channel.t @-> returning void
    ;;

    module Credentials = struct
      let c = T.Channel.Credentials.t
      let release = foreign "grpc_channel_credentials_release" @@ c @-> returning void

      let create_insecure =
        foreign "grpc_insecure_credentials_create" @@ void @-> returning c
      ;;
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
    let get_channel = foreign "grpc_channelz_get_channel" @@ intptr_t @-> returning string
    let get_server = foreign "grpc_channelz_get_server" @@ intptr_t @-> returning string

    let get_server_sockets =
      foreign "grpc_channelz_get_server_sockets"
      @@ intptr_t
      @-> intptr_t
      @-> intptr_t
      @-> returning string
    ;;

    let get_servers = foreign "grpc_channelz_get_servers" @@ intptr_t @-> returning string
    let get_socket = foreign "grpc_channelz_get_socket" @@ intptr_t @-> returning string

    let get_subchannel =
      foreign "grpc_channelz_get_subchannel" @@ intptr_t @-> returning string
    ;;

    let get_top_channels =
      foreign "grpc_channelz_get_top_channels" @@ intptr_t @-> returning string
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
      foreign "grpc_completion_queue_create_for_next" @@ ptr void @-> returning t
    ;;

    let create_for_pluck =
      foreign "grpc_completion_queue_create_for_pluck" @@ ptr void @-> returning t
    ;;

    let destroy = foreign "grpc_completion_queue_destroy" @@ t @-> returning void

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

    let shutdown = foreign "grpc_completion_queue_shutdown" @@ t @-> returning void

    module Thread_local_cache = struct
      let flush =
        foreign "grpc_completion_queue_thread_local_cache_flush"
        @@ t
        @-> ptr (ptr void)
        @-> ptr int
        @-> returning int
      ;;

      let init =
        foreign "grpc_completion_queue_thread_local_cache_init" @@ t @-> returning void
      ;;
    end
  end

  let dump_xds_configs = foreign "grpc_dump_xds_configs" @@ void @-> returning T.Slice.t

  module Header = struct
    let key_is_legal = foreign "grpc_header_key_is_legal" @@ T.Slice.t @-> returning int

    let nonbin_value_is_legal =
      foreign "grpc_header_nonbin_value_is_legal" @@ T.Slice.t @-> returning int
    ;;
  end
end
