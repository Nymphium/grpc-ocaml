let () =
  let open Alcotest in
  run
    "ffi"
    [ ( "type"
      , let open Grpc_stub.Type in
        [ (test_case "slice" `Quick @@ fun () -> ignore @@ Ctypes.make Slice.t)
        ; (test_case "slice_inlined" `Quick
          @@ fun () -> ignore @@ Ctypes.make Slice.Inlined.t)
        ; (test_case "slice_refcounted" `Quick
          @@ fun () -> ignore @@ Ctypes.make Slice.Refcounted.t)
        ; (test_case "slice_data" `Quick @@ fun () -> ignore @@ Ctypes.make Slice.Data.t)
        ; (test_case "slice_buffer" `Quick
          @@ fun () -> ignore @@ Ctypes.make Slice.Buffer.t)
        ; (test_case "compression_options" `Quick
          @@ fun () -> ignore @@ Ctypes.make Compression.Options.t)
        ; (test_case "compressed_buffer" `Quick
          @@ fun () -> ignore @@ Ctypes.make Byte_buffer.Data.Compressed.t)
        ; (test_case "byte_buffer_data" `Quick
          @@ fun () -> ignore @@ Ctypes.make Byte_buffer.Data.t)
        ; (test_case "byte_buffer" `Quick @@ fun () -> ignore @@ Ctypes.make Byte_buffer.t)
        ; (test_case "metadata" `Quick @@ fun () -> ignore @@ Ctypes.make Metadata.t)
        ; (test_case "metadata_array" `Quick
          @@ fun () -> ignore @@ Ctypes.make Metadata.Array.t)
        ; (test_case "arg_pointer" `Quick
          @@ fun () -> ignore @@ Ctypes.make Arg.Value.Pointer.t)
        ; (test_case "arg_value" `Quick @@ fun () -> ignore @@ Ctypes.make Arg.Value.t)
        ; (test_case "arg" `Quick @@ fun () -> ignore @@ Ctypes.make Arg.t)
        ; (test_case "arg_pointer_vtable" `Quick
          @@ fun () -> ignore @@ Ctypes.make Arg.Pointer_vtable.t)
        ; (test_case "gpr_timespec" `Quick @@ fun () -> ignore @@ Ctypes.make Timespec.t)
        ; (test_case "call_details" `Quick
          @@ fun () -> ignore @@ Ctypes.make Call_details.t)
          (* ; (test_case "channel" `Quick @@ fun () -> ignore @@ Ctypes.make Channel.t) *)
        ; (test_case "channel_args" `Quick
          @@ fun () -> ignore @@ Ctypes.make Channel.Arg.t)
          (* ; (test_case "channel_credentials" `Quick *)
          (* @@ fun () -> ignore @@ Ctypes.make Channel.Credentials.t) *)
        ; (test_case "channel_info" `Quick
          @@ fun () -> ignore @@ Ctypes.make Channel.Info.t)
          (* ; (test_case "call" `Quick @@ fun () -> ignore @@ Ctypes.make Call.t) *)
          (* ; (test_case "completion_queue" `Quick *)
          (* @@ fun () -> ignore @@ Ctypes.make Completion.Queue.t) *)
          (* ; (test_case "completion_queue_attributes" `Quick *)
          (* @@ fun () -> ignore @@ Ctypes.make Completion.Queue.Attributes.t) *)
          (* ; (test_case "completion_queue_factory" `Quick *)
          (* @@ fun () -> ignore @@ Ctypes.make Completion.Queue.Factory.t) *)
        ; (test_case "completion_queue_functor" `Quick
          @@ fun () -> ignore @@ Ctypes.make Completion.Queue.Functor.t)
          (* ; (test_case "completion_queue_type" `Quick *)
          (* @@ fun () -> ignore @@ Ctypes.make Completion.Queue.Type.t) *)
        ; (test_case "event" `Quick @@ fun () -> ignore @@ Ctypes.make Event.t)
          (* ; (test_case "op" `Quick @@ fun () -> ignore @@ Ctypes.make Op.t) *)
          (* ; (test_case "socket_factory" `Quick *)
          (* @@ fun () -> ignore @@ Ctypes.make Socket.Factorty.t) *)
          (* ; (test_case "socket_mutator" `Quick *)
          (* @@ fun () -> ignore @@ Ctypes.make Socket.Mutator.t) *)
        ; (test_case "serving_status_update" `Quick
          @@ fun () -> ignore @@ Ctypes.make Serving_status_update.t)
          (* ; (test_case "census_context" `Quick *)
          (* @@ fun () -> ignore @@ Ctypes.make Census_context.t) *)
          (* ; (test_case "resource_quota" `Quick *)
          (* @@ fun () -> ignore @@ Ctypes.make Resource_quota.t) *)
          (* ; (test_case "server" `Quick @@ fun () -> ignore @@ Ctypes.make Server.t) *)
          (* ; (test_case "server_credentials" `Quick *)
          (* @@ fun () -> ignore @@ Ctypes.make Server.Credentials.t) *)
          (* ; (test_case "server_config_fetcher" `Quick *)
          (* @@ fun () -> ignore @@ Ctypes.make Server.Config_fetcher.t) *)
        ; (test_case "server_xds_status_notifier" `Quick
          @@ fun () -> ignore @@ Ctypes.make Server.Xds_status_notifier.t)
        ] )
    ]
;;
