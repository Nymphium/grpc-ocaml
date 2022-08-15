/* suppress for ignored `const` keywords */
#pragma GCC diagnostic ignored "-Wdiscarded-qualifiers"
/* suppress for opaque types as void ptr */
#pragma GCC diagnostic ignored "-Wincompatible-pointer-types"

#include <grpc/grpc.h>
#include <grpc/grpc_security.h>
#include <grpc/slice.h>
#include <grpc/byte_buffer_reader.h>
#include <grpc/support/log.h>
#include <grpc/support/alloc.h>
#include <grpc/support/sync.h>
#include <grpc/impl/codegen/sync_generic.h>
