#!/usr/bin/env sh

set -eu

DEBUG=${DEBUG:-}

if [ "${DEBUG}" ]; then
  DEBUG="--debug=/dev/stdout"
fi

proto="misc/proto.proto"
service="grpc_test.Echo"
rpc="Greet"

cat <<EOL | ghz -r "${RATE:-0}" -c "${CONCURRENCY:-50}" -n "${NUMBER:-200}" --proto "${proto}" -m '{ "x-req-md": "hello" }' --import-paths test --call "${service}/${rpc}" --data-file=/dev/stdin --insecure localhost:$GRPC_PORT ${DEBUG}
{
  "message": "hello"
}
EOL
