const grpc = require('grpc');
const protoLoader = require('@grpc/proto-loader');

const PROTO_PATH = __dirname + '/proto.proto'
const packageDefinition = protoLoader.loadSync(PROTO_PATH)
const proto = grpc.loadPackageDefinition(packageDefinition)

const Greet = (call, callback) => {
  console.log(`received message: ${JSON.stringify(call.request)}`);
  callback(null, { message: `${call.request.message}` });
}

const server = new grpc.Server();
server.addService(proto.grpc_test.Echo.service, { Greet });

function run(host, port) {

  server.bind(`${host}:${port}`, grpc.ServerCredentials.createInsecure());
  console.log(`start server ${host}:${port}`);
  server.start();
}

const host = '127.0.0.1';
const port = '50051';
run(host, port);
