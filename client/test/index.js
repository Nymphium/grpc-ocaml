const grpc = require('@grpc/grpc-js');
const protoLoader = require('@grpc/proto-loader');

const PROTO_PATH = __dirname + '/proto.proto'
const packageDefinition = protoLoader.loadSync(PROTO_PATH)
const proto = grpc.loadPackageDefinition(packageDefinition)

const Greet = (call, callback) => {
  console.log(`received message: ${JSON.stringify(call.request)}`);
  callback(null, { message: `${call.request.message}` });
}

const Unit = (call, callback) => {
  console.log(`received message: ${JSON.stringify(call.request)}`);
  callback(null, {});
}

const server = new grpc.Server();
server.addService(proto.grpc_test.Echo.service, { Greet, Unit });

function run(host, port) {

  server.bindAsync(`${host}:${port}`, grpc.ServerCredentials.createInsecure(), (err, result) => {
    if (!err) {
      setTimeout(() => { server.forceShutdown(); }, 10000)
      console.log(`start server ${host}:${port}`);
      server.start();
    } else { 
      console.error(err);
     }
  });
}

const host = '0.0.0.0';
const port = '50051';
run(host, port);
