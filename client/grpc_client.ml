module Log = Log.Export

exception Connection_error = Pool.Connection_error

let make = Client.make
