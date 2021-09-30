open Core_kernel
open Bap.Std
open Regular.Std
open Bap_knowledge
open Bap_core_theory
open Monads.Std
open Bap_plugins.Std
open Bap_knowledge

let package = "superset-cache-guide"

let knowledge_reader = Data.Read.create
    ~of_bigstring:Knowledge.of_bigstring ()

let knowledge_writer = Data.Write.create
    ~to_bigstring:Knowledge.to_bigstring ()

let knowledge_cache () =
  Data.Cache.Service.request
    knowledge_reader
    knowledge_writer
            
module Cache_metadata = struct
  type t = string String.Map.t [@@deriving sexp, bin_io]
  let equal = String.Map.equal String.equal
  let set = String.Map.set
  let empty = String.Map.empty
  let fold = String.Map.fold
end

let cache_metadata_t =
  Knowledge.Domain.optional
    ~inspect:Cache_metadata.sexp_of_t ~equal:Cache_metadata.equal
    "cache_metadata_t"

let cache_persistent =
  Knowledge.Persistent.of_binable
    (module struct type t = Cache_metadata.t option [@@deriving bin_io] end)

let digests =
  let attr ty persistent name desc =
    let open Theory.Program in
    Knowledge.Class.property ~package cls name ty
      ~persistent ~public:true ~desc in
  attr cache_metadata_t cache_persistent "cache_metadata_t"
    "Information about what other items can be looked up in the cache"

(* Retrieve the metadata of all digests *)
let with_digests f =
  Toplevel.set @@ Knowledge.load "superset-cache-metadata";
  let open KB.Syntax in
  let guide = KB.Symbol.intern "cache_map" Theory.Program.cls in
  let ds = Toplevel.eval digests guide in
  f ds
  
let cache_corpus_metrics ds =
  match ds with
  | Some ds -> 
     Cache_metadata.fold ds ~init:[] ~f:(fun ~key ~data l ->
         let digest = Data.Cache.Digest.of_string data in
         let cache = knowledge_cache () in
         match Data.Cache.load cache digest with
         | Some state ->
            Toplevel.set state;
            let r = Metrics.get_summary () in
            r :: l
         | None -> l
       )
  | None -> []
