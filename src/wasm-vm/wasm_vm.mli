
module Contract : sig
    type t

    val make : storage:bytes ->  code:string -> (t, string) result
    (** [make ~storage ~code] creates new contract instance with [storage] and [code]. 
        [storage] can be the initial storage of the updated one
        and [code] is a WebAssembly Text Format for now. *)
end

module Runtime : sig
    type t

    val make : contract:Contract.t -> t
    (** [make contract] instantiates a new contract runtime to be invoked. *)

    val invoke : t -> bytes -> (bytes, string) result
    (** [invoke runtime argument] invokes the entrypoint of the contract
        with a given argument.
        
        The contract will receive two arguments: a pointer to the argument
        in memory and a pointer to the storage. The contract should return
        the size of the new storage that should be on the same place as before. *)
end