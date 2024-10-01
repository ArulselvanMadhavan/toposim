[@@@ocaml.warning "-27-30-39-44"]

type track_event_type =
  | Type_unspecified
  | Type_slice_begin
  | Type_slice_end
  | Type_instant
  | Type_counter

type track_event_name_field =
  | Name_iid of int64
  | Name of string

and track_event =
  { name_field : track_event_name_field
  ; type_ : track_event_type option
  ; track_uuid : int64 option
  }

type process_descriptor =
  { pid : int32 option
  ; process_name : string option
  }

type thread_descriptor =
  { pid : int32 option
  ; tid : int32 option
  ; thread_name : string option
  }

type track_descriptor_static_or_dynamic_name =
  | Name of string
  | Static_name of string

and track_descriptor =
  { uuid : int64 option
  ; parent_uuid : int64 option
  ; static_or_dynamic_name : track_descriptor_static_or_dynamic_name
  ; process : process_descriptor option
  ; thread : thread_descriptor option
  }

type trace_packet_data =
  | Track_event of track_event
  | Track_descriptor of track_descriptor

and trace_packet_optional_trusted_packet_sequence_id =
  | Trusted_packet_sequence_id of int32

and trace_packet =
  { timestamp : int64 option
  ; data : trace_packet_data
  ; optional_trusted_packet_sequence_id : trace_packet_optional_trusted_packet_sequence_id
  }

type trace = { packet : trace_packet list }

let rec default_track_event_type () : track_event_type = Type_unspecified

let rec default_track_event_name_field () : track_event_name_field = Name_iid 0L

and default_track_event
  ?(name_field : track_event_name_field = Name_iid 0L)
  ?(type_ : track_event_type option = None)
  ?(track_uuid : int64 option = None)
  ()
  : track_event
  =
  { name_field; type_; track_uuid }
;;

let rec default_process_descriptor
  ?(pid : int32 option = None)
  ?(process_name : string option = None)
  ()
  : process_descriptor
  =
  { pid; process_name }
;;

let rec default_thread_descriptor
  ?(pid : int32 option = None)
  ?(tid : int32 option = None)
  ?(thread_name : string option = None)
  ()
  : thread_descriptor
  =
  { pid; tid; thread_name }
;;

let rec default_track_descriptor_static_or_dynamic_name ()
  : track_descriptor_static_or_dynamic_name
  =
  Name ""

and default_track_descriptor
  ?(uuid : int64 option = None)
  ?(parent_uuid : int64 option = None)
  ?(static_or_dynamic_name : track_descriptor_static_or_dynamic_name = Name "")
  ?(process : process_descriptor option = None)
  ?(thread : thread_descriptor option = None)
  ()
  : track_descriptor
  =
  { uuid; parent_uuid; static_or_dynamic_name; process; thread }
;;

let rec default_trace_packet_data () : trace_packet_data =
  Track_event (default_track_event ())

and default_trace_packet_optional_trusted_packet_sequence_id ()
  : trace_packet_optional_trusted_packet_sequence_id
  =
  Trusted_packet_sequence_id 0l

and default_trace_packet
  ?(timestamp : int64 option = None)
  ?(data : trace_packet_data = Track_event (default_track_event ()))
  ?(optional_trusted_packet_sequence_id : trace_packet_optional_trusted_packet_sequence_id =
    Trusted_packet_sequence_id 0l)
  ()
  : trace_packet
  =
  { timestamp; data; optional_trusted_packet_sequence_id }
;;

let rec default_trace ?(packet : trace_packet list = []) () : trace = { packet }

type track_event_mutable =
  { mutable name_field : track_event_name_field
  ; mutable type_ : track_event_type option
  ; mutable track_uuid : int64 option
  }

let default_track_event_mutable () : track_event_mutable =
  { name_field = Name_iid 0L; type_ = None; track_uuid = None }
;;

type process_descriptor_mutable =
  { mutable pid : int32 option
  ; mutable process_name : string option
  }

let default_process_descriptor_mutable () : process_descriptor_mutable =
  { pid = None; process_name = None }
;;

type thread_descriptor_mutable =
  { mutable pid : int32 option
  ; mutable tid : int32 option
  ; mutable thread_name : string option
  }

let default_thread_descriptor_mutable () : thread_descriptor_mutable =
  { pid = None; tid = None; thread_name = None }
;;

type track_descriptor_mutable =
  { mutable uuid : int64 option
  ; mutable parent_uuid : int64 option
  ; mutable static_or_dynamic_name : track_descriptor_static_or_dynamic_name
  ; mutable process : process_descriptor option
  ; mutable thread : thread_descriptor option
  }

let default_track_descriptor_mutable () : track_descriptor_mutable =
  { uuid = None
  ; parent_uuid = None
  ; static_or_dynamic_name = Name ""
  ; process = None
  ; thread = None
  }
;;

type trace_packet_mutable =
  { mutable timestamp : int64 option
  ; mutable data : trace_packet_data
  ; mutable optional_trusted_packet_sequence_id :
      trace_packet_optional_trusted_packet_sequence_id
  }

let default_trace_packet_mutable () : trace_packet_mutable =
  { timestamp = None
  ; data = Track_event (default_track_event ())
  ; optional_trusted_packet_sequence_id = Trusted_packet_sequence_id 0l
  }
;;

type trace_mutable = { mutable packet : trace_packet list }

let default_trace_mutable () : trace_mutable = { packet = [] }

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_track_event_type (v : track_event_type) encoder =
  match v with
  | Type_unspecified -> Pbrt.Encoder.int_as_varint 0 encoder
  | Type_slice_begin -> Pbrt.Encoder.int_as_varint 1 encoder
  | Type_slice_end -> Pbrt.Encoder.int_as_varint 2 encoder
  | Type_instant -> Pbrt.Encoder.int_as_varint 3 encoder
  | Type_counter -> Pbrt.Encoder.int_as_varint 4 encoder
;;

let rec encode_pb_track_event_name_field (v : track_event_name_field) encoder =
  match v with
  | Name_iid x ->
    Pbrt.Encoder.int64_as_varint x encoder;
    Pbrt.Encoder.key 10 Pbrt.Varint encoder
  | Name x ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 23 Pbrt.Bytes encoder

and encode_pb_track_event (v : track_event) encoder =
  (match v.name_field with
   | Name_iid x ->
     Pbrt.Encoder.int64_as_varint x encoder;
     Pbrt.Encoder.key 10 Pbrt.Varint encoder
   | Name x ->
     Pbrt.Encoder.string x encoder;
     Pbrt.Encoder.key 23 Pbrt.Bytes encoder);
  (match v.type_ with
   | Some x ->
     encode_pb_track_event_type x encoder;
     Pbrt.Encoder.key 9 Pbrt.Varint encoder
   | None -> ());
  (match v.track_uuid with
   | Some x ->
     Pbrt.Encoder.int64_as_varint x encoder;
     Pbrt.Encoder.key 11 Pbrt.Varint encoder
   | None -> ());
  ()
;;

let rec encode_pb_process_descriptor (v : process_descriptor) encoder =
  (match v.pid with
   | Some x ->
     Pbrt.Encoder.int32_as_varint x encoder;
     Pbrt.Encoder.key 1 Pbrt.Varint encoder
   | None -> ());
  (match v.process_name with
   | Some x ->
     Pbrt.Encoder.string x encoder;
     Pbrt.Encoder.key 6 Pbrt.Bytes encoder
   | None -> ());
  ()
;;

let rec encode_pb_thread_descriptor (v : thread_descriptor) encoder =
  (match v.pid with
   | Some x ->
     Pbrt.Encoder.int32_as_varint x encoder;
     Pbrt.Encoder.key 1 Pbrt.Varint encoder
   | None -> ());
  (match v.tid with
   | Some x ->
     Pbrt.Encoder.int32_as_varint x encoder;
     Pbrt.Encoder.key 2 Pbrt.Varint encoder
   | None -> ());
  (match v.thread_name with
   | Some x ->
     Pbrt.Encoder.string x encoder;
     Pbrt.Encoder.key 5 Pbrt.Bytes encoder
   | None -> ());
  ()
;;

let rec encode_pb_track_descriptor_static_or_dynamic_name
  (v : track_descriptor_static_or_dynamic_name)
  encoder
  =
  match v with
  | Name x ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder
  | Static_name x ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 10 Pbrt.Bytes encoder

and encode_pb_track_descriptor (v : track_descriptor) encoder =
  (match v.uuid with
   | Some x ->
     Pbrt.Encoder.int64_as_varint x encoder;
     Pbrt.Encoder.key 1 Pbrt.Varint encoder
   | None -> ());
  (match v.parent_uuid with
   | Some x ->
     Pbrt.Encoder.int64_as_varint x encoder;
     Pbrt.Encoder.key 5 Pbrt.Varint encoder
   | None -> ());
  (match v.static_or_dynamic_name with
   | Name x ->
     Pbrt.Encoder.string x encoder;
     Pbrt.Encoder.key 2 Pbrt.Bytes encoder
   | Static_name x ->
     Pbrt.Encoder.string x encoder;
     Pbrt.Encoder.key 10 Pbrt.Bytes encoder);
  (match v.process with
   | Some x ->
     Pbrt.Encoder.nested encode_pb_process_descriptor x encoder;
     Pbrt.Encoder.key 3 Pbrt.Bytes encoder
   | None -> ());
  (match v.thread with
   | Some x ->
     Pbrt.Encoder.nested encode_pb_thread_descriptor x encoder;
     Pbrt.Encoder.key 4 Pbrt.Bytes encoder
   | None -> ());
  ()
;;

let rec encode_pb_trace_packet_data (v : trace_packet_data) encoder =
  match v with
  | Track_event x ->
    Pbrt.Encoder.nested encode_pb_track_event x encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder
  | Track_descriptor x ->
    Pbrt.Encoder.nested encode_pb_track_descriptor x encoder;
    Pbrt.Encoder.key 60 Pbrt.Bytes encoder

and encode_pb_trace_packet_optional_trusted_packet_sequence_id
  (v : trace_packet_optional_trusted_packet_sequence_id)
  encoder
  =
  match v with
  | Trusted_packet_sequence_id x ->
    Pbrt.Encoder.int32_as_varint x encoder;
    Pbrt.Encoder.key 10 Pbrt.Varint encoder

and encode_pb_trace_packet (v : trace_packet) encoder =
  (match v.timestamp with
   | Some x ->
     Pbrt.Encoder.int64_as_varint x encoder;
     Pbrt.Encoder.key 8 Pbrt.Varint encoder
   | None -> ());
  (match v.data with
   | Track_event x ->
     Pbrt.Encoder.nested encode_pb_track_event x encoder;
     Pbrt.Encoder.key 11 Pbrt.Bytes encoder
   | Track_descriptor x ->
     Pbrt.Encoder.nested encode_pb_track_descriptor x encoder;
     Pbrt.Encoder.key 60 Pbrt.Bytes encoder);
  (match v.optional_trusted_packet_sequence_id with
   | Trusted_packet_sequence_id x ->
     Pbrt.Encoder.int32_as_varint x encoder;
     Pbrt.Encoder.key 10 Pbrt.Varint encoder);
  ()
;;

let rec encode_pb_trace (v : trace) encoder =
  Pbrt.List_util.rev_iter_with
    (fun x encoder ->
      Pbrt.Encoder.nested encode_pb_trace_packet x encoder;
      Pbrt.Encoder.key 1 Pbrt.Bytes encoder)
    v.packet
    encoder;
  ()
;;

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_track_event_type d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Type_unspecified : track_event_type)
  | 1 -> (Type_slice_begin : track_event_type)
  | 2 -> (Type_slice_end : track_event_type)
  | 3 -> (Type_instant : track_event_type)
  | 4 -> (Type_counter : track_event_type)
  | _ -> Pbrt.Decoder.malformed_variant "track_event_type"
;;

let rec decode_pb_track_event_name_field d =
  let rec loop () =
    let ret : track_event_name_field =
      match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "track_event_name_field"
      | Some (10, _) ->
        (Name_iid (Pbrt.Decoder.int64_as_varint d) : track_event_name_field)
      | Some (23, _) -> (Name (Pbrt.Decoder.string d) : track_event_name_field)
      | Some (n, payload_kind) ->
        Pbrt.Decoder.skip d payload_kind;
        loop ()
    in
    ret
  in
  loop ()

and decode_pb_track_event d =
  let v = default_track_event_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (10, Pbrt.Varint) -> v.name_field <- Name_iid (Pbrt.Decoder.int64_as_varint d)
    | Some (10, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(track_event), field(10)" pk
    | Some (23, Pbrt.Bytes) -> v.name_field <- Name (Pbrt.Decoder.string d)
    | Some (23, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(track_event), field(23)" pk
    | Some (9, Pbrt.Varint) -> v.type_ <- Some (decode_pb_track_event_type d)
    | Some (9, pk) -> Pbrt.Decoder.unexpected_payload "Message(track_event), field(9)" pk
    | Some (11, Pbrt.Varint) -> v.track_uuid <- Some (Pbrt.Decoder.int64_as_varint d)
    | Some (11, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(track_event), field(11)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({ name_field = v.name_field; type_ = v.type_; track_uuid = v.track_uuid }
   : track_event)
;;

let rec decode_pb_process_descriptor d =
  let v = default_process_descriptor_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (1, Pbrt.Varint) -> v.pid <- Some (Pbrt.Decoder.int32_as_varint d)
    | Some (1, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(process_descriptor), field(1)" pk
    | Some (6, Pbrt.Bytes) -> v.process_name <- Some (Pbrt.Decoder.string d)
    | Some (6, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(process_descriptor), field(6)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({ pid = v.pid; process_name = v.process_name } : process_descriptor)
;;

let rec decode_pb_thread_descriptor d =
  let v = default_thread_descriptor_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (1, Pbrt.Varint) -> v.pid <- Some (Pbrt.Decoder.int32_as_varint d)
    | Some (1, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(thread_descriptor), field(1)" pk
    | Some (2, Pbrt.Varint) -> v.tid <- Some (Pbrt.Decoder.int32_as_varint d)
    | Some (2, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(thread_descriptor), field(2)" pk
    | Some (5, Pbrt.Bytes) -> v.thread_name <- Some (Pbrt.Decoder.string d)
    | Some (5, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(thread_descriptor), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({ pid = v.pid; tid = v.tid; thread_name = v.thread_name } : thread_descriptor)
;;

let rec decode_pb_track_descriptor_static_or_dynamic_name d =
  let rec loop () =
    let ret : track_descriptor_static_or_dynamic_name =
      match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "track_descriptor_static_or_dynamic_name"
      | Some (2, _) ->
        (Name (Pbrt.Decoder.string d) : track_descriptor_static_or_dynamic_name)
      | Some (10, _) ->
        (Static_name (Pbrt.Decoder.string d) : track_descriptor_static_or_dynamic_name)
      | Some (n, payload_kind) ->
        Pbrt.Decoder.skip d payload_kind;
        loop ()
    in
    ret
  in
  loop ()

and decode_pb_track_descriptor d =
  let v = default_track_descriptor_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (1, Pbrt.Varint) -> v.uuid <- Some (Pbrt.Decoder.int64_as_varint d)
    | Some (1, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(track_descriptor), field(1)" pk
    | Some (5, Pbrt.Varint) -> v.parent_uuid <- Some (Pbrt.Decoder.int64_as_varint d)
    | Some (5, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(track_descriptor), field(5)" pk
    | Some (2, Pbrt.Bytes) -> v.static_or_dynamic_name <- Name (Pbrt.Decoder.string d)
    | Some (2, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(track_descriptor), field(2)" pk
    | Some (10, Pbrt.Bytes) ->
      v.static_or_dynamic_name <- Static_name (Pbrt.Decoder.string d)
    | Some (10, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(track_descriptor), field(10)" pk
    | Some (3, Pbrt.Bytes) ->
      v.process <- Some (decode_pb_process_descriptor (Pbrt.Decoder.nested d))
    | Some (3, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(track_descriptor), field(3)" pk
    | Some (4, Pbrt.Bytes) ->
      v.thread <- Some (decode_pb_thread_descriptor (Pbrt.Decoder.nested d))
    | Some (4, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(track_descriptor), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({ uuid = v.uuid
   ; parent_uuid = v.parent_uuid
   ; static_or_dynamic_name = v.static_or_dynamic_name
   ; process = v.process
   ; thread = v.thread
   }
   : track_descriptor)
;;

let rec decode_pb_trace_packet_data d =
  let rec loop () =
    let ret : trace_packet_data =
      match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "trace_packet_data"
      | Some (11, _) ->
        (Track_event (decode_pb_track_event (Pbrt.Decoder.nested d)) : trace_packet_data)
      | Some (60, _) ->
        (Track_descriptor (decode_pb_track_descriptor (Pbrt.Decoder.nested d))
         : trace_packet_data)
      | Some (n, payload_kind) ->
        Pbrt.Decoder.skip d payload_kind;
        loop ()
    in
    ret
  in
  loop ()

and decode_pb_trace_packet_optional_trusted_packet_sequence_id d =
  let rec loop () =
    let ret : trace_packet_optional_trusted_packet_sequence_id =
      match Pbrt.Decoder.key d with
      | None ->
        Pbrt.Decoder.malformed_variant "trace_packet_optional_trusted_packet_sequence_id"
      | Some (10, _) ->
        (Trusted_packet_sequence_id (Pbrt.Decoder.int32_as_varint d)
         : trace_packet_optional_trusted_packet_sequence_id)
      | Some (n, payload_kind) ->
        Pbrt.Decoder.skip d payload_kind;
        loop ()
    in
    ret
  in
  loop ()

and decode_pb_trace_packet d =
  let v = default_trace_packet_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      ();
      continue__ := false
    | Some (8, Pbrt.Varint) -> v.timestamp <- Some (Pbrt.Decoder.int64_as_varint d)
    | Some (8, pk) -> Pbrt.Decoder.unexpected_payload "Message(trace_packet), field(8)" pk
    | Some (11, Pbrt.Bytes) ->
      v.data <- Track_event (decode_pb_track_event (Pbrt.Decoder.nested d))
    | Some (11, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(trace_packet), field(11)" pk
    | Some (60, Pbrt.Bytes) ->
      v.data <- Track_descriptor (decode_pb_track_descriptor (Pbrt.Decoder.nested d))
    | Some (60, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(trace_packet), field(60)" pk
    | Some (10, Pbrt.Varint) ->
      v.optional_trusted_packet_sequence_id
      <- Trusted_packet_sequence_id (Pbrt.Decoder.int32_as_varint d)
    | Some (10, pk) ->
      Pbrt.Decoder.unexpected_payload "Message(trace_packet), field(10)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({ timestamp = v.timestamp
   ; data = v.data
   ; optional_trusted_packet_sequence_id = v.optional_trusted_packet_sequence_id
   }
   : trace_packet)
;;

let rec decode_pb_trace d =
  let v = default_trace_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
      v.packet <- List.rev v.packet;
      continue__ := false
    | Some (1, Pbrt.Bytes) ->
      v.packet <- decode_pb_trace_packet (Pbrt.Decoder.nested d) :: v.packet
    | Some (1, pk) -> Pbrt.Decoder.unexpected_payload "Message(trace), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({ packet = v.packet } : trace)
;;
