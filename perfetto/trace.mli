(** Code for trace.proto *)

(* generated from "../toposim/protos/trace.proto", do not edit *)

(** {2 Types} *)

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

(** {2 Basic values} *)

(** [default_track_event_type ()] is the default value for type [track_event_type] *)
val default_track_event_type : unit -> track_event_type

(** [default_track_event_name_field ()] is the default value for type [track_event_name_field] *)
val default_track_event_name_field : unit -> track_event_name_field

(** [default_track_event ()] is the default value for type [track_event] *)
val default_track_event
  :  ?name_field:track_event_name_field
  -> ?type_:track_event_type option
  -> ?track_uuid:int64 option
  -> unit
  -> track_event

(** [default_process_descriptor ()] is the default value for type [process_descriptor] *)
val default_process_descriptor
  :  ?pid:int32 option
  -> ?process_name:string option
  -> unit
  -> process_descriptor

(** [default_thread_descriptor ()] is the default value for type [thread_descriptor] *)
val default_thread_descriptor
  :  ?pid:int32 option
  -> ?tid:int32 option
  -> ?thread_name:string option
  -> unit
  -> thread_descriptor

(** [default_track_descriptor_static_or_dynamic_name ()] is the default value for type [track_descriptor_static_or_dynamic_name] *)
val default_track_descriptor_static_or_dynamic_name
  :  unit
  -> track_descriptor_static_or_dynamic_name

(** [default_track_descriptor ()] is the default value for type [track_descriptor] *)
val default_track_descriptor
  :  ?uuid:int64 option
  -> ?parent_uuid:int64 option
  -> ?static_or_dynamic_name:track_descriptor_static_or_dynamic_name
  -> ?process:process_descriptor option
  -> ?thread:thread_descriptor option
  -> unit
  -> track_descriptor

(** [default_trace_packet_data ()] is the default value for type [trace_packet_data] *)
val default_trace_packet_data : unit -> trace_packet_data

(** [default_trace_packet_optional_trusted_packet_sequence_id ()] is the default value for type [trace_packet_optional_trusted_packet_sequence_id] *)
val default_trace_packet_optional_trusted_packet_sequence_id
  :  unit
  -> trace_packet_optional_trusted_packet_sequence_id

(** [default_trace_packet ()] is the default value for type [trace_packet] *)
val default_trace_packet
  :  ?timestamp:int64 option
  -> ?data:trace_packet_data
  -> ?optional_trusted_packet_sequence_id:trace_packet_optional_trusted_packet_sequence_id
  -> unit
  -> trace_packet

(** [default_trace ()] is the default value for type [trace] *)
val default_trace : ?packet:trace_packet list -> unit -> trace

(** {2 Protobuf Encoding} *)

(** [encode_pb_track_event_type v encoder] encodes [v] with the given [encoder] *)
val encode_pb_track_event_type : track_event_type -> Pbrt.Encoder.t -> unit

(** [encode_pb_track_event_name_field v encoder] encodes [v] with the given [encoder] *)
val encode_pb_track_event_name_field : track_event_name_field -> Pbrt.Encoder.t -> unit

(** [encode_pb_track_event v encoder] encodes [v] with the given [encoder] *)
val encode_pb_track_event : track_event -> Pbrt.Encoder.t -> unit

(** [encode_pb_process_descriptor v encoder] encodes [v] with the given [encoder] *)
val encode_pb_process_descriptor : process_descriptor -> Pbrt.Encoder.t -> unit

(** [encode_pb_thread_descriptor v encoder] encodes [v] with the given [encoder] *)
val encode_pb_thread_descriptor : thread_descriptor -> Pbrt.Encoder.t -> unit

(** [encode_pb_track_descriptor_static_or_dynamic_name v encoder] encodes [v] with the given [encoder] *)
val encode_pb_track_descriptor_static_or_dynamic_name
  :  track_descriptor_static_or_dynamic_name
  -> Pbrt.Encoder.t
  -> unit

(** [encode_pb_track_descriptor v encoder] encodes [v] with the given [encoder] *)
val encode_pb_track_descriptor : track_descriptor -> Pbrt.Encoder.t -> unit

(** [encode_pb_trace_packet_data v encoder] encodes [v] with the given [encoder] *)
val encode_pb_trace_packet_data : trace_packet_data -> Pbrt.Encoder.t -> unit

(** [encode_pb_trace_packet_optional_trusted_packet_sequence_id v encoder] encodes [v] with the given [encoder] *)
val encode_pb_trace_packet_optional_trusted_packet_sequence_id
  :  trace_packet_optional_trusted_packet_sequence_id
  -> Pbrt.Encoder.t
  -> unit

(** [encode_pb_trace_packet v encoder] encodes [v] with the given [encoder] *)
val encode_pb_trace_packet : trace_packet -> Pbrt.Encoder.t -> unit

(** [encode_pb_trace v encoder] encodes [v] with the given [encoder] *)
val encode_pb_trace : trace -> Pbrt.Encoder.t -> unit

(** {2 Protobuf Decoding} *)

(** [decode_pb_track_event_type decoder] decodes a [track_event_type] binary value from [decoder] *)
val decode_pb_track_event_type : Pbrt.Decoder.t -> track_event_type

(** [decode_pb_track_event_name_field decoder] decodes a [track_event_name_field] binary value from [decoder] *)
val decode_pb_track_event_name_field : Pbrt.Decoder.t -> track_event_name_field

(** [decode_pb_track_event decoder] decodes a [track_event] binary value from [decoder] *)
val decode_pb_track_event : Pbrt.Decoder.t -> track_event

(** [decode_pb_process_descriptor decoder] decodes a [process_descriptor] binary value from [decoder] *)
val decode_pb_process_descriptor : Pbrt.Decoder.t -> process_descriptor

(** [decode_pb_thread_descriptor decoder] decodes a [thread_descriptor] binary value from [decoder] *)
val decode_pb_thread_descriptor : Pbrt.Decoder.t -> thread_descriptor

(** [decode_pb_track_descriptor_static_or_dynamic_name decoder] decodes a [track_descriptor_static_or_dynamic_name] binary value from [decoder] *)
val decode_pb_track_descriptor_static_or_dynamic_name
  :  Pbrt.Decoder.t
  -> track_descriptor_static_or_dynamic_name

(** [decode_pb_track_descriptor decoder] decodes a [track_descriptor] binary value from [decoder] *)
val decode_pb_track_descriptor : Pbrt.Decoder.t -> track_descriptor

(** [decode_pb_trace_packet_data decoder] decodes a [trace_packet_data] binary value from [decoder] *)
val decode_pb_trace_packet_data : Pbrt.Decoder.t -> trace_packet_data

(** [decode_pb_trace_packet_optional_trusted_packet_sequence_id decoder] decodes a [trace_packet_optional_trusted_packet_sequence_id] binary value from [decoder] *)
val decode_pb_trace_packet_optional_trusted_packet_sequence_id
  :  Pbrt.Decoder.t
  -> trace_packet_optional_trusted_packet_sequence_id

(** [decode_pb_trace_packet decoder] decodes a [trace_packet] binary value from [decoder] *)
val decode_pb_trace_packet : Pbrt.Decoder.t -> trace_packet

(** [decode_pb_trace decoder] decodes a [trace] binary value from [decoder] *)
val decode_pb_trace : Pbrt.Decoder.t -> trace
