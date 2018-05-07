(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* JSON-RPC Client *)

module D = Debug.Make(struct let name = "jsonrpc_client" end)
open D

exception Timeout

let json_rpc_max_len = ref 65536 (* Arbitrary maximum length of RPC response *)
let json_rpc_read_timeout = ref "60000000000"
let json_rpc_write_timeout = ref "60000000000"

let to_s  s = (Int64.to_float s) *. 1e-9

(* Read the entire contents of the fd, of unknown length *)
let timeout_read fd timeout =
	let buf = Buffer.create !json_rpc_max_len in
	let rec inner max_time max_bytes =
		let c = Mtime_clock.counter () in
		let get_used_time counter = Mtime.Span.to_uint64_ns (Mtime_clock.count counter) in
		let (ready_to_read, _, _) = Unix.select [fd] [] [] (to_s max_time) in
		if List.mem fd ready_to_read
		then
		begin
			let bytes = Bytes.make 4096 '\000' in
			match Unix.read fd bytes 0 4096 with
				| 0 -> Buffer.contents buf (* EOF *)
				| n ->
					if n > max_bytes
					then
					begin
						debug "exceeding maximum read limit %d, clear buffer" !json_rpc_max_len;
						Buffer.clear buf; (* otherwise might be parse error *)
						Buffer.contents buf
					end
					else
					begin
						let used_time = get_used_time c in
						let remain_time = Int64.sub max_time used_time in
						(* here we want to make an exception in case the time is due but there are still some bytes to read, 
						 * in such case we would like to expire a little to complete the read if possible. *)
						let remain_time' = if remain_time < 0L then 0L else remain_time in
						Buffer.add_subbytes buf bytes 0 n;
						inner remain_time' (max_bytes - n)
					end
				| exception Unix.Unix_error(err,_,_) when err = Unix.EAGAIN || err = Unix.EWOULDBLOCK ->
					let used_time = get_used_time c in
					let remain_time = Int64.sub max_time used_time in
					if remain_time < 0L then raise Timeout
					else inner remain_time max_bytes
		end
		else
		begin
			let used_time = get_used_time c in
			let remain_time = Int64.sub max_time used_time in
			if remain_time < 0L then raise Timeout
			else inner remain_time max_bytes
		end
	in
	inner timeout !json_rpc_max_len

(* Write as many bytes to a file descriptor as possible from data before a given clock time. *)
(* Raises Timeout exception if the number of bytes written is less than the specified length. *)
(* Writes into the file descriptor at the current cursor position. *)
let timeout_write filedesc length data response_time =
	let rec inner_write filedesc data offset length remain_time =
		let c = Mtime_clock.counter () in
		let get_used_time counter = Mtime.Span.to_uint64_ns (Mtime_clock.count counter) in
		let (_, ready_to_write, _) = Unix.select [] [filedesc] [] (to_s remain_time) in 
		if List.mem filedesc ready_to_write then 
		begin
			let bytes_written = 
				(try Unix.single_write filedesc data offset length with 
				| Unix.Unix_error(Unix.EAGAIN,_,_)
				| Unix.Unix_error(Unix.EWOULDBLOCK,_,_) -> 0)
			in
			let new_offset = offset + bytes_written in
			let new_length = length - bytes_written in
			let used_time = get_used_time c in
			let new_remain_time = Int64.sub remain_time used_time in
			if new_length = 0 then ()
			else
			if new_remain_time < 0L then raise Timeout
			else inner_write filedesc data new_offset new_length new_remain_time
		end
		else
		begin
			let used_time = get_used_time c in
			let new_remain_time = Int64.sub remain_time used_time in
			if new_remain_time < 0L then raise Timeout
			else inner_write filedesc data offset length new_remain_time
		end
	in
	inner_write filedesc data 0 length response_time

let with_rpc ?(version=Jsonrpc.V2) ~path ~call () =
	let uri = Uri.of_string (Printf.sprintf "file://%s" path) in
	Open_uri.with_open_uri uri (fun s ->
		Unix.set_nonblock s;
		let req = Bytes.of_string (Jsonrpc.string_of_call ~version call) in
		timeout_write s (Bytes.length req) req (Int64.of_string !json_rpc_write_timeout);
		let res = timeout_read s (Int64.of_string !json_rpc_read_timeout) in
		debug "Response: %s" res;
		Jsonrpc.response_of_string res)
