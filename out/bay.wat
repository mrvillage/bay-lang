(module
(import "wasi_snapshot_preview1" "proc_exit" (func $proc_exit (param i32)))
(import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
(memory (export "memory") 1)
(func (export "_start") call $fn2)

(data (i32.const 0) "00000000000\n")
(func $print_i32 (param $n i32)
  (local $i i32)
  (local $digit i32)
  (local $is_negative i32)

  (if (i32.lt_s (local.get $n) (i32.const 0))
    (then
      (local.set $is_negative (i32.const 1))
      (local.set $n (i32.sub (i32.const 0) (local.get $n)))
    )
  )

  ;; we start at position 11 (the newline)
  (local.set $i (i32.const 11))

  ;; we extract digits in reverse order
  (loop $loop
    (local.set $i (i32.sub (local.get $i) (i32.const 1)))
    (local.set $digit (i32.rem_u (local.get $n) (i32.const 10)))
    (i32.store8
      (local.get $i)
      (i32.add (local.get $digit) (i32.const 48))
    )
    (local.set $n (i32.div_u (local.get $n) (i32.const 10)))
    (br_if $loop (i32.ne (local.get $n) (i32.const 0)))
  )

  ;; if negative, we add a minus sign
  (if (local.get $is_negative)
    (then
      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
      (i32.store8 (local.get $i) (i32.const 45))
    )
  )

  ;; we store the pointer to our data
  (i32.store (i32.const 20) (local.get $i))
  ;; and the length of our data
  (i32.store (i32.const 24) (i32.sub (i32.const 12) (local.get $i)))

  (call $fd_write
    (i32.const 1)
    (i32.const 20)
    (i32.const 1)
    (i32.const 28)
  )
  drop
)

(global $ptr (mut i32) (i32.const 256))
(global $end (mut i32) (i32.const 65536))
(global $tree_root (mut i32) (i32.const 0))
(func $alloc (param $size i32) (result i32)
  (local $allocated_ptr i32)
  ;; size should be at least 4 bytes (next)
  (if (i32.lt_u (local.get $size) (i32.const 4))
  (then (local.set $size (i32.const 4))))
  ;; size should be a multiple of 4
  (if (i32.ne (i32.rem_u (local.get $size) (i32.const 4)) (i32.const 0))
    (then
      (local.set $size
        (i32.add
          (local.get $size)
          (i32.sub (i32.const 4) (i32.rem_u (local.get $size) (i32.const 4)))
        )
      )
    )
  )
  ;; now we want to ensure that we have enough space to store the size of the chunk
  (local.set $size (i32.add (local.get $size) (i32.const 4)))
  ;; we want to start from $tree_root, if it's 0 then we allocate from $ptr
  ;; otherwise, we find a suitable chunk in the tree
  (i32.eqz (global.get $tree_root))
  (if (result i32)
    (then
      (call $alloc_ptr (local.get $size))
    )
    (else
      (call $find_suitable_chunk (local.get $size))
    )
  )
  (local.tee $allocated_ptr)
  (call $print_i32 (local.get $allocated_ptr))
  return
)
(func $alloc_ptr (param $size i32) (result i32)
  (local $allocated_ptr i32)
  ;; while ptr + size > end, grow memory
  ;; if end + page size overflows, raise out of memory
  (loop $grow_loop
    (if (i32.gt_u (i32.add (global.get $ptr) (local.get $size)) (global.get $end))
      (then
        (if (i32.eq (memory.grow (i32.const 1)) (i32.const -1))
          (then
            ;; out of memory
            (call $proc_exit (i32.const 1))
          )
        )
        (global.set $end (i32.add (global.get $end) (i32.const 65536)))
        (br $grow_loop)
      )
    )
  )
  (global.get $ptr)
  (global.set $ptr (i32.add (global.get $ptr) (local.get $size)))
  (local.set $allocated_ptr)
  (i32.store (local.get $allocated_ptr) (local.get $size))
  (i32.add (local.get $allocated_ptr) (i32.const 4))
  return
)
(func $find_suitable_chunk (param $size i32) (result i32)
  (local $prev i32)
  (local $current i32)
  (local $new_chunk i32)
  (local $chunk_size i32)
  ;; traverse the linked list of free chunks to find a suitably sized one
  ;; if found, remove it from the tree and return its pointer
  (global.get $tree_root)
  (local.tee $current)
  (local.set $prev (i32.const 0))
  (loop $next_chunk
    ;; load chunk size
    (local.set $chunk_size (i32.load (local.get $current)))
    (if (i32.ge_u (local.get $chunk_size) (local.get $size))
      (then
        ;; suitable chunk found
        (if (i32.lt_u (local.get $chunk_size) (i32.add (local.get $size) (i32.const 8)))
          ;; exact fit or too small to split, remove it from tree
          (then
            (if (i32.eqz (local.get $prev))
              ;; removing root
              (then
                (global.set $tree_root (i32.load (i32.add (local.get $current) (i32.const 4))))
              )
              ;; removing non-root
              (else
                (i32.store
                (i32.add (local.get $prev) (i32.const 4))
                (i32.load (i32.add (local.get $current) (i32.const 4)))
                )
              )
            )
            (i32.add (local.get $current) (i32.const 4))
            return
          )
          ;; larger chunk, split it
          (else
            (local.set $new_chunk (i32.add (local.get $current) (local.get $size)))
            ;; store new chunk size
            (i32.store (local.get $new_chunk) (i32.sub (local.get $chunk_size) (local.get $size)))
            (i32.store (local.get $current) (local.get $size))
            ;; link new chunk to next
            (i32.store
            (i32.add (local.get $new_chunk) (i32.const 4))
            (i32.load (i32.add (local.get $current) (i32.const 4)))
            )
            ;; update previous chunk or tree root to point to new chunk
            (if (i32.eqz (local.get $prev))
              ;; updating root
              (then
                (global.set $tree_root (local.get $new_chunk))
              )
              ;; updating non-root
              (else
                (i32.store
                (i32.add (local.get $prev) (i32.const 4))
                (local.get $new_chunk)
                )
              )
            )
            (i32.add (local.get $current) (i32.const 4))
            return
          )
        )
      )
      (else
        ;; move to next chunk
        (local.set $prev (local.get $current))
        (local.set $current (i32.load (i32.add (local.get $current) (i32.const 4))))
        (if (local.get $current)
          (br $next_chunk)
        )
      )
    )
  )
  ;; no suitable chunk found, allocate from ptr
  return (call $alloc_ptr (local.get $size))
)

(func $dealloc (param $chunk i32)
  (local $prev i32)
  (local $current i32)
  ;; we get the original chunk pointer
  (local.set $chunk (i32.sub (local.get $chunk ) (i32.const 4)))
  ;; the free list is sorted by address, we need to find the correct place to insert
  ;; then (recursively) check if we can merge with adjacent free chunks
  (local.set $prev (i32.const 0))
  (local.set $current (global.get $tree_root))
  (loop $find_place
    (if (i32.eqz (local.get $current))
      (then
        ;; reached end of list, insert here
        (call $left_merge_or_insert (local.get $prev) (local.get $chunk))
        drop
        return
      )
      (else
        (if (i32.lt_u (local.get $current) (local.get $chunk))
          (then
            ;; move to next
            (local.set $prev (local.get $current))
            (local.set $current (i32.load (i32.add (local.get $current) (i32.const 4))))
            (br $find_place)
          )
          (else
            ;; found place to insert before current
            (local.set $chunk (call $left_merge_or_insert (local.get $prev) (local.get $chunk)))
            ;; try to merge with current as well
            (call $merge (local.get $chunk) (local.get $current))
            return
          )
        )
      )
    )
  )
)
(func $left_merge_or_insert (param $prev i32) (param $chunk i32) (result i32)
  ;; try to merge with previous chunk
  (i32.and
    (local.get $prev)
    (i32.eq
      (i32.add (local.get $prev) (i32.load (local.get $prev)))
      (local.get $chunk)
    )
  ) 
  (if (result i32)
    (then
      ;; merge with previous
      (i32.store
        (local.get $prev)
        (i32.add
          (i32.load (local.get $prev))
          (i32.load (local.get $chunk))
        )
      )
      (local.get $prev)
    )
    (else
      ;; no merge, insert chunk
      (i32.store
        (i32.add (local.get $chunk) (i32.const 4))
        (if (result i32) (local.get $prev)
          (then (i32.load (i32.add (local.get $prev) (i32.const 4))))
          (else (global.get $tree_root))
        )
      )
      (if (local.get $prev)
        (then
          (i32.store
            (i32.add (local.get $prev) (i32.const 4))
            (local.get $chunk)
          )
        )
        (else
          (global.set $tree_root (local.get $chunk))
        )
      )
      (local.get $chunk)
    )
  )
)
(func $merge (param $left i32) (param $right i32)
  ;; try to merge left and right chunks
  (i32.and
    (local.get $left)
    (i32.eq
      (i32.add (local.get $left) (i32.load (local.get $left)))
      (local.get $right)
    )
  )
  (if
    (then
      ;; merge left and right
      (i32.store
        (local.get $left)
        (i32.add
          (i32.load (local.get $left))
          (i32.load (local.get $right))
        )
      )
      ;; update next pointer
      (i32.store
        (i32.add (local.get $left) (i32.const 4))
        (i32.load (i32.add (local.get $right) (i32.const 4)))
      )
    )
  )
)
(func $destruct8 (param $var1 i32)
  local.get $var1
  i32.const -2147483648
  i32.ne
  if
  local.get $var1
  call $destruct6
  end
)
(type $fn_type9 (func (param i32) ))
(type $fn_type10 (func ))
(func $destruct6 (param $var1 i32)
  local.get $var1
  i32.const 4
  i32.add
  i32.load
  call $destruct8
  local.get $var1
  call $dealloc
)
  (table 2 funcref)
  (elem (i32.const 0) $fn2 $fn4 )
(func $fn2 (local $var1 i32) (local $var3 i32) 
  i32.const 8
call $alloc
  local.tee $var1
  local.get $var1
  i32.const 0
  i32.add
  i32.const 1000
  i32.store
  local.get $var1
  i32.const 4
  i32.add
  i32.const 8
call $alloc
  local.tee $var1
  local.get $var1
  i32.const 0
  i32.add
  i32.const 2
  i32.store
  local.get $var1
  i32.const 4
  i32.add
  i32.const -2147483648
  i32.store
  i32.store
  local.set $var3
  local.get $var3
  i32.const 0
  i32.add
  i32.load
  i32.const 1
  call_indirect (type $fn_type9)
  i32.const 8
call $alloc
  local.tee $var1
  local.get $var1
  i32.const 0
  i32.add
  i32.const 1
  i32.store
  local.get $var1
  i32.const 4
  i32.add
  i32.const -2147483648
  i32.store
  call $destruct6
  i32.const 8
call $alloc
  local.tee $var1
  local.get $var1
  i32.const 0
  i32.add
  i32.const 1
  i32.store
  local.get $var1
  i32.const 4
  i32.add
  i32.const -2147483648
  i32.store
  call $destruct6
  i32.const 8
call $alloc
  local.tee $var1
  local.get $var1
  i32.const 0
  i32.add
  i32.const 1
  i32.store
  local.get $var1
  i32.const 4
  i32.add
  i32.const -2147483648
  i32.store
  call $destruct6
  local.get $var3
  call $destruct6
)
(func $fn4 (param i32))
)
