use crate::{
    backends::wasm32::Wasm32Backend,
    prelude::*,
    reprs::ir::{IrType, IrValue},
};

pub fn codegen() -> Result<()> {
    // we want to find the main function, and then only generate IR for the
    // types and values from that function
    let pkg_root = crate::scope::Scope::get_by_id(1).unwrap();
    let main_fn = pkg_root
        .resolve_name_value(&token::Ident {
            value: "main",
            span:  Span::dummy(),
        })
        .map_err(|_| CompileError::NoMainFunction)?;
    match main_fn {
        Value::Fn {
            name,
            params,
            ret_ty,
            ..
        } => {
            if !params.is_empty() {
                return Err(CompileError::error(
                    name.span,
                    "main function must not have parameters",
                ));
            }
            if ret_ty.ty() != &Type::Unit {
                return Err(CompileError::error(
                    name.span,
                    "main function must have return type `()`",
                ));
            }
        },
        _ => return Err(CompileError::NoMainFunction),
    }
    let main_fn = main_fn.into_ir();
    let mut i = 0;
    for val in IrValue::all_mut() {
        if let IrValue::Fn { idx, .. } = val {
            *idx = i;
            i += 1;
        }
    }
    let mut wat = format!(
        r#"(module
(import "wasi_snapshot_preview1" "proc_exit" (func $proc_exit (param i32)))
(import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
(memory (export "memory") 130)
(func (export "_start") (call $fn{} (i32.const 8388608)))

(data (i32.const 8388608) "00000000000\n")
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
  (local.set $i (i32.const 8388619))

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
  (i32.store (i32.const 8388628) (local.get $i))
  ;; and the length of our data
  (i32.store (i32.const 8388632) (i32.sub (i32.const 8388620) (local.get $i)))

  (call $fd_write
    (i32.const 1)
    (i32.const 8388628)
    (i32.const 1)
    (i32.const 8388636)
  )
  drop
)
;; max 39 digits + newline + negative sign = 41 bytes
(data (i32.const 8388864) "0000000000000000000000000000000000000000\n")
(func $print_words (param $n i32) (param $count i32)
  (local $i i32)
  (local $digit i32)
  (local $is_negative i32)
  (local $zero i32)
  (local $work i32)
  (local $ten i32)

  (local.set $n (i32.sub (local.get $n) (i32.mul (local.get $count) (i32.const 4))))

  ;; if single word, defer to print_i32
  (if (i32.eq (local.get $count) (i32.const 1))
    (then
      (i32.load (local.get $n))
      (call $print_i32)
      return
    )
  )

  ;; initialize work
  (local.set $work (call $sc_alloc (i32.mul (local.get $count) (i32.const 4))))
  (local.set $zero (call $sc_alloc (i32.mul (local.get $count) (i32.const 4))))
  (local.set $i (i32.const 0))
  (loop $zero_loop
    (i32.store
      (i32.add (local.get $zero) (i32.mul (local.get $i) (i32.const 4)))
      (i32.const 0)
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $zero_loop (i32.lt_u (local.get $i) (local.get $count)))
  )

  ;; we normalize to positive now
  (if (i32.lt_s (call $range_cmp (local.get $n) (local.get $zero) (local.get $count) (i32.const 0)) (i32.const 0))
    (then
      (local.set $is_negative (i32.const 1))
      ;; does not dealloc zero
      (call $range_neg (local.get $n) (local.get $count))
      drop
    )
  )

  ;; initialize ten
  (local.set $ten (call $sc_alloc (i32.mul (local.get $count) (i32.const 4))))
  (i32.store
    (local.get $ten)
    (i32.const 10)
  )
  (local.set $i (i32.const 1))
  (loop $zero_loop2
    (i32.store
      (i32.add (local.get $ten) (i32.mul (local.get $i) (i32.const 4)))
      (i32.const 0)
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $zero_loop2 (i32.lt_u (local.get $i) (local.get $count)))
  )

  ;; we start at position 41 (the newline)
  (local.set $i (i32.const 8388904))

  ;; we extract digits in reverse order
  (loop $loop
    (local.set $i (i32.sub (local.get $i) (i32.const 1)))
    ;; work = n
    (call $memcpy
      (local.get $work)
      (local.get $n)
      (i32.mul (local.get $count) (i32.const 4))
    )
    ;; work = work % 10
    (call $range_mod (local.get $work) (local.get $ten) (local.get $count))
    drop
    (call $sc_alloc (i32.mul (local.get $count) (i32.const 4))) ;; realloc ten since range_mod deallocs
    drop
    (local.set $digit (i32.load (local.get $work)))
    (i32.store8
      (local.get $i)
      (i32.add (local.get $digit) (i32.const 48))
    )
    (call $range_div (local.get $n) (local.get $ten) (local.get $count))
    drop
    (call $sc_alloc (i32.mul (local.get $count) (i32.const 4))) ;; realloc ten since range_div deallocs
    drop
    (br_if $loop (i32.ne (call $range_cmp (local.get $n) (local.get $zero) (local.get $count) (i32.const 0)) (i32.const 0)))
  )

  ;; if negative, we add a minus sign
  (if (local.get $is_negative)
    (then
      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
      (i32.store8 (local.get $i) (i32.const 45))
    )
  )

  ;; we store the pointer to our data
  (i32.store (i32.const 8388628) (local.get $i))
  ;; and the length of our data
  (i32.store (i32.const 8388632) (i32.sub (i32.const 8388906) (local.get $i)))

  (call $fd_write
    (i32.const 1)
    (i32.const 8388628)
    (i32.const 1)
    (i32.const 8388636)
  )
  drop
)
;; multi-word addition
(func $range_add (param $left i32) (param $right i32) (param $words i32) (result i32)
  (local $i i32)
  (local $carry i32)
  (local $x i32)
  (local $y i32)
  (local $z i32)
  (local $offset i32)
  (local $left_sign i32)
  (local $right_sign i32)
  (local.set $left_sign
    (i32.shr_s
      (i32.load (i32.add (local.get $left) (i32.mul (i32.sub (local.get $words) (i32.const 1)) (i32.const 4))))
      (i32.const 31)
    )
  )
  (local.set $right_sign
    (i32.shr_s
      (i32.load (i32.add (local.get $right) (i32.mul (i32.sub (local.get $words) (i32.const 1)) (i32.const 4))))
      (i32.const 31)
    )
  )
  ;; first word addition, this saves us an addition of an empty carry bit
  (local.get $left)
  (local.tee $x (i32.load (local.get $left)))
  (local.tee $y (i32.load (local.get $right)))
  (local.tee $z (i32.add))
  (i32.store)
  ;; carry = ((x & y) | ((x | y) & ~z)) >> 31
  (local.set $carry
    (i32.shr_u
      (i32.or
        (i32.and (local.get $x) (local.get $y))
        (i32.and
          (i32.or (local.get $x) (local.get $y))
          (i32.xor (local.get $z) (i32.const -1))
        )
      )
      (i32.const 31)
    )
  )
  (local.set $i (i32.const 1))
  (loop $main_loop
    (local.set $offset (i32.mul (local.get $i) (i32.const 4)))
    (i32.add (local.get $left) (local.get $offset))
    (local.tee $x (i32.load (i32.add (local.get $left) (local.get $offset))))
    (local.tee $y (i32.load (i32.add (local.get $right) (local.get $offset))))
    (i32.add)
    (local.tee $z (i32.add (local.get $carry)))
    (i32.store)
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $main_loop (i32.lt_u (local.get $i) (local.get $words)))
    ;; carry = ((x & y) | ((x | y) & ~z)) >> 31
    (local.set $carry
      (i32.shr_u
        (i32.or
          (i32.and (local.get $x) (local.get $y))
          (i32.and
            (i32.or (local.get $x) (local.get $y))
            (i32.xor (local.get $z) (i32.const -1))
          )
        )
        (i32.const 31)
      )
    )
  )
  ;; overflow occurs if both operands have the same sign and the result has a different sign
  (if (i32.and (i32.eq (local.get $left_sign) (local.get $right_sign)) (i32.ne (local.get $left_sign) (i32.shr_s (local.get $z) (i32.const 31))))
    (then
      ;; trap
      unreachable
    )
  )
  (i32.mul (local.get $words) (i32.const 4))
  (call $sc_dealloc)
  (local.get $left)
  return
)
(func $range_sub (param $left i32) (param $right i32) (param $words i32) (result i32)
  (call $range_neg (local.get $right) (local.get $words))
  (call $range_add (local.get $left) (local.get $right) (local.get $words))
  return
)
(func $range_mul (param $left i32) (param $right i32) (param $words i32) (result i32)
  ;; loop variables
  (local $i i32)
  (local $j i32)
  (local $carry i64) ;; k
  (local $product i64) ;; t
  (local $borrow i64) ;; b
  (local $result i32)
  (local $sign i32)
  (local.set $result (call $sc_alloc (i32.mul (local.get $words) (i32.const 8))))
  (local.set $i (i32.const 0))
  ;; initialize result to 0
  (loop $init_loop
    (i32.store
      (i32.add (local.get $result) (i32.mul (local.get $i) (i32.const 4)))
      (i32.const 0)
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $init_loop (i32.lt_u (local.get $i) (i32.mul (local.get $words) (i32.const 2))))
  )
  (local.set $j (i32.const 0))
  (loop $outer_loop ;; j < words
    (local.set $i (i32.const 0))
    (local.set $carry (i64.const 0))
    (loop $inner_loop ;; i < words
      ;; product = carry + left[i] * right[j] + result[i + j]
      (local.set $product
        (i64.add
          (local.get $carry)
          (i64.add 
            (i64.mul
              (i64.extend_i32_u (i32.load (i32.add (local.get $left) (i32.mul (local.get $i) (i32.const 4)))))
              (i64.extend_i32_u (i32.load (i32.add (local.get $right) (i32.mul (local.get $j) (i32.const 4)))))
            )
            (i64.extend_i32_u (i32.load (i32.add (local.get $result) (i32.mul (i32.add (local.get $i) (local.get $j)) (i32.const 4)))))
          )
        )
      )
      ;; store lower 32 bits
      (i32.store
        (i32.add (local.get $result) (i32.mul (i32.add (local.get $i) (local.get $j)) (i32.const 4)))
        (i32.wrap_i64 (local.get $product))
      )
      ;; carry = upper 32 bits
      (local.set $carry (i64.shr_u (local.get $product) (i64.const 32)))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br_if $inner_loop (i32.lt_u (local.get $i) (local.get $words)))
    )
    (i32.store
      (i32.add (local.get $result) (i32.mul (i32.add (local.get $j) (local.get $words)) (i32.const 4)))
      (i32.wrap_i64 (local.get $carry))
    )
    (local.set $j (i32.add (local.get $j) (i32.const 1)))
    (br_if $outer_loop (i32.lt_u (local.get $j) (local.get $words)))
  )
  ;; now, result is the unsigned product, we need to correct
  (if (i32.lt_s
        (i32.load (i32.add (local.get $left) (i32.mul (i32.sub (local.get $words) (i32.const 1)) (i32.const 4))))
        (i32.const 0))
    (then
      (local.set $borrow (i64.const 0))
      (local.set $j (i32.const 0))
      (loop $neg_right_loop
        (local.set $product 
          (i64.sub
            (i64.sub
              (i64.extend_i32_u (i32.load (i32.add (local.get $right) (i32.mul (i32.add (local.get $j) (local.get $words)) (i32.const 4)))))
              (i64.extend_i32_u (i32.load (i32.add (local.get $right) (i32.mul (local.get $j) (i32.const 4)))))
            )
            (local.get $borrow)
          )
        )
        (i32.store
          (i32.add (local.get $result) (i32.mul (i32.add (local.get $j) (local.get $words)) (i32.const 4)))
          (i32.wrap_i64 (local.get $product))
        )
        (local.set $borrow (i64.shr_u (local.get $product) (i64.const 31)))
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br_if $neg_right_loop (i32.lt_u (local.get $j) (local.get $words)))
      )
    )
  )
  (if (i32.lt_s
        (i32.load (i32.add (local.get $right) (i32.mul (i32.sub (local.get $words) (i32.const 1)) (i32.const 4))))
        (i32.const 0))
    (then
      (local.set $borrow (i64.const 0))
      (local.set $i (i32.const 0))
      (loop $neg_left_loop
        (local.set $product 
          (i64.sub
            (i64.sub
              (i64.extend_i32_u (i32.load (i32.add (local.get $left) (i32.mul (i32.add (local.get $i) (local.get $words)) (i32.const 4)))))
              (i64.extend_i32_u (i32.load (i32.add (local.get $left) (i32.mul (local.get $i) (i32.const 4)))))
            )
            (local.get $borrow)
          )
        )
        (i32.store
          (i32.add (local.get $result) (i32.mul (i32.add (local.get $i) (local.get $words)) (i32.const 4)))
          (i32.wrap_i64 (local.get $product))
        )
        (local.set $borrow (i64.shr_u (local.get $product) (i64.const 31)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br_if $neg_left_loop (i32.lt_u (local.get $i) (local.get $words)))
      )
    )
  )

  ;; now, we check if result fits in words
  (local.set $sign 
    (i32.shr_s
      (i32.load (i32.add (local.get $result) (i32.mul (i32.sub (i32.mul (local.get $words) (i32.const 2)) (i32.const 1)) (i32.const 4))))
      (i32.const 31)
    )
  )
  ;; if the upper words are not all the same as the sign, we have overflow
  (if (local.get $sign)
    (then
      (local.set $sign (i32.const -1))
    )
  )
  (local.set $i (i32.const 0))
  (loop $overflow_check
    (if (i32.ne
          (i32.load (i32.add (local.get $result) (i32.mul (i32.add (local.get $i) (local.get $words)) (i32.const 4))))
          (local.get $sign))
      (then
        ;; overflow
        unreachable
      )
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $overflow_check (i32.lt_u (local.get $i) (local.get $words)))
  )
  ;; copy lower words to left
  (call $memcpy
    (local.get $left)
    (local.get $result)
    (i32.mul (local.get $words) (i32.const 4))
  )
  ;; dealloc result
  (call $sc_dealloc (i32.mul (local.get $words) (i32.const 8)))
  ;; dealloc right
  (call $sc_dealloc (i32.mul (local.get $words) (i32.const 4)))
  (local.get $left)
  return
)
(func $range_div (param $left i32) (param $right i32) (param $words i32) (result i32)
  (call $range_div_inner (local.get $left) (local.get $right) (local.get $words) (i32.const 0))
  return
)
(func $range_mod (param $left i32) (param $right i32) (param $words i32) (result i32)
  (call $range_div_inner (local.get $left) (local.get $right) (local.get $words) (i32.const 1))
  return
)
;; mode: 0 = quotient, 1 = remainder
(func $range_div_inner (param $left i32) (param $right i32) (param $words i32) (param $mode i32) (result i32)
  ;; for now, we implement with repeated subtraction since a full multi-word division algorithm is very complex
  (local $cmp i32)
  (local $quotient i32)
  (local $one i32)
  (local $i i32)
  (local $right_copy i32)
  ;; initialize quotient to 0
  (local.set $quotient (call $sc_alloc (i32.mul (local.get $words) (i32.const 4))))
  (local.set $i (i32.const 0))
  (loop $init_loop
    (i32.store
      (i32.add (local.get $quotient) (i32.mul (local.get $i) (i32.const 4)))
      (i32.const 0)
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $init_loop (i32.lt_u (local.get $i) (local.get $words)))
  )
  ;; check for division by zero
  (if (i32.eqz
        (call $range_cmp (local.get $right) (i32.const 0) (local.get $words) (i32.const 0))
      )
    (then
    ;; division by zero, trap
    unreachable
    )
  )
  ;; initial one to 1
  (local.set $one (call $sc_alloc (i32.mul (local.get $words) (i32.const 4))))
  (i32.store
    (local.get $one)
    (i32.const 1)
  )
  (local.set $i (i32.const 1))
  (loop $zero_loop
    (i32.store
      (i32.add (local.get $one) (i32.mul (local.get $i) (i32.const 4)))
      (i32.const 0)
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $zero_loop (i32.lt_u (local.get $i) (local.get $words)))
  )
  ;; initialize right_copy
  (local.set $right_copy (call $sc_alloc (i32.mul (local.get $words) (i32.const 4))))
  (call $memcpy
    (local.get $right_copy)
    (local.get $right)
    (i32.mul (local.get $words) (i32.const 4))
  )
  (loop $div_loop
    ;; reinitialize right from right_copy
    (call $memcpy
      (local.get $right)
      (local.get $right_copy)
      (i32.mul (local.get $words) (i32.const 4))
    )
    (local.set $cmp (call $range_cmp (local.get $left) (local.get $right) (local.get $words) (i32.const 0)))
    (if (i32.ge_s (local.get $cmp) (i32.const 0))
      (then
        (call $range_sub (local.get $left) (local.get $right) (local.get $words))
        drop
        (call $sc_alloc (i32.mul (local.get $words) (i32.const 4))) ;; reallocateright since range_sub deallocs
        drop
        (call $range_add (local.get $quotient) (local.get $one) (local.get $words))
        drop
        (call $sc_alloc (i32.mul (local.get $words) (i32.const 4))) ;; reallocate since range_add deallocs
        drop
        (br $div_loop)
      )
    )
  )
  (if (i32.eqz (local.get $mode))
    (then
      ;; move quotient to left
      (call $memcpy
        (local.get $left)
        (local.get $quotient)
        (i32.mul (local.get $words) (i32.const 4))
      )
    )
    ;; left already has remainder
  )
  (call $sc_dealloc (i32.mul (local.get $words) (i32.const 12))) ;; dealloc right, quotient, one
  (local.get $left)
  return
)
(func $range_not (param $left i32) (param $words i32) (result i32)
  (local $i i32)
  (local.set $i (i32.const 0))
  (loop $main_loop
    (i32.store
      (i32.add (local.get $left) (i32.mul (local.get $i) (i32.const 4)))
      (i32.xor
        (i32.load (i32.add (local.get $left) (i32.mul (local.get $i) (i32.const 4))))
        (i32.const -1)
      )
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $main_loop (i32.lt_u (local.get $i) (local.get $words)))
  )
  (local.get $left)
  return
)
(func $range_neg (param $left i32) (param $words i32) (result i32)
  (local $one i32)
  (local $i i32)
  (call $range_not (local.get $left) (local.get $words))
  (local.set $one (call $sc_alloc (i32.mul (local.get $words) (i32.const 4))))
  (local.set $i (i32.const 1))
  (i32.store
    (local.get $one)
    (i32.const 1)
  )
  (loop $zero_loop
    (i32.store
      (i32.add (local.get $one) (i32.mul (local.get $i) (i32.const 4)))
      (i32.const 0)
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $zero_loop (i32.lt_u (local.get $i) (local.get $words)))
  )
  (call $range_add (local.get $left) (local.get $one) (local.get $words))
  (i32.mul (local.get $words) (i32.const 4))
  (local.get $left)
  return
)
;; -1 if left < right
;; 0 if left == right
;; 1 if left > right
(func $range_cmp (param $left i32) (param $right i32) (param $words i32) (param $dealloc i32) (result i32)
  (local $i i32)
  (local $left_work i32)
  (local $right_work i32)
  ;; we can do this early since it just moves a pointer, saves repeating it later
  (if (local.get $dealloc)
    (then
      (call $sc_dealloc (i32.mul (local.get $words) (i32.const 8)))
    )
  )
  ;; first we check the signs
  (local.set $i (i32.sub (local.get $words) (i32.const 1)))
  (local.set $left_work
    (i32.shr_s
      (i32.load (i32.add (local.get $left) (i32.mul (local.get $i) (i32.const 4))))
      (i32.const 31)
    )
  )
  (local.set $right_work
    (i32.shr_s
      (i32.load (i32.add (local.get $right) (i32.mul (local.get $i) (i32.const 4))))
      (i32.const 31)
    )
  )
  (if (i32.ne (local.get $left_work) (local.get $right_work))
    (then
      (if (local.get $left_work)
        (then
          ;; left negative, right positive
          (i32.const -1)
          return
        )
        (else
          ;; left positive, right negative
          (i32.const 1)
          return
        )
      )
    )
  )
  ;; same sign, compare magnitude
  (loop $cmp_loop
    (local.set $left_work (i32.load (i32.add (local.get $left) (i32.mul (local.get $i) (i32.const 4)))))
    (local.set $right_work (i32.load (i32.add (local.get $right) (i32.mul (local.get $i) (i32.const 4)))))
    (if (i32.gt_u (local.get $left_work) (local.get $right_work))
      (then
        ;; left > right
        (i32.const 1)
        return
      )
    )
    (if (i32.lt_u (local.get $left_work) (local.get $right_work))
      (then
        ;; left < right
        (i32.const -1)
        return
      )
    )
    (local.set $i (i32.sub (local.get $i) (i32.const 1)))
    (br_if $cmp_loop (i32.ge_s (local.get $i) (i32.const 0)))
  )
  ;; equal
  (i32.const 0)
  return
)
(func $range_eq (param $left i32) (param $right i32) (param $words i32) (result i32)
  (i32.eq
    (call $range_cmp (local.get $left) (local.get $right) (local.get $words) (i32.const 1))
    (i32.const 0)
  )
  return
)
(func $range_ne (param $left i32) (param $right i32) (param $words i32) (result i32)
  (i32.ne
    (call $range_cmp (local.get $left) (local.get $right) (local.get $words) (i32.const 1))
    (i32.const 0)
  )
  return
)
(func $range_lt (param $left i32) (param $right i32) (param $words i32) (result i32)
  (i32.lt_s
    (call $range_cmp (local.get $left) (local.get $right) (local.get $words) (i32.const 1))
    (i32.const 0)
  )
  return
)
(func $range_le (param $left i32) (param $right i32) (param $words i32) (result i32)
  (i32.le_s
    (call $range_cmp (local.get $left) (local.get $right) (local.get $words) (i32.const 1))
    (i32.const 0)
  )
  return
)
(func $range_gt (param $left i32) (param $right i32) (param $words i32) (result i32)
  (i32.gt_s
    (call $range_cmp (local.get $left) (local.get $right) (local.get $words) (i32.const 1))
    (i32.const 0)
  )
  return
)
(func $range_ge (param $left i32) (param $right i32) (param $words i32) (result i32)
  (i32.ge_s
    (call $range_cmp (local.get $left) (local.get $right) (local.get $words) (i32.const 1))
    (i32.const 0)
  )
  return
)

;; stack ptr, starts at 0 (8MB)
(global $sp (mut i32) (i32.const 8388608))
;; scratch ptr, starts after stack (64KB + 512 bytes)
(global $scp (mut i32) (i32.const 8389120))
(func $sc_alloc (param $size i32) (result i32)
  (local $allocated_ptr i32)
  (local.set $allocated_ptr (global.get $scp))
  (global.set $scp (i32.add (global.get $scp) (local.get $size)))
  (local.get $allocated_ptr)
  return
)
(func $sc_dealloc (param $size i32)
  (global.set $scp (i32.sub (global.get $scp) (local.get $size)))
)
(func $sc_put (param $src i32) (param $size i32) (result i32)
  (local $dest i32)
  (local.set $dest (call $sc_alloc (local.get $size)))
  (call $memcpy (local.get $dest) (local.get $src) (local.get $size))
  (local.get $dest)
  return
)
(func $memcpy (param $dest i32) (param $src i32) (param $size i32)
  (local $i i32)
  (local.set $i (i32.const 0))
  (loop $copy_loop
    (i32.store8
      (i32.add (local.get $dest) (local.get $i))
      (i32.load8_u (i32.add (local.get $src) (local.get $i)))
    )
    (local.set $i (i32.add (local.get $i) (i32.const 1)))
    (br_if $copy_loop (i32.lt_u (local.get $i) (local.get $size)))
  )
)
;; bump allocator ptr, starts after scratch (8MB + 64KB)
(global $ptr (mut i32) (i32.const 8454144))
(global $end (mut i32) (i32.const 8454144))
;; free tree root
(global $tree_root (mut i32) (i32.const 0))
(func $alloc (param $size i32) (result i32)
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
"#,
        main_fn.id()
    );
    // build types and functions
    for ty in IrType::all() {
        wat += &ty.wat_compile_type();
    }
    // build function table
    wat += "  (table ";
    let mut fns = IrValue::all()
        .into_iter()
        .filter(|val| matches!(val, IrValue::Fn { .. }))
        .collect::<Vec<_>>();
    fns.sort_by_key(|val| {
        if let IrValue::Fn { idx, .. } = val {
            *idx
        } else {
            unreachable!()
        }
    });
    wat += fns.len().to_string().as_str();
    wat += " funcref)\n";
    wat += "  (elem (i32.const 0) ".to_string().as_str();
    for val in &fns {
        if let IrValue::Fn { id, .. } = val {
            wat += format!("$fn{} ", id).as_str();
        }
    }
    wat += ")\n";
    // build functions
    for val in IrValue::all() {
        if let IrValue::Fn { id, orig_id, .. } = val {
            if *orig_id == PRINT_I32_VALUE_ID {
                wat += &format!(
                    "(func $fn{} (param i32)\n(i32.load (i32.sub (local.get 0) (i32.const \
                     4)))\n(call $print_i32))\n",
                    id
                );
            } else if *orig_id == PRINT_WORDS_VALUE_ID {
                wat += &format!(
                    "(func $fn{} (param i32 i32)\nlocal.get 0\nlocal.get 1\ncall $print_words)\n",
                    id
                );
            } else {
                wat += &Wasm32Backend::compile_function(val).to_wat();
            }
        }
    }
    wat += ")\n";
    let dir = config().root().join("out");
    if !dir.exists() {
        std::fs::create_dir_all(&dir).unwrap();
    }
    std::fs::write(dir.join(format!("{}.wat", config().package().name())), wat).unwrap();
    let output = std::process::Command::new("wat2wasm")
        .arg(dir.join(format!("{}.wat", config().package().name())))
        .arg("-o")
        .arg(dir.join(format!("{}.wasm", config().package().name())))
        .arg("--debug-names")
        .output()
        .expect("failed to execute wat2wasm");
    if !output.status.success() {
        return Err(CompileError::error(
            Span::dummy(),
            format!(
                "wat2wasm failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ),
        ));
    }
    tracing::info!(
        "WASM written to {}",
        dir.join(format!("{}.wasm", config().package().name()))
            .display()
    );
    Ok(())
}
