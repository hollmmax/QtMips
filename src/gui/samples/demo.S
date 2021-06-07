.globl _start
.set noat
.set noreorder

.text

_start:
loop:
lw     x2, 0x200(x0)
addi   x3, x2, 0x200
xor    x4, x2, x3
and    x5, x4, x2
sub    x3, x3, x2
addi   x1, x0, 0x1
addi   x2, x0, 0x1
addi   x1, x0, 0x1
addi   x2, x0, 0x1
addi   x1, x0, 0x1
sw     x2, 0x204(x0)
lw     x2, 0x200(x0)
addi   x3, x2, 0x200
xor    x4, x2, x3
and    x5, x4, x2
sub    x3, x3, x2
addi   x1, x0, 0x1
addi   x2, x0, 0x1
addi   x1, x0, 0x1
addi   x2, x0, 0x1
addi   x1, x0, 0x1
sw     x2, 0x204(x0)
lw     x2, 0x200(x0)
addi   x3, x2, 0x200
xor    x4, x2, x3
and    x5, x4, x2
sub    x3, x3, x2
addi   x1, x0, 0x1
addi   x2, x0, 0x1
addi   x1, x0, 0x1
addi   x2, x0, 0x1
addi   x1, x0, 0x1
sw     x2, 0x204(x0)
lw     x2, 0x200(x0)
addi   x3, x2, 0x200
xor    x4, x2, x3
and    x5, x4, x2
sub    x3, x3, x2
addi   x1, x0, 0x1
addi   x2, x0, 0x1
addi   x1, x0, 0x1
addi   x2, x0, 0x1
addi   x1, x0, 0x1
sw     x2, 0x204(x0)


.data
.org 0x200

src_val:
.word  0x12345678
dst_val:
.word  0
