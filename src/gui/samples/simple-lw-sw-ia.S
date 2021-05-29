// Directives to make interresting windows visible
#pragma simulator show registers
#pragma simulator show memory

.globl _start
.set noat
.set noreorder

.text

_start:
loop:
// load the word from absolute address
lw     x2, 0x200(x0)
// store the word to absolute address
sw     x2, 0x204(x0)

// stop execution wait for debugger/user
//break
// ensure that continuation does not
// interpret random data
beq    x0, x0, loop
nop
nop
break

.data
.org 0x200

src_val:
.word  0x12345678
dst_val:
.word  0

// Specify location to show in memory window
#pragma simulator focus memory src_val
