// SPDX-License-Identifier: GPL-2.0+
/*******************************************************************************
 * QtMips - MIPS 32-bit Architecture Subset Simulator
 *
 * Implemented to support following courses:
 *
 *   B35APO - Computer Architectures
 *   https://cw.fel.cvut.cz/wiki/courses/b35apo
 *
 *   B4M35PAP - Advanced Computer Architectures
 *   https://cw.fel.cvut.cz/wiki/courses/b4m35pap/start
 *
 * Copyright (c) 2017-2019 Karel Koci<cynerd@email.cz>
 * Copyright (c) 2019      Pavel Pisa <pisa@cmp.felk.cvut.cz>
 *
 * Faculty of Electrical Engineering (http://www.fel.cvut.cz)
 * Czech Technical University        (http://www.cvut.cz/)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA  02110-1301, USA.
 *
 ******************************************************************************/

#include "ossyscall.h"

#include "errno.h"
#include "qtmips_machine/core.h"
#include "qtmips_machine/utils.h"
#include "syscall_nr.h"
#include "target_errno.h"

#include <cerrno>
#include <cstdio>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

using namespace machine;
using namespace osemu;

// The copyied from musl-libc

#define TARGET_O_CREAT 0400
#define TARGET_O_EXCL 02000
#define TARGET_O_NOCTTY 04000
#define TARGET_O_TRUNC 01000
#define TARGET_O_APPEND 0010
#define TARGET_O_NONBLOCK 0200
#define TARGET_O_DSYNC 0020
#define TARGET_O_SYNC 040020
#define TARGET_O_RSYNC 040020
#define TARGET_O_DIRECTORY 0200000
#define TARGET_O_NOFOLLOW 0400000
#define TARGET_O_CLOEXEC 02000000

#define TARGET_O_PATH 010000000

#define TARGET_O_SYNC1 040000

#define TARGET_O_ACCMODE (03 | TARGET_O_PATH)
#define TARGET_O_RDONLY 00
#define TARGET_O_WRONLY 01
#define TARGET_O_RDWR 02

// static const QMap<int, int> map_target_o_flags_to_o_flags = {
//     #ifdef O_CREAT
//         {TARGET_O_CREAT, O_CREAT},
//     #endif
//     #ifdef O_EXCL
//         {TARGET_O_EXCL, O_EXCL},
//     #endif
//     #ifdef O_NOCTTY
//         {TARGET_O_NOCTTY, O_NOCTTY},
//     #endif
//     #ifdef O_TRUNC
//         {TARGET_O_TRUNC, O_TRUNC},
//     #endif
//     #ifdef O_APPEND
//         {TARGET_O_APPEND, O_APPEND},
//     #endif
//     #ifdef O_NONBLOCK
//         {TARGET_O_NONBLOCK, O_NONBLOCK},
//     #endif
//     #ifdef O_DSYNC
//         {TARGET_O_DSYNC, O_DSYNC},
//     #endif
//     #ifdef O_SYNC
//         {TARGET_O_SYNC1, O_SYNC},
//     #endif
//     #ifdef O_RSYNC
//         {TARGET_O_SYNC1, O_RSYNC},
//     #endif
//     #ifdef O_DIRECTORY
//         {TARGET_O_DIRECTORY, O_DIRECTORY},
//     #endif
//     #ifdef O_NOFOLLOW
//         {TARGET_O_NOFOLLOW, O_NOFOLLOW},
//     #endif
//     #ifdef O_CLOEXEC
//         {TARGET_O_CLOEXEC, O_CLOEXEC},
//     #endif
// };

QIODevice::OpenMode map_target_o_flags_to_o_flags(int target_o_flags) {
    QIODevice::OpenMode ret = 0;
    if (target_o_flags & TARGET_O_APPEND) ret |= QIODevice::Append;
    if (target_o_flags & TARGET_O_TRUNC) ret |= QIODevice::Truncate;
    if (target_o_flags & TARGET_O_EXCL) ret |= QIODevice::NewOnly;
    if (!(target_o_flags & TARGET_O_CREAT)) ret |= QIODevice::ExistingOnly;
    return ret;
}

#if defined(S_IRUSR) & defined(S_IWUSR) & defined(S_IRGRP) & defined(S_IWGRP)  \
    & defined(S_IROTH) & defined(S_IWOTH)
    #define OPEN_MODE                                                          \
        (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#else
    #define OPEN_MODE 0
#endif

// The list copyied from musl-libc

static const QMap<int, int> errno_map = {
#ifdef EPERM
    { EPERM, TARGET_EPERM },
#endif
#ifdef ENOENT
    { ENOENT, TARGET_ENOENT },
#endif
#ifdef ESRCH
    { ESRCH, TARGET_ESRCH },
#endif
#ifdef EINTR
    { EINTR, TARGET_EINTR },
#endif
#ifdef EIO
    { EIO, TARGET_EIO },
#endif
#ifdef ENXIO
    { ENXIO, TARGET_ENXIO },
#endif
#ifdef E2BIG
    { E2BIG, TARGET_E2BIG },
#endif
#ifdef ENOEXEC
    { ENOEXEC, TARGET_ENOEXEC },
#endif
#ifdef EBADF
    { EBADF, TARGET_EBADF },
#endif
#ifdef ECHILD
    { ECHILD, TARGET_ECHILD },
#endif
#ifdef EAGAIN
    { EAGAIN, TARGET_EAGAIN },
#endif
#ifdef ENOMEM
    { ENOMEM, TARGET_ENOMEM },
#endif
#ifdef EACCES
    { EACCES, TARGET_EACCES },
#endif
#ifdef EFAULT
    { EFAULT, TARGET_EFAULT },
#endif
#ifdef ENOTBLK
    { ENOTBLK, TARGET_ENOTBLK },
#endif
#ifdef EBUSY
    { EBUSY, TARGET_EBUSY },
#endif
#ifdef EEXIST
    { EEXIST, TARGET_EEXIST },
#endif
#ifdef EXDEV
    { EXDEV, TARGET_EXDEV },
#endif
#ifdef ENODEV
    { ENODEV, TARGET_ENODEV },
#endif
#ifdef ENOTDIR
    { ENOTDIR, TARGET_ENOTDIR },
#endif
#ifdef EISDIR
    { EISDIR, TARGET_EISDIR },
#endif
#ifdef EINVAL
    { EINVAL, TARGET_EINVAL },
#endif
#ifdef ENFILE
    { ENFILE, TARGET_ENFILE },
#endif
#ifdef EMFILE
    { EMFILE, TARGET_EMFILE },
#endif
#ifdef ENOTTY
    { ENOTTY, TARGET_ENOTTY },
#endif
#ifdef ETXTBSY
    { ETXTBSY, TARGET_ETXTBSY },
#endif
#ifdef EFBIG
    { EFBIG, TARGET_EFBIG },
#endif
#ifdef ENOSPC
    { ENOSPC, TARGET_ENOSPC },
#endif
#ifdef ESPIPE
    { ESPIPE, TARGET_ESPIPE },
#endif
#ifdef EROFS
    { EROFS, TARGET_EROFS },
#endif
#ifdef EMLINK
    { EMLINK, TARGET_EMLINK },
#endif
#ifdef EPIPE
    { EPIPE, TARGET_EPIPE },
#endif
#ifdef EDOM
    { EDOM, TARGET_EDOM },
#endif
#ifdef ERANGE
    { ERANGE, TARGET_ERANGE },
#endif
#ifdef ENOMSG
    { ENOMSG, TARGET_ENOMSG },
#endif
#ifdef EIDRM
    { EIDRM, TARGET_EIDRM },
#endif
#ifdef ECHRNG
    { ECHRNG, TARGET_ECHRNG },
#endif
#ifdef EL2NSYNC
    { EL2NSYNC, TARGET_EL2NSYNC },
#endif
#ifdef EL3HLT
    { EL3HLT, TARGET_EL3HLT },
#endif
#ifdef EL3RST
    { EL3RST, TARGET_EL3RST },
#endif
#ifdef ELNRNG
    { ELNRNG, TARGET_ELNRNG },
#endif
#ifdef EUNATCH
    { EUNATCH, TARGET_EUNATCH },
#endif
#ifdef ENOCSI
    { ENOCSI, TARGET_ENOCSI },
#endif
#ifdef EL2HLT
    { EL2HLT, TARGET_EL2HLT },
#endif
#ifdef EDEADLK
    { EDEADLK, TARGET_EDEADLK },
#endif
#ifdef ENOLCK
    { ENOLCK, TARGET_ENOLCK },
#endif
#ifdef EBADE
    { EBADE, TARGET_EBADE },
#endif
#ifdef EBADR
    { EBADR, TARGET_EBADR },
#endif
#ifdef EXFULL
    { EXFULL, TARGET_EXFULL },
#endif
#ifdef ENOANO
    { ENOANO, TARGET_ENOANO },
#endif
#ifdef EBADRQC
    { EBADRQC, TARGET_EBADRQC },
#endif
#ifdef EBADSLT
    { EBADSLT, TARGET_EBADSLT },
#endif
#ifdef EDEADLOCK
    { EDEADLOCK, TARGET_EDEADLOCK },
#endif
#ifdef EBFONT
    { EBFONT, TARGET_EBFONT },
#endif
#ifdef ENOSTR
    { ENOSTR, TARGET_ENOSTR },
#endif
#ifdef ENODATA
    { ENODATA, TARGET_ENODATA },
#endif
#ifdef ETIME
    { ETIME, TARGET_ETIME },
#endif
#ifdef ENOSR
    { ENOSR, TARGET_ENOSR },
#endif
#ifdef ENONET
    { ENONET, TARGET_ENONET },
#endif
#ifdef ENOPKG
    { ENOPKG, TARGET_ENOPKG },
#endif
#ifdef EREMOTE
    { EREMOTE, TARGET_EREMOTE },
#endif
#ifdef ENOLINK
    { ENOLINK, TARGET_ENOLINK },
#endif
#ifdef EADV
    { EADV, TARGET_EADV },
#endif
#ifdef ESRMNT
    { ESRMNT, TARGET_ESRMNT },
#endif
#ifdef ECOMM
    { ECOMM, TARGET_ECOMM },
#endif
#ifdef EPROTO
    { EPROTO, TARGET_EPROTO },
#endif
#ifdef EDOTDOT
    { EDOTDOT, TARGET_EDOTDOT },
#endif
#ifdef EMULTIHOP
    { EMULTIHOP, TARGET_EMULTIHOP },
#endif
#ifdef EBADMSG
    { EBADMSG, TARGET_EBADMSG },
#endif
#ifdef ENAMETOOLONG
    { ENAMETOOLONG, TARGET_ENAMETOOLONG },
#endif
#ifdef EOVERFLOW
    { EOVERFLOW, TARGET_EOVERFLOW },
#endif
#ifdef ENOTUNIQ
    { ENOTUNIQ, TARGET_ENOTUNIQ },
#endif
#ifdef EBADFD
    { EBADFD, TARGET_EBADFD },
#endif
#ifdef EREMCHG
    { EREMCHG, TARGET_EREMCHG },
#endif
#ifdef ELIBACC
    { ELIBACC, TARGET_ELIBACC },
#endif
#ifdef ELIBBAD
    { ELIBBAD, TARGET_ELIBBAD },
#endif
#ifdef ELIBSCN
    { ELIBSCN, TARGET_ELIBSCN },
#endif
#ifdef ELIBMAX
    { ELIBMAX, TARGET_ELIBMAX },
#endif
#ifdef ELIBEXEC
    { ELIBEXEC, TARGET_ELIBEXEC },
#endif
#ifdef EILSEQ
    { EILSEQ, TARGET_EILSEQ },
#endif
#ifdef ENOSYS
    { ENOSYS, TARGET_ENOSYS },
#endif
#ifdef ELOOP
    { ELOOP, TARGET_ELOOP },
#endif
#ifdef ERESTART
    { ERESTART, TARGET_ERESTART },
#endif
#ifdef ESTRPIPE
    { ESTRPIPE, TARGET_ESTRPIPE },
#endif
#ifdef ENOTEMPTY
    { ENOTEMPTY, TARGET_ENOTEMPTY },
#endif
#ifdef EUSERS
    { EUSERS, TARGET_EUSERS },
#endif
#ifdef ENOTSOCK
    { ENOTSOCK, TARGET_ENOTSOCK },
#endif
#ifdef EDESTADDRREQ
    { EDESTADDRREQ, TARGET_EDESTADDRREQ },
#endif
#ifdef EMSGSIZE
    { EMSGSIZE, TARGET_EMSGSIZE },
#endif
#ifdef EPROTOTYPE
    { EPROTOTYPE, TARGET_EPROTOTYPE },
#endif
#ifdef ENOPROTOOPT
    { ENOPROTOOPT, TARGET_ENOPROTOOPT },
#endif
#ifdef EPROTONOSUPPORT
    { EPROTONOSUPPORT, TARGET_EPROTONOSUPPORT },
#endif
#ifdef ESOCKTNOSUPPORT
    { ESOCKTNOSUPPORT, TARGET_ESOCKTNOSUPPORT },
#endif
#ifdef EOPNOTSUPP
    { EOPNOTSUPP, TARGET_EOPNOTSUPP },
#endif
#ifdef ENOTSUP
    { ENOTSUP, TARGET_ENOTSUP },
#endif
#ifdef EPFNOSUPPORT
    { EPFNOSUPPORT, TARGET_EPFNOSUPPORT },
#endif
#ifdef EAFNOSUPPORT
    { EAFNOSUPPORT, TARGET_EAFNOSUPPORT },
#endif
#ifdef EADDRINUSE
    { EADDRINUSE, TARGET_EADDRINUSE },
#endif
#ifdef EADDRNOTAVAIL
    { EADDRNOTAVAIL, TARGET_EADDRNOTAVAIL },
#endif
#ifdef ENETDOWN
    { ENETDOWN, TARGET_ENETDOWN },
#endif
#ifdef ENETUNREACH
    { ENETUNREACH, TARGET_ENETUNREACH },
#endif
#ifdef ENETRESET
    { ENETRESET, TARGET_ENETRESET },
#endif
#ifdef ECONNABORTED
    { ECONNABORTED, TARGET_ECONNABORTED },
#endif
#ifdef ECONNRESET
    { ECONNRESET, TARGET_ECONNRESET },
#endif
#ifdef ENOBUFS
    { ENOBUFS, TARGET_ENOBUFS },
#endif
#ifdef EISCONN
    { EISCONN, TARGET_EISCONN },
#endif
#ifdef ENOTCONN
    { ENOTCONN, TARGET_ENOTCONN },
#endif
#ifdef EUCLEAN
    { EUCLEAN, TARGET_EUCLEAN },
#endif
#ifdef ENOTNAM
    { ENOTNAM, TARGET_ENOTNAM },
#endif
#ifdef ENAVAIL
    { ENAVAIL, TARGET_ENAVAIL },
#endif
#ifdef EISNAM
    { EISNAM, TARGET_EISNAM },
#endif
#ifdef EREMOTEIO
    { EREMOTEIO, TARGET_EREMOTEIO },
#endif
#ifdef ESHUTDOWN
    { ESHUTDOWN, TARGET_ESHUTDOWN },
#endif
#ifdef ETOOMANYREFS
    { ETOOMANYREFS, TARGET_ETOOMANYREFS },
#endif
#ifdef ETIMEDOUT
    { ETIMEDOUT, TARGET_ETIMEDOUT },
#endif
#ifdef ECONNREFUSED
    { ECONNREFUSED, TARGET_ECONNREFUSED },
#endif
#ifdef EHOSTDOWN
    { EHOSTDOWN, TARGET_EHOSTDOWN },
#endif
#ifdef EHOSTUNREACH
    { EHOSTUNREACH, TARGET_EHOSTUNREACH },
#endif
#ifdef EWOULDBLOCK
    { EWOULDBLOCK, TARGET_EWOULDBLOCK },
#endif
#ifdef EALREADY
    { EALREADY, TARGET_EALREADY },
#endif
#ifdef EINPROGRESS
    { EINPROGRESS, TARGET_EINPROGRESS },
#endif
#ifdef ESTALE
    { ESTALE, TARGET_ESTALE },
#endif
#ifdef ECANCELED
    { ECANCELED, TARGET_ECANCELED },
#endif
#ifdef ENOMEDIUM
    { ENOMEDIUM, TARGET_ENOMEDIUM },
#endif
#ifdef EMEDIUMTYPE
    { EMEDIUMTYPE, TARGET_EMEDIUMTYPE },
#endif
#ifdef ENOKEY
    { ENOKEY, TARGET_ENOKEY },
#endif
#ifdef EKEYEXPIRED
    { EKEYEXPIRED, TARGET_EKEYEXPIRED },
#endif
#ifdef EKEYREVOKED
    { EKEYREVOKED, TARGET_EKEYREVOKED },
#endif
#ifdef EKEYREJECTED
    { EKEYREJECTED, TARGET_EKEYREJECTED },
#endif
#ifdef EOWNERDEAD
    { EOWNERDEAD, TARGET_EOWNERDEAD },
#endif
#ifdef ENOTRECOVERABLE
    { ENOTRECOVERABLE, TARGET_ENOTRECOVERABLE },
#endif
#ifdef ERFKILL
    { ERFKILL, TARGET_ERFKILL },
#endif
#ifdef EHWPOISON
    { EHWPOISON, TARGET_EHWPOISON },
#endif
#ifdef EDQUOT
    { EDQUOT, TARGET_EDQUOT },
#endif
};

uint32_t result_errno_if_error(uint32_t result) {
    if (result < (uint32_t)-4096)
        return result;
    result = (uint32_t)-errno_map.value(errno);
    if (!result)
        result = (uint32_t)-1;
    return result;
}

int status_from_result(uint32_t result) {
    if (result < (uint32_t)-4096)
        return 0;
    else
        return -result;
}

typedef int (OsSyscallExceptionHandler::*syscall_handler_t)(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8);

struct mips_syscall_desc_t {
    const char *name;
    unsigned int args;
    syscall_handler_t handler;
};

// The list copyied from QEMU

#define MIPS_SYS(name, args, handler)                                          \
    { #name, args, &OsSyscallExceptionHandler::handler },
static const mips_syscall_desc_t mips_syscall_args[] = {
        MIPS_SYS(sys_exit       , 1, do_sys_exit)
            MIPS_SYS(sys_read, 3, do_sys_read)
                MIPS_SYS(sys_write, 3, do_sys_write)
                    MIPS_SYS(sys_open, 3, do_sys_open) /* 4005 */
    MIPS_SYS(sys_close, 1, do_sys_close)
        MIPS_SYS(sys_brk        , 1, do_sys_brk)    /* 4045 */
        MIPS_SYS(sys_ftruncate  , 2, do_sys_ftruncate)
        MIPS_SYS(sys_readv      , 3, do_sys_readv)    /* 4145 */
        MIPS_SYS(sys_writev     , 3, do_sys_writev)
        MIPS_SYS(sys_mmap2      , 6, do_sys_mmap2)               /* 4210 */
        MIPS_SYS(sys_set_thread_area, 1, do_sys_set_thread_area)
};

const unsigned mips_syscall_args_size
    = sizeof(mips_syscall_args) / sizeof(*mips_syscall_args);

static const mips_syscall_desc_t spim_syscall_args[] = {
    MIPS_SYS(print_integer, 1, do_spim_print_integer)     /*  1 */
    MIPS_SYS(print_float, 1, syscall_default_handler)     /*  2 */
    MIPS_SYS(print_double, 1, syscall_default_handler)    /*  3 */
    MIPS_SYS(print_string, 1, do_spim_print_string)       /*  4 */
    MIPS_SYS(read_integer, 0, syscall_default_handler)    /*  5 */
    MIPS_SYS(read_float, 0, syscall_default_handler)      /*  6 */
    MIPS_SYS(read_double, 0, syscall_default_handler)     /*  7 */
    MIPS_SYS(read_string, 0, do_spim_read_string)         /*  8 */
    MIPS_SYS(sbrk, 0, do_spim_sbrk)                       /*  9 */
    MIPS_SYS(exit, 1, do_spim_exit)                       /* 10 */
    MIPS_SYS(print_character, 1, do_spim_print_character) /* 11 */
    MIPS_SYS(read_character, 1, do_spim_read_character)   /* 12 */
    MIPS_SYS(open_file, 3, do_sys_open)                   /* 13 */
    MIPS_SYS(read_from_file, 3, do_sys_read)              /* 14 */
    MIPS_SYS(read_to_file, 3, do_sys_write)               /* 15 */
    MIPS_SYS(close_file, 3, do_sys_close)                 /* 16 */
    MIPS_SYS(exit, 1, do_sys_exit)                        /* 17 */
};

const unsigned spim_syscall_args_size
    = sizeof(spim_syscall_args) / sizeof(*spim_syscall_args);

OsSyscallExceptionHandler::OsSyscallExceptionHandler(
    bool known_syscall_stop,
    bool unknown_syscall_stop,
    QString fs_root)
    : fd_mapping(3, FD_TERMINAL) {
    brk_limit = 0;
    anonymous_base = 0x60000000;
    anonymous_last = anonymous_base;
    this->known_syscall_stop = known_syscall_stop;
    this->unknown_syscall_stop = unknown_syscall_stop;
    this->fs_root = fs_root;
}

bool OsSyscallExceptionHandler::handle_exception(
    Core *core,
    Registers *regs,
    ExceptionCause excause,
    Address inst_addr,
    Address next_addr,
    Address jump_branch_pc,
    bool in_delay_slot,
    Address mem_ref_addr) {
    unsigned int syscall_num = regs->read_gp(2).as_u32();
    const mips_syscall_desc_t *sdesc;
    RegisterValue a1 = 0, a2 = 0, a3 = 0, a4 = 0, a5 = 0, a6 = 0, a7 = 0,
                  a8 = 0;
    Address sp = Address(regs->read_gp(29).as_u32());
    uint32_t result;
    int status;

    FrontendMemory *mem_data = core->get_mem_data();
    FrontendMemory *mem_program = core->get_mem_program();
    (void)mem_program;

#if 1
    printf(
        "Exception cause %d instruction PC 0x%08lx next PC 0x%08lx jump branch "
        "PC 0x%08lx "
        "in_delay_slot %d registers PC 0x%08lx mem ref 0x%08lx\n",
        excause, (unsigned long)inst_addr.get_raw(),
        (unsigned long)next_addr.get_raw(),
        (unsigned long)jump_branch_pc.get_raw(), (int)in_delay_slot,
        (unsigned long)regs->read_pc().get_raw(),
        (unsigned long)mem_ref_addr.get_raw());
#else
    (void)excause;
    (void)inst_addr;
    (void)next_addr;
    (void)mem_ref_addr;
    (void)regs;
    (void)jump_branch_pc;
    (void)in_delay_slot;
#endif

    if (syscall_num >= 1 && syscall_num < 1 + spim_syscall_args_size) {
        sdesc = &spim_syscall_args[syscall_num - 1];
    } else if (syscall_num >= 4000 && syscall_num < 4000 + mips_syscall_args_size) {
        syscall_num -= 4000;
        sdesc = &mips_syscall_args[syscall_num];
    } else {
        throw QTMIPS_EXCEPTION(
            SyscallUnknown, "System call number unknown ",
            QString::number(syscall_num));
    }

    a1 = regs->read_gp(4);
    a2 = regs->read_gp(5);
    a3 = regs->read_gp(6);
    a4 = regs->read_gp(7);

    switch (sdesc->args) {
    case 8: a8 = mem_data->read_u32(sp + 28); FALLTROUGH
    case 7: a7 = mem_data->read_u32(sp + 24); FALLTROUGH
    case 6: a6 = mem_data->read_u32(sp + 20); FALLTROUGH
    case 5: a5 = mem_data->read_u32(sp + 16); FALLTROUGH
    default: break;
    }

#if 1
    printf(
        "Syscall %s number %d/0x%x a1 %ld a2 %ld a3 %ld a4 %ld\n", sdesc->name,
        syscall_num, syscall_num, a1.as_u64(), a2.as_u64(), a3.as_u64(),
        a4.as_u64());

#endif
    status = (this->*sdesc->handler)(
        result, core, syscall_num, a1.as_u32(), a2.as_u32(), a3.as_u32(),
        a4.as_u32(), a5.as_u32(), a6.as_u32(), a7.as_u32(), a8.as_u32());
    if (known_syscall_stop) {
        emit core->stop_on_exception_reached();
    }

    regs->write_gp(7, status);
    if (status < 0) {
        regs->write_gp(2, status);
    } else {
        regs->write_gp(2, result);
    }

    return true;
}

int32_t OsSyscallExceptionHandler::write_mem(
    machine::FrontendMemory *mem,
    Address addr,
    const QVector<uint8_t> &data,
    uint32_t count) {
    if ((uint32_t)data.size() < count)
        count = data.size();

    for (uint32_t i = 0; i < count; i++) {
        mem->write_u8(addr, data[i]);
        addr += 1;
    }
    return count;
}

int32_t OsSyscallExceptionHandler::read_mem(
    machine::FrontendMemory *mem,
    Address addr,
    QVector<uint8_t> &data,
    uint32_t count) {
    data.resize(count);
    for (uint32_t i = 0; i < count; i++) {
        data[i] = mem->read_u8(addr);
        addr += 1;
    }
    return count;
}

int32_t OsSyscallExceptionHandler::write_io(
    int fd,
    const QVector<uint8_t> &data,
    uint32_t count) {
    if ((uint32_t)data.size() < count)
        count = data.size();
    if (fd == FD_UNUSED) {
        return -1;
    } else if (fd == FD_TERMINAL) {
        for (uint32_t i = 0; i < count; i++)
            emit char_written(fd, data[i]);
    } else {
        count = file_objects[fd]->write(reinterpret_cast<const char*>(data.data()), count);
        // count = write(fd, data.data(), count);
    }
    return result_errno_if_error(count);
}

int32_t OsSyscallExceptionHandler::read_io(
    int fd,
    QVector<uint8_t> &data,
    uint32_t count,
    bool add_nl_at_eof) {
    data.resize(count);
    if ((uint32_t)data.size() < count)
        count = data.size();
    if (fd == FD_UNUSED) {
        return -1;
    } else if (fd == FD_TERMINAL) {
        for (uint32_t i = 0; i < count; i++) {
            unsigned int byte;
            bool available = false;
            emit rx_byte_pool(fd, byte, available);
            if (!available) {
                // add final newline if there are no more data
                if (add_nl_at_eof)
                    data[i] = '\n';
                count = i + 1;
                break;
            }
            data[i] = byte;
        }
    } else {
        count = file_objects[fd]->read(reinterpret_cast<char*>(data.data()), count);
        // count = read(fd, data.data(), count);
    }
    if ((int32_t)count >= 0)
        data.resize(count);
    return result_errno_if_error(count);
}

int OsSyscallExceptionHandler::allocate_fd(int val) {
    int i;
    for (i = 0; i < fd_mapping.size(); i++) {
        if (fd_mapping[i] == FD_UNUSED) {
            fd_mapping[i] = val;
            return i;
        }
    }
    i = fd_mapping.size();
    fd_mapping.resize(i + 1);
    fd_mapping[i] = val;
    return i;
}

int OsSyscallExceptionHandler::file_open(QString fname, int flags, int mode) {
    int targetfd, fd;
    QIODevice::OpenMode hostflags = map_target_o_flags_to_o_flags(flags);
    (void)mode;
    // for (auto i = map_target_o_flags_to_o_flags.begin();
    //      i != map_target_o_flags_to_o_flags.end(); i++)
    //     if (flags & i.key())
    //         hostflags |= i.value();

    switch (flags & TARGET_O_ACCMODE) {
    case TARGET_O_RDONLY: hostflags |= QIODevice::ReadOnly; break;
    case TARGET_O_WRONLY: hostflags |= QIODevice::WriteOnly; break;
    case TARGET_O_RDWR: hostflags |= QIODevice::ReadWrite; break;
    }

    if (fs_root.size() == 0) {
        return allocate_fd(FD_TERMINAL);
    }

    fname = filepath_to_host(fname);
    QFile* file = new QFile(fname);
    if (file->open(hostflags)) {
        targetfd = allocate_fd(++last_mapped);
        file_objects[last_mapped] = file;
    } else {
        targetfd = result_errno_if_error(EACCES);
    }
    return targetfd;

    // fd = open(fname.toLatin1().data(), hostflags, OPEN_MODE);
    // if (fd >= 0) {
    //     targetfd = allocate_fd(fd);
    // } else {
    //     targetfd = result_errno_if_error(fd);
    // }
    // return targetfd;
}

int OsSyscallExceptionHandler::targetfd_to_fd(int targetfd) {
    if (targetfd < 0)
        return FD_INVALID;
    if (targetfd >= fd_mapping.size())
        return FD_INVALID;
    return fd_mapping.at(targetfd);
}

void OsSyscallExceptionHandler::close_fd(int targetfd) {
    if (targetfd <= fd_mapping.size())
        delete file_objects[fd_mapping[targetfd]];
        file_objects.remove(fd_mapping[targetfd]);
        fd_mapping[targetfd] = FD_UNUSED;
}

QString OsSyscallExceptionHandler::filepath_to_host(QString path) {
    int pos = 0;
    int prev;
    while (true) {
        if (((path.size() - pos == 2) && (path.mid(pos, -1) == ".."))
            || ((path.size() - pos > 2) && (path.mid(pos, 3) == "../"))) {
            if (pos == 0) {
                prev = 0;
            } else {
                if (pos == 1)
                    prev = 0;
                else
                    prev = path.lastIndexOf('/', pos - 2) + 1;
            }
            path.remove(prev, pos + 3 - prev);
            pos = prev;
            continue;
        }
        pos = path.indexOf('/', pos);
        if (pos == -1)
            break;
        pos += 1;
    }
    if ((path.size() >= 1) && path.at(0) == '/')
        path = fs_root + path;
    else
        path = fs_root + '/' + path;
    return path;
}

int OsSyscallExceptionHandler::syscall_default_handler(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    const mips_syscall_desc_t *sdesc = &mips_syscall_args[syscall_num];
#if 1
    printf(
        "Unimplemented syscall %s number %d/0x%x a1 %ld a2 %ld a3 %ld a4 %ld\n",
        sdesc->name, syscall_num, syscall_num, (unsigned long)a1,
        (unsigned long)a2, (unsigned long)a3, (unsigned long)a4);

#endif
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;
    result = 0;
    if (unknown_syscall_stop)
        emit core->stop_on_exception_reached();
    return TARGET_ENOSYS;
}

// void exit(int status);
int OsSyscallExceptionHandler::do_sys_exit(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;
    int status = a1;

    printf("sys_exit status %d\n", status);
    emit core->stop_on_exception_reached();

    return 0;
}

int OsSyscallExceptionHandler::do_sys_set_thread_area(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;
    core->set_c0_userlocal(a1);
    result = 0;
    return 0;
}

// ssize_t writev(int fd, const struct iovec *iov, int iovcnt);
int OsSyscallExceptionHandler::do_sys_writev(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;
    int fd = a1;
    Address iov = Address(a2);
    int iovcnt = a3;
    FrontendMemory *mem = core->get_mem_data();
    int32_t count;
    QVector<uint8_t> data;

    printf("sys_writev to fd %d\n", fd);

    fd = targetfd_to_fd(fd);
    if (fd == FD_INVALID) {
        result = -TARGET_EINVAL;
        return 0;
    }

    while (iovcnt-- > 0) {
        Address iov_base = Address(mem->read_u32(iov));
        uint32_t iov_len = mem->read_u32(iov + 4);
        iov += 8;

        read_mem(mem, iov_base, data, iov_len);
        count = write_io(fd, data, iov_len);
        if (count >= 0) {
            result += count;
        } else {
            if (result == 0)
                result = count;
        }
        if (count < (int32_t)iov_len)
            break;
    }

    return status_from_result(result);
}

// ssize_t write(int fd, const void *buf, size_t count);
int OsSyscallExceptionHandler::do_sys_write(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;
    int fd = a1;
    Address buf = Address(a2);
    int size = a3;
    FrontendMemory *mem = core->get_mem_data();
    int32_t count;
    QVector<uint8_t> data;

    printf("sys_write to fd %d\n", fd);

    fd = targetfd_to_fd(fd);
    if (fd == FD_INVALID) {
        result = -TARGET_EINVAL;
        return 0;
    }

    read_mem(mem, buf, data, size);
    count = write_io(fd, data, size);

    result = count;

    return status_from_result(result);
}

// ssize_t readv(int fd, const struct iovec *iov, int iovcnt);
int OsSyscallExceptionHandler::do_sys_readv(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;
    int fd = a1;
    Address iov = Address(a2);
    int iovcnt = a3;
    FrontendMemory *mem = core->get_mem_data();
    int32_t count;
    QVector<uint8_t> data;

    printf("sys_readv to fd %d\n", fd);

    fd = targetfd_to_fd(fd);
    if (fd == FD_INVALID) {
        result = -TARGET_EINVAL;
        return 0;
    }

    while (iovcnt-- > 0) {
        Address iov_base = Address(mem->read_u32(iov));
        uint32_t iov_len = mem->read_u32(iov + 4);
        iov += 8;

        count = read_io(fd, data, iov_len, true);
        if (count >= 0) {
            write_mem(mem, iov_base, data, count);
            result += count;
        } else {
            if (result == 0)
                result = count;
        }
        if (count < (int32_t)iov_len)
            break;
    }

    return status_from_result(result);
}

// ssize_t read(int fd, void *buf, size_t count);
int OsSyscallExceptionHandler::do_sys_read(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;
    int fd = a1;
    Address buf = Address(a2);
    int size = a3;
    FrontendMemory *mem = core->get_mem_data();
    int32_t count;
    QVector<uint8_t> data;

    printf("sys_read to fd %d\n", fd);

    fd = targetfd_to_fd(fd);
    if (fd == FD_INVALID) {
        result = -TARGET_EINVAL;
        return 0;
    }

    result = 0;

    count = read_io(fd, data, size, true);
    if (count >= 0) {
        write_mem(mem, buf, data, size);
    }
    result = count;

    return status_from_result(result);
}

// int open(const char *pathname, int flags, mode_t mode);
int OsSyscallExceptionHandler::do_sys_open(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;
    Address pathname_ptr = Address(a1);
    int flags = a2;
    int mode = a3;
    uint32_t ch;
    FrontendMemory *mem = core->get_mem_data();

    printf("sys_open filename\n");

    QString fname;
    while (true) {
        ch = mem->read_u8(pathname_ptr);
        pathname_ptr += 1;
        if (ch == 0)
            break;
        fname.append(QChar(ch));
    }

    result = file_open(fname, flags, mode);

    return status_from_result(result);
}

// int close(int fd);
int OsSyscallExceptionHandler::do_sys_close(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;
    int fd = a1;

    printf("sys_close fd %d\n", fd);

    int targetfd = fd;
    fd = targetfd_to_fd(fd);
    if (fd == FD_INVALID) {
        result = -TARGET_EINVAL;
        return 0;
    }

    close(fd);
    close_fd(targetfd);

    return status_from_result(result);
}

// int ftruncate(int fd, off_t length);
int OsSyscallExceptionHandler::do_sys_ftruncate(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;
    int fd = a1;
    uint64_t length = ((uint64_t)a2 << 32) | a3;

    printf("sys_ftruncate fd %d\n", fd);

    fd = targetfd_to_fd(fd);
    if (fd == FD_INVALID) {
        result = -TARGET_EINVAL;
        return 0;
    }

    // result = result_errno_if_error(ftruncate(fd, length));
    result = result_errno_if_error(file_objects[fd]->resize(length) - 1);

    return status_from_result(result);
}

// int or void * brk(void *addr);
int OsSyscallExceptionHandler::do_sys_brk(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;
    uint32_t new_limit = a1;
    brk_limit = new_limit;
    result = brk_limit;

    return 0;
}

#define TARGET_SYSCALL_MMAP2_UNIT 4096ULL
#define TARGET_MAP_ANONYMOUS 0x20

// void *mmap2(void *addr, size_t length, int prot,
//             int flags, int fd, off_t pgoffset);
int OsSyscallExceptionHandler::do_sys_mmap2(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;
    uint32_t addr = a1;
    uint32_t lenght = a2;
    uint32_t prot = a3;
    uint32_t flags = a4;
    uint32_t fd = a5;
    uint64_t offset = a6 * TARGET_SYSCALL_MMAP2_UNIT;

    printf(
        "sys_mmap2 addr = 0x%08lx lenght= 0x%08lx prot = 0x%08lx flags = "
        "0x%08lx fd = %d offset = 0x%08llx\n",
        (unsigned long)addr, (unsigned long)lenght, (unsigned long)prot,
        (unsigned long)flags, (int)fd, (unsigned long long)offset);

    lenght = (lenght + TARGET_SYSCALL_MMAP2_UNIT - 1)
             & ~(TARGET_SYSCALL_MMAP2_UNIT - 1);
    anonymous_last = (anonymous_last + TARGET_SYSCALL_MMAP2_UNIT - 1)
                     & ~(TARGET_SYSCALL_MMAP2_UNIT - 1);
    result = anonymous_last;
    anonymous_last += lenght;

    return 0;
}

int OsSyscallExceptionHandler::do_spim_print_integer(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;

    QString str = QString::number(a1);
    QVector<uint8_t> data;
    foreach (QChar ch, str)
        data.append(ch.toLatin1());
    write_io(1, data, data.size());

    return 0;
}

int OsSyscallExceptionHandler::do_spim_print_string(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    Address str_ptr = Address(a1);
    QVector<uint8_t> data;
    FrontendMemory *mem = core->get_mem_data();

    while (true) {
        uint8_t ch;
        ch = mem->read_u8(str_ptr);
        str_ptr += 1;
        if (ch == 0)
            break;
        data.append(ch);
    }
    write_io(1, data, data.size());
    result = 0;

    return 0;
}

int OsSyscallExceptionHandler::do_spim_read_string(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    QVector<uint8_t> data;
    read_io(1, data, (uint32_t)data.size(), false);

    result = 0;

    return 0;
}

int OsSyscallExceptionHandler::do_spim_sbrk(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    uint32_t increment = a1;
    increment = (increment + 15) & ~15;

    brk_limit = (brk_limit + 15) & ~15;

    result = brk_limit;
    brk_limit += increment;

    return 0;
}

int OsSyscallExceptionHandler::do_spim_exit(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    return do_sys_exit(
        result, core, syscall_num, 0, a2, a3, a4, a5, a6, a7, a8);
}

int OsSyscallExceptionHandler::do_spim_print_character(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;

    return 0;
}

int OsSyscallExceptionHandler::do_spim_read_character(
    uint32_t &result,
    Core *core,
    uint32_t syscall_num,
    uint32_t a1,
    uint32_t a2,
    uint32_t a3,
    uint32_t a4,
    uint32_t a5,
    uint32_t a6,
    uint32_t a7,
    uint32_t a8) {
    (void)core;
    (void)syscall_num;
    (void)a1;
    (void)a2;
    (void)a3;
    (void)a4;
    (void)a5;
    (void)a6;
    (void)a7;
    (void)a8;

    result = 0;

    return 0;
}
