;;; Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>
;;;
;;; Permission to use, copy, modify, and distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(include "errno.h")
(include "locale.h")

(in-package :tungsten.system)

;; Errors
(constantenum errno
              ((:e2big "E2BIG"))
              ((:eacces "EACCES"))
              ((:eaddrinuse "EADDRINUSE"))
              ((:eaddrnotavail "EADDRNOTAVAIL"))
              ((:eafnosupport "EAFNOSUPPORT"))
              ((:eagain "EAGAIN"))
              ((:ealready "EALREADY"))
              ((:ebadf "EBADF"))
              ((:ebadmsg "EBADMSG"))
              ((:ebusy "EBUSY"))
              ((:ecanceled "ECANCELED"))
              ((:echild "ECHILD"))
              ((:econnaborted "ECONNABORTED"))
              ((:econnrefused "ECONNREFUSED"))
              ((:econnreset "ECONNRESET"))
              ((:edeadlk "EDEADLK"))
              ((:edestaddrreq "EDESTADDRREQ"))
              ((:edom "EDOM"))
              ((:edquot "EDQUOT"))
              ((:eexist "EEXIST"))
              ((:efault "EFAULT"))
              ((:efbig "EFBIG"))
              ((:ehostunreach "EHOSTUNREACH"))
              ((:eidrm "EIDRM"))
              ((:eilseq "EILSEQ"))
              ((:einprogress "EINPROGRESS"))
              ((:eintr "EINTR"))
              ((:einval "EINVAL"))
              ((:eio "EIO"))
              ((:eisconn "EISCONN"))
              ((:eisdir "EISDIR"))
              ((:eloop "ELOOP"))
              ((:emfile "EMFILE"))
              ((:emlink "EMLINK"))
              ((:emsgsize "EMSGSIZE"))
              ((:emultihop "EMULTIHOP"))
              ((:enametoolong "ENAMETOOLONG"))
              ((:enetdown "ENETDOWN"))
              ((:enetreset "ENETRESET"))
              ((:enetunreach "ENETUNREACH"))
              ((:enfile "ENFILE"))
              ((:enobufs "ENOBUFS"))
              ((:enodata "ENODATA"))
              ((:enodev "ENODEV"))
              ((:enoent "ENOENT"))
              ((:enoexec "ENOEXEC"))
              ((:enolck "ENOLCK"))
              ((:enolink "ENOLINK"))
              ((:enomem "ENOMEM"))
              ((:enomsg "ENOMSG"))
              ((:enoprotoopt "ENOPROTOOPT"))
              ((:enospc "ENOSPC"))
              ((:enosr "ENOSR"))
              ((:enostr "ENOSTR"))
              ((:enosys "ENOSYS"))
              ((:enotconn "ENOTCONN"))
              ((:enotdir "ENOTDIR"))
              ((:enotempty "ENOTEMPTY"))
              ((:enotrecoverable "ENOTRECOVERABLE"))
              ((:enotsock "ENOTSOCK"))
              ((:enotsup "ENOTSUP"))
              ((:enotty "ENOTTY"))
              ((:enxio "ENXIO"))
              ((:eopnotsupp "EOPNOTSUPP"))
              ((:eoverflow "EOVERFLOW"))
              ((:eownerdead "EOWNERDEAD"))
              ((:eperm "EPERM"))
              ((:epipe "EPIPE"))
              ((:eproto "EPROTO"))
              ((:eprotonosupport "EPROTONOSUPPORT"))
              ((:eprototype "EPROTOTYPE"))
              ((:erange "ERANGE"))
              ((:erofs "EROFS"))
              ((:espipe "ESPIPE"))
              ((:esrch "ESRCH"))
              ((:estale "ESTALE"))
              ((:etime "ETIME"))
              ((:etimedout "ETIMEDOUT"))
              ((:etxtbsy "ETXTBSY"))
              ((:ewouldblock "EWOULDBLOCK"))
              ((:exdev "EXDEV")))

;; Locales
(ctype locale-t "locale_t")

(constantenum locale-category
              ((:lc-all "LC_ALL"))
              ((:lc-collate "LC_COLLATE"))
              ((:lc-ctype "LC_CTYPE"))
              ((:lc-messages "LC_MESSAGES"))
              ((:lc-monetary "LC_MONETARY"))
              ((:lc-numeric "LC_NUMERIC"))
              ((:lc-time "LC_TIME")))
