/*
 * Copyright (c) 2002,2004 Damien Miller <djm@mindrot.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <config.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/param.h>

#if !defined(HAVE_GETPEEREID)

#if defined(SO_PEERCRED)
int
getpeereid(int s, uid_t *euid, gid_t *gid)
{
	struct ucred cred;
	socklen_t len = sizeof(cred);

	if (getsockopt(s, SOL_SOCKET, SO_PEERCRED, &cred, &len) < 0)
		return (-1);
	*euid = cred.uid;
	*gid = cred.gid;

	return (0);
}
#elif defined(LOCAL_CREDS)	/* NetBSD */
int
getpeereid(int s, uid_t *euid, gid_t *gid)
{
/* Credentials structure */
#if defined(HAVE_STRUCT_CMSGCRED)
        typedef struct cmsgcred Cred;

#define cruid cmcred_euid
#define crgid cmcred_groups[0]
#elif defined(HAVE_STRUCT_FCRED)
        typedef struct fcred Cred;

#define cruid fc_uid
#define crgid fc_gid
#elif defined(HAVE_STRUCT_SOCKCRED)
        typedef struct sockcred Cred;

#define cruid sc_euid
#define crgid sc_egid
#endif
        Cred *cred;

        /* Compute size without padding */
        char cmsgmem[CMSG_SPACE(sizeof(Cred))]; /* for NetBSD */

        /* Point to start of first structure */
        struct cmsghdr *cmsg = (struct cmsghdr *)cmsgmem;

        struct iovec iov;
        char buf;
        struct msghdr msg;

        memset(&msg, 0, sizeof(msg));
        msg.msg_iov = &iov;
        msg.msg_iovlen = 1;
        msg.msg_control = (char *)cmsg;
        msg.msg_controllen = sizeof(cmsgmem);
        memset(cmsg, 0, sizeof(cmsgmem));

        /*
         * The one character which is received here is not meaningful; its
         * purposes is only to make sure that recvmsg() blocks long enough for
         * the other side to send its credentials.
         */
        iov.iov_base = &buf;
        iov.iov_len = 1;

        if (recvmsg(s, &msg, 0) < 0 ||
                cmsg->cmsg_len < sizeof(cmsgmem) ||
                cmsg->cmsg_type != SCM_CREDS)
        {
                return -1;
        }

        cred = (Cred *)CMSG_DATA(cmsg);
	*euid = cred->cruid;
	*gid = cred->crgid;

	return 0;
}
#else
int
getpeereid(int s, uid_t *euid, gid_t *gid)
{
	*euid = geteuid();
	*gid = getgid();

	return (0);
}
#endif /* defined(SO_PEERCRED) */

#endif /* !defined(HAVE_GETPEEREID) */
