/* $XFree86: xc/programs/Xserver/hw/xfree86/os-support/bsd/ppc_video.c,v 1.6 2003/10/07 23:14:55 herrb Exp $ */
/*
 * Copyright 1992 by Rich Murphey <Rich@Rice.edu>
 * Copyright 1993 by David Wexelblat <dwex@goblin.org>
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Rich Murphey and David Wexelblat 
 * not be used in advertising or publicity pertaining to distribution of 
 * the software without specific, written prior permission.  Rich Murphey and
 * David Wexelblat make no representations about the suitability of this 
 * software for any purpose.  It is provided "as is" without express or 
 * implied warranty.
 *
 * RICH MURPHEY AND DAVID WEXELBLAT DISCLAIM ALL WARRANTIES WITH REGARD TO 
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND 
 * FITNESS, IN NO EVENT SHALL RICH MURPHEY OR DAVID WEXELBLAT BE LIABLE FOR 
 * ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER 
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF 
 * CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

/* $XConsortium: bsd_video.c /main/10 1996/10/25 11:37:57 kaleb $ */

#include "X.h"
#include "xf86.h"
#include "xf86Priv.h"

#include "xf86_OSlib.h"
#include "xf86OSpriv.h"

#include "bus/Pci.h"

#ifndef MAP_FAILED
#define MAP_FAILED ((caddr_t)-1)
#endif

#include <fcntl.h>

/*#define DEBUG*/
/***************************************************************************/
/* Video Memory Mapping section                                            */
/***************************************************************************/

#ifndef __OpenBSD__
#define DEV_MEM "/dev/mem"
#else
#define DEV_MEM "/dev/xf86"
#endif

static pointer ppcMapVidMem(int, unsigned long, unsigned long, int flags);
static void ppcUnmapVidMem(int, pointer, unsigned long);
void xf86EnableIO();
void xf86DisableIO();

void
xf86OSInitVidMem(VidMemInfoPtr pVidMem)
{
	pVidMem->linearSupported = TRUE;
	pVidMem->mapMem = ppcMapVidMem;
	pVidMem->unmapMem = ppcUnmapVidMem;
	pVidMem->initialised = TRUE;
	xf86EnableIO();
}


volatile unsigned char *ioBase = MAP_FAILED;

static pointer
ppcMapVidMem(int ScreenNum, unsigned long Base, unsigned long Size, int flags)
{
	int fd = xf86Info.screenFd;
	pointer base;
#ifdef DEBUG

	xf86MsgVerb(X_WARNING, 3, "mapVidMem %lx, %lx, %d fd = %d", 
		    Base, Size, p, fd);
#endif
	base = mmap(0, Size,
		    (flags & VIDMEM_READONLY) ?
		     PROT_READ : (PROT_READ | PROT_WRITE),
		    MAP_SHARED|MAP_FILE, fd, Base);
#ifdef DEBUG
	xf86MsgVerb(X_INFO, 3, "mapVidMem %lx", 
		    base);
#endif
	if (base == MAP_FAILED)
		FatalError("%s: could not mmap screen [s=%x,a=%x] (%s)",
			   "xf86MapVidMem", Size, Base, strerror(errno));

	return base;
}

static void
ppcUnmapVidMem(int ScreenNum, pointer Base, unsigned long Size)
{
	munmap(Base, Size);
}

int
xf86ReadBIOS(unsigned long Base, unsigned long Offset, unsigned char *Buf,
	     int Len)
{
	int rv;
	static int kmem = -1;

	if (kmem == -1) {
		kmem = open(DEV_MEM, 2);
		if (kmem == -1) {
			FatalError("xf86ReadBIOS: open %s", DEV_MEM);
		}
	}

#ifdef DEBUG
	xf86MsgVerb(X_INFO, 3, "xf86ReadBIOS() %lx %lx, %x\n", 
		    Base, Offset, Len);
#endif


	lseek(kmem, Base + Offset, 0);
	rv = read(kmem, Buf, Len);
	return rv;
}

/***************************************************************************/
/* Interrupt Handling section                                              */
/***************************************************************************/

Bool
xf86DisableInterrupts()
{

	return(TRUE);
}

void
xf86EnableInterrupts()
{

	return;
}

#ifdef USE_PPC_MMAP

void xf86EnableIO()
{
	int fd = xf86Info.screenFd;
	
	xf86MsgVerb(X_WARNING, 3, "xf86EnableIO %d\n", fd);
	if (ioBase == MAP_FAILED)
	{
		ioBase=mmap(NULL, 0x10000, PROT_READ|PROT_WRITE, MAP_SHARED, fd,
		    0xf2000000);
		xf86MsgVerb(X_INFO, 3, "xf86EnableIO: %08x\n", ioBase);
		if (ioBase == MAP_FAILED)
			xf86MsgVerb(X_WARNING, 3, "Can't map IO space!\n");
	}
}

void xf86DisableIO()
{

	if (ioBase != MAP_FAILED)
	{
		munmap(__UNVOLATILE(ioBase), 0x10000);
		ioBase = MAP_FAILED;
	}
}

#endif